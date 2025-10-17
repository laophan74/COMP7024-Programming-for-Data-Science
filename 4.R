## Step 1: dataset label (PG1 vs PG2)
src_pg1 <- unique(tolower(if ("id" %in% names(patients1)) patients1$id else patients1$patient))
src_pg2 <- unique(tolower(if ("id" %in% names(patients2)) patients2$id else patients2$patient))

covid_patients$._id_lower <- tolower(covid_patients[[id_col]])
covid_patients$dataset <- ifelse(covid_patients$._id_lower %in% src_pg1, "PG1",
                                 ifelse(covid_patients$._id_lower %in% src_pg2, "PG2", "Unknown"))

table(covid_patients$dataset)

## Step 2: Recovery days & simple outcome (Fast/Slow)
# Helper parser (tolerant)
parse_dt <- function(x) {
  x <- as.character(x); x <- trimws(x); x[x==""] <- NA
  x <- gsub("T", " ", x, fixed=TRUE)
  x <- gsub("Z$", "", x)
  x <- gsub("([+-][0-9]{2}):?([0-9]{2})$", "", x)
  dt <- suppressWarnings(as.POSIXct(x, format="%Y-%m-%d %H:%M:%S", tz="UTC"))
  bad <- is.na(dt) & !is.na(x)
  if (any(bad)) dt[bad] <- suppressWarnings(as.POSIXct(x[bad], format="%Y-%m-%d", tz="UTC"))
  dt
}

patient_key <- if ("patient" %in% names(conditions)) "patient" else
  if ("subject" %in% names(conditions)) "subject" else "patient"

is_covid <- !is.na(conditions$description) & grepl("covid", conditions$description, ignore.case = TRUE)
covid_only <- conditions[is_covid, c(patient_key, "start", "stop")]
names(covid_only) <- c("patient_id", "start_raw", "stop_raw")

covid_only$start_dt <- parse_dt(covid_only$start_raw)
covid_only$stop_dt  <- parse_dt(covid_only$stop_raw)

# earliest stop per patient (can be NA)
min_stop <- aggregate(covid_only$stop_dt, by = list(patient_id = covid_only$patient_id),
                      FUN = function(z){ z <- z[!is.na(z)]; if (length(z)==0) return(NA); min(z) })
names(min_stop)[2] <- "first_stop_dt"

q4 <- merge(covid_patients, min_stop, by.x = id_col, by.y = "patient_id", all.x = TRUE)

# recovery days = first_stop_dt - covid_first_dt
q4$recovery_days <- as.numeric(difftime(q4$first_stop_dt, q4$covid_first_dt, units = "days"))
q4$recovery_days[q4$recovery_days < 0] <- NA  # drop weird negatives

# Simple outcome: Fast vs Slow with a 7-day threshold
cutoff <- 7
q4$outcome <- ifelse(is.na(q4$recovery_days), NA,
                     ifelse(q4$recovery_days < cutoff, "Fast", "Slow"))
table(q4$outcome, useNA = "ifany")
summary(q4$recovery_days)

## Step 3: Two simple symptom flags (fever, cough) in ±7 days
sym_terms <- list(
  fever = "fever",
  cough = "cough"
)

cond_cvd <- conditions[conditions[[patient_key]] %in% q4[[id_col]], c(patient_key, "description", "start"), drop=FALSE]
cond_cvd$description <- tolower(trimws(cond_cvd$description))
cond_cvd$start_dt <- parse_dt(cond_cvd$start)

first_dt_map <- q4[, c(id_col, "covid_first_dt")]
cond_cvd2 <- merge(cond_cvd, first_dt_map, by.x = patient_key, by.y = id_col, all.x = TRUE)

win <- 7
in_win <- !is.na(cond_cvd2$start_dt) & !is.na(cond_cvd2$covid_first_dt) &
  (cond_cvd2$start_dt >= (cond_cvd2$covid_first_dt - win*24*3600)) &
  (cond_cvd2$start_dt <= (cond_cvd2$covid_first_dt + win*24*3600))
sym_win <- cond_cvd2[in_win, ]

# Make per-patient flags for fever/cough
sym_by_pt <- aggregate(description ~ get(patient_key), data = sym_win, FUN = function(dd) paste(dd, collapse=" || "))
names(sym_by_pt)[1] <- "patient_id"
sym_by_pt$fever <- grepl(sym_terms$fever, sym_by_pt$description, ignore.case = TRUE)
sym_by_pt$cough <- grepl(sym_terms$cough, sym_by_pt$description, ignore.case = TRUE)

q4 <- merge(q4, sym_by_pt[, c("patient_id","fever","cough")],
            by.x = id_col, by.y = "patient_id", all.x = TRUE)
q4$fever[is.na(q4$fever)] <- FALSE
q4$cough[is.na(q4$cough)] <- FALSE

## Step 4: summaries

# Outcome summary
cat("\nOutcome (Fast/Slow):\n"); print(table(q4$outcome, useNA="ifany"))

# Recovery days: overall and by dataset
cat("\nRecovery days (overall):\n"); print(summary(q4$recovery_days))
cat("\nRecovery days by dataset (mean, median):\n")
print(tapply(q4$recovery_days, q4$dataset, mean,  na.rm=TRUE))
print(tapply(q4$recovery_days, q4$dataset, median, na.rm=TRUE))

# Age: mean/median and by age group
cat("\nAge (overall): mean/median\n")
print(mean(q4$age_years, na.rm=TRUE)); print(median(q4$age_years, na.rm=TRUE))
q4$age_group <- cut(q4$age_years, breaks=c(-Inf,18,35,50,Inf),
                    labels=c("0-18","19-35","36-50","51+"), right=TRUE)
cat("\nAge-group counts:\n"); print(table(q4$age_group, useNA="ifany"))

# Gender counts
cat("\nGender counts:\n"); print(table(q4$gender, useNA="ifany"))

# Fever/Cough: prevalence overall and by dataset
cat("\nSymptom prevalence overall (count TRUE):\n")
print(colSums(q4[, c("fever","cough")], na.rm=TRUE))

cat("\nSymptom prevalence by dataset (% TRUE):\n")
print(round(100 * tapply(q4$fever, q4$dataset, mean, na.rm=TRUE), 1))
print(round(100 * tapply(q4$cough, q4$dataset, mean, na.rm=TRUE), 1))


## Step 5: Plots
if (!dir.exists("outputs")) dir.create("outputs")

# 1) Histogram of recovery days
png("outputs/q4_recovery_days_hist.png", width = 1200, height = 900, res = 150)
hist(pmin(pmax(q4$recovery_days, 0), 7), breaks = 14,
     main = "Recovery Days (clipped to 0–7)", xlab = "Days")
dev.off()

# 2) Fever/Cough prevalence by dataset
fever_rate <- tapply(q4$fever, q4$dataset, mean, na.rm=TRUE)
cough_rate <- tapply(q4$cough, q4$dataset, mean, na.rm=TRUE)
rates <- rbind(Fever = as.numeric(fever_rate)*100,
               Cough = as.numeric(cough_rate)*100)
colnames(rates) <- names(fever_rate)

png("outputs/q4_symptoms_by_dataset.png", width = 1100, height = 800, res = 150)
barplot(rates, beside=TRUE, ylim=c(0,100),
        main="Symptom prevalence by Dataset (%)",
        ylab="Percent", xlab="Dataset")
legend("topright", legend=rownames(rates), bty="n")
dev.off()

# 3) Recovery days by dataset
png("outputs/q4_recovery_by_dataset_box.png", width = 1100, height = 800, res = 150)
boxplot(recovery_days ~ dataset, data=q4,
        main="Recovery Days by Dataset", xlab="Dataset", ylab="Days",
        outline=FALSE)
dev.off()

