## === Step 1: Subset encounters for COVID cohort & parse timestamps ===

# Identify the patient id column in encounters
enc_patient_key <- if ("patient" %in% names(encounters)) {
  "patient"
} else if ("subject" %in% names(encounters)) {
  "subject"
} else {
  "patient"
}

# IDs of COVID patients from Task 1
covid_ids <- unique(covid_patients[[id_col]])

# Keep only encounters for those patients
enc_covid <- encounters[encounters[[enc_patient_key]] %in% covid_ids, , drop = FALSE]

# Locate start/end columns
time_start_col <- if ("start" %in% names(enc_covid)) {
  "start"
} else if ("period.start" %in% names(enc_covid)) {
  "period.start"
} else {
  NA_character_
}

time_end_col <- if ("stop" %in% names(enc_covid)) {
  "stop"
} else if ("end" %in% names(enc_covid)) {
  "end"
} else if ("period.end" %in% names(enc_covid)) {
  "period.end"
} else {
  NA_character_
}

# Parse times tolerantly only if the columns exist
enc_covid$start_dt <- if (!is.na(time_start_col)) {
  suppressWarnings(as.POSIXct(enc_covid[[time_start_col]], tz = "UTC"))
} else {
  warning("No recognized start time column found in encounters.")
  as.POSIXct(NA)
}

enc_covid$end_dt <- if (!is.na(time_end_col)) {
  suppressWarnings(as.POSIXct(enc_covid[[time_end_col]], tz = "UTC"))
} else {
  as.POSIXct(NA)
}

# Summaries
cat("Rows in encounters for COVID cohort:", nrow(enc_covid), "\n")
cat("Start column used:", ifelse(is.na(time_start_col), "NONE", time_start_col), "\n")
cat("End column used:",   ifelse(is.na(time_end_col),   "NONE", time_end_col),   "\n")

cat("\nSummary of start_dt:\n"); print(summary(enc_covid$start_dt))
cat("\nSummary of end_dt:\n");   print(summary(enc_covid$end_dt))


## Step 2: Length of stay in days, where end >= start
valid_los <- !is.na(enc_covid$start_dt) & !is.na(enc_covid$end_dt) &
  (enc_covid$end_dt >= enc_covid$start_dt)

enc_covid$los_days <- NA_real_
enc_covid$los_days[valid_los] <- as.numeric(difftime(enc_covid$end_dt[valid_los],
                                                     enc_covid$start_dt[valid_los],
                                                     units = "days"))

cat("LOS available rows:", sum(!is.na(enc_covid$los_days)),
    "out of", nrow(enc_covid), "\n")
print(summary(enc_covid$los_days))


## Step 3: Aggregate by month
enc_covid$ym <- NA_character_
has_start <- !is.na(enc_covid$start_dt)
enc_covid$ym[has_start] <- strftime(enc_covid$start_dt[has_start], format = "%Y-%m")

# Encounters per month (ascending by time)
tab_month_count <- sort(table(enc_covid$ym), decreasing = FALSE)

# Mean LOS per month (using rows where LOS is not NA)
mean_los_by_month <- tapply(enc_covid$los_days, enc_covid$ym, function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
})

cat("Encounter counts by month (head):\n")
print(head(tab_month_count, 12))  # first 12 entries
cat("Mean LOS by month (head):\n")
print(head(mean_los_by_month, 12))

## Step 4: Save monthly plots to outputs/
if (!dir.exists("outputs")) dir.create("outputs")

# Encounters per month
png("outputs/q3_encounters_per_month.png", width = 1400, height = 900, res = 150)
par(mar = c(8, 5, 3, 2))
barplot(tab_month_count,
        main = "Encounters per Month (COVID cohort)",
        xlab = "Year-Month", ylab = "Count", las = 2, cex.names = 0.8)
dev.off()

# Mean LOS per month
months_vec <- names(mean_los_by_month)
los_vec <- as.numeric(mean_los_by_month)

png("outputs/q3_mean_los_per_month.png", width = 1400, height = 900, res = 150)
par(mar = c(8, 5, 3, 2))
plot(seq_along(los_vec), los_vec, type = "b", xaxt = "n",
     main = "Mean LOS (days) per Month (where available)",
     xlab = "Year-Month", ylab = "Mean LOS (days)")
axis(1, at = seq_along(los_vec), labels = months_vec, las = 2, cex.axis = 0.8)
abline(h = 0, lty = 3, col = "gray")
dev.off()


## Step 5: Top classes/types
class_col <- if ("class" %in% names(enc_covid)) "class" else
  if ("encounterclass" %in% names(enc_covid)) "encounterclass" else NA
type_col  <- if ("type" %in% names(enc_covid)) "type" else
  if ("encountertype" %in% names(enc_covid)) "encountertype" else NA

if (!dir.exists("outputs")) dir.create("outputs")

# Class
if (!is.na(class_col)) {
  tb_class <- sort(table(enc_covid[[class_col]]), decreasing = TRUE)
  cat("Encounter class (top 10):\n"); print(head(tb_class, 10))
  png("outputs/q3_encounters_by_class.png", width = 1200, height = 900, res = 150)
  par(mar = c(6,5,3,2))
  barplot(head(tb_class, 10), las = 2, main = "Encounters by Class (Top 10)",
          xlab = "", ylab = "Count")
  dev.off()
}

# Type
if (!is.na(type_col)) {
  type_chr <- as.character(enc_covid[[type_col]])
  tb_type <- sort(table(type_chr), decreasing = TRUE)
  cat("Encounter type (top 10):\n"); print(head(tb_type, 10))
  png("outputs/q3_encounters_by_type.png", width = 1200, height = 900, res = 150)
  par(mar = c(8,5,3,2))
  barplot(head(tb_type, 10), las = 2, main = "Encounters by Type (Top 10)",
          xlab = "", ylab = "Count", cex.names = 0.8)
  dev.off()
}

## Step 6: Mean LOS by class
if (!is.na(class_col)) {
  mean_los_by_class <- tapply(enc_covid$los_days, enc_covid[[class_col]], function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA_real_)
    mean(x)
  })
  cat("Mean LOS by class:\n"); print(round(sort(mean_los_by_class, na.last = TRUE, decreasing = TRUE), 2))
}


