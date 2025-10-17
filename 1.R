## === Step 1: Setup ===
set.seed(42)
options(stringsAsFactors = FALSE)

# standardize column names to lowercase for robustness
to_lower_names <- function(df) { names(df) <- tolower(names(df)); df }

# Create an outputs folder for figures if it does not exist
if (!dir.exists("outputs")) dir.create("outputs")

## === Step 2: Read, combine PG1 + PG2  ===
patients1   <- to_lower_names(read.csv("patientsPG1.csv"))
patients2   <- to_lower_names(read.csv("patientsPG2.csv"))
encounters1 <- to_lower_names(read.csv("encountersPG1.csv"))
encounters2 <- to_lower_names(read.csv("encountersPG2.csv"))
conditions1 <- to_lower_names(read.csv("conditionsPG1.csv"))
conditions2 <- to_lower_names(read.csv("conditionsPG2.csv"))

patients   <- rbind(patients1, patients2)
encounters <- rbind(encounters1, encounters2)
conditions <- rbind(conditions1, conditions2)

cat("Rows (patients / encounters / conditions):",
    nrow(patients), "/", nrow(encounters), "/", nrow(conditions), "\n")
head(names(patients)); head(names(conditions))

## === Step 3: Flag COVID and take first diagnosis per patient ===
# Determine the patient key column in conditions
patient_key <- if ("patient" %in% names(conditions)) "patient" else
  if ("subject" %in% names(conditions)) "subject" else "patient"

# Select rows whose description contains 'COVID' (case-insensitive)
has_covid <- !is.na(conditions$description) &
  grepl("covid", conditions$description, ignore.case = TRUE)
covid_cond <- conditions[has_covid, ]

# Parse start timestamp tolerantly
covid_cond$start_dt <- suppressWarnings(as.POSIXct(covid_cond$start, tz = "UTC"))

# Order by patient id and time, then keep the earliest per patient
o <- order(covid_cond[[patient_key]], covid_cond$start_dt)
covid_cond <- covid_cond[o, ]

first_idx <- !duplicated(covid_cond[[patient_key]])
covid_first <- covid_cond[first_idx, c(patient_key, "start_dt")]
names(covid_first) <- c("patient_id", "covid_first_dt")

cat("Unique COVID patients:", nrow(covid_first), "\n")
head(covid_first)

## === Step 4: Merge with patients; compute age and age groups ===
# Determine the id column in patients
id_col <- if ("id" %in% names(patients)) "id" else "patient"

# Parse birthdate
patients$birthdate_parsed <- suppressWarnings(as.Date(patients$birthdate))

# Merge first-COVID date back to patients
covid_patients <- merge(patients, covid_first,
                        by.x = id_col, by.y = "patient_id", all = FALSE)

# Compute age in years at first COVID diagnosis (approximate leap years by 365.25)
age_days <- as.numeric(difftime(covid_patients$covid_first_dt,
                                covid_patients$birthdate_parsed,
                                units = "days"))
covid_patients$age_years <- floor(age_days / 365.25)

# Keep plausible ages only
covid_patients <- subset(covid_patients,
                         !is.na(age_years) & age_years >= 0 & age_years <= 120)

# Define age groups: 0–18, 19–35, 36–50, 51+
covid_patients$age_group <- cut(
  covid_patients$age_years,
  breaks = c(-Inf, 18, 35, 50, Inf),
  labels = c("0-18", "19-35", "36-50", "51+"),
  right = TRUE
)

# Show distribution by age group
summary(covid_patients$age_group)

## === Step 5: County distribution ===
if (!"county" %in% names(covid_patients)) {
  warning("Column 'county' not found in patient data.")
} else {
  county_vec <- covid_patients$county
  county_vec <- county_vec[!is.na(county_vec) & county_vec != ""]
  
  tb <- sort(table(county_vec), decreasing = TRUE)
  topN <- 20
  tb_top <- head(tb, topN)
  
  # Save a horizontal barplot for readability
  if (!dir.exists("outputs")) dir.create("outputs")
  png("outputs/task1_county_top20.png", width = 1600, height = 1000, res = 150)
  op <- par(mar = c(5, 12, 3, 2))
  barplot(tb_top,
          horiz = TRUE, las = 1,
          main  = paste("Top", topN, "Counties by Number of COVID Patients"),
          xlab  = "Count", cex.names = 0.8)
  par(op); dev.off()
  
  # Print top 10 for later narrative
  print(tb_top[1:min(10, length(tb_top))])
}


## === Step 6: Age-group distribution ===
tb_age <- table(covid_patients$age_group)

if (!dir.exists("outputs")) dir.create("outputs")
png("outputs/task1_age_group.png", width = 1200, height = 900, res = 150)
barplot(tb_age,
        main = "Distribution of COVID Patients by Age Group",
        xlab = "Age Group", ylab = "Count")
dev.off()

print(tb_age)


