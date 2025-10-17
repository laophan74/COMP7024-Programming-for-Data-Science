## === Step 1: Filter conditions for COVID patients ===
# Detect the patient key column in `conditions`
patient_key <- if ("patient" %in% names(conditions)) "patient" else
  if ("subject" %in% names(conditions)) "subject" else "patient"

# Ensure we have a vector of unique COVID patient IDs from Q1
covid_ids <- unique(covid_patients[[id_col]])

# Keep only condition rows for those COVID patients
cond_covidpat <- conditions[conditions[[patient_key]] %in% covid_ids, , drop = FALSE]

# Clean description and exclude rows that are COVID themselves
cond_covidpat$description <- trimws(cond_covidpat$description)
cond_non_covid <- subset(cond_covidpat,
                         !is.na(description) & !grepl("covid", description, ignore.case = TRUE))

# sanity checks
cat("All condition rows for COVID patients (including non-COVID): ", nrow(cond_covidpat), "\n")
cat("Non-COVID condition rows kept: ", nrow(cond_non_covid), "\n")

## Step 2: Attach gender to each non-COVID condition row
gender_map <- unique(covid_patients[, c(id_col, "gender")])

cond_non_covid_g <- merge(cond_non_covid, gender_map,
                          by.x = patient_key, by.y = id_col, all.x = TRUE)

# Standardize gender labels
cond_non_covid_g$gender_label <- toupper(trimws(cond_non_covid_g$gender))
cond_non_covid_g$gender_label[cond_non_covid_g$gender_label %in% c("M","MALE")]   <- "Male"
cond_non_covid_g$gender_label[cond_non_covid_g$gender_label %in% c("F","FEMALE")] <- "Female"

# check
table(cond_non_covid_g$gender_label, useNA = "ifany")

## Step 3: Overall Top-5 non-COVID conditions
tb_overall   <- sort(table(cond_non_covid_g$description), decreasing = TRUE)
top5_overall <- head(tb_overall, 5)
cat("Top-5 non-COVID conditions (overall):\n")
print(top5_overall)

## Step 4: Top-10 conditions by gender (Male/Female)
top_by_gender <- function(df, gender_label = "Male", N = 10) {
  sub <- df[!is.na(df$gender_label) & df$gender_label == gender_label, , drop = FALSE]
  tb  <- sort(table(sub$description), decreasing = TRUE)
  if (length(tb) == 0) return(data.frame())
  top <- head(tb, N)
  data.frame(
    Rank       = seq_along(top),
    Condition  = names(top),
    Count      = as.integer(top),
    Proportion = round(100 * as.numeric(top) / sum(tb), 2),
    stringsAsFactors = FALSE
  )
}

top10_male   <- top_by_gender(cond_non_covid_g, "Male",   N = 10)
top10_female <- top_by_gender(cond_non_covid_g, "Female", N = 10)

cat("\nTop 10 conditions - Male\n");   print(top10_male)
cat("\nTop 10 conditions - Female\n"); print(top10_female)

if (!dir.exists("outputs")) dir.create("outputs")
write.csv(top10_male,   "outputs/q2_top10_male.csv",   row.names = FALSE)
write.csv(top10_female, "outputs/q2_top10_female.csv", row.names = FALSE)

