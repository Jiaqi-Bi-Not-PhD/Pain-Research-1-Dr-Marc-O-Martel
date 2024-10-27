Cortisol_sampledata <- readxl::read_excel("Cortisol; Dataset cortisol.xlsx", 
                                          col_types = c("guess", "guess", rep("numeric", 6)))
Cortisol_sampledata <- Cortisol_sampledata |>
  filter(Label_sample != "088D8-2358")
Cortisol_Dailydata <- haven::read_sav("Cortisol; Dataset; 2024.10.08.sav")

Cortisol_sampledata$Cortisol_mean <- as.numeric(Cortisol_sampledata$Cortisol_mean)

Cortisol_sampledata$sampleind <- 1
Cortisol_sampledata <- Cortisol_sampledata |> rename(Wave_Day = Wave_day)

Cortisol_sampledata$Sample_number <- as.character(Cortisol_sampledata$Sample_number)

## Keep those with submissions, and ascending order!
Cortisol_Dailydata_submitted <- Cortisol_Dailydata |>
  filter(Raw_Lev1_MedIntake_FU_ResponseType1b == 1)

Cortisol_Dailydata_submitted <- Cortisol_Dailydata_submitted |>
  group_by(ID, Wave_Day) |>
  arrange(Wave_Moment) |>  
  mutate(Sample_number = row_number()) |>
  ungroup()

Cortisol_Dailydata_submitted$Sample_number <- as.character(Cortisol_Dailydata_submitted$Sample_number)

## Sample counts in Cortisol sample data
sample_counts <- Cortisol_sampledata |>
  group_by(ID, Wave_Day) |>
  summarise(
    Sample_count = n(),
    Sample_numbers = list(sort(Sample_number))
  ) |>
  ungroup()

## Moment counts in Daily data
moment_counts <- Cortisol_Dailydata_submitted |>
  group_by(ID, Wave_Day) |>
  summarise(
    Submission_count = n(),
    Daily_Sample_numbers = list(sort(Sample_number))
  ) |>
  ungroup()

## Merge
merged_counts <- sample_counts |>
  full_join(moment_counts, by = c("ID", "Wave_Day")) |>
  mutate(
    Sample_count = ifelse(is.na(Sample_count), 0, Sample_count),
    Submission_count = ifelse(is.na(Submission_count), 0, Submission_count)
  )

## Three flags
merged_flags <- merged_counts |>
  rowwise() |>
  mutate(
    More_samples_than_Submissions = ifelse(Sample_count > Submission_count, 1, 0),
    Less_samples_than_Submissions = ifelse(Sample_count < Submission_count, 1, 0),
    Wrong_sample_indexes = if_else(
      Sample_count != Submission_count |
        (Sample_count == Submission_count & !all(Sample_numbers == Daily_Sample_numbers)),
      1,
      0
    )
  )

openxlsx::write.xlsx(merged_flags, "Flagged Data.xlsx")

perfectly_matched <- merged_flags |>
  filter(More_samples_than_Submissions == 0 & 
           Less_samples_than_Submissions == 0 &
           Wrong_sample_indexes == 0)

Cortisol_Daily_perfect <- semi_join(Cortisol_Dailydata, perfectly_matched, by = c("ID", "Wave_Day"))
Cortisol_Daily_perfect <- Cortisol_Daily_perfect |>
  filter(Raw_Lev1_MedIntake_FU_ResponseType1b == 1)

Cortisol_sampledata_perfect <- semi_join(Cortisol_sampledata, perfectly_matched, by = c("ID", "Wave_Day"))

Cortisol_sampledata_perfect$Sample_number <- as.numeric(Cortisol_sampledata_perfect$Sample_number)

Cortisol_Perfect <- Cortisol_Daily_perfect |>
  left_join(Cortisol_sampledata_perfect, by = c("ID", "Wave_Day", "Wave_Moment" = "Sample_number"))
haven::write_sav(Cortisol_Perfect, "Merged; Perfectly Matched Sample Number and Daily data.sav")
