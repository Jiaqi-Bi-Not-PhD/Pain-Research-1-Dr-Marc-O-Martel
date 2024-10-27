## 
CDO_Diaries <- haven::read_sav("CDO; Diaries; 7.sav")

CDO_Diaries <- CDO_Diaries |>
  mutate(Date = dmy(TimeStamp_Date_DiarySubmitted),
         Time = hms(TimeStamp_Time_DiarySubmitted),
         DateTime = as.POSIXct(Date) + as.numeric(Time))

CDO_Diaries <- CDO_Diaries |>
  arrange(DateTime)

CDO_Diaries <- CDO_Diaries |>
  group_by(ID) |>
  mutate(Wave_Day = dense_rank(Date)) # Assigns 1 to the earliest date/time in the day, 2 to the next, etc.

CDO_Diaries <- CDO_Diaries |>
  group_by(ID, Wave_Day) |>
  mutate(
    Wave_Moment = dense_rank(Time)  
  ) |>
  ungroup()

CDO_Diaries <- CDO_Diaries |>
  select(ID, Wave_Day, Wave_Moment, everything())

CDO_Diaries_check <- CDO_Diaries |> filter(ID == "CDO_025")

haven::write_sav(CDO_Diaries, "CDO; Diaries; 7; Wave Day and Moment generated.sav")
