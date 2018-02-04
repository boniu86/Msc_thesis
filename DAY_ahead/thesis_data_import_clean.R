Sys.setlocale(category = "LC_ALL", locale = "English_United States.1252")#english datetime

##load data weather from 
weather.file.list <- list.files(pattern="*_2017.csv$")
weather.raw <- lapply(weather.file.list, read_csv)


all_weather<-(weather.raw
              %>%bind_rows()#bind all csv together
              %>%filter(Partition_Key=="CYYZ") #only keep toronto weather information
              %>%distinct(Local_Time,.keep_all = TRUE)# remove one redundant obs
              %>%select(-c(Partition_Key,Row_Key,Precip_Accumulation,
                           Precip_Intensity,Precip_Probability,
                           Precip_Type,Summary,UTC_Time,Local_Time))
              %>%mutate(DateTime=seq(ymd_hm("2017-09-29 00:00"),  #add date and time
                                     ymd_hm("2017-12-31 23:00"),by="hour"),
                        Day_of_week=as.factor(weekdays(DateTime,abbreviate = FALSE)),
                        Day_of_week=fct_recode(Day_of_week,  #collapse 7 levels to 2 levels
                                               wkday="Monday", 
                                               wkday="Tuesday",
                                               wkday="Wednesday",
                                               wkday="Thursday",
                                               wkday="Friday", 
                                               wknd="Saturday",
                                               wknd="Sunday"),
                        Hr_of_day=as.factor(format(as.POSIXct(DateTime),"%H")))
              %>%bind_cols((read_csv("On_Demand_29_Sep_31_Dec.csv",col_names = FALSE)
                            %>%select(X4)))#combine demand in
              %>%rename(Demand=X4)
              %>%mutate(Demand=as.numeric(Demand),
                        Wind_Bearing=as.numeric(Wind_Bearing))
              %>%select(-c(Icon,DateTime))
)







skim(all_weather)##summary of data...other summary, may consider later


##load all ieso day ahead prediction
IESO.file.list <- list.files(pattern="*_IESO.csv$")
IESO.raw <- lapply(IESO.file.list, read_csv,col_names=FALSE)
IESO<-(IESO.raw
       %>%bind_rows()
)#day ahead prediction from Oct 9 to Dec 31