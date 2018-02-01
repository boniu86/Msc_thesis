library(tidyverse)
library(readr)
library(lubridate)
library(skimr)
library(olsrr)
library(sjstats)
library(car)

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

##try forward selection and backward elimination for lm;
set.seed(123)
sample<-sample(1:nrow(all_weather),240)
train<-all_weather[sample,]

null<-lm(Demand~1,data = train)
full<-lm(Demand~.,data=train)
step(null, scope=list(lower=null, upper=full), direction="forward")
for_model<-lm(Demand ~ Hr_of_day + Temperature + Day_of_week + 
                Cloud_Cover + Pressure + Apparent_Temperature + 
                Ozone + Humidity,data=train)

for2_model<-lm(Demand~Hr_of_day + Temperature + Day_of_week + 
                 Cloud_Cover + Pressure  + 
                 Ozone + Humidity,data=train)
anova(for_model,for2_model,test="Chisq")#as good as full model
Anova(for2_model)
par(mfrow=c(2,2))
plot(for2_model)

cohens_f(for2_model)
Anova(for2_model)
anova_stats(for2_model)

step(full, data=train, direction="backward")
#Demand ~ Apparent_Temperature + Cloud_Cover + Dew_Point + 
#Humidity + Ozone + Pressure + Visibility + Day_of_week + 
#  Hr_of_day
back_model<-lm(Demand ~ Apparent_Temperature + Cloud_Cover + Dew_Point + 
                 Humidity + Ozone + Pressure + Visibility + Day_of_week + 
                 Hr_of_day, data = train)
Anova(back_model)



set.seed(1234)
OLM_7days<-c()
A_fit<-c()
A_lwr<-c()
A_upr<-c()
MSE_fit<-c()
MSE_lwr<-c()
MSE_upr<-c()
MSE_IESO<-c()
for (i in ((0:83))){
  Train_7days_1<-all_weather[(1:240)+i*24,]
  Test_7days_1<-all_weather[(241:264)+i*24,]
  IESO_prediction_1<-IESO[(1:24)+i*24,]
  
  IESO_error<-sqrt(1/24*(sum(((Test_7days_1$Demand- IESO_prediction_1)^2))))
  
  Final_LM_day_ahead<-lm(Demand ~ Apparent_Temperature + Cloud_Cover + Dew_Point + 
                           Humidity + Ozone + Pressure + Visibility + Day_of_week + 
                           Hr_of_day,
                           #Hr_of_day + Temperature + Day_of_week + 
                           #Cloud_Cover + Pressure + Apparent_Temperature + Visibility + 
                           #Ozone + Wind_Speed,
                         data=Train_7days_1)
  OLM_7days_1<-predict(Final_LM_day_ahead,Test_7days_1,type = "response")
  
  a<-predict(Final_LM_day_ahead,Test_7days_1,interval = "predict",level = 0.95)
  a_fit<-a[,1]
  a_lwr<-a[,2]
  a_upr<-a[,3]
  
  sq_MSE_fit<-sqrt(1/24*(sum(((Test_7days_1$Demand- a[,1])^2))))
  sq_MSE_lwr<-sqrt(1/24*(sum(((Test_7days_1$Demand- a[,2])^2))))
  sq_MSE_upr<-sqrt(1/24*(sum(((Test_7days_1$Demand- a[,3])^2))))
  
  OLM_7days<-c(OLM_7days,OLM_7days_1)
  A_fit<-c(A_fit,a_fit)
  A_lwr<-c(A_lwr,a_lwr)
  A_upr<-c(A_upr,a_upr)
  
  MSE_fit<-c(MSE_fit,sq_MSE_fit)
  MSE_lwr<-c(MSE_lwr,sq_MSE_lwr)
  MSE_upr<-c(MSE_upr,sq_MSE_upr)
  MSE_IESO<-c(MSE_IESO,IESO_error)
}


##forward model:50663.17
##backward model:51664.45


sum(MSE_fit)
