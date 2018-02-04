##try forward selection and backward elimination for lm;
set.seed(123)
sample<-sample(1:nrow(all_weather),240)
train<-all_weather[sample,]

null<-lm(Demand~1,data = train)
full<-lm(Demand~.,data=train)
step(null, scope=list(lower=null, upper=full), direction="forward")
for_model<-lm(Demand ~ Hr_of_day + Temperature + Day_of_week + 
                Cloud_Cover + Visibility,data=train)

par(mfrow=c(2,2))


cohens_f(for_model)
Anova(for2_model)
anova_stats(for2_model)

step(full, data=train, direction="backward")
#Demand ~ Apparent_Temperature + Cloud_Cover + Dew_Point + 
#Humidity + Ozone + Pressure + Visibility + Day_of_week + 
#  Hr_of_day
back_model<-lm( Demand ~ Cloud_Cover + Dew_Point + Humidity + Visibility + 
                  Day_of_week + Hr_of_day,
                #Demand ~ Apparent_Temperature + Cloud_Cover + Dew_Point + 
                #Humidity + Ozone + Pressure + Visibility + Day_of_week + 
                #Hr_of_day, 
                data = train)


Anova(back_model)
cohens_f(back_model)

anova(for_model,back_model,test="Chisq")

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
  
  Final_LM_day_ahead<-lm(Demand ~ Cloud_Cover + Dew_Point + Humidity + Visibility + 
                           Day_of_week + Hr_of_day,
                         #Demand ~ Apparent_Temperature + Cloud_Cover + Dew_Point + 
                         #Humidity + Ozone + Pressure + Visibility + Day_of_week + 
                         #Hr_of_day,
                         
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


##forward model:47740.33
##backward model: 48279.18


sum(MSE_fit)
sum(MSE_IESO)
