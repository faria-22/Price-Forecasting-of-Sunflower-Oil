library(fpp3)
remove(list=ls())

# Q2.b)
# 1st tsibble:
temp=readr::read_csv("PSUNOUSDM.csv")
sunfloweroil=temp%>%mutate(DATE=yearmonth(DATE))%>%
  as_tsibble(index=DATE)


sunfloweroil%>%autoplot(PSUNOUSDM)+ylab("Before Transformation")
sunfloweroil%>%gg_season(PSUNOUSDM)+ylab("Seasonality")

# Finding lambda value for transformation
lambda = sunfloweroil %>% features(PSUNOUSDM, guerrero) %>%
  pull(lambda_guerrero)

# Taking log on entire data set(Lambda=-0.3883855)
sunfloweroil=sunfloweroil%>%mutate(log(PSUNOUSDM))   
sunfloweroil%>%autoplot(log(PSUNOUSDM))+ylab("Logarithmic Transformation")
sunfloweroil%>%autoplot(box_cox(PSUNOUSDM,lambda))+ylab("Box-Cox Transformation")

# 2nd tsibble taking log transformation and excluding last 4 observations
sunfloweroilHOLD=sunfloweroil%>%filter_index(~"2022 May")
sunfloweroilHOLD%>%autoplot(log(PSUNOUSDM))+ylab("Restricted Period")


#Q2.C)
# STEP1 1: Is my data stationary or not?
# Step 1a: To determine if seasonal differencing is needed

sunfloweroilHOLD%>%features(log(PSUNOUSDM),unitroot_nsdiffs)

# D=0
# Step 1b: To determine if non-seasonal differencing is needed,
sunfloweroilHOLD%>%features(log(PSUNOUSDM),unitroot_ndiffs)

# d=1
#KPSS Test
sunfloweroilHOLD%>%features(log(PSUNOUSDM),unitroot_kpss)
#p value: 0.01

# Q2.e.i
# Providing auto-correlations and partial auto-correlations:
sunfloweroilHOLD%>%
  gg_tsdisplay(difference(log(PSUNOUSDM),1),lag_max=72,plot_type='partial')

# Estimating several models:
sunfloweroilHOLD%>%model(ARIMA(log(PSUNOUSDM)~1+pdq(1,1,9)+PDQ(1,0,0)))%>%
  report()
sunfloweroilHOLD%>%model(ARIMA(log(PSUNOUSDM)~1+pdq(2,1,9)+PDQ(1,0,3)))%>%
  report()

sunfloweroilHOLD%>%model(ARIMA(log(PSUNOUSDM)~1+pdq(0,1,1)+PDQ(1,0,0)))%>%
  report()

sunfloweroilHOLD%>%model(ARIMA(log(PSUNOUSDM)~1+pdq(1,1,1)+PDQ(1,0,1)))%>%
  report()

sunfloweroilHOLD%>%model(ARIMA(log(PSUNOUSDM)~1+pdq(2,1,1)+PDQ(1,0,4)))%>%
  report()

sunfloweroilHOLD%>%model(ARIMA(log(PSUNOUSDM)~1+pdq(0,1,2)+PDQ(2,0,4)))%>%
  report()


finalArimaMODEL=sunfloweroilHOLD%>%
  model(ARIMA(log(PSUNOUSDM)~
                0+pdq(0,1,1)+PDQ(1,0,0)))

# Q2.e.ii
# Diagnostic Checks
finalArimaMODEL%>%gg_tsresiduals()

residuals(finalArimaMODEL)%>%
  gg_tsdisplay(.resid,plot_type="partial",lag_max=72)

augment(finalArimaMODEL)%>%
  features(.innov,ljung_box,lag=36,dof=3)
qchisq(0.95,33)

# Q2.f
# Forecasting Using Best ARIMA Model
finalArimaMODEL%>%forecast(h=4)%>%
  autoplot(sunfloweroil%>%filter_index("2018 Jan"~"2022 Sep"))+ylab("ARIMA Forecast")
finalArimaMODEL%>%forecast(h=4)%>%as.data.frame()%>%
  select(DATE,.mean)

# Q2.g
# Applying Different ETS models
fit =sunfloweroilHOLD%>%model(MAN=ETS(log(PSUNOUSDM)~error("M")+trend("A")+season("N")),
                              AAN=ETS(log(PSUNOUSDM)~error("A")+trend("A")+season("N")),
                              MAM=ETS(log(PSUNOUSDM)~error("M")+trend("A")+season("M")),
                              MAA=ETS(log(PSUNOUSDM)~error("M")+trend("A")+season("A")))


glance(fit)%>%as.data.frame()%>%select(.model,BIC,AIC)

BestETSmodel=sunfloweroilHOLD%>%model(ETS(log(PSUNOUSDM)~error("M")+trend("A")+season("N")))

report(BestETSmodel)

sunfloweroilHOLD%>%model(ETS(log(PSUNOUSDM)~error("M")+trend("A")+season("N")))%>%
  forecast(h=4)%>%autoplot(filter_index(sunfloweroil,"2018 Jan"~"2022 Sep"))+ylab("MAN")
sunfloweroilHOLD%>%model(ETS(log(PSUNOUSDM)~error("A")+trend("A")+season("N")))%>%
  forecast(h=4)%>%autoplot(filter_index(sunfloweroil,"2018 Jan"~"2022 Sep"))+ylab("AAN")
sunfloweroilHOLD%>%model(ETS(log(PSUNOUSDM)~error("M")+trend("A")+season("M")))%>%
  forecast(h=4)%>%autoplot(filter_index(sunfloweroil,"2018 Jan"~"2022 Sep"))+ylab("MAM")
sunfloweroilHOLD%>%model(ETS(log(PSUNOUSDM)~error("M")+trend("A")+season("A")))%>%
  forecast(h=4)%>%autoplot(filter_index(sunfloweroil,"2018 Jan"~"2022 Sep"))+ylab("MAA")

forecast(BestETSmodel,h=4)%>%as.data.frame()%>%
  select(DATE,.mean)


forecastsFINAL_1=forecast(BestETSmodel,h=4)
accuracy(forecastsFINAL_1,sunfloweroil)%>%select(.model,RMSE)%>%as.data.frame()

forecastsFINAL_2=forecast(finalArimaMODEL,h=4)
accuracy(forecastsFINAL_2,sunfloweroil)%>%select(.model,RMSE)%>%as.data.frame()


# Q2.h
# 6 step ahead forecasting 
bestmodel=sunfloweroil%>%
  model(ARIMA(log(PSUNOUSDM)~
                0+pdq(0,1,1)+PDQ(1,0,0)))

forecast(bestmodel,h=6)%>%as.data.frame()%>%
  select(DATE,.mean)
forecast(bestmodel,h=6)%>% autoplot(sunfloweroil%>%filter_index("2018 Jan"~"2023 Mar"))+ylab("Final Forecast")
