library(scales)
library(rlang)
library(lazyeval)
library(ggplot2)
library(fpp2)
library(readxl)

df_prod <- read.csv("https://raw.githubusercontent.com/dhykac/manpower_planning/main/df_productivity.csv")
summary(df_prod)
View(df_prod)

# INB-PROD
Yinp <- ts(df_prod[,2])
autoplot(Yinp) + ggtitle("Time Series Plot : INB-PROD") + ylab("Sum of Pallets")

DYinp <- diff(Yinp)
autoplot(DYinp) + ggtitle("Time Series Plot : INB-PROD with diff") + ylab("Sum of Pallets")
ggseasonplot(DYinp) + ggtitle("Time Series Plot : INB-PROD with seasonal") + ylab("Sum of Pallets")

fit <- naive(DYinp)
print(summary(fit))
checkresiduals(fit)
## Residuals = 1208

fit_ets <- ets(Yinp)
print(summary(fit_ets))
checkresiduals(fit_ets)
## Residuals = 713

fit_arima <- auto.arima(DYinp, d=0, D=0, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)
## Residuals = 710
## Residuals diff 2 = 779

fcast <- forecast(fit_arima, h=3)
autoplot(fcast)
print(summary(fcast))

### Forecast workload for next day with confidence 95% is 1919 pallets.





# INBOUND
Yinc <- ts(df_prod[,3])
autoplot(Yinc) + ggtitle("Time Series Plot : INBOUND") + ylab("Sum of Pallets")

DYinc <- diff(Yinc)
autoplot(DYinc) + ggtitle("Time Series Plot : INBOUND with diff") + ylab("Sum of Pallets")
ggseasonplot(DYinC) + ggtitle("Time Series Plot : INBOUND with seasonal") + ylab("Sum of Pallets")

fit <- naive(DYinc)
print(summary(fit))
checkresiduals(fit)
## Residuals = 1806

fit_ets <- ets(Yinc)
print(summary(fit_ets))
checkresiduals(fit_ets)
## Residuals = 1069

fit_arima <- auto.arima(DYinc, d=0, D=0, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)
## Residuals = 1032
## Residuals diff 2 = 1108

fcast <- forecast(fit_arima, h=3)
autoplot(fcast)
print(summary(fcast))

### Forecast workload for next day with confidence 95% is 2397 pallets.





# INTERNAL
Yint <- ts(df_prod[,4])
autoplot(Yint) + ggtitle("Time Series Plot : INTERNAL") + ylab("Sum of Pallets")

DYint <- diff(Yint)
autoplot(DYint) + ggtitle("Time Series Plot : INTERNAL with diff") + ylab("Sum of Pallets")
ggseasonplot(DYint) + ggtitle("Time Series Plot : INTERNAL with seasonal") + ylab("Sum of Pallets")

fit <- naive(DYint)
print(summary(fit))
checkresiduals(fit)
## Residuals = 400

fit_ets <- ets(Yint)
print(summary(fit_ets))
checkresiduals(fit_ets)
## Residuals = 189

fit_arima <- auto.arima(DYint, d=0, D=0, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)
## Residuals = 182
## Residuals diff 2 = 290

fcast <- forecast(fit_arima, h=3)
autoplot(fcast)
print(summary(fcast))

### Forecast workload for next day with confidence 95% is 397 pallets.





# NARROW
Ynar <- ts(df_prod[,5])
autoplot(Ynar) + ggtitle("Time Series Plot : NARROW") + ylab("Sum of Pallets")

DYnar <- diff(Ynar)
autoplot(DYnar) + ggtitle("Time Series Plot : NARROW with diff") + ylab("Sum of Pallets")
ggseasonplot(DYnar) + ggtitle("Time Series Plot : NARROW with seasonal") + ylab("Sum of Pallets")

fit <- naive(DYnar)
print(summary(fit))
checkresiduals(fit)
## Residuals = 319

fit_ets <- ets(Ynar)
print(summary(fit_ets))
checkresiduals(fit_ets)
## Residuals = 141

fit_arima <- auto.arima(DYnar, d=0, D=0, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)
## Residuals = 145
## Residuals diff 2 = 154

fcast <- forecast(fit_ets, h=3)
autoplot(fcast)
print(summary(fcast))

### Forecast workload for next day with confidence 95% is 498 pallets.





# NARROW-OUT
Ynao <- ts(df_prod[,6])
autoplot(Ynao) + ggtitle("Time Series Plot : NARROW-OUT") + ylab("Sum of Pallets")

DYnao <- diff(Ynao)
autoplot(DYnao) + ggtitle("Time Series Plot : NARROW-OUT with diff") + ylab("Sum of Pallets")
ggseasonplot(DYnao) + ggtitle("Time Series Plot : NARROW-OUT with seasonal") + ylab("Sum of Pallets")

fit <- naive(DYnao)
print(summary(fit))
checkresiduals(fit)
## Residuals = 258

fit_ets <- ets(Ynao)
print(summary(fit_ets))
checkresiduals(fit_ets)
## Residuals = 115

fit_arima <- auto.arima(DYnao, d=0, D=0, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)
## Residuals = 118
## Residuals diff 2 = 137

fcast <- forecast(fit_ets, h=3)
autoplot(fcast)
print(summary(fcast))

### Forecast workload for next day with confidence 95% is 565 pallets.





# NARROW-RPL
Ynarp <- ts(df_prod[,7])
autoplot(Ynarp) + ggtitle("Time Series Plot : NARROW-RPL") + ylab("Sum of Pallets")

DYnarp <- diff(Ynarp)
autoplot(DYnarp) + ggtitle("Time Series Plot : NARROW-RPL with diff") + ylab("Sum of Pallets")
ggseasonplot(DYnarp) + ggtitle("Time Series Plot : NARROW-RPL with seasonal") + ylab("Sum of Pallets")

fit <- naive(DYnarp)
print(summary(fit))
checkresiduals(fit)
## Residuals = 19

fit_ets <- ets(Ynarp)
print(summary(fit_ets))
checkresiduals(fit_ets)
## Residuals = 10

fit_arima <- auto.arima(DYnarp, d=0, D=0, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)
## Residuals = 10
## Residuals diff 2 = 14

fcast <- forecast(fit_ets, h=3)
autoplot(fcast)
print(summary(fcast))

### Forecast workload for next day with confidence 95% is 25 pallets.





# O-PND-OUT
Ypnd <- ts(df_prod[,8])
autoplot(Ypnd) + ggtitle("Time Series Plot : O-PND-OUT") + ylab("Sum of Pallets")

DYpnd <- diff(Ypnd)
autoplot(DYpnd) + ggtitle("Time Series Plot : O-PND-OUT with diff") + ylab("Sum of Pallets")
ggseasonplot(DYpnd) + ggtitle("Time Series Plot : O-PND-OUT with seasonal") + ylab("Sum of Pallets")

fit <- naive(DYpnd)
print(summary(fit))
checkresiduals(fit)
## Residuals = 254

fit_ets <- ets(Ypnd)
print(summary(fit_ets))
checkresiduals(fit_ets)
## Residuals = 101

fit_arima <- auto.arima(DYpnd, d=0, D=0, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)
## Residuals = 114
## Residuals diff 2 = 147

fcast <- forecast(fit_ets, h=3)
autoplot(fcast)
print(summary(fcast))

### Forecast workload for next day with confidence 95% is 425 pallets.





# OUT-CONT
Youc <- ts(df_prod[,9])
autoplot(Youc) + ggtitle("Time Series Plot : OUT-CONT") + ylab("Sum of Pallets")

DYouc <- diff(Youc)
autoplot(DYouc) + ggtitle("Time Series Plot : OUT-CONT with diff") + ylab("Sum of Pallets")
ggseasonplot(DYouc) + ggtitle("Time Series Plot : OUT-CONT with seasonal") + ylab("Sum of Pallets")

fit <- naive(DYouc)
print(summary(fit))
checkresiduals(fit)
## Residuals = 695

fit_ets <- ets(Youc)
print(summary(fit_ets))
checkresiduals(fit_ets)
## Residuals = 367

fit_arima <- auto.arima(DYouc, d=0, D=0, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)
## Residuals = 420
## Residuals diff 2 = 475

fcast <- forecast(fit_ets, h=3)
autoplot(fcast)
print(summary(fcast))

### Forecast workload for next day with confidence 95% is 1602 pallets.





# OUTBOUND
Youb <- ts(df_prod[,10])
autoplot(Youb) + ggtitle("Time Series Plot : OUTBOUND") + ylab("Sum of Pallets")

DYoub <- diff(Youb)
autoplot(DYoub) + ggtitle("Time Series Plot : OUTBOUND with diff") + ylab("Sum of Pallets")
ggseasonplot(DYoub) + ggtitle("Time Series Plot : OUTBOUND with seasonal") + ylab("Sum of Pallets")

fit <- naive(DYoub)
print(summary(fit))
checkresiduals(fit)
## Residuals = 1104

fit_ets <- ets(Youb)
print(summary(fit_ets))
checkresiduals(fit_ets)
## Residuals = 544

fit_arima <- auto.arima(DYoub, d=0, D=0, stepwise = FALSE, approximation = FALSE, trace = TRUE)
print(summary(fit_arima))
checkresiduals(fit_arima)
## Residuals = 557
## Residuals diff 2 = 661

fcast <- forecast(fit_ets, h=3)
autoplot(fcast)
print(summary(fcast))

### Forecast workload for next day with confidence 95% is 2617 pallets.





# So the total workload is :
## INB-PROD 1919 pallets
## INBOUND 2397 pallets
## INTERNAL 397 pallets
## NARROW 498 pallets
## NARROW-OUT 565 pallets
## NARROW-RPL 25 pallets
## O-PND-OUT 425 pallets
## OUT-CONT 1602 pallets
## OUTBOUND 2617 pallets