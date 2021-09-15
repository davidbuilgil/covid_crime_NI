
#######################
#
# Crime and COVID in NI
#
######################

rm(list=ls())

#load packages
library(here)
library(tseries)
library(forecast)
library(car)
library(dplyr)

#load data
data <- read.csv(here("data/data_rev.csv"))

#save dates
dates <- data$X

#function to obtain model results
modelit <- function(dataset, Y, T, D1, P1, D2, P2, D3, P3, S){
  
  #estimate temporal model
  ts <- lm(Y ~ T + D1 + P1 + D2 + P2 + D3 + P3 + S, data = dataset)
  
  #print model results
  summary(ts)
  
}

#function to obtain ARIMA results
arimait.model <- function(dataset, Y, T, D1, P1, D2, P2, D3, P3){
  
  #convert crime trend to time series
  crime.ts <- ts(Y)
  
  #create dataframe with covariates
  xreg <- cbind(D1, P1, D2, P2, D3, P3)
  
  #estimate ARIMA
  arima <- auto.arima(y = crime.ts, xreg = xreg)
  
  #print result
  summary(arima)
  
}

#function Durbin Watson test (a value between 1.5 and 2.5 indicates no autocorrelation)
#function Ljung-Box (p-value larger than 0.05 indicates independendency of residuals)
#function KPSS (p-value larger 0.05 indicate data is stationary: good)
arimait.tests <- function(dataset, Y, T, D1, P1, D2, P2, D3, P3){
  
  #convert crime trend to time series
  crime.ts <- ts(Y)
  
  #create dataframe with covariates
  xreg <- cbind(D1, P1, D2, P2, D3, P3)
  
  #estimate ARIMA
  arima <- auto.arima(y = crime.ts, xreg = xreg)
  
  #save results
  tests <- as.data.frame(cbind(
    durbinWatsonTest(as.vector(arima$residuals)),
    Box.test(as.vector(arima$residuals), type = c("Ljung-Box"))$p.value,
    kpss.test(arima$fitted, null = "Trend")$p.value
  ))
  
  #print results
  tests %>%
    summarise(durbin.watson = ifelse( V1 > 1.5 & V1 < 2.5, "tick", "not.tick"),
              ljung.box = ifelse( V2 > 0.05, "tick", "not.tick"),
              kpss = ifelse( V3 > 0.05, "tick", "not.tick"))
  
}

#function ARIMA coefficients
arimait.coef <- function(dataset, Y, T, D1, P1, D2, P2, D3, P3){
  
  #convert crime trend to time series
  crime.ts <- ts(Y)
  
  #create dataframe with covariates
  xreg <- cbind(D1, P1, D2, P2, D3, P3)
  
  #estimate ARIMA
  arima <- auto.arima(y = crime.ts, xreg = xreg)
  
  #calculate coefficients and 95% CI
  arima.result <- cbind(coef = arima$coef,
                        LI = arima$coef - 1.06 * sqrt(diag(vcov(arima))),
                        UI = arima$coef + 1.06 * sqrt(diag(vcov(arima))))
  
  #print result
  round(arima.result, digits = 1)
  
}

#function to automate plotting
plotit <- function(dataset, Y, T, D1, P1, D2, P2, D3, P3, S, title){
  
  #estimate temporal model
  ts <- lm(Y ~ T + D1 + P1 + D2 + P2 + D3 + P3 + S, data = dataset)
  
  #print model results
  summary(ts)
  
  #predict from the model
  pred1 <- predict(ts, dataset)
  
  #create new dataset where D and P are 0
  datanew <- as.data.frame(cbind(T = rep(1 : nrow(dataset)), 
                                 D1 = rep(0), 
                                 P1 = rep(0),
                                 D2 = rep(0),
                                 P2 = rep(0),
                                 D3 = rep(0),
                                 P3 = rep(0),
                                 S  = S))
  
  #predict counterfactual
  pred2 <- predict(ts, datanew)
  
  #design plot
  plot( Y,
        bty = "n",
        col = gray(0.5, 0.5), pch=19,
        ylim = c(0, max(Y)), xlim=c(0, nrow(dataset)),
        xlab = "", xaxt = "n",
        ylab = "Crime",
        main = title)
  
  #add dates
  axis(1, at  = 1:length(dates), 
       labels = c(dates),
       cex.axis = 0.9, las=2)
  
  #draw lines
  lines( rep(1:59), pred1[1:59], col = "dodgerblue4", lwd = 3 )
  lines( rep(60:66), pred1[60:66], col="brown2", lwd = 3 )
  lines( rep(67:69), pred1[67:69], col="brown2", lwd = 3 )
  lines( rep(70:length(pred1)), pred1[70:length(pred1)], col="brown2", lwd = 3 )
  lines( rep(60:length(pred2)), pred2[60:length(pred2)], col="dodgerblue2", lwd = 3, lty = 5 ) 
  abline( v=59.5, col="darkorange2", lty=2 )
  abline( v=66.5, col="darkorange2", lty=2 )
  abline( v=69.5, col="darkorange2", lty=2 )
  
}

##all crime

#plot all crime
plot.all <- function(){plotit(data, data$All.crime,
                                data$T, data$D1, data$P1, 
                                data$D2, data$P2, data$D3, data$P3, 
                                data$S, "All crime")}
plot.all()

#all text
text(57.5, 3500, "First lockdown \n 26/03/20", srt = 90, cex = 0.75)
text(64.5, 3500, "Second lockdown \n 16/10/20", srt = 90, cex = 0.75)
text(71.5, 3500, "Stay-at-home order \n 08/01/21", srt = 90, cex = 0.75)

#save figure: 9.49 x 3.60 inches

##violence
par(mfrow=c(2,2), mai = c(0.7, 0.5, 0.5, 0.1))

#model and plot violence injury
modelit(data, data$Violence.with.injury..including.homicide...death.serious.injury.by.unlawful.driving.,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$Violence.with.injury..including.homicide...death.serious.injury.by.unlawful.driving.,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$Violence.with.injury..including.homicide...death.serious.injury.by.unlawful.driving.,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

#estimate model (1, 1, 0)
arima <- arima(x = ts(data$Violence.with.injury..including.homicide...death.serious.injury.by.unlawful.driving.), 
               xreg = cbind(data$D1, data$P1, data$D2, data$P2, data$D3, data$P3),
               order = c(1, 1, 0))

durbinWatsonTest(as.vector(arima$residuals)) #tick

Box.test(as.vector(arima$residuals), type = c("Ljung-Box")) #tick

kpss.test(fitted(arima), null = "Trend") #tick

summary(arima)

round(cbind(coef = arima$coef,
            LI = arima$coef - 1.06 * sqrt(diag(vcov(arima))),
            UI = arima$coef + 1.06 * sqrt(diag(vcov(arima)))), 
      digits = 1)

plot.vio.i <- function(){plotit(data, data$Violence.with.injury..including.homicide...death.serious.injury.by.unlawful.driving.,
                                data$T, data$D1, data$P1, 
                                data$D2, data$P2, data$D3, data$P3, data$S, "Violence with injury")}
plot.vio.i()

#plot violence no injury
modelit(data, data$Violence.without.injury,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$Violence.without.injury,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$Violence.without.injury,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.coef(data, data$Violence.without.injury,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

plot.vio.ni <- function(){plotit(data, data$Violence.without.injury,
                                 data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, 
                                 data$S, "Violence without injury")}
plot.vio.ni()

#sexual crime
modelit(data, data$Sexual.offences,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$Sexual.offences,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$Sexual.offences,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

#estimate model (1, 1, 0)
arima <- arima(x = ts(data$Sexual.offences), 
               xreg = cbind(data$D1, data$P1, data$D2, data$P2, data$D3, data$P3),
               order = c(1, 1, 0))

durbinWatsonTest(as.vector(arima$residuals)) #tick

Box.test(as.vector(arima$residuals), type = c("Ljung-Box")) #tick

kpss.test(fitted(arima), null = "Trend") #tick

summary(arima)

round(cbind(coef = arima$coef,
            LI = arima$coef - 1.06 * sqrt(diag(vcov(arima))),
            UI = arima$coef + 1.06 * sqrt(diag(vcov(arima)))), 
      digits = 1)

plot.sex <- function(){plotit(data, data$Sexual.offences,
                              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                              data$S, "Sexual offences")}
plot.sex()

#model and plot robbery
modelit(data, data$Robbery,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$Robbery,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$Robbery,
           data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

#estimate model (1, 1, 0)
arima <- arima(x = ts(data$Robbery), 
               xreg = cbind(data$D1, data$P1, data$D2, data$P2, data$D3, data$P3),
               order = c(1, 1, 0))

durbinWatsonTest(as.vector(arima$residuals)) #tick

Box.test(as.vector(arima$residuals), type = c("Ljung-Box")) #tick

kpss.test(fitted(arima), null = "Trend") #tick

summary(arima)

round(cbind(coef = arima$coef,
            LI = arima$coef - 1.06 * sqrt(diag(vcov(arima))),
            UI = arima$coef + 1.06 * sqrt(diag(vcov(arima)))), 
      digits = 1)

plot.robbery <- function(){plotit(data, data$Robbery,
                                  data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                  data$S, "Robbery")}
plot.robbery()

#save figure: 9.49 x 5.60 inches

##drugs, damage and public order
data <- data %>%
  mutate(order.weapons = Public.order.offences + Possession.of.weapons.offences)
par(mfrow=c(2,2), mai = c(0.7, 0.5, 0.5, 0.1))

#model and plot possession of drugs
modelit(data, data$Possession.of.drugs,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$Possession.of.drugs,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$Possession.of.drugs,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.coef(data, data$Possession.of.drugs,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

plot.drug.po <- function(){plotit(data, data$Possession.of.drugs,
                                  data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                  data$S, "Possession of drugs")}
plot.drug.po()

#model and plot drug trafficking
modelit(data, data$Trafficking.of.drugs,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$Trafficking.of.drugs,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$Trafficking.of.drugs,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

#estimate model (1, 1, 0)
arima <- arima(x = ts(data$Trafficking.of.drugs), 
                    xreg = cbind(data$D1, data$P1, data$D2, data$P2, data$D3, data$P3),
               order = c(1, 1, 0))

durbinWatsonTest(as.vector(arima$residuals)) #tick

Box.test(as.vector(arima$residuals), type = c("Ljung-Box")) #tick

kpss.test(fitted(arima), null = "Trend") #tick

summary(arima)

round(cbind(coef = arima$coef,
            LI = arima$coef - 1.06 * sqrt(diag(vcov(arima))),
            UI = arima$coef + 1.06 * sqrt(diag(vcov(arima)))), 
      digits = 1)

plot.drug.tr <- function(){plotit(data, data$Trafficking.of.drugs,
                                  data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                  data$S, "Drug trafficking")}
plot.drug.tr()

#model and plot public order
modelit(data, data$order.weapons,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$order.weapons,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$order.weapons,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

#estimate model (2, 2, 0)
arima <- arima(x = ts(data$order.weapons), 
               xreg = cbind(data$D1, data$P1, data$D2, data$P2, data$D3, data$P3),
               order = c(2, 2, 0))

durbinWatsonTest(as.vector(arima$residuals)) #tick

Box.test(as.vector(arima$residuals), type = c("Ljung-Box")) #tick

kpss.test(fitted(arima), null = "Trend") #tick

summary(arima)

round(cbind(coef = arima$coef,
            LI = arima$coef - 1.06 * sqrt(diag(vcov(arima))),
            UI = arima$coef + 1.06 * sqrt(diag(vcov(arima)))), 
      digits = 1)

plot.order <- function(){plotit(data, data$order.weapons,
                                data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                data$S, "Public order and possession of weapons")}
plot.order()

#model and plot criminal damage
modelit(data, data$Criminal.damage,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$Criminal.damage,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$Criminal.damage,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.coef(data, data$Criminal.damage,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

plot.damage <- function(){plotit(data, data$Criminal.damage,
                                 data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                 data$S, "Criminal damage")}
plot.damage()

#save figure: 9.49 x 5.60 inches

##burglary
data <- data %>%
  mutate(domestic.burg = Theft...burglary.residential + Theft...domestic.burglary,
         nondomestic.burg = Theft...burglary.business...community + Theft...non.domestic.burglary)
par(mfrow=c(1,2), mai = c(0.7, 0.5, 0.5, 0.1))

#model and plot domestic burglary
modelit(data, data$domestic.burg,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$domestic.burg,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$domestic.burg,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

#estimate model (1, 1, 0)
arima <- arima(x = ts(data$domestic.burg), 
               xreg = cbind(data$D1, data$P1, data$D2, data$P2, data$D3, data$P3),
               order = c(1, 1, 0))

durbinWatsonTest(as.vector(arima$residuals)) #tick

Box.test(as.vector(arima$residuals), type = c("Ljung-Box")) #tick

kpss.test(fitted(arima), null = "Trend") #tick

summary(arima)

round(cbind(coef = arima$coef,
            LI = arima$coef - 1.06 * sqrt(diag(vcov(arima))),
            UI = arima$coef + 1.06 * sqrt(diag(vcov(arima)))), 
      digits = 1)

plot.burgl.dom <- function(){plotit(data, data$domestic.burg,
                                    data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                    data$S, "Residential burglary")}
plot.burgl.dom()

#model and plot non-domestic burglary
modelit(data, data$nondomestic.burg,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$nondomestic.burg,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$nondomestic.burg,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

#estimate model (2, 2, 0)
arima <- arima(x = ts(data$nondomestic.burg), 
               xreg = cbind(data$D1, data$P1, data$D2, data$P2, data$D3, data$P3),
               order = c(2, 2, 0))

durbinWatsonTest(as.vector(arima$residuals)) #tick

Box.test(as.vector(arima$residuals), type = c("Ljung-Box")) #tick

kpss.test(fitted(arima), null = "Trend") #tick

summary(arima)

round(cbind(coef = arima$coef,
            LI = arima$coef - 1.06 * sqrt(diag(vcov(arima))),
            UI = arima$coef + 1.06 * sqrt(diag(vcov(arima)))), 
      digits = 1)

plot.burgl.nondom <- function(){plotit(data, data$nondomestic.burg,
                                       data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                       data$S, "Non-residential burglary")}
plot.burgl.nondom()

#save figure: 9.49 x 3.60 inches

##theft
data <- data %>%
  mutate(All.other.theft.offences = All.other.theft.offences + Theft...shoplifting)
par(mfrow=c(2,2), mai = c(0.7, 0.5, 0.5, 0.1))

#model and plot theft from the person
modelit(data, data$Theft.from.the.person,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$Theft.from.the.person,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$Theft.from.the.person,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

#estimate model (4, 3, 0)
arima <- arima(x = ts(data$Theft.from.the.person), 
               xreg = cbind(data$D1, data$P1, data$D2, data$P2, data$D3, data$P3),
               order = c(4, 3, 0))

durbinWatsonTest(as.vector(arima$residuals)) #tick

Box.test(as.vector(arima$residuals), type = c("Ljung-Box")) #tick

kpss.test(fitted(arima), null = "Trend") #tick

summary(arima)

round(cbind(coef = arima$coef,
            LI = arima$coef - 1.06 * sqrt(diag(vcov(arima))),
            UI = arima$coef + 1.06 * sqrt(diag(vcov(arima)))), 
      digits = 1)

plot.theft.p <- function(){plotit(data, data$Theft.from.the.person,
                                  data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                  data$S, "Theft from person")}
plot.theft.p()

#model and plot bicycle theft
modelit(data, data$Bicycle.theft,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$Bicycle.theft,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$Bicycle.theft,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.coef(data, data$Bicycle.theft,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

plot.theft.b <- function(){plotit(data, data$Bicycle.theft,
                                  data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                  data$S, "Bicycle theft")}
plot.theft.b()

#model and plot theft of/from vehicle
modelit(data, data$Theft...vehicle.offences,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$Theft...vehicle.offences,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$Theft...vehicle.offences,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.coef(data, data$Theft...vehicle.offences,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

plot.theft.v <- function(){plotit(data, data$Theft...vehicle.offences,
                                  data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                  data$S, "Theft of/from vehicle")}
plot.theft.v()

#model and plot shoplifting
modelit(data, data$Theft...shoplifting,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$Theft...shoplifting,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$Theft...shoplifting,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

#estimate model (1, 2, 8)
arima <- arima(x = ts(data$Theft...shoplifting), 
                    xreg = cbind(data$D1, data$P1, data$D2, data$P2, data$D3, data$P3),
                    order = c(1, 2, 8))

durbinWatsonTest(as.vector(arima$residuals)) #tick

Box.test(as.vector(arima$residuals), type = c("Ljung-Box")) #tick

kpss.test(fitted(arima), null = "Trend") #tick

summary(arima)

round(cbind(coef = arima$coef,
            LI = arima$coef - 1.06 * sqrt(diag(vcov(arima))),
            UI = arima$coef + 1.06 * sqrt(diag(vcov(arima)))), 
      digits = 1)

plot.shoplift <- function(){plotit(data, data$Theft...shoplifting,
                                   data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                   data$S, "Shoplifting")}
plot.shoplift()

#save figure: 9.49 x 5.60 inches

##fraud and cyber
data <- data %>%
  mutate_all(~replace(., is.na(.), 0)) %>%
  mutate(other.fraud = False.Accounting + Bankruptcy.and.Insolvency + 
           Department.of.Works.and.Pensions..DWP..Fraud + Fraudulent.Applications.for.Grants.from.Government.Funded.Organisations +
           HM.Revenue...Customs.Fraud..HMRC. + Pension.Fraud.by.Pensioner..or.their.estates. +
           Pension.Fraud.committed.on.Pensions + Pension.Liberation.Fraud +
           Other.Regulatory.Fraud + Fraud.by.Failing.to.Disclose.Information +
           Fraud.by.Abuse.of.Position.of.Trust +
           Dishonestly.retaining.a.wrongful.credit +
           Insurance.Related.Fraud + Insurance.Broker.Fraud +
           Telecom.Industry.Fraud..Misuse.of.Contracts. +
           Corporate.Employee.Fraud + Business.Trading.Fraud + 
           DVLA.Driving.Licence.Application.Fraud + None.of.the.Above +
           Application..Fraud..excluding.Mortgages. + Mortgage.Related.Fraud +
           Cheque..Plastic.Card.and.Online.Bank.Accounts..not.PSP. +
           Mandate.Fraud + Counterfeit.Cashiers.Cheques + Ticket.Fraud +
           Retail.Fraud + Charity.Fraud,
         investment.advance.fee = X.419..Advance.Fee.Fraud + Lottery.Scams + Fraud.Recovery +
           Inheritance.Fraud + Rental.Fraud + Other.Advance.Fee.Frauds + Lender.Loan.Fraud +
           Dating.Scam + Share.sales.or.Boiler.Room.Fraud + Pyramid.or.Ponzi.Schemes +
           Prime.Bank.Guarantees + Time.Shares.and.Holiday.Club.Fraud +
           Other.Financial.Investment,
         Consumer.fraud.online = Online.Shopping.and.Auctions + Computer.Software.Service.Fraud +
           Consumer.Phone.Fraud,
         consumer.fraud.exc.on.shop = Door.to.Door.Sales.and.Bogus.Tradesmen +
           Other.Consumer.Non.Investment.Fraud,
         cyberdependent = Denial.of.Service.Attack + Denial.of.Service.Attack..Extortion +
           Hacking...Server + Hacking...Personal + Hacking...Social.Media.and.Email +
           Hacking...PBX...Dial.Through + Hacking.Extortion + Computer.Virus...Malware...Spyware
  )
par(mfrow=c(3,2), mai = c(0.5, 0.3, 0.5, 0.1))

#model and plot investment and advance fee fraud
modelit(data, data$investment.advance.fee,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$investment.advance.fee,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$investment.advance.fee,
           data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.coef(data, data$investment.advance.fee,
             data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

plot.investment.fraud <- function(){plotit(data, data$investment.advance.fee,
                                         data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                         data$S, "Investment and advance fee fraud")}
plot.investment.fraud()

#model and plot consumer fraud excluding online shopping
modelit(data, data$consumer.fraud.exc.on.shop,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$consumer.fraud.exc.on.shop,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$consumer.fraud.exc.on.shop,
           data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

#estimate model (4, 5, 0)
arima <- arima(x = ts(data$consumer.fraud.exc.on.shop), 
               xreg = cbind(data$D1, data$P1, data$D2, data$P2, data$D3, data$P3),
               order = c(4, 5, 0))

durbinWatsonTest(as.vector(arima$residuals)) #tick

Box.test(as.vector(arima$residuals), type = c("Ljung-Box")) #tick

kpss.test(fitted(arima), null = "Trend") #tick

summary(arima)

round(cbind(coef = arima$coef,
            LI = arima$coef - 1.06 * sqrt(diag(vcov(arima))),
            UI = arima$coef + 1.06 * sqrt(diag(vcov(arima)))), 
      digits = 1)

plot.consumer.off.fraud <- function(){plotit(data, data$consumer.fraud.exc.on.shop,
                                           data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                           data$S, "Consumer fraud offline")}
plot.consumer.off.fraud()

#model and plot online consumer fraud
modelit(data, data$Consumer.fraud.online,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$Consumer.fraud.online,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$Consumer.fraud.online,
           data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

#estimate model (4, 5, 0)
arima <- arima(x = ts(data$Consumer.fraud.online),
               xreg = cbind(data$D1, data$P1, data$D2, data$P2, data$D3, data$P3),
               order = c(4, 5, 0))

durbinWatsonTest(as.vector(arima$residuals)) #tick

Box.test(as.vector(arima$residuals), type = c("Ljung-Box")) #tick

kpss.test(fitted(arima), null = "Trend") #tick

summary(arima)

round(cbind(coef = arima$coef,
            LI = arima$coef - 1.06 * sqrt(diag(vcov(arima))),
            UI = arima$coef + 1.06 * sqrt(diag(vcov(arima)))), 
      digits = 1)

plot.consumer.on.fraud <- function(){plotit(data, data$Consumer.fraud.online,
                                             data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                             data$S, "Consumer fraud online")}
plot.consumer.on.fraud()

#model and plot other fraud
modelit(data, data$other.fraud,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$other.fraud,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$other.fraud,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3) #tick

#estimate model (5, 4, 0)
arima <- arima(x = ts(data$other.fraud), 
               xreg = cbind(data$D1, data$P1, data$D2, data$P2, data$D3, data$P3),
                 order = c(5, 4, 0))

durbinWatsonTest(as.vector(arima$residuals)) #tick

Box.test(as.vector(arima$residuals), type = c("Ljung-Box")) #tick

kpss.test(fitted(arima), null = "Trend") #tick

summary(arima)

round(cbind(coef = arima$coef,
            LI = arima$coef - 1.06 * sqrt(diag(vcov(arima))),
            UI = arima$coef + 1.06 * sqrt(diag(vcov(arima)))), 
      digits = 1)

plot.other.fraud <- function(){plotit(data, data$other.fraud,
                                            data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                            data$S, "Other fraud")}
plot.other.fraud()

#model and plot cyber-dependent crime
modelit(data, data$cyberdependent,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3, data$S)

arimait.model(data, data$cyberdependent,
        data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.tests(data, data$cyberdependent,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

arimait.coef(data, data$cyberdependent,
              data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3)

plot.cyber <- function(){plotit(data, data$cyberdependent,
                                data$T, data$D1, data$P1, data$D2, data$P2, data$D3, data$P3,
                                data$S, "Cyber-dependent crime")}
plot.cyber()

#save figure: 8.49 x 5.60 inches

#load Google Mobility data
mobility.2020 <- read.csv(here("data/2020_GB_Region_Mobility_Report.csv"))
mobility.2021 <- read.csv(here("data/2021_GB_Region_Mobility_Report.csv"))
mobility <- rbind(mobility.2020, mobility.2021)

#select Belfast
mobility <- mobility %>%
  filter(sub_region_1 == "Belfast")

#remove June
mobility <- mobility[1:472, ]

#plot Google Mobility trends
par(mfrow=c(3,2), mai = c(0.1, 0.3, 0.2, 0.1))

plot( mobility$retail_and_recreation_percent_change_from_baseline,
        bty = "n",
        col = gray(0.5, 0.5), pch=19,
        xlab = "", xaxt = "n",
        ylab = "% change",
        main = "Retails and recreation")

abline(h = 0, col = "grey")
abline( v = 41, col="darkorange2", lty = 2 )
abline( v = 245, col="darkorange2", lty = 2 )
abline( v = 329, col="darkorange2", lty = 2 )

plot( mobility$grocery_and_pharmacy_percent_change_from_baseline,
      bty = "n",
      col = gray(0.5, 0.5), pch=19,
      xlab = "", xaxt = "n",
      ylab = "% change",
      main = "Grocery and pharmacy")

abline(h = 0, col = "grey")
abline( v = 41, col="darkorange2", lty = 2 )
abline( v = 245, col="darkorange2", lty = 2 )
abline( v = 329, col="darkorange2", lty = 2 )

text(20, -65, "First lockdown \n 26/03/20", srt = 90, cex = 0.9)
text(223, -60, "Second lockdown \n 16/10/20", srt = 90, cex = 0.9)
text(355, -65, "Stay-at-home \n order \n 08/01/21", srt = 90, cex = 0.9)

plot( mobility$parks_percent_change_from_baseline,
      bty = "n",
      col = gray(0.5, 0.5), pch=19,
      xlab = "", xaxt = "n",
      ylab = "% change",
      main = "Parks")

abline(h = 0, col = "grey")
abline( v = 41, col="darkorange2", lty = 2 )
abline( v = 245, col="darkorange2", lty = 2 )
abline( v = 329, col="darkorange2", lty = 2 )

plot( mobility$transit_stations_percent_change_from_baseline,
      bty = "n",
      col = gray(0.5, 0.5), pch=19,
      xlab = "", xaxt = "n",
      ylab = "% change",
      main = "Transit stations")

abline(h = 0, col = "grey")
abline( v = 41, col="darkorange2", lty = 2 )
abline( v = 245, col="darkorange2", lty = 2 )
abline( v = 329, col="darkorange2", lty = 2 )

plot( mobility$workplaces_percent_change_from_baseline,
      bty = "n",
      col = gray(0.5, 0.5), pch=19,
      xlab = "", xaxt = "n",
      ylab = "% change",
      main = "Workplaces")

abline(h = 0, col = "grey")
abline( v = 41, col="darkorange2", lty = 2 )
abline( v = 245, col="darkorange2", lty = 2 )
abline( v = 329, col="darkorange2", lty = 2 )

plot( mobility$residential_percent_change_from_baseline,
      bty = "n",
      col = gray(0.5, 0.5), pch=19,
      xlab = "", xaxt = "n",
      ylab = "% change",
      main = "Residential")

abline(h = 0, col = "grey")
abline( v = 41, col="darkorange2", lty = 2 )
abline( v = 245, col="darkorange2", lty = 2 )
abline( v = 329, col="darkorange2", lty = 2 )

#save figure: 8.49 x 5.60 inches

par(mfrow=c(3,2), mai = c(0.1, 0.3, 0.2, 0.1))

plot( mobility$workplaces_percent_change_from_baseline,
      bty = "n",
      col = gray(0.5, 0.5), pch=19,
      xlab = "", xaxt = "n",
      ylab = "% change",
      main = "Workplaces")

abline(h = 0, col = "grey")
abline( v = 41, col="darkorange2", lty = 2 )
abline( v = 245, col="darkorange2", lty = 2 )
abline( v = 329, col="darkorange2", lty = 2 )

axis(side=1, at = c(seq(from = 0, to = length(mobility$date2), by = 8)), labels = FALSE)
axis(1, at  = 1:length(mobility$date2), 
     labels = c(mobility$date2),
     cex.axis = 1, las = 2,
     tick = FALSE)

plot( mobility$residential_percent_change_from_baseline,
      bty = "n",
      col = gray(0.5, 0.5), pch=19,
      xlab = "", xaxt = "n",
      ylab = "% change",
      main = "Residential")

abline(h = 0, col = "grey")
abline( v = 41, col="darkorange2", lty = 2 )
abline( v = 245, col="darkorange2", lty = 2 )
abline( v = 329, col="darkorange2", lty = 2 )

axis(side=1, at = c(seq(from = 0, to = length(mobility$date2), by = 8)), labels = FALSE)
axis(1, at  = 1:length(mobility$date2), 
     labels = c(mobility$date2),
     cex.axis = 1, las = 2,
     tick = FALSE)

#save figure: 8.49 x 5.60 inches
