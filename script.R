
#######################
#
# Crime and COVID in NI
#
######################

rm(list=ls())

#load packages
library(here)
library(dplyr)

#load data
data <- read.csv(here("data/data_rev.csv"))

#function to obtain model results
modelit <- function(dataset, Y, T, D1, P1, D2, P2){
  
  #estimate temporal model
  ts <- lm(Y ~ T + D1 + P1 + D2 + P2, data = dataset)
  
  #print model results
  summary(ts)
  
}

#function to automate plotting
plotit <- function(dataset, Y, T, D1, P1, D2, P2, title){
  
  #estimate temporal model
  ts <- lm(Y ~ T + D1 + P1 + D2 + P2, data = dataset)
  
  #print model results
  summary(ts)
  
  #predict from the model
  pred1 <- predict(ts, dataset)
  
  #create new dataset where D and P are 0
  datanew <- as.data.frame(cbind(T = rep(1 : length(dataset)), 
                                 D1 = rep(0), 
                                 P1 = rep(0),
                                 D2 = rep(0),
                                 P2 = rep(0)))
  
  #predict counterfactual
  pred2 <- predict(ts, datanew)
  
  #design plot
  plot( Y,
        bty = "n",
        col = gray(0.5, 0.5), pch=19,
        ylim = c(0, max(Y)), xlim=c(0, length(dataset)),
        xlab = "Time (months)", 
        ylab = "Crime",
        main = title)
  
  #draw lines
  lines( rep(1:59), pred1[1:59], col = "dodgerblue4", lwd = 3 )
  lines( rep(60:69), pred1[60:69], col="brown2", lwd = 3 )
  lines( rep(70:length(pred1)), pred1[70:length(pred1)], col="brown2", lwd = 3 )
  lines( rep(60:length(pred2)), pred2[60:length(pred2)], col="dodgerblue2", lwd = 3, lty = 5 ) 
  abline( v=59.5, col="darkorange2", lty=2 )
  abline( v=69.5, col="darkorange2", lty=2 )
  
}

##all crime

#plot all crime
plot.all <- function(){plotit(data, data$All.crime,
                                data$T, data$D1, data$P1, 
                                data$D2, data$P2, "All crime")}
plot.all()

#all text
text(52, 1000, "First lockdown \n 26/03/20")
text(79, 2000, "Stay-at-home order \n 05/01/21")

#save figure: 9.49 x 3.60 inches

##violence
par(mfrow=c(2,2))

#model and plot violence injury
modelit(data, data$Violence.with.injury..including.homicide...death.serious.injury.by.unlawful.driving.,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.vio.i <- function(){plotit(data, data$Violence.with.injury..including.homicide...death.serious.injury.by.unlawful.driving.,
                                data$T, data$D1, data$P1, 
                                data$D2, data$P2, "Violence with injury")}
plot.vio.i()

#plot violence no injury
modelit(data, data$Violence.without.injury,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.vio.ni <- function(){plotit(data, data$Violence.without.injury,
                                 data$T, data$D1, data$P1, data$D2, data$P2, "Violence without injury")}
plot.vio.ni()

#sexual crime
modelit(data, data$Sexual.offences,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.sex <- function(){plotit(data, data$Sexual.offences,
                              data$T, data$D1, data$P1, data$D2, data$P2, "Sexual offences")}
plot.sex()

#Harassment
modelit(data, data$Harassment,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.har <- function(){plotit(data, data$Harassment,
                              data$T, data$D1, data$P1, data$D2, data$P2, "Harassment")}
plot.har()

#save figure: 9.49 x 5.60 inches

##drugs, damage and public order
data <- data %>%
  mutate(order.weapons = Public.order.offences + Possession.of.weapons.offences)
par(mfrow=c(2,2))

#model and plot possession of drugs
modelit(data, data$Possession.of.drugs,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.drug.po <- function(){plotit(data, data$Possession.of.drugs,
                                  data$T, data$D1, data$P1, data$D2, data$P2, "Possession of drugs")}
plot.drug.po()

#model and plot drug trafficking
modelit(data, data$Trafficking.of.drugs,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.drug.tr <- function(){plotit(data, data$Trafficking.of.drugs,
                                  data$T, data$D1, data$P1, data$D2, data$P2, "Drug trafficking")}
plot.drug.tr()

#model and plot public order
modelit(data, data$order.weapons,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.order <- function(){plotit(data, data$order.weapons,
                                data$T, data$D1, data$P1, data$D2, data$P2, "Public order and possession of weapons")}
plot.order()

#model and plot criminal damage
modelit(data, data$Criminal.damage,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.damage <- function(){plotit(data, data$Criminal.damage,
                                 data$T, data$D1, data$P1, data$D2, data$P2, "Criminal damage")}
plot.damage()

#save figure: 9.49 x 5.60 inches

##burglary
data <- data %>%
  mutate(domestic.burg = Theft...burglary.residential + Theft...domestic.burglary,
         nondomestic.burg = Theft...burglary.business...community + Theft...non.domestic.burglary)
par(mfrow=c(2,1))

#model and plot domestic burglary
modelit(data, data$domestic.burg,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.burgl.dom <- function(){plotit(data, data$domestic.burg,
                                    data$T, data$D1, data$P1, data$D2, data$P2, "Residential burglary")}
plot.burgl.dom()

#model and plot non-domestic burglary
modelit(data, data$nondomestic.burg,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.burgl.nondom <- function(){plotit(data, data$nondomestic.burg,
                                       data$T, data$D1, data$P1, data$D2, data$P2, "Non-residential burglary")}
plot.burgl.nondom()

#save figure: 6.49 x 5.60 inches

##theft and robbery
par(mfrow=c(3,2))

#model and plot theft from the person
modelit(data, data$Theft.from.the.person,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.theft.p <- function(){plotit(data, data$Theft.from.the.person,
                                  data$T, data$D1, data$P1, data$D2, data$P2, "Theft from person")}
plot.theft.p()

#model and plot bicycle theft
modelit(data, data$Bicycle.theft,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.theft.b <- function(){plotit(data, data$Bicycle.theft,
                                  data$T, data$D1, data$P1, data$D2, data$P2, "Bicycle theft")}
plot.theft.b()

#model and plot theft of/from vehicle
modelit(data, data$Theft...vehicle.offences,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.theft.v <- function(){plotit(data, data$Theft...vehicle.offences,
                                  data$T, data$D1, data$P1, data$D2, data$P2, "Theft of/from vehicle")}
plot.theft.v()

#model and plot shoplifting
modelit(data, data$Theft...shoplifting,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.shoplift <- function(){plotit(data, data$Theft...shoplifting,
                                   data$T, data$D1, data$P1, data$D2, data$P2, "Shoplifting")}
plot.shoplift()

#model and plot robbery
modelit(data, data$Robbery,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.robbery <- function(){plotit(data, data$Robbery,
                                  data$T, data$D1, data$P1, data$D2, data$P2, "Robbery")}
plot.robbery()

#model and plot all other theft
modelit(data, data$All.other.theft.offences,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.theft.o <- function(){plotit(data, data$All.other.theft.offences,
                                  data$T, data$D1, data$P1, data$D2, data$P2, "All other theft")}
plot.theft.o()

#save figure: 8.49 x 5.60 inches

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
           DVLA.Driving.Licence.Application.Fraud + None.of.the.Above,
         advance.fee.fraud = X.419..Advance.Fee.Fraud + Lottery.Scams + Fraud.Recovery +
           Inheritance.Fraud + Rental.Fraud + Other.Advance.Fee.Frauds + Lender.Loan.Fraud +
           Dating.Scam,
         Online.Shopping.and.Auctions = Online.Shopping.and.Auctions,
         consumer.fraud.exc.on.shop = Consumer.Phone.Fraud + Door.to.Door.Sales.and.Bogus.Tradesmen +
           Other.Consumer.Non.Investment.Fraud + Computer.Software.Service.Fraud,
         cyberdependent = Denial.of.Service.Attack + Denial.of.Service.Attack..Extortion +
           Hacking...Server + Hacking...Personal + Hacking...Social.Media.and.Email +
           Hacking...PBX...Dial.Through + Hacking.Extortion,
         investment.banking.credit.fraud = Cheque..Plastic.Card.and.Online.Bank.Accounts..not.PSP. +
           Application..Fraud..excluding.Mortgages. + Mortgage.Related.Fraud +
           Mandate.Fraud + Counterfeit.Cashiers.Cheques +
           Share.sales.or.Boiler.Room.Fraud + Pyramid.or.Ponzi.Schemes +
           Prime.Bank.Guarantees + Time.Shares.and.Holiday.Club.Fraud +
           Other.Financial.Investment
  )
par(mfrow=c(3,2))

#model and plot online shopping fraud
modelit(data, data$Online.Shopping.and.Auctions,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.online.shop <- function(){plotit(data, data$Online.Shopping.and.Auctions,
                                      data$T, data$D1, data$P1, data$D2, data$P2, "Online shopping fraud")}
plot.online.shop()

#model and plot advance fee fraud
modelit(data, data$advance.fee.fraud,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.advance.fee <- function(){plotit(data, data$advance.fee.fraud,
                                      data$T, data$D1, data$P1, data$D2, data$P2, "Advance fee fraud")}
plot.advance.fee()

#model and plot consumer fraud
modelit(data, data$consumer.fraud.exc.on.shop,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.consumer.fraud <- function(){plotit(data, data$consumer.fraud.exc.on.shop,
                                         data$T, data$D1, data$P1, data$D2, data$P2, "Consumer fraud")}
plot.consumer.fraud()

#model and plot investment and credit fraud
modelit(data, data$investment.banking.credit.fraud,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.invest.fraud <- function(){plotit(data, data$investment.banking.credit.fraud,
                                       data$T, data$D1, data$P1, data$D2, data$P2, "Investment and credit fraud")}
plot.invest.fraud()

#model and plot other fraud
modelit(data, data$other.fraud,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.other.fraud <- function(){plotit(data, data$other.fraud,
                                      data$T, data$D1, data$P1, data$D2, data$P2, "Other fraud")}
plot.other.fraud()

#model and plot cyber-dependent crime
modelit(data, data$cyberdependent,
        data$T, data$D1, data$P1, data$D2, data$P2)

plot.cyber <- function(){plotit(data, data$cyberdependent,
                                data$T, data$D1, data$P1, data$D2, data$P2, "Cyber-dependent crime")}
plot.cyber()

#save figure: 8.49 x 5.60 inches
