{library(readxl)
library(data.table)
library(ggplot2)
library(lubridate)
library(reshape2)
library(knitr)
library(forecast)
library(tstools)
library(mFilter)
library(urca)
library(tseries)
library(lmtest)
library(boot)
library(latex2exp)
library(dynlm)
library(dyn)
library(MLmetrics)
library(adf)}

AMBU <- read.csv("~/Desktop/PBL/6. semester/EXam/AMBU.csv")
BAVA <- read.csv("~/Desktop/PBL/6. semester/EXam/BAVA.csv")
COLO <- read.csv("~/Desktop/PBL/6. semester/EXam/COLO.csv")
DANSKE <- read.csv("~/Desktop/PBL/6. semester/EXam/DANSKE.csv")
DEMANT <- read.csv("~/Desktop/PBL/6. semester/EXam/DEMANT.csv")
DSV <- read.csv("~/Desktop/PBL/6. semester/EXam/DSV.csv")
GMAB <- read.csv("~/Desktop/PBL/6. semester/EXam/GMAB.csv")
GN <- read.csv("~/Desktop/PBL/6. semester/EXam/GN.csv")
LUN <- read.csv("~/Desktop/PBL/6. semester/EXam/LUN.csv")
NZYM <- read.csv("~/Desktop/PBL/6. semester/EXam/NZYM.csv")
RBREW <- read.csv("~/Desktop/PBL/6. semester/EXam/RBREW.csv")
ROCK <- read.csv("~/Desktop/PBL/6. semester/EXam/ROCK.csv")
SIM <- read.csv("~/Desktop/PBL/6. semester/EXam/SIM.csv")
TRYG <- read.csv("~/Desktop/PBL/6. semester/EXam/TRYG.csv")
VEST <- read.csv("~/Desktop/PBL/6. semester/Vest.csv")
NOVO <- read.csv("~/Desktop/PBL/6. semester/NOVO-B.CO.csv")
CARL <- read.csv("~/Desktop/PBL/6. semester/CARL-B.CO.csv")
FLS <- read.csv("~/Desktop/PBL/6. semester/FLS.CO.csv")
MAER <- read.csv("~/Desktop/PBL/6. semester/MAERSKA.csv")

Stock <- MAER
{ttp <- Stock[33:503,] #503(2019/2020) #451(2018)
  y <- ts(ttp$Adj.Close, freq=365.25/7, start=decimal_date(ymd("2009-12-28"))) #365.25/7
  y <- (y[-1]-y[-length(y)])/y[-length(y)]
  train <- head(y, (0.8*length(y)))
  test <- tail(y, (0.2*length(y)))
data <- Stock[33:555,] #555 #607 #503
  y <- ts(data$Adj.Close, freq=365.25/7, start=decimal_date(ymd("2009-12-28"))) #365.25/7
  yy <- y
  y <- (y[-1]-y[-length(y)])/y[-length(y)]
  actual <- tail(y, 52)
data <- Stock[33:607,] #555 %607
  y2 <- ts(data$Adj.Close, freq=365.25/7, start=decimal_date(ymd("2009-12-28"))) #365.25/7
  yy2 <- y2
  y2 <- (y2[-1]-y2[-length(y2)])/y2[-length(y2)]
  #actual <- tail(y2, 52)
  }
  
adf.test(y, alternative = "stationary")

get_CV <- function(j,k){
  modelCV <- function(x, h){forecast(Arima(x, order=c(j,0,k)), h=h)}
  e <- tsCV(c(train,test), modelCV, h=1, initial = length(train)+1)
  RMSE <- sqrt(mean(e^2, na.rm=TRUE))
  adjRMSE <- sqrt((RMSE/(length(c(train, test))-(j+k))))
  return(list(RMSE = RMSE, adjRMSE = adjRMSE))
}

{i = 0
  MAPES <- matrix(ncol= 4, nrow = 42)
  colnames(MAPES) <- c("p","q", "RMSE", "adjRMSE")
  for (j in 0:5){
    for (k in 0:6){
      modell <- get_CV(j,k)
      i = i+1
      MAPES[i,1] = j
      MAPES[i, 2] = k
      MAPES[i,3] <- modell$RMSE
      MAPES[i,4] <- modell$adjRMSE
    }
  }
  
  MAPES}
min(tail(MAPES[,3],41));min(tail(MAPES[,4],41))
#AMBU = (3,5), BAVA = (0,1), COLO = (0,1), DANSKE = (2,3), DEMANT = (1,0), DSV = (0,1), GMAB = (2,4), 
#GN = (0,1), LUN = (1,1), NZYM = (1,0), RBREW = (2,2), ROCK = (1,1), SIM = (2,4), TRYG = (2,0)
 #VEST = (3, 5), NOVO = (2, 3), CARL = (1, 1), FLS = (3, 3), MAER = (0, 2) <- 2019/2020
#VEST = (0, 1), NOVO = (0, 1), CARL = (0, 3), FLS = (1, 0), MAER = (3, 0) <- 2018

#based on the above table, choose your model.

{train = head(y, (length(y)-52))
mod <- Arima(train, order = c(0,0,2)) #Write in new p and q
refit <- Arima(y, model=mod)}
predictions <- tail(refit$fitted, 52)

#results <- matrix(ncol = 19*3, nrow = 52)
colnames(results) <- c("AMBU actual", "AMBU ARIMA", "AMBU RF",
                       "BAVA actual", "BAVA ARIMA", "BAVA RF",
                       "COLO actual", "COLO ARIMA", "COLO RF",
                       "DANSKE actual", "DANSKE ARIMA", "DANSKE RF",
                       "DEMANT actual", "DEMANT ARIMA", "DEMANT RF",
                       "DSV actual", "DSV ARIMA", "DSV RF",
                       "GMAB actual", "GMAB ARIMA", "GMAB RF",
                       "GN actual", "GN ARIMA", "GN RF",
                       "LUN actual", "LUN ARIMA", "LUN RF",
                       "NZYM actual", "NZYM ARIMA", "NZYM RF",
                       "RBREW actual", "RBREW ARIMA", "RBREW RF",
                       "ROCK actual", "ROCK ARIMA", "ROCK RF",
                       "SIM actual", "SIM ARIMA", "SIM RF",
                       "TRYG actual", "TRYG ARIMA", "TRYG RF",
                      "Vactual", "VARIMA", "Vest RF",
                     "Novo actual", "Novo ARIMA", "Novo RF",
                     "Carl actual", "Carl ARIMA", "Carl RF",
                     "FLS actual", "FLS ARIMA", "FLS RF",
                     "MAER actual", "MAER ARIMA", "MAER RF")
numb = 19
results[,numb*3-2] <- actual
results[,numb*3-1] <- predictions
write.table(results,file="resultsall.csv")
write.table(resultsnum,file="resultsnum.csv")


resultsnum <- read.csv("resultsnum.csv", sep="")
resultsreal[,13] <-tail(yy, 52)
resultsreal[,2] <- head(tail(yy, 53), 52)*(1+results[,2])
resultsreal <- read.csv("resultspct.csv", sep="")
write.table(results,file="results.csv")

plot <- ggplot(results, aes(Vactual, VARIMA, group=1)) +
               geom_point()


rfres <- read_excel("~/Downloads/Resultater.xls")

resultsnum[,c(15)] <- rfres[,5]
resultsreal[,15] <- rfres[,(15)]



resultsnum[,15] <- ((1+as.numeric(resultsreal[,15]))*as.numeric(head(tail(yy, 53), 52)))

Get_arima_m <- function(y, p, q, test, train, actual){
  model <- Arima(train, order = c(p,0,q))
  refit <- Arima(c(train, test), model= model)
  predicted <- tail(refit$fitted, length(test))
  
  
  RMSE <- RMSE(predicted, test)
  adjRMSE <- sqrt((RMSE/(length(c(train, test))-(p+q))))
  
  return(list(adjRMSE = adjRMSE, predicted = predicted, test = test, RMSE = RMSE, refit=refit))
}

{i = 0
  MAPES <- matrix(ncol= 4, nrow = 81)
  colnames(MAPES) <- c("p","q", "RMSE", "adjMSE")
  for (j in 0:8){
    for (k in 0:8){
      modell <- Get_arima_m(y,j,k, test, train, actual)
      i = i+1
      MAPES[i,1] = j
      MAPES[i, 2] = k
      MAPES[i,3] <- modell$RMSE
      MAPES[i,4] <- modell$adjRMSE
    }
  }
  
  MAPES}
min(MAPES[,4])



