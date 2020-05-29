library(data.table)
library(lubridate)
# pkgload::load_all(path= "/home/qsheng3/HedgeFundApp/packages/blotter")
# pkgload::load_all(path= "/home/qsheng3/HedgeFundApp/packages/quantstrat")

library(shinydashboard)
library(DT)
library(shiny)
library(shinyWidgets)
library(rhandsontable)
library(quantmod)
library(PortfolioAnalytics)
library(ROI)
require(ROI.plugin.quadprog)
require(ROI.plugin.glpk)
library(Rglpk)
library(DEoptim)
library(quantmod)
library(downloader)
# library(quantstrat)
library(tseries)

# rsconnect::appDependencies()

# portfolio.st <- "Port.Luxor"
account.st <- "Acct.Luxor"
strategy.st <- "Strat.Luxor"


date_choices = seq(as.Date("2010-01-01"), today(), by="1 month")
date_choices[length(date_choices)] = today()


# factorFile <- "http://mba.tuck.dartmouth.edu/pages/faculty/ken.french/ftp/F-F_Research_Data_Factors_daily_CSV.zip"
# download(factorFile, "F-F_Research_Data_Factors_daily_CSV.zip")
# unzip(zipfile="F-F_Research_Data_Factors_daily_CSV.zip")
# dataFF3 <- read.csv("F-F_Research_Data_Factors_daily.CSV",
# skip=4,header=T)
dataFF3 <- read.csv("factorsSummary.CSV",
                    skip=4,header=T)
# dataFF3=dataFF3[1:length(dataFF3[,1])-1,]
dataFF3=dataFF3[1:(length(dataFF3[,1])-2),]
ff3=xts(dataFF3[,2:(dim(dataFF3)[2])]/100,order.by=as.Date(dataFF3[,1],"%Y%m%d"))
factorDataGlobal = ff3[,-4]


df_sec3 = data.frame(
  Factors = colnames(factorDataGlobal),
  LowerBound = rep("-1", length(colnames(factorDataGlobal))),
  UpperBound = rep("2", length(colnames(factorDataGlobal))),
  # Factors = c("MKT", "SML", "HML", rep("",2)),
  # LowerBound = c(-1,-1,-1,rep("",2)),
  # UpperBound = c(2,2,2,rep("",2)),
  # stopLimLong = c(4,5,6,7,rep("",26)),
  # stopLimShort = c(1.5,2,3,4,rep("",26)),
  stringsAsFactors = FALSE)

colnames(df_sec3) = c("Factors", "Lower Limit", "Upper Limit")