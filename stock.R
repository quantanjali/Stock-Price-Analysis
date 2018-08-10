#******************************************************************************************************************************************************************************
# Developer : Kumari Anjali
# Stock analysis for 1.Goldman Sachs(GS) 2. Morgan Stanley(MS) 3. Citigroup(C)  4. Wells Fargo Securities(WFC) 5. JP Morgan Chase(JPM)
#******************************************************************************************************************************************************************************

# NOTE : Analysis for 1.Goldman Sachs(GS) 2. Morgan Stanley(MS) 3. Citigroup(C)  4. Wells Fargo Securities(WFC) 5. JP Morgan Chase(JPM)
# All relevant Standard Packages are mentioned below. If its not installed on your system, please install on prompt by this program
# Platform : This code has been developed on R 3.4.3 GUI 1.70 El Capitan build (7463), Normal R version 3.4.3 for MAC OS El Capitan
# Disclaimer : I have developed and tested these codes on MacBook Pro, MAC OS - El Capitan,where its running fine. There may be some error on other OS platform


# Installing Library Function, if required

if (!require(quantmod)) install.packages('quantmod')
if (!require(xts)) install.packages('xts')
if (!require(zoo)) install.packages('zoo')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(graphics)) install.packages('graphics')
if (!require(BatchGetSymbols)) install.packages('BatchGetSymbols')
if (!require(PerformanceAnalytics)) install.packages('PerformanceAnalytics')
if (!require(readr)) install.packages('readr')
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(stringr)) install.packages('stringr')
if (!require(data.table)) install.packages('data.table')

# Loading Library Function

library(quantmod)
library(xts)
library(zoo)
library(ggplot2)
library(graphics)
library(BatchGetSymbols)
library(PerformanceAnalytics)
library(readr)
library(tidyverse)
library(stringr)
library(reshape2)
library(data.table)

#******************************************************************************************************************************************************************************
# Downloaded data for last 1 years for a set of the any five stock tickers belonging to the same industry segment, i.e. Investment Banks . 
# Data downloaded from Yahoo Finance for 1.Goldman Sachs(GS) 2. Morgan Stanley(MS) 3. Citigroup(C)  4. Wells Fargo Securities(WFC) 5. JP Morgan Chase(JPM)
#******************************************************************************************************************************************************************************

tickers <- c('GS','MS','C','WFC','JPM')
getSymbols(tickers, src='yahoo', from="2017-03-07", to="2018-03-07")
Stocks <- list(GS, MS, C, WFC, JPM)
Stocks

#**********************************************************************************************************************************
# Calculate Monthly returns of downloaded stock over the period under study 
#**********************************************************************************************************************************


# Monthly Return for each stocks on the basis of adjusted closing price
# Separating Daily Adjusted Closing data from OHLC data table
# Calculation and displaying monthly Arithmetic returns

GoldmanSach_MReturn<-monthlyReturn(GS$GS.Adjusted, subset=NULL, type="arithmetic", leading=TRUE)
MorganStanley_MReturn<-monthlyReturn(MS$MS.Adjusted, subset=NULL, type="arithmetic", leading=TRUE)
Citigroup_MReturn<-monthlyReturn(C$C.Adjusted, subset=NULL, type="arithmetic", leading=TRUE)
WellsFargo_MReturn<-monthlyReturn(WFC$WFC.Adjusted, subset=NULL, type="arithmetic", leading=TRUE)
JPMC_MReturn<-monthlyReturn(JPM$JPM.Adjusted, subset=NULL, type="arithmetic", leading=TRUE)

Monthly_Return <- list(GoldmanSach_MReturn, MorganStanley_MReturn, Citigroup_MReturn, WellsFargo_MReturn, JPMC_MReturn)
Monthly_Return


#******************************************************************************************************************************************************************************
# Using a combination function, calculate the monthly returns of an equally weighted portfolio consisting of any 3 of the five stocks in question 
#******************************************************************************************************************************************************************************

Stock_MReturn <- data.table(GoldmanSach_MReturn, MorganStanley_MReturn, Citigroup_MReturn, WellsFargo_MReturn, JPMC_MReturn)
Stock_MReturn

Portfolio_Combination <- combn(c('GS','MS','C','WFC','JPM'),3)
Portfolio_Combination
Portfolio_Monthly_Return <- combn(Stock_MReturn, 3, rowSums, simplify = TRUE)
Portfolio_Monthly_Return

#**********************************************************************************************************************************
# Graphically represent the cumulative monthly returns of each of the possible portfolios through line plots 
#*********************************************************************************************************************************


# Cumulative monthly returns of each of the possible portfolios 
Cumulative_MReturn <- (Return.cumulative(Portfolio_Monthly_Return[,1:10]))
Cumulative_MReturn
Cumulative_Return <- as.vector (Cumulative_MReturn)
Cumulative_Return

# Line Plot to represent the cumulative monthly returns of each of the possible portfolios 
plot.zoo(Cumulative_Return, col = "red", xlab = "Portfolio", ylab = "Cumulative Return")

# Line Plot to represent the cumulative monthly returns of each of the possible portfolios 
barplot(Cumulative_MReturn, type="l", col= "royalblue" , space= 0.05, width =  1 ,
						border = "yellow", 
						main="Cumulative Monthly Return of possible Portfolio", 
						xlab=" Portfolio 1 to 10 ->", 
						ylab=" Cumulative Monthly Return",
						xaxt='n')
axis(1, at=c(0,1, 2, 3, 4, 5, 6, 7, 8, 9, 10))



#*************************************************************************************************************************************************************************
# Calculate mean, median and standard deviation of monthly values for each of the portfolios in question and plot them on the same graph mentioned in step 4. 
#*************************************************************************************************************************************************************************

# Calculation of Mean, Median & Standard Deviation of 10 Portfolios
#*******************************************************************************

P <- data.table(Portfolio_Monthly_Return[,1:10])

# Portfolio Mean Calculation : 
Portfolio_Mean <- colMeans(Portfolio_Monthly_Return)
Portfolio_Mean

# Alternate Portfolio Mean Calculation : 
Mean_Portfolio <- c(mean(P$V1), mean(P$V2), mean(P$V3), mean(P$V4), mean(P$V5), mean(P$V6), mean(P$V7), mean(P$V8), mean(P$V9), mean(P$V10))

Mean_Portfolio 

# Portfolio Median Calculation : 
Median_Portfolio <- c(median(P$V1), median(P$V2), median(P$V3), median(P$V4), median(P$V5), median(P$V6), median(P$V7), median(P$V8), median(P$V9), median(P$V10))

Median_Portfolio 

# Portfolio Standard Deviation Calculation : 
StDev_Portfolio <- c(sd(P$V1), sd(P$V2), sd(P$V3), sd(P$V4), sd(P$V5), sd(P$V6), sd(P$V7), sd(P$V8), sd(P$V9), sd(P$V10))

StDev_Portfolio 
 

# Plot of MEAN of different portfolio at Bar Plot
#####################################################

colors <- c("red", "blue", "yellow", "orange", "green", "peachpuff")

barplot(Mean_Portfolio, type="l", col=colors, space= 0.05, width =  1 ,
						border = "yellow", 
						main="Mean for Overall Portfolio", 
						xlab=" Portfolio 1 to 10 ->", 
						ylab=" Mean of Portfolio Return",
						xaxt='n')
axis(1, at=c(0,1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
 					

# Plot of MEDIAN of different portfolio at Bar Plot
barplot(Median_Portfolio, type="l", col=colors, space= 0.05, width =  1 ,
						border = "yellow", 
						main="Median for Overall Portfolio", 
						xlab=" Portfolio 1 to 10 ->", 
						ylab=" Median of Portfolio Return",
						xaxt='n')
axis(1, at=c(0,1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
 						
 						
# Plot of STANDARD DEVIATION of different portfolio at Bar Plot
barplot(StDev_Portfolio, type="l", col=colors, space= 0.05, width =  1 ,
						border = "yellow", 
						main="Standard Deviation for Overall Portfolio", 
						xlab=" Portfolio 1 to 10 ->", 
						ylab=" Standard Deviation of Portfolio Return",
						xaxt='n')
axis(1, at=c(0,1, 2, 3, 4, 5, 6, 7, 8, 9, 10))


########################################################################
# Plot of Cumulative Monthly Return, Mean, Median and Standard Deviation
########################################################################

plot.zoo(Cumulative_Return, col = "red", xlab = "Portfolio", ylab = "Cumulative Return")
plot.zoo(Mean_Portfolio, col = "blue", xlab = "Portfolio", ylab = "Mean")
plot.zoo(Median_Portfolio, col = "green", xlab = "Portfolio", ylab = "Median")
plot.zoo(StDev_Portfolio, col = "peachpuff", xlab = "Portfolio", ylab = "Standard Deviation")

plot(Cumulative_Return,	type = "l",
				col = "red", 
				xlim= c(1, 10), 
				ylim = c(-0.1, 0.9), 
				xlab = "Portfolio", 
				ylab = "Return",
				lwd = 2, 
   			main = "Portfolio Combination : Statistics")

lines(Mean_Portfolio, type = "l", lwd = 2, col = "blue")
lines(Median_Portfolio, type = "l", lwd = 2, col = "green")
lines(StDev_Portfolio, type = "l", lwd = 2, col = "peachpuff")
# axis(side = 1, at = c(1,2,3,4,5,6,7,8,9,10), labels = c("P1","P2","P3","P4","P5","P6","P7","P8","9","P10" )

# location of legend within plot area
legend(x = "bottom", bty = "n", horiz = TRUE,  
		xjust = 0, yjust =0, "(x,y)", pch = 1, x.intersp = 0.5, cex = 0.75,
 		c("Cuml.Return","Mean ","Median","StdEv"), 
 		col = c("red", "blue", "green","peachpuff"),
 		lwd = c(2, 2, 2, 2))
 		


#*********************************************************************************************************************************
# Calculate the overall variance of all portfolio returns 
#*********************************************************************************************************************************


OverAll_Mean_P_Return = mean(Mean_Portfolio)       
OverAll_Mean_P_Return        # Overall Portfolio Return Mean calculation

OverAll_Median_P_Return = median(Median_Portfolio)     
OverAll_Median_P_Return      # Overall Portfolio Return Mean calculation

OverAll_StDev_P_Return = sd(StDev_Portfolio)       
OverAll_StDev_P_Return       # Standard Deviation calculation

Overall_Variance_P_Return = OverAll_StDev_P_Return^2       
Overall_Variance_P_Return    # Overall Variance calculation

var(Mean_Portfolio)

#**********************************************************************************************************************************
# The End
#**********************************************************************************************************************************

