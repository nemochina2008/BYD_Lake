library(data.table)
library(anytime)
library(lubridate)
library(ggpubr)
library(ggsci)
library(stR)
library(ggExtra)
library(AnomalyDetection)
library(reshape2)
library(stringr)
library(ggsignif)

setwd("X:\\R\\Lake_Allocation")
Data <- fread("Data.csv",header=T,sep=",",quote="")
Data$Date <- anytime::anydate(Data$Date)
Data$Water_Allocation <- as.factor(Data$Water_Allocation)

Data$month <- month(Data$Date)


########## discriptive statistics for mean,max,min,mode of NDVI in BYD
ggmean <- ggplot(Data) +
  geom_histogram(aes(Mean,fill = factor(Water_Allocation)),binwidth = 0.05,alpha = 0.9) +
  labs(fill='Water\nAllocation') +
  scale_fill_jco() +
  labs(x = expression('NDVI'[mean])) +
  theme_bw(15) 
  #theme(legend.position = "top")
 
ggmax <- ggplot(Data) +
  geom_histogram(aes(Max,fill = factor(Water_Allocation)),binwidth = 0.05,alpha = 0.9) +
  labs(fill='Water\nAllocation') +
  scale_fill_jco() +
  labs(x = expression('NDVI'[max])) +
  theme_bw(15) 

ggmin <- ggplot(Data) +
  geom_histogram(aes(Min,fill = factor(Water_Allocation)),binwidth = 0.05,alpha = 0.9) +
  labs(fill='Water\nAllocation') +
  scale_fill_jco() +
  labs(x = expression('NDVI'[min])) +
  theme_bw(15) 

ggmode <- ggplot(Data) +
  geom_histogram(aes(Mode,fill = factor(Water_Allocation)),binwidth = 0.05,alpha = 0.9) +
  labs(fill='Water\nAllocation') +
  scale_fill_jco() +
  labs(x = expression('NDVI'[mode])) +
  theme_bw(15) 

fig1.BYD <- ggarrange(ggmean,ggmax,ggmin,ggmode,
                          labels = c("a)", "b)", "c)", "d)"),ncol = 1, nrow=4)
ggsave(fig1.BYD, filename = "fig1.BYD.pdf", width = 6, height = 7)

##################### sTR - seasonal trend decompositon -- x must ts sytle
sMean  <- ts(Data$Mean,frequency=36,start=c(1998,10))

STRmean <- AutoSTR(sMean,robust = T,confidence = 0.95,trace = T)

#pdf("STR-BYD.pdf",width=6, height=22)
#plot(STRmode, dataPanels = 4,forecastPanels=0, randomColor="DarkGreen", vLines = 1998:2008, lwd = 2)  
#dev.off()
#plotBeta(STRmean, predictorN =1)

#plot(STRmean)


#plot(STRmean, forecastPanels=0, randomColor="DarkGreen", vLines = 1998:2008, lwd = 2)

#plot(STRmean, dataPanels = 1) 
#plotBeta(STRmean, predictorN = 2, dim = 2)
#beta = STRmean$output$predictors[[2]]$beta

#plot(STRmean$output$predictors[[1]]$beta)


########## offically extractiong and ploting

sMean  <- ts(Data$Mean,frequency=36,start=c(1998,10))
sMax   <- ts(Data$Max, frequency=36,start=c(1998,10))
sMin   <- ts(Data$Min, frequency=36,start=c(1998,10))
sMode  <- ts(Data$Mode,frequency=36,start=c(1998,10))

STRmean <- AutoSTR(sMean,robust = T,confidence = 0.95,trace = T)
STRmax  <- AutoSTR(sMax, robust = T,confidence = 0.95,trace = T)
STRmin  <- AutoSTR(sMin, robust = T,confidence = 0.95,trace = T)
STRmode <- AutoSTR(sMode,robust = T,confidence = 0.95,trace = T)

Trendmean <- STRmean$output$predictors[[1]]$data
Trendmax  <- STRmax$output$predictors[[1]]$data
Trendmin <- STRmin$output$predictors[[1]]$data
Trendmode <- STRmode$output$predictors[[1]]$data

# structural changes for change point detection # library(strucchange)
Trendmean  <- ts(Trendmean,frequency=36,start=c(1998,10))
Trendmax  <-  ts(Trendmax,frequency=36,start=c(1998,10))
Trendmin  <-  ts(Trendmin,frequency=36,start=c(1998,10))
Trendmode  <- ts(Trendmode,frequency=36,start=c(1998,10))

Trendmean_brk <- breakpoints(Trendmean ~ 1, h = 0.1)
Trendmax_brk <- breakpoints(Trendmax ~ 1, h = 0.1)
Trendmin_brk <- breakpoints(Trendmin ~ 1, h = 0.1)
Trendmode_brk <- breakpoints(Trendmode ~ 1, h = 0.1)

summary(Trendmean_brk) # determine the breaks
summary(Trendmax_brk) # determine the breaks
summary(Trendmin_brk) # determine the breaks
summary(Trendmode_brk) # determine the breaks

pdf("BICs.pdf",width = 10, height=9)
par(mfrow=c(2,2)) 
plot(Trendmean_brk, main = "BIC and Residual sum of Squares for NDVImean")
plot(Trendmax_brk, main = "BIC and Residual sum of Squares for NDVImax")
plot(Trendmin_brk, main = "BIC and Residual sum of Squares for NDVImin")
plot(Trendmode_brk, main = "BIC and Residual sum of Squares for NDVImost")
dev.off()

# The break dates are:
breakdates(Trendmean_brk, breaks = 4)
breakdates(Trendmax_brk, breaks = 6)
breakdates(Trendmin_brk, breaks = 8)
breakdates(Trendmode_brk, breaks = 6)

confint(Trendmean_brk, breaks = 4) # 95%confidence levels
confint(Trendmax_brk, breaks = 6) # 95%confidence levels
confint(Trendmin_brk, breaks = 8) # 95%confidence levels
confint(Trendmean_brk, breaks = 6) # 95%confidence levels

############################################################
#                                                          #
#     segment of breakpoint year with 95% confidence       #
#                                                          #
############################################################

strmean <- ggplot(DateTrend, aes(Date,Trendmean) ) +
  annotate("rect",xmin = as.Date("1998-11-01"),xmax = as.Date("1998-11-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("1999-02-01"),xmax = as.Date("1999-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-07-01"),xmax = as.Date("2000-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-12-01"),xmax = as.Date("2001-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2001-06-01"),xmax = as.Date("2001-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-02-01"),xmax = as.Date("2002-05-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-07-01"),xmax = as.Date("2002-08-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2003-01-01"),xmax = as.Date("2003-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2004-02-01"),xmax = as.Date("2004-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2005-03-01"),xmax = as.Date("2005-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2006-03-01"),xmax = as.Date("2006-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2007-11-01"),xmax = as.Date("2008-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
geom_point(color="blue",alpha=0.5, size = 1) +
  annotate("rect",xmin = as.Date("1999-03-21"),xmax = as.Date("1999-04-11"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") + # 95% breakpoints data
  annotate("rect",xmin = as.Date("2000-10-21"),xmax = as.Date("2001-06-11"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2003-07-21"),xmax = as.Date("2003-08-11"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2004-12-01"),xmax = as.Date("2005-01-11"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
    geom_vline(xintercept = as.Date("1999-04-01"),lty="dashed",size = 0.7) + # breakpoint year
    geom_vline(xintercept = as.Date("2001-06-01"),lty="dashed",size = 0.7) + # breakpoint year
    geom_vline(xintercept = as.Date("2003-08-01"),lty="dashed",size = 0.7) + # breakpoint year
    geom_vline(xintercept = as.Date("2004-12-11"),lty="dashed",size = 0.7) + # breakpoint year
  labs(x = NULL,y = expression('NDVI'[mean])) + #x = "Date",,title = expression('Trend of NDVI'[mean])
  #scale_x_date(labels = NULL)+
  theme_bw(15)



strmax <- ggplot(DateTrend, aes(Date,Trendmax) ) +
  annotate("rect",xmin = as.Date("1998-11-01"),xmax = as.Date("1998-11-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("1999-02-01"),xmax = as.Date("1999-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-07-01"),xmax = as.Date("2000-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-12-01"),xmax = as.Date("2001-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2001-06-01"),xmax = as.Date("2001-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-02-01"),xmax = as.Date("2002-05-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-07-01"),xmax = as.Date("2002-08-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2003-01-01"),xmax = as.Date("2003-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2004-02-01"),xmax = as.Date("2004-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2005-03-01"),xmax = as.Date("2005-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2006-03-01"),xmax = as.Date("2006-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2007-11-01"),xmax = as.Date("2008-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  geom_point(color="blue",alpha=0.5, size = 1) +
  annotate("rect",xmin = as.Date("1999-04-11"),xmax = as.Date("1999-05-01"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") + # 95% breakpoints data
  annotate("rect",xmin = as.Date("2000-03-21"),xmax = as.Date("2001-05-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2003-11-11"),xmax = as.Date("2003-12-01"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2005-06-11"),xmax = as.Date("2005-08-01"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2006-06-11"),xmax = as.Date("2006-08-01"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2007-06-11"),xmax = as.Date("2007-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  geom_vline(xintercept = as.Date("1999-04-21"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2001-04-01"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2003-11-21"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2005-06-21"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2006-07-01"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2007-07-11"),lty="dashed",size = 0.7) + # breakpoint year
  labs(x = NULL,y = expression('NDVI'[max])) + #x = "Date",,title = expression('Trend of NDVI'[mean])
  #scale_x_date(labels = NULL)+
  theme_bw(15)


strmin <- ggplot(DateTrend, aes(Date,Trendmin) ) +
  annotate("rect",xmin = as.Date("1998-11-01"),xmax = as.Date("1998-11-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("1999-02-01"),xmax = as.Date("1999-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-07-01"),xmax = as.Date("2000-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-12-01"),xmax = as.Date("2001-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2001-06-01"),xmax = as.Date("2001-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-02-01"),xmax = as.Date("2002-05-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-07-01"),xmax = as.Date("2002-08-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2003-01-01"),xmax = as.Date("2003-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2004-02-01"),xmax = as.Date("2004-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2005-03-01"),xmax = as.Date("2005-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2006-03-01"),xmax = as.Date("2006-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2007-11-01"),xmax = as.Date("2008-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  geom_point(color="blue",alpha=0.5, size = 1) +
  annotate("rect",xmin = as.Date("1999-04-11"),xmax = as.Date("1999-05-01"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") + # 95% breakpoints data
  annotate("rect",xmin = as.Date("2000-05-01"),xmax = as.Date("2000-06-01"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2001-05-11"),xmax = as.Date("2001-06-11"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2002-06-01"),xmax = as.Date("2002-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2003-06-11"),xmax = as.Date("2003-07-11"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2004-12-21"),xmax = as.Date("2005-01-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2006-01-01"),xmax = as.Date("2006-02-01"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2007-06-21"),xmax = as.Date("2007-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  geom_vline(xintercept = as.Date("1999-04-21"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2000-05-21"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2001-06-01"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2002-06-11"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2003-06-21"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2005-01-11"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2006-01-21"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2007-07-11"),lty="dashed",size = 0.7) + # breakpoint year
  labs(x = NULL,y = expression('NDVI'[min])) + #x = "Date",,title = expression('Trend of NDVI'[mean])
  #scale_x_date(labels = NULL)+
  theme_bw(15)
 

strmode <- ggplot(DateTrend, aes(Date,Trendmode) ) +
  annotate("rect",xmin = as.Date("1998-11-01"),xmax = as.Date("1998-11-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("1999-02-01"),xmax = as.Date("1999-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-07-01"),xmax = as.Date("2000-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-12-01"),xmax = as.Date("2001-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2001-06-01"),xmax = as.Date("2001-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-02-01"),xmax = as.Date("2002-05-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-07-01"),xmax = as.Date("2002-08-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2003-01-01"),xmax = as.Date("2003-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2004-02-01"),xmax = as.Date("2004-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2005-03-01"),xmax = as.Date("2005-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2006-03-01"),xmax = as.Date("2006-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2007-11-01"),xmax = as.Date("2008-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  geom_point(color="blue",alpha=0.5, size = 1) +
  annotate("rect",xmin = as.Date("1999-03-21"),xmax = as.Date("1999-04-11"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") + # 95% breakpoints data
  annotate("rect",xmin = as.Date("2001-04-11"),xmax = as.Date("2001-05-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2000-12-21"),xmax = as.Date("2002-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2003-07-11"),xmax = as.Date("2003-08-01"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2004-12-01"),xmax = as.Date("2005-01-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  annotate("rect",xmin = as.Date("2006-12-11"),xmax = as.Date("2007-02-01"),ymin = -Inf,ymax = Inf,alpha=.5,fill="green") +
  geom_vline(xintercept = as.Date("1999-04-01"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2001-05-11"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2002-03-21"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2003-07-21"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2004-12-11"),lty="dashed",size = 0.7) + # breakpoint year
  geom_vline(xintercept = as.Date("2007-01-01"),lty="dashed",size = 0.7) + # breakpoint year
  labs(x = NULL,y = expression('NDVI'[most])) + #x = "Date",,title = expression('Trend of NDVI'[mean])
  #scale_x_date(labels = NULL)+
  theme_bw(15)


figxx.str <- ggarrange(strmean,strmax,strmin,strmode,
                           labels = c("A", "B", "C","D"),
                           ncol = 1, nrow=4, align = "v")
ggsave(figxx.str,filename = "figxx.structurechange.pdf",width=12, height=8)
getwd()

#Level breaks coefficients:
#coef(Trendmean_brk, breaks = 4)
#ff <- confint(Trendmean_brk, breaks = 4)
#lines(ff)

plot(Trendmean)
lines((fitted(Trendmean_brk, breaks = 4)), col = 4)
lines(confint(Trendmean_brk, breaks = 4))

#Trendmean1 <- as.vector(Trendmean)
#plot(Trendmean1)
#polygon(x = c(ts(25:114,frequency=36,start=c(1998,10)),ts(25:114,frequency=36,start=c(1998,10)),
#              ts(25:114,frequency=36,start=c(1998,10)),ts(25:114,frequency=36,start=c(1998,10))), 
#        y = c(-Inf, Inf, Inf, -Inf), col = "red", border = NA)

#annotate("rect",xmin = 1999.5,xmax = 67,ymin = -Inf,ymax = Inf,alpha=.5,fill="gray50")


#library(zoo)



################ below for trend detection in breakpoints detection
l <- length(globtemp)
tt <- 1:l
globtemp_brk <- breakpoints(globtemp ~ tt, h = 0.1)
# # The BIC minimum value is reached for m = 4.
plot(globtemp)
lines(fitted(globtemp_brk, breaks = 4), col = 4)
lines(confint(globtemp_brk, breaks = 4))


as.data.frame(Trendmean)


#####


DateTrend <- as.data.table( cbind.data.frame (Data$Date,Data$Water_Allocation,
                   Trendmean = as.vector(Trendmean),Trendmax = as.vector(Trendmax),
                   Trendmin = as.vector(Trendmin),Trendmode = as.vector(Trendmode) )  )

names(DateTrend)[1:2] <- c("Date","Water_Allocation")


### ggscatter plot and marginal plot

pmean <-ggscatter(DateTrend, x = "Date",y = "Trendmean", add = "reg.line", 
                          conf.int = TRUE,size=1,shape=1, color="blue",alpha=0.5 ) +
  stat_cor(label.x = 12000,position = "identity",label.x.npc = "middle", label.y.npc = "top") +
  annotate("rect",xmin = as.Date("1998-11-01"),xmax = as.Date("1998-11-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("1999-02-01"),xmax = as.Date("1999-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-07-01"),xmax = as.Date("2000-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-12-01"),xmax = as.Date("2001-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2001-06-01"),xmax = as.Date("2001-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-02-01"),xmax = as.Date("2002-05-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-07-01"),xmax = as.Date("2002-08-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2003-01-01"),xmax = as.Date("2003-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2004-02-01"),xmax = as.Date("2004-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2005-03-01"),xmax = as.Date("2005-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2006-03-01"),xmax = as.Date("2006-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2007-11-01"),xmax = as.Date("2008-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  labs(x = NULL,y = expression('NDVI'[mean])) + #x = "Date",,title = expression('Trend of NDVI'[mean])
  scale_x_date(labels = NULL)+
  theme_bw(15)
pmeanM <- ggMarginal(pmean, type = 'boxplot', margins = 'y', size = 8, col = 'black',fill = 'white' )  

pmax <-ggscatter(DateTrend, x = "Date",y = "Trendmax", add = "reg.line", 
                  conf.int = TRUE,size=1,shape=1, color="blue",alpha=0.5 ) +
  stat_cor(label.x = 12000,position = "identity",label.x.npc = "middle", label.y.npc = "top") +
  annotate("rect",xmin = as.Date("1998-11-01"),xmax = as.Date("1998-11-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("1999-02-01"),xmax = as.Date("1999-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-07-01"),xmax = as.Date("2000-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-12-01"),xmax = as.Date("2001-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2001-06-01"),xmax = as.Date("2001-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-02-01"),xmax = as.Date("2002-05-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-07-01"),xmax = as.Date("2002-08-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2003-01-01"),xmax = as.Date("2003-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2004-02-01"),xmax = as.Date("2004-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2005-03-01"),xmax = as.Date("2005-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2006-03-01"),xmax = as.Date("2006-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2007-11-01"),xmax = as.Date("2008-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  labs(x = NULL,y = expression('NDVI'[max])) + #x = "Date",,title = expression('Trend of NDVI'[mean])
  scale_x_date(labels = NULL)+
  theme_bw(15)
pmaxM <- ggMarginal(pmax, type = 'boxplot', margins = 'y', size = 8, col = 'black',fill = 'white' )  

pmin <-ggscatter(DateTrend, x = "Date",y = "Trendmin", add = "reg.line", 
                  conf.int = TRUE,size=1,shape=1, color="blue",alpha=0.5 ) +
  stat_cor(label.x = 12000,position = "identity",label.x.npc = "middle", label.y.npc = "top") +
  annotate("rect",xmin = as.Date("1998-11-01"),xmax = as.Date("1998-11-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("1999-02-01"),xmax = as.Date("1999-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-07-01"),xmax = as.Date("2000-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-12-01"),xmax = as.Date("2001-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2001-06-01"),xmax = as.Date("2001-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-02-01"),xmax = as.Date("2002-05-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-07-01"),xmax = as.Date("2002-08-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2003-01-01"),xmax = as.Date("2003-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2004-02-01"),xmax = as.Date("2004-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2005-03-01"),xmax = as.Date("2005-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2006-03-01"),xmax = as.Date("2006-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2007-11-01"),xmax = as.Date("2008-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  labs(x = NULL,y = expression('NDVI'[min])) + #x = "Date",,title = expression('Trend of NDVI'[mean])
  scale_x_date(labels = NULL)+
  theme_bw(15)
pminM <- ggMarginal(pmin, type = 'boxplot', margins = 'y', size = 8, col = 'black',fill = 'white' )  

pmode <-ggscatter(DateTrend, x = "Date",y = "Trendmode", add = "reg.line", 
                 conf.int = TRUE,size=1,shape=1, color="blue",alpha=0.5 ) +
  stat_cor(label.x = 12000,position = "identity",label.x.npc = "middle", label.y.npc = "top") +
  annotate("rect",xmin = as.Date("1998-11-01"),xmax = as.Date("1998-11-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("1999-02-01"),xmax = as.Date("1999-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-07-01"),xmax = as.Date("2000-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2000-12-01"),xmax = as.Date("2001-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2001-06-01"),xmax = as.Date("2001-07-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-02-01"),xmax = as.Date("2002-05-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2002-07-01"),xmax = as.Date("2002-08-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2003-01-01"),xmax = as.Date("2003-03-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2004-02-01"),xmax = as.Date("2004-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2005-03-01"),xmax = as.Date("2005-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2006-03-01"),xmax = as.Date("2006-04-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  annotate("rect",xmin = as.Date("2007-11-01"),xmax = as.Date("2008-06-21"),ymin = -Inf,ymax = Inf,alpha=.5,fill="red") +
  labs(x = "Date",y = expression('NDVI'[mode])) + #x = "Date",,title = expression('Trend of NDVI'[mean])
  theme_bw(15)
pmodeM <- ggMarginal(pmode, type = 'boxplot', margins = 'y', size = 8, col = 'black',fill = 'white' )  

###### in one page

fig1.Trendall <- ggarrange(pmeanM,pmaxM,pminM,pmodeM,
                               labels = c("A", "B", "C","D"),
                               ncol = 1, nrow=4, align = "v")
ggsave(fig1.Trendall,filename = "fig1.Trendall.pdf",width=9, height=6)


############ abnormal detecting and plotting     ####### the result showed no abnormal detected!!!

###mean -detecting
ADmean <- cbind.data.frame(DateTrend$Date,DateTrend$Trendmean)
names(ADmean)[1:2]<-c("Date","mean")
resADmean <- AnomalyDetectionTs(ADmean, max_anoms=0.12, direction='both', plot=T)
aresADmean <- resADmean$anoms
names(aresADmean)[1:2]<-c("Date","mean")
aresADmean

#####
ADmax <- cbind.data.frame(DateTrend$Date,DateTrend$Trendmax)
names(ADmax)[1:2]<-c("Date","mean")
resADmean <- AnomalyDetectionTs(ADmax, max_anoms=0.12, direction='both', plot=T)
#aresADmean <- resADmean$anoms
#names(aresADmean)[1:2]<-c("Date","mean")
#aresADmean

#####
ADmin <- cbind.data.frame(DateTrend$Date,DateTrend$Trendmin)
names(ADmin)[1:2]<-c("Date","mean")
resADmin <- AnomalyDetectionTs(ADmin, max_anoms=0.12, direction='both', plot=T)
aresADmean <- resADmean$anoms
names(aresADmean)[1:2]<-c("Date","mean")
aresADmean

#####
ADmode <- cbind.data.frame(DateTrend$Date,DateTrend$Trendmode)
names(ADmode)[1:2]<-c("Date","mean")
resADmin <- AnomalyDetectionTs(ADmode, max_anoms=0.49, direction='both', plot=T)
aresADmean <- resADmean$anoms
names(aresADmean)[1:2]<-c("Date","mean")
aresADmean
########################
###################################### ANOVA   #############
##### melt the data

datamelt <- Data[,-(3:6)]
datamelt1 <- melt(datamelt, id = c("Date","Water_Allocation"))
datamelt1 <- datamelt1[,-(2:3)]
datamelt1 <- datamelt1[-(142477:142848),]

###Stage 1 - stage 12
stage1.1 <- datamelt1[which (datamelt1$Date >= as.Date("1998-11-01") & datamelt1$Date <= as.Date("1998-11-21")),]
  stage1.2 <- datamelt1[which (datamelt1$Date >= as.Date("1999-11-01") & datamelt1$Date <= as.Date("1999-11-21")),] 
  # Angzhstage1.3 <- Angzhdata[which (Angzhdata$Date >= as.Date("2001-07-01") & Angzhdata$Date <= as.Date("2001-07-21")),] 
  stage1.1$levels <- "Current"
  stage1.2$levels <- "After"
  stage1 <- rbind.data.frame(stage1.1,stage1.2)
  stage1$levels <- factor(stage1$levels,levels=c("Current","After"))
  names(stage1)[2] <- c("NDVI")

stage2.1 <- datamelt1[which (datamelt1$Date >= as.Date("1999-02-01") & datamelt1$Date <= as.Date("1999-03-21")),]
  stage2.2 <- datamelt1[which (datamelt1$Date >= as.Date("2000-02-01") & datamelt1$Date <= as.Date("2000-03-21")),] 
  # Angzhstage1.3 <- Angzhdata[which (Angzhdata$Date >= as.Date("2001-07-01") & Angzhdata$Date <= as.Date("2001-07-21")),] 
  stage2.1$levels <- "Current"
  stage2.2$levels <- "After"
  stage2 <- rbind.data.frame(stage2.1,stage2.2)
  stage2$levels <- factor(stage2$levels,levels=c("Current","After"))
  names(stage2)[2] <- c("NDVI")
  
  
stage3.1 <- datamelt1[which (datamelt1$Date >= as.Date("2000-07-01") & datamelt1$Date <= as.Date("2000-07-21")),]
  stage3.2 <- datamelt1[which (datamelt1$Date >= as.Date("2001-07-01") & datamelt1$Date <= as.Date("2001-07-21")),] 
  stage3.3 <- datamelt1[which (datamelt1$Date >= as.Date("1999-07-01") & datamelt1$Date <= as.Date("1999-07-21")),]
  stage3.1$levels <- "Current"
  stage3.2$levels <- "After"
  stage3.3$levels <- "Ahead"
  stage3 <- rbind.data.frame(stage3.1,stage3.2,stage3.3)
  stage3$levels <- factor(stage3$levels,levels=c("Ahead","Current","After"))
  names(stage3)[2] <- c("NDVI")
  
stage4.1 <- datamelt1[which (datamelt1$Date >= as.Date("2000-12-01") & datamelt1$Date <= as.Date("2001-03-21")),]
  stage4.2 <- datamelt1[which (datamelt1$Date >= as.Date("2001-12-01") & datamelt1$Date <= as.Date("2002-03-21")),] 
  stage4.3 <- datamelt1[which (datamelt1$Date >= as.Date("1999-12-01") & datamelt1$Date <= as.Date("2000-03-21")),]
  stage4.1$levels <- "Current"
  stage4.2$levels <- "After"
  stage4.3$levels <- "Ahead"
  stage4 <- rbind.data.frame(stage4.1,stage4.2,stage4.3)
  stage4$levels <- factor(stage4$levels,levels=c("Ahead","Current","After"))
  names(stage4)[2] <- c("NDVI")
  
stage5.1 <- datamelt1[which (datamelt1$Date >= as.Date("2001-06-01") & datamelt1$Date <= as.Date("2001-07-21")),]
  stage5.2 <- datamelt1[which (datamelt1$Date >= as.Date("2002-06-01") & datamelt1$Date <= as.Date("2002-07-21")),] 
  stage5.3 <- datamelt1[which (datamelt1$Date >= as.Date("2000-06-01") & datamelt1$Date <= as.Date("2000-07-21")),]
  stage5.1$levels <- "Current"
  stage5.2$levels <- "After"
  stage5.3$levels <- "Ahead"
  stage5 <- rbind.data.frame(stage5.1,stage5.2,stage5.3)
  stage5$levels <- factor(stage5$levels,levels=c("Ahead","Current","After"))
  names(stage5)[2] <- c("NDVI")
  
stage6.1 <- datamelt1[which (datamelt1$Date >= as.Date("2002-02-01") & datamelt1$Date <= as.Date("2002-05-21")),]
  stage6.2 <- datamelt1[which (datamelt1$Date >= as.Date("2003-02-01") & datamelt1$Date <= as.Date("2003-05-21")),] 
  stage6.3 <- datamelt1[which (datamelt1$Date >= as.Date("2001-02-01") & datamelt1$Date <= as.Date("2001-05-21")),]
  stage6.1$levels <- "Current"
  stage6.2$levels <- "After"
  stage6.3$levels <- "Ahead"
  stage6 <- rbind.data.frame(stage6.1,stage6.2,stage6.3)
  stage6$levels <- factor(stage6$levels,levels=c("Ahead","Current","After"))
  names(stage6)[2] <- c("NDVI")
  
stage7.1 <- datamelt1[which (datamelt1$Date >= as.Date("2002-07-01") & datamelt1$Date <= as.Date("2002-08-21")),]
  stage7.2 <- datamelt1[which (datamelt1$Date >= as.Date("2003-07-01") & datamelt1$Date <= as.Date("2003-08-21")),] 
  stage7.3 <- datamelt1[which (datamelt1$Date >= as.Date("2001-07-01") & datamelt1$Date <= as.Date("2001-08-21")),]
  stage7.1$levels <- "Current"
  stage7.2$levels <- "After"
  stage7.3$levels <- "Ahead"
  stage7 <- rbind.data.frame(stage7.1,stage7.2,stage7.3)
  stage7$levels <- factor(stage7$levels,levels=c("Ahead","Current","After"))  
  names(stage7)[2] <- c("NDVI")
  
stage8.1 <- datamelt1[which (datamelt1$Date >= as.Date("2003-01-01") & datamelt1$Date <= as.Date("2003-03-21")),]
  stage8.2 <- datamelt1[which (datamelt1$Date >= as.Date("2004-01-01") & datamelt1$Date <= as.Date("2004-03-21")),] 
  stage8.3 <- datamelt1[which (datamelt1$Date >= as.Date("2002-01-01") & datamelt1$Date <= as.Date("2002-03-21")),]
  stage8.1$levels <- "Current"
  stage8.2$levels <- "After"
  stage8.3$levels <- "Ahead"
  stage8 <- rbind.data.frame(stage8.1,stage8.2,stage8.3)
  stage8$levels <- factor(stage8$levels,levels=c("Ahead","Current","After"))   
  names(stage8)[2] <- c("NDVI")
  
stage9.1 <- datamelt1[which (datamelt1$Date >= as.Date("2004-02-01") & datamelt1$Date <= as.Date("2004-06-21")),]
  stage9.2 <- datamelt1[which (datamelt1$Date >= as.Date("2005-02-01") & datamelt1$Date <= as.Date("2005-06-21")),] 
  stage9.3 <- datamelt1[which (datamelt1$Date >= as.Date("2003-02-01") & datamelt1$Date <= as.Date("2003-06-21")),]
  stage9.1$levels <- "Current"
  stage9.2$levels <- "After"
  stage9.3$levels <- "Ahead"
  stage9 <- rbind.data.frame(stage9.1,stage9.2,stage9.3)
  stage9$levels <- factor(stage9$levels,levels=c("Ahead","Current","After"))
  names(stage9)[2] <- c("NDVI")
  
stage10.1 <- datamelt1[which (datamelt1$Date >= as.Date("2005-03-01") & datamelt1$Date <= as.Date("2005-04-21")),]
  stage10.2 <- datamelt1[which (datamelt1$Date >= as.Date("2006-03-01") & datamelt1$Date <= as.Date("2006-04-21")),] 
  stage10.3 <- datamelt1[which (datamelt1$Date >= as.Date("2004-03-01") & datamelt1$Date <= as.Date("2004-04-21")),]
  stage10.1$levels <- "Current"
  stage10.2$levels <- "After"
  stage10.3$levels <- "Ahead"
  stage10 <- rbind.data.frame(stage10.1,stage10.2,stage10.3)
  stage10$levels <- factor(stage10$levels,levels=c("Ahead","Current","After"))
  names(stage10)[2] <- c("NDVI")
  
stage11.1 <- datamelt1[which (datamelt1$Date >= as.Date("2006-03-01") & datamelt1$Date <= as.Date("2006-04-21")),]
  stage11.2 <- datamelt1[which (datamelt1$Date >= as.Date("2007-03-01") & datamelt1$Date <= as.Date("2007-04-21")),] 
  stage11.3 <- datamelt1[which (datamelt1$Date >= as.Date("2005-03-01") & datamelt1$Date <= as.Date("2005-04-21")),]
  stage11.1$levels <- "Current"
  stage11.2$levels <- "After"
  stage11.3$levels <- "Ahead"
  stage11 <- rbind.data.frame(stage11.1,stage11.2,stage11.3)
  stage11$levels <- factor(stage11$levels,levels=c("Ahead","Current","After")) 
  names(stage11)[2] <- c("NDVI")
  
stage12.1 <- datamelt1[which (datamelt1$Date >= as.Date("2007-11-01") & datamelt1$Date <= as.Date("2008-06-21")),]
  #stage12.2 <- datamelt1[which (datamelt1$Date >= as.Date("2007-11-01") & datamelt1$Date <= as.Date("2008-06-21")),] 
  stage12.3 <- datamelt1[which (datamelt1$Date >= as.Date("2006-11-01") & datamelt1$Date <= as.Date("2007-06-21")),]
  stage12.1$levels <- "Current"
  #stage12.2$levels <- "After"
  stage12.3$levels <- "Ahead"
  stage12 <- rbind.data.frame(stage12.1,stage12.3)
  stage12$levels <- factor(stage12$levels,levels=c("Ahead","Current"))
  names(stage12)[2] <- c("NDVI")
  
  # write.txt datamelt1
  write.table(datamelt1,file="datamelt1.txt")
  write.table(stage1,file="stage1.txt")
  write.table(stage2,file="stage2.txt")
  write.table(stage3,file="stage3.txt")
  write.table(stage4,file="stage4.txt")
  write.table(stage5,file="stage5.txt")
  write.table(stage6,file="stage6.txt")
  write.table(stage7,file="stage7.txt")
  write.table(stage8,file="stage8.txt")
  write.table(stage9,file="stage9.txt")
  write.table(stage10,file="stage10.txt")
  write.table(stage11,file="stage11.txt")
  write.table(stage12,file="stage12.txt")
  
# summary         
  getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
  }
  
  aggregate(NDVI ~ levels, stage12, getmode)
  
  stage1 <- as.data.table(stage1)
#  k <- unique(stage1[, ':='(most = getmode(NDVI),by = .(levels))][,-(1:2)])
#rm(ss)
 ss1 <- unique( stage1[, ':='(N= length(NDVI), max = max(NDVI), min = min(NDVI), most = mode(NDVI),mean = mean(NDVI), sd = sd(NDVI)),by = .(levels)][,-(1:2)])
 ss2 <- unique( stage2[, ':='(N= length(NDVI), max = max(NDVI), min = min(NDVI), mean = mean(NDVI), sd = sd(NDVI)),by = .(levels)][,-(1:2)])
 ss3 <- unique( stage3[, ':='(N= length(NDVI), max = max(NDVI), min = min(NDVI), mean = mean(NDVI), sd = sd(NDVI)),by = .(levels)][,-(1:2)])
 ss4 <- unique( stage4[, ':='(N= length(NDVI), max = max(NDVI), min = min(NDVI), mean = mean(NDVI), sd = sd(NDVI)),by = .(levels)][,-(1:2)])
 ss5 <- unique( stage5[, ':='(N= length(NDVI), max = max(NDVI), min = min(NDVI), mean = mean(NDVI), sd = sd(NDVI)),by = .(levels)][,-(1:2)])
 ss6 <- unique( stage6[, ':='(N= length(NDVI), max = max(NDVI), min = min(NDVI), mean = mean(NDVI), sd = sd(NDVI)),by = .(levels)][,-(1:2)])
 ss7 <- unique( stage7[, ':='(N= length(NDVI), max = max(NDVI), min = min(NDVI), mean = mean(NDVI), sd = sd(NDVI)),by = .(levels)][,-(1:2)])
 ss8 <- unique( stage8[, ':='(N= length(NDVI), max = max(NDVI), min = min(NDVI), mean = mean(NDVI), sd = sd(NDVI)),by = .(levels)][,-(1:2)])
 ss9 <- unique( stage9[, ':='(N= length(NDVI), max = max(NDVI), min = min(NDVI), mean = mean(NDVI), sd = sd(NDVI)),by = .(levels)][,-(1:2)])
 ss10 <- unique( stage10[, ':='(N= length(NDVI), max = max(NDVI), min = min(NDVI), mean = mean(NDVI), sd = sd(NDVI)),by = .(levels)][,-(1:2)])
 ss11 <- unique( stage11[, ':='(N= length(NDVI), max = max(NDVI), min = min(NDVI), mean = mean(NDVI), sd = sd(NDVI)),by = .(levels)][,-(1:2)])
 ss12 <- unique( stage12[, ':='(N= length(NDVI), max = max(NDVI), min = min(NDVI), mean = mean(NDVI), sd = sd(NDVI)),by = .(levels)][,-(1:2)])
 
 sss <- rbind(ss1,ss2,ss3,ss4,ss5,ss6,ss7,ss8,ss9,ss10,ss11,ss12)
 
   write.table(sss, file = "tabel2.txt")
 # s_stage1 <- list(time_duration = range(stage1$Date),stage_N=length(unique(stage1$levels)),N=length(stage1$Date)/length(unique(stage1$levels)),
   #                if(stage1$levels = "Current")
 #                       {currentNDVImax = max(stage1$NDVI), currentNDVImin=min(stage1$NDVI),currentNDVImean = mean(stage1$NDVI),currentNDVIsd=sd(stage1$NDVI) )}
   #                elseif(stage1$levels = "After")
  #                      afterNDVImax = max(stage1$NDVI), afterNDVImin=min(stage1$NDVI), afterNDVImean = mean(stage1$NDVI), afterNDVIsd=sd(stage1$NDVI))
  #                 ) 
  
##########  let's go .... offically  plottiing
my_comparisons <- list( c("Ahead","Curren"),c("Current", "After"),  c("Ahead", "After") ) 

  #stage1$levels <- paste0(stage1$levels,"\t")   # make more spacings whin legends with more \t\t\t\t\t\t
  #stage1$levels <- str_trim(stage1$levels, side = c( "right")) # delect the \t\n... side = c("both", "left", "right")
pstage1 <- ggboxplot(stage1, x = "levels", y = "NDVI",
                       color = "levels",palette = "jco",
                       add = "jitter", ylim = c(0,0.5)) +
  stat_compare_means(method = "t.test", label.y = 0.48, label.x = 0.6) +
  labs(x = NULL) +
  theme_bw(15) + ## theme(legend.title.align=1.5)
  theme(legend.position = 'none')
  #theme(legend.position = "right",legend.title = element_blank(),legend.key.width = unit(1.2,'cm')) 
  
  
  
  stage2$levels <- paste0(stage2$levels,"\t")   # make more spacings whin legends with more \t\t\t\t\t\t
  #stage1$levels <- str_trim(stage1$levels, side = c( "right")) # delect the \t\n... side = c("both", "left", "right")
pstage2 <- ggboxplot(stage2, x = "levels", y = "NDVI",
                     color = "levels",palette = "jco",
                     add = "jitter", ylim = c(-0.05,0.35)) +
  stat_compare_means(method = "t.test", label.y = 0.32, label.x = 0.6) +
  labs(x = NULL) +
  theme_bw(15) + theme(legend.position = 'none')
 # theme(legend.position = "right",legend.title = element_blank(),legend.key.width = unit(1.2,'cm'))
  
  


  #stage3$levels <- paste0(stage3$levels,"\t")   # make more spacings whin legends with more \t\t\t\t\t\t
  #stage3$levels <- factor(as.character(stage3$levels),levels=c("Ahead","Current","After"))
pstage3 <- ggboxplot(stage3, x = "levels", y = "NDVI",
                     color = "levels",palette = "jco",
                     add = "jitter", ylim = c(0.2,0.9)) +
            stat_compare_means(method = "anova",label.y = 0.9,label.x = 0.6) +
            stat_compare_means(label = "p.format", method = "t.test", ref.group = "Current",label.y = 0.8) +
        labs(x = NULL) +
        theme_bw(15) + theme(legend.position = 'none')
        #theme(legend.position = "right",legend.title = element_blank(),legend.key.width = unit(1.2,'cm'))

pstage4 <- ggboxplot(stage4, x = "levels", y = "NDVI",
                     color = "levels",palette = "jco",
                     add = "jitter", ylim = c(-0.05,0.6)) +
            stat_compare_means(method = "anova",label.y = 0.55,label.x = 0.6) +
            stat_compare_means(label = "p.format",method = "t.test", ref.group = "Current",label.y = 0.45) +
            labs(x = NULL) +
            theme_bw(15) + theme(legend.position = 'none')
          #  theme(legend.position = "right",legend.title = element_blank(),legend.key.width = unit(1.2,'cm'))

pstage5 <- ggboxplot(stage5, x = "levels", y = "NDVI",
                     color = "levels",palette = "jco",
                     add = "jitter", ylim = c(0,0.9)) +
            stat_compare_means(method = "anova",label.y = 0.9,label.x = 0.6) +
            stat_compare_means(label = "p.format",method = "t.test", ref.group = "Current",label.y = 0.85) +
            labs(x = NULL) +
            theme_bw(15) + theme(legend.position = 'none')
          #  theme(legend.position = "right",legend.title = element_blank(),legend.key.width = unit(1.2,'cm'))

pstage6 <- ggboxplot(stage6, x = "levels", y = "NDVI",
                     color = "levels",palette = "jco",
                     add = "jitter", ylim = c(-0.05,0.9)) +
          stat_compare_means(method = "anova",label.y = 0.9,label.x = 0.6) +
          stat_compare_means(label = "p.format",method = "t.test", ref.group = "Current",label.y = 0.85) +
          labs(x = NULL) +
          theme_bw(15) + theme(legend.position = 'none')
         # theme(legend.position = "right",legend.title = element_blank(),legend.key.width = unit(1.2,'cm'))

pstage7 <- ggboxplot(stage7, x = "levels", y = "NDVI",
                     color = "levels",palette = "jco",
                     add = "jitter", ylim = c(0.25,0.9)) +
            stat_compare_means(method = "anova",label.y = 0.9,label.x = 0.6) +
            stat_compare_means(label = "p.format",method = "t.test", ref.group = "Current",label.y = 0.85) +
            labs(x = NULL) +
            theme_bw(15) + theme(legend.position = 'none')
          #  theme(legend.position = "right",legend.title = element_blank(),legend.key.width = unit(1.2,'cm'))


pstage8 <- ggboxplot(stage8, x = "levels", y = "NDVI",
                     color = "levels",palette = "jco",
                     add = "jitter", ylim = c(-0.05,0.45)) +
            stat_compare_means(method = "anova",label.y = 0.45,label.x = 0.6) +
            stat_compare_means(label = "p.format",method = "t.test", ref.group = "Current",label.y = 0.4) +
            labs(x = NULL) +
            theme_bw(15) + theme(legend.position = 'none')
          #  theme(legend.position = "right",legend.title = element_blank(),legend.key.width = unit(1.2,'cm'))

pstage9 <- ggboxplot(stage9, x = "levels", y = "NDVI",
                     color = "levels",palette = "jco",
                     add = "jitter", ylim = c(-0.05,0.9)) +
            stat_compare_means(method = "anova",label.y = 0.9,label.x = 0.6) +
            stat_compare_means(label = "p.format",method = "t.test", ref.group = "Current",label.y = 0.85) +
            labs(x = NULL) +
            theme_bw(15) + theme(legend.position = 'none')
          #  theme(legend.position = "right",legend.title = element_blank(),legend.key.width = unit(1.2,'cm'))

pstage10 <- ggboxplot(stage10, x = "levels", y = "NDVI",
                     color = "levels",palette = "jco",
                     add = "jitter", ylim = c(0,0.8)) +
              stat_compare_means(method = "anova",label.y = 0.8,label.x = 0.6) +
              stat_compare_means(label = "p.format",method = "t.test", ref.group = "Current",label.y = 0.75) +
              labs(x = NULL) +
              theme_bw(15) + theme(legend.position = 'none')
           #   theme(legend.position = "right",legend.title = element_blank(),legend.key.width = unit(1.2,'cm'))

pstage11 <- ggboxplot(stage11, x = "levels", y = "NDVI",
                     color = "levels",palette = "jco",
                     add = "jitter", ylim = c(-0.05,0.75)) +
              stat_compare_means(method = "anova",label.y = 0.75,label.x = 0.6) +
              stat_compare_means(label = "p.format",method = "t.test", ref.group = "Current",label.y = 0.7) +
              labs(x = NULL) +
              theme_bw(15) + theme(legend.position = 'none')
           #   theme(legend.position = "right",legend.title = element_blank(),legend.key.width = unit(1.2,'cm'))

pstage12 <- ggboxplot(stage12, x = "levels", y = "NDVI",
                     color = "levels",palette = "jco",
                     add = "jitter", ylim = c(-0.1,0.8)) +
            stat_compare_means(method = "t.test", label.y = 0.8, label.x = 0.6) +
            labs(x = NULL) +
            theme_bw(15) + theme(legend.position = 'none')
          #  theme(legend.position = "right",legend.title = element_blank(),legend.key.width = unit(1.2,'cm'))

########### Angzh Figure
pp <- ggarrange(pstage1,pstage2,pstage3,pstage4,
                pstage5,pstage6,pstage7,pstage8,
                pstage9,pstage10,pstage11,pstage12,
                labels = LETTERS[1:12],
                ncol = 3, nrow=4, align = "v")

ggsave(pp,filename = "fig.anova-final3.pdf",width=16, height=11)







  
  
