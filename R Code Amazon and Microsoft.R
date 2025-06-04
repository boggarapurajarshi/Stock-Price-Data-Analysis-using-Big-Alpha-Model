############### LIBRARIES ###############

library(PerformanceAnalytics)
library(quantmod)
library(TTR)
library(RedditExtractoR)
library(sentimentr)
library(ggplot2)
library(vctrs)
library(stringi)
library(sentimentr)

############### ALPHA MODEL INTRO ###############

sym.vec <-c("AMZN", "MSFT", "^GSPC")
proj1data <-getSymbols(sym.vec, from = "2021-01-01", to = "2023-01-01")
str(proj1data)

AMZN <- AMZN[, "AMZN.Adjusted", drop=F]
AMZN.logret <-diff(log(AMZN))
AMZN.logret[1]; AMZN.logret[1] = 0.0
MSFT <- MSFT[, "MSFT.Adjusted", drop=F]
MSFT.logret <-diff(log(MSFT))
MSFT.logret[1]; MSFT.logret[1] = 0.0
GSPC <- GSPC[, "GSPC.Adjusted", drop=F]
GSPC.logret = diff(log(GSPC))
GSPC.logret[1]; GSPC.logret[1] = 0.0

AMZN_daily <-to.daily(AMZN)
MSFT_daily <-to.daily(MSFT)
GSPC_daily <-to.daily(GSPC)
AMZN_daily_logret <-diff(log(AMZN_daily))
MSFT_daily_logret <-diff(log(MSFT_daily))
GSPC_daily_logret <-diff(log(GSPC_daily))

MSFT_logret_close <-MSFT_daily_logret$MSFT.Close
AMZN_logret_close <-AMZN_daily_logret$AMZN.Close
market_logret_close <-GSPC_daily_logret$GSPC.Close

summary(lm(MSFT_logret_close~market_logret_close))
summary(lm(AMZN_logret_close~market_logret_close))

############### PERFORMANCE ANALYTICS ###############

data.AMZN <- getSymbols("AMZN",from="2021-01-01",to="2023-01-01",auto.assign=FALSE)
AMZN.ret <- Delt(data.AMZN$AMZN.Adjusted)
names(AMZN.ret) <- paste("AMZN.ret")
data.MSFT <- getSymbols("MSFT",from="2021-01-01",to="2023-01-01",auto.assign=FALSE)
MSFT.ret <- Delt(data.MSFT$MSFT.Adjusted)
names(MSFT.ret) <- paste("MSFT.ret")
returns<-cbind(AMZN.ret,MSFT.ret)
returns<-returns[-1,]
returns

sd.AMZN <-sd(returns$AMZN.ret)*sqrt(252)
sd.MSFT <-sd(returns$MSFT.ret)*sqrt(252)
ret.cov <-cov(returns$AMZN.ret,returns$MSFT.ret)*252

wgt.AMZN=0.3; wgt.MSFT=0.7 #asset allocation
port.var <-wgt.AMZN^2*sd.AMZN^2+wgt.MSFT^2*sd.MSFT^2+2*ret.cov*wgt.AMZN*wgt.MSFT
port.sd<-sqrt(port.var) #portfolio risk is 0.2984343
port.sd

AMZN.mean <-mean(returns$AMZN.ret)
AMZN.risk <-sd(returns$AMZN.ret)
VaR_AMZN.Gaussian <--(AMZN.mean+AMZN.risk*qnorm(0.05))*25000 #Value at Risk is $1042.011 at 5% confidence
ES_AMZN.Gaussian <-25000*(AMZN.mean+AMZN.risk*dnorm(qnorm(0.05))/0.05) #Expected Shortfall is $1252.16 at 5% confidence

MSFT.mean <-mean(returns$MSFT.ret)
MSFT.risk <-sd(returns$MSFT.ret)
VaR_MSFT.Gaussian <--(MSFT.mean+MSFT.risk*qnorm(0.05))*25000 #Value at Risk is $743.9655 at 5% confidence
ES_MSFT.Gaussian <-25000*(MSFT.mean+MSFT.risk*dnorm(qnorm(0.05))/0.05) #Expected Shortfall is $955.2678 at 5% confidence

Return.annualized(returns$AMZN.ret) #Annualized Returns Amazon: 27.4831%
maxDrawdown(returns$AMZN.ret) #Maximum Drawdown Amazon: 0.5614526
Return.annualized(returns$MSFT.ret) #Annualized Returns Microsoft: 37.14849%
maxDrawdown(returns$MSFT.ret) #Maximum Drawdown Microsoft: 0.3714849

SharpeRatio.annualized(returns$AMZN.ret) #Sharpe Ratio Amazon: -0.6994871
SharpeRatio.annualized(returns$MSFT.ret) #Sharpe Ratio Microsoft: 0.2032739

VaR(returns$AMZN.ret, 0.05,method="gaussian")
KellyRatio(returns$AMZN.ret) #Kelly Ratio Amazon: -0.7902843
VaR(returns$MSFT.ret, 0.05,method="gaussian")
KellyRatio(returns$MSFT.ret) #Kelly Ratio Microsoft: 0.588874

InformationRatio(returns$AMZN.ret,returns$MSFT.ret,scale=252)
chart.VaRSensitivity(returns$AMZN.ret)
chart.VaRSensitivity(returns$MSFT.ret)
charts.PerformanceSummary(returns$AMZN.ret)
charts.PerformanceSummary(returns$MSFT.ret)

############### TECHNICAL ANALYSIS ###############

getSymbols("AMZN")
chartSeries(AMZN,subset='2021-01::2022-12',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

candleChart(AMZN,multi.col=TRUE,theme="black",subset='2021-01::2022-12')
lineChart(AMZN,line.type='h',TA=NULL,subset='2021-01::2022-12')

getSymbols("MSFT")
chartSeries(MSFT,subset='2021-01::2022-12',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

candleChart(MSFT,multi.col=TRUE,theme="black",subset='2021-01::2022-12')
lineChart(MSFT,line.type='h',TA=NULL,subset='2021-01::2022-12')

###AMZN###
data_amzn = read.csv(file="AMZN.csv")
sma20_amzn <- SMA(data_amzn[c('Close')],n=20)
head(sma20_amzn, n=50)
ema14_amzn = EMA(data_amzn[c('Close')],n=14)
head(ema14_amzn,n=20)
bb20_amzn = BBands(data_amzn[c('Close')], sd=2.0)
head(bb20_amzn,n=30)

rsi14_amzn = RSI(data_amzn[c('Close')], n=14)
head(rsi14_amzn,n=30)
macd_amzn = MACD(data_amzn[c('Close')], nFast=12, nSlow=26, nSig=9, maType=SMA)
head(macd_amzn,n=30)
allData_amzn = data.frame(data_amzn,sma20_amzn,ema14_amzn,bb20_amzn,rsi14_amzn,macd_amzn) 
head(allData_amzn,n=20)

###MSFT###
data_msft = read.csv(file="MSFT.csv")
head(data_msft); nrow(data_msft); colnames(data_msft)
sma20_msft <- SMA(data_msft[c('Close')],n=20)
head(sma20_msft, n=50)
ema14_msft = EMA(data_msft[c('Close')],n=14)
head(ema14_msft,n=20)
bb20_msft = BBands(data_msft[c('Close')], sd=2.0)
head(bb20_msft,n=30)

rsi14_msft = RSI(data_msft[c('Close')], n=14)
head(rsi14_msft,n=30)
macd_msft = MACD(data_msft[c('Close')], nFast=12, nSlow=26, nSig=9, maType=SMA)
head(macd_msft,n=30)
allData_msft = data.frame(data_msft,sma20_msft,ema14_msft,bb20_msft,rsi14_msft,macd_msft) 
head(allData_msft,n=20)

############### SENTIMENTAL ANALYSIS ###############

### AMAZON ###
AMZN_URLs<-find_thread_urls(keywords="AMZN",subreddit="Business",sort_by="new",period = 'all') # you can specify the time periods as well
AMZN.Reddit<-get_thread_content(AMZN_URLs$url) #list format, 1 is threads and 2 is comment
AMZN.Reddit
AMZN.RedditThread<- AMZN.Reddit[1] #[1] is threads
AMZN.RedditThread
AMZN.RedditText<- AMZN.RedditThread$threads$text # this is strange syntax designed by the package (but works): you need to specify $thread$text o get reddit texts
AMZN.RedditText
AMZN.RedditComment <-AMZN.Reddit[2]
AMZN.RedditComment
comments <-AMZN.RedditComment$comments$comment # another strange syntax, but it works: you need to specify $comment$comment to get comments
comments

AMZN.RedditText <- stri_encode(AMZN.RedditText, "", "UTF-8") # re-mark encodings
AMZN_post_sentiment <- sentiment(AMZN.RedditText)
AMZN_post_sentiment <- sentiment(AMZN.RedditText) 
AMZN_post_sentiment
Positivesentiment <- subset(AMZN_post_sentiment, AMZN_post_sentiment$sentiment>0)
Negativesentiment <- subset(AMZN_post_sentiment, AMZN_post_sentiment$sentiment<0)
Positivesentiment
Negativesentiment
AvgNegativesentiment<- sum(Negativesentiment$sentiment)/length(Negativesentiment$sentiment)
AvgNegativesentiment
AvgPositivesentiment <- sum(Positivesentiment$sentiment)/length(Positivesentiment$sentiment)
AvgPositivesentiment
Avgsentiment <- sum(AMZN_post_sentiment$sentiment)/length(AMZN_post_sentiment$sentiment)
Avgsentiment

data <- data.frame(
  Amazon_Text=c("Average Total Sentiment","Average Positive Sentiment","Average Negative Sentiment") ,  
  value=c(Avgsentiment,AvgPositivesentiment,AvgNegativesentiment),
  colour = c('grey','green','red')
)

data 
ggplot(data, aes(x=Amazon_Text,y=value, fill=colour )) +  
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c('green','grey','red') ) +
  theme(legend.position="none") +
  geom_point() +
  geom_text(
    label=data$value,
    check_overlap=T)

AMZN_comment_sentiment <- sentiment(comments)
AMZN_comment_sentiment                               
Positivesentiment <- subset(AMZN_comment_sentiment, AMZN_comment_sentiment$sentiment>0)
Negativesentiment <- subset(AMZN_comment_sentiment, AMZN_comment_sentiment$sentiment<0)
Positivesentiment
Negativesentiment
AvgNegativesentiment<- sum(Negativesentiment$sentiment)/length(Negativesentiment$sentiment)
AvgNegativesentiment
AvgPositivesentiment <- sum(Positivesentiment$sentiment)/length(Positivesentiment$sentiment)
AvgPositivesentiment
Avgsentiment <- sum(AMZN_comment_sentiment$sentiment)/length(AMZN_comment_sentiment$sentiment)
Avgsentiment
data <- data.frame(
  Amazon_Comments=c("Average Total Sentiment","Average Positive Sentiment","Average Negative Sentiment") ,  
  value=c(Avgsentiment,AvgPositivesentiment,AvgNegativesentiment),
  colour = c('grey','green','red')
)

data 
ggplot(data, aes(x=Amazon_Comments,y=value, fill=colour )) +  
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c('green','grey','red') ) +
  theme(legend.position="none") +
  geom_point() +
  geom_text(
    label=data$value,
    check_overlap=T)

### MICROSOFT ###
MSFT_URLs<-find_thread_urls(keywords="microsoft",subreddit="Business",sort_by="new",period = 'all') # you can specify the time periods as well
MSFT.Reddit<-get_thread_content(MSFT_URLs$url) #list format, 1 is threads and 2 is comment
MSFT.Reddit
MSFT.RedditThread<- MSFT.Reddit[1] #[1] is threads
MSFT.RedditThread
MSFT.RedditText<- MSFT.RedditThread$threads$text # this is strange syntax designed by the package (but works): you need to specify $thread$text o get reddit texts
MSFT.RedditText
MSFT.RedditComment <-MSFT.Reddit[2]
MSFT.RedditComment
comments <-MSFT.RedditComment$comments$comment # another strange syntax, but it works: you need to specify $comment$comment to get comments
comments
MSFT.RedditText <- stri_encode(MSFT.RedditText, "", "UTF-8") # re-mark encodings
MSFT_post_sentiment <- sentiment(MSFT.RedditText)
MSFT_post_sentiment
Positivesentiment <- subset(MSFT_post_sentiment, MSFT_post_sentiment$sentiment>0)
Negativesentiment <- subset(MSFT_post_sentiment, MSFT_post_sentiment$sentiment<0)
Positivesentiment
Negativesentiment
AvgNegativesentiment<- sum(Negativesentiment$sentiment)/length(Negativesentiment$sentiment)
AvgNegativesentiment
AvgPositivesentiment <- sum(Positivesentiment$sentiment)/length(Positivesentiment$sentiment)
AvgPositivesentiment
Avgsentiment <- sum(MSFT_post_sentiment$sentiment)/length(MSFT_post_sentiment$sentiment)
Avgsentiment

data <- data.frame(
  Microsoft_Text=c("Average Total Sentiment","Average Positive Sentiment","Average Negative Sentiment") ,  
  value=c(Avgsentiment,AvgPositivesentiment,AvgNegativesentiment),
  colour = c('grey','green','red')
)

data 
ggplot(data, aes(x=Microsoft_Text,y=value, fill=colour )) +  
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c('green','grey','red') ) +
  theme(legend.position="none") +
  geom_point() +
  geom_text(
    label=data$value,
    check_overlap=T)

MSFT_comment_sentiment <- sentiment(comments)
MSFT_comment_sentiment                               
Positivesentiment <- subset(MSFT_comment_sentiment, MSFT_comment_sentiment$sentiment>0)
Negativesentiment <- subset(MSFT_comment_sentiment, MSFT_comment_sentiment$sentiment<0)
Positivesentiment
Negativesentiment
AvgNegativesentiment<- sum(Negativesentiment$sentiment)/length(Negativesentiment$sentiment)
AvgNegativesentiment
AvgPositivesentiment <- sum(Positivesentiment$sentiment)/length(Positivesentiment$sentiment)
AvgPositivesentiment
Avgsentiment <- sum(MSFT_comment_sentiment$sentiment)/length(MSFT_comment_sentiment$sentiment)
Avgsentiment
data <- data.frame(
  Microsoft_Comments=c("Average Total Sentiment","Average Positive Sentiment","Average Negative Sentiment") ,  
  value=c(Avgsentiment,AvgPositivesentiment,AvgNegativesentiment),
  colour = c('grey','green','red')
)

data 
ggplot(data, aes(x=Microsoft_Comments,y=value, fill=colour )) +  
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c('green','grey','red') ) +
  theme(legend.position="none") +
  geom_point() +
  geom_text(
    label=data$value,
    check_overlap=T)

############### ALPHA MODEL ###############

getSymbols("UNRATE",src="FRED") #Extracting data from FRED for unemployment rate
unemployementrate <- UNRATE["2021-01-01/2022-12-01"];
dim(unemployementrate); head(unemployementrate); tail(unemployementrate)
chartSeries(unemployementrate,theme='white')

getSymbols("CPIAUCSL",src="FRED") #Extracting data from FRED for CPI of Urban Consumers
cpi_urban_consumers <- CPIAUCSL["2021-01-01/2022-12-01"];
dim(cpi_urban_consumers); head(cpi_urban_consumers); tail(cpi_urban_consumers)
chartSeries(cpi_urban_consumers,theme='white')

getSymbols('CPIAUCNS',src='FRED') #Extracting data from FRED for CPI Inflation
cpi_inflation <- CPIAUCNS["2021-01-01/2022-12-01"];
dim(cpi_inflation); head(cpi_inflation); tail(cpi_inflation)
chartSeries(cpi_inflation,theme='white')

sym.vec <-c("AMZN", "MSFT", "^GSPC")
proj1data <-getSymbols(sym.vec, from = "2021-01-01", to = "2023-01-01")
str(proj1data)

AMZN <- AMZN[, "AMZN.Adjusted", drop=F]
AMZN.logret <-diff(log(AMZN))
AMZN.logret[1]; AMZN.logret[1] = 0.0
head(AMZN.logret)
MSFT <- MSFT[, "MSFT.Adjusted", drop=F]
MSFT.logret <-diff(log(MSFT))
MSFT.logret[1]; MSFT.logret[1] = 0.0
head(MSFT.logret)
GSPC <- GSPC[, "GSPC.Adjusted", drop=F]
GSPC.logret = diff(log(GSPC))
GSPC.logret[1]; GSPC.logret[1] = 0.0
head(GSPC.logret)

##converting the data to monthly###
AMZN_monthly <-to.monthly(AMZN)
MSFT_monthly <-to.monthly(MSFT)
GSPC_monthly <-to.monthly(GSPC)
AMZN_monthly_logret <-diff(log(AMZN_monthly))
MSFT_monthly_logret <-diff(log(MSFT_monthly))
GSPC_monthly_logret <-diff(log(GSPC_monthly))

MSFT_logret_close <-MSFT_monthly_logret$MSFT.Close
AMZN_logret_close <-AMZN_monthly_logret$AMZN.Close
market_logret_close <-GSPC_monthly_logret$GSPC.Close

summary(lm(MSFT_logret_close~market_logret_close))
summary(lm(AMZN_logret_close~market_logret_close))

model1 <-lm(MSFT_logret_close ~ market_logret_close + unemployementrate + cpi_inflation + cpi_urban_consumers)
model2 <-lm(AMZN_logret_close ~ market_logret_close + unemployementrate + cpi_inflation + cpi_urban_consumers)

summary(model1)
summary(model2)

############### SUBMITTED BY ###############
#Rajarshi Boggarapu RXB220048
#Harshith Nilagiri HXN210021
#Tarun Sai Reddy Vippala TRV220000
#Leela Ashish Parvataneni LXP220012