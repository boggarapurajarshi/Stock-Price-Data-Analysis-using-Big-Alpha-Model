############### LIBRARIES ###############

library(PerformanceAnalytics)
library(quantmod)
library(TTR)

############### ALPHA MODEL INTRO ###############

sym.vec <-c("AAPL", "GOOG", "^GSPC")
project1 <-getSymbols(sym.vec, from = "2021-01-01", to = "2023-01-01")
str(project1)

AAPL <- AAPL[, "AAPL.Adjusted", drop=F]
AAPL.logret <-diff(log(AAPL))
AAPL.logret[1]; AAPL.logret[1] = 0.0
GOOG <- GOOG[, "GOOG.Adjusted", drop=F]
GOOG.logret <-diff(log(GOOG))
GOOG.logret[1]; GOOG.logret[1] = 0.0
GSPC <- GSPC[, "GSPC.Adjusted", drop=F]
GSPC.logret = diff(log(GSPC))
GSPC.logret[1]; GSPC.logret[1] = 0.0

AAPL_daily <-to.daily(AAPL)
GOOG_daily <-to.daily(GOOG)
GSPC_daily <-to.daily(GSPC)
AAPL_daily_logret <-diff(log(AAPL_daily))
GOOG_daily_logret <-diff(log(GOOG_daily))
GSPC_daily_logret <-diff(log(GSPC_daily))

GOOG_logret_close <-GOOG_daily_logret$GOOG.Close
AAPL_logret_close <-AAPL_daily_logret$AAPL.Close
market_logret_close <-GSPC_daily_logret$GSPC.Close

summary(lm(GOOG_logret_close~market_logret_close))
summary(lm(AAPL_logret_close~market_logret_close))

############### PERFORMANCE ANALYTICS ###############

data.AAPL <- getSymbols("AAPL",from="2021-01-01",to="2023-01-01",auto.assign=FALSE)
AAPL.ret <- Delt(data.AAPL$AAPL.Adjusted)
names(AAPL.ret) <- paste("AAPL.ret")
data.GOOG <- getSymbols("GOOG",from="2021-01-01",to="2023-01-01",auto.assign=FALSE)
GOOG.ret <- Delt(data.GOOG$GOOG.Adjusted)
names(GOOG.ret) <- paste("GOOG.ret")
returns<-cbind(AAPL.ret,GOOG.ret)
returns<-returns[-1,]
returns

sd.AAPL <-sd(returns$AAPL.ret)*sqrt(252)
sd.GOOG <-sd(returns$GOOG.ret)*sqrt(252)
ret.cov <-cov(returns$AAPL.ret,returns$GOOG.ret)*252

wgt.AAPL=0.6; wgt.GOOG=0.4 #asset allocation
port.var <-wgt.AAPL^2*sd.AAPL^2+wgt.GOOG^2*sd.GOOG^2+2*ret.cov*wgt.AAPL*wgt.GOOG
port.sd<-sqrt(port.var) #portfolio risk is 0.2916512
port.sd

AAPL.mean <-mean(returns$AAPL.ret)
AAPL.risk <-sd(returns$AAPL.ret)
VaR_AAPL.Gaussian <--(AAPL.mean+AAPL.risk*qnorm(0.01))*15000 #Value at Risk is $674.8289 at 1% confidence
ES_AAPL.Gaussian <-15000*(AAPL.mean+AAPL.risk*dnorm(qnorm(0.01))/0.01) #Expected Shortfall is $780.2052 at 1% confidence

GOOG.mean <-mean(returns$GOOG.ret)
GOOG.risk <-sd(returns$GOOG.ret)
VaR_GOOG.Gaussian <--(GOOG.mean+GOOG.risk*qnorm(0.01))*15000 #Value at Risk is $704.9141 at 1% confidence
ES_GOOG.Gaussian <-15000*(GOOG.mean+GOOG.risk*dnorm(qnorm(0.01))/0.01) #Expected Shortfall is $815.9144 at 1% confidence

Return.annualized(returns$AAPL.ret) #Annualized Returns Apple: 0.8035289%
maxDrawdown(returns$AAPL.ret) #Maximum Drawdown Apple: 0.3034915
Return.annualized(returns$GOOG.ret) #Annualized Returns Google: 1.337716%
maxDrawdown(returns$GOOG.ret) #Maximum Drawdown Google: 0.4460185

SharpeRatio.annualized(returns$AAPL.ret) #Sharpe Ratio Apple: 0.02604688
SharpeRatio.annualized(returns$GOOG.ret) #Sharpe Ratio Google: 0.04148693

VaR(returns$AAPL.ret, 0.01,method="gaussian")
KellyRatio(returns$AAPL.ret) #Kelly Ratio Apple: 0.29115
VaR(returns$GOOG.ret, 0.01,method="gaussian")
KellyRatio(returns$GOOG.ret) #Kelly Ratio Google: 0.313259

InformationRatio(returns$AAPL.ret,returns$GOOG.ret,scale=252)
chart.VaRSensitivity(returns$AAPL.ret)
chart.VaRSensitivity(returns$GOOG.ret)
charts.PerformanceSummary(returns$AAPL.ret)
charts.PerformanceSummary(returns$GOOG.ret)

############### TECHNICAL ANALYSIS ###############

getSymbols("AAPL")
chartSeries(AAPL,subset='2021-01::2022-12',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

candleChart(AAPL,multi.col=TRUE,theme="black",subset='2021-01::2022-12')
lineChart(AAPL,line.type='h',TA=NULL,subset='2021-01::2022-12')

getSymbols("GOOG")
chartSeries(GOOG,subset='2021-01::2022-12',
            type='candlesticks',theme=chartTheme('white',up.col='green',dn.col='red'),
            TA=c(addBBands(n=20,sd=2,),addSMA(n=50,col="blue"),addSMA(n=10,col="black"), 
                 addRSI(n=14),addVo(),addMACD(),addWPR())) 

candleChart(GOOG,multi.col=TRUE,theme="black",subset='2021-01::2022-12')
lineChart(GOOG,line.type='h',TA=NULL,subset='2021-01::2022-12')

###AAPL###
data_AAPL = read.csv(file="AAPL.csv")
sma20_AAPL <- SMA(data_AAPL[c('Close')],n=20)
head(sma20_AAPL, n=50)
ema14_AAPL = EMA(data_AAPL[c('Close')],n=14)
head(ema14_AAPL,n=20)
bb20_AAPL = BBands(data_AAPL[c('Close')], sd=2.0)
head(bb20_AAPL,n=30)

rsi14_AAPL = RSI(data_AAPL[c('Close')], n=14)
head(rsi14_AAPL,n=30)
macd_AAPL = MACD(data_AAPL[c('Close')], nFast=12, nSlow=26, nSig=9, maType=SMA)
head(macd_AAPL,n=30)
allData_AAPL = data.frame(data_AAPL,sma20_AAPL,ema14_AAPL,bb20_AAPL,rsi14_AAPL,macd_AAPL) 
head(allData_AAPL,n=20)

###GOOG###
data_GOOG = read.csv(file="GOOG.csv")
head(data_GOOG); nrow(data_GOOG); colnames(data_GOOG)
sma20_GOOG <- SMA(data_GOOG[c('Close')],n=20)
head(sma20_GOOG, n=50)
ema14_GOOG = EMA(data_GOOG[c('Close')],n=14)
head(ema14_GOOG,n=20)
bb20_GOOG = BBands(data_GOOG[c('Close')], sd=2.0)
head(bb20_GOOG,n=30)

rsi14_GOOG = RSI(data_GOOG[c('Close')], n=14)
head(rsi14_GOOG,n=30)
macd_GOOG = MACD(data_GOOG[c('Close')], nFast=12, nSlow=26, nSig=9, maType=SMA)
head(macd_GOOG,n=30)
allData_GOOG = data.frame(data_GOOG,sma20_GOOG,ema14_GOOG,bb20_GOOG,rsi14_GOOG,macd_GOOG) 
head(allData_GOOG,n=20)

############### SENTIMENTAL ANALYSIS ###############

### GOOGLE ###
GGL_URLs<-find_thread_urls(keywords="google",subreddit="Business",sort_by="new",period = 'all') # you can specify the time periods as well
GGL.Reddit<-get_thread_content(GGL_URLs$url) #list format, 1 is threads and 2 is comment
GGL.Reddit
GGL.RedditThread<- GGL.Reddit[1] #[1] is threads
GGL.RedditThread
GGL.RedditText<- GGL.RedditThread$threads$text # this is strange syntax designed by the package (but works): you need to specify $thread$text o get reddit texts
GGL.RedditText
GGL.RedditComment <-GGL.Reddit[2]
GGL.RedditComment
comments <-GGL.RedditComment$comments$comment # another strange syntax, but it works: you need to specify $comment$comment to get comments
comments
GGL.RedditText <- stri_encode(GGL.RedditText, "", "UTF-8") # re-mark encodings
GGL_post_sentiment <- sentiment(GGL.RedditText) 
GGL_post_sentiment
Positivesentiment <- subset(GGL_post_sentiment, GGL_post_sentiment$sentiment>0)
Negativesentiment <- subset(GGL_post_sentiment, GGL_post_sentiment$sentiment<0)
Positivesentiment
Negativesentiment
AvgNegativesentiment<- sum(Negativesentiment$sentiment)/length(Negativesentiment$sentiment)
AvgNegativesentiment
AvgPositivesentiment <- sum(Positivesentiment$sentiment)/length(Positivesentiment$sentiment)
AvgPositivesentiment
Avgsentiment <- sum(GGL_post_sentiment$sentiment)/length(GGL_post_sentiment$sentiment)
Avgsentiment
data <- data.frame(
  Google_Text=c("Average Total Sentiment","Average Positive Sentiment","Average Negative Sentiment") ,  
  value=c(Avgsentiment,AvgPositivesentiment,AvgNegativesentiment),
  colour = c('grey','green','red')
)

data 
ggplot(data, aes(x=Google_Text,y=value, fill=colour )) +  
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c('green','grey','red') ) +
  theme(legend.position="none") +
  geom_point() +
  geom_text(
    label=data$value,
    check_overlap=T)


GGL_comment_sentiment <- sentiment(comments)
GGL_comment_sentiment                               
Positivesentiment <- subset(GGL_comment_sentiment, GGL_comment_sentiment$sentiment>0)
Negativesentiment <- subset(GGL_comment_sentiment, GGL_comment_sentiment$sentiment<0)
Positivesentiment
Negativesentiment
AvgNegativesentiment<- sum(Negativesentiment$sentiment)/length(Negativesentiment$sentiment)
AvgNegativesentiment
AvgPositivesentiment <- sum(Positivesentiment$sentiment)/length(Positivesentiment$sentiment)
AvgPositivesentiment
Avgsentiment <- sum(GGL_comment_sentiment$sentiment)/length(GGL_comment_sentiment$sentiment)
Avgsentiment
data <- data.frame(
  Google_Comments=c("Average Total Sentiment","Average Positive Sentiment","Average Negative Sentiment") ,  
  value=c(Avgsentiment,AvgPositivesentiment,AvgNegativesentiment),
  colour = c('grey','green','red')
)

data 
ggplot(data, aes(x=Google_Comments,y=value, fill=colour )) +  
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c('green','grey','red') ) +
  theme(legend.position="none") +
  geom_point() +
  geom_text(
    label=data$value,
    check_overlap=T)

### APPLE ###
AAPL_URLs<-find_thread_urls(keywords="AAPL",subreddit="Business",sort_by="new",period = 'all') # you can specify the time periods as well
AAPL.Reddit<-get_thread_content(AAPL_URLs$url) #list format, 1 is threads and 2 is comment
AAPL.Reddit
AAPL.RedditThread<- AAPL.Reddit[1] #[1] is threads
AAPL.RedditThread
AAPL.RedditText<- AAPL.RedditThread$threads$text # this is strange syntax designed by the package (but works): you need to specify $thread$text o get reddit texts
AAPL.RedditText
AAPL.RedditComment <-AAPL.Reddit[2]
AAPL.RedditComment
comments <-AAPL.RedditComment$comments$comment # another strange syntax, but it works: you need to specify $comment$comment to get comments
comments
AAPL.RedditText <- stri_encode(AAPL.RedditText, "", "UTF-8") # re-mark encodings
AAPL_post_sentiment <- sentiment(AAPL.RedditText)
AAPL_post_sentiment <- sentiment(AAPL.RedditText) 
AAPL_post_sentiment
Positivesentiment <- subset(AAPL_post_sentiment, AAPL_post_sentiment$sentiment>0)
Negativesentiment <- subset(AAPL_post_sentiment, AAPL_post_sentiment$sentiment<0)
Positivesentiment
Negativesentiment
AvgNegativesentiment<- sum(Negativesentiment$sentiment)/length(Negativesentiment$sentiment)
AvgNegativesentiment
AvgPositivesentiment <- sum(Positivesentiment$sentiment)/length(Positivesentiment$sentiment)
AvgPositivesentiment
Avgsentiment <- sum(AAPL_post_sentiment$sentiment)/length(AAPL_post_sentiment$sentiment)
Avgsentiment
data <- data.frame(
  Apple_Text=c("Average Total Sentiment","Average Positive Sentiment","Average Negative Sentiment") ,  
  value=c(Avgsentiment,AvgPositivesentiment,AvgNegativesentiment),
  colour = c('grey','green','red')
)

data 
ggplot(data, aes(x=Apple_Text,y=value, fill=colour )) +  
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c('green','grey','red') ) +
  theme(legend.position="none") +
  geom_point() +
  geom_text(
    label=data$value,
    check_overlap=T)

AAPL_comment_sentiment <- sentiment(comments)
AAPL_comment_sentiment                               
Positivesentiment <- subset(AAPL_comment_sentiment, AAPL_comment_sentiment$sentiment>0)
Negativesentiment <- subset(AAPL_comment_sentiment, AAPL_comment_sentiment$sentiment<0)
Positivesentiment
Negativesentiment
AvgNegativesentiment<- sum(Negativesentiment$sentiment)/length(Negativesentiment$sentiment)
AvgNegativesentiment
AvgPositivesentiment <- sum(Positivesentiment$sentiment)/length(Positivesentiment$sentiment)
AvgPositivesentiment
Avgsentiment <- sum(AAPL_comment_sentiment$sentiment)/length(AAPL_comment_sentiment$sentiment)
Avgsentiment
data <- data.frame(
  Apple_Comments=c("Average Total Sentiment","Average Positive Sentiment","Average Negative Sentiment") ,  
  value=c(Avgsentiment,AvgPositivesentiment,AvgNegativesentiment),
  colour = c('grey','green','red')
)

data 
ggplot(data, aes(x=Apple_Comments,y=value, fill=colour )) +  
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c('green','grey','red') ) +
  theme(legend.position="none") +
  geom_point() +
  geom_text(
    label=data$value,
    check_overlap=T)

############### ALPHA MODEL ###############

getSymbols("UNRATE",src="FRED") #Extracting data from FRED for unemployment rate
unemprate <- UNRATE["2021-01-01/2022-12-01"];
dim(unemprate); head(unemprate); tail(unemprate)
chartSeries(unemprate,theme='white')

getSymbols("CPIAUCSL",src="FRED") #Extracting data from FRED for CPI of Urban Consumers
cpi_data <- CPIAUCSL["2021-01-01/2022-12-01"];
dim(cpi_data); head(cpi_data); tail(cpi_data)
chartSeries(cpi_data,theme='white')

sym.vec <-c("AAPL", "GOOG", "^GSPC")
proj1data <-getSymbols(sym.vec, from = "2021-01-01", to = "2023-01-01")
str(proj1data)

AAPL <- AAPL[, "AAPL.Adjusted", drop=F]
AAPL.logret <-diff(log(AAPL))
AAPL.logret[1]; AAPL.logret[1] = 0.0
head(AAPL.logret)
GOOG <- GOOG[, "GOOG.Adjusted", drop=F]
GOOG.logret <-diff(log(GOOG))
GOOG.logret[1]; GOOG.logret[1] = 0.0
head(GOOG.logret)
GSPC <- GSPC[, "GSPC.Adjusted", drop=F]
GSPC.logret = diff(log(GSPC))
GSPC.logret[1]; GSPC.logret[1] = 0.0
head(GSPC.logret)

##converting the data to monthly###
AAPL_monthly <-to.monthly(AAPL)
GOOG_monthly <-to.monthly(GOOG)
GSPC_monthly <-to.monthly(GSPC)
AAPL_monthly_logret <-diff(log(AAPL_monthly))
GOOG_monthly_logret <-diff(log(GOOG_monthly))
GSPC_monthly_logret <-diff(log(GSPC_monthly))

GOOG_logret_close <-GOOG_monthly_logret$GOOG.Close
AAPL_logret_close <-AAPL_monthly_logret$AAPL.Close
market_logret_close <-GSPC_monthly_logret$GSPC.Close

summary(lm(GOOG_logret_close~market_logret_close))
summary(lm(AAPL_logret_close~market_logret_close))

google_model <-lm(GOOG_logret_close ~ market_logret_close + unemprate + cpi_data)
apple_model <-lm(AAPL_logret_close ~ market_logret_close + unemprate + cpi_data)

summary(google_model)
summary(apple_model)

############### THANK YOU ###############
#Sunal Sathwik Batchu BXS220020
#Harsha Mahanti SXM220117
#Chelluri Chiranjeevi Satya Abhisheik -  CXC210077