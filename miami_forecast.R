#import libraries
library(reshape2)
library(tidyr)
library(lubridate)
library(plyr)
require(xts)
library(tseries)
library(stats4)
library(forecast)
library(ggplot2)

cities=read.csv("city_rentals.csv")
long <- cities %>% gather(RegionName, Metro,CountyName,SizeRank)
long1=melt(cities,id.vars=c("RegionName","State","Metro",
                            "CountyName","SizeRank"))

#gsub
long1$variable=gsub('X','',long1$variable)
long1$variable=as.Date(long1$variable,"%Y%m")

year=substr(long1$variable,start=1,stop=4)
year1=as.data.frame(year)

month=substr(long1$variable,start=6,stop=7)
month1=as.data.frame(month)
dash=substr(long1$variable,start=5,stop=5)

#average rental price by year
long2=long1
final_ciudad=cbind(long2,year1)
#average price region and year (one-bedroom rental)
avg_prices=ddply(final_ciudad,.(RegionName,year),summarize,avg_price=mean(value))
avg_prices$year<-as.Date(avg_prices$year) #4545 cities overall 
avg_prices1=na.omit(avg_prices) #1717 cities left 
avg_prices1$year=as.Date(ISOdate(avg_prices1$year, 1, 1)) # beginning of the year

#change in average rent from a year earlier (17-18)
sub_cities=subset(avg_prices1,year=="2017-01-01"| year=="2018-01-01")

#long to wide by city name? (through feb 18?)
sub_cities_wide=reshape(sub_cities,idvar="RegionName",timevar="year",direction="wide")
sub_cities_wide1=rename(sub_cities_wide,c("RegionName"="City","avg_price.2017-01-01"="price_17","avg_price.2018-01-01"="price_18"))
sub_cities_wide1$percent_change=(sub_cities_wide1$price_18-sub_cities_wide1$price_17)/(sub_cities_wide1$price_17)
mean(sub_cities_wide1$percent_change) 

#miami subset for forecast 
miami=subset(avg_prices1,RegionName=="Miami")
#create a time series object
mia_ts=xts(miami$avg_price,order.by=as.POSIXct(miami$year),format="Y%m%d")

#stationary time series
#1. the mean is constant over time
#2. the variance dopes not increase over time
#3. seasonality effect is minimal 

#test for stationarity
adf.test(mia_ts) 
mean(miami$avg_price) #1823 
#forecast  
fit1=HoltWinters(mia_ts,beta=FALSE,gamma=FALSE)
forecast(fit1,3)#algo errors?

# dencer subset 
denver=subset(avg_prices1,RegionName=="Denver")
den_ts=xts(denver$avg_price,order.by=as.POSIXct(denver$year),format="Y%m%d")
fit2=HoltWinters(den_ts,beta=FALSE,gamma=FALSE)
forecast(fit2,3)

#average price by year (across all U.S. markets)
price_ano=ddply(avg_prices1,.(year),summarize,avg_price=mean(avg_price))

fill="gold1" #source: http://t-redactyl.io/blog/2016/04/creating-plots-in-r-using-ggplot2-part-10-boxplots.html
ggplot(price_ano,aes(x=year,y=avg_price))+geom_bar(stat="identity",fill=fill)+ 
  xlab("Year")+ylab("Average Price")+ggtitle("The Price for One-Bedroom Rentals Has Held Steady")+
  theme(plot.title = element_text(hjust = 0.5)) 

#scatterplot price in 2012 vs. 2017
sub_17=subset(avg_prices1,year=="2012-01-01" | year=="2017-01-01")

#long to wide format 
sub_17x=reshape(sub_17,idvar="RegionName",timevar="year",direction="wide")
sub_17x2=rename(sub_17x, c("RegionName"="city", "avg_price.2017-01-01"="avg_17","avg_price.2012-01-01"="avg_12"))
ggplot(sub_17x2,aes(x=avg_12,y=avg_17))+geom_point() 
