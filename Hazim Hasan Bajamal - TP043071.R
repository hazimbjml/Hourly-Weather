#Hazim Hasan Bajamal

#Libraries

library(dplyr)
library(ggplot2)
library(weathermetrics) 
library(corrplot)

#pre-processing
getwd()
setwd("/Users/hazimbajammal/Desktop/pfdaAssignment")
#import data to r studio
Data = read.csv(file = "HourlyWeatherData.csv")

#to check summary of the data and whether the data has missing values or clean
summary(Data)


#TREATMENT FOR wind_speed
Data$wind_speed = ifelse(is.na(Data$wind_speed),
                                     ave(Data$wind_speed,
                                         FUN = function(a) 
                                           mean(a, na.rm = 'TRUE')),
                                     Data$wind_speed)
#TREATMENT FOR pressure
Data$pressure = ifelse(is.na(Data$pressure),
                                    ave(Data$pressure,
                                        FUN = function(a) 
                                          mean(a, na.rm = 'TRUE')),
                                    Data$pressure)


#TREATMENT FOR Wind Direction
Mode = function(z){
  Mostfreq = unique(Data$wind_dir)
  Mostfreq[which.max(tabulate(match(Data$wind_dir,Mostfreq)))]
}
highestFreqWind_dir = Mode(z)
Data$wind_dir[which(is.na(Data$wind_dir))] <- highestFreqWind_dir


#it shows that the highest frequency falls under the value of 310
#therefore the missing value under Wind Direction variable will be changed into 310 as the highest frequency

#additional feature 1: Convert Fahrenheit degrees into Celsius

install.packages("weathermetrics")
library(weathermetrics)
Data$tempCelsius = fahrenheit.to.celsius(Data$temp)


#Analaysis 1: visualize the correlation for each numeric variables
#Additional Feature 2
install.packages("corrplot")
library(corrplot)
VariablesCorrealation = cor(Data[,3:14])
VisualizeVariablesCorrealation = corrplot(VariablesCorrealation)


#analysis 2: Distribution of Temperature Status

Data$temp_status = ifelse(Data$tempCelsius <=5.9,"Cold", ifelse(Data$tempCelsius >=6 & Data$tempCelsius <= 19.9, "Normal", "Hot") )
Data$temp_status = factor(Data$temp_status, levels = c("Cold","Normal","Hot"))

ggplot(Data, aes(x=temp_status, fill = temp_status)) + geom_bar(stat = "count") + 
  geom_text(stat = "count", aes(label=..count..), vjust=2)


#analysis 3: distribution of origin 

ggplot(Data, aes(origin, fill = origin)) + geom_bar()


table(Data$origin)

#Analysis 4

coordinate_wind_dir = ggplot(data =Data) +
  geom_bar(aes(wind_dir, ,fill=origin), show.legend = TRUE)+
  theme(aspect.ratio = 1)+
  labs(x=NULL,y=NULL) +
  facet_wrap(~origin) + theme_light() + 
  labs(title="Distribution of Wind Direction for Each Origin ", x="Degrees", y="Counts")

coordinate_wind_dir+coord_polar()

#analysis 5: distribution of month

Data$months = ifelse(Data$month == 1, "Jan",
              ifelse(Data$month == 2, "Feb",
              ifelse(Data$month == 3, "Mar",
              ifelse(Data$month == 4, "Apr",
              ifelse(Data$month == 5, "May",
              ifelse(Data$month == 6, "Jun",
              ifelse(Data$month == 7, "Jul",
              ifelse(Data$month == 8, "Aug",
              ifelse(Data$month == 9, "Sep",
              ifelse(Data$month == 10, "Oct",
              ifelse(Data$month == 11, "Nov", "Dec")))))))))))

Data$months <- factor(Data$months, levels = c("Jan","Feb","Mar",
                                              "Apr","May","Jun","Jul","Aug",
                                              "Sep","Oct","Nov","Dec"))



ggplot(Data, aes(x = months, fill = months )) + geom_bar(stat="count") +
  geom_text(stat = "count", aes(label=..count..), vjust=2)


#analysis 6: range of temperature in December

December = Data%>%filter(month==12)
ggplot(December, aes(x=tempCelsius)) + geom_freqpoly(col='blue',bins=24)

View(December)
max(December$tempCelsius)
mean(December$tempCelsius)
min(December$tempCelsius)sc
#it shows that the coldest temperature in December is on 25 of December that falls at -6.7 Celsius degrees, with the 
#average at 3.715, and the highest temperature is on 22 December at 20.6 Celsius degrees



#analysis 7: Study The Relation between Humidity and Dewpoint 

ggplot(Data,aes(x=humid,y=dewp))+geom_point()+geom_smooth(method="lm",color="green",formula=y~x)+
  labs(title = "Scatter Plot of Humidity and Dewpoint",x="Humidity",y="Dew point")

cor(x=Data$humid,y=Data$dewp,use="complete.obs")

#it shows that the as the dewpoint increased, the humidity most likely will increase aswell


#Analaysis 8: study the daily temperature for Fall Season, September, October, and November

FallSeason = Data%>%filter(months%in%c("Sep","Oct","Nov"))
FallSeasonLGA = FallSeason%>%filter(origin=="LGA")

ggplot(FallSeasonLGA,aes(x=day,y=tempCelsius,color=tempCelsius))+theme_dark()+geom_point()+
  labs(title="Temperature in days of Fall Season",x="Days",y="Temperature in Celsius")+
  geom_smooth(se=FALSE,method = "gam",formula = y~s(x,bs="cs"))+scale_color_gradient(low = "Pink",high = "Red") + facet_wrap(~months)


DescriptiveStat4 = FallSeasonLGA%>%summarise(origin="LGA",MaxTemp=max(tempCelsius,na.rm=TRUE),MinTemp=min(tempCelsius,na.rm=TRUE),MeanTemp=mean(tempCelsius,na.rm = TRUE))



#analysis 9: Distribution of Flight in weekly basis

VisualizeWeek = ggplot(Data, aes(theme = "dark", day)) +
  geom_histogram(col="orange",fill="darkgreen", binwidth = 9) + 
  theme_dark() + labs(title = "Distribution of Flight in Weekly Basis")



WeekCount = ggplot_build(VisualizeWeek)



#analysis 10: Wind Speed and Wind Gust in November
November = Data%>%filter(month==11)

ggplot(November, aes(x = wind_gust, y = wind_speed)) +
  geom_point() + labs(title="Scatter Plot Between Wind Speed and Wind Gust", x="wind speed", y="wind gust")+
  geom_smooth(se=FALSE,method="lm", formula = y~x, col="green")


cor(x=Data$wind_gust,y=Data$wind_speed,use="complete.obs")

#analysis 11: study the relationship between temp and dewpoint in February

February<-Data%>%filter(month==2)

ggplot(February, aes(x = temp, y = dewp)) + geom_point() +
  labs(title="Scatter plot between temperature and dew point", x="Temperature", y="Dew point")+
  geom_smooth(method="lm",formula = y~x, col="violet",) 

cor(x=Data$temp,y=Data$dewp,use="complete.obs")


#Analaysis 12: Distribution of Visibility Accross the Year

VisualizeVisib = ggplot(Data,aes(x=visib))+theme_bw()+geom_histogram(col="white",fill="blue", bins = 20)+facet_wrap(~origin)+
  labs(title="Distribution of Visibility Accross the Year",x="Visibility")
VisualizeVisib+coord_flip()

LowestVisibJFK = Data %>% filter(between(visib,0,1.25), origin=="JFK")
# in JFK, it has 247 records of very low visibility between 0 to 1.25 
LowestVisibLGA = Data %>% filter(between(visib,0,1.25), origin=="LGA")
# in LGA, it has 130 records of very low visibility between 0 to 1.25 

View(LowestVisibJFK)

#analaysis 13: Wind Gust Across The Year for Both Region

ggplot(Data, aes(wind_gust, colour = origin)) +
  geom_freqpoly(binwidth = 3) + facet_wrap(~months, scales = "free_x")

highestGust= Data%>%filter(month==1, wind_gust > 50)%>%View()
lowestGust= Data%>%filter(month==8, wind_gust > 30)

#it shows that the range of wind gust in August is the lowest compare to other month with the maximum wind gust through out the month is 33.3726.
#moreover, the highest wind gust is on 31st January at 3pm with the value of 62.1421 in LGA airport.
#furthermore, the wind gust in July for LGA is more constant compare to JFK.

#analysis 14: Temperature Across The Year for Both Region
  
ggplot(Data, aes(x=as.factor(month), y=tempCelsius, color=months)) + theme_bw()+geom_point() +
  labs(title = "Temperature Across The Year for Both Region", x="Month", y="Temperature in Celsius")
  
  
#analysis 15: temperature in May for both region
May = Data%>%filter(month==5)

ggplot(May, aes(x = day, y= tempCelsius, color=origin))+
                  geom_point() +
                  facet_wrap(~origin) +
                  labs(title ="Temperature in May for Both Region", x= "Days", y="Temperature in Celsius" )
                            
#it shows that there is extreme outliers in jfk airport  below the 10th days, therefore, to view the data is by using pipe filter
OutliersMay = May%>%filter(day < 10, tempCelsius<0)%>%View()

#the result shown the extreme outliers is -10.5 Celsius degrees on 8th may at 10pm , which is an inconsistent data and it is a wrong input.
#therefore, the value of -10.5 will be corrected to 10.5
 
Data%>%filter(month == 5, tempCelsius==-10.5)%>%mutate(tempCelsius=10.5)

 





