#### PRE PROCESS ####

## Load libraries ##
pacman::p_load("RMySQL",
               "dplyr",
               "tidyr",
               "lubridate",
               "esquisse",
               "padr",
               "imputeTS",
               "ggplot2",
               "chron",
               "plotly",
               "forecast",
               "tseries",
               "zoo",
               "scales",
               "tidyquant",
               "xts",
               "forecastHybrid")

## Read SQL ##
con = dbConnect(MySQL(),
                user='deepAnalytics',
                password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com') #connecting SQL

dbListTables(con) #viewing tables                
dbListFields(con, "yr_2006") #viewing e.g fields

## Get querys ##
yr.2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, global_active_power FROM yr_2006")
yr.2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, global_active_power FROM yr_2007")
yr.2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, global_active_power FROM yr_2008")
yr.2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, global_active_power FROM yr_2009")
yr.2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, global_active_power FROM yr_2010")

## Bind df ##
df <- bind_rows(yr.2007, yr.2008, yr.2009, yr.2010) #bind df the tables we want

## Scales ##
df$global_active_power <- df$global_active_power*1000/60 #scaling Global Active Power as Watts per Hour

## Data types ##
summary(df)
str(df)

## Date type ##
df$Date <- lubridate:: ymd(df$Date)

## DateTime column ##
df$DateTime <- lubridate:: ymd_hms(paste(df$Date, df$Time))

sum(is.na(df)) 

## Temporary data frames ##
dfs <- as.data.frame(df) #creating temporary df
dft <- as.data.frame(df) #creating temporary df

## NA's ##
NAtable <- pad(dfs, by = "DateTime", break_above = 3) #putting NA's on daylights 
sum(is.na(NAtable)) 

sum(is.na(dfs)) 

## calendar heat ##
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")

heatplot <- calendarHeat(dfs$Date, dfs$global_active_power) #See where the NA's are

## Arrange ##
NAtable <- arrange(NAtable, DateTime)

## Imputation of NA's by interpolation ##
NAtable <- na.interpolation(NAtable, option = "linear") 

dfs <- NAtable #NATable is now dfs
dfs$Date <- NULL
dfs$Time <- NULL

## More information to the main df ##
dfs$Submeterings <- dfs$Sub_metering_1+dfs$Sub_metering_2+dfs$Sub_metering_3
dfs$OtherAreas <- dfs$global_active_power-dfs$Submeterings

dfs$year <- year(dfs$DateTime)
dfs$month <- month(dfs$DateTime)
dfs$week <- week(dfs$DateTime)
dfs$day <- day(dfs$DateTime)
dfs$hour <- hour(dfs$DateTime)
dfs$minute <- minute(dfs$DateTime)

dfs$Season <- ifelse(dfs$month == 12|dfs$month == 1|dfs$month == 2,"Winter",
                     ifelse(dfs$month == 3|dfs$month == 4|dfs$month == 5, "Spring",
                            ifelse(dfs$month == 6|dfs$month == 7|dfs$month == 8, "Summer", "Autumn"))) #season info

#### APPLIANCES ANALYSIS ####

## Detecting the fridge consumption with a common Pattern ##
loundryRoom <- select(dfs, 2,5)
loundryRoom <- as.data.frame(loundryRoom)
loundryRoom$year <- year(loundryRoom$DateTime)

loundryRoom$fridge <- ifelse(
  loundryRoom$Sub_metering_2 < 43,
  ifelse(
    loundryRoom$Sub_metering_2 == 0,
    "Stopped fridge",
    "Fridge"),
  "Other") 

sum(loundryRoom$Sub_metering_2)

fridge <- filter(loundryRoom, fridge == "Fridge")
sum(fridge$Sub_metering_2) # The 88% of consumption in Loundry Room comes from the Fridge

others <- filter(loundryRoom, fridge == "Other")
sum(others$Sub_metering_2) # The 11% of consumption in Loundry Room comes from others electrodomestics

#### GROUPING BY GRANULARITY IN ORDER TO UNDERSTAND DATA AND  FIND PATTERNS ###

## by30 min ##
by30 <- dfs %>% group_by(cut(DateTime, "30 min")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,global_active_power, OtherAreas), funs(sum)) 
colnames(by30)[1] <- "DateTime" 

by30$Submeterings <- by30$Sub_metering_1+by30$Sub_metering_2+by30$Sub_metering_3
by30$OtherAreas <- by30$global_active_power-by30$Submeterings
by30$year <- year(by30$DateTime)
by30$month <- month(by30$DateTime)
by30$week <- week(by30$DateTime)
by30$day <- day(by30$DateTime) #lubridate information

by30$Season <- ifelse(by30$month == 12|by30$month == 1|by30$month == 2,"Winter",
                     ifelse(by30$month == 3|by30$month == 4|by30$month == 5, "Spring",
                            ifelse(by30$month == 6|by30$month == 7|by30$month == 8, "Summer", "Autumn"))) #season info

## by hour ##
byhour <- dfs %>% group_by(DateTime=floor_date(DateTime, "hour")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,global_active_power, OtherAreas), funs(sum))

byhour$Submeterings <- byhour$Sub_metering_1+byhour$Sub_metering_2+byhour$Sub_metering_3
byhour$OtherAreas <- byhour$global_active_power-byhour$Submeterings
byhour$year <- year(byhour$DateTime)
byhour$month <- month(byhour$DateTime)
byhour$week <- week(byhour$DateTime)
byhour$day <- day(byhour$DateTime) #lubridate information

byhour$Season <- ifelse(byhour$month == 12|byhour$month == 1|byhour$month == 2,"Winter",
                     ifelse(byhour$month == 3|byhour$month == 4|byhour$month == 5, "Spring",
                            ifelse(byhour$month == 6|byhour$month == 7|byhour$month == 8, "Summer", "Autumn"))) #season info

## by day ##
byday <- dfs %>% group_by(DateTime=floor_date(DateTime, "day")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3, global_active_power, OtherAreas), funs(sum)) #grouping by day

byday$Submeterings <- byday$Sub_metering_1+byday$Sub_metering_2+byday$Sub_metering_3
byday$OtherAreas <- byday$global_active_power-byday$Submeterings

byday$year <- year(byday$DateTime)
byday$month <- month(byday$DateTime)
byday$week <- week(byday$DateTime)
byday$day <- day(byday$DateTime) #lubridate information

byday$Season <- ifelse(byday$month == 12|byday$month == 1|byday$month == 2,"Winter",
                         ifelse(byday$month == 3|byday$month == 4|byday$month == 5, "Spring",
                                ifelse(byday$month == 6|byday$month == 7|byday$month == 8, "Summer", "Autumn"))) #season info

## by week ##
byweek <- dfs %>% group_by(DateTime=floor_date(DateTime, "week")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3, global_active_power, OtherAreas), funs(sum))#grouping by week

byweek$Submeterings <- byweek$Sub_metering_1+byweek$Sub_metering_2+byweek$Sub_metering_3
byweek$OtherAreas <- byweek$global_active_power-byweek$Submeterings

byweek$year <- year(byweek$DateTime)
byweek$month <- month(byweek$DateTime)
byweek$week <- week(byweek$DateTime)
byweek$day <- day(byweek$DateTime) #lubridate information

byweek$Season <- ifelse(byweek$month == 12|byweek$month == 1|byweek$month == 2,"Winter",
                       ifelse(byweek$month == 3|byweek$month == 4|byweek$month == 5, "Spring",
                              ifelse(byweek$month == 6|byweek$month == 7|byweek$month == 8, "Summer", "Autumn"))) #season info

byweek <- byweek[-c(1),]

## by month ##
bymonth <- dfs %>% group_by(DateTime=floor_date(DateTime, "month")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3, global_active_power, OtherAreas), funs(sum)) #grouping by month

bymonth$Submeterings <- bymonth$Sub_metering_1+bymonth$Sub_metering_2+bymonth$Sub_metering_3
bymonth$OtherAreas <- bymonth$global_active_power-bymonth$Submeterings

bymonth$year <- year(bymonth$DateTime)
bymonth$month <- month(bymonth$DateTime)
bymonth$week <- week(bymonth$DateTime)
bymonth$day <- day(bymonth$DateTime) #lubridate information

bymonth$Season <- ifelse(bymonth$month == 12|bymonth$month == 1|bymonth$month == 2,"Winter",
                        ifelse(bymonth$month == 3|bymonth$month == 4|bymonth$month == 5, "Spring",
                               ifelse(bymonth$month == 6|bymonth$month == 7|bymonth$month == 8, "Summer", "Autumn"))) #season info

## by season and year ##
byseason <- dfs %>% group_by(Season) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,global_active_power, OtherAreas), funs(sum)) #grouping by season
colnames(by30)[1] <- "DateTime" 

byyear <- dfs %>% group_by(DateTime=floor_date(DateTime, "year")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3, global_active_power, OtherAreas), funs(sum)) #grouping by year


#### DATA VISALIZATION ####

## The submeterings are not tracking all the consumption in the house ##
ggplot(data = bymonth, aes(x = DateTime)) +
  geom_line(aes(y = Submeterings, color = "Submetering")) +
  geom_line(aes(y = global_active_power, color = "Global Power")) +
  theme_minimal()+
  labs(title = "Global Power vs Submetering",
     x = "Time",
     y = "Power")

## Subset and Plot one day to undersand behaviors of the family ##
houseDay <- filter(by30, year == 2008 & month == 1 & day == 9) #subset 09/01/2008

plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1,  #plot 3 submeters and GAP in 9/01/2008
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~houseDay$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseDay$global_active_power,
            name = 'Global Active Power', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Subset and plot Weeks to undersand behaviors of the family ##
houseWeek <- filter(by30, year == 2008 & week == 1) #subset 1st week 2008

plot_ly(houseWeek, x = ~houseWeek$DateTime, y = ~houseWeek$Sub_metering_1,  #plot 3 submeters and GAP in 1st week 2008
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~houseWeek$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseWeek$global_active_power,
            name = 'Global Active Power', mode = 'lines') %>%
  layout(title = "Power Consumption 1st week 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

houseWeek2009 <- filter(by30, year == 2009 & week == 1) #subset 1st week 2009

plot_ly(houseWeek2009, x = ~houseWeek2009$DateTime, y = ~houseWeek2009$Sub_metering_1,  #plot 3 sub amnd GAP in 1st week 2009
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~houseWeek2009$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek2009$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseWeek2009$global_active_power,
            name = 'Global Active Power', mode = 'lines') %>%
  layout(title = "Power Consumption 1st week 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## 3 submeters by month and by week in hole year ##
plot_ly(bymonth, x = ~bymonth$DateTime, y = ~bymonth$Sub_metering_1, #plot month
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~bymonth$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~bymonth$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~bymonth$global_active_power,
            name = 'Global Active Power', mode = 'lines') %>%
  layout(title = "Power Consumption by month",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(byweek, x = ~byweek$DateTime, y = ~byweek$Sub_metering_1, #plot week
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~byweek$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~byweek$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~byweek$global_active_power,
            name = 'Global Active Power', mode = 'lines') %>%
  layout(title = "Power Consumption by week",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Comparison between years ##
by2007 <- filter(bymonth, year == 2007)
by2008 <- filter(bymonth, year == 2008)
by2009 <- filter(bymonth, year == 2009)
by2010 <- filter(bymonth, year == 2010)

## Comparison beetween years ####
years.month <- bind_rows(by2007, by2008, by2009, by2010)

plot_ly(by2007, x = ~by2007$DateTime, y = ~by2007$Sub_metering_1, #plot 2007
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~by2007$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~by2007$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~by2007$global_active_power,
            name = 'Global Active Power', mode = 'lines') %>%
  layout(title = "Power Consumption 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(by2008, x = ~by2008$DateTime, y = ~by2008$Sub_metering_1, #plot 2008
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~by2008$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~by2008$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~by2008$global_active_power,
            name = 'Global Active Power', mode = 'lines') %>%
  layout(title = "Power Consumption 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


plot_ly(by2009, x = ~by2009$DateTime, y = ~by2009$Sub_metering_1, #plot 2009
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~by2009$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~by2009$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~by2009$global_active_power,
            name = 'Global Active Power', mode = 'lines') %>%
  layout(title = "Power Consumption 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

## Detecting possible outlier ##
august2008 <- filter(byhour, year == 2008, month == 8) #holidays 2008
plot_ly(august2008, x = ~august2008$DateTime, y = ~august2008$global_active_power, #Holidays 2008
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  layout(title = "Power Consumption 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


## Seasons ##
houseWinter <- filter(byhour, Season == "Winter", year == 2009)
houseSummer <- filter(byhour, Season == "Summer", year == 2009)
houseAutumn <- filter(by30, Season == "Autumn")
houseSpring <- filter(by30, Season == "Spring")

plot_ly(houseSummer, x = ~houseSummer$DateTime, y = ~houseSummer$Sub_metering_1, #plot Summer 2009
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~houseSummer$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseSummer$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  add_trace(y = ~houseSummer$global_active_power,
            name = 'Global Active Power', mode = 'lines') %>%
  layout(title = "Power Consumption by Season",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#### TIME SERIES USING GLOBAL ACTIVE POWER ####

## NULL season cause character ##
byhour$Season <- NULL 
byday$Season <- NULL 
byweek$Season <- NULL 
bymonth$Season <- NULL 

## Time Series ##
tshour <- ts(byhour, frequency = 8760) #byhour

tsday <- msts(byday, seasonal.periods = c(7,356.25)) #byday

tsweek <- ts(byweek,start = c(2007,1), end = c(2010, 47), frequency = 52) #byweek

tsmonth <- ts(bymonth, start = c(2007,1), end = c(2010,10), frequency = 12) #bymonth

## Global Active Power by month visualization ##
plot.ts(tsmonth[,"global_active_power"])

#### Global Active Power by week visualization ####
plot.ts(tsweek[,"global_active_power"])

## GAP decomposition by week with STL ##
tsweek[,"global_active_power"] %>% stl(s.window = 52) %>%
  autoplot() + xlab("Year") +
  ggtitle("Gap decomposition
          by week")

## ts month GAP decomposition by month with STL ##
tsmonth[,"global_active_power"] %>% stl(s.window = 12) %>%
  autoplot() + xlab("Year") +
  ggtitle("GAP decomposition
          by month")

#### Selecting Granularity to use (MAE of Remainder/Ramdom) ####
monthgap <- stl(tsmonth[,"global_active_power"], s.window = 12)
weekgap <- stl(tsweek[,"global_active_power"], s.window = 12)
daygap <- stl(tsday[,"global_active_power"], s.window = 12)
hourgap <- stl(tshour[,"global_active_power"], s.window = 12)

monthremainder <- monthgap$time.series[,3]
weekremainder <- weekgap$time.series[,3]
dayremainder <- daygap$time.series[,3]
hourremainder <- hourgap$time.series[,3]

monthremainder <- as.data.frame(monthremainder)
weekremainder <- as.data.frame(weekremainder)
dayremainder <- as.data.frame(dayremainder)
hourremainder <- as.data.frame(hourremainder)

metricGAP <- c() 
metricGAP$Month <- mean(abs(monthremainder$x))/mean(tsmonth[,5]) #Column 5 is Global Active Power
metricGAP$Week <- mean(abs(weekremainder$x))/mean(tsweek[,5])
metricGAP$Day <- mean(abs(dayremainder$x))/mean(tsday[,5])
metricGAP$Hour <- mean(abs(hourremainder$x))/mean(tshour[,5])

metricGAP <- as.data.frame(metricGAP) 
metricGAP <- gather(metricGAP) #pivot table

ggplot(data = metricGAP) +
  aes(x = key, weight = value) +
  geom_bar(fill = "#0c4c8a") +
  labs(title = "MAE per granularity ",
       x = "Variables",
       y = "Error",
       subtitle = "GAP") +
  theme_minimal() #Decide to use Monthly granularity

#### 1st FORECAST With Gblobal Active Power ####

## Train and test ##
train <- window(tsmonth[,"global_active_power"], start = c(2007,1), end = c(2010,1))
test <- window(tsmonth[,"global_active_power"], start = c(2010,2))

## HW ##
hwModel <- HoltWinters(train)
plot(hwModel)

mpredictHW <- forecast(hwModel, h = 10, seasonal = "multiplicative")

accuracy(mpredictHW, test)

## ARIMA ##
arimaModel <- auto.arima(train)

predictArima <- forecast(arimaModel, h = 10)

plot(predictArima)

accuracy(predictArima, test)

## Plot models 1st Forecast ##
autoplot(tsmonth[,"global_active_power"], series = "Real Gap")+
  autolayer(predictArima, series = "Arima Gap", PI = FALSE)+
  autolayer(mpredictHW, series = "Holt Winters Gap", PI = FALSE)+
  xlab("Years") +
  ylab("Electric Consumption (watts/hour)") +
  ggtitle("Models with month Granularity") +
  guides(colour=guide_legend(title="models"))


#### FORECAST WITHOUT OUTLIER (Holidays of August 2008) ####

#### OUTLIERS ####
outliers <- select(bymonth, 1,5) #selecting Global Active Power and DateTime from bymonth grouping

dfoutliers <- ifelse(outliers$global_active_power < 206000, 
       mean(as.numeric(c(outliers[8,2], outliers[32,2], outliers[44,2]))),
       outliers$global_active_power) #replacing outlier of August 2008 by mean of the August of 2007,2009 and 2010.

dfoutliers <- as.data.frame(dfoutliers)
dfoutliers$DateTime <- outliers$DateTime #create DateTime column
names(dfoutliers)[1]<- "GAP"
dfoutliers$Year <- year(dfoutliers$DateTime)
dfoutliers$Month <- month(dfoutliers$DateTime)

## Time Series ##
tsGAP <- ts(dfoutliers, start = c(2007,1), end = c(2010,10), frequency = 12) 
plot.ts(tsGAP[,"GAP"])

## tsGAP decomposition by month with STL ##
tsGAP[,"GAP"] %>% stl(s.window = 12) %>%
  autoplot() + xlab("Year") +
  ggtitle("GAP decomposition
          by month")
  
## Train and Test ##
trainGAP <- window(tsGAP[,"GAP"], start = c(2007,1), end = c(2010,1))
testGAP <- window(tsGAP[,"GAP"], start = c(2010,2))

## HW GAP ##
hwModelGAP <- HoltWinters(trainGAP)
plot(hwModelGAP)

GAPmpredictHW <- forecast(hwModelGAP, h = 18, seasonal = "multiplicative") # Best Model

accuracy(GAPmpredictHW, testGAP)

#### ARIMA GAP ####
arimaModelGAP <- auto.arima(trainGAP)

predictArimaGAP <- forecast(arimaModelGAP, h = 18)

plot(predictArimaGAP)

accuracy(predictArimaGAP, testGAP)

## Hibrid Model ##
hibridModel <- hybridModel(trainGAP, weights = "equal")
hibridModelI <- hybridModel(trainGAP, weights = "insample")

predictHibrid <- forecast(hibridModel, h = 18)
predictHIbridI <- forecast(hibridModelI, h = 18)

accuracy(predictHibrid, testGAP) #worst results
accuracy(predictHIbridI, testGAP) #worst results


## Plot multiple models models ##
autoplot(tsGAP[,"GAP"], series = "Real Gap")+
  autolayer(predictArimaGAP, series = "Arima Gap", PI = FALSE)+
  autolayer(GAPmpredictHW, series = "Holt Winters Gap", PI = FALSE)+
  autolayer(predictHIbridI, series = "Hybrid Model", PI = FALSE)+
  xlab("Years") +
  ylab("Electric Consumption (watts/hour)") +
  ggtitle("Models with month Granularity") +
  guides(colour=guide_legend(title="models"))

#### FINAL VISUALIZATION ####

## cut to plot it ##
shortened <- window(GAPmpredictHW$mean, start= c(2010, 10))

str(GAPmpredictHW$mean)

autoplot(tsGAP[,"GAP"], series = "Real Gap")+
  autolayer(shortened, series = "Holt Winters Gap", PI = FALSE)+
  xlab("Years") +
  ylab("Electric Consumption (watts/hour)") +
  ggtitle("Model with month Granularity") +
  guides(colour=guide_legend(title="Model"))


#### POWER BI ####

write.csv(tsGAP, file = "TimeSeries.csv")










