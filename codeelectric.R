#### load libraries ####
pacman::p_load("RMySQL","dplyr", "tidyr","lubridate","esquisse","padr","imputeTS","ggplot2", "chron","plotly", "forecast")

#### read SQL ####
con = dbConnect(MySQL(),
                user='deepAnalytics',
                password='Sqltask1234!',
                dbname='dataanalytics2018',
                host='data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com') #connecting SQL

dbListTables(con) #viewing tables                
dbListFields(con, "yr_2006") #viewing fields

#### Get querys ####
yr.2006 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, global_active_power FROM yr_2006")
yr.2007 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, global_active_power FROM yr_2007")
yr.2008 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, global_active_power FROM yr_2008")
yr.2009 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, global_active_power FROM yr_2009")
yr.2010 <- dbGetQuery(con, "SELECT Date, Time, Sub_metering_1, Sub_metering_2, Sub_metering_3, global_active_power FROM yr_2010")

#### check data ####
str(yr.2007)
head(yr.2007)
tail(yr.2007)
summary(yr.2007)
head(yr.2008)
tail(yr.2008)
str(yr.2009)
head(yr.2009)
tail(yr.2009)

#### bind df ####
df <- bind_rows(yr.2007, yr.2008, yr.2009, yr.2010) #bind df with a complete year

#### scales ####
df$global_active_power <- df$global_active_power*1000/60

#### data types ####
summary(df)
str(df)

#### date type ####
df$Date <- lubridate:: ymd(df$Date)

#### DateTime column ####
df$DateTime <- lubridate:: ymd_hms(paste(df$Date, df$Time))

sum(is.na(df)) 

#### dfs ####
dfs <- as.data.frame(df) #creating temporary df
dft <- as.data.frame(df) #creating temporary df

#### NA's ####
NAtable <- pad(dfs, by = "DateTime", break_above = 3) #putting NA's 
sum(is.na(NAtable)) 

sum(is.na(dfs)) 

#### calendar heat ####
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")

heatplot <- calendarHeat(dfs$Date, dfs$global_active_power)

#### arrange ####
NAtable <- arrange(NAtable, DateTime)

#### imputing values in missing values ####
NAtable <- na.interpolation(NAtable, option = "linear") 

dfs <- NAtable #NATable is now dfs
dfs$Date <- NULL
dfs$Time <- NULL
#### most information to the main df ####
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
#### Sampling ###

## by30 min ##
by30 <- dfs %>% group_by(cut(DateTime, "30 min")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,global_active_power, OtherAreas), funs(sum)) 
colnames(by30)[1] <- "DateTime" 

by30$Submeterings <- by30$Sub_metering_1+by30$Sub_metering_2+by30$Sub_metering_3
by30$lost <- by30$global_active_power-by30$Submeterings
by30$year <- year(by30$DateTime)
by30$month <- month(by30$DateTime)
by30$week <- week(by30$DateTime)
by30$day <- day(by30$DateTime)

by30$Season <- ifelse(by30$month == 12|by30$month == 1|by30$month == 2,"Winter",
                     ifelse(by30$month == 3|by30$month == 4|by30$month == 5, "Spring",
                            ifelse(by30$month == 6|by30$month == 7|by30$month == 8, "Summer", "Autumn")))

## by hour ##
byhour <- dfs %>% group_by(DateTime=floor_date(DateTime, "hour")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,global_active_power, OtherAreas), funs(sum))

byhour$Submeterings <- byhour$Sub_metering_1+byhour$Sub_metering_2+byhour$Sub_metering_3
byhour$lost <- byhour$global_active_power-byhour$Submeterings
byhour$year <- year(byhour$DateTime)
byhour$month <- month(byhour$DateTime)
byhour$week <- week(byhour$DateTime)
byhour$day <- day(byhour$DateTime)

byhour$Season <- ifelse(byhour$month == 12|byhour$month == 1|byhour$month == 2,"Winter",
                     ifelse(byhour$month == 3|byhour$month == 4|byhour$month == 5, "Spring",
                            ifelse(byhour$month == 6|byhour$month == 7|byhour$month == 8, "Summer", "Autumn")))

## by day ##
byday <- dfs %>% group_by(DateTime=floor_date(DateTime, "day")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3, global_active_power, OtherAreas), funs(sum)) #grouping by day

byday$Submeterings <- byday$Sub_metering_1+byday$Sub_metering_2+byday$Sub_metering_3
byday$lost <- byday$global_active_power-byday$Submeterings

byday$year <- year(byday$DateTime)
byday$month <- month(byday$DateTime)
byday$week <- week(byday$DateTime)
byday$day <- day(byday$DateTime)

byday$Season <- ifelse(byday$month == 12|byday$month == 1|byday$month == 2,"Winter",
                         ifelse(byday$month == 3|byday$month == 4|byday$month == 5, "Spring",
                                ifelse(byday$month == 6|byday$month == 7|byday$month == 8, "Summer", "Autumn")))

## by week ##
byweek <- dfs %>% group_by(DateTime=floor_date(DateTime, "week")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3, global_active_power, OtherAreas), funs(sum))#grouping by week

byweek$Submeterings <- byweek$Sub_metering_1+byweek$Sub_metering_2+byweek$Sub_metering_3
byweek$lost <- byweek$global_active_power-byweek$Submeterings

byweek$year <- year(byweek$DateTime)
byweek$month <- month(byweek$DateTime)
byweek$week <- week(byweek$DateTime)
byweek$day <- day(byweek$DateTime)

byweek$Season <- ifelse(byweek$month == 12|byweek$month == 1|byweek$month == 2,"Winter",
                       ifelse(byweek$month == 3|byweek$month == 4|byweek$month == 5, "Spring",
                              ifelse(byweek$month == 6|byweek$month == 7|byweek$month == 8, "Summer", "Autumn")))

byweek <- byweek[-c(1),]

## by month ##
bymonth <- dfs %>% group_by(DateTime=floor_date(DateTime, "month")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3, global_active_power, OtherAreas), funs(sum)) #grouping by month

bymonth$Submeterings <- bymonth$Sub_metering_1+bymonth$Sub_metering_2+bymonth$Sub_metering_3
bymonth$lost <- bymonth$global_active_power-bymonth$Submeterings

bymonth$year <- year(bymonth$DateTime)
bymonth$month <- month(bymonth$DateTime)
bymonth$week <- week(bymonth$DateTime)
bymonth$day <- day(bymonth$DateTime)

bymonth$Season <- ifelse(bymonth$month == 12|bymonth$month == 1|bymonth$month == 2,"Winter",
                        ifelse(bymonth$month == 3|bymonth$month == 4|bymonth$month == 5, "Spring",
                               ifelse(bymonth$month == 6|bymonth$month == 7|bymonth$month == 8, "Summer", "Autumn")))

## by season and year ##
byseason <- dfs %>% group_by(Season) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,global_active_power, OtherAreas), funs(sum)) #grouping by season
colnames(by30)[1] <- "DateTime" 

byyear <- dfs %>% group_by(DateTime=floor_date(DateTime, "year")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3, global_active_power, OtherAreas), funs(sum)) #grouping by year


#### client visualization ####
ggplot(data = bymonth, aes(x = DateTime)) +
  geom_line(aes(y = Submeterings, color = "Submetering")) +
  geom_line(aes(y = global_active_power, color = "Global Power")) +
  theme_minimal()+
  labs(title = "Global Power vs Submetering",
     x = "Time",
     y = "Power")

#### Subset and plotting ####
houseDay <- filter(by30, year == 2008 & month == 1 & day == 9) #subset 09/01/2008

plot_ly(houseDay, x = ~houseDay$DateTime, y = ~houseDay$Sub_metering_1,  #plot 3 submeters in 9/01/2008
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~houseDay$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseDay$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption January 9th, 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

houseWeek <- filter(by30, year == 2008 & week == 1) #subset 1st week 2008

plot_ly(houseWeek, x = ~houseWeek$DateTime, y = ~houseWeek$Sub_metering_1,  #plot 3 submeters in 1st week 2008
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~houseWeek$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 1st week 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

houseWeek2009 <- filter(by30, year == 2009 & week == 1) #subset 1st week 2009

plot_ly(houseWeek2009, x = ~houseWeek2009$DateTime, y = ~houseWeek2009$Sub_metering_1,  #plot 3 sub in 1st week 2009
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>% 
  add_trace(y = ~houseWeek2009$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~houseWeek2009$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 1st week 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))


#### By Year, month and week ####
plot_ly(bymonth, x = ~bymonth$DateTime, y = ~bymonth$Sub_metering_1, #plot month
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~bymonth$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~bymonth$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption by month",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(byweek, x = ~byweek$DateTime, y = ~byweek$Sub_metering_1, #plot week
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~byweek$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~byweek$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption by week",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#### comparison between years ####
by2007 <- filter(bymonth, year == 2007)
by2008 <- filter(bymonth, year == 2008)
by2009 <- filter(bymonth, year == 2009)
august2008 <- filter(byhour, year == 2008, month == 8) #holidays 2008

years.month <- bind_rows(by2007, by2008, by2009)

plot_ly(by2007, x = ~by2007$DateTime, y = ~by2007$Sub_metering_1, #plot 2007
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~by2007$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~by2007$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2007",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(by2008, x = ~by2008$DateTime, y = ~by2008$Sub_metering_1, #plot 2008
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~by2008$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~by2008$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(august2008, x = ~august2008$DateTime, y = ~august2008$Sub_metering_1, #Holidays 2008
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~august2008$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~august2008$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2008",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(by2009, x = ~by2009$DateTime, y = ~by2009$Sub_metering_1, #plot 2009
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~by2009$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~by2009$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption 2009",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

plot_ly(byseason, x = ~byseason$Season, y = ~byseason$Sub_metering_1, #plot Seasons
        name = 'Kitchen', type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~byseason$Sub_metering_2,
            name = 'Laundry Room', mode = 'lines') %>%
  add_trace(y = ~byseason$Sub_metering_3,
            name = 'Water Heater & AC', mode = 'lines') %>%
  layout(title = "Power Consumption by Season",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#### Seasons ####
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
  layout(title = "Power Consumption by Season",
         xaxis = list(title = "Time"),
         yaxis = list (title = "Power (watt-hours)"))

#### Forecasting ####

## create df to forecast ##


## NULL season cause character ##
byhour$Season <- NULL 
byday$Season <- NULL 
byweek$Season <- NULL 
bymonth$Season <- NULL 

## by hour ##
tshours <- ts(byhour, frequency = )

## by day ##
tsday <- ts(byday, frequency = 365.25*7)

## by week ##
tsweek <- ts(byweek,start = c(2007,1), end = c(2010, 47), frequency = 52)

## by month ##
tsmonth <- ts(bymonth, start = c(2007,1), end = c(2010,11), frequency = 12)

## ecplore ts ##
tshours
tsday
tsweek
tsmonth

#### month forecasting ####
plot.ts(tsmonth[,"Sub_metering_1"])
plot.ts(tsmonth[,"Sub_metering_2"])
plot.ts(tsmonth[,"Sub_metering_3"])
plot.ts(tsmonth[,"global_active_power"])
plot.ts(tsmonth[,"Submeterings"])

#### week forecasting ####
plot.ts(tsweek[,"Sub_metering_1"])
plot.ts(tsweek[,"Sub_metering_2"])
plot.ts(tsweek[,"Sub_metering_3"])
plot.ts(tsweek[,"global_active_power"])
plot.ts(tsweek[,"Submeterings"])

## week ##
tsweek[,"Submeterings"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Submeterings decomposition
          of weeks")

tsweek[,"Sub_metering_3"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Submeterings decomposition
          of weeks")

## month ##
tsmonth[,"global_active_power"] %>% stl(s.window = 12) %>%
  autoplot() + xlab("Year") +
  ggtitle("Submeterings decomposition
          of months")

tsmonth[,"Sub_metering_1"] %>% stl(s.window = 12) %>%
  autoplot() + xlab("Year") +
  ggtitle("Submetering 1 decomposition
          of months")

tsmonth[,"Sub_metering_2"] %>% stl(s.window = 12) %>%
  autoplot() + xlab("Year") +
  ggtitle("Submetering 2 decomposition
          of months")

tsmonth[,"Sub_metering_3"] %>% stl(s.window = 12) %>%
  autoplot() + xlab("Year") +
  ggtitle("Submetering 3 decomposition
          of months")

tsmonth[,"OtherAreas"] %>% decompose() %>%
  autoplot() + xlab("Year") +
  ggtitle("Other Areas decomposition
          of months")









