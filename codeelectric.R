#### load libraries ####
pacman::p_load("RMySQL","dplyr", "tidyr","lubridate","esquisse","padr","imputeTS","ggplot2", "chron")

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
str(yr.2008)
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
class(df$Date)

#### DateTime column ####
df$DateTime <- lubridate:: ymd_hms(paste(df$Date, df$Time))

sum(is.na(df)) 

#### dfs ####
dfs <- as.data.frame(df) #creating temporary df
dft <- as.data.frame(df) #creating temporary df

#### NA's ####
NAdetection <- pad(dfs,by = "DateTime",break_above = 3, interval = NULL, start_val = NULL, end_val = NULL, group = NULL) #putting NA's 

sum(is.na(NAdetection)) 

#### calendar heat ####
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")

heatplot <- calendarHeat(dfs$Date, dfs$global_active_power)

#### imputing values in missing values ####
dfs <- na.kalman(dfs) 

#### sampling data by hour ###

by15 <- dfs %>% group_by(cut(DateTime, "15 min")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,global_active_power), funs(sum)) #grouping by hour

byhour <- dfs %>% group_by(DateTime=floor_date(DateTime, "hour")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3,global_active_power), funs(sum))

byday <- dfs %>% group_by(DateTime=floor_date(DateTime, "day")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3, global_active_power), funs(sum))

bymonth <- dfs %>% group_by(DateTime=floor_date(DateTime, "month")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3, global_active_power), funs(sum))

byyear <- dfs %>% group_by(DateTime=floor_date(DateTime, "year")) %>%
  dplyr::summarize_at(vars(Sub_metering_1,Sub_metering_2,Sub_metering_3, global_active_power), funs(sum))

#### we uso by month for the client ####
bymonth$Submeterings <- bymonth$Sub_metering_1+bymonth$Sub_metering_2+bymonth$Sub_metering_3
bymonth$lost <- bymonth$global_active_power-bymonth$Submeterings

bymonth$year <- year(bymonth$DateTime)
bymonth$month <- month(bymonth$DateTime)
bymonth$week <- week(bymonth$DateTime)
bymonth$day <- day(bymonth$DateTime)

#### data visualization ####
esquisser()

ggplot(data = bymonth, aes(x = DateTime)) +
  geom_line(aes(y = Sub_metering_1, color = "Sub1")) +
  geom_line(aes(y = Sub_metering_2, color = "Sub2")) +
  geom_line(aes(y = Sub_metering_3, color = "Sub3")) +
  theme_minimal() +
  labs(title = "Global Power vs Submetering",
       x = "Time",
       y = "Power")

ggplot(data = bymonth, aes(x = DateTime)) +
  geom_line(aes(y = Submeterings, color = "Submetering")) +
  geom_line(aes(y = global_active_power, color = "Global Power")) +
  theme_minimal()+
  labs(title = "Global Power vs Submetering",
     x = "Time",
     y = "Power")

ggplot(data = bymonth, aes(x = DateTime)) +
  geom_line(aes(y = Submeterings, color = "Submetering")) +
  geom_line(aes(y = lost, color = "lost power")) +
  theme_minimal()+
  labs(title = "Global Power vs Submetering",
       x = "Time",
       y = "Power")



