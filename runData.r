# Load Packages
pacman::p_load(pacman, rio, tidyverse, lubridate, forcats, stargazer)

# Load Run Data ############################################
# When adding years 2013-2019, change df1 to df20 and make full dataframe out of all years
df19 <- read.csv("~/Health Data Reformat 2020 - run2019.csv")
df19$Distance <- as.character(df19$Distance)

df20 <- read.csv("~/Health Data Reformat 2020 - run2020.csv")
df20$Distance <- as.character(df20$Distance)

df21 <- read.csv("~/Health Data Reformat 2020 - run2021.csv")
df21$Distance <- as.character(df21$Distance)

df1 <- bind_rows(df19,df20,df21)

#df1 <- read.csv("~/runDatajan2021.csv")

#rename columns
df1 <- df1 %>%
  rename(
    garminActivity = Activity.Type,
    favorite = Favorite,
    title = Title,
    distance = Distance,
    calories = Calories,
    time = Time,
    avgHR = Avg.HR,
    maxHR = Max.HR,
    aerobicTE = Aerobic.TE, 
    avgRunCadence = Avg.Run.Cadence,
    maxRunCadence = Max.Run.Cadence,
    avgPace = Avg.Pace,
    bestPace = Best.Pace,
    elevGain = Elev.Gain,
    elevLoss = Elev.Loss,
    avgStride = Avg.Stride.Length,
    avgVertRatio = Avg.Vertical.Ratio,
    avgVertOsc = Avg.Vertical.Oscillation,
    trainingStress = Training.Stress.Score?.,
    grit = Grit,
    flow = Flow,
    climbTime = Climb.Time,
    bottomTime = Bottom.Time,
    minTemp = Min.Temp,
    surfaceInterval = Surface.Interval,
    Decompression = Decompression,
    bestLap = Best.Lap.Time,
    numberLaps = Number.of.Laps,
    maxTemp = Max.Temp,
    runType = Run.Type,
    distanceType = Distance.Type,
    location = Location,
    shoes = Shoes,
    weather = Weather
  )

# Change Class for Date
df1$date <- as.character(df1$date)
df1$date <- as.Date(df1$date,format = "%m/%d/%Y")

#Add column for week number
df1$week <- isoweek(df1$date)

# READ ME: Pasting "00:0" into the avgPace and bestPace columns causes blank values to show 
# "00:0", too. I need to only paste the zeros into specific observations with times, not empty cells.

# Recode Time Data
# Change Class to hms for avgPace
df1$avgPace <- paste("00:0", df1$avgPace, sep = "")
df1$avgPace <- hms::as_hms(df1$avgPace)

# Change Class to hms for bestPace
df1$bestPace <- paste("00:0", df1$bestPace, sep = "")
df1$bestPace <- hms::as_hms(df1$bestPace)

# Change CLass to hms for total_time
df1$time <- as.character(df1$time)
df1$time <- hms::as_hms(df1$time)

# Change Class for miles
df1$distance <- as.character(df1$distance)
df1$distance <- as.numeric(df1$distance)

#Change Class for elevGain
df1$elevGain <- as.character(df1$elevGain)
df1$elevGain <- as.numeric(df1$elevGain)

# Change Class for Start Time
# df1$start_time <- paste(df1$start_time, ":00", sep = "")

#Change in source document: dateTime needs to contain date before Garmin data. Time needs to be 24hour

df1$dateTime <- as.character(df1$start_time)
df1$dateTime <- hms::as_hms(df$start_time)

# Convert start_time from 12 hr time to 24 hr time and change class
#depricated:
#df1$start_time <- as.POSIXct(df1$start_time, format = "%I:%M %p")
#df1$start_time <- strftime(df1$start_time, format = "%H:%M:%S")

# Separate the "weather" column

df1 <- separate(df1, col = "weather", into = c("temp", "conditions"), sep = ",")

# Change Class for variables
df1$temp <- as.numeric(df1$temp)

# Recode Run Types
df1$distanceType = fct_recode(df1$distanceType, "long_distance" = "Long Distance")

df1$distanceType = fct_recode(df1$distanceType, "mid_distance" = "Mid-Distance")
df1$distanceType = fct_recode(df1$distanceType, "mid_distance" = "Mid Distance")
df1$distanceType = fct_recode(df1$distanceType, "mid_distance" = "MId Distance")
df1$distanceType = fct_recode(df1$distanceType, "mid_distance" = "Mid Distnace")

df1$distanceType = fct_recode(df1$distanceType, "short_distance" = "Short-Distance")
df1$distanceType = fct_recode(df1$distanceType, "short_distance" = "Short Distance")

df1$location = fct_recode(df1$location, "Barnett Shoals, Old Lexington neighborhood" = "Barnet Shoals, Old Lexington Neighborhood")
df1$location = fct_recode(df1$location, "Barnett Shoals" = "Barnet Shoals")
df1$location = fct_recode(df1$location, "College Station, Campus" = "College Station to Campus")

#recode the running shoes

df1$shoes = fct_recode(df1$shoes, "Brooks Ghost 11" = "Brooks Ghost 11")
df1$shoes = fct_recode(df1$shoes, "Brooks Ghost 11" = "Brooks Ghost 11 ")

df1$distanceType = fct_recode(df1$distanceType, "short_distance" = "Short Distance")

as.factor(df1$weekDay)
fct_reorder(df1$weekDay, c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), .desc = TRUE)

#Separate runs by YEAR
df1$full_date <- df1$date
df1$full_date <- as.character(df1$full_date)
df1 <- df1 %>% separate(col = "full_date", into = c("year", "mm", "dd"), sep = "-")
df1$year <- as.double(df1$year)
df1$mm <- as.double(df1$mm)
df1$dd <- as.double(df1$dd)

# EVERYTHING ABOUT MY SHOES
# Calculate total miles in all shoes
ghostMiles <- df1 %>%  filter(shoes == "Brooks Ghost 11") %>% select(distance) %>% drop_na() %>% sum()
epicReactMiles <- df1 %>%  filter(shoes == "Nike Epic React 2") %>% select(distance) %>% sum()
zoomFly3 <- df1 %>% filter(shoes == "Nike Zoom Fly 3") %>% select(distance) %>% drop_na() %>% sum()
pegasus36 <- df1 %>% filter(shoes == "Nike Pegasus 36") %>% select(distance) %>% drop_na() %>% sum()
nbfcp <- df1 %>% filter(shoes == "New Balance Fuel Cell Propel") %>% select(distance) %>% drop_na %>%sum()
barefoot <- df1 %>% filter(shoes == "Barefoot") %>% select(distance) %>% drop_na() %>% sum()
reactMiler <- df1 %>% filter(shoes == "Nike React Miler") %>% select(distance) %>% drop_na() %>% sum()
pegasus37 <- df1 %>% filter(shoes == "Nike Pegasus 37") %>% select(distance) %>% drop_na() %>% sum()
hokaArahi4 <- df1 %>% filter(shoes == "Hoka Arahi 4") %>% select(distance) %>% drop_na() %>% sum()
endorphinSpeed <- df1 %>% filter(shoes == "Saucony Endorphin Speed") %>% select(distance) %>% drop_na()%>% sum() 
endorphinShift <- df1 %>% filter(shoes == "Saucony Endorphin Shift") %>% select(distance) %>% drop_na()%>% sum()
fastwitch9 <- df1 %>% filter(shoes == "Saucony Fastwitch 9") %>% select(distance) %>% drop_na() %>% sum()
glycerin19 <- df1 %>% filter(shoes == "Brooks Glycerin 19") %>% select(distance) %>% drop_na() %>% sum()
carbonX2 <- df1 %>% filter(shoes == "Hoka Carbon X 2") %>% select(distance) %>% drop_na() %>% sum()
# Dataframe for my SHOES!
shoes_df <- data.frame("shoes" = c("Nike Epic React 2", "Brooks Ghost 11","Nike Zoom Fly 3", "Nike Pegasus 36","NB Fuel Cell Propel","Nike React Miler", "Nike Pegasus 37", "Hoka Arahi 4", "Saucony Endrophin Speed", "Saucony Endorphin Shift", "Saucony Fastwitch 9", "Brooks Glycerin 19", "Hoka Carbon X 2", "Barefoot"), "miles" = c(epicReactMiles,ghostMiles,zoomFly3,pegasus36,nbfcp,reactMiler, pegasus37, hokaArahi4, endorphinSpeed, endorphinShift,fastwitch9,glycerin19,carbonX2,barefoot))

#create Garmin dataframe to graph data specific to Garmin use (ex.Heartrate)

garmin <- df1 %>% filter(date >= "2020-04-16")

# End Cleaning ####
# CALCULATIONS ####
totalMiles <- df1 %>% select(distance, year, mm) %>% drop_na()
total_miles <- sum(map_dbl(totalMiles$distance, sum))

total_miles2019 <- totalMiles %>% filter(year == "2019") %>% select(distance) %>% sum()
total_miles2020 <- totalMiles %>% filter(year == "2020") %>% select(distance) %>% sum()
total_miles2021 <- totalMiles %>% filter(year == "2021") %>% select(distance) %>% sum()

#depricated
#jan2019 <- totalMiles %>% filter(year == "2019" & mm =="1") %>% select (distance) %>% sum() 
#feb2019 <- totalMiles %>% filter(year == "2019" & mm =="2") %>% select (distance) %>% sum()
#mar2019 <- totalMiles %>% filter(year == "2019" & mm =="3") %>% select (distance) %>% sum()
#apr2019 <- totalMiles %>% filter(year == "2019" & mm =="4") %>% select (distance) %>% sum()
#may2019 <- totalMiles %>% filter(year == "2019" & mm =="5") %>% select (distance) %>% sum()
#jun2019 <- totalMiles %>% filter(year == "2019" & mm =="6") %>% select (distance) %>% sum()
#jul2019 <- totalMiles %>% filter(year == "2019" & mm =="7") %>% select (distance) %>% sum()
#aug2019 <- totalMiles %>% filter(year == "2019" & mm =="8") %>% select (distance) %>% sum()
#sep2019 <- totalMiles %>% filter(year == "2019" & mm =="9") %>% select (distance) %>% sum()
#oct2019 <- totalMiles %>% filter(year == "2019" & mm =="10") %>% select (distance) %>% sum()
#nov2019 <- totalMiles %>% filter(year == "2019" & mm =="11") %>% select (distance) %>% sum()
#dec2019 <- totalMiles %>% filter(year == "2019" & mm =="12") %>% select (distance) %>% sum()
#jan2020 <- totalMiles %>% filter(year == "2020" & mm =="1") %>% select (distance) %>% sum()
#feb2020 <- totalMiles %>% filter(year == "2020" & mm =="2") %>% select (distance) %>% sum()
#mar2020 <- totalMiles %>% filter(year == "2020" & mm =="3") %>% select (distance) %>% sum()
#apr2020 <- totalMiles %>% filter(year == "2020" & mm =="4") %>% select (distance) %>% sum()
#may2020 <- totalMiles %>% filter(year == "2020" & mm =="5") %>% select (distance) %>% sum()
#jun2020 <- totalMiles %>% filter(year == "2020" & mm =="6") %>% select (distance) %>% sum()
#jul2020 <- totalMiles %>% filter(year == "2020" & mm =="7") %>% select (distance) %>% sum()
#aug2020 <- totalMiles %>% filter(year == "2020" & mm == "8") %>% select(distance) %>% sum() 
#sep2020 <- totalMiles %>% filter(year == "2020" & mm == "9") %>% select(distance) %>% sum() 
#oct2020 <- totalMiles %>% filter(year == "2020" & mm == "10") %>% select(distance) %>% sum()
#nov2020 <- totalMiles %>% filter(year == "2020" & mm == "11") %>% select(distance) %>% sum()
#dec2020 <- totalMiles %>% filter(year == "2020" & mm == "12") %>% select(distance) %>% sum()
#jan2021 <- totalMiles %>% filter(year == "2021" & mm == "1") %>% select(distance) %>% sum()

#put it in a dataframe

#monthlyMiles <- data.frame("year" = c(2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2019, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020, 2020,2020,2020,2020,2021), "month" = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December","January"), "miles"= c(jan2019,feb2019,mar2019,apr2019,may2019,jun2019,jul2019,aug2019,sep2019,oct2019,nov2019,dec2019,jan2020,feb2020,mar2020,apr2020,may2020,jun2020,jul2020,aug2020,sep2020,oct2020,nov2020,dec2020,jan2021))

monthlyMiles <- df1 %>% select(year, mm, distance) %>% drop_na()%>% group_by(year,mm) %>% summarise(total = sum(distance))

df1 %>%
  select(distance)%>% drop_na() %>% sum()

df1 %>%
  select(elevGain)%>%drop_na() %>% sum()


#Calculate weekly mileage
weeklyMiles <- df1 %>% select(year, week, distance) %>% group_by(year,week) %>% drop_na() %>% summarise(total = sum(distance))
weeklyMiles2 <- weeklyMiles %>% group_by(year, week) %>% select(distance) %>% sum()

#END CALCULATIONS ####
df1 %>% 
  ggplot(aes(x=date, y=avgPace))+
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)

df1 %>%
  ggplot(aes(x=shoes, y=avgPace))+
  geom_boxplot()

df1 %>% 
  ggplot(aes(x=date, y=avgPace))+
  geom_smooth()

df1 %>% 
  ggplot(aes(x=date, y=distance))+
  geom_smooth()

df1 %>% 
  ggplot(aes(x=date, y=elevGain))+
  geom_smooth()

df1 %>%
  ggplot(aes(x=runType))+
  geom_bar()

df1 %>%
  ggplot(aes(x=temp, y=avgPace))+
  geom_smooth()

df1 %>% 
  ggplot(aes(x=distance, y=avgPace))+
  geom_smooth()

#heart rate from 4-16-2020 for distance runs
garmin %>% filter(runType == "Distance")%>%
  ggplot(aes(x=date, y=avgHR))+
  geom_smooth()

garmin %>% filter(distance > 7.9 & distance < 8.25)%>%
  ggplot(aes(x=date, y=avgHR))+
  geom_smooth()

#Track Workouts - Avg. HR
garmin %>% filter(runType == "Track")%>%
  ggplot(aes(x=date,y=avgHR))+
  geom_smooth()

#Track workouts - Max HR
garmin %>% filter(runType == "Track")%>%
  ggplot(aes(x=date,y=maxHR))+
  geom_smooth()

#Graphs with Regression Lines

df1 %>% filter(distance > 15)%>%
  ggplot(aes(x=date, y=avgPace))+
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)+
  labs(title="Average Pace", x="Date", y="Average Pace")

garmin %>% 
  ggplot(aes(x=date, y=avgHR))+
  geom_point()+
  geom_smooth(method = "lm",se = FALSE)+
  labs(title="Average HR", x="Date", y = "Average HR")

garmin %>% 
  ggplot(aes(x=temp, y=avgHR))+
  geom_smooth()


#weekly Volume Increase
#weeklyMiles %>%
#group_by(year) %>%
#ggplot(aes(x=week, y=total))+
#geom_smooth()

#write CSV file
#write.csv(df1, "C:/Users/jmm03185/Documents/MSBA/df1Run.csv")
