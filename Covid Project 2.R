#NYC Coronavirus data ending 12/2

#data sourced from CDC Covid Data page

#Question 1: What did cases, vaccination rates look like grouped by state? By age?

library("dplyr")
library("ggplot2")
library("Hmisc")
library("RColorBrewer")
library("maps")
library("sp")
library("viridis")
library("mltools")
library("effects")
library("e1071")
library("fpp2")
library("xts")

install.packages("caret", dependencies = TRUE)

install.packages('rgdal', type = "source", configure.args=c(
  '--with-gdal-config=/Library/Frameworks/GDAL.framework/Programs/gdal-config',
  '--with-proj-include=/Library/Frameworks/PROJ.framework/Headers',
  '--with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib'))

install.packages('rgeos', type = "source", configure.args=c(
  '--with-gdal-config=/Library/Frameworks/GDAL.framework/Programs/gdal-config',
  '--with-proj-include=/Library/Frameworks/PROJ.framework/Headers',
  '--with-proj-lib=/Library/Frameworks/PROJ.framework/unix/lib'))

install.packages('maptools', dependencies=TRUE)

library("rgdal")
library("rgeos")
library("ggthemes")
library("leaflet")
library("maptools")
library("broom")
library("httr")
library("plyr")
library("tigris")
library("raster")

#file 2: cases and deaths by state over time.

by_state_cases_deaths <- read.csv("United_States_COVID-19_Cases_and_Deaths_by_State_over_Time.csv")

#file 3: vaccinations by type by state over time.

by_state_vaccinations <- read.csv("COVID-19_Vaccinations_in_the_United_States_Jurisdiction.csv")

#file 4: cases by age by state over time.

by_state_by_age_cases_deaths <- read.csv("COVID-19_Case_Surveillance_Public_Use_Data_with_Geography.csv")

#file 5: proportion of variants by state over time.

variants <- read.csv("StateVBMTable.csv", stringsAsFactors = FALSE)


#--------------------------------------------Step 2: EDA of State Data----------------------------------------------------------

#Use shapefile to begin map

my_spdf <- shapefile("cb_2018_us_state_20m.shp")

plot(my_spdf)

#merge and add data 

by_state_by_age_cases_deaths2 <- subset(by_state_by_age_cases_deaths, case_month == "2021-10")

test2 <- subset(by_state_by_age_cases_deaths, case_month == "2021-09")

t2  <- by_state_by_age_cases_deaths2 %>%
  group_by(res_state) %>%
  dplyr::summarize(count1 = n())

my_spdf1 <- fortify(my_spdf, region = "STUSPS")
plot(my_spdf1)

colnames(t2) <- c("id", "count1")
 
map2 <- left_join(my_spdf1, t2)

#add map details

map2 <- subset(map2, map2$long >= -150)
map2 <- subset(map2, map2$long <= -50)

map2 <- subset(map2, map2$lat >= 20)
map2 <- subset(map2, map2$lat <= 52)

map <- ggplot(data = map2, aes(x = long, y = lat, group = group))

map + geom_path()

map + 
  geom_polygon(aes(fill = as.factor(count1)), size = 0.1) +
  coord_fixed(1.3) + scale_fill_brewer(palette = 'OrRd')

map + 
  geom_polygon(aes(fill = count1), size = 0.1) +
  scale_fill_viridis(option = "plasma", labels = scales::comma) + labs(title = "Positive Cases" , fill = "Case Count", caption = "Source: CDC Covid Data Tracker as of 10/31/2021. Data unavailable for VT, PA, and LA.",  
                                               tag = "1")

#plot other parameters

#age cohorts
t3 <- by_state_by_age_cases_deaths2 %>%
  group_by(res_state) %>%
  filter(age_group == "0 - 17 years") %>%
  dplyr::summarize(count1 = n())

colnames(t3) <- c("id", "count1")

map3 <- left_join(my_spdf1, t3)

#add map details

map3 <- subset(map3, map3$long >= -150)
map3 <- subset(map3, map3$long <= -50)

map3 <- subset(map3, map3$lat >= 20)
map3 <- subset(map3, map3$lat <= 52)

map3.1 <- ggplot(data = map3, aes(x = long, y = lat, group = group))

map3.1 + geom_path()

map3.1 + 
  geom_polygon(aes(fill = count1), size = 0.1) +
  scale_fill_viridis(option = "plasma", labels = scales::comma) + labs(title = "Positive Cases" , fill = "Case Count", caption = "Source: CDC Covid Data Tracker as of 10/31/2021. Data unavailable for VT, PA, WV, TX, and LA.",  
                                               tag = "2", subtitle = "0-17 Years")


#18-49
t4 <- by_state_by_age_cases_deaths2 %>%
  group_by(res_state) %>%
  filter(age_group == "18 to 49 years") %>%
  dplyr::summarize(count1 = n())

colnames(t4) <- c("id", "count1")

map4 <- left_join(my_spdf1, t4)

#add map details

map4 <- subset(map4, map4$long >= -150)
map4 <- subset(map4, map4$long <= -50)

map4 <- subset(map4, map4$lat >= 20)
map4 <- subset(map4, map4$lat <= 52)

map4.1 <- ggplot(data = map4, aes(x = long, y = lat, group = group))

map4.1 + geom_path()

map4.1 + 
  geom_polygon(aes(fill = count1), size = 0.1) +
  scale_fill_viridis(option = "plasma", labels = scales::comma) + labs(title = "Positive Cases" , fill = "Case Count", caption = "Source: CDC Covid Data Tracker as of 10/31/2021. Data unavailable for WV, VT, PA, and LA.",  
                                               tag = "3", subtitle = "18 to 49 Years")


#50 to 64

t5 <- by_state_by_age_cases_deaths2 %>%
  group_by(res_state) %>%
  filter(age_group == "50 to 64 years") %>%
  dplyr::summarize(count1 = n())

colnames(t5) <- c("id", "count1")

map5 <- left_join(my_spdf1, t5)

#add map details

map5 <- subset(map5, map5$long >= -150)
map5 <- subset(map5, map5$long <= -50)

map5 <- subset(map5, map5$lat >= 20)
map5 <- subset(map5, map5$lat <= 52)

map5.1 <- ggplot(data = map5, aes(x = long, y = lat, group = group))

map5.1 + geom_path()

map5.1 + 
  geom_polygon(aes(fill = count1), size = 0.1) +
  scale_fill_viridis(option = "plasma", labels = scales::comma) + labs(title = "Positive Cases" , fill = "Case Count", caption = "Source: CDC Covid Data Tracker as of 10/31/2021. Data unavailable for TX, MS, WV, VT, PA, and LA.",  
                                               tag = "4", subtitle = "50 to 64 Years")

#65+ years

t6 <- by_state_by_age_cases_deaths2 %>%
  group_by(res_state) %>%
  filter(age_group == "65+ years") %>%
  dplyr::summarize(count1 = n())

colnames(t6) <- c("id", "count1")

map6 <- left_join(my_spdf1, t6)

#add map details

map6 <- subset(map6, map6$long >= -150)
map6 <- subset(map6, map6$long <= -50)

map6 <- subset(map6, map6$lat >= 20)
map6 <- subset(map6, map6$lat <= 52)

map6.1 <- ggplot(data = map6, aes(x = long, y = lat, group = group))

map6.1 + geom_path()

map6.1 + 
  geom_polygon(aes(fill = count1), size = 0.1) +
  scale_fill_viridis(option = "plasma", labels = scales::comma) + labs(title = "Positive Cases" , fill = "Case Count", caption = "Source: CDC Covid Data Tracker as of 10/31/2021. Data unavailable for TX, WV, VT, PA, and LA.",  
                                               tag = "5", subtitle = "65+ Years")


#percent delta
variants$res_state <- state.abb[match(variants$State.Jurisdiction,state.name)]
variants$Measure.Names <- NULL
variants$Total.Sequences <- as.numeric(gsub(",","",variants$Total.Sequences))

options(scipen = 999)

variants$variant_cases <- as.numeric(variants$Measure.Values) * variants$Total.Sequences

t7 <- variants %>%
  group_by(res_state) %>%
  filter(Variant %in%  c("AY.1", "AY.2", "B.1.617.2")) %>%
  dplyr::summarize(count1 = sum(variant_cases))

colnames(t7) <- c("id", "count1")

map7 <- left_join(my_spdf1, t7)

#add map details

map7 <- subset(map7, map7$long >= -150)
map7 <- subset(map7, map7$long <= -50)

map7 <- subset(map7, map7$lat >= 20)
map7 <- subset(map7, map7$lat <= 52)

map7.1 <- ggplot(data = map7, aes(x = long, y = lat, group = group))

map7.1 + geom_path()

map7.1 + 
  geom_polygon(aes(fill = count1), size = 0.1) +
  scale_fill_viridis(option = "plasma", labels = scales::comma) + labs(title = "Delta Variant" , fill = "Case Count", caption = "Source: CDC Covid Data Tracker as of 11/13/2021.",  
                                               tag = "6")


#percent vaccinated
vaccinated <- data.frame(matrix(NA,   
                          nrow = 23064))
vaccinated$Date <- by_state_vaccinations$Date
vaccinated$Location <- by_state_vaccinations$Location
vaccinated$Distributed <- by_state_vaccinations$Distributed

vaccinated$matrix.NA..nrow...23064. <- NULL

t8 <- vaccinated %>%
  group_by(Location) %>%
  filter(Date %in%  c("12/04/2021")) %>%
  distinct(Location, Distributed) %>%
  dplyr::summarize(count1 = sum(Distributed)) 

colnames(t8) <- c("id", "count1")

map8 <- left_join(my_spdf1, t8)

#add map details

map8 <- subset(map8, map8$long >= -150)
map8 <- subset(map8, map8$long <= -50)

map8 <- subset(map8, map8$lat >= 20)
map8 <- subset(map8, map8$lat <= 52)

map8.1 <- ggplot(data = map8, aes(x = long, y = lat, group = group))

map8.1 + geom_path()

map8.1 + 
  geom_polygon(aes(fill = count1), size = 0.1) +
  scale_fill_viridis(option = "plasma", labels = scales::comma) + labs(title = "Vaccinations" , fill = "Case Count", caption = "Source: CDC Covid Data Tracker as of 11/13/2021.",  
                                               tag = "7") 


#-----------------------------------------------------Step 3: Data Wrangling to Prep for Machine Learning------------------------------------------------------------------------
#trend forecasting: cases v. time, linear regression, and support vector machine.
#table total cases v time

trend1 <- by_state_cases_deaths

trend2 <- trend1 %>%
  group_by(submission_date) %>%
  dplyr::mutate(total_case_rollup = sum(tot_cases))

trend2 <- trend2 %>%
  group_by(submission_date) %>%
  dplyr::mutate(total_death_rollup = sum(tot_death))

trend2$state <- NULL; trend2$tot_cases <-NULL; trend2$conf_cases <- NULL; trend2$prob_cases <- NULL; trend2$new_case <- NULL;
trend2$pnew_case <- NULL; trend2$tot_death <- NULL; trend2$conf_death <- NULL; trend2$prob_death <- NULL; trend2$new_death <- NULL;
trend2$pnew_death <- NULL; trend2$created_at <- NULL; trend2$consent_cases <- NULL; trend2$consent_deaths <- NULL

trend2 <- distinct(trend2)

trend2$submission_date <- as.Date(trend2$submission_date, format = "%m/%d/%Y")

plot(trend2$submission_date, trend2$total_case_rollup)
plot(trend2$submission_date, trend2$total_death_rollup)

#cases
model1 <- lm(total_case_rollup ~ submission_date, data = trend2)
summary(model1)
plot(model1)

ggplot(trend2, aes(x = submission_date, y = total_case_rollup)) + 
  geom_point(size = .05) +
  geom_smooth(method = "lm", se=TRUE, col = "red") + labs(title = "Positive Cases over Time, US" , caption = "R-squared: 0.9612",  
                                                          tag = "1") + xlab("Date") + ylab("Total Cases") +
  theme_minimal() + scale_y_continuous(labels = scales::comma)

#prediction values

predict(model1, newdata= data.frame(submission_date = as.Date(c("2021-12-25", "2022-01-05", "2022-01-15"))))

#deaths
model2 <- lm(total_death_rollup ~ submission_date, data = trend2)
summary(model2)
plot(model2)

ggplot(trend2, aes(x = submission_date, y = total_death_rollup)) + 
  geom_point(size = .05) +
  geom_smooth(method = "lm", se=TRUE, col = "red") + labs(title = "Deaths over Time, US" , caption = "R-squared: 0.9783",  
                                                          tag = "1") + xlab("Date") + ylab("Total Deaths") +
  theme_minimal() + scale_y_continuous(labels = scales::comma)


#exponential smoothing

ts1 <-xts(trend2$total_case_rollup, trend2$submission_date)

# subset
ts2 <- window(ts1, end="2020-05-01")

# Inspect our data: 
autoplot(ts1) + 
  geom_line(color = "cyan") + 
  geom_point(color = "cyan")

library(forecast)
fit <- ets(ts1)
fc <- forecast(fit, 300)
plot(fc, xlim=c(0,1000), ylim= c(200000,150000000))

fc
summary (fit)

#using a historical lookback of 6 months, create a multilinear regression model to predict: what effect does age, state, vaccination rate, and time 
#have on number of cases?

regression_source1 <- subset(by_state_by_age_cases_deaths, case_month %in% c("2021-01", "2021-02", "2021-03", "2021-04","2021-05", "2021-06", "2021-07", "2021-08",
                                                                             "2021-09", "2021-10"))

regression_source1 <- by_state_by_age_cases_deaths
regression_source2 <- regression_source1[1:6]
regression_source2$state_fips_code <-NULL;regression_source2$res_county <- NULL;regression_source2$county_fips_code<-NULL;

source2  <- regression_source2 %>%
  group_by(res_state, case_month, age_group) %>%
  dplyr::mutate(count = n())

source2 <- distinct(source2)

#select top ten states, group rest as "other"
source3  <- source2 %>%
  group_by(res_state) %>%
  dplyr::mutate(state_total = sum(count))
                
source4 <- source3
source4$State <-ifelse(source4$res_state %in% c("CA", "FL", "NC", "NY", "GA", "OH", "TN", "PA", "IL", "SC"), source4$res_state, "OTHER")

source4$res_state <- NULL; source4$state_total <- NULL;

#time series regression on age and cases
#perform one hot encoding

source5 <- source4 %>% 
  mutate(CA = ifelse(State =="CA", 1, 0),
         FL = ifelse(State =="FL", 1, 0),
         NC = ifelse(State =="NC", 1, 0),
         NY = ifelse(State == "NY", 1, 0),
         GA = ifelse(State == "GA", 1, 0),
        OH = ifelse(State == "OH", 1, 0),
        TN = ifelse(State == "TN", 1, 0),
        PA = ifelse(State == "PA", 1, 0),
        IL = ifelse(State == "IL", 1, 0),
        SC = ifelse(State == "SC", 1, 0),
        OTHER = ifelse(State == "OTHER", 1, 0))

source6 <- source5 %>%
  mutate(Age1 = ifelse(age_group == "0 - 17 years", 1, 0),
         Age2 = ifelse(age_group == "18 to 49 years", 1, 0),
         Age3 = ifelse(age_group == "50 to 64 years", 1, 0),
         Age4 = ifelse(age_group == "65+ years", 1, 0),
         No_Age = ifelse(age_group == "Missing" || is.na(age_group) ==TRUE, 1, 0)
           )

source6$case_count <- source6$count
source6$age_group <- NULL; source6$count <- NULL; source6$State <- NULL

colinearity_test <-source6

colinearity_test$case_month <- NULL; colinearity_test$case_count <-NULL
cor(colinearity_test)
round(cor(colinearity_test), digits = 2)

#mlr using age, state
source6$case_month <- as.Date(source6$case_month, format = "%Y-%m")

fit1 <- lm(case_count ~ CA + FL + NC + NY + GA + OH + TN + PA + IL + SC + Age1 + Age2 + Age3 + Age4, data=source6)
summary(fit1)
plot(fit1)

anova(fit1)


#lr using vaccination
vaccinated$Date <- as.Date(vaccinated$Date,format = "%m/%d/%Y")

vaccinated <- subset(vaccinated, vaccinated$Location != "US")
vaccinated <- subset(vaccinated, vaccinated$Location != "VA2")
vaccinated <- subset(vaccinated, vaccinated$Location != "LTC")
vaccinated <- subset(vaccinated, vaccinated$Location != "DD2")


source10 <- vaccinated %>%
  group_by(Date) %>%
  dplyr::mutate(vaccinated_count = sum(Distributed))

source10$Location <- NULL; source10$Distributed <- NULL

source10$Date <- as.Date(source10$Date, format = "%m/%d/%Y")

source10$Date <- as.character(source10$Date)

source11 <- subset(source10, source10$Date %in% c("2021-01-01", "2021-02-01", "2021-03-01", "2021-04-01", "2021-05-01", "2021-06-01", "2021-07-01", "2021-08-01",
                                               "2021-09-01", "2021-10-01"))

source11 <- source10

source11$Date <- format(as.Date(source11$Date), format= "%Y-%m")

source11 <- distinct(source11)

source11.5 <- source4 %>%
  group_by(case_month) %>%
  dplyr::mutate(case_count_rollup = sum(count))

source11.5$age_group <- NULL; source11.5$count <- NULL; source11.5$State <- NULL

source11.5 <- distinct(source11.5)

source12 <- merge(source11.5, source11, by.x = 'case_month', by.y = 'Date')

plot(source12$vaccinated_count, source12$case_count_rollup)

fit2 <- lm(case_count_rollup ~ vaccinated_count, data = source12)

summary(fit2)
plot(fit2)

anova(fit2)

ggplot(source12, aes(x = vaccinated_count, y = case_count_rollup)) + 
  geom_point(size = .15) +
  geom_smooth(method = "lm", se=TRUE, col = "blue") + labs(title = "Vaccinations vs. Total Cases" , caption = "R-squared: 0.17"  
                                                          ) + xlab("Vaccinated") + ylab("Cases") +
  theme_minimal() + scale_y_continuous(labels = scales::comma) + scale_x_continuous(labels = scales::comma)


#final graphs

#age grouping
agebar<- source4 %>%
  group_by(age_group) %>%
  dplyr::mutate(case_count_rollup = sum(count))

agebar$State <- NULL; agebar$count <- NULL
agebar <- distinct(agebar)
agebar$case_month <- NULL;

agebar <- subset(agebar, agebar$age_group != "Missing")

p <- ggplot(agebar, aes(x=age_group, y=case_count_rollup), fill = age_group) +
  geom_bar(stat = "identity", width = .3)+ scale_fill_brewer(palette = "Set1") + labs(title = "Total Cases By Age Group" , caption = "Jan 2020-Oct 2021",  
                                                                                      tag = "1") + xlab("Age Group") + ylab("Total Cases") +
  theme_minimal() + scale_y_continuous(labels = scales::comma)
p


#state grouping
statebar<- source4 %>%
  group_by(State) %>%
  dplyr::mutate(case_count_rollup = sum(count))

statebar$age_group <- NULL; statebar$count <- NULL
statebar <- distinct(statebar)
statebar$case_month <- NULL;

statebar <- subset(statebar, statebar$age_group != "Missing")

p <- ggplot(statebar, aes(x=State, y=case_count_rollup), fill = State) +
  geom_bar(stat = "identity", width = .3)+ scale_fill_brewer(palette = "Set1") + labs(title = "Total Cases By State" , caption = "Jan 2020-Oct 2021",  
                                                                                      tag = "1") + xlab("State") + ylab("Total Cases") +
  theme_minimal() + scale_y_continuous(labels = scales::comma)
p















