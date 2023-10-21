
url_next <- "https://vincentarelbundock.github.io/Rdatasets/csv/causaldata/restaurant_inspections.csv"
df <- read.csv(url_next)

## Let's first understand the dataframe columns and its subject and get basic idea of what data is regarding. 

## Business name - Name of restaurent or name of chain
## inspection_score - Score at the time of inspection
## Year - Year of inspections
## Number of locations of restaurent of particular location.
## was inspection done on week end or weekdays. False - Weekdays True - Weekend. 

## data source - https://vincentarelbundock.github.io/Rdatasets/articles/data.html  & https://www.kaggle.com/datasets/loulouashley/inspection-score-restaurant-inspection?select=restaurant-and-food-inspections-1.csv
## data dictionary - https://vincentarelbundock.github.io/Rdatasets/doc/causaldata/restaurant_inspections.html 

head(df)

## purpose of dataset - We have dataset of restaurent inspection score performed in Anchorage, Alaska. We have different restaurents and their inspection scores over the years we can describle that how the score has imporoved
## over the time also is there any difference if inspection done in weekends or weekdays how does it affect the score as number of guest at weekends are more as compare to weekdays. 

dim(df)
str(df)

## Let's check do we need to drop any columns or replace null values with any desired number ? 

sum(is.na(df))

## we can clearly see that data has zero null values.

##  let's print box plot for outlier analysis.

bx_1 <- boxplot(df$inspection_score, 
                main = "Inspection score boxplot", 
                ylab = "score")

bx_2 <- boxplot(df$NumberofLocations, 
                main = "Number of locations box plot", 
                ylab = "number of locations")


## we can see that we have few outliers for inspection score column in lower quartiles and few outliers in upper quartiles for number of locations.. 


q1 <- quantile(df$inspection_score, 0.25)
q3 <- quantile(df$inspection_score, 0.75)
IQR <- q3-q1
lower <- q1 - 1.5*IQR
upper <- q3 + 1.5*IQR
df <- df[ which(df$inspection_score < upper
                & df$inspection_score > lower), ]
cat(lower,upper)

q1 <- quantile(df$NumberofLocations, 0.25)
q3 <- quantile(df$NumberofLocations, 0.75)
IQR <- q3-q1
lower <- q1 - 1.5*IQR
upper <- q3 + 1.5*IQR
df <- df[ which(df$NumberofLocations < upper
                & df$NumberofLocations > lower), ]
cat(lower,upper)

## lower and upper for inspection column is 77.5 and 113.5 we can clealry see in box plot all data point are in given range hence no outliers.
## lower and upper for numberof location column is -20 and 100 we can clealry see in box plot all data point are in given range hence no outliers.
## we can clearly see that we have removed outliers from both the columns and now our data is cleaned. 
bx_1_no_out <- boxplot(df$inspection_score, 
                       main = "Inspection score boxplot", 
                       ylab = "score" )

bx_2_no_out <- boxplot(df$NumberofLocations, 
                       main = "Number of locations box plot", 
                       ylab = "number of locations")

### data is free from null value and outliers let's explore further. 

names <- data.frame(table(df$business_name))
names <- setNames(names, c("name","count"))
names <-names[order(names$stores,decreasing=TRUE,na.last=FALSE),]
head(names,10)

## we can clearly see that there are few restaurents which are repeating so let's check how many are unique.

dim(names)

## so out of 27178 number of uniques names is 1571 hence we can clearly see that each year inspection is conducted and same is recored in this dataframe. 

## let's check summary of cleaned dataframe.
## here we can see that data is for 20 years starting from 2000 to 2019. 
## Also mostly inspection is conducted on weekdays. 
## max inspection score in cleaned data is 100 i.e ideal number of scoring range and maximum number locations for any store is 96. 
summary(df)


hist(df$NumberofLocations,xlab = "Number of Locations",col = "brown",border = "black")
## we can see that as number of locations increases in x axis frequency of number of stores with more number of locations decreases which is the ideal situation in real time. 

hist(df$inspection_score,xlab = "inspection_score",col = "green",border = "black")
### we can clealry see that maximum number of restaurents hold inspection score which is greater then 95 which is actually a positive score and good score for those restaurants. 

df["count"] <- 1
head(df)

## If we compare inspection with years we get an idea each year how many inspections were done over these 20 years and also average number of inspection scores each year and how the score is shifting over these 20 years. 

## First let's find how many inspection were done each year. 
g_yer <- data.frame(aggregate(df$count, list(df$Year), FUN=sum)) 
g_yer <- setNames(g_yer, c("year","Num_of_inspection"))
g_yer


barplot(g_yer$Num_of_inspection, names.arg=g_yer$Year ,xlab="Year",ylab="Number of inspection",col="red", main="Number of inspection each year over the years",border="red")
mean(g_yer_score$inspection_score_avg)

## Now let's findaverage inspection score each year. 
g_yer_score <- data.frame(aggregate(df$inspection_score, list(df$Year), FUN=mean)) 
g_yer_score <- setNames(g_yer_score, c("year","inspection_score_avg"))
g_yer_score


barplot(g_yer_score$inspection_score_avg, names.arg=g_yer_score$Year ,xlab="Year",ylab="inspection_score_avg",col="blue", main="average score of inspection each year over the years",border="red")


## ONE SAMPLE T TEST
## H0 NULL : Average mean of average of each year is 96
## H1 ALTERNATE : mean is not equal to 96


## https://www.guru99.com/r-t-test-one-sample.html


t.test(g_yer_score$inspection_score_avg, mu = 96)

head(df)
str(df)


### Looking at the above graph we can see that average mean of inspection score in year 2018 and 2017 is same 
## let's see that same with two sample t-test. 

df_17 = df[df["Year"] == 2017,] 
df_18 = df[df["Year"] == 2018, ] 

library(dplyr)
#df_17 %>% filter(row("Year") == 2017)

## http://www.sthda.com/english/wiki/unpaired-two-samples-t-test-in-r
two_tailed <- t.test(df_17['inspection_score'], df_18['inspection_score'])
two_tailed
head(df)


library(dplyr)
df_max <- df %>%
  group_by(business_name) %>%
  slice(which.max(Year))
column <- c("business_name","Year","inspection_score","NumberofLocations")
df_max <- df_max[column]
names(df_max)[names(df_max) == "inspection_score"] <- "inspection_score_recent_yr"
names(df_max)[names(df_max) == "Year"] <- "Year_recent_yr"
names(df_max)[names(df_max) == "NumberofLocations"] <- "NumberofLocations_recent_yr"
head(df_max)


library(dplyr)
df_min <- df %>%
  group_by(business_name) %>%
  slice(which.min(Year))
column <- c("business_name","Year","inspection_score","NumberofLocations")
df_min <- df_min[column]
names(df_min)[names(df_min) == "inspection_score"] <- "inspection_score_start"
names(df_min)[names(df_min) == "Year"] <- "Year_start"
names(df_min)[names(df_min) == "NumberofLocations"] <- "NumberofLocations_start"
head(df_min)


library(tidyverse)
joined_data <- left_join(df_min, df_max, by = "business_name")

dim(joined_data)

joined_data$Location_diff <- joined_data$NumberofLocations_recent_yr-joined_data$NumberofLocations_start
joined_data$num_yrs_old <- joined_data$Year_recent_yr-joined_data$Year_start
joined_data$inspection_score_diff <- joined_data$inspection_score_recent_yr-joined_data$inspection_score_start

head(joined_data)
dim(joined_data)

joined_data <- joined_data[order(-joined_data$inspection_score_diff),]
head(joined_data)
mean(joined_data$inspection_score_diff)

nf <- data.frame(table(joined_data$num_yrs_old))
barplot(nf$Freq, names.arg=nf$Var1 ,xlab="Year",ylab="Frequency of restaurants number",col="red", main="Frequcy of restaurants being older",border="red",cex.names=0.8)
head(df)

install.packages("fastDummies")
library('fastDummies')

### https://www.marsja.se/create-dummy-variables-in-r/
df$Year <- as.character(df$Year)

library(tidyverse)
column <- c("business_name","num_yrs_old")
df_zz <- joined_data[column]
new_frame <- left_join(df, df_zz, by = "business_name")

head(new_frame)
new_frame$num_yrs_old <- as.character(new_frame$num_yrs_old)

head(new_frame)
new_frame$num_yrs_old <- as.character(new_frame$num_yrs_old)

dataf <- dummy_cols(new_frame, select_columns = c('Year', 'num_yrs_old','Weekend'))

model = lm(inspection_score~NumberofLocations + Year_2000+Year_2001+Year_2002+Year_2003+Year_2004+Year_2005+Year_2006+Year_2007+Year_2008+Year_2009+Year_2015+Year_2016+
             Year_2017+Year_2018+Year_2019+num_yrs_old_0+num_yrs_old_1+num_yrs_old_2+num_yrs_old_3+num_yrs_old_4+num_yrs_old_6+num_yrs_old_7+num_yrs_old_8+num_yrs_old_9+num_yrs_old_10+num_yrs_old_11+
             num_yrs_old_12+num_yrs_old_13+num_yrs_old_14+num_yrs_old_15+num_yrs_old_16+num_yrs_old_17+num_yrs_old_18+num_yrs_old_19+Weekend_FALSE+Weekend_TRUE
           , data = dataf) 
#Create a linear regression with multiple variables. 
summary(model)

new_frame$num_yrs_old <- as.numeric(new_frame$num_yrs_old)
model_2 = lm(inspection_score~NumberofLocations+num_yrs_old, data = new_frame) #Create a linear regression with two variables
summary(model_2) #Review the result

cor(df$inspection_score,df$NumberofLocations)

install.packages("ggplot2")
library(ggplot2)

counts <- table(df$inspection_score,df$Weekend)
barplot(counts, main="Inspection_score_over_the_weekdays and weekends",
        xlab="Weekend", col="black",
        legend = rownames(counts),cex.names=0.8, beside=FALSE)

x <- dataf$inspection_score 
y <- dataf$NumberofLocations 
plt <- ggplot(dataf, aes(x=x,y=y)) + geom_point(color="black")+theme_bw()
plt

plt2 <- plt + geom_smooth(method = lm, color="red",se=FALSE)
plt2

plt3 <- plt + geom_smooth(method = lm , color = "red", fill="green",se= TRUE)
plt3













