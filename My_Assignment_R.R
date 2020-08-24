# Student assignment
# Alexander Shemetev
#### My assignment on the PS on the reproducible research
######################################################################################################################
# Part I. The Main Analysis
######################################################################################################################

##################################
#Initialize
##################################
#packages (might need to install some) - THIS FUNCTION INSTALLS ALL MAIN PACKAGES IF YOU DID NOT INSTALL SOME, and LOAD LIBRARIES THAT ARE INTALLED YET OR WILL BE INSTALLED AUTOMATICALLY BY THIS FUNCTION IN THIS CELL
install_and_load = function(name, char = T)
{
  if (!require(name, character.only = char)) 
  {
    install.packages(name)
  }
  require(name, character.only = char)
}

 #install_github("dgrtwo/broom")
# library(broom)
sapply(
  c("data.table","tidyverse","magrittr", "modelr",
    "arules","arulesViz","readxl","dplyr", "ggplot2", "haven", "expss", "ipumsr", "devtools", "broom",
    "purrr", "GGally", "cluster", "readxl", "tidyr", "writexl", "xlsx", "openxlsx", "psych", 'data.table',
    "readxl", "knitr", "ExPanDaR", "kableExtra", "plm", "sampleSelection", "nnet", "reshape2", "data.table","dplyr","magrittr",
	"lubridate", "scales","norm","RPostgreSQL","tidyr","gganimate","gifski","stringr","tm","readr","textclean","grid","ggpubr","gridExtra","lubridate","writexl","xlsx","RSQLite","odbc","dbplyr","seasonal","plotly","forecast","mFilter","tseries","plm","AER","stargazer","merTools","RColorBrewer","colorRamps"),
  install_and_load
)
rm(install_and_load)

### Urgently needed packages for our work 
#library(data.table)
#library(ggplot2)
#library(lubridate)
#library(dplyr)
#library(purrr)
#require(norm)
##################################
### Load the data (i.e. \color{red}{\verb|read.csv()|}read.csv())
##################################
my_path_to_file = "C:\\Users\\Alex\\Documents\\COURSERA STUDIES\\Reproducible Research JHI\\Week_2 PS1\\"
dir(my_path_to_file) #To see if there is the file we need and to check the name of the file 
data_on_activities <- read.csv(unz(paste(my_path_to_file, "repdata_data_activity.zip", sep=""),"activity.csv"), header=TRUE, na.strings="NA", sep=",", quote="\"")
# Checking if the file opened OK:
str(data_on_activities) # OK - file is opened OK 

##################################
### Process/transform the data (if necessary) into a format suitable for your analysis
##################################
typeof(data_on_activities)
data_on_activities = data.table(data_on_activities)
class(data_on_activities) #Data table data frame - so it is OK for the future analysis 
## Datet time transformation   $ date    : chr  "2012-10-01" "2012-10-01" 
# EPL2011_12$Date2 <- as.Date( as.character(EPL2011_12$Date), "%d-%m-%y")
library(lubridate) 
data_on_activities$date1 <- as.Date( as.character(data_on_activities$date), "%Y-%m-%d")
data_suggested_for_analysis_1 <- data_on_activities[!is.na(data_on_activities$steps),]
##################################
### Calculate the total number of steps taken per day
##################################
total_steps_by_date <- aggregate(data=data_suggested_for_analysis_1,steps~date1,sum) #Done in this line 
# View(total_steps_by_date) #We have a small dataset - so it is OK to view it 
head(total_steps_by_date, 32)

##################################
### If you do not understand the difference between a histogram and a barplot, research the difference between them. 
### Make a histogram of the total number of steps taken each day
##################################
hist(total_steps_by_date$steps, 
     main="Total Steps Done Per Day", 
     xlab="Total Steps Taken On A Daily Basis", 
     ylab="Number of Days We Saw This Pattern", 
     col="red",
     ylim=range(0:45)) #Cut the OY axis till 45 
##################################
### Calculate and report the mean and median of the total number of steps taken per day
##################################
### Method 1:
summary(total_steps_by_date) # Done:  Mean and median will be in this output 
### Method 2:
require(Hmisc)
Hmisc::describe(total_steps_by_date) #More detailed statistics 
### Method 3:
mn1 <- round(mean(total_steps_by_date$steps),digits=2)
md1 <- round(median(total_steps_by_date$steps),digits=2)
mn1
md1

##################################
### Calculate and report the mean and median of the total number of steps taken per day
##################################
average_steps_by_interval <- aggregate(data=data_suggested_for_analysis_1,steps~interval,mean)
ggplot(average_steps_by_interval, aes(x=interval, y=steps)) +
  geom_line(color="green") +
  xlab("Interval") +
  ylab("Steps Taken") +
  ggtitle("Average Steps Taken By Interval")

##################################
### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
##################################
### Method 1: 
five_min_interval_Avg_all_days_maximum_N_steps <- average_steps_by_interval[average_steps_by_interval$steps==max(average_steps_by_interval$steps), ]
View(five_min_interval_Avg_all_days_maximum_N_steps) #Second column 
summary(five_min_interval_Avg_all_days_maximum_N_steps) #Or all numbers here with the name Interval 
### Method 2: 
summary(average_steps_by_interval) #We see Max.   :206.170
which(average_steps_by_interval$steps > 206.169) # See the result 104 
average_steps_by_interval$interval[104] #We see the answer 
### Method 3: 
which(average_steps_by_interval$steps == max(average_steps_by_interval$steps)) #Returns 104 
average_steps_by_interval$interval[104] #We see the answer 
### Method 4:
v1 = table(average_steps_by_interval$steps == max(average_steps_by_interval$steps), average_steps_by_interval$interval)
v1 = data.table(v1) #Otherwise the next step will be impossible
v2 = v1[v1$V1 == TRUE & v1$N > 0, ] #V1 - T/F variable if maximum => we need it is to be TRUE; N - number of observations where the Maximum value is True (need to be more than 0 - at least once Max value should appear)
str(v2) #We see the Answer: 835 
######################################################################################################################
# Part II. Imputing missing values
######################################################################################################################

##################################
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
##################################
### Method I
summary(data_on_activities) #The easiest one and the most obvious 
# We see the number of NAs in each solumn 
### Method II 
my_NA_data <- data_on_activities[is.na(data_on_activities$steps),]
my_NA_data <- count(my_NA_data)
View(my_NA_data)
length(my_NA_data) #Check the Number of columns
dim(my_NA_data) #Check all NA data Number of observations
### Method III 
purrr::map(data_on_activities, ~sum(is.na(.)))
### Method IV
data_on_activities %>%
  dplyr::summarise_all(dplyr::funs(sum(is.na(.))))
### Method V
sapply(X = data_on_activities, FUN = function(x) sum(is.na(x)))
### Method VI
apply(data_on_activities, 2, function(x) length(which(is.na(x)))) #Longer in computation than the previous one 
### Method VII
apply(is.na(data_on_activities), 2, sum)
### Method VIII
colSums(is.na(data_on_activities))
##################################
### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
### For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
### Create a new dataset that is equal to the original dataset but with the missing data filled in.
##################################
### Method I - Just replacing with averages 
remade_to_df_data_on_activities <- dplyr::tbl_df(data_on_activities)
# average_steps_by_interval <- aggregate(data=data_suggested_for_analysis_1,steps~interval,mean)
remade_to_df_data_on_activities <- inner_join(remade_to_df_data_on_activities, average_steps_by_interval, by = "interval")
# names(remade_to_df_data_on_activities) <- c("steps","date","interval","avg", "date1")
remade_to_df_data_on_activities$steps <- ifelse(is.na(remade_to_df_data_on_activities$steps.x), remade_to_df_data_on_activities$steps.y, remade_to_df_data_on_activities$steps.x)
# remade_to_df_data_on_activities <- remade_to_df_data_on_activities[1:3] #Only preserves 3 columns 
### Method II - Just replacing with zeroes - also a common method 
data_on_activities_2 <- dplyr::tbl_df(data_on_activities)
# d[is.na(d)] <- 0
data_on_activities_2[is.na(data_on_activities_2)] <-0
# View(data_on_activities_2)
head(data_on_activities_2, 12)
rm(data_on_activities_2) #Removing from the memory what we are not going to use at all
### Method III - Complex replacing with the most probable values 
data_on_activities_2 <- dplyr::tbl_df(data_on_activities)
data_on_activities_2$date = NULL #we have date1 column
data_on_activities_2 <- data.table(data_on_activities_2)
# View(data_on_activities_2)
class(data_on_activities_2)
data_on_activities_2$date = data_on_activities_2$date1
data_on_activities_2$steps1 = as.numeric(data_on_activities_2$steps)
data_on_activities_2$interval1 = as.numeric(data_on_activities_2$interval)
data_on_activities_2$date1 = NULL
data_on_activities_2$steps = NULL
data_on_activities_2$interval = NULL

str(data_on_activities_2)
data_on_activities_2 = as.matrix(data_on_activities_2) #Object must be a matrix for norm objects + all data should be numeric 
class(data_on_activities_2)

s <- norm::prelim.norm(data_on_activities_2)   #do preliminary manipulations
thetahat <- norm::em.norm(s)   #find the mle
norm::rngseed(2020)   #set random number generator seed
ximp <- norm::imp.norm(s,thetahat,data_on_activities_2)  #impute missing data under the MLE
# View(ximp) #Ok all NAs are replaced with the most probable expected values 
head(ximp, 33)
##################################
### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
### Do these values differ from the estimates from the first part of the assignment? 
### What is the impact of imputing missing data on the estimates of the total daily number of steps?
##################################
imputed_aggregated_data_steps_per_day <- aggregate(data=remade_to_df_data_on_activities,steps~date,sum)

hist(imputed_aggregated_data_steps_per_day$steps, 
     main="Total Steps Per Day On Imputed Data", 
     xlab="Total Steps Taken Per Day", 
     ylab="Number Of Days With This Pattern", 
     col="darkgreen",
     ylim=range(0:45))

summary(imputed_aggregated_data_steps_per_day$steps) #There is mean and median 
# Another way: 
mn_2 <- round(mean(imputed_aggregated_data_steps_per_day$steps),digits=2)
md_2 <- round(median(imputed_aggregated_data_steps_per_day$steps),digits=2)
mn_2
md_2
### Differences from the first part of the assignment: 
mn1
md1
diff_mean = mn1 - mn_2
diff_median = md1 - md_2
diff_mean #Mean is the same 
diff_median #Median is a bit different (-1.19) 

# Other ways for doing same are made above (for the non-imputed data)

######################################################################################################################
# Part II. Are there differences in activity patterns between weekdays and weekends?
######################################################################################################################

##################################
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
##################################
system("defaults write org.R-project.R force.LANG en_US.UTF-8") 
Sys.setlocale("LC_ALL","English")

remade_to_df_data_on_activities_3 <- data.frame(remade_to_df_data_on_activities, dow = weekdays(as.Date(remade_to_df_data_on_activities$date)))
remade_to_df_data_on_activities_3$dow <- as.factor(remade_to_df_data_on_activities_3$dow)
summary(remade_to_df_data_on_activities_3$dow)
remade_to_df_data_on_activities_3$dow_w_nd <- ifelse(remade_to_df_data_on_activities_3$dow %in% c("Sunday","Saturday"), "weekend","weekday")
### Other method: 
remade_to_df_data_on_activities_3 <- mutate(remade_to_df_data_on_activities_3, dow_cat = ifelse(remade_to_df_data_on_activities_3$dow %in% c("Sunday","Saturday"), "weekend","weekday"))
remade_to_df_data_on_activities_3$dow_cat <- as.factor(remade_to_df_data_on_activities_3$dow_cat)
remade_to_df_data_on_activities_3$interval <- as.factor(remade_to_df_data_on_activities_3$interval)

weekend_vs_weekdays_steps_intevals <- aggregate(data=remade_to_df_data_on_activities_3,steps~dow_cat+interval,mean)
weekend_vs_weekdays_steps_intevals$interval <- as.character(weekend_vs_weekdays_steps_intevals$interval)

xyplot(steps~interval|factor(dow_cat),
       data = weekend_vs_weekdays_steps_intevals,
       type='l',
       layout=c(1,2),
       xlab='Interval',
       ylab='Number of Steps',
	   # OX = from -100 to 2500; bin size 500 (step on the chart)
       xlim=seq(-100,2500,500))






