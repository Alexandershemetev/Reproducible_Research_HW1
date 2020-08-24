---
title: "reproducible_research_hw"
author: "Alexander Shemetev"
date: "24 8 2020"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

######################################################################################################################
# Part I. The Main Analysis
######################################################################################################################

##################################
#Initialize
##################################

Install all the potentially needed packages:

```{r package_installation, echo=TRUE, message=FALSE}
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
```

##################################
### Load the data (i.e. read.csv())
##################################

You can do this, for example:

```{r open_file_csv, echo=TRUE}
my_path_to_file = "C:\\Users\\Alex\\Documents\\COURSERA STUDIES\\Reproducible Research JHI\\Week_2 PS1\\"
dir(my_path_to_file) #To see if there is the file we need and to check the name of the file 
data_on_activities <- read.csv(unz(paste(my_path_to_file, "repdata_data_activity.zip", sep=""),"activity.csv"), header=TRUE, na.strings="NA", sep=",", quote="\"")
# Checking if the file opened OK:
str(data_on_activities) # OK - file is opened OK 

```

##################################
### Process/transform the data (if necessary) into a format suitable for your analysis
##################################

```{r process_my_data, echo=TRUE}

typeof(data_on_activities)
data_on_activities = data.table(data_on_activities)
class(data_on_activities) #Data table data frame - so it is OK for the future analysis 
## Datet time transformation   $ date    : chr  "2012-10-01" "2012-10-01" 
# EPL2011_12$Date2 <- as.Date( as.character(EPL2011_12$Date), "%d-%m-%y")
# library(lubridate) 
data_on_activities$date1 <- as.Date( as.character(data_on_activities$date), "%Y-%m-%d")
data_suggested_for_analysis_1 <- data_on_activities[!is.na(data_on_activities$steps),]

```

##################################
### Calculate the total number of steps taken per day
##################################

```{r steps_per_day, echo=TRUE}

total_steps_by_date <- aggregate(data=data_suggested_for_analysis_1,steps~date1,sum) #Done in this line 
# View(total_steps_by_date) #We have a small dataset - so it is OK to view it 
knitr::kable(head(total_steps_by_date, 10))

```

##################################
### If you do not understand the difference between a histogram and a barplot, research the difference between them. 
### Make a histogram of the total number of steps taken each day
##################################

```{r histogram_1, echo=TRUE}

hist1 = hist(total_steps_by_date$steps, 
     main="Total Steps Done Per Day", 
     xlab="Total Steps Taken On A Daily Basis", 
     ylab="Number of Days We Saw This Pattern", 
     col="red",
     ylim=range(0:45)) #Cut the OY axis till 45 
hist1
# figure/ directory

png("C:\\Users\\Alex\\Documents\\R\\HOMEPROJECTS\\Reproducible_Research_1\\figure\\directory\\figure_1.png", width=480, height=480)
hist(total_steps_by_date$steps, 
     main="Total Steps Done Per Day", 
     xlab="Total Steps Taken On A Daily Basis", 
     ylab="Number of Days We Saw This Pattern", 
     col="red",
     ylim=range(0:45)) #Cut the OY axis till 45 
dev.off()

```

##################################
### Calculate and report the mean and median of the total number of steps taken per day
##################################

```{r mean_median_1, echo=TRUE}

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

```

**The MEAN steps per day is: `r as.character(mn1)[1]`** 

**The MEDIAN steps per day is: `r as.character(md1)[1]`** 

##################################
### Calculate and report the mean and median of the total number of steps taken per day
##################################

```{r mean_median_plot_1, echo=TRUE}

average_steps_by_interval <- aggregate(data=data_suggested_for_analysis_1,steps~interval,mean)
myplot1 = ggplot(average_steps_by_interval, aes(x=interval, y=steps)) +
  geom_line(color="green") +
  xlab("Interval") +
  ylab("Steps Taken") +
  ggtitle("Mean Steps Taken By Interval")
myplot1

png("C:\\Users\\Alex\\Documents\\R\\HOMEPROJECTS\\Reproducible_Research_1\\figure\\directory\\figure_2.png", width=480, height=480)
myplot1
dev.off()

```

```{r mean_median_plot_2, echo=TRUE}

average_steps_by_interval_1 <- aggregate(data=data_suggested_for_analysis_1,steps~interval,median)
myplot2 = ggplot(average_steps_by_interval_1, aes(x=interval, y=steps)) +
  geom_line(color="green") +
  xlab("Interval") +
  ylab("Steps Taken") +
  ggtitle("Median Steps Taken By Interval")
myplot2
png("C:\\Users\\Alex\\Documents\\R\\HOMEPROJECTS\\Reproducible_Research_1\\figure\\directory\\figure_3.png", width=480, height=480)
myplot2
dev.off()

```

##################################
### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
##################################

```{r max_N_steps_1, echo=TRUE}

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
rm(v1) #v2 is small - it doesn't hold any significant place in the PC memory
```


######################################################################################################################
# Part II. Imputing missing values
######################################################################################################################

##################################
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
##################################

```{r processing_missing_values, echo=TRUE}

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

```

##################################
### Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. 
### For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
### Create a new dataset that is equal to the original dataset but with the missing data filled in.
##################################

```{r strategies_for_dealing_with_NAs, echo=TRUE}

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

```

##################################
### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
### Do these values differ from the estimates from the first part of the assignment? 
### What is the impact of imputing missing data on the estimates of the total daily number of steps?
##################################

```{r different_stuff_1, echo=TRUE}
remade_to_df_data_on_activities <- dplyr::tbl_df(data_on_activities)
# average_steps_by_interval <- aggregate(data=data_suggested_for_analysis_1,steps~interval,mean)
remade_to_df_data_on_activities <- inner_join(remade_to_df_data_on_activities, average_steps_by_interval, by = "interval")
# names(remade_to_df_data_on_activities) <- c("steps","date","interval","avg", "date1")
remade_to_df_data_on_activities$steps <- ifelse(is.na(remade_to_df_data_on_activities$steps.x), remade_to_df_data_on_activities$steps.y, remade_to_df_data_on_activities$steps.x)

imputed_aggregated_data_steps_per_day <- aggregate(data=remade_to_df_data_on_activities,steps~date,sum)

hist2 = hist(imputed_aggregated_data_steps_per_day$steps, 
     main="Total Steps Per Day On Imputed Data", 
     xlab="Total Steps Taken Per Day", 
     ylab="Number Of Days With This Pattern", 
     col="darkgreen",
     ylim=range(0:45))
hist2

png("C:\\Users\\Alex\\Documents\\R\\HOMEPROJECTS\\Reproducible_Research_1\\figure\\directory\\figure_4.png", width=480, height=480)
hist(imputed_aggregated_data_steps_per_day$steps, 
     main="Total Steps Per Day On Imputed Data", 
     xlab="Total Steps Taken Per Day", 
     ylab="Number Of Days With This Pattern", 
     col="darkgreen",
     ylim=range(0:45))
dev.off()

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

```

**The difference in the MEAN steps per day with imputed data is: `r as.character(diff_mean)`** **The difference in the MEDIAN steps per day with imputed data is: `r as.character(diff_median)`** 

######################################################################################################################
# Part II. Are there differences in activity patterns between weekdays and weekends?
######################################################################################################################

##################################
### Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
##################################

```{r missing_values_2, echo=TRUE}

system("defaults write org.R-project.R force.LANG en_US.UTF-8") 
Sys.setlocale("LC_ALL","English") #to be sure the names in the DS will be in English

remade_to_df_data_on_activities_3 <- data.frame(remade_to_df_data_on_activities, dow = weekdays(as.Date(remade_to_df_data_on_activities$date)))
remade_to_df_data_on_activities_3$dow <- as.factor(remade_to_df_data_on_activities_3$dow)
summary(remade_to_df_data_on_activities_3$dow)
remade_to_df_data_on_activities_3$dow_w_nd <- ifelse(remade_to_df_data_on_activities_3$dow %in% c("Sunday","Saturday"), "weekend","weekday")
### Other method: 
remade_to_df_data_on_activities_3 <- mutate(remade_to_df_data_on_activities_3, dow_cat = ifelse(remade_to_df_data_on_activities_3$dow %in% c("Sunday","Saturday"), "weekend","weekday"))
remade_to_df_data_on_activities_3$dow_cat <- as.factor(remade_to_df_data_on_activities_3$dow_cat)
remade_to_df_data_on_activities_3$interval <- as.factor(remade_to_df_data_on_activities_3$interval)


```

##################################
### Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
##################################

```{r final_plot, echo=TRUE}

weekend_vs_weekdays_steps_intevals <- aggregate(data=remade_to_df_data_on_activities_3,steps~dow_cat+interval,mean)
weekend_vs_weekdays_steps_intevals$interval <- as.character(weekend_vs_weekdays_steps_intevals$interval)

final_plot = xyplot(steps~interval|factor(dow_cat),
       data = weekend_vs_weekdays_steps_intevals,
       type='l',
       layout=c(1,2),
       xlab='Interval',
       ylab='Number of Steps',
	   # OX = from -100 to 2500; bin size 500 (step on the chart)
       xlim=seq(-100,2500,500)) #The plot is the copy of the plot requested in the task
final_plot

png("C:\\Users\\Alex\\Documents\\R\\HOMEPROJECTS\\Reproducible_Research_1\\figure\\directory\\figure_5.png", width=480, height=480)
final_plot
dev.off()
```



Note that the `echo = TRUE` parameter was added to the code chunk to allow printing of the R code that generated the plot.
