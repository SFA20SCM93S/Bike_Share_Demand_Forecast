# ----------Step 1: Import all the required libraries ----------
# We will add to this as and when required
library(ggplot2)

# ------------------- Step 1 ends here --------------------------


# ------------------- Step 2: Gather the data -----------------

### Data is provided as .csv file and already split into Test and Train.
### The training set is comprised of the first 19 days of each month, while the test set is the 20th to the end of the month.
### Let's import the data
bike_train <- read.csv("C:/Users/siyuz/Desktop/CSP 571/Project/data/train.csv")
bike_test <- read.csv("C:/Users/siyuz/Desktop/CSP 571/Project/data/test.csv")
data2 <- read.csv("C:/Users/siyuz/Desktop/CSP 571/Project/data/data2.csv")
bike = bike_train #(for now)
head(bike, 10)

# ------------------- Step 2 ends here --------------------------



# ------------------- Step 3: Data Cleaning -----------------

    # 3b. Complete Data Perform missing value analysis and Impute if needed

  # Although we have already seen above that there are no null values in the dataset. 
  # Lets try other way to confirm
  # Checking nulls
      is.null(bike)
  # what we can infer:
  # ->There are no null values in the dataset.

    # 3c. Correct Data: Check for any invalid data points

    # 3d. Create Derived Attributes - Feature Extraction
      bike$year = as.factor(year(bike$datetime))
      bike$month = as.factor(month(bike$datetime))
      bike$hour = as.factor(hour(bike$datetime))
      bike$wkday = as.factor(wday(bike$datetime))

# ------------------- Step 3 ends here --------------------------

# ------------- Step 4: Exploratory Data Analysis -----------

    # 4b(2) Explore categorical features
   
      # i. Check distribution of categorical variables --> pie chart
        # simple pie -->> pie(table(bike$season), main="season")

ggplot(bike, aes(x=" ",fill=year))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "year")+theme_void()

ggplot(bike, aes(x=" ",fill=month))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "month")+theme_void()
bike$season = factor(bike$season)
ggplot(bike, aes(x=" ",fill=season))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "Season")+theme_void()
bike$holiday = factor(bike$holiday)
ggplot(bike, aes(x=" ",fill=holiday))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "holiday")+theme_void()

ggplot(bike, aes(x=" ",fill=wkday))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "weekday")+theme_void()
bike$workingday = factor(bike$workingday)
ggplot(bike, aes(x=" ",fill=workingday))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "workingday")+theme_void()
bike$weather = factor(bike$weather)
ggplot(bike, aes(x=" ",fill=weather))+ geom_bar(width = 1)+ coord_polar("y")+labs(title = "weather")+theme_void()

      # ii. Check how individual categorical features affects the target variable

ggplot(bike, aes(x=season, y=count, fill=year)) + 
  stat_summary(
    fun.y=median, 
    geom='bar', 
    position=position_dodge(),
    ) + labs(title="Histogram for Seasons") +  labs(x="Season", y="Count")

ggplot(bike, aes(x=year, y=count, fill=year)) + 
  stat_summary(
    fun.y=median, 
    geom='bar', 
    position=position_dodge(),
  ) + 
  labs(title="Histogram for year") +  labs(x="year", y="Count")

ggplot(bike, aes(x=month, y=count, fill=month)) + 
  stat_summary(
    fun.y=median, 
    geom='bar', 
    position=position_dodge(),
  ) + 
  labs(title="Histogram for month") +  labs(x="month", y="Count")

ggplot(bike, aes(x=holiday, y=count, fill=holiday)) + 
  stat_summary(
    fun.y=median, 
    geom='bar', 
    position=position_dodge(),
  ) +   labs(title="Histogram for holiday") +labs(x="holiday", y="Count")

ggplot(bike, aes(x=wkday, y=count, fill=wkday)) + 
  stat_summary(
    fun.y=median, 
    geom='bar', 
    position=position_dodge(),
  ) +    labs(title="Histogram for weekday") +labs(x="weekday", y="Count")

ggplot(bike, aes(x=workingday, y=count, fill=workingday)) + 
  stat_summary(
    fun.y=median, 
    geom='bar', 
    position=position_dodge(),
  ) +    labs(title="Histogram for working day") +labs(x="working day", y="Count")

ggplot(bike, aes(x=weather, y=count, fill=weather)) + 
  stat_summary(
    fun.y=median, 
    geom='bar', 
    position=position_dodge(),
  ) +  labs(title="Histogram for weather") +labs(x="weather", y="Count")

      # iii. Explore trends over time ---- exploring some more pairplots

ggplot(bike, aes(x=season, y=count, group=year, color=year)) + 
  stat_summary(
    fun.y=mean, 
    geom='line'
  ) + 
  stat_summary(
    fun.y=mean, 
    geom='point'
  ) + 
  labs(title="Average Count by Month Across Season") +
  labs(x="Season", y="Count")


ggplot(bike, aes(x=bike$hour, y=count, group=season, color=season)) + 
  stat_summary(
    fun.y=mean, 
    geom='line'
  ) + 
  stat_summary(
    fun.y=mean, 
    geom='point'
  )+ 
  labs(title="Average Count By Hour Of The Day Across Season") +
  labs(x="Hour of the Day", y="Count")

ggplot(bike, aes(x=bike$hour, y=count, group=wkday, color=wkday)) + 
  stat_summary(
    fun.y=mean, 
    geom='line'
  ) + 
  stat_summary(
    fun.y=mean, 
    geom='point'
  )+ 
  labs(title="Average Count By Hour Of The Day Across Weekdays") +
  labs(x="Hour of the Day", y="Count")

