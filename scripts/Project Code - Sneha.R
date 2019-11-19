# ----------Step 1: Import all the required libraries ----------
  # We will add to this as and when required
  library(dplyr)
  library(data.table)
  # install.packages("corrplot") --Plot correlation plot
  library(corrplot)
  # install.packages("lattice")
  library(lattice)
  library(ggplot2)
  # install.packages("GGally")
  library("GGally")
  #install.packages("ggExtra")
  library(ggExtra)
  # install.packages("randomForest")
  library(randomForest)
  library(Metrics)
# ------------------- Step 1 ends here --------------------------


# ------------------- Step 2: Gather the data -----------------

  ### Data is provided as .csv file and already split into Test and Train.
  ### The training set is comprised of the first 19 days of each month, while the test set is the 20th to the end of the month.
  ### Let's import the data
    bike= read.csv("/Users/snehashrungarpawar/Documents/Master in Data Science/DPA/Project/Data/train.csv", header=TRUE)
    bike_test = read.csv("/Users/snehashrungarpawar/Documents/Master in Data Science/DPA/Project/Data/test.csv", header=TRUE)

    head(bike, 10)
# ------------------- Step 2 ends here --------------------------



# ------------------- Step 3: Data Cleaning ---------------------
  # 3a. Analyze Attributes: Check properties of data
      dim(bike)
      str(bike)

  # 3b. Complete Data Perform missing value analysis and Impute if needed
      table(is.na(bike))

  # 3c. Correct Data: Check for any invalid data points


  # 3d. Create Derived Attributes - Feature Extraction
      
      bike$date=as.Date(substr(bike$datetime,1,10))
      bike$year = as.factor(year(bike$datetime))
      bike$month = as.factor(month(bike$datetime))
      bike$hour = as.factor(hour(bike$datetime))
      bike$wkday = as.factor(wday(bike$datetime))
      
      bike_test$date=as.Date(substr(bike_test$datetime,1,10))
      bike_test$year = as.factor(year(bike_test$datetime))
      bike_test$month = as.factor(month(bike_test$datetime))
      bike_test$hour = as.factor(hour(bike_test$datetime))
      bike_test$wkday = as.factor(wday(bike_test$datetime))

  # 3e. Convert - Converting data to proper formats
      names = c("season", "holiday", "workingday", "weather")
      bike[,names] = lapply(bike[,names], factor)
      bike_test[,names] = lapply(bike_test[,names], factor)
      


# ------------------- Step 3 ends here --------------------------

# ------------- Step 4: Exploratory Data Analysis -----------

  # 4a. Outlier Analysis

    # 4a(1). Visualize continuos variables wrt target variable

    # 4a(2). Visualize categorical variables wrt target variable

  # 4b. Correlation Analysis

    # 4b(1). Explore continous features

      # i. Check distribution of target variable
         boxplot(bike$count) 
         plot(bike$count)
      # Conclusion: Target variable "count" is almost normally distributed.

      # ii. Explore correlation between independent continuous variables with target variable
        bike_subset_df = tibble(bike$temp, bike$atemp, bike$humidity, bike$windspeed, bike$count)
        df = melt(bike_subset_df, id=c("id","time"))
      
        boxplot(bike$temp,bike$count)
        boxplot(bike$atemp,bike$count)
        boxplot(bike$windspeed,bike$count)
        boxplot(bike$humidity,bike$count)
    
      # iii. Plot heatmap for correlation matrix (to check for multicolinearity)
        corr <- as.data.frame(lapply(bike[c(6:12)], as.numeric))
        corrplot(cor(corr), method = "color", type='lower')
      
      ### Conclusion: temp and atemp are highly correlated, we would need to drop one of them to remove multicolinearity.
      ### We can also drop Registered and Casual from our analysis as Counts are categorized as Registered and Casual and we will be predicting total counts "Count" variable only.
        

      # iv. Visualize the relationship among all continuous variables using pairplots
        ggpairs(bike[6:8], lower=list(continuous=wrap("smooth", colour="orange")) )
      

      # v. Explore relationship between independent continuous variables and dependent variables using Joint Plot
        # 1. temp vs Count
        plot_center = ggplot(bike, aes(x=temp,y=count)) + geom_point(colour="blue") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="blue")
        # 2. atemp vs Count
        plot_center = ggplot(bike, aes(x=atemp,y=count)) + geom_point(colour="red") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="red")
        # 3. humidity vs Count
        plot_center = ggplot(bike, aes(x=humidity,y=count)) + geom_point(colour="green") + geom_smooth(method="lm") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="green")
        # 3. windspeed vs Count
        plot_center = ggplot(bike, aes(x=windspeed,y=count)) + geom_point(colour="orange") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="orange")
      
      ### Conclusion: atemp and temp has good correlation with count. Humidity has low correlation with count.
        
    # 4b(2) Explore categorical features

      # i. Check distribution of categorical variables

      # ii. Check how individual categorical features affects the target variable

      # iii. Explore trends over time

    # 4c. Drop some variables from the dataset based on the analysis so far 
      # drop temp, registered, count and date
      bike_subset = bike[-c(1,6,10:11,13)]
        
# 5. Build Model   
      
      # 5a. Split data into Test and Train  
      sample_size = floor(0.8 * nrow(bike_subset))
      set.seed(1)
      train_index = sample(nrow(bike_subset), size = sample_size)
      train <- bike_subset[train_index, ]
      test <- bike_subset[-train_index, ]

      # 5b. Linear Regression
        # Fit Linear Model
        lm_fit = lm(count ~ ., data = train)
        summary(lm_fit)
        
        # Choosing the best model by AIC in a Stepwise Algorithm
        # The step() function iteratively removes insignificant features from the model.
        step(lm_fit)
        summary(lm_fit)
        
        # Calculate RMSLE
        y_act <- abs(test$count)
        y_pred <- abs(predict(lm_fit, test))
        lm_RMSLE = rmsle(y_act, y_pred)
        
        # Save the results
        lm_results = predict(lm_fit, bike_test)
        hist(lm_results)
        
        
      # 5b. Random Forest
        rf_fit = randomForest(x = train[,-which(names(train)=="count")], y = train$count)
        rf_results = predict(rf_fit, test)
        rf_RMSLE = rmsle(abs(test$count), abs(rf_results))
        
        # Save the results
        rf_results = predict(rf_fit, bike_test)
        hist(bike_train$count)
        hist(rf_results)
        
        Ntree=500
        Mtry = 5
        myImportance = TRUE
        
        set.seed(1)
        rf_train <- subset(bike, select = -c(datetime, count, registered))
        testFit <- randomForest(casual ~ ., data=rf_train, ntree=Ntree, mtry=Mtry,
                                importance=myImportance)
        
        rf_train2 <- subset(bike, select = -c(datetime, count, casual))
        testFit2 <- randomForest(registered ~ ., data=rf_train2, ntree=Ntree, mtry=Mtry,
                                importance=myImportance)
        

   
        
        
        
        
        
