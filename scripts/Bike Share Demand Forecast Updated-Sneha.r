
# ----------- Step 1a: Define and categorize the problem statement --------------

# The problem statement is to "Predict the daily bike rental count based on the environmental and seasonal settings"
# This is clearly a 'Supervised machine learning regression problem' to predict a number based on the input features.

# ----------- Step 1a ends here ----------------- 


# ----------Step 1b: Import all the required libraries ----------

#---- for data transformations----
    #install.packages("lubridate")
    library(lubridate)

#---- for EDA Visualizations ------
    #install.packages("corrplot")
    library(corrplot)
    #install.packages("ggplot2")
    library(ggplot2)
    #install.packages("GGally")
    library("GGally")
    #install.packages("ggExtra")
    library(ggExtra)

#---- for model building----
    library(caret)
    #install.packages("Metrics")
    library(Metrics)
    #install.packages("randomForest")
    library(randomForest)

# ------------------- Step 1b ends here --------------------------



# ------------------- Step 2: Gather the data -----------------

  # Data is provided as .csv file and already split into Test and Train.
  # The training set is comprised of the first 19 days of each month, while the test set is the 20th to the end of the month.
  # Let's import the data
    bike= read.csv("/Users/snehashrungarpawar/Documents/Master in Data Science/DPA/Project/Data/train.csv", header=TRUE)
    bike_test = read.csv("/Users/snehashrungarpawar/Documents/Master in Data Science/DPA/Project/Data/test.csv", header=TRUE)
# ------------------- Step 2 ends here --------------------------


# ------------------- Step 3: Data Preparation ---------------------
  # 3a. Analyze Attributes: Check properties of data
  # 3b. Complete Data Perform missing value analysis and Impute if needed
  # 3c. Correct Data: Check for any invalid data points
  # 3d. Create Derived Attributes - Feature Extraction
  # 3e. Convert - Converting data to proper formats


  # 3a. Analyze Attributes: Check properties of data
      dim(bike)
      str(bike)
      head(bike, 10)
  # 3a -> Inference: 
        #i. The dataset has 10,886 observations (n=10886) and 12 columns of type int, num and factor.
        #ii. Season, Holiday, Working day and weather are categorical variables.
        #ii. temp, atemp, humidity, windspeed, casual, registered and count are continuous numerical variables.


  # 3b. Complete Data Perform missing value analysis and Impute if needed
      table(is.na(bike))
  # 3b -> Inference: There are no null values in the dataset. If it had, then either the rows/columns had to be
    # dropped or the null values be imputed based on the % of null values


  # 3c. Correct Data: Check for any invalid data points
    # From above observations data doesnot seem to have any invalid datatypes to be handled.
    # Let's check for the outliers in EDA step


  # 3d. Create Derived Attributes - Feature Extraction
      # Lets extract 'date','month','weekday' and 'year' from 'datetime' column as we will be needing it for analysis
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

      # Drop datetime as we have extracted all the above needed information from it
      bike = bike[-c(1)]
      bike_test = bike_test[-c(1)]

      head(bike, 5)
      head(bike_test, 5)

  # 3d -> Inference: There are no null values in the dataset. If it had, then either the rows/columns had to be 
                    #dropped or the null values be imputed based on the % of null values.


  # 3e. Convert - Converting data to proper formats
    # We can clearly see that "season", "yr","mnth","holiday","weekday","workingday","weather","date" are categories,rather than continous variable.
    # Let's convert them to categories
      names = c("season", "holiday", "workingday", "weather")
      bike[,names] = lapply(bike[,names], factor)
      bike_test[,names] = lapply(bike_test[,names], factor)

      str(bike) 
      str(bike_test)

# ------------------- Step 3: Data Preparation ends here --------------------------



# ------------- Step 4: Exploratory Data Analysis -----------
    # 4a. Outlier Analysis

    # 4a(1). Visualize continuos variables wrt target variable

    # 4a(2). Visualize categorical variables wrt target variable


# 4b. Correlation Analysis

# --------------- Explore Continuous Variables----------------
    # 4b(1). Explore continous features
        # i. Check distribution of target variable
        # ii. Explore correlation between independent continuous variables with target variable
        # iii. Plot heatmap for correlation matrix (to check for multicolinearity)
        # iv. Visualize the relationship among all continuous variables using pairplots
        # v. Explore relationship between independent continuous variables and dependent variables using Joint Plot


   # 4b(1) i. Check distribution of target variable
          hist(bike$count) 
          plot(bike$count)
   # Inference: Target variable "count" is almost normally distributed.


    # 4b(1) ii. Explore correlation between independent continuous variables with target variable      
        plot(bike$temp,bike$count)
        plot(bike$atemp,bike$count)
        plot(bike$windspeed,bike$count)
        plot(bike$humidity,bike$count)


    # 4b(1) iii. Plot heatmap for correlation matrix (to check for multicolinearity)
        corr <- as.data.frame(lapply(bike[c(6:12)], as.numeric))
        corrplot(cor(corr), method = "color", type='lower')
    # Inference: 
        # i. temp and atemp are highly correlated, we would need to drop one of them to remove multicolinearity.
        # ii. We can also drop Registered and Casual from our analysis as Counts are categorized as Registered and Casual 
             # and we will be predicting "Count" variable only.


    # 4b(1) iv. Visualize the relationship among all continuous variables using pairplots
        ggpairs(bike[c(5:8, 11)], lower=list(continuous=wrap("smooth", colour="orange")) )


    # 4b(1) v. Explore relationship between independent continuous variables and dependent variables using Joint Plot       
        
        # 1. temp vs Count
        plot_center = ggplot(bike, aes(x=temp,y=count)) + geom_point(colour="blue") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="blue")
        # Inference: temp has good correlation with count.


        # 4b(1).v.2. atemp vs Count
        plot_center = ggplot(bike, aes(x=atemp,y=count)) + geom_point(colour="red") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="red")
        # Inference: atemp has good correlation with count.


        # 4b(1).v.3. humidity vs Count
        plot_center = ggplot(bike, aes(x=humidity,y=count)) + geom_point(colour="green") + geom_smooth(method="lm") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="green")
        # Inference: Humidity has low correlation with count.


        # 4b(1).v.4. windspeed vs Count
        plot_center = ggplot(bike, aes(x=windspeed,y=count)) + geom_point(colour="orange") + geom_smooth(method="lm", colour="grey")
        ggMarginal(plot_center, type="densigram", colour="orange")


    # 4b(1) Inferences Summary - Analysis of continous variables
        # 1. Target variable 'count' is almost normally distributed.
        # 2. From correlation with dependent variable "count", we can see that 'casual','registered' are very 
             # highly correlated to cnt. Needs to be dropped from the dataset.
        # 3. 'humidity' has low correlation with 'count'. For now, lets keep it.
        # 4. atemp and temp has good correlation with 'count'
        # 5. From heatmap, we can see that atemp and temp are highly correlated. So we need to drop 1 to remove multicollinearity.
        # 6. Since, as seen from jointplot, p(atemp) < p(temp), we can drop 'temp' and retain 'atemp' in the dataset.


# --------------- Explore Catogorical Variables----------------
    # 4b(2) Explore categorical features
          # i. Check distribution of categorical variables
          # ii. Check how individual categorical features affects the target variable
          # iii. Explore trends over time


# 4c. Drop some variables from the dataset based on the analysis so far 
        # drop temp, casual, registered and date
        bike_subset = bike[-c(5,9:10, 12)] 
        head(bike_subset,5)


#------ Step 4: Exploratory Data Analysis ENDS Here------------------
# Final observations:
#1.) 'casual' and 'registered' needs to be dropped from the dataset
#2.) 'atemp' and 'temp' are very strongly correlated . Drop 'atemp' from the dataset (since it has higher p-value 
        #than 'temp')
#3.) 'date' does not seem to have any affect on count of bikes, it can be dropped from the dataset
#------------------------------------------------------------


#----------Part 5 : Model Builing starts here ----------------------
    # 5a. Split data into test and train set
    # 5b. Linear Regression
    # 5c. Random Forest
    # 5d. Gradient Boosting


    # 5a. Split data into test and train set
        sample_size = floor(0.8 * nrow(bike))
        set.seed(1)
        train_index = sample(nrow(bike), size = sample_size)
        train <- bike[train_index, ]
        test <- bike[-train_index, ]    


    # 5b. Linear Regression
        # Fit Linear Model
        train_subset = train[-c(5,9:10, 12)]
        test_subset = test[-c(5,9:10, 12)]

        lm_fit = lm(count ~ ., data = train_subset)
        summary(lm_fit)
        
        # Choosing the best model by AIC in a Stepwise Algorithm
        # The step() function iteratively removes insignificant features from the model.
        step(lm_fit)
        summary(lm_fit)

        # Calculate Train RMSLE
        y_act_train <- abs(train_subset$count)
        y_pred_train <- abs(predict(lm_fit, train_subset))
        lm_train_RMSLE = rmsle(y_act_train, y_pred_train)
        
        # Calculate Test RMSLE
        y_act_test <- abs(test_subset$count)
        y_pred_test <- abs(predict(lm_fit, test_subset))
        lm_test_RMSLE = rmsle(y_act_test, y_pred_test)
        
        # Save the results
        lm_results = predict(lm_fit, bike_test)
        hist(lm_results)


    # 5b. Random Forest
        Ntree=500
        Mtry = 5
        myImportance = TRUE
        
        # Predict Casual Counts
        set.seed(1)
        CasualData <- subset(train, select = -c(count, registered, date))
        CasualFit <- randomForest(casual ~ ., data=CasualData, ntree=Ntree, mtry=Mtry,
                                importance=myImportance)
        

        # Predict Registered Counts
        RegisteredData <- subset(train, select = -c(count, casual, date))
        RegisteredFit <- randomForest(registered ~ ., data=RegisteredData, ntree=Ntree, mtry=Mtry,
                                importance=myImportance)

        varImpPlot(CasualFit) 
        varImpPlot(RegisteredFit)

    #Inference - Casual Fit: season, holiday, windspeed and weather are not much significant here.
    #Inference - Registered Fit: season, holiday, windspeed and temp are not much significant here.


        casualFitFinal <- randomForest(casual ~ hour + year + humidity + month + temp + atemp + workingday + wkday, 
                               data=CasualData, ntree=Ntree, mtry=Mtry,importance=myImportance)
        RegisteredFitFinal <- randomForest(registered ~ hour + year + month + weather + workingday + humidity + atemp 
                                        + wkday, data=RegisteredData, ntree=Ntree, mtry=Mtry,importance=myImportance)


        # Prediction on train data
        
            # Prediction on train data - casual users
            PredTrainCasual = round(predict(CasualFit, train),0)
            PredTrainCasualFinal = round(predict(casualFitFinal, train),0)

            # Prediction on train data - Registered users
            PredTrainRegistered = round(predict(RegisteredFit, train),0)
            PredTrainRegisteredFinal = round(predict(RegisteredFitFinal, train),0)
            
            # Sum up Casual and Registered to get Total Count
            PredTrainCount = PredTrainCasual+PredTrainRegistered
            PredTrainCountFinal = PredTrainCasualFinal+PredTrainRegisteredFinal

            # Calculate Train RMSLE
            rf_train_rmsle_full = rmsle(train$count, PredTrainCount)
            rf_train_rmsle2_reduced = rmsle(train$count, PredTrainCountFinal)
    

        # Prediction on test data
            # Prediction on test data - casual users
            PredTestCasual = round(predict(CasualFit, test),0)
            PredTestCasualFinal = round(predict(casualFitFinal, test),0)

            # Prediction on test data - registered users
            PredTestRegistered = round(predict(RegisteredFit, test),0)
            PredTestRegisteredFinal = round(predict(RegisteredFitFinal, test),0)

            # Sum up Casual and Registered to get Total Count
            PredTestCount = PredTestCasual+PredTestRegistered
            PredTestCountFinal = PredTestCasualFinal+PredTestRegisteredFinal

            # Calculate Train RMSLE
            rf_test_rmsle_full = rmsle(test$count, PredTestCount)
            rf_test_rmsle2_reduced = rmsle(test$count, PredTestCountFinal)



cat("Training RMSLE - Linear Regression: ", lm_train_RMSLE)
cat("\nTraining RMSLE - Random Forest (Full Model): ", rf_train_rmsle_full)
cat("\nTraining RMSLE - Random Forest (Reduced Model): : ", rf_train_rmsle2_reduced)

cat("\n\nTest RMSLE - Linear Regression: ", lm_test_RMSLE)
cat("\nTest RMSLE - Random Forest (Full Model): ", rf_test_rmsle_full)
cat("\nTest RMSLE - Random Forest (Reduced Model): ", rf_test_rmsle2_reduced)


        # Save the RF results
        #rf_test_casual = round(predict(casualFitFinal, bike_test),0)
        #rf_test_registered = round(predict(RegisteredFitFinal, bike_test),)
        #rf_results = rf_test_casual + rf_test_registered
        
        hist(bike$count, main="Training Data")
        hist(lm_results, main="Linear Regression Fit")
        hist(rf_results, main="Random Forest Fit")

        # Inference: The distribution of predicted count looks similar to that of train data. 

        plot(bike$count, main="Training Data")
        plot(lm_results, main="Linear Regression Fit")
        plot(rf_results, main="Random Forest Fit")

        # Histograms and plots clearly shows that Random Forest fits better than Linear Regression.


