#Eleanor Lindsey
#February 24, 2025
datasets::airquality
help(airquality)

require(graphics)
pairs(airquality, panel = panel.smooth, main = "airquality data")

air_quality<-airquality #Creating a data frame to view changes

install.packages('visdat')
library(visdat)
vis_miss(airquality) #Ozone and Solar R columns have missing data
vis_dat(airquality) #Wind is numeric, everything else is integer 
vis_guess(airquality)

air_noNA<-air_quality%>%
  na.omit(air_quality) #Removing NA values from the dataset

#The predictor I chose to fit a linear model of ozone to was months. I chose this becuase I wanted to see how the Ozone layer flucutates from May to September.

#Fit a linear model to the cleaned data to predict Ozone from one of the possible predictors of your choosing. Why did you chose thats variable?
  
#Using summary(), Does this seem like a valid model?

#Explain the R2 found in a sentence.

#Use broom::augment to predict the Ozone of the cleaned data

#Use ggplot to plot the actual vs predicted Ozone

#Add a red line to show where the actual and predicted values are equal This can be done by plotting a 1:1 line (e.g. intercept 0, slope 1) with geom_abline(intercept = 0, slope = 1, color = "red")
#Add a subtitle to the plot showing the correlation between the actual and predicted values are equal This can be done by plotting a 1:1 line (e.g. intercept 0, slope 1) with
#paste("Correlation:", round(cor(a$Ozone, a$.fitted),2)) assuming your augmented data object is called a