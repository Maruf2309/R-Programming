
# Assignment 1.1
# Importing Data

df <- read.csv('E:/Secondary File/Study/R/Assignment/breast_cancer.csv')
df

# Required library
library(tidyverse)
library(rgl)

# Taking two independent Variables

ind_var1 = df$radius_mean[1:100]
ind_var1

# Converting row data into data frame
ind_var1 = data.frame(df$radius_mean[1:100]) 
ind_var1

ind_var2 = data.frame(df$perimeter_mean[1:100]) 
ind_var2

# Taking an Independent Variable
dep_1 <- data.frame(df$radius_worst[1:100]) 
dep_1

# estimate a linear regration model:

lrm <- lm(radius_worst ~ radius_mean + perimeter_mean, data = df)
lrm
summary(lrm)

### The model is: radius_worst =  -2.02636 +  0.68557 * radius_mean +  0.09362 * perimeter_mean

# plot model vs actual (plot for actual value)
plot(df$radius_worst, lrm$fitted.values)  # for drawing a line

# 3D plot (Plot for fitted values)
plot3d (x = df$radius_mean,
        y = df$perimeter_mean,
        z = lrm$fitted.values,
        type = 's',
        col = 'green',
        xlim = c(min(df$radius_mean)-1, max(df$radius_mean)+1),
        ylim = c(min(df$perimeter_mean)-1, max(df$perimeter_mean)+1),
        zlim = c(min(df$radius_worst)-1, max(df$radius_worst)+1),
        xlab = 'radius_mean',
        ylab =  'perimeter_mean',
        zlab = 'radius_worst',
        add = TRUE)

# Model Error
lrm      # gives you the 'beta' vector
summary(lrm)


# calculate error measures for model
error.measure2 <- as.data.frame(cbind(lrm$model$radius_worst,lrm$fitted.values))
error.measure2
  colnames(error.measure2) <- c ("Actuals", "Predicted.Values")
error.measure2 <-error.measure2 %>%
  mutate(error = Actuals - Predicted.Values,
         squared.error = error^2)
error.measure2


### Calculating beta vector
# Define breast cancer data (desired model: radius_worst ~ radius_mean + perimeter_mean)
y <-as.matrix(df$radius_worst)
x <-as.matrix(cbind(rep(1,length(df$radius_worst)), df[ ,c('radius_mean', 'perimeter_mean')]))
View(x)


# calculate beta vector
beta <-solve(t(x) %*%x) %*% t(x) %*% y
beta

# Finding Relationship among dependent & indipendent variables
# Sub set of data

df1 <- (cbind(df$radius_mean, df$perimeter_mean, df$radius_worst))
head(df1)

df1 <- data.frame(cbind(df$radius_mean, df$perimeter_mean, df$radius_worst))
head(df1)



# matrix of scattar plot
pairs(df1)








