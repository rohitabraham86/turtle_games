################################################################################
## 1. Prepare the workstation.

## SET THE WORKING DIRECTORY 

# Install and load library
library(tidyverse)

# Load and explore the data
turtle_sales <- read.csv("turtle_sales.csv", header = TRUE)
# Print the data frame.
View(turtle_sales)

# Removing redundant columns.
turtle_sales_sub <- select(turtle_sales, -Ranking, -Year, -Genre, -Publisher)
View(turtle_sales_sub)

################################################################################
## Explore the data set.

# Summary of the dataframe
summary(turtle_sales_sub)

### 'NA_Sales' & 'EU_sales' has min sales as 0
sales <- filter(turtle_sales_sub, NA_Sales > 0, EU_Sales > 0)

### Convert 'Product' to factor (categorical variable)
sales <- mutate(sales, Product = as.factor(Product))

head(sales)
summary(sales)
dim(sales)
colnames(sales)
################################################################################
# Visualizing the dataset.

# Histogram
par(mfcol = c(1,1))
ggplot(data = sales, aes(x = NA_Sales)) + geom_histogram(bins = 20)
ggplot(data = sales, aes(x = EU_Sales)) + geom_histogram(bins = 20)
ggplot(data = sales, aes(x = Global_Sales)) + geom_histogram(bins = 20)

# Scatterplot
ggplot(data = sales, aes(x = Platform, y = NA_Sales)) + 
  geom_point() 
ggplot(data = sales, aes(x = Platform, y = EU_Sales)) + 
  geom_point() 
ggplot(data = sales, aes(x = Platform, y = Global_Sales)) + 
  geom_point() 

# Box plots
ggplot(data = sales, aes(x = NA_Sales)) + geom_boxplot() 
ggplot(data = sales, aes(x = EU_Sales)) + geom_boxplot() 
ggplot(data = sales, aes(x = Global_Sales)) + geom_boxplot() 


# The histogram for the three regions show that they are are right-skewed, or positive-skewed, 
# many of the values are near the lower end of the range, and higher values are infrequent.
# The boxplot and scatterplots shows there are outliers.

# Export the data as a CSV file.
write_csv(sales, file='sales.csv')
################################################################################

## Cleaning and manipulating

# Check output: Determine the min, max, and mean of sales values.
summary(subset(sales, select = -c(Product, Platform)))


sales_prd <- sales %>%
  group_by(Product,Platform) %>%
  summarise(NA_Sales_sum = sum(NA_Sales),
            EU_Sales_sum = sum(EU_Sales),
            Global_Sales_sum = sum(Global_Sales),
            .groups = 'drop')



# Histogram
ggplot(data = sales_prd, aes(x = NA_Sales_sum)) + geom_histogram(bins = 20)
ggplot(data = sales_prd, aes(x = EU_Sales_sum)) + geom_histogram(bins = 20)
ggplot(data = sales_prd, aes(x = Global_Sales_sum)) + geom_histogram(bins = 20)

# Scatterplot
ggplot(data = sales_prd, aes(x = Platform, y = NA_Sales_sum)) + 
  geom_point() 
ggplot(data = sales_prd, aes(x = Platform, y = EU_Sales_sum)) + 
  geom_point() 
ggplot(data = sales_prd, aes(x = Platform, y = Global_Sales_sum)) + 
  geom_point() 

# Box plots
ggplot(data = sales_prd, aes(x = NA_Sales_sum)) + geom_boxplot() 
ggplot(data = sales_prd, aes(x = EU_Sales_sum)) + geom_boxplot() 
ggplot(data = sales_prd, aes(x = Global_Sales_sum)) + geom_boxplot()


# Determine the normality of the data set.
# Create Q-Q Plots
library(moments)
par(mfrow=c(1,3))
qqnorm(sales_prd$NA_Sales_sum, main = 'NA - Normal Q-Q Plot')
qqline(sales_prd$NA_Sales_sum,
       col = 'red',
       lwd = 2)

qqnorm(sales_prd$EU_Sales_sum, main = 'EU - Normal Q-Q Plot')
qqline(sales_prd$EU_Sales_sum,
       col = 'blue',
       lwd = 2)

qqnorm(sales_prd$Global_Sales_sum, main = 'Global - Normal Q-Q Plot')
qqline(sales_prd$Global_Sales_sum,
       col = 'orange',
       lwd = 2)
#qqnorm(sales_prd)

# The values in the tails of the distribution of all 3 plots are quiet extreme 
# from the normal distribution.

# Perform Shapiro-Wilk test
shapiro.test(sales_prd$NA_Sales_sum)
shapiro.test(sales_prd$EU_Sales_sum)
shapiro.test(sales_prd$Global_Sales_sum)
# Since the p-values of all the 3 are very less than the statistically significant
# p-value of 0.05, the values are not distributed normally.

## Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales_prd$NA_Sales_sum)
skewness(sales_prd$EU_Sales_sum)
skewness(sales_prd$Global_Sales_sum)
# The values indicate that the distribution is a positive skew.

kurtosis(sales_prd$NA_Sales_sum)
kurtosis(sales_prd$EU_Sales_sum)
kurtosis(sales_prd$Global_Sales_sum)
# The kurtosis values of all the three are more than 3, which means that all three
# are heavy tailed, meaning all are further away from the mean.

## Determine correlation
# Determine correlation.
cor(sales_prd$NA_Sales_sum, sales_prd$EU_Sales_sum) 
cor(sales_prd$NA_Sales_sum, sales_prd$Global_Sales_sum)
cor(sales_prd$EU_Sales_sum, sales_prd$Global_Sales_sum)
# All three sales data have positive correlation and are more close to 1, hence 
# they have strong positive correlation, meaning as one increases the other also increases.

# Visualize
ggplot(data = sales_prd, aes(x = NA_Sales_sum, y = EU_Sales_sum)) + 
  geom_point() + 
  geom_smooth() +
  labs(title = "Relation between North America & European Union Sales",
       x = "North America Sales",
       y = "European Union Sales")


ggplot(data = sales_prd, aes(x = NA_Sales_sum, y = Global_Sales_sum)) + 
  geom_point() + 
  geom_smooth() +
  labs(title = "Relation between North America & Global Sales",
       x = "North America Sales",
       y = "Global Sale")


ggplot(data = sales_prd, aes(x = EU_Sales_sum, y = Global_Sales_sum)) + 
  geom_point() + 
  geom_smooth() +
  labs(title = "Relation between European Union & Global Sales",
       x = "European Union Sales",
       y = "Global Sale")
################################################################################

## Create a simple linear regression model
# Determine the correlation between columns

cor(sales_prd$NA_Sales_sum, sales_prd$EU_Sales_sum)
cor(sales_prd$NA_Sales_sum, sales_prd$Global_Sales_sum)
cor(sales_prd$EU_Sales_sum, sales_prd$Global_Sales_sum)
# There is a positive correlation of 69.7% between North America & EU sales, 
# but it is not that strong.
# There is a strong positive correlation of 93.3% between North America & Global sales. 
# There is a strong positive correlation of 87.4% between EU & Global sales. 

plot(sales_prd$NA_Sales_sum, sales_prd$EU_Sales_sum)
plot(sales_prd$NA_Sales_sum, sales_prd$Global_Sales_sum)
plot(sales_prd$EU_Sales_sum, sales_prd$Global_Sales_sum)

modelne <- lm(EU_Sales_sum ~ NA_Sales_sum, data = sales_prd)
modelne
summary(modelne)
# The p-value is very very small, which tell NA_Sales_sum is a highly significant variable.
# Multiple R-squared is 48.68%. That is NA_Sales_sum explains 48.68% of the variability
# of the EU_Sales_sum value.

plot(modelne$residuals)
# There is no pattern.

modelng <- lm(Global_Sales_sum ~ NA_Sales_sum, data = sales_prd)
modelng
summary(modelng)
# The p-value is very very small, which tell NA_Sales_sum is a highly significant variable.
# Multiple R-squared is 87.07%. That is NA_Sales_sum explains 87.07% of the variability
# of the Global_Sales_sum value.

plot(modelng$residuals)
# There is no pattern.

modeleg <- lm(Global_Sales_sum ~ EU_Sales_sum, data = sales_prd)
modeleg
summary(modeleg)
# The p-value is very very small, which tell EU_Sales_sum is a highly significant variable.
# Multiple R-squared is 76.53%. That is NA_Sales_sum explains 76.53% of the variability
# of the Global_Sales_sum value.

plot(modeleg$residuals)
# There is no pattern.

################################################################################


## Create a multiple linear regression model


# Create a new object and 
# specify the lm function and the variables.
modela = lm(Global_Sales_sum~NA_Sales_sum + EU_Sales_sum, data=sales_prd)

# Print the summary statistics.
summary(modela)
#NA_Sales_sum  1.15237    0.02502  46.064  < 2e-16 ***
#EU_Sales_sum  1.34554    0.04213  31.939  < 2e-16 ***
# Multiple R-squared:  0.9683,	Adjusted R-squared:  0.9681

################################################################################


# Predictions based on given values
# Compare with observed values for a number of records.

NA_Sales_sum <- c(34.02, 3.93, 2.73, 2.26, 22.08)
EU_Sales_sum <- c(23.80, 1.56, 0.65, 0.97, 0.52)

test_sales_value <- data.frame(NA_Sales_sum, EU_Sales_sum)

# Create a new object and specify the predict function.
predictTest = predict(modela, newdata=test_sales_value,
                      interval='confidence')

# Print the object.
predictTest 


