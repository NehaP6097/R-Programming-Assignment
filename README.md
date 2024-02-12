NY.House.Dataset =read.csv("C:/Users/patil/Downloads/NY-House-Dataset.csv/NY.House.Dataset.csv", header=FALSE)
#View(NY.House.Dataset)

#changing row 1 to column names
colnames(NY.House.Dataset) = NY.House.Dataset[1, ] 

#delete column 1
NY.House.Dataset = NY.House.Dataset[-1, ]

#converting datatypes
NY.House.Dataset$PRICE = as.integer(NY.House.Dataset$PRICE)
NY.House.Dataset$BEDS = as.integer(NY.House.Dataset$BEDS)
NY.House.Dataset$BATH = as.integer(NY.House.Dataset$BATH)
NY.House.Dataset$PROPERTYSQFT = as.double(NY.House.Dataset$PROPERTYSQFT)
NY.House.Dataset$LATITUDE = as.double(NY.House.Dataset$LATITUDE)
NY.House.Dataset$LONGITUDE = as.double(NY.House.Dataset$LONGITUDE)
```

```{r struct, echo=TRUE}
knitr::opts_chunk$set(
	echo = TRUE,
	message = TRUE,
	warning = TRUE
)
#Importing Libraries
library(dplyr)
library(tidyr)

#Print the structure of your dataset
str(NY.House.Dataset)
```

```{r list, echo=TRUE}
#List the variables in your dataset
names(NY.House.Dataset)
```

```{r rop15, echo=TRUE}
# Print the top 15 rows of your dataset
head(NY.House.Dataset, n=15)
TOP_15_Rows= head(NY.House.Dataset, n=15)
TOP_15_Rows
```

```{r echo=TRUE}
# Write a user defined function using any of the variables from the data set
Get_Max_Price = function(df, var){ return(max(df[[var]])) }
Get_Max_Price(NY.House.Dataset, "PRICE")
Get_Min_Price = function(df, var){ return(min(df[[var]])) }
Get_Min_Price(NY.House.Dataset, "PRICE")
Bed_Bath_Total = function(df, var1, var2) { return((df[[var1]])+(df[[var2]]))}
Bed_Bath_Total(NY.House.Dataset, "BEDS", "BATH")
```

```{r echo=TRUE}
#Use data manipulation techniques and filter rows based on any logical criteria that exist in your dataset
filtered_dataset = filter(NY.House.Dataset, TYPE == "House for sale" & LOCALITY == "New York")
#View(filtered_dataset)
filtered_dataset
```

```{r echo=TRUE}

#Identify the dependent & independent variables and use reshaping techniques and create a new data frame by joining those variables from your dataset.
House_address = cbind(NY.House.Dataset$ADDRESS, NY.House.Dataset$STATE, NY.House.Dataset$LOCALITY, NY.House.Dataset$SUBLOCALITY)
#View(House_address)
House_address

```

```{r echo=TRUE}
# Remove missing values in your dataset.
NY.House.Dataset = na.omit(NY.House.Dataset)
#Identify and remove duplicated data in your dataset
Duplicate_Values = duplicated(NY.House.Dataset)
Duplicate_Values
```

```{r echo=TRUE}
#Deleting Duplicate rows if any
Delete_duplicate = NY.House.Dataset[!duplicated(NY.House.Dataset), ]
#View(Delete_duplicate)
NY.House.Dataset[!duplicated(NY.House.Dataset), ]
NY.House.Dataset = NY.House.Dataset[!duplicated(NY.House.Dataset), ]
```

```{r echo=TRUE}
# Remove missing values in your dataset.
NY.House.Dataset = na.omit(NY.House.Dataset)
#Deleting Duplicate rows if any
Delete_duplicate = NY.House.Dataset[!duplicated(NY.House.Dataset), ]
#View(Delete_duplicate)
NY.House.Dataset[!duplicated(NY.House.Dataset), ]
NY.House.Dataset = NY.House.Dataset[!duplicated(NY.House.Dataset), ]
```

```{r echo=TRUE}
# Reorder multiple rows in descending order
library(dplyr)
library(tidyr)
Ordered_DataSet = NY.House.Dataset %>% arrange(desc(PRICE) , desc(PROPERTYSQFT) , desc(BEDS) , desc(BATH))
#View(Ordered_DataSet)
Ordered_DataSet
```

```{r}
#Rename some of the column names in your dataset
library(dplyr)
NY.House.Dataset = NY.House.Dataset %>% rename(BROKER = BROKERTITLE, HOUSE_TYPE = TYPE, PROPERTY_SQFT = PROPERTYSQFT)
#View(NY.House.Dataset)
NY.House.Dataset
```

```{r echo=TRUE}
#Add new variables in your data frame by using a mathematical function (for e.g. – multiply an existing column by 2 and add it as a new variable to your data frame
library(dplyr)
NY.House.Dataset = NY.House.Dataset %>% mutate(MORTGAGE = ((0.03/12)*PRICE))
NY.House.Dataset
```

```{r}
#Create a training set using random number generator engine.
library(dplyr)
library(tidyr)
set.seed(123)
n_train = 40
train_indices <- sample(1:nrow(NY.House.Dataset), n_train)
train_set <- NY.House.Dataset[train_indices, ]
#View(train_set)
train_set
```

```{r}
#Print the summary statistics of your dataset
library(dplyr)
library(tidyr)
Summary_stats = summary(NY.House.Dataset)
Summary_stats
```

```{r}
#Use any of the numerical variables from the dataset and perform the following statistical functions
#Mean
#Median
#Mode
#Range
library(dplyr)
library(tidyr)
mean_value <- mean(NY.House.Dataset$PRICE)
median_value = median(NY.House.Dataset$PRICE)
mode_value <- table(NY.House.Dataset$PRICE)[which.max(table(NY.House.Dataset$PRICE))]
range_value = range(NY.House.Dataset$PRICE)
print(paste0("mean: ", mean_value))
print(paste0("median: ", median_value))
print(paste0("mode: ", mode_value))
print(paste0("range: ", range_value))
```

```{r}
library(dplyr)
library(tidyr)
library(ggplot2)
#Plot a scatter plot for any 2 variables in your dataset

ggplot(NY.House.Dataset, aes(x = BEDS, y = BATH)) +
geom_point(size = 1.2, color = "blue", shape = 21) + labs(x = " BEDS", y = " BATH", title = "Scatter Plot")
```

```{r echo=TRUE}
# Plot a bar plot for any 2 variables in your dataset
library(dplyr)
library(tidyr)
library(ggplot2)
ggplot(NY.House.Dataset, aes(x = LOCALITY, fill = PROPERTYSQFT)) + geom_bar(fill = "blue") + labs(x = "LOCALITY", y = "PROPERTY SQFT", title = "Bar Plot")
```

```{r}
#Find the correlation between any 2 variables by applying least square linear regression model
library(dplyr)
library(tidyr)
library(ggplot2)
ggplot(NY.House.Dataset, aes(x = LATITUDE, y = LONGITUDE)) +
geom_smooth(method = "lm", se = FALSE) +
labs(x = "LATITUDE", y = "LONGITUDE", title = "Linear Regression Correlation Plot")
```
