# Customer-Segmentation-Analysis

## Table of Contents

- [Project Overview](#project-overview)
- [Data Sources](#data-sources)
- [Tools](#tools)
- [Data Cleaning/Preparation](#data-cleaningpreparation)
- [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)
- [Data Analysis](#data-analysis)
- [Results/Findings](#resultsfindings)
- [Recommendations](#recommendations)
- [Limitations](#limitations)
- [References](#references)

### Project Overview

This project intends to provide insight on which customers generate the most revenue, and the different ways customers can be grouped or categorized, based on their demographic and spending habits in order to get a better understanding of them. This, in return will help the store adapt optimum sales strategies and maximize on profits.


### Data Sources

Customer Data: The main dataset used for this project is the "customer personality.csv" file, containing items purchased, total cost and other demographic information that is vital for analysis.

### Tools

- R programming - Data cleaning, Data Analysis and Creating Report
   
[Download Here](https://posit.co/download/rstudio-desktop/) 


### Data Cleaning/Preparation 

In the initial data preparation stage, we carried out the following tasks:
1. Loading data and inspection
2. Handling missing values
3. Handling duplicate values
4. Creating new columns by combining pre-existing ones
5. Removing unnecessary columns
6. Renaming columns for clarity

### Exploratory Data Analysis (EDA)

We explored the customer data to get answers to questions such as:

- Who make up the majority of customers for the store?
- Which customer factors have an influence in generating the most revenue?
- What are the key groups of customers and how do they contribute to the sales?

### Data Analysis

```R script
## DATA CLEANING
library(tidyverse)

Data = read.delim("C:/Users/WAME/Documents/Customer Personality Analysis Project/customer personality.csv", header = T)

head(Data)

##showing missing values

show_missing = function(df) {
  n = sum(is.na(df)) 
  cat("Missing values: ", n, "\n", sep = "")

invisible(df) 
}

show_missing(Data)

inco = arrange(Data, Income)  #shows that all the missing values come from income

Data2 = na.omit(Data)

which(duplicated(Data2))

## DATA MANIPULATION/TRANSFORMATION

# Combining columns
Data3 = transmute(Data2, ID, Age = 2014 - Year_Birth, Education, Marital_Status, 
                  Tot_Kids = Kidhome + Teenhome, Income, Dt_Customer, Tot_Revenue = 
                    MntWines + MntFruits + MntMeatProducts + MntFishProducts + 
                    MntSweetProducts + MntGoldProds, NumDealsPurchases, NumWebPurchases,
                   NumCatalogPurchases, NumStorePurchases, Complain)



# Renaming entries
Data3 <- Data3 %>%
  mutate(Marital_Status = recode(Marital_Status,
                                 "YOLO" = "Single",
                                 "Absurd"= "Single",
                                 "Alone" = "Single",
                                 "Together" = "In a Relationship",
                                 "Widow" = "Widowed"))


Data4 <- Data3 %>%
  mutate(Education = recode(Education,
                                 "2n Cycle" = "Undergraduate",
                                 "Graduation"= "Undergraduate",
                                 "Basic" = "O-Level"))


print(df)


## EXPLORATORY DATA ANALYSIS
library(ggplot2)

## 1.HISTOGRAM FOR AGE
ggplot(Data3, aes(x = Age)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  labs(title = "Histogram of Age", x = "Age", y = "Frequency")

## 2.PIE CHARTS FOR CUSTOMER CATEGORIES

# Summarized data for Education
education_freq <- Data4 %>%
  group_by(Education) %>%
  summarize(Frequency = n()) %>%
  mutate(Percentage = Frequency / sum(Frequency) * 100)

# Summarized data for Marital Status
marital_status_freq <- Data3 %>%
  group_by(Marital_Status) %>%
  summarize(Frequency = n()) %>%
  mutate(Percentage = Frequency / sum(Frequency) * 100)

# pie chart for Education
ggplot(education_freq, aes(x = "", y = Frequency, fill = Education)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Education Levels", x = NULL, y = NULL) +
  theme_void() +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))

# pie chart for Marital Status
ggplot(marital_status_freq, aes(x = "", y = Frequency, fill = Marital_Status)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  labs(title = "Distribution of Marital Status", x = NULL, y = NULL) +
  theme_void() +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5))


## 3.BARPLOTS FOR TOTAL REVENUE(SALES)

library(scales)
library(dplyr)

## EDUCATION VS. TOTAL REVENUE

# Summary total revenue by education level
education_revenue <- Data4 %>%
  group_by(Education) %>%
  summarise(Total_Revenue = sum(Tot_Revenue))

# Plot the bar graph
ggplot(education_revenue, aes(x = Education, y = Total_Revenue, fill = Education)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Revenue by Education Level", x = "Education Level", y = "Total Revenue") + 
  scale_y_continuous(labels = comma) + 
  theme_minimal()



## No. OF KIDS VS. TOTAL REVENUE

# Summary of total revenue by no. of kids
Kids_revenue = Data3 %>%
  group_by(Tot_Kids) %>%
  summarise(Revenue = sum(Tot_Revenue))

# Plot the bar graph
ggplot(Kids_revenue, aes(x = Tot_Kids, y = Revenue, fill = Tot_Kids)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Revenue by Number of Kids", x = "Kids Total", y = "Revenue") + 
  scale_y_continuous(labels = comma) +
  theme_minimal()



## MARITAL STATUS VS. TOTAL REVENUE

# Summary of total revenue by marital status
Status_revenue = Data3 %>%
  group_by(Marital_Status) %>%
  summarise(Revenue = sum(Tot_Revenue))


# Plot the bar graph
ggplot(Status_revenue, aes(x = Marital_Status, y = Revenue, fill = Marital_Status)) +
  geom_bar(stat = "identity") +
  labs(title = "Total Revenue by Marital Status", x = "Marital Status", y = "Revenue") +
  scale_y_continuous(labels = comma) +
  theme_minimal()



##CLUSTER ANALYSIS


library(cluster)
library(factoextra)


# Selecting the demographic components

Demographics = Data3 %>%
  select(Age, Tot_Kids, Income, Tot_Revenue, NumWebPurchases,
          NumStorePurchases)

#Normalizing the data

Norm.Data = scale(Demographics)

#Finding optimum number of clusters using elbow method

fviz_nbclust(Norm.Data, kmeans, method = "wss") + 
  labs(title = "Elbow Method for Optimal Clusters")

#Performing kmeans clustering
set.seed(123)
K.means = kmeans(Norm.Data, centers = 4, nstart = 25)

#Adding assignment of clusters to original data
Data3$Cluster = K.means$cluster

#Visualization
fviz_cluster(K.means, data = Norm.Data, geom = "point", ellipse.type = "t",
             ggtheme = theme_minimal(), main = "K-means Clustering")
               
               
Cluster.summ = Data3 %>% 
  group_by(Cluster) %>% 
  summarize(across(c(Age, Tot_Kids, Income, Tot_Revenue,
                     NumWebPurchases, NumStorePurchases), 
        mean))

view(Cluster.summ)     
```

### Results/Findings

The analysis results are summarized as follows:
1. The majority of customers for the store are about 390 with ages between 38-42
2. 59.4% of the store buyers have Undergraduate level of education
3. 38.7% of buyers are married
4. The higher education level corresponds to a higher income, therefore higher spending habits.
5. Customers who have less children generate more sales/revenue for the store.
6. Couples who are married generate the most sales compared to those who are in relationships, single etc.
7. The optimum number of clusters of customers is 4. Based on the demographic information on each group, we decided to name them:
   
 -Emerging Spenders: younger, low income shoppers with occasional purchases and growing potential

 -Premium spenders: well off, high spending and frequent middle-aged buyers who are key contributors the store's revenue
 
 -Family-focused budget shoppers: older customers with children who prioritize affordability and shop moderately 
 
 -Convenience seekers: middle-aged high income shoppers who prioritize convenience with frequent purchases both online and  offline.

### Recommendations
- The store could collaborate with local brands for cheaper products that cater to low budget customers
- Offer discounts for combining in-store and online purchases
- Introduce a reward programme where loyal customers get early access to sales or new products
- Create bulk deals tailored to families, such as discounts on items for kids
- Create affordable bundles or packages with essential items

### Limitations

Rows with null values and some columns were deleted which may affect the statistical power of the analysis and may introduce bias.

### References

1. R for Data Science by Hadley Wickham & Garrett Grolemund
2. Practical Guide to Cluster Analysis in R by Alboukadel Kassanbara
3. An introduction to data cleaning with R by Edwin de Jonge & Mark van der Loo
4. [R-bloggers](https://www.r-bloggers.com/2020/02/the-complete-guide-to-clustering-analysis-k-means-and-hierarchical-clustering-by-hand-and-in-r/)
