# ==============================================================================
# PROJECT: STAT383 Final Project
# TITLE:   Statistical Analysis of U.S. Avocado Prices and Sales Volume
# AUTHORS: Christian Garrelts, Andrew Doherty, Savannah Manning
# DATE:    December 2025
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. ENVIRONMENT SETUP
# ------------------------------------------------------------------------------
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")
if(!require(lubridate)) install.packages("lubridate")

library(ggplot2)
library(dplyr)
library(lubridate)

# ------------------------------------------------------------------------------
# 2. DATA IMPORT AND CLEANING
# ------------------------------------------------------------------------------
# Load dataset (Assumes 'avocado.csv' is in the working directory)
raw_data <- read.csv("avocado.csv")
data <- raw_data

# Date Conversion
data$Date <- as.Date(data$Date, format="%Y-%m-%d")
data$Month <- month(data$Date, label=TRUE, abbr=TRUE)

# ------------------------------------------------------------------------------
# 3. VISUALIZATION PART 1: EXPLORATORY ANALYSIS
# ------------------------------------------------------------------------------

# --- FIGURE 1: SEASONALITY (Time Series) ---
monthly_trends <- data %>%
  group_by(Month) %>%
  summarize(Mean_Price = mean(AveragePrice, na.rm=TRUE))

p1 <- ggplot(monthly_trends, aes(x=Month, y=Mean_Price)) +
  geom_bar(stat="identity", fill="#558B2F", alpha=0.9) +
  theme_minimal() +
  labs(title="Figure 1: Average Avocado Price by Month (2015-2018)", 
       y="Average Price ($)", x="Month")
print(p1)

# --- FIGURE 2: TYPE COMPARISON (Boxplot) ---
p2 <- ggplot(data, aes(x=type, y=AveragePrice, fill=type)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#AED581", "#33691E")) +
  theme_minimal() +
  labs(title="Figure 2: Price Distribution: Conventional vs. Organic",
       y="Average Price ($)", x="Avocado Type")
print(p2)

# --- FIGURE 3: REGIONAL ANALYSIS (Top 10 Regions) ---
region_summary <- data %>%
  group_by(region) %>%
  summarize(Mean_Price = mean(AveragePrice)) %>%
  arrange(desc(Mean_Price)) %>%  
  slice(1:10)

p3 <- ggplot(region_summary, aes(x=reorder(region, Mean_Price), y=Mean_Price)) +
  geom_bar(stat="identity", fill="#2E7D32") + 
  coord_flip() + 
  theme_minimal() +
  labs(title="Figure 3: Top 10 Most Expensive Regions",
       x="Region",
       y="Average Price ($)") +
  geom_text(aes(label=round(Mean_Price, 2)), hjust=1.2, color="white", fontface="bold")
print(p3)

# ------------------------------------------------------------------------------
# 4. INFERENTIAL STATISTICS & MODELING
# ------------------------------------------------------------------------------

# --- T-TEST (Type) ---
t_test_result <- t.test(AveragePrice ~ type, data = data)
print("--- T-TEST RESULTS ---")
print(t_test_result)

# --- ANOVA (Region) ---
anova_result <- aov(AveragePrice ~ region, data = data)
print("--- ANOVA RESULTS ---")
summary(anova_result)

# --- REGRESSION MODEL ---
# Predict Price based on Type and Total Volume
model <- lm(AveragePrice ~ type + Total.Volume, data = data)
print("--- REGRESSION SUMMARY ---")
summary(model)

# ------------------------------------------------------------------------------
# 5. VISUALIZATION PART 2: MODEL VALIDATION
# ------------------------------------------------------------------------------

par(mfrow=c(2,2), oma=c(0,0,5,0)) 

# Plot 1: Linearity Check (Residuals vs Fitted)
plot(model, which=1, 
     main="Check for Linearity",
     caption=NULL)

# Plot 2: Normality Check (Q-Q Plot)
plot(model, which=2, 
     main="Check for Normal Distribution",
     caption=NULL)

# Plot 3: Constant Variance Check (Scale-Location)
plot(model, which=3, 
     main="Check for Constant Variance",
     caption=NULL)

# Plot 4: Outlier Check (Residuals vs Leverage)
plot(model, which=5, 
     main="Check for Influential Outliers",
     caption=NULL)

mtext("Figure 4: Model Validation Checks", outer=TRUE, cex=1.5, font=2, line=2)
par(mfrow=c(1,1), oma=c(0,0,0,0))