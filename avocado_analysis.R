# =============================================================================


#  === ENVIRONMENT CONFIGURATION ===

# Utility function to format console output for readability
print_header <- function(title) {
  cat("\n")
  cat(paste0(rep("=", 60), collapse = ""), "\n")
  cat(paste0("  ", title), "\n")
  cat(paste0(rep("=", 60), collapse = ""), "\n")
}

# Load required libraries 
suppressPackageStartupMessages({
  if(!require(ggplot2)) install.packages
  if(!require(dplyr)) install.packages
  if(!require(lubridate)) install.packages
  library(ggplot2)
  library(dplyr)
  library(lubridate)
})


# === DATA IMPORT AND PRE-PROCESSING ===

# Load raw dataset (Assumes 'avocado.csv' is in the working directory)
raw_data <- read.csv("avocado.csv")
data <- raw_data

# Convert 'Date' to Date class object for time-series analysis
data$Date <- as.Date(data$Date, format="%Y-%m-%d")

# Extract 'Month' factor to analyze seasonal trends
data$Month <- month(data$Date, label=TRUE, abbr=TRUE)



# === EXPLORATORY DATA ANALYSIS (VISUALIZATION) ===

# --- Figure 1: Seasonality Analysis ---
# Aggregating mean price by month to identify seasonal cycles
monthly_trends <- data %>%
  group_by(Month) %>%
  summarize(Mean_Price = mean(AveragePrice, na.rm=TRUE))

p1 <- ggplot(monthly_trends, aes(x=Month, y=Mean_Price)) +
  geom_bar(stat="identity", fill="#558B2F", alpha=0.9) +
  theme_minimal() +
  labs(title="Figure 1: Average Avocado Price by Month (2015-2018)", 
       y="Average Price ($)", x="Month")
print(p1)

# --- Figure 2: Product Type Analysis ---
# Comparing price distribution between Organic and Conventional types
p2 <- ggplot(data, aes(x=type, y=AveragePrice, fill=type)) +
  geom_boxplot() +
  scale_fill_manual(values=c("#AED581", "#33691E")) +
  theme_minimal() +
  labs(title="Figure 2: Price Distribution: Conventional vs. Organic",
       y="Average Price ($)", x="Avocado Type")
print(p2)

# --- Figure 3: Regional Analysis ---
# Identifying the top 10 regions with the highest average prices
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
       x="Region", y="Average Price ($)") +
  geom_text(aes(label=round(Mean_Price, 2)), hjust=1.2, color="white", fontface="bold")
print(p3)


# === STATISTICAL ANALYSIS (CONSOLE OUTPUT) ===

print_header("SECTION 1: DESCRIPTIVE STATISTICS")
cat("Total Observations:", nrow(data), "\n")
cat("Overall Mean Price: $", round(mean(data$AveragePrice), 2), "\n")
cat("Price Range:        $", min(data$AveragePrice), "to $", max(data$AveragePrice), "\n")

print_header("SECTION 2: HYPOTHESIS TESTING (T-TEST)")
# Test for significant difference in means between Organic and Conventional
t_test_result <- t.test(AveragePrice ~ type, data = data)
print(t_test_result)

print_header("SECTION 3: ANALYSIS OF VARIANCE (ANOVA)")
# Test for significant difference in means across geographic regions
anova_result <- aov(AveragePrice ~ region, data = data)
print(summary(anova_result))

print_header("SECTION 4: REGRESSION MODEL SUMMARY")
# Model: AveragePrice ~ Type + Total.Volume
model <- lm(AveragePrice ~ type + Total.Volume, data = data)
print(summary(model))


# === MODEL VALIDATION (DIAGNOSTICS) ===

print_header("GENERATING DIAGNOSTIC PLOTS (FIGURE 4)...")

par(mfrow=c(2,2), oma=c(0,0,5,0)) 

# 1. Linearity Check
plot(model, which=1, main="Check for Linearity\n(Residuals vs Fitted)", caption=NULL)
# 2. Normality Check
plot(model, which=2, main="Check for Normality\n(Normal Q-Q)", caption=NULL)
# 3. Homoscedasticity Check
plot(model, which=3, main="Check for Constant Variance\n(Scale-Location)", caption=NULL)
# 4. Influential Outlier Check
plot(model, which=5, main="Check for Outliers\n(Residuals vs Leverage)", caption=NULL)

mtext("Figure 4: Regression Model Diagnostics", outer=TRUE, cex=1.5, font=2, line=2)
par(mfrow=c(1,1), oma=c(0,0,0,0))


cat(">> Execution Complete. All figures generated.\n")
