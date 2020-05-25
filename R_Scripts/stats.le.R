library(easypackages)
libraries("tidyverse","inspectdf", "dlookr", "naniar", "DataExplorer")
theme_set(theme_light())

# Inspect the Data Frame
data.raw.le <- read.csv("Data/Life Expectancy Data.csv")
dim_desc(data.raw.le)
str(data.raw.le)
data.raw.le$Year <- as.factor(data.raw.le$Year)
glimpse(data.raw.le)
summary(data.raw.le)

inspect_num(data.raw.le) %>%
  show_plot()

# Exploring Missing Values
sum(is.na(data.raw.le))
colSums(is.na(data.raw.le))

missing.data.le <- data.raw.le %>%
  diagnose() %>%
  select(-unique_count, unique_rate) %>%
  filter(missing_count>0) %>%
  arrange(desc(missing_count))
head(missing.data.le)

# Rounding Off missing values
round.2.two <- function(x, digits){
  numeric.cols <- sapply(x, mode) == 'numeric'
  x[numeric.cols] <- round(x[numeric.cols], digits)
  x
}
missing.data.le <-  round.2.two(missing.data.le, 2)
inspect_na(data.raw.le) %>%
  show_plot()
plot_missing(data.raw.le)
gg_miss_upset(data.raw.le)

# Listwise deletion
le.data <-  data.raw.le[complete.cases(data.raw.le), ]
dim_desc(le.data)
sum(is.na(le.data))

# Outliers Table
le.outliers <- le.data %>%
  diagnose_outlier() %>%
  select(-outliers_mean) %>%
  arrange(desc(outliers_cnt))
le.outliers <- round.2.two(le.outliers, 2)
le.outliers


le.data %>%
  plot_outlier(
    diagnose_outlier(le.data) %>%
      filter(outliers_ratio >= 10) %>%
      select(variables) %>%
      unlist()
  )



# Outlier Visuals
ggplot(le.data, aes(x = Alcohol)) + geom_histogram(aes(x = Alcohol, y = ..density..), fill="#69b3a2") + 
  geom_label(aes(x = 4.5, y = 0.25, label = "HIV AIDS"), color = "#69b3a2") +
  geom_histogram(aes(x = BMI, y = -..density..), fill="#404080") + 
  geom_label(aes(x = 4.5, y = -0.25, label = "BMI"), color = "#404080") + xlab("Value of x")

ggplot(le.data, aes(Alcohol)) +  geom_histogram()
   







