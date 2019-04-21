# Importing and exploring the dataset
library(readr)
library(dplyr)
org <- read.csv('C:/Users/SARANG/Desktop/DOCS/Jobs/Portfolio/R_EmployeeTurnover/org.csv')
head(org)
org %>%
  count(status)
org %>%
  summarise(turnover_rate = mean(turnover))
# Turnover rate at each level
df_level <- org %>%
  group_by(level) %>%
  summarise(turnover_level = mean(turnover))

install.packages('ggplot2')
library(ggplot2)
ggplot(df_level, aes(x = level, y = turnover_level)) + 
  geom_col()

# Filtering the dataset
org2 <- org %>%
  filter(level %in% c('Analyst', 'Specialist'))
org2 %>%
  count(level)

# Importing the rating data
rating <- read.csv('C:/Users/SARANG/Desktop/DOCS/Jobs/Portfolio/R_EmployeeTurnover/rating.csv')
head(rating)
org3 <- left_join(org2, rating, by = 'emp_id')

# Ratingwise turnover rate
rating_turnover <- org3 %>%
  group_by(rating) %>%
  summarise(turnover_rating = mean(turnover))
rating_turnover

# Importing some more datasets like Survey data, distance, leaves taken etc

org_final <- read.csv('C:/Users/SARANG/Desktop/DOCS/Jobs/Portfolio/R_EmployeeTurnover/org_final.csv')

glimpse(org_final)

ggplot(org_final, aes(x = status, y = distance_from_home)) +
  geom_boxplot()

# Age difference impact
emp_age_diff <- org_final %>%
  mutate(age_diff = mgr_age - emp_age)

ggplot(emp_age_diff, aes(x = status, y = age_diff)) + 
  geom_boxplot()

# Tenure impact
install.packages('lubridate')
library(lubridate)
emp_tenure <- org_final %>%
  mutate(tenure = ifelse(status == 'Active', time_length(interval(date_of_joining,cutoff_date),'years'),
                         time_length(interval(date_of_joining,last_working_date),'years')))

ggplot(emp_tenure, aes(x = status, y = tenure)) + 
  geom_boxplot()

# Job Hoping Index
emp_jhi <- emp_tenure %>%
  mutate(jhi = (total_experience-tenure)/no_previous_companies_worked)

ggplot(emp_jhi, aes(x = status, y = jhi)) + 
  geom_boxplot()

# Compensation
ggplot(emp_jhi, aes(x = level, y = compensation, fill = status)) +
  geom_boxplot()

emp_compa_ratio <- emp_jhi %>%
  group_by(level) %>%
  mutate(compa_ratio = compensation/median(compensation))
emp_compa_ratio %>%
  distinct(level, median(compensation))

emp <- emp_compa_ratio %>%
  mutate(compa_level = ifelse(compa_ratio > 1, 'Above', 'Below'))

ggplot(emp, aes(x = status, fill = compa_level)) +
  geom_bar(position = 'fill')

install.packages('Information')
library(Information)

IV <- create_infotables(data = emp, y = 'turnover')

IV$Summary

# Building the model

# Split the dataset into train,test
install.packages('caret')
library(caret)
index <- createDataPartition(emp$turnover, p = 0.7, list = FALSE)
train_set <- emp[index,]
test_set <- emp[-index,]

library(dplyr)
train_set %>%
  count(status) %>%
  mutate(prop = n/sum(n))

test_set %>%
  count(status) %>%
  mutate(prop = n/sum(n))

# Building model

# Dropping unwanted variables before running logistic regression
train_set_multi <- train_set %>%
  select(-c(emp_id, mgr_id,
            date_of_joining, last_working_date, cutoff_date,
            mgr_age, emp_age,
            department, status,jhi))

log_model <- glm(turnover ~ ., family = 'binomial', data = train_set_multi)
summary(log_model)

# Validating the correlation
install.packages('car')
library(car)

vif(log_model) # level, promotion_last_2_years, compa ratio are highly correleated

final_train_data <- train_set_multi %>%
  select(-c(level,promotion_last_2_years,compa_ratio))

final_log_model <- glm(turnover ~ ., family = 'binomial', data = final_train_data)
vif(final_log_model)

# Predicting the outcome

pred_train <- predict(final_log_model, final_train_data, type = 'response')
hist(pred_train)
pred_test <- predict(final_log_model, test_set, type = 'response')
hist(pred_test)

# Validating using Confusion Matrix
library(caret)

pred_cutoff_test_50 <- ifelse(pred_test > 0.50, 1, 0)

test_set$turnover <- as.factor(test_set$turnover)
pred_cutoff_test_50 <- as.factor(pred_cutoff_test_50)

typeof(test_set$turnover)
typeof(pred_cutoff_test_50)

install.packages('e1071')
library(e1071)
conf_matrix_50 <- confusionMatrix(pred_cutoff_test_50, test_set$turnover)
conf_matrix_50

# Calculate turnover risk probability

install.packages('tidypredict')
library(tidypredict)

emp_risk <- emp %>%
  filter(status == 'Active') %>%
  tidypredict_to_column(final_log_model)
head(emp_risk)

emp_risk %>%
  ungroup(select(level)) %>%
  select(emp_id,fit) %>%
  top_n(5)

# Bucketing the employees
emp_risk_bucket <- emp_risk %>%
  mutate(risk_bucket = cut(fit, breaks = c(0, 0.5, 0.65, 0.80, 1),
                           labels = c('No Risk','Low Risk','Medium Risk', 'High Risk')))
emp_risk_bucket %>%
  ungroup(select(level)) %>%
  count(risk_bucket)

# Salary hike and turnover relationship for Analyst

ggplot(emp, aes(x = percent_hike))+
  geom_histogram(binwidth = 3)

emp_hike_range <- emp %>%
  filter(level == 'Analyst') %>%
  mutate(hike_range = cut(percent_hike, breaks = c(0, 8, 14, 100), include.lowest = TRUE,
                          labels = c('0 to 8%', '8% to 14%', 'Above 14%')))
hike_turnover <- emp_hike_range %>%
  group_by(hike_range) %>%
  summarise(turnover_hike = round(mean(turnover),2))
hike_turnover
"
  # A tibble: 3 x 2
  hike_range turnover_hike
<fct>              <dbl>
1 0 to 8%             0.42
2 8% to 14%           0.14
3 Above 14%           0.03
"
# Say we increase the hike range for 1st group by 4% (12%) and achieve 
# a corresponding reduction in turnover by 12% (0.42 - 0.12)

median_analyst_salary = 80000
extra_cost <- median_analyst_salary * 0.04 # For incremental hike

turnover_cost = 15000
savings <- turnover_cost * 0.12

# RoI attributing to this trade-off between hike and attrition control
RoI = (savings/extra_cost) * 100
cat(paste0('The return on the investment is ', round(RoI), '%!'))


  


