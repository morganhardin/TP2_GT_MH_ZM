# Exploring the Student Scores Dataset

# Install Packages
library(tidyverse)

# Loading the Dataset
data = read_csv("student-scores.csv", show_col_types = FALSE)

# Exploring the Dataset
view(data) # show the data in a separate tab
head(data) # gives top 6 rows
tail(data) # gives bottom 6 rows
length(data) # gives number of columns
names(data) # gives column names
str(data) # gives data types
summary(data) # gives summary of data

# Finding Unique Values Categorical Columns in Dataset
unique(data$first_name) # finding unique first names
unique(data$last_name) # finding unique last names
unique(data$email) # finding unique emails
unique(data$gender) # finding unique genders
unique(data$career_aspiration) # finding unique career aspirations

# Finding Names and Gender with Career Aspirations, Weekly Study Hours, and Math Scores
data %>%
  select("first_name", "last_name", "gender", "weekly_self_study_hours", "career_aspiration", "math_score") %>% 
  view()

# Finding Names and Gender with Weekly Study Hours and all Scores
data %>%
  select("first_name", "last_name", "gender", "weekly_self_study_hours", "math_score", "history_score", "physics_score", "chemistry_score", "biology_score", "english_score", "geography_score") %>% 
  view()

# Filter Rows: Students with Career Aspirations = Doctor or Scientist and Math Scores > 80
data %>% 
  select(first_name, last_name, career_aspiration, math_score) %>% 
  filter(career_aspiration == "Doctor" | career_aspiration == "Scientist" & math_score > 80) %>% 
  view()

# Checking for Missing Data
paste("Number of missing values: ", sum(is.na(data))) # no missing values

# GGPlot Bar Plot for Biology Scores < 50
biology_data = data %>%
  filter(biology_score < 50)
ggplot(data = biology_data, aes(x = biology_score)) + geom_bar() + 
  labs(title = "Bar Plot of Biology Scores < 50")

# GGPlot Bar Plot for Gender
ggplot(data = data, aes(x = gender)) + geom_bar() + 
  labs(title = "Bar Plot of Gender")

# GGPlot Histogram Plot for Weekly Self Study Hours
ggplot(data = data, aes(x = weekly_self_study_hours)) + 
  geom_histogram() + labs(title = "Histogram Plot of Weekly Study Hours")

# GGPlot Histogram Plot for Absence Days
ggplot(data = data, aes(x = absence_days)) + 
  geom_histogram() + labs(title = "Histogram Plot of Absence Days")

# GGPlot Box Plot for Chemistry Scores
ggplot(data = data, aes(x = chemistry_score)) + 
  geom_boxplot(fill = "green") + labs(title = "Box Plot of Chemistry Scores")

# GGPlot Box Plot for English Scores > 50 and < 85
english_data = data %>%
  filter(english_score > 50 & english_score < 85)
ggplot(data = english_data, aes(x = english_score)) + 
  geom_boxplot(fill = "blue") + 
  labs(title = "Box Plot of English Scores > 50 and < 85")

# GGPlot Density Plot for Physics and Gender
ggplot(data = data, aes(x = physics_score, color = gender, fill = gender)) + 
  geom_density(alpha = 0.30) + 
  labs(title = "Density Plot of Geography Scores and Gender")

# GGPlot Density Plot for Physics and Extracurricular Activities
ggplot(data = data, aes(x = physics_score, color = extracurricular_activities, 
                        fill = extracurricular_activities)) + 
  geom_density(alpha = 0.30) + 
  labs(title = "Density Plot of Physics Scores and Extracurricular Activities")

# GGPlot Scatter Plot Absence Days vs Weekly Study Hours
ggplot(data = data, aes(x = absence_days, y = weekly_self_study_hours, 
                        color = weekly_self_study_hours)) + geom_point() + 
  labs(title = "Scatter Plot of Absence Days vs Weekly Study Hours")

# GGPlot Scatter Plot History Scores vs Weekly Study Hours
ggplot(data = data, aes(x = history_score, y = weekly_self_study_hours)) + geom_point() + 
  labs(title = "Scatter Plot of History Scores vs Weekly Study Hours")

# GGPlot Facet Grid of Math Scores vs Weekly Study Hours by Career Aspirations
table(data$career_aspiration)
data %>%
  filter(career_aspiration %in% c("Teacher", "Writer", "Game Developer", 
                                  "Accountant")) %>%
  ggplot(aes(x = math_score, y = weekly_self_study_hours, 
             color = career_aspiration, fill = career_aspiration)) + 
  geom_point() + facet_wrap(~career_aspiration) + geom_smooth() + 
  labs(title = "Math Scores vs Weekly Study Hours by Career Aspirations")

# GGPlot Facet Grid of Math Scores vs Weekly Study Hours by Career Aspirations
table(data$career_aspiration)
data %>%
  filter(career_aspiration %in% c("Teacher", "Writer", "Game Developer", 
                                  "Accountant")) %>%
  ggplot(aes(x = english_score, y = weekly_self_study_hours, 
             color = career_aspiration, fill = career_aspiration)) + 
  geom_point() + facet_wrap(~career_aspiration) + geom_smooth() + 
  labs(title = "English Scores vs Weekly Study Hours by Career Aspirations")

# GGPlot Facet Grid of History Scores vs Weekly Study Hours by Career Aspirations
data %>%
  filter(career_aspiration %in% c("Teacher", "Writer", "Game Developer", 
                                  "Accountant")) %>%
  ggplot(aes(x = history_score, y = weekly_self_study_hours, 
             color = career_aspiration, fill = career_aspiration)) + 
  geom_point() + facet_wrap(~career_aspiration) + geom_smooth() + 
  labs(title = "History Scores vs Weekly Study Hours by Career Aspirations")

# GGPlot Facet Grid of Physics Scores vs Weekly Study Hours by Career Aspirations
data %>%
  filter(career_aspiration %in% c("Teacher", "Writer", "Game Developer", 
                                  "Accountant")) %>%
  ggplot(aes(x = physics_score, y = weekly_self_study_hours, 
             color = career_aspiration, fill = career_aspiration)) + 
  geom_point() + facet_wrap(~career_aspiration) + geom_smooth() + 
  labs(title = "Physics Scores vs Weekly Study Hours by Career Aspirations")

# GGPlot Facet Grid of Chemistry Scores vs Weekly Study Hours by Career Aspirations
data %>%
  filter(career_aspiration %in% c("Teacher", "Writer", "Game Developer", 
                                  "Accountant")) %>%
  ggplot(aes(x = chemistry_score, y = weekly_self_study_hours, 
             color = career_aspiration, fill = career_aspiration)) + 
  geom_point() + facet_wrap(~career_aspiration) + geom_smooth() + 
  labs(title = "Chemistry Scores vs Weekly Study Hours by Career Aspirations")

# GGPlot Facet Grid of Biology Scores vs Weekly Study Hours by Career Aspirations
data %>%
  filter(career_aspiration %in% c("Teacher", "Writer", "Game Developer", 
                                  "Accountant")) %>%
  ggplot(aes(x = biology_score, y = weekly_self_study_hours, 
             color = career_aspiration, fill = career_aspiration)) + 
  geom_point() + facet_wrap(~career_aspiration) + geom_smooth() + 
  labs(title = "Biology Scores vs Weekly Study Hours by Career Aspirations")

# GGPlot Facet Grid of Geography Scores vs Weekly Study Hours by Career Aspirations
table(data$career_aspiration)
data %>%
  filter(career_aspiration %in% c("Teacher", "Writer", "Game Developer", 
                                  "Accountant")) %>%
  ggplot(aes(x = geography_score, y = weekly_self_study_hours, 
             color = career_aspiration, fill = career_aspiration)) + 
  geom_point() + facet_wrap(~career_aspiration) + geom_smooth() + 
  labs(title = "Geography Scores vs Weekly Study Hours by Career Aspirations")

# Correlation Matrix
cor(data[sapply(data, is.numeric)])
