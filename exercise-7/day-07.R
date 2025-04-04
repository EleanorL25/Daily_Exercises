# Eleanor Lindsey
# 02/18/2025
# The purpose of this document/assignment is to gain familiarity with ggplot2
library(tidyverse)
#Question 1
url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
covid <- read_csv(url)

head(covid, 6)

top_states <- c("California", "Texas", "Florida", "New York", "Illinois", "Pennsylvania")

filtered_data <- covid %>%
  filter(state %in% top_states) %>% 
  group_by(state, date) %>%         
  summarise(total_cases = sum(cases, na.rm = TRUE)) %>%  
  ungroup()  

ggplot(filtered_data, aes(x = date, y = total_cases, color = state)) +
  geom_line() +
  facet_wrap(~ state, scales = "free_y") +
  labs(title = "COVID-19 Cases Over Time in Top 6 States",
       x = "Date",
       y = "Number of Cases") +
  theme_minimal()
#Question 2
daily_total_cases <- covid %>%
  group_by(date) %>%
  summarise(total_cases = sum(cases, na.rm = TRUE))

daily_cases_plot <- ggplot(daily_total_cases, aes(x = date, y = total_cases)) +
  geom_col(fill = "lavender") +
  labs(title = "Daily Total COVID-19 Cases in the USA",
       x = "Date",
       y = "Total Cases") +
  theme_minimal()

print(daily_cases_plot)