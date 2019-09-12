# Make sure current working directory has data.
bug_data <- read_csv("./cole_arthropod_data_1946.csv")
n = nrow(bug_data)

library(ggplot2)

# Question 1
total_number = sum(bug_data$C_count_of_boards_with_k_spiders)
mean = mean(bug_data$C_count_of_boards_with_k_spiders)
ggplot() + 
  geom_point(aes(x = 1:n, y = (bug_data$C_count_of_boards_with_k_spiders)/total_number)) + 
  geom_line(aes(x = 1:n, y = (dpois(0:17,1/mean))), color = "red") + 
  scale_y_continuous(sec.axis = sec_axis(~.*total_number, name = "spider_data"))
