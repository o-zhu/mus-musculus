# Make sure current working directory has data.
bug_data <- read_csv("./cole_arthropod_data_1946.csv")
n = nrow(bug_data)

library(ggplot2)

# Question 1 and 4
total_number = sum(bug_data$C_count_of_boards_with_k_spiders)
mean = mean(bug_data$C_count_of_boards_with_k_spiders)
ggplot() + 
  geom_point(aes(x = 1:n, y = (bug_data$C_count_of_boards_with_k_spiders)/total_number))+
  xlab("# of spiders") + ylab("Probability") + 
  geom_line(aes(x = 1:n, y = (dpois(0:17,1/mean))), color = "red") + 
  geom_line(aes(x = 1:n, y = (dLGP(0:17,1/mean, 0))), color = "blue", linetype="dashed") +
  scale_y_continuous(sec.axis = sec_axis(~.*total_number))

