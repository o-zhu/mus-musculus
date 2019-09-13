#The purpose of question 1 is to plot the Poisson distribution with the same mean as the spider counts, while question 4 aims to add a curve to that plot showing the LGP distribution for the spider counts

# Make sure current working directory has data.
bug_data <- read_csv("./cole_arthropod_data_1946.csv")
#set that n() is equivalent to the number of rows for the subsequent code
n = nrow(bug_data)

library(ggplot2)

# Question 1 and 4

#select from all data only the data representing boards that have spiders present
total_number = sum(bug_data$C_count_of_boards_with_k_spiders)
#find mean number of spider counts 
mean = mean(bug_data$C_count_of_boards_with_k_spiders)

#creation of a plot with the mean number of spiders on the x axis vs. probability of finding spiders on the y axis. 
#the red line shows the Poisson distribution with the same mean as the spider count 
#the blue, dashed line shows the LGP distribution for the spider counts
ggplot() + 
  geom_point(aes(x = 1:n, y = (bug_data$C_count_of_boards_with_k_spiders)/total_number))+
   xlab("# of spiders") + ylab("Probability") + 
    geom_line(aes(x = 1:n, y = (dpois(0:17,1/mean))), color = "red") + 
     geom_line(aes(x = 1:n, y = (dLGP(0:17,1/mean, 0))), color = "blue", linetype="dashed") +
      scale_y_continuous(sec.axis = sec_axis(~.*total_number))

