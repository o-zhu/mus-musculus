library(ggplot2)




#Question 3
weevil_data <- read.csv("mitchell_weevil_egg_data_1975.csv")
total_count <- sum(weevil_data$C_count_of_beans_with_k_eggs)
weevil_data$p_weevil <- weevil_data$C_count_of_beans_with_k_eggs/total_count
avg <- mean(weevil_data$C_count_of_beans_with_k_eggs)
weevil_data$weevil_poisson <- dpois(0:4, 1/avg)
weevil_rows <- nrow(weevil_data)
weevil_plot <- ggplot(data = weevil_data, aes(x = weevil_data$k_number_of_eggs, y = weevil_data$p_weevil)) + 
 geom_point(size = 3) + xlab("# of eggs") + ylab("probability")
weevil_plot <- weevil_plot + geom_line(data=weevil_data, aes(x=weevil_data$k_number_of_eggs, y=weevil_data$weevil_poisson),  
                             linetype='dashed', colour='#006400') +  
  geom_point(data=weevil_data, aes(x=weevil_data$k_number_of_eggs, y=weevil_data$weevil_poisson),  
             colour='#006400', shape=0, size = 3)
weevil_plot




