library(ggplot2)

# Question 1
bug_data <- read.csv("cole_arthropod_data_1946.csv")
n = nrow(bug_data)
total_number = sum(bug_data$C_count_of_boards_with_k_spiders)
mean = mean(bug_data$C_count_of_boards_with_k_spiders)
ggplot() + 
  geom_point(aes(x = 1:n, y = (bug_data$C_count_of_boards_with_k_spiders)/total_number)) + 
  geom_line(aes(x = 1:n, y = (dpois(0:17,1/mean))), color = "red") + 
  scale_y_continuous(sec.axis = sec_axis(~.*total_number, name = "spider_data"))

# Question 2
total_number = sum(bug_data$C_count_of_boards_with_k_sowbugs)
mean = mean(bug_data$C_count_of_boards_with_k_sowbugs)
ggplot() + 
  geom_point(aes(x = 1:n, y = (bug_data$C_count_of_boards_with_k_sowbugs)/total_number)) + 
  geom_line(aes(x = 1:n, y = (dpois(0:17,1/mean))), color = "red") + 
  scale_y_continuous(sec.axis = sec_axis(~.*total_number, name = "sowbug_data"))


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


#Question 6
library(RMKdiscrete)
library(ggplot2)

weevil_data <- read.csv("mitchell_weevil_egg_data_1975.csv")
total_count <- sum(weevil_data$C_count_of_beans_with_k_eggs)
#the probability of laying k number of eggs
weevil_data$p_weevil <- weevil_data$C_count_of_beans_with_k_eggs/total_count

avg <- mean(weevil_data$C_count_of_beans_with_k_eggs)
λ2<-
  
  #get the y axis for the standard Possion distribution with λ=1/avg
  weevil_data$weevil_poisson <- dpois(0:4, 1/avg)
weevil_data$weevil_LGP <- dLGP(0:4, 1, 1/avg*(1-λ2))

weevil_rows <- nrow(weevil_data)

#draw the real data point graph
weevil_plot <- ggplot(data = weevil_data, aes(x = weevil_data$k_number_of_eggs, y = weevil_data$p_weevil)) + 
  geom_point(size = 3) + xlab("# of eggs laid at the same area") + ylab("Probability")

#draw the Poisson distribution curve
weevil_plot <- weevil_plot + 
  geom_line(data=weevil_data, aes(x=weevil_data$k_number_of_eggs, y=weevil_data$weevil_poisson), linetype='dashed', colour='#006400') +  
  geom_point(data=weevil_data, aes(x=weevil_data$k_number_of_eggs, y=weevil_data$weevil_poisson), colour='#006400', shape=0, size = 3) +
  scale_y_continuous(sec.axis = sec_axis(~.*total_number, name = "weevil_data")) 

#draw the LGP distribution curve

weevil_plot <- weevil_plot +
  geom_line(data=weevil_data, aes(x=weevil_data$k_number_of_eggs, y=weevil_data$weevil_LGP), linetype='dashed', colour='005400') +  
  geom_point(data=weevil_data, aes(x=weevil_data$k_number_of_eggs, y=weevil_data$weevil_LGP), colour='005400', shape=0, size = 3)
weevil_plot
