# Make sure current working directory has data.
bug_data <- read.csv("cole_arthropod_data_1946.csv")

# Make n() be equivalent to the number of rows in subsequent code.
n = nrow(bug_data)

library(ggplot2)



# Question 2
#Aim: Plot the Poisson distribution with the same mean as the sowbug counrs

#Pull out all of the times in the data table that sow bugs were counted
total_number = sum(bug_data$C_count_of_boards_with_k_sowbugs)

#take mean of all counted sow bugs 
mean = mean(bug_data$C_count_of_boards_with_k_sowbugs)

#plot line graph with the Poisson distribution with the same mean as the sowbug counts
ggplot() + 
  geom_point(aes(x = 1:n, y = (bug_data$C_count_of_boards_with_k_sowbugs)/total_number)) + 
  geom_line(aes(x = 1:n, y = (dpois(0:17,1/mean))), color = "red") + 
  scale_y_continuous(sec.axis = sec_axis(~.*total_number, name = "sowbugs"))

# Question 5
#Aim: Add a curve to the plot from question 2 showing the LGP distribution for the sowbug 

#this package is used to determine LGP using the function dLGP
install.packages('RMKdiscrete')
library(RMKdiscrete)

l2 <- .53214 #tgus us the empirically determined lamba 2 value for sowbugs
l1 <- mean*(1-l2) #this formula is an estimate for lamba 1 of sowbugs based off the empirical lamba 2 

#Pull out all the times within the data table that sowbugs were counted
total_number = sum(bug_data$C_count_of_boards_with_k_sowbugs)

#Take the mean of the counted sowbugs 
mean = mean(bug_data$C_count_of_boards_with_k_sowbugs)

#Plot a line graph of the sowbug data 
#The red line shows the Poisson distribution with the same mean as the sowbug count
#The blue line shows the LGP distribution 
ggplot() + 
  geom_point(aes(x = 1:n, y = (bug_data$C_count_of_boards_with_k_sowbugs)/total_number)) + 
  geom_line(aes(x = 1:n, y = (dpois(0:17,1/mean))), color = "red") + 
  geom_line(aes(x = 1:n, y = (dLGP(0:17, l1, l2))), color = "blue") +
  scale_y_continuous(sec.axis = sec_axis(~.*total_number, name = "sowbugs"))

