# Question 3

df = read.table("http://www2.isye.gatech.edu/~jeffwu/book/data/PackingMachine.dat", header = FALSE, skip = 1)
names(df) = c("MC1", "MC2", "MC3")
df
# As we can see, in this case k = 3 as we have three different machines and N = 60
# We also see that there are n observations of each level (i.e., n = 20)

# Organizing and plotting the data to get a better understanding
df_long = data.frame(Machine = rep(c("Machine 1", "Machine 2", "Machine 3"), 20), Performance = c(t(df)))

library(ggplot2)
ggplot(df_long, aes(x = Machine, y = Performance)) + geom_boxplot() + geom_point()
# Clearly from the plot, the machine performances seem to be quite different. Machine 2 seems to have the highest performance. We can also see that the average performance value for this machine is higher.

# Calculations
mean_MC1 = mean(df$MC1)
mean_MC1

mean_MC2 = mean(df$MC2)
mean_MC2

mean_MC3 = mean(df$MC3)
mean_MC3

grand_mean = mean(c(mean_MC1,mean_MC2,mean_MC3))
grand_mean

TreatmentSumofSquares = 20*(mean_MC1 - grand_mean)^2 + 20*(mean_MC2 - grand_mean)^2 + 20*(mean_MC3 - grand_mean)^2 
TreatmentSumofSquares

TotalSumofSquares = sum((df_long$Performance - grand_mean)^2)
TotalSumofSquares

ResidualSumofSquares = TotalSumofSquares - TreatmentSumofSquares
ResidualSumofSquares

# F-test
F_obs = (TreatmentSumofSquares/(3-1))/(ResidualSumofSquares/(60 - 3))
F_obs
# in this case (k-1) = 2 and (N-1) = 57 which are the degrees of freedom associated with the terms

# p_value
1 - pf(F_obs,2,57)
# A very small p value suggesting that we can reject the null hypothesis

# anova using R functions for verifitcation with calculated values
anova(lm(Performance ~ Machine, df_long))
# We see that our calculated values match closely with the values obtained from anova table using R functions

# Calculations for pairwise comparison of each of the three machines (there are three pairs in total)
MSE = ResidualSumofSquares/57
MSE

sigma_hat = sqrt(MSE)
sigma_hat

t_MC1_vs_MC2 = (mean_MC2 - mean_MC1)/(sigma_hat * sqrt(1/20 + 1/20))
t_MC1_vs_MC3 = (mean_MC3 - mean_MC1)/(sigma_hat * sqrt(1/20 + 1/20))
t_MC2_vs_MC3 = (mean_MC3 - mean_MC2)/(sigma_hat * sqrt(1/20 + 1/20))


c(t_MC1_vs_MC2, t_MC1_vs_MC3, t_MC2_vs_MC3)
# We are only interested in the absolute values of each pairwise comparison. We will compare these with our threshold values

# Threshold for regular t-test
qt(1 -0.05/2,57)
# Threshold for t-test Bonferroni method
qt(1 -0.05/(2*3),57)
# Threshold for Tukey's method
1/sqrt(2)*qtukey(0.95,3,57)
# Based on the above threshold values and pairwise comparisons we can reject the null hypothesis and say that Machines 1&2 as well as Machines 2&3 are different from each other
# This is because the absolute values of these pairs is greater than the threshold values we obtained from the Bonferroni and Tukey's methods
# These two methods are basically t-statistic tests but with a higher threshold so that we avoid the cases where we end up rejecting a null hypothesis when it is actually true

# Bonferroni's confidence intervals
# Confidence interval for pairwise comparison of perfromances of Machine 1 and Machine 2
mean_MC2 - mean_MC1 + qt(1 -0.05/(2*3),57) * (sigma_hat * sqrt(1/20 + 1/20))
mean_MC2 - mean_MC1 - qt(1 -0.05/(2*3),57) * (sigma_hat * sqrt(1/20 + 1/20))

# Confidence interval for pairwise comparison of perfromances of Machine 1 and Machine 3
mean_MC3 - mean_MC1 + qt(1 -0.05/(2*3),57) * (sigma_hat * sqrt(1/20 + 1/20))
mean_MC3 - mean_MC1 - qt(1 -0.05/(2*3),57) * (sigma_hat * sqrt(1/20 + 1/20))

# Confidence interval for pairwise comparison of perfromances of Machine 2 and Machine 3
mean_MC3 - mean_MC2 + qt(1 -0.05/(2*3),57) * (sigma_hat * sqrt(1/20 + 1/20))
mean_MC3 - mean_MC2 - qt(1 -0.05/(2*3),57) * (sigma_hat * sqrt(1/20 + 1/20))



# Tukey's confidence intervals
# Confidence interval for pairwise comparison of perfromances of Machine 1 and Machine 2
mean_MC2 - mean_MC1 + 1/sqrt(2) * qtukey(1-0.05,3,57) * (sigma_hat * sqrt(1/20 + 1/20))
mean_MC2 - mean_MC1 - 1/sqrt(2) * qtukey(1-0.05,3,57) * (sigma_hat * sqrt(1/20 + 1/20))

# Confidence interval for pairwise comparison of perfromances of Machine 1 and Machine 3
mean_MC3 - mean_MC1 + 1/sqrt(2) * qtukey(1-0.05,3,57) * (sigma_hat * sqrt(1/20 + 1/20))
mean_MC3 - mean_MC1 - 1/sqrt(2) * qtukey(1-0.05,3,57) * (sigma_hat * sqrt(1/20 + 1/20))

# Confidence interval for pairwise comparison of perfromances of Machine 2 and Machine 3
mean_MC3 - mean_MC2 + 1/sqrt(2) * qtukey(1-0.05,3,57) * (sigma_hat * sqrt(1/20 + 1/20))
mean_MC3 - mean_MC2 - 1/sqrt(2) * qtukey(1-0.05,3,57) * (sigma_hat * sqrt(1/20 + 1/20))

