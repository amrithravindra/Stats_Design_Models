# Question 1

# Getting the data
df = read.table("http://www2.isye.gatech.edu/~jeffwu/book/data/onewaymuzzle.dat", header = TRUE)

# Convert Area into a factor (optional)
df$Area = factor(paste0("A",df$Area), levels = paste0("A",unique(df$Area)))

# Separating the data frame according to area
a1 <- df[1:16, 1:2]
a2 <- df[17:32, 1:2]
a3 <- df [33:48, 1:2]
a4 <- df[49:64, 1:2]

# Organizing data into better format

df1 <- as.data.frame(cbind(a1,a2,a3,a4))
df2 <- as.data.frame(df1[,c(2,4,6,8)])
colnames(df2)[1] = c("Area 0.016")
colnames(df2)[2] = c("Area 0.03")
colnames(df2)[3] = c("Area 0.044")
colnames(df2)[4] = c("Area 0.058")
df2
# As we can see, the data contains four different levels (i.e., k = 4) which represent the four different sizes of discharge holes
# We can also see that the total number of observations (i.e., N = 64) is 64 with (n = 16)

df_long = data.frame(Area = rep(c("A0.016", "A0.03", "A0.044", "A0.058"), 16), Velocity = c(t(df2)))

# Plotting the data to take get a better understanding of the data
library(ggplot2)
ggplot(df_long, aes(x = Area, y = Velocity)) + geom_boxplot() + geom_point()

# It is clear from the above plot that the muzzle velocity is inversely related to the area (i.e., the muzzle velocity decreases with an increase in area of discharge hole)

# Calculations
mean_A1 = mean(a1$Velocity)
mean_A1

mean_A2 = mean(a2$Velocity)
mean_A2

mean_A3 = mean(a3$Velocity)
mean_A3

mean_A4 = mean(a4$Velocity)
mean_A4

grand_mean = mean(c(mean_A1,mean_A2,mean_A3,mean_A4))
grand_mean

TreatmentSumofSquares = 16*(mean_A1 - grand_mean)^2 + 16*(mean_A2 - grand_mean)^2 + 16*(mean_A3 - grand_mean)^2 + 16*(mean_A4 - grand_mean)^2
TreatmentSumofSquares

TotalSumofSquares = sum((df_long$Velocity - grand_mean)^2)
TotalSumofSquares

ResidualSumofSquares = TotalSumofSquares - TreatmentSumofSquares
ResidualSumofSquares

# F-test
F_obs = (TreatmentSumofSquares/(4-1))/(ResidualSumofSquares/(64 - 4))
F_obs
# in this case (k-1) = 3 and (N-1) = 60 which are the degrees of freedom associated with the terms

# p_value
1 - pf(F_obs,3,60)
# A very small p value suggesting that we can reject the null hypothesis

# anova using R functions for verifitcation with calculated values
anova(lm(Velocity ~ Area, df_long))
# We see that our calculated values match closely with the values obtained from anova table using R functions

# Calculations for pairwise comparison of each of the four discharge hole areas (there are six pairs in total)
MSE = ResidualSumofSquares/60 
MSE

sigma_hat = sqrt(MSE)
sigma_hat

t_A1_vs_A2 = (mean_A2 - mean_A1)/(sigma_hat * sqrt(1/16 + 1/16))
t_A1_vs_A3 = (mean_A3 - mean_A1)/(sigma_hat * sqrt(1/16 + 1/16))
t_A1_vs_A4 = (mean_A4 - mean_A1)/(sigma_hat * sqrt(1/16 + 1/16))
t_A2_vs_A3 = (mean_A3 - mean_A2)/(sigma_hat * sqrt(1/16 + 1/16))
t_A2_vs_A4 = (mean_A4 - mean_A2)/(sigma_hat * sqrt(1/16 + 1/16))
t_A3_vs_A4 = (mean_A4 - mean_A3)/(sigma_hat * sqrt(1/16 + 1/16))

c(t_A1_vs_A2, t_A1_vs_A3, t_A1_vs_A4, t_A2_vs_A3, t_A2_vs_A4, t_A3_vs_A4)
# We are only interested in the absolute values of each pairwise comparison. We will compare these with our threshold values

# Threshold for regular t-test
qt(1 -0.05/2,60)
# Threshold for t-test Bonferroni method
qt(1 -0.05/(2*6),60)
# Threshold for Tukey's method
1/sqrt(2)*qtukey(0.95,4,60)

# Based on the above threshold values and pairwise comparisons we can reject the null hypothesis and say that A1&A3, A1&A4, A2&A4 are all different from each other
# This is because the absolute values of these pair of comparisons is greater than the threshold values we obtained from the Bonferroni and Tukey's methods
# These two methods are basically t-statistic tests but with a higher threshold so that we avoid the cases where we end up rejecting a null hypothesis when it is actually true

# Bonferroni's confidence intervals
# Confidence interval for pairwise comparison of Area 1 (i.e., 0.16) and Area 3 (i.e., 0.044)
mean_A3 - mean_A1 + qt(1 -0.05/(2*6),60) * (sigma_hat * sqrt(1/16 + 1/16))
mean_A3 - mean_A1 - qt(1 -0.05/(2*6),60) * (sigma_hat * sqrt(1/16 + 1/16))

# Confidence interval for pairwise comparison of Area 1 (i.e., 0.16) and Area 4 (i.e., 0.058)
mean_A4 - mean_A1 + qt(1 -0.05/(2*6),60) * (sigma_hat * sqrt(1/16 + 1/16))
mean_A4 - mean_A1 - qt(1 -0.05/(2*6),60) * (sigma_hat * sqrt(1/16 + 1/16))

# Confidence interval for pairwise comparison of Area 2 (i.e., 0.03) and Area 4 (i.e., 0.058)
mean_A4 - mean_A2 + qt(1 -0.05/(2*6),60) * (sigma_hat * sqrt(1/16 + 1/16))
mean_A4 - mean_A2 - qt(1 -0.05/(2*6),60) * (sigma_hat * sqrt(1/16 + 1/16))



# Tukey's confidence intervals
# Confidence interval for pairwise comparison of Area 1 (i.e., 0.16) and Area 3 (i.e., 0.044)
mean_A3 - mean_A1 + 1/sqrt(2) * qtukey(1-0.05,4,60) * (sigma_hat * sqrt(1/16 + 1/16))
mean_A3 - mean_A1 - 1/sqrt(2) * qtukey(1-0.05,4,60) * (sigma_hat * sqrt(1/16 + 1/16))

# Confidence interval for pairwise comparison of Area 1 (i.e., 0.16) and Area 4 (i.e., 0.058)
mean_A4 - mean_A1 + 1/sqrt(2) * qtukey(1-0.05,4,60) * (sigma_hat * sqrt(1/16 + 1/16))
mean_A4 - mean_A1 - 1/sqrt(2) * qtukey(1-0.05,4,60) * (sigma_hat * sqrt(1/16 + 1/16))

# Confidence interval for pairwise comparison of Area 2 (i.e., 0.03) and Area 4 (i.e., 0.058)
mean_A4 - mean_A2 + 1/sqrt(2) * qtukey(1-0.05,4,60) * (sigma_hat * sqrt(1/16 + 1/16))
mean_A4 - mean_A2 - 1/sqrt(2) * qtukey(1-0.05,4,60) * (sigma_hat * sqrt(1/16 + 1/16))



