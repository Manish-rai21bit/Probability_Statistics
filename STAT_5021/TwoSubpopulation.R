# Clearing environment variable
rm(list=ls())

# Creating DataFrames for further analysis
Brand = c("A", "A", "A", "A", "A", "B", "B", "B", "B", "B", "B") 
Life = c(10.5, 9.6, 10.2, 9.9, 10.4, 8.9, 9.4, 9.7, 9.3, 8.8, 10.0)
df = data.frame(Brand, Life)       # df is a data frame
xbar = mean(df[df$Brand == "A",]$Life)
s1 = sd(df[df$Brand == "A",]$Life)
n1 = nrow(df[df$Brand == "A",])
ybar = mean(df[df$Brand == "B",]$Life)
s2 = sd(df[df$Brand == "B",]$Life)
n2 = nrow(df[df$Brand == "B",])
sp = sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2)/(n1 + n2 -2))

xbar
ybar
sd(df$Life)
sp

# Example 2.2
xbar = 316.2
ybar = 308.4
s1 = 22.6
s2 = 24.1
n1 = 36
n2 = 36
sp = sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2)/(n1 + n2 -2))
t = ((xbar - ybar) - 0)/(sp*sqrt((1/n1) + (1/n2)))

pval = 1- pt(t, 70)


# Example 2.3
# 1. Hypothesis on the number of Cigarettes
cigHbar = 2.57
pufHbar = 22.74
cigLbar = 2.05
pufLbar = 15.09
sp.c = 1.0583
sp.p = 10.701
n1 = 23
n2 = 22
# Calculate the realised statistics
t_c = ((cigHbar - cigLbar) - 0)/(sp.c*sqrt((1/n1) + (1/n2)))
# get the p_value
pval_c = 1- pt(t_c, n1+n2-2)

# 2. Hypothesis on the number of puffs
t_p = ((pufHbar - pufLbar) - 0)/(sp.p*sqrt((1/n1) + (1/n2)))
# get the p_value
pval_p = 1- pt(t_p, n1+n2-2)

# 3. 90% Confidence Interval
(pufHbar - pufLbar) + c(-1, 1)*qt(0.95, 43)*sp.p*sqrt(1/n1 + 1/n2)


# Example 3.3
# Clearing environment variable
rm(list=ls())

# Creating DataFrames for further analysis
Patient = c(1, 2, 3, 4, 5, 6)
Before_Insulin = c(275, 290, 328, 218, 292, 209)
After_Insulin = c(82, 95, 127, 99, 122, 88)
df = data.frame(Patient, Before_Insulin, After_Insulin)       # df is a data frame
df$insulin_diff = df$Before_Insulin - df$After_Insulin

xbar = mean(df$insulin_diff)
s = sd(df$insulin_diff)

# t statistics realization
t = (xbar - 0)/(s/sqrt(nrow(df)))
# p-value 
p_val = 1- pt(t, nrow(df) - 1)

############################################################
# 4. Solved Examples
# 4.1
# changing it to the homework question
before_bar = 30.8
after_bar = 30.7
diff_bar = 0.1
s_diff = 0.5
n = 200

t = (diff_bar - 0)/(s_diff/sqrt(n))
pval = 1 - pt(t, n-1)

# HW 5 Question
# 2.
xbar = 28.6
ybar = 18.9
s1 = 9.1
s2 = 9.8
n1 = 200
n2 = 200
sp = sqrt(((n1 - 1)*s1^2 + (n2 - 1)*s2^2)/(n1 + n2 -2))
t = ((xbar - ybar) - 0)/(sp*sqrt((1/n1) + (1/n2)))

pval = 1- pt(t, n1+n2-2)
