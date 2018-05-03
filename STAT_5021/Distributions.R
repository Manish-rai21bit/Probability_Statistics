v =vector()

# X = (X=0) + (X=1) + (X=2) + ... + (X=30)
for (i in c(1:31))
{
   v[i] = dbinom(size = 30, prob = 0.5, x = i-1)
}

v

v[1]+v[2]+v[3]+v[4]+v[5]+v[6]


# for normal distributions
qnorm(p = 0.975, mean = 66, sd = 5) - qnorm(p = 0.025, mean = 66, sd = 5)