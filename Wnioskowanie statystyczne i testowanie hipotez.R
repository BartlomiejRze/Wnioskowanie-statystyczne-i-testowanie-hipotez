> head(tips)
  total_bill  tip    sex smoker day   time size
1      16.99 1.01 Female     No Sun Dinner    2
2      10.34 1.66   Male     No Sun Dinner    3
3      21.01 3.50   Male     No Sun Dinner    3
4      23.68 3.31   Male     No Sun Dinner    2
5      24.59 3.61 Female     No Sun Dinner    4
6      25.29 4.71   Male     No Sun Dinner    4
> unique(tips$sex)
[1] Female Male  
Levels: Female Male
> unique(tips$day)
[1] Sun  Sat  Thur Fri 
Levels: Fri Sat Sun Thur
> t.test(tips$tip, alternative = "two.sided", mu=2.50)

	One Sample t-test

data:  tips$tip
t = 5.6253, df = 243, p-value = 5.08e-08
alternative hypothesis: true mean is not equal to 2.5
95 percent confidence interval:
 2.823799 3.172758
sample estimates:
mean of x 
 2.998279 

> randT <- rt(30000, df=NROW(tips)-1)
> tipTTest <- t.test(tips$tip, alternative = "two.sided", mu=2.50)
> ggplot(data.frame(x = randT)) + geom_density(aes(x = x), fill = "orange", color = "blue") + geom_vline(xintercept = tipTTest$statistic, color = "red") + geom_vline(xintercept = mean(randT) + c(-2, 2) * sd(randT), linetype = 2)
> t.test(tips$tip, alternative = "greater", mu=2.50)

	One Sample t-test

data:  tips$tip
t = 5.6253, df = 243, p-value = 2.54e-08
alternative hypothesis: true mean is greater than 2.5
95 percent confidence interval:
 2.852023      Inf
sample estimates:
mean of x 
 2.998279 

> aggregate(tip ~ sex, data=tips, var)
     sex      tip
1 Female 1.344428
2   Male 2.217424
> shapiro.test(tips$tip)

	Shapiro-Wilk normality test

data:  tips$tip
W = 0.89781, p-value = 8.2e-12

> shapiro.test(tips$tip[tips$sex == "Female"])

	Shapiro-Wilk normality test

data:  tips$tip[tips$sex == "Female"]
W = 0.95678, p-value = 0.005448

> shapiro.test(tips$tip[tips$sex == "Male"])

	Shapiro-Wilk normality test

data:  tips$tip[tips$sex == "Male"]
W = 0.87587, p-value = 3.708e-10

> ggplot(tips, aes(x=tip, fill=sex)) + geom_histogram(binwidth=.5, alpha=1/2)
> ansari.test(tip ~ sex, tips)

	Ansari-Bradley test

data:  tip by sex
AB = 5582.5, p-value = 0.376
alternative hypothesis: true ratio of scales is not equal to 1

> t.test(tip ~ sex, data=tips, var.equal=TRUE)

	Two Sample t-test

data:  tip by sex
t = -1.3879, df = 242, p-value = 0.1665
alternative hypothesis: true difference in means between group Female and group Male is not equal to 0
95 percent confidence interval:
 -0.6197558  0.1074167
sample estimates:
mean in group Female   mean in group Male 
            2.833448             3.089618 
> tipSummary <- ddply(tips, "sex", summarize, tip.mean=mean(tip), tip.sd=sd(tip), Lower=tip.mean-2*tip.sd/sqrt(NROW(tip)), Upper=tip.mean + 2*tip.sd/sqrt(NROW(tip)))
> tipSummary
     sex tip.mean   tip.sd    Lower    Upper
1 Female 2.833448 1.159495 2.584827 3.082070
2   Male 3.089618 1.489102 2.851931 3.327304
> ggplot(tipSummary, aes(x = tip.mean, y = sex)) + geom_point() + geom_errorbarh(aes(xmin = Lower, xmax = Upper), height = 0.2)
> data(father.son, package='UsingR')
> head(father.son)
   fheight  sheight
1 65.04851 59.77827
2 63.25094 63.21404
3 64.95532 63.34242
4 65.75250 62.79238
5 61.13723 64.28113
6 63.02254 64.24221
> t.test(father.son$fheight, father.son$sheight, paired=TRUE)

	Paired t-test

data:  father.son$fheight and father.son$sheight
t = -11.789, df = 1077, p-value < 2.2e-16
alternative hypothesis: true mean difference is not equal to 0
95 percent confidence interval:
 -1.1629160 -0.8310296
sample estimates:
mean difference 
     -0.9969728 

> heightDiff <- father.son$fheight - father.son$sheight
> head(heightDiff)
[1]  5.27024  0.03690  1.61290  2.96012 -3.14390 -1.21967
> tipAnova <- aov(tip~day-1, tips)
> tipIntercept <- aov(tip ~ day, tips)
> tipAnova$coefficients
  dayFri   daySat   daySun  dayThur 
2.734737 2.993103 3.255132 2.771452 
> tipIntercept$coefficients
(Intercept)      daySat      daySun     dayThur 
 2.73473684  0.25836661  0.52039474  0.03671477 
> summary(tipAnova)
           Df Sum Sq Mean Sq F value Pr(>F)    
day         4 2203.0   550.8   290.1 <2e-16 ***
Residuals 240  455.7     1.9                   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
> tipsByDay <- ddply(tips, "day", plyr::summarize, tip.mean=mean(tip), tip.sd=sd(tip), Length=NROW(tip), tfrac=qnorm(p=.95)*tip.sd/sqrt(Length), Lower=tip.mean - tfrac, Upper=tip.mean + tfrac)
> ggplot(tipsByDay, aes(x=tip.mean, y=day)) + geom_point() + geom_errorbarh( aes(xmin=Lower, xmax=Upper), height=.3)
> nrow(tips)
[1] 244
> NROW(tips)
[1] 244
> nrow(tips$tip)
NULL
> NROW(tips$tip)
[1] 244