# chisq.test() to test group population frequencies
	# Are the frequencies of cells in a given population any different than what you'd expect from a random sample of the total, on the basis of their totals. 
	# chisq.test() generally _operates on a table_

	# e.g. Looking at segment populations in a group vs normal distribution
		tmp.tab<-table(rep(c(1:4), times=c(25,25,25,20)))
		tmp.tab
			# 1  2  3  4 
			# 25 25 25 20 
		chisq.test(tmp.tab)
			# Chi-squared test for given probabilities
			# data:  tmp.tab
			# X-squared = 0.78947, df = 3, p-value = 0.852

			# The above tells us that there is an 85% probability of seeing the above result based on the null hypothesis - that the data were randomly sampled from a large population where the values 1:4 are equally distributed.

			# Generally, a p-value of 0.1 or 0.05 suggests that there is a significant difference between groups. P value of <=0.05 means that we can reject the null hypothesis and say with 95% confidence that we'd expect to see similar distribution between groups over the entire population based on this sample.

			# Significance tests are sensitive to both the observed sample and the sample size - the smaller the sample, the lower the p-value will relatively be for the same proportional distributions.

	# Checking for relationships between factors
		table(seg.df$subscribe, seg.df$ownHome)    			        
		  #        ownNo ownYes
		  # subNo    433    187
		  # subYes   102     28
		chisq.test(table(seg.df$subscribe, seg.df$ownHome))
			# Pearson's Chi-squared test with Yates' continuity correction
			# data:  table(seg.df$subscribe, seg.df$ownHome)
			# X-squared = 3.4972, df = 1, p-value = 0.06147

		# Here, the null hypothesis is that the factors ownHome and subscribe have no relationship - that the counts shown in table() are as would be expected given the marginal proportions.

	# Testing Observed proportions (binom.test())
		# synatax: binom.test(successes, trials, probability)
		binom.test(12, 20, p=0.5)
				# Exact binomial test
				# data:  12 and 25
				# number of successes = 12, number of trials = 25, p-value = 1
				# alternative hypothesis: true probability of success is not equal to 0.5
				# 95 percent confidence interval:
				#  0.277968 0.686943
				# sample estimates:
				# probability of success 
				#                   0.48 

		# For the above - we're asking R if the observed proportions in the sample are statistically significant versus a population of equal representation.

# About confidence Intervals
	# If you have a population of 20 - 1 group with 5 adding 1 group with 15 members, and you get a 95% confidence interval of 15% - 75%, the 15-75 is describing the range of possible answers you would expect to see 95% of the time in random samples of the same size, assuming that the large/infinite total population is distributed the same as the initial sample.

	# So generally, you want to check within a ~95% confidence interval that further samples based on the result of your sample will fall within the ranges you'd like them to.

# Back to confidence intervals / group populations

	# Testing the probability of seeing a given range, provided a true split of 0.5
	sum(dbinom(8:12, 20, p=0.5))
		# [1] 0.736824
		# This returns the sum of the probabilities of seeing each individual population - so the total probability of seeing a result somewhere within the range.

	# Using the less conservative Agresti-Coull method to estimate population proportions
	binom.confint(12,20,method='ac')
		# method  x  n mean     lower     upper
		# 1 agresti-coull 12 20  0.6 0.3860304 0.7817446
		# Runs the Agresti-Couli method - others including Bayesian can be specified.
		# Tells us that the likely true population still includes 0.5 so the observation doesn't prove a probable deviation from normal distribution.

	# Using binom.confint() again: since we observed 0 mixed groups out of 20, what is the likely distribution of mixed groups in the population?
	binom.confint(0,20,method="ac")
		# method x  n mean      lower     upper
		# 1 agresti-coull 0 20    0 -0.0286844 0.1898096
		# The above tells us that given this observation, the likely occurrence of mixed groups is 0-19% of the total population.

# Testing group means to compare differences
	# Before running - inspect using histograms / boxplots and check the distribution. Many of the tests below assume a normal distribution and do not account for outliers.
		 with(seg.df, hist(income[ownHome=="ownYes"]))
		 with(seg.df, hist(income[ownHome=="ownNo"]))

	# Running t.test to understand if income is explained by home ownership
	t.test(income ~ ownHome, data=seg.df)
		# Welch Two Sample t-test

		# data:  income by ownHome
		# t = -6.2791, df = 323.16, p-value = 1.097e-09
		# alternative hypothesis: true difference in means is not equal to 0
		# 95 percent confidence interval:
		#  -14038.841  -7340.402
		# sample estimates:l
		#  mean in group ownNo mean in group ownYes 
		#             30026.30             40715.92 

		# In the results above:
			# p value tells us that the null hypothesis of no difference in means between the 2 groups can be ignored.
			# 95 percent confidence interval gives us the range of difference between the means of the 2 groups. Since this does not include 0, it's likely that there is a significant difference in the mean incomes of the 2 groups in the wider population.

	# Running a second t.test to look at the difference in income by home ownership for only the Travelers segment.
	t.test(income ~ ownHome, data=subset(seg.df, Segment=="Travelers"))
		# Welch Two Sample t-test

		# data:  income by ownHome
		# t = 2.0256, df = 68.844, p-value = 0.04668
		# alternative hypothesis: true difference in means is not equal to 0
		# 95 percent confidence interval:
		#    134.4615 17683.4517
		# sample estimates:
		#  mean in group ownNo mean in group ownYes 
		#             70521.06             61612.11 

# ANOVA
	# Testing group means with ANOVA
		# ANOVA - compares the mean of multiple groups - observes the variance within each group vs group mean and compares to the variance between groups / means.

		# Basic ANOVA:
			aov(formula, data) # to set up model
			anova(model) # to display standard ANOVA summary

		# Example 1 - income by ownHome
			seg.aov.own<-aov(income ~ ownHome, data=seg.df)
			anova(seg.aov.own)
				# Analysis of Variance Table
				# Response: income
				#            Df     Sum Sq    Mean Sq F value    Pr(>F)    
				# ownHome     1 1.7525e+10 1.7525e+10  48.676 6.655e-12 ***
				# Residuals 748 2.6930e+11 3.6003e+08                      
				# ---
				# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				# Pr(>F) gives us the p-value, which is statistically significant in this case - telling us that the difference in income between homeowners and non-homeowners is unlikely due to chance.

		# Example 2 - income by Segment
			seg.aov.seg<-aov(income ~ Segment, data=seg.df)
			anova(seg.aov.seg)


				# Response: income
				#            Df     Sum Sq    Mean Sq F value    Pr(>F)    
				# Segment     3 2.1852e+11 7.2841e+10  795.54 < 2.2e-16 ***
				# Residuals 746 6.8305e+10 9.1562e+07                      
				# ---
				# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				# In this instance, there is again a strong relationship between the Segments and income. 

		# Example 3 - testing for both income and Segment as indicators
			anova(aov(income ~ Segment + ownHome, data=seg.df))
				# Analysis of Variance Table
				# Response: income
				#            Df     Sum Sq    Mean Sq  F value  Pr(>F)    
				# Segment     3 2.1852e+11 7.2841e+10 798.5782 < 2e-16 ***
				# ownHome     1 3.5140e+08 3.5140e+08   3.8525 0.05004 .  
				# Residuals 745 6.7954e+10 9.1213e+07                     
				# ---
				# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				# The above tells us that Segment is a very good predictor of income (p-value very low) and ownHome is good, but with p-value 0.05, not nearly as good as Segment. In the book, ownHome has a p-value of 0.57 and therefore is not a good enough indicator - the effects are already captured by Segment, which is a good predictor of home ownership and income.

		# Example 4 - maybe home ownership is related to income in some segments but not others - the interaction effect model, using *
			anova(aov(income ~ Segment * ownHome, data=seg.df))
				# Analysis of Variance Table
				# Response: income
				#                  Df     Sum Sq    Mean Sq  F value    Pr(>F)    
				# Segment           3 2.1852e+11 7.2841e+10 808.8896 < 2.2e-16 ***
				# ownHome           1 3.5140e+08 3.5140e+08   3.9022  0.048592 *  
				# Segment:ownHome   3 1.1364e+09 3.7880e+08   4.2065  0.005793 ** 
				# Residuals       742 6.6817e+10 9.0050e+07                       
				# ---
				# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				# The above tells us that Segment alone is the best preditor of income - home ownership and Segment+home ownership combined are good predictors but not as good as Segment alone.

	# Model comparison with ANOVA
		# We can also pass in multiple aov() arguments, to compare different models against each other.
		# You can *only* compare models where those using sub-segments could be pieced back together again to formulate the makeup of the more general model - i.e. they are nested.
		anova(aov(income ~ Segment, data=seg.df), aov(income ~ Segment + ownHome, data=seg.df))	
			# Analysis of Variance Table
			# Model 1: income ~ Segment
			# Model 2: income ~ Segment + ownHome
			#   Res.Df        RSS Df Sum of Sq      F  Pr(>F)  
			# 1    746 6.8305e+10                              
			# 2    745 6.7954e+10  1 351398018 3.8525 0.05004 .
			# ---
			# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			# The above tells us that model 2 is a significantly better indicator of income than model 1 - but in the book, since the p-value is >0.05, it isn't!

	# Visualising group means with the multcomp package
		library(multcomp)
		
		# First usage - demonstrating that by default the formula has an intercept term - in this case, 'moving up' segment. This can be hard for stakeholders to understand, so we remove it in the next formula.
		seg.aov<-aov(income ~ Segment, data=seg.df)
		glht(seg.aov)
			#  General Linear Hypotheses
			# Linear Hypotheses:
			#                        Estimate
			# (Intercept) == 0          50108s
			# SegmentSuburb Mix == 0     4926
			# SegmentTravelers == 0     14622
			# SegmentUrban hip == 0    -28851

		# Second usage - removing the intercept by passing -1 as an argument
		seg.aov<-aov(income ~ -1 + Segment, data=seg.df)
		glht(seg.aov)
			# General Linear Hypotheses
			# Linear Hypotheses:
			#                        Estimate
			# SegmentMoving up == 0     50108
			# SegmentSuburb Mix == 0    55034
			# SegmentTravelers == 0     64730
			# SegmentUrban hip == 0     21257

		# This spits out the means for each segment - but if we plot it, it gets more interesting.
		
		# Plotting the aov with glht
			par(mar=c(6,10,2,2))
			plot(glht(seg.aov),
				xlab="Income",
				main="Average income by Segment (95% intervals)")

			# As a result, we can see clear and significant discrepancies in the average income by segment.

	# Variable selection in ANOVA: stepwise modelling
		# We can build the best model by iteratively adding / subtracting variables and seeing if this changes the effectiveness.
		# Backward: starting with large set of variables, progressively removing.
		# Forward: starting with few and stepping forward.

		seg.aov.step<-step(aov(income ~ ., data=seg.df)) # here, the . after ~ tells R to use all other variables in the model (except the response variable, income).

		# The output of this is to run through each of the variables and stop when the best model has been reached - this will be at the bottom of the output.

# Bayesian ANOVA
	# Too advanced - not necessary for now

# Key points from the chapter
	# chisq.test() to test distributions in a table
	# binom.test() to test proportions
	# t.test() to test between the means of 2 groups
	# ANOVA to test differences among several groups
	# 	Fit the basic model with aov()
	# 	common summary statistics with anova()
	# 	anova() can also compare 2 or more nested anova() models
	# 	step() for stepwise models evaluating a list of variables
	# glht() plots confidence intervals for ANOVA models
	

	





