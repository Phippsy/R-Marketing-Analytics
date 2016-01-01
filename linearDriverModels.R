# Loading in the data
	sat.df <- read.csv("http://r-marketing.r-forge.r-project.org/data/rintro-chapter7.csv")

# Inspecting the data
	summary(sat.df)
	library(gpairs)

	# Extremely useful multi-faceted vis comparing variables using gpairs
	gpairs(sat.df)
		# From this, we can see normal distribution in most variables except distance - so we might need to visualise distance in a different way - e.g. below

	# Using a log-scale for distance
	sat.df$logdistance<-log(sat.df$distance)
	gpairs(sat.df)
		# When we inspect the result of the above, we can see medium positive correlation between many of the satisfaction ratings. Overly correlated ratings can be a problem as it may suggest that respondents are not rating items individually, but are being influenced by a halo effect.

		# In this case, we check the correlation using corrplot, below.

# Investigating correlations
	# Checking the correlation strength between satisfaction ratings using corrplot()
	library(corrplot)
	corrplot.mixed(cor(sat.df[c(2, 4:9)]), upper='ellipse')
		# By inspecting the values here, we can see that none of them exceed 0.8 (very strong correlation - almost identical) and can proceed with a linear model.

	# Inspecting individual variable influences on overall satisfaction using plot()
	plot(overall ~ rides, data=sat.df,
		xlab="Ride Satisfaction",
		ylab="Overall Satisfation",
		main="Ride versus Overall satisfaction")

	# Creating a linear model to describe the relationship between rides & overall
	lm(overall ~ rides, data=sat.df)
		# Call:
		# lm(formula = overall ~ rides, data = sat.df)

		# Coefficients:
		# (Intercept)        rides  
		#     -94.962        1.703  
		
		# This output gives us the linear model for the relationship between overall and rides.
			# -94.962 is the y-axis intercept
			# 1.703 is the multiplier

			# So e.g. if a customer rates rides as 65, we can predict that their overall satisfaction rating will be -94.962 + 1.703 * 65 (=66.8).

	# Visualising the linear model with lm()
	# Assign the model to a variable m1
	m1<-lm(overall ~ rides, data=sat.df)

	# Redraw the plot
		plot(overall ~ rides, data=sat.df,
		xlab="Ride Satisfaction",
		ylab="Overall Satisfation",
		main="Ride versus Overall satisfaction")

	# Add the model line in blue
	abline(m1, col='blue')

	# Inspecting the properties of the m1 variable
	str(m1)
	summary(m1)
		# Output
			# Call:
			# lm(formula = overall ~ rides, data = sat.df)

			# Residuals:
			#     Min      1Q  Median      3Q     Max 
			# -33.597 -10.048   0.425   8.694  34.699 

			# Coefficients:
			#             Estimate Std. Error t value Pr(>|t|)    
			# (Intercept) -94.9622     9.0790  -10.46   <2e-16 ***
			# rides         1.7033     0.1055   16.14   <2e-16 ***
			# ---
			# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			# Residual standard error: 12.88 on 498 degrees of freedom
			# Multiple R-squared:  0.3434,	Adjusted R-squared:  0.3421 
			# F-statistic: 260.4 on 1 and 498 DF,  p-value: < 2.2e-16

		# Explanation of the items above
			# coefficients - contains the coefficient data
				# Std. Error column shows uncertainty in the coefficient estimate, under the assumption that the data are a random sample of a larger population. 
				# t value, p-value, 'Pr(>|t)' and significance codes indicate a wald test - which assesses whether the coefficient is significantly different than zero. 
				# The confidence interval of 95% for the coefficient estimate is that it will fall within +- 1.96 std.error - so we are confident that the coefficient for rides (see line 37) is 1.495 - 1.910.

			confint(m1)
				# 		            2.5 %     97.5 %
				# (Intercept) -112.800120 -77.124371
				# rides          1.495915   1.910656
				
				# Reports the confidence intervals - that we just calculated above

			# Residuals section tells us how closely the data follows the best fit line. A residual is the difference between the model-predicted value of a point and its actual value.
			# In the summary of m1, the residuals are quite wide - -33 to +35, meaning that the values can vary in proximity. The first and third quartiles are symmetrical, which is a good sign that the model is unbiased.

			# The final section tells us how well the model fits the data. 
				# Residual standard error: standard +- of residuals. Similar to sd(m1$residuals)
				# R-squared is a measure of how much variation in the dependent variable is captured by the model. Above, R-squared of 0.34 means that with this model, about a third of the variation in overall satisfaction is explained by variation in satisfaction with rides.
				# For linear models using a single predictor, r-squared is the same as the pearson correlation figure ^ 2. 
	# Checking model fit
		# Linear models are easy to fit - the line will go somewhere. You need to check if the model is reasonable.

		# Checking that relationships are linear
			x<-rnorm(500)
			y<-x^2 + rnorm(500)
			toy.model<-lm(y~x)
			summary(toy.model)
			# If we read the summary, including R-squared, it looks like x and y are not correlated. 

			plot(y~x)
			abline(toy.model)
			# Here we can examine the curvature of the relationship which is completely missed by the linear model.

			# Examining the model's fitted values versus residuals to check if prediction errors are normally distributed.
			plot(toy.model$fitted.values, toy.model$residuals)
			# The model under-predicts far from the centre of the distribution and over-predicts close to it. To deal with this, you likely need to transform x - as described previously with common transformations. 

		# Inspecting a diagnostic plot for satisfaction driver data.
			par(mfrow=c(2,2))
			plot(m1)
				# This resulting plot shows:
					# Upper-left: Fitted values versus residuals. Since there is no obvious pattern of bias, the residuals are due to random error and the model can be seen as adequate.
					# Lower-left: Plots the fitted values vs sqrt of residuals. The lack of pattern again indicates adequate model.
					# A common pattern in residual plots is a cone or funnel, where the range of errors gets larger for larger fitted values. This is called heteroskedasticity and indicates poor fit.
					# FIX: sometimes transforming the variable will resolve heteroskedasticity.
					# Upper right: Normal QQ plot. Compares the values that residuals would be expected to take if they are normally distributed versus actual values. Points should fall close to the line when the model is appropriate.
					# Bottom right: helps identify potential outliers. This plot shows the leverage of each point - a measure of how much influence the point has on model coefficients. If a point has high residual and high leverage, it has undue influence on the model. 
				# In our example, 3 points have been automatically labelled because they have undue influence on the model. We can inspect these points and check for e.g. data entry errors, and consider removing them if we are certain that they are invalid.
				
				sat.df[c(57,129,295),]

# Fitting linear models with multiple predictors
	# Calling lm() with all variables
	m2<-lm(overall ~ rides + games + wait + clean, data=sat.df)	
	summary(m2)
	# Output
		# Call:
		# lm(formula = overall ~ rides + games + wait + clean, data = sat.df)

		# Residuals:
		#     Min      1Q  Median      3Q     Max 
		# -29.944  -6.841   1.072   7.167  28.618 

		# Coefficients:
		#               Estimate Std. Error t value Pr(>|t|)    
		# (Intercept) -131.40919    8.33377 -15.768  < 2e-16 ***
		# rides          0.52908    0.14207   3.724 0.000219 ***
		# games          0.15334    0.06908   2.220 0.026903 *  
		# wait           0.55333    0.04781  11.573  < 2e-16 ***
		# clean          0.98421    0.15987   6.156 1.54e-09 ***
		# ---
		# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

		# Residual standard error: 10.59 on 495 degrees of freedom
		# Multiple R-squared:  0.5586,	Adjusted R-squared:  0.5551 
		# F-statistic: 156.6 on 4 and 495 DF,  p-value: < 2.2e-16

		# Since R-squared in the above model has increased to 0.5586 (vs 0.34), we can see that the model has improved.
		# Residual standard error is down to 10 from 12, meaning that predictions are more accurate. 
		# Looking at residuals (lines 140-141), the min/max & 1q/2q are more symmetrical now, suggesting good balance.
		# A good next step would be to inspect the model using plot and manually check - like we did for the previous model.
		par(mfrow=c(2,2))
		plot(m2)

		# Examining the model coefficients
		library(coefplot)
		coefplot(m2, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
			ylab="Rating of Feature",
			xlab="Association with overall satisfaction")

			# This marvellous visual shows the coefficients for each variable, to a width of 1.96 standard errors, which corresponds to a 95% confidence interval. So we can quickly see which items have the strongest pull on the overall satisfaction rating.

	# Comparing models
		# We can compare the r squared value
			summary(m1)$r.squared
				# [1] 0.3433799
			summary(m2)$r.squared
				# [1] 0.558621

				# This suggests that m2 accounts for more of the variation. 

		# However models with more predictors usually have a higher r^2, so we can adjust the r^2 value
			summary(m1)$adj.r.squared
				# 1] 0.3420614
			summary(m2)$adj.r.squared
				# [1] 0.5550543

				# Even with the adjustment, model 2 explains more of the variation. 

		# We can also plot the fitted versus actual values for each.
			plot(sat.df$overall, fitted(m1), col='red',
				xlim=c(0,100), ylim=c(0,100),
				xlab="Actual overall satisfaction", ylab="Fitted overall satisfaction")
			points(sat.df$overall, fitted(m2), col='blue')
			legend('topleft', legend=c("model 1", "model 2"),
				col=c("red","blue"), pch=1)

			# On this visual - perfect fitting models would fall along a 45 degree line on the resulting plot. 

		# Using ANOVA to compare models
		anova(m1,m2)
			# Analysis of Variance Table
			# Output
				# Model 1: overall ~ rides
				# Model 2: overall ~ rides + games + wait + clean
				#   Res.Df   RSS Df Sum of Sq      F    Pr(>F)    
				# 1    498 82612                                  
				# 2    495 55532  3     27080 80.463 < 2.2e-16 ***
				# ---
				# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			# The above tells us that model 2 gives the better fit.

# Using a model to make predictions
	# We can predict the overall satisfaction rating by using the model and our created coefficients.
	
	# e.g. if a customer rates 4 separate aspects as 100 each, we can use the model:
	coef(m2)["(Intercept)"] + coef(m2)["rides"]*100 + coef(m2)["games"]*100 + 
		coef(m2)["wait"]*100 + coef(m2)["clean"] * 100
	# Output
		# (Intercept) 
  		# 90.58612 

  		# This gives us the predicted value of 90 for overall satisfaction.

  	# A less clunky way to run the formula above using matrix operations
  	coef(m2)%*%c(1,100,100,100,100)

  	# Using the predict() function to predict values
  	# predict(object, newdata) takes the model (object) and accepts a dataframe (newdata) with the same column headings as the df which was used to generate the model, and can then give predictions.
  	# e.g. 
  	predict(m2, sat.df[1:10,])
  	# Output
  		# 1        2        3        4        5        6        7        8        9       10 
		# 46.60864 54.26012 51.17289 50.30434 52.94625 27.87214 36.27435 43.13123 66.91439 45.38024 


	# We can also access the predictions for observations used to estimate the model - they are stored in the model object
	fitted(m2)[1:10]
  		# 1        2        3        4        5        6        7        8        9       10 
		# 46.60864 54.26012 51.17289 50.30434 52.94625 27.87214 36.27435 43.13123 66.91439 45.38024

# Standardising predictors
	# When variables do not share a common scale (e.g. 1-100), it can be helpful to standardise them in order to compare their coefficients. 
	# We can achieve this by converting units to zero-centred units of standard deviation. The formula to do this for e.g. rides would be as follows:
	(sat.df$rides - mean(sat.df$rides)) / sd(sat.df$rides)

	# But it's such a common operation that R gives us the scale() function
	scale(sat.df$rides)
	# Output = same as line 245.

	# Standardising our entire dataframe
	sat.std<-sat.df[ , -3] # sat.df values but remove distance, use log distance.
	sat.std[ , 3:8]<-scale(sat.std[ , 3:8]) # convert the values to normalised, excluding number children and weekend.
	summary(sat.std)
	# When performing this kind of operation, check the output using summary - the mean of standardised variables should always be 0.

# Using factors as predictors
	# Including the factors weekend/kids and the variable logdistance in the model
	m3<-lm(overall ~ rides + games + wait + clean + weekend + logdistance + num.child, data=sat.std)

	summary(m3)
	# Output
		# Call:
		# lm(formula = overall ~ rides + games + wait + clean + weekend + 
		#     logdistance + num.child, data = sat.std)

		# Residuals:
		#      Min       1Q   Median       3Q      Max 
		# -1.51427 -0.40271  0.01142  0.41613  1.69000 

		# Coefficients:
		#             Estimate Std. Error t value Pr(>|t|)    
		# (Intercept) -0.37271    0.04653  -8.009 8.41e-15 ***
		# rides        0.21288    0.04197   5.073 5.57e-07 ***
		# games        0.07066    0.03026   2.335   0.0199 *  
		# wait         0.38138    0.02777  13.734  < 2e-16 ***
		# clean        0.29690    0.04415   6.725 4.89e-11 ***
		# weekendyes  -0.04589    0.05141  -0.893   0.3725    
		# logdistance  0.06470    0.02572   2.516   0.0122 *  
		# num.child    0.22717    0.01711  13.274  < 2e-16 ***
		# ---
		# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

		# Residual standard error: 0.5709 on 492 degrees of freedom
		# Multiple R-squared:  0.6786,	Adjusted R-squared:  0.674 
		# F-statistic: 148.4 on 7 and 492 DF,  p-value: < 2.2e-16

	# We now have an R-squared value of 0.674, even better than m2.
	# Looking at the coefficients, we can see that logdistance and num.child are significantly greater than zero, suggesting that people who travel further / have more kids have greater satisfaction overall.
	# For weekend - R converts the factor to a numeric value where 1 is yes, and indicates this by listing the direction of the output as 'weekendyes' so we know what 1 stands for. We can interpret the summary as telling us that those who come on the weekend have on average * -0.04589 less overall satisfaction rating.

	# Converting num.child to a factor
	# In the previous model, we're assuming that the number of children has a linear relationship with satisfaction. But what if we convert this variable to a factor (since there are only 6 options)
		sat.std$num.child.factor<-factor(sat.std$num.child)
		m4<-lm(overall ~ rides + games + wait + clean + weekend + logdistance + num.child.factor, data=sat.std)
		summary(m4)
		# Output
			# Residuals:
			#      Min       1Q   Median       3Q      Max 
			# -1.25923 -0.35048 -0.00154  0.31400  1.52690 

			# Coefficients:
			#                   Estimate Std. Error t value Pr(>|t|)    
			# (Intercept)       -0.69100    0.04488 -15.396  < 2e-16 ***
			# rides              0.22313    0.03541   6.301 6.61e-10 ***
			# games              0.04258    0.02551   1.669   0.0958 .  
			# wait               0.38472    0.02338  16.453  < 2e-16 ***
			# clean              0.30917    0.03722   8.308 9.72e-16 ***
			# weekendyes        -0.02227    0.04322  -0.515   0.6065    
			# logdistance        0.03187    0.02172   1.467   0.1429    
			# num.child.factor1  1.01610    0.07130  14.250  < 2e-16 ***
			# num.child.factor2  1.03732    0.05640  18.393  < 2e-16 ***
			# num.child.factor3  0.98000    0.07022  13.955  < 2e-16 ***
			# num.child.factor4  0.93154    0.08032  11.598  < 2e-16 ***
			# num.child.factor5  1.00193    0.10369   9.663  < 2e-16 ***
			# ---
			# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			# Residual standard error: 0.4795 on 488 degrees of freedom
			# Multiple R-squared:  0.7751,	Adjusted R-squared:   0.77 
			# F-statistic: 152.9 on 11 and 488 DF,  p-value: < 2.2e-16

		# R-squared is now 0.77 - even better than the model where children are linear. We notice that there is little variation in the coefficients for num.child.factor1-5 - each approximately 1 standard deviation higher than 0 children (which isn't shown because it is taken as the baseline)

	# Making hasChildren a binary variable - since there is little difference in the number of children apart from those with or without.
		sat.std$hasChild<-factor(sat.std$num.child > 0)
		m5<-lm(overall ~ rides + games + wait + clean + weekend + logdistance + hasChild, data=sat.std)
		summary(m5)
		# Output
			# Residuals:
			#      Min       1Q   Median       3Q      Max 
			# -1.22366 -0.35107 -0.01747  0.31852  1.45703 

			# Coefficients:
			#              Estimate Std. Error t value Pr(>|t|)    
			# (Intercept)  -0.69039    0.04478 -15.418  < 2e-16 ***
			# rides         0.22193    0.03517   6.310 6.24e-10 ***
			# games         0.04409    0.02541   1.735   0.0833 .  
			# wait          0.38603    0.02328  16.583  < 2e-16 ***
			# clean         0.30904    0.03699   8.354 6.75e-16 ***
			# weekendyes   -0.02280    0.04311  -0.529   0.5971    
			# logdistance   0.03404    0.02159   1.576   0.1156    
			# hasChildTRUE  1.00485    0.04689  21.428  < 2e-16 ***
			# ---
			# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			# Residual standard error: 0.4785 on 492 degrees of freedom
			# Multiple R-squared:  0.7742,	Adjusted R-squared:  0.771 
			# F-statistic:   241 on 7 and 492 DF,  p-value: < 2.2e-16

			# Above - R-squared hasn't changed much versus m4, so we can just use hasChild for the model, instead of the number of children. 

# Interaction terms
# Understanding if variables have a greater effect with specific combinations (e.g. waiting times may have a greater effect for families with children versus those without. 
	# We can include interaction of terms for consideration in the model by using :
		m6<-lm(overall ~ rides + games + wait + clean + weekend + logdistance + hasChild + rides:hasChild + games:hasChild + wait:hasChild + clean:hasChild+ rides:weekend + games:weekend+ wait:weekend + clean:weekend, data=sat.std)
		summary(m6)
		# Output
			# Residuals:
			#      Min       1Q   Median       3Q      Max 
			# -1.15097 -0.31487 -0.01245  0.30277  1.45388 

			# Coefficients:
			#                     Estimate Std. Error t value Pr(>|t|)    
			# (Intercept)        -0.677443   0.043034 -15.742  < 2e-16 ***
			# rides               0.146980   0.067982   2.162  0.03110 *  
			# games               0.079569   0.049365   1.612  0.10765    
			# wait                0.129718   0.044266   2.930  0.00355 ** 
			# clean               0.312757   0.079685   3.925 9.93e-05 ***
			# weekendyes         -0.020461   0.041261  -0.496  0.62021    
			# logdistance         0.025801   0.020671   1.248  0.21258    
			# hasChildTRUE        0.995076   0.044869  22.177  < 2e-16 ***
			# rides:hasChildTRUE  0.057837   0.073070   0.792  0.42902    
			# games:hasChildTRUE -0.064043   0.052797  -1.213  0.22572    
			# wait:hasChildTRUE   0.350649   0.047241   7.423 5.21e-13 ***
			# clean:hasChildTRUE -0.001854   0.079710  -0.023  0.98146    
			# rides:weekendyes    0.061784   0.067750   0.912  0.36225    
			# games:weekendyes    0.018511   0.049036   0.377  0.70597    
			# wait:weekendyes     0.035168   0.044463   0.791  0.42936    
			# clean:weekendyes   -0.027305   0.071005  -0.385  0.70074    
			# ---
			# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			# Residual standard error: 0.4521 on 484 degrees of freedom
			# Multiple R-squared:  0.8018,	Adjusted R-squared:  0.7956 
			# F-statistic: 130.5 on 15 and 484 DF,  p-value: < 2.2e-16

		# This model has an even better R-squared value. We observe from all the interactions that not many have especially significant coefficients, so we can drop most, with the exception of wait:hasChildTRUE

	# Building a model with only the wait:hasChild interaction
		m7<-lm(overall ~ rides + games + wait + clean + weekend + logdistance + hasChild + wait:hasChild, data=sat.std)
		summary(m7)

		# Output
			# Residuals:
			#      Min       1Q   Median       3Q      Max 
			# -1.12284 -0.32520 -0.00351  0.31079  1.42950 

			# Coefficients:
			#                   Estimate Std. Error t value Pr(>|t|)    
			# (Intercept)       -0.68687    0.04223 -16.267  < 2e-16 ***
			# rides              0.21223    0.03319   6.395 3.75e-10 ***
			# games              0.04861    0.02397   2.028   0.0431 *  
			# wait               0.15136    0.03694   4.098 4.88e-05 ***
			# clean              0.30260    0.03489   8.673  < 2e-16 ***
			# weekendyes        -0.01243    0.04067  -0.306   0.7600    
			# logdistance        0.02861    0.02037   1.404   0.1608    
			# hasChildTRUE       0.99787    0.04422  22.564  < 2e-16 ***
			# wait:hasChildTRUE  0.34645    0.04386   7.899 1.86e-14 ***
			# ---
			# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

			# Residual standard error: 0.4512 on 491 degrees of freedom
			# Multiple R-squared:  0.7997,	Adjusted R-squared:  0.7964 
			# F-statistic:   245 on 8 and 491 DF,  p-value: < 2.2e-16

		# From the output, we can see pretty much no loss of accuracy through R-squared in the models 7 and 6, so it's looking good.


# What do we do with these results?
	# We could e.g.
		# Attract more visitors with children.
		# Investigate why visitors without children are less satisfied and try to fix problems.
		# Commit resources to cleanliness.
		# Investigate the relationship between children and waiting time.

	# Sharing the results

		# Make a new satisfaction drivers plot
			library(coefplot)
			coefplot(m7, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
				ylab="Rating of Feature",
				xlab="Association with Overall Satisfaction")

		# Caution - using interaction effects (as in lines 355)
			# Make sure to always include the terms individually as well as for interactions (e.g. x + y as well as x:y).
			# Make sure to standardise the interaction terms so you have a comparable scale for coefficients.

# Recap: Recommended process for linear model fitting
	# Inspect the data - make sure it is clean and has the structure you'd expect. 
	# Check distributions of the variables to make sure they are not highly skewed.
		# If skewed - consider transformation.
	# Examine the bivariate scatterplots and correlation matrix to see if any variables are extremely correlated (r>0.9 or lots with r>0.8). If so - omit some variables or consider transforming them if needed. 
	# If you need to estimate coefficients on a consistent scale, standardise the data with scale(). 
	# Check the residual quantiles from the model.
	# Check the standard model plots using plot(model), to judge if a linear model is appropriate or if there is nonlinearity, and check for outliers.
	# Try several models, compare them for overall interpretability and model fit. If models are nested, you can use anova() to compare them.
	# Report the confidence intervals of the estimates with your interpretations / recommendations.


# Key points
	# Linear models relate continuous scale outcome variables to predictors by finding a straight line that best fits the data points. The basic formula is lm(formula, data). From here, you can use the object produced by lm() with plot, predict and summary().
	# Before modelling, check data quality & distribution. Also check independent variables aren't overly correlated.
	# To interpret coefficients on a standard scale, you need to normalise the variables or be confident that they are normalised.
	# summary for lm() is hugely useful!
	# Factors can be included by adding the name of the factor to the model formula - R will automatically create binary variables with 0/1 for each individual level.
	# Interactions are predictors which are the product of two other predictors - it checks whether predictors reinforce or cancel each other out.
	# Model building involves adding and removing predictors to find a set that fit the data well.
	# Interpret coefficients in terms of their predicted ranges to really understand them - include confidence intervals. 
