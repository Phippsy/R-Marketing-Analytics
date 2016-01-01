# Linear models
	# Inspect / visualise the variables.
	# Use corrplot.mixed
	model1<-lm(dependent ~ independent, data=data.df)
	summary(model)

# --Possibly revisit this section
		# What to look out for:
			# Coefficients, intercepts, to define the model.
			# Residuals - how much over / under by quartile. Well-fitting models have symmetrical residual quartiles.
			# t-value / Pr(>|t|) - is the coefficient significantly different than zero?
			# R-squared - how much of the overall variance is captured by this model? e.g. 0.1 means approx 10% of variance is captured.
				# R-squared is comparable to the SD of residuals for models with a single independent variable.

# Evaluating the quality of the linear model
	# Inspecting a diagnostic plot for satisfaction driver data.
	par(mfrow=c(2,2))
	plot(model1)
	# This resulting plot shows:
		# Upper-left: Fitted values versus residuals. Look for patterns of bias / balance.
		# Lower-left: Plots the fitted values vs sqrt of residuals. Again, look for patterns of bias.
			# A common pattern in residual plots is a cone or funnel, where the range of errors gets larger for larger fitted values. This is called heteroskedasticity and indicates poor fit.
			# FIX: sometimes transforming the variable will resolve heteroskedasticity.
		# Upper right: Normal QQ plot. Compares the values that residuals would be expected to take if they are normally distributed versus actual values. Points should fall close to the line when the model is appropriate.
		# Bottom right: helps identify potential outliers. This plot shows the leverage of each point - a measure of how much influence the point has on model coefficients. If a point has high residual and high leverage, it has undue influence on the model. 
