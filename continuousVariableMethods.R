# Simulating retailer data
set.seed(21821)
ncust<-1000

# CUSTOMER DATA
	cust.df<-data.frame(cust.id=as.factor(c(1:ncust)))
	cust.df$age<-rnorm(n=ncust, mean=35, sd=5) # set age to be a normal distribution with mean 35 and sd 5
	cust.df$credit.score<-rnorm(n=ncust, mean=3*cust.df$age+620, sd=50) # set credit score to be normal distribution linked to age & with SD 50
	cust.df$email<-factor(sample(c("yes", "no"), size=ncust, replace=TRUE, prob=c(0.8, 0.2))) # set email to be a factor of random yes/no, with probability weighted favourably to yes
	cust.df$distance.to.store<-exp(rnorm(n=ncust, mean=2, sd=1.2)) # set distance to store to be an exponential normal distribution - i.e. more customers will be close than far
	# hist(cust.df$distance.to.store) # to see the distribution of distances

# SALES DATA
	# Online data
	cust.df$online.visits<-rnbinom(ncust, size=0.3,
		mu=15 + ifelse(cust.df$email=="yes", 15, 0) # model the mean of the negative binomial with baseline value 15
		- 0.7 * (cust.df$age-median(cust.df$age))) # age deviation from mean influences the number of visits
	cust.df$online.trans<-rbinom(ncust, size=cust.df$online.visits, prob=0.3)
	cust.df$online.spend<-exp(rnorm(ncust, mean=3, sd=0.1)) * cust.df$online.trans

	# In-store data
	cust.df$store.trans<-rnbinom(ncust, size=5,
		mu=3 / sqrt(cust.df$distance.to.store)) 
	cust.df$store.spend<-exp(rnorm(ncust, mean=3.5, sd=0.4)) * cust.df$store.trans

# Customer satisfaction
	sat.overall<-rnorm(ncust, mean=3.1, sd=0.7) # Set the overall satisfaction - a psychological construct
	sat.service<-floor(sat.overall + rnorm(ncust, mean=0.5, sd=0.4)) # derive service satisfaction from overall sat + random numbers with mean 0.5
	sat.selection<-floor(sat.overall + rnorm(ncust, mean=-0.2, sd=0.6)) # Set the selection satisfaction to overall sat + random num, mean -0.2
	sat.service[sat.service>5]<-5 # set all values > 5 to be 5
	sat.service[sat.service<1]<-1 # set all values < 1 to be 1
	sat.selection[sat.selection>5]<-5
	sat.selection[sat.selection<1]<-1

	# modelling non-responses. Create a subset of non-reponders, with older individuals less likely to respond. 
	no.response<-as.logical(rbinom(ncust, size=1, prob=cust.df$age/100))
	sat.service[no.response]<-NA
	sat.selection[no.response]<-NA
	summary(cbind(sat.service, sat.selection))

	cust.df$sat.service<-sat.service
	cust.df$sat.selection<-sat.selection

# PLOT THAT DATA!
	plot(cust.df$age, cust.df$credit.score,
		col="blue",
		xlim=c(15,55), ylim=c(500,900),
		main="Active Customers as of June 2014",
		xlab="Customer Age (Years)", ylab="Customer Credit Score")
	abline(h=mean(cust.df$credit.score), col="dark blue", lty="dotted")
	abline(v=mean(cust.df$age), col="dark blue", lty="dotted")

	# Other useful commands to add after plot
		# points() - add specific points
		# abline() - add a line by slope
		# lines() - add a set of lines by coordinates
		# legend() - add a legend

	# Plotting store spend vs online spend
	plot(cust.df$store.spend, cust.df$online.spend,
		main="Customers as of June 2014",
		xlab="Prior 12 months in-store sales ($)",
		ylab="Prior 12 months of online sales ($)",
		cex=0.7) # not sure what cex is

	# histogram of just in-store sales
	hist(cust.df$store.spend,
		breaks=(0:ceiling(max(cust.df$store.spend)/10)*10),
		main="Customers as of June 2014",
		xlab="Prior 12 months in-store sales ($)",
		ylab="count of customers")

	# Do the same for online sales
	hist(cust.df$online.spend,
		breaks=(0:ceiling(max(cust.df$online.spend)/10))*10,
		main="Customers as of June 2014",
		xlab="Prior 12 months online sales",
		ylab="count of customers")

	# Colour coding the scatter (lines 59-64) to identify cusomers who have opted in
	# Setting up colour vectors
		my.col<-c("black", "green3")
		my.pch<-c(1,19) # 1 and 19 are the symbols for solid / open circles (?points)

	# Setting up the plot
		plot(cust.df$store.spend, cust.df$online.spend,
		main="Customers as of June 2014",
		col=my.col[cust.df$email], pch=my.pch[cust.df$email],
		xlab="Prior 12 months in-store sales ($)",
		ylab="Prior 12 months of online sales ($)",
		cex=0.7)

	# Adding a legend
		legend(x="topright", legend=paste("email on file: ", levels(cust.df$email)),
			col=my.col, pch=my.pch)

	# Adding a log-scale to make the plot easier to read (repeats lines 85-95, but full code added below. Only lines 99 & 100 are new.)
			# Setting up the plot
				plot(cust.df$store.spend + 1, cust.df$online.spend + 1, # +1 is added to avoid the log error because the log of 0 does not pass.
				log="xy",
				main="Customers as of June 2014",
				col=my.col[cust.df$email], pch=my.pch[cust.df$email],
				xlab="Prior 12 months in-store sales ($)",
				ylab="Prior 12 months of online sales ($)",
				cex=0.7) 

			# Adding a legend
				legend(x="topright", legend=paste("email on file: ", levels(cust.df$email)),
					col=my.col, pch=my.pch)

# Multiple plots in the same vis
	par(mfrow=c(2,2)) # tells R that you want a 2x2 graphics space.

	# Plotting store/online Vs distance to store
		plot(cust.df$store.spend, cust.df$distance.to.store, main="store")
		plot(cust.df$online.spend, cust.df$distance.to.store, main="online")
		plot(cust.df$store.spend+1, cust.df$distance.to.store + 1, log="xy")
		plot(cust.df$store.spend+1, cust.df$distance.to.store + 1, log="xy")


	# Making a megaplot, specifying the variables

		pairs(formula = ~ age + credit.score + email + 
			distance.to.store + online.visits + online.trans + online.spend + store.trans + store.spend,
			data=cust.df)

# Using the car library for better scatterplots

	library(car)
	scatterplotMatrix(formula = ~ age + credit.score + email + 
		distance.to.store + online.visits + online.trans + online.spend + 
		store.trans + store.spend,
		data=cust.df, diagonal="histogram")


# Visualising factorial variables vs continuous using gpairs
	library(gpairs)
	gpairs(cust.df[,2:10])


# Understanding correlations
	# Covariance: 1 = x and y tend to go up together. -1 = x goes up, y goes down. 0 = no linear association
		cov(cust.df$age,cust.df$credit.score)


	# Pearson's product-moment correlation
		# -1 = perfect negative linear correlation, +1 = perfect positive, 0=no correlation.
		# Cohen's rule of thumb for data involving people: (Cohen, J (1988) Statistical power analysis for the behavioural sciences (2nd ed.). Hillsdale: Lawrence Erlbaum Associates)
		# 	0.1 = weak correlation
		# 	0.3 = medium correlation
		# 	0.5 = strong correlation
		# N.B. - these correlations apply only to normal distribution - i.e. neither of the scales are logarithmic. To judge correlation, first convert your data to normal scales.
		cor(cust.df$age, cust.df$credit.score)
		
		# Testing the statistical significance of the correlation
			cor.test(cust.df$age,cust.df$credit.score)
			# When running the above, since the confidence interval is 95%, giving p-values of 0.1955974 - 0.3115816, the association is significant (since the lowest interval is > 0.05)

		# Creating a correlation matrix
			# Pass multiple vectors to the cor() function. 
			cor(cust.df[,c(2,3,5:12)])
			# Optional - you can pass the argument use="complete.obs" to the function above and R will only compare cases without NA values.

	# Plotting the correlation using gplots/corrplot
		library(corrplot)
		library(gplots)
		corrplot.mixed(corr=cor(cust.df[,c(2,3,5:12)], use="complete.obs", ),
			upper="ellipse", tl.pos="lt",
			col=colorpanel(50,"red","gray60","blue4"))

# switching display back to 1x1 in the plot area
	par(mfrow=c(1,1))

# Transforming variables before computing correlations
		print("There is a perfect linear relationship between x and x^2 when we plot the following, but correlation with cor() shows no correlation")
		x<-runif(1000,min=-10, max=10)
		plot(x, x^2)
		cor(x,x^2)


	# Tracking the linear correlation between distance to store and store spend reveals a small linear correlation
		cor(cust.df$distance.to.store, cust.df$store.spend)


	# But if you track the correlation between store spend and the inverse of distance to store, there's a much stronger correlation
		cor(1/cust.df$distance.to.store, cust.df$store.spend)

	# Showing the difference between plotting normal and inverse square representations
		par(mfrow=c(2,1))
		plot(cust.df$distance.to.store, cust.df$store.trans)
		plot(1/sqrt(cust.df$distance.to.store), cust.df$store.trans)


# Commonly-used data transformations for marketing
	
	# Unit sales, revenue, household income, price
	log(x)

	# distance
	1/x, 1/x^2, log(x)

	# Market or preference share based on a utility value
	e^x / 1 + e^x (see later section)

	# Right-tailed distributions (generally)
	sqrt(x) or log(x)

	# Left-tailed distributions
	x^2

# Box-Cox transformations
# Useful for finding relationships between variables with skewed distributions

	library(car)

	# Find the best value of lambda to make distance to store the most normal distribution.
	# If powerTransform returns a value close to 1, this suggests that the original distribution is already close to normal.
		powerTransform(cust.df$distance.to.store) 

	# Extract the value of lambda using coef()
		lambda<-coef(powerTransform(1/cust.df$distance.to.store))

	# Transform the distance to store variable using bcPower
		bcPower(cust.df$distance.to.store, lambda)


	# See how this now affects the distance variable in a couple of plots
		par(mfrow=c(1,2))

		# Histogram for the original variable
		hist(cust.df$distance.to.store,
			xlab="Distance to Nearest Store", ylab="Count of customers",
			main="Count of original customers")

		# Histogram for the transformed Box-Cox variable
		hist(bcPower(cust.df$distance.to.store, lambda),
			xlab="Distance to Nearest Store", ylab="Count of customers",
			main="Count of lambda'd customers")

	# Using Box-Cox to find associations between distance and spend

		# Transform both variables using the same process as line 218
			l.dist<-coef(powerTransform(cust.df$distance.to.store))
			l.spend<-coef(powerTransform(cust.df$store.spend+1)) # +1 because some of the spend values will be 0

		# Check the correlation with cor()
			cor(bcPower(cust.df$distance.to.store,l.dist), bcPower(cust.df$store.spend+1, l.spend))

		# Plot the two transformed variables
			plot(bcPower(cust.df$distance.to.store,l.dist), bcPower(cust.df$store.spend+1, l.spend))


# Exploring associations in survey responses (ordinal variables, on an ordinal scale)

	# Plotting as below demonstrates that the only information that can be gathered as-is is that among the population, most of the possible scale variations were used. It doesn't tell us about the density of given combinations.
	plot(cust.df$sat.service, cust.df$sat.selection,
		xlab="Service Satisfaction", ylab="Selection satisfaction",
		main="Customers as of June 2014")

	# To fix this, use jitter() to add random noise to the plots so you can see intensity.
	plot(jitter(cust.df$sat.service), jitter(cust.df$sat.selection),
		xlab="Service Satisfaction", ylab="Selection satisfaction",
		main="Jittery customers as of June 2014")

# polychloric() - this can wait, save for later. Page 107

# Plots ticklist
# 	xlab, ylab, main
# 	legend
# 	cex= argument adjusts point sizes on the plot
# 	When plots don't look as you expect, check the variable type with str()
# 	For skewed distributions, consider log scales or common transformations (lines 193-228)

# Correlation ticklist
# 	cor() to understand the correlation
# 	cor.test() to gauge the statistical significance
# 	corrplot pacakge to plot the correlations
# 	