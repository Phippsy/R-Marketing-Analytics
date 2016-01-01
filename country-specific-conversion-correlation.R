# Multi Metrics - UK only


#Added 23 Nov - Session duration
  #  Query - build a model for as many goals as we can grab in the API, then regression model to see which of these explains each conversion rate the best.
  query.list <- Init(start.date = "2015-01-01",
                     end.date = "2015-11-01",
                     dimensions = "ga:channelGrouping",
                     metrics = "ga:avgSessionDuration, ga:avgTimeOnPage,ga:bounceRate,ga:avgPageLoadTime,ga:goalConversionRateAll,ga:goal2ConversionRate,ga:goal4ConversionRate,ga:goal6ConversionRate,ga:goal16ConversionRate",
                     max.results = 10000,
                     #sort = "-ga:date",
                     table.id = GAcountries$Juk)
  
	# Create the Query Builder object so that the query parameters are validated
	ga.query <- QueryBuilder(query.list)
  
	# Extract the data and store it in a data-frame
	gaConversionFinder<- GetReportData(ga.query, token, split_daywise = T)
	# gaConversionFinder$channelGrouping<-as.factor(gaConversionFinder$channelGrouping)

	names(gaConversionFinder)[6:10]<-c('AllGoals', 'ConfigCompletes', 'AnyLeadComplete', 'TDComplete','QuoteComplete')

	corrplot.mixed(cor(gaConversionFinder[c(2:10)]), upper='ellipse', main="Correlations - JDX UK, Jan-Date")

	# Explain those goals!

		# Explaining all goal conversion rate: Adjusted R-squared:  0.1342 
			bounceUKmodel<-lm(AllGoals ~ bounceRate + avgPageLoadTime + avgSessionDuration + avgTimeOnPage, data=gaConversionFinder)

			# Output
				# Call:
				# lm(formula = AllGoals ~ bounceRate + avgPageLoadTime + avgSessionDuration + 
				#     avgTimeOnPage, data = gaConversionFinder)

				# Residuals:
				#    Min     1Q Median     3Q    Max 
				# -90.39 -11.82  -4.29   6.35 426.32 

				# Coefficients:
				#                     Estimate Std. Error t value Pr(>|t|)    
				# (Intercept)        70.630689   1.515554  46.604  < 2e-16 ***
				# bounceRate         -0.676029   0.042536 -15.893  < 2e-16 ***
				# avgPageLoadTime    -0.354323   0.107213  -3.305 0.000965 ***
				# avgSessionDuration  0.011275   0.002570   4.387 1.20e-05 ***
				# avgTimeOnPage      -0.022414   0.003617  -6.197 6.81e-10 ***
				# ---
				# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				# Residual standard error: 33.36 on 2263 degrees of freedom
				# Multiple R-squared:  0.1357,	Adjusted R-squared:  0.1342 
				# F-statistic: 88.82 on 4 and 2263 DF,  p-value: < 2.2e-16

		# Explaining Config completes: Adjusted R-squared:  0.1336, 
			model<-lm(ConfigCompletes ~ avgSessionDuration + bounceRate + avgPageLoadTime + avgTimeOnPage, data=gaConversionFinder)

			# Output
				# Call:
				# lm(formula = ConfigCompletes ~ avgSessionDuration + bounceRate + 
				#     avgPageLoadTime + avgTimeOnPage, data = gaConversionFinder)

				# Residuals:
				#     Min      1Q  Median      3Q     Max 
				# -68.737  -3.060  -0.803   2.105  87.965 

				# Coefficients:
				#                      Estimate Std. Error t value Pr(>|t|)    
				# (Intercept)        11.5966278  0.4148043  27.957  < 2e-16 ***
				# avgSessionDuration  0.0091357  0.0007034  12.988  < 2e-16 ***
				# bounceRate         -0.1053375  0.0116419  -9.048  < 2e-16 ***
				# avgPageLoadTime    -0.0958425  0.0293439  -3.266  0.00111 ** 
				# avgTimeOnPage      -0.0129873  0.0009899 -13.119  < 2e-16 ***
				# ---
				# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				# Residual standard error: 9.131 on 2263 degrees of freedom
				# Multiple R-squared:  0.1351,	Adjusted R-squared:  0.1336 
				# F-statistic:  88.4 on 4 and 2263 DF,  p-value: < 2.2e-16

		# Explaining Any leads: Adjusted R-squared:  0.01428 
			model<-lm(AnyLeadComplete ~ avgSessionDuration + bounceRate + avgPageLoadTime + avgTimeOnPage, data=gaConversionFinder)

			# Output
				# Call:
				# lm(formula = AnyLeadComplete ~ avgSessionDuration + bounceRate + 
				#     avgPageLoadTime + avgTimeOnPage, data = gaConversionFinder)

				# Residuals:
				#    Min     1Q Median     3Q    Max 
				# -1.811 -0.795 -0.485 -0.042 48.205 

				# Coefficients:
				#                      Estimate Std. Error t value Pr(>|t|)    
				# (Intercept)         1.811e+00  1.249e-01  14.505  < 2e-16 ***
				# avgSessionDuration -2.498e-06  2.117e-04  -0.012   0.9906    
				# bounceRate         -1.885e-02  3.504e-03  -5.380 8.22e-08 ***
				# avgPageLoadTime    -1.910e-02  8.832e-03  -2.163   0.0306 *  
				# avgTimeOnPage      -1.450e-04  2.980e-04  -0.487   0.6265    
				# ---
				# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				# Residual standard error: 2.748 on 2263 degrees of freedom
				# Multiple R-squared:  0.01602,	Adjusted R-squared:  0.01428 
				# F-statistic: 9.209 on 4 and 2263 DF,  p-value: 2.223e-07


		# Explaining Test Drives: Adjusted R-squared:  0.001574 
			model<-lm(TDComplete ~ avgSessionDuration + bounceRate + avgPageLoadTime + avgTimeOnPage, data=gaConversionFinder)

			# Output
				# Call:
				# lm(formula = TDComplete ~ avgSessionDuration + bounceRate + avgPageLoadTime + 
				#     avgTimeOnPage, data = gaConversionFinder)

				# Residuals:
				#    Min     1Q Median     3Q    Max 
				# -0.569 -0.174 -0.129 -0.059 49.714 

				# Coefficients:
				#                      Estimate Std. Error t value Pr(>|t|)    
				# (Intercept)         0.3197735  0.0655673   4.877 1.15e-06 ***
				# avgSessionDuration -0.0001116  0.0001112  -1.004    0.315    
				# bounceRate         -0.0045749  0.0018402  -2.486    0.013 *  
				# avgPageLoadTime    -0.0049113  0.0046383  -1.059    0.290    
				# avgTimeOnPage       0.0001221  0.0001565   0.780    0.435    
				# ---
				# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				# Residual standard error: 1.443 on 2263 degrees of freedom
				# Multiple R-squared:  0.003335,	Adjusted R-squared:  0.001574 
				# F-statistic: 1.893 on 4 and 2263 DF,  p-value: 0.1089

		# Explaining Quote completions: Adjusted R-squared:  0.001891
			model<-lm(QuoteComplete ~ avgSessionDuration + bounceRate + avgPageLoadTime + avgTimeOnPage, data=gaConversionFinder)

			# Output
				# Call:
				# lm(formula = QuoteComplete ~ avgSessionDuration + bounceRate + 
				#     avgPageLoadTime + avgTimeOnPage, data = gaConversionFinder)

				# Residuals:
				#       Min        1Q    Median        3Q       Max 
				# -0.008000 -0.002582 -0.002264 -0.001868  0.244903 

				# Coefficients:
				#                      Estimate Std. Error t value Pr(>|t|)    
				# (Intercept)         2.581e-03  6.000e-04   4.303 1.76e-05 ***
				# avgSessionDuration  1.277e-06  1.017e-06   1.255   0.2095    
				# bounceRate         -3.404e-05  1.684e-05  -2.021   0.0434 *  
				# avgPageLoadTime     4.545e-05  4.244e-05   1.071   0.2843    
				# avgTimeOnPage      -2.053e-06  1.432e-06  -1.434   0.1518    
				# ---
				# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

				# Residual standard error: 0.01321 on 2263 degrees of freedom
				# Multiple R-squared:  0.003652,	Adjusted R-squared:  0.001891 
				# F-statistic: 2.074 on 4 and 2263 DF,  p-value: 0.08171

  query.list <- Init(start.date = "2015-01-01",
                     end.date = "2015-11-01",
                     dimensions = "ga:channelGrouping",
                     metrics = "ga:avgSessionDuration, ga:avgTimeOnPage,ga:bounceRate,ga:avgPageLoadTime,ga:goalConversionRateAll,ga:goal2ConversionRate,ga:goal4ConversionRate,ga:goal6ConversionRate,ga:goal16ConversionRate",
                     max.results = 10000,
                     #sort = "-ga:date",
                     table.id = GAcountries$LRuk)
  
  # Create the Query Builder object so that the query parameters are validated
  ga.query <- QueryBuilder(query.list)
  
  # Extract the data and store it in a data-frame
	gaUKLRConversionFinder<- GetReportData(ga.query, token, split_daywise = T)
	gaUKLRConversionFinder$channelGrouping<-as.factor(gaUKLRConversionFinder$channelGrouping)

	names(gaUKLRConversionFinder)[6:10]<-c('AllGoals', 'ConfigCompletes', 'AnyLeadComplete', 'TDComplete','QuoteComplete')

	corrplot.mixed(cor(gaConversionFinder[c(2:10)]), upper='ellipse', main="Correlations - JDX UK, Jan-Date")

		# Explaining all goal conversion rate: Adjusted R-squared:  0.1342 
		model<-lm(AllGoals ~ bounceRate + avgPageLoadTime + avgSessionDuration + avgTimeOnPage, data=gaUKLRConversionFinder)

		# Output


	# Explaining Config completes: Adjusted R-squared:  0.1336, 
		model<-lm(ConfigCompletes ~ avgSessionDuration + bounceRate + avgPageLoadTime + avgTimeOnPage, data=gaUKLRConversionFinder)

		# Output


	# Explaining Any leads: Adjusted R-squared:  0.01428 
		model<-lm(AnyLeadComplete ~ avgSessionDuration + bounceRate + avgPageLoadTime + avgTimeOnPage, data=gaUKLRConversionFinder)

		# Output


	# Explaining Test Drives: Adjusted R-squared:  0.001574 
		model<-lm(TDComplete ~ avgSessionDuration + bounceRate + avgPageLoadTime + avgTimeOnPage, data=gaUKLRConversionFinder)

		# Output


	# Explaining Quote completions: Adjusted R-squared:  0.001891
		model<-lm(QuoteComplete ~ avgSessionDuration + bounceRate + avgPageLoadTime + avgTimeOnPage, data=gaUKLRConversionFinder)
