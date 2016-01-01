# Generate the data
	
	# Why we do it this way
	# We separate the data definitions from procedural code, which is good general practice. Data can easily be updated afterwards without rooting through all the procedures.

	# Setting column headers for customer attributes then the distribution type of each
	segVars<-c("age","gender","income","kids","ownHome","subscribe")
	segVarType<-c("norm","binom","norm","pois","binom","binom")

	# Setting Segment names & population sizes
	segNames<-c("Suburb Mix","Urban hip","Travelers","Moving up")
	segSize<-c(100,500,80,70)

	# Making a 4 x 6 matrix to describe the means of each attribute, by Segment
	segMeans<-matrix(c(
		40,0.5,55000,2,0.5,0.1,
		24,0.7,21000,1,0.2,0.2,
		58,0.5,64000,0,0.7,0.05,
		36,0.3,52000,2,0.3,0.2), ncol=length(segVars), byrow=TRUE) # the byrow argument has the values fill up across each row instead of down each column.

	segSDs<-matrix(c(
		5,NA,12000,NA,NA,NA,
		2,NA,5000,NA,NA,NA,
		8,NA,21000,NA,NA,NA,
		4,NA,10000,NA,NA,NA
		), ncol=length(segVars), byrow=TRUE)

	# Binominal and Poisson

	# Refresher on for loops
	# Print out the index
	for ( i in 1:10 ) {print(i)} # 1,2,3,4,5...

	# But the for loop doesn't only return integers - it returns every item in the vector, of any type.
	i.seq<-rep(sqrt(seq(from=2.1, to=6.2, by=1.7)), 3)
	for ( i in i.seq) { print (i)}

	# The gotcha - beware indexing by vector lengths- use seq_along instead
		test<-NULL

		for ( i in 1:length(test)) { print(i)} # 1,0. Prints this because we instructed R to start with 1, which it does, but then the length of test is 0, so it prints out 0 and stops. 

		# Better approach - using seq_along
		for ( i in seq_along(test)) {print(i)} # R knows to inspect the sequence of the test variable, which is NULL, so it returns nothing.

# If statements / ifelse and rules
	# If statement is not vectorized - it only evaluated once for a given variable.
	x<-c(1:5)
	if ( x > 1){
		print("Hi")
	} else {
		print("Bye")
	} # This prints out Bye since x[1] is not >1.

	# To evaluate for all items in a vector, use the ifelse() statement

	ifelse(x>1, print("Hi"), print("Bye"))

	# You can also call functions within the ifelse statement
	fn.hi<-function(){"Hi"}
	fn.bye<-function(){"Bye"}
	ifelse(x>1, fn.hi(), fn.bye()) # [1] "Bye" "Hi"  "Hi"  "Hi"  "Hi" 


# Populating the groups with data using nested loops

	seg.df<-NULL
	set.seed(02554)

	# Iterate over the segments, create data for each
	for (i in seq_along(segNames)) {
		cat(i, segNames[i], "\n")

		# Empty matrix to hold this segment's data
		this.seg<-data.frame(matrix(NA, nrow=segSize[i], ncol=length(segVars)))

		# Within the segment, iterate over variables and draw appropriate random data
		for ( j in seq_along(segVars)) {
			if (segVarType[j] == "norm" ) {
				this.seg[, j]<-rnorm(segSize[i], mean=segMeans[i,j], sd=segSDs[i,j])
			} else if ( segVarType[j] == "pois" ) {
				this.seg[, j]<-rpois(segSize[i], lambda=segMeans[i,j])
			} else if ( segVarType[j] == "binom" ) {
				this.seg[, j]<-rbinom(segSize[i], size=1, prob=segMeans[i,j])
			} else {
				stop("Bad segment data type: ", segVarType[j])
			}
		}

		# Add the current segment to the dataset
		seg.df<-rbind(seg.df, this.seg)
	}


	# Adding column names
	names(seg.df)<-segVars

	# Add a segment name column for each row
	seg.df$Segment<-factor(rep(segNames, times=segSize))

	# Convert the binom variables to meaningfully labelled factors
	seg.df$ownHome<-factor(seg.df$ownHome, labels=c("ownNo", "ownYes"))
	seg.df$gender<-factor(seg.df$gender, labels=c("Female", "Male"))
	seg.df$subscribe<-factor(seg.df$subscribe, labels=c("subNo", "subYes"))

# --------------------------------
# Inspecting the data
# --------------------------------

	# Getting the mean income of the 'moving up' segment.

		# 	Use data frame indexing to match only rows which fit the segment
			mean(seg.df$income[seg.df$Segment == "Moving up" & seg.df$subscribe == "subNo"])

		# But this can become tedious - an easier way might be to use the by() function
			by(seg.df$income, seg.df$Segment, mean)

		# You can spilt by multiple factors if you supply the indices argument as a list
			by(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)

		# As previously - better option is aggregate
			aggregate(seg.df$income, list(seg.df$Segment, seg.df$subscribe), mean)


	# Creating a lookup data frame of average segment income
		
		# Making the average data frame
		seg.income.mean<-aggregate(seg.df$income, list(seg.df$Segment), mean)

		# Doing a lookup on the segment name to return values
		seg.df$segIncome<-seg.income.mean[seg.df$Segment, 2]
		
		# Note that for the above to work, the row lookup (seg.df$Segment) seemingly has to be a factor. You can pass in character vectors and have them still work in the following way:
		seg.income.mean[as.factor(segNames), 2]


	# Basic formula syntax
		# We describe the relationship between variables through formula specification. Basic syntax is x ~ y, where with linear variables, x is described by y. x is the response variable, y is the explanatory variable. 
		# Depending on the situation and the data types, the meaning of response and explanatory will vary.

		# Using aggregate to calculate the mean of income by segment.
			aggregate(income ~ Segment, data=seg.df, mean)

		# Formulas are a very important part of R and used throughout to look at variable relationships. The basic syntax here is aggregate(formula, data, fun)

		# We can get more complex and add in more than 1 explantory variable
			aggregate(income ~ Segment + ownHome + subscribe, data=seg.df, mean)


	# Using with() to look at the frequency of a given factor
		with(seg.df, table(Segment, ownHome)) # The with() function takes the first argument - data - and then applies the given expression to that data. 

		# Same again - this time checking the number of kids by segment
		with(seg.df, table(Segment, kids))
		
		# When we run this code (above), we're treating kids as a factor, not a number - i.e. it's not seen as a count. We can change this to check the sum of the counts for a given segment, as below.
		xtabs(kids ~ Segment, data=seg.df) # returns the sum of kids for each segment.

		# Alternative ways to achieve the same result from line 157
		aggregate(kids ~ Segment, data=seg.df, sum)

# Visualisation by group: frequencies & proportions
	# Using the lattice package to do all the work
		# histogram(formula, data, type) can be used, and understands formula notation. We can also use _conditioning_ on a factor - which means separating the plot into multiple panes based on that factor (using | )
		# e.g.
	require(lattice)
	histogram(~subscribe | Segment, data=seg.df) # plot the total count of yes/no subscribes as a histogram, splitting into separate charts for each segment.

	# Specifying that we want results shown as counts, not percentages:
	histogram(~subscribe | Segment, data=seg.df, type="count", layout=c(4,1), col=c("burlywood", "darkolivegreen"))

	# You can condition by more than one factor - just specify it after the pipe
	histogram(~subscribe | Segment + ownHome, data=seg.df)

	# The prop.table tells you what proportion of the table each value is. By default it gives the percentage of the entire table, but you can change this by specifying the margin argument. 1=by row, 2=by column.
	prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)

			#        Moving up Suburb Mix Travelers Urban hip
			# subNo  0.7142857  0.9400000 0.9625000 0.7980000
			# subYes 0.2857143  0.0600000 0.0375000 0.2020000

	# Once we have this, we can plot just the subYes values in a barchart as follows:
		# Repeating the code from line 180, assigning to a variable
		proptable<-prop.table(table(seg.df$subscribe, seg.df$Segment), margin=2)

		# Using the prop.table in the barchart and selecting only subYes
		barchart(proptable[2, ], xlab="Subscriber proportion by Segment", col="darkolivegreen")

	# Discussed in the book but never properly explained - look into this
	# barplot()

# Visualisation by group - Continuous data
	# Plotting the mean of income by segment
	seg.mean<-aggregate(income ~ Segment, data=seg.df, mean)
	library(lattice)
	barchart(income~Segment, data=seg.mean, col="grey")

	# Splitting the data out further by home ownership
	seg.income.agg<-aggregate(income ~ Segment + ownHome, data=seg.df, mean)

	barchart(income ~ Segment, data=seg.income.agg,
		groups=ownHome, auto.key=TRUE,
		par.settings = simpleTheme(col=terrain.colors(2)))

	# But a boxplot tells us much more about the data distribution, as well as the means
	boxplot(income ~ Segment, data=seg.df, yaxt="n", ylab="Income ($k)")
	ax.seq<-seq(from=0, to=120000, by=20000)
	axis(side=2, at=ax.seq, labels=paste(ax.seq/1000, "k", sep=""), las=1)

	# Using the super-sexy bwplot() vis from the lattice package
	# Warning - formula notation is back-to-front from what you'd expect - explanatory on the left, descriptive on the right.

	bwplot(Segment ~ income, data=seg.df, horizontal=TRUE, xlab="Income") # Now it looks pretty.

	# You can also break out home ownership as a conditioning variable within the formula for bwplot()
	bwplot(Segment ~ income | ownHome, data=seg.df, horizontal=TRUE, xlab="Income")


# Summary - key lessons
# 	Use seq_along in for loops.
# 	When creating data objects from scratch, prepopulate with NA data to avoid problems, and for speed & reliability.

# 	When describing & visualising data
# 		by() splits data up and applies functions
# 		aggregate() split up and tabulates results, and understands the formula model
# 		table() gives you the frequency of occurrence for an item.
# 		xtabs() gives you count data and can apply a formula
# 		