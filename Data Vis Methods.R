#creating a ready-to-go histogram function

histoFunk<-function(vector){
	#using it again for product 2
	histoid<-hist(vector,
		main="Product 2 Weekly Sales Frequencies, All Stores", # set the main title
		xlab="product 2 Sales (Units)", # set the x axis label
		ylab="Count", # set the y axis label
		breaks=30, # sets the number of buckets to fit the data into
		col="lightblue", # sets the bar colour
		freq=FALSE, # uses plot density instead of counts for the histogram
		xaxt="n" # sets X axis text to 'none'
		)

	# start axis labels at 60, end at 300 with intervals of 20
	axis(side=1, at=seq(60, 300, by=20))

	# add a smoothed estimation line
	lines(density(vector, bw=10), # bw adjusts smoothing
		type="l", col="darkred", lwd=2) # lwd = line width
}

# Boxplots
	# A simple, single boxplot showing the distribution of all P2 sales
	boxplot(store.df$p2sales, xlab="Weekly Sales", ylab="P2", main="Weekly sales of P2, All Stores", horizontal=TRUE)

	# A boxplot comparing p2 sales over the different stores
	boxplot(store.df$p2sales ~ store.df$StoreNum, horizontal=TRUE, # tilde separates the response variables (p2 sales) from the explanatory variable (storeNum)
		ylab="Store", xlab="Weekly unit sales", las=1, # las=1 forces the axes to have text in the horizontal direction
		main="Weekly Sales of P2 by Store")

	# A boxplot comparing p2 sales over the different promotion statuses
	boxplot(store.df$p2sales ~ store.df$p2prom, horizontal=TRUE, yaxt="n", # yaxt="n" removes the default y axis text
		ylab="P2 promoted in store?", xlab="Weekly unit sales",
		main="Weekly Sales of P2 with or without promotion")
	axis(side=2, at=c(1,2), labels=c("No", "Yes")) # set the Y axis text

# Language brief
# Breaking out data by factors and summarising

# by() FUNCTION. Easy to read but not re-usable

	# Calculating the mean of store sales by store number
	by(data=store.df$p1sales, INDICES=store.df$StoreNum, FUN=mean)

	# Calculating the mean of store sales by store number AND year
	by(data=store.df$p1sales, INDICES=list(store.df$StoreNum, store.df$Year), FUN=mean)

# aggregate() FUNCTION - returns as data frame

	# sum all of the sales by country and return into the country row
	aggregate(store.df$p1sales, by=list(country=store.df$country), sum)

	p1sales.sum<-aggregate(store.df$p1sales, by=list(country=store.df$country), sum) # storing the aggregated data for use in the next part.

# Maps
	# calling the function to join the country data to the map, matching country column as containing the ISO codes
	p1sales.map<-joinCountryData2Map(p1sales.sum, joinCode="ISO2", nameJoinColumn="country") 
	mapCountryData(p1sales.map, nameColumnToPlot="x", mapTitle="Total P1 Sales by Country",
		colourPalette=brewer.pal(7, "Greens"),
		catMethod="fixedWidth", addLegend=FALSE)