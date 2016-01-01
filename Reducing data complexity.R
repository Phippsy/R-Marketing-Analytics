brand.ratings<-read.csv('http://goo.gl/IQl8nc')

# Rescale the data
	# To make the data more comparable across individuals and samples. Common practice is to centre the variable by subtracting its mean from every observation, then rescaling those centred values as units of standard deviation. This is known as standardising, normlising or Z scoring the data.

	# basic formula is
		x.sc<-(x-mean(x) / sd(x))

	# But to make it easier you can just use scale()
		x.sc<-scale(x)

	# Scaling our data
		brand.sc<-brand.ratings
		brand.sc[, 1:9]<-scale(brand.sc[, 1:9])

# Looking at correlations
	library(corrplot)
	corrplot(
		cor(brand.sc[, 1:9]), 
		order="hclust"
		)
	# Here we can see some clusters of 2-3 variables which will be examined later.

# Aggregating mean ratings by brand.
	brand.mean<-aggregate(. ~ brand, data=brand.sc, mean)

	# Tidying up - naming the rows using the brand names in the first column, then removing that redundant column.
	rownames(brand.mean)<-brand.mean[,1]
	brand.mean<-brand.mean[, -1]

# Visualising results using a heatmap
	library(gplots)
	library(RColorBrewer)
	heatmap.2(as.matrix(brand.mean),
		col=brewer.pal(9, "GnBu"),
		trace="none",
		key=FALSE,
		dend="none",
		main="\n\n\n\n\nBrand attributes"
		)

		heatmap.2(as.matrix(segments.sc),
		col=brewer.pal(9, "GnBu"),
		trace="none",
		key=FALSE,
		dend="none",
		main="\n\n\n\n\nCustomer Segments"
		)
		
		
		gplots:::heatmap.2(as.matrix(segments.anon),
		          col=brewer.pal(9, "OrRd"),
		          trace="none",
		          key=FALSE,
		          dend="none",
		          main="\n\n\n\n\nCustomer Segments",
		          cexRow=1,
		          cexCol=1,
		          lhei=c(1,1),
		          lwid=c(3,1)
		)