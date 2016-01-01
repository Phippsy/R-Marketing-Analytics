k.stores<-20 # we're creating 20 stores
k.weeks<-104 # data will be over 2 years

# create the store.df data frame, filled with NA data, 104 * 20 rows
store.df<-data.frame(matrix(NA, ncol=10, nrow=k.stores*k.weeks))

#set the column header names to be descriptive
names(store.df)<-c("StoreNum", "Year", "Week", "p1sales", "p2sales", "p1price", "p2price", "p1prom", "p2prom", "country")

#set store numbers
store.num<-101:(100+k.stores)
#make the city names repeat
store.cty<-c(rep("US", 3), rep("DE", 5), rep("GB", 3), rep("BR", 2), rep("JP", 4), rep("AU", 1), rep("CN", 2))

store.df$StoreNum<-rep(store.num, each=k.weeks) # fill in the store numbers
store.df$country<-rep(store.cty, each=k.weeks) # fill in the country codes
rm(store.num, store.cty) # remove the temporary variables
store.df$Week<-rep(1:52, times=k.stores*2) # fill out the week numbers
store.df$Year<-rep(rep(1:2, each=k.weeks/2), times=k.stores)

store.df$StoreNum<-factor(store.df$StoreNum) # turn store number into factor type
store.df$country<-factor(store.df$country) # turn country into factor type
head(store.df)

set.seed(98250) # setting the seed for random gen

# generate random distribution of 1s, with distribution probabilty of 10%
store.df$p1prom<-rbinom(n=nrow(store.df), size=1, p=0.1)

# generate random distribution of 1s, with distribution probabilty of 15%
store.df$p2prom<-rbinom(n=nrow(store.df), size=1, p=0.15)

# create random distribution of p1 sale prices
store.df$p1price<-sample(x=c(2.19, 2.29, 2.49, 2.79, 2.99), size=nrow(store.df), replace=TRUE)

#create random distribution of p2 sale prices
store.df$p2price<-sample(x=c(2.29, 2.49, 2.59, 2.99, 3.19), size=nrow(store.df), replace=TRUE)

#create the tmp.sales1 figures randomly distributed around 120
tmp.sales1<-rpois(nrow(store.df), lambda=120)

#create the tmp.sales2 figures randomly distributed around 100
tmp.sales2<-rpois(nrow(store.df), lambda=100)

# set the sales1 figures to be related to the log of p2 price / log p1 price
tmp.sales1<-tmp.sales1 * log(store.df$p2price) / log(store.df$p1price)

# set the sales2 figures to be related to the log of p1 pricde / log p2 price
tmp.sales2<-tmp.sales2 * log(store.df$p1price) / log(store.df$p2price)

# set the p1 sales figures to be lifted by 30% if p1 is being promoted, and make this number absolute using floor
store.df$p1sales<-floor(tmp.sales1 * (1 + store.df$p1prom * 0.3))

# set the p2 sales figures to be lifted by 40% if p2 is being promoted
store.df$p2sales<-floor(tmp.sales2 * (1 + store.df$p2prom * 0.4))

# inspecting the data
library(car)
some(store.df) # look at a random bunch of rows from store.df

table(store.df$p1price) # summary table of factor counts
plot(table(store.df$p1price)) # plot the summary table
table(store.df$p1price, store.df$p1prom) # summary table of p1 price vs p1 prom
p1.table<-table(store.df$p1price, store.df$p1prom) # store summary p1 price/prom in table

# divide total promotions by promotions+price instances, to give the ratio showing how many times product was promoted at each price point
p1.table[, 2] / (p1.table2[, 1] + p1.table2[, 2])
p1.table

# SUMMARISING BY DISTRIBUTION
firstAnalysis<-function(x){
  stats<-matrix(nrow=7, ncol=1, dimnames=list(c("max", "mean", "median", "var", "sd", "IQR", "mad" ), "Numbers"))
  stats["max", ]<-max(x)
  stats["mean", ]<-mean(x)
  stats["median", ]<-median(x)
  stats["var", ]<-var(x)
  stats["sd", ]<-sd(x)
  stats["IQR", ]<-IQR(x)
  stats["mad", ]<-mad(x)
  stats
}

firstAnalysis(store.df$p1sales) # calling our newly created function

# Pulling quantile data - any intervals can be set within the prob argument
quantile(store.df$p1sales, probs=c(0.25, 0.5, 0.75))

# using the summary() method
summary(store.df)

#using the describe() method
library(psych)
describe(store.df)

# using the apply() function to apply mean to multiple columns of our data frame
apply(store.df[,2:9], MARGIN=2, FUN=mean) # columns2-9, margin 1 = by row, 2 = by column
