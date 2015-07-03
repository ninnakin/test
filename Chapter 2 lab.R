library(plyr)
require(gdata)
require(rJava)
library(xlsx)
library(gridExtra)
library(ggplot2)
##############################################################
## Download and clean data for the Manhattan subset
##############################################################

## Download data
mh <- read.xls("C:\\Users\\Ninna\\Documents\\My Dropbox\\doing data science\\rollingsales_manhattan.xls",pattern="BOROUGH",perl="C:\\Strawberry\\perl\\bin\\perl.exe")

## Get statistics for original data
head(mh)
summary(mh)
names(mh)
str(mh)
## Number of records: 27395

## Number of records with valid price:
#remove all not numeric characters
mh$SALE.PRICE.N<-as.numeric(gsub("[^[:digit:]]","",mh$SALE.PRICE))

# 19802 records with non-zero price (72%)
count(mh$SALE.PRICE.N==0)


## Number of records with zero size: 23069 (84%) (gross), 22906 (84%) (land)
count(mh$gross.square.feet==0)
count(mh$land.square.feet==0)

names(mh) <- tolower(names(mh))
mh$gross.sqft<-as.numeric(gsub("[^[:digit:]]","",mh$gross.square.feet))
mh$land.sqft<-as.numeric(gsub("[^[:digit:]]","",mh$land.square.feet))

## Change format of dates and exclude incorrect dates (replace with...?)
mh$sale.date <- as.Date(mh$sale.date)
mh$year.built <- as.numeric(mh$year.built)



## Identify outliers in the data. With regards to: 


## Price
head(mh[mh$sale.price.n==0,])

# histogram for price: shows there are some extreme outliers 
hist(mh$sale.price.n)
# look at quantiles to find out more about distribution
price_quant<-quantile(mh$sale.price.n[mh$sale.price.n>0])
price_perc<-quantile(mh$sale.price.n[mh$sale.price.n>0],c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1))

# check histogram for all prices between the 10th and 90th percentile
hist(mh$sale.price.n[mh$sale.price.n>price_perc[2] & mh$sale.price.n<price_perc[10]])
# bulk of records are cheaper than $1 million

# how many of these have non-zero gross square feet?
count(mh$gross.sqft[mh$sale.price.n>price_perc[2] & mh$sale.price.n<price_perc[10]]>0)
# Only 1527 records
count(mh$land.sqft[mh$sale.price.n>price_perc[2] & mh$sale.price.n<price_perc[10]]>0)

# does the price distribution differ between records with and without non-zero size?
# plot histograms of price for both
hist(mh$sale.price.n[mh$sale.price.n>price_perc[2] & mh$sale.price.n<price_perc[10] & mh$gross.sqft>0])
hist(mh$sale.price.n[mh$sale.price.n>price_perc[2] & mh$sale.price.n<price_perc[10] & mh$gross.sqft==0])

quantile(mh$gross.sqft[mh$sale.price.n>price_perc[2] & mh$sale.price.n<price_perc[10] & mh$gross.sqft>0])

# oddly enough the price seems to be generally lower for records with non-zero size
# original plan to identify association between price and size now seems pointless

# what about the most expensive apartements? do they have size?
quantile(mh$gross.sqft[mh$sale.price.n>=price_perc[10] & mh$gross.sqft>0])
quantile(mh$gross.sqft[mh$sale.price.n>=price_perc[10]])


# New approach: focus on total units (residential + commercial) rather than gross square feet
# something wrong. total.units.n set to 1 when total.units = 0
# why?
mh$total.units.n <- as.numeric(gsub("[^[:digit:]]","",mh$total.units))
mh$res.units.n <- as.numeric(gsub("[^[:digit:]]","",mh$residential.units))
mh$com.units.n <- as.numeric(mh$commercial.units)

# test: is.numeric(mh$total.units)=FALSE

quantile(mh$total.units.n)
hist(mh$total.units.n)
count(mh$total.units.n==0)

quantile(mh$com.units.n[mh$com.units.n>0])
hist(mh$com.units.n[mh$com.units.n>0])
count(mh$com.units.n==0)

quantile(mh$res.units.n[mh$res.units.n>0])
hist(mh$res.units.n[mh$res.units.n>0])
count(mh$res.units.n==0)

# separate records with commercial and residential units?
quantile(mh$total.units.n[mh$sale.price.n>=price_perc[10]])
hist(mh$total.units.n[mh$sale.price.n>=price_perc[10]])

# most apartements are small with only 1 or 2 units 
# connection between number of units and price?

# look at with simple scatter plot
plot(mh$total.units.n[mh$sale.price.n>=price_perc[10]],mh$sale.price.n[mh$sale.price.n>=price_perc[10]])

plot(mh$sale.date[mh$sale.price.n>=price_perc[10]],mh$sale.price.n[mh$sale.price.n>=price_perc[10]])


# nah, no obvious connection
head(mh)


# Further steps:
# look at type of building (with regards to price)

# plot price histogram for all building classes
# how many lack a building class?  3454 (14%)
mh$building.class.n <- substr(mh$building.class.category,1,2)  
mh$building.class.n <- as.numeric(gsub("[^[:digit:]]","",mh$building.class.n ))
count(mh$building.class.n>0)

a <- hist(mh$sale.price.n[mh$building.class.n==1])
b <- hist(mh$sale.price.n[mh$building.class.n==2])
c <- hist(mh$sale.price.n[mh$building.class.n==3])
d <- hist(mh$sale.price.n[mh$building.class.n==4])

ggplot(data=mh, aes(x = sale.price.n, y = gross.sqft, colour=mh$building.class.n))+ geom_line()

grid.arrange(a, b, c, d, main="Histograms for building class 1 to 4")

################

# original data in a 'wide' format
x  <- seq(-2, 2, 0.05)
y1 <- pnorm(x)
y2 <- pnorm(x, 1, 1)
df <- data.frame(x, y1, y2)

# melt the data to a long format
df2 <- melt(data = df, id.vars = "x")

# plot, using the aesthetics argument 'colour'
ggplot(data = df2, aes(x = x, y = value, colour = variable)) + geom_line()

###############

# plot scatterplot of price/units for all types

# look at neighbourhood (with regards to price, no units, type of buildings (commercial or residential))
# plot price histogram for all neighbourhoods
# plot scatterplot of price/units for all neighbourhoods

# for this company: maybe different people should spezialce on different neighborhood7types of buildings?

