
#Segmentation

#Statistical segmentation


head(data)
#Create three variables using SQL syntax via R
library(sqldf)

customers<- sqldf("SELECT customer_id,
                  MIN(days_since) AS 'recency',
                  COUNT(*) AS 'frequency',
                  AVG(purchase_amount) AS 'amount'
                  FROM data GROUP BY 1")
                
        

# Quick EDA using base R graphics

hist(customers$recency)
hist(customers$frequency)
hist(customers$amount)
# Can't see much, let's use GGPLOT2 to edit the Y axis
ggplot(customers, aes(x= amount)) + geom_histogram() + 
    coord_cartesian(y= c(0,50))
# Amount is highly skewed with possible outliers

# Create new data frame with existing dataset

new_data<- customers
nd<- new_data
# A little manipulation to shape it up
row.names(nd)<- nd$customer_id
nd$customer_id<- NULL
head(nd)

# Log amount
nd$amount<- log(nd$amount)
hist(nd$amount)
# That's a distribution we can work with

# Scale the variables
nd<- scale(nd)

# Create a sample out of 10% of the data
sample<- seq(1, 18417, by= 10)
nds<- nd[sample, ]
cs<- customers[sample, ]
nrow(nds)
nrow(cs)
# Distance calculation
d<- dist(nds)
# Hierarchical clustering
c<- hclust(d, method= "ward.D2")
# Plot the dendrogram
plot(c)

# Cut the tree
members<- cutree(c, k= 9)
table(members)

aggregate(cs[, 2:4], by=list(members), mean)

# Another clustering segmentation example

seg.raw <- read.csv("http://goo.gl/qw303p")
table(seg.raw$Segment)
# Now remove
seg.df<- seg.raw[, -7]
summary(seg.df)
library(cluster)
# Calculate distance
seg.dist<- daisy(seg.df)
as.matrix(seg.dist)[1:5, 1:5]

# Apply algorithm
seg.hc<- hclust(seg.dist, method= "complete")

# Cut the tree
plot(cut(as.dendrogram(seg.hc), h=0.5)$lower[[1]])
# Now, what do we have?
# Similar 
seg.df[c(256, 287),]
# Not so similar
seg.df[c(129, 173),]

# Compare dendrogram to the distance matrix
cor(cophenetic(seg.hc), seg.dist)
# 0.7682436

plot(seg.hc)
rect.hclust(seg.hc, k=4, border="blue")

# Membership                
seg.hc.segment <- cutree(seg.hc, k=4)  
table(seg.hc.segment)

# Aggregate the numerical variables
seg.df.rev<- seg.df %>% select(age, income, kids)
aggregate(seg.df.rev, by= list(seg.hc.segment), mean)



# Managerial segmentation               
customers_2015<- sqldf("SELECT customer_id,
                               MIN(days_since) AS 'recency',
                       MAX(days_since) AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM data GROUP BY 1")                


customers_2015$segment = "NA"
customers_2015$segment[which(customers_2015$recency > 365*3)] = "inactive"
customers_2015$segment[which(customers_2015$recency <= 365*3 & customers_2015$recency > 365*2)] = "cold"
customers_2015$segment[which(customers_2015$recency <= 365*2 & customers_2015$recency > 365*1)] = "warm"
customers_2015$segment[which(customers_2015$recency <= 365)] = "active"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$first_purchase <= 365*2)] = "new warm"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount < 100)] = "warm low value"
customers_2015$segment[which(customers_2015$segment == "warm" & customers_2015$amount >= 100)] = "warm high value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$first_purchase <= 365)] = "new active"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount < 100)] = "active low value"
customers_2015$segment[which(customers_2015$segment == "active" & customers_2015$amount >= 100)] = "active high value"
table(customers_2015$segment)

aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

customers_2015$segment = factor(x = customers_2015$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))table(customers_2015$segment)
aggregate(x = customers_2015[, 2:5], by = list(customers_2015$segment), mean)

customers_2014<- sqldf("SELECT customer_id,
                               MIN(days_since) - 365 AS 'recency',
                       MAX(days_since) - 365 AS 'first_purchase',
                       COUNT(*) AS 'frequency',
                       AVG(purchase_amount) AS 'amount'
                       FROM data
                       WHERE days_since > 365
                       GROUP BY 1")
customers_2014$segment = "NA"
customers_2014$segment[which(customers_2014$recency > 365*3)] = "inactive"
customers_2014$segment[which(customers_2014$recency <= 365*3 & customers_2014$recency > 365*2)] = "cold"
customers_2014$segment[which(customers_2014$recency <= 365*2 & customers_2014$recency > 365*1)] = "warm"
customers_2014$segment[which(customers_2014$recency <= 365)] = "active"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$first_purchase <= 365*2)] = "new warm"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount < 100)] = "warm low value"
customers_2014$segment[which(customers_2014$segment == "warm" & customers_2014$amount >= 100)] = "warm high value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$first_purchase <= 365)] = "new active"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount < 100)] = "active low value"
customers_2014$segment[which(customers_2014$segment == "active" & customers_2014$amount >= 100)] = "active high value"


customers_2014$segment<- factor(x = customers_2014$segment, levels = c("inactive", "cold",
                                                                       "warm high value", "warm low value", "new warm",
                                                                       "active high value", "active low value", "new active"))
table(customers_2014$segment)

aggregate(x = customers_2014[, 2:5], by = list(customers_2014$segment), mean)

revenue_2015<- sqldf("SELECT customer_id, SUM(purchase_amount) AS 'revenue_2015'
                      FROM data
                     WHERE year_of_purchase = 2015
                     GROUP BY 1")
summary(revenue_2015)

actual<-  merge(customers_2015, revenue_2015, all.x = TRUE)
actual$revenue_2015[is.na(actual$revenue_2015)] = 0

aggregate(x = actual$revenue_2015, by = list(customers_2015$segment), mean)

forward<-  merge(customers_2014, revenue_2015, all.x = TRUE)
forward$revenue_2015[is.na(forward$revenue_2015)] = 0

r<-  aggregate(x = forward$revenue_2015, by = list(customers_2014$segment), mean)

r %>% arrange(desc(x))

#End
                                                                                                   