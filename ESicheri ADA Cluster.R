install.packages("cluster")
library(cluster)
install.packages("dplyr")
library(dplyr)

d <- read.csv("QB for Cluster.csv")
head(d)

#Create classifications for raw states in dataset
state.lookup <- data.frame(ship_state=unique(d$ship_state),
                           region="")

w.region <- c("CA","OR","WA", "ID", "NV", "MT", "WY", "UT", "CO", "ND", "SD", "AK")
mw.region <- c("MN", "IA", "MO", "IL", "WI", "IN", "OH", "MI", "KY", "KA", "KS", "NE")
s.region <- c("AR", "OK", "TX", "LA", "MS", "AL", "GA", "FL", "SC", "NC", "TN", "HI", "AZ", "NM")
e.region <- c("ME", "VT", "NH", "DE", "MA", "CT", "NY", "MD", "PA", "NJ", "WV", "RI")

state.lookup <- data.frame(ship_state=c(w.region,mw.region,e.region,s.region),
                           region=c(rep("West",length(w.region)),
                                    rep("Midwest",length(mw.region)), 
                                    rep("East",length(e.region)),
                                    rep("South",length(s.region))))

#add state classifications to main data set
d <- 
  merge(d,
        state.lookup,
        by="ship_state")

#Create 3 clusters
data.for.clust <- d %>% 
  select(subscription_term, months_subscribed, region)

clust <- 
  pam(x=data.for.clust,
      k=3)

summary(clust)
table(clust$clustering)

d$cluster <- clust$clustering

# R doesn't have a mode function
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

#Create table that summarizes the contents of the three clusters
d %>% 
  group_by(cluster) %>% 
  summarize(most.common.term = Mode(subscription_term),
            mean.months = mean(months_subscribed),
            west = sum(region=="West"),
            east = sum(region=="East"),
            south = sum(region=="South"),
            mw = sum(region=="Midwest"))

#Calculated for overall comparison 
Mode(d$subscription_term)
mean(d$months_subscribed)
sum(d$region == "West")
sum(d$region == "East")
sum(d$region == "South")
sum(d$region == "Midwest")

#Plot the differences in variables used between the three clusters
df.subterm <- data.frame(Cluster = c("Cluster 1", "Cluster 2", "Cluster 3"),
                         Freq = c(1,1,3))
plot(df.subterm,
     main = "Most Common Subscription Term \n By Cluster",
     xlab = "",
     ylab = "Number of Months Per Term")

df.meanmonths <- data.frame(Cluster = c("Cluster 1", "Cluster 2", "Cluster 3"),
                            Freq = c(12.16, 1.32, 4.14))
plot(df.meanmonths,
     main = "Average Number of Months Subscribed \n By Cluster",
     xlab = "",
     ylab = "Number of Months")


par(mfrow=c(2,2))
df.west.region <- data.frame(Cluster = c("Cluster 1", "Cluster 2", "Cluster 3"),
                             Freq = c(223, 412, 490))
plot(df.west.region, 
     main = "Number of Customers from the Western US \n By Cluster",
     xlab = "",
     ylab = "Number of Customers")

df.east.region <- data.frame(Cluster = c("Cluster 1", "Cluster 2", "Cluster 3"),
                             Freq = c(108, 285, 296))

plot(df.east.region,
     main = "Number of Customers from the Eastern US \n By Cluster",
     xlab = "",
     ylab = "Number of Customers")

df.south.region <- data.frame(Cluster = c("Cluster 1", "Cluster 2", "Cluster 3"),
                              Freq = c(252, 569, 615))

plot(df.south.region, 
     main = "Number of Customers from the Southern US \n By Cluster",
     xlab = "",
     ylab = "Number of Customers")

df.mw.region <- data.frame(Cluster = c("Cluster 1", "Cluster 2", "Cluster 3"),
                           Freq = c(182, 492, 538))
plot(df.mw.region,
     main = "Number of Customers from the Midwestern US \n By Cluster",
     xlab = "",
     ylab = "Number of Customers")

#Subset by cluster for further analysis down the line
cluster.one <- subset(d, cluster == "1")
cluster.two <- subset(d, cluster == "2")
cluster.three <- subset(d, cluster == "3")

