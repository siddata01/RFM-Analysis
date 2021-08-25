#reading the file

require(readr)
online_retail <- read_csv("Online_Retail.csv")
View(online_retail)
str(online_retail)


####checking for na values
any(is.na(online_retail)) #No NA values found


#####Checking for duplicate values
require(dplyr)

online_retail <- mutate_if(online_retail, is.character, toupper)

nrow(online_retail) == n_distinct(online_retail)  ##False, thus duplicate rows exist


#Dataset with duplicate rows as well as its respective original one
duplicate_or <- online_retail[which(duplicated(online_retail)),]

#Checking the percentage of duplicate rows
(nrow(duplicate_or)/nrow(online_retail))*100       #1.2848 % of our data

n_distinct(duplicate_or)  #duplicates rows of the duplicates exist too 

#It maybe due to improper data-collection
#We have to remove these rows as they are just duplicates 

online_retail <- unique.data.frame(online_retail)



######univariate analysis ######
str(online_retail)


#####Invoice No.
n_distinct(online_retail$InvoiceNo)

####StockCode
n_distinct(online_retail$StockCode)

######Quantity
summary(online_retail$Quantity)   #negative values in the quantity column

negative_or <- online_retail[which(online_retail$Quantity <= 0),]
View(negative_or)

nrow(negative_or)/nrow(online_retail)*100   #2.209% of our data

#There is no specific pattern to the negative data
#The negatives may be due to returns or refunds and thus doesn't contribute to RFM analysis


online_retail <- online_retail[which(online_retail$Quantity >= 1),]


plot(quantile(online_retail$Quantity, seq(0, 1, 0.01)))
quantile(online_retail$Quantity, seq(0, 1, 0.01))

View(online_retail[which(online_retail$Quantity >= 1534), ])

tail(quantile(online_retail$Quantity, seq(0,1, 0.0001)), n = 100)

online_retail$Quantity[which(online_retail$Quantity > 120)] <- 120

######Invoice date 

# Changed to compatible format
require(lubridate)
online_retail$InvoiceDate <-  mdy_hm(online_retail$InvoiceDate)


#Unit-price
summary(online_retail$UnitPrice)

plot(quantile(online_retail$UnitPrice, seq(0, 1, 0.01)))
tail(quantile(online_retail$UnitPrice, seq(0, 1, 0.0001)), n = 100)
quantile(online_retail$UnitPrice, seq(0, 1, 0.01))
online_retail$UnitPrice[which(online_retail$UnitPrice > 14.95)] <- 14.95


#customer_id should be as character/factor

online_retail$CustomerID <- as.character(online_retail$CustomerID)
n_distinct(online_retail$CustomerID)


######Generating new features

### Date
#YYYY-MM-DD
online_retail$date <- date(online_retail$InvoiceDate)

####No. of days - difference in days between current and last ever transaction

max(online_retail$date) 
#since 2011-12-09 is the last date for any transaction
#Thus our reference will be 2011-12-10 so that no zero values for recency comes

online_retail$No._of_days <- as.Date("2011-12-10") - as.Date(online_retail$date)
online_retail$No._of_days <- as.numeric(online_retail$No._of_days)

####total amount
online_retail$total_amount <- online_retail$Quantity*online_retail$UnitPrice


####Average spending and Frequency
online_retail_2 <- group_by(online_retail, CustomerID ) %>% 
                        mutate(recency = min(No._of_days)) %>% 
                        mutate(total_amount = sum(total_amount)) %>% 
                        mutate(avg_spending = total_amount/n_distinct(InvoiceNo)) %>% 
                        mutate(frequency = n_distinct(InvoiceNo))  %>%
                        ungroup()



##### Creating Final data frame for analysis
final_data <- online_retail_2[, c(6,9,10,11,12)]
final_data <- unique.data.frame(final_data)


####scaling
df <- mutate_if(final_data, is.numeric, scale)



#######Building Models
df <- df[, -1]

r <- rep(0, 20)
r

for (k in 1:20) { cluster <- kmeans(df, centers = k, nstart = 50)
r[k] <- cluster$betweenss/cluster$totss 
}


round(r, 2)

plot(r, type = 'l')

#optimal value of k = 4

clus_4 <- kmeans(df, centers = 4, iter.max = 50, nstart = 50)

final_data$cluster <- as.factor(clus_4$cluster)


###visualization
final_data <- group_by(final_data, cluster) %>% summarise(avg_TA = mean(total_amount),
                                                          avg_recency = mean(recency),
                                                          avg_frequency = mean(frequency),
                                                          mean_spending = mean(avg_spending))

require(ggplot2)
ggplot(final_data, aes(x = avg_TA, y = avg_recency,
                       size = mean_spending,
                       color = avg_frequency,
                       shape = cluster)) + geom_point()

write.csv(final_data, "k-means(Online_retail).csv", row.names = F)




