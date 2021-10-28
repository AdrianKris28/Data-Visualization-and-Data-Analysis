g <- read.csv("video_games.csv", fileEncoding = "UTF-8-BOM")
t <- read.csv("video_games_transaction.csv", fileEncoding = "UTF-8-BOM")


#a. Show the Game Frequency of each year with review score above 70
data1 <- g[g$Review.Score > 70, ]

data1 <- table(data1$Year)
data1 <- data.frame(data1)

barplot(
  data1$Freq,
  names.arg = data1$Var1,
  main = "Game Frequency of each year with review score above 70",
  xlab = "Year",
  ylab = "Frequency",
  ylim = c(0, 200),
  col = rainbow(5)
)

#b. Show the Video game average prices by each year.
data2 <- aggregate(g$Price,list(g$Year), mean)

plot(type="o",
     data2$Group.1,
     data2$x,
     main = "Video game average prices by each year",
     xlab = "Year",
     ylab = "Price",
     col = "green"
     )


#c. Show the Game Frequency Based on Category and labeling the categories based on: 
# 'E' which means 'Everyone'.
# 'M' which means 'Mature'.
# 'T' which means 'Teen'.

data3 <- ifelse(g$Category == "E", "Everyone",
                ifelse(g$Category == "M", "Mature",
                       ifelse(g$Category == "T", "Teen", g$Category)))

data3 <- table(data3)

pie(
  data3,
  main = "Game Frequency Based on Category",
  col = rainbow(3),
  labels = paste(row.names(data3), " ", round((data3 / sum(data3))*100), "%" )
  )


#Apriori

# a. Data Preprocessing
# Remove all missing value.
# Remove all data where category is 'M' and 'T'.
# Remove all data where year is below 2005.
#	Remove all data where sales are under 1.

mergedata <- merge(g, t, by ="GameID")

preprocess_data <- na.omit(mergedata)
preprocess_data <- preprocess_data[preprocess_data$Category != "M" & 
                                     preprocess_data$Category != "T", ]
preprocess_data <- preprocess_data[preprocess_data$Year >= 2005,]
preprocess_data <- preprocess_data[preprocess_data$Sales >= 1,]
View(preprocess_data)

# b. Data Transformation
data_apriori <- split(preprocess_data$Title, preprocess_data$TransactionID)


# c. Data Mining

# Show the frequent games using Apriori algorithm with minimum support: 0.25

#install.packages("arules")
library(arules)

freq_item <- apriori(data_apriori, parameter = 
                          list(
                            support=0.25,
                            target="frequent itemsets"
                          ))

inspect(freq_item)

# Association Rules
#	Show the association rules using minimum confidence: 0.4 
assoc_rules <- ruleInduction(freq_item, confidence=0.4)

inspect(assoc_rules)

