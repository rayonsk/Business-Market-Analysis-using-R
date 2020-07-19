library(arules)
library(arulesViz)

TransactionDataSet <-suppressMessages( read.transactions("ElectronidexTransactions2017.csv",format = c( "basket"),header = FALSE, sep = ",",cols = NULL, rm.duplicates = FALSE, quote = "\"'", skip = 0,encoding = "unknown"))

summary(TransactionDataSet)
inspect (TransactionDataSet[1:5])
LIST(TransactionDataSet[1:5])#Lists the transactions by conversion (LIST must be capitalized)
length(TransactionDataSet) # length of transaction
size(TransactionDataSet[1:10]) #No:of items per transaction upto the 10th row

length(itemLabels(TransactionDataSet))# To see the length of item labels.. or lets say the list od label

#There is total of 125 items & our data set has 9835 transactions
itemFrequencyPlot(TransactionDataSet, topN =10, col = rainbow(4),type = "relative")


#In addition to looking at the items, it’s also possible to visualize the entire sparse matrix. 
#To do so, use the image() function. The command to display
#the sparse matrix for the first 10 transactions is as follows:
image(TransactionDataSet[1:10],
      xlab = "Items (Columns)", 
      ylab = "Transactions (Rows)")


image(sample(TransactionDataSet,100))

rules1 <- apriori(TransactionDataSet,parameter = list(supp = 0.005, conf =0.6,minlen = 1,maxlen=10,target = "rules"))
rules1
rules1 <- sort(rules1, by = 'lift')

inspect(rules1[1:5])
#str(rules_df)

summary(rules1)
table(is.redundant(rules1))

topRules <- rules1[1:5]
plot(rules1)

invisible(plot(rules1[1:3], method = "graph", control = list(type= "items")))

plot(rules1, method="paracoord", control=list(reorder=TRUE))
plot(topRules, method = "grouped")

ItemRules <- subset(rules1, items %in% "HP Laptop")
inspect(ItemRules[1:5])

rules_df <- as(rules1, "data.frame")
str(rules_df)
write(rules1, file = "rules_df.csv", sep = ",", quote = TRUE, row.names = FALSE)

rules_highest_lhs <- apriori(data= TransactionDataSet, parameter = list(supp = 0.01, conf=0.2),appearance = list(default = "rhs", lhs = "iMac"))
rules_highest_lhs
rules_highest_lhs <- sort(rules_highest_lhs, by ="lift")
inspect(rules_highest_lhs)
plot(rules_highest_lhs, method="paracoord", control=list(reorder=TRUE))

rules_highest_lhs <- apriori(data= TransactionDataSet, parameter = list(supp = 0.03, conf=0.2),appearance = list(default = "rhs", lhs = "HP Laptop"))
rules_highest_lhs
rules_highest_lhs <- sort(rules_highest_lhs, by ="lift")
inspect(rules_highest_lhs)
plot(rules_highest_lhs, method="paracoord", control=list(reorder=TRUE))