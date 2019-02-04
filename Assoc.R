library(arules)
library(arulesViz)
library(tidyverse)
library(plyr)
library(ggplot2)
library(knitr)
library(lubridate)
library(RColorBrewer)



CustomerPurchases <- read.csv("D:/Data_Input/ARules/CustPuchHistoryAUCANZ.csv")

CustomerPurchases <- CustomerPurchases[complete.cases(CustomerPurchases), ] #all data is complete

names(CustomerPurchases) <- c("TrxCode", "Product", "UnitsSold", "SellDate", "TotalSell", "CustomerID", "Region", "PurchSeqNo")

str(CustomerPurchases)

#Transform field types

CustomerPurchases$SellDate <- as.Date(CustomerPurchases$SellDate) 

CustomerPurchases$TrxCode <- as.character(CustomerPurchases$TrxCode)

glimpse(CustomerPurchases)


#ddply(dataframe, variables_to_be_used_to_split_data_frame, function_to_be_applied)
#The R function paste() concatenates vectors to character and separated results using collapse=[any optional charcater string ]. Here ',' is used

CustomerData <- ddply(CustomerPurchases, "CustomerID",
                      function(df1)paste(df1$Product,
                                         collapse = ", "))
  
#Null Customer Number

CustomerData$CustomerID <- NULL

#Label Sub Category Column

colnames(CustomerData) <- c("Product")


#Change to transaction format

#CustomerTransactions <- data.frame(do.call('rbind',strsplit(as.character(CustomerData), ", ", fixed = TRUE)))



write.csv(CustomerData, "D:/Data_Input/ARules/CustomerBasket.csv", quote = FALSE, row.names = TRUE)


####################################Read in file###########################################

#sep tell how items are separated. In this case you have separated using ','

tr <- read.transactions('D:/Data_Input/ARules/CustomerBasket.csv', format = 'basket', sep=',')



itemFrequencyPlot(tr,topN=20,type="absolute",col=brewer.pal(8,'Pastel2'), main="Absolute Item Frequency Plot")



itemFrequencyPlot(tr,topN=20,type="relative",col=brewer.pal(8,'Pastel2'), main="Relative Item Frequency Plot")


# Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))


summary(association.rules)


inspect(association.rules)


bridal.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.5),appearance = list(default="lhs",rhs="Engagement - Bridal"))

inspect(bridal.association.rules)

wedders.association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.5),appearance = list(default="lhs",rhs="Wedders - Bridal"))

inspect(wedders.association.rules)




###########################
#CustomerData
#
#
#  
#CustomerDateData 
#
#InvoiceDateData
#
#CustomerPurchases$Sub.Category...Category
#
#
#CustomerPurchases <- lapply(CustomerPurchases, function(x){as.factor(x)})
#
#str(CustomerPurchases)
#
#rules <- apriori(CustomerPurchases)
#