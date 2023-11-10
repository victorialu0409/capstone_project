#rm(list=ls())
#bankrupt <- read.csv('/Users/victorialu/Desktop/Datasets/american_bankruptcy.csv')
bankrupt <- read.csv('/capstone_project/datasets/american_bankruptcy.csv')

head(bankrupt)
nrow(bankrupt) #78682 entries 
ncol(bankrupt) #78682 entries 

#data visualization
bankrupt_counts <- table(bankrupt$status_label)

barplot(bankrupt_counts, 
        xlab = "alive/failed",
        ylab = "Frequency", 
        ylim = c(0, 90000))

year_counts <- table(bankrupt$year)

barplot(year_counts, 
        xlab = "year",
        ylab = "Frequency", 
        ylim = c(0, 6000))

#checking for duplicates and na values
sum(duplicated(bankrupt)) #no duplicates 
sum(is.na(bankrupt)) #no na values 

#keeping relevant data
#dropping the years 1999, 2004, 2007, and 2015 due to ambiguity
bankrupt.filtered <- bankrupt[!(bankrupt$year %in% c(1999, 2004, 2007, 2015)), ]
nrow(bankrupt.filtered) #61806 entries remaining

#randomly select one entry per company
library(dplyr)
set.seed(123)
b.final <- data.frame(bankrupt.filtered %>%
  group_by(company_name) %>%
  sample_n(1))[,-1]
head(b.final)
nrow(b.final) #8462 entries remaining 

#calculating new financial metrics
# X1	Current assets - All the assets of a company that are expected to be sold or used as a result of standard
# X2	Cost of goods sold 
# X3	Depreciation and amortization - 
# X4	EBITDA - Earnings before interest, taxes, depreciation, and amortization. 
# X5	Inventory
# X6	Net Income 
# X7	Total Receivables
# X8	Market value - The price of an asset in a marketplace. In this dataset, it refers to the market
# capitalization since companies are publicly traded in the stock market.
# X9	Net sales - The sum of a company's gross sales minus its returns, allowances, and discounts.
# X10	Total assets 
# X11	Total Long-term debt 
# X12	EBIT 
# X13	Gross Profit - The profit a business makes after subtracting all costs
# X14	Total Current Liabilities - The sum of accounts payable, accrued liabilities, and taxes such as Bonds
# payable at the end of the year, salaries, and commissions remaining.
# X15	Retained Earnings
# X16	Total Revenue - The amount of income that a business has made from all sales before subtracting expenses.
# It may include interest and dividends from investments.
# X17	Total Liabilities 
# X18	Total Operating Expenses

#liquidity ratios 
b.final$Current_Ratio<-b.final$X1/b.final$X14
b.final$Quick_Ratio<-(b.final$X1 - b.final$X5) / b.final$X14

#profitability ratios
b.final$Gross_Profit_Margin<-b.final$X13/b.final$X9 * 100
b.final$Net_Income_Margin<-(b.final$X6/b.final$X9) * 100
b.final$Return_on_Assets<-(b.final$X6/b.final$X10) * 100
b.final$Return_on_Equity <- (b.final$X6/(b.final$X10 - b.final$X17)) * 100

#leverage ratios  
#some companies have no long-term debt, add 0.01 to facilitate calculation 
b.final$Debt_to_Assets_Ratio <- (b.final$X11 + 0.01)/ b.final$X10
b.final$Interest_Coverage_Ratio <- b.final$X12 / (b.final$X11 + 0.01)
b.final$Debt_to_EBITDA_Ratio<-(b.final$X11 + 0.01)/ b.final$X12
b.final$Debt_Service_Coverage_Ratio <-(b.final$X12 + b.final$X11 + 0.01) / (b.final$X11 + 0.01)

#efficiency ratios 
b.final$Asset_Turnover_Ratio<-b.final$X9/b.final$X10
b.final$Inventory_Turnover_Ratio<-b.final$X9 / (b.final$X5  + 0.01)
b.final$Receivables_Turnover_Ratio<-b.final$X9/ (b.final$X7 + 0.01)
b.final$Fixed_Asset_Turnover_Ratio<-b.final$X9 / (b.final$X10 - b.final$X1 + 0.01)
b.final$Accounts_Payable_Turnover_Ratio<- b.final$X9 / (b.final$X14 - b.final$X5 + 0.01)
b.final$Operating_Profit_Margin <- (b.final$X12 / b.final$X16) * 100

#other 
b.final$EBITDA_Margin<-(b.final$X4/b.final$X9) * 100
b.final$EBIT_Margin <-(b.final$X12/b.final$X9) * 100
b.final$Price_to_Book_Ratio <- b.final$X8 / ((b.final$X10 - b.final$X17) / b.final$X8)
b.final$EV_to_EBITDA_Ratio <- (b.final$X8 + b.final$X11 - (b.final$X10 - b.final$X1)) / b.final$X12
#b.final$Retention_Ratio<- (b.final$X15+0.01) /(b.final$X6 + 0.01)

#get rid of the existing data
b.final <- b.final[, -c(3:20)] 

#encoding easing and hiking years
b.final$cycle_type <- NA  
b.final$cycle_type[b.final$year %in% c(2000, 2005, 2006, 2016, 2017, 2018)] <- 1 #hiking cycle, 1
b.final$cycle_type[b.final$year %in% c(2001, 2002, 2003, 2008, 2009, 2010, 2011, 2012, 2013, 2014)] <- 0 #easing cycle, 0
head(b.final)
ncol(b.final)
nrow(b.final)

#other cleaning-for Python processing
b.final$status_label <- ifelse(b.final$status_label == "alive", 1, 0)
# check which columns contain Inf values
columns_with_inf <- sapply(b.final, function(col) any(is.infinite(col)))
#print the names of columns with Inf values
names(b.final[columns_with_inf])

b.final <- b.final[, !names(b.final) %in% "year"]
nrow(b.final)
ncol(b.final)

#calculate class distribution
class_distribution <- table(b.final$status_label)
class_distribution
#603 7862 

#plot the class distribution using a bar plot
plot_class <- barplot(class_distribution, 
        main = "Class Distribution in the Cleaned Dataset",
        xlab = "Class",
        ylab = "Count",
        ylim = c(0, 8500))


# Create the full path
save the cleaned file
#file_path <- '/Users/victorialu/Desktop/Datasets/cleaned_bankruptcy.csv'
file_path <- '/capstone_project/datasets/cleaned_bankruptcy.csv'
write.csv(b.final, file = file_path, row.names = FALSE)