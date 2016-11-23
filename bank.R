library(readr)
library(dplyr)
library(magrittr)
library(readxl)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(leaflet)
library(networkD3)
library(zoo)

bank_data_path <- "C:/Users/imran/Documents/R projects/Bank data/saver account.csv"
bank <- read_csv(bank_data_path, col_names = TRUE, skip = 0, col_types = NULL)
colnames(bank) <- tolower(colnames(bank)) 

head(bank)
dim(bank)
str(bank)
summary(bank)

mean(bank$amount) #returns mean of column "amount"
unique(bank$subcategory) #returns list of unique subcategories
unique(bank$memo) #returns list of unique memos
table(bank$subcategory) #returns number of each subcategory
table(bank$amount) 

getwd()
setwd("C:/Users/imran/Documents/R projects/Bank data")

#adds proper proper date, weekday, month and year columns
bank <- bank %>% 
  mutate(date.clean = dmy(date)) %>%
  mutate(weekday = wday(date.clean, label = TRUE, abbr = FALSE)) %>%
  mutate(week = week(date.clean)) %>%
  mutate(month = month(date.clean, label = T, abbr = F)) %>%
  mutate(year = year(date.clean)) %>%
  mutate(month_year = as.yearmon(date.clean))

money_out <- bank %>% filter(amount < 0)
money_in <- bank %>% filter(amount > 0)

#average number of transactions on a particular day
bank %>% 
  group_by(date.clean, weekday) %>% 
  summarise(transactions = n()) %>% 
  group_by(weekday) %>% 
  summarise(avg_transactions = mean(transactions))

#number of transactions in a week
week_transaction <- bank %>% 
  group_by(date.clean, week) %>% 
  summarise(transactions = n()) %>% 
  group_by(week) %>% 
  summarise(transactions_week = sum(transactions))

#number of transactions in a particular month
bank %>%
  group_by(month, date.clean) %>%
  summarise(transactions = n()) %>%
  summarise(transactions_month = sum(transactions))

#number of transactions and average transaction in a particular year
bank %>%
  group_by(year) %>%
  summarise(transactions = n(), amt = sum(amount))

credit <- bank %>% filter(subcategory == "CRE") #returns all data with subcategory "CRE"
payments <- bank %>% filter(subcategory == "PAYMENT")
FT <- bank %>% filter(subcategory == "FT")

train <- bank[grep("chiltern", bank$memo, ignore.case = T), ] #returns all data with "chiltern" in the memo, not case sensitive
tube <- bank[grep("ticket|oyster", bank$memo, ignore.case = T), ]
tuition <- bank[grep("lets", bank$memo, ignore.case = T), ]
yoyo <- bank[grep("yoyo", bank$memo, ignore.case = T), ]
tesco <- bank[grep("tesco", bank$memo, ignore.case = T), ]
sainsburys <- bank[grep("sains", bank$memo, ignore.case = T), ]

yoyo <- yoyo %>%
  mutate(date = date.clean) %>%
  mutate(date.clean = NULL) #delete date.clean column

yoyo <- yoyo[c(1,2,3,4,5,7,6)] #reorder columns

FT_out <- money_out %>% filter(subcategory == "FT")

spending <- money_out %>% filter(subcategory != "FT") #all the money that I've spent
everyday_spending <- spending %>% filter(amount >= -50)

everyday_expenditure <- everyday_spending %>%
  group_by(month_year) %>%
  summarise(transactions = n(), total_spent = -sum(amount), avg = -mean(amount))

expenditure <- spending %>%
  group_by(month_year) %>%
  summarise(transactions = n(), total_spent = -sum(amount), avg = -mean(amount))

#histogram
qplot(x = as.numeric(-amount), data = everyday_spending, 
      main = "Histogram of transactions", 
      xlab = "Amount (£)", ylab = "Count",
      fill = I("steelblue"), colour = I("white"),
      binwidth = 0.5)

#scattergraph
qplot(y = as.numeric(-amount), x = date.clean, data = everyday_spending,
      ylab = "Amount (£)", xlab = "Date", geom = c("point", "smooth"))
      
qplot(y = -as.numeric(amount), x = week, data = everyday_spending,
      ylab = "Amount (£)", xlab = "Date", geom = c("point", "smooth"),
      shape = subcategory, colour = subcategory)

#line of best fit
m = as.numeric(lm(everyday_expenditure$total_spent ~ everyday_expenditure$month_year)$coefficients[2])      
c = as.numeric(lm(everyday_expenditure$total_spent ~ everyday_expenditure$month_year)$coefficients[1])
qplot(y = as.numeric(total_spent), x = as.numeric(month_year), data = everyday_expenditure,
      ylab = "Amount (£)", xlab = "Year") + geom_abline(intercept = c, slope = m, colour = "red", size = 1)

m2 = as.numeric(lm(expenditure$total_spent ~ expenditure$month_year)$coefficients[2])      
c2 = as.numeric(lm(expenditure$total_spent ~ expenditure$month_year)$coefficients[1])
qplot(y = as.numeric(total_spent), x = as.numeric(month_year), data = expenditure,
      ylab = "Amount (£)", xlab = "Year") + geom_abline(intercept = c2, slope = m2, colour = "red", size = 1)

#polynomial fit
lm(expenditure$total_spent ~ poly(expenditure$month_year, 3, raw=T))

#boxplots
qplot(x = month, y = -as.numeric(amount), 
      data = everyday_spending, geom = "boxplot", ylab = "Amount (£)")

#barcharts
ggplot(data = expenditure, aes(x = week, y = total_spent)) + 
  xlab("Week") +
  ylab("Expenditure") +
  geom_bar(stat = "identity", fill = "blue") +
  #theme(axis.text.x = element_text(angle = 0)) +
  theme(#axis.title.y = element_blank(),
        #axis.ticks.y = element_blank(),
        #axis.text.y = element_blank(),
        text = element_text(size = 14),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        #         element_line(colour = "lightgrey",
        #                                         linetype = "dotted"),
        #panel.grid.major.y = element_blank(),
        #panel.grid.minor.y = element_blank(),
        panel.margin.y = unit(0.1, units = "in"),
        panel.background = element_rect(fill = "white", colour = "lightgrey"))
