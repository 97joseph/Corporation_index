#The CT Corporate INDEX

setwd("~/Forecasting/Boot")
options(scipen = 99999, digits = 3, knitr.table.format = "rst", length = 120)

remove(list = ls())  # clear the environment panel

#============================================
#load libraries
library(tidyverse)
library(pdfetch)
library(lubridate)  
library(ggplot2)
  # ============================ #
  hd = rownames_to_column(data.frame(pdfetch_YAHOO("HD", fields = c("close"), 
                                                   from = as.Date("2010-01-01"), to = Sys.Date(), interval = "1m")))
  wba = rownames_to_column(data.frame(pdfetch_YAHOO("WBA", fields = c("close"), 
                                                    from = as.Date("2010-01-01"), to = Sys.Date(), interval = "1m")))
  lmt = rownames_to_column(data.frame(pdfetch_YAHOO("LMT", fields = c("close"), 
                                                    from = as.Date("2010-01-01"), to = Sys.Date(), interval = "1m")))
  pbct = rownames_to_column(data.frame(pdfetch_YAHOO("PBCT", fields = c("close"), 
                                                     from = as.Date("2010-01-01"), to = Sys.Date(), interval = "1m")))
  
  stocks = cbind(hd, wba, lmt, pbct) %>% select(1, c(HD, WBA,LMT, PBCT))
  head(stocks)
head(hd)



stockdata = stocks %>% gather(measure, values, -rowname, na.rm=TRUE) 

      stockdata = stocks %>% pivot_longer(cols = c(HD, WBA,LMT, PBCT), names_to = "measure",  
                                          values_to = "values")
      head(stockdata)
  
stockdata = stockdata %>% 
  rename(date = rowname) 

stockdata$date = ymd(stockdata$date)  

stockdata_short = stockdata %>% 
  filter(date >= today() - years(2))

head(stockdata_short)

stockdata2 = stockdata_short %>% group_by(measure) %>%
  #arrange(date) %>%
  dplyr::mutate(growth = values - first(values), 
         growth_percent = (values - first(values))/first(values)*100) 

stockdata2 %>% ggplot(aes(x = date, y = growth, col = measure)) + 
  geom_line() 

stockdata2 %>% ggplot(aes(x = date, y = growth_percent, col = measure)) + 
  geom_line() + 
  geom_smooth(se = FALSE)


stockdata3 = stockdata_short %>% group_by(measure) %>%
  arrange(date) %>%
  mutate(growth = values - first(values), growth_percent = (values - first(values))/first(values)*100, 
         Index = values*100/first(values)
  ) 

head(stockdata3)  
stockdata3 %>% ggplot(aes(x = date, y = Index, col = measure)) + 
  geom_line()  +
  labs(title = "my graph", subtitle = "arods class")

# ===============================

head(stockdata4)

stockdata4 = stockdata3 %>% select(date, measure, values) %>%
  pivot_wider(names_from = measure, 
                           values_from = values) 
  #select(HD,WBA, LMT, PBCT) 
  
stockdata4$BigfatIndex = rowSums(stockdata4[2:5], dims = 1)/4



stockdata4 %>% ggplot(aes(x = date, y = BigfatIndex, col = "darkred")) + 
  geom_line()  +
  labs(title = "my graph", subtitle = "arods class")

first(stockdata4$date)
last(stockdata4$date)
BigFatIndex = ts(stockdata4$BigfatIndex, start = c(2019, 01), end = c(2021, 9), frequency = 12)
str(BigFatIndex)
library(dygraphs)
dygraph(BigFatIndex) %>% dyRangeSelector()

# ============================================  #
# Deliverable: The Bet Part 2 of 3

# Assemble all 5 series chosen by Paul Ehrlich in The Bet.
# Graph them simultaneously after calculating growth in levels relative to the first day.
# Graph them simultaneously after calculating growth percent relative to the first day.
# Graph them simultaneously after constructing an index relative to the first day.

# Convert the last one to a pdf and upload to the bucket labeled TheBet_2

# ============================================  #

# ============================================  #
# Deliverable: The Bet Part 3 of  3

# Construct and Equally Weighted Index of the metal prices. 
# Lets call it the Metals Index.

# Create a ggplot of the Metals Index

# Create a dygraph of the Metals Index

# Convert the last one to a pdf and upload to the bucket labeled TheBet_2

# ============================================  #




