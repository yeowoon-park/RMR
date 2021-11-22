# import libraries --------------------------------------------------------
library(dplyr)
library(readxl)
library(writexl)
library(tidyr)

load("rmr.data.final.Rdata")

rmr.1 <- rmr.data.final %>% filter(year == "2021")
rmr.2 <- rmr.data.final %>% filter(year == "2020" & month > 8)
rmr = rbind(rmr.1, rmr.2)
check <- unique(rmr %>% select(year, month))
rm(check)

rmr.total.q = quantile(rmr$sales, probs = c(0.75, 0.90), na.rm = TRUE)
rmr.total.sum <- rmr %>% filter(chain == "0" & out == "0")%>% group_by(year, month) %>% summarise(mean = mean(sales, na.rm = TRUE), median = median(sales, na.rm = TRUE), 
                                                      upper_25 = quantile(sales, probs = 0.75, na.rm = TRUE), upper_10 = quantile(sales, probs = 0.90, na.rm = TRUE),
                                                      upper_1 = quantile(sales, probs = 0.99, na.rm = TRUE),
                                                      max = max(sales, na.rm = TRUE))

rmr.total.sum2 <- rmr %>% filter(chain == "0" & out == "0")%>% summarise(mean = mean(sales, na.rm = TRUE), median = median(sales, na.rm = TRUE), 
                                                                                                  upper_25 = quantile(sales, probs = 0.75, na.rm = TRUE), upper_10 = quantile(sales, probs = 0.90, na.rm = TRUE),
                                                                                                  upper_1 = quantile(sales, probs = 0.99, na.rm = TRUE),
                                                                                                  max = max(sales, na.rm = TRUE))


write.csv(rmr.total.sum, "result1.csv")
write.csv(rmr.total.sum2, "result2.csv")
