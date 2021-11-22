
# import libraries --------------------------------------------------------
library(plyr)
library(dplyr)
library(readxl)
library(writexl)
library(fastDummies)
# load sales volume data of Market Kurly-------------------------------------------------------------------------
sales <- read_excel("salestotal.xlsx", sheet="Data", range="A1:BE4434", col_names=TRUE, col_types="guess", na="NA")
product <- read_excel("rmr763.xlsx", sheet="data", range="A1:BA764", col_names=TRUE, col_types="guess", na="NA")
#pricenew <- read_excel("newprice.xlsx", sheet="Sheet1", range="A1:B4023", col_names=TRUE, col_types="guess", na="NA")


#pp <- left_join(product, pricenew,  by = c('product'='product'))
#write_xlsx(pp, path = "product_price_match.xlsx")

# generating sustainability variables
product <- product %>% 
  mutate(sustain_pname =
           (ifelse(grepl('복지', product),'동물복지',
                   ifelse(grepl('무항생', product),'무항생제',
                          ifelse(grepl('유기', product),'유기농',
                                 ifelse(grepl('베지|배지|비건|고기대신|식물성|무빙마운틴|비욘드미트|언리미트|제로미트|인테이크', product),'채식','일반')))))) %>% 
  mutate(sustain_yn = 
           ifelse(sustain_pname == '일반', '0', '1'))


#productinfo
summary(product$price)
summary(product$g)
product %>% group_by(storage) %>% summarise(n = n()/760)
product %>% group_by(recipe) %>% summarise(n = n()/760)
product %>% group_by(mprocat) %>% summarise(n = n()/760)
category <- product %>% group_by(cat_small2) %>% summarise(n = n()/760)
sum((product$kurly_only)/760)
sum((product$out)/760)

product %>% group_by(loc) %>% summarise(n = n())
sum((product$chain)/760)
product %>% group_by(storetype_new) %>% summarise(n = n()/760)
product %>% group_by(onmenu) %>% summarise(n = n())
product %>% group_by(maindish) %>% summarise(strtype = n()/322)

#2017
X2017_1 <- sales %>% select(product, X2017_1) %>% rename("volume"="X2017_1") %>% mutate(year = 2017) %>% mutate(month = 1) 
X2017_2 <- sales %>% select(product, X2017_2) %>% rename("volume"="X2017_2") %>% mutate(year = 2017) %>% mutate(month = 2)
X2017_3 <- sales %>% select(product, X2017_3) %>% rename("volume"="X2017_3") %>% mutate(year = 2017) %>% mutate(month = 3)
X2017_4 <- sales %>% select(product, X2017_4) %>% rename("volume"="X2017_4") %>% mutate(year = 2017) %>% mutate(month = 4)
X2017_5 <- sales %>% select(product, X2017_5) %>% rename("volume"="X2017_5") %>% mutate(year = 2017) %>% mutate(month = 5)
X2017_6 <- sales %>% select(product, X2017_6) %>% rename("volume"="X2017_6") %>% mutate(year = 2017) %>% mutate(month = 6)
X2017_7 <- sales %>% select(product, X2017_7) %>% rename("volume"="X2017_7") %>% mutate(year = 2017) %>% mutate(month = 7)
X2017_8 <- sales %>% select(product, X2017_8) %>% rename("volume"="X2017_8") %>% mutate(year = 2017) %>% mutate(month = 8)
X2017_9 <- sales %>% select(product, X2017_9) %>% rename("volume"="X2017_9") %>% mutate(year = 2017) %>% mutate(month = 9)
X2017_10 <- sales %>% select(product, X2017_10) %>% rename("volume"="X2017_10") %>% mutate(year = 2017) %>% mutate(month = 10)
X2017_11 <- sales %>% select(product, X2017_11) %>% rename("volume"="X2017_11") %>% mutate(year = 2017) %>% mutate(month = 11)
X2017_12 <- sales %>% select(product, X2017_12) %>% rename("volume"="X2017_12") %>% mutate(year = 2017) %>% mutate(month = 12)

#2018
X2018_1 <- sales %>% select(product, X2018_1) %>% rename("volume"="X2018_1") %>% mutate(year = 2018) %>% mutate(month = 1) 
X2018_2 <- sales %>% select(product, X2018_2) %>% rename("volume"="X2018_2") %>% mutate(year = 2018) %>% mutate(month = 2) 
X2018_3 <- sales %>% select(product, X2018_3) %>% rename("volume"="X2018_3") %>% mutate(year = 2018) %>% mutate(month = 3) 
X2018_4 <- sales %>% select(product, X2018_4) %>% rename("volume"="X2018_4") %>% mutate(year = 2018) %>% mutate(month = 4) 
X2018_5 <- sales %>% select(product, X2018_5) %>% rename("volume"="X2018_5") %>% mutate(year = 2018) %>% mutate(month = 5) 
X2018_6 <- sales %>% select(product, X2018_6) %>% rename("volume"="X2018_6") %>% mutate(year = 2018) %>% mutate(month = 6) 
X2018_7 <- sales %>% select(product, X2018_7) %>% rename("volume"="X2018_7") %>% mutate(year = 2018) %>% mutate(month = 7) 
X2018_8 <- sales %>% select(product, X2018_8) %>% rename("volume"="X2018_8") %>% mutate(year = 2018) %>% mutate(month = 8) 
X2018_9 <- sales %>% select(product, X2018_9) %>% rename("volume"="X2018_9") %>% mutate(year = 2018) %>% mutate(month = 9) 
X2018_10 <- sales %>% select(product, X2018_10) %>% rename("volume"="X2018_10") %>% mutate(year = 2018) %>% mutate(month = 10) 
X2018_11 <- sales %>% select(product, X2018_11) %>% rename("volume"="X2018_11") %>% mutate(year = 2018) %>% mutate(month = 11) 
X2018_12 <- sales %>% select(product, X2018_12) %>% rename("volume"="X2018_12") %>% mutate(year = 2018) %>% mutate(month = 12) 

#2019
X2019_1 <- sales %>% select(product, X2019_1) %>% rename("volume"="X2019_1") %>% mutate(year = 2019) %>% mutate(month = 1)
X2019_2 <- sales %>% select(product, X2019_2) %>% rename("volume"="X2019_2") %>% mutate(year = 2019) %>% mutate(month = 2)
X2019_3 <- sales %>% select(product, X2019_3) %>% rename("volume"="X2019_3") %>% mutate(year = 2019) %>% mutate(month = 3)
X2019_4 <- sales %>% select(product, X2019_4) %>% rename("volume"="X2019_4") %>% mutate(year = 2019) %>% mutate(month = 4)
X2019_5 <- sales %>% select(product, X2019_5) %>% rename("volume"="X2019_5") %>% mutate(year = 2019) %>% mutate(month = 5)
X2019_6 <- sales %>% select(product, X2019_6) %>% rename("volume"="X2019_6") %>% mutate(year = 2019) %>% mutate(month = 6)
X2019_7 <- sales %>% select(product, X2019_7) %>% rename("volume"="X2019_7") %>% mutate(year = 2019) %>% mutate(month = 7)
X2019_8 <- sales %>% select(product, X2019_8) %>% rename("volume"="X2019_8") %>% mutate(year = 2019) %>% mutate(month = 8)
X2019_9 <- sales %>% select(product, X2019_9) %>% rename("volume"="X2019_9") %>% mutate(year = 2019) %>% mutate(month = 9)
X2019_10 <- sales %>% select(product, X2019_10) %>% rename("volume"="X2019_10") %>% mutate(year = 2019) %>% mutate(month = 10)
X2019_11 <- sales %>% select(product, X2019_11) %>% rename("volume"="X2019_11") %>% mutate(year = 2019) %>% mutate(month = 11)
X2019_12 <- sales %>% select(product, X2019_12) %>% rename("volume"="X2019_12") %>% mutate(year = 2019) %>% mutate(month = 12)

#2020
X2020_1 <- sales %>% select(product, X2020_1) %>% rename("volume"="X2020_1") %>% mutate(year = 2020) %>% mutate(month = 1)
X2020_2 <- sales %>% select(product, X2020_2) %>% rename("volume"="X2020_2") %>% mutate(year = 2020) %>% mutate(month = 2)
X2020_3 <- sales %>% select(product, X2020_3) %>% rename("volume"="X2020_3") %>% mutate(year = 2020) %>% mutate(month = 3)
X2020_4 <- sales %>% select(product, X2020_4) %>% rename("volume"="X2020_4") %>% mutate(year = 2020) %>% mutate(month = 4)
X2020_5 <- sales %>% select(product, X2020_5) %>% rename("volume"="X2020_5") %>% mutate(year = 2020) %>% mutate(month = 5)
X2020_6 <- sales %>% select(product, X2020_6) %>% rename("volume"="X2020_6") %>% mutate(year = 2020) %>% mutate(month = 6)
X2020_7 <- sales %>% select(product, X2020_7) %>% rename("volume"="X2020_7") %>% mutate(year = 2020) %>% mutate(month = 7)
X2020_8 <- sales %>% select(product, X2020_8) %>% rename("volume"="X2020_8") %>% mutate(year = 2020) %>% mutate(month = 8)
X2020_9 <- sales %>% select(product, X2020_9) %>% rename("volume"="X2020_9") %>% mutate(year = 2020) %>% mutate(month = 9)
X2020_10 <- sales %>% select(product, X2020_10) %>% rename("volume"="X2020_10") %>% mutate(year = 2020) %>% mutate(month = 10)
X2020_11 <- sales %>% select(product, X2020_11) %>% rename("volume"="X2020_11") %>% mutate(year = 2020) %>% mutate(month = 11)
X2020_12 <- sales %>% select(product, X2020_12) %>% rename("volume"="X2020_12") %>% mutate(year = 2020) %>% mutate(month = 12)

#2021
X2021_1 <- sales %>% select(product, X2021_1) %>% rename("volume"="X2021_1") %>% mutate(year = 2021) %>% mutate(month = 1)
X2021_2 <- sales %>% select(product, X2021_2) %>% rename("volume"="X2021_2") %>% mutate(year = 2021) %>% mutate(month = 2)
X2021_3 <- sales %>% select(product, X2021_3) %>% rename("volume"="X2021_3") %>% mutate(year = 2021) %>% mutate(month = 3)
X2021_4 <- sales %>% select(product, X2021_4) %>% rename("volume"="X2021_4") %>% mutate(year = 2021) %>% mutate(month = 4)
X2021_5 <- sales %>% select(product, X2021_5) %>% rename("volume"="X2021_5") %>% mutate(year = 2021) %>% mutate(month = 5)
X2021_6 <- sales %>% select(product, X2021_6) %>% rename("volume"="X2021_6") %>% mutate(year = 2021) %>% mutate(month = 6)
X2021_7 <- sales %>% select(product, X2021_7) %>% rename("volume"="X2021_7") %>% mutate(year = 2021) %>% mutate(month = 7)
X2021_8 <- sales %>% select(product, X2021_8) %>% rename("volume"="X2021_8") %>% mutate(year = 2021) %>% mutate(month = 8)


X2017 <- rbind(X2017_1, X2017_2, X2017_3, X2017_4, X2017_5, X2017_6, X2017_7, X2017_8, X2017_9, X2017_10, X2017_11, X2017_12)
X2018 <- rbind(X2018_1, X2018_2, X2018_3, X2018_4, X2018_5, X2018_6, X2018_7, X2018_8, X2018_9, X2018_10, X2018_11, X2018_12)
X2019 <- rbind(X2019_1, X2019_2, X2019_3, X2019_4, X2019_5, X2019_6, X2019_7, X2019_8, X2019_9, X2019_10, X2019_11, X2019_12)
X2020 <- rbind(X2020_1, X2020_2, X2020_3, X2020_4, X2020_5, X2020_6, X2020_7, X2020_8, X2020_9, X2020_10, X2020_11, X2020_12)
X2021 <- rbind(X2021_1, X2021_2, X2021_3, X2021_4, X2021_5, X2021_6, X2021_7, X2021_8)

total <- rbind(X2017, X2018, X2019, X2020, X2021)
rm(sales)

# left join sales to product info -----------------------------------------
final <- left_join(product, total,  by = c('product'='product'))

final <- transform(final, sold = ifelse(volume==0, 0, 1)) %>% mutate(sales = price*volume)

dta <- as.data.frame(subset(final, select=c(manager, brand, manufact, type, product, price, year, month, volume, sales, sold, cat_small3, cat_small2, g, storage,
                                            recipe, mpro, mprocat, mprocat2, probeef, propork, prochicken, produck, proegg, prolamb, proleftmeat, procrus, profish, procephal, proshell, proleftsea,
                                            proexsea, proplant, pronone, carb, carbtype, spicy, sustain_pname, sustain_yn, physical, store, star, chain, michelin, loc, locdist, chef, tv, tvcount, storetype, storetype_new, specialty, maindish, onmenu, out, kurly_only)))

data <- dummy_cols(.data = dta, select_columns = c("storage"), remove_first_dummy = FALSE)
data <- dummy_cols(.data = data, select_columns = c("recipe"), remove_first_dummy = FALSE)
data <- dummy_cols(.data = data, select_columns = c("mprocat"), remove_first_dummy = FALSE)
data <- dummy_cols(.data = data, select_columns = c("mprocat2"), remove_first_dummy = FALSE)
data <- dummy_cols(.data = data, select_columns = c("carb"), remove_first_dummy = FALSE)
data <- dummy_cols(.data = data, select_columns = c("carbtype"), remove_first_dummy = FALSE)
data <- dummy_cols(.data = data, select_columns = c("sustain_pname"), remove_first_dummy = FALSE)
data <- dummy_cols(.data = data, select_columns = c("michelin"), remove_first_dummy = FALSE)
data <- dummy_cols(.data = data, select_columns = c("loc"), remove_first_dummy = FALSE)
data <- dummy_cols(.data = data, select_columns = c("storetype_new"), remove_first_dummy = FALSE)

data$y_m <- paste(data$year,"_", data$month, sep = "")

summary(data)

#count the number of stores
stores <- data %>% group_by(store) %>% summarise(n = n())

#total number of products and amount of sales
tot <- data %>% group_by(year, month) %>% summarise(totsales = sum(sales), totsku = sum(sold), totvolume = sum(volume), salessku = totsales/totsku, volumesku = totvolume/totsku)
write_xlsx(tot, path = "total_sum.xlsx")

loc_y <- data %>% filter(loc_seoul == "1") %>% group_by(year, month) %>% summarise(seoul_y_sku = sum(sold))
loc_n <- data %>% filter(loc_seoul == "0") %>% group_by(year, month) %>% summarise(seoul_n_sku = sum(sold))

chain_y <- data %>% filter(chain == "1") %>% group_by(year, month) %>% summarise(chain_y_sku = sum(sold))
chain_n <- data %>% filter(chain == "0") %>% group_by(year, month) %>% summarise(chain_n_sku = sum(sold))

service_q <- data %>% filter(storetype_new_Q == "1") %>% group_by(year, month) %>% summarise(service_q_sku = sum(sold))
service_c <- data %>% filter(storetype_new_C == "1") %>% group_by(year, month) %>% summarise(service_c_sku = sum(sold))
service_f <- data %>% filter(storetype_new_F == "1") %>% group_by(year, month) %>% summarise(service_f_sku = sum(sold))

specialty_y <- data %>% filter(specialty == "1") %>% group_by(year, month) %>% summarise(specialty_y_sku = sum(sold))
specialty_n <- data %>% filter(specialty == "0") %>% group_by(year, month) %>% summarise(specialty_n_sku = sum(sold))

chef_y <- data %>% filter(chef == "1") %>% group_by(year, month) %>% summarise(chef_y_sku = sum(sold))
chef_n <- data %>% filter(chef == "0") %>% group_by(year, month) %>% summarise(chef_n_sku = sum(sold))

onmenu_y <- data %>% filter(onmenu == "1") %>% group_by(year, month) %>% summarise(onmenu_y_sku = sum(sold))
onmenu_n <- data %>% filter(onmenu == "0") %>% group_by(year, month) %>% summarise(onmenu_n_sku = sum(sold))

sku <- data %>% group_by(year, month) %>% summarise(sku = sum(sold))

data1 <- left_join(data, loc_y,  by = c('year'='year', 'month'='month'))
data2 <- left_join(data1, loc_n,  by = c('year'='year', 'month'='month'))
data3 <- left_join(data2, chain_y,  by = c('year'='year', 'month'='month'))
data4 <- left_join(data3, chain_n,  by = c('year'='year', 'month'='month'))
data5 <- left_join(data4, service_q,  by = c('year'='year', 'month'='month'))
data6 <- left_join(data5, service_c,  by = c('year'='year', 'month'='month'))
data7 <- left_join(data6, service_f,  by = c('year'='year', 'month'='month'))
data8 <- left_join(data7, specialty_y,  by = c('year'='year', 'month'='month'))
data9 <- left_join(data8, specialty_n,  by = c('year'='year', 'month'='month'))
data10 <- left_join(data9, chef_y,  by = c('year'='year', 'month'='month'))
data11 <- left_join(data10, chef_n,  by = c('year'='year', 'month'='month'))
data12 <- left_join(data11, onmenu_y,  by = c('year'='year', 'month'='month'))
data13 <- left_join(data12, onmenu_n,  by = c('year'='year', 'month'='month'))
data14 <- left_join(data12, sku,  by = c('year'='year', 'month'='month'))

final <- transform(data14, volume = ifelse(volume==0, NA, volume))
final <- transform(final, sales = ifelse(volume==0, NA, sales))

write_xlsx(final, path = "dta763.xlsx")
