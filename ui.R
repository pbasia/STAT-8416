install.packages("maptools")

library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(plotly)
library(dplyr)
library(reshape2)
library(stringr)
library(lubridate)
library(ggplot2)
library(maps)
library(tidyr)
library(ggmap)
library(ggthemes)
library(tidyverse)
library(rvest)
library(ggrepel)
library(rgdal)
library(usmap)
library(rsconnect)
library(data.table)
library(dbplyr)
library(XML)
library(leaflet)
library(webshot)
library(htmlwidgets)
library(maptools)
library(mapview)
library(viridis)
options(ggrepel.max.overlaps = Inf)

#Sale Price Vs List Price

SaleDat<- read.csv("https://raw.githubusercontent.com/pbasia/STAT-8416/main/Metro_median_sale_price_uc_sfrcondo_sm_sa_month.csv", header = TRUE,na.string = "NA")
sales = SaleDat[-1,]
salessub <- subset(sales,select=c(1,2,3,5,75:125))
saless <- na.omit(salessub)
saless <- subset(saless,select= -c(1,2,3))
listDat<- read.csv("https://raw.githubusercontent.com/pbasia/STAT-8416/main/Metro_mlp_uc_sfrcondo_sm_month.csv", header = TRUE,na.string = "NA")
listp <- listDat[-1,]
listp <- na.omit(listp)
listp <- subset(listp,select= -c(1,2,3,4))


# House Price Maps

zhvi_scondo1="https://raw.githubusercontent.com/pbasia/STAT-8416/main/Metro_zhvi_uc_sfrcondo_tier1.csv"
condo_dat1=read.table(zhvi_scondo1,header=TRUE,sep = ",", dec =".")

condo_dat1=subset(condo_dat1, select = c("RegionID", "SizeRank","RegionName","RegionType","StateName","X01.31.2022","X12.31.2021","X12.31.2020","X12.31.2019","X12.31.2018"))

cities_file="https://raw.githubusercontent.com/pbasia/STAT-8416/main/City_Latitude_Longitude.csv"
cities_dat = read.table(cities_file,header=TRUE,sep=",",dec=".",fileEncoding="UTF-8-BOM")

condo_dat1 = condo_dat1 %>% rename("2022"=X01.31.2022,"2021"=X12.31.2021,"2020"=X12.31.2020,"2019"=X12.31.2019,"2018"=X12.31.2018) 
condo_dat1 = condo_dat1 %>% gather(Year,value, "2022":"2018")
condo_dat1$Year=as.integer(condo_dat1$Year)

#Rental Data
zhvi_rental="https://raw.githubusercontent.com/pbasia/STAT-8416/main/Metro_Rentals.csv"
metro_rental_dat123=read.table(zhvi_rental,header=TRUE,sep = ",", dec =".")

colnames(metro_rental_dat123)=gsub("[~X]","",colnames(metro_rental_dat123))
colnames(metro_rental_dat123)=gsub("[~X]","",colnames(metro_rental_dat123))
colnames(metro_rental_dat123) = gsub("\\.","",colnames(metro_rental_dat123))
metro_rental_dat123 = metro_rental_dat123 %>% select(c("RegionID", "SizeRank","RegionName") | ends_with("12") | c("202203") )
metro_rental_dat123 = metro_rental_dat123 %>% rename('2014'= '201412','2015'= '201512','2016'= '201612','2017'= '201712','2018'= '201812','2019'= '201912','2020'= '202012','2021'= '202112','2022'= '202203')
metro_rental_dat123$year_2018 = metro_rental_dat123$"2018"
metro_rental_dat123 = metro_rental_dat123 %>% gather(Year,rental_value, "2014":"2022")

metro_rental_dat123$StateName = gsub(".*,", "", metro_rental_dat123$RegionName)
metro_rental_dat123$cityname = gsub("^(.*?),.*", "\\1", metro_rental_dat123$RegionName)
metro_rental_dat123 = metro_rental_dat123 %>% filter(StateName != 'HI' & StateName != 'AK' & RegionName != "United States" )
metro_rental_dat123$year_2018 = as.integer(metro_rental_dat123$year_2018)
metro_rental_dat123$rental_value = as.integer(metro_rental_dat123$rental_value)
metro_rental_dat123 = metro_rental_dat123 %>% group_by(RegionID) %>%
  mutate(prev_rental_value = dplyr::lag(rental_value,n=1,default=1,order_by = Year))
metro_rental_dat123$increase_pct_2018 = ifelse(metro_rental_dat123$Year <= 2018, 0, round(100*((metro_rental_dat123$rental_value - metro_rental_dat123$year_2018)/ metro_rental_dat123 $year_2018 )))
metro_rental_dat123$price_increase_yearly = round(100 *
                                                    (( metro_rental_dat123$rental_value - metro_rental_dat123$prev_rental_value) /metro_rental_dat123$prev_rental_value))
metro_rental_dat_fltr123 = metro_rental_dat123 %>% filter(!is.na(rental_value) & Year >= 2018 & Year <= 2022)
cities_file123="https://raw.githubusercontent.com/pbasia/STAT-8416/main/City_Latitude_Longitude.csv"
cities_dat123 = read.table(cities_file123,header=TRUE,sep=",",dec=".",fileEncoding="UTF-8-BOM")
metro_rental_dat_merged123= merge(metro_rental_dat_fltr123,cities_dat123, by.x = 'RegionName', by.y = 'City',sort=FALSE)
metro_rental_dat_merged123 = metro_rental_dat_merged123  %>% drop_na()


metro_rental_dat_srtd123 = metro_rental_dat_merged123 %>% arrange(desc(price_increase_yearly))
metro_rental_dat_srtd123$value = scale(as.integer(metro_rental_dat_srtd123$price_increase_yearly))
metro_rental_loss_srtd123 = metro_rental_dat_srtd123 %>% group_by(Year) %>% mutate(Rank = rank(price_increase_yearly))
metro_rental_dat_srtd123 = metro_rental_dat_srtd123 %>% group_by(Year) %>% mutate(Rank = order(price_increase_yearly,decreasing=TRUE),Rank_1=order(increase_pct_2018,decreasing=TRUE))
metro_rental_dat_srtd123$cityname = gsub("^(.*?),.*", "\\1", metro_rental_dat_srtd123$RegionName)

#

#---------

zhvi_scondo="https://raw.githubusercontent.com/pbasia/STAT-8416/main/Metro_zhvi_uc_sfrcondo_tier.csv"
metro_condo_dat=read.table(zhvi_scondo,header=TRUE,sep = ",", dec =".")
colnames(metro_condo_dat)=gsub("[~X]","",colnames(metro_condo_dat))
metro_condo_dat = metro_condo_dat %>% select(c("RegionID", "SizeRank","RegionName","RegionType","StateName") | ends_with("12.31") | c("2022.03.31") )
colnames(metro_condo_dat) = gsub(".12.31|.03.31$","",colnames(metro_condo_dat))
metro_condo_dat$year_2018 = metro_condo_dat$"2018"
metro_condo_dat = metro_condo_dat %>% gather(Year,housevalue, "2022":"2000")
metro_condo_dat = metro_condo_dat %>% filter(StateName != 'HI' & StateName != 'AK' & RegionType == 'Msa')
metro_condo_dat$year_2018 = as.integer(metro_condo_dat$year_2018)
metro_condo_dat$housevalue = as.integer(metro_condo_dat$housevalue)
metro_condo_dat = metro_condo_dat %>% group_by(RegionID) %>%
  mutate(prev_housevalue = dplyr::lag(housevalue,n=1,default=1,order_by = Year))
metro_condo_dat$increase_pct_2018 = ifelse(metro_condo_dat$Year <= 2018, 0, round(100*((metro_condo_dat$housevalue - metro_condo_dat$year_2018)/ metro_condo_dat $year_2018 )))
metro_condo_dat$price_increase_yearly = round(100 * 
                                                (( metro_condo_dat$housevalue - metro_condo_dat$prev_housevalue) /metro_condo_dat$prev_housevalue))

metro_condo_dat_fltr = metro_condo_dat %>% filter(!is.na(housevalue) & Year > 2018 & Year <= 2022)
cities_file="https://raw.githubusercontent.com/pbasia/STAT-8416/main/City_Latitude_Longitude.csv"
cities_dat = read.table(cities_file,header=TRUE,sep=",",dec=".",fileEncoding="UTF-8-BOM")

metro_condo_dat_merged= merge(metro_condo_dat_fltr,cities_dat, by.x = 'RegionName', by.y = 'City',sort=FALSE)
metro_condo_dat_merged = metro_condo_dat_merged  %>% drop_na()

metro_condo_dat_srtd = metro_condo_dat_merged %>% arrange(desc(price_increase_yearly))

metro_condo_dat_srtd$value = scale(as.integer(metro_condo_dat_srtd$price_increase_yearly))
metro_condo_loss_srtd = metro_condo_dat_srtd %>% group_by(Year) %>% mutate(Rank = rank(price_increase_yearly))
metro_condo_dat_srtd = metro_condo_dat_srtd %>% group_by(Year) %>% mutate(Rank = order(price_increase_yearly,decreasing=TRUE),Rank_1=order(increase_pct_2018,decreasing=TRUE))
metro_condo_dat_srtd$cityname = gsub("^(.*?),.*", "\\1", metro_condo_dat_srtd$RegionName)

metro_scondo_dat =read.table(zhvi_scondo,header=TRUE,sep = ",", dec =".")
colnames(metro_scondo_dat)=gsub("[~X]","",colnames(metro_scondo_dat))
colnames(metro_scondo_dat) = gsub("\\.","",colnames(metro_scondo_dat))
metro_scondo_dat$year_2018 = metro_scondo_dat$"20181231"
metro_scondo_dat = metro_scondo_dat %>% gather(Year,housevalue, "20000131":"20220331")
metro_scondo_dat = metro_scondo_dat %>% filter(StateName != 'HI' & StateName != 'AK' & RegionType == 'Msa' )
metro_scondo_dat$Year = as.Date(metro_scondo_dat$Year,"%Y%m%d")
metro_scondo_dat = metro_scondo_dat %>% filter(Year >= as.Date("2018-12-31"))
metro_scondo_dat$year_2018 = as.integer(metro_scondo_dat$year_2018)
metro_scondo_dat$housevalue = as.integer(metro_scondo_dat$housevalue)
metro_scondo_dat = metro_scondo_dat %>% group_by(RegionID) %>%
  mutate(prev_housevalue = dplyr::lag(housevalue,n=1,default=1,order_by = Year))
metro_scondo_dat$increase_pct_2018 = ifelse(metro_scondo_dat$Year <= 2018, 0, round(100*((metro_scondo_dat$housevalue - metro_scondo_dat$year_2018)/ metro_scondo_dat $year_2018 )))

metro_scondo_dat$price_increase_monthly  = 100 * 
  (( metro_scondo_dat$housevalue - metro_scondo_dat$prev_housevalue) /metro_scondo_dat$prev_housevalue)

metro_scondo_dat = metro_scondo_dat %>%  filter(!is.na(housevalue) & !is.na(year_2018) )
metro_Scondo_dat = metro_scondo_dat %>% filter(Year != as.Date("2018-12-31"))

#


metro_rental_dat=read.table(zhvi_rental,header=TRUE,sep = ",", dec =".")
colnames(metro_rental_dat)=gsub("[~X]","",colnames(metro_rental_dat))
colnames(metro_rental_dat)=gsub("[~X]","",colnames(metro_rental_dat))
colnames(metro_rental_dat) = gsub("\\.","",colnames(metro_rental_dat))
metro_rental_dat = metro_rental_dat %>% select(c("RegionID", "SizeRank","RegionName") | ends_with("12") | c("202203") )
metro_rental_dat$year_2018 = metro_rental_dat$"201812"
metro_rental_dat = metro_rental_dat %>% gather(Year,rental_value, "201412":"202203")
metro_rental_dat$StateName = gsub(".*,", "", metro_rental_dat$RegionName) 
metro_rental_dat$cityname = gsub("^(.*?),.*", "\\1", metro_rental_dat$RegionName)
metro_rental_dat = metro_rental_dat %>% filter(StateName != 'HI' & StateName != 'AK' & RegionName != "United States" )
metro_rental_dat$Year = as.Date(paste(metro_rental_dat$Year, "01", sep=""), "%Y%m%d")
metro_rental_dat = metro_rental_dat %>% filter(Year >= as.Date("2018-12-01"))
metro_rental_dat$year_2018 = as.integer(metro_rental_dat$year_2018)
metro_rental_dat$rental_value = as.integer(metro_rental_dat$rental_value)
metro_rental_dat = metro_rental_dat %>% group_by(RegionID) %>%
  mutate(prev_rental_value = dplyr::lag(rental_value,n=1,default=1,order_by = Year))
metro_rental_dat$increase_pct_2018 = ifelse(metro_rental_dat$Year <= 2018, 0, round(100*((metro_rental_dat$rental_value - metro_rental_dat$year_2018)/ metro_rental_dat $year_2018 )))

metro_rental_dat$price_increase_yearly = round(100 * 
                                                 (( metro_rental_dat$rental_value - metro_rental_dat$prev_rental_value) /metro_rental_dat$prev_rental_value))


metro_rental_dat_fltr = metro_rental_dat %>% filter(!is.na(rental_value) & Year > as.Date("2018-12-01","%Y-%m-%d") & Year <= as.Date("2022-03-01","%Y-%m-%d"))
cities_file="https://raw.githubusercontent.com/pbasia/STAT-8416/main/City_Latitude_Longitude.csv"
cities_dat = read.table(cities_file,header=TRUE,sep=",",dec=".",fileEncoding="UTF-8-BOM")

metro_rental_dat_merged= merge(metro_rental_dat_fltr,cities_dat, by.x = 'RegionName', by.y = 'City',sort=FALSE)
metro_rental_dat_merged = metro_rental_dat_merged  %>% drop_na()

metro_rental_dat_srtd = metro_rental_dat_merged %>% arrange(desc(price_increase_yearly))
metro_rental_dat_srtd$value = scale(as.integer(metro_rental_dat_srtd$price_increase_yearly))
metro_rental_loss_srtd = metro_rental_dat_srtd %>% group_by(Year) %>% mutate(Rank = rank(price_increase_yearly))
metro_rental_dat_srtd = metro_rental_dat_srtd %>% group_by(Year) %>% mutate(Rank = order(price_increase_yearly,decreasing=TRUE),Rank_1=order(increase_pct_2018,decreasing=TRUE))
metro_rental_dat_srtd$cityname = gsub("^(.*?),.*", "\\1", metro_rental_dat_srtd$RegionName)

metro_rental_top_srtd = metro_rental_dat_merged %>% arrange(desc(rental_value))
metro_rental_top_srtd$value = scale(as.integer(metro_rental_top_srtd$rental_value))
metro_rental_top_srtd = metro_rental_top_srtd %>% group_by(Year) %>% mutate(Rank = order(rental_value,decreasing=TRUE),Rank_1=order(rental_value,decreasing=TRUE))
metro_rental_top_srtd$cityname = gsub("^(.*?),.*", "\\1", metro_rental_top_srtd$RegionName)

zhvi_rental="https://raw.githubusercontent.com/pbasia/STAT-8416/main/Metro_Rentals.csv"
metro_rental_dat=read.table(zhvi_rental,header=TRUE,sep = ",", dec =".")
colnames(metro_rental_dat)=gsub("[~X]","",colnames(metro_rental_dat))
colnames(metro_rental_dat)=gsub("[~X]","",colnames(metro_rental_dat))
colnames(metro_rental_dat) = gsub("\\.","",colnames(metro_rental_dat))
#metro_rental_dat = metro_rental_dat %>% select(c("RegionID", "SizeRank","RegionName") )
#metro_rental_dat$year_2018 = metro_rental_dat$"201812"
metro_rental_dat$StateName = gsub(".*,", "", metro_rental_dat$RegionName) 
metro_rental_dat$cityname = gsub("^(.*?),.*", "\\1", metro_rental_dat$RegionName)
metro_rental_dat$year_2018 = metro_rental_dat$"201812"
metro_rental_dat = metro_rental_dat %>% gather(Year,rental_value, "201401":"202203")
metro_rental_dat$StateName = gsub(".*,", "", metro_rental_dat$RegionName) 
metro_rental_dat$cityname = gsub("^(.*?),.*", "\\1", metro_rental_dat$RegionName)
metro_rental_dat = metro_rental_dat %>% filter(StateName != 'HI' & StateName != 'AK' & RegionName != "United States" )
metro_rental_dat$Year = as.Date(paste(metro_rental_dat$Year, "01", sep=""), "%Y%m%d")
metro_rental_dat = metro_rental_dat %>% filter(Year >= as.Date("2018-12-01"))
metro_rental_dat$year_2018 = as.integer(metro_rental_dat$year_2018)
metro_rental_dat$rental_value = as.integer(metro_rental_dat$rental_value)
metro_rental_dat = metro_rental_dat %>% group_by(RegionID) %>%
  mutate(prev_rental_value = dplyr::lag(rental_value,n=1,default=1,order_by = Year))
metro_rental_dat$increase_pct_2018 = ifelse(metro_rental_dat$Year <= 2018, 0, round(100*((metro_rental_dat$rental_value - metro_rental_dat$year_2018)/ metro_rental_dat $year_2018 )))

metro_rental_dat$price_increase_yearly = round(100 * 
                                                 (( metro_rental_dat$rental_value - metro_rental_dat$prev_rental_value) /metro_rental_dat$prev_rental_value))

metro_rental_dat = metro_rental_dat %>%  filter(!is.na(rental_value) & !is.na(year_2018) )
metro_rental_dat = metro_rental_dat %>% filter(Year != as.Date("2018-12-01"))

metro_rental_dat$Year = as.Date(metro_rental_dat$Year,origin = '1964-10-22')

metro_scondo_dat$Year = floor_date(as.Date(metro_scondo_dat$Year, origin = '1964-10-22'),unit="month")

metro_rental_house_cmbnd = metro_rental_dat %>% inner_join(metro_scondo_dat,by=c("Year","RegionID"))

metro_rental_house_cmbnd =  metro_rental_house_cmbnd %>% gather(category,value, increase_pct_2018.x,increase_pct_2018.y)
metro_rental_house_cmbnd$category = ifelse(metro_rental_house_cmbnd$category == "increase_pct_2018.x","Rental Pct Increase","House Value Pct Increase")  
metro_rental_bkp = metro_rental_house_cmbnd

zhvi_scondo="https://raw.githubusercontent.com/pbasia/STAT-8416/main/Metro_zhvi_uc_sfrcondo_tier.csv"
metro_scondo_dat =read.table(zhvi_scondo,header=TRUE,sep = ",", dec =".")
colnames(metro_scondo_dat)=gsub("[~X]","",colnames(metro_scondo_dat))
colnames(metro_scondo_dat) = gsub("\\.","",colnames(metro_scondo_dat))
metro_scondo_dat$year_2018 = metro_scondo_dat$"20181231"
metro_scondo_dat = metro_scondo_dat %>% gather(Year,housevalue, "20000131":"20220331")
metro_scondo_dat = metro_scondo_dat %>% filter(StateName != 'HI' & StateName != 'AK' & RegionType == 'Msa' )
metro_scondo_dat$Year = as.Date(metro_scondo_dat$Year,"%Y%m%d")
metro_scondo_dat = metro_scondo_dat %>% filter(Year >= as.Date("2018-12-31"))
metro_scondo_dat$year_2018 = as.integer(metro_scondo_dat$year_2018)
metro_scondo_dat$housevalue = as.integer(metro_scondo_dat$housevalue)
metro_scondo_dat = metro_scondo_dat %>% group_by(RegionID) %>%
  mutate(prev_housevalue = dplyr::lag(housevalue,n=1,default=1,order_by = Year))
metro_scondo_dat$increase_pct_2018 = ifelse(metro_scondo_dat$Year <= 2018, 0, round(100*((metro_scondo_dat$housevalue - metro_scondo_dat$year_2018)/ metro_scondo_dat $year_2018 )))

metro_scondo_dat$price_increase_monthly  = 100 * 
  (( metro_scondo_dat$housevalue - metro_scondo_dat$prev_housevalue) /metro_scondo_dat$prev_housevalue)

metro_scondo_dat = metro_scondo_dat %>%  filter(!is.na(housevalue) & !is.na(year_2018) )
metro_scondo_dat = metro_scondo_dat %>% filter(Year != as.Date("2018-12-31"))

zhvi_forecast="https://raw.githubusercontent.com/pbasia/STAT-8416/main/Metro_Forecasts.csv"
metro_forecast_dat=read.table(zhvi_forecast,header=TRUE,sep = ",", dec =".")
colnames(metro_forecast_dat)=gsub("[~X]","",colnames(metro_forecast_dat))
colnames(metro_forecast_dat) = gsub("\\.","",colnames(metro_forecast_dat))
metro_forecast_dat = metro_forecast_dat %>% gather(Year,forecast_pct, "20220430":"20230331")
metro_forecast_dat$StateName = gsub(".*,", "", metro_forecast_dat$RegionName) 
metro_forecast_dat$cityname = gsub("^(.*?),.*", "\\1", metro_forecast_dat$RegionName)
metro_forecast_dat$StateName = trimws(metro_forecast_dat$StateName, which = c("both"))
metro_forecast_dat = metro_forecast_dat %>% filter(StateName != 'HI' & StateName != 'AK' & RegionName != "United States" )
metro_forecast_dat$Year = as.Date(metro_forecast_dat$Year, "%Y%m%d")
metro_forecast_dat$cityname = gsub("^(.*?),.*", "\\1", metro_forecast_dat$RegionName)
cities_file="https://raw.githubusercontent.com/pbasia/STAT-8416/main/City_Latitude_Longitude.csv"
cities_dat = read.table(cities_file,header=TRUE,sep=",",dec=".",fileEncoding="UTF-8-BOM")

metro_forecast_dat_merged= merge(metro_forecast_dat,cities_dat, by.x = 'RegionName', by.y = 'City',sort=FALSE)
metro_forecast_dat_merged = metro_forecast_dat_merged  %>% drop_na()
metro_forecast_top_srtd = metro_forecast_dat_merged %>% arrange(desc(forecast_pct))
metro_forecast_top_srtd$value = scale(as.integer(metro_forecast_top_srtd$forecast_pct))
metro_forecast_top_srtd = metro_forecast_top_srtd %>% group_by(Year) %>% mutate(Rank = order(forecast_pct,decreasing=TRUE),Rank_1=order(forecast_pct,decreasing=TRUE))
metro_forecast_top_srtd$cityname = gsub("^(.*?),.*", "\\1", metro_forecast_top_srtd$RegionName)

metro_forecast_cmbnd = metro_forecast_dat_merged %>% inner_join(metro_scondo_dat,by=c("RegionID")) %>% filter(
  BaseDate <= Year.y)
metro_forecast_cmbnd$forecast_pct = metro_forecast_cmbnd$increase_pct_2018 + metro_forecast_cmbnd$forecast_pct
metro_forecast_cmbnd = metro_forecast_cmbnd %>% select(c("RegionName.x","RegionID","StateName.x","Year.x","forecast_pct")) 

metro_forecast_cmbnd$Category = "Forecast Percent Increase"
colnames(metro_forecast_cmbnd) = c("RegionName","RegionID","StateName","Year","Increase_Pct","Category")

metro_scondo_dat = metro_scondo_dat %>% filter(Year != as.Date("2018-12-31")) %>% select(c("RegionName","RegionID","StateName","Year","increase_pct_2018"))

metro_scondo_dat$category = "HouseValue Percent Increase"
colnames(metro_scondo_dat) = c("RegionName","RegionID","StateName","Year","Increase_Pct","Category")

nrow(metro_rental_bkp)
metro_rental_house_sub  = metro_rental_bkp %>% filter(Year != as.Date("2018-12-31")) %>% select(c("RegionName.x","RegionID","StateName.x","Year","value","category"))

metro_rental_house_sub = metro_rental_house_sub %>% filter(grepl('Rental', category))
colnames(metro_rental_house_sub) = c("RegionName","RegionID","StateName","Year","Increase_Pct","Category")


metro_house_forecast_dat = union(metro_scondo_dat,metro_forecast_cmbnd)
metro_house_forecast_dat =  union(metro_house_forecast_dat,metro_rental_house_sub)

#-----



shinyUI(fluidPage(theme = "bootstrap.css",
                       
                       titlePanel("Analysis of Home Prices in the USA using Zillow Datasets - Group 2, STAT-8416"),
                       
                       dashboardPage(
                         skin = "red",
                         dashboardHeader(title = p("Zillow Data Explorer")),
                         
                         dashboardSidebar(
                           sidebarMenu(
                             menuItem("Zillow Project", tabName = "home",
                                      icon = icon("home")),
                             
                             menuItem("House Price Data", tabName = "HPMaps", 
                                      icon = icon("th"),startExpanded = TRUE,
                                      
                                      
                                      menuSubItem('State Average Map',
                                                  tabName = 'HPMap1',
                                                  icon = icon('map')),
                                      menuSubItem('Top Regions Map',
                                                  tabName = 'HPMap2',
                                                  icon = icon('map')),
                                      menuSubItem('House Price Increase Map',
                                                  tabName = 'HPMap0',
                                                  icon = icon('map'))
                             ),
                             
                             menuItem("Zillow Rental Data", tabName = "rent1",
                                      icon = icon("th"),
                                      menuSubItem('Highest Rent Price Regions',
                                                  tabName = 'rent',
                                                  icon = icon('map')),
                                      menuSubItem('Highest Rent Increase',
                                                  tabName = 'topmap',
                                                  icon = icon('map')),
                                      menuSubItem('Lowest Rent Increase',
                                                  tabName = 'lastmap',
                                                  icon = icon('map'))),
                             
                             menuItem("Sale Price Vs List Price", tabName = "control", 
                                      icon = icon("th"),
                                      menuSubItem("State Wise Graph", tabName = "SWG", 
                                                  icon = icon("chart-line")),
                                      
                                      menuSubItem("Year Wise Plot", tabName = "SLPy", 
                                                  icon = icon("chart-bar"))
                             ),
                             
                             menuItem("Zillow ForeCast Data", tabName = "fore1",
                                      icon = icon("th"),
                                      menuSubItem('Highest Forecast Price Regions',
                                                  tabName = 'fore',
                                                  icon = icon('map')),
                                      menuSubItem('Forecast and Price Plot',
                                                  tabName = 'reg_fore_map',
                                                  icon = icon('chart-line'))
                             )
                             
                           )
                         ),
                         
                         
                         dashboardBody(
                           
                           
                           tabItems(
                             
                             tabItem(tabName = "home",
                                     h2("1. Introduction"),
                                     p("A home is often the largest and most expensive purchase a person makes in their lifetime. Ensuring homeowners have a trusted way to monitor this asset is incredibly important. As the most-visited real estate website in the United States, Zillow and its affiliates offer customers an on-demand experience for selling, buying, renting and financing with transparency and nearly seamless end-to-end service. The Zestimate was created to give consumers as much information as possible about homes and the housing market, marking the first time consumers had access to this type of home value information at no cost. The great housing boom in the U.S. continues unabated after eight years of strong house price growth. It has been buoyed by continued low interest rates and by the government's massive stimulus packages to cushion the impact of the pandemic. A limited supply of properties in the market has added to upward house price pressure. In this research project, We explored the relationships between inventory and sale price, rentals and home values. We added data on the maps that we have created to show these relationship comparison over time. We worked to see the trends in the data for forecasting prices of house values and to fit regression model to correctly forecast house values in US for future years"),
                                     h2("2 About the Data"),
                                     p("The data used in this analysis is sourced from the Zillow website, more specifically, Zillow Home Value Index (ZHVI); A smoothed, seasonally adjusted measure of the typical home value and market changes across a given region and housing type in USA. The data currently available spans from 2015 to 2022, so we would be able to follow how prices have increased or decreased over time and make some assumptions as to why this is guided."),
                                     p("The data used for this project can be found under the website:"),
                                     a("https://www.zillow.com/research/data/" ,href="https://www.zillow.com/research/data/"),
                                     h3("About the Dashboard"),
                                     p("Add Each Tab details here"),
                                     a("Sale Price Vs List Price",href="#shiny-tab-SWG", "data-toggle" = "tab"),
                                     br(),
                                     a("State Average Map",href="#shiny-tab-HPMap1", "data-toggle" = "tab"),
                                     br(),
                                     a("Top Regions Map",href="#shiny-tab-HPMap2", "data-toggle" = "tab")
                                     
                             ),
                             
                             
                             tabItem(tabName = "SWG",
                                     h2("State Wise Zillow House Price and List Price Comparison"),
                                     fluidRow(
                                       column(width = 4,
                                              selectInput("state11", "Select State Nam", choices=unique(saless$StateName),selected= 'NE')
                                       ),
                                       column( width=12,
                                               plotlyOutput("graph")
                                       )
                                     )
                             ),
                             tabItem(tabName = "SLPy",
                                     h2("Year Wise House Price and List Price Comparison"),
                                     fluidRow(
                                       column(width = 4,
                                              selectInput("year11", "Select Year", choices=c('2018','2019','2020','2021','2022'),selected= '2022')
                                       ),
                                       column( width=12,
                                               plotlyOutput("graph1")
                                       )
                                     )
                             ),
                             
                             #Second Sub tab 1Tab Item
                             tabItem(tabName = "HPMap1",
                                     h2("Average Median House Price Across the U.S.A States Year Wise "),
                                     fluidRow(
                                       column(5,
                                              wellPanel(
                                                selectInput("year1", "Select Year", choices=unique(condo_dat1$Year),selected= '2022')
                                              )
                                       ),
                                       
                                       
                                       column(12,
                                              plotOutput("Map1",height = 600)
                                       )
                                       
                                     )
                                     
                                     
                             ),
                             
                             tabItem(tabName = "HPMap2",
                                     h2("Top Regions with Highest Median House Prices in the U.S.A"),
                                     fluidRow(
                                       column(8,
                                              wellPanel(
                                                selectInput("year2", "Select Year", choices=unique(condo_dat1$Year),selected= '2022'),
                                                sliderInput("obs2","Select Number of Regions:",min = 1,max = 100,value = 50)
                                              )
                                       ),
                                       column(12,
                                              plotOutput("Map2",height = 600)
                                       )
                                     )
                             ),
                             
                             tabItem(tabName = "HPMap0",
                                     h2("Top Regions with Highest House Prices Increase in the U.S.A"),
                                     fluidRow(
                                       column(8,
                                              wellPanel(
                                                selectInput("year0", "Select Year", choices=unique(condo_dat1$Year),selected= '2022'),
                                                sliderInput("obs0","Select Number of Regions:",min = 1,max = 100,value = 50)
                                              )
                                       ),
                                       column(12,
                                              plotOutput("Map0",height = 600)
                                       )
                                     )
                             ),
                             
                             
                             tabItem(tabName = "rent",
                                     h2("Top Regions with Highest Rent Prices in the U.S.A"),
                                     fluidRow(
                                       column(8,
                                              wellPanel(
                                                selectInput("year3", "Select Year", choices=sort(unique(metro_rental_dat_srtd123$Year)),selected= '2021'),
                                                sliderInput("obs3","Select Number of Regions:",min = 1,max = 100,value = 50)
                                              )
                                       ),
                                       column(12,
                                              plotOutput("Map3",height = 600)
                                       )
                                     )
                             ),
                             
                             tabItem(tabName = "topmap",
                                     h2("Top Regions with Highest Rent Price Increase in the U.S.A"),
                                     fluidRow(
                                       column(8,
                                              wellPanel(
                                                selectInput("year4", "Select Year", choices=sort(unique(metro_rental_dat_srtd123$Year)),selected= '2021'),
                                                sliderInput("obs4","Select Number of Regions:",min = 1,max = 100,value = 15)
                                              )
                                       ),
                                       column(12,
                                              plotOutput("Map4",height = 600)
                                       )
                                     )
                             ),
                             
                             tabItem(tabName = "lastmap",
                                     h2("Top Regions with Lowest Rent Price Increase in the U.S.A"),
                                     fluidRow(
                                       column(8,
                                              wellPanel(
                                                selectInput("year5", "Select Year", choices=sort(unique(metro_rental_dat_srtd123$Year)),selected= '2021'),
                                                sliderInput("obs5","Select Number of Regions:",min = 1,max = 100,value = 15)
                                              )
                                       ),
                                       column(12,
                                              plotOutput("Map5",height = 600)
                                       )
                                     )
                             ),
                             
                             
                             tabItem(tabName = "fore",
                                     h2("Top Regions with Highest Forecasted Prices in the U.S.A"),
                                     fluidRow(
                                       column(8,
                                              wellPanel(
                                                selectInput("year6", "Select Forecast Day", choices=unique(metro_forecast_top_srtd$Year),selected= '2023-03-31'),
                                                sliderInput("obs6","Select Number of Regions:",min = 1,max = 100,value = 20)
                                              )
                                       ),
                                       column(12,
                                              plotOutput("Map6",height = 600)
                                       )
                                     )
                             ),
                             
                             tabItem(tabName = "reg_fore_map",
                                     h2("Region Wise Forecast vs House Price vs Rent Price Increase in the U.S.A"),
                                     fluidRow(
                                       column(8,
                                              wellPanel(
                                                selectInput("state7", "Select State", choices=sort(unique(metro_house_forecast_dat$StateName)),selected= 'NE')
                                              )
                                       ),
                                       column(12,
                                              plotOutput("plot7",height = 600)
                                       )
                                     )
                             )
                           )
                           
                         )
                       )
)
)