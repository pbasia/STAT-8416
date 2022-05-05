#install.packages("maptools")

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



ui <-shinyUI(fluidPage(theme = "bootstrap.css",
                       
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

server <- function(input, output, session){
 
  output$graph<- renderPlotly({
    
    StateSale <- subset(saless, saless$StateName %in% c(input$state11))
    a<-(StateSale%>% summarise_if(is.numeric, mean))
    a['StateName'] <- StateSale$StateName[1]
    Saledf <- melt(a,id.vars = 52)
    Saledf$variable <- substr(Saledf$variable,2,11) #Removed 'X'
    Saledf$date<-str_replace_all(Saledf$variable, "[^[:alnum:]]", "-")
    Saledf$date<-ymd(Saledf$date)
    
    StateList <- subset(listp,listp$StateName %in% c(input$state11))
    a1<-(StateList%>% summarise_if(is.numeric, mean))
    a1['StateName'] <- StateList$StateName[1]
    Listdf <- melt(a1,id.vars = 52)
    Listdf$variable <- substr(Listdf$variable,2,11) #Removed 'X'
    Listdf$date<-str_replace_all(Listdf$variable, "[^[:alnum:]]", "-")
    Listdf$date<-ymd(Listdf$date)
    
    combineddata <-full_join(Saledf, Listdf, by="date")
    
    plottitle <- paste("Selected State Name: ", input$state11)
    t2 <- list(
      family = "Courier New",
      size = 20,
      color = "black")
    
    fig <- plot_ly() %>% add_trace(x = ~combineddata$date, y = ~combineddata$value.x,name="Sale Price", mode = "lines+markers",line=list(color="blue"),marker=list(color="blue"), type = "scatter") %>% 
      add_trace(x = combineddata$date, y = ~combineddata$value.y, mode = "lines+markers",line=list(color="red"),marker=list(color="red"),name="List Price", type = "scatter") %>%
      layout(title = list(text=plottitle,font = t2),xaxis = list(title = "Year"),yaxis = list(title = "House Price"))
    
    fig
    
    
    
  })
  
  output$graph1<- renderPlotly({
    
    year<-input$year11
    abc<-(which(grepl(year, colnames(sales), fixed = TRUE)))
    
    
    sales_year <- subset(sales,select=c(5,abc[1]:abc[length(abc)]))
    sales_year$AvgSalePrice <- rowMeans(sales_year[ ,-c(1)])
    sales_year$state <- sales_year$StateName
    n<-as.integer(length(colnames(sales_year)))
    AvgSales_year <- subset(sales_year,select=c(n,(n-1)))
    AvgSales_yr<- as.data.frame(AvgSales_year %>%
                                  group_by(state) %>%
                                  summarise(across(AvgSalePrice,mean)))
    
    AvgSales_yr$AvgSalePrice <- round(AvgSales_yr$AvgSalePrice,0)
    
    
    abc1<-which(grepl(year, colnames(listp), fixed = TRUE))
    
    List_year <- subset(listp,select=c(1,abc1[1]:abc1[length(abc1)]))
    List_year$AvgListPrice <- rowMeans(List_year[ , -c(1)])
    List_year$state <- List_year$StateName
    m<-as.integer(length(colnames(List_year)))
    AvgList_year <- subset(List_year,select=c(m,(m-1)))
    AvgList_yr<- as.data.frame(AvgList_year %>%
                                 group_by(state) %>%
                                 summarise(across(AvgListPrice,mean)))
    
    AvgList_yr$AvgListPrice <- round(AvgList_yr$AvgListPrice,0)
    
    mergeddata <- merge(AvgSales_yr,AvgList_yr,by="state")
    mDat <- melt(mergeddata)
    
    plot7_title<- paste("Visualization of Average List and Sale Price Across States for the year:",year)
    
    pg1<-ggplot(mDat,aes(x = reorder(state, -value),y=value,fill=variable,text=paste(" State: ",reorder(state, -value),"\n","House Price Type: ",variable,"\n","Value: ",value)))+
      geom_bar(stat="identity",position="dodge")+
      theme(axis.text.x=element_text(angle=90, hjust=1))+
      ggtitle(plot7_title) +
      theme(plot.title = element_text(hjust = 0.5,size = 15))+
      xlab("State Name") + ylab("House Value")+labs(fill="House Price Type")
    
    ggplotly(pg1,tooltip="text")
    
  })
  
  output$Map1<- renderPlot({
    
    condo_dat_fltr = condo_dat1 %>% filter(Year == input$year1)
    condo_dat_fltr = condo_dat_fltr %>% filter(StateName != 'HI' & StateName != 'AK')
    condo_dat_merged= merge(condo_dat_fltr,cities_dat, by.x = 'RegionName', by.y = 'City',sort=FALSE)
    condo_dat_merged = condo_dat_merged  %>% drop_na()
    condo_dat_srtd = condo_dat_merged %>% arrange(desc(value))
    
    condo_dat_srtd$state <- condo_dat_srtd$StateName
    statedata<- as.data.frame(condo_dat_srtd %>% group_by(state) %>% summarise(HouseValues=mean(value)))
    
    plottitle_Map1 <- paste("Average House Prices for year: " ,input$year1, " in the U.S.A.")
    
    p1<-plot_usmap(data = statedata, values = "HouseValues",labels = TRUE, label_color = "white",exclude = c("AK", "HI"))+
      scale_fill_continuous(low = "blue", high = "red", name = "Average House Values", label = scales::comma)+
      labs(title = plottitle_Map1) +
      theme(legend.position = "right", plot.title = element_text(size = 16, face = "bold"),
            legend.title=element_text(size=10),
            legend.text=element_text(size=9))
    
    p1
    
    
  })
  
  output$Map2<- renderPlot({
    
    condo_dat_fltr = condo_dat1 %>% filter(Year == input$year2)
    
    condo_dat_fltr = condo_dat_fltr %>% filter(StateName != 'HI' & StateName != 'AK')
    condo_dat_merged= merge(condo_dat_fltr,cities_dat, by.x = 'RegionName', by.y = 'City',sort=FALSE)
    condo_dat_merged = condo_dat_merged  %>% drop_na()
    condo_dat_srtd = condo_dat_merged %>% arrange(desc(value))
    
    condo_dat_srtd_50 <- condo_dat_srtd %>% top_n(input$obs2, value)
    
    cities_t <- usmap_transform(condo_dat_srtd_50,input_names = c("Longitude", "Latitude"),output_names = c("x", "y"))
    
    
    condo_dat_srtd$state <- condo_dat_srtd$StateName
    statedata<- as.data.frame(condo_dat_srtd %>% group_by(state) %>% summarise(HouseValues=mean(value)))
    
    plottitle_Map2 <- paste("Top ", input$obs2, " Regions in " ,input$year2, " with Highest Median House Prices in the U.S.A ")
    
    p2<-plot_usmap(fill = "yellow", alpha = 0.25,exclude = c("AK", "HI")) +
      geom_point(data = cities_t,
                 aes(x = x, y = y, size = value),
                 alpha = 0.5) +
      scale_size_continuous(label = scales::comma) +
      labs(title = plottitle_Map2,
           size = "Median House Value") +
      theme(legend.position = "right", plot.title = element_text(size = 16, face = "bold"),
            legend.title=element_text(size=10), 
            legend.text=element_text(size=9))+
      ggrepel::geom_label_repel(data = cities_t,
                                aes(x = x, y = y, label = RegionName),
                                size = 3, alpha = 0.8,
                                label.r = unit(0.5, "lines"), label.size = 0.5,
                                segment.color = "red", segment.size = 1,
                                seed = 1002)
    
    p2
    
    
  })
  
  output$Map0<- renderPlot({
    
    metro_condo_dat_srtd <- metro_condo_dat_srtd %>%                                      # Top N highest values by group
      arrange(desc(price_increase_yearly)) %>% 
      group_by(Year) %>%
      slice(1:input$obs0)
    metro_condo_dat_srtd = metro_condo_dat_srtd %>%
      mutate(text = str_c(str_to_title(RegionName), "\n",  "Increase:", price_increase_yearly,"% ,Rank:", Rank))
    metro_cities <- usmap_transform(metro_condo_dat_srtd, input_names = c("Longitude",
                                                                          "Latitude"), output_names = c("x", "y"))
    metro_cities = metro_cities %>% filter(Year==input$year0)
    metro_cities_2021 = metro_cities %>% filter(Year == input$year0) 
    
    plttitle0<-paste("Top ",input$obs0," Regions in ",input$year0," with Highest Price Increase in the U.S.A")
    
    p0 <- plot_usmap(fill = "darksalmon", alpha = 0.25, exclude = c("AK", "HI"))  +
      geom_point(data = metro_cities_2021, aes(x = x, y = y, size = price_increase_yearly), alpha = 0.5) + 
      scale_size_continuous(label = scales::comma) +
      labs(title = plttitle0,
           size = "House Value Price Increase Percent") + 
      theme(legend.position = "right",plot.title = element_text(size = 16, face = "bold"), 
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 9)) +
      ggrepel::geom_label_repel(data = metro_cities_2021,
                                aes(x = x, y = y, label = text), size = 3, alpha = 0.8,
                                label.r = unit(0.5, "lines"), label.size = 0.5, segment.color = "red",
                                segment.size = 1, seed = 1002)
    p0
    
    
  })
  
  
  output$Map3<- renderPlot({
    
    metro_rental_dat_srtd1 = metro_rental_dat_merged123 %>% group_by(Year) %>%  arrange(desc(rental_value)) %>% slice(1:input$obs3) %>% filter(Year == input$year3)
    metro_rental_dat_srtd1
    
    metro_rental_dat_srtd1 = metro_rental_dat_srtd1 %>%
      mutate(text = str_c(str_to_title(RegionName), "\n",  "Rent Value:", rental_value))
    
    metro_cities11 <- usmap_transform(metro_rental_dat_srtd1, input_names = c("Longitude",
                                                                              "Latitude"), output_names = c("x", "y"))
    metro_cities11
    
    plt3_title <- paste("Top ", input$obs3,"Regions in ", input$year3,"with Highest Rental Prices in the U.S.A")
    
    p22 <- plot_usmap(fill = "LightSeaGreen", alpha = 0.25, exclude = c("AK", "HI"))  +
      geom_point(data = metro_cities11, aes(x = x, y = y, size = rental_value), alpha = 0.5) + 
      scale_size_continuous(label = scales::comma) +
      labs(title = plt3_title,
           size = "House Rent") + 
      theme(legend.position = "right",plot.title = element_text(size = 16, face = "bold"), 
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 9)) +
      ggrepel::geom_label_repel(data = metro_cities11,
                                aes(x = x, y = y, label = text), size = 3, alpha = 0.8,
                                label.r = unit(0.5, "lines"), label.size = 0.5, segment.color = "red",
                                segment.size = 1, seed = 1002)
    p22
    
    
  })
  
  output$Map4<- renderPlot({
    
    metro_rental_dat_srtd123 <- metro_rental_dat_srtd123 %>%                                      
      arrange(desc(price_increase_yearly)) %>% 
      group_by(Year) %>%
      slice(1:input$obs4)
    
    metro_rental_dat_srtd123 = metro_rental_dat_srtd123 %>%
      mutate(text = str_c(str_to_title(RegionName), "\n",  "Increase:", price_increase_yearly,"% Rank:", Rank))
    
    
    plt_title4 <- paste("Top ", input$obs4,"Regions in ", input$year4,"with Highest Rental Price Increase in the U.S.A")
    
    metro_rental_dat_srtd_year123<-metro_rental_dat_srtd123 %>% filter(Year==input$year4)
    
    ggplot(metro_rental_dat_srtd_year123, aes(x=Longitude, y=Latitude, fill=Rank)) +
      borders("state",colour="grey75")+
      scale_fill_continuous(low="red",high="green")+
      geom_label_repel(aes(label = text), size = 5)+
      coord_quickmap() +
      labs(title=plt_title4,fill="Rank")+xlab("")+ylab("")+
      theme(plot.title=element_text(hjust=0.5,size = 25),plot.title.position = "plot")
    
    
  })
  
  output$Map5<- renderPlot({
    
    metro_rental_loss_srtd123 <- metro_rental_loss_srtd123 %>%
      arrange(Rank) %>% 
      group_by(Year) %>%
      slice(1:input$obs5)
    
    
    metro_rental_loss_srtd123 = metro_rental_loss_srtd123 %>%
      mutate(text = str_c(str_to_title(RegionName), "\n",  ifelse(price_increase_yearly <= 0 ,"Decrease:","Increase:"), abs(price_increase_yearly),"%"))
    metro_cities <- usmap_transform(metro_rental_loss_srtd123, input_names = c("Longitude",
                                                                               "Latitude"), output_names = c("x", "y"))
    
    metro_rental_loss_srtd_year123<-metro_rental_loss_srtd123 %>% filter(Year==input$year5)
    
    
    plt_title5 <- paste("Top ", input$obs5,"Regions in ", input$year5,"with Lowest Rental Price Increase in the U.S.A")
    
    
    
    ggplot(metro_rental_loss_srtd_year123, aes(x=Longitude, y=Latitude, fill=price_increase_yearly)) +
      borders("state",colour="grey75")+
      scale_fill_continuous(low="red",high="green")+
      geom_label_repel(aes(label = text), size = 5)+
      coord_quickmap() +
      labs(title=plt_title5,fill="Price Increase Percent")+xlab("")+ylab("")+
      theme(plot.title=element_text(hjust=0.5,size = 25),plot.title.position = "plot")
    
    
  })
  
  
  output$Map6<- renderPlot({
    
    metro_forecast_top_srtd <- metro_forecast_top_srtd %>%                                      # Top N highest values by group
      arrange(desc(forecast_pct)) %>% 
      group_by(Year) %>%
      slice(1:input$obs6)
    
    metro_forecast_top_srtd = metro_forecast_top_srtd %>% filter(Year == as.Date(input$year6)) %>% 
      mutate(text = str_c(str_to_title(RegionName),". Rank:", Rank, "\n",  "Forecasted Increase:", forecast_pct,"%,"))
    
    metro_cities <- usmap_transform(metro_forecast_top_srtd, input_names = c("Longitude",
                                                                             "Latitude"), output_names = c("x", "y"))
    p6_title<-paste("Top ",input$obs6," Regions with Highest Forecasted Values for date: ",input$year6)
    
    p6 <- plot_usmap(fill = "seagreen1", alpha = 0.25, exclude = c("AK", "HI"))  +
      geom_point(data = metro_cities, aes(x = x, y = y, size = forecast_pct), alpha = 0.5) + 
      scale_size_continuous(label = scales::comma) +
      labs(title = p6_title,
           size = "Price Increase Percent") + 
      theme(legend.position = "right",plot.title = element_text(size = 16, face = "bold"), 
            legend.title = element_text(size = 10),
            legend.text = element_text(size = 9)) +
      ggrepel::geom_label_repel(data = metro_cities,
                                aes(x = x, y = y, label = text), size = 3, alpha = 0.8,
                                label.r = unit(0.5, "lines"), label.size = 0.5, segment.color = "red",
                                segment.size = 1, seed = 1002)
    p6
    
    
  })
  
  
  
  output$plot7<- renderPlot({
    
    cols <- c("Forecast Percent Increase" = "red", "HouseValue Percent Increase" = "blue", "Rental Pct Increase" = "orange")
    plot7_title<-paste("House Value ,Rental Value and Forecast Percent increase across the state",input$state7,".")
    
    metro_house_forecast_dat %>% filter(grepl(input$state7, StateName)) %>%
      ggplot( aes(x=Year, y=Increase_Pct, group=Category,color=Category)) +
      geom_line(lwd=1) +
      scale_colour_manual(values = cols) +
      ggtitle(plot7_title) +
      theme(plot.title = element_text(hjust = 0.5,size = 25)) +
      ylab("Percent")  +
      facet_wrap(~RegionName)
    
  })
  
  
}

shinyApp(ui, server)
