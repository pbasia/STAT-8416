## Introduction:

As Part of the final class project of STAT-8416, Intro to Data Science at the University of Nebraska - Omaha, I have explored the statistics and dynamics in home values, rentals, market list prices, sale prices, and forecasts in the USA using [Zillow Research Dataset](https://www.zillow.com/research/data/) and created a dashboard with all our analyses in one place. The Dashboard, ['Zillow Data Explorer'](https://basiapiyush.shinyapps.io/Group2_STAT8416/) shows all the changes in different house values over the last 5 years in interactive ways for Zillow data which helps users to understand and analyze the US house market. It answers all our research questions for this project.

## Research Questions:    
 1.  How do the home values currently are and how it changed in top-tier cities over a period?
 2.  How much increase or decrease do the home value and rent values faced over the last few years?
 3.  How do the rent values currently are and how its change in top-tier cities over a period?
 4.  Relationships and comparison between rentals vs home values.
 5.  Relationships and comparison between inventory(list) vs. sale price.
 6.  How does the future forecast look for house value?
 7.  Relationships and comparison between Forecasted home price vs home values and rental values.

## Description of The Dashboard:
I have used `R` programming language and `R Studio` platform to materialize this dashboard. The dashboard has been hosted at `Shinyapps` server for ease of public access. 
  
To access the dashboard click on visit the below link:  
  
Link: [https://basiapiyush.shinyapps.io/Group2\_STAT8416](https://basiapiyush.shinyapps.io/Group2_STAT8416)  
  
  or  
   
Scan this QR code with your smart phone: <img src="https://raw.githubusercontent.com/pbasia/STAT-8416/main/Images/QR_Code_Piyush.jpg" width="200" height="200" />
          
This Dashboard is comprised with four Sections: 'House Price Data', 'Zillow Rental Data', 'Sale Price vs List Price' and 'Zillow Forecast Data'. Each section explores and visualizes the related data.  
  
1.  **House Price Data:** A smoothed, seasonally adjusted measure of the typical home value and market changes across a given region and housing type. It reflects the typical value for homes in the 35th to 65th percentile range in zillow dataset. Below listed 3 maps helps to understand the data.
    1.  **USA Average Map:** This US map shows average median house price across the U.S.A states for different years from 2018 to 2022.
    2.  **Top Regions Map:** This US map shows the top n number of regions with highest median house prices in the U.S.A for different years. Users can select the number of regions and the year to see desired map.
    3.  **House Price Increase Map:** This US map shows the top n number of regions with highest house prices increases in the U.S.A for different years. Users can select the number of regions and the year to see desired map.
2.  **Zillow Rental Data:** A smoothed measure of the typical observed market rate rent across a given region. ZORI is a repeat-rent index that is weighted to the rental housing stock to ensure representativeness across the entire market, not just those homes currently listed for-rent. The index is dollar-denominated by computing the mean of listed rents that fall into the 40th to 60th percentile range for all homes and apartments in a given region. Below 3 maps are used to see the data and compare them:
    1.  **Highest Rent Price Regions:** This map shows us the top n regions where the house rent is the highest for a particular year.
    2.  **Highest Rent Increase:** This US maps deals with the highest increase in rent from previous year and shows the top n regions for a selected year.
    3.  **Lowest Rent Increase:** This US maps deals with the lowest increase in rent from previous year and shows the top n regions for a selected year. It also shows if there was a decrease in rent price for a region.
3.  **Sale Price vs List Price:** The median price at which homes across various geographies were sold and the median price at which homes across various geographies were listed were used to obtain the below two graph and plot.
    1.  **State Wise Graph:** This Line Chart shows how the list price and sale price of houses for a selected state changes over last 5 years. Also the interaction between these two prices can be visualized which helps us to understand the trend in price change.
    2.  **Year Wise Plot:** This Bar plot shows and compares list price and sale price of all the available states in the data for a given year from 2018 to 2022.
4.  **Zillow Forecast Data:** The Zillow Home Value Forecast (ZHVF) is the month-ahead, quarter-ahead and year-ahead forecast of the Zillow Home Values Index (ZHVI). ZHVF is created using the all homes, mid-tier cut of ZHVI and is available smoothed, seasonally adjusted. Below two maps and graphs help us to understand the data:
    1.  **Highest Forecast Price Regions:** This US map shows the top n regions with highest forecasted prices in the U.S.A. We have 3 forecast period '2022-04-30','2022-06-30' and '2023-03-31'. User can view the changes on selecting any of these periods.
    2.  **Forecast and Prices Plot:** This faceted line graph shows the region wise Forecast vs House Price vs Rent Price increase in the U.S.A and helps to compare different prices.
