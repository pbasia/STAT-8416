function(input, output, session){
  
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
