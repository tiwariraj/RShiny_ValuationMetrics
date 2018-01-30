###Server.R code###

function(input, output, session)  {
  

#############################################################################################################
###########################################Reactive Function - Exploratory Data Analysis#####################

  ValuationRateSummary <- reactive({
    CRS <- 
      new.mpv %>% 
      filter(EffectiveDate_Year == input$YearAppraised & Market == input$Market & 
               PropertyType == input$PropertyType)
    
    return(CRS)
  })
  
  
#############################################################################################################
##########################################################################################################
###############################################Render Statistic Summary - Histogram############################
  output$histogram <- renderPlot({
    hist.table = ValuationRateSummary()
    x = hist.table[,input$Metric]

    hist(x, col = 'aquamarine4', border = 'grey', xlab = input$Metric, ylab = "Frequency"
         ,main = paste("Distribution of",input$Metric),breaks=5) 

  })
  
###########################################################################################################
###########################################################################################################

  output$metric <- renderPlotly({
  
    hist.table = ValuationRateSummary() %>% filter(BuildingClass != "")
    
      p <- ggplot(data = hist.table) +
        geom_jitter(mapping = aes(x = Cap.Rate, y = BuildingClass, color = MarketOrientation)) + 
        labs(title = "Cap Rate (%) Distribution by Building Class and Market Orientation") + theme(legend.title = element_blank())
         
        #Alternative Chart#
        #ggplot(data = hist.table) +
        #geom_density(aes(x = Cap.Rate, color = BuildingClass), kernel = "gaussian", adjust = 4)
        #ggplot(data=hist.table, aes(x=BuildingClass, y=Cap.Rate)) + 
         #geom_boxplot(alpha = .6,fill="cadetblue4") + geom_jitter(alpha=.4,color = "tomato") +
         #facet_grid(MarketOrientation~.) + labs(x = "",title = "Cap Rate (%) by Building Class and Market Orientation")
      p
      
  })
###########################################################################################################
###########################################################################################################
### EDA KPI's
  output$twentyfifth_subbox <- renderValueBox({
    infotable = ValuationRateSummary()
    x = infotable[,input$Metric]
    twentyfifth_Metric = round(quantile(x,.25,na.rm =TRUE),2)
    valueBox(twentyfifth_Metric, paste("25th %tile -", input$Metric), icon = icon("list"), color = "purple")
  })
  
  output$seventyfifth_subbox <- renderValueBox({
    infotable = ValuationRateSummary()
    x = infotable[,input$Metric]
    seventyfifth_Metric = round(quantile(x,.75,na.rm =TRUE),2)
    valueBox(seventyfifth_Metric, paste("75th %tile -", input$Metric), icon = icon("list"), color = "aqua")
  })
  
  output$median_subbox <- renderValueBox({
    infotable = ValuationRateSummary()
    x = infotable[,input$Metric]
    Median_Metric = round(median(x,na.rm =TRUE),2)
    valueBox(Median_Metric, paste("Median -", input$Metric), icon = icon("list"), color = "blue")
  })
  
  output$avg_subbox <- renderValueBox({
    infotable = ValuationRateSummary()
    x = infotable[,input$Metric]
    Avg_Metric = round(mean(x,na.rm =TRUE),2)
    valueBox(Avg_Metric, paste("Average -", input$Metric), icon = icon("list"), color = "black") 
    })
  

#############################################################################################################
#############################################Market Comparison Tab###########################################
#############################################################################################################
#############################################Market Comparison - Reactive Function###########################
  CapRateSummary1 <- reactive({
    CRS <- 
      new.mpv %>% 
      filter(PropertyType == input$PropertyType1 & EffectiveDate_Year == input$YearAppraised1
             & BuildingClass == input$BuildingClass1 & MarketOrientation == input$MarketOrientation1 
             & Market == input$market10) %>% 
      group_by(Submarket) %>%
      summarise(Count = n(),
                Avg_CapRate = mean(Cap.Rate,na.rm =TRUE),
                Median_CapRate = median(Cap.Rate,na.rm =TRUE),
                Min_CapRate = min(Cap.Rate,na.rm =TRUE),
                Max_CapRate = max(Cap.Rate,na.rm =TRUE))
    
    
    return(CRS)
})
###########################################################################################################
##############################################Market Comparison - Render Plotly############################
  output$comparison1 <- renderPlotly({
    y <- list(title = "")
    
    pchart <-  plot_ly(data=CapRateSummary1())%>%
      add_trace(x=CapRateSummary1()[[1]], y = ~Median_CapRate,
                type = 'bar', line = list(color = 'rgb(200, 50, 50)')) %>% 
      layout(title = "<b>Median Cap Rates (%)</b>",yaxis = y)
    
    pchart
  })
###########################################################################################################  
###########################################################################################################
  
 boxplotSummary1 <- reactive({
    CRS <- 
      new.mpv %>%
      filter(PropertyType == input$PropertyType1 & EffectiveDate_Year == input$YearAppraised1
             & BuildingClass == input$BuildingClass1 & MarketOrientation == input$MarketOrientation1 
             & Market == input$market10)
    
    return(CRS)
  })
#############################################################################################################
###############################################Render Cap Rate Submarket Summary############################

  output$boxplot1 <- renderPlotly({
    boxplot.table = boxplotSummary1()
    x = boxplot.table %>% select(Submarket,Cap.Rate)

    p = ggplot(x, aes(x = Submarket,y = Cap.Rate)) + geom_boxplot() + coord_flip()
    p
  })
###########################################################################################################  
###########################################################################################################
  CapRateSummary <- reactive({
    CRS <- 
      new.mpv %>% 
      filter(PropertyType == input$PropertyType1 & EffectiveDate_Year == input$YearAppraised1
              & BuildingClass == input$BuildingClass1 & MarketOrientation == input$MarketOrientation1 
              & Market == input$market9) %>% 
      group_by(Submarket) %>%
      summarise(Count = n(),
                Avg_CapRate = mean(Cap.Rate,na.rm =TRUE),
                Median_CapRate = median(Cap.Rate,na.rm =TRUE),
                Min_CapRate = min(Cap.Rate,na.rm =TRUE),
                Max_CapRate = max(Cap.Rate,na.rm =TRUE))
    
    
    return(CRS)
  })
#############################################################################################################
###############################################Render Cap Rate Submarket Summary############################
  output$comparison <- renderPlotly({
    y <- list(title = "")
    
    pchart <-  plot_ly(data=CapRateSummary())%>%
      add_trace(x=CapRateSummary()[[1]], y = ~Median_CapRate,
                type = 'bar', line = list(color = 'rgb(200, 50, 50)')) %>% #mode = 'lines', , width = 1
      layout(title = "<b>Median Cap Rates (%)</b>",yaxis = y) 
    
    pchart
  })

###########################################################################################################  
################################# Reactive Function - Market Comparison #########################################################
  boxplotSummary <- reactive({
    CRS <- 
     new.mpv %>% 
      filter(PropertyType == input$PropertyType1 & EffectiveDate_Year == input$YearAppraised1
             & BuildingClass == input$BuildingClass1 & MarketOrientation == input$MarketOrientation1 
             & Market == input$market9)
    return(CRS)
  })

#############################################################################################################
###############################################Render Submarket Cap Rate Comparison############################
  output$boxplot <- renderPlotly({
      boxplot.table = boxplotSummary()
      x = boxplot.table %>% select(Submarket,Cap.Rate)
      #bins <- seq(min(hist.table$CapitalizationRatePercentage), max(hist.table$CapitalizationRatePercentage), length.out = input$bins + 1)
      #ggplot(hist.table,aes(x = CapitalizationRatePercentage))+
      p = ggplot(x, aes(x = Submarket,y = Cap.Rate)) + geom_boxplot() + coord_flip()
        #theme(axis.text.x = element_text(angle = 45, hjust = 1))
      p
  })
###########################################################################################################  
################################# Reactive Function - Market Comparison #########################################################
  marketratesummary <- reactive({
    CRS <- 
      new.mpv %>% 
      filter(Market == input$market9 & RepresentativeSpaceType == input$repspace & LeaseType == input$leasetype)
    return(CRS)
  })
  
###############################################Render Submarket Cap Rate Comparison############################
  output$marketrent <- renderPlotly({
     scatter.table = marketratesummary()

    p <- ggplot(data = scatter.table) +
      geom_jitter(mapping = aes(x = AverageMarketRent, y= SpaceTypeSF, color = BuildingClass)) + 
      labs(title = paste("Market Rent ($/SF) by Space Type for",input$market9)) + theme(legend.title = element_blank())
    p
  })
  ###########################################################################################################  
  ################################# Reactive Function - Market Comparison #########################################################
  marketratesummary1 <- reactive({
    CRS <- 
      new.mpv %>% 
      filter(Market == input$market10 & RepresentativeSpaceType == input$repspace & LeaseType == input$leasetype)
    return(CRS)
  })
  
  ###############################################Render Submarket Cap Rate Comparison############################
  output$marketrent1 <- renderPlotly({
    scatter.table = marketratesummary1()

    p <- ggplot(data = scatter.table) +
      geom_jitter(mapping = aes(x = AverageMarketRent, y= SpaceTypeSF, color = BuildingClass)) + 
      labs(title = paste("Market Rent ($/SF) by Space Type for",input$market10)) + theme(legend.title = element_blank())
    p
  })
  ###########################################################################################################  
  ###########################################################################################################  
  # show statistics using infoBox
  output$avgBox <- renderInfoBox({
    infotable = marketratesummary() %>% 
      filter(!is.na(AverageMarketRent)) %>% 
      summarise(mean(AverageMarketRent))
    
      infoBox(paste("Avg. Market Rent ($/SF):",input$market9),value = round(infotable,2),icon = icon("calculator"), fill = TRUE)

  })
  
  # show statistics using infoBox
  output$avgBox1 <- renderInfoBox({
    infotable = marketratesummary1() %>% 
      filter(!is.na(AverageMarketRent)) %>% 
      summarise(mean(AverageMarketRent))
    
      infoBox(paste("Avg. Market Rent ($/SF):",input$market10),value = round(infotable,2),icon = icon("calculator"), fill = TRUE)
  })


#################################Cap Rate Summary##########################################################
  datasetInput <- reactive({
    CRS <- 
      new.mpv %>% 
      filter(PropertyType == input$PropertyType2 & EffectiveDate_Year == input$YearAppraised2
             & BuildingClass == input$BuildingClass2 & MarketOrientation == input$MarketOrientation2 
             & Market == input$Market2)
    
      #Following code slows the map functionality down#
      # select(PropertyName,EffectiveDate = EffectiveDate2,PropertyType,SubType,SpecificUse,State,
      #        CityMunicipality,Market,Submarket,MarketOrientation,GrossBuildingAreaSF,
      #        BuildingClass,InterestAppraised,Cap.Rate,NOI.PerSF,Value.PerSFGLA)
    return(CRS)
  })  
  ######################################################################################################
  #######################################################################################################
  # show data using DataTable
  output$table <- DT::renderDataTable({
    x = datasetInput() %>% select(PropertyName,PropertyType,SubType,SpecificUse,State,CityMunicipality,
              Market,Submarket,MarketOrientation,GrossBuildingAreaSF,
              BuildingClass,InterestAppraised,Cap.Rate,NOI.PerSF,Value.PerSFGLA)
    DT::datatable(x, options = list(pageLength = 15,background="skyblue", fontWeight='bold'))
  })
    
   output$MarketData <- downloadHandler(

        filename = function() {
        paste("Your", ".csv", sep = "")
      },
        content = function(file) {
        write.csv(x, file, row.names = FALSE)
      }
     )

  output$mymap <- renderLeaflet({
    df = datasetInput()
    leaflet(df) %>%
      addProviderTiles("Esri.WorldStreetMap") %>%
      addMarkers(~RooftopLongitude, ~RooftopLatitude,label = df$PropertyName,
                   popup=paste("Cap Rate (%):", df$Cap.Rate,"<br>"
                               ,"GLA (SF):", df$GrossBuildingAreaSF,"<br>"
                               ,"Value ($/SF):", df$Value.PerSFGLA,"<br>"
                               ))

  })
}



