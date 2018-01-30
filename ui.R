

shinyUI(dashboardPage(skin="green",
  dashboardHeader(title = "Valuation Metrics Portal"),
  
  ##Sidebar##
  sidebar <-  
    dashboardSidebar(
    sidebarMenu(id='sideBarMenu',
      menuItem("Background Information", tabName = "About", icon = icon("address-card")),
      menuItem("Exploratory Data Analysis", tabName = "EDA", icon = icon("dashboard")),
      menuItem("Market Comparison", tabName = "KPI", icon = icon("dashboard")),
      menuItem("Property Search", icon = icon("map-marker"), tabName = "Data",
               badgeLabel = "Data", badgeColor = "green")),
    
    conditionalPanel("input.sideBarMenu == 'EDA'",
                     selectizeInput('Metric',label = 'Metric',choice,selected = "Cap.Rate"),
                     selectizeInput('Market',label = 'Market',choices = unique(mpv[, Market]),selected = "NNJ/New York City"),
                     selectizeInput('PropertyType','Property Type', choices = c("Multifamily","Office","Industrial","Retail","Hotel","Self Storage","Senior Housing"),multiple = TRUE, selected = c("Office")),
                     selectizeInput('YearAppraised',label = 'Year',choices = unique(mpv[,EffectiveDate_Year],decreasing = TRUE), multiple = TRUE,selected = c("2014","2015","2016","2017"))
      ),
    
    conditionalPanel("input.sideBarMenu == 'KPI'",
                    selectizeInput('PropertyType1','Property Type', choices = c("Multifamily","Office","Industrial","Retail","Hotel","Self Storage","Senior Housing"), multiple = FALSE, selected = c("Office")),
                    selectizeInput('MarketOrientation1','Market Orientation',choices = unique(mpv[, MarketOrientation]),multiple = TRUE,selected = c("Suburban","Urban","CBD")),
                    selectizeInput('BuildingClass1','Building Class',choices = c("A","A-","B","C","Historic"), multiple = TRUE,selected =c("A","A-","B","C","Historic")),
                    selectizeInput('YearAppraised1',label = 'Year',choices = unique(mpv[,EffectiveDate_Year],decreasing = TRUE), multiple = TRUE,selected = c("2014","2015","2016","2017"))
      ),
    
    conditionalPanel("input.sideBarMenu == 'Data'",
                     selectizeInput('Market2','Market',choices = unique(mpv[, Market]),multiple = TRUE,selected = "NNJ/New York City"),
                     selectizeInput('PropertyType2','Property Type', choices = unique(mpv[, PropertyType]), multiple=TRUE,selected = c("Office")),
                     selectizeInput('MarketOrientation2','Market Orientation',choices = unique(mpv[, MarketOrientation]),multiple = TRUE,selected = c("Suburban","Urban","CBD")),
                     selectizeInput('BuildingClass2','Building Class',choices = c("A","A-","B","C","Historic"), multiple = TRUE,selected = c("A","A-","B")),
                     selectizeInput('YearAppraised2',label = 'Year',choices = unique(mpv[,EffectiveDate_Year],decreasing = TRUE), multiple = TRUE,selected = c("2014","2015","2016","2017"))
    
      )
    ),
###########################################################################################################
#############################################################Body##########################################
  body <- dashboardBody(
    tabItems(
      tabItem(
        tabName = "About",
                fluidRow(align = "left",box(width = 12,
                  h2(tags$b("Valuation Metrics Portal")),
                  tags$hr(),
                  h4('Welcome to Valuation Metrics Portal, a Commercial Real Estate (CRE) application built using R Shiny.
                      Access the portal to research summary statistics of
                      valuation parameters from income generating properties across an array of property types and markets in the U.S.')
                  )),
                fluidRow(
                 box(width=6,align = "left",
                  p(tags$p(tags$b('The Valuation Metrics Portal provides access to the following:'))),
                  p(tags$p(em(tags$b(' - Direct Capitalization Approach KPIs (Cap Rates, NOI/SF)')))),
                  p(tags$p(em(tags$b(' - Market parameters from the DCF Approach (Discount Rates, Reversion Rates)')))),
                  p(tags$p(em(tags$b(' - Market and Submarket Cap Rate Comparison Tool')))),
                  p(tags$p(em(tags$b(' - Exploration of Market Valuation Estimates'))))),
                  box(width=6,align = "left",
                      p(tags$p('Appraisers, Developers, Investors, and Brokers consitently require transparent, real market
                               insights, to support their decisions as they partake in the CRE marketplace.')),
                      p(tags$p('The data presented herein is a sample of value estimates provided by Integra Realty
                               Resources (IRR). IRR is the largest independent and private CRE valuation and consulting 
                               firm in the U.S. The firm has a network of 50+ offices across the U.S. and Caribbean.')))),
                fluidRow(
                  box(width=12,align = "left",
                      p(tags$p('Please find a Glossary of Key Metrics below:')),
                      p(tags$p(em(' - Direct Capitalization Approach: Valuation method in which a propertys stabilized net operating income (NOI) 
                                                                      is divided by the market capitalization rate.'))),
                      p(tags$p(em(' - Cap.Rate: The Cap Rate is the rate of return that an investment property will 
                                                generate based on its current market value, and is a quick way to compare different 
                                                 investment property options including apartments and office buildings.'))),
                      p(tags$p(em(' - NOI.PerSF: Average Net Operating Income ($) of income generating property divided by the Gross Leasable Area of the property.'))),
                      p(tags$p(em(' - Discounted Cash Flow: A discounted cash flow (DCF) is a valuation method used to estimate the attractiveness of an investment opportunity. 
                                                            DCF analysis uses future free cash flow projections and discounts them to arrive at a present value estimate, which is used to evaluate 
                                                            the potential for investment.'))),
                      p(tags$p(em(' - Discount.Rate:  The rate of return used in a discounted cash flow analysis to determine the present value of future cash flows. 
                                                      The rate reflects the cost of tying up capital and may also allow for the risk that the payment may not be received in full.'))),
                      p(tags$p(em(' - Reversion.Rate: The rate used to estimate the resale value of a property at the end of the holding period. The expected 
                                                      net operating income (NOI) per year is divided by the reversion cap rate (expressed as a percentage) to derive the reversion value.'))))),
                  fluidRow(box(width=6,
                               p(tags$p('Coming Soon:')),
                               p(tags$p(em(' - Point Estimates and Confidence Interval'))),
                               p(tags$p(em(' - Locational Adjustment Measures'))),
                               p(tags$p(em(' - CRE Market Cycle Factors')))),
                           ## insert image ##
                           img(src = "NYC.jpg",height = 140L))
                       ),
      tabItem(   
        tabName = "EDA",
          titlePanel(tags$b("Exploratory Data Anaysis")),
        
              fluidRow(plotOutput(outputId = "histogram"), width=12),
              fluidRow(
                  # Dynamic valueBox
                  valueBoxOutput("twentyfifth_subbox", width=3),
                  valueBoxOutput("median_subbox", width=3),
                  valueBoxOutput("avg_subbox", width=3),
                  valueBoxOutput("seventyfifth_subbox", width=3)),
               fluidRow(plotlyOutput(outputId = "metric"), width=12)
              ),
 
      tabItem(   
        tabName = "KPI",
          titlePanel(tags$b("Market Comparison")),
              fluidRow(align = "center",
                selectizeInput('market9',label = 'Market One',choices = unique(mpv[, Market]),multiple = FALSE,selected = "NNJ/New York City")),
              fluidRow(align = "center",
                selectizeInput('market10',label = 'Market Two',choices = unique(mpv[, Market]),multiple = FALSE,selected = "Chicago, IL")),       
              fluidRow(
                box(plotlyOutput(outputId = "comparison"),width=6),
                box(plotlyOutput(outputId = "comparison1"), width=6)),
              fluidRow(
                box(plotlyOutput(outputId = "boxplot"), width=6),
                box(plotlyOutput(outputId = "boxplot1"), width=6)),       
              br(),
              fluidRow(box(
                selectizeInput('repspace',label = 'Space Type',choices = unique(mpv[, RepresentativeSpaceType]),multiple = FALSE,selected = "Office")),             
                box(selectizeInput('leasetype',label = 'Lease Type',choices = unique(mpv[, LeaseType]),multiple = FALSE,selected = "Full Service"))),
              fluidRow(
                # Dynamic valueBox
                 infoBoxOutput("avgBox",width=6),
                 infoBoxOutput("avgBox1",width=6)),
               #  infoBoxOutput("avgBox")),
              fluidRow(
                box(plotlyOutput(outputId = "marketrent"),width=12),
                box(plotlyOutput(outputId = "marketrent1"), width=12))
            ),
      tabItem(
          tabName = "Data",h2(tags$b("Property Search")),
           leafletOutput("mymap"),
              br(),
            fluidRow(align = "right",downloadButton("MarketData", "Download")),
              br(),
            fluidRow(box(DT::dataTableOutput("table"), width = 12)
                 ))
      )
    )
))

