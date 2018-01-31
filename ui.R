##Shiny UI Script##

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
                    selectizeInput('MarketOrientation1','Market Orientation',choices = unique(mpv[, MarketOrientation]),multiple = TRUE,selected = c("Suburban")),
                    selectizeInput('BuildingClass1','Building Class',choices = c("A","A-","B","C","Historic"), multiple = TRUE,selected =c("A","A-")),
                    selectizeInput('YearAppraised1',label = 'Year',choices = unique(mpv[,EffectiveDate_Year],decreasing = TRUE), multiple = TRUE,selected = c("2015","2016","2017"))
      ),
    
    conditionalPanel("input.sideBarMenu == 'Data'",
                     selectizeInput('Market2','Market',choices = unique(mpv[, Market]),multiple = TRUE,selected = "NNJ/New York City"),
                     selectizeInput('PropertyType2','Property Type', choices = unique(mpv[, PropertyType]), multiple=TRUE,selected = c("Office")),
                     selectizeInput('MarketOrientation2','Market Orientation',choices = unique(mpv[, MarketOrientation]),multiple = TRUE,selected = c("Suburban")),
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
                fluidRow(align = "center",box(width = 12,
                  h2(tags$b("Valuation Metrics Portal")),
                  tags$hr(),
                  h4('Welcome to Valuation Metrics Portal, a Commercial Real Estate (CRE) application built using R Shiny.'),
                  br(),
                  h4('The portal enables the exploration of valuation parameters from income 
                  generating properties in the U.S.'),
                  img(src = "NYC.jpg",height = 140L, width = 200),
                  img(src = "chicago.jpg",height = 140L, width = 200),
                  img(src = "sf.jpg",height = 140L, width = 200),
                  img(src = "la.jpg",height = 140L, width = 200)
                  )),
                fluidRow(
                  box(width=6,align = "center",
                      h4(tags$b('WHAT DOES THE PORTAL OFFER?')),
                      p(tags$li(em(tags$b('  Cap Rates, NOI/SF, Expense Growth Rates')))),
                      p(tags$li(em(tags$b('  Discount Rates, Reversion Rates, Market Rent Growth Rates')))),
                      p(tags$li(em(tags$b('  Cap Rate Comparison Between Markets')))),
                      p(tags$li(em(tags$b('  Market Rent Comparison Between Markets')))),
                      p(tags$li(em(tags$b('  Breakout of Cap Rates by Submarket')))),
                      p(tags$li(em(tags$b('  CRE Market Valuation Estimates')))),
                      p(tags$li(em(tags$b('  Explore and Download Comparables'))))),
                  box(width=6,align = "center",
                      h4(tags$b('WHY IS THIS IMPORTANT?')),
                      p(tags$p('Appraisers, Brokers, Developers, and Investors are routinely seeking transparent market
                                data to support their value conclusions or investment decisions. These individuals can use 
                                this portal to facilitate their research.')),
                      img(src = "financehead.jpg",height = 140L),
                      br()
                      )),
                      # p(tags$p('Coming Soon:')),
                      # p(tags$p(em(' - Locational Adjustment Measures (e.g. Income, Unemployment)'))),
                      # p(tags$p(em(' - CRE Market Cycle Factors (e.g. New Construction, Absorption)'))),
                      # p(tags$p(em(' - Point Estimation / Confidence Intervals of Valuation Parameters'))))),
                  
                fluidRow(align = "center",
                  h2(tags$b("Commercial Real Estate Glossary")),
                  tags$hr(),  
                  box(width=12,align = "left",
                      p(tags$u(tags$b(tags$p('Direct Capitalization Approach:'))),
                        'Valuation method in which a propertys stabilized Net Operating Income (NOI) is divided by the market capitalization rate.'),
                      p(tags$u(tags$b(tags$p('Capitalization (Cap) Rate:'))),
                                                    'The ratio of Net Operating Income (NOI) to property asset value. The rate of return that an investment property will generate based on its 
                                                     current market value.'),
                      p(tags$u(tags$b(tags$p('Discounted Cash Flow (DCF):'))),
                                                      'Valuation method used to estimate the attractiveness of an investment opportunity. 
                                                        DCF analysis uses future free cash flow projections and discounts them to arrive at a 
                                                        present value estimate, which is used to evaluate the potential for investment.'),
                      p(tags$u(tags$b(tags$p('Discount Rate:'))),
                                                      'The rate of return used in a discounted cash flow analysis to determine the present value of future cash flows. 
                                                       The rate reflects the cost of tying up capital and may also allow for the risk that the payment may not be received in full.'),
                      p(tags$u(tags$b(tags$p('Reversion Rate:'))),
                                                      'The rate used to estimate the resale value of a property at the end of the holding period. The expected 
                                                       net operating income (NOI) per year is divided by the reversion cap rate (expressed as a percentage) to derive the reversion value.'))),

                    fluidRow(box(width=12,align = "center",
                                 h4(tags$b('WHAT IS THE DATA SOURCE?')),
                                 p(tags$u(tags$p('Integra Realty Resources (IRR)'))),
                                 p(tags$p('   The largest independent and private CRE valuation and consulting firm in North America. The data is a sample of value 
                                              estimates provided by IRR.')),
                             img(src = "logo.png",height = 140L)))
        
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
                selectizeInput('market10',label = 'Market Two',choices = unique(mpv[, Market]),multiple = FALSE,selected = "San Francisco, CA")),       
              fluidRow(
                box(plotlyOutput(outputId = "comparison"),width=6),
                box(plotlyOutput(outputId = "comparison1"), width=6)),
              fluidRow(
                box(plotlyOutput(outputId = "boxplot"), width=6),
                box(plotlyOutput(outputId = "boxplot1"), width=6)),       
              br(),
              fluidRow(box(
                selectizeInput('repspace',label = 'Space Type',choices = unique(mpv[, RepresentativeSpaceType]),multiple = FALSE,selected = "Office")),             
                box(selectizeInput('leasetype',label = 'Lease Type',choices = unique(mpv[, LeaseType]),multiple = FALSE,selected = "Modified Gross"))),
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

