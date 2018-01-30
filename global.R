library(shiny)
library(data.table)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(ggplot2)
library(shinythemes)
library(tidyr)
library(DT)
library(googleVis)
library(maps)
library(shinydashboard)
library(plotly)


mpv = fread("mpv.csv", stringsAsFactors = F)
new.mpv <- mpv %>% filter(.,
                  PropertyType %in% c("Industrial","Retail","Office","Hotel","Self Storage","Senior Housing","Multifamily") 
                  #Eliminates Land & Other Specialty Property Types
                  #BuildingClass != "" & #Only records with a valid Building Class
                  #InterestAppraised == "Fee Simple"
                  #Market != "" #Eliminates records not tagged to a market
                          
)
attach(new.mpv)


choice = c("Value.PerSFGLA","Value.PerUnit","NOI.PerSF","NOI.PerUnit"
           ,"Cap.Rate","ExpenseRatio","MarketRentGrowth","Direct.Cap.Vacancy","DCF.DiscountRate",
           "DCF.ReversionRate","DCF.ImpliedGoingInCapRate")

