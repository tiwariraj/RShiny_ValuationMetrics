###NYCDSA Bootcamp 12, R Shiny Project###
###Raj Tiwari###

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
                  PropertyType %in% c("Industrial","Retail","Office","Hotel","Self Storage","Senior Housing","Multifamily"))

attach(new.mpv)


choice = c("Value.PerSFGLA","Value.PerUnit","NOI.PerSF","NOI.PerUnit"
           ,"Cap.Rate","ExpenseRatio","MarketRentGrowth","Direct.Cap.Vacancy","DCF.DiscountRate",
           "DCF.ReversionRate","DCF.ImpliedGoingInCapRate")

