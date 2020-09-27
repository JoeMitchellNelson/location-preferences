#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(shinyBS)
library(googlesheets4)
library(tippy)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Where does Kati want to live?"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(3,
               sliderInput(inputId="climate",
                           min=0,
                           max=100,
                           value=50,
                           step = 1,
                           round=F,
                           ticks=F,
                           label="Climate security"),
               
               sliderInput(inputId="school",
                           min=0,
                           max=100,
                           value=50,
                           step = 1,
                           ticks=F,
                           label="High school graduation rate"),
               sliderInput(inputId="summer",
                           min=0,
                           max=100,
                           value=50,
                           step = 1,
                           ticks=F,
                           label="Mild summers"),
               sliderInput(inputId="winter",
                           min=0,
                           max=100,
                           value=50,
                           step = 1,
                           ticks=F,
                           label="Mild winters"),
               sliderInput(inputId="air",
                           min=0,
                           max=100,
                           value=50,
                           step = 1,
                           ticks=F,
                           label="Air quality")
               
               ),
        
        column(3,
               sliderInput(inputId="dist",
                           min=0,
                           max=100,
                           value=50,
                           step = 1,
                           ticks=F,
                           label="Close to \"civilization\""),
               numericInput("distlim", label="Maximum allowable distance (mi)", value=1000, min = 0, max = NA, step = NA,
                            width = NULL),
               
               sliderInput(inputId="distbig",
                           min=0,
                           max=100,
                           value=50,
                           step = 1,
                           ticks=F,
                           label="Close to a big city"),
               numericInput("distlimbig", label="Maximum allowable distance (mi)", value=1000, min = 0, max = NA, step = NA,
                            width = NULL),
               
               sliderInput(inputId="dem",
                           min=0,
                           max=100,
                           value=50,
                           step = 1,
                           ticks=F,
                           label="Politics"),
               
               radioButtons(inputId="party",label="Which politics?", choiceValues=c("blue","red","other"),choiceNames=c("Blue","Red","Other"),inline=T),
               
               sliderInput(inputId="cost",
                           min=0,
                           max=100,
                           value=50,
                           step = 1,
                           ticks=F,
                           label="Housing affordability"),
               
               numericRangeInput("costlim", label="Max/min median county rent", value=c(200,2500), width = NULL, separator = " to ")
               
               
        ),

        # Show a plot of the generated distribution
        column(6,
               
               # tags$style(".tooltip-inner {
               #  
               #    background-color: #795548 !important;
               #     color: #e2d4d0;
               #     max-width: 250px;
               #     text-align: left;
               #     font-size: 14px !important;}
               #     
               #     .tooltip.in {
               #      opacity: 1 !important;
               #     }
               #     
               #  .tooltip.top .tooltip-arrow {
               #  border-top-color: #795548;
               #  }
               # 
               #  .tooltip.right .tooltip-arrow {
               #    border-right-color: #795548;
               #      }
               # 
               #    .tooltip.bottom .tooltip-arrow {
               #  border-bottom-color: #795548;
               #  }
               # 
               #  .tooltip.left .tooltip-arrow {
               #    border-left-color: #795548;
               #  }
               #  
               #  "),
               
           htmlOutput("message"),
           checkboxInput(inputId="showmap",label="Show map?",value=T),
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    
    dat <- reactive({
        
            dat2 <- readRDS("~/location-preferences/cleaned_data.rds")

            dat2$dist_to_city_c <- ifelse(dat2$dist_to_city >= input$distlim,NA,dat2$dist_to_city_c)
            dat2$dist_to_citybig_c <- ifelse(dat2$dist_to_citybig >= input$distlimbig,NA,dat2$dist_to_citybig_c)
            
            dat2$housing_cost_c <- ifelse(dat2$housing_cost < input$costlim[1] | dat2$housing_cost > input$costlim[2],NA,dat2$housing_cost_c)
            
          #  dat2 <- dat2[which(dat2$dist_to_city<input$distlim),]

            climatec <- input$climate^2

            schoolc <- input$school^2
            
            demc <- input$dem^2
            
            winterc <- input$winter^2
            
            summerc <- input$summer^2
            
            distc <- input$dist^2
            
            costc <- input$cost^2
            
            airc <- input$air^2
            
            distbigc <- input$distbig^2
            
            if (input$party=="blue") {
                dat2$u <- -climatec*dat2$climate_damage_c + schoolc*dat2$educ_c + demc*dat2$dem_prop_c  - distbigc*dat2$dist_to_citybig_c -
                  summerc*dat2$maxtemp_c + winterc*dat2$mintemp_c - distc*dat2$dist_to_city_c - costc*dat2$housing_cost_c - airc*dat2$airpollution_c
            }
             else if (input$party=="red") {
                dat2$u <- -climatec*dat2$climate_damage_c + schoolc*dat2$educ_c + demc*dat2$rep_prop_c - distbigc*dat2$dist_to_citybig_c -
                  summerc*dat2$maxtemp_c + winterc*dat2$mintemp_c - distc*dat2$dist_to_city_c - costc*dat2$housing_cost_c - airc*dat2$airpollution_c
             } else {
                dat2$u <- -climatec*dat2$climate_damage_c + schoolc*dat2$educ_c + demc*dat2$other_prop_c  - distbigc*dat2$dist_to_citybig_c -
                  summerc*dat2$maxtemp_c + winterc*dat2$mintemp_c - distc*dat2$dist_to_city_c - costc*dat2$housing_cost_c - airc*dat2$airpollution_c
            }
            
            dat2$u <- (dat2$u - min(dat2$u,na.rm=T))
            dat2$u <- 100*dat2$u/max(dat2$u,na.rm=T)
            
            dat2

        })

    output$distPlot <- renderPlot({
      
      if (input$showmap==T) {
        test <- dat()
       # best <- dat()
        # b <- ggplot(best[which(!str_detect(best$NAME,"Alaska|Hawaii|Puerto Rico")),]) +
        #     geom_sf(aes(fill=u),color=NA) +
        #     scale_fill_viridis_c()
        
        
        
        b <- ggplot(test[which(!str_detect(test$NAME,"Alaska|Hawaii|Puerto Rico")),]) +
          geom_sf(aes(fill=u),color=NA) +
          scale_fill_viridis_c() +
          labs(fill="Score") +
          theme_map()
        b }

    })
    
    output$message <- renderUI({
      
      dat2 <- dat()
      
        
        best <- dat2[order(-dat2$u),] %>% head()
        best$link <- str_replace_all(best$NAME," ","_")
        best$link <- paste0("<a href=https://en.wikipedia.org/wiki/",best$link,">",best$NAME,"</a>")
        best$message <- paste0(best$link," (",round(best$dist_to_city)," miles to ", best$closest_city,")")
        best <- best$message %>% paste(collapse="<br>")

      HTML(paste(h3("Top 6 counties"),best,sep="<br>"))
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
