

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(tidyverse)
library(patchwork)
library(shinyBS)
library(googlesheets4)
library(tippy)

options(gargle_oob_default = TRUE)
#options(gargle_oauth_cache = ".secrets")
# Or, if you don't use multiple Google identities, you can be more vague:

#gs4_auth(cache=".secrets")
#response_data <- matrix(data=NA,nrow=1,ncol=42) %>% as.data.frame()

#ss <- gs4_create("survey_responses", sheets=response_data)



ess_list <- "Supermarkets, convenience stores<br>Specialty food stores, bakeries<br>Beer, wine and liquor stores<br>Health care, medical and pharmacy<br>Pet store services<br>Gas stations<br>Home improvement/hardware stores<br>Office supplies"
noness_list <- "Shopping malls, clothing and shoe stores<br>Barber shops, hair, nail and tanning salons<br>Non-medical wellness spas<br>Tattoo/piercing parlors<br>Galleries"
school_list <- "Public and private school for kindergarten through 12th grade<br>Child daycare services<br>Fine Arts school, language schools<br>School and employee bus transportation<br>Sports and recreation instruction<br>Exam preparation and tutoring<br>Automobile driving schools"
uni_list <- "Academic four- and two-year colleges and professional schools<br>Business schools and computer and management training<br>Technical and trade schools<br>Libraries and archives"
outdoor_list <- "National, state or local park services and administration<br>RV parks and recreational camps, marinas<br>Public beaches, public pools<br>Sports courts, skate parks<br>Zoos and botanical gardens, nature parks<br>Landscaping services, golf courses and country clubs<br>Skiing facilities, amusement and theme parks"
indoor_list <- "Fitness and recreational sports centers<br>Yoga studios, dance studios, tennis clubs<br>Martial arts centers<br>Roller-skating and ice-skating rinks<br>Bowling alleys, pool halls, arcades"
theater_list <- "Movie theaters, live theater<br>Performing arts venues<br>Community theater<br>Bands, orchestras, choirs"
restaurant_list <- "Restaurants, cafes, food courts<br>Bars, taverns, brew pubs, wine bars<br>Cafes and coffee shops<br>Social and private clubs<br>Senior centers, youth clubs"
meeting_list <- "Churches, synagogues, mosques, and other houses of worship<br>Facilities for weddings, wedding receptions<br>Facilities for funerals and memorial services<br>Conference centers or similar business meeting facilities (e.g. at hotels)<br>Support-group meetings"
alf_list <- "Nursing home and residential care facilities<br>Community housing services, temporary shelters<br>Facilities support for military bases<br>Facilities support for jails, prisons, psychiatric hospitals"


ui <- fluidPage(
    # tags$script(jscode),
    setBackgroundColor("#c9deff"),
    fluidRow(
        column(3,
               
               sliderInput(inputId="dem",
                           min=0,
                           max=100,
                           value=0,
                           step = 1,
                           ticks=F,
                           label="Politics"),
              
                  radioButtons(inputId="party",label="Red or Blue counties?", choiceValues=c("red","blue"),choiceNames=c("Red","Blue"),inline=T),
               
              
        ),
        
        column(3,
               sliderInput(inputId="crime",
                           min=0,
                           max=100,
                           value=0,
                           step = 1,
                           ticks=F,
                           label="Violent crime rate (incidents per 100k)"),
               box(height=70,width=100,
                   htmlOutput("crime_mess"))
               
        ),
        
        column(6, align="center",

               
               htmlOutput("message"),
               textOutput("message"),
               plotOutput("plot")
               
               
        )
    )
)





server <- function(input, output) {
    
    
    output$message <- renderUI({
        
        hcost <- paste0("$","100")
        
        a <- "TESTING"

        hcost
    })
    
    output$climate_mess <- ({
        b <- "test message"
        b
    })
    
    output$plot <- renderPlot({
        
        bar_graph <- ggplot() +
            geom_point(aes(x=input$dem,y=input$vax))
        
        bar_graph
        
    }, height=140, width=350)

    
    
}

shinyApp(ui = ui, server = server)
