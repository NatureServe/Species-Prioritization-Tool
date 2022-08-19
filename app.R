#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#.rs.files.restoreBindings() ##run this to publish app

library(shiny)
library(ggplot2)
library(readxl)
library(tidyverse)
library(shinyWidgets)

sss<-read_excel(path = "Data/Prioritization_Tool_11Aug2022.xlsx", sheet = "Data")
sss$Percent_EOs_BLM<-as.numeric(sss$Percent_EOs_BLM)
sss$Percent_Model_Area_BLM<-as.numeric(sss$Percent_Model_Area_BLM)

##source code for prioritization function
source('Prioritization-function.R', local = T)

# Define UI for application that draws a histogram
ui <- fluidPage(
  chooseSliderSkin("Big"), # 'Shiny', 'Flat', 'Modern', 'Nice', 'Simple', 'HTML5', 'Round' and 'Square'

    fluidRow(style = "padding-left: 30px; padding-top: 20px;",
             
             img(src = "ns_logo.png"),
             
             h1("BLM SSS Prioritization Tool")
    ),
    
    # Inputs for prioritization tool
    fluidRow(style = "padding-left: 50px; padding-top: 10px; background-color: lightgrey;",
             
             fluidRow(
               h3("Input thresholds for species prioritization"),
               h4("Stewardship Responsibility"),
               column(width = 6,
                      sliderInput("threshold.eo",
                                  "Percent of species' EOs on BLM-Administered Lands >",
                                  min = 1,
                                  max = 100,
                                  value = 30)),
               column(width = 6,
                      fluidRow(h4("Partnering Opportunities >")),
                      fluidRow(selectInput(inputId = "threshold.partner", choices = c(1:5), selected = 3, label = "", width = "25%")))
             ),
             
             fluidRow(style = "padding-top: 15px;",
                      column(width = 6,
                             sliderInput("threshold.model",
                                         "Percent of modeled species distribution on BLM-Administered Lands >",
                                         min = 1,
                                         max = 100,
                                         value = 30*.7)),
                      column(width = 6,
                             fluidRow(h4("Practical Conservation Value >")),
                             fluidRow(selectInput(inputId = "threshold.practical", choices = c(1:5), selected = 3, label = "", width = "25%")))
                      )#,
             #fluidRow(style = "padding-top: 15px;", 
                      #actionButton(inputId = "reset", label = "Return to default thresholds", width = "25%"))
             ),
    
    ##View results         
    fluidRow(style = "padding-left: 50px; padding-top: 10px; border: 4px solid lightgrey;",
        fluidRow(h3("View and filter prioritization results")),
             
        column(width = 6,
               plotOutput("distPlot"),
               h4("Fig 1. Number of taxa in each Tier based on prioritization criteria.")),
        
        column(width = 6,
               selectizeInput("selected_Tier", "Filter results by Tier", choices = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"), selected = "Tier 1", width = "50%"),
               tableOutput("data.table"))
     ),
    
    ##about the app
    fluidRow(style = "padding-left: 50px; padding-top: 10px;",
             h4("This application was developed by NatureServe for the Bureau of Land Management. It is intended to aid in the selection of prioritization criteria for SSS. The application uses selected inputs to assign tiers to 87 test species that were provided by BLM. Input data include practicality for conservation of the species, as scored by BLM staff (1 = low, 5 = high) and partnering opportunities presented by the species, as scored by BLM staff (1 = low, 5 = high). Species with Inventory Priority equal to True have a habitat model that experts reviewed as poor. Species that are a monitoring priority are those with an unknown short-term trend and a rank that was reviewed in the past 10 years."))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  new_dat <- reactive({
    # generate results based on inputs from ui.R
    results<-prioritize(species = sss$Scientific_Name, threshold.eo = input$threshold.eo, threshold.model = input$threshold.model, threshold.practical = input$threshold.practical, threshold.partner = input$threshold.partner)
    results
  })
    
  output$distPlot <- renderPlot({

      ##plot results
      results<-new_dat()
      data.plot<-data.frame(table(results$Tier))
      
      ##get the label positions
      data.plot <- data.plot %>%
        arrange(desc(Var1)) %>%
        mutate(lab.ypos = cumsum(Freq) - 0.5*Freq) %>%
        data.frame()
      data.plot
      
      fig <- ggplot(data.plot, aes(x = 2, y = Freq, fill = Var1)) +
        geom_bar(stat = "identity", color = "white") +
        coord_polar(theta = "y", start = 0)+
        geom_text(aes(y = lab.ypos, label = Freq), color = "black", size=8)+
        #geom_text(aes(y = 1, x = 1, label = paste0(round(label*100,0), "%")), color = c("black"), size = 6) +
        scale_fill_brewer(palette = "Greens", name="", direction = -1) +
        theme_void() +
        xlim(.9, 2.5) +
        theme(text = element_text(size = 20), legend.position="right")
      fig
    })
    
    output$data.table<-renderTable(subset(new_dat(), Tier == input$selected_Tier, select = -Species))
}

# Run the application 
shinyApp(ui = ui, server = server)
