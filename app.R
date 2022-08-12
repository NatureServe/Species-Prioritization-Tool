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

sss<-read_excel(path = "Data/Prioritization_Tool_11Aug2022.xlsx", sheet = "Data")
sss$Percent_EOs_BLM<-as.numeric(sss$Percent_EOs_BLM)
sss$Percent_Model_Area_BLM<-as.numeric(sss$Percent_Model_Area_BLM)

##source code for prioritization function
source('Prioritization-function.R', local = T)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Species Prioritization Tool"),
    
    # Inputs for prioritization tool
    fluidRow(style = "padding-left: 50px; padding-top: 10px;",
             
             fluidRow(
               h3("Thresholds for Species Prioritization"),
               column(width = 4,
                      fluidRow(h4("Practical Conservation Value >")),
                      fluidRow(selectInput(inputId = "threshold.practical", choices = c(1:5), selected = 3, label = "", width = "95%"))),
               column(width = 4,
                      fluidRow(h4("Partnering Opportunities >")),
                      fluidRow(selectInput(inputId = "threshold.partner", choices = c(1:5), selected = 3, label = "", width = "95%"))),
               sliderInput("threshold.overlap",
                           "Percent of species distribution on BLM-Administered Lands",
                           min = 1,
                           max = 100,
                           value = 30)
             ),
             
             fluidRow(
               plotOutput("distPlot"),
               
               column(2, style = "padding-left: 0;", selectizeInput("selected_Tier", "Filter by Tier", choices = c("Tier 1", "Tier 2", "Tier 3", "Tier 4"), selected = "Tier 1")),
               
               tableOutput("data.table")
             )
             )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  new_dat <- reactive({
    # generate results based on inputs from ui.R
    results<-prioritize(species = sss$Scientific_Name, threshold.eo = input$threshold.overlap, threshold.model = input$threshold.overlap, threshold.practical = input$threshold.practical, threshold.partner = input$threshold.partner)
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
