### Libraries
library(shiny)
library(dplyr)
library(DT)
library(googlesheets4)
library(googledrive)
library(shinyBS)

#'
#run this code if get publishing error: .rs.files.restoreBindings()
#'
#' ##### Google oauth authentication
#' designate project-specific cache
#options(gargle_oauth_cache = ".secrets")
#' check the value of the option, if you like
#gargle::gargle_oauth_cache()
#' trigger auth on purpose --> store a token in the specified cache
#drive_auth()
#' see your token file in the cache, if you like
#list.files(".secrets/")
#options(
#  gargle_oauth_cache = ".secrets",
#  gargle_oauth_email = TRUE
#)

drive_auth(path = "BLM-Scores/skilful-berm-368100-59d29742d3f1.json")
gs4_auth(path = "BLM-Scores/skilful-berm-368100-59d29742d3f1.json")

### Data

input_data <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet="ESA_spp_x_state") %>% 
  #separate_rows(BLM_SSS_States, sep = ", |,") %>% ##split comments into separate rows based on separator
  data.frame(stringsAsFactors = FALSE) %>%
  rename_with(.fn = gsub,pattern = "\\.", replacement = "\n") %>%
  rename_with(.fn = gsub,pattern = "_", replacement = "\n")

fields_edit<-names(input_data)[13:15]

### Module
modFunction <- function(input, output, session, data, reset, submit, fields) {
  
  v <- reactiveValues(data = data)
  
  proxy = dataTableProxy("mod_table")
  
  observeEvent(input$mod_table_cell_edit, {
    print(names(v$data))
    info = input$mod_table_cell_edit
    str(info)
    i = info$row
    j = info$col
    k = info$value
    str(info)
    
    isolate(
      if (j %in% match(fields, names(v$data))) {
        print(match(fields, names(v$data)))
        v$data[i, j] <<- DT::coerceValue(k, v$data[i, j])
        print(v$data)
      } else {
        stop("You are not supposed to change this column.") # check to stop the user from editing only few columns
      }
    )
    replaceData(proxy, v$data, resetPaging = FALSE)  # replaces data displayed by the updated table
    
  })
  
  ## Select state
  
  observeEvent(input$selected_state, {
    if (input$selected_state != "All"){
      v$data <- proxy %>% 
        dplyr::filter(grepl(input$selected_state,`BLM\nSSS\nStates`))
    }
  })
  
  ### Reset Table
  observeEvent(reset(), {
    v$data <- data # your default data
    
  })
  
  output$mod_table <- DT::renderDataTable({
    DT::datatable(v$data, editable = list(target= "column", disable = list(columns = 1:12))) %>% formatStyle(columns = fields_edit, backgroundColor = "lightyellow", fontWeight = "bold")
    
  })
  
  observeEvent(submit(), {
    sheet_write(ss = "https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", data = isolate(v$data), sheet = "ESA_spp_x_state")
  })
  
}

modFunctionUI <- function(id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("mod_table"))
  
}

### Shiny App
shinyApp(
  ui = basicPage(
    mainPanel(
      actionButton("reset", "Reset"),
      tags$hr(),
      selectizeInput("selected_state", "Select State", choices = c("All", input_data$`BLM\nSSS\nStates` %>% unique() %>% sort()), width = "100%"),
      modFunctionUI("editable"),
      bsButton(inputId = "submit", label = "Submit", style = "primary", size = "large")
    )
  ),
  server = function(input, output) {
    demodata<-input_data
    callModule(modFunction,"editable", demodata,
               reset = reactive(input$reset), submit = reactive(input$submit),
               fields = fields_edit)
  }
)