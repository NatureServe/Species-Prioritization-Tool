#' ---
#' title: NatureServe BLM Species Prioritization Tool Scoring App
#' ---
#' 
#' # Load packages
library(shiny)
library(dplyr)
library(DT)
library(googlesheets4)
library(googledrive)
library(shinyBS)
library(shinyWidgets)
library(shinyauthr)
library(shinyjs)
#'
#' Google oauth authentication
drive_auth(path = "BLM-Scores/skilful-berm-368100-59d29742d3f1.json")
gs4_auth(path = "BLM-Scores/skilful-berm-368100-59d29742d3f1.json")

#' Alternative authentication (subject to 2 factor)
#' #' ## designate project-specific cache
#options(gargle_oauth_cache = ".secrets")
#' ## check the value of the option, if you like
#gargle::gargle_oauth_cache()
#' ## trigger auth on purpose --> store a token in the specified cache
#drive_auth()
#' ## see your token file in the cache, if you like
#list.files(".secrets/")
#options(
#  gargle_oauth_cache = ".secrets",
#  gargle_oauth_email = TRUE
#)

#' # Add this code if need to add Login module
#' # Include simple login module for the app
#' ## Define database of authenticated users
#' ## In this case, only one is needed
# user_base <- dplyr::tibble(
#   user = c("blm_user"),
#   password = c("T@xon0m1c"),
#   permissions = c("admin"),
#   name = c("BLM User")
# )
#'
#' # Load Data
#' ## Initial scores
latest_scores <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet="ESA_spp_x_state") %>%
  data.frame(stringsAsFactors = TRUE) %>%
  dplyr::mutate(BLM.Practicability.Score = as.numeric(NA),
                BLM.Mutispecies.Score = as.numeric(NA),
                BLM.Partnering.Score = as.numeric(NA)) %>% 
  rename_with(.fn = gsub,pattern = "\\.", replacement = " ") %>%
  rename_with(.fn = gsub,pattern = "_", replacement = " ") %>% 
  dplyr::mutate(Notes = as.character(NA))
### Shiny App
shinyApp(
  ui = fluidPage(
    
    # Add this code if need to add Login module
    # # add logout button UI
    # div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
    # # add login panel UI function
    # shinyauthr::loginUI(id = "login"),
    
    theme = "style.css",
    
    shinyjs::useShinyjs(),
    
    div(id = "user_interface",
        
        fluidRow(style = "padding-left: 30px; padding-top: 20px;",
                 
                 h1("BLM Special Status Species Prioritization Scores")
        ),
        
        fluidRow(style = "padding-left: 50px; padding-top: 10px;",
                 
                 fluidRow(
                   h3("About you"),
                   fluidRow(p("Please enter your contact details - this is required to successfully submit your scores!"), style = "padding-left: 15px;"),
                   
                   column(width = 2, 
                          fluidRow(h4("First name: ")),
                          fluidRow(textInput(inputId = "first_name", label = "", width = "95%"))
                   ),
                   column(width = 2, 
                          fluidRow(h4("Last name: ")),
                          fluidRow(textInput(inputId = "last_name", label = "", width = "95%"))
                   ),
                   column(width = 2, 
                          fluidRow(h4("Email address: ")),
                          fluidRow(textInput(inputId = "email", label = "", width = "95%"))
                   ),
                   column(width = 2, 
                          fluidRow(h4("BLM affiliation: ")),
                          fluidRow(selectizeInput("selected_state", "", choices = c("Headquarters", sort(latest_scores$`BLM SSS State`)) %>% unique(), width = "100%"))
                   )
                 ),
                 
                 fluidRow(
                   
                   h3("Review and update priority scores"),
                   
                   fluidRow(p("Instructions:"), style = "padding-left: 15px;"),
                   fluidRow(p("1. Use the dropdown menus 'Filter by taxon' and 'Filter by state' below to filter the table."), style = "padding-left: 15px;"),
                   fluidRow(p("2. Use the dropdown menus 'Set all ... scores to' to update values across all selected species for the corresponding field"), style = "padding-left: 15px;"),               
                   fluidRow(p("3. Double-click on any individual cell to edit its value - make sure you double-click outside the cell to save your entry before moving on to a different one"), style = "padding-left: 15px;"),               
                   fluidRow(p("4. Skip to the following pages using the menu at the bottom right of the table"), style = "padding-left: 15px;"),
                   fluidRow(p("5. When you are done, click Submit!"), style = "padding-left: 15px;")
                 ),
                 
                 fluidRow(
                   h3("Filter data:")
                 ),
                 
                 fluidRow(
                   column(3, style = "padding-left: 0;", selectizeInput("selected_taxon1", "Filter by taxon (high level)", choices = c("All", sort(latest_scores$`Major Group`)) %>% unique(), width = "100%")),
                   column(3, style = "padding-left: 0;", selectizeInput("selected_taxon2", "Filter by taxon (mid level)", choices = c("All", sort(latest_scores$`Higher Level Informal Group`)) %>% unique(), width = "100%")),
                   column(3, style = "padding-left: 0;", selectizeInput("selected_taxon3", "Filter by taxon (low level)", choices = c("All", sort(latest_scores$`Lower Level Informal Group`)) %>% unique(), width = "100%"))
                 ),
                 
                 fluidRow(
                   h3("Set default scores for all taxa:")
                 ),
                 
                 fluidRow(
                   column(3, style = "padding-left: 0;", selectInput(inputId = "modify_practicability_scores", label = "Set all BLM practicability scores to:", choices = c("", 1:5), width = "100%")),
                   column(3, style = "padding-left: 0;", selectInput(inputId = "modify_multispecies_scores", label = "Set all BLM multispecies scores to:", choices = c("", 1:5), width = "100%")),
                   column(3, style = "padding-left: 0;", selectInput(inputId = "modify_partnering_scores", label = "Set all BLM partnering scores to:", choices = c("", 1:5), width = "100%"))
                 ),
                 
                 
                 fluidRow(style = "padding-bottom: 20px;",
                          
                          DT::dataTableOutput("scores_table", width = "95%", height = 300)
                 ),
                 
                 fluidRow(
                   h3("Submit your scores"),
                   
                   fluidRow(style = "padding-left: 15px; padding-bottom: 50px;", bsButton(inputId = "submit", label = "Submit", style = "primary", size = "large")),
                 )
        )
    )
  ),
  server = function(input, output, session) {
    
    # Add this code if need to add Login module
    # # call login module supplying data frame,
    # # user and password cols and reactive trigger
    # credentials <- shinyauthr::loginServer(
    #   id = "login",
    #   data = user_base,
    #   user_col = user,
    #   pwd_col = password,
    #   log_out = reactive(logout_init())
    # )
    # # call the logout module with reactive trigger to hide/show
    # logout_init <- shinyauthr::logoutServer(
    #   id = "logout",
    #   active = reactive(credentials()$user_auth)
    # )
    
    # Add this code if need to add Login module
    # observe({
    #   
    #   if(credentials()$user_auth) shinyjs::show("user_interface")
    # 
    #   })
    
    state_scores <- latest_scores_edits <- reactiveValues(values = latest_scores)
    
    observeEvent(input$selected_state, {
      
      if (input$selected_state != "Headquarters"){
        
        state_scores$values <- state_scores$values %>%
          dplyr::filter(`BLM SSS State` == input$selected_state)
      }
      
    })
    
    observeEvent(input$selected_taxon1, {
      
      if (input$selected_taxon1 != "All"){
        
        latest_scores_edits$values <- state_scores$values %>%
          dplyr::filter(`Major Group` == input$selected_taxon1)
      }
      
    })
    
    observeEvent(input$selected_taxon2, {
      
      if (input$selected_taxon2 != "All"){
        
        latest_scores_edits$values <- state_scores$values %>%
          dplyr::filter(`Higher Level Informal Group` == input$selected_taxon2)
      }
      
    })
    
    observeEvent(input$selected_taxon3, {
      
      if (input$selected_taxon3 != "All"){
        
        latest_scores_edits$values <- state_scores$values %>%
          dplyr::filter(`Lower Level Informal Group` == input$selected_taxon3)
      }
      
    })
    
    
    output$scores_table <- renderDT({
      
      # Add this code if need to add Login module
      # req(credentials()$user_auth)
      
      # if (input$selected_state != "Headquarters"){
      #   latest_scores_edits$values <- latest_scores_edits$values %>%
      #     dplyr::filter(`BLM SSS State` == input$selected_state)
      # }
      
      # if (input$selected_taxon1 != "All"){
      #   updateSelectizeInput(session = session, "selected_taxon2", "Filter by taxon (mid level)", selected = "All")
      #   updateSelectizeInput(session = session, "selected_taxon3", "Filter by taxon (low level)", selected = "All")
      #   updateSelectizeInput(session = session, "selected_taxon1", "Filter by taxon (high level)", selected = input$selected_taxon1)
      #   latest_scores_edits$values <- state_scores$values %>%
      #     dplyr::filter(`Major Group` == input$selected_taxon1)
      # }
      # 
      # if (input$selected_taxon2 != "All"){
      #   updateSelectizeInput(session = session, "selected_taxon1", "Filter by taxon (high level)", selected = "All")
      #   updateSelectizeInput(session = session, "selected_taxon3", "Filter by taxon (low level)", selected = "All")
      #   updateSelectizeInput(session = session, "selected_taxon2", "Filter by taxon (mid level)", selected = input$selected_taxon2)
      #   latest_scores_edits$values <- state_scores$values %>%
      #     dplyr::filter(`Higher Level Informal Group` == input$selected_taxon2)
      # }
      # 
      # if (input$selected_taxon3 != "All"){
      #   updateSelectizeInput(session = session, "selected_taxon1", "Filter by taxon (high level)", selected = "All")
      #   updateSelectizeInput(session = session, "selected_taxon2", "Filter by taxon (mid level)", selected = "All")
      #   updateSelectizeInput(session = session, "selected_taxon3", "Filter by taxon (mid level)", selected = input$selected_taxon3)
      #   latest_scores_edits$values <- state_scores$values %>%
      #     dplyr::filter(`Lower Level Informal Group` == input$selected_taxon3)
      # }
      
      print(latest_scores_edits$values)
      
      datatable(latest_scores_edits$values, options = list(dom = 'tp', pageLength = 20), editable = list(target = "cell", disable = list(columns = c(1:12)), numeric = c(13:15), area = 16), selection = 'none')
      
    })
    
    scores_table_proxy <- dataTableProxy('scores_table')
    
    observeEvent(input$modify_practicability_scores, {      
      
      if (input$modify_practicability_scores != ""){
        
        new_practicability_scores <- tibble(row = 1:nrow(latest_scores_edits$values), col = 13, value = input$modify_practicability_scores)
        
        latest_scores_edits$values <<- editData(latest_scores_edits$values, new_practicability_scores, 'scores_table')
        
        print(new_practicability_scores)
      }
      
    })
    
    observeEvent(input$modify_multispecies_scores, {      
      
      if (input$modify_multispecies_scores != ""){
        
        new_multispecies_scores <- tibble(row = 1:nrow(latest_scores_edits$values), col = 14, value = input$modify_multispecies_scores)
        
        latest_scores_edits$values <<- editData(latest_scores_edits$values, new_multispecies_scores, 'scores_table')
        
      }
      
    })
    
    
    observeEvent(input$modify_partnering_scores, {      
      
      if (input$modify_partnering_scores != ""){
        
        new_partnering_scores <- tibble(row = 1:nrow(latest_scores_edits$values), col = 15, value = input$modify_partnering_scores)
        
        latest_scores_edits$values <<- editData(latest_scores_edits$values, new_partnering_scores, 'scores_table')
        
      }
      
    })
    
    observeEvent(input$scores_table_cell_edit, {
      
      
      # if (input$selected_taxon1 != "All"){
      #   latest_scores_edits$values <- scores$values %>%
      #     dplyr::filter(`Major Group` == input$selected_taxon1)
      # }
      # 
      # if (input$selected_taxon2 != "All"){
      #   latest_scores_edits$values <- latest_scores_edits$values %>%
      #     dplyr::filter(`Higher Level Informal Group` == input$selected_taxon2)
      # }
      # 
      # if (input$selected_taxon3 != "All"){
      #   latest_scores_edits$values <- latest_scores_edits$values %>%
      #     dplyr::filter(`Lower Level Informal Group` == input$selected_taxon3)
      # }
      
      latest_scores_edits$values <<- editData(latest_scores_edits$values, input$scores_table_cell_edit, 'scores_table')
      
      print(latest_scores_edits$values)
      
    })
    
    saved_scores <- reactiveValues(values = head(latest_scores, 1)[-1, ])
    
    
    observeEvent(input$save, {
      
      saved_scores$values <- rbind(saved_scores$values, latest_scores_edits$values)
      
      latest_scores_edits$values <- latest_scores
      
      print(latest_scores_edits$values)
      
      updateSelectizeInput(session = session, "selected_taxon1", "Filter by taxon (high level)", selected = "All")
      updateSelectizeInput(session = session, "selected_taxon2", "Filter by taxon (mid level)", selected = "All")
      updateSelectizeInput(session = session, "selected_taxon3", "Filter by taxon (low level)", selected = "All")
      
      print(latest_scores_edits$values)
      
      latest_scores_edits$values <<- editData(state_scores$values, data.frame(row = 1, col = 1, value = NA), 'scores_table')
      
    })
    
    observeEvent(input$submit, {
      
      if (input$email != ""){
        saved_scores$values <- saved_scores$values %>% 
          dplyr::mutate(Reviewer.Name = paste(input$first_name, input$last_name, sep = " "),
                        Reviewer.Email = input$email,
                        Reviewer.Affiliation = input$affiliation
          )
        sheet_append(ss = "https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", data = isolate(saved_scores$values), sheet = "suggested_scores")
        sendSweetAlert(session, type = "success", title = "Success!", text = "We have received your scores\nThank you so much for your time!", closeOnClickOutside = TRUE)
      } else {
        sendSweetAlert(session, type = "warning", title = "Oops!", text = "You need to enter your contact details at the top of this form", closeOnClickOutside = TRUE)
      }
      
    })
    
  }
)

# code 2 tables, unfiltered, filtered
