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

# .rs.files.restoreBindings() ##run this to publish app

#'
#' Google oauth authentication
drive_auth(path = "BLM-Scores/skilful-berm-368100-59d29742d3f1.json")
gs4_auth(path = "BLM-Scores/skilful-berm-368100-59d29742d3f1.json")

#' Alternative authentication (subject to 2 factor)
#' #' ## designate project-specific cache
# options(gargle_oauth_cache = ".secrets")
## check the value of the option, if you like
# gargle::gargle_oauth_cache()
## trigger auth on purpose --> store a token in the specified cache
# drive_auth()
## see your token file in the cache, if you like
# list.files(".secrets/")
# options(
#   gargle_oauth_cache = ".secrets",
#   gargle_oauth_email = TRUE
# )

#' # Add this code if need to add Login module
#' # Include simple login module for the app
#' ## Define database of authenticated users
#' ## In this case, only one is needed
user_base <- dplyr::tibble(
  user = c("blm_user"),
  # password = c("T@xon0m1c"),
  password = c("123"), #simple pw for testing
  permissions = c("admin"),
  name = c("BLM User")
)
#'
#' # Load Data
#' ## Initial scores
latest_scores <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet="ESA_spp_2022-12-09") %>%
  data.frame(stringsAsFactors = TRUE) %>%
  dplyr::mutate(Notes = as.character(NA),
                Higher_Level_Informal_Group = as.factor(Higher_Level_Informal_Group),
                Scientific_Name = paste0("<a href='", `Explorer.url`,"' target='_blank'>", Scientific_Name,"</a>"),
                Evaluation = paste(Evaluation, ifelse(is.na(HQ_Notes),"",HQ_Notes)),
                BLM_SSS_States = ifelse(is.na(BLM_SSS_States), "NA", BLM_SSS_States)) %>% 
  rename_with(.fn = gsub,pattern = "\\.", replacement = " ") %>%
  rename_with(.fn = gsub,pattern = "_", replacement = " ") %>% 
  dplyr::select("Higher Level Informal Group", "Scientific Name", "NatureServe Common Name", "Rounded Global Rank", "ESA Status", "BLM SSS States", "USFWS Recovery Priority Num", "Evaluation", "Tier", "BLM Practicability Score", "BLM Mutispecies Score", "BLM Partnering Score", "Notes")
## Replace scientific name with active natureserve explorer url

### Shiny App
shinyApp(
  ui = fluidPage(
    
    # Add this code if need to add Login module
    # add logout button UI
    # div(class = "pull-right", shinyauthr::logoutUI(id = "logout")),
    # add login panel UI function
    shinyauthr::loginUI(id = "login"),
    
    theme = "style.css",
    
    shinyjs::useShinyjs(),
    
    shinyjs::hidden(
      div(id = "user_interface",
          
          fluidRow(style = "padding-left: 30px; padding-top: 20px;",
                   
                   img(src = "ns_logo.png"),
                   
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
                            fluidRow(selectizeInput("selected_state", "", choices = c("", "Headquarters", gsub(" ", "", gsub(" ", "", sort(unique(strsplit(paste0(latest_scores$`BLM SSS States`, collapse = ","), split = ",")[[1]]))))) %>% unique(), width = "100%"))
                     )
                   ),
                   
                   fluidRow(
                     
                     h3("Review and update priority scores"),
                     
                     fluidRow(p("Instructions:"), style = "padding-left: 15px;"),
                     fluidRow(p("1. Use the dropdown menu 'BLM affiliation' above to see scores for taxa in your state."), style = "padding-left: 15px;"),
                     fluidRow(p("2. Use the dropdown menus below the field names in the table below to filter the table."), style = "padding-left: 15px;"),
                     fluidRow(p("3. Review BLM scores and resulting Tier assignments for relevant species. Double-click on any cell highlighted in yellow to edit its value - make sure you click outside the cell to save your entry before moving on to a different one"), style = "padding-left: 15px;"),
                     fluidRow(p("4. Navigate to more pages of results using the menu at the bottom right of the table"), style = "padding-left: 15px;"),
                     fluidRow(p("5. After editing scores, select species for which you have reviewed the BLM scores by clicking on the row. Note that additional edits to cell values will reset the selected rows. If you have reviewed all species in your state, use the 'Mark BLM scores for all species in your state as reviewed' toggle at the bottom of the page as a shortcut"), style = "padding-left: 15px;"),
                     fluidRow(p("6. After you have clicked on every row for which you have reviewed the scores, click Submit!"), style = "padding-left: 15px;")
                   ),
                   
                   fluidRow(
                     fluidRow(p("**Refer to the decision tree and the Evaluation field in the table to interpret the assigned Tier**"), style = "padding-left: 18px;"),
                     actionButton(inputId = "view_tree", label = "View Prioritization Decision Tree", style = "secondary"),
                     tags$style(
                       type = 'text/css',
                       '.modal-dialog { width: fit-content !important; }'
                     ),
                     bsModal(id = "decisiontree", title = "Prioritization Decision Tree", trigger = "view_tree", img(src = "decision_tree.png"), size = "large")
                   ),
                   
                   # fluidRow(
                   #   h3("Update scores:")
                   # ),
                   
                   # shinyjs::hidden(
                   #   div(id = "scores_table_panel",
                   #       
                   #       fluidRow(style = "padding-bottom: 20px;",
                   #                
                   #                DT::dataTableOutput("scores_table", width = "95%", height = 300)
                   #       )
                   #   )
                   # ),
                   
                   shinyjs::hidden(
                     div(id = "filtered_table_panel",
                         
                         fluidRow(style = "padding-bottom: 20px;",
                                  
                                  DT::dataTableOutput("filtered_table", width = "95%", height = 300)
                         )
                     )
                   ),
                   
                   fluidRow(
                     column(3, style = "padding-left: 30; margin-top: 30px;", prettySwitch(inputId = "reviewed_all", label = "Mark BLM scores for all species in your state as reviewed", status = "primary"))
                   ),
                   
                   fluidRow(
                     h3("Submit your scores"),
                     
                     fluidRow(style = "padding-left: 15px; padding-bottom: 50px;", bsButton(inputId = "submit", label = "Submit", style = "primary", size = "large")),
                   )
          )
      )
    )
  ),
  server = function(input, output, session) {
    
    # Add this code if need to add Login module
    # call login module supplying data frame,
    # user and password cols and reactive trigger
    credentials <- shinyauthr::loginServer(
      id = "login",
      data = user_base,
      user_col = user,
      pwd_col = password,
      log_out = reactive(logout_init())
    )
    # call the logout module with reactive trigger to hide/show
    logout_init <- shinyauthr::logoutServer(
      id = "logout",
      active = reactive(credentials()$user_auth)
    )
    
    # Add this code if need to add Login module
    observe({
      
      if(credentials()$user_auth) shinyjs::show("user_interface")
      
    })
    
    state_scores <- reactiveValues(values = latest_scores)
    latest_scores_edits <- reactiveValues(values = latest_scores)
    
    observeEvent(
      {
        input$selected_state
        # input$selected_taxon2
      }, {
        
        if (input$selected_state != ""){
          shinyjs::show("filtered_table_panel")
          if (input$selected_state != ""){
            latest_scores_edits$values <- state_scores$values %>%
              dplyr::filter(grepl(x = `BLM SSS States`, pattern = ifelse(input$selected_state != "Headquarters", input$selected_state, paste(c("CA", "WY", "AZ", "NM", "NV", "UT", "OR", "CO", "MT", "AK", "ID", "NA"), collapse = "|"))))
            
            ## Allow HQ to see taxa with no BLM SSS state (NA value) - changed NA to character above
            # if (input$selected_state != "Headquarters") {
            #   latest_scores_edits$values <- latest_scores_edits$values %>%
            #     filter(grepl(x = `BLM SSS States`, pattern = input$selected_state))
            # } else {
            #   latest_scores_edits$values <- latest_scores_edits$values %>%
            #     filter(grepl(x = `BLM SSS States`, pattern = paste(c("CA", "WY", "AZ", "NM", "NV", "UT", "OR", "CO", "MT", "AK", "ID", "NA"), collapse = "|")) | is.na(`BLM SSS States`))
            #   }
          }
        }
        
      })
    
    output$filtered_table <- renderDT({
      
      # Add this code if need to add Login module
      req(credentials()$user_auth)
      
      n.cols<-ncol(latest_scores_edits$values)
      
      datatable(latest_scores_edits$values, options = list(dom = 'tp', pageLength = 10), editable = list(target = "cell", disable = list(columns = c(1:(n.cols-4)))), selection = list(mode = "multiple", target = "row"), filter = list(position = 'top', columns = c(1,(n.cols-5):(n.cols-2))), escape = F) #%>%
        # formatStyle(columns = c((n.cols-3):n.cols), backgroundColor = "lightyellow")
      
    })
    
    # Create proxy for filtered_table
    filtered_table_proxy <- dataTableProxy("filtered_table")
    
    observeEvent(input$filtered_table_cell_edit, {
      
      latest_scores_edits$values <<- editData(latest_scores_edits$values, input$filtered_table_cell_edit, 'filtered_table')
      
      print(input$filtered_table_search_columns)
      
      if(input$filtered_table_search_columns[1] != '') {
        filtered_table_proxy %>% updateSearch(keywords = list(global = isolate(input$filtered_table_search),
                                                              columns = c("", isolate(input$filtered_table_search_columns)))
        )
      }
      
      print(input$filtered_table_search_columns)
      
    })
    
    observeEvent(input$reviewed_all, {      
      
      if (isTRUE(input$reviewed_all)){
        
        dataTableProxy("filtered_table") %>% 
          selectRows(selected = 1:nrow(latest_scores_edits$values))
        
      }
      
      if (isFALSE(input$reviewed_all)){
        
        dataTableProxy("filtered_table") %>% 
         reloadData(clearSelection = "row")
       
     }
      
    })
    
    observeEvent(input$submit, {
      
      if (input$email != "" & length(input$filtered_table_rows_selected)>0){
        reviewed_scores <- latest_scores_edits$values[input$filtered_table_rows_selected, ] %>% 
          dplyr::mutate(`Reviewer Name` = paste(input$first_name, input$last_name, sep = " "),
                        `Reviewer Email` = input$email,
                        `Reviewer Affiliation` = input$selected_state,
                        `Scientific Name` = sub(pattern = ".*>(.+)</a>.*", x = `Scientific Name`, replacement = "\\1") #find text in between >link text</a>
          ) %>% 
          dplyr::select(`Reviewer Name`, `Reviewer Email`, `Reviewer Affiliation`, names(latest_scores_edits$values))
        
        sheet_append(ss = "https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", data = reviewed_scores, sheet = "suggested_scores")
        # session$reload()
        sendSweetAlert(session, type = "success", title = "Success!", text = paste0("We have received your scores for ", nrow(reviewed_scores), " species"), closeOnClickOutside = TRUE)
      } else {
        if (input$email == "") {
          sendSweetAlert(session, type = "warning", title = "Oops!", text = "You need to enter your contact details at the top of this form", closeOnClickOutside = TRUE)
        } else {
          sendSweetAlert(session, type = "warning", title = "Oops!", text = "You did not select any scores to submit. Select the scores you have reviewed by clicking on the row.", closeOnClickOutside = TRUE)
        }
      }
      
    })
    
  }
)