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

#.rs.files.restoreBindings() ##run this before publishing, then comment out to publish

#'
#' Google oauth authentication
drive_auth(path = "skilful-berm-368100-59d29742d3f1.json")
gs4_auth(path = "skilful-berm-368100-59d29742d3f1.json")

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
  user = c("blm_user"), ## blm_user
  password = c("T@xon0m1c"),
  # password = c("123"), #simple pw for testing
  permissions = c("admin"),
  name = c("BLM User")
)
#'
#' # Load Data
#' ## Initial scores
latest_scores <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet="ESA_spp_2023-02-02") %>%
  data.frame(stringsAsFactors = TRUE) %>%
  dplyr::mutate(Notes = as.character(NA),
                Provisional_Tier = as.factor(Tier),
                Group = as.factor(Lower_Level_Informal_Group),
                Global_Rank = as.factor(Rounded_Global_Rank),
                BLM_SSS_States = ifelse(is.na(BLM_SSS_States), "NA", BLM_SSS_States),
                Evaluation = paste(Evaluation, ifelse(is.na(HQ_Notes),"",paste0("Comments from BLM HQ: ", HQ_Notes)))) %>% 
  rename_with(.fn = gsub,pattern = "\\.", replacement = " ") %>%
  rename_with(.fn = gsub,pattern = "_", replacement = " ") #%>% 
#dplyr::select("Higher Level Informal Group", "Scientific Name", "NatureServe Common Name", "Rounded Global Rank", "ESA Status", "BLM SSS States", "USFWS Recovery Priority Num", "Evaluation", "Tier", "BLM Practicability Score", "BLM Multispecies Score", "BLM Partnering Score", "Notes")
## Replace scientific name with active natureserve explorer url

## Suggested scores by BLM staff
suggested_scores <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1KIpQPLvHiJY1KvbGY3P04HwU2WESqKOQZYECpN_dxgo/edit?usp=sharing", sheet="suggested_scores") %>% unique() %>%
  data.frame(stringsAsFactors = TRUE) %>% rename_with(.fn = gsub,pattern = "\\.", replacement = " ") %>% mutate(Reviewers = paste0(`Reviewer Name`, " (", `Reviewer Affiliation`, ")")) %>% group_by(`Scientific Name`) %>% summarise(Reviewers = paste(Reviewers, collapse = ", "), Reviews = paste(Notes, collapse = "; "))

latest_scores <- left_join(latest_scores, suggested_scores) %>% 
  mutate(`Scientific Name` = paste0("<a href='", `Explorer url`,"' target='_blank'>", `Scientific Name`,"</a>"),
         Reviewers = ifelse(is.na(Reviewers), "None", Reviewers))

## Code to make a larger text box for the notes field in the table. Source; https://github.com/rstudio/DT/issues/821
# callback <- c(
#   "table.on('focus', 'td.areaEdit input[type=text]', function(){",
#   "  $(this).on('blur', function(e){",
#   "    e.stopImmediatePropagation();",
#   "    return false;",
#   "  })",
#   "});",
#   "var id = $(table.table().node()).closest('.datatables').attr('id');",
#   "$.contextMenu({",
#   "  selector: '#' + id + ' td.areaEdit input[type=text]',",
#   "  trigger: 'hover',",
#   "  autoHide: true,",
#   "  items: {",
#   "    text: {",
#   "      name: 'Edit:',",
#   "      type: 'textarea',",
#   "      value: ''",
#   "    },",
#   "    sep1: '---------',",
#   "    cancel: {",
#   "      name: 'Cancel',",
#   "      icon: function($element, key, item){",
#   "        return 'context-menu-icon context-menu-icon-quit';",
#   "      },",
#   "      callback: function(itemKey, opts, e){",
#   "        this.trigger('change');",
#   "      }",
#   "    }",
#   "  },",
#   "  events: {",
#   "    show: function(opts){",
#   "      $.contextMenu.setInputValues(opts, {text: opts.$trigger.val()});",
#   "    },",
#   "    hide: function(opts){",
#   "      var $this = this;",
#   "      var data = $.contextMenu.getInputValues(opts, $this.data());",
#   "      var $input = opts.$trigger;",
#   "      $input.val(data.text);",
#   "      $input.trigger('change');",
#   "    }",
#   "  }",
#   "});"
# )

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
    
    # js function to reset a button, variableName is the button name whose value we want to reset
    tags$script("Shiny.addCustomMessageHandler('resetInputValue', function(variableName){
                Shiny.onInputChange(variableName, null);
                });
                "),
    
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
                            fluidRow(selectizeInput("selected_state", "", choices = c("", "Headquarters", sort(gsub(" ", "", gsub(" ", "", unique(strsplit(paste0(latest_scores$`BLM SSS States`, collapse = ","), split = ",")[[1]]))))), width = "100%"))
                     )
                   ),
                   
                   fluidRow(
                     
                     h3("Review and update priority scores"),
                     
                     fluidRow(p("Instructions:"), style = "padding-left: 15px;"),
                     fluidRow(p("1. Use the dropdown menu 'BLM affiliation' above to see scores for taxa in your state."), style = "padding-left: 15px;"),
                     fluidRow(p("2. Use the filtering cells below the field names in the table below to filter the table."), style = "padding-left: 15px;"),
                     fluidRow(p("3. Review BLM scores and resulting Tier assignments for relevant species. Double-click on any cell in the last 4 columns to edit its value - make sure you click outside the cell to save your entry before moving on to a different one"), style = "padding-left: 15px;"),
                     fluidRow(p("4. Select species for which you have reviewed the BLM scores by clicking on the row. If you have reviewed all species in your state, use the 'Mark BLM scores for all species in your state as reviewed' toggle at the bottom of the page as a shortcut"), style = "padding-left: 15px;"),
                     fluidRow(p("5. Navigate to more pages of results using the menu at the bottom right of the table"), style = "padding-left: 15px;"),
                     fluidRow(p("6. After you have clicked on every row for which you have reviewed the scores, click Submit!"), style = "padding-left: 15px;"),
                     fluidRow(
                       fluidRow(p("**Refer to the decision tree and the Assessment field in the table to interpret the assigned Tier**"), style = "padding-left: 18px;"),
                       actionButton(inputId = "view_tree", label = "View Prioritization Decision Tree", style = "secondary"),
                       tags$style(
                         type = 'text/css',
                         '.modal-dialog { width: fit-content !important; }'
                       ),
                       bsModal(id = "decisiontree", title = "Prioritization Decision Tree", trigger = "view_tree", img(src = "decision_tree.png"), size = "large")
                     )
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
    ),
    uiOutput("modal")
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
    
    # previousPage <- NULL ##previous page arguments now allow you to preseve the page when you edit a score, but it doesn't work with a filter (ends up removing the filter)
    
    # state_scores <- reactiveValues(values = latest_scores)
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }
    
    state_scores <- reactiveValues(values = cbind(subset(latest_scores, select=c("Group", "Scientific Name", "NatureServe Common Name", "Global Rank", "USESA Status", "BLM SSS States", "States of Occurrence", "Provisional Tier")), `Provisional Assessment` = shinyInput(actionButton, nrow(latest_scores), 'button_', label = "Assessment", onclick = 'Shiny.onInputChange(\"select_button\",  this.id)' ), subset(latest_scores, select=c("Practical Cons BLM Score", "Multispecies Benefit BLM Score", "Partnering Opps BLM Score", "Notes"))))
    latest_scores_edits <- reactiveValues(values = latest_scores)
    
    observeEvent(
      {
        input$selected_state
        # input$selected_taxon2
      }, {
        
        if (input$selected_state != ""){
          shinyjs::show("filtered_table_panel")
          # if (input$selected_state != ""){
          #   state_scores$values <- state_scores$values %>%
          #     dplyr::filter(grepl(x = `States of Occurrence`, pattern = ifelse(input$selected_state != "Headquarters", input$selected_state, paste(c("CA", "WY", "AZ", "NM", "NV", "UT", "OR", "CO", "MT", "AK", "ID"), collapse = "|"))))
          # }
        }
        
      })
    
    observe({
      latest_scores_edits$values <- state_scores$values %>% dplyr::filter(grepl(x = `States of Occurrence`, pattern = ifelse(input$selected_state != "Headquarters", input$selected_state, paste(c("CA", "WY", "AZ", "NM", "NV", "UT", "OR", "CO", "MT", "AK", "ID"), collapse = "|"))))
    })
    
    output$filtered_table <- renderDT({
      
      # Add this code if need to add Login module
      req(credentials()$user_auth)
      
      n.cols <- ncol(state_scores$values %>% dplyr::filter(grepl(x = `States of Occurrence`, pattern = ifelse(input$selected_state != "Headquarters", input$selected_state, paste(c("CA", "WY", "AZ", "NM", "NV", "UT", "OR", "CO", "MT", "AK", "ID"), collapse = "|")))))
      
      datatable(state_scores$values %>% dplyr::filter(grepl(x = `States of Occurrence`, pattern = ifelse(input$selected_state != "Headquarters", input$selected_state, paste(c("CA", "WY", "AZ", "NM", "NV", "UT", "OR", "CO", "MT", "AK", "ID"), collapse = "|")))),
                editable = list(target = "cell", disable = list(columns = c(1:(n.cols-4)))),
                # callback = JS(callback),
                options = list(
                  dom = 'tp', pageLength = 10
                  # columnDefs = list(
                  #   list(targets = n.cols, className = "areaEdit")
                  # ),
                ),
                selection = list(mode = "multiple", target = "row"), 
                filter = list(position = 'top', columns = 1:n.cols), escape = F)
      
    })
    
    # Create proxy for filtered_table
    filtered_table_proxy <- dataTableProxy("filtered_table") 
    
    observeEvent(input$filtered_table_cell_edit, {
      
      latest_scores_edits$values <<- editData(latest_scores_edits$values, input$filtered_table_cell_edit, filtered_table_proxy, resetPaging = FALSE)
      
    })
    
    Eval <- reactiveValues(text = '')
    
    observeEvent(input$select_button, {
      s <- as.numeric(strsplit(input$select_button, "_")[[1]][2])
      Eval$text <<- latest_scores$Evaluation[s]
      output$reviews <- renderTable(subset(latest_scores, select = c("Reviewers", "Reviews"))[s,])
      output$scores <- renderTable(subset(latest_scores, select=c("Riparian", "Percent EOs BLM 2019", "Percent Model Area BLM", "Percent AB EOs BLM", "USFWS Recovery Priority Num"))[s,] %>% rename("Percent of EOs on BLM-administered lands" = "Percent EOs BLM 2019", "Percent of modeled habitat on BLM-administered lands" = "Percent Model Area BLM", "Percent of EOs with rank A/B on BLM-administered lands" = "Percent AB EOs BLM"))
      output$modal <- renderUI({
        tagList(
          bsModal(paste('model', s ,sep=''), "Provisional Assessment", "select_button", size = "small",
                  fluidRow(
                    column(width = 6,
                           p(renderText({Eval$text})),
                           h5("Table 1. Review history. Notes made by BLM staff during reviews of provisional scores and tiers."),
                           tableOutput("reviews")
                           ),
                    column(width = 6, 
                           h5("Table 2. Additional input data for prioritization of this taxon. NA values indicate that no data were available for assessment."),
                           tableOutput("scores")
                           )
                  )
                  
                  # fluidRow(align = "center",
                  #          h4("Prioritization Decision Tree"),
                  #          img(src = "decision_tree.png"))
          ))
      })
      toggleModal(session,paste('model', s ,sep=''), toggle = "Assessment")
      ##Reset the select_button
      session$sendCustomMessage(type = 'resetInputValue', message =  "select_button")
    })
    
    observeEvent(input$reviewed_all, {      
      
      if (isTRUE(input$reviewed_all)){
        
        filtered_table_proxy %>% 
          selectRows(selected = 1:nrow(latest_scores_edits$values))
        
      }
      
      if (isFALSE(input$reviewed_all)){
        
        filtered_table_proxy %>% 
          reloadData(clearSelection = "row")
        
      }
      
    })
    
    observeEvent(input$submit, {
      
      if (input$email != "" & length(input$filtered_table_rows_selected)>0){
        reviewed_scores <- latest_scores_edits$values[input$filtered_table_rows_selected, ] %>% 
          dplyr::mutate(Date = Sys.Date(), `Reviewer Name` = paste(input$first_name, input$last_name, sep = " "),
                        `Reviewer Email` = input$email,
                        `Reviewer Affiliation` = input$selected_state,
                        `Scientific Name` = sub(pattern = ".*>(.+)</a>.*", x = `Scientific Name`, replacement = "\\1") #find text in between >link text</a>
          ) %>% 
          dplyr::select(Date, `Reviewer Name`, `Reviewer Email`, `Reviewer Affiliation`, `Scientific Name`, `NatureServe Common Name`, `Provisional Tier`, `Practical Cons BLM Score`,	`Multispecies Benefit BLM Score`,	`Partnering Opps BLM Score`, `Notes`)
        
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