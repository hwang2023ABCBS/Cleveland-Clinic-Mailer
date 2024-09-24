# update 9/17 this works, need a little edit 
# 9/18 rename to "Cohort Explorer" 
# 9/23 change to 36 months look back from 24 months
# next: add spinner when make changes on filters, recaculating and update outputs..   
# ref: https://github.com/daattali/shinycssloaders


# libraries ---------------------------------------------------------------


library(shiny)
library(bslib)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(dbplyr)
library(DBI)
library(tidyverse)
library(data.table)
library(shinyWidgets)
library(profvis)
library(gt)
library(gtsummary)

# Con ---------------------------------------------------------------------
con <- DBI::dbConnect(odbc::odbc(), "snowflakeodbc", UID = "SFK_SVC_PRD_POSIT")
dbGetQuery(con, 'use warehouse PRD_ML_STD_WH')
dbGetQuery(con, 'USE DATABASE PRD_DATASCIENCE_DB')
dbGetQuery(con,'USE SCHEMA HEALTHECON')

# Tables ------------------------------------------------------------------

# mbr claims
MEMBER_MONTHLY_COMPLETE <- tbl(con, in_catalog('PRD_DATASCIENCE_DB','HEALTHECON','MEMBER_MONTHLY_COMPLETE'))

# mbr characteristics
MBR_MASTER_STATIC <- tbl(con, in_catalog('PRD_DATASCIENCE_DB','HEALTHECON','MBR_MASTER_STATIC'))

# mbr CPT codes
CPT_Code_tbl <- tbl(con, in_catalog('PRD_RAW_DB', 'VDT_CM', 'FINALCLAIMSDETAIL'))

# mbr Diagnosis codes
Diag_Code_tbl <- tbl(con, in_catalog('PRD_RAW_DB', 'VDT_CM', 'FINALCLAIMSDETAILCODE'))

# load the package and overwrite dplyr methods
library(duckplyr) # use duckdb to make opearations on remote big data more efficiently
duckplyr::methods_overwrite()
# turn off with duckplyr::methods_restore()





# select time range 
# date_24Months_ago <- Sys.Date() %m-% months(24) 


# filter
# joined_data <- CPT_Code_tbl %>%
#   select(MVDID:SERVICEFROMDATE,PROCEDURECODE,ALLOWEDAMOUNT,PAIDAMOUNT,PARTYKEY:COMPANYKEY) %>%
#   mutate(MONTH= floor_date(SERVICEFROMDATE,'month')) %>% 
#   filter(MONTH >= date_24Months_ago) %>% 
#   inner_join(Diag_Code_tbl %>% select(CLAIMNUMBER,CODEVALUE), by=join_by(CLAIMNUMBER)) %>%
#   distinct() %>%
#   rename('DXCODE'='CODEVALUE') %>% 
#   inner_join(MEMBER_MONTHLY_COMPLETE %>% select(MEMBERID,MONTH,ALLOWED:TH) %>% filter(MONTH>= date_24Months_ago), join_by(MEMBERID,MONTH)) %>% 
#   inner_join(MBR_MASTER_STATIC %>% select(MBR_ID,GENDER,AGE_GROUP,RACE,STATE,SOCIOECONOMIC_SVI_CAT,HOUSING_TRANSP_SVI_CAT),
#              join_by("MEMBERID"=="MBR_ID")) 




# shiny starts ------------------------------------------------------------


# Shiny example
ui <- page_sidebar(
  title = "Cohort Explorer",
  theme= bs_theme(bootswatch = "minty"),
  # theme= bs_theme(
  #   bg = "#101010",
  #   fg = "#FFF",
  #   primary = "#E69F00",
  #   secondary = "#0072B2",
  #   success = "#009E73",
  #   base_font = font_google("Inter"),
  #   code_font = font_google("JetBrains Mono")
  # )
  sidebar= sidebar(
    textInput("diag_codes", "Enter Diagnosis Codes (comma separated):", value = "I350"),
    switchInput("diagnosis_logic","Diagnosis Logic:", onLabel = "AND", offLabel = "OR", value = FALSE),
    textInput("CPT_codes", "Enter Procedure Codes (comma separated):", value = "93303"),
    switchInput("procedure_logic","Procedure Logic:", onLabel = "AND", offLabel = "OR", value = FALSE),
    pickerInput('company_key', 'Company_Key',choices = NULL ,
                selected = NULL, options = pickerOptions(actionsBox = TRUE), multiple = TRUE),
    pickerInput('age_group',"Age_Group", choices = NULL ,
                selected = NULL, options = pickerOptions(actionsBox = TRUE), multiple = TRUE),
    pickerInput('gender','Gender', choices = NULL,
                selected = NULL, options = pickerOptions(actionsBox = TRUE),multiple = TRUE),
    pickerInput('race','Race', choices = NULL, 
                selected = NULL, options = pickerOptions(actionsBox = TRUE), multiple = TRUE ),
    pickerInput('socioeconomic_svi_cat','Socioeconomic_SVI_Cat', choices = NULL,
                selected = NULL, options = pickerOptions(actionsBox = TRUE), multiple = TRUE ),
    pickerInput('housing_transp_svi_cat','Housing_Transp_SVI_Cat', choices = NULL,
                selected = NULL, options = pickerOptions(actionsBox = TRUE), multiple = TRUE),
    
    actionButton("submit", "Submit")
    
    
    
  ),
  mainPanel(
    navset_pill(
      nav_panel(title= "Overall Cohort Utilization & Cost Summary", 
                # p("First tab content.")
                card(
                  card_header("Corhot Summary Table:"),
                  card_body(DTOutput("summary_tbl"))
                ),
                card(
                  card_header("Message:"),
                  card_body(uiOutput("message_ui"))
                ),
                card(
                  card_header("Trend:"),
                  card_body (plotOutput("trend_plot"))
                )
      ),
      nav_panel(title= "Demographics", 
                # p("Second tab content.")
                card(
                  card_header("Demographic Decompostion"),
                  card_body(gt_output('filteredCharacteristicTable'))
                )),
      nav_panel(title = "Event Study", p("Third tab content")),
      nav_spacer()
    )
  )
)

server <- function(input, output,session) {
  # bs_themer()
  
  
  # select time range 
  # date_24Months_ago <- Sys.Date() %m-% months(24) 
  # date_24Months_ago <- today()-months(24)
  date_36Months_ago <- today()-months(36)  # change to 36 months look back .. 
  
  
  # filter
  selected_joined_data <- reactive({
    
    diag_codes <- unlist(strsplit(input$diag_codes, ",\\s*"))
    CPT_codes <- unlist(strsplit(input$CPT_codes, ",\\s*"))
    print(diag_codes)
    print(CPT_codes)
    diagnosis_logic <- if(input$diagnosis_logic) "AND" else "OR"
    procedure_logic <- if(input$procedure_logic) "AND" else "OR"
    
    
    CPT_Code_tbl %>%
      select(MVDID:SERVICEFROMDATE,PROCEDURECODE,PARTYKEY:COMPANYKEY) %>%
      # mutate(MONTH= floor_date(SERVICEFROMDATE,'month')) %>% 
      filter(SERVICEFROMDATE >= date_36Months_ago,
             # !is.na(PROCEDURECODE) & PROCEDURECODE %in% !!input$CPT_codes,
             if(procedure_logic == "OR") PROCEDURECODE %in% CPT_codes else all(PROCEDURECODE %in% CPT_codes),
      ) %>% 
      inner_join(Diag_Code_tbl %>% 
                   select(CLAIMNUMBER,CODEVALUE),
                 filter(
                   # !is.na(CODEVALUE) & CODEVALUE %in% !!input$ diag_codes
                   if(diagnosis_logic == "OR") CODEVALUE %in% diag_codes else all(CODEVALUE %in% diag_codes)
                 ),
                 by=join_by(CLAIMNUMBER)) %>%
      distinct(MEMBERID
               # PROCEDURECODE,
               # CODEVALUE
      ) %>%
      # rename('DXCODE'='CODEVALUE') %>% 
      inner_join(MEMBER_MONTHLY_COMPLETE %>% select(MEMBERID,MONTH,ALLOWED:TH) %>% filter(MONTH>= date_36Months_ago), join_by(MEMBERID)) %>% 
      inner_join(MBR_MASTER_STATIC %>% select(MBR_ID,GENDER,COMPANY_KEY,AGE_GROUP,RACE,STATE,SOCIOECONOMIC_SVI_CAT,HOUSING_TRANSP_SVI_CAT),
                 join_by("MEMBERID"=="MBR_ID")) %>% 
      collect() %>% 
      # convert to factors and set level orders
      mutate(across(where(is.character),as.factor), COMPANY_KEY= as.factor(COMPANY_KEY))%>% 
      mutate(across(where(is.factor), ~ fct_explicit_na(.x, na_level= "Missing"))) %>% 
      mutate(across(where(is.factor), ~ fct_relevel(.x, "Missing", after = Inf))) %>% 
      mutate(AGE_GROUP= factor(AGE_GROUP, levels= c("Age [0-18]","Age [19-30]","Age [31-40]","Age [41-50]","Age [51-60]","Age [>60]")),
             STATE_Simplified= ifelse(STATE=="Arkansas","Arkansas","Non-Arkansas"),
             COMPANY_KEY_Simplified= ifelse(COMPANY_KEY=="11753","Walmart","Non-Walmart"),
             across(c(SOCIOECONOMIC_SVI_CAT:HOUSING_TRANSP_SVI_CAT), ~factor(.x, levels=c("Low","Moderate","High","Very High")))
      )
      
    
  })
  
  
  
  # filter data now
  filtered_data <- reactive({
    req(input$submit)
    isolate({
      diag_codes <- unlist(strsplit(input$diag_codes, ",\\s*"))
      CPT_codes <- unlist(strsplit(input$CPT_codes, ",\\s*"))
      # 
      # diagnosis_logic <- if(input$diagnosis_logic) "AND" else "OR"
      # procedure_logic <- if(input$procedure_logic) "AND" else "OR"
      
      selected_joined_data()  %>% 
        # mutate(Month= as.Date(floor_date(SERVICEFROMDATE,"month"))) %>% 
        filter(
          # if(diagnosis_logic == "OR") CODEVALUE %in% diag_codes else all(CODEVALUE %in% diag_codes),
          # if(procedure_logic == "OR") PROCEDURECODE %in% CPT_codes else all(PROCEDURECODE %in% CPT_codes),
          # CODEVALUE %in% !!input$diag_codes,
          # PROCEDURECODE %in% !!input$CPT_codes,
          COMPANY_KEY %in% !! input$company_key,
          AGE_GROUP %in% !!input$age_group,
          GENDER %in% !!input$gender,
          RACE %in% !!input$race,
          SOCIOECONOMIC_SVI_CAT %in% !!input$socioeconomic_svi_cat,
          HOUSING_TRANSP_SVI_CAT %in% !!input$housing_transp_svi_cat
          # MONTH >= date_36Months_ago
        ) %>%
        group_by(MONTH) %>%
        summarise(TOTAL_ALLOWED = round(sum(ALLOWED),2),
                  TOTAL_PAID = round(sum(PAID),2),
                  member_count = n_distinct(MEMBERID),
                  case_count = n()
        ) %>% 
        mutate(pmpm_ALLOWED = round(TOTAL_ALLOWED / member_count, 2),
               pmpm_PAID = round(TOTAL_PAID / member_count,2)) %>% 
        collect()
      
    })
  })
  
  
  # Update selectInput choices dynamically
  observe({
    
    selected_joined_data <- selected_joined_data() # access the reactive expression
    print(head(selected_joined_data))
    print(colnames(selected_joined_data))
    print(glimpse(selected_joined_data))
    if(is.null(selected_joined_data) || nrow(selected_joined_data)==0) {
      # notification
      # showNotification("The combination of CPT codes and diagnosis codes does not exist.", type = "error")
      
      # another way: update the message send to ui to display there
      output$message_ui <- renderUI({
        div(style= "color: red;","Oops! The combination of CPT codes and diagnosis codes does not exist. Please adjust!")
      })
      
      # Set default choices or empty choices
      updatePickerInput(session, 'company_key', choices = c("No data available"))
      updatePickerInput(session, 'age_group', choices = c("No data available"))
      updatePickerInput(session, 'gender', choices = c("No data available"))
      updatePickerInput(session, 'race', choices = c("No data available"))
      updatePickerInput(session, 'socioeconomic_svi_cat', choices = c("No data available"))
      updatePickerInput(session, 'housing_transp_svi_cat', choices = c("No data available"))
      
    } else {
      # notification
      # showNotification("The combination of CPT codes and diagnosis codes does exist.", type = "message")
      output$message_ui <- renderUI({
        div(style= "color: green;","Great! we found the cohort for you!")
      })
      
      # update the picker with actual data
      updatePickerInput(session, 'company_key', choices = unique(selected_joined_data %>% pull(COMPANY_KEY)),selected =unique(selected_joined_data %>% pull(COMPANY_KEY)) )
      updatePickerInput(session, 'age_group', choices = unique(selected_joined_data %>% pull(AGE_GROUP)), selected = unique(selected_joined_data %>% pull(AGE_GROUP)))
      updatePickerInput(session, 'gender', choices = unique(selected_joined_data %>% pull(GENDER)), selected = unique(selected_joined_data %>% pull(GENDER)))
      updatePickerInput(session, 'race', choices = unique(selected_joined_data %>% pull(RACE)), selected = unique(selected_joined_data %>% pull(RACE)) )
      updatePickerInput(session, 'socioeconomic_svi_cat', choices = unique(selected_joined_data%>% pull(SOCIOECONOMIC_SVI_CAT)), selected = unique(selected_joined_data%>% pull(SOCIOECONOMIC_SVI_CAT)))
      updatePickerInput(session, 'housing_transp_svi_cat', choices = unique(selected_joined_data %>% pull(HOUSING_TRANSP_SVI_CAT)), selected =unique(selected_joined_data %>% pull(HOUSING_TRANSP_SVI_CAT)) )
      
    }
    
  })
  
  
  output$summary_tbl <- renderDT({
    filtered_data()
  })
  
  output$trend_plot <- renderPlot({
    ggplot(filtered_data(), aes(x= MONTH))+
      geom_line(aes(y= pmpm_ALLOWED, color= "pmpm_ALLOWED"))+
      geom_line(aes(y= pmpm_PAID, color= 'pmpm_PAID'))+
      labs(title= "Trend Line for PMPM Allowed and Paid",
           x= "Month",
           y= "Amount",
           color= "Legend")+
      theme_minimal()
  })
  
  # page 2: demographics table
  # reactive expression to show characteristic breakout tables.
  characteristic_filtered <- reactive({
    selected_joined_data() %>%
      filter(
        COMPANY_KEY %in% !! input$company_key,
        AGE_GROUP %in% !!input$age_group,
        GENDER %in% !!input$gender,
        RACE %in% !!input$race,
        SOCIOECONOMIC_SVI_CAT %in% !!input$socioeconomic_svi_cat,
        HOUSING_TRANSP_SVI_CAT %in% !!input$housing_transp_svi_cat
      ) %>%
      # collect()
      compute()
    
  })
  
  
  # create a summary table using tbl_summary
  demographics_summary_table <- reactive({
    characteristic_filtered() %>%
      tbl_summary(
        include= c(COMPANY_KEY_Simplified,
                   # COMPANY_KEY,
                   # STATE,
                   STATE_Simplified,
                   GENDER,
                   AGE_GROUP,
                   RACE,
                   SOCIOECONOMIC_SVI_CAT,
                   HOUSING_TRANSP_SVI_CAT
        ),
        missing = "ifany"
        # ,sort = list(everything() ~ "frequency")
      ) %>%
      add_n() %>%
      # add_p() %>%
      modify_header(label="**Variable**") %>%
      bold_labels() 
  })
  
  
  
  output$filteredCharacteristicTable <- render_gt({
    demographics_summary_table() %>% as_gt()%>%
      opt_interactive(use_compact_mode = TRUE, page_size_default = 22,height = 900)  # GREAT JOB! NOW THE TABLE TAKE THE WHOLE SPACE.
    # REF: https://gt.rstudio.com/reference/gt_output.html?q=shiny
    
  })
  
  
}


# bs_theme_preview()

# runApp(list(ui=ui, server=server), launch.browser = rstudioapi::viewer)

shinyApp(ui, server)




