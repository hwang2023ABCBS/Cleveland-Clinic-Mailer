# Sys.unsetenv("DOWNLOAD_STATIC_LIBV8")
# Sys.setenv(V8_PKG_CFLAGS="-I/usr/local/v8/include", V8_PKG_LIBS="-L/usr/local/v8/lib -lv8_monolith")
# install.packages('libv8')


# install.packages("V8", configure.vars="INCLUDE_DIR=/usr/local/v8/include LIB_DIR=/usr/local/v8/lib V8_PKG_LIBS=-lv8_monolith")

# now everything works well!
# DONE
# running into deploy issue, configuration of V8 package on Connect fails. Contacted IT.... 

library(tidyverse)
library(data.table)
library(lubridate)
library(dbplyr)
library(shiny)
library(DBI)
library(DT)
library(shinyWidgets)
library(bslib)
library(shinydashboard)
library(gt)
library(gtsummary)

# source other script files

# source("data_puller.R")
load(file = 'EnvObjects.RData')



# gg theme -------
gg_theme <- theme(axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 12),
                  plot.title = element_text(hjust = 0.5, size = 14))


# ui ------------------

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "zephyr"),
  titlePanel("Cleveland Clinic Partnership/Data Needs for Mailer"),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        "Cohorts","Pick a cohort:",
        choices= unique(temp$Cohort),
        selected = 'Aortic',
        multiple = TRUE,  
        options = list('action_box'= TRUE)
      ),
      # add TERMED filter 
      pickerInput(
        "Termed","Termed Status:",
        choices= c("Yes","No","All"),
        # choices= unique(temp$TERMED),
        selected = "All",
        multiple = FALSE,
        options = list('action_box'= TRUE)
      ),
      # put cohort selection criteria here 
      uiOutput('criteria'),
      # value box to show member count for each selected cohort
      uiOutput('unique_member_count_box'),
      # value box to show date range (relative dynamic timeframe)
      uiOutput('dateRangeBox')
      
    ),
    mainPanel(
      
      tabsetPanel(
        tabPanel(title = 'Cohort Members',
                 # uiOutput('unique_member_count_box')
                 # show table here for selected cohort members
                 fluidRow(
                   column(12,
                          downloadButton("downloadData", "Download Filtered Data"),
                          DTOutput("filteredTable")
                   )
                 )
                 
                 
                 
        ),
        tabPanel(title = 'Characteristics&Conditions',
                 # add CSS code here for formatting....
                 tags$style(HTML("
                                 .gt_table {
                                 margin-left: auto;
                                 margin-right: auto;
                                 width: 100%;
                                 }")),
                 fluidRow(
                   column(12,
                          gt_output("filteredCharacteristicTable")
                   )

                 )
        ),
        tabPanel(title = 'Utilizations&Costs',
                 fluidRow(
                   column(12,
                          downloadButton("downloadCostData", "Download Filtered Cost Summary"),
                          DTOutput("filteredCostTable"),
                          textOutput("staticInfoText")  # for interpretation of ER, IP... utilizations counts
                   )
                 )
        )
        
      )
    )
  )
)

# server ------------------
server <- function(input,output,session){
  
  # value box 1
  # reactive expression to filter data based on selected cohort
  unique_member_count <- reactive({
    if (input$Termed=="All"){
      # if "All" is selected, ignore the TERMED filter
      temp %>% 
        filter(Cohort %in% input$Cohorts) %>% 
        summarise(uniqueCount= n_distinct(MEMBERID)) %>% 
        pull(uniqueCount)
      
    } else {
      # otherwise, filter by selected TERMED values
      temp %>% 
        filter(Cohort %in% input$Cohorts, TERMED== input$Termed) %>% 
        summarise(uniqueCount= n_distinct(MEMBERID)) %>% 
        pull(uniqueCount)
    }
    
    
    
  })
  
  # calculate the unique member count based on selected cohort
  output$unique_member_count_box <- renderUI({
    value_box(
      title = "Total Unique Member Count",
      value = unique_member_count(),
      showcase = bsicons::bs_icon("People fill"),
      theme = "primary"
    )
  })
  
  # value box 2
  # reactive expression to calculate the date range
  date_range_text <- reactive({
    date_24Months_ago <- Sys.Date() %m-% months(24)
    date_range <- paste(format(date_24Months_ago, "%m/%d/%Y"), " - ",format(Sys.Date(), "%m/%d/%Y"))
    return(date_range)
  })
  
  # render the date range in value box
  output$dateRangeBox <- renderUI({
    value_box(
      title = "Timeframe Range",
      value = date_range_text(),
      showcase = bsicons::bs_icon("calendar-range-fill"),
      theme = "primary"
    )
  })
  
  
  
  
  # criteria message
  ## reactive expression to get the criteria message based on the selected cohort
  criteria_message <- reactive({
    selected_cohort <- input$Cohorts
    criteria_tbl %>%
      filter(Cohort %in% selected_cohort) %>%
      pull(Criteria)
  })
  
  ## render the criteria message
  output$criteria <- renderUI({
    selected_criteria <- criteria_message()
    HTML(selected_criteria)
  })
  
  # reactive expression of showing table accordingly based on cohort picked
  CPT_DX_table4Cohort_filtered <- reactive({
    if(input$Termed=="All"){
      MbrCohortLevelSummary_tbl_combined %>% filter(Cohort %in% input$Cohorts)
    } else {
      MbrCohortLevelSummary_tbl_combined %>% filter(Cohort %in% input$Cohorts, TERMED== input$Termed)
      
    }
    
    
  })
  
  # render the filtered table
  output$filteredTable <- renderDT({
    datatable(CPT_DX_table4Cohort_filtered())
  })
  
  
  # Download handler to download the filtered data 
  output$downloadData <- downloadHandler(
    filename = function(){
      paste("filtered_data_", input$Cohorts,".csv", sep = "")
    }, 
    content = function(file) {
      write.csv(CPT_DX_table4Cohort_filtered(), file, row.names = FALSE)
    }
  )
  
  # reactive expression to show characteristic breakout tables.
  characteristic_filtered <- reactive({
    if(input$Termed=="All"){
      temp %>%
        filter(Cohort %in% input$Cohorts) 
    } else {
      temp %>%
        filter(Cohort %in% input$Cohorts,TERMED== input$Termed) 
    }


  })

  # create a summary table using tbl_summary
  summary_table <- reactive({
    characteristic_filtered() %>%
      # distinct(MEMBERID) %>%    # some member fall into multiple cohort buckets
      tbl_summary(
        include= c(GENDER,
                   AGE_GROUP,
                   RACE,
                   RACE_MINORITY_CAT,
                   SOCIOECONOMIC_SVI_CAT,
                   HOUSEHOLD_SVI_CAT,
                   MINORITY_SVI_CAT,
                   HOUSING_TRANSP_SVI_CAT,
                   ALL_SVI_CAT,
                   NEAREST_ED_DIST_TRACT,
                   PCP_PER_100K_FCC,
                   AIDS:SYMPTOMS_SIGNS_ABNORMALITES),
        missing = "ifany"
      ) %>%
      add_n() %>%
      # add_p() %>%
      modify_header(label="**Variable**") %>%
      bold_labels()

  })

  output$filteredCharacteristicTable <- render_gt({
    summary_table() %>% as_gt()%>%
      opt_interactive(use_compact_mode = TRUE, page_size_default = 22,height = 900)  # GREAT JOB! NOW THE TABLE TAKE THE WHOLE SPACE.
      # REF: https://gt.rstudio.com/reference/gt_output.html?q=shiny

  })

  # Tab3: 
  # reactive expression of showing cost table accordingly based on cohort picked
  Cost_table4Cohort_filtered <- reactive({
    if(input$Termed == "All") {
      temp2 %>% filter(Cohort %in% input$Cohorts)
    } else {
      temp2 %>% filter(Cohort %in% input$Cohorts,TERMED== input$Termed)
    }
    
  })
  
  
  
  # render the filtered table
  output$filteredCostTable <- renderDT({
    datatable(Cost_table4Cohort_filtered())
  })
  
  # Download handler to download the filtered data 
  output$downloadCostData <- downloadHandler(
    filename = function(){
      paste("filtered_data_cost_", input$Cohorts,".csv", sep = "")
    }, 
    content = function(file) {
      write.csv(filteredCostTable(), file, row.names = FALSE)
    }
  )
  
  
  # render staticInfoText
  output$staticInfoText <- renderText({
    "All utilization counts of ER to TH are the month counts that a member had at least 1 specific utlization claim."
  })
  
  
  
  
  
}


shinyApp(ui= ui, server= server)
