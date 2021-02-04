### Join SELECx by State v2 

library(shiny)
library(shinyFeedback)
library(tidyverse)
library(lubridate)
library(vroom)
library(tools)
library(readxl)
library(openxlsx)


# UI ----------------------------------------------------------------------


ui <- fluidPage(
    shinyFeedback::useShinyFeedback(),
    
    titlePanel("SELECx - Check Submission"),
    hr(),
    
    fluidRow(
        column(8,
               
               helpText("1",tags$sup("st"),": Download .csv or .xlsx from SELECx, Rename that files to English (Short names)"),
               helpText("2",tags$sup("nd"),": Prepare student's ID file that has student's id, student's name
           as column names:\'ID',\'Name' respectively"),
           
           helpText("3",tags$sup("rd"),": Upload multiple files from SELECx"),
           
           fileInput("file", NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload files",
                     placeholder = "choose file .csv or .xlsx",multiple = TRUE),
        ),
        column(4,
               
               downloadButton("download", "Download Data .xlsx"),
               downloadButton("download_miss", "Download Missing Name .xlsx")
               
               
        )
        
        
    ),
    
    
    fluidRow(
        column(6,
               helpText("4",tags$sup("th"),": Upload student's ID file and choose more column if you want"),
               fileInput("file_id", NULL, accept = c(".csv", ".xls",".xlsx"),buttonLabel = "Upload ID",
                         placeholder = "choose file .csv or .xlsx")
               
        ),
        
        column(4,offset = 2,
               checkboxInput("add_cols","Add more column from ID file ?",value = FALSE),
               uiOutput("select")
        )
        
        
    ),
    
    hr(),
    
    h3("Encode"),
    h5("Encode by default : 1 = Finished, 0 = In progress, Empty cell = other record or no record in SELECx"),
    br(),
    checkboxInput("check_enc","Encode manually ?",value = F),
    
    tabsetPanel(
        id = "tab_enc",
        type = "hidden",
        tabPanel("not_show"),
        tabPanel("show",
                 h5("Type number to encode"),
                 br(),
                 uiOutput("enc_disp"))
    ),
    
    br(),
    hr(),
    
    h3("Status"),
    tableOutput("stat"),
    
    hr(),
    h3("Data"),
    br(),
    
    
    
    
    dataTableOutput("table"),
    
    
    numericInput("filter","Filter : Finished_total ≤", value = NULL, min = 0 ),
    dataTableOutput("table_filter"),
    
    hr(),
    h3("Missing name / No record in SELECx"),
    dataTableOutput("missing")
    
    
    
)
