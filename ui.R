#
# This is my laboratory information management system (LIMS)
# Shiny web application
# 
#--------------------------------------------------------------------
# problems:
#  table only save the last edit 

#  CAN delete and add new dataset row
#  BUT after refresh, it return to the original table

#  try to save as .rds but ERROR


#  User permission Settings ??? Editable permissions???

#---------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(shinyjs)
library(tidyverse)
library(shinymanager)
library(DT)
library(readr)
library(dplyr)
library(tidyr)


# ui
# dashboard_header -------------------------------------------------------------
header <- dashboardHeader(
  #titleWidth = '15%',
  title="My LIMS",
  dropdownMenu(
    type="messages",
    #add Items
    messageItem(
      # message source
      from="system",
      # message content
      message="process finished",
      # icon for the message
      icon=icon(name="envelope")
    )
  )
)



# dashboard_sidebar ------------------------------------------------------------
sidebar <- dashboardSidebar(
  #width = '15%',
  sidebarMenu(
    menuItem("Home", tabName = "home", 
             icon = icon(name="home")),
    menuItem("My Project",tabName = "myproject",
             icon = icon(name="dna")),
    menuItem("Raw Datasets", tabName = "datasets", 
             icon = icon(name="database")),
    menuItem("About Us", tabName = "aboutus", 
             icon = icon(name="user-friends")),
    menuItem("FAQ", tabName = "FAQ", 
             icon = icon(name="question-circle"))
  )
)

# dashboard_body ---------------------------------------------------------------
body <- dashboardBody(
  
  
  # zoom the web
  tags$style("
    body {
    -moz-transform: scale(0.8, 0.8); /* Moz-browsers */
    zoom: 0.9; /* Other non-webkit browsers */
    zoom: 90%; /* Webkit browsers */

    }
    
    "),
  
  
  
  
  
  
  
  # ui- Home- Log in page  
  tabItems(
    tabItem(tabName = "home",
            h1("HOME"),
            h3("Welcome to My LIMS!"),
            
            
            ## log in
            box(width = 5,
            h4("Log In"),
            textInput(inputId = "loginName", 
                      label = "User Name :"),
            passwordInput(inputId = "loginpw", 
                          label = "Password :"),
            br(),
            actionButton("login", " Log in", icon = icon(name = "sign-in-alt"), width = "200px")
            ),
            
            ## register
            box(width = 7,
            h4("New user? Register here!"),
            useShinyjs(),  # Include shinyjs
            actionButton("register", "Register", icon = icon(name = "sign-in-alt"), width = "200px"),
            # cilck the 'register' button then the following blanks appear, otherwise hidden
            uiOutput("ui_register")
            ),
            
            verbatimTextOutput("successfully_registered"),
            
            verbatimTextOutput("auth_output")
            
    ),
    
    
    
    
    
    
    
    
    
    
    
    tabItem(tabName = "myproject",
            h1("PROJECT"),
            h3("Create New Project"),
            fluidRow(
              box(width = 12,status = "primary",collapsible = FALSE,solidHeader = TRUE,
                  box(
                    
                    textInput(inputId = "projName", 
                              label = "Project Name:",placeholder = 'Project Name'),
                    
                    textInput(inputId = "projID", 
                              label = "Project ID:"),
                    
                    passwordInput(inputId = "projParent", 
                                  label = "Parent Project:"),
                    
                    textInput('projDescription', 
                              'Description:', placeholder = 'You can descrip your project'),
                    
                    dateInput('projDate', 'Start Date:',format = "dd/mm/yyyy",startview = 'month', language = 'en'),
                    
                    dateInput('projDate2', 'End Date:',format = "dd/mm/yyyy",startview = 'month', language = 'en'),
                    
                    textInput('projPath', 'Path:', placeholder = 'Where the project stored in the server'),
                    
                    textInput('projSample', 'Sample ID:', placeholder = 'Sample ID'),
                    
                    
                  ),
                  
                  box(
                    textInput('projReport', 'Report:'),
                    #fileInput(inputId = "projReport",
                    #          label = "Report:",
                    #          multiple = TRUE
                    #          accept = c('text/csv','text/comma-separated-values','.csv','.tsv')
                    #),
                    #textInput(inputId = "projAdministrator",
                    #          label = "Administrator:"),
                    
                    
                    #textInput('projResearcher', 'Researcher:', placeholder = 'Researcher'),
                    selectInput("projResearcher", 
                                "Researcher:",
                                c("JCW" = "JCW",
                                  "SL" = "SL",
                                  "Other" = "Other"), 
                                selected = 'JCW'),
                    textInput('projBioinformatician', 'Bioinformatician:', placeholder = 'Bioinformatician'),
                    
                    selectInput('projGroup', 'Group:',
                                c("Bird" = "Bird",
                                  "Hill" = "Hill",
                                  "Wind" = "Wind",
                                  "Other" = "Other"), 
                                selected = 'Bird'
                                ),
                    
                    
                    textInput('projdataRepository', 'Data Repository:', placeholder = 'Data Repository GEO'),
                    
                    textInput('codeRepository', 'Code Repository:', placeholder = 'Code Repository github URL'),
                    
                    selectInput("projStatus", 
                                "Status:",
                                c("Complete" = "Complete",
                                  "Published" = "Published",
                                  "Ongoing" = "Ongoing"), 
                                selected = 'Ongoing'),
                    
                    selectInput("projPermissions", 
                                "Permissions:",
                                c("Group" = "Group",
                                  "Individual" = "Individual",
                                  "Open" = "Open"), 
                                selected = 'Group'),
                    
                    actionButton('add_proj', 'Add',style = "color: white; background-color: teal")
                    #h5('List of uploaded files:'),
                    #verbatimTextOutput('fileList')
                  ))),
            
            
            h3("My Projects"),
            h5(),
            fluidRow(
              box(width = 12,
                  DTOutput(outputId='x1'),   ## projects table
                  
                  #actionButton('save_proj','Save',style = "color: white; background-color: green"),
                  verbatimTextOutput(outputId='y1'), ## list the selected rows and columns / list of current projects
                  
                  actionButton('edit_proj', 'Edit',style = "color: white; background-color: teal"),
                  actionButton('delete_proj', 'Delete',style = "color: white; background-color: red"),
                  actionButton('save_proj','Save',style = "color: white; background-color: green"),
                  h5(),
              )),
            h3("Related Datasets"),
            DTOutput(outputId='related_datasets')
            
    ),
    
    #h3("Projects"),
    #h5("This is a read-only table."),
    #h5("You can edit project information on 'My Current Projects' page."),
    #fluidRow(  
    #  box(
    #    width = 12,status = "primary",collapsible = FALSE,solidHeader = TRUE,
    #    DTOutput(outputId='x0')   ## projects table
    #  )
    #)
    
    ## log in
    #actionButton("action_create",'action',
    #             icon = icon(name = "sign-in-alt"),
    #             width = "100px"
    #)
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    tabItem(tabName = "datasets",
            h1("DATASET"),
            h3("Add New Datasets"),
            
            fluidRow(
              box(width = 12,status = "primary",collapsible = FALSE,solidHeader = TRUE,
                  box(
                    # input datasets information
                    textInput('dataID', 'Data ID:', placeholder = 'Data ID'),
                    textInput('dataprojID', 'Project ID:', placeholder = 'Project IDs'),
                    textInput('sampleName', 'Sample Name:', placeholder = 'Sample Name'),
                    textInput('dataDescription', 
                              'Description:', placeholder = 'You can descrip your data'),
                    dateInput('dataDate', 'Date:',format = "dd/mm/yyyy",startview = 'month', language = 'en'),
                    textInput('dataPath', 'Path:', placeholder = 'where the date stored'),
                    textInput('dataRepository', 'Data Repository:', placeholder = 'Data Repository GEO'),
                  ),
                  
                  box(
                    selectInput("method", "Method:",
                                c("ChIP-seq" = "ChIP-seq",
                                  "BS-seq" = "BS-seq"), 
                                selected = 'ChIP-seq'),
                    
                    
                    selectInput("organism", "Organism:",
                                c("Human" = "Human",
                                  "Mouse" = "Mouse",
                                  "Other" = "Other"), 
                                selected = 'Human'),
                    
                    selectInput("cell", "Tissue/Cell:",
                                c("Brain" = "Brain",
                                  "Neuron" = "Neuron",
                                  "Liver" = "Liver",
                                  "Kidney" = "Kidney",
                                  "Lung" = "Lung",
                                  "Heart" = "Heart",
                                  "Other" = "Other"), 
                                selected = 'Brain'),
                    
                    
                    selectInput("genotype", "Genotype:",
                                c("WT(wildtype)" = "WT(wildtype)",
                                  "KO(knock-out)" = "KO(knock-out)",
                                  "TG(transgenic)" = "TG(transgenic)",
                                  "Other" = "Other"), 
                                selected = 'WT(wildtype)'),
                    
                    selectInput("format", "Format:",
                                c("fastq" = "fastq",
                                  "fastq.gz" = "fastq.gz",
                                  "BAM" = "BAM",
                                  "Other" = "Other"), 
                                selected = 'fastq.gz'),
                    
                    
                    textInput('treatment', 'Treatment:'),
                    
                    actionButton('add_data', 'Add',style = "color: white; background-color: teal")
                  )
              )
            ),
            
            
            h3("My Datasets"),
            h5(),
            fluidRow(
              
              box(width = 12,
                  DTOutput(outputId='x2'),  ## the place to output datasets table
                  
                  
                  actionButton('edit_data', 'Edit',style = "color: white; background-color: teal"),
                  actionButton('delete_data', 'Delete',style = "color: white; background-color: red"),
                  actionButton('save_data','Save',style = "color: white; background-color: green")
                  
                  
                  #verbatimTextOutput(outputId='y2'),  ## the place to output text
              )
            ),
            
            h3("Related Projects"),
            fluidRow(
              
              box(width = 12,
                  DTOutput(outputId='related_proj')  ## the place to output datasets table
              )
            )
    ),
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    tabItem(tabName = "aboutus",
            h1("About Us"),
            h5("LIMS is a modularised web-based laboratory information management system built to centrally track projects and data with standardised metadata, while still maintaining appropriate access and permissions to users and groups."), 
            h5("This system will make our analyses more findable, accessible, interoperable and reproducible based on the FAIR data principles.")
    ),
    
    tabItem(tabName = "FAQ",
            h1("FAQ"),
            h5()
    )
  ))


ui <- dashboardPage(
  header,
  sidebar,
  body
)


#ui <- secure_app(ui, choose_language = TRUE)