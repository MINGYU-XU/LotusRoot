# UTF8
# This is my laboratory information management system (LIMS)
# Shiny web application
# 
#--------------------------------------------------------------------
# problems:
#  table only save the last edit 

#  delete and add new row
#Error in data.frame: arguments imply differing number of rows: 1, 0

#  interaction of datasets and projects

#  how to set log in page    ???ui<-secure_app(ui)

#---------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinymanager)
library(DT)
library(readr)


## Functions --------------------------------------------------------------------

# 1 dt_output
# output table (id:outputId)
dt_output = function(title, id) {
  h1(paste0('Table:', title)) 
  hr()
  DTOutput(id)
}

# 2 render_dt
# select rows and columns
# edit table by cell/row/column/all
render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = list(target = 'row+column'), server = server, editable = editable, ...)
  # selection = list(target = 'row+column')
  # selection = 'none'
}

# 3 print_rows_cols
# print row+column selected
print_rows_cols = function(id) {
  cat('Rows selected:\n')
  print(input[[paste0(id, '_rows_selected')]])
  cat('Columns selected:\n')
  print(input[[paste0(id, '_columns_selected')]])
}

#----------------------------------------------------------functions-------



#read test files
#testfile_dataset <- read_csv('csv_test_dataset.csv') #sep = ','
#testfile_project <- read_csv('csv_test_project.csv')

datas <- testfile_dataset
proj <- testfile_project  #!!!EDIT
dataVal <- datas
projVal <- proj

# data.frame with credentials info ??
credentials <- data.frame(
  user = c("test", "xmy", "zzz", "shiny"),
  password = c("test", "123", "zzz", "shinypw"),
  # comment = c("alsace", "auvergne", "bretagne"), %>% 
  stringsAsFactors = FALSE
)




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
    menuItem("My Project", icon = icon(name="dna"), 
             tabName = "myproject",
        menuSubItem("Creat New Project", 
                 tabName = "creat_new_project", 
                 icon = icon(name = "plus-circle")),
        menuSubItem("My Current Projects", 
                 tabName = "current_project", 
                 icon = icon(name = "list")),
        startExpanded = TRUE
    ),
    
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
            h3("Home"),
            ## user_name
            textInput(inputId = "userName", 
                      label = "User Name :"),
            ## password
            passwordInput(inputId = "passwd", 
                          label = "Password :"),
            ## email
            #textInput(inputId = "email",
            #          label = "Email :"),
            br(),
            ## log in
            actionButton("login",
                         " Log in / Register",
                         icon = icon(name = "sign-in-alt"),
                         width = "200px")
    ),
 
    
    tabItem(tabName = "creat_new_project",
            h3("Creat New Project"),
            fluidRow(
              box(
                ## project_name
                textInput(inputId = "projectName", 
                          label = "New Project Name :"),
                ## project_password
                passwordInput(inputId = "projectPW", 
                              label = "Project Password :"),
                ## administrator
                textInput(inputId = "projectAdministrator",
                          label = "Administrator :")
              ),
                       
              box(
                fileInput(inputId = "projectUploadfile",
                          label = "Upload files:",
                          multiple = TRUE,
                          accept = c('text/csv','text/comma-separated-values','.csv','.tsv')
                ),
                h5('List of uploaded files:'),
                verbatimTextOutput('fileList')
              )
                       
            )
            
            ## log in
            #actionButton("action_creat",'action',
            #             icon = icon(name = "sign-in-alt"),
            #             width = "100px"
            #)
    ),
    
    
    tabItem(tabName = "current_project",
            h3("My Current Projects"),
            hr(),
            DTOutput(outputId='x1'),   ## projects table
            verbatimTextOutput(outputId='y1') ## list the selected rows and columns / list of current projects
    ),
    
    
    tabItem(tabName = "datasets",
            h3("Datasets"),
            
            fluidRow(
              box(
                # input datasets information
                textInput('dataName', 'Sample name:', placeholder = 'sample name'),
                textInput('dataDscription', 'Description:', placeholder = 'you can descrip the date'),
                dateInput('dataDate', 'Date:',format = "yyyy-mm-dd",startview = 'month', language = 'en'),
                textInput('dataLocation', 'Location:', placeholder = 'where the date stored')
              ),
              
              box(
                selectInput("datatype", 
                            "Data type:",
                            c("Cell" = "cel",
                              "Tissue" = "tis",
                              "Species" = "spe"), 
                            selected = 'cel'),
                
                textInput('dataLab', 'Lab/Research:', placeholder = 'lab/research obtained the data'),
                selectInput("dataStatus", 
                            "Status:",
                            c("Private" = "pri",
                              "Publish" = "pub",
                              "Archived" = "arc"), 
                            selected = 'pri'),
                textInput('projectid', 'Project_linked:', placeholder = 'project id')
                #fileInput('file', 'Choose file:'),
              )
            ),
            
            actionButton('add_data', 'Add'),
            h1(),

            fluidRow(
              box(width = 12,
                DTOutput(outputId='x2')  ## the place to output datasets table
              )
            ),
            
            
            actionButton('delete_data', 'Delete'),
            h1()
            
            #verbatimTextOutput(outputId='y2'),  ## the place to output text
            
            ## other input type
            #passwordInput('passwd', 'Your password is: ', placeholder = 'Your password please', width = '100%')
            #sliderInput('num', 'Choose a number: ', min = 0, max = 100, value = 30),
            #radioButtons('gender', 'Gender', c('Male'='m', 'Female'='f','Transgender'='trans'),inline = T),
            #conditionalPanel("input.gender == 'f'",
            #radioButtons('gender1', 'Gender', c('Male'='m', 'Female'='f','Transgender'='trans'),inline = T)),
    ),
    
    tabItem(tabName = "aboutus",
            h3("About Us")
    ),
      
    tabItem(tabName = "FAQ",
            h3("FAQ")
    )
    ))


ui <- dashboardPage(
        header,
        sidebar,
        body
)

#ui <- secure_app(ui)
#--------------------------------------------------------- ui ------------------


#Logged = FALSE;
#my_username <- "test"
#my_password <- "test"


#server ------------------------------------------------------------------------

server <- function(input, output) {
  
  options(DT.options = list(pageLength = 10)) ## The initial display is 5 rows
  
  #My Current Projects 
  
  # edit a cell
  observeEvent(input$x1_cell_edit, {
    projVal <<- editData(proj(), input$x1_cell_edit, 'x1') ## double <
    
  })
  
  proj <- reactiveVal(projVal)
  output$x1 <- renderDT(proj(),
                        server = FALSE,     ## client-side processing 
                        #selection = 'none',
                        selection = list(target = 'row'),   ## Multiple selection: rows
                        editable = list(target = "cell", disable = list(columns = c(0))), ## cannot edit column1
                        # search options
                        filter = list(position = 'top', clear = FALSE),
                        
                        extensions = c('Buttons'),
                        ## 'SearchPanes':No SearchPanes: it needs server = FALSE
                        ## 'Select'
                        options = list(
                          dom = 'Blfrtip', #dom = 'PBlfrtip',
                          style = 'os', items = 'row',
                          buttons = c(#'selectAll', 'selectNone',
                                      'csv', 'excel', 'pdf', 'print'),   #'selectRows', 'selectColumns', 'selectCells','copy',
                          searchHighlight = TRUE,
                          search = list(regex = TRUE)
                          #columnDefs = list(list(targets = c(1), searchable = FALSE)) #Disable Searching for Individual Columns禁用搜索第一列
                          
                          ## ??? no searchPanes
                          #columnDefs = list(list(searchPanes = list(show = FALSE), targets = 1:3))
                          )
  )
              
  
  # print the selected projects
  output$y1 <- renderPrint({
    cat('Projects selected:\n')
    input$x1_rows_selected
    })
  


  
  
  # Raw Datasets 
  
  
  #output$tableout <- renderDT({
  #  if(input$submit == 0){
  #    return()
  #  }
  
  # add new row
  observeEvent(input$add_data,{
    #newrow <- data.frame(Sample_Name = input$name, 
    #                     Status = input$status, 
    #                     Date = as.character(input$date),
    #                     Description = input$description, 
    #                     Location = input$location, 
    #                     Datatype = input$datatype,
    #                     Lab = input$lab, 
    #                     Projectlinked = input$projectid,
    #                     stringsAsFactors = F)
    
    
    #Error in data.frame: arguments imply differing number of rows: 1, 0
    
    t <- rbind(data.frame(Sample_Name = input$dataName, 
                              Description = input$dataDescription, 
                              Date = as.character(input$dataDate),
                              Location = input$dataLocation, 
                              Datatype = input$datatype,
                              Lab = input$dataLab, 
                              Status = input$dataStatus, 
                              Projectlinked = input$projectid
                              
                              ),datas)
    
    datas <<- t
    })
  
  # delete row
  observeEvent(input$delete_data, {
    t = datas
    print(nrow(t))
    if (!is.null(input$x2_rows_selected)) {
      t <- t[-as.numeric(input$x2_rows_selected),]
    }
    datas <<- t
  })
  
  
  # edit a cell
  observeEvent(input$x2_cell_edit, {
    dataVal <<- editData(datas(), input$x2_cell_edit, 'x2') ## double <
    
  })
  
  
  # output dataset table
  datas <- reactiveVal(dataVal)
  
  output$x2 <- renderDT(
    datas(),
    server = FALSE,     ## client-side processing
    
    #selection = 'single',  #selection = 'none',
    selection = list(target = 'row+column'),   ## Multiple selection: rows and columns
    
    #editable = 'cell', 
    editable = list(target = "cell", disable = list(columns = c(0))), ## cannot edit column1
    
    # search options
    filter = list(position = 'top', clear = FALSE),
    
    extensions = c('Buttons'),
    options = list(
      
      dom = 'Blfrtip',  ##dom = 'PBlfrtip',
      style = 'os', items = 'row',
      
      #buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      buttons = c(#'selectAll', 'selectNone',
        'csv', 'excel', 'pdf', 'print'),   #'selectRows', 'selectColumns', 'selectCells',
      
      searchHighlight = TRUE,
      
      search = list(regex = TRUE)
      #columnDefs = list(list(targets = c(1), searchable = FALSE))  
      #Disable Searching for Individual Columns
      
      
      ## ??? no searchPanes
      #columnDefs = list(list(searchPanes = list(show = FALSE), targets = 1:3))
    )         
    
  )
              
}

#----------------------------------------------------- server ------------------



  # Run the application 
  shinyApp(ui, server)
