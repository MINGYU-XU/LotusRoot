# UTF8
# This is my laboratory information management system (LIMS)
# Shiny web application
# 
#--------------------------------------------------------------------
# problems:
#  table only save the last edit 

#  CAN delete and add new row
#  BUT after refresh, it return to the original table

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
                
                textInput(inputId = "projName", 
                          label = "New Project Name :"),
                
                passwordInput(inputId = "projPW", 
                              label = "Project Password :"),
                
                textInput(inputId = "projAdministrator",
                          label = "Administrator :"),
                
                textInput('projDescription', 
                          'Description:', placeholder = 'you can descrip the project'),
                
                dateInput('projDate', 'Date:',format = "yyyy/mm/dd",startview = 'month', language = 'en'),
                
                
              ),
                       
              box(
                fileInput(inputId = "projUploadfile",
                          label = "Upload files:",
                          multiple = TRUE,
                          accept = c('text/csv','text/comma-separated-values','.csv','.tsv')
                ),
                
                textInput('projLocation', 'Location:', placeholder = 'where the project stored'),
                
                textInput('projLab', 'Lab/Research:', placeholder = 'lab/research where the project carried out'),
                
                selectInput("projStatus", 
                            "Status:",
                            c("Private" = "pri",
                              "Publish" = "pub",
                              "Archived" = "arc"), 
                            selected = 'pri'),
                
                actionButton('add_proj', 'Add')
                #h5('List of uploaded files:'),
                #verbatimTextOutput('fileList')
              )
            ),
            
            h3("Projects"),
            h5("This is a read-only table."),
            h5("You can edit project information on 'My Current Projects' page."),
            fluidRow(  
              box(
                width = 12,
                DTOutput(outputId='x0')   ## projects table
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
                textInput('dataDescription', 'Description:', placeholder = 'you can descrip the date'),
                dateInput('dataDate', 'Date:',format = "yyyy/mm/dd",startview = 'month', language = 'en'),
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
  
  # Create new project
  ### Error in rep: attempt to replicate an object of type 'closure'??????????????????????
  
  observeEvent(input$add_proj,{
    
    p <- rbind(data.frame(ProjectID = '666',#读取行数加一 
                          ProjectName = input$projName, 
                          Description = input$projDescription, 
                          Date = as.character(input$projDate),
                          Location = input$projLocation, 
                          Datasets = '666',#???
                          LabResearchers = input$projLab, 
                          ExternalLinks = '666',#???
                          Status = input$projStatus),proj)
    
    proj(p)
  })
  
  # delete row
  #observeEvent(input$delete_data, {
  #  p = proj()
  #  print(nrow(p))
  #  if (!is.null(input$x1_rows_selected)) {
  #    t <- t[-as.numeric(input$x1_rows_selected),]
  #  }
  #  proj(p)
  #})
  
  
  
  #My Current Projects 
  
  # edit a cell
  observeEvent(input$x1_cell_edit, {
    projVal <<- editData(proj(), input$x1_cell_edit, 'x1') ## double <
    
  })
  
  
  proj <- reactiveVal(projVal)
  
  # output proj table
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
                          search = list(regex = TRUE),
                          columnDefs = list(list(targets = c(3), searchable = FALSE)) #Disable Searching for Individual Columns
                          
                          ## ??? no searchPanes
                          #columnDefs = list(list(searchPanes = list(show = FALSE), targets = 1:3))
                          
                          # aLengthMenu = c(1,5,10,20,50) #???
                          
                          )
  )
  
  # print the selected projects
  output$y1 <- renderPrint({
    cat('Projects selected:\n')
    input$x1_rows_selected
    })
  
  
  
  

  # creat new project
  output$x0 <- renderDT(proj(),
                        selection = 'none')
  
  
  
  
  
  
  # Raw Datasets 
  
  
  #output$tableout <- renderDT({
  #  if(input$submit == 0){
  #    return()
  #  }
  
  # add new row
  datas <- reactiveVal(dataVal)
  
  observeEvent(input$add_data,{

    t <- rbind(data.frame(Sample_name = input$dataName, 
                              Description = input$dataDescription, 
                              Date = as.character(input$dataDate),
                              Location = input$dataLocation, 
                              Datatype = input$datatype,
                              Lab = input$dataLab, 
                              Status = input$dataStatus, 
                              Projectlinked = input$projectid
                              
                              ),datas())
    
    datas(t)
    })
  
  # delete row
  observeEvent(input$delete_data, {
    t = datas()
    print(nrow(t))
    if (!is.null(input$x2_rows_selected)) {
      t <- t[-as.numeric(input$x2_rows_selected),]
    }
    datas(t)
  })
  
  
  # edit a cell
  observeEvent(input$x2_cell_edit, {
    dataVal <<- editData(datas(), input$x2_cell_edit, 'x2') ## double <
    
  })
  
  
  # output dataset table
  output$x2 <- renderDT(
    datas(),
    server = FALSE,     ## client-side processing
    
    #selection = 'single',  #selection = 'none',
    selection = list(target = 'row'),   ## Multiple selection: rows
    
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
