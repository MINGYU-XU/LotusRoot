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


#  interaction of datasets and projects
#  related datasets CANNOT shown

#  User permission Settings ??? Editable permissions???

#---------------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(shinymanager)
library(DT)
library(readr)
library(dplyr)
library(tidyr)


## Functions --------------------------------------------------------------------
# print_rows_cols
# print row+column selected
print_rows_cols = function(id) {
  cat('Rows selected:\n')
  print(input[[paste0(id, '_rows_selected')]])
  cat('Columns selected:\n')
  print(input[[paste0(id, '_columns_selected')]])
}



## Read files---------------------------------------------------------------

#read test files
testfile_dataset <- read.csv('csv_test_dataset.csv') #sep = ','
testfile_project <- read.csv('csv_test_project.csv')


#datas<-readRDS("df_data.rds") ### error
datas <- read.csv('df_data.csv')
   #####Error in as.data.frame.default: cannot coerce class ‘c("reactiveVal", "reactive", "function")’ to a data.frame

proj <- read.csv('df_proj.csv')#!!!EDIT

dataVal <- datas

projVal <- proj



## secure credentials info 
if (interactive()) {
  
  # define some credentials
  credentials <- data.frame(
    user = c("test", "123", "z"),
    password = c("test", "123", "z"),
    stringsAsFactors = FALSE
  )}





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
             tabName = "myproject"
        #menuSubItem("Create New Project", 
        #         tabName = "create_new_project", 
        #         icon = icon(name = "plus-circle")),
        #menuSubItem("My Current Projects", 
        #         tabName = "current_project", 
        #         icon = icon(name = "list")),
        #startExpanded = TRUE
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
            h5("Hello!"),
            
            
            ## user_name
            #textInput(inputId = "userName", 
            #          label = "User Name :"),
            
            ## password
            #passwordInput(inputId = "passwd", 
            #              label = "Password :"),
            
            ## email
            #textInput(inputId = "email",
            #          label = "Email :"),
            br(),
            ## log in
            #actionButton("login", " Log in / Register", icon = icon(name = "sign-in-alt"), width = "200px"),
            
            verbatimTextOutput("auth_output")
    ),
 
    
    tabItem(tabName = "myproject",
            h3("Create New Project"),
            fluidRow(
              box(width = 12,status = "primary",collapsible = FALSE,solidHeader = TRUE,
              box(
                
                textInput(inputId = "projName", 
                          label = "New Project Name :"),
                
                textInput(inputId = "projID", 
                          label = "New Project ID :"),
                
                #passwordInput(inputId = "projPW", 
                #              label = "Project Password :"),
                
                textInput(inputId = "projAdministrator",
                          label = "Administrator :"),
                
                textInput('projDescription', 
                          'Description:', placeholder = 'you can descrip the project'),
                
                dateInput('projDate', 'Date:',format = "yyyy/mm/dd",startview = 'month', language = 'en'),
                
                
              ),
                       
              box(
                #fileInput(inputId = "projUploadfile",
                #          label = "Upload files:",
                #          multiple = TRUE,
                #          accept = c('text/csv','text/comma-separated-values','.csv','.tsv')
                #),
                
                textInput('projLocation', 'Location:', placeholder = 'where the project stored'),
                
                textInput('projLab', 'Lab/Research:', placeholder = 'lab/research where the project carried out'),
                
                textInput('projSample', 'Sample ID:', placeholder = 'sample ID'),
                
                selectInput("projStatus", 
                            "Status:",
                            c("Private" = "private",
                              "Publish" = "publish",
                              "Archived" = "archived"), 
                            selected = 'pri'),
                
                actionButton('add_proj', 'Add',style = "color: white; background-color: teal")
                #h5('List of uploaded files:'),
                #verbatimTextOutput('fileList')
              ))),
              
            
            h3("My Projects"),
            h5("Note: You must click the 'Save' button below to confirm the new project added or edited, 
               otherwise the new project will not be saved!"),
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
            h3("Add New Datasets"),
            
            fluidRow(
              box(width = 12,status = "primary",collapsible = FALSE,solidHeader = TRUE,
              box(
                # input datasets information
                textInput('dataID', 'Sample ID:', placeholder = 'sample ID'),
                textInput('dataDescription', 'Description:', placeholder = 'you can descrip the date'),
                dateInput('dataDate', 'Date:',format = "yyyy/mm/dd",startview = 'month', language = 'en'),
                textInput('dataLocation', 'Location:', placeholder = 'where the date stored')
              ),
              
              box(
                selectInput("datatype", 
                            "Data type:",
                            c("Cell" = "cell",
                              "Tissue" = "tissue",
                              "Species" = "species"), 
                            selected = 'cel'),
                
                textInput('dataLab', 'Lab/Research:', placeholder = 'lab/research obtained the data'),
                selectInput("dataStatus", 
                            "Status:",
                            c("Private" = "private",
                              "Publish" = "publish",
                              "Archived" = "archived"), 
                            selected = 'pri'),
                textInput('projectid', 'Project_linked:', placeholder = 'project id'),
                #fileInput('file', 'Choose file:'),
                actionButton('add_data', 'Add',style = "color: white; background-color: teal")
              )
              )
            ),
            
            
            h3("My Datasets"),
            h5("Note: You must click the 'Save' button below to confirm the new dataset added or edited, 
               otherwise the new dataset will not be saved!"),
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
            h3("About Us"),
            h5("LIMS is a modularised web-based laboratory information management system built to centrally track projects and data with standardised metadata, while still maintaining appropriate access and permissions to users and groups."), 
            h5("This system will make our analyses more findable, accessible, interoperable and reproducible based on the FAIR data principles.")
    ),
      
    tabItem(tabName = "FAQ",
            h3("FAQ"),
            h5()
    )
    ))


ui <- dashboardPage(
        header,
        sidebar,
        body
)


#ui <- secure_app(ui, choose_language = TRUE)


#--------------------------------------------------------- ui ------------------




#server ------------------------------------------------------------------------

server <- function(input, output) {
  
  # HOME page
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    
    reactiveValuesToList(res_auth)
  })
  
  
  options(DT.options = list(pageLength = 10)) ## The initial display is 10 rows
  
  # Create new project
  ### Error in rep: attempt to replicate an object of type 'closure' ??????????????????????
  
  observeEvent(input$add_proj,{
    
    p <- rbind(data.frame(ProjectID = input$projID,  #paste0("P",nrow(proj)+1),
                          ProjectName = input$projName, 
                          Description = input$projDescription, 
                          Date = as.character(input$projDate),
                          Location = input$projLocation, 
                          LabResearchers = input$projLab, 
                          Sample.ID = input$projSample,
                          Status = input$projStatus),projVal())
    
    projVal(p)
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
  
  # edit a cell ##############################################
  ###最好是让用户选择一行，点击“编辑”按钮，然后在另一个框中编辑并保存更改。###
  observeEvent(input$edit_proj, {
    proj <<- editData(projVal(), input$x1_cell_edit, 'x1') ## double <
    
  })
  
  
  # Associating two tables ########################################
  
  # Expected function: selecte a proj then the Datasets included in it are displayed  
  # 预期功能：选中一个proj后，显示该项目中包含的datasets
  
  ## if no row(in x1) selected, No table
  ## if select one row, display the datasets included in that proj 如果选择了某含，则显示该proj的datasets
  ## return：'pids' is the project ID
  
  pids<-reactive({
    if(length(input$x1_rows_selected)==0){
      #pids<-projVal(input$x1_rows_all[1],)
      
      return(NULL)  ## No output
    }
    else{
      pids <- projVal()[input$x1_rows_selected,"ProjectID"]
      
    }
    print(pids)
    return(pids)
  })
  
  
  output$related_datasets <- DT::renderDT({
    pid<-pids()
    
    if(length(pid)==0){
      return(NULL)
    }
    
    
    ds<-dataVal() %>% filter(Project.ID  %in% pid) 
    ## Filter the table
    
    dt <- DT::datatable(ds,
                        selection="single",
                        filter="bottom",
                        options = list(SortClasses = TRUE,
                                       LengthMenu = c(1,5,10,20,50), 
                                       pageLength = 10
                                       )
                        )
                    
          
    
    dt <- dt %>% formatStyle('pval',target = 'row')

    dt
  })
  
  
  
  
  
  
  
  
  # output proj table #########################
  
  projVal <- reactiveVal(proj)
  output$x1 <- renderDT(projVal(),
                        server = FALSE,     ## client-side processing 
                        selection = list(target = 'row'),   ## Multiple selection: rows
                        editable = list(target = "cell", disable = list(columns = c(0))), ## cannot edit column1
                        filter = list(position = 'top', clear = FALSE),
                        extensions = c('Buttons'),
                        options = list(
                          dom = 'Blfrtip', 
                          style = 'os', 
                          items = 'row',
                          buttons = c('csv', 'excel', 'pdf', 'print'),
                          searchHighlight = TRUE,
                          search = list(regex = TRUE)
                          #columnDefs = list(list(targets = c(3), searchable = FALSE)) #Disable Searching for Individual Columns
                          )
  )
  
  # print the selected projects
  output$y1 <- renderPrint({
    cat('Projects selected:\n')
    input$x1_rows_selected
    })
  
  
  
  # create new project
  projVal <- reactiveVal(proj)
  output$x0 <- renderDT(projVal(),
                        selection = 'none')
  
  
  
  # Raw Datasets 
  
  # add new row
  dataVal <- reactiveVal(datas)
  
  observeEvent(input$add_data,{

    t <- rbind(data.frame(Sample.ID = input$dataID, 
                          Description = input$dataDescription, 
                          Date = as.character(input$dataDate),
                          Location = input$dataLocation, 
                          Datatype = input$datatype,
                          Lab = input$dataLab, 
                          Status = input$dataStatus, 
                          Project.ID = input$projectid),dataVal())    
    
    dataVal(t)
    })
  
  
  
  # delete row
  observeEvent(input$delete_data, {
    t = dataVal()
    print(nrow(t))
    if (!is.null(input$x2_rows_selected)) {
      t <- t[-as.numeric(input$x2_rows_selected),]
    }
    dataVal(t)
  })
  
  observeEvent(input$delete_proj, {
    r = projVal()
    print(nrow(r))
    if (!is.null(input$x1_rows_selected)) {
      r <- r[-as.numeric(input$x1_rows_selected),]
    }
    projVal(r)
  })
  
  
  
  
  # edit a row ????????????????????
  # select a row, press edit and 
  # a popup window comes up with the data for that row that can be edited and saved.
  observeEvent(input$edit_data, {
    ### Error in split.default: first argument must be a vector ????????????????????????????     
    dataVal()
    options=list(
      editable = list(target = "row", disable = list(columns = c(0)))
    )                     
    
    datas <<- editData(dataVal(), input$x2_row_edit, 'x2') ## double <
  })
  
  
  observeEvent(input$edit_proj, {
    ### Error in split.default: first argument must be a vector ????????????????????????????     
    projVal()
    options=list(
      editable = list(target = "row", disable = list(columns = c(0)))
    )                     
    
    proj <<- editData(projVal(), input$x1_row_edit, 'x1') ## double <
  })
  
  
  
  
  
  
  
  # output dataset table
  output$x2 <- renderDT(
    dataVal(),
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
  
  
  
## SAVE the table into a file, and then load the file
  ## Error in as.data.frame.default: cannot coerce class ‘c("reactiveVal", "reactive", "function")’ to a data.frame
  
  observeEvent(input$save_data,{
    write.csv(dataVal(),'df_data.csv',row.names = FALSE)
    
    #df_data<-read.csv('df_data.csv')
    
    })
  
  observeEvent(input$save_proj,{  
    write.csv(projVal(),'df_proj.csv',row.names = FALSE)     ### ERROR in as.data.frame.default: cannot coerce class ‘c("reactiveVal", "reactive", "function")’ to a data.frame
    
    #df_proj<-read.csv('df_proj.csv')
    
  })
  
  ### Error in <Anonymous>: 'data' must be 2-dimensional (e.g. data frame or matrix) ???
  #observeEvent(input$save_data,{
  #  saveRDS(dataVal,"df_data.rds")
  #  df_data<-readRDS("df_data.rds")
  
  ####saveRDS(df,"df.rds")
  ####df<-readRDS ("df.rds")
  #})
  
}

#----------------------------------------------------- server ------------------



  # Run the application 
  shinyApp(ui, server)
