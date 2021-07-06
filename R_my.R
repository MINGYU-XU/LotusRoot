#
# This is my laboratory information management system (LIMS)
# Shiny web application
# 
#--------------------------------------------------------------------
# problems:
#  table only save the last edit 

#  CAN delete and add new dataset row
#  BUT after refresh, it return to the original table

#  Error in add project row

#  interaction of datasets and projects
#  related datasets CANNOT shown

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
#testfile_dataset <- read.csv('csv_test_dataset.csv') #sep = ','
#testfile_project <- read.csv('csv_test_project.csv')


#datas<-readRDS("df_data.rds") ### error
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
            verbatimTextOutput(outputId='y1'), ## list the selected rows and columns / list of current projects
            h3("Related Datasets"),
            DTOutput(outputId='related_datasets')
    ),
    
    
    tabItem(tabName = "datasets",
            h3("Datasets"),
            
            fluidRow(
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
                textInput('projectid', 'Project_linked:', placeholder = 'project id'),
                #fileInput('file', 'Choose file:'),
                actionButton('add_data', 'Add')
              )
            ),
            h1(),
            actionButton('delete_data', 'Delete'),
            actionButton('save_data','Save'),
            

            fluidRow(
              
              box(width = 12,
                DTOutput(outputId='x2')  ## the place to output datasets table
              )
            ),
            
            
            
            
            #verbatimTextOutput(outputId='y2'),  ## the place to output text
            h3("Related Projects"),
            fluidRow(
              
              box(width = 12,
                  DTOutput(outputId='related_proj')  ## the place to output datasets table
              )
            )
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
                          Status = input$projStatus),projVal)
    
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
  observeEvent(input$x1_cell_edit, {
    projVal <<- editData(proj(), input$x1_cell_edit, 'x1') ## double <
    
  })
  
  
  # Associating two tables ########################################
  
  # Expected function: selecte a proj then the Datasets included in it are displayed  
  # 预期功能：选中一个proj后，显示该项目中包含的datasets
  
  
  ## if no row(in x1) selected, display all datasets 如果未选中任何一行，则显示所有的datasets（x2）
  ## if select one row, display the datasets included in that proj 如果选择了某含，则显示该proj的datasets
  ## return：pids is the project ID
  
  pids<-reactive({
    if(length(input$x1_rows_selected)==0){
      pids<-projVal[input$x1_rows_all[1],]
    }
    else{
      pids<-projVal[input$x1_rows_selected[1],]
      #pids<-proj[1,]
    }
    return(pids)
  })
  
  ## Project.ID列 $ProjectID 给dsub
  ## return：dsub is the Associated datas
  rd <- reactive({
    pid<-pids()  ## selected proj row
    dsub<-subset(dataVal,dataVal$Project.ID == unique(pid$ProjectID))   ## Associated by project.id
    return(dsub)
  })
  
  
  ### Error: object of type 'closure' is not subsettable ??????????
  
  
  output$related_datasets <- DT::renderDT({
    dsub <- rd()
    #if(dim(dsub)[1]==0){
    #  return(NULL)
    #}
    dt <- DT::datatable(dsub,
                        selection="single")
                        #filter="bottom",
                        #options = list(#bSortClasses = TRUE,
                        #               #aLengthMenu = c(1,5,10,20,50), ##???
                        #               pageLength = 50
                    
          
    
    #dt <- dt %>% formatStyle('pval',
    #                         target = 'row',
    #                         backgroundColor = styleInterval(c(0.05), c(cols[1], cols[2]))
    #      )
    dt
  })
  
  
  
  
  
  
  
  
  # output proj table #########################
  
  projVal <- reactiveVal(proj)
  
  output$x1 <- renderDT(projVal(),
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
  output$x0 <- renderDT(projVal(),
                        selection = 'none')
  
  
  
  
  
  
  # Raw Datasets 
  
  
  #output$tableout <- renderDT({
  #  if(input$submit == 0){
  #    return()
  #  }
  
  
  
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
  
  
  # edit a cell
  observeEvent(input$x2_cell_edit, {
    datas <<- editData(dataVal(), input$x2_cell_edit, 'x2') ## double <
    
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
