#
# This is my laboratory information management system (LIMS)
# Shiny web application --- ui
# 
#--------------------------------------------------------------------
# bugs:
# 1 not list the selected rows and columns
# 2 how to set log in page    ???ui<-secure_app(ui)
# 3 table not saved after edit
# 4 how to add "pick" radio button as the first column   ???server=T/F
# 5 design the information table ??? cannot show up 
#---------------------------------------------------------------------


library(shinydashboard)
library(shiny)
library(tidyverse)
library(shinymanager)
library(DT)


## Functions --------------------------------------------------------------------

# 1 dt_output
# output table (id:outputId)
dt_output = function(title, id) {
  h1(paste0('Table:', title)) #我们本来期望的是提取两个“(1st)”和“(2nd)”组合，不料整个地提取了“(1st) other (2nd)”。这就是因为.+的贪婪匹配。如果要求尽可能短的匹配， 使用*?, +?, {3,}?等“懒惰型”重复模式。 在无上限重复标志后面加问号表示懒惰性重复。
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

#----------------------------------------------------------------functions-------



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
  
  tabItems(
    tabItem(tabName = "home",
            h3("Home"),
            ## user_name
            textInput(inputId = "user_name", 
                      label = "User Name :"),
            ## password
            passwordInput(inputId = "password", 
                          label = "Password :"),
            ## email
            textInput(inputId = "email",
                      label = "Email :"),
            ## log in
            submitButton(text = " Log in / Register",
                         icon = icon(name = "sign-in-alt"),
                         width = "200px")
    ),
    
    tabItem(tabName = "creat_new_project",
            h3("Creat New Project"),
            ## project_name
            textInput(inputId = "project_name", 
                      label = "New Project Name :"),
            ## project_password
            passwordInput(inputId = "project_password", 
                          label = "Project Password :"),
            ## administrator
            textInput(inputId = "administrator",
                      label = "Administrator :"),
            fileInput(inputId = "uploadfile",
                      label = "Upload files:",
                      multiple = TRUE,
                      accept = c('text/csv','text/comma-separated-values','.csv','.tsv')
                      ),
            h5('List of uploaded files:'),
            verbatimTextOutput('fileList'),
    
            ## log in
            submitButton(text = " Submit ",
                         icon = icon(name = "sign-in-alt"),
                         width = "100px"
            )
    ),
    
    
    tabItem(tabName = "current_project",
            h3("My Current Projects"),
            hr(),
            DTOutput(outputId='x1'),   ## projects table
            #div(DTOutput("x1"), style ="font-size:75%"), ## change font size
          
          # dt_output('server-side processing (editable = "cell")', 'x5'),
          # dt_output('server-side processing (editable = "column")', 'x7'),
          # dt_output('server-side processing (editable = "all")', 'x8'),
          #  dt_output('server-side processing (editable = "row")', 'x1'),
 
            verbatimTextOutput(outputId='y1') ## list the selected rows and columns / list of current projects
          
    ),
    
    
    tabItem(tabName = "datasets",
            h3("Datasets"),
            
            # input datasets information
            textInput('name', 'Sample name:', placeholder = 'sample name'),
            textInput('description', 'Description:', placeholder = 'you can descrip the date'),
            dateInput('date', 'Date:',format = "yyyy-mm-dd",startview = 'month', language = 'en'),
            textInput('location', 'Location:', placeholder = 'where the date stored'),
            selectInput("datatype", 
                        "Data type:",
                        c("Cell" = "cel",
                          "Tissue" = "tis",
                          "Species" = "spe"), 
                        selected = 'cel'),
            
            textInput('lab', 'Lab/Research:', placeholder = 'lab/research obtained the data'),
            selectInput("status", 
                        "Status:",
                        c("Private" = "pri",
                          "Publish" = "pub",
                          "Archived" = "arc"), 
                        selected = 'pri'),
            
            fileInput('file', 'Choose file:'),
            
    
            actionButton('submit', 'Submit Now'),
    
            DTOutput(outputId='tableout'),
            br(),
    
            DTOutput(outputId='x2'),  ## the place to output datasets table
            verbatimTextOutput(outputId='y2'),  ## the place to output text
            
            
            
            ## other input type
            #passwordInput('passwd', 'Your password is: ', placeholder = 'Your password please', width = '100%')
            #sliderInput('num', 'Choose a number: ', min = 0, max = 100, value = 30),
            #radioButtons('gender', 'Gender', c('Male'='m', 'Female'='f','Transgender'='trans'),inline = T),
            #conditionalPanel("input.gender == 'f'",
            #radioButtons('gender1', 'Gender', c('Male'='m', 'Female'='f','Transgender'='trans'),inline = T)),
    ),
    
    tabItem(tabName = "aboutus",
            h3("About Us"),
    ),
      
    tabItem(tabName = "FAQ",
            h3("FAQ"),
    )
  )
)

ui <- dashboardPage(
        header,
        sidebar,
        body
)
#--------------------------------------------------------- ui ------------------





#server ------------------------------------------------------------------------
server <- function(input, output) {
  options(DT.options = list(pageLength = 5)) ## The initial display is 5 rows

  
#My Current Projects 
  #df=projects
  output$x1 <- renderDT(projects,  ## data frame
                        selection = list(target = 'row+column'), ## Multiple selection: rows and columns
                        server = TRUE,     ## Server-side processing 
                        #editable = 'row',  ## can edit a whole row
                        editable = list(target = "row", disable = list(columns = c(0))) ## cannot edit column1
                        )
  
  #or use a function
  #output$x1 <- render_dt(d1, list(target = 'row', disable = list(columns = c(0))))
  
  # print the selected indices ??????????
  print_rows_cols = function(id) {
    cat('Rows selected:\n')
    print(input[[paste0(id, '_rows_selected')]])     ##?????NULL
    cat('Columns selected:\n')
    print(input[[paste0(id, '_columns_selected')]])  ##????NULL
  }
  output$y1 <- renderPrint(print_rows_cols('x1'))
  
  
  # edit a row
  #observeEvent(input$x1_cell_edit, {
  #  df <- editData(df, input$x1_cell_edit, 'x1')
  #})
  # server-side processing
  # output$x1 <- renderDT(iris, selection = list(target = 'row+column'))
  
 
  
  
# Raw Datasets 
  #df=datasets
  
  output$tableout <- renderDT({
    if(input$submit == 0){
      return()
    }
    
    isolate(
        data.frame(
        Sample_Name = input$name,
        Description = input$description,
        Date = as.character(input$date),
        Location = input$location,
        Data_Type = input$datatype,
        Lab_Research = input$lab,
        Status = input$status, 
        Files = as.character(input$file$name),
        stringsAsFactors = F)
        #'Value' = c(input$name, input$description, as.character(input$date), input$location,  input$datatype, input$lab, input$status, as.character(input$file$name)),
        )
    })

  
  
  output$x2 <- renderDT(datasets,  ## data frame
                        selection = list(target = 'row+column'), ## Multiple selection: rows and columns
                        server = TRUE,     ## Server-side processing 
                        #editable = 'row',  ## can edit a whole row
                        editable = list(target = "row", disable = list(columns = c(0))) ## cannot edit column1
  )
  #or use a function
  #output$x1 <- render_dt(d1, list(target = 'row', disable = list(columns = c(0))))
  
  # print the selected indices ??????????
  #print_rows_cols = function(id) {
  #  cat('Rows selected:\n')
  #  print(input[[paste0(id, '_rows_selected')]])     ##?????NULL
  #  cat('Columns selected:\n')
  #  print(input[[paste0(id, '_columns_selected')]])  ##????NULL
  #}
  #output$y2 <- renderPrint(print_rows_cols('x2'))
  
  
  
}

#----------------------------------------------------- server ------------------

# Run the application 
shinyApp(ui, server)
