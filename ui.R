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
            
            
            
            verbatimTextOutput("auth_output")
            
    ),
    
    
    tabItem(tabName = "myproject",
            h1("PROJECT"),
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
            h1("DATASET"),
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