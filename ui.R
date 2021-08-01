#
# This is my laboratory information management system (LIMS)
# Shiny web application
# 
#--------------------------------------------------------------------
# problems:

#  !/Save to group table then general table

#  !/mysql? try to save as .rds but ERROR

#   Edit tables by pop-up window or fixed blanks

#   A button back to initial state
#   Help page


#PROBLEM
# real time login???
# check if it is not unique -> hide button ??


#  !/User permissions
#   Editable permissions???


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
library(shinyFeedback)
library(data.table) ## fwrite: fast

# ui
# dashboard_header -------------------------------------------------------------
header <- dashboardHeader(
  #titleWidth = '15%',
  #title="LotusRoot",
  title = tags$img(src = "logo-2.png", height = 60)
  
)



# dashboard_sidebar ------------------------------------------------------------
sidebar <- dashboardSidebar(
  #width = '15%',
  sidebarMenu(
    menuItem("Home", tabName = "home", 
             icon = icon(name="home")),
    menuItem("Projects",tabName = "myproject", icon = icon(name="dna"),
             menuSubItem("Create New Project", 
                      tabName = "create_new_project", 
                      icon = icon(name = "plus-circle")),
             menuSubItem("Projects", 
                      tabName = "current_project", 
                      icon = icon(name = "list")),
             startExpanded = F),
    menuItem("Datasets", tabName = "datasets",icon = icon(name="database"),
             menuSubItem("Create New Dataset", 
                         tabName = "create_new_dataset", 
                         icon = icon(name = "plus-circle")),
             menuSubItem("Datasets", 
                         tabName = "current_dataset", 
                         icon = icon(name = "list")),
             startExpanded = F),
    
    menuItem("About Us", tabName = "aboutus", 
             icon = icon(name="user-friends")),
    menuItem("FAQ", tabName = "FAQ", 
             icon = icon(name="question-circle")),
    menuItemOutput("admin_item")
    
    #shinyjs::hidden(
    #  tags$div(
    #    id = 'admin_item',
    #    style="display:inline-block;text-align: center;margin-left: 8%;align = center",
    #    #div(style="text-align: left;",
    #        #padding-bottom: 10px;display:inline-block;
    #    menuItem("Administrator", tabName = "admin",icon = icon(name="user-cog"),
    #             #div(style="display:inline-block;text-align: left;",
    #             menuSubItem("User Information",
    #                         tabName = "admin_user", 
    #                         icon = icon(name = "id-card")),
    #             menuSubItem("Project Options", 
    #                         tabName = "admin_p", 
    #                         icon = icon(name = "search")),
    #             menuSubItem("Dataset Options", 
    #                         tabName = "admin_d", 
    #                         icon = icon(name = "building")),
    #             startExpanded = F)
                 #)
        #)
    #  )
    #)

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
            h3("HOME"),
            
            # hide the welcome message at the first place
            shinyjs::hidden(tags$div(
              id = 'tab_login.welcome_div',
              class = 'login-text', 
              textOutput('tab_login.welcome_text', container = tags$h3),
              textOutput('tab_login.permissions_text', container = tags$h5),
              h5('Please contact your administrator if you want to change your permissions.')
              )
            ),
            br(),
            h5("Click here to get LotusRoot manual:"),
            uiOutput("home_manual"),
            h5("If you have any problems about using this App, please turn to the FAQ page."),
            h5("If the FAQ page does not solve your problem, please contact s2045156@ed.ac.uk. We are very happy to help you!"),
            #verbatimTextOutput("auth_output")
            br(),
            h5("Related Links:"),
            uiOutput("links"),
            br(),
            fluidRow(
              column(5),
              column(2,tags$img(src = "logo.png", height = 100)),
              column(5)
            )
    ),
    
    
    
    tabItem(tabName = "create_new_project",
            h3("PROJECT"),
            
            fluidRow(
              #useShinyFeedback(),
              box(width = 12,status = "primary",collapsible = FALSE,solidHeader = TRUE,
                  h3("Create New Project"),
                  box(
                    # automatically generated Proj ID
                    h5("Project ID generated by the system:"),
                    verbatimTextOutput("proj_id"), ## not display id??
                    textInput("projName","Project Name:",placeholder = 'Project Name (eg. BioCenter_Lucy_2021)'),
                    #textInput("projID","Project ID:"),
                    #textInput("projParent","Parent Project(optional):"),
                    #selectInput("projParent","Parent Project(optional):")
                    uiOutput("proj_parent"),
                    textInput('projDescription', 'Description:', placeholder = 'Brief description of your project'),
                    dateInput('projDate', 'Start Date:',format = "dd/mm/yyyy",startview = 'month', language = 'en'),
                    #dateInput('projDate2', 'End Date:',format = "dd/mm/yyyy",startview = 'month', language = 'en'),
                    textInput('projPath', 'Path(optional):', placeholder = 'Where the project stored on the server/URL'),
                    textInput('projSampleSheet', 'Sample Sheet(optional):', placeholder = 'Path/URL where the sample sheet stored'),
                    textInput('projReport', 'Report(optional):',placeholder = 'Path/URL where the project report stored')
                  ),
                  box(
                    uiOutput("proj_researcher"),
                    uiOutput("proj_bioinfo"),
                    uiOutput("proj_group"),
                    textInput('projdataRepository', 'Data Repository:', placeholder = 'Web address/GEO number(eg. GEO10000)'),
                    textInput('codeRepository', 'Code Repository:', placeholder = 'Where your code stored (eg.GitHub URL / path'),
                    selectInput("projStatus", "Status:",
                                c("Complete" = "Complete",
                                  "On Hold"="On Hold",              ## different colours:blue,gray,green,yellow
                                  "Published" = "Published",
                                  "Ongoing" = "Ongoing"), selected = 'Ongoing'),
                    
                    selectInput("projPermissions", "Permissions:",
                                c("Group" = "Group",
                                  "Individual" = "Individual",
                                  "Open" = "Open"), selected = 'Group'),
                    actionButton('add_proj', 'Add',
                                 style = "color: white; background-color: teal"),
                    verbatimTextOutput("new_proj_added")
                    )
                  )
              )),
    
    tabItem(tabName = "current_project",
            h3("PROJECT"),
            fluidRow(
              #useShinyFeedback(),
              box(width = 12,status = "primary",collapsible = FALSE,solidHeader = TRUE,
                  h3("Project Table"),
                  h5(),
                  DTOutput(outputId='x1'),   ## projects table
                  #verbatimTextOutput(outputId='y1'), ## list the selected rows and columns / list of current projects
                  actionButton('edit_proj', 'Edit',style = "color: white; background-color: teal"),
                  actionButton('delete_proj', 'Delete',style = "color: white; background-color: red"),
                  actionButton('clear_selected_proj', 'Clear selected',style = "color: white; background-color: green"),
                  h5(),
                  )),
            fluidRow(
              box(width = 12,status = "primary",collapsible = FALSE,solidHeader = TRUE,
                  h3("Related Datasets"),
                  DTOutput(outputId='related_datasets')
              )),
            fluidRow(
              box(width = 12,status = "primary",collapsible = FALSE,solidHeader = TRUE,
                  h3("Related Projects"),
                  DTOutput(outputId='rp'),
                  #actionButton('show_data', 'Show datasets',
                  #             style = "color: white; background-color: green"),
                  h5("Select one sub-project here then 
                  you will see the related datasets of this project 
                  in the below 'Datasets for One Project' table.")
              )),
            fluidRow(
              box(width = 12,status = "primary",collapsible = FALSE,solidHeader = TRUE,
                  h3("Datasets for One Project"),
                  DTOutput(outputId='one_proj_datasets')
              ))
            
            ),
            
            
            

    tabItem(tabName = "create_new_dataset",
            h3("DATASET"),
            
            fluidRow(
              box(width = 12,status = "primary",collapsible = FALSE,solidHeader = TRUE,
                  h3("Add New Dataset"),
                  box(
                    h5("Data ID generated by the system:"),
                    verbatimTextOutput(outputId='data_id'),
                    uiOutput("related_project_name"),
                    textInput('dataName', 'Dataset Name:', placeholder = 'eg. mCG_BS_WT1, mCG_BS_WT1'),
                    textInput('dataDescription', 
                              'Description:', placeholder = 'Brief description of your dataset'),
                    dateInput('dataDate', 'Date:',format = "dd/mm/yyyy",startview = 'month', language = 'en'),
                    textInput('dataPath', 'Path(optional):', placeholder = 'Where the data stored on the server/URL'),
                    textInput('dataRepository', 'Data Repository:', placeholder = 'Web address/GEO number(eg. GEO10000)'),
                  ),
                  box(
                    uiOutput("data_method"),
                    uiOutput("data_organism"),
                    uiOutput("data_cell"),
                    textInput("genotype", "Genotype:",placeholder = 'eg. wildtype, knock-out...'),
                    uiOutput("data_format"),
                    actionButton('add_data', 'Add',
                                 style = "color: white; background-color: teal"),
                    verbatimTextOutput("new_data_added")
                  )
              )
            )),
    
    
    tabItem(tabName = "current_dataset",
            h3("DATASET"),
            fluidRow(
              box(width = 12,status = "primary",collapsible = FALSE,solidHeader = TRUE,
                  h3("Dataset Table"),
                  h5(),
                  DTOutput(outputId='x2'),  ## the place to output datasets table
                  br(),
                  actionButton('edit_data', 'Edit',style = "color: white; background-color: teal"),
                  actionButton('delete_data', 'Delete',style = "color: white; background-color: red"),
                  actionButton('clear_selected_data', 'Clear selected',style = "color: white; background-color: green")
              )),
            fluidRow(
              box(width = 12,status = "primary",collapsible = FALSE,solidHeader = TRUE,
                  h3("Related Projects"),
                  DTOutput(outputId='related_proj')
                  # ???
                  #actionButton('go_to_proj', 'Search Projects',style = "color: white; background-color: green"),
                  #h5("(If you want to know more about the project,
                  #   select the project and click 'Search Projects' button then you will go to project page.)")
              ))

            
    ),
    
    tabItem(tabName = "aboutus",
            h3("About Us"),
            fluidRow(
              column(4),
              column(4,tags$img(src = "logo.png", height = 180)),
              column(4)
            ),
            fluidRow(
              column(1),
              column(10,
                     h5("LotusRoot is a sequencing data tracking system. 
                        It can track projects and datasets with standardised metadata, while 
                        still maintaining appropriate access and permissions to users and groups."), 
                     h5("LotusRoot system will make our projects/datasets more 
                        findable, accessible, interoperable and reproducible 
                        based on the FAIR data principles."),
                     br(),
                     h5("Click here to get LotusRoot manual:"),
                     uiOutput("LotusRoot_manual")
                     ),
              column(1),
            
            )
    ),
    tabItem(tabName = "FAQ",
            h3("FAQ"),
            fluidRow(
              
              column(11,
                     box(width = 12,
                         h4("Question: "),
                         h5("What are the different permissions? What can I do with my permissions?"),
                         h4("Answer: "),
                         tags$img(src = "permissions_intro.png", height = 250),
                         ),
                     box(width = 12,
                         h4("Question: "),
                         h5("How to find the datasets related to a project?"),
                         h4("Answer: "),
                         h5("Go to 'Projects' page --> 
                            Select the project you want --> 
                            Find the related datasets in the 'Related Datasets' table")
                         ),
                     box(width = 12,
                         h4("Question: "),
                         h5("How to find the C related to a dataset?"),
                         h4("Answer: "),
                         h5("Go to 'Datasets' page --> 
                            Select the dataset you want --> 
                            Find the related project in the 'Related Projects' table")
                     ),
                     box(width = 12,
                         h4("Question: "),
                         h5("I cannot find the group option I want. What can I do?"),
                         h4("Answer: "),
                         h5("If you have the permissions to add group options, 
                            just go to 'Project Options' page to add the group you want."),
                         h5("If you do not have permissions to add group options,
                            contact your System_Maintenance or Project_Supervisor to do that for you.
                            And you can choose 'Other' in the group input box for the time being.")
                     ),
                     #h5(""), 
                     #h5(""),
                     h5("")
              ),
              column(1),
              
            ),
            fluidRow(
              column(5),
              column(2,tags$img(src = "logo.png", height = 100)),
              column(5)
            )
    ),
    
    
    
    tabItem(tabName = "admin_user",
            h3("System Management —— User"),
            h5(),
            fluidRow(
              box(width = 12,
                  status = "primary",collapsible = FALSE,solidHeader = TRUE,
                  h3("User Information"),
                  fluidRow(
                    column(4, textInput('userName','Username',placeholder = "Please enter full name")),
                    column(4, passwordInput('userPW','Password')),
                    column(4, textInput("userEmail", "Email :")),
                    uiOutput("admin_user_group"),
                    column(4, selectInput(inputId = "userPermissions", "User Permissions:", 
                                          choices = c('General_Staff','Data_Administrator','Project_Supervisor ','System_Maintenance')
                                          )),
                    column(5,offset = 0, actionButton('add_user','Add',style = "color: white; background-color: teal")),
                    column(5,verbatimTextOutput("user_successfully_added"))
                  ),
                  br(),
                  box(width = 12, 
                      DTOutput(outputId='admin_user_info'),
                      actionButton('delete_user','Delete',style = "color: white; background-color: red"))
              ))
    ),
    tabItem(tabName = "admin_p",
            h3("System Management —— Project Options"),
            h5(),
            fluidRow(
              
              box(width = 6, height = 850,
                  status = "primary",collapsible = FALSE,solidHeader = TRUE,
                  h3("Researcher/Bioinformatician"), 
                  br(),
                  fluidRow(
                    column(8, textInput('researcherName','Name:')),
                    column(3, actionButton('add_researcher','Add',style = "color: white; background-color: teal"))
                  ),
                  verbatimTextOutput("researcher_successfully_added"),
                  br(),
                  box(width = 12,
                      DTOutput(outputId='admin_researcher'),
                      actionButton('delete_researcher','Delete',
                                   style = "color: white; background-color: red")
                      )
              ),
              #box(width = 6,height = 670,
              #  status = "primary",collapsible = FALSE,solidHeader = TRUE,
              #  h3("Bioinformatician"), ###
              #  br(),
              #  fluidRow(
              #    column(8, textInput('bioinformaticianName','Bioinformatician Name:')),
              #    column(3, actionButton('add_bioinformatician','Add',style = "color: white; background-color: teal"))
              #  ),
              #  verbatimTextOutput("bioinformatician_successfully_added"),
              #  br(),
              #  box(width = 12,
              #      DTOutput(outputId='admin_bioinformatician'),
              #      actionButton('delete_bioinformatician','Delete',
              #                   style = "color: white; background-color: red")
              #      )
              #),
              box(width = 6,height = 850,
                status = "primary",collapsible = FALSE,solidHeader = TRUE,
                h3("Group"),###
                br(),
                fluidRow(
                  column(8, textInput('groupName','Group Name:')),
                  column(3, actionButton('add_group','Add',style = "color: white; background-color: teal"))
                ),
                verbatimTextOutput("group_successfully_added"),
                br(),
                box(width = 12,
                    DTOutput(outputId='admin_group'),
                    actionButton('delete_group','Delete',
                                 style = "color: white; background-color: red")
                    )
              )
            )
    ),
    tabItem(tabName = "admin_d",
            h3("System Management —— Dataset Options"),
            h5(),
            fluidRow(
              box(width = 6,height = 850,
                  status = "primary",collapsible = FALSE,solidHeader = TRUE,
                  h3("Method"),
                  br(),
                  fluidRow(
                    column(8, textInput('methodName','Method:')),
                    column(3, actionButton('add_method','Add',style = "color: white; background-color: teal"))
                  ),
                  verbatimTextOutput("method_successfully_added"),
                  br(),
                  box(width = 12,
                      DTOutput(outputId='admin_method'),
                      actionButton('delete_method','Delete',
                                   style = "color: white; background-color: red")
                  )
              ),
              box(width = 6,height = 850,
                status = "primary",collapsible = FALSE,solidHeader = TRUE,
                h3("Organism"),
                br(),
                fluidRow(
                  column(8, textInput('organismName','Organism Name:')),
                  column(3, actionButton('add_organism','Add',style = "color: white; background-color: teal"))
                ),
                verbatimTextOutput("organism_successfully_added"),
                br(),
                box(width = 12,
                    DTOutput(outputId='admin_organism'),
                    actionButton('delete_organism','Delete',
                                 style = "color: white; background-color: red")
                )
              ),
              box(width = 6,height = 850,
                status = "primary",collapsible = FALSE,solidHeader = TRUE,
                h3("Tissue/Cell"),
                br(),
                fluidRow(
                  column(8, textInput('cellName','Tissue/Cell Name:')),
                  column(3, actionButton('add_cell','Add',style = "color: white; background-color: teal"))
                ),
                verbatimTextOutput("cell_successfully_added"),
                br(),
                box(width = 12,
                    DTOutput(outputId='admin_cell'),
                    actionButton('delete_cell','Delete',
                                 style = "color: white; background-color: red")
                )
              ),
              #box(width = 6,height = 850,
              #  status = "primary",collapsible = FALSE,solidHeader = TRUE,
              #  h3("Genotype"),
              #  br(),
              #  fluidRow(
              #    column(8, textInput('genotypeName','Genotype Name:')),
              #    column(3, actionButton('add_genotype','Add',style = "color: white; background-color: teal"))
              #  ),
              #  verbatimTextOutput("genotype_successfully_added"),
              #  br(),
              #  box(width = 12,
              #      DTOutput(outputId='admin_genotype'),
              #      actionButton('delete_genotype','Delete',
              #                   style = "color: white; background-color: red")
              #  )
              #),
              box(width = 6,height = 850,
                status = "primary",collapsible = FALSE,solidHeader = TRUE,
                h3("Format"),
                br(),
                fluidRow(
                  column(8, textInput('formatName','Format Name:')),
                  column(3, actionButton('add_format','Add',style = "color: white; background-color: teal"))
                ),
                verbatimTextOutput("format_successfully_added"),
                br(),
                box(width = 12,
                    DTOutput(outputId='admin_format'),
                    actionButton('delete_format','Delete',
                                 style = "color: white; background-color: red")
                )
                
              )
            )
    )
    
    
  )
)
  

##############
ui <- dashboardPage(
  skin = "black",
  header,
  sidebar,
  body,
  
  useShinyjs()
)
#ui <- secure_app(ui, choose_language = TRUE)