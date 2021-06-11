#
# This is my laboratory information management system (LIMS)
# Shiny web application --- ui
# 


library(shinydashboard)
library(shiny)
library(tidyverse)
library(shinymanager)

# ui
# dashboard_header ----------------------------------
header <- dashboardHeader(
  titleWidth = '180px',
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

# dashboard_sidebar -----------------------------------
sidebar <- dashboardSidebar(
  width = '180px',
  sidebarMenu(
    menuItem("Home", tabName = "home", 
             icon = icon(name="home")),
    menuItem("My Project", icon = icon(name="dna"), 
             tabName = "myproject",
        menuItem("Creat New Project", 
                 tabName = "creat_new_project", 
                 icon = icon(name = "plus-circle")),
        menuItem("My Current Projects", 
                 tabName = "current_project", 
                 icon = icon(name = "th-list"))
    ),
    menuItem("About Us", tabName = "aboutus", 
             icon = icon(name="user-friends")),
    menuItem("FAQ", tabName = "FAQ", 
             icon = icon(name="question-circle"))
  )
)

# dashboard_body ------------------------------------------
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "home",
            h2("Home"),
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
            h2("Creat New Project"),
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
            h2("My Current Projects")
            # list of current projects
    ),
    
    tabItem(tabName = "aboutus",
            h2("About Us"),
    ),
      
    tabItem(tabName = "FAQ",
            h2("FAQ"),
    )
  )
)



#ui
ui <- dashboardPage(
        header,
        sidebar,
        body
)

#---------------------------------------------------------

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
