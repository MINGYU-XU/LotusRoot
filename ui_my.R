#
# This is my laboratory information management system (LIMS)
# Shiny web application
#


library(shinydashboard)
library(shiny)
library(tidyverse)

# ui
# dashboard_header
header <- dashboardHeader(
  title="MY LIMS",
  dropdownMenu(
    type="messages",
    #add Items
    messageItem(
      # message source
      from="system",
      # message content
      message="process ended",
      # icon for the message
      icon=icon(name="envelope")
    )
  )
)

# dashboard_sidebar 
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Home", tabName = "home", icon = icon(name="home")),
    menuItem("My Project", icon = icon(name="dna"), tabName = "my_project",
             badgeLabel = "new", badgeColor = "green"),
    menuItem("About Us", tabName = "about_us", icon = icon(name="user-friends")),
    menuItem("FAQ", tabName = "FAQ", icon = icon(name="question-circle"))
  )
)

# dashboard_body
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "Home",
            h2("Home")
    ),
    
    tabItem(tabName = "My Project",
            h2("My Project")
    ),
    
    tabItem(tabName = "About us",
            h2("About us")
    ),
      
    tabItem(tabName = "FAQ",
            h2("FAQ")
    )
  )
)


#ui
ui <- dashboardPage(
  header,
  sidebar,
  body
)



# Define server logic required to draw a histogram
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
