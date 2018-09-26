#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# install.packages("shinydashboard")
library(shiny)
library(shinydashboard)

header <- dashboardHeader(
  dropdownMenu(
    type = "notifications",
    notificationItem(
      text = "The International Space Station is overhead!"
    )
  )
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem(text = "Dashboard", tabName = "dashboard"),
    menuItem(text = "Inputs", tabName = "inputs")
  )
)

# body <- dashboardBody(
# 
#   tabItems(
#     tabItem(
#       tabName = "dashboard",
#       tabBox(
#         title =  "International Space Station Fun Facts",
#         tabPanel("Fun Fact 1"),
#         tabPanel("Fun Fact 2")
#       )
#     ),
#     tabItem(tabName = "inputs")
#   )
# )

server <- function(input, output, session) {
  reactive_starwars_data <- reactiveFileReader(
    intervalMillis = 1000,
    session = session,
    filePath = starwars_url,
    readFunc = function(filePath) { 
      read.csv(url(filePath))
    }
  )
  
  output$table <- renderTable({
    reactive_starwars_data()
  })
}

body <- dashboardBody(
  tableOutput("table")
)

ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body
)
shinyApp(ui, server)

# # Define UI for application that draws a histogram
# ui <- fluidPage(
#    
#    # Application title
#    titlePanel("Old Faithful Geyser Data"),
#    
#    # Sidebar with a slider input for number of bins 
#    sidebarLayout(
#       sidebarPanel(
#          sliderInput("bins",
#                      "Number of bins:",
#                      min = 1,
#                      max = 50,
#                      value = 30)
#       ),
#       
#       # Show a plot of the generated distribution
#       mainPanel(
#          plotOutput("distPlot")
#       )
#    )
# )
# 
# # Define server logic required to draw a histogram
# server <- function(input, output) {
#    
#    output$distPlot <- renderPlot({
#       # generate bins based on input$bins from ui.R
#       x    <- faithful[, 2] 
#       bins <- seq(min(x), max(x), length.out = input$bins + 1)
#       
#       # draw the histogram with the specified number of bins
#       hist(x, breaks = bins, col = 'darkgray', border = 'white')
#    })
# }
# 
# # Run the application 
# shinyApp(ui = ui, server = server)

