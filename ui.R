library(shiny)
library(shinythemes)

fluidPage(theme = shinytheme("cerulean"),
  titlePanel("SimpleR TODO App"),
  fluidRow(
    column(4,
           textInput("filtCrit","Filter Criteria") 
           ),
    column(4,
           selectInput("showLvl","Show Levels",choices = c("1","1 and 2"))      
           )
      ),
  tabsetPanel(type = "tabs",
              tabPanel("TODOlist",
                 uiOutput("todolist")
                       ),
              tabPanel("Add/Edit Task",
                 br(),
                 fluidRow(
                   column(2,
                          actionButton("addnewtask","Add New Task")
                   ),
                   column(2,
                          actionButton("updatetask","Update Task")
                   )                 
                 ),
                 uiOutput("addedit_task")
                       ),
              tabPanel("Details View",
                 uiOutput("viewDetails")
                       ),
              tabPanel("Add Details",
                 uiOutput("addDetailsUI")
                       )
  )
)