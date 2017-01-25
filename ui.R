library(shiny)

fluidPage(
  titlePanel("SimpleR TODO App"),
  actionButton("addnewtask","Add New Task"),
  textInput("filtCrit","Filter Criteria"),
  actionButton("gettodo","Get Todo"),
  tabsetPanel(type = "tabs",
              tabPanel("TODOlist",
                 uiOutput("todolist")
                       ),
              tabPanel("Add/Edit Task",
                 uiOutput("addedit_task"),
                 actionButton("updatetask","Update Task")
                       ),
              tabPanel("Details View",
                 uiOutput("viewDetails")
                       ),
              tabPanel("Add Details",
                 uiOutput("addDetailsUI")
                       )
  )
)