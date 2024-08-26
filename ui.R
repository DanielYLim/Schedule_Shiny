ui <- navbarPage(
  title = "Circle Time Schedule App",
  
  # First tab for schedule table
  tabPanel("Schedule Table",
           sidebarLayout(
             sidebarPanel(
               textInput("newTime", "New Time Interval (HH:MM - HH:MM):"),
               textInput("newActivity", "New Activity Name:"),
               actionButton("addButton", "Add"),
               actionButton("deleteButton", "Delete Selected Row")
             ),
             
             mainPanel(
               DTOutput("scheduleTable")
             )
           )
  ),
  
  # Second tab for pie chart
  tabPanel("Schedule Circle",
           mainPanel(
             plotOutput("pieChart")
           )
  )
)
