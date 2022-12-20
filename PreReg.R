library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "Pre-Registration App"),
  
  dashboardSidebar(  # Copy the line below to make a select box 
    selectInput("Programme", label = h3("Choose Your Degree Programme"), 
                choices = list("G100 BSc Mathematics" = 1, 
                               "G103 MMath Mathematics" = 2, 
                               "GG13 BSc Mathematics and Statistics" = 3), 
                selected = 1),
    selectInput("Stage", 
                label = h3("Which Stage Are You Selecting Modules For?"),
                choices = list("Stage 2" = 2,
                               "Stage 3" = 3,
                               "Stage 4" = 4),
                selected = 3),
    sidebarMenu(
      menuItem("Selection", tabName = "Selection"),
      menuItem("Module Information", tabName = "Module Information")
      )
    ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Selection",
              h2("Module Selection"),
              fluidRow(
                column(6,
                       h3("Semester 1 Modules")),
                column(6,
                       h3("Semester 2 Modules"))
              ),
              fluidRow(
                column(6,
                       checkboxGroupInput("P1", label = h3("Pure Maths"), 
                                          choices = list("MAS3701 Group Theory" = 1, 
                                                         "MAS3702 Linear Algebra" = 2, 
                                                         "MAS3705 Matrix Analysis" = 3,
                                                         "MAS3707 Number Theory and Cryptography" = 4))
                        ),
                column(6,
                       checkboxGroupInput("P2", label = h3("Pure Maths"), 
                                          choices = list("MAS3706 Topology" = 1,
                                                         "MAS3708 Graphs and Symmetry" = 2,
                                                         "MAS3709 Representation Theory" = 3,
                                                         "MAS3713 Curves and Surfaces" = 4))
                       )
              ),
              fluidRow(
                column(6,
                       checkboxGroupInput("A1", label = h3("Applied Maths"), 
                                          choices = list("MAS3803 Advanced Fluid Dynamics" = 1, 
                                                         "MAS3804 Relativity" = 2,
                                                         "MAS3808 Instabilities" = 3,
                                                         "MAS3810 Methods for Solving Differential Equations" = 4))
              ),
              column(6,
                     checkboxGroupInput("A2", label = h3("Applied Maths"), 
                                        choices = list("MAS3802 Quantum Mechanics" = 1,
                                                       "MAS3809 Variational Methods" = 2,
                                                       "MAS3815 Mathematical Biology" = 3))
                     )
              ),
              fluidRow(
                column(6,
                       checkboxGroupInput("S1", label = h3("Statistics"), 
                                          choices = list("MAS3913 Linear Models and Generalised Linear Models" = 1))
                       ),
                column(6,
                       checkboxGroupInput("S2", label = h3("Statistics"), 
                                          choices = list("MAS3902 Bayesian Inference" = 1, 
                                                         "MAS3911 Time Series" = 2)))
              ),
              plotOutput("cred.bar"),       
      tabItem(tabName = "Module Information", h2("Module Information")
      )
    )
  )
)
)

server <- function(input, output) {
  output$cred.bar = renderPlot({
    cred.df = data.frame(Section = rep(c("Pure", "Applied", "Stats"), each = 2),
                    Semester = factor(rep(1 : 2, 3)),
                    Credits = c(length(input$P1), 
                                length(input$P2),
                                length(input$A1), 
                                length(input$A2), 
                                length(input$S1), 
                                length(input$S2)) * 10)
    ggplot(data = cred.df, aes(x = Semester, 
                               y = Credits, 
                               fill = Section)) +
      geom_col()
  })
}

shinyApp(ui, server)
