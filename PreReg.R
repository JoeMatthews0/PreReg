library(shinydashboard)
library(ggplot2)
library(DT)

mods = read.csv("Modules.csv")

ui <- dashboardPage(
  dashboardHeader(title = "Pre-Registration App"),
  
  dashboardSidebar(
    selectInput("Programme", label = h3("Choose Your Degree Programme"), 
                choices = list("G100 BSc Mathematics" = 1, 
                               "G103 MMath Mathematics" = 2, 
                               "GG13 BSc Mathematics and Statistics" = 3,
                               "GGC3 MMathStat Mathematics and Statistics" = 4), 
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
                conditionalPanel(condition = "input.Stage == 4",
                                 column(6, 
                                        selectInput("Special", 
                                                    label = h3("Which Section Are You Specialising In?"),
                                                    choices = list("Pure" = 1,
                                                                   "Applied" = 2,
                                                                   "Statistics" = 3),
                                                    selected = 1)     
                                 ) 
                ),
                h2("Compulsory Modules"),
                fluidRow(textOutput("comp.text")),
                fluidRow(DTOutput("comp.table"))
                  ),
               fluidRow(
                 h2("Optional Modules")
               ),
               fluidRow(
                 column(6,
                   h3("Semester 1 Modules")),
                 column(6,
                   h3("Semester 2 Modules"))
                ),
                fluidRow( 
                  column(6,
                    checkboxGroupInput("P1", label = "Pure Maths", 
                                             choices = mods$Module)
                          ),
                   column(6,
                    checkboxGroupInput("P2", label = "Pure Maths", 
                                             choices = mods$Module)
                         )
                        ),
                 fluidRow(
                   column(6,
                     checkboxGroupInput("A1", label = "Applied Maths", 
                                              choices = mods$Module)
                         ),
                    column(6,
                      checkboxGroupInput("A2", label = "Applied Maths", 
                                               choices = mods$Module)
                          )
                         ),
                   fluidRow(
                    column(6,
                      checkboxGroupInput("S1", label = "Statistics", 
                                               choices = mods$Module)
                         ),
                     column(6,
                       checkboxGroupInput("S2", label = "Statistics", 
                                                choices = mods$Module)
                         )
                   ),
              fluidRow(h2("Your Selection")),
              fluidRow(p("Your current module selection is")),
              fluidRow(
                DTOutput("table.summary")
              ),
              fluidRow(plotOutput("cred.bar")), 
       tabItem(tabName = "Module Information", h2("Module Information"))
              )
          )
      )
)

server <- function(input, output, session) {
  
  output$comp.table = renderDT({
    if(input$Stage == 3){
      data.frame(Module = "MAS3091 Group Project",
                 Stage = 3,
                 Section = "Project",
                            `Semester 1 Credits` = 10,
                            `Semester 2 Credits` = 0,
                            Exam = "None")
    } else if(input$Stage == 4 & input$Special == 1){
      data.frame(Module = c("MAS8714 Topics in Analysis and Functional Analysis",
                                       "MAS8715 Algebraic Topology & Galois Theory",
                                       "MAS8091 MMath Project"),
                 Stage = rep(4, 3),
                 Section = c("Pure", "Pure", "Project"),
                            `Semester 1 Credits` = c(10, 20, 20),
                            `Semester 2 Credits` = c(20, 10, 20),
                            Exam = c("Semester 2", "Semester 2", "None"))
    } else if(input$Stage == 4 & input$Special == 2){
      data.frame(Module = c("MAS8810 Geophysical and Astrophysical Fluids",
                                       "MAS8811 General Relativity",
                                       "MAS8812 Quantum Fluids",
                                       "MAS8091 MMath Project"),
                 Stage = rep(4, 3),
                 Section = c("Applied", "Applied", "Project"),
                            `Semester 1 Credits` = c(10, 10, 10, 20),
                            `Semester 2 Credits` = c(10, 10, 10, 20),
                            Exam = c("Semester 2", "Semester 2", "Semester 2", "None"))
    } else if(input$Stage == 4 & input$Special == 3){
      data.frame(Module = c("MAS8951 Modern Bayesian Inference",
                                       "MAS8952 Research Topics in Statistics",
                                       "MAS8391 MMathStat Project"),
                 Stage = rep(4, 3),
                 Section = c("Statistics", "Statistics", "Project"),
                            `Sem 1 Credits` = c(15, 15, 20),
                            `Sem 2 Credits` = c(15, 15, 20),
                            Exam = c("Semester 2", "Semester 2", "None"))
    }
  })
  
  output$comp.text = renderText({
    if(input$Stage == 3){
      pl = "module."
    }
    else{
      pl = "modules."
    }
    c("As part of your programme, you must take the following complusory", pl)
  })
  
  observe({
    p1.choice = subset(mods, Stage == input$Stage & 
                                Section == "Pure" & 
                                Semester.1.Credits > 0)$Module
    p2.choice = subset(mods, Stage == input$Stage & 
                                Section == "Pure" & 
                                Semester.2.Credits > 0)$Module
    a1.choice = subset(mods, Stage == input$Stage & 
                                Section == "Applied" & 
                                Semester.1.Credits > 0)$Module
    a2.choice = subset(mods, Stage == input$Stage & 
                                Section == "Applied" & 
                                Semester.2.Credits > 0)$Module
    s1.choice = subset(mods, Stage == input$Stage & 
                                Section == "Statistics" & 
                                Semester.1.Credits > 0)$Module
    s2.choice = subset(mods, Stage == input$Stage & 
                                Section == "Statistics" & 
                                Semester.2.Credits > 0)$Module
  
    updateCheckboxGroupInput(session, "P1", label = "Pure Maths", 
                            choices = p1.choice)
    updateCheckboxGroupInput(session, "P2", label = "Pure Maths", 
                            choices = p2.choice)
    updateCheckboxGroupInput(session, "A1", label = "Applied Maths", 
                            choices = a1.choice)
    updateCheckboxGroupInput(session, "A2", label = "Applied Maths", 
                            choices = a2.choice)
    updateCheckboxGroupInput(session, "S1", label = "Statistics", 
                            choices = s1.choice)
    updateCheckboxGroupInput(session, "S2", label = "Statistics", 
                            choices = s2.choice)
  })
  
  output$cred.bar = renderPlot({
    
    comp.cred = rep(0, 8)
    
      if(input$Stage  == 3){
        comp.cred = c(10, rep(0, 7))
      } else if(input$Stage == 4){
        if(input$Special == 1){
          comp.cred = c(20, 20, 30, 30, rep(0, 4))
        } else if(input$Special == 2){
          comp.cred = c(20, 20, 0, 0, 30, 30, 0, 0)
        } else if(input$Special == 3){
          comp.cred = c(20, 20, 0, 0, 0, 0, 30, 30)
        }
      }
    
    cred.df = data.frame(Section = rep(c("Project", "Pure", "Applied", "Stats"), each = 2),
                         Semester = factor(rep(1 : 2, 4)),
                         Credits = comp.cred + 
                           c(0, 0,
                              sum(mods$Semester.1.Credits[mods$Module %in% input$P1 & mods$Section == "Pure"]),
                              sum(mods$Semester.2.Credits[mods$Module %in% input$P2 & mods$Section == "Pure"]),
                              sum(mods$Semester.1.Credits[mods$Module %in% input$A1 & mods$Section == "Applied"]),
                              sum(mods$Semester.2.Credits[mods$Module %in% input$A2 & mods$Section == "Applied"]),
                              sum(mods$Semester.1.Credits[mods$Module %in% input$S1 & mods$Section == "Statistics"]),
                              sum(mods$Semester.2.Credits[mods$Module %in% input$S2 & mods$Section == "Statistics"])
                             )
                         )
    ggplot(data = cred.df, aes(x = Semester, 
                               y = Credits, 
                               fill = Section)) +
      geom_col() +
      ylim(0, 80)
  })
  
  output$table.summary = renderDT({
    comp.mod = data.frame()
    if(input$Stage == 3){
      comp.mod = data.frame(Module = "MAS3091 Group Project",
                            Stage = 3,
                            Section = "Project",
                 `Semester 1 Credits` = 10,
                 `Semester 2 Credits` = 0,
                 Exam = "None")
    } else if(input$Stage == 4 & input$Special == 1){
      comp.mod = data.frame(Module = c("MAS8714 Topics in Analysis and Functional Analysis",
                            "MAS8715 Algebraic Topology & Galois Theory",
                            "MAS8091 MMath Project"),
                            Stage = rep(4, 3),
                            Section = c("Pure", "Pure", "Project"),
                 `Semester 1 Credits` = c(10, 20, 20),
                 `Semester 2 Credits` = c(20, 10, 20),
                 Exam = c("Semester 2", "Semester 2", "None"))
    } else if(input$Stage == 4 & input$Special == 2){
      comp.mod = data.frame(Module = c("MAS8810 Geophysical and Astrophysical Fluids",
                            "MAS8811 General Relativity",
                            "MAS8812 Quantum Fluids",
                            "MAS8091 MMath Project"),
                            Stage = rep(4, 3),
                            Section = c("Applied", "Applied", "Project"),
                 `Semester 1 Credits` = c(10, 10, 10, 20),
                 `Semester 2 Credits` = c(10, 10, 10, 20),
                 Exam = c("Semester 2", "Semester 2", "Semester 2", "None"))
    } else if(input$Stage == 4 & input$Special == 3){
      comp.mod = data.frame(Module = c("MAS8951 Modern Bayesian Inference",
                            "MAS8952 Research Topics in Statistics",
                            "MAS8391 MMathStat Project"),
                            Stage = rep(4, 3),
                            Section = c("Statistics", "Statistics", "Project"),
                 `Semester 1 Credits` = c(15, 15, 20),
                 `Semester 2 Credits` = c(15, 15, 20),
                 Exam = c("Semester 2", "Semester 2", "None"))
    }
    rbind(comp.mod, mods[mods$Module %in% c(input$P1, input$P2,
                                            input$A1, input$A2,
                                            input$S1, input$S2), ])
  })
}

shinyApp(ui, server)
