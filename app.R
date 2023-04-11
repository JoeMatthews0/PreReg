library(shinydashboard); library(ggplot2); library(DT); library(dplyr); library(stringr); library(readxl)

# Read in module data
mods.full = read_xlsx("Modules.xlsx")
mods = mods.full %>% filter(Active == "Y")

#clashTrips = matrix(c("MAS3701", "MAS3808", "MAS3905",
#                      "MAS3706", "MAS3805", "MAS3907",
#                      "MAS3709", "MAS3802", "MAS3918"), byrow = T, ncol = 3)

pureReq = mods %>% filter(`S4 Prereq` == "PU") %>% select(Module) %>% unlist() %>% as.character()
appReq = mods %>% filter(`S4 Prereq` == "AP") %>% select(Module) %>% unlist() %>% as.character()
statReq = mods %>% filter(`S4 Prereq` == "ST") %>% select(Module) %>% unlist() %>% as.character()

ui <- dashboardPage(
  
  # App header
  dashboardHeader(title = "Pre-Registration App"),
  
  dashboardSidebar(
    
    # User enters their degree programme
    selectInput("Programme", label = h3("Choose Your Degree Programme"), 
                choices = list("G100 BSc Mathematics" = 'BMath', 
                               "G103 MMath Mathematics" = 'MMath', 
                               "GG13 BSc Mathematics and Statistics" = 'BMaS',
                               "GGC3 MMathStat Mathematics and Statistics" = 'MMaS',
                               "GL11 BSc Mathematics and Economics" = 'MaE',
                               "NG41 BSc Mathematics and Accounting" = 'MaA',
                               "G1N3 BSc Mathematics with Finance" = 'MwF',
                               "G1N4 BSc Mathematics with Business" = 'MwB',
                               "G300 BSc Statistics" = 'Stat',
                               "F300 BSc Physics" = 'BPhys',
                               "F303 MPhys Physics" = 'MPhys',
                               "F3F5 BSc Physics with Astrophysics" = 'BPwA',
                               "F3FM MPhys Physics with Astrophysics" = 'MPwA',
                               "F345 BSc Theoretical Physics" = 'BTP',
                               "F344 MPhys Theoretical Physics" = 'MTP'),
                selected = "BMath"),
    
    # User enters their degree stage
    selectInput("Stage", 
                label = h3("Which Stage Are You Selecting Modules For?"),
                choices = list("Stage 2" = 2,
                               "Stage 3" = 3,
                               "Stage 4" = 4),
                selected = 3),
    sidebarMenu(
      menuItem("Selection", tabName = "Selection"),
      menuItem("Module Information", tabName = "ModuleInfo")
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "Selection",
              h2("Module Selection"),
                p("Welcome to the School of Maths, Stats and Physics pre-registration app. 
                  This app is designed to help you make your pre-registration choices for your modules next year.
                  Please note that you cannot formally submit your choices on this app, that must still be done through S3P.
                  This app is simply to help you understand which choices are possible for you, and what your next year might look like."
                  ),
                p("To use the app, please select the programme you are enrolled on, and the stage you will be entering next year
                  (i.e. if you are currently in Stage 2, you should select Stage 3 since that is the stage you will be choosing modules for)."),
                p("Once you have selected your Programme and Stage, please see below for your programme regulations, compulsory modules,
                  and the optional modules you can choose from."),
                p("Once you have found a combination of modules you're happy with, log in to S3P to formally submit your selection."),
                h2("Programme Regulations"),
                
                # If the user is MMath/MMathStat -- ask which specialism
                conditionalPanel(condition = "input.Stage == 4 && (input.Programme == 'MMath' || input.Programme == 'MMaS')",
                                        selectInput("Special", 
                                                    label = h3("Which Section Are You Specialising In?"),
                                                    choices = list("Pure" = 'P',
                                                                   "Applied" = 'A',
                                                                   "Statistics" = 'S'),
                                                    selected = 1),
                                        htmlOutput("s4SpecialPreReq")
                                 ),
                conditionalPanel(condition = "input.Programme == 'MaE' || 
                                                                   input.Programme == 'MaA' ||
                                                                   input.Programme == 'MwF' ||
                                                                   input.Programme == 'MwB'",
                                 htmlOutput("JHText"),
                                 selectInput("JHPathway",
                                             label = NULL,
                                             choices = list("Pure" = 'P',
                                                            "Applied" = 'A',
                                                            "Statistics" = 'S'),
                                             selected = 'P')
                ),
                h2("Compulsory Modules"),
                
                # Show compulsory module details
                textOutput("comp.text"),
                DTOutput("comp.table"),
                 h2("Optional Modules"),
                p("Please note some modules take place over both semesters, and so will appear under both 
                'Semester 1 Modules' and 'Semester 2 Modules'. 
                For these modules, clicking either the Semester 1 or Semester 2 box will select the module 
                for the entire year (i.e. you don't need to click both boxes)"),
               fluidRow(
                 column(6,
                   h3("Semester 1 Modules")),
                 column(6,
                   h3("Semester 2 Modules"))
                ),
              
                # Pure maths optional modules
              conditionalPanel(condition = "input.Programme == 'BMath' ||
                               input.Programme == 'MMath' ||
                               input.Programme == 'BMaS' ||
                               input.Programme == 'MMaS' ||
                               input.Programme == 'MaE' || 
                               input.Programme == 'MaA' ||
                               input.Programme == 'MwF' ||
                               input.Programme == 'MwB'",
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
                               
                               # Applied maths optional modules
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
                               
                               # Statistics optional modules
                               fluidRow(
                                 column(6,
                                        checkboxGroupInput("S1", label = "Statistics", 
                                                           choices = mods$Module)
                                 ),
                                 column(6,
                                        checkboxGroupInput("S2", label = "Statistics", 
                                                           choices = mods$Module)
                                 )
                               ) 
              ),
              conditionalPanel(condition = "input.Programme == 'MaA' | input.Programme == 'MwF' | input.Programme == 'MwB'",
                               fluidRow(
                                 column(6,
                                        checkboxGroupInput("ACC1", label = "Accounting", 
                                                           choices = mods$Module)
                                 ),
                                 column(6,
                                        checkboxGroupInput("ACC2", label = "Accounting", 
                                                           choices = mods$Module)
                                 )
                               )
                            ),
              conditionalPanel(condition = "input.Programme == 'MaE'",
                               fluidRow(
                                 column(6,
                                        checkboxGroupInput("ECO1", label = "Economics", 
                                                           choices = mods$Module)
                                 ),
                                 column(6,
                                        checkboxGroupInput("ECO2", label = "Economics", 
                                                           choices = mods$Module)
                                 )
                               )
              ),
              conditionalPanel(condition = "input.Programme == 'MwB'",
                               fluidRow(
                                 column(6,
                                        checkboxGroupInput("BUS1", label = "Business", 
                                                           choices = mods$Module)
                                 ),
                                 column(6,
                                        checkboxGroupInput("BUS2", label = "Business", 
                                                           choices = mods$Module)
                                 )
                               )
              ),
              conditionalPanel(condition = "input.Programme == 'BPhys' | input.Programme == 'MPhys' | input.Programme == 'BPwA' | 
                               input.Programme == 'MPwA' | input.Programme == 'BTP' | input.Programme == 'MTP'",
                               fluidRow(
                                 column(6,
                                        checkboxGroupInput("PHY1", label = "Physics", 
                                                           choices = mods$Module)
                                 ),
                                 column(6,
                                        checkboxGroupInput("PHY2", label = "Physics", 
                                                           choices = mods$Module)
                                 )
                               )
                               ),
              conditionalPanel(condition = "input.Stage == 3",
                               fluidRow(column(6,
                                               checkboxGroupInput("C", label = "Careers", 
                                                                  choices = mods$Module)
                                               )
                                    )
                               ),
              p("Alternatively copy and paste your selection here"),
              textInput("textModules", "Enter your module choices, separated by spaces"),
              textOutput("dupWarning"),
              textOutput("invalidWarning"),
              h2("Your Selection"),
              p("Your current module selection is"),
              # Display chosen module summaries
                DTOutput("tableSummary"),
              # Bar chart showing credit breakdown by semester/section
                plotOutput("cred.bar"), 
                htmlOutput("examText"),
                htmlOutput("credTotals"),
              h3("Number of Credits"),
              htmlOutput("credsWarning"),
              conditionalPanel(condition = "output.totCreds == 120 && output.s1Creds >= 50 && output.s1Creds <= 70 && 
                               output.s2Creds >= 50 && output.s2Creds <= 70",
                               h3("Timetable Clashes & Co-requisite Requirements"),
                                  htmlOutput("coreqWarning"),
                                  htmlOutput("clashWarning"),
                                  #conditionalPanel(condition = "output.nMissingCoreqs == 0 && output.nClashes < 2",
                               h3("Required Pre-requisites"),
                                                 htmlOutput("hasPrereqWarning"),
                                    conditionalPanel("input.Stage == 3 && (input.Programme == 'BMath' ||
                                                                            input.Programme == 'MMath' ||
                                                                            input.Programme == 'BMaS' ||
                                                                            input.Programme == 'MMaS' ||
                                                                            input.Programme == 'BStat')",
                                                     h3("Requirements for Stage 4 MMath/MMathStat Programmes"),
                                                  htmlOutput("s4Warning")
                                ),
                                conditionalPanel(condition = "(input.Programme == 'BMath' || input.Programme == 'BMaS') && input.Stage == 3",
                                                 h3("Degree Title"),
                                                htmlOutput("degreeTitle")
                               )
                            #)
                  )
                ),
      tabItem(tabName = "ModuleInfo",
              DTOutput("fullTable")
              )
          )
  )
)

server <- function(input, output, session) {
  
  observe({
    if(input$Programme %in% c('BMath', 'BMaS', 'BStat')){ # If on SH Maths, you only need to choose for stages 3 and 4
      stageChoices = 3
      } else if(input$Programme %in% c('MMath', 'MMaS')){
        stageChoices = 3 : 4
      } else if(input$Programme %in% c('MPhys', 'MPwA', 'MTP')){ # Masters Physics might need to choose any from stages 2 to 4
      stageChoices = 2 : 4
    } else{
      stageChoices = 2 : 3 # Joint honours & BSc Phys just need stages 2 and 3
    }
    updateSelectInput(session, "Stage", label = "Which Stage Are You Selecting Modules For?", 
                             choices = stageChoices)
  })
  
  output$JHText <- renderText({
    if(input$Stage == 2){
      paste0("<span style=\"font-size:18px\">", "Which pathway would you like to take?", "</span>")
    } else if(input$Stage == 3){
      paste0("<span style=\"font-size:18px\">", "Which pathway are you on?", "</span>")
    }
  })
  
  output$comp.text = renderText({
    if(nrow(compulsory() == 1)){
      pl = "module."
    }
    else{
      pl = "modules."
    }
    c("As part of your programme, you must take the following complusory", pl)
  })
  
  f = function(cc){
    tmp = sapply(1 : length(cc), function(i){str_detect(mods$Compulsory, cc[i])})                
    return(apply(tmp, 1, sum))
  }
  
  compulsory <- reactive({
    if(input$Stage == 3 & input$Programme %in% c('BMath', 'MMath', 'BMaS', 'MMaS', 'BStat')){
      compCode = "SH"
    } else if(input$Stage == 4 & input$Programme %in% c('MMath', 'MMaS')){
      if(input$Special == 'P'){
        compCode = "PU"
      } else if(input$Special == 'A'){
        compCode = "AP"
      } else if(input$Special == 'S'){
        compCode = "ST"
      }
    } else if(input$Programme %in% c('MaE', 'MaA', 'MwF', 'MwB')){
      if(input$JHPathway == 'P'){
        cc = "PJH"
      } else if(input$JHPathway == 'A'){
        cc = "AJH"
      } else if(input$JHPathway == 'S'){
        cc = "SJH"
      }
      if(input$Programme == 'MaE'){
        compCode = c(cc, "GL11")
      } else if(input$Programme == 'MaA'){
        compCode = c(cc, "NG41") 
      } else if(input$Programme == 'MwF'){
        compCode = c(cc, "G1N3") 
      } else if(input$Programme == 'MwB'){
        compCode = c(cc, "G1N4") 
        } 
      } else if(input$Programme == 'BPhys'){
      compCode = "F300"
    } else if(input$Programme == 'MPhys'){
      compCode = "F303"
    } else if(input$Programme == 'BPwA'){
      compCode = "F3F5"
    } else if(input$Programme == 'MPwA'){
      compCode = "F3FM"
    } else if(input$Programme == 'BTP'){
      compCode = "F345"
    } else if(input$Programme == 'MTP'){
      compCode = "F344"
    }
    mods %>% filter(Stage == input$Stage & f(compCode))
  })

  output$s4SpecialPreReq <- renderText({
    if(input$Special == 'P'){
      txt = paste0("To take the pure maths pathway you must have taken ")
      for(i in 1 : length(pureReq)){
        txt = paste(txt, pureReq[i], ", ")
      }
    } else if(input$Special == 'A'){
      txt = paste0("To take the applied maths pathway you must have taken ")
      for(i in 1 : length(appReq)){
        txt = paste(txt, appReq[i], ", ")
      }
    } else if(input$Special == 'S'){
      txt = paste0("To take the statistics pathway you must have taken ")
      for(i in 1 : length(statReq)){
        txt = paste(txt, statReq[i], ", ")
      }
    }  
    paste(txt, "in Stage 3. <br> Please make sure you've done this before selecting your Stage 4 modules.")
  })
  
  # Table for compulsory module(s)
  output$comp.table = renderDT({
    compulsory() %>% select(Module, 
                            Section, 
                            `Semester 1 Credits`, 
                            `Semester 2 Credits`,
                            Exam)
  })
  
  output$rulesText <- renderText({
    txt = paste0("From your optional modules, you must choose ")
    if(input$Programme == "BMath"){
      paste0(txt, "At least ")
    }
  })
  
  # Display the correct optional modules for the user programme/stage
  
  observe({
    p1.choice = subset(mods, Stage == input$Stage & 
                         Section == "Pure" & 
                         `Semester 1 Credits` > 0 &
                         !Module %in% compulsory()$Module)$Module
    p2.choice = subset(mods, Stage == input$Stage & 
                         Section == "Pure" & 
                         `Semester 2 Credits` > 0 &
                         !Module %in% compulsory()$Module)$Module
    a1.choice = subset(mods, Stage == input$Stage & 
                         Section == "Applied" & 
                         `Semester 1 Credits` > 0 &
                         !Module %in% compulsory()$Module)$Module
    a2.choice = subset(mods, Stage == input$Stage & 
                         Section == "Applied" & 
                         `Semester 2 Credits` > 0 &
                         !Module %in% compulsory()$Module)$Module
    s1.choice = subset(mods, Stage == input$Stage & 
                         Section == "Statistics" & 
                         `Semester 1 Credits` > 0 &
                         !Module %in% compulsory()$Module)$Module
    s2.choice = subset(mods, Stage == input$Stage & 
                         Section == "Statistics" & 
                         `Semester 2 Credits` > 0 &
                         !Module %in% compulsory()$Module)$Module
    phy1.choice = subset(mods, Stage == input$Stage & 
                            Section == "Physics" & 
                            `Semester 1 Credits` > 0 &
                           !Module %in% compulsory()$Module)$Module
    phy2.choice = subset(mods, Stage == input$Stage & 
                           Section == "Physics" & 
                           `Semester 2 Credits` > 0 &
                           !Module %in% compulsory()$Module)$Module
    acc1.choice = subset(mods, Stage == input$Stage & 
                           Section == "Accounting" & 
                           `Semester 1 Credits` > 0 &
                           !Module %in% compulsory()$Module)$Module
    acc2.choice = subset(mods, Stage == input$Stage & 
                           Section == "Accounting" & 
                           `Semester 2 Credits` > 0 &
                           !Module %in% compulsory()$Module)$Module
    eco1.choice = subset(mods, Stage == input$Stage & 
                           Section == "Economics" & 
                           `Semester 1 Credits` > 0 &
                           !Module %in% compulsory()$Module)$Module
    eco2.choice = subset(mods, Stage == input$Stage & 
                           Section == "Economics" & 
                           `Semester 2 Credits` > 0 &
                           !Module %in% compulsory()$Module)$Module
    bus1.choice = subset(mods, Stage == input$Stage & 
                           Section == "Business" & 
                           `Semester 1 Credits` > 0 &
                           !Module %in% compulsory()$Module)$Module
    bus2.choice = subset(mods, Stage == input$Stage & 
                           Section == "Business" & 
                           `Semester 2 Credits` > 0 &
                           !Module %in% compulsory()$Module)$Module
    c.choice = subset(mods, Stage == input$Stage & 
                        Section == "Careers"&
                        !Module %in% compulsory()$Module)$Module
    
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
    updateCheckboxGroupInput(session, "PHY1", label = "Physics",
                             choices = phy1.choice)
    updateCheckboxGroupInput(session, "PHY2", label = "Physics",
                             choices = phy2.choice)
    updateCheckboxGroupInput(session, "ACC1", label = "Accountancy",
                             choices = acc1.choice)
    updateCheckboxGroupInput(session, "ACC2", label = "Accountancy",
                             choices = acc2.choice)
    updateCheckboxGroupInput(session, "ECO1", label = "Economics",
                             choices = eco1.choice)
    updateCheckboxGroupInput(session, "ECO2", label = "Economics",
                             choices = eco2.choice)
    updateCheckboxGroupInput(session, "BUS1", label = "Business",
                             choices = bus1.choice)
    updateCheckboxGroupInput(session, "BUS2", label = "Business",
                             choices = bus2.choice)
    updateCheckboxGroupInput(session, "C", label = "Careers", 
                             choices = c.choice)
  })
  
  chosenModules <- reactive({
    rbind(compulsory(), subset(mods, Code %in% strsplit(input$textModules, " ")[[1]] |
                                 Module %in% c(input$P1, 
                                               input$P2, 
                                               input$A1, 
                                               input$A2,
                                               input$S1, 
                                               input$S2,
                                               input$ACC1,
                                               input$ACC2,
                                               input$ECO1,
                                               input$ECO2,
                                               input$BUS1,
                                               input$BUS2,
                                               input$PHY1,
                                               input$PHY2,
                                               input$C)))
  })
  
  uniqueChosen <- reactive({
    chosenModules() %>% distinct %>% arrange(Module)
  })
  
  output$credTotals <- renderText({
    txt = paste0("You have currently selected ",
                 sum(uniqueChosen()$`Semester 1 Credits`) + sum(uniqueChosen()$`Semester 2 Credits`),
                 " credits of modules (",
                 sum(uniqueChosen()$`Semester 1 Credits`),
                 " credits in Semester 1, and ",
                 sum(uniqueChosen()$`Semester 2 Credits`),
                 " credits in Semester 2).")
    paste("<span style=\"font-size:18px\">", txt, "</span><br>")
  })
  
  output$s1Creds <- reactive({
    sum(uniqueChosen()$`Semester 1 Credits`)
  })
  
  output$s2Creds <- reactive({
    sum(uniqueChosen()$`Semester 2 Credits`)
  })
  
  output$totCreds <- reactive({
    sum(uniqueChosen()$`Semester 1 Credits`) + sum(uniqueChosen()$`Semester 2 Credits`)
  })
  
  outputOptions(output, "s1Creds", suspendWhenHidden = FALSE)
  outputOptions(output, "s2Creds", suspendWhenHidden = FALSE)
  outputOptions(output, "totCreds", suspendWhenHidden = FALSE)
  
  output$dupWarning <- renderText({
    modCounts = table(chosenModules()$Code)
    if(max(modCounts > 1)){
      paste("Warning: You have chosen the same module(s) twice. Duplicated modules: ", names(which(modCounts > 1)))
    } 
  })
  
  output$invalidWarning <- renderText({
    if(sum(!chosenModules()$Code %in% mods$Code) > 0){
      paste("Warning: You have selected a module code which is either invalid, or not available to be selected. Invalid module: ",
            uniqueChosen()[!chosenModules()$Code %in% mods$Code])
    }
  })
  
  # Bar plot for module credits
  output$cred.bar = renderPlot({
    cred.df = data.frame(Semester = rep(1 : 2, 9),
                         Credits = c(sum(subset(uniqueChosen(), Section == "Pure")$`Semester 1 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Pure")$`Semester 2 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Applied")$`Semester 1 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Applied")$`Semester 2 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Statistics")$`Semester 1 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Statistics")$`Semester 2 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Project")$`Semester 1 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Project")$`Semester 2 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Accounting")$`Semester 1 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Accounting")$`Semester 2 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Economics")$`Semester 1 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Economics")$`Semester 2 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Business")$`Semester 1 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Business")$`Semester 2 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Careers")$`Semester 1 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Careers")$`Semester 2 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Physics")$`Semester 1 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Physics")$`Semester 2 Credits`)
                                     ),
                         Section = rep(c("Pure", "Applied", "Statistics", "Project", "Accounting",
                                         "Economics", "Business", "Careers", "Physics"), each = 2)
    ) %>% filter(Credits > 0)
    ggplot(data = cred.df, aes(x = Semester, 
                               y = Credits, 
                               fill = Section)) +
      geom_col() +
      ylim(0, 80) +
      scale_x_continuous(breaks = 1 : 2, labels = c(1, 2))
  })
  
  # Table summary of modules
  
  output$tableSummary = renderDT({
    uniqueChosen() %>% select(Module, 
                             Section, 
                             `Semester 1 Credits`, 
                             `Semester 2 Credits`,
                             Exam)
  })
  
  output$examText = renderText({
    s1exam = uniqueChosen() %>% filter(grepl("Semester 1", Exam)) %>%
      mutate(cred.total = `Semester 1 Credits` + `Semester 2 Credits`)
    s2exam = uniqueChosen() %>% filter(grepl("Semester 2", Exam)) %>%
      mutate(cred.total = `Semester 1 Credits` + `Semester 2 Credits`)
    s1text = ifelse(nrow(s1exam) == 1, " exam", " exams")
    s2text = ifelse(nrow(s2exam) == 1, " exam", " exams")
    paste("<span style=\"font-size:18px\">You will have ", nrow(s1exam), s1text," in January (", sum(s1exam$cred.total), " credits).
           You will have ", nrow(s2exam), s2text, " in the summer (", sum(s2exam$cred.total), " credits).</span>")
  })
  
  output$credsWarning = renderText({
    s1.creds = sum(uniqueChosen()$`Semester 1 Credits`)
    s2.creds = sum(uniqueChosen()$`Semester 2 Credits`)
    tot.creds = s1.creds + s2.creds
    
    if(tot.creds < 120){
      paste("<span style=\"color:#E23E1A;font-size:18px\"> You haven't selected enough credits. You need 120 credits, but have only ",
             tot.creds, " credits so far. Please select an additional ",
             120 - tot.creds, " credits of modules. </span><br>")
    } else if(tot.creds > 120){
      paste("<span style=\"color:#E23E1A;font-size:18px\"> You have selected too many credits. You need 120 credits, but have selected ",
             tot.creds, " credits. Please remove ",
             tot.creds - 120, " credits of modules. </span><br>")
    } else if(s1.creds > 70){
      paste("<span style=\"color:#E23E1A;font-size:18px\"> You have selected too many credits in Semester 1, please switch ", 
             s1.creds - 70, 
             " credits of modules to Semester 2. </span><br>")
    } else if(s2.creds > 70){
      paste("<span style=\"color:#E23E1A;font-size:18px\"> You have selected too many credits in Semester 2, please switch ", 
             s1.creds - 70, 
             " credits of modules to Semester 1. </span><br>")
    } else if(s1.creds == 70 | s2.creds == 70){
      paste("<span style=\"color:#E28A1A;font-size:18px\"> You have selected to do a ", 
                  s1.creds, ":", 
                  s2.creds, 
                  " credit split. You will need DPD permission to do this. </span><br>")
    } else{
      paste("<span style=\"color:#239B56;font-size:18px\"> You have chosen the correct number of credits! </span><br>")
    }
  })
  

  
  output$clashWarning = renderText({
    clashMods = mods %>% filter(hasClash != "N")
    clashTrips = unique(t(apply(cbind(clashMods$Code, matrix(unlist(strsplit(clashMods$hasClash, ";")), byrow = T, ncol = 2)), 1, sort)))
    txt = ""
    for(i in 1 : nrow(clashTrips)){
      trip = clashTrips[i,]
      if(sum(uniqueChosen()$Code %in% trip) > 1){
        txt = paste0(txt, "You have chosen ", sum(uniqueChosen()$Code %in% trip),
                     " modules from ", paste(trip, collapse = "/"), 
                     ". Please remove modules until you have at most 1 from this group. <br>")
      }
    }
    if(txt == ""){
      txt = paste0("<span style=\"color:#239B56;font-size:18px\">", 
                   "You have no timetable clashes for your chosen modules.",
                   "</span>")
      txt
    } else{
      paste("<span style=\"color:#E23E1A;font-size:18px\">", txt, "</span>")
      }
  })
  
  output$nClashes <- reactive({
    c1 = uniqueChosen() %>% 
      filter(hasClash != "N")
    if(nrow(c1) == 0){
      return(0)
    } else{
      clash1 = character()
      clash2 = character()
      for(i in 1 : length(strsplit(c1$hasClash, ";"))){
        clash1[i] = strsplit(c1$hasClash, ";")[[i]][1]
        clash2[i] = strsplit(c1$hasClash, ";")[[i]][2]
      }
      clashMat = cbind(c1$Code, clash1, clash2)
      return(max(apply(clashMat, 1, function(x){sum(uniqueChosen()$Code %in% x)})))
    }
  })
  
  nClashes <- reactive({
    c1 = uniqueChosen() %>% 
      filter(hasClash != "N")
    if(nrow(c1) == 0){
      return(0)
    } else{
      clash1 = character()
      clash2 = character()
      for(i in 1 : length(strsplit(c1$hasClash, ";"))){
        clash1[i] = strsplit(c1$hasClash, ";")[[i]][1]
        clash2[i] = strsplit(c1$hasClash, ";")[[i]][2]
      }
      clashMat = cbind(c1$Code, clash1, clash2)
      return(max(apply(clashMat, 1, function(x){sum(uniqueChosen()$Code %in% x)})))
    }
  })
  
  outputOptions(output, "nClashes", suspendWhenHidden = FALSE)
  
  output$hasPrereqWarning = renderText({
    txt = ""
    modswithPrereqs = uniqueChosen() %>% filter(`Has Prereq` != "N")
    if(nrow(modswithPrereqs) == 0){
      txt = paste("Your chosen modules don't have any pre-requisite requirements. ")
    } else{
      for(i in 1 : nrow(modswithPrereqs)){
        prereqs = strsplit(modswithPrereqs$`Has Prereq`[i], ";")[[1]]
        txt = paste(txt, "In order to take ", 
                    modswithPrereqs$Module[i], 
                    " you must have taken ",
                    paste0(prereqs, collapse = ", "),
                    ".<br>")
      }
    }
    paste("<span style=\"color:#E28A1A;font-size:18px\">", txt, 
          "Please confirm you have studied the necessary modules before submitting your choices on S3P.", 
          "</span><br>")
  })
  
  
  output$coreqWarning = renderText({
    missingCoreqs = uniqueChosen() %>% 
      filter(HasCoreq != "N" & !HasCoreq %in% Code)
    if(nrow(missingCoreqs) == 0){
      paste("<span style=\"color:#239B56;font-size:18px\">", 
            "You are not missing any co-requisites for your chosen modules.", 
            "</span><br>")
    } else{
      txt = ""
      for(i in 1 : nrow(missingCoreqs)){
        txt = paste0(txt, "To take ", missingCoreqs$Module[i],
                     " you must also take ",
                     strsplit(missingCoreqs$HasCoreq[i], ";")[[1]],
                     ".<br>")
      }
      paste("<span style=\"color:#E23E1A;font-size:18px\">", txt, "</span><br>")
    }
  })
  
  output$nMissingCoreqs = reactive({
    uniqueChosen() %>% 
      filter(HasCoreq != "N" & !HasCoreq %in% Code) %>% 
      count
  })
  
  nMissingCoreqs = reactive({
    uniqueChosen() %>% 
      filter(HasCoreq != "N" & !HasCoreq %in% Code) %>% 
      count
  })
  
  outputOptions(output, "nMissingCoreqs", suspendWhenHidden = FALSE)
  
  output$s4Warning = renderText({
    pureMissing = !pureReq %in% uniqueChosen()$Module
    appMissing = !appReq %in% uniqueChosen()$Module
    statMissing = !statReq %in% uniqueChosen()$Module
    
    txt = ""
    if(sum(pureMissing) > 0){
      txt = paste(txt, "You will not be able to take a pure maths pathway in Stage 4. To do so you need to add ")
      for(i in 1 : sum(pureMissing)){
        txt = paste(txt, pureReq[pureMissing][i], ", ")
      }
        txt = paste(txt, " to your module selection. <br><br>")
    }
    if(sum(appMissing) > 0){
      txt = paste(txt, "You will not be able to take an applied maths pathway in Stage 4. To do so you need to add ")
      for(i in 1 : sum(appMissing)){
        txt = paste(txt, appReq[appMissing][i], ", ")
      }
      txt = paste(txt, " to your module selection. <br><br>")
    }
    if(sum(statMissing) > 0){
      txt = paste(txt, "You will not be able to take a statistics pathway in Stage 4. To do so you need to add ")
      for(i in 1 : sum(statMissing)){
        txt = paste(txt, statReq[statMissing][i], ", ")
      }
      txt = paste(txt, " to your module selection. <br><br>")
    }
    if(sum(pureMissing) > 0 & sum(appMissing) > 0 & sum(statMissing) > 0){
      txt = paste(txt, "Since you do not have the prerequisites for the pure, applied or stats pathways, 
                  you would not be able to progress into Stage 4. <br>")
    }
    if(sum(pureMissing) == 0){
      txt = paste(txt, "You will be able to take a pure maths pathway in Stage 4 based on your choices if you wish. <br><br>")
    }
    if(sum(appMissing) == 0){
      txt = paste(txt, "You will be able to take an applied maths pathway in Stage 4 based on your choices if you wish. <br><br>")
    }
    if(sum(pureMissing) == 0){
      txt = paste(txt, "You will be able to take a statistics pathway in Stage 4 based on your choices if you wish.")
    }
    paste("<span style=\"font-size:18px\">", txt, "</span>")
  })
  
  output$degreeTitle = renderText({
    mathsCreds = uniqueChosen() %>% 
      filter(Section %in% c("Pure", "Applied")) %>%
      mutate(cred.tot = `Semester 1 Credits` + `Semester 2 Credits`, .keep = "none") %>%
      sum
    statsCreds = uniqueChosen() %>% 
      filter(Section %in% c("Statistics")) %>%
      mutate(cred.tot = `Semester 1 Credits` + `Semester 2 Credits`, .keep = "none") %>%
      sum
    
      txt = paste0("Currently you have selected ", mathsCreds, " credits of maths modules, and ", 
                   statsCreds, " credits of statistics modules. ")
      
      if(input$Programme == "BMath"){
        if(mathsCreds >= 60){
          paste0("<span style=\"color:#239B56;font-size:18px\">Based on your choices you would graduate with a BSc Mathematics degree.</span>")
        } else{
          paste0("<span style=\"color:#E28A1A;font-size:18px\">
          Based on your choices you would be unable to graduate with a BSc Mathematics degree, and would need to switch to either BSc Mathematics and Statistics or BSc Statistics. <br>
                 To remain on the BSc Mathematics programme, you must select at least 50 credits of pure or applied modules (currently ", mathsCreds, ").</span>")
        }
      } else if(input$Programme == "BMathStat"){
        if(mathsCreds >= 40 & statsCreds >= 40){
          paste0("<span style=\"color:#239B56;font-size:18px\">Based on your choices you would graduate with a BSc Mathematics and Statistics degree.</span>")
        } else if(mathsCreds < 40){
          paste0("<span style=\"color:#E28A1A;font-size:18px\">
          Based on your choices you would be unable to graduate with a BSc Mathematics and Statistics degree, and would need to switch to BSc Statistics. <br>
                 To remain on the BSc Mathematics and Statistics programme, you must select at least 50 credits of pure or applied modules (currently ", mathsCreds, ").</span>")
        } else{
          paste0("<span style=\"color:#E28A1A;font-size:18px\">
          Based on your choices you would be unable to graduate with a BSc Mathematics and Statistics degree, and would need to switch to BSc Mathematics. <br>
                 To remain on the BSc Mathematics and Statistics programme, you must select at least 40 credits of statistics modules (currently ", statsCreds, ").</span>")
        }
      }
  })
  
  output$fullTable <- renderDT({
    datatable(mods %>% 
      filter(Stage == input$Stage) %>%
      select(Module, Section, `Semester 1 Credits`, `Semester 2 Credits`, Exam, hasClash),
      options = list(
        "pageLength" = 40))
  })
}

shinyApp(ui, server)
