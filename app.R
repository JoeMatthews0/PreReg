library(shinydashboard); library(ggplot2); library(DT); library(dplyr); library(stringr); library(readxl)

# Read in module data
mods.full = read_xlsx("Modules.xlsx")
mods = mods.full %>% filter(Active == "Y")

pureReq = mods %>% filter(`S4 Prereq` == "PU") %>% select(Module) %>% unlist() %>% as.character()
appReq = mods %>% filter(`S4 Prereq` == "AP") %>% select(Module) %>% unlist() %>% as.character()
statReq = mods %>% filter(`S4 Prereq` == "ST") %>% select(Module) %>% unlist() %>% as.character()

ui <- dashboardPage(
  
  # App header
  dashboardHeader(title = "Pre-Registration App"),
  
  dashboardSidebar(
    
    # User enters their degree programme
    selectInput("Programme", label = h3("Choose Your Degree Programme"), 
                choices = list("G100 BSc Mathematics" = 1, 
                               "G103 MMath Mathematics" = 2, 
                               "GG13 BSc Mathematics and Statistics" = 3,
                               "GGC3 MMathStat Mathematics and Statistics" = 4,
                               "GL11 BSc Mathematics and Economics" = 5,
                               "NG41 BSc Mathematics and Accounting" = 6,
                               "G1N3 BSc Mathematics with Finance" = 7,
                               "G1N4 BSc Mathematics with Business" = 8,
                               "G300 BSc Statistics" = 9,
                               "F300 BSc Physics" = 10,
                               "F303 MPhys Physics" = 11,
                               "F3F5 BSc Physics with Astrophysics" = 12,
                               "F3FM MPhys Physics with Astrophysics" = 13,
                               "F345 BSc Theoretical Physics" = 14,
                               "F344 MPhys Theoretical Physics" = 15),
                selected = 1),
    
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
              fluidRow(
                
                # If the user is MMath/MMathStat -- ask which specialism
                conditionalPanel(condition = "input.Stage == '4' && $.inArray(input.Programme, ['2', '4']) != -1",
                                        selectInput("Special", 
                                                    label = h3("Which Section Are You Specialising In?"),
                                                    choices = list("Pure" = 1,
                                                                   "Applied" = 2,
                                                                   "Statistics" = 3),
                                                    selected = 1),
                                        textOutput("s4SpecialPreReq")
                ),
                conditionalPanel(condition = "input.Stage == '2' && $.inArray(input.Programme, ['5', '6']) != -1",
                                 selectInput("JHPathway", 
                                             label = h3("Which pathway would you like to take?"),
                                             choices = list("Pure" = 1,
                                                            "Applied" = 2,
                                                            "Statistics" = 3),
                                             selected = 1)
                ),
                h2("Compulsory Modules"),
                
                # Show compulsory module details
                textOutput("comp.text"),
                DTOutput("comp.table")
                  ),
                 h2("Optional Modules"),
               fluidRow(
                 column(6,
                   h3("Semester 1 Modules")),
                 column(6,
                   h3("Semester 2 Modules"))
                ),
              
                # Pure maths optional modules
              conditionalPanel(condition = 'as.numeric(input.Programme) <= 9',
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
              conditionalPanel(condition = "$.inArray(input.Programme, ['6', '7', '8']) != -1",
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
              conditionalPanel(condition = "input.Programme == '5'",
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
              conditionalPanel(condition = "input.Programme == '8'",
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
              conditionalPanel(condition = 'input.Programme >= 10',
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
              conditionalPanel(condition = "input.Stage == '3'",
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
              fluidRow(
                DTOutput("tableSummary")
              ),
              # Bar chart showing credit breakdown by semester/section
                fluidRow(plotOutput("cred.bar")), 
                htmlOutput("examText"),
                htmlOutput("credTotals"),
              htmlOutput("credsWarning"),
              conditionalPanel(condition = 'output.credsOK',
                               conditionalPanel(condition = 'input.Programme %in% c(1, 3) & input.Stage == 2',
                                                htmlOutput("degreeTitle")),
                               h3("Warnings"),
                               htmlOutput("clashWarning"),
                               htmlOutput("hasPrereqWarning"),
                               htmlOutput("coreqWarning"),
                               conditionalPanel('input.Stage == 3',
                                                htmlOutput("s4Warning")
                               )            
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
    if(input$Programme %in% c(1, 3, 9)){ # If on SH Maths, you only need to choose for stages 3 and 4
      stageChoices = 3
      } else if(input$Programme %in% c(2, 4)){
        stageChoices = 3 : 4
      } else if(input$Programme %in% c(11, 13, 15)){ # Physics might need to choose any from stages 2 to 4
      stageChoices = 2 : 4
    } else{
      stageChoices = 2 : 3 # Joint honours just need stages 2 and 3
    }
    updateSelectInput(session, "Stage", label = "Which Stage Are You Selecting Modules For?", 
                             choices = stageChoices)
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
  
  compulsory <- reactive({
    if(input$Stage == 3 & input$Programme < 5){
      compCode = "SH"
    } else if(input$Stage == 4 & input$Programme < 5){
      if(input$Special == 1){
        compCode = "PU"
      } else if(input$Special == 2){
        compCode = "AP"
      } else{
        compCode = "ST"
      }
    } else if(input$Programme %in% c(5, 6)){
      if(input$JHPathway == 1){
        cc = "PJH"
      } else if(input$JHPathway == 2){
        cc = "AJH"
      } else{
        cc = "SJH"
      }
      compCode = paste0(input$Stage, cc)
    } else if(input$Programme == 7){
      compCode = "G1N3"
    } else if(input$Programme == 8){
      compCode = "G1N4"
    } else if(input$Programme == 9){
      compCode = "G300"
    } else if(input$Programme == 10){
      compCode = "F300"
    } else if(input$Programme == 11){
      compCode = "F303"
    } else if(input$Programme == 12){
      compCode = "F3F5"
    } else if(input$Programme == 13){
      compCode = "F3FM"
    } else if(input$Programme == 14){
      compCode = "F345"
    } else if(input$Programme == 15){
      compCode = "F344"
    }
    mods %>% filter(Stage == input$Stage & str_detect(Compulsory, compCode))
  })

  output$s4SpecialPreReq <- renderText({
    if(input$Special == 1){
      txt = paste0("To take the pure maths pathway you must have taken ")
      for(i in 1 : length(pureReq)){
        txt = paste(txt, pureReq[i], ", ")
      }
    } else if(input$Special == 2){
      txt = paste0("To take the applied maths pathway you must have taken ")
      for(i in 1 : length(appReq)){
        txt = paste(txt, appReq[i], ", ")
      }
    } else{
      txt = paste0("To take the statistics pathway you must have taken ")
      for(i in 1 : length(statReq)){
        txt = paste(txt, statReq[i], ", ")
      }
    }  
    paste(txt, "in Stage 3. Please make sure you've done this before selecting your Stage 4 modules.")
  })
  
  # Table for compulsory module(s)
  output$comp.table = renderDT({
    compulsory() %>% select(Module, 
                            Section, 
                            `Semester 1 Credits`, 
                            `Semester 2 Credits`,
                            Exam)
  })
  
  # Display the correct optional modules for the user programme/stage
  
  observe({
    p1.choice = subset(mods, Stage == input$Stage & 
                         Section == "Pure" & 
                         `Semester 1 Credits` > 0 &
                         !Module %in% compulsory())$Module
    p2.choice = subset(mods, Stage == input$Stage & 
                         Section == "Pure" & 
                         `Semester 2 Credits` > 0 &
                         !Module %in% compulsory())$Module
    a1.choice = subset(mods, Stage == input$Stage & 
                         Section == "Applied" & 
                         `Semester 1 Credits` > 0 &
                         !Module %in% compulsory())$Module
    a2.choice = subset(mods, Stage == input$Stage & 
                         Section == "Applied" & 
                         `Semester 2 Credits` > 0 &
                         !Module %in% compulsory())$Module
    s1.choice = subset(mods, Stage == input$Stage & 
                         Section == "Statistics" & 
                         `Semester 1 Credits` > 0 &
                         !Module %in% compulsory())$Module
    s2.choice = subset(mods, Stage == input$Stage & 
                         Section == "Statistics" & 
                         `Semester 2 Credits` > 0 &
                         !Module %in% compulsory())$Module
    phy1.choice = subset(mods, Stage == input$Stage & 
                            Section == "Physics" & 
                            `Semester 1 Credits` > 0 &
                           !Module %in% compulsory())$Module
    phy2.choice = subset(mods, Stage == input$Stage & 
                           Section == "Physics" & 
                           `Semester 2 Credits` > 0 &
                           !Module %in% compulsory())$Module
    acc1.choice = subset(mods, Stage == input$Stage & 
                           Section == "Accounting" & 
                           `Semester 1 Credits` > 0 &
                           !Module %in% compulsory())$Module
    acc2.choice = subset(mods, Stage == input$Stage & 
                           Section == "Accounting" & 
                           `Semester 2 Credits` > 0 &
                           !Module %in% compulsory())$Module
    eco1.choice = subset(mods, Stage == input$Stage & 
                           Section == "Economics" & 
                           `Semester 1 Credits` > 0 &
                           !Module %in% compulsory())$Module
    eco2.choice = subset(mods, Stage == input$Stage & 
                           Section == "Economics" & 
                           `Semester 2 Credits` > 0 &
                           !Module %in% compulsory())$Module
    bus1.choice = subset(mods, Stage == input$Stage & 
                           Section == "Business" & 
                           `Semester 1 Credits` > 0 &
                           !Module %in% compulsory())$Module
    bus2.choice = subset(mods, Stage == input$Stage & 
                           Section == "Business" & 
                           `Semester 2 Credits` > 0 &
                           !Module %in% compulsory())$Module
    c.choice = subset(mods, Stage == input$Stage & 
                        Section == "Careers"&
                        !Module %in% compulsory())$Module
    
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
                                               input$S2)))
  })
  
  uniqueChosen <- reactive({
    chosenModules() %>% distinct %>% arrange(Module)
  })
  
  output$credTotals <- renderText({
    txt = paste0("You have currently selected ",
                 totCreds(),
                 " credits of modules (",
                 sum(uniqueChosen()$`Semester 1 Credits`),
                 " credits in Semester 1, and ",
                 sum(uniqueChosen()$`Semester 2 Credits`),
                 " credits in Semester 2).")
    paste("<span style=\"font-size:18px\">", txt, "</span><br>")
  })
  
  totCreds <- reactive({
    uniqueChosen() %>%
      mutate(cred.tot = `Semester 1 Credits` + `Semester 2 Credits`, .keep = "none") %>% 
      sum
  })
  
  output$totCreds <- reactive({
    totCreds()
  })
  
  output$credsOK = reactive({
    if(totCreds() == 120 & 
       sum(uniqueChosen()$`Semester 1 Credits`) %in% 50 : 70 &
       sum(uniqueChosen()$`Semester 2 Credits`) %in% 50 : 70){
      TRUE
    } else{
      FALSE
    }
  })
  
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
    cred.df = data.frame(Semester = rep(1 : 2, 4),
                         Credits = c(sum(subset(uniqueChosen(), Section == "Pure")$`Semester 1 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Pure")$`Semester 2 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Applied")$`Semester 1 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Applied")$`Semester 2 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Statistics")$`Semester 1 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Statistics")$`Semester 2 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Project")$`Semester 1 Credits`),
                                     sum(subset(uniqueChosen(), Section == "Project")$`Semester 2 Credits`)),
                         Section = rep(c("Pure", "Applied", "Statistics", "Project"), each = 2)
    )
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
      paste("<span style=\"color:#2CE21A;font-size:18px\"> Your module selection is fine! </span><br>")
    }
  })
  
  output$clashWarning = renderText({
    txt = ""
    for(i in 1 : nrow(uniqueChosen())){ # Go through each selected module
      clashes = unlist(strsplit(uniqueChosen()$hasClash[i],";")) # Extract the modules which clash with module i
      for(j in 1 : length(clashes)){ # Go through each of the modules which clash with module i
        if(sum(clashes[j] == uniqueChosen()$Code)){ # If one of the clashing modules has been selected
          txt = paste(txt, # Append the warning message
                      clashes[j],
                      " has a timetable clash with ",
                      uniqueChosen()$Code[i],
                      ".<br>")
        }
      }
    }
      if(sum(txt == "")){
        txt = paste("You have no timetable clashes for your chosen modules. ")
      } else{
        txt = paste0("Unfortunately your selection has some timetable clashes.<br>", 
                     txt,
                     "Please remove a clashing module from your selection. ")
      }
    paste("<span style=\"font-size:18px\">", txt, "</span><br>")
  })
  
  output$hasPrereqWarning = renderText({
    txt = ""
    modswithPrereqs = uniqueChosen() %>% filter(`Has Prereq` != "N")
    if(nrow(modswithPrereqs) == 0){
    } else{
      for(i in 1 : nrow(modswithPrereqs)){
        txt = paste(txt, "In order to take ", 
                    modswithPrereqs$Module[i], 
                    " you must have taken ",
                    strsplit(modswithPrereqs$`Has Prereq`[i], ";")[[1]],
                    ".<br>")
      }
      txt = paste(txt, " Please confirm you have studied the necessary modules before submitting your choices on S3P.")
    }
    paste("<span style=\"color:#E28A1A;font-size:18px\">", txt, "</span><br>")
  })
  
  output$coreqWarning = renderText({
    missingCoreqs = uniqueChosen() %>% 
      filter(HasCoreq != "N" & !HasCoreq %in% Code)
    txt = ""
    if(nrow(missingCoreqs) == 0){
    } else{
      for(i in 1 : nrow(missingCoreqs)){
        txt = paste0(txt, "To take ", missingCoreqs$Module[i],
                     " you must also take ",
                     strsplit(missingCoreqs$HasCoreq[i], ";")[[1]],
                     ".<br>")
      }
    }
    paste("<span style=\"color:#E23E1A;font-size:18px\">", txt, "</span><br>")
  })
  
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
      
      if(input$Programme == 1){
        if(mathsCreds >= 60){
          paste0("<span style=\"color:#2CE21A;font-size:18px\">Based on your choices you would graduate with a BSc Mathematics degree.</span>")
        } else{
          paste0("<span style=\"color:#E28A1A;font-size:18px\">
          Based on your choices you would be unable to graduate with a BSc Mathematics degree, and would need to switch to either BSc Mathematics and Statistics or BSc Statistics. <br>
                 To remain on the BSc Mathematics programme, you must select at least 50 credits of pure or applied modules (currently ", mathsCreds, ").</span>")
        }
      } else if(input$Programme == 3){
        if(mathsCreds >= 40 & statsCreds >= 40){
          paste0("<span style=\"color:#2CE21A;font-size:18px\">Based on your choices you would graduate with a BSc Mathematics and Statistics degree.</span>")
        } else if(mathsCreds < 40){
          paste0("<span style=\"color:#E28A1A;font-size:18px\">
          Based on your choices you would be unable to graduate with a BSc Mathematics and Statistics degree, and would need to switch to BSc Statistics. <br>
                 To remain on the BSc Mathematics and Statistics programme, you must select at least 40 credits of pure or applied modules (currently ", mathsCreds, ").</span>")
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
