library(shinydashboard); library(ggplot2); library(DT); library(dplyr); library(stringr); library(readxl)

# Read in module data
mods.full = read_xlsx("Modules.xlsx")
mods = mods.full %>% filter(Active == "Y")
#mods = mods.full %>% filter(Active == "Y")

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
                               "G300 BSc Statistics" = 'BStat',
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
                htmlOutput("preambleText"),
                h2("Programme Regulations"),
                htmlOutput("regsText"),
                # If the user is MMath/MMathStat -- ask which specialism
                conditionalPanel(condition = "input.Stage == 4 && input.Programme == 'MMath'",
                                        selectInput("Special", 
                                                    label = h3("Which Section Would You Like To Specialise In?"),
                                                    choices = list("Pure" = 'P',
                                                                   "Applied" = 'A'),
                                                    selected = 'P'),
                                        htmlOutput("s4SpecialPreReq")
                                 ),
                conditionalPanel(condition = "input.Programme == 'MaE' || 
                                                                   input.Programme == 'MaA' ||
                                                                   (input.Stage == 2 && (input.Programme == 'MwF' ||
                                                                   input.Programme == 'MwB'))",
                                 htmlOutput("JHText"),
                                 selectInput("JHPathway",
                                             label = NULL,
                                             choices = list("Pure" = 'P',
                                                            "Applied" = 'A',
                                                            "Statistics" = 'S'),
                                             selected = 'P'),
                                 conditionalPanel(condition = "input.Programme == 'MwF' && input.Stage == 2 && input.JHPathway == 'S'",
                                                  htmlOutput("MwFStatsText"),
                                                  selectInput("MwFStats", label = NULL,
                                                              choices = list("40 Credits of Pure" = 'P',
                                                                             "20 Credits of Pure and 20 Credits of Applied" = 'PA',
                                                                             "40 Credits of Applied" = 'A'),
                                                              selected = 'P')
                                                  ),
                                 conditionalPanel(condition = "input.Programme == 'MwB'",
                                                  htmlOutput("MwBChoiceText"),
                                                  selectInput("MwBPath",
                                                              label = NULL,
                                                              choices = list("Pure" = 'Pure',
                                                                             "Pure and Applied" = 'Pure and Applied',
                                                                             "Pure and Statistics" = 'Pure and Statistics',
                                                                             "Applied" = 'Applied',
                                                                             "Applied and Statistics" = 'Applied and Statistics',
                                                                             "Statistics" = 'Statistics'),
                                                              selected = 'P')
                                                  ),
                                 conditionalPanel(condition = "input.Stage == 2 && (input.Programme == 'MaE' || input.Programme == 'MaA' || input.Programme == 'MwF')
                                                  && !(input.Programme == 'MwF' && input.JHPathway == 'S')",
                                                  htmlOutput("JHText2"),
                                      selectInput("JHPathway2",
                                                  label = NULL,
                                                  choices = list("Pure" = 'Pure',
                                                                 "Applied" = 'Applied',
                                                                 "Statistics" = 'Statistics'),
                                                  selected = 'A'  
                                                  ),
                                      conditionalPanel(condition = "input.Programme == 'MaA'",
                                                       htmlOutput("AccText"),
                                                       selectInput("ACCOption", label = NULL,
                                                                   choices = list("ACC2000 Interpreting Company Accounts" = "ACC2000",
                                                                                  "ACC2007 Corporate Finance" = "ACC2007"),
                                                                   selected = "ACC2000")
                                                       )
                                                  )
                ),
              conditionalPanel(condition = "input.Programme == 'MPwA' && input.Stage == 4",
                               htmlOutput("MPwAText"),
                               selectInput("MPwAChoice", 
                                           label = NULL,
                                           choices = list("PHY8042 Quantum Fluids"= "PHY8042",
                                                          "PHY8044 Quantum Information & Technology" = "PHY8044"),
                                           selected = "PHY8042")
              ),
              conditionalPanel(condition = 'output.compCreds < 120',
                h2("Compulsory Modules"),
                
                # Show compulsory module details
                textOutput("comp.text"),
                DTOutput("comp.table"),
                conditionalPanel(condition = "input.Programme == 'BMath' || input.Programme == 'MMath' ||
                                              input.Programme == 'BMaS' || input.Programme == 'MMaS' ||
                                              input.Programme == 'MwF' || input.Programme == 'MwB'",
                                 h2("Timetable Clashes"),
                                 htmlOutput("clashList")
                                 ),
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
                               input.Programme == 'BStat' ||
                               input.Programme == 'MaE' || 
                               input.Programme == 'MaA' ||
                               input.Programme == 'MwF' ||
                               input.Programme == 'MwB'",
                                                conditionalPanel(condition = "!(input.Stage == 3 && (input.Programme == 'MaE' ||
                                                                                 input.Programme == 'MaA') &
                                                                                (input.JHPathway == 'A' ||
                                                                                 input.JHPathway == 'S'))",
                                                                 fluidRow( 
                                                                   column(6,
                                                                          checkboxGroupInput("P1", label = "Pure Maths", 
                                                                                             choices = mods$Module)
                                                                   ),
                                                                   column(6,
                                                                          checkboxGroupInput("P2", label = "Pure Maths", 
                                                                                             choices = mods$Module)
                                                                   )
                                                                 )               
                                                                ),
                                                # Applied maths optional modules
                                                conditionalPanel(condition = "!(input.Stage == 3 && (input.Programme == 'MaE' ||
                                                                                 input.Programme == 'MaA') &
                                                                                (input.JHPathway == 'P' ||
                                                                                 input.JHPathway == 'S'))",
                                                fluidRow(
                                                  column(6,
                                                         checkboxGroupInput("A1", label = "Applied Maths", 
                                                                            choices = mods$Module)
                                                  ),
                                                  column(6,
                                                         checkboxGroupInput("A2", label = "Applied Maths", 
                                                                            choices = mods$Module)
                                                  )
                                                )
                                                ),
                                                
                                                # Statistics optional modules
                                                conditionalPanel(condition = "!(input.Stage == 3 && (input.Programme == 'MaE' ||
                                                                                 input.Programme == 'MaA') &
                                                                                (input.JHPathway == 'P' ||
                                                                                 input.JHPathway == 'A'))",
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
                                                )
                               ),
                               conditionalPanel(condition = "input.Programme == 'MaA' | input.Programme == 'MwF'",
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
                                                ),
                                                column(12,
                                                                htmlOutput("careersWarning") 
                                                                )
                                                )
                               ),
                p(""),
                               p("Alternatively copy and paste your selection here"),
                               textInput("textModules", "Enter your module choices, separated by spaces"),
                               htmlOutput("dupWarning"),
                               htmlOutput("invalidWarning"),
                               htmlOutput("stageClash")
                               ),
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
                               conditionalPanel(condition = "input.Programme == 'MaE' || input.Programme == 'MaA'",
                                                htmlOutput("JHCredsWarning")
                                                ),
                                                h3("Timetable Clashes & Co-requisite Requirements"),
                                                htmlOutput("coreqWarning"),
                                                p(""),
                                                htmlOutput("clashWarning"),
                                                conditionalPanel(condition = "output.isMissingCoreqs == 0 && output.nClashes < 2",
                                                h3("Required Pre-requisites"),
                                                htmlOutput("PreReqText"),
                                                htmlOutput("hasPrereqWarning"),
                                                conditionalPanel(condition = "input.Stage == 3 && (input.Programme == 'BMath' ||
                                                                            input.Programme == 'MMath' ||
                                                                            input.Programme == 'BMaS' ||
                                                                            input.Programme == 'MMaS' ||
                                                                            input.Programme == 'BStat')",
                                                                 h3("Requirements for Stage 4 MMath/MMathStat Programmes"),
                                                                 conditionalPanel(condition = "input.Programme == 'BMath' ||
                                                                                               input.Programme == 'BMaS' ||
                                                                                               input.Programme == 'BStat'",
                                                                                  htmlOutput("s4DPDText"),
                                                                                  p("")
                                                                                               ),
                                                                 htmlOutput("s4Warning"),
                                                                 conditionalPanel(condition = "input.Programme == 'BMath' || input.Programme == 'BMaS' || input.Programme == 'BStat'",
                                                                                  h3("Degree Title"),
                                                                                  htmlOutput("degreeTitle") 
                                                                                  )
                                                ),
                            )
                  ),
              h3("Selection Decision"),
              htmlOutput("decisionText")
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
  
  output$preambleText <- renderText({
    paste0("Welcome to the School of Maths, Stats and Physics pre-registration app. 
                  This app is designed to help you make your pre-registration choices for your modules next year.<br>
                  <b>Please note that you cannot formally submit your choices on this app, that must still be done through <a href='https://s3p.ncl.ac.uk'> S3P </a>.
                  This app is simply to help you understand which choices are possible for you, and what your next year might look like.</b><br>
                  To use the app, please select the programme you are enrolled on, and the stage you will be entering next year
                  (i.e. if you are currently in Stage 2, you should select Stage 3 since that is the stage you will be choosing modules for).<br>
                  Once you have selected your Programme and Stage, please see below for your programme regulations, compulsory modules,
                  and the optional modules you can choose from.<br>
           Once you have found a combination of modules you're happy with, <b>log in to <a href = 'https://s3p.ncl.ac.uk'> S3P </a> to formally submit your selection. 
           Module selection for the academic year 2023/2024 in <a href='https://s3p.ncl.ac.uk'> S3P </a> will be available from 9am on Friday 5th May 2023 until 8pm on Thursday 11th May 2023. </b><br>
           If you have any questions about your module choices, please email <a href='mailto:beth.mcclelland@newcastle.ac.uk'> Beth McClelland </a>. <br>
    If you encounter any issues with using the app, please email <a href='mailto:joe.matthews@newcastle.ac.uk'> Joe Matthews</a>. <br>
           If you wish to request a selection not visible through the app, this would be classed as a non-standard request and require DPD approval.
           Please complete <a href ='https://forms.office.com/e/ufY4Lh5VCn'> this form </a> to inform the DPD of your request and the rationale for it.")
  })
  
  output$regsText <- renderText({
    txt = ""
    if(input$Programme == 'BMath'){
      txt = "As part of the requirements for the G100 BSc Mathematics degree:
      <ul>
        <li> You must take MAS3091 Group Project </li>
        <li> You must choose at least 60 credits of maths modules (Pure or Applied) </li>
        <li> The remaining 50 credits can come from Pure, Applied or Statistics modules </li>
      </ul>
    "
    } else if(input$Programme == 'MMath'){
      if(input$Stage == 3){
        txt = "As part of the requirements for the G103 MMath Mathematics degree in Stage 3:
        <ul>
          <li> You must take MAS3091 Group Project </li>
          <li> You must choose at least 60 credits of maths modules (Pure or Applied) </li>
          <li> The remaining 50 credits can come from Pure, Applied or Statistics modules </li>
          <li> To take a Pure pathway at Stage 4, you must take MAS3701, MAS3702 and MAS3706 at Stage 3 </li>
          <li> To take an Applied pathway at Stage 4, you must take MAS3802, MAS3804 and MAS3809 at Stage 3 </li>
          <li> If you wish to give yourself the choice of a Pure, Applied or Statistics pathway at Stage 4
                (i.e. if you want to keep all your options open) you should select MAS3701, MAS3702, MAS3706, 
                MAS3802, MAS3804, MAS3809, MAS3902 + 40 credits of your choosing </li>
        </ul>
      " 
      } else if(input$Stage == 4){
        txt = "As part of the requirements for the G103 MMath Mathematics degree in Stage 4:
        <ul>
          <li> You must take MAS8091 MMath Project </li>
          <li> If you are specialising in Pure, you must take MAS8751 Algebraic Topology, MAS8752 Galois Theory, MAS8753 Functional Analysis and MAS8754 Measure Theory. </li>
          <li> If you are specialising in Applied, you must take MAS8810 Geophysical and Astrophysical Fluids, MAS8811 General Relativity and MAS8812 Quantum Fluids. </li>
          <li> You may choose the remaining 20 credits from any Pure, Applied or Statistics modules.
        </ul>
      " 
      }
    } else if(input$Programme == 'BMaS'){
      txt = "As part of the requirements for the GG13 BSc Mathematics and Statistics degree in Stage 3:
        <ul>
          <li> You must take MAS3091 Group Project </li>
          <li> You must choose at least 40 credits of maths modules (Pure or Applied) </li>
          <li> You must choose at least 40 credits of Statistics modules </li>
          <li> The remaining 30 credits can come from Pure, Applied or Statistics modules </li>
        </ul>
      " 
    } else if(input$Programme == 'MMaS'){
      if(input$Stage == 3){
        txt = "As part of the requirements for the GGC3 MMathStat Mathematics and Statistics degree in Stage 3:
        <ul>
          <li> You must take MAS3091 Group Project, MAS3902 Bayesian Inference and MAS3917 Stochastic Processes </li>
          <li> The remaining 90 credits can come from Pure, Applied or Statistics modules </li>
          <li> If you do not meet the requirements for fourth year, you will be given an exit award at the end of Stage 3. Your module choices will determine what this exit award is.
               If you wish to ensure you would receive BSc Mathematics and Statistics as an exit award, select at least 40 credits of Statistics modules. </li>
        </ul>
      " 
      } else if(input$Stage == 4){
        txt = "As part of the requirements for the GGC3 MMathStat Mathematics and Statistics degree in Stage 4:
        <ul>
          <li> You must take MAS8391 MMathStat Project, MAS8953 Computational Statistics, MAS8954 Advanced Topics in Statistics A and MAS8955 Advanced Topics in Statistics B </li>
          <li> You may choose the remaining 20 credits from any Pure, Applied or Statistics modules.
        </ul>
      " 
      }
    } else if(input$Programme == 'MaE'){
      if(input$Stage == 2){
        txt = "As part of the requirements for the GL11 BSc Mathematics and Economics degree in Stage 2:
        <ul>
          <li> You must take ECO2101 Microeconomic Analysis and ECO2102 Macroeconomic Analysis. </li>
          <li> You must choose a primary pathway (Pure, Applied or Statistics). You will take 40 credits of modules from this pathway in Stage 2,
                and will only be able to take modules from your chosen section in Stage 3 for the maths half of your degree 
                (e.g. if you choose Applied as your primary pathway, you will only be able to take Applied modules in stage 3, and won't be able to take Pure or Statistics modules). </li>
          <li> You must choose a secondary pathway, which will add 20 credits of modules from that section to your selection in Stage 2, but does not affect your options in Stage 3. </li>
        </ul>
      " 
      } else if(input$Stage == 3){
        txt = "As part of the requirements for the GL11 BSc Mathematics and Economics degree in Stage 3:
        <ul>
          <li> You must take ECO3001 Advanced Microeconomics. </li>
          <li> You must choose 40 additional credits of Economics modules (to make 60 credits in total when combined with ECO3001) </li>
          <li> Based on your pathway chosen in Stage 2, you will have 20 credits of compulsory maths/stats modules, and must choose the remaining 40 credits of modules from your pathway. </lii>
        </ul>
      "
      }
    } else if(input$Programme == 'MaA'){
      if(input$Stage == 2){
        txt = "As part of the requirements for the NG41 BSc Mathematics and Accounting degree in Stage 2:
          <ul>
          <li> You must take ACC2003 Financial Control and ACC2005 Intermediate Financial Accounting. </li>
          <li> For your remaining 20 credits of Accounting, you must choose either ACC2000 Interpreting Company Accounts or ACC2007 Corporate Finance </li>
          <li> You must choose a primary pathway (Pure, Applied or Statistics). You will take 40 credits of modules from this pathway in Stage 2,
        and will only be able to take modules from your chosen section in Stage 3 for the maths half of your degree 
        (e.g. if you choose Applied as your primary pathway, you will only be able to take Applied modules in stage 3, and won't be able to take Pure or Statistics modules). </li>
          <li> You must choose a secondary pathway, which will add 20 credits of modules from that section to your selection in Stage 2, but does not affect your options in Stage 3. </li>
        </ul>
      "
      } else if(input$Stage == 3){
        txt = "As part of the requirements for the NG41 BSc Mathematics and Accounting degree in Stage 3:
          <ul>
          <li> You must take ACC2003 Financial Control and ACC2005 Intermediate Financial Accounting. </li>
          <li> For your remaining 20 credits of Accounting, you must choose either ACC2000 Interpreting Company Accounts or ACC2007 Corporate Finance </li>
          <li> You must choose a primary pathway (Pure, Applied or Statistics). You will take 40 credits of modules from this pathway in Stage 2,
        and will only be able to take modules from your chosen section in Stage 3 for the maths half of your degree 
        (e.g. if you choose Applied as your primary pathway, you will only be able to take Applied modules in stage 3, and won't be able to take Pure or Statistics modules). </li>
          <li> You must choose a secondary pathway, which will add 20 credits of modules from that section to your selection in Stage 2, but does not affect your options in Stage 3. </li>
        </ul>
      "
      }
    } else if(input$Programme == 'MwF'){
      if(input$Stage == 2){
        txt = "As part of the requirements for the G1N3 BSc Mathematics with Finance degree in Stage 2:
          <ul>
          <li> You must take ACC2000 Interpreting Company Accounts, ACC2007 Corporate Finance, MAS2901 Introduction to Statistical Inference and MAS2902 Stochastic Modelling. </li>
          <li> You must choose a primary pathway (Pure, Applied or Statistics). You will take 40 credits of modules from this pathway in Stage 2,
        and will only be able to take modules from your chosen section in Stage 3 for the maths half of your degree 
        (e.g. if you choose Applied as your primary pathway, you will only be able to take Applied modules in stage 3, and won't be able to take Pure or Statistics modules). </li>
          <li> If you choose a Statistics pathway, you will split the remaining 40 credits between Pure and Applied modules (in either a 40:0, 20:20 or 0:40 split). </li>
          <li> If you choose a Pure or Applied pathway, you will choose a secondary pathway from the options you didn't select (e.g. if you chose a Pure pathway, you will choose either 
          an Applied or Statistics secondary pathway) which will contribute 40 credits of modules to your Stage 2 selection, but won't affect your Stage 3 choices. </li>
        </ul>
      "
      } else if(input$Stage == 3){
        txt = "As part of the requirements for the G1N3 BSc Mathematics with Finance degree in Stage 3:
          <ul>
          <li> You must take ACC3000 Case Studies in Accounting, Finance and Business, ACC3006 International Financial Management and MAS3904 Stochastic Financial Modelling. </li>
          <li> The remaining 70 credits can be made up of any Pure, Applied or Statistics modules. </li>
          </ul>
      "
      }
    } else if(input$Programme == 'MwB'){
      if(input$Stage == 2){
        txt = "As part of the requirements for the G1N4 BSc Mathematics with Business degree in Stage 2:
        <ul>
          <li> You must take ACC2000 Interpreting Company Accounts and BUS2000 Human Resource Management </li>
          <li> You must choose a primary pathway (Pure, Applied or Statistics) which will contribute 40 credits of modules to your Stage 2 selection 
          (but won't impact your Stage 3 choices) </li>
          <li> You will split the remaining 40 credits among the 2 sections you didn't choose for your primary pathway (e.g. if you chose an Applied pathway, 
          you would split the remaining 40 credits between Pure and Statistics modules) in either a 40:0, 20:20 or 0:40 split. </li>
        </ul>
      " 
      } else if(input$Stage == 3){
        txt = "As part of the requirements for the G1N4 BSc Mathematics with Business degree in Stage 3:
        <ul>
          <li> You must take ACC3000 Case Studies in Accounting, Finance and Business and BUS3000 Enterprise & Entrepeneurship with Lean Innovation </li>
          <li> The remaining 80 credits can be made up of any Pure, Applied or Statistics modules. </li>
        </ul>
      " 
      } 
    } else if(input$Programme == 'BStat'){
      txt = "As part of the requirements for the G300 BSc Statistics degree:
        <ul>
          <li> You must take MAS3091 Group Project </li>
          <li> You must choose at least 60 credits of Statistics modules </li>
          <li> The remaining 50 credits can come from Pure, Applied or Statistics modules </li>
        </ul>
      " 
    } else if(input$Programme == 'BPhys'){
      if(input$Stage == 2){
        txt = "As part of the requirements for the F300 BSc Physics degree in Stage 2 you will take 120 credits of compulsory Physics modules."
      } else if(input$Stage == 3){
        txt = "As part of the requirements for the F300 BSc Physics degree in Stage 3 you will take 80 credits of compulsory Physics modules and will choose 40 credits of
              optional Physics modules."
      } 
    } else if(input$Programme == 'BPwA'){
      if(input$Stage == 2){
        txt = "As part of the requirements for the F3F5 BSc Physics with Astrophysics degree in Stage 2 you will take 120 credits of compulsory Physics modules."
      } else if(input$Stage == 3){
        txt = "As part of the requirements for the F3F5 BSc Physics with Astrophysics degree in Stage 3 you will take 110 credits of compulsory Physics modules and choose
               one 10 credit optional module."
      } 
    } else if(input$Programme == 'BTP'){
      if(input$Stage == 2){
        txt = "As part of the requirements for the F345 BSc Theoretical Physics degree in Stage 2 you will take 120 credits of compulsory Physics modules."
      } else if(input$Stage == 3){
        txt = "As part of the requirements for the F345 BSc Theoretical Physics degree in Stage 3 you will take 90 credits of compulsory Physics modules and choose 30
               credits of optional modules."
      } 
    } else if(input$Programme == 'MPhys'){
      if(input$Stage == 2){
        txt = "As part of the requirements for the F303 MPhys Physics degree in Stage 2 you will take 120 credits of compulsory Physics modules."
      } else if(input$Stage == 3){
        txt = "As part of the requirements for the F303 MPhys Physics degree in Stage 3 you will take 80 credits of compulsory Physics modules
            and will choose 40 credits of optional Physics modules."
      } else if(input$Stage == 4){
        txt = "As part of the requirements for the F303 MPhys Physics degree in Stage 4 you will take 120 credits of compulsory Physics modules."
      }
    } else if(input$Programme == 'MPwA'){
      if(input$Stage == 2){
        txt = "As part of the requirements for the F3FM MPhys Physics with Astrophysics degree in Stage 2 you will take 120 credits of compulsory Physics modules."
      } else if(input$Stage == 3){
        txt = "As part of the requirements for the F3FM MPhys Physics with Astrophysics degree in Stage 3 you will take 90 credits of compulsory Physics modules
             and will choose 30 credits of optional Physics modules."
      } else if(input$Stage == 4){
        txt = "As part of the requirements for the F3FM MPhys Physics with Astrophysics degree in Stage 4 you will take 100 credits of compulsory Physics modules
             and will choose one optional 20 credit Physics module."
      }
    } else if(input$Programme == 'MTP'){
      if(input$Stage == 2){
        txt = "As part of the requirements for the F344 MPhys Theoretical Physics degree in Stage 2 you will take 120 credits of compulsory Physics modules."
      } else if(input$Stage == 3){
        txt = "As part of the requirements for the F344 MPhys Theoretical Physics degree in Stage 3 you will take 80 credits of compulsory Physics modules and 40 credits of 
        optional Physics modules."
      } else if(input$Stage == 4){
        txt = "As part of the requirements for the F344 MPhys Theoretical Physics degree in Stage 4 you must take PHY8050 Extended Project - MPhys Theoretical Physics, and 80 credits of
         optional Physics modules."
      }
    }
    txt = paste0("<span style=\"font-size:14px\">", txt, "</span>")
  })
  
  output$JHText <- renderText({
    if(input$Stage == 2){
      if(input$Programme %in% c('MaE', 'MaA')){
        paste0("<span style=\"font-size:18px\">", "Which pathway would you like to take?", "</span><br>",
               "<span style=\"font-size:14px\">", "You will take 40 credits of modules from this section in Stage 2, and 60 credits from this section (and only this section) in Stage 3. <br> 
               If you wish to see what the Stage 3 options are like for the different pathways, select Stage 3 on the drop down menu in the side bar.", "</span>")
      } else{
        paste0("<span style=\"font-size:18px\">", "Which pathway would you like to take?", "</span><br>",
               "<span style=\"font-size:14px\">", "You will take 40 credits of modules from this section in Stage 2. This will not affect your Stage 3 options.", "</span>")
      }
    } else if(input$Stage == 3){
      paste0("<span style=\"font-size:18px\">", "Which pathway are you on?", "</span><br>",
             "<span style=\"font-size:14px\">", "This was the pathway you chose when entering Stage 2, and took 40 credits of in Stage 2.", "</span>")
    }
  })
  
  observe({
    if(input$JHPathway == 'P'){
      JH2Choices = c('Applied', 'Statistics')
      MwBChoices = c('Applied', 'Applied and Statistics', 'Statistics')
    } else if(input$JHPathway == 'A'){
      JH2Choices = c('Pure', 'Statistics')
      MwBChoices = c('Pure', 'Pure and Statistics', 'Statistics')
    } else if(input$JHPathway == 'S'){
      JH2Choices = c('Pure', 'Applied')
      MwBChoices = c('Pure', 'Pure and Applied', 'Applied')
    }
    updateSelectInput(session, "JHPathway2", label = NULL, choices = JH2Choices)
    updateSelectInput(session, "MwBPath", label = NULL, choices = MwBChoices)
  })
  
  output$JHText2 <- renderText({
    txt = paste0("<span style=\"font-size:18px\">", "Which secondary pathway would you like to take?", "</span><br>")
    if(input$Programme == 'MwF'){
      paste0(txt, "<span style=\"font-size:14px\">", "This will determine 20 credits of your Stage 2 modules, but will not affect your Stage 3 choices.", "</span>")
    } else{
      paste0(txt, "<span style=\"font-size:14px\">", "You will take 20 credits from your secondary pathway in Stage 2, but can only take modules from your primary pathway in Stage 3.", "</span>")
    }
  })
  
  output$AccText <- renderText({
    paste0("<span style=\"font-size:18px\">", "Which 20 credit Accounting module would you like to take?", "</span>")
  })
  
  output$MwBChoiceText <- renderText({
    paste0("<span style=\"font-size:18px\">", "How would you like to split your remaining 40 credits of modules?", "</span>")
  })
  
  output$MPwAText <- renderText({
    paste0("<span style=\"font-size:18px\">", "Which optional Physics module would you like to take?", "</span>")
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
  
  f2 = function(cc){
    tmp = sapply(1 : length(cc), function(i){str_detect(mods$Forbidden, cc[i])})                
    return(apply(tmp, 1, sum))
  }
  
  compulsory <- reactive({
    cc = NULL
    compCode = "Misc"
    if(input$Stage == 3 & input$Programme %in% c('BMath', 'MMath', 'BMaS', 'MMaS', 'BStat')){
      compCode = "SH"
    } else if(input$Stage == 4 & input$Programme == 'MMath'){
      if(input$Special == 'P'){
        compCode = "PU"
      } else if(input$Special == 'A'){
        compCode = "AP"
        } 
    } else if(input$Stage == 4 & input$Programme == 'MMaS'){
      compCode = "ST"
    } else if(input$Programme %in% c('MaE', 'MaA') || (input$Stage == 2 && input$Programme %in% c('MwF', 'MwB'))){
      if(input$JHPathway == 'P'){
        cc = "PJH"
      } else if(input$JHPathway == 'A'){
        cc = "AJH"
      } else if(input$JHPathway == 'S'){
        cc = "SJH"
      }
    }
      if(input$Stage == 2 && input$Programme %in% c('MaE', 'MaA', 'MwF')){
        if(input$Programme == 'MwF' & input$JHPathway == 'S'){
          if(input$MwFStats == 'P'){
            cc = c(cc, "MWFSP")
          } else if(input$MwFStats == 'PA'){
            cc = c(cc, "MWFSB")
          } else if(input$MwFStats == 'A'){
            cc = c(cc, "MWFSA")
          }
        } else{
          if(input$JHPathway2 == 'Pure'){
          cc = c(cc, "P2JH")
        } else if(input$JHPathway2 == 'Applied'){
          cc = c(cc, "A2JH")
        } else if(input$JHPathway2 == 'Statistics'){
          cc = c(cc, "S2JH")
          }
        }
        if(input$Programme == 'MaA'){
          if(input$ACCOption == "ACC2000"){
            cc = c(cc, "ACC1")
          } else if(input$ACCOption == "ACC2007"){
            cc = c(cc, "ACC2")
          }
        }
      }
      if(input$Stage == 2 && input$Programme == "MwB"){
        if(input$MwBPath == 'Pure'){
          cc = c(cc, "MWBJP")
        } else if(input$MwBPath == 'Pure and Applied'){
          cc = c(cc, "MWBPA")
        } else if(input$MwBPath == 'Applied'){
          cc = c(cc, "MWBJA")
        } else if(input$MwBPath == 'Applied and Statistics'){
          cc = c(cc, "MWBAS")
        } else if(input$MwBPath == 'Statistics'){
          cc = c(cc, "MWBJS")
        } else if(input$MwBPath == 'Pure and Statistics'){
          cc = c(cc, "MWBPS")
        }
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
      if(input$Programme == 'BPhys'){
      compCode = "F300"
    } else if(input$Programme == 'MPhys'){
      compCode = "F303"
    } else if(input$Programme == 'BPwA'){
      compCode = "F3F5"
    } else if(input$Programme == 'MPwA'){
      compCode = "F3FM"
      if(input$Stage == 4){
        if(input$MPwAChoice == "PHY8042"){
          compCode = c(compCode, "MPWA1")
        } else{
          compCode = c(compCode, "MPWA2")
        }
      }
    } else if(input$Programme == 'BTP'){
      compCode = "F345"
    } else if(input$Programme == 'MTP'){
      compCode = "F344"
    }
    mods %>% filter(Stage == input$Stage & f(compCode))
  })
  
  output$MwFStatsText <- renderText({
    paste0("<span style=\"font-size:16px\"> 
           For your remaining 40 credits of maths modules, please indicate how you would like these to be split between pure and applied modules. </span>")
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
    paste(txt, "in Stage 3. <br> Please make sure you've done this before selecting your Stage 4 modules. <br>
          If you haven't completed the Stage 3 pre-requisites for the section you wish to specialise in in Stage 4, 
          please complete <a href ='https://forms.office.com/e/ufY4Lh5VCn'> this form </a> to inform the DPD of your request.")
  })
  
  # Table for compulsory module(s)
  output$comp.table = renderDT({
    compulsory() %>% select(Module, 
                            Section, 
                            `Semester 1 Credits`, 
                            `Semester 2 Credits`,
                            Exam)
  })
  
  output$compCreds = reactive({
    sum(compulsory()$`Semester 1 Credits`) + sum(compulsory()$`Semester 2 Credits`)
  })
  
  outputOptions(output, "compCreds", suspendWhenHidden = FALSE)
  
  output$clashList = renderText({
    if(input$Stage == 3){
      paste0("<span style=\"font-size:14px\">Certain modules are grouped together on the timetable, and as such to avoid a timetable clash <b>you cannot select more than 1 module from each group.</b> <br>
           These groups are:
           <ul>
            <li> MAS3701, MAS3808 and MAS3905 </li>
            <li> MAS3706, MAS3805 and MAS3907 </li>
            <li> MAS3709, MAS3802 and MAS3918 </li>
           </ul> </span>")
    } else if(input$Stage == 4){
      paste0("<span style=\"font-size:14px\">Certain modules are grouped together on the timetable, and as such to avoid a timetable clash <b>you cannot select more than 1 module from each group.</b> <br>
           These groups are:
           <ul>
            <li> MAS8701, MAS8808 and MAS8905 </li>
            <li> MAS8706 and MAS8907 </li>
            <li> MAS8709 and MAS8918 </li>
           </ul> </span>")
    }
  })
  
  nForbidden = reactive({
    cc = NULL
    compCode = "Misc"
    if(input$Stage == 3 & input$Programme %in% c('BMath', 'MMath', 'BMaS', 'MMaS', 'BStat')){
      compCode = "SH"
    } else if(input$Stage == 4 & input$Programme == 'MMath'){
      if(input$Special == 'P'){
        compCode = "PU"
      } else if(input$Special == 'A'){
        compCode = "AP"
        }
      }
    else if(input$Stage == 4 & input$Programme == 'MMaS'){
      compCode = "ST"
    } else if(input$Programme %in% c('MaE', 'MaA') || (input$Stage == 2 && input$Programme %in% c('MwF', 'MwB'))){
      if(input$JHPathway == 'P'){
        cc = "PJH"
      } else if(input$JHPathway == 'A'){
        cc = "AJH"
      } else if(input$JHPathway == 'S'){
        cc = "SJH"
      }
    }
    if(input$Stage == 2 && input$Programme %in% c('MaE', 'MaA', 'MwF')){
      if(input$Programme == 'MwF' & input$JHPathway == 'S'){
        if(input$MwFStats == 'P'){
          cc = c(cc, "MWFSP")
        } else if(input$MwFStats == 'PA'){
          cc = c(cc, "MWFSB")
        } else if(input$MwFStats == 'A'){
          cc = c(cc, "MWFSA")
        }
      } else{
        if(input$JHPathway2 == 'Pure'){
          cc = c(cc, "P2JH")
        } else if(input$JHPathway2 == 'Applied'){
          cc = c(cc, "A2JH")
        } else if(input$JHPathway2 == 'Statistics'){
          cc = c(cc, "S2JH")
        }
      }
      if(input$Programme == 'MaA'){
        if(input$ACCOption == "ACC2000"){
          cc = c(cc, "ACC1")
        } else if(input$ACCOption == "ACC2007"){
          cc = c(cc, "ACC2")
        }
      }
    }
    if(input$Stage == 2 && input$Programme == "MwB"){
      if(input$MwBPath == 'Pure'){
        cc = c(cc, "MWBJP")
      } else if(input$MwBPath == 'Pure and Applied'){
        cc = c(cc, "MWBPA")
      } else if(input$MwBPath == 'Applied'){
        cc = c(cc, "MWBJA")
      } else if(input$MwBPath == 'Applied and Statistics'){
        cc = c(cc, "MWBAS")
      } else if(input$MwBPath == 'Statistics'){
        cc = c(cc, "MWBJS")
      } else if(input$MwBPath == 'Pure and Statistics'){
        cc = c(cc, "MWBPS")
      }
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
    if(input$Programme == 'BPhys'){
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
    mods %>% filter(!f2(compCode))
  })
  
  # Display the correct optional modules for the user programme/stage
  
  observe({
    p1.choice = subset(nForbidden(), Stage == input$Stage & 
                         Section == "Pure" & 
                         `Semester 1 Credits` > 0 &
                         !Module %in% compulsory()$Module)$Module
    p2.choice = subset(nForbidden(), Stage == input$Stage & 
                         Section == "Pure" & 
                         `Semester 2 Credits` > 0 &
                         !Module %in% compulsory()$Module)$Module
    a1.choice = subset(nForbidden(), Stage == input$Stage & 
                         Section == "Applied" & 
                         `Semester 1 Credits` > 0 &
                         !Module %in% compulsory()$Module)$Module
    a2.choice = subset(nForbidden(), Stage == input$Stage & 
                         Section == "Applied" & 
                         `Semester 2 Credits` > 0 &
                         !Module %in% compulsory()$Module)$Module
    s1.choice = subset(nForbidden(), Stage == input$Stage & 
                         Section == "Statistics" & 
                         `Semester 1 Credits` > 0 &
                         !Module %in% compulsory()$Module)$Module
    s2.choice = subset(nForbidden(), Stage == input$Stage & 
                         Section == "Statistics" & 
                         `Semester 2 Credits` > 0 &
                         !Module %in% compulsory()$Module)$Module
    phy1.choice = subset(nForbidden(), Stage == input$Stage & 
                            Section == "Physics" & 
                            `Semester 1 Credits` > 0 &
                           !Module %in% compulsory()$Module)$Module
    phy2.choice = subset(nForbidden(), Stage == input$Stage & 
                           Section == "Physics" & 
                           `Semester 2 Credits` > 0 &
                           !Module %in% compulsory()$Module)$Module
    acc1.choice = subset(nForbidden(), Stage == input$Stage & 
                           Section == "Accounting" & 
                           `Semester 1 Credits` > 0 &
                           !Module %in% compulsory()$Module)$Module
    acc2.choice = subset(nForbidden(), Stage == input$Stage & 
                           Section == "Accounting" & 
                           `Semester 2 Credits` > 0 &
                           !Module %in% compulsory()$Module)$Module
    eco1.choice = subset(nForbidden(), Stage == input$Stage & 
                           Section == "Economics" & 
                           `Semester 1 Credits` > 0 &
                           !Module %in% compulsory()$Module)$Module
    eco2.choice = subset(nForbidden(), Stage == input$Stage & 
                           Section == "Economics" & 
                           `Semester 2 Credits` > 0 &
                           !Module %in% compulsory()$Module)$Module
    c.choice = subset(nForbidden(), Stage == input$Stage & 
                        Section == "Careers" &
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
    updateCheckboxGroupInput(session, "ACC1", label = "Accounting",
                             choices = acc1.choice)
    updateCheckboxGroupInput(session, "ACC2", label = "Accounting",
                             choices = acc2.choice)
    updateCheckboxGroupInput(session, "ECO1", label = "Economics",
                             choices = eco1.choice)
    updateCheckboxGroupInput(session, "ECO2", label = "Economics",
                             choices = eco2.choice)
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
  
  s1Creds <- reactive({
    sum(uniqueChosen()$`Semester 1 Credits`)
  })
  
  output$s1Creds <- reactive({
    s1Creds()
  })
  
  s2Creds <- reactive({
    sum(uniqueChosen()$`Semester 2 Credits`)
  })
  
  output$s2Creds <- reactive({
    s2Creds()
  })
  
  totCreds <- reactive({
    s1Creds() + s2Creds()
  })
  
  output$totCreds <- reactive({
    totCreds()
  })
  
  outputOptions(output, "s1Creds", suspendWhenHidden = FALSE)
  outputOptions(output, "s2Creds", suspendWhenHidden = FALSE)
  outputOptions(output, "totCreds", suspendWhenHidden = FALSE)
  
  output$careersWarning <- renderText({
    paste0("<span style=\"font-size:14px\"> In order to select Careers modules, you must first receive DPD permission (via <a href ='https://forms.office.com/e/ufY4Lh5VCn'> this form </a>) and are recommended to make your selection as 
           close to the opening of module selection on S3P as possible to secure your place given the limited availability on these modules for students across the whole university.</span>")
  })
  
  output$dupWarning <- renderText({
    modCounts = table(chosenModules()$Code)
    if(max(modCounts > 1)){
      paste("<span style=\"color:#E28A1A;font-size:18px\"> Warning: You have chosen the same module(s) more than once. Duplicated modules: ", names(which(modCounts > 1)), "</span><br>")
    } 
  })
  
  output$invalidWarning <- renderText({
    txtCodes = strsplit(input$textModules, " ")[[1]]
    if(sum(!txtCodes %in% mods$Code) > 0){
      paste("<span style=\"color:#E28A1A;font-size:18px\">Warning: You have selected a module code which is either invalid, or not available to be selected. Invalid module: ",
            txtCodes[!txtCodes %in% mods$Code], "</span><br>")
    }
  })
  
  output$stageClash <- renderText({
    stages = vector()
    for(i in 1 : length(uniqueChosen()$Code)){
      stages[i] = substr(uniqueChosen()$Code[i], 4, 4)
    }
    if(length(unique(stages)) > 1){
      paste("<span style=\"color:#E28A1A;font-size:18px\">Warning: You have selected modules across multiple stages. Stages selected: ", paste(unique(stages), collapse = ", "), 
            ".<br>Please check for typos in your module codes! </span>")
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
                  " credit split. You will need DPD permission to do this. Please complete <a href ='https://forms.office.com/e/ufY4Lh5VCn'> this form </a> to inform the DPD of your request.</span><br>")
    } else{
      paste("<span style=\"color:#239B56;font-size:18px\"> You have chosen the correct number of credits! </span><br>")
    }
  })
  
  output$JHCredsWarning = renderText({
    mathsCreds = uniqueChosen() %>% 
      filter(Section %in% c("Pure", "Applied", "Statistics")) %>%
      mutate(cred.tot = `Semester 1 Credits` + `Semester 2 Credits`, .keep = "none") %>% 
      sum
    NUBSCreds = uniqueChosen() %>% 
      filter(Section %in% c("Accounting", "Economics", "Business")) %>%
      mutate(cred.tot = `Semester 1 Credits` + `Semester 2 Credits`, .keep = "none") %>% 
      sum
    if(mathsCreds == 60 & NUBSCreds == 60){
      txt = ""
    } else if(input$Programme == "MaE"){
      txt = paste0("<span style=\"color:#E23E1A;font-size:18px\"> You need to select 60 credits of maths/stats modules (currently ", 
                   mathsCreds, 
                   ") and 60 credits of Economics modules (currently ",
                   NUBSCreds, 
                   ").</span>")
    } else if(input$Programme == "MaA"){
      txt = paste0("<span style=\"color:#E23E1A;font-size:18px\"> You need to select 60 credits of maths/stats modules (currently ", 
                   mathsCreds, 
                   ") and 60 credits of Accounting modules (currently ",
                   NUBSCreds, 
                   ").</span>")
    }
  })
  
  output$MSPMods = reactive({
    mathsCreds = uniqueChosen() %>% 
      filter(Section %in% c("Pure", "Applied", "Statistics")) %>%
      mutate(cred.tot = `Semester 1 Credits` + `Semester 2 Credits`, .keep = "none") %>% 
      sum
  })
  
  output$NUBSMods = reactive({
    mathsCreds = uniqueChosen() %>% 
      filter(Section %in% c("Accounting", "Economics", "Business")) %>%
      mutate(cred.tot = `Semester 1 Credits` + `Semester 2 Credits`, .keep = "none") %>% 
      sum
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
  
  output$nClashes <- reactive({
    nClashes()
  })

  outputOptions(output, "nClashes", suspendWhenHidden = FALSE)
  
  output$PreReqText <- renderText({
    paste0("<span style=\"font-size:16px\">", 
           em("Please confirm you have taken the required pre-requisites for your choices before submitting them on S3P."), 
           "</span><br>")
  })
  
  output$hasPrereqWarning = renderText({
    txt = ""
    modswithPrereqs = uniqueChosen() %>% filter(`Has Prereq` != "N")
    if(nrow(modswithPrereqs) == 0){
      txt = paste("<span style=\"font-size:18px\"> Your chosen modules don't have any pre-requisite requirements. </span>")
    } else{
      for(i in 1 : nrow(modswithPrereqs)){
        prereqs = paste0(unlist(strsplit(modswithPrereqs$`Has Prereq`[i], ";")[[1]]), collapse = ",")
        txt = paste(txt, "In order to take ", 
                    modswithPrereqs$Module[i], 
                    " you must have taken ",
                    paste0(prereqs, collapse = ", "),
                    ".<br>")
      }
      txt = paste0("<span style=\"color:#E28A1A;font-size:18px\">", txt,
                   "</span>")
    }
    txt
  })
  
  output$coreqWarning = renderText({
    hasCoreqs = uniqueChosen() %>% 
      filter(HasCoreq != "N") 
    nMissing = 0
    txt = ""
    if(nrow(hasCoreqs) > 0){
      for(i in 1 : nrow(hasCoreqs)){
        cs = strsplit(hasCoreqs$HasCoreq[i], ";")[[1]]
        if(sum(uniqueChosen()$Code %in% cs) != length(cs)){
          nMissing = nMissing + 1
          txt = c(txt, "In order to take ", hasCoreqs$Module[i] , "you must also take ", paste(cs[!cs %in% uniqueChosen()$Code], collapse = " & "), ". <br>")
        }
      }
    }
    if(nMissing == 0){
      paste0("<span style=\"color:#239B56;font-size:18px\">", 
             "Your selection is not missing any co-requisites.",
             "</span>")
    } else{
      paste("<span style=\"color:#E23E1A;font-size:18px\">", txt, "</span>")
    }
  })
  
  isMissingCoreqs = reactive({
    hasCoreqs = uniqueChosen() %>% 
      filter(HasCoreq != "N") 
    nMissing = 0
    if(nrow(hasCoreqs) > 0){
      for(i in 1 : nrow(hasCoreqs)){
        cs = strsplit(hasCoreqs$HasCoreq[i], ";")[[1]]
        if(sum(uniqueChosen()$Code %in% cs) != length(cs)){
          nMissing = nMissing + 1
        }
      }
    }
    nMissing
  })
  
  output$isMissingCoreqs = reactive({
    isMissingCoreqs()
  })
  
  outputOptions(output, "isMissingCoreqs", suspendWhenHidden = FALSE)
  
  output$s4DPDText = renderText({
    paste0("<span style=\"font-size:16px\"><em> With DPD permission, some students may transfer onto the MMath or MMathStat programmes. 
           Please see below for if this would be possible based on your current choices.</em></span><br>")
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
    if(sum(statMissing) == 0){
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
                   statsCreds, " credits of statistics modules. <br>")
      if(input$Programme == "BMath"){
        if(mathsCreds >= 60){
          txt = paste0("<span style=\"color:#239B56;font-size:18px\">", txt, "Based on your choices you would graduate with a BSc Mathematics degree.</span>")
        } else if(mathsCreds >= 40 && statsCreds >= 40){
          txt = paste0("<span style=\"color:#E28A1A;font-size:18px\">", txt, "
          Based on your choices you would be unable to graduate with a BSc Mathematics degree, and would need to switch to BSc Mathematics and Statistics. <br>
                 To remain on the BSc Mathematics programme, you must select at least 60 credits of pure or applied modules (currently ", mathsCreds, ").</span>")
        } else if(statsCreds >= 40){
          txt = paste0("<span style=\"color:#E28A1A;font-size:18px\">", txt, "
          Based on your choices you would be unable to graduate with a BSc Mathematics degree, and would need to switch to BSc Statistics. <br>
                 To remain on the BSc Mathematics programme, you must select at least 60 credits of pure or applied modules (currently ", mathsCreds, ").</span>")
        }
      } else if(input$Programme == "BMaS"){
        if(mathsCreds >= 40 & statsCreds >= 40){
          txt = paste0("<span style=\"color:#239B56;font-size:18px\">", txt, "Based on your choices you would graduate with a BSc Mathematics and Statistics degree.</span>")
        } else if(mathsCreds < 40){
          txt = paste0("<span style=\"color:#E28A1A;font-size:18px\">", txt, "
          Based on your choices you would be unable to graduate with a BSc Mathematics and Statistics degree, and would need to switch to BSc Statistics. <br>
                 To remain on the BSc Mathematics and Statistics programme, you must select at least 60 credits of pure or applied modules (currently ", mathsCreds, ").</span>")
        } else if(statsCreds < 40){
          txt = paste0("<span style=\"color:#E28A1A;font-size:18px\">", txt, "
          Based on your choices you would be unable to graduate with a BSc Mathematics and Statistics degree, and would need to switch to BSc Mathematics. <br>
                 To remain on the BSc Mathematics and Statistics programme, you must select at least 40 credits of statistics modules (currently ", statsCreds, ").</span>")
        }
      } else if(input$Programme == "BStat"){
        if(statsCreds >= 60){
          txt = paste0("<span style=\"color:#239B56;font-size:18px\">", txt, "Based on your choices you would graduate with a BSc Statistics degree.</span>")
        } else if(statsCreds >= 40 && mathsCreds >= 40){
          txt = paste0("<span style=\"color:#E28A1A;font-size:18px\">", txt, "
          Based on your choices you would be unable to graduate with a BSc Statistics degree, and would need to switch to BSc Mathematics and Statistics. <br>
                 To remain on the BSc Statistics programme, you must select at least 60 credits of statistics modules (currently ", statsCreds, ").</span>")
        } else if(mathsCreds >= 60){
          txt = paste0("<span style=\"color:#E28A1A;font-size:18px\">", txt, "
          Based on your choices you would be unable to graduate with a BSc Statistics degree, and would need to switch to BSc Mathematics. <br>
                 To remain on the BSc Statistics programme, you must select at least 60 credits of statistics modules (currently ", statsCreds, ").</span>")
        }
      }
      txt
  })
  
  output$decisionText <- renderText({
    pureMissing = sum(!pureReq %in% uniqueChosen()$Module)
    appMissing = sum(!appReq %in% uniqueChosen()$Module)
    statMissing = sum(!statReq %in% uniqueChosen()$Module)
    nCareers = uniqueChosen() %>% filter(Section == "Careers") %>% nrow
    
    if(totCreds() != 120 || isMissingCoreqs() > 0 || nClashes() > 1){
      paste0("<span style=\"color:#E23E1A;font-size:18px\"> This is not a valid module selection, please make the recommended changes before attempting to submit your choices via S3P. </span>")
    } else if((s1Creds() %in% c(50, 70) || 
               s2Creds() %in% c(50, 70)) || 
              nCareers > 0 || 
              (input$Stage == 3 & ((input$Programme == 'MMath' & pureMissing > 0 & appMissing > 0) ||
                                   input$Programme == 'MMaS'& statMissing > 0))){
      txt = ""
      if(s1Creds() %in% c(50, 70) || s2Creds() %in% c(50, 70)){
        txt = paste0(txt, "<span style=\"color:#E28A1A;font-size:18px\"> Your module selection contains a 70:50 credit split and so requires DPD approval. 
                     Please complete the <a href ='https://forms.office.com/e/ufY4Lh5VCn'> DPD request form </a> before submitting your choices via S3P. </span><br>")
      }
      if(nCareers > 0){
        txt = paste0(txt, "<span style=\"color:#E28A1A;font-size:18px\"> Your module selection contains a careers module and so requires DPD approval. 
               Please complete the <a href ='https://forms.office.com/e/ufY4Lh5VCn'> DPD request form </a> before submitting your choices via S3P. </span><br>")
      }
      if(input$Stage == 3){
        if(input$Programme %in% c('MMath', 'MMaS') & pureMissing > 0 & appMissing > 0 & statMissing > 0){
          txt = paste0(txt, "<span style=\"color:#E28A1A;font-size:18px\"> Your module selection is valid for Stage 3, however you will not be able to progress to Stage 4 with your current selection since 
             you are missing pre-requisites for all pathways. 
                     If you wish to continue on to Stage 4 then please change your selection, otherwise you will need to switch to a 3 year degree. </span><br>")
        } else if(input$Programme == 'MMath' & pureMissing > 0 & appMissing > 0){
          txt = paste0(txt, "<span style=\"color:#E28A1A;font-size:18px\"> Your module selection is valid for Stage 3, however you will not be able to progress onto Stage 4 of the MMath degree since you are missing 
             pre-requisites for both the pure and applied pathways. If you wish to proceeed to Stage 4 MMath then please add the relevant pre-requisite(s) to your selection, alternatively you will need
             to transfer to GGC3 MMathStat Mathematics and Statistics, or a 3 year degree. </span><br>")
        } else if(input$Programme == 'MMaS' & statMissing > 0){
          txt = paste0(txt, "<span style=\"color:#E28A1A;font-size:18px\"> Your module selection is valid for Stage 3, however you will not be able to progress onto Stage 4 of the MMathStat degree since you are missing 
             pre-requisites for both the statistics pathway. If you wish to proceeed to Stage 4 MMathStat then please add the relevant pre-requisite(s) to your selection, alternatively you will need
             to transfer to G103 MMath Mathematics, or a 3 year degree. </span><br>")
        }
      }
      txt
    } else{
      paste0("<span style=\"color:#239B56;font-size:18px\"> You have made a valid module selection! If you are happy with your choices you may submit this via <a href = 'https://s3p.ncl.ac.uk'> S3P </a>. </span>")
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