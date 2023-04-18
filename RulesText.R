output$regsText <- renderText({
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
      txt = "As part of the requirements for the G103 MMath Mathematics degree:
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
      txt = ""
    }
  } else if(input$Programme == 'BMaS'){
    txt = "As part of the requirements for the GG13 BSc Mathematics and Statistics degree:
        <ul>
          <li> You must take MAS3091 Group Project </li>
          <li> You must choose at least 40 credits of maths modules (Pure or Applied) </li>
          <li> You must choose at least 40 credits of Statistics modules </li>
          <li> The remaining 30 credits can come from Pure, Applied or Statistics modules </li>
        </ul>
      " 
  } else if(input$Programme == 'MMaS'){
    if(input$Stage == 3){
      txt = "As part of the requirements for the GGC3 MMathStat Mathematics and Statistics degree:
        <ul>
          <li> You must take MAS3091 Group Project, MAS3902 Bayesian Inference and MAS3917 Stochastic Processes </li>
          <li> The remaining 90 credits can come from Pure, Applied or Statistics modules </li>
          <li> If you do not meet the requirements for fourth year, you will be given an exit award at the end of Stage 3. Your module choices will determine what this exit award is.
               If you wish to ensure you would receive BSc Mathematics and Statistics as an exit award, select at least 40 credits of Statistics modules. </li>
        </ul>
      " 
    } else if(input$Stage == 4){
      txt = ""
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
          <li> Based on your pathway chosen in Stage 2, you will have 20 credits of compulsory maths/stats modules, and must choose the remaining 40 credits of modules from your pathway. <\li>
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
          <li> For your remaining 20 credits of Accounting, you must choose either ACC2000 Interpreting Company Accounts or ACC2007 Corporate Finance </li>
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
          <li> You must take ACC3000 Case Studies in Accounting, Finance and Business, ACC3006 International Financial Management and MAS3904 Stochastic Financial Modelling. <\li>
          <li> The remaining 70 credits can be made up of any Pure, Applied or Statistics modules. <\li>
          </ul>
      "
        }
      } else if(input$Programme == 'MwB'){
          if(input$Stage == 2){
            txt = "As part of the requirements for the G1N4 BSc Mathematics with Business degree in Stage 2:
        <ul>
          <li> You must take ACC2000 Interpreting Company Accounts and BUS2000 Human Resource Management </li>
          <li> You must choose a primary pathway (Pure, Applied or Statistics) which will contribute 40 credits of modules to your Stage 2 selection 
          (but won't impact your Stage 3 choices) <\li>
          <li> You will split the remaining 40 credits among the 2 sections you didn't choose for your primary pathway (e.g. if you chose an Applied pathway, 
          you would split the remaining 40 credits between Pure and Statistics modules) in either a 40:0, 20:20 or 0:40 split. <\li>
        </ul>
      " 
          } else if(input$Stage == 3){
            txt = "As part of the requirements for the G1N4 BSc Mathematics with Business degree in Stage 3:
        <ul>
          <li> You must take ACC3000 Case Studies in Accounting, Finance and Business and BUS3000 Enterprise & Entrepeneurship with Lean Innovation </li>
          <li> The remaining 80 credits can be made up of any Pure, Applied or Statistics modules. <\li>
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
               txt = "As part of the requirements for the F345 BSc Theoretical Physics degree in Stage 3 you will take 80 credits of compulsory Physics modules and choose 40
               credits of optional modules."
             } 
          } else if(input$Programme == 'MPhys'){
           if(input$Stage == 2){
            txt = "As part of the requirements for the F344 MPhys Physics degree in Stage 2 you will take 90 credits of compulsory Physics modules
             and will choose 30 credits of optional Physics modules."
        } else if(input$Stage == 3){
           txt = "As part of the requirements for the F344 MPhys Physics degree in Stage 2 you will take 30 credits of compulsory Physics modules
            and will choose 90 credits of optional Physics modules."
        } else if(input$Stage == 4){
          txt = "As part of the requirements for the F344 MPhys Physics degree in Stage 2 you will take 120 credits of compulsory Physics modules."
        }
      }
      txt = paste0("<span style=\"font-size:12px\">", txt, "</span>")
    })