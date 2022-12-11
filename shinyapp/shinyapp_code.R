library(shiny)
library(shinythemes)
library(ggplot2)
library(scales)
library(plotly)


ui <- 
  navbarPage("PH290 Project: Trials Analysis",collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
             tabPanel("About", 
                      tags$h1("R Shiny App for Trials Analysis"),
                      tags$p("This app was made for analyzing a randomized control trial dataset. 
                    Users can upload any trials dataset saved as a file in their local machines 
                    (up to 5MB), choose variables relevant to their analyses (ie primary outcome and treatment assignments), 
                    choose regression (GLM) and/or semi-parametric and double-robust (TMLE) estimators in order to 
                    calculate unadjusted estimates (outcome differences) of intervention effects."),
                      tags$br(),
                      tags$h3("How to use app"),
                      tags$h4("Data"),
                      tags$p("User must choose parameters for the function: `read.table()` that is used to read in the uploaded dataset.
                    These parameters include setting the `header` option to either `TRUE` or `FALSE` and specifycing
                    a delimiter for the `sep` option. Lastly, a user is asked to upload a dataset from their local machine 
                    onto the R shiny server."),
                      tags$i("The uploaded dataset must be comma, semi-colon,tab, or space - delimited. Prior to upload, the 
             dataset must include the following columns: primary outcome (binary or continuous), treatment assignment 
             (categorical or binary),and an ID column describing the granularity of trial (ie individual or paired IDs). 
             The dataset must be ordered in ascending order of this ID column for perfect replication when using random 
             splits for V-fold cross-validation (TMLE)."),
                      tags$h4("Analysis"),
                      tags$p("The analysis outputs include a histogram (if continuous outcome) or bar graph (if binary outcome) showing
                    the distribution of the outcome variable by assignment group. A table of the model outputs summarizing the estimate,
                    95% confidence intervals, p-values, and the name of the analysis model is included in the next tab. The last tab
                    visualizes the estimate, the 95% confidence intervals, the null value line, and the name of the analysis model. For any 
                    of these outputs to correctly display in the main panel, users must specify variables and models in the side panel on 
                    the left."),
                      tags$p("The exposure variable (ie the column name containing the treatment assignments) must be chosen from the dropdown menu. 
                    The actual values or names of two comparison arms within this exposure variable column must be specified for the analysis 
                    to make contrasts between two study arms. Whether the exposure variable is binary or categorical, the values/names of each of the
                    two levels should be typed into each text box."),
                      tags$p("Similarly, the outcome variable must be chosen from the dropdown menu of all column names in the uploaded dataset. 
                    The type of variable (ie binary or continous) of the outcome must also be specified. Lastly, the ID variable 
                    must be chosen from the dropdown menu, and as the note reminds users, the dataset must be ordered in ascending order 
                    by this column prior to the data upload into the Shiny app."),
                      tags$p("The last item in the side panel is a checkbox that allows for users to choose either or both model options for analysis. 
                    Note that both analyses will estimate the outcome difference (or average treatment effect) between the two study arms specified by users. 
                    The `GLM` option will fit a generalized linear model to estimate intention-to-treat (ITT) effects in a trial. 
                    To estimate a difference between arms for a binary outcome (the prevalence difference or risk difference, depending on the outcome definition), 
                    the link will be set for a linear (rather than log-linear) model: family='gaussian'. This is sometimes called a linear probability model. 
                    Similarly, for a continuous outcome, the family will also be set to family='gaussian'. The`TMLE` option allows the analysis to estimate ITT 
                    effects in a trial using TMLE. The TMLE has similar arguments as the GLM (by design), with the addition of the super learner libraries. 
                    The default library is set to SL.mean,SL.glm, SL.bayesglm, SL.gam, SL.glmnet."),
                      tags$a(href="https://ben-arnold.github.io/washb/articles/washb.html#washb_tmle", "Analysis provided by functions from washb package."),
                      tags$br(),
                      tags$br()),
             tabPanel("Example",
                      tags$h1("PROVIDE Randomized Control Trial"),
                      tags$a(href="https://pubmed.ncbi.nlm.nih.gov/29514306/", "Primary publication"),
                      tags$h4("Background: "),
                      tags$p("Oral vaccines have been shown to be less effective in low-income and developing countries, 
                     limiting the optimal benefits of vaccination in populations with the greatest disease burden. 
                     There are several plausible biologic explanations for the observation of vaccine underperformance, 
                     including environmental enteropathy (EE), malnutrition, and breast milk interference. 
                     PROVIDE study was designed to evaluate factors that could interfere with oral vaccine efficacy 
                     in an environment characterized by poverty, urban overcrowding, poor sanitary conditions, 
                     and environmental enteropathy."),
                      tags$h4("Methodology: "),
                      tags$ul(
                        tags$li("Geographic Location/Study Sites: Dhaka, Bangladesh"), 
                        tags$li("Dates of Data Collection: May 2011 - November 2014"), 
                        tags$li("Study Design: Randomized controlled trial with two vaccine interventions in a 2x2 factorial design.")
                      ),
                      tags$ol(" Study arms: ",
                              tags$li("Dose 4 IPV + No Rotavirus"), 
                              tags$li("Dose 4 IPV + Rotavirus"), 
                              tags$li("Dose 4 tOPV + No Rotavirus"),
                              tags$li("Dose 4 tOPV + Rotavirus")
                      ),
                      tags$img(src = "Randomization_scheme.png", width = "300px", height = "300px"),
                      tags$h5("Capstone objectives: "),
                      tags$p("Primary: Assess causal effect (average treatment effect [ATE]) of the combining concomitant administration 
             of a 2-dose Rotarix, oral rotavirus vaccine, as well as 4th dose IPV replacement on the expected tOPV effectiveness. 
             Secondary objective: Compare traditional trials analyses of binary outcomes to TMLE and identity superior methodology. 
                    Objectives, hypothesis, endpoint measures, and planned analyses are summarized in table below."),
                      tags$img(src ="sap_table.png", width = "600px", height = "400px"),
                      tags$br(),
                      tags$br()
             ),
             tabPanel("Data",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            helpText("Select the read.table parameters below"),
                            radioButtons("header", "Header", choices = c(True = TRUE, False = FALSE), selected = FALSE),
                            radioButtons("sep", 'Delimiter', choices = c(Comma=',',Semicolon=';',Tab='\t', Space=''), selected = ','),
                            fileInput("file", "Upload a file", buttonLabel = "Upload...")
                          ),
                          mainPanel(
                            tags$p("See uploaded data below"),
                            dataTableOutput("dataframe")
                          )))
             ),
             tabPanel("Analysis",
                      fluidPage(
                        sidebarLayout(
                          sidebarPanel(
                            uiOutput("x"),
                            textInput("cl", "Type name of comparison group 1 (ie control group)"),
                            textInput("tr", "Type name of comparison group 2 (ie treatment group)"),
                            uiOutput("y"),
                            radioButtons("ytype", "Choose outcome variable type", c("Continuous", "Binary")),
                            helpText("Note: imported data must be ordered by ID prior to upload."),
                            uiOutput("id"),
                            checkboxGroupInput("mods", "Check models to run", c("GLM", "TMLE"))
                          ),
                          
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Outcome distribution",
                                       plotlyOutput("distplot")),
                              tabPanel("Table", 
                                       tableOutput("restable")),
                              tabPanel("Figure", 
                                       plotlyOutput("resplot"))
                            )
                          )
                        )
                      )),
             tabPanel("Reflections",
                      tags$h1("PH290 Project Reflections"),
                      tags$h4("Deeper dive into R Shiny: "),
                      tags$ul(
                        tags$li("Page layout options: navigation bars"),
                        tags$li("Learned to use panels (ie side bar, main panels"), 
                        tags$li("Used HTML tags for written text (ie headers, paragraphs, lists) and embedding images/links"), 
                        tags$li("Incorporated different user input optionsn (ie file input, radio buttons, text input, checkbox)"),
                        tags$li("Learned how to change selectInput options dependent on fileInput"),
                        tags$li("Wrote helper functions and ran models within Shiny app from user input datasets")
                      ),
                      tags$h4("Limitations: "),
                      tags$ul(
                        tags$li("Many assumptions made on user's data (ie small dataset, ordered, cleaned)"), 
                        tags$li("Adjusted analyses with covariates not included"), 
                        tags$li("Not very many options for models (restricted to set options for GLM and TMLE)")
                      ),
                      tags$br(), 
                      tags$br(),
                      tags$br(),
                      tags$h4("Thank you!"),
                      tags$ul(
                        tags$li(tags$a(href="https://github.com/jihyeonpak/github_thesis/tree/main/shinyapp", "Code can be found on Github")), 
                        tags$li("Email: jhpak@berkeley.edu")
                      ))
  )




server <- function(input, output, session){
  
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return(NULL)} 
    data <- as.data.frame(read.table(file1$datapath, sep=input$sep, header = as.logical(input$header)))
    
    return(data)
  })
  
  # OUTPUT for "Data" tab
  output$dataframe <- renderDataTable({
    df <- data
    df()
  })
  
  # updating selectInput options (to display column names dependent on fileInput)
  output$x<-renderUI({
    selectizeInput('x',"Select exposure variable (treatment assignments)", choices = names(data()))
  })
  
  output$y<-renderUI({
    selectizeInput('y',"Select outcome variable", choices = names(data()))
  })
  
  output$id<-renderUI({
    selectizeInput('id',"Select ID variable", choices = names(data()))
  })
  
  
  # OUTPUTS for "Analysis" tab
  output$distplot <- renderPlotly({
    p1()
    ggplotly(p1())
  })
  
  output$restable <- renderTable({
    tbl()
  })
  
  output$resplot <- renderPlotly({
    p2()
    ggplotly(p2())
  })
  
  # ANALYSIS
  
  # reactives for outputs
  p1 <- reactive({
    if (input$ytype == "Continuous"){
      p1 <- ggplot(data(), aes_string(x = input$y, fill = input$x)) + geom_histogram(position = "dodge") +
        ggtitle("Distribution of outcome variable across treatment groups") +
        ylab("Count") +
        scale_x_continuous(name = "Outcome", breaks = pretty_breaks()) +
        labs(fill = "Assignment Groups")
    }
    else if (input$ytype == "Binary"){
      p1 <- ggplot(data(), aes_string(x = input$y, fill = input$x)) + geom_bar(position = "dodge") +
        ggtitle("Distribution of outcome variable across treatment groups") +
        ylab("Count") +
        scale_x_continuous(name = "Outcome", breaks = c(0, 1)) +
        labs(fill = "Assignment Groups")
    }
    
    p1
  })
  
  
  tbl <- reactive({
    modeldat <- data
    
    if(length(input$mods) == 1){
      switch(input$mods,
             "GLM"= {
               if(input$ytype == "Binary"){
                 results <- runMods.unadjbin(Y= modeldat()[[input$y]],
                                             tr = modeldat()[[input$x]],
                                             id = modeldat()[[input$id]],
                                             group1 = input$cl,
                                             group2 = input$tr)
                 
                 results <- results[1,]
               } else if (input$ytype == "Continuous"){ 
                 results <- runMods.unadjcont(Y= modeldat()[[input$y]],
                                              tr = modeldat()[[input$x]],
                                              id = modeldat()[[input$id]],
                                              group1 = input$cl,
                                              group2 = input$tr)
                 results <- results[1,]
               }
             },
             "TMLE" = {
               if(input$ytype == "Binary"){ 
                 results <- runMods.unadjbin(Y= modeldat()[[input$y]],
                                             tr = modeldat()[[input$x]],
                                             id = modeldat()[[input$id]],
                                             group1 = input$cl,
                                             group2 = input$tr)
                 
                 results <- results[2,]
               } else if (input$ytype == "Continuous"){ 
                 results <- runMods.unadjcont(Y= modeldat()[[input$y]],
                                              tr = modeldat()[[input$x]],
                                              id = modeldat()[[input$id]],
                                              group1 = input$cl,
                                              group2 = input$tr)
                 results <- results[2,]
               }
             })
    } else if (length(input$mods) == 2){
      if(all(c("GLM", "TMLE") %in% input$mods)){
        if(input$ytype == "Binary"){ 
          results <- runMods.unadjbin(Y= modeldat()[[input$y]],
                                      tr = modeldat()[[input$x]],
                                      id = modeldat()[[input$id]],
                                      group1 = input$cl,
                                      group2 = input$tr)
        } else if (input$ytype == "Continuous"){ 
          results <- runMods.unadjcont(Y= modeldat()[[input$y]],
                                       tr = modeldat()[[input$x]],
                                       id = modeldat()[[input$id]],
                                       group1 = input$cl,
                                       group2 = input$tr)
        }
      }
    }
    
    
    results
    
  })
  
  p2 <- reactive({
    modeldat <- data
    
    if(length(input$mods) == 1){
      switch(input$mods,
             "GLM"= {
               if(input$ytype == "Binary"){ 
                 plotdf <- runMods.unadjbin(Y= modeldat()[[input$y]],
                                            tr = modeldat()[[input$x]],
                                            id = modeldat()[[input$id]],
                                            group1 = input$cl,
                                            group2 = input$tr)
                 
                 plotdf <- plotdf[1,]
               } else if (input$ytype == "Continuous"){ 
                 plotdf <- runMods.unadjcont(Y= modeldat()[[input$y]],
                                             tr = modeldat()[[input$x]],
                                             id = modeldat()[[input$id]],
                                             group1 = input$cl,
                                             group2 = input$tr)
                 plotdf <- plotdf[1,]
               }}, 
             "TMLE" = {
               if(input$ytype == "Binary"){ 
                 plotdf <- runMods.unadjbin(Y= modeldat()[[input$y]],
                                            tr = modeldat()[[input$x]],
                                            id = modeldat()[[input$id]],
                                            group1 = input$cl,
                                            group2 = input$tr)
                 
                 plotdf <- plotdf[2,]
               } else if (input$ytype == "Continuous"){ 
                 plotdf <- runMods.unadjcont(Y= modeldat()[[input$y]],
                                             tr = modeldat()[[input$x]],
                                             id = modeldat()[[input$id]],
                                             group1 = input$cl,
                                             group2 = input$tr)
                 plotdf <- plotdf[2,]
                 }})
    } else if (length(input$mods) == 2){
      if(all(c("GLM", "TMLE") %in% input$mods)){
        if(input$ytype == "Binary"){ 
          plotdf <- runMods.unadjbin(Y= modeldat()[[input$y]],
                                     tr = modeldat()[[input$x]],
                                     id = modeldat()[[input$id]],
                                     group1 = input$cl,
                                     group2 = input$tr)
        } else if (input$ytype == "Continuous"){ 
          plotdf <- runMods.unadjcont(Y= modeldat()[[input$y]],
                                      tr = modeldat()[[input$x]],
                                      id = modeldat()[[input$id]],
                                      group1 = input$cl,
                                      group2 = input$tr)
         } 
      }
    } 
    
    
    p2 <- ggplot(plotdf, aes(x = estimate, y = models)) +
      geom_vline(aes(xintercept = 0), linewidth = 0.25, linetype = "dashed") +
      geom_errorbarh(aes(xmax = ci_ul, xmin = ci_ll), linewidth = 0.5, height = 0.2, color = "gray50") +
      geom_point(size = 3.5, color = "orange") +
      theme_bw() +
      theme(panel.grid.minor = element_blank()) +
      ylab("")+
      xlab("Risk difference") +
      ggtitle("Risk differences: Treatment assignments on outcome")
    
    p2
  })
  
  
}

shinyApp(ui, server)
