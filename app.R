#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny); library(DT);library(tidyverse);library(caret);library(elasticnet);library(lime);library(doParallel);


# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(title = "LASSOMAN",
             tabPanel(title = "Howdy",
                      h1("Howdy, partner!"),
                      div(style="display: inline-block;vertical-align:top; width: 49%;",
                      h2("What in tarnation?"),
                      p("In statistics and machine learning, LASSO (least absolute shrinkage and selection operator) 
                         is a regression analysis method that performs both variable selection and regularization in order to enhance the 
                         prediction accuracy and interpretability of the resulting statistical model."),
                      h2("Instructions"),
                      h3("1. Upload your data"),
                      p("Head over to the Data Wranglin' section using the navigation bar and select a file. Your data should be in CSV format and have variable names as the first row."),
                      h3("2. Wrangle it"),
                      p("The data need to be formatted in order to be LASSO'd correctly. Drop any variables that you don't want to be considered as predictors, 
                        and be sure to create dummy variables for any categorical variables. It is recommended to center and scale the numerical values and drop any variables with little
                        to no variance. LASSO requires there be no missing data, so you can choose to drop rows with missing data or impute."),
                      h3("3. Yeehaw!"),
                      p("Pick a variable you'd like to predict and then run the LASSO by clicking 'Giddy Up', you'll see information about 
                        how the model was selected and which variables were important. You may chose to do further analysis with the selected 
                        variables, or right-click to save the plots. LASSOMAN uses repeated k-fold cross-validation to avoid overfitting and to estimate the variability of the prediction accuracy")
                      
                      ),
                      div(style="display: inline-block;vertical-align:top; width: 49%;",
                          tags$div(img(src = "./images/cowboy.jpg"))
                          ),
                      
             ),
             tabPanel(title = "Data Wranglin'",
                      sidebarPanel(
                        fileInput(inputId = "dataUpload",
                                  label = "Upload Data",
                                  accept = "csv"),
                        
                        selectInput("dropVars", "Drop Variables", choices = NULL, multiple = T),
                        selectInput("makeFactor", "Make Factor", choices = NULL, multiple = T),
                        selectInput("dummies", "Dummy Variables", choices = c("None" = "none",
                                                                              "Full Rank" = "fullRank",
                                                                              "All Levels" = "ltFullRank")),
                        selectInput("impute", "Imputation Method", choices = c("None (Drop Incomplete Cases)" = "none",
                                                                               "Median" = "medianImpute")),
                        checkboxGroupInput(
                          inputId = "preProcess",
                          label = "Pre-Processing Options",
                          choices = c("Center" = "center",
                                      "Scale" = "scale",
                                      "Remove Zero Variance Variables" = "zv",
                                      "Remove Near-Zero Variance Variables" = "nzv"),
                          selected = c("center","scale","zv","nzv"),
                          inline = FALSE
                        )
                        
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Clean Data", DT::dataTableOutput("cleanDataTab")),
                          tabPanel("Raw Data", DT::dataTableOutput("dataTab"))
                        )
                      )
                      
             ),
             tabPanel(title = "The LASSO",
                      sidebarPanel(
                        selectInput("outcomeVar", "Outcome", choices = NULL),
                        selectInput("dropVarsPost", "Drop Variables", choices = NULL, multiple = T),
                        selectInput("lassoType", "LASSO Type", choices = c("The LASSO" = "lasso",
                                                                           "Relaxed LASSO" = "relaxo",
                                                                           "The Bayesian LASSO" = "blasso",
                                                                           "Bayesian Ridge Regression (Model Averaged)" = "blassoAveraged"
                        )),
                        # selectInput("validationScheme", "Validation Scheme", choices = c("k-Fold Cross-Validation" = "kfoldcv",
                        #                                                                  "80:20 Split" = "simple8020")),
                        numericInput(inputId = "kfold",
                                     label = "k",
                                     value = 5,
                                     min = 3,
                                     step = 1
                        ),
                        numericInput(inputId = "repeats",
                                     label = "Repeats",
                                     value = 10,
                                     min = 3,
                                     step = 1
                        ),
                        numericInput(inputId = "seed",
                                     label = "Set Random Seed",
                                     value = 66,
                                     min = 1,
                                     step = 1
                        ),
                        actionButton(inputId = "lassoRun", label = "Giddy Up!")
                        
                      ),
                      mainPanel(
                        div(style="display: inline-block;vertical-align:top; width: 100%;",plotOutput("fitMetricsPlot", height = "30vh")),
                        
                        div(style="display: inline-block;vertical-align:top; width: 49%;",plotOutput("weightsPlot", height = "30vh")),
                        div(style="display: inline-block;vertical-align:top; width: 49%;",plotOutput("explainerPlot", height = "30vh")),
                        tabsetPanel(
                          tabPanel("Results", DT::dataTableOutput("fitMetricsTab"))
                        )
                      )
                      
             )
             # tabPanel(title = "Tying Things Up",
             #          sidebarPanel(),
             #          mainPanel()
             #          
             # )
  )
  
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  ## Increasing max input file size
  options(shiny.maxRequestSize=30*1024^2)
  
  dataUpload <- reactive({
    req(input$dataUpload)
    vroom::vroom(input$dataUpload$datapath,
                 .name_repair = 'minimal')
  })
  
  cleanData <- reactive({
    if (length(input$dropVars) > 0){
      df <- dataUpload() %>% select(-input$dropVars)
    }
    else {
      df <- dataUpload()
    }
    
    if (length(input$makeFactor) > 0){
      for (fac in input$makeFactor){
        df[fac] <- as.factor(df[[fac]])
      }
    }
    
    
    
    methods <- NULL
    
    if (input$impute == "medianImpute"){
      methods <- c(methods, "medianImpute")
    # } else if (input$impute == "knnImpute"){
    #   methods <- c(methods, "knnImpute")
    } else
      df <- df[complete.cases(df),]
    
    if (length(input$preProcess) > 0){
      methods <- c(methods, input$preProcess)
      
    }
    
    if (length(methods) > 0) {
      preProcValues  <- preProcess(df, method = methods)
      df <- predict(preProcValues, df)
    }
    
    if (input$dummies == "fullRank"){
      df <- model.matrix( ~ ., data = df)
    } else if (input$dummies == "ltFullRank"){
      dummies <- dummyVars( ~ ., data = df)
      df <- predict(dummies, newdata = df)
    }
    
    
    
    df <- as.data.frame(df)
    saveRDS(df, file = "clean_data.RData")
    df
    
    
  })
  
  analysisData <- reactive({
    if (length(input$dropVarsPost) > 0){
      df <- cleanData() %>% select(-input$dropVarsPost)
    }
    else {
      df <- cleanData()
    }
  })
  
  fit <- eventReactive(input$lassoRun,{
    df <- analysisData()
    
    cl <- makePSOCKcluster(parallel:::detectCores()-1)
    registerDoParallel(cl)
    
    
    
    fitControl <- trainControl(
      method = "repeatedcv",
      number = as.numeric(input$kfold),
      repeats = as.numeric(input$repeats))
    set.seed(input$seed)
    fit <- train(as.formula(paste0("`",input$outcomeVar,"`","~.")), data = df, 
                 method = input$lassoType, 
                 trControl = fitControl)
    saveRDS(fit, file = "fit.RData")
    
    stopCluster(cl)
    fit
    
  })
  
  explanation <- eventReactive(fit(),{
    explainer <- lime(cleanData(), fit(), bin_continuous = F)
    explanation <- lime::explain(cleanData(), explainer, n_features = 10)
    explanation$feat_val_weight <- explanation$feature_value * explanation$feature_weight
    explanation
  })
  
  output$dataTab <- DT::renderDataTable({
    DT::datatable(dataUpload(),options = list(
      pageLength = 100,
      scrollY = "70vh",scrollX = TRUE))
  })
  
  output$cleanDataTab <- DT::renderDataTable({
    DT::datatable(cleanData(),options = list(
      pageLength = 100, 
      scrollY = "70vh",scrollX = TRUE))
  })
  
  output$fitMetricsTab <- DT::renderDataTable({
    DT::datatable(arrange(fit()$results,RMSE,MAE),options = list(
      pageLength = 10, 
      scrollY = "20vh",scrollX = TRUE))
  })
  
  
  output$fitMetricsPlot <- renderPlot({
    req(fit())
    ggplot(fit()) +
      theme_bw()
  })
  
  # output$varImpPlot <- renderPlot({
  #   req(fit())
  #   ggplot(varImp(fit(), scale = F)) +
  #     theme_bw()
  # })
  
  output$explainerPlot <- renderPlot({
    req(explanation())
    ggplot(data = explanation(), aes(x = feat_val_weight, y = reorder(feature, abs(feature_weight)), color = feature_value)) +
      #geom_dotplot(binaxis='x', stackdir='center', dotsize=0.1, stackratio = 0.1) +
      geom_jitter(width = 0.1) + 
      scale_color_gradient2(high = "red", mid = "gray", low = "blue") +
      labs(x = "Feature Effect on Prediction", y = "", color = "Feature Value") +
      theme_bw()
  })
  
  output$weightsPlot <- renderPlot({
    req(explanation())
    ggplot(data = explanation(), aes(x = feature_weight, y = reorder(feature, abs(feature_weight)))) +
      geom_bar(stat = "summary") +
      labs(x = "Feature Weight", y = "") +
      theme_bw()
  })
  
  
  
  
  observeEvent(dataUpload(),
               updateSelectInput(session, "dropVars",
                                 choices = names(dataUpload()) ))
  
  
  observeEvent(dataUpload(),
               updateSelectInput(session, "makeFactor",
                                 choices = names(dataUpload()) ))
  
  observeEvent(req(cleanData()),{
    updateSelectInput(session, "outcomeVar", choices = names(cleanData()))
  })
  
  observeEvent(cleanData(),
               updateSelectInput(session, "dropVarsPost",
                                 choices = names(cleanData()) ))
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
