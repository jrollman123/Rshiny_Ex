
library(tidyverse) 
library(ggplot2)
library(caret)
library(gbm)
library(DT)
library(psych) 
library(knitr)
library(plotly)
library(shiny)
library(shinydashboard)

dat <- read.csv("heart_cleveland_upload.csv") %>% 
  mutate(across(c("sex","cp","fbs","restecg","exang","slope","thal","condition"),factor))

# Define UI for application 
ui <- dashboardPage(skin="blue",
                    
                    #add title
                    dashboardHeader(title="RShiny Example: Heart Disease",titleWidth=1000),
                    
                    #define sidebar items
                    dashboardSidebar(sidebarMenu(
                      menuItem("About", tabName = "about", icon = icon("archive")),
                      menuItem("Date Exploration", tabName = "eda", icon = icon("chart-bar")),
                      menuItem("Cluster Analysis", tabName = "clust", icon = icon("grip-horizontal")),
                      menuItem("Predictive Models", tabName = "model", icon = icon("laptop-code")),
                      menuItem("Raw Data", tabName = "data", icon = icon("table"))
                    )),
                    
                    #define the body of the app
                    dashboardBody(
                      tabItems(
                        # First tab content
                        tabItem(tabName = "about",
                                fluidRow(
                                  #add in latex functionality if needed
                                  withMathJax(),
                                  
                                  #two columns for each of the two items
                                  column(6,
                                         #Description of App
                                         h1("Purpose"),
                                         #box to contain description
                                         box(background="light-blue",width=12,
                                             h4("The purpose of this app is to demonstrate R Shiny as a tool to analyze a particular data set. This app contains various plots and statistical models, each located on a distinct tab."),
                                             h4("The full analysis was done on the", a(href="https://archive.ics.uci.edu/ml/datasets/Heart+Disease", target="_blank","Cleveland Heart Disease Data Set.", style="color:navy"), " However, a preprocessed version listed", a(href="https://www.kaggle.com/cherngs/heart-disease-cleveland-uci", target="_blank","here,", style="color:navy"), " was used for the analysis.")
                                         ),
                                         h1("App Information"),
                                         #box to contain description
                                         box(background="light-blue",width=12,
                                             h4(p(strong("Data Exploration:"))," Customizable plots and summaries for the variables included in the data set. Investigate numerical summaries by selected factors."),
                                             h4(p(strong("Cluster Analysis:")), "Hierarchical clustering with user specified linkage method."),
                                             h4(p(strong("Predictive Models:")), "Logistic Regression and Boosted Tree Models with user specified inputs."),
                                             h4(p(strong("Raw Data:")), "Table of the raw data, subsettable and downloadable.")
                                         )
                                  ),
                                  
                                  column(6,
                                         h1("Data Information"),
                                         #box to contain description
                                         box(background="light-blue",width=12,
                                             h4("Information about the data and variables taken directly from the source website listed to the side."),
                                             h4("There are 13 attributes"),
                                             h4("1. age: age in years"),
                                             h4("2. sex: sex (1 = male; 0 = female)"),
                                             h4("3. cp: chest pain type"),
                                             h5("-- Value 0: typical angina"),
                                             h5("-- Value 1: atypical angina"),
                                             h5("-- Value 2: non-anginal pain"),
                                             h5("-- Value 3: asymptomatic"),
                                             h4("4. trestbps: resting blood pressure (in mm Hg on admission to the hospital)"),
                                             h4("5. chol: serum cholestoral in mg/dl"),
                                             h4("6. fbs: (fasting blood sugar > 120 mg/dl) (1 = true; 0 = false)"),
                                             h4("7. restecg: resting electrocardiographic results"),
                                             h5("-- Value 0: normal"),
                                             h5("-- Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)"),
                                             h5("-- Value 2: showing probable or definite left ventricular hypertrophy by Estes' criteria"),
                                             h4("8. thalach: maximum heart rate achieved"),
                                             h4("9. exang: exercise induced angina (1 = yes; 0 = no)"),
                                             h4("10. oldpeak = ST depression induced by exercise relative to rest"),
                                             h4("11. slope: the slope of the peak exercise ST segment"),
                                             h5("-- Value 0: upsloping"),
                                             h5("-- Value 1: flat"),
                                             h5("-- Value 2: downsloping"),
                                             h4("12. ca: number of major vessels (0-3) colored by flourosopy"),
                                             h4("13. thal: 0 = normal; 1 = fixed defect; 2 = reversable defect"),
                                             h4("14. condition (outcome): 0 = no disease, 1 = disease")
                                         )
                                  )
                                )
                        ),
                        #Exploratory Data Analysis Tab
                        tabItem(tabName = "eda",
                                fluidRow(
                                  column(width=3,
                                         h3("Box-Plot Controls"),
                                         box(width=12,background="light-blue",
                                             selectInput("fct","Box-Plot Factor",c("sex", "cp", "fbs", "restecg","exang","slope","thal","condition"), "sex"),
                                             selectInput("num", "Box-Plot Numeric Variable", c("age","trestbps","chol","thalach","oldpeak","ca"), "chol"),
                                             actionButton("save","Save Box-Plot to PNG")
                                         ),
                                         br(),
                                         h3("Summary Plot Controls"),
                                         box(width=12,
                                             background="light-blue",
                                             selectInput("var","List of Plottable Variables",c("sex", "cp", "fbs", "restecg","exang","slope","thal","condition","age","trestbps","chol","thalach","oldpeak","ca"), "cp"),
                                             conditionalPanel(condition = "input.var == 'age'| input.var == 'chol'| input.var == 'trestbps'| input.var == 'thalach'| input.var == 'oldpeak'| input.var == 'ca'",
                                                              sliderInput("bin","Histogram Bin Width",min = 0,max = 100,value = 30, step=1))
                                         ),
                                         br(),
                                         h3("Summary Table Filters"),
                                         box(width=12, title = "Would you like to subset data by Sex or Disease Status?",
                                             background="light-blue",
                                             checkboxInput("filter","Yes, filter!"),
                                             conditionalPanel(condition = "input.filter",
                                                              radioButtons("summSex","Select Person's Sex",
                                                                           c("Female" = "0",
                                                                             "Male" = "1")),
                                                              radioButtons("summDis","Select Person's Disease Status",
                                                                           c("No Heart Disease" = "0",
                                                                             "Heart Disease Present" = "1"))
                                             )
                                         )
                                  ),
                                  column(width=9,
                                         fluidRow(
                                           withMathJax(),
                                           box(width=6,
                                               h4(uiOutput("boxPlotTitle")),
                                               plotlyOutput("boxPlot"),
                                               br(),
                                               h4("Correlation plot for numeric variables with scatter plot and histograms"),
                                               plotOutput("pairPlot")
                                           ),
                                           box(width=6,
                                               h4(uiOutput("summPlotTitle")),
                                               plotOutput("summPlot"),
                                               br(),
                                               h4(uiOutput("summTabTitle")),
                                               tableOutput("summTable")
                                           )
                                         )
                                  )
                                )
                        ),
                        #Hierarchical Clustering Tab
                        tabItem(tabName = "clust",
                                fluidRow(
                                  column(width=3,
                                         box(width=12,background="light-blue", title = "Choose Clustering Linkage Method",
                                             selectInput("link","Linkage Method",c("complete", "single", "average", "centroid"), "complete"),
                                             checkboxGroupInput("clustVar", "Variables for Clustering", c("age","trestbps","chol","thalach","oldpeak","ca"), c("chol","age","trestbps")),
                                             actionButton("clustRun","Run Hierarchical Custering")
                                         )
                                  ),
                                  column(width=9,
                                         fluidRow(
                                           withMathJax(),
                                           box(width=12,
                                               h4("Hierarchical Clustering Dendrogram"),
                                               plotOutput("dendo", width = "100%", height = "800px")
                                           )
                                         )
                                  )
                                )
                        ),
                        #Predictive Model Tab
                        tabItem(tabName = "model",
                                fluidRow(
                                  column(width=3,
                                         box(width=12,background="light-blue",
                                             h4("Choose Output"),
                                             radioButtons("modOut", "Output", c("Confusion Matrix Test Data Summary"="0","Test Data Missclassification Rate" = "1", "Model Training Output"="2"))
                                             
                                         ),
                                         h3("Logistic Regression Controls"),
                                         box(width=12,background="light-blue",
                                             h4("Parameter Selection"),
                                             checkboxGroupInput("logVar", "Variables for Logistic Regression", c("sex", "cp", "fbs", "restecg","exang","slope","thal","age","trestbps","chol","thalach","oldpeak","ca")),
                                             actionButton("logRun","Run Logistic Regression")
                                         ),
                                         h3("Boosted Classifcation Tree Controls"),
                                         box(width=12,background="light-blue",
                                             h4("Parameter Selection"),
                                             radioButtons("modtype", "Model Parameters", c("Default Parameters"="0","Custom Parameters" = "1")),
                                             conditionalPanel(condition = "input.modtype=='1'",
                                                              sliderInput("trees","# of Boosting Iterations (trees)",min = 10,max = 1000,value = 100, step=10),
                                                              sliderInput("depth","Max Tree Depth",min = 1,max = 5,value = 3, step=1),
                                                              sliderInput("shrink","Shrinkage",min = .01,max = .9,value = .1, step=.01),
                                                              sliderInput("node","Min Terminal Node Size",min = 0,max = 100,value = 30, step=1)),
                                             actionButton("boostRun","Run Boosted Classification Tree")
                                         ),
                                         h3("Prediction Controls"),
                                         box(width=12,background="light-blue",
                                             h4("Predictor Values (See About Tab for Information)"),
                                             numericInput("predSex", "Subject's sex (1 = male; 0 = female)", value = 0, min = 0, max = 1, step = 1),
                                             numericInput("predCP", "Subject's level of chest pain", value = 0, min = 0, max = 3, step = 1),
                                             numericInput("predFBS", "Subject's fasting blood sugar > 120 mg/dl? (1 = true; 0 = false)", value = 0, min = 0, max = 1, step = 1),
                                             numericInput("predRest", "Subject's  resting electrocardiographic results", value = 0, min = 0, max = 2, step = 1),
                                             numericInput("predExang", "Subject has exercise induced angina? (1 = yes; 0 = no)", value = 0, min = 0, max = 1, step = 1),
                                             numericInput("predSlope", "Subject's slope of the peak exercise ST segment", value = 0, min = 0, max = 2, step = 1),
                                             numericInput("predThal", "Subject's Thal Defect", value = 0, min = 0, max = 2, step = 1),
                                             numericInput("predAge", "Subject's age", value = 65, min = 1, max = 120, step = 1),
                                             numericInput("predBPS", "Subject's resting blood pressure (in mm Hg)", value = 130, min = 50, max = 250, step = 1),
                                             numericInput("predChol", "Subject's serum cholestoral (in mg/dl)", value = 240, min = 100, max = 600, step = 1),
                                             numericInput("predThala", "Subject's maximum heart rate achieved", value = 153, min = 40, max = 250, step = 1),
                                             numericInput("predPeak", "Subject's ST depression induced by exercise relative to rest", value = .8, min = 0, max = 10, step = .1),
                                             numericInput("predCA", "Subject's number of major vessels (0-3) colored by flourosopy", value = 0, min = 0, max = 3, step = 1),
                                             h4("Select Model (Created Above)"),
                                             radioButtons("predType", "Models", c("Run Logistic Regression"="0","Run Boosted Classification Tree" = "1")),
                                             actionButton("modPred","Run Model")
                                         )
                                         
                                  ),
                                  column(width=9,
                                         fluidRow(
                                           withMathJax(),
                                           box(width=6,
                                               h4("Logistic Regression Model Output"),
                                               verbatimTextOutput("logText"),
                                               br(),
                                               h3(textOutput("predText")),
                                               br(),
                                               h3("Probability Function:"),
                                               h4(p("P(Heart Disease) = \\(\\frac{e^{\\alpha+\\beta X}}{1+e^{\\alpha+\\beta X}}\\)"))
                                               #p("\\P = \\frac{\\e^{\\a+\\b\\X}}{1 + e^{a+bX}}")
                                           ),
                                           box(width=6,
                                               h4("Boosted Classification Tree Output"),
                                               verbatimTextOutput("boostText")
                                           )
                                         )
                                  )
                                )
                        ),
                        #Raw Data Tab
                        tabItem(tabName = "data",
                                fluidRow(
                                  column(width=3,
                                         h3("Data Table Controls"),
                                         box(width=12,background="light-blue", title = "Filter Data",
                                             checkboxInput("filterTab","Yes, filter!"),
                                             conditionalPanel(condition = "input.filterTab",
                                                              radioButtons("sexTab","Subject Sex",c("Male" = "1", "Female" = "0"),"1"),
                                                              radioButtons("conTab","Subject Recorded Condition",c("Heart Disease" = "1", "No Heart Disease" = "0"), "1")
                                             ),
                                             br(),
                                             h4("Export Raw Data to CSV"),
                                             textInput("csvName", "Save File as..."),
                                             actionButton("saveTab","Export")
                                             
                                         )
                                  ),
                                  column(width=9,
                                         fluidRow(
                                           withMathJax(),
                                           box(width=12,
                                               h4("Raw Data"),
                                               dataTableOutput("rawData")
                                               
                                           )
                                         )
                                  )
                                )
                        )
                        
                      )))









# Define server logic
server <- function(input, output, session) {
  
  output$boxPlotTitle <- renderText({
    paste("Box-Plot showing the numerical variable", input$num, "by the factor", input$fct, sep = " ")
  })
  
  #Create interactive Plotly ggplot box plot
  output$boxPlot <- renderPlotly({
    fact1 <- input$fct
    num1 <- input$num
    bp <- ggplot(dat, aes_string(x = input$fct, y = input$num)) +
      geom_boxplot() +
      geom_jitter(aes_string(color = input$fct))
    bp2 <- ggplotly(bp)
    print(bp2)
  })
  
  
  output$pairPlot <- renderPlot({
    pairs.panels(select(dat, age, trestbps, chol, thalach, oldpeak),
                 method = "pearson", # correlation method
                 hist.col = "#00AFBB",
                 density = TRUE,  # show density plots
                 ellipses = F # show correlation ellipses
    )
  })
  
  output$summPlotTitle <- renderText({
    var <- input$var
    if (is.numeric(dat[,var])) {
      p <- paste("Histogram of", input$var, "with a binwidth size of", input$bin, sep = " ")
      print(p)
    } else if (is.factor(dat[,var])){
      p <- paste("Bar plot of", input$var, sep = " ")
      print(p)
    }
  })
  
  output$summPlot <- renderPlot({
    #If numeric
    var <- input$var
    if (is.numeric(dat[,var])) {
      ggplot(dat, aes_string(x = input$var)) +
        geom_histogram(binwidth = input$bin, aes(y=..density..))
      
    } else if (is.factor(dat[,var])){
      #If Factor
      ggplot(dat, aes_string(x = input$var)) +
        geom_bar( aes_string(fill = input$var))
    }
  })
  
  output$summTable <- renderTable({
    if (input$filter) {
      df <- data.frame(do.call(cbind, lapply(select(filter(dat,sex==input$summSex & condition==input$summDis), where(is.numeric)), summary)))
    } else {
      df <- data.frame(do.call(cbind, lapply(select(dat, where(is.numeric)), summary)))
    }
    rownames(df) <- c("Min","1st Qu","Median","Mean","3rd Qu","Max")
    print(df)
  },rownames = T)
  
  
  output$summTabTitle <- renderText({
    if (input$filter) {
      p <- paste("Summary Statistics for Numeric Variables Filtered on Sex =", input$summSex, "and Disease Status =", input$summDis, sep = " ")
      print(p)
    } else {
      p <- "Summary Statistics for Numeric Variables"
      print(p)
    }
  })
  
  observeEvent(input$save,{
    fact1 <- input$fct
    num1 <- input$num
    bp <- ggplot(dat, aes_string(x = input$fct, y = input$num)) +
      geom_boxplot() +
      geom_jitter(aes_string(color = input$fct))
    fn <- paste0("BoxPlot_",input$fct,"_",input$num,".png")
    ggsave(filename = fn, bp)
  })
  #dat$age, dat$trestbps, dat$chol, dat$thalach, dat$oldpeak
  hierClust <- eventReactive(input$clustRun,{
    hierClust <- hclust(dist(data.frame(select(dat, input$clustVar))), method = input$link)
    clustPlot <- plot(hierClust, xlab="")
    print(clustPlot)
  })
  
  output$dendo <-  renderPlot({
    hierClust()
  })
  
  logModel <- eventReactive(input$logRun,{
    newdat <- select(dat, c(condition, input$logVar))
    DataIndex <- createDataPartition(newdat$condition, p = .8, list = F)
    dat.Train <- newdat[DataIndex,]
    dat.Test <- newdat[-DataIndex,]
    fit1 <- train(condition ~ ., data = dat.Train,
                  method = "glm",
                  family = "binomial",
                  trControl = trainControl(method = "cv", number = 10))
    cm <- confusionMatrix(data = dat.Test$condition, reference = predict(fit1,newdata = dat.Test))
    if (input$modOut == '0'){
      cm
    } else if (input$modOut=='1'){
      mc <- round((1-cm$overall[1])*100,1)
      names(mc) <- "Missclassification Rate (%)"
      mc
    } else {
      fit1
    }
  })
  
  output$logText <- renderPrint({
    logModel()
  })
  
  boostModel <- eventReactive(input$boostRun,{
    newdat <- dat #select(dat, c(condition, input$logVar))
    DataIndex <- createDataPartition(newdat$condition, p = .8, list = F)
    dat.Train <- newdat[DataIndex,]
    dat.Test <- newdat[-DataIndex,]
    
    if (input$modtype=='1'){
      fit2 <- train(condition ~ ., data = dat.Train,
                    method = "gbm",
                    trControl = trainControl(method = "cv", number = 10),
                    tuneGrid = expand.grid(n.trees = input$trees, interaction.depth = input$depth, shrinkage = input$shrink, n.minobsinnode = input$node),
                    verbose = FALSE
      )
    } else {
      fit2 <- train(condition ~ ., data = dat.Train,
                    method = "gbm",
                    trControl = trainControl(method = "cv", number = 10),
                    verbose = FALSE
      )
    }
    
    cm <- confusionMatrix(data = dat.Test$condition, reference = predict(fit2,newdata = dat.Test))
    
    if (input$modOut == '0'){
      cm
    } else if (input$modOut=='1'){
      mc <- round((1-cm$overall[1])*100,1)
      names(mc) <- "Missclassification Rate (%)"
      mc
    } else {
      fit2
    }
  })
  
  output$boostText <- renderPrint({
    boostModel()
  })
  
  FullModel <- eventReactive(input$modPred,{
    #Model Variable Selection
    newdat <- select(dat, c(condition, input$logVar))
    #Prediction Data
    predDat <- data.frame( sex = input$predSex,
                           cp= input$predCP, 
                           fbs = input$predFBS, 
                           restecg= input$predRest,
                           exang= input$predExang,
                           slope= input$predSlope,
                           thal= input$predThal,
                           age= input$predAge,
                           trestbps= input$predBPS,
                           chol= input$predChol,
                           thalach= input$predThala,
                           oldpeak= input$predPeak,
                           ca= input$predCA)
    #Change to Factors
    predDat <- mutate(predDat,across(c("sex","cp","fbs","restecg","exang","slope","thal"),factor))
    #Run Full Model
    if (input$predType == '0'){
      fit <- train(condition ~ ., data = newdat,
                   method = "glm",
                   family = "binomial",
                   trControl = trainControl(method = "cv", number = 10))
    } else if (input$predType == '1'){
      if (input$modtype=='1'){
        fit <- train(condition ~ ., data = dat,
                     method = "gbm",
                     trControl = trainControl(method = "cv", number = 10),
                     tuneGrid = expand.grid(n.trees = input$trees, interaction.depth = input$depth, shrinkage = input$shrink, n.minobsinnode = input$node),
                     verbose = FALSE
        )
      } else {
        fit <- train(condition ~ ., data = dat,
                     method = "gbm",
                     trControl = trainControl(method = "cv", number = 10),
                     verbose = FALSE
        )
      }
    }
    #Predict Outcome
    pred.o <- predict(fit,newdata = predDat)
    pred.p <- predict(fit,newdata = predDat, type = "prob")
    outcome <- ifelse(pred.o == 1, "heart disease", "no heart disease")
    prob <- round(pred.p[1,2],2)
    
    f.pred <- paste("This subject with the currently selected inputs is predicted to have", outcome,"(probabilty of heart disease =",prob,")", sep = " ")
    print(f.pred)
    
  })
  
  output$predText <- renderText({
    FullModel()
  })
  
  output$rawData <- DT::renderDataTable({
    if (input$filterTab) {
      s <- as.integer(input$sexTab)
      c <- as.integer(input$conTab)
      filtdat <- filter(dat, sex == s & condition == c)
    } else {
      filtdat <- dat
    }
    
    datatable(filtdat)
  })
  
  observeEvent(input$saveTab,{
    if (input$filterTab) {
      s <- as.integer(input$sexTab)
      c <- as.integer(input$conTab)
      filtdat <- filter(dat, sex == s & condition == c)
    } else {
      filtdat <- dat
    }
    
    fileNM <- paste0(input$csvName,".csv")
    write.csv(filtdat,file = fileNM)
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

