library(shiny)
library(tidyverse)
library(nortest)
library(mvnormtest)
library(MASS)
library(shinyLP)
library(gmodels)
library(caret)
library(rattle)
library(ranger)
library(kernlab)
library(nnet)
library(mclust)
library(keras)
library(mlbench) 
library(randomForest)
library(Metrics)
library(BBmisc)
library(corrplot)
library(lars)
library(xgboost)
library(Matrix)
library(methods)
library(mlr)
library(scales)
library(shinythemes)
library(plotly)
library(data.table)
library(flexdashboard)
library(DiagrammeR)
ui <- fluidPage(
  theme = shinytheme("darkly"),
  
  navbarPage(title = "Air pollution",
             
             tabPanel("Data Sets", 
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file1", "Choose CSV File", accept=c('text/csv', 'text/comma-separated-values', 'text/plain', '.csv')),
                          radioButtons("indata", "Choice:", choices = c("Full", "Columns")),
                          selectInput("cols", "Choose the variable", choices = "", selected = " ", multiple = TRUE), 
                          downloadButton('downloaddatset', "Download"),
                          hr(),
                          radioButtons("trans1", "Transformation:", choices = c("Not-Required","standardize")),
                          hr()
                          
                        ), 
                        
                        mainPanel(tableOutput("tab1"))
                      )
                      
             ), 
             
             navbarMenu("Statistical Analysis",
                        tabPanel("Summary Statistics",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols1", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("ssoption", "Select Option", choices = c("Summary", "Length", "Dim", "Type of", "Class"))
                                     
                                   ), 
                                   mainPanel(
                                     fluidRow(
                                       h3("Summary Statistics"),
                                       div(
                                         verbatimTextOutput("summar")
                                       )
                                     )
                                   )
                                 )
                        ), 
                        tabPanel("Plots",
                                 sidebarLayout(
                                   sidebarPanel(
                                     radioButtons("plotoption", "Choose the Option:", choices = c("Histogram", "DensityPlot", "Scatter" )),
                                     selectInput("cols6", "Choose Varibale 1:", choices = "", selected = " ", multiple = TRUE),
                                     textInput("xaxisname", "Write X Axis Name"),
                                     textInput("yaxisname", "Write Y Axis Name"),
                                     textInput("title", "Write Title For the Graph")
                                   ), 
                                   mainPanel(
                                     h3("Plots"),
                                     fluidRow(
                                       plotOutput("plot")
                                     )
                                   )
                                 )
                                 
                        ),
                        tabPanel("Correlation", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("cols9", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     selectInput("cols10", "Choose Variable:", choices = "", selected = " ", multiple = TRUE),
                                     radioButtons("cormethod", "Select Method:", choices = c("Covariance", "KarlPearson", "Spearman", "Kendals")),
                                     hr(),
                                     helpText("For Details Visit:"),
                                     a(href="https://en.wikipedia.org/wiki/Spearman%27s_rank_correlation_coefficient", "Karl Pearson Correlation Test"),
                                     hr()
                                   ), 
                                   mainPanel(
                                     h3("Covariance & Correlation"),
                                     verbatimTextOutput("cor_t")
                                   )
                                   
                                 )
                                 
                        )
                        
             ),
             
             navbarMenu("Supervised Learning",
                        
                        tabPanel("Random Forests", 
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("rfvar", "Select Variable", choices = "", selected = ""),
                                     
                                     textInput("rfprop", "Select Proportion", value = 0.8, placeholder = "Percentage of rows"),
                                     radioButtons("rfoption", "Select Method", choices = c("No Option", "Show Prop.", "Train & Test Data", "Importance", "Pred. Accuracy", "Summary")),
                                     hr(),
                                     helpText("Variable selected must be Non NA ."),
                                     hr(),
                                     a(href="https://en.wikipedia.org/wiki/Random_forest", "Random Forest")
                                   ),
                                   mainPanel(
                                     div(verbatimTextOutput("rfoutput")),
                                     div(
                                       plotOutput("rfplot")
                                     )
                                     
                                   )
                                 )
                        ),
                        tabPanel("XGBoost",
                                 sidebarLayout(
                                   sidebarPanel(
                                     selectInput("nbvar", "Select Variable", choices = "", selected = ""),
                                     
                                     textInput("nbprop", "Select Proportion", value = 0.8, placeholder = "Percentage of rows"),
                                     radioButtons("nboption", "Select Method", choices = c("No Option", "Show Prop.", "Train & Test Data", "Fit", "Importance", "Pred. Accuracy")),
                                     hr(),
                                     helpText("Variable selected must be categorical and numerical."),
                                     hr(),
                                     a(href="https://en.wikipedia.org/wiki/Naive_Bayes_classifier", "Naive Bayes Classifier")
                                   ),
                                   mainPanel(
                                     div(verbatimTextOutput("nboutput")),
                                     div(
                                       plotOutput("xgbplot")
                                     )
                                     
                                   )
                                 )
                        )
                        ),
             
             tabPanel("Contact", 
                      sidebarLayout(
                        sidebarPanel(
                          "Information to contact"
                        ), 
                        mainPanel(htmlOutput("text1"))
                      )
             )
  )
)






server <- function(input, output, session) {
  
  # for DATASET TAB
  
  data_input <- reactive({
    infile <- input$file1
    req(infile)
    data.frame(read.csv(infile$datapath)) 
  })
  
  observeEvent(input$file1,{
    updateSelectInput(session, inputId = "cols", choices = names(data_input()))
  }
  )
  
  logno <- reactive({
    df <- data_input()
    x <- matrix(NA, length(df[, input$cols]), length(df[, input$cols][[1]]))
    for(i in 1:length(df[, input$cols])){
      for(j in 1:length(df[, input$cols][[1]])){
        x[i, j] <- dlnorm(df[, input$cols][[i]][j]) 
      }
    }
    return(t(x))
  })
  
  standout <- reactive({
    df <- data_input()
    
    x <- matrix(NA, length(df[, input$cols]), length(df[, input$cols][[1]]))
    
    if(!is.list(df[, input$cols])){
      df[, input$cols] <- list(df[, input$cols])
    }
    
    for(i in 1:length(df[, input$cols])){
      
      for(j in 1:length(df[, input$cols][[1]])){
        x[i, j] <- df[, input$cols][[i]][j]-mean(df[, input$cols][[i]])/sd(df[, input$cols][[i]])
      }
    }
    return(t(x))
    
  })
  
  logdata <- reactive({
    df <- data_input()
    ld <- log(df[, input$cols])
    return(ld)
  })
  
  invlogdata <- reactive({
    df <- data_input()
    ild <- 1/log(df[, input$cols])
    return(ild)
  })
  
  expdata <- reactive({
    df <- data_input()
    expd <- log(df[input$cols])
    return(expd)
  })
  
  
  output$tab1 <- renderTable(
    {
      df <- data_input()
      
      if (input$indata == "Full"){
        print(df)
      }else if(input$indata == "Columns"){
        print(names(df))
      }else if(input$trans1 == "Not-Required"){
        print(data)
      }else if(input$trans1 == "standardize"){
        data <- df[, input$cols]
        m <- colMeans(data)
        s <- apply(data, 2, sd)
        z_data <- scale(data, center = m, scale = s)
        print(z_data)
      }
      
    }
  )
  
  
  output$downloaddatset <- downloadHandler(
    
    filename <- function(){
      paste("data-", Sys.Date(), ".csv", sep = "")
    },
    
    content <- function(file){
      df <- data_input()
      if(input$trans1 == "log"){
        write.csv(logdata(), file, row.names = TRUE)
      } else if(input$trans1 == "standardize"){
        write.csv(standout(), file, row.names = TRUE)
      }
      
    }
    
  )
  
  # summary statistics
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols1", choices = names(data_input()))
  }
  )
  
  summ <- reactive({
    var1 <- data_input()[,input$cols1]
    
    if (input$ssoption == "Summary"){
      su <- summary(var1)
      return(su)
    } else if (input$ssoption == "Length"){
      return(length(var1))
    } else if(input$ssoption == "Dim"){
      return(dim(var1))
    } else if (input$ssoption == "Type of"){
      return(typeof(var1))
    } else if(input$ssoption == "Class"){
      return(class(var1))
    }
  })
  
  output$summar <- renderPrint({
    
    if (input$ssoption == "Summary"){
      summ()
    } else if (input$ssoption == "Length"){
      summ()
    } else if(input$ssoption == "Dim"){
      summ()
    } else if (input$ssoption == "Type of"){
      summ()
    } else if(input$ssoption == "Class"){
      summ()
    }
  })
  
  # frequency tab
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols2", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols3", choices = names(data_input()))
  }
  )
  
  freq <- reactive({
    var1 <- data_input()[,input$cols2]
    var2 <- data_input()[,input$cols3]
    fre <- table(var1, var2)
    return(fre)
  })
  
  output$freq_tables <- renderPrint({
    freq()
  })
  
  # Cross tabulation
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols4", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols5", choices = names(data_input()))
  }
  )
  
  cross <- reactive({
    var1 <- data_input()[,input$cols4]
    var2 <- data_input()[,input$cols5]
    
    cro <- chisq.test(var1, var2)
    return(cro)
  })
  
  output$chi_t <- renderPrint({
    cross()
    
  })
  
  # Plots 
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols6", choices = names(data_input()))
  }
  )
  
  output$plot <- renderPlot({
    df <- data_input()
    if(input$plotoption == "Histogram"){
      ggplot(data = df, aes(x= df[, input$cols6])) +
        geom_histogram() +
        xlab(input$xaxisname)+
        ylab(input$yaxisname)+
        ggtitle(input$title)
    } else if(input$plotoption == "DensityPlot"){
      ggplot(data = df, aes(x= df[, input$cols6]),fill = "lightgray") +
        geom_density() +
        xlab(input$xaxisname)+
        ylab(input$yaxisname)+
        ggtitle(input$title)+
        geom_vline(aes(xintercept = mean(input$cols6)), 
                   linetype = "dashed", size = 0.6)
    } else if (input$plotoption == "Scatter"){
      scatter.smooth(df[, input$cols6], xlab = input$xaxisname, ylab = input$yaxisname, main = input$title)
    }
  })
  
  # Statistical Tests
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols7", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols8", choices = names(data_input()))
  }
  )
  
  output$qqp <- renderPlot({
    df <- data_input()
    qqnorm(df[, input$cols7]);qqline(df[, input$cols7])
  })
  
  adt <- reactive({
    df <- data_input()
    var <- df[, input$cols7]
    ad <- ad.test(var)
    return(ad)
  })
  
  sht <- reactive({
    df <- data_input()
    var <- df[, input$cols7]
    sh <- shapiro.test(var)
    return(sh)
  })
  
  kst <- reactive({
    df <- data_input()
    var1 <- df[, input$cols7]
    var2 <- df[, input$cols8]
    ks <- ks.test(var1, var2)
    return(ks)
  })
  
  mvst <- reactive({
    df <- data_input()
    var1 <- df[, input$cols7]
    var2 <- df[, input$cols8]
    return(mshapiro.test(t(as.data.frame(var1, var2))))
  })
  
  output$normtest <- renderPrint({
    
    if(input$normaltest == "A-D-Test"){
      print(adt())
    } else if(input$normaltest == "Shapiro"){
      print(sht())
    } else if(input$normaltest == "KS-Test"){
      print(kst())
    } else if(input$normaltest == "MV-Shapiro"){
      print(mvst())
    }
    
  }
  
  )
  # correlation & regression 
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols9", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols10", choices = names(data_input()))
  }
  )
  
  cortest <- reactive({
    var1 <- data_input()[,input$cols9]
    var2 <- data_input()[,input$cols10]
    
    if (input$cormethod == "Covariance"){
      return(cov(var1, var2))
    } else if (input$cormethod == "KarlPearson"){
      return(cor.test(var1, var2, method = "pearson"))
    } else if(input$cormethod == "Spearman"){
      return(cor.test(var1, var2, method="spearman"))
    } else {
      return(cor.test(var1, var2, method="kendall"))
    }
  }
  )
  
  output$cor_t <- renderPrint({
    
    cortest()
  })
  
  # simple linear regression
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "cols11", choices = names(data_input()))
    updateSelectInput(session, inputId = "cols12", choices = names(data_input()))
  }
  )
  
  lmout <- reactive({
    df <- data_input()
    var1 <- df[, input$cols11]
    var2 <- df[, input$cols12]
    out <- lm(var1 ~ var2, data = df)
    return(out)
  })
  
  output$regout <- renderPrint({
    if(input$regmethod == "Fit"){
      lmout()
    } else if(input$regmethod == "Summary"){
      summary(lmout())
    } else if (input$regmethod == "ANOVA"){
      anova(lmout())
    }
  })
  
  output$regplot <- renderPlot({
    par(mfrow=c(2,2))
    plot(lmout())
  })
  
  # efa & sem
  
  faout <- reactive({
    
    df <- data_input()
    
    fit <- fa(df, input$nf1)
    return(fit)
  }
  
  )
  
  omegaout <- reactive({
    df <- data_input()
    fit <- omega(df)
    return(fit)
  })
  
  output$efaoutput <- renderPrint({
    if(input$efaplot == "I don't know anything; please fit EFA"){
      omegaout()
    } else {
      faout()
    }
    
  })
  
  output$efadiagram <- renderPlot({
    if(input$efaplot == "Structure Diagram"){
      structure.diagram(faout())
    } else if(input$efaomegaplot == "Omega Plot"){
      omega.diagram(omegaout())
    }
  })
  
  # Logistic Regression
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "logrvar", choices = names(data_input()))
  }
  )
  
  logrout <- reactive({
    
    df <- data_input()
    
    var <- input$logrvar
    
    Train <- createDataPartition(df[, var], p=as.numeric(input$logrprop), list=FALSE)
    training <- df[Train, ]
    testing <- df[-Train, ]
    
    trainprop <- nrow(training)/(nrow(testing)+nrow(training))
    
    # var1 <- input$logryname
    
    mod_fit <- train(as.formula(paste(var, "~", ".")),  data=training, method="glm", family="binomial")
    
    expout <- exp(coef(mod_fit$finalModel))
    
    if (input$logroption == "Show Prop."){
      return(trainprop)
    } else if (input$logroption == "Fit"){
      return(mod_fit)
    }
    
    if (input$logroption == "Coef."){
      return(data.frame(expout))
    }
    
    predout <- predict(mod_fit, newdata=testing)
    # predoutproba <- predict(mod_fit, newdata=testing, type="prob")
    accuracy <- table(predout, testing[, input$logrvar])
    out <- sum(diag(accuracy))/sum(accuracy)
    
    confmat <- confusionMatrix(round(predout), testing[, input$logrvar])
    
    if (input$logroption == "Pred. Accuracy"){
      return(confmat)
    }
    return(var1)
  })
  
  
  output$logroutput <- renderPrint({
    
    if(input$logroption == "Coef."){
      logrout()
    } else if (input$logroption == "Show Prop."){
      logrout()
    } else if (input$logroption == "Fit"){
      logrout()
    } else if (input$logroption == "Pred. Accuracy"){
      logrout()
    }
    
  })
  
  # KNN
  
  knnout <- reactive({
    
    df <- data_input()
    
    rows <- round(as.numeric(input$knntrain)*dim(df)[1])
    
    if(input$knnoption == "Show Prop."){
      return(rows)  
    }
    
    lascol <- dim(df)[2]
    
    train_data <- df[1:rows, 2:lascol]
    test_data <- df[-(1:rows), 2:lascol]
    
    if(input$knnoption == "Show Train & Test Data"){
      return(list(head(train_data), head(test_data)))
    }
    
    train_labels <- df[1:rows, 1]
    test_labels <- df[-(1:rows), 1]
    
    k_ = round(sqrt(dim(df)[1]))
    
    if(input$knnoption == "Show No. of Classes"){
      return(k_)
    }
    
    fit <- knn(train = train_data, test = test_data, cl = train_labels, k = k_)
    
    if(input$knnoption == "Fit"){
      return(data.frame(fit))
    }
    
    out <- CrossTable(x = test_labels, y = fit, prop.chisq = FALSE)
    
    if(input$knnoption == "Accuracy"){
      return(out)
    }
    
  })
  
  output$knnoutput <- renderPrint({
    if(input$knnoption == "Show Prop."){
      knnout()
    } else if(input$knnoption == "Show Train & Test Data"){
      knnout()
    } else if(input$knnoption == "Show No. of Classes"){
      knnout()
    } else if(input$knnoption == "Fit"){
      knnout()
    } else if(input$knnoption == "Accuracy"){
      knnout()
    }
    
  })
  
  
  # SVM
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "svmvar", choices = names(data_input()))
  }
  )
  
  
  svmout <- reactive({
    
    df <- data_input()
    
    intrain <- createDataPartition(y = df[, input$svmvar], p= as.numeric(input$svmprop), list = FALSE)
    training <- df[intrain,]
    testing <- df[-intrain,]
    
    if (input$svmoption == "Show Prop."){
      return(list(dim(training), dim(testing)))
    }
    
    training[,input$svmvar] = factor(training[,input$svmvar])
    
    var <- input$svmvar
    
    trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
    
    svm_Linear <- train(as.formula(paste(var, "~", ".")), data = training, method = "svmLinear",
                        trControl=trctrl,
                        preProcess = c("center", "scale"),
                        tuneLength = 10)
    
    if (input$svmoption == "Fit"){
      return(svm_Linear)
    }
    
    test_pred <- predict(svm_Linear, newdata = testing)
    
    if (input$svmoption == "Predicted"){
      return(data.frame(test_pred))
    }
    
    confmat <- confusionMatrix(test_pred, testing[, "num"] )
    
    if (input$svmoption == "Pred. Accuracy"){
      return(confmat)
    }
    
  })
  
  output$svmoutput <- renderPrint({
    
    if(input$svmoption == "Coef."){
      svmout()
    } else if (input$svmoption == "Show Prop."){
      svmout()
    } else if (input$svmoption == "Fit"){
      svmout()
    } else if (input$svmoption == "Pred. Accuracy"){
      svmout()
    } else if (input$svmoption == "Predicted"){
      svmout()
    }
    
  })
  
  # DECISION TREE
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "dtyname", choices = names(data_input()))
  }
  )
  
  dtout <- reactive({
    
    df <- data_input()
    tab = table(df[, input$dtyname])
    
    if (input$dtoption == "Table"){
      return(tab)
    }
    # # Matrix
    # air <- as.matrix(air)
    # dimnames(air) <- NULL
    # 
    # # Partition
    # set.seed(1234)
    # ind <- sample(2, nrow(air), replace = T, prob = c(.7, .3))
    # training <- air[ind==1,1:5]
    # test <- air[ind==2, 1:5]
    # trainingtarget <- air[ind==1, 6]
    # testtarget <- air[ind==2, 6]
    # 
    index = createDataPartition(y=df[, input$dtyname], p=0.7, list=FALSE)
    
    train.set = df[index,]
    test.set = df[-index,]
    
    if (input$dtoption == "Train & Test Data"){
      return(list(head(train.set), head(test.set)))
    }
    
    if (input$dtoption == "Show Prop."){
      return(dim(train.set))
    }
    
    var <- input$dtvar
    
    brest.tree = train(as.formula(paste(PM25, "~", ".")),
                       data=train.set,
                       method="rpart",
                       trControl = trainControl(method = "cv"))
    
    if (input$dtplot == "QPlot"){
      
      plot(brest.tree$finalModel, uniform=TRUE, main="Classification Tree"); text(brest.tree$finalModel, use.n.=TRUE, all=TRUE, cex=.8)
    }
    
    
    if (input$dtoption == "Fit"){
      return(brest.tree)
    }
    
    
    if (input$dtplot == "DTree"){
      fancyRpartPlot(brest.tree$finalModel)
    }
    
    pred <- predict(brest.tree, test.set)
    out <- confusionMatrix(pred, test.set[, "Class_num"])
    
    if (input$dtoption == "Predicted"){
      return(data.frame(pred))
    }
    
    if (input$dtoption == "Pred. Accuracy"){
      return(out)
    }
    
  })
  
  output$dtoutput <- renderPrint({
    dtout()
  })
  
  output$dtplot <- renderPlot({
    if (input$dtplot == "QPlot"){
      dtout()
    } else if (input$dtplot == "DTree"){
      dtout()
    } else if (input$dtoption == "Pred. Accuracy"){
      dtout()
    } else if (input$dtoption == "Predicted"){
      dtout()
    }
  })  
  
  # RANDOM FOREST
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "rfvar", choices = names(data_input()))
  }
  )
  rf_fit <- readRDS("random_forest.rds")
  rfout <- reactive({
    df <- data_input()
    
    if (input$rfoption == "Table"){
      return(table(df[, input$rfvar]))
    }
    
    # train_index <- sample(1:nrow(df), as.numeric(input$rfprop) * nrow(df))
    set.seed(1234)
    ind <- sample(2, nrow(df), replace = T, prob = c(.8, .2))
    train_set <- df[ind==1, ]
    test_set <- df[ind==2, ]
    
    
    if (input$rfoption == "Show Prop."){
      return(dim(train_set)[1]/dim(df)[1])
    }
    
    
    if (input$rfoption == "Train & Test Data"){
      return(list(head(train_set), head(test_set), dim(train_set), dim(test_set)))
    }
    
    var <- input$rfvar
    rf_fit <- readRDS("random_forest.rds")
    p1 <- predict(rf_fit, test_set)
    
    if (input$rfoption == "Fit"){
      return(rf_fit)
    }
    
    if (input$rfoption == "Importance"){
      return(rf_fit$importance)
    }
    
    if (input$rfoption == "Pred. Accuracy"){
      return(RMSE(p1, test_set$PM25))
    }
    if (input$rfoption == "Summary"){
      return(rf_fit)
    }
    # return(out)
  })
  
  
  output$rfoutput <- renderPrint({
    rfout()
  })
  
  output$rfplot <- renderPlot({
 if (input$rfoption =="Importance"){
   varImpPlot(rf_fit,
              sort = T,
              n.var = 10,
              main = "Top 10"
   )
 }
    if (input$rfoption =="Pred. Accuracy"){
      plot(rf, log = "y")
    }
  
    if (input$rfoption =="Summary"){
      hist(treesize(rf_fit),
           main = "No of nodes for the trees",
           col = "green"
      )
    }
  })
  
  
  
  # XGBoost
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "nbvar", choices = names(data_input()))
  }
  )
  model <- readRDS("xgb.rds")
  nbout <- reactive({
    
    df <- data_input()
    
    set.seed(1234)
    ind <- sample(2, nrow(df), replace = T, prob = c(.8, .2))
    train <- df[ind==1, ]
    test <- df[ind==2, ]
    
    t_train <- setDT(train)
    t_test <- setDT(test)
    labels <- df[ind == 1, 8]
    ts_labels <- df[ind == 2, 8]
    
    
    dtrain <- xgb.DMatrix(label = labels, data = as.matrix(train))
    dtest <- xgb.DMatrix(label = ts_labels, data = as.matrix(test))
    
    
    if (input$nboption == "Table"){
      return(table(df[, input$nbvar]))
    }
    
    if (input$nboption == "Show Prop."){
      return(dim(train)[1]/dim(df)[1])
    }
    
    if (input$nboption == "Train & Test Data"){
      return(list(head(train), head(test)))
    }
    
    # train xgb
    model <- readRDS("xgb.rds")
    
    if (input$nboption == "Fit"){
      return(model)
    }
    
    predictions <- predict(model, dtest)
    importance_matrix <- xgb.importance(model = model)
    
    if (input$nboption == "Importance"){
      return(importance_matrix)
    }
    # summarize results
  
    
    if (input$nboption == "Pred. Accuracy"){
      return(RMSE(predictions,ts_labels))
    }
  })
  
  output$nboutput <- renderPrint({
    nbout()
  })
  output$xgbplot <- renderPlot({
    if (input$nboption =="Importance"){
      mat <- xgb.importance(feature_names = colnames(dtrain), model = model)
      xgb.plot.importance(importance_matrix = mat[1:10])
    }
    if (input$nboption =="Pred. Accuracy"){
      xgb.plot.tree(
        feature_names = NULL, model = model, trees = 10,
        plot_width = 800, plot_height = 600, render = TRUE,
        show_node_id = FALSE
      )
    }
  })
  # NEURAL NETWORKS
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "nnvar", choices = names(data_input()))
  }
  )
  
  nnout <- reactive({
    df <- data_input()
    
    if (input$nnoption == "Table"){
      return(table(df[, input$nnvar]))
    }
    inTrain <- createDataPartition(df[, input$nnvar], p=as.numeric(input$nnprop), list=FALSE)
    
    train.set <- df[inTrain,]
    test.set  <- df[-inTrain,]
    
    if (input$nnoption == "Show Prop."){
      return(dim(train.set)[1]/dim(df)[1])
    }
    
    if (input$nnoption == "Train & Test Data"){
      return(list(head(train.set), head(test.set)))
    }
    
    var <- input$nnvar
    hl1 <- as.numeric(input$nnhl1)
    hl2 <- as.numeric(input$nnhl2)
    
    nngrid <- expand.grid(
      layer1 = hl1, 
      layer2 = hl2,
      layer3 = 1
    )
    
    model <- train(as.formula(paste(var, "~", ".")), train.set, method="nnet", trace = FALSE)
    # model <- neuralnet(as.formula(paste(var, "~", ".")), data=train.set, hidden = c(5, 3), linear.output=T)
    
    if (input$nnoption == "Fit"){
      return(model)
    }
    
    if (input$nnoption == "Summary"){
      return(summary(model))
    }
    
    prediction <- predict(model, test.set) 
    
    out <- confusionMatrix(round(prediction), test.set[, var])
    
    if (input$nnoption == "Predictions"){
      return(prediction)
    }
    
    if (input$nnoption == "Pred. Accuracy"){
      return(out)
    }
    
    if (input$nnoption == "Plot"){
      plotnet(model)
    }
    # if (input$nnplottype == "Weight Decay"){
    #   plot(model)
    # }
    # 
    # if (input$nnplottype == "Network Plot"){
    #   plotnet(model)
    # }
  })
  
  
  output$nnoutput <- renderPrint({
    nnout()
  })
  
  output$nnplot <- renderPlot({
    # if (input$nnplottype == "Weight Decay"){
    #   nnout()
    # } else if (input$nnplottype == "Network Plot"){
    #   nnout()
    # }
    
    if (input$nnoption == "Plot"){
      nnout()
    }
  })
  
  
  # UNSUPERVISED LEARNING
  # PCA
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "pcavar", choices = names(data_input()))
  }
  )
  
  pcaout <- reactive({
    df <- data_input()
    
    if (input$pcaoption == "Table"){
      return(table(df[, input$pcavar]))
    }
    
    inTrain <- createDataPartition(y=df[, input$pcavar], p=as.numeric(input$pcaprop), list=FALSE)
    
    training <- df[inTrain,]
    testing <- df[-inTrain,]
    
    if (input$pcaoption == "Show Prop."){
      return(dim(training)[1]/dim(df)[1])
    }
    
    preProc <- preProcess(training, method="pca", pcaComp=2)
    
    trainPC <- predict(preProc, training)
    
    if (input$pcaoption == "Components"){
      return(trainPC)
    }
    
    var <- input$pcavar
    
    modelFit <- train(as.formula(paste(var, "~", ".")), method="glm", data=training)
    
    if (input$pcaoption == "Fit"){
      return(modelFit)
    }
    
    testPC <- predict(modelFit,testing)
    
    if (input$pcaoption == "Predictions"){
      return(data.frame(testPC))
    }
    
    out <- confusionMatrix(round(testPC), testing[, var])
    
    if (input$pcaoption == "Mod. Accuracy"){
      return(out)
    }
    
  })
  
  output$pcaoutput <- renderPrint({
    pcaout()
  })
  
  # Gaussina Mixture
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "gmvar", choices = names(data_input()))
  }
  )
  
  gmout <- reactive({
    
    df <- data_input()
    
    if (input$gmoption == "Table"){
      return(table(df[, input$gmvar]))
    }
    
    class <- df[, input$gmvar]
    X <- df[, -c(1, 2)]
    
    if (input$gmoption == "Data"){
      return(head(X))
    }
    
    if (input$gmplottype == "CLPairs"){
      clPairs(X, class)
    }
    
    BIC <- mclustBIC(X)
    
    if (input$gmplottype == "BIC"){
      plot(BIC)
    }
    
    if (input$gmoption == "BIC"){
      return(BIC)
    }
    
    mod1 <- Mclust(X, x = BIC)
    
    if (input$gmoption == "Fit"){
      return(mod1)
    }
    
    if (input$gmoption == "Summary"){
      return(summary(mod1, parameters = TRUE))
    }
    
    if (input$gmplottype == "Classification"){
      plot(mod1, what = "classification")
    }
    
    if (input$gmoption == "Predictions-GMM"){
      table(class, mod1$classification)
    }
    
    if (input$gmplottype == "Uncertainity"){
      plot(mod1, what = "uncertainty")
    }
    
    # confusion matrix
    
    hclust.avg <- hclust(dist(X),method="average")
    
    if (input$gmoption == "HC"){
      
      gps <- c(names(X))[cutree(hclust.avg,3)]
      out <- table(true=class,pred=gps)
      return(out)
    }
    
    if (input$gmplottype == "HC"){
      plot(hclust.avg)
    }
    
    
  })
  
  output$gmoutput <- renderPrint({
    gmout()
  })
  
  output$gmplot <- renderPlot({
    if (input$gmplottype == "CLPairs"){
      gmout()
    }
    
    if (input$gmplottype == "BIC"){
      gmout()
    }
    
    if (input$gmplottype == "Classification"){
      gmout()
    }
    if (input$gmplottype == "Uncertainity"){
      gmout()
    }
    if (input$gmplottype == "HC"){
      gmout()
    }
    
  })
  
  
  
  # ANOMALY DETECTION (in testing with iris data set)
  
  mlout <- reactive({
    
    df <- data_input()
    
    micad.model <- micad(x=iris_anomaly[iris_anomaly$ANOMALY==0,], 
                         vars=c("SEPAL_LENGTH","SEPAL_WIDTH",
                                "PETAL_LENGTH","PETAL_WIDTH",
                                "SPECIES"),
                         weights=c(10,10,10,10,20))
    if (input$mloption == "Fit"){
      out <- micad.model
      return(out)
    }
    
    if (input$mloption == "Predictions"){
      scored.data <- predict(micad.model, iris_anomaly)
      out <- tail(scored.data)
      return(out)
    }
  })
  
  output$mloutput <- renderPrint({
    if (input$mloption == "Fit"){
      mlout()
    } else if (input$mloption == "Predictions"){
      mlout()
    }
  }
  )
  
  
  # # Fuzzy c-means clustering (prostate data)
  
  observeEvent(input$file1, {
    updateSelectInput(session, inputId = "fmcvar1", choices = names(data_input()))
    updateSelectInput(session, inputId = "fmcvar2", choices = names(data_input()))
  }
  )
  
  fmcout <- reactive({
    
    df <- data_input()
    
    result <- cmeans(df[-11], centers=3, iter.max=100, m=2, method="cmeans")  # 3 clusters
    
    if (input$fmcoption == "Fit"){
      return(result)
    }
    
    if (input$fmplottype == "Plot"){
      plot(df[, input$fmcvar2][1], df[, input$fmcvar2][2], col=result$cluster)
      points(result$centers[,c(1,2)], col=1:3, pch=19, cex=2)
    }
    
    if (input$fmcoption == "Memberships"){
      result$membership[1:5,]
      out <- table(df[, "prostate"], result$cluster)
      return(out)
    }
    
  })
  
  output$fmcoutput <- renderPrint({
    fmcout()
  })  
  
  output$fmcplot <- renderPlot({
    if (input$fmcplottype == "Plot"){
      fmcout() 
    }
  })
  
  
  # Contact Information 
  
  output$text1 <- renderText({
    str1 <- paste("LUCKY THIRTEEN") 
    str2 <- paste("Vishnu") 
    str3 <- paste("Teslin Rose") 
    str4 <- paste("Vini") 
    str5 <- paste("Sruthi") 
    str6 <- paste("***********************")
    str7 <- paste("optra7@gmail.com")
    str8 <- paste("9562515945")
    
    
    HTML(paste(str1, str2, str3, str4, str5, str6, str7, str8, sep = '<br/>'))
  })
  
}



shinyApp(ui, server)