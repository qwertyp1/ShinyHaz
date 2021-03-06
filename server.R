# setwd("/Users/kate.hu/addhazard_shiny") # to do: finish
# to do: kate
# practice
#data<-read.csv("aric.csv", header=TRUE)
#aric$survtime<-aric$SURVTIME+runif(dim(aric)[1],0,1)*1e-8
#write.csv(aric, file="newaric.csv")
#data<-read.csv("newaric.csv", header=TRUE)

sourceDir <- function(path, trace = TRUE, ...) {
  for (nm in list.files(path, pattern = "\\.[RrSsQq]$")) {
    if(trace) cat(nm,":")           
    source(file.path(path, nm), ...)
    if(trace) cat("\n")
  }
}
# sourceDir("/Users/kate.hu/Documents/ah/R",trace=TRUE)
library(shiny)
library(xtable)
library("addhazard")
library("survival")

shinyServer(function(input, output) {
  
  data  <- reactive({
   inFile <- input$file1
   
   k <- nchar(inFile) # length of filepath
   if (substr(inFile[[1]], k-2, k) == "csv"){
      dat <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
   
   } else if (substr(inFile[[1]], k-2, k) == "txt"){
      dat <- read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
   
   } else if (substr(inFile[[1]], k-3, k) == "xlsx"){
      library(xlsx)
      if (input$header == T){
         dat <- read.xlsx2(inFile$datapath, sheetIndex=1, startRow=1)
      } else { 
         dat <- read.xlsx2(inFile$datapath, sheetIndex=1, startRow=2)
      } 
   
   } else if (substr(inFile[[1]], k-2, k) == "dta"){
      library(foreign)
      dat <- read.dta(inFile$datapath)
         
   } else if (substr(inFile[[1]], k-2, k) == "sav"){
      library(memisc)
      dat <- as.data.set(spss.system.file(inFile$datapath))
   }
   
   dat <-data.frame(dat)
   return(dat)
   })
  
  #mydata <- dat()
  
  
  output$contents <- renderDataTable(
      {
        # input$file1 will be NULL initially. After the user selects and uploads a
        # file, it will be a data frame with 'name', 'size', 'type', and 'datapath'
        # columns. The 'datapath' column will contain the local filenames where the
        # data can be found.
        
        inFile <- input$file1
        if (is.null(inFile))
           return(NULL) else
        #read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
           return(data())
      }, options = list(pageLength = 10))
  

 
  output$surv <- renderUI({
   
    selectInput("surv", label = h5("Survival Outcomes"), choices = names(data()))
  })
  
  
  output$cen <- renderUI({
    selectInput("cen", label = h5("Censoring Indicator"), choices = names(data()))
  })
  
  output$covariates <- renderUI({
  selectizeInput("covariates", label = h5("Multi-select Covariates"), choices = names(data()), multiple = TRUE)
  })
  
  
  output$phase1<- renderUI({
    selectInput("ph1.cov", label = h5("Phase I Covariates"), choices = names(data()), multiple = TRUE)
  })
  
  output$phase2 <- renderUI({
    selectInput("ph2.cov", label = h5("Phase II Covariates"), choices = names(data()), multiple = TRUE)
  })
  
  output$R <- renderUI({
    selectInput("R", label = h5("Phase II Membership"), choices = names(data()))
  })
  
  output$weights<- renderUI({
    selectInput("weights", label = h5("Weights"), choices = names(data()))
  })
  
  output$p2probs <- renderUI({
    selectInput("p2probs", label = h5("Phase II Subsampling Probabilities"), choices = names(data()))
                
  })
  
  output$cal <- renderUI({
    selectInput("cal", label = h5("Calibration Variables"), choices = names(data()), multiple = TRUE,
                selected = NULL)
  })
  
# output$regTab <- renderTable({    
#    inFile <- input$file1
#    if (input$Sampling == "Random Sampling"){
#    data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
#    fit1 <- ah(Surv(input$surv, input$cen) ~ input$covariates, data = data)
#     summary(fit1)
#    }
# })

# output$plot <- renderPlot({
#   dat <- get(input$dataset)
#   hist(input$surv)
# })

  ## "Fit Model" button
  observeEvent(input$fitModel, {
  
      inFile <- input$file1
      dat <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
    
      ## regular Cox model
      #fit1 <- coxph(as.formula(paste("Surv(", input$surv,",", input$cen,") ~ ",paste(input$covariates,collapse="+"))), data=dat)
      
      ## additive hazards model
      if (input$twophase==F){
         #if (input$wgts==T){
         #  dat$w <- dat[, unlist(input$weights)]
         #  fit1 <-ah(as.formula(paste("Surv(", input$surv,",", input$cen,") ~ ",paste(input$covariates,collapse="+"))),
         #       data=dat, robust=input$robust, ties=input$ties, weights=w)
         #} else {
           fit1 <-ah(as.formula(paste("Surv(", input$surv,",", input$cen,") ~ ",paste(input$covariates,collapse="+"))),
                     data=dat, robust=input$robust, ties=input$ties)
         #}
      ## additive hazards model with 2-phase sampling
      } else {
         dat$R <- dat[, unlist(input$R)]            
         dat$Pi <- dat[, unlist(input$p2probs)]
        
         ## currently with no consideration of calibration variables
         fit1 <-ah.2ph(as.formula(paste("Surv(", input$surv,",", input$cen,") ~ ",paste(input$covariates,collapse="+"))),
                       data=dat, robust=input$robust, R=R, Pi=Pi)
    
      }
      
      # ah() options: robust = T/F; weights; ties = T/F
      # ah.2ph() options: robust = T/F; R (indicator for being in phase 2; Pi: Pr(subject selected to phase 2 subsample)
      # calibration.variables: used to calibrate the weights
      
      # output$modelSummary <- renderPrint({   
      #     summary(fit1)  ## for checking purposes
      # })
      
      output$regTab <- renderTable({
          # headings: coef	 se	 lower.95	 upper.95	 z	 p.value
          summary(fit1)$coef
      }, caption = "Table 1. Regression Output")
      
  
  }) # end observeEvent
  
}) ## end document

# runRegression <- reactive({
#   #f1<-ah(as.formula(paste("Surv(", input$surv,",", input$cen,") ~ ",paste(input$covariates,collapse="+"))),data=data, ties=FALSE)
#   #mysummary(f1)$coef
#   
#   lm(as.formula(paste(input$surv," ~ ",paste(input$covariates,collapse="+"))),data=data)
#   
#   })

# output$regTab <- renderTable({
#   if(!is.null(input$covariates)){
#     summary(runRegression())$coefficients
#   } else {
#     print(data.frame(Warning="Please select Model Parameters."))
#   }
#  })



#  formula<-Surv(survtime,CHD)~crp+AGE+SEX+RACE+as.factor(SMOK)+SBP+LDL+HDL+DIABTS
 # fit1.2<-ah.2ph(formula,data=aric.LDL, R=aric.LDL$R, Pi=1/aric.LDL$wts, robust=robust,ties=FALSE)
  #su
  