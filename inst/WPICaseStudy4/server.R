library(rsconnect)
library(shiny)
library(farff)
library(stringr) 
library(ROCR)
library(popbio)
library(visreg)
library(rsconnect)
library(readr)
library(caret)

clDataSet<- read.csv("clData.csv")
server <- function(input, output){
  output$eth<- renderDataTable(
    {
      ethnC<- as.matrix(table(clDataSet$ethnicity)) 
      ethnC})
  output$ageB<- renderPlot({boxplot(summary(clDataSet$age), ylim = range(1:15), main = "Boxplot of Age", ylab = "Age (years)")})
  output$jun<- renderPlot(
    {
      junC<- as.data.frame(table(clDataSet$jundice))
      barplot(junC$Freq, names.arg =  c("no", "yes"), main = "Born with Jaundice?", ylim = range(0:200), ylab = "Frequency")})
  output$asd<- renderPlot({
    
    ausC<- as.data.frame(table(clDataSet$austim))
    barplot(ausC$Freq, names.arg =  c("no", "yes"), main = "Does an Immediate Family Member have ASD?", ylab = "Frequency")})
  output$rel<- renderPlot({
    
    relC<- as.data.frame(table(clDataSet$relation ))
    barplot(relC$Freq, names.arg = relC$Var1, main = "Who is Completing the Test? ", ylab = "Frequency")})
  output$asdr<- renderPlot({
    
    ASDC<- as.data.frame(table(clDataSet$Class.ASD))
    barplot(ASDC$Freq, names.arg = c("no", "yes"), main = "Does Patient have symptoms of ASD?", ylab = "Frequency", ylim = range(0:150))})
  output$other_var_select<-renderUI({
    checkboxGroupInput("other_var_select","Select the dependent variables you want to look at (Must pick at least one to work",choices =as.list(names(clDataSet[2:17])))
  })
  output$other_var_select2<-renderUI({
    checkboxGroupInput("other_var_select2","Select the dependent variables you want to look at (Must pick at least one to work",choices =as.list(names(clDataSet[2:17])))
  })
  output$other_val_show<-renderPrint({
    input$other_var_select
    f<-clDataSet
    
    form <- sprintf("Class.ASD ~ %s",paste0(input$other_var_select,collapse="+"))
    print(form)
    
    ASDLogreg <-glm(as.formula(form),family=binomial(link="logit"),data=f)
    print(summary(ASDLogreg))
    print(confint(ASDLogreg))
  })
  output$train_show<-renderPrint({
    input$other_var_select2
    set.seed(2015)
    splitASD = caret::createDataPartition(clDataSet[,1], p = 0.8, list=F, times=1)
    trainASD = clDataSet[splitASD,]
    testASD = clDataSet[!row.names(clDataSet) %in% row.names(trainASD),]
    s<-trainASD
    
    form <- sprintf("Class.ASD ~  %s",paste0(input$other_var_select2,collapse="+"))
    print(form)
    
    trainASDLR = glm(as.formula(form), data=s, family=binomial(link="logit"))
    print(summary(trainASDLR))
    print(confint(trainASDLR))
    
    Phat = predict(trainASDLR,testASD,type="response")
    head(Phat)
    prop.table(xtabs(~ Class.ASD, data=testASD))
    thresh = 0.5
    facHat = cut(Phat, breaks=c(-Inf, thresh, Inf), labels=c(0, 1))
    cTab   = xtabs(~ Class.ASD + facHat, data=testASD)
    addmargins(cTab)
    
    CCR = sum(diag(cTab)) / sum(cTab)
    sent <- sprintf("The rate of correct classifictaion is: %f",CCR)
    print(sent)
  })
  
  output$g<- renderPlot({
    input$other_var_select2
    set.seed(2015)
    splitASD = caret::createDataPartition(clDataSet[,1], p = 0.8, list=F, times=1)
    trainASD = clDataSet[splitASD,]
    testASD = clDataSet[!row.names(clDataSet) %in% row.names(trainASD),]
    s<-trainASD
    library(caret)
    form <- sprintf("Class.ASD ~  %s",paste0(input$other_var_select2,collapse="+"))
    
    logReg = glm(as.formula(form), data=s, family=binomial(link="logit"))
    visreg::visreg(logReg, "age", scale="response", partial=FALSE, xlab="Age", ylab="P(ASD)", rug=2)
  })
  
  output$g2<- renderPlot({
    input$other_var_select2
    set.seed(2015)
    splitASD = caret::createDataPartition(clDataSet[,1], p = 0.8, list=F, times=1)
    trainASD = clDataSet[splitASD,]
    testASD = clDataSet[!row.names(clDataSet) %in% row.names(trainASD),]
    s<-trainASD
    library(caret)
    form <- sprintf("Class.ASD ~  %s",paste0(input$other_var_select2,collapse="+"))
    print(form)
    
    trainASDLR = glm(as.formula(form), data=s, family=binomial(link="logit"))
    pred = predict(trainASDLR, testASD[,2:17], type="response")
    
    pObject = ROCR::prediction(pred, testASD$Class.ASD )
    
    rocObj = ROCR::performance(pObject, measure="tpr", x.measure="fpr")
    aucObj = ROCR::performance(pObject, measure="auc")  
    plot(rocObj, main = paste("Area under the curve:", round(aucObj@y.values[[1]] ,4), sub = "\n How well the model predicts if the subject as ASD based on given data" )) 
  })
  output$g3<- renderPlot({
    set.seed(2015)
    splitASD = caret::createDataPartition(clDataSet[,1], p = 0.8, list=F, times=1)
    trainASD = clDataSet[splitASD,]
    trainASDRandom = trainASD
    set.seed(1235)
    trainASDRandom$Class.ASD = sample(c(0,1), replace=T, size=nrow(trainASD))
    input$other_var_select2
    r<-trainASD
    form <- sprintf("Class.ASD ~  %s",paste0(input$other_var_select2,collapse="+"))
    print(form)
    logRegRandom = glm(as.formula(form), data=r, family=binomial(link="logit"))
    trainASDRandom$Class.ASD = sample(c(0,1), replace=T, size=nrow(trainASD))
    rand_pred = predict(logRegRandom, trainASD[,2:17], type="response")
    randObject = ROCR::prediction(rand_pred, trainASD$Class.ASD )
    
    rocRandObj = ROCR::performance(randObject, measure="tpr", x.measure="fpr")
    aucRandObj = ROCR::performance(randObject, measure="auc")  
    plot(rocRandObj, main = paste("Area under the curve:", round(aucRandObj@y.values[[1]] ,4), sub = "\n How well the model predicts if the subject as ASD on random data")) 
  })
}
