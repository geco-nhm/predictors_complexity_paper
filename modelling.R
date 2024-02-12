# 1 Importing libraries ----
library(MIAmaxent)
library(PrevMap)
library(raster)
library(rgdal)
library(distances)
library(geoR)
library(usdm)
library(ape)
library(ncf)
library(stringr)
library(writexl)
library(readxl)
library(Metrics)
library(PresenceAbsence)
library(MLmetrics)
library(terra)
library(spdep)
library(gtools)
library(rms)
library(rgeos)
install.packages(c("remotes", "R.rsp"))
remotes::install_github("julienvollering/MIAmaxent", build_vignettes = TRUE)

# 2 Generating functions ----
#Function that assembles the data into the correct format
#Extracting data for north and south regions
#Equalising the number of presences and absences
assembleModelling <- function(dfModelling,unit,factorVariables,maskArea,regions)
{
  dataModelling <- rbind(dfModelling[which(dfModelling[[1]] == unit),],dfModelling[which(dfModelling[[1]] != unit),])
  colnames(dataModelling)[1] <- "RV"
  dataModelling$RV[which(dataModelling$RV != unit)] <- 0
  dataModelling$RV[which(dataModelling$RV == unit)] <- 1
  dataModelling <- dataModelling[order(dataModelling[,1],decreasing = T),]
  intersectPointsTrain <- over(SpatialPointsDataFrame(dataModelling[,c(2:3)], data = data.frame(as.numeric(rownames(dataModelling))), 
                                                      proj4string = crs(maskArea), bbox = bbox(maskArea), match.ID = F), regions) 
  
  trainingCV <- list()
  whichTraining <- list()
  trainingCV[[1]] <- dataModelling
  for (i in 1:length(unique(regionsNorway$OBJECTID))) {
    whichTraining[[i]] <- which(rownames(dataModelling) %in% rownames(intersectPointsTrain)[which(intersectPointsTrain == unique(regions$OBJECTID)[i])])
    trainingCV[[i+1]] <- dataModelling[whichTraining[[i]],]
  }
  for (i in 1:length(trainingCV)) {
    #Selecting as many presences as absences or vice versa
    presences <- trainingCV[[i]][which(trainingCV[[i]]$RV == 1),]
    absences <- trainingCV[[i]][which(trainingCV[[i]]$RV == 0),]
    if(nrow(presences) < nrow(absences)) {
      absences <- absences[sample(1:nrow(absences), nrow(presences), replace = F),]
    } else {
      presences <- presences[sample(1:nrow(presences), nrow(absences), replace = F),]
    }
    trainingCV[[i]] <- rbind(presences,absences)
    
    #Changing the class of all predictors
    for (j in 1:ncol(trainingCV[[i]])) {
      trainingCV[[i]][,j] <- as.numeric(trainingCV[[i]][,j])
    }
    if(length(which(colnames(trainingCV[[i]]) %in% factorVariables)) > 0){
      for (j in which(colnames(trainingCV[[i]]) %in% factorVariables)) {
        trainingCV[[i]][,j] <- as.factor(trainingCV[[i]][,j])
      }
    }
  }
  names(trainingCV) <- c("norway","north","south")
  return(trainingCV)
} 

#Choosing transformations for each predictor until the deviance explained 
#increases less than a given threshold
DVcomplexity <- function(modelComplexity,DVselect,DVselectData,transformedDVdata,factorDVs,trainingData)
{
  DVselection <- character()
  for (i in 1:length(DVselect)) {
    DVselection[i] <- NA
  }
  Ftest <- numeric()
  for(j in 1:length(DVselect)){ 
    #Reset the F-value recorder
    Ftest <- 0
    DVselect[[j]]$F[which(is.na(DVselect[[j]]$F))] <- 0
    for(i in 1:nrow(DVselect[[j]])) {
      if(is.na(DVselection[j]))
      {
        #If the proportion of deviance explained by adding a parameter in the stepwise forward selection 
        #is more than 25% of the null deviance, then record that parameter Otherwise, record the deviance
        #explained
        if(DVselect[[j]]$F[nrow(DVselect[[j]])-(i-1)] > modelComplexity/DVselect[[j]]$round[i] && DVselect[[j]]$F[nrow(DVselect[[j]])-(i-1)] > Ftest) 
        {
          #Record the selected transformations for each preduictor 
          DVselection[j] <- DVselect[[j]]$variables[which(DVselect[[j]]$round == DVselect[[j]]$round[nrow(DVselect[[j]])-(i-1)])[1]]
        } else {
          #Record the deviance explained
          Ftest <- DVselect[[j]]$F[nrow(DVselect[[j]])-(i-1)]
        }
      }
    }
  }
  #For the predictors where no transformations were chosen, add the predictor wwith the highest deviance explained
  for (i in which(is.na(DVselection))) {
    DVselection[i] <- DVselect[[i]]$variables[1]
  }
  #Add the transformed predictor values to a data frame which will be used in model fitting
  for (i in 1:(length(transformedDVdata)-1)) {
    if(i %in% factorDVs)
    {
      DVselectData[[i+1]] <- transformedDVdata[[i+1]]
      DVselectData[[i+1]] <- DVselectData[[i+1]][,-c(1)] 
    }
    else
    {
      newDVdata <- transformedDVdata[[i+1]][which(names(transformedDVdata[[i+1]]) == strsplit(gsub("\\+ ", DVselection[i], replacement = "") , " ")[[1]][1])]
      if(length(strsplit(gsub("\\+ ", DVselection[i], replacement = "") , " ")[[1]]) > 1)
      {
        for (k in 2:length(strsplit(gsub("\\+ ", DVselection[i], replacement = "") , " ")[[1]])) {
          newDVdata <- cbind(newDVdata,transformedDVdata[[i+1]][which(names(transformedDVdata[[i+1]]) == strsplit(gsub("\\+ ", DVselection[i], replacement = "") , " ")[[1]][k])])
        }
      }
      DVselectData[[i+1]] <- newDVdata
    }
  }
  names(DVselectData) <- colnames(trainingData[,-c(2:3)])
  return(DVselectData)
}

#Assembling the transformations selected for the entire study area into a data frame ready for model fitting
DVcomplexityCV <- function(DVselection,DVselectData,transformedDVdata,factorDVs,trainingData,trainingLastNames)
{
  #Finding the transformations that were used in the models for the entire study area
  DVselection <- DVselection[order(match(gsub("\\_.*", "",DVselection), names(transformedDVdata)[-c(1)]))]
  #Adding the transformed predictor values to a table that will be used for  model fitting
  for (i in 1:(length(transformedDVdata)-1)) {
    if(names(transformedDVdata)[i+1] %in% factorDVs)
    {
      DVselectData[[i+1]] <- transformedDVdata[[i+1]]
      DVselectData[[i+1]] <- DVselectData[[i+1]][,-c(1)]
    }
    else
    {
      newDVdata <- transformedDVdata[[i+1]][which(names(transformedDVdata[[i+1]]) %in% DVselection[which(match(gsub("\\_.*", "",DVselection), names(transformedDVdata)[-c(1)]) == i)])]
      DVselectData[[i+1]] <- newDVdata
    }
  }
  names(DVselectData) <- trainingLastNames
  return(DVselectData)
}

#Generating logistic regression models and adding predictors until the deviance explained 
#increases less than a given threshold
modelFitting <- function(modelComplexity,DVselectData,transformedDVtransformations,trainingData,testData,predictorSet,previousPredictorSets,previousModel,predCombination,allPredSets,predictorVar)
{
  #Extracting the formula for the previous model
  modelString <- character()
  for (i in 2:length(previousModel$coefficients)) {
    modelString <- c(modelString,gsub("\\_.*", "",names(previousModel$coefficients[i])))
  }
  modelString <- unique(modelString)
  
  #Excluding predictors from predictor sets that are not supposed to be in the model at this round (of the loop)
  excludingPredictors <- numeric()
  for (i in 1:length(modelString)) {
    excludingPredictors[i] <- modelString[i] %in% previousPredictorSets
  }
  modelString <- modelString[which(excludingPredictors != 0)]
  
  #If there are predictors in the model, a formula with more predictors must be constructed
  if(length(modelString) > 0) {
    #Making the formula for the model to be fitted
    modelStringFormula <- character()
    for (i in 1:length(modelString)) {
      modelStringFormula <- paste(modelStringFormula,modelString[i],sep = " + ")
    }
    modelStringFormula <- str_sub(modelStringFormula, start = 4)
    #If more than one predictor group goes into the model the pre-constructed formula is used
    #Otherwise the model is fitted with the predictor group which correspond with the current round
    if(length(which(predCombination != 0)) > 1){
      EVs <- selectEV(formula = paste("RV ~", modelStringFormula),
                      DVselectData[c(1, which(names(DVselectData) %in% predictorSet), which(names(DVselectData) %in% modelString))], 
                      alpha = 0.001, 
                      test = "F", 
                      interaction = F, 
                      algorithm = "LR")
    }
    else {EVs <- selectEV(DVselectData[c(1,which(names(DVselectData) %in% predictorSet))], 
                          alpha = 0.001, 
                          test = "F", 
                          interaction = F, 
                          algorithm = "LR")
    }
  } else {EVs <- selectEV(DVselectData[c(1,which(names(DVselectData) %in% predictorSet))], 
                          alpha = 0.001, 
                          test = "F", 
                          interaction = F, 
                          algorithm = "LR")
  }
  #Making objects to record which predictors are chosen and how much deviance they explain
  EVselection <- character()
  Ftest <- numeric()
  Ftest <- 0
  for(i in 1:nrow(EVs$selection)) {
    if(length(EVselection) > 0)
    {}
    else{
      #If the proportion of deviance explained by adding a predictor in the stepwise forward selection 
      #is more than 25% of the null deviance, then record that predictor. Otherwise, record the deviance
      #explained
      if(EVs$selection$F[nrow(EVs$selection)-(i-1)] > modelComplexity/EVs$selection$round[i] && EVs$selection$F[nrow(EVs$selection)-(i-1)] > Ftest) 
      {EVselection <- EVs$selection$variables[which(EVs$selection$round == EVs$selection$round[nrow(EVs$selection)-(i-1)])[1]]
      } else {
        #Recording the deviance explained
        Ftest <- EVs$selection$F[nrow(EVs$selection)-(i-1)]
      }
    }
  }
  #if no predictors had high enough deviance explained a null model is generated
  if(length(EVselection) == 0)
  {
    EVselection <- "random"
  }
  #Generating a logistic regression model with the chosen predictors
  selectedModel <- chooseModel(DVselectData, 
                               paste("y ~",EVselection), 
                               algorithm = "LR")
  
  #Model evaluation
  datTrain <- matrix(0,nrow(trainingData),4)
  datTrain[,1] <- 1:length(trainingData$RV)
  datTrain[,2] <- trainingData$RV
  #Predicted probabilities of presence for the training data
  datTrain[,3] <- selectedModel$fitted.values
  #Model residuals at trainig data locations
  datTrain[,4] <- selectedModel$residuals
  
  predictorValues <- predictorVar[cellFromXY((predictorVar), testData[,c(2:3)])[1:length(cellFromXY((predictorVar), testData[,c(2:3)]))]]
  projectTest <- projectModel(selectedModel, transformedDVtransformations, predictorValues)
  datVal <- matrix(0,nrow(testData),3)
  datVal[,1] <- 1:length(testData$RV)
  datVal[,2] <- testData$RV
  #Predicted probabilities of presence for the test data
  datVal[,3] <- projectTest$output$response
  
  evaluationDF <- numeric()
  #AUC
  evaluationDF[1] <- AUC(datTrain[,3], datTrain[,2])
  #Max Kappa
  #evaluationDF[2] <- ecospat.max.kappa(datTrain[,3], datTrain[,2])$max.Kappa
  #Max TSS
  #evaluationDF[3] <- ecospat.max.tss(datTrain[,3], datTrain[,2])$max.TSS
  #PRAUC
  evaluationDF[4] <- PRAUC(datTrain[,3], datTrain[,2])
  #Log loss
  evaluationDF[5] <- LogLoss(datTrain[,3], datTrain[,2])
  #Calibration metrics
  evaluationDF[6:23] <- val.prob(datTrain[,3], datTrain[,2])
  #Number of training observations
  evaluationDF[24] <- nrow(trainingData)
  #Prevalence
  evaluationDF[25] <- length(which(datTrain[,2] == 1))/nrow(trainingData)
  
  #AUC
  evaluationDF[26] <- AUC(datVal[,3], datVal[,2])
  #Max Kappa
  #evaluationDF[27] <- ecospat.max.kappa(datVal[,3], datVal[,2])$max.Kappa
  #Max TSS
  #evaluationDF[28] <- ecospat.max.tss(datVal[,3], datVal[,2])$max.TSS
  #PRAUC
  evaluationDF[29] <- PRAUC(datVal[,3], datVal[,2])
  #Log loss
  evaluationDF[30] <- LogLoss(datVal[,3], datVal[,2])
  #Calibration metrics
  evaluationDF[31:48] <- val.prob(datVal[,3], datVal[,2])
  #Number of test observations
  evaluationDF[49] <- nrow(testData)
  #Prevalence
  evaluationDF[50] <- length(which(datVal[,2] == 1))/nrow(testData)
  
  #Spatial autocorrelation (moran's I)
  #Using 1000 randomly selected values
  randomSample <- numeric()
  if(nrow(trainingData) > 1000){
    randomSample <- sample(1:nrow(trainingData), 1000, replace = F)
  } else {
    randomSample <- 1:nrow(trainingData)
  }
  SA <- moranTest(trainingData[,c(2:3)][randomSample,], selectedModel$residuals[randomSample], 3)
  evaluationDF[51:54] <- c(SA[[1]],SA[[2]],SA[[3]],SA[[4]])
  
  #Residual degrees of freedom
  evaluationDF[55] <- selectedModel$df.residual
  #Degrees of freedom (null model)
  evaluationDF[56] <- selectedModel$df.null
  #Null deviance
  evaluationDF[57] <- selectedModel$null.deviance
  #Residual deviance (%)
  evaluationDF[58] <- selectedModel$deviance/selectedModel$null.deviance
  #Deviance explained (%)
  evaluationDF[59] <- (selectedModel$null.deviance-selectedModel$deviance)/selectedModel$null.deviance
  #Number of predictors
  evaluationDF[60] <- length(unique(gsub("\\_.*","",names(selectedModel$coefficients))))-1
  #Number of parameters
  evaluationDF[61] <- length(selectedModel$coefficients)-1
  #Number of different transformations
  letters <- c("L","M","D","T","HF","HR","BX")
  for (i in 1:length(letters))  {
    if (length(which(rownames(table(gsub('[0-9]+', '', unique(gsub(".*\\_","",names(selectedModel$coefficients)))[-c(1)]))) == letters[i])) > 0) {
      evaluationDF[61+i] <- table(gsub('[0-9]+', '', gsub(".*\\_","",names(selectedModel$coefficients))[-c(1)]))[which(rownames(table(gsub('[0-9]+', '', unique(gsub(".*\\_","",names(selectedModel$coefficients)))[-c(1)]))) == letters[i])]
    }
    else  {
      evaluationDF[61+i] <- 0  
    }
  }
  for (i in 1:(length(allPredSets)-1)) {
    evaluationDF[68+i] <- ifelse(sum(unique(gsub("\\_.*","",names(selectedModel$coefficients)))[-c(1)] %in% allPredSets[[i]]) > 0, 1, 0)
  }
  evaluationDF[73:(72+length(colnames(trainingData)))] <- as.numeric(colnames(trainingData) %in% unique(gsub("\\_.*","",names(selectedModel$coefficients)))[-c(1)])
  return(list(selectedModel,evaluationDF,EVs))
}

#Generating logistic regression models for combinations of predictor groups
modelFittingAdd <- function(DVselectData,transformedDVtransformations,trainingData,testData,previousPredictorSets,allPredSets,predictorVar,previousModels,predictorComb)
{
  #Finding the models containing the predictor groups that should be included in the model to be fitted
  modelSets <- list()
  for (i in 1:length(which(predictorComb != 0))) {
    modelSets[[i]] <- previousModels[[as.numeric(predictorComb[which(predictorComb != 0)][i])]][[1]]
  }
  #Constructing the formula for the model to be fitted
  predictorString <- list()
  for (j in 1:length(modelSets)) {
    modelString <- character()
    for (i in 2:length(modelSets[[j]]$coefficients)) {
      modelString <- c(modelString,gsub("\\_.*", "",names(modelSets[[j]]$coefficients[i])))
    }
    modelStringFormula <- character()
    for (i in 1:length(modelString)) {
      modelStringFormula <- paste(modelStringFormula,modelString[i],sep = " + ")
    }
    modelStringFormula <- str_sub(modelStringFormula, start = 4)
    predictorString[[j]] <- modelStringFormula
  }
  
  finalString <- character()
  for (i in 1:length(predictorString)) {
    finalString <- paste(finalString,predictorString[[i]],sep = " + ")
  }
  finalString <- str_sub(finalString, start = 4)
  #If there are no predictors, make a null model
  if(length(finalString) == 0)
  {
    finalString <- "random"
  }
  #Generating a logistic regression model with the chosen predictors
  selectedModel <- chooseModel(DVselectData, 
                               paste("y ~",finalString), 
                               algorithm = "LR")
  
  #Model evaluation
  datTrain <- matrix(0,nrow(trainingData),4)
  datTrain[,1] <- 1:length(trainingData$RV)
  datTrain[,2] <- trainingData$RV
  #Predicted probabilities of presence for the training data
  datTrain[,3] <- selectedModel$fitted.values
  #Model residuals at trainig data locations
  datTrain[,4] <- selectedModel$residuals
  
  predictorValues <- predictorVar[cellFromXY((predictorVar), testData[,c(2:3)])[1:length(cellFromXY((predictorVar), testData[,c(2:3)]))]]
  projectTest <- projectModel(selectedModel, transformedDVtransformations, predictorValues)
  datVal <- matrix(0,nrow(testData),3)
  datVal[,1] <- 1:length(testData$RV)
  datVal[,2] <- testData$RV
  #Predicted probabilities of presence for the test data
  datVal[,3] <- projectTest$output$response
  
  evaluationDF <- numeric()
  #AUC
  evaluationDF[1] <- AUC(datTrain[,3], datTrain[,2])
  #Max Kappa
  #evaluationDF[2] <- ecospat.max.kappa(datTrain[,3], datTrain[,2])$max.Kappa
  #Max TSS
  #evaluationDF[3] <- ecospat.max.tss(datTrain[,3], datTrain[,2])$max.TSS
  #PRAUC
  evaluationDF[4] <- PRAUC(datTrain[,3], datTrain[,2])
  #Log loss
  evaluationDF[5] <- LogLoss(datTrain[,3], datTrain[,2])
  #Calibration metrics
  evaluationDF[6:23] <- val.prob(datTrain[,3], datTrain[,2])
  #Number of training observations
  evaluationDF[24] <- nrow(trainingData)
  #Prevalence
  evaluationDF[25] <- length(which(datTrain[,2] == 1))/nrow(trainingData)
  
  #AUC
  evaluationDF[26] <- AUC(datVal[,3], datVal[,2])
  #Max Kappa
  #evaluationDF[27] <- ecospat.max.kappa(datVal[,3], datVal[,2])$max.Kappa
  #Max TSS
  #evaluationDF[28] <- ecospat.max.tss(datVal[,3], datVal[,2])$max.TSS
  #PRAUC
  evaluationDF[29] <- PRAUC(datVal[,3], datVal[,2])
  #Log loss
  evaluationDF[30] <- LogLoss(datVal[,3], datVal[,2])
  #Calibration metrics
  evaluationDF[31:48] <- val.prob(datVal[,3], datVal[,2])
  #Number of test observations
  evaluationDF[49] <- nrow(testData)
  #Prevalence
  evaluationDF[50] <- length(which(datVal[,2] == 1))/nrow(testData)
  
  #Spatial autocorrelation (moran's I)
  randomSample <- numeric()
  if(nrow(trainingData) > 1000){
    randomSample <- sample(1:nrow(trainingData), 1000, replace = F)
  } else {
    randomSample <- 1:nrow(trainingData)
  }
  SA <- moranTest(trainingData[,c(2:3)][randomSample,], selectedModel$residuals[randomSample], 3)
  evaluationDF[51:54] <- c(SA[[1]],SA[[2]],SA[[3]],SA[[4]])
  
  #Residual degrees of freedom
  evaluationDF[55] <- selectedModel$df.residual
  #Degrees of freedom (null model)
  evaluationDF[56] <- selectedModel$df.null
  #Null deviance
  evaluationDF[57] <- selectedModel$null.deviance
  #Residual deviance (%)
  evaluationDF[58] <- selectedModel$deviance/selectedModel$null.deviance
  #Deviance explained (%)
  evaluationDF[59] <- (selectedModel$null.deviance-selectedModel$deviance)/selectedModel$null.deviance
  #Number of predictors
  evaluationDF[60] <- length(unique(gsub("\\_.*","",names(selectedModel$coefficients))))-1
  #Number of parameters
  evaluationDF[61] <- length(selectedModel$coefficients)-1
  #Number of different transformations
  letters <- c("L","M","D","T","HF","HR","BX")
  for (i in 1:length(letters))  {
    if (length(which(rownames(table(gsub('[0-9]+', '', unique(gsub(".*\\_","",names(selectedModel$coefficients)))[-c(1)]))) == letters[i])) > 0) {
      evaluationDF[61+i] <- table(gsub('[0-9]+', '', gsub(".*\\_","",names(selectedModel$coefficients))[-c(1)]))[which(rownames(table(gsub('[0-9]+', '', unique(gsub(".*\\_","",names(selectedModel$coefficients)))[-c(1)]))) == letters[i])]
    }
    else  {
      evaluationDF[61+i] <- 0  
    }
  }
  for (i in 1:(length(allPredSets)-1)) {
    evaluationDF[68+i] <- ifelse(sum(unique(gsub("\\_.*","",names(selectedModel$coefficients)))[-c(1)] %in% allPredSets[[i]]) > 0, 1, 0)
  }
  evaluationDF[73:(72+length(colnames(trainingData)))] <- as.numeric(colnames(trainingData) %in% unique(gsub("\\_.*","",names(selectedModel$coefficients)))[-c(1)])
  return(list(selectedModel,evaluationDF))
} 

#Fitting new logistic regression models with the same parameters as in the models for the entire study area
modelFittingAddCV <- function(lastModel,DVselectData,trainingData,testData,externalTrainData,externalTestData,transformedDVtransformations,predictorVar,allPredSets)
{
  #Making a formula with the predictors that should be included in the model to be fitted
  modelString <- character()
  for (i in 2:length(lastModel$coefficients)) {
    modelString <- c(modelString,gsub("\\_.*", "",names(lastModel$coefficients[i])))
  }
  modelStringFormula <- character()
  for (i in 1:length(modelString)) {
    modelStringFormula <- paste(modelStringFormula,modelString[i],sep = " + ")
  }
  modelStringFormula <- str_sub(modelStringFormula, start = 4)
  finalString <- modelStringFormula
  #If no predictors, make a null model
  if(length(finalString) == 0)
  {
    finalString <- "random"
  }
  #Fit the model with logistic regression
  selectedModel <- chooseModel(selectedDVsCross, 
                               paste("y ~",finalString), 
                               algorithm = "LR")
  
  #Model evaluation
  datTrain <- matrix(0,nrow(trainingData),4)
  datTrain[,1] <- 1:length(trainingData$RV)
  datTrain[,2] <- trainingData$RV
  #Predicted probabilities of presence for the training data
  datTrain[,3] <- selectedModel$fitted.values
  #Model residuals at training data locations
  datTrain[,4] <- selectedModel$residuals
  
  predictorValues <- predictorVar[cellFromXY((predictorVar), testData[,c(2:3)])[1:length(cellFromXY((predictorVar), testData[,c(2:3)]))]]
  projectTest <- projectModel(selectedModel, transformedDVtransformations, predictorValues)
  datVal <- matrix(0,nrow(testData),3)
  datVal[,1] <- 1:length(testData$RV)
  datVal[,2] <- testData$RV
  #Predicted probabilities of presence for the test data
  datVal[,3] <- projectTest$output$response
  
  #Model evaluation for extrapolation
  predictorValuesExtTrain <- predictorVar[cellFromXY((predictorVar), externalTrainData[,c(2:3)])[1:length(cellFromXY((predictorVar), externalTrainData[,c(2:3)]))]]
  projectTest <- projectModel(selectedModel, transformedDVtransformations, predictorValuesExtTrain)
  extTrain <- matrix(0,nrow(externalTrainData),3)
  extTrain[,1] <- 1:length(externalTrainData$RV)
  extTrain[,2] <- externalTrainData$RV
  extTrain[,3] <- projectTest$output$response
  
  predictorValuesExtTest <- predictorVar[cellFromXY((predictorVar), externalTestData[,c(2:3)])[1:length(cellFromXY((predictorVar), externalTestData[,c(2:3)]))]]
  projectTest <- projectModel(selectedModel, transformedDVtransformations, predictorValuesExtTest)
  extTest <- matrix(0,nrow(externalTestData),3)
  extTest[,1] <- 1:length(externalTestData$RV)
  extTest[,2] <- externalTestData$RV
  extTest[,3] <- projectTest$output$response
  
  evaluationDF <- numeric()
  #AUC
  evaluationDF[1] <- AUC(datTrain[,3], datTrain[,2])
  #Max Kappa
  #evaluationDF[2] <- ecospat.max.kappa(datTrain[,3], datTrain[,2])$max.Kappa
  #Max TSS
  #evaluationDF[3] <- ecospat.max.tss(datTrain[,3], datTrain[,2])$max.TSS
  #PRAUC
  evaluationDF[4] <- PRAUC(datTrain[,3], datTrain[,2])
  #Log loss
  evaluationDF[5] <- LogLoss(datTrain[,3], datTrain[,2])
  #Calibration metrics
  evaluationDF[6:23] <- val.prob(datTrain[,3], datTrain[,2])
  #Number of training observations
  evaluationDF[24] <- nrow(trainingData)
  #Prevalence
  evaluationDF[25] <- length(which(datTrain[,2] == 1))/nrow(trainingData)
  
  #AUC
  evaluationDF[26] <- AUC(datVal[,3], datVal[,2])
  #Max Kappa
  #evaluationDF[27] <- ecospat.max.kappa(datVal[,3], datVal[,2])$max.Kappa
  #Max TSS
  #evaluationDF[28] <- ecospat.max.tss(datVal[,3], datVal[,2])$max.TSS
  #PRAUC
  evaluationDF[29] <- PRAUC(datVal[,3], datVal[,2])
  #Log loss
  evaluationDF[30] <- LogLoss(datVal[,3], datVal[,2])
  #Calibration metrics
  evaluationDF[31:48] <- val.prob(datVal[,3], datVal[,2])
  #Number of test observations
  evaluationDF[49] <- nrow(testData)
  #Prevalence
  evaluationDF[50] <- length(which(datVal[,2] == 1))/nrow(testData)
  
  #AUC
  evaluationDF[51] <- AUC(extTrain[,3], extTrain[,2])
  #Max Kappa
  #evaluationDF[52] <- ecospat.max.kappa(extTrain[,3], extTrain[,2])$max.Kappa
  #Max TSS
  #evaluationDF[53] <- ecospat.max.tss(extTrain[,3], extTrain[,2])$max.TSS
  #PRAUC
  evaluationDF[54] <- PRAUC(extTrain[,3], extTrain[,2])
  #Log loss
  evaluationDF[55] <- LogLoss(extTrain[,3], extTrain[,2])
  #Calibration metrics
  evaluationDF[56:73] <- val.prob(extTrain[,3], extTrain[,2])
  #Number of test observations
  evaluationDF[74] <- nrow(externalTrainData)
  #Prevalence
  evaluationDF[75] <- length(which(extTrain[,2] == 1))/nrow(externalTrainData)
  
  #AUC
  evaluationDF[76] <- AUC(extTest[,3], extTest[,2])
  #Max Kappa
  #evaluationDF[77] <- ecospat.max.kappa(extTest[,3], extTest[,2])$max.Kappa
  #Max TSS
  #evaluationDF[78] <- ecospat.max.tss(extTest[,3], extTest[,2])$max.TSS
  #PRAUC
  evaluationDF[79] <- PRAUC(extTest[,3], extTest[,2])
  #Log loss
  evaluationDF[80] <- LogLoss(extTest[,3], extTest[,2])
  #Calibration metrics
  evaluationDF[81:98] <- val.prob(extTest[,3], extTest[,2])
  #Number of test observations
  evaluationDF[99] <- nrow(externalTestData)
  #Prevalence
  evaluationDF[100] <- length(which(extTest[,2] == 1))/nrow(externalTestData)
  
  #Spatial autocorrelation (moran's I)
  randomSample <- numeric()
  if(nrow(trainingData) > 1000){
    randomSample <- sample(1:nrow(trainingData), 1000, replace = F)
  } else {
    randomSample <- 1:nrow(trainingData)
  }
  SA <- moranTest(trainingData[,c(2:3)][randomSample,], selectedModel$residuals[randomSample], 3)
  evaluationDF[101:104] <- c(SA[[1]],SA[[2]],SA[[3]],SA[[4]])
  
  #Residual degrees of freedom
  evaluationDF[105] <- selectedModel$df.residual
  #Degrees of freedom (null model)
  evaluationDF[106] <- selectedModel$df.null
  #Null deviance
  evaluationDF[107] <- selectedModel$null.deviance
  #Residual deviance (%)
  evaluationDF[108] <- selectedModel$deviance/selectedModel$null.deviance
  #Deviance explained (%)
  evaluationDF[109] <- (selectedModel$null.deviance-selectedModel$deviance)/selectedModel$null.deviance
  #Number of predictors
  evaluationDF[110] <- length(unique(gsub("\\_.*","",names(selectedModel$coefficients))))-1
  #Number of parameters
  evaluationDF[111] <- length(selectedModel$coefficients)-1
  #Number of different transformations
  letters <- c("L","M","D","T","HF","HR","BX")
  for (i in 1:length(letters))  {
    if (length(which(rownames(table(gsub('[0-9]+', '', unique(gsub(".*\\_","",names(selectedModel$coefficients)))[-c(1)]))) == letters[i])) > 0) {
      evaluationDF[111+i] <- table(gsub('[0-9]+', '', gsub(".*\\_","",names(selectedModel$coefficients))[-c(1)]))[which(rownames(table(gsub('[0-9]+', '', unique(gsub(".*\\_","",names(selectedModel$coefficients)))[-c(1)]))) == letters[i])]
    }
    else  {
      evaluationDF[111+i] <- 0  
    }
  }
  for (i in 1:(length(allPredSets)-1)) {
    evaluationDF[118+i] <- ifelse(sum(unique(gsub("\\_.*","",names(selectedModel$coefficients)))[-c(1)] %in% allPredSets[[i]]) > 0, 1, 0)
  }
  evaluationDF[123:(122+length(colnames(trainingData)))] <- as.numeric(colnames(trainingData) %in% unique(gsub("\\_.*","",names(selectedModel$coefficients)))[-c(1)])
  return(list(selectedModel,evaluationDF))
}

#Moran's I
moranTest <- function(xy,residuals,neighbourDistance)
{
  dists <- as.matrix(dist(xy))
  dists.inv <- 1/dists
  diag(dists.inv) <- 0
  return(Moran.I(residuals, dists.inv))
}

# 3 Importing data ----
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 2/R/")
random <- raster("predictor_sets/random.tif")
maskLayer <- raster("predictor_sets/mask.tif")
combinationsPredictors <- as.data.frame(read_xlsx("combinations.xlsx"))
regionsNorway <- readOGR("study_area/north_south.shp")
crs(regionsNorway) <- crs(maskLayer)
region <- c("north","south")
folderNames <- c("terrain","surface","natural","anthropogenic","neutral")
predictorStacks <- list()
predictorSets <- list()
for (i in 1:length(folderNames)) {
  predictorSets[[i]] <- list.files(paste("predictor_sets/",folderNames[i], sep = ""),pattern="tif$", full.names=TRUE)
  predictorStacks[[i]] <- stack(predictorSets[[i]])
  predictorSets[[i]] <- gsub("predictor_sets/","",predictorSets[[i]])
  predictorSets[[i]] <- gsub(folderNames[i],"",predictorSets[[i]])
  predictorSets[[i]] <- gsub("/","",predictorSets[[i]])
  predictorSets[[i]] <- gsub(".tif","",predictorSets[[i]])
  predictorSets[[i]] <- gsub("_",".",predictorSets[[i]])
  names(predictorStacks[[i]]) <- predictorSets[[i]]
}

folderNames <- c("1","2","3","4","5")
names(predictorStacks) <- folderNames
names(predictorSets) <- folderNames
names(random) <- "random"

predictors <- stack(predictorStacks[[1]],predictorStacks[[2]],predictorStacks[[3]],predictorStacks[[4]],predictorStacks[[5]],random)

#Character vector with categorical predictors
categorical <- c("berggrunn","calcium.content","geo.norge123","losmasse","CLG.soil.type","CorineLandCover2012",
                 "CorineLandCover2018","arjordbr","arskogbon","artreslag","artype","arveget","LA","LAHT",
                 "CLG.glacier.presence","CLG.vegetation.cover","reindeer.presence","reindeer.grazing",
                 "reindeer.nonwinter.grazing","reindeer.winter.grazing","deer","moose","roe.deer","avalanches",
                 "floods","ruminant.grazing","landslide.floodslide","rock.slides","grunnvann.pot","infiltr.evne",
                 "CLG.terrain.variation","CLG.lake.abundance","CLG.inner.outer.coast","Snow.Duration","NeGibbsA",
                 "NeGibbsB","NeLaShuf1","NeLaShuf2","NeLaShuf3","conservation.area","treslag","bonitet","Snow.Dur.Mon",
                 "mask")

#change these object names
trainingData <- read.csv("data_sets/training_representative.csv")[,-c(1)]
testData <- read.csv("data_sets/test_random.csv")[,-c(1)]

#Checking sample size for each unit
obs <- matrix(0,60,2)
for (i in 1:60) {
  obs[i,1] <- length(which(trainingData$majorType == i))
  obs[i,2] <- length(which(testData$majorType == i))
}
obs

#Selecting model objects
modelObjects <- c(1,3,4,7,14,19,22,27,30,31,32,34,46,47,48)
for (p in modelObjects) {
  # 4 Preparing response variable ---- 
  #Assembling modelling training and test data sets
  #df = data frame with model object in the 1st column, x and y in column 2 and 3, and predictors in the following; the rows are observations
  #unit = numeric vector with number corresponding to position in modelObjects
  #factorVariables = character vector with names of predictors
  majorType <- p
  trainingAll <- assembleModelling(trainingData[,-c(1,3,4)],
                                   modelObjects[majorType],
                                   categorical,
                                   maskLayer,
                                   regionsNorway)
  testingAll <- assembleModelling(testData[,-c(1,3,4)],
                                  modelObjects[majorType],
                                  categorical,
                                  maskLayer,
                                  regionsNorway)
  
  training <- trainingAll[[1]]
  testing <- testingAll[[1]]
  
  # 5 Generating model to find the null deviance ----
  #Transforming predictors
  #Generating three types of transformations for all continuous predictors
  #one transformation type for categorical predictors
  transformationDVs <- deriveVars(training[,-c(2,3)], 
                                  algorithm = "LR", 
                                  transformtype = c("L","M","D","B"))
  #Generating model with practically no limit on model complexity
  #Selecting transformations for all predictors 
  DVfirstSelection <- selectDVforEV(transformationDVs$dvdata, 
                                    alpha = 0.001, 
                                    test = "F", 
                                    algorithm = "LR")
  #Selecting arbitrary predictor (this step is only performed to find the null deviance)
  EVfirstSelection <- chooseModel(DVfirstSelection$dvdata,
                                  paste("RV ~ ", names(DVfirstSelection$dvdata[2]), sep = ""), 
                                  algorithm = "LR")
  complexityValues <- c(1/(4^1),1/(4^2),1/(4^3),1/(4^4),1/(4^5))
  
  # 6 Modelling and evaluation ----
  #Assembling data frame for evaluation metrics
  #Data frame with predictor sets and order of model fitting
  complexityNames <- c("low","low_medium","medium","medium_high","high")
  columnNames <- c("trainAUC","trainKappa","trainTSS","trainPRAUC","trainLogLoss","trainDxy","trainROC",
                   "trainR2","trainD","trainDChisq","trainDp","trainU","trainUChisq","trainUp","trainQ",
                   "trainBrier","trainIntercept","trainSlope","trainEmax","trainE90","trainEavg","trainSz",
                   "trainSp","number_train","train_prevalence","testAUC","testKappa","testTSS","testPRAUC","testLogLoss",
                   "testDxy","testROC","testR2","testD","testDChisq","testDp","testU","testUChisq","testUp",
                   "testQ","testBrier","testIntercept","testSlope","testEmax","testE90","testEavg","testSz",
                   "testSp","number_test","test_prevalence","mIobs","mIexp","mIsd","mIp","resDF","nullDF","nullD","resD","expD",
                   "EVs","DVs","L","M","D","T","HF","HR","BX","stationary","variable","natural","anthropogenic",
                   colnames(training))
  modelEvaluation <- as.data.frame(matrix(0,nrow(combinationsPredictors),length(columnNames)))
  colnames(modelEvaluation) <- columnNames
  #Data frame with model object
  #Use ED sheets to characterize the major types inside ecological space
  modelObjectInfo <- as.data.frame(matrix(0,nrow(combinationsPredictors),6))
  colnames(modelObjectInfo) <- c("majortype","majortype_group","LCE1","LCE2","LCE3","dLCE")
  modelObjectInfo[,1] <- modelObjects[majorType]
  for (i in 1:nrow(modelObjectInfo)) {
    modelObjectInfo[i,2] <- ifelse(modelObjectInfo[i,1] < 46, "T", "V")
  }
  #Making array for evaluation metrics
  evaluationArray <- array(0, dim = c(nrow(modelEvaluation), ncol(cbind(modelObjectInfo, modelEvaluation, combinationsPredictors)), length(complexityNames)),
                           dimnames = list(1:nrow(modelEvaluation), colnames(cbind(modelObjectInfo, modelEvaluation, combinationsPredictors)), 
                                           complexityNames))
  #Input: 
  #modelComplexity = numeric vector with minimum residual deviance explained for a predictor to be selected
  #DVselectData = dvdata (list) returned from DVselect with transformed values for all observations across all predictors 
  #transformedDVtransformations = transformations (list) returned from deriveVars with transformations applied to the predictors
  #trainingData = data frame with the binary response variable (RV) in the 1st column, coordinates (x and y) in the 2nd and 3rd column and the predictors in following columns
  #testData = data frame with the binary response variable (RV) in the 1st column, coordinates (x and y) in the 2nd and 3rd column and the predictors in following columns
  #predictorSet = character vector with names corresponding to the predictor names in the training and test data of the predictor set to be tested
  #previousPredictorSets = character vector with names corresponding to the predictor names in the training and test data of the predictor sets already tested
  #predictorVar = raster stack with predictors with names corresponding to the predictor names in the training and test data
  #previousModel = selectedmodel (model object) returned by selectEV, corresponding with the previous model
  for (m in 1:length(complexityValues)) {
    selectedDVs <- DVcomplexity(EVfirstSelection$null.deviance*complexityValues[m],
                                DVfirstSelection$selection,
                                DVfirstSelection$dvdata,
                                transformationDVs$dvdata,
                                which(names(transformationDVs$dvdata) %in% categorical)-1,
                                training)
    
    #Selecting predictors
    EVfirstSelection <- selectEV(selectedDVs[c(1,length(names(selectedDVs)))],
                                 RV ~ random, 
                                 algorithm = "LR")$selectedmodel
    allResults <- list()
    #Generating models and calculating evaluation metrics
    #Looping over all predictor sets
    for (k in 1:nrow(combinationsPredictors)) {
      
      #Finding the previous model
      previousModelIdentity <- numeric()
      for (i in 1:nrow(combinationsPredictors)) {
        previousModelIdentity[i] <- identical(as.numeric(combinationsPredictors[i,5:8]),as.numeric(combinationsPredictors[k,1:4]))
      }
      prevPred <- as.character(combinationsPredictors[k,1:4][which(combinationsPredictors[which(previousModelIdentity == 1),5:8] != 0)])
      previousPredSets <- numeric()
      if(length(which(names(predictorSets) %in% prevPred) > 0)) {
        for (i in 1:length(which(names(predictorSets) %in% prevPred))) {
          previousPredSets <- c(previousPredSets, predictorSets[[which(names(predictorSets) %in% prevPred)[i]]])
        }
      }
      
      predSet <- numeric()
      predSet <- predictorSets[[which(names(predictorSets) %in% as.character(combinationsPredictors[k,9:12][as.numeric(combinationsPredictors[k,9:12]) != 0]))]]
      
      previous <- list()
      if(length(which(previousModelIdentity == 1)) > 0)
      {
        previous <- allResults[[which(previousModelIdentity == 1)]][[1]]
      }
      if(k < 6)
      {
        #Running the function (put all the arguments in the function)
        model <- modelFitting(EVfirstSelection$null.deviance*complexityValues[m],
                              selectedDVs,
                              transformationDVs$transformations,
                              training,
                              testing,
                              predSet,
                              previousPredSets,
                              previous,
                              combinationsPredictors[k,1:4],
                              predictorSets,
                              predictors)
        
        #Recording the results in data frame
        allResults[[k]] <- model
        modelEvaluation[k,] <- model[[2]]
        #Progress bar
        print(k/nrow(combinationsPredictors))
      }
      else
      {
        #Running the function (put all the arguments in the function)
        model <- modelFittingAdd(selectedDVs,
                                 transformationDVs$transformations,
                                 training,
                                 testing,
                                 previousPredSets,
                                 predictorSets,
                                 predictors,
                                 allResults,
                                 combinationsPredictors[k,5:8])
        
        allResults[[k]] <- model
        modelEvaluation[k,] <- model[[2]]
        print(k/nrow(combinationsPredictors))
        print(m/length(complexityNames))
        print(p/length(modelObjects))
      }
    }
    evaluationArray[,,m] <- as.matrix(cbind(modelObjectInfo, modelEvaluation, combinationsPredictors))
    saveRDS(list(allResults,transformationDVs,selectedDVs,DVfirstSelection$selection,training,testing), paste("new_results/modelling_results_",modelObjects[majorType],"_",complexityNames[m],".rds",sep=""))
    write.csv(as.data.frame(evaluationArray[,,m]), paste("new_evaluation/model_evaluation_",modelObjects[majorType],"_",complexityNames[m],".csv",sep=""))
    # 6 Extrapolation  ----
    #The models are reparameterised (i.e., same predictors but different parameter values) and tested in the two regions
    for (q in 1:length(region)) {
      DVmodels <- list()
      for (i in 1:5) {
        DVmodels[[i]] <- names(allResults[[i]][[1]]$coefficients)[-c(1)]
      }
      DVmodels <- unlist(DVmodels)
      DVmodels <- c(DVmodels,"random_L")
      EVlastCol <- which(colnames(trainingAll[[q+1]]) %in% gsub("\\_.*", "",DVmodels))
      lastPredictors <- c("RV",colnames(trainingAll[[q+1]])[EVlastCol])
      #Transforming predictors
      transformationDVsCross <- deriveVars(trainingAll[[q+1]][,c(1,EVlastCol)], algorithm = "LR", transformtype = c("L","M","D","B"))
      #Selecting transformations for all predictors 
      DVfirstSelectionCross <- selectDVforEV(transformationDVsCross$dvdata, alpha = 0.001, test = "F", algorithm = "LR")
      allResultsCV <- list()
      selectedDVsCross <- DVcomplexityCV(DVmodels, 
                                         DVfirstSelectionCross$dvdata, 
                                         transformationDVsCross$dvdata, 
                                         categorical, 
                                         trainingAll[[q+1]], 
                                         lastPredictors)
      
      #Data frame with predictor sets and order of model fitting
      columnNamesCV <- c("trainAUC","trainKappa","trainTSS","trainPRAUC","trainLogLoss","trainDxy","trainROC",
                         "trainR2","trainD","trainDChisq","trainDp","trainU","trainUChisq","trainUp","trainQ",
                         "trainBrier","trainIntercept","trainSlope","trainEmax","trainE90","trainEavg","trainSz",
                         "trainSp","number_train","train_prevalence","testAUC","testKappa","testTSS","testPRAUC","testLogLoss",
                         "testDxy","testROC","testR2","testD","testDChisq","testDp","testU","testUChisq","testUp",
                         "testQ","testBrier","testIntercept","testSlope","testEmax","testE90","testEavg","testSz",
                         "testSp","number_test","test_prevalence",paste("ext","_",c("trainAUC","trainKappa","trainTSS","trainPRAUC","trainLogLoss","trainDxy","trainROC",
                                                                                    "trainR2","trainD","trainDChisq","trainDp","trainU","trainUChisq","trainUp","trainQ",
                                                                                    "trainBrier","trainIntercept","trainSlope","trainEmax","trainE90","trainEavg","trainSz",
                                                                                    "trainSp","number_train","train_prevalence","testAUC","testKappa","testTSS","testPRAUC","testLogLoss",
                                                                                    "testDxy","testROC","testR2","testD","testDChisq","testDp","testU","testUChisq","testUp",
                                                                                    "testQ","testBrier","testIntercept","testSlope","testEmax","testE90","testEavg","testSz",
                                                                                    "testSp","number_test","test_prevalence"),sep=""),"mIobs","mIexp","mIsd","mIp","resDF","nullDF","nullD","resD","expD",
                         "EVs","DVs","L","M","D","T","HF","HR","BX","stationary","variable","natural","anthropogenic",
                         colnames(trainingAll[[q+1]]))
      modelEvaluationCV <- as.data.frame(matrix(0,nrow(combinationsPredictors),length(columnNamesCV)))
      colnames(modelEvaluationCV) <- columnNamesCV
      #Making array
      evaluationArrayCV <- array(0, dim = c(nrow(modelEvaluationCV), ncol(modelEvaluationCV), length(complexityNames)),
                                 dimnames = list(1:nrow(modelEvaluationCV), colnames(modelEvaluationCV), 
                                                 complexityNames))
      ifelse(q == 1, g <- 2, g <- 1)
      for (k in 1:length(allResults)) {
          model <- modelFittingAddCV(allResults[[k]][[1]],
                                     selectedDVsCross,
                                     trainingAll[[q+1]],
                                     testingAll[[q+1]],
                                     trainingAll[[g+1]],
                                     testingAll[[g+1]],
                                     transformationDVsCross$transformations,
                                     predictors,
                                     predictorSets)
        }
        
        allResultsCV[[k]] <- model
        modelEvaluationCV[k,] <- model[[2]]
        print(k/nrow(combinationsPredictors))
        print(q/length(region))
        print(m/length(complexityNames))
        print(p/length(modelObjects))
      }
      evaluationArrayCV[,,m] <- as.matrix(modelEvaluationCV)
      saveRDS(modelEvaluationCV, paste("new_results/modelling_results_",modelObjects[majorType],"_",complexityNames[m],"_",region[q],".rds",sep=""))
      write.csv(as.data.frame(evaluationArrayCV[,,m]), paste("new_evaluation/model_evaluation_",modelObjects[majorType],"_",complexityNames[m],"_",region[q],".csv",sep=""))
    }
  }
  finalList <- list()
  listData <- list(list(),list())
  for (i in 1:length(complexityNames)) {
    finalList[[i]] <- read.csv(paste("new_evaluation/model_evaluation_",modelObjects[majorType],"_",complexityNames[i],".csv",sep=""))[-c(1)]
    for (j in 1:length(region)) {
      listData[[j]][[i]] <- read.csv(paste("new_evaluation/model_evaluation_",modelObjects[majorType],"_",complexityNames[i],"_",region[j],".csv",sep=""))[-c(1)]
    }
  }
  dataList <- list()
  #Bind data frames
  for (i in 1:length(listData)) {
    dataList[[i]] <- as.data.frame(do.call(rbind, listData[[i]]))
    colnames(dataList[[i]]) <- paste(region[i],"_",colnames(dataList[[i]]), sep="")
  }
  data <- as.data.frame(do.call(rbind, finalList))
  evaluationDataFrame <- as.data.frame(do.call(cbind, list(data,dataList)))
  #write.csv(as.data.frame(evaluationDataFrame), paste("new_evaluation/model_evaluation_",modelObjects[majorType],".csv",sep=""))
}
