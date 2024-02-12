library(raster)

# 1 Importing data----
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 2/R/")
folderNames <- c("terrain","surface","natural","anthropogenic")
predictorStacks <- list()
predictorSets <- list()
for (i in 1:length(folderNames)) {
  predictorSets[[i]] <- list.files(paste("predictor_sets/", folderNames[i], sep = ""),
                                   pattern = "tif$", 
                                   full.names = TRUE)
  predictorStacks[[i]] <- stack(predictorSets[[i]])
  predictorSets[[i]] <- gsub("predictor_sets/", "", predictorSets[[i]])
  predictorSets[[i]] <- gsub(folderNames[i], "", predictorSets[[i]])
  predictorSets[[i]] <- gsub("/", "", predictorSets[[i]])
  predictorSets[[i]] <- gsub(".tif", "", predictorSets[[i]])
  predictorSets[[i]] <- gsub("_", ".", predictorSets[[i]])
  names(predictorStacks[[i]]) <- predictorSets[[i]]
}
names(predictorStacks) <- folderNames
names(predictorSets) <- folderNames

#Make one raster stack for all predictors
predictors <- stack(predictorStacks[[1]],
                    predictorStacks[[2]],
                    predictorStacks[[3]],
                    predictorStacks[[4]])

# 2 Correlation analysis----
#Function that takes a stack of rasters as input, extracts 1000 random grid cells, and computes
#Kendall's tau correlation between every pair of rasters for those grid cells.
#The results are presented in a correrlation matrix
correlationMatrix <- function(predictors)
{
#Choosing 1000 random grid cell numbers without replacement
#Making sure the sample is taken from cells within the study area
randomValues <- sample(which(!is.na(predictors[[1]][])), 1000, replace = FALSE)

#Extracting data for all predictors for the chosen grid cells
rasterLayers <- list()
for (i in 1:length(names(predictors))) {
  rasterLayers[[i]] <- predictors[[i]][randomValues]
}

#Convert the list into a data frame
listData <- rasterLayers[[1]]
for (i in 2:length(rasterLayers)) {
  listData <- cbind(listData, rasterLayers[[i]])
}
listData <- as.data.frame(listData)
colnames(listData) <- names(predictors)

#Compute the correlation between each pair of predictors and put it into a data frame
corrObject <- numeric()
corrMatrix <- matrix(0, ncol(listData), ncol(listData))
for (i in 1:ncol(listData)) {
  for (j in 1:ncol(listData)) {
    corrObject[j] <- cor(listData[,j], 
                         listData[,i], 
                         use = "complete.obs", 
                         method = "kendall")
  }
  corrMatrix[,i] <- corrObject
}
corrMatrix <- as.data.frame(corrMatrix)
colnames(corrMatrix) <- names(predictors)
rownames(corrMatrix) <- names(predictors)
return(corrMatrix)
}

#Compute the correlation matrix separately for every predictor group
correlatedVariables <- correlationMatrix(predictorStacks[[2]])

#Excluding all but one predictor variable in each group of inter-correlated variables
correlationTable <- sort(table(which(correlatedVariables > 0.90, arr.ind = TRUE)))
names(correlationTable) <- names(correlatedVariables)[as.numeric(names(correlationTable))]
corrTable <- which(correlatedVariables > 0.90, arr.ind = TRUE)
removing <- numeric()
for (i in 1:nrow(corrTable)) {
  if(corrTable[i,1] == corrTable[i,2])
  {
    removing[i] <- 1
  }
  else{
    removing[i] <- 0
}
}
correlationTable
corrTable[which(removing == 0),]
which(correlatedVariables < -0.90, arr.ind = TRUE)
