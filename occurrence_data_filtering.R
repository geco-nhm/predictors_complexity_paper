# 1 Importing libraries ----
library(sp)
library(raster)
library(rgeos)
library(rgdal)
library(distances)

# 2 Importing data ----
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 2/R/")
#Importing mask and predictor raster layers
maskLayer <- raster("predictor_sets/mask.tif")
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

#Making one raster stack for all predictors
predictors <- stack(predictorStacks[[1]],
                    predictorStacks[[2]],
                    predictorStacks[[3]],
                    predictorStacks[[4]],
                    maskLayer)

#Making object with categorical predictor names
categoricalNames <- c("berggrunn","losmasse","CLG.soil.type","CorineLandCover2012",
                 "CorineLandCover2018","arjordbr","arskogbon","artreslag","artype","arveget","LA","LAHT",
                 "CLG.glacier.presence","CLG.vegetation.cover","reindeer.presence","reindeer.grazing",
                 "reindeer.nonwinter.grazing","reindeer.winter.grazing","deer","moose","roe.deer","avalanches",
                 "floods","ruminant.grazing","landslide.floodslide","rock.slides",
                 "CLG.terrain.variation","CLG.lake.abundance","CLG.inner.outer.coast","NeGibbsA",
                 "NeGibbsB","NeLaShuf1","NeLaShuf2","NeLaShuf3","conservation.area","treslag","bonitet","Snow.Dur.Mon")

#Importing data in which grid cells will be extracted from
#The file includes the observations from the centroids of all grid cells within all sampled polygons in the study area
#THe columns in the file are polygon ID, ecosystem type, data set (training or evaluation data), size of polygon, x-coordinate, y-cooridnate 
gridCells <- read.csv("res_var/processed/larger_polygons.csv")[-c(1)]

#Calculating how many the number of grid cells that should be extracted per polygon (5 cells per km2)
cellDensity <- 0.000005

#Adding column with number of grid cells to be extracted per polygon
gridCells <- cbind(gridCells, 
                   cellFrequency = ceiling(gridCells$area*cellDensity))

# 3 Systematic filtering ----
#Extracting predictor data for all continuous predictors
categoricalPredictors <- which(names(predictors) %in% categoricalNames)
#Find the cell numbers in the predictor layers corresponding with the coordinates in the data frame
cellNumber <- cellFromXY(predictors, gridCells[,c(5:6)])
#Adding predictor data for all continuous predictors
predictorValues <- extract(x = predictors[[-c(categoricalPredictors)]], 
                           y = cellNumber)
predictorValues <- as.data.frame(predictorValues)

#Making mask (some of the polygons are situated outside the study area)
mask <- which(!is.na(predictorValues[,ncol(predictorValues)]))

#Adding the extracted predictor data to the data set
gridCellsDF <- cbind(gridCells[mask,], predictorValues[mask,-c(ncol(predictorValues))])

#Sorting the data by polygon ID and dividing the different polygons into lists
orderedGridCellsDF <- gridCellsDF[order(gridCellsDF$polygonID),]
polygonCellList <- split(orderedgridCellsDF, orderedgridCellsDF$polygonID, drop = TRUE)

#Function that extracts grid cells that are representative from each polygon
representativePolygons <- function(polygonList)
{
  #Removing unimportant columns (only the coordinates and predictor values are needed)
  polygons <- polygonList[,-c(1,2,3,4,7)]
  
  #Normalising the predictor data by subtracting the polygon mean and dividing by the polygon standard deviation 
  means <- apply(polygons, 2, mean, na.rm = TRUE)
  sds <- apply(polygons, 2, sd, na.rm = TRUE)
  nor <- as.data.frame(scale(polygons, 
                             center = means, 
                             scale = sds))
  
  #If normalisation gives undefined values (due to zero standard deviation)
  #Remove the columns with those predictors, the cells are not dissimilar with respect to those predictors
  nor <- nor[,-unique(as.numeric(which(is.na(nor), arr.ind = T)[,2]))]
  if(length(unique(which(apply(nor, 2, is.nan), arr.ind = T)[,2])) > 0)
  {
    notAnumber <- unique(which(apply(nor, 2, is.nan), arr.ind = T)[,2])
    
    #Computing (environmental) distance between cells from the same polygons with respect to the predictors
    distance <- distances(nor[,-c(notAnumber)])
  }
  else
  {
    #Computing (environmental) distance between cells from the same polygons with respect to the predictors
    #Making a dissimilarity matrix between each cell (the dissimilarity is computed based on the predictor values)
    distance <- distances(nor)
  }
  #Calculating the summed difference between each cell and the other cells
  distanceSum <- apply(distance, 2, sum, na.rm = T)
  
  #Choosing the cells with the shortest distance to all the other cells in the polygon 
  #in "environmental space". The number of cells that are chosen are based on the size of the polygon.
  #The density of grid cells extracted per polygon is 5 per km2
  chooseCells <- sort(distanceSum, index.return = T)$ix[1:polygonList$cellFrequency[1]]
  return(polygonList[chooseCells,1:6])
}

#Extracting grid cells for all the polygons
numberExtractCells <- length(polygonCellList)
newCellsList <- vector("list", length = numberExtractCells)
for (i in 1:numberExtractCells) {
  newCellsList[[i]] <- representativePolygons(polygonCellList[[i]])
}

# Combining the list of extracted cells into a single data frame
newCells <- do.call(rbind, newCellsList)

#Adding data for polygons with only one and two grid cells inside them to the data frame
smallPolygons <- read.csv("res_var/processed/smaller_polygons.csv")[-c(1)]
additionalSmallPolygons <- read.csv("res_var/processed/additional_smaller_polygons.csv")[-c(1)]
data <- rbind(smallPolygons,additionalSmallPolygons,newCells)
cellNumber <- cellFromXY(predictors, data[,c(5:6)])

#Extracting predictor data for all grid cells
predictorValues <- extract(x = predictors$mask, y = cellNumber)

#Applying mask
mask <- which(!is.na(predictorValues))
data <- data[mask,]

#Saving the data frame
#write.csv(data, "res_var/processed/pointsTrainingRepresentative.csv")


# 4 Random filtering ----
#Sorting the data by polygon ID and dividing the different polygons into lists
orderedGridCellsDF <- gridCells[order(gridCells$polygonID),]
polygonCellList <- split(orderedGridCellsDF, orderedGridCellsDF$polygonID, drop = T)

#Function that extracts grid cells randomly (without replacement) from each polygon
randomPolygons <- function(polygonList)
{
  return(polygonList[sample(1:nrow(polygonList),polygonList$cellFrequency[1], replace = F),1:6])
}

#Extracting grid cells for all the polygons
numberExtractCells <- length(polygonCellList)
newCellsList <- vector("list", length = numberExtractCells)
for (i in 1:numberExtractCells) {
  newCellsList[[i]] <- randomPolygons(polygonCellList[[i]])
}

# Combining the list of extracted cells into a single data frame
newCells <- do.call(rbind, newCellsList)

#Adding data for polygons with only one and two grid cells inside them to the data frame
smallPolygons <- read.csv("res_var/processed/smaller_polygons.csv")[-c(1)]
additionalSmallPolygons <- read.csv("res_var/processed/additional_smaller_polygons.csv")[-c(1)]
data <- rbind(smallPolygons, additionalSmallPolygons, newCells)
cellNumber <- cellFromXY(predictors, data[,c(5:6)])

#Extracting predictor data for all grid cells
predictorValues <- extract(x = predictors$mask, y = cellNumber)

#Applying mask
mask <- which(!is.na(predictorValues))
data <- data[mask,]

#Saving the data frame
#write.csv(data, "res_var/processed/pointsTrainingRandom.csv")