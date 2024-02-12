# 1 Import libraries ----
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(colorspace)
library(vegan)
library(e1071)
library(extrafont)
library(gridExtra)
library(VennDiagram)
library(cowplot)
library(writexl)

#Modify function for plotting the venn diagram (from VennDiagram)
draw.quad.venn <- function(area1, area2, area3, area4, n12, n13, n14, n23, n24,n34, n123, n124, n134, n234, n1234, category = rep("", 4), 
                           lwd = rep(2, 4), lty = rep("solid", 4), col = rep("black", 4), fill = NULL, alpha = rep(0.5, 4), 
                           label.col = rep("black", 15), cex = rep(1, 15), fontface = rep("plain", 15), fontfamily = rep("Tahoma", 15), 
                           cat.pos = c(-15, 15, 0, 0), cat.dist = c(0.22, 0.22, 0.11, 0.11), cat.col = rep("black", 4), cat.cex = rep(1, 4), 
                           cat.fontface = rep("plain", 4), cat.fontfamily = rep("Tahoma", 4), cat.just = rep(list(c(0.5, 0.5)), 4), 
                           rotation.degree = 0, rotation.centre = c(0.5, 0.5), ind = TRUE, cex.prop = NULL, print.mode = "raw", 
                           sigdigs = 3, direct.area = FALSE, area.vector = 0, ...) 
{
  if (length(category) == 1) {
    cat <- rep(category, 4)
  }
  else if (length(category) != 4) {
    flog.error("Unexpected parameter length for \"category\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"category\"")
  }
  if (length(lwd) == 1) {
    lwd <- rep(lwd, 4)
  }
  else if (length(lwd) != 4) {
    flog.error("Unexpected parameter length for \"lwd\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"lwd\"")
  }
  if (length(lty) == 1) {
    lty <- rep(lty, 4)
  }
  else if (length(lty) != 4) {
    flog.error("Unexpected parameter length for \"lty\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"lty\"")
  }
  if (length(col) == 1) {
    col <- rep(col, 4)
  }
  else if (length(col) != 4) {
    flog.error("Unexpected parameter length for \"col\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"col\"")
  }
  if (length(label.col) == 1) {
    label.col <- rep(label.col, 15)
  }
  else if (length(label.col) != 15) {
    flog.error("Unexpected parameter length for \"label.col\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"label.col\"")
  }
  if (length(cex) == 1) {
    cex <- rep(cex, 15)
  }
  else if (length(cex) != 15) {
    flog.error("Unexpected parameter length for \"cex\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"cex\"")
  }
  if (length(fontface) == 1) {
    fontface <- rep(fontface, 15)
  }
  else if (length(fontface) != 15) {
    flog.error("Unexpected parameter length for \"fontface\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"fontface\"")
  }
  if (length(fontfamily) == 1) {
    fontfamily <- rep(fontfamily, 15)
  }
  else if (length(fontfamily) != 15) {
    flog.error("Unexpected parameter length for \"fontfamily\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"fontfamily\"")
  }
  if (length(fill) == 1) {
    fill <- rep(fill, 4)
  }
  else if (length(fill) != 4 & length(fill) != 0) {
    flog.error("Unexpected parameter length for \"fill\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"fill\"")
  }
  if (length(alpha) == 1) {
    alpha <- rep(alpha, 4)
  }
  else if (length(alpha) != 4 & length(alpha) != 0) {
    flog.error("Unexpected parameter length for \"alpha\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"alpha\"")
  }
  if (length(cat.pos) == 1) {
    cat.pos <- rep(cat.pos, 4)
  }
  else if (length(cat.pos) != 4) {
    flog.error("Unexpected parameter length for \"cat.pos\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"cat.pos\"")
  }
  if (length(cat.dist) == 1) {
    cat.dist <- rep(cat.dist, 4)
  }
  else if (length(cat.dist) != 4) {
    flog.error("Unexpected parameter length for \"cat.dist\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"cat.dist\"")
  }
  if (length(cat.col) == 1) {
    cat.col <- rep(cat.col, 4)
  }
  else if (length(cat.col) != 4) {
    flog.error("Unexpected parameter length for \"cat.col\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"cat.col\"")
  }
  if (length(cat.cex) == 1) {
    cat.cex <- rep(cat.cex, 4)
  }
  else if (length(cat.cex) != 4) {
    flog.error("Unexpected parameter length for \"cat.cex\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"cat.cex\"")
  }
  if (length(cat.fontface) == 1) {
    cat.fontface <- rep(cat.fontface, 4)
  }
  else if (length(cat.fontface) != 4) {
    flog.error("Unexpected parameter length for \"cat.fontface\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"cat.fontface\"")
  }
  if (length(cat.fontfamily) == 1) {
    cat.fontfamily <- rep(cat.fontfamily, 4)
  }
  else if (length(cat.fontfamily) != 4) {
    flog.error("Unexpected parameter length for \"cat.fontfamily\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter length for \"cat.fontfamily\"")
  }
  if (!(is.list(cat.just) && length(cat.just) == 4 && length(cat.just[[1]]) == 
        2 && length(cat.just[[2]]) == 2 && length(cat.just[[3]]) == 
        2 && length(cat.just[[4]]) == 2)) {
    flog.error("Unexpected parameter format for \"cat.just\"", 
               name = "VennDiagramLogger")
    stop("Unexpected parameter format for \"cat.just\"")
  }
  cat.pos <- cat.pos + rotation.degree
  if (direct.area) {
    areas <- area.vector
    for (i in 1:15) {
      assign(paste("a", i, sep = ""), area.vector[i])
    }
  }
  else {
    a6 <- n1234
    a12 <- n123 - a6
    a11 <- n124 - a6
    a5 <- n134 - a6
    a7 <- n234 - a6
    a15 <- n12 - a6 - a11 - a12
    a4 <- n13 - a6 - a5 - a12
    a10 <- n14 - a6 - a5 - a11
    a13 <- n23 - a6 - a7 - a12
    a8 <- n24 - a6 - a7 - a11
    a2 <- n34 - a6 - a5 - a7
    a9 <- area1 - a4 - a5 - a6 - a10 - a11 - a12 - a15
    a14 <- area2 - a6 - a7 - a8 - a11 - a12 - a13 - a15
    a1 <- area3 - a2 - a4 - a5 - a6 - a7 - a12 - a13
    a3 <- area4 - a2 - a5 - a6 - a7 - a8 - a10 - a11
    areas <- c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, 
               a11, a12, a13, a14, a15)
  }
  areas.error <- c("a1  <- area3 - a2 - a4 - a5 - a6 - a7 - a12 - a13", 
                   "a2  <- n34 - a6 - a5 - a7", "a3  <- area4 - a2 - a5 - a6 - a7 - a8 - a10 - a11", 
                   "a4  <- n13 - a6 - a5 - a12", "a5  <- n134 - a6", "a6  <- n1234", 
                   "a7  <- n234 - a6", "a8  <- n24 - a6 - a7 - a11", "a9  <- area1 - a4 - a5 - a6 - a10 - a11 - a12 - a15", 
                   "a10 <- n14 - a6 - a5 - a11", "a11 <- n124 - a6", "a12 <- n123 - a6", 
                   "a15 <- n12 - a6 - a11 - a12", "a13 <- n23 - a6 - a7 - a12", 
                   "a14 <- area2 - a6 - a7 - a8 - a11 - a12 - a13 - a15")
  for (i in 1:length(areas)) {
    if (areas[i] < 0) {
      areas[i] <- 0
    }
  }
  if (length(cex.prop) > 0) {
    if (length(cex.prop) != 1) {
      flog.error("Value passed to cex.prop is not length 1", 
                 name = "VennDiagramLogger")
      stop("Value passed to cex.prop is not length 1")
    }
    func = cex.prop
    if (!is(cex.prop, "function")) {
      if (cex.prop == "lin") {
        func = function(x) x
      }
      else if (cex.prop == "log10") {
        func = log10
      }
      else flog.error(paste0("Unknown value passed to cex.prop: ", 
                             cex.prop), name = "VennDiagramLogger")
      stop(paste0("Unknown value passed to cex.prop: ", 
                  cex.prop))
    }
    maxArea = max(areas)
    for (i in 1:length(areas)) {
      cex[i] = cex[i] * func(areas[i])/func(maxArea)
      if (cex[i] <= 0) 
        stop(paste0("Error in rescaling of area labels: the label of area ", 
                    i, " is less than or equal to zero"))
    }
  }
  grob.list <- gList()
  ellipse.positions <- matrix(nrow = 4, ncol = 7)
  colnames(ellipse.positions) <- c("x", "y", "a", "b", "rotation", 
                                   "fill.mapping", "line.mapping")
  ellipse.positions[1, ] <- c(0.65, 0.47, 0.35, 0.2, 45, 2, 
                              2)
  ellipse.positions[2, ] <- c(0.35, 0.47, 0.35, 0.2, 135, 
                              1, 1)
  ellipse.positions[3, ] <- c(0.5, 0.57, 0.33, 0.15, 45, 4, 
                              4)
  ellipse.positions[4, ] <- c(0.5, 0.57, 0.35, 0.15, 135, 
                              3, 3)
  for (i in 1:4) {
    grob.list <- gList(grob.list, VennDiagram::ellipse(x = ellipse.positions[i, 
                                                                             "x"], y = ellipse.positions[i, "y"], a = ellipse.positions[i, 
                                                                                                                                        "a"], b = ellipse.positions[i, "b"], rotation = ellipse.positions[i, 
                                                                                                                                                                                                          "rotation"], gp = gpar(lty = 0, fill = fill[ellipse.positions[i, 
                                                                                                                                                                                                                                                                        "fill.mapping"]], alpha = alpha[ellipse.positions[i, 
                                                                                                                                                                                                                                                                                                                          "fill.mapping"]])))
  }
  for (i in 1:4) {
    grob.list <- gList(grob.list, ellipse(x = ellipse.positions[i, 
                                                                "x"], y = ellipse.positions[i, "y"], a = ellipse.positions[i, 
                                                                                                                           "a"], b = ellipse.positions[i, "b"], rotation = ellipse.positions[i, 
                                                                                                                                                                                             "rotation"], gp = gpar(lwd = lwd[ellipse.positions[i, 
                                                                                                                                                                                                                                                "line.mapping"]], lty = lty[ellipse.positions[i, 
                                                                                                                                                                                                                                                                                              "line.mapping"]], col = col[ellipse.positions[i, 
                                                                                                                                                                                                                                                                                                                                            "line.mapping"]], fill = "transparent")))
  }
  label.matrix <- matrix(nrow = 15, ncol = 3)
  colnames(label.matrix) <- c("label", "x", "y")
  label.matrix[1, ] <- c(a1, 0.35, 0.77)
  label.matrix[2, ] <- c(a2, 0.5, 0.69)
  label.matrix[3, ] <- c(a3, 0.65, 0.77)
  label.matrix[4, ] <- c(a4, 0.31, 0.67)
  label.matrix[5, ] <- c(a5, 0.4, 0.58)
  label.matrix[6, ] <- c(a6, 0.5, 0.47)
  label.matrix[7, ] <- c(a7, 0.6, 0.58)
  label.matrix[8, ] <- c(a8, 0.69, 0.67)
  label.matrix[9, ] <- c(a9, 0.18, 0.58)
  label.matrix[10, ] <- c(a10, 0.32, 0.42)
  label.matrix[11, ] <- c(a11, 0.425, 0.38)
  label.matrix[12, ] <- c(a12, 0.575, 0.38)
  label.matrix[13, ] <- c(a13, 0.68, 0.42)
  label.matrix[14, ] <- c(a14, 0.82, 0.58)
  label.matrix[15, ] <- c(a15, 0.5, 0.28)
  processedLabels <- rep("", length(label.matrix[, "label"]))
  if (print.mode[1] == "percent") {
    processedLabels <- paste(signif(label.matrix[, "label"]/sum(label.matrix[, 
                                                                             "label"]) * 100, digits = sigdigs), "%", sep = "")
    if (isTRUE(print.mode[2] == "raw")) {
      processedLabels <- paste(processedLabels, "\n(", 
                               label.matrix[, "label"], ")", sep = "")
    }
  }
  if (print.mode[1] == "raw") {
    processedLabels <- label.matrix[, "label"]
    if (isTRUE(print.mode[2] == "percent")) {
      processedLabels <- paste(processedLabels, "\n(", 
                               paste(signif(label.matrix[, "label"]/sum(label.matrix[, 
                                                                                     "label"]) * 100, digits = sigdigs), "%)", 
                                     sep = ""), sep = "")
    }
  }
  for (i in 1:nrow(label.matrix)) {
    grob.list <- gList(grob.list, textGrob(label = processedLabels[i], 
                                           x = label.matrix[i, "x"], y = label.matrix[i, "y"], 
                                           gp = gpar(col = label.col[i], cex = cex[i], fontface = fontface[i], 
                                                     fontfamily = fontfamily[i])))
  }
  cat.pos.x <- c(0.18, 0.82, 0.35, 0.65)
  cat.pos.y <- c(0.58, 0.58, 0.77, 0.77)
  for (i in 1:4) {
    this.cat.pos <- find.cat.pos(x = cat.pos.x[i], y = cat.pos.y[i], 
                                 pos = cat.pos[i], dist = cat.dist[i])
    grob.list <- gList(grob.list, textGrob(label = category[i], 
                                           x = this.cat.pos$x, y = this.cat.pos$y, just = cat.just[[i]], 
                                           gp = gpar(col = cat.col[i], cex = cat.cex[i], fontface = cat.fontface[i], 
                                                     fontfamily = cat.fontfamily[i])))
  }
  grob.list <- VennDiagram::adjust.venn(VennDiagram::rotate.venn.degrees(grob.list, 
                                                                         rotation.degree, rotation.centre[1], rotation.centre[2]), 
                                        ...)
  if (ind) {
    grid.draw(grob.list)
  }
  return(grob.list)
}

# 2 Import data ----
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 2/R")
dataList <- list()
for (i in 1:length(list.files("new_evaluation/",pattern=".csv"))) {
  dataList[[i]] <- read.csv(paste("new_evaluation/",list.files("new_evaluation/",pattern=".csv")[i],sep = ""))[,-c(1)]
}
data <- as.data.frame(do.call(rbind, dataList))

# 3 Adding new columns  ----
#Number of models for each ecosystem type
n <- length(which(data$majortype == 14))
#Making a column for complexity levels
data$complexity <- rep(c(rep(1,n/5),rep(2,n/5),rep(3,n/5),rep(4,n/5),rep(5,n/5)),length(unique(data$majortype)))
data$complexity <- c("Low","Low-medium","Medium","Medium-High","High")[data$complexity]
data$complexity <- factor(data$complexity, levels = c("Low","Low-medium","Medium","Medium-High","High"))
#Making a column for majortype group
data$majortype_group[which(data$majortype > 45)] <- "V"
data$majortype_group[which(data$majortype < 46)] <- "T"
data$majortype_names <- ifelse(data$majortype < 46, paste("T", data$majortype, sep = ""), paste("V", data$majortype-45, sep = ""))
#Making a column for ecosystem types
data$majortype_names <- factor(data$majortype_names, levels = c("T1","T3","T4","T7","T14","T19","T22","T27","T30","T31","T32","T34","V1","V2","V3"))
#Making a column for number of predictor sets that were included
data$sets_included <- rowSums(data[,75:78])
#Making column for predictor sets
data$set_combination <- rep(c("T","S","N","A","P","T-S","N-T-S","N-A-T-S","A-T-S","N-T","N-A-T","A-T","N-S","A-S","N-A-S","N-A"),5*15)
data$set_combination <- factor(data$set_combination, levels = c("N","A","T","S","N-A","N-T",
                                                                "N-S","A-T","A-S","T-S","N-A-T",
                                                                "N-A-S","N-T-S","A-T-S","N-A-T-S","P"))

#Excluding models with neutral predictors (not used in the article)
data$Neutral <- numeric(nrow(data))
data$Neutral[which(data$added_set1 == 5)] <- 1
data$EVs[which(rowSums(data[,c(75:78,334:345)]) == 0)] <- 0
data$DVs[which(rowSums(data[,c(75:78,334:345)]) == 0)] <- 0
colors_predictor_sets <- setNames(c("darkgreen","deepskyblue4","khaki","firebrick"), c("Natural","Anthropogenic","Terrain","Surface"))

# 4 Variation partitioning ----
#Identifying the complexity level with the highest average AUC for each ecosystem type
bestModels <- data %>% group_by(majortype_names, complexity)  %>% 
  reframe(mean_AUC = mean(testAUC))
bestModels <- bestModels %>% group_by(majortype_names)  %>%  
  reframe(complexity = complexity[which(mean_AUC == max(mean_AUC))])

#Making a function that calculates the shared and unique contributions from each predictor group
variation.partitioning <- function(data, row_start, variation_measure, rounding)
{
  joint3 <- data[row_start,variation_measure]
  joint1 <- data[row_start+1,variation_measure]
  joint2 <- data[row_start+2,variation_measure]
  joint4 <- data[row_start+3,variation_measure]
  union13 <- data[row_start+5,variation_measure]
  union123 <- data[row_start+6,variation_measure]
  union1234 <- data[row_start+7,variation_measure]
  union134 <- data[row_start+8,variation_measure]
  union23 <- data[row_start+9,variation_measure]
  union234 <- data[row_start+10,variation_measure]
  union34 <- data[row_start+11,variation_measure]
  union12 <- data[row_start+12,variation_measure]
  union14 <- data[row_start+13,variation_measure]
  union124 <- data[row_start+14,variation_measure]
  union24 <- data[row_start+15,variation_measure]
  
  c1g2 <- union12 - joint2
  c1g3 <- union13 - joint3
  c1g4 <- union14 - joint4
  
  c2g1 <- union12 - joint1
  c2g3 <- union23 - joint3
  c2g4 <- union24 - joint4
  
  c3g1 <- union13 - joint1
  c3g2 <- union23 - joint2
  c3g4 <- union34 - joint4
  
  c4g1 <- union14 - joint1
  c4g2 <- union24 - joint2
  c4g3 <- union34 - joint3
  
  intersect12 <- union12 - (c1g2 + c2g1)
  intersect13 <- union13 - (c1g3 + c3g1)
  intersect14 <- union14 - (c1g4 + c4g1)
  intersect23 <- union23 - (c2g3 + c3g2)
  intersect24 <- union24 - (c2g4 + c4g2)
  intersect34 <- union34 - (c3g4 + c4g3)
  
  c1g23 <- union123 - union23
  c2g13 <- union123 - union13
  c3g12 <- union123 - union12
  i12g3 <- union123 - (joint3 + c1g23 + c2g13)
  i13g2 <- union123 - (joint2 + c1g23 + c3g12)
  i23g1 <- union123 - (joint1 + c2g13 + c3g12)
  intersect123 <- union123 - (c1g23 + c2g13 + c3g12 + i12g3 + i13g2 + i23g1) 
  
  c1g24 <- union124 - union24
  c2g14 <- union124 - union14
  c4g12 <- union124 - union12
  i12g4 <- union124 - (joint4 + c1g24 + c2g14)
  i14g2 <- union124 - (joint2 + c1g24 + c4g12)
  i24g1 <- union124 - (joint1 + c2g14 + c4g12)
  intersect124 <- union124 - (c1g24 + c2g14 + c4g12 + i12g4 + i14g2 + i24g1) 
  
  c1g34 <- union134 - union34
  c3g14 <- union134 - union14
  c4g13 <- union134 - union13
  i13g4 <- union134 - (joint4 + c1g34 + c3g14)
  i14g3 <- union134 - (joint3 + c1g34 + c4g13)
  i34g1 <- union134 - (joint1 + c3g14 + c4g13)
  intersect134 <- union134 - (c1g34 + c3g14 + c4g13 + i13g4 + i14g3 + i34g1) 
  
  c2g34 <- union234 - union34
  c3g24 <- union234 - union24
  c4g23 <- union234 - union23
  i23g4 <- union234 - (joint4 + c2g34 + c3g24)
  i24g3 <- union234 - (joint3 + c2g34 + c4g23)
  i34g2 <- union234 - (joint2 + c3g24 + c4g23)
  intersect234 <- union234 - (c2g34 + c3g24 + c4g23 + i23g4 + i24g3 + i34g2) 
  
  #intersect1234
  c1g234 <- union1234 - union234
  c2g134 <- union1234 - union134
  c3g124 <- union1234 - union124
  c4g123 <- union1234 - union123
  
  i12g34 <- union1234 - (c1g234 + c2g134 + union34)
  i23g14 <- union1234 - (c2g134 + c3g124 + union14)
  i34g12 <- union1234 - (c3g124 + c4g123 + union12)
  i14g23 <- union1234 - (c1g234 + c4g123 + union23)
  i13g24 <- union1234 - (c1g234 + c3g124 + union24)
  i24g13 <- union1234 - (c2g134 + c4g123 + union13)
  
  i123g4 <- union1234 - (joint4 + c1g234 + c2g134 + c3g124 + i12g34 + i13g24 + i23g14)
  i124g3 <- union1234 - (joint3 + c1g234 + c2g134 + c4g123 + i12g34 + i14g23 + i24g13)
  i134g2 <- union1234 - (joint2 + c1g234 + c3g124 + c4g123 + i13g24 + i14g23 + i34g12)
  i234g1 <- union1234 - (joint1 + c2g134 + c3g124 + c4g123 + i23g14 + i24g13 + i34g12)
  intersect1234 <- union1234 - (c1g234 + c2g134 + c3g124 + c4g123 + i12g34 + i23g14 + i34g12 + i14g23 + i13g24 + i24g13 + i123g4 + i124g3 + i134g2 + i234g1)
  
  areaVector <- c(c3g124, i34g12, c4g123, i13g24, i134g2, intersect1234, i234g1, i24g13, c1g234, i14g23, 
                        i124g3, i123g4, i23g14, c2g134, i12g34, joint1, joint2, joint3, joint4)
  return(areaVector)
}

#Calculating the shared and unique contributions of each predictor group for each ecosystem type
varPartList <- list()
#Looping over all ecosystem types
#Only for the complexity level with the highest average AUC
for (i in 1:length(unique(data$majortype_names))) {
  startRows <- which(data$majortype_names == bestModels$majortype_names[i])[which(data$majortype_names == bestModels$majortype_names[i]) %in% which(data$complexity == bestModels$complexity[i])]
  varPartList[[i]] <- variation.partitioning(data, startRows[1], "expD", 4)
}

#Sharing the negative components among the predictor sets
for (i in 1:length(unique(data$majortype_names))) {
  if(length(which(varPartList[[i]][1:15] < 0)) > 0)
negativeParts <- abs(sum(varPartList[[i]][1:15][which(varPartList[[i]][1:15] < 0)]))
varPartList[[i]][1:15][which(varPartList[[i]][1:15] < 0)] <- 0
varPartList[[i]][1:15] <- varPartList[[i]][1:15]-(varPartList[[i]][1:15]*(negativeParts/sum(varPartList[[i]][1:15])))
}

#Making a table with the results for each ecosystem type
varTable <- t(as.data.frame(varPartList))
varTable <- as.data.frame(varTable)
rownames(varTable) <- 1:15
colnames(varTable) <- c("T","A-T","A","T-S","A-T-S","N-A-T-S","N-A-T","N-A","S","A-S","N-A-S","N-T-S","N-T","N","N-S","Sur","Nat","Ter","Ant")

#Saving the results
#write_xlsx(rbind(varTable, varPartData), "varTable.xlsx")

#Averaging the shared and unique contributions for each predictor group over all ecosystem types
varPartData <- colMeans(as.data.frame(do.call(rbind, varPartList)))/sum(varPartData[1:15])

#Rounding to two digits
varPartNew <- round(c(varPartData[3],varPartData[2],varPartData[1],varPartData[8],
                varPartData[7],varPartData[6],varPartData[5],varPartData[4],
                varPartData[14],varPartData[13],varPartData[12],varPartData[11],
                varPartData[10],varPartData[9],varPartData[15],varPartData[19], 
                varPartData[17],varPartData[18], artData[16]),2)

#Plotting the results
drawVenn <- draw.quad.venn(category = c(paste("Natural = ", round(varPartNew[17], digits = 2), sep = ""),
                                        paste("Surface = ", round(varPartNew[19], digits = 2), sep = ""),
                                        paste("Anthropogenic = ", round(varPartNew[16], digits = 2), sep = ""),
                                        paste("Terrain = ", round(varPartNew[18], digits = 2), sep = "")), lwd = rep(2, 4), lty = rep("solid", 4), 
                           col = rep("black", 4), fill = as.character(colors_predictor_sets[c(1,4,2,3)]), 
                           alpha = rep(0.5, 4), label.col = rep("black", 15), cex = rep(2.5, 15),
                           fontface = rep("bold", 15), fontfamily = rep("Tahoma",15), 
                           cat.pos = c(-12, 12, 0, 0), cat.dist = c(0.22,0.22, 0.11, 0.11), 
                           cat.col = rep("black", 4), cat.cex = rep(2.5, 4), cat.fontface = rep("bold", 4),
                           cat.fontfamily = rep("Tahoma", 4), cat.just = rep(list(c(0.5, 0.5)), 4), rotation.degree = 0,
                           rotation.centre = c(0.5, 0.5), ind = TRUE, cex.prop = exp, 
                           print.mode = "raw", sigdigs = 2, direct.area = TRUE, area.vector = varPartNew[1:15])

