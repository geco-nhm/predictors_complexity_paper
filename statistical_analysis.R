# 1 Import libraries ----
library(ggplot2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(colorspace)
library(extrafont)
library(gridExtra)
library(VennDiagram)
library(cowplot)
library(writexl)
library(readxl)
library(egg)

# 2 Import data ----
setwd("C:/Users/adamen/OneDrive - Universitetet i Oslo/documents/Doktorgrad/Artikkel 2/R")
dataList <- list()
for (i in 1:length(list.files("new_evaluation/",
                              pattern = ".csv"))) {
  dataList[[i]] <- read.csv(paste("new_evaluation/",
                                  list.files("new_evaluation/",
                                             pattern = ".csv")[i], sep = ""))[,-c(1)]
}
data <- as.data.frame(do.call(rbind, dataList))

# 3 Calculate metrics ----
#Making columns for ecosystem type, complexity level, major-type group and predictor sets included
n <- length(which(data$majortype == 14))
data$complexity <- rep(c(rep(1,n/5),rep(2,n/5),rep(3,n/5),rep(4,n/5),rep(5,n/5)),length(unique(data$majortype)))
data$complexity <- c("Low","Low-medium","Medium","Medium-High","High")[data$complexity]
data$complexity <- factor(data$complexity, levels = c("Low","Low-medium","Medium","Medium-High","High"))
data$majortype_group[which(data$majortype > 45)] <- "V"
data$majortype_group[which(data$majortype < 46)] <- "T"
data$majortype_names <- ifelse(data$majortype < 46, paste("T", data$majortype, sep = ""), paste("V", data$majortype-45, sep = ""))
data$majortype_names <- factor(data$majortype_names, levels = c("T1","T3","T4","T7","T14","T19","T22","T27","T30","T31","T32","T34","V1","V2","V3"))
data$sets_included <- rowSums(data[,75:78])

#Calculating TI (transferability index)
data$ext_test_AUC <- (data$north_ext_testAUC*data$north_ext_number_test+data$south_ext_testAUC*data$south_ext_number_test)/(data$north_ext_number_test+data$south_ext_number_test)
data$int_test_AUC <- (data$north_testAUC*data$north_number_test+data$south_testAUC*data$south_number_test)/(data$north_number_test+data$south_number_test)
data$ext_train_AUC <- (data$north_ext_trainAUC*data$north_ext_number_train+data$south_ext_trainAUC*data$south_ext_number_train)/(data$north_ext_number_train+data$south_ext_number_train)
data$int_train_AUC <- (data$north_trainAUC*data$north_number_train+data$south_trainAUC*data$south_number_train)/(data$north_number_train+data$south_number_train)
data$TI <- apply(cbind(data$ext_test_AUC-data$int_train),1,mean)

#Column for predictor sets
data$set_combination <- rep(c("T","S","N","A","P","T-S","N-T-S","N-A-T-S","A-T-S","N-T","N-A-T","A-T","N-S","A-S","N-A-S","N-A"),5*15)
#Order the factor levels
data$set_combination <- factor(data$set_combination, levels = c("N","A","T","S","N-A","N-T",
                                                                "N-S","A-T","A-S","T-S","N-A-T",
                                                                "N-A-S","N-T-S","A-T-S","N-A-T-S","P"))

#Column for neutral predictors (not used in article)
data$Neutral <- numeric(nrow(data))
data$Neutral[which(data$added_set1 == 5)] <- 1
#Changing the number of parameters and predictors in intercept-only models to zero
data$EVs[which(rowSums(data[,c(75:78,334:345)]) == 0)] <- 0
data$DVs[which(rowSums(data[,c(75:78,334:345)]) == 0)] <- 0
#Filtering out models with neutral predictors and intercept-only models
data <- data[-which(data$Neutral == 1),]
data <- data[which(data$EVs != 0),]


# 4 Define colors----
colors_majortype <- setNames(c("gray80", "lightcoral", "green4", "wheat1", "darkseagreen", "plum1", "deeppink4", 
                               "slategray", "mediumspringgreen", "darkgoldenrod4", "yellow1", "firebrick2", "steelblue1", "deepskyblue4","navy"), 
                             levels(data$majortype_names))

colors_complexity <- setNames(c("gray80","gray60","gray40","gray20","gray0"), levels(data$complexity))

colors_predictor_sets <- setNames(c("darkgreen","deepskyblue4","khaki","firebrick"), c("Natural","Anthropogenic","Terrain","Surface"))

colors_set_combinations <- setNames(c("mediumspringgreen", "yellow1", "deepskyblue4", "deeppink4", "darkseagreen", "steelblue1", "lightcoral", 
       
                                                                     "green4", "plum1", "firebrick2", "navy", "brown", "magenta", "mediumpurple","darkmagenta","black"), levels(data$set_combination))
# 5 Figure 2 and 3 ----------------
p <- list()
leg_pos <- "right"
leg_dir <- "vertical"
leg_jus <- c(1,1)
myPalette <- colorRampPalette(colors=c("#C4D29D","#9AD0C2", "#2D9596", "#265073"))

means_df <- data %>% 
  group_by(set_combination, majortype_names) %>% 
  summarise(across(c(TI, testAUC), mean))

line_df <- data %>% 
  summarise(across(c(TI, testAUC), mean))

p[[1]] <- ggplot(data, aes(x = factor(set_combination), y = TI)) +
  geom_boxplot(outlier.shape = NA) +
  geom_abline(data = line_df, aes(intercept = TI, slope = 0), color = "black", linewidth = 0.5, linetype = 2) +
  geom_jitter(data = means_df, aes(color = factor(majortype_names)), size = 2) +
  scale_color_manual(values = colors_majortype, name = "Ecosystem type") +
  xlab("Predictor set") +
  ylab("TI") +
  theme_bw() +
  guides(colour = guide_legend(override.aes = list(size = 8))) +
  theme(text = element_text(family = "Tahoma", color = "black"),
        axis.text.x = element_text(size = 12, family = "Tahoma", color = "black"), 
        axis.text.y = element_text(size = 12, family = "Tahoma", color = "black"),
        axis.title.x = element_text(size = 12, family = "Tahoma", color = "black"),
        axis.title.y = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.title = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.text = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.position = leg_pos, 
        legend.justification = leg_jus,
        legend.direction = leg_dir)

p[[2]] <- ggplot(data, aes(x = factor(set_combination), y = testAUC)) +
  geom_boxplot(outlier.shape = NA) +
  geom_abline(data = line_df, aes(intercept = testAUC, slope = 0), color = "black", linewidth = 0.5, linetype = 2) +
  geom_jitter(data = means_df, aes(color = factor(majortype_names)), size = 2) +
  scale_color_manual(values = colors_majortype, name = "Ecosystem type") +
  scale_y_continuous(breaks = round(seq(0.2, 1.0, by = 0.1),1)) +
  xlab("Predictor set") +
  ylab("AUC") +
  theme_bw() +
  guides(colour = guide_legend(override.aes = list(size = 8))) +
  theme(text = element_text(family = "Tahoma", color = "black"),
        axis.text.x = element_text(size = 12, family = "Tahoma", color = "black"), 
        axis.text.y = element_text(size = 12, family = "Tahoma", color = "black"),
        axis.title.x = element_text(size = 12, family = "Tahoma", color = "black"),
        axis.title.y = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.title = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.text = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.position = leg_pos, 
        legend.justification = leg_jus,
        legend.direction = leg_dir)

means_df <- data %>% 
  group_by(majortype_names, set_combination) %>% 
  summarise(across(c(TI, testAUC), mean))

line_df <- data %>% 
  summarise(across(c(TI, testAUC), mean))

p[[3]] <- ggplot(data, aes(x = factor(majortype_names), y = TI)) +
  geom_boxplot(outlier.shape = NA) +
  geom_abline(data = line_df, aes(intercept = TI, slope = 0), color = "black", linewidth = 0.5, linetype = 2) +
  geom_jitter(data = means_df, aes(color = factor(set_combination)), size = 2) +
  scale_color_manual(values = colors_set_combinations, name = "Predictor set") +
  xlab("Ecosystem type") +
  ylab("TI") +
  theme_bw() +
  guides(colour = guide_legend(override.aes = list(size = 8))) +
  theme(text = element_text(family = "Tahoma", color = "black"),
        axis.text.x = element_text(size = 12, family = "Tahoma", color = "black"), 
        axis.text.y = element_text(size = 12, family = "Tahoma", color = "black"),
        axis.title.x = element_text(size = 12, family = "Tahoma", color = "black"),
        axis.title.y = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.title = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.text = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.position = leg_pos, 
        legend.justification = leg_jus,
        legend.direction = leg_dir)

p[[4]] <- ggplot(data, aes(x = factor(majortype_names), y = testAUC)) +
  geom_boxplot(outlier.shape = NA) +
  geom_abline(data = line_df, aes(intercept = testAUC, slope = 0), color = "black", linewidth = 0.5, linetype = 2) +
  geom_jitter(data = means_df, aes(color = factor(set_combination)), size = 2) +
  scale_color_manual(values = colors_set_combinations, name = "Predictor set") +
  scale_y_continuous(breaks = round(seq(0.2, 1.0, by = 0.1),1)) +
  xlab("Ecosystem type") +
  ylab("AUC") +
  theme_bw() +
  guides(colour = guide_legend(override.aes = list(size = 8))) +
  theme(text = element_text(family = "Tahoma", color = "black"),
        axis.text.x = element_text(size = 12, family = "Tahoma", color = "black"), 
        axis.text.y = element_text(size = 12, family = "Tahoma", color = "black"),
        axis.title.x = element_text(size = 12, family = "Tahoma", color = "black"),
        axis.title.y = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.title = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.text = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.position = leg_pos, 
        legend.justification = leg_jus,
        legend.direction = leg_dir)

p[[5]] <- ggplot(data, aes(x = DVs, y = TI, color = testAUC)) +
  geom_point(alpha = 0.8) +
  geom_abline(data = line_df, aes(intercept = TI, slope = 0), color = "black", linewidth = 0.5, linetype = 2) +
  geom_smooth(data = data, aes(x = DVs, y = TI), color = "black", linewidth = 1.5, span = 1.5, formula = y ~ poly(x, 4), se = T) +
  scale_x_continuous(breaks = round(seq(0, 150, by = 10),1), limits = c(0,150)) +
  scale_colour_gradientn(colours = myPalette(100), name = "AUC") +
  xlab("Number of parameters") +
  ylab("TI") +
  theme_bw() +
  theme(text = element_text(family = "Tahoma", color = "black"),
        axis.text.x = element_text(size = 12, family = "Tahoma", color = "black"), 
        axis.text.y = element_text(size = 12, family = "Tahoma", color = "black"),
        axis.title.x = element_text(size = 12, family = "Tahoma", color = "black"),
        axis.title.y = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.title = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.text = element_text(size = 12, family = "Tahoma", color = "black", hjust = 1, margin = margin(l = 8)),
        legend.key.width = unit(0.2, "cm"),
        legend.position = leg_pos, 
        legend.justification = leg_jus,
        legend.direction = leg_dir)

p[[6]] <- ggplot(data, aes(x = DVs, y = testAUC, color = TI)) +
  geom_point(alpha = 0.8) +
  geom_abline(data = line_df, aes(intercept = testAUC, slope = 0), color = "black", linewidth = 0.5, linetype = 2) +
  geom_smooth(data = data, aes(x = DVs, y = testAUC), color = "black", linewidth = 1.5, span = 1.5, formula = y ~ poly(x, 4), se = T) +
  scale_x_continuous(breaks = round(seq(0, 150, by = 10),1), limits = c(0,150)) +
  scale_y_continuous(breaks = round(seq(0.2, 1.0, by = 0.1),1), limits = c(0.2,1.0)) +
  scale_colour_gradientn(colours = myPalette(100), name = "TI") +
  xlab("Number of parameters") +
  ylab("AUC") +
  theme_bw() +
  theme(text = element_text(family = "Tahoma", color = "black"),
        axis.text.x = element_text(size = 12, family = "Tahoma", color = "black"), 
        axis.text.y = element_text(size = 12, family = "Tahoma", color = "black"),
        axis.title.x = element_text(size = 12, family = "Tahoma", color = "black"),
        axis.title.y = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.title = element_text(size = 12, family = "Tahoma", color = "black"),
        legend.text = element_text(size = 12, family = "Tahoma", color = "black", hjust = 1, margin = margin(l = 8)),
        legend.key.width = unit(0.2, "cm"),
        legend.position = leg_pos, 
        legend.justification = leg_jus,
        legend.direction = leg_dir)

plot_grid(p[[3]] + theme(legend.margin = margin(t = 0, r = 55, b = 0, l = 0)), 
          p[[1]] + theme(legend.margin = margin(t = 0, r = 55, b = 0, l = 0)),
          p[[5]] + theme(legend.margin = margin(t = 0, r = 55, b = 0, l = 0)),
          nrow = 3, align = "v", axis = "tblr", labels = c("(a)","(b)","(c)"), hjust = -0.2, vjust = 4)


plot_grid(p[[4]] + theme(legend.margin = margin(t = 0, r = 55, b = 0, l = 0)), 
          p[[2]] + theme(legend.margin = margin(t = 0, r = 55, b = 0, l = 0)),
          p[[6]] + theme(legend.margin = margin(t = 0, r = 55, b = 0, l = 0)),
          nrow = 3, align = "v", axis = "tblr", labels = c("(a)","(b)","(c)"), hjust = -0.2, vjust = 4)

# 6 Tables ----------------
#How many ecosystem types have a significant unimodal relationship between AUC and TI?
for (i in levels(data$majortype_names)) {
  print(paste(i,"Linear:", round(summary(lm(testAUC~TI, data[which(data$majortype_names == i),]))$coefficients[2,4],3)))
  #print(paste(i,"Unimodal:", round(summary(lm(testAUC~TI+I(TI^2), data[which(data$majortype_names == i),]))$coefficients[3,4],3)))
}
for (i in levels(data$majortype_names)) {
  print(paste(i,"Linear:", round(summary(lm(testAUC~TI, data[which(data$majortype_names == i),]))$coefficients[2,1],3)))
  print(paste(i,"Unimodal:", round(summary(lm(testAUC~TI+I(TI^2), data[which(data$majortype_names == i),]))$coefficients[3,1],3)))
}
print(summary(lm(testAUC~TI, data))$coefficients[2,4],3)
print(summary(lm(testAUC~TI+I(TI^2), data))$coefficients[2,4],3)

print(summary(lm(testAUC-mean(data$testAUC)~majortype_names-1, data)))
print(summary(lm(TI-mean(data$TI)~majortype_names-1, data)))

print(summary(lm(testAUC-mean(data$testAUC)~set_combination-1, data)))
print(summary(lm(TI-mean(data$TI)~set_combination-1, data)))
print(summary(lm(testAUC-mean(data$testAUC)~sets_included-1, data)))

#How many presences and absences in each region?
ecosystemTable <- data %>% group_by(majortype_names) %>% reframe(across(c(number_train,
                                                                          south_number_train,
                                                                          north_number_train,
                                                                          south_ext_number_train,
                                                                          north_ext_number_train,
                                                                          number_test,
                                                                          south_number_test,
                                                                          north_number_test,
                                                                          south_ext_number_test,
                                                                          north_ext_number_test), mean))
#write.csv(as.data.frame(ecosystemTable),"ecosystem_type_table.csv")

#How is sample size related to AUC and TI?
summary(lm(testAUC~number_train,data))
summary(lm(TI~number_train,data))

summary(lm(testAUC~DVs+I(DVs^2),data))
summary(lm(TI~DVs+I(DVs^2),data))

#Statistics for model design parameters
data %>% reframe(across(c(testAUC), mean))
data %>% reframe(across(c(TI), mean))

#Statistics - best models
best_models <- data %>%
  group_by(majortype_names) %>% 
  reframe(across(c(best_testAUC = testAUC), max),
          TI[which(best_testAUC == testAUC)],
          set_combination[which(best_testAUC == testAUC)],
          DVs[which(best_testAUC == testAUC)],
          EVs[which(best_testAUC == testAUC)])

# 7 Figure 5 ----------------
data$majortype_names <- factor(data$majortype_names, levels = c("T1","T3","T4","T7","T14","T19","T22","T27","T30","T31","T32","T34","V1","V2","V3"))
colnames(data)[32] <- "AUC"
colnames(data)[1148] <- "TI"

plotList <- list()
low_AUC <- 0.5
low_TI <- -0.5
high_TI <- round(max(data$TI),1)
high_DV <- round(max(data$DVs),0)+3
leg_pos <- c(0.05, 0.75)
leg_dir <- "vertical"
leg_jus <- "left"
#myPalette <- colorRampPalette(brewer.pal(9, "GnBu"))
myPalette <- colorRampPalette(colors=c("#C4D29D","#9AD0C2", "#2D9596", "#265073"))

naturtyper <- levels(data$majortype_names)

labels <- c("(a)","(b)","(c)","(d)",
            "(e)","(f)","(g)","(h)",
            "(i)","(j)","(k)","(l)",
            "(m)","(n)","(o)","(p)")


{
  (plotList[[1]] <- ggplot(data, aes(x = TI, y = AUC, color = DVs)) +
     #geom_abline(data = line_df, aes(intercept = AUC, slope = 0), color = "black", linewidth = 0.5, linetype = 2) +
     geom_point(alpha = 0.8) +
     geom_smooth(data = data, aes(x = TI, y = AUC), color = "black", linewidth = 1.5, span = 1.5, method = "gam", formula = y ~ poly(x, 2)) +
     scale_x_continuous(breaks = seq(low_TI, high_TI, by = 0.1), limits = c(low_TI,high_TI)) +
     scale_y_continuous(breaks = seq(low_AUC, 1.025, by = 0.1), limits = c(low_AUC,1.025)) +
     scale_colour_gradientn(breaks = seq(0, high_DV, by = 25),
                            colours = myPalette(100),
                            name = "Number of parameters", limits = c(0,high_DV)) +
     
     geom_text(x = -0.05, y = 0.50, size = 5, color = "black", family = "Tahoma", hjust = 0,
               label = ifelse(cor.test(data$AUC, data$TI)$p.value < 0.001, 
                              paste("p < 0.001"), paste("p = ", round(cor.test(data$AUC, data$TI)$p.value,3), sep = ""))) +
     geom_text(x = -0.05, y = 0.53, label = paste("r = ", 
                                                  round(cor.test(data$AUC, data$TI)$estimate, 2), sep = ""), 
               size = 5, color = "black", family = "Tahoma", hjust = 0) +
     xlab("TI") +
     ylab("AUC") +
     geom_text(x = -0.475, y = 1.025, 
               label = paste0(labels[1]), size = 7, family = "Tahoma", color = "black") +
     geom_text(x = -0.2, y = 1.025, 
               label = "All ecosystem types", size = 7, family = "Tahoma", color = "black") + #ggtitle("All ecosystem types") +
     theme_bw() +
     theme(text = element_text(family = "Tahoma", color = "black"),
           plot.title = element_text(color = "black", size = 16, face = "bold",  family = "Tahoma", hjust = 0.5),
           axis.text.x = element_text(size = 16, family = "Tahoma", color = "black"), 
           axis.text.y = element_text(size = 16, family = "Tahoma", color = "black"),
           axis.title.x = element_text(size = 18, family = "Tahoma", color = "black"),
           axis.title.y = element_text(size = 18, family = "Tahoma", color = "black"),
           
           legend.title = element_text(size = 18, family = "Tahoma", color = "black", hjust = 1, vjust = 0.5),
           legend.text = element_text(size = 16, family = "Tahoma", color = "black", hjust = 1, margin = margin(l = 8)),
           legend.direction="horizontal",
           legend.key.width = unit(4, "cm"),
           legend.position = "none", 
           
           panel.grid.minor = element_blank(),
           plot.margin = margin(0, 0, 0, 0, "cm")
     )
  )
  
  for (i in 1:length(levels(data$majortype_names))) {
    
    plotList[[i+1]] <- ggplot(data[which(data$majortype_names == naturtyper[i]),], aes(x = TI, y = AUC, color = DVs)) +
      geom_point(alpha = 0.8) +
      #geom_abline(data = line_df, aes(intercept = AUC, slope = 0), color = "black", linewidth = 0.5, linetype = 2) +
      geom_smooth(data = data[which(data$majortype_names == naturtyper[i]),],
                  aes(x = TI, y = AUC), color = "black", linewidth = 1.5, span = 1.5, method = "gam", formula = y ~ poly(x, 2), se = T) +
      
      scale_x_continuous(breaks = seq(low_TI, high_TI, by = 0.1), limits = c(low_TI,high_TI)) +
      scale_y_continuous(breaks = seq(low_AUC, 1.025, by = 0.1), limits = c(low_AUC,1.025)) +
      
      scale_colour_gradientn(breaks = seq(0, high_DV, by = 25), colours = myPalette(100), name = "Number of parameters", limits = c(0,high_DV)) +
      xlab("TI") +
      ylab("AUC") +
      geom_text(x = -0.475, y = 1.025, 
                label = paste0(labels[i+1]), size = 7, family = "Tahoma", color = "black") +
      geom_text(x = -0.05, y = 0.50, 
                size = 5, color = "black", family = "Tahoma", hjust = 0,
                label = ifelse(cor.test(data[which(data$majortype_names == naturtyper[i]),]$AUC, data[which(data$majortype_names == naturtyper[i]),]$TI)$p.value < 0.001, 
                               paste("p < 0.001"), paste("p = ", round(cor.test(data[which(data$majortype_names == naturtyper[i]),]$AUC, data[which(data$majortype_names == naturtyper[i]),]$TI)$p.value,3), sep = ""))) +
      geom_text(x = -0.05, y = 0.53, 
                label = paste("r = ", round(cor.test(data[which(data$majortype_names == naturtyper[i]),]$AUC, data[which(data$majortype_names == naturtyper[i]),]$TI)$estimate, 2), sep = ""), 
                size = 5, color = "black", family = "Tahoma", hjust = 0) +
      geom_text(x = -0.2, y = 1.025, 
                label = paste0(naturtyper[i]), size = 7, family = "Tahoma", color = "black") + #ggtitle(i) +      
      theme_bw() +
      theme(text = element_text(family = "Tahoma", color = "black"),
            plot.title = element_text(color = "black", size = 16, face = "bold",  family = "Tahoma", hjust = 0.5),
            axis.text.x = element_text(size = 16, family = "Tahoma", color = "black"), 
            axis.text.y = element_text(size = 16, family = "Tahoma", color = "black"),
            axis.title.x = element_text(size = 18, family = "Tahoma", color = "black"),
            axis.title.y = element_text(size = 18, family = "Tahoma", color = "black"),
            
            legend.title = element_text(size = 18, family = "Tahoma", color = "black", hjust = 1, vjust = 0.5),
            legend.text = element_text(size = 16, family = "Tahoma", color = "black", hjust = 1, margin = margin(l = 8)),
            legend.direction="horizontal",
            legend.key.width = unit(3, "cm"),
            legend.position = "none",
            
            panel.grid.minor = element_blank(),
            plot.margin = margin(0, 0, 0, 0, "cm")
      )
    
  }
  
  # Fjern tekst p?? y-akse
  rem_yaxistitle <- seq(1,16,1)[-c(1,5,9,13)]
  
  for(i in rem_yaxistitle){
    print(i)
    plotList[[i]] <- plotList[[i]] +
      theme(axis.title.y = element_blank())
  }
  
  # Fjern tekst p?? x-akse
  rem_xaxistitle <- 1:12
  
  for(i in rem_xaxistitle){
    print(i)
    plotList[[i]] <- plotList[[i]] +
      theme(axis.title.x = element_blank())
  }
  
  
  ggarrange_plots <- ggpubr::ggarrange(plotlist=plotList, 
                                       nrow = 4, ncol = 4, 
                                       common.legend = T,
                                       legend = "bottom",
                                       align = "hv")
  
  ggsave("fig5.png", ggarrange_plots, width = 50, height = 50, units = "cm", dpi = 300)
}

# 8 Appendix plots ----------------
data$majortype_names <- factor(data$majortype_names, levels = c("T1","T3","T4","T7","T14","T19","T22","T27","T30","T31","T32","T34","V1","V2","V3"))
colnames(data)[32] <- "AUC"
colnames(data)[1148] <- "TI"

plotList <- list()
low_AUC <- round(min(data$AUC),1)
low_TI <- round(min(data$DVs),1)
high_TI <- round(max(data$DVs),1)+3
high_DV <- 80000
leg_pos <- c(0.05, 0.75)
leg_dir <- "vertical"
leg_jus <- "left"
myPalette <- colorRampPalette(colors=c("#C4D29D","#9AD0C2", "#2D9596", "#265073"))

naturtyper <- levels(data$majortype_names)

labels <- c("(a)","(b)","(c)","(d)",
            "(e)","(f)","(g)","(h)",
            "(i)","(j)","(k)","(l)",
            "(m)","(n)","(o)","(p)")

{
  (plotList[[1]] <- ggplot(data, aes(x = DVs, y = AUC, color = number_train)) +
     #geom_abline(data = line_df, aes(intercept = AUC, slope = 0), color = "black", linewidth = 0.5, linetype = 2) +
     geom_point(alpha = 0.8) +
     geom_smooth(data = data, aes(x = DVs, y = AUC), color = "black", linewidth = 1.5, span = 1.5, method = "gam", formula = y ~ poly(x, 2)) +
     scale_x_continuous(breaks = seq(0, high_TI, by = 25), limits = c(0,high_TI)) +
     scale_y_continuous(breaks = seq(low_AUC, 1.025, by = 0.1), limits = c(low_AUC,1.025)) +
     scale_colour_gradientn(breaks = seq(0, high_DV, by = 20000),
                            colours = myPalette(100),
                            name = "Sample size", limits = c(0,high_DV)) +
     xlab("Number of parameters") +
     ylab("AUC") +
     geom_text(x = 3, y = 1.025, 
               label = paste0(labels[1]), size = 7, family = "Tahoma", color = "black") +
     geom_text(x = 75, y = 1.025, 
               label = "All ecosystem types", size = 7, family = "Tahoma", color = "black") + #ggtitle("All ecosystem types") +
     theme_bw() +
     theme(text = element_text(family = "Tahoma", color = "black"),
           plot.title = element_text(color = "black", size = 16, face = "bold",  family = "Tahoma", hjust = 0.5),
           axis.text.x = element_text(size = 16, family = "Tahoma", color = "black"), 
           axis.text.y = element_text(size = 16, family = "Tahoma", color = "black"),
           axis.title.x = element_text(size = 18, family = "Tahoma", color = "black"),
           axis.title.y = element_text(size = 18, family = "Tahoma", color = "black"),
           
           legend.title = element_text(size = 18, family = "Tahoma", color = "black", hjust = 1, vjust = 0.5),
           legend.text = element_text(size = 16, family = "Tahoma", color = "black", hjust = 1, margin = margin(l = 8)),
           legend.direction="horizontal",
           legend.key.width = unit(4, "cm"),
           legend.position = "none", 
           
           panel.grid.minor = element_blank(),
           plot.margin = margin(.4, .4, .4, .4, "cm")
     )
  )
  
  for (i in 1:length(levels(data$majortype_names))) {
    
    plotList[[i+1]] <- ggplot(data[which(data$majortype_names == naturtyper[i]),], aes(x = DVs, y = AUC, color = number_train)) +
      geom_point(alpha = 0.8) +
      #geom_abline(data = line_df, aes(intercept = AUC, slope = 0), color = "black", linewidth = 0.5, linetype = 2) +
      geom_smooth(data = data[which(data$majortype_names == naturtyper[i]),],
                  aes(x = DVs, y = AUC), color = "black", linewidth = 1.5, span = 1.5, method = "gam", formula = y ~ poly(x, 2), se = T) +
      
      scale_x_continuous(breaks = seq(0, high_TI, by = 25), limits = c(0,high_TI)) +
      scale_y_continuous(breaks = seq(low_AUC, 1.025, by = 0.1), limits = c(low_AUC,1.025)) +
      
      scale_colour_gradientn(breaks = seq(0, high_DV, by = 20000), colours = myPalette(100), name = "Number of parameters", limits = c(0,high_DV)) +
      xlab("Number of parameters") +
      ylab("AUC") +
      geom_text(x = 3, y = 1.025, 
                label = paste0(labels[i+1]), size = 7, family = "Tahoma", color = "black") +
      geom_text(x = 75, y = 1.025, 
                label = paste0(naturtyper[i]), size = 7, family = "Tahoma", color = "black") + #ggtitle(i) +      
      theme_bw() +
      theme(text = element_text(family = "Tahoma", color = "black"),
            plot.title = element_text(color = "black", size = 16, face = "bold",  family = "Tahoma", hjust = 0.5),
            axis.text.x = element_text(size = 16, family = "Tahoma", color = "black"), 
            axis.text.y = element_text(size = 16, family = "Tahoma", color = "black"),
            axis.title.x = element_text(size = 18, family = "Tahoma", color = "black"),
            axis.title.y = element_text(size = 18, family = "Tahoma", color = "black"),
            
            legend.title = element_text(size = 18, family = "Tahoma", color = "black", hjust = 1, vjust = 0.5),
            legend.text = element_text(size = 16, family = "Tahoma", color = "black", hjust = 1, margin = margin(l = 8)),
            legend.direction="horizontal",
            legend.key.width = unit(3, "cm"),
            legend.position = "none",
            
            panel.grid.minor = element_blank(),
            plot.margin = margin(.4, .4, .4, .4, "cm")
      )
    
  }
  
  # Fjern tekst p?? y-akse
  rem_yaxistitle <- seq(1,16,1)[-c(1,5,9,13)]
  
  for(i in rem_yaxistitle){
    print(i)
    plotList[[i]] <- plotList[[i]] +
      theme(axis.title.y = element_blank())
  }
  
  # Fjern tekst p?? x-akse
  rem_xaxistitle <- 1:12
  
  for(i in rem_xaxistitle){
    print(i)
    plotList[[i]] <- plotList[[i]] +
      theme(axis.title.x = element_blank())
  }
  
  
  ggarrange_plots <- ggpubr::ggarrange(plotlist=plotList, 
                                       nrow = 4, ncol = 4, 
                                       common.legend = T,
                                       legend = "bottom",
                                       align = "hv")
  
  ggsave("figS1.png", ggarrange_plots, width = 50, height = 50, units = "cm", dpi = 300)
}

plotList <- list()
low_AUC <- round(min(data$TI),1)
low_TI <- round(min(data$DVs),1)-1
high_TI <- round(max(data$DVs),1)+3
high_DV <- 80000
leg_pos <- c(0.05, 0.75)
leg_dir <- "vertical"
leg_jus <- "left"
myPalette <- colorRampPalette(colors=c("#C4D29D","#9AD0C2", "#2D9596", "#265073"))

naturtyper <- levels(data$majortype_names)

labels <- c("(a)","(b)","(c)","(d)",
            "(e)","(f)","(g)","(h)",
            "(i)","(j)","(k)","(l)",
            "(m)","(n)","(o)","(p)")


{
  (plotList[[1]] <- ggplot(data, aes(x = DVs, y = TI, color = number_train)) +
     #geom_abline(data = line_df, aes(intercept = AUC, slope = 0), color = "black", linewidth = 0.5, linetype = 2) +
     geom_point(alpha = 0.8) +
     geom_smooth(data = data, aes(x = DVs, y = TI), color = "black", linewidth = 1.5, span = 1.5, method = "gam", formula = y ~ poly(x, 2)) +
     scale_x_continuous(breaks = seq(low_TI, high_TI, by = 25), limits = c(low_TI,high_TI)) +
     scale_y_continuous(breaks = seq(low_AUC, 0.1025, by = 0.1), limits = c(low_AUC,0.1025)) +
     scale_colour_gradientn(breaks = seq(0, high_DV, by = 20000),
                            colours = myPalette(100),
                            name = "Sample size", limits = c(0,high_DV)) +
     xlab("Number of parameters") +
     ylab("TI") +
     geom_text(x = 3, y = 1.025, 
               label = paste0(labels[1]), size = 7, family = "Tahoma", color = "black") +
     geom_text(x = 75, y = 1.025, 
               label = "All ecosystem types", size = 7, family = "Tahoma", color = "black") + #ggtitle("All ecosystem types") +
     theme_bw() +
     theme(text = element_text(family = "Tahoma", color = "black"),
           plot.title = element_text(color = "black", size = 16, face = "bold",  family = "Tahoma", hjust = 0.5),
           axis.text.x = element_text(size = 16, family = "Tahoma", color = "black"), 
           axis.text.y = element_text(size = 16, family = "Tahoma", color = "black"),
           axis.title.x = element_text(size = 18, family = "Tahoma", color = "black"),
           axis.title.y = element_text(size = 18, family = "Tahoma", color = "black"),
           
           legend.title = element_text(size = 18, family = "Tahoma", color = "black", hjust = 1, vjust = 0.5),
           legend.text = element_text(size = 16, family = "Tahoma", color = "black", hjust = 1, margin = margin(l = 8)),
           legend.direction="horizontal",
           legend.key.width = unit(4, "cm"),
           legend.position = "none", 
           
           panel.grid.minor = element_blank(),
           plot.margin = margin(.4, .4, .4, .4, "cm")
     )
  )
  
  for (i in 1:length(levels(data$majortype_names))) {
    
    plotList[[i+1]] <- ggplot(data[which(data$majortype_names == naturtyper[i]),], aes(x = DVs, y = TI, color = number_train)) +
      geom_point(alpha = 0.8) +
      #geom_abline(data = line_df, aes(intercept = AUC, slope = 0), color = "black", linewidth = 0.5, linetype = 2) +
      geom_smooth(data = data[which(data$majortype_names == naturtyper[i]),],
                  aes(x = DVs, y = TI), color = "black", linewidth = 1.5, span = 1.5, method = "gam", formula = y ~ poly(x, 2), se = T) +
      
      scale_x_continuous(breaks = seq(low_TI, high_TI, by = 25), limits = c(low_TI,high_TI)) +
      scale_y_continuous(breaks = seq(low_AUC, 0.1025, by = 0.1), limits = c(low_AUC,0.1025)) +
      
      scale_colour_gradientn(breaks = seq(0, high_DV, by = 20000), colours = myPalette(100), name = "Number of parameters", limits = c(0,high_DV)) +
      xlab("Number of parameters") +
      ylab("TI") +
      geom_text(x = 3, y = 1.025, 
                label = paste0(labels[i+1]), size = 7, family = "Tahoma", color = "black") +
      geom_text(x = 75, y = 1.025, 
                label = paste0(naturtyper[i]), size = 7, family = "Tahoma", color = "black") + #ggtitle(i) +      
      theme_bw() +
      theme(text = element_text(family = "Tahoma", color = "black"),
            plot.title = element_text(color = "black", size = 16, face = "bold",  family = "Tahoma", hjust = 0.5),
            axis.text.x = element_text(size = 16, family = "Tahoma", color = "black"), 
            axis.text.y = element_text(size = 16, family = "Tahoma", color = "black"),
            axis.title.x = element_text(size = 18, family = "Tahoma", color = "black"),
            axis.title.y = element_text(size = 18, family = "Tahoma", color = "black"),
            
            legend.title = element_text(size = 18, family = "Tahoma", color = "black", hjust = 1, vjust = 0.5),
            legend.text = element_text(size = 16, family = "Tahoma", color = "black", hjust = 1, margin = margin(l = 8)),
            legend.direction="horizontal",
            legend.key.width = unit(3, "cm"),
            legend.position = "none",
            
            panel.grid.minor = element_blank(),
            plot.margin = margin(.4, .4, .4, .4, "cm")
      )
    
  }
  
  # Fjern tekst p?? y-akse
  rem_yaxistitle <- seq(1,16,1)[-c(1,5,9,13)]
  
  for(i in rem_yaxistitle){
    print(i)
    plotList[[i]] <- plotList[[i]] +
      theme(axis.title.y = element_blank())
  }
  
  # Fjern tekst p?? x-akse
  rem_xaxistitle <- 1:12
  
  for(i in rem_xaxistitle){
    print(i)
    plotList[[i]] <- plotList[[i]] +
      theme(axis.title.x = element_blank())
  }
  
  
  ggarrange_plots <- ggpubr::ggarrange(plotlist=plotList, 
                                       nrow = 4, ncol = 4, 
                                       common.legend = T,
                                       legend = "bottom",
                                       align = "hv")
  
  ggsave("figS2.png", ggarrange_plots, width = 50, height = 50, units = "cm", dpi = 300)
}