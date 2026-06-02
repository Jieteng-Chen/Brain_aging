library(openxlsx)
library(dplyr)
library(tidyverse)
library(reshape2)
library(car)
library(ggplot2)
library(ggrepel)
library(RColorBrewer)
library(ggpubr)
library(ggsignif) 
library(pROC)
library(pheatmap)
library(colorspace)
library(MetBrewer) 

Manet <- MetBrewer::met.brewer("Manet")
Benedictus <- MetBrewer::met.brewer("Benedictus")
Archambault <- MetBrewer::met.brewer("Archambault")
OKeeffe1 <- MetBrewer::met.brewer("OKeeffe1")
OKeeffe2 <- MetBrewer::met.brewer("OKeeffe2")
Signac <- MetBrewer::met.brewer("Signac")
Morgenstern <- MetBrewer::met.brewer("Morgenstern")
VanGogh3 <- MetBrewer::met.brewer("VanGogh3")
Paquin <- MetBrewer::met.brewer("Paquin")


#[Chronological age and Cognition]
# setwd(paste0(workpath))
library(ggridges)
##---------------------------------------------------------------------------
# plot (density) ------------------------
input_data <- data.GNHS
input_data$ml_subtype <- factor(input_data$ml_subtype, levels = seq(0,2,1))


plot <- ggplot(input_data, aes(x = chronological.age, y = ml_subtype, fill = ml_subtype)) +
  geom_density_ridges_gradient(scale=0.8, rel_min_height = 0) + 
  
  scale_fill_manual(values = c(OKeeffe1[9], OKeeffe2[c(2,6)]))+  
  scale_y_discrete(expand = c(0,0))+
  scale_x_continuous(expand = c(0,0), limits = c(55,85), breaks = seq(60,80,10)) +
  labs(tag="A", x = "Chronological age distribution", y = NULL, title = "GNHS-2008 cohort") +
  
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black") 
  ) 
plot


# plot (box) -----------------------
input_data <- data.GNHS
input_data$ml_subtype <- factor(input_data$ml_subtype, levels = seq(0,2,1))


plot2 <- ggplot(data=input_data, aes(x=ml_subtype, y=cognitive, fill=ml_subtype))+  
  # geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5, color="grey85")+
  stat_boxplot(geom = "errorbar", width=0.3, position = position_dodge(0.4)) +
  geom_boxplot(width=0.6, position = position_dodge(0.4), color="black", alpha=0.5, outlier.alpha = 0)+
  geom_point(aes(fill=ml_subtype), shape=21, color="transparent", alpha=1, size=4, 
             position=position_jitterdodge(jitter.width=0.8, dodge.width=0.4, jitter.height = 0.1))+
  
  
  scale_y_continuous(expand = c(0,0), limits = c(-5,4.5), breaks = seq(-5,4.5,2))+
  scale_fill_manual(values = c(OKeeffe1[9], OKeeffe2[c(2,6)]))+ 
  labs(fill=NULL, x=NULL, y="Cognitive assessment", title = NULL)+  
  
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=20, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot2


# merge ----------------------
library(cowplot)
plot_merge <- cowplot::plot_grid(plot, plot2, ncol = 1, 
                                 rel_heights = c(1,1), rel_widths = c(0.5,1)); plot_merge




#' [Prediction performance, brain age]
setwd(paste0(workpath))
library(ggExtra)
##---------------------------------------------------------------------------
# data --------------------------


stat <- cor.test(data.GNHS2008$brain.predicted_age, data.GNHS2008$chronological.age, type = "pearson")
MAE <- mean(abs(data.GNHS2008$brain.predicted_age-data.GNHS2008$chronological.age))
tag.2008 <- paste0("MAE = ", round(MAE,2), "\nPearson r = ", round(stat$estimate, 2))

stat <- cor.test(data.GNHS2010$brain.predicted_age, data.GNHS2010$chronological.age, type = "pearson")
MAE <- mean(abs(data.GNHS2010$brain.predicted_age-data.GNHS2010$chronological.age))
tag.2010 <- paste0("MAE = ", round(MAE,2), "\nPearson r = ", round(stat$estimate, 2))

#' @:merge
database <- rbind(data.frame(group="GNHS-2008", data.GNHS2008),
                  data.frame(group="GNHS-2010", data.GNHS2010))

# plot --------------------------
input_data <- database 

plot <- ggplot(data=input_data, aes(x=chronological.age, y=brain.predicted_age))+  
  geom_point(aes(fill=group), shape=21, color="black", alpha=0.85, size=5)+
  geom_smooth(method = "lm", se=T, color="black", fill="grey70", alpha=0.5, linewidth=0.5)+
  
  annotate("text", label=tag.2008, x=77, y=55, size=5, color="black", hjust=0)+
  annotate("text", label=tag.2010, x=77, y=45, size=5, color="black", hjust=0)+
  scale_x_continuous(expand = c(0,0), limits = c(50,90))+
  scale_y_continuous(expand = c(0,0))+ 
  scale_fill_manual(values = c("#94D4FF","#E78D9C"))+ 
  labs(tag = "C", fill=NULL, x="Chronological age", y="Predicted brain age ", title = NULL)+  
  
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = c(0.1,0.8),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot


#' @:add [Distribution]
#'  groupColour = TRUE, groupFill = TRUE
plot2 <- ggMarginal(plot, type = "densigram", groupFill = T)
plot2



#' [Age-gap validation and distribution]
setwd(paste0(workpath))
##---------------------------------------------------------------------------
# data -----------------------
data_long <- openxlsx::read.xlsx("distribution_plot_data.xlsx")
data_long$color <- as.character(data_long$group)
data_long$color[data_long$group=="1"] <- as.character(data_long$disease[data_long$group==1])
data_long$color[data_long$color=="0"] <- 'Control'
table(data_long$color)

data_long$color <- factor(data_long$color,levels = c('Control',"Hypertension","T2D","Dyslipidemia","MCI"))
res <- data.frame()
for (i in c('Control',"Hypertension","T2D","Dyslipidemia","MCI")) {
  
  disease <- data_long[data_long$disease==i,]
  CN <- disease[disease$color=='Control',]
  case  <- disease[disease$color==i,]
  
  sig <- wilcox.test(CN$age_gap,case$age_gap)
  res <- rbind.fill(res, data.frame(disease =i, pval=sig$p.value))
}


# plot -----------------------
input_data <- data_long
input_data$cohort <- ifelse(input_data$disease %in% c('Control',"Hypertension","T2D","Dyslipidemia","MCI")input_data$cohort[input_data$disease=="PD"] <- "PPMI"
input_data$disease <- factor(input_data$disease, levels = rev(c('Control',"Hypertension","T2D","Dyslipidemia","MCI")))

input_stat <- res
input_stat$tag <- paste0("P = ", ifelse(input_stat$pval<0.001, format(input_stat$pval, scientific = T, digits = 2), round(input_stat$pval,3)))


plot <- ggplot(input_data, aes(x = age_gap, y = disease)) + 
  geom_density_ridges_gradient(aes(fill = color), scale=0.8) +
  geom_text(data=input_stat, aes(y=disease, x=-40, label=tag), vjust=-1, size=5)+
  
  # scale_fill_manual(values = c('grey',Isfahan2,NewKingdom), name='Disease') +  
  scale_fill_manual(values = c('grey85',Manet[-c(1,8)]), name='Disease') +
  scale_x_continuous(expand=c(0,0)) +
  scale_y_discrete(expand = expand_scale(mult = c(0, 0))) +
  labs(tag="D", x = 'Age gap distribution', y = NULL) +
  # facet_grid(cohort~., scales = "free_y", space = "free_y")+ 
  
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(10,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "right",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(1,"cm"),
    legend.key.height = unit(1,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_blank(), axis.ticks.x = element_blank(),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot


 

#'[Age gap , GNHS]
setwd(paste0(workpath)) 
##---------------------------------------------------------------------------
# plot -----------------------
input_data <- data.GNHS
input_data$ml_subtype <- factor(input_data$ml_subtype, levels = seq(0,2,1))


plot <- ggplot(data=input_data, aes(x=ml_subtype, y=age_gap, fill=ml_subtype))+  
  # geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5, color="grey85")+
  stat_boxplot(geom = "errorbar", width=0.3, position = position_dodge(0.4)) +
  geom_boxplot(width=0.6, position = position_dodge(0.4), color="black", alpha=0.5, outlier.alpha = 0)+
  geom_point(aes(fill=ml_subtype), shape=21, color="transparent", alpha=1, size=4, 
             position=position_jitterdodge(jitter.width=0.8, dodge.width=0.4, jitter.height = 0.1))+
  
  
  # scale_y_continuous(expand = c(0,0), limits = c(55,115), breaks = seq(60,100,20))+
  scale_fill_manual(values = c(OKeeffe1[9], OKeeffe2[c(2,6)]))+ 
  labs(tag = "E", fill=NULL, x=NULL, y="Brain age gap", title = "GNHS cohort")+  
  
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=20, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot



#' [External validation, cognitive score, PPMI]
size_point <- 5
##---------------------------------------------------------------------------
# data -------------------
data_PPMI$cognitive <- scale(data_PPMI$cognitive)
data_PPMI$group <- factor(data_PPMI$ml_subtype, levels = seq(0,2,1))


# plot -------------------
input_data <- data_PPMI

plot <- ggplot(data=input_data, aes(x=group, y=cognitive, fill=group))+  
  # geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5, color="grey85")+
  stat_boxplot(geom = "errorbar", width=0.3, position = position_dodge(0.4)) +
  geom_boxplot(width=0.6, position = position_dodge(0.4), color="black", alpha=0.5, outlier.alpha = 0)+
  geom_point(aes(fill=group), shape=21, color="transparent", alpha=1, size=size_point, 
             position=position_jitterdodge(jitter.width=0.8, dodge.width=0.4, jitter.height = 0.1))+
  
  
  scale_y_continuous(expand = c(0,0), limits = c(-5,4.5), breaks = seq(-5,4.5,2))+ 
  scale_fill_manual(values = c(OKeeffe1[9], OKeeffe2[c(2,6)]))+ 
  labs(tag = "A", fill=NULL, x=NULL, y="Cognitive assessment", title = "PPMI cohort")+  
  
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=20, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot




#' [External validation, Age Gap, PPMI]
##---------------------------------------------------------------------------
# data -------------------

data_PPMI$group <- factor(data_PPMI$ml_subtype, levels = seq(0,2,1))

# plot -------------------
input_data <- data_PPMI

plot <- ggplot(data=input_data, aes(x=group, y=age_gap, fill=group))+  
  # geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5, color="grey85")+
  stat_boxplot(geom = "errorbar", width=0.3, position = position_dodge(0.4)) +
  geom_boxplot(width=0.6, position = position_dodge(0.4), color="black", alpha=0.5, outlier.alpha = 0)+
  geom_point(aes(fill=group), shape=21, color="transparent", alpha=1, size=size_point, 
             position=position_jitterdodge(jitter.width=0.8, dodge.width=0.4, jitter.height = 0.1))+
  
  
  # scale_y_continuous(expand = c(0,0), limits = c(23,33), breaks = seq(22,30,2))+ 
  scale_fill_manual(values = c(OKeeffe1[9], OKeeffe2[c(2,6)]))+ 
  labs(tag = "B", fill=NULL, x=NULL, y="Age gap", title = "PPMI cohort")+  
  
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=20, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot


#'  [External validation, Age, PPMI]
##---------------------------------------------------------------------------
# data -------------------
data_PPMI$group <- factor(data_PPMI$ml_subtype, levels = seq(0,2,1))

# plot -------------------
input_data <- data_PPMI

plot <- ggplot(data=input_data, aes(x=group, y=Age, fill=group))+  
  # geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5, color="grey85")+
  stat_boxplot(geom = "errorbar", width=0.3, position = position_dodge(0.4)) +
  geom_boxplot(width=0.6, position = position_dodge(0.4), color="black", alpha=0.5, outlier.alpha = 0)+
  geom_point(aes(fill=group), shape=21, color="transparent", alpha=1, size=size_point, 
             position=position_jitterdodge(jitter.width=0.8, dodge.width=0.4, jitter.height = 0.1))+

  
  # scale_y_continuous(expand = c(0,0), limits = c(23,33), breaks = seq(22,30,2))+ 
  scale_fill_manual(values = c(OKeeffe1[9], OKeeffe2[c(2,6)]))+ 
  labs(tag = "C", fill=NULL, x=NULL, y="Chronologic age", title = "PPMI cohort")+  
  
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=20, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot





#' [External validation, cognitive score, ADNI]
##---------------------------------------------------------------------------
# data -------------------
data_ADNI <- data_ADNI[!is.na(data_ADNI$ml_subtype),]
data_ADNI$group <- factor(data_ADNI$ml_subtype, levels = seq(0,2,1))
data_ADNI$cognitive <- scale(data_ADNI$cognitive)

# plot -------------------
input_data <- data_ADNI

plot <- ggplot(data=input_data, aes(x=group, y=cognitive, fill=group))+  
  # geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5, color="grey85")+
  stat_boxplot(geom = "errorbar", width=0.3, position = position_dodge(0.4)) +
  geom_boxplot(width=0.6, position = position_dodge(0.4), color="black", alpha=0.5, outlier.alpha = 0)+
  geom_point(aes(fill=group), shape=21, color="transparent", alpha=1, size=size_point, 
             position=position_jitterdodge(jitter.width=0.8, dodge.width=0.4, jitter.height = 0.1))+
  
  
  # scale_y_continuous(expand = c(0,0), limits = c(23,33), breaks = seq(22,30,2))+ 
  scale_fill_manual(values = c(OKeeffe1[9], OKeeffe2[c(2,6)]))+ 
  labs(tag = "D", fill=NULL, x=NULL, y="Cognitive assessment", title = "ADNI cohort")+  
  scale_y_continuous(expand = c(0,0), limits = c(-5,4.5), breaks = seq(-5,4.5,2))+ 
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=20, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot


#' [External validation, Age Gap, ADNI]
##---------------------------------------------------------------------------
# data -------------------
data_ADNI <- data_ADNI[!is.na(data_ADNI$ml_subtype),]
data_ADNI$group <- factor(data_ADNI$ml_subtype, levels = seq(0,2,1))


# plot -------------------
input_data <- data_ADNI

plot <- ggplot(data=input_data, aes(x=group, y=age_gap, fill=group))+  
  # geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5, color="grey85")+
  stat_boxplot(geom = "errorbar", width=0.3, position = position_dodge(0.4)) +
  geom_boxplot(width=0.6, position = position_dodge(0.4), color="black", alpha=0.5, outlier.alpha = 0)+
  geom_point(aes(fill=group), shape=21, color="transparent", alpha=1, size=size_point, 
             position=position_jitterdodge(jitter.width=0.8, dodge.width=0.4, jitter.height = 0.1))+
  
  
  # scale_y_continuous(expand = c(0,0), limits = c(23,33), breaks = seq(22,30,2))+ 
  scale_fill_manual(values = c(OKeeffe1[9], OKeeffe2[c(2,6)]))+ 
  labs(tag = "E", fill=NULL, x=NULL, y="Age gap", title = "ADNI cohort")+  
  
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=20, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot





#' [External validation, Age, ADNI]
##---------------------------------------------------------------------------
# data -------------------
data_ADNI <- data_ADNI[!is.na(data_ADNI$ml_subtype),]
data_ADNI$group <- factor(data_ADNI$ml_subtype, levels = seq(0,2,1))


# plot -------------------
input_data <- data_ADNI

plot <- ggplot(data=input_data, aes(x=group, y=Age, fill=group))+  
  # geom_vline(xintercept = 1.5, linetype="dashed", linewidth=0.5, color="grey85")+
  stat_boxplot(geom = "errorbar", width=0.3, position = position_dodge(0.4)) +
  geom_boxplot(width=0.6, position = position_dodge(0.4), color="black", alpha=0.5, outlier.alpha = 0)+
  geom_point(aes(fill=group), shape=21, color="transparent", alpha=1, size=size_point, 
             position=position_jitterdodge(jitter.width=0.8, dodge.width=0.4, jitter.height = 0.1))+
  
  stat_compare_means( 
    comparisons = list(c(1,2), c(1,3), c(2,3)), #
    method = "t.test", paired = FALSE,
    label="p.format", #p.format p.signif 表示*
    # label.y = c(0.25,0.3,0.35),
    # ref.group = "LPS",
    size = 6,
    hide.ns = T
  )+
  
  # scale_y_continuous(expand = c(0,0), limits = c(23,33), breaks = seq(22,30,2))+ 
  scale_fill_manual(values = c(OKeeffe1[9], OKeeffe2[c(2,6)]))+ 
  labs(tag = "F", fill=NULL, x=NULL, y="Chronologic age", title = "ADNI cohort")+  
  
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=20),
    axis.title.y = element_text(color = "black",size=20),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=20, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot



#' [External validation, brain subtypes, Polar chart] 
library(scales)
##---------------------------------------------------------------------------
# data -------------------

code <- openxlsx::read.xlsx("/Biomarker_codebook.xlsx")
names(code) <- c("DK","name","roi")
code$name <- paste0(toupper(substr(code$name,1,1)), substr(code$name,2,str_length(code$name)))

data_radar.GNHS <- subset(data_merge,cohort=='GNHS')
data_radar.ADNI <- subset(data_merge,cohort=='ADNI')


#' @:merge
list <- colnames(data_radar.GNHS2008)[grep("Biomarker", colnames(data_radar.GNHS2008))]

data_radar.merge <- rbind(data.frame(cohort="GNHS", data_radar.GNHS2008[,c("ml_subtype",list)]),
                          data.frame(cohort="ADNI", data_radar.ADNI[,c("ml_subtype",list)]))
data_radar.merge <- data_radar.merge[data_radar.merge$ml_subtype==0,]


#' @:mean
data_radar.mean <- aggregate(data_radar.merge[,list], by=list(data_radar.merge$cohort),mean,na.rm=T)
names(data_radar.mean)[1] <- "cohort"
data_radar.mean$cohort <- as.character(data_radar.mean$cohort)

data_radar.rank <- data_radar.mean
data_radar.rank[,-1] <- apply(data_radar.rank[,-1],1,rank)


#' @:reshape
data_radar.mean2 <- reshape2::melt(data_radar.mean)
data_radar.mean2 <- merge(data_radar.mean2, code[,c("name","roi")], by.x="variable", by.y = "roi", all.x = T)


# plot (polar) ---------------------- 
input_data <- data_radar.mean2 
input_data <- input_data[order(input_data$cohort, decreasing = T),]

a <- data.frame(input_data[input_data$cohort=="GNHS",], tag=seq(1,nrow(input_data[input_data$cohort=="GNHS",]),1))
input_data <- merge(input_data, a[,c("name","tag")], by="name", all.x=T) 
input_data$cohort <- factor(input_data$cohort, levels = c("GNHS", "ADNI"))


#' @:increase-gray
increase_gray <- function(color, factor = 0.7) {
  rgb_values <- col2rgb(color)
  gray_values <- rgb_values * factor + 255 * (1 - factor)
  rgb(t(gray_values) / 255)
}
muted_Archambault <- sapply(Archambault, increase_gray)


#' @:plot
plot1 <- ggplot(data = input_data, aes(x=tag, y=value)) +
  geom_col(aes(tag, value, fill = tag), color="grey40", width = 1, linewidth=0.2, alpha=1) +  
  
  
  # scale_fill_continuous_sequential(palette = "Hawaii", begin=0, end=1, rev = T)+
  # scale_fill_continuous_sequential(palette = "Terrain")+
  scale_fill_gradientn(colours = muted_Archambault)+
  labs(tag="G", title = "Validation of subtype")+
  
  coord_polar(clip = "off") +
  facet_wrap(.~cohort, ncol = 2)+
  
  theme_minimal()+
  theme( 
    # for facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    # panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title = element_blank(), 
    axis.text = element_blank()
  ) 
plot1


# plot (legend) ---------------------
input_data <- data_radar.mean2 
input_data <- input_data[order(input_data$cohort, decreasing = T),]
a <- data.frame(input_data[input_data$cohort=="GNHS",], tag=seq(1,nrow(input_data[input_data$cohort=="GNHS",]),1))

input_data <- merge(input_data, a[,c("name","tag")], by="name", all.x=T) 
input_data <- input_data[input_data$cohort=="GNHS",]
input_data <- input_data[order(input_data$tag, decreasing = F),]

input_data$name <- factor(input_data$name, levels = unique(input_data$name))
input_data$cohort <- factor(input_data$cohort, levels = c("GNHS", "ADNI"))
input_data$value <- 1
input_data$angle <- rep(c(rev(seq(0,90,90/5)),rev(seq(-90,0,90/5))),2)


plot2 <- ggplot(data = input_data, aes(x=name, y=value)) +
  geom_col(aes(name, value, fill = tag), color="grey40", width = 1, linewidth=0.2) +  
  
  # scale_x_continuous(breaks = seq(1,24,1))+ 
  scale_fill_gradientn(colours = muted_Archambault)+
  labs(title = NULL)+ 
  coord_polar(clip = "off") + 
  
  theme_minimal()+
  theme( 
    # for facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title = element_blank(), 
    axis.text.x = element_text(angle = input_data$angle, hjust=1, vjust=1, size=20, color="black"),
    axis.text.y = element_blank() 
  ) 
plot2



#' [ROI vs subtypes, SuStain]
##---------------------------------------------------------------------------
# data ----------------------- 

stat_merge <- rbind(data.frame(group="0vs1", stat_01), data.frame(group="0vs2", stat_02))
names(stat_merge) <- c("group","tag","name","beta","pval","qval")

stat_merge <- stat_merge[order(stat_merge$beta, decreasing = F),]
stat_merge$name <- paste0(toupper(substr(stat_merge$name,1,1)),substr(stat_merge$name,2,str_length(stat_merge$name)))


# plot -----------------------
input_data <- stat_merge[stat_merge$qval<0.05,]
input_data$group <- factor(input_data$group, levels = c("0vs1","0vs2"))


plot <- ggplot(data = input_data, aes(x = group, y=name)) + 
  geom_point(aes(color=qval), size=5)+ 
   
  # scale_color_gradientn(colours = rev(c(VanGogh3[c(1:5)])), values = c(0, 0.5, 1)) +
  scale_color_gradientn(colours = rev(c(rev(OKeeffe1[c(7:10)]), OKeeffe2[c(1,2,6)])), values = c(0, 0.5, 1)) +
  
  labs(tag = "A", x=NULL, y=NULL, title = "Divergent atrophy trajectory in ROIs", color="Adjusted-P") +
  facet_grid(.~group, scales = "free", space = "free")+
   
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "bottom",
    legend.title = element_text(size = 12, vjust = 0.8, hjust = 0.5),
    legend.text = element_text(size = 12),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(15,"mm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15),
    axis.title.y = element_text(color = "black",size=15),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=12, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=12, color="black") 
  )   
plot


#' [Atrophy trajectory, Sustain]
##---------------------------------------------------------------------------
# data ------------------------
code <- openxlsx::read.xlsx("Biomarker_codebook.xlsx")
names(code) <- c("DK","name","ROI")
database <- rbind(data.frame(group="subtype0", data_sub0),
                  data.frame(group="subtype1", data_sub1),
                  data.frame(group="subtype2", data_sub2))
names(database) <- gsub("X","", colnames(database))
names(database)[2] <- "ROI"
database <- merge(database, code[,c("name","ROI")], by="ROI", all.x=T)


setwd(paste0(workpath,"/compare_subtype x stage"))
stat <- read.csv("res01.csv")
names(stat) <- c("label","name","beta","pval")
stat$tag <- paste0("P for stage*subtype = ", ifelse(stat$pval<0.001, format(stat$pval, scientific = T, digits = 2), round(stat$pval,3)))


# plot (hippocampus) ------------------------
#' @:Stat
input_stat <- stat[grep("^hippo", stat$name),]

#' @:value
input_data <- database[grep("^hippo", database$name),]
input_data <- input_data[input_data$group!="subtype2",]
input_data <- reshape2::melt(input_data)  
input_data <- merge(input_data, input_stat[,c("name","tag")], by="name", all=F)

input_data$group <- factor(input_data$group, levels = c("subtype0","subtype1"))
input_data$name <- factor(input_data$name, levels = c("hippocampus_L","hippocampus_R"), labels = c("Hippocampus_L","Hippocampus_R"))


#' @:plot
plot <- ggplot(data = input_data, mapping = aes(x = variable, y=value)) +
  stat_smooth(aes(group=group, color=group), method="loess", se=F, span=0.5) +
  geom_point(aes(x = variable, y=value, group=group, color=group), size=4)+
  geom_text(aes(label=tag), x=5.5, y=0.2, inherit.aes = F, color="black", size=5)+
  
  scale_color_manual(values = c(OKeeffe1[9], OKeeffe2[c(2,6)]))+ 
  labs(tag = "A", x="SuStain stage", y="Atrophy value", title = "Divergent atrophy trajectory", color=NULL) +
  facet_grid(name~.)+
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = c(0.15,0.9),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15),
    axis.title.y = element_text(color = "black",size=15),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=12, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=12, color="black") 
  )  

plot




#' [Disease & Brain subtypes]
##---------------------------------------------------------------------------
# data -------------------
data <- openxlsx::read.xlsx("disease_logistic_reg_merge_all_new.xlsx")
data <- data[order(data$CI5, decreasing = T),]


# plot -------------------
input_data <- data
input_data$Disease <- factor(input_data$Disease, levels = rev(unique(input_data$Disease)))


plot <- ggplot(data=input_data, aes(x=OR, y=Disease))+  
  geom_vline(xintercept = 1, linewidth=0.5, color="black")+
  geom_errorbar(aes(xmin=CI5, xmax=CI95, color=term), width=.0)+  
  geom_point(aes(color=term), alpha=1, size=4)+
  
  scale_x_continuous(expand = c(0.1,0.1), transform = "log",
                     labels = scales::label_number(accuracy = 0.01))+
  scale_color_manual(values = c(OKeeffe1[9], OKeeffe2[c(6)],"grey80"))+ 
  labs(tag = "B", fill=NULL, x="Odds ratio (log-transformed)", y=NULL, title = NULL)+  
  facet_grid(.~term)+
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15),
    axis.title.y = element_text(color = "black",size=15),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=12, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot


#'  [Traits & Brain subtypes]

##---------------------------------------------------------------------------
# data -------------------
data <- read.csv("traits_linear_reg_result.csv") 
data <- data[order(data$beta, decreasing = F),]


# plot -------------------
input_data <- data 
input_data$pheno <- factor(input_data$pheno, levels = unique(input_data$pheno))


plot <- ggplot(data=input_data, aes(x=beta, y=pheno, fill=subtype))+  
  geom_vline(xintercept = 0, linewidth=0.5, color="black")+
  geom_errorbar(aes(xmin=CI5, xmax=CI95, color=subtype), width=.0)+  
  geom_point(aes(color=subtype), alpha=1, size=4)+
  
  scale_x_continuous(expand = c(0.1,0.1),
                     labels = scales::label_number(accuracy = 0.1))+
  scale_color_manual(values = c(OKeeffe1[9], OKeeffe2[c(6)]))+ 
  labs(tag = "C", fill=NULL, x="Beta coefficient", y=NULL, title = NULL)+  
  facet_grid(.~subtype)+
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 20, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15),
    axis.title.y = element_text(color = "black",size=15),
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=12, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot



#'  [Brain & Protein] 
##---------------------------------------------------------------------------
# data ------------------------

database <- data_all
database$direction <- ifelse(database$estimate>0,"Positive","Negative")
table(database$brain, database$cluster)

database$label <- substr(database$brain,4,str_length(database$brain))
database$label <- gsub("_subcortex/brainstem","",database$label)
database$label <- gsub("_cortex","",database$label)
database$label <- paste0(toupper(substr(database$label,1,1)), substr(database$label,2,str_length(database$label)))

database$hemisphere <- ifelse(grepl("_L",database$label),"Left","Right")
database$cluster[database$cluster=="brainstem"] <- "Brainstem"

database <- merge(database, code, by.x = "PRO", by.y = "Uniprot", all.x = T)


#' @:sort
a <- database[database$Q_value<0.05,]
data_sort <- data.frame(table(a$direction, a$label))
data_sort <- data_sort[order(data_sort$Freq, decreasing = T),]
data_sort$Freq[data_sort$Var1=="Negative"] <- -1*data_sort$Freq[data_sort$Var1=="Negative"]

a <- database[!duplicated(database$label), c("label","cluster")]
data_sort <- merge(data_sort, a, by.x = "Var2", by.y = "label", all.x=T)

# plot --------------------------
input_data <- data_sort

plot1 <- ggplot(data = input_data, aes(x= Var2, y= Freq, fill=Var1))+ 
  geom_bar(stat="identity", color="white", width=1) + 
   
  scale_fill_manual(values = c("#94D4FF","#E78D9C"))+ 
  scale_x_discrete(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0), limits = c(-8,10), breaks = seq(-8,10,2), labels = abs(seq(-8,10,2)))+
  labs(x=NULL, y="N. of significantly associated protein", fill="Direction", title="sserum proteins and cognition-related brain ROIs")+
  
  facet_grid(.~cluster, scales = "free", space = "free")+
  theme_bw()+
  theme(
    # for facet
    strip.text.x = element_text(size = 15, face = "plain", angle = 0),
    strip.text.y = element_text(size = 15, face = "plain", angle = -90),
    strip.background = element_rect(color = "black", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, face = "plain", hjust = 0.5),
    plot.background = element_rect(fill = "transparent", color = "transparent"),
    
    panel.grid.major = element_line(color = "transparent", linewidth = 0.3),
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    panel.grid.minor = element_blank(),
    
    # legend
    legend.position = c(0.2,0.1),
    legend.title = element_text(face = "plain"),
    legend.text = element_text(size=12),
    legend.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(face = "plain", color = "black"),
    axis.title.y = element_text(face = "plain", color = "black"),
    axis.title = element_text(size=15),
    
    axis.text.y = element_text(angle=0, face = "plain", hjust=1, vjust=0.5, color = "black",size=12), 
    axis.text.x = element_text(angle=45, face = "plain", hjust=1, vjust=1, color = "black",size=15)
  )
plot1



#' [Differential protein, subgroup comparison, 0 vs 1]
##------------------------------------------------------------------
# data -----------------

palette<- c(brewer.pal(7,"Set2")[c(1,2,3,4,5)])

# plot (0vs1) ------------------
input_data <- data_0vs1

plot_0vs1 <-  ggplot(input_data, aes(OR, -log10(p_value),color=OR))+
  annotate(geom = "rect", xmin = 0.6, xmax = 1, ymin=0, ymax=3.5, fill ="#66c2a5", alpha=0.05)+
  annotate(geom = "rect", xmin = 1, xmax = 1.4, ymin=0, ymax=3.5, fill ="#f46d43", alpha=0.05)+ 
  
  geom_hline(yintercept= -log10(0.05), linetype="dashed", color = "#8E0000")+
  # geom_hline(yintercept= -log10(0.001), linetype="dashed", color = "#184EC6")+
  geom_vline(xintercept=1, linetype="dashed", color = "grey50")+
  
  geom_point(aes(size = -log10(p_value)), alpha = 0.5)+ 
  geom_text_repel(aes(label = label), size = 5, max.overlaps = 25)+
  
  scale_y_continuous(expand = c(0,0), limits = c(0,3.5))+
  scale_x_continuous(expand = c(0,0), limits = c(0.6, 1.4), breaks = seq(0.6,1.4,0.2), labels = seq(0.6,1.4,0.2))+ # 自定义X轴刻度的标签
  scale_color_gradientn(colours = c("#3288bd", "#66c2a5","#ffffbf", "#f46d43", "#9e0142"), values = seq(0, 1, 0.2)) +
  scale_size_continuous(range = c(0.1,10))+
  scale_shape_discrete(name = NULL)+ 
  
  labs(x="Odds ratio (OR)", y="-log10 (P-value)")+
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15),
    axis.title.y = element_text(color = "black",size=15),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=12, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=12, color="black") 
  )  
plot_0vs1



#' [Gene ontology enrichment analysis] 

# data ----------------- 

data$Description <- paste0(toupper(substr(data$Description,1,1)), substr(data$Description,2,str_length(data$Description)))
data <- data[order(data$p.adjust, decreasing = F),]

# plot ------------------
input_data <- data
input_data$Description <- factor(input_data$Description, levels = rev(input_data$Description))

plot <-  ggplot(input_data, aes(x=Count, y=Description))+  
  geom_bar(aes(fill=-log10(p.adjust)), stat="identity", position=position_dodge(), color="black", width=0.7) + 
  
  scale_fill_gradientn(colours = rev(c("#3288bd", "#66c2a5","#FFF0DC")), values = seq(0, 1, 0.2)) +
  scale_x_continuous(expand = c(0,0))+
  scale_y_discrete(expand = c(0,0))+
  
  labs(x="Counts", y=NULL, title = "Gene ontology enrichment", fill="-log10\n(Adjusted-P)")+
  guides(fill = guide_colorbar(
    title.position = "top", title.hjust = 0.5,
    barheight = unit(6, "cm"), barwidth = unit(0.5, "cm"),
    ticks.colour = "black", frame.colour = "black"
  ))+
  
  theme_classic2()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "right",
    legend.title = element_text(size = 15, hjust = 0.5),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.height = unit(15,"mm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15),
    axis.title.y = element_text(color = "black",size=15),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=12, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=12, color="black") 
  )  
plot



#' [Marker gene in hippocampus] 

##------------------------------------------------------------------
# data ----------------- 
data.filter <- data[1:3,]
data.filter$pval <- 10^(-1*data.filter$log10p_nm)
data.filter$sig_label <- paste0("P = ", ifelse(data.filter$pval<0.001, 
                                               format(data.filter$pval, digits = 2, scientific = T), 
                                               round(data.filter$pval,3))) 

# plot ------------------
input_data <- data.filter 
input_data$gene <- factor(input_data$gene, levels = c("PZP","A2M","GSN"))

plot <-  ggplot(input_data, aes(y=logFC_nb, x=gene))+   
  geom_hline(yintercept = 0, linewidth=0.5, color="lightgrey", linetype="dashed")+
  geom_bar(stat="identity", position=position_dodge(), fill="black", width=0.01)+
  geom_point(aes(size=log10p_nm, fill=gene), shape=21, color="black")+
  geom_text(aes(label=sig_label))+
  
  scale_fill_manual(values = c(OKeeffe1[c(7:9)]), guide="none")+ 
  scale_y_continuous(limits = c(-0.6,0.2))+ 
  scale_size_continuous(range = c(3,10))+
  
  labs(y="logFC", x=NULL, title = "Marker gene expression\nin hippocampus", size="-log10(P)")+ 
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = c(0.7,0.25),
    legend.title = element_text(size = 15, hjust = 0.5),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"), 
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15),
    axis.title.y = element_text(color = "black",size=15),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=12, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=12, color="black") 
  )  
plot




#' [Associations of hippocampus volume and protein expression] 
##------------------------------------------------------------------
# data ----------------- 
data$sig_label <- paste0("P = ", ifelse(data$P.value<0.001, format(data$P.value, digits = 2, scientific = T), round(data$P.value,3)))
colnames(data)

# plot ------------------
input_data <- data
input_data$Protein <- factor(input_data$Protein, levels = c("GSN","A2M","PZP"))

plot <-  ggplot(input_data, aes(x=Protein, y=Coef))+  
  geom_hline(yintercept = 0, linewidth=0.5, color="lightgrey", linetype="dashed")+
  geom_bar(aes(fill=Brain), stat="identity", position=position_dodge(), color="black", width=0.7) + 
  geom_text(aes(label=sig_label, y=Coef*1.1, x=Protein), color="black", size=4)+
   
  scale_x_discrete(expand = c(0.2,0))+
  scale_y_continuous(expand = c(0,0), limits = c(-0.1,0.1))+
  scale_fill_manual(values = rev(c(OKeeffe1[c(7,9)]))) +  
  labs(x=NULL, y="Spearman correlation", title = NULL, fill=NULL)+ 
  
  theme_classic2()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "top",
    legend.title = element_text(size = 15, hjust = 0.5),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"), 
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15),
    axis.title.y = element_text(color = "black",size=15),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=12, color="black") 
  )  
plot



#' [Prediction of hippocampus volume]  
library(pROC)
library(plotROC)
##---------------------------------------------------------------------------
# data ------------------------
data_basic <- read.csv("basic_model_hippocampus_L.csv")
data_full <- read.csv("full_model_hippocampus_L.csv")

#' @:AUC
mod1 <- pROC::roc(response = data_basic$actuals.test, predictor = data_basic$predCV.test) 
auc_ci.mod1 <- data.frame(t(data.frame(ci(mod1, of="auc", thresholds="best"))))
names(auc_ci.mod1) <- c("lci","auc","uci")
label1 <- paste0("AUC of basic model: ", round(auc_ci.mod1$auc,3), " [", round(auc_ci.mod1$lci,2), ",", round(auc_ci.mod1$uci,2),"]")

mod2 <- pROC::roc(response = data_full$actuals.test, predictor = data_full$predCV.test)
auc_ci.mod2 <- data.frame(t(data.frame(ci(mod2, of="auc", thresholds="best"))))
names(auc_ci.mod2) <- c("lci","auc","uci")
label2 <- paste0("AUC of full model: ", round(auc_ci.mod2$auc,3), " [", round(auc_ci.mod2$lci,2), ",", round(auc_ci.mod2$uci,2),"]")

#' @:stat
set.seed(123456)
stat <- pROC::roc.test(mod2, mod1, method = "venkatraman", boot.n=1000)
label3 <- paste0("P value = ", round(stat$p.value,3))

#' @:merge
label <- paste0(label1, "\n", label2, "\n", label3)


# plot ------------------------
input_data <- rbind(data.frame(mod="basic", data_basic),
                    data.frame(mod="full", data_full))

plot <- ggplot(input_data, aes(d=actuals.test, m=predCV.test, color=mod))+
  
  geom_roc(n.cuts = 0)+
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="grey80", linetype="dashed", linewidth=0.3)+
  ggsci::scale_color_nejm()+
  
  annotate("rect", xmin = 0.23, xmax = 0.27, ymin = 0.20, ymax = 0.22, alpha = 1, fill="#125D98")+
  annotate("rect", xmin = 0.23, xmax = 0.27, ymin = 0.27, ymax = 0.29, alpha = 1, fill="#DF2E38")+
  annotate("text", x = 0.3, y = .15, label = label, vjust=0, hjust=0, fontface="plain", size = 4, angle=0) + 
  
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+ 
  ggtitle(label="Atrophy of left hippocampus")+
  labs(tag = "C", x="1 - Specificity", y="Sensitivity")+
  
  theme_bw()+
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15),
    axis.title.y = element_text(color = "black",size=15),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=12, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=12, color="black") 
  )  
plot


#'  [Prediction of hippocampus volume, right]  
library(pROC)
library(plotROC)
##---------------------------------------------------------------------------
# data ------------------------
data_basic <- read.csv("basic_model_hippocampus_R.csv")
data_full <- read.csv("full_model_hippocampus_R.csv")

#' @:AUC
mod1 <- pROC::roc(response = data_basic$actuals.test, predictor = data_basic$predCV.test) 
auc_ci.mod1 <- data.frame(t(data.frame(ci(mod1, of="auc", thresholds="best"))))
names(auc_ci.mod1) <- c("lci","auc","uci")
label1 <- paste0("AUC of basic model: ", round(auc_ci.mod1$auc,3), " [", round(auc_ci.mod1$lci,2), ",", round(auc_ci.mod1$uci,2),"]")

mod2 <- pROC::roc(response = data_full$actuals.test, predictor = data_full$predCV.test)
auc_ci.mod2 <- data.frame(t(data.frame(ci(mod2, of="auc", thresholds="best"))))
names(auc_ci.mod2) <- c("lci","auc","uci")
label2 <- paste0("AUC of full model: ", round(auc_ci.mod2$auc,3), " [", round(auc_ci.mod2$lci,2), ",", round(auc_ci.mod2$uci,2),"]")

#' @:stat
set.seed(123456)
stat <- pROC::roc.test(mod2, mod1, method = "venkatraman", boot.n=1000)
label3 <- paste0("P value = ", round(stat$p.value,3))

#' @:merge
label <- paste0(label1, "\n", label2, "\n", label3)


# plot ------------------------
input_data <- rbind(data.frame(mod="basic", data_basic),
                    data.frame(mod="full", data_full))

plot <- ggplot(input_data, aes(d=actuals.test, m=predCV.test, color=mod))+
  
  geom_roc(n.cuts = 0)+
  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1), color="grey80", linetype="dashed", linewidth=0.3)+
  ggsci::scale_color_nejm()+
  
  annotate("rect", xmin = 0.23, xmax = 0.27, ymin = 0.20, ymax = 0.22, alpha = 1, fill="#125D98")+
  annotate("rect", xmin = 0.23, xmax = 0.27, ymin = 0.27, ymax = 0.29, alpha = 1, fill="#DF2E38")+
  annotate("text", x = 0.3, y = .15, label = label, vjust=0, hjust=0, fontface="plain", size = 4, angle=0) + 
  
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+ 
  ggtitle(label="Atrophy of right hippocampus")+
  labs(tag = "C", x="1 - Specificity", y="Sensitivity")+
  
  theme_bw()+
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15),
    axis.title.y = element_text(color = "black",size=15),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=12, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=12, color="black") 
  )  
plot





#' [Protein & multi-timepoints] 
##---------------------------------------------------------------------------
# data -----------------------
database <- read.csv("GNHS_0_1_2.result.csv")

# plot ----------------------
input_data <- database; colnames(input_data)

plot <- ggplot(data = input_data, aes(x = Time, y = Concentration, group=group)) + 
  geom_smooth(aes(color=group), alpha=1, method="loess", formula=y~x, se=F, linewidth=1) + 
  scale_x_continuous(expand = c(0,0), limits = c(1,3), breaks = seq(1,3,1))+
  scale_y_continuous(expand = c(0,0))+
  # scale_color_manual(values = c("#729D39", OKeeffe2[c(2,6)]))+ 
  scale_color_manual(values = c(OKeeffe1[9], OKeeffe2[c(2,6)]))+ 
  
  labs(tag = "H", x="Year", y="Protein density (Z-scored)", title = NULL,
       color="Subtypes") +
  facet_grid(.~Protein, scales = "free")+
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.x = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(6,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "bottom",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(0.5,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15),
    axis.title.y = element_text(color = "black",size=15),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=12, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=12, color="black") 
  )  

plot



#'  [SHBG & MIND interaction, stratified by MIND]

##---------------------------------------------------------------------------
# data --------------------------
database <- openxlsx::read.xlsx("GNHS_MIND_pro.xlsx")

# plot --------------------------
input_data <- database
input_data$subtype <- as.character(input_data$subtype)

plot1 <- ggplot(data=input_data, aes(y=SHBG, x=diet_index))+   
  # geom_hline(yintercept = 0)+
  # geom_boxplot(aes(color=subtype), width=0.6, alpha=0.5, outlier.alpha = 0)+
  geom_point(aes(fill=subtype, group=subtype), shape=21, color="white", alpha=0.3, size=3, position = position_jitterdodge(jitter.width = 0))+ 
  geom_smooth(method = "lm", aes(group=subtype, color=subtype, fill=subtype), alpha=0.2)+
  annotate("text", x=8, y=3.5, label="P for interaction = 0.04", color="black", size=5)+
  
  scale_color_manual(values = c(OKeeffe2[c(6)], OKeeffe1[9]))+ 
  scale_fill_manual(values = c(OKeeffe2[c(6)], OKeeffe1[9]))+  
  
  # scale_color_manual(values = c(OKeeffe1[c(2,9)]))+
  scale_x_continuous(expand = c(0,0))+
  scale_y_continuous(expand = c(0,0))+
  # ggtitle(label = "SHBG")+ 
  labs(tag="B", fill=NULL, color=NULL, y="SHBG (z-score transformed)", x="MIND diet index")+
  guides(
    fill  = guide_legend(override.aes = list(size = 5, alpha = 0.9), title = NULL),
    color = guide_none()  # merge fill and color into one legend
  )+
  # 
  theme_classic()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5), 
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = c(0.15,0.85),
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    # legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15),
    axis.title.y = element_text(color = "black",size=15),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=12, color="black") 
  ) 
plot1



#' [subgroup of MIND diet & SHBG] 
##---------------------------------------------------------------------------
# data ---------------------------
stat_merge <- rbind(data.frame(group="high", stat_high_score[stat_high_score$term=="SHBG",c("p.value","OR","CI5","CI95")]),
                    data.frame(group="low", stat_low_score[stat_low_score$term=="SHBG",c("p.value","OR","CI5","CI95")]))


# plot ---------------------------
input_data <- stat_merge
input_data$label <- paste0("P = ", ifelse(input_data$p.value<0.001, format(input_data$p.value, scientific = T, digits = 2), round(input_data$p.value,3)))
input_data$group <- factor(input_data$group, levels = c("high","low"), labels = c("High
MIND", "Low
MIND"))


plot1 <- ggplot(data=input_data, aes(x=group, y=OR, fill=group, color=group))+  
  geom_hline(yintercept = 1, linetype="dashed", linewidth=0.5, color="grey80")+
  geom_errorbar(aes(ymin=CI5, ymax=CI95), width=.3)+  
  geom_point(aes(fill=group), shape=21, color="transparent", alpha=1, size=3)+
  geom_text(aes(label=label, y=CI95+0.03, x=group), hjust=0.5, vjust=-1, size=5, color="black")+
  
  scale_fill_manual(values = c(OKeeffe1[c(2,9)]))+
  scale_color_manual(values = c(OKeeffe1[c(2,9)]))+
  scale_y_continuous(expand = c(0,0), limits = c(0.6,1.2), breaks = seq(0.6,1.2,0.2))+
  ggtitle(label = "SHBG")+ 
  labs(tag="C", fill=NULL, x=NULL, y="Odds ratio for hyperglycemic subtype")+ 
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = 0), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15),
    axis.title.y = element_text(color = "black",size=15),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=15, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=12, color="black") 
  ) 
plot1


#' [PRS] 
##---------------------------------------------------------------------------
# data ---------------------------
stat_merge <- stat_merge[order(stat_merge$OR, decreasing = T),]
stat_merge <- stat_merge[order(stat_merge$group, decreasing = F),]
stat_merge <- stat_merge[stat_merge$p.value<0.05,]

# plot ---------------------------
input_data <- stat_merge
input_data$sig <- ifelse(input_data$p.value<0.05,"sig","insig")
input_data$sig[input_data$p.value<0.1 & input_data$p.value>0.05] <- "partial"

input_data$gene <- factor(input_data$gene, levels = input_data$gene)
input_data$sig <- factor(input_data$sig, levels = c("sig","partial","insig"))

plot1 <- ggplot(data=input_data, aes(y=gene, x=OR))+  
  geom_vline(xintercept = 1, linetype="dashed", linewidth=0.5, color="grey80")+
  geom_errorbar(aes(xmin=CI5, xmax=CI95, color=sig), width=.3)+  
  geom_point(aes(color=sig, fill=sig), shape=21, color="transparent", alpha=1, size=5)+ 
  
  scale_fill_manual(values = c(OKeeffe1[c(2,9)],"grey70"))+
  scale_color_manual(values = c(OKeeffe1[c(2,9)],"grey70"))+
  
  scale_x_continuous(expand = c(0,0), limits = c(0.4,1.4), breaks = seq(0.4,1.4,0.2), transform = "log")+
  ggtitle(label = "External validation (ADNI cohort)")+ 
  labs(tag = "F", fill=NULL, y=NULL, x="Odds ratio (log-transformed)")+ 
  facet_grid(group~., scales = "free", space = "free")+
  
  theme_bw()+ 
  theme( 
    # for facet
    strip.text.y = element_text(size = 15, angle = -90), 
    strip.background = element_rect(color = "transparent", fill= "transparent"),
    panel.spacing = unit(0,"mm"),
    
    # for ggtitle
    plot.title=element_text(color = 'black', size = 15, hjust = 0.5),
    
    panel.grid.major = element_line(color = NA), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "transparent",color = NA),
    plot.background = element_rect(fill = "transparent",color = NA),
    
    # legend
    legend.position = "none",
    legend.title = element_text(size = 15),
    legend.text = element_text(size = 15),
    legend.background = element_rect(fill = "transparent", color="transparent"),
    legend.key.width = unit(2,"cm"),
    legend.key = element_rect(fill="transparent",color = "transparent"), # legend point background
    legend.box.background = element_rect(fill = "transparent", color = "transparent"),
    
    # for axis
    axis.ticks.length = unit(3,"mm"),
    axis.title.x = element_text(color = "black",size=15),
    axis.title.y = element_text(color = "black",size=12),    
    axis.text.x = element_text(angle=0, hjust=0.5, vjust=1, size=12, color="black"),
    axis.text.y = element_text(angle=0, hjust=1, vjust=0.5, size=15, color="black") 
  ) 
plot1
