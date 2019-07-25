# Intro ----

# Load libraries
Packages <- c("data.table", "ggplot2", "cowplot", "gridExtra", "grid")
lapply(Packages, library, character.only = TRUE)
rm(Packages)# import libraries

TTime <- fread("Results/t05_TTResults/TravTime.csv")


# add 60-minutes resolution to H & W scenarios & 0-values 
TTime <- rbind(TTime, 
               TTime[res == 60 & sampling == "R" ][ , sampling := "H"],
               TTime[res == 60 & sampling == "R" ][ , sampling := "W"],
               TTime[res == 5 ][, res := 1][ , c("MAPE", "MAE", "MaxDiff") := 0])

# aggregate results for all scenarios
TTime <- rbind(TTime, 
               TTime[, .(MAPE = mean(MAPE), 
                         MAE = mean(MAE), 
                         MaxDiff = max(MaxDiff)), by = .(sampling, res) ][, file := "All"])[order(file, sampling, res)]
# add color size
TTime <- TTime[file == "All", width := 0.8][file != "All", width := 0.3]

# add point size
TTime <- TTime[file == "All", point := 1][file != "All", point := 0.5]

# plot MAPE & MAE for Hybrid method: comparison of scenarios ----

Fig_TTime <- TTime[sampling == "H"]

Plot_HMAPE <- ggplot(data = Fig_TTime) +
    aes(x = res, y = MAPE, color = file) +
    geom_line(size = Fig_TTime$width) +
    labs(title = "Mean Absolute Percentage Error (%)",
         x = "temporal resolution (minutes)",
         y = "MAPE (%)") +
    theme_light() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
          legend.title=element_text(size=14), 
          legend.text=element_text(size=11),
          axis.text=element_text(size=10),
          axis.title=element_text(size=12)) +
    scale_color_manual(name = "",
        values=c("#FF0000", "#0066CC", "#FF8000", "#CCCC00", "#00994C"),
        labels = c("All","  02:00 - 03:00","  07:00 - 08:00","  10:00 - 11:00","  22:00 - 23:00"))+
    geom_point(size = Fig_TTime$point)
    
PlotLegend <- get_legend(Plot_HMAPE)
Plot_HMAPE <- Plot_HMAPE + theme(legend.position = "none")
    
    
Plot_HMAE <- ggplot(data = Fig_TTime) +
    aes(x = res, y = MAE, color = file) +
    geom_line(size = Fig_TTime$width) +
    labs(title = "Mean Absolute Error (minutes)",
         x = "temporal resolution (minutes)",
         y = "minutes") +
    theme_light() +
    theme(plot.title = element_text(size = 15, hjust = 0.5, face = "bold")) +
    theme(legend.title=element_blank(), 
          legend.position="none",
          axis.text=element_text(size=10),
          axis.title=element_text(size=12)) +
    scale_color_manual(values=c("#FF0000", "#0066CC", "#FF8000", "#CCCC00", "#00994C") ) +
    geom_point(size = Fig_TTime$point)

# define layot 
lay <- rbind(c(1,1),
             c(2,2),
             c(3,4))

# blank space
blank<-rectGrob(gp=gpar(col="white"))

Plot_Hybrid <- grid.arrange(Plot_HMAPE, blank, Plot_HMAE, PlotLegend, 
                               layout_matrix = lay, 
                               heights=c(0.55, 0.05, 0.45), widths = c(0.7, 0.3))
ggsave(plot = Plot_Hybrid, "Fig_5_TravelTime_estimation.png", path="Results/t07_Graphs", width=10,height=10, scale=2, units="cm" )

rm(Fig_TTime, blank, lay, Plot_HMAE, Plot_HMAPE, Plot_Hybrid, PlotLegend)

# Comparison of sampling methods ----
Fig_TTime <- TTime[file == "All"]

Plot_Sampling <- ggplot(data = Fig_TTime) +
    aes(x = res, y = MAPE, color = sampling) +
    geom_line(size = Fig_TTime$width) +
    labs(title = "Travel time comparisons \n Mean Absolute Percentage Error (%) \n",
         x = "temporal resolution (minutes)",
         y = "MAPE (%)") +
    theme_light() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, lineheight=1.5),
          legend.title=element_text(size=14, lineheight=.2), 
          legend.text=element_text(size=11),
          axis.text=element_text(size=10),
          axis.title=element_text(size=12)) +
    scale_colour_discrete(name  ="Sampling Method \n",
                          breaks=c("S", "R", "H", "W"),
                          labels=c("Systematic", "Simple Random", "Hybrid", "Constrained Random Walk"))

ggsave(plot = Plot_Sampling, "TravelTime_sampling.png", path="Results/t07_Graphs", width=13.5,height=7.5, scale=2, units="cm" )

# clear workspace
rm(list=ls())