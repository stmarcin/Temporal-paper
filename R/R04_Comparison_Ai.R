# Load libraries
Packages <- c("data.table", "ggplot2", "cowplot", "gridExtra", "grid")
lapply(Packages, library, character.only = TRUE)
rm(Packages)# import libraries

# intro ----

# open files and include missing values
AddMissingValues <- function(dest){
    # add missing values (for 60 and 1-minute resolutions)
    dest <- rbind(dest,
                  dest[res == 60 & sampling == "R" ][ , sampling := "H"],
                  dest[res == 60 & sampling == "R" ][ , sampling := "W"],
                  dest[res == 5 ][, res := 1][ , c("MAPE", "MAE", "MaxDiff") := 0]) 
    # aggregate results for all scenarios
    dest <- rbind(dest, 
                  dest[, .(MAPE = mean(MAPE), 
                             MAE = mean(MAE), 
                             MaxDiff = max(MaxDiff)), 
                       by = .(sampling, res) ][, file := "All"])[order(file, sampling, res)]
    # add color size
    dest <- dest[file == "All", width := 0.8][file != "All", width := 0.4]
    
    # add dot size
    dest <- dest[file == "All", point := 1][file != "All", point := 0.6]
    }

# proximity
Adm  <- AddMissingValues(fread("Results/t06_Results/Adm4.csv"))
Zlob <- AddMissingValues(fread("Results/t06_Results/Zlob4.csv"))

# cumulative
Teatr  <- AddMissingValues(fread("Results/t06_Results/30_Teatr4.csv"))
SpecHC <- AddMissingValues(fread("Results/t06_Results/30_SpecHC4.csv"))

HOS  <- AddMissingValues(fread("Results/t06_Results/HOS4.csv"))
Edu  <- AddMissingValues(fread("Results/t06_Results/Edu_LO4.csv"))
POP  <- AddMissingValues(fread("Results/t06_Results/OBWOD4.csv"))

rm(AddMissingValues)

# plot MAPE for Hybrid method: comparison of destinations (aggregated values) ----

# prepare data for comparison

H_Ai <- rbind(Adm[sampling == "H" & file == "All", .(res, MAPE)][, file := "Adm"],
              Zlob[sampling == "H" & file == "All", .(res, MAPE)][, file := "Zlob"],
              Teatr[sampling == "H" & file == "All", .(res, MAPE)][, file := "Teatr"],
              SpecHC[sampling == "H" & file == "All", .(res, MAPE)][, file := "SpecHC"],
              HOS[sampling == "H" & file == "All", .(res, MAPE)][, file := "HOS"],
              Edu[sampling == "H" & file == "All", .(res, MAPE)][, file := "Edu"],
              POP[sampling == "H" & file == "All", .(res, MAPE)][, file := "POP"])

Plot_HMAPE <- ggplot(data = H_Ai) +
    aes(x = res, y = MAPE, color = file) +
    geom_line(size = 0.5) +
    labs(title = "Mean Absolute Percentage Error (%) \n",
         x = "temporal resolution (minutes)",
         y = "MAPE (%)") +
    theme_light() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, lineheight=1.5),
          legend.title=element_text(size=14), 
          legend.text=element_text(size=11),
          axis.text=element_text(size=10),
          axis.title=element_text(size=12)) +
    scale_color_manual(name = "",
                       breaks=c("Adm", "Zlob", "Teatr", "SpecHC", "HOS", "Edu", "POP"),
                       values=c("#4C9900", "#FF9999", "#990000", "#FF0000", "#9999FF", "#4C0099", "#66FF66"),
                       labels = c("Administration","Nurseries",
                                  " Theatres", "Health care",
                                  "Hospitals", "Schools", "Population")) +
    geom_point(size = 0.9)

ggsave(plot = Plot_HMAPE, "Ai_Hybrid.png", path="Results/t07_Graphs", width=10,height=8, scale=2, units="cm" )

PlotLegend <- get_legend(Plot_HMAPE)
Plot_HMAPE <- Plot_HMAPE + theme(legend.position = "none")

Plot_HMAPESel <- ggplot(data = H_Ai[file != c("Teatr") & file != c("SpecHC")]) +
    aes(x = res, y = MAPE, color = file) +
    geom_line(size = 0.5) +
    labs(title = "Mean Absolute Percentage Error (%) \n zoom \n (without cumulative opportunities measures) \n",
         x = "temporal resolution (minutes)",
         y = "MAPE (%)") +
    theme_light() +
    theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5, lineheight=1.5),
          legend.title=element_text(size=14), 
          legend.text=element_text(size=11),
          axis.text=element_text(size=10),
          axis.title=element_text(size=12)) +
    scale_color_manual(name = "",
                       breaks=c("Adm", "Zlob", "HOS", "Edu", "POP"),
                       values=c("#4C9900", "#FF9999", "#990000", "#FF0000", "#66FF66"),
                       labels = c("Administration","Nurseries",
                                  "Hospitals", "Schools", "Population")) +
    geom_point(size = 0.9) +
    theme(legend.position = "none")


blank<-rectGrob(gp=gpar(col="white"))
lay <- rbind(c(1,1,1,4,2,2),
             c(1,1,1,4,2,2),
             c(1,1,1,4,3,3))
Plot_MAPE  <- grid.arrange(Plot_HMAPE, Plot_HMAPESel, PlotLegend, blank,
                               layout_matrix = lay, widths = c(0.2, 0.2, 0.2, 0.05, 0.2, 0.2))

ggsave(plot = Plot_MAPE, "Fig_6_Ai_Hybrid_complex.png", 
       path="Results/t07_Graphs", width=18,height=10, scale=2, units="cm" )

rm(H_Ai, blank, lay, Plot_MAPE, Plot_HMAPE, Plot_HMAPESel, PlotLegend)

# Comparison of sampling methods ----
# version without unified ylim
Plot_Adm <- ggplot(data = Adm[file == "All"]) +
    aes(x = res, y = MAPE, color = sampling) +
    geom_line(size = 0.5) +
    labs(title = "public administration",
         x = "Temporal Resolution",
         y = "MAPE (%)") +
    theme_light() +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.title=element_text(size=12, lineheight=.2), 
          legend.text=element_text(size=11),
          axis.text=element_text(size=8),
          axis.title=element_text(size=10)) +
    scale_colour_discrete(name  ="Sampling Method \n",
                          breaks=c("S", "R", "H", "W"),
                          labels=c("Systematic", "Simple Random", "Hybrid", "Constrained Random Walk"))
PlotLegend <- get_legend(Plot_Adm)
Plot_Adm <- Plot_Adm + theme(legend.position = "none")

PlotSampling <- function(file, title){
    ggplot(data = file) +
        aes(x = res, y = MAPE, color = sampling) +
        geom_line(size = 0.5) +
        labs(title = title,
             x = "temporal resolution",
             y = "MAPE (%)") +
        theme_light() +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
        theme(legend.title=element_text(size=12), 
              legend.text=element_text(size=11),
              axis.text=element_text(size=8),
              legend.position = "none",
              axis.title=element_text(size=10)) +
        scale_colour_discrete(name  ="Sampling Method",
                              breaks=c("S", "R", "H", "W"),
                              labels=c("Systematic", "Simple Random", "Hybrid", "Constrained Random Walk")) }

Plot_Edu <- PlotSampling(Edu[file == "All"], "secondary school")
Plot_HOS <- PlotSampling(HOS[file == "All"], "hospital")
Plot_POP <- PlotSampling(POP[file == "All"], "population")
Plot_SpecHC <- PlotSampling(SpecHC[file == "All"], "specialized health care")
Plot_Teatr <- PlotSampling(Teatr[file == "All"], "theatre")
Plot_Zlob <- PlotSampling(Zlob[file == "All"], "nursery")

lay <- rbind(c(1,2),
             c(3,4),
             c(5,6),
             c(7,8))
Plot_Sampling  <- grid.arrange(Plot_Adm, Plot_Zlob, 
                               Plot_SpecHC, Plot_Teatr,
                               Plot_Edu, Plot_HOS, 
                               Plot_POP, PlotLegend, 
                               layout_matrix = lay, 
                               heights=c(0.25, 0.25, 0.25, 0.25))

ggsave(plot = Plot_Sampling, "Ai_Sampling.png", 
       path="Results/t07_Graphs", width=10,height=14, scale=2, units="cm" )

rm(PlotSampling, Plot_Sampling, lay, 
   Plot_Adm, Plot_Zlob, Plot_SpecHC, Plot_Teatr, Plot_Edu, Plot_HOS, Plot_POP, PlotLegend)

# plot MAPE for Hybrid method: comparison of destinations (separate curves for each of the scenarios)----
# variant with synchronized ylim

Plot_Adm <- ggplot(data = Adm[sampling == "H"]) +
    aes(x = res, y = MAPE, color = file) +
    geom_line(size = Adm[sampling == "H", width]) +
    labs(title = "public administration",
         x = "Temporal Resolution",
         y = "MAPE (%)") +
    ylim(0, 11.5) +
    theme_light() +
    theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
          legend.title=element_text(size=12, lineheight=.2), 
          legend.text=element_text(size=11),
          axis.text=element_text(size=8),
          axis.title=element_text(size=10)) +
    scale_color_manual(name = "",
                       values=c("#0066CC", "#FF8000", "#CCCC00", "#00994C","#FF0000"),
                       labels = c("  02:00 - 03:00","  07:00 - 08:00","  10:00 - 11:00","  22:00 - 23:00", "  All"))

PlotLegend <- get_legend(Plot_Adm)
Plot_Adm <- Plot_Adm + theme(legend.position = "none")

PlotScenarios <- function(Dest, title, ylimmax = 11.5){
    
    ggplot(data = Dest) +
        aes(x = res, y = MAPE, color = file) +
        geom_line(size = Dest$width) +
        labs(title = title,
             x = "Temporal Resolution",
             y = "MAPE (%)") +
        theme_light() +
        ylim(0, ylimmax) +
        theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
              legend.position = "none",
              axis.text=element_text(size=8),
              axis.title=element_text(size=10)) +
        scale_color_manual(name = "",
                           breaks=unique(substr(Zlob[, file], nchar(Zlob[, file]), nchar(Zlob[, file]))),
                           values=c("#FF0000", "#0066CC", "#FF8000", "#CCCC00", "#00994C"),
                           labels = c("  All", "  02:00 - 03:00","  07:00 - 08:00","  10:00 - 11:00","  22:00 - 23:00")) }

Plot_Edu <- PlotScenarios(Edu[sampling == "H"], "secondary school")
Plot_HOS <- PlotScenarios(HOS[sampling == "H"], "hospital")
Plot_POP <- PlotScenarios(POP[sampling == "H"], "population")
Plot_SpecHC <- PlotScenarios(SpecHC[sampling == "H"], "specialized health care", ylimmax = 60.5)
Plot_Teatr <- PlotScenarios(Teatr[sampling == "H"], "theatre", ylimmax = 60.5)
Plot_Zlob <- PlotScenarios(Zlob[sampling == "H"], "nursery")

lay <- rbind(c(1,2),
             c(3,4),
             c(5,6),
             c(7,8))

Plot_Scenarios <- grid.arrange(Plot_Adm, Plot_Zlob, 
                               Plot_Teatr, Plot_SpecHC,
                               Plot_HOS, Plot_Edu, 
                               Plot_POP, PlotLegend, 
                               layout_matrix = lay)

ggsave(plot = Plot_Scenarios, "Ai_H_scenarios.png", 
       path="Results/t07_Graphs", width=10,height=14, scale=2, units="cm" )

rm(PlotScenarios, Plot_Scenarios, Plot_Adm, Plot_Zlob, Plot_Teatr, Plot_SpecHC, 
   Plot_HOS, Plot_Edu, Plot_POP, PlotLegend, lay)

