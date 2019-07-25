
# load scripts for proximity, cumulative & potential accessibility

source('RStudio_Temporal/R011_Ai_proximity.R')

source('RStudio_Temporal/R011_Ai_cumulative.R')

source('RStudio_Temporal/R011_Ai_potential.R')

# set number of iterations for all scripts
mc_max <- 100

# create a table with: col1: resolutions and col2: number of iterations for a selected resolution
tres <- data.table::data.table(resolution = c(seq(2,6), 10, 12, 15, 20, 30))
tres[, interations := round(60/(resolution),0)]

# proximity

file_all <- list.files(paste('Results', 'f03_Aggregates', sep="/"), pattern = "Adm")
Ai_proximity(file_all, mc_max)

file_all <- list.files(paste('Results', 'f03_Aggregates', sep="/"), pattern = "Zlob")
Ai_proximity(file_all, mc_max)



# cumulative opportunities

threshold <- 30

file_all <- list.files(paste('Results', 'f03_Aggregates', sep="/"), pattern = "SpecHC")
Ai_cumulative(file_all, threshold, mc_max)

file_all <- list.files(paste('Results', 'f03_Aggregates', sep="/"), pattern = "Teatr")
Ai_cumulative(file_all, threshold, mc_max)


# potential accessibility

beta <- 0.023105

# Education:
file_all <- list.files(paste('Results', 'f03_Aggregates_Ai', sep="/"), pattern = "Edu") 
file_mass <- "Results/p00_data/EduLO.csv"
Ai_potential(file_mass = file_mass, file_all = file_all, beta = beta, mc_max = mc_max)

# Hospitals
file_all <- list.files(paste('Results', 'f03_Aggregates_Ai', sep="/"), pattern = "HOS") 
file_mass <- "Results/p00_data/HOS.csv"
Ai_potential(file_mass = file_mass, file_all = file_all, beta = beta, mc_max = mc_max)

# Population
file_all <- list.files(paste('Results', 'f03_Aggregates_Ai', sep="/"), pattern = "OBWOD") 
file_mass <- "Results/p00_data/POP.csv"
Ai_potential(file_mass = file_mass, file_all = file_all, beta = beta, mc_max = mc_max)


