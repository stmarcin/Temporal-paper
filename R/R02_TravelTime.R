# Intro
Packages <- c("data.table")
lapply(Packages, library, character.only = TRUE)
rm(Packages)

file_all <- list.files(paste('Results', 'f03_Aggregates_Ai', sep="/"), pattern = "OBWOD") 

mc_max <- 100
# set.seed for reproductability of results    
set.seed(6543) 

# create a table with: col1: resolutions and col2: number of iterations for a selected resolution
tres <- data.table(resolution = c(seq(2,6), 10, 12, 15, 20, 30))
tres[, interations := round(60/(resolution),0)]

mape <- function(y, x) {round(mean(abs((y - x)/y))*100,2)}
mae <- function(y, x) {round(mean(abs(y-x)),2)}
maxdif <- function(y, x) {round(max(abs(y-x)),2)}

# define function for simple aggregation
SimpleAggr <- function(TempF, col_output){
    TempOD[, paste(col_output, res, sep="_") := round(as.data.table(TempF[, rowMeans(.SD)]), 1)] }

# create data.tables for the output
Results <- data.table(file = character(),
                      sampling = character(),
                      res = numeric(),
                      MAPE = numeric(),
                      MAE = numeric(),
                      MaxDiff = numeric())
cat(paste0("Start: ", Sys.time()))

for(file in file_all){
    Temp <- fread(paste('Results', 'f03_Aggregates_Ai', file, sep="/"))[Or != Des, c(3:62)]
    Temp <- Temp[rowSums(abs(Temp)) != 0,]
    
    # Calculate benchmark value
    TempOD <- data.table(TR_1 = round(Temp[, rowMeans(.SD)], 1))
    TempRes <- TempOD[, 1, with = F]
    
    # "S" Systematic Sampling 
        for(res in tres$resolution){
            SimpleAggr(TempF =  Temp[,seq(1, 60, res), with=F] , col_output = "TR") }
        # 60-minutes resolution
        TempOD <- TempOD[, TR_60 := Temp[, 1]]    
        
        Results <- rbind(Results, data.table(
            file = rep(gsub(".csv|OD_", "", file),
                       (nrow(tres)+1)))[, c("sampling", "res") := list(
                           "S", names(TempOD)[-c(1)])][, res := tstrsplit(
                               res, "_", fixed=TRUE)[2]][, c("MAPE", "MAE", "MaxDiff") := list(
                                   unlist(TempOD[, lapply(.SD, mape, y=TempOD$TR_1), .SDcols = -c(1)]),
                                   unlist(TempOD[, lapply(.SD, mae, y=TempOD$TR_1), .SDcols = -c(1)]),
                                   unlist(TempOD[, lapply(.SD, maxdif, y=TempOD$TR_1), .SDcols = -c(1)]))])
        
    # "R" - Simple Random Sampling
        # 60- minutes resolution
            res = 60
            mc = 1
            Tmape <- Tmae <- Tmax <- numeric()
            while(mc <= mc_max){
                random_cols <- c(sample(colnames(Temp), 1))
                TempOD <- cbind(TempRes, Temp[, ..random_cols]) 
                Tmape <- c(Tmape, as.numeric(TempOD[, lapply(.SD, mape, y=TempOD$TR_1), .SDcols = -c(1)]))    
                Tmae <- c(Tmae, as.numeric(TempOD[, lapply(.SD, mae, y=TempOD$TR_1), .SDcols = -c(1)]))    
                Tmax <- c(Tmax, as.numeric(TempOD[, lapply(.SD, maxdif, y=TempOD$TR_1), .SDcols = -c(1)]))    
                mc = mc +1 }
            
            Results <- rbind(Results, data.table(
                file = (gsub(".csv|OD_", "", file)))[, c("sampling", "res") := list(
                    "R", 60)][, c("MAPE", "MAE", "MaxDiff") := list(mean(Tmape), mean(Tmae), max(Tmax))])
            rm(res, Tmape, Tmae, Tmax, mc, TempOD, random_cols)
    
        # other resolutions
            for(res in tres$resolution){
            
            # "R" - Simple Random Sampling
                mc = 1
                Tmape <- Tmae <- Tmax <- numeric()
                
                while(mc <= mc_max){
                    
                    random_cols <- c(sample(colnames(Temp), tres$interations[tres$resolution==res]))
                   
                    TempOD <- rowMeans(Temp[, ..random_cols])
                    Tmape <- c(Tmape, mape(TempRes$TR_1, TempOD))
                    Tmae <- c(Tmae, mae(TempRes$TR_1, TempOD))
                    Tmax <- c(Tmax, maxdif(TempRes$TR_1, TempOD))
                              
                    mc = mc +1 }
                
                Results <- rbind(Results, data.table(
                    file = (gsub(".csv|OD_", "", file)))[, c("sampling", "res") := list(
                        "R", res)][, c("MAPE", "MAE", "MaxDiff") := list(mean(Tmape), mean(Tmae), max(Tmax))])
                rm(mc, random_cols, Tmape, Tmae, Tmax, TempOD)
                
            # "H" - Hybrid Sampling 
                fixed_cols <- which(names(Temp) %in% names(Temp[,seq(1, 60, res), with=F]))
                mc = 1
                Tmape <- Tmae <- Tmax <- numeric()
                
                while(mc <= mc_max){
                    random_cols <- numeric(0)
                    for(i in fixed_cols){
                        col_max <- i + res - 1
                        random_cols <- c(random_cols, sample(i:col_max, 1)) }
                    TempOD <- rowMeans(Temp[, ..random_cols])
                    Tmape <- c(Tmape, mape(TempRes$TR_1, TempOD))
                    Tmae <- c(Tmae, mae(TempRes$TR_1, TempOD))
                    Tmax <- c(Tmax, maxdif(TempRes$TR_1, TempOD))
                    mc = mc +1 }
                
                Results <- rbind(Results, data.table(
                    file = (gsub(".csv|OD_", "", file)))[, c("sampling", "res") := list(
                        "H", res)][, c("MAPE", "MAE", "MaxDiff") := list(mean(Tmape), mean(Tmae), max(Tmax))])
                rm(col_max, fixed_cols, i, random_cols, mc, Tmape, Tmae, Tmax, TempOD)
                
            # "W" - Constrained Random Walk Sampling        
                mc = 1
                Tmape <- Tmae <- Tmax <- numeric()
                while(mc <= mc_max){
                    random_cols <- sample(3:(res+2), 1) # select the first column
                    
                    while(tail(random_cols, n=1) <= 60){
                        random_cols <- c(random_cols, 
                                         sample((round(tail(random_cols, n=1) + res*0.5, 0)):
                                                    (round(tail(random_cols, n=1) + res*1.5, 0)), 1) ) }
                    random_cols <- random_cols[-length(random_cols)] # remove last column (out of table)
                    
                    TempOD <- rowMeans(Temp[, ..random_cols])
                    Tmape <- c(Tmape, mape(TempRes$TR_1, TempOD))
                    Tmae <- c(Tmae, mae(TempRes$TR_1, TempOD))
                    Tmax <- c(Tmax, maxdif(TempRes$TR_1, TempOD))
                    mc = mc +1 }
                Results <- rbind(Results, data.table(
                    file = (gsub(".csv|OD_", "", file)))[, c("sampling", "res") := list(
                        "W", res)][, c("MAPE", "MAE", "MaxDiff") := list(mean(Tmape), mean(Tmae), max(Tmax))])
                rm(random_cols, mc, Tmape, Tmae, Tmax, TempOD) }
        cat(paste("End file:", file, Sys.time()), "\n")
}
    
fwrite(Results, paste('Results', 't05_TTResults', "TravTime.csv", sep="/"), row.names=F, quote =F)        
    
    