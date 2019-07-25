
Ai_potential <- function(file_mass, file_all, beta, mc_max){

    Packages <- c("data.table", "ineq", "dplyr")
    lapply(Packages, library, character.only = TRUE)
    rm(Packages)
    
    # define functions
    mape <- function(y, x) {round(mean(abs((y - x)/y))*100,2)}
    mae <- function(y, x) {round(mean(abs(y-x)),2)}
    maxdif <- function(y, x) {round(max(abs(y-x)),2)}
    GINI<-function(x) {round(ineq(x, type="Gini"), 3)}
        
    FAi <- function(time, mass, beta) {round(mass*(exp(-beta*time)),2)}
        
    # set.seed for reproductability of results    
    set.seed(6543) 
    # create data.tables for the output
    TempMAPE <- cbind(data.table(file = character(),
                                 sampling = character(),
                                 res = numeric()), 
                      data.table(matrix(nrow = 0, ncol = mc_max)))
    setnames(TempMAPE, names(TempMAPE), gsub("V", "M", names(TempMAPE)))
    TempGini <- TempMaxDiff <- TempMAE <- TempMAPE
        
    Results <- data.table(file = character(),
                          sampling = character(),
                          res = numeric(),
                          MAPE = numeric(),
                          MAE = numeric(),
                          MaxDiff = numeric())
    
    ResGini <- data.table(file = character(),
                          sampling = character(),
                          res = numeric(),
                          Gini = numeric())
    
    HarmonicAggr <- function(TempF, col_output, col_output2=""){
        TempOD[, paste0(paste(col_output, res, sep="_"), col_output2) := round(
            as.data.table(apply(TempF, 1, function(x,y) hm_exp(x,beta))), 1)] }
        
    # function defition
    hm_exp <- function(a,b) {log(1/mean(1/exp(a*b)))/b}
    
    
    for(file in file_all){
        
        # open file with travel times
        
        Fmass <- fread(file_mass, colClasses=list(character=c(1)))
        setnames(Fmass, 1:2, c("Des", "mass"))
        
        Temp <- fread(paste('Results', 'f03_Aggregates_Ai', file, sep="/"), 
                      colClasses=list(character=1:2))[,c(1:62)] %>%
            merge(Fmass, by = "Des")
        rm(Fmass)
        
        TempOD <- Temp[,c(1:2, 63)]
        BaseAi <- Temp[, !c(1:2, 63)]
        
        # Calculate a benchmark value
                cat("Calculate a benchmark value")
            BaseAi <- TempOD[,"TR_1" := round(as.data.table(apply(BaseAi, 1, function(x,y) hm_exp(x,beta))), 1)]
            
            BaseAi[, Ai_1 := round(mass*(exp(-beta*BaseAi[,4])),2)]
            BaseAi <- BaseAi[, .(sum(Ai_1)), by = Or]
            setnames(BaseAi, 2, "TR_1")
        
        # "S" Systematic Sampling
            cat("Systematic Sampling")
            # Aggregate travel times
            for(res in tres$resolution){
               HarmonicAggr(TempF =  Temp[,seq(3, 62, res), with=F] , col_output = "TR")}
            TempOD[, TR_60 := Temp[,3]]
                    
            TempAi <- cbind(Temp[, .(Or)], TempOD[, lapply(.SD, FAi, mass = mass, beta = beta), .SDcols = -c(1:5) ])
            
            TempAi <- merge(BaseAi, TempAi[, lapply(.SD, sum), .SDcols = -c(1), by = Or], by = "Or")
                
            fwrite(TempAi, paste('Results', 't04_Temporary', gsub("OD_", "Ai_", file), sep="/"), row.names=F, quote =F)    
    
            TempIso <- TempAi[TR_1 != 0]
            
            Results <- rbind(Results, data.table(
                file = rep(gsub(".csv|OD_", "", file),
                           (nrow(tres)+1)))[, c("sampling", "res") := list(
                               "S", names(TempAi)[-c(1,2)])][, res := tstrsplit(
                                   res, "_", fixed=TRUE)[2]][, c("MAPE", "MAE", "MaxDiff") := list(
                                       unlist(TempIso[, lapply(.SD, mape, y=TempIso$TR_1), .SDcols = -c(1:2)]),
                                       unlist(TempAi[, lapply(.SD, mae, y=TempAi$TR_1), .SDcols = -c(1:2)]),
                                       unlist(TempAi[, lapply(.SD, maxdif, y=TempAi$TR_1), .SDcols = -c(1:2)]))])
            
            ResGini <- rbind(ResGini,data.table(
                file = rep(gsub(".csv|OD_", "", file),
                           (nrow(tres)+2)))[, c("sampling", "res", "Gini")  := list(
                               "S", names(TempAi)[-1],
                               unlist(TempAi[, lapply(.SD, GINI), .SDcols = -c(1)]))][, res := tstrsplit(
                                   res, "_", fixed=TRUE)[2]])
            rm(TempAi, res, TempOD, TempIso)
        
        # "R" for 60-minutes resolution
            cat("Random for 60 minutes")
            TempAi <- BaseAi
            res = 60
            mc = 1
            
            while(mc <= mc_max){
                random_cols <- c("Or", "mass", sample(colnames(Temp)[3:62], 1))
                TempOD <- Temp[, ..random_cols]
                TempOD[, Ai := round(mass*(exp(-beta*TempOD[,3])),2)]
                
                TempAi <- merge(TempAi, TempOD[, .(sum(Ai)), by = Or], by = "Or")
                setnames(TempAi, "V1", paste0("M",mc))
                mc = mc + 1   }
            
            TempAi[is.na(TempAi)] <- 0
            TempIso <- TempAi[TR_1 != 0]
            
            TempMAPE <- rbind(TempMAPE,
                              cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res,
                                    TempIso[, lapply(.SD, mape, y=TempIso$TR_1), .SDcols = -c(1:2)]))
            TempMAE <- rbind(TempMAE,
                             cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res,
                                   TempAi[, lapply(.SD, mae, y=TempAi$TR_1), .SDcols = -c(1:2)]))
            TempMaxDiff <- rbind(TempMaxDiff,
                                 cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res,
                                       TempAi[, lapply(.SD, maxdif, y=TempAi$TR_1), .SDcols = -c(1:2)]))
            TempGini <- rbind(TempGini,
                              cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res,
                                    TempAi[, lapply(.SD, GINI), .SDcols = -c(1:2)]))
            rm(TempAi, TempIso, TempOD, res, mc)
        
        # R & H & W for other resolutions
            for(res in tres$resolution){
                cat(paste("Resolution:", res, Sys.time()))
    
            # "R" - Simple Random Sampling
                cat("Random sampling")
                TempAi <- BaseAi
                
                mc = 1
                
                while(mc <= mc_max){
                    random_cols <- c("Or", "mass", 
                                     sample(colnames(Temp)[3:62], tres$interations[tres$resolution==res]))
                    TempOD <- Temp[, ..random_cols]
                    
                    HarmonicAggr(TempF = TempOD[, -c(1:2)], col_output = "TR")        
                    
                    TempOD[, Ai := round(mass*(exp(-beta*TempOD[,ncol(TempOD), with = F])),2)]
                    
                    TempAi <- merge(TempAi, TempOD[, .(sum(Ai)), by = Or], by = "Or")
                    setnames(TempAi, "V1", paste0("M",mc))
                    mc = mc + 1   }
                
                TempIso <- TempAi[TR_1 != 0]
                
                # combine simulations
                TempMAPE <- rbind(TempMAPE,
                                  cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res, 
                                        TempIso[, lapply(.SD, mape, y=TempIso$TR_1), .SDcols = -c(1:2)]))
                TempMAE <- rbind(TempMAE,
                                 cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res, 
                                       TempAi[, lapply(.SD, mae, y=TempAi$TR_1), .SDcols = -c(1:2)]))
                TempMaxDiff <- rbind(TempMaxDiff,
                                     cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res, 
                                           TempAi[, lapply(.SD, maxdif, y=TempAi$TR_1), .SDcols = -c(1:2)]))
                TempGini <- rbind(TempGini,
                                  cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res, 
                                        TempAi[, lapply(.SD, GINI), .SDcols = -c(1:2)]))   
                rm(TempAi, TempIso, TempOD, mc)
            
            # "H" - Simple Random Sampling
                cat("Hybrid sampling")
                fixed_cols <- which(names(Temp) %in% names(Temp[,seq(3, 62, res), with=F]))
                
                TempAi <- BaseAi
                mc = 1
                
                while(mc <= mc_max){
                    random_cols <- c(2, 63)
                    for(i in fixed_cols){
                        col_max <- i + res - 1
                        random_cols <- c(random_cols, sample(i:col_max, 1)) }
    
                    TempOD <- Temp[, ..random_cols]
                    
                    HarmonicAggr(TempF = TempOD[, -c(1:2)], col_output = "TR")        
                    
                    TempOD[, Ai := round(mass*(exp(-beta*TempOD[,ncol(TempOD), with = F])),2)]
                    
                    TempAi <- merge(TempAi, TempOD[, .(sum(Ai)), by = Or], by = "Or")
                    setnames(TempAi, "V1", paste0("M",mc))
                    mc = mc + 1   }
                
                TempIso <- TempAi[TR_1 != 0]
                
                # combine simulations
                TempMAPE <- rbind(TempMAPE,
                                  cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "H"), res, 
                                        TempIso[, lapply(.SD, mape, y=TempIso$TR_1), .SDcols = -c(1:2)]))
                TempMAE <- rbind(TempMAE,
                                 cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "H"), res, 
                                       TempAi[, lapply(.SD, mae, y=TempAi$TR_1), .SDcols = -c(1:2)]))
                TempMaxDiff <- rbind(TempMaxDiff,
                                     cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "H"), res, 
                                           TempAi[, lapply(.SD, maxdif, y=TempAi$TR_1), .SDcols = -c(1:2)]))
                TempGini <- rbind(TempGini,
                                  cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "H"), res, 
                                        TempAi[, lapply(.SD, GINI), .SDcols = -c(1:2)]))   
                rm(TempAi, TempIso, TempOD, mc, col_max, fixed_cols, i)
                        
    
            # "W" - Constrained Random Walk Sampling
                cat("Constrained Random Walk Sampling")
                TempAi <- BaseAi
                mc = 1
                
                while(mc <= mc_max){
                    random_cols <- c(2, 63, sample(3:(res+2), 1)) # select the first column
                    
                    while(tail(random_cols, n=1) <= 62){
                        random_cols <- c(random_cols, 
                                         sample((round(tail(random_cols, n=1) + res*0.5, 0)):
                                                    (round(tail(random_cols, n=1) + res*1.5, 0)), 1) ) }
                    
                    random_cols <- random_cols[-length(random_cols)] # remove last column (out of table)
                    
                    TempOD <- Temp[, ..random_cols]
                    
                    HarmonicAggr(TempF = TempOD[, -c(1:2)], col_output = "TR")        
                    
                    TempOD[, Ai := round(mass*(exp(-beta*TempOD[,ncol(TempOD), with = F])),2)]
                    
                    TempAi <- merge(TempAi, TempOD[, .(sum(Ai)), by = Or], by = "Or")
                    setnames(TempAi, "V1", paste0("M",mc))
                    mc = mc + 1   }
                
                TempIso <- TempAi[TR_1 != 0]
                
                # combine simulations
                TempMAPE <- rbind(TempMAPE,
                                  cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "W"), res, 
                                        TempIso[, lapply(.SD, mape, y=TempIso$TR_1), .SDcols = -c(1:2)]))
                TempMAE <- rbind(TempMAE,
                                 cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "W"), res, 
                                       TempAi[, lapply(.SD, mae, y=TempAi$TR_1), .SDcols = -c(1:2)]))
                TempMaxDiff <- rbind(TempMaxDiff,
                                     cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "W"), res, 
                                           TempAi[, lapply(.SD, maxdif, y=TempAi$TR_1), .SDcols = -c(1:2)]))
                TempGini <- rbind(TempGini,
                                  cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "W"), res, 
                                        TempAi[, lapply(.SD, GINI), .SDcols = -c(1:2)]))   
                rm(TempAi, TempIso, TempOD, mc, random_cols)            
            }
    }
    
    Results <- rbind(Results, TempMAPE[, c(1:3)][, c("MAPE", "MAE", "MaxDiff") := list(
        TempMAPE[, round(rowMeans(.SD), 3), .SDcols = -c(1:3)],
        TempMAE[, round(rowMeans(.SD), 3), .SDcols = -c(1:3)],
        TempMaxDiff[, round(rowMeans(.SD),3), .SDcols = -c(1:3)])])
    
    ResGini <-  rbind(ResGini, TempGini[, c(1:3)][, Gini := round(TempGini[, rowMeans(.SD), .SDcols = -c(1:3)], 3)])              
    
    fwrite(Results, paste('Results', 't06_Results', gsub("OD_", "", file), sep="/"), row.names=F, quote =F)    
    fwrite(ResGini, paste('Results', 't06_Results', 'Gini', gsub("OD_", "Gini", file), sep="/"), row.names=F, quote =F)
    fwrite(TempMAPE, paste('Results', 't04_Temporary', gsub("OD_", "MAPE", file), sep="/"), row.names=F, quote =F)
    fwrite(TempMAE, paste('Results', 't04_Temporary', gsub("OD_", "MAE", file), sep="/"), row.names=F, quote =F)
    fwrite(TempMaxDiff, paste('Results', 't04_Temporary', gsub("OD_", "MaxDiff", file), sep="/"), row.names=F, quote =F)
    fwrite(TempGini, paste('Results', 't04_Temporary', paste(threshold, gsub("OD_", "TGini", file), sep = "_"), sep="/"), row.names=F, quote =F)
    
    rm(BaseAi, beta, FAi, file, file_all, file_mass, HarmonicAggr, hm_exp, res, 
       Temp, TempGini, TempMAE, TempMAPE, TempMaxDiff, Results, ResGini)
}

