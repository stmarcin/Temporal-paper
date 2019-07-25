Ai_cumulative <- function(file_all, threshold, mc_max){

    Packages <- c("data.table", "ineq")
    lapply(Packages, library, character.only = TRUE)
    rm(Packages)
    
    #intro
    {# define functions
    mape <- function(y, x) {round(mean(abs((y - x)/y))*100,2)}
    mae <- function(y, x) {round(mean(abs(y-x)),2)}
    maxdif <- function(y, x) {round(max(abs(y-x)),2)}
    GINI<-function(x) {round(ineq(x, type="Gini"), 3)}
    
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
    }
    
    
    for(file in file_all){
        
        # open file with travel times
        Temp <- fread(paste('Results', 'f03_Aggregates', file, sep="/"), colClasses=list(character=1:2))[,c(1:62)]  
        
        # calculate a benchmark value
            Cols <- names(Temp)[-c(1,2)]
            
            
            TempAi <- data.table(Or = unique(Temp$Or))
            
            for(column in Cols){
                TempOD <- Temp[, c("Or", column), with = FALSE]
                TempAi <- merge(TempAi, TempOD[TempOD[[2]] <= threshold, .(.N), by = Or], by = "Or", all = T) 
                setnames(TempAi, "N", column)}
            TempAi[is.na(TempAi)] <- 0  
            
            BaseAi <- data.table(Or = TempAi$Or, TR_1 = TempAi[, rowMeans(.SD), .SDcols = -c(1)])
            rm(Cols, column, TempOD, TempAi)    
    
        # "S" Systematic Sampling
            TempAi <- BaseAi
            
            for(res in tres$resolution){
                TempOD <- Temp[, c(1, seq(3, 62, res)), with = F]
                Cols <- names(TempOD)[-1]
                TempIso <- data.table(Or = unique(Temp$Or))
                for(column in Cols){
                    TempOD <- Temp[, c("Or", column), with = FALSE]
                    TempIso <- merge(TempIso, TempOD[TempOD[[2]] <= threshold, .(.N), by = Or], by = "Or", all = T) 
                    setnames(TempIso, "N", column)   }
                TempIso[is.na(TempIso)] <- 0      
                TempIso <- data.table(Or = TempIso$Or, Iso = TempIso[, rowMeans(.SD), .SDcols = -c(1)])
                setnames(TempIso, "Iso", paste0("TR_", res))
                TempAi <- merge(TempAi, TempIso, by = "Or")   
                rm(Cols)}
            TempAi <- merge(TempAi, Temp[Temp[[3]] <= threshold, c(1,3)][, .(.N), by = Or], by = "Or", all=T)
            setnames(TempAi, "N", "TR_60")
            TempAi[is.na(TempAi)] <- 0 
            
            fwrite(TempAi, paste('Results', 't04_Temporary', gsub("OD_", "Avg_", file), sep="/"), row.names=F, quote =F)
            
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
            rm(TempAi, res, TempOD, TempIso, column)
        
        # "R" for 60-minutes resolution
            TempAi <- BaseAi
            res = 60
            mc = 1
            
            while(mc <= mc_max){
                random_cols <- c("Or", sample(colnames(Temp)[3:62], 1))
                TempOD <- Temp[, ..random_cols] 
                TempAi <- merge(TempAi, TempOD[TempOD[[2]] <= threshold, .(.N), by = Or], by = "Or", all = T) 
                setnames(TempAi, "N", paste0("M",mc))   
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
            
            # "R" - Simple Random Sampling
                TempAi <- BaseAi
                
                mc = 1
                while(mc <= mc_max){
                    random_cols <- c(sample(colnames(Temp)[3:62], tres$interations[tres$resolution==res]))
                    TempIso <- data.table(Or = unique(Temp$Or))
                    
                    for(column in random_cols){
                        TempOD <- Temp[, c("Or", column), with = FALSE]
                        TempIso <- merge(TempIso, TempOD[TempOD[[2]] <= threshold, .(.N), by = Or], by = "Or", all = T) 
                        setnames(TempIso, "N", column)   }
                    TempIso[is.na(TempIso)] <- 0      
                    TempIso <- data.table(Or = TempIso$Or, Iso = TempIso[, rowMeans(.SD), .SDcols = -c(1)])
                    setnames(TempIso, "Iso", paste0("M", mc))
                    TempAi <- merge(TempAi, TempIso, by = "Or")   
                    rm(column, random_cols) 
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
            
            # "H" - Hybrid Sampling 
                fixed_cols <- which(names(Temp) %in% names(Temp[,seq(3, 62, res), with=F]))
                
                TempAi <- BaseAi
                mc = 1
                while(mc <= mc_max){
                    random_cols <- numeric(0)
                    for(i in fixed_cols){
                        col_max <- i + res - 1
                        random_cols <- c(random_cols, sample(i:col_max, 1)) }
                    
                    TempIso <- data.table(Or = unique(Temp$Or))    
                    
                    for(column in random_cols){
                        TempOD <- cbind(Temp[, .(Or)], Temp[, c(column), with = FALSE])
                        TempIso <- merge(TempIso, TempOD[TempOD[[2]] <= threshold, .(.N), by = Or], by = "Or", all = T) 
                        setnames(TempIso, "N", paste0("C", column))   }
                    TempIso[is.na(TempIso)] <- 0      
                    TempIso <- data.table(Or = TempIso$Or, Iso = TempIso[, rowMeans(.SD), .SDcols = -c(1)])
                    setnames(TempIso, "Iso", paste0("M", mc))
                    TempAi <- merge(TempAi, TempIso, by = "Or")   
                    rm(column, col_max, i, random_cols) 
                    mc = mc + 1 }
                    
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
                rm(TempAi, TempIso, TempOD, mc, fixed_cols)
                
            # "W" - Constrained Random Walk Sampling        
                TempAi <- BaseAi
                mc = 1
                
                while(mc <= mc_max){
                    random_cols <- sample(3:(res+2), 1) # select the first column
                    
                    while(tail(random_cols, n=1) <= 62){
                        random_cols <- c(random_cols, 
                                         sample((round(tail(random_cols, n=1) + res*0.5, 0)):
                                                    (round(tail(random_cols, n=1) + res*1.5, 0)), 1) ) }
                    
                    random_cols <- random_cols[-length(random_cols)] # remove last column (out of table)
                    
                    TempIso <- data.table(Or = unique(Temp$Or))    
                    
                    for(column in random_cols){
                        TempOD <- cbind(Temp[, .(Or)], Temp[, c(column), with = FALSE])
                        TempIso <- merge(TempIso, TempOD[TempOD[[2]] <= threshold, .(.N), by = Or], by = "Or", all = T) 
                        setnames(TempIso, "N", paste0("C", column))   }
                    TempIso[is.na(TempIso)] <- 0      
                    TempIso <- data.table(Or = TempIso$Or, Iso = TempIso[, rowMeans(.SD), .SDcols = -c(1)])
                    setnames(TempIso, "Iso", paste0("M", mc))
                    TempAi <- merge(TempAi, TempIso, by = "Or")   
                    rm(column, random_cols) 
                    mc = mc + 1 }
                    
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
                rm(TempAi, TempIso, TempOD, mc)     
            
        }
        
    }
    
    Results <- rbind(Results, TempMAPE[, c(1:3)][, c("MAPE", "MAE", "MaxDiff") := list(
        TempMAPE[, round(rowMeans(.SD), 3), .SDcols = -c(1:3)],
        TempMAE[, round(rowMeans(.SD), 3), .SDcols = -c(1:3)],
        TempMaxDiff[, round(rowMeans(.SD), 3), .SDcols = -c(1:3)])])
    
    ResGini <-  rbind(ResGini, TempGini[, c(1:3)][, Gini := round(TempGini[, rowMeans(.SD), .SDcols = -c(1:3)], 3)])              
    
    fwrite(Results, paste('Results', 't06_Results', paste(threshold, gsub("OD_", "", file), sep = "_"), sep="/"), row.names=F, quote =F)    
    fwrite(ResGini, paste('Results', 't06_Results', 'Gini', paste(threshold, gsub("OD_", "Gini", file), sep = "_"), sep="/"), row.names=F, quote =F)
    fwrite(TempMAPE, paste('Results', 't04_Temporary', paste(threshold, gsub("OD_", "MAPE", file), sep = "_"), sep="/"), row.names=F, quote =F)
    fwrite(TempMAE, paste('Results', 't04_Temporary', paste(threshold, gsub("OD_", "MAE", file), sep = "_"), sep="/"), row.names=F, quote =F)
    fwrite(TempMaxDiff, paste('Results', 't04_Temporary', paste(threshold, gsub("OD_", "MaxDiff", file), sep = "_"), sep="/"), row.names=F, quote =F)
    fwrite(TempGini, paste('Results', 't04_Temporary', paste(threshold, gsub("OD_", "TGini", file), sep = "_"), sep="/"), row.names=F, quote =F)
    
    rm(Temp, TempMAPE, TempMAE, TempMaxDiff, TempGini, res, BaseAi, file)

}