
Ai_proximity <- function(file_all, mc_max){

    Packages <- c("data.table", "ineq")
    lapply(Packages, library, character.only = TRUE)
    rm(Packages)
    
    # define functions
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
    
    # define function for simple aggregation
    SimpleAggr <- function(TempF, col_output){
        TempOD[, paste(col_output, res, sep="_") := round(as.data.table(TempF[, rowMeans(.SD)]), 1)] }
    
    for(file in file_all){
        
        # open file with travel times
        Temp <- fread(paste('Results', 'f03_Aggregates', file, sep="/"), colClasses=list(character=1:2))[, c(1:62)]  
        
        # Calculate a benchmark value
            BaseAi <- data.table(Or = Temp$Or,
                                    TR_1 = round(Temp[, rowMeans(.SD), .SDcols = 3:62], 1) )[, .(min(TR_1)), by = Or]
            setnames(BaseAi, "V1", "TR_1")
        
        # "S" Systematic Sampling
            TempAi <- BaseAi
            
            TempOD <- Temp[, 1]
            for(res in tres$resolution){
                SimpleAggr(TempF =  Temp[,seq(3, 62, res), with=F] , col_output = "TR")}
            
            TempAi <- merge(TempAi, 
                            merge(Temp[, lapply(.SD, min), .SDcols = c(3), by = "Or"], 
                                  TempOD[ , lapply(.SD, min), by = Or], by = "Or"), 
                            by = "Or")
            setnames(TempAi, c(3), "TR_60")
            
            fwrite(TempAi, paste('Results', 't04_Temporary', gsub("OD_", "Avg_", file), sep="/"), row.names=F, quote =F)
            
            Results <- rbind(Results, data.table(
                file = rep(gsub(".csv|OD_", "", file),
                           (nrow(tres)+1)))[, c("sampling", "res") := list(
                               "S", names(TempAi)[-c(1,2)])][, res := tstrsplit(
                                   res, "_", fixed=TRUE)[2]][, c("MAPE", "MAE", "MaxDiff") := list(
                                       unlist(TempAi[, lapply(.SD, mape, y=TempAi$TR_1), .SDcols = -c(1:2)]),
                                       unlist(TempAi[, lapply(.SD, mae, y=TempAi$TR_1), .SDcols = -c(1:2)]),
                                       unlist(TempAi[, lapply(.SD, maxdif, y=TempAi$TR_1), .SDcols = -c(1:2)]))])
            
            
            ResGini <- rbind(ResGini,data.table(
                file = rep(gsub(".csv|OD_", "", file),
                           (nrow(tres)+2)))[, c("sampling", "res", "Gini")  := list(
                               "S", names(TempAi)[-1],
                               unlist(TempAi[, lapply(.SD, GINI), .SDcols = -c(1)]))][, res := tstrsplit(
                                   res, "_", fixed=TRUE)[2]])
                             
            rm(TempAi, res, TempOD)
        
        # "R" for 60-minutes resolution
            TempAi <- BaseAi    
            res = 60
            mc = 1
            while(mc <= mc_max){
                random_cols <- c("Or", sample(colnames(Temp)[3:62], 1))
                TempOD <- Temp[, ..random_cols] 
                TempAi <- merge(TempAi, TempOD[, lapply(.SD, min), .SDcols = c(2), by = "Or"], by = "Or")
                setnames(TempAi, (mc+2), paste0("M",mc))
                mc = mc + 1
            }
            
            TempMAPE <- rbind(TempMAPE,
                              cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res, 
                                    TempAi[, lapply(.SD, mape, y=TempAi$TR_1), .SDcols = -c(1:2)]))
            TempMAE <- rbind(TempMAE,
                             cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res, 
                                   TempAi[, lapply(.SD, mae, y=TempAi$TR_1), .SDcols = -c(1:2)]))
            TempMaxDiff <- rbind(TempMaxDiff,
                                 cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res, 
                                       TempAi[, lapply(.SD, maxdif, y=TempAi$TR_1), .SDcols = -c(1:2)]))
            TempGini <- rbind(TempGini,
                              cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res, 
                                    TempAi[, lapply(.SD, GINI), .SDcols = -c(1:2)]))   
            rm(TempAi, res)
                
        
        for(res in tres$resolution){
        
        # "R" - Simple Random Sampling
            TempAi <- BaseAi
            
            mc = 1
            while(mc <= mc_max){
                random_cols <- c(sample(colnames(Temp)[3:62], tres$interations[tres$resolution==res]))
                
                TempOD <- data.table(Or = Temp$Or)[, "TT" :=  rowMeans(Temp[, random_cols, with = FALSE])]
                
                TempAi <- merge(TempAi, TempOD[, .(min(TT)), by = Or], by = "Or")         
                setnames(TempAi, "V1", paste0("M", mc))
                rm(random_cols, TempOD)                          
                mc = mc +1   }
            
            # combine simulations
            TempMAPE <- rbind(TempMAPE,
                             cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res, 
                                   TempAi[, lapply(.SD, mape, y=TempAi$TR_1), .SDcols = -c(1:2)]))
            TempMAE <- rbind(TempMAE,
                             cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res, 
                                   TempAi[, lapply(.SD, mae, y=TempAi$TR_1), .SDcols = -c(1:2)]))
            TempMaxDiff <- rbind(TempMaxDiff,
                             cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res, 
                                   TempAi[, lapply(.SD, maxdif, y=TempAi$TR_1), .SDcols = -c(1:2)]))
            TempGini <- rbind(TempGini,
                             cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "R"), res, 
                                   TempAi[, lapply(.SD, GINI), .SDcols = -c(1:2)]))   
            rm(TempAi)
        
        # "H" - Hybrid Sampling 
            fixed_cols <- which(names(Temp) %in% names(Temp[,seq(3, 62, res), with=F]))
            
            TempAi <- BaseAi
            mc = 1
            while(mc <= mc_max){
                random_cols <- numeric(0)
                for(i in fixed_cols){
                    col_max <- i + res - 1
                    random_cols <- c(random_cols, sample(i:col_max, 1)) }
                
                TempOD <- data.table(Or = Temp$Or)[, "TT" :=  rowMeans(Temp[, random_cols, with = FALSE])]
                TempAi <- merge(TempAi, TempOD[, .(min(TT)), by = Or], by = "Or")         
                setnames(TempAi, "V1", paste0("M", mc))
                rm(random_cols, TempOD, col_max)                          
                mc = mc +1   }
            
            # combine simulations
            TempMAPE <- rbind(TempMAPE,
                              cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "H"), res, 
                                    TempAi[, lapply(.SD, mape, y=TempAi$TR_1), .SDcols = -c(1:2)]))
            TempMAE <- rbind(TempMAE,
                             cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "H"), res, 
                                   TempAi[, lapply(.SD, mae, y=TempAi$TR_1), .SDcols = -c(1:2)]))
            TempMaxDiff <- rbind(TempMaxDiff,
                                 cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "H"), res, 
                                       TempAi[, lapply(.SD, maxdif, y=TempAi$TR_1), .SDcols = -c(1:2)]))
            TempGini <- rbind(TempGini,
                              cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "H"), res, 
                                    TempAi[, lapply(.SD, GINI), .SDcols = -c(1:2)]))   
            rm(TempAi, fixed_cols, i)   
            
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
                
                TempOD <- data.table(Or = Temp$Or)[, "TT" :=  rowMeans(Temp[, random_cols, with = FALSE])]
                TempAi <- merge(TempAi, TempOD[, .(min(TT)), by = Or], by = "Or")         
                setnames(TempAi, "V1", paste0("M", mc))
                rm(random_cols, TempOD)                          
                mc = mc +1   }
            
            # combine simulations
            TempMAPE <- rbind(TempMAPE,
                              cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "W"), res, 
                                    TempAi[, lapply(.SD, mape, y=TempAi$TR_1), .SDcols = -c(1:2)]))
            TempMAE <- rbind(TempMAE,
                             cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "W"), res, 
                                   TempAi[, lapply(.SD, mae, y=TempAi$TR_1), .SDcols = -c(1:2)]))
            TempMaxDiff <- rbind(TempMaxDiff,
                                 cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "W"), res, 
                                       TempAi[, lapply(.SD, maxdif, y=TempAi$TR_1), .SDcols = -c(1:2)]))
            TempGini <- rbind(TempGini,
                              cbind(data.table(file = gsub(".csv|OD_", "", file), sampling = "W"), res, 
                                    TempAi[, lapply(.SD, GINI), .SDcols = -c(1:2)]))   
            rm(TempAi, mc)
            
        }
    
    }
    
    Results <- rbind(Results, TempMAPE[, c(1:3)][, c("MAPE", "MAE", "MaxDiff") := list(
        TempMAPE[, round(rowMeans(.SD), 3), .SDcols = -c(1:3)],
        TempMAE[, round(rowMeans(.SD), 3), .SDcols = -c(1:3)],
        TempMaxDiff[, round(rowMeans(.SD), 3), .SDcols = -c(1:3)])])
    
    ResGini <-  rbind(ResGini, TempGini[, c(1:3)][, Gini := round(TempGini[, rowMeans(.SD), .SDcols = -c(1:3)], 3)])              
    
    fwrite(Results, paste('Results', 't06_Results', gsub("OD_", "", file), sep="/"), row.names=F, quote =F)    
    fwrite(ResGini, paste('Results', 't06_Results', 'Gini', gsub("OD_", "Gini", file), sep="/"), row.names=F, quote =F)
    fwrite(TempMAPE, paste('Results', 't04_Temporary', gsub("OD_", "MAPE", file), sep="/"), row.names=F, quote =F)
    fwrite(TempMAE, paste('Results', 't04_Temporary', gsub("OD_", "MAE", file), sep="/"), row.names=F, quote =F)
    fwrite(TempMaxDiff, paste('Results', 't04_Temporary', gsub("OD_", "MaxDiff", file), sep="/"), row.names=F, quote =F)
    fwrite(TempGini, paste('Results', 't04_Temporary', paste(threshold, gsub("OD_", "TGini", file), sep = "_"), sep="/"), row.names=F, quote =F)
    
    rm(Temp, TempMAPE, TempMAE, TempMaxDiff, TempGini, res, BaseAi, file)
    
}
    
