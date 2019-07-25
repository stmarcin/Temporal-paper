# Load libraries
Packages <- c("data.table", "xlsx")
lapply(Packages, library, character.only = TRUE)
rm(Packages)

# intro ----
Gini <- setNames(data.table(matrix(nrow = 0, ncol = 3)), c("file", "res", "Gini"))

file_all <-  list.files(paste('Results', 't06_Results', 'Gini',sep="/"))

for(filepath in file_all){
    Gini <- rbind(Gini, fread(paste('Results', 't06_Results', 'Gini', 
                                    filepath ,sep="/"))[sampling =="H" |
                                                            (sampling =="S" & res == 1) |
                                                            (sampling =="R" & res == 60)][,.(file, res, Gini)]) }


write.xlsx(Gini, "Results/t07_Graphs/Gini_table.xlsx")
