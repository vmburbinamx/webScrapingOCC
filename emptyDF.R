dataBase <- readRDS("dataBase.rds")
dataBase <- dataBase[0,]
saveRDS(dataBase, file = "dataBase.rds")
remove(dataBase)