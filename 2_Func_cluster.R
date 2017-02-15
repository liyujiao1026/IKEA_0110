# function is used for clustering


Func_cluster <- function(treat_ID, invYear, clusterNumber){ 
            
            var.cluster <- c("Kommun_code", "Year", "Kommun_name", "Productivity",
                             "Population", "Population_University", "EmployeeIndex")
            
            data.SOM0 <- subset(data_firm, Year %in% (invYear - 3):(invYear - 1))[, var.cluster]
            data.SOM1 <- data.SOM0[order(data.SOM0$Kommun_code),]
            data.SOM2 <- with( data.SOM1, data.frame(Kommun_code, Kommun_name) %>% unique)
            for (i in c(1,4:7)) {
                        
                        new <- by(data.SOM1[,i], data.SOM1$Kommun_code, mean) %>% as.numeric()
                        data.SOM2 <- cbind(data.SOM2, new)
                        colnames(data.SOM2)[colnames(data.SOM2) == "new"] <- colnames(data.SOM1)[i]
            }
            data.SOM <- data.SOM2[,-3]
            #rm(list = c("data.SOM0", "data.SOM1", "data.SOM2", "i", "new"))
            

            
            somnet <- som(scale(as.matrix(data.SOM[,-c(1:2)])),
                          grid = somgrid(clusterNumber, 1, "rectangular"))
            plot(somnet)
            
            data.SOM$cluster <- map(somnet)$unit.classif
            treat.cluster <- data.SOM[data.SOM$Kommun_code == treat_ID,]$cluster
            clustered_UnitId <- subset(data.SOM, cluster == treat.cluster)$Kommun_code
            
            return(list("clustered_UnitId" = clustered_UnitId,  "data.SOM" = data.SOM))
            
}


#
# treat_ID <- 2583
# invYear <- 2006
# Func_cluster(treat_ID, invYear, 7)
