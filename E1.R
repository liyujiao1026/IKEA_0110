rm(list = ls())
source("https://raw.githubusercontent.com/liyujiao1026/IKEA_0110/master/0_source.R")



treat_ID <- 2583
invYear <-  2006
repTimes <-  500
clusterNumber <- 7
inv_th <- which(2001:2012 == invYear)
# 1. cluster
cluster_result <- Func_cluster(treat_ID, invYear, clusterNumber =  clusterNumber)
Donor_ID <- setdiff( cluster_result$clustered_UnitId, c(880, 1480, 2583, 1780))
write.csv( cluster_result$data.SOM , "./data/data_SOM.csv", row.names = F)

# 2. Bootstrap
Bootstrap_Haparanda <- Func_Bootstrap_cmp(treat_ID = treat_ID , invYear = invYear, repTimes = repTimes, Donor_ID = Donor_ID)

saveRDS(Bootstrap_Haparanda, "./Bootstrap_Haparanda")
write.csv(Bootstrap_Haparanda,"Bootstrap_Haparanda.csv", row.names = F)






#-------------------------------------------------------------#
# 3. Parametric model of growth curve
Bootstrap_Result2 <- readRDS("./Empirical_study/Empirical_result/Bootstrap_Result2")
y.post <- do.call(cbind, Bootstrap_Result2[2,])[(inv_th - 1):12,]

gap.mean <- apply(y.post, 1, mean)
gap.up <- apply(y.post, 1, quantile, 0.99)
gap.low <- apply(y.post, 1, quantile, 0.01)

plot(x = 1:length(gap.mean), y = gap.mean, ylim = c(-10, 120))
lines(x = 1:length(gap.mean), y = gap.up, ylim = c(-10, 120))
lines(x = 1:length(gap.mean), y = gap.low, ylim = c(-10, 120))



par.model <- apply(y.post, 2, function(x){Func_parametric(x)$predict.Y[,2]})







gap.mean <- apply(par.model, 1, mean)
gap.up <- apply(par.model, 1, quantile, 0.95)
gap.low <- apply(par.model, 1, quantile, 0.05)


gap <- apply(gap.post, 1, mean)
gap.up1 <- apply(gap.post, 1, quantile, 0.95)

y.pre <- Func_parametric(gap)$predict.Y

plot(x = 1:length(gap), gap, ylim = c(0, 120), pch = 16)
lines(x= y.pre[,1], gap.up, ylim = c(0, 120))
lines(x= y.pre[,1], gap.low, ylim = c(0, 120))








we <- unlist(sapply(1:500, function(i){subset(Bootstrap.Result[3,i]$synth.tables$tab.w, w.weights >= 0.1)$unit.numbers}))


