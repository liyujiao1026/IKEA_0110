rm(list = ls())
#source('./S1_SCM_estimate.R', echo = F)
source("https://raw.githubusercontent.com/liyujiao1026/IKEA_0110/master/S1_SCM_estimate.R",echo = F)

#source('./S2_SCM_loop.R', echo = F)
source("https://raw.githubusercontent.com/liyujiao1026/IKEA_0110/master/S2_SCM_loop.R")

treatID_4 <- unique(data_firm$Kommun_code[which(data_firm$Kommun_name %in% c("Haparanda", "Kalmar", "Karlstad", "Göteborg"))])
treat_ID <- treatID_4[1]
invYear <- 2006
control_ID <- setdiff( unique(data_firm$Kommun_code), treatID_4)



# 1. loop all units -----------------------------------------#
res1 <- Permutation(treat_ID, control_ID, invYear)
result_treat <- res1$Result_treat
control_length <- length(control_ID)

control_ID <- setdiff(control_ID, res1$removeID)


while (length(control_ID) > 3){
            
            control_length <- c(control_length, length(control_ID))
            res <- Permutation(treat_ID, control_ID, invYear)
            result_treat <- cbind( result_treat, res$Result_treat)
            control_ID <- setdiff(control_ID, res$removeID)
}

# 2. results ------------------------------------------#
# 2.1 Sales ---#
DonorSize <- control_length
MSPE_vector <- sapply( 1:length(DonorSize), function(x){result_treat[1,x][[1]]})
alpha.1_vector <- sapply( 1:length(DonorSize), function(x){result_treat[2,x][[1]][invYear - 1999]})
alpha.n_vector <- sapply( 1:length(DonorSize), function(x){mean(result_treat[2,x][[1]][(invYear - 1999):10])})

sales_data <- data.frame(DonorSize, MSPE_vector, alpha.1_vector, alpha.n_vector)

write.csv(sales_data, "./data/sales_data.csv", row.names = F)


# 2.2 covariates discrepency -------------#
synth_cov <- sapply( 1:length(DonorSize), function(x){result_treat[3,x][[1]]$tab.pred[,2]})
treat_cov <- result_treat[3,1][[1]]$tab.pred[,1]
covar <- cbind(treat_cov, synth_cov)
cov_data <- rbind("DonorSize" = c(NA, DonorSize), covar)
colnames(cov_data) <- c("Treat", paste0("Synth", 1:(ncol(covar)-1) ))
covariate_data <- t(cov_data)
covariate_data 

write.csv(covariate_data, "./data/covariate_data.csv", row.names = T)


# 2.3 weight -----------------------------#
weightFun <- function(x){t(result_treat[3,x][[1]]$tab.w[,1, drop = F])}
weightVector <- weightFun(1)

for(i in 2:length(DonorSize)){
            weightVector <- rbind.fill.matrix(weightVector, weightFun(i))
            
}

weight_data <- as.data.frame(cbind(DonorSize, weightVector))
colnames(weight_data) <- c("DonarSize" ,as.character(result_treat[3,1][[1]]$tab.w[,2]))

write.csv(weight_data, "./data/weight_data.csv", row.names = F)


#======================================================#
salesData <- read.csv("./data/sales_data.csv")
covData <- read.csv("./data/covariate_data.csv")
weightData <- read.csv("./data/weight_data.csv")


# 3. plot
# plot.1 mspe ---#
plot(x = salesData$DonorSize, y = salesData$MSPE_vector, 
     xlab = "donar size",ylab = "MSPE", type = "l")

# plot.2 alpha ---#
y.lim <- c(salesData$alpha.1_vector, salesData$alpha.n_vector)
y.min <- 0.8*min(y.lim); y.max <- 1.2*max(y.lim)
plot(x = salesData$DonorSize, y = salesData$alpha.1_vector,  
     xlab = "donar size",ylab = "alpha",
     type = "l",lty=1, ylim = c(y.min, y.max))
lines(x = salesData$DonorSize, y = salesData$alpha.n_vector, lty=2)
legend("topright", c("alpha.1", "alpha.n"), lty = c(1,2))


# plot.3 cov ---#
# plot function
plotFun <- function(cov_name){
            
            var_th <- which(names(covData) == cov_name)
            cov.ylim <- c(0.8 * min(covData[,var_th]), 1.2 * max(covData[,var_th]))
            
            plot(x = covData$DonorSize[-1], y = covData[,var_th][-1],
                 xlab = "donor size", ylab = cov_name, type = "l", ylim = cov.ylim )
            
            abline(h = covData[1,cov_name], lty = 2)
            
            legend("topright", lty = c(2,1), c("Treat", "Synth"))
            
}     


plotFun("EmployeeIndex")
plotFun("Population")
plotFun("Percent_University")


# plot.4 weight ---#
weightMatrix <- as.matrix(weightData[,-1])
image(weightMatrix, col = heat.colors(100))

