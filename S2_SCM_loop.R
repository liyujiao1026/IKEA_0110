# 1. SCM estimation loop for all units------------------------------------------------------#
Permutation <- function(treat_ID, control_ID, invYear){ 
            myres <- sapply(c(treat_ID, control_ID), 
                            FUN = function(x){ SCM_estimate_cmp( invYear = invYear, treat_ID = x,  ctrl_ID = setdiff(control_ID, x))}) 
            Result_treat <- myres[,1]
            
            
            removeID_th <- which(as.numeric(myres[1,])[-1] > quantile(as.numeric(myres[1,]), 0.95))
            
            if(length(removeID_th) == 0){
                        removeID_th <- which(as.numeric(myres[1,])[-1] == max(as.numeric(myres[1,])))
            }
            
            removeID <- control_ID[removeID_th]
            
            return(list("Result_treat" = Result_treat, "removeID" = removeID))
}


Permutation <- cmpfun(Permutation)
# Test 
# res <- Permutation(treat_ID, control_ID, invYear =2006)