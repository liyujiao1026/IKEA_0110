# function of bootstrap



Func_Bootstrap <- function(treat_ID, invYear, repTimes, Donor_ID){
            
            f_Bootstrap <- function(treat_ID, invYear, Donor_ID){
                        
                        donor_id <- setdiff(Donor_ID , treat_ID)
                        ctrl_ID = unique(sample(donor_id, size = length(donor_id), replace = T))
                        tes <- try(SCM_estimate_cmp( invYear = invYear, treat_ID = treat_ID, ctrl_ID = ctrl_ID))
                        return(tes)
            }
            
            result_bootstrap <- replicate(n = repTimes, 
                                          expr = f_Bootstrap(treat_ID = treat_ID , invYear = invYear, Donor_ID))
            return(result_bootstrap)
}

Func_Bootstrap_cmp <- cmpfun(Func_Bootstrap)

# invYear = 2006
# treat_ID <- 2583
# repTimes = 3
# Donor_ID <- sample(unique(data_firm$Kommun_code), 30)
# 
# tes <- Func_Bootstrap_cmp(treat_ID, invYear, repTimes, Donor_ID)
