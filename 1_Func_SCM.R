# function of running SCM

data_firm <- read.csv("./data/IKEA_data_1207.csv")
data_firm$Kommun_name <- as.character(data_firm$Kommun_name)


SCM_estimate <- function(treat_ID, ctrl_ID, invYear){
            
            opt_from <- 2001
            opt_to <- invYear - 1
            
            dataprep.out <-
                        dataprep(
                                    foo = data_firm,
                                    predictors = c("EmployeeIndex",  "Population", 
                                                    "Population_University"),
                                    
                                    predictors.op = "mean",
                                    
                                    special.predictors = list(
                                                list("Productivity", invYear - 1, "mean"),
                                                list("Productivity", invYear - 2, "mean"),
                                                list("Productivity", invYear - 3, "mean")
                                    ),
                                    
                                    dependent = "Productivity",
                                    unit.variable = "Kommun_code",
                                    time.variable = "Year",
                                    treatment.identifier = treat_ID,
                                    controls.identifier = ctrl_ID,
                                    time.predictors.prior = c(opt_from:opt_to),
                                    time.optimize.ssr = c(opt_from:opt_to),
                                    unit.names.variable = "Kommun_name",
                                    time.plot = 2001:2012
                        )
            
            
            
            synth.out <- try(synth(dataprep.out), silent = T)
            
            if ("try-error" %in% class(synth.out)) {
                        MSPE <-  gaps <-  synth.tables <- NA
            } else { 
                        synth.tables <- synth.tab(
                                    dataprep.res = dataprep.out,
                                    synth.res = synth.out)
                        
                        
                        gaps <- dataprep.out$Y1plot - (
                                    dataprep.out$Y0plot %*% synth.out$solution.w
                        )
                        
                        preYear <- invYear > row.names(gaps)%>%as.numeric
                        
                        MSPE <- sum(gaps[preYear]^2)
            }
            
            
            return(list("MSPE" = MSPE, "gaps" = gaps, "synth.tables" = synth.tables))
}


SCM_estimate_cmp <- cmpfun(SCM_estimate)



# #test
# treat_ID <- 2583
# invYear = 2006
# treat_ID = 2583
# 
# 
# ID_all <- unique(data_firm$Kommun_code)
# ctrl_ID = sample(setdiff(ID_all , treat_ID),30)
# tes <- try(SCM_estimate_cmp( invYear = invYear, treat_ID = treat_ID, ctrl_ID = ctrl_ID))
