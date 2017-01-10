library(Synth)
library(plyr)
library(compiler)

#data_firm <- read.csv("./data/IKEA_data_1207.csv")
data_firm <- read.csv("https://raw.githubusercontent.com/liyujiao1026/IKEA_0110/master/IKEA_data_1207.csv")

data_firm$Kommun_name <- as.character(data_firm$Kommun_name)
ID_all <- unique(data_firm$Kommun_code)

SCM_estimate <- function(treat_ID, ctrl_ID, invYear){
            
            dataprep.out <-
                        dataprep(
                                    foo = data_firm,
                                    predictors = c("EmployeeIndex",  "Population", "Percent_University"),
                                    predictors.op = "mean",
                                    dependent = "SalesIndex",
                                    unit.variable = "Kommun_code",
                                    time.variable = "Year",
                                    treatment.identifier = treat_ID,
                                    controls.identifier = ctrl_ID,
                                    time.predictors.prior = c(2001:2005),
                                    time.optimize.ssr = c(2001:2005),
                                    unit.names.variable = "Kommun_name",
                                    time.plot = 2001:2010
                        )
            
            synth.out <- synth(dataprep.out)
            
            synth.tables <- synth.tab(
                        dataprep.res = dataprep.out,
                        synth.res = synth.out)
            
            
            gaps <- dataprep.out$Y1plot-(
                        dataprep.out$Y0plot %*% synth.out$solution.w
            )
            
            inv_th <- invYear - 2001 
            
            MSPE <- sum(gaps[1:inv_th]^2)
            
            return(list("MSPE" = MSPE, "gaps" = gaps, "synth.tables" = synth.tables))
}


SCM_estimate_cmp <- cmpfun(SCM_estimate)
# test
# treat_ID <- 2583
# ID_all <- unique(data_firm$Kommun_code)
# ctrl_ID = sample( setdiff(ID_all , 2583),10)
# testResult <- SCM_estimate( invYear = 2006, treat_ID = 2583, ctrl_ID = ctrl_ID)
