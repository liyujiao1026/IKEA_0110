# fuction for growu curve estimation of intervention

Func_parametric <- function(gap.obs){ 
            
            time <- 1:length(gap.obs)
            
            f.beta <- function(par, gap.obs, type){
                        a <- par[1]
                        r <- par[2]
                        B <- par[3]
                        
                        gap.pre_Monomolecular <- a * (1 - B * exp((-1) * r * time)) 
                        gap.pre_Logistic <- a / (1 + B * exp((-1) * r * time))
                        gap.pre_Gompertz <- a * exp((-1) * B * exp((-1) * r * time))
                        
                        
                        if (type == "M") { gap.pre <- gap.pre_Monomolecular
                        } else if (type == "L") {
                                    gap.pre <- gap.pre_Logistic
                        } else {
                                    gap.pre <- gap.pre_Gompertz        
                        }
                        
                        
                        gaps <-  gap.obs - gap.pre
                        gaps2 <- sum( gaps ^ 2 )
                        return(gaps2)
                        
            }
            
            init <- quantile(gap.obs, 0.95)
            
            M.model <- optim(par = c(init, 2, 1), f.beta , gap.obs = gap.obs , type = "M")
            L.model <- optim(par = c(init, 2, 1), f.beta , gap.obs = gap.obs , type = "L")
            G.model <- optim(par = c(init, 2, 1), f.beta , gap.obs = gap.obs , type = "G")
            
            optimal <- which.min(c(M.model$value, L.model$value, G.model$value))
            
            
            model.optimal <- c("M","L","G")[optimal]
            par.optimal <- list(M.model$par, L.model$par, G.model$par )[[optimal]]
            names(par.optimal) <- c("a","r","B")
            
            a <- par.optimal[1]
            r <- par.optimal[2]
            B <- par.optimal[3]
            
            
            time.plot <- seq(from = 1, to = length(gap.obs), length.out = 20)
            predict.Y <- if (optimal == 1){
                        a * (1 - B * exp((-1) * r * time.plot)) 
            }else if (optimal == 2){
                        a / (1 + B * exp((-1) * r * time.plot))
            }else{
                        a * exp((-1) * B * exp((-1) * r * time.plot))
            }
            
            return(list( "model.optimal" = model.optimal, 
                         "par.optimal"= par.optimal,  
                         "predict.Y" = data.frame(time.plot,predict.Y)))
}

# 
# gap <- tes$gaps
# gap.post <- gap[row.names(gap)%>%as.numeric > invYear]
# 
# y.pre <- Func_parametric(gap.post)$predict.Y
# 
# plot(x = 1:length(gap.post), gap.post, ylim = c(50, 120))
# lines(x= y.pre[,1], y.pre[,2], ylim = c(50, 120))


