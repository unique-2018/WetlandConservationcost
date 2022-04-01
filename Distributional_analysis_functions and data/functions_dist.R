
library(fitdistrplus)
library(logspline)
library(ggpubr)

set.seed(1)

dist_fit <- function(df){
    
  df <- df %>% drop_na()
  
  fit.weibull <- fitdist(df$value, "weibull")
  fit.norm <- fitdist(df$value, "norm")
  fit.uniform <- fitdist(df$value, "unif")
  fit.exp <- fitdist(df$value, "exp")
  fit.lnorm <- fitdist(df$value, "lnorm")
  fit.gamma <- fitdist(df$value, "gamma")
  fit.logistic <- fitdist(df$value, "logis")
  
  df_plot <- data.frame(
    dist = c("normal", "uniform", "exponential", " weibull", "lognormal", "gamma", "logistic"),
    aic = c(fit.norm$aic, fit.uniform$aic, fit.exp$aic,  fit.weibull$aic, fit.lnorm$aic, fit.gamma$aic, fit.logistic$aic)
  ) %>% dplyr::arrange(aic) %>% dplyr::mutate(aic = round(aic,1)) 
  
  g1 <- ggplot(df_plot, aes(x = reorder(dist, aic), y = aic), fill=dist) + geom_bar(stat="identity") + 
    geom_text(aes(label=aic), vjust=1.5, colour="white", size=3.5) + labs(title = "Best Distribution Fit of Varaible (lowest AIC value)",x ="Distibution", y = "Akaike Information Critera") + theme_bw() 
  
  g2 <- ggplot(df, aes(x = value)) +  geom_histogram(aes(y = ..density..),  colour = 1, fill = "white") + geom_density() + labs(title = "Distribution of Variable") +
         theme_bw() 
  
  g3 <- ggplot(df, aes(x = value, y = year)) + geom_point() +labs(title = "Trend of Variable over Time") + theme_bw() 
  
  g4 <- ggplot(df, aes(x = "", y = value)) + geom_boxplot() + labs(title = "Summary Statistics of Variable", x ="") + theme_bw()
  
  figure <- ggarrange(g2, g1, g3, g4,
                      #labels = c("A", "B", "C", "D"),
                      ncol = 2, nrow = 2)
  return(figure)
}



summary_plots <- function(df){
  
  df <- df %>% drop_na()
  
  fit.weibull <- fitdist(df$value, "weibull")
  fit.norm <- fitdist(df$value, "norm")
  fit.uniform <- fitdist(df$value, "unif")
  fit.exp <- fitdist(df$value, "exp")
  fit.lnorm <- fitdist(df$value, "lnorm")
  fit.gamma <- fitdist(df$value, "gamma")
  fit.logistic <- fitdist(df$value, "logis")
  
  df_plot <- data.frame(
    dist = c("normal", "uniform", "exponential", " weibull", "lognormal", "gamma", "logistic"),
    aic = c(fit.norm$aic, fit.uniform$aic, fit.exp$aic,  fit.weibull$aic, fit.lnorm$aic, fit.gamma$aic, fit.logistic$aic)
  ) %>% arrange(aic) %>% mutate(aic = round(aic,1)) 
  
  sel_df <- df_plot[1,1]
  
  return(
    
    if(sel_df == "lognormal"){
      fit.lnorm <- fitdist(df$value, "lnorm")
      plot(fit.lnorm)
    } else if(sel_df == "normal"){
      fit.norm <- fitdist(df$value, "norm")
      plot(fit.norm)
    } else if(sel_df == "unif"){
      fit.unif <- fitdist(df$value, "unif")
      plot(fit.unif)
    }else if(sel_df == "exponential"){
      fit.exp <- fitdist(df$value, "exp")
      plot(fit.exp)
    }else if(sel_df == "gamma"){
      fit.gamma <- fitdist(df$value, "gamma")
      plot(fit.gamma)
    } else{
      fit.logistic <- fitdist(df$value, "logis")
      plot(fit.logistic)
    }
  )
  
  #return(plots)
}



