
#yield <- read_csv("data/yield.csv")
yield_df <- function(df, province){
  
  crops <- c("Barley","Canola","Corn","Flaxseed","Oats","Peas","Soybeans","Wheat" )
  
  for (i in crops){
    
    yield %>% filter(Geography == province)  %>% dplyr::select(year, i)  %>% write_csv(paste("data/yield/", i, "_yield_", province, ".csv"))
    
  }
  
}



#prices <- read_csv("data/prices.csv")

price_df <- function(df, province){
  
  crops <- c("Wheat (except durum wheat) [1121111]", "Dry peas [114314]", "Flaxseed [115122111]", "Canola (including rapeseed) [113111]",
             "Corn for grain [1151111]", "Soybeans [1151211]", "Barley [1151141]", "Oats [115113111]")
  prairies <- c("Manitoba","Saskatchewan","Alberta" )
  
  selected_crops <- prices %>% dplyr::filter(`Farm products` %in% crops & GEO == province) %>% dplyr::select(REF_DATE, GEO, `Farm products`, VALUE)
  
  for (i in crops){
    
    prices <- selected_crops %>% dplyr::filter(`Farm products` == i) %>% dplyr::group_by(REF_DATE) %>% dplyr::select(REF_DATE, `Farm products`, VALUE) %>% 
      write_csv(paste("data/price/", i, "yield_", province, ".csv"))
    
  }
  
}


price_dist_fit <- function(df){
  
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
  
  g1 <- ggplot(df_plot, aes(x = reorder(dist, aic), y = aic), fill=dist) + 
    geom_bar(stat="identity") +
    geom_text(aes(label=aic), vjust=1.5, colour="white", size=3.5) + theme_bw() 
  
  return(g1)
}


price_summary_plots <- function(df){
  
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