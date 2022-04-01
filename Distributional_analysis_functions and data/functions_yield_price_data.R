

yield_df <- function(df, province){
  
  crops <- c("Barley","Canola","Corn","Flaxseed","Oats","Peas","Soybeans","Wheat" )
  
  for (i in crops){
    
    yield %>% dplyr::filter(Geography == province) %>% dplyr::select(year, i)  %>% write_csv(paste("data/yield/", i, "_yield_", province, ".csv"))
    
  }
  
}

price_df <- function(df, prov){
  
  df <- df %>% drop_na()
  
  crops <- c("Wheat (except durum wheat), other", 'Wheat (except durum wheat), milling', "Dry peas [114314]","Canola (including rapeseed) [113111]",
             "Barley [1151141]", "Oats [115113111]")
  
  #prairies <- c("Manitoba","Saskatchewan","Alberta" )
  
  pr_df <- df %>% dplyr::filter(`Farm products` %in% crops) %>% dplyr::select(year, province, `Farm products`, value)

  for (i in crops){
    
    pr_df %>% dplyr::filter(`Farm products` == i & province == prov) %>%
      dplyr::select(year, province, `Farm products`, value) %>% 
      dplyr::group_by(year) %>% dplyr::summarise(value = mean(value)) %>%
      write_csv(paste("data/price/",i,prov,".csv"))
    
  }
  
}

