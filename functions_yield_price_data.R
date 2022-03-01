

yield_df <- function(df, province){
  
  crops <- c("Barley","Canola","Corn","Flaxseed","Oats","Peas","Soybeans","Wheat" )
  
  for (i in crops){
    
    yield %>% dplyr::filter(Geography == province) %>% dplyr::select(year, i)  %>% write_csv(paste("data/yield/", i, "_yield_", province, ".csv"))
    
  }
  
}


price_df <- function(df, province){
  
  df <- df %>% drop_na()
  
  crops <- c("Wheat (except durum wheat), milling" , "Wheat","Dry peas [114314]" , "Flaxseed [115122111]",
             "Canola (including rapeseed) [113111]", "Corn for grain [1151111]","Soybeans [1151211]",
             "Barley [1151141]", "Oats [115113111]")
  
  prairies <- c("Manitoba","Saskatchewan","Alberta" )
  
  pr_df <- df %>% dplyr::filter(`Farm products` %in% crops & province == province) %>% dplyr::select(year, province, `Farm products`, value)
  
  for (i in crops){
    
    prices_df <- pr_df %>% dplyr::filter(`Farm products` == i ) %>% dplyr::select(year, province, `Farm products`, value) %>% write_csv(paste("data/price/",i,province,".csv"))
    
  }
  
}


