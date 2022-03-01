library(tidyverse)
yield <- read_csv("data/yield.csv")
crops <- c("Geography", "year", "Barley","Canola","Corn","Flaxseed","Oats","Peas","Soybeans","Wheat" )
prairies <- c("Manitoba","Saskatchewan","Alberta" )

yield <- yield %>% filter(Geography %in% prairies)
view(yield)
colnames(yield)
yield_sk_wheat <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Wheat ) 
yield_sk_drypeas <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Peas ) 
yield_sk_flax <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Flax ) 
yield_sk_canola <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Canola ) 
yield_sk_soybeans <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Soybeans ) 
yield_sk_barley <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Barley ) 
yield_sk_corn <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Corn ) 
yield_sk_oats <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Oats ) 


prices_mn_wheat <- selected_crops %>% filter(GEO == "Manitoba" & `Farm products` == "Wheat (except durum wheat) [1121111]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)
prices_mn_drypeas <- selected_crops %>% filter(GEO == "Manitoba" & `Farm products` == "Dry peas [114314]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)
prices_mn_flax <- selected_crops %>% filter(GEO == "Manitoba" & `Farm products` == "Flaxseed [115122111]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)
prices_mn_canola <- selected_crops %>% filter(GEO == "Manitoba" & `Farm products` == "Canola (including rapeseed) [113111]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)
prices_mn_soybeans <- selected_crops %>% filter(GEO == "Manitoba" & `Farm products` == "Soybeans [1151211]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)
prices_mn_barley <- selected_crops %>% filter(GEO == "Manitoba" & `Farm products` == "Barley [1151141]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)
prices_mn_corn <- selected_crops %>% filter(GEO == "Manitoba" & `Farm products` == "Corn for grain [1151111]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)
prices_mn_oats <- selected_crops %>% filter(GEO == "Manitoba" & `Farm products` == "Oats [115113111]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)

prices_ab_wheat <- selected_crops %>% filter(GEO == "Alberta" & `Farm products` == "Wheat (except durum wheat) [1121111]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)
prices_ab_drypeas <- selected_crops %>% filter(GEO == "Alberta" & `Farm products` == "Dry peas [114314]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)
prices_ab_flax <- selected_crops %>% filter(GEO == "Alberta" & `Farm products` == "Flaxseed [115122111]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)
prices_ab_canola <- selected_crops %>% filter(GEO == "Alberta" & `Farm products` == "Canola (including rapeseed) [113111]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)
prices_ab_soybeans <- selected_crops %>% filter(GEO == "Alberta" & `Farm products` == "Soybeans [1151211]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)
prices_ab_barley <- selected_crops %>% filter(GEO == "Alberta" & `Farm products` == "Barley [1151141]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)
prices_ab_corn <- selected_crops %>% filter(GEO == "Alberta" & `Farm products` == "Corn for grain [1151111]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)
prices_ab_oats <- selected_crops %>% filter(GEO == "Alberta" & `Farm products` == "Oats [115113111]") %>% group_by(REF_DATE) %>% select(REF_DATE, VALUE)

prices <- read_csv("data/prices.csv")

yield_sk_wheat <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Wheat) 
yield_sk_drypeas <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Peas ) 
yield_sk_flax <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Flax ) 
yield_sk_canola <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Canola ) 
yield_sk_soybeans <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Soybeans ) 
yield_sk_barley <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Barley ) 
yield_sk_corn <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Corn ) 
yield_sk_oats <- yield %>% filter(Geography == "Saskatchewan")  %>% dplyr::select(year, Oats )


yield_df(df=yield, province = "Alberta")







