

totalcost <- function(df, soil, croptype, province){
  cost <- df %>% dplyr::mutate(totalcost = other_variable_cost + fixed_cost +  fertilizer_cost + herb_insect_fung) 
  cost <- cost %>% dplyr::filter(crop %in% croptype & soil_zone %in% soil & province %in% province) %>% dplyr::select(totalcost) %>% pluck(1)
  return(cost)
}


