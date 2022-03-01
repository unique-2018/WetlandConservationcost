

totalcost <- function(df, soil, croptype){
  cost <- df %>% dplyr::mutate(totalcost = other_variable_cost+fixed_cost++ (fertilizer_nitrogen+phosphorus+sulphur_others+herbicides+insecticides+fungicides)) 
  cost <- cost %>% filter(crop %in% croptype & soil_zone %in% soil) %>% select(totalcost) %>% pluck(1)
  return(cost)
}

