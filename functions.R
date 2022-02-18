
 #________________________________________________________________Net-Present value___________________________________
      #0nly wetland
NPB_prop_fl <- function(crop_price, yield,  yield_wl, proportion_cultivated, production_cost, farm_acre, farm_acre_wl, wetland_acre, delayed_seeding, 
                        nuisance_cost, discount_rate, planning_horizon, af) {
  
  af = (discount_rate*(1+discount_rate)^planning_horizon/((1+discount_rate)^planning_horizon - 1)) 
  
  pv = 0
  
  for(t in 0:planning_horizon){
 
    disc_profit = (((crop_price * yield*(proportion_cultivated*farm_acre)) - nuisance_cost) - proportion_cultivated*production_cost*farm_acre)*(1/(1+discount_rate)^t)
    
    pv = pv + disc_profit 

  }
  
  annual_nr = ((pv)/farm_acre_wl) * af
  return(annual_nr)
}


# With Wetlands
      
NPB_prop_flwl <- function(crop_price, yield,  yield_wl, proportion_cultivated, production_cost, farm_acre, farm_acre_wl, wetland_acre, delayed_seeding, 
                        nuisance_cost, discount_rate, planning_horizon, drainage_cost, af) {
  
  af = (discount_rate*(1+discount_rate)^planning_horizon/((1+discount_rate)^planning_horizon - 1)) 
  
  pv = 0
  
  for(t in 0:planning_horizon){
    
      discounted_profit = ((((crop_price * (yield)*(proportion_cultivated*farm_acre)) 
                   + delayed_seeding*(crop_price*(yield_wl)*(proportion_cultivated*wetland_acre))) 
                   - (proportion_cultivated*production_cost*farm_acre  
                   + delayed_seeding*proportion_cultivated*production_cost*wetland_acre))
                   * (1/(1+discount_rate)^t))
    
    pv = pv + discounted_profit
    
  }
  
  annual_nr = ((pv - drainage_cost*wetland_acre)/farm_acre_wl)*af
  return(annual_nr)
  }


  simulated_npv <- function(df) { 
  sim = df[1, "simulations"] %>% pluck(1)
  
  npv_sim_fl <- list()
  npv_sim_flwl <- list()
  npv_sim_wl <- list()
  crop_price_sim <- list()
  yield_sim <- list()
  production_cost_sim <- list()
  drainage_cost_sim <- list ()
  wetlandacre_sim <- list()
  
  for (i in 1:sim) {
    
    seed = 100
    
    df <- df %>% mutate(drainage_cost = dplyr::if_else(drainagecostsimulation ==1, unlist(pmap_dbl(list(n=1, min_drainage_cost, max_drainage_cost, dr_cost),rtriangle)), dr_cost),
                        yield = dplyr::if_else(yieldsimulation==1, unlist(pmap_dbl(list(n=1, min_yield, max_yield, avg_yield),rtriangle)), avg_yield),
                        yield_wl = yd_diff +  dplyr::if_else(yieldsimulation==1, unlist(pmap_dbl(list(n=1, min_yield, max_yield, avg_yield),rtriangle)), avg_yield),
                        production_cost = dplyr::if_else(productioncostsimulation==1, unlist(pmap_dbl(list(n=1, min_prod_cost, max_prod_cost, prod_cost),rtriangle)), prod_cost),
                        crop_price = dplyr::if_else(pricesimulation==1, unlist(pmap_dbl(list(n=1, min_crop_price, max_crop_price, avg_crop_price),rtriangle)), avg_crop_price),
                        
      npv_fl = round(unlist(pmap_dbl(list(crop_price, yield,  yield_wl, proportion_cultivated, production_cost, farm_acre,
                                            farm_acre_wl, wetland_acre, delayed_seeding, 
                                            nuisance_cost, discount_rate, planning_horizon),NPB_prop_fl)),2),
      
      npv_flwl = round(unlist(pmap_dbl(list(crop_price, yield,  yield_wl, proportion_cultivated, production_cost, farm_acre, 
                                                                          farm_acre_wl, wetland_acre, delayed_seeding, 
                                                                          nuisance_cost, discount_rate, planning_horizon, drainage_cost),NPB_prop_flwl)),2),
      npv_wl = npv_flwl - npv_fl) 
    
    sum_npv_fl <- sum(df$npv_fl) 
    sum_npv_flwl <- sum(df$npv_flwl) 
    sum_npv_wl <- sum(df$npv_wl) 
    mean_cropprice <- sum(df$crop_price)
    mean_yield <- sum(df$yield)
    mean_production_cost <- sum(df$production_cost)
    mean_dc <- mean(df$drainage_cost)
    mean_wl <- mean(df$wetland_acre)
    
    
    npv_sim_fl[[i]] <- sum_npv_fl
    npv_sim_flwl[[i]] <- sum_npv_flwl
    npv_sim_wl[[i]] <- sum_npv_wl
    wetlandacre_sim[[i]] <- mean_wl
    
    crop_price_sim[[i]] <- mean_cropprice
    yield_sim[[i]] <- mean_yield
    production_cost_sim[[i]] <- mean_production_cost
    drainage_cost_sim[[i]] <- mean_dc
 
}
  
  npv_sim_fl1 = as.numeric(unlist(npv_sim_fl))
  npv_sim_flwl1 = as.numeric(unlist(npv_sim_flwl))
  npv_sim_wl1 = as.numeric(unlist(npv_sim_wl))
  crop_price_sim1 = as.numeric(unlist(crop_price_sim))
  yield_sim1 = as.numeric(unlist(yield_sim))
  production_cost_sim1 = as.numeric(unlist(production_cost_sim))
  drainage_cost_sim1 = as.numeric(unlist(drainage_cost_sim))
  wetlandacre_sim1 = as.numeric(unlist(wetlandacre_sim))
  
  anpb_sim_df <- data.frame(npv_fl = npv_sim_fl1, npv_flwl = npv_sim_flwl1, npv_wl = npv_sim_wl1, wetlandarea = wetlandacre_sim1, cropprice = crop_price_sim1, 
                            cropyield = yield_sim1, productioncost = production_cost_sim1,drainagecost = drainage_cost_sim1)
  
  return(anpb_sim_df)
  
}      

      #__________________________________________________End of functions________________________________________________________________________________