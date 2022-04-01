

#####################################################Annual net return functions#################################################
#.Theere are three functions which are 
#1) NPB_prop_fl: estimates annual net returns of cultivated crops with intact wetlands
#2) NPB_prop_flwl: estimates annual net returns of cultivated crops with drained wetland  areas
#3) simulated_npv: estimates uncertainties around annual net returns of cultivated crops
source("totalcost.R")


 #________________________________________________________________Net-Present value___________________________________
#1. Annual Net Returns (with intact wetland areas)

NPB_prop_fl <- function(crop_price, yield,  yield_wl, proportion_cultivated, production_cost, farm_acre, farm_acre_wl, wetland_acre, delayed_seeding, 
                        nuisance_cost, discount_rate, planning_horizon, af) {
  
  af = (discount_rate*(1+discount_rate)^planning_horizon/((1+discount_rate)^planning_horizon - 1)) 
  
  pv = 0
  
  for(t in 0:planning_horizon){
 
    disc_profit = (((crop_price * yield*(proportion_cultivated*farm_acre)) - nuisance_cost)       # Total production revenue net nuisance cost
                   - proportion_cultivated*production_cost*farm_acre)*(1/(1+discount_rate)^t)     # Total production cost
    
    pv = pv + disc_profit                                                                         # sums the discounted profit over the planning horizon

  }
  
  annual_nr = ((pv)/farm_acre_wl) * af                                                            # annualized net returns with the annualized factor (af)
  return(annual_nr)                                                                               # returns the annualized returns
}


#2. Annual Net Returns (with drained wetland areas)
      
NPB_prop_flwl <- function(crop_price, yield,  yield_wl, proportion_cultivated, production_cost, farm_acre, farm_acre_wl, wetland_acre, delayed_seeding, 
                        nuisance_cost, discount_rate, planning_horizon, drainage_cost, af) {
  
  af = (discount_rate*(1+discount_rate)^planning_horizon/((1+discount_rate)^planning_horizon - 1)) 
  
  pv = 0
  
  for(t in 0:planning_horizon){
    
      discounted_profit = ((((crop_price * (yield)*(proportion_cultivated*farm_acre))                # production revenue of cultivated crops on upland area
                   + delayed_seeding*(crop_price*(yield_wl)*(proportion_cultivated*wetland_acre)))   # production revenue of cultivated crops on drained wetland area
                   - (proportion_cultivated*production_cost*farm_acre                                # production cost of cultivated crops on upland area
                   + delayed_seeding*proportion_cultivated*production_cost*wetland_acre))            # production cost of cultivated crops on drained wetland area
                   * (1/(1+discount_rate)^t)) 
    
    pv = pv + discounted_profit
    
  }
  
  annual_nr = ((pv - drainage_cost*wetland_acre)/farm_acre_wl)*af
  return(annual_nr)
  }


#3. Uncertainties around annual net returns

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
    
    seed = 1
    
    # The variables created below(such as drainage cost) allows this function to estimate uncertainties. Also, it could allow it to turn off uncertainties on specified variables
    
      df <- df %>% dplyr::mutate(
        
        drainage_cost = dplyr::if_else(drainagecostsimulation1 ==1, unlist(pmap_dbl(list(n=1, dr_cost*0.3, dr_cost*1.3, dr_cost),rtriangle)), dr_cost),
        
        yield_sim = case_when(
          crop == "Canola" ~ round(rlnorm(n=1, 3.15, 0.35), 2),
          crop == "Feed Barley" ~ round(runif(n=1 , 16.5, 75.9), 2),
          crop == "Malt Barley" ~ round(rweibull(n=1 , 16.5, 75.9), 2),
          crop == "Yellow Peas" ~ round(runif(n=1 , 10, 47.1), 2),  
          crop == "Spring Wheat" ~ round(rgamma(n=1, shape= 5.14, rate=0.18), 2),
          crop == "Oat" ~ round(runif(n=1, 20, 95.1), 2),
          crop == "Fallow" ~ 0
        ),
        yield = dplyr::if_else(yieldsimulation1==1, yield_sim, avg_yield),
        
        yield_wl = yd_diff +  yield,
        crop_price_sim = case_when(
          crop == "Canola" ~ round(rgamma(n=1, shape=32, rate=2), 2),  
          crop == "Feed Barley" ~ round(runif(n=1, 3.8, 8.6), 2),
          crop == "Malt Barley" ~ round(runif(n=1, 3.8, 8.6), 2),
          crop == "Yellow Peas" ~ round(runif(n=1, 5.3, 12.4), 2),
          crop == "Spring Wheat" ~ round(runif(n=1, 7.4, 10.2), 2),
          crop == "Oat" ~ round(runif(n=1, 3.7, 8.7), 2),
          crop == "Fallow" ~ 0
        ),
        crop_price = dplyr::if_else(pricesimulation1==1, crop_price_sim, avg_crop_price),
        production_cost = dplyr::if_else(productioncostsimulation1==1, unlist(pmap_dbl(list(n=1, prod_cost*0.7, prod_cost*1.7, prod_cost),rtriangle)), prod_cost),
        
  # The NPB_prop_fl is used to estimate the annual net returns (with intact wetland areas) and also estimate the uncertainties around for certain variables if needed              
  npv_fl = round(unlist(pmap_dbl(list(crop_price, yield,  yield_wl, proportion_cultivated, production_cost, farm_acre,
                                        farm_acre_wl, wetland_acre, delayed_seeding, 
                                        nuisance_cost, discount_rate, planning_horizon),NPB_prop_fl)),2),

 # The NPB_prop_flwl is used to estimate the annual net returns (with drained wetland areas) and also estimate the uncertainties around for certain variables if needed              
 npv_flwl = round(unlist(pmap_dbl(list(crop_price, yield,  yield_wl, proportion_cultivated, production_cost, farm_acre, 
                                                                      farm_acre_wl, wetland_acre, delayed_seeding, 
                                                                      nuisance_cost, discount_rate, planning_horizon, drainage_cost),NPB_prop_flwl)),2),
 
 # We estimate the difference between the annual net returns of fields with drained wetland areas and fields with intact wetland areas. It is also wetland conservation cost            
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
  
  #simulated data which can be downloaded
  anpb_sim_df <- data.frame(npv_fl = npv_sim_fl1, npv_flwl = npv_sim_flwl1, npv_wl = npv_sim_wl1, wetlandarea = wetlandacre_sim1, cropprice = crop_price_sim1, 
                            cropyield = yield_sim1, productioncost = production_cost_sim1,drainagecost = drainage_cost_sim1)
  
  return(anpb_sim_df)
  
}      

      #__________________________________________________End of functions________________________________________________________________________________