

#####################################################Annual net return functions#################################################
#.Theere are three functions which are 
#1) NPB_prop_fl: estimates annual net returns of cultivated crops with intact wetlands
#2) NPB_prop_flwl: estimates annual net returns of cultivated crops with drained wetland  areas
#3) simulated_npv: estimates uncertainties around annual net returns of cultivated crops
set.seed(1)
#________________________________________________________________Net-Present value___________________________________
#1. Annual Net Returns (with intact wetland areas)

NPB_prop_fl_ls <- function(crop_price, yield,production_cost, nuisance_cost, farm_acre, farm_acre_wl, 
                           discount_rate, planning_horizon) { #, discount_rate, planning_horizon, af
  
  af = (discount_rate*(1+discount_rate)^planning_horizon/((1+discount_rate)^planning_horizon - 1)) 
  
  pv = 0
  
  for(t in 0:planning_horizon){
    
    disc_profit = ((crop_price * yield*(farm_acre) - nuisance_cost)       # Total production revenue net nuisance cost
                   - production_cost*farm_acre)*(1/(1+discount_rate)^t)     # Total production cost
    
    pv = pv + disc_profit                                                                         # sums the discounted profit over the planning horizon
    
  }
  
  annual_nr = ((pv)/farm_acre_wl)* af                                                            # annualized net returns with the annualized factor (af)
  return(annual_nr)                                                                               # returns the annualized returns
}


#2. Annual Net Returns (with drained wetland areas)

NPB_prop_flwl_ls <- function(crop_price, yield,  yield_wl, production_cost, farm_acre, farm_acre_wl, wetland_acre, delayed_seeding, 
                          drainage_cost,  discount_rate, planning_horizon) {
  
  af = (discount_rate*(1+discount_rate)^planning_horizon/((1+discount_rate)^planning_horizon - 1)) 
  
  pv = 0
  
  for(t in 0:planning_horizon){
    
    discounted_profit = ((((crop_price * (yield)*(farm_acre)) + delayed_seeding*(crop_price*(yield_wl)*(wetland_acre)))   # production revenue of cultivated crops on drained wetland area
                          - (production_cost*farm_acre                                 # production cost of cultivated crops on upland area
                          + delayed_seeding*production_cost*wetland_acre))*(1/(1+discount_rate)^t))   # production cost of cultivated crops on drained wetland area
                         
    
    pv = pv + discounted_profit
    
  }
  
  annual_nr = ((pv - drainage_cost*wetland_acre)/farm_acre_wl)*af
  return(annual_nr)
}

#__________________________________________________End of functions________________________________________________________________________________