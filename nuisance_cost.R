
#________Nuisance cost calculation

 # Read data the has information on input cost (seed, seed treatment, fertilizer use, and pesticide use)
# and machinery operating cost (machinery fuel and repairs)


nuisance_cost <- function(df){
  
  ioc_select <- df %>% 
      mutate(
      nuisance_factor = case_when(                                             # nuisance factors are selected (from Cortus et al. (2011) Table 3) based on number of wetlands
            (num_wl >= 1 & num_wl <=3) ~ 0.08,
            (num_wl >  3 & num_wl <=6) ~ 0.09,
            (num_wl >= 6 & num_wl <=9) ~ 0.11,
            num_wl > 9 ~ 0.14
    ),
    input_waste_factor = iwf,                                                #input waste factor are assumed to be 10% of input cost as farmer applies inputs around wetland areas
    nuisance_cost_mach = nuisance_factor * machinery_oc,                        # nuisance cost associated with machinery operation: nuisance factor times machinery operating cost
    nuisance_cost_input = input_waste_factor * input_cost,                   # nuisance cost associated with input wastage (overlap): input waste factor times input cost
    Efficiency_loss = round(nuisance_cost_mach + nuisance_cost_input,0)) %>% dplyr::select(Efficiency_loss)  # Efficiency loss is the sum of the two above
  
  return(ioc_select)
  
}


