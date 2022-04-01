library(tidyverse)
wl_policies <- read.csv("data/peimer.csv") %>% filter(ï.. != "Country") %>%
  rename(country = ï..,
         wl_policy = X,
         signatory_ramsar = X.1,
         num_designated_wl_interimportance = X.2,
         wise_use = X.3,
         no_net_loss_gain = X.4,
         biodiversity_goal = X.5,
         ess_goal = X.6,
         nationa_wl_inventory = X.7,
         compensatory_mitigation = X.8,
         use_penalties = X.9,
         use_incentives = X.10)

wl_policies <- wl_policies[!apply(wl_policies == "", 1, all), ]                # Remove rows with only empty cells

unique(wl_policies$wl_policy)
wl_policies <- wl_policies %>%
  mutate(has_wl = ifelse(wl_policy == "A wetland-specific policy is in place to protect all wetlands" | wl_policy == "A wetland-specific policy is in place", 1, 0),
         some_laws_protwl = ifelse(wl_policy == "A broad environmental policy protects most wetlands indirectly" | wl_policy == "A broad environmental policy protects wetlands indirectly"
                                   | wl_policy == "Some types of wetlands are protected under specific laws" | 
                                     wl_policy == "Integrated wetland strategy or plan, without the force of law", 1,0),
         no_policy = ifelse(wl_policy== "No policy"| wl_policy == "A policy is promised or \"in development\"", 1, 0)
  )

Perc_wlpolicy = 100*(sum(wl_policies$has_wl)/nrow(wl_policies))
some_laws = 100*(sum(wl_policies$some_laws_protwl)/nrow(wl_policies))
no_laws = 100*(sum(wl_policies$no_policy)/nrow(wl_policies))

no_net_los <- wl_policies %>% filter(has_wl == 1) %>% mutate(nnlg = ifelse(no_net_loss_gain == "Yes", 1, 0))

Perc_nnlg = 100*(sum(wl_policies$nnlg)/nrow(no_net_los))


unique(no_net_los$country)
view(wl_policies)
colnames(wl_policies)
