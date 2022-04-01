
################################# replicate number fields
library(tidyverse)
source("totalcost.R")
set.seeed(1)
fieldData <- read_csv("landsape_cropdata11.csv")                                   # Read the crop production information guide data
landscape_df <- function(df, num){
  out = NULL
  for (i in 1:nrow(df)){
    a <- df[i,]
    b <- a[, "proportion"]
    d <- as.integer(b*num)
    e <- a %>% slice(rep(1:n(), each = d)) # This is where we can add the information on yield and 
    n_row <- nrow(e)
    f <- e %>% dplyr::mutate(
      yield = case_when(
        crop == "Canola" ~ round(rgamma(n=n_row , shape=7.34, rate=0.11), 2),  #"Feed Barley", "Malt Barley", "Yellow Peas", "Soybean", "Canola", "Corn", "Flax", "Spring Wheat", "Oats", "Fallow"
        crop == "Feed Barley" ~ round(rweibull(n=n_row , shape=2.53, scale =80), 2),
        crop == "Malt Barley" ~ round(rweibull(n=n_row , shape=2.53, scale =80), 2),
        crop == "Yellow Peas" ~ round(runif(n=n_row , 0.7*avg_yield, 1.3*avg_yield), 2),  
        crop == "Soybean" ~ round(runif(n_row , 20, 50), 2), #i can use a normal distribution 
        crop == "Corn" ~ round(runif(n=n_row , 50, 130), 2), #corn no data use normal
        crop == "Flax" ~ round(rweibull(n=n_row , shape=2.18, scale = 35), 2),
        crop == "Spring Wheat" ~ round(rweibull(n=n_row , shape= 2.48, scale=80), 2),
        crop == "Oat" ~ round(rgamma(n=n_row , shape=4.59, rate=0.03), 2),
        crop == "Fallow" ~ 0
      ),
      crop_price = case_when(
        crop == "Canola" ~ round(rgamma(n=n_row , shape=10.5, rate=0.91), 2),  
        crop == "Feed Barley" ~ round(rgamma(n=n_row , shape=7.54, rate=1.65), 2),
        crop == "Malt Barley" ~ round(rgamma(n=n_row , shape=7.54, rate=1.65), 2),
        crop == "Yellow Peas" ~ round(rlnorm(n_row , 1.85, 0.30), 2),
        crop == "Soybean" ~ round(rgamma(n=n_row , shape=15.34, rate=1.34), 2),
        crop == "Corn" ~ round(runif(n=n_row , 0.7*avg_crop_price, 1.3*avg_crop_price), 2),                            #no data
        crop == "Flax" ~ round(rlnorm(n=n_row , 2.33, 0.42), 2),
        crop == "Spring Wheat" ~ round(rlnorm(n=n_row , 2, 0.13), 2),
        crop == "Oat" ~ round(rgamma(n=n_row , shape=8.52, rate=1.86), 2),
        crop == "Fallow" ~ 0
      ) # production cost? problem is with the inpt$ 
      
      )

    out = rbind(out, f)
  }
  return(out)
}

