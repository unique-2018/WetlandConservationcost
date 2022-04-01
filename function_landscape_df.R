
################################# replicate number fields
library(tidyverse)
set.seed(1)

source("totalcost.R")
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
        crop == "Canola" ~ round(rlnorm(n=n_row, 3.15, 0.35), 2),
        crop == "Feed Barley" ~ round(runif(n=n_row , 16.5, 75.9), 2),
        crop == "Malt Barley" ~ round(rweibull(n=n_row , 16.5, 75.9), 2),
        crop == "Yellow Peas" ~ round(runif(n=n_row , 10, 47.1), 2),  
        crop == "Spring Wheat" ~ round(rgamma(n=n_row , shape= 5.14, rate=0.18), 2),
        crop == "Oat" ~ round(runif(n=n_row , 20, 95.1), 2),
        crop == "Fallow" ~ 0
      ),
      crop_price = case_when(
        crop == "Canola" ~ round(rgamma(n=n_row , shape=32, rate=2), 2),  
        crop == "Feed Barley" ~ round(runif(n=n_row , 3.8, 8.6), 2),
        crop == "Malt Barley" ~ round(runif(n=n_row , 3.8, 8.6), 2),
        crop == "Yellow Peas" ~ round(runif(n=n_row , 5.3, 12.4), 2),
        crop == "Spring Wheat" ~ round(runif(n=n_row , 7.4, 10.2), 2),
        crop == "Oat" ~ round(runif(n=n_row , 3.7, 8.7), 2),
        crop == "Fallow" ~ 0
      ) # production cost? problem is with the inpt$ 
      
      )

    out = rbind(out, f)
  }
  return(out)
}


