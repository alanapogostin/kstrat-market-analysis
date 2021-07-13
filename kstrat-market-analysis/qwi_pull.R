library(future)
library(tidyqwi)
library(purrr)


year <- c()
tracts <- c()

argList <- list(x = tracts, y = year) # Update

arguments <- cross_df(argList) #Update

plan("multiprocess")

qwi_data <- map2(arguments$x, arguments$y, ~
                   get_qwi(
                     tracts = .x,
                     years  = .y ,
                     industry_level = "2",
                     all_groups = FALSE,
                     endpoint = "se",
                     geography = "cbsa",
                     processing = "multiprocess",
                     apikey = APIkey))