library(future)
library(tidyqwi)
library(purrr)


year <- c("2010", "2020")
tracts <- c("09007541600",
            "09007541700",
            "09006802000")

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
                     processing = "multiprocess"))