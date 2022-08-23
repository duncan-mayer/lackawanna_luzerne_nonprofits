library(tidyverse)

bmf_20 <- read.csv("/Volumes/TOSHIBA4tb/nccs_data/bmfs/bmf.bm2020.4.csv")

'%!in%' <- function(x,y)!('%in%'(x,y))

# bring in bmfs 2010-2016
keep_vars <- c("ein", "fips","nteecc","filer","zfiler","name","address",
               "city", "zip5","state","ruledate",
               "level1","level2","level3","nteefinal","cassets","ctotrev", "year")

## cleaning function 1
lower_year <-  function(bmf1, y) {
  names(bmf1) <- tolower(names(bmf1))
  bmf1$year <- y
  bmf1$city <- tolower(bmf1$city)
  bmf1 <- bmf1[bmf$fips %in% c("42079","42069") & is.na(bmf$fips) == FALSE,]
  return(bmf1[keep_vars])
}


bmf1 <- lower_year(bmf1 = bmf_20, y = 2020)
dim(bmf1)
numb <- 0:99
suff <- c("ST", "ND", "RD", "TH")

## remove floor when the order is "number"-suffix "floor" 
for (j in seq_along(suff))   {
  for (i in seq_along(numb)) {
    bmf1$address1 <- gsub(
      paste0(numb[[i]], suff[[j]], " ", "FL"),
      "", bmf1$address)
  }
}

## some have: "floor" "number" no suffix 
for (i in seq_along(numb)) {
  bmf1$address1 <- gsub(
    paste0("FL"," ", numb[[i]]),
    "", bmf1$address1)
}
bmf1 <- bmf1 %>% mutate(ctotrev = ifelse(ctotrev < 0, 0, ctotrev),
                                      ctotrev = ifelse(cassets < 0, 0, cassets))


bmf1$address_full <- paste0(bmf1$address1, ", ",bmf1$city, ",", " PA", ", " , bmf1$zip5, ", USA")
