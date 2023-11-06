## Main script for cherries

source('cherries_pollination_sv.r') ## Pollinatörer enl svenska hemsidor
## dta

## dta$var

## Phenology --------------
source('cherries_phenology.r') ## Rosbreed

## Compatibility ----------------------
source('cherry_compatibility.r')  ## read S-allele data
source('synonyms.r')  ## aggregate some synonyms

## Summary --------------------
source('cherries_summary.r')

## Quarto -----

source("cherries_quarto.r")
## Magit push elswhere github/main in minibuffer



## Redundant, testing -------------

## ## test sanitize
## testvar <- c("Sweet AryanaTM (PA1UNIBO)",
## "StardustTM (13N-07-70)",
## "Srdcovka přeúrodná",
## "Spraultm (Früh)",
## "Souvenir des Charmes \n(St. Charmes, Big. Moreau)",
## "Souvenir des Charmes \n (ig. Moreau)"
## )
## test <- as.data.table(testvar)
## names(test) <- "var" ## Replace newline char
## test[, tempvar := var]
## test[, tempvar := gsub("\n", "", var)] ## remove parens
## test[, tempvar := gsub("\\(.*", "", tempvar)] ## stringi foreign characters
## custom_rules <- "å > aa;
##                  ø > oe;
##                  ::Latin-ASCII;"
## test[, tempvar := stringi::stri_trans_general(tempvar, id = custom_rules, rules = TRUE)] ## TM
## test[, tempvar := gsub("TM$", "", tempvar)] ## leading and trailing ws
## test[, tempvar := gsub("TM ", "", tempvar)]
## test[, tempvar := stringr::str_trim(tempvar)]  ## tolower
## test[, tempvar := tolower(tempvar)]  ## internal ws to underscore
## test[, tempvar := gsub(" ", "_", tempvar)]

## https://stackoverflow.com/questions/17517319/r-replacing-foreign-characters-in-a-string
## Define custom rules for å and ø, otherwise transliterate according to Latin-ASCII
## custom_rules <- "å > aa;
##                  ø > oe;
##                  ::Latin-ASCII;"
## stringi::stri_trans_general(c("Tårnby", "Søborg"), id = custom_rules, rules = TRUE)
