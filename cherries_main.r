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


## Notes on selection -----

## Regina (god smak, blomning 4, zon 3, mognar sent), S1S3
## Kombinera med:
## Sam (S2S4), dock ej bra på colt
## Margit? (S4S12, god smak, blomning 3 (2?), mognar tidigt)
## Sunburst (S3S4', blomning 4, mognar sent), delvis kompatibel

## ## Heidi has different haplotypes in Lacis2008 and Schuster2007
## Lacis: Allmän Gulröd (S2S3) x Heinrichs Riesen (S3S4), S4S6
## Schuster: S1S3
## Note: None of these compatible with suggested origin
## Nybom 2012 anger som föräldrar Allmän gulröd bigarrå x Büttners Rote Späte (=Heinrichs Riesen) (S3S4)
## "För sötkörsbären noterades att Heidi placerades nära sin ena förälder, Büttners Rote Späte. " "För Heidi stämmer härstamningen (Allmän gulröd bigarrå x Büttners Rote Späte) om föräldern var samma klon av Allmän gulröd bigarrå som återfinns på Brunstorp. Provet av Allmän gulröd bigarrå som erhölls från Elitplantstationen stämmer däremot inte alls med Heidi i tre av sex undersökta loci" " För söt- körsbäret Heidi noterades samma profil för två av träden (Balsgård och Capellagården) medan det tredje (på Elitplantstationen) visade helt andra värden i ett locus"

## minegentradgard.se uppger som pollinatörer Merton Glory (S4S6) och Stella (S3S4')
## E-planta uppger skuggmorell, merton glory (S4S6), merton premier (S2S3)

cols <- c("variety", "var", "genotype") ## , "genotype"
variety_genotype_group[grepl("hedel", tolower(variety)), ..cols]
variety_genotype_group[grepl("s6", tolower(genotype)), ..cols]$var



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
