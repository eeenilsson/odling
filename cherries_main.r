## Main script for cherries


source('cherries_pollination_sv.r') ## Pollinatörer enl svenska hemsidor
## dta
names(dta)

dta$label

objects()

## Phenology --------------
source('cherries_phenology.r') ## Rosbreed



## Compatibility ----------------------
source('cherry_compatibility.r')  ## S-allele data
## Note: Not al SC have S4
## ## Note: All S4' are SC (Self-compatible)

## look
## variety_genotype_group[grepl("0", incompatibility_group) , ]
## variety_genotype_group[grepl("Lapins", variety), .(variety, genotype, incompatibility_group, mother, father)]
## variety_genotype_group[grepl("Lapins|Van|Stella", variety), .(variety, genotype, incompatibility_group, mother, father)]


## add sanitized var ------

variety_genotype_group[, tempvar := variety] ## create temp var
variety_genotype_group[, tempvar := gsub("\n", "", tempvar)] ## remove newline char
variety_genotype_group[, tempvar := gsub("\\.", "", tempvar)] ## remove 
variety_genotype_group[, tempvar := gsub(" / ", "_", tempvar)] ## replace / w _
variety_genotype_group[, tempvar := gsub(" - ", "", tempvar)] ## replace / w _
variety_genotype_group[, tempvar := gsub("\\(.*", "", tempvar)] ## remove parens
## stringi foreign characters
custom_rules <- "å > aa;
                 ø > oe;
                 ::Latin-ASCII;"
variety_genotype_group[, tempvar := stringi::stri_trans_general(tempvar, id = custom_rules, rules = TRUE)]
## TM
variety_genotype_group[, tempvar := gsub("TM$", "", tempvar)]
variety_genotype_group[, tempvar := gsub("TM", "", tempvar)]
variety_genotype_group[, tempvar := stringr::str_trim(tempvar)] ## tolower
variety_genotype_group[, tempvar := tolower(tempvar)] ## internal ws to underscore
variety_genotype_group[, tempvar := gsub(" ", "_", tempvar)]

## clean
variety_genotype_group[, tempvar := gsub("'", "", tempvar)]
variety_genotype_group[, tempvar := gsub("\\(r\\)", "", tempvar)]
variety_genotype_group[, tempvar := gsub("-", "_", tempvar)]

## check
## variety_genotype_group[tempvar == "cherry_grant", .(tempvar, variety)]
## variety_genotype_group[, .(tempvar, variety)]
## paste(variety_genotype_group$tempvar, collapse = ",  ")


lookfor <- paste0(dta$label, collapse = "|")
tmp <- variety_genotype_group[grepl(lookfor, variety), .(tempvar, variety)]
## Note: Some duplicated variety

## change to matching var
variety_genotype_group[, tempvar := gsub("guigne_dannonay", "annonay", tempvar)]
variety_genotype_group[, tempvar := gsub("allman_gulrod", "allman_gulrod", tempvar)]
variety_genotype_group[, tempvar := gsub("altenburger_melonen_kirsche", "buttners_rote", tempvar)]
variety_genotype_group[, tempvar := gsub("buttners_spate_rote_knorpelkirsche", "buttners_rote", tempvar)]
variety_genotype_group[, tempvar := gsub("fryksaas", "fryksas", tempvar)]
variety_genotype_group[, tempvar := gsub("gaardebo", "gardebo", tempvar)]


dta$var[!dta$var %in% tmp$tempvar] ## no match in tempvar

variety_genotype_group[grepl("Nord", variety), ]

## Does not exist in variety_genotype_group:
## berit
## fanal : sour
## frogmore
## kelleris : sour
## kirsa
## kauffs_kirsche  # felnämnd?
## nordia : sour


x

## Nice historical info: http://www.bernwodeplants.co.uk/descriptions/cherry2.htm

## add existing labels
dta$label
dta$var

varnames <- c(
    'buttners_rote' = "Büttners Späte Rote",
    'allm_gulrod' = "Allm. gulröd bigarrå",
    'hedelfinger' = "Hedelfinger",
    'merton_glory' = "Merton Glory",
    'sam' = "Sam",
    'stor_svart' = "Stor svart bigarrå",
    'van' = "Van",
    'almore' = "Almore",
    'stella' = "Stella",
    'sjalvfertil'= "Självfertil",
    'allm_gulrod' = "Allmän gulröd bigarrå",
    'victor' = "Victor",
    'heidi' = "Heidi",
    'buttners_rote' = " Büttners Späte Rote",
    'kordia' = "Kordia",
    'lapins' = "Lapins"
)

## replace names
## see : cherries_functions.r
x <- cbind(replace_name(dta$pollinated_by_plantagen, varnames),
      dta$pollinated_by_plantagen)
## dta$temp <- replace_name(dta$pollinated_by_plantagen, varnames)




## Quarto -----

source("cherries_quarto.r")




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
