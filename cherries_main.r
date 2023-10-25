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

## some duplicates in tempvar:
## variety_genotype_group[grepl("Büttners Späte Rote Knorpelkirsche", variety)]
## Note: Süßkirsche Büttner`s Rote Knorpelkirsche Alte Sorte mit Tradition, auch "Altenburger Melonenkirsche" genannt.


## replace var in other datasets so it matches genotype data -----
lookfor <- paste0(dta$label, collapse = "|")
tmp <- variety_genotype_group[grepl(lookfor, variety), .(tempvar, variety)]
## Note: Some duplicated variety
dta$var[!dta$var %in% tmp$tempvar] ## no match in tempvar
nomatch <- dta$var[!dta$var %in% tmp$tempvar] ## no match in tempvar
sour <-  c("berit","fanal","kelleris","kirsa","ostheimer","skuggmorell","stora_klarbar", "triaux", "tschernokorka","nordia", "lettisk_lag") ## remove sour
nomatch <- nomatch[!nomatch %in% sour]

varnames_tmp <- c(
'guigne_dannonay' = "annonay",
'allm_gulrod' = "allman_gulrod",
'altenburger_melonen_kirsche' = "buttners_rote",
'buttners_spate_rote_knorpelkirsche' = "buttners_rote",
'fryksaas' = "fryksas",
'gaardebo' = "gardebo",
'grosse_schwarze_knorpel' = "stor_svart",
'donissens_gelbe_knorpel' = "donissen",
'frogmore_early' = "frogmore"
)
## reverse
varnames_new <- names(varnames_tmp)
names(varnames_new) <- unname(varnames_tmp)

varnames_tmp

## substitute names in source .csv files
filenames_temp <- c(
    "cherries_table.csv",
"cherries_wexthuset.csv",
"cherries_rangedala.csv",
"cherries_splendor.csv"
)
## i <- "cherries_table.csv"
for(i in filenames_temp){
out <- fread(i)
## out[["var"]]
## names(query_label(out[["var"]], varnames_tmp))
## query_label(out[["var"]], varnames_tmp)
## unname(query_label(out$var, varnames_new))
out[["var"]] <- query_label(out[["var"]], varnames_tmp)
write.csv(out, i)
}
#### Note: Not Working



variety_genotype_group[grepl("", tempvar), ]
variety_genotype_group[grepl("Rote Knorpel", variety), ]


dta$var[!dta$var %in% tmp$tempvar] ## no match in tempvar
variety_genotype_group[grepl("", variety), ]

## Note:
## kauffs_kirsche  # samma som 'Knauff's Schwarze'? (den enda som hittas av google när man söker på Knauff)? ändrade till var knauffs_schwarze i den tidigare egna listan

## Note: Does not exist in variety_genotype_group due to being sour cherries


## Note: change in this list in cherries_pollination_sv.r

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
