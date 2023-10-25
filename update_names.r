
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

## rename the sanitized tempvar to var for matching with dta
variety_genotype_group[, var := tempvar]
variety_genotype_group[, tempvar := NULL]

##################### This part is redundant but keep the commented code #####
## replace var in other datasets so it matches genotype data 
## updated .csv source files
## ## look
## lookfor <- paste0(dta$label, collapse = "|")
## tmp <- variety_genotype_group[grepl(lookfor, variety), .(tempvar, variety)]

## ## test which ones has no match
## nomatch <- dta$var[!dta$var %in% tmp$tempvar] ## no match in sanit
## ## names of sour cherries
## sour <-  c("berit","fanal","kelleris","kirsa","ostheimer","skuggmorell","stora_klarbar", "triaux", "tschernokorka","nordia", "lettisk_lag") ## remove sour
## nomatch <- nomatch[!nomatch %in% sour]

## ## renaming list
## ## Note: change varnames for recoding plantagen in cherries_pollination_sv.r if any more changes to varname is done
## varnames_tmp <- c(
## 'guigne_dannonay' = "annonay",
## 'allman_gulrod' = "allm_gulrod",
## 'buttners_spate_rote_knorpelkirsche' = "buttners_rote",
## 'fryksaas' = "fryksas",
## 'gaardebo' = "gardebo",
## ## 'grosse_schwarze_knorpel' = "grosse_schwarze",
## 'grosse_schwarze_knorpel' = "stor_svart", ## olika enl wexthuset
## 'donissens_gelbe_knorpel' = "donissen",
## 'frogmore_early' = "frogmore"
## )
## ## reverse
## varnames_new <- names(varnames_tmp)
## names(varnames_new) <- unname(varnames_tmp)
## ## query_label(nomatch, varnames_new) ## test


## ## ###### substitute names in source .csv files (KEEP) ######
## ## filenames_temp <- c(
## ##     "cherries_table.csv",
## ## "cherries_wexthuset.csv",
## ## "cherries_rangedala.csv",
## ## "cherries_splendor.csv"
## ## )
## ## ## i <- "cherries_table.csv"
## ## for(i in filenames_temp){
## ## out <- fread(i)
## ## out[["var"]] <- query_label(out[["var"]], varnames_new)
## ##     message(paste0("replacing", i))
## ## write.csv(out, i, row.names = FALSE)
## ## }
## ####################################################


## ## check again if all names in dta is in genotype data

## ## remove var for sour cherries
## tmp <- dta[type != "sour", var]
## tmp <- tmp[!tmp %in% sour]

## tmp[!tmp %in% variety_genotype_group$tempvar] ## Should be 0 ie all dta$varvar is also in variety_genotype_group$var






## variety_genotype_group[grepl("Große Schwarze Knorpel", variety), ]
## variety_genotype_group[grepl("", variety), ]
