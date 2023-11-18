## Synonyms -----

genotype_syn <- variety_genotype_group

## Add manually entered synonyms from cherries_table.csv
cherries_table <- fread("cherries_table.csv")
tmp <- cherries_table[, .(var, label_syn)]
genotype_syn <- tmp[genotype_syn, on = "var"]
genotype_syn[, syn := paste0(syn, ", ", label_syn)]
genotype_syn[, label_syn := NULL]
genotype_syn[, syn := gsub(", NA$", "", syn)]
genotype_syn[, syn := stringr::str_trim(syn)]
genotype_syn[, syn := gsub("^, ", "", syn)]
genotype_syn[, syn := gsub("^,$", "", syn)]

## fix spelling errors
genotype_syn[, variety := gsub("Schneider Späte Knorpelkirsche", "Schneiders Späte Knorpelkirsche", variety)] 
genotype_syn[, syn := gsub("Schneider Späte Knorpelkirsche", "Schneiders Späte Knorpelkirsche", syn)] 

## ad label to syn2
genotype_syn[ , syn2 := paste0(syn, ", ", label)]
genotype_syn[ , syn2 := gsub("^, ", "", syn2)]

## Find candidates with multiple synonyms
test_dupl <- paste0(genotype_syn[, syn2], collapse = ", ")
test_dupl <- stringr::str_split(test_dupl, ",|,, ")[[1]]
test_dupl <- gsub("^ ", "", test_dupl)
dupl_syn <- unique(test_dupl[duplicated(test_dupl)]) ## these have one or more duplicates

## explore
cols <- c("var", "variety", "syn", "genotype", "label") ## , "genotype"
## genotype_syn[grepl("schne", tolower(variety)), ..cols]

## aggregate those with the same name, adding label to syn
genotype_syn[, var2 := var]

## schneiders ---------
select_syn <- genotype_syn[grepl("schne", tolower(variety)), ..cols][, var]
select_syn <- paste0(select_syn, collapse = "|")

## genotype_syn[grepl(select_syn, var), syn2 := paste0(syn, ", ", label)]

## check
cols <- c(cols, "syn2", "var2")
genotype_syn[grepl(select_syn, var), ..cols]

## manually enter var name
genotype_syn[grepl(select_syn, var), var2 := "schneiders_spate_knorpelkirsche"]

## collapse synonyms
myfun <- Vectorize(paste0)
genotype_syn[grepl(select_syn, var), syn2 := myfun(syn2, collapse = ", ")]
genotype_syn[ , syn2 := gsub("^, ", "", syn2)]

## mark for deletion
genotype_syn[, var_keep := TRUE]
genotype_syn[grepl(select_syn, var), var_keep := grepl(paste0(cherries_table$var, collapse = "|"), var)]

## check
## cols <- c(cols, "var_keep")
## genotype_syn[grepl(select_syn, var), ..cols]
## genotype_syn[1:4, ..cols]

genotype_syn[grepl("emperor|kaiser", tolower(syn2)), ..cols]
## ## Note: Kaiser franz exists with different haplotype
##  genotype                           label
## 1:      S3S4                 Emperor Francis
## 2:      S1S2               Emperor Francis B
## 3: S2S4/S3S4                    Kaiser Franz
## todo: remove from synonyms of schneiders?

genotype_syn[grepl("anno", tolower(syn2)), ..cols]

genotype_syn[grepl("schnei", tolower(variety)), ..cols]

## 5:    S3S12                     


## Misc ---------

## Kaiser Franz
## genotype_syn[grepl("emperor|kaiser", tolower(syn2)), ..cols]
## Note: Kaiser Franz finns med andra haplotyper, tas därför bort som synonym för Schneiders:
genotype_syn[grepl("schneider", tolower(var2)), syn := gsub(", Kaiser Franz", "", syn)]
genotype_syn[grepl("schneider", tolower(var2)), syn2 := gsub(", Kaiser Franz", "", syn)]

## Lapins aka Cherokee, https://www.gardenfocused.co.uk/fruitarticles/cherry/variety-lapins-cherokee.php
genotype_syn[grepl("lapin", tolower(syn2)), ..cols]
genotype_syn[grepl("napoleon", tolower(syn2)), ..cols]

## ## Ferrovia
## ## Iblan syn för Schneiders Späte
## genotype_syn[grepl("schneiders|ferrovia", tolower(syn2)), ..cols]


## Long list of cherries with synonyms: http://www.chathamapples.com/CherriesNY/TCNYIndex.htm#INDEX
## Annonayer Herzkirsche (syn. of Annonay),
## Annonay: A Heart cherry mentioned in 1882 as a promising new fruit because of its extreme earliness and excellent quality. This variety, introduced by Thomas Rivers & Son, Sawbridgeworth, England, should not be confused with an older French sort often known by the same name but of a reddish-brown color. 
## Guigne d'Annonay (syn. of Guinge la Plus Hative)
## Bigarreau Empereur-Francois (syn. of Emperor Francis)
## Kaiser Franz Josef (syn. of Emperor Francis)
## Figur/tabell mognad: https://www.graeb.com/sortiment/suesskirschen/reifezeittabelle/

## Buttners späte rote ---------

## ## explore
## cols <- c("var", "variety", "syn", "genotype", "label") ## , "genotype"
## genotype_syn[grepl("späte rote|heinr|altem|büttner", tolower(syn2)), ..cols]

select_syn <- genotype_syn[grepl("späte rote|heinr|altem|büttner", tolower(variety)), ..cols][, var]
select_syn <- paste0(select_syn, collapse = "|")

## ## check
## cols <- c(cols, "syn2", "var2")
## genotype_syn[grepl(select_syn, var), ..cols]

## manually enter var name
genotype_syn[grepl(select_syn, var), var2 := "buttners_spate_rote_knorpelkirsche"]

## collapse synonyms
myfun <- Vectorize(paste0)
genotype_syn[grepl(select_syn, var), syn2 := myfun(syn2, collapse = ", ")]
genotype_syn[ , syn2 := gsub("^, ", "", syn2)]

## mark for deletion
genotype_syn[, var_keep := TRUE]
genotype_syn[grepl(select_syn, var), var_keep := grepl(paste0(cherries_table$var, collapse = "|"), var)]

## ## check
## cols <- c(cols, "var_keep")
## genotype_syn[grepl(select_syn, var), ..cols]
## genotype_syn[1:4, ..cols]

## ## Next -------

## ## explore
## cols <- c("var", "variety", "syn", "genotype", "label", "syn2", "var2") ## , "genotype"
## genotype_syn[grepl("grolls", tolower(syn2)), ..cols]
## dupl_syn ## listar de som har flera synonymer
## ## Todo: Remove duplicates in syn2

## Checked those in cherries_table

## Keep only one per aggregated group ---------

genotype_syn <- genotype_syn[var_keep == TRUE, ]

genotype_syn <- genotype_syn[var %in% cherries_table$var, ] ## keep only those in cherries_table

## tidy
genotype_syn[, syn2 := gsub(",,", ",", syn2)]
genotype_syn[, syn := syn2]
genotype_syn[, label2 := label]

genotype_syn <- genotype_syn[, .(var, genotype, syn)]
## genotype_syn[, label := gsub(" \\\n", "", label)]
genotype_syn <- genotype_syn[cherries_table, on = "var"] ## add those from cherries_table not in genotype data
## genotype_syn[is.na(label), ]
genotype_syn <- genotype_syn[, .(var, label, syn, syn_note)]
## prominent_features

write.csv(genotype_syn, "genotype_syn.csv", row.names = FALSE)
