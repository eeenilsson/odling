## Genotypes data--------------------------------------------

## functions
pacman::p_load(data.table)

## ## faster version /S2S4'
## genotype_a <- "S4S8"
## genotype_b <- "S4'S6"
## genotype_a <- "S4S3/S9S3"
## genotype_b <- "S1S3/S4"
## compat(genotype_a, genotype_b)
## compat("S4S8", "S1S3")
## genotype_a <- "S1S6S26S36a" ## test
## genotype_a <- "S6S13ʹS26S36a" ## skuggmorell surkörsbär
## genotype_b <- "S1S6"
## compat(genotype_a, genotype_b)

compat <- function(genotype_a, genotype_b){
    ## calculate relative compatibility
    ## If uncertain genotype (ie genotype separated with "/", the worst case scenario will be used)
    pollinator <- paste0(strsplit(genotype_a, "/")[[1]], collapse = "")
    target <- paste0(strsplit(genotype_b, "/")[[1]], collapse = "")
    pollinator <- strsplit(pollinator, "S")[[1]][-1]
    target <- strsplit(target, "S")[[1]][-1]

    ## check if terraploid (sour cherry)
    tetraploid_pol <- ifelse(!grepl("/", genotype_a) & length(pollinator) == 4, TRUE, FALSE)
    tetraploid_target <- ifelse(!grepl("/", genotype_b) & length(target) == 4, TRUE, FALSE)
    ## Note: Man har identifierat 12 fungerande S-haplotyper (S1, S4, S6, S9, S12, S13, S14, S16 ,S26, S33, S34, och S35) och nio icke-fungerande (S1ʹ, S6m, S6m2, S13ʹ, S13m, S36a , S36b, S36b2, och S36b3). Vissa finns även hos sörkörsbär (S1, S4, S6, S9, S12, S13, S 14, S16, och S34). The presence of two or more non-functional S-haplotypes within sour cherry 2x pollen renders that pollen SC.
    mutants <- c("ʹ$|'$|^6m$|^6m2$|^13m$|^36a$|^36b$|^36b2$|^36b3$")
    target <- target[!grepl(mutants, target)] ## remove mutated S-alleles (does not confer GSI)
    check <- !grepl(paste0("^", paste0(target, collapse = "$|^"), "$"), pollinator)
    sucess <- pmax(check, grepl("ʹ$|'$", pollinator)) ## count as success if "'"
    success_alt1 <- sucess[c(TRUE, TRUE, FALSE, FALSE)]
    success_alt2 <- sucess[!c(TRUE, TRUE, FALSE, FALSE)] ## NA if length 2
    comp <- sum(success_alt1, na.rm = TRUE)
    if(grepl("/", genotype_a)){
        ## if uncertain genotype (/) use worst case
        comp <- min(sum(success_alt1, na.rm = TRUE), sum(success_alt2, na.rm = TRUE), na.rm = TRUE)
    }
    if(tetraploid_pol){
        ## mutants <- c("'", "6m", "6m2", "13m", "36a", "36b", "36b2", "36b3")
        p_mut <- pollinator[grepl(mutants, pollinator)]
        p_nonmut <- pollinator[!grepl(mutants, pollinator)]
        check_nonmut <- !grepl(paste0("^", paste0(target, collapse = "$|^"), "$"), p_nonmut)
        p_nonmut_compat <- p_nonmut[check_nonmut]
        p_compat <- c(p_mut, p_nonmut_compat) ## mut or absent in target
        success_probability <- 0
        if(length(p_compat) == 2){success_probability <- 2/8}
        if(length(p_compat) == 3){success_probability <- 4/8}
        if(length(p_compat) == 4){success_probability <- 8/8}
        ## abcd => ab ac ad bc bd cd but this is not the case in meiosis, see youtube:
        ## see https://www.youtube.com/watch?v=NttqS-N17FQ
        ## possible number of gametes = 2^n        
        ## where n is the haploid number (2n=4 => diploid nr=4, haplod nr=2)
        ##  sour cherry is an allotetraploid (2n = 4x = 32)
        ## if there is 1 mutant the probability of a sperm to contain 2 mutants is 0.
        ## 2 mutants => probability of pollen to contain 2 mutants = 2/8, 3 m => 4/8, 4m => 8/8
        comp <- ifelse(success_probability >0, ceiling(success_probability+0.1), 0)
    }

    return(comp)        
    ## Undantaget är S4**’**-allelen (notera apostrofen) som medför _självfertilitet_. Ett pollenkorn med S4**’** kan befrukta alla mottagare (inklusive de med S4**’**). Körsbär med S4**’** kan således betraktas som universella givare.
    ## De enstaka själv-**in**fertila sorter som har S4**’** ej kan befruktas av S4 (utan apostrof).
## Enstaka universella givare saknar också S4**’**.
    ## Enstaka universella givare saknar också S4**’**.
    ## S3' =  SC
    ## S5' =  SC
}


## load data files --------------------

variety_genotype_group <- fread("cherries_variety_genotype_group.csv", header = TRUE)

## curate
source("update_names.r") ## adds a column with sanitized varnames

## look
## variety_genotype_group[grepl("0", incompatibility_group) , ]
## variety_genotype_group[grepl("Lapins", variety), .(variety, genotype, incompatibility_group, mother, father)]
## variety_genotype_group[grepl("Lapins|Van|Stella", variety), .(variety, genotype, incompatibility_group, mother, father)]

## Observations
## some duplicates in Variety:
## variety_genotype_group[grepl("Büttners Späte Rote Knorpelkirsche", variety)]
## Note: Süßkirsche Büttner`s Rote Knorpelkirsche Alte Sorte mit Tradition, auch "Altenburger Melonenkirsche" genannt.

## Note: Some from dta Does not exist in variety_genotype_group due to being sour cherries

## Metadata ###############################

### Data from: Self-incompatibility (S) genotypes of cultivated sweet cherries – An overview update 2020 Mirko Schuster Julius Kühn-Institut (JKI)

### In the new update from October 2020

### From abstract: a total of 63 incompatibility groups in 1483 sweet cherries have been defined. 26 sweet cherries have a unique combination of S alleles and were described as universal donors and placed in the incompatibility group 0. Additionally, there exist 91 self-compatible sweet cherries, called group SC. A total of 22 different S alleles are described in the cultivated sweet cherries up to date. These are the S alleles S1, S2, S3, S4, S5, S6, S7, S9, S10, S12, S13, S14, S16, S17, S18, S19, S21, S22, S24, S27, S30, S37.

## Some varieties appear on more than one row with different names (synonyms probably)

## Variable descriptions

### Country : according ISO 3166 Code list

### Incompatibiliy_group : S4'|any => Self compatible (SC). Some have a trailing "?".

##############################################

## ## Inkompatibilitetsgrupper lista på kategorier #############
## ## Note: SC and 0 missing from groups list
## incompatibility_groups <- fread("cherry_incompatibility_groups_2020.csv") 
## incompatibility_groups[, s_alleles := gsub(" ", "", s_alleles)] ## squish
## ## note: Not needed, all groups in incompatibility_groups are in variety_genotype_group
## Note: Not al SC have S4
## ## Note: All S4' are SC (Self-compatible)
###############################

variety_genotype_group[, incompatibility_group := gsub(" ", "", incompatibility_group)] ## SC has a trailing space, sqish
## variety_genotype_group[incompatibility_group == "", ]
## ## Note: these have questionmark, uncommon varieties all
variety_genotype_group[, genotype := gsub(" ", "", genotype)]

## setkey(variety_genotype_group, var)

## variety_genotype_group[, unique(incompatibility_group)]
## variety_genotype_group[, unique(genotype)]

## print(variety_genotype_group[, unique(genotype)])

dupl_var <- variety_genotype_group[duplicated(var), var]
## variety_genotype_group[grepl(paste0(dupl_var, collapse = "$|^"), var), .(var, genotype, variety, incompatibility_group)]
## Note: some have two or more duplicated rows (n = 90 varieties), these all have different genotypes in different studies, ie uncertain

## Add sour cherries with known genotype ---------
## Skuggmorell anges till haplotyp S6 S13ʹ S26 S36a.
## Syn : Schattenmorelle (Prunus cerasus subsp. acida), auch Große Lange Lotkirsche, Nordkirsche, ‘Łutówka’, Griotte de Nord, Chatel Morel, Morello.

tmp <- data.table(
    variety = "Skuggmorell (Schattenmorelle, Große Lange Lotkirsche, Nordkirsche, Łutówka, Griotte de Nord, Chatel Morel, English Morello)",
    genotype = "S6S13ʹS26S36a",
    incompatibility_group = "SC",
    origin_country = "DE",
    mother = NA,
    father = NA,
    reference = NA,
    var = "skuggmorell"
)

variety_genotype_group <- rbind(variety_genotype_group,
      tmp)

tmp <- data.table( ## Note: BT missing so will be excluded from plot
    variety = "Tschernokorka",
    genotype = "S9S13S35S36b2",
    incompatibility_group = "SI",
    origin_country = "RU",
    mother = NA,
    father = NA,
    reference = NA,
    var = "tschernokorka"
)

variety_genotype_group <- rbind(variety_genotype_group,
      tmp)

## extract synonyms
source("../functions/removeParens.r")
myfun <- Vectorize(extractParens)
variety_genotype_group[, syn := myfun(variety)]
variety_genotype_group[, syn := gsub(", $", "", syn)]
## collapse syn for duplicated var
variety_genotype_group[grepl(paste0(dupl_var, collapse = "|"), var),  syn := paste0(syn, collapse = ", "), by = "var"]

## add "/" for duplicated then remove duplicates
variety_genotype_group[grepl(paste0(dupl_var, collapse = "|"), var),  genotype := paste0(genotype, collapse = "/"), by = "var"]
variety_genotype_group[grepl(paste0(dupl_var, collapse = "|"), var),  incompatibility_group := paste0(incompatibility_group, collapse = "/"), by = "var"]
variety_genotype_group[grepl(paste0(dupl_var, collapse = "|"), var),  reference := paste0(reference, collapse = ", "), by = "var"]
variety_genotype_group[grepl(paste0(dupl_var, collapse = "|"), var),  reference := paste0(reference, collapse = ", "), by = "var"]
variety_genotype_group <- variety_genotype_group[!duplicated(var), ]
## eg:
## "S1S6/S3S4"
## "S1S4'/S3S4'"
## "S1S6/S4S6"
variety_genotype_group[, syn := gsub(", $", "", syn)]
variety_genotype_group[, syn := gsub("^,", "", syn)]
## todo: check synonyms and merge those which are the same
## variety_genotype_group[syn != "", syn]

## sanitize labels
variety_genotype_group[, label := variety]
variety_genotype_group[, label := gsub("\\([^$]*", "", label)]
variety_genotype_group[, label := gsub("TM[^$]*", "", label)]
variety_genotype_group[, label := gsub(" $", "", label)]
variety_genotype_group[, label := gsub(" \\\n", "", label)]
## variety_genotype_group[, unique(label)]

## redundant:
## ## remove variants with duplicated rows and different genotypes
## variety_genotype_group <- variety_genotype_group[!grepl(paste0(dupl_var, collapse = "|"), var), ]

## calculate relative compatibility
## check
## variety_genotype_group[, unique(genotype)]
## variety_genotype_group[, unique(incompatibility_group)]
## variety_genotype_group[grepl("S3\\'", genotype), ] ## SC
## variety_genotype_group[grepl("S5\\'", genotype), ] ## SC
## variety_genotype_group[grepl("\\/", genotype), ] ## no incomp gr
## strsplit("S3S12", "S")[[1]][2:3]
## strsplit("S3'S12", "S")[[1]][2:3]
## strsplit("S6S17/30?", "S")[[1]][2:3]

## ## split, redundant:
## variety_genotype_group[, genotype1 := sapply(genotype, function(x){strsplit(x, "S")[[1]][2]})]
## variety_genotype_group[, genotype2 := sapply(genotype, function(x){strsplit(x, "S")[[1]][3]})]

## ## explore
## variety_genotype_group[genotype1 == genotype2, ]
## ## Note: All three are S3S3 and two of these noted ad SC.

## variety_genotype_group[incompatibility_group == "SC", ]
## ## Note: with very few exceptions SC are 4'

## variety_genotype_group[genotype == "S3S3", ]
## ## Note: 2 of 3 S3S3 are SC

## variety_genotype_group[genotype1 == "4'" | genotype2 == "4'", ]
## ## All S4' are SC

####### todo
## See also: functional_genotypes_compatibility_groups.csv
## Note: Check if they correspond
#####

## ## restrict to varieties of interest for test
## anfic_bt$var[anfic_bt$var %in% unique(dta$var)]
## dta$var[unique(dta$var) %in% unique(anfic_bt$var)]
## dta$var[!unique(dta$var) %in% unique(anfic_bt$var)]
## anfic_bt[grepl("eri", label) , ]
## restr <- anfic_bt$var[anfic_bt$var %in% unique(dta$var)]
## restr_anfic <- anfic_bt[grepl(paste0(restr, collapse = "|"), var), ]

## restr <- pollination_groups_test$var[pollination_groups_test$var %in% unique(dta$var)]
## restr_uk <- pollination_groups_test[grepl(paste0(restr, collapse = "|"), var), ]

## select vars --------------------------------------------
## use all var that have blooming group data, see phenology.r
## swed <- fread("cherries_table.csv")
selectvars <- dta[type == "sweet" & available == 1, unique(var)] ## n = 23
selectvars <- c(selectvars, "skuggmorell", "tschernokorka")
## add pollinators (this will include sour cherries):
## tmp <- dta[type == "sweet", paste(pollinated_by_concordance_chr, collapse = ", ")]
## tmp <- gsub("NA \\([^,]*,", "", tmp)
## tmp <- gsub(" \\([^,]*\\)", "", tmp)
## tmp <- gsub("  ", " ", tmp)
## tmp <- gsub(", NA", "", tmp)
## selectvars <- c(selectvars, strsplit(tmp, ", ")[[1]])

## tmp <- c("celeste", "napoleon") ## just to add some more
## selectvars <- c(selectvars, tmp)

selectvars <- unique(selectvars)
bg_selected <- blooming_group_aggr[grepl(paste0(selectvars, collapse = "$|^"), var), ] ## select those in selectvar (var listed in dta)

## ## write cvs with those missing bt
## tmp <- data.frame(
##     var = dta$var,
##     bt_missing = !dta$var %in% bg_selected$var,
##     bt_google_sv = NA
## )
## write.csv(tmp, "google_bt_sverige.csv", row.names = FALSE)

## add these even if bt is unknown
tmp <- c("fryksaas") ## bt missing for , "erianne"
## sour: ostheimer, nordia, fanal, kelleris, kirsa, berit, mfl

tmp <- tmp[!tmp %in% bg_selected[, var]] ## those not already selected
if(length(tmp > 0)){
lookfor <- ifelse(length(tmp > 1),
                  paste0(tmp, collapse = "$|"),
                  tmp)
tmp <- variety_genotype_group[grepl(paste0(tmp, collapse = "$|"), var), .(var)]
tmp[, bgr := 99] ## use 99 for thos w unknown bgr
bg_selected <- rbind(bg_selected,
      tmp
      )
}

## todo: schneiders_spate_knorpelkirsche syn nordwunder mfl

## blooming_group_aggr

## ## explore
## blooming_group_aggr[grepl("tar", var), ]
cols <- c("variety", "var", "genotype") ## , "genotype"
variety_genotype_group[grepl("schne", tolower(variety)), ..cols]

## tmp <- variety_genotype_group[grepl(paste0(paste0(notselected, collapse = "|")), tolower(variety)), ..cols]
## print(tmp[, .(var)], n = 200)
## bg_selected[bg_selected$var %in% variety_genotype_group$var, ]
## notselected <- blooming_group_aggr[!grepl(paste0(selectvars, collapse = "$|^"), var), var] ## in bgr but not selected

bg_selected <- bg_selected[bg_selected$var %in% variety_genotype_group$var, ] ## skip those not matching var name in genotype data

dta_toplot <- variety_genotype_group[bg_selected, on = "var"] ## all selected have genotype data matching var name

bt_wide <- dta_toplot[, .(var, genotype, bgr, incompatibility_group)]

## make a df to add cols
newcols <- as.data.table(matrix(nrow = nrow(bt_wide), ncol = nrow(bt_wide)+1))
names(newcols) <- c("var", bt_wide$var)
cols <- names(newcols)
newcols_mod <- newcols[ , (cols) := lapply(.SD, as.character), .SDcols = cols]
newcols_mod$var <- bt_wide$var
bt_wide <- newcols_mod[bt_wide, on = "var"]
for(i in 1:nrow(bt_wide)){ ## loop over rows
    for(varn in bt_wide$var){ ## loop over cols
        ## get genotype of column name
             ## bt_wide[i, (varn) := bt_wide[var == varn, genotype]]  
        ## get compatibility value of column name
                bt_wide[i, (varn) := compat(bt_wide[var == varn, genotype],
                                         bt_wide[i, genotype])  ]  
            }
}

## bt_wide
## str(bt_wide)

## melt
dtplot <-  melt(bt_wide, id.vars = c("var", "genotype", "bgr",  "incompatibility_group"))
dtplot[, value := as.numeric(value)]

## fix names
names(dtplot) <- c("target", "genotype", "bgr", "incompatibility_group", "pollinator", "compatibility")

## add blooming time for **pollinator**
tmp <- bg_selected[var %in% dtplot$pollinator, .(var, bgr)]
names(tmp) <- c("pollinator", "pollinator_blooming_group")
dtplot <- dtplot[tmp, on = "pollinator"]

## [target == "areko", ]
## dtplot[target == "areko", ]
## ?data.table
## X[Y, on=c(x1="y1", x2="y2")]

## calculate blooming proximity
dtplot[, proximity := abs(as.numeric(bgr) - as.numeric(pollinator_blooming_group))]
dtplot[, proximity := ifelse(bgr == 99 & as.numeric(pollinator_blooming_group) == 99, 99, proximity)] ## if both unknown
dtplot[, compat_proximity := "no"]
## dtplot[, proximity]
dtplot[proximity <0.51 & compatibility != 0, compat_proximity := "same"]
dtplot[proximity >0.5 & proximity < 1.1 & compatibility != 0, compat_proximity := "close"]
dtplot[proximity >10 & compatibility != 0, compat_proximity := "bt_unknown"]
## dtplot[proximity >10, ]
## dtplot[, unique(proximity)]

## Variable type
## dtplot[, compatibility := as.numeric(compatibility)]
## str(dtplot)
dtplot[, target:= as.factor(target)]
dtplot[, pollinator:= as.factor(pollinator)]
dtplot[, pollinator_blooming_group_num := as.numeric(pollinator_blooming_group)]
dtplot[, blooming_group := as.numeric(bgr)]

## myfun <- function(x){
##     ## round to bg most distant from 3, eg 3.5 is rounded to 4, ie towards the extremes
##     ## to reduce the number of bg to five
##     out <- c()
##     if(x > 3){out <- ceiling(x)}
##     if(x == 3){out <- round(x, digits = 0)}
##     if(x < 3){out <- floor(x)}
##     return(out)
## }
## myfun <- Vectorize(myfun)

dtplot[, blooming_group := round(blooming_group, digits = 0)]
dtplot[, blooming_group := factor(blooming_group, ordered = TRUE, levels = c(1:5, 99), labels = c("Tidig", "Medeltidig", "Medel", "Medelsen", "Sen", "Okänd"))]
dtplot[, pollinator_blooming_group := round(pollinator_blooming_group_num, digits = 0)]
dtplot[, pollinator_blooming_group := factor(pollinator_blooming_group, ordered = TRUE, c(1:5, 99), labels = c("Tidig", "Medeltidig", "Medel", "Medelsen", "Sen", "Okänd"))]
## dtplot[, unique(pollinator_blooming_group)]

## dtplot[, pollinator_blooming_group_num]

setkey(dtplot, target)
## str(dtplot)

############## here #####################

## ## If genotype is missing, look here:
## Identification of self-incompatibility (S) alleles in Latvian
## and Swedish sweet cherry genetic resources collections by
## PCR based typing Gunars Lacis Æ Edite Kaufmane Æ Isaak Rashal Æ
## Viktor Trajkovski Æ Amy F. Iezzoni
## SOme summary data: https://pub.epsilon.slu.se/2393/1/Acta_Thesis-Lacis-final.pdf

## Identification of self-incompatibility ( S ) alleles in Latvian and Swedish sweet cherry genetic resources collections by PCR based typing Lacis 2008a
## Also Lisek2017.pdf


## Sour cherries ------

## https://www.tandfonline.com/doi/abs/10.1080/14620316.2017.1289071
## Identification of S-haplotypes of European cultivars of sour cherry Anna Lisek
## "The tested cultivars were found to contain 15 S-haplotypes: S1, S1ʹ, S4, S6, S6m, S6m2, S9, S12, S13, S13ʹ, S26, S35, S36a, S36b, and S36b2. The most frequently occurring S-haplotypes were S13ʹ (61.9%), S36a (57.1%), and S26 (47.6%). On the basis of the results, 17 of the 21 cultivars were deduced to be self-compatible." pdf på dropbox
## Mest tyska och polska sorter. Skuggmorell anges till S6 S13ʹ S26 S36a

## SLU: Four sour cherry cultivars (‘Kirsa’, ‘Ljubskaya’, ‘Nordia’ and ‘Oblachinska’) were present in both collections and had completely identical allele profiles

## " Genetic and molecular characterization of three novel S-haplotypes in sour cherry (Prunus cerasus L.)."
## "s sour cherry selections can be either self-compatible (SC) ora self-incompatible (SI), polyploidy per se does not result in SC. Instead the genotype-dependent loss of SI in sour cherry is due to the accumulation of non-functional S-haplotypes. The presence of two or more non-functional S-haplotypes within sour cherry 2x pollen renders that pollen SC."
## Tatsuya Tsukamoto 2008


## The S-genotypes of many sweet and sour cherry cultivars have been published and cultivars are assigned to incompatibility groups (Schuster 2012, Schuster 2018, Sebolt et al. 2017)

## Shuster 2014, Physicochemical characterization of fruit quality traits in a German sour cherry collection
## https://doi.org/10.1016/j.scienta.2014.09.047

## Sebolt S -genotyping of cultivars and breeding selections of sour cherry ( Prunus cerasus L.) in the Michigan State University sour cherry breeding program May 2017 Acta Horticulturae DOI:10.17660/ActaHortic.2017.1161.5
## Sebolt2017.pdf

## Identification of bloom date QTLs and haplotype analysis in tetraploid sour cherry (Prunus cerasus) Lichun Cai 2017 ## EJ svenska sorter

## -----------------


## dtplot[grepl("samba|frisco", target), ]

## dta[ , paste(var, ": ", pollinated_by_concordance_chr)]



## variety_genotype_group[grepl("", tempvar), ]
## variety_genotype_group[grepl("Rote Knorpel", variety), ]

## variety_genotype_group[grepl("Große Schwarze Knorpel", variety), ]
## variety_genotype_group[grepl("", variety), ]


## names(variety_genotype_group)
## variety_genotype_group[, .(incompatibility_group)]
## unique(variety_genotype_group$incompatibility_group)

## Explore S4'
## variety_genotype_group[grepl("S4'", genotype) , ]
## ## Note: All S4' are SC (Self-compatible)
## sc <- variety_genotype_group[grepl("SC", incompatibility_group) , ]
## sc[grepl("S4'", genotype), .(variety, genotype, incompatibility_group)]
## sc[grepl("S4'", genotype) == FALSE, .(variety, genotype, incompatibility_group)]

## explore
## variety_genotype_group[grepl("0", incompatibility_group) , ]

## variety_genotype_group[grepl("Lapins", variety), .(variety, genotype, incompatibility_group, mother, father)]

## variety_genotype_group[grepl("Lapins|Van|Stella", variety), .(variety, genotype, incompatibility_group, mother, father)]

## Rosbreed har en xls med S-gruppe också: https://www.rosbreed.org/breeding/dna-tests/cherry/cross-compatibility


