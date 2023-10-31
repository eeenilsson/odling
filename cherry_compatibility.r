## Genotypes data--------------------------------------------

## functions
pacman::p_load(data.table)

compat <- function(genotype_a, genotype_b){
    ## pollinator
    a1 <-  strsplit(genotype_a, "S")[[1]][2]
    a2 <-  strsplit(genotype_a, "S")[[1]][3]
    ## target
    b1 <-  strsplit(genotype_b, "S")[[1]][2]
    b2 <-  strsplit(genotype_b, "S")[[1]][3]
    ## pollen sucess = TRUE
    p1 <- (a1 != b1 & a1 != b2) | a1 == "4'" | a1 == "3'" | a1 == "5'"  
    p2 <- (a2 != b1 & a2 != b2) | a2 == "4'" | a2 == "3'" | a2 == "5'"
    ## De enstaka själv-**in**fertila sorter som har S4**’** ej kan befruktas av S4 (utan apostrof).
    if(a1 == "4" & b1 == "4'"){p1 <- FALSE}
    if(a2 == "4" & b2 == "4'"){p2 <- FALSE}
    ## Sum
    comp <- sum(p1, p2)
    return(comp)

    ## calculate relative compatibility
    ## Undantaget är S4**’**-allelen (notera apostrofen) som medför _självfertilitet_. Ett pollenkorn med S4**’** kan befrukta alla mottagare (inklusive de med S4**’**). Körsbär med S4**’** kan således betraktas som universella givare.
    ## Enstaka universella givare saknar också S4**’**.
    ## S3' =  SC
    ## S5' =  SC
}

## compat("S1S2", "S1S3")
## compat("S4'S2", "S1S3")
## compat("S3S4'", "S3S4'")

## with uncertain genotypes, separated by "/" calculate worst case scenario
## "S1S6/S3S4"
## "S1S4'/S3S4'"
## "S1S6/S4S6"

genotype_a <- "S1S6/S3S4"
genotype_b <- "S1S2"

compat <- function(genotype_a, genotype_b){
    ## for uncertain genotypes
    if(grepl("/", genotype_a)){ ## eg "S1S6/S3S4"
    genotype_a_alt <- strsplit(genotype_a, "/")[[1]][2]
    genotype_a <- strsplit(genotype_a, "/")[[1]][1]
    }
    if(grepl("/", genotype_b)){
    genotype_b_alt <- strsplit(genotype_b, "/")[[1]][2]
    genotype_b <- strsplit(genotype_b, "/")[[1]][1]
    }
    ## pollinator
    a1 <-  strsplit(genotype_a, "S")[[1]][2]
    a2 <-  strsplit(genotype_a, "S")[[1]][3]
    ## target
    b1 <-  strsplit(genotype_b, "S")[[1]][2]
    b2 <-  strsplit(genotype_b, "S")[[1]][3]
    ## pollen sucess = TRUE
    p1 <- (a1 != b1 & a1 != b2) | a1 == "4'" | a1 == "3'" | a1 == "5'"  
    p2 <- (a2 != b1 & a2 != b2) | a2 == "4'" | a2 == "3'" | a2 == "5'"
    ## De enstaka själv-**in**fertila sorter som har S4**’** ej kan befruktas av S4 (utan apostrof).
    if(a1 == "4" & b1 == "4'"){p1 <- FALSE}
    if(a2 == "4" & b2 == "4'"){p2 <- FALSE}
    ## Sum
    comp <- sum(p1, p2)
    return(comp)

    ## calculate relative compatibility
    ## Undantaget är S4**’**-allelen (notera apostrofen) som medför _självfertilitet_. Ett pollenkorn med S4**’** kan befrukta alla mottagare (inklusive de med S4**’**). Körsbär med S4**’** kan således betraktas som universella givare.
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

## redundant:
## ## remove variants with duplicated rows and different genotypes
## variety_genotype_group <- variety_genotype_group[!grepl(paste0(dupl_var, collapse = "|"), var), ]

## calculate relative compatibility
## Undantaget är S4**’**-allelen (notera apostrofen) som medför _självfertilitet_. Ett pollenkorn med S4**’** kan befrukta alla mottagare (inklusive de med S4**’**). Körsbär med S4**’** kan således betraktas som universella givare.
## De enstaka själv-**in**fertila sorter som har S4**’** ej kan befruktas av S4 (utan apostrof).
## Enstaka universella givare saknar också S4**’**.

## check
## variety_genotype_group[, unique(genotype)]
## variety_genotype_group[, unique(incompatibility_group)]
## variety_genotype_group[grepl("S3\\'", genotype), ] ## SC
## variety_genotype_group[grepl("S5\\'", genotype), ] ## SC
## variety_genotype_group[grepl("\\/", genotype), ] ## no incomp gr
## strsplit("S3S12", "S")[[1]][2:3]
## strsplit("S3'S12", "S")[[1]][2:3]
## strsplit("S6S17/30?", "S")[[1]][2:3]

## split
variety_genotype_group[, genotype1 := sapply(genotype, function(x){strsplit(x, "S")[[1]][2]})]
variety_genotype_group[, genotype2 := sapply(genotype, function(x){strsplit(x, "S")[[1]][3]})]

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

## use all var that have blooming group data, see phenology.r
## swed <- fread("cherries_table.csv")
selectvars <- dta[type == "sweet", unique(var)] ## n = 23
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
bg_selected <- blooming_group_aggr[grepl(paste0(selectvars, collapse = "$|^"), var), ] ## select those in selectvar

tmp <- c("gaardebo", "fryksaas", "berit", "erianne", "fanal", "heidi", "nordia", "ostheimer", "kelleris", "buttners_spate_rote_knorpelkirsche", "knauffs_schwarze") ## bt missing
tmp <- tmp[!tmp %in% bg_selected[, var]]
tmp <- variety_genotype_group[grepl(paste0(tmp, collapse = "$|"), var), .(var)]

## blooming_group_aggr

tmp[, bgr := 99] ## use 99 for thos w unknown bgr

bg_selected <- rbind(bg_selected,
      tmp
      )

## ## explore
## blooming_group_aggr[grepl("tar", var), ]
## cols <- c("variety", "var", "genotype") ## , "genotype"
## variety_genotype_group[grepl("tar", tolower(variety)), ..cols]
## tmp <- variety_genotype_group[grepl(paste0(paste0(notselected, collapse = "|")), tolower(variety)), ..cols]
## print(tmp[, .(var)], n = 200)
## bg_selected[bg_selected$var %in% variety_genotype_group$var, ]
## notselected <- blooming_group_aggr[!grepl(paste0(selectvars, collapse = "$|^"), var), var] ## in bgr but not selected

bg_selected <- bg_selected[bg_selected$var %in% variety_genotype_group$var, ] ## skip those not matching var name in genotype data

dta_toplot <- variety_genotype_group[bg_selected, on = "var"] ## all selected have genotype data matching var name

test <- dta_toplot[, .(var, genotype, bgr, incompatibility_group)]

## make a df to add cols
newcols <- as.data.table(matrix(nrow = nrow(test), ncol = nrow(test)+1))
names(newcols) <- c("var", test$var)
cols <- names(newcols)
newcols_mod <- newcols[ , (cols) := lapply(.SD, as.character), .SDcols = cols]
newcols_mod$var <- test$var
test <- newcols_mod[test, on = "var"]
for(i in 1:nrow(test)){ ## loop over rows
    for(varn in test$var){ ## loop over cols
        ## get genotype of column name
             ## test[i, (varn) := test[var == varn, genotype]]  
        ## get compatibility value of column name
                test[i, (varn) := compat(test[var == varn, genotype],
                                         test[i, genotype])  ]  
            }
}

## test
## str(test)

## melt
dtplot <-  melt(test, id.vars = c("var", "genotype", "bgr",  "incompatibility_group"))
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

## plot
pacman::p_load(ggplot2)
library(forcats) ## for reordering plot levels

## labels
tmp <- variety_genotype_group[, .(var, genotype, variety, incompatibility_group)]
names(tmp) <- c("var", "genotype", "label", "incompatibility_group")

tmp[, label := gsub(" \\([^$]*", "", label)] ## sanitize
tmp[, label := gsub(" \\\n[^$]*", "", label)]
tmp[, label := gsub(" \\/[^$]*", "", label)]
tmp[, label := gsub("TM$", "", label)]
tmp[, label := gsub("Späte Rote Knorpelkirsche", "Rote", label)]
tmp[, label := gsub("Knauffs Schwarze", "Knauffs", label)]
tmp[, label_ss := paste0(label, " [", genotype, ", ", incompatibility_group, "]")]
varnames <- tmp$label_ss
names(varnames) <- tmp$var
dtplot[, target := factor(target, levels = levels(target), labels = unname(query_label(levels(target), varnames)))]
dtplot[, pollinator:= factor(pollinator, levels = levels(pollinator), labels = unname(query_label(levels(pollinator), varnames)))]

## dtplot[pollinator_blooming_group_num == "Early", ]

## plot base
p <- ggplot(dtplot, aes(x = fct_reorder(pollinator, pollinator_blooming_group_num), y = fct_reorder(target, bgr))) +
  geom_point(aes(size = compatibility, colour = compat_proximity))

## plot customization
plot_pollination_table <- p +
    scale_size_area() +
    scale_color_manual(values=c("no" = "red", "close" = "lightgreen", "same" = "chartreuse3", "bt_unknown" = "white")) +
    theme(
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "centimeters"),
        legend.position = "none",
        axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0),
        plot.title = element_text(hjust = 0, vjust = 3, size = 22, face="bold"),
        axis.title.x = element_text(hjust = 0.5, vjust = -5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=18, face="bold")
    ) +
    labs(title="Pollinationsdiagram för körsbär",
         x ="Pollinatör",
         y = "Mottagare")

## add strips
plot_pollination_table <- plot_pollination_table +
    facet_grid(~ pollinator_blooming_group,
               scales = "free",
               ## switch = "x",
               space = "free_x") +
    theme(panel.spacing = unit(0, "lines"),
            panel.background = element_rect(fill = "gray94",
                                colour = "gray",
                                linewidth = 1, linetype = "solid"),
         strip.background = element_rect(colour="black", fill = NA),
         panel.border = element_rect(colour="black", fill = NA),
         strip.placement = "outside"         
         )

plot_pollination_table
  ## meta:
## Size of dot corresponds to genetic compatibility, color to blooming time (dark green = same bloomin group, light green = proximity 1 in blooming group, red = outside proximity blooming groups OR not genetically compatible)
## "Blomningstid"

## dtplot[grepl("samba|frisco", target), ]

dta[ , paste(var, ": ", pollinated_by_concordance_chr)]


############## here #####################

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
