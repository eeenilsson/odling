## Genotypes data--------------------------------------------
pacman::p_load(data.table)

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

dupl_var <- variety_genotype_group[duplicated(var), var]
variety_genotype_group[grepl(paste0(dupl_var, collapse = "|"), var), ]
## Note: some have two or more duplicated rows (n = 90 varieties), these all have different genotypes in different studies, i uncertain, can be removed since they are uncommon varieties
variety_genotype_group <- variety_genotype_group[!grepl(paste0(dupl_var, collapse = "|"), var), ]

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


####### todo
## See also: functional_genotypes_compatibility_groups.csv
## Note: Check if they correspond
#####


## test pollination groups (only for testing) ---------

## uk sites
pollination_groups_test <- fread("cherries_pollination_groups_uk.csv")
pollination_groups_test[, var := tolower(variety)]
pollination_groups_test <- pollination_groups_test[, .(var, pollination_test)]

pollination_groups_test[, var := gsub(" ", "_", var)]
names(pollination_groups_test) <- c("var", "blooming_group")
pollination_groups_test[, blooming_group := factor(blooming_group, ordered = TRUE, labels = c("Early", "Early mid", "Mid", "Late mid", "Late"))]

## ## test matches and misses
## pollination_groups_test$var[!pollination_groups_test$var %in% unique(variety_genotype_group$var)]
## pollination_groups_test$var[pollination_groups_test$var %in% unique(variety_genotype_group$var)]

## ## Notes on missing:

## avium but not in genome data:
## "knights_early_black"
## "petit_noir"
## "amber_heart" ## Syn 'Kent Bigarreau'
## "may_duke" ## Syn. 'Dubbele Meikers'    noted as "sour cherry" by some. "Duke cherry" It is a cross between Prunus avium and Prunus cerasus?
## "stardust_coveu" ## self-fertile white cherry

## ## cesarus
## "morello"
## "nabella"


## anfic
anfic_bt <- fread("anfic_blooming_time.csv")

anfic_bt[, var := tolower(label)]
anfic_bt <- anfic_bt[, .(var, pollination_period_anfic, label)]
anfic_bt[, var := gsub(" \\(.*", "", var)] ## remove parens
anfic_bt[, var := gsub("-", "_", var)]
anfic_bt[, var := gsub(" ", "_", var)]
names(anfic_bt) <- c("var", "blooming_group", "label")
anfic_bt[, blooming_group := factor(blooming_group, ordered = TRUE, labels = c("Early", "Early mid", "Mid", "Late mid", "Late"))]

## ## test matches and misses
## anfic_bt$var[!unique(anfic_bt$var) %in% unique(variety_genotype_group$var)]
## anfic_bt$var[unique(anfic_bt$var) %in% unique(variety_genotype_group$var)]
## ## note: many misses, todo: explore this

## ## restrict to varieties of interest for test
## anfic_bt$var[anfic_bt$var %in% unique(dta$var)]
## dta$var[unique(dta$var) %in% unique(anfic_bt$var)]
## dta$var[!unique(dta$var) %in% unique(anfic_bt$var)]
## anfic_bt[grepl("eri", label) , ]
## restr <- anfic_bt$var[anfic_bt$var %in% unique(dta$var)]
## restr_anfic <- anfic_bt[grepl(paste0(restr, collapse = "|"), var), ]

## restr <- pollination_groups_test$var[pollination_groups_test$var %in% unique(dta$var)]
## restr_uk <- pollination_groups_test[grepl(paste0(restr, collapse = "|"), var), ]


## using just anfic data for testing

anfic_selected <- anfic_bt[!duplicated(var), ]
anfic_selected <- anfic_selected[!grepl("[0-9]", var), ] ## skip those with numbers in name
anfic_selected <- anfic_selected[anfic_selected$var %in% variety_genotype_group$var, ] ## skip those not matching var name in genotype data

## anfic_bt$var[!unique(anfic_bt$var) %in% unique(variety_genotype_group$var)]

anfic_selected <- anfic_selected[anfic_selected$var %in% variety_genotype_group$var, ] ## skip those not matching var name in 

dta_toplot <- variety_genotype_group[anfic_selected, on = "var"] ## all selected have genotype data matching var name
str(dta_toplot)





dta_toplot <- dta_toplot[, .(var, variety, genotype, genotype1, genotype2, incompatibility_group, label)]

test <- dta_toplot[1:20, .(var, genotype, genotype1, genotype2, incompatibility_group)]

## make a df to add cols
newcols <- as.data.table(matrix(nrow = nrow(test), ncol = nrow(test)+1))
names(newcols) <- c("var", test$var)
cols <- names(newcols)
newcols_mod <- newcols[ ,                         
                             (cols) := lapply(.SD, as.character), 
                             .SDcols = cols]
## str(newcols_mod)
newcols_mod$var <- test$var
test <- newcols_mod[test, on = "var"]

test[1, ..i]

i <- 1
varn <- test$var[1]

test[var == varn, genotype]

for(i in 1:nrow(test)){ ## loop over rows

    for(varn in test$var){ ## loop over cols

        ## test[i, (varn) := varn] ## function here
        test[i, (varn) := test[var == varn, genotype]]

                ## test[i, (varn) := "testing"]
        ## test[var == i, paste0(i)]
            ## test[var == i, (i) := "hello"]
            }

}


test[var == i, black_tartarian := genotype]

i <- test$var[[1]]
for(i in test$var){
    test[var == i, 2..i]
}

str(test)




?matrix


variety_genotype_group[, unique(genotype1)]
variety_genotype_group[, unique(genotype2)]




## plot
pacman::p_load(ggplot2)

## plot base
p <- ggplot(toplot, aes(pollinator, target)) +
  geom_point(aes(size = concordance))



## plot customization
plot_pollination_table <- p +
    scale_size_area() +
    theme(
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "centimeters"),
        legend.position = "none",
        axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0),
        plot.title = element_text(hjust = 0, vjust = 3, size = 32, face="bold"),
        axis.title.x = element_text(hjust = 0.5, vjust = -5),
        axis.text=element_text(size=16),
        axis.title=element_text(size=24, face="bold")
    ) +
    labs(title="Pollinationsschema för körsbär",
         x ="Pollinatör",
         y = "Mottagare")



## "Blomningstid"

## Note: these from uk data are not in anfic:
## SUMMER SUN
## NAPOLEON
## MERTON BIGARREAU
## MERTON GLORY
## PENNY
## PETIT NOIR
## AMBER HEART
## MAY DUKE
## SASHA
## KARINA
## SASHA


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
