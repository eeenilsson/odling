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

##############################################



############## here #####################

variety_genotype_group[grepl("", tempvar), ]
variety_genotype_group[grepl("Rote Knorpel", variety), ]

variety_genotype_group[grepl("Große Schwarze Knorpel", variety), ]
variety_genotype_group[grepl("", variety), ]


variety_genotype_group[, unique(incompatibility_group)]
## SC has a trailing space


## See also: functional_genotypes_compatibility_groups.csv
## Note: Check if they correspond



## Inkompatibilitetsgrupper lista på kategorier -------------

## incompatibility_groups <- fread("cherry_incompatibility_groups_2020.csv") 
## Note: SC and 0 missing from groups list
## S4'|any => Self compatible (SC)

## Lista grupper + S-alleler redundant?
## incompatibility_groups <- fread("cherries_incompatibility_groups_2020.csv") 

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
