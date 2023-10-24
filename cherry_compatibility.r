## Genotypes data--------------------------------------------
pacman::p_load(data.table)

## load data files ------

## Data from Self-incompatibility (S) genotypes of cultivated sweet cherries – An overview update 2020 Mirko Schuster Julius Kühn-Institut (JKI)
## In the new update from October 2020
## a total of 63 incompatibility groups in 1483 sweet cherries have been defined. 26 sweet cherries have a unique combination of S alleles and were described as universal donors and placed in the incompatibility group 0. Additionally, there exist 91 self-compatible sweet cherries, called group SC. A total of 22 different S alleles are described in the cultivated sweet cherries up to date. These are the S alleles S1, S2, S3, S4, S5, S6, S7, S9, S10, S12, S13, S14, S16, S17, S18, S19, S21, S22, S24, S27, S30, S37.

### Lista grupper + S-alleler
incompatibility_groups <- fread("cherries_incompatibility_groups_2020.csv") 
## S4'|any => Self compatible (SC)

## Innkompatibilitetsgrupper
incompatibility_groups <- fread("cherry_incompatibility_groups_2020.csv") 

### Lista sort och S-alleler mm 2020
variety_genotype_group <- fread("cherries_variety_genotype_group.csv", header = TRUE)
## Country according ISO 3166 Code list
## See also: functional_genotypes_compatibility_groups.csv


names(variety_genotype_group)
variety_genotype_group[, .(incompatibility_group)]
unique(variety_genotype_group$incompatibility_group)

## Explore S4'
variety_genotype_group[grepl("S4'", genotype) , ]
## Note: All S4' are SC (Self-compatible)
sc <- variety_genotype_group[grepl("SC", incompatibility_group) , ]
sc[grepl("S4'", genotype), .(variety, genotype, incompatibility_group)]
sc[grepl("S4'", genotype) == FALSE, .(variety, genotype, incompatibility_group)]
## Note: Not al SC have S4


variety_genotype_group[grepl("0", incompatibility_group) , ]

variety_genotype_group[grepl("Lapins", variety), .(variety, genotype, incompatibility_group, mother, father)]

variety_genotype_group[grepl("Lapins|Van|Stella", variety), .(variety, genotype, incompatibility_group, mother, father)]

## Rosbreed har en xls med S-gruppe också: https://www.rosbreed.org/breeding/dna-tests/cherry/cross-compatibility
