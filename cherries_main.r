## Main script for cherries

source('cherries_pollination_sv.r') ## Pollinatörer enl svenska hemsidor

## Compatibility ----------------------
source('cherry_compatibility.r')  ## S-allele data
## Note: Not al SC have S4
## ## Note: All S4' are SC (Self-compatible)

variety_genotype_group[grepl("0", incompatibility_group) , ]

variety_genotype_group[grepl("Lapins", variety), .(variety, genotype, incompatibility_group, mother, father)]

variety_genotype_group[grepl("Lapins|Van|Stella", variety), .(variety, genotype, incompatibility_group, mother, father)]


variety_genotype_group

source('cherries_phenology.r') ## Rosbreed


varnames <- c(
    'buttners_rote' = "Büttners Späte Rote",
    'allm_gulrod' = "Allm. gulröd bigarrå",
    'hedelfinger' = "Hedelfinger",
    'merton_glory' = "Merton Glory",
    ', ' = " samt",
    'sam' = "Sam",
    'stor_svart' = "Stor svart bigarrå",
    'van' = "Van",
    'almore' = "Almore",
    'stella' = "Stella",
    'sjalvfertil'= "Självfertil",
    ## = "Självfertil ",
    ## = " Allm. gulröd bigarrå",
    'victor' = "Victor",
    'heidi' = "Heidi",
    ## = " Büttners Späte Rote",
    'kordia' = "Kordia",
    'lapins' = "Lapins"
)



## Quarto -----
pacman::p_load(quarto)
## quarto_render('cherries_website/index.qmd')
## ?quarto_render
getwd()
quarto_render('cherries_website')
getwd()
## quarto_render('test_presentation.qmd')





