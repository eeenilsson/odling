## Körsbär

## functions
pacman::p_load(data.table)
pacman::p_load(ggplot2)

source('cherries_functions.r')
source("../functions/query_label.r")

## data
dta <- fread("cherries_table.csv")

## variable descriptions -----
## pollinated_by_sveriges_tradgardsmastare :
## 0 = Troligen självfertil

## curate ------------------

## Recode to numbers plantagen 

tmp <- paste(dta$pollinated_by_plantagen, collapse = ", ") 
tmp <- strsplit(tmp, c(", |samt"))
tmp <- unique(tmp[[1]])

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

x <- cbind(replace_name(dta$pollinated_by_plantagen, varnames),
      dta$pollinated_by_plantagen)

dta$temp <- replace_name(dta$pollinated_by_plantagen, varnames)

## create lookup table
lookuptable <- dta[, .(var, nr)]
lookupvarnames <- lookuptable$var
names(lookupvarnames) <- lookuptable$nr

## replace names with numbers
dta$pollinated_by_plantagen_num <-  replace_name(dta$temp, lookupvarnames)

## Splendor
tmp <- fread('cherries_splendor.csv')
tmp$var %in% dta$var ## check
dta <- tmp[dta, on = "var"] ## add var
dta$pollinated_by_splendor_num <-
    replace_name(dta$pollinated_by_splendor, lookupvarnames)

## Rangedala
tmp <- fread('cherries_rangedala.csv')
tmp$var %in% dta$var ## check
dta <- tmp[dta, on = "var"] ## add var
dta$pollinated_by_rangedala_num <-
    replace_name(dta$pollinated_by_rangedala, lookupvarnames)

## Replace sjalvfertil med eget nr

for(i in 1:nrow(dta)){
dta[i, pollinated_by_rangedala_num := sub("sjalvfertil", nr, pollinated_by_rangedala_num)]
}

for(i in 1:nrow(dta)){
dta[i, pollinated_by_splendor_num := sub("sjalvfertil", nr, pollinated_by_splendor_num)]
}

for(i in 1:nrow(dta)){
dta[i, pollinated_by_plantagen_num := sub("sjalvfertil", nr, pollinated_by_plantagen_num)]
}

for(i in 1:nrow(dta)){
dta[i, pollinated_by_sveriges_tradgardsmastare_num := sub("sjalvfertil", nr, pollinated_by_sveriges_tradgardsmastare_num)]
}

## "" to NA
dta$pollinated_by_plantagen_num[nchar(dta$pollinated_by_plantagen_num)==0] <- NA
dta$pollinated_by_sveriges_tradgardsmastare_num[nchar(dta$pollinated_by_sveriges_tradgardsmastare_num)==0] <- NA

## squish
cols <- c("pollinated_by_rangedala_num", "pollinated_by_plantagen_num",
          "pollinated_by_splendor_num", "pollinated_by_sveriges_tradgardsmastare_num"
)
dta[ , (cols) := lapply(.SD, function(x){gsub(" *", "", x)}),
    .SDcols = cols]

## numbers to varnames ----------

## create lookup table
lookuptable <- dta[, .(var, nr)]
lookupvarnames <- as.character(lookuptable$nr)
names(lookupvarnames) <- lookuptable$var

## replace numbers with names
dta$pollinated_by_rangedala_chr <- replace_name(dta$pollinated_by_rangedala_num, lookupvarnames,
    exact = TRUE)
dta$pollinated_by_plantagen_chr <- replace_name(dta$pollinated_by_plantagen_num, lookupvarnames,
    exact = TRUE)
dta$pollinated_by_splendor_chr <- replace_name(dta$pollinated_by_splendor_num, lookupvarnames,
    exact = TRUE)
dta$pollinated_by_sveriges_tradgardsmastare_chr <- replace_name(dta$pollinated_by_sveriges_tradgardsmastare_num, lookupvarnames,
    exact = TRUE)

## concordance pollination -----

## concatenate sources
dta[, pollinated_by_concat_num := paste(pollinated_by_rangedala_num, pollinated_by_plantagen_num,
pollinated_by_splendor_num, pollinated_by_sveriges_tradgardsmastare_num,
sep = ";")]

dta[, pollinated_by_concat_chr := paste(pollinated_by_rangedala_chr, pollinated_by_plantagen_chr,
pollinated_by_splendor_chr, pollinated_by_sveriges_tradgardsmastare_chr,
sep = ";")]

## test concordance
myfun <- function(x){paste(test_concordance(x), collapse = ", ")}
dta[, pollinated_by_concordance_num := unlist(lapply(dta$pollinated_by_concat_num, myfun))]
myfun <- function(x){paste(test_concordance(x), collapse = ", ")}
dta[, pollinated_by_concordance_chr := unlist(lapply(dta$pollinated_by_concat_chr, myfun))]

## ## check
## dta$pollinated_by_concat_chr
## dta$pollinated_by_concordance_chr
## write.csv(dta, "test.csv")


## Plot pollineringsschema -------

temp <- dta[intresse == 1|intresse == 2, .(var, pollinated_by_concordance_chr, pollinated_by_concat_chr)]

## create varnames for pollinators
pol <- dta$pollinated_by_concordance_chr
pol <- unlist(strsplit(pol, ", "))
pol <- gsub(" .*", "", pol)
pol <- unique(pol)
pol <- gsub("NA", "var", pol)
dt <- matrix(nrow = nrow(temp), ncol = length(pol))
colnames(dt) <- pol
dt <- data.table(dt, key = "var")
dt$var <- temp$var
temp <- temp[dt, on = "var"]

## melt
temp <- melt(temp, id.vars = c("var", "pollinated_by_concordance_chr", "pollinated_by_concat_chr"))
temp[, value := stringi::stri_detect_fixed(pollinated_by_concordance_chr, paste0(variable, " ("))]

## Calculate concordance?
myfun <- Vectorize(test_concordance_prop)
temp[, concordance := myfun(pollinated_by_concat_chr, variable)]

## select cols for plotting
toplot <- temp[, .(var, variable, value, concordance)]
names(toplot) <- c("target", "pollinator", "value", "concordance")
setkey(toplot, target)


## setkey(toplot, var)
toplot[, value:= as.numeric(value)]
toplot[, target:= as.factor(target)]
## View(toplot)
## str(toplot)

## labels
varnames <- dta$label
names(varnames) <- dta$var
toplot[, target:= factor(target, levels = levels(target), labels = unname(query_label(levels(target), varnames)))]
toplot[, pollinator:= factor(pollinator, levels = levels(pollinator), labels = unname(query_label(levels(pollinator), varnames)))]

## plot base
p <- ggplot(toplot, aes(pollinator, target)) +
  geom_point(aes(size = concordance))

## plot customization
p +
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

toplot

### HERE #################

## Genotypes --------------------


pacman::p_load(data.table)

## Data from Self-incompatibility (S) genotypes of cultivated sweet cherries – An overview update 2020 Mirko Schuster Julius Kühn-Institut (JKI)
## In the new update from October 2020
## a total of 63 incompatibility groups in 1483 sweet cherries have been defined. 26 sweet cherries have a unique combination of S alleles and were described as universal donors and placed in the incompatibility group 0. Additionally, there exist 91 self-compatible sweet cherries, called group SC. A total of 22 different S alleles are described in the cultivated sweet cherries up to date. These are the S alleles S1, S2, S3, S4, S5, S6, S7, S9, S10, S12, S13, S14, S16, S17, S18, S19, S21, S22, S24, S27, S30, S37.

### Lista grupper + S-alleler
incompatibility_groups <- fread("cherries_incompatibility_groups_2020.csv") 
## S4'|any => Self compatible

incompatibility_groups <- fread("cherry_incompatibility_groups_2020.csv") 


### Lista sort och S-alleler mm 2020
variety_genotype_group <- fread("cherries_variety_genotype_group.csv", header = TRUE)
## 1 Country according ISO 3166 Code list
## See also: functional_genotypes_compatibility_groups.csv

names(variety_genotype_group)

variety_genotype_group[, .(incompatibility_group)]

unique(variety_genotype_group$incompatibility_group)

## S4'
variety_genotype_group[grepl("S4'", genotype) , ]
## Note: All S4' are SC
sc <- variety_genotype_group[grepl("SC", incompatibility_group) , ]
sc[grepl("S4'", genotype), .(variety, genotype, incompatibility_group)]
sc[grepl("S4'", genotype) == FALSE, .(variety, genotype, incompatibility_group)]
## Note: Not al SC have S4


variety_genotype_group[grepl("0", incompatibility_group) , ]

variety_genotype_group[grepl("Lapins", variety), .(variety, genotype, incompatibility_group, mother, father)]

variety_genotype_group[grepl("Lapins|Van|Stella", variety), .(variety, genotype, incompatibility_group, mother, father)]



## Rosbreed har en xls med S-gruppe också: https://www.rosbreed.org/breeding/dna-tests/cherry/cross-compatibility

## Phenology data bloom time ------------------

fread("anfic_blooming_time.csv")
## pollination_period_anfic (I-V) => 1-5
## 1 = "Early", 2 = "Early mid", 3 = "Mid", 4 = "Late mid", 5 = Late
## Crosstable: Cols ordered by blooming period, rows by comp_gr, heatmap-style with red = incompatible, green = "All compatible", yellow = "Compatible but different bloom sequence"


fread("sweet_cherry_phenology_data_1978_2015.csv")

## Metadata Sweet cherry phenology data: 1978 - 2015

## The dataset file includes the dataset and a metadata spreadsheet with the description of all information in the dataset spreadsheet.

## Header	Description
## Country	
## Institute	Research or experimental institute attached to the experimental station
## Station		Name of station
## Latitude	Latitude in decimal degrees
## Longitude	Longitude in decimal degrees
## Altitude	Altitude in meters
## Plantation	Year of plantation of the tree
## Year		Year of observation
## Cultivar	Registered name of the cutlivar
## Clone		Number of clone
## Rootstock	Name of the rootstock
## Beginning of flowering (date)	Date observed for the BBCH stage corresponding to beginning of flowering
## Full flowering (date)	Date observed for the BBCH stage corresponding to full flowering
## End of flowering (date)	Date observed for the BBCH stage corresponding to end of flowering
## Beginning of maturity (date)	Date observed for the BBCH stage corresponding to maturity
## Beginning of flowering	Number of days in year [1 - 365/366] for the BBCH stage corresponding to beginning of flowering
## Full flowering	Number of days in year [1 - 365/366] for the BBCH stage corresponding to full flowering
## End of flowering	Number of days in year [1 - 365/366] for the BBCH stage corresponding to end of flowering
## Beginning of maturity	Number of days in year [1 - 365/366] for the BBCH stage corresponding to maturity
## Flowering duration	Number of days between beginning and end of flowering
## Sweet cherry phenological data were collected from two networks: flowering and maturity dates for up to 191 reference cultivars, and from 10 sites, were extracted from the French database,
## [A collection of European sweet cherry phenology data for assessing climate change](https://www.nature.com/articles/sdata2016108)
## - [Data](https://datadryad.org/stash/dataset/doi:10.5061/dryad.1d28m)


## ANFIC, australien
## ANFIC-Sweet-Cherry-Pollination-Table
## https://anfic.com.au/wp-content/uploads/2020/07/ANFIC-Sweet-Cherry-Pollination-Table-06072020.pdf








#############################

## https://ggplot2.tidyverse.org/reference/scale_size.html
## p <- ggplot(toplot, aes(pollinator, target)) +
##   geom_count(aes(size = value))
## p + scale_size_area()



## ## filter matches
## dt <- temp[value == TRUE, .(var, variable)]
## str(dt)
## setkey(dt, var)



temp[, value := grepl(paste0(variable, " \\("), pollinated_by_concordance_chr)]



    lookfor <- paste(i, " (")
    temp[var == i, stringi::stri_detect_fixed(pollinated_by_concordance_chr, lookfor)]



write.csv(temp, "test.csv")
?melt.data.table

names(temp)

## temp$pollinated_by_concordance_chr <- temp$pollinated_by_concordance_chr

pol <- pol[pol != "var"] ## remove "var" before loop
i <- "merton_glory"
for(i in pol){
    lookfor <- paste(i, " (")
    temp[var == i, stringi::stri_detect_fixed(pollinated_by_concordance_chr, lookfor)]

        temp[[i]][var == i, stringi::stri_detect_fixed(pollinated_by_concordance_chr, lookfor)]

    
    ## temp[var == i, grepl(lookfor, pollinated_by_concordance_chr)]
    print(temp[[i]])
}

x <- "NA (75%), allm_gulrod (100%), erianne (100%), kelleris (100%), kirsa (100%), merton_glory (100%), merton_premier (100%), nordia (100%)"
grepl("xurt", x, fixed = TRUE)
grepl("merton_glory", x, fixed = TRUE)
grepl("merton", x, fixed = TRUE)
grepl("merton_glory \\(", x)
grepl("merton \\(", x)
stringi::stri_detect_fixed(x, "merton_glory (")
stringi::stri_detect_fixed(x, "merton")
stringi::stri_detect_regex(x, "merton_glory \\(")
grepl(paste0("merton_glory", " \\("), x)


?stringi::stri_detect

?stringi::stri_detect_fixed()
?grepl

names(dta)
## dta[pollinated_by_concat_num == "NA;NA;NA;NA", .(label, pollinated_by_concat_num)]





    replace_name(dta$pollinated_by_plantagen_num[33], lookupvarnames,
    exact = FALSE)

varnames <- lookupvarnames
x <- dta$pollinated_by_plantagen_num[33]
dta$pollinated_by_plantagen_num[33]


dta$pollinated_by_concat

test_concordance(dta$pollinated_by_concat[33])

## write.csv(dta[, .(var, pollinated_by_concat)], "test.csv")

## dta$pollinated_by_concat
## item <- dta$pollinated_by_concat[33]
## item <- dta$pollinated_by_concat[36]

test_concordance(dta$pollinated_by_concat[33])
dta$pollinated_by_concat[33]
test_concordance(dta$pollinated_by_concat[36])



## dta$pollinated_by_plantagen_chr <-
##     replace_name(dta$pollinated_by_plantagen_num, lookupvarnames)




## check

dta$pollinated_by_rangedala_num
dta$pollinated_by_plantagen_num
dta$pollinated_by_splendor_num
dta$pollinated_by_sveriges_tradgardsmastare_num

dta[, .(pollinated_by_rangedala_num, pollinated_by_plantagen_num, pollinated_by_splendor_num)]






## Bilder -----------

## /home/e/dropbox/images/plants


## notes -------

## Mognad mm
"Mognadstiden för körsbär anges i veckor av körsbärstiden, från körsbärsvecka 1 till 8. När körsbärstiden infaller varierar mellan åren beroende på väder och vind. Ofta börjar de tidigaste körsbären mogna vid midsommartid i södra Sverige. Tidiga körsbär mognar körsbärsvecka 1–2, medelsena vecka 3–5, sena vecka 6–8."
## 1 korsbärsvecka = 15 dagar2?

## tidiga mognar körsbärsvecka 1–2, medelsena vecka 3–5, sena vecka 6–8

## Compare cherries

## https://www.gardenia.net/compare-plants/cherries

## https://thomasfruittrees.eu/pollinationchecker.aspx?v=10080

## Uppsats ; https://stud.epsilon.slu.se/182/1/vandermaarel_l_090515.pdf

##  Våra päron-, plommon- och körsbärssorter : deras historia, egenskaper och kännetecken av Anton Nilsson Inbunden bok.

## Artikel
## https://link.springer.com/article/10.1007/s001220051700
## https://anfic.com.au/wp-content/uploads/2020/07/ANFIC-Sweet-Cherry-Pollination-Table-06072020.pdf

## E-planta

## http://www.eriksbo-plantskola.se/wp-content/uploads/2017/03/ERIKSBO-2018-utan-priser.pdf


## Körsbär ska plockas med stjälken om de inte äts med det samma så håller dom längre.

## Wexthuset
## https://www.wexthuset.com/fakta-och-rad/skotselrad-om-vaxter-i-kruka-och-tradgard/beskrivningar-odling-skotsel-bar-och-frukter/korsbar-odling-sorter

"Surkörsbär delas in i moreller som har starkt färgad saft och klarbär, med ofärgad saft. De är nättare i formen och säljs ibland som buskträd och passar mycket bra för spaljering där de till och med kan spaljeras i norrläge, om än med något sämre skörd. De är vanligen inte ympade, utan växer på egen rot. Därmed är de väl lämpade för utekrukan. Surkörsbär är ofta självfertila och har bättre härdighet än sötkörsbären."

## Se cherries_wexthuset.csv

## Real English Fruit
## https://realenglishfruit.co.uk/cherry-pollination/
"Cherry varieties are subdivided into six groups, A, B, C, D, E, F, according to their flowering time. A tree will cross-pollinate with trees in its own group, and the groups on either side. For example, for a tree in group C, a pollinator can be in groups B, C or D. Things are made easier by the fact that most cherry trees are in groups C and D."


## Bra sida: https://www.erlingnielsensplanteskole.dk/svenska/include/popDetail.php?c=124&p=1220


## Terminologi

## Skiftande genom tiderna
## Med namnet bigarrå avsågs fastare sötkörsbär, oberoende av färg.

## Amareller (Caproniana-gruppen) – frukterna har blekt fruktkött och ofärgad saft. Till sorterna hör bland andra:
    ## 'Allmänt klarbär'
    ## 'Grosse Glaskirsche' ('stort klarbär')
    ## 'Montmorency'
    ## 'Pernilla'.

## Moreller (Austera-gruppen) – frukterna har mörkt fruktkött och stark färgad fruktsaft. De används bland annat till saft, marmelad, sylt och vin. Några sorter i gruppen är:

    ## 'Allmänt Brunkörsbär'
    ## 'Griot Moskovskii'
    ## 'Heimanns Konservenweichsel' ('fanal')
    ## 'Kelleris 14'
    ## 'Kelleris 16'
    ## 'Kirsa'
    ## 'Maraska' (maraskakörsbär) – en morell med relativt små, bittra och torra frukter som odlas i Kroatien. Av frukterna görs likören maraschino.
    ## 'Morellenfeuer'
    ## 'Morello' ('skuggmorell')
    ## 'Nefris'
    ## 'Nordia'
    ## 'Ostheimer Weichsel' ('Ostheimer')
    ## 'Rheinische Schattenmorelle'
    ## 'Semperflorens' (allhelgonakörsbär) – blommar två gånger per säsong, först i april-maj och senare i augusti-september. Frukterna är sura, men smakrika.
    ## 'Stevnsbär'
    ## 'Stevnsbär Birgitte'
    ## 'Stevnsbär Viki'
    ## 'Tshernokorka'
    ## 'Weichel'
    ## 'Wildholdt'
    ## 'Vladimir'

## Pollinering ---------------
## Svenska pollineringsscheman över körsbär har tyvärr inte uppdaterats med de nyare sorterna som dykt upp i handeln, om du vill googla själv så rekommenderar jag därför att du söker på växtens namn på latin och engelska, (inte på svenska) exempelvis: Prunus avium Karina + pollinator så hittar du


## Saft, konservering etc ------------

## Surkörsbär lämpar sig bäst för saft och sylt


## Guide
## https://www.tradgardstrollet.se/2023/06/08/sortguide-sotkorsbar/

## ## E-planta (märkning)

## En E-planta är alltid utvald för svenskt
## klimat och odlas i Sverige. Det ger säkra
## underlag till korrekta zonangivelser och
## växtbeskrivningar

## En E-planta kommer alltid från ett friskt
## utgångsmaterial. Det ger bättre motstånds-
## kraft mot sjukdomar och skadegörare.

## En E-planta kommer alltid från godkända
## svenska frökällor eller moderplantor. Kort
## sagt får du samma växt när du kompletterar
## eller handlar från olika plantskolor.

## Zonkarta: https://eplanta.com/tips-rad/zonkarta/


## Grundstam

## Grundstammar för körsbär
## Colt:

## Colt är en grundstam som ger medelstarkväxande träd, sluthöjden blir ca 3-4 m efter 8 år. Bördigheten är god och Colt ger frukt tidigt, ofta inom 1-2 år. Träd ympade på denna grundstam klarar lite sämre jord. I övrigt kräver Colt stöd de först åren men klarar sig sedan utan. Zon 1-3.

## GiSelA:
## Denna grundstam ger mindre träd som passar bra för små trädgårdar där körsbär ibland, på grund av sin storlek inte får plats. Sluthöjden varierar beroende på sort men ligger på ca 3-4 m. Fördelen med GiSelA är också att sort ympade på denna grundstam ger skörd i tidig ålder, ofta redan efter 1-2 år. Härdigheten är mycket god då träd ympade på denna grundstam avmognar tidigt. Zon 5.

## Prunus avium:
## Ger stora, starkväxande träd med god förankring i jorden. Något senare fruktsättning beroende på sort och jordmån. Zon 1-5.


## Grundstammar för plommon

## St. Julien:
## En grundstam som ger medelstarkväxande träd. Ger inte lika stora träd som grundstammen Prunus cerasifera. Härdigheten är god och rotsystemet välutvecklat. Grundstammen har inga krav på jordmån. Ger frukt efter ca 3-4 år. Zon 1-5(6). Planteringsyta 10-15 m2.

## Prunus cerasifera:
## Denna grundstam ger starkväxande träd med ett välutvecklat rotsystem. Vinterhärdigheten är god, dock inte lika god som St. Julien. Fruktsättningen kommer också något senare än St. Julien. Zon 1-4(5). Planteringsyta 15-20 m2.

## Prunus Pixy:
## Denna grundstam ger svagväxande träd, ca 30% jämfört med Prunus cerasifera och lämpar sig därför till den mindre trädgården. Pixy ger frukt tidigt, redan efter 2-3 år. Härdigheten är också god eftersom grundstammen ger träd som avmojnar tidigt på hösten. Zon 1-4(5). Planteirngsyta 6-8 m2.
