## Körsbär

## functions
pacman::p_load(data.table)
pacman::p_load(ggplot2)

source('cherries_functions.r')
source("../functions/query_label.r")

## data
dta <- fread("cherries_table.csv")

## pollinated_by_plantagen <- dta[, .(var, pollinated_by_plantagen)]
## write.csv(pollinated_by_plantagen, "pollinated_by_plantagen.csv", row.names = FALSE)




## variable descriptions -----
## pollinated_by_sveriges_tradgardsmastare :
## 0 = Troligen självfertil

## curate ------------------

## Recode to numbers plantagen 
pollinated_by_plantagen <- read.csv("pollinated_by_plantagen.csv")
tmp <- paste(pollinated_by_plantagen$pollinated_by_plantagen, collapse = ", ") 
tmp <- strsplit(tmp, c(", |samt"))
tmp <- unique(tmp[[1]])

varnames <- c(
    'buttners_spate_rote_knorpelkirsche' = "Büttners Späte Rote",
    'allman_gulrod' = "Allm. gulröd bigarrå",
    'hedelfinger' = "Hedelfinger",
    'merton_glory' = "Merton Glory",
    ', ' = " samt",
    'sam' = "Sam",
    'grosse_schwarze_knorpel' = "Stor svart bigarrå",
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

## x <- cbind(replace_name(dta$pollinated_by_plantagen, varnames),
##       dta$pollinated_by_plantagen)

pollinated_by_plantagen$temp <- replace_name(pollinated_by_plantagen$pollinated_by_plantagen, varnames)

## create lookup table
lookuptable <- dta[, .(var, nr)]
lookupvarnames <- lookuptable$var
names(lookupvarnames) <- lookuptable$nr

## replace names with numbers
dta$pollinated_by_plantagen <- pollinated_by_plantagen$temp
dta$pollinated_by_plantagen_num <-  replace_name(pollinated_by_plantagen$temp, lookupvarnames)


## fix error in sveriges tradgardsmastare ---------------------

tmp <- dta[, .(var, kompletterat_sveriges_tradgardsmastare, pollinated_by_sveriges_tradgardsmastare_num)]
## write.csv(tmp, "pollinated_by_sv_tradgm.csv", row.names = FALSE)
## dta[, .(var, pollinated_by_table)]
lookup <- fread("pollinated_by_sv_tradgm_lookup.csv")
lookup$var <- sanitize_var(lookup$pollinator)
lookup <- data.table(lookup)
names(tmp) <- c("var", "kompl", "pol")
tmp$kompl <- NULL

lookupname <- function(x, lookup_table){
    ## lookup table needs var and kodnr
    paste0(lookup_table$var[lookup_table$kodnr %in% strsplit(x, ", ")[[1]]], collapse = ", ")
}
## lookupname <- Vectorize(lookupname)

pol_repl <- c()
for(i in 1:nrow(tmp)){
    dt <- data.table(
            var = tmp[i, var],
    polin = paste0(
        lookup$var[lookup$kodnr %in% strsplit(tmp$pol[i], ", ")[[1]]], collapse = ", ")
)
    pol_repl <- rbind(pol_repl, dt)
}

pol_repl$pollinated_by_sveriges_tradgardsmastare <- pol_repl$polin
pol_repl <- pol_repl[, .(var, pollinated_by_sveriges_tradgardsmastare)]
## write.csv(pol_repl, "pollinated_by_sv_tradgm_chr.csv", row.names = FALSE)


dta <- pol_repl[dta, on = "var"] ## add var
dta[, .(var, pollinated_by_sveriges_tradgardsmastare)]

dta$pollinated_by_sveriges_tradgardsmastare_num <-
    replace_name(dta$pollinated_by_sveriges_tradgardsmastare, lookupvarnames)

## Splendor -----------------------------
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

## dta[var == "van", ]

## test concordance
myfun <- function(x){paste(test_concordance(x), collapse = ", ")}
dta[, pollinated_by_concordance_num := unlist(lapply(dta$pollinated_by_concat_num, myfun))]
myfun <- function(x){paste(test_concordance(x), collapse = ", ")}
dta[, pollinated_by_concordance_chr := unlist(lapply(dta$pollinated_by_concat_chr, myfun))]

## ## check
## dta$pollinated_by_concat_chr
## dta$pollinated_by_concordance_chr
## write.csv(dta, "test.csv")


## Plot --------------------

## source("cherries_plot_pollinationtable.r")


### HERE #################

## https://ggplot2.tidyverse.org/reference/scale_size.html
## p <- ggplot(toplot, aes(pollinator, target)) +
##   geom_count(aes(size = value))
## p + scale_size_area()

## ## filter matches
## dt <- temp[value == TRUE, .(var, variable)]
## str(dt)
## setkey(dt, var)



## temp[, value := grepl(paste0(variable, " \\("), pollinated_by_concordance_chr)]



##     lookfor <- paste(i, " (")
##     temp[var == i, stringi::stri_detect_fixed(pollinated_by_concordance_chr, lookfor)]



## temp$pollinated_by_concordance_chr <- temp$pollinated_by_concordance_chr




## pol <- pol[pol != "var"] ## remove "var" before loop
## i <- "merton_glory"
## for(i in pol){
##     lookfor <- paste(i, " (")
##     temp[var == i, stringi::stri_detect_fixed(pollinated_by_concordance_chr, lookfor)]

##         temp[[i]][var == i, stringi::stri_detect_fixed(pollinated_by_concordance_chr, lookfor)]

    
##     ## temp[var == i, grepl(lookfor, pollinated_by_concordance_chr)]
##     print(temp[[i]])
## }

## x <- "NA (75%), allm_gulrod (100%), erianne (100%), kelleris (100%), kirsa (100%), merton_glory (100%), merton_premier (100%), nordia (100%)"
## grepl("xurt", x, fixed = TRUE)
## grepl("merton_glory", x, fixed = TRUE)
## grepl("merton", x, fixed = TRUE)
## grepl("merton_glory \\(", x)
## grepl("merton \\(", x)
## stringi::stri_detect_fixed(x, "merton_glory (")
## stringi::stri_detect_fixed(x, "merton")
## stringi::stri_detect_regex(x, "merton_glory \\(")
## grepl(paste0("merton_glory", " \\("), x)

## names(dta)

## dta[pollinated_by_concat_num == "NA;NA;NA;NA", .(label, pollinated_by_concat_num)]

##     replace_name(dta$pollinated_by_plantagen_num[33], lookupvarnames,
##     exact = FALSE)

## varnames <- lookupvarnames
## x <- dta$pollinated_by_plantagen_num[33]
## dta$pollinated_by_plantagen_num[33]



## write.csv(dta[, .(var, pollinated_by_concat)], "test.csv")

## dta$pollinated_by_concat
## item <- dta$pollinated_by_concat[33]
## item <- dta$pollinated_by_concat[36]

## test_concordance(dta$pollinated_by_concat[33])
## dta$pollinated_by_concat[33]
## test_concordance(dta$pollinated_by_concat[36])

## dta$pollinated_by_plantagen_chr <-
##     replace_name(dta$pollinated_by_plantagen_num, lookupvarnames)




## ## check
## dta$pollinated_by_rangedala_num
## dta$pollinated_by_plantagen_num
## dta$pollinated_by_splendor_num
## dta$pollinated_by_sveriges_tradgardsmastare_num
## dta[, .(pollinated_by_rangedala_num, pollinated_by_plantagen_num, pollinated_by_splendor_num)]



## Bilder -----------

## /home/e/dropbox/images/plants


## notes -------

## Mognad mm "Mognadstiden för körsbär anges i veckor av körsbärstiden, från körsbärsvecka 1 till 8. När körsbärstiden infaller varierar mellan åren beroende på väder och vind. Ofta börjar de tidigaste körsbären mogna vid midsommartid i södra Sverige. Tidiga körsbär mognar körsbärsvecka 1–2, medelsena vecka 3–5, sena vecka 6–8."
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

## "Surkörsbär delas in i moreller som har starkt färgad saft och klarbär, med ofärgad saft. De är nättare i formen och säljs ibland som buskträd och passar mycket bra för spaljering där de till och med kan spaljeras i norrläge, om än med något sämre skörd. De är vanligen inte ympade, utan växer på egen rot. Därmed är de väl lämpade för utekrukan. Surkörsbär är ofta självfertila och har bättre härdighet än sötkörsbären."

## Se cherries_wexthuset.csv

## ## Real English Fruit
## ## https://realenglishfruit.co.uk/cherry-pollination/
## "Cherry varieties are subdivided into six groups, A, B, C, D, E, F, according to their flowering time. A tree will cross-pollinate with trees in its own group, and the groups on either side. For example, for a tree in group C, a pollinator can be in groups B, C or D. Things are made easier by the fact that most cherry trees are in groups C and D."


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

