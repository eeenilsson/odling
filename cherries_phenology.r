## Phenology data bloom time etc ----------------------

## Kolla upp:
## https://garden.org/plants/view/711604/Cherry-Prunus-avium-Knauffs-Schwarze/
## National gardening association plants database!
### Anger sådant som:
## Sun Requirements: 	Full Sun
## Full Sun to Partial Shade
## Water Preferences: 	Mesic
## Soil pH Preferences: 	Moderately acid (5.6 – 6.0)
## Slightly acid (6.1 – 6.5)
## Neutral (6.6 – 7.3)

## Orangepippin
## https://www.orangepippin.com/varieties/cherries/a

## Nice historical info: http://www.bernwodeplants.co.uk/descriptions/cherry2.htm

## Trädgårdshuset har en fin tabell med egenskaper som mognadstid, grundstam, egenskaper etc:
## https://tradgardshuset.com/sortiment/frukt-och-bar/bigarraer/

## rosbreed
source('cherries_rosbreed.r') ## dataset analysis


## Blooming time =============================================

## Australian ANFIC blooming periods ------
## fread("anfic_blooming_time.csv")
## pollination_period_anfic (I-V) => 1-5
## 1 = "Early", 2 = "Early mid", 3 = "Mid", 4 = "Late mid", 5 = Late
## Crosstable: Cols ordered by blooming period, rows by comp_gr, heatmap-style with red = incompatible, green = "All compatible", yellow = "Compatible but different bloom sequence"

anfic_bt <- fread("anfic_blooming_time.csv")
lookup_gt <- fread("cherry_incompatibility_groups_2020.csv")
lookup_gt[, s_alleles := gsub(" ", "", s_alleles)]

anfic_bt[, var := tolower(label)]
anfic_bt <- anfic_bt[, .(var, pollination_period_anfic, label, comp_gr_anfic)]
anfic_bt[, var := gsub(" \\(.*", "", var)] ## remove parens
anfic_bt[, var := gsub("-", "_", var)]
anfic_bt[, var := gsub(" ", "_", var)]
names(anfic_bt) <- c("var", "blooming_group_anfic", "label", "comp_gr_anfic")
anfic_bt[, blooming_group_anfic := factor(blooming_group_anfic, ordered = TRUE, labels = c("Early", "Early mid", "Mid", "Late mid", "Late"))]
anfic_bt <- lookup_gt[anfic_bt, on = c("group" = "comp_gr_anfic")] ## add anfic s_alleles

## test matches and misses
nomatch <- anfic_bt$var[!anfic_bt$var %in% variety_genotype_group$var]
## anfic_bt$var[unique(anfic_bt$var) %in% unique(variety_genotype_group$var)]
## ## note: many misses, todo: explore this
## cols <- c("variety", "var", "genotype") ## , "genotype"
## variety_genotype_group[grepl("sms", tolower(variety)), ..cols]
## anfic_bt[grepl("douglas", var), ]
## nomatch

## rename anfic var to match genotype data
varnames_tmp <- c(  ## from (anfic) = to (genotype data)
    'simcoe' = "probla", ## probably this, same GT
'starkcrimson' = "starkrimson",
'burgundy_pearl' = "burgandy_pearl",
'emperor' = "emperor_francis", ## probably this, same gt
'schneiders' = "schneiders_spate_knorpelkirsche", ## same gt, aka nordwunder
'ziraat_0900' = "0900_ziraat",
'early_korvik' = "korvik" ## same gt
)
## not found:
    ## 'tulare' = ""
    ## 'st_margaret', = ""
## 'black_douglas', = ""  ## not same as sir douglas
## 'simone', 'bing', 'burgsdorf', 'empress', 'rons_seedling', 'supreme', 'australise', 'spc335', 'spc276', 'ny_13696', 'ny_13788', 'ny_13791', 'spc414', 'spc411', 'spc424', 'sms_290', 'bf_9', 'spc234', 'sofia_spc106', 'spc342', 'ny_412068', 'ny_564', 'sms_33', 'sms_311', 'pc_7064_3', 'pc_7616_4', 'ny_270', 'ny_2131', 'ny_7690', 'ny_9801', 'ny_413087', 'ny_414205', 'pc_7309_4', 'pc_8008_1', 'ny_410213', 'ny_412113', 'ny_9295', 'pc_7636_1', = "" 
anfic_bt$var <- query_label(anfic_bt$var, varnames_tmp)


## UK Pollination groups -------------------
## fread("cherries_pollination_groups_uk.csv")
## Notes:
## If a range of pollination group is given, eg "2 to 3", this seems to indicate that matching partners must be in the range 2 to 3, ie there is a variation/uncertainty in blooming time, tot a long blooming time
## "2-3" overlaps with "3-4" but not "4"?
## "3 to 4 = late April to early May"
## Knights early black: "Only really suitable for growing in warmer parts of the UK because blossom first appears on average in the second to third week of April (pollination group 2 to 3)."
## Lapins: "second to third week of April (pollination group 2 to 3)."

uk_bt <- fread("cherries_pollination_groups_uk.csv")
uk_bt[, var := tolower(variety)]
uk_bt[, var := gsub(" ", "_", var)]

## curate var names
## Note: already matching, although some are missing from genotype data

## curate variables
uk_bt[, pollination_uk_num := pollination_aggr]
uk_bt <- uk_bt[, .(var, pollination_aggr)] ## select only aggr bt
## set interval bt to a decimal
uk_bt[, pollination_aggr := gsub("2 to 3", "2.5", pollination_aggr)]
uk_bt[, pollination_aggr := gsub("3 to 4", "3.5", pollination_aggr)]
uk_bt[, pollination_aggr := gsub("4 to 5", "4.5", pollination_aggr)]
uk_bt[, pollination_aggr := gsub(" ", "", pollination_aggr)] ## squish
uk_bt[, pollination_aggr := as.numeric(pollination_aggr)]

names(uk_bt) <- c("var", "blooming_group_uk")
## uk_bt[, blooming_group := factor(blooming_group, ordered = TRUE, labels = c("Early", "Early mid", "Mid", "Late mid", "Late"))] ## dont use with decimals

## ## test matches and misses
## uk_bt$var[!uk_bt$var %in% unique(variety_genotype_group$var)]
## uk_bt$var[uk_bt$var %in% unique(variety_genotype_group$var)]
## ## Notes on missing:
## avium but not in genome data:
## "knights_early_black"
## "petit_noir"
## "amber_heart" ## Syn 'Kent Bigarreau'
## "may_duke" ## Syn. 'Dubbele Meikers'    noted as "sour cherry" by some. "Duke cherry" It is a cross between Prunus avium and Prunus cerasus?
## "stardust_coveu" ## self-fertile white cherry
## ## cesarus/sour:
## "morello"
## "nabella"
## Note: these from uk data are not in anfic:
## SUMMER SUN, ## NAPOLEON, ## MERTON BIGARREAU, ## MERTON GLORY, ## PENNY, ## PETIT NOIR, ## AMBER HEART, ## MAY DUKE, ## SASHA, ## KARINA, ## SASHA, 

## from rosbreed script (quintiles of bt and gdd) ------
rosbreed_bt <- fread("rosbreed_bt.csv")

## European data ----------------------

## dataset
eur_bt <- fread("sweet_cherry_phenology_data_1978_2015.csv")
## sanitize names
names(eur_bt) <- tolower(names(eur_bt))
names(eur_bt) <- gsub(" \\(", "_", names(eur_bt))
names(eur_bt) <- gsub("\\)", "", names(eur_bt))
names(eur_bt) <- gsub(" ", "_", names(eur_bt))

## explore
eur_bt[!is.na(beginning_of_flowering) & is.na(beginning_of_flowering_date), ]
eur_bt[is.na(beginning_of_flowering) & beginning_of_flowering_date != "", ]
## Note: some have bt date but not time
## str(eur_bt)
## unique(eur_bt$beginning_of_flowering_date)

## "" to NA
eur_bt[beginning_of_flowering_date == "", beginning_of_flowering_date := NA]
eur_bt[full_flowering_date == "", full_flowering_date := NA]
eur_bt[end_of_flowering_date == "", end_of_flowering_date := NA]

## fix date vars
eur_bt[!is.na(beginning_of_flowering_date), beginning_of_flowering_date := paste0(year, "/", beginning_of_flowering_date)]
eur_bt[!is.na(full_flowering_date), full_flowering_date := paste0(year, "/", full_flowering_date)]
eur_bt[!is.na(end_of_flowering_date), end_of_flowering_date := paste0(year, "/", end_of_flowering_date)]
eur_bt[, beginning_of_flowering_date := as.Date(beginning_of_flowering_date, "%Y/%d/%m")]
eur_bt[, end_of_flowering_date := as.Date(end_of_flowering_date, "%Y/%d/%m")]
eur_bt[, full_flowering_date := as.Date(full_flowering_date, "%Y/%d/%m")]
eur_bt[, startofyear := paste0(year, "/01/01")] 
eur_bt[, startofyear := as.Date(startofyear, "%Y/%d/%m")]

eur_bt[, bt_start := difftime(beginning_of_flowering_date, startofyear)]
eur_bt[, bt_full := difftime(full_flowering_date, startofyear)]
eur_bt[, bt_end := difftime(end_of_flowering_date, startofyear)]

eur_bt[, bt_start := as.numeric(bt_start)]
eur_bt[, bt_full := as.numeric(bt_full)]
eur_bt[, bt_end := as.numeric(bt_end)]

## c("site", "year", "cultivar")

## rename var to match genotype data
## eur_bt[, clone := "  Clone  "]
eur_bt[, var := cultivar]
eur_bt[, var := tolower(var)]
eur_bt[, var := gsub(" ", "_", var)]
eur_bt[, var := gsub("'", "", var)]
## eur_bt[, unique(var)]

## explore misses
nomatch <- eur_bt$var[!eur_bt$var %in% variety_genotype_group$var]
nomatch <- unique(nomatch)
paste(nomatch, collapse = ", ")
cols <- c("variety", "var", "genotype") ## , "genotype"
variety_genotype_group[grepl("yna", tolower(variety)), ..cols]

varnames_tmp <- c(  ## from (eur) = to (genotype data)
'satin_sumele' = "satin",
'bellise_bedel' = "bellise",
'ziraat' = "0900_ziraat",
'rainier_sport' = "rainier",
'imperiale' = "imperiale_blancale",
'belge' = "ferrovia",
'sweetheart_sumtare' = "sweetheart",
'duroni_3' = "durone_nero_3",
'hedelfingen' = "hedelfinger",
## 'simone_chaleat' = "", ## simonis?
'stark_hardy_giant' = "starking_hardy_giant",
## 'vesseau' = "",
## 'bargioni' = "",
## 'cerise_corse' = "",
## 'cerise_montmorency' = "",
'verdel_ferbolus' = "ferbolus",
'arcina_fercer' = "fercer",
## 'maipot' = "",
'marvin_niram' = "marvin",
'simcoe_probla' = "probla",
'tragana_dedessa' = "tragana_dedessa",
'celeste_sumpaca' = "celeste",
## 'lodi_large_red' = "",
## 'delice_de_malicorne_agoudel' = "",
'durona_di_cesena' = "durone_di_cesena",
## 'fougerouse' = "",
## 'graffioni' = "",
'cristalina_sumnue' = "cristalina",
## 'napoleon_compact' = "",
'canada_giant_sumgita' = "canada_giant",
## 'ruby_maru' = "",
## 'vicentina' = "",
'primulat_ferprime' = "primulat",
'new_moon_sumini' = "sumini",
'samba_sumste' = "samba",
'sonata_sumleta' = "sonata",
'coralise_gardel' = "coralise",
'earlise_rivedel' = "earlise",
## 'bianca_di_verona' = "",
## 'sabina' = "",
## 'successa' = "",
'early_red_maraly' = "early_garnet",
## 'bouarkoub' = "",
## 'uriase_de_bistrita' = "",
## 'pico_color' = "",
## 'peter' = "",
'bigalise_enjidel' = "bigalise",
## 'babelle' = "",
## 'arodel' = "",
## 'ferrador' = "",
## 'firm_red_marim' = "",
## 'belle_de_fabrega' = "",
## 'baia' = "",
## 'knuthenborg' = "",
## 'kavics' = "",
## 'annelone' = "",
## 'vittoria' = "",
## 'sumcoja' = "",
## 'enrica' = "",
'weisse_herzkirche' = "weisse_herzkirsche",
## 'alex' = "", ## alexus?
## 'renaldi' = "",
## 'guillaume' = "",
## 'ero' = "",
## 'tardif_de_vignola' = "durone_nero_ii",
## 'turca' = "",
## 'ferracida' = "",
## 'zaicourtif' = "",
## 'sharo' = "",
'durone_nero_iii' = "durone_nero_3",
## 'kunzego' = "",
## 'grossa_gamba' = "",
## 'sumcoro' = "",
## 'vigred' = "",
## 'panthere' = "",
## 'di_vignola_primo' = "",
## 'olympic' = "",
## 'tanguy' = "",
## 'bing' = "",
'tragana_edessis_agra' = "tragana_dedessa",
## 'sandar' = "",
## 'cerella' = "",
## 'gubinska_czarna' = "", ## czarna_pozna ?
## 'kutahya' = "",
## 'francesca' = "",
## 'negus' = "",
## 'giulietta' = "",
'pico_l_negro' = "pico_negro"
## 'montmorency' = "",
## 'durona_di_arezzo' = "",
## 'late_maria' = "",
## 'sweet_ann' = "",
## 'girodel' = "",
## 'lucyna' = ""    
)
eur_bt$var <- query_label(eur_bt$var, varnames_tmp)


## variable types
eur_bt[, year := as.factor(year)]
eur_bt[, site := as.factor(site)]

## agregate
## Note: Aggregate using median to avoid data entry errors?

## eur_bt[, .(bt_start_q = cut(bt_start, breaks = quantile(bt_start, probs = seq(0, 1, 1/5), na.rm = TRUE), include.lowest = TRUE)  ),
##                 by = list(var, site, year)]

tmp <- eur_bt[site == "Vienna", .(var, site, year, bt_start, first_bt_start)]
View(tmp)
## calculate the bt relative to the lowest bt

eur_bt_aggr <- eur_bt
eur_bt_aggr[, first_bt_start := min(bt_start), by = c("var", "site", "year")] 

eur_bt_aggr <- eur_bt[, .(
    bt_start = bt_start - min(bt_start, na.rm = TRUE),
    bt_full = bt_full - min(bt_full, na.rm = TRUE),
    bt_end = bt_end - min(bt_end, na.rm = TRUE)
),
                by = list(var, year, site)]

eur_bt_aggr

eur_bt_aggr <- eur_bt[, .(
    bt_start = mean(bt_start, na.rm = TRUE),
    bt_full = mean(bt_full, na.rm = TRUE),
    bt_end = mean(bt_end, na.rm = TRUE)
),
                by = list(var, year, site)]



## take the maen bt for each var and site (removing year)
eur_bt_aggr <- eur_bt[, .(
    bt_start = mean(bt_start, na.rm = TRUE),
    bt_full = mean(bt_full, na.rm = TRUE),
    bt_end = mean(bt_end, na.rm = TRUE)
),
                by = list(var, site)]


## calculate bt groups by var, site and year
eur_bt_quintile <- eur_bt[, .(
    bt_start_q = .bincode(bt_start, breaks = quantile(bt_start, probs = seq(0, 1, 1/5), na.rm = TRUE), include.lowest = TRUE),
    bt_full_q = .bincode(bt_full, breaks = quantile(bt_full, probs = seq(0, 1, 1/5), na.rm = TRUE), include.lowest = TRUE),
    bt_end_q = .bincode(bt_end, breaks = quantile(bt_end, probs = seq(0, 1, 1/5), na.rm = TRUE), include.lowest = TRUE)
    ),
                by = list(var, site, year)]

## take the mean quintile for each var (removing site and year)

eur_bt_quintile <- eur_bt_quintile[, .(
    bt_start_q = round(mean(bt_start_q, na.rm = TRUE), digits = 0),
    bt_full_q = round(mean(bt_full_q, na.rm = TRUE), digits = 0),
    bt_end_q = round(mean(bt_end_q, na.rm = TRUE), digits = 0)
),
                by = list(var)]

View(eur_bt_quintile)


str(eur_bt_quintile)
eur_bt_quintile[!is.na(bt_start_q) & !is.na(bt_full_q) & bt_start_q != bt_full_q, ]

quantile(eur_bt$bt_start, probs = seq(0, 1, 1/5), na.rm = TRUE)

eur_bt[, .(bt_start = mean(bt_start, na.rm = TRUE)),
                by = list(var, site, year)]
unique(eur_bt$site)


## select variables
bloom_table_eur <- eur_bt[, .(var, site, year, bt_start, bt_full, bt_end)]





str(eur_bt)
names(eur_bt)



## Metadata:
## Sweet cherry phenology data: 1978 - 2015
## Ref: Wenden, B., Campoy, J., Lecourt, J. et al. A collection of European sweet cherry phenology data for assessing climate change. Sci Data 3, 160108 (2016). https://doi.org/10.1038/sdata.2016.108
## https://doi.org/10.1038/sdata.2016.108
## [A collection of European sweet cherry phenology data for assessing climate change](https://www.nature.com/articles/sdata2016108)
## - [Data](https://datadryad.org/stash/dataset/doi:10.5061/dryad.1d28m)
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



## compare with anfic ------------------
tmp <- anfic_bt[rosbreed_bt, on = "var"][, .(var, blooming_group_anfic, bt_quintile, gdd_quintile)]
tmp[, anfic_bg := as.numeric(blooming_group_anfic)][, .(var, anfic_bg, bt_quintile, gdd_quintile)]



## more ------------------





## fread("bbch_scale_stone_fruit.csv")

## fread("cherries_gardenfocused_uk_varieties_description.csv") ## long format, has pollination group etc


## Temperature data SMHI

## SMHI väderdata, ladda ner mätpunkter(https://www.smhi.se/data/meteorologi/ladda-ner-meteorologiska-observationer/#param=airtemperatureInstant,stations=core,stationid=95160)

## temp_orebro <- fread("smhi_temp_orebro.csv")
## temp_orebro[, date := as.character(date)]

## temp_orebro[, year := strsplit(date, "-")[[1]]]
## ??as.date

## ## Metadata smhi_temp_orebro.csv
## Stationsnamn	Stationsnummer	Stationsnät	Mäthöjd (meter över marken)	
## Örebro	95160	SMHIs stationsnät	2	
## Parameternamn	Beskrivning	Enhet		
## Lufttemperatur	momentanvärde, 1 gång/tim	celsius		
				
## Tidsperiod (fr.o.m)	Tidsperiod (t.o.m)	Höjd (meter över havet)	Latitud (decimalgrader)	Longitud (decimalgrader)
## 1858-12-01 00:00:00	1964-04-30 23:59:59	36	59.2448	15.2854
## 1964-05-01 00:00:00	1971-10-19 23:59:59	36	59.2448	15.2854
## 1971-10-01 00:00:00	1988-03-31 23:59:59	31	59.2448	15.2854
## 1988-05-01 00:00:00	2005-06-30 23:59:59	35	59.2782	15.1574

## Tidsutsnitt:
## Kvalitetskontrollerade historiska data (utom de senaste 3 mån)
## Tidsperiod (fr.o.m.) = 1858-12-01 00:00:00 (UTC)
## Tidsperiod (t.o.m.) = 2005-06-30 23:59:59 (UTC)
## Samplingstid = Ej angivet
## Kvalitetskoderna:
## Grön (G) = Kontrollerade och godkända värden.
## Gul (Y) = Misstänkta eller aggregerade värden. Grovt kontrollerade arkivdata och okontrollerade realtidsdata (senaste 2 tim).
## Nätinformation:
## SMHIs Stationsnät: Data samlas in och lagras i SMHIs databaser. Data kvalitetskontrolleras vilket innebär att felaktiga data korrigeras och att databortfall kompletteras utifrån expertbedömning där det är möjligt. De flesta stationerna övervakas, inspekteras och underhålls löpande av SMHI.
## Övriga stationer: Data samlas in och lagras i SMHIs databaser. Datakvaliteten är för SMHI okänd då SMHI varken utför kvalitetskontroll på data eller inspektioner på stationerna.
## Möjliga orsaker till saknade data:
## - stationen eller givaren har varit ur funktion.
## - stationen har endast levererat värden med kvalitetskod Röd (R). Dessa levereras ej.


