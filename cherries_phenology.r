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


## Blooming time ------------------------------

## from rosbrred script (quintiles of bt and gdd) ------
rosbreed_bt <- fread("rosbreed_bt.csv")
## compare with anfic
tmp <- anfic_bt[rosbreed_bt, on = "var"][, .(var, blooming_group_anfic, bt_quintile, gdd_quintile)]
tmp[, anfic_bg := as.numeric(blooming_group_anfic)][, .(var, anfic_bg, bt_quintile, gdd_quintile)]

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














## fread("bbch_scale_stone_fruit.csv")

## fread("cherries_gardenfocused_uk_varieties_description.csv") ## long format


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






## Scientific data --------------------------

## dataset
## dta_phenology <- fread("sweet_cherry_phenology_data_1978_2015.csv")

## Sweet cherry phenology data: 1978 - 2015

## Metadata

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
