pacman::p_load(data.table, ggplot2)

## Data ------

## downloaded thusly
## https://www.rosaceae.org/search/quantitative_traits
## Select trait => Search => Push "table" to download csv

## ## data join loop (load joined data below)
## filenam <- c( ## list of files to add
##     ## "Bloom_Days.csv", ## skip the first
##     "TA.csv", "Fruit_Dim.csv", "Flesh_C.csv", "Spec_Flesh_Color_b.csv", "Spec_Flesh_Color_a.csv", "Bulked_Fruit_SSC.csv", "Pull_Force.csv", "Pit_Lh.csv", "Spec_Skin_Color_a.csv", "Skin_C_mahogany.csv", "Spec_Skin_Color_b.csv", "Spec_Skin_Color_L.csv", "Bulked_Fruit_Firmness.csv", "FreeStone.csv", "Perc_Cracking.csv", "Fruit_Wt.csv", "SSC.csv", "Perc_Pitting.csv", "Spec_Flesh_Color_L.csv" , "Pit_Wt.csv", "Fruit_Wd2.csv", "Pit_Wd1.csv", "Harvest_Time.csv", "Stem_length.csv", "Foliar_PM.csv", "Bloom_Time.csv",
##     "Fruit_L.csv",
##     "Bulked_Fruit_Wt.csv",
##     "Harvest_Days.csv",
##     "Skin_C_blush.csv",
##     "Firmness_1.csv", ## Note: Some values in "Germplasm" are dates, probably a data entry error
##    "pH.csv",
##    "Fruit_Shape.csv", "Harvest_Date.csv", "Bloom_Date.csv"
## )

## ## i <- filenam[2]
## dt <- fread("Bloom_Days.csv") ## start with one
## dt[["#"]] <- NULL ## remove unnecessary col
## for (i in filenam){
## tmp <- fread(i)
## tmp[["#"]] <- NULL ## remove unnecessary col
## dt <- merge(dt, tmp, by = c("Germplasm", "Species", "Dataset"), all = TRUE)
## }
## write.csv(dt, "rosbreed_alldata.csv")

## read joined data
ros <- fread("rosbreed_alldata.csv")
## paste(unique(ros$Germplasm), collapse = ", ")

## select and curate data
ros <- ros[grepl("23-", Germplasm) == FALSE, ] ## remove probable data entry error
## ros <- ros[grepl("FR", Germplasm) == FALSE, ] ## remove unwanted strains
## ros <- ros[grepl("PC", Germplasm) == FALSE, ] ## remove unwanted strains
## ros <- ros[grepl("Unk", Germplasm) == FALSE, ] ## remove unwanted strains

## select common varieties (those with a non-numeric name)
common_varieties <- c("Ambrunes", "Benton", "Bing", "Black Republican", "Cashmere", "Chelan", "Chinook", "Cowiche", "Emperor Francis", "Gil Peck", "Glacier", "Index", "Kiona", "Kordia", "Krupnoplodnaya", "Lambert", "Lapins", "Moreau", "Olympus", "Rainier", "Regina", "Schmidt", "Schneiders", "Selah", "Stella", "Summit", "Sunburst", "Sweetheart", "Tieton", "Van", "Venus", "Vic", "Windsor")
ros <- ros[grepl(paste0(common_varieties, collapse = "|"), Germplasm), ] 

## redundant:
## for (i in filenam){
##     tmp <- fread(i)
## tmp[["#"]] <- NULL ## remove unnecessary col
## print(i)
## print(names(tmp))
## }

## curate data types
## str(ros)
ros[["V1"]] <- NULL
ros[, Germplasm := as.factor(Germplasm)]
ros[, Species := as.factor(Species)]
ros[, Dataset := as.factor(Dataset)]

## check date/time variables
ros[, Harvest_Date := as.Date(Harvest_Date, "%m/%d/%Y")]
ros[, Bloom_Date := as.Date(Bloom_Date, "%m/%d/%Y")]
## ros[, as.Date(Harvest_Date, "%m/%d/%y")]
## as.Date("7/24/2010", "%m/%d/%Y")

## calculate relative times
ros[, startofyear := paste0(format(as.Date(Harvest_Date, format="%Y-%m-%d"),"%Y"), "-01-01")]
ros[, startofyear := as.Date(startofyear)]
ros[, harvest_time_relative := difftime(Harvest_Date, startofyear)]

## ## Check date variables (keep this comment)
## ros[, .(Harvest_Date, Harvest_Days, Harvest_Time, harvest_time_relative)]
## ## Note: Harvest_Date = Date, Harvest_Days ~160-190 range, Harvest_Time ~ 800-1000 range with decimal places. 
## ros[, .(Bloom_Date, Bloom_Days, Bloom_Time)]
## ## Norge: "First bloom required 221 Baskerville-Emin Growing degree days (GDD)" Note: Detta stämmer ungefär med "Bloom_Time"-värdena

## vars to select cols
vars_bloom <- c("Bloom_Date", "Bloom_Days", "Bloom_Time")
vars_harvest <- c("Harvest_Date", "Harvest_Days", "Harvest_Time")
vars_id <- c("Germplasm", "Species", "Species", "Dataset")

## extract some vars
selectvars <- c(c(vars_id, vars_harvest))
ros_harvest <- ros[, ..selectvars]
selectvars <- c(c(vars_id, vars_bloom))
ros_bloom <- ros[, ..selectvars]

## aggregate the datasets from different years
## Note: Aggregate using median to avoid data entry errors?
bloom_table <- ros[, .(bt = mean(Bloom_Days, na.rm = TRUE),
        gdd = mean(Bloom_Time, na.rm = TRUE)),
    by = Germplasm]

bloom_table <- na.omit(bloom_table)

## calculate proportions etc
bloom_table[, bt_prop := round(bt/max(bt), digits = 2)]
bloom_table[, bt0 := round(bt-min(bt), digits = 0)]
bloom_table[, gdd_prop := round(bt/max(bt), digits = 2)]
bloom_table[, gdd0 := round(gdd-min(gdd), digits = 0)]

## bloom_table[order(bt), ] ## order by bt

bloom_table[, Germplasm := factor(Germplasm, levels = bloom_table[order(-bt), Germplasm])]

## calculate relative blooming group
## Note: Quintiles might put varieties on the extremes of bt in the same group as others
## maybe better to break by 3 day intervals? or remove outliers
## boxplot(gdd ~ Germplasm, data = bloom_table)

## break bt into quintiles
bloom_table[, bt_quintile := cut(bt, breaks = quantile(bt, probs = seq(0, 1, 1/5)), include.lowest = TRUE)]
bloom_table[, bt_quintile := factor(bt_quintile, ordered = TRUE, labels = c(1:5))]
bloom_table[, gdd_quintile := cut(gdd, breaks = quantile(gdd, probs = seq(0, 1, 1/5)), include.lowest = TRUE)]
bloom_table[, gdd_quintile := factor(gdd_quintile, ordered = TRUE, labels = c(1:5))]
bloom_table[, .(Germplasm, bt, gdd, bt_quintile, gdd_quintile)]
## Note: Some have very different bt q vs gdd q

## curate var names to match genotype data ---------------
rosbreed_bt <- bloom_table
rosbreed_bt[, var := tolower(Germplasm)]
rosbreed_bt[, var := gsub(" ", "_", var)]
## ## test matches and misses
## nomatch <- rosbreed_bt$var[!rosbreed_bt$var %in% variety_genotype_group$var]
## rosbreed_bt$var[unique(rosbreed_bt$var) %in% unique(variety_genotype_group$var)]
## ## note: many misses, todo: explore this
## cols <- c("variety", "var", "genotype") ## , "genotype"
## variety_genotype_group[grepl("bin", tolower(variety)), ..cols]
## rosbreed_bt[grepl("douglas", var), ]
## nomatch
## rename rosbreed var to match genotype data
varnames_tmp <- c(  ## from (rosbreed) = to (genotype data)
    'krupnoplodnaya' = "krupnoplidna",
    'schneiders' = "schneiders_spate_knorpelkirsche" ## same gt
    ## "bing"  ## missing from genotype
)
rosbreed_bt$var <- query_label(rosbreed_bt$var, varnames_tmp)
rosbreed_bt <- rosbreed_bt[, .(var, bt_quintile, gdd_quintile)]
## write.csv(rosbreed_bt, "rosbreed_bt.csv", row.names = FALSE)


## cherry_pollenizers.csv  from https://treeconnect.com/cherry-tree-pollenizer/

## Most sweet cherry varieties are:

##     self-unfruitful (self-incompatible, SI) – which require cross-pollination with another variety as the pollen source

## Some varieties (e.g. Bing, Lambert, Royal Ann/Napoleon) are:

##     cross-unfruitful – which cannot be depended upon to provide pollen for each other

## Other varieties (e.g. Index, Lapins, Skeena, Sweetheart, White Gold, Sonata, Stella, Symphony, Sunburst, and Black Gold) are:

##     self-fruitful (SF) – these can serve as “universal” pollen sources for many self-unfruitful varieties with the same bloom time. Their use as “universal” pollinators should also take bloom timing into consideration.



## Metadata Rosbreed --------------------------------

## Trait descriptors details: https://www.rosaceae.org/bio_data/8433870
## Note: Very incomplete, cureted here rosbreed_trait_descriptors.csv
## https://www.rosaceae.org/sweet_cherry_trait_RB
## List of datasets: https://www.rosaceae.org/trait_descriptor/75694?pane=dataset
## eg "2012 Sweet Cherry CRS phenotypic data"

## GDD: Heat units, expressed in growing degree-days (GDD), are frequently used to describe the timing of biological processes. The basic equation used is GDD = [ (T MAX + T MIN ) 2 ]−T BASE , where TMAX and TMIN are daily maximum and minimum air temperature, respectively, and TBASE is the base temperature.

## Phenotyping Protocol published here (paywalled):
## Chavoshi M, Watkins C, Oraguzie B, Zhao Y, Iezzoni A, Oraguzie N. Phenotyping protocol for sweet cherry (Prunus avium L.) to facilitate an understanding of trait inheritance. Am Pomol Soc. 2014;68:125–34.
## Publications on phenotyping protocols: https://www.rosbreed.org/phenotyping-protocols

## Non-paywalled publication on Rosbreed containing some methods:
## Piaskowski, J., Hardner, C., Cai, L. et al. Genomic heritability estimates in sweet cherry reveal non-additive genetic variance is relevant for industry-prioritized traits. BMC Genet 19, 23 (2018). https://doi.org/10.1186/s12863-018-0609-8
## https://bmcgenomdata.biomedcentral.com/articles/10.1186/s12863-018-0609-8#Sec2

## "We used all individuals from the RosBREED sweet cherry Crop Reference Set with genome-wide SNP data, totaling 505 individuals (Additional file 1). This set consisted of cultivars (n = 42), wild accessions (n = 3), unreleased selections (n = 24), and unselected offspring (n = 436) from 66 families."
## "Trees were grown at two sites in Washington State (U.S.A.) located approximately 0.5 km apart: the Irrigated Agriculture Research and Extension Center of Washington State University Roza Unit, (46 ̊29’N and 119 ̊73’W) and at Pear Acres (46 ̊29’N and 119 ̊75’W). Each tree was planted in 2006, 2007, or 2008 and managed using conventional orchard management practices. Unselected offspring were grown on their own roots, and the remaining germplasm were grown on Gisela 6 rootstock [47]. A single tree was used for each individual. The Crop Reference Set was established to represent North American sweet cherry breeding germplasm for QTL identification and validation and other quantitative genetics endeavors [48].

## Gisela 6 rootstock can impact crop performance by decreasing tree size and increasing crop load, among other effects [49,50,51]. Because only released cultivars and wild germplasm were grown on rootstocks while all seedlings were grown on their own roots, any effect of rootstock was confounded with the effect of seedling versus non-seedling in this study. However, in this study, understanding the relative performance among seedlings or among cultivars was the primary purpose in order to identify possible parents and candidate cultivars."

## This study used the sweet cherry phenotypic data set previously described in Chavoshi et al. [52] obtained in the RosBREED project. This data set consisted of 32 traits evaluated in 2010, 2011, and 2012. Standardized phenotyping protocols for sweet cherry [52] were used. For individual fruit traits, the five largest fruit without blemish were measured and averaged. In the case of pitting and cracking, the proportion of fruit observed with symptoms out of 25 fruit was recorded. Bulked fruit traits (bulked fruit weight, bulked firmness, bulked SSC, and bulked TA) were reported as the average of measurements over 25 fruit.

## Nine traits of the 32 were focused on here because of their importance in new sweet cherry cultivars: time to bloom, time to maturity, pedicel-fruit retention force (PFRF), fruit dimensions, fruit weight, firmness, SSC, TA, and powdery mildew incidence.

## Time to bloom and time to maturity were measured both in Julian calendar days starting from January 1st of the calendar year and in growing degree days (GDD).

## The force required to pull a ripe cherry fruit from its pedicel, PFRF, and fruit weight were both measured in grams.

## Firmness, SSC, and TA were measured in units of g/mm, °Brix, and percentage, respectively.

## Foliar powdery mildew incidence was scored in August of each year, immediately after the fruiting season, on a 0–5 scale, where 0 is no infection and 5 is highly infected leaves. 

## Growing degree days was calculated for an alternative measure of phenological traits. Climatic data was obtained from Washington State University’s AgWeatherNet using the “Roza” station [53], using a base temperature of 4.5 °C and maximum of 30 °C. Daily maximum temperatures above 30 °C were reduced to 30 °C, and negative temperatures were set to zero, following McMaster and Wilhelm

## All trait distributions (consisting of 600–755 data points for each trait) were influenced by the year of data collection (Fig. 1).

## Publications on specific traits --------------------

## Fruit cracking and firmness
## Detection of Breeding-Relevant Fruit Cracking and Fruit Firmness Quantitative Trait Loci in Sweet Cherry via Pedigree-Based and Genome-Wide Association Approaches, Crumb 2022
## https://www.frontiersin.org/articles/10.3389/fpls.2022.823250/full
## "RosBREED seedlings, planted in 2006–2008 and grown on ‘Gisela 6’ rootstocks, were chosen through a stratified random sample, the strata being defined by the previous firmness classifications by Cai et al. (2019), in order to ensure relatively equal firmness-class representation."


## Notes ------

## Market intermediaries indicated a willingness to pay producers more per pound for fruit greater than 2.5 cm in diameter, firmness above 300 g/mm, and SSC above 18 °Brix. https://bmcgenomdata.biomedcentral.com/articles/10.1186/s12863-018-0609-8

## high heritability traits, such as fruit skin color and self-compatibility [13,14,15,16]. The Washington State University breeding program has seen genetic gains in fruit dimensions, firmness and other traits of breeding relevance due to moderate heritability of those traits

## Sweet cherry has a juvenility period of three to five years before a tree is capable of flowering and producing fruit [20]
