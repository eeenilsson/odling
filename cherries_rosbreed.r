

## Data downloaded -------
## https://www.rosaceae.org/search/quantitative_traits
## Select trait => Search => Push "table" to download csv

## Data join test
## Note: "#" is not from source data, only row number in download interface
bloom_days <- fread("Bloom_Days.csv")
acidity <- fread("TA.csv")
## bloom_days ## 631 rows
## acidity ## 521 rows
## bloom_days[acidity, on = c("Germplasm", "Species", "Dataset")] ## 521 rows
bloom_days[["#"]] <- NULL
acidity[["#"]] <- NULL
dt <- merge(bloom_days, acidity, by = c("Germplasm", "Species", "Dataset"), all = TRUE)

## data join loop

filenam <- c( ## list of files to add
    ## "Bloom_Days.csv", ## skip the first
    "TA.csv", "Fruit_Dim.csv", "Flesh_C.csv", "Spec_Flesh_Color_b.csv", "Spec_Flesh_Color_a.csv", "Bulked_Fruit_SSC.csv", "Pull_Force.csv", "Pit_Lh.csv", "Spec_Skin_Color_a.csv", "Skin_C_mahogany.csv", "Spec_Skin_Color_b.csv", "Spec_Skin_Color_L.csv", "Bulked_Fruit_Firmness.csv", "FreeStone.csv", "Perc_Cracking.csv", "Fruit_Wt.csv", "SSC.csv", "Perc_Pitting.csv", "Spec_Flesh_Color_L.csv" , "Pit_Wt.csv", "Fruit_Wd2.csv", "Pit_Wd1.csv", "Harvest_Time.csv", "Stem_length.csv", "Foliar_PM.csv", "Bloom_Time.csv",
    "Fruit_L.csv",
    "Bulked_Fruit_Wt.csv",
    "Harvest_Days.csv",
    "Skin_C_blush.csv",
    "Firmness_1.csv", ## Note: Some values in "Germplasm" are dates, probably a data entry error
   "pH.csv",
   "Fruit_Shape.csv", "Harvest_Date.csv", "Bloom_Date.csv"
)

## i <- filenam[2]
dt <- fread("Bloom_Days.csv") ## start with one
dt[["#"]] <- NULL ## remove unnecessary col
for (i in filenam){
tmp <- fread(i)
tmp[["#"]] <- NULL ## remove unnecessary col
dt <- merge(dt, tmp, by = c("Germplasm", "Species", "Dataset"), all = TRUE)
}

str(dt)

write.csv(tmp, "rosbreed_alldata.csv")

unique(tmp$Germplasm)

unique(tmp$Dataset)

dt[1, ]

## for (i in filenam){
##     tmp <- fread(i)
## tmp[["#"]] <- NULL ## remove unnecessary col

## print(i)
## print(names(tmp))
## }





## Metadata --------------------------------

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


## Publications on specific traits --------------------

## Fruit cracking and firmness
## Detection of Breeding-Relevant Fruit Cracking and Fruit Firmness Quantitative Trait Loci in Sweet Cherry via Pedigree-Based and Genome-Wide Association Approaches, Crumb 2022
## https://www.frontiersin.org/articles/10.3389/fpls.2022.823250/full
## "RosBREED seedlings, planted in 2006–2008 and grown on ‘Gisela 6’ rootstocks, were chosen through a stratified random sample, the strata being defined by the previous firmness classifications by Cai et al. (2019), in order to ensure relatively equal firmness-class representation."


## Notes ------

## Market intermediaries indicated a willingness to pay producers more per pound for fruit greater than 2.5 cm in diameter, firmness above 300 g/mm, and SSC above 18 °Brix. https://bmcgenomdata.biomedcentral.com/articles/10.1186/s12863-018-0609-8

## high heritability traits, such as fruit skin color and self-compatibility [13,14,15,16]. The Washington State University breeding program has seen genetic gains in fruit dimensions, firmness and other traits of breeding relevance due to moderate heritability of those traits

## Sweet cherry has a juvenility period of three to five years before a tree is capable of flowering and producing fruit [20]
