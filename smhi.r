## Read and curate SMHI data
pacman::p_load(rvest, data.table)

## read smhi data

## sites info
sites <- fread("metobs_airtemp_sites.csv")
names(sites) <- c("id", "name", "lat", "long", "alt", "active")
sites$name <- factor(sites$name)
## Data on airtemp measuring sites sweden
length(sites$id) ## 920 sites

chunks <- c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900)

chunks <- c(0, 10, 20, 30)
## n <- 1
## n <- 3
for(n in 1:length(chunks)){
## loop over site id to get data

    smhi_airtemp <- data.table(date = NA, time = NA, temp = NA, quality = NA, id = NA)

    from <- chunks[n]+1    
    to <- ifelse(chunks[n] == max(chunks),
                 40,
                 ## length(sites$id),
                 chunks[n+1])
for(i in sites$id[from:to]){
    ## i <- sites$id[3]
    ## i <- "98040"
    ## i <- 66420
    ## i <- 125490
    ## Error in read.table("temp.txt", sep = ";") : no lines available in input
    message(i)
    ## min max (vanligen kl 06 och 18)
    ## link <- paste0("https://opendata-download.smhi.se/stream?type=metobs&parameterIds=26,27&stationId=", i, "&period=corrected-archive") ## 12 h min max, note: check csv, not in order

    ## alla mätningar
    link <- paste0("https://opendata-download.smhi.se/stream?type=metobs&parameterIds=1&stationId=", i, "&period=corrected-archive")

    page_html <- rvest::read_html(link)
    test <- page_html %>% rvest::html_nodes("body")
    text <- html_text(test)
    text <- gsub("^.* Dessa levereras ej..", "", text) ## remove intro
    ## example <- "1858-12-05;13:00:00;1.4;G\n1858-12-05;20:00:00;0.6;G\n1858-12-06;07:00:00;0.8;G\n"
    ## writeLines(example, "example.txt")
    if(text != ""){ ## test if empty source
        writeLines(text, "temp.txt")

        ## if(i == 98040){
        ##     incomplete <- readLines("temp.txt", n = 376019) ## line 376020 incomplete
        ##     writeLines(incomplete, "temp.txt")
        ##     message(paste0(i, " has an incomplete line"))
        ## }
        
        ##     if(i == 74180){
        ##             incomplete <- readLines("temp.txt", n = 375805) ## line 375806 did not have 4 elements
        ##     writeLines(incomplete, "temp.txt")
        ##     message(paste0(i, " has an incomplete line"))

        ## } ## Note: using fill = TRUE instead. Seems to be last line
        result <- read.table("temp.txt", sep = ";", fill = TRUE)
        names(result) <- c("date", "time", "temp", "quality")
        ## result[376090:376094, ]
        ## nrow(result)
        result$id <- i
        smhi_airtemp <- rbind(smhi_airtemp, result)
    }
}
    smhi_airtemp <- smhi_airtemp[!is.na(date), ]
    saveRDS(smhi_airtemp, paste0("smhi_airtemp_", to, ".rds"))
    }

## test <- readRDS("smhi_airtemp_20.rds")

## join with sites info
## Note: only add label to reduce size of data?
smhi_airtemp_curated_1 <- sites[, .(id, name)][smhi_airtemp, on = "id"]
saveRDS(smhi_airtemp_curated_1, "smhi_airtemp_curated_1.rds")
## 206

is.data.table(smhi_airtemp_curated)
x

## last(sites$id)
## "3119590"
## x


############ Metadata #####################

## SMHI väderdata, ladda ner mätpunkter(https://www.smhi.se/data/meteorologi/ladda-ner-meteorologiska-observationer/#param=airtemperatureInstant,stations=core,stationid=95160)
## Med våra öppna data följer licensvillkoren Creative commons Erkännande 4.0 SE. Licensvillkoren innebär att du har tillstånd att kopiera och distribuera våra öppna data samt skapa bearbetningar. Detta är tillåtet även för kommersiella ändamål. Du ska ange SMHI som källa och även ange om du har ändrat i licensmaterialet.

## Stationsnamn;Stationsnummer;Stationsnät;Mäthöjd (meter över marken)\nÖrebro;95160;SMHIs stationsnät;2.0\n\nParameternamn;Beskrivning;Enhet\nLufttemperatur;momentanvärde, 1 gång/tim;celsius\n\nTidsperiod (fr.o.m);Tidsperiod (t.o.m);Höjd (meter över havet);Latitud (decimalgrader);Longitud (decimalgrader)\n1858-12-01 00:00:00;1964-04-30 23:59:59;36.0;59.2448;15.2854\n1964-05-01 00:00:00;1971-10-19 23:59:59;36.0;59.2448;15.2854\n1971-10-01 00:00:00;1988-03-31 23:59:59;31.0;59.2448;15.2854\n1988-05-01 00:00:00;2005-06-30 23:59:59;35.0;59.2782;15.1574\n\nDatum;Tid (UTC);Lufttemperatur;Kvalitet;;Tidsutsnitt:\n1858-12-01;07:00:00;0.2;G;;Kvalitetskontrollerade historiska data (utom de senaste 3 mån)\n1858-12-01;13:00:00;1.0;G;;Tidsperiod (fr.o.m.) = 1858-12-01 00:00:00 (UTC)\n1858-12-01;20:00:00;0.9;G;;Tidsperiod (t.o.m.) = 2005-06-30 23:59:59 (UTC)\n1858-12-02;07:00:00;0.8;G;;Samplingstid = Ej angivet\n1858-12-02;13:00:00;1.4;G;;Kvalitetskoderna:\n1858-12-02;20:00:00;1.0;G;;Grön (G) = Kontrollerade och godkända värden.\n1858-12-03;07:00:00;1.0;G;;Gul (Y) = Misstänkta eller aggregerade värden. Grovt kontrollerade arkivdata och okontrollerade realtidsdata (senaste 2 tim).\n1858-12-03;13:00:00;1.6;G;;Nätinformation:\n1858-12-03;20:00:00;0.8;G;;SMHIs Stationsnät: Data samlas in och lagras i SMHIs databaser. Data kvalitetskontrolleras vilket innebär att felaktiga data korrigeras och att databortfall kompletteras utifrån expertbedömning där det är möjligt. De flesta stationerna övervakas, inspekteras och underhålls löpande av SMHI.\n1858-12-04;07:00:00;1.4;G;;Övriga stationer: Data samlas in och lagras i SMHIs databaser. Datakvaliteten är för SMHI okänd då SMHI varken utför kvalitetskontroll på data eller inspektioner på stationerna.\n1858-12-04;13:00:00;2.4;G;;Möjliga orsaker till saknade data:\n1858-12-04;20:00:00;0.7;G;;- stationen eller givaren har varit ur funktion.\n1858-12-05;07:00:00;1.6;G;;- stationen har endast levererat värden med kvalitetskod Röd (R). Dessa levereras ej.

## https://opendata-download.smhi.se/stream?type=metobs&parameterIds=1&stationId=95160&period=corrected-archive

######################



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
