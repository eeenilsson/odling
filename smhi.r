## Read and curate SMHI data
pacman::p_load(rvest, data.table, ggplot2)

## read smhi data

## sites info
sites <- fread("metobs_airtemp_sites.csv")
names(sites) <- c("id", "name", "lat", "long", "alt", "active")
sites$name <- factor(sites$name)
## Data on airtemp measuring sites sweden
length(sites$id) ## 920 sites

## ## download temperature data in chunks (large data) ----------
## chunks <- c(0, 100, 200, 300, 400, 500, 600, 700, 800, 900)

## ## chunks <- c(0, 10, 20, 30)
## ## n <- 1
## ## n <- 3
## for(n in 1:length(chunks)){
## ## loop over site id to get data
##     message(chunks[n])
##     smhi_airtemp <- data.table(date = NA, time = NA, temp = NA, quality = NA, id = NA)

##     from <- chunks[n]+1    
##     to <- ifelse(chunks[n] == max(chunks),
##                  ## 40,
##                  length(sites$id),
##                  chunks[n+1])
## for(i in sites$id[from:to]){
##     ## Error in read.table("temp.txt", sep = ";") : no lines available in input
##     ## min max (vanligen kl 06 och 18)
##     ## link <- paste0("https://opendata-download.smhi.se/stream?type=metobs&parameterIds=26,27&stationId=", i, "&period=corrected-archive") ## 12 h min max, note: check csv, not in order

##     ## alla mätningar
##     link <- paste0("https://opendata-download.smhi.se/stream?type=metobs&parameterIds=1&stationId=", i, "&period=corrected-archive")

##     page_html <- rvest::read_html(link)
##     test <- page_html %>% rvest::html_nodes("body")
##     text <- html_text(test)
##     text <- gsub("^.* Dessa levereras ej..", "", text) ## remove intro
##     ## example <- "1858-12-05;13:00:00;1.4;G\n1858-12-05;20:00:00;0.6;G\n1858-12-06;07:00:00;0.8;G\n"
##     ## writeLines(example, "example.txt")
##     if(text != ""){ ## test if empty source
##         writeLines(text, "temp.txt")

##         ## if(i == 98040){
##         ##     incomplete <- readLines("temp.txt", n = 376019) ## line 376020 incomplete
##         ##     writeLines(incomplete, "temp.txt")
##         ##     message(paste0(i, " has an incomplete line"))
##         ## }
        
##         ##     if(i == 74180){
##         ##             incomplete <- readLines("temp.txt", n = 375805) ## line 375806 did not have 4 elements
##         ##     writeLines(incomplete, "temp.txt")
##         ##     message(paste0(i, " has an incomplete line"))

##         ## } ## Note: using fill = TRUE instead. Seems to be last line
##         result <- read.table("temp.txt", sep = ";", fill = TRUE)
##         names(result) <- c("date", "time", "temp", "quality")
##         ## result[376090:376094, ]
##         ## nrow(result)
##         result$id <- i
##         smhi_airtemp <- rbind(smhi_airtemp, result)
##     }
## }
##     smhi_airtemp <- smhi_airtemp[!is.na(date), ]
##     saveRDS(smhi_airtemp, paste0("smhi_airtemp_", to, ".rds"))
##     }
## test <- readRDS("smhi_airtemp_20.rds")

## read data stored in .rds

## airtemp <- readRDS("smhi_airtemp_100.rds") ## A-D
## airtemp <- as.data.table(airtemp)

## Note: Too large data to load everything into memory
## tmp <- c(200, ## V-Ö
## 300, 400, 500, 600, 700, 800, 900, 920)
## for(i in tmp){
## dt_tmp <- readRDS(paste0("smhi_airtemp_", i, ".rds"))
## dt_tmp <- as.data.table(dt_tmp)
## airtemp <- rbind(airtemp, dt_tmp)
## }

airtemp <- readRDS("smhi_airtemp_920.rds") ## Ö
airtemp <- as.data.table(airtemp)


str(airtemp)

## join with sites info
## Note: only add label to reduce size of data?
airtemp_curated <- sites[, .(id, name)][airtemp, on = "id"]
## saveRDS(smhi_airtemp_curated_1, "smhi_airtemp_curated_1.rds")
## 206
str(airtemp_curated)
unique(airtemp_curated$name)

orebro <- airtemp_curated[grepl("Örebro", name), ]
unique(orebro$name)
str(orebro)

orebro <- orebro[quality == "G", ] ## keep only checked values
## summary(orebro[["temp"]])
orebro[, hour := as.numeric(substr(time, 1, 2))] ## hours
## summary(orebro[["hour"]])
orebro[hour == 0, ]
## str(orebro)
orebro[, day_min := min(temp), by = "date"]
orebro[date == "1858-12-05", ]
orebro[, startofyear := format(as.Date(date, format="%Y-%m-%d"), paste0("%Y", "-01-01"))]
orebro[, startofyear := as.Date(startofyear, format="%Y-%m-%d")]
orebro[, date := as.Date(date, format="%Y-%m-%d")]
orebro[, dayofyear := difftime(date, startofyear, unit = "days")]
orebro[, weekofyear := difftime(date, startofyear, unit = "weeks")]
orebro <- orebro[difftime(date, as.Date("1991-01-01", format="%Y-%m-%d")) >0, ] ## select 1991-
orebro[, calendarweek := as.integer(ceiling(weekofyear))]
orebro_s1 <- orebro[dayofyear < 365/2, ]
orebro_s1[, frost := day_min < 0]
orebro_s1[frost == TRUE, last_frost_week := max(calendarweek, na.rm = TRUE), by = startofyear]
orebro_s1[frost == TRUE, last_frost_day := max(dayofyear, na.rm = TRUE), by = startofyear]

## orebro_s1[calendarweek == last_frost_week, ]
## lastfrost <- orebro_s1[dayofyear == last_frost_day, ]
## orebro_s1[date == "1991-05-08", ]

lastfrost <- lastfrost[frost == TRUE, .SD[.N], by="date"][, .(date, startofyear, dayofyear, calendarweek)] ## 1991-2023
lastfrost[, frostdate := NULL]
lastfrost[, mo := format(as.Date(date, format="%Y-%m-%d"), "%m")]
lastfrost[, dy := format(as.Date(date, format="%Y-%m-%d"), "%d")]
lastfrost[mo == "04", ] ## min = 13/4 (dayofyear = 102)
lastfrost[, daysfrom0413 := dayofyear - 102]

lastfrost[, pretendyr := as.Date("1999-01-01", format="%Y-%m-%d")]
lastfrost[, pretendmody := pretendyr + dayofyear]


## plot probability of frost
p <- ggplot(lastfrost, aes(x=pretendmody)) +
    stat_density(kernel="biweight")


p <- ggplot(orebro_s1, aes(x=frost)) +
  geom_histogram(aes(y = ..density..), binwidth=density(df$x)$bw) +
  geom_density(fill="red", alpha = 0.2) +
  theme_bw()

## This R tutorial describes how to create an ECDF plot (or Empirical Cumulative Density Function) using R software and ggplot2 package. ECDF reports for any given number the percent of individuals that are below that threshold. http://www.sthda.com/english/wiki/ggplot2-ecdf-plot-quick-start-guide-for-empirical-cumulative-density-function-r-software-and-data-visualization

# Basic ECDF plot
p <- ggplot(lastfrost, aes(pretendmody)) + stat_ecdf(geom = "step")+
    labs(
        title="Datum då sista frost passerat i Örebro",
     y = "% av åren 1991-2023 då sista frost passerat",
     x="Datum"
    ) + scale_x_date(breaks = scales::breaks_pretty(9),
                 date_minor_breaks = "1 days") +
    scale_y_continuous(labels = function(x) paste0(x*100, "%"))

p <- p + theme(text = element_text(size=24),
          plot.margin = unit(c(0.9, 0.9, 0.9, 0.9), "centimeters"))

p + geom_density(fill="blue", alpha = 0.3)

ggsave(
  "last_frost_orebro.png",
  plot = last_plot(),
  device = NULL,
  path = "../dropbox/images/plants/",
  scale = 1,
  width = 25,
  height = 20,
  units = "cm", ## c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)



## sista frosten på våren (minimitemperaturen under dygnet under noll)

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
