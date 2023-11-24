## Summarise results from eur bt

## Select two largest sites in France (n > 1000) -------
## summary(as.factor(eur_bt$country))
## dtaeurbt <- eur_bt[country == "France", ]

## table(factor(eur_bt$var))
## names(summary(factor(eur_bt$site)))[!names(summary(factor(eur_bt$site))) %in% names(summary(factor(eur_bt[var == "burlat", site])))
## ]
## ## Note: East Malling does not have Burlat
## ## eur_bt[site == "East Malling", ]

## data ---------------
dtaeurbt <- eur_bt

## Explore NAs:
## dtaeurbt[is.na(bt_start), ]
## dtaeurbt[is.na(bt_start) & !is.na(bt_full), ]
## Note: For about 1500 rows bt_full is noted instead of bt_start

## Exclude rows with missing bt_start:
dtaeurbt <- dtaeurbt[!is.na(bt_start), ]

## Sanitize colnames:
## names(dtaeurbt) <-  gsub("P.a.fr.stamme", "rootstock", names(dtaeurbt))

## Variable types:
dtaeurbt$year <- factor(dtaeurbt$year)
dtaeurbt$site <- factor(dtaeurbt$site)

## computed variables:
dtaeurbt[, age := as.numeric(as.character(year)) - as.numeric(plantation)]
message("some NAs created in age due to missing data")

## Exclusion criteria ---------------
dtaeurbt <- dtaeurbt[!age<3 | is.na(age), ]
## dtaeurbt <- dtaeurbt[!is.na(age), ]
## dtaeurbt <- dtaeurbt[as.numeric(as.character(year)) < 1996, ] ## remove years before 1996 to make data more homogenous

## ## Remove outliers ------------
## plot(bt_start ~ year, data = dtaeurbt)
## plot(bt_duration ~ year, data = dtaeurbt)
## dtaeurbt <- dtaeurbt[var != "cristobalina" | year != 1993 | flowering_duration != 20, ] ## outlier rm

## Select cols --------
idcols <- c("country", "site", "year", "age", "var" ,"cultivar", "rootstock")
btcols <- c("bt_start", "bt_full", "bt_end", "bt_duration", "bt_start_to_full", "beginning_of_maturity")
cols <- c(idcols, btcols)
dtaeurbt <- dtaeurbt[, ..cols]

## Lookup names -------
nam <- dtaeurbt[, .(var, cultivar)] ## names

## ## model non-relative ------
## ## summary(lm(bt_start ~ var + age, data = dtaeurbt))
## m1 <- lm(bt_start ~ var + site + year + age, data = dtaeurbt)
## ## summary(m1)
## s1 <- summary(m1)$coef
## mean_start <- s1[row.names(s1) == "(Intercept)", ]["Estimate"][[1]]
## format(as.Date(as.Date("1985-01-01") + mean_start), "%m-%d")
## quantile(dtaeurbt$bt_start_to_full, prob = seq(0,1,0.1), na.rm = TRUE)

## Median bt_start per var and yearsite -----
dtaeurbt[, yearsite := factor(paste0(as.character(year), as.character(site)))]
dtaeurbt[, mdn_ys_start := median(bt_start, na.rm = TRUE), by = "yearsite"]
dtaeurbt[, mdn_ys_beginning_of_maturity := median(beginning_of_maturity, na.rm = TRUE), by = "yearsite"]

## BTs relative to median BT start ----------
relative <- dtaeurbt
relative[, start := bt_start - mdn_ys_start]
relative[, full := bt_full - mdn_ys_start]
relative[, end := bt_end - mdn_ys_start]
relative[, duration := bt_duration]
relative[, start_to_full := bt_start_to_full]
relative[, maturity := beginning_of_maturity - mdn_ys_beginning_of_maturity]

## select cols --------
cols <- c(idcols, "yearsite", "start", "full", "end", "start_to_full", "duration", "maturity")
relative <- relative[, ..cols]

## Impute missing ? -----------------
## note: start_to_full Varied by year, site and for some var and More depending on year and site than var. Inference: Subjective assessment. Therefore was not imputed. Instead omitted n = 679 rows (ie about 5%) lacking info (almost all from eg Ullensvang which recorde mostly start) 
## m1 <- lm(start_to_full ~ var + site + year, data = relative)
## summary(m1)
## plot(start_to_full ~ var, data = relative)
## relative[is.na(full), start_to_full_imp := median(start_to_full, na.rm = TRUE), by = list(site, var)]

## collapse to one row per var, year and site -------
## sum(duplicated(relative[, .(var, site, year)])) ## n = 3645
## indx <- duplicated(relative[, .(var, site, year)])
## relative[indx, ]
## Note: looks similar
cols <- c("var", "year", "site", "start", "full", "end", "start_to_full", "duration", "maturity")
relative <- relative[, ..cols]
myfun <- function(x){mean(x, na.rm = TRUE)}
relative <- relative[, lapply(.SD, myfun), by=c("var", "year", "site")]

## aggregate, keeping year and var ------------
rel <- relative[, .(var, year, start, full, end, duration, maturity)]
## collapse sites:
myfun <- function(x){median(x, na.rm = TRUE)}
rel <- rel[, lapply(.SD, myfun), by=c("var", "year")] 

## assume fertilization possible from start to full + 4d
rel[, fert := full + 4]

## select varieties --------
## sel <- summary(factor(rel$var))[1:20] ## most commonly noted
## sel <- names(sel)
## dta$var[!dta$var %in% eur_bt$var] ## missing from eur_bt
sel <- dta$var
rel <- rel[grepl(paste0(sel, collapse = "$|^"), var), ]
## unique(rel$var)

## ## look for synonyms
## table(factor(relative$var))
## names(genotype_syn)
## unique(relative$var)
## tmp <- variety_genotype_group
## tmp[ , syn := paste0(syn, ", ", label)] ## add label to syn
## tmp[ , syn := gsub("^, ", "", syn)]
## cols <- c("variety", "var", "syn")
## lookfor <- unique(relative$var)
## lookfor <- gsub("_.*", "", lookfor)
## tmp[grepl(paste0(lookfor, collapse = "|"), tolower(syn)), ..cols]
## print(tmp[grepl(paste0(lookfor, collapse = "|"), tolower(syn)), "syn"], n = 400)
## dta$var[!dta$var %in% eur_bt$var] ## missing from eur_bt
## tmp[grepl("rote", tolower(syn)), ..cols]
## eur_bt[grepl("mer", tolower(cultivar)), unique(var)]
## unique(eur_bt$var)

## loop over years -------------

## prepare:
rel[["var"]] <- as.character(rel[["var"]])
rel[["year"]] <- as.character(rel[["year"]])
rel <- rel[, .(var, year, start, fert)]

## test:
## yr <- "2008"
## varn <- "fercer"
## i <- 2

allyears <- c()
allyears_days <- c()

for(yr in unique(rel[["year"]])){
    thisyear <- rel[year == yr, ]
    
    ## add missing var with NA:
    addthis <- rel[!grepl(paste0(thisyear[["var"]], collapse = "|"), var), ]
    addthis[, year := yr]
    addthis[, start := NaN]
    addthis[, fert := NaN]
    addthis <- unique(addthis)
    thisyear <- rbind(thisyear, addthis)
    
    ## make a df to add cols:
    newcols <- as.data.table(matrix(nrow = nrow(thisyear), ncol = nrow(thisyear)+1))
    names(newcols) <- c("var", thisyear$var)
    cols <- names(newcols)
    newcols_mod <- newcols[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols] ## changed from "as.charecter"
    newcols_mod$var <- thisyear$var
    thisyear <- newcols_mod[thisyear, on = "var"]
    thisyear_days <- newcols_mod[thisyear, on = "var"]

    ## test:
    ## overlap(thisyear[var == varn, start],
    ##         thisyear[var == varn, fert],
    ##         thisyear[i, start],
    ##         thisyear[i, fert])
    
    for(i in 1:nrow(thisyear)){ ## loop over rows

        for(varn in thisyear$var){ ## loop over cols
            ## get genotype of column name
            ## thisyear[i, (varn) := thisyear[var == varn, genotype]]  

            ## get compatibility value of column name:
            thisyear[i, (varn) := overlap(thisyear[var == varn, start],
                                          thisyear[var == varn, fert],
                                          thisyear[i, start],
                                          thisyear[i, fert],
                                          criteria = 4)]

            ## get overlap in days:
            thisyear_days[i, (varn) := overlap(thisyear_days[var == varn, start],
                                               thisyear_days[var == varn, fert],
                                               thisyear_days[i, start],
                                               thisyear_days[i, fert])]
        }

    }

    allyears <- rbind(allyears, thisyear, fill = TRUE)
    allyears_days <- rbind(allyears_days, thisyear_days, fill = TRUE)
}

## setkey(allyears, start)
cols <- c("var", "year", "start", newcols_mod$var)
allyears <- allyears[, ..cols]
allyears_days <- allyears_days[, ..cols]
## cols <- newcols_mod$var

## prepare data for plotting etc -------------

## Calculate proportion of years with overlap (mean of 0/1 = prop):
myfun <- function(x){signif(mean(as.numeric(x), na.rm = TRUE), digits = 2)}
toplot <- allyears[, lapply(.SD, myfun), by=c("var")] 
toplot$year <- NULL
toplot$fert <- NULL
## melt:
toplot <- melt(toplot, id.vars = c("var", "start"))
toplot[, value := round(value*100, digits = 0)]

## add days overlap for labels:
myfun <- function(x){round(median(as.numeric(x), na.rm = TRUE), digits = 0)}
toplot_labels <- allyears_days[, lapply(.SD, myfun), by=c("var")] ## mean = prop
toplot_labels$year <- NULL
toplot_labels$start <- NULL
toplot_labels <- melt(toplot_labels, id.vars = c("var"))
names(toplot_labels) <- c("var", "variable", "label")
toplot <- toplot_labels[toplot, on = c("var", "variable")]

## add genotype match to label:
lookupgenotype <- variety_genotype_group[, .(var, genotype)]
toplot <- lookupgenotype[toplot, on = "var"]
names(toplot) <- gsub("^genotype$", "genotype_cultivar", names(toplot))
toplot <- merge(x=toplot, y=lookupgenotype, by.x="variable", by.y="var")
names(toplot) <- gsub("^genotype$", "genotype_pollinator", names(toplot))
myfun <- Vectorize(compat)
toplot[, compat := myfun(genotype_pollinator, genotype_cultivar)]
toplot[, label := as.character(label)]
toplot[, label := ifelse(compat == 0, "X", label)]
toplot[, label := ifelse(compat == 1 & var == variable, "SC", label)]
## toplot[, label := ifelse(compat == 2, paste0("~", label, "^{a}"), label)]
toplot[, label := ifelse(compat == 2, paste0(label, "+"), label)]

## lookup bt start for ordering pollinators:
myfun <- function(x){round(mean(as.numeric(x), na.rm = TRUE), digits = 2)}
lookupstart <- toplot[, .(var, start)]
lookupstart <- lookupstart[, lapply(.SD, myfun), by=c("var")] 
names(lookupstart) <- c("variable", "start_pollinator")
## unique(toplot$var)
toplot <- lookupstart[toplot, on = "variable"]

## ## names using labels from eur_bt
## varnames_infrance <- nam$cultivar
## names(varnames_infrance) <- nam$var
## toplot$cultivar <- query_label(toplot$var, varnames_infrance)

## names using varnames
toplot$cultivar <- query_label(toplot$var, varnames3)
toplot$cultivar <- factor(toplot$cultivar)
toplot$pollinator <- query_label(toplot$variable, varnames3)
toplot$pollinator <- factor(toplot$pollinator)

## plot  --------------------

p <- ggplot(toplot, aes(x = forcats::fct_reorder(pollinator, start_pollinator), y = forcats::fct_reorder(cultivar, start), fill = value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = toplot$label), color = "white", size = 4,
            parse = FALSE) +
  coord_fixed()

p <- p + theme(
        plot.margin = unit(c(0.9, 0.9, 0.9, 0.9), "centimeters"),
        ## legend.position = "none",
        axis.ticks = element_blank(),
        axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0),
        plot.title = element_text(hjust = 0, vjust = 3, size = 12, face="bold"),
        ## axis.title.x = element_text(hjust = 0.5, vjust = -5),
        axis.text=element_text(size=14),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
        ## axis.title=element_text(size=12, face="bold")
    )

bt_eur_heatmap <- p +
    scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
    guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 30,
                                ticks.linewidth = 2,
                                title.hjust = 1,
                                label.theme = element_text(size = 18),
                                title.theme = element_text(size = 18),
                                title = "% \n")
           )
bt_eur_heatmap

ggsave(
  "bt_eur_heatmap.png",
  plot = last_plot(),
  device = NULL,
  path = "../dropbox/images/plants/",
  scale = 1,
  width = 16.6,
  height = 10.86,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)



## 
## + theme(legend.position = "none")

## ## colors:
## library(RColorBrewer)
## # To see all palettes available in this package
## par(mfrow=c(1, 1))
## display.brewer.all()


## Note: Color is % years with bt overlap minimum 4 days. Number in boxes is median days overlap. This means the number in the box where the cultivar is both pollinator and target corresponds to the median BT for that variety (but is only shown for SC varieties, else X). "X" is incompatible. 
## Note: Time of receptivity/pollination was assumed to be bt_start to (bt_full + 4 days)
## Note: Those with + sign are fully compatible
## Note: margit and sylvia had relatively large variations in both sdstart and sdfert

## ## explore:
rel[var == "margit", ]
rel[var == "van", ]

## sdstart <- rel[, sd(start), by = "var"]
## names(sdstart) <- c("var", "sdstart")
## sdfert <- rel[, sd(fert, na.rm = TRUE), by = "var"]
## names(sdfert) <- c("var", "sdfert")
## sdtable <- sdstart[sdfert, on = "var"]
## sdtable[, large_sdstart := sdstart > quantile(sdtable$sdstart, 0.75)]
## sdtable[, large_sdfert := sdfert > quantile(sdtable$sdfert, 0.75)]

