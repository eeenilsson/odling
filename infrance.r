## Select two largest sites in France (n > 1000) -------
## summary(as.factor(eur_bt$country))
## dtafra <- eur_bt[country == "France", ]
dtafra <- eur_bt[site == "Toulenne"|site == "Balandran", ]
dtafra[, age := as.numeric(as.character(year)) - as.numeric(plantation)]
dtafra <- dtafra[!is.na(age), ]

dtafra <- dtafra[!age<3, ]
dtafra <- dtafra[var != "cristobalina" | year != 1993 | flowering_duration != 20, ] ## outlier rm
## dtafra <- dtafra[as.numeric(as.character(year)) < 1996, ] ## remove years before 1996 to make data more homogenous
dtafra$year <- factor(dtafra$year)
dtafra$site <- factor(dtafra$site)

## summary(as.factor(dtafra$site))
nam <- dtafra[, .(var, cultivar)] ## names

## model non-relative
## summary(lm(bt_start ~ var + age, data = dtafra))
m1 <- lm(bt_start ~ var + site + year + age, data = dtafra)
## summary(m1)
s1 <- summary(m1)$coef
mean_start <- s1[row.names(s1) == "(Intercept)", ]["Estimate"][[1]]
format(as.Date(as.Date("1985-01-01") + mean_start), "%m-%d")

## look at summaries:
## summary(m1)
## summary(dtafra$age)
## summary(s1[grepl("^age", row.names(s1)), "Estimate"])
## summary(s1[grepl("^site", row.names(s1)), "Estimate"])
## summary(s1[grepl("^var", row.names(s1)), "Estimate"])
## quantile(s1[grepl("^var", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))
## summary(s1[grepl("^year", row.names(s1)), "Estimate"])
## quantile(s1[grepl("^year", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))

## ## descriptive
## summary(dtafra$bt_start_to_full)
## quantile(dtafra$bt_start_to_full, prob = seq(0,1,0.1), na.rm = TRUE)
## summary(dtafra$bt_duration)
## quantile(dtafra$bt_duration, prob = seq(0,1,0.1), na.rm = TRUE)

## ## model start to full
## m1 <- lm(bt_start_to_full ~ var + site + year + age, data = dtafra)
## s1 <- summary(m1)$coef
## summary(s1[grepl("^age", row.names(s1)), "Estimate"])
## summary(s1[grepl("^site", row.names(s1)), "Estimate"])
## summary(s1[grepl("^var", row.names(s1)), "Estimate"])
## quantile(s1[grepl("^var", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))
## summary(s1[grepl("^year", row.names(s1)), "Estimate"])
## quantile(s1[grepl("^year", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))

## ## model duration
## m1 <- lm(bt_duration ~ var + site + year + age, data = dtafra)
## s1 <- summary(m1)$coef
## summary(s1[grepl("^age", row.names(s1)), "Estimate"])
## summary(s1[grepl("^site", row.names(s1)), "Estimate"])
## summary(s1[grepl("^var", row.names(s1)), "Estimate"])
## quantile(s1[grepl("^var", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))
## summary(s1[grepl("^year", row.names(s1)), "Estimate"])
## quantile(s1[grepl("^year", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))

## Note: Balandran 1 day earlier start (p signif)
dtafra <- dtafra[, .(var, bt_start, bt_full, bt_end, bt_duration, bt_start_to_full, beginning_of_maturity, year, age, site)]
dtafra[, site := as.numeric(site == "Balandran")]
myfun <- function(x){mean(x, na.rm = TRUE)}
## collapse to one row per var:
dtafra <- dtafra[, lapply(.SD, myfun), by=c("var", "year", "site")] 
sum(dtafra[var == "burlat", year] %in% unique(dtafra$year) == FALSE) ## Note: burlat in all years
dtafra[, yearsite := paste0(as.character(year), as.character(site))]
look <- dtafra[var == "burlat", .(yearsite, bt_start, bt_full, bt_end, bt_duration, bt_start_to_full, beginning_of_maturity)]
names(look)[-1] <- paste0("burlat_", names(look)[-1])
dtafra <- look[dtafra, on = "yearsite"] ## add burlat as reference

## calculate times relative to burlat:
relative <- dtafra
relative[, start := bt_start - burlat_bt_start]
relative[, full := bt_full - burlat_bt_start]
relative[, end := bt_end - burlat_bt_start]
relative[, duration := bt_duration]
relative[, start_to_full := bt_start_to_full]
relative[, duration_relative := bt_duration / burlat_bt_duration]
relative[, start_to_full_relative := bt_start_to_full / burlat_bt_start_to_full]
relative[, maturity := beginning_of_maturity - burlat_beginning_of_maturity]
relative <- relative[, .(var, year, site, age, start, full, end, duration, start_to_full, maturity, duration_relative, start_to_full_relative)]

p <- seq(0,1,0.1)
m1 <- lm(start ~ var + site + year + age, data = relative)
s1 <- summary(m1)$coef
s1[grepl("^age", row.names(s1)), "Estimate"]
s1[grepl("^site", row.names(s1)), "Estimate"]
quantiles_start <- data.table(
    decile = p,
    year = quantile(s1[grepl("^year", row.names(s1)), "Estimate"], prob = p),
    variety = quantile(s1[grepl("^var", row.names(s1)), "Estimate"], prob = p)
)
m1 <- lm(start_to_full_relative ~ var + site + year + age, data = relative)
s1 <- summary(m1)$coef
s1[grepl("^age", row.names(s1)), "Estimate"]
s1[grepl("^site", row.names(s1)), "Estimate"]
quantiles_start_to_full <- data.table(
    decile = p,
    year = quantile(s1[grepl("^year", row.names(s1)), "Estimate"], prob = p),
    variety = quantile(s1[grepl("^var", row.names(s1)), "Estimate"], prob = p)
)
m1 <- lm(duration_relative ~ var + site + year + age, data = relative)
s1 <- summary(m1)$coef
s1[grepl("^age", row.names(s1)), "Estimate"]
s1[grepl("^site", row.names(s1)), "Estimate"]
quantiles_duration <- data.table(
    decile = p,
    year = quantile(s1[grepl("^year", row.names(s1)), "Estimate"], prob = p),
    variety = quantile(s1[grepl("^var", row.names(s1)), "Estimate"], prob = p)
)
m1 <- lm(maturity ~ var + site + year + age, data = relative)
s1 <- summary(m1)$coef
s1[grepl("^age", row.names(s1)), "Estimate"]
s1[grepl("^site", row.names(s1)), "Estimate"]
quantiles_maturity <- data.table(
    decile = p,
    year = quantile(s1[grepl("^year", row.names(s1)), "Estimate"], prob = p),
    variety = quantile(s1[grepl("^var", row.names(s1)), "Estimate"], prob = p)
)
names(quantiles_start) <- c("decile", "start_year", "start_variety")
names(quantiles_start_to_full) <- c("decile2", "full_relative_year", "full_relative_variety")
names(quantiles_duration) <- c("decile3", "duration_relative_year", "duration_relative_variety")
names(quantiles_maturity) <- c("decile4", "maturity_year", "maturity_variety")

quant <- cbind(
    quantiles_start,
    quantiles_start_to_full,
    quantiles_duration,
    quantiles_maturity
)
quant$decile2 <- NULL
quant$decile3 <- NULL
quant$decile4 <- NULL

i <- names(quant)[-1][1]
out <- c()
for(i in names(quant)[-1]){
    tmp <- quant[[i]]
    tmp <- as.character(signif(tmp, digits = 2))
    txt <- paste0(
        i, " ", tmp[1], " till ", tmp[11], " (", tmp[2], " till ", tmp[10], ")")
    out <- paste0(out, "; ", txt)    
}
## out

## Note: relative start affected by year

## ## collapse sites
## myfun <- function(x){mean(x, na.rm = TRUE)}
## ## collapse to one row per var:
## relative <- relative[, lapply(.SD, myfun), by=c("var", "year")] 

## ## explore age
## summary(lm(start ~age + var, data = relative))
## summary(lm(bt_start ~age + var + site + year, data = dtafra))
## relative[, age_tertile := cut(age, breaks = quantile(age, probs = seq(0, 1, 1/3), na.rm = TRUE), include.lowest = TRUE)  ]
## plot(start ~ age_tertile, data = relative)
## summary(lm(start ~ age_tertile, data = relative))
## plot(bt_start ~ year, data = dtafra)

## add type of year
yeartype <- dtafra[, median(bt_start, na.rm = TRUE), by = "year"]
names(yeartype) <- c("year", "mdn")
yeartype[, yrtype := cut(mdn, breaks = quantile(mdn, probs = seq(0, 1, 1/3), na.rm = TRUE), include.lowest = TRUE)]

yeartype$yrtype <-  factor(as.numeric(yeartype$yrtype), levels = 1:3, labels = c("early", "medium", "late"))
yeartype <- yeartype[, .(year, yrtype)]
relative <- yeartype[relative, on = "year"]
## summary(lm(full ~ yrtype + var + site + age, data = relative)) ## late years -1 day
## summary(lm(duration ~ yrtype + var + site + age, data = relative)) ## late years -1.8 days
## summary(lm(maturity ~ yrtype + var + site + age, data = relative)) ## late years +4d

## aggregate, keeping year and var
rel <- relative[, .(var, year, start, full, end, duration, maturity)]
## collapse sites
myfun <- function(x){mean(x, na.rm = TRUE)}
rel <- rel[, lapply(.SD, myfun), by=c("var", "year")] 

## assume fertilization possible from start to full + 4d
rel[, fert := full + 4]

## select varieties --------
## sel <- summary(factor(rel$var))[1:20] ## most commonly noted
## sel <- names(sel)
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
## eur_bt[grepl("mert", tolower(cultivar)), unique(var)]
## unique(eur_bt$var)

## overlap function

## prepare
rel[["var"]] <- as.character(rel[["var"]])
rel[["year"]] <- as.character(rel[["year"]])
rel <- rel[, .(var, year, start, fert)]

## loop over years ---

    ## overlap fun

## yr <- "2008"
    ## varn <- "fercer"
    ## i <- 2

allyears <- c()
allyears_days <- c()

for(yr in unique(rel[["year"]])){
    thisyear <- rel[year == yr, ]
    
    ## add missing var with NA
    addthis <- rel[!grepl(paste0(thisyear[["var"]], collapse = "|"), var), ]
    addthis[, year := yr]
    addthis[, start := NaN]
    addthis[, fert := NaN]
    addthis <- unique(addthis)
    thisyear <- rbind(thisyear, addthis)
    
    ## make a df to add cols
    newcols <- as.data.table(matrix(nrow = nrow(thisyear), ncol = nrow(thisyear)+1))
    names(newcols) <- c("var", thisyear$var)
    cols <- names(newcols)
    newcols_mod <- newcols[ , (cols) := lapply(.SD, as.numeric), .SDcols = cols] ## changed from "as.charecter"
    newcols_mod$var <- thisyear$var
    thisyear <- newcols_mod[thisyear, on = "var"]
    
    ## newcols_mod_days <- newcols_mod
    ## names(newcols_mod_days)[-1] <- paste0(names(newcols_mod_days)[-1], "_days")
    thisyear_days <- newcols_mod[thisyear, on = "var"]

    ## test
    ## overlap(thisyear[var == varn, start],
    ##         thisyear[var == varn, fert],
    ##         thisyear[i, start],
    ##         thisyear[i, fert])
    
    for(i in 1:nrow(thisyear)){ ## loop over rows

        for(varn in thisyear$var){ ## loop over cols
            ## get genotype of column name
            ## thisyear[i, (varn) := thisyear[var == varn, genotype]]  

            ## get compatibility value of column name
            thisyear[i, (varn) := overlap(thisyear[var == varn, start],
                                          thisyear[var == varn, fert],
                                          thisyear[i, start],
                                          thisyear[i, fert],
                                          criteria = 4)]

            ## get overlap in days
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
cols <- newcols_mod$var
## allyears[ , (cols) := lapply(.SD, "sum"), .SDcols = cols] ## redundant?

## str(allyears)
## allyears[, ..cols]

## collapse to one row per var:
## myfun <- function(x){
##     x <- na.omit(x)
##     out <- x == 1/length(x)
##     out <- signif(out*100, digits = 2)
##     return(out)
## }

## prepare data for plotting etc -------------
myfun <- function(x){signif(mean(as.numeric(x), na.rm = TRUE), digits = 2)}
toplot <- allyears[, lapply(.SD, myfun), by=c("var")] ## mean = prop
## todo: Exclude varieties tested less tha 3 yrs
toplot$year <- NULL
toplot$fert <- NULL
## melt
toplot <- melt(toplot, id.vars = c("var", "start"))
toplot[, value := round(value*100, digits = 0)]

## add genotype
lookupgenotype <- variety_genotype_group[, .(var, genotype)]
toplot <- lookupgenotype[toplot, on = "var"]
names(toplot) <- gsub("^genotype$", "genotype_cultivar", names(toplot))
toplot <- merge(x=toplot, y=lookupgenotype, by.x="variable", by.y="var")
names(toplot) <- gsub("^genotype$", "genotype_pollinator", names(toplot))
myfun <- Vectorize(compat)
toplot[, compat := myfun(genotype_pollinator, genotype_cultivar)]

## days overlap for labels
myfun <- function(x){round(median(as.numeric(x), na.rm = TRUE), digits = 0)}
tmp <- allyears_days[, lapply(.SD, myfun), by=c("var")] ## mean = prop
tmp$year <- NULL
tmp <- melt(tmp, id.vars = c("var", "start"))

## lookup bt start for ordering pollinators
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

## plot eur bt infra --------------------
## unique(toplot$pollinator)

p <- ggplot(toplot, aes(x = forcats::fct_reorder(pollinator, start_pollinator), y = forcats::fct_reorder(cultivar, start), fill = value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = tmp$value), color = "white", size = 4) +
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

plot_infrance <- p + scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn")) +
    guides(fill = guide_colourbar(barwidth = 0.5,
                                barheight = 20)) +
    guides(fill = guide_colourbar(title = "% år med\növerlapp\n"))
## + theme(legend.position = "none")

## explore ----

## rel[var == "margit" & (year == "1997"|year == "1998")|var == "stella" & (year == "1997"|year == "1998"), ]
## ## Note: Margit had relatively few datapoints and two years with large deviating relative start. Stella was not recorded during those years, therefore the % years overlap was higher for stella vs eg hedelfinger.


### Here


## ###########################
## plot(start ~ year, data = relative)


## summary(lm(start ~ year, data = relative))

## ## summary(lm(age ~ var, data = dtafra))


## plot(bt_start ~year, data = dtafra)

## agebyvar <- dtafra[, mean(age), by = "var"]
## names(agebyvar) <- c("var", "age")
## q1 <- quantile(agebyvar$age, 0.05)[[1]]
## q9 <- quantile(agebyvar$age, 0.95)[[1]]

## agebyvar[age < q1 | age > q9, var]
## agebyvar[var == "margit", ]

## dtafra[, mean(age), by = "var"]

## ## predict -------
## str(relative)

## ## tmp <- names(summary(relative$var))[summary(relative$var) > 30]
## ## tmp <- tmp[-length(tmp)]
## ## train <- na.omit(relative[grepl(paste0(tmp, collapse = "|"), var), .(start, var, site, year, age)])

## train <- na.omit(relative[, .(start, var, site, year, age)])
## str(train)
## m1 <- lm(start ~ var + site, year + age, data = train)
## summary(m1)
## test <- train  ## [, .(var, year, age)]
## test <- na.omit(test)
## test$age = median(test$age, na.rm = TRUE)
## ## test$site = 1 ## Balandran
## ## test <- na.omit(test[, .(var, site, year, age)])
## m1$xlevels[["year"]] <- union(m1$xlevels[["year"]], levels(test$year))
## m1$xlevels[["var"]] <- union(m1$xlevels[["var"]], levels(test$var))
## ## test$year <- factor(test$year)
## ## test$var <- factor(test$var)
## ## Note: Works with na.omit in test

## ## names(test) <- names(train)[-1]
## test$start_relative <- predict(m1, test, type="response")
## relative


## ## plot
## ## plot base
## p <- ggplot(test, aes(x = fct_reorder(var, -start), y = full)) + coord_flip()
## p <- p + geom_point(size = 2) + geom_errorbar(aes(ymin = start, ymax = end), width = 0.2)
## p <- p + scale_y_continuous(breaks = 0:27)
## p <- p + theme(
##         ## axis.ticks.y=element_blank(),
##         panel.grid.minor.x = element_blank(),
##         ## axis.title.x = element_blank(),
##         axis.title.y = element_blank(),
##         axis.title.x = element_blank(),
##         axis.text=element_text(size=24),
##         plot.title = element_text(size = 16, face="bold")
##           )
## ## p <- p + labs(title="Blomningstid: Medelvärden för start, full blomning (markerad med en prick) och blomningens längd, justerat för ort och år.",
## ##              y = "Dagar (från tidigaste sortens blomningsstart)")

## p + geom_point(aes(y = fullplus4), shape = 3, size = 3) ## add slash at 4 days after full bloom (assumed end of fertility)





