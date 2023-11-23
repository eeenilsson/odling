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
summary(m1)
s1 <- summary(m1)$coef
mean_start <- s1[row.names(s1) == "(Intercept)", ]["Estimate"][[1]]
format(as.Date(as.Date("1985-01-01") + mean_start), "%m-%d")
summary(m1)
summary(dtafra$age)
summary(s1[grepl("^age", row.names(s1)), "Estimate"])
summary(s1[grepl("^site", row.names(s1)), "Estimate"])
summary(s1[grepl("^var", row.names(s1)), "Estimate"])
quantile(s1[grepl("^var", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))
summary(s1[grepl("^year", row.names(s1)), "Estimate"])
quantile(s1[grepl("^year", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))

## descriptive
summary(dtafra$bt_start_to_full)
quantile(dtafra$bt_start_to_full, prob = seq(0,1,0.1), na.rm = TRUE)
summary(dtafra$bt_duration)
quantile(dtafra$bt_duration, prob = seq(0,1,0.1), na.rm = TRUE)

## model start to full
m1 <- lm(bt_start_to_full ~ var + site + year + age, data = dtafra)
s1 <- summary(m1)$coef
summary(s1[grepl("^age", row.names(s1)), "Estimate"])
summary(s1[grepl("^site", row.names(s1)), "Estimate"])
summary(s1[grepl("^var", row.names(s1)), "Estimate"])
quantile(s1[grepl("^var", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))
summary(s1[grepl("^year", row.names(s1)), "Estimate"])
quantile(s1[grepl("^year", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))

## model duration
m1 <- lm(bt_duration ~ var + site + year + age, data = dtafra)
s1 <- summary(m1)$coef
summary(s1[grepl("^age", row.names(s1)), "Estimate"])
summary(s1[grepl("^site", row.names(s1)), "Estimate"])
summary(s1[grepl("^var", row.names(s1)), "Estimate"])
quantile(s1[grepl("^var", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))
summary(s1[grepl("^year", row.names(s1)), "Estimate"])
quantile(s1[grepl("^year", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))

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
out

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

plot(bt_start ~ year, data = dtafra)

## add type of year
yeartype <- dtafra[, median(bt_start, na.rm = TRUE), by = "year"]
names(yeartype) <- c("year", "mdn")
yeartype[, yrtype := cut(mdn, breaks = quantile(mdn, probs = seq(0, 1, 1/3), na.rm = TRUE), include.lowest = TRUE)]

yeartype$yrtype <-  factor(as.numeric(yeartype$yrtype), levels = 1:3, labels = c("early", "medium", "late"))
yeartype <- yeartype[, .(year, yrtype)]
relative <- yeartype[relative, on = "year"]
summary(lm(full ~ yrtype + var + site + age, data = relative)) ## late years -1 day
summary(lm(duration ~ yrtype + var + site + age, data = relative)) ## late years -1.8 days
summary(lm(maturity ~ yrtype + var + site + age, data = relative)) ## late years +4d

## aggregate, keeping year and var
rel <- relative[, .(var, year, start, full, end, duration, maturity)]
## collapse sites
myfun <- function(x){mean(x, na.rm = TRUE)}
rel <- rel[, lapply(.SD, myfun), by=c("var", "year")] 

## assume fertilization possible from start to full + 4d
rel[, fert := full + 4]

## select 99 most commonly registered varieties
sel <- summary(factor(rel$var))[1:20] 
sel <- names(sel)
rel <- rel[grepl(paste0(sel, collapse = "|"), var), ]

## overlap function

## prepare
rel[["var"]] <- as.character(rel[["var"]])
rel[["year"]] <- as.character(rel[["year"]])
rel <- rel[, .(var, year, start, fert)]

## loop over years TODO ---

    ## overlap fun
    overlap <- function(astart, aend, bstart, bend, criteria = NULL){
        ## criteria 4 days overlap
        
        ## astart <- thisyear[var == varn, start] ## col
        ## aend <- thisyear[var == varn, fert]
        ## bstart <- thisyear[i, start] ## row
        ## bend <- thisyear[i, fert]

        ## get order
        ## firstone <- ifelse(astart <= bstart, "a", "b")
        firststart <- ifelse(astart <= bstart, astart, bstart) ## b
        secondstart <- ifelse(astart > bstart, astart, bstart)
        firstend <- ifelse(astart <= bstart, aend, bend)
        secondend <- ifelse(astart > bstart, aend, bend)

        ## set to index 0
        secondstart <- secondstart - firststart
        firstend <- firstend - firststart
        secondend <- secondend - firststart
        firststart <- 0
        ## calculate overlap
        overlapmax <- firstend - secondstart
        overshoot <- ifelse(firstend > secondend, firstend - secondend, 0)
        overlap <- overlapmax - overshoot

        if(!is.null(criteria)){
        out <- overlap >= criteria
        return(as.numeric(out))
        }else{
            return(round(overlap, digits = 0))
}

    }

## yr <- "2008"
    ## varn <- "fercer"
    ## i <- 2

allyears <- c()

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
newcols_mod <- newcols[ , (cols) := lapply(.SD, as.character), .SDcols = cols]
newcols_mod$var <- thisyear$var
thisyear <- newcols_mod[thisyear, on = "var"]



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
            criteria = 4)
                         ]
}

}

    allyears <- rbind(allyears, thisyear, fill = TRUE)
    }

setkey(allyears, start)
cols <- c("var", "year", newcols_mod$var)
## dt[ , (cols) := lapply(.SD, "*", -1), .SDcols = cols]
allyears <- allyears[, ..cols]
cols <- newcols_mod$var
allyears[ , (cols) := lapply(.SD, "sum"), .SDcols = cols]

allyears[, ..cols]


myfun <- function(x){
    x <- na.omit(x)
    out <- x == 1/length(x)
    out <- signif(out*100, digits = 2)
    return(out)
}
myfun <- function(x){signif(mean(as.numeric(x), na.rm = TRUE), digits = 2)}
## collapse to one row per var:
tmp <- allyears[, lapply(.SD, myfun), by=c("var")] 
## todo: Exclude varieties tested less tha 3 yrs
tmp$year <- NULL

## melt
tmp <- melt(tmp, id.vars = c("var"))
tmp[, value := round(value*100, digits = 0)]

p <- ggplot(tmp, aes(x = variable, y = var, fill = value)) +
  geom_tile(color = "black") +
  geom_text(aes(label = value), color = "white", size = 4) +
  coord_fixed()

p <- p +     theme(
        plot.margin = unit(c(0.9, 0.9, 0.9, 0.9), "centimeters"),
        ## legend.position = "none",
        axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0),
        plot.title = element_text(hjust = 0, vjust = 3, size = 12, face="bold"),
        axis.title.x = element_text(hjust = 0.5, vjust = -5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=12, face="bold")
    )

p + scale_fill_gradientn(colors = hcl.colors(20, "RdYlGn"))


### Here

allyears[, 3][["summit"]]

allyears[ , sum, .SDcols = cols]


###########################
plot(start ~ year, data = relative)


summary(lm(start ~ year, data = relative))

## summary(lm(age ~ var, data = dtafra))


plot(bt_start ~year, data = dtafra)

agebyvar <- dtafra[, mean(age), by = "var"]
names(agebyvar) <- c("var", "age")
q1 <- quantile(agebyvar$age, 0.05)[[1]]
q9 <- quantile(agebyvar$age, 0.95)[[1]]

agebyvar[age < q1 | age > q9, var]
agebyvar[var == "margit", ]

dtafra[, mean(age), by = "var"]

## predict -------
str(relative)

## tmp <- names(summary(relative$var))[summary(relative$var) > 30]
## tmp <- tmp[-length(tmp)]
## train <- na.omit(relative[grepl(paste0(tmp, collapse = "|"), var), .(start, var, site, year, age)])

train <- na.omit(relative[, .(start, var, site, year, age)])
str(train)
m1 <- lm(start ~ var + site, year + age, data = train)
summary(m1)
test <- train  ## [, .(var, year, age)]
test <- na.omit(test)
test$age = median(test$age, na.rm = TRUE)
## test$site = 1 ## Balandran
## test <- na.omit(test[, .(var, site, year, age)])
m1$xlevels[["year"]] <- union(m1$xlevels[["year"]], levels(test$year))
m1$xlevels[["var"]] <- union(m1$xlevels[["var"]], levels(test$var))
## test$year <- factor(test$year)
## test$var <- factor(test$var)
## Note: Works with na.omit in test

## names(test) <- names(train)[-1]
test$start_relative <- predict(m1, test, type="response")
relative


## plot
## plot base
p <- ggplot(test, aes(x = fct_reorder(var, -start), y = full)) + coord_flip()
p <- p + geom_point(size = 2) + geom_errorbar(aes(ymin = start, ymax = end), width = 0.2)
p <- p + scale_y_continuous(breaks = 0:27)
p <- p + theme(
        ## axis.ticks.y=element_blank(),
        panel.grid.minor.x = element_blank(),
        ## axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text=element_text(size=24),
        plot.title = element_text(size = 16, face="bold")
          )
## p <- p + labs(title="Blomningstid: Medelvärden för start, full blomning (markerad med en prick) och blomningens längd, justerat för ort och år.",
##              y = "Dagar (från tidigaste sortens blomningsstart)")

p + geom_point(aes(y = fullplus4), shape = 3, size = 3) ## add slash at 4 days after full bloom (assumed end of fertility)





## names
varnames_infrance <- nam$cultivar
names(varnames_infrance) <- nam$var
relative$cultivar <- query_label(relative$var, varnames_infrance)
relative$cultivar <- factor(relative$cultivar)
summary(relative$cultivar) > 5
