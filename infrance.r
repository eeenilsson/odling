## Select two largest sites in France (n > 1000) -------
## summary(as.factor(eur_bt$country))
## tmp <- eur_bt[country == "France", ]
tmp <- eur_bt[site == "Toulenne"|site == "Balandran", ]
tmp[, age := as.numeric(as.character(year)) - as.numeric(plantation)]
tmp <- tmp[!is.na(age), ]
tmp <- tmp[!age<3, ]
## summary(as.factor(tmp$site))
nam <- tmp[, .(var, cultivar)] ## names

## model non-relative
m1 <- lm(bt_start ~ var + site + year + age, data = tmp)
s1 <- summary(m1)$coef
mean_start <- s1[row.names(s1) == "(Intercept)", ]["Estimate"][[1]]
format(as.Date(as.Date("1985-01-01") + mean_start), "%m-%d")
summary(m1)
summary(tmp$age)
summary(s1[grepl("^age", row.names(s1)), "Estimate"])
summary(s1[grepl("^site", row.names(s1)), "Estimate"])
summary(s1[grepl("^var", row.names(s1)), "Estimate"])
quantile(s1[grepl("^var", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))
summary(s1[grepl("^year", row.names(s1)), "Estimate"])
quantile(s1[grepl("^year", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))

## descriptive
summary(tmp$bt_start_to_full)
quantile(tmp$bt_start_to_full, prob = seq(0,1,0.1), na.rm = TRUE)
summary(tmp$bt_duration)
quantile(tmp$bt_duration, prob = seq(0,1,0.1), na.rm = TRUE)

## model start to full
m1 <- lm(bt_start_to_full ~ var + site + year + age, data = tmp)
s1 <- summary(m1)$coef
summary(s1[grepl("^age", row.names(s1)), "Estimate"])
summary(s1[grepl("^site", row.names(s1)), "Estimate"])
summary(s1[grepl("^var", row.names(s1)), "Estimate"])
quantile(s1[grepl("^var", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))
summary(s1[grepl("^year", row.names(s1)), "Estimate"])
quantile(s1[grepl("^year", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))

## model duration
m1 <- lm(bt_duration ~ var + site + year + age, data = tmp)
s1 <- summary(m1)$coef
summary(s1[grepl("^age", row.names(s1)), "Estimate"])
summary(s1[grepl("^site", row.names(s1)), "Estimate"])
summary(s1[grepl("^var", row.names(s1)), "Estimate"])
quantile(s1[grepl("^var", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))
summary(s1[grepl("^year", row.names(s1)), "Estimate"])
quantile(s1[grepl("^year", row.names(s1)), "Estimate"], prob = seq(0,1,0.1))

## Note: Balandran 1 day earlier start (p signif)
tmp <- tmp[, .(var, bt_start, bt_full, bt_end, bt_duration, bt_start_to_full, beginning_of_maturity, year, age, site)]
tmp[, site := as.numeric(site == "Balandran")]
myfun <- function(x){mean(x, na.rm = TRUE)}
## collapse to one row per var:
tmp <- tmp[, lapply(.SD, myfun), by=c("var", "year", "site")] 
sum(tmp[var == "burlat", year] %in% unique(tmp$year) == FALSE) ## Note: burlat in all years
tmp[, yearsite := paste0(as.character(year), as.character(site))]
look <- tmp[var == "burlat", .(yearsite, bt_start, bt_full, bt_end, bt_duration, bt_start_to_full, beginning_of_maturity)]
names(look)[-1] <- paste0("burlat_", names(look)[-1])
tmp <- look[tmp, on = "yearsite"] ## add burlat as reference

## calculate times relative to burlat:
relative <- tmp
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

## predict -------
str(relative)

training <- relative[, .(start, var, site, year, age)]
training <- na.omit(training)
m1 <- lm(start ~ var + site + year + age, data = training)
test <- training
test[, age := median(test$age, na.rm = TRUE)]
test[, site := 1] ## Balandran
test <- na.omit(test[, .(var, site, year, age)])
m1$xlevels[["year"]] <- union(m1$xlevels[["year"]], levels(test$year))
m1$xlevels[["var"]] <- union(m1$xlevels[["var"]], levels(test$var))
test$year <- factor(test$year)
test$var <- factor(test$var)
## Note: Works with na.omit in test
test$start_relative <- predict(m1, test, type="response")



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




## collapse sites
myfun <- function(x){mean(x, na.rm = TRUE)}
## collapse to one row per var:
relative <- relative[, lapply(.SD, myfun), by=c("var", "year")] 

## names
varnames_infrance <- nam$cultivar
names(varnames_infrance) <- nam$var
relative$cultivar <- query_label(relative$var, varnames_infrance)
relative$cultivar <- factor(relative$cultivar)
summary(relative$cultivar) > 5
