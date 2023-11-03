
## from cherries_main

## ## change to matching var
## variety_genotype_group[, tempvar := gsub("guigne_dannonay", "annonay", tempvar)]
## variety_genotype_group[, tempvar := gsub("allm_gulrod", "allman_gulrod", tempvar)]
## variety_genotype_group[, tempvar := gsub("altenburger_melonen_kirsche", "buttners_rote", tempvar)]
## variety_genotype_group[, tempvar := gsub("buttners_spate_rote_knorpelkirsche", "buttners_rote", tempvar)]
## variety_genotype_group[, tempvar := gsub("fryksaas", "fryksas", tempvar)]
## variety_genotype_group[, tempvar := gsub("gaardebo", "gardebo", tempvar)]
## variety_genotype_group[, tempvar := gsub("grosse_schwarze_knorpel", "stor_svart", tempvar)]
## variety_genotype_group[, tempvar := gsub("donissens_gelbe_knorpel", "donissen", tempvar)]
## variety_genotype_group[, tempvar := gsub("frogmore_early", "frogmore", tempvar)]

## From cherry_compatibility.r

## using just anfic data for testing (see phenology)
anfic_selected <- anfic_bt[!duplicated(var), ]
anfic_selected <- anfic_selected[!grepl("[0-9]", var), ] ## skip those with numbers in name
anfic_selected <- anfic_selected[anfic_selected$var %in% variety_genotype_group$var, ] ## skip those not matching var name in genotype data

## anfic_bt$var[!unique(anfic_bt$var) %in% unique(variety_genotype_group$var)]

anfic_selected <- anfic_selected[anfic_selected$var %in% variety_genotype_group$var, ] ## skip those not matching var name in 

dta_toplot <- variety_genotype_group[anfic_selected, on = "var"] ## all selected have genotype data matching var name
## str(dta_toplot)

## function to test genotypes againt each other
## genotype_a <- "S1S2"
## genotype_b <- "S1S3"
## genotype_a <- "S3S4'"
## genotype_b <- "S3S4'"

compat <- function(genotype_a, genotype_b){
    ## pollinator
    a1 <-  strsplit(genotype_a, "S")[[1]][2]
    a2 <-  strsplit(genotype_a, "S")[[1]][3]
    ## target
    b1 <-  strsplit(genotype_b, "S")[[1]][2]
    b2 <-  strsplit(genotype_b, "S")[[1]][3]
    ## pollen sucess = TRUE
    p1 <- (a1 != b1 & a1 != b2) | a1 == "4'" | a1 == "3'" | a1 == "5'"  
    p2 <- (a2 != b1 & a2 != b2) | a2 == "4'" | a2 == "3'" | a2 == "5'"
    ## De enstaka själv-**in**fertila sorter som har S4**’** ej kan befruktas av S4 (utan apostrof).
    if(a1 == "4" & b1 == "4'"){p1 <- FALSE}
    if(a2 == "4" & b2 == "4'"){p2 <- FALSE}
    ## Sum
    comp <- sum(p1, p2)
    return(comp)

    ## calculate relative compatibility
    ## Undantaget är S4**’**-allelen (notera apostrofen) som medför _självfertilitet_. Ett pollenkorn med S4**’** kan befrukta alla mottagare (inklusive de med S4**’**). Körsbär med S4**’** kan således betraktas som universella givare.
    ## Enstaka universella givare saknar också S4**’**.
    ## S3' =  SC
    ## S5' =  SC
}

## compat("S1S2", "S1S3")
## compat("S4'S2", "S1S3")
## compat("S3S4'", "S3S4'")

## dta_toplot <- dta_toplot[, .(var, variety, genotype, genotype1, genotype2, blooming_group_anfic, incompatibility_group, label)]

## select some random varieties for testing
z <- c(48, 12, 13, 40, 12, 4, 79, 5, 40, 47, 3, 24, 36, 28, 10, 40, 38, 54, 68, 47, 19, 73, 64, 43, 23)
z <- unique(z)
## z <- sample(1:nrow(dta_toplot), 25, replace=TRUE)
test <- dta_toplot[z, .(var, genotype, blooming_group_anfic, incompatibility_group)]

## make a df to add cols
newcols <- as.data.table(matrix(nrow = nrow(test), ncol = nrow(test)+1))
names(newcols) <- c("var", test$var)
cols <- names(newcols)
newcols_mod <- newcols[ , (cols) := lapply(.SD, as.character), .SDcols = cols]
newcols_mod$var <- test$var
test <- newcols_mod[test, on = "var"]
for(i in 1:nrow(test)){ ## loop over rows
    for(varn in test$var){ ## loop over cols
        ## get genotype of column name
             ## test[i, (varn) := test[var == varn, genotype]]  
        ## get compatibility value of column name
                test[i, (varn) := compat(test[var == varn, genotype],
                                         test[i, genotype])  ]  
            }
}

## test
## str(test)

## melt
dtplot <-  melt(test, id.vars = c("var", "genotype", "blooming_group_anfic",  "incompatibility_group"))
dtplot[, value := as.numeric(value)]

## fix names
names(dtplot) <- c("target", "genotype", "blooming_group_anfic", "incompatibility_group", "pollinator", "compatibility")

## add blooming time for **pollinator**
tmp <- anfic_selected[var %in% dtplot$pollinator, .(var, blooming_group_anfic)]
names(tmp) <- c("pollinator", "pollinator_blooming_group")
dtplot <- dtplot[tmp, on = "pollinator"]

## [target == "areko", ]
## dtplot[target == "areko", ]
## ?data.table
## X[Y, on=c(x1="y1", x2="y2")]

## calculate blooming proximity
dtplot[, proximity := abs(as.numeric(blooming_group_anfic) - as.numeric(pollinator_blooming_group))]
dtplot[, proximity_ok := ifelse(proximity < 2, TRUE, FALSE)]

## color compatibility and proximity in bloom
## dtplot[, match_color := "red"]
## dtplot[proximity_ok & compatibility == 1, match_color := "lawngreen"]
## dtplot[proximity_ok & compatibility == 2, match_color := "darkgreen"]

dtplot[, compat_proximity := "no"]
dtplot[proximity == 0 & compatibility != 0, compat_proximity := "same"]
dtplot[proximity == 1 & compatibility != 0, compat_proximity := "close"]

## Variable type
## dtplot[, compatibility := as.numeric(compatibility)]
dtplot[, target:= as.factor(target)]
dtplot[, pollinator:= as.factor(pollinator)]
dtplot[, pollinator_blooming_group_num := as.numeric(pollinator_blooming_group)]
dtplot[, blooming_group_num := as.numeric(blooming_group_anfic)]
## str(dtplot)

setkey(dtplot, target)
## str(dtplot)

## plot
pacman::p_load(ggplot2)
library(forcats) ## for reordering plot levels


## labels
tmp <- variety_genotype_group[anfic_bt, on = "var"][, .(var, genotype, label)]
tmp[, label_ss := paste0(label, " [", genotype, "]")]
varnames <- tmp$label_ss
names(varnames) <- tmp$var
dtplot[, target:= factor(target, levels = levels(target), labels = unname(query_label(levels(target), varnames)))]
dtplot[, pollinator:= factor(pollinator, levels = levels(pollinator), labels = unname(query_label(levels(pollinator), varnames)))]

## dtplot[pollinator_blooming_group == "Early", ]

## plot base
p <- ggplot(dtplot, aes(x = fct_reorder(pollinator, pollinator_blooming_group_num), y = fct_reorder(target, blooming_group_num))) +
  geom_point(aes(size = compatibility, colour = compat_proximity))

## plot customization
plot_pollination_table <- p +
    scale_size_area() +
    scale_color_manual(values=c("no" = "red", "close" = "lightgreen", "same" = "chartreuse3")) +
    theme(
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "centimeters"),
        legend.position = "none",
        axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0),
        plot.title = element_text(hjust = 0, vjust = 3, size = 22, face="bold"),
        axis.title.x = element_text(hjust = 0.5, vjust = -5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=18, face="bold")
    ) +
    labs(title="Pollinationsdiagram för körsbär",
         x ="Pollinatör",
         y = "Mottagare")

## add strips
plot_pollination_table <- plot_pollination_table +
    facet_grid(~ pollinator_blooming_group,
               scales = "free",
               ## switch = "x",
               space = "free_x") +
    theme(panel.spacing = unit(0, "lines"),
            panel.background = element_rect(fill = "gray94",
                                colour = "gray",
                                linewidth = 1, linetype = "solid"),
         strip.background = element_rect(colour="black", fill = NA),
         panel.border = element_rect(colour="black", fill = NA),
         strip.placement = "outside"         
         )

plot_pollination_table




compat <- function(genotype_a, genotype_b){
    ## pollinator
    a1 <-  strsplit(genotype_a, "S")[[1]][2]
    a2 <-  strsplit(genotype_a, "S")[[1]][3]
    ## target
    b1 <-  strsplit(genotype_b, "S")[[1]][2]
    b2 <-  strsplit(genotype_b, "S")[[1]][3]
    ## pollen sucess = TRUE
    p1 <- (a1 != b1 & a1 != b2) | a1 == "4'" | a1 == "3'" | a1 == "5'"  
    p2 <- (a2 != b1 & a2 != b2) | a2 == "4'" | a2 == "3'" | a2 == "5'"
    ## De enstaka själv-**in**fertila sorter som har S4**’** ej kan befruktas av S4 (utan apostrof).
    if(a1 == "4" & b1 == "4'"){p1 <- FALSE}
    if(a2 == "4" & b2 == "4'"){p2 <- FALSE}
    ## Sum
    comp <- sum(p1, p2)
    return(comp)

    ## calculate relative compatibility
    ## Undantaget är S4**’**-allelen (notera apostrofen) som medför _självfertilitet_. Ett pollenkorn med S4**’** kan befrukta alla mottagare (inklusive de med S4**’**). Körsbär med S4**’** kan således betraktas som universella givare.
    ## Enstaka universella givare saknar också S4**’**.
    ## S3' =  SC
    ## S5' =  SC
}

## compat("S1S2", "S1S3")
## compat("S4'S2", "S1S3")
## compat("S3S4'", "S3S4'")

## with uncertain genotypes, separated by "/" calculate worst case scenario
## "S1S6/S3S4"
## "S1S4'/S3S4'"
## "S1S6/S4S6"

## version that calculates with uncertain genotypes marked "/"

## genotype_a <- "S1S6/S3S4"
## genotype_b <- "S1S6/S4S6"
## genotype_b <- "S4S6"
## compat("S1S6/S3S4", "S4S6")
## compat("S1S3", "S4S6")
## compat("S1S3", "S4S6/S1S3")
## compat("S4'S3?", "S4S6/S1S3")
## compat("S5S8/S2S4", "S4S6/S1S3")
## genotype_a <- "S5S8/S2S4"
## genotype_b <- "S4S6/S1S3"
## compat(genotype_a, genotype_b)

compat <- function(genotype_a, genotype_b){
    ## remove questionmark
    genotype_a <- gsub("\\?", "", genotype_a)
    genotype_b <- gsub("\\?", "", genotype_b)
    ## for uncertain genotypes
    uncertain_a <- grepl("/", genotype_a)
    uncertain_b <- grepl("/", genotype_b)
    if(uncertain_a){ ## eg "S1S6/S3S4"
        genotype_a_alt <- strsplit(genotype_a, "/")[[1]][2]
        genotype_a <- strsplit(genotype_a, "/")[[1]][1]
    }
    if(uncertain_b){
        genotype_b_alt <- strsplit(genotype_b, "/")[[1]][2]
        genotype_b <- strsplit(genotype_b, "/")[[1]][1]
    }
    ## pollinator
    a1 <-  strsplit(genotype_a, "S")[[1]][2]
    a2 <-  strsplit(genotype_a, "S")[[1]][3]
    ## target
    b1 <-  strsplit(genotype_b, "S")[[1]][2]
    b2 <-  strsplit(genotype_b, "S")[[1]][3]
    ## pollen sucess = TRUE
    p1 <- (a1 != b1 & a1 != b2) | a1 == "4'" | a1 == "3'" | a1 == "5'"  
    p2 <- (a2 != b1 & a2 != b2) | a2 == "4'" | a2 == "3'" | a2 == "5'"
    ## De enstaka själv-**in**fertila sorter som har S4**’** ej kan befruktas av S4 (utan apostrof).
    if(a1 == "4" & b1 == "4'"){p1 <- FALSE}
    if(a2 == "4" & b2 == "4'"){p2 <- FALSE}


    ## for uncertain genotypes
    if(uncertain_a){

        ## pollinator
        a1_alt <-  strsplit(genotype_a_alt, "S")[[1]][2]
        a2_alt <-  strsplit(genotype_a_alt, "S")[[1]][3]

        ## pollen sucess = TRUE
        p1_alt <- (a1_alt != b1 & a1_alt != b2) | a1_alt == "4'" | a1_alt == "3'" | a1_alt == "5'"  
        p2_alt <- (a2_alt != b1 & a2_alt != b2) | a2_alt == "4'" | a2_alt == "3'" | a2_alt == "5'"
        if(a1_alt == "4" & b1 == "4'"){p1_alt <- FALSE}
        if(a2_alt == "4" & b2 == "4'"){p2_alt <- FALSE}

        ## if also genotype b is uncertain
        if(uncertain_b){
            ## target
            b1_alt <-  strsplit(genotype_b_alt, "S")[[1]][2]
            b2_alt <-  strsplit(genotype_b_alt, "S")[[1]][3]
            p1_alt2 <- (a1_alt != b1_alt & a1_alt != b2_alt) | a1_alt == "4'" | a1_alt == "3'" | a1_alt == "5'"  
            p2_alt2 <- (a2_alt != b1_alt & a2_alt != b2_alt) | a2_alt == "4'" | a2_alt == "3'" | a2_alt == "5'"
            if(a1_alt == "4" & b1_alt == "4'"){p1_alt2 <- FALSE}
            if(a2_alt == "4" & b2_alt == "4'"){p2_alt2 <- FALSE}
            ## take worst case
            p1_alt <- min(p1_alt, p1_alt2)
            p2_alt <- min(p2_alt, p2_alt2)

        }
    }

            ## if genotype_b is uncertain
        if(uncertain_b){
            ## target
            b1_alt <-  strsplit(genotype_b_alt, "S")[[1]][2]
            b2_alt <-  strsplit(genotype_b_alt, "S")[[1]][3]
            p1_alt2b <- (a1 != b1_alt & a1 != b2_alt) | a1 == "4'" | a1 == "3'" | a1 == "5'"  
            p2_alt2b <- (a2 != b1_alt & a2 != b2_alt) | a2 == "4'" | a2 == "3'" | a2 == "5'"
            if(a1 == "4" & b1_alt == "4'"){p1_alt2b <- FALSE}
            if(a2 == "4" & b2_alt == "4'"){p2_alt2b <- FALSE}
            ## take worst case
            if(uncertain_a){
            p1_alt <- min(p1_alt, p1_alt2b)
            p2_alt <- min(p2_alt, p2_alt2b)
            }else{
                p1_alt <- min(p1, p1_alt2b)
                p2_alt <- min(p2, p2_alt2b)
            }
        }


    if(uncertain_a|uncertain_b){
            ## take worst case
            p1 <- min(p1_alt, p1)
            p2 <- min(p2_alt, p2)
}

    ## Sum
    comp <- sum(p1, p2)
    return(comp)

    ## calculate relative compatibility
    ## Undantaget är S4**’**-allelen (notera apostrofen) som medför _självfertilitet_. Ett pollenkorn med S4**’** kan befrukta alla mottagare (inklusive de med S4**’**). Körsbär med S4**’** kan således betraktas som universella givare.
    ## Enstaka universella givare saknar också S4**’**.
    ## S3' =  SC
    ## S5' =  SC
}




## superscript in tick labels

### bquote

mylabs <- levels(dtplot$target)
mylabs <- levels(dtplot$target)[2:3]
mylabs <- c("almore")
plot_pollination_table + scale_y_discrete("Mottagare", labels = bquote(.(mylabs)^1))

bquote(expression(mylabs))
expression(mylabs)
bquote("^mylabs")
bquote(mylabs^s)

lapply(mylabs, function(x){bquote(x)})
bquote(get(mylabs)^s)

### parse

## https://stackoverflow.com/questions/28978011/how-to-subscript-the-x-axis-tick-label

mylabs <- levels(dtplot$target)
plot_pollination_table + scale_y_discrete("Mottagare", labels = parse(text = levels(dtplot$target))) ## unexpected symbol error

mylabs <- c("alm**öre^X")
plot_pollination_table + scale_y_discrete("Mottagare", labels = parse(text = mylabs)) ## parse not working with blankspace

mylabs <- c("alm~öre^X")
plot_pollination_table + scale_y_discrete("Mottagare", labels = parse(text = mylabs)) ## parse not working with blankspace


mylabs <- levels(dtplot$target)
plot_pollination_table + scale_y_discrete("Mottagare", labels = expression("PM"^10))
plot_pollination_table + scale_y_discrete("Mottagare", labels = expression(mylabs))
mylabs <- gsub("\\[|\\]", "", mylabs)
plot_pollination_table + scale_y_discrete("Mottagare", labels = expression(get(mylabs)))

 text(1:3, (3:1)-0.03, labels= mapply(function(x,y)
      as.expression(bquote(.(x)^.(y))
                    ),
      first, second))

mylabs_sup <- gsub("^[^\\^]*", "", mylabs)
mylabs_sup <- gsub("\\^", "", mylabs_sup)
mylabs_test <- gsub("\\^.*", "", mylabs)
mylabs_test <- gsub("\\*", "", mylabs_test)


plot_pollination_table +
    scale_y_discrete("Mottagare3", labels = mapply(function(x,y){as.expression(bquote(.(x)^.(y)))},
      mylabs_test, mylabs_sup)) 


plot_pollination_table +
    scale_y_discrete("Mottagare", labels = paste0(
      mylabs_test, mylabs_sup)) 

plot_pollination_table +
    scale_y_discrete("Mottagare", labels = paste0(
      mylabs_test, "^", mylabs_sup)) 

plot_pollination_table +
    scale_y_discrete("Mottagare3", labels = as.expression(bquote(.(mylabs_test)^.(mylabs_sup)))) 


plot_pollination_table +
    scale_y_discrete("Mottagare", labels = as.expression(mapply(function(x, y){bquote(.(x)^.(y))}, mylabs_test, mylabs_sup))
                                      )

