
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

