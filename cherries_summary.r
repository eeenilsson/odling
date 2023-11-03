## summary tables and plots etc

pacman::p_load(ggplot2)
library(forcats) ## for reordering plot levels

## add pollination info from websites -----

tmp <- data.table(dta)
tmp <- tmp[, .(var, pollinated_by_concordance_chr)]
tmp[, pol := gsub("NA \\([^,]*", "", pollinated_by_concordance_chr)]
tmp[, pol := gsub("^, ", "", pol)]
source('../functions/removeParens.r')
tmp[, pol := deleteParens(pol)]
tmp[, pol := gsub(" , ", ", ", pol)]
tmp[, pol := gsub(" ,$", "", pol)]
tmp[, pol := gsub(" ,,", "", pol)]
tmp <- tmp[, .(var, pol)]

dtplot <- dtplot[, var := target]
dtplot <- tmp[dtplot, on = "var"]

myfun <- Vectorize(grepl)
dtplot[, pol_yes := myfun(pollinator, pol)] ## check matches
dtplot[, pol := NULL]
dtplot[, pol_yes := as.numeric(pol_yes)]
dtplot[, pol_yes := factor(pol_yes, levels = 0:1, labels = c("no", "yes"))]

## dtplot[pol_yes == "yes" & compatibility == 0, .(var, pollinator, pol_yes, compatibility)] ## discrepancy

## plot pollination table ---------------------

## make labels
tmp <- variety_genotype_group[, .(var, genotype, label, incompatibility_group)]

sanitize_label <- function(x){
x <- gsub(" \\([^$]*", "", x)
x <- gsub(" \\\n[^$]*", "", x)
x <- gsub(" \\/[^$]*", "", x)
x <- gsub("TM$", "", x)
x <- gsub("Späte Rote Knorpelkirsche", "Rote", x)
x <- gsub("Knauffs Schwarze", "Knauffs", x)
x <- gsub("Große Schwarze Knorpel", "Große Schwarze", x)
x <- gsub("Dönissens Gelbe Knorpel", "Dönissens Gelbe", x)
x <- gsub("Guigne d'Annonay", "Annonay", x)
x <- gsub("Schneiders Späte Knorpelkirsche", "Schneiders Späte", x)

return(x)
}
tmp[, label := sanitize_label(label)]
tmp[, label := ifelse(incompatibility_group == "SC", paste0(label, "*"), label)] ## add asterisk for SC
tmp[, label_ss_gr := paste0(label, " [", genotype, ", ", incompatibility_group, "]")]
tmp[, label_ss := paste0(label, " [", genotype, "]")]
tmp[, label_gr := paste0(label, "^[", incompatibility_group, "]")]

## varnames <- tmp$label_ss_gr ## S-alleles and incompat group added
## varnames <- tmp$label_gr ## Only incompat group added
varnames <- tmp$label_ss ## Only ss added
names(varnames) <- tmp$var

dtplot[, target := factor(target, levels = levels(target), labels = unname(query_label(levels(target), varnames)))]

## factor(dtplot$target, levels = levels(dtplot$target), labels = unname(query_label(levels(dtplot$target), varnames)))

varnames2 <- gsub("|\\^\\[[^$]*", "", varnames) ## remove part in brackets for pollinators
varnames2 <- gsub(" $", "", varnames2)
varnames2 <- gsub("\\[.*", "", varnames2)
## varnames2 <- gsub("\\*", "", varnames2) ## remove asterisk

dtplot[, pollinator:= factor(pollinator, levels = levels(pollinator), labels = unname(query_label(levels(pollinator), varnames2)))]

## dtplot[pollinator_blooming_group_num == "Early", ]

## ## plot base #############
## p <- ggplot(dtplot, aes(x = fct_reorder(pollinator, pollinator_blooming_group_num), y = fct_reorder(target, bgr))) +
##   geom_point(aes(size = compatibility, colour = compat_proximity))
##################

## plot base
p <- ggplot(dtplot, aes(x = fct_reorder(pollinator, pollinator_blooming_group_num), y = fct_reorder(target, bgr))) +
  geom_point(aes(size = compatibility,  fill = compat_proximity, colour = pol_yes), shape = 21, stroke = 1) + scale_color_manual(values=c("no" = "white", "yes" = "black"))

## plot customization
plot_pollination_table <- p +
    scale_size_area() +
    scale_fill_manual(values=c("no" = "red", "close" = "lightgreen", "same" = "chartreuse3", "bt_unknown" = "white")) +
    theme(
        plot.margin = unit(c(0.9, 0.9, 0.9, 0.9), "centimeters"),
        legend.position = "none",
        axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0),
        plot.title = element_text(hjust = 0, vjust = 3, size = 12, face="bold"),
        axis.title.x = element_text(hjust = 0.5, vjust = -5),
        axis.text=element_text(size=14),
        axis.title=element_text(size=12, face="bold")
    ) +
    labs(title="Blomningstid",
         x ="Pollinatör",
         y = "Mottagare")

## add strips
plot_pollination_table <- plot_pollination_table +
    facet_grid(~ pollinator_blooming_group,
               scales = "free",
               ## switch = "x",
               space = "free_x") +
    theme(
        panel.spacing = unit(0, "lines"),
            panel.background = element_rect(fill = "gray94",
                                colour = "gray",
                                linewidth = 1, linetype = "solid"),
         strip.background = element_rect(colour="black", fill = NA),
         panel.border = element_rect(colour="black", fill = NA),
         strip.placement = "outside",
        plot.title = element_text(hjust = 0.5)
         )

plot_pollination_table

## ## superscript incompat gr KEEP #############################
## mylabs <- levels(dtplot$target)
## mylabs <- gsub("\\[|\\]", "", mylabs)
## ## mylabs <- gsub("^[^\\^]*", "", mylabs)
## mylabs <- gsub(" ", "~", mylabs) ## parse not working with blankspace
## mylabs <- gsub("\\*", "", mylabs)
## mylabs <- gsub("/", "**~eller", mylabs)
## mylabs <- gsub("\\^SC", "**sk", mylabs)
## ## mylabs <- gsub("/", "~**", mylabs)
## plot_pollination_table + scale_y_discrete("Mottagare", labels = parse(text = mylabs))
##############################################################

ggsave(
  "plot_pollination_table.png",
  plot = last_plot(),
  device = NULL,
  path = "../dropbox/images/plants/",
  scale = 1,
  width = NA,
  height = NA,
  units = c("in", "cm", "mm", "px"),
  dpi = 300,
  limitsize = TRUE,
  bg = NULL
)

## meta:
## Size of dot corresponds to genetic compatibility, color to blooming time (dark green = same bloomin group, light green = proximity 1 in blooming group, red = outside proximity blooming groups OR not genetically compatible). Black circles = noted as pollinator on a swedish website.
## "Blomningstid"


## plot relative BT start from eur data -----------
## Note: Coefs are adjusted for site and year. The intercept for duration was 7.53 and the ref category for site was Balandran, southern france. The intercept for BT start (number of days from start of year) was 110.
## bt_start_relative is relative to median of the coef for all varieties

## unique(eur_lm_bt_duration$var)
## str(eur_lm_bt_start)

## ## boxplot ## Not useful for aggregated data
## p <- ggplot(eur_lm_bt_start, aes(x=var, y=coef_bt_start)) + geom_boxplot()
## p <- p + geom_dotplot(binaxis='y', stackdir='center', dotsize=0.2)
## # Rotate the box plot
## p + coord_flip()
## ## summary(tmp$test_start_date)

## plot bloom time from rosbreed ----------------------
toplot <- bloom_table
p <- ggplot(toplot, aes(y=Germplasm, x=bt))
p_bt <- p + geom_point() + xlab("Start of bloom (days from jan 1st)") + ylab("")

## break_quantiles <- quantile(toplot$bt, probs = seq(0, 1, by = 1/5))
## Break at dates: https://stackoverflow.com/questions/39257867/date-minor-breaks-in-ggplot2
## plot bloom time relative to lowest bloom time
## https://stackoverflow.com/questions/74924921/how-to-put-axis-labels-in-between-the-axis-ticks-in-ggplot2

break_in_5 <- seq.int(from = min(toplot$bt0), to = max(toplot$bt0), by = max(toplot$bt0)/5)
break_in_5 <- round(break_in_5, digits = 0)
break_minor <- seq(min(break_in_5)+1, max(break_in_5)-1, by = 1)

## base plot
p <- ggplot(toplot, aes(y=Germplasm, x=bt0))
p_bt0 <- p + geom_point() + xlab("Start av blomning (dagar från tidigaste sorten)") + ylab("")

p_bt0 <- p_bt0 + scale_x_continuous(breaks = break_in_5,
                           minor_breaks = break_minor)

## labels outside plot area
p_bt0 <- p_bt0 + theme(plot.margin = unit(c(2,1,1,1), "cm"))
p_bt0 <- p_bt0 + coord_cartesian(ylim = c(0, length(levels(toplot$Germplasm))), clip = "off")

## p_bt0 + annotate("text", x = 1, y = length(levels(toplot$Germplasm)) + 2, label = "text")
## i <- 1

## calculate text positions
text_xpos <- c() 
for(i in 1:length(break_in_5)-1){
                text_xpos <- c(text_xpos, break_in_5[i] + (break_in_5[i+1] - break_in_5[i])/2)
}

## annotate plot
p_bt0_annotated <- p_bt0
for(i in 1:length(text_xpos)){
    p_bt0_annotated <- p_bt0_annotated  +
        annotate("text",
                 x = text_xpos[i],
                 y = length(levels(toplot$Germplasm)) + 2,
                 label = paste("Period", i),
                 colour = "aquamarine4")
}

## rgb(red, green, blue, alpha)
## quantity of red (between 0 and 1), of green and of blue, and finally transparency (alpha).
## rgb(0.2,0.5,1,0.7)

## pacman::p_load("RColorBrewer")
red <- seq(0, 1 , length.out = length(levels(toplot$Germplasm)))
blue <-rev(seq(0, 1 , length.out = length(levels(toplot$Germplasm))))
green <- seq(-0.85, 0.85 , length.out = length(levels(toplot$Germplasm)))
green <- 1-green*green ## ascending-descending
usecolors <- rgb(red, green, blue, 0.7)

## darkolivegreen3

p_bt0_annotated <- p_bt0_annotated + 
  geom_point(aes(color=Germplasm)) +
    scale_color_manual(values = usecolors) + theme(legend.position = "none")
ggsave("bt_start_ros.png", path = "../dropbox/images/plants/")

## colors
## pacman::p_load("RColorBrewer")
## ## View a single RColorBrewer palette by specifying its name
## display.brewer.pal(n = 8, name = 'RdBu')
## display.brewer.all()

## brewer.pal(n = length(levels(toplot$Germplasm), name = "RdBu"))
## display.brewer.pal(n = length(levels(toplot$Germplasm)), name = 'RdBu')

## The average blossoming period for cherries when pollination can take place is about seven to eight days.


## Bubblechert : https://r-graph-gallery.com/320-the-basis-of-bubble-plot.html

## https://stackoverflow.com/questions/75899955/scale-ticks-breaks-to-percentile-in-continuous-colourbar-in-scale-fill-gradient

## selectvars <- c(vars_id, "Bloom_Days", "Bloom_Time")
## ## ros_bloom[ , ..selectvars]
## str(ros_bloom)

## ros_bloom[, .(bt = mean(Bloom_Days, na.rm = TRUE), gdd = mean(Bloom_Time, na.rm = TRUE)), by = Germplasm]

## ros[ , median(Bloom_Days), by = list(Germplasm, Species, Species)]
## str(ros)

## ros[ , count:=sum(col3), by = list(col1, col2)]

## summary(ros_bloom)

## data_frame[, lapply(.SD, sum), by= col1]

## Tables -----------------------

## Rosbreed aggregated phenology data
ros_phenology_aggr <- fread("ros_phenology_aggr.csv")
ros_phenology_aggr_curated <- ros_phenology_aggr

## colnames
varnames_cols <- c(
'var' = "Sort",
'ta_q' = "Syra",
'sweetness_q' = "Sötma",
'firmness_q' = "Fasthet",
'wt_q' = "Vikt",
'freestone_q' = "Kärnsläpp",
'skin_mahogany_q' = "Rödbrunhet",
'flesh_color' = "Kött",
'pm_q' = "Mjöldagg"
)
names(ros_phenology_aggr_curated) <- query_label(names(ros_phenology_aggr_curated), varnames_cols)

## varnames
ros_phenology_aggr_curated[, Sort := query_label(Sort, varnames)]
write.csv(ros_phenology_aggr_curated, "ros_phenology_aggr_curated.csv", row.names = FALSE) ## for web page

## dt[, pollinator:= factor(pollinator, levels = levels(pollinator), labels = unname(query_label(levels(pollinator), varnames)))]


## collected data
cherries_table <- read.csv("cherries_table.csv")
cols <- c("label", "type", "zone", "size", "maturity_rank", "sweet", "sour", "firm", "pulp_color", "skin_color", "good_taste", "sylt", "eplanta"
)
cherries_table <- cherries_table[, names(cherries_table) %in% cols]
str(cherries_table)

## factor levels
cherries_table$type <- factor(cherries_table$type, ordered = FALSE, levels = c("sour", "sweet"), labels = c("Sur", "Söt"))  ## Todo: Tag med buskkörsbär här

cherries_table$pulp_color <- factor(
    cherries_table$pulp_color,
    ordered = TRUE,
    levels =  c("", "yellow_white", "yellow", "orange", "light_red", "red", "dark_red"),
    labels =  c("", "Gulvitt", "Gult", "Orange", "Röd ljus", "Röd", "Röd mörk")            
)

cherries_table$skin_color <- factor(
    cherries_table$skin_color,
    ordered = TRUE,
    levels =  c("yellow", "yellow_red", "red", "dark_red"),
    labels =  c("Gult", "Gulrött", "Rött", "Rött mörkt"),    
)

## column labels
tmp <- c('label' = "Sort", 'type' = "Typ", 'eplanta' = "Eplanta", 'zone' = "Zon", 'size' = "Storlek", 'maturity_rank' = "Mognad", 'sweet' = "Söt", 'sour' = "Syrlig", 'firm' = "Fasthet", 'pulp_color' = "Kött", 'skin_color' = "Färg", 'good_taste' = "God", 'sylt' = "Sylt")
names(cherries_table) <- query_label(names(cherries_table), tmp)

write.csv(cherries_table, "cherries_table_curated.csv", row.names = FALSE)



