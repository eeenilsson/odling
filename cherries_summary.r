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

dtplot <- bt_long[, var := target]
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

## for site
varnames3 <- tmp$label
names(varnames3) <- tmp$var


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
    geom_point(aes(size = compatibility,  fill = compat_proximity, colour = pol_yes), shape = 21, stroke = 1)

## plot customization
plot_pollination_table <- p +
    scale_size_area() +
    scale_fill_manual(values=c("no" = "red", "stretch" = "cadetblue" ,"close" = "lightgreen", "same" = "chartreuse3", "bt_unknown" = "white")) +
    scale_color_manual(values=c("no" = "white", "yes" = "black")) +
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
         y = "Mottagare [haplotyp]")

## add strips
plot_pollination_table <- plot_pollination_table +
    facet_grid(~ pollinator_blooming_group,
               scales = "free",
               ## switch = "x",
               space = "free_x") +
    theme(
        panel.spacing = unit(0, "lines"),
        panel.background = element_rect(fill = "gray83",
                                        colour = "gray",
                                        linewidth = 0.8, linetype = "solid"),
        strip.background = element_rect(colour="black", fill = NA),
        strip.text = element_text(size = 10, color = "black"
                                ##, face = "bold"
                                  ),
        panel.border = element_rect(colour="black", fill = NA),
        strip.placement = "outside",
        panel.grid.major = element_line(linewidth = 1, linetype = 'solid',
                                        colour = "white"),
        plot.title = element_text(hjust = 0.5)
    )

plot_pollination_table


## ## check:
## cols <- names(dta)[grepl("pollinat.*", names(dta))]
## dta[var == "regina", ..cols]

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
  width = 16.6,
  height = 8.86,
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

## BT wide table for filtering --------------------------------

bt_wide_curated <- data.table(bt_wide)

## categorize bgr
bt_wide_curated[, blooming_group := round(bgr, digits = 0)]
## bt_wide_curated[, blooming_group := factor(blooming_group, ordered = TRUE, levels = c(1:5, 99), labels = c("Tidig", "Medeltidig", "Medel", "Medelsen", "Sen", "Okänd"))]
bt_wide_curated[, label := query_label(var, varnames3)]

## todo: add synonyms

cols <- c("label", "var", "genotype", "incompatibility_group", "blooming_group", "bgr")
cols2 <- names(bt_wide_curated)[!names(bt_wide_curated) %in% cols]
cols2 <- sort(cols2)
cols <- c(cols, cols2)
bt_wide_curated <- bt_wide_curated[, ..cols] ## order cols

## loop over columns and rows to get compat and BT proximity
cols_loop <- cols
cols_loop <- cols_loop[c(2, 7:length(cols))]
res <- c()
for(i in 1:nrow(bt_wide_curated)){ 
    out <- c()
    proximity <- c()
    for(n in cols_loop){        
        if(sum(grepl(paste0("^", n, "$"), bt_wide_curated[, var]))>0){
            bg_pol <- bt_wide_curated[var == n, as.numeric(blooming_group)]
            proximity <- bt_wide_curated[i, as.numeric(blooming_group)] - as.numeric(bg_pol)
            proximity <- abs(proximity)
            if(proximity == 0){note <- "++"}
            if(proximity == 1){note <- "+"}
            if(proximity == 2){note <- "-"}
            if(proximity == 3){note <- "--"}
            if(proximity == 3){note <- "---"}            
        }else{
             note <- ""
        }
        compat <- bt_wide_curated[i, get(n)]
        note <- ifelse(compat == 0, "", note)
        compat <- gsub("2", "A", compat)
        compat <- gsub("1", "B", compat)
        out <- c(out, paste0(compat, note))        
    }
    res <- rbind(res, out)
}
res <- as.data.table(res)
names(res) <- cols_loop
## res
cols_new <- cols[!cols %in% c(cols_loop,  "incompatibility_group", "bgr")]
cols_new <- c("var", cols_new)
tmp <- bt_wide_curated[, ..cols_new]

bt_wide_curated <- tmp[res, on = "var"] ## this replaces compat

## Dela upp i blooming group 1-2, 2-3, 3-4, 4-5
bg12 <- bt_wide_curated[blooming_group == 1|blooming_group == 2, ]
cols12 <- names(bg12)[names(bg12) %in% bg12$var]
cols12 <- c("label", "genotype",  "blooming_group", cols12)
bg12 <- bg12[, ..cols12]
write.csv(bg12, "bg12.csv", row.names = FALSE)

## value <- names(bg12)[[1]]
## function(value){unname(query_label(value, varnames3))}

bg23 <- bt_wide_curated[blooming_group == 2|blooming_group == 3, ]
cols23 <- names(bg23)[names(bg23) %in% bg23$var]
cols23 <- c("label", "genotype",  "blooming_group", cols23)
bg23 <- bg23[, ..cols23]
write.csv(bg23, "bg23.csv", row.names = FALSE)

bg34 <- bt_wide_curated[blooming_group == 3|blooming_group == 4, ]
cols34 <- names(bg34)[names(bg34) %in% bg34$var]
cols34 <- c("label", "genotype",  "blooming_group", cols34)
bg34 <- bg34[, ..cols34]
write.csv(bg34, "bg34.csv", row.names = FALSE)

bg45 <- bt_wide_curated[blooming_group == 4|blooming_group == 5, ]
cols45 <- names(bg45)[names(bg45) %in% bg45$var]
cols45 <- c("label", "genotype",  "blooming_group", cols45)
bg45 <- bg45[, ..cols45]
write.csv(bg45, "bg45.csv", row.names = FALSE)

bt_wide_curated_bt <- bt_wide_curated[, .(var, genotype, blooming_group)]
## Save for site
cols <- cols[!cols == "var"] ## drop var
cols <- cols[cols %in% names(bt_wide_curated)] ## drop vars not in _curated
bt_wide_curated <- bt_wide_curated[, ..cols]

write.csv(bt_wide_curated, "bt_wide_curated.csv", row.names = FALSE)
varnames3 <- c(varnames3, c('label' = "Sort", 'genotype' = "Haplotyp", 'blooming_group' = "Blomningsgrupp"))
saveRDS(varnames3, "varnames3.rds") 


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
p_bt0 <- p +
    geom_point(size = 5) +
    xlab("Start av blomning (dagar från tidigaste sorten)") +
    ylab("")

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
                 colour = "aquamarine4",
                 size = 12)
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

p_bt0_annotated + theme(
                      axis.text=element_text(size=14),
                      axis.title=element_text(size=18,face="bold")
                  )


ggsave("bt_start_ros.png", path = "../dropbox/images/plants/", width = 16.6, height = 8.86,
)




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
ros_phenology_aggr_curated[, Sort := query_label(Sort, varnames2)]
write.csv(ros_phenology_aggr_curated, "ros_phenology_aggr_curated.csv", row.names = FALSE) ## for web page

## dt[, pollinator:= factor(pollinator, levels = levels(pollinator), labels = unname(query_label(levels(pollinator), varnames)))]

## collected data -------------
cherries_table <- fread("cherries_table.csv")
cherries_table <- bt_wide_curated_bt[cherries_table, on = "var"] ## add blooming_group

cols <- c("var", "label", "genotype", "type", "zone", "blooming_group", "size", "maturity_rank", "sweet", "sour", "firm", "pulp_color", "skin_color", "good_taste", "sylt", "eplanta"
)

cherries_table <- cherries_table[, ..cols]
## str(cherries_table)

## factor levels
cherries_table$type <- factor(cherries_table$type, ordered = FALSE, levels = c("sour", "sweet"), labels = c("Cerasus", "Avium"))  ## Todo: Tag med buskkörsbär här

cherries_table$sylt <- factor(cherries_table$sylt, ordered = TRUE, levels = c(0:2), labels = c("Nej", "Kanske", "Ja"))

cherries_table$eplanta <- factor(cherries_table$eplanta, ordered = TRUE, levels = c(0:1), labels = c("Nej", "Ja"))

cherries_table$size <- factor(cherries_table$size, ordered = TRUE, levels = c(1:3), labels = c("Liten", "Medel", "Stor"))

cherries_table$maturity_rank <- factor(cherries_table$maturity_rank, ordered = TRUE, levels = c(1:3), labels = c("Tidig", "Mitten", "Sen"))

cherries_table$sweet <- factor(cherries_table$sweet, ordered = TRUE, levels = c(1:3), labels = c("Låg", "Medel", "Hög"))

cherries_table$sour <- factor(cherries_table$sour, ordered = TRUE, levels = c(1:3), labels = c("Låg", "Medel", "Hög"))

cherries_table$firm <- factor(cherries_table$firm, ordered = TRUE, levels = c(1:3), labels = c("Mjuk", "Medel", "Fast"))

cherries_table$good_taste <- factor(cherries_table$good_taste, ordered = TRUE, levels = c(1:3), labels = c("Nej", "Ja", "Ja!"))

cherries_table$pulp_color <- factor(
    cherries_table$pulp_color,
    ordered = TRUE,
    levels =  c("", "yellow_white", "yellow", "orange", "light_red", "red", "dark_red"),
    labels =  c("", "Gulvitt", "Gult", "Orange", "Ljusrött", "Rött", "Mörkrött")            
)

cherries_table$skin_color <- factor(
    cherries_table$skin_color,
    ordered = TRUE,
    levels =  c("yellow", "yellow_red", "red", "dark_red"),
    labels =  c("Gul", "Gulröd", "Röd", "Mörkröd"),    
)

## column labels
tmp <- c('label' = "Sort", 'genotype' = "Haplotyp", 'type' = "Typ", 'eplanta' = "Eplanta", 'zone' = "Zon", 'blooming_group' = "Blomning", 'size' = "Storlek", 'maturity_rank' = "Mognad", 'sweet' = "Sötma", 'sour' = "Syra", 'firm' = "Fasthet", 'pulp_color' = "Kött", 'skin_color' = "Färg", 'good_taste' = "God", 'sylt' = "Sylt")
names(cherries_table) <- query_label(names(cherries_table), tmp)

write.csv(cherries_table[, -1], "cherries_table_curated.csv", row.names = FALSE)


## Sorter lista --------------

cherries_table$Blomning <- factor(cherries_table$Blomning, ordered = TRUE, levels = c(1:5), labels = c("tidigt under", "medeltidigt under", "i mitten av", "medelsent under", "sent under"))
features <- fread("cherries_table.csv")
features <- features[, .(var, prominent_features)]
origin <- variety_genotype_group[, .(var, origin_country, mother, father, incompatibility_group)]
## genotype <- bt_wide[, .(var, genotype, incompatibility_group)]
syn <- fread("genotype_syn.csv")

tmp <- features[cherries_table, on = "var"]
## tmp <- features[tmp, on = "var"]
tmp <- origin[tmp, on = "var"]
tmp <- syn[tmp, on = "var"]

pictures_list <- list.files("../dropbox/images/plants/", "png$|jpg$|jpeg$|.webp$|.avif$")
pictures_list <- data.table(pictures_list)
pictures_list[, var := gsub(".png$|.jpg$|.jpeg$|.webp$|.avif$", "", pictures_list)]
pictures_list <- pictures_list[grepl(paste0(tmp$var, collapse = "$|"), var), ]
pictures_list[, image_path := paste0("(../../dropbox/images/plants/", pictures_list, "){width=40%}")]
pictures_list <- pictures_list[, .(var, image_path)]

tmp <- pictures_list[tmp, on = "var"]
varieties <- tmp

## varieties[, Typ := gsub("Avium", "_Prunus Avium_", Typ)]
## varieties[, Typ := gsub("Cerasus", "_Prunus Cerasus_", Typ)]

## varieties[, txt := paste0(Blomning, " blomning och mognad ", tolower(Mognad), ". ", Sötma, " sötma och ", Syra, " med ", Fasthet, " ", Kött, " och ", Färg, " färg på skalet.")]

## Färg och smak
varieties[, note_fasthet := ifelse(!is.na(Fasthet), as.character(Fasthet), "")]
varieties[, note_fasthet := gsub("Fast", "fast", note_fasthet)]
varieties[, note_fasthet := gsub("Mjuk", "mjukt", note_fasthet)]
varieties[, note_fasthet := gsub("Medel", "medelfast", note_fasthet)]
varieties[, note_color := ifelse(!is.na(Färg), paste0(Färg, " färg på skalet"), "")]
varieties[, note_fleshcolor := ifelse(!is.na(Färg), paste0("och ", tolower(Kött), ", ", note_fasthet," kött"), "")]
varieties[, Sötma := tolower(Sötma)]
varieties[, Sötma := gsub("medel", "medelhög", Sötma)]
varieties[, Syra := tolower(Syra)]
varieties[, Syra := gsub("medel", "medelhög", Syra)]
varieties[, note_sweet := ifelse(!is.na(Sötma) & !is.na(Syra), paste0("som har ", Sötma, " sötma och ", Syra, " syra"), "")]
varieties[, note_sweet := ifelse(!is.na(Sötma) & is.na(Syra), paste0("som har ", Sötma, " sötma"), note_sweet)]
varieties[, note1 := paste0(note_color, " ", note_fleshcolor, " ", note_sweet, ".")]

varieties[, note_tmp := ifelse(Storlek == "Liten", " Bäret är relativt litet.", "")]
varieties[, note_tmp := ifelse(Storlek == "Medel", " Bäret är medelstort.", note_tmp)]
varieties[, note_tmp := ifelse(Storlek == "Stor", " Bäret är stort.", note_tmp)]
varieties[is.na(note_tmp), note_tmp := ""]
varieties[, note1 := paste0(note1, note_tmp)]

varieties[, note_tmp := ifelse(God == "Ja", " Smaken beskrivs som god.", "")]
varieties[, note_tmp := ifelse(God == "Ja!", " Smaken beskrivs som mycket god.", note_tmp)]
varieties[, note1 := paste0(note1, note_tmp)]

## Blomning och mognad
varieties[, note_tmp := ifelse(!is.na(Blomning), paste0("Blommar ", Blomning, " blomningsperioden"), "")]
varieties[, note_maturity := ifelse(!is.na(Mognad), as.character(Mognad), "")]
varieties[, note_maturity := gsub("Mitten", " och mognar i mitten av mognadsperioden", note_maturity)]
varieties[, note_maturity := gsub("Sen", " och mognar sent", note_maturity)]
varieties[, note_maturity := gsub("Tidig", " och mognar tidigt", note_maturity)]
varieties[, note2 := paste0(note_tmp, note_maturity, ".")]

## aggregate note
varieties[, note_txt := paste0(note1, " ", note2, " ", prominent_features)]
varieties[, note_txt := gsub("$", ".", note_txt)]
varieties[, note_txt := gsub("\\.\\.", "\\.", note_txt)] ## add dot

## pollinators
poll <- bt_long
poll <- poll[compatibility > 0, ]
poll <- poll[compat_proximity == "same"|compat_proximity == "close", ]
poll$pollinator <- query_label(poll$pollinator, varnames3)
poll[, pollinators := paste0(pollinator, collapse=", "), by=c("target")]
poll <- unique(poll[, .(var, pollinators)])
## varieties$var[!varieties$var %in% poll$var]
varieties <- poll[varieties, on = "var"]
varieties[, pollinators := gsub("\\*", "&ast;", pollinators)]
## varieties[, pollinators]
varieties[, pollinators := ifelse(incompatibility_group == "SC", paste0(pollinators, ". Självfertil"), pollinators)]


## varieties[!is.na(father), .(father, mother)]
## varieties[!is.na(father), .(origin)]

## origin
## iso <- fread("iso_country_codes.csv")
## iso <- iso[, .(alpha2_code, country)]
## iso[, origin_country := alpha2_code]
## iso[, origin_country := gsub(" ", "", origin_country)]
## origin <- varieties[, .(var, origin_country)]
## origin <- iso[origin, on = "origin_country"]
## origin[is.na(origin_country), var]
## unique(origin[, .(alpha2_code, country)])

origin <- varieties[, .(var, origin_country, mother, father)]
varnames_country <- c(
'SE' = "Sverige",
'ES' = "Spanien",
'DE' = "Tyskland",
'FR/DE' = "Frankrike eller Tyskland",
'GB' = "Storbrittannien",
'CA' = "Kanada",
'RU' = "Ryssland",
'CZ' = "Tjeckien", 
'US' = "USA",
'FR' = "Frankrike", 
'HU' = "Ungern",
'<NA>' = "Okänt"
)
origin[, origin_country := query_label(origin_country, varnames_country)]
origin[, mother := gsub("seedling from", "fröplanta från", mother)]
origin[, mother := gsub("cultivar from", "kultivar från", mother)]
origin[, mother := gsub("cv. group", "sortgrupp", mother)]
origin[, mother := gsub("local cultivar", "lokal kultivar", mother)]
origin[, mother := gsub("selected by", "selekterad av", mother)]
origin[, mother := gsub("open pollinated", "öppet pollinerad", mother)]
origin[, mother := gsub("X-rayed pollen", "bestrålat pollen", mother)]
origin[, mother := gsub("from Rhone valley", "från Rhendalen", mother)]
origin[, mother := gsub("North Bohemia", "Norra Böhmen", mother)]
origin[, mother := gsub("selection from", "selektion från", mother)]
origin[, mother := gsub("Seedling from", "Fröplanta från", mother)]
origin[, father := gsub("seedling from", "fröplanta från", father)]
origin[, father := gsub("cultivar from", "kultivar från", father)]
origin[, father := gsub("cv. group", "sortgrupp", father)]
origin[, father := gsub("local cultivar", "lokal kultivar", father)]
origin[, father := gsub("selected by", "selekterad av", father)]
origin[, father := gsub("open pollinated", "öppet pollinerad", father)]
origin[, father := gsub("X-rayed pollen", "bestrålat pollen", father)]
origin[, father := gsub("from Rhone valley", "från Rhendalen", father)]
origin[, father := gsub("North Bohemia", "Norra Böhmen", father)]
origin[, father := gsub("selection from", "selektion från", father)]
origin[, father := gsub("Seedling from", "Fröplanta från", father)]
origin[, mother := ifelse(is.na(mother)|mother == "", "?", mother)]
origin[, father := ifelse(is.na(father)|father == "", "?", father)]
origin[, mother_father := paste0(mother, " x ", father)]
origin[mother_father == "? x ?", mother_father := ""]
origin[, country_txt := ifelse(!is.na(origin_country), paste0("Ursprung: ", origin_country), "")]
origin[, country_txt := ifelse(mother_father == "okänd", country_txt, paste0(country_txt, ", ", mother_father))]
origin[, country_txt := ifelse(country_txt == ""|is.na(country_txt), "", paste0(country_txt, "."))]
origin <- origin[, .(var, country_txt)]
varieties <- origin[varieties, on = "var"]

## Type
varieties[, note_txt := ifelse(Typ == "Cerasus", paste0(note_txt, ". Surkörsbär."), note_txt)]

## sanitize
varieties[, image_path := ifelse(!is.na(image_path), paste0("![", label, "]", image_path), "")]
varieties[, note_syn := ifelse(!is.na(syn), paste0("Synonymer: ", syn), "")]
varieties[, note_txt := gsub(", NA kött", " kött", note_txt)]
varieties[, note_txt := gsub("\\.  och mognar", ". Mognar", note_txt)]
varieties[, note_txt := gsub(".NA  och mognar", ". Mognar", note_txt)]
varieties[, note_txt := gsub("och , ", "och ", note_txt)]
varieties[, note_txt := gsub(" \\.", "", note_txt)]
varieties[, note_txt := gsub(" \\.", "\\.", note_txt)]
varieties[, note_txt := gsub(", \\.", "\\.", note_txt)]
varieties[, note_txt := gsub("NA", "", note_txt)]
varieties[, note_txt := gsub("\\.\\.", "\\.", note_txt)]
varieties[, country_txt := gsub(", \\.", "\\.", country_txt)]
varieties[, country_txt := gsub("^.$", "", country_txt)]

## names(varieties)
## varieties[var == n, ]
## varieties[var == "merton_late", ]
## ?writeLines
## ?connections
## ?write

## names(varieties)
## n <- varieties$var[[1]]

tmp <- "./website/varieties_autogenerated.qmd"
file.create(tmp)
for(n in varieties$var){
    write(paste0("## ", varieties[var == n, label], "\n"), tmp, append = TRUE)
    write(paste0(varieties[var == n, note_syn], "\n"), tmp, append = TRUE)
        write(paste0(varieties[var == n, country_txt], "\n"), tmp, append = TRUE)
    write(paste0(varieties[var == n, image_path], "\n"), tmp, append = TRUE)
    write(paste0(varieties[var == n, note_txt], ""), tmp, append = TRUE)
    write(paste0("Zon ", varieties[var == n, Zon], ".\n"), tmp, append = TRUE)
    if(varieties[var == n, !is.na(pollinators)]){
    write(paste0("Pollinatörer som är genetiskt kompatibla och troligen blommar ungefär samtidigt: ", varieties[var == n, pollinators], ".\n"), tmp, append = TRUE)
    }
    write("\n", tmp, append = TRUE)
}

## Note: Klistra in varieties.qmd manuellt i choosing_variety.qmd

## ![Körsbärsfluga. Foto: Bauer Karl.](../../dropbox/images/plants/rhagoletis_cerasi2.jpg){width=50%}


## cherries_table <- fread("cherries_table.csv")
## cherries_table <- bt_wide_curated_bt[cherries_table, on = "var"] ## add blooming_group

