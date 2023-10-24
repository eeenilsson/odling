## Plot pollineringsschema -------

temp <- dta[intresse == 1|intresse == 2, .(var, pollinated_by_concordance_chr, pollinated_by_concat_chr)]

## create varnames for pollinators
pol <- dta$pollinated_by_concordance_chr
pol <- unlist(strsplit(pol, ", "))
pol <- gsub(" .*", "", pol)
pol <- unique(pol)
pol <- gsub("NA", "var", pol)
dt <- matrix(nrow = nrow(temp), ncol = length(pol))
colnames(dt) <- pol
dt <- data.table(dt, key = "var")
dt$var <- temp$var
temp <- temp[dt, on = "var"]

## melt
temp <- melt(temp, id.vars = c("var", "pollinated_by_concordance_chr", "pollinated_by_concat_chr"))
temp[, value := stringi::stri_detect_fixed(pollinated_by_concordance_chr, paste0(variable, " ("))]

## Calculate concordance?
myfun <- Vectorize(test_concordance_prop)
temp[, concordance := myfun(pollinated_by_concat_chr, variable)]

## select cols for plotting
toplot <- temp[, .(var, variable, value, concordance)]
names(toplot) <- c("target", "pollinator", "value", "concordance")
setkey(toplot, target)

## setkey(toplot, var)
toplot[, value:= as.numeric(value)]
toplot[, target:= as.factor(target)]
## View(toplot)
## str(toplot)

## labels
varnames <- dta$label
names(varnames) <- dta$var
toplot[, target:= factor(target, levels = levels(target), labels = unname(query_label(levels(target), varnames)))]
toplot[, pollinator:= factor(pollinator, levels = levels(pollinator), labels = unname(query_label(levels(pollinator), varnames)))]

## plot base
p <- ggplot(toplot, aes(pollinator, target)) +
  geom_point(aes(size = concordance))

## plot customization
plot_pollination_table <- p +
    scale_size_area() +
    theme(
        plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "centimeters"),
        legend.position = "none",
        axis.text.x = element_text(angle = -90, vjust = 0.5, hjust=0),
        plot.title = element_text(hjust = 0, vjust = 3, size = 32, face="bold"),
        axis.title.x = element_text(hjust = 0.5, vjust = -5),
        axis.text=element_text(size=16),
        axis.title=element_text(size=24, face="bold")
    ) +
    labs(title="Pollinationsschema för körsbär",
         x ="Pollinatör",
         y = "Mottagare")

## toplot
