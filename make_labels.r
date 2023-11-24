## varnames objects created here

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

## for site
varnames3 <- tmp$label
names(varnames3) <- tmp$var

varnames2 <- gsub("|\\^\\[[^$]*", "", varnames) ## remove part in brackets for pollinators
varnames2 <- gsub(" $", "", varnames2)
varnames2 <- gsub("\\[.*", "", varnames2)
## varnames2 <- gsub("\\*", "", varnames2) ## remove asterisk
