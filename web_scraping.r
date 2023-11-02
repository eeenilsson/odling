pacman::p_load(rvest)

link <- "https://www.blomsterlandet.se/tips-rad/vaxtinformation/tradgard/frukttrad/korsbar/"
link <- "https://www.blomsterlandet.se/tips-rad/vaxtinformation/tradgard/frukttrad/korsbar/?page=2"

page_html <- read_html(link)
test <- page_html %>% rvest::html_nodes("body")


## extract links
library(stringr)
test <- paste(test)
res <- str_match_all(test, "(?s)href[^/](.*?)(?s)div.class")
res <- res[[1]]
res_trimmed <- res[grepl("frukttrad.korsbar", res)]
## res_trimmed <- gsub("href=...", "", res_trimmed)
res_trimmed <- gsub("^[^t]*", "", res_trimmed)
res_trimmed <- res_trimmed[!grepl("div", res_trimmed)]
res_trimmed <- gsub("..><", "", res_trimmed)

base <- "https://www.blomsterlandet.se/"
res_trimmed <- paste0(base, res_trimmed, "/")

## extract info from linked pages
bloml_table <- c()
n <- 2
for(n in 2:length(res_trimmed)){
link_sub <- res_trimmed[n]
sub_html <- read_html(link_sub)
tmp <- sub_html %>% rvest::html_nodes("body")%>%paste(.)
tmp_trimmed <- gsub("^(.*?)PlantInformationBlockstyled__ProductFamilyName", "", tmp)
tmp_trimmed <- gsub("SellingPointsstyled(.*)", "", tmp_trimmed)

## extract items
pname <- str_match_all(tmp, "(?s)PlantInformationBlockstyled__ScientificName[^>]*(.*?)(?s)</p>")[[1]][2]
pinfo_short <- str_match_all(tmp, "(?s)PlantInformationBlockstyled__ShortDescription[^>]*(.*?)(?s)</div>")[[1]][2]
pinfo <- str_match_all(tmp, "(?s)ProductInformationstyled__Description[^>]*(.*?)(?s)</p>")[[1]][2]

## pcare has several items
pcare <- str_match_all(tmp, "(?s)CareInformationItemstyled__Name[^>]*(.*?)(?s)CareInformationItemstyled__Icons")[[1]][,1]
pcare_table <- c()
for(i in 1:length(pcare)){
    ## loop across pcare variables
    pcare_var <- str_match_all(pcare[i], "(?s)>(.*?)(?s)<.dt")[[1]][,2]
    pcare_value <- str_match_all(pcare[i], "(?s)CareInformationItemstyled__Value[^>]*(.*?)(?s)<.dd")[[1]][, 2]
    pcare_table <- rbind(
        pcare_table,
        data.frame(
            pcare_var = pcare_var,
            value = pcare_value
        )
    )
}
pcare_table <- rbind(
        pcare_table,
        data.frame(
            pcare_var = "description",
            value = paste0(pinfo_short, ". ", pinfo)
        )
    )
pcare_table$value <- gsub(">", "", pcare_table$value)
pcare_table$pcare_var <- tolower(pcare_table$pcare_var)
pcare_table$label <- gsub(">", "", pname)

bloml_table <- rbind(bloml_table, pcare_table)

}

bloml_table$pcare_var <- gsub(":", "", bloml_table$pcare_var)
bloml_table$pcare_var <- gsub("/", "_", bloml_table$pcare_var)
bloml_table$type <- "sweet"
bloml_table$type[grepl("Prunus cerasus", bloml_table$label)] <- "sour"
1+1

bloml_table$label_tmp <- bloml_table$label
bloml_table$label_tmp <- gsub("Prunus avium ", "", bloml_table$label_tmp)
bloml_table$label_tmp <- gsub("Prunus cerasus ", "", bloml_table$label_tmp)
bloml_table$label_tmp <- gsub(" E", "", bloml_table$label_tmp)

unique(bloml_table$label_tmp)

## str_match(bloml_table$label, "(?s)'(.*?)(?s)'")
bloml_table$var <- tolower(bloml_table$label)


store <- bloml_table

######################
test%>%
  html_elements(css = "PlantInformationBlockstyled__ScientificName")



html_elements(page_html, 'css', link)


?html_elements

page_html%>%
  html_elements(xpath = "")
