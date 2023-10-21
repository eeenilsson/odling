replace_name <- function(x, varnames, exact = FALSE){
    ## replace text matching varnames with names(varnames)
    out <- x
    ## i <- 17
for(i in 1:length(varnames)){
    if(exact){matchthis <- paste0("^", varnames[i], "$", "|,", varnames[i], "$", "|,", varnames[i], ",", "|^", varnames[i], ",")}else{matchthis <- varnames[i]}

    out <- gsub(matchthis, paste0(",", names(varnames)[i], ","), out)

## gsub(matchthis, paste0(names(varnames)[i], ","), out)
    
}
    
    ## sanitize
    out <- stringr::str_squish(out)
    out <- gsub("^,", "", out) ## remove leading comma
    out <- gsub(",$", "", out) ## remove trailing comma
    out <- gsub("(,)\\1+", "\\1", out, perl=TRUE) ## remove repeated ,,
    out <- gsub(", ,", ",", out)

    return(out)
}

## out <- "1, ,5, ,11, ,21, ,23, ,24, ,29"
## gsub(", ,", ",", out)

test_concordance <- function(item){
    ## calculate concordance proportion
    ## item is a character vector with ";" as separator between sources 
    ## Returns a character vector with var (prop)
    totest <- unique(strsplit(item, ",|;")[[1]])
    out <- c()
    for(i in totest){
        pollinated_cols <- strsplit(item, ";")[[1]]
        ## i <- totest[1]
        ## i <- "NA"
        hasmatch <- sapply(pollinated_cols, function(x){sum(i == strsplit(x, ",")[[1]])>0})
        if(i == "NA"){
            concordant_prop <- sum(hasmatch) / length(names(hasmatch))
        }else{
            concordant_prop <- sum(hasmatch) / sum(names(hasmatch)!="NA")
        }
    concordant_perc <- signif(concordant_prop*100, digits = 2)
    concordant_txt <- paste0(i, " (", concordant_perc, "%)")
    out <- c(out, concordant_txt)
}
return(out)
}


## item <- "allm_gulrod,buttners_rote,hedelfinger,merton_glory,stella;allm_gulrod,almore,buttners_rote,hedelfinger,heidi,merton_glory,stella,stor_svart,van;allm_gulrod,buttners_rote,hedelfinger,van;allm_gulrod,almore,fryksas,gardebo,heidi,merton_glory,ostheimer"
## item <- "allm_gulrod;van"
## filter <- "buttners_rote"
## i <- "allm_gulrod"

test_concordance_prop <- function(item, filter = NULL){
    ## calculate concordance proportion for filter only if specified
    ## item is a character vector with ";" as separator between sources 
    ## Returns numeric proportion
    totest <- unique(strsplit(item, ",|;")[[1]])
    out <- c()

    concordant_table <- data.frame()
    for(i in totest){
        pollinated_cols <- strsplit(item, ";")[[1]]
        ## i <- totest[1]
        ## i <- "NA"
        hasmatch <- sapply(pollinated_cols, function(x){sum(i == strsplit(x, ",")[[1]])>0})
        if(i == "NA"){
            concordant_prop <- sum(hasmatch) / length(names(hasmatch))
        }else{
            concordant_prop <- sum(hasmatch) / sum(names(hasmatch)!="NA")
        }
        concordant_table <- rbind(concordant_table, data.frame(var = i, prop = concordant_prop)) 
        ## concordant_perc <- signif(concordant_prop*100, digits = 2)
        ## concordant_txt <- paste0(i, " (", concordant_perc, "%)")
        ## out <- c(out, concordant_txt)
        if(is.null(filter)){
            out <- concordant_table
        }else{
            out <- concordant_table[concordant_table$var == filter, ][["prop"]]
            if(length(out) == 0){out <- 0}
        }
        
    }
    return(out)
}

