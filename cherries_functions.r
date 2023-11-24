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


## sanitize var names to maybe match genotype data
sanitize_var <- function(tempvar){
tempvar <- gsub("\n", "", tempvar) ## remove newline char
tempvar <- gsub("\\.", "", tempvar) ## remove 
tempvar <- gsub(" / ", "_", tempvar) ## replace / w _
tempvar <- gsub(" - ", "", tempvar) ## replace / w _
tempvar <- gsub("\\(.*", "", tempvar) ## remove parens
## stringi foreign characters
custom_rules <- "å > aa;
                 ø > oe;
                 ::Latin-ASCII;"
tempvar <- stringi::stri_trans_general(tempvar, id = custom_rules, rules = TRUE)
## TM
tempvar <- gsub("TM$", "", tempvar)
tempvar <- gsub("TM", "", tempvar)
tempvar <- stringr::str_trim(tempvar) ## tolower
tempvar <- tolower(tempvar) ## internal ws to underscore
tempvar <- gsub(" ", "_", tempvar)

## clean
tempvar <- gsub("'", "", tempvar)
tempvar <- gsub("\\(r\\)", "", tempvar)
tempvar <- gsub("-", "_", tempvar)

return(tempvar)

}


    overlap <- function(astart, aend, bstart, bend, criteria = NULL){
        ## function to calculate overlap for eur_bt
        ## criteria eg 4 days overlap
        
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


## ## faster version /S2S4'
## genotype_a <- "S4S8"
## genotype_b <- "S4'S6"
## genotype_a <- "S4S3/S9S3"
## genotype_b <- "S1S3/S4"
## compat(genotype_a, genotype_b)
## compat("S4S8", "S1S3")
## genotype_a <- "S1S6S26S36a" ## test
## genotype_a <- "S6S13ʹS26S36a" ## skuggmorell surkörsbär
## genotype_b <- "S1S6"
## compat(genotype_a, genotype_b)

compat <- function(genotype_a, genotype_b){
    ## calculate relative compatibility
    ## If uncertain genotype (ie genotype separated with "/", the worst case scenario will be used)
    pollinator <- paste0(strsplit(genotype_a, "/")[[1]], collapse = "")
    target <- paste0(strsplit(genotype_b, "/")[[1]], collapse = "")
    pollinator <- strsplit(pollinator, "S")[[1]][-1]
    target <- strsplit(target, "S")[[1]][-1]

    ## check if terraploid (sour cherry)
    tetraploid_pol <- ifelse(!grepl("/", genotype_a) & length(pollinator) == 4, TRUE, FALSE)
    tetraploid_target <- ifelse(!grepl("/", genotype_b) & length(target) == 4, TRUE, FALSE)
    ## Note: Man har identifierat 12 fungerande S-haplotyper (S1, S4, S6, S9, S12, S13, S14, S16 ,S26, S33, S34, och S35) och nio icke-fungerande (S1ʹ, S6m, S6m2, S13ʹ, S13m, S36a , S36b, S36b2, och S36b3). Vissa finns även hos sörkörsbär (S1, S4, S6, S9, S12, S13, S 14, S16, och S34). The presence of two or more non-functional S-haplotypes within sour cherry 2x pollen renders that pollen SC.
    mutants <- c("ʹ$|'$|^6m$|^6m2$|^13m$|^36a$|^36b$|^36b2$|^36b3$")
    target <- target[!grepl(mutants, target)] ## remove mutated S-alleles (does not confer GSI)
    check <- !grepl(paste0("^", paste0(target, collapse = "$|^"), "$"), pollinator)
    sucess <- pmax(check, grepl("ʹ$|'$", pollinator)) ## count as success if "'"
    success_alt1 <- sucess[c(TRUE, TRUE, FALSE, FALSE)]
    success_alt2 <- sucess[!c(TRUE, TRUE, FALSE, FALSE)] ## NA if length 2
    comp <- sum(success_alt1, na.rm = TRUE)
    if(grepl("/", genotype_a)){
        ## if uncertain genotype (/) use worst case
        comp <- min(sum(success_alt1, na.rm = TRUE), sum(success_alt2, na.rm = TRUE), na.rm = TRUE)
    }
    if(tetraploid_pol){
        ## mutants <- c("'", "6m", "6m2", "13m", "36a", "36b", "36b2", "36b3")
        p_mut <- pollinator[grepl(mutants, pollinator)]
        p_nonmut <- pollinator[!grepl(mutants, pollinator)]
        check_nonmut <- !grepl(paste0("^", paste0(target, collapse = "$|^"), "$"), p_nonmut)
        p_nonmut_compat <- p_nonmut[check_nonmut]
        p_compat <- c(p_mut, p_nonmut_compat) ## mut or absent in target
        success_probability <- 0
        if(length(p_compat) == 2){success_probability <- 2/8}
        if(length(p_compat) == 3){success_probability <- 4/8}
        if(length(p_compat) == 4){success_probability <- 8/8}
        ## abcd => ab ac ad bc bd cd but this is not the case in meiosis, see youtube:
        ## see https://www.youtube.com/watch?v=NttqS-N17FQ
        ## possible number of gametes = 2^n        
        ## where n is the haploid number (2n=4 => diploid nr=4, haploid nr=2)
        ##  sour cherry is an allotetraploid (2n = 4x = 32)
        ## for a tetraploid, n=2x, and 2n=4x
        ## if there is 1 mutant the probability of a sperm to contain 2 mutants is 0.
        ## 2 mutants => probability of pollen to contain 2 mutants = 2/8, 3 m => 4/8, 4m => 8/8
        comp <- ifelse(success_probability >0, ceiling(success_probability+0.1), 0)
    }

    return(comp)        
    ## Undantaget är S4**’**-allelen (notera apostrofen) som medför _självfertilitet_. Ett pollenkorn med S4**’** kan befrukta alla mottagare (inklusive de med S4**’**). Körsbär med S4**’** kan således betraktas som universella givare.
    ## De enstaka själv-**in**fertila sorter som har S4**’** ej kan befruktas av S4 (utan apostrof).
## Enstaka universella givare saknar också S4**’**.
    ## Enstaka universella givare saknar också S4**’**.
    ## S3' =  SC
    ## S5' =  SC
}
