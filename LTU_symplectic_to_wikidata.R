# Setup -----
remove.packages("WikidataR", lib="~/R/win-library/3.6")
devtools::install_local("C:\\Users\\thoma\\OneDrive\\1-Scripts\\GitHub\\WikidataR",force=TRUE)
library(WikidataR)
library(rorcid)
library(scholar)
library(tibble)
library(progress)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(stringr)
library(crayon)

# Specific Functions ---------------------

build_positiontable <- function(data){

  positiontable <- tibble(position=names(sort(table(datafull$Position[datafull$College!="Central" & !is.na(datafull$ORCID)]),decreasing = T)),
                         qid=NA)

  for(pos in 1:nrow(positiontable)){
    qids <- find_item(positiontable$position[pos])
    message("\014",
            "---------------------------------------------------------------------------",
            "\nLets disambiguate the position ",bold$white(positiontable$position[pos]),
            "\n---------------------------------------------------------------------------",
            "\nTo accept the presented match, enter y",
            "\nTo reject the presented match and request alternatives, enter n",
            "\nTo provide an alternative QID, enter that that QID",
            "\nTo search for an alternative term, enter that term",
            "\n---------------------------------------------------------------------------")
    if(length(qids>0)){
      filter_qids(unlist(lapply(find_item(positiontable$position[pos]),function(x) x$id)))
    }else{
      message("There's no good wikidata match for ",bold$white(data$Expertise_split[[person]][[exp]]),
              ".\nWould you like to note it for creation later [y/n]?")
      suppressWarnings(invisible(selection <- readline()))
      if(selection=="y"|selection==1){
        correctqid <- NULL
        tocreate   <- append(tocreate,positiontable$position[pos])
      }else if(selection=="n"|selection=="0"|selection==""){
        correctqid <- NULL
      }else{
        altqids <- unlist(lapply(find_item(selection),function(x) x$id))
        message("Searching for ",bold$white(selection)," as an alternative term")
        expertise.filtered <- filter_qids(altqids)
        message("Are any of these appropriate? (Type the row number or an alternative QID followed by [enter])")
        print(data.frame(expertise.filtered))
        suppressWarnings(invisible(selection <- readline()))
        if(selection %in% 1:nrow(expertise.filtered)){
          correctqid <- as.character(expertise.filtered[selection,1])
        }else{
          correctqid <- NULL
        }
      }
    }
  }
  return(positiontable)
}

ORCID_from_names <- function(data = data){
  ORCIDs_certain <- sum(!is.na(data$ORCID))/nrow(data)
  message("Finding additional ORCiDs based on names and affiliations")
  data$ORCID_estimate<-NA
  pb <- progress_bar$new(total = sum(data$College!="Central" & is.na(data$ORCID), na.rm = 1),
                         format = "[:bar] :percent eta::eta")
  for (i in which(data$College!="Central" & is.na(data$ORCID))){
    pb$tick()
    ORCID.guess <- orcid_search (given_name  = gsub(pattern = ".*, ([^ ]*).*",replacement = "\\1 ",data$Name[i]),
                                 family_name = gsub(pattern = "([^ ]*),.*",replacement = "\\1 ",data$Name[i]),
                                 current_inst = 'Trobe')
    if(length(ORCID.guess)==0){
      ORCID.guess<-NA
    }else if(length(ORCID.guess)>1){
      ORCID.guess<-tibble(ORCID_estimates=ORCID.guess$ORCID)
    }
    data$ORCID_estimate[i] <- ORCID.guess
  }
  ORCIDs_certain <- sum(!is.na(data$ORCID))/nrow(data)
  ORCIDs_estimate <- (sum(!is.na(data$ORCID_estimate))+sum(!is.na(data$ORCID)))/nrow(data) - ORCIDs_certain
  message(100*round(ORCIDs_certain,3),"% of the ",nrow(data)," rows had an ORCiD. I've found ORCiDs for an additional ",
          100*round(ORCIDs_estimate,3),"% based on name and affiliation, leaving ",
          100*round(1-ORCIDs_certain-ORCIDs_estimate,3),"% unaccounted for.")
  return(data)
}

scholar_from_names <- function(data = data){
  if(suppressWarnings(is.null(data$gsc))){data$gsc <- NA}
  gsc_certain <- sum(!is.na(data$gsc))/nrow(data)
  message("Finding additional Google Scholar profiles based on names and affiliations")
  data$gsc_estimate<-NA
  pb <- progress_bar$new(total = sum(data$College!="Central" & is.na(data$gsc), na.rm = 1),
                         format = "[:bar] :percent eta::eta")
  for (i in which(data$College!="Central" & is.na(data$gsc))){
    pb$tick()
    gsc.guess <-   suppressMessages(get_scholar_id(last_name = trimws(gsub(pattern = "([^ ]*),.*",replacement = "\\1 ",data$Name[i])),
                                                   first_name = trimws(gsub(pattern = ".*, ([^ ]*).*",replacement = "\\1 ",data$Name[i])),
                                                   affiliation = "Trobe"))

    if(length(gsc.guess)==0){
      gsc.guess<-NA
    }else if(length(gsc.guess)>1){
      gsc.guess<-tibble(gsc_estimates=gsc.guess$gsc)
    }
    data$gsc_estimate[i] <- gsc.guess
  }
  gsc_certain <- sum(!is.na(data$gsc))/nrow(data)
  gsc_estimate <- (sum(!is.na(data$gsc_estimate))+sum(!is.na(data$gsc)))/nrow(data) - gsc_certain
  message(100*round(gsc_certain,3),"% of the ",nrow(data)," rows had a GSc profile I've found ORCiDs for an additional ",
          100*round(gsc_estimate,3),"% based on name and affiliation, leaving ",
          100*round(1-gsc_certain - gsc_estimate,3),"% unaccounted for.")
  return(data)
}

disambiguate_ORCiDs <- function(data = data){
  ORCIDs_certain  <- sum(!is.na(data$ORCID))/nrow(data)
  ORCIDs_estimate <- (sum(!is.na(data$ORCID_estimate))+sum(!is.na(data$ORCID)))/nrow(data) - ORCIDs_certain
  message("Assuming ORCiDs found are correct if only a single result found with the matching name and affiliation.")
  unambiguous_ORCIDs <- !is.na(data$ORCID_estimate) & sapply(data$ORCID_estimate,length)==1
  data$ORCID[unambiguous_ORCIDs] <- data$ORCID_estimate[unambiguous_ORCIDs]
  if(sum(sapply(data$ORCID_estimate,length)>1 & is.na(data$ORCID))>0){
    message("Select the most appropriate ORCiD from the options listed (or leave blank if none look correct)")
    for(i in which(sapply(data$ORCID_estimate,length)>1 & is.na(data$ORCID))){
      print(data.frame(ORCID_estimates=unlist(data$ORCID_estimate[i])))
      sapply(data$ORCID_estimate[i][[1]],function(x) browseURL(paste0("https://ORCID.org/",x)))
      message("Some webpages should have just opened. Select an option then press [enter]
              To select the correct ORCiD, just enter the number
              To select none of the suggested ORCiDs, leave blank
              To stop for now, type stop")
      suppressWarnings(invisible(selection <- readline()))
      if(selection=="stop"){
        break
      }else if(selection %in% 1:length(unlist(data$ORCID_estimate[i]))){
        ORCID.selected <- data$ORCID_estimate[i][[1]][as.numeric(selection)]
        message("-> ", ORCID.selected," selected")
        data$ORCID[i]<- ORCID.selected
      }else{
        message("-> none selected")
      }
    }
  }else{
    message("All found ORCiDs unambiguous")
  }
  return(data)
}


ORCiD_pub_number <- function(data = data, plot=TRUE){
  message("Finding whether ORCIDs profiles have publications listed (as a proxy for open data accounts)")
  data$ORCID_n<-NA
  pb <- progress_bar$new(total = sum(!is.na(data$ORCID)),
                         format = "[:bar] :percent eta::eta")
  for (i in which(!is.na(data$ORCID))){
    pb$tick()
    data$ORCID_n[i] <- suppressMessages(length(orcid_works(data$ORCID[i])[[1]]$works))
  }
  if(plot){hist(data$ORCID_n,breaks = 20,main = "Publications per ORCiD account")}
  message(100*round(sum(data$ORCID_n>0,na.rm = 1)/sum(!is.na(data$ORCID)),3),
          "% of those with ORCiDs have publications associated with them.")
  return(data)
}


QIDs_from_ORCiDs <- function(data = data){
  message("Finding QIDs based on ORCiDs")
  if(suppressWarnings(is.null(data$QID))){data$QID<-NA}
  rows.without.qids <- !is.qid(data$QID)
  rows.with.orcids  <- !is.na(data$ORCID)
  rows              <- which(rows.without.qids & rows.with.orcids)
  pb <- progress_bar$new(total = length(rows),
                         format = "[:bar] :percent eta::eta")
  for (i in rows){
    pb$tick()
    newqid <- suppressMessages(qid_from_ORCID(ORCID = data$ORCID[i]))
    if(is.na(newqid)){
      data$QID[i] <- newqid
    }else{
      data$QID[i] <- as.character(newqid)
    }
  }
  data$QID[data$QID=="logical(0)"]<-NA
  qids_certain <- sum(is.qid(data$QID))/nrow(data)
  message(100*round(qids_certain,3),"% of the ",nrow(data)," rows have a QID")
  return(data)
}


QIDs_from_ScopusIDs <- function(data = data){
  message("Finding QIDs based on ScopusIDs")
  if(suppressWarnings(is.null(data$QID))){data$QID<-NA}
  rows.without.qids <- !is.qid(data$QID)
  rows.with.scopus  <- !is.na(data$ScopusID)
  rows              <- which(rows.without.qids & rows.with.scopus)
  pb <- progress_bar$new(total = length(rows),
                         format = "[:bar] :percent eta::eta")
  for (i in rows){
    pb$tick()
    newqid <- suppressMessages(qid_from_identifier("P1153",value=data$ScopusID[i]))
    if(is.na(newqid)){
      data$QID[i] <- newqid
    }else{
      data$QID[i] <- as.character(newqid)
    }
  }
  data$QID[data$QID=="logical(0)"]<-NA
  qids_certain <- sum(is.qid(data$QID))/nrow(data)
  message(100*round(qids_certain,3),"% of the ",nrow(data)," rows have a QID")
  return(data)
}


QIDs_from_ResearcherIDs <- function(data = data){
  message("Finding QIDs based on ResearcherIDs")
  if(suppressWarnings(is.null(data$QID))){data$QID<-NA}
  rows.without.qids      <- !is.qid(data$QID)
  rows.with.researcherid <- !is.na(data$ResearcherID)
  rows                   <- which(rows.without.qids & rows.with.researcherid)
  pb <- progress_bar$new(total = length(rows),
                         format = "[:bar] :percent eta::eta")
  for (i in rows){
    pb$tick()
    newqid <- suppressMessages(qid_from_identifier("P1053",value=data$ScopusID[i]))
    if(is.na(newqid)){
      data$QID[i] <- newqid
    }else{
      data$QID[i] <- as.character(newqid)
    }
  }
  data$QID[data$QID=="logical(0)"]<-NA
  qids_certain <- sum(is.qid(data$QID))/nrow(data)
  message(100*round(qids_certain,3),"% of the ",nrow(data)," rows have a QID")
  return(data)
}


QIDs_from_names <- function(data = data, plot=TRUE){
  message("Finding additional possible QIDs based on names")
  data$QID_estimate <- NA
  data$QID_estimate_desc <- NA
  pb <- progress_bar$new(total = sum(!is.qid(data$QID)),
                         format = "[:bar] :percent eta::eta")
  for (i in which(!is.qid(data$QID))){
    pb$tick()
    qid.guess <- suppressMessages(qid_from_name(gsub(pattern = "(.*), (.*)",replacement = "\\2 \\1",data$Name[i])))
    if(is.null(qid.guess)){
      qid.guess      <- NA
      qid.guess.desc <- NA
    }else if(length(qid.guess)==1){
      qid.guess.desc <- get_item(qid.guess)[[1]]$descriptions$en$value
    }else if(length(qid.guess)>1){
      qid.guess      <- tibble(qid.guess)
      qid.guess.desc <- tibble(unlist(lapply(get_item(qid.guess[[1]]),function(x){if(!is.null(x$descriptions$en$value)){x$descriptions$en$value}else{NA}})))
    }

    data$QID_estimate[i] <- qid.guess
    data$QID_estimate_desc[i] <- qid.guess.desc
  }
  data$QID[data$QID_estimate=="logical(0)"]<-NA
  if(plot){hist(sapply(data$QID_estimate,length),main="Name matches per unknown QID")}
  qids_certain  <- sum(is.qid(data$QID))/nrow(data)
  qids_estimate <- (sum(is.qid(data$QID))+sum(!is.na(data$QID_estimate)))/nrow(data) - qids_certain
  message(100*round(qids_certain,3),"% of the ",nrow(data)," rows have a QID. I've taken a guess at an additional ",
          100*round(qids_estimate,3),"% based only on matching names, leaving ",
          100*round(1-qids_certain-qids_estimate,3),"% unaccounted for.")
  return(data)
}

expertiseQIDs_from_expertise <- function(data){
  if(all(class(data)=="list")){
    data <- data$data
  }
  message("Gathering possible QIDs for expertise and the description for most likely correct QID")
  pb <- progress_bar$new(total  = nrow(data),
                         format = "[:bar] eta::eta")
  newcols <- NULL
  for (i in 1:nrow(data)){
    if(is.na(data$`Expertise`[i])){
      newcols <- bind_rows(newcols,tibble(split=NA,qid=NA,desc=NA))
    }else{
      expertise_split     <- trimws(unlist(str_split(data$`Expertise`[i],pattern = "[;]|and")))
      expertise.qids      <- lapply(expertise_split,function(x) if(x!=""){qid_from_name(x[[1]])}else{NA})
      expertise.qids_desc <- lapply(expertise.qids,function(x){if(all(is.na(x))){
                                                                 tibble(qid=NA,
                                                                        label=NA,
                                                                        desc="No current matching Wikidata item")
                                                               }else{
                                                                 filter_qids(x[1])
                                                               }})
      newcols <- bind_rows(newcols,tibble(split=list(expertise_split),
                                          qid=list(expertise.qids),
                                          desc=list(expertise.qids_desc)))
    }
    pb$tick()
  }
  data <- mutate(data,
                 "Expertise_split"=newcols$split,
                 "Expertise.QIDs_estimate"=newcols$qid,
                 "Expertise.QIDs_desc"=newcols$desc)
  return(data)
}


disambiguate_expertise <- function(data,limit=10){
  if(all(class(data)=="list")){
    data <- data$data
  }
  rows <- which(!is.na(data$`Expertise`))
  data$expertise.qids.correct <- NA
  tocreate      <- NULL
  for(person in rows){
    expertiseqids <- NULL
    correctqid    <- NULL
    for(exp in 1:length(data$Expertise.QIDs_desc[[person]])){
      message("\014",
              "---------------------------------------------------------------------------",
              "\nLets disambiguate the expertise of ",bold$cyan(data$Name[[person]]),":\n",
              gsub(data$Expertise_split[[person]][[exp]],
                   bold$white$underline(data$Expertise_split[[person]][[exp]]),
                   data$Expertise[[person]]),
              "\n---------------------------------------------------------------------------",
              "\nTo accept the presented match, enter y",
              "\nTo reject the presented match and request alternatives, enter n",
              "\nTo provide an alternative QID, enter that that QID",
              "\nTo search for an alternative term, enter that term",
              "\n---------------------------------------------------------------------------")
      if(is.na(data$Expertise.QIDs_desc[[person]][[exp]]$qid)){
        message("There's no good wikidata match for ",bold$white(data$Expertise_split[[person]][[exp]]),
                ".\nWould you like to note it for creation later [y/n]?")
        suppressWarnings(invisible(selection <- readline()))
        if(selection=="y"|selection==1){
          correctqid <- NULL
          tocreate   <- append(tocreate,data$Expertise_split[[person]][[exp]])
        }else if(selection=="n"|selection=="0"|selection==""){
          correctqid <- NULL
        }else{
          message("Searching for ",bold$white(selection)," as an alternative term")
          altqids <- unlist(lapply(find_item(selection),function(x) x$id))
          expertise.filtered <- filter_qids(altqids)
          message("Are any of these appropriate? (Type the row number or an alternative QID followed by [enter])")
          print(data.frame(expertise.filtered))
          suppressWarnings(invisible(selection <- readline()))
          if(selection %in% 1:nrow(expertise.filtered)){
            correctqid <- as.character(expertise.filtered[selection,1])
          }else{
            correctqid <- NULL
          }
        }
      }else{
        message("The first search hit on wikidata is often correct, so we'll look at those first and only ceck alternatives if it's not quite right.\n",
                white(data$Expertise.QIDs_desc[[person]][[exp]]$desc))
        suppressWarnings(invisible(selection <- readline()))
        if(selection=="stop"){
          done_so_far <- sum(!is.na(data$expertise.qids.correct))
          message("Stopping. You've completed ",
                  bold$white(done_so_far),
                  " lists so far (",
                  bold$white(sum(!is.na(data$Expertise) & is.na(data$expertise.qids.correct))),
                  " remaining). This has been saved as 'done_so_far'."
                  ,"\nTo restart from where you left off, use the 'done_so_far' parameter for this function")
          return(list(data=data,done_so_far=done_so_far))
        }else if(selection=="y"|selection==1){
          correctqid <- data$Expertise.QIDs_estimate[[person]][[exp]][[1]]
        }else if(selection=="n"|selection=="0"){
          correctqid
          message("I'm looking up a few possible alternatives")
          expertise.filtered   <- filter_qids(head(data$Expertise.QIDs_estimate[[person]][[exp]],limit))
          message("Are any of these appropriate? (Type the row number or an alternative QID followed by [enter])")
          print(data.frame(expertise.filtered))
          suppressWarnings(invisible(selection <- readline()))
          if(selection %in% 1:nrow(expertise.filtered)){
            correctqid <- as.character(expertise.filtered[selection,1])
          }else{
            correctqid <- NULL
          }
        }else if(grepl("^Q[0-9]*$",selection)){
          correctqid <- selection
        }else{
          message("Searching for ",bold$white(selection)," as an alternative term")
          altqids <- unlist(lapply(find_item(selection),function(x) x$id))
          expertise.filtered <- filter_qids(altqids)
          message("Are any of these appropriate? (Type the row number or an alternative QID followed by [enter])")
          print(data.frame(expertise.filtered))
          suppressWarnings(invisible(selection <- readline()))
          if(selection %in% 1:nrow(expertise.filtered)){
            correctqid <- as.character(expertise.filtered[selection,1])
          }else{
            correctqid <- NULL
          }
        }
        expertiseqids <- append(expertiseqids,correctqid)
      }
      data$expertise.qids.correct[person] <- tibble(expertiseqids)
    }
  }
  if(is.null(tocreate)){
    return(data)
  }
  return(list(data=data,
              tocreate=tocreate))
}

capwords <- function(s, strict = FALSE) {
  cap <- function(s) paste(toupper(substring(s, 1, 1)),
                           {s <- substring(s, 2); if(strict) tolower(s) else s},
                           sep = "", collapse = " " )
  sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

# > expertise and keywords -----
#pull keywords from ORCID and Gscholar based on wikidata item
prep_keywords <- function(QID){
  out <- NULL
  pb <- progress_bar$new(total = length(QID),
                         format = "[:bar] :percent eta::eta")
  for(i in 1:length(QID)){
    pb$tick()
    q_item <- get_item(QID[i])
    name   <- q_item[[1]]$labels$en$value
    date   <- paste0("+",Sys.Date(),"T00:00:00Z%2F11")
    
    has.orc <- all(!is.na(extract_claims(q_item,'P496')[[1]][[1]]))
    has.gsc <- all(!is.na(extract_claims(q_item,'P1960')[[1]][[1]]))
    
    orc     <- if(has.orc){extract_claims(q_item,'P496')[[1]][[1]]$mainsnak$datavalue$value}
    url.orc <- if(has.orc){URLencode(paste0("https://orcid.org/",orc),reserved = TRUE)}
    key.orc <- if(has.orc){key.orc=unlist(str_split(orcid_keywords(orc)[[1]]$keyword$content,"[,;] *"))}  
    
    gsc     <- if(has.gsc){extract_claims(q_item,'P1960')[[1]][[1]]$mainsnak$datavalue$value}
    url.gsc <- if(has.gsc){URLencode(paste0("https://scholar.google.co.uk/citations?hl=en&user=",gsc),reserved = TRUE)}
    key.gsc <- if(has.gsc){key.gsc=get_profile(gsc)$fields}
    
    row <- tibble(name=name,qid=QID[i],key.orc=list(key.orc),key.gsc=list(key.gsc),orc,gsc,url.orc,url.gsc,date)
    out <- bind_rows(out,row)
  }
  return(out)
}

# Identify QIDs for fields (where possible)
global_local_disambiguate <- function(tibble,names){

  global_disambiguate <- function(tibble){
    key.all         <- sort(unique(unlist(tibble)))
    key.all.qid     <- disambiguate_QIDs(key.all,variablename = "keyword")
    renames         <- sapply(key.all.qid,function(x){if(!is.null(names(x))){names(x)}else{NA}})
    renames[renames==key.all] <- NA
    key.all.qid.tib <- tibble(QID=unlist(key.all.qid),
                              stated.as=key.all,
                              renames=renames)
    out <- rapply(tibble,function(x){key.all.qid[x]},how="replace")
    colnames(out) <- paste0(gsub(".*?\\$([$]*)","",colnames(tibble)),".qid")
    return(out)
  }
  local_disambiguate <- function(keyqidcolumn,namecolumn){
    ambiguous <- rapply(keyqidcolumn[[1]],function(x){if(is.na(x)){names(x)}},how = "replace")
    ambiguous <- lapply(ambiguous,function(x){Filter(Negate(is.null), x)})
    names(ambiguous) <- sapply(keyqidcolumn[[1]],function(x){if(!is.null(x)){if(length(x)!=0){paste(names(x),collapse = ", ")}}})
    output <- NULL
    for (i in 1:length(keyqidcolumn[[1]])){
      if (length(ambiguous[i][[1]])!=0){
        disambiguated <- disambiguate_QIDs(ambiguous[i][!(sapply(ambiguous[i],is.null))],variablename = paste0("ambiguous item in ",white(namecolumn[i]),"'s list"))[[1]]
        working <- unlist(keyqidcolumn[[1]][[i]])
        working[sapply(working,is.na)]<-disambiguated
        output[[i]] <- unlist(working)
      }else{
        output[[i]] <- unlist(keyqidcolumn[[1]][[i]])
      }
    }
    return(output)
  }
  
  out <- global_disambiguate(tibble)
  for(i in 1:ncol(out)){
    working <- local_disambiguate(out[,i],names)
    if(length(working)>0){
      out[1:length(working),i] <- tibble(working) 
    }
  }
  return(out)
}

# Full data load -----
# Note: "2021.06.01_UserData_Thomas.xlsx" is the result of "SQL_qery.sql"
datafull <- read_excel("C:/Users/thoma/OneDrive/1-Scripts/GitHub/wikiR test scripts/2021.06.01_UserData_Thomas.xlsx")
datafull$`Leave Date`[datafull$`Leave Date`==as.POSIXct("9999-12-31","UTC")]<-NA
datafull$Name_forward <- capwords(gsub(pattern = "(.*), (.*)",replacement = "\\2 \\1",datafull$Name),strict=TRUE)
datafull$Name_noinit  <- capwords(gsub(pattern = "(.*), ([^ ]*).*",replacement = "\\2 \\1",datafull$Name),strict=TRUE)
researchers <- (!is.na(datafull$ScopusID) |
                  !is.na(datafull$ORCID) |
                  grepl("PROF",datafull$Classification) |
                  grepl("LEC",datafull$Classification) |
                  grepl("RES",datafull$Classification) |
                  grepl("FELL",datafull$Classification) |
                  grepl("AP",datafull$Classification) |
                  grepl("ADJ",datafull$Classification))
datafull$Description  <- ""
datafull$Affiliation  <- ""
datafull$Description[researchers & datafull$School=="Other"] <- "researcher (PhD)"
datafull$Description[researchers & datafull$School!="Other"] <- paste0("researcher (",str_replace(datafull$Department,"School of ",""),")")[researchers & datafull$School!="Other"]
datafull$Affiliation[researchers & datafull$School!="Other"] <- paste0(str_replace_na(datafull$Department,""),
                                                                       sapply(datafull$School,function(x) if(is.na(x)){""}else{paste(",",x)}))[researchers & datafull$School!="Other"]
datafull$Expertise_split <- str_split(datafull$Expertise,"; *")
datafull$Expertise_split[is.na(datafull$Expertise_split)] <- list(rep(NULL,length(is.na(datafull$Expertise_split))))

#users requesting omission
omission.ids <- c(15564,9934,15569,17743,5575,6663,12982,16024,10532,16327,7468,10973,12910,527,15574,15566,13232)
datafull <- datafull[!datafull$`User ID` %in% omission.ids,]
saveRDS(datafull,paste0("C:\\Users\\thoma\\OneDrive\\1-Scripts\\GitHub\\wikiR test scripts\\LTU_all_",format(Sys.time(), "%Y-%m-%d"),".RDS"))

retrieval.date <- "2021-12-09"

# > positions ------
# First get options for position titles (Those with ORCIDs and in either SHE or ASSC)
positionoptions     <- names(sort(table(datafull[researchers,]$Position[datafull[researchers,]$College!="Central" & !is.na(datafull[researchers,]$ORCID)]),decreasing = TRUE))
positionoptions.qid <- disambiguate_QIDs(positionoptions)
positionoptions.qid <- data.frame(cbind(position=positionoptions,
                                        qid=positionoptions.qid))
positionoptions.qid <- positionoptions.qid[order(unlist(positionoptions.qid$position)),]

#As backup, get options for classification
classoptions <- names(sort(table(datafull[researchers,]$Classification),decreasing = TRUE))
classoptions.qid <- disambiguate_QIDs(classoptions)
classoptions.qid <- data.frame(cbind(class=classoptions,
                                     qid=classoptions.qid))
classoptions.qid <- classoptions.qid[order(unlist(classoptions.qid$class)),]

saveRDS(classoptions.qid,"C:\\Users\\thoma\\OneDrive\\1-Scripts\\GitHub\\LaTrobot\\classoptions.qid.RDS")
saveRDS(positionoptions.qid,"C:\\Users\\thoma\\OneDrive\\1-Scripts\\GitHub\\LaTrobot\\positionoptions.qid.RDS")
datafull$classoptions.qid <- classoptions.qid[datafull$Classification,2]
datafull$positionoptions.qid <- positionoptions.qid[datafull$Position,2]
# where positionqid is NULL, fall back on classqid
datafull$position.or.class.qid <- datafull$positionoptions.qid
datafull$position.or.class.qid[as.character(datafull$positionoptions.qid)=="NULL"] <- datafull$classoptions.qid[as.character(datafull$positionoptions.qid)=="NULL"]

# Data Analysis Pipeline -----
message(round(sum(!is.na(datafull$ORCID[researchers]))/sum(researchers)*100,1),"% ORCIDS\n",
        round(sum(!is.na(datafull$ScopusID[researchers]))/sum(researchers)*100,1),"% ScopusID\n",
        round(sum(!is.na(datafull$ResearcherID[researchers]))/sum(researchers)*100,1),"% ResearcherID")

data <- datafull[researchers,][101:5719,]
data <- ORCiDs_from_names(data)
data <- scholar_from_names(data)

# stepwise <- NULL
for (i in seq(36, nrow(data), 5)){
  stepwise <- bind_rows(stepwise,
                        scholar_from_names(data[i:(i+4),]))
  Sys.sleep(2400)
}

data <- disambiguate_ORCiDs(data)
data <- ORCiD_pub_number(data)
data <- QIDs_from_ORCiDs(data)
data <- QIDs_from_ScopusIDs(data)
data <- QIDs_from_ResearcherIDs(data)

set_to_check <- !(is.qid(data$QID))                     # variant version to find items just created
set_to_check <- !(is.qid(data$QID)|is.create(data$QID)) # standard version
data$QID[set_to_check] <- disambiguate_QIDs(data$Name_noinit[set_to_check],
                                            variablename = "identity",
                                            variableinfo = paste0(data$Position[set_to_check]," ",
                                                                  data$Department[set_to_check]," (",
                                                                  data$`Arrive Date`[set_to_check]," to ",
                                                                  data$`Leave Date`[set_to_check],") ",
                                                                  data$Expertise[set_to_check],":\n",
                                                                  str_trunc(data$Overview[set_to_check],200)),
                                            Q_min = 110173000,
                                            limit=100
)

saveRDS(data,paste0("C:\\Users\\thoma\\OneDrive\\1-Scripts\\GitHub\\wikiR test scripts\\LTU_prepared_",format(Sys.time(), "%Y-%m-%d"),".RDS"))

# > expertise ------
# supplement from gscholar and orcid
keywords <- bind_cols(prep_keywords(data$QID),
                      data$Expertise_split)
temp<- unlist(lapply(keywords$key.orc,length))
temp[temp==0]<-NA
hist(temp,main = "number of orcid expertise listed",breaks = max(temp,na.rm = 1),xlim = c(0,1+max(temp,na.rm = 1)))

keyword.qids <- bind_cols(keywords$name,
                          global_local_disambiguate(tibble(keywords$key.orc,keywords$key.gsc),
                                                    keywords$name))

statedas <- c(unlist(keywords$key.orc),
              unlist(keywords$key.gsc),
              unlist(data$Expertise_split))
statedas[-grep(pattern = "->",names(c(unlist(keyword.qids$key.orc.qid),
                                      unlist(keyword.qids$key.gsc.qid),
                                      unlist(keyword.qids$Expertise_split.qid))))] <- NA

expertise <- bind_cols(qid=unlist(c(unnest(data,key.orc)$QID,
                                    unnest(data,key.gsc)$QID,
                                    unnest(data,Expertise_split)$QID)),
                       keyword=c(unlist(keywords$key.orc),
                                 unlist(keywords$key.gsc),
                                 unlist(data$Expertise_split)),
                       keyword.qid=c(unlist(keyword.qids$key.orc.qid),
                                     unlist(keyword.qids$key.gsc.qid),
                                     unlist(keyword.qids$Expertise_split.qid)),
                       statedas=statedas,
                       ref.prop=c(rep("S854",length(c(unlist(keywords$key.orc),unlist(keywords$key.gsc)))),
                                  rep("S248",length(unlist(data$Expertise_split)))),
                       ref.val=c(unnest(keywords,key.orc)$url.orc,
                                 unnest(keywords,key.gsc)$url.gsc,
                                 rep("Q109699444",length(unlist(data$Expertise_split)))))
expertise <- expertise[!is.na(expertise$keyword.qid),]

# Prepping to write to Wikidata --------

rows                    <- 1:5619
items                   <- data$QID[rows]

label                   <- data$Name_noinit[rows]
description             <- data$Description[rows]
alias                   <- data$Name_forward[rows]

emp                     <- !(data$Classification[rows]=="HDRSTUDT"|data$Position[rows]=="Graduate Researcher")
emp[is.na(emp)]         <- FALSE
stu                     <-  (data$Classification[rows]=="HDRSTUDT"|data$Position[rows]=="Graduate Researcher")
stu[is.na(stu)]         <- FALSE
emp.properties          <- "P108"
emp.values              <- "Q1478723"
emp.qual.properties     <- data.frame(rep("P6424",length(rows)),
                                      rep("P39",  length(rows)),
                                      rep("P580", length(rows)),
                                      rep("P582", length(rows)))[emp,]
emp.qual.values         <- data.frame(cbind(data$Affiliation[rows],
                                            data$position.or.class.qid[rows],
                                            sapply(data$`Arrive Date`[rows],function(x) paste0("+", x, "T00:00:00Z%2F9")),
                                            sapply(data$`Leave Date`[rows],function(x) if(is.na(x)){""}else{paste0("+", x, "T00:00:00Z%2F9")})))[emp,]
emp.qual.values$X2 <- as.character(emp.qual.values$X2)
emp.qual.values$X2[emp.qual.values$X2=="NULL"]<-NA

stu.properties          <- "P69"
stu.values              <- "Q1478723"
stu.qual.properties     <- data.frame(rep("P512", length(rows)),
                                      rep("P6424",length(rows)),
                                      rep("P580", length(rows)),
                                      rep("P582", length(rows)))[stu,]
stu.qual.values         <- data.frame(cbind(rep("Q752297",length(rows)),
                                            data$Affiliation,
                                            sapply(data$`Arrive Date`[rows],function(x) paste0("+", x, "T00:00:00Z%2F9")),
                                            sapply(data$`Leave Date`[rows],function(x) if(is.na(x)){""}else{paste0("+", x, "T00:00:00Z%2F9")})))[stu,]

website.properties      <- rep("P856",length(rows))
recent                  <- data$`Leave Date`[rows] >= as.POSIXct("2020-01-01")
recent[is.na(recent)]   <- TRUE
website.values          <- rep(NA,length(rows))
website.values[recent]  <- paste0("https://scholars.latrobe.edu.au/",data$Username[rows][recent])
website.qual.properties <- data.frame(rep("P407",length(rows)),
                                      rep("P580",length(rows)),
                                      rep("P582",length(rows)))
website.qual.values     <- data.frame(cbind(rep("Q1860",length(rows)),
                                            sapply(data$`Arrive Date`[rows],function(x) paste0("+", x, "T00:00:00Z%2F9")),
                                            sapply(data$`Leave Date`[rows],function(x) if(is.na(x)){""}else{paste0("+", x, "T00:00:00Z%2F9")})))

id.properties           <- data.frame(rep("P496", length(rows)),
                                      rep("P1153",length(rows)),
                                      rep("P1053",length(rows)))
id.values               <- data.frame(data$ORCID[rows],data$ScopusID[rows],data$ResearcherID[rows])

reference.properties    <- data.frame(rep("S248", length(rows)),
                                      rep("S813", length(rows)),
                                      rep("S1932",length(rows)),
                                      rep("S1324",length(rows)))
reference.values        <- data.frame(rep("Q109699444",length(rows)),
                                      rep(paste0("+",retrieval.date,"T00:00:00Z%2F11"),length(rows)),
                                      paste0(data$Name," (",data$Position," in ",data$Department,", ",data$School,")")[rows],
                                      rep("https://github.com/TS404/LaTrobot",length(rows)))

# Prepping to create on Wikidata --------
tocreate          <- which(data$QID=="CREATE")#[1:100]
sum(data$QID=="CREATE",na.rm = TRUE)
tocreate.names    <- data$Name_noinit[tocreate]
tocreate.alias    <- data$Name_forward[tocreate]
tocreate.alias[tocreate.alias==tocreate.names] <- NA
tocreate.desc     <- paste0("researcher (",casefold(data$Department),")")[tocreate]
tocreate.orcid    <- data$ORCID[tocreate]
tocreate.scopus   <- data$ScopusID[tocreate]
tocreate.resid    <- data$ResearcherID[tocreate]
create.properties <- rep(c("Len","Den","Aen","P31","P496","P1153","P1053"),length(tocreate))
create.values     <- c(rbind(tocreate.names,tocreate.desc,tocreate.alias,"Q5",tocreate.orcid,tocreate.scopus,tocreate.resid))
create.items      <- rep(c("CREATE",rep("LAST",length(create.properties)/length(tocreate))),length(tocreate))

# Writing to Wikidata! --------

write_wikidata(items           = create.items,            # create missing with identifier
               properties      = create.properties,
               values          = create.values,
               format          = "website",
               api.batchname   = "Creation of La Trobe Uni researchers (having ORCID, SCOPUSID or researcherID, or professor/lecturer) via https://github.com/TS404/LaTrobot"
)

write_wikidata(items           = unlist(items[emp]),              # employment
               properties      = emp.properties,
               values          = emp.values,
               qual.properties = emp.qual.properties,
               qual.values     = emp.qual.values,
               src.properties  = reference.properties[emp,],
               src.values      = reference.values[emp,],
               format          = "website",
               api.batchname   = "Employment and affiliations of La Trobe Uni researchers (having ORCID, SCOPUSID or researcherID, or professor/lecturer) via https://github.com/TS404/LaTrobot"
)

write_wikidata(items           = unlist(items[stu]),              # education 
               properties      = stu.properties,
               values          = stu.values,
               qual.properties = stu.qual.properties,
               qual.values     = stu.qual.values,
               src.properties  = reference.properties[stu,],
               src.values      = reference.values[stu,],
               format          = "website",
               api.batchname   = "PhD dates for La Trobe Uni published PhD students (having ORCID, SCOPUSID or researcherID) via https://github.com/TS404/LaTrobot"
)

write_wikidata(items           = unlist(items),           # website
               properties      = website.properties,
               values          = website.values,
               qual.properties = website.qual.properties,
               qual.values     = website.qual.values,
               format          = "website",
               api.batchname   = "Official websites of La Trobe Uni researchers (having ORCID, SCOPUSID or researcherID, or professor/lecturer) via https://github.com/TS404/LaTrobot"
)

write_wikidata(items           = unlist(items),           # ids
               properties      = id.properties,
               values          = id.values,
               format          = "website",
               api.batchname   = "Relevant IDs for La Trobe Uni researchers (having ORCID, SCOPUSID or researcherID, or professor/lecturer) via https://github.com/TS404/LaTrobot"
)

write_wikidata(items           = expertise$qid,         # expertise
               properties      = "P101",
               values          = expertise$keyword.qid,
               qual.properties = "P1932",
               qual.values     = expertise$statedas,
               src.properties  = data.frame(expertise$ref.prop,
                                            rep("S813", nrow(expertise)),
                                            rep("S1324",nrow(expertise))),
               src.values      = data.frame(expertise$ref.val,
                                            rep(paste0("+",retrieval.date,"T00:00:00Z%2F11"),nrow(expertise)),
                                            rep("https://github.com/TS404/LaTrobot",nrow(expertise))),
               format          = "website",
               api.batchname   = "Fields of work for La Trobe Uni researchers (having ORCID, SCOPUSID or researcherID, or professor/lecturer) from Gscholar, Orcid and uni user database via https://github.com/TS404/LaTrobot"
)

