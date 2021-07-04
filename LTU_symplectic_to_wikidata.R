# Setup -----
remove.packages("WikidataR", lib="~/R/win-library/3.6")
devtools::install_local("C:\\Users\\thoma\\OneDrive\\1-Scripts\\GitHub\\WikidataR",force=TRUE)
library(WikidataR)
library(rorcid)
library(tibble)
library(progress)
library(dplyr)
library(readr)
library(readxl)
library(pbapply)
library(stringr)
library(crayon)

researchers <- (!is.na(datafull$ScopusID) |
                !is.na(datafull$ORCID) |
                grepl("PROF",datafull$Classification) |
                grepl("LEC",datafull$Classification) |
                grepl("RES",datafull$Classification) |
                grepl("FELL",datafull$Classification) |
                grepl("AP",datafull$Classification) |
                grepl("ADJ",datafull$Classification))
message(round(sum(!is.na(datafull$ORCID[researchers]))/sum(researchers)*100,1),"% ORCIDS\n",
        round(sum(!is.na(datafull$ScopusID[researchers]))/sum(researchers)*100,1),"% ScopusID\n",
        round(sum(!is.na(datafull$ResearcherID[researchers]))/sum(researchers)*100,1),"% ResearcherID")

list <- list(alice=list(apple="Q1",avocado="Q2"),
             bob = list(banana="Q3", beetroot="STOP"),
             charles = list(crisps=""))

# Metafunctions -----------------

#Reverse names in format surname comma firstname
reverse_names <- function(names          = NULL,
                          correct_case   = TRUE,
                          rm_middle      = FALSE,
                          rm_middle_init = FALSE){
  lastnames  <- gsub(pattern = "(.*), (.*)",replacement = "\\1",names)
  firstnames <- gsub(pattern = "(.*), (.*)",replacement = "\\2",names)
  if(any(rm_middle,rm_middle_init)){
    firstnames <- gsub(pattern = "\\.",replacement = "",firstnames)
    firstnames.s <- str_split(firstnames," ")
    firstnames.s <- lapply(firstnames.s,function(x){x[str_length(x)>1]})
    if(rm_middle){
      firstnames.s <- lapply(firstnames.s,function(x){x[[1]]})
    }
    firstnames <- lapply(firstnames.s,paste,collapse = " ")
  }
  torev <- grepl(",",names)
  names[torev] <- paste(firstnames[torev],
                        lastnames[torev])
  if(correct_case){
    names <- str_to_title(names)
  }
  return(names)
}

split_names <- function(names           = NULL,
                        rm_middle       = TRUE){
  torev <- grepl(",",names)
  names[torev] <- reverse_names(names[torev])
  names.split <- str_split(names," ")

  if(rm_middle){
    firstnames <- sapply(names.split,function(x){x[1]})
    lastnames  <- sapply(names.split,function(x){x[length(x)]})
    return(tibble(firstnames=firstnames,
                  lastnames=lastnames))
  }else{
    return(names.split)
  }
}

disambiguate_QIDs <- function(list,
                              variablename="variables",
                              variableinfo=NULL,
                              limit=10){
  #make sure formatted as a list (e.g. if vector)
  if(!all(class(list)=="list")){
    list <- as.list(list)
  }

  #is the list the outut from a previous half-done run?
  if(any(unlist(lapply(list,function(x) x=="STOP")),na.rm = TRUE)){
    item_to_start_from    <- which(unlist(lapply(list,function(x) any(x=="STOP"))))
    subitem_to_start_from <- first(which(list[[item_done_so_far]] == "STOP"))
    output <- list
  }else{
    item_to_start_from    <- 1
    subitem_to_start_from <- 1
    output <- blank_output_list(list)
  }

  #create output
  pb_main <- progress_bar$new(total = sum(unlist(output,recursive = TRUE)==""|unlist(output,recursive = TRUE)=="STOP"),
                         format     = ":bar :percent eta::eta \n",
                         current    = "|",
                         width      = 90,
                         show_after = 0)
  for(item in item_to_start_from:length(list)){
    for(subitem in subitem_to_start_from:length(list[[item]])){
      #check item to search
      tosearch <- list[[item]][subitem]
      if(is.na(tosearch)){break}                                   #skip NAs
      if(tosearch=="STOP"|tosearch==""){tosearch<-names(tosearch)} #use name for items not done in previous run (stored as "STOP" and "")
      if(grepl("^[Qq][0-9]+$",tosearch)){break}                    #skip if already a QID
      if(is.null(tosearch)){break}                                 #skip nulls or empty items with no name (usually errors)
      list[[item]][subitem] <- tosearch                            #if no skips, place that text back in the list

      #announce choice to be made
      message_header(list,item,subitem,variablename,variableinfo)
      pb_main$tick()
      #execute search and record choice
      first_hit_qid <- firsthit(list[[item]][subitem])
      output[[item]][[subitem]] <- makechoice(first_hit_qid,limit=limit)

      #check if stop request made
      if(!is.na(output[[item]][[subitem]])){if(output[[item]][[subitem]]=="STOP"){
        done_so_far <- item
        message_stop(done_so_far,total = length(list))
        break
      }}
    }
    subitem_to_start_from <- 1 # reset the subitem to start from if completed a full item
    if(!is.na(output[[item]][[subitem]])){if(output[[item]][[subitem]]=="STOP"){break}}
  }
  return(output)
}

#pulling the first hit from wikidata and presenting appropriate choice text options in prep for makechoice()
firsthit <- function(text){
  item <- find_item(text,limit = 1)
  if(length(item)>0){
    if(is.null(item[[1]]$description)){
      desc <- "no description"
    }else{
      desc <- item[[1]]$description
    }
    if(is.null(item[[1]]$label)){
      label <- "no label"
    }else{
      label <- item[[1]]$label
    }
    qid <- item[[1]]$id
    message(white(qid,"   ",label,"   ",desc,sep=""))
    message_choices()
  }else{
    qid <- NA
    message(white("No good match found"))
    message_choices_na()
  }
  names(qid) <- text
  return(qid)
}

# input list of QIDs
# the input list omitting any QIDs where the item's property matches a particular value
# By default, used to omit publications and journals by filtering based on property P31 ('instance of') with filters for Q5633421 ('scientific article') etc.
filter_qids <- function (ids,
                         property = "P31",
                         filter = c("Q737498",
                                    "Q5633421",
                                    "Q7725634",
                                    "Q13442814",
                                    "Q18918145"),
                         message=NULL){
  out <- NULL
  pb <- progress_bar$new(total  = length(ids),
                         format = paste0(message,":bar :percent eta::eta"),
                         width  = 75,
                         show_after = 0)
  for (i in 1:length(ids)){
    pb$tick()
    qid  <- ids[i]
    item <- get_item(qid)
    P31  <- item[[1]]$claims[[property]]$mainsnak$datavalue$value$id
    if(all(is.null(P31))){P31<-"other"}
    if(!any(P31 %in% filter)){
      label <- item[[1]]$labels[[1]]$value
      if(length(item[[1]]$descriptions)>0){
        if(!is.null(item[[1]]$descriptions$en$value)){
          desc <- item[[1]]$descriptions$en$value
        }else{
          desc <- item[[1]]$descriptions[[1]]$value
        }
      }else{
        desc <- "no description"
      }
      if(length(item[[1]]$labels)>0){
        if(!is.null(item[[1]]$labels$en$value)){
          label <- item[[1]]$labels$en$value
        }else{
          label <- item[[1]]$labels[[1]]$value
        }
      }else{
        label <- "no label"
      }
      out <- bind_rows(out,tibble(qid=qid,label=label,desc=desc))
    }
  }
  if(is.null(out)){
    out <- tibble(qid=NA,
                  label=NA,
                  desc="No current matching Wikidata item")
  }
  return(out)
}

blank_output_list <- function(list){
  make_attr_names <- function(x){
    x1 <- list[[x]]
    attr(x1, 'names') <- x1
    x1
  }
  if(all(is.null(names(list)))){
    output <- list
    names(output) <- list
  }else{
    output <- lapply(names(list), make_attr_names)
    names(output) <- names(list)
  }
  output <- rapply(output,function(x) ifelse(is.na(x),NA,""),how = 'replace')
  return(output)
}


restarted_output_list <- function(list){
  make_attr_names_rev <- function(x){
    x1 <- list[[x]]
    x1 <- attr(x1, 'names')
    x1
  }
  listnames <- lapply(names(list), make_attr_names_rev)
  output <- rapply(output,function(x) ifelse(is.na(x),NA,""),how = 'replace')
  return(output)
}

choices_alt <- function(selection,limit=10){
  altqids <- unlist(lapply(find_item(selection,limit=limit),function(x) x$id))
  if(is.null(altqids)){
    message("Searching for ",bold$white(selection)," as an alternative term")
    results <- tibble(qid=NA,
                      label=NA,
                      desc="No current matching Wikidata item")
  }else{
    message("Searching for ",bold$white(selection)," as an alternative term")
    results <- filter_qids(altqids)
  }
  if(all(is.na(results$qid))){
    message(white("No good match found"))
    message_choices_na()
    return(NULL)
  }else{
    message_choices_alt(results)
    names(results$qid) <- results$label
    return(results)
  }
}

# When provided with a QID, interactively make a decision on whether the output should be that qid or some other value
makechoice <- function(qid=NULL,
                       text=NULL,
                       table=NULL,
                       limit=10){
  if(is.null(text)){
    text <- names(qid)
  }

  # announce item for disambig
  suppressWarnings(invisible(selection <- readline()))
  if      (selection=="s"|selection=="stop"){                   #s = stop
    output <- "STOP"
    names(output) <- text

  }else if(selection=="y"|selection=="yes"){                    #y = accept
    output <- qid
    names(output) <- text

  }else if(selection=="n"|selection=="no"|selection==""){       #n = reject
    output <- NA
    names(output) <- text

  }else if(selection=="c"|selection=="create"){                 #c = create
    output <- "CREATE"
    names(output) <- text

  }else if(selection=="?"){                                     #? = loop up in browser
    browseURL(paste0("https://www.wikidata.org/wiki/",qid))
    output <- makechoice(qid,text,table,limit)

  }else if(grepl("^[Qq][0-9]+$",selection)){                    #Q123 = id
    output <- selection
    names(output) <- paste0("-> ",selection)

  }else if(grepl("^[Qq][0-9]+?$",selection)){                   #Q123? = search that id
    browseURL(paste0("https://www.wikidata.org/wiki/",
                     gsub("\\?","",selection)))
    output <- makechoice(qid,text,table,limit)

  }else if(grepl("^[0-9]+$",selection) & !is.null(table)){      #number = select row
    output <- table$qid[as.numeric(selection)]
    label  <- table$label[as.numeric(selection)]

  }else if(grepl("^[0-9]+\\?$",selection)& !is.null(table)){    #number? = loop up row in browser
    browseURL(paste0("https://www.wikidata.org/wiki/",
                     table$qid[as.numeric(gsub("\\?","",selection))]))
    output <- makechoice(qid,text,table,limit)
    label  <- table$label[as.numeric(selection)]

  }else if((selection=="a"|selection=="alt") & !is.null(text)){ #a = alternative
    table  <- choices_alt(text,limit)
    output <- makechoice(qid,text,table,limit)
    if(!is.null(names(output)) & !is.null(text)){if(names(output)!=text){
      names(output) <- paste0(text," -> ",names(output))
    }}

  }else{                                                        #freetext = freetext to search
    table  <- choices_alt(selection,limit)
    output <- makechoice(qid,selection,table,limit)
    if(!is.null(names(output)) & !is.null(text)){if(names(output)!=text){
      names(output) <- paste0(text," -> ",names(output))
    }}
  }

  return(output)
}

#> messages --------
message_header <- function(list,
                           i,
                           j,
                           variablename=NULL,
                           variableinfo=NULL){
  list         <- as.list(list)
  name         <- bold$cyan(names(list)[[i]])
  variables    <- list[[i]]
  variables[j] <- bold$white$underline(variables[j])
  variables    <- paste(variables,collapse = " | ")
  if(!is.null(variablename)){
    variablename <- paste0("the ",variablename," of ")
  }else{
    variablename <- NULL
  }
  if(!is.null(variableinfo)){
    variableinfo <- paste0(variableinfo,"\n")
  }else{
    variableinfo <- NULL
  }
  message("\014",
          "--------------------------------------------------------------------------- \n",
          "Let's disambiguate ",variablename,
          name, ": \n",
          variableinfo,
          variables)
}

message_choices <- function(){
  message(bold(" y    "),"-> accept the presented match \n",
          bold(" n    "),"-> reject the presented match and move on to the next \n",
          bold(" a    "),"-> request alternative possible matches \n",
          bold(" Q123 "),"-> use this as the wikidata QID \n",
          bold(" text "),"-> try this text as alternative search term \n",
          bold(" c    "),"-> create a new item for this later \n",
          bold(" s    "),"-> stop here, save those done so far and come back later \n",
          bold(" ?    "),"-> check the presented match in your browser")
}

message_choices_na <- function(){
  message(bold(" y/n  "),"-> leave as 'NA' \n",
          bold(" Q123 "),"-> use this as the wikidata QID \n",
          bold(" text "),"-> try this text as alternative search term \n",
          bold(" c    "),"-> create a new item for this later \n",
          bold(" s    "),"-> stop here, save those done so far and come back later")
}

message_choices_alt <- function(table){
  message("Are any of these appropriate?")
  print(data.frame(table),right=FALSE)
  message(bold(" number "),"-> select one of the matches presented (include ",bold("?")," to check an item in your browser) \n",
          bold(" Q123   "),"-> use this as the wikidata QID \n",
          bold(" text   "),"-> try this text as alternative search term \n",
          bold(" c      "),"-> create a new item for this later \n",
          bold(" s      "),"-> stop here, save those done so far and come back later")
}

message_stop <- function(done_so_far,total){
  message("Stopping. You've completed ",
          bold$white(done_so_far),
          " so far (",
          bold$white(total - done_so_far),
          " remaining). \n",
          "To restart from where you left off, use the output from this function as the list for disambiguate_QIDs()")
}

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




ORCiDs_from_names <- function(data = data){
  ORCIDs_certain <- sum(!is.na(data$ORCID))/nrow(data)
  message("Finding additional ORCiDs based on names and affiliations")
  data$ORCID_estimate<-NA
  pb <- progress_bar$new(total = sum(data$College!="Central" & is.na(data$ORCID), na.rm = 1),
                         format = "[:bar] :percent eta::eta")
  for (i in which(data$College!="Central" & is.na(data$ORCID))){
    pb$tick()
    ORCID.guess <- orcid_search (given_name  = gsub(pattern = ".*, ([^ ]*).*",replacement = "\\1 ",data$name[i]),
                                 family_name = gsub(pattern = "([^ ]*),.*",replacement = "\\1 ",data$name[i]),
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
  message(100*round(ORCIDs_certain,3),"% of the ",nrow(data)," rows have an ORCiD. I've found ORCiDs for an additional ",
          100*round(ORCIDs_estimate,3),"% based on name and affiliation, leaving ",
          100*round(1-ORCIDs_certain-ORCIDs_estimate,3),"% unaccounted for.")
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
  rows.without.qids <- is.na(data$QID)
  rows.with.orcids  <- !is.na(data$ORCID)
  rows              <- which(rows.without.qids & rows.with.orcids)
  pb <- progress_bar$new(total = length(rows),
                         format = "[:bar] :percent eta::eta")
  for (i in rows){
    pb$tick()
    newqid <- suppressMessages(qid_from_ORCID(ORCID = data$ORCID[i])$qid[[1]])
    if(is.na(newqid)){
      data$QID[i] <- newqid
    }else{
      data$QID[i] <- as.character(newqid)
    }
  }
  data$QID[data$QID=="logical(0)"]<-NA
  qids_certain <- sum(!is.na(data$QID))/nrow(data)
  message(100*round(qids_certain,3),"% of the ",nrow(data)," rows have a QID")
  return(data)
}


QIDs_from_ScopusIDs <- function(data = data){
  message("Finding QIDs based on ScopusIDs")
  if(suppressWarnings(is.null(data$QID))){data$QID<-NA}
  rows.without.qids <- is.na(data$QID)
  rows.with.scopus  <- !is.na(data$ScopusID)
  rows              <- which(rows.without.qids & rows.with.scopus)
  pb <- progress_bar$new(total = length(rows),
                         format = "[:bar] :percent eta::eta")
  for (i in rows){
    pb$tick()
    newqid <- suppressMessages(qid_from_identifier("P1153",value=data$ScopusID[i])$qid[[1]])
    if(is.na(newqid)){
      data$QID[i] <- newqid
    }else{
      data$QID[i] <- as.character(newqid)
    }
  }
  data$QID[data$QID=="logical(0)"]<-NA
  qids_certain <- sum(!is.na(data$QID))/nrow(data)
  message(100*round(qids_certain,3),"% of the ",nrow(data)," rows have a QID")
  return(data)
}


QIDs_from_ResearcherIDs <- function(data = data){
  message("Finding QIDs based on ResearcherIDs")
  if(suppressWarnings(is.null(data$QID))){data$QID<-NA}
  rows.without.qids      <- is.na(data$QID)
  rows.with.researcherid <- !is.na(data$ResearcherID)
  rows                   <- which(rows.without.qids & rows.with.researcherid)
  pb <- progress_bar$new(total = length(rows),
                         format = "[:bar] :percent eta::eta")
  for (i in rows){
    pb$tick()
    newqid <- suppressMessages(qid_from_identifier("P1053",value=data$ScopusID[i])$qid[[1]])
    if(is.na(newqid)){
      data$QID[i] <- newqid
    }else{
      data$QID[i] <- as.character(newqid)
    }
  }
  data$QID[data$QID=="logical(0)"]<-NA
  qids_certain <- sum(!is.na(data$QID))/nrow(data)
  message(100*round(qids_certain,3),"% of the ",nrow(data)," rows have a QID")
  return(data)
}


QIDs_from_names <- function(data = data, plot=TRUE){
  message("Finding additional possible QIDs based on names")
  data$QID_estimate <- NA
  data$QID_estimate_desc <- NA
  pb <- progress_bar$new(total = sum(is.na(data$QID)),
                         format = "[:bar] :percent eta::eta")
  for (i in which(is.na(data$QID))){
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
  qids_certain  <- sum(!is.na(data$QID))/nrow(data)
  qids_estimate <- (sum(!is.na(data$QID))+sum(!is.na(data$QID_estimate)))/nrow(data) - qids_certain
  message(100*round(qids_certain,3),"% of the ",nrow(data)," rows have a QID. I've taken a guess at an additional ",
          100*round(qids_estimate,3),"% based only on matching names, leaving ",
          100*round(1-qids_certain-qids_estimate,3),"% unaccounted for.")
  return(data)
}


disambiguate_QIDs_old <- function(data = data, done_so_far=0){
  if(all(class(data)=="list")){
    done_so_far <- data$done_so_far
    data <- data$data
  }
  if(done_so_far>0){
    message("Looks like ",done_so_far," records have already been checked. To start again, set done_so_far to 0.")
  }
  message("Select the most appropriate QID from the options listed (or leave blank if none look correct)")
  qids_estimate_start  <- sum(!is.na(data$QID_estimate))
  pb <- progress_bar$new(total  = round(sum(!is.na(data$QID_estimate) & is.na(data$QID))/10,0),
                         format = "[:bar] approx eta::eta \n")
  for (i in which(!is.na(data$QID_estimate) & is.na(data$QID))){
    repeat{
      print(cbind(QID=data$QID_estimate[i][[1]],
                  Description=data$QID_estimate_desc[i][[1]]))
      message("Select an option then press [enter]
              To select the correct QID, just enter the number
              To check the current wikidata entry, include ? after the number
              To select none of the suggested QIDs, leave blank
              To stop for now, type stop")
      suppressWarnings(invisible(selection <- readline()))
      if(selection=="stop"){
        message("Stopping. You've completed ",
                done_so_far,
                " so far (",
                sum(!is.na(data$QID_estimate) & is.na(data$QID)),
                " remaining). This has been saved as 'done_so_far'."
                ,"\nTo restart from where you left off, use the 'done_so_far' parameter for this function")
        return(list(data=data,done_so_far=done_so_far))
      }else if(grepl("[?]",selection)){
        browseURL(paste0("https://www.wikidata.org/wiki/",
                         data$QID_estimate[i][[1]][as.numeric(gsub("[?]","",selection))]))
        browseURL(paste0("https://scholars.latrobe.edu.au/",
                         data$Username[i]))
      }else if(selection %in% 1:length(data$QID_estimate[i][[1]])){
        qid.selected <- data$QID_estimate[i][[1]][as.numeric(selection)]
        message("-> ", qid.selected," selected")
        data$QID[i]<- qid.selected
        break
      }else{
        message("-> none selected")
        break
      }
    }
    done_so_far <- done_so_far+1
    if(done_so_far%%10==0){
      pb$tick()
    }
  }
  qids_certain  <- sum(!is.na(data$QID))/nrow(data)
  qids_estimate <- sum(!is.na(data$QID_estimate) & is.na(data$QID))/nrow(data)
  message("Done. ",
          100*round(qids_certain,3),"% of the ",nrow(data)," rows now have a confirmed QID. \nOf the ",
          qids_estimate_start," to be disambiguated, ",
          sum(!is.na(data$QID_estimate) & !is.na(data$QID)),
          " have been sucessfully confirmed, leaving ",
          sum(!is.na(data$QID_estimate) & is.na(data$QID)),
          " with no wikidata item")
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







# Data load -----
# Note: "2021.06.01_UserData_Thomas.xlsx" is the result of "SQL_qery.sql"
datafull <- read_excel("2021.06.01_UserData_Thomas.xlsx")
datafull$`Leave Date`[datafull$`Leave Date`==as.POSIXct("9999-12-31","UTC")]<-NA
saveRDS(datafull,"LTU_all.RDS")
data <- head(datafull,25)
expertiseoptions <- names(sort(table(trimws(unlist(strsplit(datafull$Expertise[!is.na(datafull$Expertise)],";"))))))
positionoptions  <- head(names(sort(table(datafull$Position[datafull$College!="Central" & !is.na(datafull$ORCID)]),decreasing = TRUE)),40)
positionoptions.qid <- cbind(position=positionoptions,
                             qid=disambiguate_QIDs(positionoptions))

# Analysis -----
data <- ORCiDs_from_names(data)
# scholar_from_names(data) # ... get_scholar_id(last_name = "", first_name = "", affiliation = "La Trobe")
data <- disambiguate_ORCiDs(data)
data <- ORCiD_pub_number(data)
data <- QIDs_from_ORCiDs(data)
data <- QIDs_from_ScopusIDs(data)
data <- QIDs_from_ResearcherIDs(data)
data <- QIDs_from_names(data)
data <- disambiguate_QIDs(data)
data <- expertiseQIDs_from_expertise(data)
data <- disambiguate_expertise(data)

#saveRDS(data,file=paste0("LTU_ORCIDs_qids_",format(Sys.time(), "%Y-%m-%d"),".RDS"))

# Prepping to write to Wikidata --------

if(all(class(data)=="list")){
  data <- data$data
}

rows                    <- which(!is.na(data$QID))
items                   <- data$QID[rows]
affil.properties        <- "P108"
affil.values            <- "Q1478723"
affil.qual.properties   <- data.frame(rep("affiliation string",length(rows)),
                                      rep("P580",length(rows)),
                                      rep("P582",length(rows)))
affil.qual.values       <- data.frame(cbind(paste0(str_replace_na(data$Department[rows],""),
                                        sapply(data$School[rows],function(x) if(is.na(x)){""}else{paste(",",x)})),
                                 sapply(data$`Arrive Date`[rows],function(x) paste0("+", x, "-01-01T00:00:00Z%2F9")),
                                 sapply(data$`Leave Date`[rows],function(x) if(is.na(x)){""}else{paste0("+", x, "-01-01T00:00:00Z%2F9")})))

website.properties      <- "P856"
website.values          <- paste0("https://scholars.latrobe.edu.au/",data$Username[rows])
website.qual.properties <- data.frame(rep("P407",length(rows)),
                                      rep("P580",length(rows)),
                                      rep("P582",length(rows)))
website.qual.values     <- data.frame(cbind(rep("Q1860",length(rows)),
                                 sapply(data$`Arrive Date`[rows],function(x) paste0("+", x, "-01-01T00:00:00Z%2F9")),
                                 sapply(data$`Leave Date`[rows],function(x) if(is.na(x)){""}else{paste0("+", x, "-01-01T00:00:00Z%2F9")})))

expertise.rows          <- which(!is.na(data$QID) & !is.na(data$expertise.qids.correct))
expertise.items         <- rep(expertise.items,lapply(data$expertise.qids.correct[expertise.rows],length))
expertise.properties    <- "P101"
expertise.values        <- unlist(data$expertise.qids.correct[expertise.rows])


# Writing to Wikidata! --------
write_wikidata(items           = items,
               properties      = affil.properties,
               values          = affil.values,
               qual.properties = affil.qual.properties,
               qual.values     = affil.qual.values,
               format          = "website"
)

write_wikidata(items           = items,
               properties      = website.properties,
               values          = website.values,
               qual.properties = website.qual.properties,
               qual.values     = website.qual.values,
               format          = "website"
)

write_wikidata(items           = expertise.items,
               properties      = expertise.properties,
               values          = expertise.values,
               format          = "website"
)

write_wikidata(items        = c("Q4115189","Q13406268"),
               properties   = "P108",
               values       = "Q1478723",
               qual.properties = data.frame(c("P580","P580"),c("P582","P582")),
               qual.values  = data.frame(sapply(data[1:2,c('Arrive Date','Leave Date')],
                                                function(x) if(x[[1]]!=as.POSIXlt("9999-12-31 00:00:00",'UTC')){
                                                  paste0("+", x, "T00:00:00Z/11")
                                                  }else{
                                                    ""
                                                  })),
               format       = "website",
               api.username = "Evolution_and_evolvability", # Enter your Wikimedia username here
               api.token    = "%242y%2410%24Kmj2vtbn4D9BwxXjM7ZlrOIKTMYewniVb845puSGsahMkQurWoN7C", #REDACTED# Find yours from https://tools.wmflabs.org/quickstatements/#/user
               api.submit   = TRUE
)
