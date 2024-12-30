

# Clean and format PDF text prior to keyword search -----
pdf_clean <- function(docs) {
  if(class(try(qpdf::pdf_length(docs),silent=TRUE))!="try-error"){
    ##y_by_lines <- suppressWarnings(pdftools::pdf_text(docs))
    kw_by_lines <- suppressWarnings(pdfsearch::keyword_search(docs,path=TRUE,token_results=FALSE,keyword = c(''),split_pdf = TRUE,surround_lines = FALSE, ignore_case = TRUE, remove_hyphen = TRUE, heading_search = FALSE,convert_sentence = FALSE,split_pattern="\\p{WHITE_SPACE}{5,}"))
    y_by_lines <- paste(kw_by_lines$line_text)
    if(NROW(y_by_lines)==0){y_by_lines <- suppressWarnings(pdftools::pdf_text(docs))}
    
    y_by_lines <- stringi::stri_trans_general(str = y_by_lines, id = "Latin-ASCII")
    y_by_lines <- textclean::replace_contraction(y_by_lines)
    y_by_lines <- gsub("'", " ", y_by_lines, perl = TRUE)
    
    y_by_lines <- gsub("(\")", "", y_by_lines, perl = TRUE, useBytes = TRUE)
    y_by_lines <- gsub("[\f]", " ", y_by_lines, perl = TRUE, useBytes = TRUE)
    y_by_lines <- gsub("[\001]|[\002]|[\003]|[\004]|[\005]", " ", y_by_lines, perl = TRUE, useBytes = TRUE)
    y_by_lines <- gsub("[\r]", " ", y_by_lines, perl = TRUE, useBytes = TRUE)
    y_by_lines <- gsub("[\n]", "  ", y_by_lines, perl = TRUE, useBytes = TRUE)
    
    y_by_lines <- gsub("[\u008f]|[\u0081]|[\u0083]|[\u0093]|[\uF0B7]|[\uF0A7]|[\u2022]|[\u00B7]|[•]| o |^- |^[0-9][,]|^[0-9][.]|^[0-9][ ]|^[0-9][0-9][,]|^[0-9][0-9][.]|^[0-9][0-9][ ]|^[0-9][0-9][0-9][,]|^[0-9][0-9][0-9][.]|^[0-9][0-9][0-9][ ]|[+]|[ ][o][ ]|^[o][ ]", ". ", y_by_lines, perl = TRUE)
    
    y_by_lines <- unlist(strsplit(y_by_lines, "\\s\\s"))
    y_by_lines[grep("^ [A-Z]", y_by_lines, perl = TRUE, useBytes = TRUE)] <- gsub("^ ", ". ", y_by_lines[grep("^ [A-Z]", y_by_lines, perl = TRUE, useBytes = TRUE)], perl = TRUE, useBytes = TRUE)
    y_by_lines[setdiff(grep("^[A-Z]", y_by_lines, perl = TRUE, useBytes = TRUE),grep("[.]|[;]|[:]", y_by_lines, perl = TRUE, useBytes = TRUE))] <- paste(". ", y_by_lines[setdiff(grep("^[A-Z]", y_by_lines, perl = TRUE, useBytes = TRUE),grep("[.]|[;]|[:]", y_by_lines, perl = TRUE, useBytes = TRUE))], sep = "")
    y_by_lines[grep("[ ][A-Z][.][ ]", y_by_lines, perl = TRUE, useBytes = TRUE)] <- gsub("[.]","",y_by_lines[grep("[ ][A-Z][.][ ]", y_by_lines, perl = TRUE, useBytes = TRUE)])
    
    y_by_lines <- unlist(strsplit(y_by_lines, "\\s"))
    y_by_lines <- y_by_lines[y_by_lines != ""]
    y_by_lines <- gsub("[(]", "( ", y_by_lines, perl = TRUE)
    y_by_lines[grep("^ac[.]|^Dept[.]|^Dr[.]|^est[.]|^etc[.]|^ft[.]|^inc[.]|^cm[.]|^mm[.]|^m[.]|^mi[.]|^rd[.]|^sp[.]|^spp[.]|^sq[.]|^st[.]|^ed[.]|^eds[.]",y_by_lines, ignore.case = TRUE)] <- gsub("[.]", "", y_by_lines[grep("^ac[.]|^Dept[.]|^Dr[.]|^est[.]|^etc[.]|^ft[.]|^inc[.]|^cm[.]|^mm[.]|^m[.]|^mi[.]|^rd[.]|^sp[.]|^spp[.]|^sq[.]|^st[.]|^ed[.]|^eds[.]",y_by_lines, ignore.case = TRUE)], ignore.case = TRUE, perl = TRUE, useBytes = TRUE)
    y_by_lines <- gsub("[(][ ]", "(", y_by_lines, perl = TRUE)
    
    y_whole <- paste(y_by_lines, collapse = ' ')
    
    y_whole <- qdapRegex::rm_citation(y_whole)
    y_whole <- qdapRegex::rm_url(y_whole)
    y_whole <- qdapRegex::rm_abbreviation(y_whole)
    y_whole <- textclean::replace_email(y_whole)
    
    y_whole <- gsub("[,]", " ,", y_whole, perl = TRUE)
    
    y_whole <- gsub("[.][0-9]", "x", y_whole, ignore.case = TRUE, perl = TRUE, useBytes = TRUE)
    
    y_whole <- gsub("[-][ ]|[/]", " ", y_whole, ignore.case = TRUE, perl = TRUE, useBytes = TRUE)
    
    y_whole<-gsub("[^ -~]"," ",y_whole, perl = TRUE, useBytes = TRUE)   
    y <- unlist(strsplit(y_whole, "[.]|[;]|[:]\\s|\\s[0-9][)]|[(][0-9][)]"))  
    
    y <- gsub("  ", " ", y, perl = TRUE, useBytes = TRUE)
    y <- gsub("^ ", "", y, perl = TRUE, useBytes = TRUE)
    y <- gsub("$", " ", y, perl = TRUE, useBytes = TRUE)
    unique(y)
  }
}



# Match keywords -----
synonym_searcher <- function(docs, keywords, add_sent = 1, res = dplyr::tibble()) {
  if(is.vector(docs)) {
    path <- docs
    name <- basename(docs)
  } else {
    path <- dplyr::select(docs, dplyr::contains("path"))[[1]]
    name <- dplyr::select(docs, dplyr::contains("name"))[[1]]
  }
  
  text <- pdf_clean(path)
  
  if(NROW(text) > 0) {
    text_as_df <- dplyr::tibble("Line Number" = seq(1, NROW(text)),
                                "Sentence" = text)
    
    for(c in unique(keywords$Category)) {
      keyword_sub <- dplyr::filter(keywords, Category == c)
      
      for(i in 1:NROW(keyword_sub)) {
        matched_rows <- text_as_df$`Line Number`[grep(keyword_sub$Include[i], text_as_df$Sentence, ignore.case = TRUE, perl = TRUE, useBytes = TRUE)]
        
        if(NROW(matched_rows) > 0) {
          ind_hits <- dplyr::filter(text_as_df, `Line Number` %in% matched_rows)
          matched_rows<-ind_hits$'Line Number'[setdiff(grep(keyword_sub$Near[i], ind_hits$Sentence,ignore.case=TRUE,perl=TRUE, useBytes = TRUE),grep(keyword_sub$Exclude[i], ind_hits$Sentence,ignore.case=TRUE,perl=TRUE, useBytes = TRUE))]
          ind_hits <- dplyr::filter(text_as_df, `Line Number` %in% matched_rows) |>
            dplyr::mutate(`Planning Doc` = name,
                          Category = keyword_sub$Category[i],
                          Class = keyword_sub$Class[i],
                          SubClass = keyword_sub$SubClass[i],
                          Tier = keyword_sub$Tier[i]) |>
            dplyr::select(`Planning Doc`, Category, Class, SubClass, Tier, `Line Number`, `Text from Planning Doc` = Sentence)
          
          res <- rbind(res, ind_hits)
        }
        
      }
      
      if(which(unique(keywords$Category)==c)<3){
        line_sub <- res$`Line Number`
        
        for(s in 0:(as.numeric(3-which(unique(keywords$Category)==c))*as.numeric(add_sent))) {
          line_sub <- c(line_sub, res$`Line Number` + s, res$`Line Number` - s)
        }
        
        line_sub <- unique(line_sub[line_sub > 0 & line_sub <= max(text_as_df$`Line Number`, na.rm = TRUE)])
        text_as_df <- dplyr::filter(text_as_df, `Line Number` %in% line_sub)
      }
    }
    
    if(NROW(res) > 0) {
      res <- unique(res)
      
      subclasses<-unique(res[,c("Class","SubClass","Tier")])
      
      tmp <- dplyr::filter(res,Tier==4) |>
        dplyr::select('Planning Doc', 'Line Number','Text from Planning Doc','Category','Class','SubClass')
      
      for(i in c(3,2,1)){
        tmp2 <- dplyr::filter(res,Tier==i) |>
          dplyr::select('Planning Doc', 'Line Number','Text from Planning Doc','Category',Class,SubClass) %>%
          dplyr::full_join(., tmp, by = c("Planning Doc","Category","Line Number","Text from Planning Doc","SubClass" = "Class"), relationship = "many-to-many") |>
          dplyr::mutate('Class0'=SubClass)
        colnames(tmp2)[which(colnames(tmp2)=="SubClass.y")]<-paste0(i+1,"_SubClass")
        colnames(tmp2)[which(colnames(tmp2)=="Class0")]<-paste0(i+1,"_Class")
        tmp<-dplyr::select(tmp2,-Class) |>
          dplyr::left_join(unique(keywords[keywords$Word_type1=="include",c("Class","SubClass"),])) |>
          dplyr::select(colnames(tmp2))
      }
      
      res<-dplyr::select(tmp,'Planning Doc','Category',Class,SubClass, 'Line Number','Text from Planning Doc') |>
        dplyr::mutate(SubClass = ifelse(is.na(tmp$'4_SubClass')==FALSE,tmp$'4_SubClass',
                                        ifelse(is.na(tmp$'3_SubClass')==FALSE,tmp$'3_SubClass',				
                                               ifelse(is.na(tmp$'2_SubClass')==FALSE,tmp$'2_SubClass',tmp$SubClass)))) |>			
        dplyr::mutate(Class = ifelse(is.na(tmp$'4_SubClass')==FALSE,tmp$'4_Class',
                                     ifelse(is.na(tmp$'3_SubClass')==FALSE,tmp$'3_Class',				
                                            ifelse(is.na(tmp$'2_SubClass')==FALSE,tmp$'2_Class',tmp$Class))))	 |>
        dplyr::left_join(subclasses)
      
    }
    
  }
  
  res
}