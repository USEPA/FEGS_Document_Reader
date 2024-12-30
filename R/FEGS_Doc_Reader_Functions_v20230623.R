#' Create the synonym lists
#'
#' @param synonym_list_file_path 
#'
#' @return
#' @export
#'
#' @examples
#' create_synonym_lists("Synonym_List_NESCS_plustaxa.csv")
#' load('data/synonym_list.rda')
#' load('data/synonym_list_all.rda')
#' 
create_synonym_lists <- function(synonym_list_file_path) {
  # First column is FEGS category, 2nd is name of class, 3rd column is subclass, all additional columns are synonyms
  synonym_list <- utils::read.table(file = synonym_list_file_path, sep = ",", header = TRUE, as.is = TRUE)
  
  # Save synonym_list to data/
  usethis::use_data(synonym_list, overwrite = TRUE)
  
  # Stack the synonym file into one long vector; place near,include,exclude side by side
  synonym_list_all <- dplyr::tibble(Category = rep(synonym_list$Category, 12),
                                    Class = rep(synonym_list$Class, 12),
                                    SubClass = rep(synonym_list$SubClass, 12),
                                    Tier = rep(synonym_list$Tier, 12),
                                    Word_type = rep(synonym_list$Word_type, 12),
                                    utils::stack(synonym_list[, 6:NCOL(synonym_list)])) |>
    dplyr::rename(Synonym = values, S_Column = ind) |>
    dplyr::filter(Synonym != "", !is.na(Synonym)) |>
    tidyr::pivot_wider(names_from = c(Word_type), values_from = c(Word_type, Synonym)) |>
    dplyr::select(Category, Class, SubClass, Tier, S_Column, Word_type1 = Word_type_include, Include = Synonym_include,
                  Word_type2 = Word_type_near, Near = Synonym_near, Word_type3 = Word_type_exclude, Exclude = Synonym_exclude) |>
    dplyr::mutate(Near = ifelse(is.na(Near), " ", Near),
                  Exclude = ifelse(is.na(Exclude), "stringoftexttoforcenotexclude", Exclude))
  
  # Save synonym_list_all to data/
  usethis::use_data(synonym_list_all, overwrite = TRUE)
}

#' Manually test run FEGS computations
#'
#' @param pdf_file_paths character vector of pdf file paths to analyze
#'
#' @return
#'
#' @examples
#' run_manually(dir('PdfFiles/',full.names = TRUE))
run_manually <- function(docs_dt) {
  
    # (1) find the keyword matches
    keyword_matches <- synonym_searcher(docs_dt, synonym_list_all, 1)
    
    # (2) Find all triplets from keyword matches
    trips_all <- find_triplets(keyword_matches, 1, synonym_list_all)
    
    # (3) Count triplets from documents
    trip_count <- count_triplets(trips_all)
    
    # (4) Clean Triplets
    clean <- clean_triplets(trips_all, trip_count, 1)
  
    return(clean)
}




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



# Find triplets -----
find_triplets <- function(syn_match, add_sent = 1, keywords) {
  BEF <- dplyr::tibble(Document = character(), Line_Number = integer(), Sentence = character())
  for (c in sort(unique(syn_match$Category))) {
    tmp <- dplyr::filter(syn_match, Category == c) |>
      dplyr::select(Document = `Planning Doc`, Line_Number = `Line Number`, Sentence = `Text from Planning Doc`, SubClass)
    colnames(tmp)[4] <- paste0(c, "_SubClass")
    BEF <- dplyr::full_join(BEF, tmp, relationship = "many-to-many")
    if(c == "Environment") {
      BEF <- dplyr::rename(BEF, Ecosystem_SubClass = Environment_SubClass)
    }
    if(c == sort(unique(syn_match$Category))[max(length(unique(syn_match$Category)))]) {
      BEF <- dplyr::mutate(BEF, Matches = 3 - (is.na(Beneficiary_SubClass) + is.na(Ecosystem_SubClass) + is.na(FEGS_SubClass)))|>
        dplyr::mutate(Paragraph_Text = Sentence,
                      Consider_Paragraph = 0)
    }
  }
  
  BEF_Trips <- BEF |>
    dplyr::filter(Matches == 3) |>
    dplyr::mutate(Paragraph_Text = Sentence,
                  Consider_Paragraph = 0)
  
  if(NROW(BEF_Trips) > 0) {
    if(add_sent>0){for(a_s in 1:add_sent){  
      
      BEF_NoB <- BEF |>
        dplyr::filter(Matches != 3) |>
        dplyr::mutate(Line_Pm1 = Line_Number - as.numeric(a_s),
                      Line_Pp1 = Line_Number + as.numeric(a_s)) %>%
        dplyr::left_join(., dplyr::select(BEF, Document, Line_Number, Sentence), by = c("Document", "Line_Pm1" = "Line_Number"), relationship = "many-to-many") %>%
        dplyr::left_join(., dplyr::select(BEF, Document, Line_Number, Sentence), by = c("Document", "Line_Pp1" = "Line_Number"), relationship = "many-to-many") |>
        dplyr::mutate(Paragraph_Text = paste(Sentence.y, Sentence.x, Sentence, sep = " . "),
                      Paragraph_Text = ifelse(Paragraph_Text == "NA . NA", NA, Paragraph_Text)) |>
        dplyr::rename(Original_Sentence = Sentence.x) |>
        dplyr::select(Document, Line_Pm1, Line_Number, Line_Pp1, Original_Sentence, Paragraph_Text, Beneficiary_SubClass, Ecosystem_SubClass, FEGS_SubClass, Consider_Paragraph) |>
        unique()
    
      BEF_mp <- dplyr::tibble(Document = character(), Line_Pm1 = numeric(), Line_Number = integer(), Line_Pp1 = numeric(), Original_Sentence = character(), Paragraph_Text = character(),
                              Original_Beneficiary_SubClass = character(), Original_Ecosystem_SubClass = character(), Original_FEGS_SubClass = character(),Consider_Paragraph = numeric(),
                              Beneficiary_SubClass_MP1 = character(), Ecosystem_SubClass_MP1 = character(), FEGS_SubClass_MP1 = character())

      for(n in colnames(BEF_NoB)[7:9]) {
        if(n == colnames(BEF_NoB)[7]) {
          df <- BEF_NoB
        }
        
        tmp_m1 <- dplyr::inner_join(df, unique(dplyr::select(dplyr::filter(BEF, !is.na(n)), Document, Line_Number, all_of(n))),
                                    by = c("Document", "Line_Pm1" = "Line_Number"), relationship = "many-to-many")
        colnames(tmp_m1)[grepl(".x", colnames(tmp_m1), fixed = TRUE, perl = TRUE, useBytes = TRUE)] <- paste0("Original_", n)
        colnames(tmp_m1)[grepl(".y", colnames(tmp_m1), fixed = TRUE, perl = TRUE, useBytes = TRUE)] <- paste0(n, "_MP1")
      
        tmp_p1 <- dplyr::inner_join(df, unique(dplyr::select(dplyr::filter(BEF, !is.na(n)), Document, Line_Number, all_of(n))),
                                    by = c("Document", "Line_Pp1" = "Line_Number"), relationship = "many-to-many")
        colnames(tmp_p1)[grepl(".x", colnames(tmp_p1), fixed = TRUE, perl = TRUE, useBytes = TRUE)] <- paste0("Original_", n)
        colnames(tmp_p1)[grepl(".y", colnames(tmp_p1), fixed = TRUE, perl = TRUE, useBytes = TRUE)] <- paste0(n, "_MP1")
      
        tmp_mp <- unique(rbind(tmp_m1, tmp_p1))
      
        tmp_x <- df |>
          dplyr::filter(!paste(Document, Line_Number) %in% paste(tmp_mp$Document, tmp_mp$Line_Number))
        colnames(tmp_x)[grepl(n, colnames(tmp_x), fixed = TRUE, perl = TRUE, useBytes = TRUE)] <- paste0("Original_", n)
        tmp_x$New <- NA
        colnames(tmp_x)[grepl("New", colnames(tmp_x), fixed = TRUE, perl = TRUE, useBytes = TRUE)] <- paste0(n, "_MP1")
      
        tmp <- rbind(tmp_mp, tmp_x)
      
        BEF_mp <- dplyr::bind_rows(BEF_mp, setNames(tmp, names(BEF_mp)))
      
        df <- tmp
      }
    
      BEF_mp <- BEF_mp |>
        dplyr::mutate(Beneficiary_SubClass = ifelse(is.na(Original_Beneficiary_SubClass) & !is.na(Beneficiary_SubClass_MP1),
                                                    Beneficiary_SubClass_MP1, Original_Beneficiary_SubClass),
                      Consider_Paragraph = ifelse(is.na(Original_Beneficiary_SubClass) & !is.na(Beneficiary_SubClass_MP1),
                                                  Consider_Paragraph + (as.numeric(a_s) * 10), Consider_Paragraph),
                      Ecosystem_SubClass = ifelse(is.na(Original_Ecosystem_SubClass) & !is.na(Ecosystem_SubClass_MP1),
                                                  Ecosystem_SubClass_MP1, Original_Ecosystem_SubClass),
                      Consider_Paragraph = ifelse(is.na(Original_Ecosystem_SubClass) & !is.na(Ecosystem_SubClass_MP1),
                                                  Consider_Paragraph + as.numeric(a_s), Consider_Paragraph),
                      FEGS_SubClass = ifelse(is.na(Original_FEGS_SubClass) & !is.na(FEGS_SubClass_MP1),
                                             FEGS_SubClass_MP1, Original_FEGS_SubClass),
                      Consider_Paragraph = ifelse(is.na(Original_FEGS_SubClass) & !is.na(FEGS_SubClass_MP1),
                                                  Consider_Paragraph + (as.numeric(a_s) * 100), Consider_Paragraph))
    
      BEF_mp1 <- BEF_mp |>
        dplyr::filter(Consider_Paragraph>0) |>
        dplyr::mutate(Matches = 3 - (is.na(Beneficiary_SubClass) + is.na(Ecosystem_SubClass) + is.na(FEGS_SubClass)))
      
      BEF_mp1_Done <- BEF_mp1 |>
        dplyr::filter(Matches==3) |>
        dplyr::select(Document, Line_Number, Sentence = Original_Sentence, Beneficiary_SubClass=Original_Beneficiary_SubClass, Ecosystem_SubClass=Original_Ecosystem_SubClass, FEGS_SubClass=Original_FEGS_SubClass, Paragraph_Text, Consider_Paragraph)
      BEF_mp1_Done <- unique(BEF_mp1_Done)
      
      BEF_mp1 <- BEF_mp1 |>
        dplyr::select(Document, Line_Number, Sentence = Original_Sentence, Beneficiary_SubClass, Ecosystem_SubClass, FEGS_SubClass, Paragraph_Text, Consider_Paragraph)

      BEF<-dplyr::anti_join(BEF,BEF_mp1_Done,by=c('Line_Number','Beneficiary_SubClass','Ecosystem_SubClass','FEGS_SubClass'))
      
      BEF <- unique(rbind(dplyr::select(BEF, -Matches), unique(BEF_mp1)))   |>   
        dplyr::mutate(Matches = 3 - (is.na(Beneficiary_SubClass) + is.na(Ecosystem_SubClass) + is.na(FEGS_SubClass)))
      
      BEF_Trips <- BEF |>
        dplyr::filter(Matches == 3)

    }}  ###loop over +1,+2


    
    
    
    res<-dplyr::select(BEF_Trips, 'Planning Doc'=Document,Beneficiary_SubClass,Ecosystem_SubClass,FEGS_SubClass,'Line Number'=Line_Number,'Text from Planning Doc'=Paragraph_Text) |> 
      tidyr::pivot_longer(cols=c("Beneficiary_SubClass","Ecosystem_SubClass","FEGS_SubClass"),names_to="Category",values_to = "SubClass") |>
      dplyr::left_join(unique(keywords[keywords$Word_type1=="include",c("Class","SubClass","Tier"),])) |>
      unique()
    tmp <- dplyr::filter(res,Tier==4) |>
      dplyr::select('Planning Doc', 'Line Number','Text from Planning Doc','Category',Class,SubClass)
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
                                          ifelse(is.na(tmp$'2_SubClass')==FALSE,tmp$'2_Class',tmp$Class))))
    
    BEF_tmp <- dplyr::tibble('Document' = character(), 'Line_Number' = integer(), Paragraph_Text = character())
    for (c in sort(unique(res$Category))) {
      tmp <- dplyr::filter(res, Category == c) |>
        dplyr::select(Document = `Planning Doc`, Line_Number = `Line Number`, Paragraph_Text = `Text from Planning Doc`, SubClass)
      colnames(tmp)[4] <- c
      BEF_tmp <- dplyr::full_join(BEF_tmp, tmp, relationship = "many-to-many")
      if(c == sort(unique(res$Category))[max(length(unique(res$Category)))]) {
        BEF_tmp <- dplyr::mutate(BEF_tmp, Matches = 3 - (is.na(Beneficiary_SubClass) + is.na(Ecosystem_SubClass) + is.na(FEGS_SubClass)))    
      }
    }
    
    BEF_Trips<-dplyr::left_join(BEF_tmp,BEF_Trips) |>
      dplyr::select("Document","Line_Number","Sentence","Beneficiary_SubClass","Ecosystem_SubClass","FEGS_SubClass","Paragraph_Text","Consider_Paragraph","Matches")
    
    
    
              
    Triplets <- unique(dplyr::select(BEF_Trips, -Matches)) |>
      dplyr::mutate(Beneficiary_SubClass = ifelse(is.na(Beneficiary_SubClass), "blank", Beneficiary_SubClass),
                    Ecosystem_SubClass = ifelse(is.na(Ecosystem_SubClass), "blank", Ecosystem_SubClass),
                    FEGS_SubClass = ifelse(is.na(FEGS_SubClass), "blank", FEGS_SubClass)) %>%
      dplyr::left_join(., dplyr::select(dplyr::filter(synonym_list, Word_type == "include"), Class, SubClass), by = c("Beneficiary_SubClass" = "SubClass")) %>%
      dplyr::left_join(., dplyr::select(dplyr::filter(synonym_list, Word_type == "include"), Class, SubClass), by = c("Ecosystem_SubClass" = "SubClass")) %>%
      dplyr::left_join(., dplyr::select(dplyr::filter(synonym_list, Word_type == "include"), Class, SubClass), by = c("FEGS_SubClass" = "SubClass")) |>
      dplyr::rename(bens_class1 = Class.x, eco_class1 = Class.y, fegs_class1 = Class)

    Triplets <- Triplets |>
      dplyr::mutate(FEGS_Fauna = ifelse(grepl("fauna_", Triplets$FEGS_SubClass, ignore.case = TRUE, perl = TRUE, useBytes = TRUE) & fegs_class1 == "Fauna",
                                        paste0(fegs_class1, ": ", stringr::str_remove(FEGS_SubClass, "[Ff]auna_")), NA),
                    FEGS_SubClass_New = ifelse(!is.na(FEGS_Fauna), FEGS_Fauna, FEGS_SubClass))
    
    Triplets_sc <- Triplets |>
      dplyr::select(Beneficiary_SubClass, Ecosystem_SubClass, FEGS_SubClass = FEGS_SubClass_New, Line_Number, Document, Sentence, Consider_Paragraph, Paragraph_Text) |>
      unique()
    
    (Triplets_sc <- Triplets_sc[order(paste(Triplets_sc$FEGS_SubClass, Triplets_sc$Line_Number, Triplets_sc$Ecosystem_SubClass, Triplets_sc$Beneficiary_SubClass)), ] |>
        dplyr::mutate(keep = 1))
  } else {
    BEF_Trips
  }
  
}



# Count triplets -----
count_triplets <- function(triplets_match) {
  trips_est <- triplets_match |>
    dplyr::group_by(Beneficiary_SubClass, Ecosystem_SubClass, FEGS_SubClass, Document) |>
    dplyr::summarise(Line_Number = length(Line_Number)) |>
    dplyr::ungroup()
  
  triplets_estuary <- trips_est |>
    dplyr::group_by(Beneficiary_SubClass, Ecosystem_SubClass, FEGS_SubClass) |>
    dplyr::summarise(Count = length(Line_Number),
                     Hits = sum(Line_Number)) |>
    dplyr::ungroup()
  
  triplets <- triplets_match %>%
    dplyr::mutate(Rando = rnorm(NROW(.), 100, 50))
  
  trips_max <- triplets |>
    dplyr::group_by(Beneficiary_SubClass, Ecosystem_SubClass, FEGS_SubClass) |>
    dplyr::summarise(Rando = max(Rando, na.rm = TRUE)) %>%
    dplyr::left_join(triplets_estuary, .)
  
  examples <- paste(triplets$Rando, triplets$Beneficiary_SubClass, triplets$Ecosystem_SubClass, triplets$FEGS_SubClass) %in%
    paste(trips_max$Rando, trips_max$Beneficiary_SubClass, trips_max$Ecosystem_SubClass, trips_max$FEGS_SubClass)
  
  examples <- triplets[examples == TRUE, ]
  
  triplets_estuary <- dplyr::left_join(triplets_estuary, dplyr::select(examples, Beneficiary_SubClass, Ecosystem_SubClass, FEGS_SubClass, Paragraph_Text)) |>
    dplyr::rename(Example = Paragraph_Text)
  
  triplets_estuary <- triplets_estuary[order(paste(triplets_estuary$FEGS_SubClass, triplets_estuary$Ecosystem_SubClass)), ] |>
    dplyr::mutate(FINAL = 1)
  
  triplets_estuary
}



# Read and clean the friendly names file -----
Friendly_Names_upd <- readr::read_csv("Friendly_Names_20240708.csv")



# Establish figure dimensions -----
dims <- dplyr::tibble(profile = c("ben", "ben", "fegs", "fegs"),
                      plot_type = c("bar", "pie", "bar", "pie"),
                      width = rep(c("90%"), 4),
                      height = c(900, 850, 1100, 850))



# Clean triplets -----
clean_triplets <- function(triplets_match, triplets_count, add_sent = 1) {
  AllTriplets <- dplyr::inner_join(dplyr::filter(triplets_match, FEGS_SubClass != "blank"),
                                   dplyr::select(triplets_count, Beneficiary_SubClass, Ecosystem_SubClass, FEGS_SubClass, Count, FINAL),
                                   by = c("Beneficiary_SubClass", "Ecosystem_SubClass", "FEGS_SubClass")) |>
    dplyr::filter(keep >= 0, FINAL >= 0, Beneficiary_SubClass != "blank", Ecosystem_SubClass != "blank", FEGS_SubClass != "blank")
  
  AllTriplets <- dplyr::left_join(AllTriplets, dplyr::select(Friendly_Names_upd, -c(Category, Class, NESCS_Code, Color_code, Pie_Color_code, Friendly_Name)),
                                  by = c("Beneficiary_SubClass" = "SubClass")) |>
    dplyr::rename(Ben_SubClass_Orig = Beneficiary_SubClass, DR_Ben_Class = DR_Class, DR_Ben_SubClass = DR_SubClass, FST_Ben_Class = FST_Class, FST_Ben_SubClass = FST_SubClass,
                  Ben_Tier = Tier) %>%
    dplyr::left_join(., dplyr::select(Friendly_Names_upd, -c(Category, Class, NESCS_Code, Color_code, Pie_Color_code, Friendly_Name)),
                     by = c("Ecosystem_SubClass" = "SubClass")) |>
    dplyr::rename(Eco_SubClass_Orig = Ecosystem_SubClass, DR_Eco_Class = DR_Class, DR_Eco_SubClass = DR_SubClass, FST_Eco_Class = FST_Class, FST_Eco_SubClass = FST_SubClass,
                  Eco_Tier = Tier) %>%
    dplyr::left_join(., dplyr::select(Friendly_Names_upd, -c(Category, Class, NESCS_Code, Color_code, Pie_Color_code, Friendly_Name)),
                     by = c("FEGS_SubClass" = "SubClass")) |>
    dplyr::rename(FEGS_SubClass_Orig = FEGS_SubClass, DR_FEGS_Class = DR_Class, DR_FEGS_SubClass = DR_SubClass, FST_FEGS_Class = FST_Class, FST_FEGS_SubClass = FST_SubClass,
                  FEGS_Tier = Tier) |>
    dplyr::filter(!is.na(DR_Ben_SubClass) & !is.na(DR_Eco_SubClass) & !is.na(DR_FEGS_SubClass)) |>
    dplyr::mutate(Document = sub(".*PdfFiles/", "", Document))
  
  AllTriplets
}



# Ecosystem frequencies by document -----
eco_freq <- function(triplets_clean) {
  trips_all <- unique(dplyr::select(triplets_clean, DR_Eco_SubClass, Document, Line_Number))
  hits_per_eco <- trips_all |>
    dplyr::mutate(dplyr::across(DR_Eco_SubClass:Document, ~as.factor(.x))) |>
    dplyr::group_by(DR_Eco_SubClass, Document, .drop = FALSE) |>
    dplyr::summarise(HitsPerEco = length(Line_Number)) |>
    dplyr::ungroup()
  
  hits_per_doc <- hits_per_eco |>
    dplyr::group_by(Document, .drop = FALSE) |>
    dplyr::summarise(HitsPerDoc = sum(HitsPerEco, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  ecos_freq_docs <- dplyr::left_join(hits_per_eco, hits_per_doc) |>
    dplyr::mutate(ecos_score = ifelse(HitsPerDoc > 0, (HitsPerEco / HitsPerDoc) * 100, NA))
  
  qc_check <- ecos_freq_docs |>
    dplyr::group_by(Document) |>
    dplyr::summarise(Total = sum(ecos_score, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 100), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Total score is equal to 100%!")
  }
  
  ecos_freq_docs
}



# Mean ecosystem frequencies -----
mean_eco_freq <- function(eco_freqs) {
  ecos_freq_mean <- eco_freqs |>
    dplyr::filter(HitsPerDoc > 0) |>
    dplyr::group_by(DR_Eco_SubClass) |>
    dplyr::summarise(Mean_Ecos_Score = mean(ecos_score, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  qc_check <- ecos_freq_mean |>
    dplyr::summarise(Total = sum(Mean_Ecos_Score, na.rm = TRUE)) |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 100), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Total score is equal to 100%!")
  }
  
  ecos_freq_mean <- ecos_freq_mean |>
    dplyr::mutate(DR_Eco_SubClass = factor(DR_Eco_SubClass, levels = unique(Friendly_Names_upd$DR_SubClass[Friendly_Names_upd$Category == "Environment"]))) |>
    tidyr::complete(DR_Eco_SubClass, fill = list(Mean_Ecos_Score = 0)) %>%
    dplyr::left_join(., unique(dplyr::select(Friendly_Names_upd, DR_Class, DR_SubClass, Color_code)), by = c("DR_Eco_SubClass" = "DR_SubClass")) |>
    dplyr::mutate(DR_Eco_SubClass = factor(DR_Eco_SubClass, levels = unique(Friendly_Names_upd$DR_SubClass[Friendly_Names_upd$Category == "Environment"])),
                  LabText = ifelse(DR_Eco_SubClass %in% c("Aquatic Ecosystems in General", "Terrestrial Ecosystems in General"),
                                   paste0("<span style = 'font-size: 12pt'>**", DR_Eco_SubClass, "**</span>"),
                                   ifelse(DR_Eco_SubClass %in% c("Open Water in General", "Wetlands in General", "Forests in General", "Agroecosystems", "Grasslands",
                                                                 "Scrublands and Shrublands", "Tundra in General", "Ice and Snow", "Urban Suburban Greenspace", "Barren Rock and Sand"),
                                          paste0("<span style = 'font-size: 12pt'>*", DR_Eco_SubClass, "*</span>"),
                                          paste0("<span style = 'font-size: 11pt'>", DR_Eco_SubClass, "</span>")))) |>
    dplyr::arrange(DR_Eco_SubClass)
  
  ecos_freq_mean
}



# Ecosystem frequency figure -----
eco_bars <- function(eco_freqs) {
  ggplot2::ggplot(eco_freqs, ggplot2::aes(x = reorder(LabText, sort(seq(NROW(eco_freqs)), decreasing = TRUE)), y = Mean_Ecos_Score)) +
    ggplot2::geom_bar(position = "stack", stat = "identity", ggplot2::aes(fill = LabText)) +
    ggplot2::scale_fill_manual(values = dplyr::arrange(unique(dplyr::select(eco_freqs, LabText, Color_code)), LabText)$Color_code) +
    ggplot2::xlab("") +
    ggplot2::ylab("Average Frequency (%)\nPer Document") +
    ggplot2::theme_bw() +
    ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                   axis.title.x = ggplot2::element_text(size = 13, face = "bold"),
                   axis.text.y = ggtext::element_markdown(),
                   axis.text.x = ggplot2::element_text(size = 12),
                   legend.position = "none",
                   plot.title = ggplot2::element_blank()) +
    ggplot2::scale_y_continuous(limits = c(0, 101), expand = c(0, 0)) +
    ggplot2::coord_flip()
}



# Specific ecosystem pie charts -----
eco_pies <- function(eco_freqs) {
  eco_freq_class <- eco_freqs |>
    dplyr::group_by(DR_Class) |>
    dplyr::summarize(Eco_Score = sum(Mean_Ecos_Score, na.rm = TRUE)) |>
    dplyr::mutate(LabPos = ifelse(is.na(Eco_Score / 2 + dplyr::lead(rev(cumsum(rev(Eco_Score))), 1)),
                                  Eco_Score / 2, Eco_Score / 2 + dplyr::lead(rev(cumsum(rev(Eco_Score))), 1)),
                  LabText = ifelse(stringr::str_detect(DR_Class, "in General"), gsub(" in General", "\nin General", DR_Class),
                                   ifelse(stringr::str_detect(DR_Class, "Aquatic: "), gsub("Aquatic: ", "Aquatic:\n", DR_Class),
                                          ifelse(DR_Class == "Terrestrial: Barren Rock and Sand", "Terrestrial: Barren\nRock and Sand",
                                                 ifelse(DR_Class == "Terrestrial: Scrublands and Shrublands", "Terrestrial: Scrublands\nand Shrublands",
                                                        ifelse(DR_Class == "Terrestrial: Urban Suburban Greenspace", "Terrestrial: Urban\nSuburban Greenspace",
                                                               ifelse(stringr::str_detect(DR_Class, "Terrestrial: "), gsub("Terrestrial: ", "Terrestrial:\n", DR_Class),
                                                                      DR_Class))))))) |>
    dplyr::ungroup() %>%
    dplyr::left_join(., dplyr::distinct(dplyr::select(Friendly_Names_upd, DR_Class, Pie_Color_code)))
  
  eco_freq_class |>
    ggplot2::ggplot(ggplot2::aes(x = "", y = Eco_Score)) +
    ggplot2::geom_bar(stat = "identity", color = "black", linewidth = 0.75, ggplot2::aes(fill = DR_Class)) +
    ggplot2::scale_fill_manual(values = eco_freq_class$Pie_Color_code) +
    ggplot2::coord_polar("y", start = 0) +
    ggplot2::theme_void() +
    ggrepel::geom_label_repel(ggplot2::aes(y = LabPos, label = paste0(LabText, " (", sprintf("%.1f", Eco_Score), "%", ")")), size = 4, nudge_x = 1, force = 50) +
    ggplot2::theme(plot.title = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.spacing = ggplot2::unit(0, "lines"),
                   plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"))
}



# Beneficiary frequencies by ecosystem -----
ben_freq <- function(triplets_clean) {
  trips_all_ecos <- triplets_clean |>
    dplyr::mutate(DR_Eco_SubClass = "All Ecosystems") |>
    dplyr::select(DR_Eco_SubClass, DR_Ben_SubClass, Document, Line_Number) |>
    unique()
  
  trips_all <- rbind(trips_all_ecos, unique(dplyr::select(triplets_clean, DR_Eco_SubClass, DR_Ben_SubClass, Document, Line_Number)))
  
  ind_ben_freq <- dplyr::tibble()
  
  for(e in unique(trips_all$DR_Eco_SubClass)) {
    hits_per_ben <- trips_all |>
      dplyr::filter(DR_Eco_SubClass == e) |>
      dplyr::mutate(dplyr::across(DR_Eco_SubClass:Document, ~as.factor(.x))) |>
      dplyr::group_by(DR_Eco_SubClass, DR_Ben_SubClass, Document, .drop = FALSE) |>
      dplyr::summarise(HitsPerBen = length(Line_Number)) |>
      dplyr::ungroup()
    
    hits_per_doc <- hits_per_ben |>
      dplyr::group_by(DR_Eco_SubClass, Document, .drop = FALSE) |>
      dplyr::summarise(HitsPerDoc = sum(HitsPerBen, na.rm = TRUE)) |>
      dplyr::ungroup()
    
    bens_freq_docs <- dplyr::left_join(hits_per_ben, hits_per_doc) |>
      dplyr::mutate(beneficiary_score = ifelse(HitsPerDoc > 0, (HitsPerBen / HitsPerDoc) * 100, NA))
    
    ind_ben_freq <- rbind(ind_ben_freq, bens_freq_docs)
  }
  
  qc_check <- ind_ben_freq |>
    dplyr::group_by(DR_Eco_SubClass, Document) |>
    dplyr::summarise(Total = sum(beneficiary_score, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 100), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Total score is equal to 100%!")
  }
  
  ind_ben_freq
}



# Mean beneficiary frequencies by ecosystem -----
mean_ben_freq <- function(ben_freqs) {
  bens_freq_mean <- ben_freqs |>
    dplyr::filter(HitsPerDoc > 0) |>
    dplyr::group_by(DR_Eco_SubClass, DR_Ben_SubClass) |>
    dplyr::summarise(Mean_Ben_Score = mean(beneficiary_score, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  qc_check <- bens_freq_mean |>
    dplyr::group_by(DR_Eco_SubClass) |>
    dplyr::summarise(Total = sum(Mean_Ben_Score, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 100), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Total score is equal to 100%!")
  }
   
  bens_freq_mean <- bens_freq_mean |>
    dplyr::mutate(DR_Eco_SubClass = factor(DR_Eco_SubClass, levels = c("All Ecosystems", unique(Friendly_Names_upd$DR_SubClass[Friendly_Names_upd$Category == "Environment"]))),
                  DR_Ben_SubClass = factor(DR_Ben_SubClass, levels = unique(dplyr::arrange(dplyr::mutate(dplyr::filter(Friendly_Names_upd, Category == "Beneficiary"),
                                                                                                         DR_Class = factor(DR_Class, levels = unique(DR_Class))),
                                                                                           DR_Class, Tier)$DR_SubClass))) |>
    tidyr::complete(DR_Eco_SubClass, DR_Ben_SubClass, fill = list(Mean_Ben_Score = 0)) %>%
    dplyr::left_join(., unique(dplyr::select(Friendly_Names_upd, DR_Class, DR_SubClass, NESCS_Code, Color_code)), by = c("DR_Ben_SubClass" = "DR_SubClass"), multiple = "first") |>
    dplyr::mutate(DR_Ben_SubClass = factor(DR_Ben_SubClass, levels = unique(dplyr::arrange(dplyr::mutate(dplyr::filter(Friendly_Names_upd, Category == "Beneficiary"),
                                                                                                         DR_Class = factor(DR_Class, levels = unique(DR_Class))),
                                                                                           DR_Class, Tier)$DR_SubClass)),
                  LabText = ifelse(DR_Ben_SubClass %in% c("Agriculture in General", "Commercial & Industrial in General", "Government / Municipal / Residential in General",
                                                          "Transportation & Shipping in General", "Subsistence in General", "Recreation in General", "Inspirational in General",
                                                          "Learning in General", "People Who Care In General","All Humans"),
                                   paste0("<span style = 'font-size: 12pt'>**", DR_Ben_SubClass, "**</span>"),
                                   paste0("<span style = 'font-size: 11pt'>", DR_Ben_SubClass, "</span>")))
  
  bens_freq_mean
}



# Beneficiary prioritization by ecosystem bar charts -----
eco_ben_bars <- function(ben_freqs) {
  ben_bar_list <- list()
  
  for (e in c("All Ecosystems", unique(Friendly_Names_upd$DR_SubClass[Friendly_Names_upd$Category == "Environment"]))) {
    tmp <- dplyr::filter(ben_freqs, DR_Eco_SubClass == e, DR_Class != "Unknown Beneficiary") %>%
      ggplot2::ggplot(ggplot2::aes(x = reorder(LabText, sort(seq(NROW(.)), decreasing = TRUE)), y = Mean_Ben_Score)) +
      ggplot2::geom_bar(position = "stack", stat = "identity", ggplot2::aes(fill = DR_Class)) +
      ggplot2::scale_fill_manual(values = dplyr::arrange(unique(dplyr::select(dplyr::filter(ben_freqs, DR_Eco_SubClass == e, DR_Class != "Unknown Beneficiary"),
                                                                              DR_Class, Color_code)), DR_Class)$Color_code) +
      ggplot2::xlab("") +
      ggplot2::ylab("Average Frequency (%)\nPer Document") +
      ggplot2::theme_bw() +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Beneficiary", direction = "horizontal", title.position = "top", ncol = 3)) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(size = 13, face = "bold"),
                     axis.text.y = ggtext::element_markdown(),
                     axis.text.x = ggplot2::element_text(size = 12),
                     legend.title = ggplot2::element_text(size = 14, face = "bold"),
                     legend.text = ggplot2::element_text(size = 12),
                     legend.position = "top") +
      ggplot2::scale_y_continuous(limits = c(0, 101), expand = c(0, 0), sec.axis = ggplot2::dup_axis()) +
      ggplot2::coord_flip()
    ben_bar_list[[e]] <- tmp
  }
  ben_bar_list
}



# Beneficiary prioritization by ecosystem pie charts -----
eco_ben_pies <- function(ben_freqs) {
  ben_freq_class <- ben_freqs |>
    dplyr::mutate(DR_Class = ifelse(DR_Ben_SubClass == "All Humans", "All Humans", DR_Class)) |>
    dplyr::group_by(DR_Eco_SubClass, DR_Class) |>
    dplyr::summarise(Mean_Ben_Score = sum(Mean_Ben_Score, na.rm = TRUE)) %>%
    dplyr::left_join(., unique(dplyr::select(Friendly_Names_upd, DR_Class, Color_code))) |>
    dplyr::filter(Mean_Ben_Score > 0) |>
    dplyr::mutate(LabPos = ifelse(is.na(Mean_Ben_Score / 2 + dplyr::lead(rev(cumsum(rev(Mean_Ben_Score))), 1)),
                                  Mean_Ben_Score / 2, Mean_Ben_Score / 2 + dplyr::lead(rev(cumsum(rev(Mean_Ben_Score))), 1)),
                  LabText = ifelse(DR_Class == "Commerical & Industrial", "Commercial &\nIndustrial",
                                   ifelse(DR_Class == "Government / Municipal / Residential", "Government / Municipal /\nResidential",
                                          ifelse(DR_Class == "People Who Care", "People Who\nCare",
                                                 ifelse(DR_Class == "Subsistence Users", "Subsistence\nUsers",
                                                        ifelse(DR_Class == "Transportation & Shipping", "Transportation &\nShipping",
                                                               ifelse(DR_Class == "Commercial & Industrial", "Commercial &\nIndustrial", DR_Class)))))),
                  ) |>
    dplyr::arrange(DR_Eco_SubClass, DR_Class) |>
    dplyr::ungroup()
  
  ben_pie_list <- list()
  
  for (e in c("All Ecosystems", unique(Friendly_Names_upd$DR_SubClass[Friendly_Names_upd$Category == "Environment"]))) {
    tmp <- dplyr::filter(ben_freq_class, DR_Eco_SubClass == e, DR_Class != "Unknown Beneficiary") |>
      ggplot2::ggplot(ggplot2::aes(x = "", y = Mean_Ben_Score)) +
      ggplot2::geom_bar(stat = "identity", color = "black", linewidth = 0.75, ggplot2::aes(fill = DR_Class)) +
      ggplot2::scale_fill_manual(values = dplyr::arrange(unique(dplyr::select(dplyr::filter(ben_freq_class, DR_Eco_SubClass == e, DR_Class != "Unknown Beneficiary"),
                                                                              DR_Class, Color_code)), DR_Class)$Color_code) +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::theme_void() +
      ggrepel::geom_label_repel(ggplot2::aes(y = LabPos, label = paste0(LabText, " (", sprintf("%.1f", Mean_Ben_Score), "%", ")")), size = 4, nudge_x = 1, force = 50) +
      ggplot2::theme(plot.title = ggplot2::element_blank(),
                     legend.position = "none",
                     panel.spacing = ggplot2::unit(0, "lines"),
                     plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"))
    ben_pie_list[[e]] <- tmp
  }
  ben_pie_list
}



# FEGS frequencies by beneficiary -----
fegs_freq <- function(triplets_clean) {
  trips_all_ecos <- triplets_clean |>
    dplyr::mutate(DR_Eco_SubClass = "All Ecosystems") |>
    dplyr::select(DR_Eco_SubClass, DR_Ben_SubClass, DR_FEGS_SubClass, Document, Line_Number) |>
    unique()
  
  trips_all <- rbind(trips_all_ecos, unique(dplyr::select(triplets_clean, DR_Eco_SubClass, DR_Ben_SubClass, DR_FEGS_SubClass, Document, Line_Number)))
  
  ind_fegs_freq <- dplyr::tibble()
  
  for(e in unique(trips_all$DR_Eco_SubClass)) {
    hits_per_fegs <- trips_all |>
      dplyr::filter(DR_Eco_SubClass == e) |>
      dplyr::mutate(dplyr::across(DR_Eco_SubClass:Document, ~as.factor(.x))) |>
      dplyr::group_by(DR_Eco_SubClass, DR_Ben_SubClass, DR_FEGS_SubClass, Document, .drop = FALSE) |>
      dplyr::summarise(HitsPerFEG = length(Line_Number)) |>
      dplyr::ungroup()
    
    hits_per_doc <- hits_per_fegs |>
      dplyr::group_by(DR_Eco_SubClass, DR_Ben_SubClass, Document, .drop = FALSE) |>
      dplyr::summarise(HitsPerDoc = sum(HitsPerFEG, na.rm = TRUE)) |>
      dplyr::ungroup()
    
    fegs_freq_docs <- dplyr::left_join(hits_per_fegs, hits_per_doc) |>
      dplyr::mutate(fegs_score = ifelse(HitsPerDoc > 0, (HitsPerFEG / HitsPerDoc) * 100, NA))
    
    ind_fegs_freq <- rbind(ind_fegs_freq, fegs_freq_docs)
  }
  
  qc_check <- ind_fegs_freq |>
    dplyr::group_by(DR_Eco_SubClass, DR_Ben_SubClass, Document) |>
    dplyr::summarise(Total = sum(fegs_score, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 100), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Total score is equal to 100%!")
  }
  
  ind_fegs_freq
}



# Mean weighted FEGS frequencies by beneficiary -----
fegs_wgt_mean <- function(fegs_freqs, ben_freqs) {
  wgt_fegs_freq <- dplyr::left_join(fegs_freqs, dplyr::select(ben_freqs, Document, DR_Eco_SubClass, DR_Ben_SubClass, beneficiary_score)) |>
    dplyr::mutate(wgt_fegs_score = ifelse(is.na((beneficiary_score / 100) * fegs_score), 0, (beneficiary_score / 100) * fegs_score))
  
  qc_check <- wgt_fegs_freq |>
    dplyr::group_by(DR_Eco_SubClass, Document) |>
    dplyr::summarise(Total = sum(wgt_fegs_score, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 100), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Total score is equal to 100%!")
  }
  
  wgt_fegs_mean <- wgt_fegs_freq |>
    dplyr::group_by(DR_Eco_SubClass, DR_Ben_SubClass, DR_FEGS_SubClass, .drop = FALSE) |>
    dplyr::summarise(Mean_FEGS_Score = mean(wgt_fegs_score, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  wgt_fegs_mean <- wgt_fegs_mean |>
    dplyr::mutate(DR_Eco_SubClass = factor(DR_Eco_SubClass, levels = c("All Ecosystems", unique(Friendly_Names_upd$DR_SubClass[Friendly_Names_upd$Category == "Environment"]))),
                  DR_Ben_SubClass = factor(DR_Ben_SubClass, levels = unique(dplyr::arrange(dplyr::mutate(dplyr::filter(Friendly_Names_upd, Category == "Beneficiary"),
                                                                                                         DR_Class = factor(DR_Class, levels = unique(DR_Class))),
                                                                                           DR_Class, Tier)$DR_SubClass)),
                  DR_FEGS_SubClass = factor(DR_FEGS_SubClass, levels = unique(Friendly_Names_upd$DR_SubClass[Friendly_Names_upd$Category == "FEGS"]))) |>
    tidyr::complete(DR_Eco_SubClass, DR_Ben_SubClass, DR_FEGS_SubClass, fill = list(Mean_FEGS_Score = 0)) %>%
    dplyr::left_join(., unique(dplyr::select(Friendly_Names_upd, DR_Class, DR_SubClass, Color_code)), by = c("DR_Ben_SubClass" = "DR_SubClass"), multiple = "first") %>%
    dplyr::left_join(., unique(dplyr::select(Friendly_Names_upd, DR_Class, DR_SubClass)), by = c("DR_FEGS_SubClass" = "DR_SubClass"), multiple = "first") |>
    dplyr::rename(DR_Ben_Class = DR_Class.x, DR_FEGS_Class = DR_Class.y) |>
    dplyr::mutate(LabText = ifelse(DR_FEGS_SubClass %in% c("Atmosphere in General", "Soil & Substrate in General", "Water in General", "Fauna in General", "Flora in General",
                                                           "Fungi in General", "Other Natural Components in General", "Composite in General"),
                                   paste0("<span style = 'font-size: 12pt'>**", DR_FEGS_SubClass, "**</span>"),
                                   ifelse(DR_FEGS_SubClass %in% c("Site Appeal in General", "Ecological Condition", "Open Space", "Mitigating Extreme Events in General"),
                                          paste0("<span style = 'font-size: 12pt'>*", DR_FEGS_SubClass, "*</span>"),
                                          paste0("<span style = 'font-size: 11pt'>", DR_FEGS_SubClass, "</span>"))))
  
  qc_check <- wgt_fegs_mean |>
    dplyr::group_by(DR_Eco_SubClass) |>
    dplyr::summarise(Total = sum(Mean_FEGS_Score, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 100), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Total score is equal to 100%!")
  }
  
  wgt_fegs_mean
}



# Final FEGS score -----
fegs_score <- function(fegs_freqs, ben_freqs) {
  wgt_fegs_freq <- dplyr::left_join(fegs_freqs, dplyr::select(ben_freqs, Document, DR_Eco_SubClass, DR_Ben_SubClass, beneficiary_score)) |>
    dplyr::mutate(wgt_fegs_score = ifelse(is.na((beneficiary_score / 100) * fegs_score), 0, (beneficiary_score / 100) * fegs_score))
  
  final_fegs_freq <- wgt_fegs_freq |>
    dplyr::group_by(Document, DR_Eco_SubClass, DR_FEGS_SubClass) |>
    dplyr::summarise(Final_FEGS_Score = sum(wgt_fegs_score, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  qc_check <- final_fegs_freq |>
    dplyr::group_by(DR_Eco_SubClass, Document) |>
    dplyr::summarise(Total = sum(Final_FEGS_Score, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 100), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Total score is equal to 100%!")
  }
  
  mean_final_fegs <- final_fegs_freq |>
    dplyr::group_by(DR_Eco_SubClass, DR_FEGS_SubClass) |>
    dplyr::summarise(Mean_FEGS_Score = mean(Final_FEGS_Score)) |>
    dplyr::ungroup() |>
    tidyr::complete(DR_Eco_SubClass, DR_FEGS_SubClass, fill = list(Mean_FEGS_Score = 0)) %>%
    dplyr::left_join(., unique(dplyr::select(Friendly_Names_upd, DR_Class, DR_SubClass, Color_code)), by = c("DR_FEGS_SubClass" = "DR_SubClass"))
  
  qc_check <- mean_final_fegs |>
    dplyr::group_by(DR_Eco_SubClass) |>
    dplyr::summarise(Total = sum(Mean_FEGS_Score, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 100), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Total score is equal to 100%!")
  }
  
  mean_final_fegs
}



# Environmental attributes prioritization by ecosystem bar chart -----
eco_fegs_bars <- function(fegs_freqs) {
  fegs_bar_list <- list()
  
  for (e in c("All Ecosystems", unique(Friendly_Names_upd$DR_SubClass[Friendly_Names_upd$Category == "Environment"]))) {
    tmp <- dplyr::filter(fegs_freqs, DR_Eco_SubClass == e, DR_Ben_Class != "Unknown Beneficiary") %>%
      ggplot2::ggplot(ggplot2::aes(x = reorder(LabText, sort(seq(NROW(.)), decreasing = TRUE)), y = Mean_FEGS_Score)) +
      ggplot2::geom_bar(position = "stack", stat = "identity", ggplot2::aes(fill = DR_Ben_Class)) +
      ggplot2::scale_fill_manual(values = dplyr::arrange(unique(dplyr::select(dplyr::filter(fegs_freqs, DR_Eco_SubClass == e, DR_Ben_Class != "Unknown Beneficiary"),
                                                                              DR_Ben_Class, DR_FEGS_SubClass, Color_code)), DR_FEGS_SubClass, DR_Ben_Class)$Color_code) +
      ggplot2::xlab("") +
      ggplot2::ylab("Average Frequency (%) By Beneficiary\nPer Document") +
      ggplot2::theme_bw() +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Beneficiary", direction = "horizontal", title.position = "top", ncol = 3)) +
      ggplot2::theme(axis.title.y = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(size = 13, face = "bold"),
                     axis.text.y = ggtext::element_markdown(),
                     axis.text.x = ggplot2::element_text(size = 12),
                     plot.title = ggplot2::element_blank(),
                     legend.title = ggplot2::element_text(size = 14, face = "bold"),
                     legend.text = ggplot2::element_text(size = 12),
                     legend.position = "top") +
      ggplot2::scale_y_continuous(limits = c(0, 101), expand = c(0, 0), sec.axis = ggplot2::dup_axis()) +
      ggplot2::coord_flip()
    fegs_bar_list[[e]] <- tmp
  }
  fegs_bar_list
}



# Environmental attributes prioritization by ecosystem pie chart -----
eco_fegs_pies <- function(fegs_scores) {
    fegs_score_class <- fegs_scores |>
    dplyr::group_by(DR_Eco_SubClass, DR_Class) |>
    dplyr::summarise(Mean_FEGS_Score = sum(Mean_FEGS_Score, na.rm = TRUE)) |>
    dplyr::filter(Mean_FEGS_Score > 0) |>
    dplyr::mutate(LabPos = ifelse(is.na(Mean_FEGS_Score / 2 + dplyr::lead(rev(cumsum(rev(Mean_FEGS_Score))), 1)),
                                  Mean_FEGS_Score / 2, Mean_FEGS_Score / 2 + dplyr::lead(rev(cumsum(rev(Mean_FEGS_Score))), 1)),
                  LabText = ifelse(DR_Class == "Composite in General", "Composite in\nGeneral",
                                   ifelse(DR_Class == "Composite: Ecological Condition", "Composite: Ecological\nCondition",
                                          ifelse(DR_Class == "Composite: Extreme Events", "Composite: Extreme\nEvents",
                                                 ifelse(DR_Class == "Composite: Site Appeal", "Composite: Site\nAppeal",
                                                        ifelse(DR_Class == "Other Natural Components", "Other Natural\nComponents",
                                                               ifelse(DR_Class == "Soil & Substrate", "Soil &\nSubstrate", DR_Class))))))) |>
    dplyr::ungroup() %>%
    dplyr::left_join(., unique(dplyr::select(Friendly_Names_upd, DR_Class, Color_code)))

  fegs_pie_list <- list()
  
  for (e in c("All Ecosystems", unique(Friendly_Names_upd$DR_SubClass[Friendly_Names_upd$Category == "Environment"]))) {
    tmp <- dplyr::filter(fegs_score_class, DR_Eco_SubClass == e) |>
      ggplot2::ggplot(ggplot2::aes(x = "", y = Mean_FEGS_Score)) +
      ggplot2::geom_bar(stat = "identity", color = "black", linewidth = 0.75, ggplot2::aes(fill = DR_Class)) +
      ggplot2::scale_fill_manual(values = dplyr::arrange(unique(dplyr::select(dplyr::filter(fegs_score_class, DR_Eco_SubClass == e), DR_Class, Color_code)), DR_Class)$Color_code) +
      ggplot2::coord_polar("y", start = 0) +
      ggplot2::theme_void() +
      ggrepel::geom_label_repel(ggplot2::aes(y = LabPos, label = paste0(LabText, " (", paste(sprintf("%.1f", Mean_FEGS_Score)), "%", ")")), size = 4, nudge_x = 1, force = 50) +
      ggplot2::guides(fill = ggplot2::guide_legend(title = "Ecosystem Goods/Services")) +
      ggplot2::theme(plot.title = ggplot2::element_blank(),
                     legend.position = "none")
    fegs_pie_list[[e]] <- tmp
  }
  fegs_pie_list
}



# Export results as an Excel file -----
export_wb <- function(triplets_clean, eco_freqs, ben_freqs, fegs_freqs, fegs_score) {
  wb <- openxlsx::createWorkbook()
  
  # Keyword match results
  trip_xpt <- dplyr::select(triplets_clean,
                            Document,
                            `Environment SubClass` = DR_Eco_SubClass,
                            `Beneficiary SubClass` = DR_Ben_SubClass,
                            `Attribute SubClass` = DR_FEGS_SubClass,
                            `Matched Document Text` = Paragraph_Text) |>
    unique() |>
    dplyr::arrange(Document, `Beneficiary SubClass`, `Attribute SubClass`, `Environment SubClass`)
  openxlsx::addWorksheet(wb, sheetName = "Sentences in Documents", gridLines = FALSE)
  openxlsx::writeDataTable(wb, sheet = 1, x = trip_xpt)
  
  # Ecosystem frequencies
  eco_xpt <- dplyr::select(eco_freqs,
                           `Environment Class` = DR_Class,
                           `Environment SubClass` = DR_Eco_SubClass,
                           `Mean Ecos Score` = Mean_Ecos_Score)
  openxlsx::addWorksheet(wb, sheetName = "Environment Frequency", gridLines = FALSE)
  openxlsx::writeDataTable(wb, sheet = 2, x = eco_xpt)
  
  # Beneficiary frequencies
  ben_xpt <- dplyr::select(ben_freqs,
                           `Environment SubClass` = DR_Eco_SubClass,
                           `Beneficiary Class` = DR_Class,
                           `Beneficiary SubClass` = DR_Ben_SubClass,
                           `Mean Ben Score` = Mean_Ben_Score)
  openxlsx::addWorksheet(wb, sheetName = "Beneficiary Profile", gridLines = FALSE)
  openxlsx::writeDataTable(wb, sheet = 3, x = ben_xpt)
  
  # FEGS by beneficiary frequencies
  fxb_xpt <- dplyr::select(fegs_freqs,
                           `Environment SubClass` = DR_Eco_SubClass,
                           `Beneficiary Class` = DR_Ben_Class,
                           `Beneficiary SubClass` = DR_Ben_SubClass,
                           `Attribute Class` = DR_FEGS_Class,
                           `Attribute SubClass` = DR_FEGS_SubClass,
                           `Mean Weighted FxB Score` = Mean_FEGS_Score)
  openxlsx::addWorksheet(wb, sheetName = "Attributes By Beneficiary", gridLines = FALSE)
  openxlsx::writeDataTable(wb, sheet = 4, x = fxb_xpt)
  
  # FEGS frequencies
  fegs_xpt <- dplyr::select(fegs_score,
                            `Environment SubClass` = DR_Eco_SubClass,
                            `Attribute Class` = DR_Class,
                            `Attribute SubClass` = DR_FEGS_SubClass,
                            `Mean FEGS Score` = Mean_FEGS_Score)
  openxlsx::addWorksheet(wb, sheetName = "Ecosystem Attributes Profile", gridLines = FALSE)
  openxlsx::writeDataTable(wb, sheet = 5, x = fegs_xpt)
  
  metadata <- dplyr::tibble(`Sheet Name` = c("Sentences in Documents", "Environment Frequency", "Beneficiary Profile", "Attributes By Beneficiary", "Ecosystem Attributes Profile",
                                             "", c(sort(unique(c(names(trip_xpt), names(eco_xpt), names(ben_xpt), names(fxb_xpt), names(fegs_xpt)))))),
                            Description = c("List of sentences in each document identified as matching an ecosystem, beneficiary, and ecosystem attribute.",
                                            "Mean relative frequency of Environment SubClasses in documents.",
                                            "Mean relative frequency (percent) of beneficiary subclasses in documents, for any ecosystem or in reference to a particular ecosystem.",
                                            "Mean relative frequency (percent) of Environment SubClasses mentioned in association with each beneficiary, for any ecosystem or in reference to a particular ecosystem.",
                                            "Mean relative frequency (percent) of Environment SubClasses across all beneficiaries, for any ecosystem or in reference to a particular ecosystem.",
                                            "",
                                            "Beneficiary Class in NESCS Plus",
                                            "Beneficiary SubClass in NESCS Plus",
                                            "Document File",
                                            "Environment Class in NESCS Plus",
                                            "Environment SubClass in NESCS Plus",
                                            "Ecosystem Attribute Class in NESCS Plus",
                                            "Ecosystem Attribute SubClass in NESCS Plus",
                                            "Sentence Text Matching Keywords of Associated Beneficiary, Ecosystem, and Ecosystem Attributes",
                                            "Mean relative frequency (percent) of beneficiary subclasses in documents, for any ecosystem or in reference to a particular ecosystem.",
                                            "Mean relative frequency of Environment SubClasses in documents.",
                                            "Mean relative frequency (percent) of Environment SubClasses across all beneficiaries, for any ecosystem or in reference to a particular ecosystem.",
                                            "Mean relative frequency (percent) of Environment SubClasses mentioned in association with each beneficiary, for any ecosystem or in reference to a particular ecosystem."))
  
  openxlsx::addWorksheet(wb, sheetName = "Metadata", gridLines = FALSE)
  openxlsx::writeData(wb, sheet = 6, x = metadata, withFilter = FALSE)
  
  wb
}



# Calculate the relative importance of non-tier 1 beneficiary groups -----
ben_relimp <- function(ben_freqs, terms) {
  ben_reconciled_terms <- terms |>
    dplyr::filter(Category == "Beneficiary") |>
    dplyr::mutate(FST_Class = factor(FST_Class, levels = unique(FST_Class)),
                  FST_SubClass = factor(FST_SubClass, levels = unique(FST_SubClass)),
                  Class_SubClass = paste(FST_Class, FST_SubClass, sep = ":"),
                  Class_SubClass = factor(Class_SubClass, levels = unique(Class_SubClass))) |>
    dplyr::select(DR_Ben_SubClass = DR_SubClass, FST_Ben_Class = FST_Class, FST_Ben_SubClass = FST_SubClass, Class_SubClass)
  
  ben_default_relimp <- ben_reconciled_terms |>
    dplyr::filter(!is.na(FST_Ben_SubClass)) |>
    dplyr::distinct(FST_Ben_Class, FST_Ben_SubClass) |>
    dplyr::group_by(FST_Ben_Class) |>
    dplyr::summarise(nSC = length(FST_Ben_SubClass)) |>
    dplyr::ungroup() |>
    dplyr::mutate(ben_default_relimp = 1 / nSC)
  
  ben_freqs_reconciled <- ben_freqs %>%
    dplyr::left_join(., ben_reconciled_terms) |>
    dplyr::select(DR_Eco_SubClass, Class_SubClass, Document, beneficiary_score) |>
    tidyr::complete(DR_Eco_SubClass, Class_SubClass, Document, fill = list(beneficiary_score = 0)) %>%
    dplyr::left_join(., dplyr::distinct(dplyr::select(ben_reconciled_terms, -DR_Ben_SubClass))) |>
    dplyr::select(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, Document, beneficiary_score) |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, Document) |>
    dplyr::summarise(beneficiary_score = sum(beneficiary_score, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  ben_sc_tot <- ben_freqs_reconciled |>
    dplyr::filter(!is.na(FST_Ben_SubClass)) |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class, Document) |>
    dplyr::summarise(ben_sc_tot = sum(beneficiary_score, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  qc_check <- ben_sc_tot |>
    dplyr::group_by(DR_Eco_SubClass, Document) |>
    dplyr::summarise(Total = sum(ben_sc_tot, na.rm = TRUE)) |>
    dplyr::ungroup() %>%
    dplyr::left_join(., ben_freqs |>
                       dplyr::mutate(DR_Ben_SubClass = as.character(DR_Ben_SubClass),
                                     DR_Ben_SubClass = ifelse(DR_Ben_SubClass == "All Humans", "Government / Municipal / Residential in General", DR_Ben_SubClass)) |>
                       dplyr::filter(!DR_Ben_SubClass %in% c("Agriculture in General", "Commercial & Industrial in General", "Government / Municipal / Residential in General",
                                                             "Transportation & Shipping in General", "Subsistence in General", "Recreation in General", "Inspirational in General",
                                                             "Learning in General", "People Who Care In General")) |>
                       dplyr::group_by(DR_Eco_SubClass, Document) |>
                       dplyr::summarise(Tot = sum(beneficiary_score, na.rm = TRUE)) |>
                       dplyr::ungroup()) |>
    tidyr::replace_na(list(Tot = 0)) |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, Tot), TRUE, FALSE))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Original and reconciled beneficiary scores are equal!")
  }
  
  ben_sc_relimp <- ben_freqs_reconciled |>
    dplyr::filter(!is.na(FST_Ben_SubClass)) %>%
    dplyr::left_join(., ben_sc_tot) |>
    dplyr::mutate(ben_sc_relimp = ifelse(dplyr::near(ben_sc_tot, 0), 0, beneficiary_score / ben_sc_tot))
  
  qc_check <- ben_sc_relimp |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class, Document) |>
    dplyr::summarise(Total = sum(ben_sc_relimp, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 1), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Relative importance of individual beneficiaries totals 1!")
  }
  
  ben_sc_tot_relimp <- ben_sc_relimp |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class, Document) |>
    dplyr::summarise(ben_sc_tot_relimp = sum(ben_sc_relimp, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  ben_sc_corr_relimp <- ben_sc_relimp %>%
    dplyr::left_join(., ben_sc_tot_relimp) %>%
    dplyr::left_join(., dplyr::select(ben_default_relimp, FST_Ben_Class, ben_default_relimp)) |>
    dplyr::mutate(ben_sc_corr_relimp = ifelse(dplyr::near(ben_sc_tot_relimp, 0), ben_default_relimp, ben_sc_relimp)) |>
    dplyr::select(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, Document, beneficiary_score, ben_sc_corr_relimp)
  
  qc_check <- ben_sc_corr_relimp |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class, Document) |>
    dplyr::summarise(Total = sum(ben_sc_corr_relimp, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 1), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Corrected relative importance of individual beneficiaries totals 1!")
  }
  
  ben_sc_corr_relimp
}



# Redistribute tier 1 beneficiary scores based on relative importance -----
ben_score_redist <- function(ben_freqs, relimp, terms) {
  ben_reconciled_terms <- terms |>
    dplyr::filter(Category == "Beneficiary") |>
    dplyr::mutate(FST_Class = factor(FST_Class, levels = unique(FST_Class)),
                  FST_SubClass = factor(FST_SubClass, levels = unique(FST_SubClass)),
                  Class_SubClass = paste(FST_Class, FST_SubClass, sep = ":"),
                  Class_SubClass = factor(Class_SubClass, levels = unique(Class_SubClass))) |>
    dplyr::select(DR_Ben_SubClass = DR_SubClass, FST_Ben_Class = FST_Class, FST_Ben_SubClass = FST_SubClass, Class_SubClass)
  
  ben_default_relimp <- ben_reconciled_terms |>
    dplyr::filter(!is.na(FST_Ben_SubClass)) |>
    dplyr::distinct(FST_Ben_Class, FST_Ben_SubClass) |>
    dplyr::group_by(FST_Ben_Class) |>
    dplyr::summarise(nSC = length(FST_Ben_SubClass)) |>
    dplyr::ungroup() |>
    dplyr::mutate(ben_default_relimp = 1 / nSC)
  
  ben_freqs_reconciled <- ben_freqs %>%
    dplyr::left_join(., ben_reconciled_terms) |>
    dplyr::select(DR_Eco_SubClass, Class_SubClass, Document, beneficiary_score) |>
    tidyr::complete(DR_Eco_SubClass, Class_SubClass, Document, fill = list(beneficiary_score = 0)) %>%
    dplyr::left_join(., dplyr::distinct(dplyr::select(ben_reconciled_terms, -DR_Ben_SubClass))) |>
    dplyr::select(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, Document, beneficiary_score) |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, Document) |>
    dplyr::summarise(beneficiary_score = sum(beneficiary_score, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  ben_tier1_tot <- ben_freqs_reconciled |>
    dplyr::filter(is.na(FST_Ben_SubClass)) |>
    dplyr::select(DR_Eco_SubClass, FST_Ben_Class, Document, ben_tier1_tot = beneficiary_score)
  
  qc_check <- ben_tier1_tot |>
    dplyr::group_by(DR_Eco_SubClass, Document) |>
    dplyr::summarise(Total = sum(ben_tier1_tot, na.rm = TRUE)) |>
    dplyr::ungroup() %>%
    dplyr::left_join(., ben_freqs |>
                       dplyr::mutate(DR_Ben_SubClass = as.character(DR_Ben_SubClass),
                                     DR_Ben_SubClass = ifelse(DR_Ben_SubClass == "All Humans", "Government / Municipal / Residential in General", DR_Ben_SubClass)) |>
                       dplyr::filter(DR_Ben_SubClass %in% c("Agriculture in General", "Commercial & Industrial in General", "Government / Municipal / Residential in General",
                                                            "Transportation & Shipping in General", "Subsistence in General", "Recreation in General", "Inspirational in General",
                                                            "Learning in General", "People Who Care In General")) |>
                       dplyr::group_by(DR_Eco_SubClass, Document) |>
                       dplyr::summarise(Tot = sum(beneficiary_score, na.rm = TRUE)) |>
                       dplyr::ungroup()) |>
    tidyr::replace_na(list(Tot = 0)) |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, Tot), TRUE, FALSE))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Original and reconciled beneficiary scores are equal!")
  }
  
  redist_ben_score <- dplyr::left_join(relimp, ben_tier1_tot) |>
    dplyr::mutate(redist_ben_score = beneficiary_score + (ben_sc_corr_relimp * ben_tier1_tot)) |>
    dplyr::select(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, Document, redist_ben_score)
  
  qc_check <- redist_ben_score |>
    dplyr::group_by(DR_Eco_SubClass, Document) |>
    dplyr::summarise(Total = sum(redist_ben_score, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 100), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Redistributed beneficiary scores are equal to 100%!")
  }
  
  qc_check <- redist_ben_score |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class, Document) |>
    dplyr::summarise(Total = sum(redist_ben_score, na.rm = TRUE)) |>
    dplyr::ungroup() %>%
    dplyr::left_join(., ben_freqs |>
                       dplyr::mutate(DR_Ben_SubClass = as.character(DR_Ben_SubClass),
                                     DR_Ben_SubClass = ifelse(DR_Ben_SubClass == "All Humans", "Government / Municipal / Residential in General", DR_Ben_SubClass)) %>%
                       dplyr::left_join(., ben_reconciled_terms) |>
                       dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class, Document) |>
                       dplyr::summarise(Tot = sum(beneficiary_score, na.rm = TRUE)) |>
                       dplyr::ungroup()) |>
    tidyr::replace_na(list(Tot = 0)) |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, Tot), TRUE, FALSE))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Original and redistributed beneficiary scores are equal!")
  }
  
  redist_ben_score
}



# Redistribute tier 1 beneficiaries' attribute scores based on relative importance -----
attr_redist_by_ben <- function(relimp, fegs_freqs, terms) {
  ben_reconciled_terms <- terms |>
    dplyr::filter(Category == "Beneficiary") |>
    dplyr::mutate(FST_Class = factor(FST_Class, levels = unique(FST_Class)),
                  FST_SubClass = factor(FST_SubClass, levels = unique(FST_SubClass)),
                  Class_SubClass = paste(FST_Class, FST_SubClass, sep = ":"),
                  Class_SubClass = factor(Class_SubClass, levels = unique(Class_SubClass))) |>
    dplyr::select(DR_Ben_SubClass = DR_SubClass, FST_Ben_Class = FST_Class, FST_Ben_SubClass = FST_SubClass, Class_SubClass)
  
  ben_relimp <- relimp |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass) |>
    dplyr::summarise(ben_relimp = mean(ben_sc_corr_relimp, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  qc_check <- ben_relimp |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class) |>
    dplyr::summarise(Total = sum(ben_relimp, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 1), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Corrected relative importance of individual beneficiaries totals 1!")
  }
  
  attr_reconciled_terms <- terms |>
    dplyr::filter(Category == "FEGS") |>
    dplyr::mutate(FST_Class = ifelse(DR_SubClass == "Site Appeal in General", "Composite (and Extreme Events)", FST_Class),
                  FST_Class = factor(FST_Class, levels = unique(FST_Class)),
                  FST_SubClass = ifelse(DR_SubClass == "Site Appeal in General", "Site Appeal in General", FST_SubClass),
                  FST_SubClass = factor(FST_SubClass, levels = unique(FST_SubClass)),
                  Class_SubClass = paste(FST_Class, FST_SubClass, sep = ":"),
                  Class_SubClass = factor(Class_SubClass, levels = unique(Class_SubClass))) |>
    dplyr::select(DR_FEGS_SubClass = DR_SubClass, FST_FEGS_Class = FST_Class, FST_FEGS_SubClass = FST_SubClass, Class_SubClass)
  
  fegs_freqs_reconciled <- fegs_freqs |>
    dplyr::select(-c(DR_Ben_Class:LabText)) |>
    tidyr::replace_na(list(fegs_score = 0)) %>%
    dplyr::left_join(., dplyr::select(ben_reconciled_terms, DR_Ben_SubClass, Ben_Class_SubClass = Class_SubClass)) |>
    dplyr::mutate(DR_FEGS_SubClass = as.character(DR_FEGS_SubClass),
                  DR_FEGS_SubClass = ifelse(DR_FEGS_SubClass == "Keystone Fungi", "Fungal Community", DR_FEGS_SubClass)) %>%
    dplyr::left_join(., dplyr::distinct(dplyr::select(attr_reconciled_terms, DR_FEGS_SubClass, FEGS_Class_SubClass = Class_SubClass))) |>
    dplyr::select(DR_Eco_SubClass, Ben_Class_SubClass, FEGS_Class_SubClass, Mean_FEGS_Score) |>
    dplyr::group_by(DR_Eco_SubClass, Ben_Class_SubClass, FEGS_Class_SubClass) |>
    dplyr::summarise(Mean_FEGS_Score = sum(Mean_FEGS_Score, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    tidyr::complete(DR_Eco_SubClass, Ben_Class_SubClass, FEGS_Class_SubClass, fill = list(Mean_FEGS_Score = 0)) %>%
    dplyr::left_join(., dplyr::distinct(dplyr::select(ben_reconciled_terms, FST_Ben_Class, FST_Ben_SubClass, Ben_Class_SubClass = Class_SubClass))) %>%
    dplyr::left_join(., dplyr::distinct(dplyr::select(attr_reconciled_terms, FST_FEGS_Class, FST_FEGS_SubClass, FEGS_Class_SubClass = Class_SubClass))) |>
    dplyr::select(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, FST_FEGS_Class, FST_FEGS_SubClass, Mean_FEGS_Score)
  
  ben_attr_redist <- fegs_freqs_reconciled |>
    dplyr::filter(!is.na(FST_Ben_SubClass)) %>%
    dplyr::left_join(., ben_relimp) %>%
    dplyr::left_join(., fegs_freqs_reconciled |>
                       dplyr::filter(is.na(FST_Ben_SubClass)) |>
                       dplyr::select(-FST_Ben_SubClass, ben_tier1_tot = Mean_FEGS_Score)) |>
    dplyr::mutate(add_to_attr = ben_relimp * ben_tier1_tot,
                  redist_bengen = Mean_FEGS_Score + add_to_attr) |>
    dplyr::select(-c(Mean_FEGS_Score:add_to_attr))
  
  fegs_score_downwgt <- ben_attr_redist |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_SubClass) |>
    dplyr::summarise(fegs_tot = sum(redist_bengen, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(downwgt = fegs_tot / 100)
  
  redist_attr_score <- dplyr::left_join(ben_attr_redist, dplyr::select(fegs_score_downwgt, -fegs_tot)) |>
    dplyr::mutate(fegs_score = ifelse(redist_bengen == 0, 0, redist_bengen / downwgt)) |>
    dplyr::select(-c(redist_bengen, downwgt))
  
  qc_check <- redist_attr_score |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_SubClass) |>
    dplyr::summarise(Total = sum(fegs_score, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 100), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Redistributed attribute scores are equal to 100%!")
  }
  
  redist_attr_score
}



# Redistribute attributes scores for "Site Appeal in General" to "Sounds", "Scents", "Viewscapes", and "Phenomena" based on their relative importance -----
site_appeal_redist <- function(corr_fegs, terms) {
  ben_reconciled_terms <- terms |>
    dplyr::filter(Category == "Beneficiary") |>
    dplyr::mutate(FST_Class = factor(FST_Class, levels = unique(FST_Class)),
                  FST_SubClass = factor(FST_SubClass, levels = unique(FST_SubClass)),
                  Class_SubClass = paste(FST_Class, FST_SubClass, sep = ":"),
                  Class_SubClass = factor(Class_SubClass, levels = unique(Class_SubClass))) |>
    dplyr::select(DR_Ben_SubClass = DR_SubClass, FST_Ben_Class = FST_Class, FST_Ben_SubClass = FST_SubClass, Class_SubClass)
  
  attr_reconciled_terms <- terms |>
    dplyr::filter(Category == "FEGS",
                  DR_SubClass %in% c("Environmental Aesthetics (Sounds)", "Environmental Aesthetics (Scents)", "Environmental Aesthetics (Viewscapes)",
                                     "Atmospheric Phenomena", "Site Appeal in General")) |>
    dplyr::mutate(FST_Class = factor(FST_Class, levels = unique(FST_Class)),
                  FST_SubClass = factor(FST_SubClass, levels = unique(FST_SubClass)),
                  Class_SubClass = ifelse(is.na(FST_SubClass), NA, paste(FST_Class, FST_SubClass, sep = ":")),
                  Class_SubClass = factor(Class_SubClass, levels = unique(Class_SubClass))) |>
    dplyr::select(DR_FEGS_SubClass = DR_SubClass, FST_FEGS_Class = FST_Class, FST_FEGS_SubClass = FST_SubClass, Class_SubClass)
  
  attr_default_relimp <- attr_reconciled_terms |>
    dplyr::distinct(FST_FEGS_Class, FST_FEGS_SubClass) |>
    dplyr::filter(!is.na(FST_FEGS_SubClass)) |>
    dplyr::group_by(FST_FEGS_Class) |>
    dplyr::summarise(nSC = length(FST_FEGS_SubClass)) |>
    dplyr::ungroup() |>
    dplyr::mutate(attr_default_relimp = 1 / nSC)
  
  fegs_freqs_reconciled <- corr_fegs |>
    dplyr::filter(FST_FEGS_SubClass %in% c("Sounds", "Scents", "Viewscapes", "Phenomena (e.g. Sunsets, Northern Lights, etc)", "Site Appeal in General")) |>
    dplyr::mutate(FST_FEGS_Class = factor(FST_FEGS_Class, levels = "Composite (and Extreme Events)"),
                  FST_FEGS_SubClass = as.character(FST_FEGS_SubClass),
                  FST_FEGS_SubClass = ifelse(FST_FEGS_SubClass == "Site Appeal in General", NA, FST_FEGS_SubClass),
                  FST_FEGS_SubClass = factor(FST_FEGS_SubClass, levels = c("Sounds", "Scents", "Viewscapes", "Phenomena (e.g. Sunsets, Northern Lights, etc)")))
  
  attr_sc_tot <- fegs_freqs_reconciled |>
    dplyr::filter(!is.na(FST_FEGS_SubClass)) |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, FST_FEGS_Class) |>
    dplyr::summarise(attr_sc_tot = sum(fegs_score, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  attr_sc_relimp <- fegs_freqs_reconciled |>
    dplyr::filter(!is.na(FST_FEGS_SubClass))  %>%
    dplyr::left_join(., attr_sc_tot) |>
    dplyr::mutate(attr_sc_relimp = ifelse(dplyr::near(attr_sc_tot, 0), 0, fegs_score / attr_sc_tot))
  
  qc_check <- attr_sc_relimp |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_SubClass, FST_FEGS_Class) |>
    dplyr::summarise(Total = sum(attr_sc_relimp, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 1), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Relative importance of individual attributes totals 1!")
  }
  
  attr_sc_tot_relimp <- attr_sc_relimp |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, FST_FEGS_Class) |>
    dplyr::summarise(attr_sc_tot_relimp = sum(attr_sc_relimp, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  attr_sc_corr_relimp <- dplyr::left_join(attr_sc_relimp, attr_sc_tot_relimp) %>%
    dplyr::left_join(., dplyr::select(attr_default_relimp, -nSC)) |>
    dplyr::mutate(attr_sc_corr_relimp = ifelse(dplyr::near(attr_sc_tot_relimp, 0), attr_default_relimp, attr_sc_relimp)) |>
    dplyr::select(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, FST_FEGS_Class, FST_FEGS_SubClass, fegs_score, attr_sc_corr_relimp)
  
  qc_check <- attr_sc_corr_relimp |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_SubClass, FST_FEGS_Class) |>
    dplyr::summarise(Total = sum(attr_sc_corr_relimp, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 1), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Corrected relative importance of individual attributes totals 1!")
  }
  
  attr_tier1_tot <- fegs_freqs_reconciled |>
    dplyr::filter(is.na(FST_FEGS_SubClass)) |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, FST_FEGS_Class) |>
    dplyr::summarise(fegs_score = sum(fegs_score, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::select(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, FST_FEGS_Class, attr_tier1_tot = fegs_score)
  
  redist_attr_score <- dplyr::left_join(attr_sc_corr_relimp, attr_tier1_tot) |>
    dplyr::mutate(fegs_score = fegs_score + (attr_sc_corr_relimp * attr_tier1_tot)) |>
    dplyr::select(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, FST_FEGS_Class, FST_FEGS_SubClass, fegs_score)
  
  qc_check <- redist_attr_score |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_SubClass) |>
    dplyr::summarise(Total = sum(fegs_score, na.rm = TRUE)) |>
    dplyr::ungroup() %>%
    dplyr::left_join(., corr_fegs |>
                       dplyr::filter(FST_FEGS_SubClass %in% c("Sounds", "Scents", "Viewscapes", "Phenomena (e.g. Sunsets, Northern Lights, etc)", "Site Appeal in General")) |>
                       dplyr::group_by(DR_Eco_SubClass, FST_Ben_SubClass) |>
                       dplyr::summarise(Tot = sum(fegs_score, na.rm = TRUE)) |>
                       dplyr::ungroup()) |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, Tot), TRUE, FALSE))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Original and redistributed attribute scores are equal!")
  }
  
  redist_attr_score
}



# Redistribute tier 1 attribute scores based on relative importance -----
attr_score_redist <- function(corr_fegs, site_redist, terms) {
  ben_reconciled_terms <- terms |>
    dplyr::filter(Category == "Beneficiary") |>
    dplyr::mutate(FST_Class = factor(FST_Class, levels = unique(FST_Class)),
                  FST_SubClass = factor(FST_SubClass, levels = unique(FST_SubClass)),
                  Class_SubClass = paste(FST_Class, FST_SubClass, sep = ":"),
                  Class_SubClass = factor(Class_SubClass, levels = unique(Class_SubClass))) |>
    dplyr::select(DR_Ben_SubClass = DR_SubClass, FST_Ben_Class = FST_Class, FST_Ben_SubClass = FST_SubClass, Class_SubClass)
  
  attr_reconciled_terms <- terms |>
    dplyr::filter(Category == "FEGS",
                  DR_SubClass != "Site Appeal in General") |>
    dplyr::mutate(FST_Class = factor(FST_Class, levels = unique(FST_Class)),
                  FST_SubClass = factor(FST_SubClass, levels = unique(FST_SubClass)),
                  Class_SubClass = ifelse(is.na(FST_SubClass), NA, paste(FST_Class, FST_SubClass, sep = ":")),
                  Class_SubClass = factor(Class_SubClass, levels = unique(Class_SubClass))) |>
    dplyr::select(DR_FEGS_SubClass = DR_SubClass, FST_FEGS_Class = FST_Class, FST_FEGS_SubClass = FST_SubClass, Class_SubClass)
  
  attr_default_relimp <- attr_reconciled_terms |>
    dplyr::distinct(FST_FEGS_Class, FST_FEGS_SubClass) |>
    dplyr::filter(!is.na(FST_FEGS_SubClass)) |>
    dplyr::group_by(FST_FEGS_Class) |>
    dplyr::summarise(nSC = length(FST_FEGS_SubClass)) |>
    dplyr::ungroup() |>
    dplyr::mutate(attr_default_relimp = 1 / nSC)
  
  fegs_freqs_reconciled <- corr_fegs |>
    dplyr::filter(!FST_FEGS_SubClass %in% c("Sounds", "Scents", "Viewscapes", "Phenomena (e.g. Sunsets, Northern Lights, etc)", "Site Appeal in General")) %>%
    rbind(., site_redist) |>
    dplyr::mutate(FST_FEGS_SubClass = factor(FST_FEGS_SubClass, levels = unique(FST_FEGS_SubClass)))
  
  attr_sc_tot <- fegs_freqs_reconciled |>
    dplyr::filter(!is.na(FST_FEGS_SubClass)) |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, FST_FEGS_Class) |>
    dplyr::summarise(attr_sc_tot = sum(fegs_score, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  attr_sc_relimp <- fegs_freqs_reconciled |>
    dplyr::filter(!is.na(FST_FEGS_SubClass)) %>%
    dplyr::left_join(., attr_sc_tot) |>
    dplyr::mutate(attr_sc_relimp = ifelse(dplyr::near(attr_sc_tot, 0), 0, fegs_score / attr_sc_tot))
  
  qc_check <- attr_sc_relimp |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_SubClass, FST_FEGS_Class) |>
    dplyr::summarise(Total = sum(attr_sc_relimp, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 1), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Relative importance of individual attributes totals 1!")
  }
  
  attr_sc_tot_relimp <- attr_sc_relimp |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, FST_FEGS_Class) |>
    dplyr::summarise(attr_sc_tot_relimp = sum(attr_sc_relimp, na.rm = TRUE)) |>
    dplyr::ungroup()
  
  attr_sc_corr_relimp <- dplyr::left_join(attr_sc_relimp, attr_sc_tot_relimp) %>%
    dplyr::left_join(., dplyr::select(attr_default_relimp, -nSC)) |>
    dplyr::mutate(attr_sc_corr_relimp = ifelse(dplyr::near(attr_sc_tot_relimp, 0), attr_default_relimp, attr_sc_relimp)) |>
    dplyr::select(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, FST_FEGS_Class, FST_FEGS_SubClass, fegs_score, attr_sc_corr_relimp)
  
  qc_check <- attr_sc_corr_relimp |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_SubClass, FST_FEGS_Class) |>
    dplyr::summarise(Total = sum(attr_sc_corr_relimp, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, 1), TRUE,
                                 ifelse(dplyr::near(Total, 0), TRUE, FALSE)))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Corrected relative importance of individual attributes totals 1!")
  }
  
  attr_tier1_tot <- fegs_freqs_reconciled |>
    dplyr::filter(is.na(FST_FEGS_SubClass)) |>
    dplyr::select(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, FST_FEGS_Class, attr_tier1_tot = fegs_score)
  
  redist_attr_score <- dplyr::left_join(attr_sc_corr_relimp, attr_tier1_tot) |>
    dplyr::mutate(redist_attr_score = fegs_score + (attr_sc_corr_relimp * attr_tier1_tot)) |>
    dplyr::select(DR_Eco_SubClass, FST_Ben_Class, FST_Ben_SubClass, FST_FEGS_Class, FST_FEGS_SubClass, redist_attr_score)
  
  qc_check <- redist_attr_score |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_SubClass) |>
    dplyr::summarise(Total = sum(redist_attr_score, na.rm = TRUE)) |>
    dplyr::ungroup() %>%
    dplyr::left_join(., corr_fegs |>
                       dplyr::group_by(DR_Eco_SubClass, FST_Ben_SubClass) |>
                       dplyr::summarise(Tot = sum(fegs_score, na.rm = TRUE)) |>
                       dplyr::ungroup()) |>
    dplyr::mutate(Check = ifelse(dplyr::near(Total, Tot), TRUE, FALSE))
  
  if(unique(qc_check$Check) == TRUE) {
    message("QC CHECK: Original and redistributed attribute scores are equal!")
  }
  
  redist_attr_score
}



# Convert beneficiary scores to FST export format -----
ben_fst_format <- function(ben_redist) {
  ben_reformat <- ben_redist |>
    dplyr::mutate(FST_format = ifelse(dplyr::near(redist_ben_score, 0),
                                      paste0('"', FST_Ben_SubClass, '":{"percentageOfStakeholder":""}'),
                                      paste0('"', FST_Ben_SubClass, '":{"percentageOfStakeholder":"', as.character(redist_ben_score), '"}'))) |>
    dplyr::group_by(DR_Eco_SubClass, Document) |>
    dplyr::summarise(bens_to_string = stringr::str_c(FST_format, collapse = ",")) |>
    dplyr::ungroup()
  
  ben_fst_format <- ben_reformat |>
    dplyr::mutate(doc_format = paste0('"', Document, '":{"scores":{"magnitude":1,"influence":1,"interest":1,"urgency":1,"proximity":1,"economic-interest":1,"rights":1,"fairness":1,"representation":1},"beneficiaries":{', bens_to_string, '},"noBenefit":false,"lastBeneficiaries":{}}')) |>
    dplyr::group_by(DR_Eco_SubClass) |>
    dplyr::summarise(ben_fst_format = stringr::str_c(doc_format, collapse = ",")) |>
    dplyr::mutate(ben_fst_format = paste0('"stakeholders":{', ben_fst_format, '}')) |>
    dplyr::ungroup()
  
  ben_fst_format
}



# Convert attribute scores to FST export format -----
fegs_fst_format <- function(fegs_redist) {
  fegs_redist |>
    dplyr::mutate(FST_format = ifelse(dplyr::near(redist_attr_score, 0),
                                      paste0('"', FST_FEGS_SubClass, '":{"percentageOfBeneficiary":""}'),
                                      paste0('"', FST_FEGS_SubClass, '":{"percentageOfBeneficiary":"', as.character(redist_attr_score), '"}'))) |>
    dplyr::group_by(DR_Eco_SubClass, FST_Ben_SubClass) |>
    dplyr::summarise(attr_to_string = stringr::str_c(FST_format, collapse = ",")) |>
    dplyr::ungroup() |>
    dplyr::mutate(ben_to_string = paste0('"', FST_Ben_SubClass, '":{', attr_to_string, '}')) |>
    dplyr::group_by(DR_Eco_SubClass) |>
    dplyr::summarise(attr_to_string = stringr::str_c(ben_to_string, collapse = ",")) |>
    dplyr::ungroup() |>
    dplyr::mutate(doc_format = paste0('"attributes":{', attr_to_string, '}'))
}



# Create the FST header -----
fst_header <- function() {
  criteria <- '"criteria":["magnitude","influence","interest","urgency","proximity","economic-interest","rights","fairness","representation"]'
  
  fegsCriteria <- '"fegsCriteria":["Magnitude & Probability of Impact","Level of Influence","Level of Interest","Urgency & Temporal Immediacy","Proximity","Economic Interest","Rights","Fairness","Underrepresented & Underserved Groups"]'
  
  criteriaMapOldToNew <- '"criteriaMapOldToNew":{"magnitude":"Magnitude & Probability of Impact","influence":"Level of Influence","interest":"Level of Interest","urgency":"Urgency & Temporal Immediacy","proximity":"Proximity","economic-interest":"Economic Interest","rights":"Rights","fairness":"Fairness","representation":"Underrepresented & Underserved Groups"}'
  
  criteriaMapNewToOld <- '"criteriaMapNewToOld":{"Magnitude & Probability of Impact":"magnitude","Level of Influence":"influence","Level of Interest":"interest","Urgency & Temporal Immediacy":"urgency","Proximity":"proximity","Economic Interest":"economic-interest","Rights":"rights","Fairness":"fairness","Underrepresented & Underserved Groups":"representation"}'
  
  fegsBeneficiaries <- '"fegsBeneficiaries":["Livestock Grazers","Agricultural Processors","Aquaculturalists","Farmers","Foresters","Food Extractors","Timber / Fiber / Ornamental Extractors","Industrial Processors","Energy Generators","Pharmaceutical / Food Supplement Suppliers","Fur / Hide Trappers / Hunters","Commercial Property Owners","Private Drinking Water Plant Operators","Municipal Drinking Water Plant Operators","Public Energy Generators","Residential Property Owners","Military / Coast Guard","Public Sector Property Owners","Transporters of Goods","Transporters of People","Water Subsisters","Food and Medicinal Subsisters","Timber / Fiber / Ornamental Subsisters","Building Material Subsisters","Experiencers / Viewers","Food Pickers / Gatherers","Hunters","Anglers","Waders / Swimmers / Divers","Boaters","Spiritual and Ceremonial Participants","Artists","Students and Educators","Researchers","People Who Care"]'
  
  fegsBeneficiariesTier1 <- '"fegsBeneficiariesTier1":{"Livestock Grazers":"Agricultural","Agricultural Processors":"Agricultural","Aquaculturalists":"Agricultural","Farmers":"Agricultural","Foresters":"Agricultural","Food Extractors":"Commercial / Industrial","Timber / Fiber / Ornamental Extractors":"Commercial / Industrial","Industrial Processors":"Commercial / Industrial","Energy Generators":"Commercial / Industrial","Pharmaceutical / Food Supplement Suppliers":"Commercial / Industrial","Fur / Hide Trappers / Hunters":"Commercial / Industrial","Commercial Property Owners":"Commercial / Industrial","Private Drinking Water Plant Operators":"Commercial / Industrial","Municipal Drinking Water Plant Operators":"Governmental / Municipal / Residential","Public Energy Generators":"Governmental / Municipal / Residential","Residential Property Owners":"Governmental / Municipal / Residential","Military / Coast Guard":"Governmental / Municipal / Residential","Public Sector Property Owners":"Governmental / Municipal / Residential","Transporters of Goods":"Transportation","Transporters of People":"Transportation","Water Subsisters":"Subsistence","Food and Medicinal Subsisters":"Subsistence","Timber / Fiber / Ornamental Subsisters":"Subsistence","Building Material Subsisters":"Subsistence","Experiencers / Viewers":"Recreational","Food Pickers / Gatherers":"Recreational","Hunters":"Recreational","Anglers":"Recreational","Waders / Swimmers / Divers":"Recreational","Boaters":"Recreational","Spiritual and Ceremonial Participants":"Inspirational","Artists":"Inspirational","Students and Educators":"Learning","Researchers":"Learning","People Who Care":"Non-Use"}'
  
  tier1 <- '"tier1":["Atmosphere","Soil","Water","Fauna","Flora","Fungi","Other Natural Components","Composite (and Extreme Events)"]'
  
  fegsAttributes <- '"fegsAttributes":["Air Quality","Wind Strength / Speed","Precipitation","Sunlight","Temperature","Soil Quality","Soil Quantity","Substrate Quality","Substrate Quantity","Water Quality","Water Quantity","Water Movement","Fauna Community","Edible Fauna","Medicinal Fauna","Keystone Fauna","Charismatic Fauna","Rare Fauna","Pollinating Fauna","Pest Predator / Depredator Fauna","Commercially Important Fauna","Spiritually / Culturally Important Fauna","Flora Community","Edible Flora","Medicinal Flora","Keystone Flora","Charismatic Flora","Rare Flora","Commercially Important Flora","Spiritually / Culturally Important Flora","Fungal Community","Edible Fungi","Medicinal Fungi","Rare Fungi","Commercially Important Fungi","Spiritually / Culturally Important Fungi","Fuel Quality","Fuel Quantity","Fiber Material Quality","Fiber Material Quantity","Mineral / Chemical Quality","Mineral / Chemical Quantity","Presence of Other Natural Materials for Artistic Use or Consumption (e.g. Shells, Acorns, Honey)","Sounds","Scents","Viewscapes","Phenomena (e.g. Sunsets, Northern Lights, etc)","Ecological Condition","Open Space","Flooding","Wildfire","Extreme Weather Events","Earthquakes"]'
  
  fegsAttributesTier1 <- '"fegsAttributesTier1":{"Air Quality":"Atmosphere","Wind Strength / Speed":"Atmosphere","Precipitation":"Atmosphere","Sunlight":"Atmosphere","Temperature":"Atmosphere","Soil Quality":"Soil","Soil Quantity":"Soil","Substrate Quality":"Soil","Substrate Quantity":"Soil","Water Quality":"Water","Water Quantity":"Water","Water Movement":"Water","Fauna Community":"Fauna","Edible Fauna":"Fauna","Medicinal Fauna":"Fauna","Keystone Fauna":"Fauna","Charismatic Fauna":"Fauna","Rare Fauna":"Fauna","Pollinating Fauna":"Fauna","Pest Predator / Depredator Fauna":"Fauna","Commercially Important Fauna":"Fauna","Spiritually / Culturally Important Fauna":"Fauna","Flora Community":"Flora","Edible Flora":"Flora","Medicinal Flora":"Flora","Keystone Flora":"Flora","Charismatic Flora":"Flora","Rare Flora":"Flora","Commercially Important Flora":"Flora","Spiritually / Culturally Important Flora":"Flora","Fungal Community":"Fungi","Edible Fungi":"Fungi","Medicinal Fungi":"Fungi","Rare Fungi":"Fungi","Commercially Important Fungi":"Fungi","Spiritually / Culturally Important Fungi":"Fungi","Fuel Quality":"Other Natural Components","Fuel Quantity":"Other Natural Components","Fiber Material Quality":"Other Natural Components","Fiber Material Quantity":"Other Natural Components","Mineral / Chemical Quality":"Other Natural Components","Mineral / Chemical Quantity":"Other Natural Components","Presence of Other Natural Materials for Artistic Use or Consumption (e.g. Shells, Acorns, Honey)":"Other Natural Components","Sounds":"Composite (and Extreme Events)","Scents":"Composite (and Extreme Events)","Viewscapes":"Composite (and Extreme Events)","Phenomena (e.g. Sunsets, Northern Lights, etc)":"Composite (and Extreme Events)","Ecological Condition":"Composite (and Extreme Events)","Open Space":"Composite (and Extreme Events)","Flooding":"Composite (and Extreme Events)","Wildfire":"Composite (and Extreme Events)","Extreme Weather Events":"Composite (and Extreme Events)","Earthquakes":"Composite (and Extreme Events)"}'
  
  appName <- '"appName":"FEGS Scoping Tool 1.8.0 | US EPA"'
  
  version <- '"version":"1.8.0"'
  
  projectName <- '"projectName":"New Project"'
  
  projectDescription <- '"projectDescription":""'
  
  notes <- '"notes":{"criteria":"","stakeholders":"","beneficiaries":"","attributes":""}'
  
  filePath <- '"filePath":""'
  
  scores <- '"scores":{"magnitude":"1","influence":"1","interest":"1","urgency":"1","proximity":"1","economic-interest":"1","rights":"1","fairness":"1","representation":"1"}'
  
  (header <- paste(criteria, fegsCriteria, criteriaMapOldToNew, criteriaMapNewToOld, fegsBeneficiaries,
                   fegsBeneficiariesTier1, tier1, fegsAttributes, fegsAttributesTier1, appName,
                   version, projectName, projectDescription, notes, filePath, scores, sep = ","))
}



# Save FST compatible beneficiary and attribute scores by ecosystem -----
fst_by_eco <- function(bens, wgt_attr, header) {
  fst_res <- list()
  for (e in unique(wgt_attr$DR_Eco_SubClass)) {
    ben <- dplyr::filter(bens, DR_Eco_SubClass == e)$ben_fst_format
    fegs <- dplyr::filter(wgt_attr, DR_Eco_SubClass == e)$doc_format
    fst_xpt <- paste(header, ben, fegs, sep = ",")
    fst_res[[e]] <- fst_xpt
  }
  
  fst_res
}


my_wordcloud <- function(triplets_clean,eco_dd)  {

    Sentences <- dplyr::select(triplets_clean,
                            Document,
                            `Environment SubClass` = DR_Eco_SubClass,
                            `Beneficiary SubClass` = DR_Ben_SubClass,
                            `Attribute SubClass` = DR_FEGS_SubClass,
                            `Matched Document Text` = Paragraph_Text) 
    if(eco_dd!="All Ecosystems"){ Sentences <- dplyr::filter(Sentences, `Environment SubClass`==eco_dd) }


    ######Cloud all words in All Tagged Sentences#####
  docs<-unique(Sentences$'Matched Document Text')
  docs<-unlist(strsplit(docs, "\\s"))
  docs<-tolower(docs)
  docs<-gsub("'","",docs,perl=TRUE)
  docs<-gsub("[^[:alnum:]]"," ",docs,perl=TRUE)
  docs<-stringr::str_trim(docs)
  docs<-gsub("[0-9]+","",docs,perl=TRUE)
  docs<-docs[(docs %in% tm::stopwords("english"))==FALSE]
  docs<-docs[docs!=""]
  docs<-docs[nchar(docs)>2]
  
  dtm <- dplyr::tibble("Sentences"=docs,"Count"=rep(1,NROW(docs))) |>
    dplyr::group_by(Sentences) |>
    dplyr::summarise(Count = sum(Count))
  dtm<-dtm[order(dtm$Count,decreasing=TRUE),]
  
  my_size<-min(0.8,(1/2000)*NROW(dtm)+0.5)
  
  my_wordcloud<-wordcloud2::wordcloud2(data=dtm, shape='circle',size=my_size, color=c(rep(RColorBrewer::brewer.pal(8, "Dark2"),each=5),rep(RColorBrewer::brewer.pal(9, "Set1")[-6],each=5)),shuffle=FALSE,ellipticity = 0.95)
  return(my_wordcloud)  
}

