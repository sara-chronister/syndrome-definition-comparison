## Functions to clean different free text variables

library(tidytext);library(dplyr)

#### ChiefComplaintUpdates ####
clean_ChiefComplaintUpdates <- function(data = my_file) {
  data2 <- data %>%
    dplyr::select(ChiefComplaintUpdates) %>%
    mutate(
      # clean punctuation and convert all letters to upper case
      ChiefComplaintUpdates = str_replace_all(ChiefComplaintUpdates, "[[:cntrl:]]|<BR>|[?.!???'+):@]|\\|", ""),
      ChiefComplaintUpdates = str_replace_all(ChiefComplaintUpdates,"\\{[[:digit:]]\\}", ""),
      ChiefComplaintUpdates = str_replace_all(ChiefComplaintUpdates,";|\\\\|\\/", " "),
      ChiefComplaintUpdates = str_trim(ChiefComplaintUpdates, side = "both"),
      ChiefComplaintUpdates = toupper(ChiefComplaintUpdates),
      ChiefComplaintUpdates = str_replace_all(ChiefComplaintUpdates, "PT", "PATIENT"),
      # count number of characters and number of words
      number_chars_updates = str_count(ChiefComplaintUpdates),
      number_words_ccupdates = str_count(ChiefComplaintUpdates, boundary("word"))) 
    
  data3 <- dplyr::select(.data = data,-ChiefComplaintUpdates)
  
  data <- bind_cols(data3, data2)
  
  return(data)
}

#### ChiefComplaintOrig ####
clean_ChiefComplaintOriginal <- function(data = my_file) {
  data2 <- data %>%
    dplyr::select(ChiefComplaintOrig) %>%
    mutate(
      # clean punctuation and convert all letters to upper case
      ChiefComplaintOrig = str_replace_all(ChiefComplaintOrig, "[[:cntrl:]]", ""),
      ChiefComplaintOrig = str_trim(ChiefComplaintOrig, side = "both"),
      ChiefComplaintOrig = if_else(nchar(ChiefComplaintOrig)==2, "NA", ChiefComplaintOrig),
      ChiefComplaintOrig = toupper(ChiefComplaintOrig),
      ChiefComplaintOrig = str_replace_na(ChiefComplaintOrig, replacement = "NA"),
      ChiefComplaintOrig = str_replace_all(ChiefComplaintOrig, " PT | PT|PT ", " PATIENT "),
      # count number of characters and number of words
      number_chars_orig = str_count(ChiefComplaintOrig),
      number_words_ccorig = str_count(ChiefComplaintOrig, boundary("word"))) %>%
    mutate(
      number_words_ccorig = case_when(ChiefComplaintOrig == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_words_ccorig))),
      number_chars_orig = case_when(ChiefComplaintOrig == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_chars_orig)))) 
  
  data3 <- dplyr::select(.data = data,-ChiefComplaintOrig)
  
  data <- bind_cols(data3, data2)
  
  return(data)
}

#### DischargeDiagnosis ####
clean_DischargeDiagnosis <- function(data = my_file) {
  data2 <- data %>%
    dplyr::select(DischargeDiagnosis) %>%
    mutate(
      # clean punctuation and convert all letters to upper case
      DischargeDiagnosis = str_replace_all(DischargeDiagnosis, "[[:cntrl:]]|<BR>|[?.!???'+):]", ""),
      DischargeDiagnosis = str_replace_all(DischargeDiagnosis, "\\\\|([a-zA-Z])/([\\d])|([\\d])/([a-zA-Z])|([a-zA-Z])/([a-zA-Z])|([\\d])/([\\d])|;", " "),
      DischargeDiagnosis = str_trim(DischargeDiagnosis, side = "both"),
      DischargeDiagnosis = if_else(nchar(DischargeDiagnosis)<=2, "NA", DischargeDiagnosis),
      DischargeDiagnosis = str_replace_na(DischargeDiagnosis, replacement = "NA"),
      # count number of characters and number of words
      number_chars_dx = str_count(DischargeDiagnosis),
      number_words_dx = str_count(DischargeDiagnosis, boundary("word"))) %>%
    mutate(
      number_chars_dx = case_when(DischargeDiagnosis == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_chars_dx))),
      number_words_dx = case_when(DischargeDiagnosis == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_words_dx)))) 
  
  data3 <- dplyr::select(.data = data,-DischargeDiagnosis)
  
  data <- bind_cols(data3, data2)
  
  return(data)
}

#### CCDD ####
clean_CCDD <- function(data = my_file) {
  data2 <- data %>%
    dplyr::select(CCDD) %>%
    mutate(
      # clean punctuation and convert all letters to upper case
      CCDD = str_replace_all(CCDD, "[[:cntrl:]]|<BR>|[?.!???'+):]", ""),
      CCDD = str_replace_all(CCDD, "\\\\|;|([a-zA-Z])/([\\d])|([\\d])/([a-zA-Z])|([a-zA-Z])/([a-zA-Z])|([\\d])/([\\d])|\\W", " "),
      CCDD = str_trim(CCDD, side = "both"),
      CCDD = if_else(nchar(CCDD)<=2, "NA", CCDD),
      CCDD = str_replace_na(CCDD, replacement = "NA"),
      # count number of characters and number of words
      number_chars_CCDD = str_count(CCDD),
      number_words_CCDD = str_count(CCDD, boundary("word"))) %>%
    mutate(
      number_chars_CCDD = case_when(CCDD == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_chars_CCDD))),
      number_words_CCDD = case_when(CCDD == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_words_CCDD)))) 
  
  data3 <- dplyr::select(.data = data,-CCDD)
  
  data <- bind_cols(data3, data2)
  
  return(data)
}

#### ChiefComplaintParsed ####
clean_ChiefComplaintParsed <- function(data = my_file) {
  data2 <- data %>%
    dplyr::select(ChiefComplaintParsed) %>%
    mutate(
      # clean punctuation and convert all letters to upper case
      ChiefComplaintParsed = str_replace_all(ChiefComplaintParsed, "[[:cntrl:]]", ""),
      ChiefComplaintParsed = str_trim(ChiefComplaintParsed, side = "both"),
      ChiefComplaintParsed = if_else(nchar(ChiefComplaintParsed)==2, "NA", ChiefComplaintParsed),
      ChiefComplaintParsed = str_replace_na(ChiefComplaintParsed, replacement = "NA"),
      # count number of characters and number of words
      number_chars_parsed = str_count(ChiefComplaintParsed),
      number_words_parsed = str_count(ChiefComplaintParsed, boundary("word"))) %>%
    mutate(
      number_words_parsed = case_when(ChiefComplaintParsed == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_words_parsed))),
      number_chars_parsed = case_when(ChiefComplaintParsed == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_chars_parsed)))) 
  
  data3 <- dplyr::select(.data = data,-ChiefComplaintParsed)
  
  data <- bind_cols(data3, data2)
  
  return(data)
}

#### Admit_Reason_Combo ####
clean_Admit_Reason_Combo <- function(data = my_file) {
  data2 <- data %>%
    dplyr::select(Admit_Reason_Combo) %>%
    mutate(
      # clean punctuation and convert all letters to upper case
      Admit_Reason_Combo = str_replace_all(Admit_Reason_Combo, "[[:cntrl:]]", ""),
      Admit_Reason_Combo = str_trim(Admit_Reason_Combo, side = "both"),
      Admit_Reason_Combo = if_else(nchar(Admit_Reason_Combo)==2, "NA", Admit_Reason_Combo),
      Admit_Reason_Combo = toupper(Admit_Reason_Combo),
      Admit_Reason_Combo = str_replace_na(Admit_Reason_Combo, replacement = "NA"),
      Admit_Reason_Combo = str_replace_all(Admit_Reason_Combo, "PT", "PATIENT"),
      # count number of characters and number of words
      number_chars_admit = str_count(Admit_Reason_Combo),
      number_words_admit = str_count(Admit_Reason_Combo, boundary("word"))) %>%
    mutate(
      number_words_admit = case_when(Admit_Reason_Combo == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_words_admit))),
      number_chars_admit = case_when(Admit_Reason_Combo == "NA" ~ 0, TRUE ~ as.numeric(as.character(.$number_chars_admit)))) 
  
  data3 <- dplyr::select(.data = data,-Admit_Reason_Combo)
  
  data <- bind_cols(data3, data2)
  
  return(data)
}
