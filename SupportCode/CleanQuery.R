

clean_query_essence <- function(query) {
  
  clean_query <- data.frame(Element = query) %>%
    mutate(Element = str_to_lower(Element)) %>%
    mutate(Element = str_replace_all(Element, ",andnot,\\^.*?\\^|,andnot,\\(.*?\\)", "")) %>%
    mutate(Element = str_replace_all(Element,",and,",",or,")) %>%
    cSplit(., splitCols = "Element", sep = ",or,", type.convert = FALSE) %>%
    pivot_longer(cols = starts_with("Element"), values_to = "Element") %>%
    mutate(Element = str_replace_all(Element,"\\[;/ \\]|\\[;/\\]","")) %>%
    mutate(Element = str_replace_all(Element,"\\)|\\(|\\^|,|;|/|\\.","")) %>%
    mutate(Type = ifelse(str_detect(Element,"[[:digit:]]"),"Diagnosis code","Syndrome term")) %>%
    select(-name, `Syndrome Element` = Element, `Element Type` = Type) %>%
    dplyr::distinct()
  
  return(clean_query)
  
}
