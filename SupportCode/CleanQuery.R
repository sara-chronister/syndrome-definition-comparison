
clean_query_essence <- function(query) {
  
  clean_query <- data.frame(Element = query) %>%
    mutate(Element = str_to_lower(Element)) %>%
    mutate(Element = str_replace_all(Element, ",andnot,\\^.*?\\^|,andnot,\\(.*?\\)", "")) %>%
    mutate(Element = str_replace_all(Element, "!","^")) %>%
    mutate(Element = str_replace_all(Element,",and,",",or,")) %>%
    mutate(Element = str_replace_all(Element,",or,","|")) %>%
    cSplit(., splitCols = "Element", sep = "|", type.convert = FALSE) %>%
    pivot_longer(cols = starts_with("Element"), values_to = "Element") %>%
    mutate(Element = str_replace_all(Element,"\\[;/ \\]|\\[;/\\]","")) %>%
    mutate(Element = str_replace_all(Element,"\\)|\\(|\\^|,|;|/|\\.","")) %>%
    mutate(Type = case_when(
      str_detect(Element,"v[[:digit:]]") ~ "CCDD Category (see ESSENCE)",
      str_detect(Element,"[[:digit:]]") ~ "Diagnosis Code",
      str_detect(Element, "[[:digit:]]", negate = TRUE) ~ "Syndrome Term")) %>%
    select(-name, `Syndrome Element` = Element, `Element Type` = Type) %>%
    dplyr::distinct()
  
  return(clean_query)
  
}
