# Choose which observations to keep ---------------------------------------
select_records <- function(x, choice) {
  
  df <- readxl::read_xlsx(x) 
  colid <- names(df)[1]
  
  keep <- df %>% 
    filter(decision == choice) %>% 
    select(!!colid) %>% 
    pull
  
  keep[which(keep == "NA")] <- NA
  return(keep)
}