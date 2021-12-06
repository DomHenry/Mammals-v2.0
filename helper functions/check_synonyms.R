# Add species synonyms to PostgreSQL query --------------------------------
check_synonyms <- function(x){
  
  if (x == "Parahyaena brunnea") {
    spp_qry <- c("Parahyaena brunnea", "Hyaena brunnea")
  } else if (x == "Atelerix frontalis") {
    spp_qry <- c("Atelerix frontalis", "Erinaceus frontalis")
  } else if (x == "Calcochloris obtusirostris") {
    spp_qry <- c("Calcochloris obtusirostris", "Chrysochloris obtusirostris")
  } else if (x == "Graphiurus rupicola") {
    spp_qry <- c("Graphiurus rupicola", "Graphiurus australis", "Graphiurus kaokoensis", "Graphiurus montosus")
  } else if (x == "Cercopithecus albogularis erythrarchus") {
    spp_qry <- c("Cercopithecus albogularis erythrarchus", "Cercopithecus albogularis erythrarcus")
  } else {
    spp_qry <- x
  }
  
  return(spp_qry)
  
}

