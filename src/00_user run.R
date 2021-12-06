# Step 1: select focal species --------------------------------------------

# sppselect <- "Amblysomus corriae"
# sppselect <- "Amblysomus septentrionalis"
# sppselect <- "Aonyx capensis"
# sppselect <- "Atelerix frontalis"
# sppselect <- "Calcochloris obtusirostris"
# sppselect <- "Cephalophus natalensis"
# sppselect <- "Ceratotherium simum simum"
# sppselect <- "Cercopithecus albogularis erythrarchus"
# sppselect <- "Cistugo seabrae"
# sppselect <- "Crocidura mariquensis"
# sppselect <- "Crocuta crocuta"
# sppselect <- "Dasymys incomtus"
# sppselect <- "Graphiurus ocularis"
# sppselect <- "Graphiurus rupicola"
# sppselect <- "Kerivoula argentata"
# sppselect <- "Leptailurus serval"
# sppselect <- "Miniopterus inflatus"
# sppselect <- "Nycteris woodi"
# sppselect <- "Otomops martiensseni"
# sppselect <- "Otomys auratus"
# sppselect <- "Otomys laminatus"
# sppselect <- "Parahyaena brunnea"
# sppselect <- "Paraxerus palliatus"
# sppselect <- "Parotomys littledalei"
# sppselect <- "Pelea capreolus"
# sppselect <- "Petrodromus tetradactylus"
# sppselect <- "Poecilogale albinucha"
# sppselect <- "Rhinolophus blasii"
# sppselect <- "Rhinolophus denti"
# sppselect <- "Rhinolophus smithersi"
# sppselect <- "Scotoecus albofuscus"
# sppselect <- "Scotophilus nigrita "
# sppselect <- "Taphozous perforatus"


# Step 2: extract the occurrence data from EWT database -------------------
source("src/01_extract and process occ data.R")
rm(list = ls()[!(ls() %in% "sppselect")])

# Step 3: create plots ----------------------------------------------------
source("src/02_spatiotemporal occ plots.R")
rm(list = ls()[!(ls() %in% "sppselect")])

# Step 4: prepare data for SDMs  ------------------------------------------
source("src/03_prepare SDM inputs.R")
rm(list = ls()[!(ls() %in% "sppselect")])

# Step 5: Run BART SDMs ---------------------------------------------------
source("src/04_BART SDMs.R")
rm(list = ls()[!(ls() %in% "sppselect")])



# Not run -----------------------------------------------------------------

# Loop to process bats
bat_list <- c("Cistugo seabrae", "Kerivoula argentata", "Miniopterus inflatus", "Nycteris woodi",
              "Otomops martiensseni", "Rhinolophus blasii", "Rhinolophus denti", "Rhinolophus smithersi",
              "Scotoecus albofuscus", "Scotophilus nigrita", "Taphozous perforatus")


for(i in 10:length(bat_list)){

  sppselect <- bat_list[[i]]

  source("src/01_extract and process occ data.R")
  rm(list = ls()[!(ls() %in% c("sppselect","bat_list"))])

  source("src/02_spatiotemporal occ plots.R")
  rm(list = ls()[!(ls() %in% c("sppselect","bat_list"))])

  print("COMPLETED")

}


# Species issues ----------------------------------------------------------

# Hedgehog sf object has several very clearly QDS records - need to filter those out! Explore further


# Notes -------------------------------------------------------------------

## Thin data (If model is for Wild Dog then run "src/functions/wild_dog_thinning.R")



