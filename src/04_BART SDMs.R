## ________________________________________________________________________

## Title:   BART SDMs
## Purpose: Run regression tree analysis on mammals species to develop SDMs
## Author:  Dominic Henry
## Date:    05/04/2021

## Libraries
library(embarcadero)
library(dismo)
library(glue)
library(rasterVis)
library(RColorBrewer)
library(readxl)
library(sf)
library(raster)
library(tidyverse)
## ________________________________________________________________________

start <- Sys.time()

# Select species ----------------------------------------------------------
(folder_list <- dir("data output/sdm data processing/"))
# (sppselect <- folder_list[[2]])

# Import species and environmental data -----------------------------------
load(glue("data output/sdm data processing/{sppselect}/sdm_input_data.RData"))

# Create output folder ----------------------------------------------------
BART_dir <- glue("data output/sdm BART results/{sppselect}")
if(dir.exists(BART_dir)) {
  print("Folder exists")
} else {
  print("Folder created")
  dir.create(BART_dir)
}

plot(envstack)

# Extract PB data and create input dataframe ------------------------------
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

op <- occ_points %>% 
  sf::st_transform(crs = latlongCRS) 

op <- as.data.frame(as(op, "Spatial")) %>% 
  select(coords.x1, coords.x2, occurrence_id , scientific_name)

op_thin <- spThin::thin(loc.data = op,
                        lat.col = "coords.x1",
                        long.col = "coords.x2",
                        spec.col = "scientific_name",
                        thin.par = 1.5, # units = km
                        reps = 3,
                        locs.thinned.list.return = TRUE,
                        write.files = FALSE,
                        out.dir = BART_dir)

keep_ref <- as.numeric(row.names(op_thin[[1]]))

occ_points <- occ_points %>% 
  filter(row_number() %in% keep_ref) 

occ_points %>% 
  sf::st_write(glue("{BART_dir}/occ_points_thinned.shp"), delete_dsn = TRUE)

PB_data <- occ_points %>%
  ungroup %>%
  mutate(Species = 1) %>%
  select(Species) %>%
  rbind(bck_points %>%
          ungroup %>%
          mutate(Species = 0) %>%
          select(Species)
  )

num_occ_points <- nrow(PB_data %>%
                         filter(Species == 1))

occ <- as(PB_data, 'Spatial')
occ.df <- cbind(PB_data$Species,
                raster::extract(envstack, occ))

occ.df <- as.data.frame(occ.df) 
colnames(occ.df)[1] <- "Observed"
head(occ.df)


##TODO LOOK AT THE EFFECTS OF THE NUMBER OF BACKGROUND POINTS

## Correct for small random sample of occurrence/background points
num_bck <- num_occ_points

if(num_bck < 30){
  num_bck_small <- 40
  keepabs <- sample(2000:8500, num_bck_small)
} else {
  keepabs <- sample(2000:8500, num_bck)
}

occ.df <- occ.df[c(1:num_occ_points,keepabs),]
head(occ.df)

# Run full model BART -----------------------------------------------------
sdm_full <- bart(y.train=occ.df[,'Observed'],
                 x.train=occ.df[,-1],
                 keeptrees = TRUE)

summary(sdm_full)

# Check variable importance -----------------------------------------------
varimp.diag(occ.df[,-1], occ.df[,'Observed'], iter=50, quiet=TRUE)
ggsave(glue("{BART_dir}/variable_importance_diag.png"), width = 8, height = 6)
dev.off()

# Run stepwise variable selection -----------------------------------------

## Custom variable selection for spp ##
if(sppselect == "spp x"){
  
  step.model <- names(occ.df)[c(2:8,11,13)]
  
} else {
  
  step.model <- variable.step(x.data=occ.df[,-1],
                              y.data=occ.df[,'Observed'],
                              quiet=TRUE)
  step.model
  ggsave(glue("{BART_dir}/RMSE_var_selection.png"), width = 8, height = 6)
}

# Retrain the model after variable selection ------------------------------
sdm <- bart(x.train=occ.df[, step.model],
            y.train=occ.df[,'Observed'],
            keeptrees = TRUE)

# Model summary and diagnostics -------------------------------------------
summary(sdm)
ggsave(glue("{BART_dir}/model_diagnostics.png"), width = 8, height = 6)
capture.output(summary(sdm), file = glue("{BART_dir}/model_summary_capture.txt"))
sum_text <- read_lines(glue("{BART_dir}/model_summary_capture.txt"))
tss_threshold <- as.numeric(word(sum_text[10],4))
auc <- as.numeric(word(sum_text[7],3))

# Select prediction area --------------------------------------------------
aeaproj <- "+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

x <- glue("data input/Queries/SDM_query_{sppselect}.xlsx")

env_layer_list <- read_xlsx(x, sheet = "env_var_list") %>% 
  filter(.data[[glue("select - {sppselect}")]] == 1) %>% # .data[[]] is tidy eval for a string input
  dplyr::select(folder,layer) 

envpaths <- enframe(step.model) %>% 
  rename(layer = value) %>% 
  mutate(layer = glue("{layer}.tif")) %>% 
  left_join(env_layer_list) %>% 
  mutate(path = glue("C:/Users/DominicH/Documents/GIS data/Environmental data 30s reduced/{folder}/{layer}")) %>% 
  dplyr::select(path) %>% 
  pull

envpaths

za <- st_read("data input/RSA_fixed.shp",crs = latlongCRS) %>% 
  st_transform(aeaproj)

projenv_aea <- function(x){
  projection(x) <- latlongCRS
  projectRaster(x, crs=aeaproj)
}

envstack_rsa <- readAll(stack(envpaths))
envstack_rsa <- stack(map(envstack_rsa@layers,projenv_aea))
envstack_rsa <- stack(envstack_rsa)

# Run model prediction with 95% CIs ---------------------------------------

map <- predict2.bart(sdm,
  envstack_rsa,
  quantiles = c(0.025, 0.975),
  quiet = FALSE,
  splitby = 10
)

# Map plotting ------------------------------------------------------------

##TODO Add PNG SDM plot with QDS cells overlaid

## Mean SDM probability
maptheme_mean <- rasterTheme(region = rev(brewer.pal(11, "Spectral")),
                        layout.widths = list(right.padding = 10),
                        axis.line = list(col = "transparent"),
                        tick = list(col = 'transparent'))

p1 <- levelplot(map[[1]],
          maxpixels = 1e10,
          margin = FALSE,
          par.settings = maptheme_mean,
          scales = list(x = list(draw = FALSE),
                        y = list(draw = FALSE)),
          zlim = c(0, 1)) +
  layer(sp.points(occurence_points, pch=20, col="black", cex = 0.8),
        data = list(occurence_points=SpatialPoints(occ[1:num_occ_points,])))

pdf(glue("{BART_dir}/SDM_mean_probability.pdf"), width = 16, height = 9)
print(p1)
grid::grid.text('Probability of presence',
                rot = 90,
                y = unit(0.5, "npc"),
                x = unit(0.925, "npc"),
                gp = grid::gpar(fontsize = 15))
dev.off()

png(glue("{BART_dir}/SDM_mean_probability.png"), width = 2800, height = 2000)
print(p1)
grid::grid.text('Probability of presence',
                rot = 90,
                y = unit(0.5, "npc"),
                x = unit(0.925, "npc"),
                gp = grid::gpar(fontsize = 15))
dev.off()

## Posterior width
maptheme_ci <- rasterTheme(region = brewer.pal(9, "Blues"),
                        layout.widths = list(right.padding = 10),
                        axis.line = list(col = "transparent"),
                        tick = list(col = 'transparent'))


p2 <- levelplot(map[[3]] - map[[2]],
                maxpixels = 1e10,
                margin = FALSE,
                par.settings = maptheme_ci,
                scales = list(x = list(draw = FALSE),
                              y = list(draw = FALSE)),
                zlim = c(0, 1))

pdf(glue("{BART_dir}/SDM_posterior_width.pdf"), width = 16, height = 9)
print(p2)
grid::grid.text('Posterior width',
                rot = 90,
                y = unit(0.5, "npc"),
                x = unit(0.925, "npc"),
                gp = grid::gpar(fontsize = 15))
dev.off()


png(glue("{BART_dir}/SDM_posterior_width.png"), width = 2800, height = 2000)
print(p2)
grid::grid.text('Posterior width',
                rot = 90,
                y = unit(0.5, "npc"),
                x = unit(0.925, "npc"),
                gp = grid::gpar(fontsize = 15))
dev.off()

## Four measure SDM plots
pdf(glue("{BART_dir}/SDM_four_panel_probability.pdf"), width = 16, height = 9)
par(mfrow=c(2,2))
par(mar=c(2,1,2,5))
plot(map[[1]], main = 'Posterior mean',
     box=F, axes=F)
plot(map[[2]], main = 'Lower 95% CI bound',
     box=F, axes=F)
plot(map[[3]], main = 'Upper 95% CI bound',
     box=F, axes=F)
plot(map[[3]]-map[[2]], main = 'Credible interval width',
     box=F, axes=F)
dev.off()

## Binary predictions
pdf(glue("{BART_dir}/SDM_four_panel_binary.pdf"), width = 16, height = 9)
par(mfrow=c(2,2))
plot(map[[1]] > tss_threshold, main = 'Posterior mean',
     box=F, axes=F)
plot(map[[2]] > tss_threshold, main = 'Lower 95% CI bound',
     box=F, axes=F)
plot(map[[3]] > tss_threshold, main = 'Upper 95% CI bound',
     box=F, axes=F)
quant <- quantile(values(map[[3]] - map[[2]]),
                  0.75,
                  na.rm = TRUE)
plot((map[[3]] - map[[2]]) > quant,
     box = FALSE,
     axes = FALSE,
     main = "Highest uncertainty zones",
     axis.args=list(at=pretty(0:1), labels=pretty(0:1)),
     legend.args=list(text='', side=2, line=1.3)
)
dev.off()


## Mean binary
pdf(glue("{BART_dir}/SDM_mean_binary.pdf"), width = 16, height = 9)
par(mfrow = c(1,1))
plot(map[[1]] > tss_threshold, main='Predicted presence',
     box=F, axes=F)
points(points(occ[1:num_occ_points,], col = "blue", pch = 20))
dev.off()

png(glue("{BART_dir}/SDM_mean_binary.png"), width = 2800, height = 2000)
par(mfrow = c(1,1))
plot(map[[1]] > tss_threshold, main='Predicted presence',
     box=F, axes=F)
points(points(occ[1:num_occ_points,], col = "blue", pch = 20, cex = 3.5))
dev.off()


# Variable importance -----------------------------------------------------
varimp(sdm, plots=TRUE)
ggsave(glue("{BART_dir}/variable_importance.png"), width = 8, height = 6)


# Partial dependence plots ------------------------------------------------
p <- partial(sdm,
             trace=FALSE,
             ci=TRUE,
             smooth=10,
             equal=TRUE,
             panels=TRUE)
p

ggsave(glue("{BART_dir}/partial_dep.png"), width = 12, height = 8)

# Spatial partial maps ----------------------------------------------------
spa <- spartial(
  model = sdm,
  envs = envstack_rsa,
  x.vars = NULL,
  equal = FALSE,
  smooth = 1,
  transform = TRUE,
  save = TRUE
)

pdf(glue("{BART_dir}/spartial_maps.pdf"), width = 8, height = 6)
plot(spa)
dev.off()

# Write rasters -----------------------------------------------------------
writeRaster(map[[1]],
            glue("{BART_dir}/SDM_prob_mean{sppselect}.tif"),
            format = "GTiff",overwrite = TRUE)

writeRaster(map[[2]],
            glue("{BART_dir}/SDM_prob_lower{sppselect}.tif"),
            format = "GTiff",overwrite = TRUE)

writeRaster(map[[3]],
            glue("{BART_dir}/SDM_prob_upper{sppselect}.tif"),
            format = "GTiff",overwrite = TRUE)

writeRaster(map[[1]] > tss_threshold,
            glue("{BART_dir}/SDM_bin_mean{sppselect}.tif"),
            format = "GTiff",overwrite = TRUE)

writeRaster(map[[2]] > tss_threshold,
            glue("{BART_dir}/SDM_bin_lower{sppselect}.tif"),
            format = "GTiff",overwrite = TRUE)

writeRaster(map[[3]] > tss_threshold,
            glue("{BART_dir}/SDM_bin_upper{sppselect}.tif"),
            format = "GTiff",overwrite = TRUE)

writeRaster(map[[3]]- map[[2]],
            glue("{BART_dir}/SDM_cred_int_width{sppselect}.tif"),
            format = "GTiff",overwrite = TRUE)


# Write BART objects ------------------------------------------------------

saveRDS(object = occ.df, file = glue("{BART_dir}/bart_occdf.rds"))
# occ.df <- readRDS(glue("{BART_dir}/bart_occdf.rds"))

saveRDS(object = sdm, file = glue("{BART_dir}/bart_sdm.rds"))
# sdm <- readRDS(glue("{BART_dir}/bart_sdm.rds"))

saveRDS(object = map, file = glue("{BART_dir}/bart_predict_maps.rds"))
# map <- readRDS(glue("{BART_dir}/bart_predict_maps.rds"))

saveRDS(object = p, file = glue("{BART_dir}/bart_partial_plots.rds"))
# p <- readRDS(glue("{BART_dir}/bart_partial_plots.rds"))

saveRDS(object = spa, file = glue("{BART_dir}/bart_spartial_maps.rds"))
# spp <- readRDS(glue("{BART_dir}/bart_partial_spartial_maps.rds"))

# Timings -----------------------------------------------------------------
print("SUCCESSFULLY COMPLETED")
end <- Sys.time()
print(glue("SUCCESSFULLY COMPLETED - RUNTIME: {round(end-start,2)} {attr(end-start, 'units')}"))


