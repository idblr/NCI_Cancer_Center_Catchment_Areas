# -------------------------------------------- #
# NCI-Designated Cancer Center Catchment Areas
# -------------------------------------------- #
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: January 12, 2022
#
# Most recently modified by: @idblr
# Most recently modified on: January 31, 2022
#
# Notes:
# A) Code to generate Figure 1 in the CEBP Manuscript
# B) For the n=63 Cancer Centers and Comprehensive Cancer Centers only (excludes Basic Laboratories and Pediatric Cancer Centers)
# C) Definitions as of July 2021
# D) 2022/01/31: Shapefiles are available for download from the Catchment Areas of NCI-Designated Cancer Centers web application <https://gis.cancer.gov/ncicatchment/>
# -------------------------------------------- #

####################
# DATA IMPORTATION #
####################

# Use the code found in 'catchments.R' file
## Loads seven objects
### A) 'cancer_centers' an 'sf' object of the NCI-Designated Cancer Center locations
### B) 'catchments' an 'sf' object of the NCI-Designated Cancer Center Catchment Areas
### C) "proj_states" an 'sf' object of the 2018 U.S. States
### D) "proj_counties" an 'sf' object of the 2018 U.S. Counties
### E) "proj_l48_counties" an 'sf' object of the 2018 conterminous U.S.
### F) "proj_coast" an 'sf' object of the U.S. coastline
### G) "not_l48" a 'vector' object of names of non-conterminous U.S. States
source("code/Catchments.R")

#######################
# ADDITIONAL PACKAGES #
#######################

loadedPackages <- c("cowplot", "dplyr", "ggplot2", "sf")
suppressMessages(invisible(lapply(loadedPackages, library, character.only = TRUE)))

############
# SETTINGS #
############

sf::sf_use_s2(FALSE)
`%notin%` <- Negate(`%in%`) # https://www.r-bloggers.com/the-notin-operator/

###################
# DATA PROCESSING #
###################

# Lower 48 Shapefiles
proj_l48_states <- proj_states[proj_states$NAME %notin% not_l48, ]
proj_l48s_coast <- sf::st_intersection(proj_l48_states, proj_coast) # clip by US coastal boundary
proj_l48c_coast <- sf::st_intersection(proj_l48_counties, proj_coast) # clip by US coastal boundary

# Order catchment area by descending size
## Will plot larger catchments first (lower level when stacked by ggplot2)
order_catch <- catchments[order(-catchments$area), ]

# Transform CRS to Albers Equal Area Conic Projection
aeac_centers <- sf::st_transform(cancer_centers, crs = 2163) # projected catchment center locations
aeac_catch <- sf::st_transform(order_catch, crs = 2163) # projected catchment areas
aeac_l48s <- sf::st_transform(proj_l48s_coast, crs = 2163) # projected state boundaries
aeac_l48c <- sf::st_transform(proj_l48c_coast, crs = 2163) # projected county boundaries

# Separate by NCI designation
## NCI-Designated Cancer Center Locations
aeac_c <- aeac_centers[aeac_centers$name != "University of Hawai'i Cancer Center", ] 
## NCI-Designated Cancer Center Catchments
aeac_cc <- aeac_catch[aeac_catch$type == "Clinical Cancer Center" & aeac_catch$name != "University of Hawai'i Cancer Center", ]
## NCI-Designated Comprehensive Cancer Center Catchments
aeac_ccc <- aeac_catch[aeac_catch$type == "Comprehensive Cancer Center", ]

###########################
# MINIMAP SPATIAL WINDOWS #
###########################

# Bounding boxes for Alaska, Puerto Rico, and Hawai'i
## Alaska
proj_ak <- sf::st_transform(proj_states[proj_states$NAME == "Alaska", ], crs = 3338)
alaska_bb <- sf::st_as_sfc(sf::st_bbox(proj_ak))
alaska_bbb <- sf::st_buffer(alaska_bb, dist = 100000)

## Puerto Rico
proj_pr <- sf::st_transform(proj_states[proj_states$NAME == "Puerto Rico", ], crs = 3920)
pr_bb <- sf::st_as_sfc(sf::st_bbox(proj_pr))
pr_bbb <- sf::st_buffer(pr_bb, dist = 50000)

## Hawai'i
proj_hi <- sf::st_transform(proj_states[proj_states$NAME == "Hawaii", ], crs = 3759)
hawaii_bb <- sf::st_as_sfc(sf::st_bbox(proj_hi))
hawaii_bbb <- sf::st_buffer(hawaii_bb, dist = 10000)

############
# FIGURE 1 #
############

f <- 2 # graphical expansion factor
ggm1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = aeac_l48s,
                   ggplot2::aes(fill = "aeac_l48s"),
                   color = "black",
                   size = 0.33) +
  ggplot2::scale_fill_manual(name = "",
                             values = c("aeac_l48s" = "white"),
                             labels = c("U.S. State Boundary"),
                             guide = ggplot2::guide_legend(order = 2)) +
  ggnewscale::new_scale_fill() +
  ggplot2::geom_sf(data = aeac_cc, ggplot2::aes(fill = "Clinical Cancer Center",
                                                color = "Clinical Cancer Center"),
                   size = 0.35,
                   alpha = 0.2) +
  ggplot2::geom_sf(data = aeac_ccc,
                   ggplot2::aes(fill = "Comprehensive Cancer Center",
                                color = "Comprehensive Cancer Center"),
                   size = 0.35,
                   alpha = 0.2) +
  ggplot2::geom_sf(data = aeac_c[aeac_c$type == "Clinical Cancer Center" | aeac_c$type == "Comprehensive Cancer Center", ],
                   ggplot2::aes(color = type,
                                fill = type),
                   size = 0.5*f) +
  ggplot2::scale_color_manual(name = "NCI-designated\nCancer Center Catchment Areas",
                              aesthetics = c("color", "fill"),
                              values = c("Clinical Cancer Center" = "#D95F02",
                                         "Comprehensive Cancer Center" = "#8DA0CB"),
                              labels = c("Clinical Cancer Center",
                                         "Comprehensive Cancer Center"),
                              guide = ggplot2::guide_legend(order = 1)) +
  ggplot2::theme_minimal(base_size = 14/f) +
  ggplot2::coord_sf(label_axes = list(bottom = "E",
                                      right = "N")) +
  ggplot2::theme(legend.position = "left",
                 legend.title = ggplot2::element_text(size = ggplot2::rel(1*f)), 
                 legend.text = ggplot2::element_text(size = ggplot2::rel(0.75*f)),
                 legend.key.size = ggplot2::unit(0.5*f, "lines"),
                 legend.spacing.y = ggplot2::unit(0, "cm"),
                 plot.title = ggplot2::element_text(size = ggplot2::rel(1*f)))
ggm2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = pr_bbb,
                   fill = "white",
                   lty = 2,
                   size = 0.1) +
  ggplot2::geom_sf(data = proj_pr,
                   fill = "white",
                   size = 0.33) +
  ggplot2::theme_void()
ggm3 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = hawaii_bbb,
                   fill = "white",
                   lty = 2,
                   size = 0.1) +
  ggplot2::geom_sf(data = proj_hi,
                   fill = "#D95F02",
                   color = "#D95F02",
                   size = 0.35,
                   alpha = 0.2) +
  ggplot2::geom_sf(data = proj_centers[proj_centers$name == "University of Hawai'i Cancer Center", ],
                   color = "#D95F02",
                   size = 0.5*f) +
  ggplot2::theme_void()
ggm4 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = alaska_bbb,
                   fill = "white",
                   lty = 2,
                   size = 0.1) +
  ggplot2::geom_sf(data = proj_ak,
                   fill = "white",
                   size = 0.33) +
  ggplot2::theme_void()

gg_inset_map <- cowplot::ggdraw() +
  cowplot::draw_plot(ggm1, x = 0.02, width = 0.98, height = 0.98) +
  cowplot::draw_plot(ggm2, x = 0.3, y = 0.033, width = 0.18, height = 0.18) +
  cowplot::draw_plot(ggm3, x = 0.0, y = 0.033, width = 0.28, height = 0.28) +
  cowplot::draw_plot(ggm4, x = 0.0, y = 0.667, width = 0.28, height = 0.28)

ggplot2::ggsave(filename = "figures/CEBP_Figure1.png",
                plot = gg_inset_map,
                bg = "white",
                width = 8*f,
                height = 4*f,
                dpi = 1000)

# --------------- END OF CODE --------------- #
