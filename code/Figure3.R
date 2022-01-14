# -------------------------------------------- #
# NCI-Designated Cancer Center Catchment Areas
# -------------------------------------------- #
#
# Created by: Ian Buller, Ph.D., M.A.
# Created on: January 12, 2022
#
# Most recently modified by:
# Most recently modified on:
#
# Notes:
# A) Code to generate Figure 3 in the CEBP Manuscript
# B) For the n=63 Cancer Centers and Comprehensive Cancer Centers only (excludes Basic Laboratories and Pediatric Cancer Centers)
# C) Definitions as of July 2021
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
source("code/catchments.R")

#######################
# ADDITIONAL PACKAGES #
#######################

loadedPackages <- c("cowplot", "dplyr", "ggplot2", "sf", "tidycensus")
suppressMessages(invisible(lapply(loadedPackages, library, character.only = TRUE)))

############
# SETTINGS #
############

sf::sf_use_s2(FALSE)
`%notin%` <- Negate(`%in%`) # https://www.r-bloggers.com/the-notin-operator/

################
# DATA LINKAGE #
################

# County population (ACS5 2015-2019)
us_county_2019 <- tidycensus::get_acs(geography = "county",
                                      variables = "B01001_001",
                                      year = 2019)
str(us_county_2019)

us_county_2019$state <- substr(us_county_2019$GEOID, start = 1, stop = 2)
table(us_county_2019$state)

###################
# DATA PROCESSING #
###################

# Lower 48 Shapefiles
proj_l48_states <- proj_states[proj_states$NAME %notin% not_l48, ]
proj_l48s_coast <- sf::st_intersection(proj_l48_states, proj_coast) # clip by US coastal boundary

# Order catchment area by descending size
## Will plot larger catchments first (lower level when stacked by ggplot2)
order_catch <- catchments[order(-catchments$area), ]

# Transform CRS to Equal Area Albers Projection
proj_catch <- sf::st_transform(order_catch, crs = 2163) # projected catchment areas
aeac_counties <- sf::st_transform(proj_counties, crs = 2163) # projected catchment areas
aeac_l48s <- sf::st_transform(proj_l48s_coast, crs = 2163) # projected state boundaries

# Population Linkage
aeac_counties$poppt <- us_county_2019$estimate[match(aeac_counties$GEOID, us_county_2019$GEOID)]
aeac_counties$state <- us_county_2019$state[match(aeac_counties$GEOID, us_county_2019$GEOID)]
table(aeac_counties$poppt, useNA = "always")
table(us_county_2019$estimate, useNA = "always")
aeac_counties$poppt0 <- as.numeric(ifelse(is.na(aeac_counties$poppt), 0, aeac_counties$poppt))

# Binary indicator variable for if there is an NCI center within
ncic <- sf::st_union(proj_catch)
ncicc <- sf::st_buffer(ncic, dist = -1000) # reduce borders by 1km to reduce chance of overlap including county borders (and erroneously including counties outside of catchment definition)
overlap <- sf::st_intersects(aeac_counties, ncicc)
is.na(overlap) <- lengths(overlap) == 0
aeac_counties$nci <- unlist(overlap)
aeac_counties$nci <- ifelse(is.na(aeac_counties$nci), 0, 1)
table(aeac_counties$nci)

# Correction for Hawaii (because of error in original shapefile)
county_hawaii <- aeac_counties[aeac_counties$STATEFP %in% "15", ]
county_hawaii$nci[county_hawaii$NAME %in% county_hawaii$NAME] <- 1
county_hawaii$poppt1 <- as.numeric(ifelse(county_hawaii$nci == 0, -county_hawaii$poppt0, county_hawaii$poppt0))
county_hawaii$poppt2 <- cut(county_hawaii$poppt1, breaks = c(-Inf, -100000, -10000, -1000, 0, 1000, 10000, 100000, Inf))

# Format Population 
aeac_counties$poppt1 <- as.numeric(ifelse(aeac_counties$nci == 0, -aeac_counties$poppt0, aeac_counties$poppt0))
aeac_counties$poppt2 <- cut(aeac_counties$poppt1, breaks = c(-Inf, -100000, -10000, -1000, 0, 1000, 10000, 100000, Inf))

# Mask to Lower 48
nl48_county <- c("02", "15", "72", "60", "66", "69", "78")
aeac_l48_counties <- aeac_counties[aeac_counties$STATEFP %notin% nl48_county, ]

###########################
# MINIMAP SPATIAL WINDOWS #
###########################

# Bounding boxes for Alaska, Puerto Rico, and Hawai'i
## Alaska
county_alaska <- aeac_counties[aeac_counties$STATEFP %in% "02", ]
proj_county_ak <- sf::st_transform(county_alaska, crs = 3338)
proj_ak <- sf::st_transform(proj_states[proj_states$NAME == "Alaska", ], crs = 3338)
alaska_bb <- sf::st_as_sfc(sf::st_bbox(proj_county_ak))
alaska_bbb <- sf::st_buffer(alaska_bb, dist = 100000)

## Puerto Rico
county_pr <- aeac_counties[aeac_counties$STATEFP %in% "72", ]
proj_county_pr <- sf::st_transform(county_pr, crs = 3920)
proj_pr <- sf::st_transform(proj_states[proj_states$NAME == "Puerto Rico", ], crs = 3920)
pr_bb <- sf::st_as_sfc(sf::st_bbox(proj_county_pr))
pr_bbb <- sf::st_buffer(pr_bb, dist = 50000)

## Hawai'i
#county_hawaii <- aeac_counties[aeac_counties$STATEFP %in% "15", ] # above
proj_county_hi <- sf::st_transform(county_hawaii, crs = 3759)
proj_hi <- sf::st_transform(proj_states[proj_states$NAME == "Hawaii", ], crs = 3759)
hawaii_bb <- sf::st_as_sfc(sf::st_bbox(proj_hi))
hawaii_bbb <- sf::st_buffer(hawaii_bb, dist = 10000)

############
# FIGURE 3 #
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
  ggplot2::geom_sf(data = aeac_l48_counties,
                   ggplot2::aes(fill = poppt2),
                   color = "black",
                   size = 0.1,
                   alpha = 0.8) +
  ggplot2::scale_fill_brewer(name = "Population by NCI-designated\nCancer Center Catchment Areas",
                             type = "div",
                             palette = "PRGn",
                             labels = c(">100,000", "10,001-100,000", "1,001-10,000", "0-1,000",
                                        "0-1,000", "1,001-10,000", "10,001-100,000", ">100,000"),
                             guide = ggplot2::guide_legend(reverse = TRUE,
                                                           order = 1)) +
  ggplot2::geom_sf(data = aeac_l48s,
                   fill = "transparent",
                   color = "black",
                   size = 0.33) + 
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
  ggplot2::geom_sf(data = proj_county_pr,
                   ggplot2::aes(fill = poppt2),
                   color = "black",
                   size = 0.1,
                   alpha = 0.8) +
  ggplot2::geom_sf(data = proj_pr,
                   fill = "transparent",
                   size = 0.33) + 
  ggplot2::scale_fill_brewer(guide = "none",
                             drop = FALSE,
                             type = "div",
                             palette = "PRGn",
                             labels = c(">100,000", "10,001-100,000", "1,001-10,000", "0-1,000",
                                        "0-1,000", "1,001-10,000", "10,001-100,000", ">100,000")) +
  ggplot2::theme_void()
ggm3 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = hawaii_bbb,
                   fill = "white",
                   lty = 2,
                   size = 0.1) +
  ggplot2::geom_sf(data = proj_county_hi,
                   ggplot2::aes(fill = poppt2),
                   color = "black",
                   size = 0.1,
                   alpha = 0.8) +
  ggplot2::geom_sf(data = proj_hi,
                   fill = "transparent",
                   size = 0.33) + 
  ggplot2::scale_fill_brewer(guide = "none",
                             drop = FALSE,
                             type = "div",
                             palette = "PRGn",
                             labels = c(">100,000", "10,001-100,000", "1,001-10,000", "0-1,000",
                                        "0-1,000", "1,001-10,000", "10,001-100,000", ">100,000")) +
  ggplot2::theme_void()
ggm4 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = alaska_bbb,
                   fill = "white",
                   lty = 2,
                   size = 0.1) +
  ggplot2::geom_sf(data = proj_county_ak,
                   ggplot2::aes(fill = poppt2),
                   color = "black",
                   size = 0.1,
                   alpha = 0.8) +
  ggplot2::geom_sf(data = proj_ak,
                   fill = "transparent",
                   size = 0.33) + 
  ggplot2::scale_fill_brewer(guide = "none",
                             drop = FALSE,
                             type = "div",
                             palette = "PRGn",
                             labels = c(">100,000", "10,001-100,000", "1,001-10,000", "0-1,000",
                                        "0-1,000", "1,001-10,000", "10,001-100,000", ">100,000")) +
  ggplot2::theme_void()

gg_inset_map <- cowplot::ggdraw() +
  cowplot::draw_plot(ggm1, x = 0.02, width = 0.98, height = 0.98) +
  cowplot::draw_plot(ggm2, x = 0.3, y = 0.033, width = 0.18, height = 0.18) +
  cowplot::draw_plot(ggm3, x = 0.0, y = 0.033, width = 0.28, height = 0.28) +
  cowplot::draw_plot(ggm4, x = 0.0, y = 0.667, width = 0.28, height = 0.28)

ggplot2::ggsave(filename = "figures/CEBP_Figure3.png",
                plot = gg_inset_map,
                bg = "white",
                width = 8*f,
                height = 4*f,
                dpi = 1000)

# NOTE: Post-processing was conducted to add a diverging legend labels

# --------------- END OF CODE --------------- #