# -------------------------------------------- #
# NCI-Designated Cancer Center Catchment Areas
# -------------------------------------------- #
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: January 12, 2022
#
# Most recently modified by: @idblr
# Most recently modified on: May 11, 2022
#
# Notes:
# A) Code to generate Figure 3 in the CEBP Manuscript
# B) For the n=63 Cancer Centers and Comprehensive Cancer Centers only (excludes Basic Laboratories and Pediatric Cancer Centers)
# C) Definitions as of July 2021
# D) 2022/01/31: Shapefiles are available for download from the Catchment Areas of NCI-Designated Cancer Centers web application <https://gis.cancer.gov/ncicatchment/>
# E) 2022/05/11: Updated data paths
# -------------------------------------------- #

############
# PACKAGES #
############

loadedPackages <- c("cowplot", "dplyr", "ggplot2", "sf", "tidycensus", "tigris", "utils")
suppressMessages(invisible(lapply(loadedPackages, library, character.only = TRUE)))

####################
# DATA IMPORTATION #
####################

# Option 1: Shapefiles are available to download from the Catchment Areas of NCI-Designated Cancer Centers web application <https://gis.cancer.gov/ncicatchment/>
## After downloading and placing in a subdirectory called "data":
### A) 'cancer_centers' an 'sf' object of the NCI-Designated Cancer Center locations
utils::unzip(zipfile = "data/NCI_Cancer_Center_Point_Shapefile.zip", exdir = "data") # Note: Modify for directory with downloaded ZIP file
cancer_centers <- sf::read_sf(dsn = "data/NCI_Cancer_Center_Point.shp")

### B) 'cancer_centers' an 'sf' object of the NCI-Designated Cancer Center locations
utils::unzip(zipfile = "data/NCI_Catchment_Area_Shapefile.zip", exdir = "data") # Note: Modify for directory with downloaded ZIP file
catchments <- sf::read_sf(dsn = "data/NCI_Catchment_Area.shp")

### C) 'cancer_centers' an 'sf' object of the NCI-Designated Cancer Center locations
utils::unzip(zipfile = "data/NCI_County_Shapefile.zip", exdir = "data") # Note: Modify for directory with downloaded ZIP file
proj_counties <- sf::read_sf(dsn = "data/US_County.shp")

# Option 2: Use the code found in 'catchments.R' file
## Loads seven objects
### A) 'cancer_centers' an 'sf' object of the NCI-Designated Cancer Center locations
### B) 'catchments' an 'sf' object of the NCI-Designated Cancer Center Catchment Areas
### C) "proj_states" an 'sf' object of the 2018 U.S. States
### D) "proj_counties" an 'sf' object of the 2018 U.S. Counties
### E) "proj_l48_counties" an 'sf' object of the 2018 conterminous U.S.
### F) "proj_coast" an 'sf' object of the U.S. coastline
### G) "not_l48" a 'vector' object of names of non-conterminous U.S. States
#source("code/Catchments.R") # uncomment to generate catchment information from source
# Note: size of catchments are in square kilometers ("area") but downloadable version is in square miles ("Area_Miles")
# Note: the ID for counties are "GEOID" but downloadable version "FIPSN"

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

# Fix FIPS to include leading 0 for states with STATE FIPS < 10
proj_counties$FIPSN <- stringr::str_pad(proj_counties$FIPSN, 5, "left", "0")

# US Coastline
# From https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
coast_US_shp <- "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_nation_5m.zip"
download.file(url = coast_US_shp, destfile = "data/cb_2018_us_nation_5m.zip")
unzip(zipfile = "data/cb_2018_us_nation_5m.zip", exdir = "data")
coast_US <- sf::st_read(dsn = "data/cb_2018_us_nation_5m.shp")
proj_coast <- sf::st_transform(coast_US, crs = 4326)

# Lower 48 Shapefiles
not_l48 <- c("Commonwealth of the Northern Mariana Islands",
             "Guam", "American Samoa", "Hawaii", "Alaska",
             "Puerto Rico", "United States Virgin Islands")
shp_states <- tigris::states(year = 2018, class = "sf", cb = TRUE)
proj_states <-  sf::st_transform(shp_states, crs = 4326)
proj_l48_states <- proj_states[proj_states$NAME %notin% not_l48, ]
proj_l48s_coast <- sf::st_intersection(proj_l48_states, proj_coast) # clip by US coastal boundary
#proj_l48c_coast <- sf::st_intersection(proj_l48_counties, proj_coast) # Uncomment for OPTION 2 (clip by US coastal boundary)
proj_l48c_coast <- proj_counties[proj_counties$STATE %in% proj_l48_states$STUSPS, ]

# Order catchment area by descending size
## Will plot larger catchments first (lower level when stacked by ggplot2)
order_catch <- catchments[order(-catchments$Area_Miles), ]

# Transform CRS to Equal Area Albers Projection
proj_catch <- sf::st_transform(order_catch, crs = 2163) # projected catchment areas
aeac_counties <- sf::st_transform(proj_counties, crs = 2163) # projected catchment areas
aeac_l48s <- sf::st_transform(proj_l48s_coast, crs = 2163) # projected state boundaries

# Population Linkage
aeac_counties$poppt <- us_county_2019$estimate[match(aeac_counties$FIPSN, us_county_2019$GEOID)]
aeac_counties$state <- us_county_2019$state[match(aeac_counties$FIPSN, us_county_2019$GEOID)]
table(aeac_counties$poppt, useNA = "always")
table(us_county_2019$estimate, useNA = "always")
aeac_counties$poppt0 <- as.numeric(ifelse(is.na(aeac_counties$poppt), 0, aeac_counties$poppt))

# Binary indicator variable for if there is an NCI center within
ncic <- sf::st_union(proj_catch) # Note: may require high computational resources (i.e., many minutes to run)
ncicc <- sf::st_buffer(ncic, dist = -1000) # reduce borders by 1km to reduce chance of overlap including county borders (and erroneously including counties outside of catchment definition)
overlap <- sf::st_intersects(aeac_counties, ncicc)
is.na(overlap) <- lengths(overlap) == 0
aeac_counties$nci <- unlist(overlap)
aeac_counties$nci <- ifelse(is.na(aeac_counties$nci), 0, 1)
table(aeac_counties$nci)

# Correction for Hawaii (because of error in original shapefile)
county_hawaii <- aeac_counties[aeac_counties$STATE %in% "HI", ]
county_hawaii$nci[county_hawaii$STATE %in% county_hawaii$STATE] <- 1
county_hawaii$poppt1 <- as.numeric(ifelse(county_hawaii$nci == 0, -county_hawaii$poppt0, county_hawaii$poppt0))
county_hawaii$poppt2 <- cut(county_hawaii$poppt1, breaks = c(-Inf, -100000, -10000, -1000, 0, 1000, 10000, 100000, Inf))

# Format Population 
aeac_counties$poppt1 <- as.numeric(ifelse(aeac_counties$nci == 0, -aeac_counties$poppt0, aeac_counties$poppt0))
aeac_counties$poppt2 <- cut(aeac_counties$poppt1, breaks = c(-Inf, -100000, -10000, -1000, 0, 1000, 10000, 100000, Inf))

# Mask to Lower 48
aeac_l48_counties <- aeac_counties[aeac_counties$STATE %in% proj_l48_states$STUSPS, ]

# Population information for Puerto Rico
## OPTION 1 does not have counties for PR, import here
county_pr <- tigris::counties(state = "72", year = 2018, class = "sf", cb = TRUE)
county_pr$poppt <- us_county_2019$estimate[match(county_pr$GEOID, us_county_2019$GEOID)]
county_pr$nci <- 0 # no NCI-designated Cancer Center Catchment in Puerto Rico
county_pr$poppt0 <- as.numeric(ifelse(is.na(county_pr$poppt), 0, county_pr$poppt))
county_pr$poppt1 <- as.numeric(ifelse(county_pr$nci == 0, -county_pr$poppt0, county_pr$poppt0))
county_pr$poppt2 <- cut(county_pr$poppt1, breaks = c(-Inf, -100000, -10000, -1000, 0, 1000, 10000, 100000, Inf))

###########################
# MINIMAP SPATIAL WINDOWS #
###########################

# Bounding boxes for Alaska, Puerto Rico, and Hawai'i
## Alaska
county_alaska <- aeac_counties[aeac_counties$STATE %in% "AK", ]
proj_county_ak <- sf::st_transform(county_alaska, crs = 3338)
proj_ak <- sf::st_transform(proj_states[proj_states$STUSPS == "AK", ], crs = 3338)
alaska_bb <- sf::st_as_sfc(sf::st_bbox(proj_county_ak))
alaska_bbb <- sf::st_buffer(alaska_bb, dist = 100000)

## Puerto Rico
proj_county_pr <- sf::st_transform(county_pr, crs = 3920)
proj_pr <- sf::st_transform(proj_states[proj_states$STUSPS == "PR", ], crs = 3920)
pr_bb <- sf::st_as_sfc(sf::st_bbox(proj_county_pr))
pr_bbb <- sf::st_buffer(pr_bb, dist = 50000)

## Hawai'i
county_hawaii <- aeac_counties[aeac_counties$STATE %in% "HI", ]
proj_county_hi <- sf::st_transform(county_hawaii, crs = 3759)
proj_hi <- sf::st_transform(proj_states[proj_states$STUSPS == "HI", ], crs = 3759)
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

# NOTE: Post-processing was conducted in a separate graphical software to add figure number and diverging legend labels

# --------------- END OF CODE --------------- #
