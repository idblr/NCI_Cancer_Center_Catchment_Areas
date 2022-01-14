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
# A) Code to generate Supplemental Figure 1 in the CEBP Manuscript
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

# Order catchment area by descending size
## Will plot larger catchments first (lower level when stacked by ggplot2)
order_catch <- catchments[order(-catchments$area), ]

# Transform CRS to Albers Equal Area Conic Projection
aeac_centers <- sf::st_transform(cancer_centers, crs = 2163) # projected catchment center locations
aeac_catch <- sf::st_transform(order_catch, crs = 2163) # projected catchment areas

# Lower 48 Shapefiles
#proj_l48_states <- proj_states[proj_states$NAME %notin% not_l48, ]
#proj_l48s_coast <- sf::st_intersection(proj_l48_states, proj_coast) # clip by US coastal boundary
proj_l48c_coast <- sf::st_intersection(proj_l48_counties, proj_coast) # clip by US coastal boundary

# Separate by NCI designation
## NCI-Designated Cancer Center Locations
aeac_c <- aeac_centers[aeac_centers$name != "University of Hawai'i Cancer Center", ] 
## NCI-Designated Cancer Center Catchments
aeac_cc <- aeac_catch[aeac_catch$type == "Clinical Cancer Center" & aeac_catch$name != "University of Hawai'i Cancer Center", ]
## NCI-Designated Comprehensive Cancer Center Catchments
aeac_ccc <- aeac_catch[aeac_catch$type == "Comprehensive Cancer Center", ]

# New York City, New York Metropolitan Area NCI-Designated Cancer Center Catchment Areas
nycfips <- c("36085", "36059", "36081", "36047", "36061", "36059", "36005", "36119", "36087", "36103", "09001", "34003", "34017", "34013", "34031", "34039", "34027", "34023")
nycounty <- proj_l48c_coast[proj_l48c_coast$GEOID %in% nycfips[5], ]
nyca <- proj_l48c_coast[proj_l48c_coast$GEOID %in% nycfips, ]
proj_nycounty <- sf::st_transform(nycounty, crs = 2263)
proj_nyca <- sf::st_transform(nyca, crs = 2263)
ny_bb <- sf::st_as_sfc(sf::st_bbox(proj_nycounty))
ny_bbb <- sf::st_buffer(ny_bb, dist = 80000)

nycnci <- c("Laura & Isaac Perlmutter Cancer Center at NYU Langone Health",
            "Herbert Irving Comprehensive Cancer Center at Columbia University",
            "Tisch Cancer Institute at Icahn School of Medicine at Mount Sinai",
            "Albert Einstein Cancer Center at Albert Einstein College of Medicine",
            "Memorial Sloan-Kettering Cancer Center", 
            "Rutgers Cancer Institute of New Jersey at Rutgers Biomedical & Health Sciences",
            "Georgetown Lombardi Comprehensive Cancer Center at Georgetown University",
            "Yale Cancer Center at Yale University School of Medicine")

nyccc <- sf::st_transform(aeac_ccc[aeac_ccc$name %in% nycnci[c(1:2, 5:8)], ], crs = 2263)
nycc <- sf::st_transform(aeac_cc[aeac_cc$name %in% nycnci[3:4], ], crs = 2263)
nynci <- rbind(nyccc, nycc)
nyc <- sf::st_transform(aeac_c[aeac_c$name %in% nycnci, ], crs = 2263)

#########################
# SUPPLEMENTAL FIGURE 1 #
#########################

f <- 1 # graphical expansion factor
ggm1 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = proj_nyca,
                   ggplot2::aes(fill = "proj_nyca"),
                   color = "black", size = 0.1) +
  ggplot2::scale_fill_manual(name = "",
                             values = c("proj_nyca" = "white"),
                             labels = c("U.S. County Boundary")) +
  ggnewscale::new_scale_fill() +
  ggplot2::geom_sf(data = nycc,
                   ggplot2::aes(color = "Clinical Cancer Center",
                                fill = "Clinical Cancer Center"),
                   size = 0.25,
                   alpha = 0.2) +
  ggplot2::geom_sf(data = nyccc, ggplot2::aes(color = "Comprehensive Cancer Center",
                                              fill = "Comprehensive Cancer Center"),
                   size = 0.25,
                   alpha = 0.2) +
  ggplot2::geom_sf(data = nyc[nyc$type == "Clinical Cancer Center", ],
                   ggplot2::aes(color = "Clinical Cancer Center",
                                fill = "Clinical Cancer Center"),
                   size = 0.5) +
  ggplot2::geom_sf(data = nyc[nyc$type == "Comprehensive Cancer Center", ],
                   ggplot2::aes(color = "Comprehensive Cancer Center",
                                fill = "Comprehensive Cancer Center"),
                   size = 0.5) +
  ggplot2::scale_color_manual(
    aesthetics = c("color", "fill"),
    values = c("Clinical Cancer Center" = "#D95F02",
               "Comprehensive Cancer Center" = "#8DA0CB"),
    labels = c("NCI-designated Clinical Cancer Center",
               "NCI-designated Comprehensive Cancer Center")) +
  ggplot2::coord_sf(xlim = sf::st_bbox(ny_bbb)[c(1,3)],
                    ylim = sf::st_bbox(ny_bbb)[c(2,4)]) +
  ggplot2::labs(title = "Overlap of NCI-designated\nCancer Center Catchment Areas",
                fill = NULL,
                color = NULL) +
  ggplot2::guides(size = ggplot2::guide_legend(order = 1),
                  color = ggplot2::guide_legend(nrow = 2)) +
  ggplot2::theme_minimal(base_size = 7) +
  ggplot2::theme(legend.position = "bottom",
                 legend.box = "vertical",
                 legend.title = ggplot2::element_text(size = ggplot2::rel(1.25*f)), 
                 legend.text = ggplot2::element_text(size = ggplot2::rel(1*f)),
                 legend.key.size = ggplot2::unit(0.5*f, "lines"),
                 axis.text = ggplot2::element_text(size = ggplot2::rel(0.5*f)),
                 axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5),
                 plot.margin = ggplot2::unit(c(0,0,0,0), "cm"),
                 plot.title = ggplot2::element_text(size = ggplot2::rel(1*f)))
ggm2 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = proj_nyca,
                   fill = "white", size = 0.1) +
  ggplot2::geom_sf(data = nynci[nynci$name %in% nycnci[1], ],
                   fill = "#8DA0CB",
                   color = "#8DA0CB",
                   size = 0.20, alpha = 0.2) +
  ggplot2::geom_sf(data = nyc[nyc$name == nycnci[1], ],
                   color = "#8DA0CB", size = 0.5) +
  ggplot2::coord_sf(xlim = sf::st_bbox(ny_bbb)[c(1,3)],
                    ylim = sf::st_bbox(ny_bbb)[c(2,4)]) +
  ggplot2::labs(title = "Laura and Isaac Perlmutter\nCancer Center at\nNYU Langone Health") +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(0.5*f)))
ggm3 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = proj_nyca,
                   fill = "white",
                   size = 0.1) +
  ggplot2::geom_sf(data = nynci[nynci$name %in% nycnci[2], ],
                   fill = "#8DA0CB",
                   color = "#8DA0CB",
                   size = 0.20,
                   alpha = 0.2) +
  ggplot2::geom_sf(data = nyc[nyc$name == nycnci[2], ],
                   color = "#8DA0CB", size = 0.5) +
  ggplot2::coord_sf(xlim = sf::st_bbox(ny_bbb)[c(1,3)],
                    ylim = sf::st_bbox(ny_bbb)[c(2,4)]) +
  ggplot2::labs(title = "Herbert Irving\nComprehensive Cancer Center\nat Columbia University") +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(0.5*f)))
ggm4 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = proj_nyca,
                   fill = "white",
                   size = 0.1) +
  ggplot2::geom_sf(data = nynci[nynci$name %in% nycnci[3], ],
                   fill = "#D95F02",
                   color = "#D95F02",
                   size = 0.20,
                   alpha = 0.2) +
  ggplot2::geom_sf(data = nyc[nyc$name == nycnci[3], ],
                   color = "#D95F02", size = 0.5) +
  ggplot2::coord_sf(xlim = sf::st_bbox(ny_bbb)[c(1,3)],
                    ylim = sf::st_bbox(ny_bbb)[c(2,4)]) +
  ggplot2::labs(title = "Tisch Cancer Institute at\nIcahn School of Medicine at\nMount Sinai") +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(0.5*f)))
ggm5 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = proj_nyca,
                   fill = "white",
                   size = 0.1) +
  ggplot2::geom_sf(data = nynci[nynci$name %in% nycnci[4], ],
                   fill = "#D95F02",
                   color = "#D95F02",
                   size = 0.20,
                   alpha = 0.2) +
  ggplot2::geom_sf(data = nyc[nyc$name == nycnci[4], ],
                   color = "#D95F02", size = 0.5) +
  ggplot2::coord_sf(xlim = sf::st_bbox(ny_bbb)[c(1,3)],
                    ylim = sf::st_bbox(ny_bbb)[c(2,4)]) +
  ggplot2::labs(title = "Albert Einstein Cancer Center at\nAlbert Einstein College of Medicine") +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(0.5*f)))
ggm6 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = proj_nyca,
                   fill = "white",
                   size = 0.1) +
  ggplot2::geom_sf(data = nynci[nynci$name %in% nycnci[5], ],
                   fill = "#8DA0CB",
                   color = "#8DA0CB",
                   size = 0.20,
                   alpha = 0.2) +
  ggplot2::geom_sf(data = nyc[nyc$name == nycnci[5], ],
                   color = "#8DA0CB",
                   size = 0.5) +
  ggplot2::coord_sf(xlim = sf::st_bbox(ny_bbb)[c(1,3)],
                    ylim = sf::st_bbox(ny_bbb)[c(2,4)]) +
  ggplot2::labs(title = "Memorial Sloan-Kettering\nCancer Center") +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(0.5*f)))
ggm7 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = proj_nyca,
                   fill = "white",
                   size = 0.1) +
  ggplot2::geom_sf(data = nynci[nynci$name %in% nycnci[6], ],
                   fill = "#8DA0CB",
                   color = "#8DA0CB",
                   size = 0.20,
                   alpha = 0.2) +
  ggplot2::geom_sf(data = nyc[nyc$name == nycnci[6], ],
                   color = "#8DA0CB", size = 0.5) +
  ggplot2::coord_sf(xlim = sf::st_bbox(ny_bbb)[c(1,3)],
                    ylim = sf::st_bbox(ny_bbb)[c(2,4)]) +
  ggplot2::labs(title = "Rutgers Cancer Institute\nof New Jersey at Rutgers\nBiomedical & Health Sciences") +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(0.5*f)))
ggm8 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = proj_nyca,
                   fill = "white",
                   size = 0.1) +
  ggplot2::geom_sf(data = nynci[nynci$name %in% nycnci[7], ],
                   fill = "#8DA0CB",
                   color = "#8DA0CB",
                   size = 0.20,
                   alpha = 0.2) +
  ggplot2::geom_sf(data = nyc[nyc$name == nycnci[7], ],
                   color = "#8DA0CB",
                   size = 0.5) +
  ggplot2::coord_sf(xlim = sf::st_bbox(ny_bbb)[c(1,3)],
                    ylim = sf::st_bbox(ny_bbb)[c(2,4)]) +
  ggplot2::labs(title = "Georgetown Lombardi\nComprehensive Cancer Center\nat Georgetown University") +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(0.5*f)))
ggm9 <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = proj_nyca,
                   fill = "white",
                   size = 0.1) +
  ggplot2::geom_sf(data = nynci[nynci$name %in% nycnci[8], ],
                   fill = "#8DA0CB",
                   color = "#8DA0CB",
                   size = 0.20,
                   alpha = 0.2) +
  ggplot2::geom_sf(data = nyc[nyc$name == nycnci[8], ],
                   color = "#8DA0CB",
                   size = 0.5) +
  ggplot2::coord_sf(xlim = sf::st_bbox(ny_bbb)[c(1,3)],
                    ylim = sf::st_bbox(ny_bbb)[c(2,4)]) +
  ggplot2::labs(title =  "Yale Cancer Center at\nYale University\nSchool of Medicine") +
  ggplot2::theme_void() +
  ggplot2::theme(plot.title = ggplot2::element_text(size = ggplot2::rel(0.5*f)))

gg_inset_map <- cowplot::ggdraw() +
  cowplot::draw_plot(ggm1, x = -0.180, y = 0.1, width = 0.67, height = 0.67) +
  cowplot::draw_plot(ggm5, x = 0.200, y = 0.5, width = 0.35, height = 0.35) +
  cowplot::draw_plot(ggm8, x = 0.375, y = 0.5, width = 0.35, height = 0.35) +
  cowplot::draw_plot(ggm3, x = 0.550, y = 0.5, width = 0.35, height = 0.35) +
  cowplot::draw_plot(ggm2, x = 0.725, y = 0.5, width = 0.35, height = 0.35) +
  cowplot::draw_plot(ggm6, x = 0.200, y = 0.1, width = 0.35, height = 0.35) + 
  cowplot::draw_plot(ggm7, x = 0.375, y = 0.1, width = 0.35, height = 0.35) +
  cowplot::draw_plot(ggm4, x = 0.550, y = 0.1, width = 0.35, height = 0.35) +
  cowplot::draw_plot(ggm9, x = 0.725, y = 0.1, width = 0.35, height = 0.35)

ggplot2::ggsave(filename = "figures/CEBP_Supplemental1.png",
                plot = gg_inset_map,
                bg = "white",
                width = 8*f,
                height = 4*f,
                dpi = 1000)

# --------------- END OF CODE --------------- #
