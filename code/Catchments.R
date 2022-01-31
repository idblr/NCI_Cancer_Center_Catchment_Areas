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
# A) Code to generate the shapefiles for the NCI-Designated Cancer Center Catchment Areas
# B) Code to import the NCI-Designated Cancer Center
# C) For the n=63 Cancer Centers and Comprehensive Cancer Centers only (excludes Basic Laboratories and Pediatric Cancer Centers)
# D) Definitions as of July 2021
# E) 2022/01/31: Shapefiles are available for download from the Catchment Areas of NCI-Designated Cancer Centers web application <https://gis.cancer.gov/ncicatchment/>
# -------------------------------------------- #

############
# PACKAGES #
############

loadedPackages <- c("dplyr", "sf", "stringr", "tigris", "units")
suppressMessages(invisible(lapply(loadedPackages, library, character.only = TRUE)))

############
# SETTINGS #
############

sf::sf_use_s2(FALSE)
options(tigris_use_cache = TRUE)
`%notin%` <- Negate(`%in%`) # https://www.r-bloggers.com/the-notin-operator/

####################
# DATA IMPORTATION #
####################

# Set path for data
catch_path <- "data/nci_cancer_centers_2021-11-22.csv" # INSERT YOUR OWN PATH
center_path <- "data/nci_catchment_areas_2021-05-07.csv" # INSERT YOUR OWN PATH

# Generate two CSV files
## 1) NCI-Designated Cancer Center Data
### 'data.frame':	63 obs. of  8 variables:
  # $ id       : int  [1:63]
  # $ name     : chr  [Official Name for each Cancer Center]
  # $ state    : chr  [U.S. State the Cancer Center is located]
  # $ type     : chr  [NCI-Designation: "Cancer Center" or "Comprehensive Cancer Center"]
  # $ longitude: num  [x coordinate, WGS84 CRS]
  # $ latitude : num  [y coordinate, WGS84 CRS]
  # $ address  : chr  [Textual address, for reference]
  # $ year     : int  [Year initiated]
## 2) NCI-Designated Cancer Center Catchment Area Data
### 'data.frame':	63 obs. of  8 variables:
  # $ Center : chr  [Keyword for each center per GeoID, see examples below]
  # $ GeoType: chr  [GeoID Type]
  # $ GeoID  : int  [FIPS for components of each catchment area. See Supplemental Table 1 in the CEBP manuscript]

# Import NCI Center Information
nci_centers <- read.csv(file = catch_path)
#str(nci_centers)
nci_centers$type <- as.factor(nci_centers$type)
cancer_centers <- sf::st_as_sf(x = nci_centers,                         
                               coords = c("longitude", "latitude"),
                               crs = 4326)
cancer_centers$type

# Import gold standard list of FIPS
nci_catch <- read.csv(file = center_path)
#str(nci_catch)

# Fix FIPS in nci_catch dataset to include leading 0 for states with STATE FIPS < 10
nci_catch$GeoID <- stringr::str_pad(nci_catch$GeoID, 5, "left", "0")

###################
# SPATIAL WINDOWS #
###################

# US State Shapefile
shp_states <- tigris::states(year = 2018, class = "sf", cb = TRUE)
proj_states <-  sf::st_transform(shp_states, crs = 4326)
not_l48 <- c("Commonwealth of the Northern Mariana Islands", "Guam", "American Samoa", "Hawaii", "Alaska", "Puerto Rico", "United States Virgin Islands")
proj_l48_states <- proj_states[proj_states$NAME %notin% not_l48 , ]
union_l48 <- sf::st_union(proj_l48_states)
l48_name <- proj_l48_states$NAME[proj_l48_states$NAME %notin% not_l48]

# Lower 48 US County Shapefile
shp_counties <- tigris::counties(year = 2018, class = "sf", cb = TRUE)
proj_counties <-  sf::st_transform(shp_counties, crs = 4326)
shp_l48_counties <- tigris::counties(state = l48_name, year = 2018, class = "sf", cb = TRUE)
proj_l48_counties <- sf::st_transform(shp_l48_counties, crs = 4326)

# US Coastline
# From https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.html
coast_US_shp <- "https://www2.census.gov/geo/tiger/GENZ2018/shp/cb_2018_us_nation_5m.zip"
download.file(url = coast_US_shp, destfile = "data/cb_2018_us_nation_5m.zip")
unzip(zipfile = "data/cb_2018_us_nation_5m.zip")
coast_US <- sf::st_read(dsn = "cb_2018_us_nation_5m.shp")
proj_coast <- sf::st_transform(coast_US, crs = 4326)
coast <- sf::st_intersection(union_l48, proj_coast) # clip by US coastal boundary
coast <- sf::st_collection_extract(coast, "POLYGON") 
#plot(sf::st_geometry(coast))

# U.S. State and County Shapefiles
proj_l48s_coast <- sf::st_intersection(proj_l48_states, proj_coast) # clip by US coastal boundary
proj_l48c_coast <- sf::st_intersection(proj_l48_counties, proj_coast) # clip by US coastal boundary

# Shapefiles for US Counties
shp_counties <- tigris::counties(year = 2018, class = "sf")

#####################################################
# NCI-DESIGNATED CANCER CENTER CATCHMENT SHAPEFILES #
#####################################################

## STATES ####

# Fred & Pamela Buffett Cancer Center at Nebraska medicine & the University of Nebraska Medical Center
proj_buffett <- proj_states[proj_states$NAME %in% "Nebraska", ]
proj_buffett <- sf::st_transform(proj_buffett, crs = 4326)
proj_buffett <- sf::st_intersection(proj_buffett, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_buffett))

# University of Wisconsin Carbone Cancer Center
proj_carbone <- proj_states[proj_states$NAME %in% "Wisconsin", ]
proj_carbone <- sf::st_intersection(proj_carbone, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_carbone))

# Rutgers Cancer Institute of New Jersey at Rutgers Biomedical & Health Sciences
proj_cinj <- proj_states[proj_states$NAME %in% "New Jersey", ]
proj_cinj <- sf::st_intersection(proj_cinj, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_cinj))

# Norris Cotton Cancer Center at Dartmouth-Hitchcock Medical Center
proj_cotton <- proj_states[proj_states$NAME %in% c("Vermont", "New Hampshire"), ]
proj_cotton <- sf::st_union(proj_cotton)
proj_cotton <- sf::st_intersection(proj_cotton, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_cotton))

# Dana-Farber/Harvard Cancer Center
proj_farber <- proj_states[proj_states$NAME %in% "Massachusetts", ]
proj_farber <- sf::st_intersection(proj_farber, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_farber))

# Holden Comprehensive Cancer Center at University of Iowa
proj_holden <- proj_states[proj_states$NAME %in% "Iowa", ]
proj_holden <- sf::st_intersection(proj_holden, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_holden))

# Hollings Cancer Center at Medical University of South Carolina
proj_hollings <- proj_states[proj_states$NAME %in% "South Carolina", ]
proj_hollings <- sf::st_intersection(proj_hollings, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_hollings))

# Huntsman Cancer Institute at University of Utah
proj_huntsman <- proj_states[proj_states$NAME %in% c("Idaho", "Montana", "Nevada", "Wyoming", "Utah"), ]
proj_huntsman <- sf::st_union(proj_huntsman)
proj_huntsman <- sf::st_transform(proj_huntsman, crs = 4326)
proj_huntsman <- sf::st_intersection(proj_huntsman, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_huntsman))

# Fred Hutchinson/University of Washington Cancer Consortium
proj_hutchinson <- proj_states[proj_states$NAME %in% "Washington", ]
proj_hutchinson <- sf::st_intersection(proj_hutchinson, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_hutchinson))

# Sidney Kimmel Comprehensive Cancer Center at Johns Hopkins University
proj_hopkins <- proj_states[proj_states$NAME %in% "Maryland", ]
proj_hopkins <- sf::st_transform(proj_hopkins, crs = 4326)
proj_hopkins <- sf::st_intersection(proj_hopkins, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_hopkins))

# Knight Cancer Institute at Oregon Health & Science University
proj_knight <- proj_states[proj_states$NAME %in% "Oregon", ]
proj_knight <- sf::st_intersection(proj_knight, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_knight))

# UNC Lineberger Comprehensive Cancer Center
proj_lineberger <- proj_states[proj_states$NAME %in% "North Carolina", ]
proj_lineberger <- sf::st_intersection(proj_lineberger) # clip by US coastal boundary
#plot(sf::st_geometry(proj_lineberger))

# Markey Cancer Center at University of Kentucky
proj_markey <- proj_states[proj_states$NAME %in% "Kentucky", ]
proj_markey <- sf::st_transform(proj_markey, crs = 4326)
proj_markey <- sf::st_intersection(proj_markey, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_markey))

# Masonic Cancer Center University of Minnesota
proj_masonic <- proj_states[proj_states$NAME %in% "Minnesota", ]
proj_masonic <- sf::st_intersection(proj_masonic, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_masonic))

# The University of Texas MD Anderson Cancer Center
proj_anderson <- proj_states[proj_states$NAME %in% "Texas", ]
proj_anderson <- sf::st_intersection(proj_anderson, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_anderson))

# O'Neal Comprehensive Cancer Center at University of Alabama at Birmingham
proj_oneal <- proj_states[proj_states$NAME %in% "Alabama", ]
proj_oneal <- sf::st_intersection(proj_oneal, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_oneal))

# The Ohio State University Comprehensive Cancer Center at James Cancer Hospital & Solove Research Institute
proj_ohio <- proj_states[proj_states$NAME %in% "Ohio", ]
proj_ohio <- sf::st_intersection(proj_ohio, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_ohio))

# University of Michigan Rogel Cancer Center
proj_rogel <- proj_states[proj_states$NAME %in% "Michigan", ]
proj_rogel <- sf::st_intersection(proj_rogel, coast) # clip by US coastal boundary
proj_rogel <- sf::st_collection_extract(proj_rogel, "POLYGON") 
#plot(sf::st_geometry(proj_rogel))

# Indiana University Melvin & Bren Simon Comprehensive Cancer Center
proj_simon <- proj_states[proj_states$NAME %in% "Indiana", ]
proj_simon <- sf::st_intersection(proj_simon, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_simon))

# Stephenson Cancer Center at University of Oklahoma
proj_stephenson <- proj_states[proj_states$NAME %in% "Oklahoma", ]
proj_stephenson <- sf::st_intersection(proj_stephenson, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_stephenson))

# University of Colorado Cancer Center
proj_colorado <- proj_states[proj_states$NAME %in% "Colorado", ]
proj_colorado <- sf::st_intersection(proj_colorado, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_colorado))

# University of New Mexico Comprehensive Cancer Center
proj_unm <- proj_states[proj_states$NAME %in% "New Mexico", ]
proj_unm <- sf::st_intersection(proj_unm, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_unm))

# Winship Cancer Institute at Emory University
proj_winship <- proj_states[proj_states$NAME %in% "Georgia", ]
proj_winship <- sf::st_intersection(proj_winship, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_winship))

# Yale Cancer Center at Yale University School of Medicine
proj_yale <- proj_states[proj_states$NAME %in% "Connecticut", ]
proj_yale <- sf::st_intersection(proj_yale, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_yale))

## COUNTIES ####

# Abramson Cancer Center at University of Pennsylvania
catch_abramson <- nci_catch$GeoID[nci_catch$Center == "Abramson"]
proj_abramson <- proj_counties[proj_counties$GEOID %in% catch_abramson, ]
proj_abramson <- sf::st_union(proj_abramson)
proj_abramson <- sf::st_intersection(proj_abramson, coast) # clip by US coastal boundary
proj_abramson <- sf::st_collection_extract(proj_abramson, "POLYGON") 
#plot(sf::st_geometry(proj_abramson))

# Wake Forest Baptist Comprehensive Cancer Center
catch_baptist <- nci_catch$GeoID[nci_catch$Center == "Baptist"]
proj_baptist <- proj_counties[proj_counties$GEOID %in% catch_baptist, ]
proj_baptist <- sf::st_union(proj_baptist)
proj_baptist <- sf::st_intersection(proj_baptist, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_baptist))

# Case Comprehensive Cancer Center at Case Western Reserve University
catch_case <- nci_catch$GeoID[nci_catch$Center == "Case"]
proj_case <- proj_counties[proj_counties$GEOID %in% catch_case, ]
proj_case <- sf::st_union(proj_case)
proj_case <- sf::st_intersection(proj_case, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_case))

# Chao Family Comprehensive Cancer Center at University of California, Irvine
catch_chao <- nci_catch$GeoID[nci_catch$Center == "Chao"]
proj_chao <- proj_counties[proj_counties$GEOID %in% catch_chao, ] 
proj_chao <- sf::st_intersection(proj_chao, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_chao))

# City of Hope Comprehensive Cancer Center
catch_chao <- nci_catch$GeoID[nci_catch$Center == "COH"]
proj_coh <- proj_counties[proj_counties$GEOID %in% catch_chao, ] 
proj_coh <- sf::st_union(proj_coh)
proj_coh <- sf::st_intersection(proj_coh, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_coh))

# UCSF Helen Diller Family Comprehensive Cancer Center at the University of California at San Francisco
catch_diller <- nci_catch$GeoID[nci_catch$Center == "Diller"]
proj_diller <- proj_counties[proj_counties$GEOID %in% catch_diller, ]
proj_diller <- sf::st_union(proj_diller)
proj_diller <- sf::st_intersection(proj_diller, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_diller))

# Duke Cancer Institute at Duke University Cancer Center
catch_duke <- nci_catch$GeoID[nci_catch$Center == "Duke"]
proj_duke <- proj_counties[proj_counties$GEOID %in% catch_duke, ]
proj_duke <- sf::st_union(proj_duke)
proj_duke <- sf::st_intersection(proj_duke, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_duke))

# Dan L. Duncan Comprehensive Cancer Center at Baylor College of Medicine
catch_duncan <- nci_catch$GeoID[nci_catch$Center == "Duncan"]
proj_duncan <- proj_counties[proj_counties$GEOID %in% catch_duncan, ]
proj_duncan <- sf::st_union(proj_duncan)
proj_duncan <- sf::st_intersection(proj_duncan, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_duncan))

# Albert Einstein Cancer Center at Albert Einstein College of Medicine
catch_einstein <- nci_catch$GeoID[nci_catch$Center == "Einstein"]
proj_einstein <- proj_counties[proj_counties$GEOID %in% catch_einstein, ] 
proj_einstein <- sf::st_intersection(proj_einstein, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_einstein))

# Fox Chase Cancer Center
catch_foxchase <- nci_catch$GeoID[nci_catch$Center == "Fox Chase"]
proj_foxchase <- proj_counties[proj_counties$GEOID %in% catch_foxchase, ] 
proj_foxchase <- sf::st_union(proj_foxchase)
proj_foxchase <- sf::st_intersection(proj_foxchase, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_foxchase))

# University of Maryland Marlene and Stewart Greenebaum Comprehensive Cancer Center
catch_greenebaum <- nci_catch$GeoID[nci_catch$Center == "Greenebaum"]
proj_greenebaum <- proj_counties[proj_counties$GEOID %in% catch_greenebaum, ] 
proj_greenebaum <- sf::st_union(proj_greenebaum)
proj_greenebaum <- sf::st_intersection(proj_greenebaum, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_greenebaum))

# UPMC Hillman Cancer Center
catch_hillman <- nci_catch$GeoID[nci_catch$Center == "Hillman"]
proj_hillman <- proj_counties[proj_counties$GEOID %in% catch_hillman, ]
proj_hillman <- sf::st_union(proj_hillman)
proj_hillman <- sf::st_intersection(proj_hillman, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_hillman))

# Vanderbilt-Ingram Cancer Centers
catch_ingram <- nci_catch$GeoID[nci_catch$Center == "Ingram"]
proj_TN <- proj_counties[proj_counties$GEOID %in% seq(47000, 47190, by = 1), ]
proj_ingram <- proj_counties[proj_counties$GEOID %in% catch_ingram, ]
proj_ingram <- rbind(proj_TN, proj_ingram)
proj_ingram <- sf::st_union(proj_ingram)
proj_ingram <- sf::st_intersection(proj_ingram, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_ingram))

# Herbert Irving Comprehensive Cancer Center at Columbia University
catch_irving <- nci_catch$GeoID[nci_catch$Center == "Irving"]
proj_irving <- proj_counties[proj_counties$GEOID %in% catch_irving, ]
proj_irving <- sf::st_union(proj_irving)
proj_irving <- sf::st_intersection(proj_irving, coast) # clip by US coastal boundary
proj_irving <- sf::st_collection_extract(proj_irving, "POLYGON") 
#plot(sf::st_geometry(proj_irving))

# Sidney Kimmel Cancer Center at Thomas Jefferson University
catch_jefferson <- nci_catch$GeoID[nci_catch$Center == "Jefferson"]
proj_jefferson <- proj_counties[proj_counties$GEOID %in% catch_jefferson, ]
proj_jefferson <- sf::st_union(proj_jefferson)
proj_jefferson <- sf::st_intersection(proj_jefferson, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_jefferson))

# Jonsson Comprehensive Cancer Center at University of California at Los Angeles
catch_jonsson <- nci_catch$GeoID[nci_catch$Center == "Jonsson"]
proj_jonsson <- proj_counties[proj_counties$GEOID %in% catch_jonsson, ] 
proj_jonsson <- sf::st_intersection(proj_jonsson, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_jonsson))

# Barbara Ann Karmanos Cancer Institute at Wayne State University School of Medicine
catch_karmanos <- nci_catch$GeoID[nci_catch$Center == "Karmanos"]
proj_karmanos <- proj_counties[proj_counties$GEOID %in% catch_karmanos, ]
proj_karmanos <- sf::st_union(proj_karmanos)
proj_karmanos <- sf::st_intersection(proj_karmanos, coast) # clip by US coastal boundary
proj_karmanos <- sf::st_collection_extract(proj_karmanos, "POLYGON") 
#plot(sf::st_geometry(proj_karmanos))

# Georgetown Lombardi Comprehensive Cancer Center at Georgetown University
catch_lombardi <- nci_catch$GeoID[nci_catch$Center == "Lombardi"]
proj_lombardi <- proj_counties[proj_counties$GEOID %in% catch_lombardi, ]
proj_lombardi <- sf::st_union(proj_lombardi)
proj_lombardi <- sf::st_intersection(proj_lombardi, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_lombardi))

# Robert H. Lurie Comprehensive Cancer Center at Northwestern University
catch_lurie <- nci_catch$GeoID[nci_catch$Center == "Lurie"]
proj_lurie <- proj_counties[proj_counties$GEOID %in% catch_lurie, ]
proj_lurie <- sf::st_union(proj_lurie)
proj_lurie <- sf::st_intersection(proj_lurie, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_lurie))

# Massey Cancer Center at Virginia Commonwealth University
catch_massey <- nci_catch$GeoID[nci_catch$Center == "Massey"]
proj_massey <- proj_counties[proj_counties$GEOID %in% catch_massey, ] 
proj_massey <- sf::st_union(proj_massey)
proj_massey <- sf::st_intersection(proj_massey, coast) # clip by US coastal boundary
proj_massey <- sf::st_collection_extract(proj_massey, "POLYGON") 
#plot(sf::st_geometry(proj_massey))

# Mayo Clinic Cancer Center
catch_mayo <- nci_catch$GeoID[nci_catch$Center == "Mayo Clinic"]
proj_mayo <- proj_counties[proj_counties$GEOID %in% catch_mayo, ]
proj_mayo <- sf::st_union(proj_mayo)
proj_mayo <- sf::st_intersection(proj_mayo, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_mayo))

# Mays Cancer Center at UT Health San Antonio, University of Texas Health Science Center
catch_mays <- nci_catch$GeoID[nci_catch$Center == "Mays"]
proj_mays <- proj_counties[proj_counties$GEOID %in% catch_mays, ]
proj_mays <- sf::st_union(proj_mays)
proj_mays <- sf::st_intersection(proj_mays, coast) # clip by US coastal boundary
proj_mays <- sf::st_collection_extract(proj_mays, "POLYGON") 
#plot(sf::st_geometry(proj_mays))

# Moffitt Cancer Center
catch_moffitt <- nci_catch$GeoID[nci_catch$Center == "Moffitt"]
proj_moffitt <- proj_counties[proj_counties$GEOID %in% catch_moffitt, ]
proj_moffitt <- sf::st_union(proj_moffitt)
proj_moffitt <- sf::st_intersection(proj_moffitt, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_moffitt))

# Moores Comprehensive Cancer Center at University of California San Diego
catch_moores <- nci_catch$GeoID[nci_catch$Center == "Moores"]
proj_moores <- proj_counties[proj_counties$GEOID %in% catch_moores, ] 
proj_moores <- sf::st_intersection(proj_moores, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_moores))

# USC Norris Comprehensive Cancer Center at University of Southern California
catch_norris <- nci_catch$GeoID[nci_catch$Center == "Norris"]
proj_norris <- proj_counties[proj_counties$GEOID %in% catch_norris, ] 
proj_norris <- sf::st_intersection(proj_norris, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_norris))

# Roswell Park Comprehensive Cancer Center
catch_roswell <- nci_catch$GeoID[nci_catch$Center == "Roswell Park"]
proj_roswell <- proj_counties[proj_counties$GEOID %in% catch_roswell, ]
proj_roswell <- sf::st_union(proj_roswell)
proj_roswell <- sf::st_intersection(proj_roswell, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_roswell))

# Harold C. Simmons Comprehensive Cancer Center at University of Texas Southwestern Medical Center
catch_simmons <- nci_catch$GeoID[nci_catch$Center == "Simmons"]
proj_simmons <- proj_counties[proj_counties$GEOID %in% catch_simmons, ] 
proj_simmons <- sf::st_union(proj_simmons)
proj_simmons <- sf::st_intersection(proj_simmons, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_simmons))

# Alvin J. Siteman Cancer Center at Washington University School of Medicine & Barnes-Jewish Hospital
catch_siteman <- nci_catch$GeoID[nci_catch$Center == "Siteman"]
proj_siteman <- proj_counties[proj_counties$GEOID %in% catch_siteman, ]
proj_siteman <- sf::st_union(proj_siteman)
proj_siteman <- sf::st_intersection(proj_siteman, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_siteman))

# Stanford Cancer Institute at Stanford University
catch_stanford <- nci_catch$GeoID[nci_catch$Center == "Stanford"]
proj_stanford <- proj_counties[proj_counties$GEOID %in% catch_stanford, ]
proj_stanford <- sf::st_union(proj_stanford)
proj_stanford <- sf::st_intersection(proj_stanford, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_stanford))

# Sylvester Comprehensive Cancer Center at University of Miami Miller School of Medicine
catch_sylvester <- nci_catch$GeoID[nci_catch$Center == "Sylvester"]
proj_sylvester <- proj_counties[proj_counties$GEOID %in% catch_sylvester, ]
proj_sylvester <- sf::st_union(proj_sylvester)
proj_sylvester <- sf::st_intersection(proj_sylvester, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_sylvester))

# Tisch Cancer Institute at Icahn School of Medicine at Mount Sinai
catch_tisch <- nci_catch$GeoID[nci_catch$Center == "Tisch"]
proj_tisch <- proj_counties[proj_counties$GEOID %in% catch_tisch, ]
proj_tisch <- sf::st_union(proj_tisch)
proj_tisch <- sf::st_intersection(proj_tisch, coast) # clip by US coastal boundary
proj_tisch <- sf::st_collection_extract(proj_tisch, "POLYGON") 
#plot(sf::st_geometry(proj_tisch))

# Arizona Cancer Center at Univerity of Arizona
catch_arizona <- nci_catch$GeoID[nci_catch$Center == "U. Arizona"]
proj_arizona <- proj_counties[proj_counties$GEOID %in% catch_arizona, ]
proj_arizona <- sf::st_union(proj_arizona)
proj_arizona <- sf::st_intersection(proj_arizona, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_arizona))

# University of Chicago Comprehensive Cancer Center
catch_chicago <- nci_catch$GeoID[nci_catch$Center == "U. Chicago"]
proj_chicago <- proj_counties[proj_counties$GEOID %in% catch_chicago, ]
proj_chicago <- sf::st_union(proj_chicago)
proj_chicago <- sf::st_intersection(proj_chicago, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_chicago))

# The University of Kansas Cancer Center at University of Kansas
catch_kansas <- nci_catch$GeoID[nci_catch$Center == "U. Kansas"]
proj_KS <- proj_counties[proj_counties$GEOID %in% seq(from = 20001, to = 20209, by = 2), ]
proj_MO <- proj_counties[proj_counties$GEOID %in% catch_kansas, ]
proj_kansas <- rbind(proj_KS, proj_MO)
proj_kansas <- sf::st_union(proj_kansas)
proj_kansas <- sf::st_intersection(proj_kansas, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_kansas))

# University of Virginia Cancer Center
catch_uva <- nci_catch$GeoID[nci_catch$Center == "U. Virginia"]
proj_uva <- proj_counties[proj_counties$GEOID %in% catch_uva, ] 
proj_uva <- sf::st_union(proj_uva)
proj_uva <- sf::st_intersection(proj_uva, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_uva))

# UC Davis Comprehensive Cancer Center at University of California Davis
catch_davis <- nci_catch$GeoID[nci_catch$Center == "U.C. Davis"]
proj_davis <- proj_counties[proj_counties$GEOID %in% catch_davis, ]
proj_davis <- sf::st_union(proj_davis)
proj_davis <- sf::st_intersection(proj_davis, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_davis))

## COMPLICATED ####

# Memorial Sloan-Kettering Cancer Center
msk_coordinates <- c(1, "MSK", 40.7641, -73.9568) 
center_df <- rbind.data.frame(msk_coordinates, make.row.names = F,
                              stringsAsFactors = F, deparse.level = 2)
colnames(center_df) <- c("ID", "Name", "Latitude", "Longitude")
center_df$Latitude <- as.numeric(center_df$Latitude)
center_df$Longitude <- as.numeric(center_df$Longitude)
center_df_sp <- sf::st_as_sf(x = center_df,                         
                             coords = c("Longitude", "Latitude"),
                             crs = 4326)
utm_msk <- sf::st_transform(center_df_sp, crs = 2829)
distConvert <- 1609.34  
buffer_msk <- sf::st_buffer(sf::st_as_sf(utm_msk), dist = 100*distConvert)
proj_msk <- sf::st_transform(buffer_msk, crs = 4326)
proj_msk <- sf::st_intersection(proj_msk, coast) # clip by US coastal boundary
#plot(sf::st_geometry(proj_msk))

# Laura & Isaac Perlmutter Cancer Center at NYU Langone Health
puma_nyc <- tigris::pumas("New York", class = "sf", cb = TRUE)
catch_nyu <- nci_catch$GeoID[nci_catch$Center == "Perlmutter"]
sub_nyu <- shp_counties[shp_counties$GEOID %in% catch_nyu, ]
puma_nyu <- puma_nyc[puma_nyc$GEOID10 %in% catch_nyu, ]
sub_perlmutter <-sf::st_union(sub_nyu, puma_nyu)
shp_perlmutter <- sf::st_union(sub_perlmutter)
proj_perlmutter <- sf::st_transform(shp_perlmutter, crs = 4326)
proj_perlmutter <- sf::st_intersection(proj_perlmutter, coast) # clip by US coastal boundary
proj_perlmutter <- sf::st_collection_extract(proj_perlmutter, "POLYGON") 
#plot(sf::st_geometry(proj_perlmutter))

# University of Hawai'i Cancer Center
## https://catalog.data.gov/dataset/territorial-sea-u-s-affiliated-pacifc-islands
## three geometries have an error (start and end coordinates are not the same)
## workaround in {sf} environment: small buffer around the three geometries
## https://www.r-spatial.org/r/2017/03/19/invalid.html

shp_hawaii <- st_read("http://geo.pacioos.hawaii.edu/geoserver/PACIOOS/pac_ocs_usa_territorial_sea/ows?service=WFS&version=1.0.0&request=GetCapabilities")
sf_hawaii <- sf::st_as_sf(shp_hawaii)
sf_hawaii[16,] <- sf::st_buffer(sf_hawaii[16,], 0.000001)
sf_hawaii[17,] <- sf::st_buffer(sf_hawaii[17,], 0.000001)
sf_hawaii[18,] <- sf::st_buffer(sf_hawaii[18,], 0.000001)
sf_hawaii <- sf::st_cast(sf_hawaii, "LINESTRING")
proj_hawaii <- sf::st_polygonize(sf_hawaii)
proj_hawaii <- sf::st_union(proj_hawaii) 
#plot(sf::st_geometry(proj_hawaii))

# COMBINE ####

# Cancer center names from "nci_centers" data
catchment_dat <- data.frame("id" = seq(1,length(nci_centers$name), 1), "name" = nci_centers$name)

# list of all NCI-designated cancer center catchment areas
catchment_pol <- lapply(list(proj_abramson, proj_einstein, proj_siteman, proj_arizona, proj_karmanos, proj_case,
                             proj_chao, proj_coh, proj_duncan, proj_farber, proj_duke, proj_foxchase,
                             proj_buffett, proj_hutchinson, proj_lombardi, proj_simmons, proj_irving, proj_holden,
                             proj_hollings, proj_huntsman, proj_simon, proj_jonsson, proj_knight, proj_perlmutter,
                             proj_markey, proj_masonic, proj_massey, proj_mayo, proj_mays, proj_msk,
                             proj_moffitt, proj_moores, proj_cotton, proj_oneal, proj_lurie, proj_roswell,
                             proj_cinj, proj_jefferson, proj_hopkins, proj_stanford, proj_stephenson, proj_sylvester,
                             proj_ohio, proj_tisch, proj_davis, proj_diller, proj_lineberger, proj_chicago,
                             proj_colorado, proj_hawaii, proj_kansas, proj_greenebaum, proj_rogel, proj_unm,
                             proj_anderson, proj_uva, proj_carbone, proj_hillman, proj_norris, proj_ingram,
                             proj_baptist, proj_winship, proj_yale),
                             function(x) sf::st_sf(geom = sf::st_geometry(sf::st_combine(sf::st_cast(x, "MULTIPOLYGON")))))

# Combine into one Simple feature collection (sfc)
catchment_pols <- dplyr::bind_rows(catchment_pol)

# Merge NCI-designated cancer center catchment center names
catchments <- sf::st_as_sf(cbind(catchment_dat, catchment_pols))

#################
# DATA FEATURES #
#################

# Merge information from data to new sfc
catchments$type <- as.character(nci_centers$type[match(catchments$name, nci_centers$name)])
catchments$latitude <- nci_centers$latitude[match(catchments$name, nci_centers$name)]
catchments$longitude <- nci_centers$longitude[match(catchments$name, nci_centers$name)]
catchments$address <- as.character(nci_centers$address[match(catchments$name, nci_centers$name)])
catchments$state <- as.character(nci_centers$state[match(catchments$name, nci_centers$name)])
catchments$year <- as.character(nci_centers$year[match(catchments$name, nci_centers$name)])
catchments$area <- units::set_units(sf::st_area(catchments), km^2)

###########
# CLEANUP #
###########

rm(list = setdiff(ls(), c("cancer_centers", "catchments", "not_l48", "proj_coast",
                          "proj_states", "proj_counties", "proj_l48_counties")))

# --------------- END OF CODE --------------- #
