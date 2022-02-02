# ------------------------------------------------------------------------------ #
# Hexsticker for the GitHub Repository idblr/NCI_Cancer_Center_Catchment_Areas
# ------------------------------------------------------------------------------ #
#
# Created by: Ian Buller, Ph.D., M.A. (GitHub: @idblr)
# Created on: February 1, 2022
#
# Recently modified by:
# Recently modified on:
#
# Notes:
# A) Uses the "hexSticker" package
# B) Modified image: https://openclipart.org/detail/325249/cartoon-crab within the creative commons
# C) Image attribution: @liftarn https://openclipart.org/artist/liftarn
# D) Hexsticker for the GitHub Repository https://github.com/idblr/NCI_Cancer_Center_Catchment_Areas
# ------------------------------------------------------------------------------ #

# Packages
library(hexSticker)

# Image file
path_image <- "hex/crab.png"

# Create hexSticker
s <- hexSticker::sticker(subplot = path_image,
                         package = "NCI-Designated\nCancer Center Catchment Areas",
                         p_size = 2.85, p_x = 1, p_y = 1.5, p_color = "#0C3333", # title
                         s_x = 1, s_y = 0.8, s_width = 0.67, s_height = 0.67, # symbol
                         h_fill = "#FDF7F3", # inside
                         h_color = "#BD7650", # outline
                         dpi = 1000, # resolution
                         filename = "hex/hex.png",
                         white_around_sticker = F)

# -------------------------------- END OF CODE --------------------------------- #
