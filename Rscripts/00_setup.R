# Set up the repo

# Load packages, prep folders and paths -----------------------------------
pacman::p_load("tidyverse", "sf", "readxl", "llamar", "lubridate", "hrbrthemes", 
               "extrafont", "extrafontdb")

dir.create("Data")
dir.create("Articles")
dir.create("Graphics")
dir.create("Dataout")
dir.create("Rscripts")
dir.create("docs")
dir.create("Images")

# Set up file path shortcuts
datapath <- "Data"
dataout <- "Dataout"
gispath <- "Data/GHA_adm"
imagepath <- "Images"
rpath <- "Rscripts"