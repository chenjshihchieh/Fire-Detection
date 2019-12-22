#loading the necessary packages

necessary_packages <- c("keras", "tidyverse", "magick", "pixmap")
for(i in necessary_packages){
if(!require(necessary_packages, character.only = TRUE)) {
  install.packages(necessary_packages)
  require(necessary_packages, character.only = TRUE)
  }
}

no_fire <- list.files("Fire-Detection/0")
fire <- list.files("Fire-Detection/1")