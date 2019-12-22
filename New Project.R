#loading the necessary packages

necessary_packages <- c("keras", "tidyverse", "magick", "pixmap")
for(i in necessary_packages){
  if(!require(i, character.only = TRUE)) {
    install.packages(i)
    require(i, character.only = TRUE)
  }
}

#Setting up the names for the files paths to the images
NoFire_filepath <- "Fire_Images/0/"
Fire_filepath <- "Fire_Images/1/"

#Getting the list of 
NoFireList <- list.files(NoFire_filepath)
FireList <- list.files(Fire_filepath)

#Testing out reading images
image_test <- image_read(paste0(NoFire_filepath, NoFireList[1]))

#Looking at the structure of the read image
str(image_test) #class 'magick-image'
summary(image_test) #not much information here

#trying to call image_test
image_test #seems to produce the image in viewer tab

#This will produce the bitmap
image_test[[1]]

#looking at the bitmap dimensions
dim(image_test[[1]]) #it is 3 by 1500 by 1087
str(image_test[[1]])

#There is too many pixel information to be analyzed so reducing pixel information
resized_image <- image_resize(image_test, "10%x10%")
dim(resized_image[[1]])
numerical_resized <- as.numeric(resized_image[[1]])
str(numerical_resized) #looks like it became a matrix and [height, width, channel]

#Testing image read on a matrix of numeric
image_read(numerical_resized) #seems like it is possible to read image from numerical data

#Testing what the three channels are
#Testing channel 1
testing_channels <- numerical_resized
testing_channels[,,-1] <- 0
image_read(testing_channels) #1 is Red

#Testing channel 2
testing_channels <- numerical_resized
testing_channels[,,-2] <- 0
image_read(testing_channels) #2 is Green

#Testing channel 3
testing_channels <- numerical_resized
testing_channels[,,3] <- 0
image_read(testing_channels) #3 is blue

#Changing the 3 dimensional matrix to a long vector
vector_resized <- as.vector(numerical_resized)
length(vector_resized)




#The function will pull the images from the indicated filepath, extract the pixel
#information from three colour channels (red, blue, yellow), and lay them out in a vector
ImagetoVector <- function(ImageNameList, Folderpath){
  
  bitmap <- list()
  
  for i in ImageNameList{
    image <- read_image(paste0(Folderpath,"/",i))
    #Find out how the files are organized and extract the bit data
    bitmap <- image[[1]]
    
  }
  
  
}

#Loading in the images
NoFireImages <- LoadingImages(NoFireList, NoFire_filepath)
FireImages <- LoadingImages(FireList, Fire_filepath)

#Creating a train and test dataset
#Function for creating the data sets
Dataset.Creator <- function(data, Training.set, seed = 200){
  
  folds <- cut(1:length(data), breaks = 10, label = FALSE
               set.seed(seed)  
               test.index <- sample(folds, length(data), replace = FALSE)
               
               if(Training.set = TRUE){
                 train.set <- data[
                   
               }else{
               }
}