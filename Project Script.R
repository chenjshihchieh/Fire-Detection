####loading the necessary packages####

necessary_packages <- c("keras", "tidyverse", "magick", "pixmap", "randomForest")
for(i in necessary_packages){
  if(!require(i, character.only = TRUE)) {
    install.packages(i)
    require(i, character.only = TRUE)
  }
}
rm(necessary_packages)

##Setting up the necessary file paths
#Setting up the names for the files paths to the images
nofire_filepath <- "Fire_Images/0/"
fire_filepath <- "Fire_Images/1/"

#Getting the list of 
nofire_list <- list.files(nofire_filepath)
fire_list <- list.files(fire_filepath)


####Testing and playing with available functions####
##Testing out reading images
image_test <- image_read(paste0(nofire_filepath, nofire_list[1]))

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


#Testing out image_info function
x <- image_info(image_test) #Pulls information of the image
rbind(x, image_info(image_test)) #You can bind the information to a data frame and turn them into rows

##Creating a list of information of the image (Format, Width, Height, Colorspace, Matte, Filesize, and Density)
#A function to simplify creating the filepaths for each image
ImageFilepathGenerator <- function(path, image_name){
  image_list <- list()
  for(i in image_name){
    x <- paste0(path, i)
    image_list <- append(image_list, x)
  }
  return(unlist(image_list))
}

#The list of path to the images with Fire and Image without Fire
filepaths_nofire_list <- ImageFilepathGenerator(nofire_filepath, nofire_list)
filepaths_fire_list <- ImageFilepathGenerator(fire_filepath, fire_list)




#A function that will generate a list of information given the filepaths
PathToInfo <- function(filepath){
  x <- image_info(image_read(filepath[1]))
  for(i in filepath[-1]){
    x <- rbind(x, image_info(image_read(i)))
  }
  return(x)
}

#Using the function to obtain the information from the nofire images
information_of_image <- PathToInfo(filepaths_nofire_list)

#appending the information from the fire images to information_of_image
information_of_image <- rbind(information_of_image, PathToInfo(filepaths_fire_list))

#In total, there are 651 fire and nofire images
length(filepaths_nofire_list) + length(filepaths_fire_list)
nrow(information_of_image) #there are 651 rows in information_of_image so we have all the rows

#A rough look at the summary of the information
summary(information_of_image)
hist(information_of_image$width)
hist(information_of_image$height)
information_of_image %>% ggplot(aes(width, height)) +
  geom_point() #looks like the images are generally wider than they are tall but the dimensions of the image arent all that uniform
information_of_image %>% mutate(ratio = width/height) %>% 
  ggplot(aes(ratio)) + geom_histogram(bins = 20) #From the graph, it looks like there are a few images with more extreme ratios


#There are a lot of data sets that have extremely high resolution. 
#Lets cap the resolution so that the longer side is at 640 and shorter size is adjusted to maintain the ratio of the original image

####Data Wrangling####

#A function to simplify creating the filepaths for each image
Imagefilepath_generator <- function(path, image_name){
  image_list <- list()
  for(i in image_name){
    x <- paste0(path, i)
    image_list <- append(image_list, x)
  }
  return(unlist(image_list))
}

#Creating the paths to each image
ListPaths_NoFire <- Imagefilepath_generator(nofire_filepath, nofire_list)
ListPaths_Fire <- Imagefilepath_generator(fire_filepath, fire_list)


#Function to create index for generating train and test set
Generate_Index <- function(listpath, seed = 200, sections = 10){
  length <- length(listpath)
  folds <- cut(1:length, breaks = sections, labels = FALSE)
  set.seed(seed)
  index <- sample(folds, length(folds), replace = FALSE)
  return(index)
}

#While resizing the image, might as well randomly sort them into testing and training sets
ImageResizor <- function(listpath1, listpath2){
  #Combinging the two lists
  whole_list <- c(listpath1, listpath2)
  
  #Creating the indices for splitting images into training and testing sets
  index <- c(Generate_Index(listpath1, seed = 1225), Generate_Index(listpath2), seed = 1226)
  
  index[index != 1] <- "train"
  index[index == 1] <- "test"
  
  for(i in seq(length(index))){
    
    image_read(whole_list[i]) %>%
      image_resize("640>") %>%
      image_resize("x640>") %>%
      image_write(path = paste0("Fire_Images/Resized Images/", index[i],"/", 
                                if(str_detect(whole_list[i], "/1/")){"fire"}else{
                                  "nofire"
                                },
                                i, str_extract(whole_list[i], "\\.\\w*")))
    
  }
}

ImageResizor(ListPaths_Fire, ListPaths_NoFire)

#Now that the images are divided into test and train packages,
#We need to extract the necessary information from the images for data analysis.

#Creating a function to transform the data into analyzable rows
#Instead of analyzing every pixel as their own variable,
#I am dividing the images by region and using the average value of those regions as predictor
Image.to.Vector <- function(filepath){
  data <- data.frame()
  for(i in filepath){
    image <- image_read(filepath)
    summarized_pixel_data <- data.frame()
    # The two for loops cycles through the different regions of an image
    
    for(y in seq(0, 90, 10)){
      for(x in seq(0, 90, 10)){
        resized_numerical <- image %>% image_crop(paste0("10%x10%+", x,"%", "+", y,"%")) %>%
          .[[1]] %>% as.numeric(.)
        
        channel_average_by_column <- NULL
        
        for(channel in seq(3)){
          channel_average_by_column <- cbind(channel_average_by_column, mean(resized_numerical[,,channel]))
          
        }
        summarized_pixel_data <- rbind(summarized_pixel_data, channel_average_by_column)
        
      }
    }
    summarized_pixel_data_rearranged <- c(summarized_pixel_data$V1, summarized_pixel_data$V2, summarized_pixel_data$V3, class = str_detect(filepath, "/fire"))
    data <- rbind(data, summarized_pixel_data_rearranged)
    names(data) <- c(paste0(rep(c("R", "G", "B"), each = 100), 1:100), "Class")
  }
  return(data)
}

#Generating the filepaths for the images
testimage_filepath <- "Fire_Images/Resized Images/test/"
trainimage_filepath <- "Fire_Images/Resized Images/train/"

testimage_filenames <- list.files(testimage_filepath)
trainimage_filenames <- list.files(trainimage_filepath)

testlist <- paste0(testimage_filepath, testimage_filenames)
trainlist <- paste0(trainimage_filepath, trainimage_filenames)
rm(testimage_filepath, trainimage_filepath, testimage_filenames, trainimage_filenames)

train <- Image.to.Vector(trainlist)
test <- Image.to.Vector(testlist)

####Analyzing the image####
rforest_fit <- randomForest(class~., data = train)
confusionMatrix(predict(rforest_fit, test[,-301]), test$class)

lm_fit <- lm(class~., data = train)
confusionmatrix(predict(lm_fit, test[,-301]), test$class)


