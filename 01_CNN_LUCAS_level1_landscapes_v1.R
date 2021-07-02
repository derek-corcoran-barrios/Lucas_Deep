### Keras model on LUCAS sample for image recognition of level 1 woodland land cover classifications
# Author: Naia Morueta-Holme
# Date: June 7 2021

#---------------------#
# Load libraries
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
#---------------------#


# Downloaded sample of LUCAS woodland photos for year 2018
# 7 woodland classes with ~600 training and 15 testing photos each

# Set working directory to folder with images
setwd("C://Users/vwn831/ImageRecognition/lucas01")

########### PREPROCESSING #################

## get list of names (based on folder names) and save list
label_list <- dir("train/")
output_n <- length(label_list)
save(label_list, file="label_list.RData")


## set target size for rescaling images (in pixels)
width <- 224
height<- 224
target_size <- c(width, height)
rgb <- 3 #color channels


## preprocess images
path_train <- "train" #training data path - NOTE: changed from "/train/" in tutorial, as next functions were not finding images
train_data_gen <- image_data_generator(rescale = 1/255, 
                                       validation_split = .2) #rescale pixel values to between 0 and 1 and reserve 20% of images for validation (let's keras reserve via random split)


## batch-process images with generator function
# create object for training data
train_images <- flow_images_from_directory(path_train,
                                           train_data_gen,
                                           subset = 'training', #if manual separation of training and test data, change this to training file path
                                           target_size = target_size,
                                           class_mode = "categorical",
                                           shuffle=F,
                                           classes = label_list, #this tells the function to use folder names as class labels (tags)
                                           seed = 2021)

# create object for validation data
validation_images <- flow_images_from_directory(path_train,
                                                train_data_gen, 
                                                subset = 'validation', #if manual choice of test data, change this to validation file path
                                                target_size = target_size,
                                                class_mode = "categorical",
                                                classes = label_list,
                                                seed = 2021)

## check things worked so far
table(train_images$classes) # should correspond to number of pictures in each folder
plot(as.raster(train_images[[1]][[1]][17,,,])) #plot image 17 (first element in train_image has pixel values of each image, defined as 4D-tensor (number of image, width, height, rgb channel))

## side note - fixing missing SciPy package to plot image and run the training later
## # install missing python package for plotting
## py_install("SciPy")
## scipy <- import("scipy")
## had to restart R to get scipy to be recognized


############# LOADING AND ADJUSTING MODEL ##############
## load pre-trained model and define model function

# load pre-trained model on ImageNet dataset
# final layer classifies the images in ImageNet dataset, so include_top = F allows to train on own dataset
mod_base <- application_xception(weights = 'imagenet', 
                                 include_top = FALSE, input_shape = c(width, height, 3))
freeze_weights(mod_base)

# function to build layer on top of pre-trained network and allow setting some model tuning parameters
model_function <- function(learning_rate = 0.001, 
                           dropoutrate=0.2, n_dense=1024){
  
  k_clear_session()
  
  model <- keras_model_sequential() %>%
    mod_base %>% 
    layer_global_average_pooling_2d() %>% 
    layer_dense(units = n_dense) %>%
    layer_activation("relu") %>%
    layer_dropout(dropoutrate) %>%
    layer_dense(units=output_n, activation="softmax")
  
  model %>% compile(
    loss = "categorical_crossentropy",
    optimizer = optimizer_adam(lr = learning_rate),
    metrics = "accuracy"
  )
  
  return(model)
  
}


## check model architecture
model <- model_function()
model

########### TRAIN MODEL ###################
## let's go - train the model
batch_size <- 32
epochs <- 6 #increase if suspecting validation accuracy would increase beyond 6 steps - decrease if model already over-fitting (validation value accuracy decreases while training accuracy further increases)

hist <- model %>% fit(
  train_images,
  steps_per_epoch = train_images$n %/% batch_size, 
  epochs = epochs, 
  validation_data = validation_images,
  validation_steps = validation_images$n %/% batch_size,
  verbose = 2
)
# should take up to an hour on laptop
# NOTE: warning message suggests using "fit()" instad, as "fit_generator()" is deprecated

########### EVALUATE AND TEST MODEL ###########
# Evaluate and test model
path_test <- "test"

test_data_gen <- image_data_generator(rescale = 1/255)

test_images <- flow_images_from_directory(path_test,
                                          test_data_gen,
                                          target_size = target_size,
                                          class_mode = "categorical",
                                          classes = label_list,
                                          shuffle = F,
                                          seed = 2021)

model %>% evaluate(test_images, 
                   steps = test_images$n)
## warning message: "ING:tensorflow:Your input ran out of data; interrupting training. Make sure that 
## your dataset or generator can generate at least `steps_per_epoch * epochs` batches 
## (in this case, 200 batches). You may need to use the repeat() function when building your dataset."
# ----- not sure what the error means, but output was similar to tutorial blog.... 


############ CHECK TEST FILES WERE NOT USED IN TRAINING #################

i<-1
for(i in 1:length(label_list)) {
 t1<-dir(paste0("train/",label_list[i]))
 t2<-dir(paste0("test/",label_list[i]))
 if(any(t2%in%t1)) {
   file.remove(paste0("test/",label_list[i],"/",t2[which(t2%in%t1)]))
   print(paste("removed files:", length(which(t2%in%t1))))
 } else {
   print("no duplicate files")
 }
}


#----------------------------------------------------------------------#



# Test on one image
test_image <- image_load("Test/Broadleaved woodland/31062100_east_2018.jpg",
                         target_size = target_size)

x <- image_to_array(test_image)
x <- array_reshape(x, c(1, dim(x)))
x <- x/255
pred <- model %>% predict(x)
pred <- data.frame("Land cover" = label_list, "Probability" = t(pred))
pred <- pred[order(pred$Probability, decreasing=T),][1:5,]
pred$Probability <- paste(format(100*pred$Probability,2),"%")
pred


## Test accuracy accross all the land cover types
predictions <- model %>% 
  predict_generator(
    generator = test_images,
    steps = test_images$n
  ) %>% as.data.frame

names(predictions) <- paste0("Class",0:6)

predictions$predicted_class <- 
  paste0("Class",apply(predictions,1,which.max)-1)
predictions$true_class <- paste0("Class",test_images$classes)

predictions %>% group_by(true_class) %>% 
  summarise(percentage_true = 100*sum(predicted_class == 
                                        true_class)/n()) %>% 
  left_join(data.frame(lc= names(test_images$class_indices), 
                       true_class=paste0("Class",0:6)),by="true_class") %>%
  select(lc, percentage_true) %>% 
  mutate(lc = fct_reorder(lc,percentage_true)) %>%
  ggplot(aes(x=lc,y=percentage_true,fill=percentage_true, 
             label=percentage_true)) +
  geom_col() + theme_minimal() + coord_flip() +
  geom_text(nudge_y = 3) + 
  ggtitle("Percentage correct classifications by land cover label")

