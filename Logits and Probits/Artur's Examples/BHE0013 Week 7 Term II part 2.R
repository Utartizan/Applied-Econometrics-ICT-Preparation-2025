# What if coefficients we obtain do not make a lot of (direct) sense? 
# ============================================================================= #
set.seed(1234)                                                                  # set seed for replicability 
# install.packages("neuralnet")
# install.packages("caret")
#
# load packages; install if not previously installed
library(neuralnet)
library(caret)
#
# load the data set
data("iris")
?iris
#
# split sample into training and testing datasets and evaluate nn performance;
# note that similar can be done for logit or probit settings in the part I
train_id=sample(1:150, size=100)                                                # training id
iris_train=iris[train_id,]                                                      # training set                                                    
iris_test=iris[-train_id,]                                                      # testing set 
#
# train a simple nn on the training dataset
nn1=neuralnet(Species~., data = iris_train, hidden = c(5,3))
# 
# plot constructed nn; is it informative?
plot(nn1)
#
# obtain predictions for the testing dataset
nn1.pred=compute(nn1, iris_test)
nn1.pred.cat=apply(nn1.pred$net.result, 1, which.max)
#
# compute confusion matrix to evaluate out-of-sample accuracy
table(nn1.pred.cat, as.numeric(iris_test$Species))                              # manual confusion matrix
confusionMatrix(factor(nn1.pred.cat), factor(as.numeric(iris_test$Species)))    # confusion matrix form caret package 
#
# ============================================================================= #
# What if the independent variables do not (necessarily) make direct sense?
# This is almost exact replica from:
# https://tensorflow.rstudio.com/tutorials/beginners/basic-ml/tutorial_basic_classification/
# to expand your perspective on the modern modelling demands
#
# install.packages("tensorflow")
# install.packages("keras")
#
# load Tensorflow; install if not installed previously
library(tensorflow)
# 
# link Tensorflow to python installation
# install_tensorflow(method="conda", conda="C:/Users/Staff/anaconda3/condabin")
#
# check if Tensorflow functions now
tf$constant("Hellow Tensorflow")
#
# load keras library; install if not installed previously
library(keras)
#
# tensorflow and keras require good level of R syntax and originally were 
# developed for python; now available for R and hence, you could always explore 
# this on your own
#
# load dataset and split it into training and testing sets
fdata=dataset_fashion_mnist()
c(train_images, train_labels) %<-% fdata$train
c(test_images, test_labels) %<-% fdata$test
#
# assign actual labels to the images
class_names=c("T-shirt/top", "Trouser", "Pullover",
              "Dress", "Coat", "Sandal", "Shirt", 
              "Sneaker", "Bag", "Ankle Boot")
#
# scale images by the total number of pixeles in each image
train_images=train_images/255
test_images=test_images/255
#
# plot the dataset
par(mfrow=c(5,5))
for (i in 1:25){
  img=train_images[i, ,]
  img=t(apply(img, 2, rev))
  image(1:28, 1:28, img, col=gray((0:255)/255), xaxt="n", yaxt="n", 
                    main=paste(class_names[train_labels[i]+1]))
}
#
# specify the cnn model
model1=keras_model_sequential(layers=list(
                             layer_flatten(input_shape=c(28, 28)), 
                             layer_dense(units=128, activation = "relu"),
                             layer_dense(units=10,  activation="softmax" )
                             ) 
)
# 
# and compile the cnn model
compile(model1, optimizer="adam", loss="sparse_categorical_crossentropy",
               metrics=c("accuracy")
        )
# estimate the cnn
fit(model1, train_images, train_labels, epochs=10, verbose=2)
#
# obtain and evaluate cnn performance
score1=evaluate(model1, test_images, test_labels, verbose=2)
predictions1=predict(model1, test_images)
#
which.max(predictions1[2,])
#
# ============================================================================= #
# 
# Visual illustration of the cnn model in action
par(mfrow=c(5,5))
for (i in 1:25){
  img=test_images[i, ,]
  img=t(apply(img, 2, rev))
  predicted_label=which.max(predictions1[i,])-1
  true_label=test_labels[i]
  if (predicted_label==true_label){
    color="green"
  } else {
    color="red"
  }
  image(1:28, 1:28, img, col=gray((0:255)/255), xaxt="n", yaxt="n", 
        main=paste0(class_names[predicted_label+1], "(", 
                    class_names[true_label+1], ")"), col.main=color)
}
# ============================================================================= #