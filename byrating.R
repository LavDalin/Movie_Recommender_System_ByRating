library(recommenderlab)
library(reshape2)
readData <- function(){
  
  ratingDF <- read.delim("C:/Users/AJITH/Desktop/ml-100k/u.data", header=F)
  colnames(ratingDF) <- c("userID","movieID","rating")
  
  ## read movie data
  moviesDF <- read.delim("C:/Users/AJITH/Desktop/ml-100k/u.item", sep="|", header=F, stringsAsFactors = FALSE)
  colnames(moviesDF)[colnames(moviesDF)=="V1"] <- "movieID" #Retrieve or set the row or column names of a matrix-like object.
  colnames(moviesDF)[colnames(moviesDF)=="V2"] <- "name"
  
  return(list(ratingDF=ratingDF, movieDF=moviesDF)) 
}

preProcess = function(ratingDF, moviesDF)
{ 
  ratingDF[,2] <- dataList$movieDF$name[as.numeric(ratingDF[,2])]
  
  # remove duplicate entries for any user-movie combination
  ratingDF <- ratingDF[!duplicated(ratingDF[,1:2]),]
}
createRatingMatrix <- function(ratingDF)
{
  # converting the ratingData data frame into rating marix
  ratingDF_tmp <- dcast( ratingDF, userID ~ movieID, value.var = "rating" , index="userID")
  ratingDF <- ratingDF_tmp[,2:ncol(ratingDF_tmp)]
  
  ratingMat <- as(ratingDF, "matrix")  ## cast data frame as matrix
  movieRatingMat <- as(ratingMat, "realRatingMatrix")   ## create the realRatingMatrix
  ### setting up the dimnames ###
  dimnames(movieRatingMat)[[1]] <- row.names(ratingDF)
  return (movieRatingMat)
}
dataList<- readData()
# data cleansing and preprocessing
ratingDF<- preProcess(dataList$ratingDF, dataList$movieDF)
# create movie rating matrix
movieRatingMat<- createRatingMatrix(ratingDF)

#checking
print(dataList)
print(ratingDF)
print(movieRatingMat)


#Convert rating matrix into a recommenderlab sparse matrix
movieRatingMat <- as(movieRatingMat, "realRatingMatrix")
#normalizing
normalize(movieRatingMat)
 #checking 
print(movieRatingMat)

evaluateModels <- function(movieRatingMat)
{
  
  ## Find out and anlayse available  recommendation algorithm option for realRatingMatrix data
  recommenderRegistry$get_entries(dataType = "realRatingMatrix")
  
  scheme <- evaluationScheme(movieRatingMat, method = "split", train = 0.8,
                             k = 1, given = 10, goodRating = 4) #training data 80% and testing data 20%
  
  algorithms <- list(
    RANDOM = list(name="RANDOM", param=NULL),
    POPULAR = list(name="POPULAR", param=NULL),
    UBCF = list(name="UBCF", param=NULL)
  )
  
  # run algorithms, predict next n movies
  results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
  ## select the first results
  return (results)
 }

 evalList <- evaluateModels(movieRatingMat)
 print(evalList)
 

recommendations <- function(movieRatingMat, model, userID, n)
{
  
recommend <- predict(recommender_model, movieRatingMat[userID], n=n) #Obtain top 10 recommendations for 1st user in dataset
as(recommend, "list") #convert recommenderlab object to readable list
}

recommender_model <- Recommender(movieRatingMat, method = "UBCF", param=list(method="Cosine",nn=30))
userID <- readline(prompt = " enter the userid :")
topN <- readline(prompt = " enter no.of movies u want :")
recommendations(movieRatingMat,recommender_model, userID, topN)



#evaluation using cross-validation

evaluation_scheme <- evaluationScheme(movieRatingMat, method="cross-validation", k=5, given=3, goodRating=5)
#k=5 meaning a 5-fold cross validation. given=3 meaning a Given-3 protocol
evaluation_results <- evaluate(evaluation_scheme, method="UBCF", n=c(1,3,5,10,15,20))


visualise <- function(evaluation_results)
{
  # Draw ROC curve
  plot(evaluation_results, annotate = 1:3, legend="topright")
  
  # See precision / recall
  plot(evaluation_results, "prec/rec", annotate=3, legend="topright", xlim=c(0,.22))
}

visualise(evaluation_results)
getConfusionMatrix(evaluation_results)[[1]]

