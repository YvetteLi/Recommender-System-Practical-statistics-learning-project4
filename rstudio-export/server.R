
library(Matrix)
library(stringr)


get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}


myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
ratings$Timestamp = NULL

i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)
# Rmat = new('realRatingMatrix', data = Rmat)
# writeMM(obj=Rmat, "rmat.mtx")
Rmat = new('realRatingMatrix', data = Rmat)
Rmat = Rmat[1:500, ] # let's use a subset
Rmat = as(Rmat, "dgCMatrix")
movieIDs = colnames(Rmat)
n.item = ncol(Rmat)  
Rmat = new('realRatingMatrix', data = Rmat)

rec_UBCF = Recommender(Rmat, method = 'UBCF',
                       parameter = list(normalize = 'Z-score', 
                                        method = 'Cosine', 
                                        nn = 25))

rec_popular = Recommender(Rmat, method = 'POPULAR')
output_ratings = as(rec_popular@model$ratings, "matrix")
output_topN = rec_popular@model$topN@items[[1]]

# read in data
movies = read.table("movies.data")
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))
unique_genres = unique(sort(unlist(strsplit(movies$Genres, "\\|"))))
tmp1 = read.table("pop_recom.RData", stringsAsFactors=FALSE, header=TRUE)
rep_str = c("'"='.','-'='.')
unique_genres = str_replace_all(unique_genres, rep_str)
shinyServer(function(input, output, session) {
  # show the books to be rated
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 6 # movies per row
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                 #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                 div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  
  # Calculate recommendations when the sbumbutton is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      output_ratings = as(rec_popular@model$ratings, "matrix")
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      # print(user_ratings)
      sel_movies = unique((unlist(strsplit( movies[movies$MovieID %in% user_ratings$MovieID, 'Genres'], "\\|"))))
      # select movies that other genres are 0
      sel_movie_list = tmp1[which(rowSums(tmp1[, unique_genres[!unique_genres %in% sel_movies]]) == 0), ]$MovieID
      if  (length(sel_movie_list) < 10){
        extra_movies = tmp1[which(rowSums(tmp1[, unique_genres[!unique_genres %in% sel_movies]]) >= 0), ]$MovieID
        extra_movies = extra_movies[1:10-length(sel_movie_list)]
      }
      sel_movie_ids = paste0('m', sel_movie_list)
      # reserve the values of only the eligible movies
      output_ratings[, setdiff(colnames(output_ratings), sel_movie_ids)] = -Inf
        
      if (nrow(user_ratings) < 5) {
        # provide top 10 popualarity result 
        
        # output_ratings = output_ratings[,!is.na(output_ratings)]
        output_rating_list = as.list(output_ratings)
        names(output_rating_list) = colnames(output_ratings)
        user_predicted_ids = output_rating_list[order(unlist(output_rating_list),decreasing=TRUE)[1:10]]
        user_results = output_ratings[,names(user_predicted_ids)]
        user_predicted_ids = as.integer(str_replace(names(user_predicted_ids), "m", ''))
        # print(user_predicted_ids)
        
      } else {
        
        user_ratings_mat = rep(NA, n.item)
        user_ratings_mat[user_ratings$MovieID] = user_ratings$Rating
        
        
        new.user = matrix(user_ratings_mat, 
                          nrow=1, ncol=n.item,
                          dimnames = list(
                            user=paste('dummyuser'),
                            item=movieIDs
                          ))
        
        new.Rmat = as(new.user, 'realRatingMatrix')
        # print (new.Rmat)
        
        recom1 = predict(rec_UBCF, new.Rmat, type = 'topN')
        recom2 = predict(rec_UBCF, new.Rmat, type = 'ratings')
        
        recom2 = as(recom2, "matrix")
        if (sum(!is.na(recom2)) == 0){
          user_predicted_ids = recom1@items
          user_results = recom1@ratings
        } else {
          user_predicted_ids = order(recom2, decreasing = TRUE)[1:20]
          user_results = recom2[user_predicted_ids]
          if (length(user_predicted_ids) != length(user_results)){ 
            user_predicted_ids = recom1@items
            user_results = recom1@ratings
          }
        }
        
        
        
      }


      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids], 
                                  Predicted_rating =  user_results)
      
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function
  
}) # server function