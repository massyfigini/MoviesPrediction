# load libraries
library(shiny)
library(recommenderlab)
library(dplyr)
library(shinyjs)


# open data
load("ratings.Rdata") 
load("movies.Rdata") 
load("genres.Rdata") 


css <- '
#large .selectize-input { line-height: 40px; }
#large .selectize-dropdown { line-height: 30px; }

@media (min-width: 769px) and (min-height: 600px) {
  #p1 {display: none; }
  body {background: #000000 url("https://massyfigini.github.io/assets/css/images/movie_stage.jpeg") center center;
        background-position: center center;
        background-repeat: no-repeat;
        background-attachment: fixed;
        background-size: cover;
        margin-top: 7em;
        font-size: 150%;}
}
      
#Top0 {font-size: 200%; font-family: "Times New Roman", Times, serif;}
#Top1,#Top2,#Top3,#Top4,#Top5 {font-size: 150%; font-family: "Times New Roman", Times, serif;}'


# ui
ui <- fluidPage(
  
  shinyjs::useShinyjs(),
  
  #titlePanel(HTML("<font size=10 color=#484848><b><center>Which movies should I watch?</center></b></font><br/>"),windowTitle="Movies App by Massy"),
  #titlePanel(HTML("<font size=10 color=#484848><b><center> </center></b></font><br/><br/><br/>"),windowTitle="Movies App by Massy"),  
  titlePanel("",windowTitle="Movies App by Massy"), 
  
  # title only without images (little screen, see CSS)
  div(id="p1", HTML("<font size=10 color=#484848><b><center>Which movies should I watch?</center></b></font><br/>")),
    
  sidebarLayout(
    sidebarPanel(tags$style(".well {background-color:#d3d3d3;}"),
                 tabsetPanel(

                   # first tab: input
                   tabPanel(HTML("<font color=black>Input</font>")
                            ,tags$style(type='text/css', css)
                            ,HTML("<font color=black><br/>Select your favourite movie genre</font>")
                            ,selectizeInput(
                              inputId = "GEN", label = NULL, choices = c("",genres),
                              multiple = FALSE)
                            ,HTML("<font color=black>Select from one to five of your favourite movies of that genre</font>")
                            ,selectizeInput(
                              inputId = "TOP", label = NULL, choices = NULL, 
                              multiple = TRUE, options = list(openOnFocus = FALSE,  maxItems = 5, closeAfterSelect = TRUE))
                            
                            ,HTML("<font color=black>If you want, select up to five movies you don't like of that genre</font>")
                            ,selectizeInput(
                              inputId = "FLOP", label = NULL, choices = NULL,
                              multiple = TRUE, options = list(openOnFocus = FALSE,  maxItems = 5, closeAfterSelect = TRUE))
                            ,HTML("<br/>")
                            ,actionButton("go","Predict!", icon = icon("film"))
                            ,HTML("<br/><br/><a href=https://massyfigini.github.io/>by Massimiliano Figini</a>")),
                   
                   # second tab: instructions
                   tabPanel(HTML("<font color=black>Instructions</font>"),
                            HTML("<br/>In the first input box, select your favourite movie genre.<br/>
                                 In the second one, select up to five of your favourite movies of that genre. The more movies you select, the more accurate will be the prediction.<br/>
                                 In the third one, if you want you can select also up to five of the movies you don't like of that genre. If you select at least one movie you don't like, the prediction will be more accurate.<br/>
                                 Then click the button <q>Predict!</q>.<br/>
                                 The app will show you five movies you will love!<br/>
                                 <br/><br/><a href=https://massyfigini.github.io/>by Massimiliano Figini</a>")),
                 
                 # third tab: about
                 tabPanel(HTML("<font color=black>About</font>"),
                          HTML(" <br/><b>Credits</b><br/>
                                 This app was built by <a href=http://www.massimilianofigini.com>Massimiliano Figini</a> only for fun with R and the packages Shiny, Shinyjs, Recommenderlab and Dplyr on RStudio.
                                 The app is hosted for free on <a href=https://www.shinyapps.io/>shinyapps.io</a><br/>
                                 <br/><b>Data</b><br/>
                                 The data used for the algorithm are provided by MovieLens, you can download the data and see terms and conditions 
                                 <a href=https://grouplens.org/datasets/movielens/>here</a>. The dataset was generated on November 21, 2019.<br/>
                                 <br/><b>Algorithm</b><br/>
                                 The app records a 10/10 rating for the movies you like, 1/10 for the movies you don't like,
                                 and then compare this ratings with other millions provided by thousands MovieLens users using a UBCF algorithm.
                                 You can find all the code on my
                                 <a href=https://github.com/massyfigini/MoviesRecommendations>Github</a>.<br/>
                                 <br/><br/><a href=https://massyfigini.github.io/>by Massimiliano Figini</a>")))
    
                 ,width=4
                 ),
    
    mainPanel(
      htmlOutput("Top0"),
      htmlOutput("Top00"),
      htmlOutput("Top1"),
      htmlOutput("Top2"),
      htmlOutput("Top3"),
      htmlOutput("Top4"),
      htmlOutput("Top5"),
      #htmlOutput("Top6"),
      #htmlOutput("Top7"),
      #htmlOutput("Top8"),
      #htmlOutput("Top9"),
      #htmlOutput("Top10")
      #HTML("<br/><br/>"),
      #htmlOutput("Flop0"),
      #htmlOutput("Flop1"),
      #htmlOutput("Flop2"),
      #htmlOutput("Flop3"),
      #htmlOutput("Flop4"),
      #htmlOutput("Flop5")
   )))


# server
server <- function(input, output, session) {
  
  # disable textboxes at startup
  shinyjs::disable("TOP")
  shinyjs::disable("FLOP")
  
  observeEvent(input$GEN, {
    
            # open wait message  
            showModal(modalDialog("Please wait...", footer=NULL))
    
            # clean
            output$Top0 <- output$Top1 <- output$Top2 <- output$Top3 <- output$Top4 <- output$Top5 <-
            #output$Top6 <- output$Top7 <- output$Top8 <- output$Top9 <- output$Top10 <- 
            renderPrint({cat()})
            
            # filter only the selected genre
            filmstd <- movies %>% filter(grepl(input$GEN, genres)) %>% ungroup() %>% select(title)
            
            # fill the other boxes
            updateSelectizeInput(session, 'TOP', choices = filmstd$title, server = TRUE)
            updateSelectizeInput(session, 'FLOP', choices = filmstd$title, server = TRUE)
            
            # enable textboxes
            if(input$GEN == "") {
                shinyjs::disable("TOP")
                shinyjs::disable("FLOP")
            } else {
                shinyjs::enable("TOP")  
                shinyjs::enable("FLOP")
            }
        
            # close wait message
            removeModal()
            
            }, 
            
            # don't do this at startup
            ignoreInit = TRUE)
  

    
  observeEvent(input$go, {
    
      # open wait message  
      showModal(modalDialog("I'm working on the prediction...", footer=NULL))
    
      # clean
      output$Top0 <- output$Top1 <- output$Top2 <- output$Top3 <- output$Top4 <- output$Top5 <-
      #output$Top6 <- output$Top7 <- output$Top8 <- output$Top9 <- output$Top10 <- 
      renderPrint({cat()})

      # distinct the movies - after each bracket closed there's a new movie
      selectedT <- input$TOP
      selectedT <- gsub("([0-9][0-9][0-9][0-9]))", "\\1),,,", selectedT)  # add separator ,,, after the year
      selectedT <- as.character(selectedT)   
      selectedT <- strsplit(selectedT, ",,,")   # from string to list
      selectedT <- unlist(selectedT, use.names=FALSE)   # convert to vector
      selectedT <- trimws(selectedT)   # delete white spaces
      
      selectedF <- input$FLOP
      selectedF <- gsub("([0-9][0-9][0-9][0-9]))", "\\1),,,", selectedF)
      selectedF <- as.character(selectedF)   
      selectedF <- strsplit(selectedF, ",,,")
      selectedF <- unlist(selectedF, use.names=FALSE)
      selectedF <- trimws(selectedF)
      
      a <- input$GEN
      
      if(length(selectedT) > 0) {
        
            # check if there are movies also in top and flop
            #if(length(selectedT) > 1) {
            #  for(i in 2:nrow(IDselectedT)) {
            #    m <- rbind(m, c(999999,IDselectedT$movieId[i],5))
            #  }
            #}
            
            #if(length(selectedF) > 0) {
            #  for(i in 1:nrow(IDselectedF)) {
            #    m <- rbind(m, c(999999,IDselectedF$movieId[i],0.5))
            #  }
            #}
        
        
            # clean
            output$Top0 <- output$Top1 <- output$Top2 <- output$Top3 <- output$Top4 <- output$Top5 <- 
            #output$Top6 <- output$Top7 <- output$Top8 <- output$Top9 <- output$Top10 <- 
            renderPrint({cat()})
            
            
            # 0) filter movies table with the genre selected
            #g <- filter(movies, grepl(input$GEN, genres))
            #g <- inner_join(ratings,g,by="movieId")
            #ratingsgen <- select(g, userId, movieId, rating)
            
            
            # 0) Take the ID of the movies
            titleT <- data.frame(selectedT, stringsAsFactors=FALSE)
            colnames(titleT) <- 'title'
            IDselectedT <- select(inner_join(titleT,movies,by="title"),movieId)
            
            titleF <- data.frame(selectedF, stringsAsFactors=FALSE)
            colnames(titleF) <- 'title'
            IDselectedF <- select(inner_join(titleF,movies,by="title"),movieId)   
            
            
            # 1) in the ratings table I want only users that have rated that movies and only movies of the genre...
            g <- filter(movies, grepl(input$GEN, genres, fixed = TRUE))   # movies of our genre
            newratings <- select(inner_join(ratings,g,by="movieId"), userId, movieId, rating)    # only ratings of our genre
            IDselected <- rbind(IDselectedT, IDselectedF)   # selected only movies of interset
            userfil <- distinct(select(inner_join(newratings,IDselected),userId))    # filter only users have rated the movies of interest
            #userfil <- distinct(filter(newratings, movieId %in% IDselected$movieId),userId)
            newratings <- inner_join(newratings,userfil)
            #newratings <- filter(newratings, userId %in% userfil$userId)   # filter only movies of these users

            
            # 2) insert in the DB the movies inserted and associate them to a new user - NB ok until userID 999999, then index error
            m <- rbind(c(999999,IDselectedT$movieId[1],5))
            if(length(selectedT) > 1) {
              for(i in 2:nrow(IDselectedT)) {
                m <- rbind(m, c(999999,IDselectedT$movieId[i],5))
              }
            }
            
            if(length(selectedF) > 0) {
                for(i in 1:nrow(IDselectedF)) {
                  m <- rbind(m, c(999999,IDselectedF$movieId[i],0.5))
                }
            }

            colnames(m) <- c('userId','movieId','rating')
            m <- as.data.frame(m)
            newratings <- rbind(newratings,m)   # add new values to the dataframe
            newratings$movieId <- as.factor(newratings$movieId)   # convert ID to factors
            
            
            # 3) create matrix with users in row, movies in columns and ratings in cells
            affinitymatrix <- as(newratings,"realRatingMatrix")
            
            # UBCF model (User Based Collaborative Filtering)
            recmodel <- Recommender(affinitymatrix, method = "UBCF")
            
              
            # 4) best prediction for the user
            df1 <- as.data.frame(as(predict(recmodel, affinitymatrix["999999",], type="ratings"), "list"))

            coldf1 <- rownames(df1)
            rownames(df1) <- NULL
            df2 <- cbind(coldf1,df1)
            colnames(df2) <- c('movieId','rating')
            top <- top_n(arrange(df2,desc(rating)),10)
            #flop <- top_n(arrange(df2,desc(rating)),-5)
            movies$movieId <- as.character(movies$movieId)
            top$movieId <- as.character(top$movieId)
            #flop$movieId <- as.character(flop$movieId)
            topMovies <- inner_join(top, movies, by='movieId') %>% select(title,rating)
            #flopMovies <- inner_join(flop, movies, by='movieId') %>% select(title,rating)
            
            # convert to vector
            topVector <- topMovies[[1]]
            topVector <- as.character(topVector)
            #flopVector <- flopMovies[[1]]
            #flopVector <- as.character(flopVector)
            
            # OUTPUT!
            output$Top0 <- renderPrint(HTML("<br/>Watch these movies:<br/>"))
            output$Top00 <- renderPrint(HTML("<br/>"))
            output$Top1 <- renderPrint({cat(paste("1. ",topVector[1]))})
            output$Top2 <- renderPrint({cat(paste("2. ",topVector[2]))})
            output$Top3 <- renderPrint({cat(paste("3. ",topVector[3]))})
            output$Top4 <- renderPrint({cat(paste("4. ",topVector[4]))})
            output$Top5 <- renderPrint({cat(paste("5. ",topVector[5]))})
            #output$Top6 <- renderPrint({cat(paste("6. ",topVector[6]))})
            #output$Top7 <- renderPrint({cat(paste("7. ",topVector[7]))})
            #output$Top8 <- renderPrint({cat(paste("8. ",topVector[8]))})
            #output$Top9 <- renderPrint({cat(paste("9. ",topVector[9]))})
            #output$Top10 <- renderPrint({cat(paste("10. ",topVector[10]))})
            
            # HIDE MOVIE YOU DON'T LIKE FOR THE MOMENT
            #output$Flop0 <- renderPrint(HTML("<font size=5>Better avoid these:</font><br/>"))
            #output$Flop1 <- renderPrint({cat(flopVector[1])})
            #output$Flop2 <- renderPrint({cat(flopVector[2])})
            #output$Flop3 <- renderPrint({cat(flopVector[3])})
            #output$Flop4 <- renderPrint({cat(flopVector[4])})
            #output$Flop5 <- renderPrint({cat(flopVector[5])})
            
            # close wait message
            removeModal()

      } else {
        
            # clean
            output$Top0 <- output$Top1 <- output$Top2 <- output$Top3 <- output$Top4 <- output$Top5 <-
            #output$Top6 <- output$Top7 <- output$Top8 <- output$Top9 <- output$Top10 <- 
            renderPrint({cat()})
            #>output$Flop0 <- output$Flop1 <- output$Flop2 <- output$Flop3 <- output$Flop4 <- output$Flop5 <- 
            
            # close wait message
            removeModal()
            
            # Error
            showModal(modalDialog(div("Please select a genre and at least one movies you like in the first box!", size = "l", style="font-size:120%"), easyClose = TRUE))

            #output$Top0 <- renderPrint(HTML("<font size=5>Please select a genre and at least one movies you like in the first box!</font><br/>"))
      }  
  })
  
}


shinyApp(ui = ui, server = server)
