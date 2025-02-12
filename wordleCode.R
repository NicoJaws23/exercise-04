library(tidyverse)
library(sjmisc)

load_dictionary <- function(x, name) {
  name <- read.table(x, header = TRUE)
}
valid_list <- load_dictionary("collins-scrabble-words-2019.txt")
#starts at 9884 obervations
solution_list <- load_dictionary("google-10000-english-usa-no-swears.txt")
str(valid_list)
str(solution_list)

solution_list <- intersect(valid_list, solution_list) #brings it down to 8336 obs

#this function picks a random word from the list that is 5 characters long
pick_solution <- function(x){
  d <- x |> filter(str_length(x[,1]) == 5)
  w <- sample(as.character(d[,1]), 1)
  w <- str_split_1(w, "")
  return(w)
}
#this variable applies the function to the solution list
solution <- pick_solution(solution_list)

#this function will be called in the play_wordle function to evaluate the users guess
evaluate_guess <- function(guessSplit, solution){
  wordLength <- length(solution)
  eval <- rep("-", wordLength)
  for (i in 1:wordLength) {
    if(guessSplit[i] == solution[i]) {
      eval[i] <- "*"
      solution[i] <- "-"
    }
  }
  for (i in 1:wordLength){
    if(eval[i] != "*"){
      check <- match(guessSplit[i], solution)
      if(!is.na(check)){
        eval[i] <- "+"
        solution[check] <- "-"
      }
    }
  }
  eval
}

#this is the function for playing worlde
play_wordle <- function(solution, solution_list, num_guesses=6){
  print(paste("Welcome to Wordle. You have", num_guesses, "chances to guess the 5-letter word"))
  #Start guessing
  guessNum <- 0
  lettersLeft <- toupper(LETTERS)
  #this loop runs as long as the number of guesses entered is less than the number of guesses
  #allowed by the game
  while(guessNum < num_guesses) {
    #show letters left
    print(paste(c("Letters left: ", lettersLeft), collapse = " "))
    
    #user adds a guess and the number of guesses is updated 
    guessNum <- guessNum + 1
    guess <- readline(paste0("Enter guess ", guessNum, ": "))
    guess <- toupper(guess)
    guessSplit <- strsplit(guess, "")[[1]]
    
    #evaluate function is called
    evalGuess <- evaluate_guess(guessSplit, solution)
    #the keyboard is reduced to show what letters the user has not used
    lettersLeft <- setdiff(lettersLeft, guessSplit)
    #the users guess is printed above the evaluation
    print(paste(strsplit(guess, "")[[1]], collapse = " "))
    print(paste(evalGuess, collapse = " "))
    
    if(all(evalGuess == "*")) {
      print("Yay! You solved the wordle!")
      return(print(paste("You got the answer in ", guessNum, "guesses!")))
    }
  }
  print(paste("You ran out of chances! The correct answer was", paste(solution, collapse = "")))
  return(print(paste("Guesses Used: ", guessNum)))
  
  
}
play_wordle(solution)
happy
sells
