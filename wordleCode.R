library(tidyverse)
library(sjmisc)

load_dictionary <- function(x) {
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
evaluate_guess <- function(guess, solution){
  guess <- toupper(guess)
  guess <- str_split_1(guess, "")
  e <- rep("-", 5)
  for(i in 1:5){
    if(guess[i] %in% solution ==F){}
    else{ if (guess[i] != solution[i]) {e[i] = "+"}
      else{e[i] = "*"}}
  }
  e <- paste(e) #collapse = "")
  return(e)
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
    while(guess %in% solution_list == F) {
      guess <- readline("This guess is not in the list of possible guesses. Try again: ")
    }
    #evaluate function is called
    evalGuess <- evaluate_guess(guess, solution)
    guessSplit <- str_split_1(guess, "")
    guessSplit <- toupper(guessSplit)
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
play_wordle(solution, solution_list, num_guesses = 6)
pairs
names
/
  