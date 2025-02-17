library(tidyverse)
library(sjmisc)

load_dictionary <- function(x) {
  name <- read.table(x, header = TRUE)
}
f1 <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/collins-scrabble-words-2019.txt"
valid_list <- load_dictionary(f1)
#starts at 9884 obervations
f2 <- "https://raw.githubusercontent.com/difiore/ada-datasets/refs/heads/main/google-10000-english-usa-no-swears.txt"
solution_list <- load_dictionary(f2)
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
  e <- paste(e)
  return(e)
}

#this is the function for playing wordle
play_wordle <- function(solution, solution_list, num_guesses=6){
  print(paste("Welcome to Wordle. You have", num_guesses, "chances to guess the 5-letter word"))
  #Start guessing
  guessNum <- 0
  lettersLeft <- toupper(LETTERS) #sets up list of letters
  feedbackHist <- c() #this vector stores each evaluation created by the evaluate_guess function
  #this loop runs as long as the number of guesses entered is less than the number of guesses allowed by the game
  while(guessNum < num_guesses) {
    #show letters left
    print(paste(c("Letters left: ", lettersLeft), collapse = " "))
    
    #user adds a guess and the number of guesses is updated 
    guessNum <- guessNum + 1
    guess <- readline(paste0("Enter guess ", guessNum, ": "))
    guess <- toupper(guess)
    while(nchar(guess) != 5){ #guess is checked to make sure it has 5 letters, if not the user is prompted to try again
      guess <- readline(paste0("Your guess must have 5 letters. Try again: "))
    }
    # Check if the guess is in the solution list
    while (toupper(guess) %in% solution_list[,1] == F) {  #guess is checked to make sure it is in the solution list, if not the user is prompted to try again
      guess <- readline("This guess is not in the list of possible guesses. Try again: ")
    }
    #evaluate function is called
    evalGuess <- evaluate_guess(guess, solution)
    feedbackHist <- c(feedbackHist, paste("Guess", guessNum, ":", guess, paste(evalGuess, collapse = ""), "; ")) #stores each guess number, the users
    #guess, and the evaluation of the guess. This then adds it to the initial feedbackHist variable to be called on later
    guessSplit <- str_split_1(guess, "")
    guessSplit <- toupper(guessSplit) #the guess is split and capitalized so that it matches the formatting of the letters
    #the list of letters is reduced to show what letters the user has not used
    lettersLeft <- setdiff(lettersLeft, guessSplit)
    guess <- toupper(guess) #double checking the guess is capitalized
    #the users guess is printed above the evaluation
    print(paste(strsplit(guess, "")[[1]], collapse = " "))
    print(paste(evalGuess, collapse = " "))
    
    if(all(evalGuess == "*")) { #if the guess is correct, the user is told and then given their feedback history
      print("Yay! You solved the wordle!")
      print(paste("You got the answer in ", guessNum, "guesses! Below is your guess history:"))
      print(paste(feedbackHist, collapse = " "))
      return(print(paste("Congratulations on getting today's wordle!")))
    }
  }
  #if the user runs out of guesses, they are told they lost, what the solution was, and their total feedback history
  print(paste("You ran out of guesses! The correct answer was", paste(solution, collapse = "")))
  print(paste(feedbackHist, collapse = ""))
  return(print("Better luck next time!"))
}

Wordle <- play_wordle(solution, solution_list, num_guesses = 6)
