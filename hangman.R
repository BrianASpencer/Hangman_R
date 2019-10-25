hangman = function(words) {
  im0 = load.image("Start.png") #starting image
  im1 = load.image("First.png")
  im2 = load.image("Second.png")
  im3 = load.image("Third.png")
  im4 = load.image("Fourth.png")
  im5 = load.image("Fifth.png") 
  plot(im0, axes=FALSE)
  
  if (length(words[,1]) > 1) { #picking a sample word
    index = sample(1:length(words[,1]), 1)
    s = words[index, 1]
  } else {
    index = sample(1:length(words[1,]), 1)
    s = words[1, index]
  }
  
  s = toString(s) #converting it to a string
  
  hint = ""
  
  lcnt = 0 #counter for 5 guesses
  
  for (i in 1:nchar(s)) { #making the hint
    hint = paste0(hint, "_")
  }
  
  while (lcnt < 5) {
    
    
    print(paste0("The word is ", nchar(s), " letters.")) #first hint
    
    
    gType <- readline(prompt="Would you like to guess a letter or the word? 
    Type L to guess a letter or type anything else to guess the word.") #get user input
    
    if (toupper(gType) == "L") { #if user chose to guess a letter, they get to 
      guess = readline(prompt="Enter a letter for your guess: ") #enter a letter
    } else {
      guess = readline(prompt="Enter a word for your guess: ") #else get to enter a word
      if (guess != s) { #if guess was incorrect, returns losing statement
        plot(im5, axes=FALSE)
        return (print(paste0("Sorry, that is not correct."," The word was ",s)))
      }
      else {
        return (print(paste0("Congrats! You won!.")))
      }
    }
    cnt = 0 #counter to determine if guess was in the word
    for (i in 1:nchar(s)) {
      if (substr(s, i, i) == guess) {
        substr(hint, i, i) = guess
        cnt = cnt + 1
      }
    }
    
    if (hint == s) {
      return (print(paste0("Congrats! You won!", " ", s, " was the word.")))
    }
    
    if (cnt > 0) {
      print(paste0("Your guess was in the word!"))
    }
    else {
      print(paste0("Your guess wasn't in the word"))
      lcnt = lcnt + 1
      if (lcnt == 1) {
        plot(im1, axes=FALSE)
      } else if (lcnt == 2) {
        plot(im2, axes=FALSE)
      } else if (lcnt == 3) {
        plot(im3, axes=FALSE)
      } else if (lcnt == 4) {
        plot(im4, axes=FALSE)
      } else if (lcnt == 5) {
        plot(im5, axes=FALSE)
      }
    }
    
    phint = "" #creating a printable hint
    for (i in 1:nchar(hint)) {
      phint = paste(phint, substr(hint, i, i))
    }
    print(paste0("Current progress: ", phint))
    if (lcnt < 5) {
      print(paste0("You have ", (5-lcnt), " guesses left")) #telling user how many guesses are left
    } else {
      print(paste0("You're out of guesses. The word was ", s))
    }
    
  } #close of while loop
  
} #close of function