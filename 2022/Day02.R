input = read.csv(
  "InputDay2.csv",
  header = FALSE,
  blank.lines.skip = FALSE,
  sep = " "
)

hand_score = function(input) {
  if (input == "X") {
    return(1)
  } else if (input == "Y") {
    return(2)
  } else if (input == "Z") {
    return(3)
  }
}

game_score = function(opponent, player) {
  if (
    # Player wins
    opponent == "A" & player == "Y" |
    opponent == "B" & player == "Z" |
    opponent == "C" & player == "X"
  ) {
    return(6)
  } else if (
    # Opponent wins
    opponent == "A" & player == "Z" |
    opponent == "B" & player == "X" |
    opponent == "C" & player == "Y"  
  ) {
    return(0)
  } else if (
    # Draw
    opponent == "A" & player == "X" |
    opponent == "B" & player == "Y" |
    opponent == "C" & player == "Z"  
  ) {
    return(3)
  }
}

recommended_hand = function(opponent, result) {
  if (
    # Recommend to play rock
    opponent == "A" & result == "Y" |
    opponent == "B" & result == "X" |
    opponent == "C" & result == "Z"
  ) {
    return("X")
  } else if (
    # Recommend to play paper
    opponent == "A" & result == "Z" |
    opponent == "B" & result == "Y" |
    opponent == "C" & result == "X"  
  ) {
    return("Y")
  } else if (
    # Recommend to play scissors
    opponent == "A" & result == "X" |
    opponent == "B" & result == "Z" |
    opponent == "C" & result == "Y"  
  ) {
    return("Z")
  }
}

# Part 1
for (i in 1:nrow(input)) {
  input[i,3] = game_score(input[i,1], input[i,2]) + hand_score(input[i,2])
}
# Answer
sum(input[,3])

# Part 2
for (i in 1:nrow(input)) {
  # Recommended hand
  input[i,3] = recommended_hand(input[i,1], input[i,2])
  # Score
  input[i,4] = game_score(input[i,1], input[i,3]) + hand_score(input[i,3])
}
# Answer
sum(input[,4])
