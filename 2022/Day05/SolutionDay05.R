# Read data ---------------------------------------------------------------

raw_stack = read.fwf(
  "InputDay05.csv",
  header = FALSE,
  widths = rep(4, 9),
  nrows = 8
)

instructions = read.csv(
  "InputDay05.csv",
  header = FALSE,
  skip = 9
)

# Data Pre-processing -----------------------------------------------------

raw_stack[raw_stack == "   "] = NA
raw_stack[raw_stack == "    "] = NA

# Part 1 ------------------------------------------------------------------

stack = list()
for (col in 1:ncol(raw_stack)) {
  stack[[col]] = na.omit(raw_stack[,col])
}

for (i in 1:nrow(instructions)) {
  move_count = as.numeric(strsplit(instructions$V1[i], " ")[[1]][2])
  source_pile = as.numeric(strsplit(instructions$V1[i], " ")[[1]][4])
  destination_pile = as.numeric(strsplit(instructions$V1[i], " ")[[1]][6])
  
  for (j in 1:move_count) {
    crate = na.omit(stack[source_pile])[[1]][1]
    
    stack[[destination_pile]] = append(crate, na.omit(stack[[destination_pile]]))
    stack[[source_pile]] = na.omit(stack[[source_pile]])[-1]
  }
}

# Answer
print(gsub("[^[:alnum:]]", "", paste(sapply(stack,"[[",1), collapse = "")))

# Part 2 ------------------------------------------------------------------

stack = list()
for (col in 1:ncol(raw_stack)) {
  stack[[col]] = na.omit(raw_stack[,col])
}

for (i in 1:nrow(instructions)) {
  move_count = as.numeric(strsplit(instructions$V1[i], " ")[[1]][2])
  source_pile = as.numeric(strsplit(instructions$V1[i], " ")[[1]][4])
  destination_pile = as.numeric(strsplit(instructions$V1[i], " ")[[1]][6])
  
  crates = na.omit(stack[source_pile])[[1]][1:move_count]
  
  stack[[destination_pile]] = append(crates, na.omit(stack[[destination_pile]]))
  stack[[source_pile]] = na.omit(stack[[source_pile]])[-1:-move_count]
}

# Answer
print(gsub("[^[:alnum:]]", "", paste(sapply(stack,"[[",1), collapse = "")))