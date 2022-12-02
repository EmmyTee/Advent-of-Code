input = read.csv(
  "Input.csv",
  header = FALSE,
  blank.lines.skip = FALSE
)

output = data.frame(header = FALSE)
count = 0

for (i in 1:nrow(input)) {
  if (!is.na(input[i,])) {
    count = count + input[i,]
  } else {
    output = rbind(output, count)
    count = 0
  }
}

# Part 1
max(output)

# Part 2
sum(sort(output[,1], decreasing = TRUE)[1:3])
