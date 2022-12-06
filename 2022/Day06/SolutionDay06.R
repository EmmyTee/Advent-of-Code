input = read.delim(
  "InputDay06.txt",
  header = FALSE
)$V1

start_of_packet_market = function (signal, marker_length) {
  for (i in 1:(nchar(input)-marker_length)) {
    
    marker = strsplit(input,"")[[1]][i:(i+marker_length-1)]
    
    if (all(table(strsplit(input,"")[[1]][i:(i+marker_length-1)]) == 1)) {
      answer = i + marker_length -1
      
      return(answer)
    }
  }
}

# Part 1 Answer
print(start_of_packet_market(input, 4))

# Part 2 Answer
print(start_of_packet_market(input, 14))