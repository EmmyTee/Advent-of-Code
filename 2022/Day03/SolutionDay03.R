input = read.csv(
  "InputDay03.csv",
  header = FALSE
)

priority = function (letter) {
  if (common_item == toupper(common_item)) {
    # If capital letter
    priority = which(LETTERS == common_item) + 26
  } else {
    # If lower case
    priority = which(letters == common_item)
  }
  
  return(priority)
}

total_priority = 0

# Part 1

for (i in 1:nrow(input)) {
  rucksack_contents = input[,1][i]
  rucksack_size = nchar(rucksack_contents)
  compartment_size = rucksack_size/2
  
  first_compartment_content = substr(rucksack_contents, 1, compartment_size)
  second_compartment_content = substr(rucksack_contents, compartment_size+1, rucksack_size)
  
  common_item = intersect(
    strsplit(first_compartment_content,"")[[1]],
    strsplit(second_compartment_content,"")[[1]]
  )
  
  item_priority = priority(common_item)
  
  total_priority = total_priority + item_priority
}

# Answer
print(total_priority)

# Part 2

total_priority = 0

for (i in 0:((nrow(input)/3)-1)) {
  first_compartment_content = input[,1][i*3+1]
  second_compartment_content = input[,1][i*3+2]
  third_compartment_content = input[,1][i*3+3]
  
  common_item = intersect(
    intersect(
      strsplit(first_compartment_content,"")[[1]],
      strsplit(second_compartment_content,"")[[1]]
    ),
    strsplit(third_compartment_content,"")[[1]]
  )
  
  item_priority = priority(common_item)
  
  total_priority = total_priority + item_priority
}

# Answer
print(total_priority)