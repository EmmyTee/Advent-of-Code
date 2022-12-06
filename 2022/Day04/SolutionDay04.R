input = read.csv(
  "InputDay04.csv",
  header = FALSE
)

# Part 1

pair_count = 0

for (i in 1:nrow(input)) {
  first_elf = input[i,1]
  second_elf = input[i,2]
  
  first_elf_sections = strsplit(first_elf, "-")[[1]][1]:strsplit(first_elf, "-")[[1]][2]
  second_elf_sections = strsplit(second_elf, "-")[[1]][1]:strsplit(second_elf, "-")[[1]][2]
  
  common_sections = intersect(first_elf_sections, second_elf_sections)
  
  if (
    all(first_elf_sections %in% second_elf_sections) |
    all(second_elf_sections %in% first_elf_sections)
  )
  {
    pair_count = pair_count + 1
  }
}

# Answer
print(pair_count)

# Part 2

pair_count = 0

for (i in 1:nrow(input)) {
  first_elf = input[i,1]
  second_elf = input[i,2]
  
  first_elf_sections = strsplit(first_elf, "-")[[1]][1]:strsplit(first_elf, "-")[[1]][2]
  second_elf_sections = strsplit(second_elf, "-")[[1]][1]:strsplit(second_elf, "-")[[1]][2]
  
  common_sections = intersect(first_elf_sections, second_elf_sections)
  
  if (
    any(first_elf_sections %in% second_elf_sections) |
    any(second_elf_sections %in% first_elf_sections)
  )
  {
    pair_count = pair_count + 1
  }
}

# Answer
print(pair_count)
