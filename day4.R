library(tidyverse)
#### Part 1 ####
input <- readLines("inputs/input_day4.txt")

input = input[-c(1:9, 150)]

input[1] = "MMSAXXMMMMSMSMXMXSSMAMSMSXMASMMAXMASMMSMMAMXXMASXMXXXMSASXSSMMXSAMXXMAXXSMSMSMXXAMXAXMMMSMXAMMASMSMXAMXXMMMMSMSXSAAMXMMMSAMXSMMMMMSAMMSMSXXX\\"


input = gsub("\\\\", "", input)


## make matrix, check row, transpose (check cols), check diags


ncolV = nchar(input[1])
nrowV = length(input)




tryThis = lapply(input, strsplit, "") %>% unlist()

thisMat = matrix(tryThis, nrow = nrowV, ncol = ncolV, byrow = T)


### rows, F & B
numXMAS = 0
for(i in 1:nrow(thisMat)){
  input_sub = paste0(thisMat[i,],collapse = "")

  indices = gregexpr("XMAS", input_sub)
  numXMAS = numXMAS + regmatches(input_sub, indices) %>% unlist() %>% length()
  
  
  input_subB = paste0(rev(unlist(strsplit(input_sub,""))), collapse = "")
  
  indices = gregexpr("XMAS", input_subB)
  numXMAS = numXMAS + regmatches(input_subB, indices) %>% unlist() %>% length()
  
  
}

### columsn, F & B
for(i in 1:ncol(thisMat)){
  input_sub = paste0(thisMat[,i],collapse = "")
  
  indices = gregexpr("XMAS", input_sub)
  numXMAS = numXMAS + regmatches(input_sub, indices) %>% unlist() %>% length()
  
  input_subB = paste0(rev(unlist(strsplit(input_sub,""))), collapse = "")
  
  indices = gregexpr("XMAS", input_subB)
  numXMAS = numXMAS + regmatches(input_subB, indices) %>% unlist() %>% length()
  
}

## diagonal

input_sub = paste0(diag(thisMat[1:nrow(thisMat), 1:nrow(thisMat)]), collapse="")
indices = gregexpr("XMAS", input_sub)
numXMAS = numXMAS + regmatches(input_sub, indices) %>% unlist() %>% length()

input_subB = paste0(rev(unlist(strsplit(input_sub,""))), collapse = "")

indices = gregexpr("XMAS", input_subB)
numXMAS = numXMAS + regmatches(input_subB, indices) %>% unlist() %>% length()


## right diag F and B
for(i in 1:(ncolV-1)){
  test = thisMat[,-c(1:i)]
  
  input_sub = paste0(diag(test), collapse="")
  
  indices = gregexpr("XMAS", input_sub)
  numXMAS = numXMAS + regmatches(input_sub, indices) %>% unlist() %>% length()
  
  input_subB = paste0(rev(unlist(strsplit(input_sub,""))), collapse = "")
  
  indices = gregexpr("XMAS", input_subB)
  numXMAS = numXMAS + regmatches(input_subB, indices) %>% unlist() %>% length()
  
}


## left diag F and B
for(i in 1:(ncolV-1)){
  test = thisMat[-c(1:i),]
  
  input_sub = paste0(diag(test), collapse="")
  
  indices = gregexpr("XMAS", input_sub)
  numXMAS = numXMAS + regmatches(input_sub, indices) %>% unlist() %>% length()
  
  input_subB = paste0(rev(unlist(strsplit(input_sub,""))), collapse = "")
  
  indices = gregexpr("XMAS", input_subB)
  numXMAS = numXMAS + regmatches(input_subB, indices) %>% unlist() %>% length()
  
}


numXMAS





thisMatR = 

