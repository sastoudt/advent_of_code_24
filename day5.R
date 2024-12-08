
#### Part 1 ####

input1 <- read.csv("inputs/day5pt1.csv", header = F)

## split, into different columns tidy

input1 = input1 %>% separate_wider_delim(V1, "|", names = c("first", "second") ) 


input2 <- read.csv("inputs/day5pt2.csv", header = F)

## get middle for everyone, then just pluck those out after verifying

#middleIdx = c()
input2$middleValue <- NA
flag <- c()
for (i in 1:nrow(input2)) {
  row <- unlist(strsplit(input2$V1[i], ","))


  rules <- which(input1$first %in% row)
#print(i)
  for (j in 1:length(rules)) {
    if (input1$second[rules[j]] %in% row) {
      

      
      second <- which(row == input1$second[rules[j]])
      first <- which(row == input1$first[rules[j]])
#print(rules[j])
      if (second > first) {
        ## good
      } else {
        flag <- c(flag, i)
      }
    } else {

    }
  }

  # middleIdx = c(middleIdx, ceiling(length(row)/2))

  input2$middleValue[i] <- row[ceiling(length(row) / 2)]
}


flag %>% unique() %>% length() ## 74

nrow(input2) ## 201


input2b = input2[-unique(flag),]
dim(input2b)

sum(as.numeric(input2b$middleValue))


