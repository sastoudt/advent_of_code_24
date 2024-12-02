
#### Part 1 ####


## The engineers are trying to figure out which reports are safe.
## The Red-Nosed reactor safety systems can only tolerate levels
## that are either gradually increasing or gradually decreasing.
## So, a report only counts as safe if both of the following are true:
  
## The levels are either all increasing or all decreasing.
## Any two adjacent levels differ by at least one and at most three.
input <- read.csv("inputs/input_day2.csv", header = F)



helper_fn <- function(idx) {
  toUse <- which(!is.na(input[idx, ]))
  toUse2 <- toUse[-length(toUse)]
  diffs <- input[idx, toUse[-1]] - input[idx, toUse2]
  check1 <- sum(diffs < 0) == length(diffs) | sum(diffs > 0) == length(diffs)
  check2a <- sum(abs(diffs)) >= length(diffs) ## since all integers
  check2b <- which(abs(diffs) > 3)
  check2b <- ifelse(length(check2b) == 0, TRUE, FALSE)

  return(check1 & check2a & check2b)
}

sum(unlist(lapply(1:nrow(input), helper_fn)))


#### Part Two ####

## Now, the same rules apply as before, except if removing a
## single level from an unsafe report would make it safe,
## the report instead counts as safe.

helper_fn_robust <- function(idx) {
  toUse <- which(!is.na(input[idx, ]))
  toUse2 <- toUse[-length(toUse)]
  diffs <- input[idx, toUse[-1]] - input[idx, toUse2]
  
  gZ1 = which(diffs>0)
  lZ1 = which(diffs<0)

  gZ <- setdiff(1:length(diffs), which(diffs > 0)) 
  lZ <- setdiff(1:length(diffs),which(diffs < 0))
  
  if(length(gZ1)<(length(diffs)-1) & length(lZ1)<length(diffs)-1){
    return(FALSE)
  }else{


  lo <- which(abs(diffs) < 1)
  gt <- which(abs(diffs) > 3)

  if(length(gZ)< length(lZ)){
    problems <- c(gZ, lo, gt)
    
  }else if(length(gZ)>length(lZ)){
    problems <- c(lZ, lo, gt)
    
  }else{
    problems <- c(lo, gt)
  }

  answer <- ifelse(length(unique(problems)) > 1, FALSE, TRUE)

  return(answer)
  }
}



sum(unlist(lapply(1:nrow(input), helper_fn_robust)))

## too low 595
## too high 606
