#### Part 1 ####

## pair up the numbers and measure how far apart they are.
## Pair up the smallest number in the left list with the smallest
## number in the right list, then the second-smallest left
## number with the second-smallest right number, and so on.

## Within each pair, figure out how far apart the two numbers are;
## you'll need to add up all of those distances.

input <- read.csv("input_day1.csv", header = F)


input$V1 <- sort(input$V1)
input$V2 <- sort(input$V2)

sum(abs(apply(input, 1, diff)))

#### Part 2 ####

## Calculate a total similarity score by adding up
## each number in the left list after multiplying
## it by the number of times that number appears in the right list.

sim_score <- 0
for (i in 1:nrow(input)) {
  mult_factor <- length(which(input$V2 == input$V1[i]))

  sim_score <- sim_score + input$V1[i] * mult_factor
}

sim_score
