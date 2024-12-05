
#### Part 1 ####
input <- readLines("inputs/input_day4.txt")

input = input[-c(1:9, 150)]

input[1] = "MMSAXXMMMMSMSMXMXSSMAMSMSXMASMMAXMASMMSMMAMXXMASXMXXXMSASXSSMMXSAMXXMAXXSMSMSMXXAMXAXMMMSMXAMMASMSMXAMXXMMMMSMSXSAAMXMMMSAMXSMMMMMSAMMSMSXXX\\"


input = gsub("\\\\", "", input)


## make matrix, check row, transpose (check cols), check diags