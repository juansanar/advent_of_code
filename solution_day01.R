input <- read.table("https://raw.githubusercontent.com/juansanar/advent_of_code/refs/heads/main/input_day01.txt", quote="\"", comment.char="")

challenge_01 <- function(data){
    left <- data[[1]]
    right <- data[[2]]
    diff_list <- sum(abs(sort((left)) - sort(right)))
    print(diff_list)
}

challenge_02 <- function(data){
    reps <-table(data[[2]][data[[2]] %in% data[[1]]])
    df<-as.data.frame(reps)
    df$Var1 <-as.numeric(as.character(df$Var1))
    print(sum(df$Var1*df$Freq))
}

challenge_01(input)
challenge_02(input)
