split_chars()
num_vowels()
count_vowels()
reverse_chars()
reverse_words()


split_chars <- function(x){strsplit(x, "")[[1]]}
split_chars("Go Bears")

num_vowels <- function(vec){
     counts <- c(sum(vec == "a"),
                 sum(vec == "e"),
                 sum(vec == "i"),
                 sum(vec == "o"),
                 sum(vec == "u"))
     names(counts) <- c("a", "e", "i", "o", "u")
     counts
}

vec <- c("G", "o", " ", "B", "e", "a", "r", "s")
num_vowels(vec)

count_vowels <- function(x){
     split <- tolower(split_chars(x))
     num_vowels(split)
}
count_vowels("GO BEARS")
count_vowels("THE QUICK BROWN FOX JUMPS OVER THE LAZY DOG")

reverse_chars <- function(x){
     empty <- c(rep(0, nchar(x)))
     for(i in 1:nchar(x)){
          split <- split_chars(x)
          empty[i] <- split[nchar(x)+1-i]
     }
     paste(empty, collapse = "")
}
reverse_chars("gattaca")
reverse_chars("Lumox Maxima")

reverse_words <- function(x){
     split <- str_split(x, " ")
     new <- c(rep(0, length(split[[1]])))
     for(i in 1:length(split[[1]])){
          new[i] <- split[[1]][length(split[[1]])+1-i]
     }
     paste(new, collapse=" ")
}
reverse_words("sentence! this reverse")
reverse_words("string")

