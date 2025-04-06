
df <- data.frame(
  words = c("information", "informal", "informer"),
  stringsAsFactors = FALSE
)

get_substrings<-function(s){
  len<-nchar(s)
  substrings<-character()
  for (i in 1:len){
    for (j in i:len){
      substrings<-c(substrings,substring(s,i,j))
    }
  }
  return(unique(substrings))
}
find_longest_common_substring <- function(strings) {
  substrings <- get_substrings(strings[1])
  
  for (s in strings[-1]) {
    substrings <- substrings[sapply(substrings, function(sub) grepl(sub, s))]
  }
  if (length(substrings) == 0) return("")
  return(substrings[which.max(nchar(substrings))])
}

common_substring<-find_longest_common_substring(df$words)
cat("longest substring:",common_substring)
