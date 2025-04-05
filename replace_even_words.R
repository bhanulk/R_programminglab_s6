cat("enter the text:")
input<-readline()
cat("enter the word to replace words at even positions:")
word<-readline()
replace_even_words<-function(input,word){
  words<-unlist(strsplit(input,"\\s+"))
  words[seq(2,length(words),by=2)]<-word
  return(paste(words, collapse = " "))
}
result<-replace_even_words(input,word)
cat("modified text:",result)