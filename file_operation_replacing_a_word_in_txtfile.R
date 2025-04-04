replace_word_in_file <- function(input_file, output_file, target_word, replacement_word) {
  text <- readLines(input_file)
  modified_text <- gsub(target_word, replacement_word,text)
  writeLines(modified_text, output_file)
  
  cat("Word is replaced in", output_file, "\n")
}
cat("enter the word to replace:")
oldword<-readline()
cat("enter the new word to replace wiith:")
newword<-readline()

replace_word_in_file("input.txt", "output.txt", oldword, newword)
