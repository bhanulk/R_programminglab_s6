df<-data.frame(
  contents<-c("hello",123,"world123","decorate9","fun","happy23"),
  stringsAsFactors = FALSE
)
remove_numbers<-function(x){
  gsub("[0-9]","",x)
}
#function gsub is used to substitute a word with another word
extract_strings<-function(x){
  chars<-unlist(strsplit(x,split="")) #split the element
  alpabets<-chars[grep("[a-zA-z]",chars)] #keeping only alphabets
  paste(alpabets,collapse = "") #recombining the alphabets to string
}

edited_df<-data.frame(lapply(df,remove_numbers))
cat("dataframe edited by using gsub function")
print(edited_df)

edit_df <- data.frame(lapply(df, function(col) sapply(col, extract_strings)), 
                      stringsAsFactors = FALSE)

cat("dataframe edited without using function")
print(edit_df)