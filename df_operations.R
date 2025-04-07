df<-data.frame(
  id<-c(1,2,3,4),
  names<-c("a","b","c","d"),
  salary<-c(200,399,200,4000),
  score<-c(4,3,NA,1),
  department<-c("finance","sales","finance","finance"),
  stringsAsFactors=FALSE
)
#adding new column status to the data frame on conditions
df$status<-ifelse(df$salary>300,"high","low")
print(df)

#claculating terms
df$year<-c(2000,2004,2003,2006)

currentyear<-as.numeric(format(Sys.Date(),"%Y"))
df$term<-currentyear-df$year

print(df)
#replacing nan values to median values
for(dept in unique( df$department)){
  median_score<-median(df$score[df$department==dept],na.rm = TRUE)
  df$score[is.na(df$score) & (df$department==dept)]<-median_score}
print(df)
