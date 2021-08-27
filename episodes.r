library(dplyr);
data_frame <- read.csv(file="elements-by-episode.csv", header=TRUE, sep=",");
data_frame <- filter ( data_frame , DIANE_ANDRE == 0 , STEVE_ROSS == 0)
data_frame <- select(data_frame , -EPISODE ,  -DIANE_ANDRE, -STEVE_ROSS, -TITLE, -WOOD_FRAMED, -contains("FRAME"))
print(data_frame)
output <- data.frame();
new.row <- data.frame();
element1 <- data.frame();
element2 <- data.frame();
col1Num = 0
for (col1 in data_frame){
  col1Num = col1Num + 1
  col2Num = 0
  for (col2 in data_frame){
    col2Num = col2Num + 1
    element1 <- filter ( data_frame , col2 == 1)
    element2 <- filter ( data_frame , col2 == 1 , col1 == 1 )
    new.row <- data.frame(colnames(data_frame)[col1Num], colnames(data_frame)[col2Num] , nrow(element2)/nrow(element1))
    output <- rbind(output , new.row)
  }
}
write.csv(output , file = "output.csv")