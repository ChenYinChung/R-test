#匯入 同一個資料夾中的R檔案
getFilePath <- function(fileName) {
  sourceObj <- source(fileName)
  return(sourceObj)
}


getFilePath("/Users/sammy/git/survey/r/pow.r")

pow(3)
