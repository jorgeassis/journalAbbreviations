
require(stringr)

removeSpaces <- function(string) {
  spaces <- as.numeric(gregexpr("[ ]",string)[[1]])
  if(1 %in% spaces) { string <- substr(string, 2, nchar(string)) }
  spaces <- as.numeric(gregexpr("[ ]",string)[[1]])
  if(nchar(string) %in% spaces) { string <- substr(string, 1, nchar(string)-1) }
  return(string)
}

journalAbbreviationsFiles <- "/Volumes/Jellyfish/Dropbox/Bears Studio/codeDevelopment/journalAbbreviations/journals"

journalAbbreviations <- data.frame()

for( f in list.files(journalAbbreviationsFiles,full.names = T)) {
  
  journalAbbreviations.i <- read.csv(f,head=F,sep="\t")
  journalAbbreviations <- rbind(journalAbbreviations,journalAbbreviations.i)
  
}

journalAbbreviations <- journalAbbreviations[!duplicated(journalAbbreviations[,1]),]
journalAbbreviations <- data.frame(journal=as.character(journalAbbreviations[,1]),abbreviation=as.character(journalAbbreviations[,2]), stringsAsFactors = F)

getAbbreviation <- function(journalTitle) {
  
  indexEq.i <- which(grepl(removeSpaces(journalTitle),journalAbbreviations[,1]))
  
  if( length(indexEq.i) > 0 ) {
    
    indexEq <- str_locate_all( removeSpaces(journalAbbreviations[indexEq.i,1]), pattern =journalTitle )
    indexEq <- sapply(1:length(indexEq),function(x) { indexEq[[x]] } )
    
    if(class(indexEq)!="matrix") { indexEq <- data.frame(do.call(rbind,indexEq))}
    if(class(indexEq)=="matrix") { indexEq <- data.frame(t(indexEq))}
    
    
    indexEq.1 <- which.min(abs(indexEq[,2] - nchar(journalTitle))) 
    indexEq.2 <- which.min(abs(indexEq[,1] - 1)) 
    
    if( indexEq.1 == indexEq.2) { abbreviation <- journalAbbreviations[indexEq.i[indexEq.1],2] } else { abbreviation <- journalTitle }
    
    string <- abbreviation
    spaces <- as.numeric(gregexpr("[ ]",string)[[1]])
    if(1 %in% spaces) { string <- substr(string, 2, nchar(string)) }
    spaces <- as.numeric(gregexpr("[ ]",string)[[1]])
    if(nchar(string) %in% spaces) { string <- substr(string, 1, nchar(string)-1) }
  }
  
  else { string <- journalTitle }
  
  return(string)
  
}

