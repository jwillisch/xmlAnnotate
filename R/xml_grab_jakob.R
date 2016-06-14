#' XML Annotate
#'
#' \code{xml_grab} takes one or multiple xml files containing annotated text and extracts annotations and the textpositions they belong to in a neatly formated dataframe.
#'
#' @param files A string indicating the path of an .xml file or a folder containint multiple .xml files
#' @param types A string vector of length 2 indicating the topic types that were used for annotation
#'
#' @return S3 \code{data.frame} object containing a sentence id, sentence number,topic code,trigger words, sentence span and position in the text and notes,
#' @examples \dontrun{
#' xml_grab("C:/Users/User/Desktop/",c("speculation","notspeculation"))
#' xml_grab("C:/Users/User/Desktop/example.xml",c("speculation","notspeculation"))
#' }
#'@author Jakob Willisch
xml_grab <- function(files,types){
  #import for single file
  if(grepl("*.xml$",files)){
    txt <- paste(readLines(files), collapse = "\n")
    names(txt) <- basename(files)
  }
  #import for all xmls in folder
  else{
    filelist<- list.files(files,pattern="*.xml$")
    txt <- sapply(paste(files, filelist, sep="/"),function(x){paste(readLines(x),collapse = "\n")})
    names(txt) <- filelist
  }
  #clean type tag
  txt <- sapply(txt, function(x,types){gsub(paste("type(",types[1],"|",types[2],")>=", sep=""), "type=", x, fixed=TRUE)},types=types,simplify=T) #clean xml tags for annotation types and get rid of ">="
  #extractor function
  data_extract <- function(xml,filename){
    doc <- XML::xmlParse(xml) #back into XML
    xmltop <- XML::xmlRoot(doc) #content of root
    #class(xmltop)#"XMLInternalElementNode" "XMLInternalNode" "XMLAbstractNode"
    #xmlName(xmltop) #give name of node, PubmedArticleSet
    #xmlSize(xmltop) #how many children in node, 19
    #xmlName(xmltop[[1]]) #name of root's children
    #xmlName(xmltop[[2]]) #name of root's children
    #xmlSize(xmltop[[2]]) #number of nodes in each child
    #xmlSApply(xmltop[[2]], xmlName) #name(s)
    dat <- as.data.frame(XML::xmlSApply(xmltop[[2]], XML::xmlAttrs)) #attribute(s)

    #put xml attributes in separate frames
    dat_H <- data.frame(t(dat[colnames(dat)=="HEDGE"]),filename=rep(filename,nrow(t(dat[colnames(dat)=="HEDGE"]))),stringsAsFactors = F)
    dat_W <- as.data.frame(t(dat[colnames(dat)=="WORD"]),stringsAsFactors = F)
    dat_N <- as.data.frame(t(dat[colnames(dat)=="NOTE"]),stringsAsFactors = F)


    # end points of sentences and word
    s <- as.integer(dat_H$end)+1
    w <- as.integer(dat_W$end)
    n <- as.integer(dat_N$end)

    #check if endpoints are monotonically increasing
    if(all(s==cummax(s))){
      #index mapping word to sentences
      w_index <- findInterval(w,s)+1
      # index mapping notes to sentences
      n_index <- findInterval(n,s)+1
    }
    #determine non-increasing end points and prompt if they should be excluded
    else{
      badpoints <- which(!(s==cummax(s)))
      x <- readline(prompt = paste("Sentence end points not monotonically increasing for HEDGE",badpoints-1,", do you want to exclude them? y/n: "))
      #exclude Hedges with non-increasing endpoints if yes
      if(x=="y"){
        dat_H <- data.frame(t(dat[colnames(dat)=="HEDGE"])[-badpoints,],filename=rep(filename,nrow(t(dat[colnames(dat)=="HEDGE"])[-badpoints,])),stringsAsFactors = F)
        s <- as.integer(dat_H$end)+1
        #index mapping word to sentences
        w_index <- findInterval(w,s)+1
        # index mapping notes to sentences
        n_index <- findInterval(n,s)+1
      }
    }
    # trigger words
    twords <- rep(NA,nrow(dat_H))
    if(length(dat_W$text)>1){
      twords[unique(w_index)]<- tapply(dat_W$text,w_index,FUN=paste0,collapse=";", simplify=T)
    }
    # ID file-sentence
    id <- paste(dat_H$filename,as.numeric(gsub("(H)(\\d)","\\2",dat_H$id)),sep="-")
    # Sentence No.
    sentence <-  as.numeric(gsub("(H)(\\d)","\\2",dat_H$id))
    # Senence Span
    hedge <- paste("[",dat_H$start,"-",dat_H$end,"]",sep = "")
    #Topic Code
    tcode <- paste(dat_H$type)
    #Notes
    notes<-rep(NA,nrow(dat_H))
    if(length(dat_N$type)>1){
      notes[unique(n_index)]<- dat_N$type
    }
    remove(list = c("doc","xml"))
    return(data.frame(id=id,sentence=sentence,topic_code=tcode,sspan=hedge,triggerwords=twords,notes=notes,stringsAsFactors = F))
  }
  # run extractor over xmls and extract xml filenames on the run
  data_list <- sapply(seq_along(txt),function(i) data_extract(txt[[i]],filename = names(txt)[[i]]),simplify=F)
  #combine
  out <- do.call("rbind",data_list)
  return(out)
}
