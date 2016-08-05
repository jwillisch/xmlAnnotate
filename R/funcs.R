#' Get tags of selected types from an XML file
#'
#' @param x filename
#' @param nodes vector of tag names to extract (default: just 'hedge')
#' @param mae version of MAE that produced the XML tags
#'
#' @return a \code{data.frame} with headings:
#' \itemize{
#'  \item \code{file} name of file
#'  \item \code{id} tag identifier, e.g. hedge
#'  \item \code{start} letter index of annotation start
#'  \item \code{end} letter index of annotation end
#'  \item \code{text} text of annotation
#'  \item \code{type} value of 'type' attribute, if there is one
#'  \item \code{here} value of 'here' attribute, if there is one
#' }
#' @export
get_tagset <- function(x, nodes=c("hedge"),mae="2.1"){
  txt <- gsub("type(speculation|notspeculation)>=", "type=",
              paste(readLines(x, warn=FALSE), collapse = "\n"), fixed=TRUE)
  lst <- list()
  h <- xml2::read_html(txt)
  for (nname in nodes){
    hh <- rvest::html_nodes(h, nname)
    if (length(hh)==0){
      message("There are no '", nname, "' tags in '", x,"'s tagset")
    } else {
        if (mae=="2.1"){
        df <- data.frame(file=basename(x),
                         node=nname,
                         id=rvest::html_attr(hh, "id"),
                         start=rvest::html_attr(hh, "start"),
                         end=rvest::html_attr(hh, "end"),
                         text=rvest::html_attr(hh, "text"),
                         type=NA,
                         here=NA,
                         stringsAsFactors=FALSE)

        df$type <- as.character(rvest::html_attr(hh, "type"))
        df$here <- as.character(rvest::html_attr(hh, "here"))

        lst[[nname]] <- df
        } else{
            if(mae=="2.0"){
              df <- data.frame(file=basename(x),
                               node=nname,
                               id=rvest::html_attr(hh, "id"),
                               start=rapply(strsplit(rvest::html_attr(hh, "spans"),split = "~"), function(x) x[1]),
                               end=rapply(strsplit(rvest::html_attr(hh, "spans"),split = "~"), function(x) x[2]),
                               text=rvest::html_attr(hh, "text"),
                               type=NA,
                               here=NA,
                               stringsAsFactors=FALSE)

            df$type <- as.character(rvest::html_attr(hh, "type"))
            df$here <- as.character(rvest::html_attr(hh, "here"))

            lst[[nname]] <- df
            } else{
              message("The MAE version you specified is not supported")
            }
        }
      }
  }
  ddf <- do.call(what=rbind, args=lst)
  rownames(ddf) <- NULL
  ddf
}

#' Apply get_tagset to each XML file in a folder
#'
#' @param x folder
#' @param nodes vector of node types that should be extracted (default: just 'hedge')
#'
#' @return a \code{data.frame} containing the \code{rbind}-ed output of \code{get_tagsets}
#'         for each XML file in the folder
#' @export
get_tagsets <- function(x, nodes=c("hedge"),mae="2.1"){
  fls <- dir(x, pattern=".xml|.XML")
  if (length(fls)==0)
    stop("There are no XML files in '", x, "'")

  lst <- list()
  for (f in fls)
    lst[[f]] = get_tagset(file.path(x, f), nodes=nodes,mae=mae)

  ddf <- do.call(what=rbind, args=lst)
  rownames(ddf) <- NULL
  ddf
}

#' Match different nodes based on start and endpoints
#'
#' @param tagset Data-frame returned by \code{get_tagset} or \code{get_tagsets}
#' @param match_x Tag value of node to be matched on
#' @param match_y Tag value to be matched
#'
#' @return a \code{data.frame} of  similar structure as the return of \code{get_tagset} with values of nodes specified in \code{match_y} matched to the corresponding rows of nodes specified in \code{match_x} based on the start-end intervals.
#'
#' @export
match_nodes <- function(tagset, match_x, match_y){
  lst <- list()
  for (nname in match_y){
  # end point
  x <- as.integer(tagset$end[tagset$node==match_x])+1
  y <- as.integer(tagset$end[tagset$node==nname])

  #check if endpoints are monotonically increasing
    if(all(x==cummax(x))){
      #index mapping node values of match_y to node values of match_x
      y_index <- findInterval(y,x)+1
    }
  #determine non-increasing end points and prompt if they should be excluded
    else{
      badpoints <- which(!(x==cummax(x)))
      message("The endpoints specified are not monotonically increasing for ",match_x," at end points:",paste0(badpoints-1,sep = ","))

    }
  y_id <- rep(NA,nrow(tagset[tagset$node==match_x,]))
  y_id[unique(y_index)] <- tapply(tagset$id[tagset$node==nname],y_index,FUN=paste0,collapse=";")
  y_text <- rep(NA,nrow(tagset[tagset$node==match_x,]))
  y_text[unique(y_index)] <- tapply(tagset$text[tagset$node==nname],y_index,FUN=paste0,collapse=";")
  y_type <- rep(NA,nrow(tagset[tagset$node==match_x,]))
  y_type[unique(y_index)] <- tapply(tagset$type[tagset$node==nname],y_index,FUN=paste0,collapse=";")

  lst[[nname]] <- data.frame(y_id,y_text,y_type,stringsAsFactors = F)
  colnames(lst[[nname]]) <- c(paste0("_id"),paste0("_text"),paste0("_type"))
  }
df <- do.call(cbind,args=list(tagset[tagset$node==match_x,],lst))
rownames(df) <- NULL
return(df)
}
