#' Get tags of selected types from an XML file
#'
#' @param x filename
#' @param nodes vector of tag names to extract (default: just 'hedge')
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
get_tagset <- function(x, nodes=c("hedge")){
  txt <- gsub("type(speculation|notspeculation)>=", "type=",
              paste(readLines(x, warn=FALSE), collapse = "\n"), fixed=TRUE)
  lst <- list()
  h <- xml2::read_html(txt)
  for (nname in nodes){
    hh <- rvest::html_nodes(h, nname)
    if (length(hh)==0){
      message("There are no '", nname, "' tags in '", x,"'s tagset")
    } else {
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
get_tagsets <- function(x, nodes=c("hedge")){
  fls <- dir(x, pattern=".xml|.XML")
  if (length(fls)==0)
    stop("There are no XML files in '", x, "'")

  lst <- list()
  for (f in fls)
    lst[[f]] = get_tagset(file.path(x, f), nodes=nodes)

  ddf <- do.call(what=rbind, args=lst)
  rownames(ddf) <- NULL
  ddf
}
