#' Chunk strings into begining middle end
#'
#' Chunk strings into begining middle end
#'
#' @param text The text field
#' @param miscue The miscue field
#' @param \ldots ignored
#' @export
#' @examples
#' chunk_text_miscue(miscue$text, miscue$miscue)
chunk_text_miscue <- function(text, miscue, ...){
    list(
        text = word_chunk(text),
        miscue = word_chunk(miscue)

    )
}


word_chunk_h <- function(x){
    letters <- strsplit(x, '')[[1]]

    addons <- rep(0, 3)
    addons[seq_along(rep(1, length(letters)%%3))] <- rep(1, length(letters)%%3)

    base <- floor(length(letters)/3)

    if (length(letters) %in% c(1,3)) return(stats::setNames(as.list(letters[1:3]), c('b', 'm', 'e')))
    if (length(letters) %in% c(2)) return(stats::setNames(as.list(letters[1:3][c(1, 3, 2)]), c('b', 'm', 'e')))
    locs <- utils::head(1 + cumsum(base + addons), 2)

    stats::setNames(textshape::split_index(letters,locs), c('b', 'm', 'e'))
}



word_chunk <- function(x) lapply(x, word_chunk_h)


















