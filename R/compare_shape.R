#' Compare the shape of chunked strings
#'
#' Compare the shape of chunked strings
#'
#' @param text The chunked text
#' @param miscue The chunked miscue
#' @param \ldots ignored
#' @export
compare_shape <- function(text, miscue, ...) {

    if (anyNA(c(text, miscue))) return(FALSE)

    text[text %in% ceilingl1] <- '^'
    miscue[miscue %in% ceilingl1] <- '^'
    text[text %in% middlel1] <- '-'
    miscue[miscue %in% middlel1] <- '-'
    text[text %in% basementl1] <- '_'
    miscue[miscue %in% basementl1] <- '_'

    paste(rle(text)$values , collapse ='') == paste(rle(miscue)$values , collapse ='')

}


ceilingl1 <- c('b', 'd', 'f', 'h', 'k', 'l', LETTERS)
middlel1 <- c('a', 'c', 'e', 'i', 'm', 'n', 'o', 'r', 's', 't', 'u', 'v', 'w', 'x', 'z')
basementl1 <- c('g', 'j', 'p', 'q', 'y')
