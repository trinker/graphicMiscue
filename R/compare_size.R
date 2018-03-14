#' Compare the size of chunked strings
#'
#' Compare the size of chunked strings
#'
#' @param text The chunked text
#' @param miscue The chunked miscue
#' @param \ldots ignored
#' @export
compare_size <- function(text, miscue, ...) {
    if (anyNA(c(text, miscue))) return(FALSE)
    abs(length(text) - length(miscue)) < 2
}

