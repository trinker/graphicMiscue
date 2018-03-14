#' Compare the first letter of chunked strings
#'
#' Compare the first letter of chunked strings
#'
#' @param text The chunked text
#' @param miscue The chunked miscue
#' @param chunk A letter indicating if it's the beginning (b), middle (m), or end (e)
#' chunk.
#' @param \ldots ignored
#' @export
compare_first_letter <- function(text, miscue, chunk, ...) {
    if (anyNA(c(text, miscue))) return(FALSE)
    if (!chunk %in% 'b') return(TRUE)
    text[1] == miscue[1]
}

