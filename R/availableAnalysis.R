#' Find the available analysis
#'
#' This function shows the available analyses and the brief
#' descriptions of them.
#'
#' @param simplify Default is \code{TRUE}. If it is set to \code{FALSE}, the
#' additional information on workspace and workflow will be printed too.
#'
#' @return A data frame. The \code{analysis} columns shows the name of the
#' available analyses, which is the required input (\code{analysis} argument)
#' for the functions implemented in RunTerraWorkflow package.
#'
#' @examples
#' availableAnalysis()
#'
#' @export
availableAnalysis <- function(simplify = TRUE) {
    dir <- system.file("extdata", package = "RunTerraWorkflow")
    map <- utils::read.table(file.path(dir, "map.tsv"), header = TRUE)

    if (isTRUE(simplify)) {
        map[,c("analysis", "description")]
    } else {map}
}

