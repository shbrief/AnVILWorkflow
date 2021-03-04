#' Download output files from Terra
#'
#' @import AnVIL
#'
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#' @param submissionId Submission Id. If it's not provided, the most recent
#' submission id with the 'succeeded' status will be used.
#' @param keyword A character string containing a regular expression to be matched
#' in the output file name. Under the default \code{NULL}, all the outputs from
#' the workflow, including log files, will be returned.
#' @param includeMetadata Under the default (\code{FALSE}), metadata files (e.g.
#' \code{stderr, stdout, .log, .sh}), will not be returned.
#' @param dest_dir Path to the directory where downloaded files are saved
#'
#' @export
getOutput <- function(accountEmail, billingProjectName, workspaceName,
                      submissionId = NULL, keyword = NULL,
                      includeMetadata = FALSE, dest_dir = ".") {

    res <- listOutput(accountEmail, billingProjectName, workspaceName,
                      submissionId, keyword, includeMetadata)
    lapply(res$path, gsutil_cp, destination = dest_dir)
}
