#' Download output files from Terra
#'
#' @import AnVIL
#'
#' @param workspaceName Name of the workspace
#' @param submissionId Submission Id. If it's not provided, the most recent
#' submission id with the 'succeeded' status will be used.
#' @param keyword A character string containing a regular expression to be matched
#' in the output file name. Under the default \code{NULL}, all the outputs from
#' the workflow, including log files, will be returned.
#' @param includeMetadata Under the default (\code{FALSE}), metadata files (e.g.
#' \code{stderr, stdout, .log, .sh}), will not be returned.
#' @param dest_dir Path to the directory where downloaded files are saved
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#'
#' @export
getOutput <- function(workspaceName,
                      submissionId = NULL, 
                      keyword = NULL,
                      includeMetadata = FALSE, 
                      dest_dir = ".",
                      accountEmail = gcloud_account(), 
                      billingProjectName = gcloud_project()) {

    ## Setup gcloud account/project
    setCloudEnv(accountEmail = accountEmail, 
                billingProjectName = billingProjectName,
                message = FALSE)
    
    ## Create destination directory
    if (!dir.exists(dest_dir)) {
        message(paste(dest_dir, "is created."))
        dir.create(dest_dir)
    }
    
    res <- listOutput(workspaceName = workspaceName, 
                      submissionId = submissionId, 
                      keyword = keyword, 
                      includeMetadata = includeMetadata)
    lapply(res$path, gsutil_cp, destination = dest_dir)
}
