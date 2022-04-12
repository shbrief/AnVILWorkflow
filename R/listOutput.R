#' List outputs
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
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#'
#' @return A tibble with four columns
#' \itemize{
#'     \item file : character() 'base name' of the file in the bucket.
#'     \item workflow : character() name of the workflow the file is associated with.
#'     \item task : character() name of the task in the workflow that generated the file.
#'     \item path : character() full path to the file in the google bucket.
#' }
#'
#' @export
listOutput <- function(workspaceName,
                       submissionId = NULL, 
                       keyword = NULL,
                       includeMetadata = FALSE,
                       accountEmail = gcloud_account(), 
                       billingProjectName = gcloud_project()) {

    ## Setup gcloud account/project
    setCloudEnv(accountEmail = accountEmail, 
                billingProjectName = billingProjectName,
                message = FALSE)

    ## The most recent submission
    if (is.null(submissionId)) {   # If submissionId is not specified
        submissionId <- .mostRecentSucceededSubmissionId(billingProjectName,
                                                         workspaceName)
    }
    message(paste("Outputs are from the submissionId", submissionId))

    ## Get the list of all outputs
    av_bucket <- avbucket(name = workspaceName)
    outputs <- avworkflow_files(submissionId = submissionId, bucket = av_bucket)

    ## Remove metadata files
    if (isFALSE(includeMetadata)) {
        outputs <- .nonMetadataOutputs(workflowOutputs = outputs)
    }

    ## Filter with keyword
    if (!is.null(keyword)) {
        ind <- grep(keyword, outputs$file)
        res <- outputs[ind,,drop=FALSE]
        return(res)   # keyword-containing output files
    } else {
        return(outputs)
    }
}
