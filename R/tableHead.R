#' Display the head of output tsv file
#'
#' If outputs contain `\code{.tsv}` files, you can check the head of those files
#' using this function without downloading them.
#'
#' @import AnVIL
#'
#' @param x Output file name
#' @param n The number of rows to return. Default is 6.
#' @param workspaceName Name of the workspace
#' @param submissionId Submission Id. If it's not provided, the most recent
#' submission id will be used.
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#'
tableHead <- function(x, n = 6, 
                      workspaceName,
                      submissionId = NULL,
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
    message(paste("Below outputs are from the submissionId", submissionId))

    ## Get the list of all outputs
    av_bucket <- avbucket(name = workspaceName)
    outputs <- avworkflow_files(submissionId = submissionId, bucket = av_bucket)

    output_ind <- which(outputs$file == x)
    output_path <- outputs$path[output_ind]
    res <- utils::read.table(gsutil_pipe(output_path), sep = "\t")
    utils::head(res, n)
}
