#' Check the status of submitted jobs
#'
#' @import AnVIL
#'
#' @param workspaceName Name of the workspace
#' @param accountEmail Email linked to Terra account
#' @param billingProjectName Name of the billing project
#'
#' @return A tibble summarizing submitted workflow jobs. Contains information such
#' as submission Id, submission date, and submission status.
#'
#' @export
monitorSubmission <- function(workspaceName,
                              accountEmail = gcloud_account(), 
                              billingProjectName = gcloud_project()) {

    ## Setup gcloud account/project
    setCloudEnv(accountEmail = accountEmail, 
                billingProjectName = billingProjectName,
                message = FALSE)

    res <- avworkflow_jobs(namespace = billingProjectName,
                           name = workspaceName)

    if (length(res) == 0) {
        stop("There is no previously submitted job.", call. = FALSE)
    } else {return(res)}
}
