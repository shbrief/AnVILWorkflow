.set_gcloud <- function(accountEmail, billingProjectName) {
    gcloud_account(accountEmail)
    gcloud_project(billingProjectName)
    # avworkspace_namespace(billingProjectName)
    # avworkspace_name(workspaceName)
}


#' Get method configuration
#'
#' @import AnVIL
#'
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace you want to get method configuration.
#'
#' @return A data frame containing method configuration. \code{name} and \code{namespace}
#' slots contain workspacename and workspacenamespace, respectively.
#'
.getMethodConfig <- function(billingProjectName, workspaceName) {

    ## Check whether the target workspace exists
    all_workspaces <- avworkspaces()
    ind <- grep(billingProjectName, all_workspaces$namespace)
    if (length(ind)==0) {stop("Workspace doesn't exist under the provided billing project.")}

    ## Get method configuration
    resp <- Terra()$listWorkspaceMethodConfigs(workspaceNamespace = billingProjectName,
                                               workspaceName = workspaceName,
                                               allRepos = TRUE)
    res <- jsonlite::fromJSON(httr::content(resp, "text"), simplifyVector = TRUE)
    return(res)
}


#' Get the most recent Submission Id
#'
#' @import AnVIL
#'
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace you want to get method configuration.
#'
#' @return Submission Id of the most recent submission
#'
.mostRecentSubmissionId <- function(billingProjectName, workspaceName) {
    submissions <- avworkflow_jobs(namespace = billingProjectName,
                                   name = workspaceName)
    submissionId <- submissions$submissionId[1]
    return(submissionId)
}


#' Get the submission id of the most recently succeeded submission
#'
#' @import AnVIL
#'
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace you want to get method configuration.
#'
#' @return Submission Id of the most recent submission
#'
.mostRecentSucceededSubmissionId <- function(billingProjectName, workspaceName) {
    submissions <- avworkflow_jobs(namespace = billingProjectName,
                                   name = workspaceName)
    ind <- which(submissions$succeeded == 1)[1]
    if (is.na(ind)) {
        print("There is no previous submission done with 'succeeded' status.")
    } else {
        submissionId <- submissions$submissionId[ind]
        return(submissionId)
    }
}


#' Subset to non-metadata output files
#'
#' @import AnVIL
#'
#' @param workflowOutputs A data frame of workflow outputs with four columns - file,
#' workflow, task, and path. Returned value from \code{\link[AnVIL]{avworkflow_files}}.
#'
#' @return A character vector containing the names of non-metadata output files
#'
.nonMetadataOutputs <- function(workflowOutputs) {
    filenames <- workflowOutputs$file

    ind1 <- which(filenames %in% c("stderr", "stdout", "rc", "script", "output"))
    ind2 <- grep(".sh$", filenames)   # bash files
    ind3 <- grep(".log$", filenames)   # log files

    meta_ind <- c(ind1, ind2, ind3)
    res <- workflowOutputs[-meta_ind,]
    return(res)
}

