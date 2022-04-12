#' Setup Google Cloud Account and Project
#'
#' @import AnVIL
#'
#' @param accountEmail Email linked to your Terra account 
#' @param billingProjectName Name of the billing project
#' @param workspaceName Name of the workspace
#' @param message Under the default (\code{TRUE}), this function will print out
#' Google Cloud Account and Billing Project set in the working environment
#'
#' @export
setCloudEnv <- function(accountEmail = gcloud_account(), 
                        billingProjectName = gcloud_project(),
                        workspaceName = avworkspace_name(),
                        message = TRUE) {
    
    ## Check whether gcloud exists
    if (!gcloud_exists()) {
        stop("You should install Google Cloud SDK before this setup.")
    }
    
    ## Check whether gcloud logged-in
    if (identical(accountEmail, character(0))) {
        stop("Please setup your Google Account")
    }
    if (!identical(accountEmail, gcloud_account())) {
        gcloud_account(accountEmail)
    }
    
    ## Check whether project is selected
    if (identical(billingProjectName, character(0))) {
        stop("Please provide your project name")
    }
    if (!identical(billingProjectName, gcloud_project())) {
        gcloud_project(billingProjectName)
    }
    
    ## Check whether workspace is selected
    if (identical(workspaceName, character(0))) {
        stop("Please provide your workspace name")
    }
    if (!identical(workspaceName, avworkspace_name())) {
        avworkspace_name(workspaceName)
    }
    
    ## Print the Google Cloud environment information for the current session
    if (message) {
        print(paste("Google Cloud Account:", gcloud_account()))
        print(paste("Billing project:", gcloud_project()))
    }
}