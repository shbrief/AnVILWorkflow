#' Setup Google Cloud Account and Project
#'
#' @import AnVIL
#'
#' @param accountEmail Character(1). Email linked to your Terra account. 
#' @param billingProjectName Character(1). Name of the billing project, which 
#' is the gcloud account.
#' @param message Under the default (\code{TRUE}), this function will print out
#' Google Cloud Account and Billing Project set in the working environment
#' 
#' @return Terra/AnVIL working environment - Google Cloud billing account
#' and the billing project name - will be printed out. 
#'
#' @export
setCloudEnv <- function(accountEmail = gcloud_account(), 
                        billingProjectName = gcloud_project(),
                        message = TRUE) {
    
    ## Check whether gcloud exists
    if (!gcloud_exists()) {
        stop("You should install Google Cloud SDK before this setup.")
    }
    
    ## Check whether gcloud account/project are set
    if (!nzchar(accountEmail)) {
        stop("Please enter your Google account linked to Terra/AnVIL")
    } else if (!nzchar(billingProjectName)) {
        stop("Please provide your project name.")
    }
    
    ## Update gcloud_account
    if (!identical(accountEmail, gcloud_account())) {
        ## Check the account is 'email'
        isValidEmail <- function(x) {
            grepl("\\<[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\>", 
                  as.character(x), ignore.case=TRUE)
        }
        if (!isValidEmail(accountEmail)) {
            stop("Terra account name should be an email.")
        } 
        gcloud_account(accountEmail)
    }
    
    ## Update glcoud_project
    if (!identical(billingProjectName, gcloud_project())) {
        gcloud_project(billingProjectName)
    }
    
    ## Print the Google Cloud environment information for the current session
    if (message) {
        message("Terra/AnVIL working environment for the current session:")
        print(paste("Google Cloud Account:", gcloud_account()))
        print(paste("Billing project:", gcloud_project()))
    }
}