#' Clone template workspace
#'
#' @import AnVIL
#'
#' @param workspaceName Name of the workspace you are creating
#' @param templateName Character(1). Name of the template workspace name you 
#' want to clone. You can provide \code{name} or \code{namespace/name}.
#' @param analysis Character(1). Name of the analysis you want to clone 
#' it's workspace. The list of available analyses can be found using 
#' \code{\link{availableAnalysis}}.
#' @param accountEmail Character(1). Email linked to Terra account
#' @param billingProjectName Character(1). Name of the billing project
#'
#' @return Name of the cloned workspace
#'
#' @export
cloneWorkspace <- function(workspaceName, 
                           templateName,
                           analysis = NULL,
                           accountEmail = gcloud_account(), 
                           billingProjectName = gcloud_project()) {

    ## Setup gcloud account/project
    setCloudEnv(accountEmail = accountEmail,
                billingProjectName = billingProjectName,
                message = FALSE)

    ## Get the WorkspaceNamespace and WorkspaceName of the template
    dir <- system.file("extdata", package = "AnVILWorkflow")
    map <- utils::read.table(file.path(dir, "map.tsv"), header = TRUE)
    
    if (!is.null(analysis)) { # using one of the curated/supported workspaces
        ind <- which(map$analysis == analysis)
        template_ws_namespace <- map$workspaceNamespace[ind]
        template_ws_name <- map$workspaceName[ind]
    } else {
        res <- .get_workspace_fullname(templateName)
        template_ws_namespace <- unlist(strsplit(res, "/"))[1]
        template_ws_name <- unlist(strsplit(res, "/"))[2]
    }

    ## Clone the workspace
    res <- try(avworkspace_clone(namespace = template_ws_namespace,  
                                 name = template_ws_name,  
                                 to_namespace = billingProjectName,
                                 to_name = workspaceName),
               silent = TRUE)

    if (methods::is(res, "try-error")) {
        return(message(res))
    } else {
        message(paste(res, "is succesfully created."))
    }
}
