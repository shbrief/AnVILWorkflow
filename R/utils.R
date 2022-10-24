.set_gcloud <- function(accountEmail, billingProjectName) {
    gcloud_account(accountEmail)
    gcloud_project(billingProjectName)
    # avworkspace_namespace(billingProjectName)
    # avworkspace_name(workspaceName)
}


#' Subset to non-metadata output files
#'
#' @import AnVIL
#'
#' @param workflowOutputs A data frame of workflow outputs with four 
#' columns: file, workflow, task, and path. Returned value 
#' from \code{\link[AnVIL]{avworkflow_files}}.
#'
#' @return A character vector containing the names of non-metadata output files
#'
.nonMetadataOutputs <- function(workflowOutputs) {
    filenames <- workflowOutputs$file

    ind1 <- which(filenames %in% c("stderr","stdout","rc","script","output"))
    ind2 <- grep(".sh$", filenames)   # bash files
    ind3 <- grep(".log$", filenames)   # log files

    meta_ind <- c(ind1, ind2, ind3)
    res <- workflowOutputs[-meta_ind,]
    return(res)
}

#' Stop the execution without error messages
.stop_quietly <- function() {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
}


#' Get the fullname of the workspace
#' 
#' @import utils
#' @param workspaceName Character(1). Name of the template workspace name you 
#' want to clone. You can provide \code{name} or \code{namespace/name}.
#' 
#' @return Character(1) of \code{workspaceNamespace/workspaceName}
#' 
.get_workspace_fullname <- function(workspaceName) {
    
    ## In case `namespace/name` is provided as workspacename
    ws_name_split <- unlist(strsplit(workspaceName, "/")) 
    
    ## Get all the workspace
    all_ws <- avworkspaces() # gcloud_account should be already set for this.
    ind <- which(all_ws$name == utils::tail(ws_name_split, 1))
    fullnames <- paste(all_ws$namespace[ind], all_ws$name[ind], sep = "/")
    
    ## Use the provided `namespace/name` if it is correct
    if (workspaceName %in% fullnames) {
        return(workspaceName)
        .stop_quietly()
    }
    
    ## Check whether the template workspace exist
    if (length(ind) == 0) {
        stop(paste(workspaceName, 
                   "workspace does not exist or you do not have access to it."))
    } else if (length(ind) == 1) {
        ws_namespace <- all_ws$namespace[ind]
        ws_name <- all_ws$name[ind]
    } else { # if there are multiple workspaces with the same name
        ws_fullname <- paste(all_ws$namespace[ind], 
                             all_ws$name[ind], sep = "/")
        if (ws_name %in% ws_fullname) { # if namespace is specified
            ws_namespace <- ws_name_split[1]
            ws_name <- ws_name_split[2]
        } else { # if namespace is NOT specified
            message(paste("Specify the complete name of", 
                          ws_name, "from the followings."))
            print(paste(all_ws$namespace[ind], all_ws$name[ind], sep = "/"))
            stop()
        }
    }
    
    ## Return workspaceNamespace/workspaceName
    res <- paste(ws_namespace, ws_name, sep = "/")
    return(res)
}


#' Get the workflow namespace and name
#' 
#' Use this internally when \code{\link{setCloudEnv}} is already run.
#' 
#' @param workspaceName A character. Name of the workspace to use. 
#' @param workflowName A character. Name of the workflow to run. If a single
#' workflow is available under the selected workspace, this function will
#' check the input of that workflow under the default (\code{NULL}). If there
#' are multiple workflows available, you should specify the workflow. 
#' 
#' @return A character of \code{"workflow_namespace/workflow_name"}
#'
.get_workflow_fullname <- function(workspaceName,
                                   workflowName = NULL) {
    
    ws_fullname <- .get_workspace_fullname(workspaceName = workspaceName)
    
    ## Get all the available workflow
    res <- avworkflows(namespace = unlist(strsplit(ws_fullname,split = "/"))[1],
                       name = unlist(strsplit(ws_fullname,split = "/"))[2])
    
    ## Select a workflow
    if (nrow(res) == 0) {
        stop("This workspace does not have any workflow.")
    } else if (nrow(res) == 1) {
        wf_fullname <- paste(res$namespace, res$name, sep = "/")
    } else if (is.null(workflowName)) {
        warning("Please specify the workflowName from the following: ")
        print(res)
        stop()
    } else {
        ind <- which(res$name == workflowName)
        wf_fullname <- paste(res$namespace[ind], res$name[ind], sep = "/")
    }
    
    return(wf_fullname)
}
