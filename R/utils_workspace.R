#' Get the fullname of the workspace
#' 
#' @param ws_name Character(1). Name of the template workspace name you 
#' want to clone. You can provide \code{name} or \code{namespace/name}.
#' 
#' @return Character(1) of \code{workspaceNamespace/workspaceName}
#' 
.get_workspace_fullname <- function(ws_name) {
    
    ## Input validity check
    ws_name_split <- unlist(strsplit(ws_name, "/")) # in case full name is provided
    
    ## Get all the workspace
    all_ws <- avworkspaces() # gcloud_account should be set already.
    ind <- which(all_ws$name == tail(ws_name_split, 1))
    
    ## Check whether the template workspace exist
    if (length(ind) == 0) {
        stop(paste(ws_name, "does not exist or you do not have access to it."))
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