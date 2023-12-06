#' Search keywords in a given metadata table
#' 
#' @param keyword A character(1). Regular expression is accepted. For example,
#' you can search multiple keywords separated by the vertical bar ("\code{|}"). 
#' @param metadata A data frame. Metadata table of workspace, workflow, or 
#' AnVIL data.
#' 
#' @returns A data frame. A subset of input metadata table with the rows
#' containing the keyword.
#' 
.search_keyword <- function(keyword, metadata) {
    
    rows <- c()
    for (row in seq_len(nrow(metadata))) {
        checkCell <- grepl(keyword, 
                           as.character(metadata[row,]), 
                           ignore.case = TRUE)
        if (any(checkCell)) {
            ## Add the row index containing the keyword
            rows <- c(rows, row) 
        }
    }
    
    res <- as_tibble(metadata[rows,])
    return(res)
}


#' Seach AnVIL workspaces using keywords
#'
#' @param keyword A character(1). Regular expression is accepted. For example,
#' you can search multiple keywords separated by the vertical bar ("\code{|}"). 
#' @param returnFrom Under the default (\code{all}), all the workspaces 
#' containing keywords in their workspace/workflow/data will be returned.
#' The other available options are \code{workspace}, \code{workflow}, and 
#' \code{data}.
#' @param metaTables Under the default (\code{default}), all the publicly 
#' accessible AnVIL workspaces will be subjected for search. If you want 
#' to search in all the workspaces you have access to, set this argument as
#' \code{custom}, and provide the inputs for \code{workspaceTable}, 
#' \code{workflowTable}, and \code{dataTable} arguments.
#' @param workspaceTable A data frame. This argument is counted only when 
#' \code{metaTables = "custom"}. Provide the output from the 
#' \code{getWorkspaces} function, to search in all the workspaces 
#' you have access to.
#' @param workflowTable A data frame. This argument is counted only when 
#' \code{metaTables = "custom"}. Provide the output from the 
#' \code{getWorkflows} function, to search in all the workflows
#' you have access to.
#' @param dataTable A data frame. This argument is counted only when 
#' \code{metaTables = "custom"}. Provide the output from the \code{getData} 
#' function, to search in all the AnVIL data you have access to.
#'
#' @return A data frame of AnVIL resources containing keywords. Depending on
#' the \code{returnFrom} argument, it can be workspaces, workflows, or data.
#' 
#' @examples 
#' AnVILBrowse("malaria")
#' AnVILBrowse("resistance")
#' AnVILBrowse("resistance", returnFrom = "workflows")
#'
#' @export
AnVILBrowse <- function(keyword, 
                        returnFrom = "all",
                        metaTables = "default",
                        workspaceTable = NULL,
                        workflowTable = NULL,
                        dataTable = NULL) {
    
    ## Load data table
    dir <- system.file("extdata", package = "AnVILWorkflow")
    if (metaTables == "default") {
        workspaceTable <- read.csv(file.path(dir, "AnVILworkspaces.csv"))
        workflowTable <- read.csv(file.path(dir, "AnVILworkflows.csv"))
        dataTable <- read.csv(file.path(dir, "AnVILdata.csv"))
    } else if (metaTables == "custom") {
        checkInput <- any(c(is.null(workspaceTable), 
                            is.null(workflowTable),
                            is.null(dataTable)))
        if (checkInput) {
            error_msg <- "Provide the proper inputs for workspaceTable, 
            workflowTable, and dataTable"
            stop(error_msg)
        }
    } else {
        error_msg <- "Provide the proper input for the metaTables argument."
        stop(error_msg)
    }
    
    ws_res <- .search_keyword(keyword, workspaceTable)
    wf_res <- .search_keyword(keyword, workflowTable)
    data_res <- .search_keyword(keyword, dataTable)
    
    # Compile query results
    if (returnFrom == "all") {
        all_ws <- unique(c(ws_res$workspace_key, 
                           wf_res$workspace_key, 
                           data_res$workspace_key))
        ind <- which(ws_res$workspace_key %in% all_ws)
        res <- ws_res[ind,]
    } else if (returnFrom == "workspace") {
        res <- ws_res
    } else if (returnFrom == "workflow") {
        res <- wf_res
    } else if (returnFrom == "data") {
        res <- data_res
    } else {
        error_msg <- "Provide a correct input for the returnFrom argument."
        stop(error_msg)
    }

    # Return results if applicable
    if (!nrow(res)) {
        msg <- paste(stringr::str_to_title(returnFrom), 
                     "table(s) doesn't include the keyword.")
        print(msg)
    } else {
        return(res)
    }
}