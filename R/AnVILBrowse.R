#' Search keywords in a given metadata table
#' 
#' @importFrom tibble as_tibble
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
    
    res <- tibble::as_tibble(metadata[rows,])
    return(res)
}


#' Search AnVIL workspaces using keywords
#' 
#' @importFrom dplyr filter
#'
#' @param keyword A character(1). Regular expression is accepted. For example,
#' you can search multiple keywords separated by the vertical bar ("\code{|}"). 
#' @param searchFrom Under the default (\code{all}), all the workspaces 
#' containing keywords in their workspace/workflow/data will be returned.
#' The other available options are \code{workspace}, \code{workflow}, and 
#' \code{data}.
#' @param returnFrom Under the default (\code{NULL}), the same data type 
#' as for \code{searchFrom} will be used, while \code{searchFrom = "all"} 
#' returns workspaces. 
#' @param metaTables Under the default (\code{default}), all the publicly 
#' accessible AnVIL workspaces will be subjected for search. If you want 
#' to search in all the workspaces you have access to, set this argument as
#' \code{custom}, and provide the inputs for \code{workspaceTable}, 
#' \code{workflowTable}, and \code{dataTable} arguments.
#' @param minAge A number. Any data with a maximum participant age lower than 
#' this parameter will be excluded from the output. Under the default 
#' (\code{0}), no data entries will be removed due to the maximum participant 
#' age. Data entries with no maximum participant age listed will not be removed 
#' by this argument.
#' @param maxAge A number. Any data with a minimum participant age higher than 
#' this parameter will be excluded from the output. Under the default 
#' (\code{130}), no data entries will be removed due to the minimum participant 
#' age. Data entries with no minimum participant age listed will not be removed 
#' by this argument.
#' @param minCount A number. Any data with a participant count fewer than this
#' parameter will be excluded from the output. Under the default (\code{0}), no
#' data entries will be removed due to participant count. Data entries with no
#' participant count listed will not be removed by this argument.
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
#' Under the default \code{returnFrom = NULL}, it returns the same data type
#' as specified in \code{searchFrom} or workspace for \code{searchFrom = "all"}.
#' 
#' 
#' @examples 
#' AnVILBrowse("malaria")
#' AnVILBrowse("resistance")
#' AnVILBrowse("resistance", searchFrom = "workflows")
#'
#' @export
AnVILBrowse <- function(keyword, 
                        searchFrom = "all",
                        returnFrom = NULL, # defaulting with `searchFrom` parameter, options are c("workspace", "workflow","data")
                        metaTables = "default",
                        minAge = 0,
                        maxAge = 130,
                        minCount = 0,
                        workspaceTable = NULL,
                        workflowTable = NULL,
                        dataTable = NULL) {
    
    ## Load data table
    dir <- system.file("extdata", package = "AnVILWorkflow")
    lastupdate <- readLines(file.path(dir, "date_of_last_update.txt"))
    message(paste("Tables last updated on:", lastupdate, collapse = " "))
    
    # Confrim that numeric arguments are numeric
    checkNumInput <- all(c(is.numeric(maxAge), 
                           is.numeric(minAge),
                           is.numeric(minCount)))
    if (!checkNumInput) {
      error_msg <- "Provide numeric values for maxAge, minAge, and minCount."
      stop(error_msg)
    }
    
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
    
    # Filter by age range and count minimum (output inclusive of NA values)
    data_res <- subset(data_res, age_min < maxAge | is.na(age_min))
    data_res <- subset(data_res, age_max > minAge | is.na(age_max))
    data_res <- subset(data_res, count >= minCount | is.na(count))

    # Compile query results
    if (searchFrom == "all") {
        all_ws <- unique(c(ws_res$workspace_key, 
                           wf_res$workspace_key, 
                           data_res$workspace_key))
        ind <- which(workspaceTable$workspace_key %in% all_ws)
        res <- workspaceTable[ind,]
    } else if (searchFrom == "workspace") {
        res <- ws_res
    } else if (searchFrom == "workflow") {
        res <- wf_res
    } else if (searchFrom == "data") {
        res <- data_res
    } else {
        error_msg <- "Provide a correct input for the searchFrom argument."
        stop(error_msg)
    }

    # Requested return type
    if (is.null(returnFrom)) {
      formatted_res <- res
    } else if (returnFrom == "workspaces") {
      formatted_res <- dplyr::filter(workspaceTable, 
                                     workspace_key %in% res$workspace_key)
    } else if (returnFrom == "workflows") {
      formatted_res <- dplyr::filter(workflowTable, 
                                     workspace_key %in% res$workspace_key)
    } else if (returnFrom == "data") {
      formatted_res <- dplyr::filter(dataTable, 
                                     workspace_key %in% res$workspace_key)
    } else {
      error_msg <- "Provide a correct input for the returnFrom argument."
      stop(error_msg)
    }
    
    # Return results if applicable
    if (!nrow(formatted_res)) {
        msg <- paste(stringr::str_to_title(searchFrom), 
                     "table(s) doesn't include the keyword.")
        message(msg)
    }
    return(as_tibble(formatted_res))
}
