#' Search workspaces containing keyword
#'
#' @param keyword A character(1).
#' @param data Under the default (\code{NULL}), metadata of all the pulbically
#' accessible AnVIL workspaces will be used. If you want to search in all the
#' workspaces you have access to, provide the output from \code{__________} 
#' function.
#'
#' @return A data frame of workspaces containing a keyword in their metadata
#' 
#' @example searchWorkspaces("lymphoma")
#'
#' @export
searchWorkspaces <- function(keyword, wsData = NULL) {
    
    ## Load data table
    if (is.null(wsData)) {
        dir <- system.file("extdata", package = "AnVILWorkflow")
        wsData <- readr::read_csv(file.path(dir, "workspaces.csv"))
    }
    
    ## Check the keyword for each row
    res <- .primary_df_query(keyword, data = wsData)
    
    if (!nrow(res)) {
        msg <- "No workspaces matched your search criteria."
        print(msg)
    } else {
        return(res)
    }
}

#' Search workflows containing keyword
#'
#' @param keyword A character(1).
#' @param wsData Under the default (\code{NULL}), all the pulbically 
#' accessible AnVIL workspaces will be subjected for search. If you want 
#' to search in all the workspaces you have access to, provide the output 
#' from the \code{__________} function.
#' @param wfData Under the default (\code{NULL}), all the pulbically 
#' accessible AnVIL workflows will be subjected for search. If you want 
#' to search in all the workflows you have access to, provide the output 
#' from the \code{__________} function.
#' @param join
#'
#' @return A data frame of workflows containing a keyword in their metadata
#' 
#' @examples 
#' searchWorkflows("malaria")
#' searchWorkflows("resistance")
#'
#' @export
searchWorkflows <- function(keyword, 
                            wsData = NULL,
                            wfData = NULL, 
                            join = "u") {
    
    ## Load data table
    dir <- system.file("extdata", package = "AnVILWorkflow")
    if (is.null(wsData)) {
        wsData <- read.csv(file.path(dir, "workspaces.csv"))
    } 
    if (is.null(wfData)) {
        wfData <- read.csv(file.path(dir, "workflows.csv"))
    }
    
    # Query the workflows dataframe
    p_res <- .primary_df_query(keyword, wfData)
    # Query the workspaces dataframe
    s_res <- .secondary_df_query(keyword, wfData, wsData)
    
    # Compile query results
    if ((nrow(p_res) > 0) & (nrow(s_res) > 0)) {
        if (join == "i") {
            ind <- which(p_res$name_key %in% s_res$name_key)
            search_results <- p_res[ind,]
        } else {
            ind <- which(!(p_res$name_key %in% s_res$name_key))
            search_results <- rbind(s_res, p_res[ind,])
        }
    } else if (nrow(p_res) > 0) {
        search_results <- p_res
    } else {
        search_results <- s_res
    }
    
    # Return results if applicable
    if (!nrow(search_results)) {
        msg <- "No workflows matched your search criteria."
        print(msg)
    } else {
        res <- as_tibble(search_results)
        return(res)
    }
}


#' Search sample metadata containing keyword
#'
#' @param keyword A character(1).
#' @param wsData Under the default (\code{NULL}), all the pulbically 
#' accessible AnVIL workspaces will be subjected for search. If you want 
#' to search in all the workspaces you have access to, provide the output 
#' from the \code{__________} function.
#' @param wfData Under the default (\code{NULL}), all the pulbically 
#' accessible AnVIL workflows will be subjected for search. If you want 
#' to search in all the workflows you have access to, provide the output 
#' from the \code{__________} function.
#' @param join
#'
#' @return A data frame of workflows containing a keyword in their metadata
#' 
#' @examples 
#' searchData("lymph node")
#' searchData("lung cancer")
#' searchData("female", sampleAttribute = "GENDER")
#' searchData("resistance")
#' searchData("resistance", join = "i")
#'
#' @export
searchData <- function(keyword, 
                       sampleData = all_data, 
                       wsData = all_spaces, 
                       sampleAttribute = (1:ncol(sampleData)),
                       spacecolumns = (1:ncol(wsData)), 
                       join = "u") {
    # Query the workflows dataframe
    p_res <- .primary_df_query(keyword, sampleData, sampleAttribute)
    # Query the workspaces dataframe
    s_res <- .secondary_df_query(keyword, sampleData, wsData, spacecolumns)
    
    # Compile query results
    if ((nrow(p_res) > 0) & (nrow(s_res) > 0)){
        if (join=="i"){
            search_results <- p_res[which(p_res$name_key %in% s_res$name_key),]
        }else{
            search_results <- rbind(s_res, p_res[which(!(p_res$name_key %in% s_res$name_key)),])
        }
    }else if(nrow(p_res) > 0){
        search_results <- p_res
    }else{
        search_results <- s_res
    }
    
    # Return results if applicable
    if (!nrow(search_results)){
        print("No sample data matched your search criteria.")
    }else{
        return(search_results)
    }
}