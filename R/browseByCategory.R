#' Browsing workspaces based on values under the selected category
#' 
#' @param target A character(1) specifying the AnVIL resource you want to 
#' perform searching. Available options are \code{workspaces}, \code{workflows},
#' and \code{data}.
#' @param category A metadata category you want to browse. 
#' @param display_categories A logical. Default is \code{FALSE}. If it 
#' is set to \code{TRUE}, the function returns the character vector 
#' of available \code{category} option for a selected \code{target}.
#' 
#' @examples
#' browseByCategory(target = "workspaces", category = "accessLevel")
#' 
#' @export
browseByCategory <- function(target,
                             category, 
                             display_categories = FALSE) {
    
    ## Input sanity check
    targets <- c("workspaces", "workflows", "data")
    if (target %in% targets) {
        msg <- paste("Please select the available target among these:", targets)
        stop(msg)
    }
    
    ## <<<<<<<<<<<<<<<<< Add more sanity check
    
    if (isTRUE(display_categories)) {
        res <- colnames(workspaces)
    } else {
        options <- unique(workspaces[, category])
        menu_title <- paste("Choose one from", category, "column:")
        choice <- menu(options, title = menu_title)
        
        filter_phrase <- paste0(category, "='", options[choice], "'")
        sql <- paste("SELECT namespace, name 
                 FROM workspaces
                 WHERE", filter_phrase)
        
        res <- sqldf(sql)
    }
    
    return(res)
}