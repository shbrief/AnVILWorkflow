#' Browsing keyword in AnVIL workspaces, workflows, and data
#' 
#' @param keyword 
#' 
#' @examples
#' browseByCategory(keyword = "CNV")
#' 
#' @export
browseByKeyword <- function(keyword, 
                            updateAllMeta = FALSE) {

    ## Load data files
    if (isFALSE(updateAllMeta)) {
        dir <- system.file("extdata", pacakge = "AnVILWorkflow")
        workspaces <- read.csv(file.path(dir, "allWorkspaces.csv"))
        workflows <- read.csv(file.path(dir, "allWorkspaces.csv"))
        data <- read.csv(file.path(dir, "allDataTables.csv"))
    }
    
    ## NAs into empty strings to deal with SQL nulls
    workspaces[is.na(workspaces)] <- ""
    workflows[is.na(workflows)]   <- ""
    data[is.na(data)]             <- ""
    
    columns_workspaces <- paste(names(workspaces), collapse=' || \n ')
    columns_workflows  <- paste(names(workflows),  collapse=' || \n ')
    columns_data       <- paste(names(data),       collapse=' || \n ')
    
        
}


## From Paul's test script
# workspaces <- read.csv("~/Packages/AnVILWorkflow/inst/extdata/allWorkspaces.csv")
# workspaces[is.na(workspaces)] <- ""
# columns_workspaces <- names(workspaces)[names(workspaces) != "workspaceId"] |>
#     paste(collapse = " || \n ")
# 
# dbWriteTable(mydb, "workspaces", workspaces, overwrite = TRUE)
# dbListTables(mydb)
# 
# sql <- paste("SELECT DISTINCT name AS ID, namespace, name, subject_count, 
#               'workspace' AS search_unit,", columns_workspaces, 
#              "AS search_string FROM workspaces LIMIT 5")
# 
# sql_df1 <- sqldf::sqldf(sql)
# sql_df1
# 
# 
# search_param <- "GATK4"
# sql2 <- paste0("SELECT ID, namespace, name, subject_count FROM sql_df1 WHERE search_string LIKE '%", search_param, "%'")
# 
# sql_df2 <- sqldf::sqldf(sql2)
# sql_df2