## Create AnVILworkspaces.csv file
allWorkspaces <- getWorkspaces()
workspaces <- subset(allWorkspaces, public == "TRUE") 
write.csv(workspaces,
          file = "~/Packages/AnVILWorkflow/inst/extdata/AnVILworkspaces.csv",
          row.names = FALSE)

## Create AnVILworkflows.csv file
allWorkspaces <- getWorkspaces()
workspaces <- subset(allWorkspaces, public == "TRUE") 
workflows <- getWorkflows(workspaces)
write.csv(workflows,
          file = "~/Packages/AnVILWorkflow/inst/extdata/AnVILworkflows.csv",
          row.names = FALSE)

## Create AnVILdata.csv file
allWorkspaces <- getWorkspaces()
workspaces <- subset(allWorkspaces, public == "TRUE") 
data <- getData(workspaces)
write.csv(data,
          file = "~/Packages/AnVILWorkflow/inst/extdata/AnVILdata.csv",
          row.names = FALSE)
