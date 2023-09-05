## All the public workspaces
allWorkspaces <- getAllWorkspaces()
workspaces <- subset(allWorkspaces, public == "TRUE") 
write.csv(workspaces,
          file = "~/Packages/AnVILWorkflow/inst/extdata/allWorkspaces.csv",
          row.names = FALSE)

## All the workflows from the public workspaces
allWorkflows <- getAllWorkflows(workspaces = workspaces) %>% tibble::as.tibble()
allWorkflows <- apply(allWorkflows, 2, as.character) # coercing a list (somewhere) into character
write.csv(allWorkflows,
          file = "~/Packages/AnVILWorkflow/inst/extdata/allWorkflows.csv",
          row.names = FALSE)

## All the data tables from the public workspaces
allDataTables <- getAllDataTables(workspaces = workspaces[-274,]) %>% #<<<<<<<<< a specific workspace with issue (`terracontest/TOSC19-idap`)
    tibble::as.tibble()
write.csv(allDataTables,
          file = "~/Packages/AnVILWorkflow/inst/extdata/allDataTables.csv",
          row.names = FALSE)