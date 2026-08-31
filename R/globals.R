## Column names used in non-standard evaluation (dplyr verbs, magrittr `.`)
## are declared here to avoid "no visible binding for global variable" NOTEs.
utils::globalVariables(c(
    ".",
    ## AnVILBrowse()
    "workspace_key",
    ## getAllWorkspaces()
    "workspace.workspaceId",
    "workspace.attributes.library:indication",
    "workspace.attributes.library:studyDesign",
    "workspace.attributes.library:numSubjects",
    "workspace.attributes.library:primaryDiseaseSite",
    "workspace.attributes.library:cohortCountry",
    "workspace.attributes.library:projectName",
    "workspace.attributes.library:reference",
    ## getWorkspaces()
    "cloudPlatform", "namespace", "name", "accessLevel", "public",
    "isLocked", "workspace_namespace", "workspace_name",
    "datatype", "itemsType", "items",
    ## updateInput()
    "ws_namespace", "ws_name", "ws_fullname", "wf_fullname"
))
