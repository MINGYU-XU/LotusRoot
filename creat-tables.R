#
# Creat tables 
# to store raw datasets and project information
#
#

## raw datasets table 
datasets <- data.frame(Sample_name = character(0),
                       Description = character(0),
                       Date = character(0),
                       Location = character(0),
                       Datatype = character(0),
                       Lab_researcher = character(0),
                       Status = character(0),  ## private私密,publish公开,archived存档
                       stringsAsFactors=FALSE)



## project information table
projects <- data.frame(Project_name = character(0),
                       Description = character(0),
                       Date = character(0),
                       Location = character(0),
                       Datasets_included = character(0),
                       Lab_researcher_access = character(0),
                       Related_projects = character(0),
                       External_links = character(0),
                       Status = character(0),  ## Live / completed / published / archived
                       stringsAsFactors=FALSE)
