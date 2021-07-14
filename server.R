#
# This is my laboratory information management system (LIMS)
# Shiny web application server 
#


## Functions --------------------------------------------------------------------
# print_rows_cols
# print row+column selected
print_rows_cols = function(id) {
  cat('Rows selected:\n')
  print(input[[paste0(id, '_rows_selected')]])
  cat('Columns selected:\n')
  print(input[[paste0(id, '_columns_selected')]])
}







## Read files---------------------------------------------------------------

#read test files
testfile_dataset <- read.csv('csv_test2_data.csv') #sep = ','
testfile_project <- read.csv('csv_test2_proj.csv')
#datas <- testfile_dataset
#proj <- testfile_project

#datas<-readRDS("df_data.rds") ### error
datas <- read.csv('df_data.csv')

#####Error in as.data.frame.default: cannot coerce class ‘c("reactiveVal", "reactive", "function")’ to a data.frame

proj <- read.csv('df_proj.csv')#!!!EDIT

#dataVal <- datas
#projVal <- proj



# register (user information table)
user <- read.csv('register.csv')





##£ secure credentials info 
if (interactive()) {
  
  # define some credentials
  credentials <- data.frame(
    user = c("test", "123", "z"),
    password = c("test", "123", "z"),
    stringsAsFactors = FALSE
  )}







server <- function(input, output) {
  
  # HOME page ----------------------------------------------------------------
  # check_credentials returns a function to authenticate users
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  
  output$ui_register <- renderUI(fluidPage(
    br(),
    textInput(inputId = "registerName", 
              label = "User Name :"),
    passwordInput(inputId = "registerpw", 
                  label = "Password :"),
    textInput(inputId = "email",
              label = "Email :",
              placeholder = "your email"),
    selectInput(inputId = "group", "Which Group/Lab/research center you belong to?", 
                choices = c('Bird','Hill','Wind','Ed LAB','BioSci','CHEM') ## can add more
                ),
    selectInput(inputId = "permissions", "Your permission :", 
                choices = c('General_Staff','Data_Administrator','Project_Supervisor ','System_Maintenance') ## can add more
    ),
    actionButton("register_ok","OK")
  )
  )                                       
  
  shinyjs::hide("ui_register")
  observeEvent(input$register, {
    shinyjs::show("ui_register", anim = TRUE, animType = "fade")
  })
  
  
  # new user register
  userVal <- reactiveVal(user)
  
  observeEvent(input$register_ok,{
    print("ok")
    output$successfully_registered <- renderPrint({
      cat("Hi",input$registerName,"!","You have registered successfully! Please log in.")
    })
    u <- rbind(data.frame(Name = input$registerName,
                          Password = input$registerpw,
                          Email = input$email,
                          Group.Lab.Center = input$group,
                          Permissions = input$permissions),userVal())    
    
    userVal(u)
  })
  
  # save user information to the table
  observeEvent(input$register_ok,{
    write.csv(userVal(),'register.csv',row.names = FALSE)
  })
  
  
  
  
  
  
  
  
  
  
  options(DT.options = list(pageLength = 10)) ## The initial display is 10 rows
  
### Create new proj/Datarow ###################################
  
  projVal <- reactiveVal(proj)
  
  observeEvent(input$add_proj,{
    
    p <- rbind(data.frame(Project.ID = input$projID,  #paste0("P",nrow(proj)+1),
                          Project.Name = input$projName, 
                          Parent = input$projParent,
                          Description = input$projDescription, 
                          Start.Date = as.character(input$projDate),
                          End.Date = as.character(input$projDate2),
                          Path = input$projPath, 
                          Sample.Sheet = input$projSample,
                          Researcher = input$projResearcher, 
                          Bioinformatician = input$projBioinformatician,
                          Group = input$projGroup,
                          Report = input$projReport,
                          Status = input$projStatus,
                          Data.Repository = input$projdataRepository,
                          Code.Repository = input$codeRepository,
                          Permissions = input$projPermissions),projVal())
    
    projVal(p)
  })
  

  
  dataVal <- reactiveVal(datas)
  
  observeEvent(input$add_data,{
    
    t <- rbind(data.frame(Data.ID = input$dataID, 
                          Project.ID = input$dataprojID,
                          Sample.Name = input$sampleName,
                          Description = input$dataDescription, 
                          Date = as.character(input$dataDate),
                          Path = input$dataPath, 
                          Data.Repository = input$dataRepository,
                          Method = input$method,
                          Organism = input$organism,
                          Tissue.Cell = input$cell, 
                          Genotype = input$genotype,
                          Format = input$format,
                          Treatment = input$treatment),dataVal())    
    
    dataVal(t)
  })
  
  
  
  
### edit a cell ##############################################
  ###最好是让用户选择一行，点击“编辑”按钮，然后在另一个框中编辑并保存更改。###
  observeEvent(input$edit_proj, {
    proj <<- editData(projVal(), input$x1_cell_edit, 'x1') ## double <
    
  })
  
  
### Associating two tables ########################################
  # 通过project id关联
  
  # proj -> data # 预期功能：选中一个proj后，显示该项目中包含的datasets
  # Expected function: selecte a proj then the Datasets included in it are displayed  
  
  ## ???????????????????????????????????????????????????????????????????
  ## if no row(in x1) selected, No table
  ## if select one parent proj, display its sub proj 
  ## if select one sub proj, display the datasets included in that proj 如果选择了某含，则显示该proj的datasets
  ## return：'pids' is the project ID
  
  pids<-reactive({
    if(length(input$x1_rows_selected)==0){
      #pids<-projVal(input$x1_rows_all[1],)
      
      return(NULL)  ## No output
    }
    else{
      pids <- projVal()[input$x1_rows_selected,"Project.ID"]
      
    }
    print(pids)
    return(pids)
  })
  
  #???????????????? different selection, different output
  output$related_datasets <- DT::renderDT({
    pid<-pids()
    
    if(length(pid)==0 ){
      return(NULL)
      
    } else if(!is.null(pid[input$x1_rows_selected,"Parent"]) & (pid[input$x1_rows_selected,"Parent"] %in% proj[,"Parent"]))
      {
      subproj <- projVal() %>% filter(Parent %in% pid) 
      ## Filter the table
      
      dt <- DT::datatable(subproj,
                          selection="single",
                          filter="bottom",
                          options = list(SortClasses = TRUE,
                                         scrollX = TRUE,
                                         LengthMenu = c(1,5,10,20,50), 
                                         pageLength = 10)
      )
      
      dt
      
    }else{
      ds <- dataVal() %>% filter(Project.ID  %in% pid) 
      ## Filter the table
      
      dt <- DT::datatable(ds,
                          selection="single",
                          filter="bottom",
                          options = list(SortClasses = TRUE,
                                         scrollX = TRUE,
                                         LengthMenu = c(1,5,10,20,50), 
                                         pageLength = 10)
      )
      
      dt <- dt %>% formatStyle('pval',target = 'row')
      
      dt
    }
    
  })
  
  
  
  
### data -> proj  预期功能：选中一个data后，显示包含该data的所有proj
  
  ## if no row(in x1) selected, No table
  ## if select one row, display the projs include the data 如果选择了某含，则显示该proj的datasets
  ## return：'pids' is the project ID
  
  pids2<-reactive({
    if(length(input$x2_rows_selected)==0){
      #pids<-projVal(input$x1_rows_all[1],)
      
      return(NULL)  ## No output
    }
    else{
      pids2 <- dataVal()[input$x2_rows_selected,"Project.ID"]
      
    }
    print(pids2)
    return(pids2)
  })
  
  
  output$related_proj <- DT::renderDT({
    pid2<-pids2()
    
    if(length(pid2)==0){
      return(NULL)
    }
    
    
    ds2<-projVal() %>% filter(Project.ID  %in% pid2) 
    ## Filter the table
    
    dt2 <- DT::datatable(ds2,
                        selection="single",
                        filter="bottom",
                        options = list(SortClasses = TRUE,
                                       scrollX = TRUE,
                                       LengthMenu = c(1,5,10,20,50), 
                                       pageLength = 10
                        )
    )
    
    
    
    dt2 <- dt2 %>% formatStyle('pval',target = 'row')
    
    dt2
  })
  
  
  
  
### output tables ################
  
  # proj table
  
  projVal <- reactiveVal(proj)
  output$x1 <- renderDT(projVal(),
                        server = FALSE,     ## client-side processing 
                        selection = list(target = 'row'),   ## Multiple selection: rows
                        editable = list(target = "cell", disable = list(columns = c(0))), ## cannot edit column1
                        filter = list(position = 'top', clear = FALSE),
                        extensions = c('Buttons'),
                        options = list(
                          dom = 'Blfrtip', 
                          style = 'os', 
                          items = 'row',
                          scrollX = TRUE,
                          buttons = c('csv', 'excel', 'pdf', 'print'),
                          searchHighlight = TRUE,
                          search = list(regex = TRUE)
                          #columnDefs = list(list(targets = c(3), searchable = FALSE)) #Disable Searching for Individual Columns
                        )
  )
  
  # print the selected projects
  output$y1 <- renderPrint({
    cat('Projects selected:\n')
    input$x1_rows_selected
  })
  
  
  
  
  
  
  # output dataset table
  output$x2 <- renderDT(
    dataVal(),
    server = FALSE,     ## client-side processing
    
    #selection = 'single',  #selection = 'none',
    selection = list(target = 'row'),   ## Multiple selection: rows
    
    #editable = 'cell', 
    editable = list(target = "cell", disable = list(columns = c(0))), ## cannot edit column1
    
    # search options
    filter = list(position = 'top', clear = FALSE),
    
    extensions = c('Buttons'),
    options = list(
      
      dom = 'Blfrtip',  ##dom = 'PBlfrtip',
      style = 'os', items = 'row',
      scrollX = TRUE,
      #buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
      buttons = c(#'selectAll', 'selectNone',
        'csv', 'excel', 'pdf', 'print'),   #'selectRows', 'selectColumns', 'selectCells',
      
      searchHighlight = TRUE,
      
      search = list(regex = TRUE)
      #columnDefs = list(list(targets = c(1), searchable = FALSE))  
      #Disable Searching for Individual Columns
      
      
      ## ??? no searchPanes
      #columnDefs = list(list(searchPanes = list(show = FALSE), targets = 1:3))
    )         
    
  )  
  

  
  
    # create new project
  #projVal <- reactiveVal(proj)
  
  ## ????
  #output$x0 <- renderDT(projVal(),
  #                      selection = 'none')
  
  
  
  

  
  
### delete data/proj row #####################################
  
  ## These values allow the actions made in the modal to be delayed until the modal is closed
  values = reactiveValues(to_print = "",   ## This is the text that will be displayed
                          modal_closed=F)  ## This prevents the values$to_print output from 
  
  ## Open the modal when button clicked
  observeEvent(input$delete_data, {
    values$modal_closed <- F
    showModal(modalDialog("Are you sure you want to delete this data?
                          If you confirm the deletion, click the Delete button below.
                          If you don't want to delete it, you can click outside the dialog box to cancel.", 
                          title = "Delete Dataset", 
                          easyClose = TRUE, ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_d",label = "Delete"))
    )
  })

  ## This event is triggered by the actionButton inside the modalDialog
  #  It closes the modal, and by setting values$modal_closed <- T
  #  it triggers values$to_print to update.
  observeEvent(input$delete_d,{
    values$modal_closed <- T
    removeModal()
  })  
  
  ## values$to_print is only updated once the modal is closed.
  observe({
    if(values$modal_closed){
      #values$to_print <- paste(input$checkboxes)
      observeEvent(input$delete_d, {
        t = dataVal()
        print(nrow(t))
        if (!is.null(input$x2_rows_selected)) {
          t <- t[-as.numeric(input$x2_rows_selected),]
        }
        dataVal(t)
      })
      
    }
  })

#  observeEvent(input$Delete, {
#    t = dataVal()
#    print(nrow(t))
#    if (!is.null(input$x2_rows_selected)) {
#      t <- t[-as.numeric(input$x2_rows_selected),]
#    }
#    dataVal(t)
#  })
  
  

  values_p = reactiveValues(to_print = "",   ## This is the text that will be displayed
                            modal_closed=F)
  
  observeEvent(input$delete_proj, {
    values_p$modal_closed <- F
    showModal(modalDialog("Are you sure you want to delete this project?
                          If you confirm the deletion, click the Delete button below.
                          If you don't want to delete it, you can click outside the dialog box to cancel.", 
                          title = "Delete Project", 
                          easyClose = TRUE, ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_p",label = "Delete"))
    )
  })
  
  observeEvent(input$delete_p,{
    values_p$modal_closed <- T
    removeModal()
  })  
  
  ## values$to_print is only updated once the modal is closed.
  observe({
    if(values_p$modal_closed){
      #values$to_print <- paste(input$checkboxes)
      observeEvent(input$delete_p, {
        r = projVal()
        print(nrow(r))
        if (!is.null(input$x1_rows_selected)) {
          r <- r[-as.numeric(input$x1_rows_selected),]
        }
        projVal(r)
      })
    }
    })
      
#  observeEvent(input$delete_proj, {
#    r = projVal()
#    print(nrow(r))
#    if (!is.null(input$x1_rows_selected)) {
#      r <- r[-as.numeric(input$x1_rows_selected),]
#    }
#    projVal(r)
#  })
  
  
  
  
## edit a row ????????????????????
  # select a row, press edit and 
  # a popup window comes up with the data for that row that can be edited and saved.
  observeEvent(input$edit_data, {
    ### Error in split.default: first argument must be a vector ????????????????????????????     
    dataVal()
    options=list(
      editable = list(target = "row", disable = list(columns = c(0)))
    )                     
    
    datas <<- editData(dataVal(), input$x2_row_edit, 'x2') ## double <
  })
  
  
  observeEvent(input$edit_proj, {
    ### Error in split.default: first argument must be a vector ????????????????????????????     
    projVal()
    options=list(
      editable = list(target = "row", disable = list(columns = c(0)))
    )                     
    
    proj <<- editData(projVal(), input$x1_row_edit, 'x1') ## double <
  })
  
  
  
  

### SAVE the table into a file, and then load the file ???????????????
  ## Error in as.data.frame.default: cannot coerce class ‘c("reactiveVal", "reactive", "function")’ to a data.frame
  
  observeEvent(input$save_data,{
    write.csv(dataVal(),'df_data.csv',row.names = FALSE)
    
    #df_data<-read.csv('df_data.csv')
    
  })
  
  observeEvent(input$save_proj,{  
    write.csv(projVal(),'df_proj.csv',row.names = FALSE)     ### ERROR in as.data.frame.default: cannot coerce class ‘c("reactiveVal", "reactive", "function")’ to a data.frame
    
    #df_proj<-read.csv('df_proj.csv')
    
  })
  
  ### Error in <Anonymous>: 'data' must be 2-dimensional (e.g. data frame or matrix) ???
  #observeEvent(input$save_data,{
  #  saveRDS(dataVal,"df_data.rds")
  #  df_data<-readRDS("df_data.rds")
  
  ####saveRDS(df,"df.rds")
  ####df<-readRDS ("df.rds")
  #})
  
}