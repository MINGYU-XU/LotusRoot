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
#testfile_dataset <- read.csv('csv_test2_data.csv') #sep = ','
#testfile_project <- read.csv('csv_test2_proj.csv')
#datas <- testfile_dataset
#proj <- testfile_project

#datas<-readRDS("df_data.rds") ### error
datas <- read.csv('df_data.csv')
proj <- read.csv('df_proj.csv')
user <- read.csv('register.csv')
# admin tables
am_method <- read.table('admin_method.csv',sep = ',',header = T)  

## secure credentials info ----------------------------------
if (interactive()) {
  # define some credentials
  credentials <- data.frame(
    user = c("test", "123"),
    password = c("test", "123"),
    stringsAsFactors = FALSE
  )}



#server---------------------------------------------------------------------
server <- function(input, output,session) {
  
  # check_credentials returns a function to authenticate users
  #res_auth <- secure_server(
  #  check_credentials = check_credentials(credentials)
  #)
  #output$auth_output <- renderPrint({
  #  reactiveValuesToList(res_auth)
  #})
  
  
  # HOME page  -------------  
  
  # links
  github_url <- a("GitHub Repository", href="https://github.com/MINGYU-XU/LotusRoot")
  NCBI_url <- a("NCBI", href="https://www.ncbi.nlm.nih.gov/")
  GEO_url <- a("GEO-NCBI", href="https://www.ncbi.nlm.nih.gov/geo/")
  output$links <- renderUI({
    tagList(github_url,br(),NCBI_url,br(),GEO_url)
    #tagList("GitHub:", github_url)
  })
  
### show login/register dialog box when initiated
  
  # After logging in, you can visit other pages
  
  # define the ui of the login dialog box
  login_dialog <- modalDialog(
    title = 'Welcome to My LIMS!',
    footer = actionButton('useless','  '),
    size = 'l',
    box(width = 5,
        h4('Login'),
        textInput('tab_login.username','Username'),
        passwordInput('tab_login.password','Password'),
        actionButton('tab_login.login','Login'),
        tags$div(class = 'warn-text',textOutput('tab_login.login_msg'))),
    box(width = 7,
        h4('Register'),
        useShinyFeedback(), 
        textInput('registerName','Username', placeholder = "Please enter your full name"),
        passwordInput('registerpw','Password'),
        textInput("email", "Email :",placeholder = "Pleasr enter your email"),
        uiOutput("register_group"),
        actionButton('register_ok','New User Register'),
        verbatimTextOutput("successfully_registered")
        )
  )  
  showModal(login_dialog)
  #register group options
  output$register_group <- renderUI(
    selectInput(inputId = "group", "Which Group/Lab/Research center you belong to?", 
                choices =  am_group[,1]
                )
  )
  
  
  # 登录 validate the login username and password 
  userVal <- reactiveVal(user)
  observeEvent(input$tab_login.login, {
    username <- input$tab_login.username
    password <- input$tab_login.password
    #row.names(user) <- user$Name
    
    if(username %in% userVal()[,'Name']) {
      # real time login
      if(password == userVal() %>% filter(Name==username) %>% pull(Password)){
        # succesfully log in
        removeModal() # remove login dialog
        output$tab_login.welcome_text <- renderText(
          paste0('Welcome,', ' ',username,'!')
          )
          
        shinyjs::show('tab_login.welcome_div') # show welcome message
      } else {
        # password incorrect, show the warning message
        # warning message disappear in 1 sec
        output$tab_login.login_msg <- renderText('Incorrect Password')
        shinyjs::show('tab_login.login_msg')
        shinyjs::delay(2000, hide('tab_login.login_msg')) ##Delay disappear
      }
    } else {
      # username not found, show the warning message
      # warning message disappear in 1 sec
      output$tab_login.login_msg <- renderText('Username Not Found. Please register.')
      shinyjs::show('tab_login.login_msg')
      shinyjs::delay(2000, hide('tab_login.login_msg'))  ##Delay disappear
    }
  })
  

  # new user register ----------------------------------
  
  # check unique user name -----
  observeEvent(input$registerName, {
    if(input$registerName %in% user[,'Name']) {
      showFeedbackDanger(
        inputId = "registerName",
        text = "The user name is already taken."
      )
      shinyjs::hide("register_ok") 
    } else {
      hideFeedback("registerName")
      shinyjs::show("register_ok") 
    }
  })
  # 注册
  observeEvent(input$register_ok,{
    # Tips for successful registration
    output$successfully_registered <- renderPrint({
      cat("Successfully registered!")
      shinyjs::delay(2000, hide('successfully_registered'))
    })
    u <- rbind(data.frame(Name = input$registerName,
                          Password = input$registerpw,
                          Email = input$email,
                          Group.Lab.Center = input$group,
                          #Permissions = input$permissions
                          Permissions = 'General_Staff'
    ),userVal())
    userVal(u)
    
    #Clear text input after submit
    updateTextInput(session, "registerName", value = "")     
    updateTextInput(session, "registerpw", value = "")     
    updateTextInput(session, "email", value = "")   
    updateTextInput(session, "group", value = "")  
    
    fwrite(userVal(),'register.csv',row.names = FALSE)
  })
  
  
### User permissions 权限管理 --------------------------------------
  observeEvent(input$tab_login.login,{
    pm <- userVal() %>% filter(Name==input$tab_login.username) %>% pull(Permissions)
    output$tab_login.permissions_text <- renderText(paste0('Your permissions: ',pm))
    print(pm)    
    #'General_Staff','Data_Administrator','Project_Supervisor ','System_Maintenance'
    if(pm == 'System_Maintenance'){
      shinyjs::show('admin_item')
    } else if(pm == 'General_Staff'){  
      shinyjs::hide("add_proj")
      shinyjs::hide("edit_proj")
      shinyjs::hide("delete_proj")
      shinyjs::hide("add_data")
      shinyjs::hide("edit_data")
      shinyjs::hide("delete_data")
      shinyjs::hide("admin")
    } else if(pm == 'Project_Supervisor'){
      shinyjs::hide("add_data")
      shinyjs::hide("edit_data")
      shinyjs::hide("delete_data")
      shinyjs::hide("admin")
    } else if(pm == 'Data_Administrator'){
      shinyjs::hide("add_proj")
      shinyjs::hide("edit_proj")
      shinyjs::hide("delete_proj")
      shinyjs::hide("admin")
    } 
  })
  

#Proj/Data---------------------------------------------------------------------
options(DT.options = list(pageLength = 10)) ## The initial display is 10 rows
  
### ADD new proj/Data row -----------------------------------------------------
  
  # Project ID generated by the system ------------------------
  autoProjidVal<- reactiveVal(nrow(proj)+1)
  observe({
    output$proj_id <- renderText({  autoProjidVal()  })
  })
  
  # Check unique proj name
  observeEvent(input$projName, {
    proj <- projVal()
    if(input$projName %in% proj[,'Project.Name']) {
      ## feedback
      showFeedbackDanger(
        inputId = "projName",
        text = "The project name is already taken."
      )
      shinyjs::hide("add_proj") 
    } else {
      hideFeedback("projName")
      shinyjs::show("add_proj") 
    }
  })
  
  # proj options
  output$proj_researcher <- renderUI(
    selectizeInput("projResearcher", "Researcher:",
                choices = am_researcher_val()[,1],
                multiple = F,
                options = list(`actions-box` = TRUE)
    )
  )
  observe({
    ## 'Bioinformatician' synchronize changes with 'Researcher'
    output$proj_bioinfo <- renderUI(
      selectizeInput("projBioinformatician", "Bioinformatician:",
                  choices = am_researcher_val()[,1],
                  selected = input$projResearcher,
                  multiple = F,
                  options = list(`actions-box` = TRUE)
                  )
    )
  })
  output$proj_group <- renderUI(
    selectizeInput("projGroup", "Group:",
                choices = am_group_val()[,1],
                multiple = F,
                options = list(`actions-box` = TRUE)
    )
  )
  output$proj_parent <- renderUI(
    selectizeInput("projParent","Parent Project Name(optional):",
                choices = projVal()[,2],
                selected = NULL,
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
    )
              
  )
  
  
  # ADD proj ------------------------
  projVal <- reactiveVal(proj)
  observeEvent(input$add_proj,{
    # change input:'Project.Name' to 'Project.ID', and store 'Project.ID' into the project table
    if(length(input$projParent) == 0 ){input_parentID=""}
    else{
    input_parentID <- projVal() %>% filter(Project.Name==input$projParent) %>% pull(Project.ID)
    }
    
    p <- rbind(data.frame(
      Project.ID = nrow(projVal())+1, #autoProjidVal(),#nrow(proj)+1, 
      Project.Name = input$projName, 
      Parent = input_parentID,   #　store 'Project.ID' into the project table
      Description = input$projDescription, 
      Start.Date = as.character(input$projDate),
      #End.Date = as.character(input$projDate2),
      Path = input$projPath, 
      Sample.Sheet = input$projSampleSheet,
      Researcher = input$projResearcher, 
      Bioinformatician = input$projBioinformatician,
      Group = input$projGroup,
      Report = input$projReport,
      Status = input$projStatus, 
      Data.Repository = input$projdataRepository,
      Code.Repository = input$codeRepository,
      Permissions = input$projPermissions),projVal())                   
    projVal(p)
    

    # Successfully added
    output$new_proj_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('new_proj_added'))
    })
    
    #Clear text input after submit
    updateTextInput(session, "projID", value = "")     
    updateTextInput(session, "projName", value = "")     
    updateTextInput(session, "projParent", value = "")   
    updateTextInput(session, "projDescription", value = "")
    updateTextInput(session, "projDate", value = "")     
    updateTextInput(session, "projDate2", value = "")     
    updateTextInput(session, "projPath", value = "")   
    updateTextInput(session, "projSampleSheet", value = "")
    updateTextInput(session, "projResearcher", value = "")     
    updateTextInput(session, "projBioinformatician", value = "")     
    updateTextInput(session, "projGroup", value = "")   
    updateTextInput(session, "projReport", value = "")
    updateTextInput(session, "projStatus", value = "")     
    updateTextInput(session, "projdataRepository", value = "")     
    updateTextInput(session, "codeRepository", value = "")   
    updateTextInput(session, "projPermissions", value = "")
    
    fwrite(projVal(),'df_proj.csv',row.names = FALSE)
  })
  
  
  
  # Data ID generated by the system ------------------------
  autoDataidVal<- reactiveVal(nrow(datas)+1)
  observe({
    output$data_id <- renderText({  autoDataidVal()}) #nrow(datas)+1
  })
  
  # Check unique dataName
  observeEvent(input$dataName, {
    datas <- dataVal() ###
    if(input$dataName %in% datas[,'Data.Name']) {
      ## feedback
      showFeedbackDanger(
        inputId = "dataName",
        text = "The sample name is already taken."
      )
      shinyjs::hide("add_data") 
    } else {
      hideFeedback("dataName")
      shinyjs::show("add_data") 
    }
  })
  
  # data options
  output$related_project_name <- renderUI(
    selectizeInput('dataprojID', 'Related Project Name(optional):',
                choices = projVal()[,2],
                selected = NULL,
                multiple = F   
    )
  )
  
  output$data_method <- renderUI(
    selectizeInput("method", "Method:",
                choices = am_method_val()[,1],
                multiple = F,
                options = list(`actions-box` = TRUE)
    )
  )
  
  output$data_organism <- renderUI(
    selectizeInput("organism", "Organism:",
                choices = am_organism_val()[,1],
                multiple = F,
                options = list(`actions-box` = TRUE)
    )
  )
  
  output$data_cell <- renderUI(
    selectizeInput("cell", "Tissue/Cell:",
                choices = am_cell_val()[,1],
                multiple = F,
                options = list(`actions-box` = TRUE)
    )
  )
  
  output$data_format <- renderUI(
    selectizeInput("format", "Format:",
                choices = am_format_val()[,1],
                multiple = F,
                options = list(`actions-box` = TRUE)
    )
  )
  
  
  # ADD data ------------------------
  dataVal <- reactiveVal(datas)
  observeEvent(input$add_data,{
    # change input:'Project.Name' to 'Project.ID', and store 'Project.ID' into the project table
    input_related_parentID <- projVal() %>% filter(Project.Name==input$dataprojID) %>% pull(Project.ID)
    
    t <- rbind(data.frame(
      Data.ID = nrow(datas)+1,  # autoDataidVal(),
      Related.ProjectID = input_related_parentID,  #store 'Project.ID' into the table
      #Related.ProjectID = input$dataprojID,
      Data.Name = input$dataName,
      Description = input$dataDescription, 
      Date = as.character(input$dataDate),
      Path = input$dataPath, 
      Data.Repository = input$dataRepository,
      Method = input$method,
      Organism = input$organism,
      Tissue.Cell = input$cell, 
      Genotype = input$genotype,
      Format = input$format,
      #Treatment = input$treatment
      ),dataVal()) 
    dataVal(t)
    
    #Successfully added
    output$new_data_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('new_data_added'))
    })
    
    #Clear text input after submit
    updateTextInput(session, "dataprojID", value = "")     
    updateTextInput(session, "dataName", value = "")   
    updateTextInput(session, "dataDescription", value = "")
    updateTextInput(session, "dataDate", value = "")     
    updateTextInput(session, "dataPath", value = "")     
    updateTextInput(session, "dataRepository", value = "")
    updateTextInput(session, "method", value = "")     
    updateTextInput(session, "organism", value = "")     
    updateTextInput(session, "cell", value = "")   
    updateTextInput(session, "genotype", value = "")
    updateTextInput(session, "format", value = "")     
    #updateTextInput(session, "treatment", value = "")     
    
    fwrite(dataVal(),'df_data.csv',row.names = FALSE)
  })
  
 

### DELETE data/proj row -----------------------------------------------------------------
  
# delete data row -------------------------
  # These values allow the actions made in the modal to be delayed until the modal is closed
  values = reactiveValues(modal_closed=F )#to_print = "", 
  
  # Open the modal when delete button clicked
  observeEvent(input$delete_data, {
    values$modal_closed <- F
    showModal(modalDialog(h4('Are you sure you want to delete this data?'),
                          h5('If you confirm the deletion, click the Delete button below.'),
                          h5('If you do not want to delete it, you can click outside the dialog box to cancel.'), 
                          title = "Delete Dataset", 
                          easyClose = TRUE, ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_d",label = "Delete"))
    )
  })
  
  # This event is triggered by the actionButton inside the modalDialog
  # It closes the modal, and by setting values$modal_closed <- T
  observeEvent(input$delete_d,{
    values$modal_closed <- T
    removeModal()
  })  
  
  # only updated once the modal is closed
  observe({
    if(values$modal_closed){
      # values$to_print <- paste(input$checkboxes)
      observeEvent(input$delete_d, {
        d = dataVal()
        #print(nrow(d))
        if (!is.null(input$x2_rows_selected)) {
          d <- d[-as.numeric(input$x2_rows_selected),]
        }
        dataVal(d)
        fwrite(dataVal(),'df_data.csv',row.names = FALSE)
        
      })
    }
    
  })
  
  
# delete proj row ----------------------------------------
  values_p = reactiveValues(modal_closed=F)
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
  observe({
    if(values_p$modal_closed){
      observeEvent(input$delete_p, {
        r = projVal()
        #print(nrow(r))
        if (!is.null(input$x1_rows_selected)) {
          r <- r[-as.numeric(input$x1_rows_selected),]
        }
        projVal(r)
        fwrite(projVal(),'df_proj.csv',row.names = FALSE)
      })
    }
  })

  
  
  
# EDIT row -----------------------------------------------------------
  
  # data row ----------
  # select a row, press edit and 
  # a popup window comes up with the data for that row that can be edited and saved.
  
  #observeEvent(input$edit_data, {
  ### Error in split.default: first argument must be a vector ???????     
  #  dataVal()
  #  options=list(
  #    editable = list(target = "row", disable = list(columns = c(0)))
  #  )                     
  
  #  datas <<- editData(dataVal(), input$x2_row_edit, 'x2') ## double <
  #})
  
  
  edit_value_d = reactiveValues(modal_closed=F)
  
  
  observeEvent(input$edit_data, {
    edit_value_d$modal_closed <- F
    showModal(modalDialog(
      title = "Edit Dataset", 
      
      DT::renderDataTable({
        ed <- data[input$x2_rows_selected,]
        
        DT::datatable(ed, escape = FALSE,
                      editable = list(target = "cell", disable = list(columns = c(0,1,5))), 
                      # cannot edit project id, saart date
                      extensions = "FixedColumns",
                      options = list(SortClasses = TRUE,
                                     scrollX = TRUE,
                                     fixedColumns = list(leftColumns = 1),
                                     pageLength = 10)
        )
      }),
      
      
      easyClose = TRUE, ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
      footer = actionButton("save_d",label = "Save")
    )
    )
  })
  
  observeEvent(input$save_d,{
    edit_value_d$modal_closed <- T
    removeModal()
  })  
  
  
  observe({
    if(edit_value_d$modal_closed){
      data <<- editData(dataVal(), input$x2_row_edit, 'x2')
    }
  })  
  
  
  
  
  
  #  observeEvent(input$edit_proj, {
  #    ### Error in split.default: first argument must be a vector ????????????????????????????     
  #    projVal()
  #    options=list(
  #      editable = list(target = "row", disable = list(columns = c(0)))
  #    )                     
  #    
  #    proj <<- editData(projVal(), input$x1_row_edit, 'x1') ## double <
  #  })
  
  
  
  
  
  
  # edit proj row -----------------------
  
  #function：让用户选择一行，点击“编辑”按钮，然后在另一个框中编辑并保存更改。
  # select one proj --> click 'edit' --> pop-up window --> edit and save
  
  # actionButton: edit_proj
  
  edit_value_p = reactiveValues(modal_closed=F)
  
  observeEvent(input$edit_proj, {
    edit_value_p$modal_closed <- F
    showModal(modalDialog(
      title = "Edit Project", 
      renderDataTable({
        ep <- proj[input$x1_rows_selected,]
        #print(ep)
        datatable(ep, escape = FALSE,
                  editable = list(target = "cell", disable = list(columns = c(0,1,5))), 
                  # cannot edit project id, saart date
                  extensions = "FixedColumns",
                  options = list(SortClasses = TRUE,
                                 scrollX = TRUE,
                                 fixedColumns = list(leftColumns = 1),
                                 pageLength = 10)
        )
        
        
      }) ,
      easyClose = TRUE, ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
      footer = actionButton("save_p",label = "Save")
      
      
    )
    )
  })
  
  observeEvent(input$save_p,{
    edit_value_p$modal_closed <- T
    removeModal()
  })
  
  #projVal <- reactiveVal(proj)
  observe({
    if(edit_value_p$modal_closed){
      proj <<- editData(projVal(), input$x1_row_edit, 'x1')
    }
    projVal()
  })
  
  ## ??? pop-up window cannot save
  ## Warning: Error in split.default: first argument must be a vector 
  
  
  
  
  #  observeEvent(input$save_p, {
  #    projVal()
  #    options=list(editable = list(target = "row", disable = list(columns = c(0))))                     
  
  #    proj <<- editData(projVal(), input$x1_row_edit, 'x1') ## double <
  #  })
  

  
  
  
  
  ## Clear selected rows-------------------------------------------------
  # clear selected proj rows -----
  x1_proxy <- DT::dataTableProxy("x1")
  observeEvent(input$clear_selected_proj,{
    selectRows(x1_proxy, NULL)
    })
  
  # clear selected data rows -----
  x2_proxy <- DT::dataTableProxy("x2")
  observeEvent(input$clear_selected_data,{
    selectRows(x2_proxy, NULL)
  })
  
  

  
  
  

  
  
### Associating two tables ---------------------------------------------------------
# 通过project id关联,预期功能：选中一个proj后，显示该项目中包含的datasets
  
# proj -> data/sub-projs -------------------------------
  ## if no row(in x1) selected, No table
  ## if select one parent proj, display its sub proj 
  ## if select one sub proj, display the datasets included in that proj
  
  # Project.ID
  pids <- reactive({
    if(length(input$x1_rows_selected)==0) {return(NULL)}   ## No output
    else { pids <- projVal()[input$x1_rows_selected,"Project.ID"] }
    return(pids)
  })
  
  # all parent IDs
  ptids <- reactive({
    if(length(input$x1_rows_selected)==0) {return(NULL)}   
    else { ptids <- projVal()[,"Parent"] }
    return(ptids)
  })
  
  output$related_datasets <- renderDT({
    pid<-pids()
    ptids <-ptids()
    if(length(input$x1_rows_selected)==0) {return(NULL)}   ## No output
    ## if: parent proj, output datasets related to all parent&sub-projs
    if(pid %in% ptids){
      subp <- projVal() %>% filter(Parent %in% pid)  # get sub projs
      all_id <- rbind(pid,subp[,'Project.ID'])  # get all project.id
    } else { 
      ## if: sub proj, output datasets related to the sub proj
      all_id <- pid   
    }
    ds <- dataVal() %>% filter(Related.ProjectID %in% all_id)   ## Filter the dataset table
    datatable(ds,
              rownames = FALSE,
              selection=list(target = 'row'),#"single", 
              extensions = "FixedColumns",
              options = list(SortClasses = TRUE, 
                             scrollX = TRUE,
                             fixedHeader=TRUE,
                             fixedColumns = list(leftColumns = 1))
    )
  })

  output$rp <- renderDT({
    pid<-pids()
    ds <- projVal() %>% filter(Parent  %in% pid)   ## Filter the table
    ds <- rbind(projVal()[input$x1_rows_selected,],ds)
    ds <<- ds[!duplicated(ds),] ## Delete duplicate rows
    datatable(ds, 
              rownames = FALSE,
              selection="single", 
              extensions = "FixedColumns",
              options = list(SortClasses = TRUE, 
                             scrollX = TRUE,
                             fixedColumns = list(leftColumns = 1)
                             )
              ) %>% formatStyle(c('Project.Name','Status'),
                                'Status',                                
                                backgroundColor = styleEqual(
                                  ## different status,different colours
                                  c('On Hold','Ongoing','Complete','Published'), 
                                  c("#C1CDCD", "#FDDBC7", "#92C5DE", "#B4EEB4")
                                )
              )
  })
    
    
output$one_proj_datasets <- renderDT({
    if(length(input$rp_rows_selected)==0) {return(NULL)}   ## No output 
    else { 
      show_id <- ds[input$rp_rows_selected,"Project.ID"]
      show_ds <- dataVal() %>% filter(Related.ProjectID %in% show_id)   ## Filter the dataset table
      datatable(show_ds,
                rownames = FALSE,
                selection=list(target = 'row'),#"single", 
                extensions = "FixedColumns",
                options = list(SortClasses = TRUE, 
                               scrollX = TRUE,
                               fixedHeader=TRUE,
                               fixedColumns = list(leftColumns = 1))
      )
    }
      
})
    

  
  
  # related proj -> related dataset
  show_ids <- reactive({
    if(length(input$rp_rows_selected)==0) {return(NULL)}   ## No output
    else { show_ids <- projVal()[input$rp_rows_selected,"Project.ID"] }
    return(show_ids)
  })
  
  #observeEvent(input$show_data,{
  #output$one_proj_datasets <- renderDT({
  #  if(length(input$rp_rows_selected)==0) {return(NULL)}   ## No output
  #  else { show_id <- projVal()[input$rp_rows_selected,"Project.ID"]}
    
  #  show_ds <- dataVal() %>% filter(Related.ProjectID %in% show_id)   ## Filter the dataset table
  #  datatable(show_ds,
  #            rownames = FALSE,
  #            selection=list(target = 'row'),#"single", 
  #            extensions = "FixedColumns",
  #            options = list(SortClasses = TRUE, 
  #                           scrollX = TRUE,
  #                           fixedHeader=TRUE,
  #                           fixedColumns = list(leftColumns = 1))
  #  )
    
  
     
    
    
 

  
# data -> proj  ----------------------------
  ## if no row(in x1) selected, No table
  ## if select one row, display the projs include the data
  ## return：'pids2' is the Related.ProjectID
  pids2<-reactive({
    if(length(input$x2_rows_selected)==0) {return(NULL)}   ## No output
    else { pids2 <- dataVal()[input$x2_rows_selected,"Related.ProjectID"] }
    return(pids2)
  })
  
  output$related_proj <- renderDT({
    pid2<-pids2()
    ds2 <- projVal() %>% filter(Project.ID  %in% pid2)   ## Filter the table
    #rp2 <- 
    datatable(ds2,
              rownames = FALSE, 
              selection="single", 
              extensions = "FixedColumns",
              options = list(SortClasses = TRUE, scrollX = TRUE,fixedColumns = list(leftColumns = 1))
    ) %>% formatStyle(c('Project.Name','Status'),
                      'Status',                                
                      backgroundColor = styleEqual(
                        ## different status,different colours
                        c('On Hold','Ongoing','Complete','Published'), 
                        c("#C1CDCD", "#FDDBC7", "#92C5DE", "#B4EEB4")
                      )
    )
  })
  
  # related data -> go to project page -----
  observeEvent(input$go_to_proj,{
    updateTabItems(session,
                   inputId = "myproject",
                   selected = "current_project")
    })
  

  
  
### output tables --------------------------------------------------------------
  
# output proj table -----
  projVal <- reactiveVal(proj)
  output$x1 <- renderDT({
    server = FALSE    ## client-side processing
    datatable(
    projVal(),
    rownames = FALSE,
     
    selection = list(target = 'row'),   ## Multiple selection: rows
    #editable = list(target = "cell", disable = list(columns = c(0))), ## cannot edit column1
    filter = list(position = 'top', clear = FALSE),
    extensions = c('Buttons','FixedColumns','Select', 'SearchPanes'),
    options = list(
      dom = 'PBlfrtip', 
      style = 'os', 
      items = 'row',
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 1),
      buttons = c('csv', 'excel', 'pdf'),
      searchHighlight = TRUE,
      search = list(regex = TRUE),
      #columnDefs = list(list(targets = c(3), searchable = FALSE)) 
      #Disable Searching for Individual Columns
      columnDefs = list(list(searchPanes = list(show = FALSE), targets = 1:2)) ## ???no searchPanes
    ) 
                        
    ) %>% formatStyle(c('Project.Name','Status'),
                      'Status',                                
                       backgroundColor = styleEqual(
                         ## different status,different colours
                         c('On Hold','Ongoing','Complete','Published'), 
                         c("#C1CDCD", "#FDDBC7", "#92C5DE", "#B4EEB4")
                       )
    )
                          
  })
  
  ### print the selected projects
  #output$y1 <- renderPrint({
  #  cat('Projects selected:\n')
  #  input$x1_rows_selected
  #})
  
  
# output dataset table -----
  output$x2 <- renderDT(
    dataVal(),
    rownames = FALSE,
    server = FALSE,     ## client-side processing
    selection = list(target = 'row'),   ## Multiple selection: rows  #selection = 'single',  #selection = 'none',
    #editable = 'cell', 
    #editable = list(target = "cell", disable = list(columns = c(0))), ## cannot edit column1
    filter = list(position = 'top', clear = FALSE),
    extensions = c('Buttons','FixedColumns','Select', 'SearchPanes'),
    options = list(
      dom = 'PBlfrtip',  ##dom = 'PBlfrtip',
      style = 'os', 
      items = 'row',
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 1),
      buttons = c('csv', 'excel', 'pdf'),   #'selectRows', 'selectColumns', 'selectCells',#'selectAll', 'selectNone',
      searchHighlight = TRUE,
      search = list(regex = TRUE),
      #columnDefs = list(list(targets = c(1), searchable = FALSE))   #Disable Searching for Individual Columns
      columnDefs = list(list(searchPanes = list(show = FALSE), targets = 1:3)) ##searchPanes
    )         
    
  )  
  

  


  
  
  
  
  
  
  
##Administrator --------------------------------------------------------------
  
# admin_user ------
  
  # ADD user info ------
  userVal <- reactiveVal(user)
  observeEvent(input$userName, {
    # check unique user name
    if(input$userName %in% user[,'Name']) {
      showFeedbackDanger(
        inputId = "userName",
        text = "The user name is already taken."
      )
      shinyjs::hide("add_user") 
    } else {
      hideFeedback("userName")
      shinyjs::show("add_user") 
    }
  })
  
  # admin user group options
  output$admin_user_group <- renderUI(
    column(4, selectInput("userGroup", "Group", choices = am_group[,1]
                          )
           )
  )
  
  observeEvent(input$add_user,{
    u <- rbind(
      data.frame(
        Name = input$userName,  
        Password = input$userPW,
        Email = input$userEmail,  
        Group.Lab.Center = input$userGroup, 
        Permissions = input$userPermissions
        ),userVal()) 
      
    userVal(u)
    
    output$user_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('user_successfully_added'))
    })
    #Clear text input after submit
    updateTextInput(session, "userName", value = "")     
    updateTextInput(session, "userPW", value = "")   
    updateTextInput(session, "userEmail", value = "")
    updateTextInput(session, "userGroup", value = "")     
    updateTextInput(session, "userPermissions", value = "")     
    # save
    fwrite(userVal(),'register.csv',row.names = FALSE)
  })
  
  ## DELETE user
  values_u = reactiveValues(modal_closed=F)
  observeEvent(input$delete_user, {
    values_u$modal_closed <- F
    showModal(modalDialog("Are you sure you want to delete this user?
                          If you confirm the deletion, click the Delete button below.
                          If you don't want to delete it, you can click outside the dialog box to cancel.", 
                          title = "Delete User", 
                          easyClose = TRUE, ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_u",label = "Delete"))
    )
  })
  observeEvent(input$delete_u,{
    values_u$modal_closed <- T
    removeModal()
  })  
  observe({
    if(values_u$modal_closed){
      observeEvent(input$delete_u, {
        r = userVal()
        if (!is.null(input$admin_user_info_rows_selected)) {
          r <- r[-as.numeric(input$admin_user_info_rows_selected),]
        }
        userVal(r)
        fwrite(userVal(),'register.csv',row.names = FALSE)
        
      })
    }
  })
  
  # output user_info
  output$admin_user_info <- DT::renderDT(
    #userVal(),
    userVal()[,c(1,3,4,5)], ##hide password col
    rownames = FALSE,
    server = FALSE,     ## client-side processing 
    selection = list(target = 'row'),   ## Multiple selection: rows
    editable = list(target = "cell", disable = list(columns = c(0))), ## cannot edit column1
    filter = list(position = 'top', clear = FALSE),
    extensions = c('Buttons'),
    options = list(dom = 'Blfrtip', style = 'os', items = 'row',
                   scrollX = TRUE,
                   buttons = c('csv', 'excel', 'pdf'),
                   searchHighlight = TRUE,search = list(regex = TRUE))
  )
  

  

# admin_proj --------------------------------------------
  
  # 1 researcher/ bioinformatician
  am_researcher <- read.table('admin_researcher.csv',sep = ',',header = T)  #am_researcher <- read.csv('admin_researcher.csv')
  am_researcher_val <- reactiveVal(am_researcher)
  #output$admin_researcher <- admin_table_output(am_researcher_val())
  output$admin_researcher <- renderDT(
    am_researcher_val(),
    server = FALSE, 
    selection = list(target = 'row'),
    editable = list(target = "cell", disable = list(columns = c(0))), 
    filter = list(position = 'top', clear = FALSE),
    options = list(#dom = 'Blfrtip', 
                   style = 'os', 
                   items = 'row',
                   scrollX = TRUE,
                   searchHighlight = TRUE)
  )
    
    
  # check unique name
  observeEvent(input$researcherName, {
    
    if(input$researcherName %in% am_researcher[,1]) {
      showFeedbackDanger(
        inputId = "researcherName",
        text = "The researcher name is already taken."
      )
      shinyjs::hide("add_researcher") 
    } else {
      hideFeedback("researcherName")
      shinyjs::show("add_researcher") 
    }
  })
  
  # add 
  observeEvent(input$add_researcher,{
    re <- rbind(data.frame(Researcher = input$researcherName),
                researcherVal() )
    researcherVal(re)
    
    output$researcher_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('researcher_successfully_added'))
    })
    
    updateTextInput(session, "researcherName", value = "")    #Clear text input after submit  
    fwrite(researcherVal(),'admin_researcher.csv',row.names = FALSE) # save
  })
  
  ## DELETE researcher
  values_r = reactiveValues(modal_closed=F)
  observeEvent(input$delete_researcher, {
    values_r$modal_closed <- F
    showModal(modalDialog("Are you sure you want to delete?
                          If you confirm the deletion, click the Delete button below.
                          If you don't want to delete it, you can click outside the dialog box to cancel.", 
                          title = "Delete Researcher", 
                          easyClose = TRUE,  ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_re",label = "Delete"))
    )
  })
  observeEvent(input$delete_re,{
    values_r$modal_closed <- T
    removeModal()
  })  
  observe({
    if(values_r$modal_closed){
      observeEvent(input$delete_re, {
        re <- researcherVal()
        if (!is.null(input$admin_researcher_rows_selected)) {
          re <- re[-as.numeric(input$admin_researcher_rows_selected),1]
        }
        researcherVal(re)
        fwrite(researcherVal(),'admin_researcher.csv',row.names = FALSE)
        
      })
    }
  })
  
  #????error
  #Warning: Error in fwrite: is.list(x) is not TRUE
  #[No stack trace available]
  #chr [1:10] "AA" "WE" "QW" "ER" "FG" "OL" "ABC" "JCW" "SL" "Other"
  #Warning: Error in <Anonymous>: 'data' must be 2-dimensional (e.g. data frame or matrix)
  
  
  # 2 bioinformatician
  #am_bioinformatician <- read.table('admin_bioinformatician.csv',sep = ',',header = T)  #am_bioinformatician <- read.csv('admin_bioinformatician.csv',header = T)
  #am_bioinformatician_val <- reactiveVal(am_bioinformatician)
  #output$admin_bioinformatician <- renderDT(
  #  am_bioinformatician_val(),
  #  server = FALSE, 
  #  selection = list(target = 'row'),
  #  editable = list(target = "cell", disable = list(columns = c(0))), 
  #  filter = list(position = 'top', clear = FALSE),
  #  options = list(#dom = 'Blfrtip', 
  #                 style = 'os', items = 'row',scrollX = TRUE,searchHighlight = TRUE)
  #)
  
  
  #observeEvent(input$bioinformaticianName, {
  #  # check unique user name
  #  if(input$bioinformaticianName %in% am_bioinformatician[,1]) {
  #    showFeedbackDanger(
  #      inputId = "bioinformaticianName",
  #      text = "The bioinformatician name is already taken."
  #    )
  #    shinyjs::hide("add_bioinformatician") 
  #  } else {
  #    hideFeedback("bioinformaticianName")
  #    shinyjs::show("add_bioinformatician") 
  #  }
  #})
  
  #observeEvent(input$add_bioinformatician,{
  #  bio <- rbind(data.frame(Bioinformatician = input$bioinformaticianName),
  #              am_bioinformatician_val() )
  #  am_bioinformatician_val(bio)
    
  #  output$bioinformatician_successfully_added <- renderPrint({
  #    cat("Successfully added!")
  #    shinyjs::delay(2000, hide('bioinformatician_successfully_added'))
  #  })
    
  #  updateTextInput(session, "bioinformaticianName", value = "")    #Clear text input after submit  
  #  fwrite(am_bioinformatician_val(),'admin_bioinformatician.csv',row.names = FALSE) # save
  #})
  
  ## DELETE bioinformatician
  #values_b = reactiveValues(modal_closed=F)
  #observeEvent(input$delete_bioinformatician, {
  #  values_b$modal_closed <- F
  #  showModal(modalDialog("Are you sure you want to delete?
  #                        If you confirm the deletion, click the Delete button below.
  #                        If you don't want to delete it, you can click outside the dialog box to cancel.", 
  #                        title = "Delete Bioinformatician", 
  #                        easyClose = TRUE,  ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
  #                        footer = actionButton("delete_bio",label = "Delete"))
  #  )
  #})
  #observeEvent(input$delete_bio,{
  #  values_b$modal_closed <- T
  #  removeModal()
  #})  
  #observe({
  #  if(values_b$modal_closed){
  #    observeEvent(input$delete_bio, {
  #      b <- am_bioinformatician_val()
  #      if (!is.null(input$admin_bioinformatician_rows_selected)) {
  #        b <- b[-as.numeric(input$admin_bioinformatician_rows_selected),]
  #      }
  #      am_bioinformatician_val(b)
  #      fwrite(am_bioinformatician_val(),'admin_bioinformatician.csv',row.names = FALSE)
  #      
  #    })
  #  }
  #})
  
  
  # 3 group
  am_group <- read.table('admin_group.csv',sep = ',',header = T)   #am_group <- read.csv('admin_group.csv', header = T)
  am_group_val <- reactiveVal(am_group)
  output$admin_group <- renderDT(
    am_group_val(),
    server = FALSE, 
    selection = list(target = 'row'),
    editable = list(target = "cell", disable = list(columns = c(0))), 
    filter = list(position = 'top', clear = FALSE),
    options = list(#dom = 'Blfrtip', 
                   style = 'os', 
                   items = 'row',
                   scrollX = TRUE,
                   searchHighlight = TRUE)
  )
  
  observeEvent(input$groupName, {
    # check unique user name
    if(input$groupName %in% am_group[,1]) {
      showFeedbackDanger(
        inputId = "groupName",
        text = "The group name is already taken."
      )
      shinyjs::hide("add_group") 
    } else {
      hideFeedback("groupName")
      shinyjs::show("add_group") 
    }
  })
  
  observeEvent(input$add_group,{
    g <- rbind(data.frame(Group = input$groupName),
                 am_group_val() )
    am_group_val(g)
    
    output$group_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('group_successfully_added'))
    })
    
    updateTextInput(session, "groupName", value = "")    #Clear text input after submit  
    fwrite(am_group_val(),'admin_group.csv',row.names = FALSE) # save
  })
  
  ## DELETE researcher
  values_g = reactiveValues(modal_closed=F)
  observeEvent(input$delete_group, {
    values_g$modal_closed <- F
    showModal(modalDialog("Are you sure you want to delete?
                          If you confirm the deletion, click the Delete button below.
                          If you don't want to delete it, you can click outside the dialog box to cancel.", 
                          title = "Delete Group", 
                          easyClose = TRUE,  ##If TRUE, the modal dialog can be dismissed by clicking outside the dialog box
                          footer = actionButton("delete_g",label = "Delete"))
    )
  })
  observeEvent(input$delete_g,{
    values_g$modal_closed <- T
    removeModal()
  })  
  observe({
    if(values_g$modal_closed){
      observeEvent(input$delete_g, {
        g <- am_group_val()
        if (!is.null(input$admin_group_rows_selected)) {
          g <- g[-as.numeric(input$admin_group_rows_selected),]
        }
        am_group_val(g)
        fwrite(am_group_val(),'admin_group.csv',row.names = FALSE)
        
      })
    }
  })
 
  
  
  # admin_dataset --------------------------------------------
  ## 1
  #am_method <- read.table('admin_method.csv',sep = ',',header = T)
  am_method_val <- reactiveVal(am_method)
  output$admin_method <- renderDT(
    am_method_val(),
    server = FALSE, 
    selection = list(target = 'row'),
    editable = list(target = "cell", disable = list(columns = c(0))), 
    filter = list(position = 'top', clear = FALSE),
    options = list(#dom = 'Blfrtip', 
                   style = 'os', 
                   items = 'row',
                   scrollX = TRUE,
                   searchHighlight = TRUE)
  )
  
  observeEvent(input$methodName, {
    # check unique name
    if(input$methodName %in% am_method[,1]) {
      showFeedbackDanger(
        inputId = "methodName",
        text = "The method name is already taken."
      )
      shinyjs::hide("add_method") 
    } else {
      hideFeedback("methodName")
      shinyjs::show("add_method") 
    }
  })
  
  observeEvent(input$add_method,{
    m <- rbind(data.frame(Method = input$methodName),
               am_method_val() )
    am_method_val(m)
    
    output$method_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('method_successfully_added'))
    })
    
    updateTextInput(session, "methodName", value = "")    #Clear text input after submit  
    fwrite(am_method_val(),'admin_method.csv',row.names = FALSE) # save
  })
  
  
  
  
  ## 2
  am_organism <- read.table('admin_organism.csv',sep = ',',header = T)
  am_organism_val <- reactiveVal(am_organism)
  output$admin_organism <- renderDT(
    am_organism_val(),
    server = FALSE, 
    selection = list(target = 'row'),
    editable = list(target = "cell", disable = list(columns = c(0))), 
    filter = list(position = 'top', clear = FALSE),
    options = list(#dom = 'Blfrtip', 
                   style = 'os', 
                   items = 'row',
                   scrollX = TRUE,
                   searchHighlight = TRUE)
  )
  
  observeEvent(input$organismName, {
    # check unique name
    if(input$organismName %in% am_organism[,1]) {
      showFeedbackDanger(
        inputId = "organismName",
        text = "The organism name is already taken."
      )
      shinyjs::hide("add_organism") 
    } else {
      hideFeedback("organismName")
      shinyjs::show("add_organism") 
    }
  })
  
  observeEvent(input$add_organism,{
    o <- rbind(data.frame(Organism = input$organismName),
               am_organism_val() )
    am_organism_val(o)
    
    output$organism_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('organism_successfully_added'))
    })
    
    updateTextInput(session, "organismName", value = "")    #Clear text input after submit  
    fwrite(am_organism_val(),'admin_organism.csv',row.names = FALSE) # save
  })
  
  
  
  ## 3
  am_cell <- read.table('admin_cell.csv',sep = ',',header = T)
  am_cell_val <- reactiveVal(am_cell)
  output$admin_cell <- renderDT(
    am_cell_val(),
    server = FALSE, 
    selection = list(target = 'row'),
    editable = list(target = "cell", disable = list(columns = c(0))), 
    filter = list(position = 'top', clear = FALSE),
    options = list(dom = 'Blfrtip', 
                   style = 'os', 
                   items = 'row',
                   scrollX = TRUE,
                   searchHighlight = TRUE)
  )
  
  observeEvent(input$cellName, {
    # check unique name
    if(input$cellName %in% am_cell[,1]) {
      showFeedbackDanger(
        inputId = "cellName",
        text = "The cell name is already taken."
      )
      shinyjs::hide("add_cell") 
    } else {
      hideFeedback("cellName")
      shinyjs::show("add_cell") 
    }
  })
  
  observeEvent(input$add_cell,{
    c <- rbind(data.frame(Tissue.Cell = input$cellName),
               am_cell_val() )
    am_cell_val(c)
    
    output$cell_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('cell_successfully_added'))
    })
    
    updateTextInput(session, "cellName", value = "")    #Clear text input after submit  
    fwrite(am_cell_val(),'admin_cell.csv',row.names = FALSE) # save
  })
  
  
  
  
  
  ## 4
  am_genotype <- read.table('admin_genotype.csv',sep = ',',header = T)
  am_genotype_val <- reactiveVal(am_genotype)
  output$admin_genotype <- renderDT(
    am_genotype_val(),
    server = FALSE, 
    selection = list(target = 'row'),
    editable = list(target = "cell", disable = list(columns = c(0))), 
    filter = list(position = 'top', clear = FALSE),
    options = list(#dom = 'Blfrtip', 
                   style = 'os', 
                   items = 'row',
                   scrollX = TRUE,
                   searchHighlight = TRUE)
  )
  
  observeEvent(input$genotypeName, {
    # check unique name
    if(input$genotypeName %in% am_genotype[,1]) {
      showFeedbackDanger(
        inputId = "genotypeName",
        text = "The genotype name is already taken."
      )
      shinyjs::hide("add_genotype") 
    } else {
      hideFeedback("genotypeName")
      shinyjs::show("add_genotype") 
    }
  })
  
  observeEvent(input$add_genotype,{
    g <- rbind(data.frame(Genotype = input$genotypeName),
               am_genotype_val() )
    am_genotype_val(g)
    
    output$genotype_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('genotype_successfully_added'))
    })
    
    updateTextInput(session, "genotypeName", value = "")    #Clear text input after submit  
    fwrite(am_genotype_val(),'admin_genotype.csv',row.names = FALSE) # save
  })
  
  
  
  
  ## 5
  am_format <- read.table('admin_format.csv',sep = ',',header = T)
  am_format_val <- reactiveVal(am_format)
  output$admin_format <- renderDT(
    am_format_val(),
    server = FALSE, 
    selection = list(target = 'row'),
    editable = list(target = "cell", disable = list(columns = c(0))), 
    filter = list(position = 'top', clear = FALSE),
    options = list(#dom = 'Blfrtip', 
                   style = 'os', 
                   items = 'row',
                   scrollX = TRUE,
                   searchHighlight = TRUE)
  )
  
  observeEvent(input$formatName, {
    # check unique name
    if(input$formatName %in% am_format[,1]) {
      showFeedbackDanger(
        inputId = "formatName",
        text = "The format name is already taken."
      )
      shinyjs::hide("add_format") 
    } else {
      hideFeedback("formatName")
      shinyjs::show("add_format") 
    }
  })
  
  observeEvent(input$add_format,{
    f <- rbind(data.frame(Format = input$formatName),
               am_format_val() )
    am_format_val(f)
    
    output$format_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('format_successfully_added'))
    })
    
    updateTextInput(session, "formatName", value = "")    #Clear text input after submit  
    fwrite(am_format_val(),'admin_format.csv',row.names = FALSE) # save
  })
  
  
  
}



### Save tables ----------------------------------------------------
# SAVE the table into a file, and then load the file 
# save as rds???
# function ???
#save_to_csv <- function(btn,Val,csv_file){
#  observeEvent(input$btn,{
#    fwrite(Val,csv_file,row.names = FALSE)
#  })
#}
# save data after add --------------------------------------------------
#observeEvent(input$add_data,{
#  fwrite(dataVal(),'df_data.csv',row.names = FALSE)
#  #df_data<-read.csv('df_data.csv')
#})






##　admin_table_output function ??
#admin_table_output = function(adminVal){
#  DT::renderDT(
#    adminVal,
#    server = FALSE, 
#    selection = list(target = 'row'),
#    editable = list(target = "cell", disable = list(columns = c(0))), 
#    filter = list(position = 'top', clear = FALSE),
#    options = list(#dom = 'Blfrtip', 
#                   style = 'os', 
#                   items = 'row',
#                   scrollX = TRUE,
#                   searchHighlight = TRUE)
#  )
#}
