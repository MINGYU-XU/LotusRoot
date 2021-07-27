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
proj <- read.csv('df_proj.csv')

# register (user information table)
user <- read.csv('register.csv')

# admin


## secure credentials info ----------------------------------
if (interactive()) {
  
  # define some credentials
  credentials <- data.frame(
    user = c("test", "123", "z"),
    password = c("test", "123", "z"),
    stringsAsFactors = FALSE
  )}



#server---------------------------------------------------------------------
server <- function(input, output,session) {
  
  # HOME page -------------
  
  
  # check_credentials returns a function to authenticate users
  #res_auth <- secure_server(
  #  check_credentials = check_credentials(credentials)
  #)
  #output$auth_output <- renderPrint({
  #  reactiveValuesToList(res_auth)
  #})
  
  
### show login/register dialog box when initiated
  
  # After logging in, you can visit other pages
  
  # define the ui of the login dialog box
  login_dialog <- modalDialog(
    title = 'Welcome to My LIMS!',
    footer = actionButton('useless','  '),
    size = 'l',
    box(width = 5,
        h4('Login'),
        textInput('tab_login.username','Username:xmy'),
        passwordInput('tab_login.password','Password:xmy'),
        actionButton('tab_login.login','Login'),
        tags$div(class = 'warn-text',textOutput('tab_login.login_msg'))),
    box(width = 7,
        h4('Register'),
        useShinyFeedback(), 
        textInput('registerName','Username'),
        passwordInput('registerpw','Password'),
        textInput("email", "Email :",placeholder = "your email"),
        selectInput(inputId = "group", "Which Group/Lab/research center you belong to?", 
                    choices = c('Bird','Hill','Wind','Ed LAB','BioSci','CHEM') 
        ),
        actionButton('register_ok','New User Register'),
        verbatimTextOutput("successfully_registered")
        )
  )  
  showModal(login_dialog)
  
  
  # validate the login username and password 
  userVal <- reactiveVal(user)
  
  observeEvent(input$tab_login.login, {
    
    username <- input$tab_login.username
    password <- input$tab_login.password
    #row.names(user) <- user$Name
    
    if(username %in% userVal()[,'Name']) {
      #if(password == user[username,'Password']) {
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
    
    write.csv(userVal(),'register.csv',row.names = FALSE)
  })
  
  
  
  
  
  
### User permissions --------------------------------------
  observeEvent(input$tab_login.login,{
    pm <- userVal() %>% filter(Name==input$tab_login.username) %>% pull(Permissions)
    output$tab_login.permissions_text <- renderText(paste0('Your permissions: ',pm))
    print(pm)    #'General_Staff','Data_Administrator','Project_Supervisor ','System_Maintenance'
    if(pm == 'System_Maintenance'){
      print("can do all things")
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

  
  options(DT.options = list(pageLength = 5)) ## The initial display is 10 rows
  
### ADD new proj/Datarow ---------------------------------------------------------
  # Project ID generated by the system ------------------------
  reactive({
    output$proj_id <- renderText({  nrow(proj)+1  })
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
  # ADD proj ------------------------
  projVal <- reactiveVal(proj)
  observeEvent(input$add_proj,{
    p <- rbind(data.frame(#Project.ID = input$projID,  #paste0("P",nrow(proj)+1),
      Project.ID = nrow(proj)+1, 
      Project.Name = input$projName, 
      Parent = input$projParent,
      Description = input$projDescription, 
      Start.Date = as.character(input$projDate),
      End.Date = as.character(input$projDate2),
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
    
    write.csv(projVal(),'df_proj.csv',row.names = FALSE)
  })
  
  
  
  # Data ID generated by the system ------------------------
  observe({
    output$data_id <- renderText({  nrow(datas)+1   })
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
  # ADD data ------------------------
  dataVal <- reactiveVal(datas)
  observeEvent(input$add_data,{
    t <- rbind(data.frame(#Data.ID = input$dataID, 
      Data.ID = nrow(datas)+1,
      Related.ProjectID = input$dataprojID,
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
      Treatment = input$treatment),dataVal()) 
    dataVal(t)
    
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
    updateTextInput(session, "treatment", value = "")     
    
    write.csv(dataVal(),'df_data.csv',row.names = FALSE)
  })
  
 

  
  
### DELETE data/proj row -----------------------------------------------------------------
  # delete data row -------------------------
  
  ## These values allow the actions made in the modal to be delayed until the modal is closed
  values = reactiveValues(#to_print = "",   
    modal_closed=F)  
  ## Open the modal when delete button clicked
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
        write.csv(dataVal(),'df_data.csv',row.names = FALSE)
        
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
        write.csv(projVal(),'df_proj.csv',row.names = FALSE)
      })
    }
  })

  
  
  
# EDIT row ---------------------------------------------------------------------------------
  
  # data row -------------------------------------
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
                      
                      options = list(SortClasses = TRUE,
                                     scrollX = TRUE,
                                     LengthMenu = c(1,5,10,20,50), 
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
                  
                  options = list(SortClasses = TRUE,
                                 scrollX = TRUE,
                                 LengthMenu = c(1,5,10,20,50), 
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
  
  
  
  
  
### Save tables --------------------------------------------------------------------------
  # SAVE the table into a file, and then load the file 
  # save as rds???
  
  # function ???
  save_to_csv <- function(btn,Val,csv_file){
    observeEvent(input$btn,{
      write.csv(Val,csv_file,row.names = FALSE)
    })
  }
  # save data after add --------------------------------------------------
  #observeEvent(input$add_data,{
  #  write.csv(dataVal(),'df_data.csv',row.names = FALSE)
  #  #df_data<-read.csv('df_data.csv')
  #})
  # save proj after add --------------------------------------------------
  #observeEvent(input$add_proj,{  
  #  write.csv(projVal(),'df_proj.csv',row.names = FALSE)
  #  #df_proj<-read.csv('df_proj.csv')
  #})
  # save user information after add to 'register.csv'----------------------
  #observeEvent(input$register_ok,{
  #  write.csv(userVal(),'register.csv',row.names = FALSE)
  #})
  
  

  
  
  
### Associating two tables ---------------------------------------------------------
  # 通过project id关联,预期功能：选中一个proj后，显示该项目中包含的datasets
  
  # proj -> data/sub-projs -------------------------------
  ## if no row(in x1) selected, No table
  ## if select one parent proj, display its sub proj 
  ## if select one sub proj, display the datasets included in that proj
  
  ## return：'pids' is the information of the selected line
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
  
  output$related_datasets <- DT::renderDT({
    pid<-pids()
    ptids <-ptids()
    
    ## if: parent proj, output datasets related to all parent&sub-projs
    if(pid %in% ptids){
      subp <- projVal() %>% filter(Parent %in% pid)  # get sub projs
      all_id <- rbind(pid,subp[,'Project.ID'])  # get all project.id
    } else { 
      ## if: sub proj, output datasets related to the sub proj
      all_id <- pid   
    }
    ds <- dataVal() %>% filter(Related.ProjectID %in% all_id)   ## Filter the dataset table
    rd <- DT::datatable(ds, selection="single", options = list(SortClasses = TRUE, scrollX = TRUE))
    rd
    
  })

  
  output$parent_sub_proj <- DT::renderDT({
    pid<-pids()
    ds <- projVal() %>% filter(Parent  %in% pid)   ## Filter the table
    ds <- rbind(projVal()[input$x1_rows_selected,],ds)
    ds <- ds[!duplicated(ds),] ## Delete duplicate rows
    ps <- DT::datatable( ds, selection="single", options = list(SortClasses = TRUE, scrollX = TRUE))
    ps
  })
  
  
  # data -> proj  ----------------------------
  ## if no row(in x1) selected, No table
  ## if select one row, display the projs include the data
  ## return：'pids' is the project ID
  pids2<-reactive({
    if(length(input$x2_rows_selected)==0) {return(NULL)}   ## No output
    else { pids2 <- dataVal()[input$x2_rows_selected,"Related.ProjectID"] }
    return(pids2)
  })
  
  output$related_proj <- DT::renderDT({
    pid2<-pids2()
    ds2 <- projVal() %>% filter(Project.ID  %in% pid2)   ## Filter the table
    rp2 <- DT::datatable( ds2, selection="single", options = list(SortClasses = TRUE, scrollX = TRUE))
    rp2
  })
  
  
  
  
  
  
  
### output tables --------------------------------------------------------------
  
  # output proj table
  
  projVal <- reactiveVal(proj)
  output$x1 <- renderDT(projVal(),
                        server = FALSE,     ## client-side processing 
                        selection = list(target = 'row'),   ## Multiple selection: rows
                        #editable = list(target = "cell", disable = list(columns = c(0))), ## cannot edit column1
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
  
  ### print the selected projects
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
    #editable = list(target = "cell", disable = list(columns = c(0))), ## cannot edit column1
    
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
  

  


##Administrator --------------------------------------------------------------
  
  # admin_user --------------------------------------------
  output$admin_user_info <- DT::renderDT(
    userVal(),
    server = FALSE,     ## client-side processing 
    selection = list(target = 'row'),   ## Multiple selection: rows
    editable = list(target = "cell", disable = list(columns = c(0))), ## cannot edit column1
    filter = list(position = 'top', clear = FALSE),
    extensions = c('Buttons'),
    options = list(dom = 'Blfrtip', style = 'os', items = 'row',
                   scrollX = TRUE,buttons = c('csv', 'excel', 'pdf', 'print'),
                   searchHighlight = TRUE,search = list(regex = TRUE))
  )
  
  # ADD user info ------------------------
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
    write.csv(userVal(),'register.csv',row.names = FALSE)
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
        write.csv(userVal(),'register.csv',row.names = FALSE)
        
      })
    }
  })
  
                                      
  
  
  ##　function ------------------------------------------------------------------
  admin_table_output = function(adminVal){
    DT::renderDT(
      adminVal,
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
  }
  
  # admin_proj --------------------------------------------
  am_researcher <- read.csv('admin_researcher.csv')
  am_researcher_val <- reactiveVal(am_researcher)
  output$admin_researcher <- admin_table_output(am_researcher_val())
  #output$admin_researcher <- DT::renderDT(
  #  am_researcher_val(),
  #  server = FALSE, 
  #  selection = list(target = 'row'),
  #  editable = list(target = "cell", disable = list(columns = c(0))), 
  #  filter = list(position = 'top', clear = FALSE),
  #  options = list(dom = 'Blfrtip', 
  #                 style = 'os', 
  #                 items = 'row',
  #                 scrollX = TRUE,
  #                 searchHighlight = TRUE)
  #)
  
  observeEvent(input$researcherName, {
    # check unique user name
    if(input$researcherName %in% am_researcher[,'Researcher']) {
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
  
  observeEvent(input$add_researcher,{
    re <- rbind(data.frame(Researcher = input$researcherName),
                am_researcher_val() )
    am_researcher_val(re)
    
    output$researcher_successfully_added <- renderPrint({
      cat("Successfully added!")
      shinyjs::delay(2000, hide('researcher_successfully_added'))
    })
    
    updateTextInput(session, "researcherName", value = "")    #Clear text input after submit  
    write.csv(am_researcher_val(),'admin_researcher.csv',row.names = FALSE) # save
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
        re <- am_researcher_val()
        if (!is.null(input$admin_researcher_rows_selected)) {
          re <- re[-as.numeric(input$admin_researcher_rows_selected),]
        }
        am_researcher_val(re)
        write.csv(am_researcher_val(),'admin_researcher.csv',row.names = FALSE)
        
      })
    }
  })
  
  
  
  

  am_bioinfomatician <- read.csv('admin_bioinfomatician.csv')
  am_bioinfomatician_val <- reactiveVal(am_bioinfomatician)
  output$admin_bioinformatician <- admin_table_output(am_bioinfomatician_val())
  
  am_group <- read.csv('admin_group.csv')
  am_group_val <- reactiveVal(am_group)
  output$admin_group <- admin_table_output(am_group_val())
  
  #    DT::renderDT(
  #    am_researcher_val(),
  #    server = FALSE, 
  #    selection = list(target = 'cell'),
  #    editable = list(target = "cell", disable = list(columns = c(0))), 
  #    filter = list(position = 'top', clear = FALSE),
  #    options = list(scrollX = TRUE,searchHighlight = TRUE)
  #  )
  
  
  # admin_dataset --------------------------------------------
  am_method <- read.csv('admin_method.csv')
  am_method_val <- reactiveVal(am_method)
  output$admin_method <- admin_table_output(am_method_val())
  
  am_organism <- read.csv('admin_organism.csv')
  am_organism_val <- reactiveVal(am_organism)
  output$admin_organism <- admin_table_output(am_organism_val())
  
  am_cell <- read.csv('admin_cell.csv')
  am_cell_val <- reactiveVal(am_cell)
  output$admin_cell <- admin_table_output(am_cell_val())
  
  am_genotype <- read.csv('admin_genotype.csv')
  am_genotype_val <- reactiveVal(am_genotype)
  output$admin_genotype <- admin_table_output(am_genotype_val())
  
  am_format <- read.csv('admin_format.csv')
  am_format_val <- reactiveVal(am_format)
  output$admin_format <- admin_table_output(am_format_val())
  
}

