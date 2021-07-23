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

proj <- read.csv('df_proj.csv')

# register (user information table)
user <- read.csv('register.csv')


## secure credentials info ----------------------------------
if (interactive()) {
  
  # define some credentials
  credentials <- data.frame(
    user = c("test", "123", "z"),
    password = c("test", "123", "z"),
    stringsAsFactors = FALSE
  )}



#---------------------------------------------------------------------
server <- function(input, output) {
  
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
        textInput('tab_login.username','Username:zz'),
        passwordInput('tab_login.password','Password:zz'),
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
  

  # new user register
  
  # check unique user name
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
      cat("Hi",input$registerName,"!","Successfully registered!")
    })
    u <- rbind(data.frame(Name = input$registerName,
                          Password = input$registerpw,
                          Email = input$email,
                          Group.Lab.Center = input$group,
                          #Permissions = input$permissions
                          Permissions = 'General_Staff'
    ),userVal())    
    
    userVal(u)
  })
  
  # save user information to 'register.csv'
  observeEvent(input$register_ok,{
    write.csv(userVal(),'register.csv',row.names = FALSE)
  })
  
  
  
  
### User permissions --------------------------------------
  observeEvent(input$tab_login.login,{
    #row.names(user) <- user$Name
    #pm <- userVal()[input$tab_login.username,'Permissions']
    pm <- userVal() %>% filter(Name==input$tab_login.username) %>% pull(Permissions)
    output$tab_login.permissions_text <- renderText(paste0('Your permissions: ',pm))
    print(pm)    #'General_Staff','Data_Administrator','Project_Supervisor ','System_Maintenance'
    if(pm == 'System_Maintenance'){
      print("can do all things")
    } else if(pm == 'General_Staff'){  
      shinyjs::hide("add_proj")
      shinyjs::hide("edit_proj")
      shinyjs::hide("delete_proj")
      shinyjs::hide("add_data")
      shinyjs::hide("edit_data")
      shinyjs::hide("delete_data")
    
    } else if(pm == 'Project_Supervisor'){
      shinyjs::hide("add_data")
      shinyjs::hide("edit_data")
      shinyjs::hide("delete_data")
      
    } else if(pm == 'Data_Administrator'){
      shinyjs::hide("add_proj")
      shinyjs::hide("edit_proj")
      shinyjs::hide("delete_proj")
    } 
    
  })
  

#---------------------------------------------------------------------

  
  options(DT.options = list(pageLength = 10)) ## The initial display is 10 rows
  
### ADD new proj/Datarow -----------------------------------
  
  # check unique projID
  observeEvent(input$projID, {
    proj <- projVal() ###
    if(input$projID %in% proj[,'Project.ID']) {
      ## feedback
      showFeedbackDanger(
        inputId = "projID",
        text = "The project ID is already taken."
      )
      shinyjs::hide("add_proj")
    } else {
      hideFeedback("projID")
      shinyjs::show("add_proj")
    }
  })
  # check unique proj name
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
  # add proj
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
  
  
  
  
  # check unique data ID
  observeEvent(input$dataID, {
    datas <- dataVal() ###
    if(input$dataID %in% datas[,'Data.ID']) {
      ## feedback
      showFeedbackDanger(
        inputId = "dataID",
        text = "The data ID is already taken."
      )
      shinyjs::hide("add_data") 
    } else {
      hideFeedback("dataID")
      shinyjs::show("add_data") 
    }
  })
  # check unique sampleName
  observeEvent(input$sampleName, {
    datas <- dataVal() ###
    if(input$sampleName %in% datas[,'Sample.Name']) {
      ## feedback
      showFeedbackDanger(
        inputId = "sampleName",
        text = "The sample name is already taken."
      )
      shinyjs::hide("add_data") 
    } else {
      hideFeedback("sampleName")
      shinyjs::show("add_data") 
    }
  })
  # add data
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
  
 
  ### DELETE data/proj row -----------------------------------------------------------------
  
  # delete data row --------
  
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
  

  # delete proj row -------------
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

  
  
  
# EDIT row ---------------------------------------------------------------------
  
  # data row
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
  
  
  
  
  
  
  # edit proj row
  
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
  
  
  
  
  
  
  
  
  
  ### Save tables --------------------------------------------------------------
  # SAVE the table into a file, and then load the file 
  # save as rds???
  
  # save data
  observeEvent(input$add_data,{
    write.csv(dataVal(),'df_data.csv',row.names = FALSE)
    #df_data<-read.csv('df_data.csv')
  })
  # save proj
  observeEvent(input$add_proj,{  
    write.csv(projVal(),'df_proj.csv',row.names = FALSE)     ### ERROR in as.data.frame.default: cannot coerce class ‘c("reactiveVal", "reactive", "function")’ to a data.frame
    #df_proj<-read.csv('df_proj.csv')
  })
  
  
  
  
  
### Associating two tables ---------------------------------------------------------
  # 通过project id关联,预期功能：选中一个proj后，显示该项目中包含的datasets
  
  # proj -> data/sub-projs -------------------------------
  ## if no row(in x1) selected, No table
  ## if select one parent proj, display its sub proj 
  ## if select one sub proj, display the datasets included in that proj
  ## return：'pids' is the information of the whole line
  pids<-reactive({
    if(length(input$x1_rows_selected)==0) {return(NULL)}   ## No output
    else { pids <- projVal()[input$x1_rows_selected,"Project.ID"] }
    return(pids)
  })
  
  output$related_datasets <- DT::renderDT({
    pid<-pids()
    ds <- dataVal() %>% filter(Project.ID  %in% pid)   ## Filter the table
    rd <- DT::datatable( ds, selection="single", options = list(SortClasses = TRUE, scrollX = TRUE))
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
    else { pids2 <- dataVal()[input$x2_rows_selected,"Project.ID"] }
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
  

  


  
  
}

