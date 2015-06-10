rm(list=ls())
source("test/R test/functions.R")
if (!require("shiny") && !require("sqldf") && !require("car")) {
  install.packages(c("shiny", "sqldf", "car"), dependencies = TRUE)
}

library(shiny)
library(sqldf)
library(car)

data(Moore)
ContextElements = colnames(Moore)
ContextElements[1] = 'partner_status'
colnames(Moore) = ContextElements
library(shiny)
runApp(list(
  ui = shinyUI(fluidPage(
    
    titlePanel(h3("Data Select", style="background-color:gray")),
    hr(),
    div(class="row-fluid", style="height=350px; background-color:lightgray",
        column(width=3,
               uiOutput("firstAxe"),
               uiOutput("firstAxeOutput")
        ),
        column(width=3,
               uiOutput("secondAxe"),
               uiOutput('type1'),
               uiOutput('type1v'),
               uiOutput("secondAxeOutput")
        ),
        column(width=3,
               uiOutput("thirdAxe"),
               uiOutput('type2'),
               uiOutput("thirdAxeOutput")
        ),
        column(width=3,
               uiOutput("fourthAxe"),
               uiOutput('type3'),
               uiOutput("fourthAxeOutput")
        )
    ),
    hr(),
    hr(),
    div(class="row-fluid",
        column(12,
               verbatimTextOutput("mainQuery"))),
    hr(),
    div(class="row-fluid",
        column(12,
               verbatimTextOutput("query1"))),
    div(class="row-fluid",
        column(12,
               verbatimTextOutput("query2"))),
    div(class="row-fluid",
        column(12,
               verbatimTextOutput("query3"))),
    div(class="row-fluid",
        column(6,h4("Selected Lines"),
               verbatimTextOutput("lines")),
        column(6,h4("conformity"),
               verbatimTextOutput("conformity"))),
    div(class="row-fluid",
        column(12,
               dataTableOutput("moore")))
    
  )),
  server=function(input, output){
    output$moore= renderDataTable(Moore, options = list(pageLength=5, 
                                                        lengthMenu=c(5, 13)),
                                  callback = "function(table){
                                  table.on('click.dt', 'tr', function(){
                                  //table.rows('.selected').removeClass('selected');
                                  $(this).toggleClass('selected');
                                  Shiny.onInputChange('rows',
                                  table.rows('.selected').indexes().toArray());
                                  Shiny.onInputChange('vals',
                                  table.rows('.selected').data()[0][1]);
                                  });
  }"
                                 )
    nbrLines = reactive({
      input$rows
    })
    output$lines <- renderText({
      nbrLines()
    })
    output$conformity <- renderText({
      paste(c(input$vals),
            collapse = ' ')
    })
    
    ######### reactive axis
    ###Axe1
    recAxe1c = reactive({
      if(is.null(input$firstAxe)|| input$firstAxe==""){
        return(NULL)
      }else{
        return(input$firstAxe)
      }
    })
    recAxe1r = reactive({
      if(is.null(input$firstAxeOutput)|| input$firstAxeOutput==""){
        return(NULL)
      }else{
        return(paste(input$firstAxeOutput, collapse = "','"))
      }
    })
    ###Axe2
    recAxe2c = reactive({
      if(is.null(input$secondAxe)|| input$secondAxe==""){
        return(NULL)
      }else{
        return(input$secondAxe)
      }
    })
    recAxe2r = reactive({
      if(is.null(input$secondAxeOutput)|| input$secondAxeOutput==""){
        return(NULL)
      }else{
        return(paste(input$secondAxeOutput, collapse = "','"))
      }
    })
    ###Axe3
    recAxe3c = reactive({
      if(is.null(input$thirdAxe)|| input$thirdAxe==""){
        return(NULL)
      }else{
        return(input$thirdAxe)
      }
    })
    recAxe3r = reactive({
      if(is.null(input$thirdAxeOutput)|| input$thirdAxeAxeOutput==' '){
        return()
      }else{
        return(paste(input$thirdAxeOutput, collapse = "','"))
      }
    })
    ###Axe4
    recAxe4c = reactive({
      if(is.null(input$fourthAxe)|| input$fourthAxe==""){
        return(NULL)
      }else{
        return(input$fourthAxe)
      }
    })
    recAxe4r = reactive({
      if(is.null(input$fourthAxeOutput)|| input$fourthAxeOutput==' '){
        return()
      }else{
        return(paste(input$fourthAxeOutput, collapse = "','"))
      }
    })
    ############################################################################
    ############################################################################
    ### Query String Building
    queryString <- function(axeName, NumAxe) {
      query = "select distinct"
      query = paste(query, axeName, sep=" ")
      query = paste(query, "FROM PROCESS p, LOT l
                    WHERE p.FACILITY=l.FACILITY AND p.PROCESS_PLAN=l.PROCESS_PLAN ", sep=" ")
      if(recAxe1r()!="" && !is.null(recAxe1r())){
        query = paste(query, " AND ", recAxe1c() , " IN ('", recAxe1r() , "') ")
      }
      if(NumAxe>1){
        if(recAxe2r()!="" && !is.null(recAxe2r())){
          query = paste(query, " AND ", recAxe2c() , " IN ('", recAxe2r() , "') ")
        }
      }
      if(NumAxe>2){
        if(recAxe3r()!="" && !is.null(recAxe3r())){
          query = paste(query, " AND ", recAxe3c() , " IN ('", recAxe3r() , "') ")
        }
      }
      if(NumAxe>3){
        if(recAxe4r()!="" && !is.null(recAxe4r())){
          query = paste(query, " AND ", recAxe4c() , " IN ('", recAxe4r() , "') ")
        }
      }
      
      query = paste(query, "ORDER BY ", axeName)
      return(query)
    }
    
    
    
    
    buildQuery= reactive({
      query= queryString(axeName = recAxe2c(), NumAxe = 2)
      return(query)
    })
      
    output$mainQuery = renderPrint(buildQuery())
    ############################################################################
    ############################################################################
    
    
    ###############
    ### first Axis
    output$firstAxe= renderUI({
      selectInput(inputId = "firstAxe", label = h4("First Axis"), 
                  choices=c(Choose='',ContextElements), multiple = FALSE) 
    })    
    # output Query
    output$firstAxeOutput=renderUI({
      if(!is.null(input$firstAxe)){
        if(input$firstAxe==""){
          selectInput(inputId = "firstAxeOutput", label="", choices = NULL, multiple = TRUE, selectize = FALSE)
        }else{
          query = paste('select distinct ', input$firstAxe, ' from Moore order by ', input$firstAxe, sep='')
          choicesValues = sqldf(query)
          selectInput(inputId = "firstAxeOutput", label="", choices = as.character(choicesValues[[1]]), multiple = TRUE, selectize = FALSE)
        } 
      }          
    })
    ###############
    ### second Axis
    precAxeRes1 =reactive(paste(input$firstAxeOutput, collapse = "','"))
    output$type1 = renderPrint(precAxeRes1())
    output$type1v = renderPrint(precAxeRes1()=="")
    output$secondAxe= renderUI({
      firstSel = input$firstAxe
      if(is.null(firstSel)|| firstSel==''){
        ContextElements = NULL
      }else{
        ContextElements = setdiff(ContextElements, firstSel) 
      }      
      selectInput(inputId = "secondAxe", label = h4("Second Axis"), 
                  choices=c(Choose='',ContextElements), multiple = FALSE) 
    })
    # output Query
    output$secondAxeOutput=renderUI({      
      if(!is.null(input$secondAxe)){
        if(input$secondAxe=="" || input$firstAxe==""){
          selectInput(inputId = "secondAxeOutput", label="", choices = NULL, multiple = TRUE, selectize = FALSE)
        }else{
          if(is.null(input$firstAxeOutput)){
            query = paste('select distinct ', input$secondAxe, ' from Moore where ',
                          input$firstAxe, " like ('%') order by ", input$secondAxe , sep='')  
          }else{
            query = paste('select distinct ', input$secondAxe, ' from Moore where ',
                          input$firstAxe, " in ('",precAxeRes1(), "') order by ", input$secondAxe , sep='') 
          }          
          choicesValues = sqldf(query)
          output$query1 = renderPrint(paste("Second Query: ",query))
          selectInput(inputId = "secondAxeOutput", label="", choices = as.character(choicesValues[[1]]), multiple = TRUE, selectize = FALSE)
        } 
      } 
    })
    ###############
    ### third Axis
    precAxeRes2 =reactive(paste(input$secondAxeOutput, collapse = "','"))
    output$type2 = renderPrint(precAxeRes2())
    output$thirdAxe= renderUI({
      precSel = c(input$firstAxe, input$secondAxe)
      if(is.null(input$secondAxe)|| input$secondAxe==''){
        ContextElements = NULL
      }else{
        ContextElements = setdiff(ContextElements, precSel)
      }
      selectInput(inputId = "thirdAxe", label = h4("Third Axis"), 
                  choices=c(Choose='',ContextElements), multiple = FALSE) 
    })
    # output Query
    output$thirdAxeOutput=renderUI({
      if(!is.null(input$thirdAxe)){
        if(input$thirdAxe=="" || input$secondAxe==""){
          selectInput(inputId = "thirdAxeOutput", label="", choices = NULL, multiple = TRUE, selectize = FALSE)
        }else{
          if(is.null(input$firstAxeOutput) && is.null(input$secondAxeOutput)){
            query = paste('select distinct ', input$thirdAxe, ' from Moore where ',
                          input$firstAxe,  " like ('%') and ",
                          input$secondAxe, " like ('%') order by " , input$thirdAxe,
                          sep='')
          }else if(!is.null(input$firstAxeOutput) && is.null(input$secondAxeOutput)){
            query = paste('select distinct ', input$thirdAxe, ' from Moore where ',
                          input$firstAxe,  " in ('",precAxeRes1(), "') and ",
                          input$secondAxe, " like ('%') order by " , input$thirdAxe,
                          sep='')
          }else if(is.null(input$firstAxeOutput) && !is.null(input$secondAxeOutput)){
            query = paste('select distinct ', input$thirdAxe, ' from Moore where ',
                          input$firstAxe,  " like ('%') and ",
                          input$secondAxe, " in ('",precAxeRes2(), "') order by " , input$thirdAxe,
                          sep='')
          }else{
            query = paste('select distinct ', input$thirdAxe, ' from Moore where ',
                          input$firstAxe,  " in ('",precAxeRes1(), "') and ",
                          input$secondAxe, " in ('",precAxeRes2(), "') order by " , input$thirdAxe,
                          sep='')
          }         
          choicesValues = sqldf(query)
          output$query2 = renderPrint(paste("Third Query: ",query))
          selectInput(inputId = "thirdAxeOutput", label="", choices = as.character(choicesValues[[1]]), multiple = TRUE, selectize = FALSE)
        } 
      } 
    })
    ###############
    ### fourth Axis
    precAxeRes3 =reactive(paste(input$thirdAxeOutput, collapse = "','"))
    output$type3 = renderPrint(precAxeRes3())
    output$fourthAxe= renderUI({
      precSel = c(input$firstAxe, input$secondAxe, input$thirdAxe)
      if(is.null(input$thirdAxe)|| input$thirdAxe==''){
        ContextElements = NULL
      }else{
        ContextElements = setdiff(ContextElements, precSel) 
      }      
      selectInput(inputId = "fourthAxe", label = h4("Fourth Axis"), 
                  choices=c(Choose='',ContextElements), multiple = FALSE) 
    })
    # output Query
    output$fourthAxeOutput=renderUI({
      if(!is.null(input$fourthAxe)){
        if(input$fourthAxe=="" || input$thirdAxe==""){
          selectInput(inputId = "fourthAxeOutput", label="", choices = NULL, multiple = TRUE, selectize = FALSE)
        }else{ 
          
          if(is.null(input$firstAxeOutput) && is.null(input$secondAxeOutput) && is.null(input$thirdAxeOutput)){
            query = paste('select distinct ', input$fourthAxe, ' from Moore where ',
                          input$firstAxe,  " like ('%') and ",
                          input$secondAxe, " like ('%') and ",
                          input$thirdAxe, " like ('%') order by " , input$fourthAxe,
                          sep='')
            #             query = paste('select distinct ', input$thirdAxe, ' from Moore order by ',  input$thirdAxe, sep='')
          }else if(!is.null(input$firstAxeOutput) && !is.null(input$secondAxeOutput) && is.null(input$thirdAxeOutput)){
            query = paste('select distinct ', input$fourthAxe, ' from Moore where ',
                          input$firstAxe,  " in ('",precAxeRes1(), "') and ",
                          input$secondAxe, " in ('",precAxeRes2(), "') and ",
                          input$thirdAxe, " like ('%') order by " , input$fourthAxe,
                          sep='')
          }else if(!is.null(input$firstAxeOutput) && is.null(input$secondAxeOutput) && !is.null(input$thirdAxeOutput)){
            query = paste('select distinct ', input$fourthAxe, ' from Moore where ',
                          input$firstAxe,  " in ('",precAxeRes1(), "') and ",
                          input$secondAxe, " like ('%') and ",
                          input$thirdAxe, " in ('",precAxeRes3(), "') order by " , input$fourthAxe,
                          sep='')
          }else if(!is.null(input$firstAxeOutput) && is.null(input$secondAxeOutput) && is.null(input$thirdAxeOutput)){
            query = paste('select distinct ', input$fourthAxe, ' from Moore where ',
                          input$firstAxe,  " in ('",precAxeRes1(), "') and ",
                          input$secondAxe, " like ('%') and ",
                          input$thirdAxe, " like ('%') order by " , input$fourthAxe,
                          sep='')
          }else if(is.null(input$firstAxeOutput) && !is.null(input$secondAxeOutput) && !is.null(input$thirdAxeOutput)){
            query = paste('select distinct ', input$fourthAxe, ' from Moore where ',
                          input$firstAxe,  " like ('%') and ",
                          input$secondAxe, " in ('",precAxeRes2(), "') and ",
                          input$thirdAxe, " in ('",precAxeRes3(), "') order by " , input$fourthAxe,
                          sep='')
          }else if(is.null(input$firstAxeOutput) && !is.null(input$secondAxeOutput) && is.null(input$thirdAxeOutput)){
            query = paste('select distinct ', input$fourthAxe, ' from Moore where ',
                          input$firstAxe,  " like ('%') and ",
                          input$secondAxe, " in ('",precAxeRes2(), "') and ",
                          input$thirdAxe, " like ('%') order by " , input$fourthAxe,
                          sep='')
          }else if(!is.null(input$firstAxeOutput) && !is.null(input$secondAxeOutput) && !is.null(input$thirdAxeOutput)){
            query = paste('select distinct ', input$fourthAxe, ' from Moore where ',
                          input$firstAxe,  " in ('",precAxeRes1(), "') and ",
                          input$secondAxe, " in ('",precAxeRes2(), "') and ",
                          input$thirdAxe, " in ('",precAxeRes3(), "') order by " , input$fourthAxe,
                          sep='')
          }else{
            query = paste('select distinct ', input$fourthAxe, ' from Moore where ',
                          input$firstAxe,  " like ('%') and ",
                          input$secondAxe, " like ('%') and ",
                          input$thirdAxe, " in ('",precAxeRes3(), "') order by " , input$fourthAxe,
                          sep='')            
          } 
          choicesValues = sqldf(query)
          output$query3 = renderPrint(paste("Fourth Query: ",query))
          selectInput(inputId = "fourthAxeOutput", label="", choices = choicesValues[[1]], multiple = TRUE, selectize = FALSE) 
        }
      }
    })
    }
    )
  )
