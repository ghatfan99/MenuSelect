 server = function(input, output, session){
    
    donnees = reactive({
      if(is.null(input$firstColOutput) || input$firstColOutput==""){
        df = data
      }else if(is.null(input$secondCol) && !is.null(input$firstColOutput) ){
        query = paste("select * from data where ", input$firstCol , " in ", input$firstColOutput, sep=" ")
        df = sqldf(query)
      }else{
        df= data
      }
      return (df)
    })
    output$da <- renderTable(donnees())
    
    output$firstCol = renderUI({
      selectInput(
        inputId = "firstCol", label = h4("First Axis"),
        choices = c(Choose = '',Choices), multiple = FALSE
      )
    })
    
    output$secondCol = renderUI({
      sel1 = input$firstCol
      if(is.null(sel1) || sel1==""){
        Choices = NULL
      }else{
        Choices = setdiff(Choices, sel1)  
      }
      selectInput(
        inputId = "secondCol", label = h4("Second Axis"),
        choices = c(Choose = '',Choices), multiple = FALSE
      )
    })
    
    output$thirdCol = renderUI({
      sel1 = input$firstCol
      sel2 = input$secondCol
      sel = c(sel1, sel2)
      if(is.null(sel2) || sel2==""){
        Choices = NULL
      }else{
        Choices = setdiff(Choices, sel)  
      }
      selectInput(
        inputId = "thirdCol", label = h4("Third Axis"),
        choices = c(Choose = '',Choices), multiple = FALSE
      )
    })
    # output first axe
    output$firstColOutput = renderUI({
      if(is.null(input$firstCol) || input$firstCol==""){
        selectInput(
          inputId = "firstColOutput", label = "", choices = NULL, multiple = TRUE, selectize = FALSE
        )
      }else{
        test = donnees()  
        query = paste("select distinct " , input$firstCol , " from test ", sep="")
        choicesValues = sqldf(query)
        selectInput(
          inputId = "firstColOutput", label = "", choices = as.character(choicesValues[[1]]), multiple = TRUE, selectize = FALSE
        )
      }
    })
    
    # output second axe
    output$secondColOutput = renderUI({
      if(is.null(input$secondCol) || input$secondCol==""){
        selectInput(
          inputId = "secondColOutput", label = "", choices = NULL, multiple = TRUE, selectize = FALSE
        )
      }else{
        test = donnees()  
        query = paste("select distinct " , input$secondCol , " from test ", sep="")
        choicesValues = sqldf(query)
        selectInput(
          inputId = "secondColOutput", label = "", choices = as.character(choicesValues[[1]]), multiple = TRUE, selectize = FALSE
        )
      }
    })
    
    # output third axe
    output$thirdColOutput = renderUI({
      if(is.null(input$thirdCol) || input$thirdCol==""){
        selectInput(
          inputId = "thirdColOutput", label = "", choices = NULL, multiple = TRUE, selectize = FALSE
        )
      }else{
        test = donnees()  
        query = paste("select distinct " , input$thirdCol , " from test ", sep="")
        choicesValues = sqldf(query)
        selectInput(
          inputId = "thirdColOutput", label = "", choices = as.character(choicesValues[[1]]), multiple = TRUE, selectize = FALSE
        )
      }
    })
  }
