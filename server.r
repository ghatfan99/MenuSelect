library(magrittr)

# Returns true if x has a non-empty value.
has <- function(x) {
  !is.null(x) && length(x) > 0 && !identical(x, "")
}

# Given data frame x, the name of a column, and zero or more values,
# filter x to only include rows where the given column value is
# in the set of given values. If either the col or values args
# are null, zero-length, or "", then return the entire data frame.
filterData <- function(x, col, values) {
  if (has(col) && has(values)) {
    x[x[,col] %in% values,]
  } else {
    x
  }
}

# Given data frame x and a column name, return a sorted list of
# unique values. Converts factor values to character.
getChoices <- function(x, col) {
    choices <- if (has(col)) {
        sort(unique(x[,col]))
    } else {
        NULL
    }

    if (is.factor(choices)) {
        choices <- as.character(choices)
    }

    choices
}

server = function(input, output, session){
 
    donnees = reactive({
        data %>%
            filterData(input$firstCol, input$firstColValue) %>%
            filterData(input$secondCol, input$secondColValue) %>%
            filterData(input$thirdCol, input$thirdColValue)
    })
    output$da <- renderTable(donnees())

    # It's OK to set up observers using lapply. (DON'T use `for`
    # or `while` loops to set up observers or reactives, there
    # are scoping issues that are really tricky to spot.)
    lapply(c("first", "second", "third"), function(num) {
        observe({
            updateSelectInput(session, paste0(num, "Col"), choices = Choices)
        })
        observe({
            updateSelectInput(session, paste0(num, "ColValue"),
                choices = getChoices(data, input[[paste0(num, "Col")]]))
        })
    })    
}
