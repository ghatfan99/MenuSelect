rm(list = ls())
if (!require("shiny") && !require("sqldf")) {
  install.packages(c("shiny", "sqldf"), dependencies = TRUE)
}

library(shiny)
library(sqldf)

reponse =c(rep("yes",5), rep("no", 5), rep("yes", 5), rep("na", 3), rep("yes", 2))
ville =c(rep("Paris",10), rep("Londre", 5), rep("Rome", 5))
score = c(rep(5,7), rep(7, 3), rep(10, 5), rep(20, 5))
data = data.frame(ville, reponse, score)

Choices = colnames(data)

  ui = shinyUI(fluidPage(
    title = 'test Menu',
    div(
      class = "row-fluid", style = "height=350px; background-color:lightgray",
      column(width = 3,
             uiOutput("firstCol"),
             uiOutput("firstColOutput")),
      column(width = 3,
             uiOutput("secondCol"),
             uiOutput("secondColOutput")),
      column(width = 3,
             uiOutput("thirdCol"),
             uiOutput("thirdColOutput"))
    ),
    div(class="row-fluid",
        column(width=12,
               tableOutput("da")
               )
        )
  ))
