library(shiny)
library(ggplot2)

#Define UI for Data Preprocessor
shinyUI(fluidPage(
  #App Title
  titlePanel("Data Preprocessor"),
  
  sidebarLayout(
    sidebarPanel(
      #Data Upload Segment
      #Enter full url into text box and check header and separation values
      #Once this has been accomplished, click "Upload" to load data and show it in interactive table to the right
      h3("Upload Data"),
      helpText("Data Preprocessor is a Shiny applet designed to simplify the data cleaning process by allowing for easy visualization while making important pre-processing choise."),
      helpText("Step 1. Download a dataset to work with and select the necessary parameters. The 'mtcars' dataset has been preloaded as an example"),
      textInput("url","Dataset URL:","http://vincentarelbundock.github.io/Rdatasets/csv/datasets/mtcars.csv"),
      checkboxInput("header",label="Header?",value=TRUE),
      textInput("space","Separator:",","),
      submitButton("Upload"),
      helpText("Tip: If you make a mistake, you can restart the data cleaning process by clicking 'Upload' again"),
      br(),
      h3("Preprocessing Options"),
      helpText("Step 2. Having loaded your dataset, examine the table to the right and select useful pre-processing options below."),
      helpText("Note: These options are somewhat reduced due to time constraints. In the future, this framework will be expanded to focus primarily on this step."),
      numericInput("skip",label="Initial Rows to Skip",value = 0),
      checkboxInput("nas",label="Remove empty/NA values",value = FALSE),
      #textInput("sub",label="Manually Subset Data",value = "c('val1','val2','val3')"),
      br(),
      h3("Download Your Data"),
      helpText("Step 3.Once preprocessing is complete, you can download you processed data here by clicking the 'download' button."),
      textInput("fileName","File Name:","newData"),
      downloadButton("downloadData","Download")
      ),
    mainPanel(
      #The first tab contains the original, uploaded dataset
      #The second tab contains the new datset generated once pre-processing and data cleaning is complete.
      tabsetPanel(
       id = 'dataset',
       tabPanel("Original Dataset",dataTableOutput('table')),
       tabPanel("Processed Dataset",dataTableOutput('table2'))
       )
    ) 
  )
))