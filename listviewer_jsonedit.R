require(listviewer)

jsonedit(
  list(
    array = c(1,2,3)
    ,boolean = TRUE
    ,null = NULL
    ,number = 123
    ,object = list( a="b", c="d" )
    ,string = "Hello World"
  )
)

require(yaml)
jsonedit(
  yaml.load_file(system.file("htmlwidgets/jsonedit.yaml",package="listviewer"))
)

### experiment with topojson
library(httr)
library(pipeR)
library(listviewer)

# topojson for Afghanistan
url_path = "https://gist.githubusercontent.com/markmarkoh/8856417/raw/6178d18115d9f273656d294a867c3f83b739a951/customAfghanMap.topo.json"

url_path %>>% 
  GET %>>%
  content( as = "text") %>>%
  jsonedit


#### HAVE A GO FROM SHINY
library(shiny)
library(listviewer)

# put some data in environment so it will show up
data(mtcars)

ui <- shinyUI(
  fluidPage(
    jsoneditOutput( "jsed" )
  )
)

server <- function(input,output){
  output$jsed <- renderJsonedit({
    jsonedit(
      as.list( .GlobalEnv )
      ,"change" = htmlwidgets::JS('function(){
                                  console.log( event.currentTarget.parentNode.editor.get() )
  }')
    )
    
})
}

runApp( list( ui = ui, server = server ) )