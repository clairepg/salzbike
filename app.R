
# import libraries
library(shiny)


server <- shinyServer(function(input, output) {
  

})



#generic line initiating the UI
ui <- shinyUI(fluidPage(
  
  #Add a title
  titlePanel("Add Title Here"),
  
  #This creates a layout with a left sidebar and main section
  sidebarLayout(
    
    #beginning of sidebar section
    #usually includes inputs
    sidebarPanel(),
    
    #beginning of main section
    mainPanel()
  )
  
  #Close the UI definition
))


#generic line that launches the app
shinyApp(ui = ui, server = server)