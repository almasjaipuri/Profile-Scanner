{
  library(shinydashboard)
  library(shiny)
  library(dplyr)
  library(ggplot2)
  library(xlsx)
  library(DT)
  library(readxl)
  library(fuzzyjoin)
 
}


#UI body Formation

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody(), skin = "purple"
)


#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Profile Scanner")  
#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    selectInput("location", "Preferred Location:",
                c("All",location)),
    selectInput("experience", "Experience:", c("All", "Greater than 3 years", "Greater than 6 years", "Greater than 10 years"))
    
  )
)

frow1 <- fluidRow(
  valueBoxOutput("skill",width = 12)
  
  
)

frow2 <- fluidRow(
  
 
  
  box(
    width = 6,
    height = 6,
    title = "Candidate's Details"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    ,color = "blue"
    , tableOutput('details')
  ),
  box(
    width = 6,
    height = 6,
    title = "Profile Ranking"
    ,status = "primary"
    ,solidHeader = TRUE
    ,collapsible = TRUE
    , plotOutput('ranking')
  )
 

)

# combine the two fluid rows to make the body
body <- dashboardBody(
    tags$head(tags$style(HTML(".fa {font-size: 48px; }"))),frow1,frow2
)

#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'This is my Page title', header, sidebar, body, skin='red')


# create the server functions for the dashboard  
server <- function(input, output,session) { 
  tab_list <- NULL
 
  datasetInput <- reactive({
    details <- data.frame()

      if(input$location == "All"){
        details <- profile_df_details
      }
      else{
        a <- list()
        for(i in 1:length(location)){
          if(as.character(input$location) == as.character(location[[i]])){
            a <- list.append(a,i)
          }

        }
        details <- profile_df_details[unlist(a),]
      }
    return(details)
  })

 expInput <- reactive({
   
   new_details <- data.frame()
   if(input$experience == "All"){
     new_details <- profile_df_details
   }
   else if(input$experience == "Greater than 3 years"){
     b <- list()
     for (k in 1:length(profile_df_details)) {
       if(!is.na(profile_df_details[k,]$Experience)){
         if(as.integer(str_extract(pattern = "[0-9]+",profile_df_details[k,]$Experience))>=3){
           b <- list.append(b,k)
         }
       }
       
     }
     new_details <- profile_df_details[unlist(b),]
   }
   else if(input$experience == "Greater than 6 years"){
     c <- list()
     for (l in 1:length(profile_df_details)) {
       if(!is.na(profile_df_details[l,]$Experience)){
         if(as.integer(str_extract(pattern = "[0-9]+",profile_df_details[l,]$Experience))>=6){
           c <- list.append(c,l)
       }
       
       }
     }
     new_details <- profile_df_details[unlist(c),]
   }
   else if(input$experience == "Greater than 10 years"){
     d <- list()
     for (m in 1:length(profile_df_details)) {
       if(!is.na(profile_df_details[m,]$Experience)){
         if(as.integer(str_extract(pattern = "[0-9]+",profile_df_details[m,]$Experience))>=10){
           d <- list.append(d,m)
       }
       
       }
     }
     new_details <- profile_df_details[unlist(d),]
   }
   
   return(new_details)
 })
 
 
 ab <- reactive(merge(datasetInput(),expInput(),by = c("Candidate Name","Contact","E-Mail","Experience")))
  
  output$skill <- renderValueBox({ 
    valueBox(
      
      value = tags$p(paste0("Job Description: ",c), style = "font-size: 50%")
      ,subtitle = ("Requirement")
      ,icon = icon("business")
      ,color = "green"
      )  
  })
  
  output$details <- renderTable({
    ab()
    
    
  
    })

  
  output$ranking=renderPlot({
    
    
    plot(rank_plotting(ab()))
  })
 
 }

#run/call the shiny app
shinyApp(ui, server)

