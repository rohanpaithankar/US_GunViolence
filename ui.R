library(shinydashboard)
library(leaflet)
library(plotly)
#Following is the code for UI.

body <- dashboardBody(titleWidth = 10,                         #Dashboard Body
  tags$head(tags$style(HTML('.modal.in .modal-dialog{          
                              width:100%;
                              height:100%;
                              margin:0px;
                              }
                            .modal-content{
                              width:100%;
                              height:100%;
                              }
                              '))),                     # HTML for landing page 
  fluidRow(column(width = 8,
    box(width = NULL,background = "black", solidHeader = TRUE,tags$h4("Select State"),       #Main Leaflet 'mymap'
      leafletOutput("mymap",height = 500)
    )
    ),
    column(width = 4,
           box(tags$h3("State :"),background = "black",width = NULL, solidHeader = TRUE,    #Box to display selected state
               tags$h2(textOutput("selected_state"))
           )),
    column(width = 4,
    box(background = "black",width = NULL, solidHeader = TRUE,                      #Box with slider to select Year
        sliderInput("slideryear", h3("Select Year"), 
                     min = 2014, max = 2017, value = 2014, sep = "")
    )
  ),
  column(width = 4,
         box(h3("Select Incident Type"),background = "black",width = NULL, solidHeader = TRUE,                  #Box with radio button to select to show 'All' or 'Mass Shooting' incidents
             radioButtons("radio", "An incident is called a Mass Shooting when more than 4 people are injured 
                          or killed.( United States' Congressional Research Service)",
                          choices = list( "All" = "all","Mass Shootings" = "mass"), inline = TRUE, selected = "all")))
  ,
  column(width = 9,
         box(tags$h4("HandGun and LongGun Sales/Use over the year"),                #Box to show line plot for HandGun and LongGun sales
             width = NULL,background = "aqua" ,solidHeader = TRUE,
             plotlyOutput("sale_guns",height = 500)
         )),
  column(width = 3,
         box(tags$h4("HandGun/LongGun Used"),background = "teal",width = NULL, solidHeader = TRUE,     #PieChart to show share of HandGun and LongGun used in shooting crimes. 
             plotlyOutput("pie_guns",height = 500)
         )),
  column(width = 5,
           box(tags$h4("Words Associated With Incident Locations"),                #WordCloud to show words associated with the locations of the incident
               width = NULL, solidHeader = TRUE,background = "red",
               plotOutput("words",height = 500)
               )),
  column(width = 7,
         box(tags$h4("Localised View of Selected State"),width = NULL, solidHeader = TRUE,background = "navy",   #Leaflet to show localised view for the select state. Uses clustering to show the number of incidents.
             leafletOutput("statemap",height = 500)
         ))
  
) 
)

# Put them together into a dashboardPage
dashboardPage(skin = 'red',
  dashboardHeader(title = "Gun Violence and Gun Sales Analysis", titleWidth = 400),
  dashboardSidebar(disable = TRUE),
  body
)
