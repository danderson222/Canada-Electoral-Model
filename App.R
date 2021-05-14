# Set your working directory

# Load packages
library(shiny)
library(shinydashboard)
library(leaflet)
library(leaflet.extras)
library(tidyverse)
library(plotly)
library(scales)
library(sf)
library(dashboardthemes)
# 
# pkgs <- c("shiny", "shinydashboard", "leaflet", "leaflet.extras", 
#           "tidyverse", "plotly", "scales", "sf", "dashboardthemes")
# lapply(pkgs, library, character.only = TRUE)

### Step 1: Read & clean data, make static objects ###

# 1.1 - Read election data

df.riding <- readRDS("data/RidingData-WithMaps.rds")
df.polls <- readRDS("data/PollData-WithMaps.rds")

# 1.2 Set party colours for graphs and maps 
partyColours <- data.frame(Party = c('Liberal', 'Conservative', 'NDP', 'Bloc', 'Green', 'Peoples', 'Independent', 'Other'),
                           colours = c('#D71920', '#1A4782', '#F58220', '#008080', "#3D9B35", "#4E5180", "#808080", "#808080"), 
                           stringsAsFactors = FALSE)

# 1.3 Make CRS to reproject map
epsg2163 <- leafletCRS(crsClass = "L.Proj.CRS",
                       code = "EPSG:2163",
                       resolutions = 2^(16:7),
                       proj4def = "+proj=laea +x_0=0 +y_0=0 +lon_0=-97 +lat_0=62.3 +units=m")


ui <- dashboardPage(
  
  # # Add in css/ text styling
  # tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  
  # Dashboard title
  dashboardHeader(title = "Canadian Elections Dashboard",
                  titleWidth = 350),
  
  # Dashboard Page Colour
  # skin = "red",
  
  # Sidebar Layout
  dashboardSidebar(width = 350,
                   sidebarMenu(menuItem("Country-Wide Analysis", tabName = "canada", icon = icon("canadian-maple-leaf")),
                               menuItem("Provincial Analysis", tabName = "province", icon = icon('poll')),
                               menuItem("Riding Analysis", tabName = "riding", icon = icon('person-booth')),
                               menuItem("Poll Analysis", tabName = "poll", icon = icon("vote-yea"))),
                   tagList(HTML(paste("<br/>
                                      This app was built by Dylan Anderson. For any questions feel free 
                                      to email dylansjanderson@gmail.com
                                      with your enquiries. Please note this application is
                                      under constant construction and evolution, as more
                                      features, elections and user functions are added.
                                      <br/>
                                      <br/>")),
                           paste("Check out "), a("PolicyInNumbers", href="https://www.policyinnumbers.com/", target="_blank"), paste(" for more political
                                  RStats content!"))
  ),
  dashboardBody(
    
    # Change the theme
    shinyDashboardThemes(
      theme = "flat_red"
    ),
    

    tabItems(
      tabItem("canada",
              fluidRow(box(status = 'info', width = 12,
                           title = strong("View Elections Across Canada!"),
                           splitLayout(cellWidths = c('25%', '25%', '48%'),
                                       tagList(HTML(paste("Please select the election <br/> you want to map"))),
                                       radioButtons(inputId = 'election', 'Election Year:', c('2015', '2019')),
                                       tagList(HTML(paste("Feel free to scroll the map to view individual ridings. When you <br/> 
                                             hover above a riding, the winning party's name will appear. Below <br/> 
                                             the map you will see the Canada-wide voting statistics in <br/> 
                                             your selected election."))))), 
                       #Boxes to display the plots
                       box(status = 'primary', width = 12, solidHeader = TRUE,
                           leafletOutput('canMap')),
                       box(status = 'primary', width = 12, solidHeader = TRUE,
                           splitLayout(cellWidths = c('48%', '48%'),
                                       plotlyOutput('canSeatStats', height = "500px"),
                                       plotlyOutput('canVoteStats', height = "500px"))))),
      tabItem("province",
              fluidRow(box(status = 'info', width = 12,
                           title = strong("View Elections At A Provincial Level"),
                           splitLayout(cellWidths = c('24%', '24%', '48%'),
                                       tagList(HTML(paste("Please select the election <br/> and province you want <br/> to map"))),
                                       radioButtons(inputId = 'election2', 'Election Year:', c('2015', '2019')),
                                       selectInput(inputId = 'province', label = 'Province Selected:',
                                                   selectize = FALSE, size = 4,
                                                   choices = c('AB', 'BC', 'MB', 'NB', 'NL', 'NS', 'ON', 'PEI', 'QB', 'SK')))),
                       #Boxes to display the plots
                       box(status = 'primary', width = 12, solidHeader = TRUE,
                           leafletOutput('provMap')),
                       box(status = 'primary', width = 12, solidHeader = TRUE,
                           splitLayout(cellWidths = c('48%', '48%'),
                                       plotlyOutput('provSeatStats', height = "500px"),
                                       plotlyOutput('provVoteStats', height = "500px"))))),
      tabItem("riding",
              fluidRow(box(status = 'info', width = 12, 
                           title = strong("View Elections At A Riding Level"),
                           splitLayout(cellWidths = c('19%', '19%', '30%', '30%'), # < 100% prevents jittering caused by screen width conflict
                                       tagList(HTML(paste("Please select the election, <br/> province and riding you <br/> want to map"))),
                                       radioButtons(inputId = 'election3', 'Election Year:', c('2015', '2019')),
                                       selectInput(inputId = 'province2', 'Province Selected:',
                                                   selectize = FALSE, size = 4,
                                                   choices = NULL),
                                       selectInput(inputId = 'riding2', 'Riding Selected:',
                                                   selectize = FALSE, size = 4,
                                                   choices = NULL))),
                       #Boxes to display the plots
                       box(status = 'primary', width = 12, solidHeader = TRUE,
                           splitLayout(cellWidths = c('48%', '48%'),
                                       leafletOutput('rideMap', height = "500px"),
                                       plotlyOutput('rideVoteStats', height = "500px"))))),
      tabItem("poll",
              fluidRow(box(status = 'info', width = 12, 
                           title = strong("View Elections In Individual Communities By Poll Level"),
                           splitLayout(cellWidths = c('19%', '19%', '30%', '30%'),
                                       tagList(HTML(paste("Please select the election, <br/> province and riding you <br/> want to map"))),
                                       radioButtons(inputId = 'election4', 'Election Year:', c('2015', '2019')),
                                       selectInput(inputId = 'province3', 'Province Selected:',
                                                   selectize = FALSE, size = 4,
                                                   choices = NULL),
                                       selectInput(inputId = 'riding3', 'Riding Selected:',
                                                   selectize = FALSE, size = 4,
                                                   choices = NULL))),
                       #Boxes to display interactive map
                       box(status = 'primary', width = 12, solidHeader = TRUE, 
                           leafletOutput("pollMap", height = "500px"))
              )
      )
    )
  )
)

server <- shinyServer(function(input, output, session) {
  
  # Country-wide Analysis Page ====
  # Map of Canada
  output$canMap <- renderLeaflet({leaflet_can_map()})
  
  leaflet_can_map <- reactive({
    election <- ifelse(input$election =='2015', '2015', '2019')
    
    df.riding <- df.riding %>% 
      filter(year==election)
    
    full_map <- leaflet(df.riding$geometry, leafletOptions(crs = epsg2163,
                                                           minZoom = 3,
                                                           maxZoom = 5)) %>%
      setView(lng = -90, lat = 63.3, zoom = 3) %>%
      setMapWidgetStyle(style = list(background = "lightblue")) %>% 
      addTiles() %>% 
      addPolygons(weight = 1)
    
    labels <- sprintf(
      "<strong>Riding Name: %s <br/>Winning Party: %s<br/>",
  
      df.riding$district.name, df.riding$WinningParty) %>% 
      lapply(htmltools::HTML)
    
    full_map %>% 
      addPolygons(
        data = df.riding, #pull the data from here
        # fillColor = ~palette(WinningParty),
        fillColor = df.riding$colours, # fill using the color column
        weight = 0.7, # weight of the separator lines
        opacity = 1, # separator line opacity
        color = "white", # separator line color
        dashArray = "3", # dash type
        fillOpacity = 0.7, # fill opacity
        highlight = highlightOptions(
          weight = 5, # hover weight of lines
          color = "#666", # hover color
          dashArray = "", # hover dash (none)
          fillOpacity = 0.7, # hover fill
          bringToFront = TRUE), # highlight in front
        label = labels, # add labels
        # style the labels
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "12px",
          direction = "auto"))
  })
  
  # Seat distribution
  output$canSeatStats <- renderPlotly({

    election <- ifelse(input$election =='2015', '2015', '2019')

    df2 <- df.riding %>%
      st_set_geometry(NULL) %>% # gets rid of geometry column in dataframe and converts it back to a df
      filter(year==election) %>%
      group_by(year, WinningParty) %>%
      select(year, riding_code, district.name, WinningParty) %>%
      count(WinningParty)

    seatColoursSub <- partyColours[match(df2$WinningParty, partyColours$Party), 'colours']

    plot_ly(df2, x = ~WinningParty, y = ~n, type = 'bar',
            marker = list(color = ~seatColoursSub)) %>%
      layout(title = sprintf("<b>Number of Seats won in the\n %s Canadian Election</b>", input$election),
             xaxis = list(title = "<b>Party</b>"),
             yaxis = list(title = "<b>Seats Won</b>"))

  })
  # Vote distribution
  output$canVoteStats <- renderPlotly({

    election <- ifelse(input$election =='2015', '2015', '2019')

    df3 <- df.riding %>%
      st_set_geometry(NULL) %>% # gets rid of geometry column in dataframe and converts it back to a df
      filter(year==election)
    df3 <- df3[,6:14] %>%
      sapply(sum) %>%
      as.data.frame() %>%
      rownames_to_column()
    colnames(df3) <- c("Party","Votes")
    df3[8,2] <- sum(df3[6:8,2])
    df3$Share <- round(df3$Votes/df3[9,2] * 100, digits = 1)
    df3$Party <- gsub('.{5}$','', df3$Party)
    df3 <- df3[-c(6:7,9),]

    voteColoursSub <- partyColours[match(df3$Party, partyColours$Party), 'colours']

    plot_ly(df3, x = ~Party, y = ~Share, type = 'bar',
            marker = list(color = ~voteColoursSub)) %>%
      layout(title = sprintf("<b>Share of National Vote by Party\n in the %s Canadian Election</b>", input$election),
             xaxis = list(title = "<b>Party</b>"),
             yaxis = list(title = "<b>Share of National Vote (%)</b>"))
  })
  
  # Provincial Analysis Page ====
  # Provincial Maps
  output$provMap <- renderLeaflet({leaflet_prov_map()})
  
  leaflet_prov_map <- reactive({
    election <- ifelse(input$election2 =='2015', '2015', '2019')
    
    df.riding <- df.riding %>% 
      filter(year==election) %>% 
      filter(province==input$province)
    
    full_map <- leaflet(df.riding$geometry, leafletOptions(crs = epsg2163,
                                                           minZoom = 3,
                                                           maxZoom = 5)) %>%
      setMapWidgetStyle(style = list(background = "lightblue")) %>% 
      addTiles() %>% 
      addPolygons(weight = 1)
    
    labels <- sprintf(
      "<strong>Riding Name: %s <br/>Winning Party: %s<br/>
      Vote Share:</strong><br/>Liberal Share = %g%% (%g votes)<br/>Conservative Share = %g%% (%g votes)
      <br/>NDP Share = %g%% (%g votes)<br/>Bloc Share = %g%% (%g votes)<br/>Green Share = %g%% (%g votes)",
  
      
      df.riding$district.name, df.riding$WinningParty, df.riding$LiberalShare, df.riding$LiberalVotes, df.riding$ConservativeShare, df.riding$ConservativeVotes,
      df.riding$NDPShare, df.riding$NDPVotes, df.riding$BlocShare, df.riding$BlocVotes,
      df.riding$GreenShare, df.riding$GreenVotes) %>% 
      lapply(htmltools::HTML)
    
    full_map %>% 
      addPolygons(
        data = df.riding, #pull the data from here
        # fillColor = ~palette(WinningParty),
        fillColor = df.riding$colours, # fill using the color column
        weight = 0.7, # weight of the separator lines
        opacity = 1, # separator line opacity
        color = "white", # separator line color
        dashArray = "3", # dash type
        fillOpacity = 0.7, # fill opacity
        highlight = highlightOptions(
          weight = 5, # hover weight of lines
          color = "#666", # hover color
          dashArray = "", # hover dash (none)
          fillOpacity = 0.7, # hover fill
          bringToFront = TRUE), # highlight in front
        label = labels, # add labels
        # style the labels
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "12px",
          direction = "auto"))
  })

  # Seat distribution
  output$provSeatStats <- renderPlotly({

    election2 <- ifelse(input$election2 =='2015', '2015', '2019')

    df2 <- df.riding %>%
      st_set_geometry(NULL) %>% # gets rid of geometry column in dataframe and converts it back to a df
      filter(year==election2) %>%
      filter(province==input$province) %>%
      group_by(year, WinningParty) %>%
      select(year, riding_code, district.name, WinningParty) %>%
      count(WinningParty)

    seatColoursSub <- partyColours[match(df2$WinningParty, partyColours$Party), 'colours']

    plot_ly(df2, x = ~WinningParty, y = ~n, type = 'bar',
            marker = list(color = ~seatColoursSub)) %>%
      layout(title = sprintf("<b>Number of Seats won in %s during\n the %s Canadian Election</b>", input$province, input$election2),
             xaxis = list(title = "<b>Party</b>"),
             yaxis = list(title = "<b>Provincial Seats Won</b>"))

  })
  # Vote distribution
  output$provVoteStats <- renderPlotly({

    election2 <- ifelse(input$election2 =='2015', '2015', '2019')

    df3 <- df.riding %>%
      st_set_geometry(NULL) %>% # gets rid of geometry column in dataframe and converts it back to a df
      filter(year==election2) %>%
      filter(province==input$province)

    df3 <- df3[,6:14] %>%
      sapply(sum) %>%
      as.data.frame() %>%
      rownames_to_column()
    colnames(df3) <- c("Party","Votes")
    df3[8,2] <- sum(df3[6:8,2])
    df3$Share <- round(df3$Votes/df3[9,2] * 100, digits = 1)
    df3$Party <- gsub('.{5}$','', df3$Party)
    df3 <- df3[-c(6:7,9),]

    voteColoursSub <- partyColours[match(df3$Party, partyColours$Party), 'colours']

    plot_ly(df3, x = ~Party, y = ~Share, type = 'bar',
            marker = list(color = ~voteColoursSub)) %>%
      layout(title = sprintf("<b>Share of %s Vote by Party\n in the %s Canadian Election</b>", input$province, input$election2),
             xaxis = list(title = "<b>Party</b>"),
             yaxis = list(title = "<b>Share of Provincial Vote (%)</b>"))
  })
  
  # Riding Analysis Page ====
  # Riding Map Select input
  updateSelectInput(session,
                    "province2",
                    choices = c('AB', 'BC', 'MB', 'NB', 'NL', 'NS', 'NT', 'NU', 'ON', 'PEI', 'QB', 'SK', 'YU'),
                    selected = 'AB')

  observeEvent(c(input$province2),
               {
                 election3 <- ifelse(input$election3 =='2015', '2015', '2019')
                 
                 ridings_in_province <- df.riding %>%
                   filter(year==election3) %>%
                   filter(province==input$province2) %>%
                   pull(unique(district.name))

                 updateSelectInput(session,
                                   "riding2",
                                   choices = ridings_in_province,
                                   selected = ridings_in_province[1])
               })
  # Riding Maps
  output$rideMap <- renderLeaflet({leaflet_ride_map()})
  
  leaflet_ride_map <- reactive({
    election3 <- ifelse(input$election3 =='2015', '2015', '2019')
    
    df.riding <- df.riding %>% 
      filter(year==election3) %>% 
      filter(district.name==input$riding2)
    
    full_map <- leaflet(df.riding$geometry, leafletOptions(crs = epsg2163,
                                                           minZoom = 3,
                                                           maxZoom = 5)) %>%
      setMapWidgetStyle(style = list(background = "lightblue")) %>% 
      addTiles() %>% 
      addPolygons(weight = 1)
    
    labels <- sprintf(
      "<strong>Riding Name: %s <br/>Winning Party: %s<br/>
      Vote Share:</strong><br/>Liberal Share = %g%% (%g votes)<br/>Conservative Share = %g%% (%g votes)
      <br/>NDP Share = %g%% (%g votes)<br/>Bloc Share = %g%% (%g votes)<br/>Green Share = %g%% (%g votes)",
      
      
      df.riding$district.name, df.riding$WinningParty, df.riding$LiberalShare, df.riding$LiberalVotes, df.riding$ConservativeShare, df.riding$ConservativeVotes,
      df.riding$NDPShare, df.riding$NDPVotes, df.riding$BlocShare, df.riding$BlocVotes,
      df.riding$GreenShare, df.riding$GreenVotes) %>% 
      lapply(htmltools::HTML)
    
    full_map %>% 
      addPolygons(
        data = df.riding, #pull the data from here
        # fillColor = ~palette(WinningParty),
        fillColor = df.riding$colours, # fill using the color column
        weight = 0.7, # weight of the separator lines
        opacity = 1, # separator line opacity
        color = "white", # separator line color
        dashArray = "3", # dash type
        fillOpacity = 0.7, # fill opacity
        highlight = highlightOptions(
          weight = 5, # hover weight of lines
          color = "#666", # hover color
          dashArray = "", # hover dash (none)
          fillOpacity = 0.7, # hover fill
          bringToFront = TRUE), # highlight in front
        label = labels, # add labels
        # style the labels
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"), 
          textsize = "12px",
          direction = "auto"))
  })
  
  # Riding Vote distribution
  output$rideVoteStats <- renderPlotly({

    election3 <- ifelse(input$election3 =='2015', '2015', '2019')

    df3 <- df.riding %>%
      st_set_geometry(NULL) %>% # gets rid of geometry column in dataframe and converts it back to a df
      filter(year==election3) %>%
      filter(district.name==input$riding2)

    df3 <- df3[,6:14] %>%
      sapply(sum) %>%
      as.data.frame() %>%
      rownames_to_column()
    colnames(df3) <- c("Party","Votes")
    df3[8,2] <- sum(df3[6:8,2])
    df3$Share <- round(df3$Votes/df3[9,2] * 100, digits = 1)
    df3$Party <- gsub('.{5}$','', df3$Party)
    df3 <- df3[-c(6:7,9),]

    voteColoursSub <- partyColours[match(df3$Party, partyColours$Party), 'colours']

    plot_ly(df3, x = ~Party, y = ~Share, type = 'bar',
            marker = list(color = ~voteColoursSub)) %>%
      layout(title = sprintf("<b>Share of %s's Vote \nby Party in the %s Canadian Election</b>", input$riding2, input$election3),
             xaxis = list(title = "<b>Party</b>"),
             yaxis = list(title = "<b>Share of Riding Vote (%)</b>"))
  })
  
  # Poll Station Analysis Page ====
  # Poll Input
  updateSelectInput(session,
                    "province3",
                    choices = c('AB', 'BC', 'MB', 'NB', 'NL', 'NS', 'NT', 'NU', 'ON', 'PEI', 'QB', 'SK', 'YU'),
                    selected = 'AB')

  observeEvent(c(input$province3),
               {
                 election4 <- ifelse(input$election4 =='2015', '2015', '2019')

                 ridings_in_province <- df.polls %>%
                   filter(year==election4) %>%
                   filter(province==input$province3) %>%
                   pull(unique(district.name)) %>% 
                   as.vector()

                 updateSelectInput(session,
                                   "riding3",
                                   choices = unique(ridings_in_province),
                                   selected = unique(ridings_in_province[1]))
               })


  # Poll Maps
  # Need to link the riding code with the riding name
  output$pollMap <- renderLeaflet({leaflet_poll_map()})

  leaflet_poll_map <- reactive({
    election4 <- ifelse(input$election4 =='2015', '2015', '2019')
    
    df3 <- df.polls %>%
      filter(year==election4) %>%
      filter(district.name==input$riding3)

    full_map <- leaflet(df3$geometry, leafletOptions(crs = epsg2163,
                                                           minZoom = 3,
                                                           maxZoom = 5)) %>%
      setMapWidgetStyle(style = list(background = "lightblue")) %>%
      addTiles() %>%
      addPolygons(weight = 1)

    labels <- sprintf(
      "<strong>Poll Name & Number: %s - %g<br/>Winning Party: %s<br/>
  Vote Share:</strong><br/>Liberal Share = %g%% (%g votes)<br/>Conservative Share = %g%% (%g votes)
  <br/>NDP Share = %g%% (%g votes)<br/>Bloc Share = %g%% (%g votes)<br/>Green Share = %g%% (%g votes)",

  df3$poll.station.name, df3$poll.station.number, df3$WinningParty,
  df3$LiberalShare, df3$LiberalVotes, df3$ConservativeShare, df3$ConservativeVotes,
  df3$NDPShare, df3$NDPVotes, df3$BlocShare, df3$BlocVotes,
  df3$GreenShare, df3$GreenVotes) %>%
      lapply(htmltools::HTML)

    full_map %>%
      addPolygons(
        data = df3, #pull the data from here
        # fillColor = ~palette(WinningParty),
        fillColor = df3$colours, # fill using the color column
        weight = 0.7, # weight of the separator lines
        opacity = 1, # separator line opacity
        color = "white", # separator line color
        dashArray = "3", # dash type
        fillOpacity = 0.7, # fill opacity
        highlight = highlightOptions(
          weight = 5, # hover weight of lines
          color = "#666", # hover color
          dashArray = "", # hover dash (none)
          fillOpacity = 0.7, # hover fill
          bringToFront = TRUE), # highlight in front
        label = labels, # add labels
        # style the labels
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "12px",
          direction = "auto"))
  })

})
shinyApp(ui, server)