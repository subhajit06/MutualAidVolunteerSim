library(data.table)
library(stringr)
library(shiny)
library(shinyjs)
library(dplyr)
library(DT)
library(gridExtra)
library(grid)
library(cowplot)
library(rdrop2)
library(data.table)
library(leaflet)
library(ggmap)
library(tigris) 
library(RColorBrewer)
library(stringi)
library(geosphere)
library(blastula)
#########################
### making the gmail account less secure
### https://forum.sierrawireless.com/t/curl-67-login-denied/18735
## https://cran.r-project.org/web/packages/blastula/vignettes/sending_using_smtp.html
## Store SMTP credentials as a file
## with the filename "gmail_creds"

#create_smtp_creds_file(
#  file = "gmail_creds",
#  user = "subho.testing.app@gmail.com",
#  host = "smtp.gmail.com",
#  port = 465,
#  use_ssl = TRUE
#)

send_emails_to_both_parties <- function(df1, df2){
  
  #df1 need help; df2 will help
  
  h1_name = df1$name
  h1_phone = df1$phone
  h1_email = as.character(df1$email)
  
  h2_name = df2$name
  h2_phone = df2$phone
  h2_email = as.character(df2$email)
  
  ### email_1 from system to helper
  email_1 <- compose_email(body = 
              sprintf('Dear %s, Thanks for responding. %s need your help. Plz call @%s to know the details.', 
                        h2_name, h1_name, h1_phone))
  
  ### email_2 from system to who needs help
  email_2 <- compose_email(body = 
              sprintf('Dear %s, A volunteer named %s is available. Plz call @%s to know the details.', 
                        h1_name, h2_name, h2_phone))
  
  # Sending email to a personal account using the on-disk credentials file
  email_1 %>%
    smtp_send(
      from = "Solidarity Network",
      to = h2_email,
      subject = "Urgent: Help Needed!",
      credentials = creds_file(file = "gmail_creds")
    )
  
  email_2 %>%
    smtp_send(
      from = "Solidarity Network",
      to = h1_email,
      subject = "Help on the way!",
      credentials = creds_file(file = "gmail_creds")
    )
}


get_dist_in_miles <- function(lng1, lat1, lng2, lat2){
  d_meter = distm (c(lng1, lat1), c(lng2, lat2), fun = distHaversine)[1]
  d_miles = d_meter/1609.34
  return(d_miles)
}

## https://rstudio.github.io/leaflet/markers.html
getColor <- function(df) {
  sapply(df$role, function(x) { ifelse(x == 'Volunteer', "green", "yellow")})
}

get_new_icons <-function(df){ 
  
  df_col = sapply(df$role, function(x) { ifelse(x == 'Volunteer', "green", "orange")})
  icons = awesomeIcons( icon = 'ios-close', iconColor = 'black', 
                       library = 'ion', markerColor = df_col)
  return(icons)
}

get_new_icons_active <-function(df){ 
  
  df_col = sapply(df$role, function(x) { ifelse(x == 'Volunteer', "blue", "red")})
  icons = awesomeIcons( icon = 'ios-close', iconColor = 'black', 
                        library = 'ion', markerColor = df_col)
  return(icons)
}


fileName = sprintf("volunteers_and_help")

current_zips_served = c("60201","60202","60076","60077")
L = length(current_zips_served)
pal <- colorBin("Accent", domain = c(1,L), bins = 2*L)

#options(tigris_use_cache = TRUE)
#data_zips <- zctas(cb = TRUE, starts_with = current_zips_served)
#data_zips$col = seq(1,L,by = 1)
#saveRDS(data_zips, file="./data_zips.rds")

data_zips = readRDS(file="./data_zips.rds")

# join zip boundaries and covid data 
#df_zip_and_other <- geo_join(data_zips, df_other, 
#                                 by_sp = "GEOID10", by_df = "zipcode", how = "inner")

outputDir <- "responses/database"

lat_reset = 42.0385
lng_reset = -87.724

# Define the fields we want to save from the form
fields <- c("name", "address", "email", "phone", "role", "slot")

saveData <- function(data_new) {
  
  data_old = loadData()
  data_all = rbind(data_old, data_new)
  
  # Create a unique file name
  fileName <- sprintf("%s.csv", fileName)
  # Write the data to a temporary file locally
  filePath <- file.path(tempdir(), fileName)
  fwrite(data_all, filePath, row.names = FALSE, quote = FALSE, sep = "\t")
  # Upload the file to Dropbox
  drop_upload(filePath, path = outputDir)
}

loadData <- function() {
  # Read all the files into a list
  data_old = NULL
  filesInfo = drop_dir(outputDir)
  if(sum(dim(filesInfo)) != 0){
    filePaths = filesInfo$path_lower
    data_old = as.data.frame(drop_read_csv(filePaths, sep = "\t"))
    # Concatenate all data together into one data.frame
  }
  return(data_old)
}


### create base map

basemap = data_zips %>%
  leaflet() %>%
  addProviderTiles("Stamen.Terrain") %>%
  setView(lat = lat_reset, lng = lng_reset, zoom = 13) %>%
  addPolygons(fillColor = ~pal(col),
              weight = 3,
              opacity = 0.9,
              color = "#164781",
              dashArray = "2")


########### Shiny code

ui <- fluidPage( useShinyjs(),
  
  titlePanel("You-AND-Me"),
  
  sidebarLayout(
    
    sidebarPanel(width=4,
                 
                 fluidRow(
                   column(10, textInput("name", "Name", ""))
                 ),
                 fluidRow(
                   column(12, textInput("street", "Street", ""))
                 ),
                 fluidRow(
                   column(5, textInput("city", "City", "")),
                   column(3, textInput("state", "State", "IL")),
                   column(4, textInput("zipcode", "Zip", ""))
                 ),
                 
                 fluidRow(
                   column(12, textInput("email", "Email", "")),
                   column(7, textInput("phone", "Phone", "")),
                   column(5, selectInput("role", "Role", c("Volunteer", "Need_Help")))
                 ),
                 
                 fluidRow(
                   column(6, checkboxGroupInput("slot", "Available Slot",
                                            c("10am-12pm" = "ts_10_12",
                                              "12pm-2pm" = "ts_12_2",
                                              "2pm-4pm" = "ts_2_4",
                                              "4pm-6pm" = "ts_4_6"))),

                   column(3, actionButton("check", "Check")),
                   column(3, actionButton("submit", "Submit"))
                 ),
                 
                 hr(),
                 fluidRow(
                   column(5, actionButton("addEvent", "Call for a help ")),
                   column(5, actionButton("resolveEvent", "Resolve crisis")),
                 )
                 
    ), ### sidebar
    
    #### Main panel
    mainPanel(width = 8,
              
              tabsetPanel(id = "tabs",type = "tabs",
                          tabPanel("Map", h4('Event log'), htmlOutput("t1"), htmlOutput("t2"), hr(),
                                   leafletOutput("map", width="100%", height="550px")),
                          #tabPanel("Event log", htmlOutput("t1")),
                          tabPanel("Data Browser", DT::dataTableOutput("responses", width = 300)),
                          tabPanel("Debug", dataTableOutput("table"))
              )
    ) ### main panel
  ) ### sidebarLayout
) ### fluidPage

server = function(input, output, session) {
    
    output$map <- renderLeaflet({
      
      data_old = loadData()
      
      if(!is.null(data_old)){
        pop_txt = paste0("<b>", data_old$role, "</b>",": ", data_old$name, "<br/>",
                         "<b>", "Email", "</b>", ": ", data_old$email,"<br/>",
                         "<b>", "Phone", "</b>", ": ", data_old$phone) %>%
                                lapply(htmltools::HTML)
        basemap %>% 
          addAwesomeMarkers(data = data_old, lat = ~lat, lng = ~lng, 
                            icon = get_new_icons(data_old), popup = pop_txt, group = ~ID)
      }else{
           basemap           
      }
  })
    
    
    
    # Whenever a field is filled, aggregate all form data
    formData <- reactive({
      data_new = NULL
      ### generate a random ID
      data_new$ID = stri_rand_strings(1, 3, pattern = "[1-9]")
      data_new$name = input$name
      data_new$street = input$street
      data_new$city = input$city
      data_new$state = input$state
      data_new$zipcode = input$zipcode
      data_new$address = as.character(paste0(data_new$street, ", ",data_new$city, ", ",
                                            data_new$state, ", ",data_new$zipcode))
      data_new$email = input$email
      data_new$phone = input$phone
      data_new$role = input$role
      data_new$slot = paste0(input$slot,collapse = "|")
    
      result = geocode(data_new$address, output = "latlona", source = "google")
      data_new$lng = as.numeric(result$lon)
      data_new$lat = as.numeric(result$lat)
      
      data_new = as.data.frame(data_new)
      return(data_new)
    })
    
    observeEvent(input$check, {
      data_new = formData()
      #output$t1 = renderText( paste0(data_new$name) )
      output$table = DT::renderDataTable(data_new)
      
      #pop_txt = paste0("<b>", data_new$role, "</b>",": ", data_new$name, "</br>", 
      #                 "<b>", "Phone", "</b>", ": ", data_new$phone) %>%
      #                 lapply(htmltools::HTML)
      
      #leafletProxy(mapId = "map", session) %>%
      #  #clearMarkers() %>%   ## clear previous markers
      #  addAwesomeMarkers(data_new$lng, data_new$lat, icon = get_new_icons(data_new), popup = pop_txt)
    })
    
    
    #read_local_data <- reactive({
    #  data_update = loadData()
    #  data_update$status = rep('not_in_action', dim(data_update)[1])
    #  data_update = readRDS("./data_update.rds")
    #  return(data_update)
    #})
    
    # When the Submit button is clicked, save the form data
    observeEvent(input$submit, {
      
      data_new = formData()
      output$table = DT::renderDataTable(data_new)
      saveData(data_new)
      
      pop_txt = paste0("<b>", data_new$role, "</b>",": ", data_new$name, "<br/>", 
                       "<b>", "Phone", "</b>", ": ", data_new$phone, "<br/>",
                       "<b>", "Email", "</b>", ": ", data_new$email) %>%
        lapply(htmltools::HTML)
      leafletProxy(mapId = "map", session) %>%
        #clearMarkers() %>%   ## clear previous markers
        addAwesomeMarkers(data_new$lng, data_new$lat, icon = get_new_icons(data_new), 
                          popup = pop_txt, group = data_new$ID)
      
      saveRDS(data_new, "./data_update.rds")
    })
    
    # Show the previous responses
    # (update with current response when Submit is clicked)
    output$responses <- DT::renderDataTable({
      input$submit
      loadData()
    })     
    
    clean_up_map <- function(){
      
      data_update = readRDS("./data_update.rds")
      
      pop_txt = paste0("<b>", data_update$role, "</b>",": ", data_update$name, "<br/>", 
                       "<b>", "Email", "</b>", ": ", data_update$email,
                       "<b>", "Phone", "</b>", ": ", data_update$phone,"<br/>") %>%
        lapply(htmltools::HTML)
      
      leafletProxy(mapId = "map", session) %>%
        clearMarkers() %>%   ## clear previous markers
        clearGroup(group = 'duo') %>%
        addAwesomeMarkers(data_update$lng, data_update$lat, icon = get_new_icons(data_update), 
                          popup = pop_txt)
    }
    
    ########## introducing an Helping event
    
    observeEvent(input$addEvent, {
      
      data_old = loadData()
      data_update = data_old
      data_update$status = rep('not_in_action', dim(data_update)[1])
      
      id_hlp = which(data_old$role == "Need_Help")
      
      K_hlp = length(id_hlp)
      
      event_chain_str = ""
      
      if(K_hlp > 0){
        rnd_indx = id_hlp[sample(1:K_hlp, 1)]
        df_need = data_old[rnd_indx, ]
        data_update$status[rnd_indx] = 'in_action'
        
        event_chain_str_1 = sprintf("Help needed for %s", df_need$name)
        event_chain_str_2 = sprintf("Searching for a volunteer .... ")
        
        saveRDS(data_update, file="./data_update.rds")
        df_tmp = df_need
      }
      
      
      output$t1 = renderUI(HTML(paste(event_chain_str_1, 
                                        event_chain_str_2,
                                        sep = '<br/>')))
      output$t2 = renderUI(HTML(paste("")))
      
      pop_txt = paste0("<b>", df_tmp$role, "</b>",": ", df_tmp$name, "<br/>",
                       "<b>", "Email", "</b>", ": ", df_tmp$email,"<br/>",
                       "<b>", "Phone", "</b>", ": ", df_tmp$phone) %>% 
        lapply(htmltools::HTML)
      
      leafletProxy(mapId = "map", session) %>%
        clearGroup(group = df_tmp$ID) %>%   ## clear previous markers
        #clearGroup(group = 'line') %>%
        addAwesomeMarkers(df_tmp$lng, df_tmp$lat, icon = get_new_icons_active(df_tmp), 
                          popup = pop_txt,
                          label = "HELP !!",
                          labelOptions = labelOptions(noHide = T, direction = "bottom",
                                                      style = list("font-size" = "14px",
                                                        "color" = "red")))
      
    })
    
    observeEvent(input$resolveEvent, {
      
      data_update = readRDS("./data_update.rds")
      df_need = data_update %>% filter(status == 'in_action')
      
      id_vol = which(data_update$role == "Volunteer")
      K_vol = length(id_vol)
      
      event_chain_str = ""
      
      if(K_vol > 0){
        
        dist_arr = unlist(lapply(id_vol, function(x) {
          get_dist_in_miles(data_update$lng[x], data_update$lat[x], df_need$lng, df_need$lat)}))
        
        id_min = which.min(dist_arr)  
        picked_vol_id = id_vol[id_min]
        
        data_update$status[picked_vol_id] = 'in_action'
        
        event_chain_str_2 = sprintf("Algorithm found a volunteer: %s", 
                                    data_update$name[picked_vol_id])
        
        event_chain_str_3 = sprintf("Sending email to %s (%s) to dispatch ...",
                                    data_update$name[picked_vol_id],data_update$email[picked_vol_id])
        
        event_chain_str_4 = sprintf("Sending email to %s (%s) to give volunteer information ...", 
                                    df_need$name, df_need$email)
        
        df_tmp = data_update[picked_vol_id,]
        df_duo = as.data.frame(rbind(df_need,df_tmp))
      }
      
      output$t2 = renderUI(HTML(paste(event_chain_str_2,
                                      event_chain_str_3,
                                      event_chain_str_4,
                                      sep = '<br/>')))
      
      pop_txt = paste0("<b>", df_tmp$role, "</b>",": ", df_tmp$name, "<br/>", 
                       "<b>", "Email", "</b>", ": ", df_tmp$email,"<br/",
                       "<b>", "Phone", "</b>", ": ", df_tmp$phone) %>%
        lapply(htmltools::HTML)
      
      leafletProxy(mapId = "map", session) %>%
        clearGroup(group = df_tmp$ID) %>%   ## clear previous markers
        addAwesomeMarkers(df_tmp$lng, df_tmp$lat, icon = get_new_icons_active(df_tmp), 
                          popup = pop_txt) %>%
        addPolylines(data = df_duo, lng = ~lng, lat = ~lat, color = 'blue', group = 'duo')
      
      need_id = which(data_update$status == 'in_action')
      data_update$status[need_id] = 'not_in_action'
      saveRDS(data_update, "./data_update.rds")
      
      send_emails_to_both_parties(df_need, data_update[picked_vol_id,])
      
      delay(5000, clean_up_map())
      
    })
    
    
}


you_and_me_app = shinyApp(ui = ui, server = server)
runApp(you_and_me_app)
#runApp(you_and_me_app,launch.browser = TRUE, host = '127.0.0.1', port = 23137)
  