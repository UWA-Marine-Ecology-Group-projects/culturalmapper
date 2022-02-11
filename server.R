# Server ----
server = function(input, output, session) {
  
  # Hide tabs at the start - so user has to click next (with validations) ----
  hideTab(inputId = "surveybox", target = "Values mapping")
  hideTab(inputId = "surveybox", target = "Spatial questions")
  hideTab(inputId = "surveybox", target = "Social values and benefits")
  
  # Pretty disconnect message ----
  observeEvent(input$disconnect, {
    session$close()
  })
  
  # Show the shiny modal on start up with welcome info ----
  showModal(
    modalDialog(
      size = "l",
      includeMarkdown("welcome.md"),
      easyClose = FALSE,
      fade = TRUE,
      footer = NULL,
      div(
        style = "display:inline-block;width:100%;text-align: center;",
        actionBttn(
          inputId = "ok",
          label = "Ok",
          style = "unite",
          size = "lg",
          color = "primary"
        )
      )
    )
  )
  
  # Then show information for participants ----
  observeEvent(input$ok, {
    showModal(modalDialog(
      size = "l",
      includeMarkdown("information.for.participants_online.md"),
      footer = NULL,
      fade = T,
      div(
        style = "display:inline-block;width:100%;text-align: center;",
        actionBttn(
          inputId = "accept",
          label = "Yes",
          style = "unite",
          size = "lg",
          color = "primary"
        ),
        actionBttn(
          inputId = "decline",
          label = "No",
          style = "unite",
          size = "lg",
          color = "danger"
        )
      )
    ))
  })
  
  # If user accepts the information for participants then close the modal and allow user to complete the survey ----
  observeEvent(input$accept, {
    removeModal()
  })
  
  # If user declines the information for participants then a new modal will display with instructions on how to exit ----
  observeEvent(input$decline, {
    shinyalert(
      title = "You have chosen to not participate in this study. ",
      text = "We thank you for your time. Please close this window or tab to exit the study.",
      size = "s",
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      html = FALSE,
      type = "error",
      showConfirmButton = FALSE,
      timer = 0,
      animation = TRUE
    )
  })
  
  # Need to validate postcode, but don't collect a postcode if user is from overseas.
  # SO, create a dummy postcode 9999 for overseas users
  observeEvent(input$residence,
               if (input$residence == "Overseas"){
                 updateTextInput(session, "postcode", value = "9999")
               } else {
                 updateTextInput(session, "postcode", value = NA)
               }
  )
  
  # Only enable nextactivities if all contact info is filled out
  observeEvent(input$nextactivities,
               if (input$name %in% c(NA, '', ' ') |
                   isValidEmail(input$email) %in% c(FALSE) |
                   !nchar(gsub("[^0-9]+", "", input$phone)) == 10 |
                   is.null(input$residence) | # if not null and all others are filled out, then the next statement will check the postcode format
                   is.null(input$gender) |
                   is.null(input$age) |
                   is.null(input$frequency) 
                   
                   ) {
                 
                 shinyalert(
                   "Please fill out all fields",
                   type = "error",
                   timer = 2000,
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE
                 )
                 
                 updateTabsetPanel(session, "surveybox", selected = "Contact information")
                 
               } else if(!is.null(input$residence) & !nchar(gsub("[^0-9]+", "", input$postcode)) == 4 ) {
                 
                 shinyalert(
                   "Please fill out all fields",
                   type = "error",
                   timer = 2000,
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE
                 )
                 
                 updateTabsetPanel(session, "surveybox", selected = "Contact information")
                 
               } else {
                 updateTabsetPanel(session, "surveybox", selected = "Values mapping")
                 showTab(inputId = "surveybox", target = "Values mapping")
                 hideTab(inputId = "surveybox", target = "Contact information")
                 updateProgressBar(session, "progress", value = 25, total = 100)
                 
                 showModal(
                   modalDialog(
                     size = "m",
                     includeMarkdown("instructions.md"),
                     easyClose = TRUE,
                     fade = TRUE,
                     footer = NULL,
                     div(
                       style = "display:inline-block;width:100%;text-align: center;",
                       actionBttn(
                         inputId = "oki",
                         label = "Ok",
                         style = "unite",
                         size = "lg",
                         color = "primary"
                       )
                   )
                 )
                 )
                 
                 # shinyalert(
                 #   title = "Values Mapping Instructions",
                 #   html = TRUE,
                 #   text =  tagList(
                 #     "Use this section to map your use and local knowledge of the marine environment on the South Coast",
                 #     br(),
                 #     br(),
                 #     h6(strong("There are 3 steps:")),
                 #     tags$ol(
                 #     tags$li("Indicate broadly in which areas you would like to map your use or local knowledge"), 
                 #     br(),
                 #     tags$li("Indicate which activities and local knowledge topics you would like to map"), 
                 #     br(),
                 #     tags$li("Map your use or local knowledge on the maps provided (next page)"))),
                 #   
                 #   size = "s",
                 #   closeOnEsc = FALSE,
                 #   closeOnClickOutside = FALSE,
                 #   type = "info",
                 #   showConfirmButton = TRUE,
                 #   timer = 0,
                 #   animation = TRUE
                 # )
               })
  
  observeEvent(input$oki, {
    removeModal()
  })
  
  # When moving from activities to spatial show 
  observeEvent(input$nextspatial,
               
               if (nrow(selected.data()) > 0 & nrow(regionlist()) > 0){
                 showTab(inputId = "surveybox", target = "Spatial questions")
                 updateTabsetPanel(session, "surveybox", selected = "Spatial questions")
                 updateProgressBar(session, "progress", value = 50, total = 100)
                 hideTab(inputId = "surveybox", target = "Values mapping")
                 
               } else if (nrow(selected.data()) > 0 & nrow(regionlist()) == 0){
                 shinyalert(
                   "Please select at least one region",
                   type = "error",
                   timer = 2000,
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE
                 )
                 
               } else if (nrow(selected.data()) == 0 & nrow(regionlist()) > 0){
                 shinyalert(
                   "Please select at least one activitiy",
                   type = "error",
                   timer = 2000,
                   closeOnEsc = TRUE,
                   closeOnClickOutside = TRUE
                 )
               } else {
                 
                 showTab(inputId = "surveybox", target = "Social values and benefits")
                 updateTabsetPanel(session, "surveybox", selected = "Social values and benefits")
                 updateProgressBar(session, "progress", value = 75, total = 100)
                 hideTab(inputId = "surveybox", target = "Values mapping")
                 
                 })
  
  observeEvent(
    input$nextnonspatial,
    showTab(inputId = "surveybox", target = "Social values and benefits")
    
  )
  
  observeEvent(
    input$nextnonspatial,
    updateTabsetPanel(session, "surveybox", selected = "Social values and benefits")
  )

  observeEvent(input$nextnonspatial,
               hideTab(inputId = "surveybox", target = "Spatial questions"))
  
  observeEvent(input$nextnonspatial,
               updateProgressBar(session, "progress", value = 75, total = 100)
               )
  
  #### Add in back buttons ----
  ### When moving from activities back to contact information 
  # - show tab contact
  # - hide tab activities
  # - change tab to contact
  # - decrease progress bar
  observeEvent(input$backcontact,
               if (!is.na(input$name)){
               showTab(inputId = "surveybox", target = "Contact information")
               updateTabsetPanel(session, "surveybox", selected = "Contact information")
               updateProgressBar(session, "progress", value = 0, total = 100)
               hideTab(inputId = "surveybox", target = "Values mapping")
               }
)
  
  ### When moving from spatial back to activities show modal to ask user if they are sure (they will lose polygons clicked) 
  observeEvent(input$backactivities, {
    
    shinyalert(title = "Going back will delete questions you have answered on this page. Are you sure you wish to go back? ",
               # text = "If you go back you will lose all locations clicked",
               type = "warning",
               html = TRUE,
               showCancelButton = FALSE,
               showConfirmButton = FALSE,
               text = tagList(
                 actionBttn(
                         inputId = "goback",
                         label = "Go back",
                         style = "unite",
                         size = "lg",
                         color = "danger"
                       ),
                       actionBttn(
                         inputId = "cancel",
                         label = "Cancel",
                         style = "unite",
                         size = "lg",
                         color = "primary"
                       )
               ))
  })
  
  observeEvent(input$goback,
               if (!is.na(input$name)){
                 showTab(inputId = "surveybox", target = "Values mapping")
                 updateTabsetPanel(session, "surveybox", selected = "Values mapping")
                 updateProgressBar(session, "progress", value = 25, total = 100)
                 hideTab(inputId = "surveybox", target = "Spatial questions")
               }
  )
  
  ### When moving from non-spatial back to spatial (if regions and activities selected) or activities (if none selected)
  ##if regions and activities selected
  # - show tab spatial
  # - hide tab non-spatial
  # - change tab to spatial
  # - decrease progress bar
  ##if NO regions and activities selected
  # - show tab activities
  # - hide tab non-spatial
  # - change tab to acitivties
  # - decrease progress bar
  
  observeEvent(input$backspatial,
               if (nrow(selected.data()) > 0 & nrow(regionlist()) > 0){
                 showTab(inputId = "surveybox", target = "Spatial questions")
                 hideTab(inputId = "surveybox", target = "Social values and benefits")
                 updateTabsetPanel(session, "surveybox", selected = "Spatial questions")
                 updateProgressBar(session, "progress", value = 50, total = 100)
                 
               } else {
                 showTab(inputId = "surveybox", target = "Values mapping")
                 hideTab(inputId = "surveybox", target = "Social values and benefits")
                 updateTabsetPanel(session, "surveybox", selected = "Values mapping")
                 updateProgressBar(session, "progress", value = 25, total = 100)
               }
  )
  
  
  
  
  
  observeEvent(input$exit, {
    removeModal()
  })
  
  
  
  # Fill in source info ----
  # observe({
  #   query <- parseQueryString(session$clientData$url_search)
  #   if (!is.null(query[['source']])) {
  #     updateTextInput(session, "source", value = query[['source']])
  #   }
  # })
  urlsource <- reactive({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['source']])) {
      data <- query[['source']]
      print(data)
      return(data)
    }
  })
  
  # Validate email ----
  iv <- InputValidator$new()
  iv$add_rule("email", sv_email())
  iv$enable()
  
  # Validate phone number ----
  iv$add_rule("phone", ~ if (!nchar(gsub("[^0-9]+", "", input$phone)) == 10)
    "Please enter a 10 digit phone number")
  
  # Validate postcode ----
  iv$add_rule("postcode", ~ if (!nchar(gsub("[^0-9]+", "", input$postcode)) == 4)
    "Please enter a valid Australian postcode")
  
  # Create a regions leaflet ----
  output$regionsmap <-
    renderLeaflet({
      leaflet(
        padding = 500,
        options = leafletOptions(zoomControl = TRUE, dragging = TRUE)) %>%
        addTiles() %>% 
        fitBounds(regionbounds[1], regionbounds[2], regionbounds[3], -32.5) %>%
        addPolygons(
          data = regions,
          layerId = regions$Reg,
          group = "unclicked_region",
          fillColor =  ~regpal(regions$Reg), #"white",
          fillOpacity = 0.5,
          
          color = "black",
          opacity = 1,
          weight = 4) %>%
        addLabelOnlyMarkers(lng = on.8$X,
                            lat = on.8$Y,
                            group = "Detailed labels",
                            labelOptions = labelOptions(noHide = F,
                                                        textsize = "12px",
                                                        textOnly = TRUE),
                            label = on.8$Name) %>%
        addLabelOnlyMarkers(lng = towns$X,
                            lat = towns$Y,
                            group = "Town labels",
                            labelOptions = labelOptions(noHide = F,
                                                        textsize = "12px",
                                                        textOnly = TRUE),
                            label = towns$Name)
    })
  
  # Add labels to regions map ----
  observeEvent(
    eventExpr = input$regionsmap_zoom, {
      print("zoom level")
      print(input$regionsmap_zoom) # Display zoom level in the console
      
      # Bigger number = more zoomed in
      if(input$regionsmap_zoom >= 14){
        leafletProxy(
          mapId = "regionsmap", 
          session = session) %>% 
          clearMarkers() %>%
          addLabelOnlyMarkers(lng = on.14$X, 
                              lat = on.14$Y,
                              group = "Detailed labels",
                              labelOptions = labelOptions(noHide = T, 
                                                          textsize = "12px", 
                                                          textOnly = TRUE),
                              label = on.14$Name) %>%
          addLabelOnlyMarkers(lng = on.12$X, 
                              lat = on.12$Y,
                              group = "Detailed labels",
                              labelOptions = labelOptions(noHide = T, 
                                                          textsize = "12px", 
                                                          textOnly = TRUE),
                              label = on.12$Name) %>%
          addLabelOnlyMarkers(lng = on.10$X, 
                              lat = on.10$Y,
                              group = "Detailed labels",
                              labelOptions = labelOptions(noHide = T, 
                                                          textsize = "12px", 
                                                          textOnly = TRUE),
                              label = on.10$Name)
      } else if(input$regionsmap_zoom >= 12) {
        
        leafletProxy(
          mapId = "regionsmap", 
          session = session) %>% 
          clearMarkers() %>%
          addLabelOnlyMarkers(lng = on.12$X, 
                              lat = on.12$Y,
                              group = "Detailed labels",
                              labelOptions = labelOptions(noHide = T, 
                                                          textsize = "12px", 
                                                          textOnly = TRUE),
                              label = on.12$Name)%>%
          addLabelOnlyMarkers(lng = on.10$X, 
                              lat = on.10$Y,
                              group = "Detailed labels",
                              labelOptions = labelOptions(noHide = T, 
                                                          textsize = "12px", 
                                                          textOnly = TRUE),
                              label = on.10$Name)
      } else if(input$regionsmap_zoom >= 10) {
        
        leafletProxy(
          mapId = "regionsmap", 
          session = session) %>% 
          clearMarkers() %>%
          addLabelOnlyMarkers(lng = on.10$X, 
                              lat = on.10$Y,
                              group = "Detailed labels",
                              labelOptions = labelOptions(noHide = T, 
                                                          textsize = "12px", 
                                                          textOnly = TRUE),
                              label = on.10$Name)
      } else if(input$regionsmap_zoom >= 8) {
        
        leafletProxy(
          mapId = "regionsmap", 
          session = session) %>% 
          clearMarkers() %>%
          addLabelOnlyMarkers(lng = on.8$X, 
                              lat = on.8$Y,
                              group = "Detailed labels",
                              labelOptions = labelOptions(noHide = T, 
                                                          textsize = "12px", 
                                                          textOnly = TRUE),
                              label = on.8$Name) %>%
          addLabelOnlyMarkers(lng = towns$X, 
                              lat = towns$Y,
                              group = "Town labels",
                              labelOptions = labelOptions(noHide = T, 
                                                           textsize = "12px", 
                                                           textOnly = TRUE),
                              label = towns$Name)
      }
    }
  )
  
  # Region data ----
  regv <- reactiveValues(
    df = SpatialPolygonsDataFrame(
      regions,
      data = data.frame(ID = as.character(c(regions$Reg)),
      display = c(1:(
        length(regions@polygons)
      ))),
      match.ID = FALSE
    ))

  # change regions colour when clicked ----
 observeEvent(input$regionsmap_shape_click, {
   
   selected.id <- input$regionsmap_shape_click
   data <- regv$df[regv$df$ID == selected.id$id, ]
   
        # execute only if the polygon has never been clicked
        if (input$regionsmap_shape_click$group == "unclicked_region") {
          
          leafletProxy("regionsmap") %>%
            removeShape(selected.id$id) %>% # remove previous occurrence
            addPolygons(
              data = data,
              layerId = data$ID,
              group = "clicked_region",
              fillColor = ~ regpal(data$ID), #"#f5b859",
              fillOpacity = 1,
              color = "white",
              weight = 4,
              opacity = 1
          )

        } else {
          
          leafletProxy("regionsmap") %>%
            removeShape(selected.id$id) %>% # remove previous occurrence
            addPolygons(
              data = data,
              layerId = data$ID,
              group = "unclicked_region",
              fillColor = ~ regpal(data$ID), #"white",
              fillOpacity = 0.5,
              color = "black",
              weight = 4,
              opacity = 1
            )

        }
      })

  # Save regions clicked - create a list ----
  regionlist <- reactiveVal(
    regionlist <- data.frame(
      id = character(0),
      group = character(0)
    )
  ) # empty dataframe

  observeEvent(input$regionsmap_shape_click, {
        id <- input$regionsmap_shape_click$id
        group <- input$regionsmap_shape_click$group

        new.dat <-
          data.frame(id, group) %>%
          mutate(number.of.times.clicked = 1)

        temp <- regionlist() # get the list of past clicks

        temp <- bind_rows(temp, new.dat) %>%
          dplyr::group_by(id) %>%
          dplyr::summarise(number.of.times.clicked = sum(number.of.times.clicked)) %>%
          dplyr::filter(number.of.times.clicked %% 2 == 1)
        
        print("regions selected")
        print(temp)
        
        regionlist(temp)

      })

  
  selected.data <- reactive({
    # Get responses to the activity accordion ----
    # create a list of all the checkbox inputs
    checkboxall <-
      c(
        unique(activity.acc$checkbox),
        unique(values.acc$checkbox),
        unique(other.acc$checkbox)
      ) 
    
    # create a dataframe with all the activity variables
    all.acc <- bind_rows(activity.acc, values.acc, other.acc)
    
    # get all checked boxes
    print("checkboxes selected")
    data <- sapply(checkboxall, function(x)
      input[[x]]) %>%
      glimpse()
    
    # turn in to a long dataframe and bind to the activity variables 
    print("checkboxes selected joined to activity data")
    data <- as.data.frame(do.call(cbind, data)) %>%
      #glimpse() %>% # works up to here with no activities selected, need to add in some empty variables to get next part to work
      mutate(checkbox_blank = "no activities selected") %>%
      pivot_longer(cols = 1:ncol(.),
                   names_to = "checkbox",
                   values_to = "nice.act") %>%
      filter(!nice.act %in%c("no activities selected")) %>%
      left_join(., all.acc) %>%
      dplyr::rename(category = Category,
                    subcategory = Sub.category,
                    activity = Activity) %>%
      dplyr::mutate(
        category = tolower(category),
        subcategory = tolower(subcategory),
        activity = tolower(activity)
      ) %>%
      distinct() %>%
      glimpse()

    return(data)
  })
  
  #### Create a map output for every Activity and Areas of conservation value that is selected ----
  # Beginning of map ----
  observeEvent(input$nextspatial, {
    activities.selected <- selected.data() %>%
      filter(!category %in% c("local_knowledge"))
    
    no.selected.activities <- nrow(activities.selected)
    
    if (no.selected.activities > 0) {
      lapply(1:nrow(activities.selected), function(i) {
        dat <- activities.selected %>%
          slice(i)
        
        category <- unique(dat$category)
        subcategory <- unique(dat$subcategory)
        activity <- unique(dat$activity)
        
        print(Sys.time())
        
        plotname <-
          paste("plot_activity", category, subcategory, activity, sep = "_")
        
        data <- SpP[SpP@data$reg %in% c(unique(regionlist()$id)),]
        
        bounds <- data %>% 
          st_bbox() %>% 
          as.character()

        output[[plotname]] <-
          renderLeaflet({
            leaflet(
              padding = 1000,
              width = "80%",
              options = leafletOptions(
                zoomControl = TRUE,
                dragging = TRUE,
                scrollWheelZoom = FALSE
              )
            ) %>%
              # suspendScroll(hoverToWake = TRUE, wakeTime = 900, sleepTime = 800, wakeMessage = "Hover or click to wake") %>%
              addmouselatlon() %>%
              addProviderTiles(
                'Esri.WorldImagery',
                group = "World Imagery",
                options = providerTileOptions(minZoom = 8, maxZoom = 14)
              ) %>%
              addTiles(group = "Open Street Map",
                       options = providerTileOptions(minZoom = 8, maxZoom = 14)) %>% 

              fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
              
              addMapPane("bathymetry", zIndex = 400) %>%
              addMapPane("polygons", zIndex = 410) %>%
              
              addGlPolygons(
                data = bathy,
                label = bathy$LABEL,
                group = "Depth",
                fillOpacity = 0.8,
                fillColor = ~ bathy.pal(bathy$LABEL),
                weight = 0,
                options = pathOptions(pane = "bathymetry")
              ) %>%

              addLayersControl(
                baseGroups = c("Open Street Map", "World Imagery"),
                overlayGroups = c("Depth"),
                options = layersControlOptions(collapsed = FALSE)) %>%
              
              addPolygons(
                data = data,
                layerId = data$ID,
                # label = data$ID,
                group = "unclicked_poly",
                fillColor = "white",
                fillOpacity = 0.2,
                color = "red",
                opacity = 0.5,
                weight = 1,
                options = pathOptions(pane = "polygons")
              ) %>%
              addLegend(
                pal = bathy.pal,
                values = bathy$LABEL,
                opacity = 0.6,
                title = "Depth",
                position = "bottomright",
                group = "Depth"
              ) %>%
              hideGroup("Depth")
          })
        
        print(Sys.time())
      })
      
    } else {
      NULL
    }
    
    values.selected <- selected.data() %>%
      filter(category %in% c("local_knowledge"))
    
    no.selected.values <- nrow(values.selected)
    
    if (no.selected.values > 0) {
      lapply(1:nrow(values.selected), function(i) {
        dat <- values.selected %>%
          slice(i)
        
        category <- unique(dat$category)
        subcategory <- unique(dat$subcategory)
        
        plotname <-
          paste("plot_values", category, subcategory, sep = "_")
        
        data <- SpP[SpP@data$reg %in% c(unique(regionlist()$id)),]
        
        bounds <- data %>% 
          st_bbox() %>% 
          as.character()
        
        output[[plotname]] <-
          renderLeaflet({
            leaflet(
              padding = 500,
              options = leafletOptions(zoomControl = TRUE, dragging = TRUE)
            ) %>%
              addmouselatlon() %>%
              addProviderTiles('Esri.WorldImagery', group = "World Imagery") %>%
              addTiles() %>% #group = "Open Street Map"
              fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
              
              addMapPane("bathymetry", zIndex = 400) %>%
              addMapPane("polygons", zIndex = 410) %>%
              
              addGlPolygons(
                data = bathy,
                label = bathy$LABEL,
                group = "Depth",
                fillOpacity = 0.6,
                fillColor = ~ bathy.pal(bathy$LABEL),
                weight = 0,
                options = pathOptions(pane = "bathymetry")
              ) %>%
              
              addLayersControl(
                baseGroups = c("Open Street Map", "World Imagery"),
                overlayGroups = c("Depth", "Town labels", "Detailed labels"),
                options = layersControlOptions(collapsed = FALSE)
              ) %>%
              addPolygons(
                data = data,
                #label = data$display,
                layerId = data$ID,
                group = "unclicked_poly",
                fillColor = "white",
                fillOpacity = 0.2,
                color = "red",
                weight = 1,
                options = pathOptions(pane = "polygons")
              ) %>%
              
              addLegend(
                pal = bathy.pal,
                values = bathy$LABEL,
                opacity = 0.6,
                title = "Depth",
                group = "Depth"
              )%>%
              hideGroup("Depth")
          })
      })
      
    } else {
      NULL
    }
    
  })
  
  # Create plot tag list and create leaflet outputs for each activity selected ----
  observeEvent(input$nextspatial, {
    output$activity_plots <- renderUI({
      activities.selected <- selected.data() %>%
        filter(!category %in% c("local_knowledge"))
      
      no.selected.activities <- nrow(activities.selected)
      
      if (no.selected.activities > 0) {
        plot_output_list_activities <-
          lapply(1:nrow(activities.selected), function(i) {
            dat <- activities.selected %>%
              slice(i)
            
            category <- unique(dat$category)
            subcategory <- unique(dat$subcategory)
            activity <- unique(dat$activity)
            
            title <- unique(dat$nice.title)
            
            daysname <-
              paste("days__activity",
                    category,
                    subcategory,
                    activity,
                    sep = "__")
            timename <-
              paste("time__activity",
                    category,
                    subcategory,
                    activity,
                    sep = "__")
            descriptionname <-
              paste("description__activity",
                    category,
                    subcategory,
                    activity,
                    sep = "__")
            plotname <-
              paste("plot_activity",
                    category,
                    subcategory,
                    activity,
                    sep = "_")
            
            box(
              title = title,
              status = "primary",
              solidHeader = TRUE,
              width = 12,
              radioButtons(
                daysname,
                width = "100%",
                label = labelMandatory(
                  paste(
                    "Estimated number of days per year that you participate in",
                    title, 
                    "across the regions you selected",
                    sep = " "
                  )
                ),
                choices = c(
                  "1 - 4",
                  "5 - 9",
                  "10 - 14",
                  "15 - 19",
                  "20 - 29",
                  "30 - 39",
                  "40 - 59",
                  "60 +"
                ),
                selected = character(0)
              ),
              checkboxGroupInput(
                timename,
                label = labelMandatory("When do most of these trips happen?:"),
                choices = c("Summer",
                            "Autumn",
                            "Winter",
                            "Spring"),
                selected = character(0)
              ),
              textAreaInput(
                descriptionname,
                width = "94%",
                label = "Description (provide more details about the activity if you like):",
                placeholder = NULL,
                height = "175px"
              ),
              
              h4(strong(paste(
                "Please click on the areas important to you for ",
                title,
                ". All cells are 2.5 km wide at the widest point. ",
                sep = ""
              )), labelMandatory("")),
              
              tags$h4(
                paste(
                  "You may click on as many areas as you like. You may also deselect an area by clicking a second time.",
                  sep = ""
                )
              ),
              
              withSpinner(
                leafletOutput(plotname, height = 600, width = "94%"),
                type = 3
              )
            )
            
          })
        
        do.call(tagList, plot_output_list_activities)
        
      } else {
        NULL
      }
      
    })
    
  })
  
  # Create plot tag list and create leaflet outputs for each value selected ----
  observeEvent(input$nextspatial, {
    output$values_plots <- renderUI({
      values.selected <- selected.data() %>%
        filter(category %in% c("local_knowledge")) 
      
      no.selected.values <- nrow(values.selected) 
      
      if (no.selected.values > 0) {
        
        print("list of values")
        plot_output_list_values <- lapply(1:nrow(values.selected), function(i) {
          dat <- values.selected %>%
            slice(i) %>%
            glimpse()
          
          title <- unique(dat$nice.title)
          
          simple.title <-  tolower(unique(dat$nice.act))
          
          category <- unique(dat$category)
          subcategory <- unique(dat$subcategory)
          
          descriptionname <-
            paste("description__values", category, subcategory, sep = "__")
          plotname <-
            paste("plot_values", category, subcategory, sep = "_")
          
          box(
            title = title,
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            textAreaInput(
              descriptionname,
              width = "94%",
              label = labelMandatory(
                paste("Please provide more details about the", simple.title ,"you would like to report on"
              )),
              placeholder = NULL,
              height = "200px"
            ),
            
            h4(strong(paste(
              "Please click on the areas important for the ",
              simple.title, " you would like to report on. All cells are 2.5 km wide at the widest point.",
              sep = ""
            )), labelMandatory("")),
            tags$h4(
              "You may click on as many areas as you like. You may also deselect an area by clicking a second time."
            ),
            withSpinner(leafletOutput(plotname, height = 600, width = "94%"), type = 3)
          )
          
        })
        
        do.call(tagList, plot_output_list_values)
        
      } else {
        NULL
      }
      
    })
    
    
  })
  
  observeEvent(input$nextactivities, {

    # change polygon colours when clicked ----
    observe ({
      activities.selected <- selected.data() %>%
        filter(!category %in% c("local_knowledge"))
      
      no.selected.activities <- nrow(activities.selected)
      
      lapply(1:no.selected.activities, function(i) {
        dat <- activities.selected %>%
          slice(i)
        
        category <- unique(dat$category)
        subcategory <- unique(dat$subcategory)
        activity <- unique(dat$activity)
        
        plotname <-
          paste("plot_activity", category, subcategory, activity, sep = "_")
        
        rv <- SpP[SpP@data$reg %in% c(unique(regionlist()$id)),]
        
        observeEvent(input[[paste0(plotname, "_shape_click", sep = "")]], {
          # execute only if the polygon has never been clicked
          if (input[[paste0(plotname, "_shape_click", sep = "")]]$group == "unclicked_poly") {
            #print("selected ID")
            selected.id <- input[[paste0(plotname, "_shape_click", sep = "")]] #%>%
              #glimpse()
            
            #print("selected")
            data <- rv[rv$ID == selected.id$id, ]
            
            change_color(
              map = plotname,
              id_to_remove =  selected.id$id,
              data = data,
              colour = "#F1BA33",
              new_group = "clicked1_poly"
            )
            
            
            
          } else {
            selected.id <- input[[paste0(plotname, "_shape_click", sep = "")]]
            data <- rv[rv$ID == selected.id$id, ]
            
            leafletProxy(plotname) %>%
            removeShape(selected.id$id) %>% # remove previous occurrence
            addPolygons(
              data = data,
              layerId = data$ID,
              group = "unclicked_poly",
              fillColor = "white",
              fillOpacity = 0.2,
              color = "red",
              weight = 1,
              options = pathOptions(pane = "polygons")
            )  # back to initialize group
            
          }
        })
        
        # Add labels to map ----
        observeEvent(
          eventExpr = input[[paste0(plotname, "_zoom", sep = "")]], {
            
            print(unique(regionlist()$id))
            on.14 <- on.14 %>% filter(Region %in% c(unique(regionlist()$id)))
            on.12 <- on.12 %>% filter(Region %in% c(unique(regionlist()$id)))
            on.10 <- on.10 %>% filter(Region %in% c(unique(regionlist()$id)))
            
            # Bigger number = more zoomed in
            if(input[[paste0(plotname, "_zoom", sep = "")]] >= 14){
              leafletProxy(
                mapId = plotname, 
                session = session) %>% 
                clearMarkers() %>%
                addLabelOnlyMarkers(lng = on.14$X, 
                                    lat = on.14$Y,
                                    group = "Detailed labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = on.14$Name) %>%
                addLabelOnlyMarkers(lng = on.12$X, 
                                    lat = on.12$Y,
                                    group = "Detailed labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = on.12$Name) %>%
                addLabelOnlyMarkers(lng = on.10$X, 
                                    lat = on.10$Y,
                                    group = "Detailed labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = on.10$Name) %>%
                addLayersControl(
                  baseGroups = c("Open Street Map", "World Imagery"),
                  overlayGroups = c("Depth", "Detailed labels"),
                  options = layersControlOptions(collapsed = FALSE))
              
            } else if(input[[paste0(plotname, "_zoom", sep = "")]] >= 12) {
              
              leafletProxy(
                mapId = plotname, 
                session = session) %>% 
                clearMarkers() %>%
                addLabelOnlyMarkers(lng = on.12$X, 
                                    lat = on.12$Y,
                                    group = "Detailed labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = on.12$Name)%>%
                addLabelOnlyMarkers(lng = on.10$X, 
                                    lat = on.10$Y,
                                    group = "Detailed labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = on.10$Name) %>%
                addLayersControl(
                  baseGroups = c("Open Street Map", "World Imagery"),
                  overlayGroups = c("Depth", "Detailed labels"),
                  options = layersControlOptions(collapsed = FALSE))
              
            } else if(input[[paste0(plotname, "_zoom", sep = "")]] >= 10) {
              
              leafletProxy(
                mapId = plotname, 
                session = session) %>% 
                clearMarkers() %>%
                addLabelOnlyMarkers(lng = on.10$X, 
                                    lat = on.10$Y,
                                    group = "Detailed labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = on.10$Name) %>%
                addLayersControl(
                  baseGroups = c("Open Street Map", "World Imagery"),
                  overlayGroups = c("Depth", "Detailed labels"),
                  options = layersControlOptions(collapsed = FALSE))
              
            } else if(input[[paste0(plotname, "_zoom", sep = "")]] >= 8) {
              
              leafletProxy(
                mapId = plotname, 
                session = session) %>% 
                clearMarkers() %>%
                addLabelOnlyMarkers(lng = on.8$X, 
                                    lat = on.8$Y,
                                    group = "Detailed labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = on.8$Name) %>%
                addLabelOnlyMarkers(lng = towns$X, 
                                    lat = towns$Y,
                                    group = "Town labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = towns$Name) %>%
                addLayersControl(
                  baseGroups = c("Open Street Map", "World Imagery"),
                  overlayGroups = c("Depth", "Town labels" ,"Detailed labels"),
                  options = layersControlOptions(collapsed = FALSE))
            }
          }
        )
      })
    })
    
    # Save polygons clicked - create a list ----
    clicklist <- reactiveVal(
      clicklist <- data.frame(
        category = character(0),
        subcategory = character(0),
        activity = character(0),
        id = character(0),
        group = character(0),
        poltime = character(0)
      )
    ) # empty dataframe
    
    observe({
      activities.selected <- selected.data() %>%
        filter(!category %in% c("local_knowledge"))
      
      no.selected.activities <- as.numeric(nrow(activities.selected))
      
      print("number of selected activities:")
      print(no.selected.activities)
      
      lapply(1:no.selected.activities, function(i) {
        dat <- activities.selected %>%
          slice(i)
        
        category <- unique(dat$category)
        subcategory <- unique(dat$subcategory)
        activity <- as.character(unique(dat$activity))
        
        plotname <-paste("plot_activity", category, subcategory, activity, sep = "_")
        
        observeEvent(input[[paste0(plotname, "_shape_click", sep = "")]], {
          id <- input[[paste0(plotname, "_shape_click", sep = "")]]$id
          group <- input[[paste0(plotname, "_shape_click", sep = "")]]$group
          
          plot <- plotname
          
          print("new data")
          new.dat <- data.frame(category, subcategory, activity, id, group) %>%
            # mutate(number.of.times.clicked = 1) %>%
            mutate(poltime = Sys.time()) %>%
            dplyr::mutate(poltime = as.character(poltime)) %>%
            distinct() #%>% glimpse()
          
          temp <- clicklist() # get the list of past clicks
          
          print("new and old data")
          temp <- bind_rows(temp, new.dat) %>%
            distinct() %>%
            mutate(number.of.times.clicked = 1) #%>% glimpse()
          
          clicklist(temp)
          
        })
      })
    })
    
    # Save polygons clicked - write to dropbox and googledrive ----
    observeEvent(input$submit, {
      # this only works up here for some reason ----
      
      if (is.null(input$visited)){
        
        shinyalert(
          "Please answer the 'Have you ever visited a marine park...' question",
          type = "error",
          timer = 3000,
          closeOnEsc = TRUE,
          closeOnClickOutside = TRUE
        )
        
      } else {
      
      # Add modal to say submitting ----
      shinyalert(
      title = "Submitting your answers...",
      text = "Please do not close this window",
      size = "s",
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      imageUrl = "https://cutewallpaper.org/21/loading-gif-transparent-background/Tag-For-Loading-Bar-Gif-Transparent-Loading-Gif-.gif",
      html = FALSE,
      # type = "warning",
      showConfirmButton = FALSE,
      timer = 0,
      animation = FALSE
    )
      
      # Sys.sleep(5)

      # Create a unique UserID ----
      userID <- randomID(1)
      
      # Create a file name, with timestamp and userID ----
      fileName <- sprintf("%s_%s.csv",
                          humanTime(),
                          userID)
      
      # Get source from url
      print("URL source")
      urlsource <- urlsource() %>% glimpse()
      
      # combine lists together ----
      # print("all clicks")
      
      click.cols <- c(category = NA_real_,
                    subcategory = NA_real_,
                    activity = NA_real_,
                    id = NA_real_,
                    number.of.times.clicked = NA_real_)
      
      clicks <- bind_rows(clicklist(), valuelist()) %>%
        distinct() %>%
        add_column(!!!click.cols[!names(click.cols) %in% names(.)]) %>%
        dplyr::group_by(category, subcategory, activity, id) %>%
        dplyr::summarise(number.of.times.clicked = sum(number.of.times.clicked)) %>%
        dplyr::filter(number.of.times.clicked %% 2 == 1) %>%
        mutate(userID = userID) %>%
        mutate(time = humanTime()) %>%
        mutate(timezone = str_replace_all(Sys.timezone(), c("[^[:alnum:]]" = ".")))# %>% glimpse()
      
      # Write polygons to database ----
      saveData(clicks, "polygons")

      # Write polygons to dropbox ----
      # Write the data to a temporary file locally
      filePath <- file.path(tempdir(), paste("polygons", fileName, sep = "_"))
      write.csv(clicks,
                filePath,
                row.names = FALSE,
                quote = TRUE)
      
      # Upload the file to Dropbox
      drop_upload(filePath, path = outputpolygonsdir, dtoken = token)
      
      # Get responses to all other questions - write to dropbox and googledrive ----
      fieldsAll <- c(fieldsAll)
      
      data <- sapply(fieldsAll, function(x)
        input[[x]])
      
      print("all data")
      data <- as.data.frame(do.call(cbind, data)) %>% distinct() %>% glimpse()
      
      vis.cols <- c(visited = NA_real_,
                    name = NA_real_,
                    email = NA_real_,
                    phone = NA_real_,
                    residence = NA_real_, 
                    postcode = NA_real_,
                    gender = NA_real_,
                    age = NA_real_,
                    frequency = NA_real_)

      
      # Get basic question answers (contact) ----
      print("metadata")
      metadata <- data %>%
        add_column(!!!vis.cols[!names(vis.cols) %in% names(.)]) %>%
        dplyr::select(name,
                      email,
                      phone,
                      residence,
                      postcode,
                      gender,
                      age,
                      visited,
                      frequency) %>%
        mutate(userID = userID) %>%
        distinct() %>%
        mutate(source = urlsource) %>%
        glimpse() # working
      
      # Format activity and value answers ----
      print("activities")
      activities <- data %>%
        add_column(!!!vis.cols[!names(vis.cols) %in% names(.)]) %>%
        # glimpse() %>%
        dplyr::select(!c(name, email, phone, residence, postcode, gender, age, visited, frequency)) %>%
        distinct() %>%
        #glimpse() %>%
        mutate(blank_activities = "blank") %>% # create a dummy column so gather works
        tidyr::gather(., "question", "answer", 1:ncol(.)) %>%
        separate(
          question,
          sep = "__",
          into = c(
            "question",
            "activity.or.value",
            "category",
            "subcategory",
            "activity"
          ),
          remove = TRUE
        ) %>%
        dplyr::filter(!answer%in%c("blank")) %>%
        glimpse()
      
      time.cols <- c(
        Summer = NA_real_,
        Autumn = NA_real_,
        Winter = NA_real_,
        Spring = NA_real_
      )
      
      print("time")
      time <- activities %>%
        dplyr::filter(question %in% c("time")) %>%
        dplyr::mutate(value = "TRUE") %>%
        distinct() %>%
        tidyr::spread(., answer, value) %>%
        mutate(userID = userID) %>%
        dplyr::select(-c(question)) %>%
        add_column(!!!time.cols[!names(time.cols) %in% names(.)]) %>%
        glimpse()
      
      activities <- activities %>%
        dplyr::filter(!question %in% c("time")) %>%
        distinct() %>%
        tidyr::spread(., question, answer) %>%
        mutate(userID = userID) %>%
        left_join(., time) %>%
        glimpse()
      
      
      q.cols <- c(activity.or.value = NA_real_, 
                  category = NA_real_, 
                  subcategory = NA_real_, 
                  activity = NA_real_, 
                  description = NA_real_,
                  days = NA_real_,
                  source = NA_real_)
      
      data <- left_join(metadata, activities) %>%
        add_column(!!!q.cols[!names(q.cols) %in% names(.)]) %>%
        dplyr::select(
          name,
          email,
          phone,
          residence,
          postcode,
          gender,
          age,
          frequency,
          visited,
          userID,
          activity.or.value,
          category,
          subcategory,
          activity,
          description,
          days,
          Summer,
          Autumn,
          Winter,
          Spring,
          source
        ) %>%
        mutate(time = humanTime()) %>%
        mutate(timezone = str_replace_all(Sys.timezone(), c("[^[:alnum:]]" = ".")))# %>%
        #glimpse()
      
      # write.csv(x = data, file = file.path(responsesDir, paste("answers", fileName, sep = "_")),
      #           row.names = FALSE, quote = TRUE)
      
      # Write answers to database ----
      saveData(data, "answers")
      
      # Write answers to dropbox ----
      # Write the data to a temporary file locally
      filePath <-
        file.path(tempdir(), paste("answers", fileName, sep = "_"))
      write.csv(data, filePath, row.names = FALSE, quote = TRUE)
      
      # Upload the file to Dropbox
      drop_upload(filePath, path = outputanswersdir, dtoken = token)
      
      # Create a bunch of empty dataframes so that the data will save even if these questions are skipped
      
      matrix9blank <- data.frame(X. = NA,
                                 stringsAsFactors = FALSE)
      
      matrix9 <-
        bind_rows(matrix9blank, as.data.frame(input$rm9)) %>% # The current level of protection and management of marine areas in the South Coast is sufficient to guarantee conservation of marine ecosystems
        dplyr::rename(The.current.level.of.protection.and.management.guarantee.conservation = X.)
      
      matrix10 <- input$rm10 #%>% glimpse()
      matrix11 <- input$rm11 #%>% glimpse()
      matrix12 <- input$rm12 #%>% glimpse()
      
      matrix.answers <-
        bind_cols(matrix9, matrix10, matrix11, matrix12) %>%
        #glimpse() %>%
        pivot_longer(
          .,
          cols = 1:ncol(.),
          names_to = "value",
          values_to = "response"
        ) %>%
        dplyr::mutate(userID = userID) %>%
        dplyr::filter(!response %in% c(NA)) %>%
        distinct() %>%
        mutate(time = humanTime()) %>%
        mutate(timezone = str_replace_all(Sys.timezone(), c("[^[:alnum:]]" = ".")))# %>%
        #glimpse()

      # Write answers to database ----
      saveData(matrix.answers, "values")
      
      # Write answers to dropbox ----
      # Write the data to a temporary file locally
      filePath <-
        file.path(tempdir(), paste("values", fileName, sep = "_"))
      write.csv(matrix.answers,
                filePath,
                row.names = FALSE,
                quote = TRUE)
      
      # Upload the file to Dropbox
      drop_upload(filePath, path = outputvaluesdir, dtoken = token)
      
      shinyjs::runjs("swal.close();")
      
      shinyalert(
        title = "Survey submitted",
        html = TRUE,
        text = tagList(
        "Thank you for providing information to inform the South Coast Marine Park planning process.",
        br(), br(),
        "For more information about the proposed South Coast Marine Park please follow ", 
        tags$a(href="https://www.dbca.wa.gov.au/parks-and-wildlife-service/plan-for-our-parks/south-coast-marine-park", "this link"),
        br(),br(),
        "Should you have information identifying values on the South Coast that are not in a suitable format for input via this tool, please contact pscmp@dbca.wa.gov.au"),
        size = "s",
        closeOnEsc = FALSE,
        closeOnClickOutside = FALSE,
        type = "success",
        showConfirmButton = FALSE,
        timer = 0,
        animation = TRUE
      )
      
      updateProgressBar(session, "progress", value = 100, total = 100)
      
      # append to googledrive ----
      data.gs <- data %>% as.list() %>% data.frame()
      sheet_append("1Rn5sCqNFWJIIecQ3vpfdh3znVP6M2fcM0__FLmtSI54", sheet = 1, data = data.gs)
      
      # append to googledrive ----
      clicks.gs <- clicks %>% as.list() %>% data.frame()
      sheet_append("1Rn5sCqNFWJIIecQ3vpfdh3znVP6M2fcM0__FLmtSI54", sheet = 2, data = clicks.gs)
      
      # append to googledrive ----
      matrix.gs <- matrix.answers %>% as.list() %>% data.frame()
      sheet_append("1Rn5sCqNFWJIIecQ3vpfdh3znVP6M2fcM0__FLmtSI54", sheet = 3, data = matrix.gs)
      
      # resetLoadingButton("submit")
      
      }
      
    })
    
    # Save polygons clicked VALUES ----
    valuelist <- reactiveVal(
      valuelist <- data.frame(
        category = character(0),
        subcategory = character(0),
        id = character(0),
        group = character(0),
        poltime = character(0)
      )
    ) # empty dataframe
    
    observe({
      values.selected <- selected.data() %>%
        filter(category %in% c("local_knowledge"))
      
      no.selected.values <- nrow(values.selected)
      
      lapply(1:no.selected.values, function(i) {
        dat <- values.selected %>%
          slice(i)
        
        category <- unique(dat$category)
        subcategory <- unique(dat$subcategory)
        
        plotname <-
          paste("plot_values", category, subcategory, sep = "_")
        
        observeEvent(input[[paste0(plotname, "_shape_click", sep = "")]], {
          id <- input[[paste0(plotname, "_shape_click", sep = "")]]$id
          group <-
            input[[paste0(plotname, "_shape_click", sep = "")]]$group
          
          plot <- plotname
          
          new.dat <-
            data.frame(category, subcategory, id, group) %>%
            mutate(poltime = Sys.time()) %>%
            dplyr::mutate(poltime = as.character(poltime)) %>%
            distinct() %>%
            glimpse()
          
          temp <- valuelist() # get the list of past clicks
          
          temp <- bind_rows(temp, new.dat) %>%
            mutate(number.of.times.clicked = 1) %>%
            glimpse()
          
          valuelist(temp)
          
        })
        
        # Add labels to map ----
        observeEvent(
          eventExpr = input[[paste0(plotname, "_zoom", sep = "")]], {
            
            print(unique(regionlist()$id))
            on.14 <- on.14 %>% filter(Region %in% c(unique(regionlist()$id)))
            on.12 <- on.12 %>% filter(Region %in% c(unique(regionlist()$id)))
            on.10 <- on.10 %>% filter(Region %in% c(unique(regionlist()$id)))
            
            # Bigger number = more zoomed in
            if(input[[paste0(plotname, "_zoom", sep = "")]] >= 14){
              leafletProxy(
                mapId = plotname, 
                session = session) %>% 
                clearMarkers() %>%
                addLabelOnlyMarkers(lng = on.14$X, 
                                    lat = on.14$Y,
                                    group = "Detailed labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = on.14$Name) %>%
                addLabelOnlyMarkers(lng = on.12$X, 
                                    lat = on.12$Y,
                                    group = "Detailed labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = on.12$Name) %>%
                addLabelOnlyMarkers(lng = on.10$X, 
                                    lat = on.10$Y,
                                    group = "Detailed labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = on.10$Name) %>%
                addLayersControl(
                  baseGroups = c("Open Street Map", "World Imagery"),
                  overlayGroups = c("Depth", "Detailed labels"),
                  options = layersControlOptions(collapsed = FALSE))
              
            } else if(input[[paste0(plotname, "_zoom", sep = "")]] >= 12) {
              
              leafletProxy(
                mapId = plotname, 
                session = session) %>% 
                clearMarkers() %>%
                addLabelOnlyMarkers(lng = on.12$X, 
                                    lat = on.12$Y,
                                    group = "Detailed labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = on.12$Name)%>%
                addLabelOnlyMarkers(lng = on.10$X, 
                                    lat = on.10$Y,
                                    group = "Detailed labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = on.10$Name) %>%
                addLayersControl(
                  baseGroups = c("Open Street Map", "World Imagery"),
                  overlayGroups = c("Depth", "Detailed labels"),
                  options = layersControlOptions(collapsed = FALSE))
              
            } else if(input[[paste0(plotname, "_zoom", sep = "")]] >= 10) {
              
              leafletProxy(
                mapId = plotname, 
                session = session) %>% 
                clearMarkers() %>%
                addLabelOnlyMarkers(lng = on.10$X, 
                                    lat = on.10$Y,
                                    group = "Detailed labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = on.10$Name) %>%
                addLayersControl(
                  baseGroups = c("Open Street Map", "World Imagery"),
                  overlayGroups = c("Depth", "Detailed labels"),
                  options = layersControlOptions(collapsed = FALSE))
              
            } else if(input[[paste0(plotname, "_zoom", sep = "")]] >= 8) {
              
              leafletProxy(
                mapId = plotname, 
                session = session) %>% 
                clearMarkers() %>%
                addLabelOnlyMarkers(lng = on.8$X, 
                                    lat = on.8$Y,
                                    group = "Detailed labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = on.8$Name) %>%
                addLabelOnlyMarkers(lng = towns$X, 
                                    lat = towns$Y,
                                    group = "Town labels",
                                    labelOptions = labelOptions(noHide = T, 
                                                                textsize = "12px", 
                                                                textOnly = TRUE),
                                    label = towns$Name) %>%
                addLayersControl(
                  baseGroups = c("Open Street Map", "World Imagery"),
                  overlayGroups = c("Depth", "Town labels" ,"Detailed labels"),
                  options = layersControlOptions(collapsed = FALSE))
            }
          }
        )
        
      })
    })
    
    observe ({
      values.selected <- selected.data() %>%
        filter(category %in% c("local_knowledge"))
      
      no.selected.values <- nrow(values.selected)
      
      lapply(1:no.selected.values, function(i) {
        dat <- values.selected %>%
          slice(i)
        
        category <- unique(dat$category)
        subcategory <- unique(dat$subcategory)
        
        plotname <-
          paste("plot_values", category, subcategory, sep = "_")
        
        rv <- SpP[SpP@data$reg %in% c(unique(regionlist()$id)),]
        
        observeEvent(input[[paste0(plotname, "_shape_click", sep = "")]], {
          # execute only if the polygon has never been clicked
          if (input[[paste0(plotname, "_shape_click", sep = "")]]$group == "unclicked_poly") {
            selected.id <- input[[paste0(plotname, "_shape_click", sep = "")]]
            data <- rv[rv$ID == selected.id$id, ]
            
            change_color(
              map = plotname,
              id_to_remove =  selected.id$id,
              data = data,
              colour = "#F1BA33",
              new_group = "clicked1_poly"
            )
          } else {
            selected.id <- input[[paste0(plotname, "_shape_click", sep = "")]]
            
            data <- rv[rv$ID == selected.id$id, ]
            
            leafletProxy(plotname) %>%
            removeShape(selected.id$id) %>% # remove previous occurrence
            addPolygons(
              data = data,
              #label = as.character(data$display),
              layerId = data$ID,
              group = "unclicked_poly",
              fillColor = "white",
              fillOpacity = 0.2,
              color = "red",
              weight = 1,
              opacity = 1,
              options = pathOptions(pane = "polygons")
            )  # back to initialize group
          }
        })
      })
    })
    
    # Create submit button with loading ----
    output$submit <- renderUI({
      actionBttn(
        inputId = "submit",
        label = "Submit",
        style = "unite",
        size = "md",
        # icon = icon("chevron-up"),
        color = "primary"
      )
      
    })
    
  })
  
}
