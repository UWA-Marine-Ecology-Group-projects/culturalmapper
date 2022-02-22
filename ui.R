function(request) {
  dashboardPage(
    dashboardHeader(titleWidth = "0px",#disable = TRUE
                    # title = "South Coast Marine Values Mapper",
                    tags$li(class = "dropdown",
                            tags$a(href="https://etntac.com.au/", target="_blank", 
                                   tags$img(height = "70px", alt="logo", src="etntac-logo.svg")
                            )
                            )#,
                    #titleWidth = 1
    ),
    dashboardSidebar(width = "0px"),
    dashboardBody(
      tags$head(tags$style(HTML(
        '.myClass { 
        font-size: 25px;
        line-height: 100px;
        text-align: left;
        font-family: "Source Sans Pro",sans-serif;
        padding: -0 10px;
        overflow: hidden;
        color: white;
      }
    '))),
      
      tags$head(HTML('<script type="text/javascript" src="https://webapiv2.navionics.com/dist/webapi/webapi.min.no-dep.js"></script>
<link rel="stylesheet" href="https://webapiv2.navionics.com/dist/webapi/webapi.min.css" >')),
      
      tags$body(HTML('<body data-root="https://webapiv2.navionics.com/dist/webapi/images" >')),
      
      tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> <b> Cultural Mapper </b> </span>\');
      })
     ')),
      
      shinyjs::useShinyjs(), # Have to put here in dashboard SEE https://cran.r-project.org/web/packages/shinyjs/vignettes/shinyjs-usage.html
      useShinyalert(), # To show shiny alerts
      shinyjs::inlineCSS(appCSS), # for mandatory star
      # tags$head(
      #   tags$link(rel = "stylesheet", href = "leaflet-gesture-handling.css"),
      #   tags$script(src="leaflet-gesture-handling.js")),
      
      disconnectMessage(
        text = "An error occurred. Please refresh the page and try again.",
        refresh = "Refresh",
        background = "#FFFFFF",
        colour = "#444444",
        refreshColour = "#337AB7",
        overlayColour = "#000000",
        overlayOpacity = 0.6,
        width = "full",
        top = "center",
        size = 22,
        css = ""
      ),
      
      # tabItems(
      #   tabItem(tabName = "survey",
      fluidRow(
        
        tabBox(title = " ",
               tags$head(tags$style(HTML('
 .nav-tabs-custom>.nav-tabs>li.active>a {
display: none;
 }
 
 .skin-blue .main-header .navbar .sidebar-toggle {
 display: none;
}

label {
  margin-left: 30px;
  margin-right: 30px;
}

.form-control {
  margin-left: 30px;
  margin-right: 30px;
}

.help-block {
  margin-left: 30px;
  margin-right: 30px;
}

h1 {
  margin-left: 30px;
  margin-right: 30px;
  margin-bottom: 10px;
  font-size: 22px;
}

h2 {
  margin-left: 30px;
  margin-right: 30px;
  margin-bottom: 10px;
  font-size: 18px;
}

h3 {
  margin-left: 60px;
  margin-right: 30px;
  margin-bottom: 10px;
  font-size: 16px;
}

.h4, h4 {
  font-size: 14px;
  margin-left: 30px;
  margin-right: 30px;
  margin-bottom: 15px;
}

.h5, h5 {
  font-size: 18px;
}

.h6, h6 {
  font-size: 16px;
}

.leaflet-container {
margin-left: 30px;
margin-right: 30px;
}

.box-group accordion shiny-bound-input {
margin-left: 30px;
margin-right: 30px;
}

p {
    margin: 0 0 10px;
    margin-left: 30px;
    margin-right: 30px;
}

.progress-group {
    margin-left: 30px;
    margin-right: 30px;
}

.bttn-unite.bttn-lg {
    padding: 10px 50px;
    margin-right: 30px;
    margin-left: 30px;
    margin-top: 10px;
}

.bttn-unite.bttn-md {
    padding: 10px 30px;
}

.skin-blue sidebar-mini{
display: none;
}

td {
    width: 100px;
}

.modal-dialog {
    border-radius: 20px;
}

.modal-body {
    border-radius: 20px;
}

.modal-content {
    border-radius: 20px;
}

.skin-blue sidebar-mini {
    padding-right: 0px;
}

html, body, .test_map_div {
    margin: 0;
    width: 100%;
    height: 100%;
}

'))),
               id = "surveybox",
               height = "100%", width = "100%",
               tabPanel("Contact information", 
                        
                        h1("Please fill out your contact information:"),
                        
                        h4(strong("Full name:"), labelMandatory("")),
                        textInput(width = '94%', "name", NULL),
                        
                        h4(strong("Email:"), labelMandatory("")),
                        textInput(width = '94%', "email", NULL),
                        
                        h4(strong("Phone number:"), labelMandatory("")),
                        textInput(width = '94%', "phone", NULL),
                        
                        
                        # DEMOGRAPHICS ----
                        h2("Demographics:"),
                        
                        h4(strong("Family Group:"), labelMandatory("")),
                        radioButtons("family", label = NULL,
                                     choices = c("Dabb",
                                                 "Reynolds",
                                                 "Bullen",
                                                 "Boxer/Rogers",
                                                 "Tucker",
                                                 "Yorkshire/Knapp"),
                                     selected = character(0)),
                        
                        h4(strong("Usual place of residence:"), labelMandatory("")),
                        radioButtons("residence", label = NULL,
                                     choices = c("Australia",
                                                 "Overseas"),
                                     selected = character(0)),
                        
                        conditionalPanel('input.residence == "Australia"',
                                         h4(strong("What is your home postcode?"), labelMandatory("")),
                                         textInput(width = '100%', "postcode", label = NULL)
                        ),
                        
                        h4(strong("What is your gender?"), labelMandatory("")),
                        radioButtons("gender", label = NULL,
                                     choices = c("Male",
                                                 "Female",
                                                 "Other"),
                                     selected = character(0)),
                        
                        h4(strong("To which age group do you belong?"), labelMandatory("")),
                        radioButtons("age", label = NULL,
                                     choices = c("0-17",
                                                 "18-24",
                                                 "25-34",
                                                 "35-44",
                                                 "45-54",
                                                 "55-64",
                                                 "65-74",
                                                 "75+"),
                                     selected = character(0)),
                        
                        div(style="display:inline-block;width:100%;text-align: center;", 
                            
                            div(
                              column(1,offset=10, actionBttn(
                                inputId = "nextactivities",
                                label = "Next",
                                style = "unite",
                                icon = icon("chevron-right"),
                                color = "primary"
                              )))
                        )),
               tabPanel("Values mapping",
                        h1("Values mapping"),
                        br(),
                        h2("In which areas would you like to map your use or local knowledge? Please select", tags$strong("all"), "of the areas where you wish to provide information."),
                        # h2("Please select", strong("all"), "of the areas where you wish to provide information"),
                        withSpinner(leafletOutput("regionsmap", height = 500, width = "94%"), type = 3),
                        br(),
                        br(),
                        h2("Which cultural values would you like to map?"),
                        # Activity 1 ----
                        accordion(id = "id-accordion1",
                                  accordionItem(
                                    id = "accordion1",
                                    title =  unique(filter(cultural_acc, cat_num %in% c("1"))$nice.cat),
                                    solidHeader = TRUE, status = "primary",
                                    do.call(accordion, c(list(id = "id-accordionv1"), 
                                                         lapply(seq_along(unique(filter(cultural_acc, cat_num %in% c("1"))$Sub.category)), function(x){
                                                           accordionItem(
                                                             title = paste0(unique(filter(cultural_acc, cat_num %in% c("1"))$nice.sub)[x]),
                                                             checkboxGroupInput(paste0("checkbox_",
                                                                                       unique(cultural_acc$Category)[1], "__",
                                                                                       unique(filter(cultural_acc, cat_num %in% c("1"))$Sub.category)[x]),
                                                                                label = NULL,
                                                                                choices = (filter(cultural_acc, cat_num == "1" & sub_num ==x)$nice.act),
                                                                                selected = character(0))
                                                           )
                                                         })))
                                  )),
                        
                        # Activity 2 ----
                        accordion(id = "id-accordion2",
                                  accordionItem(
                                    id = "accordion2",
                                    title =  unique(filter(cultural_acc, cat_num %in% c("2"))$nice.cat),
                                    solidHeader = TRUE, status = "primary",
                                    do.call(accordion, c(list(id = "id-accordionv2"), 
                                                         lapply(seq_along(unique(filter(cultural_acc, cat_num %in% c("2"))$Sub.category)), function(x){
                                                           accordionItem(
                                                             title = paste0(unique(filter(cultural_acc, cat_num %in% c("2"))$nice.sub)[x]),
                                                             checkboxGroupInput(paste0("checkbox_",
                                                                                       unique(cultural_acc$Category)[2], "__",
                                                                                       unique(filter(cultural_acc, cat_num %in% c("2"))$Sub.category)[x]),
                                                                                label = NULL,
                                                                                choices = (filter(cultural_acc, cat_num == "2" & sub_num ==x)$nice.act),
                                                                                selected = character(0))
                                                           )
                                                         })))
                                  )),
                        
                        # Activity 3 ----
                        accordion(id = "id-accordion3",                                            accordionItem(
                          id = "accordion3",
                          title = unique(filter(cultural_acc, cat_num %in% c("3"))$nice.cat),
                          solidHeader = TRUE, status = "primary",
                          checkboxGroupInput(paste0("checkbox_",
                                                    unique(filter(cultural_acc, cat_num %in% c("3"))$Category)),
                                             label = NULL,
                                             choices = unique(filter(cultural_acc, cat_num %in% c("3"))$nice.sub),
                                             selected = character(0))
                        )),
                        
                        # Activity 4 ----
                        accordion(id = "id-accordion4",                                            accordionItem(
                          id = "accordion4",
                          title = unique(filter(cultural_acc, cat_num %in% c("4"))$nice.cat),
                          solidHeader = TRUE, status = "primary",
                          checkboxGroupInput(paste0("checkbox_",
                                                    unique(filter(cultural_acc, cat_num %in% c("4"))$Category)),
                                             label = NULL,
                                             choices = unique(filter(cultural_acc, cat_num %in% c("4"))$nice.sub),
                                             selected = character(0))
                        )),
                        
                        
                        # Activity 5 ----
                        accordion(id = "id-accordion5",                                            accordionItem(
                          id = "accordion5",
                          title = unique(filter(cultural_acc, cat_num %in% c("5"))$nice.cat),
                          solidHeader = TRUE, status = "primary",
                          checkboxGroupInput(paste0("checkbox_",
                                                    unique(filter(cultural_acc, cat_num %in% c("5"))$Category)),
                                             label = NULL,
                                             choices = unique(filter(cultural_acc, cat_num %in% c("5"))$nice.sub),
                                             selected = character(0))
                        )),
                        
                        # Activity 6 ----
                        accordion(id = "id-accordion6",                                            accordionItem(
                          id = "accordion6",
                          title = unique(filter(cultural_acc, cat_num %in% c("6"))$nice.cat),
                          solidHeader = TRUE, status = "primary",
                          checkboxGroupInput(paste0("checkbox_",
                                                    unique(filter(cultural_acc, cat_num %in% c("6"))$Category)),
                                             label = NULL,
                                             choices = unique(filter(cultural_acc, cat_num %in% c("6"))$nice.sub),
                                             selected = character(0))
                        )),
                        
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        br(),
                        
                        # Other activities ----
                        h2("Use an other option if you would like to map anything not listed above. You will be asked to describe the activity or knowledge on the next page."),
                        accordion(id = "id-accordion7",
                                  accordionItem(
                                    id = "accordionother",
                                    title = unique(other_acc$nice.cat),
                                    solidHeader = TRUE, status = "primary",
                                    checkboxGroupInput(paste0("checkbox_",
                                                              unique(other_acc$Category)),
                                                       label = NULL,
                                                       choices = unique(other_acc$nice.act),
                                                       selected = character(0))
                                  )),
                        
                        # Pressures ----
                        h2("Which pressures or threats would you like to map?"),
                        accordion(id = "id-accordion8",
                                  accordionItem(
                                    id = "accordionpresures",
                                    title =  unique(filter(pressures_acc, cat_num %in% c("8"))$nice.cat),
                                    solidHeader = TRUE, status = "primary",
                                    do.call(accordion, c(list(id = "id-accordionp8"), 
                                                         lapply(seq_along(unique(filter(pressures_acc, cat_num %in% c("8"))$Sub.category)), function(x){
                                                           accordionItem(
                                                             title = paste0(unique(filter(pressures_acc, cat_num %in% c("8"))$nice.sub)[x]),
                                                             checkboxGroupInput(paste0("checkbox_",
                                                                                       unique(pressures_acc$Category), "__",
                                                                                       unique(filter(pressures_acc, cat_num %in% c("8"))$Sub.category)[x]),
                                                                                label = NULL,
                                                                                choices = (filter(pressures_acc, cat_num == "8" & sub_num ==x)$nice.act),
                                                                                selected = character(0))
                                                           )
                                                         })))
                                  )),
                        br(),
                        br(),
                        br(),
                        div(style="display:inline-block;width:100%;text-align: center;", 
                            
                            div(
                              column(1,offset=1, actionBttn(
                                inputId = "backcontact",
                                label = "Back",
                                style = "unite",
                                icon = icon("chevron-left"),
                                color = "primary"
                              )), 
                              column(1,offset = 8, actionBttn(
                                inputId = "nextspatial",
                                label = "Next",
                                style = "unite",
                                icon = icon("chevron-right"),
                                color = "primary"
                              )))
                        )
               ),
               
               tabPanel("Spatial questions",
                        h1("Map your use and/or local knowledge"),
                        h2("There are separate maps for each activity and knowledge topic you selected"),
                        uiOutput("activity_plots"),
                        uiOutput("values_plots"),
                        div(style="display:inline-block;width:100%;text-align: center;", 
                            uiOutput("submit")),
               )
               
               
               ),
        progressBar(id = "progress", value=0, total=100, display_pct = FALSE, title = "Progress")
      )
    )
  )
}
# )
# )