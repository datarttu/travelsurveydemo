library(shiny)
library(shinyTime)
# library(leaflet)
# library(leaflet.extras)
# library(rhandsontable)

activityBoxInput <- function(id, label='Activity', isfirst=FALSE) {
  ns <- NS(id)
  
  activ.choices <- c('Home', 'Work', 'Study', 'Food purchase', 'Shopping', 'Sport', 'Leisure', 'Run errands')
  mode.choices <- c('Walk', 'Bicycle', 'Citybike', 'Car: driver', 'Car: passenger',
                    'Bus', 'Tram', 'Metro', 'Regional train')
  freq.choices <- c('Daily or almost daily', '4-5 times a week', '2-3 times a week', 'Once a week', 'Less than once a week')
  
  if (isfirst==TRUE) {
    div(id=id,
        h4(label),
        fluidRow(
          column(6, selectizeInput(ns('type'), label='Activity type', choices=activ.choices, options=list(create=TRUE))),
          column(6, textInput(ns('address'), label='Address and city of the activity', placeholder='e.g. Otakaari 1, Espoo'))
        ),
        fluidRow(
          column(6, selectInput(ns('visitFreq'), label='How often do you visit this location?', choices=freq.choices)),
          column(6, timeInput(ns('depTime'), label='Departure time from this activity', seconds=FALSE))
        ),
        tags$hr()
    )
  } else {
    div(id=id,
        h4(label),
        fluidRow(
          column(6, selectizeInput(ns('modesTo'), label='Modes used on the way to this activity',
                                   choices=mode.choices, multiple=TRUE, options=list(create=TRUE))),
          column(6, timeInput(ns('arrTime'), label='Arrival time to this activity', seconds=FALSE))
        ),
        fluidRow(
          column(6, selectizeInput(ns('type'), label='Activity type', choices=activ.choices, options=list(create=TRUE))),
          column(6, textInput(ns('address'), label='Address and city of the activity', placeholder='e.g. Otakaari 1, Espoo'))
        ),
        fluidRow(
          column(6, selectInput(ns('visitFreq'), label='How often do you visit this location?', choices=freq.choices)),
          column(6, timeInput(ns('depTime'), label='Departure time from this activity', seconds=FALSE))
        ),
        tags$hr()
    )
  }
}

activityBox <- function(input, output, session) {
  values <- reactiveValues()
  
  # Consider "isfirst" box by if statements
  
  if('arrTime' %in% names(input)) {
    values$arrTime <- input$arrTime
  } else {
    values$arrTime <- ''
  }
  
  if('modesTo' %in% names(input)) {
    values$modesTo <- input$modesTo
  } else {
    values$modesTo <- ''
  }
  
  values$type <- input$type
  values$address <- input$address
  values$visitFreq <- input$visitFreq
  values$depTime <- input$depTime
  
  return(values)
}

ui <- navbarPage(
  'Travel survey tool demo', 
  id='mainNav',
  
  tabPanel('Introduction', value='intro',
           fluidRow(tags$p('Welcome to a demo of a travel survey for Otaniemi area!'),
                    tags$p(),
                    tags$p('Please prepare to tell about your activities and trips you made between them
             on a selected day. First you will be asked some background details.
             As this is only a demo, your data will not be saved on a server.
             After completing the next phases, you will be able to download your answers
             as a JSON file showing the format in which they would be saved in real use.'),
                    tags$hr(),
                    tags$i('This demo is built by Arttu Kosonen for the course Transport Modelling 
                           (SPT-E4010) at Aalto University in Spring 2019.'),
                    tags$hr(),
                    tags$strong('Notes:'),
                    tags$li('Trips do not appear in the most intuitive way at the moment:
                            they could be separate entities between activities'),
                    tags$li('Instead of or alternatively to giving an address for each activity
                            (and later geocoding it), it could be fairly easy to implement
                            a Leaflet map UI for activity geolocating'),
                    tags$li('It should indeed be possible to add and remove activities
                            "in between", now it works after the last activity only'),
                    tags$li('There are some technical bugs, e.g. the date selection calendar
                            on top of the activities appears incorrectly on the left edge of the page
                            when clicked'),
                    tags$hr()
           ),
           fluidRow(actionButton('nextFromIntro', 'Next'))
  ),
  
  tabPanel('Respondent details', value='resp',
           fluidRow(
             radioButtons('respLivesInArea', label='Do you live in Otaniemi?',
                          choices=c('No', 'Yes'), inline=TRUE),
             radioButtons('respWorksInArea', label='Do you work or study regularly in Otaniemi?',
                          choices=c('No', 'Yes'), inline=TRUE),
             radioButtons('respHasMoved', 
                          label='Have you moved to a new area of residence during the last four weeks?',
                          choices=c('No', 'Yes'), inline=TRUE),
             radioButtons('respWorkplaceChanged', 
                          label='Has your work or study location changed to a new area during the last four weeks?',
                          choices=c('No', 'Yes'), inline=TRUE),
             tags$hr(),
             numericInput('respAge', label='Age', value=0, min=0, step=1),
             radioButtons('respSex', label='Sex',
                          choices=c('Female', 'Male', 'Other', 'N/A'),
                          selected='N/A', inline=TRUE),
             selectInput('respOccupation', label='Work status',
                         choices=c('Full-time work', 'Student', 'Student + part-time work',
                                   'Unemployed', 'Parental leave', 'Retired', 'N/A'),
                         selected='N/A'),
             radioButtons('respMarital', label='Marital status',
                          choices=c('Single', 'In a relationship or married', 'N/A'),
                          selected='N/A', inline=TRUE),
             numericInput('respChildrenN', label='Number of children', value=0, min=0, step=1),
             numericInput('respPersIncome', label='Personal income per year (€)', value=0, min=0, step=1000),
             numericInput('respHouseholdIncome', label='Household income per year (€)', value=0, min=0, step=1000),
             tags$hr(),
             tags$i('For the following question, 
                    you can type and add new items if they do not appear in the list.
                    Click an item and press backspace to remove it.'),
             selectizeInput('respMobilityTools', label='Mobility tools you own and use at least on a monthly basis', 
                            multiple=TRUE, options=list(create=TRUE, placeholder='Possible to add multiple items'),
                            choices=c('Bicycle', 'Car', 'Driving license', 'Public transport season ticket',
                                      'Public transport value ticket', 'Public tranport ticket app',
                                      'Motorcycle', 'Moped or scooter',
                                      'Car-sharing membership')),
             tags$i('For the following question, 
                    consider activities', tags$strong('outside'),
                    'of your home, workplace or study place.
                    List activities you carry on at least on a monthly basis.
                    There are only some example options on the list:
                    feel free to type and add items of your own.'),
             selectizeInput('respFreetimeActivities', label='Free-time activities',
                            choices=c('Sports outside', 'Sports inside',
                                      'Student organization events', 'Going to a restaurant',
                                      'Shopping'),
                            multiple=TRUE, options=list(create=TRUE, placeholder='Possible to add multiple items'))
           ),
           fluidRow(
             actionButton('prevFromResp', 'Previous'),
             actionButton('nextFromResp', 'Next')
           ),
           fluidRow(tags$div(class='bottomSpacer'))
  ),
  
  tabPanel('Activities and trips', value='activs',
           fluidRow(
             column(4, dateInput('activDate', label=NULL, value=Sys.Date()-1, max=Sys.Date(),
                                 format='dd.mm.yyyy', weekstart=1)),
             column(8,
                    tags$p('Select a date of your choice on the left,
and consider the activities you took on that day, and the trips between them. 
Try to remember the locations of your activities as accurately as you can,
                           as well as when you arrived at and departed from them.'),
                    tags$p('Note that if none of the activity types or travel modes provided
                           does not seem suitable, you can type and add one of your own.')
             )
           ),
           fluidRow(
             activityBoxInput('activ1', label='Activity #1', isfirst=TRUE)
           ),
           fluidRow(
             actionButton('addNewActiv', label='Add new activity'),
             actionButton('removeLastActiv', label='Remove last activity'),
             tags$hr()
           ),
           fluidRow(
             actionButton('prevFromActivs', 'Previous'),
             actionButton('nextFromActivs', 'Next')
           ),
           fluidRow(tags$div(class='bottomSpacer'))
  ),
  
  tabPanel('Save results', value='final',
           fluidRow(h4('Thank you for your participation!')),
           fluidRow(column(4,
                           downloadButton(outputId='saveJson',
                                          label='Download data as JSON')),
                    column(8,
                           p('You can download your answer data as JSON on the left.
                             In this version, the data is not saved on the server.'))),
           fluidRow(
             actionButton('prevFromFinal', 'Previous')
           )
  ),
  
  tags$head(
    tags$style(
      'body{min-height: 600px; height: auto; max-width: 800px; margin: auto;}
      .bottomSpacer{height: 100px;}
      .btn{background-color: steelblue; color: white;}
      #addNewActiv{background-color: limegreen;}
      #removeLastActiv{background-color: orangered;}')
  )
)

server <- function(input, output, session) {
  # List for data from user ----
  usrdata <- list(
    respDetails = list(),
    activDate = strftime(Sys.Date()),
    activs = list(activ1=reactiveValues(
      arrTime='', modesTo='', type='', address='', visitFreq='', depTime=''))
    # activs = list(activ1=callModule(activityBox, 'activ1'))
  )
  
  # Main tabset navigation ----
  observeEvent(input$nextFromIntro, updateTabsetPanel(session, 'mainNav', selected='resp'))
  observeEvent(input$prevFromResp, updateTabsetPanel(session, 'mainNav', selected='intro'))
  observeEvent(input$nextFromResp, updateTabsetPanel(session, 'mainNav', selected='activs'))
  observeEvent(input$prevFromActivs, updateTabsetPanel(session, 'mainNav', selected='resp'))
  observeEvent(input$nextFromActivs, updateTabsetPanel(session, 'mainNav', selected='final'))
  observeEvent(input$prevFromFinal, updateTabsetPanel(session, 'mainNav', selected='activs'))
  
  
  # Activities / trips ----
  
  # Add new activity
  observeEvent(input$addNewActiv, {
    last.name <- names(usrdata$activs[length(usrdata$activs)])
    # print(paste('Adding after', last.name)) # DEBUG
    i <- length(usrdata$activs) + 1
    new.name <- sprintf('activ%d', i)
    insertUI(
      selector=paste0('#', last.name),
      where='afterEnd',
      ui=activityBoxInput(new.name, label=sprintf('Activity #%d', i))
    )
    usrdata$activs[[new.name]] <<- callModule(activityBox, new.name)
    # print(paste('Inserted as', new.name)) # DEBUG
  })
  
  # Remove last activity
  observeEvent(input$removeLastActiv, {
    # print('Removing') # DEBUG
    if (length(usrdata$activs) == 1) {
      showNotification('Cannot remove first activity.', type='error')
      return()
    } else {
      last.name <- names(usrdata$activs[length(usrdata$activs)])
      # print(paste('Removing', last.name)) # DEBUG
      removeUI(selector=paste0('#', last.name))
      usrdata$activs[[last.name]] <<- NULL
      # print(usrdata$activs) # DEBUG
    }
  })
  
  # JSON download ----
  output$saveJson <- downloadHandler(
    filename = function() {
      paste0('data-', Sys.Date(), '.json')
    },
    content = function(con) {
      writeLines('{}', con)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)

