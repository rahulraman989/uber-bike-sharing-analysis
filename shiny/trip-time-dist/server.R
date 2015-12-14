library(shiny)
library(plotly)

trip <- readRDS('./rds/trip.rds')

shinyServer(function(input, output, session) {
  
  observe({
    updateSelectInput(session, 'start', 'Start Station', 
                      choices=getStationLabel(input$city))
    updateSelectInput(session, 'end', 'End Station', 
                      choices=getStationLabel(input$city))
  })
  
  output$plot <- renderPlotly({
    
    trip[trip$type == input$type &
           trip$s.terminal == input$start & 
           trip$e.terminal == input$end, ] %>% 
      plot_ly(type='histogram',
              x=duration.m,
              text=sapply(seq(from=round(min(duration.m),0) - round(min(duration.m),0) %% 5, 
                              to=max(duration.m) + 5, 
                              by=5),
                          FUN=function(i) paste0('[', i, '-', i+5, 'mins]')),
              autobinx=F,
              xbins=list(start=0,
                         end=max(duration.m)+5,
                         size=5)) %>%
      layout(title=paste('From', s.terminal[1], ':', s.station[1], '<br>',
                          'To', e.terminal[1], ':', e.station[1]),
             xaxis=list(title='Trip Time (minutes)'),
             yaxis=list(title='Frequency'))
    
  })
  
})