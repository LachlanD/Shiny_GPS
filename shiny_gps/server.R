#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggmap)
library(sf)
library(grid)
library(gridExtra)

#Too big to preload load dynamically instead
#load("layers.RData")

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    
    # The selected file, if any
    userFile <- reactive({
        validate(need(input$gpx, message = FALSE))
        input$gpx
    })
    
    trail <- reactive({
        trail<-data.frame()
        trail <- sf::st_read(userFile()$datapath, layer = "track_points")
        d<-sf::st_distance(trail)
        
        acc<-numeric(nrow(d))
        for (i in 1:(nrow(d)-1)){
            acc[i+1]<-(as.numeric(d[i+1,i])+acc[i])  
        }
        
        trail$Cummlative_distance<-acc
        
        load("geology_layer.RData")
        
        trail<-st_transform(trail, st_crs(gsf))
        sf::sf_use_s2(FALSE)
        
        trail<-sf::st_join(trail, gsf, suffix=c("","GEO"))
        rm(gsf)
        gc()
        #load("vegetation_layer.RData")
        #trail<-sf::st_join(trail, bsf, suffix=c("","VEG"))
        #rm(bsf)
        #gc()
        
        trail$GEO_GRP<-1
        #print(trail$DESC)
        d<-trail[1,]$DESC
        grp<-1
        for(i in 1:nrow(trail)-1){
            if(!(d %in% trail[i+1,]$DESC)){
                grp<-grp+1
                d<-trail[i+1,]$DESC
            } 
            trail[i+1,]$GEO_GRP<-grp
        }
        
        trail
    })
    
    trail_co<- reactive({
        trail_co<-as.data.frame(st_coordinates(trail()))
        trail_co$Elevation<-trail()$ele
        trail_co
    })
    
    gg_map<-reactive({
        validate(need(trail_co(), message = FALSE))
        
        b<-make_bbox(lon = trail_co()$X, lat=trail_co()$Y, f=0.1)
        map<-get_map(location = b, maptype = "satellite", source = "google")
            
        ggmap(map) + geom_point(data = trail_co(), mapping = aes(x = X, y = Y, colour=Elevation))
        
    })
    
    observe({
        val<- nrow(trail())
        # Control the value, min, max, and step.
        # Step size is 2 when input value is even; 1 when value is odd.
        updateSliderInput(session, "range",  value = c(0, val), max = val)
    })
    
    
    
    output$distMap <- renderPlot({
        #print(trail_co())
        
        A <- input$range[1]
        B <- input$range[2]
        
        
        if (is.null(input$gpx) )
        {
            b<-make_bbox(lon = c(-180,180), lat=c(-70,70), f=0)
            map<-get_map(location = b, maptype = "satellite", source = "google")
            ggmap(map)
            
        } else {
            start<-trail_co()[A,]
            end<-trail_co()[B,]
        
            gg_map() + 
                geom_point(data = start, mapping = aes(x = X, y = Y), colour="green", alpha=0.3, size = 4) +
                geom_point(data = end, mapping = aes(x = X, y = Y), colour="red", alpha=0.3, size = 4) +
                labs(x = "Longitude", y = "Latitude", colour = "Elevation (m)")
        }
            
        

    })
    
    
    output$distPlot <- renderPlot({
        validate(need(trail(), message = FALSE))
        
        A <- input$range[1]
        B <- input$range[2]
        
        dat <- trail()[A:B,]
        
        ggplot(data = dat, aes(fill=DESC, group = GEO_GRP)) + 
            geom_ribbon(aes(x=Cummlative_distance, ymin=-10, ymax=ele)) + 
            theme(legend.position="bottom", legend.direction = "vertical")
        
    })
    
})
