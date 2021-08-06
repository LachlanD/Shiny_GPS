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
library(lwgeom)
library(ggmap)
library(sf)
library(grid)
library(gridExtra)

#Too big to preload load dynamically instead
#load("layers.RData")

#load("geology_layer.RData")
#load("vegetation_layer.RData")
sf_use_s2(FALSE)

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
        #trail<-st_transform(trail, 54032)
        d<-sf::st_distance(trail)
        
        acc<-numeric(nrow(d))
        for (i in 1:(nrow(d)-1)){
            acc[i+1]<-(as.numeric(d[i+1,i])+acc[i])  
        }
        
        trail$Cummlative_distance<-acc
        
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
    
    
    
    
    
    geology<-reactive({
        validate(need(trail(), message=FALSE))
        
        #Dynamic loading for less memory usage
        load("geology_layer.RData")
        geology<-sf::st_join(trail(), gsf, join = st_nearest_feature)
        rm(gsf)
        gc()
        
        geology<- subset(geology, select = c(Cummlative_distance, ele, DESC, LITHOLOGY, GEOLHIST)) 
                
        geology$GEO_GRP<-1
                
        d<-geology[1,]$DESC
        grp<-1
        for(i in 1:nrow(geology)-1){
            if(!(d %in% geology[i+1,]$DESC)){
                grp<-grp+1
                d<-geology[i+1,]$DESC
            } 
                geology[i+1,]$GEO_GRP<-grp
        }
        geology
    })
    
    veg<-reactive({
        validate(need(trail(), message=FALSE))
        
        #Dynamic loading for less memory usage
        load("vegetation_layer.RData")
        veg<-sf::st_join(trail(), bsf, join = st_nearest_feature)
        rm(bsf)
        gc()
        
        veg<- subset(veg, select = c(Cummlative_distance, ele, EVC, XGROUPNAME, XSUBGGROUP, X_EVCNAME)) 
        
        veg$VEG_GRP<-1
        
        d<-veg[1,]$EVC
        grp<-1
        for(i in 1:nrow(veg)-1){
            if(!(d %in% veg[i+1,]$EVC)){
                grp<-grp+1
                d<-veg[i+1,]$EVC
            } 
            veg[i+1,]$VEG_GRP<-grp
        }
        veg
    })
    
    
    observe({
        val<- nrow(trail())
        
        #print(val)
        updateSliderInput(session, "range",  value = c(0, val), max = val)
    })
    
    observeEvent(input$plot_brush,{
        val<-input$plot_brush
        t<-trail()
        
        #n<-nrow(t)
        
        
        xmin<-val$xmin
        xmax<-val$xmax
        
        nmin <- sum(t$Cummlative_distance<xmin)
        nmax <- sum(t$Cummlative_distance<xmax)
        
        #print(c(floor(xmin/max*n),ceiling(xmax/max*n)))
        
        updateSliderInput(session, "range",  value = c(nmin,nmax))
    })
    
    
    
    output$distMap <- renderPlot({
        #print(trail_co())
        
        
        A <- input$range[1]
        B <- input$range[2]
        
        
        if (is.null(input$gpx) )
        {
            b<-make_bbox(lon = c(-180,180), lat=c(-70,70), f=0)
            map<-get_map(location = b, maptype = "satellite", source = "google")
            ggmap(map) +  
                theme(axis.title=element_blank(),
                    axis.text=element_blank(),
                    axis.ticks=element_blank())
            
        } else {
            start<-trail_co()[A,]
            end<-trail_co()[B,]
        
            gg_map() + 
                geom_point(data = start, mapping = aes(x = X, y = Y), colour="green", alpha=0.7, size = 4) +
                geom_point(data = end, mapping = aes(x = X, y = Y), colour="red", alpha=0.7, size = 4) +  
                theme(axis.title=element_blank(),
                      axis.text=element_blank(),
                      axis.ticks=element_blank())
                
        }
            
        

    })
    
    
    output$distPlot <- renderPlot({
        validate(need(trail(), message = FALSE))
        #validate(need(input$range[1]), message = FALSE)
        #validate(need(input$range[2]), message = FALSE)
        session$resetBrush("plot_brush")
        
        A <- input$range[1]
        B <- input$range[2]
        
        c<-input$choice
        
        if(c == "Vegetation")
        {
            veg <- veg()[A:B,]
            ggplot(data = veg, aes(fill=XGROUPNAME, group = VEG_GRP)) + 
                geom_ribbon(aes(x=Cummlative_distance, ymin=-10, ymax=ele)) + 
                theme(legend.position="bottom", legend.direction = "vertical") + 
                labs(x ="Distance (m)", y = "Elevation (m)")
        } else if(c == "Geology"){
            geo <- geology()[A:B,]
            ggplot(data = geo, aes(fill=DESC, group = GEO_GRP)) + 
                geom_ribbon(aes(x=Cummlative_distance, ymin=-10, ymax=ele)) + 
                theme(legend.position="bottom", legend.direction = "vertical") + 
                    labs(x ="Distance (m)", y = "Elevation (m)")
        } else {
            dat <- trail()[A:B,]
            ggplot(data = dat) + 
                geom_path(aes(x=Cummlative_distance, y=ele, colour = ele)) + 
                theme(legend.position="none") + 
                    labs(x ="Distance (m)", y = "Elevation (m)")
        }
                
        
    })
    
})
