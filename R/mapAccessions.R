#' @title Plotting Accessions Worldwide or on Specific Countries. 
#' @description this function returns a map with points showing where accessions are located.
#' @param df object of class "data.frame" with coordinates of accessions and target variable.
#' @param long character. Column name from \code{df} representing longitude.
#' @param lat character. Column name from \code{df} representing latitude.
#' @param y Default: NULL, column name from \code{df} representing the target variable. If NULL, the accessions are plotted with a unique colour given in the \code{col} argument.
#' @param col Default: c("red","green"), character or vector of two characters representing the colour for plotting.
#' @param country Default: NULL, one or vector of ISO 3166-1 alpha-3 country codes. When specified, accessions are plotted on maps of the specified countries. If NULL, the accessions are plotted worldwide.
#' @param ... additional arguments to be passed to \code{geom_point} function in \code{ggplot2} package to control appearance of points.
#' @return A world map, or specific countries maps, with plotted points showing locations of accessions. The world map is obtained from package \code{rworldmap}.
#' @details \code{mapAccessions} plots a low resolution world map from the package \code{rworldmap}. Points showing locations of accessions are plotted based on the coordinates obtained from \code{long} and \code{lat}. 
#' If \code{y} is numeric, points are coloured using a gradient scale from the colour(s) given in the \code{col} argument. If \code{y} is not numeric, the colour of plotting is defined by factors of \code{y}.
#'
#' @author Khadija Aouzal, Zakaria Kehel, Bancy Ngatia
#' @examples
#' \dontrun{
#' if(interactive()){
#'  # Loading FIGS subset for wheat sodicity resistance
#'  data(FIGS)
#'  # World Map showing locations of accessions
#'  mapAccessions(df = FIGS, long = "Longitude", lat = "Latitude")
#'  
#'  # Map plotting locations of accessions in Morocco and Algeria with points coloured 
#'  # based on a gradient scale of SodicityIndex values
#'  mapAccessions(FIGS, long = "Longitude", lat = "Latitude", 
#'                y = "SodicityIndex", country = c("MAR","DZA"))
#'  # Map plotting locations of accessions in Morocco with points
#'  # coloured based on levels of y 
#'  mapAccessions(FIGS, long = "Longitude", lat = "Latitude", 
#'  y = "PopulationType", country = "MAR")
#'  }
#' }
#' @rdname mapAccessions
#' @export
#' @importFrom sp coordinates over proj4string 
#' @importFrom ggplot2 element_blank fortify ggplot geom_polygon geom_point scale_color_gradient theme
#' @importFrom utils data
#' @importFrom methods as

mapAccessions <- function(df, long, lat, y = NULL, col = c("red","green"), country = NULL, ...){
  
  Longitude = df[,long]
  Latitude = df[,lat]
  
  df1 = cbind(df, Longitude, Latitude)
  sp::coordinates(df1) <- c('Longitude', 'Latitude')
  
  countriesLow <- rworldmap::countriesLow
  
  if(!is.null(country)){
    
    countriesLow <- subset(countriesLow, countriesLow@data$ADM0_A3 %in% country)
    sp::proj4string(df1) <- sp::proj4string(countriesLow)
    df1sub <- !is.na(over(df1, as(countriesLow, "SpatialPolygons")))
    df1 <- df1[df1sub,]
  }
  
  if(is.null(y)) {
    
    world <- fortify(countriesLow)
    ggplot() +  geom_polygon(data = world,
                              aes(x = long, y = lat, group = world$group), colour = "black", fill = NA) + geom_point(..., data = data.frame(df1), aes(x = Longitude, y = Latitude), color = col[1]) +
      theme(axis.line=element_blank(),axis.text.x=element_blank(),
            axis.text.y=element_blank(),axis.ticks=element_blank(),
            axis.title.x=element_blank(),
            axis.title.y=element_blank(),
            panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
            panel.grid.minor=element_blank(),plot.background=element_blank())
    
  }
  
  else {
    
    if (is.numeric(unlist((df[,y])))){
      
      sub = subset(data.frame(df1), !is.na(df1@data[,y]))
      world <- fortify(countriesLow)
      
      ggplot() + geom_polygon(data = world,
                              aes(x = long, y = lat, group = world$group), colour = "black", fill = NA) + 
        geom_point( ..., data = sub, aes(x = Longitude, y = Latitude, color = sub[,y])) + 
        scale_color_gradient(low = col[1], high = col[2]) +
        labs(colour = y) +
        theme(legend.position = "right", axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())
      
    }
    
    else {
      sub = subset(df1@data, !is.na(df1@data[,y]))
      col = factor(sub[,y])
      world <- fortify(countriesLow)
      ggplot() + geom_polygon(data = world,
                              aes(x = long, y = lat, group = world$group), colour = "black", fill = NA) + 
        geom_point( ..., data = subset(data.frame(df1), !is.na(df1@data[,y])), aes(x = Longitude, y = Latitude, color = col)) +
        labs(colour = y) +
        theme(legend.position = "right", axis.line=element_blank(),axis.text.x=element_blank(),
              axis.text.y=element_blank(),axis.ticks=element_blank(),
              axis.title.x=element_blank(),
              axis.title.y=element_blank(),
              panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),plot.background=element_blank())
    }
    
  }
}
