
#' mid (milieu)
#'
#' @param u est un point du triangle
#' @param v est un point du triangle
#'
#' @return
#' @export
#'
#' @examples
mid <- function(u,v){
  return((u+v)/2)
  
}

#' divide_triangle
#'
#' @param xA 
#' @param yA 
#' @param xB 
#' @param yB 
#' @param xC 
#' @param yC 
#'
#' @return
#' @export
#'
#' @examples
divide_triangle <- function(xA,yA,xB,yB,xC,yC){
  xO <- mid(xA,xB); yO <- mid(yA,yB)
  xP <- mid(xB,xC); yP <- mid(yB,yC)
  xQ <- mid(xA,xC); yQ <- mid(yA,yC)
  triangle_interieur <- c(xO,yO,xP,yP,xQ,yQ)
  triangle1 <- c(xA,yA,xO,yO,xQ,yQ)
  triangle2 <- c(xO,yO,xB,yB,xP,yP)
  triangle3 <- c(xC,yC,xP,yP,xQ,yQ)
  
  liste <-list(triangle_interieur,triangle1,triangle2,triangle3)
  
  return(liste)
  
}

#' divide_list_triangle
#'
#' @param liste 
#'
#' @return
#' @export
#'
#' @examples
#' 
divide_list_triangle <- function(liste){
  h <- list(1:length(liste))
  total_air <- 0
  for (i in 1:length(liste)){
    xA <- liste[[i]][1]; yA <- liste[[i]][2]; xB <- liste[[i]][3]; yB <- liste[[i]][4]; xC <- liste[[i]][5]; yC <- liste[[i]][6]
    h[[i]] <- divide_tiangle(xA,yA,xB,yB,xC,yC)
   
     plot_triangle(xA,yA,xB,yB,xC,yC)
    somme_air <- total_air +air_total_tiangle(xA,yA,xB,yB,xC,yC)
  }
  liste <- list(h,somme_air)
  return(liste)
}




plot_triangle <- function(xA,yA,xB,yB,xC,yC){
  corner.points <- data.frame(x=c(xA, xB, xC),
                             y=c(yA, yB, yC))
  liste <- divide_triangle(xA,yA,xB,yB,xC,yC)
  
  new.points = data.frame(x=c(liste[[1]][1],liste[[1]][3],liste[[1]][5]),
                          y=c(liste[[1]][2],liste[[1]][4],liste[[1]][6]))
  tab1 <- as.data.frame(setNames(replicate(2,numeric(0), simplify = F),c("x","y") ))
  for( i in 2:3){
    tab1[1,1]=liste[[i]][1]
    tab1[1,2]=liste[[i]][2]
    tab1[2,1]=liste[[i]][3]
    tab1[2,2]=liste[[i]][4]
    tab1[3,1]=liste[[i]][5]
    tab1[3,2]=liste[[i]][6]
    
    new.points <- rbind(new.points,tab1)
    
  }
  
  #plot sierpinski
  
  plot.new()
  legend("left",legend=c("iter.1"))
  my_return=list(polygon(corner.points) ,polygon(new.points[1:3,],col = "black"),
                 polygon(new.points[4:6,],col = "black"),polygon(new.points[7:9,],col = "black"))
  
  return(my_return)
  
}


air_total_tiangle <- function(xA,yA,xB,yB,xC,yC){
  AB=sqrt((xB-xA)^2+(yB-yA)^2)
  BC=sqrt((xC-xB)^2+(yC-yB)^2)
  AC=sqrt((xC-xA)^2+(yC-yA)^2)
  return(air_triangle(AB,BC,AC))
  
}
