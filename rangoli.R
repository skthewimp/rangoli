require(ggplot2)
movetowards <- function(x1,y1,x2,y2,increment) 
  # given an initial point x1,y1, where will it move to so that it moves towards (x2,y2)? 
{
  xdiff <- x2 - x1
  ydiff <- y2 - y1 
  scalefact <- 1/sqrt(xdiff*xdiff+ydiff*ydiff)*increment  # scaling it to increment
  xdiff <- xdiff * scalefact
  ydiff <- ydiff * scalefact
  xres <- x1 + xdiff 
  yres <- y1 + ydiff 
  return(c(xres,yres))
}

movetowardsperc <- function(x1,y1,x2,y2,perc) 
  # given an initial point x1,y1, where will it move to so that it moves towards (x2,y2)? 
{
  xdiff <- x2 - x1
  ydiff <- y2 - y1 
  xdiff <- xdiff * perc
  ydiff <- ydiff * perc
  xres <- x1 + xdiff 
  yres <- y1 + ydiff 
  return(c(xres,yres))
}

getlines <- function(xvec,yvec,increment) # given a set of points (in order - either clock or anti-clock,
  #find out the set of lines that must be drawn along with the next set of points)
{
  newpoints <- mapply(movetowards,xvec,yvec,c(xvec[-1],xvec[1]),c(yvec[-1],yvec[1]),increment)
  ends <- as.data.frame(t(newpoints))
  names(ends) <- c("xend","yend")
  ends$x <- xvec
  ends$y <- yvec
  return(ends)
}

getlinesperc <- function(xvec,yvec,incrementperc) # given a set of points (in order - either clock or anti-clock,
  #find out the set of lines that must be drawn along with the next set of points)
{
  newpoints <- mapply(movetowardsperc,xvec,yvec,c(xvec[-1],xvec[1]),c(yvec[-1],yvec[1]),incrementperc)
  ends <- as.data.frame(t(newpoints))
  names(ends) <- c("xend","yend")
  ends$x <- xvec
  ends$y <- yvec
  return(ends)
}

drawfigure <- function(xvec,yvec,increment,relative=F,maxiter=1000,prnt=T) # starting points of x and ys and the distance to move
{
  flag <- T 
  lineset <- as.data.frame(xvec)
  names(lineset)[1] <- 'x'
  lineset$y <- yvec
  lineset$xend <- c(xvec[-1],xvec[1])
  lineset$yend <- c(yvec[-1],yvec[1])
  i <- 0 
  while(flag & i < maxiter)
  {
    i <- i + 1 
    if(relative)
      lines <- getlinesperc(xvec,yvec,increment) else
        lines <- getlines(xvec,yvec,increment)
    lines2 <- lines
    lines2$xend <- c(lines2$xend[-1],lines2$xend[1])
    lines2$yend <- c(lines2$yend[-1],lines2$yend[1])
    xend <- lines$xend
    yend <- lines$yend
    if(!relative & ( sd(xend) < increment & sd(yend) < increment) ) flag <- F 
    if(relative & (sd(xend) < 1/maxiter & sd(yend) < 1/maxiter)) flag <- F
    
    #dists <- sqrt((xvec-xend)*(xvec-xend) + (yvec-yend)*(yvec-yend))
    #if(max(dists) < increment) flag <- F
    lineset <- rbind(lineset,lines)
    lineset <- rbind(lineset,lines2)
    xvec <- xend
    yvec <- yend
  }
  if(prnt)
    print(ggplot(lineset) + geom_segment(aes(x=x,y=y,xend=xend,yend=yend)) + theme_bw(24) + 
    scale_x_continuous("",breaks=c()) + scale_y_continuous("",breaks=c()) + coord_fixed()) else
    return(lineset)
}

drawpolygon <- function(sides) # given number of sides, start with a regular polygon of that size and move
{
  internalangle <- (sides-2) * pi / sides # internal angle of polygon of this many sides
  xs <- c(0,1)
  ys <- c(0,0) # we start with the first side at (0,0) to (1,0)
  newangle <- 0 
  for(i in 3:sides) # for the rest of the vertices
  {
    lastx <- xs[length(xs)]
    lasty <- ys[length(ys)]
    #lastangle <- atan((ys[length(ys)]-ys[length(ys)-1])/(xs[length(xs)]-xs[length(xs)-1]))
    newangle <- newangle + (pi - internalangle)
    #print(paste(lastangle*180/pi,newangle*180/pi))
    xs <- c(xs,lastx + cos(newangle))
    ys <- c(ys,lasty + sin(newangle))
  }
  return(cbind(xs,ys))
}

pattern <- function(sides,relative=F,increment = 0.01,maxiter=1000) # given number of sides, start with regular polygon of that size and plot
{
  verts <- drawpolygon(sides)
  drawfigure(verts[,1],verts[,2],increment,relative,maxiter)
}

newpattern <- function(sides,relative=F,increment=0.01,maxiter=1000,prnt=T) 
{
  verts <- drawpolygon(sides)
  centroid <- colSums(verts)/nrow(verts) # centroid of the polygon
  lineset <- drawfigure(c(verts[nrow(verts),1],verts[1,1],centroid[1]),c(verts[nrow(verts),2],
                                                                               verts[1,2],centroid[2]),
                        increment,T,prnt=F,maxiter=maxiter)
  for(i in 2:nrow(verts))
  {
    if(sides %% 2 == 0 & i %% 2 == 0) # even number of sides, then we flip
    {
      vecx <- c(verts[i,1],verts[i-1,1],centroid[1])
      vecy <- c(verts[i,2],verts[i-1,2],centroid[2])
    } else
    {
      vecx <- c(verts[i-1,1],verts[i,1],centroid[1])
      vecy <- c(verts[i-1,2],verts[i,2],centroid[2])
    }
    lines1 <- drawfigure(vecx,vecy,increment,T,prnt=F,maxiter = maxiter)
    lineset <- rbind(lineset,lines1)
  }
  if(prnt)
    print(ggplot(lineset) + geom_segment(aes(x=x,y=y,xend=xend,yend=yend)) + theme_bw(24) +
          scale_x_continuous("",breaks=c()) + scale_y_continuous("",breaks=c()) + coord_fixed()) else
            return(lineset)
}


tilehexagon <- function(xmin,xmax,ymin,ymax,increment=0.05)
{
  xs <- seq(xmin-3,xmax+3,3)
  ys <- seq(ymin-sqrt(3),ymax+sqrt(3),sqrt(3)) 
  tile <- expand.grid(xs,ys) # centers of different hexagons
  names(tile) <- c('xs','ys')
  tile2 <- tile
  tile2$xs <- tile2$xs + 1.5
  tile2$ys <- tile2$ys + sqrt(3)/2
  tile <- rbind(tile,tile2)
  filling <- newpattern(6,T,increment,100,prnt=F)
  tiles <- merge(tile,filling)
  tiles$x <- tiles$x + tiles$xs
  tiles$xend <- tiles$xend + tiles$xs
  tiles$yend <- tiles$yend + tiles$ys
  tiles$y <- tiles$y + tiles$ys
  tiles <- tiles[tiles$x > xmin & tiles$x < xmax & tiles$xend > xmin & tiles$xend < xmax & 
                   tiles$y > ymin & tiles$y < ymax & tiles$yend > ymin & tiles$yend < ymax,]
  print(ggplot(tiles) + geom_segment(aes(x=x,y=y,xend=xend,yend=yend)) + theme_bw(24) +
          scale_x_continuous("",breaks=c()) + scale_y_continuous("",breaks=c()) + coord_fixed())
}