##################################################DON´T CHANGE###############################
####                                                                                  #####  
###########################################################################################
##   Function for random sampling in circle and shortest route through sampling points#####
###########################################################################################
points_path <- function(N=40,R=1, app=FALSE, pdf=TRUE){    # N: number of points to be calculated; R: radius; app:  append results  
  require(plotrix)
  
  # POINTS: 
  if(pdf==TRUE){
    pdf("points_path.pdf", height=7, width=max(N/4, 10))
    plot(1,1, pch=NA, xlim=c(-R+5, R+5), ylim=c(-R+14,R+14), asp=1, bty="n", xlab="X", ylab="Y", main="Sample points distribution")
    abline(h=14, lty=2)
    abline(v=5, lty=2)
    draw.circle(5,14,R,border="black",col=NA, lty=2, lwd=2)
  }
  puntos<-N
  t <- 2*pi*runif(puntos, 0, 1)
  u <- sample(seq(0, 1, 0.01), puntos, replace=T)+sample(seq(0, 1, 0.01), puntos, replace=T)
  r<- ifelse(u>1, 2-u, u)
  if(pdf==TRUE){
    points(R*(r*cos(t))+5, R*(r*sin(t))+14, pch=16)
  }
  coordenadas<-round(cbind(R*round(r*cos(t),2)+5, R*round(r*sin(t),2)+14),2)
  colnames(coordenadas)<-c("x", "y")
  rownames(coordenadas)<-paste("(",round(coordenadas[,1],1), ", ", round(coordenadas[,2],1),")" ,sep="")
  if(pdf==TRUE){
    thigmophobe.labels(R*(r*cos(t))+5, R*(r*sin(t))+14, labels = rownames(coordenadas), cex=0.6, offset=0.5)
  }
  
  # PATH:
  distancias <- dist(coordenadas, method = "euclidean")         
  require(TSP)
  TSP1<-as.TSP(distancias)
  TSP1 <- insert_dummy(TSP1, label = "cut")
  tours <- solve_TSP(TSP1, method = "2-opt")
  path <- cut_tour(tours, "cut")
  if(pdf==TRUE){
    plot(1,1, pch=NA, xlab="", ylab="", bty="n", axes=F, xlim=c(0, (ceiling(N/10)+1)), ylim=c(1, 11), main=paste("Tour Path: ", round(attr(tours, "tour_length"),2), "cm", sep=""))
    text(rep(1:(ceiling(N/10)), each=10)[1:(length(labels(path)))], rep(10:1,(ceiling(N/10)))[1:(length(labels(path)))],  labels(path))
    mtext(paste(1:(ceiling(N/10)), ")       ", "(X, Y)         ", sep=""), 3, at=c(1:(ceiling(N/10))), line=-1.5)
    lines(c(0.5,(ceiling(N/10)+0.5) ), c(10.5, 10.5))
    for(i in 1:(ceiling(N/10)-1)){  lines(c(0.5+i, 0.5+i), c(11.5, 0.5))}
    dev.off()
  }
  write.table(labels(path), "ruta.csv", append=app, sep=";", dec=",", col.names=NA)
  return(round(attr(tours, "tour_length"),2))    
}
####                                                                               #####
####################################DON'T CHANGE##########################################

###Options:
repeticiones<-as.numeric(readline("Number of paths to compute: "))
puntos<-as.numeric(readline("Number of points per path: "))
radio<-as.numeric(readline("Circle radius: "))
anexar<-TRUE
PDF<-FALSE
if(repeticiones==1){
  anexar<-readline("Append results? (Y/N): ")
  anexar<-ifelse(tolower(anexar)=="n" | tolower(anexar)=="no", FALSE, TRUE)
}
if(repeticiones==1){
  PDF<-readline("Create PDF with plot and path? (Y/N): ")
  PDF<-ifelse(tolower(PDF)=="n" | tolower(PDF)=="no", FALSE, TRUE)
}

#Ejecución
for(i in 1:repeticiones){
  points_path(puntos, radio, anexar, PDF)
}
