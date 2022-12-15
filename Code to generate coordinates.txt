#---------------------------------------------------------------------------------------------------+
# This script generates RT90 coordinates in a randomly placed grid with a specified grid distance   |
#                                                                                                   |
# Please, set the parameters below                                                                  |
#---------------------------------------------------------------------------------------------------+

# Random numbers that were finally used: 1: 10; 2: 22

#+------------------+
#| Define constants |
#+------------------+

SouthernmostPoint <- 6640469
WesternmostPoint <- 630937

NorthernmostPoint <- 6641948
EasternmostPoint <- 632025

GridDistance <- 100

Radius <- 7


#+------------------------+
#| Input and output files |
#+------------------------+

outfile <- "E:/AA Project work - Fiby lichens/THE_final_coordinates.txt"
polygonfile <- "E:/AA Project work - Fiby lichens/Fiby_polygon_vertices_rounded.txt"


#+-----------------------------+
#| Create vectors and outfiles |
#+-----------------------------+

WEGridLines <- mat.or.vec(100,1)
NSGridLines <- mat.or.vec(100,1)


#+-----------------------------------------+
#| Generate random integers and print them |
#+-----------------------------------------+

Random_1 <- sample(0:(GridDistance-1), 1, replace=T)
Random_2 <- sample(0:(GridDistance-1), 1, replace=T)
cat ('Random number 1: ', Random_1, "\n")
cat ('Random number 2: ', Random_2, "\n\n")

#+-------------------------------+
#| Generate the first grid lines |
#+-------------------------------+

WEGridLines[1] <- SouthernmostPoint+Radius+Random_1
NSGridLines[1] <- WesternmostPoint+Radius+Random_2


#+----------------------------------------------------------+
#| Generate the remaining grid lines towards north and east |
#+----------------------------------------------------------+

x <- 2
y <- 2

while (WEGridLines[x-1]<(NorthernmostPoint-GridDistance-Radius)) {
  WEGridLines[x] <- WEGridLines[x-1]+GridDistance
  x<-x+1
}

x <- x-1


while (NSGridLines[y-1]<(EasternmostPoint-GridDistance-Radius)) {
  NSGridLines[y] <- NSGridLines[y-1]+GridDistance
  y<-y+1
}

y <- y-1


#+----------------------------------+
#| Create empty list of coordinates |
#+----------------------------------+

Coordinates <- mat.or.vec(x*y,2)


#+---------------------------------------------------------------+
#| Generate the list of coordinates from grid line intersections |
#+---------------------------------------------------------------+

k <- 0

for (i in 1:x) {
  for (j in 1:y) {
    k <- k+1
    Coordinates[k,1] <- WEGridLines[i]
    Coordinates[k,2] <- NSGridLines[j]
  }
}


#+----------------------+
#| Load necessary tools |
#+----------------------+

# if("SDMTools" %in% rownames(installed.packages()) == FALSE) {
#   install.packages('SDMTools', repos='http://cran.us.r-project.org')
# }

# install.packages("R.utils")

library (SDMTools)


#+-----------------------------------------------------------------------+
#| Read polygon vertices and heck whether coordinates are inside polygon |
#| Generate 3-column table with "0" (outside) or "1" (inside)            |
#| Filter out lines with "1", skip the rest                              |
#+-----------------------------------------------------------------------+

PolygonVertices <- read.table(polygonfile)

Coordinates_2 <- pnt.in.poly(Coordinates,PolygonVertices)

FilteredCoordinates <- subset(Coordinates_2, pip=="1", select=c(X1, X2))


#+----------------------------------+
#| Print results to window and file |
#+----------------------------------+

print(FilteredCoordinates)
write.table(FilteredCoordinates, file=outfile, sep="\t", row.names=FALSE)

cat ('\n', nrow(FilteredCoordinates), 'points!')

Points <- nrow(FilteredCoordinates)

(PointsRandomOrder <- sample(1:Points, Points, replace=F))
PointsRandomOrder <- as.data.frame(PointsRandomOrder)


#--------------------------------------------------------------------------

