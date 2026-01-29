#https://justin-liang.com/tutorials/canny/ <--this should help
library(imager)

#"C:/Users/slate/Desktop/Anavitrinella_pampinaria_727.JPG"
#"C:/Users/slate/Pictures/Anavitrinella_pampinaria_727 - Copy (2).JPG
#"C:/Users/slate/Pictures/Cropped3.png"
#"C:/Users/slate/Desktop/New folder/Scopula_limboundata_79.JPG"<--good for block elimination
#"C:/Users/slate/Desktop/Iridopsis_ephyraria_73 - Copy.JPG"<--ehhh
#Eueretagrotis_sigmoides_629.JPG<--Best one
#Acrobasis_indiginella_727-2.jpg<--bad one

mothp <-load.image("C:/Users/slate/Desktop/Leuconycta_dipheroides_628.JPG")
moths <- imresize(mothp,scale=0.5)

#first thing to do it plot the moth to know what we dealing with :)
plot(moths,main="The original moth")
#Step 1: from Canny edge detection (turn to greyscale)

#issue with greyscale so lets test the dim to see whats up
dim(moths)        # w h     color channel   frames
#the moth image is ## ##    1(should be 3)    #
#so there is only 1 meaning there must have been something that mucked it up
#(x,y,c<--colorchannel,z<--frames)

#I found un edited images work so that is my workaround for the moment
#BUG FIX EVENTUALLY------------------------------

#step 1 greyscale--------------------------------------------------------------

gmoth <- grayscale(moths, drop = TRUE)
plot(gmoth, main="Greyscale of Moth") 

#Step 2 Gaussian Blur <--I found a way to set in imager :)
#might as well turn all the options on <--could be something to play with tho!
bmoth<-isoblur(gmoth,5,neumann = TRUE, gaussian = TRUE, na.rm = TRUE) 

plot(bmoth, main="Blurred Moth") 

#-------------------------------------------------
#step 2.5 side quest!!!
#I tried gradient magnitude by itself and it was thicker but more accurate
#maybe overlapping could work?
dx <- imgradient(bmoth,"x") 
dy <- imgradient(bmoth,"y")

grad.mag <- sqrt(dx^2+dy^2) 
plot(grad.mag,main="Gradient magnitude Side Quest")

gmoth<-isoblur(grad.mag,2,neumann = TRUE, gaussian = TRUE, na.rm = TRUE)
plot(gmoth,main="Gradient magnitude")
#--------------------------------------------------
#step 3 Uncanny
Uncanny1 <- cannyEdges(bmoth, sigma=1.5)
plot(Uncanny1, main="CannyEdges normal")

Uncanny2 <- cannyEdges(gmoth, sigma=1)
plot(Uncanny2, main="CannyEdges with gm")

##NORMAL WINS!!!
#--------------------------------------------
#step 4 plot it
coords <- which(Uncanny1, arr.ind = TRUE)
wing_points2 <- data.frame(
  yy2=coords[,1], #this will be my y corrd/row
  xx2=coords[,2] #this will be my y coord/colum
)

library(ggplot2)
#ok so now we have the data, lets make a scatter plotttt
ggplot(wing_points2, aes(x=xx2, y=yy2)) + 
  
  geom_point(
    color="blue",
    size=0.5)+ #make it pretty:)
  coord_fixed()+
  coord_flip()+
  scale_x_reverse()+
  ggtitle("moth ploted Canny Edges no reduction:))")


#I think I could hypothetically convert this to a matrix here but idk
matrixz <- as.matrix(Uncanny1)
print(matrixz)
#bruhhhh its a Travelling salesman problem TwT (says google) 
#lets put it in a data frame
df <- as.data.frame(Uncanny1)
print(df)
#we only want the edge points but that is not something I can really do
#BUT we PERSIST ON!!!
head(df)
colnames(df) #to check :)

#step 5 CONNECT THE DOTSSS
gimmeapoint<- as.matrix(df[,c("x","y")]) #<-dataframe, I do need to convert to matrix
#x,y,z,cc
#I only want it to give me x and y
#subset is basically extract points
print(gimmeapoint)
#cool  it is looking good
library(tidyverse) #gonna mess with data so prolly a good idea
library(FNN) #Fast Nearest Neighbor Search <--fingers crossed this is better

#lets try this traveling sales man problem
#what I want it to do...
#take a point and visit it
#Remove it from the dataset

#Its just like python, lets start with an unvisited dataset
unvisited<-gimmeapoint #lateron I will subtract from this...maybe 
#if it works
#lets start with the first points cause the moth I am using is tipped 
#...and its convenient
path<-unvisited[1,,drop=FALSE]
unvisited<-unvisited[-1,,drop=FALSE]#<--everything else

while(nrow(unvisited)>0){ #<--while there are still points in unvisited
  mostrecentpt<-(tail(path,1))#<adds 1
  
  #lets calculate the distance its in the loop so it does it everytime  
  #my last attemt took to long since it was calculating every possibility
  letstrynewlibrary<- get.knnx(data=unvisited,query=mostrecentpt,k=1, algorithm = "kd_tree")
  #I may try out diffrent algorithms...DO NOT TRY COVER TREE IT CRASHED SO BADDD!!!
  # get.knnx(data, query, k=10, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
  
  nearestpoint <- letstrynewlibrary$nn.index[1]
  test<-letstrynewlibrary$nn.dist[1]
  
  if (test>150){
    maybenot<- nearestpoint 
    unvisited<-unvisited[-nearestpoint,,drop=FALSE]}
  #Adding an if then statement to help me out :)
  #if it is a distance longer than 150 pixels, just get rid of it :)
  #130 seems like the minimum from what I can tell
  
  else{
    
    #PUT IT ALL TOGETHER!!
    path<-rbind(path,unvisited[nearestpoint,,drop=FALSE])
    #bind_rows does not work for matrices apparently
    unvisited<-unvisited[-nearestpoint,,drop=FALSE]
    head(unvisited)#<just to make sure its good
  }}
#-----------OK I HAVE TO FIX THE BIG JUMPS ITS A PROBLEM----------------
#I want to find distance between points
#set a threshold and make sure to delete points that pass it
#Coming in clutch!!!--> https://www.geeksforgeeks.org/r-language/distance-between-two-sets-of-points-in-r/
path <- as.data.frame(path)
euc.dist <- sqrt( diff(path$x)^2 + diff (path$y)^2 )
#diff is differences basically diffrences of x and y
threshold <- 2
vibing <- c(TRUE, euc.dist < threshold)
party <- path[vibing, ] #<--DID NOTHING!!!! TwT

#plot that baby in colors and see what we got!
ggplot(party,aes(x=x,y=y))+
  geom_path(color = "blue") +
  ggtitle("Moth outline plotted on x and y axis")


ggplot(party, aes(x=x,y=y))+
  geom_polygon(fill="green")+
  ggtitle("Awsome super cool moth fill")


#AHHHHH IT WORKSSS!!
#(don't try it with a different moth pic...its really bad)


# TO DO WHAT I NEED TO ADD
#- Better edge detection to not get patterns in wings
#- somehow eliminate the legs
#- Smoothr 
#- Raster

#Ok so eliza gave a great suggestion to use smoother so lets see how it goesss
#https://github.com/mstrimas/smoothr
library(sf) #I think I want to convert to polygon...
closedatpath<- rbind(party, party[1,])#needs to be all closed
pol = st_sfc(st_polygon(list(as.matrix(closedatpath))))
plot(pol) #ok yay that looks pretty cute

polgeo <- st_sf(geometry = pol)
nrow(polgeo)

#idk what I am doing, we just trying stuff
library(smoothr)
library(units)
#https://strimas.com/smoothr/reference/smooth.html
# trying diffrent methods to find the best

#current theory is the points are so dense and close together that the smoothr can't really smooth anything
polgeo1 <- st_simplify(polgeo, dTolerance = 0.5)
plot(polgeo1, border = "black", lwd = 1.5, main = "simplify")

polgeo11 <- drop_crumbs(polgeo1, threshold = 100)
polgeo111 <- fill_holes(polgeo11, threshold = 100)
# plot
plot(polgeo11, col = "grey", border = "black", lwd = 1.5, main = "crumbs")
plot(polgeo111, col = "grey", border = "black", lwd = 1.5, main = "fill")

#smooth_chaikin1 <- smooth(polgeo111, method = "chaikin", refinements = 1)#<--I guess idk how this works cause they all look the same...
chosen_one <- smooth(polgeo111, method = "chaikin", refinements = 3)
#smooth_chaikin3 <- smooth(polgeo111, method = "chaikin", refinements = 10)
#l_smooth_ksmooth <- smooth(polgeo11, method = "ksmooth")
#l_smooth_spline <- smooth(polgeo11, method = "spline")

#plot(st_geometry(polgeo111), col = NA, border = "black",lwd = 3,lty = 2)
#plot(st_geometry(smooth_chaikin1), col = NA, border = "red",lwd = 2,lty = 2,add = TRUE)
plot(st_geometry(chosen_one), col = NA, border = "blue",lwd = 2,lty = 2,add = TRUE)
#plot(st_geometry(smooth_chaikin3), col = NA, border = "green",lwd = 2,lty = 2,add = TRUE)
#plot(st_geometry(l_smooth_ksmooth), col = NA, border = "maroon",lwd = 2,lty = 2,add = TRUE)
#plot(st_geometry(l_smooth_spline), col = NA, border = "purple",lwd = 2,lty = 2,add = TRUE)

#legend(
#  "topright",
#  legend = c("Original", "1","2","3"),
#  col = c("black", "red","blue","green"),
#  lwd = c(3, 2, 2, 2),
#  lty = 2,
#  bty = "n")

#I want to find the  curves of everything 
coordss <- st_coordinates(chosen_one)[,1:2]
colnames(coordss) <- c("x","y")# <--this is really the only way ik how to do this
#I should change in the future to update for moth name and family and...ect
#write.csv(coordss,
#          "C:/Users/slate/Desktop/moth_test_coords.csv",
#          row.names = FALSE)

#testing<-read.csv("C:/Users/slate/Desktop/moth_test_coords.csv")

#Yayyy
#now lets work on the other side of this project >:)
#Basically have it figured out \(-⭘-")/ *yawn*
#all I am going to do is use my old code but now the color bars are isolated >:)
#perfect for my evil scheme
colorbar <-load.image("C:/Users/slate/Desktop/Screenshot 2026-01-28 152909.jpg")
colorscale <- imresize(colorbar,scale=0.5)#<should match moth resize
plot(colorscale,main="The original colorscale")

#----------------------------------------------------------------------------
#ok so convert to hsv :)
color_hsv <- RGBtoHSV(colorscale)
H <- channel(color_hsv,1)
S <- channel(color_hsv,2)
V <- channel(color_hsv,3)

#hehehe the fun part from my original code :)
#----------------------------------------------------------------------------
#THESE ARE THE COLOR RANGES FOR THE HSV (we will need to tweak it eventually to fit with color correction)
white_mask <- (H > 0/360) & (H < 160/360) &(S < 0.2)& (V > 0.8)
black_mask <- (V < 0.2)

#Adding other pretty colors :)
green_mask <- (H > 90/360) & (H < 160/360) & (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8)
blue_mask <- (H > 220/360) & (H < 250/360) & (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8)
lightblue_mask <- (H > 165/360) & (H < 210/360) & (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8)

# took me an hour to learn this but Hue is a circular range 0-360
#SOOO...if we look at a color like red it is both at 360 and 0, so I put an OR statement(that's y the line)
#basically from 0-30 OR 306-330
red_mask <- ((H >= 0 & H <= 30) | (H >= 330 & H <= 360))& (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8)
yellow_mask <- (H > 55) & (H < 75) & (S > 0.2) &(S < 0.8)& (V > 0.2)& (V < 0.8)
gray_mask <- (S < 0.2) & (V > 0.2) & (V < 0.8)

# https://stackoverflow.com/questions/76929194/how-to-apply-a-binary-mask-to-a-rgb-image-in-r-with-imager
apply_hsv_mask <- function(HSVimage, mask){
  hue <- channel(HSVimage, 1) * mask
  saturation <- channel(HSVimage, 2) * mask
  value <- channel(HSVimage, 3) * mask
  return(imappend(list(hue, saturation, value), "c"))
}

#gather them up
masked_colors <- white_mask | black_mask | gray_mask | 
  green_mask | blue_mask | lightblue_mask | 
  red_mask | yellow_mask

#MASKING PORTION DONE!!! (now the hard part, figuring out size)
mask <- list(black=black_mask,   #I tried them separately and it looked good so I am going to loop this
             green=green_mask,
             blue=blue_mask,
             lightblue=lightblue_mask,
             red=red_mask,
             yellow=yellow_mask
)
#No need for a moth anymore this is gonna b ez!
#THIS IS FOR SIZE
rectangle_sizes <- list() #empty list, I am gonna have it add as it finds the rectangles
#I have it so it is set up with width,height,color(for identification

for (colors in names(mask)){
  
  mask_rect <- mask[[colors]] #callign the colors
  
  mask_blur <- isoblur(mask_rect, 2) #I ended up having to clean them up I got no choice
  mask_thresh <- mask_blur > 0.2
  mask_clean <- clean(mask_thresh, 11)
  #filled_mask <- fill(mask_clean, 9)
  
  labeled_mask <- label(mask_clean)
  num_regions <- max(labeled_mask) #each section
  
  print(mask)
  print(num_regions) #YAY it gave me the right amount 3 regions for black :)
  #plot(mask_clean)
  
  for (region in 1:num_regions) {  #make a loop to go through all 3 regions
    pix <- which(labeled_mask == region, arr.ind = TRUE)
    #color name              #array thingy for min and max
    xmin <- min(pix[,2]) 
    xmax <- max(pix[,2])
    ymin <- min(pix[,1])
    ymax <- max(pix[,1])
    
    rect( #had to flip them cause it went the wrong way
      xleft = ymin,
      ybottom = xmax,
      xright = ymax,
      ytop = xmin,
      border = "blue",
      lwd =1 #line width so we can see it
    )
    
    #print(sprintf("Region %d: xmin=%d xmax=%d ymin=%d ymax=%d", 
    #              region, xmin, xmax, ymin, ymax)) #tells me position
    width <- xmax - xmin + 1
    height <- ymax - ymin + 1
    print(sprintf("Region %d: width = %f px, height = %f px", 
                  region, width, height))#tells me height and width
    
    #Now I wanna just list all the rectangle sizes real quick 
    #but we definetly want to exclude the moth 
    rectangle_sizes<-append (rectangle_sizes,
                               list(list(width=width,height=height,color=colors)))}
  }
##!!! my original code went off height, but as that is inaccurate I am going to go off width!!!
rect_shortside<-list() #new lists

for (element in rectangle_sizes){
  #I want it to find the longer side 
  short_side<-min(element$width,element$height) #find the min number from l or w
  rect_shortside <- append(rect_shortside, short_side)
}

#I keep getting an error abt "x must be atomic" apparently I have to unlist the varibles from the list?
rect_shortside_unlisted <-unlist(rect_shortside)

#I am not going to do the average, median is way better if there are outliers
median_width<-median(rect_shortside_unlisted)
print(median_width) #so median is 41 px

px_to_cm= 1/median_width # I need to ask joe what the width of the box is 
#
print(px_to_cm) 

#NOW THE MOTH SIZE!!!!
coordss_cm <- coordss * 0.0264583333
moth_width_cm <- max(coordss_cm[,"x"]) - min(coordss_cm[,"x"])
moth_height_cm <- max(coordss_cm[,"y"]) - min(coordss_cm[,"y"])


plot (coordss,main = sprintf("Moth width: %.2f cm\nMoth height: %.2f cm\n", 
                             moth_width_cm, moth_height_cm))#<suprisingly helpful to visualize




