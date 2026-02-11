#Stuff to install :)
#install.packages("imager")
#install.packages("FNN")
#install.packages("sf")
#install.packages("smoothr")
library(imager)
library(ggplot2)
#Load images from github :)
#---------------------------------------------------------------------------------------------------
#1 Acrobasis_indiginella_727-2.jpg <--small                   ***
#2 Acronicta_betulae_81.jpg <-- dark shadow & rect pattern    *Bad takes shadow
#3 Acronicta_retardata_64.JPG <-- black and white details     *
#4 Argyrotaenia_velutinana_79.jpg <-- small blurry but shiny  ***
#5 Calledapteryx_dryoperata_616.JPG <-- detailed wing shape   **
#6 Clepsis_persicana_615.JPG <--high contrast spots           *cuts out spots
#7 Condylolomia_participalis_73-2.JPG <--high contrast lines  ***
#8 Crambus_albellus_79.jpg<-- white low contrast              *too light only takes shadows
#9 Donacaula_longirostrallus_69.JPG <-- long legs             **<--cuts legs suprisingly well (color scale may be issue)
#10 Euclea_delphinii_73.jpg <-- bright details                 ***
#11 Eudonia_strigalis_79.jpg <-- small& blurry                 **<--shadow got caught (but usable)
#12 Eueretagrotis_sigmoides_629.JPG <--dark gives nice contrast***
#13 Iridopsis_ephyraria_73 - Copy.JPG<--light but good outline ***
#14 Leuconycta_dipheroides_615-2.JPG<--lots of details         ***
#15 Leuconycta_dipheroides_628.JPG<--less bold details         ***
#16 Meganola_minuscula_727.jpg <-- very grey                   ***
#17 Pseudeustrotia_carneola_79.jpg<-- high contrast details    *<--caught the pattern (but usable)
#18 Scopula_limboundata_79-2.jpg <--border low contrast        *<--top left was too light
#19 Scopula_limboundata_79.JPG<-- objects in background        *<--not possible
#20 Olethreutes_permundana_73-2.JPG<-- high contrast details   ***
#21 Polygrammate_hebraeicum_79.JPG<-- hight contrast details   *<--too much contrast (diffrent color background could have fixed it)
#22 Lophocampa_caryae_616.JPG<--very detailed, big shadow      *<--shadow got caught
#23 Spilosoma_virginica_527.jpg<-- fully white                 *<--not enough contrast (diffrent background could have helped)
#24 Urola_nivalis_79.jpg<--white small shiny                   *
#25 Spilonota_ocellana_710.jpg<--dark small                    **
#---------------------------------------------------------------------------------------------------
#You have to get the RAW url <--the other one will not work
#seems like smaller moths are more likley to be picked up, they normally have less detail
moth<- load.image("https://raw.githubusercontent.com/JenniferSlater/Moth-Outline-and-shape-detection/main/Moth%20Images/Leuconycta_dipheroides_628.JPG")
plot(moth,main="Moth Image") 
                                                                                                           #just change the number to the one that corresponds to moth
colorscale <- load.image("https://raw.githubusercontent.com/JenniferSlater/Moth-Outline-and-shape-detection/main/ColorBars/15_mothcolorscale.jpg")
plot(colorscale,main="Color Scale") 

#Step 1: from Canny edge detection (turn to greyscale)
#https://justin-liang.com/tutorials/canny/ 
gmoth <- grayscale(moth)
plot(moth, main="Greyscale of Moth")

#If you get an error on this stage try looking at the dimensions with
#dim(moths) <--it could be that if you use a photo editor it will change the formatting 
#so there is only 1 meaning there must have been something that mucked it up
#(x,y,c<--colorchannel,z<--frames)

#Step 2 Gaussian Blur <--I found a way to set in imager :)
#so it very varied for me if the moth image was small
#bmoth<-isoblur(gmoth,5,neumann = TRUE, gaussian = TRUE, na.rm = TRUE)

#calling for the x and y from this x,y,c<--colorchannel,z<--frames
Imagex<-dim(gmoth)[1]
Imagey<-dim(moth)[2]
#print(Imagex)
#print (ImageY)<--quick cutesy little test
#this basically will look @the height of pixels 
#Imagex*Imagey<--total pixels
#I don't really want the total area, I square rooted it so I had less to work with
blurcalc<-sqrt(Imagex*Imagey)*0.02 #<-- the higher the number the more blur
print(blurcalc)

bmoth<-isoblur(gmoth,blurcalc,neumann = TRUE, gaussian = TRUE, na.rm = TRUE)
plot(bmoth, main="Moth with Gaussian Blur")
#so IDEALLY this should change with the pixel number

#step 4 Uncanny
#https://blog.roboflow.com/edge-detection/
#Gradient magnitude did not work that well, it is not worth further exploring 
Uncanny1 <- cannyEdges(bmoth, sigma=blurcalc)
#a smaller <(0.5-1) sigma willperserve the find details
#a larger sigma >2 is agressive and blurs edges (it may merge close edges)
#I honestly want a really agressive edge detection
#I tried the blurcalc with it and it works pretty good
plot(Uncanny1, main="CannyEdges normal")

#-----I should find a way to expand and shrink the pixels like in my 1st program!!!-----------------


#Step 5 smush it into a dataset!
# I found out for fnn to work I need to make it into a numeric matrix
#cause right now it is just a bunch of true and false values
coords <- which(Uncanny1, arr.ind = TRUE) #looks for true values


wing_points2 <- data.frame(
  yy2=coords[,1], #this will be my y corrd/row
  xx2=coords[,2] #this will be my x coord/colum 
  #they are flipped<--tested it with a graph
)
#str(coords) #oh it has false and true still in it

#step 6 connect with FNN
library(FNN) #Fast Nearest Neighbor Search

#I want to essentially take a point and remove it from the dataset
unvisited_point<-coords#<--I want to start off with all the info in the matrix being marked as unvisited
path<-unvisited_point[1,,drop=FALSE]#<--starting with the first point off the list!

while(nrow(unvisited_point)>0){ #<--while there are still points in unvisited
  mostrecentpt<-(tail(path,1))#<stores the last point, and 1 just makes it a single object:)
  
  Fnnlib<- get.knnx(data=unvisited_point,query=mostrecentpt,k=1, algorithm = "kd_tree")
  #k should be less than sample size!
  #https://www.rdocumentation.org/packages/FNN/versions/1.1.4.1
  #I may try out diffrent algorithms...DO NOT TRY COVER TREE IT CRASHED SO BADDD!!!
  # get.knnx(data, query, k=10, algorithm=c("kd_tree", "cover_tree", "CR", "brute"))
  
  nearestpoint <- Fnnlib$nn.index[1]
  test<-Fnnlib$nn.dist[1]
  
  #lets put it all together :)
  path<-rbind(path,unvisited_point[nearestpoint,,drop=FALSE])
  #bind_rows does not work for matrices apparently
  unvisited_point<-unvisited_point[-nearestpoint,,drop=FALSE]
  #head(unvisited)#<just to make sure its good
}
#visualize it
plot(path)

#lets make it into a polygon/connect da lines (this will help me work with smoothr)
#-----I NEED A FIND A WAY TO LIMIT THE JUMPS!!!--------------------------------------------
library(sf) #I think I want to convert to polygon...
closedatpath<- rbind(path, path[1,])#<--basically closes it no matter how crazy the jump to close it is 
#### I should find a better solution for this----------------------------------------------
#so smooth r works on sf geometrics not matrices
#closedatpath=matrix x/y
#as.matrix(closedatpath)<--coordinates
#https://mgimond.github.io/Spatial/anatomy-of-simple-feature-objects.html<--found this it could help
#https://www.youtube.com/watch?v=BgsN-tpolZM&t=272s<---this as well!!!
geome<- st_sfc(st_polygon(list(as.matrix(closedatpath))))
ge_sf <- st_sf(geometry = geome)
#I got an error, it says use st_zm() to drop m
ge<-st_zm(ge_sf)

ggplot()+
  geom_sf(data=ge,fill="pink")+
  ggtitle("Awsome super cool moth fill!!!")
#looks pretty good :)

#step 7 now some work with smoothR
#essentially this should help with removing islands and simplifying the overall shape
#https://github.com/mstrimas/smoothr
library(smoothr)
polgeo1 <- st_simplify(ge, dTolerance = 3)
plot(polgeo1, border = "black", main = "simplify")

crumbs <- drop_crumbs(polgeo1, threshold = 100)

#the it will not fill anything without the plots being simplified first
mothfill <- fill_holes(crumbs, threshold = 100)
plot(mothfill, col = "black", main = "Filled Gaps")

chosen_one <- smooth(mothfill, method = "chaikin", refinements = 3)
plot(st_geometry(chosen_one), col = NA, border = "blue",lwd = 2,lty = 2,add = TRUE)

#I want to find the  curves of everything 
coordss <- st_coordinates(chosen_one)[,1:2]
colnames(coordss) <- c("x","y")# <--this is really the only way ik how to do this
#I should change in the future to update for moth name and family and...ect
#write.csv(coordss,
#          "C:/Users/slate/Desktop/moth_test_coords.csv",
#          row.names = FALSE)

#testing<-read.csv("C:/Users/slate/Desktop/moth_test_coords.csv")

#step 8 size conversion!!!!
#we bring the colors bars back
#all I am going to do is use my old code but now the color bars are isolated >:)
#perfect for my evil scheme
plot(colorscale,main="The original colorscale")

#----------------------------------------------------------------------------
#ok so convert to hsv :)
color_hsv <- RGBtoHSV(colorscale)
H <- channel(color_hsv,1)
S <- channel(color_hsv,2)
V <- channel(color_hsv,3)

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
masked_colors <-black_mask |
  green_mask | blue_mask | lightblue_mask | 
  red_mask | yellow_mask
#I took out the white mask!!! we wouldn't have been able to accuratly identify it anyway
#MASKING PORTION DONE!!! (now the hard part, figuring out size)
mask <- list(black=black_mask,   #I tried them separately and it looked good so I am going to loop this
             green=green_mask,
             blue=blue_mask,
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

px_to_cm= 0.5/median_width # I need to ask joe what the width of the box is 
#0.5cm is the width of the box
print(px_to_cm) 


coordss_cm <- coordss * px_to_cm#<-this is the conversion number of the cm to pixel
c<-coords *0.0264583333
print(c)
#If I find this to be inaccurate we will change it back to what it was originally
#(which was the color scale)
moth_width_cm <- max(coordss_cm[,"x"]) - min(coordss_cm[,"x"])
moth_height_cm <- max(coordss_cm[,"y"]) - min(coordss_cm[,"y"])


plot (coordss,main = sprintf("Moth width: %.2f cm\nMoth height: %.2f cm\n", 
                             moth_width_cm, moth_height_cm))#<-suprisingly helpful to visualize

#this still does not look right, mostlikely me cropping is saves the colorbar at a diffrent pixel resolution/count so it doesn't work

# I should look at ratio's to
#triangle
#rectangle
#square
#oval






