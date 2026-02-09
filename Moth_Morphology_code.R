#Stuff to install :)
#install.packages("imager")
#install.packages("FNN")
#install.packages("sf")

library(imager)
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
moth<- load.image("https://raw.githubusercontent.com/JenniferSlater/Moth-Outline-and-shape-detection/main/Moth%20Images/Polygrammate_hebraeicum_79.JPG")
plot(moth,main="Moth Image") 
                                                                                                           #just change the number to the one that corresponds to moth
colorscale <- load.image("https://raw.githubusercontent.com/JenniferSlater/Moth-Outline-and-shape-detection/main/ColorBars/21_mothcolorscale.jpg")
plot(colorscale,main="Color Scale") 

#Step 1: from Canny edge detection (turn to greyscale)
#https://justin-liang.com/tutorials/canny/ gmoth <- grayscale(moth)
plot(gmoth, main="Greyscale of Moth")

#If you get an error on this stage try looking at the dimensions with
#dim(moths) <--it could be that if you use a photo editor it will change the formatting 
#so there is only 1 meaning there must have been something that mucked it up
#(x,y,c<--colorchannel,z<--frames)

#Step 2 Gaussian Blur <--I found a way to set in imager :)
#so it very varied for me if the moth image was small
#bmoth<-isoblur(gmoth,5,neumann = TRUE, gaussian = TRUE, na.rm = TRUE)

#calling for the x and y from this x,y,c<--colorchannel,z<--frames
Imagex<-dim(gmoth)[1]
Imagey<-dim(gmoth)[2]
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
closedatpath<- rbind(path, path[1,])#needs to be all closed
ggplot(closedatpath, aes(x=closedatpath[,1],y=closedatpath[,2]))+
  geom_polygon(fill="pink")+
  ggtitle("Awsome super cool moth fill")


#step 7 now some work with smoothR
#essentially this should help with removing islands and simplifying the overall shape
#https://github.com/mstrimas/smoothr


#step 8 size conversion
##coords_cm <- coordss * 0.0264583333#<-this is the conversion number of the cm to pixel
#If I find this to be inaccurate we will change it back to what it was originally
#(which was the color scale)
##moth_width_cm <- max(coordss_cm[,"x"]) - min(coordss_cm[,"x"])
##moth_height_cm <- max(coordss_cm[,"y"]) - min(coordss_cm[,"y"])


##plot (coordss,main = sprintf("Moth width: %.2f cm\nMoth height: %.2f cm\n", 
##                             moth_width_cm, moth_height_cm))#<suprisingly helpful to visualize

# I should look at ratio's to
#triangle
#rectangle
#square
#oval





