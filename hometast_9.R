#######################################################
# LESSON 9 HOMETASK.R - STand height growth modelling #
#######################################################

rm(list = ls()) # remove all memory
getwd() # to check the initial directory
#setwd("C:/MetsMud2023") # to set my working directory
#dir() # to check if our dataset is listed in the directory

Plot_data <-plotdat <- read.csv2("https://raw.githubusercontent.com/temitopeomoniyi/Metsmud/main/yndks.csv")

View(Plot_data)
tree_data <-plotdat <- read.csv2("https://raw.githubusercontent.com/temitopeomoniyi/Metsmud/main/pndks.csv")

View(tree_data)


#################################################################
# personal Note: ks = birch, nd = most fertile soil in Estonia,
##########################################################

# to check the structure of dataset and have and overview

str(Plot_data) 
str(tree_data)

########################################################
# change the name of the variables from EST to English #
# for adequate understanding of the dataset            #
#########################################################

# check the variable name or say column names

names(Plot_data)
names(tree_data)

################
# for plotdata #
################

# create a vector plot_dataEE so as to be able to return our dataset back to original form
Plot_dataEE<-names(Plot_data)

Plot_dataENG <- c(ncol(Plot_data))

Plot_dataENG <- c("id", "year", "no", "no_max",  "site", "sp_dom", "radius", "age" )

names(Plot_data) <- Plot_dataENG

names(Plot_data) # to double check
View(Plot_data) # to  view 

##################
# for tree data  #
##################

# create a vector PnamesEE

tree_dataEE<-names(tree_data)

tree_dataENG <- c(ncol(tree_data))
tree_dataENG <- c("id", "year", "no", "no_max",  "no.ev", "storey", "idt", "spp",
               "azimuth", "distance", "dbh", "h", "fault", "raadius", "hp", "v"
)
names(tree_data) <- tree_dataENG
names(tree_data)
View(tree_data)

###############
# QUESTION 1a  #
###############


# The dominant tree species on my plot
table(Plot_data$sp_dom)

# the column sp_dom can be converted to factor and then the below can be used
# to know if there are many species
Plot_data$sp_dom <- as.factor(Plot_data$sp_dom)
nlevels(Plot_data$sp_dom)

# return the sp_dom back to character
Plot_data$sp_dom <- as.character(Plot_data$sp_dom)


################
# QUESTION 1b  #
################

# The SITE TYPES of my plot

table(Plot_data$site)
nrow(Plot_data) # to check observation

################
# QUESTION 1c  #
################

# Number of plot measurement

Plot_data1 <-subset(Plot_data, no==1) # the first measurement of plots
X <- table(Plot_data1$no_max)
as.numeric(X)
names(X)
as.numeric(names(X))
sum(X*as.numeric(names(X))) # Number of plots measurement 


################
# QUESTION 1d  #
################

# Number of plots in my dataset
unique(Plot_data$id) # this would require a bit of visual counting

# Alternatively, convert th column id to factor
Plot_data$id <- as.factor(Plot_data$id)
# check the number of distinct values
nlevels(Plot_data$id) # this will give a single count

# return the sp_dom back to character
Plot_data$id <- as.character(Plot_data$id)

# Also we could use this
length(unique(Plot_data$id))


################
# QUESTION 2  #
################


# Number of tress by storey
# Count number of the 1st storey trees for each plot measurement
# Display tree distribution by storey

table(tree_data$storey)

# Create a subset of the 1st storey trees

tree_subset<-subset(tree_data, storey=="1")

df<-aggregate(dbh~id+year,data=tree_subset,length)
names(df)[3]<-"no_dbh" # no_d = number of dbh measured
# Merge data sets tree_data and df
tree_data<-merge(tree_subset,df)
# order tree data according to id and year
tree_data<-tree_data[order(tree_data$id,tree_data$year),]

###############
# QUESTION 3  #
###############


# Calculate mean square diameter for each measurement
mean.sq<-function(x) sqrt(mean(x^2))

X <-aggregate(dbh~id+year,data=tree_subset,mean.sq)
names(X)[3]<-"D"
Plot_data<-merge(Plot_data,X)
View(Plot_data)

###############
# QUESTION 4  #
###############

# scatterplot for mean square diameter and stand age
par(mar=c(5,4,2,2)+0.2, cex = 1)

plot(D~age,data=Plot_data, xlab= "age", ylab= "mean_square_diameter", 
     main="HOME_WORK_9a", cex.lab=2, cex.axis=2, cex.main=2, cex.sub=2,
     xlim=c(0,120),ylim=c(0,45))

dev.print(device = jpeg,
          filename= "homework9b.png",
          width = 750,
          height = 550
)

###############
# QUESTION 5  #
###############

###########################################################
# diameter-age relationship with Richards function 5.3    #
#y =a*(1-exp(-b*t))^c                                     #
#b=0.03 starting value c =2                               #
###########################################################

M<-nls(D~a*(1-exp(-b*age))^c, data = Plot_data, start = list(a=45, b=0.03,c=2))
summary(M)


###########################################################
# diameter to age relationship with STRAND function 2.7   #
# y = (t/(a+(b*t)))^3                                     #
# a=0.15, b =2, t =age                                    #
###########################################################

M1<-nls(D~(age/(a+(b*age)))^3, data = Plot_data, start = list(a=2, b=0.15))

summary(M1)


####################################
# WHEN a=0.8, b=0.125 AND t = age
####################################
M2<-nls(D~(age/(a+(b*age)))^3, data = Plot_data, start = list(a=0.8, b=0.125))

summary(M2)



#####################
# QUESTION 6 and 7  #
#####################

# Using Richards function 5.3

#Parameters:
# Estimate Std. Error t value Pr(>|t|)    
# a 51.363084   4.619129  11.120  < 2e-16 ***
# b  0.019069   0.003278   5.817 1.63e-08 ***
# c  2.036098   0.242027   8.413 2.08e-15 ***
  
# using STRANDs function 2.7


#Parameters:
#Estimate Std. Error t value Pr(>|t|)    
# a 7.445092   0.195824   38.02   <2e-16 ***
#b 0.225123   0.002942   76.52   <2e-16 ***
 
#####################
# QUESTION 8 and 9 #
####################
  
Age<-0:200
test <-predict(M,newdata = data.frame(age=Age))


# Using the second model
test1 <- predict(M1, newdata = data.frame(age=Age))


# fiting the lines
lines(Age,test, col="blue",lwd=3)
lines(Age, test1, col="darkred", lwd=3)


dev.print(device = jpeg,
          filename= "homework9b.png",
          width = 750,
          height = 550
)
