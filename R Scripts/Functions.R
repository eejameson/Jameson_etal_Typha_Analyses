# 
# Helper Functions
# 
# This R script contains functions that I created to use in the analysis and data
# manipulation for the Typha project.
# 
# AUTHOR
# Emily Jameson
# 
# DATE CREATED
# 05/11/2021

#### Start of Code ####

# Create function to reassign species based on the new genetic ID's

species_fct <- function(old_species, genetic_ID){
  
  new_species <- character()
  # If the genetic ID for an observation is missing
  if(is.na(genetic_ID)){
    # then the observation was not checked and the original identification is used
    new_species = old_species
    # If the observation is assigned a rerun 
  } else if(genetic_ID == "rerun"){
    # the original identification is used
    new_species = "inconclusive"
    # If the observation is assigned ?
  } else if(genetic_ID == "?"){
    # the original identification is used
    new_species = "inconclusive"
    # If the observation is assigned F2?
  } else if(genetic_ID == "F2?"){
    # the original identification is used
    new_species = "inconclusive"
    # otherwise the new genetic identification is used
  } else {
    new_species = genetic_ID
  }
  
  # Convert the variable to a factor
  new_species <- as.factor(new_species)
  
  # return the new species variable with the appropriate identification
  return(new_species)
}

# Create a new function that works on column vectors
vspecies_fct <- Vectorize(species_fct)

# Assign species based on confidence in the question-marked samples
question_sp_fct <- function(species){
  
  new_species <- character()
  if(is.na(species)){
    new_species = species
  } else if(species == "TYLA?"){
    new_species = "TYLA"
  } else if(species == "TYAN?"){
    new_species = "TYGL"
  } else if(species == "TYGL?"){
    new_species = "TYGL"
  } else{
    new_species = paste0(species)
  }
  
  return(new_species)
}

# vectorize teh function
vquestion_sp_fct <- Vectorize(question_sp_fct)

#### Logistic Regression Functions ####

# Create function to find odds ratios
Odds_ratio <- function(fnct){
  O_ratio <- numeric(length(coef(fnct)) - 1)
  for(i in 1:(length(coef(fnct)) - 1)){
    O_ratio[i] <- exp(coef(fnct)[i + 1])
  }
  return(O_ratio)
} # exponentiates each coefficient to find the odds ratios

# Create Function to find Wald's 95% confidence interval 
CI <- function(fnct){
  conf_int <- matrix(nrow = (length(coef(fnct)) - 1), ncol = 2)
  for(i in 1:(length(coef(fnct)) - 1)){
    conf_int[i,1] <- exp(coef(fnct)[i + 1] - 1.96*coef(summary(fnct))[i + 1,2])
    conf_int[i,2] <- exp(coef(fnct)[i + 1] + 1.96*coef(summary(fnct))[i + 1,2])
  }
  return(conf_int)
} # Creates a 95% confidence interval around the odds ratios

CI2 <- function(fnct){
  conf_int <- tibble(lower_val = numeric(length = length(coef(fnct))),
                     upper_val = numeric(length = length(coef(fnct))),
                     estimate = character(length = length(coef(fnct))))
  for(i in 1:(length(coef(fnct)))){
    conf_int[i,1] <- exp(coef(fnct)[i] - 1.96*coef(summary(fnct))[i,2])
    conf_int[i,2] <- exp(coef(fnct)[i] + 1.96*coef(summary(fnct))[i,2])
    conf_int[i,3] <- names(coef(fnct)[i])
  }
  return(conf_int)
} 


NormalityPlot <- function(x, sims=500){ #creates a function called "NormalityPlot" that can be called later
  #this function has 2 inputs: x = the data input, sims = number of simulations to run (default is 500)
  mu <- mean(x)           #function first finds the mean of the data
  s <- sd(x)              #function next finds standard deviation of the data
  n <- length(x)          #finds the number of datapoints
  qqnorm(x, main = "", pch="")       #creates a blank plot (pch="" means the plotting character is blank, so dots are not drawn)
  for(i in 1:sims){       #sets up a loop, which will run "sims" number of times (default 500)
    d <- rnorm(n, mu, s)  #for each simulation, this generates a random dataset that's normally distributed and has same mean and sd as x
    lines(sort(qqnorm(d, plot.it=FALSE)$x), sort(qqnorm(d, plot.it=FALSE)$y), col="gray")   #plots the qq plot for random data
  }
  points(qqnorm(x, plot.it=FALSE)$x, qqnorm(x, plot.it=FALSE)$y)      #adds the actual data to the simulated data
  lines(qqline(x))
  lines(sort(qqnorm(x, plot.it=FALSE)$x), sort(qqnorm(x, plot.it=FALSE)$y), col="red", lwd=2) #adds a qq plot for the actual data
}

# These two lines of code provide an example of how the above function works
# They use example data included in R from teh iris data set
# Let's use this function to see how "normal" (aka, normally distributed) the iris data are:

{
  NormalityPlot(iris$Sepal.Length) #the deviations from a straight line fall within the realm of random deviations
  title("Iris Example Plot 1: Normal")
}


{
  NormalityPlot(iris$Petal.Length) #the deviations DO NOT fall within the realm of random deviations
  title("Iris Example Plot 2: Non-Normal")
}
