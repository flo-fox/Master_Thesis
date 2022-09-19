#### functions.R ####

# This file includes functions used for the main R scripts

# Adjust rdd::DCdensity function
# https://stackoverflow.com/questions/52334725/how-to-wrap-dcdensity-in-additional-graphical-options-to-modify-the-plot-rd
DCdensity_plot_mod <- function(runvar, cutpoint, caption = "", xlabel = ""){
  
  # get the default plot
  myplot <- rdd::DCdensity(runvar, cutpoint)
  
  # 'additional graphical options to modify the plot'
  #title(main = my_title)
  #title(sub = caption)
  title(xlab = xlabel)
  
  # return
  return(myplot)
}

myDCdensity <- function(runvar, cutpoint, my_abline = 0, my_title = "Default"){
  
  # get the default plot
  myplot <- DCdensity(runvar, cutpoint)
  
  # 'additional graphical options to modify the plot'
  abline(v = my_abline)
  title(main = my_title)
  
  # return
  return(myplot)
}
