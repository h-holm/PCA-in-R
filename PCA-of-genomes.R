# Grab the required packages. Install first if needed.
listOfPackages <- c("ggplot2")
for (i in listOfPackages){
  if(! i %in% installed.packages()){
    install.packages(i, dependencies = TRUE)
  }
  require(i)
}
