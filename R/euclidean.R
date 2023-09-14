# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'



euclidean<-function(a,b){
    while(b!=0){
      k<-b
      b<-a%%b
      a<-k
    }
    return(a)
}

#examples
#euclidean(12,20)
#euclidean(123612,13892347912)
#euclidean(100,1000)





