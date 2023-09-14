

euclidean<-function(a,b){
#  if(!is.numeric(a)||!is.numeric(b)||!length(a)==1||!length(b)==1){
 #   stop("check your input")
 # }
    while(b!=0){
      k<-b
      b<-a%%b
      a<-k
    }
    return(a)
}



