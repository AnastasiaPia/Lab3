


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





