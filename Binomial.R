# As the same as ModBinomialR that you can find in my reporsitories

bino2=function(S,K,T,r,n,o,put=FALSE,am=FALSE){ #am means american
  t=T/n
  u=exp(o*sqrt(t))
  d=1/u
  p=(exp(r*t)-d)/(u-d)
  val=matrix(0,n+1,n+1);C=matrix(0,n+1,n+1)
  pos=ifelse(put,-1,1)
  for(i in 1:n+1){
    for(j in 1:i){
      st=S*(u^(j-1)*d^(i-j))
      val[i,j]=st
    }
  }
  C[nrow(C),]=pmax(pos*(val[nrow(val),]-K),0)
  for(i in (nrow(C)-1):1){
    for(j in 1:i){
      if(am){
        v1=exp(-r*t)*(p*C[i+1,j+1]+(1-p)*C[i+1,j])
        st=pos*(S*u^(j-1)*d^(i-j)-K)
        v2=max(st,0)
        C[i,j]=max(v1,v2)
      }
      else{
        C[i,j]=exp(-r*t)*(p*C[i+1,j+1]+(1-p)*C[i+1,j])
      }
    }
  }
  return(C[1,1])
}
