mismatch=-1
match=5
gap=-2
seqR="CTTCA"
seqC="CTAC"
x=NULL
y=NULL
newseqR=NULL
newseqC=NULL

for (i in 1:nchar(seqR)) {
  x<-append(x,substr(seqR,i,i))
}
for (i in 1:nchar(seqC)) {
  y<-append(y,substr(seqC,i,i))
}

# initializing dp matrix
dp=matrix(data= 0,ncol = nchar(seqR)+1,nrow = nchar(seqC)+1)

for (i in 0:nchar(seqR)) {
  dp[1,i+1]=i*gap
}
for (i in 0:nchar(seqC)) {
  dp[i+1,1]=i*gap
}
# filling dp matrix
i=2
j=2

while (i<=nchar(seqC)+1) {
  j=2
  while (j<=nchar(seqR)+1) {
    matchval=NULL
    mismatchval=NULL
    vgapval=NULL
    hgapval=NULL
    if(x[j-1]==y[i-1])
      {matchval=dp[i-1,j-1]+match}
    if(x[j-1]!=y[i-1])
      {mismatchval=dp[i-1,j-1]+mismatch}
    vgapval=dp[i-1,j]+gap
    hgapval=dp[i,j-1]+gap
    
    dp[i,j]=max(c(matchval,mismatchval,vgapval,hgapval))
    print(dp[i,j])
    j=j+1
  }
  i=i+1
}

#Backtracking

high=max(c(dp[nchar(seqC)+1,],dp[,nchar(seqR)+1]))
high
i=match( high , (dp[,nchar(seqR)+1]) )
j=match( high , (dp[nchar(seqC)+1,]) )
k=0
while(is.na(i))
{ 
  i=match( high , (dp[,nchar(seqR)+1-k]) )
  k=k+1
}
k=0
while(is.na(j))
{ 
  j=match( high , (dp[nchar(seqC)+1-k,]) )
  k=k+1
}

while (i!=0&&j!=0) {

  if (dp[i,j]==(dp[i-1,j-1]+match)||dp[i,j]==(dp[i-1,j-1]+mismatch))
  {
    newseqR<-append(newseqR,x[i-1])
    newseqC<-append(newseqC,y[j-1])
    i=i-1
    j=j-1
  }
  else if (dp[i,j]==dp[i-1,j]+gap)
  {
    newseqR<-append(newseqR,x[i-1])
    newseqC<-append(newseqC,"-")
    i=i-1
  }
  else if (dp[i,j]==dp[i,j-1]+gap)
  {
    newseqR<-append(newseqR,"-")
    newseqC<-append(newseqC,y[i-1])
    j=j-1
    }


}

