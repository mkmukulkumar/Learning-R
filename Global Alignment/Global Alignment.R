mismatch=-1
match=5
gap=-2
seqR="CTTCA"
seqC="CTACA"
x=NULL
y=NULL


for (i in 1:nchar(seqR)) {
  x<-append(x,substr(seqR,i,i))
}
for (i in 1:nchar(seqC)) {
  y<-append(y,substr(seqC,i,i))
}

# initializing dp matrix
dp=matrix(data= 0,nrow = nchar(seqR)+1,ncol = nchar(seqC)+1)

for (i in 0:nchar(seqR)) {
  dp[1,i+1]=i*gap
}
for (i in 0:nchar(seqC)) {
  dp[i+1,1]=i*gap
}

i=2
j=2

while (i<=nchar(seqR)+1) {
  j=2
  while (j<=nchar(seqC)+1) {
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

