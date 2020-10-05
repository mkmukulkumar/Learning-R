a=134567
num=nchar(a)
place=10
vectr<-c()
i=0
total=0
while (i<num) {
  b=(a%%place-total)
  total=total+b
  b=b/10^i
  vectr<- append(vectr,b)
  i=i+1
  place=place*10
}
i=1
k=num
while(i!=k&&i<k)
{
  if(vectr[i]!=vectr[k])
  {
    print("Not a palindrome")
    break;
  }
  i=i+1
  k=k-1
  
}
if(vectr[i]==vectr[k])
{
  print("Palindrome")
}

