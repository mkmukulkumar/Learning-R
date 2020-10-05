var=1:100
for (i in var) {
  j=1
  count=0
  while(j<i)
    {
      
      if(i%%j==0)
      {
        count=count+1
       # print(count)
        #print(i)
      }
      j=j+1
    }
  if(count==1)
    print(i)
}


