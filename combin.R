x <- 12  ##input array size
n=c(1:x) ##array
y<-colSums(combn(n,3))

i <- 1
m <- 12  ##target value
count <- 0

for(i in 1:length(y))
{if (i < m)
{ 
  print('found combination');
  count++
}
}



