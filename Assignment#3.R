i<-0
for (i in 1:100){
    if (i%%3==0 && i%%5!=0){
    print("fizz")
  }else if(i%%5==0 && i%%3!=0){
    print("buzz")
  }else if(i%%15==0){
    print("fizzbuzz")
  }else{
    print(i)
  }
}

