a="ACGTGA"
b="GTGA"
match=1
mismatch=-1
gap=-2
seq.a=unlist(strsplit(a,''))
seq.a
seq.b=unlist(strsplit(b,''))
seq.b=c(0,seq.b)
seq.b
seq.a
seq.a=c(0,seq.a)
seq.a


### Initialization of matrix
score=matrix(NA,length(seq.a),length(seq.b))
score
score[,1]=sapply(1:length(seq.a)-1,function(x) x * gap)
score[1,]=sapply(1:length(seq.b)-1,function(x) x * gap)
score


###### dynamic Programming
for (i in 2:length(seq.a)){
  for (j in 2:length(seq.b)){
    if (seq.a[i]==seq.b[j]){
      score[i,j]=score[i-1,j-1]+match
      } else{
        score[i,j]=score[i-1,j-1]+mismatch
        }
    a=score[i-1,j] + gap
    if (a>score[i,j]){
      score[i,j]=a
      a=score[i,j-1] + gap
    if (a>score[i,j]){
      score[i,j] = a
    }
    }
  }
}
print(score)

##### traceback
i=length(seq.a)
j=length(seq.b) 
ax=character()
bx=character()
while(i>1 && j>1){
  a=score[i-1,j-1]
  if (seq.a[i]==seq.b[j]){
    a=a+match
  }else {
    a = a+mismatch
    
  }
  #print(a)
  if (score[i,j]==a){
    ax.character=c(seq.a[i],ax)
    bx.character=c(seq.b[j],bx)
    i= i -1
    j = j -1
  }
  #a=score[i-1,j]+gap
  if (score[i,j]==score[i-1,j]+gap){
    
    ax.character=c(seq.a[i],ax)
    bx=c("-",bx)
    i = i-1
    next
  }
  #a=score[i,j-1] + gap
  #print(a)
  if (score[i,j]==score[i,j-1] + gap){
    ax.character=c("-",ax)
    bx.character=c(seq.b[j],bx)
    j = j - 1
    next
    
  }
  print(ax)
  print(bx)
}

