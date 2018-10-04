
P0=0
Pinf=1
alpha=0.05
n=0:100

Pn=Pinf-(Pinf-P0)*exp(-alpha*n)
plot(n,Pn)

Pn[2:100]-Pn[1:99]


#Heathcote paper:

A = 00 #assymptote
B = 100 #change expected from beginning to end
RT0 = A+B
alpha=0.01
n=seq(0,10,by=.01)

RTn = A+B*exp(-alpha*n)
plot(n,RTn)   

rate=-alpha*(RTn-A)
rate2=diff(RTn)

rate[1:10]
rate2[1:10]


Pn2=numeric(101)
for(i in n){
  if(i==0){
    Pn2[i+1]=0
  } else {
    Pn2[i+1]= Pinf-(Pinf-P0)*exp(-alpha) + Pn2[i]
  }
}
plot(n,Pn2,add=T)