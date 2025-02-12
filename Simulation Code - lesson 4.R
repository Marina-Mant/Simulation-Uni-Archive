#ΜΕΘΟΔΟΣ ΑΝΤΙΣΤΡΟΦΗΣ (παράγουμε έναν τ.α. U ~ U(0,1) και θέτουμε X=F^(-1)(U))
#Για ομοιόμορφη: F(x)=u => (X-a)/(b-a)=u => x=a+(b-a)*U

n=10^5; a=10; b=20;
U=runif(n); X=a+(b-a)*U;
hist(X)

#Για εκθετική:
n=10^5; lambda=3; U=runif(n);
X=-log(U)/lambda
#αλγεβρικα βγαινει στην παρένθεση (1-U) όμως και τα 2 ~ U(0,1)
#print(X)
#mean(X)
#hist(X)
hist(X,seq(0,4,0.1))
hist(X[X<4],seq(0,4,0.1), prob=TRUE)
hist(X,100)

x=seq(0,4,0.1); lines(x,lambda*exp(-lambda*x), type="l", col="slateblue")

#Gumbel κατανομή: f(x)=exp(-(exp(-x)))*exp(-x)
#X= - lmu(-lnu)

n=10^5; U=runif(n);
X=-log(-log(U));
mean(X)
hist(X,100, prob=TRUE)
x=seq(-3,6,0.1);lines(x,exp(-x)*exp(-exp(-x)), type="l", col="slateblue")
sd(X)

#Κατανομη Γάμμα (δεν εχει κλειστο τυπο κατανομης αρα ουτε και αντιστροφη)

#Περιπτωση Γαμμα Erlang

lambda=6; k=12; n=10^5;
X=rep(0,n)
for(i in 1:n){U=runif(k); X[i]=sum(-log(U)/lambda)}
hist(X,100, prob=TRUE)
mean(X)
x=seq(0,5,0.05); lines(x,dgamma(x,k,lambda), col="slateblue", lwd=4)


#Κανονική κατανομή Ν(0,1) (μεσω U)
n=10^6; X=rep(0,n);
for(i in 1:n){X[i]= sum(runif(12))-6}
hist(X,100, prob= TRUE)
x=seq(-4,4,0.05);
lines(x,dnorm(x,0,1), col="cyan4", lwd=4 )

#Και τώρα μέσω της έτοιμης qnorm

U=runif(n); n=10^6;
X=qnorm(U,0,1)
hist(X,200,prob=TRUE)
x=seq(min(X),max(X),0.05); lines(x,dnorm(x,0,1), col="orange", lwd=4)
#(^καλυτερη προσεγγιστικη μεθοδος απο την προηγουμενη προφανως)


#Ακριβεστερη μέθοδος για κανονική: Μεθοδος Box Muller

#Πρώτα παράγουμε έναν τ.α. R2=-1/lambda*log(U)

R2=-2*log(runif(1));

#Τώρα θα παράξουμε και τη γωνία θ

Theta=runif(1)*2*pi;
c(R2,Theta)
Z1=sqrt(R2)*cos(Theta); Z2=sqrt(R2)*sin(Theta);
c(Z1,Z2)

#Τώρα θα αυξήσουμε το πλήθος
n=10^4; R2=-2*log(runif(n)); Theta=runif(n)*2*pi;

Z1=sqrt(R2)*cos(Theta); Z2=sqrt(R2)*sin(Theta);

#Τσεκάρουμε αν τα Z προέρχονται από την κανονική κατανομή

#hist(Z2,100, prob=TRUE)
#x=seq(-4,4,0.05); lines(x,dnorm(x,0,1), col="red2", lwd=4)

plot(Z1,Z2)
#οσο αυξανουμε το πλήθος βλεπουμε οτι σχεδον ολα τα στοιχεια
#συγκεντρώνονται στο κεντρο του plot, αρα μιλαμε για μια
#διδιαστατη κανονικη κατανομη

mu1=3; sigma1=2; mu2=3; sigma2=4;
X1=mu1+sigma1*Z1; X2=mu2+sigma2*Z2
plot(X1,X2, asp=1)









