#include <Rcpp.h>
using namespace Rcpp;

//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param N the number of samples
//' @param thin the number of between-sample random numbers
//' @return a random sample of size \code{n}
//' @examples
//' \dontrun{
//' rnC <- gibbsC(100,10)
//' par(mfrow=c(2,1));
//' plot(rnC[,1],type='l')
//' plot(rnC[,2],type='l')
//' }
//' @export
// [[Rcpp::export]]
NumericMatrix gibbsC(int N, int thin) {
  NumericMatrix mat(N, 2);
  double x = 0, y = 0;
  for(int i = 0; i < N; i++) {
    for(int j = 0; j < thin; j++) {
      x = rgamma(1, 3, 1 / (y * y + 4))[0];
      y = rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))[0];
    }
    mat(i, 0) = x;
    mat(i, 1) = y;
  }
  return(mat);
}


#include <Rcpp.h>
using namespace Rcpp;

double vacc3a(double age, bool female, bool ily){
  double p = 0.25 + 0.3 * 1 / (1 - exp(0.04 * age)) + 0.1 * ily;
  p = p * (female ? 1.25 : 0.75);
  p = std::max(p, 0.0);
  p = std::min(p, 1.0);
  return p;
}

//' @title Use three inputs to predict response using Rcpp.
//' @description The prediction model is described in http://www.babelgraph.org/wp/?p=358.
//' @param age the first predictor (numeric)
//' @param female the second predictor (logical)
//' @param ily the third predictor (logical)
//' @return a random sample of size \code{n}
//' @examples
//' \dontrun{
//' data(data)
//' attach(data)
//' res <- vaccC(age,female,ily)
//' }
//' @export
// [[Rcpp::export]]
NumericVector vaccC(NumericVector age, LogicalVector female,
                    LogicalVector ily) {
  int n = age.size();
  NumericVector out(n);
  for(int i = 0; i < n; ++i) {
    out[i] = vacc3a(age[i], female[i], ily[i]);
  }
  return out;
}

#include <Rcpp.h>
using namespace Rcpp;

NumericVector runifC(int n){
  NumericVector a(n);
  srand((unsigned)time(NULL)); 
  for(int i = 0; i < n;i++ )
    a[i]=rand()/double(RAND_MAX);
  return a;
}  /* Generate n random numbers between 0 and 1 */

double rnormC(double E,double sd){
  static double V1,V2,S;
  static int phase=0;
  double X;
  if (phase == 0){
    do {
      double U1 = (double)rand()/RAND_MAX;
      double U2 = (double)rand()/RAND_MAX;
      
      V1 = 2 * U1 - 1;
      V2 = 2 * U2 - 1;
      S = V1 * V1 + V2 * V2;
    } while(S >= 1 || S == 0);
    X = V1 * sqrt(-2 * log(S) / S);
  } else
    X = V2 * sqrt(-2 * log(S) / S);  
  phase = 1 - phase;
  X = sd*X+E;
  return X;
} /* Generate 1 normally distributed random number with expectation E and standard deviation sd */


//' @title A Gibbs sampler using Rcpp
//' @description A Gibbs sampler using Rcpp
//' @param sigma the sigma of samples
//' @param x0 
//' @param N 
//' @return a random sample of size \code{N}
//' @examples
//' \dontrun{
//' x0<-25
//' sigma<-c(0.05,1,2,16)
//' N<-3000
//' M1=MetropolisC(sigma[1],x0,N)
//' M2=MetropolisC(sigma[2],x0,N)
//' M3=MetropolisC(sigma[3],x0,N)
//' M4=MetropolisC(sigma[4],x0,N)
//' acceptanceC<-1-c(M1[[2]][1],M2[[2]][1],M3[[2]][1],M4[[2]][1])/N
//' index<-1:2000
//' My1<-M1[[1]][index]
//' My2<-M2[[1]][index]
//' My3<-M3[[1]][index]
//' My4<-M4[[1]][index]
//' opar<-par(no.readonly = T)
//' par(mfrow=c(2,2))
//' plot(index,My1,type = "l",xlab = "sigma=0.05")
//' plot(index,My2,type = "l",xlab = "sigma=1")
//' plot(index,My3,type = "l",xlab = "sigma=2")
//' plot(index,My4,type = "l",xlab = "sigam=16")
//' }
//' @export
// [[Rcpp::export]]
List MetropolisC(double sigma,double x0,int N){
  NumericVector x(N);
  x[0]=x0;
  NumericVector u(N);
  u=runifC(N);
  double y;
  int k = 0;
  for (int i=1;i<N;i++) {
    y = rnormC(x[i-1],sigma);
    if (u[i] <= exp(abs(x[i-1])-abs(y))){
      x[i] = y;
      k = k+1;
    }
    else {
      x[i] = x[i-1];
    }
  }
  List out(2);
  out[0] = x;
  out[1] = k;
  return out;
}


