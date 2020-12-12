#' @title Benchmark R and Rcpp functions.
#' @name benchmarks
#' @description Use R package \code{microbenchmark} to compare the performance of C functions (\code{gibbsR} and \code{vaccR}) and Cpp functions (\code{gibbsC} and \code{vaccC}).
#' @import microbenchmark
#' @importFrom Rcpp evalCpp
#' @importFrom stats rnorm rgamma
#' @useDynLib StatComp20058
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' tm1 <- microbenchmark::microbenchmark(
#'   rnR = gibbsR(100,10),
#'   rnC = gibbsC(100,10)
#' )
#' print(summary(tm1)[,c(1,3,5,6)])
#' 
#' tm2 <- microbenchmark::microbenchmark(
#'   vR = vaccR(age,female,ily),
#'   vC = vaccC(age,female,ily)
#' )
#' print(summary(tm2)[,c(1,3,5,6)])
#' }
NULL

#' @title Use three inputs to predict response using R.
#' @description The prediction model is described in http://www.babelgraph.org/wp/?p=358.
#' @param age the first predictor (numeric)
#' @param female the second predictor (logical)
#' @param ily the third predictor (logical)
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' data(data)
#' attach(data)
#' res <- vaccR(age,female,ily)
#' }
#' @export
vaccR <- function(age, female, ily) {
  p <- 0.25 + 0.3 * 1 / (1 - exp(0.04 * age)) + 0.1 * ily
  p <- p * ifelse(female, 1.25, 0.75)
  p <- pmax(0, p)
  p <- pmin(1, p)
  p
}

#' @title A Gibbs sampler using R
#' @description A Gibbs sampler using R
#' @param N the number of samples
#' @param thin the number of between-sample random numbers
#' @return a random sample of size \code{n}
#' @examples
#' \dontrun{
#' rnR <- gibbsR(100,10)
#' par(mfrow=c(2,1));
#' plot(rnR[,1],type='l')
#' plot(rnR[,2],type='l')
#' }
#' @export
gibbsR <- function(N, thin) {
  mat <- matrix(nrow = N, ncol = 2)
  x <- y <- 0
  for (i in 1:N) {
    for (j in 1:thin) {
      x <- rgamma(1, 3, y * y + 4)
      y <- rnorm(1, 1 / (x + 1), 1 / sqrt(2 * (x + 1)))
    }
    mat[i, ] <- c(x, y)
  }
  mat
}


#' @title A Metropolis sampler using R
#' @description A Metropolis sampler using R
#' @param sigma the sigma of samples
#' @param x0 
#' @param N 
#' @return a random sample of size \code{N}
#' @examples
#' \dontrun{
#' x0<-25
#' sigma<-c(0.05,1,2,16)
#' N<-3000
#' M1=MetropolisR(sigma[1],x0,N)
#' M2=MetropolisR(sigma[2],x0,N)
#' M3=MetropolisR(sigma[3],x0,N)
#' M4=MetropolisR(sigma[4],x0,N)
#' acceptanceC<-1-c(M1[[2]][1],M2[[2]][1],M3[[2]][1],M4[[2]][1])/N
#' index<-1:2000
#' My1<-M1[[1]][index]
#' My2<-M2[[1]][index]
#' My3<-M3[[1]][index]
#' My4<-M4[[1]][index]
#'opar<-par(no.readonly = T)
#' par(mfrow=c(2,2))
#' plot(index,My1,type = "l",xlab = "sigma=0.05")
#' plot(index,My2,type = "l",xlab = "sigma=1")
#' plot(index,My3,type = "l",xlab = "sigma=2")
#' plot(index,My4,type = "l",xlab = "sigam=16")
#' }
#' @export
MetropolisR<-function(sigma,x0,N){
  x<-numeric(N)
  x[1]<-x0
  u<-runif(N)
  k<-0
  for(i in 2:N){
    y<-rnorm(1,x[i-1],sigma)
    if(u[i]<=(exp(-abs(y))/exp(-abs(x[i-1]))))
      x[i]<-y else{
        x[i]<-x[i-1]
        k<-k+1
      }
  }
  return(list(x=x,k=k))
}


