#include <Rcpp.h>
using namespace Rcpp;

//' LBSPR Optimization function for age-based selectivity model
//'
//' Internal optimization function
//'
//' @param starts a vector of starting parameters, relative SL50 (SL50/Linf), deltaSL (SL95-SL50)/Linf, and F/M (in log space)
//' @param x vector of relative ages
//' @param P numeric value indicating proportion of cohort remaining at maximum age
//' @param LMids a vector of the midpoints of the length classes
//' @param LBins a vector of length classes
//' @param LDat a vector of lenght frequencies. Must be same length as LMids
//' @param MK the M/K value
//' @param Linf the Linf value for the population as a hole
//' @param FecB exponent of the length-fecundity relationship
//' @param L50 length at 50 per cent maturity
//' @param L95 length at 95 per cent maturity
//' @param maxsd numeric value - maximum number of standard deviations of length-at-age dist
//' @param CVLinf CV of length-at-age
//' @param Nage number of pseudo age-classes
//' @param usePen logical to use penalty for extreme estimates of selectivity
//' @return negative log-likelihood value
//' @author A. Hordyk
//' @export
// [[Rcpp::export]]
double LBSPR_NLLabsel(NumericVector starts, NumericVector x, double P,  NumericVector LMids, NumericVector LBins,
                    NumericVector LDat, double MK, double Linf, double FecB, double L50, double L95,
                    double maxsd, double CVLinf, int Nage, int usePen) {

  // LBSPR model with pseudo-age classes

  double NLL = 0;
  double FMpar; // estimated F/M
  double SL50;
  double SL95;
  double Delta;

  SL50 = exp(starts(0)) * Linf;
  SL95 = SL50 + exp(starts(1)) * Linf;
  FMpar = exp(starts(2)); // estimated F/M
  Delta = SL95 - SL50;

  double Pen = 0;
  double PenVal = 0;
  int Nlen = LMids.size();

  NumericVector EL(Nage);
  NumericVector rLens(Nage);
  NumericVector SDL(Nage);
  NumericMatrix Prob(Nage, Nlen);
  NumericMatrix Cx(Nage, Nlen);

  for (int age=0; age<Nage; age++) {
    EL(age) = (1-pow(P,x(age)/MK)) * Linf; // length at relative age
  }
  rLens = EL/Linf;
  SDL = EL * CVLinf;
  for (int age=0; age<Nage; age++) {
    Prob(age,0) = R::pnorm((LBins(1) - EL(age))/SDL(age), 0, 1.0, 1, 0);
    for (int L=1; L<(Nlen-1); L++) {
      Prob(age,L) = R::pnorm((LBins(L+1) - EL(age))/SDL(age), 0, 1.0, 1, 0) - R::pnorm((LBins(L) - EL(age))/SDL(age), 0, 1.0, 1, 0);
    }
    Prob(age,(Nlen-1)) = 1- R::pnorm((LBins(Nlen-1) - EL(age))/SDL(age), 0, 1.0, 1, 0);
  }

  for (int age=0; age<Nage; age++) {
    for (int L=0; L<(Nlen-1); L++) {
      if (abs((LMids(L) - EL(age))/SDL(age)) >= maxsd)  Prob(age, L) = 0;
    }
  }

  NumericVector SL = 1.0/(1.+exp(-log(19.)*(LMids-SL50)/Delta));

  NumericVector Sx(Nage);
  NumericVector MSX(Nage);
  NumericVector Ns(Nage);
  NumericVector Nc(Nlen);

  for (int age=0; age<Nage; age++) {
    Sx(age) = sum(Prob(age,_) * SL);
    MSX(age) = sum(Sx)/(age+1);
    Ns(age) = pow((1-rLens(age)), (MK+(MK*FMpar)*MSX(age)));
    for (int L=0; L<(Nlen-1); L++) Cx(age, L) = Prob(age, L) * SL(L);
  }

  for (int L=0; L<(Nlen-1); L++) {
    Nc(L) = sum(Cx(_,L) * Ns);
  }

  Nc = Nc/sum(Nc);

  NumericVector tempVec(Nlen);
  NumericVector lenprob = LDat/sum(LDat);

  NLL = -sum(LDat * log((Nc+1E-15)/(lenprob+1E-15)));

  Pen=0;
  if (usePen==1) {
    PenVal=NLL;
    Pen=R::dbeta(exp(starts(0)), 5.0, 0.01,0) * PenVal;
    if (exp(starts(0)) >= 1) Pen=PenVal*exp(starts(0));
  }

  NLL=NLL+Pen;
  return(NLL);

}


