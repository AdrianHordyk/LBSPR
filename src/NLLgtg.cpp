#include <Rcpp.h>
using namespace Rcpp;

//' LBSPR Optimization function for GTG model
//'
//' Internal optimization function
//'
//' @param starts a vector of starting parameters, relative SL50 (SL50/Linf), deltaSL (SL95-SL50)/Linf, and F/M (in log space)
//' @param LMids a vector of the midpoints of the length classes
//' @param LBins a vector of length classes
//' @param LDat a vector of lenght frequencies. Must be same length as LMids
//' @param gtgLinfs a vector of Linfs for the growth-type-groups
//' @param MKMat a matrix of M/K for each GTG and length-class
//' @param MK the M/K value
//' @param Linf the Linf value for the population as a hole
//' @param ngtg the number of growth-type-groups
//' @param recP a vector of recruitment by GTG
//' @param usePen logical to use penalty for extreme estimates of selectivity
//' @return negative log-likelihood value
//' @author A. Hordyk
//' @export
// [[Rcpp::export]]
double LBSPR_NLLgtg(NumericVector starts, NumericVector LMids, NumericVector LBins,
  NumericVector LDat, NumericVector gtgLinfs, NumericMatrix MKMat, double MK,
  double Linf, int ngtg, NumericVector recP, int usePen) {

  double NLL = 0;
  double FMpar; // estimated F/M
  double SL50;
  double SL95;
  double Delta;
  double FK;

  double Pen;
  double PenVal;

  int nbin = LMids.size()+1.0;
  int nmid = LMids.size();
  int i;
  int g;

  NumericVector tempVec(nmid);
  NumericVector SelLen(nbin);
  NumericVector FKL(nbin);
  NumericVector SelLen2(nmid);
  NumericVector PLen(nmid);

  NumericMatrix NPRFished(nbin,ngtg);
  NumericMatrix NatLF(nmid,ngtg);
  NumericMatrix NatLC(nmid,ngtg);
  NumericMatrix ZKLMat(nbin,ngtg);

  SL50 = exp(starts(0)) * Linf;
  SL95 = SL50 + exp(starts(1)) * Linf;
  FMpar = exp(starts(2)); // estimated F/M
  Delta = SL95 - SL50;
  FK=FMpar*MK; // F/K ratio
  SelLen=1.0/(1.+exp(-log(19.)*(LBins-SL50)/Delta));
  SelLen2=1.0/(1.+exp(-log(19.)*(LMids-SL50)/Delta));
  for (g=0;g<ngtg;g++){
	NPRFished(0,g)=recP(g);
	ZKLMat(0,g)=FK*SelLen(0);
	for(i=1;i<nbin;i++){
	  FKL(i)=FK*SelLen(i);
	  ZKLMat(i,g)=MKMat(i,g)+FKL(i);
	  NPRFished(i,g)=NPRFished(i-1,g)*pow(((gtgLinfs(g)-LBins(i))/(gtgLinfs(g)-LBins(i-1))),ZKLMat(i-1,g));
	  if (LBins(i)>gtgLinfs(g)) NPRFished(i,g)=0;
	}
  }
  for(i=0;i<nmid;i++){
	double total=0;
	for (int g=0; g<ngtg;g++){
	  NatLF(i,g)=(NPRFished(i,g)-NPRFished(i+1,g))/ZKLMat(i,g);
      NatLC(i,g)=NatLF(i,g)*SelLen2(i);
	  total +=NatLC(i,g);
    }
	PLen(i)=total;
  }
  PLen=PLen/sum(PLen);
  for(i=0;i<nmid;i++){
    tempVec(i) =LDat(i) * log(PLen(i)+0.000001)-log(LDat(i)+0.000001);
  }
  NLL=-sum(tempVec);

  Pen=0;
  if (usePen==1) {
	PenVal=NLL;
	Pen=R::dbeta(exp(starts(0)), 5.0, 0.01,0) * PenVal;
	if (exp(starts(0)) >= 1) Pen=PenVal*exp(starts(0));
  }
  NLL=NLL+Pen;
  return(NLL);
  // return(PLen);
}


