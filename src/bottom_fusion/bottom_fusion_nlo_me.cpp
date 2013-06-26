#include "BottomFusion_NLO_ME.h"

void BottomFusion_NLO_ME::set(const double inz,const double inlambda,const double inlh)
{
z=inz;
lambda=inlambda;
lh=inlh;
}



//: NLO matrix elements for b bbar -> H +X. Soft terms from virtual and real have already been combined and the 1/e^2 pole has been 
//: canceled analytically. The remaining 1/e is what gets canceled by the NLO convolution. The matrix elements are organized in 
//: soft (those for which J(1) in integrated counterterms, not necessarily having z=1) and hard, and in 1/e, finite and e pieces

double  BottomFusion_NLO_ME::bb_soft_pole(const double & delta,const double &  plus)
{
const double ME = -2.0 * delta - 8.0/3.0 * plus;
return(ME);
}

double  BottomFusion_NLO_ME::bb_soft_finite(const double &  delta,const double &  plus0,const double &  plus1)
{
const double ME = 8.0/3.0*(
                        (pi_square/6.0-1.0/2.0)*delta
                        +2.0*plus1
                        -plus0*lh
                        )
                    //+ 2.0*log_mur_sq_over_muf_sq*delta  // ***  THIS IS THE ONLY mur - DEPENDENT TERM at NLO and it has migrated to convolutions.cpp
                    ;
return(ME);
}

double  BottomFusion_NLO_ME::bb_soft_e(const double &  delta,const double &  plus0,const double &  plus1,const double &  plus2)
{
const double pisquare = pi_square;
const double ME = (2.0/3.0)*plus0*(-2.0*pow(lh,2.0)+pisquare)
                    +(16.0/3.0)*plus1*lh
                    -(16.0/3.0)*plus2
                    +(-8.0/3.0-(4.0/3.0)*lh+(4.0/9.0)*lh*pisquare)*delta;
return(ME);
}



double  BottomFusion_NLO_ME::bb_coll_pole()
{
    const double QQQ = 2.0/3.0 /(1.0-z);
    const double ME = -  (1.0+pow(z,2.0)) * QQQ  ;
    return(ME);
}


double  BottomFusion_NLO_ME::bb_coll_fin()
{
    const double QQQ = 2.0/3.0 /(1.0-z);
    const double LZ = log(z);
    const double LMZ = log(1.0-z);
    const double ME = -QQQ*(-2.0*LMZ-2.0*LMZ*pow(z,2.0)-1.0+2.0*z-pow(z,2.0)+LZ+LZ*pow(z,2.0)+lh+lh*pow(z,2.0));
    return(ME);
}




double  BottomFusion_NLO_ME::bb_coll_e()
{
    const double QQQ = 2.0/3.0 /(1.0-z);
    const double LZ = log(z);
    const double LMZ = log(1.0-z);
    const double ME =  -(1.0/4.0)*QQQ*
                        (-4.0*lh
                         -8.0*LMZ*LZ*pow(z,2.0)
                         +8.0*LZ*z
                         -4.0*lh*pow(z,2.0)
                         -4.0*LZ
                         +8.0*LMZ
                         +4.0*LZ*lh*pow(z,2.0)
                         -4.0*LZ*pow(z,2.0)
                         +8.0*lh*z
                         +2.0*pow(LZ,2.0)*pow(z,2.0)
                         +4.0*LZ*lh+2*pow(lh,2.0)*pow(z,2.0)
                         -8.0*LMZ*lh*pow(z,2.0)
                         -pow(z,2.0)*pi_square
                         +8.0*pow(LMZ,2.0)*pow(z,2.0)
                         +8.0*LMZ*pow(z,2.0)
                         +2.0*pow(lh,2.0)
                         -pi_square
                         +8.0*pow(LMZ,2.0)
                         +2.0*pow(LZ,2.0)
                         -16.0*LMZ*z
                         -8.0*LMZ*LZ
                         -8.0*LMZ*lh);
    return(ME);
}


double  BottomFusion_NLO_ME::bb_coll_soft_pole()
{
    const double QQQ = 2.0/3.0 /(1.0-z);
    const double ME = 4.0*QQQ ;
    return(ME);
}

double  BottomFusion_NLO_ME::bb_coll_soft_fin()
{
    const double QQQ = 2.0/3.0 /(1.0-z);
    const double LZ = log(z);
    const double LMZ = log(1.0-z);
    const double ME= 4.0*QQQ*(lh-2.0*LMZ);
    return(ME);
}


double  BottomFusion_NLO_ME::bb_coll_soft_e()
{
    const double QQQ = 2.0/3.0 /(1.0-z);
    const double LZ = log(z);
    const double LMZ = log(1.0-z);
    const double ME = (2.0*pow(lh,2.0)-pi_square+8.0*pow(LMZ,2.0)-8.0*LMZ*lh)*QQQ;
    return(ME);
}


double  BottomFusion_NLO_ME::bb_hard_fin()
{
const double QQQ = 1.0/lambda/(1.0-lambda);
const double bh1 = (1.0+pow(z,2.0))/(1.0-z) ;
const double bh2 = 1.0-z;
const double ME = (2.0/3.0)*bh1*QQQ;
return(ME);
}

double  BottomFusion_NLO_ME::bb_hard_e()
{
const double QQQ = 1.0/lambda/(1.0-lambda);
const double bh1 = (1.0+pow(z,2.0))/(1.0-z) ;
const double bh2 = 1.0-z;
const double Lz = log(z/pow(1.0-z,2.0));
const double LL = log(lambda*(1.0-lambda));
const double ME = (-(2.0/3.0)*bh1*QQQ*LL+(2.0/3.0*(bh2+(Lz+lh)*bh1))*QQQ);
return(ME);
}




//: NLO matrix elements for b g -> H +g. 

double  BottomFusion_NLO_ME::bg_coll_pole()
{
    const double ps = pow(z,2.0)+pow(1.0-z,2.0);
    const double ME = -(1.0/4.0)*ps ;
    return(ME);
}

double  BottomFusion_NLO_ME::bg_coll_fin()
{
    const double LZ = log(z/pow(1.0-z,2.0));
    const double ps = (pow(z,2.0)+pow(1.0-z,2.0));
    const double ME = (-1.0/4.0)* ( ps*(LZ+lh)+ps-1.0 + ps/(1.0-lambda));
    return(ME);
}



double  BottomFusion_NLO_ME::bg_coll_e()
{
const double Lz = log(z/pow(1.0-z,2.0));
const double LLZ = log(z/pow(1.0-z,2.0)/lambda/(1.0-lambda));
const double ps = (pow(z,2.0)+pow(1.0-z,2.0));
const double ME =  1.0/4.0 * (1.0-(lh+1.0+LLZ)*ps)/(1.0-lambda)
                    +1.0/4.0*(
                              -(1.0/2.0)*ps*pow(Lz,2.0)
                              +(-ps*lh-ps+1.0)*Lz
                              -(1.0/2.0)*ps*pow(lh,2.0)
                              +(-ps+1.0)*lh
                              +(1.0/4.0)*pi_square
                              +(1.0/2.0*((1.0/2.0)*ps-1.0/2.0+z))*pi_square
                              -(1.0/2.0)*z*pi_square
                              -ps
                              +1.0
                              );
return(ME);
}



double  BottomFusion_NLO_ME::bg_hard_fin()
{
    const double WW=z+(1.0-z)*lambda;
    const double QQ=1.0-(1.0-z)*(1.0-lambda);
    const double pzL = pow(z,2.0)+pow(1.0-z,2.0)*pow(lambda,2.0);
    const double Az = pzL*QQ/WW;
    const double ME =   (1.0/4.0)*Az/(1.0-lambda) ;
    return(ME);
    
}

double  BottomFusion_NLO_ME::bg_hard_e()
{
    const double WW=z+(1.0-z)*lambda;
    const double QQ=1.0-(1.0-z)*(1.0-lambda);
    const double pzL = pow(z,2.0)+pow(1.0-z,2.0)*pow(lambda,2.0);
    const double LLZ = log(z/pow(1.0-z,2.0)/lambda/(1.0-lambda));
    const double Az = (-WW  +(lh+1.0)*pzL/WW)*QQ+LLZ*pzL*QQ/WW;
    const double ME =   (1.0/4.0)*Az/(1.0-lambda) ;
    return(ME);
}


//: NLO matrix elements for g b -> H +g. 
double  BottomFusion_NLO_ME::gb_coll_pole()
{
    const double ps = pow(z,2.0)+pow(1.0-z,2.0);
    const double ME = -(1.0/4.0)*ps ;
    return(ME);
}
                                   
double  BottomFusion_NLO_ME::gb_coll_fin()
{
    const double lambda_bar = 1.0-lambda;
    const double LZ = log(z/pow(1.0-z,2.0));
    const double ps = (pow(z,2.0)+pow(1.0-z,2.0));
    const double ME = (-1.0/4.0)* ( ps*(LZ+lh)+ps-1.0 + ps/(1.0-lambda_bar));
    return(ME);
}
                                       
                                       
                                       
double  BottomFusion_NLO_ME::gb_coll_e()
{
    const double lambda_bar = 1.0-lambda;
    const double Lz = log(z/pow(1.0-z,2.0));
    const double LLZ = log(z/pow(1.0-z,2.0)/lambda_bar/(1.0-lambda_bar));
    const double ps = (pow(z,2.0)+pow(1.0-z,2.0));
    const double ME =  1.0/4.0 * (1.0-(lh+1.0+LLZ)*ps)/(1.0-lambda_bar)
            +1.0/4.0*(
                      -(1.0/2.0)*ps*pow(Lz,2.0)
                      +(-ps*lh-ps+1.0)*Lz
                      -(1.0/2.0)*ps*pow(lh,2.0)
                      +(-ps+1.0)*lh
                      +(1.0/4.0)*pi_square
                      +(1.0/2.0*((1.0/2.0)*ps-1.0/2.0+z))*pi_square
                      -(1.0/2.0)*z*pi_square
                      -ps
                      +1.0
                      );
    return(ME);
}
                                       
                                       
                                       
double  BottomFusion_NLO_ME::gb_hard_fin()
{
    const double lambda_bar = 1.0-lambda;
    const double WW=z+(1.0-z)*lambda_bar;
    const double QQ=1.0-(1.0-z)*(1.0-lambda_bar);
    const double pzL = pow(z,2.0)+pow(1.0-z,2.0)*pow(lambda_bar,2.0);
    const double Az = pzL*QQ/WW;
    const double ME =   (1.0/4.0)*Az/(1.0-lambda_bar) ;
    return(ME);
}
                                       
double  BottomFusion_NLO_ME::gb_hard_e()
{
    const double lambda_bar = 1.0-lambda;
    const double WW=z+(1.0-z)*lambda_bar;
    const double QQ=1.0-(1.0-z)*(1.0-lambda_bar);
    const double pzL = pow(z,2.0)+pow(1.0-z,2.0)*pow(lambda_bar,2.0);
    const double LLZ = log(z/pow(1.0-z,2.0)/lambda_bar/(1.0-lambda_bar));
    const double Az = (-WW  +(lh+1.0)*pzL/WW)*QQ+LLZ*pzL*QQ/WW;
    const double ME =   (1.0/4.0)*Az/(1.0-lambda_bar) ;
    return(ME);
}

