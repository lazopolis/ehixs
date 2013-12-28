/** \file splitting_kernels.h
  *
  * Defines all splitting kernels
  *
  * \author, Stefu Bulli
  */

#ifndef SPLITTING_H
#define SPLITTING_H

double Li2(const double&);
double series_li2(const double&);

double pgg0_d1(const double& NF);
double pgg0_DD(const double&);
double pgg0_DDb(const double&);
double pgg0_reg(const double&);
double pgq0_reg(const double&);
double pqg0_reg(const double&);
double pqq0_d1();
double pqq0_d1(const double&);//: for reasons of uniformity :)
double pqq0_DD(const double&);
double pqq0_DDb(const double&);
double pqq0_reg(const double&);
double no_kernel(const double&);

// NLO Splitting kernels
double pgg1_d1(const double& NF);
double pgg1_DD(const double& NF,const double&);
double pgg1_DDb(const double& NF,const double&);
double pgg1_reg(const double& NF,const double&);
double pgq1_reg(const double& NF,const double&);
double pqg1_reg(const double&);
double pqq1_d1(const double& NF);
double pqq1_DD(const double& NF,const double&);
double pqq1_DDb(const double& NF,const double&);
double pqq1_reg(const double& NF,const double&);
double pqqbar1_reg(const double&);
double pqQ1_reg(const double&);

// LO Splitting kernel convolutions
double pgg0pgg0_d1(const double& NF);
double pgg0pgg0_DD(const double& NF,const double&);
double pgg0pgg0_DDb(const double& NF,const double&);
double pgg0pgg0_reg(const double& NF,const double&);
double pgg0pgq0_reg(const double& NF,const double&);
double pgg0pqg0_reg(const double& NF,const double&);
double pgg0pqq0_d1(const double& NF);
double pgg0pqq0_DD(const double& NF,const double&);
double pgg0pqq0_DDb(const double& NF,const double&);
double pgg0pqq0_reg(const double& NF,const double&);
double pgq0pgq0_reg(const double&);
double pgq0pqg0_reg(const double&);
double pgq0pqq0_reg(const double&);
double pqg0pqg0_reg(const double&);
double pqg0pqq0_reg(const double&);
double pqq0pqq0_d1();
double pqq0pqq0_DD(const double&);
double pqq0pqq0_DDb(const double&);
double pqq0pqq0_reg(const double&);

double null_kernel(const double&);

class Kernel{
public:
     Kernel(int iparton,int from_parton,int a_power,int e_power);
     double delta();
     double reg(const double& x);
     double DDB();
     double DD(const double& x);
     
private:
     double (*delta_ptr)(const double&);
     double (*plus_ptr)(const double&);
     double (*bound_ptr)(const double&);
     double (*reg_ptr)(const double&);

};


#endif
