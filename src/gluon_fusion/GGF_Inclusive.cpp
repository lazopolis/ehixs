
#include "GGF_Inclusive.h"
#include "CConstants.h"
#include "chaplin.h"

#include <iostream>
using namespace std;


double gg_LO_delta();
double gg_NLO_delta();
double gg_NNLO_delta();

double gg_NLO_plus(double);
double gg_NLO_reg(double);

double gg_NNLO_plus(double);
double gg_NNLO_reg(double);


GGF_matrix_element::GGF_matrix_element(const string & _pi,const string & _pj,const string& _pord,const string & _name)
{
     parton_i=_pi;
     parton_j=_pj;
     name = _name;
     if (_pord=="LO") alpha_power=0;
     else if (_pord=="NLO") alpha_power=1;
     else if (_pord=="NNLO") alpha_power=2;
     else if (_pord=="N3LO") alpha_power=3;
     else {cout<<"\n unrecognized pord when constructing MatrixElement"<<endl;exit(1);}
     
}

ostream& operator<<(ostream& stream, const GGF_matrix_element& ME)
{
     stream<<"S("<<ME.parton_i<<","<<ME.parton_j<<","<< ME.name<<",a^"<<ME.alpha_power<<" ,dim="<<ME.dimension<<")";
     
     return stream;
}

GGF_Inclusive::GGF_Inclusive(const UserInterface& UI)
{
     //: assigning matrix elements
     matrix_elements.push_back(new GGF_matrix_element_delta("gluon","gluon","LO","LO delta",&gg_LO_delta));
     matrix_elements.push_back(new GGF_matrix_element_delta("gluon","gluon","NLO","NLO delta",&gg_NLO_delta));
     matrix_elements.push_back(new GGF_matrix_element_delta("gluon","gluon","NNLO","NNLO delta",&gg_NNLO_delta));
     matrix_elements.push_back(new GGF_matrix_element_plus("gluon","gluon","NLO","NLO plus",&gg_NLO_plus));
     matrix_elements.push_back(new GGF_matrix_element_plus("gluon","gluon","NNLO","NNLO plus",&gg_NNLO_plus));
     matrix_elements.push_back(new GGF_matrix_element_reg("gluon","gluon","NLO","NLO reg",&gg_NLO_reg));
     matrix_elements.push_back(new GGF_matrix_element_reg("gluon","gluon","NNLO","NNLO reg",&gg_NNLO_reg));
     //: combining in sectors
     make_sectors();

}

void GGF_Inclusive::make_sectors()
{
     
     
     
     //cout<<"\n specified pdfs "<<F1<<" , "<<F2<<" and e-order = "<<Eorder<<endl;
     vector<ExpansionTerm*> WCET_vector;
     vector<ExpansionTerm*> ZREN_vector;
     
     vector<ExpansionTerm*> AREN_vector;
     vector<ExpansionTerm*> AREN_vector_trivial;
     //: constructing the wilson coefficient factor [a*(c0+a*c1+a^2*c2)]^2
     //vector<ExpansionTerm*> WCET_vector;
     WCET_vector.push_back(new ExpansionTerm("(c0^2 a^2)",pow(WC.c0,2.0),2,0));
     WCET_vector.push_back(new ExpansionTerm("(2*c0*c1* a^3)",2.0*WC.c0*WC.c1,3,0));
     WCET_vector.push_back(new ExpansionTerm("[(c1^2 + 2*c0*c2)*a^4]",
                                             pow(WC.c1,2.0) + 2.0*WC.c0*WC.c2,4,0));
     
     //: a^2 * Z^2 = { a * (1+a*b0*L + a^2*b1*L + a^2*b0^2*L^2) * ( 1 - a*b0/e + a^2 * b0^2/e^2-a^2*b1/e) }^2
     ZREN_vector.push_back(new ExpansionTerm("(1)",1.0,0,0));
     ZREN_vector.push_back(new ExpansionTerm("(a)*(2*b0*L)",2.0*beta.zero*log_mur_sq_over_muf_sq,1,0));
     ZREN_vector.push_back(new ExpansionTerm("(a^2)*(3*b0^2*L^2)",3.0*pow(beta.zero,2.0)*pow(log_mur_sq_over_muf_sq,2),2,0));
     ZREN_vector.push_back(new ExpansionTerm("(a^2)*(2*b1*L)",2.0*beta.one*log_mur_sq_over_muf_sq,2,0));
     
     
     AREN_vector.push_back(new ExpansionTerm("(1)",1.0,0,0));
     AREN_vector.push_back(new ExpansionTerm("(a)*b0*L)",beta.zero*log_mur_sq_over_muf_sq,1,0));
     
     //AREN_vector.push_back(new ExpansionTerm("(-b0*a*lh)",-beta.zero*lh,1,0));
     int maximum_alpha_power=2;

     //: constructing alpha_vector = [[1],[a,a*b0*L],[a^2,a^2*2*b0*L,a^2*b0^2*L^2],...]
     vector<ExpansionTerm> VE;
     //: constructing sigma_LO+sigma_NLO*a(1+b0*L)+sigma_NNLO*a^2(1+b0*L)^2+...
     /*
     vector<ExpansionTerm*> SIGMA_vector;
     for (int ime=0;ime<matrix_elements.size();ime++)
          {
          GGF_matrix_element* cur_me=matrix_elements[ime];
          
          }

     for (int ime=0;ime<maximum_alpha_power;ime++)
          {
          if (matching_mes[ime]->alpha_power<=Sorder)
               {
               MatrixElement* cur_me=matching_mes[ime];
               //: assigning trivial renormalization factor (1)
               vector<ExpansionTerm*> cur_aren=AREN_vector_trivial;
               //: assigning  -b0 * a exp(e*lh) / e if the matrix element is nlo
               if (cur_me->alpha_power==1) cur_aren=AREN_vector;
               for (int iwc=0;iwc<WCET_vector.size();iwc++)
                    {
                    for (int izren=0;izren<ZREN_vector.size();izren++)
                         {
                         for (int iaren=0;iaren<cur_aren.size();iaren++)
                              {
                              
                              int total_alpha_order = WCET_vector[iwc]->give_a_power()
                              +ZREN_vector[izren]->give_a_power()
                              +cur_aren[iaren]->give_a_power()
                              +cur_me->alpha_power;
                              int total_epsilon_order = WCET_vector[iwc]->give_e_power()
                              +ZREN_vector[izren]->give_e_power()
                              +cur_aren[iaren]->give_e_power()
                              +cur_me->epsilon_power;
                              if (total_alpha_order==Sorder and total_epsilon_order==Eorder)
                                   {
                                   vector<ExpansionTerm*> factors;
                                   factors.push_back(WCET_vector[iwc]);
                                   factors.push_back(ZREN_vector[izren]);
                                   factors.push_back(cur_aren[iaren]);
                                   
                                   available_sectors.push_back(new SimpleSector(F1,F2,factors,cur_me));
                                   //cout<<" : success";
                                   }
                              else
                                   {
                                   //cout<<"failure because (a,e)= ("<<total_alpha_order<<","<<total_epsilon_order
                                   //<<") != ("<<Sorder<<","<<Eorder<<")";
                                   }
                              }
                         }
                    }
               }
          
          }
 */
}

void perform()
{

}

double gg_LO_delta()
{
     return 1.0;
}

double gg_NLO_delta()
{
     return consts::pi_square;
}

double gg_NNLO_delta()
{
     return    -120.0*consts::z3
               +(16.0/3.0)*consts::pi_square
               +consts::nf*((5.0/6.0)*consts::z3
                            -(4.0/9.0)*consts::pi_square
                            -131.0/18.0)
               -(9.0/80.0)*pow(consts::pi_square,2.0)
               +813.0/16.0;
}

double DD(int i,double x)
{
     if (i==0) return 1.0;
     else return pow(log(1.0-x),double(i));
}


double gg_NLO_plus(double x)
{
     double L=0.0;
     double res= -6.0*L*DD(0, x)+12.0*DD(1, x);
     return res;
}


double gg_NNLO_plus(double x)
{
     double res = (-122.0/3.0+(639.0/2.0)*consts::z3
                   +(131.0/8.0)*consts::pi_square
                   +consts::nf*(26.0/9.0-(7.0/12.0)*consts::pi_square)
                   )*DD(0, x)
                    +(136.0-57.0*consts::pi_square-(16.0/3.0)*consts::nf)*DD(1, x)
                    +(-174.0+4.0*consts::nf)*DD(2, x)
                    +168.0*DD(3, x);
     
     
     return res;
}






double gg_NLO_reg(double x)
{
     double x_sq = x*x;
     double x_cube = x_sq*x;
     double res=6.0*pow(x_sq-x+1.0,2.0)*log(x)/((-1.0+x)*x)
               -(12.0*(2.0*x-1.0-x_sq+x_cube))*log(1.0-x)/x
               +(1.0/2.0)*(57.0*x-23.0-45.0*x_sq+23.0*x_cube)/x;
     return res;
}


double polylog(int m, double x)
{
     if (m==2) return HPL2real(0,1,x,0.0);
     if (m==3) return HPL3real(0,0,1,x,0.0);
     if (m==4) return HPL4real(0,0,0,1,x,0.0);
     cout<<"\nFUCK YOU "<<endl;
     exit(1);
     
}

double gg_NNLO_reg(double x)
{
     complex<double> res=-0.5e1 / 0.3e1 * consts::z3 / x - 0.55e2 / 0.2e1 * x * consts::z3 + 0.425e3 / 0.108e3 * 0.3141592654e1 * 0.3141592654e1 / (-0.1e1 + x) / x - 0.207193e6 / 0.216e3 * x - 0.5e1 * 0.3141592654e1 * 0.3141592654e1 / (-0.27e2 + 0.27e2 * x) + 0.1483129e7 / 0.1296e4 * x * x - 0.25e2 * consts::z3 - 0.349819e6 / 0.324e3 / x - 0.351e3 / 0.2e1 * consts::z3 / (-0.1e1 + x) / x / (0.1e1 + x) - 0.72e2 * x * consts::z3 / (-0.1e1 + x) / (0.1e1 + x) + 0.315e3 / 0.2e1 * x * x * consts::z3 / (-0.1e1 + x) / (0.1e1 + x) + 0.693e3 / 0.2e1 * pow(x, 0.3e1) * consts::z3 / (-0.1e1 + x) / (0.1e1 + x) - 0.315e3 / 0.2e1 * pow(x, 0.4e1) * consts::z3 / (-0.1e1 + x) / (0.1e1 + x) + 0.62e2 * 0.3141592654e1 * 0.3141592654e1 / (-0.1e1 + x) / (0.1e1 + x) - 0.361e3 / 0.8e1 * 0.3141592654e1 * 0.3141592654e1 / (-0.1e1 + x) / x / (0.1e1 + x) - 0.461e3 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 * x / (-0.1e1 + x) / (0.1e1 + x) + 0.581e3 / 0.8e1 * 0.3141592654e1 * 0.3141592654e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) + 0.325e3 / 0.2e1 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) - 0.134e3 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x) - 0.180e3 * consts::z3 / (0.1e1 + x) + 0.235e3 * 0.3141592654e1 * 0.3141592654e1 * x / (-0.72e2 + 0.72e2 * x) - 0.3095e4 * 0.3141592654e1 * 0.3141592654e1 * x * x / (-0.216e3 + 0.216e3 * x) + 0.745e3 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.3e1) / (-0.108e3 + 0.108e3 * x) + 0.216e3 * consts::z3 / (-0.1e1 + x) / (0.1e1 + x) - 0.90e2 * consts::z3 / x / (0.1e1 + x) - 0.270e3 * x * consts::z3 / (0.1e1 + x) - 0.126e3 * x * x * consts::z3 / (0.1e1 + x) - 0.63e2 * pow(x, 0.3e1) * consts::z3 / (0.1e1 + x) + (0.33e2 * 0.3141592654e1 * 0.3141592654e1 / (0.2e1 + 0.2e1 * x) + 0.33e2 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 / x / (0.1e1 + x) + 0.99e2 * 0.3141592654e1 * 0.3141592654e1 * x / (0.4e1 + 0.4e1 * x) + 0.12e2 * 0.3141592654e1 * 0.3141592654e1 * x * x / (0.1e1 + x) + 0.6e1 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.3e1) / (0.1e1 + x)) * log(0.1e1 + x) + (-0.10861e5 / (-0.8e1 + 0.8e1 * x) + 0.2407e4 / 0.4e1 / (-0.1e1 + x) / x + 0.14001e5 * x / (-0.8e1 + 0.8e1 * x) - 0.16769e5 * x * x / (-0.8e1 + 0.8e1 * x) + 0.9359e4 * pow(x, 0.3e1) / (-0.8e1 + 0.8e1 * x) - 0.12e2 * 0.3141592654e1 * 0.3141592654e1 / (-0.1e1 + x) / (0.1e1 + x) - 0.18e2 * 0.3141592654e1 * 0.3141592654e1 / (-0.1e1 + x) / x / (0.1e1 + x) - 0.60e2 * 0.3141592654e1 * 0.3141592654e1 * x / (-0.1e1 + x) / (0.1e1 + x) + 0.12e2 * 0.3141592654e1 * 0.3141592654e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) + 0.123e3 / 0.2e1 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) - 0.30e2 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * log(x) - (0.3181e4 / 0.2e1 - 0.1547e4 / x - 0.5821e4 / 0.4e1 * x + 0.1547e4 * x * x + 0.51e2 * 0.3141592654e1 * 0.3141592654e1 / (0.1e1 + x) + 0.417e3 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 / (-0.1e1 + x) / (0.1e1 + x) + 0.63e2 * 0.3141592654e1 * 0.3141592654e1 / x / (0.1e1 + x) + 0.15e2 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 / (-0.1e1 + x) / x / (0.1e1 + x) + 0.153e3 * 0.3141592654e1 * 0.3141592654e1 * x / (0.1e1 + x) + 0.399e3 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 * x / (-0.1e1 + x) / (0.1e1 + x) + 0.108e3 * 0.3141592654e1 * 0.3141592654e1 * x * x / (0.1e1 + x) - 0.411e3 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) - 0.51e2 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.3e1) / (0.1e1 + x) - 0.393e3 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) - 0.33e2 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * log(0.1e1 - x) - 0.5e1 * (-0.82e2 / 0.9e1 - 0.3141592654e1 * 0.3141592654e1 / 0.36e2 + 0.560e3 / 0.27e2 / x + 0.3141592654e1 * 0.3141592654e1 / x / 0.36e2 + 0.145e3 / 0.36e2 * x + 0.3141592654e1 * 0.3141592654e1 * x / 0.72e2 - 0.560e3 / 0.27e2 * x * x) * log(0.1e1 - x) + 0.5e1 * (-0.3141592654e1 * 0.3141592654e1 + 0.781e3 / (-0.108e3 + 0.108e3 * x) - 0.280e3 / 0.27e2 / (-0.1e1 + x) / x - 0.3141592654e1 * 0.3141592654e1 * x - 0.379e3 * x / (-0.72e2 + 0.72e2 * x) + 0.4549e4 * x * x / (-0.216e3 + 0.216e3 * x) - 0.1655e4 * pow(x, 0.3e1) / (-0.108e3 + 0.108e3 * x)) * log(x) + (-0.57e2 / (0.1e1 + x) - 0.30e2 / x / (0.1e1 + x) - 0.27e2 * x / (0.2e1 + 0.2e1 * x) + 0.30e2 * x * x / (0.1e1 + x) + 0.15e2 * pow(x, 0.3e1) / (0.2e1 + 0.2e1 * x)) * (log(x) * log(0.1e1 + x) + polylog(2, -x)) + (-0.987e3 / 0.4e1 / (-0.1e1 + x) / (0.1e1 + x) + 0.267e3 / (-0.1e1 + x) / x / (0.1e1 + x) + 0.492e3 * x / (-0.1e1 + x) / (0.1e1 + x) - 0.447e3 / 0.2e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) - 0.2793e4 / 0.4e1 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) + 0.540e3 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * pow(log(x), 0.2e1) / 0.2e1 + (-0.3945e4 / (-0.2e1 + 0.2e1 * x) + 0.3843e4 / 0.4e1 / (-0.1e1 + x) / x + 0.8793e4 * x / (-0.4e1 + 0.4e1 * x) - 0.1929e4 * x * x / (-0.1e1 + x) + 0.3735e4 * pow(x, 0.3e1) / (-0.4e1 + 0.4e1 * x) - 0.57e2 / (0.1e1 + x) + 0.1251e4 / 0.4e1 / (-0.1e1 + x) / (0.1e1 + x) - 0.30e2 / x / (0.1e1 + x) - 0.1383e4 / 0.4e1 / (-0.1e1 + x) / x / (0.1e1 + x) - 0.27e2 * x / (0.2e1 + 0.2e1 * x) + 0.2859e4 / 0.4e1 * x / (-0.1e1 + x) / (0.1e1 + x) + 0.30e2 * x * x / (0.1e1 + x) - 0.2043e4 / 0.4e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) + 0.15e2 * pow(x, 0.3e1) / (0.2e1 + 0.2e1 * x) - 0.1545e4 / 0.4e1 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) + 0.795e3 / 0.4e1 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * polylog(2, x) + (-0.3945e4 / (-0.2e1 + 0.2e1 * x) + 0.3843e4 / 0.4e1 / (-0.1e1 + x) / x + 0.8793e4 * x / (-0.4e1 + 0.4e1 * x) - 0.1929e4 * x * x / (-0.1e1 + x) + 0.3735e4 * pow(x, 0.3e1) / (-0.4e1 + 0.4e1 * x)) * (-log(0.1e1 - x) * log(x) - polylog(2, x)) + (0.2154e4 - 0.1602e4 / x - 0.1806e4 * x + 0.1602e4 * x * x) * pow(log(0.1e1 - x), 0.2e1) / 0.2e1 + 0.5e1 / 0.2e1 * (-0.11e2 / (-0.9e1 + 0.9e1 * x) - 0.34e2 / 0.9e1 / (-0.1e1 + x) / x - 0.17e2 * x / (-0.4e1 + 0.4e1 * x) + 0.511e3 * x * x / (-0.36e2 + 0.36e2 * x) - 0.58e2 * pow(x, 0.3e1) / (-0.9e1 + 0.9e1 * x)) * pow(log(x), 0.2e1) + 0.5e1 * (0.2e1 / (-0.9e1 + 0.9e1 * x) - 0.68e2 / 0.9e1 / (-0.1e1 + x) / x - 0.20e2 * x / (-0.3e1 + 0.3e1 * x) + 0.190e3 * x * x / (-0.9e1 + 0.9e1 * x) - 0.100e3 * pow(x, 0.3e1) / (-0.9e1 + 0.9e1 * x)) * polylog(2, x) + 0.5e1 * (0.203e3 / (-0.18e2 + 0.18e2 * x) - 0.85e2 / 0.9e1 / (-0.1e1 + x) / x - 0.103e3 * x / (-0.12e2 + 0.12e2 * x) + 0.421e3 * x * x / (-0.36e2 + 0.36e2 * x) - 0.85e2 * pow(x, 0.3e1) / (-0.9e1 + 0.9e1 * x)) * (-log(0.1e1 - x) * log(x) - polylog(2, x)) + 0.5e1 / 0.2e1 * (-0.32e2 / 0.3e1 + 0.136e3 / 0.9e1 / x + 0.8e1 / 0.3e1 * x - 0.136e3 / 0.9e1 * x * x) * pow(log(0.1e1 - x), 0.2e1) + (0.54e2 / (0.1e1 + x) + 0.27e2 / x / (0.1e1 + x) + 0.81e2 * x / (0.1e1 + x)) * (-0.3141592654e1 * 0.3141592654e1 * log(0.1e1 + x) / 0.6e1 + pow(log(0.1e1 + x), 0.3e1) / 0.6e1 - polylog(3, 0.1e1 / (0.1e1 + x)) + consts::z3) + (-0.81e2 / (0.1e1 + x) - 0.81e2 / 0.2e1 / x / (0.1e1 + x) - 0.243e3 * x / (0.2e1 + 0.2e1 * x) - 0.36e2 * x * x / (0.1e1 + x) - 0.18e2 * pow(x, 0.3e1) / (0.1e1 + x)) * (pow(log(x), 0.2e1) * log(0.1e1 + x) / 0.2e1 + log(x) * polylog(2, -x) - polylog(3, -x)) + (-0.72e2 / (0.1e1 + x) - 0.36e2 / x / (0.1e1 + x) - 0.108e3 * x / (0.1e1 + x) - 0.72e2 * x * x / (0.1e1 + x) - 0.36e2 * pow(x, 0.3e1) / (0.1e1 + x)) * (0.3141592654e1 * 0.3141592654e1 * log(0.2e1) / 0.6e1 - pow(log(0.2e1), 0.3e1) / 0.3e1 - 0.3141592654e1 * 0.3141592654e1 * log(0.1e1 - x) / 0.12e2 + pow(log(0.2e1), 0.2e1) * log(0.1e1 - x) / 0.2e1 - 0.3141592654e1 * 0.3141592654e1 * log(0.1e1 + x) / 0.12e2 + pow(log(0.2e1), 0.2e1) * log(0.1e1 + x) / 0.2e1 - log(0.2e1) * log(0.1e1 - x) * log(0.1e1 + x) - log(0.1e1 - x) * log(x) * log(0.1e1 + x) + log(0.1e1 - x) * pow(log(0.1e1 + x), 0.2e1) / 0.2e1 - log(0.1e1 - x) * polylog(2, -x) + polylog(3, 0.1e1 / 0.2e1 - x / 0.2e1) - polylog(3, 0.1e1 - x) - polylog(3, 0.1e1 / (0.1e1 + x)) + polylog(3, (0.1e1 - x) / (0.1e1 + x)) + polylog(3, 0.1e1 / 0.2e1 + x / 0.2e1) - 0.3e1 / 0.4e1 * consts::z3) + (-0.36e2 / (0.1e1 + x) - 0.18e2 / x / (0.1e1 + x) - 0.54e2 * x / (0.1e1 + x) - 0.36e2 * x * x / (0.1e1 + x) - 0.27e2 * pow(x, 0.3e1) / (0.1e1 + x)) * (-log(x) * polylog(2, -x) + 0.2e1 * polylog(3, -x)) + (0.99e2 / (-0.1e1 + x) / (0.1e1 + x) + 0.72e2 / (-0.1e1 + x) / x / (0.1e1 + x) + 0.270e3 * x / (-0.1e1 + x) / (0.1e1 + x) - 0.99e2 * x * x / (-0.1e1 + x) / (0.1e1 + x) - 0.261e3 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) + 0.117e3 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * pow(log(x), 0.3e1) / 0.6e1 + (-0.432e3 / (-0.1e1 + x) + 0.216e3 / (-0.1e1 + x) / x + 0.648e3 * x / (-0.1e1 + x) - 0.414e3 * x * x / (-0.1e1 + x) + 0.216e3 * pow(x, 0.3e1) / (-0.1e1 + x) - 0.81e2 / (0.1e1 + x) + 0.531e3 / 0.2e1 / (-0.1e1 + x) / (0.1e1 + x) - 0.81e2 / 0.2e1 / x / (0.1e1 + x) - 0.45e2 / 0.2e1 / (-0.1e1 + x) / x / (0.1e1 + x) - 0.243e3 * x / (0.2e1 + 0.2e1 * x) + 0.675e3 / 0.2e1 * x / (-0.1e1 + x) / (0.1e1 + x) - 0.36e2 * x * x / (0.1e1 + x) - 0.477e3 / 0.2e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) - 0.18e2 * pow(x, 0.3e1) / (0.1e1 + x) - 0.378e3 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) + 0.72e2 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * polylog(3, x) + (-0.432e3 / (-0.1e1 + x) + 0.216e3 / (-0.1e1 + x) / x + 0.648e3 * x / (-0.1e1 + x) - 0.414e3 * x * x / (-0.1e1 + x) + 0.216e3 * pow(x, 0.3e1) / (-0.1e1 + x) - 0.45e2 / (0.1e1 + x) + 0.495e3 / 0.2e1 / (-0.1e1 + x) / (0.1e1 + x) - 0.45e2 / 0.2e1 / x / (0.1e1 + x) + 0.171e3 / 0.2e1 / (-0.1e1 + x) / x / (0.1e1 + x) - 0.135e3 * x / (0.2e1 + 0.2e1 * x) + 0.891e3 / 0.2e1 * x / (-0.1e1 + x) / (0.1e1 + x) - 0.441e3 / 0.2e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) + 0.9e1 * pow(x, 0.3e1) / (0.1e1 + x) - 0.945e3 / 0.2e1 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) + 0.171e3 / 0.2e1 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * (log(x) * polylog(2, x) - 0.2e1 * polylog(3, x)) + (-0.1710e4 / (-0.1e1 + x) + 0.567e3 / (-0.1e1 + x) / x + 0.1701e4 * x / (-0.1e1 + x) - 0.558e3 * x * x / (-0.1e1 + x) + 0.567e3 * pow(x, 0.3e1) / (-0.1e1 + x) - 0.1152e4 / (0.1e1 + x) - 0.2304e4 * x / (0.1e1 + x) - 0.1152e4 * x * x / (0.1e1 + x)) * (0.3141592654e1 * 0.3141592654e1 * log(0.1e1 - x) / 0.6e1 - pow(log(0.1e1 - x), 0.2e1) * log(x) / 0.2e1 - log(0.1e1 - x) * polylog(2, x) - polylog(3, 0.1e1 - x) + consts::z3) + (-0.432e3 / (-0.1e1 + x) + 0.216e3 / (-0.1e1 + x) / x + 0.648e3 * x / (-0.1e1 + x) - 0.414e3 * x * x / (-0.1e1 + x) + 0.216e3 * pow(x, 0.3e1) / (-0.1e1 + x)) * (-log(0.1e1 - x) * pow(log(x), 0.2e1) / 0.2e1 - log(x) * polylog(2, x) + polylog(3, x)) + (-0.1710e4 / (-0.1e1 + x) + 0.567e3 / (-0.1e1 + x) / x + 0.1701e4 * x / (-0.1e1 + x) - 0.558e3 * x * x / (-0.1e1 + x) + 0.567e3 * pow(x, 0.3e1) / (-0.1e1 + x) - 0.576e3 / (0.1e1 + x) - 0.1152e4 * x / (0.1e1 + x) - 0.576e3 * x * x / (0.1e1 + x)) * (-0.3141592654e1 * 0.3141592654e1 * log(0.1e1 - x) / 0.3e1 + pow(log(0.1e1 - x), 0.2e1) * log(x) + log(0.1e1 - x) * polylog(2, x) + 0.2e1 * polylog(3, 0.1e1 - x) - 0.2e1 * consts::z3) + (-0.1710e4 / (-0.1e1 + x) + 0.567e3 / (-0.1e1 + x) / x + 0.1701e4 * x / (-0.1e1 + x) - 0.558e3 * x * x / (-0.1e1 + x) + 0.567e3 * pow(x, 0.3e1) / (-0.1e1 + x) + 0.72e2 / (0.1e1 + x) + 0.1251e4 / 0.2e1 / (-0.1e1 + x) / (0.1e1 + x) + 0.36e2 / x / (0.1e1 + x) + 0.45e2 / 0.2e1 / (-0.1e1 + x) / x / (0.1e1 + x) + 0.108e3 * x / (0.1e1 + x) + 0.1197e4 / 0.2e1 * x / (-0.1e1 + x) / (0.1e1 + x) + 0.72e2 * x * x / (0.1e1 + x) - 0.1233e4 / 0.2e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) + 0.36e2 * pow(x, 0.3e1) / (0.1e1 + x) - 0.1179e4 / 0.2e1 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) - 0.99e2 / 0.2e1 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * (0.3141592654e1 * 0.3141592654e1 * log(0.1e1 - x) / 0.6e1 - polylog(3, 0.1e1 - x) + consts::z3) - (0.2016e4 - 0.1008e4 / x - 0.1008e4 * x + 0.1008e4 * x * x) * pow(log(0.1e1 - x), 0.3e1) / 0.6e1 + 0.5e1 / 0.6e1 * (0.14e2 / 0.3e1 + 0.14e2 / 0.3e1 * x) * pow(log(x), 0.3e1) + 0.5e1 * (0.8e1 + 0.8e1 * x) * polylog(3, x) + 0.5e1 * (0.20e2 / 0.3e1 + 0.20e2 / 0.3e1 * x) * (log(x) * polylog(2, x) - 0.2e1 * polylog(3, x)) + 0.5e1 * (0.32e2 / 0.3e1 + 0.32e2 / 0.3e1 * x) * (0.3141592654e1 * 0.3141592654e1 * log(0.1e1 - x) / 0.6e1 - pow(log(0.1e1 - x), 0.2e1) * log(x) / 0.2e1 - log(0.1e1 - x) * polylog(2, x) - polylog(3, 0.1e1 - x) + consts::z3) + 0.5e1 * (-0.1e1 / 0.6e1 + 0.1e1 / x / 0.6e1 + x / 0.12e2) * (-log(0.1e1 - x) * pow(log(x), 0.2e1) / 0.2e1 - log(x) * polylog(2, x) + polylog(3, x)) + 0.5e1 * (-0.1e1 / 0.6e1 + 0.1e1 / x / 0.6e1 + x / 0.12e2) * (0.3141592654e1 * 0.3141592654e1 * log(0.1e1 - x) / 0.6e1 - polylog(3, 0.1e1 - x) + consts::z3) + 0.397763e6 / 0.432e3;
     return real(res);
}

double HPL(int i, double x){return real(HPL1(i,complex<double>(x,0.0)));}


double HPL(int i,int j, double x){return HPL2real(i,j,x,0.0);}
double HPL(int i,int j,int k, double x){return HPL3real(i,j,k,x,0.0);}


//: alternative implementation with HPLs
double gg_NNLO_reg2(double x)
{
return -0.5e1 / 0.3e1 * consts::z3 / x - 0.55e2 / 0.2e1 * x * consts::z3 + 0.425e3 / 0.108e3 * 0.3141592654e1 * 0.3141592654e1 / (-0.1e1 + x) / x - 0.207193e6 / 0.216e3 * x - 0.5e1 * 0.3141592654e1 * 0.3141592654e1 / (-0.27e2 + 0.27e2 * x) + 0.1483129e7 / 0.1296e4 * x * x - 0.25e2 * consts::z3 - 0.349819e6 / 0.324e3 / x - 0.351e3 / 0.2e1 * consts::z3 / (-0.1e1 + x) / x / (0.1e1 + x) - 0.72e2 * x * consts::z3 / (-0.1e1 + x) / (0.1e1 + x) + 0.315e3 / 0.2e1 * x * x * consts::z3 / (-0.1e1 + x) / (0.1e1 + x) + 0.693e3 / 0.2e1 * pow(x, 0.3e1) * consts::z3 / (-0.1e1 + x) / (0.1e1 + x) - 0.315e3 / 0.2e1 * pow(x, 0.4e1) * consts::z3 / (-0.1e1 + x) / (0.1e1 + x) + 0.62e2 * 0.3141592654e1 * 0.3141592654e1 / (-0.1e1 + x) / (0.1e1 + x) - 0.361e3 / 0.8e1 * 0.3141592654e1 * 0.3141592654e1 / (-0.1e1 + x) / x / (0.1e1 + x) - 0.461e3 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 * x / (-0.1e1 + x) / (0.1e1 + x) + 0.581e3 / 0.8e1 * 0.3141592654e1 * 0.3141592654e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) + 0.325e3 / 0.2e1 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) - 0.134e3 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x) - 0.180e3 * consts::z3 / (0.1e1 + x) + 0.235e3 * 0.3141592654e1 * 0.3141592654e1 * x / (-0.72e2 + 0.72e2 * x) - 0.3095e4 * 0.3141592654e1 * 0.3141592654e1 * x * x / (-0.216e3 + 0.216e3 * x) + 0.745e3 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.3e1) / (-0.108e3 + 0.108e3 * x) + 0.216e3 * consts::z3 / (-0.1e1 + x) / (0.1e1 + x) - 0.90e2 * consts::z3 / x / (0.1e1 + x) - 0.270e3 * x * consts::z3 / (0.1e1 + x) - 0.126e3 * x * x * consts::z3 / (0.1e1 + x) - 0.63e2 * pow(x, 0.3e1) * consts::z3 / (0.1e1 + x) + 0.397763e6 / 0.432e3 + (-0.432e3 / (-0.1e1 + x) + 0.216e3 / (-0.1e1 + x) / x + 0.648e3 * x / (-0.1e1 + x) - 0.414e3 * x * x / (-0.1e1 + x) + 0.216e3 * pow(x, 0.3e1) / (-0.1e1 + x) - 0.45e2 / (0.1e1 + x) + 0.495e3 / 0.2e1 / (-0.1e1 + x) / (0.1e1 + x) - 0.45e2 / 0.2e1 / x / (0.1e1 + x) + 0.171e3 / 0.2e1 / (-0.1e1 + x) / x / (0.1e1 + x) - 0.135e3 * x / (0.2e1 + 0.2e1 * x) + 0.891e3 / 0.2e1 * x / (-0.1e1 + x) / (0.1e1 + x) - 0.441e3 / 0.2e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) + 0.9e1 * pow(x, 0.3e1) / (0.1e1 + x) - 0.945e3 / 0.2e1 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) + 0.171e3 / 0.2e1 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * HPL(0, 1, 0, x) + (-0.1710e4 / (-0.1e1 + x) + 0.567e3 / (-0.1e1 + x) / x + 0.1701e4 * x / (-0.1e1 + x) - 0.558e3 * x * x / (-0.1e1 + x) + 0.567e3 * pow(x, 0.3e1) / (-0.1e1 + x) - 0.1152e4 / (0.1e1 + x) - 0.2304e4 * x / (0.1e1 + x) - 0.1152e4 * x * x / (0.1e1 + x)) *   + (-0.432e3 / (-0.1e1 + x) + 0.216e3 / (-0.1e1 + x) / x + 0.648e3 * x / (-0.1e1 + x) - 0.414e3 * x * x / (-0.1e1 + x) + 0.216e3 * pow(x, 0.3e1) / (-0.1e1 + x)) * HPL(1, 0, 0, x) + (-0.1710e4 / (-0.1e1 + x) + 0.567e3 / (-0.1e1 + x) / x + 0.1701e4 * x / (-0.1e1 + x) - 0.558e3 * x * x / (-0.1e1 + x) + 0.567e3 * pow(x, 0.3e1) / (-0.1e1 + x) - 0.576e3 / (0.1e1 + x) - 0.1152e4 * x / (0.1e1 + x) - 0.576e3 * x * x / (0.1e1 + x)) * HPL(1, 0, 1, x) + (-0.1710e4 / (-0.1e1 + x) + 0.567e3 / (-0.1e1 + x) / x + 0.1701e4 * x / (-0.1e1 + x) - 0.558e3 * x * x / (-0.1e1 + x) + 0.567e3 * pow(x, 0.3e1) / (-0.1e1 + x) + 0.72e2 / (0.1e1 + x) + 0.1251e4 / 0.2e1 / (-0.1e1 + x) / (0.1e1 + x) + 0.36e2 / x / (0.1e1 + x) + 0.45e2 / 0.2e1 / (-0.1e1 + x) / x / (0.1e1 + x) + 0.108e3 * x / (0.1e1 + x) + 0.1197e4 / 0.2e1 * x / (-0.1e1 + x) / (0.1e1 + x) + 0.72e2 * x * x / (0.1e1 + x) - 0.1233e4 / 0.2e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) + 0.36e2 * pow(x, 0.3e1) / (0.1e1 + x) - 0.1179e4 / 0.2e1 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) - 0.99e2 / 0.2e1 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * HPL(1, 1, 0, x) + (0.2016e4 - 0.1008e4 / x - 0.1008e4 * x + 0.1008e4 * x * x) * HPL(1, 1, 1, x) + 0.5e1 * (-0.82e2 / 0.9e1 - 0.3141592654e1 * 0.3141592654e1 / 0.36e2 + 0.560e3 / 0.27e2 / x + 0.3141592654e1 * 0.3141592654e1 / x / 0.36e2 + 0.145e3 / 0.36e2 * x + 0.3141592654e1 * 0.3141592654e1 * x / 0.72e2 - 0.560e3 / 0.27e2 * x * x) * HPL(1, x) + 0.5e1 * (-0.11e2 / (-0.9e1 + 0.9e1 * x) - 0.34e2 / 0.9e1 / (-0.1e1 + x) / x - 0.17e2 * x / (-0.4e1 + 0.4e1 * x) + 0.511e3 * x * x / (-0.36e2 + 0.36e2 * x) - 0.58e2 * pow(x, 0.3e1) / (-0.9e1 + 0.9e1 * x)) * HPL(0, 0, x) + 0.5e1 * (0.2e1 / (-0.9e1 + 0.9e1 * x) - 0.68e2 / 0.9e1 / (-0.1e1 + x) / x - 0.20e2 * x / (-0.3e1 + 0.3e1 * x) + 0.190e3 * x * x / (-0.9e1 + 0.9e1 * x) - 0.100e3 * pow(x, 0.3e1) / (-0.9e1 + 0.9e1 * x)) * HPL(0, 1, x) + 0.5e1 * (-0.3141592654e1 * 0.3141592654e1 + 0.781e3 / (-0.108e3 + 0.108e3 * x) - 0.280e3 / 0.27e2 / (-0.1e1 + x) / x - 0.3141592654e1 * 0.3141592654e1 * x - 0.379e3 * x / (-0.72e2 + 0.72e2 * x) + 0.4549e4 * x * x / (-0.216e3 + 0.216e3 * x) - 0.1655e4 * pow(x, 0.3e1) / (-0.108e3 + 0.108e3 * x)) * HPL(0, x) + 0.5e1 * (0.14e2 / 0.3e1 + 0.14e2 / 0.3e1 * x) * HPL(0, 0, 0, x) + 0.5e1 * (0.8e1 + 0.8e1 * x) * HPL(0, 0, 1, x) + 0.5e1 * (0.20e2 / 0.3e1 + 0.20e2 / 0.3e1 * x) * HPL(0, 1, 0, x) + 0.5e1 * (0.32e2 / 0.3e1 + 0.32e2 / 0.3e1 * x) * HPL(0, 1, 1, x) + 0.5e1 * (-0.1e1 / 0.6e1 + 0.1e1 / x / 0.6e1 + x / 0.12e2) * HPL(1, 0, 0, x) + 0.5e1 * (-0.1e1 / 0.6e1 + 0.1e1 / x / 0.6e1 + x / 0.12e2) * HPL(1, 1, 0, x) + 0.5e1 * (0.203e3 / (-0.18e2 + 0.18e2 * x) - 0.85e2 / 0.9e1 / (-0.1e1 + x) / x - 0.103e3 * x / (-0.12e2 + 0.12e2 * x) + 0.421e3 * x * x / (-0.36e2 + 0.36e2 * x) - 0.85e2 * pow(x, 0.3e1) / (-0.9e1 + 0.9e1 * x)) * HPL(1, 0, x) + 0.5e1 * (-0.32e2 / 0.3e1 + 0.136e3 / 0.9e1 / x + 0.8e1 / 0.3e1 * x - 0.136e3 / 0.9e1 * x * x) * HPL(1, 1, x) + (0.33e2 * 0.3141592654e1 * 0.3141592654e1 / (0.2e1 + 0.2e1 * x) + 0.33e2 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 / x / (0.1e1 + x) + 0.99e2 * 0.3141592654e1 * 0.3141592654e1 * x / (0.4e1 + 0.4e1 * x) + 0.12e2 * 0.3141592654e1 * 0.3141592654e1 * x * x / (0.1e1 + x) + 0.6e1 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.3e1) / (0.1e1 + x)) * HPL(-1, x) + (-0.10861e5 / (-0.8e1 + 0.8e1 * x) + 0.2407e4 / 0.4e1 / (-0.1e1 + x) / x + 0.14001e5 * x / (-0.8e1 + 0.8e1 * x) - 0.16769e5 * x * x / (-0.8e1 + 0.8e1 * x) + 0.9359e4 * pow(x, 0.3e1) / (-0.8e1 + 0.8e1 * x) - 0.12e2 * 0.3141592654e1 * 0.3141592654e1 / (-0.1e1 + x) / (0.1e1 + x) - 0.18e2 * 0.3141592654e1 * 0.3141592654e1 / (-0.1e1 + x) / x / (0.1e1 + x) - 0.60e2 * 0.3141592654e1 * 0.3141592654e1 * x / (-0.1e1 + x) / (0.1e1 + x) + 0.12e2 * 0.3141592654e1 * 0.3141592654e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) + 0.123e3 / 0.2e1 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) - 0.30e2 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * HPL(0, x) + (0.3181e4 / 0.2e1 - 0.1547e4 / x - 0.5821e4 / 0.4e1 * x + 0.1547e4 * x * x + 0.51e2 * 0.3141592654e1 * 0.3141592654e1 / (0.1e1 + x) + 0.417e3 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 / (-0.1e1 + x) / (0.1e1 + x) + 0.63e2 * 0.3141592654e1 * 0.3141592654e1 / x / (0.1e1 + x) + 0.15e2 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 / (-0.1e1 + x) / x / (0.1e1 + x) + 0.153e3 * 0.3141592654e1 * 0.3141592654e1 * x / (0.1e1 + x) + 0.399e3 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 * x / (-0.1e1 + x) / (0.1e1 + x) + 0.108e3 * 0.3141592654e1 * 0.3141592654e1 * x * x / (0.1e1 + x) - 0.411e3 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) - 0.51e2 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.3e1) / (0.1e1 + x) - 0.393e3 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) - 0.33e2 / 0.4e1 * 0.3141592654e1 * 0.3141592654e1 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * HPL(1, x) + (-0.57e2 / (0.1e1 + x) - 0.30e2 / x / (0.1e1 + x) - 0.27e2 * x / (0.2e1 + 0.2e1 * x) + 0.30e2 * x * x / (0.1e1 + x) + 0.15e2 * pow(x, 0.3e1) / (0.2e1 + 0.2e1 * x)) * HPL(-1, 0, x) + (-0.987e3 / 0.4e1 / (-0.1e1 + x) / (0.1e1 + x) + 0.267e3 / (-0.1e1 + x) / x / (0.1e1 + x) + 0.492e3 * x / (-0.1e1 + x) / (0.1e1 + x) - 0.447e3 / 0.2e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) - 0.2793e4 / 0.4e1 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) + 0.540e3 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * HPL(0, 0, x) + (-0.3945e4 / (-0.2e1 + 0.2e1 * x) + 0.3843e4 / 0.4e1 / (-0.1e1 + x) / x + 0.8793e4 * x / (-0.4e1 + 0.4e1 * x) - 0.1929e4 * x * x / (-0.1e1 + x) + 0.3735e4 * pow(x, 0.3e1) / (-0.4e1 + 0.4e1 * x) - 0.57e2 / (0.1e1 + x) + 0.1251e4 / 0.4e1 / (-0.1e1 + x) / (0.1e1 + x) - 0.30e2 / x / (0.1e1 + x) - 0.1383e4 / 0.4e1 / (-0.1e1 + x) / x / (0.1e1 + x) - 0.27e2 * x / (0.2e1 + 0.2e1 * x) + 0.2859e4 / 0.4e1 * x / (-0.1e1 + x) / (0.1e1 + x) + 0.30e2 * x * x / (0.1e1 + x) - 0.2043e4 / 0.4e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) + 0.15e2 * pow(x, 0.3e1) / (0.2e1 + 0.2e1 * x) - 0.1545e4 / 0.4e1 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) + 0.795e3 / 0.4e1 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * HPL(0, 1, x) + (-0.3945e4 / (-0.2e1 + 0.2e1 * x) + 0.3843e4 / 0.4e1 / (-0.1e1 + x) / x + 0.8793e4 * x / (-0.4e1 + 0.4e1 * x) - 0.1929e4 * x * x / (-0.1e1 + x) + 0.3735e4 * pow(x, 0.3e1) / (-0.4e1 + 0.4e1 * x)) * HPL(1, 0, x) + (0.2154e4 - 0.1602e4 / x - 0.1806e4 * x + 0.1602e4 * x * x) * HPL(1, 1, x) + (0.54e2 / (0.1e1 + x) + 0.27e2 / x / (0.1e1 + x) + 0.81e2 * x / (0.1e1 + x)) * HPL(-1, -1, 0, x) + (-0.81e2 / (0.1e1 + x) - 0.81e2 / 0.2e1 / x / (0.1e1 + x) - 0.243e3 * x / (0.2e1 + 0.2e1 * x) - 0.36e2 * x * x / (0.1e1 + x) - 0.18e2 * pow(x, 0.3e1) / (0.1e1 + x)) * HPL(-1, 0, 0, x) + (-0.72e2 / (0.1e1 + x) - 0.36e2 / x / (0.1e1 + x) - 0.108e3 * x / (0.1e1 + x) - 0.72e2 * x * x / (0.1e1 + x) - 0.36e2 * pow(x, 0.3e1) / (0.1e1 + x)) * HPL(-1, 0, 1, x) + (-0.36e2 / (0.1e1 + x) - 0.18e2 / x / (0.1e1 + x) - 0.54e2 * x / (0.1e1 + x) - 0.36e2 * x * x / (0.1e1 + x) - 0.27e2 * pow(x, 0.3e1) / (0.1e1 + x)) * HPL(0, -1, 0, x) + (0.99e2 / (-0.1e1 + x) / (0.1e1 + x) + 0.72e2 / (-0.1e1 + x) / x / (0.1e1 + x) + 0.270e3 * x / (-0.1e1 + x) / (0.1e1 + x) - 0.99e2 * x * x / (-0.1e1 + x) / (0.1e1 + x) - 0.261e3 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) + 0.117e3 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * HPL(0, 0, 0, x) + (-0.432e3 / (-0.1e1 + x) + 0.216e3 / (-0.1e1 + x) / x + 0.648e3 * x / (-0.1e1 + x) - 0.414e3 * x * x / (-0.1e1 + x) + 0.216e3 * pow(x, 0.3e1) / (-0.1e1 + x) - 0.81e2 / (0.1e1 + x) + 0.531e3 / 0.2e1 / (-0.1e1 + x) / (0.1e1 + x) - 0.81e2 / 0.2e1 / x / (0.1e1 + x) - 0.45e2 / 0.2e1 / (-0.1e1 + x) / x / (0.1e1 + x) - 0.243e3 * x / (0.2e1 + 0.2e1 * x) + 0.675e3 / 0.2e1 * x / (-0.1e1 + x) / (0.1e1 + x) - 0.36e2 * x * x / (0.1e1 + x) - 0.477e3 / 0.2e1 * x * x / (-0.1e1 + x) / (0.1e1 + x) - 0.18e2 * pow(x, 0.3e1) / (0.1e1 + x) - 0.378e3 * pow(x, 0.3e1) / (-0.1e1 + x) / (0.1e1 + x) + 0.72e2 * pow(x, 0.4e1) / (-0.1e1 + x) / (0.1e1 + x)) * HPL(0, 0, 1, x);
}



