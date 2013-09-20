#include <string>
#include <iostream>
#include <vector>
#include <math.h>
#include <stdlib.h>

using namespace std;

#include "luminosity.h"
#include<algorithm>
// Declaration of the pdf names
Luminosity::pdf_desc Luminosity::F_d_00(1,1,0,0);
Luminosity::pdf_desc Luminosity::F_u_00(2,2,0,0);
Luminosity::pdf_desc Luminosity::F_s_00(3,3,0,0);
Luminosity::pdf_desc Luminosity::F_c_00(4,4,0,0);

Luminosity::pdf_desc Luminosity::F_dbar_00(-1,-1,0,0);
Luminosity::pdf_desc Luminosity::F_ubar_00(-2,-2,0,0);
Luminosity::pdf_desc Luminosity::F_sbar_00(-3,-3,0,0);
Luminosity::pdf_desc Luminosity::F_cbar_00(-4,-4,0,0);

Luminosity::pdf_desc Luminosity::F_b_00(5,5,0,0);
Luminosity::pdf_desc Luminosity::F_bbar_00(-5,-5,0,0);
Luminosity::pdf_desc Luminosity::F_g_00(0,0,0,0);

Luminosity::pdf_desc Luminosity::F_b_from_g_11(5,0,1,1);
Luminosity::pdf_desc Luminosity::F_bbar_from_g_11(-5,0,1,1);

Luminosity::pdf_desc Luminosity::F_g_from_g_11(0,0,1,1);

Luminosity::pdf_desc Luminosity::F_g_from_b_11(0,5,1,1);
Luminosity::pdf_desc Luminosity::F_g_from_bbar_11(0,-5,1,1);

Luminosity::pdf_desc Luminosity::F_g_from_d_11(0,1,1,1);
Luminosity::pdf_desc Luminosity::F_g_from_u_11(0,2,1,1);
Luminosity::pdf_desc Luminosity::F_g_from_s_11(0,3,1,1);
Luminosity::pdf_desc Luminosity::F_g_from_c_11(0,4,1,1);

Luminosity::pdf_desc Luminosity::F_g_from_dbar_11(0,-1,1,1);
Luminosity::pdf_desc Luminosity::F_g_from_ubar_11(0,-2,1,1);
Luminosity::pdf_desc Luminosity::F_g_from_sbar_11(0,-3,1,1);
Luminosity::pdf_desc Luminosity::F_g_from_cbar_11(0,-4,1,1);

Luminosity::pdf_desc Luminosity::F_b_from_b_11(5,5,1,1);
Luminosity::pdf_desc Luminosity::F_bbar_from_bbar_11(-5,-5,1,1);

Luminosity::pdf_desc Luminosity::F_b_from_b_21(5,5,2,1);
Luminosity::pdf_desc Luminosity::F_bbar_from_bbar_21(-5,-5,2,1);

Luminosity::pdf_desc Luminosity::F_b_from_b_22(5,5,2,2);
Luminosity::pdf_desc Luminosity::F_bbar_from_bbar_22(-5,-5,2,2);

Luminosity::pdf_desc Luminosity::F_b_from_bbar_22(5,-5,2,2);
Luminosity::pdf_desc Luminosity::F_bbar_from_b_22(-5,5,2,2);

Luminosity::pdf_desc Luminosity::F_b_from_bbar_21(5,-5,2,1);
Luminosity::pdf_desc Luminosity::F_bbar_from_b_21(-5,5,2,1);

Luminosity::pdf_desc Luminosity::F_b_from_g_22(5,0,2,2);
Luminosity::pdf_desc Luminosity::F_bbar_from_g_22(-5,0,2,2);

Luminosity::pdf_desc Luminosity::F_b_from_g_21(5,0,2,1);
Luminosity::pdf_desc Luminosity::F_bbar_from_g_21(-5,0,2,1);



Luminosity::pdf_desc Luminosity::F_b_from_d_21(5,1,2,1);
Luminosity::pdf_desc Luminosity::F_b_from_u_21(5,2,2,1);
Luminosity::pdf_desc Luminosity::F_b_from_s_21(5,3,2,1);
Luminosity::pdf_desc Luminosity::F_b_from_c_21(5,4,2,1);

Luminosity::pdf_desc Luminosity::F_b_from_dbar_21(5,-1,2,1);
Luminosity::pdf_desc Luminosity::F_b_from_ubar_21(5,-2,2,1);
Luminosity::pdf_desc Luminosity::F_b_from_sbar_21(5,-3,2,1);
Luminosity::pdf_desc Luminosity::F_b_from_cbar_21(5,-4,2,1);

Luminosity::pdf_desc Luminosity::F_bbar_from_d_21(-5,1,2,1);
Luminosity::pdf_desc Luminosity::F_bbar_from_u_21(-5,2,2,1);
Luminosity::pdf_desc Luminosity::F_bbar_from_s_21(-5,3,2,1);
Luminosity::pdf_desc Luminosity::F_bbar_from_c_21(-5,4,2,1);

Luminosity::pdf_desc Luminosity::F_bbar_from_dbar_21(-5,-1,2,1);
Luminosity::pdf_desc Luminosity::F_bbar_from_ubar_21(-5,-2,2,1);
Luminosity::pdf_desc Luminosity::F_bbar_from_sbar_21(-5,-3,2,1);
Luminosity::pdf_desc Luminosity::F_bbar_from_cbar_21(-5,-4,2,1);

Luminosity::pdf_desc Luminosity::F_b_from_d_22(5,1,2,2);
Luminosity::pdf_desc Luminosity::F_b_from_u_22(5,2,2,2);
Luminosity::pdf_desc Luminosity::F_b_from_s_22(5,3,2,2);
Luminosity::pdf_desc Luminosity::F_b_from_c_22(5,4,2,2);

Luminosity::pdf_desc Luminosity::F_b_from_dbar_22(5,-1,2,2);
Luminosity::pdf_desc Luminosity::F_b_from_ubar_22(5,-2,2,2);
Luminosity::pdf_desc Luminosity::F_b_from_sbar_22(5,-3,2,2);
Luminosity::pdf_desc Luminosity::F_b_from_cbar_22(5,-4,2,2);

Luminosity::pdf_desc Luminosity::F_bbar_from_d_22(-5,1,2,2);
Luminosity::pdf_desc Luminosity::F_bbar_from_u_22(-5,2,2,2);
Luminosity::pdf_desc Luminosity::F_bbar_from_s_22(-5,3,2,2);
Luminosity::pdf_desc Luminosity::F_bbar_from_c_22(-5,4,2,2);

Luminosity::pdf_desc Luminosity::F_bbar_from_dbar_22(-5,-1,2,2);
Luminosity::pdf_desc Luminosity::F_bbar_from_ubar_22(-5,-2,2,2);
Luminosity::pdf_desc Luminosity::F_bbar_from_sbar_22(-5,-3,2,2);
Luminosity::pdf_desc Luminosity::F_bbar_from_cbar_22(-5,-4,2,2);


// total quark luminosities
Luminosity::pdf_desc Luminosity::F_g_from_q_11(0,7,1,1);
Luminosity::pdf_desc Luminosity::F_g_from_qbar_11(0,-7,1,1);
Luminosity::pdf_desc Luminosity::F_g_from_q_21(0,7,2,1);
Luminosity::pdf_desc Luminosity::F_g_from_qbar_21(0,-7,2,1);
Luminosity::pdf_desc Luminosity::F_g_from_q_22(0,7,2,2);
Luminosity::pdf_desc Luminosity::F_g_from_qbar_22(0,-7,2,2);

// total luminosities

Luminosity::pdf_desc Luminosity::F_b_11(5,100,1,1);
Luminosity::pdf_desc Luminosity::F_b_21(5,100,2,1);
Luminosity::pdf_desc Luminosity::F_b_22(5,100,2,2);

Luminosity::pdf_desc Luminosity::F_bbar_11(-5,100,1,1);
Luminosity::pdf_desc Luminosity::F_bbar_21(-5,100,2,1);
Luminosity::pdf_desc Luminosity::F_bbar_22(-5,100,2,2);

Luminosity::pdf_desc Luminosity::F_g_11(0,100,1,1);
Luminosity::pdf_desc Luminosity::F_g_21(0,100,2,1);
Luminosity::pdf_desc Luminosity::F_g_22(0,100,2,2);


Luminosity::Luminosity(const UserInterface& UI)
{
    nf = UI.number_of_flavours;
    mu_f = UI.muf_over_mhiggs * UI.m_higgs;
    mu_r = UI.mur_over_mhiggs * UI.m_higgs;
    perturbative_order =  UI.perturbative_order;
    provider =  UI.pdf_provider;
    pdf_error = UI.pdf_error;
}


void Luminosity::add_pair(const pdf_desc & left, const pdf_desc & right)
{
     bool find=false;
     bool use_new_interpolators = true;
     bool caching = true;
     unsigned ipos;
     for(unsigned i=0; i<pdfs.size() and not find; ++i)
          {
          if(pdfs[i].second == left)
               {
               ipos=i;
               find=true;
               }
          }
            
    if (not find) 
    {
        ipos = pdfs.size();
        pdfs.push_back( pair<CPDF*,pdf_desc>(
            new  CPDF(nf,mu_f,mu_r, perturbative_order,left.i,left.n_as,left.n_eps,provider,pdf_error,use_new_interpolators,caching,left.j)
                                     , left )
                                );
    }
     
     
    find=false;
    unsigned jpos;
    
    for(unsigned j=0; j<pdfs.size() and not find; ++j)
    {
        if(pdfs[j].second == right) 
        {
            jpos = j;
            find=true;
        }
    }        
    if (not find) 
    {   
        jpos = pdfs.size();
        pdfs.push_back( pair<CPDF*,pdf_desc>(
            new  CPDF(nf,mu_f,mu_r, perturbative_order,right.i,right.n_as,right.n_eps,provider,pdf_error,use_new_interpolators,caching,right.j)
                                     , right )
                                );             
    }                            
    pairs.push_back(pair<CPDF*,CPDF*>(pdfs[ipos].first, pdfs[jpos].first));
    

    if (pdfs.empty())
        {
        cout<<"\n\n Critical error: I cannot initialize the _local_current_luminosity. I exit! "<<endl;
        exit(1);
        }
    else
        {
        _local_current_luminosity = vector<double>(pdfs[0].first->size(),0.0);
        }

}




void  Luminosity::set_cur_lumi(const double &x1,const double &x2, vector<double> & newlumi )
{
    //cout<<"\n in Luminosity::operator() number of members "<<pdf_size()<<endl;
    double locres;
    const double almost_zero =1e-16;// 1e-23;
    //:check if x1 and x2 are within (0,1). 
    //: if not set the lumi to zero
   
    if (x1>1.0-almost_zero or x2>1.0-almost_zero or x1<almost_zero or x2<almost_zero)
    {
        //cout<<"\n lumi cut: x1="<<x1<<" x2="<<x2;
        for (unsigned i=0;i<pdf_size();i++)
        {
            newlumi[i]=0.0;
        }
    }
    else //: calculate lumi
    {
        for (unsigned i=0;i<pdf_size();i++)
        {
            locres=0.0;
            for (unsigned k=0;k<pairs.size();k++)
            {
                locres += pairs[k].first->give_f(x1,i)*pairs[k].second->give_f(x2,i)*x1*x2;
            
            }
            newlumi[i]=locres;
        }
    }
     //cout<<"\nin lumi "<<x1<<"\t"<<x2<<"\t"<<newlumi[0];
}

void  Luminosity::set_cur_lumi(const double &x1,const double &x2)
{
    //cout<<"\n in Luminosity::operator() number of members "<<pdf_size()<<endl;
    double locres;
    const double almost_zero =1e-16;// 1e-23;
    //:check if x1 and x2 are within (0,1).
    //: if not set the lumi to zero
    
    if (x1>1.0-almost_zero or x2>1.0-almost_zero or x1<almost_zero or x2<almost_zero)
        {
        //cout<<"\n lumi cut: x1="<<x1<<" x2="<<x2;
        for (unsigned i=0;i<pdf_size();i++)
            {
            _local_current_luminosity[i]=0.0;
            }
        }
    else //: calculate lumi
        {
        for (unsigned i=0;i<pdf_size();i++)
            {
            locres=0.0;
            for (unsigned k=0;k<pairs.size();k++)
                {
                locres += pairs[k].first->give_f(x1,i)*pairs[k].second->give_f(x2,i)*x1*x2;
                
                }
            _local_current_luminosity[i]=locres;
            }
        }
}



double  Luminosity::give_lumi(const double &x1,const double &x2)
{
     //cout<<"\n in Luminosity::operator() number of members "<<pdf_size()<<endl;
     double locres;
     const double almost_zero =1e-16;// 1e-23;
     //:check if x1 and x2 are within (0,1).
     //: if not set the lumi to zero
     
     if (x1>1.0-almost_zero or x2>1.0-almost_zero or x1<almost_zero or x2<almost_zero)
          {
          return 0.0;
          }
     else //: calculate lumi
          {
          
               locres=0.0;
               for (unsigned k=0;k<pairs.size();k++)
                    {
                    locres += pairs[k].first->give_f(x1,0)*pairs[k].second->give_f(x2,0)*x1*x2;
                    
                    }
          return locres;
          
          }
     //cout<<"\nin lumi "<<x1<<"\t"<<x2<<"\t"<<newlumi[0];
}

unsigned Luminosity::pdf_size() const
{ 
    if (pdfs.empty())
    {
        cout<<"\n\n Critical error: you attempt to get the size of the PDF from Luminosity before assigning PDFs to it. I exit! "<<endl;
        exit(1);
    }
    else
    {
        return pdfs[0].first->size();
    }
     
}

vector<double> Luminosity::give_a_s_at_mz()
{
    if (pdfs.empty())
        {
        cout<<"\n\n Critical error: you attempt to get the size of the PDF from Luminosity before assigning PDFs to it. I exit! "<<endl;
        exit(1);
        }
    else
        {
        return pdfs[0].first->alpha_s_at_mz;
        }

}


void Luminosity::alpha_s_at_mz(vector<double> & as_ext)
{
    if (pdfs.empty())
    {
        cout<<"\n\n Critical error: you attempt to get the size of the PDF from Luminosity before assigning PDFs to it. I exit! "<<endl;
        exit(1);
    }
    else
    {
        as_ext = pdfs[0].first->alpha_s_at_mz;
    }
}


double InterpolatedLuminosity::integrate_out_x1(const double &z)
{
     int TRAPEZIUM=5000;
     double tau_over_z=tau/z;
     double minus_log_tau_over_z = -log(tau_over_z);
     double lambda=0.0;
     double I=0.0;
     for (int k=0; k<TRAPEZIUM; k++)
          {
          lambda += 1.0/TRAPEZIUM;
          I += 1.0/TRAPEZIUM * minus_log_tau_over_z * give_lumi(pow(tau_over_z,lambda),pow(tau_over_z,1.0-lambda));
          }
     return I;
     
}


//void Luminosity::set_alpha_s()
//{
//#ifdef debug
//     cout<<"\n["<<__func__<<"]";
//#endif
//     
//     alpha_s_at_mz(alpha_s_at_mz_vector); //: setting the alpha_s_at_mz vector via lumi (that owns the pdfs)
//     for (int i=0;i<alpha_s_at_mz_vector.size();i++) {cout<<"\n"<<i<<":"<<alpha_s_at_mz_vector[i];}
//     evolve_alpha_s_from_mz_to_mur();
//     
//}


void Luminosity::evolve_alpha_s_from_mz_to_mur(vector<double> & alpha_s_external)
{
     if (pdfs.empty())
     {
          cout<<"\n\n Critical error: you attempt to get the size of the PDF from Luminosity before assigning PDFs to it. I exit! "<<endl;
          exit(1);
     }
     else
     {
          alpha_s_at_mz_vector = pdfs[0].first->alpha_s_at_mz;
     }
     cout<<"\n alpha_s(mZ)="<<alpha_s_at_mz_vector[0];
     //alpha_s_vector = alpha_s_at_mz_vector;
//     for (int i=0;i<alpha_s_at_mz_vector.size();i++) 
//     {
//          cout<<"\n"<<i<<":"<<alpha_s_at_mz_vector[i];
//     }

     cout<<"\nEvolving alpha_s with evolution of order : "<<perturbative_order
          <<" from "<<91.187<<" to "<<mu_r<<endl;
     for (int i=0;i<alpha_s_at_mz_vector.size();i++)
     {
          const double evolved_as_value=SSC(alpha_s_at_mz_vector[i],mu_r,perturbative_order);
       //   alpha_s_vector.push_back(evolved_as_value);
          alpha_s_external.push_back(evolved_as_value);
     }

     cout<<"\t:"<<alpha_s_at_mz_vector[0]<<" ->  "<<alpha_s_external[0]<<endl;
}

void Luminosity::evolve_mb_from_mb_ref_to_mur(double& m_bottom,vector<double>& yuk_ext)
{
     double mb_to_yb_constant = sqrt(sqrt(2.0)*consts::G_fermi);
     double reference_scale = 10.0;
     double lambda0 = mb_to_yb_constant*m_bottom;
     int pertorder_for_running = perturbative_order;//perturbative_order;
     for (int i=0;i<pdf_size();i++)
          {
          yukawa_b_vector.push_back( yukawa_b(mu_r,pertorder_for_running,
                                              reference_scale,lambda0,alpha_s_at_mz_vector[i])
                                    ); 
          yuk_ext.push_back(yukawa_b_vector[i]);
          }
     double new_m_bot = yukawa_b_vector[0]/mb_to_yb_constant;
     cout<<"\nm_bot from "<<m_bottom<<" @ "<<reference_scale
	<<" ->  "<<new_m_bot<<" @ "<<mu_r<<endl;
     m_bottom = new_m_bot;
     
}


double Luminosity::yukawa_b(double mur,int iord,double mur0,double lambdab0,
                                double asZ)
{ 
#ifdef debug
     cout<<"\n["<<__func__<<"]";
#endif
     //cout<<"\n["<<__func__<<" "<<mur<<" "<<iord<<" "<<mur0<<" "<<lambdab0<<" "<<nf<<" "<<asZ;
     double retval=0.0;          
     double as_m0 = SSC(asZ,mur0,iord)/consts::Pi;
     double as_mr = SSC(asZ,mur,iord)/consts::Pi;
     //double nf=consts::nf;     
     double nf=5.0;
     double      b0 = 11.0 / 4.0 - nf / 6.0;
     double      b1 = 51.0 / 8.0 - 19.0 / 24.0 * nf;
     double      b2 = 2857.0 / 128.0 - 5033.0 / 1152.0 * nf + 325.0 / 3456.0 * nf*nf;
     double      g0 = 1.0;
     double g1 = 101.0 / 24.0 - 5.0 / 36.0 * nf;
     double g2 = 1249.0 / 64.0 - 2.284121493373735 * nf - 35.0 / 1296.0 * nf *nf;
     if (iord ==0) 
          {
          retval = lambdab0 * pow(as_mr / as_m0,g0 / b0);
          }
     else if (iord==1)
          {
          double A1 = g1 / b0 - b1 * g0 / pow(b0,2.0) ;
          retval = lambdab0 * pow(as_mr / as_m0,g0 / b0)
		* (1.0 + A1 * as_mr ) / (1.0 + A1 * as_m0);
          }
     else if (iord==2)
          {
          double A1 =  g1 / b0 - b1 * g0 / pow(b0,2.0) ;
          double A2 = pow(g1 / b0 - b1 * g0 /pow(b0,2.0),2.0) 
          + g2 / b0 - g1 * b1 / pow(b0,2.0) 
          + b1 * b1 * g0 / pow(b0,3.0)
          - b2 * g0/ pow(b0,2.0)
          ;
          double expr1 = 1.0 + A1 * as_mr + 0.5 * A2 * pow(as_mr,2.0);
          double expr2 = 1.0 + A1 * as_m0 + 0.5 * A2 * pow(as_m0,2.0);
          retval = lambdab0 * pow(as_mr / as_m0,g0 / b0) * expr1 / expr2;
          }
     else
          {
          cout<<"\n["<<__func__<<" Error unrecognized perturbative order: "<<iord<<endl;
          exit(0);
          }
     return retval;
}



double Luminosity::SSC(double asZ,double mur,int porder)
{
#ifdef debug
     cout<<"\n["<<__func__<<"]";
#endif
     // Nf = 5 **************** <---------
     //double b0= 1.916666667;
     //double b1= 1.260869565;
     //double b2= 1.474788647;
     
     //: note that what is called b1,b2 below is actually b1/b0 and b2/b0
     //double nf=consts::nf;     
//     double nf=5.0;
     if (consts::nf != 5.0)
          {
          cout<<"\n\t\t\t\t ****** ALERT in SSC: nf !=5, but in the PDF evolution it is set to 5!! "<<endl;
          //exit(1);
          }
     double      b0 = 11.0 / 4.0 - consts::nf / 6.0;
     double      b1 = (51.0 / 8.0 - 19.0 / 24.0 * consts::nf)/b0;
     double      b2 = (2857.0 / 128.0 - 5033.0 / 1152.0 * consts::nf + 325.0 / 3456.0 * pow(consts::nf,2.0))/b0;
     
     
     double as0=asZ/consts::Pi;
     double Mz=91.187;
     
     
     //      step=dlog(mu**2/Mz**2)/100
     double step=log(pow(mur,2.0)/pow(Mz,2.0))/100000.0;
     //cout<<"\nas0="<<as0<<"\tporder="<<porder;
     for (int i=0;i<100000;i++)
          {
          double incr=0.0;
          if (porder==2)  
               {
               incr = step*b0*pow(as0,2.0)*(1.0+b1*as0 + b2*pow(as0,2.0));
               }
          else if(porder==1)
               {
               incr = step*b0*pow(as0,2.0)*(1.0+b1*as0);
               }
          else if (porder==0)
               {
               incr = step*b0*pow(as0,2.0)*(1.0);
               }
          else
               {
               cout<<"\n["<<__func__<<"]\t  : Unknown perturbative order : "<<porder<<endl;
               exit(0);
               }
          as0 = as0 - incr;
          }
     return(as0*consts::Pi);
}







