#include "gluon_fusion_ew_coefficients.h"

#include "nlo_exact_matrix_elements.h"


//------------------------------------------------------------------------------

GluonFusionEWCoefficients::GluonFusionEWCoefficients(const CModel& model)
{
    
#include "electroweak_data.h"
    
    // reads EWK  corrections from Fig. 21 of
    //      http://arXiv.org/pdf/0809.3667
    //  by Actis, Passarino, Sturm, Uccirati
    //   Data in  file "./electroweak.h" provided by the authors.
    //   They have chosen the following paramegters:
    
    //       Mw = 80.398
    //       GammaW = 2.093
    //       Mz = 91.1876
    //       GammaZ = 2.4952
    //       Gfermi = 1.16637e-5 !/GeV^2
    //       a(0) = 1.0/137.0359911.0
    //       alphas_Mz = 0.118.0
    //       Mtop =  170.9
    
    
    model_=model;
    
    cout<<"\nCalcualting ew correction factor";
    const double mtop_pass = 170.9;
    
	const double eff_mh =model.higgs.m()*mtop_pass/model.top.m();
    
    if (eff_mh<100.0 or eff_mh>500.0)
    {
        cout<<"\nSoft ew corrections not available (mh<100 or mh>500). They are set to zero since mh = "<<eff_mh
        <<" mtop_pass/ mtop = "<< mtop_pass/model.top.m();
        NLO_ew_coeff_ = 0.0;
    }
    else
    {
        int N = ew_data.size();
        int position;
        
        for (int i=0;i<ew_data.size()-1;i++)
        {
            const double mleft = ew_data[i]->mass;
            const double mright = ew_data[i+1]->mass;
            if (mleft < eff_mh and eff_mh<mright)
            {
                position = i;
                if (mright-eff_mh < eff_mh-mleft) {position = i+1;}
                break;
            }
        }
        int ibefore,ihere,iafter;
        if (position==0){ibefore = 0;ihere=1;iafter=2;}
        else if (position==N-1){ibefore = N-3;ihere=N-2;iafter=N-1;}
        else
        {
            
            ibefore = position-1;
            ihere = position;
            iafter = position+1;
        }
        cout<<"\n position = ["<<ibefore<<","<<ihere<<","<<iafter<<"] / "<<N<<endl;
        double x[3]={ew_data[ibefore]->mass,
            ew_data[ihere]->mass,
            ew_data[iafter]->mass};
        double y[3]={ew_data[ibefore]->deltaew,
            ew_data[ihere]->deltaew,
            ew_data[iafter]->deltaew};
        vector<double> c = givecoeff(x,y);
        double res = (c[2] + c[1] * eff_mh + c[0] * eff_mh*eff_mh)/100.0;
        // calculating born for top only with mtop = 170.9 GeV
        // which was the choice of the authors of 0809.3667
        complex<double> Wt(4.0*170.9*170.9/pow(eff_mh,2.0),0.0);
        complex<double> xt = -Wt/pow(sqrt(1.0-Wt)+1.0,2.0);
        complex<double> born_special = born(xt);
        NLO_ew_coeff_ = (sqrt(1.0+res)-1.0 ) * born_special;
        cout<<"\n NLO ew coeff = "<<NLO_ew_coeff_;
    }
}


vector<double>  GluonFusionEWCoefficients::givecoeff(double x[3],double y[3])
{
    vector<double> res;
    const double dx12 = x[0]-x[1];
    const double dx23 = x[1]-x[2];
    const double dx31 = x[2]-x[0];
    
    const double den=dx12*dx23*dx31;
    res.push_back((-y[0]*dx23-y[1]*dx31-y[2]*dx12)/den);
    
    res.push_back(( y[0]*(x[1]*x[1]-x[2]*x[2])
                   +y[1]*(x[2]*x[2]-x[0]*x[0])
                   +y[2]*(x[0]*x[0]-x[1]*x[1]) ) /den);
    
    res.push_back( (-y[0]*dx23*x[1]*x[2]
                    -y[1]*dx31*x[0]*x[2]
                    -y[2]*dx12*x[0]*x[1]) / den);
    return res;
}




extern "C" {
    // electroweak form factors
    complex<double> fa_massless_(double*,double*, double*,
                                complex<double>*,complex<double>*);
    
}

inline complex<double> FA_massless(const double& s, const double& t, const double& u, const complex<double>& mhsq,const complex<double>& mvsq)
{ return fa_massless_((double*)&s, (double*)&t, (double*)&u,
                      (complex<double>*)&mhsq,(complex<double>*)&mvsq); }

double GluonFusionEWCoefficients::EwkUUbar(const double& s,const double& z, const double& lambda)
{
    complex<double> FFstar (0.0,0.0);
    double t = -s * (1.0-z)*lambda;
    double u = -s * (1.0-z)*(1.0-lambda);
    
    double prefactor = 1.0/4.0/consts::pi_square * (1.0-z)/z;
    complex<double> Mqcd = 4.0/3.0 * sum_of_Aqqgh(z,&model_) / s;
    complex<double> Mewk(0.0);
    
    
    double mhsq = model_.higgs.m()*model_.higgs.m();
    
    for (int iboson=0;iboson<model_.vector_bosons.size();iboson++)
    {
        VectorBoson* V = (VectorBoson*)(model_.vector_bosons[iboson]);
        complex<double> mv_sq = V->cm_sq();
        complex<double> gw_up = pow(V->cv_up,2.0) + pow(V->ca_up,2.0);
        complex<double> lambda_v = V->lamda;
        //cout<<"\n"<<gw_up;
        Mewk += - mv_sq * gw_up * lambda_v
        * (  t*t * FA_massless(t,u,s,mhsq,mv_sq)
           + u*u * FA_massless(u,t,s,mhsq,mv_sq));
    }
    
    
    
    double res = prefactor* real( Mqcd * conj(Mewk) + conj(Mqcd) * Mewk );
    if (abs(s+t+u - mhsq)>1e-4)
    {
        cout<<s<<" "<<t<<" "<<u<<" "<<mhsq<<" "<<s+t+u<<endl;
    }
    return res;
}

double GluonFusionEWCoefficients::EwkDDbar(const double& s,const double& z, const double& lambda)
{
    complex<double> FFstar (0.0,0.0);
    double t = -s * (1.0-z)*lambda;
    double u = -s * (1.0-z)*(1.0-lambda);
    
    double prefactor = 1.0/4.0/consts::pi_square * (1.0-z)/z;
    complex<double> Mqcd = 4.0/3.0 *sum_of_Aqqgh(z,&model_) / s;
    complex<double> Mewk(0.0);
    
    double mhsq = model_.higgs.m()*model_.higgs.m();
    for (int iboson=0;iboson<model_.vector_bosons.size();iboson++)
    {
        VectorBoson* V = (VectorBoson*)(model_.vector_bosons[iboson]);
        complex<double> mv_sq = V->cm_sq();
        complex<double> gw_down = pow(V->cv_down,2.0) + pow(V->ca_down,2.0);
        complex<double> lambda_v = V->lamda;
        //cout<<t<<" "<<u<<" "<<s<<" "<<mhsq<<" "<<sqrt(mv_sq)<<endl;
        Mewk += - mv_sq * gw_down * lambda_v
        * (  t*t * FA_massless(t,u,s,mhsq,mv_sq)
           + u*u * FA_massless(u,t,s,mhsq,mv_sq));
    }
    return prefactor* real( Mqcd * conj(Mewk) + conj(Mqcd) * Mewk );
    
}


double GluonFusionEWCoefficients::EwkUG(const double& s,const double& z, const double& lambda)
{
    complex<double> FFstar (0.0,0.0);
    double t = -s * (1.0-z)*lambda;
    double u = -s * (1.0-z)*(1.0-lambda);
    
    double prefactor = 1.0/4.0/consts::pi_square * (1.0-z)/z
    * 9.0/64.0*u/s;
    complex<double> Mqcd = 4.0/3.0 *sum_of_Aqqgh(z,&model_) / s;
    complex<double> Mewk(0.0);
    
    double mhsq = model_.higgs.m()*model_.higgs.m();
    for (int iboson=0;iboson<model_.vector_bosons.size();iboson++)
    {
        VectorBoson* V = (VectorBoson*)(model_.vector_bosons[iboson]);
        complex<double> mv_sq = V->cm_sq();
        complex<double> gw_up = pow(V->cv_up,2.0) + pow(V->ca_up,2.0);
        complex<double> lambda_v = V->lamda;
        //cout<<t<<" "<<u<<" "<<s<<" "<<mhsq<<" "<<sqrt(mv_sq)<<endl;
        Mewk += - mv_sq * gw_up * lambda_v
        * (  s*s * FA_massless(s,t,u,mhsq,mv_sq)
           + t*t * FA_massless(t,s,u,mhsq,mv_sq));
    }
    return prefactor* real( Mqcd * conj(Mewk) + conj(Mqcd) * Mewk );
    
}

double GluonFusionEWCoefficients::EwkDG(const double& s,const double& z, const double& lambda)
{
    complex<double> FFstar (0.0,0.0);
    double t = -s * (1.0-z)*lambda;
    double u = -s * (1.0-z)*(1.0-lambda);
    
    double prefactor = 1.0/4.0/consts::pi_square * (1.0-z)/z
    *9.0/64.0*u/s;
    complex<double> Mqcd = 4.0/3.0 * sum_of_Aqqgh(z,&model_) / s;
    complex<double> Mewk(0.0);
    
    double mhsq = model_.higgs.m()*model_.higgs.m();
    for (int iboson=0;iboson<model_.vector_bosons.size();iboson++)
    {
        VectorBoson* V = (VectorBoson*)(model_.vector_bosons[iboson]);
        complex<double> mv_sq = V->cm_sq();
        complex<double> gw_down = pow(V->cv_down,2.0) + pow(V->ca_down,2.0);
        complex<double> lambda_v = V->lamda;
        //cout<<t<<" "<<u<<" "<<s<<" "<<mhsq<<" "<<sqrt(mv_sq)<<endl;
        Mewk += - mv_sq * gw_down * lambda_v
        * (  s*s * FA_massless(s,t,u,mhsq,mv_sq)
           + t*t * FA_massless(t,s,u,mhsq,mv_sq));
    }
    return prefactor* real( Mqcd * conj(Mewk) + conj(Mqcd) * Mewk );
    
}






//------------------------------------------------------------------------------


