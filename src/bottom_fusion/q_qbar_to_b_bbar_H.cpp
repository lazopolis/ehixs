//
//
//			channel : q qbar -> b bbar H
//			
//			sectors

#include "q_qbar_to_b_bbar_H.h"


q_qbar_to_b_bbar_H::q_qbar_to_b_bbar_H(const UserInterface& UI): ExclusiveClass(UI)
{
    if (sector_control==60)
    {
        lumi.add_pair(Luminosity::F_u_00,Luminosity::F_ubar_00);
        lumi.add_pair(Luminosity::F_d_00,Luminosity::F_dbar_00);
        lumi.add_pair(Luminosity::F_c_00,Luminosity::F_cbar_00);
        lumi.add_pair(Luminosity::F_s_00,Luminosity::F_sbar_00);
        lumi.add_pair(Luminosity::F_ubar_00,Luminosity::F_u_00);
        lumi.add_pair(Luminosity::F_dbar_00,Luminosity::F_d_00);
        lumi.add_pair(Luminosity::F_cbar_00,Luminosity::F_c_00);
        lumi.add_pair(Luminosity::F_sbar_00,Luminosity::F_s_00);
        
    }
    //: constructing the cur_lumi and cur_lumi_LO vectors (necessary because lumi assigns to cur_lumi[i] instead of pushing back)
    cur_lumi = vector<double>(lumi.pdf_size(),0.0);
    cur_lumiLO = vector<double>(lumi.pdf_size(),0.0);
    //: setting topologies (Franz's functions and number of sectors per function) for the double real
    set_topologies();
}


void q_qbar_to_b_bbar_H::NNLO() //: action is here
{
string nnlo_variant="franz";
if (sector_control==60 and pole==0)
	{
	if (nnlo_variant=="franz")
		{
		NNLO_RR_Franz();
		}
	else
		{
		NNLO_RR_Romain();
		}
	}
else
	{
	cout<<"\nerror: sector control is "<<sector_control<<" but qqbar->bbH channel that is initialized needs sector 60 \n\n"<<endl;
	exit(1);
	}
}


void q_qbar_to_b_bbar_H::NNLO_RR_Romain()
{
cout<<"\n gg -> bbH is not yet implemented in Romain's version "<<endl;
}

void q_qbar_to_b_bbar_H::NNLO_RR_Franz()
{	
//: pref_sbb = pi * Y^2 / ( 2*Nc*mh^2 )
//  0.389379*10^9 is the GeV to pb
double pref_sbb = consts::Pi/6.0 * 0.389379e9
						*pow(Model.bottom.Y * yukawa_b_vector[0],2.0)
						/pow(Model.higgs.m,2.0)
						; 

double special_normalization_factor_for_double_real=12.0; 

double x1,x2,z,meas;

parametrization_for_NLO_kinematics(x1,x2,z,meas);
lumi(x1,x2,cur_lumi);
    if (cur_lumi[0]==0.0)//: case in which x1 or x2 are out of bounds.
    {
        book_production_event();
    }
    else 
    {
        double Nij=1.0/36.0; //: averaging factor for the qqbar->b bbar H channel
        double res;
        double lh=log_muf_sq_over_mh_sq;	
        double curs=pow(Model.higgs.m,2.0)/z; 
        double wd=1.0/(1.0-z)
            *cur_lumi[0]
            *pow(alpha_s_vector[0]/consts::Pi,2.0)
            *meas*pref_sbb
            *special_normalization_factor_for_double_real
            *Nij;

        
        qqbarbbbarH1n(1,0,curs,x1,x2,z,-lh,wd,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
        //: no z-subtraction here. It is assumed that f(z=1)/(1-z) is finite, i.e. that f(1)->0 faster than 1-z
    }
}


void q_qbar_to_b_bbar_H::set_topologies(){}
