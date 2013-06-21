//
//
//			channel : g g -> b bbar H
//			
//			sectors

#include "g_g_to_b_bbar_H.h"



g_g_to_b_bbar_H::g_g_to_b_bbar_H(const UserInterface& UI): ExclusiveClass(UI)
{
    if (sector_control>=50 and sector_control<=54)
    {
        string nnlo_variant="franz";
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_g_00);
        if (nnlo_variant=="franz")
		{
            pointer_to_function_for_sector = &g_g_to_b_bbar_H::NNLO_RR_gg_2_bbH_Franz;
		}
        else
		{
            pointer_to_function_for_sector = &g_g_to_b_bbar_H::NNLO_RR_gg_2_bbH_Romain;
		}
    }
    if (sector_control==55) //: bg convolution
    {
        lumi.add_pair(Luminosity::F_b_from_g_11,Luminosity::F_g_00);
        lumi.add_pair(Luminosity::F_bbar_from_g_11,Luminosity::F_g_00);
        pointer_to_function_for_sector = &g_g_to_b_bbar_H::NNLO_bg_conv;
    }
    if (sector_control==56) //: gb convolution
    {
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_b_from_g_11);
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_bbar_from_g_11);
        pointer_to_function_for_sector = &g_g_to_b_bbar_H::NNLO_gb_conv;
    }
    if (sector_control==57) //: bb convolution
    {
        lumi.add_pair(Luminosity::F_b_from_g_11,Luminosity::F_bbar_from_g_11);
        lumi.add_pair(Luminosity::F_bbar_from_g_11,Luminosity::F_b_from_g_11);
        pointer_to_function_for_sector = &g_g_to_b_bbar_H::NNLO_convolution_gg_2_bb_bb;
    }
    //: constructing the cur_lumi and cur_lumi_LO vectors (necessary because lumi assigns to cur_lumi[i] instead of pushing back)
    cur_lumi = vector<double>(lumi.pdf_size(),0.0);
    cur_lumiLO = vector<double>(lumi.pdf_size(),0.0);
    //: setting topologies (Franz's functions and number of sectors per function) for the double real
    set_topologies();
}


void g_g_to_b_bbar_H::NNLO() //: action is here
{
prepare_normalization();
(this->*pointer_to_function_for_sector)();
}


void g_g_to_b_bbar_H:: prepare_normalization()
{
    //: pref_sbb = pi * Y^2 / ( 2*Nc*mh^2 )
    //  0.389379*10^9 is the GeV to pb
    pref_sbb = consts::Pi/6.0 * 0.389379e9
						*pow(Model.bottom.Y * yukawa_b_vector[0],2.0)
						/pow(Model.higgs.m,2.0)
						; 
    special_normalization_factor_for_double_real=12.0; 
    parametrization_for_LO_kinematics(x1LO,x2LO,zLO,measLO);
    parametrization_for_NLO_kinematics(x1,x2,z,meas);
    
    lambda = xx_vegas[2]; 
    
    lumi(x1LO,x2LO,cur_lumiLO);//:sets the cur_lumi_LO according to the luminosity initialized in the constructor for this sector
    lumi(x1,x2,cur_lumi);       //:sets the cur_lumi according to the luminosity initialized in the constructor for this sector
    
    //: Nij holds various fractional factors that depend on subchannel
    //: like the initial state averaging factor
    Nij=1.0/256.0; 

    lh=log_muf_sq_over_mh_sq;		
    curs=pow(Model.higgs.m,2.0)/z; 

    Log_1mz=log(1.0-z);
    cursLO=pow(Model.higgs.m,2.0)/zLO;
}


void g_g_to_b_bbar_H::NNLO_convolution_gg_2_bb_bb()//: we are only in here if sector = 125 (gg->bbH channel)
{
if  (pole==2 )
	{
	//double pref_sbb = consts::Pi/6.0 * 0.389379e9
	//					*pow(Model.bottom.Y * yukawa_b_vector[0],2.0)
	//					/pow(Model.higgs.m,2.0); 
	//parametrization_for_LO_kinematics(x1LO,x2LO,zLO,measLO);
    //lumi(x1LO,x2LO,cur_lumiLO);
	double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);//: note that this term is of order a_s^2
	Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
	}

}



void g_g_to_b_bbar_H::NNLO_gb_conv()//: F_g->b x F_bb * sigma_gb with gluon from beam 1
{
    NLO_ME.set(z,lambda,lh);//:passing z,lambda and logs to NLO_ME

    double sigma_central =	pref_sbb*meas*cur_lumi[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
    if (pole==2)
	{
        Jnlo(sigma_central*NLO_ME.gb_coll_pole(),x1,x2,z,0.0);
	}
    else if (pole==1)
	{
        Jnlo(sigma_central*NLO_ME.gb_coll_fin(),x1,x2,z,0.0);
        Jnlo(sigma_central*NLO_ME.gb_hard_fin(),x1,x2,z,lambda);
    }
    else if (pole==0)
	{
        Jnlo(sigma_central*NLO_ME.gb_coll_e(),x1,x2,z,0.0);
        Jnlo(sigma_central*NLO_ME.gb_hard_e(),x1,x2,z,lambda);
    }
}

void g_g_to_b_bbar_H::NNLO_bg_conv()//: F_g->b x F_bb * sigma_bg with gluon from beam 2 
{
    NLO_ME.set(z,lambda,lh);//:passing z,lambda and logs to NLO_ME

    double sigma_central =	pref_sbb*meas*cur_lumi[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
    if (pole==2)
	{
        Jnlo(sigma_central*NLO_ME.bg_coll_pole(),x1,x2,z,1.0);
	}
    else if (pole==1)
	{
        Jnlo(sigma_central*NLO_ME.bg_coll_fin(),x1,x2,z,1.0);
        Jnlo(sigma_central*NLO_ME.bg_hard_fin(),x1,x2,z,lambda);
    }
    else if (pole==0)
	{
        Jnlo(sigma_central*NLO_ME.bg_coll_e(),x1,x2,z,1.0);
        Jnlo(sigma_central*NLO_ME.bg_hard_e(),x1,x2,z,lambda);
    }
}



#define ONCE_ROMAIN
#ifndef ONCE_ROMAIN
#define ONCE_ROMAIN
//:
// Parametrizations
//#include "parametrizations.hpp"
// Topology subtraction
//#include "subtraction.hpp"
// Definition of all possible topologies
//#include "topologies.hpp"
// Definition of the jet function
//#include "jet.hpp"
// Creates all the Tii::Call functions you need
//#include "input.hpp"
#endif


//g_g_to_b_bbar_H* bbfusion_global;

void g_g_to_b_bbar_H::NNLO_RR_gg_2_bbH_Romain()
{
cout<<"\n gg -> bbH is not yet implemented in Romain's version "<<endl;
}

//:--------------------------------------------------------------------------
//: integrating with Franz below


void g_g_to_b_bbar_H::NNLO_RR_gg_2_bbH_Franz()
{	
//: pref_sbb = pi * Y^2 / ( 2*Nc*mh^2 )
//  0.389379*10^9 is the GeV to pb
//double pref_sbb = consts::Pi/6.0 * 0.389379e9
//						*pow(Model.bottom.Y * yukawa_b_vector[0],2.0)
//						/pow(Model.higgs.m,2.0)
//						; 
//:integrating with Franz below
//bbfusion_global=this;
//double special_normalization_factor_for_double_real=12.0; 
//: testing franz's benchmark at z=0.01 (remember to disable N_ij below though)
/*
z=0.1;
x1=sqrt(tau/z);
x2=sqrt(tau/z);
curs=pow(Model.higgs.m,2.0)/z;
wd=1.0;
*/
//:end of test code
//double x1LO,x2LO,measLO,zLO,x1,x2,z,meas;
//parametrization_for_LO_kinematics(x1LO,x2LO,zLO,measLO);
//lumi(x1LO,x2LO,cur_lumiLO);
//parametrization_for_NLO_kinematics(x1,x2,z,meas);
//lumi(x1,x2,cur_lumi);
//double Nij=1.0/256.0; //: averaging factor for the gg->bbH channel
//double L;
//double res;
//double lh=log_muf_sq_over_mh_sq;		
//double curs=pow(Model.higgs.m,2.0)/z; 
    if (cur_lumi[0]==0.0)//: case in which x1 or x2 are out of bounds.
    {
        book_production_event();
    }
    else 
    {
        double wd=1.0/(1.0-z)
                    *cur_lumi[0]
                    *pow(alpha_s_vector[0]/consts::Pi,2.0)
                    *meas*pref_sbb
                    *special_normalization_factor_for_double_real
                    *Nij;
        int sector_offset = 50;
        int current_topology = sector_control-sector_offset;//: the offset on sector_control : 50
        for (int cur_sector=sector_init[current_topology];cur_sector<=sector_fin[current_topology];cur_sector++)
        {
            NNLO_RR_epsilon_expansion(FRTopologies[current_topology],cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"two_gluons");
            //: no z-subtraction here. It is assumed that f(z=1)/(1-z) is finite, i.e. that f(1)->0 faster than 1-z
        }   
    }

}



void g_g_to_b_bbar_H::NNLO_RR_epsilon_expansion(
		void (*my_func)(const int&, const int&, const double&,const double&,const double&,
		const double&, const double&, const double&, const double&, const double&, const double&, const double&, const double&),
		int sector,double z,double lh,double wd,double L,
		double lambda1,double lambda2,double lambda3,double lambda4,double s,double x1,double x2,const string& number_of_II_gluons)
{
//cout<<"\n["<<__func__<<"]"<<endl;
double res;
double c1,c2,c3;
if (number_of_II_gluons=="no_gluons")
	{
	c1=(-4.0*L);
	c2=(8.0*L*L);
	c3=(-32.0/3.0*L*L*L);
	}
else if (number_of_II_gluons=="one_gluon")
	{
	c1=(1.0-4.0*L);
	c2=(1.0-4.0*L+8.0*L*L);
	c3=(1.0-4.0*L+8.0*L*L-(32.0/3.0)*L*L*L);
	}
else if (number_of_II_gluons=="two_gluons")
	{
	c1=(2.0-4.0*L);
	c2=(3.0-8.0*L+8.0*L*L);
	c3=(4.0-12.0*L+16.0*L*L-(32.0/3.0)*L*L*L);
	}
if (pole==0)
	{
	(*my_func)(sector,0,s,x1,x2,z, -lh, wd, lambda1, lambda2, lambda3, lambda4,res);
	(*my_func)(sector,-1,s,x1,x2,z, -lh, wd*c1, lambda1, lambda2, lambda3, lambda4,res);
	(*my_func)(sector,-2,s,x1,x2,z, -lh, wd*c2, lambda1, lambda2, lambda3, lambda4,res);
	(*my_func)(sector,-3,s,x1,x2,z, -lh, wd*c3, lambda1, lambda2, lambda3, lambda4,res);
	}
if (pole==1)
	{
	(*my_func)(sector,-1,s,x1,x2,z, -lh, wd, lambda1, lambda2, lambda3, lambda4,res);
	(*my_func)(sector,-2,s,x1,x2,z, -lh, wd*c1, lambda1, lambda2, lambda3, lambda4,res);
	(*my_func)(sector,-3,s,x1,x2,z, -lh, wd*c2, lambda1, lambda2, lambda3, lambda4,res);
	}
if (pole==2)
	{
	(*my_func)(sector,-2,s,x1,x2,z, -lh, wd, lambda1, lambda2, lambda3, lambda4,res);
	(*my_func)(sector,-3,s,x1,x2,z, -lh, wd*c1, lambda1, lambda2, lambda3, lambda4,res);
	}
if (pole==3)
	{
	(*my_func)(sector,-3,s,x1,x2,z, -lh, wd, lambda1, lambda2, lambda3, lambda4,res);
	}
}


#include "nnlo_double_real/MggbbH.cpp"

void g_g_to_b_bbar_H::set_topologies()
{
cout<<"\n["<<__func__<<"]";
cout<<"\tsetting topologies "<<endl;
//: b bbar -> g g H 

//: g g -> b bbar H
FRTopologies[0]=ggbbH1n;sector_init[0]=1;sector_fin[0]=1;  //:  b bbar -> q qbar H topology 1 secs 1		sector_control 50
FRTopologies[1]=ggbbH2n;sector_init[1]=1;sector_fin[1]=2;  //:  b bbar -> q qbar H topology 1 secs 1-2		sector_control 51
FRTopologies[2]=ggbbH3n;sector_init[2]=1;sector_fin[2]=1;  //:  b bbar -> q qbar H topology 1 secs 1		sector_control 52
FRTopologies[3]=ggbbH4n;sector_init[3]=1;sector_fin[3]=1;  //:  b bbar -> q qbar H topology 1 secs 1		sector_control 53
FRTopologies[4]=ggbbH5n;sector_init[4]=1;sector_fin[4]=8;  //:  b bbar -> q qbar H topology 1 secs 1-8		sector_control 54

  // g g -> b bbar H 
  RTopologies.push_back(&TopologySubtraction<C00 , MggbbH_1_e0, MggbbH_1_e1, MggbbH_1_e2, MggbbH_1_e3, J>);
  RTopologies.push_back(&TopologySubtraction<C11a, MggbbH_2_e0, MggbbH_2_e1, MggbbH_2_e2, MggbbH_2_e3, J>);
}
