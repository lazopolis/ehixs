#include "b_b_to_H_b_b.h"

typedef void (*pointer_to_Franz)(const int&, const int&, const double&,const double&,const double&,
		const double&, const double&, const double&, const double&, const double&, const double&);

b_b_to_H_b_b::b_b_to_H_b_b(const UserInterface& UI): ExclusiveClass(UI)
{
    const string nnlo_variant="franz";
    if (sector_control>=71 and sector_control<=78)
    {
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_bbar_00);
        if (nnlo_variant=="franz")
		{
            pointer_to_function_for_sector = &b_b_to_H_b_b::NNLO_RR_Franz;
		}
        else
		{
            pointer_to_function_for_sector = &b_b_to_H_b_b::NNLO_RR_Romain;
		}
    }
    if (sector_control==79) //: bg convolution
    {
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_b_11);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_bbar_11);
        pointer_to_function_for_sector = &b_b_to_H_b_b::NNLO_bg_conv;
    }
    if (sector_control==70) //: gb convolution
    {
        lumi.add_pair(Luminosity::F_g_from_b_11,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_g_from_bbar_11,Luminosity::F_bbar_00);
        pointer_to_function_for_sector = &b_b_to_H_b_b::NNLO_gb_conv;
    }
    if (sector_control==69) //: bb double convolution 1/e^2
    {
        lumi.add_pair(Luminosity::F_b_from_bbar_22,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_b_22);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_bbar_22);
        lumi.add_pair(Luminosity::F_bbar_from_b_22,Luminosity::F_b_00);
        //my_memfunc_ptr = &SomeClass::some_member_func;
        pointer_to_function_for_sector = &b_b_to_H_b_b::NNLO_bb_double_conv_e2;
    }
    if (sector_control==68) //: bb double convolution 1/e
    {
        lumi.add_pair(Luminosity::F_b_from_bbar_21,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_b_21);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_bbar_21);
        lumi.add_pair(Luminosity::F_bbar_from_b_21,Luminosity::F_b_00);
        pointer_to_function_for_sector = &b_b_to_H_b_b::NNLO_bb_double_conv_e1;
    }
    //: constructing the cur_lumi and cur_lumi_LO vectors (necessary because lumi assigns to cur_lumi[i] instead of pushing back)
    cur_lumi = vector<double>(lumi.pdf_size(),0.0);
    cur_lumiLO = vector<double>(lumi.pdf_size(),0.0);
    //: setting topologies (Franz's functions and number of sectors per function) for the double real
    set_topologies();
}




void b_b_to_H_b_b::NNLO()
{
    prepare_normalization();
    string nnlo_variant="franz";
    (this->*pointer_to_function_for_sector)();
}

void b_b_to_H_b_b:: prepare_normalization()
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
    Nij=1.0/36.0 * 1.0/2.0; //: averaging factor for the bb->bbH channel
                        //: including the 1/2 symmetry factor due to identical 
                        //: final state b-quarks

    lh=log_muf_sq_over_mh_sq;		
    curs=pow(Model.higgs.m,2.0)/z; 

    Log_1mz=log(1.0-z);
    cursLO=pow(Model.higgs.m,2.0)/zLO;
}



//:=========================================== B Bbar ==============================================
//: bbar lumi 

void b_b_to_H_b_b::NNLO_gb_conv()//: F_g->b x F_bb * sigma_gb with gluon from beam 1
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

void b_b_to_H_b_b::NNLO_bg_conv()//: F_g->b x F_bb * sigma_bg with gluon from beam 2 
{
    double res=0.0;
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

void b_b_to_H_b_b::NNLO_bb_double_conv_e2()
{
if (pole==2)
	{
	double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
	Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
	}
else
	{book_production_event();}
}

void b_b_to_H_b_b::NNLO_bb_double_conv_e1()
{
if (pole==1)
	{
	double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
	Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
	}
else
	{book_production_event();}
}


void b_b_to_H_b_b::NNLO_RR_Romain()
{
}


void b_b_to_H_b_b::NNLO_RR_Franz()
{	
    int sector_offset = 71;
    int current_topology = sector_control-sector_offset;//: the offset on 
    double epsilon_coeff=4.0;
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
        for (int cur_sector=sector_init[current_topology];cur_sector<=sector_fin[current_topology];cur_sector++)
        {
        
            NNLO_RR_epsilon_expansion(epsilon_coeff,FRTopologies[current_topology],cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"no_gluons");
        }
    }

}






void b_b_to_H_b_b::set_topologies()
{
cout<<"\n["<<__func__<<"]";
cout<<"\tsetting topologies "<<endl;
//: b bbar -> g g H 
FRTopologies[0]=bbbbH1n;sector_init[0]=1;sector_fin[0]=1;     //: bbbbH topology 1 secs 1		sector_control 71
FRTopologies[1]=bbbbH2n;sector_init[1]=1;sector_fin[1]=2;     //: bbbbH topology 2 secs 1-2		sector_control 72
FRTopologies[2]=bbbbH3n;sector_init[2]=1;sector_fin[2]=1;     //: bbbbH topology 3 sec  1		sector_control 73
FRTopologies[3]=bbbbH4n;sector_init[3]=1;sector_fin[3]=2;     //: bbbbH topology 4 sec  1-2     sector_control 74
FRTopologies[4]=bbbbH5n;sector_init[4]=1;sector_fin[4]=2;     //: bbbbH topology 5 sec  1-2		sector_control 75
FRTopologies[5]=bbbbH6n;sector_init[5]=1;sector_fin[5]=1;     //: bbbbH topology 6 sec  1		sector_control 76
FRTopologies[6]=bbbbH7n;sector_init[6]=1;sector_fin[6]=1;     //: bbbbH topology 7 secs 1		sector_control 77
FRTopologies[7]=bbbbH8n;sector_init[7]=1;sector_fin[7]=1;     //: bbbbH topology 8 secs 1		sector_control 78
}




