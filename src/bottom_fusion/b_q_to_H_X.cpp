#include "b_q_to_H_X.h"

typedef void (*pointer_to_Franz)(const int&, const int&, const double&,const double&,const double&,
                                 const double&, const double&, const double&, const double&, const double&, const double&);

b_q_to_H_X::b_q_to_H_X(const UserInterface& UI): ExclusiveClass(UI)
{
    const string nnlo_variant="franz";
    if (sector_control>=114 and sector_control<=119) //: RR qb->qbH
    {
        lumi.add_pair(Luminosity::F_u_00,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_d_00,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_s_00,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_c_00,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_ubar_00,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_dbar_00,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_sbar_00,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_cbar_00,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_u_00,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_d_00,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_s_00,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_c_00,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_ubar_00,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_dbar_00,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_sbar_00,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_cbar_00,Luminosity::F_bbar_00);
        if (nnlo_variant=="franz")
		{
            pointer_to_function_for_sector = &b_q_to_H_X::NNLO_RR_Franz;
		}
        else
		{
            pointer_to_function_for_sector = &b_q_to_H_X::NNLO_RR_Romain;
		}
    }
    if (sector_control>=120 and sector_control<=125)//: RR bq->qbH
    {
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_u_00);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_d_00);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_c_00);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_s_00);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_ubar_00);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_dbar_00);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_cbar_00);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_sbar_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_u_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_d_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_c_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_s_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_ubar_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_dbar_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_cbar_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_sbar_00);
        if (nnlo_variant=="franz")
		{
            pointer_to_function_for_sector = &b_q_to_H_X::NNLO_RR_Franz;
		}
        else
		{
            pointer_to_function_for_sector = &b_q_to_H_X::NNLO_RR_Romain;
		}
    }
    if (sector_control==126) //: bg convolution
    {
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_u_11);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_d_11);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_s_11);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_c_11);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_ubar_11);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_dbar_11);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_sbar_11);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_cbar_11);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_u_11);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_d_11);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_s_11);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_c_11);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_ubar_11);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_dbar_11);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_cbar_11);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_sbar_11);
        
        pointer_to_function_for_sector = &b_q_to_H_X::NNLO_bg_conv;
    }
    if (sector_control==127) //: gb convolution
    {
        lumi.add_pair(Luminosity::F_g_from_u_11,Luminosity::F_b_00);
       
        lumi.add_pair(Luminosity::F_g_from_d_11,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_g_from_s_11,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_g_from_c_11,Luminosity::F_b_00);
        
        lumi.add_pair(Luminosity::F_g_from_ubar_11,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_g_from_dbar_11,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_g_from_sbar_11,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_g_from_cbar_11,Luminosity::F_b_00);
        
        lumi.add_pair(Luminosity::F_g_from_u_11,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_g_from_d_11,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_g_from_s_11,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_g_from_c_11,Luminosity::F_bbar_00);
        
        lumi.add_pair(Luminosity::F_g_from_ubar_11,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_g_from_dbar_11,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_g_from_cbar_11,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_g_from_sbar_11,Luminosity::F_bbar_00);
        
        pointer_to_function_for_sector = &b_q_to_H_X::NNLO_gb_conv;
    }
    if (sector_control==128) //: bb double convolution 1/e^2
    {
        lumi.add_pair(Luminosity::F_b_from_u_22,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_b_from_d_22,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_b_from_s_22,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_b_from_c_22,Luminosity::F_bbar_00);
        
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_u_22);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_d_22);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_s_22);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_c_22);
        
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_u_22);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_d_22);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_s_22);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_c_22);
        
        lumi.add_pair(Luminosity::F_bbar_from_u_22,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_bbar_from_d_22,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_bbar_from_s_22,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_bbar_from_c_22,Luminosity::F_b_00);
        
        lumi.add_pair(Luminosity::F_b_from_ubar_22,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_b_from_dbar_22,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_b_from_sbar_22,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_b_from_cbar_22,Luminosity::F_bbar_00);
        
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_ubar_22);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_dbar_22);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_sbar_22);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_cbar_22);
        
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_ubar_22);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_dbar_22);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_sbar_22);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_cbar_22);
        
        lumi.add_pair(Luminosity::F_bbar_from_ubar_22,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_bbar_from_dbar_22,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_bbar_from_sbar_22,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_bbar_from_cbar_22,Luminosity::F_b_00);
        
        //my_memfunc_ptr = &SomeClass::some_member_func;
        pointer_to_function_for_sector = &b_q_to_H_X::NNLO_bb_double_conv_e2;
    }
    if (sector_control==129) //: bb double convolution 1/e
    {
        lumi.add_pair(Luminosity::F_b_from_u_21,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_b_from_d_21,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_b_from_s_21,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_b_from_c_21,Luminosity::F_bbar_00);
        
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_u_21);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_d_21);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_s_21);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_c_21);
        
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_u_21);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_d_21);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_s_21);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_c_21);
        
        lumi.add_pair(Luminosity::F_bbar_from_u_21,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_bbar_from_d_21,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_bbar_from_s_21,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_bbar_from_c_21,Luminosity::F_b_00);
        
        lumi.add_pair(Luminosity::F_b_from_ubar_21,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_b_from_dbar_21,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_b_from_sbar_21,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_b_from_cbar_21,Luminosity::F_bbar_00);
        
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_ubar_21);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_dbar_21);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_sbar_21);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_cbar_21);
        
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_ubar_21);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_dbar_21);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_sbar_21);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_cbar_21);
        
        lumi.add_pair(Luminosity::F_bbar_from_ubar_21,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_bbar_from_dbar_21,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_bbar_from_sbar_21,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_bbar_from_cbar_21,Luminosity::F_b_00);
        
        pointer_to_function_for_sector = &b_q_to_H_X::NNLO_bb_double_conv_e1;
    }

    //: constructing the cur_lumi and cur_lumi_LO vectors (necessary because lumi assigns to cur_lumi[i] instead of pushing back)
    cur_lumi = vector<double>(lumi.pdf_size(),0.0);
    cur_lumiLO = vector<double>(lumi.pdf_size(),0.0);
    //: setting topologies (Franz's functions and number of sectors per function) for the double real
    set_topologies();
}




void b_q_to_H_X::NNLO()
{
    prepare_normalization();
    string nnlo_variant="franz";
    (this->*pointer_to_function_for_sector)();
}

void b_q_to_H_X:: prepare_normalization()
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
    Nij=1.0/36.0; //: averaging factor for the bq->bqH channel
    //: including the 1/2 symmetry factor due to identical 
    //: final state b-quarks
    
    lh=log_muf_sq_over_mh_sq;		
    curs=pow(Model.higgs.m,2.0)/z; 
    
    Log_1mz=log(1.0-z);
    cursLO=pow(Model.higgs.m,2.0)/zLO;
}


void b_q_to_H_X::NNLO_gb_conv()//: F_g->b x F_bb * sigma_gb with gluon from beam 1
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

void b_q_to_H_X::NNLO_bg_conv()//: F_g->b x F_bb * sigma_bg with gluon from beam 2 
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

void b_q_to_H_X::NNLO_bb_double_conv_e2()
{
    if (pole==2)
	{
        double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
        Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
	}
    else
	{book_production_event();}
}

void b_q_to_H_X::NNLO_bb_double_conv_e1()
{
    if (pole==1)
	{
        //cout<<"\nhere\t"<<cur_lumiLO[0];
        double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
        Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
	}
    else
	{book_production_event();}
}


void b_q_to_H_X::NNLO_RR_Romain()
{
}


void b_q_to_H_X::NNLO_RR_Franz()
{	
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
        int sector_offset = 114;
        int current_topology = sector_control-sector_offset;//: the offset on 
        double epsilon_coeff=4.0;
        for (int cur_sector=sector_init[current_topology];cur_sector<=sector_fin[current_topology];cur_sector++)
        {
        
            NNLO_RR_epsilon_expansion(epsilon_coeff,FRTopologies[current_topology],cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"no_gluons");
        
        }
    }
}






void b_q_to_H_X::set_topologies()
{
    cout<<"\n["<<__func__<<"]";
    cout<<"\tsetting topologies "<<endl;
    FRTopologies[0]=qbqbH1n;sector_init[0]=1;sector_fin[0]=1;     
    FRTopologies[1]=qbqbH2n;sector_init[1]=1;sector_fin[1]=1;     
    FRTopologies[2]=qbqbH3n;sector_init[2]=1;sector_fin[2]=1;     
    FRTopologies[3]=qbqbH4n;sector_init[3]=1;sector_fin[3]=1;     
    FRTopologies[4]=qbqbH5n;sector_init[4]=1;sector_fin[4]=1;     
    FRTopologies[5]=qbqbH6n;sector_init[5]=1;sector_fin[5]=1;
    //: b q -> b q H 
    FRTopologies[6]=bqqbH1n;sector_init[6]=1;sector_fin[6]=1;     
    FRTopologies[7]=bqqbH2n;sector_init[7]=1;sector_fin[7]=1;     
    FRTopologies[8]=bqqbH3n;sector_init[8]=1;sector_fin[8]=1;     
    FRTopologies[9]=bqqbH4n;sector_init[9]=1;sector_fin[9]=1;     
    FRTopologies[10]=bqqbH5n;sector_init[10]=1;sector_fin[10]=1;     
    FRTopologies[11]=bqqbH6n;sector_init[11]=1;sector_fin[11]=1;     
}




