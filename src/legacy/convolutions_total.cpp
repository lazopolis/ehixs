#include "convolutions_total.h"


convolution_total::convolution_total(const UserInterface& UI): ExclusiveClass(UI)
{
    if (sector_control==130)//: nlo convolution: a_s/e * f_b_11 * fbbar_00 * sigma_LO
	{
        lumi.add_pair(Luminosity::F_b_11,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_bbar_11,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_11);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_11);
        pointer_to_function_for_sector = &convolution_total::sigma_bb_LO_pole_1_NLO;
	}
    else if (sector_control==131) //: nnlo convolution: a_s^2/e^2 * f_b_22 * fbbar_00 * sigma_LO
	{
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_22);
        lumi.add_pair(Luminosity::F_bbar_22,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_b_22,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_22);
        pointer_to_function_for_sector = &convolution_total::sigma_bb_LO_pole_2;
	}
    else if (sector_control==132) //: nnlo convolution: a_s^2/e * f_b_21 * fbbar_00 * sigma_LO
	{
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_21);
        lumi.add_pair(Luminosity::F_bbar_21,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_b_21,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_21);
        pointer_to_function_for_sector = &convolution_total::sigma_bb_LO_pole_1;
	}
    else if (sector_control==133) //: nnlo convolution: a_s^2/e^2 * f_b_11 * fbbar_11 * sigma_LO
	{
        lumi.add_pair(Luminosity::F_b_11,Luminosity::F_bbar_11);
        lumi.add_pair(Luminosity::F_bbar_11,Luminosity::F_b_11);
        pointer_to_function_for_sector = &convolution_total::sigma_bb_LO_pole_2;
        
	}
    else if (sector_control==134) //: convolutions involving a_s^2/e * f_b_11 * f_bbbar_00 * (sigma_bb_NLO/e+...)
	{
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_11);
        lumi.add_pair(Luminosity::F_bbar_11,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_b_11,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_11);
        pointer_to_function_for_sector = &convolution_total::sigma_bb_NLO;
    }
    else if (sector_control==135) //: convolutions involving a_s^2/e * f_b_11 * f_g_00 * (sigma_bg_NLO/e+...) gluon at beam 2
	{
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_11);
        lumi.add_pair(Luminosity::F_bbar_11,Luminosity::F_g_00);
        lumi.add_pair(Luminosity::F_b_11,Luminosity::F_g_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_11);
        pointer_to_function_for_sector = &convolution_total::sigma_bg_LO;
    }    
    else if (sector_control==136) //: convolutions involving a_s^2/e * f_b_11 * f_g_00 * (sigma_gb_NLO/e+...)gluon at beam 1
	{
        lumi.add_pair(Luminosity::F_g_11,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_bbar_11);
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_b_11);
        lumi.add_pair(Luminosity::F_g_11,Luminosity::F_bbar_00);
        pointer_to_function_for_sector = &convolution_total::sigma_gb_LO;
    }
    else if (sector_control==137) //: log(mu_r/mu_f) terms for LO and NLO bbbar
	{
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_00);
        pointer_to_function_for_sector = &convolution_total::mur_over_muf_terms_bb;
    }
    else if (sector_control==138) //: log(mu_r/mu_f) terms for  NLO bg
	{
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_00);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_00);
        pointer_to_function_for_sector = &convolution_total::mur_over_muf_terms_bg;
    }
    else if (sector_control==139) //: log(mu_r/mu_f) terms for NLO gb
	{
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_bbar_00);
        pointer_to_function_for_sector = &convolution_total::mur_over_muf_terms_gb;
    }
    //: constructing the cur_lumi and cur_lumi_LO vectors (necessary because lumi assigns to cur_lumi[i] instead of pushing back)
    cur_lumi = vector<double>(lumi.pdf_size(),0.0);
    cur_lumiLO = vector<double>(lumi.pdf_size(),0.0);

}

void convolution_total::NLO()
{

    prepare_normalization();

	mur_over_muf_terms_bb_nlo_only();
}


void convolution_total::NNLO()
{
    prepare_normalization();
    (this->*pointer_to_function_for_sector)();
}


void convolution_total::mur_over_muf_terms_bb_nlo_only()
{
    // LO proportional pieces
    const double sigma_central =	pref_sbb*measLO*cur_lumiLO[0];
    
    const double L=log_mur_sq_over_muf_sq;
    //: gamma_0, gamma_1, beta_0
    const double g_0 = 1.0;
    
    const double renorm_factor = alpha_s_vector[0]/consts::Pi * 2.0*g_0*L ;//: this is the term that used to be in the NLO ME's
    Jnlo(sigma_central*renorm_factor,x1LO,x2LO,1.0,0.0);
    
}



void convolution_total::mur_over_muf_terms_bb()
{
    // LO proportional pieces
    const double sigma_central =	pref_sbb*measLO*cur_lumiLO[0];
    
    const double L=log_mur_sq_over_muf_sq;
    const double L_sq = L*L;
    //: gamma_0, gamma_1, beta_0
    const double g_0 = 1.0;
    const double g_1 = 101.0/24.0-5.0*consts::nf/36;
    const double b_0 = 11.0/4.0-consts::nf/6.0;
    
    const double renorm_factor = alpha_s_vector[0]/consts::Pi * 2.0*g_0*L //: this is the term that used to be in the NLO ME's
                                +pow(alpha_s_vector[0]/consts::Pi,2.0)     
                                        * (
                                           (2.0*g_0*g_0+g_0*b_0)*L_sq
                                           +2.0*g_1*L
                                           );
    Jnlo(sigma_central*renorm_factor,x1LO,x2LO,1.0,0.0);
    
    //: NLO pieces
    
    NLO_ME.set(z,lambda,lh);//:passing z,lambda and logs to NLO_ME
    
    const double renorm_factor_for_NLO = (2.0*g_0+b_0)*L;
    
    //: delta term prefactor
    const double delta_prefactor = pref_sbb
                *pow(alpha_s_vector[0]/consts::Pi,2.0)
                *cur_lumiLO[0]*measLO
                *renorm_factor_for_NLO;
    //: plus distribution term prefactors. Note that they are all multiplied with a soft jet function (J(z=1))
    const double plus0_prefactor = pref_sbb
                *pow(alpha_s_vector[0]/consts::Pi,2.0)
                *(cur_lumi[0]*meas-cur_lumiLO[0]*measLO)/(1.0-z)
                *renorm_factor_for_NLO;
    const double plus1_prefactor = plus0_prefactor * log(1.0-z);
    if (pole==1)
    {
        Jnlo(  NLO_ME.bb_soft_pole(delta_prefactor,plus0_prefactor),x1LO,x2LO,zLO,0.0);
    }
    if (pole==0)
    {
        Jnlo(  NLO_ME.bb_soft_finite(delta_prefactor,plus0_prefactor,plus1_prefactor),x1LO,x2LO,zLO,0.0);
    }
    
	
    lambda = xx_vegas[2]; 
    
    //:  prefactor for jet
    const double prefactor = pref_sbb
                            *pow(alpha_s_vector[0]/consts::Pi,2.0)
                            *cur_lumi[0]*meas
                            *renorm_factor_for_NLO;
    if (pole==1)
    {
        Jnlo(  prefactor*NLO_ME.bb_coll_pole(),x1,x2,z,1.0);
        Jnlo(  prefactor*NLO_ME.bb_coll_pole(),x1,x2,z,0.0);
        Jnlo(  prefactor*NLO_ME.bb_coll_soft_pole(),x1LO,x2LO,zLO,0.0);
    }
    if (pole==0)
    {
        Jnlo(  prefactor*NLO_ME.bb_coll_fin(),x1,x2,z,1.0);
        Jnlo(  prefactor*NLO_ME.bb_coll_fin(),x1,x2,z,0.0);
        Jnlo(  prefactor*NLO_ME.bb_coll_soft_fin(),x1LO,x2LO,zLO,0.0);
        Jnlo(  prefactor*NLO_ME.bb_hard_fin(),x1,x2,z,lambda);
        Jnlo(  -lambda* prefactor*NLO_ME.bb_hard_fin(),x1,x2,z,1.0);
        Jnlo(  -(1.0-lambda)*prefactor*NLO_ME.bb_hard_fin(),x1,x2,z,0.0);
    }
    
    
    
}

void convolution_total::mur_over_muf_terms_bg()
{
    NLO_ME.set(z,lambda,lh);//:passing z,lambda and logs to NLO_ME
    
    const double L=log_mur_sq_over_muf_sq;
    //: gamma_0, gamma_1, beta_0
    const double g_0 = 1.0;
    const double b_0 = 11.0/4.0-consts::nf/6.0;
    
    const double renorm_factor_for_NLO = (2.0*g_0+b_0)*L;
    
    
    double sigma_central =	pref_sbb
                            *meas
                            *cur_lumi[0]
                            *pow(alpha_s_vector[0]/consts::Pi,2.0)
                            *renorm_factor_for_NLO;
    if (pole==1)
	{
        Jnlo(sigma_central*NLO_ME.bg_coll_pole(),x1,x2,z,1.0);
	}
    else if (pole==0)
	{
        Jnlo(sigma_central*NLO_ME.bg_coll_fin(),x1,x2,z,1.0);
        Jnlo(sigma_central*NLO_ME.bg_hard_fin(),x1,x2,z,lambda);
    }
    

}

void convolution_total::mur_over_muf_terms_gb()
{
    NLO_ME.set(z,lambda,lh);//:passing z,lambda and logs to NLO_ME
    
    const double L=log_mur_sq_over_muf_sq;
    //: gamma_0, gamma_1, beta_0
    const double g_0 = 1.0;
    const double b_0 = 11.0/4.0-consts::nf/6.0;
    
    const double renorm_factor_for_NLO = (2.0*g_0+b_0)*L;
    
    
    double sigma_central =	pref_sbb
                            *meas
                            *cur_lumi[0]
                            *pow(alpha_s_vector[0]/consts::Pi,2.0)
                            *renorm_factor_for_NLO;
    
    if (pole==1)
	{
        Jnlo(sigma_central*NLO_ME.gb_coll_pole(),x1,x2,z,0.0);
	}
    else if (pole==0)
	{
        Jnlo(sigma_central*NLO_ME.gb_coll_fin(),x1,x2,z,0.0);
        Jnlo(sigma_central*NLO_ME.gb_hard_fin(),x1,x2,z,lambda);
    }
    
    
}

void convolution_total:: prepare_normalization()
{
    //: pref_sbb = pi * Y^2 / ( 2*Nc*mh^2 )
    //  0.389379*10^9 is the GeV to pb
    pref_sbb = consts::Pi/6.0 * 0.389379e9
    *pow(Model.bottom.Y * yukawa_b_vector[0],2.0)
    /pow(Model.higgs.m,2.0)
    ; 
    //special_normalization_factor_for_double_real=12.0; 
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


void convolution_total::sigma_gb_LO()//: F_g->b x F_bb * sigma_gb with gluon from beam 1
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

void convolution_total::sigma_bg_LO()//: F_g->b x F_bb * sigma_bg with gluon from beam 2 
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

void convolution_total::sigma_bb_LO_pole_1_NLO()
{
    
    if (pole==1)
	{
        double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*alpha_s_vector[0]/consts::Pi;
        Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
    }
    else
	{book_production_event();}
    
}

void convolution_total::sigma_bb_LO_pole_1()
{
    if (pole==1)
	{
        double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
        Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
    }
    else
	{book_production_event();}
}


void convolution_total::sigma_bb_LO_pole_2()
{
    if (pole==2)
	{
        double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
        Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
	}
    else
	{book_production_event();}
}

void convolution_total::sigma_bb_NLO()
{
    NNLO_conv_bb2gH_soft();
    NNLO_conv_bb2gH_hard();
}

void convolution_total::NNLO_conv_bb2gH_soft()
{

    NLO_ME.set(z,lambda,lh);//:passing z,lambda and logs to NLO_ME
    
    //: delta term prefactor
    const double delta_prefactor = pref_sbb
    *pow(alpha_s_vector[0]/consts::Pi,2.0)
    *cur_lumiLO[0]*measLO;
    //: plus distribution term prefactors. Note that they are all multiplied with a soft jet function (J(z=1))
    const double plus0_prefactor = pref_sbb
    *pow(alpha_s_vector[0]/consts::Pi,2.0)
    *(cur_lumi[0]*meas-cur_lumiLO[0]*measLO)/(1.0-z);
    const double plus1_prefactor = plus0_prefactor * log(1.0-z);
    const double plus2_prefactor = plus0_prefactor * pow(log(1.0-z),2.0);
    if (pole==2)
    {
        Jnlo(  NLO_ME.bb_soft_pole(delta_prefactor,plus0_prefactor),x1LO,x2LO,zLO,0.0);
    }
    if (pole==1)
    {
        Jnlo(  NLO_ME.bb_soft_finite(delta_prefactor,plus0_prefactor,plus1_prefactor),x1LO,x2LO,zLO,0.0);
    }
    if (pole==0)
    {
        Jnlo(  NLO_ME.bb_soft_e(delta_prefactor,plus0_prefactor,plus1_prefactor,plus2_prefactor),x1LO,x2LO,zLO,0.0);
    }
    
}


void convolution_total::NNLO_conv_bb2gH_hard()
{
    lambda = xx_vegas[2]; 
    NLO_ME.set(z,lambda,lh);//:passing z,lambda and logs to NLO_ME
    
    //:  prefactor for jet
    const double prefactor = pref_sbb
                            *pow(alpha_s_vector[0]/consts::Pi,2.0)
                            *cur_lumi[0]*meas;
    if (pole==2)
    {
        Jnlo(  prefactor*NLO_ME.bb_coll_pole(),x1,x2,z,1.0);
        Jnlo(  prefactor*NLO_ME.bb_coll_pole(),x1,x2,z,0.0);
        Jnlo(  prefactor*NLO_ME.bb_coll_soft_pole(),x1LO,x2LO,zLO,0.0);
    }
    if (pole==1)
    {
        Jnlo(  prefactor*NLO_ME.bb_coll_fin(),x1,x2,z,1.0);
        Jnlo(  prefactor*NLO_ME.bb_coll_fin(),x1,x2,z,0.0);
        Jnlo(  prefactor*NLO_ME.bb_coll_soft_fin(),x1LO,x2LO,zLO,0.0);
        Jnlo(  prefactor*NLO_ME.bb_hard_fin(),x1,x2,z,lambda);
        Jnlo(  -lambda* prefactor*NLO_ME.bb_hard_fin(),x1,x2,z,1.0);
        Jnlo(  -(1.0-lambda)*prefactor*NLO_ME.bb_hard_fin(),x1,x2,z,0.0);
    }
    if (pole==0)
    {
        Jnlo(  prefactor*NLO_ME.bb_coll_e(),x1,x2,z,1.0);
        Jnlo(  prefactor*NLO_ME.bb_coll_e(),x1,x2,z,0.0);
        Jnlo(  prefactor*NLO_ME.bb_coll_soft_e(),x1LO,x2LO,zLO,0.0);
        Jnlo(  prefactor*NLO_ME.bb_hard_e(),x1,x2,z,lambda);
        Jnlo(  -lambda* prefactor*NLO_ME.bb_hard_e(),x1,x2,z,1.0);
        Jnlo(  -(1.0-lambda)*prefactor*NLO_ME.bb_hard_e(),x1,x2,z,0.0);
    }
    
}













