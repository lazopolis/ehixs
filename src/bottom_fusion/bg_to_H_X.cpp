#include "bg_to_H_X.h"

b_g_to_H_X::b_g_to_H_X(const UserInterface& UI): ExclusiveClass(UI)
{
    if (sector_control==80 or (sector_control>=84 and sector_control<=92))//: bg (gluon at beam 2)
    {
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_00);
    }
    if (sector_control==81 or (sector_control>=93 and sector_control<=101)) //: gb (gluon at beam 1)
    {
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_bbar_00);
    }
    if (sector_control==82) //: convolution F_b x F_bbar_from_g (from g at beam 2)
    {
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_g_11);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_g_11);
    }
    if (sector_control==83) //: convolution F_b x F_bbar_from_g (from g at beam 1)
    {
        lumi.add_pair(Luminosity::F_bbar_from_g_11,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_b_from_g_11,Luminosity::F_bbar_00);
    }
    if (sector_control==102 )//: RV bg (gluon at beam 2)
    {
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_00);
    }
    if (sector_control==103) //: RV gb (gluon at beam 1)
    {
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_bbar_00);
    }
    if (sector_control==104 or sector_control==105) //: convolution f_b x f_bbar_from_g_11  x sigma_bbbar_nlo
    {
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_g_11);
        lumi.add_pair(Luminosity::F_bbar_from_g_11,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_b_from_g_11,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_g_11);
    }
    if (sector_control==106) //: convolution f_b x f_bbar_from_g_22  x sigma_bbbar_lo
    {
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_g_22);
        lumi.add_pair(Luminosity::F_bbar_from_g_22,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_b_from_g_22,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_g_22);
    }
    if (sector_control==107) //: convolution f_b x f_bbar_from_g_21  x sigma_bbbar_lo
    {
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_g_21);
        lumi.add_pair(Luminosity::F_bbar_from_g_21,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_b_from_g_21,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_g_21);
    }
    if (sector_control==108) //: convolution f_b_from_b_11 x f_bbar_from_g_11  x sigma_bbbar_lo
    {
        lumi.add_pair(Luminosity::F_b_from_b_11,Luminosity::F_bbar_from_g_11);
        lumi.add_pair(Luminosity::F_bbar_from_g_11,Luminosity::F_b_from_b_11);
        lumi.add_pair(Luminosity::F_b_from_g_11,Luminosity::F_bbar_from_bbar_11);
        lumi.add_pair(Luminosity::F_bbar_from_bbar_11,Luminosity::F_b_from_g_11);
    }
    if (sector_control==109 )//: convolution f_b_from_b_11 x f_g x sigma_NLO_bg  (gluon at beam 2)
    {
        lumi.add_pair(Luminosity::F_b_from_b_11,Luminosity::F_g_00);
        lumi.add_pair(Luminosity::F_bbar_from_bbar_11,Luminosity::F_g_00);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_g_11);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_g_11);
    }
    if (sector_control==110) //: convolution f_b_from_b_11 x f_g x sigma_NLO_gb  (gluon at beam 1)
    {
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_b_from_b_11);
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_bbar_from_bbar_11);
        lumi.add_pair(Luminosity::F_g_from_g_11,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_g_from_g_11,Luminosity::F_bbar_00);
    }
    if (sector_control==111) //: renormalization counterterm mutiplying sigma_NLO_gb  (gluon at beam 1)
    {
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_g_00,Luminosity::F_bbar_00);
    }
    if (sector_control==112) //: renormalization counterterm mutiplying sigma_NLO_bg  (gluon at beam 2)
    {
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_00);
    }
    //: constructing the cur_lumi and cur_lumi_LO vectors (necessary because lumi assigns to cur_lumi[i] instead of pushing back)
    cur_lumi = vector<double>(lumi.pdf_size(),0.0);
    cur_lumiLO = vector<double>(lumi.pdf_size(),0.0);
    //: setting topologies (Franz's functions and number of sectors per function) for the double real
    set_topologies();
}


//: ------- gluon fusion NLO
void b_g_to_H_X::NLO()
{
#ifdef debug
cout<<"\n["<<__func__<<"]";
#endif
prepare_normalization();
if (sector_control==80)
	{
    //cout<<"\n here ["<<__func__<<"]";
	NLO_HARD_1();
	}
if (sector_control==81) //: convolutions
	{
	NLO_HARD_2(); 
	}
if (sector_control==82) //: convolutions
	{
	NLO_conv_1(); //: NLO convolution (cancels 1/e of NLO)
	}
if (sector_control==83) //: convolutions
	{
	NLO_conv_2(); //: NLO convolution (cancels 1/e of NLO)
	}

}

void b_g_to_H_X::NNLO()
{

    prepare_normalization();
    string nnlo_variant="franz";
    
    if (sector_control==80)
	{
        //cout<<"\n here ["<<__func__<<"]";
        NLO_HARD_1();
	}
    if (sector_control==81) //: convolutions
	{
        NLO_HARD_2(); 
	}
    if (sector_control==82) //: convolutions
	{
        NLO_conv_1(); //: NLO convolution (cancels 1/e of NLO)
	}
    if (sector_control==83) //: convolutions
	{
        NLO_conv_2(); //: NLO convolution (cancels 1/e of NLO)
	}

    if (sector_control>=84 and sector_control<=101)
    {
        NNLO_RR_Franz();
    }
    if (sector_control==102)
    {
        NNLO_RV_bg();
    }
    if (sector_control==103)
    {
        NNLO_RV_gb();
    }
    if (sector_control==104)
    {
        NNLO_conv_bb_nlo_soft();
    }
    if (sector_control==105)
    {
        NNLO_conv_bb_nlo_hard();
    }
    if (sector_control==106)
    {
        NNLO_conv_bb_LO_e2();
    }
    if (sector_control==107)
    {
        NNLO_conv_bb_LO_e1();
    }
    if (sector_control==108)
    {
        NNLO_conv_bb_LO_e2();
    }
    if (sector_control==109)
    {
        NNLO_conv_bg();
    }
    if (sector_control==110)
    {
        NNLO_conv_gb();
    }
    if (sector_control==111)
    {
        NNLO_renorm_gb();
    }
    if (sector_control==112)
    {
        NNLO_renorm_bg();
    }

}

void b_g_to_H_X:: prepare_normalization()
{
//: pref_sbb = pi * Y^2 / ( 2*Nc*mh^2 )
//  0.389379*10^9 is the GeV to pb
pref_sbb = consts::Pi/6.0 * 0.389379e9
						*pow(Model.bottom.Y * yukawa_b_vector[0],2.0)
						/pow(Model.higgs.m,2.0)
						; 
special_normalization_factor_for_double_real=12.0; 
parametrization_for_LO_kinematics(x1LO,x2LO,zLO,measLO);
lumi(x1LO,x2LO,cur_lumiLO);
parametrization_for_NLO_kinematics(x1,x2,z,meas);
lumi(x1,x2,cur_lumi);
lambda = xx_vegas[2];

//: Nij holds various fractional factors that depend on subchannel
//: like the initial state averaging factor
Nij=1.0/96.0; //: averaging factor for the bb->ggH channel


lh=  log_muf_sq_over_mh_sq;
//cout<<"\n*** lh= "<<lh;		
curs=pow(Model.higgs.m,2.0)/z; 

Log_1mz=log(1.0-z);
cursLO=pow(Model.higgs.m,2.0)/zLO;
}

void b_g_to_H_X::NNLO_conv_bb_nlo_soft()
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
    const double plus3_prefactor = plus0_prefactor * pow(log(1.0-z),3.0);
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


void b_g_to_H_X::NNLO_conv_bb_nlo_hard()
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

void b_g_to_H_X::NNLO_conv_bg()//: F_g->b x F_bbar * sigma_bg with gluon from beam 1
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

void b_g_to_H_X::NNLO_conv_gb()//: this is the same convolution as above but with gluon from beam 2
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



void b_g_to_H_X::NLO_HARD_1()//: F_b x F_g * sigma_bg  //: gluon from beam 2
{
    NLO_ME.set(z,lambda,lh);//:passing z,lambda and logs to NLO_ME
    double sigma_central =	pref_sbb*meas*cur_lumi[0]*alpha_s_vector[0]/consts::Pi;
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

void b_g_to_H_X::NLO_HARD_2()//: F_b x F_g * sigma_bg  //: gluon from beam 1
{
    NLO_ME.set(z,lambda,lh);//:passing z,lambda and logs to NLO_ME

    double sigma_central =	pref_sbb*meas*cur_lumi[0]*alpha_s_vector[0]/consts::Pi;
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


void b_g_to_H_X::NNLO_renorm_bg()//: F_b x F_g * sigma_bg  //: gluon from beam 2
{
    NLO_ME.set(z,lambda,lh);//:passing z,lambda and logs to NLO_ME
    double sigma_central =	pref_sbb*meas*cur_lumi[0]
                            *pow(alpha_s_vector[0]/consts::Pi,2.0)
                            //*3.0/consts::Pi
                            //*Nij
                            *(-1.0/2.0)
                            *(19.0/2.0-consts::nf/3.0);
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

void b_g_to_H_X::NNLO_renorm_gb()//: F_b x F_g * sigma_bg  //: gluon from beam 1
{
    NLO_ME.set(z,lambda,lh);//:passing z,lambda and logs to NLO_ME
    
    double sigma_central =	pref_sbb*meas*cur_lumi[0]
                            *pow(alpha_s_vector[0]/consts::Pi,2.0)
                            //*3.0/consts::Pi
                            //*Nij
                            *(-1.0/2.0)
                            *(19.0/2.0-consts::nf/3.0);
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



void b_g_to_H_X::NLO_conv_1()//: F_b x F_g->bar * sigma_bbar  //: gluon from beam 2
{
    double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*alpha_s_vector[0]/consts::Pi;
    if (pole==1)
	{
        Jnlo(sigma_central,x1LO,x2LO,1.0,1.0);
	}
    else if (pole==0)
	{
        
    }
}

void b_g_to_H_X::NLO_conv_2()//: F_b x F_g->bar * sigma_bbar  //: gluon from beam 1
{

    double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*alpha_s_vector[0]/consts::Pi;
    if (pole==1)
	{
        Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
	}
    else if (pole==0)
	{
        
    }
}

void b_g_to_H_X::NNLO_conv_bb_LO_e2()//: F_b x F_g->bar * sigma_bbar  //: gluon from beam 1
{
    
    double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]
                        *pow(alpha_s_vector[0]/consts::Pi,2.0);
    if (pole==2)
	{
        Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
	}
    else if (pole==0)
	{
        
    }
}

void b_g_to_H_X::NNLO_conv_bb_LO_e1()//: F_b x F_g->bar * sigma_bbar  //: gluon from beam 1
{
    
    double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
    if (pole==1)
	{
        Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
	}
}

void b_g_to_H_X::NNLO_RV_bg()//: Real virtual with gluon at beam 2
{
    
    double wd=1.0// Note here: no 1/(1.0-z)
    *cur_lumi[0]
    *pow(alpha_s_vector[0]/consts::Pi,2.0)
    *meas*pref_sbb
    *3.0*(4.0)/consts::Pi
    *Nij;
    double res;
    for (int sector=1;sector<=3;sector++) // A1 has 2 sectors
	{
        if (pole==0)
		{
            RVbggH(sector,0,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
            RVbggH(sector,-1,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
            RVbggH(sector,-2,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
            RVbggH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
		}
        if (pole==1)
		{
            RVbggH(sector,-1,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
            RVbggH(sector,-2,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
            RVbggH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
		}
        if (pole==2)
		{
            RVbggH(sector,-2,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
            RVbggH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
		}
        if (pole==3)
		{
            RVbggH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
		}
        if (pole==4)
		{
            book_production_event();
		}
	}
}

void b_g_to_H_X::NNLO_RV_gb()//: Real virtual with gluon at beam 1
{
    
    double wd=1.0// Note here: no 1/(1.0-z)
    *cur_lumi[0]
    *pow(alpha_s_vector[0]/consts::Pi,2.0)
    *meas*pref_sbb
    *3.0*(4.0)/consts::Pi
    *Nij;
    double res;
    for (int sector=1;sector<=3;sector++) // A1 has 2 sectors
	{
        if (pole==0)
		{
            RVgbgH(sector,0,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
            RVgbgH(sector,-1,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
            RVgbgH(sector,-2,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
            RVgbgH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
		}
        if (pole==1)
		{
            RVgbgH(sector,-1,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
            RVgbgH(sector,-2,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
            RVgbgH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
		}
        if (pole==2)
		{
            RVgbgH(sector,-2,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
            RVgbgH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
		}
        if (pole==3)
		{
            RVgbgH(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
		}
        if (pole==4)
		{
            book_production_event();
		}
	}
}

void b_g_to_H_X::NNLO_RR_Franz()
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
            *(-1.0)//: because Franz's routines are coming from crossing the g<->b and mis a minus sign
            *Nij;

        int sector_offset = 84;
        int current_topology = sector_control-sector_offset;//: the offset on 
        double epsilon_coeff=4.0;
        for (int cur_sector=sector_init[current_topology];cur_sector<=sector_fin[current_topology];cur_sector++)
        {
            NNLO_RR_epsilon_expansion(epsilon_coeff,FRTopologies[current_topology],cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"one_gluon");
        }
    }
}



void b_g_to_H_X::set_topologies()
{
cout<<"\n["<<__func__<<"]";
cout<<"\tsetting topologies "<<endl;
//: b g -> b g H 
FRTopologies[0]=bggbH1n;sector_init[0]=1;sector_fin[0]=1;     //: bbggH topology 1 secs 1-2		sector_control 2
FRTopologies[1]=bggbH2n;sector_init[1]=1;sector_fin[1]=2;     //: bbggH topology 2 secs 1-8		sector_control 3
FRTopologies[2]=bggbH3n;sector_init[2]=1;sector_fin[2]=2;     //: bbggH topology 3 sec  1		sector_control 4
FRTopologies[3]=bggbH4n;sector_init[3]=1;sector_fin[3]=2;     //: bbggH topology 3 sec  2		sector_control 5
FRTopologies[4]=bggbH5n;sector_init[4]=1;sector_fin[4]=4;     //: bbggH topology 4 sec  1		sector_control 6
FRTopologies[5]=bggbH6n;sector_init[5]=1;sector_fin[5]=2;     //: bbggH topology 4 sec  2		sector_control 7
FRTopologies[6]=bggbH7n;sector_init[6]=1;sector_fin[6]=1;     //: bbggH topology 5 secs 1-4		sector_control 8
FRTopologies[7]=bggbH8n;sector_init[7]=1;sector_fin[7]=1;     //: bbggH topology 6 secs 1-2		sector_control 9
FRTopologies[8]=bggbH9n;sector_init[8]=1;sector_fin[8]=1;     //: bbggH topology 7 sec  1		sector_control 10

FRTopologies[9]=gbgbH1n;sector_init[9]=1;sector_fin[9]=1;     //: bbggH topology 1 secs 1-2		sector_control 2
FRTopologies[10]=gbgbH2n;sector_init[10]=1;sector_fin[10]=2;     //: bbggH topology 2 secs 1-8		sector_control 3
FRTopologies[11]=gbgbH3n;sector_init[11]=1;sector_fin[11]=2;     //: bbggH topology 3 sec  1		sector_control 4
FRTopologies[12]=gbgbH4n;sector_init[12]=1;sector_fin[12]=2;     //: bbggH topology 3 sec  2		sector_control 5
FRTopologies[13]=gbgbH5n;sector_init[13]=1;sector_fin[13]=4;     //: bbggH topology 4 sec  1		sector_control 6
FRTopologies[14]=gbgbH6n;sector_init[14]=1;sector_fin[14]=2;     //: bbggH topology 4 sec  2		sector_control 7
FRTopologies[15]=gbgbH7n;sector_init[15]=1;sector_fin[15]=1;     //: bbggH topology 5 secs 1-4		sector_control 8
FRTopologies[16]=gbgbH8n;sector_init[16]=1;sector_fin[16]=1;     //: bbggH topology 6 secs 1-2		sector_control 9
FRTopologies[17]=gbgbH9n;sector_init[17]=1;sector_fin[17]=1;     //: bbggH topology 7 sec  1		sector_control 10

}







