#include "b_bbar_to_H_X.h"


b_bbar_to_H_X::b_bbar_to_H_X(const UserInterface& UI): ExclusiveClass(UI)
{
    if (sector_control>=0 and sector_control<=39)
	{
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_00);
	}
    else if (sector_control==40 or sector_control==47 or sector_control==48) //: convolutions involving f_b_0 * f_bb^1
	{
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_bbar_11);
        lumi.add_pair(Luminosity::F_bbar_from_bbar_11,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_b_from_b_11,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_b_11);
	}
    else if (sector_control==42) //: convolutions involving f_b_1 * f_bb^1
	{
        lumi.add_pair(Luminosity::F_b_from_b_11,Luminosity::F_bbar_from_bbar_11);
        lumi.add_pair(Luminosity::F_bbar_from_bbar_11,Luminosity::F_b_from_b_11);
	}
    else if (sector_control==43) //: convolutions involving f_b_22 * f_bb^0
	{
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_bbar_22);
        lumi.add_pair(Luminosity::F_bbar_from_bbar_22,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_b_from_b_22,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_b_22);
	}
    else if (sector_control==44) //: convolutions involving f_b_21 * f_bb^0
	{
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_bbar_from_bbar_21);
        lumi.add_pair(Luminosity::F_bbar_from_bbar_21,Luminosity::F_b_00);
        lumi.add_pair(Luminosity::F_b_from_b_21,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_b_from_b_21);
    }
    else if (sector_control==45) //: convolutions involving f_g_from_b_11 * f_b^0 (gluon at beam 1)
	{
        lumi.add_pair(Luminosity::F_g_from_b_11,Luminosity::F_bbar_00);
        lumi.add_pair(Luminosity::F_g_from_bbar_11,Luminosity::F_b_00);
	}
    else if (sector_control==46) //: convolutions involving f_g_from_b_11 * f_bb^0 (gluon at beam 2)
	{
        lumi.add_pair(Luminosity::F_bbar_00,Luminosity::F_g_from_b_11);
        lumi.add_pair(Luminosity::F_b_00,Luminosity::F_g_from_bbar_11);
	}
    //: constructing the cur_lumi and cur_lumi_LO vectors (necessary because lumi assigns to cur_lumi[i] instead of pushing back)
    cur_lumi = vector<double>(lumi.pdf_size(),0.0);
    cur_lumiLO = vector<double>(lumi.pdf_size(),0.0);
    //: setting topologies (Franz's functions and number of sectors per function) for the double real
    set_topologies();
}


//: ------- bbar LO 
void b_bbar_to_H_X::LO()
{
prepare_normalization();
// initial state channel forking
if (sector_control==1)
	{
	bbdeltaLO();
	}
}
/*
template<void (b_bbar_to_H_X::&F)(), void (b_bbar_to_H_X::&G)()>
inline void binder_f()
{
    F();
    G();
}
*/

//: ------- gluon fusion NLO
void b_bbar_to_H_X::NLO()
{
#ifdef debug
cout<<"\n["<<__func__<<"]";
#endif
prepare_normalization();
if (sector_control==1)
	{
//        &binder_f<bbdeltaLO, NLO_SOF();
	bbdeltaLO();
    NLO_SOFT();
    NLO_HARD();
	}
if (sector_control==40) //: convolutions
	{
	NLO_conv(); //: NLO convolution (cancels 1/e of NLO)
	}
}

void b_bbar_to_H_X::NNLO()
{
//: we are here doing only one sector
#ifdef debug
cout<<"\n["<<__func__<<"]";
#endif
prepare_normalization();
string nnlo_variant="franz";

if (sector_control==0)
	{
        bbdeltaLO();//: LO part of the NNLO calculation (bb->H)
        NLO_SOFT();//: NLO part of the NNLO calculation (bb->Hg)
        NNLO_soft();
	}
if (sector_control==1)
	{
       NLO_HARD();//: NLO part of the NNLO calculation (bb->Hg)
	}    
if (sector_control==40) //: convolutions that involve F_b^0 * F_bb^1
	{
	NLO_conv(); //: NLO convolution (cancels 1/e of NLO)
	}
if (sector_control==47) //: convolutions that involve  F_b^0 * F_bb^1
	{
    NNLO_conv_bb2gH_soft();//: NNLO convolution (f_b^0 * f_bbar^1) * (R_{1/e} + V_{1/e})   (cancels 1/e^2, 1/e of NNLO RV)
	}
if (sector_control==48) //: convolutions that involve  F_b^0 * F_bb^1
	{
    NNLO_conv_bb2gH_hard();//: NNLO convolution (f_b^0 * f_bbar^1) * (R_{1/e} + V_{1/e})   (cancels 1/e^2, 1/e of NNLO RV)
	}
if (sector_control==42) 
	{	
	NNLO_conv_RRa();//: NNLO convolution a: (f_b^1*f_bb^1) * sigma_LO : convolutions from two nlo pdf pieces (cancels 1/e^2, 1/e of NNLO RR)
	}
if (sector_control==43) 
	{
	NNLO_conv_RRb();//: NNLO convolutions b: (f_b^0 * f_bb^2) * sigma_LO : convolutions from one nnlo pdf piece (cancels 1/e^2, 1/e of NNLO RR)
	}
if (sector_control==44) 
	{
	NNLO_conv_RRc();//: NNLO convolutions b: (f_b^0 * f_bb^2_1) * sigma_LO : convolutions from one nnlo pdf piece ( 1/e of NNLO RR)
	}
if (sector_control==45) 
	{
	NNLO_conv_RRd();//: NNLO convolutions b: (f_g->b ^1 * f_bb^0) * sigma_bg_NLO : convolutions from one nlo pdf piece ( 1/e of NNLO RR)
	}
if (sector_control==46) 
	{
	NNLO_conv_RRe();//: NNLO convolutions b: (f_g->b ^1 * f_bb^0) * sigma_bg_NLO : convolutions from one nlo pdf piece ( 1/e of NNLO RR)
	}

if (sector_control>=2 and sector_control<=24)//: RR double real contributions (11 integrals)
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
if (sector_control>=30 and sector_control<=33)//: RV  real-virtual contributions (4 integrals)
	{	
	NNLO_RV_Franz();		
	}
if (sector_control==34)
    {
    bb_renormalization();
    }
}

void b_bbar_to_H_X:: prepare_normalization()
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
Nij=1.0/36.0; //: averaging factor for the bb->ggH channel

//: b bar -> q qbar H  : summing over the final state flavours (nf-1)
if (sector_control>=13 and sector_control<=15) {Nij = Nij*(consts::nf-1.0);}
//: b bbar -> g g H : applying symmetry factor due to identical gluons in the final state
if (sector_control>=2 and sector_control<=12) {Nij = Nij* 1.0/2.0;}


lh=  log_muf_sq_over_mh_sq;
//cout<<"\n*** lh= "<<lh;		
curs=pow(Model.higgs.m,2.0)/z; 

Log_1mz=log(1.0-z);
cursLO=pow(Model.higgs.m,2.0)/zLO;
}


void b_bbar_to_H_X::NLO_conv()
{

if (pole==1)
	{
	double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*alpha_s_vector[0]/consts::Pi;//: note that this term is of order a_s
	Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
	}
}


void b_bbar_to_H_X::bbdeltaLO()
{
if (pole==0)
	{
	double sigma_central =	pref_sbb*measLO*cur_lumiLO[0];
	Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
	}
else
	{book_production_event();}
}



void b_bbar_to_H_X::NNLO_conv_RRa()
{

if (pole==2)
	{
	double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
	Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
	}
else
	{book_production_event();}
}


void b_bbar_to_H_X::NNLO_conv_RRb()
{
if (pole==2)
	{
	double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
	Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
	}
else
	{book_production_event();}
}

void b_bbar_to_H_X::NNLO_conv_RRc()
{
if (pole==1)
	{
	double sigma_central =	pref_sbb*measLO*cur_lumiLO[0]*pow(alpha_s_vector[0]/consts::Pi,2.0);
	Jnlo(sigma_central,x1LO,x2LO,1.0,0.0);
	}
else
	{book_production_event();}
}


void b_bbar_to_H_X::NNLO_conv_RRd()//: F_g->b x F_bbar * sigma_bg with gluon from beam 1
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

void b_bbar_to_H_X::NNLO_conv_RRe()//: this is the same convolution as above but with gluon from beam 2
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

void b_bbar_to_H_X::NNLO_conv_bb2gH_soft()
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


void b_bbar_to_H_X::NNLO_conv_bb2gH_hard()
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



void b_bbar_to_H_X::NLO_SOFT()
{
    NLO_ME.set(z,lambda,lh);//:passing z,lambda and logs to NLO_ME
    //: delta term prefactor
    const double delta_prefactor = pref_sbb
                            *alpha_s_vector[0]/consts::Pi
                            *cur_lumiLO[0]*measLO;
    //: plus distribution term prefactors. Note that they are all multiplied with a soft jet function (J(z=1))
    const double plus0_prefactor = pref_sbb
                            *alpha_s_vector[0]/consts::Pi
                            *(cur_lumi[0]*meas-cur_lumiLO[0]*measLO)/(1.0-z);
    const double plus1_prefactor = plus0_prefactor * log(1.0-z);

    if (pole==1)
    {
        Jnlo(  NLO_ME.bb_soft_pole(delta_prefactor,plus0_prefactor),x1LO,x2LO,zLO,0.0);
    }
    if (pole==0)
    {
        Jnlo(  NLO_ME.bb_soft_finite(delta_prefactor,plus0_prefactor,plus1_prefactor),x1LO,x2LO,zLO,0.0);
    }

}


void b_bbar_to_H_X::NLO_HARD()
{
    NLO_ME.set(z,lambda,lh);//:passing z,lambda and logs to NLO_ME
    lambda = xx_vegas[2];
    //:  prefactor for jet
    const double prefactor = pref_sbb
                            *alpha_s_vector[0]/consts::Pi
                            *cur_lumi[0]*meas;
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



//: The "soft" part of the total cross section is below
//: "soft" here means that it corresponds to soft kinematics, i.e. the jet function is at z=1 (J(1))
//: and the matrix element is at z=1 as well ( f(1) ). This does not imply that it's a z-independent piece:
//: There is a z subtraction on the luminosity function and plus distributions that originate from the expansion
//: of  
//:     ( L(z)-L(1) )                                   f[-3]    f[-2]   f[-1]    
//:     ------------- * exp( - c * e * log(1-z) )  *  ( ----- + ------ + ------ + f[0] )  
//:         1-z                                           e^3     e^2      e
//: where c depends on the particular term of the cross section the contribution comes from (i.e. RR, RV, VV, V^2, etc.)
//:
//: note however that the delta pieces (those coming from the integral  of f(1)*J(1)*L(1)/(1-z)^(1+c*e) ) are 
//: still missing a tau dependent term, since the integration limits are from tau to 1 but the analytic integration
//: performed to get the delta terms assumes limits from 0 to 1
void b_bbar_to_H_X::NNLO_soft()
{
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
    double z3=consts::z3;
    double nf=consts::nf;
    const double pisquare=pow(consts::Pi,2.0);
    const double LH= -lh;
    if (pole==4)
	{
    }
    if (pole==3)
	{
       
	}
    if (pole==2)
	{
        const double delta = -(16.0/27.0)*pisquare-(1.0/6.0)*nf+19.0/4.0;
        const double plus0 = -(2.0/9.0)*nf+9.0;
        const double plus1 = 64.0/9.0;
        Jnlo(  delta*delta_prefactor 
             + plus0*plus0_prefactor 
             + plus1*plus1_prefactor,x1LO,x2LO,zLO,0.0);
	}
    if (pole==1)
	{
        const double delta = ((1.0/36.0+(1.0/27.0)*pisquare)*nf+(32.0/27.0)*LH*pisquare-(23.0/18.0)*pisquare+43.0/24.0-(125.0/9.0)*z3);
        const double plus0 = ((10.0/27.0)*nf+(41.0/27.0)*pisquare-(16.0/3.0)*LH-35.0/9.0);
        const double plus1 = (-32.0/3.0-(128.0/9.0)*LH);
        const double plus2 = -(64.0/3.0);
        Jnlo(  delta*delta_prefactor 
             + plus0*plus0_prefactor 
             + plus1*plus1_prefactor
             + plus2*plus2_prefactor,x1LO,x2LO,zLO,0.0);
	}
    if (pole==0)
	{
        const double delta = (-(5.0/81.0)*pisquare+(2.0/3.0)*z3+2.0/27.0)*nf
                              -(32.0/27.0)*pow(LH,2.0)*pisquare
                              +((4.0/9.0)*pisquare-17.0/3.0+(250.0/9.0)*z3)*LH
                              +(29.0/27.0)*pisquare
                              -(17.0/540.0)*pow(consts::Pi,4.0)
                              -(26.0/3.0)*z3+211.0/18.0;
        
        const double plus0 =(56.0/81.0-(20.0/27.0)*LH+(2.0/9.0)*pow(LH,2.0)-(4.0/27.0)*pisquare)*nf
                            -pow(LH,2.0)
                            +(-(82.0/27.0)*pisquare+70.0/9.0)*LH
                            +(10.0/9.0)*pisquare
                            +(638.0/9.0)*z3
                            -212.0/27.0;
        
        const double plus1 =((8.0/9.0)*LH-40.0/27.0)*nf+68.0/3.0+(128.0/9.0)*pow(LH,2.0)-4.0*LH-12.0*pisquare;
        
        const double plus2 = -4.0+(8.0/9.0)*nf+(128.0/3.0)*LH;
        
        const double plus3 =(896.0/27.0);
        Jnlo(  delta*delta_prefactor 
             + plus0*plus0_prefactor 
             + plus1*plus1_prefactor
             + plus2*plus2_prefactor
             + plus3*plus3_prefactor,x1LO,x2LO,zLO,0.0);
	}
    
}






/*
#define ONCE_ROMAIN
#ifndef ONCE_ROMAIN
#define ONCE_ROMAIN
//:
// Parametrizations
#include "parametrizations.hpp"
// Topology subtraction
#include "subtraction.hpp"
// Definition of all possible topologies
#include "topologies.hpp"
// Definition of the jet function
#include "jet.hpp"
// Creates all the Tii::Call functions you need
#include "input.hpp"
#endif
*/

void b_bbar_to_H_X::bb_renormalization()
{
double nf=consts::nf;
double special_norm_factor_for_bb_renorm = 2.0;
double wd=1.0/(1.0-z)
		*cur_lumi[0]
		*pow(alpha_s_vector[0]/consts::Pi,2.0)
		*meas*pref_sbb
		*3.0/consts::Pi
		*special_norm_factor_for_bb_renorm
		*Nij;
for (int cur_sector=1;cur_sector<3;cur_sector++)
	{
	double epsilon_coeff=2.0;
	NNLO_RR_epsilon_expansion_nf(epsilon_coeff,RVbbbarggHrenorm,cur_sector,z,lh,wd,Log_1mz,nf,xx_vegas[2],curs,x1,x2,"no_gluons");
	NNLO_RR_epsilon_expansion_nf(epsilon_coeff,RVbbbarggHrenorm,cur_sector,zLO,lh,-wd,Log_1mz,nf,xx_vegas[2],cursLO,x1LO,x2LO,"no_gluons");
	}

}

void b_bbar_to_H_X::NNLO_RR_Romain()
{
  double wd=cur_lumi[0]
    *pow(alpha_s_vector[0]/consts::Pi, 2.0)
    *meas*pref_sbb
    *special_normalization_factor_for_double_real
    *Nij;

  const double lambda1 = xx_vegas[2];
  const double lambda2 = xx_vegas[3];
  const double lambda3 = xx_vegas[4];
  const double lambda4 = xx_vegas[5];

  for(unsigned i=0; i<RTopologies.size(); ++i)
    (*RTopologies[i])(wd, 1, lambda1, lambda2, lambda3, lambda4, z, curs, x1, x2, lh, pole);
}

void b_bbar_to_H_X::NNLO_RR_Franz()
{	
    int sector_offset = 2;
    int current_topology = sector_control-sector_offset;//: the offset on sector_control : 0 is LO and 1 is NLO
    double epsilon_coeff=4.0;

    for (int cur_sector=sector_init[current_topology];cur_sector<=sector_fin[current_topology];cur_sector++)
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
            NNLO_RR_epsilon_expansion(epsilon_coeff,FRTopologies[current_topology],cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"no_gluons");
            NNLO_RR_epsilon_expansion(epsilon_coeff,FRTopologies[current_topology],cur_sector,zLO,lh,-wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],cursLO,x1LO,x2LO,"no_gluons");
        }
        
	}
}


void b_bbar_to_H_X::NNLO_RV_Franz()
{	


switch(sector_control)
	{
	case 30: NNLO_RV_Franz_A1();break;
	case 31: NNLO_RV_Franz_A2();break;
	case 32: NNLO_RV_Franz_A3();break;
	case 33: NNLO_RV_Franz_A4();break;
	default: cout<<"\n NNLO_RV_Franz called with sector_control different than 30-33 : "<<sector_control<<endl;exit(1);break;
	}
}


void b_bbar_to_H_X::NNLO_RV_Franz_A1()
{
    //: although here the epsilon_coeff should have been 1.0, Franz has probably expanded over (1-z)^(-1-e) in this
    //: function alone, just for the fun of it.
    double wd=1.0// Note here: no 1/(1.0-z)
                *cur_lumi[0]
                *pow(alpha_s_vector[0]/consts::Pi,2.0)
                *meas*pref_sbb
                *3.0*(-4.0)/consts::Pi
                *Nij;
double res;
for (int sector=1;sector<=2;sector++) // A1 has 2 sectors
	{
	if (pole==0)
		{
		RVbbbarggH1n(sector,0,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
		}
	if (pole==1)
		{
		RVbbbarggH1n(sector,-1,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
		}
	if (pole==2)
		{
		RVbbbarggH1n(sector,-2,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
		}
	if (pole==3)
		{
		RVbbbarggH1n(sector,-3,curs,x1,x2,z, -lh, wd, xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],res);
		}
	if (pole==4)
		{
		book_production_event();
		}
	}
   
/*
    for (int cur_sector=1;cur_sector<=2;cur_sector++) //: A2 has 4 sectors
	{
		
		double epsilon_coeff = 0.0;
		NNLO_RR_epsilon_expansion(epsilon_coeff,RVbbbarggH1n,cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"no_gluons");
		
	}
*/
}

void b_bbar_to_H_X::NNLO_RV_Franz_A2()
{
double wd=1.0/(1.0-z)
		*cur_lumi[0]
		*pow(alpha_s_vector[0]/consts::Pi,2.0)
		*meas*pref_sbb
		*3.0*(-4.0)/consts::Pi
		*Nij;

for (int cur_sector=1;cur_sector<=4;cur_sector++) //: A2 has 4 sectors
	{
		
		double epsilon_coeff = 2.0;
		NNLO_RR_epsilon_expansion(epsilon_coeff,RVbbbarggH2n,cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"no_gluons");
		NNLO_RR_epsilon_expansion(epsilon_coeff,RVbbbarggH2n,cur_sector,zLO,lh,-wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],cursLO,x1LO,x2LO,"no_gluons");
	}
    
}

void b_bbar_to_H_X::NNLO_RV_Franz_A3()
{
double wd=1.0/(1.0-z)
		*cur_lumi[0]
		*pow(alpha_s_vector[0]/consts::Pi,2.0)
		*meas*pref_sbb
		*3.0*(-4.0)/consts::Pi
		*Nij;
for (int cur_sector=1;cur_sector<=4;cur_sector++) //: A3 has 4sectors
	{
		double epsilon_coeff = 3.0;
		NNLO_RR_epsilon_expansion(epsilon_coeff,RVbbbarggH3n,cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"no_gluons");
		//: no  1/e^4 pole  - this one is finite
	}
}

void b_bbar_to_H_X::NNLO_RV_Franz_A4()
{
double wd=1.0/(1.0-z)
		*cur_lumi[0]
		*pow(alpha_s_vector[0]/consts::Pi,2.0)
		*meas*pref_sbb
		*3.0*(-4.0)/consts::Pi
		*Nij;

for (int cur_sector=1;cur_sector<=2;cur_sector++) //: A4 has 2 sectors
	{
		double epsilon_coeff = 4.0;
		NNLO_RR_epsilon_expansion(epsilon_coeff,RVbbbarggH4n,cur_sector,z,lh,wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],curs,x1,x2,"no_gluons");
		NNLO_RR_epsilon_expansion(epsilon_coeff,RVbbbarggH4n,cur_sector,zLO,lh,-wd,Log_1mz,xx_vegas[2],xx_vegas[3], xx_vegas[4], xx_vegas[5],cursLO,x1LO,x2LO,"no_gluons");
	}
}


#include "nnlo_double_real/MbbarggH.cpp"
#include "nnlo_double_real/MbbarbbarH.cpp"
#include "nnlo_double_real/MbbarqqbarH.cpp"

void b_bbar_to_H_X::set_topologies()
{
cout<<"\n["<<__func__<<"]";
cout<<"\tsetting topologies "<<endl;
//: b bbar -> g g H 
FRTopologies[0]=bbggH1n;sector_init[0]=1;sector_fin[0]=2;     //: bbggH topology 1 secs 1-2		sector_control 2

FRTopologies[1]=bbggH2n;sector_init[1]=1;sector_fin[1]=8;     //: bbggH topology 2 secs 1-8		sector_control 3
//FRTopologies[1]=bbggH2n;sector_init[1]=1;sector_fin[1]=1;     //: bbggH topology 2 secs 1-8		sector_control 3

FRTopologies[2]=bbggH3n;sector_init[2]=1;sector_fin[2]=1;     //: bbggH topology 3 sec  1		sector_control 4
FRTopologies[3]=bbggH3n;sector_init[3]=2;sector_fin[3]=2;     //: bbggH topology 3 sec  2		sector_control 5
FRTopologies[4]=bbggH4n;sector_init[4]=1;sector_fin[4]=1;     //: bbggH topology 4 sec  1		sector_control 6
FRTopologies[5]=bbggH4n;sector_init[5]=2;sector_fin[5]=2;     //: bbggH topology 4 sec  2		sector_control 7

FRTopologies[6]=bbggH5n;sector_init[6]=1;sector_fin[6]=4;     //: bbggH topology 5 secs 1-4		sector_control 8

FRTopologies[7]=bbggH6n;sector_init[7]=1;sector_fin[7]=2;     //: bbggH topology 6 secs 1-2		sector_control 9
FRTopologies[8]=bbggH7n;sector_init[8]=1;sector_fin[8]=1;     //: bbggH topology 7 sec  1		sector_control 10
FRTopologies[9]=bbggH8n;sector_init[9]=1;sector_fin[9]=1;     //: bbggH topology 8 sec  1		sector_control 11
FRTopologies[10]=bbggH9n;sector_init[10]=1;sector_fin[10]=2;  //: bbggH topology 9 secs 1-2		sector_control 12
//: b bbar -> q qbar H (q!=b)
FRTopologies[11]=bbarqqbarH1n;sector_init[11]=1;sector_fin[11]=2;  //: bbggH topology 9 secs 1-2		sector_control 13
FRTopologies[12]=bbarqqbarH2n;sector_init[12]=1;sector_fin[12]=1;  //: bbggH topology 9 secs 1-2		sector_control 14
FRTopologies[13]=bbarqqbarH3n;sector_init[13]=1;sector_fin[13]=2;  //: bbggH topology 9 secs 1-2		sector_control 15
//: b bbar -> b bbar H
FRTopologies[14]=bbarbbarH1n;sector_init[14]=1;sector_fin[14]=2;  //: bbggH topology 9 secs 1-2		sector_control 16
FRTopologies[15]=bbarbbarH2n;sector_init[15]=1;sector_fin[15]=2;  //: bbggH topology 9 secs 1-2		sector_control 17
FRTopologies[16]=bbarbbarH3n;sector_init[16]=1;sector_fin[16]=2;  //: bbggH topology 9 secs 1-2		sector_control 18
FRTopologies[17]=bbarbbarH4n;sector_init[17]=1;sector_fin[17]=4;  //: bbggH topology 9 secs 1-2		sector_control 19
FRTopologies[18]=bbarbbarH5n;sector_init[18]=1;sector_fin[18]=2;  //: bbggH topology 9 secs 1-2		sector_control 20
FRTopologies[19]=bbarbbarH6n;sector_init[19]=1;sector_fin[19]=1;  //: bbggH topology 9 secs 1-2		sector_control 21
FRTopologies[20]=bbarbbarH7n;sector_init[20]=1;sector_fin[20]=1;  //: bbggH topology 9 secs 1-2		sector_control 22
FRTopologies[21]=bbarbbarH8n;sector_init[21]=1;sector_fin[21]=2;  //: bbggH topology 9 secs 1-2		sector_control 23
FRTopologies[22]=bbarbbarH9n;sector_init[22]=1;sector_fin[22]=1;  //: bbggH topology 9 secs 1-2		sector_control 24

  // Romain's part
  // b bbar -> g g H 
  RTopologies.push_back(&TopologySubtraction<C22b, MbbarggH_1_e0, MbbarggH_1_e1, MbbarggH_1_e2, MbbarggH_1_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<C11a, MbbarggH_2_e0, MbbarggH_2_e1, MbbarggH_2_e2, MbbarggH_2_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<C33c, MbbarggH_3_e0, MbbarggH_3_e1, MbbarggH_3_e2, MbbarggH_3_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<C33b, MbbarggH_4_e0, MbbarggH_4_e1, MbbarggH_4_e2, MbbarggH_4_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<C12a, MbbarggH_5_e0, MbbarggH_5_e1, MbbarggH_5_e2, MbbarggH_5_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<C12b, MbbarggH_6_e0, MbbarggH_6_e1, MbbarggH_6_e2, MbbarggH_6_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ1a, MbbarggH_7_e0, MbbarggH_7_e1, MbbarggH_7_e2, MbbarggH_7_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ1b, MbbarggH_8_e0, MbbarggH_8_e1, MbbarggH_8_e2, MbbarggH_8_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ2a, MbbarggH_9_e0, MbbarggH_9_e1, MbbarggH_9_e2, MbbarggH_9_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ2b, MbbarggH_10_e0, MbbarggH_10_e1, MbbarggH_10_e2, MbbarggH_10_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ3a, MbbarggH_11_e0, MbbarggH_11_e1, MbbarggH_11_e2, MbbarggH_11_e3, J>);
//
//  // b bbar -> b bbar H
//  RTopologies.push_back(&TopologySubtraction<C22b, MbbarbbarH_1_e0, MbbarbbarH_1_e1, MbbarbbarH_1_e2, MbbarbbarH_1_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<C11b, MbbarbbarH_2_e0, MbbarbbarH_2_e1, MbbarbbarH_2_e2, MbbarbbarH_2_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<C33c, MbbarbbarH_3_e0, MbbarbbarH_3_e1, MbbarbbarH_3_e2, MbbarbbarH_3_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<C12a, MbbarbbarH_4_e0, MbbarbbarH_4_e1, MbbarbbarH_4_e2, MbbarbbarH_4_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<C12b, MbbarbbarH_5_e0, MbbarbbarH_5_e1, MbbarbbarH_5_e2, MbbarbbarH_5_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ1c, MbbarbbarH_6_e0, MbbarbbarH_6_e1, MbbarbbarH_6_e2, MbbarbbarH_6_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ1a, MbbarbbarH_7_e0, MbbarbbarH_7_e1, MbbarbbarH_7_e2, MbbarbbarH_7_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ1b, MbbarbbarH_8_e0, MbbarbbarH_8_e1, MbbarbbarH_8_e2, MbbarbbarH_8_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ2a, MbbarbbarH_9_e0, MbbarbbarH_9_e1, MbbarbbarH_9_e2, MbbarbbarH_9_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ2b, MbbarbbarH_10_e0, MbbarbbarH_10_e1, MbbarbbarH_10_e2, MbbarbbarH_10_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ3a, MbbarbbarH_11_e0, MbbarbbarH_11_e1, MbbarbbarH_11_e2, MbbarbbarH_11_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ2c, MbbarbbarH_12_e0, MbbarbbarH_12_e1, MbbarbbarH_12_e2, MbbarbbarH_12_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ2d, MbbarbbarH_13_e0, MbbarbbarH_13_e1, MbbarbbarH_13_e2, MbbarbbarH_13_e3, J>);
//
//  // b bbar -> q qbar H
//  RTopologies.push_back(&TopologySubtraction<C22b, MbbarqqbarH_1_e0, MbbarqqbarH_1_e1, MbbarqqbarH_1_e2, MbbarqqbarH_1_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ1c, MbbarqqbarH_2_e0, MbbarqqbarH_2_e1, MbbarqqbarH_2_e2, MbbarqqbarH_2_e3, J>);
//  RTopologies.push_back(&TopologySubtraction<CQ1a, MbbarqqbarH_3_e0, MbbarqqbarH_3_e1, MbbarqqbarH_3_e2, MbbarqqbarH_3_e3, J>);
}





