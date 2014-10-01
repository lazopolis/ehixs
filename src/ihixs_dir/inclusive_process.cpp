#include "inclusive_process.h"




double LuminosityIntegralDelta::evaluateIntegral(const double* xx)
{
    const double x1= xx[0];
    const double measure = 1./x1;
    // the check :  x2 = tau/x1 is in [0,1]
    // is done in lumi and 0.0 is returned if it fails
    return measure*lumi_->give(x1,tau_/x1);
}

double LuminosityIntegralPlus::evaluateIntegral(const double* xx)
{
    const double x1= xx[0];
    const double z = xx[1];
    const double measure = 1./x1;
    // the check :  x2 = tau/x1 is in [0,1]
    // is done in lumi and 0.0 is returned if it fails
    const double res=  measure
            *( lumi_->give(x1,tau_/x1/z) - lumi_->give(x1,tau_/x1) )
            / (1.-z)
            * pow(log(1.-z), log_power());
    //cout<<"\nres="<<res<<"\t"<<x1<<" "<<z<<endl;
    return res;
}


InclusiveProcess::InclusiveProcess(const UserInterface& UI)
{
    _lumi = new NewLuminosity(UI);
    const double tau = pow(UI.m_higgs,2.)/pow(UI.Etot,2.);
    D_gg.Configure(_lumi,tau);
    P0_gg.Configure(_lumi,tau);
    P1_gg.Configure(_lumi,tau);
    P2_gg.Configure(_lumi,tau);
    P3_gg.Configure(_lumi,tau);
    P4_gg.Configure(_lumi,tau);
    P5_gg.Configure(_lumi,tau);
    _model.Configure(
                     _lumi->alpha_s_at_mz(),
                     UI.mur_over_mhiggs,
                     UI.perturbative_order,
                     UI.m_higgs
                     );
    // Setting up alpha_s
    _as_pi = _model.alpha_strong()/consts::Pi;
    cout << "\n[CrossSection]: a_s = " << _as_pi * consts::Pi;

}

void InclusiveProcess::Evaluate()
{
    D_gg.call_vegas();
    P0_gg.call_vegas();
    P1_gg.call_vegas();
    P2_gg.call_vegas();
    P3_gg.call_vegas();
    P4_gg.call_vegas();
    P5_gg.call_vegas();
    //: 35.0309 = Gf*pi/sqrt(2)/288 with the Gf in pb
    //: Gf = 1.16637*10^{-5} * 0.389379*10^9
    _prefactor = 35.0309;
    gg_delta_LO = _prefactor * pow(_as_pi,2.)* _eft.n_LO_delta() * D_gg.result();
    gg_delta_NLO =_prefactor * pow(_as_pi,3.)* _eft.n_NLO_delta() * D_gg.result();
    gg_delta_NNLO =_prefactor * pow(_as_pi,4.)* _eft.n_NNLO_delta() * D_gg.result();
    gg_delta_N3LO =_prefactor * pow(_as_pi,5.)* _eft.n_N3LO_delta() * D_gg.result();

}

/*

double DeltaTerm::evaluateIntegral(const double xx[])
{
    generate_initial_state_variables(xx);
    return measure * LL() * matrix_element;
}

void DeltaTerm::generate_initial_state_variables(const double xx[])
{
    x1 = xx[0];
    measure = 1.0/x1;
}

double DeltaTerm::LL()
{
    return lumistack.give(x1,tau/x1);
}

double PlusTerm::evaluateIntegral(const double xx[])
{
    generate_initial_state_variables(xx);
    set_matrix_element();
    return measure * LL() * matrix_element;
}

void PlusTerm::generate_initial_state_variables(const double xx[])
{
    x1 = xx[0];
    z = xx[1];
    measure = 1.0/x1;
}

double PlusTerm::LL()
{
    const double LL_1 = lumistack.give(x1,tau/x1);
    const double LL_z = lumistack.give(x1,tau/x1/z);
    return (LL_z - LL_1)/(1.0-z);
}

double RegularTerm::evaluateIntegral(const double xx[])
{
    generate_initial_state_variables(xx);
    set_matrix_element();
    return measure * LL() * matrix_element;
}

void RegularTerm::generate_initial_state_variables(const double xx[])
{
    x1 = xx[0];
    z = xx[1];
    measure = 1.0/x1;
}

double RegularTerm::LL()
{
    return lumistack.give(x1,tau/x1/z);
}


void LO_delta_gg::set_matrix_element()
{
    matrix_element = 1.0;
}

void NLO_delta_gg::set_matrix_element()
{
    matrix_element = 6.0*consts::z2 ;
}

void NNLO_delta_gg::set_matrix_element()
{
    const double z2=consts::z2;
    const double z3=consts::z3;
    const double z4=consts::z4;

    const double log_piece = 33.0 / 2.0  * L * z2 
                        - 171.0 / 2.0 * L * z3 
                        + 27.0/2.0 * L 
                        - 18.0 * z2 * L ** 2  
                        +(-L * z2 - 11.0/6.0 * L) * consts::nf;
    matrix_element = log_piece
                    + 1071.0 / 8.0 * z4 
                    - 54.0 * z2 ** 2 
                    + 67.0 / 2.0 * z2 
                    - 165.0 / 4.0 * z3 
                    + 837.0/ 16.0 
                    + (- 5.0/3.0 * z2 
                    + 5.0/6.0 * z3 - 247.0 / 36.0) * consts::nf ;
}

void NLO_plus_gg::set_matrix_element()
{
    matrix_element = -6.0*L+12*log(1.0-z);
}

void NNLO_plus_gg::set_matrix_element()
{
    const double DD1 = log(1.0-z);
    const double DD2 = pow(DD1,2.0);
    const double DD3 = pow(DD1,3.0);
    
    const double log_piece = ( 1.0 / 2.0 *pow(L,2.0)
                            + 5.0 / 3.0  * L 
                            - 2.0 * DD1 * L 
                               )*consts::NF 
                            + 45.0  * L * z2 
                            - 67.0 / 2.0  * L 
                            - 33.0 / 2.0  * pow(L,2.0) 
                            + 33.0 * DD1 * L 
                            + 36.0 * DD1 * pow(L,2.0) 
                            - 108.0 * DD2 * L 
    matrix_element = log_piece
                +72.0*DD3
                -33.0*DD2
                +(-90.0*consts::z2+67.0)*DD1
                + 351.0/2.0*consts::z3
                + 33.0*consts::z2
                - 101.0/3.0
                + consts::NF*(
                            2.0*DD2
                            -10.0/3.0*DD1
                            -2.0*consts::z2
                            +14.0/9.0
                            );
    
}

void NLO_reg_gg::set_matrix_element()
{
    const double A = pow(z,3)-pow(z,2)+2.0*z-1.0;
    const double log_piece = (6.0*A)*L/z;
    matrix_element = log_piece
                +6.0*pow(z*z-z+1.0,2)*log(z)/z/(z-1.0) 
                + 11.0/2.0*pow(z-1.0,3)/z 
                - 12.0*A)*log(1-z)/z;


}

void NNLO_reg_gg::set_matrix_element()
{
    const double x=z;
    const double xp = 1.0+z;
    const double xb = 1.0-z;
    const double L = log(z);
    const double LB = log(1.0-z);
    const double LP = log(1.0+z);
    const double Li2x = real(HPL2(0,1,complex<double>(z,0.0))); 
    const double Li2xsq = real(HPL2(0,1,complex<double>(z*z,0.0))); 
    const double Li3x = real(HPL3(0,0,1,complex<double>(z,0.0))); 
    const double Li3xsq = real(HPL3(0,0,1,complex<double>(z*z,0.0))); 
    const double S12xsq = real(HPL3(0,1,1,complex<double>(z*z,0.0))); 
    const double S12x = real(HPL3(0,1,1,complex<double>(z,0.0)));
    const double S12mx = real(HPL3(0,1,1,complex<double>(-z,0.0))); 
    const double z2 = consts::z2; 
    const double z3 = consts::z3;
    
    const double E210 = -(0.17e2 * pow(x, 0.3e1) - 0.3e1 * x * x + 0.12e2 * x - 0.17e2) / x / 0.18e2;
    const double E211 = 0.2e1 / 0.3e1 * x + 0.2e1 / 0.3e1;
    const double E200 = -0.36e2 * (pow(x, 0.3e1) - x * x + 0.2e1 * x - 0.1e1) / x * LB + 0.3e1 / 0.4e1 * (0.99e2 * pow(x, 0.3e1) - 0.83e2 * x * x + 0.94e2 * x - 0.99e2) / x;
    const double E201 = -0.18e2 * (pow(x, 0.4e1) - 0.4e1 * pow(x, 0.3e1) + 0.3e1 * x * x + 0.1e1) / x / xb;
    const double E110 = (0.8e1 / 0.3e1 + 0.8e1 / 0.3e1 * x) * Li2x + 0.2e1 / 0.9e1 * (0.17e2 * pow(x, 0.3e1) - 0.3e1 * x * x + 0.12e2 * x - 0.17e2) / x * LB + (-0.8e1 / 0.3e1 - 0.8e1 / 0.3e1 * x) * z2 - (0.785e3 * pow(x, 0.3e1) - 0.111e3 * x * x + 0.147e3 * x - 0.641e3) / x / 0.108e3;
    const double E111 = (0.25e2 * pow(x, 0.4e1) - 0.61e2 * pow(x, 0.3e1) + 0.21e2 * x * x + 0.7e1 * x + 0.17e2) / x / xb / 0.9e1;
    const double E112 = 0.4e1 / 0.3e1 + 0.4e1 / 0.3e1 * x;
    const double E100 = 0.18e2 * (pow(x, 0.4e1) - 0.6e1 * pow(x, 0.3e1) - 0.13e2 * x * x - 0.6e1 * x + 0.1e1) / x / xp * Li2x - 0.9e1 * pow(x * x + x + 0.1e1, 0.2e1) / x / xp * Li2xsq + 0.108e3 * (pow(x, 0.3e1) - x * x + 0.2e1 * x - 0.1e1) / x * LB * LB - 0.3e1 * (0.110e3 * pow(x, 0.3e1) - 0.116e3 * x * x + 0.127e3 * x - 0.110e3) / x * LB - 0.9e1 * (0.6e1 * pow(x, 0.4e1) - 0.14e2 * pow(x, 0.3e1) - 0.24e2 * x * x - 0.9e1 * x - 0.4e1) / x / xp * z2 + (0.2161e4 * pow(x, 0.3e1) - 0.1513e4 * x * x + 0.1781e4 * x - 0.2161e4) / x / 0.8e1;
    const double E101 = 0.126e3 * pow(x * x - x + 0.1e1, 0.2e1) / xb / x * LB - 0.18e2 * pow(x * x + x + 0.1e1, 0.2e1) / xp / x * LP - 0.3e1 / 0.2e1 * (0.187e3 * pow(x, 0.4e1) - 0.353e3 * pow(x, 0.3e1) + 0.279e3 * x * x - 0.179e3 * x + 0.77e2) / xb / x;
    const double E102 = -0.9e1 * (0.3e1 * pow(x, 0.5e1) - 0.8e1 * pow(x, 0.4e1) - 0.3e1 * pow(x, 0.3e1) + 0.8e1 * x * x + 0.3e1 * x + 0.2e1) / x / xb / xp;
    const double E010 = (x * x - 0.2e1 * x + 0.2e1) / x * LB * Li2x / 0.12e2 - (0.4e1 * pow(x, 0.4e1) + 0.227e3 * pow(x, 0.3e1) + 0.21e2 * x * x - 0.302e3 * x + 0.68e2) / xb / x * Li2x / 0.36e2 - (0.31e2 * x * x + 0.34e2 * x - 0.2e1) / x * Li3x / 0.12e2 + (0.65e2 * x * x + 0.62e2 * x + 0.2e1) / x * S12x / 0.12e2 - 0.2e1 / 0.9e1 * (0.17e2 * pow(x, 0.3e1) - 0.3e1 * x * x + 0.12e2 * x - 0.17e2) / x * LB * LB + (-(x * x - 0.2e1 * x + 0.2e1) / x * z2 / 0.12e2 + (0.1570e4 * pow(x, 0.3e1) - 0.249e3 * x * x + 0.294e3 * x - 0.1282e4) / x / 0.108e3) * LB - (0.132e3 * pow(x, 0.4e1) - 0.387e3 * pow(x, 0.3e1) + 0.99e2 * x * x + 0.70e2 * x + 0.68e2) / xb / x * z2 / 0.36e2 - (0.17e2 * x * x + 0.14e2 * x + 0.2e1) / x * z3 / 0.6e1 - (0.17774e5 * pow(x, 0.3e1) - 0.3801e4 * x * x + 0.2982e4 * x - 0.14939e5) / x / 0.1296e4;
    const double E011 = (0.47e2 * x * x + 0.50e2 * x - 0.2e1) / x * Li2x / 0.12e2 + (x * x - 0.2e1 * x + 0.2e1) / x * LB * LB / 0.24e2 - (0.68e2 * pow(x, 0.4e1) - 0.87e2 * pow(x, 0.3e1) + 0.63e2 * x * x - 0.82e2 * x + 0.68e2) / x / xb * LB / 0.12e2 + (0.2384e4 * pow(x, 0.4e1) + 0.864e3 * pow(x, 0.3e1) * z2 - 0.3041e4 * pow(x, 0.3e1) + 0.333e3 * x * x - 0.864e3 * z2 * x - 0.598e3 * x + 0.1282e4) / x / xb / 0.216e3;
    const double E012 = -(x * x - 0.2e1 * x + 0.2e1) / x * LB / 0.24e2 + (0.132e3 * pow(x, 0.4e1) - 0.351e3 * pow(x, 0.3e1) + 0.117e3 * x * x + 0.52e2 * x + 0.68e2) / x / xb / 0.72e2;
    const double E013 = 0.5e1 / 0.9e1 * x + 0.5e1 / 0.9e1;
    const double E000 = (0.9e1 / 0.2e1 * (0.3e1 * pow(x, 0.4e1) - 0.8e1 * pow(x, 0.3e1) + 0.9e1 * x * x - 0.6e1 * x + 0.3e1) / xb / x * LB - 0.9e1 * (0.4e1 * pow(x, 0.4e1) + 0.8e1 * pow(x, 0.3e1) + 0.21e2 * x * x + 0.14e2 * x + 0.7e1) / xp / x * LP - 0.3e1 / 0.4e1 * (0.121e3 * pow(x, 0.4e1) - 0.300e3 * pow(x, 0.3e1) + 0.87e2 * x * x + 0.398e3 * x - 0.317e3) / xb / x) * Li2x + (0.27e2 / 0.2e1 * (0.3e1 * x * x + 0.2e1 * x + 0.1e1) / xp / x * LP + 0.3e1 / 0.4e1 * (0.11e2 * pow(x, 0.3e1) + 0.21e2 * x * x - 0.12e2 * x - 0.14e2) / x) * Li2xsq + 0.9e1 / 0.2e1 * (0.6e1 * pow(x, 0.5e1) - 0.78e2 * pow(x, 0.4e1) - 0.29e2 * pow(x, 0.3e1) + 0.75e2 * x * x + 0.35e2 * x + 0.27e2) / x / xb / xp * Li3x - 0.9e1 / 0.8e1 * (0.8e1 * pow(x, 0.4e1) + 0.8e1 * pow(x, 0.3e1) - 0.3e1 * x * x - 0.2e1 * x - 0.1e1) / x / xp * Li3xsq - 0.9e1 / 0.2e1 * (0.5e1 * pow(x, 0.5e1) - 0.51e2 * pow(x, 0.4e1) - 0.57e2 * pow(x, 0.3e1) + 0.53e2 * x * x + 0.59e2 * x - 0.11e2) / x / xb / xp * S12x - 0.18e2 * pow(x * x + x + 0.1e1, 0.2e1) / xp / x * S12xsq + 0.9e1 * (0.4e1 * pow(x, 0.4e1) + 0.8e1 * pow(x, 0.3e1) + 0.21e2 * x * x + 0.14e2 * x + 0.7e1) / xp / x * S12mx - 0.72e2 * (pow(x, 0.3e1) - x * x + 0.2e1 * x - 0.1e1) / x * pow(LB, 0.3e1) + 0.3e1 * (0.110e3 * pow(x, 0.3e1) - 0.116e3 * x * x + 0.127e3 * x - 0.110e3) / x * LB * LB + (-0.9e1 / 0.2e1 * (0.23e2 * pow(x, 0.4e1) - 0.48e2 * pow(x, 0.3e1) + 0.69e2 * x * x - 0.66e2 * x + 0.23e2) / xb / x * z2 - (0.2295e4 * pow(x, 0.3e1) - 0.1894e4 * x * x + 0.2159e4 * x - 0.2295e4) / x / 0.4e1) * LB + 0.9e1 / 0.2e1 * (0.8e1 * pow(x, 0.4e1) + 0.16e2 * pow(x, 0.3e1) + 0.33e2 * x * x + 0.22e2 * x + 0.11e2) / xp / x * LP * z2 + 0.3e1 / 0.4e1 * (0.495e3 * pow(x, 0.4e1) - 0.886e3 * pow(x, 0.3e1) + 0.564e3 * x * x - 0.244e3 * x + 0.60e2) / xb / x * z2 + 0.9e1 / 0.2e1 * (0.33e2 * pow(x, 0.5e1) - 0.15e2 * pow(x, 0.4e1) + 0.13e2 * pow(x, 0.3e1) - 0.20e2 * x * x - 0.52e2 * x + 0.3e1) / x / xb / xp * z3 + (0.21165e5 * pow(x, 0.3e1) - 0.19751e5 * x * x + 0.18523e5 * x - 0.18321e5) / x / 0.48e2;
    const double E001 = -0.9e1 / 0.2e1 * (0.11e2 * pow(x, 0.5e1) - 0.65e2 * pow(x, 0.4e1) - 0.25e2 * pow(x, 0.3e1) + 0.59e2 * x * x + 0.31e2 * x + 0.11e2) / x / xb / xp * Li2x + 0.9e1 / 0.4e1 * (0.2e1 * pow(x, 0.4e1) - 0.15e2 * x * x - 0.10e2 * x - 0.5e1) / xp / x * Li2xsq - 0.9e1 / 0.4e1 * (0.59e2 * pow(x, 0.4e1) - 0.116e3 * pow(x, 0.3e1) + 0.177e3 * x * x - 0.118e3 * x + 0.59e2) / xb / x * LB * LB + 0.3e1 / 0.4e1 * (0.605e3 * pow(x, 0.4e1) - 0.1180e4 * pow(x, 0.3e1) + 0.1227e4 * x * x - 0.1238e4 * x + 0.641e3) / xb / x * LB + 0.27e2 / 0.2e1 * (0.3e1 * x * x + 0.2e1 * x + 0.1e1) / xp / x * LP * LP + 0.3e1 / 0.2e1 * (0.11e2 * pow(x, 0.3e1) + 0.21e2 * x * x - 0.12e2 * x - 0.14e2) / x * LP + 0.9e1 * (0.10e2 * pow(x, 0.5e1) - 0.19e2 * pow(x, 0.4e1) - 0.6e1 * pow(x, 0.3e1) + 0.18e2 * x * x + 0.6e1 * x + 0.2e1) / x / xb / xp * z2 - (0.4054e4 * pow(x, 0.4e1) - 0.6427e4 * pow(x, 0.3e1) + 0.5655e4 * x * x - 0.5083e4 * x + 0.2069e4) / xb / x / 0.8e1;
    const double E002 = 0.9e1 * (0.6e1 * pow(x, 0.4e1) - 0.11e2 * pow(x, 0.3e1) + 0.18e2 * x * x - 0.12e2 * x + 0.6e1) / xb / x * LB - 0.9e1 / 0.4e1 * (0.4e1 * pow(x, 0.4e1) + 0.8e1 * pow(x, 0.3e1) + 0.27e2 * x * x + 0.18e2 * x + 0.9e1) / xp / x * LP - 0.3e1 / 0.8e1 * (0.374e3 * pow(x, 0.4e1) - 0.827e3 * pow(x, 0.3e1) + 0.699e3 * x * x - 0.389e3 * x + 0.154e3) / xb / x;
    const double E003 = -0.3e1 / 0.2e1 * (0.9e1 * pow(x, 0.5e1) - 0.17e2 * pow(x, 0.4e1) - 0.7e1 * pow(x, 0.3e1) + 0.18e2 * x * x + 0.7e1 * x + 0.4e1) / x / xb / xp;
    
        
    const double log_piece =  E100*LF
                            + E101*LF*log(x)
                            + E102*LF*log(x)^2
                            + E103*LF*log(x)^3 
                            + E110*LF*NF
                            + E111*LF*NF*log(x)
                            + E112*LF*NF*log(x)^2 
                            + E200*LF*LF
                            + E201*LF*LF*log(x) 
                            + E210*LF*LF*NF
                            + E211*LF*LF*NF*log(x); 
    const double non_log_x_piece = E000 + NF*E010;
    const double log_x_piece = E001*log(x)
                                +E002*log(x)^2
                                +E003*log(x)^3 
                                + E011*NF*log(x)
                                + E012*NF*log(x)^2
                                + E013*NF*log(x)^3 ;
    matrix_element = log_piece + non_log_x_piece + log_x_piece;

}


InclusiveProcess::InclusiveProcess(const UserInterface & UI)
{
    lumi = new Luminosity(UI);
    
    EggLOdelta.lumistack.set(lumi,"gg");
    EggNLOdelta.lumistack.set(lumi,"gg");
    EggN2LOdelta.lumistack.set(lumi,"gg");
    EggNLOplus.lumistack.set(lumi,"gg");
    EggN2LOplus.lumistack.set(lumi,"gg");
    EggNLOregular.lumistack.set(lumi,"gg");
    EggN2LOregular.lumistack.set(lumi,"gg");

}


void InclusiveProcess::perform()
{
    for (int i=0;i<terms.size();i++)
        terms[i]->perform();
    show_results();    
    
}
*/
