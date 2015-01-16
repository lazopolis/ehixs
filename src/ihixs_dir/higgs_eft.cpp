#include "higgs_eft.h"
#include "chaplin.h"
#include <complex>
using namespace std;

namespace HEFT {

    double one_minus_z(const double& z, const double& L)
    {
        return 1.-z;
    }
    double one(const double& z, const double& L)
    {
        return 1.;
    }

    double nFull_minus_nS(const double& z, const double& L)
    {
       return   (gg_n3lo_r_lz5(z,0.0)-gg_n3lo_r_lz5_NS(z,0.0)
                +gg_n3lo_r_lz4(z,0.0)-gg_n3lo_r_lz4_NS(z,0.0)
                +gg_n3lo_r_lz3(z,0.0)-gg_n3lo_r_lz3_NS(z,0.0)
                 );
    }
    
    double qg_nFull_minus_nS(const double& z, const double& L)
    {
        return   (qg_n3lo_r_lz5(z,0.0)-qg_n3lo_r_lz5_NS(z,0.0)
                  +qg_n3lo_r_lz4(z,0.0)-qg_n3lo_r_lz4_NS(z,0.0)
                  +qg_n3lo_r_lz3(z,0.0)-qg_n3lo_r_lz3_NS(z,0.0)
                  );
    }

    
    AsSeries n_delta_at_mh()
    {
        return AsSeries(0,
                        n_LO_delta(),
                        n_NLO_delta(),
                        n_NNLO_delta(),
                        n_N3LO_delta()
                      );
    }
    
    AsSeries n_delta_log_muf(const double& _log_muf_mh_sq)
    {
        return AsSeries(2,
                        n_NNLO_delta_L()*_log_muf_mh_sq
                        + n_NNLO_delta_L2()*pow(_log_muf_mh_sq,2.),
                         n_N3LO_delta_L()*_log_muf_mh_sq
                        + n_N3LO_delta_L2()*pow(_log_muf_mh_sq,2.)
                        + n_N3LO_delta_L3()*pow(_log_muf_mh_sq,3.)
                        );
    }
    
    AsSeries n_D0_at_mh()
    {
        return AsSeries(1,
                        0.,
                        n_NNLO_D0(),
                        n_N3LO_D0()
                        );
    }
    
    AsSeries n_D0_log_muf(const double& _log_muf_mh_sq)
    {
        return AsSeries(1,
                        n_NLO_D0_L() * _log_muf_mh_sq,
                         n_NNLO_D0_L()*_log_muf_mh_sq
                        + n_NNLO_D0_L2()*pow(_log_muf_mh_sq,2.),
                         n_N3LO_D0_L()*_log_muf_mh_sq
                        + n_N3LO_D0_L2()*pow(_log_muf_mh_sq,2.)
                        + n_N3LO_D0_L3()*pow(_log_muf_mh_sq,3.)
                        );
    }
    
    AsSeries n_D1_at_mh()
    {
        return AsSeries(1,
                        n_NLO_D1(),
                        n_NNLO_D1(),
                        n_N3LO_D1()
                        );
    }
    
    AsSeries n_D1_log_muf(const double& _log_muf_mh_sq)
    {
        return AsSeries(1,
                        0.0,
                         n_NNLO_D1_L()*_log_muf_mh_sq
                        + n_NNLO_D1_L2()*pow(_log_muf_mh_sq,2.),
                         n_N3LO_D1_L()*_log_muf_mh_sq
                        + n_N3LO_D1_L2()*pow(_log_muf_mh_sq,2.)
                        + n_N3LO_D1_L3()*pow(_log_muf_mh_sq,3.)
                        );
    }
    
    AsSeries n_D2_at_mh()
    {
        return AsSeries(2,
                        n_NNLO_D2(),
                        n_N3LO_D2()
                        );
    }
    
    AsSeries n_D2_log_muf(const double& _log_muf_mh_sq)
    {
        return AsSeries(2,
                         n_NNLO_D2_L()*_log_muf_mh_sq,
                         n_N3LO_D2_L()*_log_muf_mh_sq
                        + n_N3LO_D2_L2()*pow(_log_muf_mh_sq,2.)
                        + n_N3LO_D2_L3()*pow(_log_muf_mh_sq,3.)
                        );
    }
    
    AsSeries n_D3_at_mh()
    {
        return AsSeries(2,
                        n_NNLO_D3(),
                        n_N3LO_D3()
                      );
    }
    
    AsSeries n_D3_log_muf(const double& _log_muf_mh_sq)
    {
        return AsSeries(2,
                        0.,
                          n_N3LO_D3_L()*_log_muf_mh_sq
                        + n_N3LO_D3_L2()*pow(_log_muf_mh_sq,2.)
                        );
    }
    
    AsSeries n_D4_at_mh()
    {
        return AsSeries(3,
                        n_N3LO_D4());
    }
    
    AsSeries n_D4_log_muf(const double& _log_muf_mh_sq)
    {
        return AsSeries(3,
                        n_N3LO_D4_L()*_log_muf_mh_sq);
    }
    
    AsSeries n_D5_at_mh()
    {
        return AsSeries(3,n_N3LO_D5());
    }
    
    double n_LO_delta(){return 1.0;}
    
    double n_NLO_delta(){return 6.0*consts::z2;}
    
    double n_NNLO_delta(){return
        1071.0 / 8.0 * consts::z4
        - 54.0 * pow(consts::z2,2.0)
        + 67.0/2.0 * consts::z2
        - 165./4. * consts::z3
        + 837./16.
        + ( - 5./3. * consts::z2
           + 5./6.*consts::z3
           - 247./36.)
        * consts::nf;
    }
    double n_NNLO_delta_L(){return
        33./2. * consts::z2
        -171./2.*consts::z3
        +27./2.
        + (-consts::z2 - 11./6.)
        * consts::nf;
    }
    double n_NNLO_delta_L2(){return
        - 18. * consts::z2;
    }
    
    double n_N3LO_delta(){return
        (-35.*pow(consts::nf,2.)*(-103753.0
                                  + 25776.*consts::z2
                                  + 5616.*consts::z3
                                  + 25200.*consts::z4)
         + 105.*consts::nf*(-1128767.
                            + 429096.*consts::z3
                            - 432.*consts::z2*(188. + 981.*consts::z3)
                            + 287100.*consts::z4
                            + 568872.*consts::z5
                            )
         - 27.*(-420.*consts::z2*(16151. + 52866.*consts::z3)
                + 9611910.*consts::z4
                - 105.*(215131.
                        - 265356.*consts::z3
                        + 356832.*pow(consts::z3,2.)
                        - 272844.*consts::z5)
                + 22714020.*consts::z6)
         )/544320.;
    }
    
    
    
    
    
    double n_N3LO_delta_L(){
        return  (1201. / 576.
                 + 5. / 54. * consts::pi_square
                 - 5. / 18. * consts::z3) * pow(consts::nf,2.)
        + (-3. / 8. * consts::pi_square
           + 395. / 6. * consts::z3
           - 29807. / 576.
           - 5. / 96. * pow(consts::pi_square,2.)) * consts::nf
        + 18217. / 64.
        + 1089. / 4. * consts::z3 * consts::pi_square
        - 5049. / 2. * consts::z5
        - 4. * consts::beta_one
        + 6. * consts::beta_two
        - 46. / 3. * consts::pi_square
        + 5. * consts::pi_square * consts::beta_one
        - 9453. / 8. * consts::z3
        + 55. / 64. * pow(consts::pi_square,2.)  ;
    }
    
    double n_N3LO_delta_L2(){
        return (-2. / 9.
                + consts::pi_square / 36.)
        * pow(consts::nf,2.)
        + (81. / 4. * consts::z3
           + 17. / 3.
           - 2. / 3. * consts::beta_one
           + 3. / 4.* consts::pi_square)
        * consts::nf
        - 2673. / 8. * consts::z3
        - 27. / 10. * pow(consts::pi_square,2.)
        - 33.
        - 415. / 16. * consts::pi_square
        + 11. * consts::beta_one ;
    }
    
    double n_N3LO_delta_L3(){
        return -72. * consts::z3
        - 33. / 4. * consts::pi_square
        + consts::nf * consts::pi_square / 2. ;
    }
    
   
  
    
    double n_NLO_D0_L(){return -6.;}
    
    double n_NLO_D1(){return 12.;}
    
    double n_NNLO_D0(){return   - 2.  * consts::z2 * consts::nf
        + 14. / 9. * consts::nf
        + 351. / 2.  * consts::z3
        + 33.  * consts::z2
        - 101. / 3. ;}
    double n_NNLO_D0_L(){return + 5. / 3. * consts::nf
        + 45.  * consts::z2
        - 67./ 2. ;}
    double n_NNLO_D0_L2(){return  1./ 2. * consts::nf - 33. / 4.; }
    
    double n_NNLO_D1(){return   - 10. / 3. * consts::nf
        - 90.  * consts::z2
        + 67. ;
    }
    double n_NNLO_D1_L(){return - 2. * consts::nf + 33.;}
    double n_NNLO_D1_L2(){return + 36.;}
    
    double n_NNLO_D2(){return - 33. + 2. *  consts::nf ;}
    double n_NNLO_D2_L(){return - 108.;}
    double n_NNLO_D3(){return 72.;}
    
    
    // moch and vogt terms
    double n_N3LO_D0(){return ( -58./243.+(10./9.)*consts::z2
                               +(5./9.)*consts::z3)*pow(consts::nf,2.)
        +(
          41579./1296.-(2245./36.)*consts::z2
          -(4427./36.)*consts::z3
          -(59./10.)*pow(consts::z2,2.)
          )*consts::nf
        +(
          -297029./864.+(8563./12.)*consts::z2
          +(8941./4.)*consts::z3
          +(2277./20.)*pow(consts::z2,2.)
          -(6525./2.)*consts::z2*consts::z3
          +5022.*consts::z5
          );
    }
    double n_N3LO_D0_L(){return  (consts::pi_square / 9.
                                  - 25. / 54.
                                  )* pow(consts::nf,2.)
        + (  5345. / 72.
           - 81. *consts::z3
           - 47. / 6. * consts::pi_square
           ) * consts::nf
        - 30569. / 48.
        + 114. * consts::pi_square
        + 1584. *consts::z3
        + 231. / 20. * pow(consts::pi_square,2.)
        ;
    }
    double n_N3LO_D0_L2(){return - 5. / 18. * pow(consts::nf,2.)
        + (   13. / 6.
           - 7. / 4. * consts::pi_square
           ) *consts::nf
        + 945. *consts::z3
        + 231. / 8. * consts::pi_square
        - 161. / 8.
        - 27. *consts::beta_one ;
    }
    double n_N3LO_D0_L3(){return - pow(consts::nf,2.)/ 18.
        + 11. / 6. *consts::nf
        + (pow(12.*consts::Pi,2) - 121.) / 8. ;
    }
    
    double n_N3LO_D1(){return   (25./27.-(4./3.)*consts::z2)*pow(consts::nf,2.)
        +(-5345./36.+162.*consts::z3
          +94.*consts::z2
          )*consts::nf
        +30569./24.
        -1368.*consts::z2
        -3168.*consts::z3
        -(4158./5.)*pow(consts::z2,2.)
        ;}
    double n_N3LO_D1_L(){return   10. / 9. * pow(consts::nf,2.)
        + (-130. / 3.+13. * consts::pi_square) *consts::nf
        + 60. *consts::beta_one
        -429. /2. * consts::pi_square
        -4860. *consts::z3
        +1257. /2.
        ;}
    double n_N3LO_D1_L2(){return 1971. /4.
        + pow(consts::nf,2.) /3.
        -162. * consts::pi_square
        -31. *consts::nf ;}
    double n_N3LO_D1_L3(){return -6. *consts::nf +99. ;}
    
    double n_N3LO_D2(){return   -1051.
        +1683.*consts::z2
        +4887.*consts::z3
        +(469./6.-102.*consts::z2)*consts::nf
        -(10./9.)*pow(consts::nf,2.)
        ;}
    double n_N3LO_D2_L(){return  -2775. /2.
        +378. * consts::pi_square
        -2. /3. * pow(consts::nf,2.)
        +82. *consts::nf
        ;}
    double n_N3LO_D2_L2(){return + (-891. /2. +27. * consts::nf);}
    double n_N3LO_D2_L3(){return-108. ;}
    
    double n_N3LO_D3(){return 925.
        -1512.*consts::z2
        -(164./3.)*consts::nf
        +(4./9.)*pow(consts::nf,2.)
        ;}
    double n_N3LO_D3_L(){return + (660. -40. * consts::nf) ;}
    double n_N3LO_D3_L2(){return + 432. ;}
    
    double n_N3LO_D4(){return  -330.+20.*consts::nf;}
    double n_N3LO_D4_L(){return -540. ;}
    
    double n_N3LO_D5(){return 216.;}
    
    
    
    double nlo_r_lz0(const double& z, const double& L)
    {
        const double zb=1.-z;
        return 1./z * (
                   -(11./2.)*pow(zb,3.)
                   - 6.*pow(z*z-z+1.,2.)*log(z)/zb
                   )
                +
                L * 1./z*6.*(pow(z,3.)-pow(z,2.)+2.*z-1.)
        ;
    }
    
    double nlo_r_lz1(const double& z, const double& L)
    {
        return -1./z * 12.*(pow(z,3.)-z*z+2.*z-1.)*log(1.-z);
    }
    
 
    double qg_nlo_r_lz0(const double& z, const double& L)
    {
        const double zb=1.-z;
        return 1./z * (
                        -1./3.*(pow(zb,2.)+2.*(1.-2*z))
                       -2./3.*(1.+pow(zb,2.))*log(z)
                       -2./3.*(1.+pow(zb,2.))*L
                       )
        ;

    }
    
    double qg_nlo_r_lz1(const double& z, const double& L)
    {
        const double zb=1.-z;
        return 1./z * 4./3. * (1.+pow(zb,2.))*log(zb);
    }
  
    
    double qqb_nlo_r_lz0(const double& z, const double& L)
    {
        const double zb=1.-z;
        return 1./z * 32./27. * pow(zb,3.);
    }

    
    
    
    
    complex<double> operator*(int i,const complex<double>& c){return double(i)*c;}
    complex<double> operator*(const complex<double>& c,int i){return i*c;}
    
    void check_imaginary_part(const complex<double> c, const char* func_name)
    {
        if (abs(imag(c))>1e-6)
        {
            cout<<"\n nnlo regular term found having imaginary part! "
            <<c
            <<" coming from function: "<<func_name
            <<endl;
        }
    }
    
    double nnlo_r_lz0(const double& z, const double& L)
    {
        return    nnlo_r_lz0_const(z,L)
        + nnlo_r_lz0_logz(z,L)
        + nnlo_r_lz0_logz_sq(z,L)
        + nnlo_r_lz0_logz_cube(z,L) ;
    }
    
    double nnlo_r_lz0_const(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        complex<double> res =  (pow(L,2)*pow(Nc,2)*(-99 + 94*z - 83*pow(z,2) + 99*pow(z,3)))/(12.*z) + consts::z2*(-((L*pow(Nc,2)*(-4 - 9*z - 24*pow(z,2) - 14*pow(z,3) + 6*pow(z,4)))/(z*(1 + z))) + (pow(Nc,2)*(-60 + 184*z - 320*pow(z,2) + 322*pow(z,3) + 391*pow(z,4) - 495*pow(z,5) + 6*(-11 - 11*z - 11*pow(z,2) + 17*pow(z,3) + 8*pow(z,4) + 8*pow(z,5))*HPL(-1,z) + 6*(-7 + 127*z + 121*pow(z,2) - 125*pow(z,3) - 119*pow(z,4) + pow(z,5))*HPL(1,z)))/(12.*z*(-1 + pow(z,2)))) - (L*pow(Nc,2)*(144*(-1 + z)*pow(1 + z + pow(z,2),2)*HPL(-1,0,z) + (1 + z)*(-2161 + 3942*z - 3294*pow(z,2) + 3674*pow(z,3) - 2161*pow(z,4) + 288*(3 - 2*z + 9*pow(z,2) - 10*pow(z,3) + 3*pow(z,4))*HPL(0,1,z) + 288*(3 + 2*z + 9*pow(z,2) - 14*pow(z,3) + 3*pow(z,4))*HPL(1,0,z))))/(72.*z*(-1 + pow(z,2))) + (pow(Nc,2)*(18321 - 18523*z + 1430*pow(z,2) - 2642*pow(z,3) - 19751*pow(z,4) + 21165*pow(z,5) + 72*(14 + 12*z - 35*pow(z,2) - 23*pow(z,3) + 21*pow(z,4) + 11*pow(z,5))*HPL(-1,0,z) - 144*(94 - 68*z + 165*pow(z,2) - 75*pow(z,3) - 242*pow(z,4) + 160*pow(z,5))*HPL(0,1,z) - 3132*HPL(1,0,z) + 6012*z*HPL(1,0,z) - 38700*pow(z,2)*HPL(1,0,z) + 20124*pow(z,3)*HPL(1,0,z) + 39780*pow(z,4)*HPL(1,0,z) - 28188*pow(z,5)*HPL(1,0,z) - 1296*HPL(-1,-1,0,z) - 1296*z*HPL(-1,-1,0,z) - 1296*pow(z,2)*HPL(-1,-1,0,z) + 3888*pow(z,3)*HPL(-1,-1,0,z) + 216*HPL(-1,0,0,z) + 216*z*HPL(-1,0,0,z) + 216*pow(z,2)*HPL(-1,0,0,z) - 2376*pow(z,3)*HPL(-1,0,0,z) + 864*pow(z,4)*HPL(-1,0,0,z) + 864*pow(z,5)*HPL(-1,0,0,z) - 1728*HPL(-1,1,0,z) - 1728*z*HPL(-1,1,0,z) - 1728*pow(z,2)*HPL(-1,1,0,z) + 1728*pow(z,3)*HPL(-1,1,0,z) + 1728*pow(z,4)*HPL(-1,1,0,z) + 1728*pow(z,5)*HPL(-1,1,0,z) + 432*pow(z,4)*HPL(0,-1,0,z) - 432*pow(z,5)*HPL(0,-1,0,z) - 5184*HPL(0,0,1,z) - 1728*z*HPL(0,0,1,z) - 12096*pow(z,2)*HPL(0,0,1,z) + 1728*pow(z,3)*HPL(0,0,1,z) + 12096*pow(z,4)*HPL(0,0,1,z) - 5184*pow(z,5)*HPL(0,0,1,z) - 2592*HPL(0,1,0,z) + 12096*z*HPL(0,1,0,z) + 4320*pow(z,2)*HPL(0,1,0,z) - 12096*pow(z,3)*HPL(0,1,0,z) - 4104*pow(z,4)*HPL(0,1,0,z) - 4968*pow(z,5)*HPL(0,1,0,z) - 13824*HPL(0,1,1,z) - 27648*pow(z,2)*HPL(0,1,1,z) + 27648*pow(z,4)*HPL(0,1,1,z) - 13824*pow(z,5)*HPL(0,1,1,z) - 1728*HPL(1,-1,0,z) - 1728*z*HPL(1,-1,0,z) - 1728*pow(z,2)*HPL(1,-1,0,z) + 1728*pow(z,3)*HPL(1,-1,0,z) + 1728*pow(z,4)*HPL(1,-1,0,z) + 1728*pow(z,5)*HPL(1,-1,0,z) - 6048*HPL(1,0,0,z) + 18144*z*HPL(1,0,0,z) + 4320*pow(z,2)*HPL(1,0,0,z) - 17280*pow(z,3)*HPL(1,0,0,z) - 3456*pow(z,4)*HPL(1,0,0,z) - 7776*pow(z,5)*HPL(1,0,0,z) - 13824*HPL(1,0,1,z) + 13824*z*HPL(1,0,1,z) - 13824*pow(z,2)*HPL(1,0,1,z) - 13824*pow(z,3)*HPL(1,0,1,z) + 13824*pow(z,4)*HPL(1,0,1,z) - 13824*pow(z,5)*HPL(1,0,1,z) - 14472*HPL(1,1,0,z) + 42120*z*HPL(1,1,0,z) + 13176*pow(z,2)*HPL(1,1,0,z) - 41688*pow(z,3)*HPL(1,1,0,z) - 12744*pow(z,4)*HPL(1,1,0,z) - 14472*pow(z,5)*HPL(1,1,0,z) - 648*consts::z3 + 11232*z*consts::z3 + 4320*pow(z,2)*consts::z3 - 2808*pow(z,3)*consts::z3 + 3240*pow(z,4)*consts::z3 - 7128*pow(z,5)*consts::z3))/(432.*z*(-1 + pow(z,2))) + consts::nf*((pow(L,2)*(-4 - 3*z + 3*pow(z,2) + 4*pow(z,3) + pow(Nc,2)*(8 - 5*z + pow(z,2) - 8*pow(z,3))))/(24.*Nc*z) + consts::z2*(-((L*(-1 + pow(Nc,2))*(1 + z))/Nc) + (-4 - 11*z - 9*pow(z,2) + 36*pow(z,3) - 12*pow(z,4) + pow(Nc,2)*(8 + 9*z + 12*pow(z,2) - 47*pow(z,3) + 16*pow(z,4)) + 3*(-1 + z)*(2 + 2*(-9 + 8*pow(Nc,2))*z + (-15 + 16*pow(Nc,2))*pow(z,2))*HPL(1,z))/(12.*Nc*(-1 + z)*z)) - (L*(16 - 73*pow(Nc,2) + 84*z + 7*pow(Nc,2)*z - 66*pow(z,2) - 5*pow(Nc,2)*pow(z,2) - 34*pow(z,3) + 91*pow(Nc,2)*pow(z,3) + 36*(-1 + pow(Nc,2))*z*(1 + z)*HPL(0,1,z) + 72*(-1 + pow(Nc,2))*z*(1 + z)*HPL(1,0,z)))/(36.*Nc*z) - (-72*(-4 - 5*z + 9*pow(z,2) + 8*pow(z,3) - 8*pow(z,4) + pow(Nc,2)*(-4 + 21*z - 33*pow(z,2) + 8*pow(z,3)))*HPL(0,1,z) + 36*(4 + 23*z + 3*pow(z,2) - 50*pow(z,3) + 20*pow(z,4) + pow(Nc,2)*(16 - 77*z + 66*pow(z,2) + 13*pow(z,3)))*HPL(1,0,z) + (-1 + z)*(-152 - 1643*pow(Nc,2) + 3108*z - 14*pow(Nc,2)*z - 2460*pow(z,2) - 149*pow(Nc,2)*pow(z,2) - 496*pow(z,3) + 2030*pow(Nc,2)*pow(z,3) + 432*(-1 + pow(Nc,2))*z*(1 + z)*HPL(0,0,1,z) - 216*(-1 + pow(Nc,2))*z*(1 + z)*HPL(0,1,0,z) - 864*z*HPL(0,1,1,z) + 864*pow(Nc,2)*z*HPL(0,1,1,z) - 864*pow(z,2)*HPL(0,1,1,z) + 864*pow(Nc,2)*pow(z,2)*HPL(0,1,1,z) - 216*HPL(1,0,0,z) + 648*z*HPL(1,0,0,z) - 432*pow(Nc,2)*z*HPL(1,0,0,z) + 324*pow(z,2)*HPL(1,0,0,z) - 432*pow(Nc,2)*pow(z,2)*HPL(1,0,0,z) - 216*HPL(1,1,0,z) + 1944*z*HPL(1,1,0,z) - 1728*pow(Nc,2)*z*HPL(1,1,0,z) + 1620*pow(z,2)*HPL(1,1,0,z) - 1728*pow(Nc,2)*pow(z,2)*HPL(1,1,0,z) + 432*consts::z3 - 864*z*consts::z3 + 432*pow(Nc,2)*z*consts::z3 - 216*pow(z,2)*consts::z3 + 432*pow(Nc,2)*pow(z,2)*consts::z3))/(432.*Nc*(-1 + z)*z));
        
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        
        
        return real(res);
    }
    
    
    
    double nnlo_r_lz0_logz(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        complex<double> res =  (2*pow(L,2)*pow(Nc,2)*(1 + 3*pow(z,2) - 4*pow(z,3) + pow(z,4)))/((-1 + z)*z) + (L*pow(Nc,2)*(77 - 179*z + 279*pow(z,2) - 353*pow(z,3) + 187*pow(z,4)))/(6.*(-1 + z)*z) - (pow(Nc,2)*(2 + 6*z + 18*pow(z,2) - 6*pow(z,3) - 19*pow(z,4) + 10*pow(z,5))*consts::z2)/(z*(-1 + pow(z,2))) - (pow(Nc,2)*(144*(-1 + z)*pow(1 + z + pow(z,2),2)*HPL(-1,0,z) - (1 + z)*(2069 - 5083*z + 5655*pow(z,2) - 6427*pow(z,3) + 4054*pow(z,4) + 2304*z*(-1 + pow(z,2))*HPL(1,0,z))))/(72.*z*(-1 + pow(z,2))) + consts::nf*((pow(L,2)*(-1 + pow(Nc,2))*(1 + z))/(4.*Nc) + (L*(4 + 8*z - 3*pow(z,2) - 17*pow(z,3) + 8*pow(z,4) - pow(Nc,2)*(8 + 4*z + 9*pow(z,2) - 29*pow(z,3) + 12*pow(z,4))))/(12.*Nc*(-1 + z)*z) - (3*(-1 + pow(Nc,2))*(1 + z)*consts::z2)/(2.*Nc) + (pow(Nc,2)*(-146 + 46*z + 3*pow(z,2) + 337*pow(z,3) - 280*pow(z,4)) + 8*(4 + 23*z - 45*pow(z,2) + pow(z,3) + 17*pow(z,4)) - 144*(-1 + pow(Nc,2))*z*(-1 + pow(z,2))*HPL(1,0,z))/(72.*Nc*(-1 + z)*z));
        
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        
        return real(res)*log(z);
    }
    
    double nnlo_r_lz0_logz_sq(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        complex<double> res =  (pow(Nc,2)*(154 - 389*z + 699*pow(z,2) - 827*pow(z,3) + 374*pow(z,4)))/(24.*(-1 + z)*z) + (L*pow(Nc,2)*(2 + 3*z + 8*pow(z,2) - 3*pow(z,3) - 8*pow(z,4) + 3*pow(z,5)))/(z*(-1 + pow(z,2))) + consts::nf*((L*(-1 + pow(Nc,2))*(1 + z))/(2.*Nc) + (8 + 31*z - 18*pow(z,2) - 45*pow(z,3) + 24*pow(z,4) - pow(Nc,2)*(16 + 15*z + 24*pow(z,2) - 83*pow(z,3) + 32*pow(z,4)))/(48.*Nc*(-1 + z)*z));
        
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        
        return real(res)*pow(log(z),2.);
    }
    
    double nnlo_r_lz0_logz_cube(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        complex<double> res =  (5*(-1 + pow(Nc,2))*consts::nf*(1 + z))/(24.*Nc) + (pow(Nc,2)*(4 + 7*z + 18*pow(z,2) - 7*pow(z,3) - 17*pow(z,4) + 9*pow(z,5)))/(6.*z*(-1 + pow(z,2)));
        
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        
        return real(res)*pow(log(z),3.);
    }
    
    double nnlo_r_lz1(const double& z, const double& L)
    {
        const double Nc=3.;
        complex<double> res=
        (-4*pow(L,2)*pow(Nc,2)*(-1 + 2*z - pow(z,2) + pow(z,3)))/z + \
        (2*pow(Nc,2)*(-4 - 25*z - 56*pow(z,2) - 30*pow(z,3) + \
                      6*pow(z,4))*consts::z2)/(z*(1 + z)) - (L*pow(Nc,2)*(110 - 237*z + \
                                                                          243*pow(z,2) - 226*pow(z,3) + 110*pow(z,4) + 6*(13 - 10*z + \
                                                                                                                          39*pow(z,2) - 42*pow(z,3) + 13*pow(z,4))*HPL(0,z)))/(3.*(-1 + z)*z) - \
        (pow(Nc,2)*(12*(182 - 191*z + 266*pow(z,2) - 128*pow(z,3) - \
                        420*pow(z,4) + 347*pow(z,5))*HPL(0,z) + 36*(13 + 5*z + 33*pow(z,2) - \
                                                                    5*pow(z,3) - 33*pow(z,4) + 15*pow(z,5))*pow(HPL(0,z),2) - (-1 + \
                                                                                                                               z)*(144*pow(1 + z + pow(z,2),2)*HPL(-1,0,z) - (1 + z)*(-2295 + 2159*z \
                                                                                                                                                                                      - 1894*pow(z,2) + 2295*pow(z,3) + 2304*z*(1 + \
                                                                                                                                                                                                                                z)*HPL(1,0,z)))))/(36.*z*(-1 + pow(z,2))) + consts::nf*((4*(-1 + \
                                                                                                                                                                                                                                                                                            pow(Nc,2))*(1 + z)*consts::z2)/Nc + (L*(4 + 3*z - 3*pow(z,2) - \
                                                                                                                                                                                                                                                                                                                                    4*pow(z,3) + pow(Nc,2)*(-8 + 5*z - pow(z,2) + 8*pow(z,3)) - 12*(-1 + \
                                                                                                                                                                                                                                                                                                                                                                                                    pow(Nc,2))*z*(1 + z)*HPL(0,z)))/(6.*Nc*z) + (6*(-8 - 13*z + \
                                                                                                                                                                                                                                                                                                                                                                                                                                                    12*pow(z,2) + 25*pow(z,3) - 16*pow(z,4) + pow(Nc,2)*(4 + 25*z - \
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         24*pow(z,2) - 21*pow(z,3) + 12*pow(z,4)))*HPL(0,z) - 54*(-1 + \
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  pow(Nc,2))*z*(-1 + pow(z,2))*pow(HPL(0,z),2) + (-1 + z)*(4*(8 + 42*z \
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              - 33*pow(z,2) - 17*pow(z,3)) + pow(Nc,2)*(-146 + 14*z - 13*pow(z,2) + \
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        182*pow(z,3)) + 144*(-1 + pow(Nc,2))*z*(1 + \
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                z)*HPL(1,0,z)))/(36.*Nc*(-1 + z)*z))
        ;
        
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        
        return real(res)*log(1.-z);
    }
    
    double nnlo_r_lz2(const double& z, const double& L)
    {
        const double Nc=3.;
        complex<double> res=(12*L*pow(Nc,2)*(-1 + 2*z - pow(z,2) + pow(z,3)))/z + (consts::nf*(-4 \
                                                                                               - 3*z + 3*pow(z,2) + 4*pow(z,3) + pow(Nc,2)*(8 - 5*z + pow(z,2) - \
                                                                                                                                            8*pow(z,3)) + 12*(-1 + pow(Nc,2))*z*(1 + z)*HPL(0,z)))/(6.*Nc*z) + \
        (pow(Nc,2)*(220 - 474*z + 486*pow(z,2) - 452*pow(z,3) + 220*pow(z,4) \
                    + 3*(63 - 62*z + 189*pow(z,2) - 190*pow(z,3) + \
                         63*pow(z,4))*HPL(0,z)))/(6.*(-1 + z)*z)
        ;
        
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        
        return real(res)*pow(log(1.-z),2.);
    }
    
    double nnlo_r_lz3(const double& z, const double& L)
    {
        const double Nc=3.;
        complex<double> res=(-8*pow(Nc,2)*(-1 + 2*z - pow(z,2) + pow(z,3)))/z;
        
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        
        return real(res)*pow(log(1.-z),3.);
    }
// ------ n3LO gg regular: next-to-soft truncated implementation
    double gg_n3lo_r_lz0_NS(const double& z, const double& L)
    {
        const double Nc=3.;
        const double z2 = consts::z2;
        const double z3 = consts::z3;
        const double z4 = consts::z4;
        const double z5 = consts::z5;
        const double nf = consts::nf;
        double res = (834419*pow(Nc,3))/23328. + (51575*L*pow(Nc,3))/1296. + (3509*pow(L,2)*pow(Nc,3))/216. + (517*pow(L,3)*pow(Nc,3))/216. + (5065*nf)/1728. + (75*L*nf)/32. + (5*pow(L,2)*nf)/16. - (527831*pow(Nc,2)*nf)/46656. - (33335*L*pow(Nc,2)*nf)/2592. - (1889*pow(L,2)*pow(Nc,2)*nf)/432. - (29*pow(L,3)*pow(Nc,2)*nf)/54. + (49*Nc*pow(nf,2))/729. + (23*L*Nc*pow(nf,2))/81. + (4*pow(L,2)*Nc*pow(nf,2))/27. + (pow(L,3)*Nc*pow(nf,2))/54. - (11183*pow(Nc,3)*z2)/162. - (169*L*pow(Nc,3)*z2)/3. - (341*pow(L,2)*pow(Nc,3)*z2)/12. - 4*pow(L,3)*pow(Nc,3)*z2 - (5*nf*z2)/24. + (4579*pow(Nc,2)*nf*z2)/324. + (193*L*pow(Nc,2)*nf*z2)/18. + (7*pow(L,2)*pow(Nc,2)*nf*z2)/6. - (19*Nc*pow(nf,2)*z2)/36. - (2*L*Nc*pow(nf,2)*z2)/9. - (32849*pow(Nc,3)*z3)/216. - (1033*L*pow(Nc,3)*z3)/6. - 35*pow(L,2)*pow(Nc,3)*z3 - (149*nf*z3)/72. - (3*L*nf*z3)/2. + (1789*pow(Nc,2)*nf*z3)/72. + (55*L*pow(Nc,2)*nf*z3)/6. - (5*Nc*pow(nf,2)*z3)/27. + (725*pow(Nc,3)*z2*z3)/6. - (821*pow(Nc,3)*z4)/12. - (77*L*pow(Nc,3)*z4)/2. - (nf*z4)/4. + (19*pow(Nc,2)*nf*z4)/8. - 186*pow(Nc,3)*z5;
        return res;
    }

    double gg_n3lo_r_lz1_NS(const double& z, const double& L)
    {
        const double Nc=3.;
        const double z2 = consts::z2;
        const double z3 = consts::z3;
        const double z4 = consts::z4;
        const double nf = consts::nf;
        double res = (-9547*pow(Nc,3))/108. - (2239*L*pow(Nc,3))/24. - (439*pow(L,2)*pow(Nc,3))/12. - (23*pow(L,3)*pow(Nc,3))/3. - (17*nf)/4. - (3*L*nf)/4. + (8071*pow(Nc,2)*nf)/324. + (4295*L*pow(Nc,2)*nf)/216. + (61*pow(L,2)*pow(Nc,2)*nf)/9. + (2*pow(L,3)*pow(Nc,2)*nf)/3. - (163*Nc*pow(nf,2))/324. - (61*L*Nc*pow(nf,2))/108. - (pow(L,2)*Nc*pow(nf,2))/9. + (2375*pow(Nc,3)*z2)/18. + (479*L*pow(Nc,3)*z2)/3. + 36*pow(L,2)*pow(Nc,3)*z2 + (nf*z2)/24. - (1813*pow(Nc,2)*nf*z2)/72. - (26*L*pow(Nc,2)*nf*z2)/3. + (4*Nc*pow(nf,2)*z2)/9. + 362*pow(Nc,3)*z3 + 180*L*pow(Nc,3)*z3 + 3*nf*z3 - (223*pow(Nc,2)*nf*z3)/12. + 77*pow(Nc,3)*z4;
        return res*log(1.-z);
    }
    
    double gg_n3lo_r_lz2_NS(const double& z, const double& L)
    {
        const double Nc=3.;
        const double z2 = consts::z2;
        const double z3 = consts::z3;
        const double nf = consts::nf;
        double res = (2711*pow(Nc,3))/27. + (3395*L*pow(Nc,3))/36. + (89*pow(L,2)*pow(Nc,3))/2. + 4*pow(L,3)*pow(Nc,3) + nf/4. - (4139*pow(Nc,2)*nf)/216. - (151*L*pow(Nc,2)*nf)/9. - 3*pow(L,2)*pow(Nc,2)*nf + (59*Nc*pow(nf,2))/108. + (2*L*Nc*pow(nf,2))/9. - (2147*pow(Nc,3)*z2)/12. - 84*L*pow(Nc,3)*z2 + (545*pow(Nc,2)*nf*z2)/48. - 181*pow(Nc,3)*z3;
        return res*pow(log(1.-z),2);
    }
    
    double gg_n3lo_r_lz3_NS(const double& z, const double& L)
    {
        const double Nc=3.;
        const double nf = consts::nf;
        const double z2 = consts::z2;
        double res = (-3469*pow(Nc,3))/54. - (679*L*pow(Nc,3))/9. - 16*pow(L,2)*pow(Nc,3) + (205*pow(Nc,2)*nf)/18. + (40*L*pow(Nc,2)*nf)/9. - (4*Nc*pow(nf,2))/27. + 56*pow(Nc,3)*z2;
        return res*pow(log(1.-z),3);
    }
    
    double gg_n3lo_r_lz4_NS(const double& z, const double& L)
    {
        const double Nc=3.;
        const double nf = consts::nf;
        double res = (353*pow(Nc,3))/9. + 20*L*pow(Nc,3) - (20*pow(Nc,2)*nf)/9.;
        return res*pow(log(1.-z),4);
    }
    
    double gg_n3lo_r_lz5_NS(const double& z, const double& L)
    {
        const double Nc=3.;
        double res = -8*pow(Nc,3);
        return res*pow(log(1.-z),5);
    }
    
// ------
    // only up to next to soft coefficient
    // calling the NS trucated version
    double gg_n3lo_r_lz0(const double& z, const double& L)
    {
        return gg_n3lo_r_lz0_NS(z,L);
    }
    // only up to next to soft coefficient
    // calling the NS trucated version
    double gg_n3lo_r_lz1(const double& z, const double& L)
    {
        return gg_n3lo_r_lz1_NS(z,L);
    }

    // only up to next to soft coefficient
    // calling the NS trucated version
    double gg_n3lo_r_lz2(const double& z, const double& L)
    {

        return gg_n3lo_r_lz2_NS(z,L);
    }
    
    
    // the full log(1-z)^3 implemented
    double gg_n3lo_r_lz3(const double& z, const double& L)
    {
        const double Nc=3.;
        const double nf = consts::nf;
        const double z2 = consts::z2;
        complex<double> res=(Nc*pow(nf,2)*(292 - 309*z + 117*pow(z,2) - 292*pow(z,3)))/(1296.*z) + (25*pow(nf,2)*(-4 - 3*z + 3*pow(z,2) + 4*pow(z,3)))/(1296.*Nc*z) - (pow(Nc,3)*(-19980 + 1120*z + 1841*pow(z,2) + 721*pow(z,3) + 19980*pow(z,4) + 2808*z2 + 3456*z*z2 + 10152*pow(z,2)*z2 + 6480*pow(z,3)*z2 - 3240*pow(z,4)*z2 - 432*pow(1 + z + pow(z,2),2)*HPL(-1,z)*HPL(0,z) + 6912*z*pow(1 + z,2)*HPL(0,z)*HPL(1,z) + 432*HPL(0,-1,z) + 864*z*HPL(0,-1,z) + 1296*pow(z,2)*HPL(0,-1,z) + 864*pow(z,3)*HPL(0,-1,z) + 432*pow(z,4)*HPL(0,-1,z) - 6912*z*HPL(0,1,z) - 13824*pow(z,2)*HPL(0,1,z) - 6912*pow(z,3)*HPL(0,1,z)))/(54.*z*(1 + z)) + nf*((-5865 + 14900/z + 8457*z - 17492*pow(z,2) - 10368*(1 + z)*z2)/1296. - 8*(1 + z)*HPL(0,z)*HPL(1,z) + 8*(1 + z)*HPL(0,1,z)) + (pow(Nc,2)*nf*(-160268 + 172203*z - 149895*pow(z,2) + 168584*pow(z,3) + 35352*z*z2 + 35352*pow(z,2)*z2 + 35352*z*(1 + z)*HPL(0,z)*HPL(1,z) - 35352*z*(1 + z)*HPL(0,1,z)))/(5184.*z) + (nf*(-1644 - 2861*z + 2177*pow(z,2) + 2328*pow(z,3) + 2040*z*z2 + 2040*pow(z,2)*z2 + 2040*z*(1 + z)*HPL(0,z)*HPL(1,z) - 2040*z*(1 + z)*HPL(0,1,z)))/(1728.*pow(Nc,2)*z) + ((-25*pow(nf,2)*(1 + z))/(216.*Nc) + (25*Nc*pow(nf,2)*(1 + z))/216. + (nf*(-1.28125 - 23/(72.*z) - (8*z)/9. + (85*pow(z,2))/108.))/pow(Nc,2) + (nf*(1820 + 2903*z + 1229*pow(z,2) - 1564*pow(z,3)))/(432.*z) + (pow(Nc,3)*(4477 - 7514*z + 11190*pow(z,2) - 13598*pow(z,3) + 6259*pow(z,4)))/(27*z - 27*pow(z,2)) + (pow(Nc,2)*nf*(8100 - 2377*z + 11199*pow(z,2) - 19370*pow(z,3) + 7184*pow(z,4)))/(864.*(-1 + z)*z))*log(z) + ((pow(Nc,2)*nf*(-31 - 211*z))/36. - (37*nf*(1 + z))/(72.*pow(Nc,2)) + (nf*(11 + 51*z))/8. + (pow(Nc,3)*(28 + 6*z + 98*pow(z,2) - 6*pow(z,3) - 98*pow(z,4) + 32*pow(z,5)))/(z - pow(z,3)))*pow(log(z),2);
        
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        
        return real(res)*pow(log(1.-z),3.);
    }
    
    // the full log(1-z)^4 implemented
    double gg_n3lo_r_lz4(const double& z, const double& L)
    {
        const double Nc=3.;
        const double nf = consts::nf;

        complex<double> res=nf*(-1 - 4/(3.*z) + z + (4*pow(z,2))/3.) + (pow(Nc,2)*nf*(5804 - 6207*z + 2367*pow(z,2) - 5804*pow(z,3)))/(1728.*z) - (85*nf*(-4 - 3*z + 3*pow(z,2) + 4*pow(z,3)))/(1728.*pow(Nc,2)*z) + (pow(Nc,3)*(-671 + 751*z - 641*pow(z,2) + 671*pow(z,3)))/(9.*z) + (-2*nf*(1 + z) + (85*nf*(1 + z))/(288.*pow(Nc,2)) + (491*pow(Nc,2)*nf*(1 + z))/288. + (pow(Nc,3)*(27 - 22*z + 81*pow(z,2) - 86*pow(z,3) + 27*pow(z,4)))/((-1 + z)*z))*log(z);
        
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        
        return real(res)*pow(log(1.-z),4.);
    }
    
    // the full log(1-z)^5 term implemented
    double gg_n3lo_r_lz5(const double& z, const double& L)
    {
        const double Nc=3.;
        complex<double> res=8*pow(Nc,3)*(-2 + 1/z + z - pow(z,2));
        
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        
        return real(res)*pow(log(1.-z),5.);
    }
    
    double LEggNNNLOregSte(const double& z, const double& L)
    {
        double zz = z;
        double LL = L;
        return leggnnnloreg_(&zz,&LL);
    }
    
    double LEggN3LOregFalko(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double z2= consts::z2;
        const double z3= consts::z3;
        const double z4= consts::z4;
        if (abs(L)<1e-15) return 0.0;
        complex<double> L3=(-240*Nc*pow(nf,2)*(-1 + pow(z,2))*(-4 - 3*z + 3*pow(z,2) + 4*pow(z,3)) + 240*pow(Nc,3)*pow(nf,2)*(1 + z)*(6 - 7*z - 5*pow(z,3) + 6*pow(z,4)) - 45*nf*z*(-1 + pow(z,2))*(23*(-1 + z) + 24*(1 + z)*z2) - 120*pow(Nc,5)*(-1 + pow(z,2))*(6085 - 6326*z + 6205*pow(z,2) - 6085*pow(z,3) + 864*(-1 - 3*pow(z,2) + pow(z,3))*z2) + 90*pow(Nc,2)*nf*(-1 + pow(z,2))*(-352 + 352*pow(z,3) + pow(z,2)*(-493 + 144*z2) + z*(493 + 144*z2)) - 15*pow(Nc,4)*nf*(-1 + pow(z,2))*(-4576 + 4576*pow(z,3) + pow(z,2)*(-5107 + 792*z2) + z*(5459 + 792*z2)))/(25920.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((2304*pow(Nc,5)*(-1 + z)*z*pow(1 + z,2) - 12*nf*z*(1 + z)*(-1 + pow(z,2)) + 144*pow(Nc,2)*nf*z*(1 + z)*(-1 + pow(z,2)) - 132*pow(Nc,4)*nf*z*(1 + z)*(-1 + pow(z,2)))*HPL(1,0,z))/(288.*pow(Nc,2)*z*(-1 + pow(z,2))) + (4*pow(Nc,3)*(-1 + z + pow(z,2) + pow(z,4))*pow(log(1 - z),2))/(z*(1 + z)) + ((96*Nc*pow(nf,2)*z*(1 + z)*(-1 + pow(z,2)) - 96*pow(Nc,3)*pow(nf,2)*z*(1 + z)*(-1 + pow(z,2)) + nf*(-1 + pow(z,2))*(-36*pow(z,2) - 48*pow(z,3)) + 48*pow(Nc,2)*nf*(1 + z)*(10 + z - 9*pow(z,2) - 4*pow(z,3) + 2*pow(z,4)) - 12*pow(Nc,4)*nf*(88 + 92*z + 115*pow(z,2) - 96*pow(z,3) - 155*pow(z,4) + 52*pow(z,5)) + 288*pow(Nc,5)*(55 - z + 49*pow(z,2) - 43*pow(z,3) - 93*pow(z,4) + 55*pow(z,5)))*log(z))/(1728.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((12*nf*z*(1 + z)*(-1 + pow(z,2)) - 72*pow(Nc,2)*nf*z*(-3 + 7*z)*(-1 + pow(z,2)) + 12*pow(Nc,4)*nf*z*(19 - 41*z - 19*pow(z,2) + 41*pow(z,3)) + 768*pow(Nc,5)*(1 - z + 7*pow(z,2) + pow(z,3) - 7*pow(z,4) + pow(z,5)))*pow(log(z),2))/(1152.*pow(Nc,2)*z*(-1 + pow(z,2))) + log(1 - z)*((-72*pow(Nc,2)*nf*(-1 + pow(z,2))*(-4 - 3*z + 3*pow(z,2) + 4*pow(z,3)) + 3*nf*(-1 + pow(z,2))*(-8 - 6*z + 6*pow(z,2) + 8*pow(z,3)) - 288*pow(Nc,5)*(55 - 58*z - 8*pow(z,2) + 3*pow(z,3) - 47*pow(z,4) + 55*pow(z,5)) + 6*pow(Nc,4)*nf*(140 - 159*z - 77*pow(z,2) + 19*pow(z,3) - 63*pow(z,4) + 140*pow(z,5)))/(864.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((-12*nf*z*(1 + z)*(-1 + pow(z,2)) + 144*pow(Nc,2)*nf*z*(1 + z)*(-1 + pow(z,2)) - 132*pow(Nc,4)*nf*z*(1 + z)*(-1 + pow(z,2)) - 1152*pow(Nc,5)*(1 + z + 3*pow(z,2) - pow(z,3) - 3*pow(z,4) + pow(z,5)))*log(z))/(288.*pow(Nc,2)*z*(-1 + pow(z,2))));
        check_imaginary_part(L3,__PRETTY_FUNCTION__);

        complex<double> L2 = (-120*Nc*pow(nf,2)*(-1 + pow(z,2))*(-58 + 76*pow(z,3) + 3*z*(-41 + 18*z2) + 3*pow(z,2)*(35 + 18*z2)) + 120*pow(Nc,3)*pow(nf,2)*(-1 + z)*(1 + z)*(-115 + 133*pow(z,3) + z*(-32 + 54*z2) + pow(z,2)*(34 + 54*z2)) - 90*pow(Nc,2)*nf*(-1 + pow(z,2))*(1613 - 4234*z - 3089*pow(z,3) + 12*(-36 - 81*z - 57*pow(z,2) + 28*pow(z,3))*z2 + pow(z,2)*(5620 - 1728*z3)) + 15*nf*(-1 + pow(z,2))*(214 - 1357*pow(z,3) + 36*(-4 - 15*z - 6*pow(z,2) + 16*pow(z,3))*z2 + pow(z,2)*(831 - 216*z3) - 24*z*(-13 + 9*z3)) - 30*pow(Nc,5)*(-137875 + 72*(418 - 193*z + 881*pow(z,2) - 852*pow(z,3) - 1310*pow(z,4) + 1056*pow(z,5))*z2 + pow(z,4)*(148235 - 51840*z3) + z*(154387 - 25056*z3) + 6912*z3 - 192*pow(z,3)*(86 + 27*z3) + 40*pow(z,2)*(-259 + 540*z3) + pow(z,5)*(-137875 + 19008*z3)) + 15*pow(Nc,4)*nf*(-1 + z)*(36*(-116 - 367*z - 623*pow(z,2) - 276*pow(z,3) + 104*pow(z,4))*z2 - (1 + z)*(-41536 + 49249*pow(z,3) + z*(58060 - 216*z3) + pow(z,2)*(-61193 + 10152*z3))))/(25920.*pow(Nc,2)*z*(-1 + pow(z,2))) + (8*pow(Nc,3)*pow(1 + z + pow(z,2),2)*z2*HPL(-1,z))/(z*(1 + z)) + ((1296*pow(Nc,4)*nf*(-1 + z)*z*pow(1 + z,2)*z2 + 12*pow(Nc,2)*nf*(-120*z - 120*pow(z,2))*(-1 + pow(z,2))*z2 - 6*nf*(-24*z - 24*pow(z,2))*(-1 + pow(z,2))*z2 + 576*pow(Nc,5)*(-1 + 39*z + 39*pow(z,2) - 39*pow(z,3) - 39*pow(z,4) + pow(z,5))*z2)*HPL(1,z))/(288.*pow(Nc,2)*z*(-1 + pow(z,2))) - ((6*pow(Nc,2)*nf*pow(1 + z,2)*(4 - 7*z + 4*pow(z,2)) - 6*pow(Nc,4)*nf*(8 + 9*z + 6*pow(z,2) + 9*pow(z,3) + 8*pow(z,4)) + 12*pow(Nc,5)*(99 + 182*z + 177*pow(z,2) + 182*pow(z,3) + 99*pow(z,4)))*HPL(-1,0,z))/(72.*pow(Nc,2)*z*(1 + z)) + ((-72*Nc*pow(nf,2)*z*(1 + z)*(-1 + pow(z,2)) + 6*nf*(-1 + pow(z,2))*(8 - 6*z - 15*pow(z,2) + 4*pow(z,3)) + 12*pow(Nc,2)*nf*(-1 + pow(z,2))*(12 + 57*z + 75*pow(z,2) + 4*pow(z,3)) + 8*pow(Nc,3)*pow(nf,2)*(1 + z)*(-9*z + 9*pow(z,3)) - 480*pow(Nc,5)*pow(1 + z,2)*(-11 + 56*z - 56*pow(z,2) + 11*pow(z,3)) - 6*pow(Nc,4)*nf*(1 + z)*(-32 - 300*z - 27*pow(z,2) + 347*pow(z,3) + 12*pow(z,4)))*HPL(1,0,z))/(288.*pow(Nc,2)*z*(-1 + pow(z,2))) + (8*pow(Nc,3)*pow(1 + z + pow(z,2),2)*HPL(-1,-1,0,z))/(z*(1 + z)) - (6*pow(Nc,3)*pow(1 + z + pow(z,2),2)*HPL(-1,0,0,z))/(z*(1 + z)) + (4*pow(Nc,3)*pow(1 + z + pow(z,2),2)*HPL(-1,1,0,z))/(z*(1 + z)) + ((6*pow(Nc,4)*nf*pow(-1 + z,2)*z*(1 + z) - pow(Nc,2)*nf*(-1 + pow(z,2))*(-6*z + 6*pow(z,2)) - 48*pow(Nc,5)*(-1 + z - 3*pow(z,2) - pow(z,3) + 3*pow(z,4) + pow(z,5)))*HPL(0,-1,0,z))/(12.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((-(pow(Nc,2)*nf*(66*z - 114*pow(z,2))*(-1 + pow(z,2))) - pow(Nc,4)*nf*(66*z - 114*pow(z,2) - 66*pow(z,3) + 114*pow(z,4)) + 96*pow(Nc,5)*(3 + 2*z - 5*pow(z,2) - 2*pow(z,3) + 5*pow(z,4) + 2*pow(z,5)))*HPL(0,1,0,z))/(24.*pow(Nc,2)*z*(-1 + pow(z,2))) + (4*pow(Nc,3)*pow(1 + z + pow(z,2),2)*HPL(1,-1,0,z))/(z*(1 + z)) + ((-2*pow(Nc,2)*nf*(-6*z - 186*pow(z,2))*(-1 + pow(z,2)) + 3*nf*(-6*z - 6*pow(z,2))*(-1 + pow(z,2)) + pow(Nc,4)*nf*(-6*z + 354*pow(z,2) + 6*pow(z,3) - 354*pow(z,4)) - 96*pow(Nc,5)*(-1 + 15*z + 39*pow(z,2) - 15*pow(z,3) - 39*pow(z,4) + pow(z,5)))*HPL(1,0,0,z))/(48.*pow(Nc,2)*z*(-1 + pow(z,2))) - ((nf*(-1 + z)*(-24*z - 24*pow(z,2)) + 2*pow(Nc,2)*nf*(-1 + z)*(120*z + 120*pow(z,2)) - 4*pow(Nc,5)*(960*z - 960*pow(z,3)) + pow(Nc,4)*nf*(216*z - 216*pow(z,3)))*HPL(1,1,0,z))/(48.*pow(Nc,2)*(-1 + z)*z) - (16*pow(Nc,3)*(-1 + 2*z - pow(z,2) + pow(z,3))*pow(log(1 - z),3))/z + ((8*Nc*pow(nf,2)*(-1 + pow(z,2))*(24 + 93*z + 75*pow(z,2) - 36*pow(z,3)) + 24*pow(Nc,3)*pow(nf,2)*(1 + z)*(12 + 19*z + 6*pow(z,2) - 49*pow(z,3) + 16*pow(z,4)) + nf*(-1 + pow(z,2))*(297*z - 432*pow(z,2) - 144*pow(z,3) + 72*(-9*z - 9*pow(z,2))*z2) - 24*pow(Nc,5)*(-5238 + 11857*z - 3557*pow(z,2) + 2889*pow(z,3) + 8138*pow(z,4) - 15403*pow(z,5) + 576*(1 + 8*pow(z,2) - 8*pow(z,4) + 2*pow(z,5))*z2) - 3*pow(Nc,4)*nf*(1 + z)*(4992 + 9344*pow(z,4) + pow(z,2)*(16833 - 4032*z2) + 3*z*(-4003 + 216*z2) + 12*pow(z,3)*(-1514 + 282*z2)) + 6*pow(Nc,2)*nf*(-1 + z)*(1 + z)*(-792 + 2264*pow(z,3) + z*(3083 - 216*z2) + 4*pow(z,2)*(-157 + 450*z2)))*log(z))/(1728.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((-6*nf*pow(z,2)*(-33 + 32*z)*(-1 + pow(z,2)) - 16*Nc*pow(nf,2)*(-9*z - 9*pow(z,2))*(-1 + pow(z,2)) + 16*pow(Nc,3)*pow(nf,2)*(1 + z)*(9*z - 9*pow(z,3)) + 12*pow(Nc,2)*nf*(-1 + pow(z,2))*(-40 - 273*pow(z,2) + 32*pow(z,3)) - 6*pow(Nc,4)*nf*(176 + 64*z + 913*pow(z,2) - 96*pow(z,3) - 993*pow(z,4) + 160*pow(z,5)) + 96*pow(Nc,5)*(143 - 104*z + 391*pow(z,2) - 204*pow(z,3) - 501*pow(z,4) + 352*pow(z,5)))*pow(log(z),2))/(1152.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((9*nf*z*(1 + z)*(-1 + pow(z,2)) + 2*pow(Nc,2)*nf*(57*z - 129*pow(z,2))*(-1 + pow(z,2)) + pow(Nc,4)*nf*(123*z - 249*pow(z,2) - 123*pow(z,3) + 249*pow(z,4)) + 192*pow(Nc,5)*(1 - 2*z + 11*pow(z,2) + 2*pow(z,3) - 11*pow(z,4) + 2*pow(z,5)))*pow(log(z),3))/(288.*pow(Nc,2)*z*(-1 + pow(z,2))) + pow(log(1 - z),2)*((-12*nf*(1 + z)*(-4 - 3*z + 3*pow(z,2) + 4*pow(z,3)) + 120*pow(Nc,2)*nf*(1 + z)*(-4 - 3*z + 3*pow(z,2) + 4*pow(z,3)) - 108*pow(Nc,4)*nf*(1 + z)*(-12 + 13*z - 5*pow(z,2) + 12*pow(z,3)) + 144*pow(Nc,5)*(-187 + 21*z + 33*pow(z,2) + 12*pow(z,3) + 187*pow(z,4)))/(288.*pow(Nc,2)*z*(1 + z)) + ((2*pow(Nc,2)*nf*(-1 + z)*(-120*z - 120*pow(z,2)) + nf*(-1 + z)*(24*z + 24*pow(z,2)) + pow(Nc,4)*nf*(-216*z + 216*pow(z,3)) + 384*pow(Nc,5)*(7 - 4*z + 21*pow(z,2) - 24*pow(z,3) + 7*pow(z,4)))*log(z))/(96.*pow(Nc,2)*(-1 + z)*z)) + log(1 - z)*((36*Nc*pow(nf,2)*(-1 + pow(z,2))*(-4 - 3*z + 3*pow(z,2) + 4*pow(z,3)) - 12*pow(Nc,3)*pow(nf,2)*(-1 + pow(z,2))*(-20 + 7*z + pow(z,2) + 20*pow(z,3)) + 12*pow(Nc,5)*(-1 + z)*(17149 - 500*z - 1314*pow(z,2) - 814*pow(z,3) - 17149*pow(z,4) + 144*(-17 - 20*z - 59*pow(z,2) - 38*pow(z,3) + 19*pow(z,4))*z2) + 6*nf*(-1 + pow(z,2))*(-34 + 52*pow(z,3) + 3*z*(-29 + 24*z2) + 3*pow(z,2)*(23 + 24*z2)) - 18*pow(Nc,2)*nf*(-1 + pow(z,2))*(-392 + 440*pow(z,3) + pow(z,2)*(-319 + 240*z2) + z*(271 + 240*z2)) + 12*pow(Nc,4)*nf*(-1 + pow(z,2))*(-1517 + 1580*pow(z,3) + 2*z*(824 + 162*z2) + pow(z,2)*(-1463 + 324*z2)))/(864.*pow(Nc,2)*z*(-1 + pow(z,2))) + (4*pow(Nc,3)*pow(1 + z + pow(z,2),2)*HPL(-1,0,z))/(z*(1 + z)) - ((nf*(-1 + z)*(-24*z - 24*pow(z,2)) + 2*pow(Nc,2)*nf*(-1 + z)*(120*z + 120*pow(z,2)) - 8*pow(Nc,5)*(480*z - 480*pow(z,3)) + pow(Nc,4)*nf*(216*z - 216*pow(z,3)))*HPL(1,0,z))/(48.*pow(Nc,2)*(-1 + z)*z) + ((8*Nc*pow(nf,2)*(-9*z - 9*pow(z,2))*(-1 + pow(z,2)) - 8*pow(Nc,3)*pow(nf,2)*(1 + z)*(9*z - 9*pow(z,3)) + 6*nf*(-1 + pow(z,2))*(-4 - 15*z - 6*pow(z,2) + 16*pow(z,3)) - 12*pow(Nc,2)*nf*(-1 + pow(z,2))*(-60 - 93*z - 39*pow(z,2) + 44*pow(z,3)) + 6*pow(Nc,4)*nf*(1 + z)*(276 - 41*z + 381*pow(z,2) - 688*pow(z,3) + 232*pow(z,4)) - 96*pow(Nc,5)*(275 - 134*z + 254*pow(z,2) - 196*pow(z,3) - 474*pow(z,4) + 385*pow(z,5)))*log(z))/(288.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((-(pow(Nc,2)*nf*(-6*z - 186*pow(z,2))*(-1 + pow(z,2))) + nf*(-9*z - 9*pow(z,2))*(-1 + pow(z,2)) + pow(Nc,4)*nf*(-3*z + 177*pow(z,2) + 3*pow(z,3) - 177*pow(z,4)) - 96*pow(Nc,5)*(5 + 2*z + 25*pow(z,2) - 2*pow(z,3) - 25*pow(z,4) + 6*pow(z,5)))*pow(log(z),2))/(48.*pow(Nc,2)*z*(-1 + pow(z,2))));
        
        complex<double> L1 = (-20*Nc*pow(nf,2)*(-1 + pow(z,2))*(-625 - 4632*z + 3633*pow(z,2) + 1624*pow(z,3) + 72*(2 + 19*z + 25*pow(z,2) - 4*pow(z,3))*z2 - 432*z3 + 864*z*z3 + 216*pow(z,2)*z3) - 20*pow(Nc,3)*pow(nf,2)*(1 + z)*(36*(8 + 28*z + 15*pow(z,2) - 65*pow(z,3) + 12*pow(z,4))*z2 - (-1 + z)*(-2656 + 3394*pow(z,3) + z*(-1373 + 432*z3) + pow(z,2)*(835 + 432*z3))) + 3*nf*(-1 + pow(z,2))*(30*(-168 - 489*z - 738*pow(z,2) + 488*pow(z,3))*z2 + 10*(410 + 144*z3 + 9*z*(115 + 54*z3) - 6*pow(z,2)*(-725 + 369*z3) + pow(z,3)*(-5795 + 1512*z3)) + 270*z*(51 + 71*z)*z4) - 2*pow(Nc,2)*nf*(-1 + pow(z,2))*(90*(-802 + 4201*z - 263*pow(z,2) + 3614*pow(z,3))*z2 + 5*(37351 + 11232*z3 - 36*z*(4526 + 39*z3) - 9*pow(z,2)*(-25787 + 6240*z3) + pow(z,3)*(-111601 + 22464*z3)) + 810*(68 - 23*z + 407*pow(z,2))*z4) + pow(Nc,4)*nf*(90*(3872 - 9535*z + 8792*pow(z,2) - 3933*pow(z,3) - 13000*pow(z,4) + 13452*pow(z,5))*z2 + 10*(pow(z,4)*(397808 - 73494*z3) + z*(369877 - 28674*z3) + 4*(-57731 + 756*z3) - 5*pow(z,3)*(17815 + 1998*z3) + 6*pow(z,2)*(-27814 + 8361*z3) + pow(z,5)*(-280802 + 29592*z3)) + 810*z*(371 - 529*z - 371*pow(z,2) + 529*pow(z,3))*z4) + 2*pow(Nc,5)*(-180*(6116 - 36640*z + 18668*pow(z,2) - 14094*pow(z,3) - 25173*pow(z,4) + 50881*pow(z,5))*z2 - 5*(z*(1446551 - 665712*z3) + pow(z,4)*(1549609 - 604152*z3) + 27*(-46725 + 1312*z3) - 4*pow(z,3)*(5195 + 55728*z3) + 9*pow(z,5)*(-158419 + 93192*z3) + pow(z,2)*(-288034 + 457056*z3)) + 3240*(103 - 163*z + 1086*pow(z,2) + 317*pow(z,3) - 1030*pow(z,4) + 162*pow(z,5))*z4))/(25920.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((-12*nf*pow(1 + z,2)*(1 - 4*z + pow(z,2))*z2 + 18*pow(Nc,2)*nf*pow(1 + z,2)*(8 - 5*z + 8*pow(z,2))*z2 - 12*pow(Nc,4)*nf*(23 + 25*z + 15*pow(z,2) + 19*pow(z,3) + 20*pow(z,4))*z2 + pow(Nc,5)*(6*(1314 + 2614*z + 2415*pow(z,2) + 1984*pow(z,3) + 1023*pow(z,4))*z2 + 72*(72 + 144*z + 216*pow(z,2) + 122*pow(z,3) + 61*pow(z,4))*z3))*HPL(-1,z))/(72.*pow(Nc,2)*z*(1 + z)) + ((-192*pow(Nc,3)*pow(nf,2)*(-1 + z)*z*pow(1 + z,2)*z2 + 24*Nc*pow(nf,2)*(2 - 10*z - 9*pow(z,2) + 10*pow(z,3) + 7*pow(z,4))*z2 - nf*(-1 + pow(z,2))*(6*(40 - 69*z - 129*pow(z,2) + 20*pow(z,3))*z2 - 360*z*(1 + z)*z3) + 2*pow(Nc,2)*nf*(-1 + pow(z,2))*(6*(72 - 329*z - 413*pow(z,2) + 8*pow(z,3))*z2 - 36*(-4 + 13*z + 103*pow(z,2))*z3) + pow(Nc,4)*nf*z*(6*(-1489 - 1523*z + 1461*pow(z,2) + 1563*pow(z,3) + 4*pow(z,4))*z2 + 288*(-1 - 25*z + pow(z,2) + 25*pow(z,3))*z3) + 4*pow(Nc,5)*(6*(-1934 + 2498*z + 4379*pow(z,2) - 3895*pow(z,3) - 2500*pow(z,4) + 1430*pow(z,5))*z2 + 144*(-18 + 39*z + 96*pow(z,2) - 42*pow(z,3) - 97*pow(z,4) + 29*pow(z,5))*z3))*HPL(1,z))/(288.*pow(Nc,2)*z*(-1 + pow(z,2))) - (2*pow(Nc,3)*(35 + 70*z + 105*pow(z,2) + 58*pow(z,3) + 29*pow(z,4))*z2*HPL(-1,-1,z))/(z*(1 + z)) - ((pow(Nc,2)*nf*pow(1 + z,2)*(10 + 29*z - 8*pow(z,2)) + nf*pow(1 + z,2)*(8 + 175*z + 44*pow(z,2)) - 2*pow(Nc,4)*nf*(151 + 95*z - 129*pow(z,2) - 3*pow(z,3) + 110*pow(z,4)) + 2*pow(Nc,5)*(2003 + 3594*z + 1359*pow(z,2) - 500*pow(z,3) - 288*(6 + 12*z + 18*pow(z,2) + 10*pow(z,3) + 5*pow(z,4))*z2))*HPL(-1,0,z))/(72.*pow(Nc,2)*z*(1 + z)) - (pow(Nc,3)*(47 + 94*z + 141*pow(z,2) + 88*pow(z,3) + 44*pow(z,4))*z2*HPL(-1,1,z))/(z*(1 + z)) + ((-(nf*pow(-1 + z,2)*z*(1 + z)) + 4*pow(Nc,2)*nf*pow(-1 + z,2)*z*(1 + z) - 6*pow(Nc,4)*nf*pow(-1 + z,2)*z*(1 + z) + 4*pow(Nc,5)*(-18 + z - 40*pow(z,2) - pow(z,3) + 37*pow(z,4) + 24*pow(z,5)))*z2*HPL(0,-1,z))/(2.*pow(Nc,2)*z*(-1 + pow(z,2))) - ((-(nf*(-1 + z)*z*pow(1 + z,2)) + pow(Nc,4)*nf*z*(-67 + 157*z + 67*pow(z,2) - 157*pow(z,3)) + 2*pow(Nc,2)*nf*(4 + 37*z - 79*pow(z,2) - 37*pow(z,3) + 75*pow(z,4)) + 8*pow(Nc,5)*(69 + 23*z - 53*pow(z,2) - 23*pow(z,3) + 53*pow(z,4) + 13*pow(z,5)))*z2*HPL(0,1,z))/(8.*pow(Nc,2)*z*(-1 + pow(z,2))) - (pow(Nc,3)*(47 + 94*z + 141*pow(z,2) + 88*pow(z,3) + 44*pow(z,4))*z2*HPL(1,-1,z))/(z*(1 + z)) + ((-8*Nc*pow(nf,2)*z*(35 + 53*z - 4*pow(z,2))*(-1 + pow(z,2)) + 8*pow(Nc,3)*pow(nf,2)*(1 + z)*(4 - 53*z + 3*pow(z,2) + 48*pow(z,3)) + 4*pow(Nc,5)*(24057 + 1899*z - 17271*pow(z,2) + 16927*pow(z,3) - 6665*pow(z,4) - 18705*pow(z,5) + 144*(-10 + 41*z + 127*pow(z,2) - 42*pow(z,3) - 129*pow(z,4) + 16*pow(z,5))*z2) - nf*(-1 + pow(z,2))*(-268 + 24*pow(z,3) + pow(z,2)*(462 - 468*z2) - 3*z*(73 + 156*z2)) - pow(Nc,4)*nf*(1 + z)*(6068 - 2252*pow(z,4) - z*(8461 + 324*z2) - 6*pow(z,3)*(-648 + 1290*z2) + 3*pow(z,2)*(311 + 2688*z2)) + 2*pow(Nc,2)*nf*(-1 + pow(z,2))*(-1598 - 1014*pow(z,3) - z*(2299 + 288*z2) - pow(z,2)*(1777 + 4320*z2)))*HPL(1,0,z))/(288.*pow(Nc,2)*z*(-1 + pow(z,2))) - ((23*nf*z*pow(1 + z,2) + 169*pow(Nc,4)*nf*z*pow(1 + z,2) - 4*pow(Nc,2)*nf*(-2 + 48*z + 97*pow(z,2) + 47*pow(z,3)) + 16*pow(Nc,5)*(7 - 180*z - 365*pow(z,2) - 179*pow(z,3) + 5*pow(z,4)))*z2*HPL(1,1,z))/(8.*pow(Nc,2)*z*(1 + z)) + ((-2*nf*pow(1 + z,2)*(1 - 4*z + pow(z,2)) + pow(Nc,2)*nf*pow(1 + z,2)*(4 + 5*z + 4*pow(z,2)) - 2*pow(Nc,4)*nf*(7 + 7*z + 3*pow(z,2) + pow(z,3) + 4*pow(z,4)) + pow(Nc,5)*(400 + 838*z + 735*pow(z,2) + 440*pow(z,3) + 209*pow(z,4)))*HPL(-1,-1,0,z))/(6.*pow(Nc,2)*z*(1 + z)) - ((-2*nf*pow(1 + z,2)*(1 - 4*z + pow(z,2)) + pow(Nc,4)*nf*(-14 + 21*z + 84*pow(z,2) + 41*pow(z,3) - 4*pow(z,4)) + 26*pow(Nc,2)*nf*(1 + z + pow(z,3) + pow(z,4)) + pow(Nc,5)*(995 + 1962*z + 1545*pow(z,2) + 1260*pow(z,3) + 660*pow(z,4)))*HPL(-1,0,0,z))/(12.*pow(Nc,2)*z*(1 + z)) + ((10*nf*(1 + z + pow(z,3) + pow(z,4)) - 2*pow(Nc,2)*nf*(8 + 9*z + 6*pow(z,2) + 9*pow(z,3) + 8*pow(z,4)) + pow(Nc,3)*(457 + 888*z + 840*pow(z,2) + 772*pow(z,3) + 407*pow(z,4)))*HPL(-1,1,0,z))/(6.*z*(1 + z)) + ((-(pow(Nc,2)*nf*(-1 + pow(z,2))*(4 + 9*z + 3*pow(z,2) + 16*pow(z,3))) + nf*z*(9 + 6*z - 17*pow(z,2) - 6*pow(z,3) + 8*pow(z,4)) + pow(Nc,4)*nf*z*(41 - 6*z - 61*pow(z,2) + 6*pow(z,3) + 12*pow(z,4)) - 2*pow(Nc,5)*(-105 - 191*z + 24*pow(z,2) - 150*pow(z,3) + 81*pow(z,4) + 319*pow(z,5)))*HPL(0,-1,0,z))/(12.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((28*Nc*pow(nf,2)*(-1 + z)*z*pow(1 + z,2) - 28*pow(Nc,3)*pow(nf,2)*(-1 + z)*z*pow(1 + z,2) - pow(Nc,2)*nf*(-1 + pow(z,2))*(80 + 376*z - 275*pow(z,2) + 16*pow(z,3)) + nf*(-8 - 21*z + 29*pow(z,2) + 13*pow(z,3) - 21*pow(z,4) + 8*pow(z,5)) - pow(Nc,4)*nf*(152 + 960*z + 145*pow(z,2) - 964*pow(z,3) - 217*pow(z,4) + 36*pow(z,5)) + 2*pow(Nc,5)*(1678 + 1360*z + 333*pow(z,2) - 1635*pow(z,3) - 1791*pow(z,4) + 363*pow(z,5)))*HPL(0,1,0,z))/(24.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((10*nf*(1 + z + pow(z,3) + pow(z,4)) - 2*pow(Nc,2)*nf*(8 + 9*z + 6*pow(z,2) + 9*pow(z,3) + 8*pow(z,4)) + pow(Nc,3)*(457 + 888*z + 840*pow(z,2) + 772*pow(z,3) + 407*pow(z,4)))*HPL(1,-1,0,z))/(6.*z*(1 + z)) + ((-96*pow(Nc,3)*pow(nf,2)*(-1 + z)*z*pow(1 + z,2) + 3*nf*(-1 + pow(z,2))*(4 - 21*z - 35*pow(z,2) + 20*pow(z,3)) - 2*pow(Nc,2)*nf*(-1 + pow(z,2))*(-76 + 415*z - 341*pow(z,2) + 32*pow(z,3)) + 4*Nc*pow(nf,2)*(2 - 26*z - 25*pow(z,2) + 26*pow(z,3) + 23*pow(z,4)) + pow(Nc,4)*nf*(12 - 2393*z - 589*pow(z,2) + 2389*pow(z,3) + 561*pow(z,4) + 52*pow(z,5)) - 8*pow(Nc,5)*(-600 - 898*z + 622*pow(z,2) + 194*pow(z,3) - 33*pow(z,4) + 737*pow(z,5)))*HPL(1,0,0,z))/(48.*pow(Nc,2)*z*(-1 + pow(z,2))) - ((32*pow(Nc,3)*pow(nf,2)*z*(-1 + pow(z,2)) - 4*Nc*pow(nf,2)*(2 - 12*z + 3*pow(z,2) + 7*pow(z,3)) + 2*pow(Nc,2)*nf*(-1 + z)*(-52 + 329*z + 413*pow(z,2) + 12*pow(z,3)) + nf*(-1 + z)*(40 - 69*z - 129*pow(z,2) + 20*pow(z,3)) + pow(Nc,4)*nf*(64 + 1433*z + 66*pow(z,2) - 1503*pow(z,3) - 68*pow(z,4)) - 4*pow(Nc,5)*(-1477 + 4406*z - 75*pow(z,2) - 3888*pow(z,3) + 1023*pow(z,4)))*HPL(1,1,0,z))/(48.*pow(Nc,2)*(-1 + z)*z) - (12*pow(Nc,3)*(4 + 8*z + 12*pow(z,2) + 6*pow(z,3) + 3*pow(z,4))*HPL(-1,-1,-1,0,z))/(z*(1 + z)) + (pow(Nc,3)*(61 + 122*z + 183*pow(z,2) + 100*pow(z,3) + 50*pow(z,4))*HPL(-1,-1,0,0,z))/(z*(1 + z)) - (2*pow(Nc,3)*(23 + 46*z + 69*pow(z,2) + 40*pow(z,3) + 20*pow(z,4))*HPL(-1,-1,1,0,z))/(z*(1 + z)) + (2*pow(Nc,3)*(14 + 28*z + 42*pow(z,2) + 22*pow(z,3) + 11*pow(z,4))*HPL(-1,0,-1,0,z))/(z*(1 + z)) - (pow(Nc,3)*(29 + 58*z + 87*pow(z,2) + 48*pow(z,3) + 24*pow(z,4))*HPL(-1,0,0,0,z))/(z*(1 + z)) + (pow(Nc,3)*(29 + 58*z + 87*pow(z,2) + 48*pow(z,3) + 24*pow(z,4))*HPL(-1,0,1,0,z))/(z*(1 + z)) - (2*pow(Nc,3)*(23 + 46*z + 69*pow(z,2) + 40*pow(z,3) + 20*pow(z,4))*HPL(-1,1,-1,0,z))/(z*(1 + z)) + (pow(Nc,3)*(41 + 82*z + 123*pow(z,2) + 72*pow(z,3) + 36*pow(z,4))*HPL(-1,1,0,0,z))/(z*(1 + z)) - (24*pow(Nc,3)*pow(1 + z + pow(z,2),2)*HPL(-1,1,1,0,z))/(z*(1 + z)) + ((-(nf*pow(-1 + z,2)*z*(1 + z)) - 2*pow(Nc,4)*nf*pow(-1 + z,2)*z*(1 + z) + 4*pow(Nc,5)*(-6 - z - 12*pow(z,2) + pow(z,3) + 10*pow(z,4) + 9*pow(z,5)))*HPL(0,-1,-1,0,z))/(pow(Nc,2)*z*(-1 + pow(z,2))) + ((nf*pow(-1 + z,2)*z*(1 + z) - 5*pow(Nc,2)*nf*pow(-1 + z,2)*z*(1 + z) + 7*pow(Nc,4)*nf*pow(-1 + z,2)*z*(1 + z) - 2*pow(Nc,5)*(-30 + 6*z - 70*pow(z,2) - 6*pow(z,3) + 63*pow(z,4) + 41*pow(z,5)))*HPL(0,-1,0,0,z))/(2.*pow(Nc,2)*z*(-1 + pow(z,2))) + (2*(nf*pow(-1 + z,2)*z*(1 + z) - pow(Nc,2)*nf*pow(-1 + z,2)*z*(1 + z) + pow(Nc,3)*(-12 + 2*z - 28*pow(z,2) - 2*pow(z,3) + 27*pow(z,4) + 15*pow(z,5)))*HPL(0,-1,1,0,z))/(z*(-1 + pow(z,2))) - ((2*pow(Nc,2)*nf*pow(-1 + z,2)*z*(1 + z) + 2*nf*z*(-1 + pow(z,2)) + pow(Nc,4)*nf*z*(-5 + 3*z + 5*pow(z,2) - 3*pow(z,3)) + 4*pow(Nc,5)*(-4 + 7*z - 13*pow(z,2) - 7*pow(z,3) + 12*pow(z,4) + 7*pow(z,5)))*HPL(0,0,-1,0,z))/(2.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((nf*(-1 + z)*z*pow(1 + z,2) + pow(Nc,4)*nf*z*(-47 + 17*z + 47*pow(z,2) - 17*pow(z,3)) + 2*pow(Nc,2)*nf*z*(21 - 11*z - 21*pow(z,2) + 11*pow(z,3)) + 8*pow(Nc,5)*(44 + 15*z + 75*pow(z,2) - 15*pow(z,3) - 76*pow(z,4) + 21*pow(z,5)))*HPL(0,0,1,0,z))/(8.*pow(Nc,2)*z*(-1 + pow(z,2))) + (2*(nf*pow(-1 + z,2)*z*(1 + z) - pow(Nc,2)*nf*pow(-1 + z,2)*z*(1 + z) + pow(Nc,3)*(-12 + 2*z - 28*pow(z,2) - 2*pow(z,3) + 27*pow(z,4) + 15*pow(z,5)))*HPL(0,1,-1,0,z))/(z*(-1 + pow(z,2))) + ((-9*nf*(-1 + z)*z*pow(1 + z,2) + pow(Nc,4)*nf*z*(-99 + 149*z + 99*pow(z,2) - 149*pow(z,3)) + 2*pow(Nc,2)*nf*(-4 + 41*z - 79*pow(z,2) - 41*pow(z,3) + 83*pow(z,4)) + 8*pow(Nc,5)*(43 + 35*z - 73*pow(z,2) - 35*pow(z,3) + 76*pow(z,4) + 10*pow(z,5)))*HPL(0,1,0,0,z))/(8.*pow(Nc,2)*z*(-1 + pow(z,2))) - ((-(nf*(-1 + z)*z*pow(1 + z,2)) - 15*pow(Nc,4)*nf*z*(5 - 11*z - 5*pow(z,2) + 11*pow(z,3)) + 2*pow(Nc,2)*nf*(4 + 41*z - 83*pow(z,2) - 41*pow(z,3) + 79*pow(z,4)) + 8*pow(Nc,5)*(57 + 25*z - 81*pow(z,2) - 25*pow(z,3) + 80*pow(z,4) + 28*pow(z,5)))*HPL(0,1,1,0,z))/(8.*pow(Nc,2)*z*(-1 + pow(z,2))) - (2*pow(Nc,3)*(23 + 46*z + 69*pow(z,2) + 40*pow(z,3) + 20*pow(z,4))*HPL(1,-1,-1,0,z))/(z*(1 + z)) + (pow(Nc,3)*(41 + 82*z + 123*pow(z,2) + 72*pow(z,3) + 36*pow(z,4))*HPL(1,-1,0,0,z))/(z*(1 + z)) - (24*pow(Nc,3)*pow(1 + z + pow(z,2),2)*HPL(1,-1,1,0,z))/(z*(1 + z)) + (2*(nf*z*(-1 + pow(z,2)) + pow(Nc,2)*nf*(z - pow(z,3)) + pow(Nc,3)*(13 + 10*z + 39*pow(z,2) + 42*pow(z,3) + 14*pow(z,4)))*HPL(1,0,-1,0,z))/(z*(1 + z)) + ((-6*nf*(-1 + z)*z*pow(1 + z,2) + pow(Nc,4)*nf*z*(-21 + 103*z + 21*pow(z,2) - 103*pow(z,3)) + 2*pow(Nc,2)*nf*(-2 + 8*z - 55*pow(z,2) - 8*pow(z,3) + 57*pow(z,4)) - 4*pow(Nc,5)*(-10 + 53*z + 223*pow(z,2) - 57*pow(z,3) - 225*pow(z,4) + 24*pow(z,5)))*HPL(1,0,0,0,z))/(4.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((3*nf*(-1 + z)*z*pow(1 + z,2) - 24*pow(Nc,2)*nf*z*(3 - 7*z - 3*pow(z,2) + 7*pow(z,3)) + 15*pow(Nc,4)*nf*z*(5 - 11*z - 5*pow(z,2) + 11*pow(z,3)) + 8*pow(Nc,5)*(-8 - 68*z + 136*pow(z,2) + 66*pow(z,3) - 139*pow(z,4) + 19*pow(z,5)))*HPL(1,0,1,0,z))/(8.*pow(Nc,2)*z*(-1 + pow(z,2))) - (24*pow(Nc,3)*pow(1 + z + pow(z,2),2)*HPL(1,1,-1,0,z))/(z*(1 + z)) + ((19*nf*(-1 + z)*z*pow(1 + z,2) - 6*pow(Nc,2)*nf*z*(-9 - 49*z + 9*pow(z,2) + 49*pow(z,3)) + 5*pow(Nc,4)*nf*z*(-7 - 55*z + 7*pow(z,2) + 55*pow(z,3)) + 8*pow(Nc,5)*(-11 + 191*z + 385*pow(z,2) - 193*pow(z,3) - 387*pow(z,4) + 13*pow(z,5)))*HPL(1,1,0,0,z))/(8.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((-23*nf*z*(1 + z) - 169*pow(Nc,4)*nf*z*(1 + z) + 4*pow(Nc,2)*nf*(-2 + 50*z + 47*pow(z,2)) + 16*pow(Nc,5)*(-1 + 193*z + 190*pow(z,2) + pow(z,3)))*HPL(1,1,1,0,z))/(8.*pow(Nc,2)*z) + (20*pow(Nc,3)*(-1 + 2*z - pow(z,2) + pow(z,3))*pow(log(1 - z),4))/z + ((8*Nc*pow(nf,2)*(-1 + pow(z,2))*(116 - 236*pow(z,3) + pow(z,2)*(45 - 72*z2) - 12*z*(-31 + 6*z2)) + 4*pow(Nc,3)*pow(nf,2)*(1 + z)*(460 + 27*pow(z,2) + 708*pow(z,4) - 8*z*(-16 + 18*z2) + pow(z,3)*(-1243 + 144*z2)) + 2*pow(Nc,2)*nf*(1 + z)*(8826 - 50679*z + 69510*pow(z,2) - 61517*pow(z,3) + 34184*pow(z,4) - 36*(48 - 64*z + 513*pow(z,2) - 609*pow(z,3) + 112*pow(z,4))*z2 + 1728*z3 - 1728*z*z3 - 11448*pow(z,2)*z3 + 11448*pow(z,3)*z3) - 4*pow(Nc,5)*(-125239 + 275513*z - 101817*pow(z,2) + 37133*pow(z,3) + 218932*pow(z,4) - 320770*pow(z,5) + 36*(494 - 1308*z + 2759*pow(z,2) - 1189*pow(z,3) - 3154*pow(z,4) + 2816*pow(z,5))*z2 + 1728*z3 + 11664*z*z3 + 37152*pow(z,2)*z3 - 11664*pow(z,3)*z3 - 34560*pow(z,4)*z3 + 28944*pow(z,5)*z3) + nf*(-1 + pow(z,2))*(428 - 4748*pow(z,3) + 72*(-4 - 24*z - 63*pow(z,2) + 40*pow(z,3))*z2 + 9*z*(71 - 216*z3) - 3*pow(z,2)*(-389 + 72*z3)) + pow(Nc,4)*nf*(72*(84 - 61*z + 757*pow(z,2) + pow(z,3) - 805*pow(z,4) + 176*pow(z,5))*z2 - (1 + z)*(76208 + 181580*pow(z,4) - 36*pow(z,2)*(-9127 + 540*z3) - z*(233185 + 1512*z3) + pow(z,3)*(-344239 + 20952*z3))))/(1728.*pow(Nc,2)*z*(-1 + pow(z,2))) - (2*pow(Nc,2)*(11*Nc - 2*nf)*pow(1 + z + pow(z,2),2)*HPL(-1,0,z))/(3.*z*(1 + z)) + (2*(176*pow(Nc,4) + 11*Nc*nf - 43*pow(Nc,3)*nf - 2*pow(nf,2) + 2*pow(Nc,2)*pow(nf,2))*(1 + z)*HPL(1,0,z))/(3.*Nc))*log(z) + ((-16*Nc*pow(nf,2)*(-1 + pow(z,2))*(-8 - 50*z - 41*pow(z,2) + 12*pow(z,3)) + 16*pow(Nc,3)*pow(nf,2)*(1 + z)*(12 + 32*z + 12*pow(z,2) - 70*pow(z,3) + 16*pow(z,4)) + nf*z*(-1 + pow(z,2))*(573 + 1971*z - 304*pow(z,2) - 792*(1 + z)*z2) + 8*pow(Nc,5)*(10292 - 35134*z + 15203*pow(z,2) - 4791*pow(z,3) - 24302*pow(z,4) + 41654*pow(z,5) - 72*(12 - 5*z + 139*pow(z,2) + 5*pow(z,3) - 139*pow(z,4) + 50*pow(z,5))*z2) + 2*pow(Nc,2)*nf*(-1 + pow(z,2))*(-1584 + 7224*pow(z,3) + z*(9293 - 1080*z2) + pow(z,2)*(-3937 + 5256*z2)) + pow(Nc,4)*nf*(-9984 - 25568*pow(z,5) + pow(z,4)*(29863 - 9000*z2) + z*(22743 - 3384*z2) + pow(z,3)*(553 + 3384*z2) + pow(z,2)*(-21511 + 9000*z2)))*pow(log(z),2))/(1152.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((24*Nc*pow(nf,2)*(-1 + z)*z*pow(1 + z,2) - 24*pow(Nc,3)*pow(nf,2)*(-1 + z)*z*pow(1 + z,2) + nf*z*(-3 + 114*z - 68*pow(z,2))*(-1 + pow(z,2)) + 2*pow(Nc,2)*nf*(-1 + pow(z,2))*(-40 + 3*z - 516*pow(z,2) + 60*pow(z,3)) + pow(Nc,4)*nf*(-176 + 35*z - 1482*pow(z,2) + 33*pow(z,3) + 1562*pow(z,4) - 228*pow(z,5)) + 8*pow(Nc,5)*(286 - 331*z + 1158*pow(z,2) - 538*pow(z,3) - 1378*pow(z,4) + 979*pow(z,5)))*pow(log(z),3))/(288.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((11*nf*(-1 + z)*z*pow(1 + z,2) + 11*pow(Nc,4)*nf*z*(11 - 21*z - 11*pow(z,2) + 21*pow(z,3)) - 10*pow(Nc,2)*nf*z*(11 - 25*z - 11*pow(z,2) + 25*pow(z,3)) + 32*pow(Nc,5)*(4 - 11*z + 55*pow(z,2) + 11*pow(z,3) - 54*pow(z,4) + 13*pow(z,5)))*pow(log(z),4))/(384.*pow(Nc,2)*z*(-1 + pow(z,2))) + pow(log(1 - z),3)*(-(pow(Nc,4)*nf*(1956 - 2053*z + 773*pow(z,2) - 1956*pow(z,3)) + 23*nf*(4 + 3*z - 3*pow(z,2) - 4*pow(z,3)) + 192*pow(Nc,2)*nf*(-4 - 3*z + 3*pow(z,2) + 4*pow(z,3)) + 64*pow(Nc,5)*(-671 + 751*z - 641*pow(z,2) + 671*pow(z,3)))/(288.*pow(Nc,2)*z) - ((23*nf*z*(-1 + pow(z,2)) - 192*pow(Nc,2)*nf*z*(-1 + pow(z,2)) + 169*pow(Nc,4)*nf*z*(-1 + pow(z,2)) + 48*pow(Nc,5)*(51 - 38*z + 153*pow(z,2) - 166*pow(z,3) + 51*pow(z,4)))*log(z))/(48.*pow(Nc,2)*(-1 + z)*z)) + pow(log(1 - z),2)*((-16*Nc*pow(nf,2)*(-4 - 7*z + 7*pow(z,3) + 4*pow(z,4)) + 16*pow(Nc,3)*pow(nf,2)*(-8 - 3*z + 4*pow(z,2) + 7*pow(z,3) + 8*pow(z,4)) + 16*pow(Nc,5)*(-9923 + 438*z + 922*pow(z,2) + 484*pow(z,3) + 9923*pow(z,4) + 108*(13 + 16*z + 47*pow(z,2) + 30*pow(z,3) - 15*pow(z,4))*z2) + 2*pow(Nc,2)*nf*(1 + z)*(-2477 + 2909*pow(z,3) + 6*pow(z,2)*(-233 + 288*z2) + 6*z*(161 + 288*z2)) - nf*(1 + z)*(-278 + 404*pow(z,3) + z*(-543 + 414*z2) + pow(z,2)*(417 + 414*z2)) - pow(Nc,4)*nf*(1 + z)*(-13476 + 14214*pow(z,3) + pow(z,2)*(-12317 + 3042*z2) + z*(14155 + 3042*z2)))/(288.*pow(Nc,2)*z*(1 + z)) - (12*pow(Nc,3)*pow(1 + z + pow(z,2),2)*HPL(-1,0,z))/(z*(1 + z)) + ((3072*pow(Nc,5) - 23*nf + 192*pow(Nc,2)*nf - 169*pow(Nc,4)*nf)*(1 + z)*HPL(1,0,z))/(16.*pow(Nc,2)) + ((32*Nc*pow(nf,2)*z*(-1 + pow(z,2)) - 32*pow(Nc,3)*pow(nf,2)*z*(-1 + pow(z,2)) + nf*(-1 + z)*(32 + 123*z + 75*pow(z,2) - 92*pow(z,3)) + 2*pow(Nc,2)*nf*(-1 + z)*(-288 - 475*z - 214*pow(z,2) + 252*pow(z,3)) + pow(Nc,4)*nf*(-1296 + 261*z - 1782*pow(z,2) + 3229*pow(z,3) - 1164*pow(z,4)) + 8*pow(Nc,5)*(2783 - 4590*z + 7035*pow(z,2) - 8726*pow(z,3) + 4015*pow(z,4)))*log(z))/(96.*pow(Nc,2)*(-1 + z)*z) + ((19*nf*(-1 + z)*z*pow(1 + z,2) - 6*pow(Nc,2)*nf*z*(-9 - 49*z + 9*pow(z,2) + 49*pow(z,3)) + 5*pow(Nc,4)*nf*z*(-7 - 55*z + 7*pow(z,2) + 55*pow(z,3)) + 48*pow(Nc,5)*(23 + 7*z + 89*pow(z,2) - 7*pow(z,3) - 89*pow(z,4) + 27*pow(z,5)))*pow(log(z),2))/(32.*pow(Nc,2)*z*(-1 + pow(z,2)))) + log(1 - z)*((16*Nc*pow(nf,2)*(-1 + pow(z,2))*(-50 + 59*pow(z,3) + 6*pow(z,2)*(12 + 6*z2) + z*(-81 + 36*z2)) - 2*pow(Nc,3)*pow(nf,2)*(-1 + pow(z,2))*(-804 + 876*pow(z,3) + pow(z,2)*(29 + 288*z2) + z*(47 + 288*z2)) + 3*nf*(-1 + pow(z,2))*(-386 - 860*z - 82*pow(z,2) + 1328*pow(z,3) + 6*(16 + 93*z + 105*pow(z,2) - 76*pow(z,3))*z2 + 360*z*z3 + 360*pow(z,2)*z3) - 6*pow(Nc,2)*nf*(-1 + pow(z,2))*(-3269 + 5412*z - 8577*pow(z,2) + 6542*pow(z,3) + 6*(168 + 400*z + 277*pow(z,2) - 172*pow(z,3))*z2 - 288*z3 + 612*z*z3 + 3636*pow(z,2)*z3) + 12*pow(Nc,5)*(-56807 + 62199*z - 3721*pow(z,2) - 4378*pow(z,3) + 60528*pow(z,4) - 57821*pow(z,5) + 12*(1039 - 848*z + 2018*pow(z,2) - 1880*pow(z,3) - 3090*pow(z,4) + 2739*pow(z,5))*z2 + 3384*z3 - 13320*z*z3 + 6840*pow(z,2)*z3 + 360*pow(z,3)*z3 - 19512*pow(z,4)*z3 + 10152*pow(z,5)*z3) + pow(Nc,4)*nf*(-18*(656 + 823*z + 1125*pow(z,2) - 1459*pow(z,3) - 1829*pow(z,4) + 652*pow(z,5))*z2 + 2*(-1 + pow(z,2))*(-45004 + 52996*pow(z,3) + z*(60017 + 432*z3) + pow(z,2)*(-63691 + 10800*z3))))/(864.*pow(Nc,2)*z*(-1 + pow(z,2))) - (pow(Nc,3)*(47 + 94*z + 141*pow(z,2) + 88*pow(z,3) + 44*pow(z,4))*z2*HPL(-1,z))/(z*(1 + z)) - ((23*nf*(-1 + z)*z*pow(1 + z,2) + 169*pow(Nc,4)*nf*(-1 + z)*z*pow(1 + z,2) - 4*pow(Nc,2)*nf*(2 - 50*z - 49*pow(z,2) + 50*pow(z,3) + 47*pow(z,4)) + 8*pow(Nc,5)*(-15 + 375*z + 369*pow(z,2) - 373*pow(z,3) - 367*pow(z,4) + 9*pow(z,5)))*z2*HPL(1,z))/(8.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((10*nf*(1 + z + pow(z,3) + pow(z,4)) - 2*pow(Nc,2)*nf*(8 + 9*z + 6*pow(z,2) + 9*pow(z,3) + 8*pow(z,4)) + pow(Nc,3)*(457 + 888*z + 840*pow(z,2) + 772*pow(z,3) + 407*pow(z,4)))*HPL(-1,0,z))/(6.*z*(1 + z)) - ((-32*Nc*pow(nf,2)*z*(-1 + pow(z,2)) + 32*pow(Nc,3)*pow(nf,2)*z*(-1 + pow(z,2)) + 2*pow(Nc,2)*nf*(-1 + z)*(36 + 262*z + 415*pow(z,2)) + nf*(-1 + z)*(32 - 75*z - 123*pow(z,2) + 28*pow(z,3)) + pow(Nc,4)*nf*(72 + 1417*z + 90*pow(z,2) - 1535*pow(z,3) - 60*pow(z,4)) - 8*pow(Nc,5)*(-685 + 2114*z + 57*pow(z,2) - 2080*pow(z,3) + 583*pow(z,4)))*HPL(1,0,z))/(48.*pow(Nc,2)*(-1 + z)*z) - (2*pow(Nc,3)*(23 + 46*z + 69*pow(z,2) + 40*pow(z,3) + 20*pow(z,4))*HPL(-1,-1,0,z))/(z*(1 + z)) + (pow(Nc,3)*(41 + 82*z + 123*pow(z,2) + 72*pow(z,3) + 36*pow(z,4))*HPL(-1,0,0,z))/(z*(1 + z)) - (24*pow(Nc,3)*pow(1 + z + pow(z,2),2)*HPL(-1,1,0,z))/(z*(1 + z)) + (2*(nf*pow(-1 + z,2)*z*(1 + z) - pow(Nc,2)*nf*pow(-1 + z,2)*z*(1 + z) + pow(Nc,3)*(-12 + 2*z - 28*pow(z,2) - 2*pow(z,3) + 27*pow(z,4) + 15*pow(z,5)))*HPL(0,-1,0,z))/(z*(-1 + pow(z,2))) - ((-3*nf*(-1 + z)*z*pow(1 + z,2) + 24*pow(Nc,2)*nf*z*(3 - 7*z - 3*pow(z,2) + 7*pow(z,3)) - 15*pow(Nc,4)*nf*z*(5 - 11*z - 5*pow(z,2) + 11*pow(z,3)) + 8*pow(Nc,5)*(54 + 22*z - 90*pow(z,2) - 22*pow(z,3) + 91*pow(z,4) + 27*pow(z,5)))*HPL(0,1,0,z))/(8.*pow(Nc,2)*z*(-1 + pow(z,2))) - (24*pow(Nc,3)*pow(1 + z + pow(z,2),2)*HPL(1,-1,0,z))/(z*(1 + z)) + ((19*nf*(-1 + z)*z*pow(1 + z,2) + 5*pow(Nc,4)*nf*z*(-7 - 55*z + 7*pow(z,2) + 55*pow(z,3)) + 2*pow(Nc,2)*nf*(4 + 23*z + 145*pow(z,2) - 23*pow(z,3) - 149*pow(z,4)) + 32*pow(Nc,5)*(-1 + 46*z + 98*pow(z,2) - 47*pow(z,3) - 99*pow(z,4) + 5*pow(z,5)))*HPL(1,0,0,z))/(8.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((-23*nf*z*(-1 + pow(z,2)) - 169*pow(Nc,4)*nf*z*(-1 + pow(z,2)) + 4*pow(Nc,2)*nf*(2 - 52*z + 3*pow(z,2) + 47*pow(z,3)) + 8*pow(Nc,5)*(3 - 390*z + 9*pow(z,2) + 376*pow(z,3) + 3*pow(z,4)))*HPL(1,1,0,z))/(8.*pow(Nc,2)*(-1 + z)*z) + ((8*Nc*pow(nf,2)*(-1 + pow(z,2))*(-12 - 50*z - 41*pow(z,2) + 16*pow(z,3)) - 8*pow(Nc,3)*pow(nf,2)*(1 + z)*(20 + 26*z + 15*pow(z,2) - 77*pow(z,3) + 24*pow(z,4)) - 8*pow(Nc,5)*(14987 - 26842*z + 8910*pow(z,2) - 6544*pow(z,3) - 21779*pow(z,4) + 35504*pow(z,5) - 36*(47 + 15*z + 321*pow(z,2) - 15*pow(z,3) - 323*pow(z,4) + 99*pow(z,5))*z2) + 4*pow(Nc,2)*nf*(-1 + pow(z,2))*(924 - 2563*pow(z,3) + pow(z,2)*(514 - 2160*z2) - 2*z*(1093 + 72*z2)) + nf*(-1 + pow(z,2))*(-136 + 488*pow(z,3) + 6*z*(-95 + 78*z2) + 3*pow(z,2)*(73 + 156*z2)) + pow(Nc,4)*nf*(1 + z)*(12488 + 21492*pow(z,4) + z*(-26606 + 324*z2) - 21*pow(z,2)*(-1721 + 384*z2) + pow(z,3)*(-40571 + 7740*z2)))*log(z))/(288.*pow(Nc,2)*z*(-1 + pow(z,2))) + ((-16*Nc*pow(nf,2)*(-1 + z)*z*pow(1 + z,2) + 16*pow(Nc,3)*pow(nf,2)*(-1 + z)*z*pow(1 + z,2) + nf*(-1 + pow(z,2))*(-4 - 30*z - 54*pow(z,2) + 40*pow(z,3)) - pow(Nc,2)*nf*(-1 + pow(z,2))*(-120 - 136*z - 571*pow(z,2) + 152*pow(z,3)) + pow(Nc,4)*nf*(276 + 109*z + 1038*pow(z,2) - 217*pow(z,3) - 1158*pow(z,4) + 296*pow(z,5)) - 2*pow(Nc,5)*(1980 - 1860*z + 4051*pow(z,2) - 2133*pow(z,3) - 5602*pow(z,4) + 4510*pow(z,5)))*pow(log(z),2))/(48.*pow(Nc,2)*z*(-1 + pow(z,2))) - ((6*nf*(-1 + z)*z*pow(1 + z,2) - 4*pow(Nc,2)*nf*z*(3 - 28*z - 3*pow(z,2) + 28*pow(z,3)) + pow(Nc,4)*nf*z*(21 - 103*z - 21*pow(z,2) + 103*pow(z,3)) + 4*pow(Nc,5)*(40 + 3*z + 273*pow(z,2) - 3*pow(z,3) - 271*pow(z,4) + 74*pow(z,5)))*pow(log(z),3))/(24.*pow(Nc,2)*z*(-1 + pow(z,2))));
        
        return real(L3)*pow(L,3)+real(L2)*pow(L,2)+real(L1)*L;
    }
    
    double LEqgN3LOregFalko(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double z2= consts::z2;
        const double z3= consts::z3;
        const double z4= consts::z4;
        if (abs(L)<1e-15) return 0.0;

        complex<double> L3=((-1 + pow(Nc,2))*(4*Nc*nf*(-496 + 1140*z - 663*pow(z,2) + 64*pow(z,3)) - 4*pow(Nc,3)*nf*(-2636 + 2916*z - 771*pow(z,2) + 272*pow(z,3)) + 3*z*(6 - 33*z + 48*(-2 + z)*z2) - 12*pow(Nc,2)*(8*pow(nf,2)*(2 - 2*z + pow(z,2)) + 3*(-40 + 43*z + 20*pow(z,2)) + 24*(8 + 2*z + 11*pow(z,2))*z2) + pow(Nc,4)*(-129064 + 130890*z - 21165*pow(z,2) + 17344*pow(z,3) + 144*(128 + 118*z + 133*pow(z,2))*z2)))/(13824.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-1 - 2*pow(Nc,2)*(-1 + 6*z + 3*pow(z,2)) + 3*pow(Nc,4)*(9 + 32*z + 16*pow(z,2)))*HPL(1,0,z))/(48.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(1 - 10*pow(Nc,2) + 37*pow(Nc,4))*(2 - 2*z + pow(z,2))*pow(log(1 - z),2))/(96.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(180*z - 72*pow(z,2) + 24*Nc*nf*(-32 + 10*z + 31*pow(z,2)) - 12*pow(Nc,2)*z*(476 - 43*z + 64*pow(z,2)) + 24*pow(Nc,3)*nf*(136 + 94*z + 73*pow(z,2)) + 12*pow(Nc,4)*(-4112 - 1019*z - 845*pow(z,2) + 288*pow(z,3)))*log(z))/(13824.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(6*(-2 + z)*z + 96*Nc*nf*(-2 + z)*z - 96*pow(Nc,3)*nf*(-2 + z)*z + 2*pow(Nc,2)*(-84*z - 102*pow(z,2)) + 6*pow(Nc,4)*(224 - 194*z + 481*pow(z,2)))*pow(log(z),2))/(4608.*pow(Nc,3)*z) + log(1 - z)*(-((-1 + pow(Nc,2))*(-3*(-4 + z)*z - 10*Nc*nf*(2 - 2*z + pow(z,2)) + 62*pow(Nc,3)*nf*(2 - 2*z + pow(z,2)) + pow(Nc,2)*(358 - 374*z + 49*pow(z,2) - 32*pow(z,3)) + 2*pow(Nc,4)*(-1333 + 1139*z - 82*pow(z,2) + 128*pow(z,3))))/(576.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(3*(-8*z + 4*pow(z,2)) - 24*pow(Nc,2)*(8 + 2*z + 11*pow(z,2)) + 12*pow(Nc,4)*(128 + 118*z + 133*pow(z,2)))*log(z))/(1152.*pow(Nc,3)*z));
        check_imaginary_part(L3,__PRETTY_FUNCTION__);
        complex<double> L2= -((-1 + pow(Nc,2))*(-12*pow(Nc,2)*(5041 - 3848*z + 140*pow(z,2) - 2476*pow(z,3) - 8*pow(nf,2)*(29 - 38*z + 19*pow(z,2)) + 24*(-202 + 297*z - 246*pow(z,2) + 36*pow(z,3))*z2 - 432*z3 - 288*z*z3 - 1944*pow(z,2)*z3) - 9*(120*pow(z,3) + 48*(-6 - 13*z + 20*pow(z,2) + 4*pow(z,3))*z2 + 24*(13 + 8*z3) + pow(z,2)*(293 + 48*z3) - 2*z*(439 + 240*z3)) + 4*pow(Nc,3)*nf*(-44290 + 4504*pow(z,3) + 72*(78 + 94*z + 31*pow(z,2))*z2 + 27*pow(z,2)*(-551 + 64*z3) - 6*z*(-8447 + 576*z3)) + 12*Nc*nf*(1446 - 312*pow(z,3) + 72*(-14 + 6*z + 9*pow(z,2))*z2 + pow(z,2)*(4723 - 576*z3) + 2*z*(-2987 + 576*z3)) + pow(Nc,4)*(1417364 - 93272*pow(z,3) + 144*(-2654 - 653*z - 602*pow(z,2) + 468*pow(z,3))*z2 - 82944*z3 - 198*z*(7397 + 144*z3) - 3*pow(z,2)*(-60223 + 68112*z3))))/(27648.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(1 - 4*pow(Nc,2) + 27*pow(Nc,4))*(2 + 2*z + pow(z,2))*z2*HPL(-1,z))/(32.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(-16 + 2*z - pow(z,2) - 2*pow(Nc,2)*(-10 + 70*z + 41*pow(z,2)) + pow(Nc,4)*(324 + 1266*z + 623*pow(z,2)))*z2*HPL(1,z))/(64.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(6 + 10*z + 4*pow(z,2) - 4*pow(Nc,3)*nf*(2 + 2*z + pow(z,2)) - pow(Nc,2)*(6 + 14*z + 5*pow(z,2)) + pow(Nc,4)*(292 + 240*z - pow(z,2) + 32*pow(z,3)))*HPL(-1,0,z))/(64.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(48*Nc*nf*(-2 + 3*pow(z,2)) + 8*pow(Nc,3)*nf*(38 + 54*z + 15*pow(z,2)) + pow(Nc,2)*(-940 + 672*z + 612*pow(z,2) - 16*pow(z,3)) - 3*(-18 + 18*z + 31*pow(z,2) + 8*pow(z,3)) + pow(Nc,4)*(-1378 - 6162*z - 1395*pow(z,2) + 296*pow(z,3)))*HPL(1,0,z))/(384.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(1 - 2*pow(Nc,2) + 13*pow(Nc,4))*(2 + 2*z + pow(z,2))*HPL(-1,-1,0,z))/(16.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(1 - 3*pow(Nc,2) + 20*pow(Nc,4))*(2 + 2*z + pow(z,2))*HPL(-1,0,0,z))/(32.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-3*pow(Nc,2)*(2 + 2*z + pow(z,2)) + 21*pow(Nc,4)*(2 + 2*z + pow(z,2)))*HPL(-1,1,0,z))/(48.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(6*z + 3*pow(z,2) + pow(Nc,2)*(-12*z - 6*pow(z,2)) + 3*pow(Nc,4)*(48 - 46*z + 49*pow(z,2)))*HPL(0,-1,0,z))/(96.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-2 + 2*z + 8*Nc*nf*(-2 + z)*z - 8*pow(Nc,3)*nf*(-2 + z)*z - pow(z,2) + pow(Nc,2)*(32 - 22*z + 7*pow(z,2)) + 2*pow(Nc,4)*(-31 - 74*z + 51*pow(z,2)))*HPL(0,1,0,z))/(32.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-3*pow(Nc,2)*(2 + 2*z + pow(z,2)) + 21*pow(Nc,4)*(2 + 2*z + pow(z,2)))*HPL(1,-1,0,z))/(48.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-8 - 2*z + 16*Nc*nf*(-2 + z)*z - 16*pow(Nc,3)*nf*(-2 + z)*z + pow(z,2) - 4*pow(Nc,2)*(-11 + 26*z + 13*pow(z,2)) + pow(Nc,4)*(212 + 354*z + 647*pow(z,2)))*HPL(1,0,0,z))/(64.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(-16 + 2*z - pow(z,2) - 4*pow(Nc,2)*(-4 + 36*z + 21*pow(z,2)) + pow(Nc,4)*(352 + 1294*z + 637*pow(z,2)))*HPL(1,1,0,z))/(64.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(3 - 26*pow(Nc,2) + 99*pow(Nc,4))*(2 - 2*z + pow(z,2))*pow(log(1 - z),3))/(64.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(1836*z - 675*pow(z,2) + 2232*pow(z,3) + 72*(-30*z + 9*pow(z,2))*z2 - 12*pow(Nc,2)*(-376 + 3783*z + 126*pow(z,2) + 500*pow(z,3) + 24*pow(nf,2)*(2 - 2*z + pow(z,2)) + 36*(16 + 22*z + 39*pow(z,2))*z2) + 3*pow(Nc,4)*(-118504 + 218568*z - 16071*pow(z,2) + 38248*pow(z,3) + 216*(64 - 30*z + 161*pow(z,2))*z2) - 12*pow(Nc,3)*nf*(-3164 + 400*pow(z,3) + z*(2694 - 576*z2) + 3*pow(z,2)*(-669 + 96*z2)) + 12*Nc*nf*(-344 + 112*pow(z,3) + z*(2310 - 576*z2) + pow(z,2)*(-651 + 288*z2)))*log(z))/(13824.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(9*z*(-18 + 29*z + 16*pow(z,2)) - 12*Nc*nf*(-32 + 42*z + 33*pow(z,2)) - 12*pow(Nc,3)*nf*(136 + 62*z + 95*pow(z,2)) + 2*pow(Nc,2)*(2664*z - 1710*pow(z,2) + 240*pow(z,3)) - 3*pow(Nc,4)*(-7168 + 394*z - 2669*pow(z,2) + 1296*pow(z,3)))*pow(log(z),2))/(4608.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(-3*z*(6 + z) - 6*pow(Nc,2)*z*(26 + 31*z) + 2*Nc*nf*(-96*z + 48*pow(z,2)) - 2*pow(Nc,3)*nf*(-96*z + 48*pow(z,2)) + 3*pow(Nc,4)*(224 - 358*z + 703*pow(z,2)))*pow(log(z),3))/(2304.*pow(Nc,3)*z) + pow(log(1 - z),2)*(((-1 + pow(Nc,2))*(-52*Nc*nf*(2 - 2*z + pow(z,2)) + 268*pow(Nc,3)*nf*(2 - 2*z + pow(z,2)) - 3*(36 - 76*z + 31*pow(z,2)) + pow(Nc,2)*(2976 - 3308*z + 754*pow(z,2) - 208*pow(z,3)) + pow(Nc,4)*(-17044 + 14768*z - 481*pow(z,2) + 1712*pow(z,3))))/(768.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(8 - 22*z + 11*pow(z,2) - 8*pow(Nc,2)*(21 - 5*z + 22*pow(z,2)) + pow(Nc,4)*(1040 + 606*z + 981*pow(z,2)))*log(z))/(128.*pow(Nc,3)*z)) + log(1 - z)*(-((-1 + pow(Nc,2))*(2*pow(Nc,3)*nf*(2609 - 3096*z + 1020*pow(z,2) - 224*pow(z,3)) + 2*Nc*nf*(-655 + 1332*z - 762*pow(z,2) + 64*pow(z,3)) + 3*(-23 + 12*z - 24*pow(z,2) + 62*pow(z,3) + 6*(4 - 18*z + 9*pow(z,2))*z2) - pow(Nc,2)*(-6739 + 5754*z + 318*pow(z,2) + 976*pow(z,3) + 24*pow(nf,2)*(2 - 2*z + pow(z,2)) + 36*(62 - 2*z + 77*pow(z,2))*z2) + pow(Nc,4)*(-80200 + 77142*z - 8466*pow(z,2) + 8662*pow(z,3) + 18*(736 + 854*z + 829*pow(z,2))*z2)))/(1152.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-3*pow(Nc,2)*(2 + 2*z + pow(z,2)) + 21*pow(Nc,4)*(2 + 2*z + pow(z,2)))*HPL(-1,0,z))/(48.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(-16 + 2*z - pow(z,2) - 4*pow(Nc,2)*(-4 + 36*z + 21*pow(z,2)) + pow(Nc,4)*(352 + 1294*z + 637*pow(z,2)))*HPL(1,0,z))/(64.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-36*Nc*nf*(-14 + 6*z + 9*pow(z,2)) - 12*pow(Nc,3)*nf*(154 + 30*z + 69*pow(z,2)) + 3*(-78*z + 144*pow(z,2) + 24*pow(z,3)) + 6*pow(Nc,2)*(-422 + 642*z - 519*pow(z,2) + 72*pow(z,3)) - 6*pow(Nc,4)*(-5722 + 1251*z - 819*pow(z,2) + 660*pow(z,3)))*log(z))/(1152.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(15*(-2 + z)*z + 24*Nc*nf*(-4*z + 2*pow(z,2)) - 8*pow(Nc,3)*nf*(-12*z + 6*pow(z,2)) - 6*pow(Nc,2)*(16 + 14*z + 45*pow(z,2)) + 3*pow(Nc,4)*(480 + 86*z + 781*pow(z,2)))*pow(log(z),2))/(384.*pow(Nc,3)*z));
        check_imaginary_part(L2,__PRETTY_FUNCTION__);
        complex<double> L1=((-1 + pow(Nc,2))*(4*pow(Nc,3)*nf*(360*(-1782 + 1204*z - 2333*pow(z,2) + 400*pow(z,3))*z2 - 5*(-946310 + 67400*pow(z,3) + z*(1107462 - 91584*z3) + 5184*z3 + 9*pow(z,2)*(-32399 + 1248*z3)) + 207360*(-2 + z)*z*z4) - 12*Nc*nf*(120*(-880 + 3924*z - 465*pow(z,2) + 192*pow(z,3))*z2 + 5*(34394 - 10856*pow(z,3) + pow(z,2)*(114477 - 8064*z3) + 3456*z3 + 6*z*(-22931 + 2112*z3)) + 1080*(68 - 60*z + 81*pow(z,2))*z4) + 3*(360*(-368 - 1366*z + 269*pow(z,2) + 88*pow(z,3))*z2 + 5*(360*(139 + 24*z3) + 384*pow(z,3)*(17 + 108*z3) - 2*z*(69139 + 28080*z3) + pow(z,2)*(72773 + 55296*z3)) - 1080*(160 - 274*z + 347*pow(z,2))*z4) + 4*pow(Nc,2)*(-240*pow(nf,2)*(253 - 406*z + 203*pow(z,2)) + 720*(-3260 + 5607*z - 1632*pow(z,2) + 827*pow(z,3))*z2 - 5*(-383783 + z*(229842 - 181872*z3) + 18576*z3 + 640*pow(z,3)*(365 + 54*z3) + 216*pow(z,2)*(-77 + 1867*z3)) + 3240*(186 + 215*z + 390*pow(z,2))*z4) - 3*pow(Nc,4)*(120*(-56072 + 277222*z - 12185*pow(z,2) + 59360*pow(z,3))*z2 + 5*(8502148 + pow(z,2)*(400589 - 598176*z3) - 148608*z3 + 192*pow(z,3)*(-2203 + 2640*z3) + z*(-8245630 + 2804832*z3)) + 1080*(1720 - 7814*z + 6479*pow(z,2))*z4)))/(829440.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(6*(65 + 135*z + 54*pow(z,2) - 16*pow(z,3) - 20*pow(Nc,3)*nf*(2 + 2*z + pow(z,2)) + pow(Nc,2)*(-131 - 195*z - 33*pow(z,2) + 40*pow(z,3)) + pow(Nc,4)*(2788 + 2590*z + 125*pow(z,2) + 264*pow(z,3)))*z2 + 36*(7 - 16*pow(Nc,2) + 129*pow(Nc,4))*(2 + 2*z + pow(z,2))*z3)*HPL(-1,z))/(576.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(6*(-182 + 51*z + 210*pow(z,2) + 56*pow(z,3) - 4*pow(Nc,3)*nf*(114 + 162*z + 19*pow(z,2)) - 8*Nc*nf*(-19 - 4*z + 32*pow(z,2)) + pow(Nc,4)*(-1934 + 10545*z + 2479*pow(z,2) - 904*pow(z,3)) - pow(Nc,2)*(-1816 + 2240*z + 2087*pow(z,2) + 32*pow(z,3)))*z2 + 18*(-60 + 14*z + 64*Nc*nf*(-2 + z)*z - 64*pow(Nc,3)*nf*(-2 + z)*z - 23*pow(z,2) - 4*pow(Nc,2)*(-47 + 54*z + 31*pow(z,2)) + pow(Nc,4)*(40 + 866*z + 1615*pow(z,2)))*z3)*HPL(1,z))/(1152.*pow(Nc,3)*z) - ((-8 + 23*pow(Nc,2) - 140*pow(Nc,4) + 125*pow(Nc,6))*(2 + 2*z + pow(z,2))*z2*HPL(-1,-1,z))/(16.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(-12*pow(Nc,3)*nf*(35 + 34*z + 9*pow(z,2)) - 4*Nc*nf*(43 + 18*z - 9*pow(z,2) + 16*pow(z,3)) + 3*(99 + 376*z + 289*pow(z,2) - 36*(2 + 2*z + pow(z,2))*z2) + 3*pow(Nc,2)*(29 - 12*z + 147*pow(z,2) + 128*pow(z,3) + 180*(2 + 2*z + pow(z,2))*z2) - 12*pow(Nc,4)*(-786 - 629*z + 149*pow(z,2) + 50*pow(z,3) + 258*(2 + 2*z + pow(z,2))*z2))*HPL(-1,0,z))/(576.*pow(Nc,3)*z) - ((-2 + 13*pow(Nc,2) - 94*pow(Nc,4) + 83*pow(Nc,6))*(2 + 2*z + pow(z,2))*z2*HPL(-1,1,z))/(16.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(2 + 11*z + 3*pow(z,2) - pow(Nc,2)*(8 + 19*z + 9*pow(z,2)) + pow(Nc,4)*(182 - 62*z + 161*pow(z,2)))*z2*HPL(0,-1,z))/(16.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(-32 + 6*z - 64*pow(Nc,3)*nf*(-2 + z)*z - 11*pow(z,2) + 4*Nc*nf*(-4 - 36*z + 15*pow(z,2)) + 2*pow(Nc,2)*(148 - 78*z + 31*pow(z,2)) + pow(Nc,4)*(-856 - 1050*z + 565*pow(z,2)))*z2*HPL(0,1,z))/(64.*pow(Nc,3)*z) - ((-2 + 13*pow(Nc,2) - 94*pow(Nc,4) + 83*pow(Nc,6))*(2 + 2*z + pow(z,2))*z2*HPL(1,-1,z))/(16.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-1408 + 930*z + 2685*pow(z,2) - 728*pow(z,3) + 1440*z2 + 504*z*z2 - 108*pow(z,2)*z2 + 4*pow(Nc,2)*(6997 - 908*z - 2201*pow(z,2) + 514*pow(z,3) + 72*(-8 + 43*z + 34*pow(z,2))*z2) - pow(Nc,4)*(192696 + 786*z + 25005*pow(z,2) + 26560*pow(z,3) + 36*(280 + 766*z + 1953*pow(z,2))*z2) + 4*pow(Nc,3)*nf*(1010 + 48*pow(z,3) - 12*z*(227 + 96*z2) + 3*pow(z,2)*(-351 + 192*z2)) - 4*Nc*nf*(862 + 88*pow(z,3) + z*(988 - 1152*z2) + pow(z,2)*(1153 + 576*z2)))*HPL(1,0,z))/(2304.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-3*(14 - 2*z + pow(z,2)) - 10*pow(Nc,2)*(-2 + 30*z + 21*pow(z,2)) + pow(Nc,4)*(726 + 3062*z + 1469*pow(z,2)))*z2*HPL(1,1,z))/(32.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(37 + 87*z + 42*pow(z,2) - 8*pow(z,3) - 4*pow(Nc,3)*nf*(2 + 2*z + pow(z,2)) + pow(Nc,2)*(-47 - 63*z - 9*pow(z,2) + 16*pow(z,3)) + pow(Nc,4)*(856 + 818*z + 13*pow(z,2) + 56*pow(z,3)))*HPL(-1,-1,0,z))/(48.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(61 + 147*z + 66*pow(z,2) - 20*pow(z,3) + 4*pow(Nc,3)*nf*(2 + 2*z + pow(z,2)) + 9*pow(Nc,2)*(-11 - 21*z - 5*pow(z,2) + 4*pow(z,3)) + pow(Nc,4)*(2198 + 2008*z - 61*pow(z,2) + 208*pow(z,3)))*HPL(-1,0,0,z))/(96.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-2*pow(1 + z,2)*(-7 + 2*z) - 8*pow(Nc,3)*nf*(2 + 2*z + pow(z,2)) - 3*pow(Nc,2)*(14 + 22*z + 4*pow(z,2) - 4*pow(z,3)) + pow(Nc,4)*(966 + 886*z + 56*pow(z,2) + 104*pow(z,3)))*HPL(-1,1,0,z))/(48.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(18 + 138*z + 30*pow(z,2) - 16*pow(z,3) + 4*pow(Nc,3)*nf*(2 + 6*z + pow(z,2)) - 4*Nc*nf*(2 - 6*z + 3*pow(z,2)) + pow(Nc,2)*(-2 - 66*z - 27*pow(z,2) + 32*pow(z,3)) + pow(Nc,4)*(540 + 726*z - 97*pow(z,2) + 176*pow(z,3)))*HPL(0,-1,0,z))/(96.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(2*Nc*nf*(-16 - 42*z + 21*pow(z,2)) + 2*pow(Nc,3)*nf*(256 + 722*z + 279*pow(z,2)) + 9*(-6 + 21*z + 5*pow(z,2) + 8*pow(z,3)) + pow(Nc,4)*(-9394 - 7411*z - 2316*pow(z,2) + 80*pow(z,3)) - pow(Nc,2)*(-756 + 312*z + 303*pow(z,2) + 136*pow(z,3)))*HPL(0,1,0,z))/(192.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-2*pow(1 + z,2)*(-7 + 2*z) - 8*pow(Nc,3)*nf*(2 + 2*z + pow(z,2)) - 3*pow(Nc,2)*(14 + 22*z + 4*pow(z,2) - 4*pow(z,3)) + pow(Nc,4)*(966 + 886*z + 56*pow(z,2) + 104*pow(z,3)))*HPL(1,-1,0,z))/(48.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-148 + 120*z + 447*pow(z,2) + 160*pow(z,3) - 4*Nc*nf*(-162 + 98*z + 83*pow(z,2)) + 4*pow(Nc,3)*nf*(166 + 1014*z + 481*pow(z,2)) - 2*pow(Nc,2)*(-640 + 554*z + 1715*pow(z,2) + 40*pow(z,3)) - pow(Nc,4)*(10972 + 15036*z + 7885*pow(z,2) + 1648*pow(z,3)))*HPL(1,0,0,z))/(384.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(-154 + 99*z + 222*pow(z,2) + 48*pow(z,3) - 4*pow(Nc,3)*nf*(122 + 170*z + 23*pow(z,2)) - 8*Nc*nf*(-19 - 4*z + 32*pow(z,2)) + pow(Nc,4)*(-2 + 12317*z + 2591*pow(z,2) - 696*pow(z,3)) - pow(Nc,2)*(-1732 + 2372*z + 2111*pow(z,2) + 8*pow(z,3)))*HPL(1,1,0,z))/(192.*pow(Nc,3)*z) - ((-4 + 9*pow(Nc,2) - 48*pow(Nc,4) + 43*pow(Nc,6))*(2 + 2*z + pow(z,2))*HPL(-1,-1,-1,0,z))/(8.*pow(Nc,3)*z) + ((-2 + 6*pow(Nc,2) - 31*pow(Nc,4) + 27*pow(Nc,6))*(2 + 2*z + pow(z,2))*HPL(-1,-1,0,0,z))/(4.*pow(Nc,3)*z) - ((-2 + 7*pow(Nc,2) - 46*pow(Nc,4) + 41*pow(Nc,6))*(2 + 2*z + pow(z,2))*HPL(-1,-1,1,0,z))/(8.*pow(Nc,3)*z) + ((-5 + 12*pow(Nc,2) - 55*pow(Nc,4) + 48*pow(Nc,6))*(2 + 2*z + pow(z,2))*HPL(-1,0,-1,0,z))/(16.*pow(Nc,3)*z) - ((-5 + 26*pow(Nc,2) - 123*pow(Nc,4) + 102*pow(Nc,6))*(2 + 2*z + pow(z,2))*HPL(-1,0,0,0,z))/(32.*pow(Nc,3)*z) + ((-1 + 5*pow(Nc,2) - 31*pow(Nc,4) + 27*pow(Nc,6))*(2 + 2*z + pow(z,2))*HPL(-1,0,1,0,z))/(8.*pow(Nc,3)*z) - ((-2 + 7*pow(Nc,2) - 46*pow(Nc,4) + 41*pow(Nc,6))*(2 + 2*z + pow(z,2))*HPL(-1,1,-1,0,z))/(8.*pow(Nc,3)*z) + ((-1 + 7*pow(Nc,2) - 43*pow(Nc,4) + 37*pow(Nc,6))*(2 + 2*z + pow(z,2))*HPL(-1,1,0,0,z))/(8.*pow(Nc,3)*z) - (3*(1 - 8*pow(Nc,2) + 7*pow(Nc,4))*(2 + 2*z + pow(z,2))*HPL(-1,1,1,0,z))/(8.*Nc*z) + ((-1 + pow(Nc,2))*(2 + 7*z + pow(z,2) - pow(Nc,2)*(4 + 7*z + 3*pow(z,2)) + pow(Nc,4)*(58 - 14*z + 49*pow(z,2)))*HPL(0,-1,-1,0,z))/(8.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(2 + 13*z + pow(z,2) - pow(Nc,2)*(6 + 21*z + 7*pow(z,2)) + pow(Nc,4)*(152 - 66*z + 139*pow(z,2)))*HPL(0,-1,0,0,z))/(16.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(z*(2 + z) - pow(Nc,2)*(2 + 6*z + 3*pow(z,2)) + pow(Nc,4)*(62 - 24*z + 56*pow(z,2)))*HPL(0,-1,1,0,z))/(8.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(-3*pow(Nc,2)*z*(2 + z) + z*(10 + z) + pow(Nc,4)*(48 - 70*z + 51*pow(z,2)))*HPL(0,0,-1,0,z))/(16.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(8 - 14*z - 16*Nc*nf*(-2 + z)*z + 16*pow(Nc,3)*nf*(-2 + z)*z + 15*pow(z,2) - 8*pow(Nc,2)*(16 - 9*z + 13*pow(z,2)) + pow(Nc,4)*(824 + 354*z + 591*pow(z,2)))*HPL(0,0,1,0,z))/(64.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(z*(2 + z) - pow(Nc,2)*(2 + 6*z + 3*pow(z,2)) + pow(Nc,4)*(62 - 24*z + 56*pow(z,2)))*HPL(0,1,-1,0,z))/(8.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-16 - 2*z - 64*pow(Nc,3)*nf*(-2 + z)*z - 5*pow(z,2) + 6*pow(Nc,2)*(36 - 32*z + 5*pow(z,2)) + 4*Nc*nf*(4 - 28*z + 17*pow(z,2)) + pow(Nc,4)*(-536 - 1070*z + 623*pow(z,2)))*HPL(0,1,0,0,z))/(64.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(-32 + 14*z - 64*pow(Nc,3)*nf*(-2 + z)*z - 7*pow(z,2) + 4*Nc*nf*(-4 - 36*z + 15*pow(z,2)) + 2*pow(Nc,2)*(144 - 90*z + 25*pow(z,2)) + pow(Nc,4)*(-608 - 1146*z + 789*pow(z,2)))*HPL(0,1,1,0,z))/(64.*pow(Nc,3)*z) - ((-2 + 7*pow(Nc,2) - 46*pow(Nc,4) + 41*pow(Nc,6))*(2 + 2*z + pow(z,2))*HPL(1,-1,-1,0,z))/(8.*pow(Nc,3)*z) + ((-1 + 7*pow(Nc,2) - 43*pow(Nc,4) + 37*pow(Nc,6))*(2 + 2*z + pow(z,2))*HPL(1,-1,0,0,z))/(8.*pow(Nc,3)*z) - (3*(1 - 8*pow(Nc,2) + 7*pow(Nc,4))*(2 + 2*z + pow(z,2))*HPL(1,-1,1,0,z))/(8.*Nc*z) + ((-1 + pow(Nc,2))*(6 - 2*z + 5*pow(z,2) - pow(Nc,2)*(6 + 10*z + 7*pow(z,2)) + 4*pow(Nc,4)*(31 - 12*z + 28*pow(z,2)))*HPL(1,0,-1,0,z))/(16.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-4 - 14*z + 32*Nc*nf*(-2 + z)*z - 32*pow(Nc,3)*nf*(-2 + z)*z + 3*pow(z,2) - 2*pow(Nc,2)*(-23 + 66*z + 43*pow(z,2)) + 2*pow(Nc,4)*(73 + 132*z + 427*pow(z,2)))*HPL(1,0,0,0,z))/(32.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(7*(-2 + z)*z + 64*Nc*nf*(-2 + z)*z - 64*pow(Nc,3)*nf*(-2 + z)*z - 4*pow(Nc,2)*(-34 + 6*z + 7*pow(z,2)) + pow(Nc,4)*(-8 - 1626*z + 1125*pow(z,2)))*HPL(1,0,1,0,z))/(64.*pow(Nc,3)*z) - (3*(1 - 8*pow(Nc,2) + 7*pow(Nc,4))*(2 + 2*z + pow(z,2))*HPL(1,1,-1,0,z))/(8.*Nc*z) - ((-1 + pow(Nc,2))*(-52 - 10*z + 64*Nc*nf*(-2 + z)*z - 64*pow(Nc,3)*nf*(-2 + z)*z + 5*pow(z,2) - 2*pow(Nc,2)*(-52 + 214*z + 169*pow(z,2)) + pow(Nc,4)*(972 + 2614*z + 3061*pow(z,2)))*HPL(1,1,0,0,z))/(64.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-3*(14 - 2*z + pow(z,2)) - 8*pow(Nc,2)*(-1 + 39*z + 27*pow(z,2)) + pow(Nc,4)*(810 + 3146*z + 1511*pow(z,2)))*HPL(1,1,1,0,z))/(32.*pow(Nc,3)*z) - ((-27 + 221*pow(Nc,2) - 933*pow(Nc,4) + 739*pow(Nc,6))*(2 - 2*z + pow(z,2))*pow(log(1 - z),4))/(384.*pow(Nc,3)*z) + (((-1 + pow(Nc,2))*(2808 - 5898*z + 6618*pow(z,2) + 15112*pow(z,3) + 72*(-36 - 147*z + 63*pow(z,2) + 56*pow(z,3))*z2 + 1728*z3 - 9072*z*z3 - 2376*pow(z,2)*z3 + 4*Nc*nf*(-3622 + 25498*z - 23387*pow(z,2) + 600*pow(z,3) - 36*(-60 + 86*z + 17*pow(z,2))*z2 - 1728*z3 - 5184*z*z3 + 1296*pow(z,2)*z3) - 4*pow(Nc,3)*nf*(-42330 + 54350*z - 26467*pow(z,2) + 7672*pow(z,3) + 36*(76 - 62*z + 51*pow(z,2))*z2 - 3456*z*z3 + 1728*pow(z,2)*z3) - 2*pow(Nc,2)*(-26482 + 144308*z - 2071*pow(z,2) + 42820*pow(z,3) + 48*pow(nf,2)*(29 - 38*z + 19*pow(z,2)) + 30240*z2 - 54576*z*z2 + 63144*pow(z,2)*z2 - 2592*pow(z,3)*z2 + 864*z3 - 4752*z*z3 + 7992*pow(z,2)*z3) + 2*pow(Nc,4)*(-662738 + 125272*pow(z,3) - 36*(-2172 + 3727*z - 1521*pow(z,2) + 1200*pow(z,3))*z2 + 10368*z3 + 2*pow(z,2)*(-84451 + 32670*z3) + z*(1196185 + 56808*z3))))/(13824.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(11*Nc - 2*nf)*(2 + 2*z + pow(z,2))*HPL(-1,0,z))/(12.*z) + ((-1 + pow(Nc,2))*(11*Nc - 2*nf)*(1 + pow(Nc,2)*(5 + 18*z + 9*pow(z,2)))*HPL(1,0,z))/(6.*pow(Nc,2)*z))*log(z) - ((-1 + pow(Nc,2))*(-2*z*(1095 + 765*z - 8*pow(z,2) + 18*(-34 + 5*z)*z2) + 2*pow(Nc,2)*(-752 + 10619*z - 2401*pow(z,2) - 720*pow(z,3) + 48*pow(nf,2)*(2 - 2*z + pow(z,2)) + 288*(4 + 8*z + 11*pow(z,2))*z2) - 2*pow(Nc,4)*(2*(-29218 + 79094*z - 6463*pow(z,2) + 12236*pow(z,3)) + 18*(256 - 218*z + 977*pow(z,2))*z2) - 4*pow(Nc,3)*nf*(3164 - 512*pow(z,3) + pow(z,2)*(2374 - 432*z2) + z*(-3845 + 864*z2)) + 4*Nc*nf*(344 - 120*pow(z,3) + pow(z,2)*(740 - 432*z2) + z*(-2941 + 864*z2)))*pow(log(z),2))/(4608.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(2*Nc*nf*(64 - 46*z - 49*pow(z,2)) + pow(Nc,2)*z*(2372 - 1933*z + 16*pow(z,2)) + z*(39 - 3*z + 176*pow(z,2)) - 2*pow(Nc,3)*nf*(272 + 154*z + 227*pow(z,2)) + pow(Nc,4)*(7168 - 1795*z + 4420*pow(z,2) - 1920*pow(z,3)))*pow(log(z),3))/(2304.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(88*Nc*nf*(-2 + z)*z - 88*pow(Nc,3)*nf*(-2 + z)*z - 22*pow(Nc,2)*z*(6 + 7*z) - z*(22 + 9*z) + pow(Nc,4)*(448 - 902*z + 1659*pow(z,2)))*pow(log(z),4))/(3072.*pow(Nc,3)*z) + pow(log(1 - z),3)*(((-1 + pow(Nc,2))*(50*Nc*nf*(2 - 2*z + pow(z,2)) - 198*pow(Nc,3)*nf*(2 - 2*z + pow(z,2)) + 27*(9 - 16*z + 7*pow(z,2)) + pow(Nc,2)*(-4038 + 4558*z - 1241*pow(z,2) + 248*pow(z,3)) - pow(Nc,4)*(-19543 + 16818*z + 30*pow(z,2) + 2056*pow(z,3))))/(576.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(9*pow(-1 + z,2) - 2*pow(Nc,2)*(61 - 23*z + 58*pow(z,2)) + pow(Nc,4)*(673 + 316*z + 613*pow(z,2)))*log(z))/(48.*pow(Nc,3)*z)) + pow(log(1 - z),2)*(-((-1 + pow(Nc,2))*(1507 - 2538*z + 1413*pow(z,2) - 868*pow(z,3) + 4*Nc*nf*(1415 - 2772*z + 1584*pow(z,2) - 128*pow(z,3)) + 4*pow(Nc,3)*nf*(-4585 + 5648*z - 2122*pow(z,2) + 368*pow(z,3)) - 792*z2 + 2088*z*z2 - 1044*pow(z,2)*z2 + 2*pow(Nc,2)*(-23518 + 21414*z - 957*pow(z,2) + 3232*pow(z,3) + 24*pow(nf,2)*(2 - 2*z + pow(z,2)) + 72*(84 - 14*z + 97*pow(z,2))*z2) - pow(Nc,4)*(-343449 + 322538*z - 28057*pow(z,2) + 34036*pow(z,3) + 36*(1658 + 2130*z + 1935*pow(z,2))*z2)))/(2304.*pow(Nc,3)*z) - (3*(1 - 8*pow(Nc,2) + 7*pow(Nc,4))*(2 + 2*z + pow(z,2))*HPL(-1,0,z))/(16.*Nc*z) + ((-1 + pow(Nc,2))*(-34 - 2*z + pow(z,2) + pow(Nc,2)*(4 - 308*z - 218*pow(z,2)) + pow(Nc,4)*(806 + 3150*z + 1509*pow(z,2)))*HPL(1,0,z))/(64.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(108 - 399*z + 432*pow(z,2) + 56*pow(z,3) - 4*Nc*nf*(-90 + 38*z + 53*pow(z,2)) - 4*pow(Nc,3)*nf*(266 + 26*z + 95*pow(z,2)) + pow(Nc,4)*(26024 - 9859*z + 2432*pow(z,2) - 3448*pow(z,3)) + pow(Nc,2)*(-3360 + 3290*z - 3472*pow(z,2) + 304*pow(z,3)))*log(z))/(384.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(16 - 78*z + 64*Nc*nf*(-2 + z)*z - 64*pow(Nc,3)*nf*(-2 + z)*z + 39*pow(z,2) - 6*pow(Nc,2)*(56 - 2*z + 93*pow(z,2)) + pow(Nc,4)*(2864 + 722*z + 4007*pow(z,2)))*pow(log(z),2))/(256.*pow(Nc,3)*z)) + log(1 - z)*(-((-1 + pow(Nc,2))*(956 - 5496*z + 834*pow(z,2) + 3706*pow(z,3) + 612*z2 - 5670*z*z2 + 6480*pow(z,2)*z2 + 1008*pow(z,3)*z2 - 864*z3 - 1620*z*z3 - 54*pow(z,2)*z3 + pow(Nc,2)*(99775 - 81182*z + 4423*pow(z,2) - 25160*pow(z,3) - 24*pow(nf,2)*(29 - 38*z + 19*pow(z,2)) + 6*(-6930 + 6486*z - 9381*pow(z,2) + 768*pow(z,3))*z2 - 4968*z3 + 3456*z*z3 - 14256*pow(z,2)*z3) - 2*Nc*nf*(5633 - 688*pow(z,3) + 144*(-21 + 8*z + 14*pow(z,2))*z2 + pow(z,2)*(15449 - 1728*z3) + 4*z*(-5089 + 864*z3)) - 2*pow(Nc,3)*nf*(-37739 + 3888*pow(z,3) + 36*(158 + 118*z + 41*pow(z,2))*z2 + z*(46880 - 3456*z3) + pow(z,2)*(-15313 + 1728*z3)) + pow(Nc,4)*(-780899 + 50374*pow(z,3) + 6*(34548 - 1581*z + 5883*pow(z,2) - 7272*pow(z,3))*z2 + z*(791734 - 10044*z3) + 58968*z3 + pow(z,2)*(-74957 + 115614*z3))))/(3456.*pow(Nc,3)*z) - ((-2 + 13*pow(Nc,2) - 94*pow(Nc,4) + 83*pow(Nc,6))*(2 + 2*z + pow(z,2))*z2*HPL(-1,z))/(16.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-18 - 5*pow(Nc,2)*(-2 + 30*z + 21*pow(z,2)) + pow(Nc,4)*(356 + 1538*z + 731*pow(z,2)))*z2*HPL(1,z))/(16.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-2*pow(1 + z,2)*(-7 + 2*z) - 8*pow(Nc,3)*nf*(2 + 2*z + pow(z,2)) - 3*pow(Nc,2)*(14 + 22*z + 4*pow(z,2) - 4*pow(z,3)) + pow(Nc,4)*(966 + 886*z + 56*pow(z,2) + 104*pow(z,3)))*HPL(-1,0,z))/(48.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(-190 + 147*z + 201*pow(z,2) + 48*pow(z,3) - 4*pow(Nc,3)*nf*(126 + 166*z + 25*pow(z,2)) - 4*Nc*nf*(-58 + 6*z + 69*pow(z,2)) + pow(Nc,4)*(438 + 12013*z + 2545*pow(z,2) - 760*pow(z,3)) - 2*pow(Nc,2)*(-862 + 1176*z + 1059*pow(z,2) + 4*pow(z,3)))*HPL(1,0,z))/(192.*pow(Nc,3)*z) - ((-2 + 7*pow(Nc,2) - 46*pow(Nc,4) + 41*pow(Nc,6))*(2 + 2*z + pow(z,2))*HPL(-1,-1,0,z))/(8.*pow(Nc,3)*z) + ((-1 + 7*pow(Nc,2) - 43*pow(Nc,4) + 37*pow(Nc,6))*(2 + 2*z + pow(z,2))*HPL(-1,0,0,z))/(8.*pow(Nc,3)*z) - (3*(1 - 8*pow(Nc,2) + 7*pow(Nc,4))*(2 + 2*z + pow(z,2))*HPL(-1,1,0,z))/(8.*Nc*z) + ((-1 + pow(Nc,2))*(z*(2 + z) - pow(Nc,2)*(2 + 6*z + 3*pow(z,2)) + pow(Nc,4)*(62 - 24*z + 56*pow(z,2)))*HPL(0,-1,0,z))/(8.*pow(Nc,3)*z) - ((-1 + pow(Nc,2))*(-32 + 18*z + 64*Nc*nf*(-2 + z)*z - 64*pow(Nc,3)*nf*(-2 + z)*z - 9*pow(z,2) + 16*pow(Nc,2)*(18 - 11*z + 3*pow(z,2)) + pow(Nc,4)*(-544 - 1090*z + 857*pow(z,2)))*HPL(0,1,0,z))/(64.*pow(Nc,3)*z) - (3*(1 - 8*pow(Nc,2) + 7*pow(Nc,4))*(2 + 2*z + pow(z,2))*HPL(1,-1,0,z))/(8.*Nc*z) - ((-1 + pow(Nc,2))*(-40 - 22*z + 64*Nc*nf*(-2 + z)*z - 64*pow(Nc,3)*nf*(-2 + z)*z + 11*pow(z,2) - 2*pow(Nc,2)*(-56 + 218*z + 167*pow(z,2)) + pow(Nc,4)*(1064 + 2522*z + 3107*pow(z,2)))*HPL(1,0,0,z))/(64.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-9 + pow(Nc,2)*(2 - 78*z - 54*pow(z,2)) + pow(Nc,4)*(199 + 790*z + 376*pow(z,2)))*HPL(1,1,0,z))/(8.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(-2*pow(Nc,2)*(7021 - 11751*z + 2271*pow(z,2) - 2008*pow(z,3) - 24*pow(nf,2)*(2 - 2*z + pow(z,2)) - 1944*z2 - 576*z*z2 - 3708*pow(z,2)*z2) + 3*(46 - 438*z + 697*pow(z,2) - 204*pow(z,3) + 6*(-8 + 62*z - 27*pow(z,2))*z2) + pow(Nc,4)*(146108 - 233036*z + 11551*pow(z,2) - 37844*pow(z,3) - 18*(1040 + 6*z + 2333*pow(z,2))*z2) - 4*pow(Nc,3)*nf*(3025 - 336*pow(z,3) - 8*pow(z,2)*(-239 + 36*z2) + 4*z*(-695 + 144*z2)) - 4*Nc*nf*(-503 + 128*pow(z,3) - 96*z*(-26 + 6*z2) + pow(z,2)*(-645 + 288*z2)))*log(z))/(1152.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(24*Nc*nf*(7 - 8*z - 5*pow(z,2)) + 3*z*(-26 + 89*z + 32*pow(z,2)) - 8*pow(Nc,3)*nf*(77 - 4*z + 43*pow(z,2)) + pow(Nc,4)*(10212 - 5312*z + 2450*pow(z,2) - 2056*pow(z,3)) - pow(Nc,2)*(844 - 1920*z + 2223*pow(z,2) - 88*pow(z,3)))*pow(log(z),2))/(384.*pow(Nc,3)*z) + ((-1 + pow(Nc,2))*(32*Nc*nf*(-2 + z)*z - 32*pow(Nc,3)*nf*(-2 + z)*z + z*(-18 + 5*z) - pow(Nc,2)*(32 + 54*z + 125*pow(z,2)) + pow(Nc,4)*(480 - 70*z + 1021*pow(z,2)))*pow(log(z),3))/(192.*pow(Nc,3)*z));
        check_imaginary_part(L1,__PRETTY_FUNCTION__);
        
        return real(L3)*pow(L,3)+real(L2)*pow(L,2)+real(L1)*L;
    }
    
    double qg_nnlo_r_lz0(const double& z, const double& L)
    {
        return    qg_nnlo_r_lz0_const(z,L)
        + qg_nnlo_r_lz0_logz(z,L)
        + qg_nnlo_r_lz0_logz_sq(z,L)
        + qg_nnlo_r_lz0_logz_cube(z,L) ;
    }
    double qg_nnlo_r_lz0_const(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double z2 = consts::z2;
        const double z3 = consts::z3;
        complex<double> res =
        (3*(-1 + pow(Nc,2))*(2 + 2*z + pow(z,2))*z2*HPL(-1,z))/(4.*z) - ((-1 + pow(Nc,2))*(2 + 2*z - pow(z,2) + 4*pow(Nc,2)*(3 + 18*z + 8*pow(z,2)))*z2*HPL(1,z))/(8.*pow(Nc,2)*z) + ((-1 + pow(Nc,2))*(-576 - 2592*pow(Nc,2) - 1728*L*pow(Nc,2) - 864*z - 3456*pow(Nc,2)*z - 1728*L*pow(Nc,2)*z - 864*pow(z,2) - 432*pow(Nc,2)*pow(z,2) - 864*L*pow(Nc,2)*pow(z,2) - 576*pow(z,3))*HPL(-1,0,z))/(3456.*pow(Nc,2)*z) + ((-1 + pow(Nc,2))*(1656 + 864*L - 21024*pow(Nc,2) + 4320*L*pow(Nc,2) - 3024*z + 7344*pow(Nc,2)*z + 15552*L*pow(Nc,2)*z - 2268*pow(z,2) + 5940*pow(Nc,2)*pow(z,2) + 7776*L*pow(Nc,2)*pow(z,2) - 288*pow(z,3) - 1152*pow(Nc,2)*pow(z,3))*HPL(1,0,z))/(3456.*pow(Nc,2)*z) + ((-1 + pow(Nc,2))*(3456*pow(Nc,2) + 3456*pow(Nc,2)*z + 1728*pow(Nc,2)*pow(z,2))*HPL(-1,-1,0,z))/(3456.*pow(Nc,2)*z) + ((-1 + pow(Nc,2))*(-1728*pow(Nc,2) - 1728*pow(Nc,2)*z - 864*pow(Nc,2)*pow(z,2))*HPL(-1,0,0,z))/(3456.*pow(Nc,2)*z) + ((-1 + pow(Nc,2))*(3456*pow(Nc,2) + 3456*pow(Nc,2)*z + 1728*pow(Nc,2)*pow(z,2))*HPL(-1,1,0,z))/(3456.*pow(Nc,2)*z) + ((-1 + pow(Nc,2))*(-864 - 14688*pow(Nc,2) - 432*z - 34128*pow(Nc,2)*z + 216*pow(z,2) - 19224*pow(Nc,2)*pow(z,2))*HPL(0,1,0,z))/(3456.*pow(Nc,2)*z) + ((-1 + pow(Nc,2))*(3456*pow(Nc,2) + 3456*pow(Nc,2)*z + 1728*pow(Nc,2)*pow(z,2))*HPL(1,-1,0,z))/(3456.*pow(Nc,2)*z) + ((-1 + pow(Nc,2))*(-3456 - 12960*pow(Nc,2) + 1728*z - 44064*pow(Nc,2)*z - 864*pow(z,2) - 21168*pow(Nc,2)*pow(z,2))*HPL(1,0,0,z))/(3456.*pow(Nc,2)*z) + ((-1 + pow(Nc,2))*(-864 - 6912*pow(Nc,2) - 864*z - 32832*pow(Nc,2)*z + 432*pow(z,2) - 14688*pow(Nc,2)*pow(z,2))*HPL(1,1,0,z))/(3456.*pow(Nc,2)*z) + ((-1 + pow(Nc,2))*(1393 + 636*L - 44471*pow(Nc,2) - 29700*L*pow(Nc,2) - 8280*pow(L,2)*pow(Nc,2) + 2120*Nc*nf + 1392*L*Nc*nf + 288*pow(L,2)*Nc*nf + 780*z - 648*L*z - 216*pow(L,2)*z + 39068*pow(Nc,2)*z + 21336*L*pow(Nc,2)*z + 6984*pow(L,2)*pow(Nc,2)*z - 3344*Nc*nf*z - 1824*L*Nc*nf*z - 288*pow(L,2)*Nc*nf*z - 51*pow(z,2) - 216*L*pow(z,2) + 54*pow(L,2)*pow(z,2) + 2393*pow(Nc,2)*pow(z,2) + 4560*L*pow(Nc,2)*pow(z,2) - 198*pow(L,2)*pow(Nc,2)*pow(z,2) + 1432*Nc*nf*pow(z,2) + 912*L*Nc*nf*pow(z,2) + 144*pow(L,2)*Nc*nf*pow(z,2) - 2608*pow(z,3) - 744*L*pow(z,3) + 1656*pow(Nc,2)*pow(z,3) + 1128*L*pow(Nc,2)*pow(z,3) + 864*pow(L,2)*pow(Nc,2)*pow(z,3) - 2520*z2 - 864*L*z2 + 3816*pow(Nc,2)*z2 + 6048*L*pow(Nc,2)*z2 + 2592*z*z2 + 1728*L*z*z2 - 12096*pow(Nc,2)*z*z2 + 12096*L*pow(Nc,2)*z*z2 - 5508*pow(z,2)*z2 - 864*L*pow(z,2)*z2 + 2916*pow(Nc,2)*pow(z,2)*z2 + 8640*L*pow(Nc,2)*pow(z,2)*z2 - 576*pow(z,3)*z2 - 4608*pow(Nc,2)*pow(z,3)*z2 + 864*z3 + 4320*pow(Nc,2)*z3 - 7776*pow(Nc,2)*z*z3 + 4320*pow(Nc,2)*pow(z,2)*z3))/(3456.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res);
    }
    
    double qg_nnlo_r_lz0_logz(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double z2 = consts::z2;
        complex<double> res =
        -((-1 + pow(Nc,2))*(-106 + 4910*pow(Nc,2) + 2232*L*pow(Nc,2) + 432*pow(L,2)*pow(Nc,2) - 232*Nc*nf - 96*L*Nc*nf + 621*z + 72*L*z + 36*pow(L,2)*z - 5941*pow(Nc,2)*z - 2760*L*pow(Nc,2)*z + 396*pow(L,2)*pow(Nc,2)*z + 304*Nc*nf*z + 96*L*Nc*nf*z - 642*pow(z,2) - 342*L*pow(z,2) - 18*pow(L,2)*pow(z,2) - 1516*pow(Nc,2)*pow(z,2) + 390*L*pow(Nc,2)*pow(z,2) + 450*pow(L,2)*pow(Nc,2)*pow(z,2) - 152*Nc*nf*pow(z,2) - 48*L*Nc*nf*pow(z,2) + 112*pow(z,3) - 48*L*pow(z,3) - 560*pow(Nc,2)*pow(z,3) - 528*L*pow(Nc,2)*pow(z,3) + 144*z2 - 432*pow(Nc,2)*z2 - 360*z*z2 - 1080*pow(Nc,2)*z*z2 + 180*pow(z,2)*z2 - 1044*pow(Nc,2)*pow(z,2)*z2))/(576.*pow(Nc,2)*z) - ((-1 + pow(Nc,2))*(2 + 2*z + pow(z,2))*HPL(-1,0,z))/(4.*z) + ((-1 + pow(Nc,2))*(1 + pow(Nc,2)*(5 + 18*z + 9*pow(z,2)))*HPL(1,0,z))/(2.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*log(z);
    }

    double qg_nnlo_r_lz0_logz_sq(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        complex<double> res =
        ((-1 + pow(Nc,2))*(16*Nc*nf*(2 - 2*z + pow(z,2)) + z*(84 + 24*L*(-2 + z) + 189*z + 80*pow(z,2)) - pow(Nc,2)*(744 - 1292*z + 229*pow(z,2) - 192*pow(z,3) + 24*L*(12 + 14*z + 15*pow(z,2)))))/(384.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(z),2.);
    }
    
    double qg_nnlo_r_lz0_logz_cube(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        complex<double> res =
        -((-1 + pow(Nc,2))*(-5*(-2 + z)*z + pow(Nc,2)*(48 + 62*z + 65*pow(z,2))))/(192.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(z),3.);
    }
    
    double qg_nnlo_r_lz1(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double z2 = consts::z2;
        complex<double> res =
        ((-1 + pow(Nc,2))*(-439 - 162*L - 36*pow(L,2) + 4781*pow(Nc,2) + 2766*L*pow(Nc,2) + 252*pow(L,2)*pow(Nc,2) - 156*Nc*nf - 48*L*Nc*nf + 648*z + 288*L*z + 36*pow(L,2)*z - 3360*pow(Nc,2)*z - 2496*L*pow(Nc,2)*z - 252*pow(L,2)*pow(Nc,2)*z + 192*Nc*nf*z + 48*L*Nc*nf*z - 216*pow(z,2) - 126*L*pow(z,2) - 18*pow(L,2)*pow(z,2) - 828*pow(Nc,2)*pow(z,2) + 6*L*pow(Nc,2)*pow(z,2) + 126*pow(L,2)*pow(Nc,2)*pow(z,2) - 108*Nc*nf*pow(z,2) - 24*L*Nc*nf*pow(z,2) + 124*pow(z,3) - 188*pow(Nc,2)*pow(z,3) - 288*L*pow(Nc,2)*pow(z,3) + 216*z2 - 936*pow(Nc,2)*z2 - 360*z*z2 - 2088*pow(Nc,2)*z*z2 + 180*pow(z,2)*z2 - 1404*pow(Nc,2)*pow(z,2)*z2))/(288.*pow(Nc,2)*z) + ((-1 + pow(Nc,2))*(-27 + 42*z - 66*pow(z,2) - 8*pow(z,3) - 4*Nc*nf*(2 - 2*z + pow(z,2)) + pow(Nc,2)*(373 - 458*z + 46*pow(z,2) - 88*pow(z,3)) + 12*L*(-pow(-1 + z,2) + pow(Nc,2)*(17 + 6*z + 15*pow(z,2))))*HPL(0,z))/(48.*pow(Nc,2)*z) + ((-1 + pow(Nc,2))*(-2 + 6*z - 3*pow(z,2) + pow(Nc,2)*(34 + 18*z + 35*pow(z,2)))*pow(HPL(0,z),2))/(16.*pow(Nc,2)*z) + ((-1 + pow(Nc,2))*(2 + 2*z + pow(z,2))*HPL(-1,0,z))/(2.*z) + ((-1 + pow(Nc,2))*(-72 - 648*pow(Nc,2) - 72*z - 2664*pow(Nc,2)*z + 36*pow(z,2) - 1260*pow(Nc,2)*pow(z,2))*HPL(1,0,z))/(288.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*log(1.-z);
    }
    
    double qg_nnlo_r_lz2(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        complex<double> res =
        ((-1 + pow(Nc,2))*(180 - 1748*pow(Nc,2) + 8*Nc*nf - 282*z + 1550*pow(Nc,2)*z - 8*Nc*nf*z + 135*pow(z,2) + 59*pow(Nc,2)*pow(z,2) + 4*Nc*nf*pow(z,2) + 192*pow(Nc,2)*pow(z,3) - 36*L*(-1 + 7*pow(Nc,2))*(2 - 2*z + pow(z,2))))/(192.*pow(Nc,2)*z) - ((-1 + pow(Nc,2))*(-6 + 10*z - 5*pow(z,2) + pow(Nc,2)*(72 + 20*z + 62*pow(z,2)))*HPL(0,z))/(16.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(1.-z),2.);
    }
    
    double qg_nnlo_r_lz3(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        complex<double> res =
            ((13 - 96*pow(Nc,2) + 83*pow(Nc,4))*(2 - 2*z + pow(z,2)))/(96.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(1.-z),3.);
    }
    
    
    double qg_n3lo_r_lz0(const double& z, const double& L)
    {
        // NS approximation only
        return qg_n3lo_r_lz0_NS(z,L);
    }
    
    double qg_n3lo_r_lz1(const double& z, const double& L)
    {
        // NS approximation only
        return qg_n3lo_r_lz1_NS(z,L);
    }
    
    double qg_n3lo_r_lz2(const double& z, const double& L)
    {
        // NS approximation only
        return qg_n3lo_r_lz2_NS(z,L);
    }
    
    double qg_n3lo_r_lz3(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double z2 = consts::z2;
        complex<double> res =
        (nf*(15506 - 22755*z + 10707*pow(z,2) - 1292*pow(z,3)))/(2592.*z) + (nf*(-8414 + 16391*z - 9403*pow(z,2) + 768*pow(z,3)))/(5184.*pow(Nc,2)*z) + (pow(Nc,2)*nf*(-22598 + 29119*z - 12011*pow(z,2) + 1816*pow(z,3)))/(5184.*z) - (pow(Nc,3)*(-3958688 + 3718820*z - 321247*pow(z,2) + 394212*pow(z,3) + 702000*z2 + 930456*z*z2 + 826308*pow(z,2)*z2 - 36000*(2 + 2*z + pow(z,2))*HPL(-1,z)*HPL(0,z) + 108*(3112 + 12670*z + 5957*pow(z,2))*HPL(0,z)*HPL(1,z) + 72000*HPL(0,-1,z) + 72000*z*HPL(0,-1,z) + 36000*pow(z,2)*HPL(0,-1,z) - 336096*HPL(0,1,z) - 1368360*z*HPL(0,1,z) - 643356*pow(z,2)*HPL(0,1,z)))/(41472.*z) + (-10424 + 21360*z - 10401*pow(z,2) + 1364*pow(z,3) + 4272*z2 - 10056*z*z2 + 5028*pow(z,2)*z2 + 12*(-416 - 66*z + 33*pow(z,2))*HPL(0,z)*HPL(1,z) + (4992 + 792*z - 396*pow(z,2))*HPL(0,1,z))/(13824.*pow(Nc,3)*z) + Nc*((7*pow(nf,2)*(2 - 2*z + pow(z,2)))/(864.*z) + (-4654168 + 4354944*z - 360957*pow(z,2) + 487108*pow(z,3) + 858096*z2 + 901080*z*z2 + 1000980*pow(z,2)*z2 - 41472*(2 + 2*z + pow(z,2))*HPL(-1,z)*HPL(0,z) + 36*(9632 + 41538*z + 20703*pow(z,2))*HPL(0,z)*HPL(1,z) + 82944*HPL(0,-1,z) + 82944*z*HPL(0,-1,z) + 41472*pow(z,2)*HPL(0,-1,z) - 346752*HPL(0,1,z) - 1495368*z*HPL(0,1,z) - 745308*pow(z,2)*HPL(0,1,z))/(41472.*z)) + ((-7*pow(nf,2)*(2 - 2*z + pow(z,2)))/(864.*z) - (-726752 + 700204*z - 70913*pow(z,2) + 96988*pow(z,3) + 168912*z2 - 59544*z*z2 + 189756*pow(z,2)*z2 - 5472*(2 + 2*z + pow(z,2))*HPL(-1,z)*HPL(0,z) + 108*(-40 + 1154*z + 955*pow(z,2))*HPL(0,z)*HPL(1,z) + 10944*HPL(0,-1,z) + 10944*z*HPL(0,-1,z) + 5472*pow(z,2)*HPL(0,-1,z) + 4320*HPL(0,1,z) - 124632*z*HPL(0,1,z) - 103140*pow(z,2)*HPL(0,1,z))/(41472.*z))/Nc + ((nf*(-42 + 901/z - 18*z))/432. - (pow(Nc,2)*nf*(2536 + 230*z + 593*pow(z,2)))/(1728.*z) + (nf*(-1068 + 398*z + 665*pow(z,2)))/(1728.*pow(Nc,2)*z) + (pow(Nc,3)*(315012 - 131476*z + 17807*pow(z,2) - 42712*pow(z,3)))/(6912.*z) + (56220 - 49556*z + 55561*pow(z,2) - 3528*pow(z,3))/(6912.*Nc*z) - (972 - 2876*z + 1813*pow(z,2) + 88*pow(z,3))/(2304.*pow(Nc,3)*z) + (Nc*(-368316 + 172404*z - 67929*pow(z,2) + 46504*pow(z,3)))/(6912.*z))*log(z) + ((Nc*(-1390 - 7512/z - 10009*z))/768. + (nf*(2 - z))/(6.*pow(Nc,2)) + (pow(Nc,2)*nf*(2 - z))/6. + (nf*(-2 + z))/3. - (24 - 74*z + 37*pow(z,2))/(256.*pow(Nc,3)*z) + (3144 - 1298*z + 4337*pow(z,2))/(2304.*Nc*z) + (pow(Nc,3)*(19608 + 4802*z + 26023*pow(z,2)))/(2304.*z))*pow(log(z),2)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(1.-z),3.);
    }
    
    double qg_n3lo_r_lz4(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        
        complex<double> res =
        (Nc*(-168484 + 186548/z + 8459*z - 18432*pow(z,2)))/9216. - (11*nf*(2 - 2*z + pow(z,2)))/(72.*z) + (253*nf*(2 - 2*z + pow(z,2)))/(6912.*pow(Nc,2)*z) + (803*pow(Nc,2)*nf*(2 - 2*z + pow(z,2)))/(6912.*z) + (2892 - 4828*z + 2165*pow(z,2))/(9216.*pow(Nc,3)*z) + (-113308 + 131936*z - 40561*pow(z,2) + 6128*pow(z,3))/(27648.*Nc*z) + (pow(Nc,3)*(-455012 + 388000*z + 8689*pow(z,2) + 49168*pow(z,3)))/(27648.*z) + ((pow(Nc,3)*(-4726 - 11056/z - 9929*z))/1536. + (1270 - 2384/z - 2167*z)/(1536.*Nc) + (-1130 + 648/z + 565*z)/(4608.*pow(Nc,3)) + (Nc*(11498 + 39672/z + 35723*z))/4608.)*log(z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(1.-z),4.);
    }
    
    
    // the full log(1-z)^5 term implemented
    double qg_n3lo_r_lz5(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;

        complex<double> res =
    (-9*(2 - 2*z + pow(z,2)))/(256.*pow(Nc,3)*z) + (181*(2 - 2*z + pow(z,2)))/(768.*Nc*z) - (247*Nc*(2 - 2*z + pow(z,2)))/(256.*z) + (587*pow(Nc,3)*(2 - 2*z + pow(z,2)))/(768.*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(1.-z),5.);
    }
    
    
    
    
    double LEqgNNNLOregSte(const double& z, const double& L)
    {
        if (abs(L)<1e-15) return 0.0;

        double zz = z;
        double LL = L;
        return leqgnnnloreg_(&zz,&LL);
    }
    
    //---- Next to soft approximation (i.e. constants only in front of logs)
    double qg_n3lo_r_lz5_NS(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        
        complex<double> res =
        (-27 + 181*pow(Nc,2) - 741*pow(Nc,4) + 587*pow(Nc,6))/(768.*pow(Nc,3))
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(1.-z),5.);
    }
    
    double qg_n3lo_r_lz4_NS(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        
        complex<double> res =
        -((-1 + pow(Nc,2))*(687 - 15118*pow(Nc,2) + 9155*pow(Nc,4) + 72*L*(27 - 194*pow(Nc,2) + 739*pow(Nc,4)) + 1012*Nc*nf - 3212*pow(Nc,3)*nf))/(27648.*pow(Nc,3))
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(1.-z),4.);
    }
    
    double qg_n3lo_r_lz3_NS(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double Z2 = consts::z2;
        complex<double> res =
        ((-1 + pow(Nc,2))*(648*pow(L,2)*(3 - 26*pow(Nc,2) + 99*pow(Nc,4)) + 5264*Nc*nf - 29392*pow(Nc,3)*nf + 72*L*Nc*(-473*Nc + 639*pow(Nc,3) + 50*nf - 198*pow(Nc,2)*nf) + pow(Nc,4)*(166903 - 200952*Z2) - 3*(1899 + 4632*Z2) + 2*pow(Nc,2)*(-3085 + 168*pow(nf,2) + 37728*Z2)))/(41472.*pow(Nc,3))
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(1.-z),3.);
    }
    
    double qg_n3lo_r_lz2_NS(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double Z2 = consts::z2;
        const double Z3 = consts::z3;
        complex<double> res =
        ((-1 + pow(Nc,2))*(-432*pow(L,3)*(1 - 10*pow(Nc,2) + 37*pow(Nc,4)) - 54*pow(L,2)*(-27 - 214*pow(Nc,2) + 1045*pow(Nc,4) + 52*Nc*nf - 268*pow(Nc,3)*nf) + 4*pow(Nc,3)*nf*(6427 - 6660*Z2) + 4*Nc*nf*(-1313 + 1836*Z2) + 36*L*(243 - 198*Nc*nf + 1382*pow(Nc,3)*nf + 504*Z2 - 3*pow(Nc,2)*(57 + 8*pow(nf,2) + 1056*Z2) + 2*pow(Nc,4)*(-3733 + 4212*Z2)) + 9*(435 + 1392*Z2 + 2472*Z3) - 2*pow(Nc,2)*(-9001 + 528*pow(nf,2) + 52164*Z2 + 93636*Z3) + pow(Nc,4)*(-120073 + 124488*Z2 + 728784*Z3)))/(41472.*pow(Nc,3))
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(1.-z),2.);
    }
    
    double qg_n3lo_r_lz1_NS(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double Z2 = consts::z2;
        const double Z3 = consts::z3;
        const double Z4 = consts::z4;
        complex<double> res =
        -((-1 + pow(Nc,2))*(-2160*pow(L,3)*(-9 - pow(Nc,2) + 296*pow(Nc,4) + 10*Nc*nf - 62*pow(Nc,3)*nf) + 3240*pow(L,2)*(27 - 14*Nc*nf + 206*pow(Nc,3)*nf + 60*Z2 - pow(Nc,2)*(103 + 8*pow(nf,2) + 432*Z2) + pow(Nc,4)*(-954 + 1236*Z2)) + 20*Nc*nf*(-20051 + 34632*Z2 + 41040*Z3) - 20*pow(Nc,3)*nf*(-157411 + 33480*Z2 + 54000*Z3) + 360*L*(-8*pow(Nc,3)*nf*(-571 + 180*Z2) + 4*Nc*nf*(-19 + 234*Z2) + 27*(6*Z2 + 72*Z3) - pow(Nc,2)*(2144 + 240*pow(nf,2) + 8658*Z2 + 15228*Z3) + 2*pow(Nc,4)*(-6874 + 2826*Z2 + 30294*Z3)) - 9*(19395 + 89640*Z2 - 93600*Z3 + 32760*Z4) - 4*pow(Nc,2)*(469775 + 20880*pow(nf,2) + 835200*Z2 + 1258200*Z3 + 245430*Z4) + pow(Nc,4)*(-8205065 + 4055400*Z2 + 4890240*Z3 + 11288160*Z4)))/(1.24416e6*pow(Nc,3))
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*log(1.-z);
    }
    
   
    
    double qg_n3lo_r_lz0_NS(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double Z2 = consts::z2;
        const double Z3 = consts::z3;
        const double Z4 = consts::z4;
        const double Z5 = consts::z5;
        
        complex<double> res =
        ((-1 + pow(Nc,2))*(1080*pow(L,3)*(-27 + 60*Nc*nf + 292*pow(Nc,3)*nf + 48*Z2 - 4*pow(Nc,2)*(69 + 8*pow(nf,2) + 120*Z2) + pow(Nc,4)*(-665 + 1776*Z2)) - 540*pow(L,2)*(-36*Nc*nf*(13 + 24*Z2) + 4*pow(Nc,3)*nf*(-1327 + 96*Z2) + pow(Nc,4)*(13385 + 1632*Z2 - 31104*Z3) + 27*(17 + 24*Z2 - 48*Z3) + 4*pow(Nc,2)*(1143 + 80*pow(nf,2) + 1242*Z2 + 1980*Z3)) + 12*Nc*nf*(79555 + 115200*Z2 + 105120*Z3 + 74160*Z4) + 4*pow(Nc,3)*nf*(410855 - 533760*Z2 - 2165760*Z3 + 416880*Z4) + 18*L*(20*Nc*nf*(-429 + 4416*Z2 + 5904*Z3) - 20*pow(Nc,3)*nf*(-21013 + 6240*Z2 + 9360*Z3) - 9*(4965 + 13680*Z2 - 7920*Z3 + 4680*Z4) - 4*pow(Nc,2)*(105045 + 4000*pow(nf,2) + 105600*Z2 + 92160*Z3 + 38610*Z4) + pow(Nc,4)*(-1170655 + 708720*Z2 + 867600*Z3 + 1218240*Z4)) + pow(Nc,4)*(-7287205 + 49128480*Z3 - 3840*Z2*(-3691 + 13635*Z3) - 1401840*Z4 + 87454080*Z5) - 9*(2880*Z2*(64 + 93*Z3) + 247680*Z4 - 45*(-1699 + 2560*Z3 + 7872*Z5)) - 4*pow(Nc,2)*(320*pow(nf,2)*(125 + 54*Z3) + 3*(585085 - 253080*Z3 - 720*Z2*(-952 + 2361*Z3) + 589860*Z4 + 2088720*Z5))))/(4.97664e6*pow(Nc,3))
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res);
    }
    
    
    
    
    double qqb_nnlo_r_lz0(const double& z, const double& L)
    {
        return    qqb_nnlo_r_lz0_const(z,L)
        + qqb_nnlo_r_lz0_logz(z,L)
        + qqb_nnlo_r_lz0_logz_sq(z,L)
        + qqb_nnlo_r_lz0_logz_cube(z,L) ;
    }
    
    double qqb_nnlo_r_lz0_const(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double z2 = consts::z2;
        const double z3 = consts::z3;
        complex<double> res =-(pow(-1 + pow(Nc,2),2)*(2 + 2*z + pow(z,2))*z2*HPL(-1,z))/(8.*pow(Nc,3)*z) - (pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*z2*HPL(1,z))/(2.*pow(Nc,2)*z) + (pow(-1 + pow(Nc,2),2)*(432*z + 216*pow(z,2) + 144*pow(Nc,2)*pow(z,3))*HPL(-1,0,z))/(864.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(216 - 648*Nc + 864*L*Nc + 144*pow(Nc,2) - 864*z + 432*Nc*z + 864*L*Nc*z - 216*pow(Nc,2)*z + 864*pow(z,2) + 108*Nc*pow(z,2) + 216*L*Nc*pow(z,2) + 216*pow(Nc,2)*pow(z,2) - 360*pow(z,3))*HPL(1,0,z))/(864.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(-432 - 432*z - 216*pow(z,2))*HPL(-1,-1,0,z))/(864.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(648 + 648*z + 324*pow(z,2))*HPL(-1,0,0,z))/(864.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(-2160*Nc - 2160*Nc*z - 540*Nc*pow(z,2))*HPL(0,1,0,z))/(864.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(-2592*Nc - 2592*Nc*z - 648*Nc*pow(z,2))*HPL(1,0,0,z))/(864.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(-1728*Nc - 1728*Nc*z - 432*Nc*pow(z,2))*HPL(1,1,0,z))/(864.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(369 - 2835*Nc - 1377*L*Nc - 324*pow(L,2)*Nc + 1386*pow(Nc,2) + 396*L*pow(Nc,2) - 92*Nc*nf - 72*L*Nc*nf + 432*z + 144*L*z + 2538*Nc*z + 972*L*Nc*z + 216*pow(L,2)*Nc*z - 4698*pow(Nc,2)*z - 1332*L*pow(Nc,2)*z + 444*Nc*nf*z + 216*L*Nc*nf*z - 585*pow(z,2) - 144*L*pow(z,2) + 297*Nc*pow(z,2) + 405*L*Nc*pow(z,2) + 108*pow(L,2)*Nc*pow(z,2) + 4590*pow(Nc,2)*pow(z,2) + 1332*L*pow(Nc,2)*pow(z,2) - 516*Nc*nf*pow(z,2) - 216*L*Nc*nf*pow(z,2) - 216*pow(z,3) - 1278*pow(Nc,2)*pow(z,3) - 396*L*pow(Nc,2)*pow(z,3) + 164*Nc*nf*pow(z,3) + 72*L*Nc*nf*pow(z,3) - 288*z2 + 648*Nc*z2 + 864*L*Nc*z2 - 144*pow(Nc,2)*z2 + 864*z*z2 - 432*Nc*z*z2 + 864*L*Nc*z*z2 + 648*pow(Nc,2)*z*z2 - 540*pow(z,2)*z2 - 324*Nc*pow(z,2)*z2 + 216*L*Nc*pow(z,2)*z2 - 648*pow(Nc,2)*pow(z,2)*z2 + 144*pow(z,3)*z2 + 360*pow(Nc,2)*pow(z,3)*z2 - 432*z3 - 432*z*z3 - 216*pow(z,2)*z3))/(864.*pow(Nc,3)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res);
    }
    
    double qqb_nnlo_r_lz0_logz(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double z2 = consts::z2;
        complex<double> res =
        -(pow(-1 + pow(Nc,2),2)*(12*pow(Nc,2)*(-11 + (53 + 6*L)*z - 2*(25 + 3*L)*pow(z,2) + (22 + 4*L)*pow(z,3)) - 6*z*(32 + 25*z + 12*pow(z,2) + 4*L*(3 - 3*z + 2*pow(z,2))) + Nc*(18*pow(L,2)*pow(2 + z,2) - 18*L*(-12 + 8*z + 5*pow(z,2)) - 8*nf*(-3 + 15*z - 12*pow(z,2) + 4*pow(z,3)) - 3*(3*(-59 + 44*z + 29*pow(z,2)) + 12*pow(2 + z,2)*z2))))/(288.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*HPL(1,0,z))/(2.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*log(z);
    }
    
    double qqb_nnlo_r_lz0_logz_sq(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        complex<double> res =
        -(pow(-1 + pow(Nc,2),2)*(z*(15 + 18*z - 2*pow(z,2)) + pow(Nc,2)*z*(15 - 18*z + 10*pow(z,2)) + 3*Nc*(L*pow(2 + z,2) - 2*(-3 + 4*z + pow(z,2)))))/(48.*pow(Nc,3)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(z),2.);
    }
    
    double qqb_nnlo_r_lz0_logz_cube(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        complex<double> res =
        -(pow(-1 + pow(Nc,2),2)*pow(2 + z,2))/(48.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(z),3.);
    }
    
    double qqb_nnlo_r_lz1(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double z2 = consts::z2;

        complex<double> res =
        -(pow(-1 + pow(Nc,2),2)*((1 - z)*(12*pow(Nc,2)*((11 + 2*L)*pow(-1 + z,2) - 2*z) - 12*(3 + 2*L*pow(-1 + z,2) - 8*z + 3*pow(z,2)) - Nc*(8*nf*pow(-1 + z,2) + 72*L*(3 + z) + 27*(17 + 5*z))) + 72*Nc*pow(2 + z,2)*z2))/(144.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(4 - 18*z + 18*pow(z,2) - 8*pow(z,3) + 2*pow(Nc,2)*(-2 + 9*z - 9*pow(z,2) + 4*pow(z,3)) + 3*Nc*(12 - 8*z - 5*pow(z,2) + 2*L*pow(2 + z,2)))*HPL(0,z))/(24.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*pow(HPL(0,z),2))/(8.*pow(Nc,2)*z) - (pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*HPL(1,0,z))/(2.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*log(1.-z);
    }
    
    double qqb_nnlo_r_lz2(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        
        complex<double> res =
        -(pow(-1 + pow(Nc,2),2)*(-1 + z)*(-pow(-1 + z,2) + 3*pow(Nc,2)*pow(-1 + z,2) - 6*Nc*(3 + z)))/(12.*pow(Nc,3)*z) - (pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*HPL(0,z))/(4.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(1.-z),2.);
    }
    
    
    double LEqqbNNNLOregSte(const double& z, const double& L)
    {
        if (abs(L)<1e-15) return 0.0;
        double zz = z;
        double LL = L;
        return leqqbnnnlo_(&zz,&LL);
    }
    
    
    double LEqqbN3LOregFalko(const double& z, const double& L)
    {
        if (abs(L)<1e-15) return 0.0;

        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double z2= consts::z2;
        const double z3= consts::z3;
        const double z4= consts::z4;
        
        complex<double> L3=(pow(-1 + pow(Nc,2),2)*(-8640*pow(Nc,2)*nf*(-3 + 2*z + pow(z,2)) - 1080*Nc*(3*(-5 + z + 4*pow(z,2)) + 6*pow(2 + z,2)*z2) + 360*pow(Nc,3)*(-1438 + 1425*z - 3*pow(z,2) + 16*pow(z,3) + 54*pow(2 + z,2)*z2)))/(207360.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-1 + 3*pow(Nc,2))*pow(2 + z,2)*HPL(1,0,z))/(32.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(144*pow(Nc,2)*nf*pow(2 + z,2) - 54*Nc*z*(12 + 5*z) + 18*pow(Nc,3)*(-496 - 284*z + 55*pow(z,2)))*log(z))/(6912.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(Nc*(-36*z - 9*pow(z,2)) + 9*pow(Nc,3)*(16 - 12*z + 9*pow(z,2)))*pow(log(z),2))/(1152.*pow(Nc,4)*z) + log(1 - z)*(-(pow(-1 + pow(Nc,2),2)*(-216*Nc*(-3 + 2*z + pow(z,2)) + 648*pow(Nc,3)*(-3 + 2*z + pow(z,2))))/(3456.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-18*Nc*pow(2 + z,2) + 54*pow(Nc,3)*pow(2 + z,2))*log(z))/(576.*pow(Nc,4)*z));
        check_imaginary_part(L3,__PRETTY_FUNCTION__);
        complex<double> L2 = (pow(-1 + pow(Nc,2),2)*(36*(30*(-20*z + 20*pow(z,2)) + 60*(24*z - 24*pow(z,2) + 16*pow(z,3))*z2) + 1440*pow(Nc,4)*(121 + pow(z,2)*(455 - 36*z2) + pow(z,3)*(-121 + 24*z2) + z*(-455 + 36*z2)) - 4*pow(Nc,2)*(1440*pow(nf,2)*pow(-1 + z,3) + 90*nf*(-615 + 388*z + 227*pow(z,2) + 48*pow(2 + z,2)*z2) + 3*(20*(-642*z + 642*pow(z,2)) + 60*(144*z - 144*pow(z,2) + 96*pow(z,3))*z2)) - 120*pow(Nc,3)*(-24*nf*(-22 + 73*z - 73*pow(z,2) + 22*pow(z,3)) + 18*(-602 - 362*z + 85*pow(z,2))*z2 + 2*(11737 - 12165*z + 80*pow(z,3) + pow(z,2)*(348 - 648*z3) - 972*z3)) + 120*Nc*(2032 - 88*pow(z,3) + 108*(-15 + 5*z + 4*pow(z,2))*z2 + 3*pow(z,2)*(-9 + 56*nf - 72*z3) - 216*z3 - 3*z*(639 + 56*nf + 288*z3))))/(207360.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-60*Nc*pow(2 + z,2) + 180*pow(Nc,3)*pow(2 + z,2))*z2*HPL(1,z))/(192.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-3 - 2*z + pow(z,2))*HPL(-1,0,z))/(4.*Nc*z) + (pow(-1 + pow(Nc,2),2)*(pow(Nc,3)*(494 + 482*z - 49*pow(z,2)) - 6*Nc*(-3 + 7*z + 2*pow(z,2)) + 8*(1 + pow(z,3)) + 8*pow(Nc,4)*(1 + pow(z,3)) - 8*pow(Nc,2)*(nf*pow(2 + z,2) + 2*(1 + pow(z,3))))*HPL(1,0,z))/(96.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*HPL(0,-1,0,z))/(8.*Nc*z) + (pow(-1 + pow(Nc,2),2)*(72*Nc + 36*pow(Nc,3)*(2 - 12*z + 3*pow(z,2)))*HPL(0,1,0,z))/(192.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-9*Nc*(2 + 4*z + pow(z,2)) + 18*pow(Nc,3)*(7 + 3*pow(z,2)))*HPL(1,0,0,z))/(48.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-60*Nc*pow(2 + z,2) + 180*pow(Nc,3)*pow(2 + z,2))*HPL(1,1,0,z))/(192.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-12*(30 - 66*z)*z - 4*pow(Nc,4)*(1476*z - 1584*pow(z,2) + 924*pow(z,3)) - 8*pow(Nc,2)*(z*(-783 + 891*z - 462*pow(z,2)) + 3*nf*(-188 - 56*z + pow(z,2))) - 6*Nc*(-286 + 6*(213 + 28*nf)*z - 21*(-51 + 8*nf)*pow(z,2) + 16*(-1 + 7*nf)*pow(z,3) + 216*(2 + 4*z + pow(z,2))*z2) + 6*pow(Nc,3)*(-9216 + 2*(6275 + 84*nf)*z + (655 - 168*nf)*pow(z,2) + 16*(15 + 7*nf)*pow(z,3) + 144*(9 - 8*z + 6*pow(z,2))*z2))*log(z))/(6912.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(306*Nc*z + 12*z*(3 - 3*z + 4*pow(z,2)) + 12*pow(Nc,4)*z*(3 - 3*z + 4*pow(z,2)) - 6*pow(Nc,3)*(-328 - 201*z + 63*pow(z,2)) - 2*pow(Nc,2)*(18*nf*pow(2 + z,2) + 12*z*(3 - 3*z + 4*pow(z,2))))*pow(log(z),2))/(1152.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-30*Nc*z*(4 + z) + 6*pow(Nc,3)*(48 - 60*z + 37*pow(z,2)))*pow(log(z),3))/(2304.*pow(Nc,4)*z) + pow(log(1 - z),2)*(-(pow(-1 + pow(Nc,2),2)*(4*pow(-1 + z,3) - 8*pow(Nc,2)*pow(-1 + z,3) + 4*pow(Nc,4)*pow(-1 + z,3) + 15*Nc*(-3 + 2*z + pow(z,2)) - 45*pow(Nc,3)*(-3 + 2*z + pow(z,2))))/(48.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-60*Nc*pow(2 + z,2) + 180*pow(Nc,3)*pow(2 + z,2))*log(z))/(384.*pow(Nc,4)*z)) + log(1 - z)*(-(pow(-1 + pow(Nc,2),2)*(36*(-16*z + 16*pow(z,2)) - 24*pow(Nc,4)*(-77 + 255*z - 255*pow(z,2) + 77*pow(z,3)) - 24*pow(Nc,2)*(-1 + z)*(-77 + 202*z - 77*pow(z,2) + 24*nf*(3 + z)) - 3*Nc*(112*nf*pow(-1 + z,3) + 3*(-771 + 408*z + 363*pow(z,2) + 120*pow(2 + z,2)*z2)) + 3*pow(Nc,3)*(112*nf*pow(-1 + z,3) + 3*(-6289 + 6128*z + 97*pow(z,2) + 64*pow(z,3) + 360*pow(2 + z,2)*z2))))/(3456.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-60*Nc*pow(2 + z,2) + 180*pow(Nc,3)*pow(2 + z,2))*HPL(1,0,z))/(192.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(pow(Nc,3)*(4908 + 1596*z - 942*pow(z,2)) + 48*z*(3 - 3*z + 2*pow(z,2)) + 36*Nc*(-15 + 5*z + 4*pow(z,2)) + 8*pow(Nc,4)*(18*z - 18*pow(z,2) + 12*pow(z,3)) - 4*pow(Nc,2)*(72*z - 72*pow(z,2) + 48*pow(z,3) + 12*nf*pow(2 + z,2)))*log(z))/(576.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-9*Nc*(2 + 4*z + pow(z,2)) + 18*pow(Nc,3)*(7 + 3*pow(z,2)))*pow(log(z),2))/(96.*pow(Nc,4)*z));
        check_imaginary_part(L2,__PRETTY_FUNCTION__);
        complex<double> L1 =(pow(-1 + pow(Nc,2),2)*(120*pow(Nc,4)*(11679 - 45947*z + 45071*pow(z,2) - 10803*pow(z,3) + 12*(-154 + 951*z - 984*pow(z,2) + 484*pow(z,3))*z2 + 1080*z*z3 - 1296*pow(z,2)*z3 + 1296*pow(z,3)*z3) + 3*Nc*(288065 - 343900*z + 41035*pow(z,2) + 14800*pow(z,3) - 343560*z2 + 304560*z*z2 + 303480*pow(z,2)*z2 - 3840*pow(z,3)*z2 - 67680*z3 + 73440*z*z3 - 68040*pow(z,2)*z3 + 80*nf*(24*(5 - 6*z + 2*pow(z,3))*z2 + 3*(-52 + 33*pow(z,3) + 96*z3 + 12*pow(z,2)*(27 + 4*z3) + z*(-305 + 96*z3))) + 172800*z4 + 293760*z*z4 + 73440*pow(z,2)*z4) - pow(Nc,3)*(8545465 - 8483940*z - 240165*pow(z,2) + 178640*pow(z,3) - 2153880*z2 + 5256720*z*z2 + 196920*pow(z,2)*z2 + 138240*pow(z,3)*z2 + 80*nf*(4504 - 17511*z + 17952*pow(z,2) - 4945*pow(z,3) + 72*(-7 + 36*z - 33*pow(z,2) + 16*pow(z,3))*z2) + 626400*z3 + 997920*z*z3 + 191160*pow(z,2)*z3 + 272160*z4 - 2501280*z*z4 + 521640*pow(z,2)*z4) + 36*(60*(6 + 38*z + 55*pow(z,2) + 42*pow(z,3))*z2 + 30*(-13 + pow(z,2)*(1 - 372*z3) - 8*z3 - z*(1 + 48*z3) + pow(z,3)*(13 + 136*z3)) + 2610*z*(2 + z)*z4) - 4*pow(Nc,2)*(160*pow(nf,2)*(-1 + z)*(23 - 88*z + 41*pow(z,2)) + 60*nf*(6*(176 + 200*z + 53*pow(z,2))*z2 + z*(2687 - 216*z3) + pow(z,2)*(1027 - 54*z3) - 6*(619 + 36*z3)) + 3*(60*(238 + 168*z - 351*pow(z,2) + 478*pow(z,3))*z2 + 20*(pow(z,2)*(6325 - 1530*z3) - 2*z*(2903 - 954*z3) + 15*pow(z,3)*(43 + 84*z3) + 12*(-97 + 129*z3)) + 7830*z*(2 + z)*z4))))/(207360.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(144*pow(Nc,4)*pow(z,3)*z2 + 864*pow(Nc,3)*(-3 - 2*z + pow(z,2))*z2 - 48*Nc*nf*(2 + 2*z + pow(z,2))*z2 - 3*(6*z*(28 + 13*z)*z2 + 96*(2 + 2*z + pow(z,2))*z3) + pow(Nc,2)*(6*(88 + 172*z + 83*pow(z,2) - 24*pow(z,3))*z2 + 288*(2 + 2*z + pow(z,2))*z3))*HPL(-1,z))/(576.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(6*(3*pow(Nc,3)*(992 + 1116*z - 129*pow(z,2)) - 3*Nc*(36 + 252*z + 73*pow(z,2)) + 16*pow(Nc,4)*(7 - 6*z + 6*pow(z,2) + 4*pow(z,3)) + 8*(-4 + 24*z - 33*pow(z,2) + 16*pow(z,3)) - 4*pow(Nc,2)*(20 + 24*z - 42*pow(z,2) + 48*pow(z,3) + 9*nf*pow(2 + z,2)))*z2 + 36*(8*(2 + 2*z + pow(z,2)) - 8*pow(Nc,2)*(2 + 2*z + pow(z,2)) - Nc*(20 + 68*z + 17*pow(z,2)) + pow(Nc,3)*(148 + 4*z + 97*pow(z,2)))*z3)*HPL(1,z))/(1152.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*z2*HPL(-1,-1,z))/(2.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(4*pow(Nc,4)*z*(-3 + 6*z + 44*pow(z,2)) - 4*Nc*(22 + 3*(3 + 8*nf)*z + 3*(-3 + 4*nf)*pow(z,2) + 4*pow(z,3)) + pow(Nc,3)*(-1071 - 792*z + 279*pow(z,2) - 32*nf*pow(z,3)) + 6*(-9*pow(z,2) + 24*(2 + 2*z + pow(z,2))*z2) - 6*pow(Nc,2)*(-(z*(90 + 49*z)) + 24*(2 + 2*z + pow(z,2))*z2))*HPL(-1,0,z))/(288.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*z2*HPL(-1,1,z))/(8.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(12*pow(Nc,3)*pow(-2 + z,2) + z*(2 + z) - pow(Nc,2)*z*(2 + z))*z2*HPL(0,-1,z))/(16.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(24 + pow(Nc,2)*(4 - 132*z + 31*pow(z,2)))*z2*HPL(0,1,z))/(16.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*z2*HPL(1,-1,z))/(8.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(36*(4 + 4*z + 13*pow(z,2) + 4*pow(z,3)) + 8*pow(Nc,4)*(88 - 60*z + 42*pow(z,2) + 55*pow(z,3)) - 4*pow(Nc,2)*(-8 + 576*z - 459*pow(z,2) + 366*pow(z,3) + nf*(104 + 248*z + 77*pow(z,2))) + Nc*(1311 + 234*z + 747*pow(z,2) - 16*pow(z,3) + 16*nf*(-6 + 33*z - 30*pow(z,2) + 13*pow(z,3)) + 72*(16 + 28*z + 7*pow(z,2))*z2) - 2*pow(Nc,3)*(4408 - 1291*z + 770*pow(z,2) + 96*pow(z,3) + 8*nf*(4 + 3*z + 3*pow(z,3)) + 36*(48 - 20*z + 27*pow(z,2))*z2))*HPL(1,0,z))/(576.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-49 + 143*pow(Nc,2))*pow(2 + z,2)*z2*HPL(1,1,z))/(32.*pow(Nc,3)*z) - (pow(-1 + pow(Nc,2),2)*(8*pow(Nc,4)*pow(z,3) - 3*z*(12 + 5*z) + 48*pow(Nc,3)*(-3 - 2*z + pow(z,2)) - 8*Nc*nf*(2 + 2*z + pow(z,2)) + pow(Nc,2)*(88 + 124*z + 59*pow(z,2) - 8*pow(z,3)))*HPL(-1,-1,0,z))/(48.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(16*pow(Nc,4)*pow(z,3) - 3*z*(28 + 11*z) + 120*pow(Nc,3)*(-3 - 2*z + pow(z,2)) - 24*Nc*nf*(2 + 2*z + pow(z,2)) + pow(Nc,2)*(264 + 348*z + 165*pow(z,2) - 16*pow(z,3)))*HPL(-1,0,0,z))/(96.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(2*pow(Nc,4)*pow(z,3) - 3*z*(2 + z) + pow(Nc,2)*z*(6 + 3*z - 2*pow(z,2)) + 12*pow(Nc,3)*(-3 - 2*z + pow(z,2)))*HPL(-1,1,0,z))/(12.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(4*pow(Nc,4)*pow(z,3) - 6*z*(2 + z) - 3*pow(Nc,3)*(15 + 6*z - 5*pow(z,2)) + Nc*(-4 + 6*z - 3*pow(z,2)) + 2*pow(Nc,2)*z*(6 + 3*z - 2*pow(z,2)))*HPL(0,-1,0,z))/(24.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(Nc*(344 + 276*z - 147*pow(z,2)) + pow(Nc,3)*(-3436 - 1908*z - 63*pow(z,2)) + 8*pow(Nc,4)*(4 - 6*z + 3*pow(z,2) + 2*pow(z,3)) + 8*(4 - 21*z - 12*pow(z,2) + 4*pow(z,3)) + 8*pow(Nc,2)*(-8 + 27*z + 9*pow(z,2) - 6*pow(z,3) + 9*nf*pow(2 + z,2)))*HPL(0,1,0,z))/(192.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(2*pow(Nc,4)*pow(z,3) - 3*z*(2 + z) + pow(Nc,2)*z*(6 + 3*z - 2*pow(z,2)) + 12*pow(Nc,3)*(-3 - 2*z + pow(z,2)))*HPL(1,-1,0,z))/(12.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-4 + 6*z - 72*pow(z,2) + 28*pow(z,3) + pow(Nc,3)*(-370 - 204*z - 237*pow(z,2)) - 6*Nc*(-6 - 2*z + 7*pow(z,2)) + pow(Nc,4)*(20 - 6*z + 20*pow(z,3)) + 8*pow(Nc,2)*(-2 + 9*pow(z,2) - 6*pow(z,3) + 3*nf*pow(2 + z,2)))*HPL(1,0,0,z))/(48.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(3*pow(Nc,3)*(1088 + 1180*z - 161*pow(z,2)) - 3*Nc*(36 + 252*z + 73*pow(z,2)) + 16*pow(Nc,4)*(7 - 6*z + 6*pow(z,2) + 3*pow(z,3)) + 16*(-2 + 15*z - 15*pow(z,2) + 8*pow(z,3)) - 4*pow(Nc,2)*(9*nf*pow(2 + z,2) + 4*(5 + 9*z - 9*pow(z,2) + 11*pow(z,3))))*HPL(1,1,0,z))/(192.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*HPL(-1,-1,-1,0,z))/(2.*pow(Nc,4)*z) - (5*pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*HPL(-1,-1,0,0,z))/(8.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*HPL(-1,-1,1,0,z))/(4.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*HPL(-1,0,-1,0,z))/(4.*pow(Nc,4)*z) + (3*pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*HPL(-1,0,0,0,z))/(8.*pow(Nc,4)*z) - (3*pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*HPL(-1,0,1,0,z))/(8.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*HPL(-1,1,-1,0,z))/(4.*pow(Nc,4)*z) - (3*pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*HPL(-1,1,0,0,z))/(8.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(4*pow(Nc,3)*pow(-2 + z,2) + z*(2 + z) - pow(Nc,2)*z*(2 + z))*HPL(0,-1,-1,0,z))/(8.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(10*pow(Nc,3)*pow(-2 + z,2) + 3*z*(2 + z) - 3*pow(Nc,2)*z*(2 + z))*HPL(0,-1,0,0,z))/(16.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*HPL(0,-1,1,0,z))/(2.*Nc*z) - (pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*HPL(0,0,-1,0,z))/(4.*Nc*z) - (pow(-1 + pow(Nc,2),2)*(-24 - 20*z - 5*pow(z,2) + pow(Nc,2)*(104 + 52*z + 21*pow(z,2)))*HPL(0,0,1,0,z))/(32.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*HPL(0,1,-1,0,z))/(2.*Nc*z) + (pow(-1 + pow(Nc,2),2)*(16 - 4*z - pow(z,2) + 4*pow(Nc,2)*(-1 - 28*z + 6*pow(z,2)))*HPL(0,1,0,0,z))/(16.*pow(Nc,3)*z) - (pow(-1 + pow(Nc,2),2)*(24 + pow(Nc,2)*(20 - 148*z + 35*pow(z,2)))*HPL(0,1,1,0,z))/(16.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*HPL(1,-1,-1,0,z))/(4.*pow(Nc,4)*z) - (3*pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*HPL(1,-1,0,0,z))/(8.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*HPL(1,0,-1,0,z))/(2.*Nc*z) + (pow(-1 + pow(Nc,2),2)*(-2*(3 + 8*z + 2*pow(z,2)) + pow(Nc,2)*(42 - 12*z + 23*pow(z,2)))*HPL(1,0,0,0,z))/(8.*pow(Nc,3)*z) - (3*pow(-1 + pow(Nc,2),2)*(2 + pow(Nc,2)*(2 - 12*z + 3*pow(z,2)))*HPL(1,0,1,0,z))/(4.*pow(Nc,3)*z) - (pow(-1 + pow(Nc,2),2)*(-8*(5 + 8*z + 2*pow(z,2)) + pow(Nc,2)*(212 + 44*z + 83*pow(z,2)))*HPL(1,1,0,0,z))/(16.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(-49 + 143*pow(Nc,2))*pow(2 + z,2)*HPL(1,1,1,0,z))/(32.*pow(Nc,3)*z) + ((pow(-1 + pow(Nc,2),2)*(-4*pow(Nc,4)*(-2904 - 3*pow(z,2)*(6667 - 720*z2) + 3*z*(7057 - 648*z2) + 4*pow(z,3)*(2270 - 396*z2)) - Nc*(-12492 + 68058*z + 5712*nf*z + 19545*pow(z,2) + 192*nf*pow(z,2) + 2464*pow(z,3) + 2784*nf*pow(z,3) + 36*(392 - 324*z + 57*pow(z,2))*z2 + 3456*z*z3 + 864*pow(z,2)*z3) + pow(Nc,3)*(-193320 - 4224*nf + 299966*z + 23376*nf*z - 16369*pow(z,2) - 20256*nf*pow(z,2) + 864*pow(z,3) + 8672*nf*pow(z,3) + 23040*z2 + 11952*z*z2 - 9756*pow(z,2)*z2 + 5184*z3 + 17280*z*z3 + 5616*pow(z,2)*z3) - 12*z*(48*(15 + 6*z + pow(z,2))*z2 - 3*(57 + 241*z + 58*pow(z,2) + 48*z3 + 24*z*z3)) - 8*pow(Nc,2)*(16*pow(nf,2)*(-3 + 15*z - 12*pow(z,2) + 4*pow(z,3)) + nf*(-2213 + 460*z + 724*pow(z,2) + 36*pow(2 + z,2)*z2) + z*(-5445 + 2835*z - 2167*pow(z,2) + 36*(-3 - 42*z + 20*pow(z,2))*z2 + 216*z3 + 108*z*z3))))/(6912.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(11*Nc - 2*nf)*pow(2 + z,2)*HPL(1,0,z))/(6.*pow(Nc,2)*z))*log(z) - (pow(-1 + pow(Nc,2),2)*(12*z*(20 + 19*z + 12*pow(z,2)) + 2*pow(Nc,4)*z*(1062 - 1257*z + 748*pow(z,2)) - 2*pow(Nc,2)*(2*nf*(188 + 8*z + 5*pow(z,2)) + 3*z*(-46 - 381*z + 156*pow(z,2))) + Nc*(-286 + (1125 - 96*nf)*z - 3*(-519 + 136*nf)*pow(z,2) + 112*(-1 + nf)*pow(z,3) + 18*(24 + 52*z + 13*pow(z,2))*z2) + pow(Nc,3)*(9176 - 18385*z - 384*nf*z - 559*pow(z,2) + 408*nf*pow(z,2) - 288*pow(z,3) - 240*nf*pow(z,3) - 18*(40 - 44*z + 45*pow(z,2))*z2))*pow(log(z),2))/(1152.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-3*Nc*z*(-228 + 49*z) + pow(Nc,3)*(2624 + 1860*z - 309*pow(z,2)) + 32*z*(-9 - 6*z + pow(z,2)) + 24*pow(Nc,4)*z*(6 - 7*z + 8*pow(z,2)) - 8*pow(Nc,2)*(6*nf*pow(2 + z,2) + z*(-18 - 45*z + 28*pow(z,2))))*pow(log(z),3))/(2304.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-11*z*(4 + z) + pow(Nc,2)*(96 - 132*z + 79*pow(z,2)))*pow(log(z),4))/(1536.*pow(Nc,3)*z) + pow(log(1 - z),3)*((pow(-1 + pow(Nc,2),2)*(8*pow(-1 + z,3) - 32*pow(Nc,2)*pow(-1 + z,3) + 24*pow(Nc,4)*pow(-1 + z,3) + 49*Nc*(-3 + 2*z + pow(z,2)) - 143*pow(Nc,3)*(-3 + 2*z + pow(z,2))))/(96.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-49 + 143*pow(Nc,2))*pow(2 + z,2)*log(z))/(192.*pow(Nc,3)*z)) + pow(log(1 - z),2)*(-(pow(-1 + pow(Nc,2),2)*(288*(-1 + 4*z - 4*pow(z,2) + pow(z,3)) + 96*pow(Nc,4)*(-22 + 71*z - 71*pow(z,2) + 22*pow(z,3)) + 8*pow(Nc,2)*(-1 + z)*(27*nf*(3 + z) - 4*(53 - 130*z + 53*pow(z,2))) + Nc*(128*nf*pow(-1 + z,3) + 27*(-265 + 148*z + 117*pow(z,2)) + 882*pow(2 + z,2)*z2) - pow(Nc,3)*(256*nf*pow(-1 + z,3) + 3*(-12791 + 12492*z + 171*pow(z,2) + 128*pow(z,3) + 858*pow(2 + z,2)*z2))))/(1152.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-49 + 143*pow(Nc,2))*pow(2 + z,2)*HPL(1,0,z))/(64.*pow(Nc,3)*z) - (pow(-1 + pow(Nc,2),2)*(3*pow(Nc,3)*(2240 + 412*z - 545*pow(z,2)) - 3*Nc*(420 - 4*z - 55*pow(z,2)) + 16*(-4 + 21*z - 21*pow(z,2) + 10*pow(z,3)) + 16*pow(Nc,4)*(-4 + 27*z - 27*pow(z,2) + 14*pow(z,3)) - 4*pow(Nc,2)*(9*nf*pow(2 + z,2) + 32*(-1 + 6*z - 6*pow(z,2) + 3*pow(z,3))))*log(z))/(384.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-8*(5 + 8*z + 2*pow(z,2)) + pow(Nc,2)*(212 + 44*z + 83*pow(z,2)))*pow(log(z),2))/(64.*pow(Nc,3)*z)) + log(1 - z)*(-(pow(-1 + pow(Nc,2),2)*(-4*pow(Nc,4)*(-4603 + 16011*z - 15993*pow(z,2) + 4585*pow(z,3) - 72*(-4 + 27*z - 27*pow(z,2) + 15*pow(z,3))*z2) - 4*pow(Nc,2)*(2167 + 32*pow(nf,2)*pow(-1 + z,3) - 10548*z + 10593*pow(z,2) - 2212*pow(z,3) - 72*z2 + 1728*z*z2 - 2052*pow(z,2)*z2 + 1296*pow(z,3)*z2 + 3*nf*(-885 + 544*z + 341*pow(z,2) + 54*pow(2 + z,2)*z2) + 432*z3 + 432*z*z3 + 216*pow(z,2)*z3) - Nc*(-40448 + 31698*z + 8046*pow(z,2) + 704*pow(z,3) + 48*nf*(-23 + 95*z - 101*pow(z,2) + 29*pow(z,3)) + 20088*z2 + 1512*z*z2 - 2106*pow(z,2)*z2 + 2160*z3 + 7344*z*z3 + 1836*pow(z,2)*z3) + pow(Nc,3)*(-193858 + 201054*z - 5916*pow(z,2) - 1280*pow(z,3) + 16*nf*(-209 + 705*z - 723*pow(z,2) + 227*pow(z,3)) + 83376*z2 + 40392*z*z2 - 16902*pow(z,2)*z2 + 15984*z3 + 432*z*z3 + 10476*pow(z,2)*z3) + 36*(-36 + 29*pow(z,3) + 6*(4 - 4*z - 2*pow(z,2) + 4*pow(z,3))*z2 + 48*z3 + 8*pow(z,2)*(13 + 3*z3) + z*(-97 + 48*z3))))/(3456.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*z2*HPL(-1,z))/(8.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-49 + 143*pow(Nc,2))*pow(2 + z,2)*z2*HPL(1,z))/(32.*pow(Nc,3)*z) - (pow(-1 + pow(Nc,2),2)*(2*pow(Nc,4)*pow(z,3) - 3*z*(2 + z) + pow(Nc,2)*z*(6 + 3*z - 2*pow(z,2)) + 12*pow(Nc,3)*(-3 - 2*z + pow(z,2)))*HPL(-1,0,z))/(12.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(3*pow(Nc,3)*(1112 + 1164*z - 169*pow(z,2)) - 3*Nc*(36 + 252*z + 73*pow(z,2)) + 16*pow(Nc,4)*(7 - 6*z + 6*pow(z,2) + 3*pow(z,3)) + 16*(-2 + 15*z - 15*pow(z,2) + 8*pow(z,3)) - 4*pow(Nc,2)*(9*nf*pow(2 + z,2) + 4*(5 + 9*z - 9*pow(z,2) + 11*pow(z,3))))*HPL(1,0,z))/(192.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*HPL(-1,-1,0,z))/(4.*pow(Nc,4)*z) - (3*pow(-1 + pow(Nc,2),3)*(2 + 2*z + pow(z,2))*HPL(-1,0,0,z))/(8.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*HPL(0,-1,0,z))/(2.*Nc*z) - (3*pow(-1 + pow(Nc,2),2)*(2 + pow(Nc,2)*(2 - 12*z + 3*pow(z,2)))*HPL(0,1,0,z))/(4.*pow(Nc,3)*z) - (pow(-1 + pow(Nc,2),2)*(-8*(5 + 8*z + 2*pow(z,2)) + pow(Nc,2)*(212 + 44*z + 83*pow(z,2)))*HPL(1,0,0,z))/(16.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(-49 + 143*pow(Nc,2))*pow(2 + z,2)*HPL(1,1,0,z))/(32.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(12*z*(56 - 5*z + 24*pow(z,2)) + 8*pow(Nc,4)*(-77 + 477*z - 495*pow(z,2) + 220*pow(z,3)) - 4*pow(Nc,2)*(-154 + 1122*z - 1005*pow(z,2) + 512*pow(z,3) + nf*(284 + 128*z + 17*pow(z,2))) + pow(Nc,3)*(19705 - 25672*z - 1519*pow(z,2) - 480*pow(z,3) - 16*nf*(-7 + 36*z - 33*pow(z,2) + 14*pow(z,3)) - 3456*z2 + 1440*z*z2 - 1944*pow(z,2)*z2) + Nc*(-2867 + 2520*z + 2655*pow(z,2) - 32*pow(z,3) + 16*nf*(-7 + 36*z - 33*pow(z,2) + 14*pow(z,3)) + 1152*z2 + 2016*z*z2 + 504*pow(z,2)*z2))*log(z))/(576.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-3*Nc*(30 - 32*z) + pow(Nc,3)*(730 + 236*z - 193*pow(z,2)) + 6*z*(-1 - 10*z + 4*pow(z,2)) + 2*pow(Nc,4)*z*(27 - 30*z + 20*pow(z,2)) - 8*pow(Nc,2)*(nf*pow(2 + z,2) + z*(6 - 15*z + 8*pow(z,2))))*pow(log(z),2))/(96.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-2*(3 + 8*z + 2*pow(z,2)) + pow(Nc,2)*(42 - 12*z + 23*pow(z,2)))*pow(log(z),3))/(48.*pow(Nc,3)*z));
        check_imaginary_part(L1,__PRETTY_FUNCTION__);
        return real(L3)*pow(L,3)+real(L2)*pow(L,2)+real(L1)*L;
    }
    
    
    double qq_nnlo_r_lz0(const double& z, const double& L)
    {
        return    qq_nnlo_r_lz0_const(z,L)
        + qq_nnlo_r_lz0_logz(z,L)
        + qq_nnlo_r_lz0_logz_sq(z,L)
        + qq_nnlo_r_lz0_logz_cube(z,L) ;
    }
    
    double qq_nnlo_r_lz0_const(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        const double z2 = consts::z2;
        const double z3 = consts::z3;
        complex<double> res =
        -((pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*z2*HPL(1,z))/(pow(Nc,2)*z)) + (pow(-1 + pow(Nc,2),2)*(20 - 3*pow(z,2) + 2*L*pow(2 + z,2))*HPL(0,1,z))/(8.*pow(Nc,2)*z) + (pow(-1 + pow(Nc,2),2)*(168*Nc + 192*L*Nc + 48*Nc*z + 192*L*Nc*z - 24*Nc*pow(z,2) + 48*L*Nc*pow(z,2))*HPL(1,0,z))/(96.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(96*Nc + 96*Nc*z + 24*Nc*pow(z,2))*HPL(0,0,1,z))/(96.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(-144*Nc - 144*Nc*z - 36*Nc*pow(z,2))*HPL(0,1,0,z))/(96.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(192*Nc + 192*Nc*z + 48*Nc*pow(z,2))*HPL(0,1,1,z))/(96.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(24 - 192*Nc - 24*z - 192*Nc*z + 12*pow(z,2) - 48*Nc*pow(z,2))*HPL(1,0,0,z))/(96.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(-384*Nc - 384*Nc*z - 96*Nc*pow(z,2))*HPL(1,1,0,z))/(96.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(-15 - 315*Nc - 153*L*Nc - 36*pow(L,2)*Nc - 48*z + 282*Nc*z + 108*L*Nc*z + 24*pow(L,2)*Nc*z + 63*pow(z,2) + 33*Nc*pow(z,2) + 45*L*Nc*pow(z,2) + 12*pow(L,2)*Nc*pow(z,2) + 72*Nc*z2 + 96*L*Nc*z2 - 48*Nc*z*z2 + 96*L*Nc*z*z2 - 36*Nc*pow(z,2)*z2 + 24*L*Nc*pow(z,2)*z2 - 24*z3 + 24*z*z3 - 12*pow(z,2)*z3))/(96.*pow(Nc,3)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res);
    }
    
    double qq_nnlo_r_lz0_logz(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        const double z2 = consts::z2;
        complex<double> res =
        -(pow(-1 + pow(Nc,2),2)*(6*z*(4 + 9*z) + Nc*(6*pow(L,2)*pow(2 + z,2) - 6*L*(-12 + 8*z + 5*pow(z,2)) - 3*(-59 + 44*z + 29*pow(z,2)) - 12*pow(2 + z,2)*z2)))/(96.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*HPL(1,0,z))/(2.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*log(z);
    }
    
    double qq_nnlo_r_lz0_logz_sq(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        complex<double> res =
        -(pow(-1 + pow(Nc,2),2)*(-(z*(2 + 3*z)) + Nc*(L*pow(2 + z,2) - 2*(-3 + 4*z + pow(z,2)))))/(16.*pow(Nc,3)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(z),2.);
    }
    
    double qq_nnlo_r_lz0_logz_cube(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        complex<double> res =
        -(pow(-1 + pow(Nc,2),2)*pow(2 + z,2))/(48.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(z),3.);
    }
    
    double qq_nnlo_r_lz1(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        const double z2 = consts::z2;
        
        complex<double> res =
        (pow(-1 + pow(Nc,2),2)*(153 + 72*L - 108*z - 48*L*z - 45*pow(z,2) - 24*L*pow(z,2) - 192*z2 - 192*z*z2 - 48*pow(z,2)*z2))/(48.*pow(Nc,2)*z) + (pow(-1 + pow(Nc,2),2)*(L*pow(2 + z,2) - 2*(-4 + z + pow(z,2)))*HPL(0,z))/(2.*pow(Nc,2)*z) + (pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*pow(HPL(0,z),2))/(4.*pow(Nc,2)*z) - (pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*HPL(1,0,z))/(pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*log(1.-z);
    }
    
    double qq_nnlo_r_lz2(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        
        complex<double> res =
        -(pow(-1 + pow(Nc,2),2)*(3 - 2*z - pow(z,2)))/(2.*pow(Nc,2)*z) - (pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*HPL(0,z))/(2.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(1.-z),2.);
    }
    
    
    double LEqqNNNLOregSte(const double& z, const double& L)
    {
        if (abs(L)<1e-15) return 0.0;
        double zz = z;
        double LL = L;
        return leqqnnnlo_(&zz,&LL);
    }
    
    double LEqqN3LOregFalko(const double& z, const double& L)
    {
        if (abs(L)<1e-15) return 0.0;
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double z2= consts::z2;
        const double z3= consts::z3;
        const double z4= consts::z4;
        
        complex<double> L3=-(pow(-1 + pow(Nc,2),2)*(8640*pow(Nc,2)*nf*(-3 + 2*z + pow(z,2)) + 1080*Nc*(3*(-5 + z + 4*pow(z,2)) + 6*pow(2 + z,2)*z2) - 360*pow(Nc,3)*(-1438 + 1425*z - 3*pow(z,2) + 16*pow(z,3) + 54*pow(2 + z,2)*z2)))/(207360.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-18*Nc*pow(2 + z,2) + 54*pow(Nc,3)*pow(2 + z,2))*HPL(1,0,z))/(576.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(144*pow(Nc,2)*nf*pow(2 + z,2) - 54*Nc*z*(12 + 5*z) + 18*pow(Nc,3)*(-496 - 284*z + 55*pow(z,2)))*log(z))/(6912.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(Nc*(-36*z - 9*pow(z,2)) + 9*pow(Nc,3)*(16 - 12*z + 9*pow(z,2)))*pow(log(z),2))/(1152.*pow(Nc,4)*z) + log(1 - z)*(-(pow(-1 + pow(Nc,2),2)*(-216*Nc*(-3 + 2*z + pow(z,2)) + 648*pow(Nc,3)*(-3 + 2*z + pow(z,2))))/(3456.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-18*Nc*pow(2 + z,2) + 54*pow(Nc,3)*pow(2 + z,2))*log(z))/(576.*pow(Nc,4)*z));
        check_imaginary_part(L3,__PRETTY_FUNCTION__);
        complex<double> L2 = -(pow(-1 + pow(Nc,2),2)*(360*pow(Nc,2)*nf*(-615 + 388*z + 227*pow(z,2) + 48*pow(2 + z,2)*z2) + 120*pow(Nc,3)*(18*(-602 - 362*z + 85*pow(z,2))*z2 + 2*(11737 - 12165*z + 80*pow(z,3) + pow(z,2)*(348 - 648*z3) - 972*z3)) - 120*Nc*(2032 - 88*pow(z,3) + 108*(-15 + 5*z + 4*pow(z,2))*z2 - 216*z3 - 27*pow(z,2)*(1 + 8*z3) - 27*z*(71 + 32*z3))))/(207360.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-120*Nc*pow(2 + z,2)*z2 + 360*pow(Nc,3)*pow(2 + z,2)*z2)*HPL(1,z))/(384.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-3 + z)*(1 + z)*HPL(-1,0,z))/(4.*Nc*z) + (pow(-1 + pow(Nc,2),2)*(-48*pow(Nc,2)*nf*pow(2 + z,2) - 36*Nc*(-3 + 7*z + 2*pow(z,2)) - 6*pow(Nc,3)*(-494 - 482*z + 49*pow(z,2)))*HPL(1,0,z))/(576.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*HPL(0,-1,0,z))/(8.*Nc*z) + (pow(-1 + pow(Nc,2),2)*(72*Nc + 36*pow(Nc,3)*(2 - 12*z + 3*pow(z,2)))*HPL(0,1,0,z))/(192.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-18*Nc*(2 + 4*z + pow(z,2)) + 36*pow(Nc,3)*(7 + 3*pow(z,2)))*HPL(1,0,0,z))/(96.*pow(Nc,4)*z) - (5*pow(-1 + pow(Nc,2),2)*(-1 + 3*pow(Nc,2))*pow(2 + z,2)*HPL(1,1,0,z))/(16.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(-24*pow(Nc,2)*nf*(-188 - 56*z + pow(z,2)) - 6*Nc*(-286 + 1278*z + 1071*pow(z,2) - 16*pow(z,3) + 216*(2 + 4*z + pow(z,2))*z2) + 6*pow(Nc,3)*(-9216 + 12550*z + 655*pow(z,2) + 240*pow(z,3) + 144*(9 - 8*z + 6*pow(z,2))*z2))*log(z))/(6912.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(306*Nc*z - 36*pow(Nc,2)*nf*pow(2 + z,2) - 6*pow(Nc,3)*(-328 - 201*z + 63*pow(z,2)))*pow(log(z),2))/(1152.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-30*Nc*z*(4 + z) + 6*pow(Nc,3)*(48 - 60*z + 37*pow(z,2)))*pow(log(z),3))/(2304.*pow(Nc,4)*z) + pow(log(1 - z),2)*((5*pow(-1 + pow(Nc,2),2)*(-1 + 3*pow(Nc,2))*(-3 + 2*z + pow(z,2)))/(16.*pow(Nc,3)*z) - (5*pow(-1 + pow(Nc,2),2)*(-1 + 3*pow(Nc,2))*pow(2 + z,2)*log(z))/(32.*pow(Nc,3)*z)) + log(1 - z)*(-(pow(-1 + pow(Nc,2),2)*(-576*pow(Nc,2)*nf*(-3 + 2*z + pow(z,2)) - 9*Nc*(-771 + 408*z + 363*pow(z,2) + 120*pow(2 + z,2)*z2) + 9*pow(Nc,3)*(-6289 + 6128*z + 97*pow(z,2) + 64*pow(z,3) + 360*pow(2 + z,2)*z2)))/(3456.*pow(Nc,4)*z) - (5*pow(-1 + pow(Nc,2),2)*(-1 + 3*pow(Nc,2))*pow(2 + z,2)*HPL(1,0,z))/(16.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(-48*pow(Nc,2)*nf*pow(2 + z,2) + pow(Nc,3)*(4908 + 1596*z - 942*pow(z,2)) + 36*Nc*(-15 + 5*z + 4*pow(z,2)))*log(z))/(576.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-9*Nc*(2 + 4*z + pow(z,2)) + 18*pow(Nc,3)*(7 + 3*pow(z,2)))*pow(log(z),2))/(96.*pow(Nc,4)*z));
        check_imaginary_part(L2,__PRETTY_FUNCTION__);
        complex<double> L1 =-(pow(-1 + pow(Nc,2),2)*(-72*(60*(3 + z - 14*pow(z,2) + 3*pow(z,3))*z2 + 15*(-13 - 8*z3 + pow(z,3)*(13 + 40*z3) + z*(-43 + 72*z3) + pow(z,2)*(43 + 126*z3)) + 540*(-2 + z)*z*z4) - 3*Nc*(288065 - 343900*z + 41035*pow(z,2) + 14800*pow(z,3) - 343560*z2 + 304560*z*z2 + 303480*pow(z,2)*z2 - 3840*pow(z,3)*z2 - 67680*z3 + 73440*z*z3 - 68040*pow(z,2)*z3 + 1440*nf*(5 - 8*z*(-2 + z3) + 8*z3 + pow(z,2)*(-21 + 4*z3)) + 172800*z4 + 293760*z*z4 + 73440*pow(z,2)*z4) + pow(Nc,3)*(360*(-5983 + 14602*z + 547*pow(z,2) + 384*pow(z,3))*z2 + 5*(1709093 + 35728*pow(z,3) + 125280*z3 + 81*pow(z,2)*(-593 + 472*z3) + 324*z*(-5237 + 616*z3)) + 3240*(84 - 772*z + 161*pow(z,2))*z4) + 24*pow(Nc,2)*(10*nf*(6*(176 + 200*z + 53*pow(z,2))*z2 + z*(2687 - 216*z3) + pow(z,2)*(1027 - 54*z3) - 6*(619 + 36*z3)) + 3*(60*(3 + z - 14*pow(z,2) + 3*pow(z,3))*z2 + 15*(97 + z*(309 - 104*z3) + 168*z3 + pow(z,3)*(13 + 40*z3) + pow(z,2)*(-419 + 214*z3)) + 540*(-2 + z)*z*z4))))/(207360.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(1 + z)*(18*pow(Nc,3)*(-3 + z) + pow(1 + z,2) - pow(Nc,2)*pow(1 + z,2))*z2*HPL(-1,z))/(12.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-48*(2 - 2*z + pow(z,2))*z3 - 12*pow(Nc,2)*(6*nf*pow(2 + z,2)*z2 - 4*(2 - 2*z + pow(z,2))*z3) - Nc*(6*(36 + 252*z + 73*pow(z,2))*z2 + 12*(20 + 68*z + 17*pow(z,2))*z3) + pow(Nc,3)*(6*(992 + 1116*z - 129*pow(z,2))*z2 + 12*(148 + 4*z + 97*pow(z,2))*z3))*HPL(1,z))/(384.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(1 + z)*(9*pow(Nc,3)*(-119 + 31*z) + 12*(3 + 10*z + 3*pow(z,2)) - 12*pow(Nc,2)*(3 + 10*z + 3*pow(z,2)) - 4*Nc*(22 - 13*z + 4*pow(z,2)))*HPL(-1,0,z))/(288.*pow(Nc,4)*z) + (3*pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*z2*HPL(0,-1,z))/(4.*Nc*z) - (pow(-1 + pow(Nc,2),2)*(24 + pow(Nc,2)*(4 - 132*z + 31*pow(z,2)))*z2*HPL(0,1,z))/(16.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(12*(-3*z*(4 + 9*z) + 6*(2 - 2*z + pow(z,2))*z2) - 4*pow(Nc,2)*(-9*z*(4 + 9*z) + nf*(104 + 248*z + 77*pow(z,2)) + 18*(2 - 2*z + pow(z,2))*z2) + Nc*(1311 + 234*z + 747*pow(z,2) - 16*pow(z,3) + 72*(16 + 28*z + 7*pow(z,2))*z2) + 2*pow(Nc,3)*(-4408 + 1291*z - 770*pow(z,2) - 96*pow(z,3) - 36*(48 - 20*z + 27*pow(z,2))*z2))*HPL(1,0,z))/(576.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-49 + 143*pow(Nc,2))*pow(2 + z,2)*z2*HPL(1,1,z))/(32.*pow(Nc,3)*z) - (pow(-1 + pow(Nc,2),2)*(1 + z)*(6*pow(Nc,3)*(-3 + z) + pow(1 + z,2) - pow(Nc,2)*pow(1 + z,2))*HPL(-1,-1,0,z))/(6.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(1 + z)*(15*pow(Nc,3)*(-3 + z) + pow(1 + z,2) - pow(Nc,2)*pow(1 + z,2))*HPL(-1,0,0,z))/(12.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-3 - 2*z + pow(z,2))*HPL(-1,1,0,z))/(Nc*z) - (pow(-1 + pow(Nc,2),2)*(3*pow(Nc,3)*(15 + 6*z - 5*pow(z,2)) - 2*z*(3 + 3*z + 2*pow(z,2)) + 2*pow(Nc,2)*z*(3 + 3*z + 2*pow(z,2)) + Nc*(4 - 6*z + 3*pow(z,2)))*HPL(0,-1,0,z))/(24.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(24*z*(2 + 3*z) + Nc*(344 + 276*z - 147*pow(z,2)) + pow(Nc,3)*(-3436 - 1908*z - 63*pow(z,2)) + 24*pow(Nc,2)*(3*nf*pow(2 + z,2) - z*(2 + 3*z)))*HPL(0,1,0,z))/(192.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-3 - 2*z + pow(z,2))*HPL(1,-1,0,z))/(Nc*z) + (pow(-1 + pow(Nc,2),2)*(3*z*(12 + 11*z) + 2*pow(Nc,3)*(-370 - 204*z - 237*pow(z,2)) + pow(Nc,2)*(88 - 124*z + 11*pow(z,2) + 48*nf*pow(2 + z,2)) - 2*Nc*(4*nf*(2 - 2*z + pow(z,2)) + 6*(-6 - 2*z + 7*pow(z,2))))*HPL(1,0,0,z))/(96.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-36 - 252*z - 73*pow(z,2) - 12*Nc*nf*pow(2 + z,2) + pow(Nc,2)*(1088 + 1180*z - 161*pow(z,2)))*HPL(1,1,0,z))/(64.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*HPL(0,-1,-1,0,z))/(2.*Nc*z) - (5*pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*HPL(0,-1,0,0,z))/(8.*Nc*z) + (pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*HPL(0,-1,1,0,z))/(2.*Nc*z) - (pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*HPL(0,0,-1,0,z))/(4.*Nc*z) - (pow(-1 + pow(Nc,2),2)*(-24 - 20*z - 5*pow(z,2) + pow(Nc,2)*(104 + 52*z + 21*pow(z,2)))*HPL(0,0,1,0,z))/(32.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*HPL(0,1,-1,0,z))/(2.*Nc*z) + (pow(-1 + pow(Nc,2),2)*(-((-2 + z)*z) + pow(Nc,2)*(-2 + z)*z - Nc*(-16 + 4*z + pow(z,2)) + 4*pow(Nc,3)*(-1 - 28*z + 6*pow(z,2)))*HPL(0,1,0,0,z))/(16.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(24 + pow(Nc,2)*(20 - 148*z + 35*pow(z,2)))*HPL(0,1,1,0,z))/(16.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*HPL(1,0,-1,0,z))/(2.*Nc*z) + (pow(-1 + pow(Nc,2),2)*(-2 + 2*z - pow(z,2) + pow(Nc,2)*(2 - 2*z + pow(z,2)) - 2*Nc*(3 + 8*z + 2*pow(z,2)) + pow(Nc,3)*(42 - 12*z + 23*pow(z,2)))*HPL(1,0,0,0,z))/(8.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-2 + 12*Nc + 2*z - pow(z,2) + pow(Nc,2)*(2 - 2*z + pow(z,2)) + 6*pow(Nc,3)*(2 - 12*z + 3*pow(z,2)))*HPL(1,0,1,0,z))/(8.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-2*(2 - 2*z + pow(z,2)) + 2*pow(Nc,2)*(2 - 2*z + pow(z,2)) - 8*Nc*(5 + 8*z + 2*pow(z,2)) + pow(Nc,3)*(212 + 44*z + 83*pow(z,2)))*HPL(1,1,0,0,z))/(16.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-49 + 143*pow(Nc,2))*pow(2 + z,2)*HPL(1,1,1,0,z))/(32.*pow(Nc,3)*z) + ((pow(-1 + pow(Nc,2),2)*(-(Nc*(-12492 + 68058*z - 1152*nf*z + 19545*pow(z,2) - 2592*nf*pow(z,2) + 2464*pow(z,3) + 36*(392 - 324*z + 57*pow(z,2))*z2 + 3456*z*z3 + 864*pow(z,2)*z3)) + pow(Nc,3)*(-193320 + 299966*z - 16369*pow(z,2) + 864*pow(z,3) - 36*(-640 - 332*z + 271*pow(z,2))*z2 + 5184*z3 + 17280*z*z3 + 5616*pow(z,2)*z3) + 24*z*(12*(9 + 12*z + 2*pow(z,2))*z2 + 3*(-49 - 12*z3 + z*(-70 + 6*z3))) - 8*pow(Nc,2)*(nf*(-2213 + 460*z + 724*pow(z,2) + 36*pow(2 + z,2)*z2) + 3*z*(12*(9 + 12*z + 2*pow(z,2))*z2 + 3*(39 - 12*z3 + 2*z*(64 + 3*z3))))))/(6912.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(11*Nc - 2*nf)*pow(2 + z,2)*HPL(1,0,z))/(6.*pow(Nc,2)*z))*log(z) - (pow(-1 + pow(Nc,2),2)*(12*z*(7 - 2*z + 6*pow(z,2)) - 4*pow(Nc,2)*(nf*(188 + 8*z + 5*pow(z,2)) + 3*z*(51 + 64*z + 6*pow(z,2))) + Nc*(-286 + (1125 + 96*nf)*z - 9*(-173 - 16*nf)*pow(z,2) - 112*pow(z,3) + 18*(24 + 52*z + 13*pow(z,2))*z2) + pow(Nc,3)*(9176 - 18385*z - 559*pow(z,2) - 288*pow(z,3) - 18*(40 - 44*z + 45*pow(z,2))*z2))*pow(log(z),2))/(1152.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-3*Nc*z*(-228 + 49*z) + pow(Nc,3)*(2624 + 1860*z - 309*pow(z,2)) + 16*z*(3 + 9*z + 2*pow(z,2)) - 16*pow(Nc,2)*(3*nf*pow(2 + z,2) + z*(3 + 9*z + 2*pow(z,2))))*pow(log(z),3))/(2304.*pow(Nc,4)*z) - (pow(-1 + pow(Nc,2),2)*(-11*z*(4 + z) + pow(Nc,2)*(96 - 132*z + 79*pow(z,2)))*pow(log(z),4))/(1536.*pow(Nc,3)*z) + pow(log(1 - z),3)*(-(pow(-1 + pow(Nc,2),2)*(-49 + 143*pow(Nc,2))*(-3 + 2*z + pow(z,2)))/(96.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(-49 + 143*pow(Nc,2))*pow(2 + z,2)*log(z))/(192.*pow(Nc,3)*z)) + pow(log(1 - z),2)*((pow(-1 + pow(Nc,2),2)*(2385 - 1332*z - 1053*pow(z,2) - 72*Nc*nf*(-3 + 2*z + pow(z,2)) - 1176*z2 - 1176*z*z2 - 294*pow(z,2)*z2 + pow(Nc,2)*(-12791 + 12492*z + 171*pow(z,2) + 128*pow(z,3) + 858*pow(2 + z,2)*z2)))/(384.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(-49 + 143*pow(Nc,2))*pow(2 + z,2)*HPL(1,0,z))/(64.*pow(Nc,3)*z) - (pow(-1 + pow(Nc,2),2)*(-420 + 4*z + 55*pow(z,2) - 12*Nc*nf*pow(2 + z,2) + pow(Nc,2)*(2240 + 412*z - 545*pow(z,2)))*log(z))/(128.*pow(Nc,3)*z) - (pow(-1 + pow(Nc,2),2)*(-8*(5 + 8*z + 2*pow(z,2)) + pow(Nc,2)*(212 + 44*z + 83*pow(z,2)))*pow(log(z),2))/(64.*pow(Nc,3)*z)) + log(1 - z)*(-(pow(-1 + pow(Nc,2),2)*(-(Nc*(-40448 + 31698*z + 8046*pow(z,2) + 704*pow(z,3) - 54*(-372 - 28*z + 39*pow(z,2))*z2 + 2160*z3 + 7344*z*z3 + 1836*pow(z,2)*z3)) + 108*(5 - 8*z*(-2 + z3) + 8*z3 + pow(z,2)*(-21 + 4*z3)) - 12*pow(Nc,2)*(nf*(-885 + 544*z + 341*pow(z,2) + 54*pow(2 + z,2)*z2) + 9*(5 - 8*z*(-2 + z3) + 8*z3 + pow(z,2)*(-21 + 4*z3))) + pow(Nc,3)*(-54*(-1544 - 748*z + 313*pow(z,2))*z2 + 2*(-96929 - 640*pow(z,3) + 7992*z3 + 3*z*(33509 + 72*z3) + 6*pow(z,2)*(-493 + 873*z3)))))/(3456.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-49 + 143*pow(Nc,2))*pow(2 + z,2)*z2*HPL(1,z))/(32.*pow(Nc,3)*z) - (pow(-1 + pow(Nc,2),2)*(-3 - 2*z + pow(z,2))*HPL(-1,0,z))/(Nc*z) - (pow(-1 + pow(Nc,2),2)*(-36 - 252*z - 73*pow(z,2) - 12*Nc*nf*pow(2 + z,2) + pow(Nc,2)*(1112 + 1164*z - 169*pow(z,2)))*HPL(1,0,z))/(64.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*pow(-2 + z,2)*HPL(0,-1,0,z))/(2.*Nc*z) - (3*pow(-1 + pow(Nc,2),2)*(2 + pow(Nc,2)*(2 - 12*z + 3*pow(z,2)))*HPL(0,1,0,z))/(4.*pow(Nc,3)*z) - (pow(-1 + pow(Nc,2),2)*(-2*(2 - 2*z + pow(z,2)) + 2*pow(Nc,2)*(2 - 2*z + pow(z,2)) - 8*Nc*(5 + 8*z + 2*pow(z,2)) + pow(Nc,3)*(212 + 44*z + 83*pow(z,2)))*HPL(1,0,0,z))/(16.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-49 + 143*pow(Nc,2))*pow(2 + z,2)*HPL(1,1,0,z))/(32.*pow(Nc,3)*z) + (pow(-1 + pow(Nc,2),2)*(-36*z*(4 + 9*z) - 4*pow(Nc,2)*(-9*z*(4 + 9*z) + nf*(284 + 128*z + 17*pow(z,2))) + Nc*(-2867 + 2520*z + 2655*pow(z,2) - 32*pow(z,3) + 72*(16 + 28*z + 7*pow(z,2))*z2) + pow(Nc,3)*(19705 - 25672*z - 1519*pow(z,2) - 480*pow(z,3) - 72*(48 - 20*z + 27*pow(z,2))*z2))*log(z))/(576.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-3*Nc*(30 - 32*z) + 6*z*(2 + 3*z) + pow(Nc,3)*(730 + 236*z - 193*pow(z,2)) - 2*pow(Nc,2)*(4*nf*pow(2 + z,2) + 3*z*(2 + 3*z)))*pow(log(z),2))/(96.*pow(Nc,4)*z) + (pow(-1 + pow(Nc,2),2)*(-2*(3 + 8*z + 2*pow(z,2)) + pow(Nc,2)*(42 - 12*z + 23*pow(z,2)))*pow(log(z),3))/(48.*pow(Nc,3)*z));
        check_imaginary_part(L1,__PRETTY_FUNCTION__);
        return real(L3)*pow(L,3)+real(L2)*pow(L,2)+real(L1)*L;
    }
    
    double q1q2_nnlo_r_lz0(const double& z, const double& L)
    {
        return    q1q2_nnlo_r_lz0_const(z,L)
        + q1q2_nnlo_r_lz0_logz(z,L)
        + q1q2_nnlo_r_lz0_logz_sq(z,L)
        + q1q2_nnlo_r_lz0_logz_cube(z,L) ;
    }
    
    double q1q2_nnlo_r_lz0_const(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        const double z2 = consts::z2;
        complex<double> res =
        (pow(-1 + pow(Nc,2),2)*(-315 - 153*L - 36*pow(L,2) + 282*z + 108*L*z + 24*pow(L,2)*z + 33*pow(z,2) + 45*L*pow(z,2) + 12*pow(L,2)*pow(z,2) + 72*z2 + 96*L*z2 - 48*z*z2 + 96*L*z*z2 - 36*pow(z,2)*z2 + 24*L*pow(z,2)*z2))/(96.*pow(Nc,2)*z) - (pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*z2*HPL(1,z))/(pow(Nc,2)*z) + (pow(-1 + pow(Nc,2),2)*(20 - 3*pow(z,2) + 2*L*pow(2 + z,2))*HPL(0,1,z))/(8.*pow(Nc,2)*z) + (pow(-1 + pow(Nc,2),2)*(168 + 192*L + 48*z + 192*L*z - 24*pow(z,2) + 48*L*pow(z,2))*HPL(1,0,z))/(96.*pow(Nc,2)*z) + (pow(-1 + pow(Nc,2),2)*(96 + 96*z + 24*pow(z,2))*HPL(0,0,1,z))/(96.*pow(Nc,2)*z) + (pow(-1 + pow(Nc,2),2)*(-144 - 144*z - 36*pow(z,2))*HPL(0,1,0,z))/(96.*pow(Nc,2)*z) + (pow(-1 + pow(Nc,2),2)*(192 + 192*z + 48*pow(z,2))*HPL(0,1,1,z))/(96.*pow(Nc,2)*z) + (pow(-1 + pow(Nc,2),2)*(-192 - 192*z - 48*pow(z,2))*HPL(1,0,0,z))/(96.*pow(Nc,2)*z) + (pow(-1 + pow(Nc,2),2)*(-384 - 384*z - 96*pow(z,2))*HPL(1,1,0,z))/(96.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res);
    }
    
    double q1q2_nnlo_r_lz0_logz(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        const double z2 = consts::z2;
        complex<double> res =
        (pow(-1 + pow(Nc,2),2)*(-177 + 132*z + 87*pow(z,2) - 6*pow(L,2)*pow(2 + z,2) + 6*L*(-12 + 8*z + 5*pow(z,2)) + 48*z2 + 48*z*z2 + 12*pow(z,2)*z2))/(96.*pow(Nc,2)*z) + (pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*HPL(1,0,z))/(2.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*log(z);
    }
    
    double q1q2_nnlo_r_lz0_logz_sq(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        complex<double> res =
        -(pow(-1 + pow(Nc,2),2)*(L*pow(2 + z,2) - 2*(-3 + 4*z + pow(z,2))))/(16.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(z),2.);
    }
    
    double q1q2_nnlo_r_lz0_logz_cube(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        complex<double> res =
        -(pow(-1 + pow(Nc,2),2)*pow(2 + z,2))/(48.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(z),3.);
    }
    
    double q1q2_nnlo_r_lz1(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        const double z2 = consts::z2;
        
        complex<double> res =
        (pow(-1 + pow(Nc,2),2)*(153 + 72*L - 108*z - 48*L*z - 45*pow(z,2) - 24*L*pow(z,2) - 192*z2 - 192*z*z2 - 48*pow(z,2)*z2))/(48.*pow(Nc,2)*z) + (pow(-1 + pow(Nc,2),2)*(L*pow(2 + z,2) - 2*(-4 + z + pow(z,2)))*HPL(0,z))/(2.*pow(Nc,2)*z) + (pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*pow(HPL(0,z),2))/(4.*pow(Nc,2)*z) - (pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*HPL(1,0,z))/(pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*log(1.-z);
    }
    
    double q1q2_nnlo_r_lz2(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        
        complex<double> res =
        -(pow(-1 + pow(Nc,2),2)*(3 - 2*z - pow(z,2)))/(2.*pow(Nc,2)*z) - (pow(-1 + pow(Nc,2),2)*pow(2 + z,2)*HPL(0,z))/(2.*pow(Nc,2)*z)
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(1.-z),2.);
    }
    
    double LEq1q2NNNLOregSte(const double& z, const double& L)
    {
        if (abs(L)<1e-15) return 0.0;

        double zz = z;
        double LL = L;
        return leq1q2nnnlo_(&zz,&LL);
    }
    
    double LEq1q2N3LOregFalko(const double& z, const double& L)
    {
        if (abs(L)<1e-15) return 0.0;

        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        const double z2= consts::z2;
        const double z3= consts::z3;
        const double z4= consts::z4;
        
        complex<double> L3=-1/(64.*pow(Nc,3)) + 481/(192.*Nc) - (953*Nc)/192. + (475*pow(Nc,3))/192. + nf/6. - nf/(12.*pow(Nc,2)) - (pow(Nc,2)*nf)/12. + 5/(64.*pow(Nc,3)*z) - 191/(72.*Nc*z) + (2921*Nc)/(576.*z) - (719*pow(Nc,3))/(288.*z) - nf/(4.*z) + nf/(8.*pow(Nc,2)*z) + (pow(Nc,2)*nf)/(8.*z) - z/(16.*pow(Nc,3)) + (23*z)/(192.*Nc) - (5*Nc*z)/96. - (pow(Nc,3)*z)/192. + (nf*z)/12. - (nf*z)/(24.*pow(Nc,2)) - (pow(Nc,2)*nf*z)/24. + pow(z,2)/(36.*Nc) - (Nc*pow(z,2))/18. + (pow(Nc,3)*pow(z,2))/36. - z2/(8.*pow(Nc,3)) + (5*z2)/(8.*Nc) - (7*Nc*z2)/8. + (3*pow(Nc,3)*z2)/8. - z2/(8.*pow(Nc,3)*z) + (5*z2)/(8.*Nc*z) - (7*Nc*z2)/(8.*z) + (3*pow(Nc,3)*z2)/(8.*z) - (z*z2)/(32.*pow(Nc,3)) + (5*z*z2)/(32.*Nc) - (7*Nc*z*z2)/32. + (3*pow(Nc,3)*z*z2)/32. - HPL(1,0,z)/(8.*pow(Nc,3)) + (5*HPL(1,0,z))/(8.*Nc) - (7*Nc*HPL(1,0,z))/8. + (3*pow(Nc,3)*HPL(1,0,z))/8. - HPL(1,0,z)/(8.*pow(Nc,3)*z) + (5*HPL(1,0,z))/(8.*Nc*z) - (7*Nc*HPL(1,0,z))/(8.*z) + (3*pow(Nc,3)*HPL(1,0,z))/(8.*z) - (z*HPL(1,0,z))/(32.*pow(Nc,3)) + (5*z*HPL(1,0,z))/(32.*Nc) - (7*Nc*z*HPL(1,0,z))/32. + (3*pow(Nc,3)*z*HPL(1,0,z))/32. + log(1 - z)/(8.*pow(Nc,3)) - (5*log(1 - z))/(8.*Nc) + (7*Nc*log(1 - z))/8. - (3*pow(Nc,3)*log(1 - z))/8. - (3*log(1 - z))/(16.*pow(Nc,3)*z) + (15*log(1 - z))/(16.*Nc*z) - (21*Nc*log(1 - z))/(16.*z) + (9*pow(Nc,3)*log(1 - z))/(16.*z) + (z*log(1 - z))/(16.*pow(Nc,3)) - (5*z*log(1 - z))/(16.*Nc) + (7*Nc*z*log(1 - z))/16. - (3*pow(Nc,3)*z*log(1 - z))/16. - (3*log(z))/(32.*pow(Nc,3)) - (53*log(z))/(96.*Nc) + (133*Nc*log(z))/96. - (71*pow(Nc,3)*log(z))/96. - (nf*log(z))/6. + (nf*log(z))/(12.*pow(Nc,2)) + (pow(Nc,2)*nf*log(z))/12. - (31*log(z))/(24.*Nc*z) + (31*Nc*log(z))/(12.*z) - (31*pow(Nc,3)*log(z))/(24.*z) - (nf*log(z))/(6.*z) + (nf*log(z))/(12.*pow(Nc,2)*z) + (pow(Nc,2)*nf*log(z))/(12.*z) - (5*z*log(z))/(128.*pow(Nc,3)) + (85*z*log(z))/(384.*Nc) - (125*Nc*z*log(z))/384. + (55*pow(Nc,3)*z*log(z))/384. - (nf*z*log(z))/24. + (nf*z*log(z))/(48.*pow(Nc,2)) + (pow(Nc,2)*nf*z*log(z))/48. - (log(1 - z)*log(z))/(8.*pow(Nc,3)) + (5*log(1 - z)*log(z))/(8.*Nc) - (7*Nc*log(1 - z)*log(z))/8. + (3*pow(Nc,3)*log(1 - z)*log(z))/8. - (log(1 - z)*log(z))/(8.*pow(Nc,3)*z) + (5*log(1 - z)*log(z))/(8.*Nc*z) - (7*Nc*log(1 - z)*log(z))/(8.*z) + (3*pow(Nc,3)*log(1 - z)*log(z))/(8.*z) - (z*log(1 - z)*log(z))/(32.*pow(Nc,3)) + (5*z*log(1 - z)*log(z))/(32.*Nc) - (7*Nc*z*log(1 - z)*log(z))/32. + (3*pow(Nc,3)*z*log(1 - z)*log(z))/32. + pow(log(z),2)/(32.*pow(Nc,3)) + pow(log(z),2)/(32.*Nc) - (5*Nc*pow(log(z),2))/32. + (3*pow(Nc,3)*pow(log(z),2))/32. - pow(log(z),2)/(8.*Nc*z) + (Nc*pow(log(z),2))/(4.*z) - (pow(Nc,3)*pow(log(z),2))/(8.*z) + (z*pow(log(z),2))/(128.*pow(Nc,3)) - (11*z*pow(log(z),2))/(128.*Nc) + (19*Nc*z*pow(log(z),2))/128. - (9*pow(Nc,3)*z*pow(log(z),2))/128.;
        check_imaginary_part(L3,__PRETTY_FUNCTION__);
        complex<double> L2 = -71/(64.*pow(Nc,3)) + 2347/(144.*Nc) - (16859*Nc)/576. + (4055*pow(Nc,3))/288. + (97*nf)/72. - (97*nf)/(144.*pow(Nc,2)) - (97*pow(Nc,2)*nf)/144. + 127/(108.*pow(Nc,3)*z) - 13769/(864.*Nc*z) + (12245*Nc)/(432.*z) - (11737*pow(Nc,3))/(864.*z) - (205*nf)/(96.*z) + (205*nf)/(192.*pow(Nc,2)*z) + (205*pow(Nc,2)*nf)/(192.*z) - z/(64.*pow(Nc,3)) - (107*z)/(288.*Nc) + (455*Nc*z)/576. - (29*pow(Nc,3)*z)/72. + (227*nf*z)/288. - (227*nf*z)/(576.*pow(Nc,2)) - (227*pow(Nc,2)*nf*z)/576. - (11*pow(z,2))/(216.*pow(Nc,3)) + pow(z,2)/(108.*Nc) + (29*Nc*pow(z,2))/216. - (5*pow(Nc,3)*pow(z,2))/54. + (5*z2)/(16.*pow(Nc,3)) + (151*z2)/(48.*Nc) - (347*Nc*z2)/48. + (181*pow(Nc,3)*z2)/48. + (2*nf*z2)/3. - (nf*z2)/(3.*pow(Nc,2)) - (pow(Nc,2)*nf*z2)/3. - (15*z2)/(16.*pow(Nc,3)*z) + (391*z2)/(48.*Nc*z) - (647*Nc*z2)/(48.*z) + (301*pow(Nc,3)*z2)/(48.*z) + (2*nf*z2)/(3.*z) - (nf*z2)/(3.*pow(Nc,2)*z) - (pow(Nc,2)*nf*z2)/(3.*z) + (z*z2)/(4.*pow(Nc,3)) - (133*z*z2)/(96.*Nc) + (97*Nc*z*z2)/48. - (85*pow(Nc,3)*z*z2)/96. + (nf*z*z2)/6. - (nf*z*z2)/(12.*pow(Nc,2)) - (pow(Nc,2)*nf*z*z2)/12. - z3/(2.*pow(Nc,3)) + z3/Nc - (Nc*z3)/2. - z3/(8.*pow(Nc,3)*z) + (11*z3)/(8.*Nc*z) - (19*Nc*z3)/(8.*z) + (9*pow(Nc,3)*z3)/(8.*z) - (z*z3)/(8.*pow(Nc,3)) + (z*z3)/Nc - (13*Nc*z*z3)/8. + (3*pow(Nc,3)*z*z3)/4. + (5*z2*HPL(1,z))/(4.*pow(Nc,3)) - (25*z2*HPL(1,z))/(4.*Nc) + (35*Nc*z2*HPL(1,z))/4. - (15*pow(Nc,3)*z2*HPL(1,z))/4. + (5*z2*HPL(1,z))/(4.*pow(Nc,3)*z) - (25*z2*HPL(1,z))/(4.*Nc*z) + (35*Nc*z2*HPL(1,z))/(4.*z) - (15*pow(Nc,3)*z2*HPL(1,z))/(4.*z) + (5*z*z2*HPL(1,z))/(16.*pow(Nc,3)) - (25*z*z2*HPL(1,z))/(16.*Nc) + (35*Nc*z*z2*HPL(1,z))/16. - (15*pow(Nc,3)*z*z2*HPL(1,z))/16. - HPL(-1,0,z)/(2.*Nc) + Nc*HPL(-1,0,z) - (pow(Nc,3)*HPL(-1,0,z))/2. - (3*HPL(-1,0,z))/(4.*Nc*z) + (3*Nc*HPL(-1,0,z))/(2.*z) - (3*pow(Nc,3)*HPL(-1,0,z))/(4.*z) + (z*HPL(-1,0,z))/(4.*Nc) - (Nc*z*HPL(-1,0,z))/2. + (pow(Nc,3)*z*HPL(-1,0,z))/4. - (7*HPL(1,0,z))/(16.*pow(Nc,3)) + (283*HPL(1,0,z))/(48.*Nc) - (503*Nc*HPL(1,0,z))/48. + (241*pow(Nc,3)*HPL(1,0,z))/48. + (2*nf*HPL(1,0,z))/3. - (nf*HPL(1,0,z))/(3.*pow(Nc,2)) - (pow(Nc,2)*nf*HPL(1,0,z))/3. + (3*HPL(1,0,z))/(16.*pow(Nc,3)*z) + (229*HPL(1,0,z))/(48.*Nc*z) - (485*Nc*HPL(1,0,z))/(48.*z) + (247*pow(Nc,3)*HPL(1,0,z))/(48.*z) + (2*nf*HPL(1,0,z))/(3.*z) - (nf*HPL(1,0,z))/(3.*pow(Nc,2)*z) - (pow(Nc,2)*nf*HPL(1,0,z))/(3.*z) - (z*HPL(1,0,z))/(8.*pow(Nc,3)) - (25*z*HPL(1,0,z))/(96.*Nc) + (43*Nc*z*HPL(1,0,z))/48. - (49*pow(Nc,3)*z*HPL(1,0,z))/96. + (nf*z*HPL(1,0,z))/6. - (nf*z*HPL(1,0,z))/(12.*pow(Nc,2)) - (pow(Nc,2)*nf*z*HPL(1,0,z))/12. + HPL(0,-1,0,z)/(2.*Nc) - Nc*HPL(0,-1,0,z) + (pow(Nc,3)*HPL(0,-1,0,z))/2. - HPL(0,-1,0,z)/(2.*Nc*z) + (Nc*HPL(0,-1,0,z))/z - (pow(Nc,3)*HPL(0,-1,0,z))/(2.*z) - (z*HPL(0,-1,0,z))/(8.*Nc) + (Nc*z*HPL(0,-1,0,z))/4. - (pow(Nc,3)*z*HPL(0,-1,0,z))/8. - (9*HPL(0,1,0,z))/(4.*Nc) + (9*Nc*HPL(0,1,0,z))/2. - (9*pow(Nc,3)*HPL(0,1,0,z))/4. + (3*HPL(0,1,0,z))/(8.*pow(Nc,3)*z) - (3*HPL(0,1,0,z))/(8.*Nc*z) - (3*Nc*HPL(0,1,0,z))/(8.*z) + (3*pow(Nc,3)*HPL(0,1,0,z))/(8.*z) + (9*z*HPL(0,1,0,z))/(16.*Nc) - (9*Nc*z*HPL(0,1,0,z))/8. + (9*pow(Nc,3)*z*HPL(0,1,0,z))/16. - (3*HPL(1,0,0,z))/(4.*pow(Nc,3)) + (3*HPL(1,0,0,z))/(2.*Nc) - (3*Nc*HPL(1,0,0,z))/4. - (3*HPL(1,0,0,z))/(8.*pow(Nc,3)*z) + (27*HPL(1,0,0,z))/(8.*Nc*z) - (45*Nc*HPL(1,0,0,z))/(8.*z) + (21*pow(Nc,3)*HPL(1,0,0,z))/(8.*z) - (3*z*HPL(1,0,0,z))/(16.*pow(Nc,3)) + (3*z*HPL(1,0,0,z))/(2.*Nc) - (39*Nc*z*HPL(1,0,0,z))/16. + (9*pow(Nc,3)*z*HPL(1,0,0,z))/8. + (5*HPL(1,1,0,z))/(4.*pow(Nc,3)) - (25*HPL(1,1,0,z))/(4.*Nc) + (35*Nc*HPL(1,1,0,z))/4. - (15*pow(Nc,3)*HPL(1,1,0,z))/4. + (5*HPL(1,1,0,z))/(4.*pow(Nc,3)*z) - (25*HPL(1,1,0,z))/(4.*Nc*z) + (35*Nc*HPL(1,1,0,z))/(4.*z) - (15*pow(Nc,3)*HPL(1,1,0,z))/(4.*z) + (5*z*HPL(1,1,0,z))/(16.*pow(Nc,3)) - (25*z*HPL(1,1,0,z))/(16.*Nc) + (35*Nc*z*HPL(1,1,0,z))/16. - (15*pow(Nc,3)*z*HPL(1,1,0,z))/16. + (17*log(1 - z))/(16.*pow(Nc,3)) - (217*log(1 - z))/(12.*Nc) + (1583*Nc*log(1 - z))/48. - (383*pow(Nc,3)*log(1 - z))/24. - (2*nf*log(1 - z))/3. + (nf*log(1 - z))/(3.*pow(Nc,2)) + (pow(Nc,2)*nf*log(1 - z))/3. - (257*log(1 - z))/(128.*pow(Nc,3)*z) + (7831*log(1 - z))/(384.*Nc*z) - (13349*Nc*log(1 - z))/(384.*z) + (6289*pow(Nc,3)*log(1 - z))/(384.*z) + (nf*log(1 - z))/z - (nf*log(1 - z))/(2.*pow(Nc,2)*z) - (pow(Nc,2)*nf*log(1 - z))/(2.*z) + (121*z*log(1 - z))/(128.*pow(Nc,3)) - (823*z*log(1 - z))/(384.*Nc) + (557*Nc*z*log(1 - z))/384. - (97*pow(Nc,3)*z*log(1 - z))/384. - (nf*z*log(1 - z))/3. + (nf*z*log(1 - z))/(6.*pow(Nc,2)) + (pow(Nc,2)*nf*z*log(1 - z))/6. - (pow(z,2)*log(1 - z))/(6.*Nc) + (Nc*pow(z,2)*log(1 - z))/3. - (pow(Nc,3)*pow(z,2)*log(1 - z))/6. + (5*z2*log(1 - z))/(4.*pow(Nc,3)) - (25*z2*log(1 - z))/(4.*Nc) + (35*Nc*z2*log(1 - z))/4. - (15*pow(Nc,3)*z2*log(1 - z))/4. + (5*z2*log(1 - z))/(4.*pow(Nc,3)*z) - (25*z2*log(1 - z))/(4.*Nc*z) + (35*Nc*z2*log(1 - z))/(4.*z) - (15*pow(Nc,3)*z2*log(1 - z))/(4.*z) + (5*z*z2*log(1 - z))/(16.*pow(Nc,3)) - (25*z*z2*log(1 - z))/(16.*Nc) + (35*Nc*z*z2*log(1 - z))/16. - (15*pow(Nc,3)*z*z2*log(1 - z))/16. + (5*HPL(1,0,z)*log(1 - z))/(4.*pow(Nc,3)) - (25*HPL(1,0,z)*log(1 - z))/(4.*Nc) + (35*Nc*HPL(1,0,z)*log(1 - z))/4. - (15*pow(Nc,3)*HPL(1,0,z)*log(1 - z))/4. + (5*HPL(1,0,z)*log(1 - z))/(4.*pow(Nc,3)*z) - (25*HPL(1,0,z)*log(1 - z))/(4.*Nc*z) + (35*Nc*HPL(1,0,z)*log(1 - z))/(4.*z) - (15*pow(Nc,3)*HPL(1,0,z)*log(1 - z))/(4.*z) + (5*z*HPL(1,0,z)*log(1 - z))/(16.*pow(Nc,3)) - (25*z*HPL(1,0,z)*log(1 - z))/(16.*Nc) + (35*Nc*z*HPL(1,0,z)*log(1 - z))/16. - (15*pow(Nc,3)*z*HPL(1,0,z)*log(1 - z))/16. - (5*pow(log(1 - z),2))/(8.*pow(Nc,3)) + (25*pow(log(1 - z),2))/(8.*Nc) - (35*Nc*pow(log(1 - z),2))/8. + (15*pow(Nc,3)*pow(log(1 - z),2))/8. + (15*pow(log(1 - z),2))/(16.*pow(Nc,3)*z) - (75*pow(log(1 - z),2))/(16.*Nc*z) + (105*Nc*pow(log(1 - z),2))/(16.*z) - (45*pow(Nc,3)*pow(log(1 - z),2))/(16.*z) - (5*z*pow(log(1 - z),2))/(16.*pow(Nc,3)) + (25*z*pow(log(1 - z),2))/(16.*Nc) - (35*Nc*z*pow(log(1 - z),2))/16. + (15*pow(Nc,3)*z*pow(log(1 - z),2))/16. - (71*log(z))/(64.*pow(Nc,3)) + (7553*log(z))/(576.*Nc) - (13189*Nc*log(z))/576. + (6275*pow(Nc,3)*log(z))/576. - (7*nf*log(z))/18. + (7*nf*log(z))/(36.*pow(Nc,2)) + (7*pow(Nc,2)*nf*log(z))/36. + (143*log(z))/(576.*pow(Nc,3)*z) - (2447*log(z))/(288.*Nc*z) + (9359*Nc*log(z))/(576.*z) - (8*pow(Nc,3)*log(z))/z - (47*nf*log(z))/(36.*z) + (47*nf*log(z))/(72.*pow(Nc,2)*z) + (47*pow(Nc,2)*nf*log(z))/(72.*z) - (119*z*log(z))/(128.*pow(Nc,3)) + (2797*z*log(z))/(1152.*Nc) - (2381*Nc*z*log(z))/1152. + (655*pow(Nc,3)*z*log(z))/1152. + (nf*z*log(z))/144. - (nf*z*log(z))/(288.*pow(Nc,2)) - (pow(Nc,2)*nf*z*log(z))/288. + (pow(z,2)*log(z))/(72.*pow(Nc,3)) + (13*pow(z,2)*log(z))/(72.*Nc) - (29*Nc*pow(z,2)*log(z))/72. + (5*pow(Nc,3)*pow(z,2)*log(z))/24. - (3*z2*log(z))/(4.*pow(Nc,3)) + (z2*log(z))/(2.*Nc) + (5*Nc*z2*log(z))/4. - pow(Nc,3)*z2*log(z) - (3*z2*log(z))/(8.*pow(Nc,3)*z) + (15*z2*log(z))/(8.*Nc*z) - (21*Nc*z2*log(z))/(8.*z) + (9*pow(Nc,3)*z2*log(z))/(8.*z) - (3*z*z2*log(z))/(16.*pow(Nc,3)) + (9*z*z2*log(z))/(8.*Nc) - (27*Nc*z*z2*log(z))/16. + (3*pow(Nc,3)*z*z2*log(z))/4. + (5*log(1 - z)*log(z))/(16.*pow(Nc,3)) + (103*log(1 - z)*log(z))/(48.*Nc) - (251*Nc*log(1 - z)*log(z))/48. + (133*pow(Nc,3)*log(1 - z)*log(z))/48. + (2*nf*log(1 - z)*log(z))/3. - (nf*log(1 - z)*log(z))/(3.*pow(Nc,2)) - (pow(Nc,2)*nf*log(1 - z)*log(z))/3. - (15*log(1 - z)*log(z))/(16.*pow(Nc,3)*z) + (499*log(1 - z)*log(z))/(48.*Nc*z) - (863*Nc*log(1 - z)*log(z))/(48.*z) + (409*pow(Nc,3)*log(1 - z)*log(z))/(48.*z) + (2*nf*log(1 - z)*log(z))/(3.*z) - (nf*log(1 - z)*log(z))/(3.*pow(Nc,2)*z) - (pow(Nc,2)*nf*log(1 - z)*log(z))/(3.*z) + (z*log(1 - z)*log(z))/(4.*pow(Nc,3)) - (205*z*log(1 - z)*log(z))/(96.*Nc) + (169*Nc*z*log(1 - z)*log(z))/48. - (157*pow(Nc,3)*z*log(1 - z)*log(z))/96. + (nf*z*log(1 - z)*log(z))/6. - (nf*z*log(1 - z)*log(z))/(12.*pow(Nc,2)) - (pow(Nc,2)*nf*z*log(1 - z)*log(z))/12. + (5*pow(log(1 - z),2)*log(z))/(8.*pow(Nc,3)) - (25*pow(log(1 - z),2)*log(z))/(8.*Nc) + (35*Nc*pow(log(1 - z),2)*log(z))/8. - (15*pow(Nc,3)*pow(log(1 - z),2)*log(z))/8. + (5*pow(log(1 - z),2)*log(z))/(8.*pow(Nc,3)*z) - (25*pow(log(1 - z),2)*log(z))/(8.*Nc*z) + (35*Nc*pow(log(1 - z),2)*log(z))/(8.*z) - (15*pow(Nc,3)*pow(log(1 - z),2)*log(z))/(8.*z) + (5*z*pow(log(1 - z),2)*log(z))/(32.*pow(Nc,3)) - (25*z*pow(log(1 - z),2)*log(z))/(32.*Nc) + (35*Nc*z*pow(log(1 - z),2)*log(z))/32. - (15*pow(Nc,3)*z*pow(log(1 - z),2)*log(z))/32. - (17*pow(log(z),2))/(64.*pow(Nc,3)) - (33*pow(log(z),2))/(64.*Nc) + (117*Nc*pow(log(z),2))/64. - (67*pow(Nc,3)*pow(log(z),2))/64. - (nf*pow(log(z),2))/4. + (nf*pow(log(z),2))/(8.*pow(Nc,2)) + (pow(Nc,2)*nf*pow(log(z),2))/8. - (41*pow(log(z),2))/(24.*Nc*z) + (41*Nc*pow(log(z),2))/(12.*z) - (41*pow(Nc,3)*pow(log(z),2))/(24.*z) - (nf*pow(log(z),2))/(4.*z) + (nf*pow(log(z),2))/(8.*pow(Nc,2)*z) + (pow(Nc,2)*nf*pow(log(z),2))/(8.*z) + (21*z*pow(log(z),2))/(64.*Nc) - (21*Nc*z*pow(log(z),2))/32. + (21*pow(Nc,3)*z*pow(log(z),2))/64. - (nf*z*pow(log(z),2))/16. + (nf*z*pow(log(z),2))/(32.*pow(Nc,2)) + (pow(Nc,2)*nf*z*pow(log(z),2))/32. - (3*log(1 - z)*pow(log(z),2))/(8.*pow(Nc,3)) + (3*log(1 - z)*pow(log(z),2))/(4.*Nc) - (3*Nc*log(1 - z)*pow(log(z),2))/8. - (3*log(1 - z)*pow(log(z),2))/(16.*pow(Nc,3)*z) + (27*log(1 - z)*pow(log(z),2))/(16.*Nc*z) - (45*Nc*log(1 - z)*pow(log(z),2))/(16.*z) + (21*pow(Nc,3)*log(1 - z)*pow(log(z),2))/(16.*z) - (3*z*log(1 - z)*pow(log(z),2))/(32.*pow(Nc,3)) + (3*z*log(1 - z)*pow(log(z),2))/(4.*Nc) - (39*Nc*z*log(1 - z)*pow(log(z),2))/32. + (9*pow(Nc,3)*z*log(1 - z)*pow(log(z),2))/16. + (5*pow(log(z),3))/(96.*pow(Nc,3)) + (5*pow(log(z),3))/(96.*Nc) - (25*Nc*pow(log(z),3))/96. + (5*pow(Nc,3)*pow(log(z),3))/32. - pow(log(z),3)/(8.*Nc*z) + (Nc*pow(log(z),3))/(4.*z) - (pow(Nc,3)*pow(log(z),3))/(8.*z) + (5*z*pow(log(z),3))/(384.*pow(Nc,3)) - (47*z*pow(log(z),3))/(384.*Nc) + (79*Nc*z*pow(log(z),3))/384. - (37*pow(Nc,3)*z*pow(log(z),3))/384.;
        check_imaginary_part(L2,__PRETTY_FUNCTION__);
        complex<double> L1 = -17195/(3456.*pow(Nc,3)) + 175789/(3456.*Nc) - (299993*Nc)/3456. + (5237*pow(Nc,3))/128. + (2687*nf)/432. - (2687*nf)/(864.*pow(Nc,2)) - (2687*pow(Nc,2)*nf)/864. + 57613/(13824.*pow(Nc,3)*z) - 2054771/(41472.*Nc*z) + (3591025*Nc)/(41472.*z) - (1709093*pow(Nc,3))/(41472.*z) - (619*nf)/(72.*z) + (619*nf)/(144.*pow(Nc,2)*z) + (619*pow(Nc,2)*nf)/(144.*z) + (8207*z)/(13824.*pow(Nc,3)) - (403*z)/(13824.*Nc) - (23815*Nc*z)/13824. + (593*pow(Nc,3)*z)/512. + (1027*nf*z)/432. - (1027*nf*z)/(864.*pow(Nc,2)) - (1027*pow(Nc,2)*nf*z)/864. + (185*pow(z,2))/(864.*pow(Nc,3)) - (3343*pow(z,2))/(2592.*Nc) + (5021*Nc*pow(z,2))/2592. - (2233*pow(Nc,3)*pow(z,2))/2592. + (141*z2)/(32.*pow(Nc,3)) - (9839*z2)/(288.*Nc) + (15871*Nc*z2)/288. - (7301*pow(Nc,3)*z2)/288. + (25*nf*z2)/9. - (25*nf*z2)/(18.*pow(Nc,2)) - (25*pow(Nc,2)*nf*z2)/18. - (2863*z2)/(576.*pow(Nc,3)*z) + (1301*z2)/(64.*Nc*z) - (4943*Nc*z2)/(192.*z) + (5983*pow(Nc,3)*z2)/(576.*z) + (22*nf*z2)/(9.*z) - (11*nf*z2)/(9.*pow(Nc,2)*z) - (11*pow(Nc,2)*nf*z2)/(9.*z) + (281*z*z2)/(64.*pow(Nc,3)) - (5605*z*z2)/(576.*Nc) + (3623*Nc*z*z2)/576. - (547*pow(Nc,3)*z*z2)/576. + (53*nf*z*z2)/72. - (53*nf*z*z2)/(144.*pow(Nc,2)) - (53*pow(Nc,2)*nf*z*z2)/144. - (pow(z,2)*z2)/(18.*pow(Nc,3)) - (5*pow(z,2)*z2)/(9.*Nc) + (23*Nc*pow(z,2)*z2)/18. - (2*pow(Nc,3)*pow(z,2)*z2)/3. + (17*z3)/(16.*pow(Nc,3)) - (111*z3)/(16.*Nc) + (171*Nc*z3)/16. - (77*pow(Nc,3)*z3)/16. - (nf*z3)/2. + (nf*z3)/(4.*pow(Nc,2)) + (pow(Nc,2)*nf*z3)/4. - (47*z3)/(48.*pow(Nc,3)*z) - (17*z3)/(16.*Nc*z) + (81*Nc*z3)/(16.*z) - (145*pow(Nc,3)*z3)/(48.*z) - (nf*z3)/(2.*z) + (nf*z3)/(4.*pow(Nc,2)*z) + (pow(Nc,2)*nf*z3)/(4.*z) - (63*z*z3)/(64.*pow(Nc,3)) + (67*z*z3)/(64.*Nc) + (55*Nc*z*z3)/64. - (59*pow(Nc,3)*z*z3)/64. - (nf*z*z3)/8. + (nf*z*z3)/(16.*pow(Nc,2)) + (pow(Nc,2)*nf*z*z3)/16. + (17*z4)/(4.*pow(Nc,3)) + (57*z4)/(16.*Nc) - (159*Nc*z4)/8. + (193*pow(Nc,3)*z4)/16. + (5*z4)/(2.*pow(Nc,3)*z) - (101*z4)/(16.*Nc*z) + (41*Nc*z4)/(8.*z) - (21*pow(Nc,3)*z4)/(16.*z) + (17*z*z4)/(16.*pow(Nc,3)) - (297*z*z4)/(64.*Nc) + (195*Nc*z*z4)/32. - (161*pow(Nc,3)*z*z4)/64. + (3*z2*HPL(-1,z))/Nc - 6*Nc*z2*HPL(-1,z) + 3*pow(Nc,3)*z2*HPL(-1,z) + (9*z2*HPL(-1,z))/(2.*Nc*z) - (9*Nc*z2*HPL(-1,z))/z + (9*pow(Nc,3)*z2*HPL(-1,z))/(2.*z) - (3*z*z2*HPL(-1,z))/(2.*Nc) + 3*Nc*z*z2*HPL(-1,z) - (3*pow(Nc,3)*z*z2*HPL(-1,z))/2. + (63*z2*HPL(1,z))/(16.*pow(Nc,3)) - (405*z2*HPL(1,z))/(16.*Nc) + (621*Nc*z2*HPL(1,z))/16. - (279*pow(Nc,3)*z2*HPL(1,z))/16. - (3*nf*z2*HPL(1,z))/2. + (3*nf*z2*HPL(1,z))/(4.*pow(Nc,2)) + (3*pow(Nc,2)*nf*z2*HPL(1,z))/4. + (9*z2*HPL(1,z))/(16.*pow(Nc,3)*z) - (133*z2*HPL(1,z))/(8.*Nc*z) + (505*Nc*z2*HPL(1,z))/(16.*z) - (31*pow(Nc,3)*z2*HPL(1,z))/(2.*z) - (3*nf*z2*HPL(1,z))/(2.*z) + (3*nf*z2*HPL(1,z))/(4.*pow(Nc,2)*z) + (3*pow(Nc,2)*nf*z2*HPL(1,z))/(4.*z) + (73*z*z2*HPL(1,z))/(64.*pow(Nc,3)) - (17*z*z2*HPL(1,z))/(64.*Nc) - (185*Nc*z*z2*HPL(1,z))/64. + (129*pow(Nc,3)*z*z2*HPL(1,z))/64. - (3*nf*z*z2*HPL(1,z))/8. + (3*nf*z*z2*HPL(1,z))/(16.*pow(Nc,2)) + (3*pow(Nc,2)*nf*z*z2*HPL(1,z))/16. + (17*z3*HPL(1,z))/(8.*pow(Nc,3)) - (35*z3*HPL(1,z))/(8.*Nc) + (19*Nc*z3*HPL(1,z))/8. - (pow(Nc,3)*z3*HPL(1,z))/8. + (5*z3*HPL(1,z))/(8.*pow(Nc,3)*z) - (47*z3*HPL(1,z))/(8.*Nc*z) + (79*Nc*z3*HPL(1,z))/(8.*z) - (37*pow(Nc,3)*z3*HPL(1,z))/(8.*z) + (17*z*z3*HPL(1,z))/(32.*pow(Nc,3)) - (131*z*z3*HPL(1,z))/(32.*Nc) + (211*Nc*z*z3*HPL(1,z))/32. - (97*pow(Nc,3)*z*z3*HPL(1,z))/32. - HPL(-1,0,z)/(8.*pow(Nc,3)) - (5*HPL(-1,0,z))/(2.*Nc) + (43*Nc*HPL(-1,0,z))/8. - (11*pow(Nc,3)*HPL(-1,0,z))/4. - (11*HPL(-1,0,z))/(36.*pow(Nc,3)*z) - (895*HPL(-1,0,z))/(288.*Nc*z) + (1027*Nc*HPL(-1,0,z))/(144.*z) - (119*pow(Nc,3)*HPL(-1,0,z))/(32.*z) + (z*HPL(-1,0,z))/(8.*pow(Nc,3)) + (23*z*HPL(-1,0,z))/(32.*Nc) - (29*Nc*z*HPL(-1,0,z))/16. + (31*pow(Nc,3)*z*HPL(-1,0,z))/32. - (pow(z,2)*HPL(-1,0,z))/(18.*pow(Nc,3)) + (pow(z,2)*HPL(-1,0,z))/(9.*Nc) - (Nc*pow(z,2)*HPL(-1,0,z))/18. - (3*z2*HPL(0,-1,z))/Nc + 6*Nc*z2*HPL(0,-1,z) - 3*pow(Nc,3)*z2*HPL(0,-1,z) + (3*z2*HPL(0,-1,z))/(Nc*z) - (6*Nc*z2*HPL(0,-1,z))/z + (3*pow(Nc,3)*z2*HPL(0,-1,z))/z + (3*z*z2*HPL(0,-1,z))/(4.*Nc) - (3*Nc*z*z2*HPL(0,-1,z))/2. + (3*pow(Nc,3)*z*z2*HPL(0,-1,z))/4. + (33*z2*HPL(0,1,z))/(4.*Nc) - (33*Nc*z2*HPL(0,1,z))/2. + (33*pow(Nc,3)*z2*HPL(0,1,z))/4. - (3*z2*HPL(0,1,z))/(2.*pow(Nc,3)*z) + (11*z2*HPL(0,1,z))/(4.*Nc*z) - (Nc*z2*HPL(0,1,z))/z - (pow(Nc,3)*z2*HPL(0,1,z))/(4.*z) - (31*z*z2*HPL(0,1,z))/(16.*Nc) + (31*Nc*z*z2*HPL(0,1,z))/8. - (31*pow(Nc,3)*z*z2*HPL(0,1,z))/16. + (13*HPL(1,0,z))/(32.*pow(Nc,3)) + (1057*HPL(1,0,z))/(288.*Nc) - (2465*Nc*HPL(1,0,z))/288. + (1291*pow(Nc,3)*HPL(1,0,z))/288. + (31*nf*HPL(1,0,z))/9. - (31*nf*HPL(1,0,z))/(18.*pow(Nc,2)) - (31*pow(Nc,2)*nf*HPL(1,0,z))/18. + (437*HPL(1,0,z))/(192.*pow(Nc,3)*z) - (5719*HPL(1,0,z))/(288.*Nc*z) + (18943*Nc*HPL(1,0,z))/(576.*z) - (551*pow(Nc,3)*HPL(1,0,z))/(36.*z) + (13*nf*HPL(1,0,z))/(9.*z) - (13*nf*HPL(1,0,z))/(18.*pow(Nc,2)*z) - (13*pow(Nc,2)*nf*HPL(1,0,z))/(18.*z) + (83*z*HPL(1,0,z))/(64.*pow(Nc,3)) - (1517*z*HPL(1,0,z))/(288.*Nc) + (3827*Nc*z*HPL(1,0,z))/576. - (385*pow(Nc,3)*z*HPL(1,0,z))/144. + (77*nf*z*HPL(1,0,z))/72. - (77*nf*z*HPL(1,0,z))/(144.*pow(Nc,2)) - (77*pow(Nc,2)*nf*z*HPL(1,0,z))/144. - (pow(z,2)*HPL(1,0,z))/(36.*pow(Nc,3)) - (5*pow(z,2)*HPL(1,0,z))/(18.*Nc) + (23*Nc*pow(z,2)*HPL(1,0,z))/36. - (pow(Nc,3)*pow(z,2)*HPL(1,0,z))/3. + (7*z2*HPL(1,0,z))/(2.*pow(Nc,3)) - (9*z2*HPL(1,0,z))/(2.*Nc) - (3*Nc*z2*HPL(1,0,z))/2. + (5*pow(Nc,3)*z2*HPL(1,0,z))/2. + (2*z2*HPL(1,0,z))/(pow(Nc,3)*z) - (10*z2*HPL(1,0,z))/(Nc*z) + (14*Nc*z2*HPL(1,0,z))/z - (6*pow(Nc,3)*z2*HPL(1,0,z))/z + (7*z*z2*HPL(1,0,z))/(8.*pow(Nc,3)) - (41*z*z2*HPL(1,0,z))/(8.*Nc) + (61*Nc*z*z2*HPL(1,0,z))/8. - (27*pow(Nc,3)*z*z2*HPL(1,0,z))/8. - (49*z2*HPL(1,1,z))/(8.*pow(Nc,3)) + (241*z2*HPL(1,1,z))/(8.*Nc) - (335*Nc*z2*HPL(1,1,z))/8. + (143*pow(Nc,3)*z2*HPL(1,1,z))/8. - (49*z2*HPL(1,1,z))/(8.*pow(Nc,3)*z) + (241*z2*HPL(1,1,z))/(8.*Nc*z) - (335*Nc*z2*HPL(1,1,z))/(8.*z) + (143*pow(Nc,3)*z2*HPL(1,1,z))/(8.*z) - (49*z*z2*HPL(1,1,z))/(32.*pow(Nc,3)) + (241*z*z2*HPL(1,1,z))/(32.*Nc) - (335*Nc*z*z2*HPL(1,1,z))/32. + (143*pow(Nc,3)*z*z2*HPL(1,1,z))/32. + (2*HPL(-1,-1,0,z))/Nc - 4*Nc*HPL(-1,-1,0,z) + 2*pow(Nc,3)*HPL(-1,-1,0,z) + (3*HPL(-1,-1,0,z))/(Nc*z) - (6*Nc*HPL(-1,-1,0,z))/z + (3*pow(Nc,3)*HPL(-1,-1,0,z))/z - (z*HPL(-1,-1,0,z))/Nc + 2*Nc*z*HPL(-1,-1,0,z) - pow(Nc,3)*z*HPL(-1,-1,0,z) - (5*HPL(-1,0,0,z))/(2.*Nc) + 5*Nc*HPL(-1,0,0,z) - (5*pow(Nc,3)*HPL(-1,0,0,z))/2. - (15*HPL(-1,0,0,z))/(4.*Nc*z) + (15*Nc*HPL(-1,0,0,z))/(2.*z) - (15*pow(Nc,3)*HPL(-1,0,0,z))/(4.*z) + (5*z*HPL(-1,0,0,z))/(4.*Nc) - (5*Nc*z*HPL(-1,0,0,z))/2. + (5*pow(Nc,3)*z*HPL(-1,0,0,z))/4. + (2*HPL(-1,1,0,z))/Nc - 4*Nc*HPL(-1,1,0,z) + 2*pow(Nc,3)*HPL(-1,1,0,z) + (3*HPL(-1,1,0,z))/(Nc*z) - (6*Nc*HPL(-1,1,0,z))/z + (3*pow(Nc,3)*HPL(-1,1,0,z))/z - (z*HPL(-1,1,0,z))/Nc + 2*Nc*z*HPL(-1,1,0,z) - pow(Nc,3)*z*HPL(-1,1,0,z) + HPL(0,-1,0,z)/(4.*pow(Nc,3)) - (5*HPL(0,-1,0,z))/(4.*Nc) + (7*Nc*HPL(0,-1,0,z))/4. - (3*pow(Nc,3)*HPL(0,-1,0,z))/4. - HPL(0,-1,0,z)/(6.*pow(Nc,3)*z) - (37*HPL(0,-1,0,z))/(24.*Nc*z) + (43*Nc*HPL(0,-1,0,z))/(12.*z) - (15*pow(Nc,3)*HPL(0,-1,0,z))/(8.*z) - (z*HPL(0,-1,0,z))/(8.*pow(Nc,3)) + (7*z*HPL(0,-1,0,z))/(8.*Nc) - (11*Nc*z*HPL(0,-1,0,z))/8. + (5*pow(Nc,3)*z*HPL(0,-1,0,z))/8. + (23*HPL(0,1,0,z))/(16.*pow(Nc,3)) - (205*HPL(0,1,0,z))/(16.*Nc) + (341*Nc*HPL(0,1,0,z))/16. - (159*pow(Nc,3)*HPL(0,1,0,z))/16. - 3*nf*HPL(0,1,0,z) + (3*nf*HPL(0,1,0,z))/(2.*pow(Nc,2)) + (3*pow(Nc,2)*nf*HPL(0,1,0,z))/2. + (43*HPL(0,1,0,z))/(24.*pow(Nc,3)*z) - (1031*HPL(0,1,0,z))/(48.*Nc*z) + (451*Nc*HPL(0,1,0,z))/(12.*z) - (859*pow(Nc,3)*HPL(0,1,0,z))/(48.*z) - (3*nf*HPL(0,1,0,z))/z + (3*nf*HPL(0,1,0,z))/(2.*pow(Nc,2)*z) + (3*pow(Nc,2)*nf*HPL(0,1,0,z))/(2.*z) - (49*z*HPL(0,1,0,z))/(64.*pow(Nc,3)) + (77*z*HPL(0,1,0,z))/(64.*Nc) - (7*Nc*z*HPL(0,1,0,z))/64. - (21*pow(Nc,3)*z*HPL(0,1,0,z))/64. - (3*nf*z*HPL(0,1,0,z))/4. + (3*nf*z*HPL(0,1,0,z))/(8.*pow(Nc,2)) + (3*pow(Nc,2)*nf*z*HPL(0,1,0,z))/8. + (2*HPL(1,-1,0,z))/Nc - 4*Nc*HPL(1,-1,0,z) + 2*pow(Nc,3)*HPL(1,-1,0,z) + (3*HPL(1,-1,0,z))/(Nc*z) - (6*Nc*HPL(1,-1,0,z))/z + (3*pow(Nc,3)*HPL(1,-1,0,z))/z - (z*HPL(1,-1,0,z))/Nc + 2*Nc*z*HPL(1,-1,0,z) - pow(Nc,3)*z*HPL(1,-1,0,z) + HPL(1,0,0,z)/(4.*pow(Nc,3)) - (19*HPL(1,0,0,z))/(4.*Nc) + (35*Nc*HPL(1,0,0,z))/4. - (17*pow(Nc,3)*HPL(1,0,0,z))/4. - 4*nf*HPL(1,0,0,z) + (2*nf*HPL(1,0,0,z))/pow(Nc,2) + 2*pow(Nc,2)*nf*HPL(1,0,0,z) + (3*HPL(1,0,0,z))/(4.*pow(Nc,3)*z) - (221*HPL(1,0,0,z))/(24.*Nc*z) + (97*Nc*HPL(1,0,0,z))/(6.*z) - (185*pow(Nc,3)*HPL(1,0,0,z))/(24.*z) - (4*nf*HPL(1,0,0,z))/z + (2*nf*HPL(1,0,0,z))/(pow(Nc,2)*z) + (2*pow(Nc,2)*nf*HPL(1,0,0,z))/z - (7*z*HPL(1,0,0,z))/(8.*pow(Nc,3)) - (51*z*HPL(1,0,0,z))/(16.*Nc) + 9*Nc*z*HPL(1,0,0,z) - (79*pow(Nc,3)*z*HPL(1,0,0,z))/16. - nf*z*HPL(1,0,0,z) + (nf*z*HPL(1,0,0,z))/(2.*pow(Nc,2)) + (pow(Nc,2)*nf*z*HPL(1,0,0,z))/2. + (63*HPL(1,1,0,z))/(16.*pow(Nc,3)) - (421*HPL(1,1,0,z))/(16.*Nc) + (653*Nc*HPL(1,1,0,z))/16. - (295*pow(Nc,3)*HPL(1,1,0,z))/16. - (3*nf*HPL(1,1,0,z))/2. + (3*nf*HPL(1,1,0,z))/(4.*pow(Nc,2)) + (3*pow(Nc,2)*nf*HPL(1,1,0,z))/4. + (9*HPL(1,1,0,z))/(16.*pow(Nc,3)*z) - (145*HPL(1,1,0,z))/(8.*Nc*z) + (553*Nc*HPL(1,1,0,z))/(16.*z) - (17*pow(Nc,3)*HPL(1,1,0,z))/z - (3*nf*HPL(1,1,0,z))/(2.*z) + (3*nf*HPL(1,1,0,z))/(4.*pow(Nc,2)*z) + (3*pow(Nc,2)*nf*HPL(1,1,0,z))/(4.*z) + (73*z*HPL(1,1,0,z))/(64.*pow(Nc,3)) + (15*z*HPL(1,1,0,z))/(64.*Nc) - (249*Nc*z*HPL(1,1,0,z))/64. + (161*pow(Nc,3)*z*HPL(1,1,0,z))/64. - (3*nf*z*HPL(1,1,0,z))/8. + (3*nf*z*HPL(1,1,0,z))/(16.*pow(Nc,2)) + (3*pow(Nc,2)*nf*z*HPL(1,1,0,z))/16. - (2*HPL(0,-1,-1,0,z))/Nc + 4*Nc*HPL(0,-1,-1,0,z) - 2*pow(Nc,3)*HPL(0,-1,-1,0,z) + (2*HPL(0,-1,-1,0,z))/(Nc*z) - (4*Nc*HPL(0,-1,-1,0,z))/z + (2*pow(Nc,3)*HPL(0,-1,-1,0,z))/z + (z*HPL(0,-1,-1,0,z))/(2.*Nc) - Nc*z*HPL(0,-1,-1,0,z) + (pow(Nc,3)*z*HPL(0,-1,-1,0,z))/2. + (5*HPL(0,-1,0,0,z))/(2.*Nc) - 5*Nc*HPL(0,-1,0,0,z) + (5*pow(Nc,3)*HPL(0,-1,0,0,z))/2. - (5*HPL(0,-1,0,0,z))/(2.*Nc*z) + (5*Nc*HPL(0,-1,0,0,z))/z - (5*pow(Nc,3)*HPL(0,-1,0,0,z))/(2.*z) - (5*z*HPL(0,-1,0,0,z))/(8.*Nc) + (5*Nc*z*HPL(0,-1,0,0,z))/4. - (5*pow(Nc,3)*z*HPL(0,-1,0,0,z))/8. - (2*HPL(0,-1,1,0,z))/Nc + 4*Nc*HPL(0,-1,1,0,z) - 2*pow(Nc,3)*HPL(0,-1,1,0,z) + (2*HPL(0,-1,1,0,z))/(Nc*z) - (4*Nc*HPL(0,-1,1,0,z))/z + (2*pow(Nc,3)*HPL(0,-1,1,0,z))/z + (z*HPL(0,-1,1,0,z))/(2.*Nc) - Nc*z*HPL(0,-1,1,0,z) + (pow(Nc,3)*z*HPL(0,-1,1,0,z))/2. + HPL(0,0,-1,0,z)/Nc - 2*Nc*HPL(0,0,-1,0,z) + pow(Nc,3)*HPL(0,0,-1,0,z) - HPL(0,0,-1,0,z)/(Nc*z) + (2*Nc*HPL(0,0,-1,0,z))/z - (pow(Nc,3)*HPL(0,0,-1,0,z))/z - (z*HPL(0,0,-1,0,z))/(4.*Nc) + (Nc*z*HPL(0,0,-1,0,z))/2. - (pow(Nc,3)*z*HPL(0,0,-1,0,z))/4. + (5*HPL(0,0,1,0,z))/(8.*pow(Nc,3)) - (23*HPL(0,0,1,0,z))/(8.*Nc) + (31*Nc*HPL(0,0,1,0,z))/8. - (13*pow(Nc,3)*HPL(0,0,1,0,z))/8. + (3*HPL(0,0,1,0,z))/(4.*pow(Nc,3)*z) - (19*HPL(0,0,1,0,z))/(4.*Nc*z) + (29*Nc*HPL(0,0,1,0,z))/(4.*z) - (13*pow(Nc,3)*HPL(0,0,1,0,z))/(4.*z) + (5*z*HPL(0,0,1,0,z))/(32.*pow(Nc,3)) - (31*z*HPL(0,0,1,0,z))/(32.*Nc) + (47*Nc*z*HPL(0,0,1,0,z))/32. - (21*pow(Nc,3)*z*HPL(0,0,1,0,z))/32. - (2*HPL(0,1,-1,0,z))/Nc + 4*Nc*HPL(0,1,-1,0,z) - 2*pow(Nc,3)*HPL(0,1,-1,0,z) + (2*HPL(0,1,-1,0,z))/(Nc*z) - (4*Nc*HPL(0,1,-1,0,z))/z + (2*pow(Nc,3)*HPL(0,1,-1,0,z))/z + (z*HPL(0,1,-1,0,z))/(2.*Nc) - Nc*z*HPL(0,1,-1,0,z) + (pow(Nc,3)*z*HPL(0,1,-1,0,z))/2. - HPL(0,1,0,0,z)/(4.*pow(Nc,3)) - (13*HPL(0,1,0,0,z))/(2.*Nc) + (55*Nc*HPL(0,1,0,0,z))/4. - 7*pow(Nc,3)*HPL(0,1,0,0,z) + HPL(0,1,0,0,z)/(pow(Nc,3)*z) - (9*HPL(0,1,0,0,z))/(4.*Nc*z) + (3*Nc*HPL(0,1,0,0,z))/(2.*z) - (pow(Nc,3)*HPL(0,1,0,0,z))/(4.*z) - (z*HPL(0,1,0,0,z))/(16.*pow(Nc,3)) + (13*z*HPL(0,1,0,0,z))/(8.*Nc) - (49*Nc*z*HPL(0,1,0,0,z))/16. + (3*pow(Nc,3)*z*HPL(0,1,0,0,z))/2. + (37*HPL(0,1,1,0,z))/(4.*Nc) - (37*Nc*HPL(0,1,1,0,z))/2. + (37*pow(Nc,3)*HPL(0,1,1,0,z))/4. - (3*HPL(0,1,1,0,z))/(2.*pow(Nc,3)*z) + (7*HPL(0,1,1,0,z))/(4.*Nc*z) + (Nc*HPL(0,1,1,0,z))/z - (5*pow(Nc,3)*HPL(0,1,1,0,z))/(4.*z) - (35*z*HPL(0,1,1,0,z))/(16.*Nc) + (35*Nc*z*HPL(0,1,1,0,z))/8. - (35*pow(Nc,3)*z*HPL(0,1,1,0,z))/16. - (2*HPL(1,0,-1,0,z))/Nc + 4*Nc*HPL(1,0,-1,0,z) - 2*pow(Nc,3)*HPL(1,0,-1,0,z) + (2*HPL(1,0,-1,0,z))/(Nc*z) - (4*Nc*HPL(1,0,-1,0,z))/z + (2*pow(Nc,3)*HPL(1,0,-1,0,z))/z + (z*HPL(1,0,-1,0,z))/(2.*Nc) - Nc*z*HPL(1,0,-1,0,z) + (pow(Nc,3)*z*HPL(1,0,-1,0,z))/2. - (2*HPL(1,0,0,0,z))/pow(Nc,3) + (5*HPL(1,0,0,0,z))/(2.*Nc) + Nc*HPL(1,0,0,0,z) - (3*pow(Nc,3)*HPL(1,0,0,0,z))/2. - (3*HPL(1,0,0,0,z))/(4.*pow(Nc,3)*z) + (27*HPL(1,0,0,0,z))/(4.*Nc*z) - (45*Nc*HPL(1,0,0,0,z))/(4.*z) + (21*pow(Nc,3)*HPL(1,0,0,0,z))/(4.*z) - (z*HPL(1,0,0,0,z))/(2.*pow(Nc,3)) + (31*z*HPL(1,0,0,0,z))/(8.*Nc) - (25*Nc*z*HPL(1,0,0,0,z))/4. + (23*pow(Nc,3)*z*HPL(1,0,0,0,z))/8. + (9*HPL(1,0,1,0,z))/Nc - 18*Nc*HPL(1,0,1,0,z) + 9*pow(Nc,3)*HPL(1,0,1,0,z) - (3*HPL(1,0,1,0,z))/(2.*pow(Nc,3)*z) + (3*HPL(1,0,1,0,z))/(2.*Nc*z) + (3*Nc*HPL(1,0,1,0,z))/(2.*z) - (3*pow(Nc,3)*HPL(1,0,1,0,z))/(2.*z) - (9*z*HPL(1,0,1,0,z))/(4.*Nc) + (9*Nc*z*HPL(1,0,1,0,z))/2. - (9*pow(Nc,3)*z*HPL(1,0,1,0,z))/4. + (4*HPL(1,1,0,0,z))/pow(Nc,3) - (43*HPL(1,1,0,0,z))/(4.*Nc) + (19*Nc*HPL(1,1,0,0,z))/2. - (11*pow(Nc,3)*HPL(1,1,0,0,z))/4. + (5*HPL(1,1,0,0,z))/(2.*pow(Nc,3)*z) - (73*HPL(1,1,0,0,z))/(4.*Nc*z) + (29*Nc*HPL(1,1,0,0,z))/z - (53*pow(Nc,3)*HPL(1,1,0,0,z))/(4.*z) + (z*HPL(1,1,0,0,z))/pow(Nc,3) - (115*z*HPL(1,1,0,0,z))/(16.*Nc) + (91*Nc*z*HPL(1,1,0,0,z))/8. - (83*pow(Nc,3)*z*HPL(1,1,0,0,z))/16. - (49*HPL(1,1,1,0,z))/(8.*pow(Nc,3)) + (241*HPL(1,1,1,0,z))/(8.*Nc) - (335*Nc*HPL(1,1,1,0,z))/8. + (143*pow(Nc,3)*HPL(1,1,1,0,z))/8. - (49*HPL(1,1,1,0,z))/(8.*pow(Nc,3)*z) + (241*HPL(1,1,1,0,z))/(8.*Nc*z) - (335*Nc*HPL(1,1,1,0,z))/(8.*z) + (143*pow(Nc,3)*HPL(1,1,1,0,z))/(8.*z) - (49*z*HPL(1,1,1,0,z))/(32.*pow(Nc,3)) + (241*z*HPL(1,1,1,0,z))/(32.*Nc) - (335*Nc*z*HPL(1,1,1,0,z))/32. + (143*pow(Nc,3)*z*HPL(1,1,1,0,z))/32. + (587*log(1 - z))/(64.*pow(Nc,3)) - (44075*log(1 - z))/(576.*Nc) + (72301*Nc*log(1 - z))/576. - (33509*pow(Nc,3)*log(1 - z))/576. - (34*nf*log(1 - z))/9. + (17*nf*log(1 - z))/(9.*pow(Nc,2)) + (17*pow(Nc,2)*nf*log(1 - z))/9. - (316*log(1 - z))/(27.*pow(Nc,3)*z) + (137377*log(1 - z))/(1728.*Nc*z) - (107041*Nc*log(1 - z))/(864.*z) + (96929*pow(Nc,3)*log(1 - z))/(1728.*z) + (295*nf*log(1 - z))/(48.*z) - (295*nf*log(1 - z))/(96.*pow(Nc,2)*z) - (295*pow(Nc,2)*nf*log(1 - z))/(96.*z) + (149*z*log(1 - z))/(64.*pow(Nc,3)) - (53*z*log(1 - z))/(18.*Nc) - (631*Nc*z*log(1 - z))/576. + (493*pow(Nc,3)*z*log(1 - z))/288. - (341*nf*z*log(1 - z))/144. + (341*nf*z*log(1 - z))/(288.*pow(Nc,2)) + (341*pow(Nc,2)*nf*z*log(1 - z))/288. + (11*pow(z,2)*log(1 - z))/(54.*pow(Nc,3)) - (pow(z,2)*log(1 - z))/(27.*Nc) - (29*Nc*pow(z,2)*log(1 - z))/54. + (10*pow(Nc,3)*pow(z,2)*log(1 - z))/27. + (7*z2*log(1 - z))/(16.*pow(Nc,3)) - (201*z2*log(1 - z))/(16.*Nc) + (381*Nc*z2*log(1 - z))/16. - (187*pow(Nc,3)*z2*log(1 - z))/16. - (3*nf*z2*log(1 - z))/2. + (3*nf*z2*log(1 - z))/(4.*pow(Nc,2)) + (3*pow(Nc,2)*nf*z2*log(1 - z))/4. + (93*z2*log(1 - z))/(16.*pow(Nc,3)*z) - (143*z2*log(1 - z))/(4.*Nc*z) + (865*Nc*z2*log(1 - z))/(16.*z) - (193*pow(Nc,3)*z2*log(1 - z))/(8.*z) - (3*nf*z2*log(1 - z))/(2.*z) + (3*nf*z2*log(1 - z))/(4.*pow(Nc,2)*z) + (3*pow(Nc,2)*nf*z2*log(1 - z))/(4.*z) - (39*z*z2*log(1 - z))/(64.*pow(Nc,3)) + (391*z*z2*log(1 - z))/(64.*Nc) - (665*Nc*z*z2*log(1 - z))/64. + (313*pow(Nc,3)*z*z2*log(1 - z))/64. - (3*nf*z*z2*log(1 - z))/8. + (3*nf*z*z2*log(1 - z))/(16.*pow(Nc,2)) + (3*pow(Nc,2)*nf*z*z2*log(1 - z))/16. + (17*z3*log(1 - z))/(8.*pow(Nc,3)) - (35*z3*log(1 - z))/(8.*Nc) + (19*Nc*z3*log(1 - z))/8. - (pow(Nc,3)*z3*log(1 - z))/8. + (5*z3*log(1 - z))/(8.*pow(Nc,3)*z) - (47*z3*log(1 - z))/(8.*Nc*z) + (79*Nc*z3*log(1 - z))/(8.*z) - (37*pow(Nc,3)*z3*log(1 - z))/(8.*z) + (17*z*z3*log(1 - z))/(32.*pow(Nc,3)) - (131*z*z3*log(1 - z))/(32.*Nc) + (211*Nc*z*z3*log(1 - z))/32. - (97*pow(Nc,3)*z*z3*log(1 - z))/32. - (49*z2*HPL(1,z)*log(1 - z))/(8.*pow(Nc,3)) + (241*z2*HPL(1,z)*log(1 - z))/(8.*Nc) - (335*Nc*z2*HPL(1,z)*log(1 - z))/8. + (143*pow(Nc,3)*z2*HPL(1,z)*log(1 - z))/8. - (49*z2*HPL(1,z)*log(1 - z))/(8.*pow(Nc,3)*z) + (241*z2*HPL(1,z)*log(1 - z))/(8.*Nc*z) - (335*Nc*z2*HPL(1,z)*log(1 - z))/(8.*z) + (143*pow(Nc,3)*z2*HPL(1,z)*log(1 - z))/(8.*z) - (49*z*z2*HPL(1,z)*log(1 - z))/(32.*pow(Nc,3)) + (241*z*z2*HPL(1,z)*log(1 - z))/(32.*Nc) - (335*Nc*z*z2*HPL(1,z)*log(1 - z))/32. + (143*pow(Nc,3)*z*z2*HPL(1,z)*log(1 - z))/32. + (2*HPL(-1,0,z)*log(1 - z))/Nc - 4*Nc*HPL(-1,0,z)*log(1 - z) + 2*pow(Nc,3)*HPL(-1,0,z)*log(1 - z) + (3*HPL(-1,0,z)*log(1 - z))/(Nc*z) - (6*Nc*HPL(-1,0,z)*log(1 - z))/z + (3*pow(Nc,3)*HPL(-1,0,z)*log(1 - z))/z - (z*HPL(-1,0,z)*log(1 - z))/Nc + 2*Nc*z*HPL(-1,0,z)*log(1 - z) - pow(Nc,3)*z*HPL(-1,0,z)*log(1 - z) + (63*HPL(1,0,z)*log(1 - z))/(16.*pow(Nc,3)) - (417*HPL(1,0,z)*log(1 - z))/(16.*Nc) + (645*Nc*HPL(1,0,z)*log(1 - z))/16. - (291*pow(Nc,3)*HPL(1,0,z)*log(1 - z))/16. - (3*nf*HPL(1,0,z)*log(1 - z))/2. + (3*nf*HPL(1,0,z)*log(1 - z))/(4.*pow(Nc,2)) + (3*pow(Nc,2)*nf*HPL(1,0,z)*log(1 - z))/4. + (9*HPL(1,0,z)*log(1 - z))/(16.*pow(Nc,3)*z) - (37*HPL(1,0,z)*log(1 - z))/(2.*Nc*z) + (565*Nc*HPL(1,0,z)*log(1 - z))/(16.*z) - (139*pow(Nc,3)*HPL(1,0,z)*log(1 - z))/(8.*z) - (3*nf*HPL(1,0,z)*log(1 - z))/(2.*z) + (3*nf*HPL(1,0,z)*log(1 - z))/(4.*pow(Nc,2)*z) + (3*pow(Nc,2)*nf*HPL(1,0,z)*log(1 - z))/(4.*z) + (73*z*HPL(1,0,z)*log(1 - z))/(64.*pow(Nc,3)) + (23*z*HPL(1,0,z)*log(1 - z))/(64.*Nc) - (265*Nc*z*HPL(1,0,z)*log(1 - z))/64. + (169*pow(Nc,3)*z*HPL(1,0,z)*log(1 - z))/64. - (3*nf*z*HPL(1,0,z)*log(1 - z))/8. + (3*nf*z*HPL(1,0,z)*log(1 - z))/(16.*pow(Nc,2)) + (3*pow(Nc,2)*nf*z*HPL(1,0,z)*log(1 - z))/16. - (2*HPL(0,-1,0,z)*log(1 - z))/Nc + 4*Nc*HPL(0,-1,0,z)*log(1 - z) - 2*pow(Nc,3)*HPL(0,-1,0,z)*log(1 - z) + (2*HPL(0,-1,0,z)*log(1 - z))/(Nc*z) - (4*Nc*HPL(0,-1,0,z)*log(1 - z))/z + (2*pow(Nc,3)*HPL(0,-1,0,z)*log(1 - z))/z + (z*HPL(0,-1,0,z)*log(1 - z))/(2.*Nc) - Nc*z*HPL(0,-1,0,z)*log(1 - z) + (pow(Nc,3)*z*HPL(0,-1,0,z)*log(1 - z))/2. + (9*HPL(0,1,0,z)*log(1 - z))/Nc - 18*Nc*HPL(0,1,0,z)*log(1 - z) + 9*pow(Nc,3)*HPL(0,1,0,z)*log(1 - z) - (3*HPL(0,1,0,z)*log(1 - z))/(2.*pow(Nc,3)*z) + (3*HPL(0,1,0,z)*log(1 - z))/(2.*Nc*z) + (3*Nc*HPL(0,1,0,z)*log(1 - z))/(2.*z) - (3*pow(Nc,3)*HPL(0,1,0,z)*log(1 - z))/(2.*z) - (9*z*HPL(0,1,0,z)*log(1 - z))/(4.*Nc) + (9*Nc*z*HPL(0,1,0,z)*log(1 - z))/2. - (9*pow(Nc,3)*z*HPL(0,1,0,z)*log(1 - z))/4. + (4*HPL(1,0,0,z)*log(1 - z))/pow(Nc,3) - (43*HPL(1,0,0,z)*log(1 - z))/(4.*Nc) + (19*Nc*HPL(1,0,0,z)*log(1 - z))/2. - (11*pow(Nc,3)*HPL(1,0,0,z)*log(1 - z))/4. + (5*HPL(1,0,0,z)*log(1 - z))/(2.*pow(Nc,3)*z) - (73*HPL(1,0,0,z)*log(1 - z))/(4.*Nc*z) + (29*Nc*HPL(1,0,0,z)*log(1 - z))/z - (53*pow(Nc,3)*HPL(1,0,0,z)*log(1 - z))/(4.*z) + (z*HPL(1,0,0,z)*log(1 - z))/pow(Nc,3) - (115*z*HPL(1,0,0,z)*log(1 - z))/(16.*Nc) + (91*Nc*z*HPL(1,0,0,z)*log(1 - z))/8. - (83*pow(Nc,3)*z*HPL(1,0,0,z)*log(1 - z))/16. - (49*HPL(1,1,0,z)*log(1 - z))/(8.*pow(Nc,3)) + (241*HPL(1,1,0,z)*log(1 - z))/(8.*Nc) - (335*Nc*HPL(1,1,0,z)*log(1 - z))/8. + (143*pow(Nc,3)*HPL(1,1,0,z)*log(1 - z))/8. - (49*HPL(1,1,0,z)*log(1 - z))/(8.*pow(Nc,3)*z) + (241*HPL(1,1,0,z)*log(1 - z))/(8.*Nc*z) - (335*Nc*HPL(1,1,0,z)*log(1 - z))/(8.*z) + (143*pow(Nc,3)*HPL(1,1,0,z)*log(1 - z))/(8.*z) - (49*z*HPL(1,1,0,z)*log(1 - z))/(32.*pow(Nc,3)) + (241*z*HPL(1,1,0,z)*log(1 - z))/(32.*Nc) - (335*Nc*z*HPL(1,1,0,z)*log(1 - z))/32. + (143*pow(Nc,3)*z*HPL(1,1,0,z)*log(1 - z))/32. - (111*pow(log(1 - z),2))/(32.*pow(Nc,3)) + (1263*pow(log(1 - z),2))/(32.*Nc) - (2193*Nc*pow(log(1 - z),2))/32. + (1041*pow(Nc,3)*pow(log(1 - z),2))/32. + (3*nf*pow(log(1 - z),2))/4. - (3*nf*pow(log(1 - z),2))/(8.*pow(Nc,2)) - (3*pow(Nc,2)*nf*pow(log(1 - z),2))/8. + (795*pow(log(1 - z),2))/(128.*pow(Nc,3)*z) - (17561*pow(log(1 - z),2))/(384.*Nc*z) + (27967*Nc*pow(log(1 - z),2))/(384.*z) - (12791*pow(Nc,3)*pow(log(1 - z),2))/(384.*z) - (9*nf*pow(log(1 - z),2))/(8.*z) + (9*nf*pow(log(1 - z),2))/(16.*pow(Nc,2)*z) + (9*pow(Nc,2)*nf*pow(log(1 - z),2))/(16.*z) - (351*z*pow(log(1 - z),2))/(128.*pow(Nc,3)) + (759*z*pow(log(1 - z),2))/(128.*Nc) - (465*Nc*z*pow(log(1 - z),2))/128. + (57*pow(Nc,3)*z*pow(log(1 - z),2))/128. + (3*nf*z*pow(log(1 - z),2))/8. - (3*nf*z*pow(log(1 - z),2))/(16.*pow(Nc,2)) - (3*pow(Nc,2)*nf*z*pow(log(1 - z),2))/16. + (pow(z,2)*pow(log(1 - z),2))/(3.*Nc) - (2*Nc*pow(z,2)*pow(log(1 - z),2))/3. + (pow(Nc,3)*pow(z,2)*pow(log(1 - z),2))/3. - (49*z2*pow(log(1 - z),2))/(16.*pow(Nc,3)) + (241*z2*pow(log(1 - z),2))/(16.*Nc) - (335*Nc*z2*pow(log(1 - z),2))/16. + (143*pow(Nc,3)*z2*pow(log(1 - z),2))/16. - (49*z2*pow(log(1 - z),2))/(16.*pow(Nc,3)*z) + (241*z2*pow(log(1 - z),2))/(16.*Nc*z) - (335*Nc*z2*pow(log(1 - z),2))/(16.*z) + (143*pow(Nc,3)*z2*pow(log(1 - z),2))/(16.*z) - (49*z*z2*pow(log(1 - z),2))/(64.*pow(Nc,3)) + (241*z*z2*pow(log(1 - z),2))/(64.*Nc) - (335*Nc*z*z2*pow(log(1 - z),2))/64. + (143*pow(Nc,3)*z*z2*pow(log(1 - z),2))/64. - (49*HPL(1,0,z)*pow(log(1 - z),2))/(16.*pow(Nc,3)) + (241*HPL(1,0,z)*pow(log(1 - z),2))/(16.*Nc) - (335*Nc*HPL(1,0,z)*pow(log(1 - z),2))/16. + (143*pow(Nc,3)*HPL(1,0,z)*pow(log(1 - z),2))/16. - (49*HPL(1,0,z)*pow(log(1 - z),2))/(16.*pow(Nc,3)*z) + (241*HPL(1,0,z)*pow(log(1 - z),2))/(16.*Nc*z) - (335*Nc*HPL(1,0,z)*pow(log(1 - z),2))/(16.*z) + (143*pow(Nc,3)*HPL(1,0,z)*pow(log(1 - z),2))/(16.*z) - (49*z*HPL(1,0,z)*pow(log(1 - z),2))/(64.*pow(Nc,3)) + (241*z*HPL(1,0,z)*pow(log(1 - z),2))/(64.*Nc) - (335*Nc*z*HPL(1,0,z)*pow(log(1 - z),2))/64. + (143*pow(Nc,3)*z*HPL(1,0,z)*pow(log(1 - z),2))/64. + (49*pow(log(1 - z),3))/(48.*pow(Nc,3)) - (241*pow(log(1 - z),3))/(48.*Nc) + (335*Nc*pow(log(1 - z),3))/48. - (143*pow(Nc,3)*pow(log(1 - z),3))/48. - (49*pow(log(1 - z),3))/(32.*pow(Nc,3)*z) + (241*pow(log(1 - z),3))/(32.*Nc*z) - (335*Nc*pow(log(1 - z),3))/(32.*z) + (143*pow(Nc,3)*pow(log(1 - z),3))/(32.*z) + (49*z*pow(log(1 - z),3))/(96.*pow(Nc,3)) - (241*z*pow(log(1 - z),3))/(96.*Nc) + (335*Nc*z*pow(log(1 - z),3))/96. - (143*pow(Nc,3)*z*pow(log(1 - z),3))/96. - (3781*log(z))/(384.*pow(Nc,3)) + (218041*log(z))/(3456.*Nc) - (333995*Nc*log(z))/3456. + (149983*pow(Nc,3)*log(z))/3456. + (115*nf*log(z))/108. - (115*nf*log(z))/(216.*pow(Nc,2)) - (115*pow(Nc,2)*nf*log(z))/216. + (347*log(z))/(192.*pow(Nc,3)*z) - (379*log(z))/(12.*Nc*z) + (11087*Nc*log(z))/(192.*z) - (895*pow(Nc,3)*log(z))/(32.*z) - (2213*nf*log(z))/(432.*z) + (2213*nf*log(z))/(864.*pow(Nc,2)*z) + (2213*pow(Nc,2)*nf*log(z))/(864.*z) - (6515*z*log(z))/(2304.*pow(Nc,3)) + (22721*z*log(z))/(6912.*Nc) + (13193*Nc*z*log(z))/6912. - (16369*pow(Nc,3)*z*log(z))/6912. + (181*nf*z*log(z))/108. - (181*nf*z*log(z))/(216.*pow(Nc,2)) - (181*pow(Nc,2)*nf*z*log(z))/216. - (77*pow(z,2)*log(z))/(216.*pow(Nc,3)) + (181*pow(z,2)*log(z))/(216.*Nc) - (131*Nc*pow(z,2)*log(z))/216. + (pow(Nc,3)*pow(z,2)*log(z))/8. + (27*z2*log(z))/(16.*pow(Nc,3)) - (79*z2*log(z))/(48.*Nc) - (85*Nc*z2*log(z))/48. + (83*pow(Nc,3)*z2*log(z))/48. + (nf*z2*log(z))/3. - (nf*z2*log(z))/(6.*pow(Nc,2)) - (pow(Nc,2)*nf*z2*log(z))/6. - (49*z2*log(z))/(24.*pow(Nc,3)*z) + (89*z2*log(z))/(12.*Nc*z) - (209*Nc*z2*log(z))/(24.*z) + (10*pow(Nc,3)*z2*log(z))/(3.*z) + (nf*z2*log(z))/(3.*z) - (nf*z2*log(z))/(6.*pow(Nc,2)*z) - (pow(Nc,2)*nf*z2*log(z))/(6.*z) - (19*z*z2*log(z))/(64.*pow(Nc,3)) - (157*z*z2*log(z))/(192.*Nc) + (485*Nc*z*z2*log(z))/192. - (271*pow(Nc,3)*z*z2*log(z))/192. + (nf*z*z2*log(z))/12. - (nf*z*z2*log(z))/(24.*pow(Nc,2)) - (pow(Nc,2)*nf*z*z2*log(z))/24. - (z3*log(z))/(2.*pow(Nc,3)) + (7*z3*log(z))/(2.*Nc) - (11*Nc*z3*log(z))/2. + (5*pow(Nc,3)*z3*log(z))/2. + (3*z3*log(z))/(4.*Nc*z) - (3*Nc*z3*log(z))/(2.*z) + (3*pow(Nc,3)*z3*log(z))/(4.*z) - (z*z3*log(z))/(8.*pow(Nc,3)) + (17*z*z3*log(z))/(16.*Nc) - (7*Nc*z*z3*log(z))/4. + (13*pow(Nc,3)*z*z3*log(z))/16. + (22*HPL(1,0,z)*log(z))/(3.*Nc) - (44*Nc*HPL(1,0,z)*log(z))/3. + (22*pow(Nc,3)*HPL(1,0,z)*log(z))/3. + (8*nf*HPL(1,0,z)*log(z))/3. - (4*nf*HPL(1,0,z)*log(z))/(3.*pow(Nc,2)) - (4*pow(Nc,2)*nf*HPL(1,0,z)*log(z))/3. + (22*HPL(1,0,z)*log(z))/(3.*Nc*z) - (44*Nc*HPL(1,0,z)*log(z))/(3.*z) + (22*pow(Nc,3)*HPL(1,0,z)*log(z))/(3.*z) + (8*nf*HPL(1,0,z)*log(z))/(3.*z) - (4*nf*HPL(1,0,z)*log(z))/(3.*pow(Nc,2)*z) - (4*pow(Nc,2)*nf*HPL(1,0,z)*log(z))/(3.*z) + (11*z*HPL(1,0,z)*log(z))/(6.*Nc) - (11*Nc*z*HPL(1,0,z)*log(z))/3. + (11*pow(Nc,3)*z*HPL(1,0,z)*log(z))/6. + (2*nf*z*HPL(1,0,z)*log(z))/3. - (nf*z*HPL(1,0,z)*log(z))/(3.*pow(Nc,2)) - (pow(Nc,2)*nf*z*HPL(1,0,z)*log(z))/3. + (35*log(1 - z)*log(z))/(8.*pow(Nc,3)) - (3839*log(1 - z)*log(z))/(72.*Nc) + (6733*Nc*log(1 - z)*log(z))/72. - (3209*pow(Nc,3)*log(1 - z)*log(z))/72. + (16*nf*log(1 - z)*log(z))/9. - (8*nf*log(1 - z)*log(z))/(9.*pow(Nc,2)) - (8*pow(Nc,2)*nf*log(1 - z)*log(z))/9. - (2867*log(1 - z)*log(z))/(576.*pow(Nc,3)*z) + (25439*log(1 - z)*log(z))/(576.*Nc*z) - (42277*Nc*log(1 - z)*log(z))/(576.*z) + (19705*pow(Nc,3)*log(1 - z)*log(z))/(576.*z) + (71*nf*log(1 - z)*log(z))/(18.*z) - (71*nf*log(1 - z)*log(z))/(36.*pow(Nc,2)*z) - (71*pow(Nc,2)*nf*log(1 - z)*log(z))/(36.*z) + (295*z*log(1 - z)*log(z))/(64.*pow(Nc,3)) - (6829*z*log(1 - z)*log(z))/(576.*Nc) + (5693*Nc*z*log(1 - z)*log(z))/576. - (1519*pow(Nc,3)*z*log(1 - z)*log(z))/576. + (17*nf*z*log(1 - z)*log(z))/72. - (17*nf*z*log(1 - z)*log(z))/(144.*pow(Nc,2)) - (17*pow(Nc,2)*nf*z*log(1 - z)*log(z))/144. - (pow(z,2)*log(1 - z)*log(z))/(18.*pow(Nc,3)) - (13*pow(z,2)*log(1 - z)*log(z))/(18.*Nc) + (29*Nc*pow(z,2)*log(1 - z)*log(z))/18. - (5*pow(Nc,3)*pow(z,2)*log(1 - z)*log(z))/6. + (7*z2*log(1 - z)*log(z))/(2.*pow(Nc,3)) - (9*z2*log(1 - z)*log(z))/(2.*Nc) - (3*Nc*z2*log(1 - z)*log(z))/2. + (5*pow(Nc,3)*z2*log(1 - z)*log(z))/2. + (2*z2*log(1 - z)*log(z))/(pow(Nc,3)*z) - (10*z2*log(1 - z)*log(z))/(Nc*z) + (14*Nc*z2*log(1 - z)*log(z))/z - (6*pow(Nc,3)*z2*log(1 - z)*log(z))/z + (7*z*z2*log(1 - z)*log(z))/(8.*pow(Nc,3)) - (41*z*z2*log(1 - z)*log(z))/(8.*Nc) + (61*Nc*z*z2*log(1 - z)*log(z))/8. - (27*pow(Nc,3)*z*z2*log(1 - z)*log(z))/8. - (pow(log(1 - z),2)*log(z))/(32.*pow(Nc,3)) - (101*pow(log(1 - z),2)*log(z))/(32.*Nc) + (205*Nc*pow(log(1 - z),2)*log(z))/32. - (103*pow(Nc,3)*pow(log(1 - z),2)*log(z))/32. - (3*nf*pow(log(1 - z),2)*log(z))/4. + (3*nf*pow(log(1 - z),2)*log(z))/(8.*pow(Nc,2)) + (3*pow(Nc,2)*nf*pow(log(1 - z),2)*log(z))/8. + (105*pow(log(1 - z),2)*log(z))/(32.*pow(Nc,3)*z) - (385*pow(log(1 - z),2)*log(z))/(16.*Nc*z) + (1225*Nc*pow(log(1 - z),2)*log(z))/(32.*z) - (35*pow(Nc,3)*pow(log(1 - z),2)*log(z))/(2.*z) - (3*nf*pow(log(1 - z),2)*log(z))/(4.*z) + (3*nf*pow(log(1 - z),2)*log(z))/(8.*pow(Nc,2)*z) + (3*pow(Nc,2)*nf*pow(log(1 - z),2)*log(z))/(8.*z) - (55*z*pow(log(1 - z),2)*log(z))/(128.*pow(Nc,3)) + (655*z*pow(log(1 - z),2)*log(z))/(128.*Nc) - (1145*Nc*z*pow(log(1 - z),2)*log(z))/128. + (545*pow(Nc,3)*z*pow(log(1 - z),2)*log(z))/128. - (3*nf*z*pow(log(1 - z),2)*log(z))/16. + (3*nf*z*pow(log(1 - z),2)*log(z))/(32.*pow(Nc,2)) + (3*pow(Nc,2)*nf*z*pow(log(1 - z),2)*log(z))/32. - (49*pow(log(1 - z),3)*log(z))/(48.*pow(Nc,3)) + (241*pow(log(1 - z),3)*log(z))/(48.*Nc) - (335*Nc*pow(log(1 - z),3)*log(z))/48. + (143*pow(Nc,3)*pow(log(1 - z),3)*log(z))/48. - (49*pow(log(1 - z),3)*log(z))/(48.*pow(Nc,3)*z) + (241*pow(log(1 - z),3)*log(z))/(48.*Nc*z) - (335*Nc*pow(log(1 - z),3)*log(z))/(48.*z) + (143*pow(Nc,3)*pow(log(1 - z),3)*log(z))/(48.*z) - (49*z*pow(log(1 - z),3)*log(z))/(192.*pow(Nc,3)) + (241*z*pow(log(1 - z),3)*log(z))/(192.*Nc) - (335*Nc*z*pow(log(1 - z),3)*log(z))/192. + (143*pow(Nc,3)*z*pow(log(1 - z),3)*log(z))/192. - (125*pow(log(z),2))/(128.*pow(Nc,3)) + (20635*pow(log(z),2))/(1152.*Nc) - (37895*Nc*pow(log(z),2))/1152. + (18385*pow(Nc,3)*pow(log(z),2))/1152. - (nf*pow(log(z),2))/18. + (nf*pow(log(z),2))/(36.*pow(Nc,2)) + (pow(Nc,2)*nf*pow(log(z),2))/36. + (143*pow(log(z),2))/(576.*pow(Nc,3)*z) - (2437*pow(log(z),2))/(288.*Nc*z) + (9319*Nc*pow(log(z),2))/(576.*z) - (1147*pow(Nc,3)*pow(log(z),2))/(144.*z) - (47*nf*pow(log(z),2))/(36.*z) + (47*nf*pow(log(z),2))/(72.*pow(Nc,2)*z) + (47*pow(Nc,2)*nf*pow(log(z),2))/(72.*z) - (173*z*pow(log(z),2))/(128.*pow(Nc,3)) + (3673*z*pow(log(z),2))/(1152.*Nc) - (2675*Nc*z*pow(log(z),2))/1152. + (559*pow(Nc,3)*z*pow(log(z),2))/1152. - (5*nf*z*pow(log(z),2))/144. + (5*nf*z*pow(log(z),2))/(288.*pow(Nc,2)) + (5*pow(Nc,2)*nf*z*pow(log(z),2))/288. + (7*pow(z,2)*pow(log(z),2))/(72.*pow(Nc,3)) + (pow(z,2)*pow(log(z),2))/(18.*Nc) - (29*Nc*pow(z,2)*pow(log(z),2))/72. + (pow(Nc,3)*pow(z,2)*pow(log(z),2))/4. - (13*z2*pow(log(z),2))/(16.*pow(Nc,3)) + (15*z2*pow(log(z),2))/(16.*Nc) + (9*Nc*z2*pow(log(z),2))/16. - (11*pow(Nc,3)*z2*pow(log(z),2))/16. - (3*z2*pow(log(z),2))/(8.*pow(Nc,3)*z) + (11*z2*pow(log(z),2))/(8.*Nc*z) - (13*Nc*z2*pow(log(z),2))/(8.*z) + (5*pow(Nc,3)*z2*pow(log(z),2))/(8.*z) - (13*z*z2*pow(log(z),2))/(64.*pow(Nc,3)) + (71*z*z2*pow(log(z),2))/(64.*Nc) - (103*Nc*z*z2*pow(log(z),2))/64. + (45*pow(Nc,3)*z*z2*pow(log(z),2))/64. + (log(1 - z)*pow(log(z),2))/pow(Nc,3) + (11*log(1 - z)*pow(log(z),2))/(24.*Nc) - (47*Nc*log(1 - z)*pow(log(z),2))/12. + (59*pow(Nc,3)*log(1 - z)*pow(log(z),2))/24. + (2*nf*log(1 - z)*pow(log(z),2))/3. - (nf*log(1 - z)*pow(log(z),2))/(3.*pow(Nc,2)) - (pow(Nc,2)*nf*log(1 - z)*pow(log(z),2))/3. - (15*log(1 - z)*pow(log(z),2))/(16.*pow(Nc,3)*z) + (455*log(1 - z)*pow(log(z),2))/(48.*Nc*z) - (775*Nc*log(1 - z)*pow(log(z),2))/(48.*z) + (365*pow(Nc,3)*log(1 - z)*pow(log(z),2))/(48.*z) + (2*nf*log(1 - z)*pow(log(z),2))/(3.*z) - (nf*log(1 - z)*pow(log(z),2))/(3.*pow(Nc,2)*z) - (pow(Nc,2)*nf*log(1 - z)*pow(log(z),2))/(3.*z) - (193*z*log(1 - z)*pow(log(z),2))/(96.*Nc) + (193*Nc*z*log(1 - z)*pow(log(z),2))/48. - (193*pow(Nc,3)*z*log(1 - z)*pow(log(z),2))/96. + (nf*z*log(1 - z)*pow(log(z),2))/6. - (nf*z*log(1 - z)*pow(log(z),2))/(12.*pow(Nc,2)) - (pow(Nc,2)*nf*z*log(1 - z)*pow(log(z),2))/12. + (pow(log(1 - z),2)*pow(log(z),2))/pow(Nc,3) - (43*pow(log(1 - z),2)*pow(log(z),2))/(16.*Nc) + (19*Nc*pow(log(1 - z),2)*pow(log(z),2))/8. - (11*pow(Nc,3)*pow(log(1 - z),2)*pow(log(z),2))/16. + (5*pow(log(1 - z),2)*pow(log(z),2))/(8.*pow(Nc,3)*z) - (73*pow(log(1 - z),2)*pow(log(z),2))/(16.*Nc*z) + (29*Nc*pow(log(1 - z),2)*pow(log(z),2))/(4.*z) - (53*pow(Nc,3)*pow(log(1 - z),2)*pow(log(z),2))/(16.*z) + (z*pow(log(1 - z),2)*pow(log(z),2))/(4.*pow(Nc,3)) - (115*z*pow(log(1 - z),2)*pow(log(z),2))/(64.*Nc) + (91*Nc*z*pow(log(1 - z),2)*pow(log(z),2))/32. - (83*pow(Nc,3)*z*pow(log(1 - z),2)*pow(log(z),2))/64. - (19*pow(log(z),3))/(64.*pow(Nc,3)) - (41*pow(log(z),3))/(192.*Nc) + (253*Nc*pow(log(z),3))/192. - (155*pow(Nc,3)*pow(log(z),3))/192. - (nf*pow(log(z),3))/6. + (nf*pow(log(z),3))/(12.*pow(Nc,2)) + (pow(Nc,2)*nf*pow(log(z),3))/12. - (41*pow(log(z),3))/(36.*Nc*z) + (41*Nc*pow(log(z),3))/(18.*z) - (41*pow(Nc,3)*pow(log(z),3))/(36.*z) - (nf*pow(log(z),3))/(6.*z) + (nf*pow(log(z),3))/(12.*pow(Nc,2)*z) + (pow(Nc,2)*nf*pow(log(z),3))/(12.*z) + (49*z*pow(log(z),3))/(768.*pow(Nc,3)) + (5*z*pow(log(z),3))/(768.*Nc) - (157*Nc*z*pow(log(z),3))/768. + (103*pow(Nc,3)*z*pow(log(z),3))/768. - (nf*z*pow(log(z),3))/24. + (nf*z*pow(log(z),3))/(48.*pow(Nc,2)) + (pow(Nc,2)*nf*z*pow(log(z),3))/48. - (log(1 - z)*pow(log(z),3))/(3.*pow(Nc,3)) + (5*log(1 - z)*pow(log(z),3))/(12.*Nc) + (Nc*log(1 - z)*pow(log(z),3))/6. - (pow(Nc,3)*log(1 - z)*pow(log(z),3))/4. - (log(1 - z)*pow(log(z),3))/(8.*pow(Nc,3)*z) + (9*log(1 - z)*pow(log(z),3))/(8.*Nc*z) - (15*Nc*log(1 - z)*pow(log(z),3))/(8.*z) + (7*pow(Nc,3)*log(1 - z)*pow(log(z),3))/(8.*z) - (z*log(1 - z)*pow(log(z),3))/(12.*pow(Nc,3)) + (31*z*log(1 - z)*pow(log(z),3))/(48.*Nc) - (25*Nc*z*log(1 - z)*pow(log(z),3))/24. + (23*pow(Nc,3)*z*log(1 - z)*pow(log(z),3))/48. + (11*pow(log(z),4))/(384.*pow(Nc,3)) + (11*pow(log(z),4))/(384.*Nc) - (55*Nc*pow(log(z),4))/384. + (11*pow(Nc,3)*pow(log(z),4))/128. - pow(log(z),4)/(16.*Nc*z) + (Nc*pow(log(z),4))/(8.*z) - (pow(Nc,3)*pow(log(z),4))/(16.*z) + (11*z*pow(log(z),4))/(1536.*pow(Nc,3)) - (101*z*pow(log(z),4))/(1536.*Nc) + (169*Nc*z*pow(log(z),4))/1536. - (79*pow(Nc,3)*z*pow(log(z),4))/1536.;
        check_imaginary_part(L1,__PRETTY_FUNCTION__);
        return real(L3)*pow(L,3)+real(L2)*pow(L,2)+real(L1)*L;
    }
}


