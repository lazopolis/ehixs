#include "higgs_eft.h"
#include "chaplin.h"
#include <complex>
using namespace std;

namespace HEFT {

    AsSeries n_delta_at_muf(const double& _log_muf_mh_sq)
    {
        return AsSeries(0,
                        n_LO_delta(),
                        n_NLO_delta(),
                        n_NNLO_delta()
                      + n_NNLO_delta_L()*_log_muf_mh_sq
                      + n_NNLO_delta_L2()*pow(_log_muf_mh_sq,2.),
                        n_N3LO_delta()
                             + n_N3LO_delta_L()*_log_muf_mh_sq
                             + n_N3LO_delta_L2()*pow(_log_muf_mh_sq,2.)
                             + n_N3LO_delta_L3()*pow(_log_muf_mh_sq,3.)
                      );
    }
    
    AsSeries n_D0_at_muf(const double& _log_muf_mh_sq)
    {
        return AsSeries(1,
                        n_NLO_D0_L() * _log_muf_mh_sq,
                        n_NNLO_D0()
                      + n_NNLO_D0_L()*_log_muf_mh_sq
                      + n_NNLO_D0_L2()*pow(_log_muf_mh_sq,2.),
                        n_N3LO_D0()
                      + n_N3LO_D0_L()*_log_muf_mh_sq
                      + n_N3LO_D0_L2()*pow(_log_muf_mh_sq,2.)
                      + n_N3LO_D0_L3()*pow(_log_muf_mh_sq,3.)
                        );
    }
    
    AsSeries n_D1_at_muf(const double& _log_muf_mh_sq)
    {
        return AsSeries(1,
                        n_NLO_D1(),
        n_NNLO_D1()
                      + n_NNLO_D1_L()*_log_muf_mh_sq
                      + n_NNLO_D1_L2()*pow(_log_muf_mh_sq,2.),
        n_N3LO_D1()
                      + n_N3LO_D1_L()*_log_muf_mh_sq
                      + n_N3LO_D1_L2()*pow(_log_muf_mh_sq,2.)
                        + n_N3LO_D1_L3()*pow(_log_muf_mh_sq,3.)
                        );
    }
    
    AsSeries n_D2_at_muf(const double& _log_muf_mh_sq)
    {
        return AsSeries(2,
                        n_NNLO_D2()
                      + n_NNLO_D2_L()*_log_muf_mh_sq,
                        n_N3LO_D2()
                      + n_N3LO_D2_L()*_log_muf_mh_sq
                      + n_N3LO_D2_L2()*pow(_log_muf_mh_sq,2.)
                      + n_N3LO_D2_L3()*pow(_log_muf_mh_sq,3.)
                      );
    }
    
    AsSeries n_D3_at_muf(const double& _log_muf_mh_sq)
    {
        return AsSeries(2,
                        n_NNLO_D3(),
                        n_N3LO_D3()
                      + n_N3LO_D3_L()*_log_muf_mh_sq
                      + n_N3LO_D3_L2()*pow(_log_muf_mh_sq,2.)
                      );
    }
    
    AsSeries n_D4_at_muf(const double& _log_muf_mh_sq)
    {
        return AsSeries(3,
                        n_N3LO_D4()
                      + n_N3LO_D4_L()*_log_muf_mh_sq);
    }
    
    AsSeries n_D5_at_muf(const double& _log_muf_mh_sq)
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
    
    double qg_n3lo_r_lz1(const double& z, const double& L)
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
    
    double qg_n3lo_r_lz2(const double& z, const double& L)
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
    
    double qg_n3lo_r_lz3(const double& z, const double& L)
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
    
    double qg_n3lo_r_lz4(const double& z, const double& L)
    {
        const double nf = consts::nf;
        const double Nc = QCD::Nc;
        
        complex<double> res =
        -((-1 + pow(Nc,2))*(687 - 15118*pow(Nc,2) + 9155*pow(Nc,4) + 72*L*(27 - 194*pow(Nc,2) + 739*pow(Nc,4)) + 1012*Nc*nf - 3212*pow(Nc,3)*nf))/(27648.*pow(Nc,3))
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(1.-z),4.);
    }
    
    double qg_n3lo_r_lz5(const double& z, const double& L)
    {
        const double Nc = QCD::Nc;
        
        complex<double> res =
        (-27 + 181*pow(Nc,2) - 741*pow(Nc,4) + 587*pow(Nc,6))/(768.*pow(Nc,3))
        ;
        check_imaginary_part(res,__PRETTY_FUNCTION__);
        return real(res)*pow(log(1.-z),5.);
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
}


