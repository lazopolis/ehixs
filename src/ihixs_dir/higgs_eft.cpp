#include "higgs_eft.h"
#include "chaplin.h"
#include <complex>
using namespace std;

namespace HEFT {

    vector<double> n_delta_at_muf(const double& _log_muf_mh_sq)
    {
        vector<double> res;
        res.push_back(n_LO_delta());
        res.push_back(n_NLO_delta());
        res.push_back(n_NNLO_delta()
                      + n_NNLO_delta_L()*_log_muf_mh_sq
                      + n_NNLO_delta_L2()*pow(_log_muf_mh_sq,2.));
        res.push_back(n_N3LO_delta()
                             + n_N3LO_delta_L()*_log_muf_mh_sq
                             + n_N3LO_delta_L2()*pow(_log_muf_mh_sq,2.)
                             + n_N3LO_delta_L3()*pow(_log_muf_mh_sq,3.));
        return res;
    }
    
    vector<double> n_D0_at_muf(const double& _log_muf_mh_sq)
    {
        vector<double> res;
        res.push_back(0.0);
        res.push_back(n_NLO_D0_L() * _log_muf_mh_sq);
        res.push_back(n_NNLO_D0()
                      + n_NNLO_D0_L()*_log_muf_mh_sq
                      + n_NNLO_D0_L2()*pow(_log_muf_mh_sq,2.));
        res.push_back(n_N3LO_D0()
                      + n_N3LO_D0_L()*_log_muf_mh_sq
                      + n_N3LO_D0_L2()*pow(_log_muf_mh_sq,2.)
                      + n_N3LO_D0_L3()*pow(_log_muf_mh_sq,3.));
        return res;
    }
    
    vector<double> n_D1_at_muf(const double& _log_muf_mh_sq)
    {
        vector<double> res;
        res.push_back(0.0);
        res.push_back(n_NLO_D1());
        res.push_back(n_NNLO_D1()
                      + n_NNLO_D1_L()*_log_muf_mh_sq
                      + n_NNLO_D1_L2()*pow(_log_muf_mh_sq,2.));
        res.push_back(n_N3LO_D1()
                      + n_N3LO_D1_L()*_log_muf_mh_sq
                      + n_N3LO_D1_L2()*pow(_log_muf_mh_sq,2.)
                      + n_N3LO_D1_L3()*pow(_log_muf_mh_sq,3.));
        return res;
    }
    
    vector<double> n_D2_at_muf(const double& _log_muf_mh_sq)
    {
        vector<double> res;
        res.push_back(0.0);
        res.push_back(0.0);
        res.push_back(n_NNLO_D2()
                      + n_NNLO_D2_L()*_log_muf_mh_sq);
        res.push_back(n_N3LO_D2()
                      + n_N3LO_D2_L()*_log_muf_mh_sq
                      + n_N3LO_D2_L2()*pow(_log_muf_mh_sq,2.)
                      + n_N3LO_D2_L3()*pow(_log_muf_mh_sq,3.));
        return res;
    }
    
    vector<double> n_D3_at_muf(const double& _log_muf_mh_sq)
    {
        vector<double> res;
        res.push_back(0.0);
        res.push_back(0.0);
        res.push_back(n_NNLO_D3());
        res.push_back(n_N3LO_D3()
                      + n_N3LO_D3_L()*_log_muf_mh_sq
                      + n_N3LO_D3_L2()*pow(_log_muf_mh_sq,2.));
        return res;
    }
    
    vector<double> n_D4_at_muf(const double& _log_muf_mh_sq)
    {
        vector<double> res;
        res.push_back(0.0);
        res.push_back(0.0);
        res.push_back(0.0);
        res.push_back(n_N3LO_D4()
                      + n_N3LO_D4_L()*_log_muf_mh_sq);
        return res;
    }
    
    vector<double> n_D5_at_muf(const double& _log_muf_mh_sq)
    {
        vector<double> res;
        res.push_back(0.0);
        res.push_back(0.0);
        res.push_back(0.0);
        res.push_back(n_N3LO_D5());
        return res;
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

    
}


