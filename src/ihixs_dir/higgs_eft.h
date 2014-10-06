#ifndef HIGGS_EFT_H
#define HIGGS_EFT_H

#include "constants.h"

class HiggsEFT
{
public:
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
    
    
    
    double n_N3LO_delta_L3(){
        return -72. * consts::z3
        - 33. / 4. * consts::pi_square
        + consts::nf * consts::pi_square / 2. ;
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
    
    //---------- plus terms
    
    
    
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
    
    double n_N3LO_D1(){return   (25./27.-(4./3.)*consts::z2)*pow(consts::nf,2.)
        +(-5345./36.+162.*consts::z3
          +94.*consts::z2
          )*consts::nf
        +30569./24.
        -1368.*consts::z2
        -3168.*consts::z3
        -(4158./5.)*pow(consts::z2,2.)
        ;}
    
    double n_N3LO_D2(){return   -1051.
        +1683.*consts::z2
        +4887.*consts::z3
        +(469./6.-102.*consts::z2)*consts::nf
        -(10./9.)*pow(consts::nf,2.)
        ;}
    
    double n_N3LO_D3(){return 925.
        -1512.*consts::z2
        -(164./3.)*consts::nf
        +(4./9.)*pow(consts::nf,2.)
        ;}
    
    double n_N3LO_D4(){return  -330.+20.*consts::nf;}
    
    double n_N3LO_D5(){return 216.;}
    
    
    // log terms
    
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
    
    double n_N3LO_D2_L(){return  -2775. /2.
        +378. * consts::pi_square
        -2. /3. * pow(consts::nf,2.)
        +82. *consts::nf
        ;}
    
    double n_N3LO_D2_L2(){return + (-891. /2. +27. * consts::nf);}
    
    double n_N3LO_D2_L3(){return-108. ;}
    
    double n_N3LO_D3_L(){return + (660. -40. * consts::nf) ;}
    
    double n_N3LO_D3_L2(){return + 432. ;}
    
    double n_N3LO_D4_L(){return -540. ;}
    
};

#endif