#ifndef INCLUSIVE_PROCESS_H
#define INCLUSIVE_PROCESS_H

#include "user_interface.h"
#include "vegas_adaptor.h"
#include "luminosity.h"
#include "constants.h"
#include <vector>
using namespace std;



class HiggsEFT
{
public:

private:
    double n_LO_delta(){return 1.0;}
    double n_NLO_delta(){return 6.0*consts::z2;}
    double n_NNLO_delta(){return
                    1071.0 / 8.0 * consts::z4
                    - 54.0 * pow(consts::z2,2.0)
                    + 67.0/2.0 * consts::z2
                    - 165./4. * consts::z3
                    + 873./16.
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
};


class LuminosityIntegral: public CoolInt
{
public:
    virtual double evaluateIntegral(const double* xx)=0;
    void Configure(LuminosityStack* lumi, const double& tau)
        {lumi_=lumi; tau_ = tau;}
protected:
    LuminosityStack* lumi_;
    double tau_;
};

class LuminosityIntegralDelta: public LuminosityIntegral
{
public:
    double evaluateIntegral(const double* xx);
};

class LuminosityIntegralPlus: public LuminosityIntegral
{
public:
    LuminosityIntegralPlus(){set_dimensions(2);}
    double evaluateIntegral(const double* xx);
    virtual double log_power()=0;
};

class LuminosityIntegralDD0: public LuminosityIntegralPlus
{
public:
    double log_power(){return 0.0;}
};

class LuminosityIntegralDD1: public LuminosityIntegralPlus
{
public:
    double log_power(){return 1.0;}
};

class LuminosityIntegralDD2: public LuminosityIntegralPlus
{
public:
    double log_power(){return 2.0;}
};

class LuminosityIntegralDD3: public LuminosityIntegralPlus
{
public:
    double log_power(){return 3.0;}
};

class LuminosityIntegralDD4: public LuminosityIntegralPlus
{
public:
    double log_power(){return 4.0;}
};

class LuminosityIntegralDD5: public LuminosityIntegralPlus
{
public:
    double log_power(){return 5.0;}
};

/*
class CrossSectionTerm : public CoolInt
{
public:
    CrossSectionTerm(){};
    void perform(){call_vegas();}
    virtual double evaluateIntegral(const double xx[])=0;
    LuminosityStack lumistack;
protected:
     
    double matrix_element;
    double x1;
    double measure;
    double tau;
    double z;
protected:
    virtual void generate_initial_state_variables(const double xx[])=0;
    virtual void set_matrix_element()=0;
    virtual double LL()=0;    
};

class DeltaTerm : public CrossSectionTerm
{
public:
    DeltaTerm(){set_dimensions(1);}
    double evaluateIntegral(const double xx[]);
protected:
    void generate_initial_state_variables(const double xx[]);
    double LL();
};

class PlusTerm : public CrossSectionTerm
{
public:
    PlusTerm(){set_dimensions(2);}
    double evaluateIntegral(const double xx[]);
protected:
    void generate_initial_state_variables(const double xx[]);
    double LL();
};

class RegularTerm : public CrossSectionTerm
{
public:
    RegularTerm(){set_dimensions(2);}
    double evaluateIntegral(const double xx[]);
protected:
    void generate_initial_state_variables(const double xx[]);
    double LL();
};


class LO_delta_gg : public DeltaTerm
{
public:
    void set_matrix_element();
};

class NLO_delta_gg : public DeltaTerm
{
public:
    void set_matrix_element();
};

class NNLO_delta_gg : public DeltaTerm
{
public:
    void set_matrix_element();
};

class NLO_plus_gg : public PlusTerm
{
public:
    void set_matrix_element();
};

class NNLO_plus_gg : public PlusTerm
{
public:
    void set_matrix_element();
};

class NLO_reg_gg : public RegularTerm
{
public:
    void set_matrix_element();
};

class NNLO_reg_gg : public RegularTerm
{
public:
    void set_matrix_element();
};

class InclusiveProcess
{
public: 
    InclusiveProcess(const UserInterface& UI);
    void perform();
private:
    LO_delta_gg EggLOdelta;
    NLO_delta_gg EggNLOdelta;
    NNLO_delta_gg EggN2LOdelta;
    NLO_plus_gg EggNLOplus;
    NNLO_plus_gg EggN2LOplus;
    NLO_reg_gg EggNLOregular;
    NNLO_reg_gg EggN2LOregular;
    Luminosity* lumi;
};
*/
#endif