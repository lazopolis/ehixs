#ifndef INCLUSIVE_PROCESS_H
#define INCLUSIVE_PROCESS_H

#include "user_interface.h"
#include "vegas_adaptor.h"
#include "luminosity.h"
#include <vector>
using namespace std;

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

#endif