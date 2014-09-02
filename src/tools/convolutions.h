
#ifndef CONVOLUTIONS_H
#define CONVOLUTIONS_H
#include<string>
#include<vector>
#include<list>
#include<complex>
//#include "splitting_kernels.h"
using namespace std;
#include "event.h"
#include "luminosity.h"

//typedef  pair<string,string> stringpair;

//class FxFxA;

///@struct InitialStateFlavors
///\brief  Container for information about initial state flavors
struct InitialStateFlavors
{

    ///@name Data members
    //@{

    string left;
    string right;

    //@}

    ///@name Member functions
    //@{

    InitialStateFlavors():
    left(),right()
    {}

    InitialStateFlavors(const InitialStateFlavors& that):
    left(that.left),right(that.right)
    {}

    InitialStateFlavors(const string& lef,const string& rig):
    left(lef),right(rig)
    {}

    ~InitialStateFlavors(){}

    //@}
};

///@struct NewMeExternalInfo
///\brief  Container for information about matrix elements
///\todo   Take "New" out of the name?
struct NewMeExternalInfo
{
    ///@name Data members
    //@{

    string name;
    InitialStateFlavors ISF;
    int alpha_power;

    //int epsilon_power_min;
    //int epsilon_power_max;

    //@}

    ///@name Member functions
    //@{

    NewMeExternalInfo():
    name(),ISF(),alpha_power()
    {}

    NewMeExternalInfo(const NewMeExternalInfo& that):
    name(that.name),ISF(that.ISF),alpha_power(that.alpha_power)
    {}

    ~NewMeExternalInfo(){}

    //@}
};

class pdf_desc_pair
{
public:
    pdf_desc_pair(int i,int j){left=i;right=j;}
    int left;
    int right;
    double flavor_dependent_prefactor;
};

class LuminosityBox
{
public:
    void AllocateLuminosity(Luminosity* lumi);
    void MatchPDFs(const string& left, const string& right, const string& pdf_selector_);
friend ostream& operator<<(ostream& stream, const LuminosityBox&);
    double give(const double& x1,const double& x2);
    
private:
    vector<pdf_desc_pair> luminosities_;
    vector<LuminositySinglePair*> my_lumis_;
    vector<double> flavor_dependent_prefactor_;
};




// gluon fusion legacy code below

class FFF
{
public:
    FFF(const string & p_i,const string & p_j,int ord){parton_i=p_i;parton_from=p_j;order=ord;epsilon_order= ord;}
    FFF(const string & p_i,const string & p_j,int ord,int eps_ord){parton_i=p_i;parton_from=p_j;order=ord;epsilon_order= eps_ord;}
    string parton_i,parton_from;
    int order,epsilon_order;
    bool is_valid();
    string give_parton(){return parton_i;}
    friend ostream& operator<<(ostream&, const FFF&);
    string name();
    string short_name(const string&) const;
};

struct WilsonCoefficients{
    complex<double> c0;
    complex<double> c1;
    complex<double> c2;
    complex<double> c0_qcd_only;
    complex<double> c1_qcd_only;
    complex<double> c2_qcd_only;
    bool exact;
    bool ew_soft;
};

struct BetaConstants{
    double zero,one,two,three;
};

class ExpansionTerm
{
public:
    ExpansionTerm(const string& _name,const double & _value,int _a_power,int _e_power){name=_name;value=_value;a_power=_a_power;e_power=_e_power;}
    int give_a_power()const {return a_power;}
    int give_e_power() const {return e_power;}
    string give_name() const {return name;}
    double give_value() const {return value;}
    void set_name(const string& _name){name=_name;}
    void set_value(const double& v){value = v;}
    friend ExpansionTerm operator*(const ExpansionTerm&,const ExpansionTerm&);
    
    friend ostream& operator<<(ostream& stream, const ExpansionTerm& P);
    
private:
    string name;
    double value;
    int a_power,e_power;
};



/*
class NewMatrixElement
{
public:
    NewMatrixElement(EventBox& event_box)
        : dimension_(0),pdf_selection_("none")
        {event_box_ = &event_box;}
    
public:
    friend ostream& operator<<(ostream&, const NewMatrixElement&);
    string give_name(){ostringstream  stream;
        stream<<*this;
        return (stream.str());}
    int alpha_power()const {return info_->alpha_power;}
    //int epsilon_power()const {return info_->epsilon_power;}
    //void set_epsilon_power(int ep) {info_->epsilon_power = ep;}
    
    
    string parton_i()const {return info_->ISF.left;}
    string parton_j()const {return info_->ISF.right;}
    string name()const {return info_->name;}
    string pdf_selection(){return pdf_selection_;}
    virtual void Evaluate(double*)=0;
    virtual void consolidate()=0;

    void set_S(const double& s){smax=s;}
    void SetAlphaStrong(const double& a_s_over_pi){a_s_over_pi_=a_s_over_pi;}
    void SetScales(const double& mur, const double& muf)
        {mur_=mur;muf_=muf;}
    int dimension() const {return dimension_;}
    void SetFFA(vector<FxFxA*> ffa){FFA_ = ffa;}

protected:
    NewMeExternalInfo* info_;
    int dimension_;
    EventBox* event_box_;

    double smax;
    //double initial_state_jacobian_;
    string pdf_selection_;
    vector<FxFxA*> FFA_;
    double a_s_over_pi_;
    double mur_;
    double muf_;
protected:
    double LL(const double& x1,const double& x2);
    
};


class Sector
{
public:
    Sector(LumiosityBox* lumi_box,NewMatrixElement* ME)
        :me_(ME),lumi_box_(lumi_box){};
    friend ostream& operator<<(ostream&, const Sector&);
    void DeterminePdfs(const vector<FSingle*>& all_pdfs);

    pdf_pair_list give_list_of_pdf_pairs();
    void AllocateLuminosity(Luminosity* lumi);
    void SetUpPrefactor(const double & a_s_over_pi);
    void PassAlphaStrong(const double & a_s_over_pi);
    void PassScales(const double& mur, const double& muf)
        {me_->SetScales(mur,muf);}
    void Evaluate(double* xx_vegas);
    string name(){return name_;}
    int dimension(){return me_->dimension();}
    double lumi_and_prefactor(const double& x1,const double& x2);
private:
    NewMatrixElement* me_;
    LuminosityBox* lumi_box_;
    string name_;
};
*/

#endif
