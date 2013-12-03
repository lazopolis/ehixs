
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


typedef  pair<string,string> stringpair;


class InitialStateFlavors
{
public:
    InitialStateFlavors(){};
    InitialStateFlavors(const string& lef,const string& rig)
    {left = lef;right=rig;}
    string left;
    string right;
};

class NewMeExternalInfo
{
public:
    NewMeExternalInfo(){};
public:
    string name;
    InitialStateFlavors ISF;
    int alpha_power;
    int epsilon_power_min;
    int epsilon_power_max;
    int epsilon_power;
};


class NewMatrixElement
{
public:
    NewMatrixElement(EventBox& event_box): dimension_(0)
    {event_box_ = &event_box;}
    
public:
    friend ostream& operator<<(ostream&, const NewMatrixElement&);
    string give_name(){ostringstream  stream;
        stream<<*this;
        return (stream.str());}
    int alpha_power()const {return info_->alpha_power;}
    int epsilon_power()const {return info_->epsilon_power;}
    void set_epsilon_power(int ep) {info_->epsilon_power = ep;}
    int epsilon_power_min()const {return info_->epsilon_power_min;}
    int epsilon_power_max()const {return info_->epsilon_power_max;}
    
    string parton_i()const {return info_->ISF.left;}
    string parton_j()const {return info_->ISF.right;}
    string name()const {return info_->name;}
    
    virtual void Evaluate(double*)=0;
    
    int dimension() const {return dimension_;}
    void SetLuminosity(Luminosity* lumi){lumi_ = lumi;}
    void SetUpPrefactor(const double & prefactor){prefactor_ = prefactor;}
protected:
    NewMeExternalInfo* info_;
    int dimension_;
    EventBox* event_box_;
    Luminosity* lumi_;
    double prefactor_;
    double initial_state_jacobian_;
    
};



// monomial in a,e
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


//polynomial in a,e
class Polynomial{
public:
    Polynomial(){};
    Polynomial(const string& name,const double & value,int a_pow,int e_pow){terms.push_back(new ExpansionTerm(name,value,a_pow,e_pow));}
    void truncate_in_alpha_up_to_power(int);
    void collect();
    vector<ExpansionTerm*> coeff(int,int);
    friend Polynomial operator+(const Polynomial& p1,const Polynomial& p2);
    friend Polynomial operator+(const Polynomial& p1,const ExpansionTerm& p2);
    friend Polynomial operator+(const ExpansionTerm& p2,const Polynomial& p1);
    friend Polynomial operator*(const Polynomial& p1,const Polynomial& p2);
    friend ostream& operator<<(ostream&, const Polynomial&);
    Polynomial pow(int k);
    int size() const {return terms.size();}
    ExpansionTerm* operator[](int i)const {return terms[i];}
    void add_term(ExpansionTerm* newterm){terms.push_back(newterm);}
private:
    vector<ExpansionTerm*> terms;
};

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
};


//------------------------------------------------------------------------------



class FSingle
{
public:
    FSingle(int parton_i,int parton_from,int as_order,int e_order)
        :parton_i_(parton_i),parton_from_(parton_from),
        as_order_(as_order),e_order_(e_order){construct_name();}
    bool init_flavor_matches(const string& fname)
    {
    if (fname==name_from_) return true;
    else return false;
    }
    int alpha_power(){return as_order_;}
    int epsilon_power(){return -e_order_;}
private:
    int parton_i_;
    int parton_from_;
    int as_order_;
    int e_order_;
    void construct_name();
    string name_from_;
};

class ListOfSingleF
{
public:
    ListOfSingleF(int parton_to);
    FSingle* give(int i){return f_[i];}
    int size(){return f_.size();}
private:
    vector<FSingle*> f_;
};



class FxF
{
public:
    FxF(FSingle* fleft,FSingle* fright){fleft_=fleft;fright_=fright;}
    bool initial_flavor_is(const string& left,const string& right)
    {
    if (
        fleft_->init_flavor_matches(left)
        and fright_->init_flavor_matches(right)
        )
        {return true;}
    else return false;
    }
    int alpha_power(){return fleft_->alpha_power()+fright_->alpha_power();}
    int epsilon_power(){return fleft_->epsilon_power()+fright_->epsilon_power();}
private:
    FSingle* fleft_;
    FSingle* fright_;
};

class ListOfFF
{
public:
    ListOfFF(const string& fleft,const string& fright);
    FxF* operator[](int i){return ff_[i];}
    int size(){return ff_.size();}
private:
    vector<FxF*> ff_;
private:
    bool flavors_match(int i,int j,const string& left, const string& right);
};


class FxFxA
{
public:
    FxFxA(FxF* ff,ExpansionTerm* term,int pole)
        {ff_=ff;term_=term;pole_for_me_ = pole;}
    bool initial_flavor_is(const string& left,const string& right)
        {return ff_->initial_flavor_is(left,right);}
    int alpha_power(){return ff_->alpha_power()+term_->give_a_power();}
    int epsilon_power(){return ff_->epsilon_power()+term_->give_e_power();}

private:
    FxF* ff_;
    ExpansionTerm* term_;
    int pole_for_me_;
};

class Sector
{
public:
    Sector(NewMatrixElement* ME);
    friend ostream& operator<<(ostream&, const Sector&);
//    void add_pair(int i,int j,int k,int m,pdf_pair_list & curlumi);
//    void single_quark(int i,int j,int k,int m,pdf_pair_list & curlumi);
//    void double_quark(int i,int j,int k,int m,pdf_pair_list & curlumi);
//    int give_pid(const string & name);
    pdf_pair_list give_list_of_pdf_pairs();
    void AllocateLuminosity(Luminosity* lumi);
    void SetUpPrefactor(const double & a_s_over_pi){};
    void Evaluate(double* xx_vegas){};
    void restrict_as(int min_a_power_requested,int max_a_power_requested);
    void restrict_epsilon(int pole);
    void restrict_flavor(const string& left,const string& right);
    string name(){return name_;}
    int dimension(){return me_->dimension();}
private:
    NewMatrixElement* me_;
    list<FxFxA*> FFA_;
    string name_;
};





class SectorBox{
public:
    SectorBox(const vector<NewMatrixElement*>&, const UserInterface&);
    int size(){return available_sectors.size();}
    Sector* give(int i){return available_sectors[i];}
private://data
    vector<Sector*> available_sectors;
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


#endif
