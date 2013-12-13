
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
};


class NewMatrixElement
{
public:
    NewMatrixElement(EventBox& event_box): dimension_(0),pdf_selection_("none")
    {event_box_ = &event_box;}
    
public:
    friend ostream& operator<<(ostream&, const NewMatrixElement&);
    string give_name(){ostringstream  stream;
        stream<<*this;
        return (stream.str());}
    int alpha_power()const {return info_->alpha_power;}
    //int epsilon_power()const {return info_->epsilon_power;}
    //void set_epsilon_power(int ep) {info_->epsilon_power = ep;}
    int epsilon_power_min()const {return info_->epsilon_power_min;}
    int epsilon_power_max()const {return info_->epsilon_power_max;}
    
    string parton_i()const {return info_->ISF.left;}
    string parton_j()const {return info_->ISF.right;}
    string name()const {return info_->name;}
    string pdf_selection(){return pdf_selection_;}
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
    string pdf_selection_;
    
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
        as_order_(as_order),e_order_(e_order){construct_names();}
    bool init_flavor_matches(const string& fname)
    {
    if (fname==name_from_ or fname=="none") return true;
    else return false;
    }
    bool flavor_matches(const string& fname)
    {
    if (fname==name_i_) return true;
    else return false;
    }
    int alpha_power(){return as_order_;}
    int epsilon_power(){return -e_order_;}
    string name(){return name_i_;}
    string complete_name(){
        stringstream ss;
        
        ss<<"F"<<as_order_<<e_order_<<"["<<parton_from_<<"->"<<parton_i_<<"]";
        return ss.str();
        }
    int parton_i(){return parton_i_;}
private:
    int parton_i_;
    int parton_from_;
    int as_order_;
    int e_order_;
    void construct_names();
    void set_name(string& parton,int pid);
    string name_from_;
    string name_i_;
};




class FxF
{
public:
    FxF(FSingle* fleft,FSingle* fright)
        {
//        cout<<"\n[FxF]: "<<fleft->complete_name()<<" * "<<fright->complete_name();
        fleft_=fleft;fright_=fright;
        }
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
    friend ostream& operator<<(ostream& stream, const FxF& P);

private:
    FSingle* fleft_;
    FSingle* fright_;
};


class FxFxA
{
public:
    FxFxA(FxF* ff,ExpansionTerm* term)
        {ff_=ff;term_=term;}
    bool initial_flavor_is(const string& left,const string& right)
        {return ff_->initial_flavor_is(left,right);}
    int alpha_power(){return ff_->alpha_power()+term_->give_a_power();}
    int epsilon_power(){return ff_->epsilon_power()+term_->give_e_power();}
    friend ostream& operator<<(ostream& stream, const FxFxA& P);

private:
    FxF* ff_;
    ExpansionTerm* term_;
};

class Sector
{
public:
    Sector(vector<FxFxA*> FFA,NewMatrixElement* ME,int e_me)
        :me_(ME),FFA_(FFA),e_pow_of_matrix_element_(e_me){};
    friend ostream& operator<<(ostream&, const Sector&);
    void DeterminePdfs(const vector<FSingle*>& all_pdfs);
    void MultiplyWithPolynomial(const Polynomial& a_renorm);

    pdf_pair_list give_list_of_pdf_pairs();
    void AllocateLuminosity(Luminosity* lumi);
    void SetUpPrefactor(const double & a_s_over_pi){};
    void Evaluate(double* xx_vegas){};
    string name(){return name_;}
    int dimension(){return me_->dimension();}
private:
    NewMatrixElement* me_;
    vector<FxFxA*> FFA_;
    string name_;
    int e_pow_of_matrix_element_;

};




// composes the sectors from matrix elements, pdfs and a polynomial
// and constraints it as requested by UserInterface
class SectorBox{
public:
    SectorBox(const vector<NewMatrixElement*>&, const UserInterface&);
    int size(){return available_sectors.size();}
    Sector* give(int i){return available_sectors[i];}
private://data
    vector<Sector*> available_sectors;
    vector<FSingle*> all_pdfs;
    Polynomial a_renorm;
private:
    void set_up_pdfs();
    void set_up_polynomial();
    bool pdfs_match(FSingle* f1, FSingle* f2,const string& selection);
    vector<FxF*>  DeterminePdfs(NewMatrixElement* me,
                                const string& Fleft,const string& Fright,
                                int max_a);

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
