
#ifndef CONVOLUTIONS_H
#define CONVOLUTIONS_H
#include<string>
#include<vector>
#include<complex>
//#include "splitting_kernels.h"
using namespace std;


typedef  pair<string,string> stringpair;

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
    Polynomial(const string& name,const double & value,int a_pow,int e_pow){add_term(ExpansionTerm(name,value,a_pow,e_pow));}
    void truncate_in_alpha_up_to_power(int);
    void collect();
    vector<ExpansionTerm> coeff(int,int);
    friend Polynomial operator+(const Polynomial& p1,const Polynomial& p2);
    friend Polynomial operator+(const Polynomial& p1,const ExpansionTerm& p2);
    friend Polynomial operator+(const ExpansionTerm& p2,const Polynomial& p1);
    friend Polynomial operator*(const Polynomial& p1,const Polynomial& p2);
    friend ostream& operator<<(ostream&, const Polynomial&);

    int size() const {return terms.size();}
    ExpansionTerm operator[](int i)const {return terms[i];}
    void add_term(const ExpansionTerm& newterm){terms.push_back(newterm);}
private:
    vector<ExpansionTerm> terms;
};

/*
class Coefficient{
public:
    Coefficient(const string& name,const double& val): name_(name),val_(val){};
    friend Coefficient operator*(const Coefficient&,const Coefficient&);
private:
    string name_;
    double val;
};


template<class T>
fvector_decl<T> operator*(const fvector_decl<T>& v, const T& t)
{
    fvector_decl<T> r;
    for(unsigned i=0; i<4; ++i)
        r[i] = t*v[i];
        return r;
}



class AEMonomial{
public:
    AEMonomial(int a_power,int e_power)
            : a_power_(a_power),e_power_(e_power),coefficient_(1.0){};
    friend AEMonomial operator*(const AEMonomial&,const double& c);
    friend AEMonomial operator*(const AEMonomial&,const double& c);
private:
    int a_power_;
    int e_power_;
    Coefficient coefficient_;
};
*/
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
