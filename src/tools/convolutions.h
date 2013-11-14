
#ifndef CONVOLUTIONS_H
#define CONVOLUTIONS_H
#include<string>
#include<vector>
//#include "splitting_kernels.h"
using namespace std;


typedef  pair<string,string> stringpair;

class ExpansionTerm
{
public:
     ExpansionTerm(const string& _name,const double & _value,int _a_power,int _e_power){name=_name;value=_value;a_power=_a_power;e_power=_e_power;}
     int give_a_power(){return a_power;}
     int give_e_power(){return e_power;}
     string give_name(){return name;}
     double give_value(){return value;}
private:
     string name;
     double value;
     int a_power,e_power;
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
     double c0,c1,c2;
};

struct BetaConstants{
     double zero,one,two,three;
};


#endif
