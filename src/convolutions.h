
#ifndef CONVOLUTIONS_H
#define CONVOLUTIONS_H
#include<string>
#include<vector>
#include "splitting_kernels.h"
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

class DeltaExpansionTerm : ExpansionTerm
{
public:
     DeltaExpansionTerm(const string& _name,const double & _value,int _a_power,int _e_power, int iprtn, int from_prtn): ExpansionTerm(_name,_value,_a_power,_e_power){mykernel = new Kernel(iprtn,from_prtn,_a_power,_e_power);}
     Kernel* mykernel;
};

//class Term
//{
//public:
//    Term(const string& name,int a_power,int e_power):name_(name),
//                                a_power_(a_power),e_power_(e_power){};
//    double value(){return value_;}
//    virtual void set_value(const double& value){value_=value;}
//protected:
//    double value_;
//private:
//    string name_;
//    int a_power_;
//    int e_power_;
//};
//
//class WC0_sq : public Term
//{
//public:
//    WC0_sq():Term("(c0^2 a^2)",2,0){};
//    void set_value(const WilsonCoefficients& WC){value_=pow(WC.c0,2.0);}
//}
//
//class WC0xWC1x2 : public Term
//{
//public:
//    WC0xWC1x2():Term("(2*c0*c1* a^3)",3,0){};
//    void set_value(const WilsonCoefficients& WC){value_=2.0*WC.c0*WC.c1;}
//}
//
//class WC1_sq_plus_2xC0xC2 : public Term
//{
//public:
//    WC1_sq_plus_2xC0xC2():Term("[(c1^2 + 2*c0*c2)*a^4],4,0){};
//    void set_value(const WilsonCoefficients& WC){value_=pow(WC.c1,2.0) + 2.0*WC.c0*WC.c2;}
//}

//WCET_vector.push_back(new WC0_sq);
//WCET_vector.push_back(new WC0xWC1x2);
//WCET_vector.push_back(new WC1_sq_plus_2xC0xC2);

//class ZOne: public Term
//{
//public:
//    ZOne():Term("(1)",0,0);
//    void set_value(){value_=1.0;}
//}
//                               
//                               
//class ZMinus2Beta0_OverEpsilon: public Term
//    {
//    public:
//    ZMinus2Beta0_OverEpsilon():Term("(a)*(-2*b0/e)",1,-1);
//    void set_value(){value_=-2.0*_beta.zero;}
//    }
//
//class Z2Beta0Log: public Term
//{
//public:
//    Z2Beta0Log():Term("(a)*(2*b0*L)",1,0);
//    void set_value(){value_=2.0*_beta.zero*_log_mur_sq_over_muf_sq;}
//}
//                               
//class Z3Beta0_OverEpsilonSq: public Term
//{
//public:
//    Z3Beta0_OverEpsilonSq():Term("(a^2)*(3*b0^2/e^2)",2,-2);
//    void set_value(){value_=3.0*pow(_beta.zero,2.0);}
//}
//                               
//                               
//class ZMinus2Beta1_OverEpsilon: public Term
//{
//public:
//    ZMinus2Beta1_OverEpsilon():Term("(a^2)*(-2*b1/e)",2,-1);
//    void set_value(){value_ = -2.0*_beta.one;}
//}
//
//class ZMinus4Beta0SqLog_OverEpsilon: public Term
//{
//public:
//    ZMinus4Beta0SqLog_OverEpsilon():Term("(a^2)*(-4*b0^2*L/e)",2,-1);
//    void set_value(){value_=-4.0*pow(_beta.zero,2.0)*_log_mur_sq_over_muf_sq;}
//}
//                               
//class Z3Beta0SqxLogSq: public Term
//{
//public:
//    Z3Beta0SqxLogSq():Term("(a^2)*(3*b0^2*L^2)",2,0);
//    void set_value()
//            {value_=3.0*pow(_beta.zero,2.0)*pow(_log_mur_sq_over_muf_sq,2);}
//}
//                               
//class Z2Beta1Log: public Term
//    {
//    public:
//    Z3Beta0SqxLogSq():Term("(a^2)*(2*b1*L)",2,0);
//    void set_value()
//    {value_=2.0*_beta.one*_log_mur_sq_over_muf_sq;}
//    }
//: a^2 * Z^2 = { a * (1+a*b0*L + a^2*b1*L + a^2*b0^2*L^2) * ( 1 - a*b0/e + a^2 * b0^2/e^2-a^2*b1/e) }^2
//ZREN_vector.push_back(new ZOne);
//ZREN_vector.push_back(new ZMinus2Beta0_OverEpsilon);
//ZREN_vector.push_back(new Z2Beta0Log);
//ZREN_vector.push_back(new Z3Beta0_OverEpsilonSq);
//ZREN_vector.push_back(new ZMinus2Beta1_OverEpsilon);
//ZREN_vector.push_back(new ZMinus4Beta0SqLog_OverEpsilon);
//ZREN_vector.push_back(new Z3Beta0SqxLogSq);
//ZREN_vector.push_back(new Z2Beta1Log);

//AREN_vector_trivial.push_back(new ExpansionTerm("(1)",1.0,0,0));
//
//AREN_vector.push_back(new ExpansionTerm("(1)",1.0,0,0));
//AREN_vector.push_back(new ExpansionTerm("(a)*(-b0/e)",-_beta.zero,1,-1));
//AREN_vector.push_back(new ExpansionTerm("(a)*b0*L)",_beta.zero*_log_mur_sq_over_muf_sq,1,0));



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
