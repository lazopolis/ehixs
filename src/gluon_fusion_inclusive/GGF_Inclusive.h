#ifndef GGF_INCLUSIVE_H
#define GGF_INCLUSIVE_H


#include<string>
#include <vector>
#include "UserInterface.h"
#include "Convolutions.h" //: ExpansionTerm, WilsonCoefficients, BetaConstants
using namespace std;

class GGF_matrix_element
{
public:
     
     GGF_matrix_element(const string & _pi,const string & _pj,const string& _pord,
                        const string & _name);
     
     string name;
     int dimension;
     string parton_i;
     string parton_j;
     int alpha_power;
     
     
     friend ostream& operator<<(ostream&, const GGF_matrix_element&);
private:
     
};

class GGF_matrix_element_delta : public GGF_matrix_element{
public:
     GGF_matrix_element_delta(const string & _pi,const string & _pj,const string& _pord,
                        const string & _name, double (*_the_ggf_func)())
          : GGF_matrix_element(_pi,_pj,_pord,_name){the_me = _the_ggf_func;}
private:
     double (*the_me)();

};

class GGF_matrix_element_plus : public GGF_matrix_element{
public:
     GGF_matrix_element_plus(const string & _pi,const string & _pj,const string& _pord,
                              const string & _name, 
                              double (*_the_ggf_func)(double))
     : GGF_matrix_element(_pi,_pj,_pord,_name){the_me = _the_ggf_func;}
private:
     double (*the_me)(double);
     
};

class GGF_matrix_element_reg : public GGF_matrix_element{
public:
     GGF_matrix_element_reg(const string & _pi,const string & _pj,const string& _pord,
                             const string & _name, 
                             double (*_the_ggf_func)(double))
     : GGF_matrix_element(_pi,_pj,_pord,_name){the_me = _the_ggf_func;}
private:
     double (*the_me)(double);
     
};


class GGF_Inclusive
{
public:
     GGF_Inclusive(const UserInterface&);
     ~GGF_Inclusive(){};
     void perform();
private:
     vector<GGF_matrix_element*> matrix_elements;
     WilsonCoefficients WC;
     BetaConstants beta;
     double log_mur_sq_over_muf_sq;

     void make_sectors();

};





#endif