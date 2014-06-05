#ifndef GGF_MATRIX_ELEMENT_H
#define GGF_MATRIX_ELEMENT_H

#include<string>
#include<vector>
#include<iostream>
#include<sstream>
using namespace std;


typedef void (*pointer_to_Franz_gluon_fusion)
        (  
        const int&,const int&,const double&,
        const double&,const double&,const double&,
        const double&,const double&,const double&,
        const double&,const double&,const double&,
        const double&,const double&);

class MeExternalInfo
{
public:
    MeExternalInfo(const string & _pi,const string & _pj,const string& _pord,
                   const string & _name, int _epower,
                   const string & _me_approximation,int alpha_ew_pow=0);
public:
    string name;
    string parton_i;
    string parton_j;
    int alpha_power;
    int alpha_ew_power;
    int epsilon_power;
    string me_approximation;
};

class FranzBinder
{
public:
    FranzBinder(){_ptr = NULL; _num_sectors = 0; _is_franz = false;}
    FranzBinder(pointer_to_Franz_gluon_fusion ptr,int num_sec)
    :_ptr(ptr),_num_sectors(num_sec),min_sec_(1),max_sec_(num_sec)
    {_is_franz = true;}
    FranzBinder(pointer_to_Franz_gluon_fusion ptr,int min,int max)
    :_ptr(ptr),_num_sectors(max-min+1), min_sec_(min),max_sec_(max)
    {_is_franz = true;}
    int number_of_sectors(){return _num_sectors;}
    pointer_to_Franz_gluon_fusion func(){return _ptr;}
    bool is_franz(){return _is_franz;}
    int min_sec(){return min_sec_;}
    int max_sec(){return max_sec_;}
private:
    pointer_to_Franz_gluon_fusion _ptr;
    int _num_sectors;
    bool _is_franz;
    int min_sec_;
    int max_sec_;
};

class MatrixElement
{
public:
    MatrixElement(MeExternalInfo* info,
                  const string & _kin,
                  const string& _str_param,
                  const string& _the_ggf_func,
                  FranzBinder* fr,
                  double _e_exp_in_subtr);
    
public:
    int number_of_sectors_in_this_topology(){return _franz->number_of_sectors();}
    int min_sec(){return _franz->min_sec();}
    int max_sec(){return _franz->max_sec();}
    
    pointer_to_Franz_gluon_fusion franz_func(){return _franz->func();}
    bool is_franz_topology(){return _franz->is_franz();}
    friend ostream& operator<<(ostream&, const MatrixElement&);
    string give_name(){ostringstream  stream;
        stream<<*this;
        return (stream.str());}
    int alpha_power()const {return _info->alpha_power;}
    int alpha_ew_power()const {return _info->alpha_ew_power;}
    int epsilon_power()const {return _info->epsilon_power;}
    string parton_i()const {return _info->parton_i;}
    string parton_j()const {return _info->parton_j;}
    string name()const {return _info->name;}
    string me_approximation(){return _info->me_approximation;}
public://data
    int dimension;
    string parametrization;
    string the_ggf_func;
    double epsilon_exponent_in_z_subtraction;
    // for brute force RR
    vector<FranzBinder* > fr_;
private:
    MeExternalInfo* _info;
    FranzBinder* _franz;
    
    
};

class MatrixElementRR : public MatrixElement
{
public:
    MatrixElementRR(MeExternalInfo* info,
                    const string & _kin,
                    const string& _str_param,
                    const string& _the_ggf_func,
                    vector<FranzBinder* > fr,
                    double _e_exp_in_subtr)
    :MatrixElement( info,_kin,_str_param,
                   _the_ggf_func,NULL,_e_exp_in_subtr)
    {fr_ = fr;}
    //vector<FranzBinder* > fr_;
};



class GluonFusionMatrixElementBox
{
public:
    GluonFusionMatrixElementBox(const string&);
    int size(){return available_matrix_elements.size();}
    MatrixElement* give_me(int k){return available_matrix_elements[k];}
private://data
    vector<MatrixElement*> available_matrix_elements;
    string rr_treatment_;
private://methods
    void add_gg_sectors();
    void add_qg_sectors();
    void add_gq_sectors();
    void add_qqbar_sectors();
    void add_q1q2_sectors();
    void add_qq_sectors();
    void push_me(const string & _pi,
                 const string & _pj,
                 const string& _pord,
                 const string & _name,
                 const string & _kin,
                 const string& _str_param,
                 const string& _the_ggf_func,
                 int from_k,int to_k,
                 FranzBinder*,
                 const string& me_approx,
                 double eps_exp);
};


#endif



