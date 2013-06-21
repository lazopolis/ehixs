
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

/*
class Topology;

class PartonicMode;



class PartonicXS
{
public:
     PartonicXS(const string & i,const string & j,int ord){parton_i=i;parton_j=j,order=ord;}
     string parton_i,parton_j;
     int order;
     int lowest_e_power;
     //friend ostream& operator<<(ostream&, const PartonicXS&);
     vector<PartonicMode *> partonic_modes;
};

#include "Luminosity.h"

class Convolution
{
public:
     Convolution(const FFF & ff1,const FFF & ff2,PartonicXS * s):f1(ff1),f2(ff2),sigma(s){};
     FFF f1,f2;
     PartonicXS * sigma;
     string order() const {string ordname[3]={"LO","NLO","NNLO"};return ordname[f1.order+f2.order+sigma->order];}
     int alpha_power(){return f1.order+f2.order+sigma->order; }
     //friend ostream& operator<<(ostream&, const Convolution&);
     //string topology_name(int i_partonic_mode,int j_topology) const;
     //vector<string> all_names();
     pdf_pair_list give_list_of_pdf_pairs();
     bool check_lumi(int,int,int,int);
     void add_pair(int i,int j,int k,int m,pdf_pair_list & curlumi);
     void single_quark(int i,int j,int k,int m,pdf_pair_list & curlumi);
     void double_quark(int i,int j,int k,int m,pdf_pair_list & curlumi);
     int give_pid(const string & name);


};


class partonicXs_nameholder{
public:
     partonicXs_nameholder(string pi,string pj,int ord,int _e_power):p_i(pi),p_j(pj),order(ord),e_power(_e_power){};
     string p_i,p_j;
     int order;
     int e_power;
};


class Channel
{
public:
     Channel(const string & i,const string & j,
             const vector<PartonicXS*>& _available_xs,
             const vector<string>& av_part,const vector<ExpansionTerm*> _ET1,const vector<ExpansionTerm*> _ET2,int _pole) {parton_left=i;parton_right=j;available_xs=_available_xs;available_partons=av_part;ET1=_ET1;ET2=_ET2;pole=_pole;
                    build_convolutions();}
     string parton_left,parton_right;
     int order,pole;
     //int number_of_sectors();
     vector<string> available_partons;
     void build_convolutions();
     void build_convolutions_special(int f1order,int f2order,int Sorder,const string&pleft,const string &pright);
     vector<Convolution*> all_convs;
     //friend ostream& operator<<(ostream&, const Channel&);
     vector<PartonicXS*> available_xs;
     vector<ExpansionTerm*> ET1,ET2;
     void check_and_create(const FFF& f_i,const FFF& f_j,const partonicXs_nameholder & pos_sig);


};
*/

#endif
