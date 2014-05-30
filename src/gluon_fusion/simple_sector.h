#ifndef GGF_SIMPLE_SECTOR_H
#define GGF_SIMPLE_SECTOR_H

#include<vector>
using namespace std;
#include "matrix_element.h"
#include "convolutions.h"

class SimpleSector
{
public:
    SimpleSector(const FFF& _f1,const FFF& _f2,const vector<ExpansionTerm*>& _factors,MatrixElement* _ME);
    MatrixElement* ME;
    vector<ExpansionTerm*> factors;
    FFF F1,F2;
    int alpha_power,epsilon_power;
    friend ostream& operator<<(ostream&, const SimpleSector&);
    string name;
    
    void add_pair(int i,int j,int k,int m,pdf_pair_list & curlumi);
    void single_quark(int i,int j,int k,int m,pdf_pair_list & curlumi);
    void double_quark(int i,int j,int k,int m,pdf_pair_list & curlumi);
    int give_pid(const string & name);
    pdf_pair_list give_list_of_pdf_pairs();
    
    void setUpPrefactor(const double & a_s_over_pi);
    double sector_specific_prefactors_from_a_e_expansion(){return _prefactor;}
private:
    double _prefactor;
private://methods
    void uubar(pdf_pair_list& curlumi);
    void ddbar(pdf_pair_list& curlumi);
    
};

#endif

