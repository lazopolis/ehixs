#ifndef GAMMA_STAR_GAMMA_STAR_H
#define GAMMA_STAR_GAMMA_STAR_H

#include "production.h"

class GluonFusion;

typedef void (GluonFusion::*ptr_to_GluonFusion_function)();


class NewMeExternalInfo
{
public:
    NewMeExternalInfo();
public:
    string name;
    string parton_i;
    string parton_j;
    int alpha_power;
    int epsilon_power_min;
    int epsilon_power_max;
};


class NewMatrixElement
{
public:
    NewMatrixElement(): dimension_(0);
    
public:
    friend ostream& operator<<(ostream&, const MatrixElement&);
    string give_name(){ostringstream  stream;
        stream<<*this;
        return (stream.str());}
    int alpha_power()const {return info_->alpha_power;}
    int epsilon_power()const {return info_->epsilon_power;}
    string parton_i()const {return info_->parton_i;}
    string parton_j()const {return info_->parton_j;}
    string name()const {return info_->name;}

    virtual void SetDimension()=0;
    virtual void Evaluate(const double&)=0;
    

protected:
    MeExternalInfo* info_;
    int dimension_;
    
};

class GstarGstarMeLO: public NewMatrixElement
{
public:
    GstarGstarMeLO()
        {
        info_ = new NewMatrixElementInfo;
        dimension_ = 3;
        info_->name = "Born";
        info_->parton_i = "quark";
        info_->parton_j = "antiquark";
        info->alpha_power = 0;
        info->epsilon_power_min = 0;
        info->epsilon_power_max = 2;
        }
    void Evaluate(const double& w){JLO(w);}
}




class GammaStarGammaStarMatrixElementBox
{
public:
    GluonFusionMatrixElementBox();
    int size(){return available_matrix_elements.size();}
    MatrixElement* give_me(int k){return available_matrix_elements[k];}
private://data
    vector<MatrixElement*> available_matrix_elements;
};




class GammaStarGammaStarSectorBox{
public:
    GammaStarGammaStarSectorBox(const WilsonCoefficients&, const BetaConstants&,const double& log_mur_sq_over_muf_sq);
    vector<string> give_sector_names(const string & pleft,const string & pright,const string & myorder,const int & requested_epsilon_power, const string& me_approx);
    vector<SimpleSector*> give_necessary_sectors(const UserInterface & UI);
    int size(){return available_sectors.size();}
    SimpleSector* give(int i){return available_sectors[i];}
private://data
    GammaStarGammaStarMatrixElementBox* available_matrix_elements;
    vector<SimpleSector*> available_sectors;
    vector<string> _av_partons;
    BetaConstants _beta;
    double _log_mur_sq_over_muf_sq;
private://methods
    void build_sectors(const string& p_left,const string & p_right);
    void build_sectors_with_fixed_a_order(int,int,int,const string& p_left,
                                          const string & p_right);
    void build_sectors_with_fixed_a_order_and_pdfs(const FFF & F1,
                                                   const FFF & F2,int Sorder);
    void build_sectors_with_fixed_a_order_e_order_and_pdfs(const FFF & F1,
                                                           const FFF & F2,
                                                           int Sorder,
                                                           int Eorder,
                                                           const vector<MatrixElement*> & matching_mes);
    vector<FFF> give_possible_F(const string & parton,int f1order);
};




class NewSimpleSector
{
public:
    NewSimpleSector(const FFF& _f1,const FFF& _f2,const vector<ExpansionTerm*>& _factors,MatrixElement* _ME,Luminosity* lumi);
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
    virtual void SetInitialStateVars();
    void Evaluate();
private:
    double _prefactor;
    Luminosity* lumi_;
    double x1,x2;
};

struct ISparams{
    double x1LO,x2LO,zLO,measLO,cursLO;
    double x1,x2,z,meas,curs,Log_1mz;
    double lambda,phi;
};




class GammaStarGammaStar : public Production
{
public:
    GammaStarGammaStar(const UserInterface & UI);
    ~GammaStarGammaStar();
    void evaluate_sector();
    string sector_name(){return my_sector_name;}
    void SetNumberOfParticles() {return 6;}
    int number_of_necessary_sectors() = 0;
    //: gluon fusion specific construction:  to be removed!
    void book_production_event(const double &,const double &,
                                       const double &,const double &,
                                       const double &,const double &,
                                       const double &,const double &,
                               const double &){};//: public to integrate with fortran Fjet
    
    
    vector<string> give_sector_names(const string & pleft,
                                             const string & pright,
                                             const string & myorder,
                                             const int &,const string &)=0;
private:
    GammaStarGammaStarSectorBox* all_sectors;

};

#endif