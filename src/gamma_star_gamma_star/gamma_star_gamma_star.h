#ifndef GAMMA_STAR_GAMMA_STAR_H
#define GAMMA_STAR_GAMMA_STAR_H

#include "production.h"
#include "convolutions.h" // for FF

class NewMeExternalInfo
{
public:
    NewMeExternalInfo(){};
public:
    string name;
    string parton_i;
    string parton_j;
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

    string parton_i()const {return info_->parton_i;}
    string parton_j()const {return info_->parton_j;}
    string name()const {return info_->name;}

    virtual void Evaluate(const double&,double*)=0;
    
    int dimension() const {return dimension_;}

protected:
    NewMeExternalInfo* info_;
    int dimension_;
    EventBox* event_box_;
    
};

class GstarGstarMeLO: public NewMatrixElement
{
public:
    GstarGstarMeLO(EventBox& event_box):NewMatrixElement(event_box)
        {
        info_ = new NewMeExternalInfo;
        dimension_ = 3;
        info_->name = "Born";
        info_->parton_i = "quark";
        info_->parton_j = "antiquark";
        info_->alpha_power = 0;
        info_->epsilon_power_min = 0;
        info_->epsilon_power_max = 2;
        }
    void Evaluate(const double& w,double* xx_vegas)
        {
        event_box_->AddNewEvent(w);
//        event_box_->SetP(1,x[0]*Etot/2.0,0.0,0.0,x[0]*Etot/2.0);
//        event_box_->SetP(2,x[1]*Etot/2.0,0.0,0.0,-x[1]*Etot/2.0);
//        event_box_->SetP(3,0.0,0.0,0.0,0.0);
//        event_box_->SetP(4,0.0,0.0,0.0,0.0);
//        event_box_->SetP(5,0.0,0.0,0.0,0.0);
//        event_box_->SetP(6,0.0,0.0,0.0,0.0);
        }
};




class GammaStarGammaStarMatrixElementBox
{
public:
    GammaStarGammaStarMatrixElementBox(EventBox&);
    int size(){return available_matrix_elements.size();}
    NewMatrixElement* give_me(int k){return available_matrix_elements[k];}
private://data
    vector<NewMatrixElement*> available_matrix_elements;
};


class NewSimpleSector
{
public:
    NewSimpleSector(const FFF& _f1,const FFF& _f2,const vector<ExpansionTerm*>& _factors,NewMatrixElement* _ME,int ep_pow);
    NewMatrixElement* ME;
    vector<ExpansionTerm*> factors;
    FFF F1,F2;
    int alpha_power,epsilon_power;
    friend ostream& operator<<(ostream&, const NewSimpleSector&);
    string name;
    
    void add_pair(int i,int j,int k,int m,pdf_pair_list & curlumi);
    void single_quark(int i,int j,int k,int m,pdf_pair_list & curlumi);
    void double_quark(int i,int j,int k,int m,pdf_pair_list & curlumi);
    int give_pid(const string & name);
    pdf_pair_list give_list_of_pdf_pairs();
    void SetLuminosity(Luminosity* lumi){lumi_ = lumi;}

    void setUpPrefactor(const double & a_s_over_pi);
    void Evaluate(double* xx_vegas);
private:
    double prefactor_;
    Luminosity* lumi_;
    double initial_state_jacobian_;
};



class GammaStarGammaStarSectorBox{
public:
    GammaStarGammaStarSectorBox(EventBox& event_box,const double& log);
    vector<string> give_sector_names(const string & pleft,const string & pright,const string & myorder,const int & requested_epsilon_power, const string& me_approx);
    vector<NewSimpleSector*> give_necessary_sectors(const UserInterface & UI);
    int size(){return available_sectors.size();}
    NewSimpleSector* give(int i){return available_sectors[i];}
private://data
    GammaStarGammaStarMatrixElementBox* available_matrix_elements;
    vector<NewSimpleSector*> available_sectors;
    vector<string> _av_partons;
    double log_mur_sq_over_muf_sq_;
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
                                                           const vector<NewMatrixElement*> & matching_mes);
    vector<FFF> give_possible_F(const string & parton,int f1order);
};





class GammaStarGammaStar : public Production
{
public:
    GammaStarGammaStar(const UserInterface & UI);
    ~GammaStarGammaStar();
    
    //: functions
    void SetDecayParticleIdInEventBox(){event_box.SetDecayParticleId(3);}
    int number_of_necessary_sectors(){return number_of_necessary_sectors_;}
    
    
    
    vector<string> give_sector_names(const string & pleft,
                                     const string & pright,
                                     const string & myorder,
                                     const int & ep_power,
                                     const string & me_approx)
    {return all_sectors->give_sector_names(pleft, pright, myorder, ep_power,me_approx);}
    
    
    void evaluate_sector();
    string sector_name(){return my_sector_name;}
    void SetNumberOfParticles() {event_box.SetNumberOfParticles(6);}
    
    //: gluon fusion specific construction:  to be removed!
    void book_production_event(const double &,const double &,
                                       const double &,const double &,
                                       const double &,const double &,
                                       const double &,const double &,
                               const double &){};//: public to integrate with fortran Fjet
    
    
    
private:
    GammaStarGammaStarSectorBox* all_sectors;
    void find_topology(const UserInterface & UI);
    void allocate_luminosity();
    NewSimpleSector* the_sector;
    int number_of_necessary_sectors_;

};

#endif