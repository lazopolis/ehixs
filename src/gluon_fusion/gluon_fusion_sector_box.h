#ifndef GLUON_FUSION_SECTOR_BOX
#define GLUON_FUSION_SECTOR_BOX

#include "convolutions.h"
#include "simple_sector.h"

class GluonFusionSectorBox
{
public:
    GluonFusionSectorBox(const WilsonCoefficients&, 
         const BetaConstants&,
         const double& log_mur_sq_over_muf_sq,
         const string& rr_treatment);
    vector<string> give_sector_names(const string & pleft,
         const string & pright,
         const string & myorder,
         const int & requested_epsilon_power, 
         const string& me_approx);
    vector<SimpleSector*> give_necessary_sectors(
                    const UserInterface & UI);
    int size(){return available_sectors.size();}
    SimpleSector* give(int i)
        {return available_sectors[i];}
private://data
    GluonFusionMatrixElementBox* available_matrix_elements;
    vector<SimpleSector*> available_sectors;
    vector<SimpleSector*> necessary_sectors;
    
    vector<string> _av_partons;
    WilsonCoefficients _WC;
    BetaConstants _beta;
    double _log_mur_sq_over_muf_sq;
private://methods
    void build_sectors(const string& p_left,
                       const string & p_right);
    void build_sectors_with_fixed_a_order(int,int,
        int,const string& p_left,
        const string & p_right);
    void build_sectors_with_fixed_a_order_and_pdfs(
        const FFF & F1,const FFF & F2,int Sorder);
    void build_sectors_with_fixed_a_order_e_order_and_pdfs(
        const FFF & F1,const FFF & F2,
        int Sorder,int Eorder,
        const vector<MatrixElement*> & matching_mes);
    vector<FFF> give_possible_F(const string & parton,
                                int f1order);
                                
    void build_ew_sectors();

    void add_simple_sector(const FFF& f1,const FFF& f2, const string& matr_elem_name);
    MatrixElement* find_matrix_element(const string& the_name);
    
    void select_ew_sector(SimpleSector* the_sector, const UserInterface& UI);
    void select_qcd_sector(SimpleSector* the_sector, const UserInterface& UI);
    
};

#endif

