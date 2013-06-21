#ifndef BOTTOM_FUSION_NLO_ME_H
#define BOTTOM_FUSION_NLO_ME_H 
#include <math.h>
using namespace std;

class BottomFusion_NLO_ME
{
    public: 
            BottomFusion_NLO_ME(){pi_square = pow(3.1415926535897932385,2.0);}
            ~BottomFusion_NLO_ME(){};
            double  bb_soft_pole(const double & delta,const double &  plus);
            double  bb_soft_finite(const double &  delta,const double &  plus0,const double &  plus1);
            double  bb_soft_e(const double &  delta,const double &  plus0,const double &  plus1,const double &  plus2);
    
            double  bb_coll_pole();
            double  bb_coll_fin();
            double  bb_coll_e();
            double  bb_coll_soft_pole();
            double  bb_coll_soft_fin();
            double  bb_coll_soft_e();
            double  bb_hard_fin();
            double  bb_hard_e();
            
            double  bg_coll_pole();
            double  bg_coll_fin();
            double  bg_coll_e();
            double  bg_hard_fin();
            double  bg_hard_e();
            
            double  gb_coll_pole();
            double  gb_coll_fin();
            double  gb_coll_e();
            double  gb_hard_fin();
            double  gb_hard_e();

            void set(const double z,const double lambda,const double lh);
    private:
            double z;
            double lambda;
            double lh;
            double pi_square;
            
};
#endif