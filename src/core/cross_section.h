#ifndef CROSS_SECTION_H
#define CROSS_SECTION_H

#include <string>
using namespace std;

#include "convolutions.h"

class CrossSection
{
public:
    virtual void Evaluate(double*)=0;
    virtual void Configure()=0;
    virtual void AllocateLuminosity(const UserInterface&)=0;
    virtual void SetDimension()=0;
    virtual double alpha_s_at_mz_from_lhapdfs()=0;
    
    void SetEColliderSq(const double& smaximum);
    void SetAlphaStrong(const double& a_s_over_pi);
    void SetScales(const double& mur,const double& muf);
    void SetEventBox(EventBox& event_box);
    int Dimension();//{return dimension_;}
    
    
    friend ostream& operator<<(ostream& stream, const CrossSection&);
protected:
    NewMeExternalInfo info_;
    //int dimension_;
    EventBox* event_box_;
    
    double smax;
    double a_s_over_pi_;
    double mur_;
    double muf_;
    string name_;
protected:
    int dimension_;
};



#endif




