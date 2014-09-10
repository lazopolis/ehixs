#ifndef CROSS_SECTION_H
#define CROSS_SECTION_H

#include "convolutions.h"
#include "model.h"
using namespace std;


class CrossSection
{
public:
    virtual void Evaluate(double*)=0;
    virtual void Configure()=0;
    virtual void AllocateLuminosity(const UserInterface&)=0;
    virtual size_t dimension() const = 0;
    void startRunning(const UserInterface&);

    void SetEColliderSq(const double& smaximum);
    void SetScales(const double& mur,const double& muf);
    void SetEventBox(EventBox& event_box);

    friend ostream& operator<<(ostream& stream, const CrossSection&);

    const CModel& model = _model;

protected:

    NewMeExternalInfo _info;
    CModel _model;
    EventBox* event_box_;
    NewLuminosity* _lumi;    ///< Pointer to the luminosity object

    double _smax;
    double _as_pi;
    double _muR;
    double _muF;
};



#endif




