#ifndef CROSS_SECTION_H
#define CROSS_SECTION_H

#include "convolutions.h"
#include "model.h"
#include "fourvector.h"
using namespace std;


class CrossSection
{
public:

    virtual ~CrossSection()
    {
        delete _lumi;
    }

    virtual void Evaluate(double*)=0;
    virtual void Configure()=0;
    virtual NewLuminosity* AllocateLuminosity(const UserInterface&)=0;
    virtual size_t dimension() const = 0;
    void initialize(const UserInterface&);

    void SetEventBox(EventBox& event_box);

    friend ostream& operator<<(ostream& stream, const CrossSection&);

    const CModel& model = _model;

protected:

    /// \name Data Members
    /// @{

    NewMeExternalInfo _info;
    CModel _model;
    EventBox* event_box_;
    NewLuminosity* _lumi;    ///< Pointer to the luminosity object

    double _smax;
    double _as_pi;
    double _muR;
    double _muF;

    /// @}

    /// \name Measurement function
    /// @{

    // Contructs the event with weight w and kinematic variables kv
    void JF(const double& w, const vector<FourVector>& kv);
    // Fills the event-box with an event with weight 0
    void JF();

    /// @}

};



#endif




