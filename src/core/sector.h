/**
 *
 * \file    sector.h
 * \ingroup core
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \date    September 2015
 *
 */

#ifndef SECTOR_H
#define SECTOR_H

#include "convolutions.h" // InitialStateFlavors, Event
#include "model.h"        // Model
#include "bjorken.h"      // Bjorken
#include "option.h"       // Option
#include <string>
using namespace std;

class ISector : protected OptionSet
{

    ISector()
    {
        _opts().push_back(new Option<double>("mur",0,
                                             "renormalization scale",Arg::Required,
                                             _muR,80.));
        _opts().push_back(new Option<double>("muf",0,
                                             "factorization scale",Arg::Required,
                                             _muF,80.));
    }

protected:

    double _muR;    ///< Renormalization scale
    double _muF;    ///< Factorization scale

};

/**
 *
 * \class Sector
 * \brief Base object for cross section evaluation
 * \todo  Meditate on membership of EventBox
 *
 */

class Sector : protected ISector
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    Sector();

    /// Destructor
    virtual ~Sector()
    {
        delete _lumi;
        return;
    }

    /// @}

    /// \name Input/output functions
    /// @{

    /// Set the EventBox
    void setEventBox(EventBox* const ev)
    {
        _eventBox = ev;
        return;
    }

    /// Send events from this cross section to the EventBox
    /// \note Override if both delta and non-delta kinematics in the same sector
    virtual void evaluate(vector<double>& randoms);

    /// @}

    /// \name Pure virtual functions
    /// @{

    /// Generate Bjorken Xs and return the jacobian
    virtual double generateXs(vector<double>& randoms) = 0;

    /// Compute the events of the current sector
    virtual void generateEvents(vector<double>& randoms) = 0;

    /// @}
    
    /// \name Data Members
    /// @{

    const CModel& model = _model; ///< Read-only public alias for the model

    /// @}

protected:

    /// \name Data Members
    /// @{

    CModel _model;                ///< Model for the running of this cross section
    EventBox* _eventBox;          ///< Pointer to the box where generated events are stored
    Luminosity* _lumi;            ///< Pointer to the luminosity
    Bjorken _x;                   ///< Storage box for Bjorken xs

    double _as_pi;                ///< Strong coupling constants over Pi
    double _prefactor;            ///< Constant prefactor outside of the phase space integral
    double _factor;               ///< Factor inside the phase space integral (luminosity, jacobian...)

    /// @}
    
};

#endif
