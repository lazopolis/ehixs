/**
 *
 * \file    xsection.h
 * \ingroup core
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#ifndef XSECTION_H
#define XSECTION_H

#include "convolutions.h" // InitialStateFlavors, Event
#include "model.h"        // Model
#include "bjorken.h"      // Bjorken
#include "option.h"       // Option
#include <string>
using namespace std;

/**
 *
 * \struct SectorInfo
 * \brief  Container for information about sectors
 *
 */

struct SectorInfo
{

    /// \name Data Members
    /// @{

    string name;                        ///< The name of the sector
    vector<InitialStateFlavors> isf;    ///< Initial state flavors for this sector
    int alpha_power;                    ///< Power of the strong coupling
    size_t dim;                         ///< Dimension of integration for this sector

    /// @}

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    SectorInfo() :
    name(), isf(), alpha_power(), dim()
    {}

    /// Constructor with data
    SectorInfo(const string& iName, const InitialStateFlavors& iIsf, const int iAlphaPow, const size_t iDim) :
    name(iName), isf({iIsf}), alpha_power(iAlphaPow), dim(iDim)
    {}

    /// Constructor with data
    SectorInfo(const string& iName, const vector<InitialStateFlavors>& iIsf, const int iAlphaPow, const size_t iDim) :
    name(iName), isf(iIsf), alpha_power(iAlphaPow), dim(iDim)
    {}

    /// Copy constructor
    SectorInfo(const SectorInfo& that) :
    name(that.name), isf(that.isf), alpha_power(that.alpha_power), dim(that.dim)
    {}

    /// Destructor
    ~SectorInfo()
    {}

    /// @}

    /// \name Output functions
    /// @{

    /// Print out the information about a sector
    friend ostream& operator<<(ostream& stream, const SectorInfo& info)
    {
        stream << info.name << " ";
        for (vector<InitialStateFlavors>::const_iterator it = info.isf.begin(); it < info.isf.end(); ++it)
        {
            stream << *it;
            if (it!=info.isf.end()-1) stream << "&";
        }
        return stream
            << ": a^" << info.alpha_power << ", "
            << "dim=" << info.dim;
    }

    /// @}

};

/**
 *
 * \class XSection
 * \brief Base object for cross section evaluation
 * \todo  Meditate on membership of EventBox
 *
 */

class XSection
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    XSection(const UserInterface& UI, const SectorInfo& info);

    /// Destructor
    virtual ~XSection()
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
    const SectorInfo* info;       ///< Pointer to the information about the specific, concrete cross section

    /// @}

protected:

    /// \name Data Members
    /// @{

    CModel _model;             ///< Model for the running of this cross section
    EventBox* _eventBox;       ///< Pointer to the box where generated events are stored
    Luminosity* _lumi;      ///< Pointer to the luminosity
    Bjorken _x;                ///< Storage box for Bjorken xs

    double _as_pi;             ///< Strong coupling constants over Pi
    double _muR;               ///< Renormalization scale
    double _muF;               ///< Factorization scale
    double _prefactor;         ///< Constant prefactor outside of the phase space integral
    double _factor;            ///< Factor inside the phase space integral (luminosity, jacobian...)

    /// @}
    
};

#endif
