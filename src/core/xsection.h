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

#include <map>
#include <string>
#include "convolutions.h"   // InitialStateFlavors, Event, NewLuminosity
#include "model.h"          // Model
#include "variables.h"      // IKinematicVariables
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

    string name;                ///< The name of the sector
    InitialStateFlavors isf;    ///< Initial state flavors for this sector
    int alpha_power;            ///< Power of the strong coupling
    size_t dim;                 ///< Dimension of integration for this sector

    /// @}

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    SectorInfo() :
    name(), isf(), alpha_power(), dim()
    {}

    /// Constructor with data
    SectorInfo(const string& iName, const InitialStateFlavors& iIsf, const int iAlphaPow, const size_t iDim) :
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
        return stream
        << info.name << " "
        << info.isf << ": "
        << "a^" << info.alpha_power << ", "
        << "dim=" << info.dim;
    }

    /// @}

};

/**
 *
 * \class XSection
 * \brief Base object for cross section evaluation
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
        delete _kin;
        return;
    }

    /// @}

    /// \name Input/output functions
    /// @{

    /// Evaluate this cross section from Vegas random numbers
    virtual Event evaluate(double*) const;

    /// @}

    /// \name Pure virtual functions
    /// @{

    /// Compute the matrix element of the instantiated concrete cross section
    virtual double matrixElement(const KinematicInvariants& s) const = 0;

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
    NewLuminosity* _lumi;      ///< Pointer to the luminosity
    IKinematicVariables* _kin; ///< Pointer to the interface with kinematis

    double _as_pi;             ///< Strong coupling constants over Pi
    double _muR;               ///< Renormalization scale
    double _muF;               ///< Factorization scale
    double _prefactor;         ///< Constant prefactor multiplying matrix element outside of the phase space integral

    /// @}
    
};

/**
 *
 * \struct BaseXSectionMaker
 * \brief  Base class for booking cross sections
 *
 */

struct BaseXSectionMaker
{

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    BaseXSectionMaker()
    {}

    /// Copy constructor
    BaseXSectionMaker(const BaseXSectionMaker& that)
    {}

    /// Destructor
    ~BaseXSectionMaker()
    {}

    /// @}

    /// \name Member functions
    /// @{

    /// Calls the constructor for a specific type of cross section
    virtual XSection* create(const UserInterface& UI) = 0;

    /// Returns information about the cross section
    virtual const SectorInfo& info() const = 0;

    /// @}

};

/**
 *
 * \struct XSectionMaker
 * \brief  Templatized creator object for different types of cross sections
 *
 */

template<typename XSectionType>
struct XSectionMaker : public BaseXSectionMaker
{

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    XSectionMaker() :
    BaseXSectionMaker()
    {}

    /// Copy constructor
    XSectionMaker(const XSectionMaker& that) :
    BaseXSectionMaker()
    {}

    /// Destructor
    ~XSectionMaker()
    {}

    /// @}

    /// \name Member functions
    /// @{

    /// Calls the constructor for a specific type of cross section
    virtual XSection* create(const UserInterface& UI)
    {
        XSectionType* foo = new XSectionType(UI);
        foo->info = &_info;
        return dynamic_cast<XSection*>(foo);
    };

    /// Returns information about the cross section
    virtual const SectorInfo& info() const
    {
        return _info;
    }

    /// @}

    /// \name Data members
    /// @{

    static const SectorInfo _info;  ///< Information about this specific type of cross section

    /// @}

};

#endif
