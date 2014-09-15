#ifndef CROSS_SECTION_H
#define CROSS_SECTION_H

#include <map>
#include <string>
#include "convolutions.h"
//#include "luminosity.h"
//#include "event.h"
//#include "production.h"
#include "model.h"
#include "fourvector.h"
using namespace std;

struct BaseXSectionMaker;
template<typename XSectionType>
class XSectionMaker;

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

    string name;                /// < The name of the sector
    InitialStateFlavors isf;    /// < Initial state flavors for this sector
    int alpha_power;            /// < Power of the strong coupling
    size_t dim;                 /// < Dimension of integration for this sector

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

    virtual ~XSection()
    {
        delete _lumi;
    }

    virtual void Evaluate(double*)=0;
    virtual void Configure()=0;
    virtual NewLuminosity* AllocateLuminosity(const UserInterface&)=0;
    void initialize(const UserInterface&);

    void SetEventBox(EventBox& event_box);

    //friend ostream& operator<<(ostream& stream, const XSection&);

    const CModel& model = _model;
    const SectorInfo* info;

protected:

    /// \name Data Members
    /// @{

    CModel _model;
    EventBox* event_box_;
    NewLuminosity* _lumi;    ///< Pointer to the luminosity object

    double _smax;
    double _as_pi;
    double _muR;
    double _muF;

    /// @}
    
};

/**
 *
 * \struct BaseXSectionMaker
 * \brief  Base class for sector booking
 *
 */

struct BaseXSectionMaker
{

    /// \name Constructors and destructor
    /// @{

    BaseXSectionMaker()
    {}

    BaseXSectionMaker(const BaseXSectionMaker& that)
    {}

    ~BaseXSectionMaker()
    {}

    /// @}

    /// \name Member functions
    /// @{

    virtual XSection* create() = 0;
    virtual const SectorInfo& info() const = 0;

    /// @}

};

template<typename XSectionType>
class XSectionMaker : public BaseXSectionMaker
{
    static bool _isAlive;

public:

    static const SectorInfo _info;
    XSectionMaker() :
    BaseXSectionMaker()
    {
        if (_isAlive) cerr << "[Sector] : Creating multiple cross section makers. You sure?" << endl;
        else _isAlive = true;
        return;
    }

    ~XSectionMaker()
    {
        _isAlive = false;
        return;
    }

    virtual XSection* create()
    {
        XSectionType* foo = new XSectionType;
        foo->info = &_info;
        return foo;
    };

    virtual const SectorInfo& info() const
    {
        return _info;
    }

};

template<typename XSectionType>
bool XSectionMaker<XSectionType>::_isAlive = false;

#endif
