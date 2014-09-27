/**
 *
 * \file    xsectionmaker.h
 * \ingroup core
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#ifndef XSECTIONMAKER_H
#define XSECTIONMAKER_H

#include "xsection.h"
using namespace std;

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
