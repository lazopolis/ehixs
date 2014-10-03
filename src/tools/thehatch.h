/**
 *
 * \file    thehatch.h
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    October 2014
 *
 */

#ifndef THE_HATCH_H
#define THE_HATCH_H

#include <vector>
#include <string>
#include <iostream>
using namespace std;

/**
 *
 * \class TheHatch
 * \brief Distributor of random variables
 *
 */

class TheHatch
{

    typedef pair<size_t,vector<double>&> RanInfo;

public:

    /// \name Constructors and destructor
    /// @{

    /// Default constructor
    TheHatch() :
    _targets()
    {}

    /// Destructor
    ~TheHatch()
    {}
    
    /// @}

    /// \name Input/output functions
    /// @{

    /// Ask for a vector of n random variables to be set up
    void request(vector<double>& v, const size_t n)
    {
        _targets.push_back(RanInfo(n,v));
        return;
    }

    /// Return the total number of random variables
    size_t n() const
    {
        size_t foo = 0;
        for (vector<RanInfo>::const_iterator it = _targets.begin(); it != _targets.end(); ++it)
            foo += it->first;
        return foo;
    }

    /// Set the variables in vectors _targets from a C array provided by Vegas
    void distribute(const double* vars) const
    {
        for (vector<RanInfo>::const_iterator it = _targets.begin(); it != _targets.end(); ++it)
        {
            it->second.clear();
            for (size_t i = 0; i < it->first; ++i)
                it->second.push_back(*(vars++));
        }
        return;
    }

    /// @}

private:

    /// \name Data members
    /// @{

    vector<RanInfo> _targets; ///< Array of random variable arrays

    /// @}

};

#endif
