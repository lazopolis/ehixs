/**
 *
 * \file    luminosity.h
 * \ingroup tools
 * \author  Achilleas Lazopoulos
 * \date    February 2015
 *
 */

#ifndef LUMINOSITY_H
#define LUMINOSITY_H

#include "LHAPDF/LHAPDF.h"
#include <string>
using namespace std;

/**
 *
 * \class   Luminosity
 * \ingroup tools
 * \brief   Interface with LHAPDFs. It initializes a PDF member (one of the many of a specific PDF grid). Then the user can add luminosity pairs (initial states) with a potential constant factor that depends on the pair. Once configured, the class provides L(x1,x2,muf) = Sum_i {x1*f1_i(x1,muf) * x2*f2_i(x2,muf) * c_i}
 * \todo
 *
 */

class Luminosity{

private:

    /// \name Private data members
    /// @{

    LHAPDF::PDF* _pdf;                  ///< pointer to LHAPDF member
    vector<pair<int,int> > _pairs;      ///< initial states
    vector<double> _coeff;              ///< coefficients for initial state pairs
    static const double _almost_zero;   ///< technical cutoff for Bjorken xs

    /// @}

public:

    /// \name Constructors and destructors
    /// @{

    Luminosity(const string& gridname);
    
    virtual ~Luminosity()
    {
        delete _pdf;
    }
    
    ///@}
    
    /// \name Input/Output
    /// @{
    
    /// \brief adds a pair of initial states by number id
    void addPair(int left, int right, const double& c = 1.);

    void clear_pairs()
    {
        _pairs.clear();
        _coeff.clear();
        return;
    }

    /// \brief returns the full luminosity Sum_i {x1*f1_i(x1,muf) * x2*f2_i(x2,muf) * c_i}
    double give(const double& x1, const double& x2, const double& muf);

    /// \brief alpha_s_at_mz
    /// \warning mz is at its nominal pdg value here - might not be in sync with model
    double alpha_s_at_mz(){return _pdf->alphasQ(91.1876);}

    ///@}
};

#endif
