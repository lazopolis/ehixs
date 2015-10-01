/**
 *
 * \file    luminosity.cpp
 * \ingroup tools
 * \author  Achilleas Lazopoulos
 * \date    February 2015
 *
 */

#include "luminosity.h"

/// \warning numerical value of _almost_zero is arbitrary here
const double Luminosity::_almost_zero = 1e-12;

Luminosity::Luminosity(const string& gridname)
{
    // gridname example: MSTW2008nnlo68cl
    _pdf = LHAPDF::mkPDF(gridname, 0);
    /// \warning what happens if the gridname is not matching the pdf names in LHAPDF?
}

double Luminosity::give(const double& x1, const double& x2, const double& muf)
{
    if (x1>1.-_almost_zero || x2>1.-_almost_zero || x1<_almost_zero || x2<_almost_zero)
        return 0.;

    double res = 0.;
    for (size_t i = 0; i < _pairs.size(); ++i)
    {
        res +=    _pdf->xfxQ(_pairs[i].first,x1,muf)
                * _pdf->xfxQ(_pairs[i].second,x2,muf)
                * _coeff[i];
    }
    return res;
}

void Luminosity::addPair(int left,int right,const double& c)
{
    _pairs.push_back(pair<int,int>(left,right));
    _coeff.push_back(c);
}
