/**
 *
 * \file    sector.cpp
 * \ingroup core
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \date    September 2015
 *
 */

#include "sector.h"

/// \class XSection

/// \name Constructors and destructor

/// Default constructor
Sector::Sector() :
    // Initializing empty stuff
    _model(), _eventBox(NULL), _lumi(NULL), _x(),
    _as_pi(0.), _prefactor(consts::convert_GeV_to_pb), _factor(1.)
{
    // Configuring the luminosity
    /// \warning Luminosity allocation assumes left-right symmetry right now
    _lumi = new Luminosity(UI.pdf_set);
    for (vector<InitialStateFlavors>::const_iterator it = info->isf.begin(); it < info->isf.end(); ++it)
    {
        _lumi->addPair(it->left, it->right, it->weight);
        if (it->left != it->right)
            _lumi->addPair(it->right, it->left, it->weight);
    }
    // Starting to run
    /// \note Model does not seem to be particularly generic...
    _model.Configure(
                     _lumi->alpha_s_at_mz(),
                     UI.mur_over_mhiggs,
                     UI.perturbative_order,
                     UI.m_higgs
                     );
    // Setting up alpha_s
    _as_pi = _model.alpha_strong()/consts::Pi;
    cout << "\n[CrossSection]: a_s = " << _as_pi * consts::Pi;
    return;
}

/// Evaluate this cross section from Vegas random numbers
void XSection::evaluate(vector<double>& randoms)
{
    _eventBox->clear();
    _factor = generateXs(randoms);
    _factor *= _lumi->give(_x.x1,_x.x2,_muF);
    if ( _factor != 0. ) generateEvents(randoms);
    else _eventBox->push_back(Event());
    return;
}
