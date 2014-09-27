/**
 *
 * \file    xsection.cpp
 * \ingroup core
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#include "xsection.h"

/// \class XSection

/// \name Constructors and destructor

/// Default constructor
XSection::XSection(const UserInterface& UI, const SectorInfo& myInfo) :
    // Initializing empty stuff
    _model(), _eventBox(NULL), _lumi(NULL), _x(),
    _as_pi(0.), _prefactor(consts::convert_GeV_to_pb), _factor(1.),
    // Getting general parameters from the user interface
    info(&myInfo), _muR(UI.mur), _muF(UI.muf)
{
    // Configuring the luminosity
    /// \warning Luminosity allocation assumes left-right symmetry right now
    _lumi = new NewLuminosity(UI);
    _lumi->add_pair(info->isf.left, info->isf.right);
    if (info->isf.left != info->isf.right)
        _lumi->add_pair(info->isf.right, info->isf.left);
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
void XSection::evaluate(const double* const randoms)
{
    _eventBox->clear();
    _factor = 1.; // generateXs may change _factor!
    generateXs(randoms);
    _factor *= _lumi->give(_x.x1,_x.x2);
    if ( _factor != 0. ) generateEvents(randoms);
    else _eventBox->push_back(Event());
    return;
}
