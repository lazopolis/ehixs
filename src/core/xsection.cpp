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
    _model(), _eventBox(NULL), _lumi(NULL), _kin(NULL),
    _as_pi(0.), _prefactor(consts::convert_GeV_to_pb),
    // Getting general parameters from the user interface
    info(&myInfo), _muR(UI.mur), _muF(UI.muf)
{
    // Configuring the luminosity
    /// \warning Luminosity allocation assumes left-right symmetry right now
    _lumi = new NewLuminosity(UI);
    _lumi->add_pair(info->isf.left, info->isf.right);
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

/// Sets the EventBox to send events to
void XSection::setEventBox(EventBox& eventBox)
{
    _eventBox = &eventBox;
    return;
}

/// Evaluate this cross section from Vegas random numbers
void XSection::evaluate(double* xx_vegas) const
{

    _kin->generate(xx_vegas);
    const double myxlumi = _lumi->give(_kin->x1,_kin->x2);
    if (myxlumi!=0.0)
    {
        const double sigma = _prefactor * _kin->jacobian
        * myxlumi
        * 1.0/(2.0*_kin->s(1,2)) //flux
        * matrixElement(*_kin)
        ;
        _eventBox->add(sigma, _kin->p);
    }
    else
    {
        _eventBox->add();
    }

}
