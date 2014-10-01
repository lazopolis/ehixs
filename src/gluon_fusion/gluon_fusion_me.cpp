#include "gluon_fusion_me.h"
#include<complex>


#include "chaplin.h"
complex<double> born_single_quark(complex<double> x)
{
    
    //: the expression below goes to 1 as mq->infty, i.e. as x->1
    complex<double > res=(-3.0)*x/pow(1.0-x,2.0)*
    (2.0-pow(1.0+x,2.0)/pow(1.0-x,2.0)*HPL2(0,0,x));
    return res;
}

// GluonFusion_gg_Delta

GluonFusion_gg_Delta::GluonFusion_gg_Delta(const UserInterface& UI, const SectorInfo& info) :
XSection(UI, info)
{
    //: 35.0309 = Gf*pi/sqrt(2)/288 with the Gf in pb
    //: Gf = 1.16637*10^{-5} * 0.389379*10^9
    _prefactor = 35.0309;
//    _prefactor *= consts::Pi * pow(yukawa_bottom,2.) / (2. * QCD::Nc * pow(UI.m_higgs,2.));
    _kin = new KinematicVariables<OneXGenerator,DeltaPG>(UI.Etot*UI.Etot,{UI.m_higgs},info.dim);
    return;
}

double GluonFusion_gg_LO::matrixElement(const KinematicInvariants& invariants) const
{
    std::complex<double> LO_top_only_exact_coeff(0.0,0.0);
    for (int i=0;i<_model.quarks.size();i++)
    {
        //defeinition of heavy quark: m > 100 GeV
        if (_model.quarks[i]->m() > 100.0)
        {
            cout<<"\n quark contributing: "<<_model.quarks[i]->name()<<endl;
            LO_top_only_exact_coeff += _model.quarks[i]->Y() * born_single_quark(_model.quarks[i]->X());
        }
    }
    
    const double wc0 = abs(LO_top_only_exact_coeff*conj(LO_top_only_exact_coeff));
    
    return wc0 * pow(_as_pi,2.) * 2.*invariants.s(1,2) * 1./2./* for left-right symmetry in lumi */;
}

template<>
const SectorInfo XSectionMaker<GluonFusion_gg_LO>::_info(
                                                          "Born",
                                                          InitialStateFlavors(QCD::g, QCD::g),
                                                          0,
                                                          1
                                                          );





