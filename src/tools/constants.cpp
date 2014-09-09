/** 
 * \file  constants.cpp
 * \brief This file contains constant definitions and shorthands
 */

#include "constants.h"

QCD::Flavor antiParticle(const QCD::Flavor& p){
    return QCD::Flavor(-p);
}

std::ostream& operator<<(std::ostream& myStream, const QCD::Flavor& flavor)
{
    switch (flavor) {
        case QCD::d : myStream<<"d"; break;
        case QCD::u : myStream<<"u"; break;
        case QCD::s : myStream<<"s"; break;
        case QCD::c : myStream<<"c"; break;
        case QCD::b : myStream<<"b"; break;
        case QCD::t : myStream<<"t"; break;
        case QCD::g : myStream<<"g"; break;
        case QCD::dbar : myStream<<"dbar"; break;
        case QCD::ubar : myStream<<"ubar"; break;
        case QCD::sbar : myStream<<"sbar"; break;
        case QCD::cbar : myStream<<"cbar"; break;
        case QCD::bbar : myStream<<"bbar"; break;
        case QCD::tbar : myStream<<"tbar"; break;
    }
    return myStream;
}

#endif // CONSTANTS_H
