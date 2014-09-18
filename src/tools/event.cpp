/**
 *
 * \file    event.cpp
 * \ingroup tools
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#include "event.h"

/// \class CombinedEvent

/// Returns the weight of the combined event
double CombinedEvent::weight() const
{
    double w1 = 1.;
    double w2 = 1.;
    if ( production != NULL ) w1 = production->weight;
    if ( decay      != NULL ) w2 = decay->weight;
    return w1 * w2;
}

/// Prints information about the event
ostream& operator<<(ostream& the_stream, const CombinedEvent& E)
{
    the_stream << '\n' << E.weight();
    if ( E.production != NULL )
        for (Momenta::const_iterator it = E.production->p.begin(); it < E.production->p.end(); ++it)
            the_stream << "\t" << *it;
    return the_stream;
}
