#include "event.h"
#include <iostream>
using namespace std;



double CombinedEvent::weight() const
{
    double w1=1.0;
    double w2=1.0;
    if (production!=NULL) w1 = production->weight;
    if (decay!=NULL) w2 = decay->weight;
    // Why pass weight 1 when both events have weight 0?
    return w1*w2;
}

EventBox::EventBox()
{
    decayParticleId=0;
    _current=0;
    return;
}

void EventBox::add(const double& inWeight, const vector<FourVector>& inP)
{
    if ( _current++ != _events.size() ) _events[_current-1] = Event(inWeight,inP);
    else _events.push_back(Event(inWeight,inP));
    return;
}

ostream& operator<<(ostream& the_stream, const CombinedEvent& E)
{
    the_stream<<'\n'<<E.weight();
    if (E.production!=NULL)
        for (vector<FourVector>::const_iterator it = E.production->p.begin(); it < E.production->p.end(); ++it)
            the_stream << "\t" << *it;
    return the_stream;
}








