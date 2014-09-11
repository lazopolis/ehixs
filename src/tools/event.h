#ifndef EVENT_H
#define EVENT_H

#include <ostream>
#include "FourVector.h"
using namespace std;

class Event
{
public:

    Event() :
    _p(), _weight(0.)
    {}

    Event(const double& weight, const vector<FourVector>& momenta) :
    _p(momenta), _weight(weight)
    {}

    virtual ~Event()
    {}

    const double& weight = _weight;
    const vector<FourVector>& p = _p;

    Event& operator=(const Event& that)
    {
        _weight = that._weight;
        _p = that._p;
        return *this;
    }

private:

    double _weight;
    vector<FourVector> _p;

};


class CombinedEvent
{
public:

    CombinedEvent(Event* prod,Event* dec)
    :production(prod),decay(dec)
    {}

    Event* production;
    Event* decay;
    double weight() const ;
    friend ostream& operator<<(ostream&, const CombinedEvent&);
};

class EventBox
{
public:
    EventBox();
    size_t decayParticleId;
    void add(const double& inWeight, const vector<FourVector>& inP);
    void add()
    {
        add(0.,vector<FourVector>());
        return;
    }
    Event* operator()(const size_t i)
    {
        return &_events.at(i);
    }
    void clear()
    {
        _current = 0;
    }
    size_t size() const
    {
        return _current;
    }
private:
    vector<Event> _events;
    size_t _current;
};

#endif