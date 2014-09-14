/**
 *
 * \file   event.h
 * \author Achilleas, Simone
 * \date   September 2014
 * Created by Achilleas
 * Revised by Simone in Sep 2014
 *
 */

#ifndef EVENT_H
#define EVENT_H

#include <ostream>
#include "FourVector.h"
using namespace std;

/**
 *
 * \class Event
 * \brief Container for weight and essential kinematics of a generated event
 * \todo  Complete tests
 *
 */

class Event
{
public:

    /// \name Constructors
    /// @{

    /// Default constructor
    Event() :
    _p(), _weight(0.)
    {}

    /// Constructor with data
    Event(const double& weight, const vector<FourVector>& momenta) :
    _p(momenta), _weight(weight)
    {}

    /// Copy constructor
    Event(const Event& that) :
    _p(that._p), _weight(that._weight)
    {}

    /// Destructor
    ~Event()
    {}

    /// @}

    /// \name Read-only data
    /// @{

    const double& weight = _weight;     /// < Weight of the event
    const vector<FourVector>& p = _p;   /// < Set of four-momenta

    /// @}

    /// \name Input/output functions
    /// @{

    /// Print event information
    friend ostream& operator<<(ostream& stream, const Event& event)
    {
        stream << "----\nEvent with weight " << event.weight << "\nFourMomenta:\n";
        for (size_t i = 0; i<event.p.size(); ++i)
            stream << "\tp" << i+1 << " = " << event.p[i] << endl;
        return stream << "----\n";
    }

    /// Assignment operator
    Event& operator=(const Event& that)
    {
        _weight = that._weight;
        _p = that._p;
        return *this;
    }

    /// @}

private:

    /// \name Data members
    /// @{

    double _weight;         /// < Weight of the event
    vector<FourVector> _p;  /// < Set of four-momenta

    /// @}

};

/**
 *
 * \class EventBox
 * \brief Vector of events optimized for minimal reallocation of memory
 * \todo  Assess speed loss with typedef vector<Event> EventBox
 *
 */

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

/**
 *
 * \class CombinedEvent
 * \brief Container for combined production+decay event
 *
 */

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

#endif