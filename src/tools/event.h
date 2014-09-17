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
#include "fourvector.h"
using namespace std;

/**
 *
 * \class Event
 * \brief Container for weight and essential kinematics of a generated event
 * \todo  Change vector<FourMomenta> to Momenta??
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

    /// Move constructor
    Event(Event&& that) noexcept :
    _p(std::move(that._p)), _weight(std::move(that._weight))
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

    /// @}

private:

    /// \name Data members
    /// @{

    double _weight;         /// < Weight of the event
    vector<FourVector> _p;  /// < Set of four-momenta

    /// @}

    /// \name Forbidden functions
    /// @{

    Event(const Event& that);
    Event& operator=(const Event& that);

    /// @}

};

typedef vector<Event> EventBox;

/**
 *
 * \class CombinedEvent
 * \brief Container for combined production+decay event
 *
 */

class CombinedEvent
{
public:

    CombinedEvent(Event* prod, Event* dec) :
    production(prod),decay(dec)
    {}

    Event* production;
    Event* decay;
    double weight() const ;
    friend ostream& operator<<(ostream&, const CombinedEvent&);
};

#endif