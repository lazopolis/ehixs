/**
 *
 * \file    event.h
 * \ingroup tools
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \date    September 2014
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
    Event(const double& weight, const Momenta& momenta) :
    _p(momenta), _weight(weight)
    {}

    /// Move constructor from data
    Event(const double& weight, Momenta&& momenta) :
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

    const double& weight = _weight; /// < Weight of the event
    const Momenta& p = _p;          /// < Set of four-momenta

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

    double _weight; /// < Weight of the event
    Momenta _p;     /// < Set of four-momenta

    /// @}

    /// \name Forbidden functions
    /// @{

    /// Copy constructor
    Event(const Event& that);

    /// Assignment operator
    Event& operator=(const Event& that);

    /// @}

};

/**
 *
 * \typedef EventBox
 * \brief   A vector of events is named EventBox for simpler semantics
 *
 */

typedef vector<Event> EventBox;

/**
 *
 * \class CombinedEvent
 * \brief Container for combined production+decay event
 * \note  Switch to references instead of pointers?
 * \todo  Extend decay to be a vector of decays, so that more than one particle can decay
 *
 */

class CombinedEvent
{
public:

    /// \name Constructors and Destructor
    /// @{

    /// Constructor with data
    CombinedEvent(Event* prod, Event* dec) :
    production(prod),decay(dec)
    {}

    /// @}

    /// \name Data members
    /// @{

    Event* production; /// < Pointer to the production event
    Event* decay;      /// < Pointer to the decay event

    /// @}

    /// \name Input/output functions
    /// @{

    /// Returns the weight of the combined event
    double weight() const;

    /// Prints information about the event
    friend ostream& operator<<(ostream&, const CombinedEvent&);

    /// @}

};

#endif
