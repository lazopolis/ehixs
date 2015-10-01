/**
 *
 * \file    timekeeper.h
 * \ingroup tools
 * \author  Achilleas Lazopoulos
 *
 */

#ifndef TIMEKEEPER_H
#define TIMEKEEPER_H

#include <time.h>

class TimeKeeper
{

private:

    clock_t _start;
    
public:

    TimeKeeper()
    : _start(clock())
    {}

    void reset()
    {
        _start = clock();
    }

    float give()
    {
        return float(clock()-_start)/CLOCKS_PER_SEC;
    }

};

#endif
