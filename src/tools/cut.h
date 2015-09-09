/**
 *
 * \file    cut.h
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#ifndef CUT_H
#define CUT_H

#include "event.h"

struct Cut
{

    virtual bool isCut(const Event& event) const = 0;
    virtual string print() const = 0;

};

#endif
