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

#include "factory.h"
#include "event.h"
#include "option.h"

struct Cut
{

    virtual bool isCut(const Event& event) const = 0;

};

struct pT3cut : public Cut
{

    double pTmin;

    pT3cut(const vector<string>& specs)
    {
        NameAndArgs::checksize("pT3cut",specs,1);
        pTmin = stod(specs[0]);
    }

    bool isCut(const Event& event) const
    {
        return event.p[3].T()<pTmin;
    }

};

static Factory<Cut,pT3cut> pT3cutFactory("pT3cut","this is a cut on the pT of particle 3.");

#endif
