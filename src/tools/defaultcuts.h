/**
 *
 * \file    defaultcuts.h
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    September 2015
 *
 */

#ifndef DEFAULT_CUTS_H
#define DEFAULT_CUTS_H

#include "factory.h"
#include "cut.h"

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

    string print() const
    {
        return "Cut: pT3 > "+to_string(pTmin);
    }

};

static Factory<Cut,pT3cut> pT3cutFactory("pT3cut","this is a cut on the pT of particle 3.");

#endif
