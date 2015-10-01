/**
 *
 * \file    production.h
 * \ingroup core
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \date    September 2014
 *
 */

#ifndef PRODUCTION_H
#define PRODUCTION_H

#include "factory.h"
#include "sector.h"
#include "option.h"
using namespace std;

class Production : protected OptionSet
{
    
    Production()
    {
        _opts().push_back(new Option<Sector*>("sector_for_production",'s',
                                              "number of the production sector to run:",
                                              Arg::Required,sector,NULL));
    };

    virtual ~Production()
    {
        for (vector<BaseFactory<Sector>*>::iterator it = sectors.begin(); it != sectors.end(); ++it)
            delete *it;
        delete sector;
        return;
    }

    Sector* sector;
    
    vector<BaseFactory<Sector>*> sectors;

};

#endif
