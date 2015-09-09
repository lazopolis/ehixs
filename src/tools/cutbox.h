/**
 *
 * \file    cutbox.h
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#ifndef CUTBOX_H
#define CUTBOX_H

#include "option.h"
#include "factory.h"
#include "cut.h"

class CutBox : protected OptionSet
{

public:

    CutBox(const string& name = "cut", const char abbr = 'c', const string& desc = "generic cuts")
    : cuts()
    {
        _opts().push_back(new Option<CutBox>(name,abbr,desc,Arg::Required,*this));
    }

    void add(const NameAndArgs& cut);
//    void add(const std::vector<NameAndArgs>& opts);

    bool isCut(const Event& event) const;

    vector<Cut*> cuts;

};

#endif
