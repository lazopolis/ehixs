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
#include "cut.h"

class CutBox
{

public:

    CutBox()
    : _cuts()
    {}

    void add(const NameAndArgs& cut);
    void add(const std::vector<NameAndArgs>& opts);

    bool isCut(const Event& event) const;

    //    void show_cut_info_and_exit();

private:

    std::vector<Cut*> _cuts;

};



#endif
