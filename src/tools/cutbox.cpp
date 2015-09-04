/**
 *
 * \file    cutbox.cpp
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#include "cutbox.h"

void CutBox::add(const NameAndArgs& cut)
{
    BaseFactory<Cut>::map::iterator factory = BaseFactory<Cut>::lookup().find(cut.name);
    if (factory != BaseFactory<Cut>::lookup().end())
        _cuts.push_back(factory->second->create(cut.args));
    else std::cerr << "[CutBox] Warning: cut " << cut.name << "not found." << std::endl;
    return;
}

void CutBox::add(const std::vector<NameAndArgs>& opts)
{
    for (std::vector<NameAndArgs>::const_iterator it = opts.cbegin(); it!=opts.cend(); ++it)
        add(*it);
    return;
}

bool CutBox::isCut(const Event& event) const
{
    for (std::vector<Cut*>::const_iterator it = _cuts.cbegin(); it!=_cuts.cend(); ++it)
        if ((*it)->isCut(event)) return true;
    return false;
}

//void CutBox::show_cut_info_and_exit()
//{
//    cout<<"\n**************************************\nAvailable cuts:";
//    for (unsigned i=0;i<available_cuts_.size();i++)
//        cout<<"\n"<<available_cuts_[i]->info();
//    cout<<"\n**************************************\nRequested cuts:";
//    for (unsigned i=0;i<cuts_.size();i++)
//        cout<<"\n"<<cuts_[i]->info();
//    cout<<"\n**************************************\n";
//    exit(0);
//}
