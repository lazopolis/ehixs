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
        cuts.push_back(factory->second->create(cut.args));
    else cerr << "[CutBox] Warning: cut " << cut.name << "not found." << endl;
    return;
}

//void CutBox::add(const std::vector<NameAndArgs>& opts)
//{
//    for (std::vector<NameAndArgs>::const_iterator it = opts.cbegin(); it!=opts.cend(); ++it)
//        add(*it);
//    return;
//}
//
bool CutBox::isCut(const Event& event) const
{
    for (vector<Cut*>::const_iterator it = cuts.cbegin(); it!=cuts.cend(); ++it)
        if ((*it)->isCut(event)) return true;
    return false;
}

template<>
void Option<CutBox>::set(const string& input)
{
    value.add(NameAndArgs::split(input));
    return;
}

template<>
string Option<CutBox>::print() const
{
    string foo = "";
    for (vector<Cut*>::const_iterator it = value.cuts.cbegin(); it!=value.cuts.cend(); ++it)
        foo = "   "+foo+(*it)->print()+"\n";
    return foo;
}

//void CutBox::show_cut_info_and_exit()
//{
//    cout<<"\n**************************************\nAvailable cuts:";
//    for (unsigned i=0;i<availablecuts_.size();i++)
//        cout<<"\n"<<availablecuts_[i]->info();
//    cout<<"\n**************************************\nRequested cuts:";
//    for (unsigned i=0;i<cuts_.size();i++)
//        cout<<"\n"<<cuts_[i]->info();
//    cout<<"\n**************************************\n";
//    exit(0);
//}
