/**
 *
 * \file   cut_test.cpp
 * \author Simone Lionetti
 * \date   April 2015
 * \brief  Tests for cuts
 *
 */

#include "cutbox.h"
#include "defaultcuts.h"
#include <iostream>

/*TEST(ExpansionBasicTest,expansion_test)
{

}*/

int main(int argc, char**argv)
{

    cout << "Entering main" << endl;
    CutBox mycutbox;
    cout << "Cutbox created" << endl;
    NameAndArgs foo = NameAndArgs::split("[pT3cut,10.]");
    std::cout << foo.name << std::endl;
    for (size_t i = 0; i < foo.args.size(); ++i)
        std::cout << foo.args[i] << std::endl;
    mycutbox.add(foo);
    cout << "Cut added" << endl;
    const double th = 0.001;
    Momenta ps;
    ps.push_back({100.,0.,0.,+100.});
    ps.push_back({100.,0.,0.,-100.});
    ps.push_back({100.,100.*sin(th),0,100.*cos(th)});
    ps.push_back({100.,-100.*sin(th),0,-100.*cos(th)});
    Event myevent(1.,ps);
    std::cout << (mycutbox.isCut(myevent) ? "true" : "false") << std::endl;
    cout << "Cut applied" << endl;
    return 0;

}
