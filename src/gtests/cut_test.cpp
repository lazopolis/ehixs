/**
 *
 * \file   cut_test.cpp
 * \author Simone Lionetti
 * \date   April 2015
 * \brief  Tests for my pow function
 *
 */

#include "cutbox.h"
#include <iostream>

//Factory<Cut,pT3cut> pT3cutFactory("pT3cut","this is a cut on the pT of particle 3.");
//
//extern template class BaseFactory<Cut>;

/*TEST(ExpansionBasicTest,expansion_test)
{

}*/

int main(int argc, char**argv)
{

//    std::cout << &BaseFactory<Cut>::lookup() << std::endl;

    CutBox mycutbox;
    NameAndArgs foo = NameAndArgs::split("[pT3cut,10.]");
    std::cout << foo.name << std::endl;
    for (size_t i = 0; i < foo.args.size(); ++i)
        std::cout << foo.args[i] << std::endl;
    mycutbox.add(foo);
    const double th = 0.001;
    Momenta ps;
    ps.push_back({100.,0.,0.,+100.});
    ps.push_back({100.,0.,0.,-100.});
    ps.push_back({100.,100.*sin(th),0,100.*cos(th)});
    ps.push_back({100.,-100.*sin(th),0,-100.*cos(th)});
    Event myevent(1.,ps);
    std::cout << (mycutbox.isCut(myevent) ? "true" : "false") << std::endl;
    return 0;

}
