/**
 *
 * \file   parser_test.cpp
 * \author Simone Lionetti
 * \date   September 2015
 * \brief  Tests for Parser class
 *
 */

#include "parser.h"
#include "cutbox.h"
#include "defaultcuts.h"
#include <iostream>

/*TEST(ExpansionBasicTest,expansion_test)
{

}*/

int main(int argc, char**argv)
{

    Parser parser;
    CutBox cutbox;
//    parser.print_help_message();
    parser.parse(argc,argv);
    cout << parser.inputfile << endl;

    const double th = 1.;
    Momenta ps;
    ps.push_back({100.,0.,0.,+100.});
    ps.push_back({100.,0.,0.,-100.});
    ps.push_back({100.,100.*sin(th),0,100.*cos(th)});
    ps.push_back({100.,-100.*sin(th),0,-100.*cos(th)});

    Event myevent(1.,ps);
    cout << (cutbox.isCut(myevent) ? "cut" : "passed") << endl;
    return 0;

    return 0;

}
