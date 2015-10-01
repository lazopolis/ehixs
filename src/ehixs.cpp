/**
 *
 * \file    ehixs.cpp
 * \author  Achilleas Lazopoulos
 * \author  Simone Lionetti
 * \brief   This file contains the main program
 *
 */

#include "timekeeper.h"
#include "parser.h"
#include "process.h"
#include "ehixs_config.h"
#include <string>
#include <iostream>
using namespace std;

void logo()
{
    cout << "[ehixs]" << endl;
    cout << "[ehixs]" << endl;
    cout << "[ehixs]" << endl;
    cout << "[ehixs]         ehixs" << endl;
#ifdef EHIXS_VERSION_MAJOR
    cout << "[ehixs]" << endl;
    cout << "[ehixs]         version " << EHIXS_VERSION_MAJOR;
#ifdef EHIXS_VERSION_MINOR
    cout << "." << EHIXS_VERSION_MINOR;
#endif
    cout << endl;
#endif
    cout << "[ehixs]" << endl;
    cout << "[ehixs]" << endl;
    cout << "[ehixs]" << endl;
}

int main(int argc, char** argv)
{

    TimeKeeper myclock;

    logo();
    Parser parser;

    try
    {
        Process* process = new Process();
        parser.parse(argc,argv);
        process->perform();
    }
    catch(const char* s)
    {
        cerr << "[ehixs] exception thrown: " << s << endl;
    }
    catch(const string& s)
    {
        cerr << "[ehixs] exception thrown :" << s << endl;
    }
    catch(...)
    {
        cerr << "[ehixs] exception thrown." << endl;
    }
    cout << "[ehixs]" << endl;
    cout << "[ehixs] Total running time  = " << myclock.give() << " s." << endl;
    cout << "[ehixs]" << endl;
    return 0;

}
