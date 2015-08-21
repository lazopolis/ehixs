/**
 * \file   pgenerator_test.cpp
 * \author Simone Lionetti
 * \date   August 2015
 * \brief  This file contains some tests for phase space generators
 */

//#include "gtest/gtest.h"
#include "xcodetest.h"
#include "parametrizations.h"
#include <fstream>

/*TEST(ExpansionBasicTest,expansion_test)
{

}*/

void writePSfile(Momenta& k, fstream& file, const size_t& i, const string& suff = "")
{
    string filename = "/Users/lionetti/ETH/External/MG5_aMC_v2_3_0/ddx2aa/PSpoints/point"+to_string(i)+".input"+suff;
    file.open(filename,ios_base::out);
    file.precision(16);
    if (file.is_open())
        for (Momenta::iterator it = k.begin(); it != k.end(); ++it)
        {
            for (size_t j=0; j!=4;++j)
                file << (*it)[j] << "\t";
            file << endl;
        }
    else cerr << "Bad file!" << endl;
    file.close();
    return;
}

int main(int argc, char**argv)
{
    // Setting up the generator
    Bjorken xs(1.,1.);
    Momenta ps;
    ps.resize(5);
    ps[1] = {0.5,0.,0.,+0.5};
    ps[2] = {0.5,0.,0.,-0.5};
    Zlambda3PG psGen(ps,xs);
    vector<double> ms = {0., 0.};
    psGen.setParameters(1.,ms);
    psGen.computeConstants();

    fstream myfile;
    myfile.precision(10);
    size_t i = 0;

    vector<double> randoms = {0.6330989210547898,0.2484647110590450,0.3092652883252196,0.,0.};
//    vector<double> randoms = {0.9320489174054389,0.4851921362917242,0.6764105546573512,0.,0.};
//    vector<double> randoms = {0.1558282452843024,0.0255120657464279,0.3750084685457403,0.,0.};

    //Right now do a single PS scan
//    for (double zeta = -6; zeta <= 6; zeta+=0.5)
    for (double zeta = -4.5; zeta <= 4.5; zeta+=1.5)
    {
        const double ez = exp(zeta);
        randoms[3] = ez/(ez+1./ez);
//        for (double y = -6; y <= 6; y+=0.5)
        for (double y = -5; y <= 5; y+=0.1)
        {
            const double ey = exp(y);
            randoms[4] = ey/(ey+1./ey);
            psGen.generateFSMomenta(randoms);
            writePSfile(ps, myfile, ++i);
            randoms[4] = 0.;
            psGen.generateFSMomenta(randoms);
            writePSfile(ps, myfile, i, ".c1");
            randoms[4] = 1.;
            psGen.generateFSMomenta(randoms);
            writePSfile(ps, myfile, i, ".c2");
        }
    }
return 0;

}
