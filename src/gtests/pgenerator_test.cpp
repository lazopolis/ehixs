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

    //Right now do a single PS scan
    for (double zeta = 1.e-5; zeta <= 5; zeta=min(zeta+0.5,zeta*exp(1.5)))
    {
        randoms[3] = 1.-exp(-2.*zeta);
        for (double y = -5; y <= 5; y+=1.)
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
