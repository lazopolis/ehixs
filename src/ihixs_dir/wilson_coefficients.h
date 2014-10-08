
#ifndef WILSON_COEFFICIENTS_H
#define WILSON_COEFFICIENTS_H

#include<vector>
using namespace std;

class WilsonCoefficient{
public:
    void Configure(const double& log_muf_over_mt_sq);
    double w(int i) const {return _w[i];}
    
private:
    vector<double> _w;
};

#endif

