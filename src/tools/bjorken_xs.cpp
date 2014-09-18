#include "bjorken_xs.h"

#include "math.h" /* log, exp, sqrt */


void BjorkenXs::generate(const double& tau,const double& v0, const double& v1)
{
    const double y= log(tau) *(1.0-v0);
    const double u = exp(y);
    const double rho = 1.0/2.0*log(u) -  log(u) * v1;
    x1_ = sqrt(u)*exp(rho);
    x2_ = sqrt(u)*exp(-rho);
    jacobian_ = -log(u)*u*(1.0-tau)*(-log(tau));
}


