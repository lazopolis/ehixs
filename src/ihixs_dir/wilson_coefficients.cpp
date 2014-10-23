#include "wilson_coefficients.h"
#include "constants.h"

void WilsonCoefficient::Configure(const double& log_muf_over_mt_sq)
{
    _w.clear();
    double c0 = 1.;
    double c1 = 11./4.;
    double c2 = 2777./288. - consts::nf * 67./96.+ log_muf_over_mt_sq * (19./16.+consts::nf/3.);
    double c3 = -2892659.0/41472.
    +(897943./9216.)*consts::z3
    +(1733./288.)*log_muf_over_mt_sq
    +(209./64.)* pow(log_muf_over_mt_sq,2.)
    +consts::nf*(
                 (55./54.)*log_muf_over_mt_sq
                 +40291./20736.
                 -(110779./13824.)*consts::z3
                 +(23./32.)*pow(log_muf_over_mt_sq,2.)
                 )
    +pow(consts::nf,2.)*(
                         -6865./31104.
                         +(77./1728.)*log_muf_over_mt_sq
                         -(1./18.)* pow(log_muf_over_mt_sq,2)
                         );
    
    _w.push_back(c0*c0);
    _w.push_back(2.*c0*c1);
    _w.push_back(c1*c1+2.*c0*c2);
    _w.push_back(2.*c1*c2+2.*c3*c0);
}
