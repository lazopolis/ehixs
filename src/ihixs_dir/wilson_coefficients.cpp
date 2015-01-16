#include "wilson_coefficients.h"
#include "constants.h"

// the wilson coeff depends on the scheme used for the top mass
// we have implemented below the wilson coefficients using the decoupling coeff.
// of hep-ph/0512058 (Steinhauser and Schroder)
// Note that one has to express their WC's in terms of the 5 flavor a_s
//
// Also note that there is a factor -1/3 different in the overall normalization
// which is here absorbed in the overall constant (as 1/9)

void WilsonCoefficient::Configure(const double& log_muf_over_mt_sq, const string& scheme)
{
    cout<<"\nConfiguring Wilson Coefficients: scheme = "<<scheme<<endl;
    const double L = log_muf_over_mt_sq;
    const double nf=consts::nf;
    //const double z2=consts::z2;
    const double z3=consts::z3;
    if (scheme=="msbar")
    {
        _c0 = 1.;
        _c1 = 11./4.;
        _c2 = 2777./288. - consts::nf * 67./96.+ L * (19./16.+consts::nf/3.);
        _c3 = -2892659.0/41472.
                    +(897943./9216.)*consts::z3
                    +(1733./288.)*L
                    +(209./64.)* pow(L,2.)
                    +consts::nf*(
                                 (55./54.)*L
                                 +40291./20736.
                                 -(110779./13824.)*consts::z3
                                 +(23./32.)*pow(L,2.)
                                 )
                    +pow(consts::nf,2.)*(
                                         -6865./31104.
                                         +(77./1728.)*L
                                         -(1./18.)* pow(L,2)
                                         );
    }
    else if (scheme=="on-shell")
    {
        _c0 = 1.;
        _c1 = 11./4.;
        _c2 = 2777./288. - consts::nf * 67./96.+ L * (19./16.+consts::nf/3.);
        _c3 = (-16567986
               + 2088288*L
               + 812592*pow(L,2)
               + 704676*nf
               + 419328*L*nf
               + 178848*pow(L,2)*nf
               - 54920*pow(nf,2)
               + 11088*L*pow(nf,2)
               - 13824*pow(L,2)*pow(nf,2)
               + 24244461*z3
               - 1994022*nf*z3)/248832.;
    }
    else
    {
        cout<<"\n[ihixs] Error: in WilsonCoeefficient.Configure the scheme \'"
            <<scheme<<"\' was not recognized."
            <<"Allowed schemes are msbar and on-shell."
            <<"This info comes from Particle within Model and there seems to be some incompatibility."
        <<endl<<"The error is fatal"<<endl;
        exit(0);
    }
    _c = AsSeries(1,_c0,_c1,_c2,_c3);
	cout<<"\nC = "<< _c<<endl;
	cout<<"\nC^2 = "<< _c*_c <<endl;
}

//MSbar:
//1 + (11*a5)/4. + (pow(a5,2)*(2777 - 201*nf + 6*L*(57 + 16*nf)))/288. \
+ (pow(a5,3)*(-17355954 + 483492*nf - 54920*pow(nf,2) - \
432*pow(L,2)*(-1881 - 414*nf + 32*pow(nf,2)) + 144*L*(10398 + 1760*nf \
+ 77*pow(nf,2)) + 24244461*z3 - 1994022*nf*z3))/248832


//OS from eq.4.39 of hep-ph/0201415 (Steinhauser)
// after going from a6 to a5 (see Decoupling_and_WC for more info and an electronic format)
//1 + (11*a5)/4. + (pow(a5,2)*(2777 + 342*L - 201*nf + 96*L*nf))/288. + (pow(a5,3)*(-16567986 + 2088288*L + 812592*pow(L,2) + 704676*nf + 419328*L*nf + 178848*pow(L,2)*nf - 54920*pow(nf,2) + 11088*L*pow(nf,2) - 13824*pow(L,2)*pow(nf,2) + 24244461*z3 - 1994022*nf*z3))/248832.



// OS:
//1 + (19*a5)/4. + (pow(a5,2)*(8689 + 1926*L - 409*nf))/288. + (pow(a5,3)*(-138672*pow(L,2)*(-33 + 2*nf) - 144*L*(-210091 + 15776*nf) - 8*pow(nf,2)*(-1679 + 6912*z2) + 3*(13554586 + 7800399*z3 + 18432*z2*(84 + 31*Log(2))) - 6*nf*(746906 + 396849*z3 + 4608*z2*(43 + Log(16)))))/248832.
