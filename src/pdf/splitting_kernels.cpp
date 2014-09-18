/**
 * \file   splitting_kernels.cpp
 * \author Stefu Bulli
 *
 * Implement splitting_kernels.h
 */

///\todo Refactor Li2
///\todo Avoid preprocessor definitions
///\note Why the syntax 0.1e1??!?!?!

#include <cmath>
//#include "constants.h"
#include "splitting_kernels.h"
#include <iostream>
#include <stdlib.h>     /* exit, EXIT_FAILURE */
using namespace std;
//BLAMED lines
#define PI 3.141592653589793
#define ZETA2 1.644934066848226
#define ZETA3 1.202056903159594

// ============================================================================

double Li2(const double& z)
{
    //PRE: receives a real number z in [-1,1]
    //POST: returns the value Li2(z)

    const double zeta2 = 1.644934066848226;
    double ret=0.0;
    if(z == 1.0)
        return zeta2;
    else
    {
        if(z < 0.5)
            return series_li2(z);
        else
            return -Li2(1.0-z) + zeta2 - log(z)*log(1.0 - z);
    }
}

// ============================================================================

double series_li2(const double& z)
{
    // PRE: receives a complex number z with |z| < 1 and Re(z) < 0.5
    // POST: returns the value Li2(z), which is obtained by summing up the first Nmin terms of the
    //	series expansion of Li2 in -log(1-z).
    //
    // NOTE: this is the low precision version, at worst the relative precision is 1e-8.
    //
    const int Nmin = 5;
    const double bern[5] = {1.0,0.8333333333333333e-1,-0.1388888888888889e-2,0.3306878306878307e-4, -0.8267195767195767e-6};
    //
    double retval = 0.0;
    double zb = -log(1.0 - z);
    for(int i=0; i<Nmin; i++)
    {
        retval += pow(zb,2*i+1)*bern[i]/(2*i+1.0);
    }
    retval += -zb*zb/4.0; //B_2 = -0.5, the only nonzero even Bernoulli number
    return retval;
}

// *************************************************************************
// NLO kernels
// *************************************************************************
// pgg0
// *************************************************************************



Kernel::Kernel(int iparton,int from_parton,int a_power,int e_power)
{
    if (a_power==1)//: NLO kernels
    {
        if (iparton==0)//:gluon
        {
            if (from_parton==0)//:from gluon
            {
                delta_ptr=pgg0_d1;
                plus_ptr=pgg0_DD;
                reg_ptr = pgg0_reg;
                bound_ptr=pgg0_DDb;
            }
            else           //: from quark
            {
                delta_ptr=null_kernel;
                plus_ptr=null_kernel;
                reg_ptr = pgq0_reg;
                bound_ptr=null_kernel;
            }
        }
        else           //: quark
        {
            if (from_parton==0)//: from gluon
            {
                delta_ptr=null_kernel;
                plus_ptr=null_kernel;
                reg_ptr = pqg0_reg;
                bound_ptr=null_kernel;
            }
            else                //: from other quark
            {
                if (iparton== from_parton)//: //from same quark
                {
                    delta_ptr=pqq0_d1;
                    plus_ptr=pqq0_DD;
                    reg_ptr = pqq0_reg;
                    bound_ptr=pqq0_DDb;
                }
                else
                {
                    delta_ptr=null_kernel;
                    plus_ptr=null_kernel;
                    reg_ptr = null_kernel;
                    bound_ptr=null_kernel;
                }
            }
        }
    }
    if (a_power==2 and e_power==-1)//: NNLO a^2/e kernels (the P^(1)_ij)
    {

    }
}

double null_kernel(const double&x){return 0.0;}
double no_kernel(const double&x){cout<<"\nNo kernel used: you shouldn't be doing this"<<endl;exit(EXIT_FAILURE);}
double pgg0_d1(const double& NF)
{
    return 11.0/4.0 - NF/6.0;
}



double pgg0_DD(const double& x)
{
    return 3.0;
}

double pgg0_DDb(const double& x)
{
    return 3.0*log(1.0-x);
}

double pgg0_reg(const double& x)
{
    return 3.0/x - 6.0 + 3.0*x*(1.0-x);
}

// *************************************************************************
// pqq0
// *************************************************************************

double pqq0_d1()
{
    return 1.0 ;
}

double pqq0_d1(const double&NF)
{
    return 1.0 ;
}

double pqq0_DD(const double& x)
{
    return 4.0/3.0 * 1.0 ; // <------------------------ o o o
}

double pqq0_DDb(const double& x)
{
    return 4.0/3.0*log(1.0-x)* 1.0 ; // <------------------------ o o o
}

double pqq0_reg(const double& x)
{
    return -(1.0+x)*2.0/3.0* 1.0 ; // <------------------------ o o o
}

// *************************************************************************
// pqg0
// *************************************************************************

double pqg0_reg(const double& x)
{
    return (x*x + (1.0-x)*(1.0-x))/4.0 ; // <------------------------ o o o
}

// *************************************************************************
// pgq0
// *************************************************************************

double pgq0_reg(const double& x)
{
    return 2.0*(1.0+(1.0-x)*(1.0-x))/x/3.0  ; // <------------------------ o o o
}

// *************************************************************************
// NNLO kernels
// *************************************************************************
// pgg1
// *************************************************************************

double pgg1_d1(const double& NF)
{
    return 6.0+27.0*ZETA3/4.0-2.0*NF/3.0;
}

double pgg1_DD(const double& NF,const double& x)
{
    return 67.0/4.0-5.0*NF/6.0-3.0*PI*PI/4.0;
}

double pgg1_DDb(const double& NF,const double& x)
{
    return (67.0/4.0-5.0*NF/6.0-3.0*PI*PI/4.0)*log(1.0-x);
}

double pgg1_reg(const double& NF,const double& x)
{

    double t3 = log(x);
    double t4 = 0.1e1 + x;
    double t5 = log(t4);
    double t8 = 0.1e1 - x;
    double t9 = log(t8);
    double t12 = t3 * x;
    double t14 = t3 * t3;
    double t19 = x * x;
    double t22 = 0.1e1 / x;
    double t25 = NF * t3;
    double t27 = NF * t14;
    double t29 = t3 * t19;
    double t31 = 0.1e1 / t4;
    double t32 = Li2(-x);
    double t37 = 0.3141592653589793e1 * 0.3141592653589793e1;
    double t42 = -0.25e2 / 0.8e1 - NF / 0.4e1 - 0.109e3 / 0.8e1 * x + 0.18e2 * t3 * t5 + 0.18e2 * t3 * t9 + 0.33e2 / 0.4e1 * t12 + 0.9e1 * t14 * x - NF * x / 0.4e1 + 0.109e3 / 0.36e2 * NF * t19 - 0.61e2 / 0.36e2 * NF * t22 - 0.3e1 / 0.2e1 * t25 - t27 / 0.3e1 - 0.33e2 * t29 - 0.9e1 * t31 * t32 + 0.9e1 / 0.4e1 * t31 * t14 - 0.3e1 / 0.4e1 * t31 * t37 + 0.9e1 * t22 * t32;
    double t51 = 0.1e1 / t8;
    double t72 = t22 * t3;
    double t80 = 0.9e1 * x * t32 + 0.9e1 * t19 * t32 - 0.9e1 / 0.2e1 * t19 * t14 + 0.3e1 / 0.2e1 * t19 * t37 + 0.9e1 / 0.4e1 * t51 * t14 - 0.75e2 / 0.4e1 * t3 + 0.3e1 * t37 - 0.9e1 * t51 * t3 * t9 - t27 * x / 0.3e1 - 0.9e1 * t31 * t3 * t5 - 0.9e1 * t12 * t9 - 0.13e2 / 0.6e1 * t25 * x + 0.9e1 * t12 * t5 + 0.9e1 * t29 * t5 + 0.9e1 * t72 * t5 - 0.9e1 * t72 * t9 + 0.9e1 * t29 * t9 + 0.18e2 * t32;

    return t42+t80;
}

// *************************************************************************
// pqq1
// *************************************************************************

double pqq1_d1(const double& NF)
{
    return 7.0/8.0-NF/36.0-NF*PI*PI/27.0+7.0*PI*PI/18.0-ZETA3/3.0;
}

double pqq1_DD(const double& NF,const double& x)
{
    return 67.0/9.0-10.0*NF/27.0-PI*PI/3.0;
}

double pqq1_DDb(const double& NF,const double& x)
{
    return (67.0/9.0-10.0*NF/27.0-PI*PI/3.0)*log(1.0-x);
}

double pqq1_reg(const double& NF,const double& x)
{
    double t3 = 0.1e1 - x;
    double t4 = 0.1e1 / t3;
    double t5 = log(x);
    double t6 = t4 * t5;
    double t7 = log(t3);
    double t10 = NF * t5;
    double t13 = x * t5;
    double t18 = 0.3141592653589793e1 * 0.3141592653589793e1;
    double t22 = x * x;
    double t28 = t5 * t5;
    double t42 = 0.7e1 / 0.18e2 - 0.8e1 / 0.9e1 * t13 - 0.8e1 / 0.9e1 * t28 * x + 0.11e2 / 0.27e2 * NF * x + t10 / 0.9e1 + 0.4e1 / 0.9e1 * t5 * t22 + x * t18 / 0.6e1 + t4 * t28 - 0.2e1 / 0.3e1 * t5 - 0.8e1 / 0.9e1 * t28 + 0.7e1 / 0.3e1 * t6;
    double t43 = -NF / 0.27e2 - 0.43e2 / 0.6e1 * x - 0.16e2 / 0.9e1 * t6 * t7 - 0.2e1 / 0.9e1 * t10 * t4 + 0.8e1 / 0.9e1 * t13 * t7 + t10 * x / 0.9e1 + t18 / 0.6e1 + 0.10e2 / 0.27e2 / x - 0.28e2 / 0.27e2 * t22 + 0.8e1 / 0.9e1 * t5 * t7 + t42;

    return t43;
}

// *************************************************************************
// pqg1
// *************************************************************************

double pqg1_reg(const double& x)
{
    double t1 = log(x);
    double t3 = log(0.1e1 + x);
    double t7 = log(0.1e1 - x);
    double t10 = x * t1;
    double t12 = t1 * t1;
    double t15 = x * x;
    double t16 = t1 * t15;
    double t18 = Li2(-x);
    double t29 = t7 * t7;
    double t34 = 0.5e1 / 0.12e2 - 0.3e1 / 0.4e1 * t1 * t3 - t1 * t7 / 0.3e1 + 0.8e1 / 0.3e1 * t10 - 0.11e2 / 0.12e2 * t12 * x + 0.37e2 / 0.6e1 * t16 - 0.3e1 / 0.2e1 * t18 * x - 0.3e1 / 0.2e1 * t15 * t18 + t15 * t12 / 0.3e1 - 0.5e1 / 0.6e1 * x * t7 - 0.5e1 / 0.6e1 * ZETA2 * x - 0.5e1 / 0.12e2 * t29 * t15 + 0.5e1 / 0.12e2 * t29 * x;
    double t56 = 0.5e1 / 0.6e1 * t15 * t7 - 0.2e1 / 0.3e1 * t15 * ZETA2 + 0.167e3 / 0.24e2 * x - ZETA2 / 0.3e1 + 0.5e1 / 0.6e1 / x - 0.89e2 / 0.12e2 * t15 + 0.5e1 / 0.8e1 * t1 - 0.7e1 / 0.24e2 * t12 - 0.3e1 / 0.4e1 * t18 - 0.3e1 / 0.2e1 * t10 * t3 - 0.3e1 / 0.2e1 * t16 * t3 + 0.2e1 / 0.3e1 * t10 * t7 - 0.2e1 / 0.3e1 * t16 * t7 - 0.5e1 / 0.24e2 * t29;

    return t34 + t56;
}

// *************************************************************************
// pgq1
// *************************************************************************

double pgq1_reg(const double& NF,const double& x)
{
    double t1 = 0.1e1 / x;
    double t2 = NF * t1;
    double t4 = log(0.1e1 - x);
    double t7 = NF * x;
    double t10 = log(x);
    double t11 = t1 * t10;
    double t13 = log(0.1e1 + x);
    double t18 = Li2(-x);
    double t21 = t4 * t4;
    double t27 = t10 * x;
    double t29 = t10 * t10;
    double t32 = x * x;
    double t38 = 0.1e1 - 0.4e1 / 0.9e1 * t2 * t4 - 0.2e1 / 0.9e1 * t7 * t4 + 0.4e1 * t11 * t13 - 0.4e1 * t11 * t4 + 0.4e1 * t18 - 0.14e2 / 0.3e1 * t4 - 0.10e2 / 0.9e1 * t21 + 0.4e1 * t10 * t13 + 0.4e1 * t10 * t4 - 0.31e2 / 0.9e1 * t27 + 0.11e2 / 0.9e1 * t29 * x - 0.8e1 / 0.3e1 * t10 * t32 + 0.2e1 * t18 * x - 0.16e2 / 0.27e2 * t7;
    double t58 = 0.3141592654e1 * 0.3141592654e1;
    double t63 = -0.20e2 / 0.27e2 * t2 + 0.4e1 / 0.9e1 * NF * t4 + 0.4e1 * t1 * t18 + 0.31e2 / 0.9e1 * x * t4 + 0.5e1 / 0.9e1 * t21 * x + 0.14e2 / 0.3e1 * t1 * t4 + 0.10e2 / 0.9e1 * t1 * t21 + 0.2e1 * t27 * t13 - 0.2e1 * t27 * t4 + 0.23e2 / 0.9e1 * x + 0.20e2 / 0.27e2 * NF + 0.2e1 / 0.3e1 * t58 + t1 + 0.44e2 / 0.9e1 * t32 - 0.100e3 / 0.9e1 * t10 + 0.14e2 / 0.9e1 * t29;
    double t64 = t38 + t63;

    return t64;
}

// *************************************************************************
// pqQ1
// *************************************************************************

double pqQ1_reg(const double& x)
{
    double t3 = log(x);
    double t5 = x * x;
    double t9 = t3 * t3;
    double t15 = 0.10e2 / 0.27e2 / x - 0.1e1 / 0.3e1 + x + t3 / 0.6e1 + 0.4e1 / 0.9e1 * t3 * t5 - 0.28e2 / 0.27e2 * t5 - t9 / 0.6e1 + 0.5e1 / 0.6e1 * t3 * x - x * t9 / 0.6e1;

    return t15;
}

// *************************************************************************
// pqqbar1
// *************************************************************************

double pqqbar1_reg(const double& x)
{
    double t4 = log(x);
    double t6 = x * x;
    double t10 = t4 * t4;
    double t12 = x * t4;
    double t16 = 0.1e1 + x;
    double t17 = 0.1e1 / t16;
    double t18 = Li2(-x);
    double t24 = log(t16);
    double t27 = 0.3141592653589793e1 * 0.3141592653589793e1;
    double t40 = 0.10e2 / 0.27e2 / x - 0.5e1 / 0.9e1 + 0.11e2 / 0.9e1 * x + t4 / 0.18e2 + 0.4e1 / 0.9e1 * t4 * t6 - 0.28e2 / 0.27e2 * t6 - t10 / 0.9e1 + 0.13e2 / 0.18e2 * t12 - 0.2e1 / 0.9e1 * t10 * x + 0.4e1 / 0.9e1 * t17 * t18 - t17 * t10 / 0.9e1 + 0.4e1 / 0.9e1 * t17 * t4 * t24 + t17 * t27 / 0.27e2 - 0.2e1 / 0.9e1 * t18 - 0.2e1 / 0.9e1 * t4 * t24 - t27 / 0.54e2 + 0.2e1 / 0.9e1 * x * t18 + 0.2e1 / 0.9e1 * t12 * t24 + t27 * x / 0.54e2;

    return t40;
}

// *************************************************************************
// double convolutions
// *************************************************************************
// pgg0pgg0
// *************************************************************************

double pgg0pgg0_d1(const double& NF)
{
    return NF*NF/36.0-11.0*NF/12.0+121.0/16.0-3.0*PI*PI/2.0;
}

double pgg0pgg0_DD(const double& NF,const double& x)
{
    return 33.0/2.0-NF+18.0*log(1.0-x);
}

double pgg0pgg0_DDb(const double& NF,const double& x)
{
    double lx = log(1.0-x);
    return (33.0/2.0-NF)*lx + 18.0*lx*lx/2.0;
}

double pgg0pgg0_reg(const double& NF,const double& x)
{
    double t1 = x * x;
    double t5 = 0.1e1 / x;
    double t7 = x - 0.1e1;
    double t8 = 0.1e1 / t7;
    double t11 = log(x);
    double t23 = log(-t7);
    double t34 = -0.6e1 + NF * t1 - NF * x + 0.2e1 * NF - NF * t5 + 0.9e1 * t8 * t1 * x * t11 - 0.36e2 * t8 * t1 * t11 + 0.27e2 * t8 * x * t11 + 0.9e1 * t8 * t5 * t11 - 0.18e2 * t1 * t23 + 0.18e2 * x * t23 - 0.36e2 * t23 + 0.18e2 * t5 * t23 - 0.21e2 / 0.2e1 * x - 0.33e2 / 0.2e1 * t5 + 0.33e2 / 0.2e1 * t1;

    return t34;
}

// *************************************************************************
// pgg0pqg0
// *************************************************************************

double pgg0pqg0_reg(const double& NF,const double& x)
{
    double t1 = x * x;
    double t10 = log(0.1e1 - x);
    double t16 = log(x);
    double t22 = -0.5e1 / 0.2e1 * t1 + 0.17e2 / 0.16e2 + 0.13e2 / 0.8e1 * x - NF * t1 / 0.12e2 - NF / 0.24e2 + NF * x / 0.12e2 + 0.3e1 / 0.2e1 * t1 * t10 + 0.3e1 / 0.4e1 * t10 - 0.3e1 / 0.2e1 * t10 * x + 0.3e1 / 0.4e1 * t16 + 0.3e1 * t16 * x + 0.1e1 / x / 0.2e1;

    return t22;
}

// *************************************************************************
// pgg0pgq0
// *************************************************************************

double pgg0pgq0_reg(const double& NF,const double& x)
{
    double t1 = 0.1e1 / x;
    double t3 = log(0.1e1 - x);
    double t9 = log(x);
    double t22 = x * x;
    double t24 = 0.4e1 * t1 * t3 - 0.4e1 * t3 + 0.2e1 * t3 * x - 0.4e1 * x * t9 - 0.4e1 * t9 - 0.4e1 * t1 * t9 - 0.2e1 / 0.9e1 * NF * t1 + 0.2e1 / 0.9e1 * NF - NF * x / 0.9e1 + 0.17e2 / 0.6e1 * x + 0.13e2 / 0.3e1 - 0.20e2 / 0.3e1 * t1 + 0.4e1 / 0.3e1 * t22;

    return t24;
}

// *************************************************************************
// pgg0pqq0
// *************************************************************************

double pgg0pqq0_d1(const double& NF)
{
    return 11.0/4.0-NF/6.0-2.0*PI*PI/3.0;
}

double pgg0pqq0_DD(const double& NF,const double& x)
{
    return 20.0/3.0-2.0*NF/9.0+8.0*log(1.0-x);
}

double pgg0pqq0_DDb(const double& NF,const double& x)
{
    double lx = log(1.0-x);
    return (20.0/3.0-2.0*NF/9.0)*lx+8.0*lx*lx/2.0;
}

double pgg0pqq0_reg(const double& NF,const double& x)
{
    double t3 = log(x);
    double t5 = 0.1e1 - x;
    double t6 = log(t5);
    double t8 = x * x;
    double t23 = -0.17e2 / 0.6e1 - 0.23e2 / 0.6e1 * x + NF / 0.9e1 + 0.6e1 * t3 - 0.10e2 * t6 + 0.4e1 * t3 * t8 + NF * x / 0.9e1 + 0.2e1 * x * t6 - 0.4e1 * t8 * t6 + 0.4e1 / x * t6 - 0.4e1 * t3 / t5;

    return t23;
}

// *************************************************************************
// pgq0pgq0
// *************************************************************************

double pgq0pgq0_reg(const double& x)
{
    double t1 = log(x);
    double t5 = 0.1e1 / x;
    double t10 = -0.4e1 / 0.9e1 * x * t1 - 0.16e2 / 0.9e1 * t1 - 0.16e2 / 0.9e1 * t5 * t1 + 0.8e1 / 0.9e1 * x + 0.16e2 / 0.9e1 - 0.8e1 / 0.3e1 * t5;

    return t10;
}

// *************************************************************************
// pgq0pqg0
// *************************************************************************

double pgq0pqg0_reg(const double& x)
{
    double t1 = log(x);
    double t5 = x * x;
    double t10 = t1 / 0.3e1 + x * t1 / 0.3e1 - 0.2e1 / 0.9e1 * t5 - x / 0.6e1 + 0.1e1 / 0.6e1 + 0.2e1 / 0.9e1 / x;

    return t10;
}

// *************************************************************************
// pgq0pqq0
// *************************************************************************

double pgq0pqq0_reg(const double& x)
{
    double t3 = log(0.1e1 - x);
    double t9 = log(x);
    double t14 = 0.16e2 / 0.9e1 / x * t3 - 0.16e2 / 0.9e1 * t3 + 0.8e1 / 0.9e1 * t3 * x + 0.8e1 / 0.9e1 * t9 - 0.4e1 / 0.9e1 * t9 * x - 0.2e1 / 0.9e1 * x + 0.8e1 / 0.9e1;

    return t14;
}

// *************************************************************************
// pqg0pqg0
// *************************************************************************

double pqg0pqg0_reg(const double& x)
{
    double t1 = log(x);
    double t2 = x * x;
    double t10 = -t1 * t2 / 0.4e1 - x * t1 / 0.4e1 - t1 / 0.16e2 + 0.3e1 / 0.8e1 * t2 - x / 0.4e1 - 0.1e1 / 0.8e1;

    return t10;
}

// *************************************************************************
// pqg0pqq0
// *************************************************************************

double pqg0pqq0_reg(const double& x)
{
    double t2 = log(x);
    double t6 = x * x;
    double t10 = log(0.1e1 - x);
    double t16 = -0.1e1 / 0.12e2 + x / 0.3e1 - t2 / 0.6e1 + t2 * x / 0.3e1 - 0.2e1 / 0.3e1 * t2 * t6 + 0.2e1 / 0.3e1 * t6 * t10 + t10 / 0.3e1 - 0.2e1 / 0.3e1 * t10 * x;

    return t16;
}

// *************************************************************************
// pqq0pqq0
// *************************************************************************

double pqq0pqq0_d1()
{
    return 1.0-8.0*PI*PI/27.0;
}

double pqq0pqq0_DD(const double& x)
{
    return 8.0/3.0+32.0*log(1.0-x)/9.0;
}

double pqq0pqq0_DDb(const double& x)
{
    double lx = log(1.0-x);
    return 8.0/3.0*lx+32.0*lx*lx/9.0/2.0;
}

double pqq0pqq0_reg(const double& x)
{
    double t2 = log(x);
    double t3 = 0.1e1 - x;
    double t7 = log(t3);
    double t14 = -0.20e2 / 0.9e1 - 0.4e1 / 0.9e1 * x - 0.16e2 / 0.9e1 * t2 / t3 - 0.16e2 / 0.9e1 * t7 - 0.16e2 / 0.9e1 * x * t7 + 0.4e1 / 0.3e1 * t2 + 0.4e1 / 0.3e1 * t2 * x;
    
    return t14;
}
