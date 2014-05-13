
#include "virtual_amplitude.h"

// the  Basis Functions Boxes Weight 3 in the physical region
// aka Qoc, Qec
#include "VV_masters_from_maple.cpp"

// the argument of polylog2 here is supposed to be such
// that the result is always real. If its imaginary part is non-zero
// we raise an error
double polylog(int i,const complex<double>& z)
{
    if (i==2)
    {
        std::complex<double> res = HPL2(0,1,z);
        if (std::imag(res)!=0.0 and abs(std::imag(res))>1e-6)
        {
            cout<<"\nWarning in polylog2: complex result! "
            <<" z = "<<z<<" HPL2(0,1,z) = "<<res
            <<endl;
            //exit(0);
        }
        return std::real(res);
    }
    else if (i==3)
    {
        std::complex<double> res = HPL3(0,0,1,z);
        if (std::imag(res)!=0.0 and abs(std::imag(res))>1e-6 )
        {
            cout<<"\nWarning in polylog3: complex result! "
            <<" z = "<<z<<" HPL3(0,1,z) = "<<res
            <<endl;
            //exit(0);
        }
        return std::real(res);
    }
    else {cout<<"\nerror in polylog: only Li2 and Li3 is defined!";return 0.0;}
}


double Ibub_m1(const double& s){return 1.0;}
double Ibub_f(const double& s){return(-log(s) + 2.0);}
double Ibub_p1(const double& s)
{
    return(pow(log(s), 2.0) / 2.0 
           - consts::pi_square/3.0  
           - 2.0 * log(s) + 4.0);
}
double Ibub_p2(const double& s)
{
    const double Ls = log(s);
    return -(1./6.)*
                (      Ls*Ls*Ls
                 -6. * Ls*Ls
                 +24.* Ls
                 -2. * Ls*consts::pi_square 
                 +4. * consts::pi_square
                 -48.);
}

double Ibub2_m1(const double& s){return 1.0;}
double Ibub2_f(const double& s){return Ibub_f(s);}
double Ibub2_p1(const double& s)
{return (pow(log(s), 2.0) / 2.0 
         - consts::pi_square/3.0  
         - 2.0 * log(s) + 4.0) + consts::pi_square/2.0;
}
double Ibub2_p2(const double& s)
{
    const double Ls = log(s);
    return -(1./6.)*
    (      Ls*Ls*Ls
     -6. * Ls*Ls
     +24.* Ls
     +1. * Ls*consts::pi_square 
     -2. * consts::pi_square
     -48.);
}



//convention: kk = {q3,q4,u,t}
double TP0011_123_m1(const vector<double>& kk) {return Ibub_m1(kk[0]);}
double TP0011_123_f(const vector<double>& kk) { return Ibub_f( kk[0]);}
double TP0011_123_p1(const vector<double>& kk) {return Ibub_p1(kk[0]);}
double TP0011_123_p2(const vector<double>& kk) {return Ibub_p2(kk[0]);}

double TP0011_124_m1(const vector<double>& kk) {return Ibub_m1(kk[1]);}
double TP0011_124_f(const vector<double>& kk)  {return Ibub_f( kk[1]);}
double TP0011_124_p1(const vector<double>& kk) {return Ibub_p1(kk[1]);}
double TP0011_124_p2(const vector<double>& kk) {return Ibub_p2(kk[1]);}

double TP0101_123_m1(const vector<double>& kk) {return Ibub2_m1(-kk[2]);}
double TP0101_123_f(const vector<double>& kk)  {return Ibub2_f( -kk[2]);}
double TP0101_123_p1(const vector<double>& kk) {return Ibub2_p1(-kk[2]);}
double TP0101_123_p2(const vector<double>& kk) {return Ibub2_p2(-kk[2]);}

double TP0101_124_m1(const vector<double>& kk) {return Ibub2_m1(-kk[3]);}
double TP0101_124_f(const vector<double>& kk)  {return Ibub2_f( -kk[3]);}
double TP0101_124_p1(const vector<double>& kk) {return Ibub2_p1(-kk[3]);}
double TP0101_124_p2(const vector<double>& kk) {return Ibub2_p2(-kk[3]);}

double TP1001_123_m1(const vector<double>& kk) {return TP0011_124_m1(kk);}
double TP1001_123_f(const vector<double>& kk) {return TP0011_124_f(kk);}
double TP1001_123_p1(const vector<double>& kk) {return TP0011_124_p1(kk);}
double TP1001_123_p2(const vector<double>& kk) {return TP0011_124_p2(kk);}

double TP1001_124_m1(const vector<double>& kk) {return TP0011_123_m1(kk);}
double TP1001_124_f(const vector<double>& kk)  {return TP0011_123_f(kk);}
double TP1001_124_p1(const vector<double>& kk) {return TP0011_123_p1(kk);}
double TP1001_124_p2(const vector<double>& kk) {return TP0011_123_p2(kk);}

double TP1010_123_m1(const vector<double>& kk) {return Ibub_m1(1.0);}
double TP1010_123_f(const vector<double>& kk)  {return Ibub_f( 1.0);}
double TP1010_123_p1(const vector<double>& kk) {return Ibub_p1(1.0);}
double TP1010_123_p2(const vector<double>& kk) {return Ibub_p2(1.0);}

double TP1010_124_m1(const vector<double>& kk) {return Ibub_m1(1.0);}
double TP1010_124_f(const vector<double>& kk)  {return Ibub_f( 1.0);}
double TP1010_124_p1(const vector<double>& kk) {return Ibub_p1(1.0);}
double TP1010_124_p2(const vector<double>& kk) {return Ibub_p2(1.0);}


double Itri(const double& z, const double& zp)
{
    return (-log((-1.0+zp)*(-1.0+z))*log(z)
            +log((-1.0+zp)*(-1.0+z))*log(zp)
            +2.0*polylog(2, 1.0-zp)
            -2.0*polylog(2, 1.0-z)
            )
    /(z-zp) ;
}

double Itri_p1(const double& q3, const double& q4,const double& z, const double& zp)
{ return
    -0.2e1 / 0.3e1 / (z - zp) * pow(log(z), 0.3e1) + 0.2e1 / (z - zp) * log(zp) * polylog(2, 0.1e1 - zp) - 0.2e1 / 0.3e1 * 0.3141592654e1 * 0.3141592654e1 / (z - zp) * log(zp) - 0.1e1 / (z - zp) * pow(log(zp), 0.3e1) / 0.3e1 - 0.1e1 / (z - zp) * pow(log(0.1e1 - z), 0.3e1) / 0.3e1 - 0.2e1 / 0.3e1 / (z - zp) * pow(log(0.1e1 - zp), 0.3e1) + 0.1e1 / (z - zp) * log(0.1e1 - z) * pow(log(0.1e1 - zp), 0.2e1) - 0.2e1 / (z - zp) * log(z - zp) * log(0.1e1 - zp) * log(0.1e1 - z) + 0.4e1 / (z - zp) * log(z - zp) * log(0.1e1 - z) * log(zp) + 0.1e1 / (z - zp) * pow(log(z), 0.2e1) * log(zp) - 0.1e1 / (z - zp) * log(0.1e1 - z) * log(z) * log(zp) - 0.1e1 / (z - zp) * log(0.1e1 - zp) * log(z) * log(zp) - 0.2e1 / (z - zp) * log(z - zp) * log(z) * log(zp) - 0.3e1 / (z - zp) * log(0.1e1 - zp) * log(z) * log(0.1e1 - z) - 0.4e1 / (z - zp) * log(z - zp) * log(z) * log(0.1e1 - zp) + 0.1e1 / (z - zp) * log((-0.1e1 + zp) * (-0.1e1 + z)) * log(q4) * log(z) - 0.1e1 / (z - zp) * log((-0.1e1 + zp) * (-0.1e1 + z)) * log(q4) * log(zp) + 0.1e1 / (z - zp) * log((-0.1e1 + zp) * (-0.1e1 + z)) * log(q3) * log(z) - 0.1e1 / (z - zp) * log((-0.1e1 + zp) * (-0.1e1 + z)) * log(q3) * log(zp) + 0.1e1 / (z - zp) * log(0.1e1 - zp) * log(0.1e1 - z) * log(zp) - 0.1e1 / (z - zp) * log(0.1e1 - z) * pow(log(z), 0.2e1) / 0.2e1 + 0.3e1 / 0.2e1 / (z - zp) * log(0.1e1 - zp) * pow(log(z), 0.2e1) + 0.1e1 / (z - zp) * log(z - zp) * pow(log(z), 0.2e1) - 0.1e1 / (z - zp) * pow(log(0.1e1 - z), 0.2e1) * log(z) / 0.2e1 + log(z - zp) / (z - zp) * pow(log(0.1e1 - zp), 0.2e1) + 0.3e1 / 0.2e1 / (z - zp) * pow(log(0.1e1 - zp), 0.2e1) * log(z) + 0.2e1 / (z - zp) * polylog(2, (-0.1e1 + z) / (-0.1e1 + zp)) * log(z) - 0.2e1 / (z - zp) * log(z) * polylog(2, 0.1e1 - z) + 0.2e1 / (z - zp) * polylog(2, 0.1e1 - zp) * log(z) + 0.2e1 / (z - zp) * polylog(3, zp / z) + 0.2e1 / (z - zp) * polylog(3, (-0.1e1 + z) / (-0.1e1 + zp)) + 0.2e1 / (z - zp) * polylog(3, 0.1e1 / (-0.1e1 + z) / zp * (z - zp)) + 0.4e1 / (z - zp) * polylog(3, -(z - zp) / (-0.1e1 + zp)) - 0.4e1 * consts::z3 / (z - zp) - 0.2e1 / (z - zp) * polylog(3, -0.1e1 / (-0.1e1 + zp) / z * (z - zp)) + 0.4e1 / (z - zp) * polylog(3, 0.1e1 / z * (z - zp)) + log(z - zp) / (z - zp) * pow(log(0.1e1 - z), 0.2e1) - 0.2e1 / (z - zp) * polylog(2, 0.1e1 - z) * log(0.1e1 - zp) + 0.2e1 / (z - zp) * polylog(2, 0.1e1 - zp) * log(0.1e1 - zp) + 0.2e1 / (z - zp) * log(q3) * polylog(2, 0.1e1 - z) - 0.2e1 / (z - zp) * polylog(2, 0.1e1 - zp) * log(q3) + 0.2e1 / (z - zp) * polylog(2, zp / z) * log(0.1e1 - z) - 0.2e1 / (z - zp) * polylog(2, 0.1e1 - z) * log(0.1e1 - z) + 0.2e1 / (z - zp) * polylog(2, 0.1e1 - zp) * log(0.1e1 - z) - 0.2e1 / 0.3e1 * consts::pi_square / (z - zp) * log(0.1e1 - z) + 0.2e1 / (z - zp) * polylog(2, zp / z) * log(0.1e1 - zp) + 0.2e1 / (z - zp) * log(q4) * polylog(2, 0.1e1 - z) - 0.2e1 / (z - zp) * polylog(2, 0.1e1 - zp) * log(q4) - 0.1e1 / (z - zp) * log(0.1e1 - z) * pow(log(zp), 0.2e1) / 0.2e1 + 0.3e1 / 0.2e1 / (z - zp) * log(0.1e1 - zp) * pow(log(zp), 0.2e1) + 0.1e1 / (z - zp) * log(z - zp) * pow(log(zp), 0.2e1) - 0.1e1 / (z - zp) * pow(log(0.1e1 - z), 0.2e1) * log(zp) / 0.2e1 + 0.3e1 / 0.2e1 / (z - zp) * pow(log(0.1e1 - zp), 0.2e1) * log(zp) + 0.2e1 / (z - zp) * polylog(2, (-0.1e1 + z) / (-0.1e1 + zp)) * log(zp) - 0.2e1 / (z - zp) * polylog(2, 0.1e1 - z) * log(zp);

}
double TP1011_123_f(const vector<double>& kk) 
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));                      
    return Itri( z, zp);
}

double TP1011_123_p1(const vector<double>& kk) 
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));                      
    return Itri_p1(q3,q4, z, zp);
}

double TP1011_124_f(const vector<double>& kk) 
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));                      
    return Itri(1.0-zp, 1.0-z );
}

double TP1011_124_p1(const vector<double>& kk) 
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));                      
    return Itri_p1(q4,q3,1.0-zp, 1.0-z );
}



double Ibox_m2(const double& x,const double& m1,const double& m2)
{
    return(1.0 / x);
}

double Ibox_m1(const double& x, 
               const double& m1,const double& m2)
{
    return 1.0 / x * ( log(m1*m2/(x*x)) ); 
}


double Ibox_f(const double& u, const double& m1,const double& m2)
{
    const double x = -u;
    return 1.0/u * (
                    (1.0/3.0)*consts::pi_square
                    +log(m1+x) * log((m1+x)/x/x)
                    +log(m2+x) * log((m2+x)/x/x)
                    -(1.0/2.0)*pow(log(m2/m1),2.0)
                    +2.0*pow(log(x),2.0)
                    +2.0*polylog(2, x/(m1+x))
                    +2.0*polylog(2, x/(m2+x))
                    );
}

double Ibox_p1(const double& u,const double& q3, const double& q4,const double& z,const double& zp)
{
    return ( (96 * consts::z3) - 0.24e2 * polylog(2, zp) * log(z * zp) - 0.24e2 * polylog(2, z) * log(z * zp) +  (48 * polylog(3, 0.1e1 - zp)) +  (48 * polylog(3, zp)) +  (48 * polylog(3, z)) +  (48 * polylog(3, 0.1e1 - z)) + 0.12e2 * log(q3) * 0.3141592654e1 * 0.3141592654e1 -  (24 * QQec(0.1e1 - zp, 0.1e1 - z, -u)) - 0.16e2 * pow(log(-u), 0.3e1) + 0.24e2 * pow(log(q3 - u), 0.3e1) + 0.24e2 * pow(log(q4 - u), 0.3e1) + 0.24e2 * polylog(2, -u / (q4 - u)) * log(-u) - 0.24e2 * polylog(2, -u / (q4 - u)) * log(q3) - 0.72e2 * polylog(2, -u / (q4 - u)) * log(q4) - 0.36e2 * pow(log(q3), 0.2e1) * log(-u) + 0.36e2 * pow(log(q3), 0.2e1) * log(q3 - u) - 0.12e2 * log(q3) * pow(log(q4 - u), 0.2e1) - 0.72e2 * log(q3) * pow(log(q3 - u), 0.2e1) + 0.12e2 * pow(log(-u), 0.2e1) * log(q3 - u) + 0.12e2 * pow(log(-u), 0.2e1) * log(q4 - u) - 0.11e2 * log(q4) * pow(log(q3), 0.2e1) - 0.12e2 * log(q4) * pow(log(q3 - u), 0.2e1) - 0.72e2 * log(q4) * pow(log(q4 - u), 0.2e1) - 0.36e2 * pow(log(q4), 0.2e1) * log(-u) - 0.2e1 * pow(log(q4), 0.2e1) * log(q3) + 0.36e2 * pow(log(q4), 0.2e1) * log(q4 - u) - 0.24e2 * log(-u) * pow(log(q4 - u), 0.2e1) - 0.24e2 * log(-u) * pow(log(q3 - u), 0.2e1) - 0.24e2 * polylog(2, -u / (q3 - u)) * log(q4) - 0.72e2 * polylog(2, -u / (q3 - u)) * log(q3) + 0.24e2 * polylog(2, -u / (q3 - u)) * log(-u) - 0.4e1 * pow(log(z * zp), 0.2e1) * log(0.1e1 - zp) - 0.4e1 * pow(log(z * zp), 0.2e1) * log(0.1e1 - z) - 0.24e2 * log((-0.1e1 + zp) * (-0.1e1 + z)) * polylog(2, 0.1e1 - zp) - 0.24e2 * log((-0.1e1 + zp) * (-0.1e1 + z)) * polylog(2, 0.1e1 - z) - 0.4e1 * pow(log((-0.1e1 + zp) * (-0.1e1 + z)), 0.2e1) * log(zp) - 0.4e1 * pow(log((-0.1e1 + zp) * (-0.1e1 + z)), 0.2e1) * log(z) + 0.48e2 * 0.3141592654e1 * 0.3141592654e1 * log(-(q3 - u) / u) + 0.48e2 * 0.3141592654e1 * 0.3141592654e1 * log(-(q4 - u) / u) -  (72 * polylog(3, -u / (q3 - u))) -  (72 * polylog(3, q3 / (q3 - u))) -  (72 * polylog(3, -u / (q4 - u))) -  (72 * polylog(3, q4 / (q4 - u))) + 0.88e2 * log(-u) * 0.3141592654e1 * 0.3141592654e1 + 0.8e1 * log(q4) * 0.3141592654e1 * 0.3141592654e1 + 0.2e1 * pow(log(q4), 0.3e1) + 0.2e1 * pow(log(q3), 0.3e1) - 0.48e2 * 0.3141592654e1 * 0.3141592654e1 * log(q3 - u) - 0.48e2 * 0.3141592654e1 * 0.3141592654e1 * log(q4 - u) + 0.24e2 * log(q3) * log(-u) * log(q4 - u) + 0.72e2 * log(q3) * log(-u) * log(q3 - u) - 0.24e2 * log(q4) * log(q3) * log(-u) + 0.72e2 * log(q4) * log(-u) * log(q4 - u) + 0.24e2 * log(q4) * log(-u) * log(q3 - u)) / u / 0.12e2;
}




double TP1111_123_m2(const vector<double>& kk)
{return Ibox_m2(kk[2],kk[0],kk[1]);}
double TP1111_123_m1(const vector<double>& kk)
{return Ibox_m1(kk[2],kk[0],kk[1]);}
double TP1111_123_f( const vector<double>& kk)
{return Ibox_f (kk[2],kk[0],kk[1]);}
double TP1111_123_p1( const vector<double>& kk)
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    
return Ibox_p1 (kk[2],q3,q4,z,zp);
}

double TP1111_124_m2(const vector<double>& kk)
{return Ibox_m2(kk[3],kk[1],kk[0]);}
double TP1111_124_m1(const vector<double>& kk)
{return Ibox_m1(kk[3],kk[1],kk[0]);}
double TP1111_124_f( const vector<double>& kk)
{return Ibox_f (kk[3],kk[1],kk[0]);}
double TP1111_124_p1( const vector<double>& kk)
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    
    return Ibox_p1 (kk[3],q4,q3,1-zp,1-z);
}



// 2 Loop master integrals



int I1m3 (const vector<double>& kk)
{
    return(0);
}
int I1m2 (const vector<double>& kk)
{
    return(0);
}
double I1m1 (
             const vector<double>& kk)
{

    const double u = kk[2];
return(-u / 0.4e1);
}


double I10 (
            const vector<double>& kk)
{
    const double u = kk[2];
return(u * log(-u) / 0.2e1 - 0.13e2 / 0.8e1 * u);
}


double I11 (
            const vector<double>& kk)
{
    const double u = kk[2];
return(-u * pow(log(-u), 0.2e1) / 0.2e1 - u * consts::pi_square / 0.12e2 + 0.13e2 / 0.4e1 * u * log(-u) - 0.115e3 / 0.16e2 * u);
}


double I12 (
            const vector<double>& kk)
{
    const double u = kk[2];

    return(u * consts::pi_square * log(-u) / 0.6e1 + u * pow(log(-u), 0.3e1) / 0.3e1 - 0.13e2 / 0.24e2 * u * consts::pi_square - 0.13e2 / 0.4e1 * u * pow(log(-u), 0.2e1) + 0.3e1 / 0.2e1 * u * consts::z3 + 0.115e3 / 0.8e1 * u * log(-u) - 0.865e3 / 0.32e2 * u);
}




int I2m3 (const vector<double>& kk)
{
    return(0);
}
int I2m2 (const vector<double>& kk)
{
    return(0);
}
double I2m1 (
             const vector<double>& kk)
{const double t = kk[3];

    return(-t / 0.4e1);
}


double I20 (
            const vector<double>& kk)
{
    const double t = kk[3];
 return(t * log(-t) / 0.2e1 - 0.13e2 / 0.8e1 * t);
}


double I21 (
            const vector<double>& kk)
{
    const double t = kk[3];
 return(-t * pow(log(-t), 0.2e1) / 0.2e1 - t * consts::pi_square / 0.12e2 + 0.13e2 / 0.4e1 * t * log(-t) - 0.115e3 / 0.16e2 * t);
}


double I22 (
            const vector<double>& kk)
{
    const double t = kk[3];
return(t * pow(log(-t), 0.3e1) / 0.3e1 + t * consts::pi_square * log(-t) / 0.6e1 - 0.13e2 / 0.4e1 * t * pow(log(-t), 0.2e1) - 0.13e2 / 0.24e2 * t * consts::pi_square + 0.115e3 / 0.8e1 * t * log(-t) + 0.3e1 / 0.2e1 * t * consts::z3 - 0.865e3 / 0.32e2 * t);
}
int I3m3 (const vector<double>& kk)
{
    return(0);
}
double I3m2 (
             const vector<double>& kk)
{
    return(0.1e1 / 0.2e1);
}


double I3m1 (
             const vector<double>& kk)
{
    const double q3=kk[0];
    return(-log(q3) + 0.5e1 / 0.2e1);
}


double I30 (
            const vector<double>& kk)
{
const double q3=kk[0];
    const double u = kk[2];

    return(pow(log(q3), 0.2e1) / 0.2e1 + log(q3) * log(-u) - 0.2e1 / 0.3e1 * consts::pi_square - log(-u) * log(q3 - u) + pow(log(q3 - u), 0.2e1) / 0.2e1 - 0.5e1 * log(q3) + polylog(2, -u / (q3 - u)) + 0.19e2 / 0.2e1);
}


double I31 (
            const vector<double>& kk)
{
const double q3=kk[0];
    const double u = kk[2];

    return(0.65e2 / 0.2e1 + 0.5e1 * log(q3) * log(-u) - 0.5e1 * log(-u) * log(q3 - u) + consts::pi_square * log(q3) / 0.3e1 + consts::pi_square * log(q3 - u) - pow(log(q3), 0.2e1) * log(q3 - u) / 0.2e1 + log(q3) * pow(log(q3 - u), 0.2e1) / 0.2e1 + pow(log(-u), 0.2e1) * log(q3 - u) - 0.2e1 * polylog(2, -u / (q3 - u)) * log(-u) - log(q3) * pow(log(-u), 0.2e1) + 0.5e1 / 0.2e1 * pow(log(q3), 0.2e1) - 0.10e2 / 0.3e1 * consts::pi_square + 0.5e1 / 0.2e1 * pow(log(q3 - u), 0.2e1) + 0.5e1 * polylog(2, -u / (q3 - u)) - 0.19e2 * log(q3) - 0.3e1 * consts::z3 - pow(log(q3 - u), 0.3e1) / 0.2e1 - pow(log(q3), 0.3e1) / 0.6e1 + polylog(3, q3 / (q3 - u)) + (double) (2 * polylog(3, -u / (q3 - u))));
}





int I4m3 (const vector<double>& kk)
{
    return(0);
}
double I4m2 (
             const vector<double>& kk)
{
    return(0.1e1 / 0.2e1);
}


double I4m1 (
             const vector<double>& kk)
{
    const double q4=kk[1];
    return(-log(q4) + 0.5e1 / 0.2e1);
}


double I40 (
            const vector<double>& kk)
{
    const double q4=kk[1];
    const double t = kk[3];
      return(pow(log(q4), 0.2e1) / 0.2e1 + log(q4) * log(-t) - log(-t) * log(q4 - t) + pow(log(q4 - t), 0.2e1) / 0.2e1 - 0.2e1 / 0.3e1 * consts::pi_square - 0.5e1 * log(q4) + polylog(2, -t / (q4 - t)) + 0.19e2 / 0.2e1);
}


double I41 (
            const vector<double>& kk)
{
    const double q4=kk[1];
    const double t = kk[3];
    return(0.65e2 / 0.2e1 + 0.5e1 * log(q4) * log(-t) - 0.5e1 * log(-t) * log(q4 - t) + 0.5e1 / 0.2e1 * pow(log(q4), 0.2e1) + 0.5e1 / 0.2e1 * pow(log(q4 - t), 0.2e1) + (double) (5 * polylog(2, -t / (q4 - t))) - 0.10e2 / 0.3e1 * consts::pi_square + polylog(3, q4 / (q4 - t)) + (double) (2 * polylog(3, -t / (q4 - t))) - pow(log(q4 - t), 0.3e1) / 0.2e1 - pow(log(q4), 0.3e1) / 0.6e1 - 0.3e1 * consts::z3 - 0.19e2 * log(q4) + consts::pi_square * log(q4 - t) - log(q4) * pow(log(-t), 0.2e1) + consts::pi_square * log(q4) / 0.3e1 + log(q4) * pow(log(q4 - t), 0.2e1) / 0.2e1 + pow(log(-t), 0.2e1) * log(q4 - t) - pow(log(q4), 0.2e1) * log(q4 - t) / 0.2e1 - 0.2e1 * (double) polylog(2, -t / (q4 - t)) * log(-t));
}



int I5m3 (const vector<double>& kk)
{
    return(0);
}
double I5m2 (
             const vector<double>& kk)
{
    return(0.1e1 / 0.2e1);
}


double I5m1 (
             const vector<double>& kk)
{
    const double q4=kk[1];
    const double u = kk[2];
    return(-log(q4) + 0.5e1 / 0.2e1);
}


double I50 (
            const vector<double>& kk)
{
    const double q4=kk[1];
    const double u = kk[2];
    return(pow(log(q4), 0.2e1) / 0.2e1 + log(q4) * log(-u) - 0.2e1 / 0.3e1 * consts::pi_square - log(q4 - u) * log(-u) + pow(log(q4 - u), 0.2e1) / 0.2e1 - 0.5e1 * log(q4) + polylog(2, -u / (q4 - u)) + 0.19e2 / 0.2e1);
}


double I51 (
            const vector<double>& kk)
{
    const double q4=kk[1];
    const double u = kk[2];
    return(0.65e2 / 0.2e1 + polylog(3, q4 / (q4 - u)) + (double) (2 * polylog(3, -u / (q4 - u))) + 0.5e1 / 0.2e1 * pow(log(q4), 0.2e1) - 0.10e2 / 0.3e1 * consts::pi_square - pow(log(q4 - u), 0.3e1) / 0.2e1 + 0.5e1 / 0.2e1 * pow(log(q4 - u), 0.2e1) + (double) (5 * polylog(2, -u / (q4 - u))) - pow(log(q4), 0.3e1) / 0.6e1 + consts::pi_square * log(q4 - u) - pow(log(q4), 0.2e1) * log(q4 - u) / 0.2e1 + pow(log(-u), 0.2e1) * log(q4 - u) + log(q4) * pow(log(q4 - u), 0.2e1) / 0.2e1 - 0.2e1 * (double) polylog(2, -u / (q4 - u)) * log(-u) - log(q4) * pow(log(-u), 0.2e1) - 0.3e1 * consts::z3 - 0.19e2 * log(q4) + 0.5e1 * log(q4) * log(-u) - 0.5e1 * log(q4 - u) * log(-u) + consts::pi_square * log(q4) / 0.3e1);
}



int I6m3 (const vector<double>& kk)
{
    return(0);
}
double I6m2 (
             const vector<double>& kk)
{
    return(0.1e1 / 0.2e1);
}


double I6m1 (
             const vector<double>& kk)
{
    const double q3=kk[0];
    
        return(-log(q3) + 0.5e1 / 0.2e1);
}


double I60 (
            const vector<double>& kk)
{
    const double q3=kk[0];
    const double t = kk[3];    return(pow(log(q3), 0.2e1) / 0.2e1 + log(q3) * log(-t) - log(q3 - t) * log(-t) + pow(log(q3 - t), 0.2e1) / 0.2e1 - 0.2e1 / 0.3e1 * consts::pi_square - 0.5e1 * log(q3) + polylog(2, -t / (q3 - t)) + 0.19e2 / 0.2e1);
}


double I61 (
            const vector<double>& kk)
{
const double q3=kk[0];
const double t = kk[3];
    return(0.65e2 / 0.2e1 + 0.5e1 * log(q3) * log(-t) - 0.5e1 * log(q3 - t) * log(-t) + 0.5e1 / 0.2e1 * pow(log(q3 - t), 0.2e1) + (double) (5 * polylog(2, -t / (q3 - t))) + consts::pi_square * log(q3) / 0.3e1 + 0.5e1 / 0.2e1 * pow(log(q3), 0.2e1) - 0.10e2 / 0.3e1 * consts::pi_square - 0.19e2 * log(q3) + consts::pi_square * log(q3 - t) - log(q3) * pow(log(-t), 0.2e1) + pow(log(-t), 0.2e1) * log(q3 - t) + log(q3) * pow(log(q3 - t), 0.2e1) / 0.2e1 - pow(log(q3), 0.2e1) * log(q3 - t) / 0.2e1 - 0.2e1 * (double) polylog(2, -t / (q3 - t)) * log(-t) - 0.3e1 * consts::z3 - pow(log(q3), 0.3e1) / 0.6e1 + polylog(3, q3 / (q3 - t)) + (double) (2 * polylog(3, -t / (q3 - t))) - pow(log(q3 - t), 0.3e1) / 0.2e1);
}




int I7m3 (const vector<double>& kk)
{
    return(0);
}
double I7m2 (
             const vector<double>& kk)
{
    return(0.1e1 / 0.2e1);
}
double I7m1 (
             const vector<double>& kk)
{
    return(0.5e1 / 0.2e1);
}
double I70 (
            const vector<double>& kk)
{
    return(-0.2e1 / 0.3e1 * consts::pi_square + 0.19e2 / 0.2e1);
}
double I71 (
            const vector<double>& kk)
{
    return(-0.10e2 / 0.3e1 * consts::pi_square - 0.2e1 * consts::z3 + 0.65e2 / 0.2e1);
}
int I8m3 (const vector<double>& kk)
{
    return(0);
}
double I8m2 (
             const vector<double>& kk)
{
    return(0.1e1 / 0.2e1);
}
double I8m1 (
             const vector<double>& kk)
{
    return(0.5e1 / 0.2e1);
}
double I80 (
            const vector<double>& kk)
{
    return(-0.2e1 / 0.3e1 * consts::pi_square + 0.19e2 / 0.2e1);
}
double I81 (
            const vector<double>& kk)
{
    return(-0.10e2 / 0.3e1 * consts::pi_square - 0.2e1 * consts::z3 + 0.65e2 / 0.2e1);
}
int I9m3 (const vector<double>& kk)
{
    return(0);
}
int I9m2 (const vector<double>& kk)
{
    return(0);
}


double I9m1 (
             const vector<double>& kk)
{
    const double q3=kk[0];
    const double q4=kk[1];

    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    return(-0.1e1 / (z - zp) * log((z - 0.1e1) * (-0.1e1 + zp)) * log(z) + 0.1e1 / (z - zp) * log((z - 0.1e1) * (-0.1e1 + zp)) * log(zp) + 0.2e1 / (z - zp) * polylog(2, 0.1e1 - zp) - 0.2e1 / (z - zp) * polylog(2, 0.1e1 - z));
}


double I90 (
            const vector<double>& kk)
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double u = kk[2];
    const double t = kk[3];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
     return(-0.8e1 / (z - zp) * consts::z3 + 0.4e1 / (z - zp) * polylog(3, zp / z) - 0.4e1 / 0.3e1 / (z - zp) * pow(log(z), 0.3e1) - 0.2e1 / 0.3e1 / (z - zp) * pow(log(zp), 0.3e1) - 0.2e1 / 0.3e1 / (z - zp) * pow(log(0.1e1 - z), 0.3e1) - 0.4e1 / 0.3e1 / (z - zp) * pow(log(0.1e1 - zp), 0.3e1) + 0.1e1 / (z - zp) * QQoc(0.1e1 - zp, 0.1e1 - z, -u) + 0.4e1 / (z - zp) * polylog(3, 0.1e1 / (z - 0.1e1) / zp * z - 0.1e1 / (z - 0.1e1)) + 0.8e1 / (z - zp) * polylog(3, -0.1e1 / (-0.1e1 + zp) * z + 0.1e1 / (-0.1e1 + zp) * zp) - 0.4e1 / (z - zp) * polylog(3, -0.1e1 / (-0.1e1 + zp) + 0.1e1 / (-0.1e1 + zp) / z * zp) + 0.4e1 / (z - zp) * polylog(3, 0.1e1 / (-0.1e1 + zp) * z - 0.1e1 / (-0.1e1 + zp)) + 0.8e1 / (z - zp) * polylog(3, 0.1e1 - zp / z) + 0.4e1 / (z - zp) * polylog(2, 0.1e1 - zp) - 0.4e1 / (z - zp) * polylog(2, 0.1e1 - z) + 0.4e1 / (z - zp) * polylog(2, 0.1e1 / (-0.1e1 + zp) * z - 0.1e1 / (-0.1e1 + zp)) * log(z) + 0.4e1 / (z - zp) * polylog(2, 0.1e1 / (-0.1e1 + zp) * z - 0.1e1 / (-0.1e1 + zp)) * log(zp) - 0.4e1 / 0.3e1 / (z - zp) * consts::pi_square * log(zp) - 0.4e1 / 0.3e1 / (z - zp) * consts::pi_square * log(0.1e1 - z) + 0.8e1 / (z - zp) * log(0.1e1 - z) * log(z - zp) * log(zp) - 0.8e1 / (z - zp) * log(z) * log(0.1e1 - zp) * log(z - zp) - 0.4e1 / (z - zp) * log(z) * log(zp) * log(z - zp) - 0.4e1 / (z - zp) * log(0.1e1 - z) * log(0.1e1 - zp) * log(z - zp) + 0.2e1 / (z - zp) * log(0.1e1 - z) * log(0.1e1 - zp) * log(zp) - 0.2e1 / (z - zp) * log(z) * log(zp) * log(0.1e1 - z) - 0.6e1 / (z - zp) * log(0.1e1 - z) * log(0.1e1 - zp) * log(z) - 0.2e1 / (z - zp) * log(z) * log(zp) * log(0.1e1 - zp) + 0.3e1 / 0.2e1 / (z - zp) * log(q3) * log((z - 0.1e1) * (-0.1e1 + zp)) * log(z) - 0.3e1 / 0.2e1 / (z - zp) * log(q3) * log((z - 0.1e1) * (-0.1e1 + zp)) * log(zp) + 0.3e1 / 0.2e1 / (z - zp) * log(q4) * log((z - 0.1e1) * (-0.1e1 + zp)) * log(z) - 0.3e1 / 0.2e1 / (z - zp) * log(q4) * log((z - 0.1e1) * (-0.1e1 + zp)) * log(zp) + 0.3e1 / (z - zp) * pow(log(0.1e1 - zp), 0.2e1) * log(zp) - 0.1e1 / (z - zp) * log(z) * pow(log(0.1e1 - z), 0.2e1) + 0.3e1 / (z - zp) * pow(log(zp), 0.2e1) * log(0.1e1 - zp) - 0.1e1 / (z - zp) * log(0.1e1 - z) * pow(log(z), 0.2e1) + 0.3e1 / (z - zp) * log(z) * pow(log(0.1e1 - zp), 0.2e1) + 0.3e1 / (z - zp) * pow(log(z), 0.2e1) * log(0.1e1 - zp) + 0.2e1 / (z - zp) * pow(log(z), 0.2e1) * log(zp) + 0.2e1 / (z - zp) * pow(log(z), 0.2e1) * log(z - zp) - 0.1e1 / (z - zp) * log(zp) * pow(log(0.1e1 - z), 0.2e1) - 0.1e1 / (z - zp) * pow(log(zp), 0.2e1) * log(0.1e1 - z) + 0.2e1 / (z - zp) * pow(log(zp), 0.2e1) * log(z - zp) + 0.2e1 / (z - zp) * log(0.1e1 - z) * pow(log(0.1e1 - zp), 0.2e1) + 0.2e1 / (z - zp) * pow(log(0.1e1 - z), 0.2e1) * log(z - zp) + 0.2e1 / (z - zp) * pow(log(0.1e1 - zp), 0.2e1) * log(z - zp) - 0.4e1 / (z - zp) * polylog(2, 0.1e1 - z) * log(0.1e1 - z) - 0.4e1 / (z - zp) * polylog(2, 0.1e1 - z) * log(z) - 0.4e1 / (z - zp) * polylog(2, 0.1e1 - z) * log(zp) - 0.4e1 / (z - zp) * polylog(2, 0.1e1 - z) * log(0.1e1 - zp) + 0.4e1 / (z - zp) * polylog(2, 0.1e1 - zp) * log(z) + 0.4e1 / (z - zp) * polylog(2, 0.1e1 - zp) * log(zp) + 0.4e1 / (z - zp) * polylog(2, 0.1e1 - zp) * log(0.1e1 - z) + 0.4e1 / (z - zp) * polylog(2, 0.1e1 - zp) * log(0.1e1 - zp) + 0.4e1 / (z - zp) * polylog(2, zp / z) * log(0.1e1 - zp) + 0.4e1 / (z - zp) * polylog(2, zp / z) * log(0.1e1 - z) - 0.3e1 / (z - zp) * log(q3) * polylog(2, 0.1e1 - zp) + 0.3e1 / (z - zp) * log(q3) * polylog(2, 0.1e1 - z) - 0.3e1 / (z - zp) * log(q4) * polylog(2, 0.1e1 - zp) + 0.3e1 / (z - zp) * log(q4) * polylog(2, 0.1e1 - z) + 0.2e1 / (z - zp) * log((z - 0.1e1) * (-0.1e1 + zp)) * log(zp) - 0.2e1 / (z - zp) * log((z - 0.1e1) * (-0.1e1 + zp)) * log(z));
}
int I10m3 (
           const vector<double>& kk)
{
    return(0);
}
int I10m2 (
           const vector<double>& kk)
{
    return(0);
}


double I10m1 (
              const vector<double>& kk)
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double u = kk[2];
    const double t = kk[3];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    return(0.1e1 / (z - zp) * log(z * zp) * log(0.1e1 - z) - 0.1e1 / (z - zp) * log(z * zp) * log(0.1e1 - zp) + 0.2e1 / (z - zp) * polylog(2, z) - 0.2e1 / (z - zp) * polylog(2, zp));
}


double I100 (
             const vector<double>& kk)
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double u = kk[2];
    const double t = kk[3];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    return(-0.8e1 / (z - zp) * consts::z3 + 0.4e1 / (z - zp) * polylog(3, zp / z) - 0.4e1 / 0.3e1 / (z - zp) * pow(log(z), 0.3e1) - 0.2e1 / 0.3e1 / (z - zp) * pow(log(zp), 0.3e1) - 0.2e1 / 0.3e1 / (z - zp) * pow(log(0.1e1 - z), 0.3e1) - 0.4e1 / 0.3e1 / (z - zp) * pow(log(0.1e1 - zp), 0.3e1) + 0.4e1 / (z - zp) * polylog(3, 0.1e1 / (z - 0.1e1) / zp * z - 0.1e1 / (z - 0.1e1)) + 0.8e1 / (z - zp) * polylog(3, -0.1e1 / (-0.1e1 + zp) * z + 0.1e1 / (-0.1e1 + zp) * zp) - 0.4e1 / (z - zp) * polylog(3, -0.1e1 / (-0.1e1 + zp) + 0.1e1 / (-0.1e1 + zp) / z * zp) + 0.4e1 / (z - zp) * polylog(3, 0.1e1 / (-0.1e1 + zp) * z - 0.1e1 / (-0.1e1 + zp)) + 0.8e1 / (z - zp) * polylog(3, 0.1e1 - zp / z) + 0.1e1 / (z - zp) * QQoc(z, zp, -t) + 0.4e1 / (z - zp) * polylog(2, 0.1e1 / (-0.1e1 + zp) * z - 0.1e1 / (-0.1e1 + zp)) * log(z) + 0.4e1 / (z - zp) * polylog(2, 0.1e1 / (-0.1e1 + zp) * z - 0.1e1 / (-0.1e1 + zp)) * log(zp) - 0.4e1 / 0.3e1 / (z - zp) * consts::pi_square * log(zp) - 0.4e1 / 0.3e1 / (z - zp) * consts::pi_square * log(0.1e1 - z) - 0.3e1 / 0.2e1 / (z - zp) * log(q3) * log(z * zp) * log(0.1e1 - z) + 0.3e1 / 0.2e1 / (z - zp) * log(q3) * log(z * zp) * log(0.1e1 - zp) - 0.3e1 / 0.2e1 / (z - zp) * log(q4) * log(z * zp) * log(0.1e1 - z) + 0.3e1 / 0.2e1 / (z - zp) * log(q4) * log(z * zp) * log(0.1e1 - zp) + 0.8e1 / (z - zp) * log(0.1e1 - z) * log(z - zp) * log(zp) - 0.8e1 / (z - zp) * log(z) * log(0.1e1 - zp) * log(z - zp) - 0.4e1 / (z - zp) * log(z) * log(zp) * log(z - zp) - 0.4e1 / (z - zp) * log(0.1e1 - z) * log(0.1e1 - zp) * log(z - zp) - 0.2e1 / (z - zp) * log(0.1e1 - z) * log(0.1e1 - zp) * log(zp) + 0.2e1 / (z - zp) * log(z) * log(zp) * log(0.1e1 - z) - 0.2e1 / (z - zp) * log(0.1e1 - z) * log(0.1e1 - zp) * log(z) - 0.6e1 / (z - zp) * log(z) * log(zp) * log(0.1e1 - zp) - 0.1e1 / (z - zp) * pow(log(0.1e1 - zp), 0.2e1) * log(zp) + 0.3e1 / (z - zp) * log(z) * pow(log(0.1e1 - z), 0.2e1) - 0.1e1 / (z - zp) * pow(log(zp), 0.2e1) * log(0.1e1 - zp) + 0.3e1 / (z - zp) * log(0.1e1 - z) * pow(log(z), 0.2e1) + 0.3e1 / (z - zp) * log(z) * pow(log(0.1e1 - zp), 0.2e1) + 0.3e1 / (z - zp) * pow(log(z), 0.2e1) * log(0.1e1 - zp) + 0.2e1 / (z - zp) * pow(log(z), 0.2e1) * log(zp) + 0.2e1 / (z - zp) * pow(log(z), 0.2e1) * log(z - zp) - 0.1e1 / (z - zp) * log(zp) * pow(log(0.1e1 - z), 0.2e1) - 0.1e1 / (z - zp) * pow(log(zp), 0.2e1) * log(0.1e1 - z) + 0.2e1 / (z - zp) * pow(log(zp), 0.2e1) * log(z - zp) + 0.2e1 / (z - zp) * log(0.1e1 - z) * pow(log(0.1e1 - zp), 0.2e1) + 0.2e1 / (z - zp) * pow(log(0.1e1 - z), 0.2e1) * log(z - zp) + 0.2e1 / (z - zp) * pow(log(0.1e1 - zp), 0.2e1) * log(z - zp) + 0.4e1 / (z - zp) * polylog(2, zp / z) * log(0.1e1 - zp) + 0.4e1 / (z - zp) * polylog(2, zp / z) * log(0.1e1 - z) + 0.4e1 / (z - zp) * polylog(2, z) - 0.4e1 / (z - zp) * polylog(2, zp) - 0.2e1 / (z - zp) * log(z * zp) * log(0.1e1 - zp) + 0.2e1 / (z - zp) * log(z * zp) * log(0.1e1 - z) - 0.3e1 / (z - zp) * log(q3) * polylog(2, z) + 0.3e1 / (z - zp) * log(q3) * polylog(2, zp) - 0.3e1 / (z - zp) * log(q4) * polylog(2, z) + 0.3e1 / (z - zp) * log(q4) * polylog(2, zp) - 0.4e1 / (z - zp) * polylog(2, zp) * log(0.1e1 - z) - 0.4e1 / (z - zp) * polylog(2, zp) * log(z) - 0.4e1 / (z - zp) * polylog(2, zp) * log(zp) - 0.4e1 / (z - zp) * polylog(2, zp) * log(0.1e1 - zp) + 0.4e1 / (z - zp) * polylog(2, z) * log(z) + 0.4e1 / (z - zp) * polylog(2, z) * log(zp) + 0.4e1 / (z - zp) * polylog(2, z) * log(0.1e1 - z) + 0.4e1 / (z - zp) * polylog(2, z) * log(0.1e1 - zp));
}
double I11m3 (
              const vector<double>& kk)
{
    const double u = kk[2];
      return(-0.1e1 / u / 0.4e1);
}


double I11m2 (
              const vector<double>& kk)
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double u = kk[2];
    const double t = kk[3];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    return(-0.1e1 / u * log(q3) / 0.2e1 - 0.1e1 / u * log(q4) / 0.2e1 + 0.1e1 / u * log(-u));
}


double I11m1 (
              const vector<double>& kk)
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double u = kk[2];
    const double t = kk[3];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    return(-0.1e1 / u * consts::pi_square / 0.6e1 + 0.1e1 / u * pow(log(q3), 0.2e1) / 0.4e1 - 0.1e1 / u * log(q3) * log(q4) + 0.1e1 / u * log(q3) * log(-u) / 0.2e1 + 0.1e1 / u * pow(log(q4), 0.2e1) / 0.4e1 + 0.1e1 / u * log(q4) * log(-u) / 0.2e1 - 0.2e1 / u * pow(log(-u), 0.2e1) + 0.3e1 / 0.2e1 / u * log(q4 - u) * log(-u) + 0.3e1 / 0.2e1 / u * log(-u) * log(q3 - u) - 0.3e1 / 0.4e1 / u * pow(log(q4 - u), 0.2e1) - 0.3e1 / 0.4e1 / u * pow(log(q3 - u), 0.2e1) - 0.3e1 / 0.2e1 / u * polylog(2, -u / (q3 - u)) - 0.3e1 / 0.2e1 / u * polylog(2, -u / (q4 - u)));
}


double I110 (
             const vector<double>& kk)
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double u = kk[2];
    const double t = kk[3];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    return-(0.2e1 * pow(log(q4), 0.3e1) + 0.2e1 * pow(log(q3), 0.3e1) - (double) (72 * QQec(1 - zp, 1 - z, -u)) - 0.64e2 * pow(log(-u), 0.3e1) + 0.78e2 * pow(log(q3 - u), 0.3e1) + 0.78e2 * pow(log(q4 - u), 0.3e1) + 0.32e2 * 0.3141592654e1 * 0.3141592654e1 * log(q4) + 0.272e3 * 0.3141592654e1 * 0.3141592654e1 * log(-u) + 0.216e3 * log(q3) * log(-u) * log(q3 - u) + 0.44e2 * 0.3141592654e1 * 0.3141592654e1 * log(q3) - 0.156e3 * 0.3141592654e1 * 0.3141592654e1 * log(q3 - u) - 0.156e3 * 0.3141592654e1 * 0.3141592654e1 * log(q4 - u) - 0.216e3 * polylog(2, -u / (q3 - u)) * log(q3) - 0.120e3 * pow(log(q3), 0.2e1) * log(-u) + (double) (264 * consts::z3) + 0.216e3 * log(q4) * log(q4 - u) * log(-u) + (double) (144 * CCL3( 1 - zp, 1 - z)) + (double) (144 * CCL3( zp, z)) + 0.36e2 * pow(log(-u), 0.2e1) * log(q3 - u) + 0.36e2 * pow(log(-u), 0.2e1) * log(q4 - u) - (double) (252 * polylog(3, q4 / (q4 - u))) - (double) (216 * polylog(3, -u / (q3 - u))) - (double) (252 * polylog(3, q3 / (q3 - u))) - (double) (216 * polylog(3, -u / (q4 - u))) + 0.144e3 * 0.3141592654e1 * 0.3141592654e1 * log(-(q4 - u) / u) - 0.96e2 * log(q4) * log(q3) * log(-u) + 0.72e2 * log(q3) * log(-u) * log(q4 - u) + 0.72e2 * log(q4) * log(-u) * log(q3 - u) - 0.216e3 * polylog(2, -u / (q4 - u)) * log(q4) - 0.120e3 * pow(log(q4), 0.2e1) * log(-u) + 0.72e2 * polylog(2, -u / (q3 - u)) * log(-u) + 0.126e3 * pow(log(q3), 0.2e1) * log(q3 - u) - 0.234e3 * log(q3) * pow(log(q3 - u), 0.2e1) + 0.24e2 * log(q3) * pow(log(-u), 0.2e1) + 0.24e2 * log(q4) * pow(log(-u), 0.2e1) + 0.72e2 * polylog(2, -u / (q4 - u)) * log(-u) - 0.234e3 * log(q4) * pow(log(q4 - u), 0.2e1) + 0.126e3 * pow(log(q4), 0.2e1) * log(q4 - u) + 0.144e3 * 0.3141592654e1 * 0.3141592654e1 * log(-(q3 - u) / u) - 0.27e2 * pow(log(q3), 0.2e1) * log(q4) - 0.36e2 * log(q3) * pow(log(q4 - u), 0.2e1) - 0.36e2 * log(q4) * pow(log(q3 - u), 0.2e1) - 0.72e2 * log(-u) * pow(log(q4 - u), 0.2e1) - 0.72e2 * log(-u) * pow(log(q3 - u), 0.2e1) - 0.72e2 * polylog(2, -u / (q4 - u)) * log(q3) - 0.72e2 * polylog(2, -u / (q3 - u)) * log(q4)) / u / 0.24e2;
}
double I12m3 (
              const vector<double>& kk)
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double u = kk[2];
    const double t = kk[3];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    return(-0.1e1 / t / 0.4e1);
}


double I12m2 (
              const vector<double>& kk)
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double u = kk[2];
    const double t = kk[3];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    return(-0.1e1 / t * log(q3) / 0.2e1 - 0.1e1 / t * log(q4) / 0.2e1 + 0.1e1 / t * log(-t));
}


double I12m1 (
              const vector<double>& kk)
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double u = kk[2];
    const double t = kk[3];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    return(-0.1e1 / t * consts::pi_square / 0.6e1 + 0.1e1 / t * pow(log(q3), 0.2e1) / 0.4e1 - 0.1e1 / t * log(q3) * log(q4) + 0.1e1 / t * log(q3) * log(-t) / 0.2e1 + 0.1e1 / t * pow(log(q4), 0.2e1) / 0.4e1 + 0.1e1 / t * log(q4) * log(-t) / 0.2e1 - 0.2e1 / t * pow(log(-t), 0.2e1) + 0.3e1 / 0.2e1 / t * log(q3 - t) * log(-t) + 0.3e1 / 0.2e1 / t * log(-t) * log(q4 - t) - 0.3e1 / 0.4e1 / t * pow(log(q3 - t), 0.2e1) - 0.3e1 / 0.4e1 / t * pow(log(q4 - t), 0.2e1) - 0.3e1 / 0.2e1 / t * polylog(2, -t / (q4 - t)) - 0.3e1 / 0.2e1 / t * polylog(2, -t / (q3 - t)));
}


double I120 (
             const vector<double>& kk)
{
    const double q3=kk[0];
    const double q4=kk[1];
    const double u = kk[2];
    const double t = kk[3];
    const double z = 0.5* (q3-q4+1.0 
                           + sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    const double zp = 0.5* (q3-q4+1.0 
                            - sqrt(1.0+q3*q3+q4*q4-2.0*q3-2.0*q4-2.0*q3*q4));
    return(-0.9e1 / t * log(q3) * log(q3 - t) * log(-t) - 0.3e1 / t * log(q3) * log(-t) * log(q4 - t) - 0.3e1 / t * log(q4) * log(q3 - t) * log(-t) - 0.9e1 / t * log(q4) * log(-t) * log(q4 - t) + 0.4e1 / t * log(q3) * log(q4) * log(-t) + 0.39e2 / 0.4e1 / t * log(q3) * pow(log(q3 - t), 0.2e1) + 0.5e1 / t * pow(log(q4), 0.2e1) * log(-t) - 0.21e2 / 0.4e1 / t * pow(log(q4), 0.2e1) * log(q4 - t) + 0.5e1 / t * pow(log(q3), 0.2e1) * log(-t) - 0.21e2 / 0.4e1 / t * pow(log(q3), 0.2e1) * log(q3 - t) - 0.3e1 / t * polylog(2, -t / (q3 - t)) * log(-t) + 0.3e1 / t * polylog(2, -t / (q3 - t)) * log(q4) + 0.9e1 / t * polylog(2, -t / (q3 - t)) * log(q3) + 0.3e1 / t * polylog(2, -t / (q4 - t)) * log(q3) + 0.9e1 / t * polylog(2, -t / (q4 - t)) * log(q4) - 0.3e1 / t * polylog(2, -t / (q4 - t)) * log(-t) - 0.4e1 / 0.3e1 / t * consts::pi_square * log(q3) + 0.13e2 / 0.2e1 / t * consts::pi_square * log(q4 - t) + 0.13e2 / 0.2e1 / t * consts::pi_square * log(q3 - t) - 0.11e2 / 0.6e1 / t * consts::pi_square * log(q4) - 0.34e2 / 0.3e1 / t * consts::pi_square * log(-t) - 0.6e1 / t * consts::pi_square * log((-q4 + t) / t) - 0.6e1 / t * consts::pi_square * log((t - q3) / t) + 0.9e1 / 0.8e1 / t * pow(log(q4), 0.2e1) * log(q3) - 0.1e1 / t * log(q4) * pow(log(-t), 0.2e1) - 0.1e1 / t * log(q3) * pow(log(-t), 0.2e1) + 0.3e1 / t * log(-t) * pow(log(q3 - t), 0.2e1) + 0.3e1 / t * log(-t) * pow(log(q4 - t), 0.2e1) + 0.3e1 / 0.2e1 / t * log(q4) * pow(log(q3 - t), 0.2e1) + 0.39e2 / 0.4e1 / t * log(q4) * pow(log(q4 - t), 0.2e1) - 0.3e1 / 0.2e1 / t * pow(log(-t), 0.2e1) * log(q4 - t) - 0.3e1 / 0.2e1 / t * pow(log(-t), 0.2e1) * log(q3 - t) + 0.3e1 / 0.2e1 / t * log(q3) * pow(log(q4 - t), 0.2e1) - 0.13e2 / 0.4e1 / t * pow(log(q4 - t), 0.3e1) - 0.1e1 / t * pow(log(q4), 0.3e1) / 0.12e2 - 0.1e1 / t * pow(log(q3), 0.3e1) / 0.12e2 + 0.8e1 / 0.3e1 / t * pow(log(-t), 0.3e1) + 0.3e1 / t * QQec(z, zp, -t) - 0.6e1 / t * CCL3( z, zp) - 0.6e1 / t * CCL3( 0.1e1 - z, 0.1e1 - zp) + 0.21e2 / 0.2e1 / t * polylog(3, q3 / (q3 - t)) + 0.21e2 / 0.2e1 / t * polylog(3, q4 / (q4 - t)) + 0.9e1 / t * polylog(3, -t / (q3 - t)) + 0.9e1 / t * polylog(3, -t / (q4 - t)) - 0.13e2 / 0.4e1 / t * pow(log(q3 - t), 0.3e1) - 0.11e2 / t * consts::z3);
}
  
        

// {q3,q4,u,t}
double VirtualAmplitude::Evaluate(const KinematicInvariants& kv) const 
{
    vector<double> kk;
    kk.push_back(kv.q(3));
    kk.push_back(kv.q(4));
    kk.push_back(kv.q(1,4));
    kk.push_back(kv.q(1,3));
    const double res = e0_(kk) -    consts::pi_square/4.0 *  em2_(kk);
    return res;
}

double VirtualAmplitude::Epsilon0(const KinematicInvariants& kv)const
{
    vector<double> kk;
    kk.push_back(kv.q(3));
    kk.push_back(kv.q(4));
    kk.push_back(kv.q(1,4));
    kk.push_back(kv.q(1,3));
    
    const double res = e0_(kk)  ;
    return res;
}

double VirtualAmplitude::EpsilonM1(const KinematicInvariants& kv)const
{
    vector<double> kk;
    kk.push_back(kv.q(3));
    kk.push_back(kv.q(4));
    kk.push_back(kv.q(1,4));
    kk.push_back(kv.q(1,3));

    const double res = em1_(kk)  ;
    return res;
}

double VirtualAmplitude::EpsilonM2(const KinematicInvariants& kv)const
{
    vector<double> kk;
    kk.push_back(kv.q(3));
    kk.push_back(kv.q(4));
    kk.push_back(kv.q(1,4));
    kk.push_back(kv.q(1,3));
    
    const double res = em2_(kk) ;
    return res;
}

double VirtualAmplitude::EpsilonM3(const KinematicInvariants& kv)const
{
    vector<double> kk;
    kk.push_back(kv.q(3));
    kk.push_back(kv.q(4));
    kk.push_back(kv.q(1,4));
    kk.push_back(kv.q(1,3));
    
    const double res = em3_(kk) ;
    return res;
}
double VirtualAmplitude::EpsilonP1(const KinematicInvariants& kv)const
{
    vector<double> kk;
    kk.push_back(kv.q(3));
    kk.push_back(kv.q(4));
    kk.push_back(kv.q(1,4));
    kk.push_back(kv.q(1,3));

    const double res = ep1_(kk);
    return res;
}

PolynomialEpsilon VirtualAmplitude::Master(int i,int eps)
{
    PolynomialEpsilon res = II[i-1].EpsilonCoefficient(eps);
    return res;
}



double VirtualAmplitude::Master(int i,const KinematicInvariants& kv,int eps)const
{
    vector<double> kk;
    kk.push_back(kv.q(3));
    kk.push_back(kv.q(4));
    kk.push_back(kv.q(1,4));
    kk.push_back(kv.q(1,3));
    PolynomialEpsilon res = II[i-1].EpsilonCoefficient(eps);
    return res(kk);
}

double VirtualAmplitude::Coefficient(int i,const KinematicInvariants& kv,int eps) const
{
    vector<double> kk;
    kk.push_back(kv.q(3));
    kk.push_back(kv.q(4));
    kk.push_back(kv.q(1,4));
    kk.push_back(kv.q(1,3));
    PolynomialEpsilon res = cc[i-1].EpsilonCoefficient(eps);
    return res(kk);
}
#include "constants.h"
#include "math.h"
#include "one_loop_virtual_from_maple.cpp"

#include "two_loop_coefficients_from_maple.cpp"

GstarVirtual::GstarVirtual()
{
                                 
    
    for (int i=0;i<12;i++) cc.push_back(PolynomialEpsilon());
    
    cc[0] += MonomialEpsilon(&GstarVCoeffsC1em1,-1);
    cc[0] += MonomialEpsilon(&GstarVCoeffsC1e0 , 0);
    cc[0] += MonomialEpsilon(&GstarVCoeffsC1e1 , 1);
    cc[0] += MonomialEpsilon(&GstarVCoeffsC1e2 , 2);
    //cout<<"\n"<<cc[0]<<endl;
    //cout<<"\n"<<cc[0]*cc[0]<<endl;
    cc[1] += MonomialEpsilon(&GstarVCoeffsC2em1,-1);
    cc[1] += MonomialEpsilon(&GstarVCoeffsC2e0 , 0);
    cc[1] += MonomialEpsilon(&GstarVCoeffsC2e1 , 1);
    cc[1] += MonomialEpsilon(&GstarVCoeffsC2e2 , 2);
    
    cc[2] += MonomialEpsilon(&GstarVCoeffsC3em1,-1);
    cc[2] += MonomialEpsilon(&GstarVCoeffsC3e0 , 0);
    cc[2] += MonomialEpsilon(&GstarVCoeffsC3e1 , 1);
    cc[2] += MonomialEpsilon(&GstarVCoeffsC3e2 , 2);
    
    cc[3] += MonomialEpsilon(&GstarVCoeffsC4em1,-1);
    cc[3] += MonomialEpsilon(&GstarVCoeffsC4e0 , 0);
    cc[3] += MonomialEpsilon(&GstarVCoeffsC4e1 , 1);
    cc[3] += MonomialEpsilon(&GstarVCoeffsC4e2 , 2);
    
    cc[4] += MonomialEpsilon(&GstarVCoeffsC5em1,-1);
    cc[4] += MonomialEpsilon(&GstarVCoeffsC5e0 , 0);
    cc[4] += MonomialEpsilon(&GstarVCoeffsC5e1 , 1);
    cc[4] += MonomialEpsilon(&GstarVCoeffsC5e2 , 2);
    
    cc[5] += MonomialEpsilon(&GstarVCoeffsC6em1,-1);
    cc[5] += MonomialEpsilon(&GstarVCoeffsC6e0 , 0);
    cc[5] += MonomialEpsilon(&GstarVCoeffsC6e1 , 1);
    cc[5] += MonomialEpsilon(&GstarVCoeffsC6e2 , 2);
    
    cc[6] += MonomialEpsilon(&GstarVCoeffsC7em1,-1);
    cc[6] += MonomialEpsilon(&GstarVCoeffsC7e0 , 0);
    cc[6] += MonomialEpsilon(&GstarVCoeffsC7e1 , 1);
    cc[6] += MonomialEpsilon(&GstarVCoeffsC7e2 , 2);
    
    cc[7] += MonomialEpsilon(&GstarVCoeffsC8em1,-1);
    cc[7] += MonomialEpsilon(&GstarVCoeffsC8e0 , 0);
    cc[7] += MonomialEpsilon(&GstarVCoeffsC8e1 , 1);
    cc[7] += MonomialEpsilon(&GstarVCoeffsC8e2 , 2);
    
    cc[8] += MonomialEpsilon(&GstarVCoeffsC9em1,-1);
    cc[8] += MonomialEpsilon(&GstarVCoeffsC9e0 , 0);
    cc[8] += MonomialEpsilon(&GstarVCoeffsC9e1 , 1);
    
    cc[9] += MonomialEpsilon(&GstarVCoeffsC10em1,-1);
    cc[9] += MonomialEpsilon(&GstarVCoeffsC10e0 , 0);
    cc[9] += MonomialEpsilon(&GstarVCoeffsC10e1 , 1);
    
    cc[10] += MonomialEpsilon(&GstarVCoeffsC11em1,-1);
    cc[10] += MonomialEpsilon(&GstarVCoeffsC11e0 , 0);
    cc[10] += MonomialEpsilon(&GstarVCoeffsC11e1 , 1);
    cc[10] += MonomialEpsilon(&GstarVCoeffsC11e2 , 2);
    cc[10] += MonomialEpsilon(&GstarVCoeffsC11e3 , 3);

    cc[11] += MonomialEpsilon(&GstarVCoeffsC12em1,-1);
    cc[11] += MonomialEpsilon(&GstarVCoeffsC12e0 , 0);
    cc[11] += MonomialEpsilon(&GstarVCoeffsC12e1 , 1);
    cc[11] += MonomialEpsilon(&GstarVCoeffsC12e2 , 2);
    cc[11] += MonomialEpsilon(&GstarVCoeffsC12e3 , 3);

    
    for (int i=0;i<12;i++) II.push_back(PolynomialEpsilon());
    
    II[0] += MonomialEpsilon(&TP0011_123_m1,-1);
    II[0] += MonomialEpsilon(&TP0011_123_f , 0);
    II[0] += MonomialEpsilon(&TP0011_123_p1, 1);
    II[0] += MonomialEpsilon(&TP0011_123_p2, 2);

    II[1] += MonomialEpsilon(&TP0011_124_m1,-1);
    II[1] += MonomialEpsilon(&TP0011_124_f , 0);
    II[1] += MonomialEpsilon(&TP0011_124_p1, 1);
    II[1] += MonomialEpsilon(&TP0011_124_p2, 2);
    
    II[2] += MonomialEpsilon(&TP0101_123_m1,-1);
    II[2] += MonomialEpsilon(&TP0101_123_f , 0);
    II[2] += MonomialEpsilon(&TP0101_123_p1, 1);
    II[2] += MonomialEpsilon(&TP0101_123_p2, 2);
    
    II[3] += MonomialEpsilon(&TP0101_124_m1,-1);
    II[3] += MonomialEpsilon(&TP0101_124_f , 0);
    II[3] += MonomialEpsilon(&TP0101_124_p1, 1);
    II[3] += MonomialEpsilon(&TP0101_124_p2, 2);
    
    II[4] += MonomialEpsilon(&TP1001_123_m1,-1);
    II[4] += MonomialEpsilon(&TP1001_123_f , 0);
    II[4] += MonomialEpsilon(&TP1001_123_p1, 1);
    II[4] += MonomialEpsilon(&TP1001_123_p2, 2);
    
    II[5] += MonomialEpsilon(&TP1001_124_m1,-1);
    II[5] += MonomialEpsilon(&TP1001_124_f , 0);
    II[5] += MonomialEpsilon(&TP1001_124_p1, 1);
    II[5] += MonomialEpsilon(&TP1001_124_p2, 2);
    
    II[6] += MonomialEpsilon(&TP1010_123_m1,-1);
    II[6] += MonomialEpsilon(&TP1010_123_f , 0);
    II[6] += MonomialEpsilon(&TP1010_123_p1, 1);
    II[6] += MonomialEpsilon(&TP1010_123_p2, 2);
    
    II[7] += MonomialEpsilon(&TP1010_124_m1,-1);
    II[7] += MonomialEpsilon(&TP1010_124_f , 0);
    II[7] += MonomialEpsilon(&TP1010_124_p1, 1);
    II[7] += MonomialEpsilon(&TP1010_124_p2, 2);
    
    II[8] += MonomialEpsilon(&TP1011_123_f , 0);
    II[8] += MonomialEpsilon(&TP1011_123_p1 , 1);

    II[9] += MonomialEpsilon(&TP1011_124_f , 0);
    II[9] += MonomialEpsilon(&TP1011_124_p1 , 1);

    II[10] += MonomialEpsilon(&TP1111_123_m2,-2);
    II[10] += MonomialEpsilon(&TP1111_123_m1,-1);
    II[10] += MonomialEpsilon(&TP1111_123_f , 0);
    II[10] += MonomialEpsilon(&TP1111_123_p1 , 1);

    II[11] += MonomialEpsilon(&TP1111_124_m2,-2);
    II[11] += MonomialEpsilon(&TP1111_124_m1,-1);
    II[11] += MonomialEpsilon(&TP1111_124_f , 0);
    II[11] += MonomialEpsilon(&TP1111_124_p1 , 1);

    PolynomialEpsilon res;
    for (int i=0;i<12;i++) res += cc[i]*II[i];
    
    em2_ = res.EpsilonCoefficient(-2);
    em1_ = res.EpsilonCoefficient(-1);
    e0_  = res.EpsilonCoefficient(0);
    ep1_  = res.EpsilonCoefficient(1);
    
     
    
    

}



GstarVirtualVirtual::GstarVirtualVirtual()
{
    
    
    for (int i=0;i<12;i++) cc.push_back(PolynomialEpsilon());

    cc[0] += MonomialEpsilon(&GstarVVCoeffsC1em2,-2);
    cc[0] += MonomialEpsilon(&GstarVVCoeffsC1em1,-1);
    cc[0] += MonomialEpsilon(&GstarVVCoeffsC1e0 , 0);
    cc[0] += MonomialEpsilon(&GstarVVCoeffsC1e1 , 1);
    //cout<<"\n"<<cc[0]<<endl;
    //cout<<"\n"<<cc[0]*cc[0]<<endl;
    cc[1] += MonomialEpsilon(&GstarVVCoeffsC2em2,-2);
    cc[1] += MonomialEpsilon(&GstarVVCoeffsC2em1,-1);
    cc[1] += MonomialEpsilon(&GstarVVCoeffsC2e0 , 0);
    cc[1] += MonomialEpsilon(&GstarVVCoeffsC2e1 , 1);
    
    cc[2] += MonomialEpsilon(&GstarVVCoeffsC3em1,-1);
    cc[2] += MonomialEpsilon(&GstarVVCoeffsC3e0 , 0);
    cc[2] += MonomialEpsilon(&GstarVVCoeffsC3e1 , 1);
    cc[2] += MonomialEpsilon(&GstarVVCoeffsC3e2 , 2);

    cc[3] += MonomialEpsilon(&GstarVVCoeffsC4em1,-1);
    cc[3] += MonomialEpsilon(&GstarVVCoeffsC4e0 , 0);
    cc[3] += MonomialEpsilon(&GstarVVCoeffsC4e1 , 1);
    cc[3] += MonomialEpsilon(&GstarVVCoeffsC4e2 , 2);
    
    cc[4] += MonomialEpsilon(&GstarVVCoeffsC5em1,-1);
    cc[4] += MonomialEpsilon(&GstarVVCoeffsC5e0 , 0);
    cc[4] += MonomialEpsilon(&GstarVVCoeffsC5e1 , 1);
    cc[4] += MonomialEpsilon(&GstarVVCoeffsC5e2 , 2);
    
    cc[5] += MonomialEpsilon(&GstarVVCoeffsC6em1,-1);
    cc[5] += MonomialEpsilon(&GstarVVCoeffsC6e0 , 0);
    cc[5] += MonomialEpsilon(&GstarVVCoeffsC6e1 , 1);
    cc[5] += MonomialEpsilon(&GstarVVCoeffsC6e2 , 2);
    
    cc[6] += MonomialEpsilon(&GstarVVCoeffsC7em1,-1);
    cc[6] += MonomialEpsilon(&GstarVVCoeffsC7e0 , 0);
    cc[6] += MonomialEpsilon(&GstarVVCoeffsC7e1 , 1);
    cc[6] += MonomialEpsilon(&GstarVVCoeffsC7e2 , 2);
    
    cc[7] += MonomialEpsilon(&GstarVVCoeffsC8em1,-1);
    cc[7] += MonomialEpsilon(&GstarVVCoeffsC8e0 , 0);
    cc[7] += MonomialEpsilon(&GstarVVCoeffsC8e1 , 1);
    cc[7] += MonomialEpsilon(&GstarVVCoeffsC8e2 , 2);
    
    cc[8] += MonomialEpsilon(&GstarVVCoeffsC9em1,-1);
    cc[8] += MonomialEpsilon(&GstarVVCoeffsC9e0 , 0);
    cc[8] += MonomialEpsilon(&GstarVVCoeffsC9e1 , 1);
    
    cc[9] += MonomialEpsilon(&GstarVVCoeffsC10em1,-1);
    cc[9] += MonomialEpsilon(&GstarVVCoeffsC10e0 , 0);
    cc[9] += MonomialEpsilon(&GstarVVCoeffsC10e1 , 1);
    
    cc[10] += MonomialEpsilon(&GstarVVCoeffsC11em1,-1);
    cc[10] += MonomialEpsilon(&GstarVVCoeffsC11e0 , 0);
    cc[10] += MonomialEpsilon(&GstarVVCoeffsC11e1 , 1);
    cc[10] += MonomialEpsilon(&GstarVVCoeffsC11e2 , 2);
    cc[10] += MonomialEpsilon(&GstarVVCoeffsC11e3 , 3);
    
    cc[11] += MonomialEpsilon(&GstarVVCoeffsC12em1,-1);
    cc[11] += MonomialEpsilon(&GstarVVCoeffsC12e0 , 0);
    cc[11] += MonomialEpsilon(&GstarVVCoeffsC12e1 , 1);
    cc[11] += MonomialEpsilon(&GstarVVCoeffsC12e2 , 2);
    cc[11] += MonomialEpsilon(&GstarVVCoeffsC12e3 , 3);
    
    
    for (int i=0;i<12;i++) II.push_back(PolynomialEpsilon());
    
    II[0] += MonomialEpsilon(&I1m1,-1);
    II[0] += MonomialEpsilon(&I10,0);
    II[0] += MonomialEpsilon(&I11,1);
    II[0] += MonomialEpsilon(&I12,2);

    II[1] += MonomialEpsilon(&I2m1,-1);
    II[1] += MonomialEpsilon(&I20,0);
    II[1] += MonomialEpsilon(&I21,1);
    II[1] += MonomialEpsilon(&I22,2);
        
    II[2] += MonomialEpsilon(&I3m2,-2);
    II[2] += MonomialEpsilon(&I3m1,-1);
    II[2] += MonomialEpsilon(&I30,0);
    II[2] += MonomialEpsilon(&I31,1);
 
    II[3] += MonomialEpsilon(&I4m2,-2);
    II[3] += MonomialEpsilon(&I4m1,-1);
    II[3] += MonomialEpsilon(&I40,0);
    II[3] += MonomialEpsilon(&I41,1);
    
    II[4] += MonomialEpsilon(&I5m2,-2);
    II[4] += MonomialEpsilon(&I5m1,-1);
    II[4] += MonomialEpsilon(&I50,0);
    II[4] += MonomialEpsilon(&I51,1);   
    
    II[5] += MonomialEpsilon(&I6m2,-2);
    II[5] += MonomialEpsilon(&I6m1,-1);
    II[5] += MonomialEpsilon(&I60,0);
    II[5] += MonomialEpsilon(&I61,1);
    
    II[6] += MonomialEpsilon(&I7m2,-2);
    II[6] += MonomialEpsilon(&I7m1,-1);
    II[6] += MonomialEpsilon(&I70,0);
    II[6] += MonomialEpsilon(&I71,1);
    
    II[7] += MonomialEpsilon(&I8m2,-2);
    II[7] += MonomialEpsilon(&I8m1,-1);
    II[7] += MonomialEpsilon(&I80,0);
    II[7] += MonomialEpsilon(&I81,1);
    
    II[8] += MonomialEpsilon(&I9m1,-1);
    II[8] += MonomialEpsilon(&I90,0);
    
    II[9] += MonomialEpsilon(&I10m1,-1);
    II[9] += MonomialEpsilon(&I100,0);
    
    II[10] += MonomialEpsilon(&I11m3,-3);
    II[10] += MonomialEpsilon(&I11m2,-2);
    II[10] += MonomialEpsilon(&I11m1,-1);
    II[10] += MonomialEpsilon(&I110,0);

    II[11] += MonomialEpsilon(&I12m3,-3);
    II[11] += MonomialEpsilon(&I12m2,-2);
    II[11] += MonomialEpsilon(&I12m1,-1);
    II[11] += MonomialEpsilon(&I120,0);
    
    PolynomialEpsilon res;
    for (int i=0;i<12;i++) res += cc[i]*II[i];
    
    em3_ = res.EpsilonCoefficient(-3);
    em2_ = res.EpsilonCoefficient(-2);
    em1_ = res.EpsilonCoefficient(-1);
    e0_  = res.EpsilonCoefficient(0);
    
    
    
    
    
}























