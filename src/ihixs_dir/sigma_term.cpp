#include "sigma_term.h"
#include "iomanip"
using namespace std;

void SigmaTerm::wc_expansion(const WilsonCoefficient& wc)
{
    vector<double> res;
    res.push_back(wc.w(0)*_val[0]);
    res.push_back(wc.w(1)*_val[0] + wc.w(0)*_val[1]);
    res.push_back(wc.w(2)*_val[0] + wc.w(1)*_val[1] + wc.w(0)*_val[2]);
    res.push_back(wc.w(3)*_val[0] + wc.w(2)*_val[1] + wc.w(1)*_val[2] + wc.w(0)*_val[3]);
    
    _val = res;
}

void SigmaTerm::ConfigureLumi(NewLuminosity* lumi,const double& tau)
{
    _lumi_int->Configure(lumi,tau);
}

void SigmaTerm::CallVegas()
{
    _lumi_int->call_vegas();
    for (int i=0;i<_val.size();i++)
    {
        _val[i] = _val[i] * _lumi_int->result();
        _err[i] = _err[i] * _lumi_int->error();
    }

}

double SigmaTerm::operator[](int i)
{
    return _val[i];
}

void SigmaTerm::multiply(const double& c)
{
    for (int i=0;i<_val.size();i++) _val[i]= c* _val[i];
    for (int i=0;i<_err.size();i++) _err[i]= c* _err[i];
}

void SigmaTerm::multiply_by_as_pi(const double& as_pi)
{
    for (int i=0;i<_val.size();i++)
    {
        _val[i]= pow(as_pi,i+2.)* _val[i];
        _err[i]= pow(as_pi,i+2.)* _err[i];
    }
}

string SigmaTerm::type()
{
    return _type;
}


ostream& pretty_print_val_and_err(ostream& stream,const double& val,const double& err)
{
    stream<<setw(12)<<setprecision(6)<<val<<setw(6)<<setprecision(0)
        <<scientific<<"("<<err<<")"<<fixed;
    return stream;
}

ostream& operator<<(ostream& stream, const SigmaTerm& st)
{
    stream<<setw(22)<<left<<st._type;
    stream<<pretty_print_val_and_err(stream,st._val[0],st._err[0]);
    stream<<pretty_print_val_and_err(stream,st._val[1],st._err[1]);
    stream<<pretty_print_val_and_err(stream,st._val[2],st._err[2]);
    stream<<pretty_print_val_and_err(stream,st._val[3],st._err[3]);

    
    
    stream<<endl;
    return stream;
}

