#include "sigma_term.h"
#include "iomanip"
using namespace std;



void SigmaTerm::ConfigureLumi(NewLuminosity* lumi,const double& tau,const UserInterface& UI)
{
    _lumi_int->Configure(lumi,tau,UI);
}

void SigmaTerm::CallVegas()
{
    if (not(_evaluated))
    {
        cout<<"--> computing "<< type()<<endl;
        _lumi_int->call_vegas();
        _result = _result * ResultPair( _lumi_int->result(),
                                       _lumi_int->error());
        
        _evaluated = true;
        _post_vegas_result = _result;
    }
}
//
//double SigmaTerm::operator[](int i)
//{
//    return _result_central[i].val();
//}
//
//double SigmaTerm::mc_error(int i)
//{
//    return _result_central[i].err();
//}

void SigmaTerm::multiply(const double& c)
{
    _result = _result * c;

}

void SigmaTerm::wc_expansion(const WilsonCoefficient& wc)
{
    AsSeries wc_series(2,wc.w(0),wc.w(1),wc.w(2),wc.w(3));
    _result = _result * wc_series;
    _result.Truncate(5);
}

void SigmaTerm::multiply_by_as_pi(const double& as_pi)
{
    _result.MultiplyAs(as_pi);
}

string SigmaTerm::type()
{
    return _type;
}




void SigmaTerm::evolve_from_muf_to_mur(const double& L)
{
    const double b0 = consts::beta_zero;
    const double b1 = consts::beta_one;
    const double b2 = consts::beta_two;

    AsSeries as_mur(1,1.0,L*b0,pow(L*b0,2.)+L*b1,pow(L*b0,3.)+2.*L*L*b0*b1+L*b2);
    _result.MultiplyBySeries(as_mur);
    _result.Truncate(5);
}





ostream& operator<<(ostream& stream, const SigmaTerm& st)
{
    stream<<setw(22)<<st._type<<st._result_central<<endl;
    return stream;
}






string SigmaTerm::print_scale_result(const double& mur,int porder)
{
    ostringstream stream;
    for (int i=0;i<_saved_results.size();i++)
    {
        if (_saved_results[i].IsMur(mur))
        {
            stream<<_saved_results[i].Result().term_of_order(porder);
            return stream.str();
        }
    }
    string noresult = "not found";
    return noresult;
}


ResultPair SigmaTerm::give(int porder,const double& mur)
{
    for (int i=0;i<_saved_results.size();i++)
    {
        if (_saved_results[i].IsMur(mur))
        {
            return _saved_results[i].Result().term_of_order(porder);
        }
    }
    cout<<"Result pair corresponding to mur="<<mur<<" was not found"<<endl;
    exit(0);
}

void SigmaTerm::Truncate(int n)
{
    _result.Truncate(n);
    for (int i=0;i<_saved_results.size();i++)
    {
        _saved_results[i].Truncate(n);
    }
}


bool SigmaTerm::IsZero(int porder)
{
    if (fabs(_result_central.term_of_order(porder).val())<1e-14) return true;
    else return false;
}

bool SigmaTerm::IsZero()
{
    return _result.IsZero();
}















