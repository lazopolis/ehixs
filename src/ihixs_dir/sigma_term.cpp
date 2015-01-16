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
    }
}

string SigmaTerm::type()
{
    return _type;
}



ostream& operator<<(ostream& stream, const SigmaTerm& st)
{
    
    
    stream<<setw(28)<<left<<st._type;
    if (st.IsIncluded()) stream<<" ";
    else stream<<"*";
    stream
    <<"\t"<<st._qcd_result.term_of_order(2)
    <<"\t"<<st._qcd_result.term_of_order(3)
    <<"\t"<<st._qcd_result.term_of_order(4)
    <<"\t"<<st._qcd_result.term_of_order(5)
    <<endl;
    return stream;
}




void SigmaTerm::Truncate(int n)
{
    _result.Truncate(n);
    _qcd_result.Truncate(n);
    _ew_result.Truncate(n);

}


bool SigmaTerm::IsZero(int porder)
{
    if (fabs(_qcd_result.term_of_order(porder).val())<1e-14) return true;
    else return false;
}

bool SigmaTerm::IsZero()
{
    return _result.IsZero();
}















