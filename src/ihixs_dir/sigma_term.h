

#ifndef SIGMA_TERM_H
#define SIGMA_TERM_H

#include "string"
#include "vector"
using namespace std;
#include "luminosity.h"
#include "luminosity_integrals.h"
#include "wilson_coefficients.h"
#include "user_interface.h"

#include "as_series.h"


class SigmaTerm{
public:
    SigmaTerm(const string& thetype, const AsSeries& val,
              LuminosityIntegral* lumi_int)
    :_type(thetype),_result(val),_lumi_int(lumi_int),_evaluated(false),_excluded(false),_exact(false)
    {};
    SigmaTerm(const string& thetype, const AsSeries& val,
              LuminosityIntegral* lumi_int,const string& str):SigmaTerm(thetype,val,lumi_int)
    {
        if (str=="excluded")
            _excluded = true;
        else
        {
            cout<<"\n Sigma Term constructor with extra string that is not equal to <excluded>."
            <<endl<<"I have to exit!"<<endl;
            exit(0);
        }
    }
    
    SigmaTerm(const string& thetype, const AsSeries& val,
              LuminosityIntegral* lumi_int,const string& str,const string& exact_str):SigmaTerm(thetype,val,lumi_int)
    {
        if (str=="excluded")
            _excluded = true;
        else
        {
            cout<<"\n Sigma Term constructor with extra string that is not equal to <excluded>."
            <<endl<<"I have to exit!"<<endl;
            exit(0);
        }
        if (exact_str=="exact") _exact=true;
        else
        {
            cout<<"\n Sigma Term constructor with extra <exact string> that is not equal to <exact>."
            <<endl<<"I have to exit!"<<endl;
            exit(0);
        }
    }
    void ConfigureLumi(NewLuminosity* lumi,const double& tau,const UserInterface& UI);
    void CallVegas();
    void Truncate(int);
    bool IsZero(int porder);
    

    void SetQCDResult(const AsSeries& qcd){_qcd_result = qcd;}
    void SetEwResult(const AsSeries& ew){_ew_result = ew;}
    // output
    AsSeries PostVegasResult(){return _result;}
    AsSeries Result(){return _qcd_result;}
    AsSeries EwResult(){return _ew_result;}
    friend ostream& operator<<(ostream&, const SigmaTerm&);

    string type();
    bool Evaluated() {return _evaluated;}
    bool IsZero();
    bool IsIncluded()const{return not(_excluded);}
    bool IsExact()const{return _exact;}
private:
    string _type;//delta, plus, reg
    AsSeries _result;
    AsSeries _qcd_result;
    AsSeries _ew_result;

    LuminosityIntegral* _lumi_int;
    bool _evaluated;
    bool _excluded;
    bool _exact;
};

#endif
