

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

class SavedResult{
public:
    SavedResult(const AsSeries& rp, const double& mur):_rp(rp),_mur(mur){}
    AsSeries Result() const {return _rp;}
    bool IsMur(const double& mur){return _mur==mur;}
    void Truncate(int n){_rp.Truncate(n);}
private:
    AsSeries _rp;
    double _mur;
};



class SigmaTerm{
public:
    SigmaTerm(const string& thetype, const AsSeries& val,
              LuminosityIntegral* lumi_int)
    :_type(thetype),_result(val),_lumi_int(lumi_int),_evaluated(false),_excluded(false)
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
    
    
    void ConfigureLumi(NewLuminosity* lumi,const double& tau,const UserInterface& UI);
    void CallVegas();
    AsSeries ResultCentral(){return _result_central;}
    AsSeries Result(const double & mur);

    void Truncate(int);
    ResultPair give(int i,const double& mur);
    bool IsZero(int porder);
    
    void multiply(const double& c);
    void wc_expansion(const WilsonCoefficient& wc);
    
    void multiply_by_as_pi(const double& as_pi);
    void evolve_from_muf_to_mur(const double& Lrf);

    void Save(const double& mur){_saved_results.push_back(SavedResult(_result,mur));}
    void SaveCentral(const double& mur){_result_central = _result;}
    void RewindToPostVegas(){_result = _post_vegas_result;}
    
    friend ostream& operator<<(ostream&, const SigmaTerm&);
    string print_scale_result(const double& mur,int porder);
    string type();
    bool Evaluated(){return _evaluated;}
    bool IsZero();
    bool IsIncluded(){return not(_excluded);}
private:
    string _type;//delta, plus, reg
    AsSeries _result;
    AsSeries _result_central;
    AsSeries _post_vegas_result;
    vector<SavedResult> _saved_results;
    LuminosityIntegral* _lumi_int;
    bool _evaluated;
    bool _excluded;
    double err_in_quadrature(const vector<double>& v);
};

#endif
