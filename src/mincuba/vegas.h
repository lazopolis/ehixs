#ifndef VEGAS_H
#define VEGAS_H



class VegasCumulant{
public:
    double sum, sqsum;
    double weightsum, avgsum;
    double chisum, chisqsum, guess;
    double avg, err, chisq;
};

class VegasGrid{

public:
    int v[128];
};

class VegasState{

public:
    int niter_;
    int nsamples_;
    int neval_;
    vector<VegasCumulant> cumul_;
    vector<VegasGrid> grid_;
};
typedef real Grid[NBINS];



class VegasIntegrator{
public:
    void Set_ndim(int ndim){ndim_=ndim;}
    void Set_ncomp(int ncomp){ncomp_=ncomp;}
    void Set_mineval(int mineval){mineval_=mineval;}
    void Set_maxeval(int maxeval){maxeval_=maxeval;}
    void Set_nincrease(int nincrease){nincrease_=nincrease;}
    void Set_nbatch(int nbatch){nbatch_=nbatch;}
    void Set_gridno(int gridno){gridno_=gridno;}
    void Set_epsrel(const double& epsrel){epsrel_=epsrel;}
    void Set_epsabs(const double& epsabs){epsabs_=epsabs;}

    void Set_statefile(const string& statefile){statefile_=statefile;}
    
    void Integrate();
    
    double result(int i){return result_[i];}
    double error(int i){return error_[i];}
    double prob(int i){return prob_[i];}
    
    
private:
    int ndim_;
    int ncomp_;
    
    int seed_;
    int mineval_;
    int maxeval_;
    int nstart_;
    int nincrease_;
    int nbatch_;
    int gridno_;
    
    double epsrel_;
    double epsabs_;
    
    string statefile_;
    
    int neval_;
    
    VegasState* state_;
    
    vector<double> result_;
    vector<double> error_;
    vector<double> prob_;
};

#endif
