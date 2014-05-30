#ifndef GLUON_FUSION_PARAMETRIZATION
#define GLUON_FUSION_PARAMETRIZATION


class GluonFusionParametrization{
public: 
    GluonFusionParametrization();
    void Configure(const double& tau,const double& mh){ tau_=tau;mh_=mh;}
    void parametrization_for_LO_kinematics(double* xx_vegas);
    void parametrization_for_NLO_kinematics(double* xx_vegas);
    
    double x1LO,x2LO,zLO,measLO,cursLO;
    double x1,x2,z,meas,curs,Log_1mz;
    double lambda,phi;
private:
    int parametrization_switch_;
    double tau_;
    double mh_;
    double generate_x1(double & 
                jac_from_rap_param,
                const double& x0);

};


#endif