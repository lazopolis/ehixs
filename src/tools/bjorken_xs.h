#ifndef BJORKEN_XS_H
#define BJORKEN_XS_H


class BjorkenXs{
public:
    void generate(const double& tau,const double& v0, const double& v1);
    double jacobian(){return jacobian_;}
    double x1() const {return x1_;}
    double x2() const {return x2_;}
    double com_rapidity_ratio(){return (x2_-x1_)/(x2_+x1_);}
private:
    double x1_,x2_;
    double jacobian_;
};


#endif

