#ifndef KINEMATIC_INVARIANTS_H
#define KINEMATIC_INVARIANTS_H

#include <iostream>
using namespace std;

class KinematicInvariants{
public:
    void SetMaxMomentumID(int max){max_=max;}
    void Set(int i,int j,double x)
    {
        s_ij_[index(min(i,j))][index(max(i,j))] = x;
    }
    void Set(int i,double x)
    {
        s_i_[index(i)]=x;
    }
    double s(int i,int j)const {return s_ij_[index(min(i,j))][index(max(i,j))];}
    double q(int i,int j)const {return q_ij_[index(min(i,j))][index(max(i,j))];}
    double s(int i)const {return s_i_[index(i)];}
    double q(int i)const {return q_i_[index(i)];}
    void compute_dimensionless_invariants();
    
    friend ostream& operator<<(ostream& stream, const KinematicInvariants& kk);
    
    
private:
    double s_ij_[10][10];//s_ij = (p_i-p_j)^2
    double q_ij_[10][10];//q_ij = s_ij/s_12
    double s_i_[10];// s_i = p_i^2
    double q_i_[10];// q_i = s_i/s_12
    int max_;
private:
    void check_size(int k) const ;
    int max(int i,int j) const ;
    int min(int i,int j) const;
    int index(int) const;
};

// why is this here and the implementation of generate in kinematic_variables.cpp, not even in tools??
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