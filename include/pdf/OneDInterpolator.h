
#ifndef ONEDINTERPOLATOR_H
#define ONEDINTERPOLATOR_H


/*
template<class T>
class OneDimensionalInterpolator
{
public:
     OneDimensionalInterpolator(double (T::* _the_f)(const double& x),T* _the_object):the_f(_the_f),the_object(_the_object){};
     double give_f_of_x(const double & x);
private:
     double (T::* the_f)(const double& x);
     T* the_object;
     
};



template<class T>
double OneDimensionalInterpolator<T>::give_f_of_x(const double &x)
{
     return the_object->*the_f(x);
}


class DummyClass
{
public:
     DummyClass(){interpol = new OneDimensionalInterpolator<DummyClass> (&DummyClass::ff,this); };
     ~DummyClass(){};
     double ff(const double & x){return x*x;}
     OneDimensionalInterpolator<DummyClass> * interpol;
     
};

class OneDimensionalInterpolator
{
public:
     OneDimensionalInterpolator(double (DummyClass::* _the_f)(const double& x),DummyClass* _the_object):the_f(_the_f),the_object(_the_object){};
     double give_f_of_x(const double & x);
private:
     double (DummyClass::* the_f)(const double& x);
     DummyClass* the_object;
     
};



template<class T>
double OneDimensionalInterpolator<T>::give_f_of_x(const double &x)
{
     return the_object->*the_f(x);
}
 */
#include<vector>
#include<iostream>
#include<math.h>
using namespace std;
class InterpolatorBase
{
public:
     InterpolatorBase(){}//: default for interpolating pdfs
     ~InterpolatorBase(){};
     void initialize();
     double give_f(const double & x);
protected:
     
     virtual double f_value(const double & x)=0;
     double compute_rr(const double& lambda);

     void fill_Grids();
     virtual double g_dist(const double & x);
     virtual double g_dist_inv(const double& lambda);

     vector<double> XGrid;
     vector<double> FGrid;
     vector<vector<double> > CoeffGrid;

     double xmin;
     double xmax;
     double hh;
     int number_of_points;
};

class DummyClass
{
public:
     DummyClass(){};
     ~DummyClass(){};
     double ff(const double & x){return log(x)*log(1.0-x);}
     
};


class Interpolator: public InterpolatorBase
{
public:
     Interpolator(DummyClass* _dd):InterpolatorBase(){dd=_dd;}
private:
     DummyClass *dd;
     double f_value(const double & x){return dd->ff(x);}
};




class Interpolator_peaking_at_zero_and_one: public InterpolatorBase
{
public:
     Interpolator_peaking_at_zero_and_one(double (*_dd)(double x)):InterpolatorBase(){dd=_dd;}
private:
     void fill_Grids();
     double g_dist_inv(const double& lambda);
     double g_dist(const double&x);

     double (*dd)(double x);
     double f_value(const double & x){return (*dd)(x);}
     double find_recursive(double x,double x0,double x1);

};














#endif