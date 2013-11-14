/** \file interpolator.h
  *
  * WTF ?
  *
  * \author, Stefu Bulli
  */

#ifndef INTERPOLATOR_H
#define INTERPOLATOR_H

#include<vector>
using namespace std;




// ###################################
// BASE CLASS
// ###################################

class interpolator
{
 public:
  double give_f(const double&);
 protected:
  vector<vector<double> > CoeffGrid;
  //CoeffGrid[xVal][Coeff = 0,1,2]
  void givecoeff(const double&,const double&,const double&,const double&,const double&,const double&, double&, double&, double&);
  double xmin;
  int NumberOfPoints;
  //FGrid[x_i]
  virtual vector<double> fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)=0;
  virtual vector<double> fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)=0;
  vector<double> fill_XGrid();
  void initialize(const double&, const double&, const int&);
  void initialize(const double&, const double&, const int&, const int&);
  double NF;
};

// ###################################
// LO daughter
// ###################################

class interpolator_0_0 : public interpolator
{
 public:
  interpolator_0_0(const double&, const double&, const int&);
  interpolator_0_0(const double&, const double&, const int&, const int&);
 private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
};

class interpolator_4q_0_0 : public interpolator
{
 public:
  interpolator_4q_0_0(const double&, const double&, const int&);
 private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
};

class interpolator_5q_0_0 : public interpolator
{
 public:
  interpolator_5q_0_0(const double&, const double&, const int&);
 private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
};

// ###################################
// NLO daughters
// ###################################

class interpolator_q_1_1 : public interpolator
{
 public:
  interpolator_q_1_1(const double&, const double&, const int&);
  interpolator_q_1_1(const double&, const double&, const int&, const int&);
  ~interpolator_q_1_1();
private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
   interpolator_0_0* q;
   interpolator_0_0* g;
};

class interpolator_4q_1_1 : public interpolator
{
 public:
  interpolator_4q_1_1(const double&, const double&, const int&);
  ~interpolator_4q_1_1();
private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
  interpolator* u;
  interpolator* d;
  interpolator* c;
  interpolator* s;
  interpolator* g;
};

class interpolator_5q_1_1 : public interpolator
{
 public:
  interpolator_5q_1_1(const double&, const double&, const int&);
  ~interpolator_5q_1_1();
private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
  interpolator* u;
  interpolator* d;
  interpolator* c;
  interpolator* s;
  interpolator* b;
  interpolator* g;
};

class interpolator_g_1_1 : public interpolator
{
 public:
  interpolator_g_1_1(const double&, const double&);
  interpolator_g_1_1(const double&, const double&, const int&);
  ~interpolator_g_1_1();
private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
  vector<interpolator*> lopdfs;
};

// ###################################
// NNLO daughters
// ###################################

class interpolator_q_2_1 : public interpolator
{
 public:
  interpolator_q_2_1(const double&, const double&, const double&, const int&);
  interpolator_q_2_1(const double&, const double&, const double&, const int&, const int&);
  ~interpolator_q_2_1();
 private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
  vector<interpolator*> lopdfs;
};

class interpolator_4q_2_1 : public interpolator
{
 public:
  interpolator_4q_2_1(const double&, const double&, const double&, const int&);
  ~interpolator_4q_2_1();
private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
  vector<interpolator*> lopdfs;
};

class interpolator_5q_2_1 : public interpolator
{
 public:
  interpolator_5q_2_1(const double&, const double&, const double&, const int&);
  ~interpolator_5q_2_1();
private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
  vector<interpolator*> lopdfs;
};

class interpolator_g_2_1 : public interpolator
{
 public:
  interpolator_g_2_1(const double&, const double&, const double&);
  interpolator_g_2_1(const double&, const double&, const double&, const int&);
  ~interpolator_g_2_1();
 private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
  vector<interpolator*> lopdfs;
};

class interpolator_q_2_2 : public interpolator
{
 public:
  interpolator_q_2_2(const double&, const double&, const int&);
  interpolator_q_2_2(const double&, const double&, const int&, const int&);
  ~interpolator_q_2_2();
 private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
  vector<interpolator*> lopdfs;
};

class interpolator_4q_2_2 : public interpolator
{
 public:
  interpolator_4q_2_2(const double&, const double&, const int&);
  ~interpolator_4q_2_2();
private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
  vector<interpolator*> lopdfs;
};

class interpolator_5q_2_2 : public interpolator
{
 public:
  interpolator_5q_2_2(const double&, const double&, const int&);
  ~interpolator_5q_2_2();
private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
  vector<interpolator*> lopdfs;
};

class interpolator_g_2_2 : public interpolator
{
 public:
  interpolator_g_2_2(const double&, const double&);
  interpolator_g_2_2(const double&, const double&, const int&);
  ~interpolator_g_2_2();
 private:
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&);
  vector<double> fill_FGrid(const int&, const vector<double>&, const double&, const double&, const int&);
  vector<interpolator*> lopdfs;
};

#endif
