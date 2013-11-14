/** \file interpolator.cpp
  *
  * Implement interpolator.cpp
  *
  * \author, Stefu Bulli
  */

#include <iostream>
#include <vector>
#include <math.h>
#include <stdlib.h> //: for exit()

using namespace std;

#include "LHAPDF/LHAPDF.h"
#include "splitting_kernels.h"
#include "interpolator.h"

#define TRAPEZIUM 50000
//#define TRAPEZIUM 5


// ###################################
// BASE CLASS
// ###################################

void interpolator::initialize(const double& muf, const double& mur, const int& iprtn){
//PRE: Receives the double muf (factorisation scale) and a parton number. A pdf set must be initialised
//POST: Fills CoeffGrid with the coefficients of the quadratic interpolation of the particular subset.

  // NOTE: workaround for the fact that constructors are not inherited by daughter classes
  
  NumberOfPoints = 2000;
   //  NumberOfPoints = 2;

  xmin = 1e-6; // !!! seems to be quite generally the default value for any lhapdf set

  vector<double> XGrid = fill_XGrid();
  vector<double> FGrid = this->fill_FGrid(iprtn,XGrid,muf,mur);
  vector<double> temp;

  for(int ix=0;ix<NumberOfPoints;ix++){

    int i1,i2,i3;
    if(ix==0){
      i1=ix;i2=ix+1;i3=ix+2;
    }
    else if(ix==NumberOfPoints-1){
      i1=ix-2;i2=ix-1;i3=ix;
    }
    else	{
      i1=ix-1;i2=ix;i3=ix+1;
    }
    double a,b,c;
    givecoeff(XGrid[i1],XGrid[i2],XGrid[i3],FGrid[i1],FGrid[i2],FGrid[i3],a,b,c);
    
    temp.push_back(a);temp.push_back(b);temp.push_back(c);
    CoeffGrid.push_back(temp);
    temp.clear();
  }
}

void interpolator::initialize(const double& muf, const double& mur, const int& iprtn, const int& jprtn)
{
//PRE: Receives the double muf (factorisation scale) and a parton number. A pdf set must be initialised
//POST: Fills CoeffGrid with the coefficients of the quadratic interpolation of the particular subset.

  // NOTE: workaround for the fact that constructors are not inherited by daughter classes
  
  NumberOfPoints = 2000;
 //    NumberOfPoints=5;
  xmin = 1e-6; // !!! seems to be quite generally the default value for any lhapdf set

  vector<double> XGrid = fill_XGrid();
  vector<double> FGrid = this->fill_FGrid(iprtn,XGrid,muf,mur,jprtn);
  vector<double> temp;

  for(int ix=0;ix<NumberOfPoints;ix++){

    int i1,i2,i3;
    if(ix==0){
      i1=ix;i2=ix+1;i3=ix+2;
    }
    else if(ix==NumberOfPoints-1){
      i1=ix-2;i2=ix-1;i3=ix;
    }
    else	{
      i1=ix-1;i2=ix;i3=ix+1;
    }
    double a,b,c;
    givecoeff(XGrid[i1],XGrid[i2],XGrid[i3],FGrid[i1],FGrid[i2],FGrid[i3],a,b,c);
    
    temp.push_back(a);temp.push_back(b);temp.push_back(c);
    CoeffGrid.push_back(temp);
    temp.clear();
  }
}

//---------------------------------------------------------------------------------------------

void interpolator::givecoeff(const double& x1,const double& x2,const double& x3,const double& y1,const double& y2,const double& y3,double& a,double& b,double& c){
//PRE: receives 9 doubles, the first 3 are x values, the second 3 y-values, the last 3 (by reference) the coefficients of the quadratic interpolation.
//POST: the 3 coefficients are calculated and overwritten with their respective values.
  double den=(x1-x2)*(x2-x3)*(x3-x1);
  a = -(y1*(x2-x3) + y2*(x3-x1) + y3*(x1-x2))/den;
  b = (y1*(x2*x2 - x3*x3) + y2*(x3*x3 - x1*x1) + y3*(x1*x1 - x2*x2))/den;
  c = -(y1*(x2-x3)*x2*x3 + y2*(x3-x1)*x1*x3 + y3*(x1-x2)*x1*x2)/den;
}

//---------------------------------------------------------------------------------------------

double interpolator::give_f(const double& x){
//PRE: receives a double x and an int iprtn. calculate_grid must have been executed beforehand.
//POST: returns the interpolated value of the pdf at x

  if((x <= 0.0) | (x >= 1.0)){
    cerr << "ERROR in [" << __func__ << "]: x-value out of allowed range! (x = " << x << " )\nAborting...\n\n";
    exit(1);
  }
  
  double hh = NumberOfPoints*(NumberOfPoints-1)/(1.0-xmin);
  double lambda = hh*(x-xmin);
  
  if(lambda<0.0){
    //cout << "\n\nWARNING: in [" << __func__ << "]: x = " << x << " out of fitting range!\n\n";
    lambda=0.0;
  }
  double rr = 0.5*(1.0 + sqrt(1.0 + 4.0*lambda));
  int L = int(rr);
  if((L<0) | (L>NumberOfPoints)){cout << "\n\n@@@@@@@@@@@@@@@\nZis can't be! L = " << L << " ( x = " << x << " ) !\n@@@@@@@@@@@@@@@\n";}
  //
  return CoeffGrid[L][2] + CoeffGrid[L][1]*x + CoeffGrid[L][0]*x*x;
}

// ----------------------------------------------------------------------------

vector<double> interpolator::fill_XGrid()
{
  // returns a vector with NumberOfPoints points between xmin and 1. 
  // !!! if this is changed, (the distribution of points, not the amount) then give_f must be changed as well.

  vector<double> XGrid;

  for(int i=0;i<NumberOfPoints;i++){
    double x = xmin + (1.0-xmin)*i*(i+1)/NumberOfPoints/(NumberOfPoints-1) - 1e-9;
    XGrid.push_back(x);
  }

  return XGrid;
}

// ###################################
// LO daughter
// ###################################

interpolator_0_0::interpolator_0_0(const double& NFl, const double& muf, const int& iprtn)
{
  NF = NFl;
  initialize(muf,muf,iprtn);
}

interpolator_0_0::interpolator_0_0(const double& NFl, const double& muf, const int& iprtn, const int& jprtn)
{
  NF = NFl;
  initialize(muf,muf,iprtn,jprtn);
}

vector<double> interpolator_0_0::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{
  //PRE: receives the parton number, the array XGrid and the double muf. A pdf set must be initialised.
  //POST: returns the FGrid vector with the values of the corresponding pdf at the x-values in XGrid().

  vector<double> FGrid;
  for(int i=0;i<NumberOfPoints;i++)
    {
      FGrid.push_back(LHAPDF::xfx(XGrid[i],muf,iprtn)/XGrid[i]);
    }
  return FGrid;
}

vector<double> interpolator_0_0::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{
  //PRE: receives the parton number, the array XGrid and the double muf. A pdf set must be initialised.
  //POST: returns the FGrid vector with the values of the corresponding pdf at the x-values in XGrid().

  vector<double> FGrid;
  for(int i=0;i<NumberOfPoints;i++)
    {
      if (jprtn == iprtn)
	{
	  FGrid.push_back(LHAPDF::xfx(XGrid[i],muf,iprtn)/XGrid[i]);
	}
      else
	{
	  FGrid.push_back(0.0);
	}
    }
  return FGrid;
}

// --------------------------------------------------

interpolator_4q_0_0::interpolator_4q_0_0(const double& NFl, const double& muf, const int& iprtn)
{
  NF = NFl;
  initialize(muf,muf,iprtn);
}

vector<double> interpolator_4q_0_0::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{
  //PRE: receives the parton number, the array XGrid and the double muf. A pdf set must be initialised.
  //POST: returns the FGrid vector with the values of the corresponding pdf at the x-values in XGrid().

  vector<double> FGrid;
  for(int i=0;i<NumberOfPoints;i++)
    {
      double temp=0.0;
      if (iprtn == 6)
	temp = LHAPDF::xfx(XGrid[i],muf,1)+LHAPDF::xfx(XGrid[i],muf,2)+LHAPDF::xfx(XGrid[i],muf,3)+LHAPDF::xfx(XGrid[i],muf,4);
      else if (iprtn == -6)
	temp = LHAPDF::xfx(XGrid[i],muf,-1)+LHAPDF::xfx(XGrid[i],muf,-2)+LHAPDF::xfx(XGrid[i],muf,-3)+LHAPDF::xfx(XGrid[i],muf,-4);
      
      FGrid.push_back(temp/XGrid[i]);
    }
  return FGrid;
}

vector<double> interpolator_4q_0_0::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{
  // dummy implementation
  vector<double> FGrid;
  return FGrid;
}

// --------------------------------------------------

interpolator_5q_0_0::interpolator_5q_0_0(const double& NFl, const double& muf, const int& iprtn)
{
  NF = NFl;
  initialize(muf,muf,iprtn);
}

vector<double> interpolator_5q_0_0::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{
  //PRE: receives the parton number, the array XGrid and the double muf. A pdf set must be initialised.
  //POST: returns the FGrid vector with the values of the corresponding pdf at the x-values in XGrid().

  vector<double> FGrid;
  for(int i=0;i<NumberOfPoints;i++)
    {
      double temp=0.0;
      if (iprtn == 7)
	temp = LHAPDF::xfx(XGrid[i],muf,1)+LHAPDF::xfx(XGrid[i],muf,2)+LHAPDF::xfx(XGrid[i],muf,3)+LHAPDF::xfx(XGrid[i],muf,4)+LHAPDF::xfx(XGrid[i],muf,5);
      else if (iprtn == -7)
	temp = LHAPDF::xfx(XGrid[i],muf,-1)+LHAPDF::xfx(XGrid[i],muf,-2)+LHAPDF::xfx(XGrid[i],muf,-3)+LHAPDF::xfx(XGrid[i],muf,-4)+LHAPDF::xfx(XGrid[i],muf,-5);
      
      FGrid.push_back(temp/XGrid[i]);
    }
  return FGrid;
}

vector<double> interpolator_5q_0_0::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{
  // dummy implementation
  vector<double> FGrid;
  return FGrid;
}

// ###################################
// NLO daughters
// ###################################

interpolator_q_1_1::interpolator_q_1_1(const double& NFl, const double& muf, const int& iprtn)
{
  NF = NFl;
  q = new interpolator_0_0(NFl,muf,iprtn);
  g = new interpolator_0_0(NFl,muf,0);
  initialize(muf,muf,iprtn);  
}

interpolator_q_1_1::interpolator_q_1_1(const double& NFl, const double& muf, const int& iprtn, const int& jprtn)
{
  NF = NFl;
  q = new interpolator_0_0(NFl,muf,jprtn);
  initialize(muf,muf,iprtn,jprtn);  
}

interpolator_q_1_1::~interpolator_q_1_1()
{
  delete q;
  delete g;
}

vector<double> interpolator_q_1_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{
  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      double qx,qxy,gxy,y,xy,yprime,jac;
      
      qx = q->give_f(x);
      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
	  qxy = q->give_f(xy);
	  gxy = g->give_f(xy);
	  
	  temp += pqq0_DD(y)*(qxy/y-qx)/(1.0-y)*jac; //plus term from pqq
	  temp += pqq0_reg(y)*qxy/y*jac; // regular pqq term
	  temp += pqg0_reg(y)*gxy/y*jac; // regular pqg term (the only nonzero part)

	  yprime+=1.0/TRAPEZIUM; //step
	}
      
      temp += (pqq0_d1() + pqq0_DDb(x))*qx; // delta and DD-boundary contribution from pqq 
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}

vector<double> interpolator_q_1_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{
  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      double qx,qxy,y,xy,yprime,jac;
      
      qx = q->give_f(x);
      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
	  qxy = q->give_f(xy);
	  
	  if (jprtn == iprtn)
	    {
	      temp += pqq0_DD(y)*(qxy/y-qx)/(1.0-y)*jac; //plus term from pqq
	      temp += pqq0_reg(y)*qxy/y*jac; // regular pqq term
	    }
	  else if (jprtn == 0)
	    {
	      temp += pqg0_reg(y)*qxy/y*jac; // regular pqg term (the only nonzero part)
	    }
	  yprime+=1.0/TRAPEZIUM; //step
	}
      if (jprtn == iprtn)
	{
	  temp += (pqq0_d1() + pqq0_DDb(x))*qx; // delta and DD-boundary contribution from pqq 
	}
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}

// --------------------------------------------------

interpolator_4q_1_1::interpolator_4q_1_1(const double& NFl, const double& muf, const int& iprtn)
{
  NF = NFl;
  g = new interpolator_0_0(NFl,muf,0);
  if(iprtn == 6)
    {
      d = new interpolator_0_0(NFl,muf,1);
      u = new interpolator_0_0(NFl,muf,2);
      s = new interpolator_0_0(NFl,muf,3);
      c = new interpolator_0_0(NFl,muf,4);
    }
  else if (iprtn == -6)
    {
      d = new interpolator_0_0(NFl,muf,-1);
      u = new interpolator_0_0(NFl,muf,-2);
      s = new interpolator_0_0(NFl,muf,-3);
      c = new interpolator_0_0(NFl,muf,-4);
    }
  initialize(muf,muf,iprtn);  
}

interpolator_4q_1_1::~interpolator_4q_1_1()
{
  delete g;
  delete d;
  delete u;
  delete s;
  delete c;
}

vector<double> interpolator_4q_1_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{
  // just a dummy
  vector<double> dummy;
  return dummy;
}

vector<double> interpolator_4q_1_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{
  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      double dx,dxy,ux,uxy,sx,sxy,cx,cxy,gxy,y,xy,yprime,jac;
      
      dx = d->give_f(x);
      ux = u->give_f(x);
      sx = s->give_f(x);
      cx = c->give_f(x);
      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
	  dxy = d->give_f(xy);
	  uxy = u->give_f(xy);
	  sxy = s->give_f(xy);
	  cxy = c->give_f(xy);
	  gxy = g->give_f(xy);
	  
	  temp += pqq0_DD(y)*((dxy/y-dx)+(uxy/y-ux)+(sxy/y-sx)+(cxy/y-cx))/(1.0-y)*jac; //plus term from pqq
	  temp += pqq0_reg(y)*(dxy/y+uxy/y+sxy/y+cxy/y)*jac; // regular pqq term
	  temp += 4.0*pqg0_reg(y)*gxy/y*jac; // regular pqg term (the only nonzero part)

	  yprime+=1.0/TRAPEZIUM; //step
	}
      
      temp += (pqq0_d1() + pqq0_DDb(x))*(dx+ux+sx+cx); // delta and DD-boundary contribution from pqq 
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}


// --------------------------------------------------

interpolator_5q_1_1::interpolator_5q_1_1(const double& NFl, const double& muf, const int& iprtn)
{
  NF = NFl;
  g = new interpolator_0_0(NFl,muf,0);
  if(iprtn == 7)
    {
      d = new interpolator_0_0(NFl,muf,1);
      u = new interpolator_0_0(NFl,muf,2);
      s = new interpolator_0_0(NFl,muf,3);
      c = new interpolator_0_0(NFl,muf,4);
      b = new interpolator_0_0(NFl,muf,5);
    }
  else if (iprtn == -7)
    {
      d = new interpolator_0_0(NFl,muf,-1);
      u = new interpolator_0_0(NFl,muf,-2);
      s = new interpolator_0_0(NFl,muf,-3);
      c = new interpolator_0_0(NFl,muf,-4);
      b = new interpolator_0_0(NFl,muf,-5);
    }
  initialize(muf,muf,iprtn);  
}

interpolator_5q_1_1::~interpolator_5q_1_1()
{
  delete g;
  delete d;
  delete u;
  delete s;
  delete c;
  delete b;
}

vector<double> interpolator_5q_1_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{
  // just a dummy
  vector<double> dummy;
  return dummy;
}

vector<double> interpolator_5q_1_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{
  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      double dx,dxy,ux,uxy,sx,sxy,cx,cxy,bx,bxy,gxy,y,xy,yprime,jac;
      
      dx = d->give_f(x);
      ux = u->give_f(x);
      sx = s->give_f(x);
      cx = c->give_f(x);
      bx = b->give_f(x);
      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
	  dxy = d->give_f(xy);
	  uxy = u->give_f(xy);
	  sxy = s->give_f(xy);
	  cxy = c->give_f(xy);
	  bxy = b->give_f(xy);
	  gxy = g->give_f(xy);
	  
	  temp += pqq0_DD(y)*((dxy/y-dx)+(uxy/y-ux)+(sxy/y-sx)+(cxy/y-cx)+(bxy/y-bx))/(1.0-y)*jac; //plus term from pqq
	  temp += pqq0_reg(y)*(dxy/y+uxy/y+sxy/y+cxy/y+bxy/y)*jac; // regular pqq term
	  temp += 5.0*pqg0_reg(y)*gxy/y*jac; // regular pqg term (the only nonzero part)

	  yprime+=1.0/TRAPEZIUM; //step
	}
      
      temp += (pqq0_d1() + pqq0_DDb(x))*(dx+ux+sx+cx+bx); // delta and DD-boundary contribution from pqq 
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}

// --------------------------------------------------

interpolator_g_1_1::interpolator_g_1_1(const double& NFl, const double& muf)
{
  NF = NFl;
  for (int i=-5; i<6; i++)
    {
      lopdfs.push_back(new interpolator_0_0(NFl,muf,i));
    }
  initialize(muf,muf,0);
}

interpolator_g_1_1::interpolator_g_1_1(const double& NFl, const double& muf, const int& jprtn)
{
  NF = NFl;
  lopdfs.push_back(new interpolator_0_0(NFl,muf,jprtn));
  initialize(muf,muf,0,jprtn);
}

interpolator_g_1_1::~interpolator_g_1_1()
{
  for (int i=0; i<lopdfs.size(); i++)
    {
      delete lopdfs[i];
    }
}

vector<double> interpolator_g_1_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{
  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      double y,xy,yprime,jac,gx;
      double pdfxy[11];
      
      gx = lopdfs[5]->give_f(x);
      yprime=0.0;
//    cout<<"\n new point";

      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
     //cout<<"\n pdfs at x*y = "<<xy;

	  for (int j=0; j<11; j++)
	    {
	      pdfxy[j] = lopdfs[j]->give_f(xy);
         //cout<<" "<<pdfxy[j];

	    }
	  
	  double plus = pgg0_DD(y)*(pdfxy[5]/y-gx)/(1.0-y)*jac; //plus term from pgg
	  double reg_gg= pgg0_reg(y)*pdfxy[5]/y*jac; // regular pgg term
	  double reg_qg= pgq0_reg(y)*(pdfxy[0]+pdfxy[1]+pdfxy[2]+pdfxy[3]+pdfxy[4]+pdfxy[6]+pdfxy[7]+pdfxy[8]+pdfxy[9]+pdfxy[10])/y*jac; // regular pgq term (the only nonzero part)
     temp += plus+reg_gg+reg_qg;
     //cout<<"\nx="<<x<<" y="<<y<<"\t"<<plus<<" "<<reg_gg<<" "<<reg_qg;
	  yprime+=1.0/TRAPEZIUM; //step
	}
      
      temp += (pgg0_d1(NF) + pgg0_DDb(x))*gx; // delta and DD-boundary contribution from pgg 
     //cout<<" delta and boundary: "<<(pgg0_d1(NF) + pgg0_DDb(x))*gx;
     // storing and resetting
      FGrid.push_back(temp);
//          if (i % 100 ==0)
//               {
//               cout << "\nDone with x = " << x << "\t f(x)="<<temp<<"\t"<<gx;
//               }
      temp=0.0;
    }
  return FGrid;
}

vector<double> interpolator_g_1_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{
  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      double y,xy,yprime,jac,gx,gxy;
      
      gx = lopdfs[0]->give_f(x);
      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
	  gxy = lopdfs[0]->give_f(xy); 
	  
     double plus=0.0;
     double reg_gg=0.0;
     double reg_qg=0.0;
	  if (jprtn == 0)
	    {
	      plus += pgg0_DD(y)*(gxy/y-gx)/(1.0-y)*jac; //plus term from pgg
	      reg_gg += pgg0_reg(y)*gxy/y*jac; // regular pgg term
	    }
	  else
	    {
	      reg_qg += pgq0_reg(y)*gxy/y*jac; // regular pgq term (the only nonzero part)
	    }
     temp += plus+reg_gg+reg_qg;
	  yprime+=1.0/TRAPEZIUM; //step
     //cout<<"\n x="<<x<<" y="<<y<<" xy="<<xy<<"\t"<<plus<<" "<<reg_gg<<" "<<reg_qg<<" "<<pgg0_DD(y)
     //<<" "<<gxy<<" "<<gx;
	}
      if (jprtn == 0)
	    {
        temp += (pgg0_d1(NF) + pgg0_DDb(x))*gx; // delta and DD-boundary contribution from pgg 
        }
    
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
      //cout << "\tDone with x = " << x;
    }
  return FGrid;
}

// ###################################
// NNLO daughters
// ###################################

interpolator_q_2_1::interpolator_q_2_1(const double& NFl, const double& muf, const double& mur, const int& iprtn)
{
  NF = NFl;
  for (int i=-5; i<6; i++)
    {
      lopdfs.push_back(new interpolator_0_0(NFl,muf,i));
    }
  initialize(muf,mur,iprtn);
}

interpolator_q_2_1::interpolator_q_2_1(const double& NFl, const double& muf, const double& mur, const int& iprtn, const int& jprtn)
{
  NF = NFl;
  lopdfs.push_back(new interpolator_0_0(NFl,muf,jprtn));
  initialize(muf,mur,iprtn,jprtn);
}

interpolator_q_2_1::~interpolator_q_2_1()
{
  for (int i=0; i<lopdfs.size(); i++)
    {
      delete lopdfs[i];
    }
}

vector<double> interpolator_q_2_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{

  const double b0lnrf = log(mur*mur/muf/muf)*(11.0/4.0-NF/6.0);

  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      //cout << "\nStarting x = " << x;
      double y,xy,yprime,jac,qx,Qxy;
      double pdfxy[11];
      
      qx = lopdfs[iprtn+5]->give_f(x);

      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
	  Qxy=0.0;
	  for (int j=0; j<11; j++)
	    {
	      pdfxy[j] = lopdfs[j]->give_f(xy); 
	      if ((j!=iprtn+5) && (j!=(-iprtn+5)) && (j!=5))
		{
		  Qxy+=pdfxy[j];
		}	      
	    }
	  
	  temp += (pqq1_DD(NF,y)+2.0*b0lnrf*pqq0_DD(y))*(pdfxy[iprtn+5]/y-qx)/(1.0-y)*jac; //plus term from pqq
	  temp += (pqq1_reg(NF,y)+2.0*b0lnrf*pqq0_reg(y))*pdfxy[iprtn+5]/y*jac; // regular pqq term
	  temp += pqqbar1_reg(y)*pdfxy[-iprtn+5]/y*jac; // regular pqqbar term
	  temp += (pqg1_reg(y)+2.0*b0lnrf*pqg0_reg(y))*pdfxy[5]/y*jac; //regular pqg term
	  temp += pqQ1_reg(y)*Qxy/y*jac; //regular pqQ term
	  
	  yprime+=1.0/TRAPEZIUM; //step
	}
      
      temp += (pqq1_d1(NF)+2.0*b0lnrf*pqq0_d1() + pqq1_DDb(NF,x)+2.0*b0lnrf*pqq0_DDb(x))*qx; // delta and DD-boundary contribution from pqq 
      temp*=0.5; // factor of two from 1/(2eps)
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}

vector<double> interpolator_q_2_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{

  const double b0lnrf = log(mur*mur/muf/muf)*(11.0/4.0-NF/6.0);

  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      //cout << "\nStarting x = " << x;
      double y,xy,yprime,jac,qx,qxy;
      
      qx = lopdfs[0]->give_f(x);

      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
	  qxy = lopdfs[0]->give_f(xy); 
	  
	  if (jprtn == iprtn)
	    {
	      temp += (pqq1_DD(NF,y)+2.0*b0lnrf*pqq0_DD(y))*(qxy/y-qx)/(1.0-y)*jac; //plus term from pqq
	      temp += (pqq1_reg(NF,y)+2.0*b0lnrf*pqq0_reg(y))*qxy/y*jac; // regular pqq term
	    }
	  else if (jprtn == -iprtn)
	    {
	      temp += pqqbar1_reg(y)*qxy/y*jac; // regular pqqbar term
	    }
	  else if (jprtn == 0)
	    {
	      temp += (pqg1_reg(y)+2.0*b0lnrf*pqg0_reg(y))*qxy/y*jac; //regular pqg term
	    }
	  else
	    {
	      temp += pqQ1_reg(y)*qxy/y*jac; //regular pqQ term
	    }
	  
	  yprime+=1.0/TRAPEZIUM; //step
	}
      
      if (jprtn == iprtn)
	{
	  temp += (pqq1_d1(NF)+2.0*b0lnrf*pqq0_d1() + pqq1_DDb(NF,x)+2.0*b0lnrf*pqq0_DDb(x))*qx; // delta and DD-boundary contribution from pqq 
	}
      temp*=0.5; // factor of two from 1/(2eps)
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}

// --------------------------------------------------

interpolator_4q_2_1::interpolator_4q_2_1(const double& NFl, const double& muf, const double& mur, const int& iprtn)
{
  NF = NFl;
  for (int i=-5; i<6; i++)
    {
      lopdfs.push_back(new interpolator_0_0(NFl,muf,i));
    }
  initialize(muf,mur,iprtn);
}

interpolator_4q_2_1::~interpolator_4q_2_1()
{
  for (int i=-5; i<6; i++)
    {
      delete lopdfs[i];
    }
}

vector<double> interpolator_4q_2_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{
  // just a dummy
  vector<double> dummy;
  return dummy;
}

vector<double> interpolator_4q_2_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{

  const double b0lnrf = log(mur*mur/muf/muf)*(11.0/4.0-NF/6.0);

  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      //cout << "\nStarting x = " << x;
      double y,xy,yprime,jac,Qxy,Qx,Qbxy;
      double pdfxy[11],pdfx[11];
      
      for (int j=0; j<11; j++)
	{
	  pdfx[j] = lopdfs[j]->give_f(x);
	} 
      if (iprtn == 6)
	Qx = pdfx[6]+pdfx[7]+pdfx[8]+pdfx[9];
      else if (iprtn == -6)
	Qx = pdfx[1]+pdfx[2]+pdfx[3]+pdfx[4];

      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly

	  for (int j=0; j<11; j++)
	    {
	      pdfxy[j] = lopdfs[j]->give_f(xy); 
	    }
	  if (iprtn == -6)
	    {
	    Qxy = pdfxy[1]+pdfxy[2]+pdfxy[3]+pdfxy[4];
	    Qbxy = pdfxy[6]+pdfxy[7]+pdfxy[8]+pdfxy[9];
	    }
	  else if (iprtn == 6)
	    {
	    Qxy = pdfxy[6]+pdfxy[7]+pdfxy[8]+pdfxy[9];
	    Qbxy = pdfxy[1]+pdfxy[2]+pdfxy[3]+pdfxy[4];
	    }
	  temp += (pqq1_DD(NF,y)+2.0*b0lnrf*pqq0_DD(y))*(Qxy/y-Qx)/(1.0-y)*jac; //plus term from pqq
	  temp += (pqq1_reg(NF,y)+2.0*b0lnrf*pqq0_reg(y))*Qxy/y*jac; // regular pqq term
	  temp += pqqbar1_reg(y)*Qbxy/y*jac; // regular pqqbar term
	  temp += 4.0*(pqg1_reg(y)+2.0*b0lnrf*pqg0_reg(y))*pdfxy[5]/y*jac; //regular pqg term
	  temp += pqQ1_reg(y)*(3.0*Qxy/y+3.0*Qbxy/y+4.0*pdfxy[0]/y+4.0*pdfxy[10]/y)*jac; //regular pqQ term
	  
	  yprime+=1.0/TRAPEZIUM; //step
	}
      
      temp += (pqq1_d1(NF)+2.0*b0lnrf*pqq0_d1() + pqq1_DDb(NF,x)+2.0*b0lnrf*pqq0_DDb(x))*Qx; // delta and DD-boundary contribution from pqq 
      temp*=0.5; // factor of two from 1/(2eps)
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}


// --------------------------------------------------

interpolator_5q_2_1::interpolator_5q_2_1(const double& NFl, const double& muf, const double& mur, const int& iprtn)
{
  NF = NFl;
  for (int i=-5; i<6; i++)
    {
      lopdfs.push_back(new interpolator_0_0(NFl,muf,i));
    }
  initialize(muf,mur,iprtn);
}

interpolator_5q_2_1::~interpolator_5q_2_1()
{
  for (int i=-5; i<6; i++)
    {
      delete lopdfs[i];
    }
}

vector<double> interpolator_5q_2_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{
  // just a dummy
  vector<double> dummy;
  return dummy;
}

vector<double> interpolator_5q_2_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{

  const double b0lnrf = log(mur*mur/muf/muf)*(11.0/4.0-NF/6.0);

  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      //cout << "\nStarting x = " << x;
      double y,xy,yprime,jac,Qxy,Qx,Qbxy;
      double pdfxy[11],pdfx[11];
      
      for (int j=0; j<11; j++)
	{
	  pdfx[j] = lopdfs[j]->give_f(x);
	} 
      if (iprtn == 7)
	Qx = pdfx[6]+pdfx[7]+pdfx[8]+pdfx[9]+pdfx[10];
      else if (iprtn == -7)
	Qx = pdfx[1]+pdfx[2]+pdfx[3]+pdfx[4]+pdfx[0];

      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly

	  for (int j=0; j<11; j++)
	    {
	      pdfxy[j] = lopdfs[j]->give_f(xy); 
	    }
	  if (iprtn == -7)
	    {
	    Qxy = pdfxy[1]+pdfxy[2]+pdfxy[3]+pdfxy[4]+pdfxy[0];
	    Qbxy = pdfxy[6]+pdfxy[7]+pdfxy[8]+pdfxy[9]+pdfxy[10];
	    }
	  else if (iprtn == 7)
	    {
	    Qxy = pdfxy[6]+pdfxy[7]+pdfxy[8]+pdfxy[9]+pdfxy[10];
	    Qbxy = pdfxy[1]+pdfxy[2]+pdfxy[3]+pdfxy[4]+pdfxy[0];
	    }
	  temp += (pqq1_DD(NF,y)+2.0*b0lnrf*pqq0_DD(y))*(Qxy/y-Qx)/(1.0-y)*jac; //plus term from pqq
	  temp += (pqq1_reg(NF,y)+2.0*b0lnrf*pqq0_reg(y))*Qxy/y*jac; // regular pqq term
	  temp += pqqbar1_reg(y)*Qbxy/y*jac; // regular pqqbar term
	  temp += 5.0*(pqg1_reg(y)+2.0*b0lnrf*pqg0_reg(y))*pdfxy[5]/y*jac; //regular pqg term
	  temp += pqQ1_reg(y)*(4.0*Qxy/y+4.0*Qbxy/y)*jac; //regular pqQ term
	  
	  yprime+=1.0/TRAPEZIUM; //step
	}
      temp += (pqq1_d1(NF)+2.0*b0lnrf*pqq0_d1() + pqq1_DDb(NF,x)+2.0*b0lnrf*pqq0_DDb(x))*Qx; // delta and DD-boundary contribution from pqq 
      temp*=0.5; // factor of two from 1/(2eps)
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}

// ---------------------------------------------------------------------

interpolator_g_2_1::interpolator_g_2_1(const double& NFl, const double& muf, const double& mur)
{
  NF = NFl;
  for (int i=-5; i<6; i++)
    {
      lopdfs.push_back(new interpolator_0_0(NFl,muf,i));
    }
  initialize(muf,mur,0);
}

interpolator_g_2_1::interpolator_g_2_1(const double& NFl, const double& muf, const double& mur, const int& jprtn)
{
  NF = NFl;
  lopdfs.push_back(new interpolator_0_0(NFl,muf,jprtn));
  initialize(muf,mur,0,jprtn);
}

interpolator_g_2_1::~interpolator_g_2_1()
{
  for (int i=0; i<lopdfs.size(); i++)
    {
      delete lopdfs[i];
    }
}

vector<double> interpolator_g_2_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{
  const double b0lnrf = log(mur*mur/muf/muf)*(11.0/4.0-NF/6.0);

  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      double y,xy,yprime,jac,gx;
      double pdfxy[11];
      
      gx = lopdfs[5]->give_f(x);
      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
	  for (int j=0; j<11; j++)
	    {
	      pdfxy[j] = lopdfs[j]->give_f(xy); 
	    }
	  	  
	  temp += (pgg1_DD(NF,y)+2.0*b0lnrf*pgg0_DD(y))*(pdfxy[5]/y-gx)/(1.0-y)*jac; //plus term from pgg
	  temp += (pgg1_reg(NF,y)+2.0*b0lnrf*pgg0_reg(y))*pdfxy[5]/y*jac; // regular pgg term
	  temp += (pgq1_reg(NF,y)+2.0*b0lnrf*pgq0_reg(y))*(pdfxy[0]+pdfxy[1]+pdfxy[2]+pdfxy[3]+pdfxy[4]+pdfxy[6]+pdfxy[7]+pdfxy[8]+pdfxy[9]+pdfxy[10])/y*jac; // regular pgq term
	  
	  yprime+=1.0/TRAPEZIUM; //step
	}
      
      temp += (pgg1_d1(NF)+2.0*b0lnrf*pgg0_d1(NF) + pgg1_DDb(NF,x)+2.0*b0lnrf*pgg0_DDb(x))*gx; // delta and DD-boundary contribution from pgg
      temp*=0.5; // factor of two from 1/(2eps)
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}

vector<double> interpolator_g_2_1::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{
  const double b0lnrf = log(mur*mur/muf/muf)*(11.0/4.0-NF/6.0);

  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      double y,xy,yprime,jac,gx,gxy;
      
      gx = lopdfs[0]->give_f(x);
      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
	  gxy = lopdfs[0]->give_f(xy); 
	  
	  if (jprtn == 0)
	    {
	      temp += (pgg1_DD(NF,y)+2.0*b0lnrf*pgg0_DD(y))*(gxy/y-gx)/(1.0-y)*jac; //plus term from pgg
	      temp += (pgg1_reg(NF,y)+2.0*b0lnrf*pgg0_reg(y))*gxy/y*jac; // regular pgg term
	    }
	  else
	    {
	  temp += (pgq1_reg(NF,y)+2.0*b0lnrf*pgq0_reg(y))*gxy/y*jac; // regular pgq term
	    }
	  
	  yprime+=1.0/TRAPEZIUM; //step
	}
      
      if (jprtn == 0)
	{
      temp += (pgg1_d1(NF)+2.0*b0lnrf*pgg0_d1(NF) + pgg1_DDb(NF,x)+2.0*b0lnrf*pgg0_DDb(x))*gx; // delta and DD-boundary contribution from pgg
	}
      temp*=0.5; // factor of two from 1/(2eps)
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}

// ---------------------------------------------------------------------

interpolator_q_2_2::interpolator_q_2_2(const double& NFl, const double& muf, const int& iprtn)
{
  NF = NFl;
  for (int i=-5; i<6; i++)
    {
      lopdfs.push_back(new interpolator_0_0(NFl,muf,i));
    }
  initialize(muf,muf,iprtn);
}

interpolator_q_2_2::interpolator_q_2_2(const double& NFl, const double& muf, const int& iprtn, const int& jprtn)
{
  NF = NFl;
  lopdfs.push_back(new interpolator_0_0(NFl,muf,jprtn));
  initialize(muf,muf,iprtn,jprtn);
}

interpolator_q_2_2::~interpolator_q_2_2()
{
  for (int i=0; i<lopdfs.size(); i++)
    {
      delete lopdfs[i];
    }
}

vector<double> interpolator_q_2_2::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{
  const double beta0 = 11.0/4.0-NF/6.0;
  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      double y,xy,yprime,jac,qx,Qxy;
      double pdfxy[11];
      
      qx = lopdfs[iprtn+5]->give_f(x);
      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
	  Qxy=0.0;
	  for (int j=0; j<11; j++)
	    {
	      pdfxy[j] = lopdfs[j]->give_f(xy); 
	      if ((j!=iprtn+5) and (j!=5))
		{
		  Qxy+=pdfxy[j];
		}	      
	    }
	  
	  temp += (pqq0pqq0_DD(y)-beta0*pqq0_DD(y))*(pdfxy[iprtn+5]/y-qx)/(1.0-y)*jac; //plus term from pqq0pqq0 and pqq0
	  temp += (pqq0pqq0_reg(y)+pgq0pqg0_reg(y)-beta0*pqq0_reg(y))*pdfxy[iprtn+5]/y*jac; // regular terms from the above and pgq0pqg0
	  temp += (pqg0pqq0_reg(y)+pgg0pqg0_reg(NF,y)-beta0*pqg0_reg(y))*pdfxy[5]/y*jac; //regular pqg-like terms
	  temp += pgq0pqg0_reg(y)*Qxy/y*jac; //regular pqQ-like term
	  
	  yprime+=1.0/TRAPEZIUM; //step
	}
      
      temp += (pqq0pqq0_d1()-beta0*pqq0_d1() + pqq0pqq0_DDb(x)-beta0*pqq0_DDb(x))*qx; // delta and DD-boundary contribution from pqq-like terms
      temp*=0.5; // factor of two from 1/(2eps^2)
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}

vector<double> interpolator_q_2_2::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{
  const double beta0 = 11.0/4.0-NF/6.0;
  vector<double> FGrid;
  double x;
  double temp=0.0;
    for (int i=0; i<NumberOfPoints; i++)
    {
        x = XGrid[i];
        double y,xy,yprime,jac,qx,qxy;
      
        qx = lopdfs[0]->give_f(x);
        yprime=0.0;
        for (int k=0; k<TRAPEZIUM; k++)
        {
            y = x + (1.0-x)*yprime*yprime;
            jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
            xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
            qxy = lopdfs[0]->give_f(xy); 
	  
            if (jprtn == iprtn)
            {
                temp += (pqq0pqq0_DD(y)-beta0*pqq0_DD(y))*(qxy/y-qx)/(1.0-y)*jac; //plus term from pqq0pqq0 and pqq0
                temp += (pqq0pqq0_reg(y)+pgq0pqg0_reg(y)-beta0*pqq0_reg(y))*qxy/y*jac; // regular terms from the above and pgq0pqg0
            }
            else if (jprtn == 0)
            {
                temp += (pqg0pqq0_reg(y)+pgg0pqg0_reg(NF,y)-beta0*pqg0_reg(y))*qxy/y*jac; //regular pqg-like terms
            }
            else
            {
                temp += pgq0pqg0_reg(y)*qxy/y*jac; //regular pqQ-like term
            }
            yprime+=1.0/TRAPEZIUM; //step
        }
        if (jprtn == iprtn)
        {
        temp += (pqq0pqq0_d1()-beta0*pqq0_d1() + pqq0pqq0_DDb(x)-beta0*pqq0_DDb(x))*qx; // delta and DD-boundary contribution from pqq-like terms
        }
        temp*=0.5; // factor of two from 1/(2eps^2)
        // storing and resetting
        FGrid.push_back(temp);
        temp=0.0;
    }
    return FGrid;
}

// --------------------------------------------------

interpolator_4q_2_2::interpolator_4q_2_2(const double& NFl, const double& muf, const int& iprtn)
{
  NF = NFl;
  for (int i=-5; i<6; i++)
    {
      lopdfs.push_back(new interpolator_0_0(NFl,muf,i));
    }
  initialize(muf,muf,iprtn);
}

interpolator_4q_2_2::~interpolator_4q_2_2()
{
  for (int i=-5; i<6; i++)
    {
      delete lopdfs[i];
    }
}

vector<double> interpolator_4q_2_2::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{
  // just a dummy
  vector<double> dummy;
  return dummy;
}

vector<double> interpolator_4q_2_2::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{
  const double beta0 = 11.0/4.0-NF/6.0;
  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      //cout << "\nStarting x = " << x;
      double y,xy,yprime,jac,Qxy,Qx,Qbxy;
      double pdfxy[11],pdfx[11];
      
      for (int j=0; j<11; j++)
	{
	  pdfx[j] = lopdfs[j]->give_f(x);
	} 
      if (iprtn == 6)
	Qx = pdfx[6]+pdfx[7]+pdfx[8]+pdfx[9];
      else if (iprtn == -6)
	Qx = pdfx[1]+pdfx[2]+pdfx[3]+pdfx[4];

      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
	  for (int j=0; j<11; j++)
	    {
	      pdfxy[j] = lopdfs[j]->give_f(xy); 
	    }
	  if (iprtn == -6)
	    {
	      Qxy = pdfxy[1]+pdfxy[2]+pdfxy[3]+pdfxy[4];
	      Qbxy = pdfxy[6]+pdfxy[7]+pdfxy[8]+pdfxy[9];
	    }
	  else if (iprtn == 6)
	    {
	      Qxy = pdfxy[6]+pdfxy[7]+pdfxy[8]+pdfxy[9];
	      Qbxy = pdfxy[1]+pdfxy[2]+pdfxy[3]+pdfxy[4];
	    }

	  temp += (pqq0pqq0_DD(y)-beta0*pqq0_DD(y))*(Qxy/y-Qx)/(1.0-y)*jac; //plus term from pqq0pqq0 and pqq0
	  temp += (pqq0pqq0_reg(y)+pgq0pqg0_reg(y)-beta0*pqq0_reg(y))*Qxy/y*jac; // regular terms from the above and pgq0pqg0
	  temp += 4.0*(pqg0pqq0_reg(y)+pgg0pqg0_reg(NF,y)-beta0*pqg0_reg(y))*pdfxy[5]/y*jac; //regular pqg-like terms
	  temp += pgq0pqg0_reg(y)*(3.0*Qxy/y+3.0*Qbxy/y+4.0*pdfxy[0]/y+4.0*pdfxy[10]/y)*jac; //regular pqQ-like term
	  
	  yprime+=1.0/TRAPEZIUM; //step
	}
      
      temp += (pqq0pqq0_d1()-beta0*pqq0_d1() + pqq0pqq0_DDb(x)-beta0*pqq0_DDb(x))*Qx; // delta and DD-boundary contribution from pqq-like terms
      temp*=0.5; // factor of two from 1/(2eps)
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}

// --------------------------------------------------

interpolator_5q_2_2::interpolator_5q_2_2(const double& NFl, const double& muf, const int& iprtn)
{
  NF = NFl;
  for (int i=-5; i<6; i++)
    {
      lopdfs.push_back(new interpolator_0_0(NFl,muf,i));
    }
  initialize(muf,muf,iprtn);
}

interpolator_5q_2_2::~interpolator_5q_2_2()
{
  for (int i=-5; i<6; i++)
    {
      delete lopdfs[i];
    }
}

vector<double> interpolator_5q_2_2::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{
  // just a dummy
  vector<double> dummy;
  return dummy;
}

vector<double> interpolator_5q_2_2::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{
  const double beta0 = 11.0/4.0-NF/6.0;
  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      //cout << "\nStarting x = " << x;
      double y,xy,yprime,jac,Qxy,Qx,Qbxy;
      double pdfxy[11],pdfx[11];
      
      for (int j=0; j<11; j++)
	{
	  pdfx[j] = lopdfs[j]->give_f(x);
	} 
      if (iprtn == 7)
	Qx = pdfx[6]+pdfx[7]+pdfx[8]+pdfx[9]+pdfx[10];
      else if (iprtn == -7)
	Qx = pdfx[1]+pdfx[2]+pdfx[3]+pdfx[4]+pdfx[0];

      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
	  for (int j=0; j<11; j++)
	    {
	      pdfxy[j] = lopdfs[j]->give_f(xy); 
	    }
	  if (iprtn == -7)
	    {
	    Qxy = pdfxy[1]+pdfxy[2]+pdfxy[3]+pdfxy[4]+pdfxy[0];
	    Qbxy = pdfxy[6]+pdfxy[7]+pdfxy[8]+pdfxy[9]+pdfxy[10];
	    }
	  else if (iprtn == 7)
	    {
	    Qxy = pdfxy[6]+pdfxy[7]+pdfxy[8]+pdfxy[9]+pdfxy[10];
	    Qbxy = pdfxy[1]+pdfxy[2]+pdfxy[3]+pdfxy[4]+pdfxy[0];
	    }

	  temp += (pqq0pqq0_DD(y)-beta0*pqq0_DD(y))*(Qxy/y-Qx)/(1.0-y)*jac; //plus term from pqq0pqq0 and pqq0
	  temp += (pqq0pqq0_reg(y)+pgq0pqg0_reg(y)-beta0*pqq0_reg(y))*Qxy/y*jac; // regular terms from the above and pgq0pqg0
	  temp += 5.0*(pqg0pqq0_reg(y)+pgg0pqg0_reg(NF,y)-beta0*pqg0_reg(y))*pdfxy[5]/y*jac; //regular pqg-like terms
	  temp += pgq0pqg0_reg(y)*(4.0*Qxy/y+4.0*Qbxy/y)*jac; //regular pqQ-like term
	  
	  yprime+=1.0/TRAPEZIUM; //step
	}
      temp += (pqq0pqq0_d1()-beta0*pqq0_d1() + pqq0pqq0_DDb(x)-beta0*pqq0_DDb(x))*Qx; // delta and DD-boundary contribution from pqq-like terms
      temp*=0.5; // factor of two from 1/(2eps)
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}

// ---------------------------------------------------------------------

interpolator_g_2_2::interpolator_g_2_2(const double& NFl, const double& muf)
{
  NF = NFl;
  for (int i=-5; i<6; i++)
    {
      lopdfs.push_back(new interpolator_0_0(NFl,muf,i));
    }
  initialize(muf,muf,0);
}

interpolator_g_2_2::interpolator_g_2_2(const double& NFl, const double& muf, const int& jprtn)
{
  NF = NFl;
  lopdfs.push_back(new interpolator_0_0(NFl,muf,jprtn));
  initialize(muf,muf,0,jprtn);
}

interpolator_g_2_2::~interpolator_g_2_2()
{
  for (int i=0; i<lopdfs.size(); i++)
    {
      delete lopdfs[i];
    }
}

vector<double> interpolator_g_2_2::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur)
{
  const double beta0 = 11.0/4.0 - NF/6.0;
  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      double y,xy,yprime,jac,gx;
      double pdfxy[11];
      
      gx = lopdfs[5]->give_f(x);
      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
	  for (int j=0; j<11; j++)
	    {
	      pdfxy[j] = lopdfs[j]->give_f(xy); 
	    }
	  	  
	  temp += (pgg0pgg0_DD(NF,y)-beta0*pgg0_DD(y))*(pdfxy[5]/y-gx)/(1.0-y)*jac; //plus term from pgg-like terms
	  temp += (pgg0pgg0_reg(NF,y)+2.0*NF*pgq0pqg0_reg(y)-beta0*pgg0_reg(y))*pdfxy[5]/y*jac; // regular pgg-like term
	  temp += (pgq0pqq0_reg(y)+pgg0pgq0_reg(NF,y)-beta0*pgq0_reg(y))*(pdfxy[0]+pdfxy[1]+pdfxy[2]+pdfxy[3]+pdfxy[4]+pdfxy[6]+pdfxy[7]+pdfxy[8]+pdfxy[9]+pdfxy[10])/y*jac; // regular pgq-like term
	  
	  yprime+=1.0/TRAPEZIUM; //step
	}
      
      temp += (pgg0pgg0_d1(NF)-beta0*pgg0_d1(NF) + pgg0pgg0_DDb(NF,x)-beta0*pgg0_DDb(x))*gx; // delta and DD-boundary contribution from pgg-like terms
      temp*=0.5; // factor of two from 1/(2eps)
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}

vector<double> interpolator_g_2_2::fill_FGrid(const int& iprtn, const vector<double>& XGrid, const double& muf, const double& mur, const int& jprtn)
{
  const double beta0 = 11.0/4.0 - NF/6.0;
  vector<double> FGrid;
  double x;
  double temp=0.0;
  for (int i=0; i<NumberOfPoints; i++)
    {
      x = XGrid[i];
      double y,xy,yprime,jac,gx,gxy;
      
      gx = lopdfs[0]->give_f(x);
      yprime=0.0;
      for (int k=0; k<TRAPEZIUM; k++)
	{
	  y = x + (1.0-x)*yprime*yprime;
	  jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
	  xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
	  
	  gxy = lopdfs[0]->give_f(xy); 
	  
	  if (jprtn == 0)
	    {
	      temp += (pgg0pgg0_DD(NF,y)-beta0*pgg0_DD(y))*(gxy/y-gx)/(1.0-y)*jac; //plus term from pgg-like terms
	      temp += (pgg0pgg0_reg(NF,y)+2.0*NF*pgq0pqg0_reg(y)-beta0*pgg0_reg(y))*gxy/y*jac; // regular pgg-like term
	    }
	  else
	    {
	      temp += (pgq0pqq0_reg(y)+pgg0pgq0_reg(NF,y)-beta0*pgq0_reg(y))*gxy/y*jac; // regular pgq-like term
	    }
	  
	  yprime+=1.0/TRAPEZIUM; //step
	}
      if (jprtn == 0)
	{
	  temp += (pgg0pgg0_d1(NF)-beta0*pgg0_d1(NF) + pgg0pgg0_DDb(NF,x)-beta0*pgg0_DDb(x))*gx; // delta and DD-boundary contribution from pgg-like terms
	}
      temp*=0.5; // factor of two from 1/(2eps)
      // storing and resetting
      FGrid.push_back(temp);
      temp=0.0;
    }
  return FGrid;
}
