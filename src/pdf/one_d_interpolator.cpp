

#include "one_d_interpolator.h"
#include <iomanip>
#include <stdlib.h> //: for exit()

using namespace std;


void InterpolatorBase::initialize()
{
     xmin=0.01;
     xmax=1.0-xmin;
     number_of_points=10000;
     
     //hh = number_of_points*(number_of_points-1)/(1.0-xmin);
     
     fill_Grids();
     
     for(int ix=0;ix<number_of_points;ix++)
          {
          int i1,i2,i3;
          if(ix==0)
               {
               i1=ix;i2=ix+1;i3=ix+2;
               }
          else if(ix==number_of_points-1)
               {
               i1=ix-2;i2=ix-1;i3=ix;
               }
          else
               {
               i1=ix-1;i2=ix;i3=ix+1;
               }
          double x1=XGrid[i1];
          double x2=XGrid[i2];
          double x3=XGrid[i3];
          double y1=FGrid[i1];
          double y2=FGrid[i2];
          double y3=FGrid[i3];
          
          double den=(x1-x2)*(x2-x3)*(x3-x1);
          double a = -(y1*(x2-x3) + y2*(x3-x1) + y3*(x1-x2))/den;
          double b = (y1*(x2*x2 - x3*x3) + y2*(x3*x3 - x1*x1) + y3*(x1*x1 - x2*x2))/den;
          double c = -(y1*(x2-x3)*x2*x3 + y2*(x3-x1)*x1*x3 + y3*(x1-x2)*x1*x2)/den;
          
          vector<double> temp;
          temp.push_back(a);temp.push_back(b);temp.push_back(c);
          
//          cout<<setprecision(16)<<"\n"<<i1<<" "<<i2<<" "<<i3<<" : "<<XGrid[i1]<<" "<<XGrid[i2]<<" "<<XGrid[i3];
//          cout<<setprecision(16)<<"\t -> "<<1.0-XGrid[i1]<<" "<<1.0-XGrid[i2]<<" "<<1.0-XGrid[i3];
//
//          cout<<"\n==>"<<temp[0]<<" "<<temp[1]<<" "<<temp[2]<<" den="<<den;
          CoeffGrid.push_back(temp);
          }
}


//: filling x_grid and f_grid
void InterpolatorBase::fill_Grids()
{
     for(int i=0;i<number_of_points;i++)
          {
          double x = xmin + (xmax-xmin)*g_dist(double(i)/double(number_of_points-1));

          //: this results in N points, x0,..x_(N-1), distributed
          //: like P(x) that is controled by g_dist(x)
          //: with x0=xmin and x_(N-1)=xmax assuming g(0)=0 and g(1)=1
          XGrid.push_back(x);
          FGrid.push_back(f_value(x));
          //cout<<setprecision(16)<<"\n* "<<i<<": x="<<XGrid[i]<<" F(x)="<<FGrid[i];
          }
}

//: default distribution of points in grid: 
double InterpolatorBase::g_dist(const double&x)
{
     return x*x;
     
}

double InterpolatorBase::give_f(const double& x)
{
     //PRE: receives a double x and an int iprtn. calculate_grid must have been executed beforehand.
     //POST: returns the interpolated value of the pdf at x
     
     if((x <= 0.0) | (x >= 1.0))
          {
          cerr << "ERROR in [" << __func__ << "]: x-value out of allowed range! (x = " << x << " )\nAborting...\n\n";
          exit(1);
          }
     
     //: finding ii such that x_i is closest to x
     int ii;
     if (x>xmin and x<xmax) //: normal operation (i.e. interpolation)
          {
          //: rr is a real number between 0 and N-1
          //: such that x=xmin+(xmax-xmin)*g(rr/(number_of_points-1))
          double rr = g_dist_inv( (x-xmin)/(xmax-xmin))*(number_of_points-1);
          
          int i1=floor(rr);//i1 is the closest integer smaller than rr
          int i2=i1+1;
          //: x is between x1 and x2
          double x1=xmin+(xmax-xmin)*g_dist(double(i1)/double(number_of_points-1));
          double x2=xmin+(xmax-xmin)*g_dist(double(i2)/double(number_of_points-1));
          
          if (x-x1<x2-x) ii=i1;
          else ii=i2;
          cout<<"\n normal operation: "
          <<x<<" in ("<<x1<<","<<x2<<") <- ("<<i1<<","<<i2<<"). "
          <<" We pick "<<xmin+(xmax-xmin)*g_dist(double(ii)/(number_of_points-1))
          <<endl;
          cout<<"\nwith coeffs "<<CoeffGrid[ii][0]<<" "<<CoeffGrid[ii][1]<<" "<<CoeffGrid[ii][2]<<endl;
          }
     else if (x<xmin)//: outside the interpolation region: extrapolating
          {
          ii=0;
          cout<<"\n outside the interpolation region : we pick ii="<<ii<<" -> x_i="
          <<xmin+(xmax-xmin)*g_dist(double(ii)/(number_of_points-1))<<endl;
          }
     else   //: outside the interpolation region: extrapolating
          {
          ii=number_of_points-1;
          cout<<"\n outside the interpolation region : we pick ii="<<ii<<" -> x_i="
          <<xmin+(xmax-xmin)*g_dist(double(ii)/(number_of_points-1))<<endl;
          }
     
     
     
     if((ii<0) or (ii>number_of_points-1))
          {
          cout << "\n"<<__func__<<"\t @@@@@@@@@@@@@@@\nZis can't be! L = " << ii << " ( x = " << x << " ) !\n@@@@@@@@@@@@@@@\n";
          }
     //
     
     return CoeffGrid[ii][2] + CoeffGrid[ii][1]*x + CoeffGrid[ii][0]*x*x;
}

double InterpolatorBase::g_dist_inv(const double& lambda)
{
     return  sqrt(lambda);
}





#include "constants.h"

double Interpolator_peaking_at_zero_and_one::g_dist(const double&x)
{
     //cout<<"\n hello from g_dist"<<endl;
     //return x;
      //return x*x;
      return 6.0* (pow(x,2.0)/2.0 - pow(x,3.0)/3.0);
     // return 30.0*((1.0/5.0)*pow(x,5.0)-(1.0/2.0)*pow(x,4.0)+(1.0/3.0)*pow(x,3.0));


}


double Interpolator_peaking_at_zero_and_one::g_dist_inv(const double& lambda)
{
     return find_recursive(lambda,0.0,1.0);

}


double Interpolator_peaking_at_zero_and_one::find_recursive(double f,double x0,double x1)
{
     double xmiddle=(x0+x1)/2.0;
     double fx = g_dist(xmiddle);
     double diff=fx-f;
     //cout<<setprecision(16)<<"\nxmiddle="<<xmiddle<<" fx="<<fx<<" f="<<f<<" diff="<<diff<<endl;
     
     if (fabs(diff)<1e-15)
          {
          return xmiddle;
          }
     else
          {
          if (diff>0) return find_recursive(f,x0,xmiddle);
          else return find_recursive(f, xmiddle, x1);
          }
}








