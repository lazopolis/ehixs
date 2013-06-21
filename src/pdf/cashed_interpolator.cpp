/** \file interpolator.cpp
 *
 * Implement interpolator.cpp
 *
 * \author, Stefu Bulli
 */

#include <iostream>
#include <fstream>
#include <vector>
#include <math.h>
#include <stdlib.h>

using namespace std;

#include "LHAPDF/LHAPDF.h"
#include "splitting_kernels.h"
#include "cashed_interpolator.h"

#define TRAPEZIUM 50000
//#define TRAPEZIUM 5




CashedInterpolator::CashedInterpolator(const double& NFl_, const double& muf_,const double & mur_, const int& iprtn_,const int& jprtn_,const int& n_as_,const int& n_eps_,bool cashing_,const string & gridname_,const int& member_):
NFl(NFl_),
muf(muf_),
mur(mur_),
iprtn(iprtn_),
jprtn(jprtn_),
n_as(n_as_),
n_eps(n_eps_),
cashing(cashing_),
gridname(gridname_),
member(member_)
{
     disambiguateFgrid();
     set_up_fgrid_and_coefficients();
}

void CashedInterpolator::set_up_fgrid_and_coefficients()
{
     my_cashing_status = "CachedInterpolator: no cashing attempted";
     NumberOfPoints = 2000;
     //NumberOfPoints=5;
     xmin = 1e-6; // !!! seems to be quite generally the default value for any lhapdf set
     
     fill_XGrid();
     fill_FGrid();
     
     vector<double> temp;
     
     for(int ix=0;ix<NumberOfPoints;ix++)
          {
          int i1,i2,i3;
          if(ix==0)
               {
               i1=ix;i2=ix+1;i3=ix+2;
               }
          else if(ix==NumberOfPoints-1)
               {
               i1=ix-2;i2=ix-1;i3=ix;
               }
          else
               {
               i1=ix-1;i2=ix;i3=ix+1;
               }
          double a,b,c;
          givecoeff(XGrid[i1],XGrid[i2],XGrid[i3],FGrid[i1],FGrid[i2],FGrid[i3],a,b,c);
          
          temp.push_back(a);temp.push_back(b);temp.push_back(c);
          CoeffGrid.push_back(temp);
          temp.clear();
          }
     
}
void CashedInterpolator::disambiguateFgrid()
{
     //: PDF convolutions are defined in two ways:
     //: (a) F_i(n_as,n_eps)
     //: (b) F_i_from_j(n_as,n_eps)
     if (jprtn == 100)//: case (a)
          {
          if ((n_as == 0) && (n_eps == 0))//: LO
               {
               if (abs(iprtn)==6 or abs(iprtn)==7)//: summing over u,d,s,c or the anti's
                    cur_fill_FGrid = &CashedInterpolator::ffgrid_LO_sum_light;
               else if (abs(iprtn)<6) //: (anti-) quark or gluon, no summation
                    cur_fill_FGrid = &CashedInterpolator::ffgrid_LO_nosum;
               else
                    {
                    cerr << "Error: CPDF: flavour number = (" << iprtn << ") not supported (also: weird)!";
                    exit(1);
                    }
               }
          else if ((n_as == 1) && (n_eps == 1))//: NLO
               {
               if(iprtn == 0)//: gluon
                    cur_fill_FGrid = &CashedInterpolator::ffgrid_NLO_gluon_new;
               else if ((abs(iprtn) == 6) or (abs(iprtn)==7))//: summing over u,d,s,c or the anti's
                    cur_fill_FGrid = &CashedInterpolator::ffgrid_NLO_quark_summed;
               else //: quark, no summation
                    cur_fill_FGrid = &CashedInterpolator::ffgrid_NLO_quark;
               }
          else if((n_as == 2) && ((n_eps == 1)|(n_eps == 2))) //:NNLO
               {
               if (n_eps == 1)//: 1/eps
                    {
                    if(iprtn == 0)
                         cur_fill_FGrid = &CashedInterpolator::ffgrid_NNLO_21_gluon;
                    else if (abs(iprtn)==6 or abs(iprtn)==7)//: summing over u,d,s,c or the anti's
                         cur_fill_FGrid = &CashedInterpolator::ffgrid_NNLO_21_quark_summed;
                    else //: quark, no summation
                         cur_fill_FGrid = &CashedInterpolator::ffgrid_NNLO_21_quark;
                    }
               else if (n_eps == 2) //: 1/eps^2
                    {
                    if(iprtn == 0)
                         cur_fill_FGrid = &CashedInterpolator::ffgrid_NNLO_22_gluon;
                    else if (abs(iprtn)==6 or abs(iprtn)==7)//: summing over u,d,s,c or the anti's
                         cur_fill_FGrid = &CashedInterpolator::ffgrid_NNLO_22_quark_summed;
                    else //: quark, no summation
                         cur_fill_FGrid = &CashedInterpolator::ffgrid_NNLO_22_quark;
                    }
               }
          else
               {
               cerr << "Error: CPDF: (n_as,n_eps) = (" << n_as << "," << n_eps << ") not supported!";
               exit(1);
               }
          } // end of case (a)
     else //: case (b), see above
          {
          if ((n_as == 0) && (n_eps == 0))//: LO no sum
               cur_fill_FGrid = &CashedInterpolator::ffgrid_LO_nosum;
          else if ((n_as == 1) && (n_eps == 1))//: NLO
               {
               if(iprtn == 0)//: gluon
                    cur_fill_FGrid = &CashedInterpolator::ffgrid_NLO_gluon_new;
               else //: quarks
                    cur_fill_FGrid = &CashedInterpolator::ffgrid_NLO_quark_from_X;
               }
          else if((n_as == 2) && ((n_eps == 1)|(n_eps == 2)))//: NNLO
               {
               if (n_eps == 1) //: 1/eps
                    {
                    if(iprtn == 0) //: gluon
                         cur_fill_FGrid = &CashedInterpolator::ffgrid_NNLO_21_gluon_from_X;
                    else //: quarks
                         cur_fill_FGrid = &CashedInterpolator::ffgrid_NNLO_21_quark_from_X;
                    }
               else if (n_eps == 2) //: 1/eps^2
                    {
                    if(iprtn == 0) //: gluon
                         cur_fill_FGrid = &CashedInterpolator::ffgrid_NNLO_22_gluon_from_X;
                    else //: quarks
                         cur_fill_FGrid = &CashedInterpolator::ffgrid_NNLO_22_quark_from_X;
                    }
               }
          else
               {
               cerr << "CPDF: (n_as,n_eps) = (" << n_as+"," << n_eps+") not supported!";
               exit(1);
               }
          }
    
}



void CashedInterpolator::fill_FGrid()
{
     if (cashing and cashed_file_found())
          {
          read_grid_from_file(FGrid);
          }
     else
          {
          (this->*cur_fill_FGrid)();
          write_fgrid_to_file();
          }
}

void CashedInterpolator::fill_XGrid()
{
     // returns a vector with NumberOfPoints points between xmin and 1.
     // !!! if this is changed, (the distribution of points, not the amount) then give_f must be changed as well.
     
     XGrid.clear();
     for(int i=0;i<NumberOfPoints;i++){
          double x = xmin + (1.0-xmin)*i*(i+1)/NumberOfPoints/(NumberOfPoints-1) - 1e-9;
          XGrid.push_back(x);
     }     
}

bool CashedInterpolator::cashed_file_found()
{
     bool found;
     stringstream ss;
     ss<<PDF_GRID_PREFIX<<gridname<<"_"<<member<<"_"<<n_as<<"_"<<n_eps<<"_"<<iprtn<<"_"<<jprtn<<"_"<<muf<<"_"<<mur<<"_"<<NFl;
     cashed_file_name=ss.str();
     ifstream ifile(cashed_file_name.c_str(),ifstream::in);
     if (ifile)
          {
          my_cashing_status="CachedInterpolator: cashing on , cashed file found, will read from it";
          should_write_fgrid_to_file=false;
          found=true;
          }
     else
          {
          should_write_fgrid_to_file=true;
          my_cashing_status="CachedInterpolator: cashing on , cashed file not found";
          found=false;
          }
     return found;
}


void CashedInterpolator::read_grid_from_file(vector<double> & )
{
     ifstream ifile(cashed_file_name.c_str(),ifstream::in);
     if (ifile)
          {
          cout<<": READING FROM FILE "<<cashed_file_name;
          FGrid.clear();
          string buffer;
          //std::getline(ifile,buffer);//:skipping first line
          while(std::getline(ifile, buffer))
               {
               double value=atof(buffer.c_str());
               FGrid.push_back(value);
               }
          }
     else
          {
          cout<<"\n Unable to read grid file: "<<cashed_file_name<<" despite the fact that the file is there. Will create the grid on the fy"<<endl;
          (this->*cur_fill_FGrid)();
          write_fgrid_to_file();
          }
}

void CashedInterpolator::write_fgrid_to_file()
{
     if (cashing and should_write_fgrid_to_file)
          {
          const char * output_fname = cashed_file_name.c_str();
          fstream my_local_outfile(output_fname, fstream::out);
     
          if(my_local_outfile.is_open())
               {
               //my_local_outfile <<cashed_file_name;
               my_local_outfile.precision(32);
               for (int i=0;i<FGrid.size();i++)
                    {
                    my_local_outfile<<FGrid[i]<<endl;
                    }
               my_local_outfile<<endl;
          
          
               }
          else
               {
               cout<<"\nfailbit = "<<my_local_outfile.fail()<<endl;
               cout << "Error opening file "<<output_fname<<endl;
               }
          my_local_outfile.close();
          }
}



//---------------------------------------------------------------------------------------------

void CashedInterpolator::givecoeff(const double& x1,const double& x2,const double& x3,const double& y1,const double& y2,const double& y3,double& a,double& b,double& c){
     //PRE: receives 9 doubles, the first 3 are x values, the second 3 y-values, the last 3 (by reference) the coefficients of the quadratic interpolation.
     //POST: the 3 coefficients are calculated and overwritten with their respective values.
     double den=(x1-x2)*(x2-x3)*(x3-x1);
     a = -(y1*(x2-x3) + y2*(x3-x1) + y3*(x1-x2))/den;
     b = (y1*(x2*x2 - x3*x3) + y2*(x3*x3 - x1*x1) + y3*(x1*x1 - x2*x2))/den;
     c = -(y1*(x2-x3)*x2*x3 + y2*(x3-x1)*x1*x3 + y3*(x1-x2)*x1*x2)/den;
}

//---------------------------------------------------------------------------------------------

double CashedInterpolator::give_f(const double& x){
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



void CashedInterpolator::ffgrid_LO_nosum()//: note that the value of jprtn is irrelevant here
{
     FGrid.clear();
     for(int i=0;i<NumberOfPoints;i++)
          {
          FGrid.push_back(LHAPDF::xfx(XGrid[i],muf,iprtn)/XGrid[i]);
          }
}

void CashedInterpolator::ffgrid_LO_sum_light()//: note that the value of jprtn is irrelevant here
{
     FGrid.clear();
     for(int i=0;i<NumberOfPoints;i++)
          {
          double temp=0.0;
          if (iprtn == 6 or iprtn == 7)//: summing over light quarks
               {
               temp = LHAPDF::xfx(XGrid[i],muf,1)+LHAPDF::xfx(XGrid[i],muf,2)+LHAPDF::xfx(XGrid[i],muf,3)+LHAPDF::xfx(XGrid[i],muf,4);
               if (iprtn==7) //: includes the bottom
                    temp += LHAPDF::xfx(XGrid[i],muf,5);
               }
          else if (iprtn == -6) //: summing over light anti-quarks
               {
               temp = LHAPDF::xfx(XGrid[i],muf,-1)+LHAPDF::xfx(XGrid[i],muf,-2)+LHAPDF::xfx(XGrid[i],muf,-3)+LHAPDF::xfx(XGrid[i],muf,-4);
               if (iprtn==-7) //: includes the antibottom
                    temp += LHAPDF::xfx(XGrid[i],muf,-5);
               }
               
          
          FGrid.push_back(temp/XGrid[i]);
          }
}


// ###################################
// NLO daughters
// ###################################



void CashedInterpolator::ffgrid_NLO_new()
{
     FGrid.clear();
     vector<CashedInterpolator*> quarks;
     vector<CashedInterpolator*> gluon;
     
     vector<CashedInterpolator*> * same_flavor;
     vector<CashedInterpolator*> * other_flavor;
     double (* plus_kernel)(const double& x);
     double (* reg_kernel)(const double& x);
     double (* reg_mixed_kernel)(const double& x);
     double (* delta_kernel)(const double& x);
     double (* boundary_kernel)(const double& x);

     if (iprtn==0)
          {
          same_flavor = &gluon;other_flavor= &quarks;
          plus_kernel=&pgg0_DD;
          reg_kernel=&pgg0_reg;
          reg_mixed_kernel=&pgq0_reg;
          delta_kernel=&pgg0_d1;
          boundary_kernel=&pgg0_DDb;
          }
     else
          {
          same_flavor = &quarks;other_flavor= &gluon;
          plus_kernel=&pqq0_DD;
          reg_kernel=&pqq0_reg;
          reg_mixed_kernel=&pqg0_reg;
          delta_kernel=&pqq0_d1;
          boundary_kernel=&pqq0_DDb;
          }
     //: allocating source partons
     if (jprtn==100)//: all partons as sources (but only same flavor quark with the daughter quark contributes)
          {
          gluon.push_back(new CashedInterpolator(NFl,muf,mur,0,0,0,0,cashing,gridname,member));
          quarks.push_back(new CashedInterpolator(NFl,muf,mur,iprtn,iprtn,0,0,cashing,gridname,member));
          
          }
     else if (jprtn==0)//: only gluon as source
          {
          gluon.push_back(new CashedInterpolator(NFl,muf,mur,0,0,0,0,cashing,gridname,member));
          }
     else //: a fixed quark as source (but it has to be the same flavor as the daughter quark)
          {
          if (iprtn==jprtn)
               {
               quarks.push_back(new CashedInterpolator(NFl,muf,mur,jprtn,jprtn,0,0,cashing,gridname,member));
               }
          }
     
     for (int i=0; i<NumberOfPoints; i++)
          {
          double x = XGrid[i];
          double temp=0.0;
          double yprime=0.0;
          double daughter_at_x=0.0;
          for (int k=0; k<TRAPEZIUM; k++)
               {
               double y = x + (1.0-x)*yprime*yprime;
               double jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
               double xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
               
               double plus=0.0;
               double reg=0.0;
               double reg_mixed=0.0;
               for (int i=0;i<(*same_flavor).size();i++) //: there might be zero or one same flavor source
                    {
                    double daughter_at_xy=(*same_flavor)[i]->give_f(xy);
                    daughter_at_x=(*same_flavor)[i]->give_f(x);
                    plus += (*plus_kernel)(y)*(daughter_at_xy/y-daughter_at_x)/(1.0-y)*jac;
                    reg  +=  (*reg_kernel)(y)*daughter_at_xy/y*jac;
                    }
               for (int i=0;i<(*other_flavor).size();i++)//: there might be zero or several other-flavor sources
                    {
                    double other_at_xy=(*other_flavor)[i]->give_f(xy);
                    reg_mixed  +=  (*reg_mixed_kernel)(y)*other_at_xy/y*jac;
                    }
               temp += plus+reg+reg_mixed;
               
               yprime+=1.0/TRAPEZIUM; //step
               }
          for (int i=0;i<(*same_flavor).size();i++)
               {
               temp += ((*delta_kernel)(NFl) + (*boundary_kernel)(x))*daughter_at_x; // delta and DD-boundary  
               }
          // storing and resetting
          FGrid.push_back(temp);
          temp=0.0;
          }
}


void CashedInterpolator::ffgrid_NLO_quark()
{
     FGrid.clear();
     CashedInterpolator* q=new CashedInterpolator(NFl,muf,mur,iprtn,iprtn,0,0,cashing,gridname,member);//: LO same flavor quark distribution
     CashedInterpolator* g=new CashedInterpolator(NFl,muf,mur,0,0,0,0,cashing,gridname,member);//: LO gluon distribution

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
}


void CashedInterpolator::ffgrid_NLO_quark_from_X()
{
     FGrid.clear();
     CashedInterpolator* q=new CashedInterpolator(NFl,muf,mur,jprtn,jprtn,0,0,cashing,gridname,member);//: LO same flavor X (not necessarily a quark)
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
}

void CashedInterpolator::ffgrid_NLO_quark_summed()
{
     FGrid.clear();
     int flavor_sign;
     if (iprtn==6 or iprtn==7) flavor_sign=1;
     else flavor_sign=-1;
     vector<CashedInterpolator*> LOpdfs;
     LOpdfs.push_back(new CashedInterpolator(NFl,muf,mur,flavor_sign*1,flavor_sign*1,0,0,cashing,gridname,member));
     LOpdfs.push_back(new CashedInterpolator(NFl,muf,mur,flavor_sign*2,flavor_sign*2,0,0,cashing,gridname,member));
     LOpdfs.push_back(new CashedInterpolator(NFl,muf,mur,flavor_sign*3,flavor_sign*3,0,0,cashing,gridname,member));
     LOpdfs.push_back(new CashedInterpolator(NFl,muf,mur,flavor_sign*4,flavor_sign*4,0,0,cashing,gridname,member));
     if (abs(iprtn)==7)
          {
          LOpdfs.push_back(new CashedInterpolator(NFl,muf,mur,flavor_sign*5,flavor_sign*5,0,0,cashing,gridname,member));
          }
     
     double x;
     double temp=0.0;
     for (int i=0; i<NumberOfPoints; i++)
          {
          x = XGrid[i];
          double dx,dxy,ux,uxy,sx,sxy,cx,cxy,gxy,y,xy,yprime,jac;
          vector<double> F_at_x;

          double sum_term3=0.0;
          for (int i=0;i<LOpdfs.size();i++)
               {
               F_at_x.push_back(LOpdfs[i]->give_f(x));
               sum_term3+=F_at_x[i];
               }
          
          yprime=0.0;
          for (int k=0; k<TRAPEZIUM; k++)
               {
               y = x + (1.0-x)*yprime*yprime;
               jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
               xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
               
               double sum_term1=0.0;
               double sum_term2=0.0;
               for (int i=0;i<LOpdfs.size();i++)
                    {
                    double F_at_xy=LOpdfs[i]->give_f(xy);
                    sum_term1+=F_at_xy/y-F_at_x[i];
                    sum_term2+=F_at_xy/y;
                    }
               
               temp += pqq0_DD(y)*(sum_term1)/(1.0-y)*jac; //plus term from pqq
               temp += pqq0_reg(y)*(sum_term2)*jac; // regular pqq term
               temp += 4.0*pqg0_reg(y)*gxy/y*jac; // regular pqg term (the only nonzero part)
               
               yprime+=1.0/TRAPEZIUM; //step
               }
          
          temp += (pqq0_d1() + pqq0_DDb(x))*(sum_term3); // delta and DD-boundary contribution from pqq
          // storing and resetting
          FGrid.push_back(temp);
          temp=0.0;
          }
}





void CashedInterpolator::ffgrid_NLO_gluon_new()
{
     FGrid.clear();
     vector<CashedInterpolator*> quarks;
     vector<CashedInterpolator*> gluon;
     //: allocating source partons
     if (jprtn==100)//: all partons as sources
          {
          gluon.push_back(new CashedInterpolator(NFl,muf,mur,0,0,0,0,cashing,gridname,member));
          for (int i=1; i<6; i++)
               {
               quarks.push_back(new CashedInterpolator(NFl,muf,mur,i,i,0,0,cashing,gridname,member));
               quarks.push_back(new CashedInterpolator(NFl,muf,mur,-i,-i,0,0,cashing,gridname,member));
               }
          }
     else if (jprtn==0)//: only gluon as source
          {
          gluon.push_back(new CashedInterpolator(NFl,muf,mur,0,0,0,0,cashing,gridname,member));
          }
     else if (jprtn==6)//: all quarks and antiquarks as sources (g_from_q summed over q)
          {
          for (int i=1; i<6; i++)
               {
               quarks.push_back(new CashedInterpolator(NFl,muf,mur,i,i,0,0,cashing,gridname,member));
               quarks.push_back(new CashedInterpolator(NFl,muf,mur,-i,-i,0,0,cashing,gridname,member));
               }
          }
     else //: a fixed quark as source
          {
          quarks.push_back(new CashedInterpolator(NFl,muf,mur,jprtn,jprtn,0,0,cashing,gridname,member));
          }
     double x;
     double temp=0.0;
     for (int i=0; i<NumberOfPoints; i++)
          {
          x = XGrid[i];

          double yprime=0.0;
          double gx=0.0;
          double gxy=0.0;
          for (int k=0; k<TRAPEZIUM; k++)
               {
               double y = x + (1.0-x)*yprime*yprime;
               double jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
               double xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
             
               double plus=0.0;
               double reg_gg=0.0;
               double reg_qg=0.0;
               for (int i=0;i<gluon.size();i++) //: there might be zero or one gluon source
                    {
                    gxy=gluon[i]->give_f(xy);
                    gx=gluon[i]->give_f(x);
                    plus += pgg0_DD(y)*(gxy/y-gx)/(1.0-y)*jac;
                    reg_gg  +=  pgg0_reg(y)*gxy/y*jac;
                    }
               for (int i=0;i<quarks.size();i++)//: there might be zero or several quark sources
                    {
                    double qxy=quarks[i]->give_f(xy);
                    reg_qg  +=  pgq0_reg(y)*qxy/y*jac;
                    }
               temp += plus+reg_gg+reg_qg;

               yprime+=1.0/TRAPEZIUM; //step
               }
          for (int i=0;i<gluon.size();i++)
               {
               temp += (pgg0_d1(NFl) + pgg0_DDb(x))*gx; // delta and DD-boundary contribution from pgg
               }
          // storing and resetting
          FGrid.push_back(temp);
          temp=0.0;
          }
}


void CashedInterpolator::ffgrid_NLO_gluon()
{
     FGrid.clear();
     vector<CashedInterpolator*> lopdfs;
     for (int i=-5; i<6; i++)
          {
          lopdfs.push_back(new CashedInterpolator(NFl,muf,mur,i,i,0,0,cashing,gridname,member));
          }
     double x;
     double temp=0.0;
     for (int i=0; i<NumberOfPoints; i++)
          {
          x = XGrid[i];
          double y,xy,yprime,jac,gx;
          double pdfxy[11];
          
          gx = lopdfs[5]->give_f(x);
          yprime=0.0;
//          cout<<"\n new point";
          for (int k=0; k<TRAPEZIUM; k++)
               {
               y = x + (1.0-x)*yprime*yprime;
               jac = 2.0*(1.0-x)*yprime/TRAPEZIUM;
               xy = x/y - 1e-9; // 1e-9 cutoff to avoid hitting 1 exactly
               //cout<<"\n pdfs at x*y = "<<xy;
               for (int j=0; j<11; j++)
                    {
                    pdfxy[j] = lopdfs[j]->give_f(xy);
               //     cout<<" "<<pdfxy[j];
                    }
               
               
               
               double plus = pgg0_DD(y)*(pdfxy[5]/y-gx)/(1.0-y)*jac; //plus term from pgg
               double reg_gg= pgg0_reg(y)*pdfxy[5]/y*jac; // regular pgg term
               double reg_qg= pgq0_reg(y)*(pdfxy[0]+pdfxy[1]+pdfxy[2]+pdfxy[3]+pdfxy[4]+pdfxy[6]+pdfxy[7]+pdfxy[8]+pdfxy[9]+pdfxy[10])/y*jac; // regular pgq term (the only nonzero part)
               temp += plus+reg_gg+reg_qg;
//               cout<<"\n y="<<y<<"\t"<<plus<<" "<<reg_gg<<" "<<reg_qg;
               yprime+=1.0/TRAPEZIUM; //step
               }
          
          temp += (pgg0_d1(NFl) + pgg0_DDb(x))*gx; // delta and DD-boundary contribution from pgg
//          cout<<"\n delta and boundary: "<<(pgg0_d1(NFl) + pgg0_DDb(x))*gx<<"\tNF="<<NFl;

          // storing and resetting
          FGrid.push_back(temp);
//          if (i % 100 ==0)
//               {
//               cout << "\nDone with x = " << x << "\t f(x)="<<temp<<"\t"<<gx;
//               }
          temp=0.0;
          }
}

void CashedInterpolator::ffgrid_NLO_gluon_from_X()
{
     FGrid.clear();
     vector<CashedInterpolator*> lopdfs;
     lopdfs.push_back(new CashedInterpolator(NFl,muf,mur,jprtn,jprtn,0,0,cashing,gridname,member));
     
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
                    temp += pgg0_DD(y)*(gxy/y-gx)/(1.0-y)*jac; //plus term from pgg
                    temp += pgg0_reg(y)*gxy/y*jac; // regular pgg term
                    }
               else
                    {
                    temp += pgq0_reg(y)*gxy/y*jac; // regular pgq term (the only nonzero part)
                    }
               yprime+=1.0/TRAPEZIUM; //step
               }
          if (jprtn == 0)
               {
               temp += (pgg0_d1(NFl) + pgg0_DDb(x))*gx; // delta and DD-boundary contribution from pgg
               }
          // storing and resetting
          FGrid.push_back(temp);
          temp=0.0;
          //cout << "\tDone with x = " << x;
          }
}

// ###################################
// NNLO daughters
// ###################################


void CashedInterpolator::ffgrid_NNLO_21_quark()
{
     FGrid.clear();
     vector<CashedInterpolator*> lopdfs;
     for (int i=-5; i<6; i++)
          {
          lopdfs.push_back(new CashedInterpolator(NFl,muf,mur,i,i,0,0,cashing,gridname,member));
          }
     const double b0lnrf = log(mur*mur/muf/muf)*(11.0/4.0-NFl/6.0);
     
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
               
               temp += (pqq1_DD(NFl,y)+2.0*b0lnrf*pqq0_DD(y))*(pdfxy[iprtn+5]/y-qx)/(1.0-y)*jac; //plus term from pqq
               temp += (pqq1_reg(NFl,y)+2.0*b0lnrf*pqq0_reg(y))*pdfxy[iprtn+5]/y*jac; // regular pqq term
               temp += pqqbar1_reg(y)*pdfxy[-iprtn+5]/y*jac; // regular pqqbar term
               temp += (pqg1_reg(y)+2.0*b0lnrf*pqg0_reg(y))*pdfxy[5]/y*jac; //regular pqg term
               temp += pqQ1_reg(y)*Qxy/y*jac; //regular pqQ term
               
               yprime+=1.0/TRAPEZIUM; //step
               }
          
          temp += (pqq1_d1(NFl)+2.0*b0lnrf*pqq0_d1() + pqq1_DDb(NFl,x)+2.0*b0lnrf*pqq0_DDb(x))*qx; // delta and DD-boundary contribution from pqq
          temp*=0.5; // factor of two from 1/(2eps)
          // storing and resetting
          FGrid.push_back(temp);
          temp=0.0;
          }
}

void CashedInterpolator::ffgrid_NNLO_21_quark_from_X()
{
     FGrid.clear();
     vector<CashedInterpolator*> lopdfs;
     lopdfs.push_back(new CashedInterpolator(NFl,muf,mur,jprtn,jprtn,0,0,cashing,gridname,member));
          
     const double b0lnrf = log(mur*mur/muf/muf)*(11.0/4.0-NFl/6.0);
     
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
                    temp += (pqq1_DD(NFl,y)+2.0*b0lnrf*pqq0_DD(y))*(qxy/y-qx)/(1.0-y)*jac; //plus term from pqq
                    temp += (pqq1_reg(NFl,y)+2.0*b0lnrf*pqq0_reg(y))*qxy/y*jac; // regular pqq term
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
               temp += (pqq1_d1(NFl)+2.0*b0lnrf*pqq0_d1() + pqq1_DDb(NFl,x)+2.0*b0lnrf*pqq0_DDb(x))*qx; // delta and DD-boundary contribution from pqq
               }
          temp*=0.5; // factor of two from 1/(2eps)
          // storing and resetting
          FGrid.push_back(temp);
          temp=0.0;
          }
}



void CashedInterpolator::ffgrid_NNLO_21_quark_summed()
{
     FGrid.clear();
     vector<CashedInterpolator*> lopdfs;
     for (int i=-5; i<6; i++)
          {
          lopdfs.push_back(new CashedInterpolator(NFl,muf,mur,i,i,0,0,cashing,gridname,member));
          }
     const double b0lnrf = log(mur*mur/muf/muf)*(11.0/4.0-NFl/6.0);
     
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
               if (abs(iprtn)==6)
                    {
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
               
                    temp += (pqq1_DD(NFl,y)+2.0*b0lnrf*pqq0_DD(y))*(Qxy/y-Qx)/(1.0-y)*jac; //plus term from pqq
                    temp += (pqq1_reg(NFl,y)+2.0*b0lnrf*pqq0_reg(y))*Qxy/y*jac; // regular pqq term
                    temp += pqqbar1_reg(y)*Qbxy/y*jac; // regular pqqbar term
                    temp += 4.0*(pqg1_reg(y)+2.0*b0lnrf*pqg0_reg(y))*pdfxy[5]/y*jac; //regular pqg term
                    temp += pqQ1_reg(y)*(3.0*Qxy/y+3.0*Qbxy/y+4.0*pdfxy[0]/y+4.0*pdfxy[10]/y)*jac; //regular pqQ term
                    }
               else
                    {
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
                    temp += (pqq1_DD(NFl,y)+2.0*b0lnrf*pqq0_DD(y))*(Qxy/y-Qx)/(1.0-y)*jac; //plus term from pqq
                    temp += (pqq1_reg(NFl,y)+2.0*b0lnrf*pqq0_reg(y))*Qxy/y*jac; // regular pqq term
                    temp += pqqbar1_reg(y)*Qbxy/y*jac; // regular pqqbar term
                    temp += 5.0*(pqg1_reg(y)+2.0*b0lnrf*pqg0_reg(y))*pdfxy[5]/y*jac; //regular pqg term
                    temp += pqQ1_reg(y)*(4.0*Qxy/y+4.0*Qbxy/y)*jac; //regular pqQ term
                    }
               
               
               yprime+=1.0/TRAPEZIUM; //step
               }
          
          temp += (pqq1_d1(NFl)+2.0*b0lnrf*pqq0_d1() + pqq1_DDb(NFl,x)+2.0*b0lnrf*pqq0_DDb(x))*Qx; // delta and DD-boundary contribution from pqq
          temp*=0.5; // factor of two from 1/(2eps)
          // storing and resetting
          FGrid.push_back(temp);
          temp=0.0;
          }
}

void CashedInterpolator::ffgrid_NNLO_21_gluon()

{
     FGrid.clear();
     vector<CashedInterpolator*> lopdfs;
     for (int i=-5; i<6; i++)
          {
          lopdfs.push_back(new CashedInterpolator(NFl,muf,mur,i,i,0,0,cashing,gridname,member));
          }
     const double b0lnrf = log(mur*mur/muf/muf)*(11.0/4.0-NFl/6.0);
     
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
               
               temp += (pgg1_DD(NFl,y)+2.0*b0lnrf*pgg0_DD(y))*(pdfxy[5]/y-gx)/(1.0-y)*jac; //plus term from pgg
               temp += (pgg1_reg(NFl,y)+2.0*b0lnrf*pgg0_reg(y))*pdfxy[5]/y*jac; // regular pgg term
               temp += (pgq1_reg(NFl,y)+2.0*b0lnrf*pgq0_reg(y))*(pdfxy[0]+pdfxy[1]+pdfxy[2]+pdfxy[3]+pdfxy[4]+pdfxy[6]+pdfxy[7]+pdfxy[8]+pdfxy[9]+pdfxy[10])/y*jac; // regular pgq term
               
               yprime+=1.0/TRAPEZIUM; //step
               }
          
          temp += (pgg1_d1(NFl)+2.0*b0lnrf*pgg0_d1(NFl) + pgg1_DDb(NFl,x)+2.0*b0lnrf*pgg0_DDb(x))*gx; // delta and DD-boundary contribution from pgg
          temp*=0.5; // factor of two from 1/(2eps)
          // storing and resetting
          FGrid.push_back(temp);
          temp=0.0;
          }
}

void CashedInterpolator::ffgrid_NNLO_21_gluon_from_X()
{
     const double b0lnrf = log(mur*mur/muf/muf)*(11.0/4.0-NFl/6.0);
     FGrid.clear();
     vector<CashedInterpolator*> lopdfs;
     
     lopdfs.push_back(new CashedInterpolator(NFl,muf,mur,jprtn,jprtn,0,0,cashing,gridname,member));
          
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
                    temp += (pgg1_DD(NFl,y)+2.0*b0lnrf*pgg0_DD(y))*(gxy/y-gx)/(1.0-y)*jac; //plus term from pgg
                    temp += (pgg1_reg(NFl,y)+2.0*b0lnrf*pgg0_reg(y))*gxy/y*jac; // regular pgg term
                    }
               else
                    {
                    temp += (pgq1_reg(NFl,y)+2.0*b0lnrf*pgq0_reg(y))*gxy/y*jac; // regular pgq term
                    }
               
               yprime+=1.0/TRAPEZIUM; //step
               }
          
          if (jprtn == 0)
               {
               temp += (pgg1_d1(NFl)+2.0*b0lnrf*pgg0_d1(NFl) + pgg1_DDb(NFl,x)+2.0*b0lnrf*pgg0_DDb(x))*gx; // delta and DD-boundary contribution from pgg
               }
          temp*=0.5; // factor of two from 1/(2eps)
          // storing and resetting
          FGrid.push_back(temp);
          temp=0.0;
          }
}



void CashedInterpolator::ffgrid_NNLO_22_quark()
{
     const double beta0 = 11.0/4.0-NFl/6.0;
     FGrid.clear();
     vector<CashedInterpolator*> lopdfs;
     for (int i=-5; i<6; i++)
          {
          lopdfs.push_back(new CashedInterpolator(NFl,muf,mur,i,i,0,0,cashing,gridname,member));
          }
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
               temp += (pqg0pqq0_reg(y)+pgg0pqg0_reg(NFl,y)-beta0*pqg0_reg(y))*pdfxy[5]/y*jac; //regular pqg-like terms
               temp += pgq0pqg0_reg(y)*Qxy/y*jac; //regular pqQ-like term
               
               yprime+=1.0/TRAPEZIUM; //step
               }
          
          temp += (pqq0pqq0_d1()-beta0*pqq0_d1() + pqq0pqq0_DDb(x)-beta0*pqq0_DDb(x))*qx; // delta and DD-boundary contribution from pqq-like terms
          temp*=0.5; // factor of two from 1/(2eps^2)
          // storing and resetting
          FGrid.push_back(temp);
          temp=0.0;
          }
}

void CashedInterpolator::ffgrid_NNLO_22_quark_from_X()
{
     const double beta0 = 11.0/4.0-NFl/6.0;
     FGrid.clear();
     vector<CashedInterpolator*> lopdfs;
     
     lopdfs.push_back(new CashedInterpolator(NFl,muf,mur,jprtn,jprtn,0,0,cashing,gridname,member));
     
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
                    temp += (pqg0pqq0_reg(y)+pgg0pqg0_reg(NFl,y)-beta0*pqg0_reg(y))*qxy/y*jac; //regular pqg-like terms
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
}



void CashedInterpolator::ffgrid_NNLO_22_quark_summed()

{
     const double beta0 = 11.0/4.0-NFl/6.0;
     FGrid.clear();
     vector<CashedInterpolator*> lopdfs;
     for (int i=-5; i<6; i++)
          {
          lopdfs.push_back(new CashedInterpolator(NFl,muf,mur,i,i,0,0,cashing,gridname,member));
          }
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
               if(abs(iprtn)==6)
                    {
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
                    temp += 4.0*(pqg0pqq0_reg(y)+pgg0pqg0_reg(NFl,y)-beta0*pqg0_reg(y))*pdfxy[5]/y*jac; //regular pqg-like terms
                    temp += pgq0pqg0_reg(y)*(3.0*Qxy/y+3.0*Qbxy/y+4.0*pdfxy[0]/y+4.0*pdfxy[10]/y)*jac; //regular pqQ-like term
                    }
               else
                    {
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
                    temp += 5.0*(pqg0pqq0_reg(y)+pgg0pqg0_reg(NFl,y)-beta0*pqg0_reg(y))*pdfxy[5]/y*jac; //regular pqg-like terms
                    temp += pgq0pqg0_reg(y)*(4.0*Qxy/y+4.0*Qbxy/y)*jac; //regular pqQ-like term

                    }
               yprime+=1.0/TRAPEZIUM; //step
               }
          
          temp += (pqq0pqq0_d1()-beta0*pqq0_d1() + pqq0pqq0_DDb(x)-beta0*pqq0_DDb(x))*Qx; // delta and DD-boundary contribution from pqq-like terms
          temp*=0.5; // factor of two from 1/(2eps)
          // storing and resetting
          FGrid.push_back(temp);
          temp=0.0;
          }
}


void CashedInterpolator::ffgrid_NNLO_22_gluon()
{
     const double beta0 = 11.0/4.0 - NFl/6.0;
     FGrid.clear();
     vector<CashedInterpolator*> lopdfs;
     for (int i=-5; i<6; i++)
          {
          lopdfs.push_back(new CashedInterpolator(NFl,muf,mur,i,i,0,0,cashing,gridname,member));
          }
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
               
               temp += (pgg0pgg0_DD(NFl,y)-beta0*pgg0_DD(y))*(pdfxy[5]/y-gx)/(1.0-y)*jac; //plus term from pgg-like terms
               temp += (pgg0pgg0_reg(NFl,y)+2.0*NFl*pgq0pqg0_reg(y)-beta0*pgg0_reg(y))*pdfxy[5]/y*jac; // regular pgg-like term
               temp += (pgq0pqq0_reg(y)+pgg0pgq0_reg(NFl,y)-beta0*pgq0_reg(y))*(pdfxy[0]+pdfxy[1]+pdfxy[2]+pdfxy[3]+pdfxy[4]+pdfxy[6]+pdfxy[7]+pdfxy[8]+pdfxy[9]+pdfxy[10])/y*jac; // regular pgq-like term
               
               yprime+=1.0/TRAPEZIUM; //step
               }
          
          temp += (pgg0pgg0_d1(NFl)-beta0*pgg0_d1(NFl) + pgg0pgg0_DDb(NFl,x)-beta0*pgg0_DDb(x))*gx; // delta and DD-boundary contribution from pgg-like terms
          temp*=0.5; // factor of two from 1/(2eps)
          // storing and resetting
          FGrid.push_back(temp);
          temp=0.0;
          }
}

void CashedInterpolator::ffgrid_NNLO_22_gluon_from_X()
{
     const double beta0 = 11.0/4.0 - NFl/6.0;
     FGrid.clear();
     vector<CashedInterpolator*> lopdfs;
     
     lopdfs.push_back(new CashedInterpolator(NFl,muf,mur,jprtn,jprtn,0,0,cashing,gridname,member));
          
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
//                    cout<<"\nhave worked from here "<<2.0*NFl*pgq0pqg0_reg(y)<<endl;
                    temp += (pgg0pgg0_DD(NFl,y)-beta0*pgg0_DD(y))*(gxy/y-gx)/(1.0-y)*jac; //plus term from pgg-like terms
                    temp += (pgg0pgg0_reg(NFl,y)+2.0*NFl*pgq0pqg0_reg(y)-beta0*pgg0_reg(y))*gxy/y*jac; // regular pgg-like term
                    }
               else
                    {
                    temp += (pgq0pqq0_reg(y)+pgg0pgq0_reg(NFl,y)-beta0*pgq0_reg(y))*gxy/y*jac; // regular pgq-like term
                    }
               
               yprime+=1.0/TRAPEZIUM; //step
               }
          if (jprtn == 0)
               {
               temp += (pgg0pgg0_d1(NFl)-beta0*pgg0_d1(NFl) + pgg0pgg0_DDb(NFl,x)-beta0*pgg0_DDb(x))*gx; // delta and DD-boundary contribution from pgg-like terms
               }
          temp*=0.5; // factor of two from 1/(2eps)
          // storing and resetting
          FGrid.push_back(temp);
          temp=0.0;
          }
}
