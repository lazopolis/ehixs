/** testing pdfs, and in particular the new interpolator class CashedInterpolator.h as compared to the old interpolator.h by Ste:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>

using namespace std;

#include "CPDF.h"

#include "gtest/gtest.h"




TEST(CashedPDFtest,check_use_cashing)
{
     double nf=5.0;
     double muf=70.0;
     double mur=70.0;
     int pert_order=2;
     int parton=0;//gluon
     int n_as=0;
     int n_eps=0;
     string provider = "MSTW";
     bool pdf_error=false;
     int jparton=0;//;source parton
     bool use_new_interpolator_class=false;
     bool use_cashing=true;
     CPDF* my_gluon=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
     
     string  cashing_status=my_gluon->cashing_status(0);
     cout<<"\n hello: "<<my_gluon->my_interpolators.size()<<endl;
     EXPECT_EQ(cashing_status,"No cashing available in the old implementation");
     //theproduction->evaluate_sector();
     
}

TEST(CashedPDFtest,check_use_cached_interpolators)
{
     double nf=5.0;
     double muf=70.0;
     double mur=70.0;
     int pert_order=2;
     int parton=0;//gluon
     int n_as=0;
     int n_eps=0;
     string provider = "MSTW";
     bool pdf_error=false;
     int jparton=0;//;source parton
     bool use_new_interpolator_class=true;
     bool use_cashing=true;
     CPDF* my_gluon=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
     
     string  cashing_status=my_gluon->cashing_status(0);
     EXPECT_EQ(cashing_status,"CachedInterpolator: cashing on , cashed file found, will read from it");
     //theproduction->evaluate_sector();
}



TEST(CashedPDFtest,check_00)
{
     double accumulated_difference=0.0;
     for (int iprtn=-5;iprtn<6;iprtn++)
          {
          double nf=5.0;
          double muf=70.0;
          double mur=70.0;
          int pert_order=2;
          int parton=iprtn;
          int jparton=iprtn;//;source parton
          int n_as=0;
          int n_eps=0;
          string provider = "MSTW";
          bool pdf_error=false;
          bool use_new_interpolator_class=false;
          bool use_cashing=true;
          CPDF* my_gluon_old=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          use_new_interpolator_class=true;
          CPDF* my_gluon_new=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          vector<double> testarray;
          for (int i=-18; i<1; i++)
               {
               testarray.push_back(0.9*pow(2.0,i));
               }
          for (int i=0;i<testarray.size();i++)
               {
               double f_old=my_gluon_old->give_f(testarray[i],0);
               double f_new=my_gluon_new->give_f(testarray[i],0);
               double diff = abs(f_old-f_new)/f_old;
               if (diff>1e-14)
                    {
                    cout<<"--- * iprtn="<<iprtn<<"\t old: "<<f_old<<"\tnew: "<<f_new<<"\trelative diff:"<<diff<<endl;
                    accumulated_difference += diff;
                    }
               
               }
          }
     EXPECT_EQ(accumulated_difference,0.0);
     //theproduction->evaluate_sector();
}

TEST(CashedPDFtest,check_11)
{
     double accumulated_difference=0.0;
     for (int iprtn=-5;iprtn<6;iprtn++)//: from -5 to 6
          {
          double nf=5.0;
          double muf=70.0;
          double mur=70.0;
          int pert_order=2;
          int parton=iprtn;
          int jparton=100;//;source parton
          int n_as=1;
          int n_eps=1;
          string provider = "MSTW";
          bool pdf_error=false;
          bool use_new_interpolator_class=false;
          bool use_cashing=true;
          CPDF* my_gluon_old=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          use_new_interpolator_class=true;
          CPDF* my_gluon_new=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          vector<double> testarray;
          for (int i=-18; i<1; i++)
               {
               testarray.push_back(0.9*pow(2.0,i));
               }
          for (int i=0;i<testarray.size();i++)
               {
               double f_old=my_gluon_old->give_f(testarray[i],0);
               double f_new=my_gluon_new->give_f(testarray[i],0);
               double diff = abs(f_old-f_new);
               if (diff>0.0)
                    {
                    cout<<"--- * iprtn="<<iprtn<<"\t old: "<<f_old<<"\tnew: "<<f_new<<endl;
                    }
               accumulated_difference += diff;
               }
          }
     EXPECT_EQ(accumulated_difference,0.0);
     //theproduction->evaluate_sector();
}

TEST(CashedPDFtest,check_11_from_xsame)
{
     double accumulated_difference=0.0;
     for (int iprtn=-5;iprtn<6;iprtn++)//: from -5 to 6
          {
          double nf=5.0;
          double muf=70.0;
          double mur=70.0;
          int pert_order=2;
          int parton=iprtn;
          int jparton=iprtn;//;source parton
          int n_as=1;
          int n_eps=1;
          string provider = "MSTW";
          bool pdf_error=false;
          bool use_new_interpolator_class=false;
          bool use_cashing=true;
          CPDF* my_gluon_old=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          use_new_interpolator_class=true;
          CPDF* my_gluon_new=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          vector<double> testarray;
          for (int i=-18; i<1; i++)
               {
               testarray.push_back(0.9*pow(2.0,i));
               }
          for (int i=0;i<testarray.size();i++)
               {
               double f_old=my_gluon_old->give_f(testarray[i],0);
               double f_new=my_gluon_new->give_f(testarray[i],0);
               double diff = abs(f_old-f_new);
               if (diff>0.0)
                    {
                    cout<<"--- * iprtn="<<iprtn<<"\t old: "<<f_old<<"\tnew: "<<f_new<<endl;
                    }
               accumulated_difference += diff;
               }
          }
     EXPECT_EQ(accumulated_difference,0.0);
     //theproduction->evaluate_sector();
}

TEST(CashedPDFtest,check_11_from_xdiff)
{
     double accumulated_difference=0.0;
     for (int iprtn=0;iprtn<2;iprtn++)//: from -5 to 6
          {
          double nf=5.0;
          double muf=70.0;
          double mur=70.0;
          int pert_order=2;
          int parton=iprtn;
          int jparton=iprtn-1;//;source parton
          int n_as=1;
          int n_eps=1;
          string provider = "MSTW";
          bool pdf_error=false;
          bool use_new_interpolator_class=false;
          bool use_cashing=true;
          CPDF* my_gluon_old=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          use_new_interpolator_class=true;
          CPDF* my_gluon_new=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          vector<double> testarray;
          for (int i=-18; i<1; i++)
               {
               testarray.push_back(0.9*pow(2.0,i));
               }
          for (int i=0;i<testarray.size();i++)
               {
               double f_old=my_gluon_old->give_f(testarray[i],0);
               double f_new=my_gluon_new->give_f(testarray[i],0);
               double diff = abs(f_old-f_new);
               if (diff>0.0)
                    {
                    cout<<"--- * iprtn="<<iprtn<<"\t old: "<<f_old<<"\tnew: "<<f_new<<endl;
                    }
               accumulated_difference += diff;
               }
          }
     EXPECT_EQ(accumulated_difference,0.0);
     //theproduction->evaluate_sector();
}


TEST(CashedPDFtest,check_21)
{
     double accumulated_difference=0.0;
     for (int iprtn=-5;iprtn<6;iprtn++)//: from -5 to 6
          {
          double nf=5.0;
          double muf=70.0;
          double mur=70.0;
          int pert_order=2;
          int parton=iprtn;
          int jparton=100;//;source parton
          int n_as=2;
          int n_eps=1;
          string provider = "MSTW";
          bool pdf_error=false;
          bool use_new_interpolator_class=false;
          bool use_cashing=true;
          CPDF* my_gluon_old=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          use_new_interpolator_class=true;
          CPDF* my_gluon_new=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          vector<double> testarray;
          for (int i=-18; i<1; i++)
               {
               testarray.push_back(0.9*pow(2.0,i));
               }
          for (int i=0;i<testarray.size();i++)
               {
               double f_old=my_gluon_old->give_f(testarray[i],0);
               double f_new=my_gluon_new->give_f(testarray[i],0);
               double diff = abs(f_old-f_new);
               if (diff>0.0)
                    {
                    cout<<"--- * iprtn="<<iprtn<<"\t old: "<<f_old<<"\tnew: "<<f_new<<endl;
                    }
               accumulated_difference += diff;
               }
          }
     EXPECT_EQ(accumulated_difference,0.0);
     //theproduction->evaluate_sector();
}


TEST(CashedPDFtest,check_21_fromxsame)
{
     double accumulated_difference=0.0;
     for (int iprtn=-5;iprtn<6;iprtn++)//: from -5 to 6
          {
          double nf=5.0;
          double muf=70.0;
          double mur=70.0;
          int pert_order=2;
          int parton=iprtn;
          int jparton=iprtn;//;source parton
          int n_as=2;
          int n_eps=1;
          string provider = "MSTW";
          bool pdf_error=false;
          bool use_new_interpolator_class=false;
          bool use_cashing=true;
          CPDF* my_gluon_old=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          use_new_interpolator_class=true;
          CPDF* my_gluon_new=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          vector<double> testarray;
          for (int i=-18; i<1; i++)
               {
               testarray.push_back(0.9*pow(2.0,i));
               }
          for (int i=0;i<testarray.size();i++)
               {
               double f_old=my_gluon_old->give_f(testarray[i],0);
               double f_new=my_gluon_new->give_f(testarray[i],0);
               double diff = abs(f_old-f_new);
               if (diff>0.0)
                    {
                    cout<<"--- * iprtn="<<iprtn<<"\t old: "<<f_old<<"\tnew: "<<f_new<<endl;
                    }
               accumulated_difference += diff;
               }
          }
     EXPECT_EQ(accumulated_difference,0.0);
     //theproduction->evaluate_sector();
}

TEST(CashedPDFtest,check_21_fromxdiff)
{
     double accumulated_difference=0.0;
     for (int iprtn=0;iprtn<2;iprtn++)//: from -5 to 6
          {
          double nf=5.0;
          double muf=70.0;
          double mur=70.0;
          int pert_order=2;
          int parton=iprtn;
          int jparton=iprtn-1;//;source parton
          int n_as=2;
          int n_eps=1;
          string provider = "MSTW";
          bool pdf_error=false;
          bool use_new_interpolator_class=false;
          bool use_cashing=true;
          CPDF* my_gluon_old=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          use_new_interpolator_class=true;
          CPDF* my_gluon_new=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          vector<double> testarray;
          for (int i=-18; i<1; i++)
               {
               testarray.push_back(0.9*pow(2.0,i));
               }
          for (int i=0;i<testarray.size();i++)
               {
               double f_old=my_gluon_old->give_f(testarray[i],0);
               double f_new=my_gluon_new->give_f(testarray[i],0);
               double diff = abs(f_old-f_new);
               if (diff>0.0)
                    {
                    cout<<"--- * iprtn="<<iprtn<<"\t old: "<<f_old<<"\tnew: "<<f_new<<endl;
                    }
               accumulated_difference += diff;
               }
          }
     EXPECT_EQ(accumulated_difference,0.0);
     //theproduction->evaluate_sector();
}


TEST(CashedPDFtest,check_22)
{
     double accumulated_difference=0.0;
     for (int iprtn=-5;iprtn<6;iprtn++)//: from -5 to 6
          {
          double nf=5.0;
          double muf=70.0;
          double mur=70.0;
          int pert_order=2;
          int parton=iprtn;
          int jparton=100;//;source parton
          int n_as=2;
          int n_eps=2;
          string provider = "MSTW";
          bool pdf_error=false;
          bool use_new_interpolator_class=false;
          bool use_cashing=true;
          CPDF* my_gluon_old=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          use_new_interpolator_class=true;
          CPDF* my_gluon_new=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          vector<double> testarray;
          for (int i=-18; i<1; i++)
               {
               testarray.push_back(0.9*pow(2.0,i));
               }
          for (int i=0;i<testarray.size();i++)
               {
               double f_old=my_gluon_old->give_f(testarray[i],0);
               double f_new=my_gluon_new->give_f(testarray[i],0);
               double diff = abs(f_old-f_new);
               if (diff>0.0)
                    {
                    cout<<"--- * iprtn="<<iprtn<<"\t old: "<<f_old<<"\tnew: "<<f_new<<endl;
                    }
               accumulated_difference += diff;
               }
          }
     EXPECT_EQ(accumulated_difference,0.0);
     //theproduction->evaluate_sector();
}

TEST(CashedPDFtest,check_22_fromxsame)
{
     double accumulated_difference=0.0;
     for (int iprtn=-5;iprtn<6;iprtn++)//: from -5 to 6
          {
          double nf=5.0;
          double muf=70.0;
          double mur=70.0;
          int pert_order=2;
          int parton=iprtn;
          int jparton=iprtn;//;source parton
          int n_as=2;
          int n_eps=2;
          string provider = "MSTW";
          bool pdf_error=false;
          bool use_new_interpolator_class=false;
          bool use_cashing=true;
          CPDF* my_gluon_old=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          use_new_interpolator_class=true;
          CPDF* my_gluon_new=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          vector<double> testarray;
          for (int i=-18; i<1; i++)
               {
               testarray.push_back(0.9*pow(2.0,i));
               }
          for (int i=0;i<testarray.size();i++)
               {
               double f_old=my_gluon_old->give_f(testarray[i],0);
               double f_new=my_gluon_new->give_f(testarray[i],0);
               double diff = abs(f_old-f_new);
               if (diff>0.0)
                    {
                    cout<<"--- * iprtn="<<iprtn<<"\t old: "<<f_old<<"\tnew: "<<f_new<<endl;
                    }
               accumulated_difference += diff;
               }
          }
     EXPECT_EQ(accumulated_difference,0.0);
     //theproduction->evaluate_sector();
}

TEST(CashedPDFtest,check_22_fromx_diff)
{
     double accumulated_difference=0.0;
     for (int iprtn=0;iprtn<2;iprtn++)//: from -5 to 6
          {
          double nf=5.0;
          double muf=70.0;
          double mur=70.0;
          int pert_order=2;
          int parton=iprtn;
          int jparton=iprtn-1;//;source parton
          int n_as=2;
          int n_eps=2;
          string provider = "MSTW";
          bool pdf_error=false;
          bool use_new_interpolator_class=false;
          bool use_cashing=true;
          CPDF* my_gluon_old=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          use_new_interpolator_class=true;
          CPDF* my_gluon_new=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          vector<double> testarray;
          for (int i=-18; i<1; i++)
               {
               testarray.push_back(0.9*pow(2.0,i));
               }
          for (int i=0;i<testarray.size();i++)
               {
               double f_old=my_gluon_old->give_f(testarray[i],0);
               double f_new=my_gluon_new->give_f(testarray[i],0);
               double diff = abs(f_old-f_new);
               if (diff>0.0)
                    {
                    cout<<"--- * iprtn="<<iprtn<<"\t old: "<<f_old<<"\tnew: "<<f_new<<endl;
                    }
               accumulated_difference += diff;
               }
          }
     EXPECT_EQ(accumulated_difference,0.0);
     //theproduction->evaluate_sector();
}



TEST(CashedPDFtest,check_11_gluon_new)
{
     double accumulated_difference=0.0;
     for (int iprtn=0;iprtn<1;iprtn++)//: from -5 to 6
          {
          double nf=5.0;
          double muf=70.0;
          double mur=70.0;
          int pert_order=2;
          int parton=iprtn;
          int jparton=1;//;source parton
          int n_as=1;
          int n_eps=1;
          string provider = "MSTW";
          bool pdf_error=false;
          bool use_new_interpolator_class=false;
          bool use_cashing=false;
          CPDF* my_gluon_old=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          use_new_interpolator_class=true;
          CPDF* my_gluon_new=new CPDF(nf, muf,  mur, pert_order,  parton, n_as, n_eps, provider, pdf_error, use_new_interpolator_class , use_cashing,jparton);
          vector<double> testarray;
          for (int i=-18; i<1; i++)
               {
               testarray.push_back(0.9*pow(2.0,i));
               }
          for (int i=0;i<testarray.size();i++)
               {
               double f_old=my_gluon_old->give_f(testarray[i],0);
               double f_new=my_gluon_new->give_f(testarray[i],0);
               double diff = abs(f_old-f_new);
               if (diff>1e-15)
                    {
                    cout<<"--- * iprtn="<<iprtn<<"\t old: "<<f_old<<"\tnew: "<<f_new<<endl;
                    accumulated_difference += diff;
                    }
               
               }
          }
     EXPECT_EQ(accumulated_difference,0.0);
     //theproduction->evaluate_sector();
}


int main(int argc, char**argv)
{
     cout << "\ntesting ehixs\n" << endl;
     
     ::testing::InitGoogleTest(&argc, argv);
     return  RUN_ALL_TESTS();
     
     return 0;
}































