/** testing ehixs: 
  *
  * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
  */

#include <iostream>
#include <cmath>
#include "decay.h"
#include "Decay_bb.h"

#include "ggf_headers.h"

#include "histograms.hpp" //: particular available histograms are defined here. The base class for histograms is at CHistogram.h
#include "cuts.hpp" //: the particular available cuts are defined here. The base class is at CCut.h
//#define debug

using namespace std;

#include "gtest/gtest.h"
/*
class DecayGammaGammaTest: public ::testing::Test {
protected:
     virtual void SetUp() {
          UserInterface UI;
          UI.m_higgs=120.0;
          
          dec.init(UI,&the_hatch);
          
          
          
          
          
          xvegas[0]=0.3;
          xvegas[1]=0.78;
          the_hatch.SetVars(xvegas);
          
          
          
          PH=fvector(Model.higgs.m,0.0,0.0,0.0);
          dec.do_decay(PH);
          decP = dec.decay_events[0]->p;
          
     }
     
     // virtual void TearDown() {}
     double xvegas[2];
     Decay_gammagamma dec;
     CModel Model;
     fvector PH;
     Momenta decP;
     TheHatch the_hatch;
};

TEST_F(DecayGammaGammaTest,EventNumber){
     
     EXPECT_EQ(dec.decay_events.size(),1);
}

TEST_F(DecayGammaGammaTest,MomentumConservation)
{
     fvector pfinal=decP["gamma1"] + decP["gamma2"];
     fvector should_be_null_vector = pfinal - PH;
     
     EXPECT_LT(should_be_null_vector.square(),1e-13);
}


class DecayGammaGammaTest2: public ::testing::Test {
protected:
     virtual void SetUp() {
          UserInterface UI;
          UI.m_higgs=120.0;
          UI.sector_control=0;
          dec.init(UI,&the_hatch);
          
          
          xvegas[0]=0.3;
          xvegas[1]=0.78;
          the_hatch.SetVars(xvegas);
          double mhsq=Model.higgs.m*Model.higgs.m;
          double px=0.1;
          double py=0.2;
          double pz=0.3;
          PH=fvector(sqrt(mhsq-px*px-py*py-pz*pz),px,py,pz);
          dec.do_decay(PH);
          decP = dec.decay_events[0]->p;
     }
     
     // virtual void TearDown() {}
     double xvegas[2];
     Decay_gammagamma dec;
     CModel Model;
     fvector PH;
     Momenta decP;
     TheHatch the_hatch;
};

TEST_F(DecayGammaGammaTest2,MomentumConservation)
{
     fvector pfinal=decP["gamma1"] + decP["gamma2"];
     fvector should_be_null_vector = pfinal - PH;
     
     EXPECT_LT(should_be_null_vector.square(),1e-13);
}
*/
/*

class DecaybbTest: public ::testing::Test {
protected:
     virtual void SetUp() {
          
          UserInterface UI;
          UI.m_higgs=125.0;
          UI.sector_control=0;
          dec.init(UI,&the_hatch);
          xvegas[0]=0.3;
          xvegas[1]=0.78;
          the_hatch.SetVars(xvegas);
          double mhsq=Model.higgs.m*Model.higgs.m;
          double px=0.1;
          double py=0.2;
          double pz=0.3;
          PH=fvector(sqrt(mhsq-px*px-py*py-pz*pz),px,py,pz);
          
          vector<double> alpha_s(1,0.117);
          vector<double> yukawa_b_vector(1,0.123);
          
          
          //: and we set the couplings to decay
          dec.set_alpha_s(alpha_s);
          dec.set_y_b(yukawa_b_vector);
          
          dec.do_decay(PH);
          decP = dec.decay_events[0]->p;
         
     }
     
     // virtual void TearDown() {}
     double xvegas[2];
     Decay_bb dec;
     CModel Model;
     fvector PH;
     Momenta decP;
     TheHatch the_hatch;

};

TEST_F(DecaybbTest,DimensionOfIntegration){EXPECT_EQ(dec.dimension_of_integration(),2);}


TEST_F(DecaybbTest,SectorSetting){EXPECT_EQ(dec.give_sector(),0);}

 TEST_F(DecaybbTest,SectorName){EXPECT_EQ(dec.sector_name(),"h->bb LO");}
//TEST_F(DecaybbTest,MomentumConservationGenericx){EXPECT_LT(abs(decP->phbb1[1] + decP->phbb2[1]-decP->pH[1]),1e-13);}
//TEST_F(DecaybbTest,MomentumConservationGenericy){EXPECT_LT(abs(decP->phbb1[2] + decP->phbb2[2]-decP->pH[2]),1e-13);}
//TEST_F(DecaybbTest,MomentumConservationGenericz){EXPECT_LT(abs(decP->phbb1[3] + decP->phbb2[3]-decP->pH[3]),1e-13);}
//TEST_F(DecaybbTest,MomentumConservationGenericE){EXPECT_LT(abs(decP->phbb1[0] + decP->phbb2[0]-decP->pH[0]),1e-13);}
TEST_F(DecaybbTest,LOEventValue){EXPECT_EQ(dec.decay_events[0]->w,0.4514728376319909/2.0);}
TEST_F(DecaybbTest,MomentumConservation)
{
     
     fvector pfinal=decP["b1"] + decP["b2"];
     fvector should_be_null_vector = pfinal - PH;
     
     EXPECT_LT(should_be_null_vector.square(),1e-13);
}

//:-----------------------------------


class ProductionTest: public ::testing::Test {
protected:
     virtual void SetUp() {
          theproduction = new ProductionMockUp();
          UI.number_of_flavours=5;
          UI.muf_over_mhiggs=0.5;
          UI.m_higgs=125.0;
          UI.mur_over_mhiggs=0.5;
          UI.perturbative_order=1;
          UI.pdf_provider="MSTW";
          UI.pdf_error=0;
          UI.Etot = 8000.0;
          
          
          theproduction->init(UI,&the_hatch);
     }
     
     Production* theproduction;
     UserInterface UI;
     TheHatch the_hatch;
};


TEST_F(ProductionTest,DimInt)
{
     int dim=theproduction->dimension_of_integration();
     EXPECT_EQ(dim,7);
     //theproduction->evaluate_sector();

}

TEST_F(ProductionTest,NoEvents)
{
     int number_of_events=theproduction->production_events.size();
     EXPECT_EQ(number_of_events,0);
     //theproduction->evaluate_sector();
     
}





TEST(TheHatchTest,VEGASDIM)
{
     TheHatch myhatch;
     double * myvars = myhatch.RequestPtr();
     myhatch.RequestVar("VEGAS");
     myhatch.RequestVar("VEGAS");
     myhatch.RequestVar("VEGAS");
     myhatch.RequestVar("VEGAS");
     myhatch.RequestVar("FLAT");
     myhatch.RequestVar("VEGAS");
     myhatch.RequestVar("FLAT");

     double  vegas[5]={0.1,0.2,0.3,0.4,0.5};
     myhatch.SetVars(vegas);
     
     EXPECT_EQ(myhatch.GetVEGASDim(),5);
}


TEST(TheHatchTest,VEGASVARCHECK)
{
     TheHatch myhatch;
     double * myvars = myhatch.RequestPtr();
     myhatch.RequestVar("FLAT");
     myhatch.RequestVar("VEGAS");
     myhatch.RequestVar("VEGAS");
     myhatch.RequestVar("VEGAS");
     myhatch.RequestVar("FLAT");
     myhatch.RequestVar("VEGAS");
     myhatch.RequestVar("FLAT");
     myhatch.RequestVar("FLAT");
     myhatch.RequestVar("VEGAS");
     myhatch.RequestVar("VEGAS");
     double  vegas[6]={0.1,0.2,0.3,0.4,0.5,0.6};
     myhatch.SetVars(vegas);
     
     EXPECT_EQ(myvars[5],0.4);
}

TEST(TheHatchTest,MULTIPLE_XX_ARRAYS)
{
     TheHatch myhatch;
     double * myvars = myhatch.RequestPtr();
     myhatch.RequestVar("FLAT");
     myhatch.RequestVar("VEGAS");
     myhatch.RequestVar("VEGAS");
     myhatch.RequestVar("VEGAS");
     myhatch.RequestVar("FLAT");
     myhatch.RequestVar("VEGAS");
     double * myvars2 = myhatch.RequestPtr();

     myhatch.RequestVar("FLAT");
     myhatch.RequestVar("FLAT");
     myhatch.RequestVar("VEGAS");
     myhatch.RequestVar("VEGAS");
     double  vegas[6]={0.1,0.2,0.3,0.4,0.5,0.6};
     myhatch.SetVars(vegas);
     
     EXPECT_EQ(myvars[5],0.4);
     EXPECT_EQ(myvars2[2],0.5);
}



//:-----------------------------------


class GluonFusionInclusiveTest: public ::testing::Test {
protected:
     virtual void SetUp() {
          theproduction = new GluonFusionInclusive();
          UI.number_of_flavours=5;
          UI.muf_over_mhiggs=0.5;
          UI.m_higgs=125.0;
          UI.mur_over_mhiggs=0.5;
          UI.perturbative_order=0;
          UI.pdf_provider="MSTW";
          UI.pdf_error=0;
          UI.Etot = 8000.0;
          UI.sector_control=0;
          
          theproduction->init(UI,&the_hatch);
     }
     
     Production* theproduction;
     UserInterface UI;
     TheHatch the_hatch;
};


TEST_F(GluonFusionInclusiveTest,DimInt)
{
     int dim=theproduction->dimension_of_integration();
     EXPECT_EQ(dim,1);
     //theproduction->evaluate_sector();
     
}

TEST_F(GluonFusionInclusiveTest,SecName)
{
     string name_of_sector=theproduction->sector_name();
     EXPECT_EQ(name_of_sector,"gg delta effective LO");
     //theproduction->evaluate_sector();
     
}

*/


/*
class GluonFusionTest: public ::testing::Test {
protected:
     virtual void SetUp() {
          theproduction = new GluonFusion();
          UI.number_of_flavours=5;
          UI.muf_over_mhiggs=0.5;
          UI.m_higgs=125.0;
          UI.mur_over_mhiggs=0.5;
          UI.perturbative_order=0;
          UI.pdf_provider="MSTW";
          UI.pdf_error=0;
          UI.Etot = 8000.0;
          UI.sector_control=0;
          
          theproduction->init(UI,&the_hatch);
     }
     
     Production* theproduction;
     UserInterface UI;
     TheHatch the_hatch;
};


TEST_F(GluonFusionTest,DimInt)
{
     int dim=theproduction->dimension_of_integration();
     EXPECT_EQ(dim,1);
     //theproduction->evaluate_sector();
     
}

TEST_F(GluonFusionTest,SecName)
{
     string name_of_sector=theproduction->sector_name();
     EXPECT_EQ(name_of_sector,"ggF LO");
     //theproduction->evaluate_sector();
     
}

TEST(CutTest,PT)
{
     Pt_cut mycut("b1",45.0,"pt_cut_b1");
     Momenta my_momenta;
     my_momenta.init_fvector("b1");
     my_momenta.init_fvector("b2");
     my_momenta["b1"] = fvector(5.0,0.0,3.0,4.0);
     my_momenta["b2"] = fvector(5.0,0.0,3.0,4.0)*100.0;
     Event * my_event = new Event(32.0,my_momenta);
     EXPECT_EQ(mycut(my_event),false);
}


class GluonFusionTestS1: public ::testing::Test {
protected:
     virtual void SetUp() {
          theproduction = new GluonFusion();
          UI.number_of_flavours=5;
          UI.muf_over_mhiggs=0.5;
          UI.m_higgs=125.0;
          UI.mur_over_mhiggs=0.5;
          UI.perturbative_order=0;
          UI.pdf_provider="MSTW";
          UI.pdf_error=0;
          UI.Etot = 8000.0;
          UI.sector_control=1;
          
          theproduction->init(UI,&the_hatch);
     }
     
     Production* theproduction;
     UserInterface UI;
     TheHatch the_hatch;
};


TEST_F(GluonFusionTestS1,DimInt)
{
     int dim=theproduction->dimension_of_integration();
     EXPECT_EQ(dim,4);
     //theproduction->evaluate_sector();
     
}

TEST_F(GluonFusionTestS1,SecName)
{
     string name_of_sector=theproduction->sector_name();
     EXPECT_EQ(name_of_sector,"gg NLO HARD");
     //theproduction->evaluate_sector();
     
}






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
*/

int main(int argc, char**argv)
{
  cout << "\ntesting ehixs\n" << endl;
     
     ::testing::InitGoogleTest(&argc, argv);
     return  RUN_ALL_TESTS();

  return 0;
}































