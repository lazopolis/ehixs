/** testing UserInterface.*:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>


#include "UserInterface.h"

using namespace std;

#include "gtest/gtest.h"


int gargc;
char ** gargv;


 TEST(defaults,input_filename)
 {
     int locargc=1;
     char* locargv[1];
     locargv[0]="./ehixs";
     UserInterface UI;
     UI.ParseInput(locargc,locargv);
     EXPECT_EQ(UI.input_filename,"runcard");
 }


TEST(defaults,output_filename)
{
     int locargc=1;
     char* locargv[1];
     locargv[0]="./ehixs";
     UserInterface UI;
     UI.ParseInput(locargc,locargv);

     EXPECT_EQ(UI.output_filename,"ehixs_output");
}


TEST(defaults,m_higgs)
{
     int locargc=1;
     char* locargv[1];
     locargv[0]="./ehixs";
     UserInterface UI;
     UI.ParseInput(locargc,locargv);

     EXPECT_EQ(UI.m_higgs,125.0);
}

TEST(defaults,muf_over_mh)
{
     int locargc=1;
     char* locargv[1];
     locargv[0]="./ehixs";
     UserInterface UI;
     UI.ParseInput(locargc,locargv);
     
     EXPECT_EQ(UI.muf_over_mhiggs,0.5);
}

TEST(defaults,production)
{
     int locargc=1;
     char* locargv[1];
     locargv[0]="./ehixs";
     UserInterface UI;
     UI.ParseInput(locargc,locargv);
     
     EXPECT_EQ(UI.production,string("ggF"));
}

TEST(defaults,cutinfo)
{
     int locargc=1;
     char* locargv[1];
     locargv[0]="./ehixs";
     UserInterface UI;
     UI.ParseInput(locargc,locargv);
     
     EXPECT_EQ(UI.cut_info,false);
}

TEST(runcard,Etot)
{
     int locargc=1;
     char* locargv[1];
     locargv[0]="./ehixs";
     UserInterface UI;
     UI.ParseInput(locargc,locargv);
     
     EXPECT_EQ(UI.Etot,14000);
}

TEST(commandLine,epsrel)
{
     int locargc=3;
     char* locargv[3];
     locargv[0]="./ehixs";
     locargv[1]="--epsrel=0.0009";
     locargv[2]="--mineval=7000";

     UserInterface UI;
     UI.ParseInput(locargc,locargv);
     
     EXPECT_EQ(UI.epsrel,0.0009);
}

TEST(commandLine,mineval)
{
     int locargc=3;
     char* locargv[3];
     locargv[0]="./ehixs";
     locargv[1]="--epsrel=0.0009";
     locargv[2]="--mineval=7000";
     UserInterface UI;
     UI.ParseInput(locargc,locargv);
     
     EXPECT_EQ(UI.mineval,7000);
}

TEST(commandLine,info)
{
     int locargc=3;
     char* locargv[3];
     locargv[0]="./ehixs";
     locargv[1]="--info";
     locargv[2]="--mineval=7000";
     UserInterface UI;
     UI.ParseInput(locargc,locargv);
     
     EXPECT_EQ(UI.info,true);
}

TEST(commandLineShort,pole)
{
     int locargc=5;
     char* locargv[5];
     locargv[0]="./ehixs";
     locargv[1]="-p";
     locargv[2]="2";
     locargv[3]="-i";
     locargv[4]="cucu";
     UserInterface UI;
     UI.ParseInput(locargc,locargv);
//     UI.ParseInput(gargc,gargv);
     EXPECT_EQ(UI.pole,2);
}

TEST(commandLineShort,poleagain)
{
     int locargc=4;
     char* locargv[4];
     locargv[0]="./ehixs";
     locargv[1]="-p";
     locargv[2]="-2";
     locargv[3]="cucu";
     UserInterface UI;
     UI.ParseInput(locargc,locargv);
     //     UI.ParseInput(gargc,gargv);
     EXPECT_EQ(UI.pole,-2);
}

TEST(commandLineShort,unrecognized)
{
     int locargc=4;
     char* locargv[4];
     locargv[0]="./ehixs";
     locargv[1]="-q";
     locargv[2]="-2";
     locargv[3]="--shit";
     UserInterface UI;
     UI.ParseInput(locargc,locargv);
     //     UI.ParseInput(gargc,gargv);
     EXPECT_EQ(UI.pole,0);
}
/*
// --help causes UserInetrface to print out help message and exit
// so the following test will print out the message but will not finish
TEST(commandLineShort,help)
{
     int locargc=4;
     char* locargv[4];
     locargv[0]="./ehixs";
     locargv[1]="-q";
     locargv[2]="-2";
     locargv[3]="--help";
     UserInterface UI;
     UI.ParseInput(locargc,locargv);
     //     UI.ParseInput(gargc,gargv);
     EXPECT_EQ(UI.pole,0);
}
*/

int main(int argc, char**argv)
{
     gargc=argc;gargv=argv;
     cout << "\ntesting UserInterface.h and .cpp \n" << endl;
     ::testing::InitGoogleTest(&argc, argv);
     cout<<"\n argc="<<argc<<endl;
     for (int i=0;i<argc;i++)cout<<" "<<argv[i];
     cout<<"\n";
     return  RUN_ALL_TESTS();
     
     return 0;
}
