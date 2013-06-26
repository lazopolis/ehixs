
#include <stdlib.h> 
#include <string.h>
#include <fstream>
#include<vector>
#include <iostream>
#include <iomanip>
#include <map>
#include <math.h>
using namespace std;

extern"C"
{
  extern void hggtotal_(double*,double*,double*,double*);

 extern void set_com_to_higgsf_(const double&, 
				double&,
				int&,
				const double&,const double&,const double&,const double&);
 extern void set_hdecay_params_(double*,double*,int&);
 extern void set_constants_();
 extern void set_vegas_input_variables_(int&,int&,int&,
			int&,int&,double&,double&,int&);
 extern void set_common_flags_(int&,double&,double&,double&,int&);
 extern void setup_ewk_model_(int&,double*);
 extern void fitbr_(double&,double&);
 extern void oldfitbr_(double&,double&);
 extern void make_decay_grid_(const char *);
extern void  compare_grid_with_hdecay_live_();
  extern void read_grid_(const char*);
 extern void set_quark_data_(int&,double*);
 extern void set_pdfset_and_provider_(int&,int&);
 extern void calculate_qcd_wilson_coefficients_(const double &);
 extern void fix_higgs_width_scheme_(const int &);
 extern void setbinfname_(const char*,const int&);
}


void TrimSpaces( string& str);

int main(int argc, char** argv)
{

cout<<"\n* * * * * * * * * * * * * * * * * * * * * * * * *";
cout<<"\n*                                               *";
cout<<"\n*                                               *";
cout<<"\n*                                               *";
cout<<"\n*                ihixs                          *";
cout<<"\n*                                               *";
cout<<"\n*                version 1.0                    *";
cout<<"\n*                                               *";
cout<<"\n*                                               *";
cout<<"\n* * * * * * * * * * * * * * * * * * * * * * * * *";
cout<<"\n\n";


//cout<<"\n\n\t\t\t ihixs \n\n";
	
			
//: reading from runcard
string inputname = "./runcard";		
string output = "./results.dat";
bool out_given_by_user = false;
map<string,int> names2flags;
names2flags.insert ( pair<string,int>("LHC",0) );
names2flags.insert ( pair<string,int>("TEVATRON",1) );
names2flags.insert ( pair<string,int>("total",0) );
names2flags.insert ( pair<string,int>("gamma gamma",1) );
names2flags.insert ( pair<string,int>("WW",2) );
names2flags.insert ( pair<string,int>("ZZ",3) );
names2flags.insert ( pair<string,int>("bb",4) );
names2flags.insert ( pair<string,int>("no_width",-1) );
names2flags.insert ( pair<string,int>("full",0) );
names2flags.insert ( pair<string,int>("prod_gg",1) );
names2flags.insert ( pair<string,int>("prod_bb",2) );
names2flags.insert ( pair<string,int>("MSTW",1) );
names2flags.insert ( pair<string,int>("GJR",2) );
names2flags.insert ( pair<string,int>("ABKM",3) );
names2flags.insert ( pair<string,int>("MSTW_P",4) );
names2flags.insert ( pair<string,int>("MSTW_M",5) );
names2flags.insert ( pair<string,int>("MSTW90",6) );
names2flags.insert ( pair<string,int>("MSTW90_P",7) );
names2flags.insert ( pair<string,int>("MSTW90_M",8) );
	
//------------- command line arguments
	for(int k=1; k<argc; ++k)
    {
		if(((argv[k])[0])=='-')
		{
			if(!strcmp(argv[k],"-o"))
			{
				k++;
				output=argv[k];
				out_given_by_user=true;
			}
			else if(!strcmp(argv[k],"-i"))
			{
				k++;
				inputname=argv[k];
			}
			else
			{
				std::cout<<"Unknown option"<<std::endl;
				exit(1);
			}
		}
		else
		{
			std::cout<<"Unknown option"<<std::endl;
			exit(-1);
		}
    }
	string default_out_extension=string(".out");
	string def_out;
	if (!out_given_by_user)
		{
		def_out = (string(inputname)+default_out_extension);
		output=def_out;
		}
	cout<<"\n input="<<inputname<<"\t output="<<output;	
	
//============================== default input parameters
//:default pdf set directory
string pdfpath = ".";
int collider = 0; //LHC
double Etot=7000.0;
double mhiggs = 120.0;
double higgs_width=0.0;
double muf_over_mhiggs=0.5;
double mur_over_mhiggs=0.5;
int DecayMode = 0; // full
int ProductionMode = 1; //gg
int i_pdf_provider = 1;// 1:MSTW 2:GJR 3:ABKM
int effective_theory_flag = 0 ;// 0: exact LO,NLO, 1:effective theory LO,NLO
int no_error_flag = 0	;	// 0: pdf error  ,   1 : NO pdf error
double K_ewk=1.0;
double K_ewk_real=1.0;
double K_ewk_real_b=1.0;
//quarks
int number_of_quarks = 2;
double quark_data[300]={172.7, 0.0,1.0,4.2,0.0,1.0};
vector<double> user_quark_data;
int user_number_of_quarks=0;
//vector bosons
int number_of_vector_bosons=2;
double vector_boson_data[400]={80.398, 2.141,1.0,1.0,91.1876,2.4952,1.0,2.0};
vector<double> user_VB_data;
int user_number_of_VB=0;
//: vegas args
int in_nstart = 5000;
int in_nincrease = 500;
int in_mineval = 0;
int in_maxeval = 50000000;
int in_adjusting_phase_flag = 0;
double in_epsrel = 1e-2;
double in_epsabs = 1e-12;
int vegas_verbose = 0;
// end of default
ifstream in(inputname.c_str());
if (in.good())
{
vector<string> text;
string line;
string input_file_string;
double min_mh=30.0;
double max_mh=Etot;
bool max_mh_set=false;
string gridfilename="HdecayGrid.dat";
int higgs_width_scheme=0;
string bin_fname=string(inputname);
int binflag=0;
while (getline(in,line)) 
	{
	if (line.substr(0,1)!="#") 
		{
		size_t found_hash = line.find("#");
		if (found_hash!=string::npos)
			{
			line.erase(line.begin()+found_hash,line.end());
			}

		text.push_back(line);
            //std::cout<<"\n"<<line;
		input_file_string=input_file_string+line+"\n";
		}
	}
//============================== setting user input parameters

for (int j=0;j<int(text.size());j++)
		{		
		//reading a line from current run		
		string curline=text[j];
		size_t found;
		string option,argument;
		found=curline.find_first_of("=");
  		//defining option and argument
  		if (found!=string::npos) 
  			{
  			option=curline.substr(0,found);
            TrimSpaces(option);
  			argument=curline.substr(found+1,curline.size());
            TrimSpaces(argument);
  			}
            //cout<<"\n option = |"<<option<<"|\targument = "<<argument;
		//options
		if (option=="pdf_dir") {pdfpath=argument;}
		if (option=="collider") {collider=names2flags[argument];}
		if (option=="Etot") {Etot=atof(argument.c_str());}
		if (option=="mhiggs") {mhiggs=atof(argument.c_str());}
		if (option=="muf/mhiggs") {muf_over_mhiggs=atof(argument.c_str());}
		if (option=="mur/mhiggs") {mur_over_mhiggs=atof(argument.c_str());}
		if (option=="K_ewk") {K_ewk=atof(argument.c_str());}
		if (option=="K_ewk_real") {K_ewk_real=atof(argument.c_str());}
		if (option=="K_ewk_real_b") {K_ewk_real_b=atof(argument.c_str());}
		if (option=="DecayMode") {DecayMode=names2flags[argument];}
		//: quarks
		if (option=="heavy quark") 
			{
			double m,width,y;
			int xxx;
			xxx=sscanf(argument.c_str(),"%lf : %lf : %lf",&m,&width,&y);
			if (xxx==3)
				{
				//cout<<"\noption="<<option<<" m,y,w="<<m<<" "<<width<<" "<<y;
				user_number_of_quarks++ ;
				user_quark_data.push_back(double(m));
				user_quark_data.push_back(double(width));
				user_quark_data.push_back(double(y));
				}
			else
				{
				cout<<"\n Failed to read heavy quark line :"<<argument;
				}
			}
		if (option=="m_top") {quark_data[0]=atof(argument.c_str());}
		if (option=="Gamma_top") {quark_data[1]=atof(argument.c_str());}
		if (option=="Y_top") {
                  quark_data[2]=atof(argument.c_str());
                  // technical cutoff in Ytop : 10^-10
                  if (quark_data[2]<1e-10)
                        {
                        quark_data[2]=1e-10;
                        }
                  }
		if (option=="m_bot") {quark_data[3]=atof(argument.c_str());}
		if (option=="Gamma_bot") {quark_data[4]=atof(argument.c_str());}
		if (option=="Y_bot") {quark_data[5]=atof(argument.c_str());}
		//: Vector bosons
		if (option=="extra vector boson") 
			{
			double m,width,y,vbtype;
			int xxx;
			xxx=sscanf(argument.c_str(),"%lf : %lf : %lf : %lf",&m,&width,&y,&vbtype);
			if (xxx==4)
				{
				//cout<<"\noption="<<option<<" m,y,w="<<m<<" "<<width<<" "<<y<<" "<<vbtype;
				user_number_of_VB++ ;
				user_VB_data.push_back(double(m));
				user_VB_data.push_back(double(width));
				user_VB_data.push_back(double(y));
				user_VB_data.push_back(double(vbtype));
				
				}
			else
				{
				cout<<"\n Failed to read extra vector boson line :"<<argument;
				}
			}
		if (option=="m_W") {vector_boson_data[0]=atof(argument.c_str());}	
		if (option=="Gamma_W") {vector_boson_data[1]=atof(argument.c_str());}		
		if (option=="Y_W") {vector_boson_data[2]=atof(argument.c_str());}	
		
		if (option=="m_Z") {vector_boson_data[4]=atof(argument.c_str());}	
		if (option=="Gamma_Z") {vector_boson_data[5]=atof(argument.c_str());}		
		if (option=="Y_Z") {vector_boson_data[6]=atof(argument.c_str());}				
		// process flags
		if (option=="ProductionMode") 
			{
			string prod="prod_";
			ProductionMode=names2flags[prod+argument];
			}
		if (option=="pdf_provider") {i_pdf_provider=names2flags[argument];}
		if (option=="effective_theory_flag") {effective_theory_flag=atoi(argument.c_str());}
		if (option=="no_error_flag") {no_error_flag=atoi(argument.c_str());}
		if (option=="nstart") {in_nstart=atoi(argument.c_str());}
		if (option=="nincrease") {in_nincrease=atoi(argument.c_str());}
		if (option=="mineval") {in_mineval=atoi(argument.c_str());}
		if (option=="maxeval") {in_maxeval=atoi(argument.c_str());}
		if (option=="adapt to central only") {in_adjusting_phase_flag=atoi(argument.c_str());}
		if (option=="epsrel") {in_epsrel=atof(argument.c_str());}
		if (option=="epsabs") {in_epsabs=atof(argument.c_str());}
		if (option=="vegas_verbose") {vegas_verbose=atoi(argument.c_str());}
            if (option=="higgs_width") {
                  higgs_width=atof(argument.c_str());
                  cout<<"\nhiggs_width read : "<<higgs_width;
                  }
            if (option=="min_mh"){min_mh=atof(argument.c_str());}
            if (option=="max_mh"){max_mh=atof(argument.c_str());max_mh_set=true;}
            if (option=="higgs_width_grid"){gridfilename=argument;}
            if (option=="higgs_width_scheme") 
                  {
                  higgs_width_scheme=atoi(argument.c_str());
                  cout<<"\nHiggs width scheme set to "<<higgs_width_scheme;
                  }
            if (option=="bin_fname"){bin_fname=argument;}
            if (option=="bin_flag"){binflag=atoi(argument.c_str());}
            
        }
//============================== setting derived  parameters
  
	

	
	double    muf= mhiggs*muf_over_mhiggs;           
	double    mur =mhiggs*mur_over_mhiggs;          
    
	if (collider==1) 
            {
            Etot = 1960.0;
            if (not(max_mh_set)) {max_mh=Etot;}
            } // Tevatron case
	if (user_number_of_quarks>0)  
		{
		//cout<<"\n ++ : number_of_quarks from "<<number_of_quarks;
		number_of_quarks=number_of_quarks+user_number_of_quarks;
		for (int ii=0;ii<3*user_number_of_quarks;ii++)
			{
			quark_data[ii+6] = user_quark_data[ii];
			}
		//cout<<" to "<<number_of_quarks<<"\n";
		}
	
	if (user_number_of_VB>0)  
		{
		//cout<<"\n ++ : number_of_vector_boson from "<<number_of_vector_bosons;
		number_of_vector_bosons=number_of_vector_bosons+user_number_of_VB;
		for (int ii=0;ii<4*user_number_of_VB;ii++)
			{
			vector_boson_data[ii+8] = user_VB_data[ii];
			}
		//cout<<" to "<<number_of_vector_bosons<<"\n";
		}
      if (higgs_width>0.0)
            {
            DecayMode=0;
            cout<<"\n\n\n------>  WARNING: DecayMode set to \"total\" because Higgs width is set in runcard\n\n";
            }
    

//============================== transfering parameters to fortran

      setbinfname_(bin_fname.c_str(),binflag);
	set_constants_();
      fix_higgs_width_scheme_(higgs_width_scheme);
	
	set_com_to_higgsf_(pow(mhiggs,2.0)/pow(Etot,2.0), 
				Etot,
				collider,
				muf,mur,min_mh,max_mh);
	
	set_hdecay_params_(vector_boson_data,quark_data,DecayMode);
	
	
	
	set_vegas_input_variables_(in_nstart,in_nincrease,in_mineval,
			in_maxeval,in_adjusting_phase_flag,in_epsrel,in_epsabs,vegas_verbose);
	
	set_common_flags_(effective_theory_flag,K_ewk,K_ewk_real,K_ewk_real_b,ProductionMode);
	
	setup_ewk_model_(number_of_vector_bosons,vector_boson_data);
	
      
      //constructing an hdecay grid with all Gtot and BRs from 30 to 14000 with a step of 1GeV
      //string newgridfilename="Hdecay_complementary_low.dat";
      //make_decay_grid_(newgridfilename.c_str());
      // reading the already existing grid
      read_grid_(gridfilename.c_str());
      //
      //compare_grid_with_hdecay_live_(); 
      //compares with Hdecay if Hdecay is linked in - see branching ration.F
      //
      // fitbr(mhiggs) assigns the higgs width and 
	// constructs the interpolation data for 
	// branching ratio 
	// (see branchingratio.F)
	// a grid on yvar is constructed, 
	// where pH^2 = mhiggs^2 + tan(yvar)*mhiggs*GammaH	
	fitbr_(mhiggs,higgs_width);
	//oldfitbr_(mhiggs,higgs_width);
      
	set_quark_data_(number_of_quarks,quark_data);
	
	set_pdfset_and_provider_(i_pdf_provider,no_error_flag);
	
	calculate_qcd_wilson_coefficients_(mur);
	
//============================== running the loop over perturbative orders in hggtotal
	
	double   my_result[3];
	double   my_result_p[3];
	double   my_result_m[3];
	double   my_mcerror[3];
	hggtotal_(my_result,my_result_p,my_result_m,my_mcerror);

//============================== output section
	string header="\n* * * * * * * * * * * * * * * * * * * * * * * * *\n\n\n   ihixs results\n\n";
	

	
	cout<<header;
      cout<<"\n input="<<inputname<<"\t output="<<output;
	cout.precision(5);
	cout <<"\n LO   = "<<		my_result[0];
	cout.precision(2);
	cout<<"\t+/- "<<			my_mcerror[0];
	cout	<<"\t delta PDF: +"<<		my_result_p[0]<<"%"
		<<" -"<<				my_result_m[0]<<"%";
	
	cout.precision(5);
	cout <<"\n NLO  = "<<		my_result[1];
	cout.precision(2);
	cout<<"\t+/- "<<			my_mcerror[1];
	cout	<<"\t delta PDF: +"<<		my_result_p[1]<<"%"
		<<" -"<<				my_result_m[1]<<"%";
	
	cout.precision(5);
	cout <<"\n NNLO = "<<		my_result[2];
	cout.precision(2);
	cout<<"\t+/- "<<			my_mcerror[2];
	cout	<<"\t delta PDF: +"<<		my_result_p[2]<<"%"
		<<" -"<<				my_result_m[2]<<"%";
	cout<<"\n\n   ihixs exiting\n\n";

	ofstream outfile(output.c_str());
	outfile<<header;
	outfile.precision(5);
	outfile <<"\n LO   = "<<		my_result[0];
	outfile.precision(2);
	outfile<<"\t+/- "<<			my_mcerror[0];
	outfile	<<"\t delta PDF: +"<<		my_result_p[0]<<"%"
		<<" -"<<				my_result_m[0]<<"%";
	
	outfile.precision(5);
	outfile <<"\n NLO  = "<<		my_result[1];
	outfile.precision(2);
	outfile<<"\t+/- "<<			my_mcerror[1];
	outfile	<<"\t delta PDF: +"<<		my_result_p[1]<<"%"
		<<" -"<<				my_result_m[1]<<"%";
	
	outfile.precision(5);
	outfile <<"\n NNLO = "<<		my_result[2];
	outfile.precision(2);
	outfile<<"\t+/- "<<			my_mcerror[2];
	outfile	<<"\t delta PDF: +"<<		my_result_p[2]<<"%"
		<<" -"<<				my_result_m[2]<<"%";
      outfile<<"\n\n* * * * * * * * * * * * * * * * * * * * * * * * *\n\nRuncard used\n\n";
	outfile<<input_file_string;
	outfile.close();
}
else //if in.good()==false
      {
      cout<<"\n\nERROR : Unable to read the runcard : "<<inputname;
      cout<<"\n\nABORTING\n\n";
      }
	return 1;
}


void TrimSpaces( string& str)
{
    // Trim Both leading and trailing spaces
    size_t startpos = str.find_first_not_of(" \t"); // Find the first character position after excluding leading blank spaces
    size_t endpos = str.find_last_not_of(" \t"); // Find the first character position from reverse af
 
    // if all spaces or empty return an empty string
    if(( string::npos == startpos ) || ( string::npos == endpos))
    {
        str = "";
    }
    else
        str = str.substr( startpos, endpos-startpos+1 );
 
    /*
    // Code for  Trim Leading Spaces only
    size_t startpos = str.find_first_not_of(” \t”); // Find the first character position after excluding leading blank spaces
    if( string::npos != startpos )
        str = str.substr( startpos );
    */
 
    /*
    // Code for Trim trailing Spaces only
    size_t endpos = str.find_last_not_of(” \t”); // Find the first character position from reverse af
    if( string::npos != endpos )
        str = str.substr( 0, endpos+1 );
    */
}




