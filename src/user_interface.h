#ifndef USER_INTERFACE_H
#define USER_INTERFACE_H

#include<string>
#include<vector>
#include<iostream>

using namespace std;

//: declaring the getopt specialized class option (not small initial 'o')
class option;

/** Option descriptor.
 *
 * Encloses all the information relevant for the option parsing. */
class Option {
public:
     /** Option type */
     string type;
     /** Full name */
     std::string name;
     /** Abreviated name */
     char short_name;
     /** Description (for output) */
     std::string desc;
     
     Option(const std::string& name_, char short_name_, const std::string& desc_, const string& type_): name(name_),short_name(short_name_),desc(desc_),type(type_){};
     
     int get_type(){if (type=="Required") return 1;else if (type=="Optional")  return 2;else if (type=="None") return 0;else cout<<"\n wrong option option"<<endl;exit(1);}
     /** Set and match... */
     virtual void set(const string&) 
     {}
     /** Print... */
     virtual void print(ostream&) const
     {}
};


class StringOption : public Option {
private:
     /** Generic pointer */
     string *p;
     
public:
     /** Constructor */
     StringOption(const std::string& name_, char short_name_, const string& desc_, const string & type_,  string * p_, const string& def)
     : Option(name_, short_name_, desc_, type_), p(p_)
     {*p=string(def);}
     
     void set(const string &p_)  {*p=p_;}
     
     /** Print in stream */
     void print(std::ostream& stream) const
     { stream << *p; }
};

class DoublePrecisionOption : public Option {
private:
     /** Generic pointer */
     double *p;
     
public:
     /** Constructor */
     DoublePrecisionOption(const std::string& name_, char short_name_, const string& desc_, const string & type_,  double * p_,const double & def)
     : Option(name_, short_name_, desc_, type_), p(p_){*p=def;}
     
     void set(const string &p_)  {*p=atof(p_.c_str());}
     
     /** Print in stream */
     void print(std::ostream& stream) const
     { stream << *p; }
};

class IntOption : public Option {
private:
     /** Generic pointer */
     int *p;
     
public:
     /** Constructor */
     IntOption(const std::string& name_, char short_name_, const string& desc_, const string & type_,  int * p_,int def)
     : Option(name_, short_name_, desc_, type_), p(p_){*p=def;}
     
     void set(const string &p_)  {*p=atoi(p_.c_str());}
     
     /** Print in stream */
     void print(std::ostream& stream) const
     { stream << *p; }
};

class BoolOption : public Option {
private:
     /** Generic pointer */
     bool *p;
     
public:
     /** Constructor */
     BoolOption(const std::string& name_, char short_name_, const string& desc_, const string & type_,  bool * p_,bool def)
     : Option(name_, short_name_, desc_, type_), p(p_){*p=def;}
     
     void set(const string &p_)  {*p=stob(p_.c_str());}
     // stob: string_to_boolean with cute return statement by Romain :)
     bool stob(const string& s) {return (s=="true" or s=="True");}
     /** Print in stream */
     void print(std::ostream& stream) const
     { stream << *p; }
};

class UserInterface
{
public:

     UserInterface();
     /*{number_of_flavours=5.0;
                    info=0;
                    histogram_info=false;
                    cut_info=false;
                    matrix_element_approximation="effective";
                    }*/
     ~UserInterface(){};
     
     
     
     vector<Option*> options;

     void ParseInput(int argc, char * const *argv);
     int ParseFile(const string &, bool);
     vector<vector<string> > ParseCmd(int argc,  char * const *argv, bool verbose);

     void print_help_message();

     //: getopt interface
     option * create_getopt_option_array();
     string create_getopt_optdesc();

     
     //: variables
     
     double Etot,m_higgs,epsrel,epsabs,muf_over_mhiggs,mur_over_mhiggs,number_of_flavours;
     string production,decay,pdf_provider,sector_name,sector_for_production,input_filename,output_filename,matrix_element_approximation,Fleft,Fright;
     int verbose,maxeval,mineval,nstart,nincrease,perturbative_order,pole,decay_sector,sector_control,requested_histogram,requested_cut;
     bool info,histogram_info,cut_info,list_processes,help,show_me_list,
            pdf_error, dummy_process;
     
};

#endif