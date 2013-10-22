#ifndef USER_INTERFACE_H
#define USER_INTERFACE_H

#include<string>
#include<vector>
#include<iostream>
#include <stdlib.h> //: for exit()
#include <sstream> //: for stringstream

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
     
    int get_type();
     /** Set and match... */
     virtual void set(const string&) 
     {}
     /** Print... */
    virtual string print() =0;
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
    string print(){return *p;}
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
    string print(){stringstream s;s<<*p;return s.str();}
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
    string print(){stringstream s;s<<*p;return s.str();}

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
    string print(){stringstream s;s<<*p;return s.str();}

};

class UserInterface
{
public://methods
    UserInterface();
    void ParseInput(int argc, char * const *argv);
    void print_help_message();
    void RunSanityChecks();
    void PrintAllOptions() const;
public://data
    double Etot,m_higgs,epsrel,epsabs,muf_over_mhiggs,mur_over_mhiggs,number_of_flavours;
    string production,decay,pdf_provider,sector_name,sector_for_production,input_filename,output_filename,matrix_element_approximation,Fleft,Fright;
    int verbose,maxeval,mineval,nstart,nincrease,perturbative_order,pole,decay_sector,sector_control,requested_histogram,requested_cut;
    int alpha_s_power;
    bool info,histogram_info,cut_info,list_processes,help,show_me_list,
    pdf_error, dummy_process;
private://methods
    int ParseFile(const string &, bool);
    vector<vector<string> > ParseCmd(int argc,  char* const *argv,
                                        bool verbose);
     //: getopt interface
     option * create_getopt_option_array();
     string create_getopt_optdesc();
    //: checks
    void CheckIf(bool condition, const string& error_message);

private://data
    vector<Option*> options;
};

#endif