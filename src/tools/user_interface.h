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


class CutOption : public Option {
private:
    /** Generic pointer */
    string p;
    vector<double> values_;
public:
    vector<string> all_cut_values;
    
    string give_name(){return p;}
    vector<double> give_values(){return values_;}
    /** Constructor */
    CutOption(const std::string& name_, char short_name_, const string& desc_, const string & type_,  const string&  p_,const vector<string>& vals)
    : Option(name_, short_name_, desc_, type_), p(p_)
        {
        for (int i=0;i<vals.size();i++)
            values_.push_back(atof(vals[i].c_str()));
        }
    
    void set(const string &stringwithnameandvalues)
        {
        all_cut_values.push_back(stringwithnameandvalues);
        }

    /** Print in stream */
    string print(){stringstream s;s<<p<<" : ";
        for (int i=0;i<values_.size();i++) s<<values_[i]<<" ";
        return s.str();}
    
};

class HistogramOption : public Option{
private:
    string name_;
    int numbins_;
    double lowend_;
    double highend_;
public:
    HistogramOption(const std::string& name, char short_name_, const string& desc_, const string & type_,  const string&  p_,const vector<string>& vals)
    : Option(name, short_name_, desc_, type_), name_(p_)
    {
    if (vals.size()<3) {cout<<"\n error in reading histogram from runcard: you didn't declare one of numbins, lowend, highend.  We are forced to ignore "<<name_<<endl;}
    numbins_ = atoi(vals[0].c_str());
    lowend_ = atof(vals[1].c_str());
    highend_ = atof(vals[2].c_str());
    }
    string give_name(){return name_;}
    int numbins(){return numbins_;}
    double lowend(){return lowend_;}
    double highend(){return highend_;}

    vector<string> all_hist_values;
    void set(const string &stringwithnameandvalues)
    {
    all_hist_values.push_back(stringwithnameandvalues);
    }
    string print(){stringstream s;s<<name_<<" : ["<<lowend_<<","<<highend_<<"] with "<<numbins_<<" bins.";
        return s.str();}

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
    string production,decay,pdf_provider,sector_name,sector_for_production,input_filename,output_filename,matrix_element_approximation,Fleft,Fright, leptonic_decay_mode_in_wwzz,xml_info,
            qcd_perturbative_order;
    int verbose,maxeval,mineval,nstart,nincrease,perturbative_order,pole,decay_sector,sector_control,requested_histogram,requested_cut;
    int alpha_s_power,alpha_ew_power;
    bool info,histogram_info,cut_info,list_processes,help,show_me_list,
    pdf_error, dummy_process,ew_soft,ew_h_plus_j,only_ew_h_j;
    
    string my_generic_cut;
    vector<CutOption*> all_cuts;
    vector<HistogramOption*> all_hists;
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