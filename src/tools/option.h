/**
 *
 * \file    option.h
 * \ingroup tools
 * \author  Simone Lionetti
 * \date    August 2015
 *
 */

#ifndef OPTION_H
#define OPTION_H

#include <string>
#include <vector>
//#include <sstream> //: for stringstream

using namespace std;

enum Need {
    None=0,
    Required=1,
    Optional=2
};

struct NameAndArgs
{
    string name;
    vector<string> args;

    static NameAndArgs split(const string& input, const string delimiters = ",]", const string start = "[");
    static void checksize(const string& fname, const vector<string>& args, const size_t n);
};

struct BaseOption
{

    const string name;
    const char   abbr;
    const string desc;
    const Need   need;

    BaseOption(const string& iname, char iabbr, const string& idesc, const Need ineed)
    : name(iname), abbr(iabbr), desc(idesc), need(ineed)
    {}

    virtual ~BaseOption()
    {}

    virtual void set(const string& input) = 0;
    
};

template<class T>
struct Option : public BaseOption
{

    T& value;

    Option(const string& iname, char iabbr, const string& idesc, const Need ineed, T& ibind, const T& ival = T())
    : BaseOption(iname,iabbr,idesc,ineed), value(ibind)
    {
        value = ival;
        return;
    }

    void set(const string& input);

};

struct OptionSet
{
    vector<BaseOption*> _opts;
};

//class CutOption : public Option<NameAndArgs>
//{
//
//private:
//    /** Generic pointer */
//    string p;
//    vector<double> values_;
//
//public:
//    vector<string> all_cut_values;
//
//    string give_name(){return p;}
//    vector<double> give_values(){return values_;}
//    /** Constructor */
//    CutOption(const string& name_, char short_name_, const string& desc_, const string & type_,  const string&  p_,const vector<string>& vals)
//    : Option(name_, short_name_, desc_, type_), p(p_)
//    {
//        for (int i=0;i<vals.size();i++)
//            values_.push_back(atof(vals[i].c_str()));
//    }
//
//    void set(const string &stringwithnameandvalues)
//    {
//        all_cut_values.push_back(stringwithnameandvalues);
//    }
//
//    /** Print in stream */
//    string print(){stringstream s;s<<p<<" : ";
//        for (int i=0;i<values_.size();i++) s<<values_[i]<<" ";
//        return s.str();}
//
//};
//
//class HistogramOption : public Option{
//private:
//    string name_;
//    int numbins_;
//    double lowend_;
//    double highend_;
//public:
//    HistogramOption(const string& name, char short_name_, const string& desc_, const string & type_,  const string&  p_,const vector<string>& vals)
//    : Option(name, short_name_, desc_, type_), name_(p_)
//    {
//        if (vals.size()<3) {cout<<"\n error in reading histogram from runcard: you didn't declare one of numbins, lowend, highend.  We are forced to ignore "<<name_<<endl;}
//        numbins_ = atoi(vals[0].c_str());
//        lowend_ = atof(vals[1].c_str());
//        highend_ = atof(vals[2].c_str());
//    }
//    string give_name(){return name_;}
//    int numbins(){return numbins_;}
//    double lowend(){return lowend_;}
//    double highend(){return highend_;}
//
//    vector<string> all_hist_values;
//    void set(const string &stringwithnameandvalues)
//    {
//        all_hist_values.push_back(stringwithnameandvalues);
//    }
//    string print(){stringstream s;s<<name_<<" : ["<<lowend_<<","<<highend_<<"] with "<<numbins_<<" bins.";
//        return s.str();}
//
//};

#endif
