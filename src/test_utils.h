#ifndef CHANNEL_NAME_H
#define CHANNEL_NAME_H
class channel_name{
public:
     channel_name(string _p1,string _p2,string _pord,int _eord):p1(_p1),p2(_p2),pord(_pord),e_ord(_eord){me_approx="effective";}
     channel_name(string _p1,string _p2,string _pord,int _eord,const string & _me_approx):p1(_p1),p2(_p2),pord(_pord),e_ord(_eord){me_approx=_me_approx;}
     string p1,p2,pord;
     int e_ord;
     string me_approx;
};

void check_sectors(vector<channel_name> channels,double mur,double muf,int pole,int pertord,double*res);
void check_sectors(vector<channel_name> channels,double mur,double muf,int pole,int pertord,double*res,const string &);
void check_sectors(vector<channel_name> channels,double mur,double muf,int pole,int pertord,double*res,const vector<int> & specific_sector_numbers);
void check_sectors(vector<channel_name> channels,double mur,double muf,int pole,int pertord,double*res,const string & _me_approx,const vector<int> & specific_sector_numbers);


class MultiThreadArgumentKeeper
{
public:
     MultiThreadArgumentKeeper(int);
     int pertord,pole;
     double mur,muf;
     string me_approx;
     
     vector<CHistogram*> all_hists;
     vector<double> xs;
     vector<double> err;

};

struct SingleThreadId
{
     MultiThreadArgumentKeeper* the_keeper;
     int ID;
     string current_sector_name;
};


#endif