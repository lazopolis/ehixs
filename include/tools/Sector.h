
#ifndef SECTOR_H
#define SECTOR_H

template<class ptr_to_process_function>
class DecaySector
{
public:
     DecaySector(const ptr_to_process_function ptr, int dim_int,string nm)
     {my_dim_int=dim_int;myptr=ptr;myname=nm;}
     DecaySector(const ptr_to_process_function ptr, int dim_int,int si,int sf,string nm)
     {my_dim_int=dim_int;myptr=ptr;sec_in=si;sec_fi=sf;myname=nm;}
     ~DecaySector(){};
     int dim(){return my_dim_int;}
     
     ptr_to_process_function ptr(){return myptr;}
     
     int sec_init(){return sec_in;}
     int sec_fin(){return sec_fi;}
     
     string name(){return myname;};
private:
     ptr_to_process_function myptr;
     
     int sec_in;
     int sec_fi;
     
     int my_dim_int;
     string myname;
};

#endif