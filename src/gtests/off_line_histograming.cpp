/** testing UserInterface.*:
 *
 * Achilleas Lazopoulos, lazopoli@phys.ethz.ch
 */

#include <iostream>
#include <cmath>
#include <fstream>
#include <sstream>
#include <cstdlib>

#include "histogram.h"
#include "histograms.hpp"
using namespace std;


#include "gtest/gtest.h"


struct eventpoint{
    vector<double> coords;
    double w;
    bool equal(const eventpoint & other)
    {
    for (int i=0;i<coords.size();i++)
        {
        if (coords[i]!=other.coords[i]) return false;
        }
    return true;
    }
    
    friend ostream& operator<<(ostream&, const eventpoint&);

};

ostream& operator<<(ostream& stream, const eventpoint& x)
{
    for (int i=0;i<x.coords.size();i++) stream <<x.coords[i]<<" ";
    stream<<x.w<<endl;
    return stream;
}


class eventpack{
public:
    vector<eventpoint> events;
    void add(const eventpoint &new_event);
    void clear(){events.clear();}
};

void eventpack::add(const eventpoint &new_event)
{
    bool found = false;
    for (int i=0;i<events.size();i++)
        {
        if (events[i].equal(new_event))
            {
            events[i].w += new_event.w;
            found = true;
            break;
            }
        }
    if (!found)
        {
        events.push_back(new_event);
        }
}

void preprocess_events(const string filename,int num_of_dimensions)
{
    fstream my_out_stream;
    const string output_fname = filename + ".processed";
    my_out_stream.open(output_fname.c_str(), fstream::out);
    
    
    int processed_events_counter=0;
    int output_events = 0;
    string line;
    std::ifstream infile(filename.c_str());
    eventpack vegas_event_pack;
    if (!infile.is_open()) {
        std::cerr << "_parsePoints(string): open(filename)" << std::endl;
        exit(1);
    }
    while(std::getline(infile, line))
        {
        if (line.find('$') != std::string::npos) {
            my_out_stream<<line<<endl;
            continue;
            }
        if (line.find('*') != std::string::npos)
            {
            for (int i=0;i<vegas_event_pack.events.size();i++)
                {
                my_out_stream<<vegas_event_pack.events[i];
                output_events++;
                }
            my_out_stream<<line<<endl;
            vegas_event_pack.clear();
            continue;
            }
        try {
            eventpoint the_event;
            std::istringstream iss(line);
            std::string buffer;                  //coordinates
            for (uint i=0; i<num_of_dimensions; ++i) {
                std::getline(iss, buffer, ' ');
                the_event.coords.push_back(atof(buffer.c_str()));
            }
            std::getline(iss, buffer, ' ');      //weight
            the_event.w = atof(buffer.c_str());
            
            vegas_event_pack.add(the_event);
            
            processed_events_counter++;
        } catch (...) {
            infile.close();
            std::cerr << "_parsePoints(string): DataPoint(char*)" << std::endl;
            exit(2);
        }
        }
    cout<<"\n events processed: "<< processed_events_counter;
    cout<<"\n events in the output: "<<output_events;
    cout<<"\n";
    infile.close();
    my_out_stream.close();
}




void do_histogram(const string filename, int num_of_dimensions)
{
    
    unsigned counter=0;
    SimpleHistogram my_hist(20,0,-5.0,5.0,"higgs rapidity");
    int zero_points_counter=0;
    int zero_rap=0;
    int zero_pt=0;

    string line;
    std::ifstream infile(filename.c_str());
    if (!infile.is_open()) {
        std::cerr << "_parsePoints(string): open(filename)" << std::endl;
        exit(1);
    }
    while(std::getline(infile, line))
        {
        if (line.find('*') != std::string::npos) {    //bypass comments
            my_hist.update();
            continue;}
        if (line.find('$') != std::string::npos) {    //bypass comments
            istringstream iss(line);
            string buf;
            getline(iss,buf,'$');
            int nop = atoi(buf.c_str());
            if (nop>0)
                my_hist.end(nop);
            continue;}
        try {
            vector<double> coords;
            std::istringstream iss(line);
            std::string buffer;                  //coordinates
            for (uint i=0; i<num_of_dimensions; ++i) {
                std::getline(iss, buffer, ' ');
                coords.push_back(atof(buffer.c_str()));
            }
            std::getline(iss, buffer, ' ');      //weight
            double weight = atof(buffer.c_str());
            
            double x1=coords[0];
            double pt=coords[1];
            double y=coords[2];
            
            if (pt<1.0 and fabs(y)<5e-2) zero_points_counter++;
            if (pt<1.0 ) zero_pt++;
            if (fabs(y)<5e-2) zero_rap++;
            if (pt<1.0)
                {
                my_hist.bin_val(y,weight );
                counter++;
                }
        } catch (...) {
            infile.close();
            std::cerr << "_parsePoints(string): DataPoint(char*)" << std::endl;
            exit(2);
        }
        }
    cout<<"\n events bined: "<< counter;
    cout<<endl<<"# of points at (0,0): "<<zero_points_counter;
    cout<<endl<<"# of points at pt=0 : "<<zero_pt;
    cout<<endl<<"# of points at y=0  : "<<zero_rap;

    infile.close();
    
    my_hist.end(counter);
    cout<<"\n\n"<<my_hist;
    cout<<endl;

}

void do_pt_histogram(const string filename, int num_of_dimensions)
{
    
    unsigned counter=0;
    SimpleHistogram my_hist(20,0,0.0,100.0,"higgs pT");
    int zero_points_counter=0;
    int zero_rap=0;
    int zero_pt=0;
    int large_rap=0;
    string line;
    std::ifstream infile(filename.c_str());
    if (!infile.is_open()) {
        std::cerr << "_parsePoints(string): open(filename)" << std::endl;
        exit(1);
    }
    while(std::getline(infile, line))
        {
        if (line.find('*') != std::string::npos) {    //bypass comments
            my_hist.update();
            continue;}
        if (line.find('$') != std::string::npos) {    //bypass comments
            istringstream iss(line);
            string buf;
            getline(iss,buf,'$');
            int nop = atoi(buf.c_str());
            if (nop>0)
                my_hist.end(nop);
            continue;}
        try {
            vector<double> coords;
            std::istringstream iss(line);
            std::string buffer;                  //coordinates
            for (uint i=0; i<num_of_dimensions; ++i) {
                std::getline(iss, buffer, ' ');
                coords.push_back(atof(buffer.c_str()));
            }
            std::getline(iss, buffer, ' ');      //weight
            double weight = atof(buffer.c_str());
            
            double x1=coords[0];
            double pt=coords[1];
            double y=coords[2];
            
            if (pt<1.0 and fabs(y)<5e-2) zero_points_counter++;
            if (pt<1.0 ) zero_pt++;
            if (fabs(y)<5e-2) zero_rap++;
            if (fabs(y)>5) large_rap++;
            my_hist.bin_val(pt,weight );
            counter++;
        } catch (...) {
            infile.close();
            std::cerr << "_parsePoints(string): DataPoint(char*)" << std::endl;
            exit(2);
        }
        }
    cout<<"\n events bined: "<< counter;
    cout<<endl<<"# of points at (0,0): "<<zero_points_counter;
    cout<<endl<<"# of points at pt=0 : "<<zero_pt
        <<" : "<<double(zero_pt)/double(counter)*100.0<<"%";
    cout<<endl<<"# of points at y=0  : "<<zero_rap
        <<" : "<<double(zero_rap)/double(counter)*100.0<<"%";
    cout<<endl<<"# of points at |y|>5  : "<<large_rap
    <<" : "<<double(large_rap)/double(counter)*100.0<<"%";
    
    infile.close();
    
    my_hist.end(counter);
    cout<<"\n\n"<<my_hist;
    cout<<endl;
    
}



TEST(off_line_hist,first_try)
{
    const string filename = "Events.dat";
    do_pt_histogram(filename,3);
    preprocess_events(filename,3);
    //do_histogram("Events.dat.processed",3);
    do_histogram("Events.dat.processed", 3);
    
    //do_histogram("Events.dat.processed.km",3);

    cout<<endl;
    EXPECT_LT(1.0,1e-5);
}



int main(int argc, char**argv)
{
    ::testing::InitGoogleTest(&argc, argv);
    return  RUN_ALL_TESTS();
    return 0;
}



