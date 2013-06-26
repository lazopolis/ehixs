#ifndef CLUSTER_H
#define CLUSTER_H

#define max_number_of_clusters 1000
class ClusterEvent{
public:
     ClusterEvent(const double &x1,const double &x2,const double &x3,const double &x4,const double &x5,const double &x6,const double &x7);
     ~ClusterEvent(){};
     double x(int m) const{return y[m];}
     double distance( ClusterEvent* other);
private:
     double y[7];
};

class Cluster{
public:
     Cluster(){cluster_counter=0;smallest_distance=1e16;}
     ~Cluster(){};
     void process(const double &x1,const double &x2,const double &x3,const double &x4,const double &x5,const double &x6,const double &x7);
private:
     int cluster_counter;
     void add_cluster(ClusterEvent*);
     void merge(ClusterEvent*);
     ClusterEvent* all_clusters[max_number_of_clusters];
     void update_smallest_distance(ClusterEvent*);
     double smallest_distance;
};

#endif