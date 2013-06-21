#include "Cluster.h"
#include "math.h"
using namespace std;


ClusterEvent::ClusterEvent(const double &x1,const double &x2,const double &x3,const double &x4,const double &x5,const double &x6,const double &x7)
{
     y[0]=x1;
     y[1]=x2;
     y[2]=x3;
     y[3]=x4;
     y[4]=x5;
     y[5]=x6;
     y[6]=x7;

}

double ClusterEvent::distance(ClusterEvent * other)
{
     double res=0.0;
     for (int i=0;i<7;i++)
          {
          res=res+pow(y[i]-other->x(i),2.0);
          }
     return res;
}

void Cluster::process(const double &x1,const double &x2,const double &x3,const double &x4,const double &x5,const double &x6,const double &x7)
{
     ClusterEvent* new_event=new ClusterEvent(x1,x2,x3,x4,x5,x6,x7);
     if (cluster_counter<=max_number_of_clusters)
          {
          add_cluster(new_event);
          }
     else
          {
          merge(new_event);
          }

}

void Cluster::add_cluster(ClusterEvent* new_event)
{
     all_clusters[cluster_counter] = new_event;
     update_smallest_distance(new_event);
     cluster_counter++;
}

void Cluster::update_smallest_distance(ClusterEvent* new_event)
{
     for (int i=0;i<cluster_counter;i++)
          {
          double new_distance = new_event->distance(all_clusters[i]);
          if (new_distance<smallest_distance)
               {
               smallest_distance = new_distance;
               }
          }
}








