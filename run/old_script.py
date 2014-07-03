#! /usr/bin/env python
#-------------------------------------------------------------------------------
runcard_local = "nlo_effective.card"
ehixs_bin_directory = "../cmake_builds"
result_directory_name = "ew_test_eff"
running_mode = "parallel" # "parallel" or "serial" or "lsf_cluster"
waiting_time_before_checking_for_status = 2.0 # in secs
verbosity_level_for_checking_reports = "moderate" # or "moderate" or "zero"
number_of_cores = 3 # the script will be firing this number of jobs if in parallel
cluster_queue = "1nh"
#-------------------------------------------------------------------------------
import sys
import getopt
#import os
#import subprocess,shlex
#import math
import StringIO
#import xml.etree.ElementTree as ET
#from time import sleep

from sector import *
from histogram import *
from jobmanager import *
from my_paths import *


# command line args (might overwrite paths above)
collect_mode = False
sector_selection=''

try:
    opts,args = getopt.getopt(sys.argv[1:],"hi:d:x:",["ifile=","ofile="])
except getopt.GetoptError:
    print 'usage: -i inputfile -o outputfile'
    quit()
for opt,arg in opts:
    if opt=='-h':
        print("\n this is a helpmessage")
        quit()
    elif opt=='-c':
        collect_mode=True
    elif opt=='-d':
        result_directory_name=arg
    elif opt=='-i':
        runcard_local=arg
    elif opt=='-x':
        sector_selection = arg
        print(sector_selection)








#check to see if runcard is there
if not(os.path.isfile(runcard_local)):
    print "[python script] ERROR: runcard with name "+runcard_local+" not found!"
    quit()

directory_name = os.getcwd()+'/'+result_directory_name
runcard = os.getcwd()+'/'+runcard_local

#check if the result directory is present and create it if not
if not(os.path.isdir(directory_name)):
    os.mkdir(directory_name)

#copy runcard to directory of the run
os.system("cp "+runcard+" "+directory_name+"/"+runcard_local)

pyscript_output_filename = directory_name + '/Total.data'







# ask ehixs for all sectors with the selection criteria of the runcard
# this information is saved at directory_name/xml_info.out
print "[python script] Asking ehixs for sectors that match the selection criteria of the runcard"
xml_filename = directory_name+'/xml_info.out'
print "[python script] with command : "+ehixs_bin_directory+'/ehixs -i '+runcard+' --info --xml_info '+xml_filename+' > '+xml_filename+'.log'
os.system(ehixs_bin_directory+'/ehixs --info -i'+runcard+' --xml_info '+xml_filename+' > '+xml_filename+'.log')
#reading the xml file
run_tree = ET.parse(xml_filename)
#constructing the root
run_root = run_tree.getroot()
# all_sectors will hold all the sectors for the run
all_sectors = []
#initializing the sectors
sector_counter=0
the_paths = MyPaths()
the_paths.ehixs_bin_directory = ehixs_bin_directory
the_paths.runcard = runcard

for xml_sector in run_root.getiterator('sector'):
    cs = Sector(the_paths)
    #the xml structure for <sector> is
    #<sector id="15" name="apotentiallyverylonguniquename">
    #retrieve id
    cs.id_number_in_ehixs = str(xml_sector.get('id'))
    #default filename S15.data
    cs.output_filename = directory_name+'/S'+cs.id_number_in_ehixs+'.data'
    #retrieve name
    cs.ehixs_name = xml_sector.get('name')
    sector_counter +=1
        #if sector_counter in range(31,60):
    #all_sectors.append(cs)
    all_sectors.append(cs)


#run ehixs in the mode chosen and read the results for each sector      
if running_mode=="serial":
    total_time_needed = 0.0
    print "[python script] Start running necessary sectors"
    for cs in all_sectors:
        if not(os.path.isfile(cs.output_filename)):
            cs.run()
        #print "would run "+cs.ehixs_name
        cs.read_results()
        total_time_needed = total_time_needed + cs.give('time')
    print "total time necessary : "+str(total_time_needed)
elif running_mode=="parallel":
    the_manager = JobManager(all_sectors,number_of_cores,waiting_time_before_checking_for_status)
    the_manager.run()
elif running_mode=="lsf_cluster" and not(collect_mode):
    collect_mode = False
    for cs in all_sectors:
        if not(os.path.isfile(cs.output_filename)):
            cs.run_lsf()
    quit()
elif running_mode=="lsf_cluster" and collect_mode:
    for cs in all_sectors:
        cs.read_results()



#total cross section and related quantities
total_xs = 0.0
total_err_sq = 0.0
highest_prob = 0.0
highest_prob_sector_id = -1                                                                                                
total_number_of_points_for_run = 0
total_time_used = 0
for cs in all_sectors:
    total_xs = total_xs + cs.give('sigma')
    cur_err = cs.give('error')
    total_err_sq = total_err_sq + cur_err*cur_err
    cur_prob = cs.give('prob')
    total_time_used += cs.give('time')
    if cur_prob>highest_prob:
        highest_prob = cur_prob
        highest_prob_sector_id = int(cs.id_number_in_ehixs)                                                                                            
    total_number_of_points_for_run = total_number_of_points_for_run + cs.give('total_number_of_points')
for cs in all_sectors:
    cs.printme(total_xs,total_time_used)
print '------------'
print 'totalxs = {0:.4e} +- {1:.2e} | T = {5} :: highest probability that error is wrong {2:.2e} from sector #{3} :: total number of points for run {4}'.format(total_xs,math.sqrt(total_err_sq),highest_prob,highest_prob_sector_id,total_number_of_points_for_run, total_time_used)


                                                                                                    
    
#histogram merging  
all_histograms = []
#setting up histograms
for hist in all_sectors[0].res.getiterator('histogram'): 
    all_histograms.append(Histogram(hist.get('name')))

for hist in all_histograms:
    #merging the current histogram (i.e. HiggsPt) from histograms of 
    #individual sectors
    hist.add_histogram(all_sectors)
    #printing out result
    hist.printme()
    hist.print_analytics()
    cur_hist_filename = directory_name+'/'+hist.name
    with open(cur_hist_filename,'w') as outfile:
        outfile.write(hist.output_for_plotting())

if sector_selection!='':
    for hist in all_histograms:
        cur_hist_filename = directory_name+'/'+hist.name+sector_selection
        with open(cur_hist_filename,'w') as outfile:
            outfile.write(hist.output_for_selection(sector_selection))

#writting output    
with open(pyscript_output_filename,'w') as outfile:
    outfile.write('ehixs total output')
    for i,cs in enumerate(all_sectors):
        outfile.write('\n'+str(i)+':'+cs.ehixs_name)
    for cs in all_sectors:
        res = '\n'+str(cs.string_to_print(total_xs,total_time_used))
        res += '\n------------'
        res += '\ntotalxs = {0:.2e} +- {1:.2e} | T = {5} :: highest probability that error is wrong {2:.2e} from sector #{3} :: total number of points for run {4}'.format(total_xs,math.sqrt(total_err_sq),highest_prob,highest_prob_sector_id,total_number_of_points_for_run, total_time_used)
        outfile.write(res)
    for hist in all_histograms:
        res = '\n'+str(hist.string_to_print_analytics())
        outfile.write(res)







