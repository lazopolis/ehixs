#! /usr/bin/env python
#-------------------------------------------------------------------------------

directory_name = "python_script_test2"
running_mode = "parallel" # or "serial"
waiting_time_before_checking_for_status = 2.0 # in secs
verbosity_level_for_checking_reports = "moderate" # or "moderate" or "zero"
number_of_cores = 2 # the script will be firing this number of jobs if in parallel
#-------------------------------------------------------------------------------

import os
import subprocess,shlex
import math
import StringIO
import xml.etree.ElementTree as ET
from time import sleep

class Sector:
    """holds results for sector"""
    def __init__(self):
        self.output_filename = ''
        self.id_number_in_ehixs = ''
        self.float_attributes = ['sigma','error','prob','time','secs_per_point']
        self.string_attributes = ['name','runcard_name']
        self.int_attributes = ['total_number_of_points']
        self.ehixs_name = "noname"
    def run(self):
        print "[python script] Running sector "+self.ehixs_name
        os.system('./ehixs -s '+ str(self.id_number_in_ehixs)+' --output_filename '+self.output_filename + ' > '+self.output_filename+'.log')
    def run_in_the_background(self):
        print "[python script] Running sector "+self.ehixs_name
        command_line = './ehixs -s '+ str(self.id_number_in_ehixs)+' --output_filename '+self.output_filename
        args = shlex.split(command_line)
        with open('./'+self.output_filename+'.log','w') as logfile:
            self.proc = subprocess.Popen(command_line,shell=True,stdout=logfile,stderr=logfile)
    def check_status(self):
        returncode = self.proc.poll()
        if returncode==None:
            return "still running"
        elif returncode==0:
            return "finished"
        else:
            return "abnormally terminated"
    def print_command(self):
        print './ehixs -s '+ str(self.id_number_in_ehixs)+' --output_filename '+self.output_filename 
    def read_results(self):
        if os.path.isfile(self.output_filename):
            print "[python script] reading from file "+self.output_filename
            self.eltree = ET.parse(self.output_filename)
            self.res = self.eltree.getroot()
        else:
            print "[python script] cannot read results - "+self.output_filename+" does not exist!"
    def give(self,attrib):
        if attrib in self.float_attributes:
            return float(self.res.get(attrib))
        elif attrib in self.int_attributes:
            return int(self.res.get(attrib))
        elif attrib in self.string_attributes:
            return self.res.get(attrib)
        else:
            print "error in Sector.give, attribute ",attrib," of unknown type"
            return 0.0
    def histogram(self,name):
        for hist in self.res.iter('histogram'):
            if hist.get('name')==name:
                return hist
    def printme(self):
        rel_error = 0.0
        if math.fabs(self.give('sigma'))>0.0:
            rel_error = math.fabs(self.give('error')/self.give('sigma'))
        print '{0:>3} | {1:>10.2e} | {2:>10.2e} | {3:>8.1%} | {4:>8} | {5:>8.2f}s | {6:8.2e}s/point'.format(self.id_number_in_ehixs,self.give('sigma'), self.give('error'),rel_error,self.give('total_number_of_points'),self.give('time'),self.give('secs_per_point'))



class Bin:
    def __init__(self):
        self.sigma = 0.0
        self.error_sq = 0.0
        self.num_of_points = 0
        self.lowend = 0.0
        self.highend = 0.0
    def add(self, thebin):
        self.sigma = self.sigma + float(thebin[0].text)
        loc_error = float(thebin[1].text)
        self.error_sq = self.error_sq + loc_error*loc_error
        self.num_of_points = self.num_of_points + int(thebin[5].text)
    def printme(self):
        error = math.sqrt(self.error_sq)
        rel_error = 0.0
        if (math.fabs(self.sigma)>0):
            rel_error = math.fabs(math.sqrt(self.error_sq)/self.sigma)
        print '{0:>8.2f} | {1:>8.2f} | {2:>10.2e} | {3:>10.2e} | {4:>8.1%} | {5:8}'.format(self.lowend,self.highend, self.sigma,error,rel_error,self.num_of_points)


class Histogram:
    def __init__(self,name):
        self.all_bins = []
        self.name = name
        self.overflow_bin = Bin()
        self.totalbinned = 0.0
        self.totalsigma = 0.0
    def number_of_bins(self):
        return len(self.all_bins)
    def add_histogram(self,all_sectors):
        init_hist = all_sectors[0].histogram(self.name)
        for curbin in init_hist.iter('bin'):
            newbin = Bin()
            newbin.lowend = float(curbin.get('le'))
            newbin.highend = float(curbin.get('he'))
            self.all_bins.append(newbin)
        for cur_sector in all_sectors:
            self.totalsigma = self.totalsigma + cur_sector.give('sigma')
            my_hist = cur_sector.histogram(self.name)
            binindex = 0
            for curbin in my_hist.iter('bin'):
                self.all_bins[binindex].add(curbin)
                binindex = binindex+1
            for curbin in my_hist.iter('overflow'):
                self.overflow_bin.add(curbin)
            for tb in my_hist.iter('totalbinned'):
                self.totalbinned = self.totalbinned + float(tb.text)
    def printme(self):
        print "******* "+self.name+"\t # of bins = "+str(len(self.all_bins))
        print '{0:^8} | {1:^8} | {2:^10} | {3:^10} | {4:^8} | {5:^8}'.format("lowend", "highend", "sigma","error", "% error",  "# of points")
        for thebin in self.all_bins:
            thebin.printme()
        overflow_rel_error = 0.0
        if math.fabs(float(self.overflow_bin.sigma))>0.0:
            overflow_rel_error = math.sqrt(self.overflow_bin.error_sq) / self.overflow_bin.sigma
        print '{0:^19} | {1:>10.2e} | {2:>10.2e} | {3:>8.1%} | {4:8}'.format("overflow", self.overflow_bin.sigma,math.sqrt(self.overflow_bin.error_sq),overflow_rel_error,self.overflow_bin.num_of_points)
        total_rel = 0.0
        if math.fabs(self.totalsigma)>0.0:
            total_rel = self.totalbinned/self.totalsigma
        overflow_rel = 0.0
        if (math.fabs(self.totalbinned))>0.0:
            overflow_rel = self.overflow_bin.sigma/self.totalbinned
        print 'Total binned = {0:.2e} with sigma_total {1:.2e} ({2:.2%}). The overflow is {3:.2%}'.format(self.totalbinned,self.totalsigma,total_rel,overflow_rel)
        print '------'


class JobManager:
    def __init__(self,sectors,ncores):
        self.sectors=sectors
        self.cores=ncores
        self.cores_used = 0

    def run(self):
        for i in range(ncores):
            




if not(os.path.isdir(directory_name)):
    os.mkdir(directory_name)

# ask ehixs for all sectors with the selection criteria of the runcard
# this information is saved at directory_name/xml_info.out
print "[python script] Asking ehixs for sectors that match the selection criteria of the runcard"
xml_filename = directory_name+'/xml_info.out'
os.system('./ehixs --info --xml_info '+xml_filename+' > '+xml_filename+'.log')
#reading the xml file
run_tree = ET.parse(xml_filename)
#constructing the root
run_root = run_tree.getroot()
# all_sectors will hold all the sectors for the run
all_sectors = []
#initializing the sectors
for xml_sector in run_root.iter('sector'):
    cs = Sector()
    #the xml structure for <sector> is
    #<sector id="15" name="apotentiallyverylonguniquename">
    #retrieve id
    cs.id_number_in_ehixs = str(xml_sector.get('id'))
    #default filename S15.data
    cs.output_filename = directory_name+'/S'+cs.id_number_in_ehixs+'.data'
    #retrieve name
    cs.ehixs_name = xml_sector.get('name')
    all_sectors.append(cs)
#-------------------------------------------------------------------------------
#
#
#
#
#

def process_is_still_running(proc):
    returncode = proc.poll()
    if returncode==None:
        return False
    elif returncode==0:
        return True
    else:
        print "[python script] one of the sectors exited abruptly"
        return True



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
    all_processes = []
    for cs in all_sectors:
        #we don't run sectors that already have a result filename
        if not(os.path.isfile(cs.output_filename)):
            all_processes.append(cs.run_in_the_background())
                #print '..sleeping'
                #sleep(2.0)
    #print 'waking up...'
    all_sectors_status = "still running"
    while (all_sectors_status=="still running"):
        sleep(waiting_time_before_checking_for_status) # Time in seconds.
        if verbosity_level_for_checking_reports == "moderate" or verbosity_level_for_checking_reports=="complete":
            print ''
            print ''
            print '[python script] checking status...'
        all_sectors_status = "finished"
        number_of_secs_still_running = 0
        for cs in all_sectors:
            
            if cs.check_status()=="still running":
                all_sectors_status = "still running"
                number_of_secs_still_running = number_of_secs_still_running + 1
                if verbosity_level_for_checking_reports == "complete":
                    print cs.ehixs_name+' is still running'
            #break
        if verbosity_level_for_checking_reports== "moderate" or verbosity_level_for_checking_reports == "complete":
            print '[python script] still running '+str(number_of_secs_still_running)+'/'+str(len(all_sectors))
        print ''
    for cs in all_sectors:
        print cs.ehixs_name + ' : '+cs.check_status()
    for cs in all_sectors:
        cs.read_results()



#
#
#
#
#
#-------------------------------------------------------------------------------
#total cross section and related quantities
total_xs = 0.0
total_err_sq = 0.0
highest_prob = 0.0
highest_prob_sector_id = -1                                                                                                
total_number_of_points_for_run = 0
for cs in all_sectors:
    total_xs = total_xs + cs.give('sigma')
    cur_err = cs.give('error')
    total_err_sq = total_err_sq + cur_err*cur_err
    cur_prob = cs.give('prob')
    if cur_prob>highest_prob:
        highest_prob = cur_prob
        highest_prob_sector_id = int(cs.id_number_in_ehixs)                                                                                            
    total_number_of_points_for_run = total_number_of_points_for_run + cs.give('total_number_of_points')
    cs.printme()
print '------------'
print 'totalxs = {0:.2e} +- {1:.2e} :: highest probability that error is wrong {2:.2e} from sector #{3} :: total number of points for run {4}'.format(total_xs,math.sqrt(total_err_sq),highest_prob,highest_prob_sector_id,total_number_of_points_for_run)
                                                                                                    
    
#histogram merging: will this work without histograms?   
all_histograms = []
#setting up histograms
for hist in all_sectors[0].res.iter('histogram'): 
    all_histograms.append(Histogram(hist.get('name')))

for hist in all_histograms:
    #merging the current histogram (i.e. HiggsPt) from histograms of 
    #individual sectors
    hist.add_histogram(all_sectors)
    #printing out result
    hist.printme()

