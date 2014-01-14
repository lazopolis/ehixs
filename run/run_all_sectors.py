#! /usr/bin/env python
#-------------------------------------------------------------------------------
runcard = "higgs.card"
ehixs_bin_directory = "../cmake_builds"
directory_name = "higgs_rapidity_zero_bin"
running_mode = "parallel" # or "serial"
waiting_time_before_checking_for_status = 2.0 # in secs
verbosity_level_for_checking_reports = "moderate" # or "moderate" or "zero"
number_of_cores = 6 # the script will be firing this number of jobs if in parallel
#-------------------------------------------------------------------------------

import os
import subprocess,shlex
import math
import StringIO
import xml.etree.ElementTree as ET
from time import sleep

#check to see if runcard is there
if not(os.path.isfile(runcard)):
    print "[python script] ERROR: runcard with name "+runcard+" not found!"
    quit()

directory_name = os.getcwd()+'/'+directory_name
runcard = os.getcwd()+'/'+runcard
#copy runcard to directory of the run
os.system("cp "+runcard+" "+directory_name+"/"+runcard)

pyscript_output_filename = directory_name + '/Total.data'

class Sector:
    """holds results for sector"""
    def __init__(self):
        self.output_filename = ''
        self.id_number_in_ehixs = ''
        self.float_attributes = ['sigma','error','prob','time','secs_per_point']
        self.string_attributes = ['name','runcard_name']
        self.int_attributes = ['total_number_of_points']
        self.ehixs_name = "noname"
        self.was_fired = False
    def run(self):
        print "[python script] firing sector "+self.ehixs_name
        os.system(ehixs_bin_directory+'/ehixs '+'-i '+runcard+' -s '+ str(self.id_number_in_ehixs)+' --output_filename '+self.output_filename + ' > '+self.output_filename+'.log')
    def run_in_the_background(self):
        #print "[python script] firing sector "+self.ehixs_name
        command_line = ehixs_bin_directory+'/ehixs '+'-i '+runcard+' -s '+ str(self.id_number_in_ehixs)+' --output_filename '+self.output_filename
        print "[python script] firing: "+command_line
        with open(self.output_filename+'.log','w') as logfile:
            self.proc = subprocess.Popen(command_line,shell=True,stdout=logfile,stderr=logfile)
        self.was_fired = True
    def check_status(self):
        if not(self.was_fired): return "not fired"
        returncode = self.proc.poll()
        if returncode==None:
            return "still running"
        elif returncode==0:
            return "finished"
        else:
            return "abnormally terminated"
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
    def printme(self,XS,Ttot):
        print self.string_to_print(XS,Ttot)
    def string_to_print(self,XS,Ttot):
        rel_error = 0.0
        if math.fabs(self.give('sigma'))>0.0:
            rel_error = math.fabs(self.give('error')/self.give('sigma'))
            return '{0:>3} | {1:>10.2e} | {2:>10.2e} | {3:>8.1%} | {4:>8} | {5:>8.2f} s | {6:8.2e} s/p | {7:>8.1%} | {8:>8.1%}'.format(self.id_number_in_ehixs,self.give('sigma'), self.give('error'),rel_error,self.give('total_number_of_points'),self.give('time'),self.give('secs_per_point'),self.give('sigma')/XS,self.give('time')/Ttot)



class Bin:
    def __init__(self):
        self.sigma = 0.0
        self.error_sq = 0.0
        self.num_of_points = 0
        self.lowend = 0.0
        self.highend = 0.0
        self.indiv_sector_contributions=[]
    def add(self, thebin):
        self.indiv_sector_contributions.append(float(thebin[0].text))
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
    def string_to_print_analytics(self):
        res='{0:>8.2f} | {1:>8.2f} | {2:>10.2e} |'.format(self.lowend,self.highend, self.sigma)
        for sec_bin in self.indiv_sector_contributions:
            res += ' {0:>8.1%} |'.format(sec_bin/self.sigma)
        return res
    def string_to_print_analytics(self,min,max):
        res='{0:>8.2f} | {1:>8.2f} | {2:>10.2e} |'.format(self.lowend,self.highend, self.sigma)
        for ii,sec_bin in enumerate(self.indiv_sector_contributions[min:max+1]):
            if not(self.sigma==0):
                res += ' {0:>8.1%} |'.format(sec_bin/self.sigma)
            else:
                res += ' {0:>8.1%} |'.format(0.0)
        #res += ' {0:>8} |'.format(min+ii)
        return res
    def print_analytics(self):
        print self.string_to_print_analytics()

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
    def print_analytics(self):
        print self.string_to_print_analytics()
    def string_to_print_analytics(self):
        res = '\nhistogram: '+self.name
        num_sec = len(self.all_bins[0].indiv_sector_contributions)
        cursec = 0
        num_batches = num_sec / 10
        print num_batches
        firstline='{0:>8} | {1:>8} | {2:>10} |'.format("lowend","highend","sigma")
        for i in range(num_batches):
            res += '\n\n'+firstline
            for x in range(10):
                res += '{0:>9} |'.format('S'+str(i*10+x))
            for thebin in self.all_bins:
                res += '\n'+thebin.string_to_print_analytics(i*10,(i+1)*10-1)
        res += '\n\n'+firstline
        for x in range(num_sec % 10):
            res += '{0:>9} |'.format('S'+str((num_batches)*10+x))
        for thebin in self.all_bins:
            res += '\n'+thebin.string_to_print_analytics((num_batches)*10,(num_batches)*10+x)
                    
        return res




def highlight(string, status, face):
    attr = []
    if status == "green":
        # green
        attr.append('32')
    elif status == "red":
        # red
        attr.append('31')
    else:
        return string
    if face == "bold":
        attr.append('1')
    return '\x1b[%sm%s\x1b[0m' % (';'.join(attr), string)


class JobManager:
    def __init__(self,sectors,ncores):
        self.sectors=sectors
        self.cores=ncores
        self.sectors_submitted = 0
        self.status_message = ""
    def run(self):
        if not(os.path.isfile(self.sectors[1].output_filename)):
            while self.sectors_submitted<len(self.sectors):
                self.run_a_batch()
                self.wait()
            self.wait_till_all_sectors_finished()
        for cs in all_sectors:
            cs.read_results()
    def check_status(self):
        my_free_cores = self.cores
        running_sectors = 0
        new_message = '\n[python script] status'
        for cs in all_sectors:
            if cs.check_status()=="still running":
                new_message += '\n[python script] '+highlight("R ","red","normal")+cs.ehixs_name
                my_free_cores -= 1
                running_sectors += 1
        if not(new_message==self.status_message):
            print new_message
            self.status_message = new_message
        #print "[python script] Status: running sectors "+str(running_sectors)
        return my_free_cores
    def run_a_batch(self):
        free_cores = self.check_status()
        if (free_cores>0):
            #print "[python script] Will fire "+str(free_cores)+" jobs"
            for x in self.sectors[self.sectors_submitted:self.sectors_submitted+free_cores]:
                if not(os.path.isfile(x.output_filename)):
                    print "[python script]"+highlight(" F "+str(self.sectors_submitted+1)+"/"+str(len(self.sectors)),"green","normal") +" : "+ x.ehixs_name
                    x.run_in_the_background()
                self.sectors_submitted +=1
    def wait_till_all_sectors_finished(self):
        while self.check_status()<self.cores:
            self.wait()
    def wait(self):
        #print "[python script] waiting..."
        sleep(waiting_time_before_checking_for_status)







if not(os.path.isdir(directory_name)):
    os.mkdir(directory_name)

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
    sector_counter +=1
        #if sector_counter in range(31,60):
    #all_sectors.append(cs)
    all_sectors.append(cs)
#-------------------------------------------------------------------------------
#
#
#
#



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
    the_manager = JobManager(all_sectors,number_of_cores)
    the_manager.run()
    

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
print 'totalxs = {0:.2e} +- {1:.2e} | T = {5} :: highest probability that error is wrong {2:.2e} from sector #{3} :: total number of points for run {4}'.format(total_xs,math.sqrt(total_err_sq),highest_prob,highest_prob_sector_id,total_number_of_points_for_run, total_time_used)
                                                                                                    
    
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
    hist.print_analytics()


    
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







