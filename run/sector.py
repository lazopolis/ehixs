

import os
import xml.etree.ElementTree as ET
import math
import subprocess,shlex



class Sector:
    """holds results for sector"""
    def __init__(self,the_paths):
        self.output_filename = ''
        self.id_number_in_ehixs = ''
        self.float_attributes = ['sigma','error','prob','time','secs_per_point']
        self.string_attributes = ['name','runcard_name']
        self.int_attributes = ['total_number_of_points']
        self.ehixs_name = "noname"
        self.was_fired = False
        self.paths = the_paths
    def run(self):
        print "[python script] firing sector "+self.ehixs_name
        os.system(self.command_to_fire_ehixs() + ' > '+self.output_filename+'.log')
    def run_lsf(self):
        print "[python script] firing sector "+self.ehixs_name
        lsf_script_filename = directory_name+'/script'+self.id_number_in_ehixs+'.sh'
        with open(lsf_script_filename,'w') as lsf_script:
            lsf_script.write("cd "+os.getcwd()+"\n"+self.paths.ehixs_bin_directory+self.command_to_fire_ehixs() + ' > '+self.output_filename+'.log')
        os.system("chmod 744 "+lsf_script_filename)
        os.system('bsub -q '+cluster_queue+' -J '+result_directory_name+self.id_number_in_ehixs+' '+lsf_script_filename)
    def run_in_the_background(self):
        print "[python script] firing: "+self.command_to_fire_ehixs()
        with open(self.output_filename+'.log','w') as logfile:
            self.proc = subprocess.Popen(self.command_to_fire_ehixs(),shell=True,stdout=logfile,stderr=logfile)
        self.was_fired = True
    def command_to_fire_ehixs(self):
        return self.paths.ehixs_bin_dir_path+'/ehixs '+'-i '+self.paths.runcard+' -s '+ str(self.id_number_in_ehixs)+' --output_filename '+self.output_filename
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
        for hist in self.res.getiterator('histogram'):
            if hist.get('name')==name:
                return hist
    def printme(self,XS,Ttot):
        print self.string_to_print(XS,Ttot)
    def string_to_print(self,XS,Ttot):
        rel_error = 0.0
        if math.fabs(self.give('sigma'))>0.0:
            rel_error = math.fabs(self.give('error')/self.give('sigma'))
            return '{0:>3} | {1:>14.4e} | {2:>10.2e} | {3:>8.1%} | {4:>8} | {5:>8.2f} s | {6:8.2e} s/p | {7:>8.1%} | {8:>8.1%}'.format(self.id_number_in_ehixs,self.give('sigma'), self.give('error'),rel_error,self.give('total_number_of_points'),self.give('time'),self.give('secs_per_point'),self.give('sigma')/XS,self.give('time')/Ttot)




