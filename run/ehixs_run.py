
from my_paths import *
from result_handler import *

class EhixsRun:
    def __init__(self,runcard,result_dir,sector_selection=''):
        self.collect_mode = False
        self.sector_selection = sector_selection
        self.the_paths = MyPaths(result_dir,runcard)
        self.the_paths.check_if_runcard_exists()
        self.the_paths.check_dir()
        self.the_paths.copy_runcard_to_dir()
        self.xml_filename = self.the_paths.result_dir_absolute_path+'/xml_info.out'
        self.running_mode = "parallel"
        self.collect_mode = False
        
    def query_ehixs_for_sectors(self):
        print '\n\n'+"[python script] Asking ehixs for sectors that match the selection criteria of the runcard"
        xml_log_filename = self.xml_filename+'.log'
        ehixs_query_command =self.the_paths.ehixs_bin_dir_path+'/ehixs --info -i'+self.the_paths.runcard+' --xml_info '+self.xml_filename+' > '+xml_log_filename
        print "[python script] with command : "+ehixs_query_command
        os.system(ehixs_query_command)
   
    def run_and_collect(self):
        self.query_ehixs_for_sectors()
        if self.running_mode=="serial":
            self.results = SerialRun(self.xml_filename,self.the_paths)
            self.results.perform()
        elif self.running_mode=="parallel":
            self.results = ParallelRun(self.xml_filename,self.the_paths)
            self.results.perform()
        elif self.running_mode=="lsf_cluster":
            self.results = ClusterLSFRun(self.xml_filename,self.the_paths)
            # in LSF running we need to specify whether we are in collect mode 
            self.results.collect_mode = self.collect_mode           
            self.results.perform()
        else:
            print "running mode is ill-defined : "+self.running_mode
            print "should be serial,parallel or lsf_cluster"
            quit()
    
    def total_xs(self):
        return self.results.total_xs
    def mc_error(self):
        return math.sqrt(self.results.total_err_sq)


