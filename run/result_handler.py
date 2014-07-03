
from sector import *
from histogram import *
from jobmanager import *

class ResultHandler:
    def __init__(self,sector_info_filename,the_paths):
        self.sector_info_filename = sector_info_filename
        self.the_paths = the_paths
        self.all_sectors = []
        self.all_histograms = []
        self.total_xs = 0.0
        self.total_err_sq = 0.0
        self.highest_prob = 0.0
        self.highest_prob_sector_id = -1                                                                                                
        self.total_number_of_points_for_run = 0
        self.total_time_used = 0
        self.sector_selection=''
    def perform(self):
        self.read_sector_info_and_setup_sectors()
        self.set_sector_selection=self.sector_selection
        self.run_all_sectors()        
        self.compute_total_xs()
        self.merge_histograms()
        self.write_output()
    def read_sector_info_and_setup_sectors(self):
        #reading the xml file
        run_tree = ET.parse(self.sector_info_filename)
        #constructing the root
        run_root = run_tree.getroot()
        #initializing the sectors
        sector_counter=0
        for xml_sector in run_root.getiterator('sector'):
            cs = Sector(self.the_paths)
            #the xml structure for <sector> is
            #<sector id="15" name="apotentiallyverylonguniquename">
            #retrieve id
            cs.id_number_in_ehixs = str(xml_sector.get('id'))
            #default filename S15.data
            cs.output_filename = self.the_paths.result_dir_absolute_path+'/S'+cs.id_number_in_ehixs+'.data'
            #retrieve name
            cs.ehixs_name = xml_sector.get('name')
            sector_counter +=1
            #if sector_counter in range(31,60):
            #all_sectors.append(cs)
            self.all_sectors.append(cs)
    
    def run_all_sectors(self):
        print "Error: if you see this polymorphism failed "
        quit()
    #total cross section and related quantities
    def compute_total_xs(self):
        for cs in self.all_sectors:
            self.total_xs = self.total_xs + cs.give('sigma')
            cur_err = cs.give('error')
            self.total_err_sq = self.total_err_sq + cur_err*cur_err
            cur_prob = cs.give('prob')
            self.total_time_used += cs.give('time')
            if cur_prob>self.highest_prob:
                self.highest_prob = cur_prob
                self.highest_prob_sector_id = int(cs.id_number_in_ehixs)                                                                                            
            self.total_number_of_points_for_run = self.total_number_of_points_for_run + cs.give('total_number_of_points')
        for cs in self.all_sectors:
            cs.printme(self.total_xs,self.total_time_used)
        print '------------'
        print 'totalxs = {0:.4e} +- {1:.2e} | T = {5} :: highest probability that error is wrong {2:.2e} from sector #{3} :: total number of points for run {4}'.format(self.total_xs,math.sqrt(self.total_err_sq),self.highest_prob,self.highest_prob_sector_id,self.total_number_of_points_for_run, self.total_time_used)
    #histogram merging  
    def merge_histograms(self):
        res_dir_path = self.the_paths.result_dir_absolute_path
        #setting up histograms
        for hist in self.all_sectors[0].res.getiterator('histogram'): 
            self.all_histograms.append(Histogram(hist.get('name')))
        
        for hist in self.all_histograms:
            #merging the current histogram (i.e. HiggsPt) from histograms of 
            #individual sectors
            hist.add_histogram(self.all_sectors)
            #printing out result
            hist.printme()
            hist.print_analytics()
            #writting output file for plotting (gnuplot mainly)
            cur_hist_filename = res_dir_path+'/'+hist.name
            with open(cur_hist_filename,'w') as outfile:
                outfile.write(hist.output_for_plotting())
        #special histogram collection for the selected sectors
        if self.sector_selection!='':
            for hist in self.all_histograms:
                cur_hist_filename = res_dir_path+'/'+hist.name+sector_selection
                with open(cur_hist_filename,'w') as outfile:
                    outfile.write(hist.output_for_selection(sector_selection))
    def write_output(self):
        output_fname = self.the_paths.result_dir_absolute_path+'/Total.data'
        #writting output    
        with open(output_fname,'w') as outfile:
            outfile.write('ehixs total output')
            for i,cs in enumerate(self.all_sectors):
                outfile.write('\n'+str(i)+':'+cs.ehixs_name)
            for cs in self.all_sectors:
                res = '\n'+str(cs.string_to_print(self.total_xs,self.total_time_used))
                res += '\n------------'
                res += '\ntotalxs = {0:.2e} +- {1:.2e} | T = {5} :: highest probability that error is wrong {2:.2e} from sector #{3} :: total number of points for run {4}'.format(self.total_xs,math.sqrt(self.total_err_sq),self.highest_prob,self.highest_prob_sector_id,self.total_number_of_points_for_run, self.total_time_used)
                outfile.write(res)
            for hist in self.all_histograms:
                res = '\n'+str(hist.string_to_print_analytics())
                outfile.write(res)    



class SerialRun(ResultHandler):
    def __init__(self,sector_info_filename,the_paths):
        ResultHandler.__init__(self,sector_info_filename,the_paths)
    def run_all_sectors(self):
        total_time_needed = 0.0
        print "[python script] Start running necessary sectors"
        for cs in self.all_sectors:
            if not(os.path.isfile(cs.output_filename)):
                cs.run()
            #print "would run "+cs.ehixs_name
            cs.read_results()
            total_time_needed = total_time_needed + cs.give('time')
        print "total time necessary : "+str(total_time_needed)
        
class ParallelRun(ResultHandler):
    def __init__(self,sector_info_filename,the_paths):
        ResultHandler.__init__(self,sector_info_filename,the_paths)
        self.number_of_cores = 4
        self.waiting_time_before_checking_for_status = 2.0 # in secs
    def run_all_sectors(self):
        the_manager = JobManager(self.all_sectors,self.number_of_cores,self.waiting_time_before_checking_for_status)
        the_manager.run()
        the_manager.collect()

class ClusterLSFRun(ResultHandler):
    def __init__(self,sector_info_filename,the_paths):
        ResultHandler.__init__(self,sector_info_filename,the_paths)
        self.collect_mode = False
    def run_all_sectors(self):
        if not(self.collect_mode):
            for cs in self.all_sectors:
                if not(os.path.isfile(cs.output_filename)):
                    cs.run_lsf()
            quit()
        else:
            for cs in self.all_sectors:
                cs.read_results()











