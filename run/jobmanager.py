
import os
import subprocess,shlex
from time import sleep

from highlight import *


class JobManager:
    def __init__(self,sectors,ncores,wt):
        self.sectors=sectors
        self.cores=ncores
        self.sectors_submitted = 0
        self.status_message = ""
        self.waiting_time_before_checking_for_status = wt
    def run(self):
        if not(os.path.isfile(self.sectors[0].output_filename)):
            while self.sectors_submitted<len(self.sectors):
                self.run_a_batch()
                self.wait()
            self.wait_till_all_sectors_finished()
    def collect(self):
        for cs in self.sectors:
            cs.read_results()
    def check_status(self):
        my_free_cores = self.cores
        running_sectors = 0
        new_message = '\n[python script] status'
        for cs in self.sectors:
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
        sleep(self.waiting_time_before_checking_for_status)




