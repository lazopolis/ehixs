import os

class MyPaths:
    """holds ehixs and result_dir paths """
    def __init__(self,dir_relative_path, runcard_relative_path):
        self.ehixs_bin_dir_path="../cmake_builds"
        self.runcard_rel_path = runcard_relative_path
        self.runcard=os.getcwd()+'/'+runcard_relative_path
        self.result_dir_absolute_path=os.getcwd()+'/'+dir_relative_path
        self.results_pre_existed = False
    #check to see if runcard is there
    def check_if_runcard_exists(self):
        if not(os.path.isfile(self.runcard_rel_path)):
            print "[python script] ERROR: runcard with name "+self.runcard_rel_path+" not found!"
            quit()
    def dir_exists(self):
        return os.path.isdir(self.result_dir_absolute_path)
    def check_dir(self):
        if not(self.dir_exists()):
            os.mkdir(self.result_dir_absolute_path)
        else:   
            self.results_pre_existed = True
    def copy_runcard_to_dir(self):
        os.system("cp "+self.runcard+" "+self.result_dir_absolute_path+"/"+self.runcard_rel_path)

