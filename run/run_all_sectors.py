#! /usr/bin/env python

import sys
import getopt
#import os
#import subprocess,shlex
#import math
#import StringIO
#import xml.etree.ElementTree as ET
#from time import sleep

from ehixs_run import *


def main():
    runcard_local = "lo.card"
    ehixs_bin_directory = "../cmake_builds"
    result_directory_name = "default_test"
    running_mode = "parallel" # "parallel" or "serial" or "lsf_cluster"
    #waiting_time_before_checking_for_status = 2.0 # in secs
    #verbosity_level_for_checking_reports = "moderate" # or "moderate" or "zero"
    #number_of_cores = 3 # the script will be firing this number of jobs if in parallel
    #cluster_queue = "1nh"
    sector_selection = ''
    collect_mode = False
    help_message = ['usage:','-i inputfile : runcard (relative path)','-d output_directory : output dir (relative path)','-c : collect mode for cluster running', '-h : help','-s : serial running mode','-p : parallel running mode', '-l : lsf_cluster running mode',]
    help_string = ""
    for x in help_message:
        help_string += x+'\n'
    try:
        opts,args = getopt.getopt(sys.argv[1:],"hcspli:d:x:",["ifile=","ofile="])
    except getopt.GetoptError:
        print help_string
        quit()
    for opt,arg in opts:
        if opt=='-h':
            print help_string
            quit()
        elif opt=='-c':
            collect_mode=True
        elif opt=='-s':
            running_mode = "serial"
        elif opt=='-p':
            running_mode = "parallel"
        elif opt=='-l':
            running_mode = "lsf_cluster"
        elif opt=='-d':
            result_directory_name=arg
        elif opt=='-i':
            runcard_local=arg
        elif opt=='-x':
            sector_selection = arg
            print(sector_selection)
    the_run = EhixsRun(runcard_local,result_directory_name,sector_selection)
    the_run.collect_mode = collect_mode
    the_run.running_mode =  running_mode
    the_run.run_and_collect()


if __name__ == "__main__":
    main()


