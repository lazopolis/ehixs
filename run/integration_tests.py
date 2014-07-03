#! /usr/bin/env python

from ehixs_run import *
import unittest
import math
import os 

class TestSequenceFunctions(unittest.TestCase):

    def setUp(self):
        self.shit=0
    
    def shortcut(self,keyword,expected):
        #keyword = "NLO_effective_gg"
        dirname = "../tests/"+keyword
        cardname = "test_"+keyword + ".card"
        os.system("rm -rf "+dirname)
        self.the_run = EhixsRun(cardname,dirname)
        self.the_run.run_and_collect()
        xs = self.the_run.total_xs()
        #expected = 13.845
        deviation = math.fabs(xs - expected ) 
        self.assertTrue(deviation - self.the_run.mc_error() < 0, keyword+" wrong : "+ str(xs)+" vs expected = "+str(expected))
    
    def xtest_lo_effective(self):
        # checking the LO effective total cross section
        self.shortcut("LO_effective",7.608)
    
    def xtest_nlo_effective_gg(self):
        # checking the LO effective total cross section
        self.shortcut("NLO_effective_gg",13.845)
        
    def test_nlo_effective_qg(self):
        # checking the LO effective total cross section
        self.shortcut("NLO_effective_qg",-0.1966/2.0)
        
    def xtest_nlo_effective_qqbar(self):
        # checking the LO effective total cross section
        self.shortcut("NLO_effective_qqbar",0.00825)
        
    def xtest_nlo_effective(self):
        # checking the LO effective total cross section
        self.shortcut("NLO_effective",13.66)
        
    
    def xtest_nnlo_effective_gg(self):
        # checking the LO effective total cross section
        self.shortcut("NNLO_effective_gg",17.67)
    
    def xtest_nnlo_effective_qg(self):
        # checking the LO effective total cross section
        self.shortcut("NNLO_effective_qg",-0.645)
        
    def xtest_nnlo_effective_qqbar(self):
        # checking the LO effective total cross section
        self.shortcut("NNLO_effective_qqbar",0.01325)
               
    def xtest_nnlo_effective_qq(self):
        # checking the LO effective total cross section
        self.shortcut("NNLO_effective_qq",0.03108)
         
    def xtest_nnlo_effective_q1q2(self):
        # checking the LO effective total cross section
        self.shortcut("NNLO_effective_q1q2",0.0108)
            
    
    def xtest_nnlo_effective(self):
        # checking the LO effective total cross section
        self.shortcut("NNLO_effective",17.06)
            

if __name__ == '__main__':
    suite = unittest.TestLoader().loadTestsFromTestCase(TestSequenceFunctions)
    unittest.TextTestRunner(verbosity=2).run(suite)
