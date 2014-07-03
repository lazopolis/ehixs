

import math


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
    def output_for_plotting(self):
        res='{0:>8.2f}\t{1:>8.2f}\t{2:>1.3e}\t'.format(self.lowend,self.highend, self.sigma)
        for sec_bin in self.indiv_sector_contributions:
            res += ' {0:>1.3e}\t'.format(sec_bin)
        return res
    def output_for_selected_sectors(self,sector_groups):
        res='{0:>8.2f}\t{1:>8.2f}\t{2:>1.3e}\t'.format(self.lowend,self.highend, self.sigma)
        for sec in sector_groups:
            sec_sigma = sum([self.indiv_sector_contributions[x] for x in sec])
            res += ' {0:>1.3e}\t'.format(sec_sigma)
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


