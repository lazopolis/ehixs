from bin import *


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
        for curbin in init_hist.getiterator('bin'):
            newbin = Bin()
            newbin.lowend = float(curbin.get('le'))
            newbin.highend = float(curbin.get('he'))
            self.all_bins.append(newbin)
        for cur_sector in all_sectors:
            self.totalsigma = self.totalsigma + cur_sector.give('sigma')
            my_hist = cur_sector.histogram(self.name)
            binindex = 0
            for curbin in my_hist.getiterator('bin'):
                self.all_bins[binindex].add(curbin)
                binindex = binindex+1
            for curbin in my_hist.getiterator('overflow'):
                self.overflow_bin.add(curbin)
            for tb in my_hist.getiterator('totalbinned'):
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
    def output_for_plotting(self):
        res = '\n#histogram: '+self.name
        num_sec = len(self.all_bins[0].indiv_sector_contributions)
        firstline='{0:>8}\t{1:>8}\t{2:>10}\t'.format("lowend","highend","sigma")
        for i in range(num_sec):
            firstline += '{0:>9}\t'.format('S'+str(i))
        res += '\n#\n#'+firstline
        for thebin in self.all_bins:
            res += '\n'+thebin.output_for_plotting()
        return res
    def output_for_selection(self,selection):
        groups = selection.split(';')
        sector_groups = []
        for x in groups:
            sector_group = [int(m) for m in x.split(',')]
            sector_groups.append(sector_group)
        
        res = '\n#histogram: '+self.name
        num_sec = len(sector_groups)
        firstline='{0:>8}\t{1:>8}\t{2:>10}\t'.format("lowend","highend","sigma")
        for x in sector_groups:
            firstline += '{0:>9}\t'.format('S'+str(x))
        res += '\n#\n#'+firstline
        for thebin in self.all_bins:
            res += '\n'+thebin.output_for_selected_sectors(sector_groups)
        return res


