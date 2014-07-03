reset
set xrange[0:]
#set yrange[1e-10:]
set term png truecolor
set output "qqbar_channel.png"
#set boxwidth width*0.9
set tics out nomirror
set xlabel "x"
set ylabel "cross section (pb)"
set title "Higgs pT at NLO"
set logscale y 
#set style fill transparent solid 0.3 noborder
plot "qqbar_nlo_eff/HistogramHiggsPt" using ($1 + 5):($3) smooth freq with boxes lc rgb "blue" t "effective",\
"qqbar_nlo_enh/HistogramHiggsPt" using ($1 + 5):($3) smooth freq with boxes lc rgb "red" t "effective enhanced",\
"qqbar_nlo_exact/HistogramHiggsPt_channels" using ($1 + 5):($4) smooth freq with boxes lc rgb "green" t "exact",\
"qqbar_nlo_exact/HistogramHiggsPt_channels" using ($1 + 5):($3) smooth freq with boxes lc rgb "black" t "exact+ew"

