  
      subroutine rrgq2qght6
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh61J1  
      doubleprecision rrgq2qgh61J2  
      doubleprecision rrgq2qgh61J3  
      doubleprecision rrgq2qgh61J4  
      doubleprecision rrgq2qgh61J5  
      doubleprecision rrgq2qgh61J6  
      doubleprecision rrgq2qgh61J7  
      doubleprecision rrgq2qgh62J1  
      doubleprecision rrgq2qgh62J2  
      doubleprecision rrgq2qgh62J3  
      doubleprecision rrgq2qgh62J4  
      doubleprecision rrgq2qgh62J5  
      doubleprecision rrgq2qgh62J6  
      doubleprecision rrgq2qgh63J1  
      doubleprecision rrgq2qgh63J2  
      doubleprecision rrgq2qgh63J3  
      doubleprecision rrgq2qgh63J4  
      doubleprecision rrgq2qgh63J5  
      doubleprecision rrgq2qgh63J6  
      doubleprecision rrgq2qgh63J7  
      doubleprecision rrgq2qgh64J1  
      doubleprecision rrgq2qgh64J2  
      doubleprecision rrgq2qgh64J3  
      doubleprecision rrgq2qgh64J4  
      doubleprecision rrgq2qgh64J5  
      doubleprecision rrgq2qgh64J6  
      doubleprecision rrgq2qght6s1e1  
      doubleprecision rrgq2qght6s1e0  
      doubleprecision rrgq2qght6s1em1  
      doubleprecision rrgq2qght6s1em2  
      doubleprecision rrgq2qght6s1em3  
      doubleprecision rrgq2qght6s1em4  
      doubleprecision rrgq2qght6s2e1  
      doubleprecision rrgq2qght6s2e0  
      doubleprecision rrgq2qght6s2em1  
      doubleprecision rrgq2qght6s2em2  
      doubleprecision rrgq2qght6s2em3  
      doubleprecision rrgq2qght6s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght6s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght6s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght6s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght6s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght6s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght6s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght6s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght6s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght6s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght6s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght6s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght6s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght6s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh63J7
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / z
      t4 = pi * t3
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t5 * t8
      t10 = rrgq2qgh64J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = x4 * pi
      t17 = Sin(t16)
      t18 = t17 ** 2
      t19 = t15 * t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = t19 * t21
      t24 = log(0.4D1 * t22)
      t25 = t24 ** 2
      t26 = t25 * pi
      t27 = t3 * lh
      t30 = pi ** 2
      t34 = lh ** 2
      t37 = -0.60D2 * lh * t30 + 0.240D3 * zeta3 + 0.120D3 * t34 * lh
      t38 = t4 * t37
      t40 = t25 * t24 * pi
      t43 = t24 * pi
      t46 = -0.180D3 * t34 + 0.30D2 * t30
      t47 = t3 * t46
      t50 = (0.90D2 * t26 * t27 + t38 + 0.15D2 * t40 * t3 - t43 * t47) *
     # t5
      t51 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t52 = t8 * t51
      t59 = t4 * t46
      t61 = (-0.180D3 * t43 * t27 - 0.45D2 * t26 * t3 + t59) * t5
      t62 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t63 = t8 * t62
      t66 = t4 * lh
      t71 = (0.180D3 * t66 + 0.90D2 * t43 * t3) * t5
      t72 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t73 = t8 * t72
      t84 = t30 ** 2
      t85 = t34 ** 2
      t91 = t25 ** 2
      t96 = (-0.30D2 * t40 * t27 + t26 * t47 / 0.2D1 - t43 * t3 * t37 + 
     #t4 * (-0.480D3 * lh * zeta3 - t84 - 0.60D2 * t85 + 0.60D2 * t34 * 
     #t30) - 0.15D2 / 0.4D1 * t91 * pi * t3) * t5
      t97 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t98 = t8 * t97
      t101 = pi * t5
      t102 = t3 * t62
      t103 = x1 ** 2
      t104 = x3 * t103
      t107 = log(0.4D1 * t104 * t22)
      t108 = t107 * t3
      t110 = t107 ** 2
      t111 = t110 * t3
      t114 = t104 * t18
      t115 = t15 * t21
      t116 = -0.1D1 + x3
      t117 = 0.1D1 / t116
      t118 = t115 * t117
      t121 = log(-0.4D1 * t114 * t118)
      t123 = t121 ** 2
      t127 = cos(t16)
      t129 = Sqrt(-x3 * t116)
      t134 = 0.1D1 / (-z - x3 + 0.2D1 * t127 * t129 * z)
      t140 = pi * lh
      t141 = t3 * t51
      t150 = pi * t46
      t153 = t97 * t134 + t3 * t97
      t154 = t9 * t153
      t157 = 0.1D1 / x3
      t159 = 0.1D1 / x1
      t162 = t103 * t18
      t165 = log(0.4D1 * t162 * t115)
      t170 = t165 ** 2
      t173 = t170 * t165
      t181 = t9 * t97
      t182 = t38 * t181
      t193 = x2 ** 2
      t194 = x3 * t193
      t195 = t194 * t103
      t198 = log(0.4D1 * t195 * t22)
      t199 = t198 * t3
      t205 = log(-0.4D1 * t195 * t19 * t21 * t117)
      t217 = 0.1D1 / x2
      t218 = t217 * t159
      t221 = t193 * t103
      t224 = log(0.4D1 * t221 * t22)
      t225 = t224 ** 2
      t226 = t225 * t3
      t229 = t224 * t3
      t240 = t150 * t5
      t241 = t8 * t3
      t250 = t9 * t51
      t255 = x3 * t18
      t258 = log(0.4D1 * t255 * t115)
      t262 = log(-0.4D1 * t255 * t118)
      t264 = t258 * t3 + t262 * t134
      t266 = t262 ** 2
      t269 = t258 ** 2
      t273 = t266 * t262 * t134 / 0.6D1 + t269 * t258 * t3 / 0.6D1
      t280 = pi * t37
      t286 = -t3 - t134
      t296 = -t269 * t3 / 0.2D1 - t266 * t134 / 0.2D1
      t303 = log(0.4D1 * t194 * t22)
      t304 = t303 * t3
      t306 = t194 * t18
      t309 = log(-0.4D1 * t306 * t118)
      t311 = t309 ** 2
      t316 = t303 ** 2
      t317 = t316 * t3
      t339 = t193 * t18
      t342 = log(0.4D1 * t339 * t115)
      t343 = t342 * t3
      t348 = t342 ** 2
      t349 = t348 * t3
      t354 = t348 * t342 * t3
      t372 = t4 * t9 * t10 / 0.32D2 - t50 * t52 / 0.2880D4 - t61 * t63 /
     # 0.2880D4 - t71 * t73 / 0.2880D4 - t96 * t98 / 0.2880D4 - (-0.90D2
     # * t101 * t8 * (t102 - t108 * t51 + t111 * t97 / 0.2D1 + (t62 - t1
     #21 * t51 + t123 * t97 / 0.2D1) * t134) + 0.180D3 * t140 * t9 * (t1
     #41 - t108 * t97 + (t51 - t121 * t97) * t134) + t150 * t154) * t157
     # * t159 / 0.1440D4 + (t59 * t9 * (-t51 + t165 * t97) - 0.90D2 * t4
     # * t9 * (-t170 * t51 / 0.2D1 - t72 + t173 * t97 / 0.6D1 + t165 * t
     #62) - t182 + 0.180D3 * t66 * t9 * (t165 * t51 - t170 * t97 / 0.2D1
     # - t62)) * t159 / 0.1440D4 - (-0.90D2 * t101 * t8 * (-t199 * t97 +
     # (t51 - t205 * t97) * t134 + t141) + 0.180D3 * t140 * t154) * t157
     # * t218 / 0.720D3 + (-0.90D2 * t101 * t8 * (-t102 - t226 * t97 / 0
     #.2D1 + t229 * t51) + 0.180D3 * t140 * t9 * (-t141 + t229 * t97) - 
     #t240 * t241 * t97) * t217 * t159 / 0.720D3 + ((-0.90D2 * t101 * t6
     #3 + 0.180D3 * t140 * t250 + t150 * t181) * t264 - 0.90D2 * t101 * 
     #t98 * t273 + (t150 * t250 - 0.90D2 * t101 * t73 + t280 * t181 + 0.
     #180D3 * t140 * t9 * t62) * t286 + (-0.90D2 * t101 * t52 + 0.180D3 
     #* t140 * t181) * t296) * t157 / 0.2880D4 + (-0.90D2 * t101 * t8 * 
     #(-t102 + t304 * t51 - (t62 - t309 * t51 + t311 * t97 / 0.2D1) * t1
     #34 - t317 * t97 / 0.2D1) + 0.180D3 * t140 * t9 * (-(t51 - t309 * t
     #97) * t134 - t141 + t304 * t97) - t150 * t9 * t153) * t157 * t217 
     #/ 0.1440D4 + (t150 * t9 * (t343 * t97 - t141) - 0.90D2 * t101 * t8
     # * (-t349 * t51 / 0.2D1 - t3 * t72 + t354 * t97 / 0.6D1 + t343 * t
     #62) - t182 + 0.180D3 * t140 * t9 * (-t102 - t349 * t97 / 0.2D1 + t
     #343 * t51)) * t217 / 0.1440D4
      t373 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t372)
      t375 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t376 = t3 * t375
      t377 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t380 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t394 = t3 * t380
      t402 = -t3 * t377 - t377 * t134
      t415 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t424 = t280 * t5
      t425 = t241 * t377
      t426 = t424 * t425
      t437 = rrgq2qgh61J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t441 = t8 * t380
      t444 = t8 * t375
      t447 = t8 * t415
      t450 = t8 * t377
      t455 = t9 * t380
      t458 = t9 * t377
      t504 = -t9 * t402
      t564 = (-0.90D2 * t101 * t8 * (-t376 - t317 * t377 / 0.2D1 - (t375
     # - t309 * t380 + t311 * t377 / 0.2D1) * t134 + t304 * t380) + 0.18
     #0D3 * t140 * t9 * (-(t380 - t309 * t377) * t134 - t394 + t304 * t3
     #77) + t150 * t9 * t402) * t157 * t217 / 0.1440D4 + (t150 * t9 * (t
     #343 * t377 - t394) - 0.90D2 * t101 * t8 * (-t349 * t380 / 0.2D1 - 
     #t3 * t415 + t354 * t377 / 0.6D1 + t343 * t375) - t426 + 0.180D3 * 
     #t140 * t9 * (-t376 - t349 * t377 / 0.2D1 + t343 * t380)) * t217 / 
     #0.1440D4 + t4 * t9 * t437 / 0.32D2 - t50 * t441 / 0.2880D4 - t61 *
     # t444 / 0.2880D4 - t71 * t447 / 0.2880D4 - t96 * t450 / 0.2880D4 +
     # ((-0.90D2 * t101 * t444 + 0.180D3 * t140 * t455 + t150 * t458) * 
     #t264 - 0.90D2 * t101 * t450 * t273 + (t150 * t455 - 0.90D2 * t101 
     #* t447 + t280 * t458 + 0.180D3 * t140 * t9 * t375) * t286 + (-0.90
     #D2 * t101 * t441 + 0.180D3 * t140 * t458) * t296) * t157 / 0.2880D
     #4 - (-0.90D2 * t101 * t8 * (t376 - t108 * t380 + t111 * t377 / 0.2
     #D1 + (t375 - t121 * t380 + t123 * t377 / 0.2D1) * t134) + 0.180D3 
     #* t140 * t9 * (t394 - t108 * t377 + (t380 - t121 * t377) * t134) +
     # t150 * t504) * t157 * t159 / 0.1440D4 + (t59 * t9 * (-t380 + t165
     # * t377) - 0.90D2 * t4 * t9 * (t173 * t377 / 0.6D1 - t170 * t380 /
     # 0.2D1 - t415 + t165 * t375) - t426 + 0.180D3 * t66 * t9 * (-t375 
     #+ t165 * t380 - t170 * t377 / 0.2D1)) * t159 / 0.1440D4 - (-0.90D2
     # * t101 * t8 * ((t380 - t205 * t377) * t134 - t199 * t377 + t394) 
     #+ 0.180D3 * t140 * t504) * t157 * t218 / 0.720D3 + (-0.90D2 * t101
     # * t8 * (t229 * t380 - t376 - t226 * t377 / 0.2D1) + 0.180D3 * t14
     #0 * t9 * (t229 * t377 - t394) - t240 * t425) * t217 * t159 / 0.720
     #D3
      t565 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t564)
      t567 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t568 = t3 * t567
      t569 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t571 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t583 = t3 * t569
      t594 = t3 * t571 + t571 * t134
      t595 = t9 * t594
      t607 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t615 = t9 * t571
      t616 = t38 * t615
      t710 = rrgq2qgh63J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t714 = t8 * t569
      t717 = t8 * t567
      t720 = t8 * t607
      t725 = t9 * t569
      t731 = t8 * t571
      t755 = -(-0.90D2 * t101 * t8 * (t568 - t108 * t569 + t111 * t571 /
     # 0.2D1 + (-t121 * t569 + t123 * t571 / 0.2D1 + t567) * t134) + 0.1
     #80D3 * t140 * t9 * (t583 - t108 * t571 + (t569 - t121 * t571) * t1
     #34) + t150 * t595) * t157 * t159 / 0.1440D4 + (t59 * t9 * (t165 * 
     #t571 - t569) - 0.90D2 * t4 * t9 * (-t170 * t569 / 0.2D1 - t607 + t
     #173 * t571 / 0.6D1 + t165 * t567) - t616 + 0.180D3 * t66 * t9 * (-
     #t170 * t571 / 0.2D1 + t165 * t569 - t567)) * t159 / 0.1440D4 - (-0
     #.90D2 * t101 * t8 * (t583 - t199 * t571 + (t569 - t205 * t571) * t
     #134) + 0.180D3 * t140 * t595) * t157 * t218 / 0.720D3 + (-0.90D2 *
     # t101 * t8 * (-t568 + t229 * t569 - t226 * t571 / 0.2D1) + 0.180D3
     # * t140 * t9 * (-t583 + t229 * t571) - t240 * t241 * t571) * t217 
     #* t159 / 0.720D3 + (-0.90D2 * t101 * t8 * (-(-t309 * t569 + t311 *
     # t571 / 0.2D1 + t567) * t134 - t568 + t304 * t569 - t317 * t571 / 
     #0.2D1) + 0.180D3 * t140 * t9 * (-(t569 - t309 * t571) * t134 - t58
     #3 + t304 * t571) - t150 * t9 * t594) * t157 * t217 / 0.1440D4 + (t
     #150 * t9 * (-t583 + t343 * t571) - 0.90D2 * t101 * t8 * (-t349 * t
     #569 / 0.2D1 - t3 * t607 + t354 * t571 / 0.6D1 + t343 * t567) - t61
     #6 + 0.180D3 * t140 * t9 * (-t568 + t343 * t569 - t349 * t571 / 0.2
     #D1)) * t217 / 0.1440D4 + t4 * t9 * t710 / 0.32D2 - t50 * t714 / 0.
     #2880D4 - t61 * t717 / 0.2880D4 - t71 * t720 / 0.2880D4 + ((-0.90D2
     # * t101 * t717 + 0.180D3 * t140 * t725 + t150 * t615) * t264 - 0.9
     #0D2 * t101 * t731 * t273 + (t150 * t725 - 0.90D2 * t101 * t720 + t
     #280 * t615 + 0.180D3 * t140 * t9 * t567) * t286 + (-0.90D2 * t101 
     #* t714 + 0.180D3 * t140 * t615) * t296) * t157 / 0.2880D4 - t96 * 
     #t731 / 0.2880D4
      t756 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t755)
      t758 = t2 * x1
      t759 = -0.1D1 + x1
      t760 = x1 * z
      t761 = 0.1D1 - x1 + t760
      t762 = 0.1D1 / t761
      t764 = t2 * t759 * t762
      t765 = s * t20
      t767 = x1 * t759 * t762
      t768 = t765 * t767
      t769 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, t
     #758, 0.0D0, -t768)
      t772 = t21 * t762
      t773 = t759 ** 2
      t778 = log(-0.4D1 * t104 * t19 * t772 * t773 * t117)
      t779 = t778 * t761
      t780 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, t
     #758, 0.0D0, -t768)
      t782 = t778 ** 2
      t783 = t782 * t761
      t784 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, t
     #758, 0.0D0, -t768)
      t788 = x3 * t761
      t790 = Sqrt(-t788 * t116)
      t794 = x3 * x1
      t795 = t794 * z
      t796 = 0.3D1 * t795
      t797 = x1 * t14
      t798 = x3 * t14
      t799 = t798 * x1
      t800 = 0.2D1 * t794
      t802 = 0.2D1 * t104 * z
      t803 = t104 * t14
      t804 = -z - x3 + t760 + 0.2D1 * t127 * t790 * z - t104 - t796 - t7
     #97 + t799 + t800 + t802 - t803
      t805 = 0.1D1 / t804
      t807 = t3 * t769
      t808 = t762 * t773
      t809 = t115 * t808
      t812 = log(0.4D1 * t114 * t809)
      t813 = t812 * t3
      t815 = t812 ** 2
      t816 = t815 * t3
      t823 = t761 * t780
      t828 = t3 * t780
      t837 = t9 * (-t3 * t784 - t761 * t784 * t805)
      t843 = t162 * t15
      t844 = t772 * t773
      t847 = log(0.4D1 * t843 * t844)
      t852 = t847 ** 2
      t855 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, t
     #758, 0.0D0, -t768)
      t856 = t852 * t847
      t876 = t194 * t162
      t881 = log(-0.4D1 * t876 * t115 * t808 * t117)
      t882 = t881 * t761
      t888 = log(0.4D1 * t876 * t809)
      t889 = t888 * t3
      t901 = t221 * t18
      t904 = log(0.4D1 * t901 * t809)
      t905 = t904 * t3
      t907 = t904 ** 2
      t908 = t907 * t3
      t926 = -(-0.90D2 * t101 * t8 * ((-t761 * t769 + t779 * t780 - t783
     # * t784 / 0.2D1) * t805 - t807 + t813 * t780 - t816 * t784 / 0.2D1
     #) + 0.180D3 * t140 * t9 * ((-t823 + t779 * t784) * t805 + t813 * t
     #784 - t828) + t150 * t837) * t157 * t159 / 0.1440D4 + (t59 * t9 * 
     #(t780 - t847 * t784) - 0.90D2 * t4 * t9 * (t852 * t780 / 0.2D1 + t
     #855 - t856 * t784 / 0.6D1 - t847 * t769) + t38 * t9 * t784 + 0.180
     #D3 * t66 * t9 * (t852 * t784 / 0.2D1 - t847 * t780 + t769)) * t159
     # / 0.1440D4 - (-0.90D2 * t101 * t8 * (-t828 + (-t823 + t882 * t784
     #) * t805 + t889 * t784) + 0.180D3 * t140 * t837) * t157 * t218 / 0
     #.720D3 + (-0.90D2 * t101 * t8 * (-t905 * t780 + t908 * t784 / 0.2D
     #1 + t807) + 0.180D3 * t140 * t9 * (-t905 * t784 + t828) + t240 * t
     #241 * t784) * t217 * t159 / 0.720D3
      t927 = FJET(XB1, XB2, s, 0.0D0, t758, -t764, 0.0D0, -t768, t926)
      t929 = x2 * s
      t930 = t929 * t1
      t931 = -0.1D1 + x2
      t932 = t931 * s
      t933 = t932 * t1
      t934 = x2 * z
      t936 = 0.1D1 / (-z + t934 - x2)
      t937 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0.
     #0D0, 0.0D0, 0.0D0)
      t938 = t936 * t937
      t939 = t115 * t931
      t942 = log(-0.4D1 * t306 * t939)
      t943 = t942 ** 2
      t944 = t943 * t936
      t945 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0.
     #0D0, 0.0D0, 0.0D0)
      t948 = t942 * t936
      t949 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0.
     #0D0, 0.0D0, 0.0D0)
      t955 = t936 * t949
      t961 = t8 * t936
      t962 = t961 * t945
      t963 = t240 * t962
      t970 = log(-0.4D1 * t339 * t939)
      t971 = t970 * t936
      t976 = t970 ** 2
      t977 = t976 * t936
      t980 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0.
     #0D0, 0.0D0, 0.0D0)
      t983 = t976 * t970 * t936
      t1002 = t21 * t931
      t1006 = log(-0.4D1 * t195 * t19 * t1002)
      t1007 = t1006 * t936
      t1013 = t140 * t5
      t1022 = log(-0.4D1 * t901 * t939)
      t1023 = t1022 ** 2
      t1024 = t1023 * t936
      t1027 = t1022 * t936
      t1042 = (-0.90D2 * t101 * t8 * (-t938 - t944 * t945 / 0.2D1 + t948
     # * t949) + 0.180D3 * t140 * t9 * (-t955 + t948 * t945) - t963) * t
     #157 * t217 / 0.1440D4 + (t150 * t9 * (-t955 + t971 * t945) - 0.90D
     #2 * t101 * t8 * (-t977 * t949 / 0.2D1 - t936 * t980 + t983 * t945 
     #/ 0.6D1 + t971 * t937) - t424 * t962 + 0.180D3 * t140 * t9 * (-t93
     #8 + t971 * t949 - t977 * t945 / 0.2D1)) * t217 / 0.1440D4 - (-0.90
     #D2 * t101 * t8 * (t955 - t1007 * t945) + 0.180D3 * t1013 * t962) *
     # t157 * t218 / 0.720D3 + (-0.90D2 * t101 * t8 * (-t1024 * t945 / 0
     #.2D1 + t1027 * t949 - t938) + 0.180D3 * t140 * t9 * (t1027 * t945 
     #- t955) - t963) * t217 * t159 / 0.720D3
      t1043 = FJET(XB1, XB2, s, 0.0D0, t930, 0.0D0, -t933, 0.0D0, t1042)
      t1045 = x2 * x3
      t1048 = Sqrt(x3 * t931 * t116)
      t1049 = t127 * t1048
      t1051 = 0.2D1 * t1049 * x2
      t1053 = 0.1D1 - x3 + t1045
      t1054 = 0.1D1 / t1053
      t1056 = t2 * (0.1D1 - x3 - x2 + t1045 + t194 + t1051) * t1054
      t1061 = t2 * x2 * (-0.1D1 + t1045 + 0.2D1 * t1049) * t1054
      t1062 = t1045 * z
      t1063 = t194 * z
      t1069 = 0.1D1 / (z - t934 - t1062 + t1063 + x3 - t194 + x2 - t1051
     # - 0.2D1 * t1049 * z + 0.2D1 * t1049 * t934)
      t1070 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, -t1061, t1056,
     # 0.0D0, 0.0D0, 0.0D0)
      t1071 = t1069 * t1070
      t1072 = t931 * t116
      t1073 = t1053 ** 2
      t1074 = 0.1D1 / t1073
      t1075 = t1072 * t1074
      t1079 = log(0.4D1 * t876 * t115 * t1075)
      t1080 = t1079 * t1069
      t1081 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, -t1061, t1056,
     # 0.0D0, 0.0D0, 0.0D0)
      t1087 = t8 * t1069
      t1088 = t1087 * t1081
      t1100 = log(0.4D1 * t194 * t19 * t1002 * t116 * t1074)
      t1101 = t1100 * t1069
      t1103 = t1100 ** 2
      t1104 = t1103 * t1069
      t1107 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, -t1061, t1056,
     # 0.0D0, 0.0D0, 0.0D0)
      t1123 = -(-0.90D2 * t101 * t8 * (t1071 - t1080 * t1081) + 0.180D3 
     #* t1013 * t1088) * t157 * t218 / 0.720D3 + (-0.90D2 * t101 * t8 * 
     #(t1101 * t1070 - t1104 * t1081 / 0.2D1 - t1069 * t1107) + 0.180D3 
     #* t140 * t9 * (-t1071 + t1101 * t1081) - t240 * t1088) * t157 * t2
     #17 / 0.1440D4
      t1124 = FJET(XB1, XB2, s, 0.0D0, t1056, 0.0D0, -t1061, 0.0D0, t112
     #3)
      t1126 = t1 * t759
      t1128 = t932 * t1126 * t762
      t1129 = t929 * t1126
      t1131 = t765 * t931 * t767
      t1136 = log(-0.4D1 * t876 * t115 * t808 * t931)
      t1137 = x2 * x1
      t1138 = t1137 * z
      t1140 = 0.1D1 / (-t1137 + z - t934 + x2 + t1138)
      t1141 = t1136 * t1140
      t1142 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, -t1129, t1128,
     # t758, 0.0D0, t1131)
      t1144 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, -t1129, t1128,
     # t758, 0.0D0, t1131)
      t1145 = t1140 * t1144
      t1150 = t8 * t1140
      t1151 = t1150 * t1142
      t1162 = log(-0.4D1 * t221 * t19 * t772 * t773 * t931)
      t1163 = t1162 ** 2
      t1164 = t1163 * t1140
      t1167 = t1162 * t1140
      t1169 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, -t1129, t1128,
     # t758, 0.0D0, t1131)
      t1185 = -(-0.90D2 * t101 * t8 * (-t1141 * t1142 + t1145) + 0.180D3
     # * t1013 * t1151) * t157 * t218 / 0.720D3 + (-0.90D2 * t101 * t8 *
     # (-t1164 * t1142 / 0.2D1 + t1167 * t1144 - t1140 * t1169) + 0.180D
     #3 * t140 * t9 * (t1167 * t1142 - t1145) - t240 * t1151) * t217 * t
     #159 / 0.720D3
      t1186 = FJET(XB1, XB2, s, 0.0D0, t1128, t758, -t1129, t1131, t1185
     #)
      t1188 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0
     #.0D0, 0.0D0, 0.0D0)
      t1190 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0
     #.0D0, 0.0D0, 0.0D0)
      t1191 = t936 * t1190
      t1196 = t961 * t1188
      t1205 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0
     #.0D0, 0.0D0, 0.0D0)
      t1206 = t936 * t1205
      t1217 = t240 * t1196
      t1244 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0
     #.0D0, 0.0D0, 0.0D0)
      t1264 = -(-0.90D2 * t101 * t8 * (-t1007 * t1188 + t1191) + 0.180D3
     # * t1013 * t1196) * t157 * t218 / 0.720D3 + (-0.90D2 * t101 * t8 *
     # (-t1024 * t1188 / 0.2D1 - t1206 + t1027 * t1190) + 0.180D3 * t140
     # * t9 * (t1027 * t1188 - t1191) - t1217) * t217 * t159 / 0.720D3 +
     # (-0.90D2 * t101 * t8 * (-t1206 - t944 * t1188 / 0.2D1 + t948 * t1
     #190) + 0.180D3 * t140 * t9 * (-t1191 + t948 * t1188) - t1217) * t1
     #57 * t217 / 0.1440D4 + (t150 * t9 * (-t1191 + t971 * t1188) - 0.90
     #D2 * t101 * t8 * (-t977 * t1190 / 0.2D1 - t936 * t1244 + t971 * t1
     #205 + t983 * t1188 / 0.6D1) - t424 * t1196 + 0.180D3 * t140 * t9 *
     # (-t977 * t1188 / 0.2D1 + t971 * t1190 - t1206)) * t217 / 0.1440D4
      t1265 = FJET(XB1, XB2, s, 0.0D0, -t933, 0.0D0, t930, 0.0D0, t1264)
      t1267 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, 
     #t758, 0.0D0, -t768)
      t1269 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, 
     #t758, 0.0D0, -t768)
      t1271 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, 
     #t758, 0.0D0, -t768)
      t1276 = t3 * t1267
      t1284 = t761 * t1269
      t1289 = t3 * t1269
      t1298 = t9 * (-t3 * t1271 - t761 * t1271 * t805)
      t1310 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, 
     #t758, 0.0D0, -t768)
      t1362 = -(-0.90D2 * t101 * t8 * ((-t761 * t1267 + t779 * t1269 - t
     #783 * t1271 / 0.2D1) * t805 - t1276 - t816 * t1271 / 0.2D1 + t813 
     #* t1269) + 0.180D3 * t140 * t9 * ((-t1284 + t779 * t1271) * t805 +
     # t813 * t1271 - t1289) + t150 * t1298) * t157 * t159 / 0.1440D4 + 
     #(t59 * t9 * (t1269 - t847 * t1271) - 0.90D2 * t4 * t9 * (t852 * t1
     #269 / 0.2D1 + t1310 - t847 * t1267 - t856 * t1271 / 0.6D1) + t38 *
     # t9 * t1271 + 0.180D3 * t66 * t9 * (t1267 - t847 * t1269 + t852 * 
     #t1271 / 0.2D1)) * t159 / 0.1440D4 - (-0.90D2 * t101 * t8 * ((-t128
     #4 + t882 * t1271) * t805 + t889 * t1271 - t1289) + 0.180D3 * t140 
     #* t1298) * t157 * t218 / 0.720D3 + (-0.90D2 * t101 * t8 * (-t905 *
     # t1269 + t908 * t1271 / 0.2D1 + t1276) + 0.180D3 * t140 * t9 * (-t
     #905 * t1271 + t1289) + t240 * t241 * t1271) * t217 * t159 / 0.720D
     #3
      t1363 = FJET(XB1, XB2, s, 0.0D0, -t764, t758, 0.0D0, -t768, t1362)
      t1365 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, -t1061, t1056,
     # 0.0D0, 0.0D0, 0.0D0)
      t1367 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, -t1061, t1056,
     # 0.0D0, 0.0D0, 0.0D0)
      t1370 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, -t1061, t1056,
     # 0.0D0, 0.0D0, 0.0D0)
      t1376 = t1069 * t1365
      t1382 = t1087 * t1367
      t1399 = (-0.90D2 * t101 * t8 * (t1101 * t1365 - t1104 * t1367 / 0.
     #2D1 - t1069 * t1370) + 0.180D3 * t140 * t9 * (-t1376 + t1101 * t13
     #67) - t240 * t1382) * t157 * t217 / 0.1440D4 - (-0.90D2 * t101 * t
     #8 * (t1376 - t1080 * t1367) + 0.180D3 * t1013 * t1382) * t157 * t2
     #18 / 0.720D3
      t1400 = FJET(XB1, XB2, s, 0.0D0, -t1061, 0.0D0, t1056, 0.0D0, t139
     #9)
      t1402 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1403 = t8 * t1402
      t1406 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1407 = t9 * t1406
      t1410 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1411 = t9 * t1410
      t1415 = t8 * t1410
      t1420 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1421 = t8 * t1420
      t1430 = t8 * t1406
      t1450 = t3 * t1402
      t1455 = t3 * t1406
      t1466 = t3 * t1410 + t1410 * t134
      t1467 = t9 * t1466
      t1486 = t38 * t1411
      t1584 = rrgq2qgh62J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1590 = ((-0.90D2 * t101 * t1403 + 0.180D3 * t140 * t1407 + t150 *
     # t1411) * t264 - 0.90D2 * t101 * t1415 * t273 + (t150 * t1407 - 0.
     #90D2 * t101 * t1421 + t280 * t1411 + 0.180D3 * t140 * t9 * t1402) 
     #* t286 + (-0.90D2 * t101 * t1430 + 0.180D3 * t140 * t1411) * t296)
     # * t157 / 0.2880D4 - t96 * t1415 / 0.2880D4 - (-0.90D2 * t101 * t8
     # * (-t108 * t1406 + (-t121 * t1406 + t123 * t1410 / 0.2D1 + t1402)
     # * t134 + t111 * t1410 / 0.2D1 + t1450) + 0.180D3 * t140 * t9 * (t
     #1455 - t108 * t1410 + (t1406 - t121 * t1410) * t134) + t150 * t146
     #7) * t157 * t159 / 0.1440D4 + (t59 * t9 * (-t1406 + t165 * t1410) 
     #- 0.90D2 * t4 * t9 * (-t1420 + t165 * t1402 + t173 * t1410 / 0.6D1
     # - t170 * t1406 / 0.2D1) - t1486 + 0.180D3 * t66 * t9 * (-t1402 - 
     #t170 * t1410 / 0.2D1 + t165 * t1406)) * t159 / 0.1440D4 - (-0.90D2
     # * t101 * t8 * (-t199 * t1410 + t1455 + (t1406 - t205 * t1410) * t
     #134) + 0.180D3 * t140 * t1467) * t157 * t218 / 0.720D3 + (-0.90D2 
     #* t101 * t8 * (t229 * t1406 - t1450 - t226 * t1410 / 0.2D1) + 0.18
     #0D3 * t140 * t9 * (t229 * t1410 - t1455) - t240 * t241 * t1410) * 
     #t217 * t159 / 0.720D3 - t50 * t1430 / 0.2880D4 - t61 * t1403 / 0.2
     #880D4 + (-0.90D2 * t101 * t8 * (-t1450 + t304 * t1406 - (-t309 * t
     #1406 + t311 * t1410 / 0.2D1 + t1402) * t134 - t317 * t1410 / 0.2D1
     #) + 0.180D3 * t140 * t9 * (-t1455 + t304 * t1410 - (t1406 - t309 *
     # t1410) * t134) - t150 * t9 * t1466) * t157 * t217 / 0.1440D4 + (t
     #150 * t9 * (-t1455 + t343 * t1410) - 0.90D2 * t101 * t8 * (-t3 * t
     #1420 + t343 * t1402 + t354 * t1410 / 0.6D1 - t349 * t1406 / 0.2D1)
     # - t1486 + 0.180D3 * t140 * t9 * (-t349 * t1410 / 0.2D1 - t1450 + 
     #t343 * t1406)) * t217 / 0.1440D4 + t4 * t9 * t1584 / 0.32D2 - t71 
     #* t1421 / 0.2880D4
      t1591 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1590)
      t1593 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, 
     #t758, 0.0D0, -t768)
      t1595 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, 
     #t758, 0.0D0, -t768)
      t1597 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, 
     #t758, 0.0D0, -t768)
      t1603 = t3 * t1593
      t1610 = t761 * t1595
      t1615 = t3 * t1595
      t1624 = t9 * (-t3 * t1597 - t761 * t1597 * t805)
      t1638 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, 
     #t758, 0.0D0, -t768)
      t1688 = -(-0.90D2 * t101 * t8 * ((-t761 * t1593 + t779 * t1595 - t
     #783 * t1597 / 0.2D1) * t805 + t813 * t1595 - t1603 - t816 * t1597 
     #/ 0.2D1) + 0.180D3 * t140 * t9 * ((-t1610 + t779 * t1597) * t805 +
     # t813 * t1597 - t1615) + t150 * t1624) * t157 * t159 / 0.1440D4 + 
     #(t59 * t9 * (-t847 * t1597 + t1595) - 0.90D2 * t4 * t9 * (t852 * t
     #1595 / 0.2D1 - t856 * t1597 / 0.6D1 + t1638 - t847 * t1593) + t38 
     #* t9 * t1597 + 0.180D3 * t66 * t9 * (t852 * t1597 / 0.2D1 - t847 *
     # t1595 + t1593)) * t159 / 0.1440D4 - (-0.90D2 * t101 * t8 * (t889 
     #* t1597 + (-t1610 + t882 * t1597) * t805 - t1615) + 0.180D3 * t140
     # * t1624) * t157 * t218 / 0.720D3 + (-0.90D2 * t101 * t8 * (t1603 
     #- t905 * t1595 + t908 * t1597 / 0.2D1) + 0.180D3 * t140 * t9 * (t1
     #615 - t905 * t1597) + t240 * t241 * t1597) * t217 * t159 / 0.720D3
      t1689 = FJET(XB1, XB2, s, t758, 0.0D0, 0.0D0, -t764, -t768, t1688)
      t1691 = t373 * t372 + t565 * t564 + t756 * t755 + t927 * t926 + t1
     #043 * t1042 + t1124 * t1123 + t1186 * t1185 + t1265 * t1264 + t136
     #3 * t1362 + t1400 * t1399 + t1591 * t1590 + t1689 * t1688
      t1692 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, -t1129, t1128,
     # t758, 0.0D0, t1131)
      t1693 = t1140 * t1692
      t1694 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, -t1129, t1128,
     # t758, 0.0D0, t1131)
      t1700 = t1150 * t1694
      t1709 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, -t1129, t1128,
     # t758, 0.0D0, t1131)
      t1725 = -(-0.90D2 * t101 * t8 * (t1693 - t1141 * t1694) + 0.180D3 
     #* t1013 * t1700) * t157 * t218 / 0.720D3 + (-0.90D2 * t101 * t8 * 
     #(-t1164 * t1694 / 0.2D1 + t1167 * t1692 - t1140 * t1709) + 0.180D3
     # * t140 * t9 * (t1167 * t1694 - t1693) - t240 * t1700) * t217 * t1
     #59 / 0.720D3
      t1726 = FJET(XB1, XB2, s, t758, -t1129, 0.0D0, t1128, t1131, t1725
     #)
      t1728 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0
     #.0D0, 0.0D0, 0.0D0)
      t1730 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0
     #.0D0, 0.0D0, 0.0D0)
      t1731 = t936 * t1730
      t1736 = t961 * t1728
      t1746 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0
     #.0D0, 0.0D0, 0.0D0)
      t1747 = t936 * t1746
      t1757 = t240 * t1736
      t1786 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0
     #.0D0, 0.0D0, 0.0D0)
      t1804 = -(-0.90D2 * t101 * t8 * (-t1007 * t1728 + t1731) + 0.180D3
     # * t1013 * t1736) * t157 * t218 / 0.720D3 + (-0.90D2 * t101 * t8 *
     # (t1027 * t1730 - t1024 * t1728 / 0.2D1 - t1747) + 0.180D3 * t140 
     #* t9 * (t1027 * t1728 - t1731) - t1757) * t217 * t159 / 0.720D3 + 
     #(-0.90D2 * t101 * t8 * (t948 * t1730 - t1747 - t944 * t1728 / 0.2D
     #1) + 0.180D3 * t140 * t9 * (-t1731 + t948 * t1728) - t1757) * t157
     # * t217 / 0.1440D4 + (t150 * t9 * (-t1731 + t971 * t1728) - 0.90D2
     # * t101 * t8 * (-t977 * t1730 / 0.2D1 + t983 * t1728 / 0.6D1 - t93
     #6 * t1786 + t971 * t1746) - t424 * t1736 + 0.180D3 * t140 * t9 * (
     #-t977 * t1728 / 0.2D1 + t971 * t1730 - t1747)) * t217 / 0.1440D4
      t1805 = FJET(XB1, XB2, s, t930, 0.0D0, -t933, 0.0D0, 0.0D0, t1804)
      t1807 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, -t1061, t1056,
     # 0.0D0, 0.0D0, 0.0D0)
      t1809 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, -t1061, t1056,
     # 0.0D0, 0.0D0, 0.0D0)
      t1810 = t1069 * t1809
      t1815 = t1087 * t1807
      t1823 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, -t1061, t1056,
     # 0.0D0, 0.0D0, 0.0D0)
      t1841 = -(-0.90D2 * t101 * t8 * (-t1080 * t1807 + t1810) + 0.180D3
     # * t1013 * t1815) * t157 * t218 / 0.720D3 + (-0.90D2 * t101 * t8 *
     # (t1101 * t1809 - t1069 * t1823 - t1104 * t1807 / 0.2D1) + 0.180D3
     # * t140 * t9 * (-t1810 + t1101 * t1807) - t240 * t1815) * t157 * t
     #217 / 0.1440D4
      t1842 = FJET(XB1, XB2, s, t1056, 0.0D0, -t1061, 0.0D0, 0.0D0, t184
     #1)
      t1844 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, -t1129, t1128,
     # t758, 0.0D0, t1131)
      t1846 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, -t1129, t1128,
     # t758, 0.0D0, t1131)
      t1847 = t1140 * t1846
      t1852 = t1150 * t1844
      t1861 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, -t1129, t1128,
     # t758, 0.0D0, t1131)
      t1877 = -(-0.90D2 * t101 * t8 * (-t1141 * t1844 + t1847) + 0.180D3
     # * t1013 * t1852) * t157 * t218 / 0.720D3 + (-0.90D2 * t101 * t8 *
     # (t1167 * t1846 - t1164 * t1844 / 0.2D1 - t1140 * t1861) + 0.180D3
     # * t140 * t9 * (-t1847 + t1167 * t1844) - t240 * t1852) * t217 * t
     #159 / 0.720D3
      t1878 = FJET(XB1, XB2, s, t1128, 0.0D0, -t1129, t758, t1131, t1877
     #)
      t1881 = t758 * t1045 * t1054
      t1882 = t2 * t759
      t1883 = t194 * t760
      t1885 = Sqrt(t788 * t1072)
      t1886 = t127 * t1885
      t1888 = 0.2D1 * t1886 * x2
      t1889 = t194 * x1
      t1893 = t1882 * (t1883 + t1888 + 0.1D1 - x3 - x2 + t1045 + t194 - 
     #t1889) * t762 * t1054
      t1897 = t116 * s * t1 * x1 * t1054
      t1903 = t1882 * x2 * (-0.1D1 + t1045 + x1 - t794 - t760 + t795 + 0
     #.2D1 * t1886) * t762 * t1054
      t1909 = x2 * t103
      t1913 = t934 - t796 + t799 + t802 - t803 + 0.2D1 * t1886 * z - t10
     #45 * x1 + t104 * x2 + t1137 * t14 + 0.2D1 * t1909 * z - t1909 * t1
     #4 - t104 - t797 + t800 - x3 + t1062 - t1063
      t1929 = -x2 - z + 0.2D1 * t1886 * t1138 - 0.3D1 * t1138 + t1883 + 
     #0.2D1 * t1045 * t760 - t798 * t1137 - 0.2D1 * t104 * t934 + t104 *
     # t14 * x2 - 0.2D1 * t1886 * t934 - 0.2D1 * t1886 * t1137 + t194 + 
     #0.2D1 * t1137 + t1888 - t1889 - t1909 + t760
      t1931 = 0.1D1 / (t1913 + t1929)
      t1932 = t761 * t1931
      t1933 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, t1903, -t1893,
     # -t1897, t1881, t1131)
      t1939 = log(0.4D1 * t194 * t843 * t844 * t1075)
      t1940 = t1939 * t761
      t1941 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t1903, -t1893,
     # -t1897, t1881, t1131)
      t1942 = t1931 * t1941
      t1948 = t8 * t761
      t1952 = -0.90D2 * t101 * t8 * (t1932 * t1933 - t1940 * t1942) + 0.
     #180D3 * t1013 * t1948 * t1942
      t1956 = FJET(XB1, XB2, s, t1881, -t1893, -t1897, t1903, t1131, -t1
     #952 * t157 * t218 / 0.720D3)
      t1959 = t157 * t217 * t159
      t1962 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, t1903, -t1893,
     # -t1897, t1881, t1131)
      t1964 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t1903, -t1893,
     # -t1897, t1881, t1131)
      t1965 = t1931 * t1964
      t1974 = -0.90D2 * t101 * t8 * (t1932 * t1962 - t1940 * t1965) + 0.
     #180D3 * t1013 * t1948 * t1965
      t1978 = FJET(XB1, XB2, s, t1903, -t1897, -t1893, t1881, t1131, -t1
     #974 * t157 * t218 / 0.720D3)
      t1982 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0
     #.0D0, 0.0D0, 0.0D0)
      t1983 = t936 * t1982
      t1984 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0
     #.0D0, 0.0D0, 0.0D0)
      t1990 = t961 * t1984
      t1997 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0
     #.0D0, 0.0D0, 0.0D0)
      t1998 = t936 * t1997
      t2011 = t240 * t1990
      t2040 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, s, t930, -t933, 0
     #.0D0, 0.0D0, 0.0D0)
      t2058 = -(-0.90D2 * t101 * t8 * (t1983 - t1007 * t1984) + 0.180D3 
     #* t1013 * t1990) * t157 * t218 / 0.720D3 + (-0.90D2 * t101 * t8 * 
     #(-t1998 + t1027 * t1982 - t1024 * t1984 / 0.2D1) + 0.180D3 * t140 
     #* t9 * (-t1983 + t1027 * t1984) - t2011) * t217 * t159 / 0.720D3 +
     # (-0.90D2 * t101 * t8 * (-t1998 - t944 * t1984 / 0.2D1 + t948 * t1
     #982) + 0.180D3 * t140 * t9 * (-t1983 + t948 * t1984) - t2011) * t1
     #57 * t217 / 0.1440D4 + (t150 * t9 * (-t1983 + t971 * t1984) - 0.90
     #D2 * t101 * t8 * (-t977 * t1982 / 0.2D1 + t983 * t1984 / 0.6D1 - t
     #936 * t2040 + t971 * t1997) - t424 * t1990 + 0.180D3 * t140 * t9 *
     # (-t977 * t1984 / 0.2D1 + t971 * t1982 - t1998)) * t217 / 0.1440D4
      t2059 = FJET(XB1, XB2, s, -t933, 0.0D0, t930, 0.0D0, 0.0D0, t2058)
      t2061 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, 
     #t758, 0.0D0, -t768)
      t2063 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, 
     #t758, 0.0D0, -t768)
      t2065 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, 
     #t758, 0.0D0, -t768)
      t2071 = t3 * t2061
      t2078 = t761 * t2063
      t2083 = t3 * t2063
      t2092 = t9 * (-t3 * t2065 - t761 * t2065 * t805)
      t2105 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t764, 
     #t758, 0.0D0, -t768)
      t2156 = -(-0.90D2 * t101 * t8 * ((-t761 * t2061 + t779 * t2063 - t
     #783 * t2065 / 0.2D1) * t805 + t813 * t2063 - t2071 - t816 * t2065 
     #/ 0.2D1) + 0.180D3 * t140 * t9 * ((-t2078 + t779 * t2065) * t805 +
     # t813 * t2065 - t2083) + t150 * t2092) * t157 * t159 / 0.1440D4 + 
     #(t59 * t9 * (-t847 * t2065 + t2063) - 0.90D2 * t4 * t9 * (t852 * t
     #2063 / 0.2D1 - t847 * t2061 + t2105 - t856 * t2065 / 0.6D1) + t38 
     #* t9 * t2065 + 0.180D3 * t66 * t9 * (t852 * t2065 / 0.2D1 - t847 *
     # t2063 + t2061)) * t159 / 0.1440D4 - (-0.90D2 * t101 * t8 * (-t208
     #3 + t889 * t2065 + (-t2078 + t882 * t2065) * t805) + 0.180D3 * t14
     #0 * t2092) * t157 * t218 / 0.720D3 + (-0.90D2 * t101 * t8 * (t908 
     #* t2065 / 0.2D1 + t2071 - t905 * t2063) + 0.180D3 * t140 * t9 * (t
     #2083 - t905 * t2065) + t240 * t241 * t2065) * t217 * t159 / 0.720D
     #3
      t2157 = FJET(XB1, XB2, s, -t764, 0.0D0, 0.0D0, t758, -t768, t2156)
      t2159 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t1129, t1128,
     # t758, 0.0D0, t1131)
      t2160 = t1140 * t2159
      t2161 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t1129, t1128,
     # t758, 0.0D0, t1131)
      t2167 = t1150 * t2161
      t2173 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, -t1129, t1128,
     # t758, 0.0D0, t1131)
      t2192 = -(-0.90D2 * t101 * t8 * (t2160 - t1141 * t2161) + 0.180D3 
     #* t1013 * t2167) * t157 * t218 / 0.720D3 + (-0.90D2 * t101 * t8 * 
     #(-t1140 * t2173 - t1164 * t2161 / 0.2D1 + t1167 * t2159) + 0.180D3
     # * t140 * t9 * (-t2160 + t1167 * t2161) - t240 * t2167) * t217 * t
     #159 / 0.720D3
      t2193 = FJET(XB1, XB2, s, -t1129, t758, t1128, 0.0D0, t1131, t2192
     #)
      t2195 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t1061, t1056,
     # 0.0D0, 0.0D0, 0.0D0)
      t2196 = t1069 * t2195
      t2197 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t1061, t1056,
     # 0.0D0, 0.0D0, 0.0D0)
      t2203 = t1087 * t2197
      t2211 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, -t1061, t1056,
     # 0.0D0, 0.0D0, 0.0D0)
      t2229 = -(-0.90D2 * t101 * t8 * (t2196 - t1080 * t2197) + 0.180D3 
     #* t1013 * t2203) * t157 * t218 / 0.720D3 + (-0.90D2 * t101 * t8 * 
     #(t1101 * t2195 - t1069 * t2211 - t1104 * t2197 / 0.2D1) + 0.180D3 
     #* t140 * t9 * (-t2196 + t1101 * t2197) - t240 * t2203) * t157 * t2
     #17 / 0.1440D4
      t2230 = FJET(XB1, XB2, s, -t1061, 0.0D0, t1056, 0.0D0, 0.0D0, t222
     #9)
      t2232 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, t1903, -t1893,
     # -t1897, t1881, t1131)
      t2234 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t1903, -t1893,
     # -t1897, t1881, t1131)
      t2235 = t1931 * t2234
      t2244 = -0.90D2 * t101 * t8 * (t1932 * t2232 - t1940 * t2235) + 0.
     #180D3 * t1013 * t1948 * t2235
      t2248 = FJET(XB1, XB2, s, -t1897, t1903, t1881, -t1893, t1131, -t2
     #244 * t157 * t218 / 0.720D3)
      t2252 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, t1903, -t1893,
     # -t1897, t1881, t1131)
      t2254 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t1903, -t1893,
     # -t1897, t1881, t1131)
      t2255 = t1931 * t2254
      t2264 = -0.90D2 * t101 * t8 * (t1932 * t2252 - t1940 * t2255) + 0.
     #180D3 * t1013 * t1948 * t2255
      t2268 = FJET(XB1, XB2, s, -t1893, t1881, t1903, -t1897, t1131, -t2
     #264 * t157 * t218 / 0.720D3)
      t2272 = t1726 * t1725 + t1805 * t1804 + t1842 * t1841 + t1878 * t1
     #877 - t1956 * t1952 * t1959 / 0.720D3 - t1978 * t1974 * t1959 / 0.
     #720D3 + t2059 * t2058 + t2157 * t2156 + t2193 * t2192 + t2230 * t2
     #229 - t2248 * t2244 * t1959 / 0.720D3 - t2268 * t2264 * t1959 / 0.
     #720D3
      rrgq2qght6s1e1 = t1691 + t2272

      end function



      doubleprecision function rrgq2qght6s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh63J7
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t21 = log(0.4D1 * t13 * t18)
      t22 = t21 ** 2
      t23 = 0.1D1 / z
      t25 = -0.1D1 + x3
      t26 = 0.1D1 / t25
      t27 = t18 * t26
      t30 = log(-0.4D1 * t13 * t27)
      t31 = t30 ** 2
      t32 = cos(t10)
      t34 = Sqrt(-x3 * t25)
      t39 = 0.1D1 / (-z - x3 + 0.2D1 * t32 * t34 * z)
      t42 = -t22 * t23 / 0.2D1 - t31 * t39 / 0.2D1
      t46 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t47 = t7 * t46
      t50 = pi * lh
      t51 = t3 * t7
      t52 = t51 * t8
      t58 = t21 * t23 + t30 * t39
      t60 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t61 = t7 * t60
      t67 = lh ** 2
      t69 = pi ** 2
      t71 = -0.180D3 * t67 + 0.30D2 * t69
      t72 = pi * t71
      t75 = -t23 - t39
      t78 = 0.1D1 / x3
      t81 = t15 * t12
      t82 = t81 * t17
      t84 = log(0.4D1 * t82)
      t85 = t84 * pi
      t86 = t23 * lh
      t89 = t84 ** 2
      t90 = t89 * pi
      t93 = pi * t23
      t96 = (-0.180D3 * t85 * t86 - 0.45D2 * t90 * t23 + t93 * t71) * t3
      t99 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t119 = (0.90D2 * t90 * t86 + t93 * (-0.60D2 * lh * t69 + 0.240D3 *
     # zeta3 + 0.120D3 * t67 * lh) + 0.15D2 * t89 * t84 * pi * t23 - t85
     # * t23 * t71) * t3
      t122 = t93 * lh
      t127 = (0.180D3 * t122 + 0.90D2 * t85 * t23) * t3
      t130 = x2 ** 2
      t131 = x3 * t130
      t132 = t131 * t12
      t135 = log(-0.4D1 * t132 * t27)
      t139 = t23 * t46
      t142 = log(0.4D1 * t131 * t82)
      t143 = t142 * t23
      t151 = -t23 * t8 - t8 * t39
      t157 = 0.1D1 / x2
      t161 = t130 * t12
      t164 = log(0.4D1 * t161 * t18)
      t165 = t164 ** 2
      t166 = t165 * t23
      t169 = t164 * t23
      t180 = t72 * t3
      t181 = t7 * t23
      t182 = t181 * t8
      t183 = t180 * t182
      t187 = x1 ** 2
      t188 = x3 * t187
      t191 = log(0.4D1 * t188 * t82)
      t192 = t191 * t23
      t194 = t188 * t12
      t197 = log(-0.4D1 * t194 * t27)
      t205 = -t151
      t211 = 0.1D1 / x1
      t214 = t4 * t7
      t216 = t157 * t211
      t220 = t130 * t187
      t223 = log(0.4D1 * t220 * t82)
      t224 = t223 * t23
      t230 = t50 * t3
      t237 = t187 * t12
      t240 = log(0.4D1 * t237 * t18)
      t242 = t240 ** 2
      t257 = (-0.90D2 * t4 * t9 * t42 + (-0.90D2 * t4 * t47 + 0.180D3 * 
     #t50 * t52) * t58 + (-0.90D2 * t4 * t61 + 0.180D3 * t50 * t51 * t46
     # + t72 * t52) * t75) * t78 / 0.2880D4 - t96 * t47 / 0.2880D4 + t93
     # * t51 * t99 / 0.32D2 - t119 * t9 / 0.2880D4 - t127 * t61 / 0.2880
     #D4 + (-0.90D2 * t4 * t7 * (-(t46 - t135 * t8) * t39 - t139 + t143 
     #* t8) + 0.180D3 * t50 * t51 * t151) * t78 * t157 / 0.1440D4 + (-0.
     #90D2 * t4 * t7 * (-t23 * t60 - t166 * t8 / 0.2D1 + t169 * t46) + 0
     #.180D3 * t50 * t51 * (t169 * t8 - t139) - t183) * t157 / 0.1440D4 
     #- (-0.90D2 * t4 * t7 * (t139 - t192 * t8 + (t46 - t197 * t8) * t39
     #) + 0.180D3 * t50 * t51 * t205) * t78 * t211 / 0.1440D4 + t214 * t
     #205 * t78 * t216 / 0.8D1 + (-0.90D2 * t4 * t7 * (-t139 + t224 * t8
     #) - 0.180D3 * t230 * t182) * t157 * t211 / 0.720D3 + (-0.90D2 * t9
     #3 * t51 * (t240 * t46 - t242 * t8 / 0.2D1 - t60) + 0.180D3 * t122 
     #* t51 * (-t46 + t240 * t8) - t183) * t211 / 0.1440D4
      t258 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t257)
      t260 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t261 = t7 * t260
      t265 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t266 = t7 * t265
      t269 = t51 * t260
      t274 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t275 = t7 * t274
      t289 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t300 = t23 * t265
      t308 = -t23 * t260 - t260 * t39
      t329 = t181 * t260
      t330 = t180 * t329
      t342 = -t308
      t380 = (-0.90D2 * t4 * t261 * t42 + (-0.90D2 * t4 * t266 + 0.180D3
     # * t50 * t269) * t58 + (-0.90D2 * t4 * t275 + 0.180D3 * t50 * t51 
     #* t265 + t72 * t269) * t75) * t78 / 0.2880D4 - t96 * t266 / 0.2880
     #D4 + t93 * t51 * t289 / 0.32D2 - t119 * t261 / 0.2880D4 - t127 * t
     #275 / 0.2880D4 + (-0.90D2 * t4 * t7 * (-(t265 - t135 * t260) * t39
     # - t300 + t143 * t260) + 0.180D3 * t50 * t51 * t308) * t78 * t157 
     #/ 0.1440D4 + (-0.90D2 * t4 * t7 * (-t23 * t274 - t166 * t260 / 0.2
     #D1 + t169 * t265) + 0.180D3 * t50 * t51 * (t169 * t260 - t300) - t
     #330) * t157 / 0.1440D4 - (-0.90D2 * t4 * t7 * (t300 - t192 * t260 
     #+ (t265 - t197 * t260) * t39) + 0.180D3 * t50 * t51 * t342) * t78 
     #* t211 / 0.1440D4 + t214 * t342 * t78 * t216 / 0.8D1 + (-0.90D2 * 
     #t4 * t7 * (t224 * t260 - t300) - 0.180D3 * t230 * t329) * t157 * t
     #211 / 0.720D3 + (-0.90D2 * t93 * t51 * (-t274 + t240 * t265 - t242
     # * t260 / 0.2D1) + 0.180D3 * t122 * t51 * (-t265 + t240 * t260) - 
     #t330) * t211 / 0.1440D4
      t381 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t380)
      t383 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t384 = t7 * t383
      t388 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t389 = t7 * t388
      t392 = t51 * t383
      t397 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t398 = t7 * t397
      t412 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t423 = t23 * t388
      t431 = -t23 * t383 - t383 * t39
      t452 = t181 * t383
      t453 = t180 * t452
      t465 = -t431
      t503 = (-0.90D2 * t4 * t384 * t42 + (-0.90D2 * t4 * t389 + 0.180D3
     # * t50 * t392) * t58 + (-0.90D2 * t4 * t398 + 0.180D3 * t50 * t51 
     #* t388 + t72 * t392) * t75) * t78 / 0.2880D4 - t96 * t389 / 0.2880
     #D4 + t93 * t51 * t412 / 0.32D2 - t119 * t384 / 0.2880D4 - t127 * t
     #398 / 0.2880D4 + (-0.90D2 * t4 * t7 * (-(t388 - t135 * t383) * t39
     # - t423 + t143 * t383) + 0.180D3 * t50 * t51 * t431) * t78 * t157 
     #/ 0.1440D4 + (-0.90D2 * t4 * t7 * (-t23 * t397 + t169 * t388 - t16
     #6 * t383 / 0.2D1) + 0.180D3 * t50 * t51 * (-t423 + t169 * t383) - 
     #t453) * t157 / 0.1440D4 - (-0.90D2 * t4 * t7 * (t423 - t192 * t383
     # + (t388 - t197 * t383) * t39) + 0.180D3 * t50 * t51 * t465) * t78
     # * t211 / 0.1440D4 + t214 * t465 * t78 * t216 / 0.8D1 + (-0.90D2 *
     # t4 * t7 * (-t423 + t224 * t383) - 0.180D3 * t230 * t452) * t157 *
     # t211 / 0.720D3 + (-0.90D2 * t93 * t51 * (-t242 * t383 / 0.2D1 + t
     #240 * t388 - t397) + 0.180D3 * t122 * t51 * (t240 * t383 - t388) -
     # t453) * t211 / 0.1440D4
      t504 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t503)
      t506 = t2 * x1
      t507 = -0.1D1 + x1
      t508 = x1 * z
      t509 = 0.1D1 - x1 + t508
      t510 = 0.1D1 / t509
      t512 = t2 * t507 * t510
      t513 = s * t16
      t515 = x1 * t507 * t510
      t516 = t513 * t515
      t517 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, t
     #506, 0.0D0, -t516)
      t520 = t17 * t510
      t521 = t507 ** 2
      t526 = log(-0.4D1 * t188 * t81 * t520 * t521 * t26)
      t527 = t526 * t509
      t528 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, t
     #506, 0.0D0, -t516)
      t531 = x3 * t509
      t533 = Sqrt(-t531 * t25)
      t537 = x3 * x1
      t538 = t537 * z
      t539 = 0.3D1 * t538
      t540 = x1 * t14
      t541 = x3 * t14
      t542 = t541 * x1
      t543 = 0.2D1 * t537
      t545 = 0.2D1 * t188 * z
      t546 = t188 * t14
      t547 = -z - x3 + t508 + 0.2D1 * t32 * t533 * z - t188 - t539 - t54
     #0 + t542 + t543 + t545 - t546
      t548 = 0.1D1 / t547
      t551 = t18 * t510 * t521
      t554 = log(0.4D1 * t194 * t551)
      t555 = t554 * t23
      t557 = t23 * t517
      t565 = -t23 * t528 - t509 * t528 * t548
      t577 = t220 * t12
      t580 = log(0.4D1 * t577 * t551)
      t581 = t580 * t23
      t587 = t181 * t528
      t598 = log(0.4D1 * t237 * t15 * t520 * t521)
      t599 = t598 ** 2
      t603 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, t
     #506, 0.0D0, -t516)
      t617 = -(-0.90D2 * t4 * t7 * ((-t509 * t517 + t527 * t528) * t548 
     #+ t555 * t528 - t557) + 0.180D3 * t50 * t51 * t565) * t78 * t211 /
     # 0.1440D4 + t214 * t565 * t78 * t216 / 0.8D1 + (-0.90D2 * t4 * t7 
     #* (-t581 * t528 + t557) + 0.180D3 * t230 * t587) * t157 * t211 / 0
     #.720D3 + (-0.90D2 * t93 * t51 * (t599 * t528 / 0.2D1 - t598 * t517
     # + t603) + 0.180D3 * t122 * t51 * (t517 - t598 * t528) + t180 * t5
     #87) * t211 / 0.1440D4
      t618 = FJET(XB1, XB2, s, 0.0D0, t506, -t512, 0.0D0, -t516, t617)
      t620 = x2 * s
      t621 = t620 * t1
      t622 = -0.1D1 + x2
      t623 = t622 * s
      t624 = t623 * t1
      t625 = x2 * z
      t627 = 0.1D1 / (-z + t625 - x2)
      t628 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, t621, -t624, 0.
     #0D0, 0.0D0, 0.0D0)
      t629 = t627 * t628
      t630 = t18 * t622
      t633 = log(-0.4D1 * t132 * t630)
      t634 = t633 * t627
      t635 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t621, -t624, 0.
     #0D0, 0.0D0, 0.0D0)
      t641 = t7 * t627
      t642 = t641 * t635
      t644 = 0.180D3 * t230 * t642
      t649 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, t621, -t624, 0.
     #0D0, 0.0D0, 0.0D0)
      t653 = log(-0.4D1 * t161 * t630)
      t654 = t653 * t627
      t656 = t653 ** 2
      t657 = t656 * t627
      t673 = t4 * t641
      t680 = log(-0.4D1 * t577 * t630)
      t681 = t680 * t627
      t691 = (-0.90D2 * t4 * t7 * (-t629 + t634 * t635) - t644) * t78 * 
     #t157 / 0.1440D4 + (-0.90D2 * t4 * t7 * (-t627 * t649 + t654 * t628
     # - t657 * t635 / 0.2D1) + 0.180D3 * t50 * t51 * (-t629 + t654 * t6
     #35) - t180 * t642) * t157 / 0.1440D4 + t673 * t635 * t78 * t216 / 
     #0.8D1 + (-0.90D2 * t4 * t7 * (t681 * t635 - t629) - t644) * t157 *
     # t211 / 0.720D3
      t692 = FJET(XB1, XB2, s, 0.0D0, t621, 0.0D0, -t624, 0.0D0, t691)
      t694 = x2 * x3
      t697 = Sqrt(x3 * t622 * t25)
      t698 = t32 * t697
      t700 = 0.2D1 * t698 * x2
      t702 = 0.1D1 - x3 + t694
      t703 = 0.1D1 / t702
      t705 = t2 * (0.1D1 - x3 - x2 + t694 + t131 + t700) * t703
      t710 = t2 * x2 * (-0.1D1 + t694 + 0.2D1 * t698) * t703
      t711 = t694 * z
      t712 = t131 * z
      t718 = 0.1D1 / (z - t625 - t711 + t712 + x3 - t131 + x2 - t700 - 0
     #.2D1 * t698 * z + 0.2D1 * t698 * t625)
      t719 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, -t710, t705, 0.
     #0D0, 0.0D0, 0.0D0)
      t723 = t702 ** 2
      t729 = log(0.4D1 * t131 * t81 * t17 * t622 * t25 / t723)
      t730 = t729 * t718
      t731 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, -t710, t705, 0.
     #0D0, 0.0D0, 0.0D0)
      t737 = t7 * t718
      t745 = t4 * t737
      t750 = (-0.90D2 * t4 * t7 * (-t718 * t719 + t730 * t731) - 0.180D3
     # * t230 * t737 * t731) * t78 * t157 / 0.1440D4 + t745 * t731 * t78
     # * t216 / 0.8D1
      t751 = FJET(XB1, XB2, s, 0.0D0, t705, 0.0D0, -t710, 0.0D0, t750)
      t753 = t1 * t507
      t755 = t623 * t753 * t510
      t756 = t620 * t753
      t758 = t513 * t622 * t515
      t759 = x2 * x1
      t760 = t759 * z
      t762 = 0.1D1 / (-t759 + z - t625 + x2 + t760)
      t763 = t7 * t762
      t764 = t4 * t763
      t765 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, -t756, t755, t5
     #06, 0.0D0, t758)
      t775 = log(-0.4D1 * t220 * t81 * t520 * t521 * t622)
      t776 = t775 * t762
      t778 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, -t756, t755, t5
     #06, 0.0D0, t758)
      t791 = t764 * t765 * t78 * t216 / 0.8D1 + (-0.90D2 * t4 * t7 * (t7
     #76 * t765 - t762 * t778) - 0.180D3 * t230 * t763 * t765) * t157 * 
     #t211 / 0.720D3
      t792 = FJET(XB1, XB2, s, 0.0D0, t755, t506, -t756, t758, t791)
      t794 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, t621, -t624, 0.
     #0D0, 0.0D0, 0.0D0)
      t795 = t627 * t794
      t796 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t621, -t624, 0.
     #0D0, 0.0D0, 0.0D0)
      t802 = t641 * t796
      t804 = 0.180D3 * t230 * t802
      t812 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, t621, -t624, 0.
     #0D0, 0.0D0, 0.0D0)
      t840 = (-0.90D2 * t4 * t7 * (-t795 + t634 * t796) - t804) * t78 * 
     #t157 / 0.1440D4 + (-0.90D2 * t4 * t7 * (-t657 * t796 / 0.2D1 + t65
     #4 * t794 - t627 * t812) + 0.180D3 * t50 * t51 * (-t795 + t654 * t7
     #96) - t180 * t802) * t157 / 0.1440D4 + t673 * t796 * t78 * t216 / 
     #0.8D1 + (-0.90D2 * t4 * t7 * (t681 * t796 - t795) - t804) * t157 *
     # t211 / 0.720D3
      t841 = FJET(XB1, XB2, s, 0.0D0, -t624, 0.0D0, t621, 0.0D0, t840)
      t843 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, t
     #506, 0.0D0, -t516)
      t845 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, t
     #506, 0.0D0, -t516)
      t850 = t23 * t843
      t858 = -t23 * t845 - t509 * t845 * t548
      t875 = t181 * t845
      t882 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, t
     #506, 0.0D0, -t516)
      t899 = -(-0.90D2 * t4 * t7 * ((-t509 * t843 + t527 * t845) * t548 
     #+ t555 * t845 - t850) + 0.180D3 * t50 * t51 * t858) * t78 * t211 /
     # 0.1440D4 + t214 * t858 * t78 * t216 / 0.8D1 + (-0.90D2 * t4 * t7 
     #* (-t581 * t845 + t850) + 0.180D3 * t230 * t875) * t157 * t211 / 0
     #.720D3 + (-0.90D2 * t93 * t51 * (t882 - t598 * t843 + t599 * t845 
     #/ 0.2D1) + 0.180D3 * t122 * t51 * (t843 - t598 * t845) + t180 * t8
     #75) * t211 / 0.1440D4
      t900 = FJET(XB1, XB2, s, 0.0D0, -t512, t506, 0.0D0, -t516, t899)
      t902 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, -t710, t705, 0.
     #0D0, 0.0D0, 0.0D0)
      t904 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, -t710, t705, 0.
     #0D0, 0.0D0, 0.0D0)
      t921 = (-0.90D2 * t4 * t7 * (-t718 * t902 + t730 * t904) - 0.180D3
     # * t230 * t737 * t904) * t78 * t157 / 0.1440D4 + t745 * t904 * t78
     # * t216 / 0.8D1
      t922 = FJET(XB1, XB2, s, 0.0D0, -t710, 0.0D0, t705, 0.0D0, t921)
      t924 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t925 = t7 * t924
      t929 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t930 = t7 * t929
      t933 = t51 * t924
      t938 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t939 = t7 * t938
      t951 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t959 = t23 * t929
      t970 = -t23 * t924 - t924 * t39
      t991 = t181 * t924
      t992 = t180 * t991
      t1006 = -t970
      t1044 = (-0.90D2 * t4 * t925 * t42 + (-0.90D2 * t4 * t930 + 0.180D
     #3 * t50 * t933) * t58 + (-0.90D2 * t4 * t939 + 0.180D3 * t50 * t51
     # * t929 + t72 * t933) * t75) * t78 / 0.2880D4 + t93 * t51 * t951 /
     # 0.32D2 - t127 * t939 / 0.2880D4 - t96 * t930 / 0.2880D4 + (-0.90D
     #2 * t4 * t7 * (-t959 + t143 * t924 - (t929 - t135 * t924) * t39) +
     # 0.180D3 * t50 * t51 * t970) * t78 * t157 / 0.1440D4 + (-0.90D2 * 
     #t4 * t7 * (-t166 * t924 / 0.2D1 - t23 * t938 + t169 * t929) + 0.18
     #0D3 * t50 * t51 * (-t959 + t169 * t924) - t992) * t157 / 0.1440D4 
     #- t119 * t925 / 0.2880D4 - (-0.90D2 * t4 * t7 * (t959 - t192 * t92
     #4 + (t929 - t197 * t924) * t39) + 0.180D3 * t50 * t51 * t1006) * t
     #78 * t211 / 0.1440D4 + t214 * t1006 * t78 * t216 / 0.8D1 + (-0.90D
     #2 * t4 * t7 * (t224 * t924 - t959) - 0.180D3 * t230 * t991) * t157
     # * t211 / 0.720D3 + (-0.90D2 * t93 * t51 * (-t938 - t242 * t924 / 
     #0.2D1 + t240 * t929) + 0.180D3 * t122 * t51 * (-t929 + t240 * t924
     #) - t992) * t211 / 0.1440D4
      t1045 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1044)
      t1047 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, 
     #t506, 0.0D0, -t516)
      t1049 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, 
     #t506, 0.0D0, -t516)
      t1054 = t23 * t1047
      t1062 = -t23 * t1049 - t509 * t1049 * t548
      t1079 = t181 * t1049
      t1089 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, 
     #t506, 0.0D0, -t516)
      t1103 = -(-0.90D2 * t4 * t7 * ((-t509 * t1047 + t527 * t1049) * t5
     #48 + t555 * t1049 - t1054) + 0.180D3 * t50 * t51 * t1062) * t78 * 
     #t211 / 0.1440D4 + t214 * t1062 * t78 * t216 / 0.8D1 + (-0.90D2 * t
     #4 * t7 * (t1054 - t581 * t1049) + 0.180D3 * t230 * t1079) * t157 *
     # t211 / 0.720D3 + (-0.90D2 * t93 * t51 * (t599 * t1049 / 0.2D1 - t
     #598 * t1047 + t1089) + 0.180D3 * t122 * t51 * (-t598 * t1049 + t10
     #47) + t180 * t1079) * t211 / 0.1440D4
      t1104 = FJET(XB1, XB2, s, t506, 0.0D0, 0.0D0, -t512, -t516, t1103)
      t1106 = t258 * t257 + t381 * t380 + t504 * t503 + t618 * t617 + t6
     #92 * t691 + t751 * t750 + t792 * t791 + t841 * t840 + t900 * t899 
     #+ t922 * t921 + t1045 * t1044 + t1104 * t1103
      t1107 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, -t756, t755, t
     #506, 0.0D0, t758)
      t1113 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, -t756, t755, t
     #506, 0.0D0, t758)
      t1126 = t764 * t1107 * t78 * t216 / 0.8D1 + (-0.90D2 * t4 * t7 * (
     #t776 * t1107 - t762 * t1113) - 0.180D3 * t230 * t763 * t1107) * t1
     #57 * t211 / 0.720D3
      t1127 = FJET(XB1, XB2, s, t506, -t756, 0.0D0, t755, t758, t1126)
      t1129 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, t621, -t624, 0
     #.0D0, 0.0D0, 0.0D0)
      t1130 = t627 * t1129
      t1131 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t621, -t624, 0
     #.0D0, 0.0D0, 0.0D0)
      t1137 = t641 * t1131
      t1139 = 0.180D3 * t230 * t1137
      t1147 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, t621, -t624, 0
     #.0D0, 0.0D0, 0.0D0)
      t1175 = (-0.90D2 * t4 * t7 * (-t1130 + t634 * t1131) - t1139) * t7
     #8 * t157 / 0.1440D4 + (-0.90D2 * t4 * t7 * (-t657 * t1131 / 0.2D1 
     #+ t654 * t1129 - t627 * t1147) + 0.180D3 * t50 * t51 * (-t1130 + t
     #654 * t1131) - t180 * t1137) * t157 / 0.1440D4 + t673 * t1131 * t7
     #8 * t216 / 0.8D1 + (-0.90D2 * t4 * t7 * (t681 * t1131 - t1130) - t
     #1139) * t157 * t211 / 0.720D3
      t1176 = FJET(XB1, XB2, s, t621, 0.0D0, -t624, 0.0D0, 0.0D0, t1175)
      t1178 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, -t710, t705, 0
     #.0D0, 0.0D0, 0.0D0)
      t1180 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, -t710, t705, 0
     #.0D0, 0.0D0, 0.0D0)
      t1197 = (-0.90D2 * t4 * t7 * (-t718 * t1178 + t730 * t1180) - 0.18
     #0D3 * t230 * t737 * t1180) * t78 * t157 / 0.1440D4 + t745 * t1180 
     #* t78 * t216 / 0.8D1
      t1198 = FJET(XB1, XB2, s, t705, 0.0D0, -t710, 0.0D0, 0.0D0, t1197)
      t1200 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, -t756, t755, t
     #506, 0.0D0, t758)
      t1205 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, -t756, t755, t
     #506, 0.0D0, t758)
      t1219 = t764 * t1200 * t78 * t216 / 0.8D1 + (-0.90D2 * t4 * t7 * (
     #-t762 * t1205 + t776 * t1200) - 0.180D3 * t230 * t763 * t1200) * t
     #157 * t211 / 0.720D3
      t1220 = FJET(XB1, XB2, s, t755, 0.0D0, -t756, t506, t758, t1219)
      t1223 = t506 * t694 * t703
      t1224 = t2 * t507
      t1225 = t131 * t508
      t1228 = Sqrt(t531 * t622 * t25)
      t1229 = t32 * t1228
      t1231 = 0.2D1 * t1229 * x2
      t1232 = t131 * x1
      t1236 = t1224 * (t1225 + t1231 + 0.1D1 - x3 - x2 + t694 + t131 - t
     #1232) * t510 * t703
      t1240 = t25 * s * t1 * x1 * t703
      t1246 = t1224 * x2 * (-0.1D1 + t694 + x1 - t537 - t508 + t538 + 0.
     #2D1 * t1229) * t510 * t703
      t1248 = t4 * t7 * t509
      t1256 = x2 * t187
      t1260 = t1225 + 0.2D1 * t694 * t508 - t541 * t759 - 0.2D1 * t188 *
     # t625 + t711 - t712 - 0.3D1 * t760 + 0.2D1 * t759 - t539 + t542 + 
     #t545 - t546 - t1256 + t1231 - t1232 + 0.2D1 * t1229 * z - t694 * x
     #1
      t1274 = t188 * x2 + t759 * t14 + 0.2D1 * t1256 * z - t1256 * t14 +
     # t188 * t14 * x2 - 0.2D1 * t1229 * t625 - 0.2D1 * t1229 * t759 + t
     #625 + t131 - x3 - x2 + 0.2D1 * t1229 * t760 - z + t508 - t188 - t5
     #40 + t543
      t1276 = 0.1D1 / (t1260 + t1274)
      t1277 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t1246, -t1236,
     # -t1240, t1223, t758)
      t1280 = t78 * t157 * t211
      t1281 = t1276 * t1277 * t1280
      t1284 = FJET(XB1, XB2, s, t1223, -t1236, -t1240, t1246, t758, t124
     #8 * t1281 / 0.8D1)
      t1286 = t51 * t509
      t1290 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t1246, -t1236,
     # -t1240, t1223, t758)
      t1292 = t1276 * t1290 * t1280
      t1295 = FJET(XB1, XB2, s, t1246, -t1240, -t1236, t1223, t758, t124
     #8 * t1292 / 0.8D1)
      t1300 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, t621, -t624, 0
     #.0D0, 0.0D0, 0.0D0)
      t1301 = t627 * t1300
      t1302 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t621, -t624, 0
     #.0D0, 0.0D0, 0.0D0)
      t1308 = t641 * t1302
      t1310 = 0.180D3 * t230 * t1308
      t1318 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, t621, -t624, 0
     #.0D0, 0.0D0, 0.0D0)
      t1346 = (-0.90D2 * t4 * t7 * (-t1301 + t634 * t1302) - t1310) * t7
     #8 * t157 / 0.1440D4 + (-0.90D2 * t4 * t7 * (-t657 * t1302 / 0.2D1 
     #+ t654 * t1300 - t627 * t1318) + 0.180D3 * t50 * t51 * (-t1301 + t
     #654 * t1302) - t180 * t1308) * t157 / 0.1440D4 + t673 * t1302 * t7
     #8 * t216 / 0.8D1 + (-0.90D2 * t4 * t7 * (-t1301 + t681 * t1302) - 
     #t1310) * t157 * t211 / 0.720D3
      t1347 = FJET(XB1, XB2, s, -t624, 0.0D0, t621, 0.0D0, 0.0D0, t1346)
      t1349 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, 
     #t506, 0.0D0, -t516)
      t1351 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, 
     #t506, 0.0D0, -t516)
      t1356 = t23 * t1349
      t1364 = -t23 * t1351 - t509 * t1351 * t548
      t1381 = t181 * t1351
      t1391 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t512, 
     #t506, 0.0D0, -t516)
      t1405 = -(-0.90D2 * t4 * t7 * ((-t509 * t1349 + t527 * t1351) * t5
     #48 + t555 * t1351 - t1356) + 0.180D3 * t50 * t51 * t1364) * t78 * 
     #t211 / 0.1440D4 + t214 * t1364 * t78 * t216 / 0.8D1 + (-0.90D2 * t
     #4 * t7 * (t1356 - t581 * t1351) + 0.180D3 * t230 * t1381) * t157 *
     # t211 / 0.720D3 + (-0.90D2 * t93 * t51 * (t599 * t1351 / 0.2D1 - t
     #598 * t1349 + t1391) + 0.180D3 * t122 * t51 * (-t598 * t1351 + t13
     #49) + t180 * t1381) * t211 / 0.1440D4
      t1406 = FJET(XB1, XB2, s, -t512, 0.0D0, 0.0D0, t506, -t516, t1405)
      t1408 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t756, t755, t
     #506, 0.0D0, t758)
      t1413 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t756, t755, t
     #506, 0.0D0, t758)
      t1427 = t764 * t1408 * t78 * t216 / 0.8D1 + (-0.90D2 * t4 * t7 * (
     #-t762 * t1413 + t776 * t1408) - 0.180D3 * t230 * t763 * t1408) * t
     #157 * t211 / 0.720D3
      t1428 = FJET(XB1, XB2, s, -t756, t506, t755, 0.0D0, t758, t1427)
      t1430 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, -t710, t705, 0
     #.0D0, 0.0D0, 0.0D0)
      t1432 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t710, t705, 0
     #.0D0, 0.0D0, 0.0D0)
      t1449 = (-0.90D2 * t4 * t7 * (-t718 * t1430 + t730 * t1432) - 0.18
     #0D3 * t230 * t737 * t1432) * t78 * t157 / 0.1440D4 + t745 * t1432 
     #* t78 * t216 / 0.8D1
      t1450 = FJET(XB1, XB2, s, -t710, 0.0D0, t705, 0.0D0, 0.0D0, t1449)
      t1452 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t1246, -t1236,
     # -t1240, t1223, t758)
      t1454 = t1276 * t1452 * t1280
      t1457 = FJET(XB1, XB2, s, -t1240, t1246, t1223, -t1236, t758, t124
     #8 * t1454 / 0.8D1)
      t1462 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t1246, -t1236,
     # -t1240, t1223, t758)
      t1464 = t1276 * t1462 * t1280
      t1467 = FJET(XB1, XB2, s, -t1236, t1223, t1246, -t1240, t758, t124
     #8 * t1464 / 0.8D1)
      t1472 = t1127 * t1126 + t1176 * t1175 + t1198 * t1197 + t1220 * t1
     #219 + t1284 * pi * t1286 * t1281 / 0.8D1 + t1295 * pi * t1286 * t1
     #292 / 0.8D1 + t1347 * t1346 + t1406 * t1405 + t1428 * t1427 + t145
     #0 * t1449 + t1457 * pi * t1286 * t1454 / 0.8D1 + t1467 * pi * t128
     #6 * t1464 / 0.8D1
      rrgq2qght6s1e0 = t1106 + t1472

      end function



      doubleprecision function rrgq2qght6s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh63J7
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t21 = log(0.4D1 * t13 * t18)
      t22 = 0.1D1 / z
      t24 = -0.1D1 + x3
      t29 = log(-0.4D1 * t13 * t18 / t24)
      t30 = cos(t10)
      t32 = Sqrt(-x3 * t24)
      t37 = 0.1D1 / (-z - x3 + 0.2D1 * t30 * t32 * z)
      t39 = t21 * t22 + t29 * t37
      t43 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t44 = t7 * t43
      t47 = pi * lh
      t48 = t3 * t7
      t53 = -t22 - t37
      t56 = 0.1D1 / x3
      t59 = pi * t22
      t60 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t69 = log(0.4D1 * t15 * t12 * t17)
      t70 = t69 * pi
      t74 = (0.180D3 * t59 * lh + 0.90D2 * t70 * t22) * t3
      t80 = t69 ** 2
      t84 = lh ** 2
      t86 = pi ** 2
      t91 = (-0.180D3 * t70 * t22 * lh - 0.45D2 * t80 * pi * t22 + t59 *
     # (-0.180D3 * t84 + 0.30D2 * t86)) * t3
      t94 = t4 * t7
      t95 = t22 * t8
      t97 = -t95 - t8 * t37
      t99 = 0.1D1 / x2
      t103 = x2 ** 2
      t104 = t103 * t12
      t107 = log(0.4D1 * t104 * t18)
      t108 = t107 * t22
      t115 = t47 * t3
      t116 = t7 * t22
      t119 = 0.180D3 * t115 * t116 * t8
      t123 = 0.1D1 / x1
      t124 = t99 * t123
      t128 = x1 ** 2
      t129 = t128 * t12
      t132 = log(0.4D1 * t129 * t18)
      t146 = (-0.90D2 * t4 * t9 * t39 + (-0.90D2 * t4 * t44 + 0.180D3 * 
     #t47 * t48 * t8) * t53) * t56 / 0.2880D4 + t59 * t48 * t60 / 0.32D2
     # - t74 * t44 / 0.2880D4 - t91 * t9 / 0.2880D4 - t94 * t97 * t56 * 
     #t99 / 0.16D2 + (-0.90D2 * t4 * t7 * (t108 * t8 - t22 * t43) - t119
     #) * t99 / 0.1440D4 + t94 * t95 * t124 / 0.8D1 + (-0.90D2 * t59 * t
     #48 * (-t43 + t132 * t8) - t119) * t123 / 0.1440D4 - t94 * t97 * t5
     #6 * t123 / 0.16D2
      t147 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t146)
      t149 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t150 = t7 * t149
      t154 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t155 = t7 * t154
      t166 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t174 = t22 * t149
      t176 = -t174 - t149 * t37
      t189 = 0.180D3 * t115 * t116 * t149
      t209 = (-0.90D2 * t4 * t150 * t39 + (-0.90D2 * t4 * t155 + 0.180D3
     # * t47 * t48 * t149) * t53) * t56 / 0.2880D4 + t59 * t48 * t166 / 
     #0.32D2 - t74 * t155 / 0.2880D4 - t91 * t150 / 0.2880D4 - t94 * t17
     #6 * t56 * t99 / 0.16D2 + (-0.90D2 * t4 * t7 * (t108 * t149 - t22 *
     # t154) - t189) * t99 / 0.1440D4 + t94 * t174 * t124 / 0.8D1 + (-0.
     #90D2 * t59 * t48 * (-t154 + t132 * t149) - t189) * t123 / 0.1440D4
     # - t94 * t176 * t56 * t123 / 0.16D2
      t210 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t209)
      t212 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t213 = t7 * t212
      t217 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t218 = t7 * t217
      t229 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t237 = t22 * t212
      t239 = -t237 - t212 * t37
      t252 = 0.180D3 * t115 * t116 * t212
      t272 = (-0.90D2 * t4 * t213 * t39 + (-0.90D2 * t4 * t218 + 0.180D3
     # * t47 * t48 * t212) * t53) * t56 / 0.2880D4 + t59 * t48 * t229 / 
     #0.32D2 - t74 * t218 / 0.2880D4 - t91 * t213 / 0.2880D4 - t94 * t23
     #9 * t56 * t99 / 0.16D2 + (-0.90D2 * t4 * t7 * (-t22 * t217 + t108 
     #* t212) - t252) * t99 / 0.1440D4 + t94 * t237 * t124 / 0.8D1 + (-0
     #.90D2 * t59 * t48 * (t132 * t212 - t217) - t252) * t123 / 0.1440D4
     # - t94 * t239 * t56 * t123 / 0.16D2
      t273 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t272)
      t275 = t2 * x1
      t276 = -0.1D1 + x1
      t277 = x1 * z
      t278 = 0.1D1 - x1 + t277
      t279 = 0.1D1 / t278
      t281 = t2 * t276 * t279
      t282 = s * t16
      t284 = x1 * t276 * t279
      t285 = t282 * t284
      t286 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t281, t
     #275, 0.0D0, -t285)
      t287 = t22 * t286
      t291 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t281, t
     #275, 0.0D0, -t285)
      t294 = t276 ** 2
      t298 = log(0.4D1 * t129 * t15 * t17 * t279 * t294)
      t313 = Sqrt(-x3 * t278 * t24)
      t317 = x3 * t128
      t318 = x3 * x1
      t328 = -z - x3 + t277 + 0.2D1 * t30 * t313 * z - t317 - 0.3D1 * t3
     #18 * z - x1 * t14 + x3 * t14 * x1 + 0.2D1 * t318 + 0.2D1 * t317 * 
     #z - t317 * t14
      t329 = 0.1D1 / t328
      t336 = -t94 * t287 * t124 / 0.8D1 + (-0.90D2 * t59 * t48 * (t291 -
     # t298 * t286) + 0.180D3 * t115 * t116 * t286) * t123 / 0.1440D4 + 
     #t94 * (-t287 - t278 * t286 * t329) * t56 * t123 / 0.16D2
      t337 = FJET(XB1, XB2, s, 0.0D0, t275, -t281, 0.0D0, -t285, t336)
      t339 = x2 * s
      t340 = t339 * t1
      t341 = -0.1D1 + x2
      t342 = t341 * s
      t343 = t342 * t1
      t344 = x2 * z
      t346 = 0.1D1 / (-z + t344 - x2)
      t347 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t340, -t343, 0.
     #0D0, 0.0D0, 0.0D0)
      t348 = t346 * t347
      t349 = t56 * t99
      t353 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, t340, -t343, 0.
     #0D0, 0.0D0, 0.0D0)
      t358 = log(-0.4D1 * t104 * t18 * t341)
      t359 = t358 * t346
      t365 = t7 * t346
      t375 = t94 * t348 * t349 / 0.16D2 + (-0.90D2 * t4 * t7 * (-t346 * 
     #t353 + t359 * t347) - 0.180D3 * t115 * t365 * t347) * t99 / 0.1440
     #D4 + t94 * t348 * t124 / 0.8D1
      t376 = FJET(XB1, XB2, s, 0.0D0, t340, 0.0D0, -t343, 0.0D0, t375)
      t378 = x2 * x3
      t379 = t103 * x3
      t382 = Sqrt(x3 * t341 * t24)
      t383 = t30 * t382
      t385 = 0.2D1 * t383 * x2
      t388 = 0.1D1 / (0.1D1 - x3 + t378)
      t390 = t2 * (0.1D1 - x3 - x2 + t378 + t379 + t385) * t388
      t395 = t2 * x2 * (-0.1D1 + t378 + 0.2D1 * t383) * t388
      t403 = 0.1D1 / (z - t344 - t378 * z + t379 * z + x3 - t379 + x2 - 
     #t385 - 0.2D1 * t383 * z + 0.2D1 * t383 * t344)
      t404 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, -t395, t390, 0.
     #0D0, 0.0D0, 0.0D0)
      t406 = t403 * t404 * t349
      t409 = FJET(XB1, XB2, s, 0.0D0, t390, 0.0D0, -t395, 0.0D0, t94 * t
     #406 / 0.16D2)
      t414 = t1 * t276
      t416 = t342 * t414 * t279
      t417 = t339 * t414
      t419 = t282 * t341 * t284
      t420 = x2 * x1
      t423 = 0.1D1 / (-t420 + z - t344 + x2 + t420 * z)
      t424 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, -t417, t416, t2
     #75, 0.0D0, t419)
      t426 = t423 * t424 * t124
      t429 = FJET(XB1, XB2, s, 0.0D0, t416, t275, -t417, t419, t94 * t42
     #6 / 0.8D1)
      t434 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t340, -t343, 0.
     #0D0, 0.0D0, 0.0D0)
      t435 = t346 * t434
      t439 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, t340, -t343, 0.
     #0D0, 0.0D0, 0.0D0)
      t455 = t94 * t435 * t349 / 0.16D2 + (-0.90D2 * t4 * t7 * (-t346 * 
     #t439 + t359 * t434) - 0.180D3 * t115 * t365 * t434) * t99 / 0.1440
     #D4 + t94 * t435 * t124 / 0.8D1
      t456 = FJET(XB1, XB2, s, 0.0D0, -t343, 0.0D0, t340, 0.0D0, t455)
      t458 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t281, t
     #275, 0.0D0, -t285)
      t459 = t22 * t458
      t463 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t281, t
     #275, 0.0D0, -t285)
      t482 = -t94 * t459 * t124 / 0.8D1 + (-0.90D2 * t59 * t48 * (t463 -
     # t298 * t458) + 0.180D3 * t115 * t116 * t458) * t123 / 0.1440D4 + 
     #t94 * (-t459 - t278 * t458 * t329) * t56 * t123 / 0.16D2
      t483 = FJET(XB1, XB2, s, 0.0D0, -t281, t275, 0.0D0, -t285, t482)
      t485 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, -t395, t390, 0.
     #0D0, 0.0D0, 0.0D0)
      t487 = t403 * t485 * t349
      t490 = FJET(XB1, XB2, s, 0.0D0, -t395, 0.0D0, t390, 0.0D0, t94 * t
     #487 / 0.16D2)
      t495 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t499 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t500 = t7 * t499
      t506 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t507 = t7 * t506
      t518 = t22 * t499
      t520 = -t518 - t499 * t37
      t533 = 0.180D3 * t115 * t116 * t499
      t555 = t59 * t48 * t495 / 0.32D2 - t91 * t500 / 0.2880D4 + (-0.90D
     #2 * t4 * t500 * t39 + (-0.90D2 * t4 * t507 + 0.180D3 * t47 * t48 *
     # t499) * t53) * t56 / 0.2880D4 - t94 * t520 * t56 * t99 / 0.16D2 +
     # (-0.90D2 * t4 * t7 * (-t22 * t506 + t108 * t499) - t533) * t99 / 
     #0.1440D4 - t74 * t507 / 0.2880D4 + t94 * t518 * t124 / 0.8D1 + (-0
     #.90D2 * t59 * t48 * (-t506 + t132 * t499) - t533) * t123 / 0.1440D
     #4 - t94 * t520 * t56 * t123 / 0.16D2
      t556 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t555)
      t558 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t281, t
     #275, 0.0D0, -t285)
      t559 = t22 * t558
      t564 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t281, t
     #275, 0.0D0, -t285)
      t582 = -t94 * t559 * t124 / 0.8D1 + (-0.90D2 * t59 * t48 * (-t298 
     #* t558 + t564) + 0.180D3 * t115 * t116 * t558) * t123 / 0.1440D4 +
     # t94 * (-t559 - t278 * t558 * t329) * t56 * t123 / 0.16D2
      t583 = FJET(XB1, XB2, s, t275, 0.0D0, 0.0D0, -t281, -t285, t582)
      t585 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, -t417, t416, t2
     #75, 0.0D0, t419)
      t587 = t423 * t585 * t124
      t590 = FJET(XB1, XB2, s, t275, -t417, 0.0D0, t416, t419, t94 * t58
     #7 / 0.8D1)
      t595 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t340, -t343, 0.
     #0D0, 0.0D0, 0.0D0)
      t596 = t346 * t595
      t600 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, t340, -t343, 0.
     #0D0, 0.0D0, 0.0D0)
      t616 = t94 * t596 * t349 / 0.16D2 + (-0.90D2 * t4 * t7 * (-t346 * 
     #t600 + t359 * t595) - 0.180D3 * t115 * t365 * t595) * t99 / 0.1440
     #D4 + t94 * t596 * t124 / 0.8D1
      t617 = FJET(XB1, XB2, s, t340, 0.0D0, -t343, 0.0D0, 0.0D0, t616)
      t619 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, -t395, t390, 0.
     #0D0, 0.0D0, 0.0D0)
      t621 = t403 * t619 * t349
      t624 = FJET(XB1, XB2, s, t390, 0.0D0, -t395, 0.0D0, 0.0D0, t94 * t
     #621 / 0.16D2)
      t629 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, -t417, t416, t2
     #75, 0.0D0, t419)
      t631 = t423 * t629 * t124
      t634 = FJET(XB1, XB2, s, t416, 0.0D0, -t417, t275, t419, t94 * t63
     #1 / 0.8D1)
      t639 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t340, -t343, 0.
     #0D0, 0.0D0, 0.0D0)
      t640 = t346 * t639
      t644 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, t340, -t343, 0.
     #0D0, 0.0D0, 0.0D0)
      t660 = t94 * t640 * t349 / 0.16D2 + (-0.90D2 * t4 * t7 * (-t346 * 
     #t644 + t359 * t639) - 0.180D3 * t115 * t365 * t639) * t99 / 0.1440
     #D4 + t94 * t640 * t124 / 0.8D1
      t661 = FJET(XB1, XB2, s, -t343, 0.0D0, t340, 0.0D0, 0.0D0, t660)
      t663 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t281, t
     #275, 0.0D0, -t285)
      t664 = t22 * t663
      t669 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t281, t
     #275, 0.0D0, -t285)
      t687 = -t94 * t664 * t124 / 0.8D1 + (-0.90D2 * t59 * t48 * (-t298 
     #* t663 + t669) + 0.180D3 * t115 * t116 * t663) * t123 / 0.1440D4 +
     # t94 * (-t664 - t278 * t663 * t329) * t56 * t123 / 0.16D2
      t688 = FJET(XB1, XB2, s, -t281, 0.0D0, 0.0D0, t275, -t285, t687)
      t690 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t395, t390, 0.
     #0D0, 0.0D0, 0.0D0)
      t692 = t403 * t690 * t349
      t695 = FJET(XB1, XB2, s, -t395, 0.0D0, t390, 0.0D0, 0.0D0, t94 * t
     #692 / 0.16D2)
      t700 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, -t417, t416, t2
     #75, 0.0D0, t419)
      t702 = t423 * t700 * t124
      t705 = FJET(XB1, XB2, s, -t417, t275, t416, 0.0D0, t419, t94 * t70
     #2 / 0.8D1)
      rrgq2qght6s1em1 = t147 * t146 + t210 * t209 + t273 * t272 + t337 *
     # t336 + t376 * t375 + t409 * pi * t48 * t406 / 0.16D2 + t429 * pi 
     #* t48 * t426 / 0.8D1 + t456 * t455 + t483 * t482 + t490 * pi * t48
     # * t487 / 0.16D2 + t556 * t555 + t583 * t582 + t590 * pi * t48 * t
     #587 / 0.8D1 + t617 * t616 + t624 * pi * t48 * t621 / 0.16D2 + t634
     # * pi * t48 * t631 / 0.8D1 + t661 * t660 + t688 * t687 + t695 * pi
     # * t48 * t692 / 0.16D2 + t705 * pi * t48 * t702 / 0.8D1

      end function



      doubleprecision function rrgq2qght6s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh63J7
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = pi * t3 * t7
      t9 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = 0.1D1 / z
      t11 = x4 * pi
      t12 = cos(t11)
      t15 = Sqrt(-x3 * (-0.1D1 + x3))
      t21 = -t10 - 0.1D1 / (-z - x3 + 0.2D1 * t12 * t15 * z)
      t23 = 0.1D1 / x3
      t27 = t10 * t9
      t28 = 0.1D1 / x2
      t32 = 0.1D1 / x1
      t36 = pi * t10
      t37 = t3 * t7
      t38 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t44 = z ** 2
      t46 = Sin(t11)
      t47 = t46 ** 2
      t49 = t1 ** 2
      t50 = t49 ** 2
      t53 = log(0.4D1 / t44 * t47 * t50)
      t58 = (0.180D3 * t36 * lh + 0.90D2 * t53 * pi * t10) * t3
      t62 = -t8 * t9 * t21 * t23 / 0.32D2 + t8 * t27 * t28 / 0.16D2 + t8
     # * t27 * t32 / 0.16D2 + t36 * t37 * t38 / 0.32D2 - t58 * t7 * t9 /
     # 0.2880D4
      t63 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t62)
      t65 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t70 = t10 * t65
      t77 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t84 = -t8 * t65 * t21 * t23 / 0.32D2 + t8 * t70 * t28 / 0.16D2 + t
     #8 * t70 * t32 / 0.16D2 + t36 * t37 * t77 / 0.32D2 - t58 * t7 * t65
     # / 0.2880D4
      t85 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t84)
      t87 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t92 = t10 * t87
      t99 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t106 = -t8 * t87 * t21 * t23 / 0.32D2 + t8 * t92 * t28 / 0.16D2 + 
     #t8 * t92 * t32 / 0.16D2 + t36 * t37 * t99 / 0.32D2 - t58 * t7 * t8
     #7 / 0.2880D4
      t107 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t106)
      t109 = t2 * x1
      t110 = -0.1D1 + x1
      t113 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t115 = t2 * t110 * t113
      t119 = s * t49 * x1 * t110 * t113
      t120 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t115, t
     #109, 0.0D0, -t119)
      t125 = FJET(XB1, XB2, s, 0.0D0, t109, -t115, 0.0D0, -t119, -t8 * t
     #10 * t120 * t32 / 0.16D2)
      t128 = t7 * t10
      t134 = x2 * s * t1
      t137 = (-0.1D1 + x2) * s * t1
      t140 = 0.1D1 / (-z + x2 * z - x2)
      t141 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t134, -t137, 0.
     #0D0, 0.0D0, 0.0D0)
      t146 = FJET(XB1, XB2, s, 0.0D0, t134, 0.0D0, -t137, 0.0D0, t8 * t1
     #40 * t141 * t28 / 0.16D2)
      t149 = t7 * t140
      t154 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t134, -t137, 0.
     #0D0, 0.0D0, 0.0D0)
      t159 = FJET(XB1, XB2, s, 0.0D0, -t137, 0.0D0, t134, 0.0D0, t8 * t1
     #40 * t154 * t28 / 0.16D2)
      t166 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t115, t
     #109, 0.0D0, -t119)
      t171 = FJET(XB1, XB2, s, 0.0D0, -t115, t109, 0.0D0, -t119, -t8 * t
     #10 * t166 * t32 / 0.16D2)
      t178 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t183 = t10 * t178
      t190 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t197 = -t8 * t178 * t21 * t23 / 0.32D2 + t8 * t183 * t28 / 0.16D2 
     #+ t8 * t183 * t32 / 0.16D2 + t36 * t37 * t190 / 0.32D2 - t58 * t7 
     #* t178 / 0.2880D4
      t198 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t197)
      t200 = t36 * t3
      t201 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t115, t
     #109, 0.0D0, -t119)
      t206 = FJET(XB1, XB2, s, t109, 0.0D0, 0.0D0, -t115, -t119, -t200 *
     # t7 * t201 * t32 / 0.16D2)
      t213 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t134, -t137, 0.
     #0D0, 0.0D0, 0.0D0)
      t218 = FJET(XB1, XB2, s, t134, 0.0D0, -t137, 0.0D0, 0.0D0, t8 * t1
     #40 * t213 * t28 / 0.16D2)
      t225 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t134, -t137, 0.
     #0D0, 0.0D0, 0.0D0)
      t230 = FJET(XB1, XB2, s, -t137, 0.0D0, t134, 0.0D0, 0.0D0, t8 * t1
     #40 * t225 * t28 / 0.16D2)
      t237 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t115, t
     #109, 0.0D0, -t119)
      t242 = FJET(XB1, XB2, s, -t115, 0.0D0, 0.0D0, t109, -t119, -t200 *
     # t7 * t237 * t32 / 0.16D2)
      rrgq2qght6s1em2 = t63 * t62 + t85 * t84 + t107 * t106 - t125 * pi 
     #* t3 * t128 * t120 * t32 / 0.16D2 + t146 * pi * t3 * t149 * t141 *
     # t28 / 0.16D2 + t159 * pi * t3 * t149 * t154 * t28 / 0.16D2 - t171
     # * pi * t3 * t128 * t166 * t32 / 0.16D2 + t198 * t197 - t206 * pi 
     #* t10 * t37 * t201 * t32 / 0.16D2 + t218 * pi * t3 * t149 * t213 *
     # t28 / 0.16D2 + t230 * pi * t3 * t149 * t225 * t28 / 0.16D2 - t242
     # * pi * t10 * t37 * t237 * t32 / 0.16D2

      end function



      doubleprecision function rrgq2qght6s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh63J7
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / z
      t4 = pi * t3
      t5 = 0.1D1 / t1
      t6 = s ** 2
      t8 = 0.1D1 / t6 / s
      t9 = t5 * t8
      t10 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t11 = t10 * t9
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t4 * t11 /
     # 0.32D2)
      t18 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t19 = t9 * t18
      t22 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t4 * t19 /
     # 0.32D2)
      t26 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t27 = t9 * t26
      t30 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t4 * t27 /
     # 0.32D2)
      t36 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t37 = t8 * t3 * t36
      t40 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, pi * t5 * 
     #t37 / 0.32D2)
      rrgq2qght6s1em3 = t14 * pi * t3 * t11 / 0.32D2 + t22 * pi * t3 * t
     #19 / 0.32D2 + t30 * pi * t3 * t27 / 0.32D2 + t40 * pi * t5 * t37 /
     # 0.32D2

      end function



      doubleprecision function rrgq2qght6s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh63J7
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght6s1em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght6s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh63J7
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t9 = x1 ** 2
      t10 = x3 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t15 = z ** 2
      t17 = 0.1D1 / t15 / z
      t18 = t1 ** 2
      t19 = t18 ** 2
      t20 = t17 * t19
      t21 = -0.1D1 + x3
      t22 = 0.1D1 / t21
      t26 = log(-0.4D1 * t14 * t20 * t22)
      t27 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t29 = t26 ** 2
      t30 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t34 = cos(t11)
      t35 = x3 * z
      t37 = Sqrt(-t35 * t21)
      t41 = 0.1D1 / (-z - x3 + 0.2D1 * t34 * t37)
      t43 = t17 * t13
      t44 = t43 * t19
      t47 = log(0.4D1 * t10 * t44)
      t49 = t47 ** 2
      t53 = 0.1D1 / z
      t59 = pi * lh
      t60 = t3 * t7
      t71 = lh ** 2
      t73 = pi ** 2
      t75 = -0.180D3 * t71 + 0.30D2 * t73
      t76 = pi * t75
      t77 = t30 * t53
      t83 = 0.1D1 / x3
      t85 = 0.1D1 / x1
      t88 = pi * t53
      t89 = t88 * t75
      t90 = t9 * t13
      t93 = log(0.4D1 * t90 * t20)
      t99 = t93 ** 2
      t102 = t99 * t93
      t105 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t115 = -0.60D2 * lh * t73 + 0.240D3 * zeta3 + 0.120D3 * t71 * lh
      t116 = t88 * t115
      t117 = t60 * t30
      t119 = t88 * lh
      t130 = x2 ** 2
      t131 = x3 * t130
      t132 = t131 * t9
      t137 = log(-0.4D1 * t132 * t43 * t19 * t22)
      t143 = log(0.4D1 * t132 * t44)
      t147 = -0.1D1 + x2
      t148 = t19 * t147
      t152 = log(-0.4D1 * t132 * t43 * t148)
      t160 = t59 * t3
      t161 = t7 * t30
      t162 = t161 * t41
      t167 = 0.1D1 / x2
      t168 = t167 * t85
      t171 = t130 * t9
      t172 = t171 * t13
      t176 = log(-0.4D1 * t172 * t20 * t147)
      t178 = t176 ** 2
      t185 = log(0.4D1 * t171 * t44)
      t187 = t185 ** 2
      t213 = t60 * t27
      t218 = x3 * t17
      t219 = t13 * t19
      t220 = t219 * t22
      t223 = log(-0.4D1 * t218 * t220)
      t227 = log(0.4D1 * t218 * t219)
      t229 = t223 * t41 + t227 * t53
      t231 = t227 ** 2
      t234 = t223 ** 2
      t238 = t231 * t227 * t53 / 0.6D1 + t234 * t223 * t41 / 0.6D1
      t246 = pi * t115
      t252 = -t41 - t53
      t263 = -t234 * t41 / 0.2D1 - t231 * t53 / 0.2D1
      t270 = log(0.4D1 * t131 * t44)
      t272 = t270 ** 2
      t277 = t131 * t17
      t280 = log(-0.4D1 * t277 * t220)
      t282 = t280 ** 2
      t287 = t219 * t147
      t290 = log(-0.4D1 * t277 * t287)
      t292 = t290 ** 2
      t314 = t76 * t3
      t322 = t27 * t53
      t327 = t17 * t130
      t330 = log(0.4D1 * t327 * t219)
      t331 = t330 ** 2
      t334 = log(-0.4D1 * t327 * t287)
      t335 = t334 ** 2
      t338 = t7 * (t331 / 0.2D1 - t335 / 0.2D1)
      t345 = t60 * (t335 * t334 / 0.6D1 - t331 * t330 / 0.6D1)
      t357 = t7 * (-t330 + t334)
      t363 = log(0.4D1 * t44)
      t364 = t363 ** 2
      t367 = t364 * t363
      t387 = t73 ** 2
      t388 = t71 ** 2
      t393 = pi * (-0.480D3 * lh * zeta3 - t387 - 0.60D2 * t388 + 0.60D2
     # * t71 * t73)
      t400 = rrgq2qgh62J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t401 = t364 ** 2
      t412 = (-0.90D2 * t4 * t7 * ((-t8 + t26 * t27 - t29 * t30 / 0.2D1)
     # * t41 + (-t8 + t47 * t27 - t49 * t30 / 0.2D1) * t53) + 0.180D3 * 
     #t59 * t60 * ((-t27 + t26 * t30) * t41 + (-t27 + t47 * t30) * t53) 
     #+ t76 * t60 * (-t77 - t30 * t41)) * t83 * t85 / 0.1440D4 + (t89 * 
     #t60 * (-t27 + t93 * t30) - 0.90D2 * t88 * t60 * (t93 * t8 - t99 * 
     #t27 / 0.2D1 + t102 * t30 / 0.6D1 - t105) - t116 * t117 + 0.180D3 *
     # t119 * t60 * (-t8 + t93 * t27 - t99 * t30 / 0.2D1)) * t85 / 0.144
     #0D4 + (-0.90D2 * t4 * t7 * ((-t27 + t137 * t30) * t41 + (-t27 + t1
     #43 * t30) * t53 - (-t27 + t152 * t30) * t53) - 0.180D3 * t160 * t1
     #62) * t83 * t168 / 0.720D3 + (-0.90D2 * t4 * t7 * (-(-t8 + t176 * 
     #t27 - t178 * t30 / 0.2D1) * t53 + (-t8 + t185 * t27 - t187 * t30 /
     # 0.2D1) * t53) + 0.180D3 * t59 * t60 * ((-t27 + t185 * t30) * t53 
     #- (-t27 + t176 * t30) * t53)) * t167 * t85 / 0.720D3 - ((0.90D2 * 
     #t4 * t7 * t8 - 0.180D3 * t59 * t213 - t76 * t117) * t229 + 0.90D2 
     #* t4 * t161 * t238 + (-t76 * t213 + 0.90D2 * t4 * t7 * t105 - t246
     # * t117 - 0.180D3 * t59 * t60 * t8) * t252 + (0.90D2 * t4 * t7 * t
     #27 - 0.180D3 * t59 * t117) * t263) * t83 / 0.2880D4 - (-0.90D2 * t
     #4 * t7 * (-(-t8 + t270 * t27 - t272 * t30 / 0.2D1) * t53 - (-t8 + 
     #t280 * t27 - t282 * t30 / 0.2D1) * t41 + (-t8 + t290 * t27 - t292 
     #* t30 / 0.2D1) * t53) + 0.180D3 * t59 * t60 * (-(-t27 + t280 * t30
     #) * t41 - (-t27 + t270 * t30) * t53 + (-t27 + t290 * t30) * t53) +
     # t314 * t162) * t83 * t167 / 0.1440D4 + ((-0.180D3 * t77 * t59 + 0
     #.90D2 * t322 * pi) * t3 * t338 + 0.90D2 * t77 * pi * t345 + (-0.18
     #0D3 * t322 * t59 + 0.90D2 * t8 * t53 * pi - t77 * t76) * t3 * t357
     #) * t167 / 0.1440D4 + (0.180D3 * (-t364 * t27 / 0.2D1 + t367 * t30
     # / 0.6D1 - t105 + t363 * t8) * t53 * t59 + (-t8 + t363 * t27 - t36
     #4 * t30 / 0.2D1) * t53 * t76 + (-t27 + t363 * t30) * t53 * t246 - 
     #t77 * t393 - 0.90D2 * (t367 * t27 / 0.6D1 - t364 * t8 / 0.2D1 + t3
     #63 * t105 - t400 - t401 * t30 / 0.24D2) * t53 * pi) * t3 * t7 / 0.
     #2880D4
      t413 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t412)
      t415 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t416 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t418 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t443 = t418 * t53
      t456 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t465 = t60 * t418
      t490 = t7 * t418
      t491 = t490 * t41
      t529 = t60 * t416
      t597 = t416 * t53
      t638 = rrgq2qgh63J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t654 = (-0.90D2 * t4 * t7 * ((-t415 + t26 * t416 - t29 * t418 / 0.
     #2D1) * t41 + (-t415 + t47 * t416 - t49 * t418 / 0.2D1) * t53) + 0.
     #180D3 * t59 * t60 * ((-t416 + t26 * t418) * t41 + (-t416 + t47 * t
     #418) * t53) + t76 * t60 * (-t418 * t41 - t443)) * t83 * t85 / 0.14
     #40D4 + (t89 * t60 * (-t416 + t93 * t418) - 0.90D2 * t88 * t60 * (t
     #93 * t415 - t456 + t102 * t418 / 0.6D1 - t99 * t416 / 0.2D1) - t11
     #6 * t465 + 0.180D3 * t119 * t60 * (-t415 + t93 * t416 - t99 * t418
     # / 0.2D1)) * t85 / 0.1440D4 + (-0.90D2 * t4 * t7 * ((-t416 + t137 
     #* t418) * t41 - (-t416 + t152 * t418) * t53 + (-t416 + t143 * t418
     #) * t53) - 0.180D3 * t160 * t491) * t83 * t168 / 0.720D3 + (-0.90D
     #2 * t4 * t7 * (-(-t415 + t176 * t416 - t178 * t418 / 0.2D1) * t53 
     #+ (-t415 + t185 * t416 - t187 * t418 / 0.2D1) * t53) + 0.180D3 * t
     #59 * t60 * (-(-t416 + t176 * t418) * t53 + (-t416 + t185 * t418) *
     # t53)) * t167 * t85 / 0.720D3 - ((0.90D2 * t4 * t7 * t415 - 0.180D
     #3 * t59 * t529 - t76 * t465) * t229 + 0.90D2 * t4 * t490 * t238 + 
     #(-t76 * t529 + 0.90D2 * t4 * t7 * t456 - t246 * t465 - 0.180D3 * t
     #59 * t60 * t415) * t252 + (0.90D2 * t4 * t7 * t416 - 0.180D3 * t59
     # * t465) * t263) * t83 / 0.2880D4 - (-0.90D2 * t4 * t7 * (-(-t415 
     #+ t270 * t416 - t272 * t418 / 0.2D1) * t53 + (-t415 + t290 * t416 
     #- t292 * t418 / 0.2D1) * t53 - (-t415 + t280 * t416 - t282 * t418 
     #/ 0.2D1) * t41) + 0.180D3 * t59 * t60 * (-(-t416 + t280 * t418) * 
     #t41 - (-t416 + t270 * t418) * t53 + (-t416 + t290 * t418) * t53) +
     # t314 * t491) * t83 * t167 / 0.1440D4 + ((-0.180D3 * t443 * t59 + 
     #0.90D2 * t597 * pi) * t3 * t338 + 0.90D2 * t443 * pi * t345 + (-0.
     #180D3 * t597 * t59 + 0.90D2 * t415 * t53 * pi - t443 * t76) * t3 *
     # t357) * t167 / 0.1440D4 + (0.180D3 * (-t456 - t364 * t416 / 0.2D1
     # + t363 * t415 + t367 * t418 / 0.6D1) * t53 * t59 + (-t415 + t363 
     #* t416 - t364 * t418 / 0.2D1) * t53 * t76 + (-t416 + t363 * t418) 
     #* t53 * t246 - t443 * t393 - 0.90D2 * (-t638 + t367 * t416 / 0.6D1
     # - t364 * t415 / 0.2D1 - t401 * t418 / 0.24D2 + t363 * t456) * t53
     # * pi) * t3 * t7 / 0.2880D4
      t655 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t654)
      t657 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t659 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t662 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t685 = t659 * t53
      t699 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t707 = t60 * t659
      t732 = t7 * t659
      t733 = t732 * t41
      t771 = t60 * t657
      t839 = t657 * t53
      t886 = rrgq2qgh61J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t896 = (-0.90D2 * t4 * t7 * ((t26 * t657 - t29 * t659 / 0.2D1 - t6
     #62) * t41 + (t47 * t657 - t49 * t659 / 0.2D1 - t662) * t53) + 0.18
     #0D3 * t59 * t60 * ((-t657 + t26 * t659) * t41 + (-t657 + t47 * t65
     #9) * t53) + t76 * t60 * (-t659 * t41 - t685)) * t83 * t85 / 0.1440
     #D4 + (t89 * t60 * (-t657 + t93 * t659) - 0.90D2 * t88 * t60 * (t10
     #2 * t659 / 0.6D1 - t699 + t93 * t662 - t99 * t657 / 0.2D1) - t116 
     #* t707 + 0.180D3 * t119 * t60 * (t93 * t657 - t99 * t659 / 0.2D1 -
     # t662)) * t85 / 0.1440D4 + (-0.90D2 * t4 * t7 * ((-t657 + t137 * t
     #659) * t41 + (-t657 + t143 * t659) * t53 - (-t657 + t152 * t659) *
     # t53) - 0.180D3 * t160 * t733) * t83 * t168 / 0.720D3 + (-0.90D2 *
     # t4 * t7 * ((t185 * t657 - t187 * t659 / 0.2D1 - t662) * t53 - (t1
     #76 * t657 - t178 * t659 / 0.2D1 - t662) * t53) + 0.180D3 * t59 * t
     #60 * ((-t657 + t185 * t659) * t53 - (-t657 + t176 * t659) * t53)) 
     #* t167 * t85 / 0.720D3 - ((0.90D2 * t4 * t7 * t662 - 0.180D3 * t59
     # * t771 - t76 * t707) * t229 + 0.90D2 * t4 * t732 * t238 + (-t76 *
     # t771 + 0.90D2 * t4 * t7 * t699 - t246 * t707 - 0.180D3 * t59 * t6
     #0 * t662) * t252 + (0.90D2 * t4 * t7 * t657 - 0.180D3 * t59 * t707
     #) * t263) * t83 / 0.2880D4 - (-0.90D2 * t4 * t7 * (-(t270 * t657 -
     # t272 * t659 / 0.2D1 - t662) * t53 - (t280 * t657 - t282 * t659 / 
     #0.2D1 - t662) * t41 + (t290 * t657 - t292 * t659 / 0.2D1 - t662) *
     # t53) + 0.180D3 * t59 * t60 * (-(-t657 + t270 * t659) * t53 - (-t6
     #57 + t280 * t659) * t41 + (-t657 + t290 * t659) * t53) + t314 * t7
     #33) * t83 * t167 / 0.1440D4 + ((-0.180D3 * t685 * t59 + 0.90D2 * t
     #839 * pi) * t3 * t338 + 0.90D2 * t685 * pi * t345 + (-0.180D3 * t8
     #39 * t59 + 0.90D2 * t662 * t53 * pi - t685 * t76) * t3 * t357) * t
     #167 / 0.1440D4 + (0.180D3 * (t363 * t662 - t699 - t364 * t657 / 0.
     #2D1 + t367 * t659 / 0.6D1) * t53 * t59 + (t363 * t657 - t364 * t65
     #9 / 0.2D1 - t662) * t53 * t76 + (-t657 + t363 * t659) * t53 * t246
     # - t685 * t393 - 0.90D2 * (-t364 * t662 / 0.2D1 - t401 * t659 / 0.
     #24D2 + t367 * t657 / 0.6D1 - t886 + t363 * t699) * t53 * pi) * t3 
     #* t7 / 0.2880D4
      t897 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t896)
      t900 = x1 * z
      t901 = -z - x1 + t900
      t902 = 0.1D1 / t901
      t904 = t2 * x1 * t147 * t902
      t905 = -0.1D1 + x1
      t906 = t2 * t905
      t909 = x2 * s * t1 * x1
      t910 = s * t18
      t913 = x1 * t905 * t902
      t914 = t910 * t147 * t913
      t915 = t131 * t90
      t916 = 0.1D1 / t15
      t917 = t916 * t19
      t918 = t905 ** 2
      t919 = t902 * t918
      t924 = log(0.4D1 * t915 * t917 * t919 * t147)
      t925 = x2 * x1
      t926 = t925 * z
      t928 = 0.1D1 / (t926 - z - t925)
      t929 = t924 * t928
      t930 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t909, t904, -t9
     #06, 0.0D0, -t914)
      t932 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, t909, t904, -t9
     #06, 0.0D0, -t914)
      t933 = t928 * t932
      t938 = t7 * t928
      t939 = t938 * t930
      t945 = t13 * t916
      t947 = t19 * t902
      t952 = log(0.4D1 * t171 * t945 * t947 * t918 * t147)
      t953 = t952 ** 2
      t954 = t953 * t928
      t957 = t952 * t928
      t959 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, t909, t904, -t9
     #06, 0.0D0, -t914)
      t975 = (-0.90D2 * t4 * t7 * (-t929 * t930 + t933) + 0.180D3 * t160
     # * t939) * t83 * t168 / 0.720D3 + (-0.90D2 * t4 * t7 * (t954 * t93
     #0 / 0.2D1 - t957 * t932 + t928 * t959) + 0.180D3 * t59 * t60 * (t9
     #33 - t957 * t930) + t314 * t939) * t167 * t85 / 0.720D3
      t976 = FJET(XB1, XB2, s, 0.0D0, t904, -t906, t909, -t914, t975)
      t978 = x2 * x3
      t979 = 0.1D1 - x3 + t978
      t980 = 0.1D1 / t979
      t981 = t978 * t980
      t982 = t2 * t981
      t984 = t2 * t21 * t980
      t986 = t979 ** 2
      t987 = 0.1D1 / t986
      t992 = log(0.4D1 * t131 * t43 * t148 * t21 * t987)
      t993 = t992 ** 2
      t994 = t147 * t21
      t996 = Sqrt(t35 * t994)
      t1000 = 0.1D1 / (-z - x3 + t978 + 0.2D1 * t34 * t996)
      t1001 = t993 * t1000
      t1002 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t984, t982, 0.0D0)
      t1005 = t992 * t1000
      t1006 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t984, t982, 0.0D0)
      t1008 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t984, t982, 0.0D0)
      t1015 = t1000 * t1006
      t1020 = t7 * t1000
      t1021 = t1020 * t1002
      t1027 = t994 * t987
      t1031 = log(0.4D1 * t915 * t20 * t1027)
      t1032 = t1031 * t1000
      t1044 = -(-0.90D2 * t4 * t7 * (-t1001 * t1002 / 0.2D1 + t1005 * t1
     #006 - t1000 * t1008) + 0.180D3 * t59 * t60 * (t1005 * t1002 - t101
     #5) - t314 * t1021) * t83 * t167 / 0.1440D4 + (-0.90D2 * t4 * t7 * 
     #(t1015 - t1032 * t1002) + 0.180D3 * t160 * t1021) * t83 * t168 / 0
     #.720D3
      t1045 = FJET(XB1, XB2, s, 0.0D0, t982, 0.0D0, -t984, 0.0D0, t1044)
      t1048 = t2 * x1 * t902
      t1049 = t910 * t913
      t1050 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1051 = t53 * t1050
      t1058 = log(0.4D1 * t10 * t945 * t947 * t918 * t22)
      t1059 = t1058 * t901
      t1060 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1062 = t1058 ** 2
      t1063 = t1062 * t901
      t1064 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1068 = x3 * x1
      t1069 = t1068 * z
      t1070 = x1 * t15
      t1071 = x3 * t15
      t1072 = t1071 * x1
      t1073 = t10 * t15
      t1075 = 0.2D1 * t10 * z
      t1076 = x3 * t901
      t1078 = Sqrt(t1076 * t21)
      t1083 = 0.1D1 / (-t900 - t1069 + t1070 + t1072 - t1073 - t10 + t10
     #75 - t35 - t15 + 0.2D1 * t34 * t1078 * z)
      t1085 = t917 * t919
      t1088 = log(-0.4D1 * t14 * t1085)
      t1089 = t1088 ** 2
      t1090 = t1089 * t53
      t1093 = t1088 * t53
      t1099 = t901 * t1060
      t1104 = t53 * t1060
      t1113 = t60 * (t53 * t1064 - t901 * t1064 * t1083)
      t1119 = t90 * t916
      t1120 = t947 * t918
      t1123 = log(-0.4D1 * t1119 * t1120)
      t1129 = t1123 ** 2
      t1132 = t1129 * t1123
      t1135 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1154 = log(-0.4D1 * t915 * t1085)
      t1155 = t1154 * t53
      t1161 = log(0.4D1 * t915 * t917 * t919 * t22)
      t1162 = t1161 * t901
      t1178 = log(-0.4D1 * t172 * t1085)
      t1179 = t1178 * t53
      t1181 = t1178 ** 2
      t1182 = t1181 * t53
      t1194 = t7 * t53
      t1201 = (-0.90D2 * t4 * t7 * (t1051 - (t901 * t1050 - t1059 * t106
     #0 + t1063 * t1064 / 0.2D1) * t1083 + t1090 * t1064 / 0.2D1 - t1093
     # * t1060) + 0.180D3 * t59 * t60 * (-(t1099 - t1059 * t1064) * t108
     #3 - t1093 * t1064 + t1104) + t76 * t1113) * t83 * t85 / 0.1440D4 +
     # (t89 * t60 * (-t1123 * t1064 + t1060) - 0.90D2 * t88 * t60 * (-t1
     #123 * t1050 + t1129 * t1060 / 0.2D1 - t1132 * t1064 / 0.6D1 + t113
     #5) + t116 * t60 * t1064 + 0.180D3 * t119 * t60 * (t1129 * t1064 / 
     #0.2D1 - t1123 * t1060 + t1050)) * t85 / 0.1440D4 + (-0.90D2 * t4 *
     # t7 * (-t1155 * t1064 - (t1099 - t1162 * t1064) * t1083 + t1104) +
     # 0.180D3 * t59 * t1113) * t83 * t168 / 0.720D3 + (-0.90D2 * t4 * t
     #7 * (-t1179 * t1060 + t1051 + t1182 * t1064 / 0.2D1) + 0.180D3 * t
     #59 * t60 * (t1104 - t1179 * t1064) + t314 * t1194 * t1064) * t167 
     #* t85 / 0.720D3
      t1202 = FJET(XB1, XB2, s, 0.0D0, -t906, -t1048, 0.0D0, t1049, t120
     #1)
      t1204 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1205 = t53 * t1204
      t1206 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1208 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1221 = t901 * t1206
      t1226 = t53 * t1206
      t1235 = t60 * (t53 * t1208 - t901 * t1208 * t1083)
      t1245 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1299 = (-0.90D2 * t4 * t7 * (t1205 - t1093 * t1206 + t1090 * t120
     #8 / 0.2D1 - (t901 * t1204 - t1059 * t1206 + t1063 * t1208 / 0.2D1)
     # * t1083) + 0.180D3 * t59 * t60 * (-(t1221 - t1059 * t1208) * t108
     #3 - t1093 * t1208 + t1226) + t76 * t1235) * t83 * t85 / 0.1440D4 +
     # (t89 * t60 * (t1206 - t1123 * t1208) - 0.90D2 * t88 * t60 * (t124
     #5 - t1132 * t1208 / 0.6D1 - t1123 * t1204 + t1129 * t1206 / 0.2D1)
     # + t116 * t60 * t1208 + 0.180D3 * t119 * t60 * (-t1123 * t1206 + t
     #1129 * t1208 / 0.2D1 + t1204)) * t85 / 0.1440D4 + (-0.90D2 * t4 * 
     #t7 * (-(t1221 - t1162 * t1208) * t1083 - t1155 * t1208 + t1226) + 
     #0.180D3 * t59 * t1235) * t83 * t168 / 0.720D3 + (-0.90D2 * t4 * t7
     # * (t1182 * t1208 / 0.2D1 - t1179 * t1206 + t1205) + 0.180D3 * t59
     # * t60 * (-t1179 * t1208 + t1226) + t314 * t1194 * t1208) * t167 *
     # t85 / 0.720D3
      t1300 = FJET(XB1, XB2, s, 0.0D0, -t1048, -t906, 0.0D0, t1049, t129
     #9)
      t1302 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t984, t982, 0.0D0)
      t1304 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t984, t982, 0.0D0)
      t1305 = t1000 * t1304
      t1310 = t1020 * t1302
      t1320 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t984, t982, 0.0D0)
      t1336 = (-0.90D2 * t4 * t7 * (-t1032 * t1302 + t1305) + 0.180D3 * 
     #t160 * t1310) * t83 * t168 / 0.720D3 - (-0.90D2 * t4 * t7 * (-t100
     #1 * t1302 / 0.2D1 + t1005 * t1304 - t1000 * t1320) + 0.180D3 * t59
     # * t60 * (t1005 * t1302 - t1305) - t314 * t1310) * t83 * t167 / 0.
     #1440D4
      t1337 = FJET(XB1, XB2, s, 0.0D0, -t984, 0.0D0, t982, 0.0D0, t1336)
      t1339 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1341 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1344 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1367 = t1341 * t53
      t1379 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1389 = t60 * t1341
      t1414 = t7 * t1341
      t1415 = t1414 * t41
      t1472 = rrgq2qgh64J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1489 = t60 * t1339
      t1557 = t1339 * t53
      t1578 = (-0.90D2 * t4 * t7 * ((t47 * t1339 - t49 * t1341 / 0.2D1 -
     # t1344) * t53 + (t26 * t1339 - t29 * t1341 / 0.2D1 - t1344) * t41)
     # + 0.180D3 * t59 * t60 * ((-t1339 + t26 * t1341) * t41 + (-t1339 +
     # t47 * t1341) * t53) + t76 * t60 * (-t1341 * t41 - t1367)) * t83 *
     # t85 / 0.1440D4 + (t89 * t60 * (-t1339 + t93 * t1341) - 0.90D2 * t
     #88 * t60 * (-t1379 + t93 * t1344 + t102 * t1341 / 0.6D1 - t99 * t1
     #339 / 0.2D1) - t116 * t1389 + 0.180D3 * t119 * t60 * (-t99 * t1341
     # / 0.2D1 - t1344 + t93 * t1339)) * t85 / 0.1440D4 + (-0.90D2 * t4 
     #* t7 * (-(-t1339 + t152 * t1341) * t53 + (-t1339 + t143 * t1341) *
     # t53 + (-t1339 + t137 * t1341) * t41) - 0.180D3 * t160 * t1415) * 
     #t83 * t168 / 0.720D3 + (-0.90D2 * t4 * t7 * ((t185 * t1339 - t187 
     #* t1341 / 0.2D1 - t1344) * t53 - (t176 * t1339 - t178 * t1341 / 0.
     #2D1 - t1344) * t53) + 0.180D3 * t59 * t60 * (-(-t1339 + t176 * t13
     #41) * t53 + (-t1339 + t185 * t1341) * t53)) * t167 * t85 / 0.720D3
     # + (0.180D3 * (-t364 * t1339 / 0.2D1 - t1379 + t363 * t1344 + t367
     # * t1341 / 0.6D1) * t53 * t59 + (t363 * t1339 - t364 * t1341 / 0.2
     #D1 - t1344) * t53 * t76 + (-t1339 + t363 * t1341) * t53 * t246 - t
     #1367 * t393 - 0.90D2 * (-t401 * t1341 / 0.24D2 - t1472 - t364 * t1
     #344 / 0.2D1 + t363 * t1379 + t367 * t1339 / 0.6D1) * t53 * pi) * t
     #3 * t7 / 0.2880D4 - ((0.90D2 * t4 * t7 * t1344 - 0.180D3 * t59 * t
     #1489 - t76 * t1389) * t229 + 0.90D2 * t4 * t1414 * t238 + (-t76 * 
     #t1489 + 0.90D2 * t4 * t7 * t1379 - t246 * t1389 - 0.180D3 * t59 * 
     #t60 * t1344) * t252 + (0.90D2 * t4 * t7 * t1339 - 0.180D3 * t59 * 
     #t1389) * t263) * t83 / 0.2880D4 - (-0.90D2 * t4 * t7 * ((t290 * t1
     #339 - t292 * t1341 / 0.2D1 - t1344) * t53 - (t270 * t1339 - t272 *
     # t1341 / 0.2D1 - t1344) * t53 - (t280 * t1339 - t282 * t1341 / 0.2
     #D1 - t1344) * t41) + 0.180D3 * t59 * t60 * (-(-t1339 + t280 * t134
     #1) * t41 + (-t1339 + t290 * t1341) * t53 - (-t1339 + t270 * t1341)
     # * t53) + t314 * t1415) * t83 * t167 / 0.1440D4 + ((-0.180D3 * t13
     #67 * t59 + 0.90D2 * t1557 * pi) * t3 * t338 + 0.90D2 * t1367 * pi 
     #* t345 + (-0.180D3 * t1557 * t59 + 0.90D2 * t1344 * t53 * pi - t13
     #67 * t76) * t3 * t357) * t167 / 0.1440D4
      t1579 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1578)
      t1581 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t909, t904, -t
     #906, 0.0D0, -t914)
      t1583 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, t909, t904, -t
     #906, 0.0D0, -t914)
      t1584 = t928 * t1583
      t1589 = t938 * t1581
      t1598 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, t909, t904, -t
     #906, 0.0D0, -t914)
      t1614 = (-0.90D2 * t4 * t7 * (-t929 * t1581 + t1584) + 0.180D3 * t
     #160 * t1589) * t83 * t168 / 0.720D3 + (-0.90D2 * t4 * t7 * (t954 *
     # t1581 / 0.2D1 - t957 * t1583 + t928 * t1598) + 0.180D3 * t59 * t6
     #0 * (-t957 * t1581 + t1584) + t314 * t1589) * t167 * t85 / 0.720D3
      t1615 = FJET(XB1, XB2, s, t909, -t906, t904, 0.0D0, -t914, t1614)
      t1617 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t909, t904, -t
     #906, 0.0D0, -t914)
      t1619 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, t909, t904, -t
     #906, 0.0D0, -t914)
      t1620 = t928 * t1619
      t1625 = t938 * t1617
      t1634 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, t909, t904, -t
     #906, 0.0D0, -t914)
      t1650 = (-0.90D2 * t4 * t7 * (-t929 * t1617 + t1620) + 0.180D3 * t
     #160 * t1625) * t83 * t168 / 0.720D3 + (-0.90D2 * t4 * t7 * (-t957 
     #* t1619 + t954 * t1617 / 0.2D1 + t928 * t1634) + 0.180D3 * t59 * t
     #60 * (-t957 * t1617 + t1620) + t314 * t1625) * t167 * t85 / 0.720D
     #3
      t1651 = FJET(XB1, XB2, s, t904, 0.0D0, t909, -t906, -t914, t1650)
      t1653 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t984, t982, 0.0D0)
      t1655 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t984, t982, 0.0D0)
      t1656 = t1000 * t1655
      t1661 = t1020 * t1653
      t1669 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t984, t982, 0.0D0)
      t1687 = (-0.90D2 * t4 * t7 * (-t1032 * t1653 + t1656) + 0.180D3 * 
     #t160 * t1661) * t83 * t168 / 0.720D3 - (-0.90D2 * t4 * t7 * (t1005
     # * t1655 - t1000 * t1669 - t1001 * t1653 / 0.2D1) + 0.180D3 * t59 
     #* t60 * (t1005 * t1653 - t1656) - t314 * t1661) * t83 * t167 / 0.1
     #440D4
      t1688 = FJET(XB1, XB2, s, t982, 0.0D0, -t984, 0.0D0, 0.0D0, t1687)
      t1693 = t21 * s * t1 * t905 * t980
      t1694 = t2 * x1
      t1696 = Sqrt(-t1076 * t994)
      t1697 = t34 * t1696
      t1703 = t1694 * x2 * (-x3 + t978 - z + t35 - x1 + t1068 + t900 - t
     #1069 + 0.2D1 * t1697) * t902 * t980
      t1704 = t906 * t981
      t1705 = t131 * t900
      t1708 = t131 * x1
      t1713 = t1694 * (-t1705 + 0.2D1 * t1697 * x2 + 0.1D1 - x3 - x2 + t
     #978 + t1708 + t131 * z) * t902 * t980
      t1726 = x2 * t9
      t1727 = t1705 - 0.2D1 * t978 * t900 + t1071 * t925 + 0.2D1 * t10 *
     # x2 * z - t10 * t15 * x2 - 0.2D1 * t1697 * t925 - t1070 + t10 + t3
     #5 + t15 + t900 + 0.2D1 * t1697 * t926 + t1726
      t1737 = t1069 - t1072 + t1073 - t1075 - t1708 - 0.2D1 * t1697 * z 
     #- t978 * z + t978 * x1 - t10 * x2 - t925 * t15 - 0.2D1 * t1726 * z
     # + t1726 * t15 + t926
      t1739 = 0.1D1 / (t1727 + t1737)
      t1740 = t901 * t1739
      t1741 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, t1703, -t1713,
     # t1693, -t1704, -t914)
      t1747 = log(-0.4D1 * t131 * t1119 * t1120 * t1027)
      t1748 = t1747 * t901
      t1749 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t1703, -t1713,
     # t1693, -t1704, -t914)
      t1750 = t1739 * t1749
      t1756 = t7 * t901
      t1760 = -0.90D2 * t4 * t7 * (-t1740 * t1741 + t1748 * t1750) - 0.1
     #80D3 * t160 * t1756 * t1750
      t1764 = FJET(XB1, XB2, s, t1693, t1703, -t1704, -t1713, -t914, t17
     #60 * t83 * t168 / 0.720D3)
      t1767 = t83 * t167 * t85
      t1770 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, t1703, -t1713,
     # t1693, -t1704, -t914)
      t1772 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t1703, -t1713,
     # t1693, -t1704, -t914)
      t1773 = t1739 * t1772
      t1782 = -0.90D2 * t4 * t7 * (-t1740 * t1770 + t1748 * t1773) - 0.1
     #80D3 * t160 * t1756 * t1773
      t1786 = FJET(XB1, XB2, s, t1703, t1693, -t1713, -t1704, -t914, t17
     #82 * t83 * t168 / 0.720D3)
      t1790 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1792 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1794 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1800 = t53 * t1790
      t1807 = t901 * t1792
      t1812 = t53 * t1792
      t1821 = t60 * (t53 * t1794 - t901 * t1794 * t1083)
      t1835 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1885 = (-0.90D2 * t4 * t7 * (-(t901 * t1790 - t1059 * t1792 + t10
     #63 * t1794 / 0.2D1) * t1083 - t1093 * t1792 + t1800 + t1090 * t179
     #4 / 0.2D1) + 0.180D3 * t59 * t60 * (-(t1807 - t1059 * t1794) * t10
     #83 - t1093 * t1794 + t1812) + t76 * t1821) * t83 * t85 / 0.1440D4 
     #+ (t89 * t60 * (t1792 - t1123 * t1794) - 0.90D2 * t88 * t60 * (-t1
     #132 * t1794 / 0.6D1 + t1129 * t1792 / 0.2D1 + t1835 - t1123 * t179
     #0) + t116 * t60 * t1794 + 0.180D3 * t119 * t60 * (t1129 * t1794 / 
     #0.2D1 + t1790 - t1123 * t1792)) * t85 / 0.1440D4 + (-0.90D2 * t4 *
     # t7 * (t1812 - (t1807 - t1162 * t1794) * t1083 - t1155 * t1794) + 
     #0.180D3 * t59 * t1821) * t83 * t168 / 0.720D3 + (-0.90D2 * t4 * t7
     # * (-t1179 * t1792 + t1800 + t1182 * t1794 / 0.2D1) + 0.180D3 * t5
     #9 * t60 * (-t1179 * t1794 + t1812) + t314 * t1194 * t1794) * t167 
     #* t85 / 0.720D3
      t1886 = FJET(XB1, XB2, s, -t906, 0.0D0, 0.0D0, -t1048, t1049, t188
     #5)
      t1888 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t909, t904, -t
     #906, 0.0D0, -t914)
      t1890 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, t909, t904, -t
     #906, 0.0D0, -t914)
      t1891 = t928 * t1890
      t1896 = t938 * t1888
      t1903 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, t909, t904, -t
     #906, 0.0D0, -t914)
      t1921 = (-0.90D2 * t4 * t7 * (-t929 * t1888 + t1891) + 0.180D3 * t
     #160 * t1896) * t83 * t168 / 0.720D3 + (-0.90D2 * t4 * t7 * (-t957 
     #* t1890 + t928 * t1903 + t954 * t1888 / 0.2D1) + 0.180D3 * t59 * t
     #60 * (t1891 - t957 * t1888) + t314 * t1896) * t167 * t85 / 0.720D3
      t1922 = FJET(XB1, XB2, s, -t906, t909, 0.0D0, t904, -t914, t1921)
      t1924 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1926 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1928 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t1934 = t53 * t1924
      t1941 = t901 * t1926
      t1946 = t53 * t1926
      t1955 = t60 * (t53 * t1928 - t901 * t1928 * t1083)
      t1968 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t1048,
     # -t906, 0.0D0, t1049)
      t2019 = (-0.90D2 * t4 * t7 * (-(t901 * t1924 - t1059 * t1926 + t10
     #63 * t1928 / 0.2D1) * t1083 - t1093 * t1926 + t1934 + t1090 * t192
     #8 / 0.2D1) + 0.180D3 * t59 * t60 * (-(t1941 - t1059 * t1928) * t10
     #83 - t1093 * t1928 + t1946) + t76 * t1955) * t83 * t85 / 0.1440D4 
     #+ (t89 * t60 * (t1926 - t1123 * t1928) - 0.90D2 * t88 * t60 * (-t1
     #132 * t1928 / 0.6D1 - t1123 * t1924 + t1968 + t1129 * t1926 / 0.2D
     #1) + t116 * t60 * t1928 + 0.180D3 * t119 * t60 * (-t1123 * t1926 +
     # t1924 + t1129 * t1928 / 0.2D1)) * t85 / 0.1440D4 + (-0.90D2 * t4 
     #* t7 * (t1946 - (t1941 - t1162 * t1928) * t1083 - t1155 * t1928) +
     # 0.180D3 * t59 * t1955) * t83 * t168 / 0.720D3 + (-0.90D2 * t4 * t
     #7 * (-t1179 * t1926 + t1934 + t1182 * t1928 / 0.2D1) + 0.180D3 * t
     #59 * t60 * (t1946 - t1179 * t1928) + t314 * t1194 * t1928) * t167 
     #* t85 / 0.720D3
      t2020 = FJET(XB1, XB2, s, -t1048, 0.0D0, 0.0D0, -t906, t1049, t201
     #9)
      t2022 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t984, t982, 0.0D0)
      t2023 = t1000 * t2022
      t2024 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t984, t982, 0.0D0)
      t2030 = t1020 * t2024
      t2038 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t984, t982, 0.0D0)
      t2056 = (-0.90D2 * t4 * t7 * (t2023 - t1032 * t2024) + 0.180D3 * t
     #160 * t2030) * t83 * t168 / 0.720D3 - (-0.90D2 * t4 * t7 * (t1005 
     #* t2022 - t1000 * t2038 - t1001 * t2024 / 0.2D1) + 0.180D3 * t59 *
     # t60 * (t1005 * t2024 - t2023) - t314 * t2030) * t83 * t167 / 0.14
     #40D4
      t2057 = FJET(XB1, XB2, s, -t984, 0.0D0, t982, 0.0D0, 0.0D0, t2056)
      t2059 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, t1703, -t1713,
     # t1693, -t1704, -t914)
      t2061 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t1703, -t1713,
     # t1693, -t1704, -t914)
      t2062 = t1739 * t2061
      t2071 = -0.90D2 * t4 * t7 * (-t1740 * t2059 + t1748 * t2062) - 0.1
     #80D3 * t160 * t1756 * t2062
      t2075 = FJET(XB1, XB2, s, -t1713, -t1704, t1703, t1693, -t914, t20
     #71 * t83 * t168 / 0.720D3)
      t2079 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, t1703, -t1713,
     # t1693, -t1704, -t914)
      t2081 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t1703, -t1713,
     # t1693, -t1704, -t914)
      t2082 = t1739 * t2081
      t2091 = -0.90D2 * t4 * t7 * (-t1740 * t2079 + t1748 * t2082) - 0.1
     #80D3 * t160 * t1756 * t2082
      t2095 = FJET(XB1, XB2, s, -t1704, -t1713, t1693, t1703, -t914, t20
     #91 * t83 * t168 / 0.720D3)
      rrgq2qght6s2e1 = t413 * t412 + t655 * t654 + t897 * t896 + t976 * 
     #t975 + t1045 * t1044 + t1202 * t1201 + t1300 * t1299 + t1337 * t13
     #36 + t1579 * t1578 + t1615 * t1614 + t1651 * t1650 + t1688 * t1687
     # + t1764 * t1760 * t1767 / 0.720D3 + t1786 * t1782 * t1767 / 0.720
     #D3 + t1886 * t1885 + t1922 * t1921 + t2020 * t2019 + t2057 * t2056
     # + t2075 * t2071 * t1767 / 0.720D3 + t2095 * t2091 * t1767 / 0.720
     #D3

      end function



      doubleprecision function rrgq2qght6s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh63J7
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t9 = x1 ** 2
      t10 = x3 * t9
      t11 = x4 * pi
      t12 = Sin(t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t15 = z ** 2
      t17 = 0.1D1 / t15 / z
      t18 = t1 ** 2
      t19 = t18 ** 2
      t20 = t17 * t19
      t21 = -0.1D1 + x3
      t22 = 0.1D1 / t21
      t26 = log(-0.4D1 * t14 * t20 * t22)
      t27 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t30 = cos(t11)
      t31 = x3 * z
      t33 = Sqrt(-t31 * t21)
      t37 = 0.1D1 / (-z - x3 + 0.2D1 * t30 * t33)
      t39 = t17 * t13
      t40 = t39 * t19
      t43 = log(0.4D1 * t10 * t40)
      t46 = 0.1D1 / z
      t52 = pi * lh
      t53 = t3 * t7
      t54 = t27 * t46
      t61 = 0.1D1 / x3
      t63 = 0.1D1 / x1
      t66 = t7 * t27
      t69 = 0.1D1 / x2
      t70 = t69 * t63
      t71 = t37 * t61 * t70
      t74 = t4 * t7
      t75 = x2 ** 2
      t76 = t75 * t9
      t79 = log(0.4D1 * t76 * t40)
      t83 = t76 * t13
      t84 = -0.1D1 + x2
      t88 = log(-0.4D1 * t83 * t20 * t84)
      t97 = pi * t46
      t98 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t99 = t9 * t13
      t102 = log(0.4D1 * t99 * t20)
      t104 = t102 ** 2
      t111 = t97 * lh
      t117 = lh ** 2
      t119 = pi ** 2
      t121 = -0.180D3 * t117 + 0.30D2 * t119
      t122 = t97 * t121
      t123 = t27 * t53
      t128 = x3 * t17
      t129 = t13 * t19
      t130 = t129 * t22
      t133 = log(-0.4D1 * t128 * t130)
      t134 = t133 ** 2
      t138 = log(0.4D1 * t128 * t129)
      t139 = t138 ** 2
      t142 = -t134 * t37 / 0.2D1 - t139 * t46 / 0.2D1
      t154 = t133 * t37 + t138 * t46
      t162 = pi * t121
      t165 = -t37 - t46
      t171 = log(0.4D1 * t40)
      t173 = t171 ** 2
      t186 = pi * (-0.60D2 * lh * t119 + 0.240D3 * zeta3 + 0.120D3 * t11
     #7 * lh)
      t190 = t173 * t171
      t193 = rrgq2qgh62J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t207 = x3 * t75
      t208 = t207 * t17
      t211 = log(-0.4D1 * t208 * t130)
      t217 = log(0.4D1 * t207 * t40)
      t221 = t129 * t84
      t224 = log(-0.4D1 * t208 * t221)
      t232 = t52 * t3
      t241 = t17 * t75
      t244 = log(0.4D1 * t241 * t129)
      t245 = t244 ** 2
      t248 = log(-0.4D1 * t241 * t221)
      t249 = t248 ** 2
      t252 = t53 * (t245 / 0.2D1 - t249 / 0.2D1)
      t263 = t7 * (-t244 + t248)
      t268 = (-0.90D2 * t4 * t7 * ((-t8 + t26 * t27) * t37 + (-t8 + t43 
     #* t27) * t46) + 0.180D3 * t52 * t53 * (-t54 - t27 * t37)) * t61 * 
     #t63 / 0.1440D4 + t4 * t66 * t71 / 0.8D1 - t74 * ((-t8 + t79 * t27)
     # * t46 - (-t8 + t88 * t27) * t46) * t69 * t63 / 0.8D1 + (-0.90D2 *
     # t97 * t53 * (-t98 + t102 * t8 - t104 * t27 / 0.2D1) + 0.180D3 * t
     #111 * t53 * (-t8 + t102 * t27) - t122 * t123) * t63 / 0.1440D4 - (
     #0.90D2 * t4 * t66 * t142 + (0.90D2 * t4 * t7 * t8 - 0.180D3 * t52 
     #* t123) * t154 + (0.90D2 * t4 * t7 * t98 - 0.180D3 * t52 * t53 * t
     #8 - t162 * t123) * t165) * t61 / 0.2880D4 + (0.180D3 * (-t98 + t17
     #1 * t8 - t173 * t27 / 0.2D1) * t46 * t52 - t54 * t186 - 0.90D2 * (
     #-t173 * t8 / 0.2D1 + t190 * t27 / 0.6D1 - t193 + t171 * t98) * t46
     # * pi + (-t8 + t171 * t27) * t46 * t162) * t3 * t7 / 0.2880D4 - (-
     #0.90D2 * t4 * t7 * (-(-t8 + t211 * t27) * t37 - (-t8 + t217 * t27)
     # * t46 + (-t8 + t224 * t27) * t46) + 0.180D3 * t232 * t66 * t37) *
     # t61 * t69 / 0.1440D4 + (0.90D2 * t54 * pi * t252 + (-0.180D3 * t5
     #4 * t52 + 0.90D2 * t8 * t46 * pi) * t3 * t263) * t69 / 0.1440D4
      t269 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t268)
      t271 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t272 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t284 = t272 * t46
      t293 = t7 * t272
      t308 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t321 = t53 * t272
      t356 = rrgq2qgh63J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t408 = (-0.90D2 * t4 * t7 * ((-t271 + t26 * t272) * t37 + (-t271 +
     # t43 * t272) * t46) + 0.180D3 * t52 * t53 * (-t272 * t37 - t284)) 
     #* t61 * t63 / 0.1440D4 + t4 * t293 * t71 / 0.8D1 - t74 * (-(-t271 
     #+ t88 * t272) * t46 + (-t271 + t79 * t272) * t46) * t69 * t63 / 0.
     #8D1 + (-0.90D2 * t97 * t53 * (-t308 + t102 * t271 - t104 * t272 / 
     #0.2D1) + 0.180D3 * t111 * t53 * (-t271 + t102 * t272) - t122 * t32
     #1) * t63 / 0.1440D4 - (0.90D2 * t4 * t293 * t142 + (0.90D2 * t4 * 
     #t7 * t271 - 0.180D3 * t52 * t321) * t154 + (0.90D2 * t4 * t7 * t30
     #8 - 0.180D3 * t52 * t53 * t271 - t162 * t321) * t165) * t61 / 0.28
     #80D4 + (0.180D3 * (-t308 + t171 * t271 - t173 * t272 / 0.2D1) * t4
     #6 * t52 - t284 * t186 - 0.90D2 * (-t356 - t173 * t271 / 0.2D1 + t1
     #71 * t308 + t190 * t272 / 0.6D1) * t46 * pi + (-t271 + t171 * t272
     #) * t46 * t162) * t3 * t7 / 0.2880D4 - (-0.90D2 * t4 * t7 * (-(-t2
     #71 + t211 * t272) * t37 - (-t271 + t217 * t272) * t46 + (-t271 + t
     #224 * t272) * t46) + 0.180D3 * t232 * t293 * t37) * t61 * t69 / 0.
     #1440D4 + (0.90D2 * t284 * pi * t252 + (-0.180D3 * t284 * t52 + 0.9
     #0D2 * t271 * t46 * pi) * t3 * t263) * t69 / 0.1440D4
      t409 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t408)
      t411 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t412 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t424 = t412 * t46
      t433 = t7 * t412
      t451 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t461 = t53 * t412
      t497 = rrgq2qgh61J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t548 = (-0.90D2 * t4 * t7 * ((-t411 + t26 * t412) * t37 + (-t411 +
     # t43 * t412) * t46) + 0.180D3 * t52 * t53 * (-t412 * t37 - t424)) 
     #* t61 * t63 / 0.1440D4 + t4 * t433 * t71 / 0.8D1 - t74 * ((-t411 +
     # t79 * t412) * t46 - (-t411 + t88 * t412) * t46) * t69 * t63 / 0.8
     #D1 + (-0.90D2 * t97 * t53 * (t102 * t411 - t104 * t412 / 0.2D1 - t
     #451) + 0.180D3 * t111 * t53 * (-t411 + t102 * t412) - t122 * t461)
     # * t63 / 0.1440D4 - (0.90D2 * t4 * t433 * t142 + (0.90D2 * t4 * t7
     # * t411 - 0.180D3 * t52 * t461) * t154 + (0.90D2 * t4 * t7 * t451 
     #- 0.180D3 * t52 * t53 * t411 - t162 * t461) * t165) * t61 / 0.2880
     #D4 + (0.180D3 * (t171 * t411 - t173 * t412 / 0.2D1 - t451) * t46 *
     # t52 - t424 * t186 - 0.90D2 * (t171 * t451 - t497 - t173 * t411 / 
     #0.2D1 + t190 * t412 / 0.6D1) * t46 * pi + (-t411 + t171 * t412) * 
     #t46 * t162) * t3 * t7 / 0.2880D4 - (-0.90D2 * t4 * t7 * (-(-t411 +
     # t217 * t412) * t46 - (-t411 + t211 * t412) * t37 + (-t411 + t224 
     #* t412) * t46) + 0.180D3 * t232 * t433 * t37) * t61 * t69 / 0.1440
     #D4 + (0.90D2 * t424 * pi * t252 + (-0.180D3 * t424 * t52 + 0.90D2 
     #* t411 * t46 * pi) * t3 * t263) * t69 / 0.1440D4
      t549 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t548)
      t552 = x1 * z
      t553 = -z - x1 + t552
      t554 = 0.1D1 / t553
      t556 = t2 * x1 * t84 * t554
      t557 = -0.1D1 + x1
      t558 = t2 * t557
      t561 = x2 * s * t1 * x1
      t562 = s * t18
      t565 = x1 * t557 * t554
      t566 = t562 * t84 * t565
      t567 = x2 * x1
      t568 = t567 * z
      t570 = 0.1D1 / (t568 - z - t567)
      t571 = t7 * t570
      t572 = t4 * t571
      t573 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t561, t556, -t5
     #58, 0.0D0, -t566)
      t578 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, t561, t556, -t5
     #58, 0.0D0, -t566)
      t580 = 0.1D1 / t15
      t581 = t13 * t580
      t583 = t19 * t554
      t584 = t557 ** 2
      t589 = log(0.4D1 * t76 * t581 * t583 * t584 * t84)
      t590 = t589 * t570
      t603 = -t572 * t573 * t61 * t70 / 0.8D1 + (-0.90D2 * t4 * t7 * (t5
     #70 * t578 - t590 * t573) + 0.180D3 * t232 * t571 * t573) * t69 * t
     #63 / 0.720D3
      t604 = FJET(XB1, XB2, s, 0.0D0, t556, -t558, t561, -t566, t603)
      t606 = x2 * x3
      t607 = 0.1D1 - x3 + t606
      t608 = 0.1D1 / t607
      t609 = t606 * t608
      t610 = t2 * t609
      t612 = t2 * t21 * t608
      t613 = t84 * t21
      t615 = Sqrt(t31 * t613)
      t619 = 0.1D1 / (-z - x3 + t606 + 0.2D1 * t30 * t615)
      t620 = t7 * t619
      t621 = t4 * t620
      t622 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t612, t610, 0.0D0)
      t629 = t607 ** 2
      t635 = log(0.4D1 * t207 * t39 * t19 * t84 * t21 / t629)
      t636 = t635 * t619
      t638 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t612, t610, 0.0D0)
      t651 = -t621 * t622 * t61 * t70 / 0.8D1 - (-0.90D2 * t4 * t7 * (t6
     #36 * t622 - t619 * t638) - 0.180D3 * t232 * t620 * t622) * t61 * t
     #69 / 0.1440D4
      t652 = FJET(XB1, XB2, s, 0.0D0, t610, 0.0D0, -t612, 0.0D0, t651)
      t655 = t2 * x1 * t554
      t656 = t562 * t565
      t657 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t655, -
     #t558, 0.0D0, t656)
      t664 = log(0.4D1 * t10 * t581 * t583 * t584 * t22)
      t665 = t664 * t553
      t666 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t655, -
     #t558, 0.0D0, t656)
      t669 = x3 * x1
      t670 = t669 * z
      t671 = x1 * t15
      t672 = x3 * t15
      t673 = t672 * x1
      t674 = t10 * t15
      t676 = 0.2D1 * t10 * z
      t677 = x3 * t553
      t679 = Sqrt(t677 * t21)
      t684 = 0.1D1 / (-t552 - t670 + t671 + t673 - t674 - t10 + t676 - t
     #31 - t15 + 0.2D1 * t30 * t679 * z)
      t688 = t580 * t19 * t554 * t584
      t691 = log(-0.4D1 * t14 * t688)
      t692 = t691 * t46
      t694 = t46 * t657
      t702 = t46 * t666 - t553 * t666 * t684
      t716 = log(-0.4D1 * t83 * t688)
      t717 = t716 * t46
      t723 = t7 * t46
      t724 = t723 * t666
      t735 = log(-0.4D1 * t99 * t580 * t583 * t584)
      t736 = t735 ** 2
      t740 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t655, -
     #t558, 0.0D0, t656)
      t750 = t162 * t3
      t755 = (-0.90D2 * t4 * t7 * (-(t553 * t657 - t665 * t666) * t684 -
     # t692 * t666 + t694) + 0.180D3 * t52 * t53 * t702) * t61 * t63 / 0
     #.1440D4 - t74 * t702 * t61 * t70 / 0.8D1 + (-0.90D2 * t4 * t7 * (t
     #694 - t717 * t666) + 0.180D3 * t232 * t724) * t69 * t63 / 0.720D3 
     #+ (-0.90D2 * t97 * t53 * (t736 * t666 / 0.2D1 - t735 * t657 + t740
     #) + 0.180D3 * t111 * t53 * (-t735 * t666 + t657) + t750 * t724) * 
     #t63 / 0.1440D4
      t756 = FJET(XB1, XB2, s, 0.0D0, -t558, -t655, 0.0D0, t656, t755)
      t758 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t655, -
     #t558, 0.0D0, t656)
      t760 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t655, -
     #t558, 0.0D0, t656)
      t765 = t46 * t758
      t773 = t46 * t760 - t553 * t760 * t684
      t790 = t723 * t760
      t800 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t655, -
     #t558, 0.0D0, t656)
      t814 = (-0.90D2 * t4 * t7 * (-(t553 * t758 - t665 * t760) * t684 -
     # t692 * t760 + t765) + 0.180D3 * t52 * t53 * t773) * t61 * t63 / 0
     #.1440D4 - t74 * t773 * t61 * t70 / 0.8D1 + (-0.90D2 * t4 * t7 * (-
     #t717 * t760 + t765) + 0.180D3 * t232 * t790) * t69 * t63 / 0.720D3
     # + (-0.90D2 * t97 * t53 * (-t735 * t758 + t736 * t760 / 0.2D1 + t8
     #00) + 0.180D3 * t111 * t53 * (t758 - t735 * t760) + t750 * t790) *
     # t63 / 0.1440D4
      t815 = FJET(XB1, XB2, s, 0.0D0, -t655, -t558, 0.0D0, t656, t814)
      t817 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t612, t610, 0.0D0)
      t823 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t612, t610, 0.0D0)
      t836 = -t621 * t817 * t61 * t70 / 0.8D1 - (-0.90D2 * t4 * t7 * (t6
     #36 * t817 - t619 * t823) - 0.180D3 * t232 * t620 * t817) * t61 * t
     #69 / 0.1440D4
      t837 = FJET(XB1, XB2, s, 0.0D0, -t612, 0.0D0, t610, 0.0D0, t836)
      t839 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t840 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t852 = t840 * t46
      t861 = t7 * t840
      t878 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t889 = t53 * t840
      t926 = rrgq2qgh64J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t976 = (-0.90D2 * t4 * t7 * ((-t839 + t26 * t840) * t37 + (-t839 +
     # t43 * t840) * t46) + 0.180D3 * t52 * t53 * (-t840 * t37 - t852)) 
     #* t61 * t63 / 0.1440D4 + t4 * t861 * t71 / 0.8D1 - t74 * (-(-t839 
     #+ t88 * t840) * t46 + (-t839 + t79 * t840) * t46) * t69 * t63 / 0.
     #8D1 + (-0.90D2 * t97 * t53 * (-t104 * t840 / 0.2D1 - t878 + t102 *
     # t839) + 0.180D3 * t111 * t53 * (-t839 + t102 * t840) - t122 * t88
     #9) * t63 / 0.1440D4 - (0.90D2 * t4 * t861 * t142 + (0.90D2 * t4 * 
     #t7 * t839 - 0.180D3 * t52 * t889) * t154 + (0.90D2 * t4 * t7 * t87
     #8 - 0.180D3 * t52 * t53 * t839 - t162 * t889) * t165) * t61 / 0.28
     #80D4 + (0.180D3 * (t171 * t839 - t173 * t840 / 0.2D1 - t878) * t46
     # * t52 - t852 * t186 - 0.90D2 * (-t173 * t839 / 0.2D1 - t926 + t17
     #1 * t878 + t190 * t840 / 0.6D1) * t46 * pi + (-t839 + t171 * t840)
     # * t46 * t162) * t3 * t7 / 0.2880D4 - (-0.90D2 * t4 * t7 * (-(-t83
     #9 + t211 * t840) * t37 + (-t839 + t224 * t840) * t46 - (-t839 + t2
     #17 * t840) * t46) + 0.180D3 * t232 * t861 * t37) * t61 * t69 / 0.1
     #440D4 + (0.90D2 * t852 * pi * t252 + (-0.180D3 * t852 * t52 + 0.90
     #D2 * t839 * t46 * pi) * t3 * t263) * t69 / 0.1440D4
      t977 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t976)
      t979 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t561, t556, -t5
     #58, 0.0D0, -t566)
      t985 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, t561, t556, -t5
     #58, 0.0D0, -t566)
      t998 = -t572 * t979 * t61 * t70 / 0.8D1 + (-0.90D2 * t4 * t7 * (-t
     #590 * t979 + t570 * t985) + 0.180D3 * t232 * t571 * t979) * t69 * 
     #t63 / 0.720D3
      t999 = FJET(XB1, XB2, s, t561, -t558, t556, 0.0D0, -t566, t998)
      t1001 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t561, t556, -t
     #558, 0.0D0, -t566)
      t1007 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, t561, t556, -t
     #558, 0.0D0, -t566)
      t1020 = -t572 * t1001 * t61 * t70 / 0.8D1 + (-0.90D2 * t4 * t7 * (
     #-t590 * t1001 + t570 * t1007) + 0.180D3 * t232 * t571 * t1001) * t
     #69 * t63 / 0.720D3
      t1021 = FJET(XB1, XB2, s, t556, 0.0D0, t561, -t558, -t566, t1020)
      t1023 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t612, t610, 0.0D0)
      t1029 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t612, t610, 0.0D0)
      t1042 = -t621 * t1023 * t61 * t70 / 0.8D1 - (-0.90D2 * t4 * t7 * (
     #t636 * t1023 - t619 * t1029) - 0.180D3 * t232 * t620 * t1023) * t6
     #1 * t69 / 0.1440D4
      t1043 = FJET(XB1, XB2, s, t610, 0.0D0, -t612, 0.0D0, 0.0D0, t1042)
      t1048 = t21 * s * t1 * t557 * t608
      t1049 = t2 * x1
      t1051 = Sqrt(-t677 * t613)
      t1052 = t30 * t1051
      t1058 = t1049 * x2 * (-x3 + t606 - z + t31 - x1 + t669 + t552 - t6
     #70 + 0.2D1 * t1052) * t554 * t608
      t1059 = t558 * t609
      t1060 = t207 * t552
      t1063 = t207 * x1
      t1068 = t1049 * (-t1060 + 0.2D1 * t1052 * x2 + 0.1D1 - x3 - x2 + t
     #606 + t1063 + t207 * z) * t554 * t608
      t1070 = t4 * t7 * t553
      t1077 = x2 * t9
      t1081 = t15 - t1063 - 0.2D1 * t1052 * z - t606 * z + t606 * x1 - t
     #10 * x2 - t567 * t15 - 0.2D1 * t1077 * z + t1077 * t15 + t568 + t6
     #70 - t673 + t674
      t1094 = -t676 + t1060 - 0.2D1 * t606 * t552 + t672 * t567 + 0.2D1 
     #* t10 * x2 * z - t10 * t15 * x2 - 0.2D1 * t1052 * t567 + t552 - t6
     #71 + t10 + t31 + 0.2D1 * t1052 * t568 + t1077
      t1096 = 0.1D1 / (t1081 + t1094)
      t1097 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1068,
     # t1048, -t1059, -t566)
      t1100 = t61 * t69 * t63
      t1101 = t1096 * t1097 * t1100
      t1104 = FJET(XB1, XB2, s, t1048, t1058, -t1059, -t1068, -t566, t10
     #70 * t1101 / 0.8D1)
      t1106 = t53 * t553
      t1110 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1068,
     # t1048, -t1059, -t566)
      t1112 = t1096 * t1110 * t1100
      t1115 = FJET(XB1, XB2, s, t1058, t1048, -t1068, -t1059, -t566, t10
     #70 * t1112 / 0.8D1)
      t1120 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t655, 
     #-t558, 0.0D0, t656)
      t1122 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t655, 
     #-t558, 0.0D0, t656)
      t1127 = t46 * t1120
      t1135 = t46 * t1122 - t553 * t1122 * t684
      t1152 = t723 * t1122
      t1161 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t655, 
     #-t558, 0.0D0, t656)
      t1176 = (-0.90D2 * t4 * t7 * (-(t553 * t1120 - t665 * t1122) * t68
     #4 - t692 * t1122 + t1127) + 0.180D3 * t52 * t53 * t1135) * t61 * t
     #63 / 0.1440D4 - t74 * t1135 * t61 * t70 / 0.8D1 + (-0.90D2 * t4 * 
     #t7 * (-t717 * t1122 + t1127) + 0.180D3 * t232 * t1152) * t69 * t63
     # / 0.720D3 + (-0.90D2 * t97 * t53 * (t736 * t1122 / 0.2D1 + t1161 
     #- t735 * t1120) + 0.180D3 * t111 * t53 * (t1120 - t735 * t1122) + 
     #t750 * t1152) * t63 / 0.1440D4
      t1177 = FJET(XB1, XB2, s, -t558, 0.0D0, 0.0D0, -t655, t656, t1176)
      t1179 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t561, t556, -t
     #558, 0.0D0, -t566)
      t1184 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, t561, t556, -t
     #558, 0.0D0, -t566)
      t1198 = -t572 * t1179 * t61 * t70 / 0.8D1 + (-0.90D2 * t4 * t7 * (
     #t570 * t1184 - t590 * t1179) + 0.180D3 * t232 * t571 * t1179) * t6
     #9 * t63 / 0.720D3
      t1199 = FJET(XB1, XB2, s, -t558, t561, 0.0D0, t556, -t566, t1198)
      t1201 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t655, 
     #-t558, 0.0D0, t656)
      t1203 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t655, 
     #-t558, 0.0D0, t656)
      t1208 = t46 * t1201
      t1216 = t46 * t1203 - t553 * t1203 * t684
      t1233 = t723 * t1203
      t1241 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t655, 
     #-t558, 0.0D0, t656)
      t1257 = (-0.90D2 * t4 * t7 * (-(t553 * t1201 - t665 * t1203) * t68
     #4 - t692 * t1203 + t1208) + 0.180D3 * t52 * t53 * t1216) * t61 * t
     #63 / 0.1440D4 - t74 * t1216 * t61 * t70 / 0.8D1 + (-0.90D2 * t4 * 
     #t7 * (t1208 - t717 * t1203) + 0.180D3 * t232 * t1233) * t69 * t63 
     #/ 0.720D3 + (-0.90D2 * t97 * t53 * (-t735 * t1201 + t1241 + t736 *
     # t1203 / 0.2D1) + 0.180D3 * t111 * t53 * (t1201 - t735 * t1203) + 
     #t750 * t1233) * t63 / 0.1440D4
      t1258 = FJET(XB1, XB2, s, -t655, 0.0D0, 0.0D0, -t558, t656, t1257)
      t1260 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t612, t610, 0.0D0)
      t1266 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t612, t610, 0.0D0)
      t1279 = -t621 * t1260 * t61 * t70 / 0.8D1 - (-0.90D2 * t4 * t7 * (
     #t636 * t1260 - t619 * t1266) - 0.180D3 * t232 * t620 * t1260) * t6
     #1 * t69 / 0.1440D4
      t1280 = FJET(XB1, XB2, s, -t612, 0.0D0, t610, 0.0D0, 0.0D0, t1279)
      t1282 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1068,
     # t1048, -t1059, -t566)
      t1284 = t1096 * t1282 * t1100
      t1287 = FJET(XB1, XB2, s, -t1068, -t1059, t1058, t1048, -t566, t10
     #70 * t1284 / 0.8D1)
      t1292 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1068,
     # t1048, -t1059, -t566)
      t1294 = t1096 * t1292 * t1100
      t1297 = FJET(XB1, XB2, s, -t1059, -t1068, t1048, t1058, -t566, t10
     #70 * t1294 / 0.8D1)
      rrgq2qght6s2e0 = t269 * t268 + t409 * t408 + t549 * t548 + t604 * 
     #t603 + t652 * t651 + t756 * t755 + t815 * t814 + t837 * t836 + t97
     #7 * t976 + t999 * t998 + t1021 * t1020 + t1043 * t1042 + t1104 * p
     #i * t1106 * t1101 / 0.8D1 + t1115 * pi * t1106 * t1112 / 0.8D1 + t
     #1177 * t1176 + t1199 * t1198 + t1258 * t1257 + t1280 * t1279 + t12
     #87 * pi * t1106 * t1284 / 0.8D1 + t1297 * pi * t1106 * t1294 / 0.8
     #D1

      end function



      doubleprecision function rrgq2qght6s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh63J7
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x3 * t12
      t14 = x4 * pi
      t15 = Sin(t14)
      t16 = t15 ** 2
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t20 = -0.1D1 + x3
      t25 = log(-0.4D1 * t13 * t19 / t20)
      t26 = cos(t14)
      t27 = x3 * z
      t29 = Sqrt(-t27 * t20)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t29)
      t37 = log(0.4D1 * t13 * t19)
      t38 = 0.1D1 / z
      t40 = t25 * t33 + t37 * t38
      t44 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t48 = pi * lh
      t49 = t3 * t7
      t50 = t49 * t8
      t54 = -t33 - t38
      t57 = 0.1D1 / x3
      t60 = pi * t38
      t61 = x1 ** 2
      t62 = t61 * t16
      t66 = log(0.4D1 * t62 * t12 * t18)
      t72 = t60 * lh
      t76 = 0.1D1 / x1
      t79 = t4 * t7
      t80 = t8 * t38
      t81 = t8 * t33
      t87 = 0.1D1 / x2
      t88 = t57 * t87
      t93 = x2 ** 2
      t94 = t12 * t93
      t97 = log(0.4D1 * t94 * t19)
      t98 = -0.1D1 + x2
      t102 = log(-0.4D1 * t94 * t19 * t98)
      t105 = t49 * (-t97 + t102) * t87
      t111 = log(0.4D1 * t12 * t16 * t18)
      t117 = rrgq2qgh62J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t119 = t111 ** 2
      t126 = lh ** 2
      t128 = pi ** 2
      t131 = pi * (-0.180D3 * t126 + 0.30D2 * t128)
      t137 = -(0.90D2 * t4 * t7 * t8 * t40 + (0.90D2 * t4 * t7 * t44 - 0
     #.180D3 * t48 * t50) * t54) * t57 / 0.2880D4 + (-0.90D2 * t60 * t49
     # * (-t44 + t66 * t8) - 0.180D3 * t72 * t50) * t76 / 0.1440D4 - t79
     # * (-t80 - t81) * t57 * t76 / 0.16D2 + t79 * t81 * t88 / 0.16D2 + 
     #t80 * pi * t105 / 0.16D2 + (0.180D3 * (-t44 + t111 * t8) * t38 * t
     #48 - 0.90D2 * (-t117 + t111 * t44 - t119 * t8 / 0.2D1) * t38 * pi 
     #- t80 * t131) * t3 * t7 / 0.2880D4
      t138 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t137)
      t140 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t145 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t149 = t49 * t140
      t167 = t140 * t33
      t168 = t140 * t38
      t185 = rrgq2qgh63J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t198 = -(0.90D2 * t4 * t7 * t140 * t40 + (0.90D2 * t4 * t7 * t145 
     #- 0.180D3 * t48 * t149) * t54) * t57 / 0.2880D4 + (-0.90D2 * t60 *
     # t49 * (-t145 + t66 * t140) - 0.180D3 * t72 * t149) * t76 / 0.1440
     #D4 - t79 * (-t167 - t168) * t57 * t76 / 0.16D2 + t79 * t167 * t88 
     #/ 0.16D2 + t168 * pi * t105 / 0.16D2 + (0.180D3 * (-t145 + t111 * 
     #t140) * t38 * t48 - 0.90D2 * (-t185 + t111 * t145 - t119 * t140 / 
     #0.2D1) * t38 * pi - t168 * t131) * t3 * t7 / 0.2880D4
      t199 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t198)
      t201 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t206 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t210 = t49 * t201
      t228 = t201 * t33
      t229 = t201 * t38
      t249 = rrgq2qgh61J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t259 = -(0.90D2 * t4 * t7 * t201 * t40 + (0.90D2 * t4 * t7 * t206 
     #- 0.180D3 * t48 * t210) * t54) * t57 / 0.2880D4 + (-0.90D2 * t60 *
     # t49 * (-t206 + t66 * t201) - 0.180D3 * t72 * t210) * t76 / 0.1440
     #D4 - t79 * (-t228 - t229) * t57 * t76 / 0.16D2 + t79 * t228 * t88 
     #/ 0.16D2 + t229 * pi * t105 / 0.16D2 + (0.180D3 * (-t206 + t111 * 
     #t201) * t38 * t48 - 0.90D2 * (t111 * t206 - t119 * t201 / 0.2D1 - 
     #t249) * t38 * pi - t229 * t131) * t3 * t7 / 0.2880D4
      t260 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t259)
      t263 = x1 * z
      t264 = -z - x1 + t263
      t265 = 0.1D1 / t264
      t267 = t2 * x1 * t98 * t265
      t268 = -0.1D1 + x1
      t269 = t2 * t268
      t272 = x2 * s * t1 * x1
      t273 = s * t17
      t276 = x1 * t268 * t265
      t277 = t273 * t98 * t276
      t278 = x2 * x1
      t281 = 0.1D1 / (t278 * z - z - t278)
      t282 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, t272, t267, -t2
     #69, 0.0D0, -t277)
      t284 = t87 * t76
      t285 = t281 * t282 * t284
      t288 = FJET(XB1, XB2, s, 0.0D0, t267, -t269, t272, -t277, -t79 * t
     #285 / 0.8D1)
      t293 = x2 * x3
      t295 = 0.1D1 / (0.1D1 - x3 + t293)
      t297 = t2 * t293 * t295
      t299 = t2 * t20 * t295
      t302 = Sqrt(t27 * t98 * t20)
      t306 = 0.1D1 / (-z - x3 + t293 + 0.2D1 * t26 * t302)
      t307 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t299, t297, 0.0D0)
      t309 = t306 * t307 * t88
      t312 = FJET(XB1, XB2, s, 0.0D0, t297, 0.0D0, -t299, 0.0D0, -t79 * 
     #t309 / 0.16D2)
      t318 = t2 * x1 * t265
      t319 = t273 * t276
      t320 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t318, -
     #t269, 0.0D0, t319)
      t321 = t38 * t320
      t328 = t268 ** 2
      t332 = log(-0.4D1 * t62 / t10 * t18 * t265 * t328)
      t334 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t318, -
     #t269, 0.0D0, t319)
      t339 = t48 * t3
      t340 = t7 * t38
      t353 = x3 * t61
      t359 = Sqrt(x3 * t264 * t20)
      t364 = 0.1D1 / (-t263 - x3 * x1 * z + x1 * t10 + x3 * t10 * x1 - t
     #353 * t10 - t353 + 0.2D1 * t353 * z - t27 - t10 + 0.2D1 * t26 * t3
     #59 * z)
      t371 = -t79 * t321 * t284 / 0.8D1 + (-0.90D2 * t60 * t49 * (-t332 
     #* t320 + t334) + 0.180D3 * t339 * t340 * t320) * t76 / 0.1440D4 - 
     #t79 * (t321 - t264 * t320 * t364) * t57 * t76 / 0.16D2
      t372 = FJET(XB1, XB2, s, 0.0D0, -t269, -t318, 0.0D0, t319, t371)
      t374 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t318, -
     #t269, 0.0D0, t319)
      t375 = t38 * t374
      t379 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t318, -
     #t269, 0.0D0, t319)
      t398 = -t79 * t375 * t284 / 0.8D1 + (-0.90D2 * t60 * t49 * (t379 -
     # t332 * t374) + 0.180D3 * t339 * t340 * t374) * t76 / 0.1440D4 - t
     #79 * (t375 - t264 * t374 * t364) * t57 * t76 / 0.16D2
      t399 = FJET(XB1, XB2, s, 0.0D0, -t318, -t269, 0.0D0, t319, t398)
      t401 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t299, t297, 0.0D0)
      t403 = t306 * t401 * t88
      t406 = FJET(XB1, XB2, s, 0.0D0, -t299, 0.0D0, t297, 0.0D0, -t79 * 
     #t403 / 0.16D2)
      t411 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t416 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t420 = t49 * t411
      t438 = t411 * t33
      t439 = t411 * t38
      t459 = rrgq2qgh64J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t469 = -(0.90D2 * t4 * t7 * t411 * t40 + (0.90D2 * t4 * t7 * t416 
     #- 0.180D3 * t48 * t420) * t54) * t57 / 0.2880D4 + (-0.90D2 * t60 *
     # t49 * (-t416 + t66 * t411) - 0.180D3 * t72 * t420) * t76 / 0.1440
     #D4 - t79 * (-t438 - t439) * t57 * t76 / 0.16D2 + t79 * t438 * t88 
     #/ 0.16D2 + t439 * pi * t105 / 0.16D2 + (0.180D3 * (-t416 + t111 * 
     #t411) * t38 * t48 - 0.90D2 * (t111 * t416 - t119 * t411 / 0.2D1 - 
     #t459) * t38 * pi - t439 * t131) * t3 * t7 / 0.2880D4
      t470 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t469)
      t472 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, t272, t267, -t2
     #69, 0.0D0, -t277)
      t474 = t281 * t472 * t284
      t477 = FJET(XB1, XB2, s, t272, -t269, t267, 0.0D0, -t277, -t79 * t
     #474 / 0.8D1)
      t482 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, t272, t267, -t2
     #69, 0.0D0, -t277)
      t484 = t281 * t482 * t284
      t487 = FJET(XB1, XB2, s, t267, 0.0D0, t272, -t269, -t277, -t79 * t
     #484 / 0.8D1)
      t492 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t299, t297, 0.0D0)
      t494 = t306 * t492 * t88
      t497 = FJET(XB1, XB2, s, t297, 0.0D0, -t299, 0.0D0, 0.0D0, -t79 * 
     #t494 / 0.16D2)
      t502 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t318, -
     #t269, 0.0D0, t319)
      t503 = t38 * t502
      t507 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t318, -
     #t269, 0.0D0, t319)
      t526 = -t79 * t503 * t284 / 0.8D1 + (-0.90D2 * t60 * t49 * (t507 -
     # t332 * t502) + 0.180D3 * t339 * t340 * t502) * t76 / 0.1440D4 - t
     #79 * (t503 - t264 * t502 * t364) * t57 * t76 / 0.16D2
      t527 = FJET(XB1, XB2, s, -t269, 0.0D0, 0.0D0, -t318, t319, t526)
      t529 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, t272, t267, -t2
     #69, 0.0D0, -t277)
      t531 = t281 * t529 * t284
      t534 = FJET(XB1, XB2, s, -t269, t272, 0.0D0, t267, -t277, -t79 * t
     #531 / 0.8D1)
      t539 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t318, -
     #t269, 0.0D0, t319)
      t540 = t38 * t539
      t544 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t318, -
     #t269, 0.0D0, t319)
      t563 = -t79 * t540 * t284 / 0.8D1 + (-0.90D2 * t60 * t49 * (t544 -
     # t332 * t539) + 0.180D3 * t339 * t340 * t539) * t76 / 0.1440D4 - t
     #79 * (t540 - t264 * t539 * t364) * t57 * t76 / 0.16D2
      t564 = FJET(XB1, XB2, s, -t318, 0.0D0, 0.0D0, -t269, t319, t563)
      t566 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t299, t297, 0.0D0)
      t568 = t306 * t566 * t88
      t571 = FJET(XB1, XB2, s, -t299, 0.0D0, t297, 0.0D0, 0.0D0, -t79 * 
     #t568 / 0.16D2)
      rrgq2qght6s2em1 = t138 * t137 + t199 * t198 + t260 * t259 - t288 *
     # pi * t49 * t285 / 0.8D1 - t312 * pi * t49 * t309 / 0.16D2 + t372 
     #* t371 + t399 * t398 - t406 * pi * t49 * t403 / 0.16D2 + t470 * t4
     #69 - t477 * pi * t49 * t474 / 0.8D1 - t487 * pi * t49 * t484 / 0.8
     #D1 - t497 * pi * t49 * t494 / 0.16D2 + t527 * t526 - t534 * pi * t
     #49 * t531 / 0.8D1 + t564 * t563 - t571 * pi * t49 * t568 / 0.16D2

      end function



      doubleprecision function rrgq2qght6s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh63J7
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t4 = 0.1D1 / z
      t5 = t3 * t4
      t7 = 0.1D1 / t1
      t8 = s ** 2
      t10 = 0.1D1 / t8 / s
      t12 = 0.1D1 / x1
      t13 = t7 * t10 * t12
      t17 = pi * t7 * t10
      t18 = x4 * pi
      t19 = cos(t18)
      t23 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t28 = -0.1D1 / (-z - x3 + 0.2D1 * t19 * t23) - t4
      t30 = 0.1D1 / x3
      t34 = pi * lh
      t37 = rrgq2qgh62J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t38 = z ** 2
      t41 = Sin(t18)
      t42 = t41 ** 2
      t44 = t1 ** 2
      t45 = t44 ** 2
      t48 = log(0.4D1 / t38 / z * t42 * t45)
      t58 = t5 * pi * t13 / 0.16D2 - t17 * t3 * t28 * t30 / 0.32D2 + (-0
     #.180D3 * t5 * t34 - 0.90D2 * (-t37 + t48 * t3) * t4 * pi) * t7 * t
     #10 / 0.2880D4
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t58)
      t61 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t62 = t61 * t4
      t72 = rrgq2qgh63J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t82 = t62 * pi * t13 / 0.16D2 - t17 * t61 * t28 * t30 / 0.32D2 + (
     #-0.180D3 * t62 * t34 - 0.90D2 * (-t72 + t48 * t61) * t4 * pi) * t7
     # * t10 / 0.2880D4
      t83 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t82)
      t85 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t86 = t85 * t4
      t96 = rrgq2qgh61J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t106 = t86 * pi * t13 / 0.16D2 - t17 * t85 * t28 * t30 / 0.32D2 + 
     #(-0.180D3 * t86 * t34 - 0.90D2 * (-t96 + t48 * t85) * t4 * pi) * t
     #7 * t10 / 0.2880D4
      t107 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t106)
      t109 = -0.1D1 + x1
      t110 = t2 * t109
      t113 = 0.1D1 / (-z - x1 + x1 * z)
      t115 = t2 * x1 * t113
      t119 = s * t44 * x1 * t109 * t113
      t120 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t115, -
     #t110, 0.0D0, t119)
      t125 = FJET(XB1, XB2, s, 0.0D0, -t110, -t115, 0.0D0, t119, -t17 * 
     #t4 * t120 * t12 / 0.16D2)
      t128 = t10 * t4
      t133 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t115, -
     #t110, 0.0D0, t119)
      t138 = FJET(XB1, XB2, s, 0.0D0, -t115, -t110, 0.0D0, t119, -t17 * 
     #t4 * t133 * t12 / 0.16D2)
      t147 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t159 = rrgq2qgh64J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t169 = pi * t4 * t7 * t10 * t147 * t12 / 0.16D2 - t17 * t147 * t28
     # * t30 / 0.32D2 + (-0.180D3 * t147 * t4 * t34 - 0.90D2 * (-t159 + 
     #t48 * t147) * t4 * pi) * t7 * t10 / 0.2880D4
      t170 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t169)
      t172 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t115, -
     #t110, 0.0D0, t119)
      t177 = FJET(XB1, XB2, s, -t110, 0.0D0, 0.0D0, -t115, t119, -t17 * 
     #t4 * t172 * t12 / 0.16D2)
      t184 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t115, -
     #t110, 0.0D0, t119)
      t189 = FJET(XB1, XB2, s, -t115, 0.0D0, 0.0D0, -t110, t119, -t17 * 
     #t4 * t184 * t12 / 0.16D2)
      rrgq2qght6s2em2 = t59 * t58 + t83 * t82 + t107 * t106 - t125 * pi 
     #* t7 * t128 * t120 * t12 / 0.16D2 - t138 * pi * t7 * t128 * t133 *
     # t12 / 0.16D2 + t170 * t169 - t177 * pi * t7 * t128 * t172 * t12 /
     # 0.16D2 - t189 * pi * t7 * t128 * t184 * t12 / 0.16D2

      end function



      doubleprecision function rrgq2qght6s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh63J7
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = rrgq2qgh62J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t4 = 0.1D1 / z
      t6 = 0.1D1 / t1
      t8 = s ** 2
      t10 = 0.1D1 / t8 / s
      t11 = pi * t6 * t10
      t14 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t3 * t4 * 
     #t11 / 0.32D2)
      t18 = rrgq2qgh63J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t22 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t18 * t4 *
     # t11 / 0.32D2)
      t26 = rrgq2qgh61J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t30 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t26 * t4 *
     # t11 / 0.32D2)
      t36 = rrgq2qgh64J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t37 = t6 * t10 * t36
      t40 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, pi * t4 * 
     #t37 / 0.32D2)
      rrgq2qght6s2em3 = t14 * t3 * t4 * t11 / 0.32D2 + t22 * t18 * t4 * 
     #t11 / 0.32D2 + t30 * t26 * t4 * t11 / 0.32D2 + t40 * pi * t4 * t37
     # / 0.32D2

      end function



      doubleprecision function rrgq2qght6s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgq2qgh61J1
      doubleprecision rrgq2qgh61J2
      doubleprecision rrgq2qgh61J3
      doubleprecision rrgq2qgh61J4
      doubleprecision rrgq2qgh61J5
      doubleprecision rrgq2qgh61J6
      doubleprecision rrgq2qgh61J7
      doubleprecision rrgq2qgh62J1
      doubleprecision rrgq2qgh62J2
      doubleprecision rrgq2qgh62J3
      doubleprecision rrgq2qgh62J4
      doubleprecision rrgq2qgh62J5
      doubleprecision rrgq2qgh62J6
      doubleprecision rrgq2qgh63J1
      doubleprecision rrgq2qgh63J2
      doubleprecision rrgq2qgh63J3
      doubleprecision rrgq2qgh63J4
      doubleprecision rrgq2qgh63J5
      doubleprecision rrgq2qgh63J6
      doubleprecision rrgq2qgh63J7
      doubleprecision rrgq2qgh64J1
      doubleprecision rrgq2qgh64J2
      doubleprecision rrgq2qgh64J3
      doubleprecision rrgq2qgh64J4
      doubleprecision rrgq2qgh64J5
      doubleprecision rrgq2qgh64J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght6s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh61J1
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S13 + S14 + S34
      t2 = S13 * t1
      t3 = t2 * S34
      t7 = 0.1D1 / S12
      t12 = 0.1D1 / (S12 + S13 + S23)
      t13 = S12 ** 2
      t17 = S13 * S14
      t19 = S23 * S13
      t21 = S13 ** 2
      t32 = S14 ** 2
      t33 = t32 * S13
      t35 = t17 * S23
      t37 = S14 * t21
      t39 = S23 ** 2
      t40 = S13 * t39
      t41 = t21 * S23
      t43 = t21 * S13
      t50 = S34 ** 2
      t56 = S24 ** 2
      t58 = S24 * S14
      t60 = S23 * S24
      t73 = S14 * t56
      t75 = S24 * t39
      t76 = t32 * S24
      t78 = t56 * S23
      t80 = t58 * S23
      t82 = t56 * S24
      t101 = t21 ** 2
      t102 = t56 ** 2
      t112 = t32 * S14
      t115 = t39 * S23
      t139 = t101 + t102 - 0.4D1 * t75 * S14 + 0.14D2 / 0.3D1 * t60 * t3
     #2 - 0.28D2 / 0.9D1 * t73 * S23 - 0.4D1 * t82 * S14 + t21 * t39 - 0
     #.4D1 * S13 * t112 + 0.7D1 / 0.9D1 * S13 * t115 + 0.7D1 / 0.9D1 * t
     #43 * S23 - 0.28D2 / 0.9D1 * t37 * S23 + 0.6D1 * t21 * t32 - 0.4D1 
     #* t40 * S14 + 0.14D2 / 0.3D1 * t19 * t32 + 0.7D1 / 0.9D1 * S24 * t
     #115 + 0.6D1 * t56 * t32 + t56 * t39 + 0.7D1 / 0.9D1 * t82 * S23 - 
     #0.4D1 * S24 * t112 - 0.4D1 * t43 * S14
      t141 = 0.20D2 / 0.3D1 * t37 + 0.37D2 / 0.9D1 * t41 - 0.26D2 / 0.3D
     #1 * t33 - 0.5D1 / 0.18D2 * t82 - 0.82D2 / 0.9D1 * t73 - 0.16D2 / 0
     #.3D1 * t75 - 0.14D2 / 0.9D1 * t76 - 0.91D2 / 0.18D2 * t78 - 0.100D
     #3 / 0.9D1 * t80 + 0.32D2 / 0.9D1 * t35 - 0.47D2 / 0.36D2 * t40 + t
     #43 / 0.3D1 + t139 * t12
      rrgq2qgh61J1 = ((0.32D2 / 0.9D1 * t3 - 0.32D2 / 0.9D1 * t2 * S23) 
     #* t7 * s * z - t2 * t12 * t13 + (-0.187D3 / 0.9D1 * S13 + (-0.4D1 
     #* t17 + 0.7D1 / 0.9D1 * t19 + 0.3D1 * t21) * t12) * t1 * S12 - 0.8
     #D1 / 0.3D1 * t3 + (0.8D1 / 0.3D1 * t19 - 0.4D1 / 0.9D1 * t21 - 0.2
     #20D3 / 0.9D1 * t17 + (-0.6D1 * t33 + 0.28D2 / 0.9D1 * t35 + 0.8D1 
     #* t37 - t40 - 0.14D2 / 0.9D1 * t41 - 0.3D1 * t43) * t12) * t1 + (-
     #t1 * S24 * t12 * t50 * S34 + (-0.47D2 / 0.36D2 * S13 - 0.215D3 / 0
     #.18D2 * S24 + (0.3D1 * t56 - 0.4D1 * t58 + 0.7D1 / 0.9D1 * t60) * 
     #t12) * t1 * t50 + (-0.82D2 / 0.9D1 * t56 - 0.265D3 / 0.18D2 * t60 
     #- 0.37D2 / 0.9D1 * t21 - 0.32D2 / 0.9D1 * t17 + 0.47D2 / 0.18D2 * 
     #t19 - 0.146D3 / 0.9D1 * t58 + (0.8D1 * t73 - t75 - 0.6D1 * t76 - 0
     #.14D2 / 0.9D1 * t78 + 0.28D2 / 0.9D1 * t80 - 0.3D1 * t82) * t12) *
     # t1 * S34 + t141 * t1) * t7 + (-0.4D1 / 0.9D1 * t115 * S14 * S13 -
     # 0.4D1 / 0.9D1 * S23 * t112 * S13) * t12 * t1 / t13) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh61J2
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S13 + S14 + S34
      t2 = S13 * t1
      t7 = 0.1D1 / S12
      t12 = 0.1D1 / (S12 + S13 + S23)
      t13 = S12 ** 2
      t21 = S13 * S14
      t23 = S23 * S13
      t25 = S13 ** 2
      t42 = S24 ** 2
      t46 = S23 * S24
      t48 = S24 * S14
      t50 = t21 * S23
      t52 = S14 ** 2
      t53 = t52 * S13
      t55 = S14 * t25
      t57 = S23 ** 2
      t58 = S13 * t57
      t59 = t25 * S13
      t61 = t25 * S23
      t68 = S34 ** 2
      t89 = t42 * S24
      t91 = S24 * t57
      t92 = t52 * S24
      t94 = S14 * t42
      t96 = t48 * S23
      t98 = t42 * S23
      t119 = t25 ** 2
      t120 = t42 ** 2
      t132 = t57 * S23
      t152 = t119 + t120 + 0.8D1 / 0.9D1 * t55 * S23 + 0.28D2 / 0.9D1 * 
     #t91 * S14 - 0.44D2 / 0.9D1 * t46 * t52 + 0.8D1 / 0.9D1 * t94 * S23
     # - t25 * t57 + 0.4D1 * t25 * t52 - 0.7D1 / 0.9D1 * S13 * t132 + 0.
     #7D1 / 0.9D1 * t59 * S23 - 0.44D2 / 0.9D1 * t23 * t52 - t42 * t57 -
     # 0.4D1 * t59 * S14 + 0.28D2 / 0.9D1 * t58 * S14 - 0.7D1 / 0.9D1 * 
     #S24 * t132 + 0.4D1 * t42 * t52 + 0.7D1 / 0.9D1 * t89 * S23 - 0.4D1
     # * t89 * S14
      t154 = -0.4D1 / 0.3D1 * t53 + 0.76D2 / 0.9D1 * t55 + 0.13D2 / 0.9D
     #1 * t89 - 0.17D2 / 0.9D1 * t61 + 0.7D1 / 0.36D2 * t58 + 0.7D1 / 0.
     #9D1 * t59 - 0.37D2 / 0.18D2 * t98 - 0.2D1 * t94 - 0.9D1 / 0.2D1 * 
     #t91 - 0.20D2 / 0.9D1 * t92 - 0.28D2 / 0.9D1 * t96 - 0.92D2 / 0.9D1
     # * t50 - 0.16D2 / 0.9D1 * S23 * t52 + t152 * t12
      rrgq2qgh61J2 = ((-0.32D2 / 0.9D1 * t2 * S34 + 0.32D2 / 0.9D1 * t2 
     #* S23) * t7 * s * z - t2 * t12 * t13 + (0.16D2 / 0.9D1 * t1 * S34 
     #+ (-0.13D2 / 0.3D1 * S13 - 0.34D2 / 0.9D1 * S24 - 0.16D2 / 0.9D1 *
     # S23 + (-0.4D1 * t21 + 0.7D1 / 0.9D1 * t23 + 0.3D1 * t25) * t12) *
     # t1) * S12 + (-0.64D2 / 0.9D1 * S24 + 0.136D3 / 0.9D1 * S13 + 0.32
     #D2 / 0.9D1 * S14) * t1 * S34 + (-0.136D3 / 0.9D1 * t23 - 0.32D2 / 
     #0.9D1 * S14 * S23 - 0.64D2 / 0.9D1 * t42 + 0.148D3 / 0.9D1 * t25 -
     # 0.20D2 / 0.3D1 * t21 - 0.64D2 / 0.9D1 * t46 - 0.6D1 * t48 + (-0.8
     #D1 / 0.9D1 * t50 - 0.4D1 * t53 + 0.8D1 * t55 + t58 - 0.3D1 * t59 -
     # 0.14D2 / 0.9D1 * t61) * t12) * t1 + (-t1 * S24 * t12 * t68 * S34 
     #+ (0.7D1 / 0.36D2 * S13 + 0.28D2 / 0.9D1 * S24 + (0.3D1 * t42 - 0.
     #4D1 * t48 + 0.7D1 / 0.9D1 * t46) * t12) * t1 * t68 + (0.23D2 / 0.3
     #D1 * t42 - 0.7D1 / 0.18D2 * t23 - 0.10D2 / 0.9D1 * t48 + 0.16D2 / 
     #0.9D1 * t52 - 0.43D2 / 0.18D2 * t46 + 0.17D2 / 0.9D1 * t25 + 0.92D
     #2 / 0.9D1 * t21 + (-0.3D1 * t89 + t91 - 0.4D1 * t92 + 0.8D1 * t94 
     #- 0.8D1 / 0.9D1 * t96 - 0.14D2 / 0.9D1 * t98) * t12) * t1 * S34 + 
     #t154 * t1) * t7 - 0.8D1 / 0.9D1 * t57 * t52 * S13 * t1 * t12 / t13
     #) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh61J3
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = 0.1D1 / S12
      t2 = S23 * t1
      t3 = S13 + S14 + S34
      t6 = 0.1D1 / (S12 + S13 + S23)
      t7 = s ** 2
      t10 = z ** 2
      t21 = S23 ** 2
      t22 = S23 * S13
      t30 = S13 * t3
      t31 = S12 ** 2
      t38 = 0.16D2 / 0.9D1 * S23
      t39 = S13 * S14
      t42 = S13 ** 2
      t50 = S34 ** 2
      t61 = S24 ** 2
      t65 = S23 * S24
      t67 = S24 * S14
      t70 = 0.16D2 / 0.3D1 * S14 * S23
      t71 = t39 * S23
      t73 = S14 ** 2
      t74 = t73 * S13
      t76 = S14 * t42
      t78 = t21 * S13
      t79 = t42 * S13
      t81 = t42 * S23
      t112 = t61 * S24
      t114 = S24 * t21
      t115 = t73 * S24
      t117 = S14 * t61
      t119 = t67 * S23
      t121 = t61 * S23
      t143 = t21 * S23
      t145 = t42 ** 2
      t146 = t61 ** 2
      t177 = t145 + t146 - 0.44D2 / 0.9D1 * t78 * S14 - 0.8D1 / 0.9D1 * 
     #t22 * t73 + 0.8D1 / 0.9D1 * t76 * S23 - 0.44D2 / 0.9D1 * t114 * S1
     #4 - 0.8D1 / 0.9D1 * t65 * t73 + 0.8D1 / 0.9D1 * t117 * S23 - t42 *
     # t21 + 0.4D1 * t42 * t73 + 0.29D2 / 0.9D1 * S13 * t143 + 0.7D1 / 0
     #.9D1 * t79 * S23 + 0.4D1 * t61 * t73 - t61 * t21 - 0.4D1 * t79 * S
     #14 + 0.29D2 / 0.9D1 * S24 * t143 + 0.7D1 / 0.9D1 * t112 * S23 - 0.
     #4D1 * t112 * S14
      t179 = -0.28D2 / 0.9D1 * t74 + 0.44D2 / 0.9D1 * t76 + 0.259D3 / 0.
     #36D2 * t78 - t79 + 0.89D2 / 0.9D1 * t112 + 0.5D1 / 0.3D1 * t81 + 0
     #.14315D5 / 0.18D2 * t114 - 0.28D2 / 0.9D1 * t115 + 0.14527D5 / 0.1
     #8D2 * t121 - 0.18598D5 / 0.9D1 * t117 - 0.6200D4 / 0.3D1 * t119 - 
     #0.10D2 * t71 - 0.16D2 / 0.3D1 * t73 * S23 + 0.8D1 / 0.3D1 * S14 * 
     #t21 + 0.28D2 / 0.9D1 * t143 + t177 * t6
      rrgq2qgh61J3 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t3 * S2
     #3 + (-0.32D2 / 0.3D1 * t21 - 0.32D2 / 0.3D1 * t22) * t3 * t1) * s 
     #* z - t30 * t6 * t31 + (0.16D2 / 0.9D1 * t3 * S34 + (-0.55D2 / 0.9
     #D1 * S13 - 0.34D2 / 0.9D1 * S24 + t38 + (-0.4D1 * t39 + 0.7D1 / 0.
     #9D1 * t22 + 0.3D1 * t42) * t6) * t3) * S12 + 0.8D1 / 0.9D1 * t3 * 
     #t50 + (-t38 + 0.16D2 / 0.3D1 * S14 - 0.6380D4 / 0.9D1 * S24 + 0.13
     #6D3 / 0.9D1 * S13) * t3 * S34 + (-0.92D2 / 0.9D1 * t39 - 0.8D1 * t
     #22 - 0.6388D4 / 0.9D1 * t61 + 0.8D1 * t21 + 0.116D3 / 0.9D1 * t42 
     #- 0.6376D4 / 0.9D1 * t65 - 0.74D2 / 0.9D1 * t67 - t70 + (-0.8D1 / 
     #0.9D1 * t71 - 0.4D1 * t74 + 0.8D1 * t76 + t78 - 0.3D1 * t79 - 0.14
     #D2 / 0.9D1 * t81) * t6) * t3 + ((0.4D1 / 0.9D1 - S24 * t6) * t3 * 
     #t50 * S34 + (-0.7024D4 / 0.9D1 * S24 + 0.8D1 / 0.3D1 * S14 + S13 /
     # 0.12D2 - 0.4D1 / 0.3D1 * S23 + (0.3D1 * t61 - 0.4D1 * t67 + 0.7D1
     # / 0.9D1 * t65) * t6) * t3 * t50 + (0.25D2 / 0.2D1 * t65 - t70 + 0
     #.16D2 / 0.3D1 * t73 + 0.4D1 / 0.3D1 * t21 + 0.10D2 * t39 - 0.6911D
     #4 / 0.9D1 * t61 + 0.17D2 / 0.9D1 * t42 - 0.18562D5 / 0.9D1 * t67 -
     # t22 / 0.6D1 + (-0.3D1 * t112 + t114 - 0.4D1 * t115 + 0.8D1 * t117
     # - 0.8D1 / 0.9D1 * t119 - 0.14D2 / 0.9D1 * t121) * t6) * t3 * S34 
     #+ t179 * t3) * t1 + (-0.4D1 / 0.9D1 * t30 * S14 * t50 - 0.4D1 / 0.
     #9D1 * t30 * t73 * S34 + (0.4D1 / 0.9D1 * S23 * t73 * S14 * S13 + 0
     #.4D1 / 0.9D1 * t143 * S14 * S13 - 0.16D2 / 0.9D1 * t21 * t73 * S13
     #) * t6 * t3) / t31) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh61J4
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = 0.1D1 / S12
      t2 = S23 * t1
      t3 = S13 + S14 + S34
      t6 = 0.1D1 / (S12 + S13 + S23)
      t7 = s ** 2
      t10 = z ** 2
      t21 = S23 ** 2
      t22 = S23 * S13
      t30 = S13 * t3
      t31 = S12 ** 2
      t38 = 0.16D2 / 0.9D1 * S23
      t39 = S13 * S14
      t42 = S13 ** 2
      t50 = S34 ** 2
      t61 = S24 ** 2
      t65 = S23 * S24
      t67 = S24 * S14
      t70 = 0.16D2 / 0.3D1 * S14 * S23
      t71 = t39 * S23
      t73 = S14 ** 2
      t74 = t73 * S13
      t76 = S14 * t42
      t78 = t21 * S13
      t79 = t42 * S13
      t81 = t42 * S23
      t112 = t61 * S24
      t114 = S24 * t21
      t115 = t73 * S24
      t117 = S14 * t61
      t119 = t67 * S23
      t121 = t61 * S23
      t143 = t21 * S23
      t145 = t42 ** 2
      t146 = t61 ** 2
      t177 = t145 + t146 - 0.44D2 / 0.9D1 * t78 * S14 - 0.8D1 / 0.9D1 * 
     #t22 * t73 + 0.8D1 / 0.9D1 * t76 * S23 - 0.44D2 / 0.9D1 * t114 * S1
     #4 - 0.8D1 / 0.9D1 * t65 * t73 + 0.8D1 / 0.9D1 * t117 * S23 - t42 *
     # t21 + 0.4D1 * t42 * t73 + 0.29D2 / 0.9D1 * S13 * t143 + 0.7D1 / 0
     #.9D1 * t79 * S23 + 0.4D1 * t61 * t73 - t61 * t21 - 0.4D1 * t79 * S
     #14 + 0.29D2 / 0.9D1 * S24 * t143 + 0.7D1 / 0.9D1 * t112 * S23 - 0.
     #4D1 * t112 * S14
      t179 = -0.28D2 / 0.9D1 * t74 + 0.44D2 / 0.9D1 * t76 + 0.259D3 / 0.
     #36D2 * t78 - t79 + 0.89D2 / 0.9D1 * t112 + 0.5D1 / 0.3D1 * t81 + 0
     #.14315D5 / 0.18D2 * t114 - 0.28D2 / 0.9D1 * t115 + 0.14527D5 / 0.1
     #8D2 * t121 - 0.18598D5 / 0.9D1 * t117 - 0.6200D4 / 0.3D1 * t119 - 
     #0.10D2 * t71 - 0.16D2 / 0.3D1 * t73 * S23 + 0.8D1 / 0.3D1 * S14 * 
     #t21 + 0.28D2 / 0.9D1 * t143 + t177 * t6
      rrgq2qgh61J4 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t3 * S2
     #3 + (-0.32D2 / 0.3D1 * t21 - 0.32D2 / 0.3D1 * t22) * t3 * t1) * s 
     #* z - t30 * t6 * t31 + (0.16D2 / 0.9D1 * t3 * S34 + (-0.55D2 / 0.9
     #D1 * S13 - 0.34D2 / 0.9D1 * S24 + t38 + (-0.4D1 * t39 + 0.7D1 / 0.
     #9D1 * t22 + 0.3D1 * t42) * t6) * t3) * S12 + 0.8D1 / 0.9D1 * t3 * 
     #t50 + (-t38 + 0.16D2 / 0.3D1 * S14 - 0.6380D4 / 0.9D1 * S24 + 0.13
     #6D3 / 0.9D1 * S13) * t3 * S34 + (-0.92D2 / 0.9D1 * t39 - 0.8D1 * t
     #22 - 0.6388D4 / 0.9D1 * t61 + 0.8D1 * t21 + 0.116D3 / 0.9D1 * t42 
     #- 0.6376D4 / 0.9D1 * t65 - 0.74D2 / 0.9D1 * t67 - t70 + (-0.8D1 / 
     #0.9D1 * t71 - 0.4D1 * t74 + 0.8D1 * t76 + t78 - 0.3D1 * t79 - 0.14
     #D2 / 0.9D1 * t81) * t6) * t3 + ((0.4D1 / 0.9D1 - S24 * t6) * t3 * 
     #t50 * S34 + (-0.7024D4 / 0.9D1 * S24 + 0.8D1 / 0.3D1 * S14 + S13 /
     # 0.12D2 - 0.4D1 / 0.3D1 * S23 + (0.3D1 * t61 - 0.4D1 * t67 + 0.7D1
     # / 0.9D1 * t65) * t6) * t3 * t50 + (0.25D2 / 0.2D1 * t65 - t70 + 0
     #.16D2 / 0.3D1 * t73 + 0.4D1 / 0.3D1 * t21 + 0.10D2 * t39 - 0.6911D
     #4 / 0.9D1 * t61 + 0.17D2 / 0.9D1 * t42 - 0.18562D5 / 0.9D1 * t67 -
     # t22 / 0.6D1 + (-0.3D1 * t112 + t114 - 0.4D1 * t115 + 0.8D1 * t117
     # - 0.8D1 / 0.9D1 * t119 - 0.14D2 / 0.9D1 * t121) * t6) * t3 * S34 
     #+ t179 * t3) * t1 + (-0.4D1 / 0.9D1 * t30 * S14 * t50 - 0.4D1 / 0.
     #9D1 * t30 * t73 * S34 + (0.4D1 / 0.9D1 * S23 * t73 * S14 * S13 + 0
     #.4D1 / 0.9D1 * t143 * S14 * S13 - 0.16D2 / 0.9D1 * t21 * t73 * S13
     #) * t6 * t3) / t31) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh61J5
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = 0.1D1 / S12
      t2 = S23 * t1
      t3 = S13 + S14 + S34
      t6 = 0.1D1 / (S12 + S13 + S23)
      t7 = s ** 2
      t10 = z ** 2
      t21 = S23 ** 2
      t22 = S23 * S13
      t30 = S13 * t3
      t31 = S12 ** 2
      t38 = 0.16D2 / 0.9D1 * S23
      t39 = S13 * S14
      t42 = S13 ** 2
      t50 = S34 ** 2
      t61 = S24 ** 2
      t65 = S23 * S24
      t67 = S24 * S14
      t70 = 0.16D2 / 0.3D1 * S14 * S23
      t71 = t39 * S23
      t73 = S14 ** 2
      t74 = t73 * S13
      t76 = S14 * t42
      t78 = t21 * S13
      t79 = t42 * S13
      t81 = t42 * S23
      t112 = t61 * S24
      t114 = S24 * t21
      t115 = t73 * S24
      t117 = S14 * t61
      t119 = t67 * S23
      t121 = t61 * S23
      t143 = t21 * S23
      t145 = t42 ** 2
      t146 = t61 ** 2
      t177 = t145 + t146 - 0.44D2 / 0.9D1 * t78 * S14 - 0.8D1 / 0.9D1 * 
     #t22 * t73 + 0.8D1 / 0.9D1 * t76 * S23 - 0.44D2 / 0.9D1 * t114 * S1
     #4 - 0.8D1 / 0.9D1 * t65 * t73 + 0.8D1 / 0.9D1 * t117 * S23 - t42 *
     # t21 + 0.4D1 * t42 * t73 + 0.29D2 / 0.9D1 * S13 * t143 + 0.7D1 / 0
     #.9D1 * t79 * S23 + 0.4D1 * t61 * t73 - t61 * t21 - 0.4D1 * t79 * S
     #14 + 0.29D2 / 0.9D1 * S24 * t143 + 0.7D1 / 0.9D1 * t112 * S23 - 0.
     #4D1 * t112 * S14
      t179 = -0.28D2 / 0.9D1 * t74 + 0.44D2 / 0.9D1 * t76 + 0.259D3 / 0.
     #36D2 * t78 - t79 + 0.89D2 / 0.9D1 * t112 + 0.5D1 / 0.3D1 * t81 + 0
     #.14315D5 / 0.18D2 * t114 - 0.28D2 / 0.9D1 * t115 + 0.14527D5 / 0.1
     #8D2 * t121 - 0.18598D5 / 0.9D1 * t117 - 0.6200D4 / 0.3D1 * t119 - 
     #0.10D2 * t71 - 0.16D2 / 0.3D1 * t73 * S23 + 0.8D1 / 0.3D1 * S14 * 
     #t21 + 0.28D2 / 0.9D1 * t143 + t177 * t6
      rrgq2qgh61J5 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t3 * S2
     #3 + (-0.32D2 / 0.3D1 * t21 - 0.32D2 / 0.3D1 * t22) * t3 * t1) * s 
     #* z - t30 * t6 * t31 + (0.16D2 / 0.9D1 * t3 * S34 + (-0.55D2 / 0.9
     #D1 * S13 - 0.34D2 / 0.9D1 * S24 + t38 + (-0.4D1 * t39 + 0.7D1 / 0.
     #9D1 * t22 + 0.3D1 * t42) * t6) * t3) * S12 + 0.8D1 / 0.9D1 * t3 * 
     #t50 + (-t38 + 0.16D2 / 0.3D1 * S14 - 0.6380D4 / 0.9D1 * S24 + 0.13
     #6D3 / 0.9D1 * S13) * t3 * S34 + (-0.92D2 / 0.9D1 * t39 - 0.8D1 * t
     #22 - 0.6388D4 / 0.9D1 * t61 + 0.8D1 * t21 + 0.116D3 / 0.9D1 * t42 
     #- 0.6376D4 / 0.9D1 * t65 - 0.74D2 / 0.9D1 * t67 - t70 + (-0.8D1 / 
     #0.9D1 * t71 - 0.4D1 * t74 + 0.8D1 * t76 + t78 - 0.3D1 * t79 - 0.14
     #D2 / 0.9D1 * t81) * t6) * t3 + ((0.4D1 / 0.9D1 - S24 * t6) * t3 * 
     #t50 * S34 + (-0.7024D4 / 0.9D1 * S24 + 0.8D1 / 0.3D1 * S14 + S13 /
     # 0.12D2 - 0.4D1 / 0.3D1 * S23 + (0.3D1 * t61 - 0.4D1 * t67 + 0.7D1
     # / 0.9D1 * t65) * t6) * t3 * t50 + (0.25D2 / 0.2D1 * t65 - t70 + 0
     #.16D2 / 0.3D1 * t73 + 0.4D1 / 0.3D1 * t21 + 0.10D2 * t39 - 0.6911D
     #4 / 0.9D1 * t61 + 0.17D2 / 0.9D1 * t42 - 0.18562D5 / 0.9D1 * t67 -
     # t22 / 0.6D1 + (-0.3D1 * t112 + t114 - 0.4D1 * t115 + 0.8D1 * t117
     # - 0.8D1 / 0.9D1 * t119 - 0.14D2 / 0.9D1 * t121) * t6) * t3 * S34 
     #+ t179 * t3) * t1 + (-0.4D1 / 0.9D1 * t30 * S14 * t50 - 0.4D1 / 0.
     #9D1 * t30 * t73 * S34 + (0.4D1 / 0.9D1 * S23 * t73 * S14 * S13 + 0
     #.4D1 / 0.9D1 * t143 * S14 * S13 - 0.16D2 / 0.9D1 * t21 * t73 * S13
     #) * t6 * t3) / t31) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh61J6
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = 0.1D1 / S12
      t2 = S23 * t1
      t3 = S13 + S14 + S34
      t6 = 0.1D1 / (S12 + S13 + S23)
      t7 = s ** 2
      t10 = z ** 2
      t21 = S13 * t3
      t24 = S23 ** 2
      t26 = S23 * S13
      t38 = 0.16D2 / 0.9D1 * S23
      t44 = S34 ** 2
      t55 = S13 ** 2
      t57 = S13 * S14
      t59 = S24 ** 2
      t62 = 0.16D2 / 0.3D1 * S14 * S23
      t63 = S23 * S24
      t65 = S24 * S14
      t67 = S14 ** 2
      t68 = t67 * S13
      t70 = S13 * t24
      t72 = t57 * S23
      t96 = t67 * S24
      t98 = S24 * t24
      t100 = t65 * S23
      t108 = S14 * t55
      t112 = t24 * S23
      t125 = t59 * S14
      t147 = t67 * S14
      t160 = -0.8D1 / 0.9D1 * t70 * S14 - 0.50D2 / 0.9D1 * t26 * t67 + 0
     #.4D1 * t108 * S23 - 0.8D1 / 0.9D1 * t98 * S14 - 0.50D2 / 0.9D1 * t
     #63 * t67 + 0.4D1 * t125 * S23 - 0.2D1 * t55 * t24 - 0.2D1 * t55 * 
     #t67 + 0.4D1 * S13 * t147 + 0.22D2 / 0.9D1 * S13 * t112 - 0.2D1 * t
     #59 * t67 - 0.2D1 * t59 * t24 + 0.22D2 / 0.9D1 * S24 * t112 + 0.4D1
     # * S24 * t147
      t162 = 0.50D2 / 0.9D1 * t68 - 0.16D2 / 0.9D1 * t108 + 0.8D1 / 0.3D
     #1 * t24 * S14 + 0.28D2 / 0.9D1 * t112 + 0.17D2 / 0.2D1 * t70 - 0.4
     #D1 / 0.3D1 * t55 * S13 + 0.61D2 / 0.6D1 * t59 * S24 - 0.22D2 / 0.9
     #D1 * t55 * S23 + 0.14411D5 / 0.18D2 * t98 - 0.14D2 / 0.9D1 * t96 +
     # 0.7309D4 / 0.9D1 * t59 * S23 - 0.6172D4 / 0.3D1 * t125 - 0.18500D
     #5 / 0.9D1 * t100 - 0.122D3 / 0.9D1 * t72 - 0.16D2 / 0.3D1 * t67 * 
     #S23 + t160 * t6
      t185 = S12 ** 2
      rrgq2qgh61J6 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t3 * S2
     #3 + (-0.32D2 / 0.9D1 * t21 * S34 + (-0.32D2 / 0.3D1 * t24 - 0.64D2
     # / 0.9D1 * t26) * t3) * t1) * s * z + (0.16D2 / 0.9D1 * t3 * S34 +
     # (0.44D2 / 0.3D1 * S13 + t38 - 0.34D2 / 0.9D1 * S24) * t3) * S12 +
     # 0.8D1 / 0.9D1 * t3 * t44 + (-t38 - 0.6380D4 / 0.9D1 * S24 + 0.160
     #D3 / 0.9D1 * S13 + 0.16D2 / 0.3D1 * S14) * t3 * S34 + (-0.32D2 / 0
     #.3D1 * t26 + 0.8D1 * t24 + 0.40D2 / 0.3D1 * t55 + 0.128D3 / 0.9D1 
     #* t57 - 0.6388D4 / 0.9D1 * t59 - t62 - 0.6376D4 / 0.9D1 * t63 - 0.
     #74D2 / 0.9D1 * t65 + (0.2D1 * t68 + 0.2D1 * t70 - 0.4D1 * t72) * t
     #6) * t3 + (0.4D1 / 0.9D1 * t3 * t44 * S34 + (0.25D2 / 0.18D2 * S13
     # - 0.1537D4 / 0.2D1 * S24 + 0.8D1 / 0.3D1 * S14 - 0.4D1 / 0.3D1 * 
     #S23) * t3 * t44 + (0.245D3 / 0.9D1 * t63 - 0.18416D5 / 0.9D1 * t65
     # + 0.16D2 / 0.3D1 * t67 + 0.122D3 / 0.9D1 * t57 - 0.25D2 / 0.9D1 *
     # t26 + 0.6D1 * t55 - 0.6829D4 / 0.9D1 * t59 + 0.4D1 / 0.3D1 * t24 
     #- t62 + (0.2D1 * t96 + 0.2D1 * t98 - 0.4D1 * t100) * t6) * t3 * S3
     #4 + t162 * t3) * t1 + (-0.4D1 / 0.9D1 * t21 * S14 * t44 - 0.4D1 / 
     #0.9D1 * t21 * t67 * S34 + (0.8D1 / 0.9D1 * t112 * S14 * S13 + 0.8D
     #1 / 0.9D1 * S23 * t147 * S13 - 0.16D2 / 0.9D1 * t24 * t67 * S13) *
     # t6 * t3) / t185) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh61J7
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = 0.1D1 / S12
      t2 = S23 * t1
      t3 = S13 + S14 + S34
      t6 = 0.1D1 / (S12 + S13 + S23)
      t7 = s ** 2
      t10 = z ** 2
      t21 = S13 * t3
      t24 = S23 ** 2
      t26 = S23 * S13
      t40 = S34 ** 2
      t49 = S13 ** 2
      t51 = S24 * S14
      t53 = S13 * S14
      t55 = S23 * S24
      t57 = S14 * S23
      t61 = S24 ** 2
      t78 = S14 ** 2
      t97 = S13 * t24
      t101 = t24 * S23
      t103 = S24 * t24
      t131 = -0.32D2 / 0.9D1 * S23 * t78 + 0.2D1 / 0.9D1 * t53 * S23 - 0
     #.32D2 / 0.9D1 * S14 * t49 - 0.8D1 / 0.9D1 * t78 * S24 + 0.7282D4 /
     # 0.9D1 * t61 * S23 + 0.7D1 * t97 - 0.18580D5 / 0.9D1 * S14 * t61 +
     # 0.28D2 / 0.9D1 * t101 + 0.7198D4 / 0.9D1 * t103 - 0.16D2 / 0.9D1 
     #* t49 * S13 + 0.32D2 / 0.9D1 * t49 * S23 + 0.76D2 / 0.9D1 * t61 * 
     #S24 - 0.18572D5 / 0.9D1 * t51 * S23 - 0.16D2 / 0.9D1 * t78 * S13 +
     # 0.8D1 / 0.3D1 * t24 * S14 + (0.4D1 * t26 * t78 - 0.8D1 * t97 * S1
     #4 + 0.4D1 * t55 * t78 - 0.8D1 * t103 * S14 + 0.4D1 * S13 * t101 + 
     #0.4D1 * S24 * t101) * t6
      t155 = S12 ** 2
      rrgq2qgh61J7 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t3 * S2
     #3 + (0.32D2 / 0.9D1 * t21 * S34 + (-0.32D2 / 0.3D1 * t24 - 0.128D3
     # / 0.9D1 * t26) * t3) * t1) * s * z + (-0.16D2 / 0.9D1 * S13 + 0.3
     #2D2 / 0.9D1 * S23) * t3 * S12 + 0.8D1 / 0.9D1 * t3 * t40 + (-0.631
     #6D4 / 0.9D1 * S24 + 0.16D2 / 0.9D1 * S14 - 0.16D2 / 0.9D1 * S23) *
     # t3 * S34 + (-0.32D2 / 0.9D1 * t49 - 0.20D2 / 0.9D1 * t51 - 0.32D2
     # / 0.9D1 * t53 - 0.2104D4 / 0.3D1 * t55 - 0.16D2 / 0.9D1 * t57 + 0
     #.64D2 / 0.9D1 * t26 + 0.8D1 * t24 - 0.2108D4 / 0.3D1 * t61) * t3 +
     # (0.4D1 / 0.9D1 * t3 * t40 * S34 + (-S13 / 0.9D1 - 0.7052D4 / 0.9D
     #1 * S24 + 0.8D1 / 0.3D1 * S14 - 0.4D1 / 0.3D1 * S23) * t3 * t40 + 
     #(-0.6184D4 / 0.3D1 * t51 - 0.2D1 / 0.9D1 * t53 - 0.6980D4 / 0.9D1 
     #* t61 + 0.32D2 / 0.9D1 * t78 + 0.134D3 / 0.9D1 * t55 + 0.2D1 / 0.9
     #D1 * t26 - 0.16D2 / 0.3D1 * t57 + 0.4D1 / 0.3D1 * t24) * t3 * S34 
     #+ t131 * t3) * t1 + (-0.4D1 / 0.9D1 * t21 * S14 * t40 - 0.4D1 / 0.
     #9D1 * t21 * t78 * S34 + (0.4D1 / 0.9D1 * t101 * S14 * S13 - 0.8D1 
     #/ 0.9D1 * t24 * t78 * S13 + 0.4D1 / 0.9D1 * S23 * t78 * S14 * S13)
     # * t6 * t3) / t155) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh62J1
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S13 + S14 + S34
      t2 = 0.1D1 / S12
      t3 = t1 * t2
      t6 = 0.1D1 / (S12 + S14 + S24)
      t7 = s ** 2
      t10 = z ** 2
      t19 = S13 * t6
      t20 = 0.16D2 * t19
      t29 = 0.8D1 * S23
      t30 = S13 * S14
      t31 = S23 * S13
      t33 = 0.32D2 * t30 - 0.32D2 * t31
      t39 = S34 ** 2
      t47 = S14 ** 2
      t49 = S23 ** 2
      t54 = t49 * S13
      t56 = t30 * S23
      t74 = S24 * S14
      t77 = S24 ** 2
      t79 = S23 * S24
      t81 = S13 ** 2
      t105 = t81 * S13
      t140 = S12 ** 2
      rrgq2qgh62J1 = (-0.16D2 * t3 * S13 * t6 * t7 * s * t10 * z + 0.48D
     #2 * t3 * S13 * t7 * t10 + ((-0.8D1 - t20) * t1 * S12 + (0.8D1 + 0.
     #32D2 * t19) * t1 * S34 + (-0.48D2 * S13 - t29 + t33 * t6) * t1 + (
     #(-0.4D1 - t20) * t1 * t39 + (t29 - 0.8D1 * S14 - t33 * t6) * t1 * 
     #S34 + (-0.8D1 * t47 - 0.4D1 * t49 + 0.8D1 * S14 * S23 - 0.48D2 * t
     #30 + (-0.16D2 * t54 + 0.32D2 * t56 - 0.16D2 * t47 * S13) * t6) * t
     #1) * t2) * s * z + (0.416D3 * S24 - 0.4D1 * S13) * t1 * S34 + (0.1
     #6D2 * t74 + 0.4D1 * t31 + 0.412D3 * t77 + 0.412D3 * t79 - 0.40D2 *
     # t81) * t1 + ((0.4D1 * S24 + 0.8D1 * S13) * t1 * t39 + (-0.16D2 * 
     #t31 + 0.8D1 * t79 + 0.4D1 * t77 + 0.4D1 * t30 + 0.412D3 * t74) * t
     #1 * S34 + (0.416D3 * t74 * S23 + 0.8D1 * t54 + 0.412D3 * S14 * t77
     # - 0.40D2 * S14 * t81 - 0.40D2 * t105 - 0.4D1 * t56 + 0.4D1 * S24 
     #* t49 + 0.4D1 * t77 * S23) * t1) * t2 + (-S13 * t1 * t39 * S34 + (
     #0.3D1 * t31 + 0.2D1 * t81) * t1 * t39 + (-0.4D1 * t81 * S23 - 0.3D
     #1 * t54 - 0.2D1 * t105) * t1 * S34 + (S13 * t49 * S23 + 0.2D1 * t8
     #1 * t49 + 0.2D1 * t105 * S23) * t1) / t140) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh62J2
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S13 + S14 + S34
      t2 = S13 * t1
      t4 = 0.1D1 / (S12 + S14 + S24)
      t8 = S13 * t4
      t14 = 0.16D2 * S14
      t15 = S13 * S14
      t16 = S23 * S13
      t18 = 0.32D2 * t15 - 0.32D2 * t16
      t25 = S34 ** 2
      t33 = S23 ** 2
      t37 = S13 * t33
      t39 = t15 * S23
      t41 = S14 ** 2
      t42 = t41 * S13
      t49 = 0.1D1 / S12
      t54 = 0.8D1 * S13
      t65 = S24 * S14
      t67 = S23 * S24
      t70 = S13 ** 2
      t72 = S24 ** 2
      t103 = t70 * S14
      t135 = S12 ** 2
      rrgq2qgh62J2 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (-0.16D2 * S23 + t14 + t18 * t4) * t1 + ((0.4D1 - 0.1
     #6D2 * t8) * t1 * t25 + (-0.8D1 * S23 - t14 - t18 * t4) * t1 * S34 
     #+ (0.4D1 * t33 + 0.16D2 * S14 * S23 + (-0.16D2 * t37 + 0.32D2 * t3
     #9 - 0.16D2 * t42) * t4) * t1) * t49) * s * z + (t54 - 0.12D2 * S24
     #) * t1 * S12 + (-0.2D1 * S13 - 0.16D2 * S24) * t1 * S34 + (0.2D1 *
     # t16 - 0.8D1 * t65 - 0.24D2 * t67 + 0.16D2 * t15 + 0.8D1 * t70 - 0
     #.20D2 * t72) * t1 + ((0.30D2 * S24 + t54) * t1 * t25 + (-0.16D2 * 
     #t16 + 0.60D2 * t67 - 0.24D2 * t65 + 0.2D1 * t15 - 0.56D2 * t72) * 
     #t1 * S34 + (-0.86D2 * t72 * S24 - 0.20D2 * S14 * t72 + 0.8D1 * t42
     # + 0.8D1 * t37 - 0.12D2 * t41 * S24 - 0.2D1 * t39 + 0.30D2 * S24 *
     # t33 - 0.16D2 * t65 * S23 - 0.56D2 * t72 * S23 + 0.8D1 * t103) * t
     #1) * t49 + (-t2 * t25 * S34 + (0.3D1 * t16 + 0.2D1 * t70) * t1 * t
     #25 + (-0.2D1 * t42 - 0.3D1 * t37 - 0.2D1 * t103 - 0.4D1 * t70 * S2
     #3) * t1 * S34 + (S13 * t33 * S23 + 0.2D1 * t70 * t33 + 0.2D1 * t16
     # * t41 + 0.2D1 * t103 * S23) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh62J3
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S13 + S14 + S34
      t2 = S13 * t1
      t4 = 0.1D1 / (S12 + S14 + S24)
      t8 = S13 * t4
      t14 = 0.16D2 * S14
      t15 = S13 * S14
      t16 = S23 * S13
      t18 = 0.32D2 * t15 - 0.32D2 * t16
      t25 = S34 ** 2
      t33 = S23 ** 2
      t37 = S13 * t33
      t39 = t15 * S23
      t41 = S14 ** 2
      t42 = t41 * S13
      t49 = 0.1D1 / S12
      t54 = 0.8D1 * S13
      t65 = S24 * S14
      t67 = S23 * S24
      t70 = S13 ** 2
      t72 = S24 ** 2
      t103 = t70 * S14
      t135 = S12 ** 2
      rrgq2qgh62J3 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (-0.16D2 * S23 + t14 + t18 * t4) * t1 + ((0.4D1 - 0.1
     #6D2 * t8) * t1 * t25 + (-0.8D1 * S23 - t14 - t18 * t4) * t1 * S34 
     #+ (0.4D1 * t33 + 0.16D2 * S14 * S23 + (-0.16D2 * t37 + 0.32D2 * t3
     #9 - 0.16D2 * t42) * t4) * t1) * t49) * s * z + (t54 - 0.12D2 * S24
     #) * t1 * S12 + (-0.2D1 * S13 - 0.16D2 * S24) * t1 * S34 + (0.2D1 *
     # t16 - 0.8D1 * t65 - 0.24D2 * t67 + 0.16D2 * t15 + 0.8D1 * t70 - 0
     #.20D2 * t72) * t1 + ((0.30D2 * S24 + t54) * t1 * t25 + (-0.16D2 * 
     #t16 + 0.60D2 * t67 - 0.24D2 * t65 + 0.2D1 * t15 - 0.56D2 * t72) * 
     #t1 * S34 + (-0.86D2 * t72 * S24 - 0.20D2 * S14 * t72 + 0.8D1 * t42
     # + 0.8D1 * t37 - 0.12D2 * t41 * S24 - 0.2D1 * t39 + 0.30D2 * S24 *
     # t33 - 0.16D2 * t65 * S23 - 0.56D2 * t72 * S23 + 0.8D1 * t103) * t
     #1) * t49 + (-t2 * t25 * S34 + (0.3D1 * t16 + 0.2D1 * t70) * t1 * t
     #25 + (-0.2D1 * t42 - 0.3D1 * t37 - 0.2D1 * t103 - 0.4D1 * t70 * S2
     #3) * t1 * S34 + (S13 * t33 * S23 + 0.2D1 * t70 * t33 + 0.2D1 * t16
     # * t41 + 0.2D1 * t103 * S23) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh62J4
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S13 + S14 + S34
      t2 = S13 * t1
      t4 = 0.1D1 / (S12 + S14 + S24)
      t8 = S13 * t4
      t14 = 0.16D2 * S14
      t15 = S13 * S14
      t16 = S23 * S13
      t18 = 0.32D2 * t15 - 0.32D2 * t16
      t25 = S34 ** 2
      t33 = S23 ** 2
      t37 = S13 * t33
      t39 = t15 * S23
      t41 = S14 ** 2
      t42 = t41 * S13
      t49 = 0.1D1 / S12
      t54 = 0.8D1 * S13
      t65 = S24 * S14
      t67 = S23 * S24
      t70 = S13 ** 2
      t72 = S24 ** 2
      t103 = t70 * S14
      t135 = S12 ** 2
      rrgq2qgh62J4 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (-0.16D2 * S23 + t14 + t18 * t4) * t1 + ((0.4D1 - 0.1
     #6D2 * t8) * t1 * t25 + (-0.8D1 * S23 - t14 - t18 * t4) * t1 * S34 
     #+ (0.4D1 * t33 + 0.16D2 * S14 * S23 + (-0.16D2 * t37 + 0.32D2 * t3
     #9 - 0.16D2 * t42) * t4) * t1) * t49) * s * z + (t54 - 0.12D2 * S24
     #) * t1 * S12 + (-0.2D1 * S13 - 0.16D2 * S24) * t1 * S34 + (0.2D1 *
     # t16 - 0.8D1 * t65 - 0.24D2 * t67 + 0.16D2 * t15 + 0.8D1 * t70 - 0
     #.20D2 * t72) * t1 + ((0.30D2 * S24 + t54) * t1 * t25 + (-0.16D2 * 
     #t16 + 0.60D2 * t67 - 0.24D2 * t65 + 0.2D1 * t15 - 0.56D2 * t72) * 
     #t1 * S34 + (-0.86D2 * t72 * S24 - 0.20D2 * S14 * t72 + 0.8D1 * t42
     # + 0.8D1 * t37 - 0.12D2 * t41 * S24 - 0.2D1 * t39 + 0.30D2 * S24 *
     # t33 - 0.16D2 * t65 * S23 - 0.56D2 * t72 * S23 + 0.8D1 * t103) * t
     #1) * t49 + (-t2 * t25 * S34 + (0.3D1 * t16 + 0.2D1 * t70) * t1 * t
     #25 + (-0.2D1 * t42 - 0.3D1 * t37 - 0.2D1 * t103 - 0.4D1 * t70 * S2
     #3) * t1 * S34 + (S13 * t33 * S23 + 0.2D1 * t70 * t33 + 0.2D1 * t16
     # * t41 + 0.2D1 * t103 * S23) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh62J5
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S13 + S14 + S34
      t2 = S13 * t1
      t4 = 0.1D1 / (S12 + S14 + S24)
      t8 = S13 * t4
      t14 = 0.16D2 * S14
      t15 = S13 * S14
      t16 = S23 * S13
      t18 = 0.32D2 * t15 - 0.32D2 * t16
      t25 = S34 ** 2
      t33 = S23 ** 2
      t37 = S13 * t33
      t39 = t15 * S23
      t41 = S14 ** 2
      t42 = t41 * S13
      t49 = 0.1D1 / S12
      t54 = 0.8D1 * S13
      t65 = S24 * S14
      t67 = S23 * S24
      t70 = S13 ** 2
      t72 = S24 ** 2
      t103 = t70 * S14
      t135 = S12 ** 2
      rrgq2qgh62J5 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (-0.16D2 * S23 + t14 + t18 * t4) * t1 + ((0.4D1 - 0.1
     #6D2 * t8) * t1 * t25 + (-0.8D1 * S23 - t14 - t18 * t4) * t1 * S34 
     #+ (0.4D1 * t33 + 0.16D2 * S14 * S23 + (-0.16D2 * t37 + 0.32D2 * t3
     #9 - 0.16D2 * t42) * t4) * t1) * t49) * s * z + (t54 - 0.12D2 * S24
     #) * t1 * S12 + (-0.2D1 * S13 - 0.16D2 * S24) * t1 * S34 + (0.2D1 *
     # t16 - 0.8D1 * t65 - 0.24D2 * t67 + 0.16D2 * t15 + 0.8D1 * t70 - 0
     #.20D2 * t72) * t1 + ((0.30D2 * S24 + t54) * t1 * t25 + (-0.16D2 * 
     #t16 + 0.60D2 * t67 - 0.24D2 * t65 + 0.2D1 * t15 - 0.56D2 * t72) * 
     #t1 * S34 + (-0.86D2 * t72 * S24 - 0.20D2 * S14 * t72 + 0.8D1 * t42
     # + 0.8D1 * t37 - 0.12D2 * t41 * S24 - 0.2D1 * t39 + 0.30D2 * S24 *
     # t33 - 0.16D2 * t65 * S23 - 0.56D2 * t72 * S23 + 0.8D1 * t103) * t
     #1) * t49 + (-t2 * t25 * S34 + (0.3D1 * t16 + 0.2D1 * t70) * t1 * t
     #25 + (-0.2D1 * t42 - 0.3D1 * t37 - 0.2D1 * t103 - 0.4D1 * t70 * S2
     #3) * t1 * S34 + (S13 * t33 * S23 + 0.2D1 * t70 * t33 + 0.2D1 * t16
     # * t41 + 0.2D1 * t103 * S23) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh62J6
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S13 + S14 + S34
      t2 = 0.1D1 / S12
      t3 = t1 * t2
      t7 = s ** 2
      t10 = z ** 2
      t28 = S34 ** 2
      t36 = S23 ** 2
      t38 = S13 * S14
      t40 = S14 ** 2
      t62 = S24 ** 2
      t64 = S23 * S13
      t66 = S24 * S14
      t68 = S23 * S24
      t70 = S13 ** 2
      t90 = t70 * S13
      t92 = t70 * S14
      t96 = t40 * S13
      t119 = S12 ** 2
      rrgq2qgh62J6 = (0.16D2 * t3 * S13 / (S12 + S14 + S24) * t7 * s * t
     #10 * z - 0.48D2 * t3 * S13 * t7 * t10 + (0.8D1 * t1 * S12 + 0.8D1 
     #* t1 * S34 + (-0.8D1 * S23 + 0.48D2 * S13 + 0.16D2 * S14) * t1 + (
     #0.8D1 * t1 * t28 + (-0.8D1 * S14 - 0.16D2 * S23) * t1 * S34 + (0.8
     #D1 * t36 + 0.48D2 * t38 + 0.8D1 * t40 + 0.8D1 * S14 * S23) * t1) *
     # t2) * s * z + (0.8D1 * S13 - 0.12D2 * S24) * t1 * S12 + (-0.432D3
     # * S24 + 0.2D1 * S13) * t1 * S34 + (0.16D2 * t38 - 0.432D3 * t62 -
     # 0.2D1 * t64 - 0.24D2 * t66 - 0.436D3 * t68 + 0.48D2 * t70) * t1 +
     # (0.26D2 * t1 * S24 * t28 + (-0.60D2 * t62 - 0.436D3 * t66 - 0.2D1
     # * t38 + 0.52D2 * t68) * t1 * S34 + (0.2D1 * t38 * S23 - 0.432D3 *
     # S14 * t62 - 0.432D3 * t66 * S23 + 0.40D2 * t90 + 0.48D2 * t92 - 0
     #.12D2 * t40 * S24 + 0.8D1 * t96 + 0.26D2 * S24 * t36 - 0.86D2 * t6
     #2 * S24 - 0.60D2 * t62 * S23) * t1) * t2 + ((-0.2D1 * t96 - 0.2D1 
     #* t92 + 0.2D1 * t90) * t1 * S34 + (0.2D1 * t64 * t40 + 0.2D1 * t92
     # * S23 - 0.2D1 * t90 * S23) * t1) / t119) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh63J1
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S23 + S24 + S34
      t2 = S24 * t1
      t3 = t2 * S34
      t7 = 0.1D1 / S12
      t12 = 0.1D1 / (S12 + S14 + S24)
      t13 = S12 ** 2
      t17 = S23 * S24
      t19 = S24 * S14
      t21 = S24 ** 2
      t32 = t21 * S23
      t34 = S14 ** 2
      t35 = t34 * S24
      t36 = S14 * t21
      t38 = t21 * S24
      t40 = S23 ** 2
      t41 = t40 * S24
      t43 = t19 * S23
      t50 = S34 ** 2
      t56 = S13 ** 2
      t58 = S23 * S13
      t60 = S13 * S14
      t73 = t34 * S13
      t74 = t56 * S13
      t76 = t60 * S23
      t78 = S14 * t56
      t80 = t56 * S23
      t82 = t40 * S13
      t101 = t56 ** 2
      t102 = t21 ** 2
      t110 = t40 * S23
      t125 = t34 * S14
      t139 = t101 + t102 - 0.4D1 * t74 * S23 + 0.6D1 * t56 * t40 + t56 *
     # t34 + 0.7D1 / 0.9D1 * t74 * S14 - 0.4D1 * S24 * t110 + 0.14D2 / 0
     #.3D1 * t82 * S14 - 0.4D1 * t58 * t34 - 0.28D2 / 0.9D1 * S23 * t78 
     #+ 0.14D2 / 0.3D1 * t41 * S14 - 0.4D1 * t17 * t34 - 0.28D2 / 0.9D1 
     #* t36 * S23 + 0.7D1 / 0.9D1 * S13 * t125 - 0.4D1 * S13 * t110 - 0.
     #4D1 * t38 * S23 + 0.7D1 / 0.9D1 * S24 * t125 + t21 * t34 + 0.6D1 *
     # t21 * t40 + 0.7D1 / 0.9D1 * t38 * S14
      t141 = -0.47D2 / 0.36D2 * t35 - 0.5D1 / 0.18D2 * t74 - 0.100D3 / 0
     #.9D1 * t76 + 0.32D2 / 0.9D1 * t43 - 0.16D2 / 0.3D1 * t73 + t38 / 0
     #.3D1 + 0.20D2 / 0.3D1 * t32 + 0.37D2 / 0.9D1 * t36 - 0.14D2 / 0.9D
     #1 * t82 - 0.26D2 / 0.3D1 * t41 - 0.91D2 / 0.18D2 * t78 - 0.82D2 / 
     #0.9D1 * t80 + t139 * t12
      rrgq2qgh63J1 = ((0.32D2 / 0.9D1 * t3 - 0.32D2 / 0.9D1 * t2 * S14) 
     #* t7 * s * z - t2 * t12 * t13 + (-0.187D3 / 0.9D1 * S24 + (-0.4D1 
     #* t17 + 0.7D1 / 0.9D1 * t19 + 0.3D1 * t21) * t12) * t1 * S12 - 0.8
     #D1 / 0.3D1 * t3 + (-0.4D1 / 0.9D1 * t21 + 0.8D1 / 0.3D1 * t19 - 0.
     #220D3 / 0.9D1 * t17 + (0.8D1 * t32 - t35 - 0.14D2 / 0.9D1 * t36 - 
     #0.3D1 * t38 - 0.6D1 * t41 + 0.28D2 / 0.9D1 * t43) * t12) * t1 + (-
     #S13 * t1 * t12 * t50 * S34 + (-0.47D2 / 0.36D2 * S24 - 0.215D3 / 0
     #.18D2 * S13 + (0.3D1 * t56 - 0.4D1 * t58 + 0.7D1 / 0.9D1 * t60) * 
     #t12) * t1 * t50 + (-0.37D2 / 0.9D1 * t21 - 0.265D3 / 0.18D2 * t60 
     #- 0.32D2 / 0.9D1 * t17 - 0.146D3 / 0.9D1 * t58 + 0.47D2 / 0.18D2 *
     # t19 - 0.82D2 / 0.9D1 * t56 + (-t73 - 0.3D1 * t74 + 0.28D2 / 0.9D1
     # * t76 - 0.14D2 / 0.9D1 * t78 + 0.8D1 * t80 - 0.6D1 * t82) * t12) 
     #* t1 * S34 + t141 * t1) * t7 + (-0.4D1 / 0.9D1 * t125 * S23 * S24 
     #- 0.4D1 / 0.9D1 * S14 * t110 * S24) * t12 * t1 / t13) / pi * wd / 
     #z

      end function
  
   
 

      doubleprecision function rrgq2qgh63J2
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S23 + S24 + S34
      t2 = S24 * t1
      t7 = 0.1D1 / S12
      t12 = 0.1D1 / (S12 + S14 + S24)
      t13 = S12 ** 2
      t21 = S23 * S24
      t23 = S24 * S14
      t25 = S24 ** 2
      t39 = S23 * S13
      t42 = S13 * S14
      t45 = S13 ** 2
      t50 = t25 * S24
      t52 = t23 * S23
      t54 = S23 ** 2
      t55 = S24 * t54
      t57 = S14 ** 2
      t58 = t57 * S24
      t59 = t25 * S23
      t61 = S14 * t25
      t68 = S34 ** 2
      t89 = S14 * t45
      t91 = t57 * S13
      t92 = t42 * S23
      t94 = t45 * S23
      t96 = t45 * S13
      t98 = S13 * t54
      t119 = t45 ** 2
      t120 = t25 ** 2
      t128 = t57 * S14
      t152 = t119 + t120 - 0.4D1 * t96 * S23 + 0.4D1 * t45 * t54 - t45 *
     # t57 + 0.7D1 / 0.9D1 * t96 * S14 - 0.7D1 / 0.9D1 * S24 * t128 - t2
     #5 * t57 - 0.44D2 / 0.9D1 * t98 * S14 + 0.28D2 / 0.9D1 * t39 * t57 
     #+ 0.8D1 / 0.9D1 * t89 * S23 - 0.44D2 / 0.9D1 * S14 * t55 + 0.28D2 
     #/ 0.9D1 * t21 * t57 + 0.8D1 / 0.9D1 * t61 * S23 - 0.7D1 / 0.9D1 * 
     #S13 * t128 + 0.7D1 / 0.9D1 * t50 * S14 + 0.4D1 * t25 * t54 - 0.4D1
     # * t50 * S23
      t154 = 0.7D1 / 0.36D2 * t58 - 0.16D2 / 0.9D1 * t54 * S14 - 0.92D2 
     #/ 0.9D1 * t52 - 0.9D1 / 0.2D1 * t91 + 0.13D2 / 0.9D1 * t96 - 0.28D
     #2 / 0.9D1 * t92 + 0.7D1 / 0.9D1 * t50 + 0.76D2 / 0.9D1 * t59 - 0.1
     #7D2 / 0.9D1 * t61 - 0.20D2 / 0.9D1 * t98 - 0.4D1 / 0.3D1 * t55 - 0
     #.37D2 / 0.18D2 * t89 - 0.2D1 * t94 + t152 * t12
      rrgq2qgh63J2 = ((-0.32D2 / 0.9D1 * t2 * S34 + 0.32D2 / 0.9D1 * t2 
     #* S14) * t7 * s * z - t2 * t12 * t13 + (0.16D2 / 0.9D1 * t1 * S34 
     #+ (-0.34D2 / 0.9D1 * S13 - 0.13D2 / 0.3D1 * S24 - 0.16D2 / 0.9D1 *
     # S14 + (-0.4D1 * t21 + 0.7D1 / 0.9D1 * t23 + 0.3D1 * t25) * t12) *
     # t1) * S12 + (-0.64D2 / 0.9D1 * S13 + 0.136D3 / 0.9D1 * S24 + 0.32
     #D2 / 0.9D1 * S23) * t1 * S34 + (-0.6D1 * t39 + 0.148D3 / 0.9D1 * t
     #25 - 0.64D2 / 0.9D1 * t42 - 0.20D2 / 0.3D1 * t21 - 0.64D2 / 0.9D1 
     #* t45 - 0.136D3 / 0.9D1 * t23 - 0.32D2 / 0.9D1 * S14 * S23 + (-0.3
     #D1 * t50 - 0.8D1 / 0.9D1 * t52 - 0.4D1 * t55 + t58 + 0.8D1 * t59 -
     # 0.14D2 / 0.9D1 * t61) * t12) * t1 + (-S13 * t1 * t12 * t68 * S34 
     #+ (0.28D2 / 0.9D1 * S13 + 0.7D1 / 0.36D2 * S24 + (0.3D1 * t45 - 0.
     #4D1 * t39 + 0.7D1 / 0.9D1 * t42) * t12) * t1 * t68 + (0.92D2 / 0.9
     #D1 * t21 - 0.43D2 / 0.18D2 * t42 + 0.16D2 / 0.9D1 * t54 - 0.10D2 /
     # 0.9D1 * t39 - 0.7D1 / 0.18D2 * t23 + 0.23D2 / 0.3D1 * t45 + 0.17D
     #2 / 0.9D1 * t25 + (-0.14D2 / 0.9D1 * t89 + t91 - 0.8D1 / 0.9D1 * t
     #92 + 0.8D1 * t94 - 0.3D1 * t96 - 0.4D1 * t98) * t12) * t1 * S34 + 
     #t154 * t1) * t7 - 0.8D1 / 0.9D1 * t57 * t54 * S24 * t1 * t12 / t13
     #) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh63J3
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = 0.1D1 / S12
      t2 = S14 * t1
      t3 = S23 + S24 + S34
      t6 = 0.1D1 / (S12 + S14 + S24)
      t7 = s ** 2
      t10 = z ** 2
      t21 = S24 * S14
      t22 = S14 ** 2
      t30 = S24 * t3
      t31 = S12 ** 2
      t37 = 0.16D2 / 0.9D1 * S14
      t39 = S23 * S24
      t42 = S24 ** 2
      t50 = S34 ** 2
      t59 = S13 ** 2
      t62 = S13 * S14
      t65 = S23 * S13
      t70 = 0.16D2 / 0.3D1 * S14 * S23
      t71 = t42 * S24
      t73 = t21 * S23
      t75 = S23 ** 2
      t76 = S24 * t75
      t78 = t22 * S24
      t79 = t42 * S23
      t81 = S14 * t42
      t112 = t59 * S14
      t114 = t22 * S13
      t115 = t62 * S23
      t117 = t59 * S23
      t119 = t59 * S13
      t121 = S13 * t75
      t134 = t22 * S14
      t145 = t59 ** 2
      t146 = t42 ** 2
      t177 = t145 + t146 + 0.8D1 / 0.9D1 * t81 * S23 + 0.29D2 / 0.9D1 * 
     #S13 * t134 - 0.4D1 * t119 * S23 + 0.4D1 * t59 * t75 - t59 * t22 + 
     #0.7D1 / 0.9D1 * t119 * S14 + 0.29D2 / 0.9D1 * S24 * t134 - t42 * t
     #22 - 0.8D1 / 0.9D1 * t121 * S14 - 0.44D2 / 0.9D1 * t65 * t22 + 0.8
     #D1 / 0.9D1 * t112 * S23 - 0.8D1 / 0.9D1 * t76 * S14 - 0.44D2 / 0.9
     #D1 * t39 * t22 + 0.4D1 * t42 * t75 - 0.4D1 * t71 * S23 + 0.7D1 / 0
     #.9D1 * t71 * S14
      t179 = 0.89D2 / 0.9D1 * t119 - 0.16D2 / 0.3D1 * t75 * S14 + 0.259D
     #3 / 0.36D2 * t78 + 0.14315D5 / 0.18D2 * t114 - 0.10D2 * t73 + 0.28
     #D2 / 0.9D1 * t134 + 0.8D1 / 0.3D1 * S23 * t22 - 0.6200D4 / 0.3D1 *
     # t115 - t71 + 0.44D2 / 0.9D1 * t79 + 0.5D1 / 0.3D1 * t81 - 0.28D2 
     #/ 0.9D1 * t121 - 0.28D2 / 0.9D1 * t76 + 0.14527D5 / 0.18D2 * t112 
     #- 0.18598D5 / 0.9D1 * t117 + t177 * t6
      rrgq2qgh63J3 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t3 * S1
     #4 + (-0.32D2 / 0.3D1 * t21 - 0.32D2 / 0.3D1 * t22) * t3 * t1) * s 
     #* z - t30 * t6 * t31 + (0.16D2 / 0.9D1 * t3 * S34 + (-0.34D2 / 0.9
     #D1 * S13 + t37 - 0.55D2 / 0.9D1 * S24 + (-0.4D1 * t39 + 0.7D1 / 0.
     #9D1 * t21 + 0.3D1 * t42) * t6) * t3) * S12 + 0.8D1 / 0.9D1 * t3 * 
     #t50 + (0.16D2 / 0.3D1 * S23 - 0.6380D4 / 0.9D1 * S13 + 0.136D3 / 0
     #.9D1 * S24 - t37) * t3 * S34 + (-0.6388D4 / 0.9D1 * t59 + 0.8D1 * 
     #t22 - 0.6376D4 / 0.9D1 * t62 - 0.92D2 / 0.9D1 * t39 - 0.74D2 / 0.9
     #D1 * t65 + 0.116D3 / 0.9D1 * t42 - 0.8D1 * t21 - t70 + (-0.3D1 * t
     #71 - 0.8D1 / 0.9D1 * t73 - 0.4D1 * t76 + t78 + 0.8D1 * t79 - 0.14D
     #2 / 0.9D1 * t81) * t6) * t3 + ((0.4D1 / 0.9D1 - S13 * t6) * t3 * t
     #50 * S34 + (-0.7024D4 / 0.9D1 * S13 + S24 / 0.12D2 + 0.8D1 / 0.3D1
     # * S23 - 0.4D1 / 0.3D1 * S14 + (0.3D1 * t59 - 0.4D1 * t65 + 0.7D1 
     #/ 0.9D1 * t62) * t6) * t3 * t50 + (0.10D2 * t39 - 0.6911D4 / 0.9D1
     # * t59 + 0.4D1 / 0.3D1 * t22 + 0.25D2 / 0.2D1 * t62 - t70 - 0.1856
     #2D5 / 0.9D1 * t65 + 0.16D2 / 0.3D1 * t75 + 0.17D2 / 0.9D1 * t42 - 
     #t21 / 0.6D1 + (-0.14D2 / 0.9D1 * t112 + t114 - 0.8D1 / 0.9D1 * t11
     #5 + 0.8D1 * t117 - 0.3D1 * t119 - 0.4D1 * t121) * t6) * t3 * S34 +
     # t179 * t3) * t1 + (-0.4D1 / 0.9D1 * t30 * S23 * t50 - 0.4D1 / 0.9
     #D1 * t30 * t75 * S34 + (0.4D1 / 0.9D1 * t134 * S23 * S24 - 0.16D2 
     #/ 0.9D1 * t22 * t75 * S24 + 0.4D1 / 0.9D1 * S14 * t75 * S23 * S24)
     # * t6 * t3) / t31) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh63J4
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = 0.1D1 / S12
      t2 = S14 * t1
      t3 = S23 + S24 + S34
      t6 = 0.1D1 / (S12 + S14 + S24)
      t7 = s ** 2
      t10 = z ** 2
      t21 = S24 * S14
      t22 = S14 ** 2
      t30 = S24 * t3
      t31 = S12 ** 2
      t37 = 0.16D2 / 0.9D1 * S14
      t39 = S23 * S24
      t42 = S24 ** 2
      t50 = S34 ** 2
      t59 = S13 ** 2
      t62 = S13 * S14
      t65 = S23 * S13
      t70 = 0.16D2 / 0.3D1 * S14 * S23
      t71 = t42 * S24
      t73 = t21 * S23
      t75 = S23 ** 2
      t76 = S24 * t75
      t78 = t22 * S24
      t79 = t42 * S23
      t81 = S14 * t42
      t112 = t59 * S14
      t114 = t22 * S13
      t115 = t62 * S23
      t117 = t59 * S23
      t119 = t59 * S13
      t121 = S13 * t75
      t134 = t22 * S14
      t145 = t59 ** 2
      t146 = t42 ** 2
      t177 = t145 + t146 + 0.8D1 / 0.9D1 * t81 * S23 + 0.29D2 / 0.9D1 * 
     #S13 * t134 - 0.4D1 * t119 * S23 + 0.4D1 * t59 * t75 - t59 * t22 + 
     #0.7D1 / 0.9D1 * t119 * S14 + 0.29D2 / 0.9D1 * S24 * t134 - t42 * t
     #22 - 0.8D1 / 0.9D1 * t121 * S14 - 0.44D2 / 0.9D1 * t65 * t22 + 0.8
     #D1 / 0.9D1 * t112 * S23 - 0.8D1 / 0.9D1 * t76 * S14 - 0.44D2 / 0.9
     #D1 * t39 * t22 + 0.4D1 * t42 * t75 - 0.4D1 * t71 * S23 + 0.7D1 / 0
     #.9D1 * t71 * S14
      t179 = 0.89D2 / 0.9D1 * t119 - 0.16D2 / 0.3D1 * t75 * S14 + 0.259D
     #3 / 0.36D2 * t78 + 0.14315D5 / 0.18D2 * t114 - 0.10D2 * t73 + 0.28
     #D2 / 0.9D1 * t134 + 0.8D1 / 0.3D1 * S23 * t22 - 0.6200D4 / 0.3D1 *
     # t115 - t71 + 0.44D2 / 0.9D1 * t79 + 0.5D1 / 0.3D1 * t81 - 0.28D2 
     #/ 0.9D1 * t121 - 0.28D2 / 0.9D1 * t76 + 0.14527D5 / 0.18D2 * t112 
     #- 0.18598D5 / 0.9D1 * t117 + t177 * t6
      rrgq2qgh63J4 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t3 * S1
     #4 + (-0.32D2 / 0.3D1 * t21 - 0.32D2 / 0.3D1 * t22) * t3 * t1) * s 
     #* z - t30 * t6 * t31 + (0.16D2 / 0.9D1 * t3 * S34 + (-0.34D2 / 0.9
     #D1 * S13 + t37 - 0.55D2 / 0.9D1 * S24 + (-0.4D1 * t39 + 0.7D1 / 0.
     #9D1 * t21 + 0.3D1 * t42) * t6) * t3) * S12 + 0.8D1 / 0.9D1 * t3 * 
     #t50 + (0.16D2 / 0.3D1 * S23 - 0.6380D4 / 0.9D1 * S13 + 0.136D3 / 0
     #.9D1 * S24 - t37) * t3 * S34 + (-0.6388D4 / 0.9D1 * t59 + 0.8D1 * 
     #t22 - 0.6376D4 / 0.9D1 * t62 - 0.92D2 / 0.9D1 * t39 - 0.74D2 / 0.9
     #D1 * t65 + 0.116D3 / 0.9D1 * t42 - 0.8D1 * t21 - t70 + (-0.3D1 * t
     #71 - 0.8D1 / 0.9D1 * t73 - 0.4D1 * t76 + t78 + 0.8D1 * t79 - 0.14D
     #2 / 0.9D1 * t81) * t6) * t3 + ((0.4D1 / 0.9D1 - S13 * t6) * t3 * t
     #50 * S34 + (-0.7024D4 / 0.9D1 * S13 + S24 / 0.12D2 + 0.8D1 / 0.3D1
     # * S23 - 0.4D1 / 0.3D1 * S14 + (0.3D1 * t59 - 0.4D1 * t65 + 0.7D1 
     #/ 0.9D1 * t62) * t6) * t3 * t50 + (0.10D2 * t39 - 0.6911D4 / 0.9D1
     # * t59 + 0.4D1 / 0.3D1 * t22 + 0.25D2 / 0.2D1 * t62 - t70 - 0.1856
     #2D5 / 0.9D1 * t65 + 0.16D2 / 0.3D1 * t75 + 0.17D2 / 0.9D1 * t42 - 
     #t21 / 0.6D1 + (-0.14D2 / 0.9D1 * t112 + t114 - 0.8D1 / 0.9D1 * t11
     #5 + 0.8D1 * t117 - 0.3D1 * t119 - 0.4D1 * t121) * t6) * t3 * S34 +
     # t179 * t3) * t1 + (-0.4D1 / 0.9D1 * t30 * S23 * t50 - 0.4D1 / 0.9
     #D1 * t30 * t75 * S34 + (0.4D1 / 0.9D1 * t134 * S23 * S24 - 0.16D2 
     #/ 0.9D1 * t22 * t75 * S24 + 0.4D1 / 0.9D1 * S14 * t75 * S23 * S24)
     # * t6 * t3) / t31) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh63J5
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = 0.1D1 / S12
      t2 = S14 * t1
      t3 = S23 + S24 + S34
      t6 = 0.1D1 / (S12 + S14 + S24)
      t7 = s ** 2
      t10 = z ** 2
      t21 = S24 * S14
      t22 = S14 ** 2
      t30 = S24 * t3
      t31 = S12 ** 2
      t37 = 0.16D2 / 0.9D1 * S14
      t39 = S23 * S24
      t42 = S24 ** 2
      t50 = S34 ** 2
      t59 = S13 ** 2
      t62 = S13 * S14
      t65 = S23 * S13
      t70 = 0.16D2 / 0.3D1 * S14 * S23
      t71 = t42 * S24
      t73 = t21 * S23
      t75 = S23 ** 2
      t76 = S24 * t75
      t78 = t22 * S24
      t79 = t42 * S23
      t81 = S14 * t42
      t112 = t59 * S14
      t114 = t22 * S13
      t115 = t62 * S23
      t117 = t59 * S23
      t119 = t59 * S13
      t121 = S13 * t75
      t134 = t22 * S14
      t145 = t59 ** 2
      t146 = t42 ** 2
      t177 = t145 + t146 + 0.8D1 / 0.9D1 * t81 * S23 + 0.29D2 / 0.9D1 * 
     #S13 * t134 - 0.4D1 * t119 * S23 + 0.4D1 * t59 * t75 - t59 * t22 + 
     #0.7D1 / 0.9D1 * t119 * S14 + 0.29D2 / 0.9D1 * S24 * t134 - t42 * t
     #22 - 0.8D1 / 0.9D1 * t121 * S14 - 0.44D2 / 0.9D1 * t65 * t22 + 0.8
     #D1 / 0.9D1 * t112 * S23 - 0.8D1 / 0.9D1 * t76 * S14 - 0.44D2 / 0.9
     #D1 * t39 * t22 + 0.4D1 * t42 * t75 - 0.4D1 * t71 * S23 + 0.7D1 / 0
     #.9D1 * t71 * S14
      t179 = 0.89D2 / 0.9D1 * t119 - 0.16D2 / 0.3D1 * t75 * S14 + 0.259D
     #3 / 0.36D2 * t78 + 0.14315D5 / 0.18D2 * t114 - 0.10D2 * t73 + 0.28
     #D2 / 0.9D1 * t134 + 0.8D1 / 0.3D1 * S23 * t22 - 0.6200D4 / 0.3D1 *
     # t115 - t71 + 0.44D2 / 0.9D1 * t79 + 0.5D1 / 0.3D1 * t81 - 0.28D2 
     #/ 0.9D1 * t121 - 0.28D2 / 0.9D1 * t76 + 0.14527D5 / 0.18D2 * t112 
     #- 0.18598D5 / 0.9D1 * t117 + t177 * t6
      rrgq2qgh63J5 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t3 * S1
     #4 + (-0.32D2 / 0.3D1 * t21 - 0.32D2 / 0.3D1 * t22) * t3 * t1) * s 
     #* z - t30 * t6 * t31 + (0.16D2 / 0.9D1 * t3 * S34 + (-0.34D2 / 0.9
     #D1 * S13 + t37 - 0.55D2 / 0.9D1 * S24 + (-0.4D1 * t39 + 0.7D1 / 0.
     #9D1 * t21 + 0.3D1 * t42) * t6) * t3) * S12 + 0.8D1 / 0.9D1 * t3 * 
     #t50 + (0.16D2 / 0.3D1 * S23 - 0.6380D4 / 0.9D1 * S13 + 0.136D3 / 0
     #.9D1 * S24 - t37) * t3 * S34 + (-0.6388D4 / 0.9D1 * t59 + 0.8D1 * 
     #t22 - 0.6376D4 / 0.9D1 * t62 - 0.92D2 / 0.9D1 * t39 - 0.74D2 / 0.9
     #D1 * t65 + 0.116D3 / 0.9D1 * t42 - 0.8D1 * t21 - t70 + (-0.3D1 * t
     #71 - 0.8D1 / 0.9D1 * t73 - 0.4D1 * t76 + t78 + 0.8D1 * t79 - 0.14D
     #2 / 0.9D1 * t81) * t6) * t3 + ((0.4D1 / 0.9D1 - S13 * t6) * t3 * t
     #50 * S34 + (-0.7024D4 / 0.9D1 * S13 + S24 / 0.12D2 + 0.8D1 / 0.3D1
     # * S23 - 0.4D1 / 0.3D1 * S14 + (0.3D1 * t59 - 0.4D1 * t65 + 0.7D1 
     #/ 0.9D1 * t62) * t6) * t3 * t50 + (0.10D2 * t39 - 0.6911D4 / 0.9D1
     # * t59 + 0.4D1 / 0.3D1 * t22 + 0.25D2 / 0.2D1 * t62 - t70 - 0.1856
     #2D5 / 0.9D1 * t65 + 0.16D2 / 0.3D1 * t75 + 0.17D2 / 0.9D1 * t42 - 
     #t21 / 0.6D1 + (-0.14D2 / 0.9D1 * t112 + t114 - 0.8D1 / 0.9D1 * t11
     #5 + 0.8D1 * t117 - 0.3D1 * t119 - 0.4D1 * t121) * t6) * t3 * S34 +
     # t179 * t3) * t1 + (-0.4D1 / 0.9D1 * t30 * S23 * t50 - 0.4D1 / 0.9
     #D1 * t30 * t75 * S34 + (0.4D1 / 0.9D1 * t134 * S23 * S24 - 0.16D2 
     #/ 0.9D1 * t22 * t75 * S24 + 0.4D1 / 0.9D1 * S14 * t75 * S23 * S24)
     # * t6 * t3) / t31) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh63J6
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = 0.1D1 / S12
      t2 = S14 * t1
      t3 = S23 + S24 + S34
      t6 = 0.1D1 / (S12 + S14 + S24)
      t7 = s ** 2
      t10 = z ** 2
      t21 = S24 * t3
      t24 = S14 ** 2
      t26 = S24 * S14
      t37 = 0.16D2 / 0.9D1 * S14
      t44 = S34 ** 2
      t53 = S13 * S14
      t55 = S13 ** 2
      t58 = S23 * S13
      t60 = S23 * S24
      t63 = 0.16D2 / 0.3D1 * S14 * S23
      t64 = S24 ** 2
      t67 = S24 * t24
      t69 = S23 ** 2
      t70 = S24 * t69
      t72 = t26 * S23
      t96 = S13 * t24
      t98 = S13 * t69
      t100 = t53 * S23
      t117 = t24 * S14
      t123 = S14 * t64
      t127 = S14 * t55
      t135 = t69 * S23
      t160 = 0.4D1 * t123 * S23 + 0.22D2 / 0.9D1 * S13 * t117 + 0.4D1 * 
     #S13 * t135 - 0.2D1 * t55 * t69 - 0.2D1 * t55 * t24 + 0.4D1 * S24 *
     # t135 + 0.22D2 / 0.9D1 * S24 * t117 - 0.2D1 * t64 * t24 - 0.50D2 /
     # 0.9D1 * t98 * S14 - 0.8D1 / 0.9D1 * t58 * t24 + 0.4D1 * t127 * S2
     #3 - 0.50D2 / 0.9D1 * t70 * S14 - 0.8D1 / 0.9D1 * t60 * t24 - 0.2D1
     # * t64 * t69
      t162 = 0.17D2 / 0.2D1 * t67 + 0.14411D5 / 0.18D2 * t96 + 0.61D2 / 
     #0.6D1 * t55 * S13 - 0.16D2 / 0.3D1 * t69 * S14 + 0.8D1 / 0.3D1 * t
     #24 * S23 - 0.18500D5 / 0.9D1 * t100 - 0.122D3 / 0.9D1 * t72 + 0.28
     #D2 / 0.9D1 * t117 - 0.4D1 / 0.3D1 * t64 * S24 - 0.16D2 / 0.9D1 * t
     #64 * S23 - 0.22D2 / 0.9D1 * t123 - 0.14D2 / 0.9D1 * t98 + 0.50D2 /
     # 0.9D1 * t70 + 0.7309D4 / 0.9D1 * t127 - 0.6172D4 / 0.3D1 * t55 * 
     #S23 + t160 * t6
      t185 = S12 ** 2
      rrgq2qgh63J6 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t3 * S1
     #4 + (-0.32D2 / 0.9D1 * t21 * S34 + (-0.32D2 / 0.3D1 * t24 - 0.64D2
     # / 0.9D1 * t26) * t3) * t1) * s * z + (0.16D2 / 0.9D1 * t3 * S34 +
     # (t37 + 0.44D2 / 0.3D1 * S24 - 0.34D2 / 0.9D1 * S13) * t3) * S12 +
     # 0.8D1 / 0.9D1 * t3 * t44 + (-0.6380D4 / 0.9D1 * S13 - t37 + 0.160
     #D3 / 0.9D1 * S24 + 0.16D2 / 0.3D1 * S23) * t3 * S34 + (-0.6376D4 /
     # 0.9D1 * t53 - 0.6388D4 / 0.9D1 * t55 + 0.8D1 * t24 - 0.74D2 / 0.9
     #D1 * t58 + 0.128D3 / 0.9D1 * t60 - t63 + 0.40D2 / 0.3D1 * t64 - 0.
     #32D2 / 0.3D1 * t26 + (0.2D1 * t67 + 0.2D1 * t70 - 0.4D1 * t72) * t
     #6) * t3 + (0.4D1 / 0.9D1 * t3 * t44 * S34 + (-0.1537D4 / 0.2D1 * S
     #13 + 0.25D2 / 0.18D2 * S24 - 0.4D1 / 0.3D1 * S14 + 0.8D1 / 0.3D1 *
     # S23) * t3 * t44 + (0.4D1 / 0.3D1 * t24 - 0.6829D4 / 0.9D1 * t55 +
     # 0.16D2 / 0.3D1 * t69 + 0.6D1 * t64 - 0.25D2 / 0.9D1 * t26 + 0.245
     #D3 / 0.9D1 * t53 - 0.18416D5 / 0.9D1 * t58 + 0.122D3 / 0.9D1 * t60
     # - t63 + (0.2D1 * t96 + 0.2D1 * t98 - 0.4D1 * t100) * t6) * t3 * S
     #34 + t162 * t3) * t1 + (-0.4D1 / 0.9D1 * t21 * S23 * t44 - 0.4D1 /
     # 0.9D1 * t21 * t69 * S34 + (0.8D1 / 0.9D1 * t117 * S23 * S24 + 0.8
     #D1 / 0.9D1 * S14 * t135 * S24 - 0.16D2 / 0.9D1 * t24 * t69 * S24) 
     #* t6 * t3) / t185) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh63J7
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = 0.1D1 / S12
      t2 = S14 * t1
      t3 = S23 + S24 + S34
      t6 = 0.1D1 / (S12 + S14 + S24)
      t7 = s ** 2
      t10 = z ** 2
      t21 = S24 * t3
      t24 = S14 ** 2
      t26 = S24 * S14
      t40 = S34 ** 2
      t49 = S23 * S13
      t52 = S13 * S14
      t54 = S13 ** 2
      t57 = S14 * S23
      t59 = S24 ** 2
      t61 = S23 * S24
      t75 = S23 ** 2
      t107 = S13 * t75
      t113 = t24 * S14
      t115 = S24 * t75
      t131 = -0.18572D5 / 0.9D1 * t52 * S23 + 0.7198D4 / 0.9D1 * S13 * t
     #24 + 0.8D1 / 0.3D1 * t24 * S23 + 0.7282D4 / 0.9D1 * t54 * S14 - 0.
     #16D2 / 0.9D1 * t59 * S24 + 0.76D2 / 0.9D1 * S13 * t54 - 0.18580D5 
     #/ 0.9D1 * t54 * S23 - 0.32D2 / 0.9D1 * t59 * S23 + 0.32D2 / 0.9D1 
     #* t59 * S14 + 0.7D1 * S24 * t24 - 0.8D1 / 0.9D1 * t107 - 0.32D2 / 
     #0.9D1 * t75 * S14 + 0.2D1 / 0.9D1 * t26 * S23 + 0.28D2 / 0.9D1 * t
     #113 - 0.16D2 / 0.9D1 * t115 + (-0.8D1 * t61 * t24 + 0.4D1 * S24 * 
     #t113 - 0.8D1 * t49 * t24 + 0.4D1 * t107 * S14 + 0.4D1 * t115 * S14
     # + 0.4D1 * S13 * t113) * t6
      t155 = S12 ** 2
      rrgq2qgh63J7 = (-0.32D2 / 0.9D1 * t2 * t3 * t6 * t7 * s * t10 * z 
     #+ 0.32D2 / 0.3D1 * t2 * t3 * t7 * t10 + (-0.32D2 / 0.3D1 * t3 * S1
     #4 + (0.32D2 / 0.9D1 * t21 * S34 + (-0.32D2 / 0.3D1 * t24 - 0.128D3
     # / 0.9D1 * t26) * t3) * t1) * s * z + (0.32D2 / 0.9D1 * S14 - 0.16
     #D2 / 0.9D1 * S24) * t3 * S12 + 0.8D1 / 0.9D1 * t3 * t40 + (0.16D2 
     #/ 0.9D1 * S23 - 0.6316D4 / 0.9D1 * S13 - 0.16D2 / 0.9D1 * S14) * t
     #3 * S34 + (-0.20D2 / 0.9D1 * t49 + 0.8D1 * t24 - 0.2104D4 / 0.3D1 
     #* t52 - 0.2108D4 / 0.3D1 * t54 + 0.64D2 / 0.9D1 * t26 - 0.16D2 / 0
     #.9D1 * t57 - 0.32D2 / 0.9D1 * t59 - 0.32D2 / 0.9D1 * t61) * t3 + (
     #0.4D1 / 0.9D1 * t3 * t40 * S34 + (-0.7052D4 / 0.9D1 * S13 - 0.4D1 
     #/ 0.3D1 * S14 - S24 / 0.9D1 + 0.8D1 / 0.3D1 * S23) * t3 * t40 + (0
     #.32D2 / 0.9D1 * t75 + 0.2D1 / 0.9D1 * t26 + 0.134D3 / 0.9D1 * t52 
     #- 0.16D2 / 0.3D1 * t57 + 0.4D1 / 0.3D1 * t24 - 0.6980D4 / 0.9D1 * 
     #t54 - 0.2D1 / 0.9D1 * t61 - 0.6184D4 / 0.3D1 * t49) * t3 * S34 + t
     #131 * t3) * t1 + (-0.4D1 / 0.9D1 * t21 * S23 * t40 - 0.4D1 / 0.9D1
     # * t21 * t75 * S34 + (-0.8D1 / 0.9D1 * t24 * t75 * S24 + 0.4D1 / 0
     #.9D1 * t113 * S23 * S24 + 0.4D1 / 0.9D1 * S14 * t75 * S23 * S24) *
     # t6 * t3) / t155) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh64J1
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S23 + S24 + S34
      t2 = 0.1D1 / S12
      t3 = t1 * t2
      t6 = 0.1D1 / (S12 + S13 + S23)
      t7 = s ** 2
      t10 = z ** 2
      t19 = S24 * t6
      t20 = 0.16D2 * t19
      t28 = 0.8D1 * S14
      t30 = S23 * S24
      t31 = S24 * S14
      t33 = 0.32D2 * t30 - 0.32D2 * t31
      t39 = S34 ** 2
      t50 = S23 ** 2
      t52 = S14 ** 2
      t56 = t31 * S23
      t58 = t52 * S24
      t75 = S23 * S13
      t77 = S13 ** 2
      t79 = S13 * S14
      t81 = S24 ** 2
      t98 = t81 * S24
      t140 = S12 ** 2
      rrgq2qgh64J1 = (-0.16D2 * t3 * S24 * t6 * t7 * s * t10 * z + 0.48D
     #2 * t3 * S24 * t7 * t10 + ((-0.8D1 - t20) * t1 * S12 + (0.8D1 + 0.
     #32D2 * t19) * t1 * S34 + (-t28 - 0.48D2 * S24 + t33 * t6) * t1 + (
     #(-0.4D1 - t20) * t1 * t39 + (t28 - 0.8D1 * S23 - t33 * t6) * t1 * 
     #S34 + (-0.48D2 * t30 + 0.8D1 * S14 * S23 - 0.8D1 * t50 - 0.4D1 * t
     #52 + (-0.16D2 * S24 * t50 + 0.32D2 * t56 - 0.16D2 * t58) * t6) * t
     #1) * t2) * s * z + (0.416D3 * S13 - 0.4D1 * S24) * t1 * S34 + (0.4
     #D1 * t31 + 0.16D2 * t75 + 0.412D3 * t77 + 0.412D3 * t79 - 0.40D2 *
     # t81) * t1 + ((0.4D1 * S13 + 0.8D1 * S24) * t1 * t39 + (-0.16D2 * 
     #t31 + 0.4D1 * t30 + 0.8D1 * t79 + 0.412D3 * t75 + 0.4D1 * t77) * t
     #1 * S34 + (-0.40D2 * t98 + 0.8D1 * t58 - 0.4D1 * t56 + 0.416D3 * t
     #79 * S23 - 0.40D2 * t81 * S23 + 0.412D3 * t77 * S23 + 0.4D1 * t52 
     #* S13 + 0.4D1 * S14 * t77) * t1) * t2 + (-S24 * t1 * t39 * S34 + (
     #0.3D1 * t31 + 0.2D1 * t81) * t1 * t39 + (-0.2D1 * t98 - 0.3D1 * t5
     #8 - 0.4D1 * S14 * t81) * t1 * S34 + (S24 * t52 * S14 + 0.2D1 * t98
     # * S14 + 0.2D1 * t81 * t52) * t1) / t140) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh64J2
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S23 + S24 + S34
      t2 = S24 * t1
      t4 = 0.1D1 / (S12 + S13 + S23)
      t8 = S24 * t4
      t14 = 0.16D2 * S23
      t15 = S23 * S24
      t16 = S24 * S14
      t18 = 0.32D2 * t15 - 0.32D2 * t16
      t25 = S34 ** 2
      t35 = S14 ** 2
      t37 = S23 ** 2
      t38 = S24 * t37
      t40 = t16 * S23
      t42 = t35 * S24
      t49 = 0.1D1 / S12
      t55 = 0.8D1 * S24
      t64 = S24 ** 2
      t68 = S13 * S14
      t70 = S23 * S13
      t72 = S13 ** 2
      t97 = t64 * S23
      t119 = S14 * t64
      t135 = S12 ** 2
      rrgq2qgh64J2 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (-0.16D2 * S14 + t14 + t18 * t4) * t1 + ((0.4D1 - 0.1
     #6D2 * t8) * t1 * t25 + (-0.8D1 * S14 - t14 - t18 * t4) * t1 * S34 
     #+ (0.16D2 * S14 * S23 + 0.4D1 * t35 + (-0.16D2 * t38 + 0.32D2 * t4
     #0 - 0.16D2 * t42) * t4) * t1) * t49) * s * z + (-0.12D2 * S13 + t5
     #5) * t1 * S12 + (-0.2D1 * S24 - 0.16D2 * S13) * t1 * S34 + (0.8D1 
     #* t64 + 0.16D2 * t15 + 0.2D1 * t16 - 0.24D2 * t68 - 0.8D1 * t70 - 
     #0.20D2 * t72) * t1 + ((0.30D2 * S13 + t55) * t1 * t25 + (0.2D1 * t
     #15 - 0.56D2 * t72 - 0.16D2 * t16 + 0.60D2 * t68 - 0.24D2 * t70) * 
     #t1 * S34 + (0.8D1 * t38 - 0.20D2 * t72 * S23 - 0.2D1 * t40 - 0.12D
     #2 * S13 * t37 + 0.30D2 * t35 * S13 + 0.8D1 * t42 + 0.8D1 * t97 - 0
     #.56D2 * S14 * t72 - 0.86D2 * t72 * S13 - 0.16D2 * t68 * S23) * t1)
     # * t49 + (-t2 * t25 * S34 + (0.2D1 * t64 + 0.3D1 * t16) * t1 * t25
     # + (-0.3D1 * t42 - 0.2D1 * t97 - 0.2D1 * t38 - 0.4D1 * t119) * t1 
     #* S34 + (0.2D1 * t64 * t35 + S24 * t35 * S14 + 0.2D1 * t38 * S14 +
     # 0.2D1 * t119 * S23) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh64J3
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S23 + S24 + S34
      t2 = S24 * t1
      t4 = 0.1D1 / (S12 + S13 + S23)
      t8 = S24 * t4
      t14 = 0.16D2 * S23
      t15 = S23 * S24
      t16 = S24 * S14
      t18 = 0.32D2 * t15 - 0.32D2 * t16
      t25 = S34 ** 2
      t35 = S14 ** 2
      t37 = S23 ** 2
      t38 = S24 * t37
      t40 = t16 * S23
      t42 = t35 * S24
      t49 = 0.1D1 / S12
      t55 = 0.8D1 * S24
      t64 = S24 ** 2
      t68 = S13 * S14
      t70 = S23 * S13
      t72 = S13 ** 2
      t97 = t64 * S23
      t119 = S14 * t64
      t135 = S12 ** 2
      rrgq2qgh64J3 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (-0.16D2 * S14 + t14 + t18 * t4) * t1 + ((0.4D1 - 0.1
     #6D2 * t8) * t1 * t25 + (-0.8D1 * S14 - t14 - t18 * t4) * t1 * S34 
     #+ (0.16D2 * S14 * S23 + 0.4D1 * t35 + (-0.16D2 * t38 + 0.32D2 * t4
     #0 - 0.16D2 * t42) * t4) * t1) * t49) * s * z + (-0.12D2 * S13 + t5
     #5) * t1 * S12 + (-0.2D1 * S24 - 0.16D2 * S13) * t1 * S34 + (0.8D1 
     #* t64 + 0.16D2 * t15 + 0.2D1 * t16 - 0.24D2 * t68 - 0.8D1 * t70 - 
     #0.20D2 * t72) * t1 + ((0.30D2 * S13 + t55) * t1 * t25 + (0.2D1 * t
     #15 - 0.56D2 * t72 - 0.16D2 * t16 + 0.60D2 * t68 - 0.24D2 * t70) * 
     #t1 * S34 + (0.8D1 * t38 - 0.20D2 * t72 * S23 - 0.2D1 * t40 - 0.12D
     #2 * S13 * t37 + 0.30D2 * t35 * S13 + 0.8D1 * t42 + 0.8D1 * t97 - 0
     #.56D2 * S14 * t72 - 0.86D2 * t72 * S13 - 0.16D2 * t68 * S23) * t1)
     # * t49 + (-t2 * t25 * S34 + (0.2D1 * t64 + 0.3D1 * t16) * t1 * t25
     # + (-0.3D1 * t42 - 0.2D1 * t97 - 0.2D1 * t38 - 0.4D1 * t119) * t1 
     #* S34 + (0.2D1 * t64 * t35 + S24 * t35 * S14 + 0.2D1 * t38 * S14 +
     # 0.2D1 * t119 * S23) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh64J4
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S23 + S24 + S34
      t2 = S24 * t1
      t4 = 0.1D1 / (S12 + S13 + S23)
      t8 = S24 * t4
      t14 = 0.16D2 * S23
      t15 = S23 * S24
      t16 = S24 * S14
      t18 = 0.32D2 * t15 - 0.32D2 * t16
      t25 = S34 ** 2
      t35 = S14 ** 2
      t37 = S23 ** 2
      t38 = S24 * t37
      t40 = t16 * S23
      t42 = t35 * S24
      t49 = 0.1D1 / S12
      t55 = 0.8D1 * S24
      t64 = S24 ** 2
      t68 = S13 * S14
      t70 = S23 * S13
      t72 = S13 ** 2
      t97 = t64 * S23
      t119 = S14 * t64
      t135 = S12 ** 2
      rrgq2qgh64J4 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (-0.16D2 * S14 + t14 + t18 * t4) * t1 + ((0.4D1 - 0.1
     #6D2 * t8) * t1 * t25 + (-0.8D1 * S14 - t14 - t18 * t4) * t1 * S34 
     #+ (0.16D2 * S14 * S23 + 0.4D1 * t35 + (-0.16D2 * t38 + 0.32D2 * t4
     #0 - 0.16D2 * t42) * t4) * t1) * t49) * s * z + (-0.12D2 * S13 + t5
     #5) * t1 * S12 + (-0.2D1 * S24 - 0.16D2 * S13) * t1 * S34 + (0.8D1 
     #* t64 + 0.16D2 * t15 + 0.2D1 * t16 - 0.24D2 * t68 - 0.8D1 * t70 - 
     #0.20D2 * t72) * t1 + ((0.30D2 * S13 + t55) * t1 * t25 + (0.2D1 * t
     #15 - 0.56D2 * t72 - 0.16D2 * t16 + 0.60D2 * t68 - 0.24D2 * t70) * 
     #t1 * S34 + (0.8D1 * t38 - 0.20D2 * t72 * S23 - 0.2D1 * t40 - 0.12D
     #2 * S13 * t37 + 0.30D2 * t35 * S13 + 0.8D1 * t42 + 0.8D1 * t97 - 0
     #.56D2 * S14 * t72 - 0.86D2 * t72 * S13 - 0.16D2 * t68 * S23) * t1)
     # * t49 + (-t2 * t25 * S34 + (0.2D1 * t64 + 0.3D1 * t16) * t1 * t25
     # + (-0.3D1 * t42 - 0.2D1 * t97 - 0.2D1 * t38 - 0.4D1 * t119) * t1 
     #* S34 + (0.2D1 * t64 * t35 + S24 * t35 * S14 + 0.2D1 * t38 * S14 +
     # 0.2D1 * t119 * S23) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh64J5
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S23 + S24 + S34
      t2 = S24 * t1
      t4 = 0.1D1 / (S12 + S13 + S23)
      t8 = S24 * t4
      t14 = 0.16D2 * S23
      t15 = S23 * S24
      t16 = S24 * S14
      t18 = 0.32D2 * t15 - 0.32D2 * t16
      t25 = S34 ** 2
      t35 = S14 ** 2
      t37 = S23 ** 2
      t38 = S24 * t37
      t40 = t16 * S23
      t42 = t35 * S24
      t49 = 0.1D1 / S12
      t55 = 0.8D1 * S24
      t64 = S24 ** 2
      t68 = S13 * S14
      t70 = S23 * S13
      t72 = S13 ** 2
      t97 = t64 * S23
      t119 = S14 * t64
      t135 = S12 ** 2
      rrgq2qgh64J5 = ((-0.16D2 * t2 * t4 * S12 + (0.16D2 + 0.32D2 * t8) 
     #* t1 * S34 + (-0.16D2 * S14 + t14 + t18 * t4) * t1 + ((0.4D1 - 0.1
     #6D2 * t8) * t1 * t25 + (-0.8D1 * S14 - t14 - t18 * t4) * t1 * S34 
     #+ (0.16D2 * S14 * S23 + 0.4D1 * t35 + (-0.16D2 * t38 + 0.32D2 * t4
     #0 - 0.16D2 * t42) * t4) * t1) * t49) * s * z + (-0.12D2 * S13 + t5
     #5) * t1 * S12 + (-0.2D1 * S24 - 0.16D2 * S13) * t1 * S34 + (0.8D1 
     #* t64 + 0.16D2 * t15 + 0.2D1 * t16 - 0.24D2 * t68 - 0.8D1 * t70 - 
     #0.20D2 * t72) * t1 + ((0.30D2 * S13 + t55) * t1 * t25 + (0.2D1 * t
     #15 - 0.56D2 * t72 - 0.16D2 * t16 + 0.60D2 * t68 - 0.24D2 * t70) * 
     #t1 * S34 + (0.8D1 * t38 - 0.20D2 * t72 * S23 - 0.2D1 * t40 - 0.12D
     #2 * S13 * t37 + 0.30D2 * t35 * S13 + 0.8D1 * t42 + 0.8D1 * t97 - 0
     #.56D2 * S14 * t72 - 0.86D2 * t72 * S13 - 0.16D2 * t68 * S23) * t1)
     # * t49 + (-t2 * t25 * S34 + (0.2D1 * t64 + 0.3D1 * t16) * t1 * t25
     # + (-0.3D1 * t42 - 0.2D1 * t97 - 0.2D1 * t38 - 0.4D1 * t119) * t1 
     #* S34 + (0.2D1 * t64 * t35 + S24 * t35 * S14 + 0.2D1 * t38 * S14 +
     # 0.2D1 * t119 * S23) * t1) / t135) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh64J6
     &(s, XB1, XB2, z, lh, wd, nf, S12, S13, S14, S23, S24, S34) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision S12
      doubleprecision S13
      doubleprecision S14
      doubleprecision S23
      doubleprecision S24
      doubleprecision S34

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = S23 + S24 + S34
      t2 = 0.1D1 / S12
      t3 = t1 * t2
      t7 = s ** 2
      t10 = z ** 2
      t28 = S34 ** 2
      t36 = S23 * S24
      t38 = S14 ** 2
      t42 = S23 ** 2
      t61 = S24 ** 2
      t63 = S24 * S14
      t65 = S13 ** 2
      t68 = S23 * S13
      t70 = S13 * S14
      t92 = t61 * S24
      t94 = t42 * S24
      t100 = t61 * S23
      t120 = S12 ** 2
      rrgq2qgh64J6 = (0.16D2 * t3 * S24 / (S12 + S13 + S23) * t7 * s * t
     #10 * z - 0.48D2 * t3 * S24 * t7 * t10 + (0.8D1 * t1 * S12 + 0.8D1 
     #* t1 * S34 + (0.16D2 * S23 - 0.8D1 * S14 + 0.48D2 * S24) * t1 + (0
     #.8D1 * t1 * t28 + (-0.8D1 * S23 - 0.16D2 * S14) * t1 * S34 + (0.48
     #D2 * t36 + 0.8D1 * t38 + 0.8D1 * S14 * S23 + 0.8D1 * t42) * t1) * 
     #t2) * s * z + (-0.12D2 * S13 + 0.8D1 * S24) * t1 * S12 + (0.2D1 * 
     #S24 - 0.432D3 * S13) * t1 * S34 + (0.48D2 * t61 - 0.2D1 * t63 - 0.
     #432D3 * t65 + 0.16D2 * t36 - 0.24D2 * t68 - 0.436D3 * t70) * t1 + 
     #(0.26D2 * S13 * t1 * t28 + (0.52D2 * t70 - 0.436D3 * t68 - 0.60D2 
     #* t65 - 0.2D1 * t36) * t1 * S34 + (0.26D2 * t38 * S13 + 0.2D1 * t6
     #3 * S23 - 0.432D3 * t70 * S23 - 0.86D2 * t65 * S13 + 0.40D2 * t92 
     #+ 0.8D1 * t94 - 0.12D2 * S13 * t42 - 0.60D2 * S14 * t65 + 0.48D2 *
     # t100 - 0.432D3 * t65 * S23) * t1) * t2 + ((0.2D1 * t92 - 0.2D1 * 
     #t94 - 0.2D1 * t100) * t1 * S34 + (-0.2D1 * t92 * S14 + 0.2D1 * t94
     # * S14 + 0.2D1 * t61 * S14 * S23) * t1) / t120) / pi * wd / z

      end function
  
 