  
      subroutine rrgq2qght8
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgq2qgh81J1  
      doubleprecision rrgq2qgh81J2  
      doubleprecision rrgq2qgh81J3  
      doubleprecision rrgq2qgh81J4  
      doubleprecision rrgq2qgh81J5  
      doubleprecision rrgq2qgh81J6  
      doubleprecision rrgq2qgh81J7  
      doubleprecision rrgq2qgh82J1  
      doubleprecision rrgq2qgh82J2  
      doubleprecision rrgq2qgh82J3  
      doubleprecision rrgq2qgh82J4  
      doubleprecision rrgq2qgh82J5  
      doubleprecision rrgq2qgh82J6  
      doubleprecision rrgq2qgh83J1  
      doubleprecision rrgq2qgh83J2  
      doubleprecision rrgq2qgh83J3  
      doubleprecision rrgq2qgh83J4  
      doubleprecision rrgq2qgh83J5  
      doubleprecision rrgq2qgh83J6  
      doubleprecision rrgq2qgh84J1  
      doubleprecision rrgq2qgh84J2  
      doubleprecision rrgq2qgh84J3  
      doubleprecision rrgq2qgh84J4  
      doubleprecision rrgq2qgh84J5  
      doubleprecision rrgq2qgh84J6  
      doubleprecision rrgq2qgh84J7  
      doubleprecision rrgq2qght8s1e1  
      doubleprecision rrgq2qght8s1e0  
      doubleprecision rrgq2qght8s1em1  
      doubleprecision rrgq2qght8s1em2  
      doubleprecision rrgq2qght8s1em3  
      doubleprecision rrgq2qght8s1em4  
      doubleprecision rrgq2qght8s2e1  
      doubleprecision rrgq2qght8s2e0  
      doubleprecision rrgq2qght8s2em1  
      doubleprecision rrgq2qght8s2em2  
      doubleprecision rrgq2qght8s2em3  
      doubleprecision rrgq2qght8s2em4  
      doubleprecision rrgq2qght8s3e1  
      doubleprecision rrgq2qght8s3e0  
      doubleprecision rrgq2qght8s3em1  
      doubleprecision rrgq2qght8s3em2  
      doubleprecision rrgq2qght8s3em3  
      doubleprecision rrgq2qght8s3em4  
      doubleprecision rrgq2qght8s4e1  
      doubleprecision rrgq2qght8s4e0  
      doubleprecision rrgq2qght8s4em1  
      doubleprecision rrgq2qght8s4em2  
      doubleprecision rrgq2qght8s4em3  
      doubleprecision rrgq2qght8s4em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght8s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght8s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght8s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght8s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgq2qght8s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght8s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght8s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght8s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgq2qght8s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght8s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght8s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght8s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgq2qght8s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght8s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght8s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght8s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgq2qght8s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght8s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght8s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght8s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgq2qght8s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgq2qght8s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgq2qght8s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgq2qght8s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgq2qght8s1e1
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t11 = lh * t4
      t12 = pi * t6
      t13 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t14 = t12 * t13
      t17 = lh ** 2
      t19 = pi ** 2
      t21 = 0.180D3 * t17 - 0.30D2 * t19
      t22 = t21 * t4
      t23 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t24 = t12 * t23
      t25 = t22 * t24
      t27 = x4 * pi
      t28 = Sin(t27)
      t29 = t28 ** 2
      t30 = x3 * t29
      t31 = z ** 2
      t32 = 0.1D1 / t31
      t33 = t1 ** 2
      t34 = t33 ** 2
      t35 = t32 * t34
      t36 = -0.1D1 + x3
      t37 = 0.1D1 / t36
      t38 = t35 * t37
      t41 = log(-0.4D1 * t30 * t38)
      t43 = cos(t27)
      t45 = Sqrt(-x3 * t36)
      t50 = 0.1D1 / (-z - x3 + 0.2D1 * t43 * t45 * z)
      t54 = log(0.4D1 * t30 * t35)
      t55 = -t41 * z * t50 - t54
      t58 = t54 ** 2
      t60 = t41 ** 2
      t65 = -t58 * t54 / 0.6D1 - t60 * t41 * z * t50 / 0.6D1
      t70 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t79 = 0.60D2 * lh * t19 - 0.240D3 * zeta3 - 0.120D3 * t17 * lh
      t80 = t79 * t4
      t81 = t80 * t24
      t87 = z * t50 + 0.1D1
      t98 = t60 * z * t50 / 0.2D1 + t58 / 0.2D1
      t101 = 0.1D1 / x3
      t104 = rrgq2qgh83J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t106 = t19 ** 2
      t107 = t17 ** 2
      t113 = t106 + 0.60D2 * t107 + 0.480D3 * lh * zeta3 - 0.60D2 * t17 
     #* t19
      t115 = t32 * t29
      t116 = t115 * t34
      t118 = log(0.4D1 * t116)
      t119 = t118 ** 2
      t120 = t119 * t118
      t143 = t119 ** 2
      t150 = z * t7
      t151 = x1 ** 2
      t152 = x3 * t151
      t153 = t152 * t29
      t156 = log(-0.4D1 * t153 * t38)
      t157 = t156 * z
      t159 = t156 ** 2
      t160 = t159 * z
      t167 = log(0.4D1 * t152 * t116)
      t169 = t167 ** 2
      t176 = z * t13
      t187 = z * t23 * t50 + t23
      t188 = t12 * t187
      t192 = 0.1D1 / x1
      t195 = t151 * t29
      t198 = log(0.4D1 * t195 * t35)
      t203 = t198 ** 2
      t206 = t203 * t198
      t224 = x2 ** 2
      t225 = x3 * t224
      t226 = t225 * t151
      t231 = log(-0.4D1 * t226 * t115 * t34 * t37)
      t232 = t231 * z
      t238 = log(0.4D1 * t226 * t116)
      t248 = 0.1D1 / x2
      t249 = t248 * t192
      t252 = t224 * t151
      t255 = log(0.4D1 * t252 * t116)
      t257 = t255 ** 2
      t275 = log(0.4D1 * t225 * t116)
      t276 = t275 ** 2
      t280 = t225 * t29
      t283 = log(-0.4D1 * t280 * t38)
      t284 = t283 * z
      t286 = t283 ** 2
      t287 = t286 * z
      t311 = t224 * t29
      t314 = log(0.4D1 * t311 * t35)
      t319 = t314 ** 2
      t322 = t319 * t314
      t340 = -((-0.90D2 * t5 * t6 * t7 + 0.180D3 * t11 * t14 - t25) * t5
     #5 - 0.90D2 * t5 * t6 * t23 * t65 + (-t22 * t14 - 0.90D2 * t5 * t6 
     #* t70 - t81 + 0.180D3 * t11 * t12 * t7) * t87 + (-0.90D2 * t5 * t6
     # * t13 + 0.180D3 * t11 * t24) * t98) * t101 / 0.2880D4 - (-0.90D2 
     #* t104 - t23 * t113 + 0.15D2 * t120 * t13 - 0.180D3 * (-t119 * t13
     # / 0.2D1 - t70 + t120 * t23 / 0.6D1 + t118 * t7) * lh - 0.45D2 * t
     #119 * t7 + (t118 * t13 - t7 - t119 * t23 / 0.2D1) * t21 + 0.90D2 *
     # t118 * t70 + (-t13 + t118 * t23) * t79 - 0.15D2 / 0.4D1 * t143 * 
     #t23) * t6 * t5 / 0.2880D4 + (0.90D2 * t5 * t6 * (-(-t150 + t157 * 
     #t13 - t160 * t23 / 0.2D1) * t50 - t167 * t13 + t7 + t169 * t23 / 0
     #.2D1) - 0.180D3 * t11 * t12 * (-(-t176 + t157 * t23) * t50 + t13 -
     # t167 * t23) + t22 * t188) * t101 * t192 / 0.1440D4 + (t22 * t12 *
     # (t13 - t198 * t23) + 0.90D2 * t5 * t6 * (t203 * t13 / 0.2D1 + t70
     # - t206 * t23 / 0.6D1 - t198 * t7) + t81 - 0.180D3 * t11 * t12 * (
     #t7 + t203 * t23 / 0.2D1 - t198 * t13)) * t192 / 0.1440D4 + (0.90D2
     # * t5 * t6 * (-(-t176 + t232 * t23) * t50 - t238 * t23 + t13) - 0.
     #180D3 * t11 * t188) * t101 * t249 / 0.720D3 + (0.90D2 * t5 * t6 * 
     #(-t255 * t13 + t7 + t257 * t23 / 0.2D1) - 0.180D3 * t11 * t12 * (t
     #13 - t255 * t23) + t25) * t248 * t192 / 0.720D3 - (0.90D2 * t5 * t
     #6 * (-t276 * t23 / 0.2D1 + t275 * t13 - t7 + (-t150 + t284 * t13 -
     # t287 * t23 / 0.2D1) * t50) - 0.180D3 * t11 * t12 * (t275 * t23 - 
     #t13 + (-t176 + t284 * t23) * t50) - t22 * t12 * t187) * t101 * t24
     #8 / 0.1440D4 - (t22 * t12 * (-t13 + t314 * t23) + 0.90D2 * t5 * t6
     # * (-t319 * t13 / 0.2D1 - t70 + t322 * t23 / 0.6D1 + t314 * t7) - 
     #t81 - 0.180D3 * t11 * t12 * (-t7 - t319 * t23 / 0.2D1 + t314 * t13
     #)) * t248 / 0.1440D4
      t341 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t340)
      t343 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t347 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t348 = t12 * t347
      t351 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t352 = t12 * t351
      t353 = t22 * t352
      t361 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t365 = t80 * t352
      t399 = rrgq2qgh81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t412 = z * t343
      t425 = z * t347
      t436 = z * t351 * t50 + t351
      t437 = t12 * t436
      t546 = -((-0.90D2 * t5 * t6 * t343 + 0.180D3 * t11 * t348 - t353) 
     #* t55 - 0.90D2 * t5 * t6 * t351 * t65 + (-t22 * t348 - 0.90D2 * t5
     # * t6 * t361 - t365 + 0.180D3 * t11 * t12 * t343) * t87 + (-0.90D2
     # * t5 * t6 * t347 + 0.180D3 * t11 * t352) * t98) * t101 / 0.2880D4
     # - (-0.180D3 * (-t119 * t347 / 0.2D1 + t120 * t351 / 0.6D1 - t361 
     #+ t118 * t343) * lh + (-t119 * t351 / 0.2D1 - t343 + t118 * t347) 
     #* t21 + 0.90D2 * t118 * t361 + (-t347 + t118 * t351) * t79 - 0.90D
     #2 * t399 - 0.15D2 / 0.4D1 * t143 * t351 - 0.45D2 * t119 * t343 - t
     #351 * t113 + 0.15D2 * t120 * t347) * t6 * t5 / 0.2880D4 + (0.90D2 
     #* t5 * t6 * (-(-t412 + t157 * t347 - t160 * t351 / 0.2D1) * t50 + 
     #t169 * t351 / 0.2D1 + t343 - t167 * t347) - 0.180D3 * t11 * t12 * 
     #(t347 - (-t425 + t157 * t351) * t50 - t167 * t351) + t22 * t437) *
     # t101 * t192 / 0.1440D4 + (t22 * t12 * (t347 - t198 * t351) + 0.90
     #D2 * t5 * t6 * (t203 * t347 / 0.2D1 - t206 * t351 / 0.6D1 + t361 -
     # t198 * t343) + t365 - 0.180D3 * t11 * t12 * (-t198 * t347 + t203 
     #* t351 / 0.2D1 + t343)) * t192 / 0.1440D4 + (0.90D2 * t5 * t6 * (-
     #(-t425 + t232 * t351) * t50 - t238 * t351 + t347) - 0.180D3 * t11 
     #* t437) * t101 * t249 / 0.720D3 + (0.90D2 * t5 * t6 * (t257 * t351
     # / 0.2D1 + t343 - t255 * t347) - 0.180D3 * t11 * t12 * (t347 - t25
     #5 * t351) + t353) * t248 * t192 / 0.720D3 - (0.90D2 * t5 * t6 * (-
     #t276 * t351 / 0.2D1 - t343 + t275 * t347 + (-t412 + t284 * t347 - 
     #t287 * t351 / 0.2D1) * t50) - 0.180D3 * t11 * t12 * (-t347 + t275 
     #* t351 + (-t425 + t284 * t351) * t50) - t22 * t12 * t436) * t101 *
     # t248 / 0.1440D4 - (t22 * t12 * (-t347 + t314 * t351) + 0.90D2 * t
     #5 * t6 * (-t319 * t347 / 0.2D1 + t322 * t351 / 0.6D1 - t361 + t314
     # * t343) - t365 - 0.180D3 * t11 * t12 * (t314 * t347 - t319 * t351
     # / 0.2D1 - t343)) * t248 / 0.1440D4
      t547 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t546)
      t549 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t553 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t554 = t12 * t553
      t557 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t558 = t12 * t557
      t559 = t22 * t558
      t567 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t571 = t80 * t558
      t587 = rrgq2qgh84J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t620 = z * t549
      t632 = z * t553
      t642 = z * t557 * t50 + t557
      t643 = t12 * t642
      t752 = -((-0.90D2 * t5 * t6 * t549 + 0.180D3 * t11 * t554 - t559) 
     #* t55 - 0.90D2 * t5 * t6 * t557 * t65 + (-t22 * t554 - 0.90D2 * t5
     # * t6 * t567 - t571 + 0.180D3 * t11 * t12 * t549) * t87 + (-0.90D2
     # * t5 * t6 * t553 + 0.180D3 * t11 * t558) * t98) * t101 / 0.2880D4
     # - (-0.90D2 * t587 - 0.45D2 * t119 * t549 + 0.90D2 * t118 * t567 +
     # (-t549 - t119 * t557 / 0.2D1 + t118 * t553) * t21 + (-t553 + t118
     # * t557) * t79 - 0.180D3 * (-t567 - t119 * t553 / 0.2D1 + t118 * t
     #549 + t120 * t557 / 0.6D1) * lh - 0.15D2 / 0.4D1 * t143 * t557 + 0
     #.15D2 * t120 * t553 - t557 * t113) * t6 * t5 / 0.2880D4 + (0.90D2 
     #* t5 * t6 * (t169 * t557 / 0.2D1 - (-t620 + t157 * t553 - t160 * t
     #557 / 0.2D1) * t50 - t167 * t553 + t549) - 0.180D3 * t11 * t12 * (
     #-t167 * t557 + t553 - (-t632 + t157 * t557) * t50) + t22 * t643) *
     # t101 * t192 / 0.1440D4 + (t22 * t12 * (-t198 * t557 + t553) + 0.9
     #0D2 * t5 * t6 * (t567 + t203 * t553 / 0.2D1 - t198 * t549 - t206 *
     # t557 / 0.6D1) + t571 - 0.180D3 * t11 * t12 * (t203 * t557 / 0.2D1
     # + t549 - t198 * t553)) * t192 / 0.1440D4 + (0.90D2 * t5 * t6 * (t
     #553 - (-t632 + t232 * t557) * t50 - t238 * t557) - 0.180D3 * t11 *
     # t643) * t101 * t249 / 0.720D3 + (0.90D2 * t5 * t6 * (t549 + t257 
     #* t557 / 0.2D1 - t255 * t553) - 0.180D3 * t11 * t12 * (t553 - t255
     # * t557) + t559) * t248 * t192 / 0.720D3 - (0.90D2 * t5 * t6 * ((-
     #t620 + t284 * t553 - t287 * t557 / 0.2D1) * t50 + t275 * t553 - t2
     #76 * t557 / 0.2D1 - t549) - 0.180D3 * t11 * t12 * (t275 * t557 - t
     #553 + (-t632 + t284 * t557) * t50) - t22 * t12 * t642) * t101 * t2
     #48 / 0.1440D4 - (t22 * t12 * (t314 * t557 - t553) + 0.90D2 * t5 * 
     #t6 * (-t567 - t319 * t553 / 0.2D1 + t314 * t549 + t322 * t557 / 0.
     #6D1) - t571 - 0.180D3 * t11 * t12 * (-t319 * t557 / 0.2D1 - t549 +
     # t314 * t553)) * t248 / 0.1440D4
      t753 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t752)
      t755 = t2 * x1
      t756 = -0.1D1 + x1
      t757 = x1 * z
      t758 = 0.1D1 - x1 + t757
      t759 = 0.1D1 / t758
      t761 = t2 * t756 * t759
      t762 = s * t33
      t764 = x1 * t756 * t759
      t765 = t762 * t764
      t766 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, t
     #755, 0.0D0, -t765)
      t767 = t756 ** 2
      t768 = t759 * t767
      t769 = t35 * t768
      t772 = log(0.4D1 * t153 * t769)
      t773 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, t
     #755, 0.0D0, -t765)
      t775 = t772 ** 2
      t776 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, t
     #755, 0.0D0, -t765)
      t779 = z * t758
      t782 = t34 * t759
      t787 = log(-0.4D1 * t152 * t115 * t782 * t767 * t37)
      t788 = t787 * z
      t791 = t787 ** 2
      t792 = t791 * z
      t793 = t758 * t776
      t797 = x3 * t758
      t799 = Sqrt(-t797 * t36)
      t803 = x3 * x1
      t804 = t803 * z
      t805 = 0.3D1 * t804
      t806 = x1 * t31
      t807 = x3 * t31
      t808 = t807 * x1
      t809 = 0.2D1 * t803
      t811 = 0.2D1 * t152 * z
      t812 = t152 * t31
      t813 = -z - x3 + t757 - t152 + 0.2D1 * t43 * t799 * z - t805 - t80
     #6 + t808 + t809 + t811 - t812
      t814 = 0.1D1 / t813
      t820 = t779 * t773
      t832 = t12 * (-t776 - t779 * t776 * t814)
      t838 = t195 * t32
      t839 = t782 * t767
      t842 = log(0.4D1 * t838 * t839)
      t847 = t842 ** 2
      t850 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, t
     #755, 0.0D0, -t765)
      t851 = t847 * t842
      t859 = t12 * t776
      t871 = t225 * t195
      t874 = log(0.4D1 * t871 * t769)
      t880 = log(-0.4D1 * t871 * t35 * t768 * t37)
      t881 = t880 * z
      t895 = t252 * t29
      t898 = log(0.4D1 * t895 * t769)
      t900 = t898 ** 2
      t917 = (0.90D2 * t5 * t6 * (-t766 + t772 * t773 - t775 * t776 / 0.
     #2D1 + (-t779 * t766 + t788 * t758 * t773 - t792 * t793 / 0.2D1) * 
     #t814) - 0.180D3 * t11 * t12 * (-t773 + (-t820 + t788 * t793) * t81
     #4 + t772 * t776) + t22 * t832) * t101 * t192 / 0.1440D4 + (t22 * t
     #12 * (-t773 + t842 * t776) + 0.90D2 * t5 * t6 * (-t847 * t773 / 0.
     #2D1 - t850 + t851 * t776 / 0.6D1 + t842 * t766) - t80 * t859 - 0.1
     #80D3 * t11 * t12 * (-t847 * t776 / 0.2D1 - t766 + t842 * t773)) * 
     #t192 / 0.1440D4 + (0.90D2 * t5 * t6 * (t874 * t776 + (-t820 + t881
     # * t793) * t814 - t773) - 0.180D3 * t11 * t832) * t101 * t249 / 0.
     #720D3 + (0.90D2 * t5 * t6 * (t898 * t773 - t900 * t776 / 0.2D1 - t
     #766) - 0.180D3 * t11 * t12 * (t898 * t776 - t773) - t22 * t859) * 
     #t248 * t192 / 0.720D3
      t918 = FJET(XB1, XB2, s, 0.0D0, t755, -t761, 0.0D0, -t765, t917)
      t920 = x2 * s
      t921 = t920 * t1
      t922 = -0.1D1 + x2
      t923 = t922 * s
      t924 = t923 * t1
      t925 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0.
     #0D0, 0.0D0, 0.0D0)
      t926 = t34 * t922
      t930 = log(-0.4D1 * t226 * t115 * t926)
      t931 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0.
     #0D0, 0.0D0, 0.0D0)
      t937 = t12 * t931
      t944 = t35 * t922
      t947 = log(-0.4D1 * t895 * t944)
      t948 = t947 ** 2
      t951 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0.
     #0D0, 0.0D0, 0.0D0)
      t962 = t22 * t937
      t969 = log(-0.4D1 * t280 * t944)
      t971 = t969 ** 2
      t989 = log(-0.4D1 * t311 * t944)
      t994 = t989 ** 2
      t997 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0.
     #0D0, 0.0D0, 0.0D0)
      t998 = t994 * t989
      t1017 = (0.90D2 * t5 * t6 * (-t925 + t930 * t931) + 0.180D3 * t11 
     #* t937) * t101 * t249 / 0.720D3 + (0.90D2 * t5 * t6 * (-t948 * t93
     #1 / 0.2D1 - t951 + t947 * t925) - 0.180D3 * t11 * t12 * (-t925 + t
     #947 * t931) - t962) * t248 * t192 / 0.720D3 - (0.90D2 * t5 * t6 * 
     #(t951 - t969 * t925 + t971 * t931 / 0.2D1) - 0.180D3 * t11 * t12 *
     # (-t969 * t931 + t925) + t962) * t101 * t248 / 0.1440D4 - (t22 * t
     #12 * (t925 - t989 * t931) + 0.90D2 * t5 * t6 * (t994 * t925 / 0.2D
     #1 + t997 - t998 * t931 / 0.6D1 - t989 * t951) + t80 * t937 - 0.180
     #D3 * t11 * t12 * (t994 * t931 / 0.2D1 + t951 - t989 * t925)) * t24
     #8 / 0.1440D4
      t1018 = FJET(XB1, XB2, s, 0.0D0, t921, 0.0D0, -t924, 0.0D0, t1017)
      t1020 = x2 * x3
      t1023 = Sqrt(x3 * t922 * t36)
      t1024 = t43 * t1023
      t1026 = 0.2D1 * t1024 * x2
      t1028 = 0.1D1 - x3 + t1020
      t1029 = 0.1D1 / t1028
      t1031 = t2 * (0.1D1 - x3 - x2 + t1020 + t225 + t1026) * t1029
      t1036 = t2 * x2 * (-0.1D1 + t1020 + 0.2D1 * t1024) * t1029
      t1037 = x2 * z
      t1038 = -z + t1037 - x2
      t1039 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t1036, t1031,
     # 0.0D0, 0.0D0, 0.0D0)
      t1040 = t1038 * t1039
      t1041 = t922 * t36
      t1042 = t1028 ** 2
      t1043 = 0.1D1 / t1042
      t1044 = t1041 * t1043
      t1048 = log(0.4D1 * t871 * t35 * t1044)
      t1049 = t1048 * t1038
      t1050 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t1036, t1031,
     # 0.0D0, 0.0D0, 0.0D0)
      t1054 = t225 * z
      t1055 = t1020 * z
      t1061 = 0.1D1 / (z - t1037 + t1054 - t1055 + x3 - t225 + x2 - t102
     #6 - 0.2D1 * t1024 * z + 0.2D1 * t1024 * t1037)
      t1065 = t11 * pi
      t1066 = t6 * t1038
      t1068 = t1066 * t1050 * t1061
      t1075 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t1036, t1031,
     # 0.0D0, 0.0D0, 0.0D0)
      t1082 = log(0.4D1 * t225 * t115 * t926 * t36 * t1043)
      t1083 = t1082 * t1038
      t1085 = t1082 ** 2
      t1086 = t1085 * t1038
      t1100 = t22 * pi
      t1106 = (0.90D2 * t5 * t6 * (-t1040 + t1049 * t1050) * t1061 + 0.1
     #80D3 * t1065 * t1068) * t101 * t249 / 0.720D3 - (-0.90D2 * t5 * t6
     # * (-t1038 * t1075 + t1083 * t1039 - t1086 * t1050 / 0.2D1) * t106
     #1 + 0.180D3 * t1065 * t6 * (-t1040 + t1083 * t1050) * t1061 + t110
     #0 * t1068) * t101 * t248 / 0.1440D4
      t1107 = FJET(XB1, XB2, s, 0.0D0, t1031, 0.0D0, -t1036, 0.0D0, t110
     #6)
      t1109 = t1 * t756
      t1111 = t923 * t1109 * t759
      t1112 = t920 * t1109
      t1114 = t762 * t922 * t764
      t1115 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t1112, t1111,
     # t755, 0.0D0, t1114)
      t1120 = log(-0.4D1 * t871 * t35 * t768 * t922)
      t1121 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t1112, t1111,
     # t755, 0.0D0, t1114)
      t1127 = t12 * t1121
      t1133 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t1112, t1111,
     # t755, 0.0D0, t1114)
      t1139 = log(-0.4D1 * t252 * t115 * t782 * t767 * t922)
      t1141 = t1139 ** 2
      t1158 = (0.90D2 * t5 * t6 * (t1115 - t1120 * t1121) - 0.180D3 * t1
     #1 * t1127) * t101 * t249 / 0.720D3 + (0.90D2 * t5 * t6 * (t1133 - 
     #t1139 * t1115 + t1141 * t1121 / 0.2D1) - 0.180D3 * t11 * t12 * (-t
     #1139 * t1121 + t1115) + t22 * t1127) * t248 * t192 / 0.720D3
      t1159 = FJET(XB1, XB2, s, 0.0D0, t1111, t755, -t1112, t1114, t1158
     #)
      t1161 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0
     #.0D0, 0.0D0, 0.0D0)
      t1162 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0
     #.0D0, 0.0D0, 0.0D0)
      t1168 = t12 * t1162
      t1175 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0
     #.0D0, 0.0D0, 0.0D0)
      t1188 = t22 * t1168
      t1218 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0
     #.0D0, 0.0D0, 0.0D0)
      t1234 = (0.90D2 * t5 * t6 * (-t1161 + t930 * t1162) + 0.180D3 * t1
     #1 * t1168) * t101 * t249 / 0.720D3 + (0.90D2 * t5 * t6 * (-t1175 -
     # t948 * t1162 / 0.2D1 + t947 * t1161) - 0.180D3 * t11 * t12 * (-t1
     #161 + t947 * t1162) - t1188) * t248 * t192 / 0.720D3 - (0.90D2 * t
     #5 * t6 * (t1175 - t969 * t1161 + t971 * t1162 / 0.2D1) - 0.180D3 *
     # t11 * t12 * (-t969 * t1162 + t1161) + t1188) * t101 * t248 / 0.14
     #40D4 - (t22 * t12 * (-t989 * t1162 + t1161) + 0.90D2 * t5 * t6 * (
     #t994 * t1161 / 0.2D1 - t989 * t1175 - t998 * t1162 / 0.6D1 + t1218
     #) + t80 * t1168 - 0.180D3 * t11 * t12 * (t994 * t1162 / 0.2D1 + t1
     #175 - t989 * t1161)) * t248 / 0.1440D4
      t1235 = FJET(XB1, XB2, s, 0.0D0, -t924, 0.0D0, t921, 0.0D0, t1234)
      t1237 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, 
     #t755, 0.0D0, -t765)
      t1238 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, 
     #t755, 0.0D0, -t765)
      t1243 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, 
     #t755, 0.0D0, -t765)
      t1244 = t758 * t1243
      t1255 = t779 * t1238
      t1267 = t12 * (-t1243 - t779 * t1243 * t814)
      t1279 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, 
     #t755, 0.0D0, -t765)
      t1287 = t12 * t1243
      t1330 = (0.90D2 * t5 * t6 * (-t1237 + t772 * t1238 + (-t779 * t123
     #7 + t788 * t758 * t1238 - t792 * t1244 / 0.2D1) * t814 - t775 * t1
     #243 / 0.2D1) - 0.180D3 * t11 * t12 * ((-t1255 + t788 * t1244) * t8
     #14 + t772 * t1243 - t1238) + t22 * t1267) * t101 * t192 / 0.1440D4
     # + (t22 * t12 * (t842 * t1243 - t1238) + 0.90D2 * t5 * t6 * (-t847
     # * t1238 / 0.2D1 - t1279 + t842 * t1237 + t851 * t1243 / 0.6D1) - 
     #t80 * t1287 - 0.180D3 * t11 * t12 * (-t1237 - t847 * t1243 / 0.2D1
     # + t842 * t1238)) * t192 / 0.1440D4 + (0.90D2 * t5 * t6 * ((-t1255
     # + t881 * t1244) * t814 - t1238 + t874 * t1243) - 0.180D3 * t11 * 
     #t1267) * t101 * t249 / 0.720D3 + (0.90D2 * t5 * t6 * (-t1237 - t90
     #0 * t1243 / 0.2D1 + t898 * t1238) - 0.180D3 * t11 * t12 * (-t1238 
     #+ t898 * t1243) - t22 * t1287) * t248 * t192 / 0.720D3
      t1331 = FJET(XB1, XB2, s, 0.0D0, -t761, t755, 0.0D0, -t765, t1330)
      t1333 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t1036, t1031,
     # 0.0D0, 0.0D0, 0.0D0)
      t1334 = t1038 * t1333
      t1335 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t1036, t1031,
     # 0.0D0, 0.0D0, 0.0D0)
      t1343 = t1066 * t1335 * t1061
      t1350 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t1036, t1031,
     # 0.0D0, 0.0D0, 0.0D0)
      t1371 = (0.90D2 * t5 * t6 * (-t1334 + t1049 * t1335) * t1061 + 0.1
     #80D3 * t1065 * t1343) * t101 * t249 / 0.720D3 - (-0.90D2 * t5 * t6
     # * (-t1038 * t1350 + t1083 * t1333 - t1086 * t1335 / 0.2D1) * t106
     #1 + 0.180D3 * t1065 * t6 * (-t1334 + t1083 * t1335) * t1061 + t110
     #0 * t1343) * t101 * t248 / 0.1440D4
      t1372 = FJET(XB1, XB2, s, 0.0D0, -t1036, 0.0D0, t1031, 0.0D0, t137
     #1)
      t1374 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1377 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1379 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1382 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1403 = rrgq2qgh82J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0
     #D0, 0.0D0, 0.0D0)
      t1412 = z * t1377
      t1422 = z * t1374
      t1433 = t1379 + z * t1379 * t50
      t1434 = t12 * t1433
      t1453 = t12 * t1379
      t1454 = t80 * t1453
      t1491 = t22 * t1453
      t1499 = t12 * t1374
      t1577 = -(-0.180D3 * (-t119 * t1374 / 0.2D1 + t118 * t1377 + t120 
     #* t1379 / 0.6D1 - t1382) * lh - 0.15D2 / 0.4D1 * t143 * t1379 + 0.
     #15D2 * t120 * t1374 - t1379 * t113 - 0.45D2 * t119 * t1377 + 0.90D
     #2 * t118 * t1382 + (-t1377 - t119 * t1379 / 0.2D1 + t118 * t1374) 
     #* t21 + (-t1374 + t118 * t1379) * t79 - 0.90D2 * t1403) * t6 * t5 
     #/ 0.2880D4 + (0.90D2 * t5 * t6 * (-t167 * t1374 + t1377 + t169 * t
     #1379 / 0.2D1 - (-t1412 + t157 * t1374 - t160 * t1379 / 0.2D1) * t5
     #0) - 0.180D3 * t11 * t12 * (t1374 - (-t1422 + t157 * t1379) * t50 
     #- t167 * t1379) + t22 * t1434) * t101 * t192 / 0.1440D4 + (t22 * t
     #12 * (-t198 * t1379 + t1374) + 0.90D2 * t5 * t6 * (t203 * t1374 / 
     #0.2D1 - t198 * t1377 + t1382 - t206 * t1379 / 0.6D1) + t1454 - 0.1
     #80D3 * t11 * t12 * (-t198 * t1374 + t1377 + t203 * t1379 / 0.2D1))
     # * t192 / 0.1440D4 + (0.90D2 * t5 * t6 * (-t238 * t1379 - (-t1422 
     #+ t232 * t1379) * t50 + t1374) - 0.180D3 * t11 * t1434) * t101 * t
     #249 / 0.720D3 + (0.90D2 * t5 * t6 * (t1377 + t257 * t1379 / 0.2D1 
     #- t255 * t1374) - 0.180D3 * t11 * t12 * (t1374 - t255 * t1379) + t
     #1491) * t248 * t192 / 0.720D3 - ((-0.90D2 * t5 * t6 * t1377 + 0.18
     #0D3 * t11 * t1499 - t1491) * t55 - 0.90D2 * t5 * t6 * t1379 * t65 
     #+ (-t22 * t1499 - 0.90D2 * t5 * t6 * t1382 - t1454 + 0.180D3 * t11
     # * t12 * t1377) * t87 + (-0.90D2 * t5 * t6 * t1374 + 0.180D3 * t11
     # * t1453) * t98) * t101 / 0.2880D4 - (0.90D2 * t5 * t6 * (t275 * t
     #1374 - t276 * t1379 / 0.2D1 - t1377 + (-t1412 + t284 * t1374 - t28
     #7 * t1379 / 0.2D1) * t50) - 0.180D3 * t11 * t12 * ((-t1422 + t284 
     #* t1379) * t50 + t275 * t1379 - t1374) - t22 * t12 * t1433) * t101
     # * t248 / 0.1440D4 - (t22 * t12 * (t314 * t1379 - t1374) + 0.90D2 
     #* t5 * t6 * (t314 * t1377 - t1382 - t319 * t1374 / 0.2D1 + t322 * 
     #t1379 / 0.6D1) - t1454 - 0.180D3 * t11 * t12 * (-t319 * t1379 / 0.
     #2D1 - t1377 + t314 * t1374)) * t248 / 0.1440D4
      t1578 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1577)
      t1580 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, 
     #t755, 0.0D0, -t765)
      t1581 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, 
     #t755, 0.0D0, -t765)
      t1584 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, 
     #t755, 0.0D0, -t765)
      t1589 = t758 * t1581
      t1599 = t779 * t1584
      t1610 = t12 * (-t779 * t1581 * t814 - t1581)
      t1622 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, 
     #t755, 0.0D0, -t765)
      t1630 = t12 * t1581
      t1673 = (0.90D2 * t5 * t6 * (-t1580 - t775 * t1581 / 0.2D1 + t772 
     #* t1584 + (-t779 * t1580 + t788 * t758 * t1584 - t792 * t1589 / 0.
     #2D1) * t814) - 0.180D3 * t11 * t12 * (-t1584 + t772 * t1581 + (-t1
     #599 + t788 * t1589) * t814) + t22 * t1610) * t101 * t192 / 0.1440D
     #4 + (t22 * t12 * (-t1584 + t842 * t1581) + 0.90D2 * t5 * t6 * (-t8
     #47 * t1584 / 0.2D1 - t1622 + t851 * t1581 / 0.6D1 + t842 * t1580) 
     #- t80 * t1630 - 0.180D3 * t11 * t12 * (-t1580 - t847 * t1581 / 0.2
     #D1 + t842 * t1584)) * t192 / 0.1440D4 + (0.90D2 * t5 * t6 * (-t158
     #4 + (-t1599 + t881 * t1589) * t814 + t874 * t1581) - 0.180D3 * t11
     # * t1610) * t101 * t249 / 0.720D3 + (0.90D2 * t5 * t6 * (-t900 * t
     #1581 / 0.2D1 - t1580 + t898 * t1584) - 0.180D3 * t11 * t12 * (-t15
     #84 + t898 * t1581) - t22 * t1630) * t248 * t192 / 0.720D3
      t1674 = FJET(XB1, XB2, s, t755, 0.0D0, 0.0D0, -t761, -t765, t1673)
      t1676 = t341 * t340 + t547 * t546 + t753 * t752 + t918 * t917 + t1
     #018 * t1017 + t1107 * t1106 + t1159 * t1158 + t1235 * t1234 + t133
     #1 * t1330 + t1372 * t1371 + t1578 * t1577 + t1674 * t1673
      t1677 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t1112, t1111,
     # t755, 0.0D0, t1114)
      t1679 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t1112, t1111,
     # t755, 0.0D0, t1114)
      t1684 = t12 * t1677
      t1693 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t1112, t1111,
     # t755, 0.0D0, t1114)
      t1708 = (0.90D2 * t5 * t6 * (-t1120 * t1677 + t1679) - 0.180D3 * t
     #11 * t1684) * t101 * t249 / 0.720D3 + (0.90D2 * t5 * t6 * (t1141 *
     # t1677 / 0.2D1 - t1139 * t1679 + t1693) - 0.180D3 * t11 * t12 * (t
     #1679 - t1139 * t1677) + t22 * t1684) * t248 * t192 / 0.720D3
      t1709 = FJET(XB1, XB2, s, t755, -t1112, 0.0D0, t1111, t1114, t1708
     #)
      t1711 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0
     #.0D0, 0.0D0, 0.0D0)
      t1712 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0
     #.0D0, 0.0D0, 0.0D0)
      t1715 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0
     #.0D0, 0.0D0, 0.0D0)
      t1726 = t12 * t1712
      t1727 = t22 * t1726
      t1740 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0
     #.0D0, 0.0D0, 0.0D0)
      t1784 = -(0.90D2 * t5 * t6 * (t1711 + t971 * t1712 / 0.2D1 - t969 
     #* t1715) - 0.180D3 * t11 * t12 * (-t969 * t1712 + t1715) + t1727) 
     #* t101 * t248 / 0.1440D4 - (t22 * t12 * (t1715 - t989 * t1712) + 0
     #.90D2 * t5 * t6 * (t994 * t1715 / 0.2D1 - t998 * t1712 / 0.6D1 + t
     #1740 - t989 * t1711) + t80 * t1726 - 0.180D3 * t11 * t12 * (-t989 
     #* t1715 + t1711 + t994 * t1712 / 0.2D1)) * t248 / 0.1440D4 + (0.90
     #D2 * t5 * t6 * (-t1715 + t930 * t1712) + 0.180D3 * t11 * t1726) * 
     #t101 * t249 / 0.720D3 + (0.90D2 * t5 * t6 * (-t1711 - t948 * t1712
     # / 0.2D1 + t947 * t1715) - 0.180D3 * t11 * t12 * (-t1715 + t947 * 
     #t1712) - t1727) * t248 * t192 / 0.720D3
      t1785 = FJET(XB1, XB2, s, t921, 0.0D0, -t924, 0.0D0, 0.0D0, t1784)
      t1787 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t1036, t1031,
     # 0.0D0, 0.0D0, 0.0D0)
      t1789 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t1036, t1031,
     # 0.0D0, 0.0D0, 0.0D0)
      t1791 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t1036, t1031,
     # 0.0D0, 0.0D0, 0.0D0)
      t1799 = t1038 * t1789
      t1807 = t1066 * t1791 * t1061
      t1825 = -(-0.90D2 * t5 * t6 * (-t1038 * t1787 + t1083 * t1789 - t1
     #086 * t1791 / 0.2D1) * t1061 + 0.180D3 * t1065 * t6 * (-t1799 + t1
     #083 * t1791) * t1061 + t1100 * t1807) * t101 * t248 / 0.1440D4 + (
     #0.90D2 * t5 * t6 * (-t1799 + t1049 * t1791) * t1061 + 0.180D3 * t1
     #065 * t1807) * t101 * t249 / 0.720D3
      t1826 = FJET(XB1, XB2, s, t1031, 0.0D0, -t1036, 0.0D0, 0.0D0, t182
     #5)
      t1828 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t1112, t1111,
     # t755, 0.0D0, t1114)
      t1829 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t1112, t1111,
     # t755, 0.0D0, t1114)
      t1835 = t12 * t1829
      t1841 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t1112, t1111,
     # t755, 0.0D0, t1114)
      t1859 = (0.90D2 * t5 * t6 * (t1828 - t1120 * t1829) - 0.180D3 * t1
     #1 * t1835) * t101 * t249 / 0.720D3 + (0.90D2 * t5 * t6 * (t1841 + 
     #t1141 * t1829 / 0.2D1 - t1139 * t1828) - 0.180D3 * t11 * t12 * (t1
     #828 - t1139 * t1829) + t22 * t1835) * t248 * t192 / 0.720D3
      t1860 = FJET(XB1, XB2, s, t1111, 0.0D0, -t1112, t755, t1114, t1859
     #)
      t1863 = t755 * t1020 * t1029
      t1864 = t2 * t756
      t1865 = t225 * t757
      t1867 = Sqrt(t797 * t1041)
      t1868 = t43 * t1867
      t1870 = 0.2D1 * t1868 * x2
      t1871 = t225 * x1
      t1875 = t1864 * (t1865 + 0.1D1 - x3 + t1870 - x2 + t1020 + t225 - 
     #t1871) * t759 * t1029
      t1879 = t36 * s * t1 * x1 * t1029
      t1885 = t1864 * x2 * (-0.1D1 + t1020 + x1 - t803 - t757 + t804 + 0
     #.2D1 * t1868) * t759 * t1029
      t1890 = log(0.4D1 * t225 * t838 * t839 * t1044)
      t1891 = t151 * x2
      t1892 = x2 * x1
      t1899 = t31 * x2
      t1905 = t1892 * z
      t1910 = t757 - t152 - t806 + t809 + t1870 - t1871 - t1891 + 0.2D1 
     #* t1892 + t1865 + 0.2D1 * t1020 * t757 - t807 * t1892 - 0.2D1 * t1
     #52 * t1037 + t152 * t1899 - 0.2D1 * t1868 * t1892 - 0.2D1 * t1868 
     #* t1037 + 0.2D1 * t1868 * t1905 - t151 * t31 * x2
      t1919 = t1899 * x1 + 0.2D1 * t1891 * z + 0.2D1 * t1868 * z - 0.3D1
     # * t1905 - t1020 * x1 - t1054 + t1055 - z - x3 + t152 * x2 + t225 
     #+ t1037 - x2 - t805 + t808 + t811 - t812
      t1921 = 0.1D1 / (t1910 + t1919)
      t1922 = t1890 * t1921
      t1923 = x2 - t1892 + z - t1037 + t1905
      t1924 = t1923 * t758
      t1925 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t1885, -t1875,
     # -t1879, t1863, t1114)
      t1928 = t1921 * t1923
      t1929 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t1885, -t1875,
     # -t1879, t1863, t1114)
      t1936 = t11 * t12
      t1941 = 0.90D2 * t5 * t6 * (-t1922 * t1924 * t1925 + t1928 * t758 
     #* t1929) - 0.180D3 * t1936 * t1928 * t758 * t1925
      t1945 = FJET(XB1, XB2, s, t1863, -t1875, -t1879, t1885, t1114, t19
     #41 * t101 * t249 / 0.720D3)
      t1948 = t101 * t248 * t192
      t1951 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t1885, -t1875,
     # -t1879, t1863, t1114)
      t1954 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t1885, -t1875,
     # -t1879, t1863, t1114)
      t1965 = 0.90D2 * t5 * t6 * (-t1922 * t1924 * t1951 + t1928 * t758 
     #* t1954) - 0.180D3 * t1936 * t1928 * t758 * t1951
      t1969 = FJET(XB1, XB2, s, t1885, -t1879, -t1875, t1863, t1114, t19
     #65 * t101 * t249 / 0.720D3)
      t1973 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0
     #.0D0, 0.0D0, 0.0D0)
      t1975 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0
     #.0D0, 0.0D0, 0.0D0)
      t1978 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0
     #.0D0, 0.0D0, 0.0D0)
      t1988 = t12 * t1975
      t1989 = t22 * t1988
      t2000 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, t921, -t924, 0
     #.0D0, 0.0D0, 0.0D0)
      t2046 = -(0.90D2 * t5 * t6 * (-t969 * t1973 + t971 * t1975 / 0.2D1
     # + t1978) - 0.180D3 * t11 * t12 * (-t969 * t1975 + t1973) + t1989)
     # * t101 * t248 / 0.1440D4 - (t22 * t12 * (-t989 * t1975 + t1973) +
     # 0.90D2 * t5 * t6 * (t994 * t1973 / 0.2D1 + t2000 - t998 * t1975 /
     # 0.6D1 - t989 * t1978) + t80 * t1988 - 0.180D3 * t11 * t12 * (-t98
     #9 * t1973 + t1978 + t994 * t1975 / 0.2D1)) * t248 / 0.1440D4 + (0.
     #90D2 * t5 * t6 * (-t1973 + t930 * t1975) + 0.180D3 * t11 * t1988) 
     #* t101 * t249 / 0.720D3 + (0.90D2 * t5 * t6 * (-t1978 - t948 * t19
     #75 / 0.2D1 + t947 * t1973) - 0.180D3 * t11 * t12 * (-t1973 + t947 
     #* t1975) - t1989) * t248 * t192 / 0.720D3
      t2047 = FJET(XB1, XB2, s, -t924, 0.0D0, t921, 0.0D0, 0.0D0, t2046)
      t2049 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, 
     #t755, 0.0D0, -t765)
      t2050 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, 
     #t755, 0.0D0, -t765)
      t2053 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, 
     #t755, 0.0D0, -t765)
      t2058 = t758 * t2050
      t2068 = t779 * t2053
      t2079 = t12 * (-t779 * t2050 * t814 - t2050)
      t2091 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t761, 
     #t755, 0.0D0, -t765)
      t2099 = t12 * t2050
      t2142 = (0.90D2 * t5 * t6 * (-t2049 - t775 * t2050 / 0.2D1 + t772 
     #* t2053 + (-t779 * t2049 + t788 * t758 * t2053 - t792 * t2058 / 0.
     #2D1) * t814) - 0.180D3 * t11 * t12 * (-t2053 + t772 * t2050 + (-t2
     #068 + t788 * t2058) * t814) + t22 * t2079) * t101 * t192 / 0.1440D
     #4 + (t22 * t12 * (t842 * t2050 - t2053) + 0.90D2 * t5 * t6 * (-t84
     #7 * t2053 / 0.2D1 - t2091 + t851 * t2050 / 0.6D1 + t842 * t2049) -
     # t80 * t2099 - 0.180D3 * t11 * t12 * (-t847 * t2050 / 0.2D1 + t842
     # * t2053 - t2049)) * t192 / 0.1440D4 + (0.90D2 * t5 * t6 * (-t2053
     # + t874 * t2050 + (-t2068 + t881 * t2058) * t814) - 0.180D3 * t11 
     #* t2079) * t101 * t249 / 0.720D3 + (0.90D2 * t5 * t6 * (t898 * t20
     #53 - t2049 - t900 * t2050 / 0.2D1) - 0.180D3 * t11 * t12 * (t898 *
     # t2050 - t2053) - t22 * t2099) * t248 * t192 / 0.720D3
      t2143 = FJET(XB1, XB2, s, -t761, 0.0D0, 0.0D0, t755, -t765, t2142)
      t2145 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t1112, t1111,
     # t755, 0.0D0, t1114)
      t2146 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t1112, t1111,
     # t755, 0.0D0, t1114)
      t2152 = t12 * t2146
      t2158 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t1112, t1111,
     # t755, 0.0D0, t1114)
      t2176 = (0.90D2 * t5 * t6 * (t2145 - t1120 * t2146) - 0.180D3 * t1
     #1 * t2152) * t101 * t249 / 0.720D3 + (0.90D2 * t5 * t6 * (t2158 - 
     #t1139 * t2145 + t1141 * t2146 / 0.2D1) - 0.180D3 * t11 * t12 * (t2
     #145 - t1139 * t2146) + t22 * t2152) * t248 * t192 / 0.720D3
      t2177 = FJET(XB1, XB2, s, -t1112, t755, t1111, 0.0D0, t1114, t2176
     #)
      t2179 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t1036, t1031,
     # 0.0D0, 0.0D0, 0.0D0)
      t2181 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t1036, t1031,
     # 0.0D0, 0.0D0, 0.0D0)
      t2183 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t1036, t1031,
     # 0.0D0, 0.0D0, 0.0D0)
      t2191 = t1038 * t2181
      t2199 = t1066 * t2183 * t1061
      t2217 = -(-0.90D2 * t5 * t6 * (-t1038 * t2179 + t1083 * t2181 - t1
     #086 * t2183 / 0.2D1) * t1061 + 0.180D3 * t1065 * t6 * (-t2191 + t1
     #083 * t2183) * t1061 + t1100 * t2199) * t101 * t248 / 0.1440D4 + (
     #0.90D2 * t5 * t6 * (-t2191 + t1049 * t2183) * t1061 + 0.180D3 * t1
     #065 * t2199) * t101 * t249 / 0.720D3
      t2218 = FJET(XB1, XB2, s, -t1036, 0.0D0, t1031, 0.0D0, 0.0D0, t221
     #7)
      t2220 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t1885, -t1875,
     # -t1879, t1863, t1114)
      t2223 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t1885, -t1875,
     # -t1879, t1863, t1114)
      t2234 = 0.90D2 * t5 * t6 * (t1928 * t758 * t2220 - t1922 * t1924 *
     # t2223) - 0.180D3 * t1936 * t1928 * t758 * t2223
      t2238 = FJET(XB1, XB2, s, -t1879, t1885, t1863, -t1875, t1114, t22
     #34 * t101 * t249 / 0.720D3)
      t2242 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t1885, -t1875,
     # -t1879, t1863, t1114)
      t2245 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t1885, -t1875,
     # -t1879, t1863, t1114)
      t2256 = 0.90D2 * t5 * t6 * (t1928 * t758 * t2242 - t1922 * t1924 *
     # t2245) - 0.180D3 * t1936 * t1928 * t758 * t2245
      t2260 = FJET(XB1, XB2, s, -t1875, t1863, t1885, -t1879, t1114, t22
     #56 * t101 * t249 / 0.720D3)
      t2264 = t1709 * t1708 + t1785 * t1784 + t1826 * t1825 + t1860 * t1
     #859 + t1945 * t1941 * t1948 / 0.720D3 + t1969 * t1965 * t1948 / 0.
     #720D3 + t2047 * t2046 + t2143 * t2142 + t2177 * t2176 + t2218 * t2
     #217 + t2238 * t2234 * t1948 / 0.720D3 + t2260 * t2256 * t1948 / 0.
     #720D3
      rrgq2qght8s1e1 = t1676 + t2264

      end function



      doubleprecision function rrgq2qght8s1e0
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = x4 * pi
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = x3 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = -0.1D1 + x3
      t19 = 0.1D1 / t18
      t20 = t17 * t19
      t23 = log(-0.4D1 * t12 * t20)
      t24 = t23 ** 2
      t26 = cos(t9)
      t28 = Sqrt(-x3 * t18)
      t33 = 0.1D1 / (-z - x3 + 0.2D1 * t26 * t28 * z)
      t37 = log(0.4D1 * t12 * t17)
      t38 = t37 ** 2
      t40 = t24 * z * t33 / 0.2D1 + t38 / 0.2D1
      t44 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t48 = lh * t4
      t49 = pi * t6
      t50 = t49 * t7
      t52 = 0.180D3 * t48 * t50
      t56 = -t23 * z * t33 - t37
      t58 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t65 = lh ** 2
      t67 = pi ** 2
      t69 = 0.180D3 * t65 - 0.30D2 * t67
      t70 = t69 * t4
      t71 = t70 * t50
      t74 = z * t33 + 0.1D1
      t77 = 0.1D1 / x3
      t80 = t14 * t11
      t81 = t80 * t16
      t83 = log(0.4D1 * t81)
      t84 = t83 ** 2
      t92 = 0.60D2 * lh * t67 - 0.240D3 * zeta3 - 0.120D3 * t65 * lh
      t94 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t96 = t84 * t83
      t114 = x2 ** 2
      t115 = t114 * x3
      t118 = log(0.4D1 * t115 * t81)
      t120 = z * t44
      t121 = t115 * t11
      t124 = log(-0.4D1 * t121 * t20)
      t125 = t124 * z
      t135 = -t7 - z * t7 * t33
      t141 = 0.1D1 / x2
      t144 = t114 * t11
      t147 = log(0.4D1 * t144 * t17)
      t148 = t147 ** 2
      t164 = x1 ** 2
      t165 = x3 * t164
      t166 = t165 * t11
      t169 = log(-0.4D1 * t166 * t20)
      t170 = t169 * z
      t176 = log(0.4D1 * t165 * t81)
      t182 = -t135
      t188 = 0.1D1 / x1
      t191 = t5 * t6
      t193 = t141 * t188
      t197 = t114 * t164
      t200 = log(0.4D1 * t197 * t81)
      t210 = t164 * t11
      t213 = log(0.4D1 * t210 * t17)
      t214 = t213 ** 2
      t230 = -(-0.90D2 * t5 * t6 * t7 * t40 + (-0.90D2 * t5 * t6 * t44 +
     # t52) * t56 + (-0.90D2 * t5 * t6 * t58 + 0.180D3 * t48 * t49 * t44
     # - t71) * t74) * t77 / 0.2880D4 - (-0.45D2 * t84 * t44 - t7 * t92 
     #- 0.90D2 * t94 + 0.15D2 * t96 * t7 + 0.90D2 * t83 * t58 - 0.180D3 
     #* (t83 * t44 - t58 - t84 * t7 / 0.2D1) * lh + (-t44 + t83 * t7) * 
     #t69) * t6 * t5 / 0.2880D4 - (0.90D2 * t5 * t6 * (t118 * t7 - t44 +
     # (-t120 + t125 * t7) * t33) - 0.180D3 * t48 * t49 * t135) * t77 * 
     #t141 / 0.1440D4 - (0.90D2 * t5 * t6 * (-t58 - t148 * t7 / 0.2D1 + 
     #t147 * t44) - 0.180D3 * t48 * t49 * (-t44 + t147 * t7) - t71) * t1
     #41 / 0.1440D4 + (0.90D2 * t5 * t6 * (-(-t120 + t170 * t7) * t33 + 
     #t44 - t176 * t7) - 0.180D3 * t48 * t49 * t182) * t77 * t188 / 0.14
     #40D4 + t191 * t182 * t77 * t193 / 0.8D1 + (0.90D2 * t5 * t6 * (t44
     # - t200 * t7) - t52) * t141 * t188 / 0.720D3 + (0.90D2 * t5 * t6 *
     # (t58 + t214 * t7 / 0.2D1 - t213 * t44) - 0.180D3 * t48 * t49 * (t
     #44 - t213 * t7) + t71) * t188 / 0.1440D4
      t231 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t230)
      t233 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t238 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t242 = t49 * t233
      t244 = 0.180D3 * t48 * t242
      t247 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t254 = t70 * t242
      t268 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t283 = z * t238
      t293 = -t233 - z * t233 * t33
      t324 = -t293
      t360 = -(-0.90D2 * t5 * t6 * t233 * t40 + (-0.90D2 * t5 * t6 * t23
     #8 + t244) * t56 + (-0.90D2 * t5 * t6 * t247 + 0.180D3 * t48 * t49 
     #* t238 - t254) * t74) * t77 / 0.2880D4 - (-0.45D2 * t84 * t238 - 0
     #.180D3 * (-t84 * t233 / 0.2D1 - t247 + t83 * t238) * lh - 0.90D2 *
     # t268 + (-t238 + t83 * t233) * t69 + 0.15D2 * t96 * t233 - t233 * 
     #t92 + 0.90D2 * t83 * t247) * t6 * t5 / 0.2880D4 - (0.90D2 * t5 * t
     #6 * (-t238 + t118 * t233 + (-t283 + t125 * t233) * t33) - 0.180D3 
     #* t48 * t49 * t293) * t77 * t141 / 0.1440D4 - (0.90D2 * t5 * t6 * 
     #(t147 * t238 - t148 * t233 / 0.2D1 - t247) - 0.180D3 * t48 * t49 *
     # (-t238 + t147 * t233) - t254) * t141 / 0.1440D4 + (0.90D2 * t5 * 
     #t6 * (t238 - (-t283 + t170 * t233) * t33 - t176 * t233) - 0.180D3 
     #* t48 * t49 * t324) * t77 * t188 / 0.1440D4 + t191 * t324 * t77 * 
     #t193 / 0.8D1 + (0.90D2 * t5 * t6 * (t238 - t200 * t233) - t244) * 
     #t141 * t188 / 0.720D3 + (0.90D2 * t5 * t6 * (-t213 * t238 + t214 *
     # t233 / 0.2D1 + t247) - 0.180D3 * t48 * t49 * (t238 - t213 * t233)
     # + t254) * t188 / 0.1440D4
      t361 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t360)
      t363 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t368 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t372 = t49 * t363
      t374 = 0.180D3 * t48 * t372
      t377 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t384 = t70 * t372
      t390 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t413 = z * t368
      t423 = -z * t363 * t33 - t363
      t454 = -t423
      t490 = -(-0.90D2 * t5 * t6 * t363 * t40 + (-0.90D2 * t5 * t6 * t36
     #8 + t374) * t56 + (-0.90D2 * t5 * t6 * t377 + 0.180D3 * t48 * t49 
     #* t368 - t384) * t74) * t77 / 0.2880D4 - (-0.90D2 * t390 - 0.45D2 
     #* t84 * t368 + (-t368 + t83 * t363) * t69 - 0.180D3 * (-t377 - t84
     # * t363 / 0.2D1 + t83 * t368) * lh + 0.90D2 * t83 * t377 - t363 * 
     #t92 + 0.15D2 * t96 * t363) * t6 * t5 / 0.2880D4 - (0.90D2 * t5 * t
     #6 * (t118 * t363 - t368 + (-t413 + t125 * t363) * t33) - 0.180D3 *
     # t48 * t49 * t423) * t77 * t141 / 0.1440D4 - (0.90D2 * t5 * t6 * (
     #-t148 * t363 / 0.2D1 - t377 + t147 * t368) - 0.180D3 * t48 * t49 *
     # (t147 * t363 - t368) - t384) * t141 / 0.1440D4 + (0.90D2 * t5 * t
     #6 * (-t176 * t363 + t368 - (-t413 + t170 * t363) * t33) - 0.180D3 
     #* t48 * t49 * t454) * t77 * t188 / 0.1440D4 + t191 * t454 * t77 * 
     #t193 / 0.8D1 + (0.90D2 * t5 * t6 * (t368 - t200 * t363) - t374) * 
     #t141 * t188 / 0.720D3 + (0.90D2 * t5 * t6 * (t214 * t363 / 0.2D1 +
     # t377 - t213 * t368) - 0.180D3 * t48 * t49 * (-t213 * t363 + t368)
     # + t384) * t188 / 0.1440D4
      t491 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t490)
      t493 = t2 * x1
      t494 = -0.1D1 + x1
      t495 = x1 * z
      t496 = 0.1D1 - x1 + t495
      t497 = 0.1D1 / t496
      t499 = t2 * t494 * t497
      t500 = s * t15
      t502 = x1 * t494 * t497
      t503 = t500 * t502
      t504 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t499, t
     #493, 0.0D0, -t503)
      t505 = z * t496
      t508 = t16 * t497
      t509 = t494 ** 2
      t514 = log(-0.4D1 * t165 * t80 * t508 * t509 * t19)
      t515 = t514 * z
      t516 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t499, t
     #493, 0.0D0, -t503)
      t520 = x3 * t496
      t522 = Sqrt(-t520 * t18)
      t526 = x3 * x1
      t527 = t526 * z
      t528 = 0.3D1 * t527
      t529 = x1 * t13
      t530 = x3 * t13
      t531 = t530 * x1
      t532 = 0.2D1 * t526
      t534 = 0.2D1 * t165 * z
      t535 = t165 * t13
      t536 = -z - x3 + t495 - t165 + 0.2D1 * t26 * t522 * z - t528 - t52
     #9 + t531 + t532 + t534 - t535
      t537 = 0.1D1 / t536
      t540 = t17 * t497 * t509
      t543 = log(0.4D1 * t166 * t540)
      t551 = -t516 - t505 * t516 * t537
      t563 = t197 * t11
      t566 = log(0.4D1 * t563 * t540)
      t572 = t49 * t516
      t583 = log(0.4D1 * t210 * t14 * t508 * t509)
      t584 = t583 ** 2
      t587 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t499, t
     #493, 0.0D0, -t503)
      t602 = (0.90D2 * t5 * t6 * (-t504 + (-t505 * t504 + t515 * t496 * 
     #t516) * t537 + t543 * t516) - 0.180D3 * t48 * t49 * t551) * t77 * 
     #t188 / 0.1440D4 + t191 * t551 * t77 * t193 / 0.8D1 + (0.90D2 * t5 
     #* t6 * (t566 * t516 - t504) + 0.180D3 * t48 * t572) * t141 * t188 
     #/ 0.720D3 + (0.90D2 * t5 * t6 * (-t584 * t516 / 0.2D1 - t587 + t58
     #3 * t504) - 0.180D3 * t48 * t49 * (-t504 + t583 * t516) - t70 * t5
     #72) * t188 / 0.1440D4
      t603 = FJET(XB1, XB2, s, 0.0D0, t493, -t499, 0.0D0, -t503, t602)
      t605 = x2 * s
      t606 = t605 * t1
      t607 = -0.1D1 + x2
      t608 = t607 * s
      t609 = t608 * t1
      t610 = t17 * t607
      t613 = log(-0.4D1 * t121 * t610)
      t614 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t606, -t609, 0.
     #0D0, 0.0D0, 0.0D0)
      t616 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t606, -t609, 0.
     #0D0, 0.0D0, 0.0D0)
      t621 = t49 * t614
      t623 = 0.180D3 * t48 * t621
      t630 = log(-0.4D1 * t144 * t610)
      t631 = t630 ** 2
      t634 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t606, -t609, 0.
     #0D0, 0.0D0, 0.0D0)
      t655 = log(-0.4D1 * t563 * t610)
      t665 = -(0.90D2 * t5 * t6 * (-t613 * t614 + t616) - t623) * t77 * 
     #t141 / 0.1440D4 - (0.90D2 * t5 * t6 * (t631 * t614 / 0.2D1 + t634 
     #- t630 * t616) - 0.180D3 * t48 * t49 * (t616 - t630 * t614) + t70 
     #* t621) * t141 / 0.1440D4 - t191 * t614 * t77 * t193 / 0.8D1 + (0.
     #90D2 * t5 * t6 * (-t616 + t655 * t614) + t623) * t141 * t188 / 0.7
     #20D3
      t666 = FJET(XB1, XB2, s, 0.0D0, t606, 0.0D0, -t609, 0.0D0, t665)
      t668 = x2 * x3
      t671 = Sqrt(x3 * t607 * t18)
      t672 = t26 * t671
      t674 = 0.2D1 * t672 * x2
      t676 = 0.1D1 - x3 + t668
      t677 = 0.1D1 / t676
      t679 = t2 * (0.1D1 - x3 - x2 + t668 + t115 + t674) * t677
      t684 = t2 * x2 * (-0.1D1 + t668 + 0.2D1 * t672) * t677
      t685 = x2 * z
      t686 = -z + t685 - x2
      t687 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t684, t679, 0.
     #0D0, 0.0D0, 0.0D0)
      t691 = t676 ** 2
      t697 = log(0.4D1 * t115 * t80 * t16 * t607 * t18 / t691)
      t698 = t697 * t686
      t699 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t684, t679, 0.
     #0D0, 0.0D0, 0.0D0)
      t703 = t115 * z
      t704 = t668 * z
      t710 = 0.1D1 / (z - t685 + t703 - t704 + x3 - t115 + x2 - t674 - 0
     #.2D1 * t672 * z + 0.2D1 * t672 * t685)
      t714 = t48 * pi
      t715 = t6 * t686
      t716 = t699 * t710
      t724 = t5 * t715
      t726 = t77 * t141 * t188
      t730 = -(-0.90D2 * t5 * t6 * (-t686 * t687 + t698 * t699) * t710 -
     # 0.180D3 * t714 * t715 * t716) * t77 * t141 / 0.1440D4 - t724 * t7
     #16 * t726 / 0.8D1
      t731 = FJET(XB1, XB2, s, 0.0D0, t679, 0.0D0, -t684, 0.0D0, t730)
      t733 = t1 * t494
      t735 = t608 * t733 * t497
      t736 = t605 * t733
      t738 = t500 * t607 * t502
      t739 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t736, t735, t4
     #93, 0.0D0, t738)
      t749 = log(-0.4D1 * t197 * t80 * t508 * t509 * t607)
      t751 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t736, t735, t4
     #93, 0.0D0, t738)
      t763 = t191 * t739 * t77 * t193 / 0.8D1 + (0.90D2 * t5 * t6 * (-t7
     #49 * t739 + t751) - 0.180D3 * t48 * t49 * t739) * t141 * t188 / 0.
     #720D3
      t764 = FJET(XB1, XB2, s, 0.0D0, t735, t493, -t736, t738, t763)
      t766 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t606, -t609, 0.
     #0D0, 0.0D0, 0.0D0)
      t768 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t606, -t609, 0.
     #0D0, 0.0D0, 0.0D0)
      t773 = t49 * t766
      t775 = 0.180D3 * t48 * t773
      t782 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t606, -t609, 0.
     #0D0, 0.0D0, 0.0D0)
      t810 = -(0.90D2 * t5 * t6 * (-t613 * t766 + t768) - t775) * t77 * 
     #t141 / 0.1440D4 - (0.90D2 * t5 * t6 * (t631 * t766 / 0.2D1 + t782 
     #- t630 * t768) - 0.180D3 * t48 * t49 * (-t630 * t766 + t768) + t70
     # * t773) * t141 / 0.1440D4 - t191 * t766 * t77 * t193 / 0.8D1 + (0
     #.90D2 * t5 * t6 * (-t768 + t655 * t766) + t775) * t141 * t188 / 0.
     #720D3
      t811 = FJET(XB1, XB2, s, 0.0D0, -t609, 0.0D0, t606, 0.0D0, t810)
      t813 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t499, t
     #493, 0.0D0, -t503)
      t815 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t499, t
     #493, 0.0D0, -t503)
      t827 = -t815 - t505 * t815 * t537
      t844 = t49 * t815
      t851 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t499, t
     #493, 0.0D0, -t503)
      t868 = (0.90D2 * t5 * t6 * ((-t505 * t813 + t515 * t496 * t815) * 
     #t537 + t543 * t815 - t813) - 0.180D3 * t48 * t49 * t827) * t77 * t
     #188 / 0.1440D4 + t191 * t827 * t77 * t193 / 0.8D1 + (0.90D2 * t5 *
     # t6 * (-t813 + t566 * t815) + 0.180D3 * t48 * t844) * t141 * t188 
     #/ 0.720D3 + (0.90D2 * t5 * t6 * (-t851 - t584 * t815 / 0.2D1 + t58
     #3 * t813) - 0.180D3 * t48 * t49 * (t583 * t815 - t813) - t70 * t84
     #4) * t188 / 0.1440D4
      t869 = FJET(XB1, XB2, s, 0.0D0, -t499, t493, 0.0D0, -t503, t868)
      t871 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t684, t679, 0.
     #0D0, 0.0D0, 0.0D0)
      t873 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t684, t679, 0.
     #0D0, 0.0D0, 0.0D0)
      t880 = t873 * t710
      t891 = -(-0.90D2 * t5 * t6 * (-t686 * t871 + t698 * t873) * t710 -
     # 0.180D3 * t714 * t715 * t880) * t77 * t141 / 0.1440D4 - t724 * t8
     #80 * t726 / 0.8D1
      t892 = FJET(XB1, XB2, s, 0.0D0, -t684, 0.0D0, t679, 0.0D0, t891)
      t894 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t895 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t898 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t913 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t919 = z * t898
      t930 = -t895 - z * t895 * t33
      t950 = t49 * t895
      t951 = t70 * t950
      t963 = 0.180D3 * t48 * t950
      t985 = -t930
      t1021 = -(-0.180D3 * (-t894 - t84 * t895 / 0.2D1 + t83 * t898) * l
     #h - 0.45D2 * t84 * t898 + (-t898 + t83 * t895) * t69 + 0.15D2 * t9
     #6 * t895 + 0.90D2 * t83 * t894 - t895 * t92 - 0.90D2 * t913) * t6 
     #* t5 / 0.2880D4 - (0.90D2 * t5 * t6 * ((-t919 + t125 * t895) * t33
     # + t118 * t895 - t898) - 0.180D3 * t48 * t49 * t930) * t77 * t141 
     #/ 0.1440D4 - (0.90D2 * t5 * t6 * (-t148 * t895 / 0.2D1 - t894 + t1
     #47 * t898) - 0.180D3 * t48 * t49 * (t147 * t895 - t898) - t951) * 
     #t141 / 0.1440D4 - (-0.90D2 * t5 * t6 * t895 * t40 + (-0.90D2 * t5 
     #* t6 * t898 + t963) * t56 + (-0.90D2 * t5 * t6 * t894 + 0.180D3 * 
     #t48 * t49 * t898 - t951) * t74) * t77 / 0.2880D4 + (0.90D2 * t5 * 
     #t6 * (t898 - (-t919 + t170 * t895) * t33 - t176 * t895) - 0.180D3 
     #* t48 * t49 * t985) * t77 * t188 / 0.1440D4 + t191 * t985 * t77 * 
     #t193 / 0.8D1 + (0.90D2 * t5 * t6 * (t898 - t200 * t895) - t963) * 
     #t141 * t188 / 0.720D3 + (0.90D2 * t5 * t6 * (-t213 * t898 + t894 +
     # t214 * t895 / 0.2D1) - 0.180D3 * t48 * t49 * (-t213 * t895 + t898
     #) + t951) * t188 / 0.1440D4
      t1022 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1021)
      t1024 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t499, 
     #t493, 0.0D0, -t503)
      t1025 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t499, 
     #t493, 0.0D0, -t503)
      t1038 = -t505 * t1025 * t537 - t1025
      t1055 = t49 * t1025
      t1062 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t499, 
     #t493, 0.0D0, -t503)
      t1079 = (0.90D2 * t5 * t6 * (-t1024 + t543 * t1025 + (-t505 * t102
     #4 + t515 * t496 * t1025) * t537) - 0.180D3 * t48 * t49 * t1038) * 
     #t77 * t188 / 0.1440D4 + t191 * t1038 * t77 * t193 / 0.8D1 + (0.90D
     #2 * t5 * t6 * (-t1024 + t566 * t1025) + 0.180D3 * t48 * t1055) * t
     #141 * t188 / 0.720D3 + (0.90D2 * t5 * t6 * (-t1062 - t584 * t1025 
     #/ 0.2D1 + t583 * t1024) - 0.180D3 * t48 * t49 * (-t1024 + t583 * t
     #1025) - t70 * t1055) * t188 / 0.1440D4
      t1080 = FJET(XB1, XB2, s, t493, 0.0D0, 0.0D0, -t499, -t503, t1079)
      t1082 = t231 * t230 + t361 * t360 + t491 * t490 + t603 * t602 + t6
     #66 * t665 + t731 * t730 + t764 * t763 + t811 * t810 + t869 * t868 
     #+ t892 * t891 + t1022 * t1021 + t1080 * t1079
      t1083 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t736, t735, t
     #493, 0.0D0, t738)
      t1088 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t736, t735, t
     #493, 0.0D0, t738)
      t1101 = t191 * t1083 * t77 * t193 / 0.8D1 + (0.90D2 * t5 * t6 * (t
     #1088 - t749 * t1083) - 0.180D3 * t48 * t49 * t1083) * t141 * t188 
     #/ 0.720D3
      t1102 = FJET(XB1, XB2, s, t493, -t736, 0.0D0, t735, t738, t1101)
      t1104 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t606, -t609, 0
     #.0D0, 0.0D0, 0.0D0)
      t1109 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t606, -t609, 0
     #.0D0, 0.0D0, 0.0D0)
      t1115 = t49 * t1104
      t1117 = 0.180D3 * t48 * t1115
      t1132 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t606, -t609, 0
     #.0D0, 0.0D0, 0.0D0)
      t1148 = -t191 * t1104 * t77 * t193 / 0.8D1 + (0.90D2 * t5 * t6 * (
     #-t1109 + t655 * t1104) + t1117) * t141 * t188 / 0.720D3 - (0.90D2 
     #* t5 * t6 * (-t613 * t1104 + t1109) - t1117) * t77 * t141 / 0.1440
     #D4 - (0.90D2 * t5 * t6 * (-t630 * t1109 + t1132 + t631 * t1104 / 0
     #.2D1) - 0.180D3 * t48 * t49 * (t1109 - t630 * t1104) + t70 * t1115
     #) * t141 / 0.1440D4
      t1149 = FJET(XB1, XB2, s, t606, 0.0D0, -t609, 0.0D0, 0.0D0, t1148)
      t1151 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t684, t679, 0
     #.0D0, 0.0D0, 0.0D0)
      t1152 = t1151 * t710
      t1156 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t684, t679, 0
     #.0D0, 0.0D0, 0.0D0)
      t1171 = -t724 * t1152 * t726 / 0.8D1 - (-0.90D2 * t5 * t6 * (-t686
     # * t1156 + t698 * t1151) * t710 - 0.180D3 * t714 * t715 * t1152) *
     # t77 * t141 / 0.1440D4
      t1172 = FJET(XB1, XB2, s, t679, 0.0D0, -t684, 0.0D0, 0.0D0, t1171)
      t1174 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t736, t735, t
     #493, 0.0D0, t738)
      t1179 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t736, t735, t
     #493, 0.0D0, t738)
      t1192 = t191 * t1174 * t77 * t193 / 0.8D1 + (0.90D2 * t5 * t6 * (t
     #1179 - t749 * t1174) - 0.180D3 * t48 * t49 * t1174) * t141 * t188 
     #/ 0.720D3
      t1193 = FJET(XB1, XB2, s, t735, 0.0D0, -t736, t493, t738, t1192)
      t1196 = t493 * t668 * t677
      t1197 = t2 * t494
      t1198 = t115 * t495
      t1201 = Sqrt(t520 * t607 * t18)
      t1202 = t26 * t1201
      t1204 = 0.2D1 * t1202 * x2
      t1205 = t115 * x1
      t1209 = t1197 * (t1198 + 0.1D1 - x3 + t1204 - x2 + t668 + t115 - t
     #1205) * t497 * t677
      t1213 = t18 * s * t1 * x1 * t677
      t1219 = t1197 * x2 * (-0.1D1 + t668 + x1 - t526 - t495 + t527 + 0.
     #2D1 * t1202) * t497 * t677
      t1220 = x2 * x1
      t1222 = t164 * x2
      t1227 = t13 * x2
      t1235 = t1220 * z
      t1238 = -z - x3 + 0.2D1 * t1220 - t1222 + t495 + t532 - t529 - t16
     #5 - 0.2D1 * t1202 * t1220 - 0.2D1 * t1202 * t685 + t165 * t1227 - 
     #t530 * t1220 - 0.2D1 * t165 * t685 + t165 * x2 + 0.2D1 * t668 * t4
     #95 - 0.3D1 * t1235 - t668 * x1
      t1248 = -t164 * t13 * x2 + t1227 * x1 + 0.2D1 * t1222 * z + 0.2D1 
     #* t1202 * z + t1204 - t1205 - t528 + t531 - t535 + t534 + t1198 - 
     #x2 + t704 - t703 + t115 + t685 + 0.2D1 * t1202 * t1235
      t1250 = 0.1D1 / (t1238 + t1248)
      t1252 = x2 - t1220 + z - t685 + t1235
      t1254 = t5 * t6 * t1250 * t1252
      t1255 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t1219, -t1209,
     # -t1213, t1196, t738)
      t1260 = FJET(XB1, XB2, s, t1196, -t1209, -t1213, t1219, t738, t125
     #4 * t496 * t1255 * t726 / 0.8D1)
      t1262 = t49 * t1250
      t1264 = t1252 * t496
      t1269 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t1219, -t1209,
     # -t1213, t1196, t738)
      t1274 = FJET(XB1, XB2, s, t1219, -t1213, -t1209, t1196, t738, t125
     #4 * t496 * t1269 * t726 / 0.8D1)
      t1281 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t606, -t609, 0
     #.0D0, 0.0D0, 0.0D0)
      t1286 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t606, -t609, 0
     #.0D0, 0.0D0, 0.0D0)
      t1292 = t49 * t1281
      t1294 = 0.180D3 * t48 * t1292
      t1309 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t606, -t609, 0
     #.0D0, 0.0D0, 0.0D0)
      t1325 = -t191 * t1281 * t77 * t193 / 0.8D1 + (0.90D2 * t5 * t6 * (
     #-t1286 + t655 * t1281) + t1294) * t141 * t188 / 0.720D3 - (0.90D2 
     #* t5 * t6 * (-t613 * t1281 + t1286) - t1294) * t77 * t141 / 0.1440
     #D4 - (0.90D2 * t5 * t6 * (-t630 * t1286 + t1309 + t631 * t1281 / 0
     #.2D1) - 0.180D3 * t48 * t49 * (-t630 * t1281 + t1286) + t70 * t129
     #2) * t141 / 0.1440D4
      t1326 = FJET(XB1, XB2, s, -t609, 0.0D0, t606, 0.0D0, 0.0D0, t1325)
      t1328 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t499, 
     #t493, 0.0D0, -t503)
      t1329 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t499, 
     #t493, 0.0D0, -t503)
      t1342 = -t505 * t1329 * t537 - t1329
      t1359 = t49 * t1329
      t1369 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t499, 
     #t493, 0.0D0, -t503)
      t1383 = (0.90D2 * t5 * t6 * (-t1328 + t543 * t1329 + (-t505 * t132
     #8 + t515 * t496 * t1329) * t537) - 0.180D3 * t48 * t49 * t1342) * 
     #t77 * t188 / 0.1440D4 + t191 * t1342 * t77 * t193 / 0.8D1 + (0.90D
     #2 * t5 * t6 * (t566 * t1329 - t1328) + 0.180D3 * t48 * t1359) * t1
     #41 * t188 / 0.720D3 + (0.90D2 * t5 * t6 * (-t584 * t1329 / 0.2D1 +
     # t583 * t1328 - t1369) - 0.180D3 * t48 * t49 * (t583 * t1329 - t13
     #28) - t70 * t1359) * t188 / 0.1440D4
      t1384 = FJET(XB1, XB2, s, -t499, 0.0D0, 0.0D0, t493, -t503, t1383)
      t1386 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t736, t735, t
     #493, 0.0D0, t738)
      t1391 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t736, t735, t
     #493, 0.0D0, t738)
      t1404 = t191 * t1386 * t77 * t193 / 0.8D1 + (0.90D2 * t5 * t6 * (t
     #1391 - t749 * t1386) - 0.180D3 * t48 * t49 * t1386) * t141 * t188 
     #/ 0.720D3
      t1405 = FJET(XB1, XB2, s, -t736, t493, t735, 0.0D0, t738, t1404)
      t1407 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t684, t679, 0
     #.0D0, 0.0D0, 0.0D0)
      t1408 = t1407 * t710
      t1412 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t684, t679, 0
     #.0D0, 0.0D0, 0.0D0)
      t1427 = -t724 * t1408 * t726 / 0.8D1 - (-0.90D2 * t5 * t6 * (-t686
     # * t1412 + t698 * t1407) * t710 - 0.180D3 * t714 * t715 * t1408) *
     # t77 * t141 / 0.1440D4
      t1428 = FJET(XB1, XB2, s, -t684, 0.0D0, t679, 0.0D0, 0.0D0, t1427)
      t1430 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t1219, -t1209,
     # -t1213, t1196, t738)
      t1435 = FJET(XB1, XB2, s, -t1213, t1219, t1196, -t1209, t738, t125
     #4 * t496 * t1430 * t726 / 0.8D1)
      t1442 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t1219, -t1209,
     # -t1213, t1196, t738)
      t1447 = FJET(XB1, XB2, s, -t1209, t1196, t1219, -t1213, t738, t125
     #4 * t496 * t1442 * t726 / 0.8D1)
      t1454 = t1102 * t1101 + t1149 * t1148 + t1172 * t1171 + t1193 * t1
     #192 + t1260 * t4 * t1262 * t1264 * t1255 * t726 / 0.8D1 + t1274 * 
     #t4 * t1262 * t1264 * t1269 * t726 / 0.8D1 + t1326 * t1325 + t1384 
     #* t1383 + t1405 * t1404 + t1428 * t1427 + t1435 * t4 * t1262 * t12
     #64 * t1430 * t726 / 0.8D1 + t1447 * t4 * t1262 * t1264 * t1442 * t
     #726 / 0.8D1
      rrgq2qght8s1e0 = t1082 + t1454

      end function



      doubleprecision function rrgq2qght8s1em1
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = x4 * pi
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = x3 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = -0.1D1 + x3
      t23 = log(-0.4D1 * t12 * t17 / t18)
      t25 = cos(t9)
      t27 = Sqrt(-x3 * t18)
      t32 = 0.1D1 / (-z - x3 + 0.2D1 * t25 * t27 * z)
      t36 = log(0.4D1 * t12 * t17)
      t37 = -t23 * z * t32 - t36
      t41 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t45 = lh * t4
      t46 = pi * t6
      t49 = 0.180D3 * t45 * t46 * t7
      t52 = z * t32 + 0.1D1
      t55 = 0.1D1 / x3
      t61 = log(0.4D1 * t14 * t11 * t16)
      t68 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t70 = t61 ** 2
      t73 = lh ** 2
      t75 = pi ** 2
      t77 = 0.180D3 * t73 - 0.30D2 * t75
      t83 = t5 * t6
      t86 = -t7 - z * t7 * t32
      t88 = 0.1D1 / x2
      t92 = x2 ** 2
      t93 = t92 * t11
      t96 = log(0.4D1 * t93 * t17)
      t106 = 0.1D1 / x1
      t110 = x1 ** 2
      t111 = t110 * t11
      t114 = log(0.4D1 * t111 * t17)
      t128 = -(-0.90D2 * t5 * t6 * t7 * t37 + (-0.90D2 * t5 * t6 * t41 +
     # t49) * t52) * t55 / 0.2880D4 - (-0.180D3 * (-t41 + t61 * t7) * lh
     # + 0.90D2 * t61 * t41 - 0.90D2 * t68 - 0.45D2 * t70 * t7 - t7 * t7
     #7) * t6 * t5 / 0.2880D4 - t83 * t86 * t55 * t88 / 0.16D2 - (0.90D2
     # * t5 * t6 * (-t41 + t96 * t7) + t49) * t88 / 0.1440D4 + t83 * t7 
     #* t88 * t106 / 0.8D1 + (0.90D2 * t5 * t6 * (t41 - t114 * t7) - t49
     #) * t106 / 0.1440D4 - t83 * t86 * t55 * t106 / 0.16D2
      t129 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t128)
      t131 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t136 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t142 = 0.180D3 * t45 * t46 * t131
      t150 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t165 = -t131 - z * t131 * t32
      t195 = -(-0.90D2 * t5 * t6 * t131 * t37 + (-0.90D2 * t5 * t6 * t13
     #6 + t142) * t52) * t55 / 0.2880D4 - (-0.45D2 * t70 * t131 - 0.90D2
     # * t150 + 0.90D2 * t61 * t136 - 0.180D3 * (-t136 + t61 * t131) * l
     #h - t131 * t77) * t6 * t5 / 0.2880D4 - t83 * t165 * t55 * t88 / 0.
     #16D2 - (0.90D2 * t5 * t6 * (-t136 + t96 * t131) + t142) * t88 / 0.
     #1440D4 + t83 * t131 * t88 * t106 / 0.8D1 + (0.90D2 * t5 * t6 * (t1
     #36 - t114 * t131) - t142) * t106 / 0.1440D4 - t83 * t165 * t55 * t
     #106 / 0.16D2
      t196 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t195)
      t198 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t203 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t209 = 0.180D3 * t45 * t46 * t198
      t223 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t232 = -z * t198 * t32 - t198
      t262 = -(-0.90D2 * t5 * t6 * t198 * t37 + (-0.90D2 * t5 * t6 * t20
     #3 + t209) * t52) * t55 / 0.2880D4 - (-0.45D2 * t70 * t198 - 0.180D
     #3 * (-t203 + t61 * t198) * lh + 0.90D2 * t61 * t203 - 0.90D2 * t22
     #3 - t198 * t77) * t6 * t5 / 0.2880D4 - t83 * t232 * t55 * t88 / 0.
     #16D2 - (0.90D2 * t5 * t6 * (t96 * t198 - t203) + t209) * t88 / 0.1
     #440D4 + t83 * t198 * t88 * t106 / 0.8D1 + (0.90D2 * t5 * t6 * (-t1
     #14 * t198 + t203) - t209) * t106 / 0.1440D4 - t83 * t232 * t55 * t
     #106 / 0.16D2
      t263 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t262)
      t265 = t2 * x1
      t266 = -0.1D1 + x1
      t267 = x1 * z
      t268 = 0.1D1 - x1 + t267
      t269 = 0.1D1 / t268
      t271 = t2 * t266 * t269
      t272 = s * t15
      t274 = x1 * t266 * t269
      t275 = t272 * t274
      t276 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, t
     #265, 0.0D0, -t275)
      t281 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, t
     #265, 0.0D0, -t275)
      t284 = t266 ** 2
      t288 = log(0.4D1 * t111 * t14 * t16 * t269 * t284)
      t300 = z * t268
      t301 = x3 * t110
      t304 = Sqrt(-x3 * t268 * t18)
      t308 = x3 * x1
      t318 = -z - x3 + t267 - t301 + 0.2D1 * t25 * t304 * z - 0.3D1 * t3
     #08 * z - x1 * t13 + x3 * t13 * x1 + 0.2D1 * t308 + 0.2D1 * t301 * 
     #z - t301 * t13
      t319 = 0.1D1 / t318
      t327 = -t83 * t276 * t88 * t106 / 0.8D1 + (0.90D2 * t5 * t6 * (-t2
     #81 + t288 * t276) + 0.180D3 * t45 * t46 * t276) * t106 / 0.1440D4 
     #+ t83 * (-t276 - t300 * t276 * t319) * t55 * t106 / 0.16D2
      t328 = FJET(XB1, XB2, s, 0.0D0, t265, -t271, 0.0D0, -t275, t327)
      t330 = x2 * s
      t331 = t330 * t1
      t332 = -0.1D1 + x2
      t333 = t332 * s
      t334 = t333 * t1
      t335 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t331, -t334, 0.
     #0D0, 0.0D0, 0.0D0)
      t340 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t331, -t334, 0.
     #0D0, 0.0D0, 0.0D0)
      t344 = log(-0.4D1 * t93 * t17 * t332)
      t360 = -t83 * t335 * t55 * t88 / 0.16D2 - (0.90D2 * t5 * t6 * (t34
     #0 - t344 * t335) - 0.180D3 * t45 * t46 * t335) * t88 / 0.1440D4 - 
     #t83 * t335 * t88 * t106 / 0.8D1
      t361 = FJET(XB1, XB2, s, 0.0D0, t331, 0.0D0, -t334, 0.0D0, t360)
      t363 = x2 * x3
      t364 = t92 * x3
      t367 = Sqrt(x3 * t332 * t18)
      t368 = t25 * t367
      t370 = 0.2D1 * t368 * x2
      t373 = 0.1D1 / (0.1D1 - x3 + t363)
      t375 = t2 * (0.1D1 - x3 - x2 + t363 + t364 + t370) * t373
      t380 = t2 * x2 * (-0.1D1 + t363 + 0.2D1 * t368) * t373
      t381 = x2 * z
      t382 = -z + t381 - x2
      t384 = t5 * t6 * t382
      t385 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t380, t375, 0.
     #0D0, 0.0D0, 0.0D0)
      t393 = 0.1D1 / (z - t381 + t364 * z - t363 * z + x3 - t364 + x2 - 
     #t370 - 0.2D1 * z * t368 + 0.2D1 * t368 * t381)
      t395 = t55 * t88
      t399 = FJET(XB1, XB2, s, 0.0D0, t375, 0.0D0, -t380, 0.0D0, -t384 *
     # t385 * t393 * t395 / 0.16D2)
      t404 = t393 * t55 * t88
      t408 = t1 * t266
      t410 = t333 * t408 * t269
      t411 = t330 * t408
      t413 = t272 * t332 * t274
      t414 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t411, t410, t2
     #65, 0.0D0, t413)
      t419 = FJET(XB1, XB2, s, 0.0D0, t410, t265, -t411, t413, t83 * t41
     #4 * t88 * t106 / 0.8D1)
      t423 = t88 * t106
      t427 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t331, -t334, 0.
     #0D0, 0.0D0, 0.0D0)
      t433 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t331, -t334, 0.
     #0D0, 0.0D0, 0.0D0)
      t448 = -t83 * t427 * t55 * t88 / 0.16D2 - (0.90D2 * t5 * t6 * (-t3
     #44 * t427 + t433) - 0.180D3 * t45 * t46 * t427) * t88 / 0.1440D4 -
     # t83 * t427 * t88 * t106 / 0.8D1
      t449 = FJET(XB1, XB2, s, 0.0D0, -t334, 0.0D0, t331, 0.0D0, t448)
      t451 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, t
     #265, 0.0D0, -t275)
      t457 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, t
     #265, 0.0D0, -t275)
      t475 = -t83 * t451 * t88 * t106 / 0.8D1 + (0.90D2 * t5 * t6 * (t28
     #8 * t451 - t457) + 0.180D3 * t45 * t46 * t451) * t106 / 0.1440D4 +
     # t83 * (-t451 - t300 * t451 * t319) * t55 * t106 / 0.16D2
      t476 = FJET(XB1, XB2, s, 0.0D0, -t271, t265, 0.0D0, -t275, t475)
      t478 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t380, t375, 0.
     #0D0, 0.0D0, 0.0D0)
      t483 = FJET(XB1, XB2, s, 0.0D0, -t380, 0.0D0, t375, 0.0D0, -t384 *
     # t478 * t393 * t395 / 0.16D2)
      t490 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t491 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t500 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t509 = -t491 - z * t491 * t32
      t521 = 0.180D3 * t45 * t46 * t491
      t554 = -(-0.180D3 * (-t490 + t61 * t491) * lh + 0.90D2 * t61 * t49
     #0 - 0.45D2 * t70 * t491 - 0.90D2 * t500 - t491 * t77) * t6 * t5 / 
     #0.2880D4 - t83 * t509 * t55 * t88 / 0.16D2 - (0.90D2 * t5 * t6 * (
     #t96 * t491 - t490) + t521) * t88 / 0.1440D4 - (-0.90D2 * t5 * t6 *
     # t491 * t37 + (-0.90D2 * t5 * t6 * t490 + t521) * t52) * t55 / 0.2
     #880D4 + t83 * t491 * t88 * t106 / 0.8D1 + (0.90D2 * t5 * t6 * (-t1
     #14 * t491 + t490) - t521) * t106 / 0.1440D4 - t83 * t509 * t55 * t
     #106 / 0.16D2
      t555 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t554)
      t557 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, t
     #265, 0.0D0, -t275)
      t562 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, t
     #265, 0.0D0, -t275)
      t581 = -t83 * t557 * t88 * t106 / 0.8D1 + (0.90D2 * t5 * t6 * (-t5
     #62 + t288 * t557) + 0.180D3 * t45 * t46 * t557) * t106 / 0.1440D4 
     #+ t83 * (-t300 * t557 * t319 - t557) * t55 * t106 / 0.16D2
      t582 = FJET(XB1, XB2, s, t265, 0.0D0, 0.0D0, -t271, -t275, t581)
      t584 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t411, t410, t2
     #65, 0.0D0, t413)
      t589 = FJET(XB1, XB2, s, t265, -t411, 0.0D0, t410, t413, t83 * t58
     #4 * t88 * t106 / 0.8D1)
      t596 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t331, -t334, 0.
     #0D0, 0.0D0, 0.0D0)
      t605 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t331, -t334, 0.
     #0D0, 0.0D0, 0.0D0)
      t617 = -t83 * t596 * t88 * t106 / 0.8D1 - t83 * t596 * t55 * t88 /
     # 0.16D2 - (0.90D2 * t5 * t6 * (t605 - t344 * t596) - 0.180D3 * t45
     # * t46 * t596) * t88 / 0.1440D4
      t618 = FJET(XB1, XB2, s, t331, 0.0D0, -t334, 0.0D0, 0.0D0, t617)
      t620 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t380, t375, 0.
     #0D0, 0.0D0, 0.0D0)
      t625 = FJET(XB1, XB2, s, t375, 0.0D0, -t380, 0.0D0, 0.0D0, -t384 *
     # t620 * t393 * t395 / 0.16D2)
      t632 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t411, t410, t2
     #65, 0.0D0, t413)
      t637 = FJET(XB1, XB2, s, t410, 0.0D0, -t411, t265, t413, t83 * t63
     #2 * t88 * t106 / 0.8D1)
      t644 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t331, -t334, 0.
     #0D0, 0.0D0, 0.0D0)
      t654 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t331, -t334, 0.
     #0D0, 0.0D0, 0.0D0)
      t665 = -t83 * t644 * t88 * t106 / 0.8D1 - t83 * t644 * t55 * t88 /
     # 0.16D2 - (0.90D2 * t5 * t6 * (-t344 * t644 + t654) - 0.180D3 * t4
     #5 * t46 * t644) * t88 / 0.1440D4
      t666 = FJET(XB1, XB2, s, -t334, 0.0D0, t331, 0.0D0, 0.0D0, t665)
      t668 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, t
     #265, 0.0D0, -t275)
      t674 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t271, t
     #265, 0.0D0, -t275)
      t692 = -t83 * t668 * t88 * t106 / 0.8D1 + (0.90D2 * t5 * t6 * (t28
     #8 * t668 - t674) + 0.180D3 * t45 * t46 * t668) * t106 / 0.1440D4 +
     # t83 * (-t300 * t668 * t319 - t668) * t55 * t106 / 0.16D2
      t693 = FJET(XB1, XB2, s, -t271, 0.0D0, 0.0D0, t265, -t275, t692)
      t695 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t411, t410, t2
     #65, 0.0D0, t413)
      t700 = FJET(XB1, XB2, s, -t411, t265, t410, 0.0D0, t413, t83 * t69
     #5 * t88 * t106 / 0.8D1)
      t707 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t380, t375, 0.
     #0D0, 0.0D0, 0.0D0)
      t712 = FJET(XB1, XB2, s, -t380, 0.0D0, t375, 0.0D0, 0.0D0, -t384 *
     # t707 * t393 * t395 / 0.16D2)
      rrgq2qght8s1em1 = t129 * t128 + t196 * t195 + t263 * t262 + t328 *
     # t327 + t361 * t360 - t399 * t4 * t46 * t382 * t385 * t404 / 0.16D
     #2 + t419 * t4 * pi * t6 * t414 * t423 / 0.8D1 + t449 * t448 + t476
     # * t475 - t483 * t4 * t46 * t382 * t478 * t404 / 0.16D2 + t555 * t
     #554 + t582 * t581 + t589 * t4 * pi * t6 * t584 * t423 / 0.8D1 + t6
     #18 * t617 - t625 * t4 * t46 * t382 * t620 * t404 / 0.16D2 + t637 *
     # t4 * pi * t6 * t632 * t423 / 0.8D1 + t666 * t665 + t693 * t692 + 
     #t700 * t4 * pi * t6 * t695 * t423 / 0.8D1 - t712 * t4 * t46 * t382
     # * t707 * t404 / 0.16D2

      end function



      doubleprecision function rrgq2qght8s1em2
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = t5 * t6
      t8 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = x4 * pi
      t10 = cos(t9)
      t13 = Sqrt(-x3 * (-0.1D1 + x3))
      t20 = z / (-z - x3 + 0.2D1 * t10 * t13 * z) + 0.1D1
      t22 = 0.1D1 / x3
      t26 = t6 * t8
      t27 = 0.1D1 / x2
      t31 = 0.1D1 / x1
      t37 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t39 = z ** 2
      t41 = Sin(t9)
      t42 = t41 ** 2
      t44 = t1 ** 2
      t45 = t44 ** 2
      t48 = log(0.4D1 / t39 * t42 * t45)
      t55 = t7 * t8 * t20 * t22 / 0.32D2 + t5 * t26 * t27 / 0.16D2 + t5 
     #* t26 * t31 / 0.16D2 - (0.180D3 * t8 * lh - 0.90D2 * t37 + 0.90D2 
     #* t48 * t8) * t6 * t5 / 0.2880D4
      t56 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t55)
      t58 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t63 = t6 * t58
      t70 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t80 = t7 * t58 * t20 * t22 / 0.32D2 + t5 * t63 * t27 / 0.16D2 + t5
     # * t63 * t31 / 0.16D2 - (-0.90D2 * t70 + 0.180D3 * t58 * lh + 0.90
     #D2 * t48 * t58) * t6 * t5 / 0.2880D4
      t81 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t80)
      t83 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t88 = t6 * t83
      t97 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t105 = t7 * t83 * t20 * t22 / 0.32D2 + t5 * t88 * t27 / 0.16D2 + t
     #5 * t88 * t31 / 0.16D2 - (0.90D2 * t48 * t83 - 0.90D2 * t97 + 0.18
     #0D3 * t83 * lh) * t6 * t5 / 0.2880D4
      t106 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t105)
      t108 = t2 * x1
      t109 = -0.1D1 + x1
      t112 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t114 = t2 * t109 * t112
      t118 = s * t44 * x1 * t109 * t112
      t119 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t114, t
     #108, 0.0D0, -t118)
      t121 = t6 * t119 * t31
      t124 = FJET(XB1, XB2, s, 0.0D0, t108, -t114, 0.0D0, -t118, -t5 * t
     #121 / 0.16D2)
      t130 = x2 * s * t1
      t133 = (-0.1D1 + x2) * s * t1
      t134 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t130, -t133, 0.
     #0D0, 0.0D0, 0.0D0)
      t136 = t6 * t134 * t27
      t139 = FJET(XB1, XB2, s, 0.0D0, t130, 0.0D0, -t133, 0.0D0, -t5 * t
     #136 / 0.16D2)
      t144 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t130, -t133, 0.
     #0D0, 0.0D0, 0.0D0)
      t146 = t6 * t144 * t27
      t149 = FJET(XB1, XB2, s, 0.0D0, -t133, 0.0D0, t130, 0.0D0, -t5 * t
     #146 / 0.16D2)
      t154 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t114, t
     #108, 0.0D0, -t118)
      t156 = t6 * t154 * t31
      t159 = FJET(XB1, XB2, s, 0.0D0, -t114, t108, 0.0D0, -t118, -t5 * t
     #156 / 0.16D2)
      t164 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t169 = t6 * t164
      t178 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t186 = t7 * t164 * t20 * t22 / 0.32D2 + t5 * t169 * t27 / 0.16D2 +
     # t5 * t169 * t31 / 0.16D2 - (0.180D3 * t164 * lh - 0.90D2 * t178 +
     # 0.90D2 * t48 * t164) * t6 * t5 / 0.2880D4
      t187 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t186)
      t189 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t114, t
     #108, 0.0D0, -t118)
      t191 = t6 * t189 * t31
      t194 = FJET(XB1, XB2, s, t108, 0.0D0, 0.0D0, -t114, -t118, -t5 * t
     #191 / 0.16D2)
      t199 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t130, -t133, 0.
     #0D0, 0.0D0, 0.0D0)
      t201 = t6 * t199 * t27
      t204 = FJET(XB1, XB2, s, t130, 0.0D0, -t133, 0.0D0, 0.0D0, -t5 * t
     #201 / 0.16D2)
      t209 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t130, -t133, 0.
     #0D0, 0.0D0, 0.0D0)
      t211 = t6 * t209 * t27
      t214 = FJET(XB1, XB2, s, -t133, 0.0D0, t130, 0.0D0, 0.0D0, -t5 * t
     #211 / 0.16D2)
      t219 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t114, t
     #108, 0.0D0, -t118)
      t221 = t6 * t219 * t31
      t224 = FJET(XB1, XB2, s, -t114, 0.0D0, 0.0D0, t108, -t118, -t5 * t
     #221 / 0.16D2)
      rrgq2qght8s1em2 = t56 * t55 + t81 * t80 + t106 * t105 - t124 * t4 
     #* pi * t121 / 0.16D2 - t139 * t4 * pi * t136 / 0.16D2 - t149 * t4 
     #* pi * t146 / 0.16D2 - t159 * t4 * pi * t156 / 0.16D2 + t187 * t18
     #6 - t194 * t4 * pi * t191 / 0.16D2 - t204 * t4 * pi * t201 / 0.16D
     #2 - t214 * t4 * pi * t211 / 0.16D2 - t224 * t4 * pi * t221 / 0.16D
     #2

      end function



      doubleprecision function rrgq2qght8s1em3
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t5 * t6 * 
     #t7 / 0.32D2)
      t13 = pi * t6
      t16 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t5 * t6 * 
     #t16 / 0.32D2)
      t24 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t5 * t6 * 
     #t24 / 0.32D2)
      t32 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t5 * t6 * 
     #t32 / 0.32D2)
      rrgq2qght8s1em3 = t11 * t4 * t13 * t7 / 0.32D2 + t20 * t4 * t13 * 
     #t16 / 0.32D2 + t28 * t4 * t13 * t24 / 0.32D2 + t36 * t4 * t13 * t3
     #2 / 0.32D2

      end function



      doubleprecision function rrgq2qght8s1em4
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght8s1em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght8s2e1
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = lh ** 2
      t8 = pi ** 2
      t10 = 0.180D3 * t6 - 0.30D2 * t8
      t11 = s ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t14 = 0.1D1 / t1
      t15 = pi * t14
      t16 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0, 
     #0.0D0, 0.0D0)
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = x3 * t18
      t20 = x4 * pi
      t21 = Sin(t20)
      t22 = t21 ** 2
      t23 = z ** 2
      t24 = 0.1D1 / t23
      t25 = t22 * t24
      t26 = t25 * t4
      t29 = log(-0.4D1 * t19 * t26)
      t30 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0, 
     #0.0D0, 0.0D0)
      t35 = t12 * pi
      t36 = t29 ** 2
      t37 = t36 * t29
      t40 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0, 
     #0.0D0, 0.0D0)
      t42 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0, 
     #0.0D0, 0.0D0)
      t54 = 0.60D2 * lh * t8 - 0.240D3 * zeta3 - 0.120D3 * t6 * lh
      t55 = t54 * t12
      t56 = t15 * t30
      t58 = lh * t12
      t67 = 0.1D1 / x3
      t70 = x1 ** 2
      t71 = x3 * t70
      t75 = log(-0.4D1 * t71 * t18 * t26)
      t76 = t75 ** 2
      t89 = t13 * t56
      t92 = 0.1D1 / x1
      t95 = t71 * x2
      t96 = t18 * t22
      t101 = log(-0.4D1 * t95 * t96 * t24 * t4)
      t111 = 0.1D1 / x2
      t112 = t111 * t92
      t120 = log(-0.4D1 * x2 * t22 * t24 * t19 * t4)
      t121 = t120 ** 2
      t138 = -(t13 * t15 * (t16 - t29 * t30) + 0.90D2 * t35 * t14 * (-t3
     #7 * t30 / 0.6D1 - t29 * t40 + t42 + t36 * t16 / 0.2D1) + t55 * t56
     # - 0.180D3 * t58 * t15 * (-t29 * t16 + t40 + t36 * t30 / 0.2D1)) *
     # t67 / 0.1440D4 - (0.90D2 * t35 * t14 * (t40 + t76 * t30 / 0.2D1 -
     # t75 * t16) - 0.180D3 * t58 * t15 * (-t75 * t30 + t16) + t89) * t6
     #7 * t92 / 0.720D3 + (0.90D2 * t35 * t14 * (t101 * t30 - t16) + 0.1
     #80D3 * t58 * t56) * t67 * t112 / 0.720D3 - (0.90D2 * t35 * t14 * (
     #t121 * t30 / 0.2D1 + t40 - t120 * t16) - 0.180D3 * t58 * t15 * (t1
     #6 - t120 * t30) + t89) * t67 * t111 / 0.1440D4
      t139 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t138)
      t141 = t4 * s
      t142 = -0.1D1 + x1
      t143 = t1 * t142
      t144 = t141 * t143
      t145 = t1 * x1
      t146 = t141 * t145
      t148 = x3 * s * t143
      t149 = x3 * x1
      t150 = t2 * t149
      t151 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t144, -t148, -t
     #146, t150, 0.0D0)
      t152 = t70 * t22
      t153 = t24 * t18
      t154 = t152 * t153
      t156 = x1 * z
      t157 = 0.1D1 - x1 + t156
      t158 = 0.1D1 / t157
      t159 = t142 ** 2
      t160 = t158 * t159
      t164 = log(-0.4D1 * t154 * x3 * t4 * t160)
      t165 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t144, -t148, -t
     #146, t150, 0.0D0)
      t167 = t164 ** 2
      t168 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t144, -t148, -t
     #146, t150, 0.0D0)
      t180 = t15 * t168
      t185 = x2 * x3
      t187 = t185 * t70 * t18
      t188 = t160 * t4
      t192 = log(-0.4D1 * t187 * t25 * t188)
      t204 = -(0.90D2 * t35 * t14 * (-t151 + t164 * t165 - t167 * t168 /
     # 0.2D1) - 0.180D3 * t58 * t15 * (-t165 + t164 * t168) - t13 * t180
     #) * t67 * t92 / 0.720D3 + (0.90D2 * t35 * t14 * (-t192 * t168 + t1
     #65) - 0.180D3 * t58 * t180) * t67 * t112 / 0.720D3
      t205 = FJET(XB1, XB2, s, t144, -t146, -t148, t150, 0.0D0, t204)
      t207 = -0.1D1 + x2
      t208 = t207 * s
      t209 = t208 * t1
      t211 = x2 * s * t1
      t212 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0.
     #0D0, 0.0D0, 0.0D0)
      t213 = t207 ** 2
      t214 = t24 * t213
      t218 = log(0.4D1 * t95 * t96 * t214)
      t219 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0.
     #0D0, 0.0D0, 0.0D0)
      t225 = t15 * t219
      t232 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0.
     #0D0, 0.0D0, 0.0D0)
      t233 = x2 * t70
      t234 = t233 * t18
      t235 = t25 * t213
      t238 = log(0.4D1 * t234 * t235)
      t239 = t238 ** 2
      t252 = t13 * t225
      t260 = log(0.4D1 * t185 * t18 * t235)
      t261 = t260 ** 2
      t278 = x2 * t18
      t281 = log(0.4D1 * t278 * t235)
      t286 = t281 ** 2
      t287 = t286 * t281
      t292 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0.
     #0D0, 0.0D0, 0.0D0)
      t309 = (0.90D2 * t35 * t14 * (-t212 + t218 * t219) + 0.180D3 * t58
     # * t225) * t67 * t112 / 0.720D3 + (0.90D2 * t35 * t14 * (-t232 - t
     #239 * t219 / 0.2D1 + t238 * t212) - 0.180D3 * t58 * t15 * (-t212 +
     # t238 * t219) - t252) * t111 * t92 / 0.720D3 - (0.90D2 * t35 * t14
     # * (t232 + t261 * t219 / 0.2D1 - t260 * t212) - 0.180D3 * t58 * t1
     #5 * (t212 - t260 * t219) + t252) * t67 * t111 / 0.1440D4 + (t13 * 
     #t15 * (-t212 + t281 * t219) + 0.90D2 * t35 * t14 * (t287 * t219 / 
     #0.6D1 - t286 * t212 / 0.2D1 - t292 + t281 * t232) - t55 * t225 - 0
     #.180D3 * t58 * t15 * (-t286 * t219 / 0.2D1 - t232 + t281 * t212)) 
     #* t111 / 0.1440D4
      t310 = FJET(XB1, XB2, s, -t209, 0.0D0, t211, 0.0D0, 0.0D0, t309)
      t312 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0,
     # 0.0D0, 0.0D0)
      t314 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0,
     # 0.0D0, 0.0D0)
      t317 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0,
     # 0.0D0, 0.0D0)
      t327 = t15 * t314
      t328 = t13 * t327
      t352 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0,
     # 0.0D0, 0.0D0)
      t385 = -(0.90D2 * t35 * t14 * (-t75 * t312 + t76 * t314 / 0.2D1 + 
     #t317) - 0.180D3 * t58 * t15 * (t312 - t75 * t314) + t328) * t67 * 
     #t92 / 0.720D3 + (0.90D2 * t35 * t14 * (-t312 + t101 * t314) + 0.18
     #0D3 * t58 * t327) * t67 * t112 / 0.720D3 - (t13 * t15 * (-t29 * t3
     #14 + t312) + 0.90D2 * t35 * t14 * (t36 * t312 / 0.2D1 - t37 * t314
     # / 0.6D1 + t352 - t29 * t317) + t55 * t327 - 0.180D3 * t58 * t15 *
     # (t36 * t314 / 0.2D1 + t317 - t29 * t312)) * t67 / 0.1440D4 - (0.9
     #0D2 * t35 * t14 * (t121 * t314 / 0.2D1 + t317 - t120 * t312) - 0.1
     #80D3 * t58 * t15 * (-t120 * t314 + t312) + t328) * t67 * t111 / 0.
     #1440D4
      t386 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t385)
      t388 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0,
     # 0.0D0, 0.0D0)
      t390 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0,
     # 0.0D0, 0.0D0)
      t391 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0,
     # 0.0D0, 0.0D0)
      t403 = t15 * t391
      t404 = t13 * t403
      t418 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0,
     # 0.0D0, 0.0D0)
      t461 = -(0.90D2 * t35 * t14 * (-t120 * t388 + t390 + t121 * t391 /
     # 0.2D1) - 0.180D3 * t58 * t15 * (t388 - t120 * t391) + t404) * t67
     # * t111 / 0.1440D4 - (t13 * t15 * (-t29 * t391 + t388) + 0.90D2 * 
     #t35 * t14 * (-t37 * t391 / 0.6D1 + t36 * t388 / 0.2D1 - t29 * t390
     # + t418) + t55 * t403 - 0.180D3 * t58 * t15 * (t390 + t36 * t391 /
     # 0.2D1 - t29 * t388)) * t67 / 0.1440D4 - (0.90D2 * t35 * t14 * (-t
     #75 * t388 + t76 * t391 / 0.2D1 + t390) - 0.180D3 * t58 * t15 * (-t
     #75 * t391 + t388) + t404) * t67 * t92 / 0.720D3 + (0.90D2 * t35 * 
     #t14 * (-t388 + t101 * t391) + 0.180D3 * t58 * t403) * t67 * t112 /
     # 0.720D3
      t462 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t461)
      t464 = cos(t20)
      t466 = Sqrt(-t185 * t4)
      t467 = t464 * t466
      t468 = 0.2D1 * t467
      t471 = -0.1D1 + t185
      t472 = 0.1D1 / t471
      t474 = t2 * t207 * (-t185 - 0.1D1 + x3 + t468) * t472
      t475 = 0.3D1 * t185
      t476 = x2 ** 2
      t477 = t476 * x3
      t479 = 0.2D1 * t467 * x2
      t482 = t2 * (-x2 - x3 + t475 - t477 - t468 + t479) * t472
      t483 = x2 * z
      t484 = 0.1D1 - x2 + t483
      t485 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t474, t482, 0.
     #0D0, 0.0D0, 0.0D0)
      t486 = t484 * t485
      t488 = t471 ** 2
      t489 = 0.1D1 / t488
      t494 = log(-0.4D1 * t187 * t25 * t213 * t4 * t489)
      t495 = t494 * t484
      t496 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t474, t482, 0.
     #0D0, 0.0D0, 0.0D0)
      t500 = t477 * z
      t501 = t185 * z
      t502 = 0.2D1 * t185
      t506 = 0.1D1 / (-t500 + t477 - t483 + t501 + x2 - t502 - t479 + 0.
     #2D1 * t467 * t483 + t468 - 0.1D1)
      t510 = t58 * pi
      t511 = t14 * t484
      t513 = t511 * t496 * t506
      t520 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t474, t482, 0.
     #0D0, 0.0D0, 0.0D0)
      t527 = log(-0.4D1 * t185 * t96 * t214 * t4 * t489)
      t528 = t527 * t484
      t530 = t527 ** 2
      t531 = t530 * t484
      t545 = t13 * pi
      t551 = (-0.90D2 * t35 * t14 * (t486 - t495 * t496) * t506 + 0.180D
     #3 * t510 * t513) * t67 * t112 / 0.720D3 - (0.90D2 * t35 * t14 * (t
     #484 * t520 - t528 * t485 + t531 * t496 / 0.2D1) * t506 - 0.180D3 *
     # t510 * t14 * (t486 - t528 * t496) * t506 + t545 * t513) * t67 * t
     #111 / 0.1440D4
      t552 = FJET(XB1, XB2, s, -t474, 0.0D0, t482, 0.0D0, 0.0D0, t551)
      t554 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t474, t482, 0.
     #0D0, 0.0D0, 0.0D0)
      t556 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t474, t482, 0.
     #0D0, 0.0D0, 0.0D0)
      t558 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t474, t482, 0.
     #0D0, 0.0D0, 0.0D0)
      t566 = t484 * t556
      t574 = t511 * t558 * t506
      t592 = -(0.90D2 * t35 * t14 * (t484 * t554 - t528 * t556 + t531 * 
     #t558 / 0.2D1) * t506 - 0.180D3 * t510 * t14 * (t566 - t528 * t558)
     # * t506 + t545 * t574) * t67 * t111 / 0.1440D4 + (-0.90D2 * t35 * 
     #t14 * (t566 - t495 * t558) * t506 + 0.180D3 * t510 * t574) * t67 *
     # t112 / 0.720D3
      t593 = FJET(XB1, XB2, s, 0.0D0, t482, 0.0D0, -t474, 0.0D0, t592)
      t595 = t2 * x1
      t598 = t595 * x3 * t207 * t472
      t599 = t2 * t142
      t600 = t185 * t156
      t602 = t477 * t156
      t606 = Sqrt(-x3 * t157 * x2 * t4)
      t607 = t464 * t606
      t608 = 0.2D1 * t607
      t610 = 0.2D1 * t607 * x2
      t611 = t149 * z
      t612 = t185 * x1
      t614 = t477 * x1
      t615 = 0.2D1 * t600 - t602 - t608 - x2 - x3 + t610 - t611 - 0.2D1 
     #* t612 + t475 + t149 - t477 + t614
      t618 = t599 * t615 * t158 * t472
      t620 = t141 * t145 * t472
      t625 = t599 * t207 * (-t185 - 0.1D1 + x3 + x1 - t149 - t156 + t611
     # + t608) * t158 * t472
      t630 = s * t17 * x2 * x1 * t142 * t158
      t631 = x2 * x1
      t632 = t631 * z
      t633 = -0.1D1 - t156 + x1 + x2 + t632 - t631 - t483
      t634 = t157 * t633
      t635 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t625, -t618, t6
     #20, t598, -t630)
      t643 = log(-0.4D1 * t185 * t188 * t235 * t489 * t70 * t18)
      t644 = t643 * t157
      t645 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t625, -t618, t6
     #20, t598, -t630)
      t664 = t23 * x2
      t667 = t70 * t23
      t670 = 0.1D1 + t483 - t608 + 0.2D1 * t607 * t632 - 0.2D1 * x1 + 0.
     #4D1 * t600 - t602 - 0.2D1 * t607 * t631 - 0.2D1 * t607 * t156 - 0.
     #2D1 * t607 * t483 - x3 * t23 * t631 - 0.2D1 * t71 * t483 + t71 * t
     #664 + 0.2D1 * t156 - t667 * x2 + t664 * x1
      t680 = 0.2D1 * t233 * z + 0.2D1 * t607 * x1 + t95 + t70 + 0.2D1 * 
     #t631 - t233 - 0.2D1 * t70 * z + t667 - x2 + t500 - t501 + t610 - 0
     #.3D1 * t612 + t614 - 0.3D1 * t632 + t502 - t477
      t682 = 0.1D1 / (t670 + t680)
      t686 = t58 * t15
      t691 = 0.90D2 * t35 * t14 * (t634 * t635 - t644 * t633 * t645) * t
     #682 - 0.180D3 * t686 * t634 * t645 * t682
      t695 = FJET(XB1, XB2, s, t598, -t618, t620, t625, -t630, t691 * t6
     #7 * t112 / 0.720D3)
      t698 = t67 * t111 * t92
      t701 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t625, -t618, t6
     #20, t598, -t630)
      t703 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t625, -t618, t6
     #20, t598, -t630)
      t715 = 0.90D2 * t35 * t14 * (t634 * t701 - t644 * t633 * t703) * t
     #682 - 0.180D3 * t686 * t634 * t703 * t682
      t719 = FJET(XB1, XB2, s, t625, t620, -t618, t598, -t630, t715 * t6
     #7 * t112 / 0.720D3)
      t723 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t625, -t618, t6
     #20, t598, -t630)
      t725 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t625, -t618, t6
     #20, t598, -t630)
      t737 = 0.90D2 * t35 * t14 * (t634 * t723 - t644 * t633 * t725) * t
     #682 - 0.180D3 * t686 * t634 * t725 * t682
      t741 = FJET(XB1, XB2, s, -t618, t598, t625, t620, -t630, t737 * t6
     #7 * t112 / 0.720D3)
      t745 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t144, -t148, -t
     #146, t150, 0.0D0)
      t748 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t144, -t148, -t
     #146, t150, 0.0D0)
      t749 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t144, -t148, -t
     #146, t150, 0.0D0)
      t760 = t15 * t745
      t776 = -(0.90D2 * t35 * t14 * (-t167 * t745 / 0.2D1 - t748 + t164 
     #* t749) - 0.180D3 * t58 * t15 * (-t749 + t164 * t745) - t13 * t760
     #) * t67 * t92 / 0.720D3 + (0.90D2 * t35 * t14 * (-t192 * t745 + t7
     #49) - 0.180D3 * t58 * t760) * t67 * t112 / 0.720D3
      t777 = FJET(XB1, XB2, s, -t148, t150, t144, -t146, 0.0D0, t776)
      t779 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t474, t482, 0.
     #0D0, 0.0D0, 0.0D0)
      t780 = t484 * t779
      t781 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t474, t482, 0.
     #0D0, 0.0D0, 0.0D0)
      t789 = t511 * t781 * t506
      t796 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t474, t482, 0.
     #0D0, 0.0D0, 0.0D0)
      t817 = (-0.90D2 * t35 * t14 * (t780 - t495 * t781) * t506 + 0.180D
     #3 * t510 * t789) * t67 * t112 / 0.720D3 - (0.90D2 * t35 * t14 * (t
     #484 * t796 - t528 * t779 + t531 * t781 / 0.2D1) * t506 - 0.180D3 *
     # t510 * t14 * (t780 - t528 * t781) * t506 + t545 * t789) * t67 * t
     #111 / 0.1440D4
      t818 = FJET(XB1, XB2, s, 0.0D0, -t474, 0.0D0, t482, 0.0D0, t817)
      t820 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t625, -t618, t6
     #20, t598, -t630)
      t822 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t625, -t618, t6
     #20, t598, -t630)
      t834 = 0.90D2 * t35 * t14 * (t634 * t820 - t644 * t633 * t822) * t
     #682 - 0.180D3 * t686 * t634 * t822 * t682
      t838 = FJET(XB1, XB2, s, t620, t625, t598, -t618, -t630, t834 * t6
     #7 * t112 / 0.720D3)
      t845 = log(0.4D1 * x3 * t22 * t153)
      t846 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t848 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t852 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t853 = t845 ** 2
      t854 = t853 * t845
      t859 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t865 = t15 * t846
      t866 = t55 * t865
      t877 = t25 * t18
      t879 = log(0.4D1 * t877)
      t883 = t879 ** 2
      t888 = t883 ** 2
      t896 = t883 * t879
      t903 = rrgq2qgh82J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t907 = t8 ** 2
      t908 = t6 ** 2
      t914 = t907 + 0.60D2 * t908 + 0.480D3 * lh * zeta3 - 0.60D2 * t6 *
     # t8
      t924 = log(0.4D1 * t71 * t877)
      t926 = t924 ** 2
      t938 = t13 * t865
      t944 = log(0.4D1 * t154)
      t949 = t944 ** 2
      t950 = t949 * t944
      t972 = log(0.4D1 * t95 * t877)
      t986 = log(0.4D1 * t233 * t877)
      t987 = t986 ** 2
      t1006 = log(0.4D1 * t185 * t877)
      t1008 = t1006 ** 2
      t1026 = log(0.4D1 * t278 * t25)
      t1031 = t1026 ** 2
      t1032 = t1031 * t1026
      t1052 = -(t13 * t15 * (t845 * t846 - t848) + 0.90D2 * t35 * t14 * 
     #(-t852 + t854 * t846 / 0.6D1 - t853 * t848 / 0.2D1 + t845 * t859) 
     #- t866 - 0.180D3 * t58 * t15 * (-t859 + t845 * t848 - t853 * t846 
     #/ 0.2D1)) * t67 / 0.1440D4 + (-0.90D2 * t879 * t852 + (t859 - t879
     # * t848 + t883 * t846 / 0.2D1) * t10 + 0.15D2 / 0.4D1 * t888 * t84
     #6 + (-t879 * t846 + t848) * t54 - 0.180D3 * (t852 + t883 * t848 / 
     #0.2D1 - t896 * t846 / 0.6D1 - t879 * t859) * lh + 0.90D2 * t903 + 
     #0.45D2 * t883 * t859 + t846 * t914 - 0.15D2 * t896 * t848) * t14 *
     # t35 / 0.1440D4 - (0.90D2 * t35 * t14 * (t924 * t848 - t859 - t926
     # * t846 / 0.2D1) - 0.180D3 * t58 * t15 * (-t848 + t924 * t846) - t
     #938) * t67 * t92 / 0.720D3 + (t13 * t15 * (-t944 * t846 + t848) + 
     #0.90D2 * t35 * t14 * (t852 - t950 * t846 / 0.6D1 + t949 * t848 / 0
     #.2D1 - t944 * t859) + t866 - 0.180D3 * t58 * t15 * (t859 - t944 * 
     #t848 + t949 * t846 / 0.2D1)) * t92 / 0.720D3 + (0.90D2 * t35 * t14
     # * (t848 - t972 * t846) - 0.180D3 * t58 * t865) * t67 * t112 / 0.7
     #20D3 + (0.90D2 * t35 * t14 * (t987 * t846 / 0.2D1 + t859 - t986 * 
     #t848) - 0.180D3 * t58 * t15 * (t848 - t986 * t846) + t938) * t111 
     #* t92 / 0.720D3 - (0.90D2 * t35 * t14 * (-t859 + t1006 * t848 - t1
     #008 * t846 / 0.2D1) - 0.180D3 * t58 * t15 * (t1006 * t846 - t848) 
     #- t938) * t67 * t111 / 0.1440D4 + (t13 * t15 * (-t1026 * t846 + t8
     #48) + 0.90D2 * t35 * t14 * (t852 - t1032 * t846 / 0.6D1 + t1031 * 
     #t848 / 0.2D1 - t1026 * t859) + t866 - 0.180D3 * t58 * t15 * (t859 
     #- t1026 * t848 + t1031 * t846 / 0.2D1)) * t111 / 0.1440D4
      t1053 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t1052)
      t1057 = t2 * t142 * x2 * t158
      t1058 = t208 * t143
      t1059 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1057,
     # t595, 0.0D0, -t630)
      t1064 = log(0.4D1 * t187 * t25 * t160 * t213)
      t1065 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1057,
     # t595, 0.0D0, -t630)
      t1071 = t15 * t1065
      t1083 = log(0.4D1 * t233 * t96 * t24 * t158 * t159 * t213)
      t1084 = t1083 ** 2
      t1088 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1057,
     # t595, 0.0D0, -t630)
      t1103 = (0.90D2 * t35 * t14 * (t1059 - t1064 * t1065) - 0.180D3 * 
     #t58 * t1071) * t67 * t112 / 0.720D3 + (0.90D2 * t35 * t14 * (t1084
     # * t1065 / 0.2D1 - t1083 * t1059 + t1088) - 0.180D3 * t58 * t15 * 
     #(t1059 - t1083 * t1065) + t13 * t1071) * t111 * t92 / 0.720D3
      t1104 = FJET(XB1, XB2, s, -t1057, 0.0D0, t1058, t595, -t630, t1103
     #)
      t1106 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0
     #.0D0, 0.0D0, 0.0D0)
      t1108 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0
     #.0D0, 0.0D0, 0.0D0)
      t1113 = t15 * t1106
      t1122 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0
     #.0D0, 0.0D0, 0.0D0)
      t1133 = t13 * t1113
      t1163 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0
     #.0D0, 0.0D0, 0.0D0)
      t1179 = (0.90D2 * t35 * t14 * (t218 * t1106 - t1108) + 0.180D3 * t
     #58 * t1113) * t67 * t112 / 0.720D3 + (0.90D2 * t35 * t14 * (-t239 
     #* t1106 / 0.2D1 - t1122 + t238 * t1108) - 0.180D3 * t58 * t15 * (t
     #238 * t1106 - t1108) - t1133) * t111 * t92 / 0.720D3 - (0.90D2 * t
     #35 * t14 * (t1122 + t261 * t1106 / 0.2D1 - t260 * t1108) - 0.180D3
     # * t58 * t15 * (-t260 * t1106 + t1108) + t1133) * t67 * t111 / 0.1
     #440D4 + (t13 * t15 * (t281 * t1106 - t1108) + 0.90D2 * t35 * t14 *
     # (t287 * t1106 / 0.6D1 - t286 * t1108 / 0.2D1 + t281 * t1122 - t11
     #63) - t55 * t1113 - 0.180D3 * t58 * t15 * (-t1122 - t286 * t1106 /
     # 0.2D1 + t281 * t1108)) * t111 / 0.1440D4
      t1180 = FJET(XB1, XB2, s, 0.0D0, -t209, 0.0D0, t211, 0.0D0, t1179)
      t1182 = t139 * t138 + t205 * t204 + t310 * t309 + t386 * t385 + t4
     #62 * t461 + t552 * t551 + t593 * t592 + t695 * t691 * t698 / 0.720
     #D3 + t719 * t715 * t698 / 0.720D3 + t741 * t737 * t698 / 0.720D3 +
     # t777 * t776 + t818 * t817 + t838 * t834 * t698 / 0.720D3 + t1053 
     #* t1052 + t1104 * t1103 + t1180 * t1179
      t1183 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t1188 = log(0.4D1 * t71 * t22 * t153 * t160)
      t1189 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t1191 = t1188 ** 2
      t1192 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t1204 = t15 * t1192
      t1205 = t13 * t1204
      t1214 = log(0.4D1 * t152 * t24 * t18 * t158 * t159)
      t1219 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t1220 = t1214 ** 2
      t1221 = t1220 * t1214
      t1241 = t25 * t160
      t1244 = log(0.4D1 * t187 * t1241)
      t1257 = log(0.4D1 * t234 * t1241)
      t1258 = t1257 ** 2
      t1275 = -(0.90D2 * t35 * t14 * (t1183 - t1188 * t1189 + t1191 * t1
     #192 / 0.2D1) - 0.180D3 * t58 * t15 * (t1189 - t1188 * t1192) + t12
     #05) * t67 * t92 / 0.720D3 + (t13 * t15 * (-t1189 + t1214 * t1192) 
     #+ 0.90D2 * t35 * t14 * (-t1219 + t1221 * t1192 / 0.6D1 + t1214 * t
     #1183 - t1220 * t1189 / 0.2D1) - t55 * t1204 - 0.180D3 * t58 * t15 
     #* (t1214 * t1189 - t1183 - t1220 * t1192 / 0.2D1)) * t92 / 0.720D3
     # + (0.90D2 * t35 * t14 * (t1244 * t1192 - t1189) + 0.180D3 * t58 *
     # t1204) * t67 * t112 / 0.720D3 + (0.90D2 * t35 * t14 * (-t1258 * t
     #1192 / 0.2D1 + t1257 * t1189 - t1183) - 0.180D3 * t58 * t15 * (-t1
     #189 + t1257 * t1192) - t1205) * t111 * t92 / 0.720D3
      t1276 = FJET(XB1, XB2, s, t595, -t599, 0.0D0, 0.0D0, 0.0D0, t1275)
      t1278 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t1280 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t1283 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t1293 = t15 * t1280
      t1294 = t13 * t1293
      t1302 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t1348 = -(0.90D2 * t35 * t14 * (-t1188 * t1278 + t1191 * t1280 / 0
     #.2D1 + t1283) - 0.180D3 * t58 * t15 * (-t1188 * t1280 + t1278) + t
     #1294) * t67 * t92 / 0.720D3 + (t13 * t15 * (-t1278 + t1214 * t1280
     #) + 0.90D2 * t35 * t14 * (-t1302 + t1221 * t1280 / 0.6D1 + t1214 *
     # t1283 - t1220 * t1278 / 0.2D1) - t55 * t1293 - 0.180D3 * t58 * t1
     #5 * (t1214 * t1278 - t1283 - t1220 * t1280 / 0.2D1)) * t92 / 0.720
     #D3 + (0.90D2 * t35 * t14 * (-t1278 + t1244 * t1280) + 0.180D3 * t5
     #8 * t1293) * t67 * t112 / 0.720D3 + (0.90D2 * t35 * t14 * (-t1283 
     #- t1258 * t1280 / 0.2D1 + t1257 * t1278) - 0.180D3 * t58 * t15 * (
     #-t1278 + t1257 * t1280) - t1294) * t111 * t92 / 0.720D3
      t1349 = FJET(XB1, XB2, s, -t599, t595, 0.0D0, 0.0D0, 0.0D0, t1348)
      t1351 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1353 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1357 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1360 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1368 = t15 * t1351
      t1369 = t55 * t1368
      t1380 = rrgq2qgh83J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1423 = t13 * t1368
      t1517 = -(t13 * t15 * (t845 * t1351 - t1353) + 0.90D2 * t35 * t14 
     #* (-t1357 + t854 * t1351 / 0.6D1 + t845 * t1360 - t853 * t1353 / 0
     #.2D1) - t1369 - 0.180D3 * t58 * t15 * (t845 * t1353 - t1360 - t853
     # * t1351 / 0.2D1)) * t67 / 0.1440D4 + (0.90D2 * t1380 + (-t879 * t
     #1353 + t1360 + t883 * t1351 / 0.2D1) * t10 - 0.15D2 * t896 * t1353
     # - 0.90D2 * t879 * t1357 + (-t879 * t1351 + t1353) * t54 - 0.180D3
     # * (t883 * t1353 / 0.2D1 - t896 * t1351 / 0.6D1 + t1357 - t879 * t
     #1360) * lh + 0.15D2 / 0.4D1 * t888 * t1351 + 0.45D2 * t883 * t1360
     # + t1351 * t914) * t14 * t35 / 0.1440D4 - (0.90D2 * t35 * t14 * (-
     #t926 * t1351 / 0.2D1 + t924 * t1353 - t1360) - 0.180D3 * t58 * t15
     # * (t924 * t1351 - t1353) - t1423) * t67 * t92 / 0.720D3 + (t13 * 
     #t15 * (-t944 * t1351 + t1353) + 0.90D2 * t35 * t14 * (t949 * t1353
     # / 0.2D1 + t1357 - t944 * t1360 - t950 * t1351 / 0.6D1) + t1369 - 
     #0.180D3 * t58 * t15 * (t1360 + t949 * t1351 / 0.2D1 - t944 * t1353
     #)) * t92 / 0.720D3 + (0.90D2 * t35 * t14 * (-t972 * t1351 + t1353)
     # - 0.180D3 * t58 * t1368) * t67 * t112 / 0.720D3 + (0.90D2 * t35 *
     # t14 * (t987 * t1351 / 0.2D1 + t1360 - t986 * t1353) - 0.180D3 * t
     #58 * t15 * (-t986 * t1351 + t1353) + t1423) * t111 * t92 / 0.720D3
     # - (0.90D2 * t35 * t14 * (t1006 * t1353 - t1360 - t1008 * t1351 / 
     #0.2D1) - 0.180D3 * t58 * t15 * (t1006 * t1351 - t1353) - t1423) * 
     #t67 * t111 / 0.1440D4 + (t13 * t15 * (-t1026 * t1351 + t1353) + 0.
     #90D2 * t35 * t14 * (t1357 - t1032 * t1351 / 0.6D1 - t1026 * t1360 
     #+ t1031 * t1353 / 0.2D1) + t1369 - 0.180D3 * t58 * t15 * (-t1026 *
     # t1353 + t1360 + t1031 * t1351 / 0.2D1)) * t111 / 0.1440D4
      t1518 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t1517)
      t1520 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t1522 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t1525 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t1535 = t15 * t1522
      t1536 = t13 * t1535
      t1547 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t1590 = -(0.90D2 * t35 * t14 * (-t1188 * t1520 + t1191 * t1522 / 0
     #.2D1 + t1525) - 0.180D3 * t58 * t15 * (t1520 - t1188 * t1522) + t1
     #536) * t67 * t92 / 0.720D3 + (t13 * t15 * (t1214 * t1522 - t1520) 
     #+ 0.90D2 * t35 * t14 * (t1221 * t1522 / 0.6D1 + t1214 * t1525 - t1
     #547 - t1220 * t1520 / 0.2D1) - t55 * t1535 - 0.180D3 * t58 * t15 *
     # (-t1525 + t1214 * t1520 - t1220 * t1522 / 0.2D1)) * t92 / 0.720D3
     # + (0.90D2 * t35 * t14 * (t1244 * t1522 - t1520) + 0.180D3 * t58 *
     # t1535) * t67 * t112 / 0.720D3 + (0.90D2 * t35 * t14 * (-t1525 + t
     #1257 * t1520 - t1258 * t1522 / 0.2D1) - 0.180D3 * t58 * t15 * (-t1
     #520 + t1257 * t1522) - t1536) * t111 * t92 / 0.720D3
      t1591 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t595, -t599, 0.0D0, t1590)
      t1593 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1057,
     # t595, 0.0D0, -t630)
      t1594 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1057,
     # t595, 0.0D0, -t630)
      t1600 = t15 * t1594
      t1606 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1057,
     # t595, 0.0D0, -t630)
      t1624 = (0.90D2 * t35 * t14 * (t1593 - t1064 * t1594) - 0.180D3 * 
     #t58 * t1600) * t67 * t112 / 0.720D3 + (0.90D2 * t35 * t14 * (t1606
     # + t1084 * t1594 / 0.2D1 - t1083 * t1593) - 0.180D3 * t58 * t15 * 
     #(-t1083 * t1594 + t1593) + t13 * t1600) * t111 * t92 / 0.720D3
      t1625 = FJET(XB1, XB2, s, t595, t1058, 0.0D0, -t1057, -t630, t1624
     #)
      t1627 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1628 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1633 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1639 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1644 = t15 * t1628
      t1645 = t55 * t1644
      t1657 = rrgq2qgh84J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1699 = t13 * t1644
      t1793 = -(t13 * t15 * (-t1627 + t845 * t1628) + 0.90D2 * t35 * t14
     # * (t845 * t1633 - t853 * t1627 / 0.2D1 + t854 * t1628 / 0.6D1 - t
     #1639) - t1645 - 0.180D3 * t58 * t15 * (-t1633 + t845 * t1627 - t85
     #3 * t1628 / 0.2D1)) * t67 / 0.1440D4 + (t1628 * t914 + 0.90D2 * t1
     #657 - 0.15D2 * t896 * t1627 - 0.180D3 * (t1639 + t883 * t1627 / 0.
     #2D1 - t896 * t1628 / 0.6D1 - t879 * t1633) * lh + 0.45D2 * t883 * 
     #t1633 + (t1633 - t879 * t1627 + t883 * t1628 / 0.2D1) * t10 - 0.90
     #D2 * t879 * t1639 + (t1627 - t879 * t1628) * t54 + 0.15D2 / 0.4D1 
     #* t888 * t1628) * t14 * t35 / 0.1440D4 - (0.90D2 * t35 * t14 * (-t
     #1633 - t926 * t1628 / 0.2D1 + t924 * t1627) - 0.180D3 * t58 * t15 
     #* (-t1627 + t924 * t1628) - t1699) * t67 * t92 / 0.720D3 + (t13 * 
     #t15 * (t1627 - t944 * t1628) + 0.90D2 * t35 * t14 * (-t944 * t1633
     # + t949 * t1627 / 0.2D1 - t950 * t1628 / 0.6D1 + t1639) + t1645 - 
     #0.180D3 * t58 * t15 * (t1633 - t944 * t1627 + t949 * t1628 / 0.2D1
     #)) * t92 / 0.720D3 + (0.90D2 * t35 * t14 * (-t972 * t1628 + t1627)
     # - 0.180D3 * t58 * t1644) * t67 * t112 / 0.720D3 + (0.90D2 * t35 *
     # t14 * (t1633 - t986 * t1627 + t987 * t1628 / 0.2D1) - 0.180D3 * t
     #58 * t15 * (-t986 * t1628 + t1627) + t1699) * t111 * t92 / 0.720D3
     # - (0.90D2 * t35 * t14 * (-t1633 + t1006 * t1627 - t1008 * t1628 /
     # 0.2D1) - 0.180D3 * t58 * t15 * (-t1627 + t1006 * t1628) - t1699) 
     #* t67 * t111 / 0.1440D4 + (t13 * t15 * (t1627 - t1026 * t1628) + 0
     #.90D2 * t35 * t14 * (-t1026 * t1633 + t1031 * t1627 / 0.2D1 - t103
     #2 * t1628 / 0.6D1 + t1639) + t1645 - 0.180D3 * t58 * t15 * (t1633 
     #- t1026 * t1627 + t1031 * t1628 / 0.2D1)) * t111 / 0.1440D4
      t1794 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t1793)
      t1796 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0
     #.0D0, 0.0D0, 0.0D0)
      t1797 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0
     #.0D0, 0.0D0, 0.0D0)
      t1799 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0
     #.0D0, 0.0D0, 0.0D0)
      t1811 = t15 * t1799
      t1812 = t13 * t1811
      t1826 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0
     #.0D0, 0.0D0, 0.0D0)
      t1869 = -(0.90D2 * t35 * t14 * (t1796 - t260 * t1797 + t261 * t179
     #9 / 0.2D1) - 0.180D3 * t58 * t15 * (t1797 - t260 * t1799) + t1812)
     # * t67 * t111 / 0.1440D4 + (t13 * t15 * (-t1797 + t281 * t1799) + 
     #0.90D2 * t35 * t14 * (t287 * t1799 / 0.6D1 + t281 * t1796 - t286 *
     # t1797 / 0.2D1 - t1826) - t55 * t1811 - 0.180D3 * t58 * t15 * (-t1
     #796 - t286 * t1799 / 0.2D1 + t281 * t1797)) * t111 / 0.1440D4 + (0
     #.90D2 * t35 * t14 * (-t1797 + t218 * t1799) + 0.180D3 * t58 * t181
     #1) * t67 * t112 / 0.720D3 + (0.90D2 * t35 * t14 * (-t1796 - t239 *
     # t1799 / 0.2D1 + t238 * t1797) - 0.180D3 * t58 * t15 * (-t1797 + t
     #238 * t1799) - t1812) * t111 * t92 / 0.720D3
      t1870 = FJET(XB1, XB2, s, 0.0D0, t211, 0.0D0, -t209, 0.0D0, t1869)
      t1872 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1057,
     # t595, 0.0D0, -t630)
      t1874 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1057,
     # t595, 0.0D0, -t630)
      t1879 = t15 * t1872
      t1887 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1057,
     # t595, 0.0D0, -t630)
      t1903 = (0.90D2 * t35 * t14 * (-t1064 * t1872 + t1874) - 0.180D3 *
     # t58 * t1879) * t67 * t112 / 0.720D3 + (0.90D2 * t35 * t14 * (t108
     #4 * t1872 / 0.2D1 + t1887 - t1083 * t1874) - 0.180D3 * t58 * t15 *
     # (t1874 - t1083 * t1872) + t13 * t1879) * t111 * t92 / 0.720D3
      t1904 = FJET(XB1, XB2, s, 0.0D0, -t1057, t595, t1058, -t630, t1903
     #)
      t1906 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t144, -t148, -
     #t146, t150, 0.0D0)
      t1908 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t144, -t148, -
     #t146, t150, 0.0D0)
      t1911 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t144, -t148, -
     #t146, t150, 0.0D0)
      t1921 = t15 * t1908
      t1937 = -(0.90D2 * t35 * t14 * (t164 * t1906 - t167 * t1908 / 0.2D
     #1 - t1911) - 0.180D3 * t58 * t15 * (-t1906 + t164 * t1908) - t13 *
     # t1921) * t67 * t92 / 0.720D3 + (0.90D2 * t35 * t14 * (t1906 - t19
     #2 * t1908) - 0.180D3 * t58 * t1921) * t67 * t112 / 0.720D3
      t1938 = FJET(XB1, XB2, s, -t146, t144, t150, -t148, 0.0D0, t1937)
      t1940 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t144, -t148, -
     #t146, t150, 0.0D0)
      t1941 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t144, -t148, -
     #t146, t150, 0.0D0)
      t1944 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t144, -t148, -
     #t146, t150, 0.0D0)
      t1955 = t15 * t1941
      t1971 = -(0.90D2 * t35 * t14 * (-t1940 - t167 * t1941 / 0.2D1 + t1
     #64 * t1944) - 0.180D3 * t58 * t15 * (-t1944 + t164 * t1941) - t13 
     #* t1955) * t67 * t92 / 0.720D3 + (0.90D2 * t35 * t14 * (t1944 - t1
     #92 * t1941) - 0.180D3 * t58 * t1955) * t67 * t112 / 0.720D3
      t1972 = FJET(XB1, XB2, s, t150, -t148, -t146, t144, 0.0D0, t1971)
      t1974 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1057,
     # t595, 0.0D0, -t630)
      t1975 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1057,
     # t595, 0.0D0, -t630)
      t1981 = t15 * t1975
      t1988 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t1058, -t1057,
     # t595, 0.0D0, -t630)
      t2005 = (0.90D2 * t35 * t14 * (t1974 - t1064 * t1975) - 0.180D3 * 
     #t58 * t1981) * t67 * t112 / 0.720D3 + (0.90D2 * t35 * t14 * (-t108
     #3 * t1974 + t1988 + t1084 * t1975 / 0.2D1) - 0.180D3 * t58 * t15 *
     # (t1974 - t1083 * t1975) + t13 * t1981) * t111 * t92 / 0.720D3
      t2006 = FJET(XB1, XB2, s, t1058, t595, -t1057, 0.0D0, -t630, t2005
     #)
      t2008 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0
     #, 0.0D0, 0.0D0)
      t2009 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0
     #, 0.0D0, 0.0D0)
      t2018 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0
     #, 0.0D0, 0.0D0)
      t2019 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t5, t3, 0.0D0
     #, 0.0D0, 0.0D0)
      t2025 = t15 * t2009
      t2049 = t13 * t2025
      t2081 = -(t13 * t15 * (t2008 - t29 * t2009) + 0.90D2 * t35 * t14 *
     # (-t37 * t2009 / 0.6D1 + t36 * t2008 / 0.2D1 + t2018 - t29 * t2019
     #) + t55 * t2025 - 0.180D3 * t58 * t15 * (-t29 * t2008 + t2019 + t3
     #6 * t2009 / 0.2D1)) * t67 / 0.1440D4 - (0.90D2 * t35 * t14 * (-t75
     # * t2008 + t2019 + t76 * t2009 / 0.2D1) - 0.180D3 * t58 * t15 * (t
     #2008 - t75 * t2009) + t2049) * t67 * t92 / 0.720D3 + (0.90D2 * t35
     # * t14 * (-t2008 + t101 * t2009) + 0.180D3 * t58 * t2025) * t67 * 
     #t112 / 0.720D3 - (0.90D2 * t35 * t14 * (t121 * t2009 / 0.2D1 - t12
     #0 * t2008 + t2019) - 0.180D3 * t58 * t15 * (t2008 - t120 * t2009) 
     #+ t2049) * t67 * t111 / 0.1440D4
      t2082 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t2081)
      t2084 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0
     #.0D0, 0.0D0, 0.0D0)
      t2086 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0
     #.0D0, 0.0D0, 0.0D0)
      t2091 = t15 * t2084
      t2101 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0
     #.0D0, 0.0D0, 0.0D0)
      t2111 = t13 * t2091
      t2139 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, -t209, t211, 0
     #.0D0, 0.0D0, 0.0D0)
      t2157 = (0.90D2 * t35 * t14 * (t218 * t2084 - t2086) + 0.180D3 * t
     #58 * t2091) * t67 * t112 / 0.720D3 + (0.90D2 * t35 * t14 * (-t239 
     #* t2084 / 0.2D1 + t238 * t2086 - t2101) - 0.180D3 * t58 * t15 * (-
     #t2086 + t238 * t2084) - t2111) * t111 * t92 / 0.720D3 - (0.90D2 * 
     #t35 * t14 * (-t260 * t2086 + t261 * t2084 / 0.2D1 + t2101) - 0.180
     #D3 * t58 * t15 * (-t260 * t2084 + t2086) + t2111) * t67 * t111 / 0
     #.1440D4 + (t13 * t15 * (-t2086 + t281 * t2084) + 0.90D2 * t35 * t1
     #4 * (t287 * t2084 / 0.6D1 + t281 * t2101 - t2139 - t286 * t2086 / 
     #0.2D1) - t55 * t2091 - 0.180D3 * t58 * t15 * (t281 * t2086 - t2101
     # - t286 * t2084 / 0.2D1)) * t111 / 0.1440D4
      t2158 = FJET(XB1, XB2, s, t211, 0.0D0, -t209, 0.0D0, 0.0D0, t2157)
      t2160 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t2162 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t2166 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t2167 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t2177 = t15 * t2160
      t2178 = t55 * t2177
      t2201 = t13 * t2177
      t2305 = rrgq2qgh81J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t2326 = -(t13 * t15 * (t845 * t2160 - t2162) + 0.90D2 * t35 * t14 
     #* (-t2166 + t845 * t2167 + t854 * t2160 / 0.6D1 - t853 * t2162 / 0
     #.2D1) - t2178 - 0.180D3 * t58 * t15 * (-t853 * t2160 / 0.2D1 + t84
     #5 * t2162 - t2167)) * t67 / 0.1440D4 - (0.90D2 * t35 * t14 * (-t92
     #6 * t2160 / 0.2D1 + t924 * t2162 - t2167) - 0.180D3 * t58 * t15 * 
     #(t924 * t2160 - t2162) - t2201) * t67 * t92 / 0.720D3 + (t13 * t15
     # * (-t944 * t2160 + t2162) + 0.90D2 * t35 * t14 * (t949 * t2162 / 
     #0.2D1 + t2166 - t950 * t2160 / 0.6D1 - t944 * t2167) + t2178 - 0.1
     #80D3 * t58 * t15 * (t949 * t2160 / 0.2D1 + t2167 - t944 * t2162)) 
     #* t92 / 0.720D3 + (0.90D2 * t35 * t14 * (-t972 * t2160 + t2162) - 
     #0.180D3 * t58 * t2177) * t67 * t112 / 0.720D3 + (0.90D2 * t35 * t1
     #4 * (t2167 - t986 * t2162 + t987 * t2160 / 0.2D1) - 0.180D3 * t58 
     #* t15 * (t2162 - t986 * t2160) + t2201) * t111 * t92 / 0.720D3 - (
     #0.90D2 * t35 * t14 * (-t1008 * t2160 / 0.2D1 + t1006 * t2162 - t21
     #67) - 0.180D3 * t58 * t15 * (t1006 * t2160 - t2162) - t2201) * t67
     # * t111 / 0.1440D4 + (t13 * t15 * (t2162 - t1026 * t2160) + 0.90D2
     # * t35 * t14 * (t2166 - t1026 * t2167 - t1032 * t2160 / 0.6D1 + t1
     #031 * t2162 / 0.2D1) + t2178 - 0.180D3 * t58 * t15 * (t1031 * t216
     #0 / 0.2D1 + t2167 - t1026 * t2162)) * t111 / 0.1440D4 + (-0.180D3 
     #* (t883 * t2162 / 0.2D1 - t896 * t2160 / 0.6D1 + t2166 - t879 * t2
     #167) * lh + 0.15D2 / 0.4D1 * t888 * t2160 + 0.90D2 * t2305 + 0.45D
     #2 * t883 * t2167 - 0.15D2 * t896 * t2162 + (t883 * t2160 / 0.2D1 +
     # t2167 - t879 * t2162) * t10 + (-t879 * t2160 + t2162) * t54 - 0.9
     #0D2 * t879 * t2166 + t2160 * t914) * t14 * t35 / 0.1440D4
      t2327 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t2326)
      t2329 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t2332 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t2334 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t2344 = t15 * t2329
      t2345 = t13 * t2344
      t2356 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, -t599, 0.0D0, 
     #t595, 0.0D0, 0.0D0)
      t2399 = -(0.90D2 * t35 * t14 * (t1191 * t2329 / 0.2D1 - t1188 * t2
     #332 + t2334) - 0.180D3 * t58 * t15 * (-t1188 * t2329 + t2332) + t2
     #345) * t67 * t92 / 0.720D3 + (t13 * t15 * (t1214 * t2329 - t2332) 
     #+ 0.90D2 * t35 * t14 * (t1214 * t2334 + t1221 * t2329 / 0.6D1 - t2
     #356 - t1220 * t2332 / 0.2D1) - t55 * t2344 - 0.180D3 * t58 * t15 *
     # (-t2334 - t1220 * t2329 / 0.2D1 + t1214 * t2332)) * t92 / 0.720D3
     # + (0.90D2 * t35 * t14 * (-t2332 + t1244 * t2329) + 0.180D3 * t58 
     #* t2344) * t67 * t112 / 0.720D3 + (0.90D2 * t35 * t14 * (t1257 * t
     #2332 - t2334 - t1258 * t2329 / 0.2D1) - 0.180D3 * t58 * t15 * (t12
     #57 * t2329 - t2332) - t2345) * t111 * t92 / 0.720D3
      t2400 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t599, t595, 0.0D0, t2399)
      t2402 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t474, t482, 0
     #.0D0, 0.0D0, 0.0D0)
      t2403 = t484 * t2402
      t2404 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t474, t482, 0
     #.0D0, 0.0D0, 0.0D0)
      t2412 = t511 * t2404 * t506
      t2419 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t474, t482, 0
     #.0D0, 0.0D0, 0.0D0)
      t2440 = (-0.90D2 * t35 * t14 * (t2403 - t495 * t2404) * t506 + 0.1
     #80D3 * t510 * t2412) * t67 * t112 / 0.720D3 - (0.90D2 * t35 * t14 
     #* (t484 * t2419 - t528 * t2402 + t531 * t2404 / 0.2D1) * t506 - 0.
     #180D3 * t510 * t14 * (t2403 - t528 * t2404) * t506 + t545 * t2412)
     # * t67 * t111 / 0.1440D4
      t2441 = FJET(XB1, XB2, s, t482, 0.0D0, -t474, 0.0D0, 0.0D0, t2440)
      t2443 = t1276 * t1275 + t1349 * t1348 + t1518 * t1517 + t1591 * t1
     #590 + t1625 * t1624 + t1794 * t1793 + t1870 * t1869 + t1904 * t190
     #3 + t1938 * t1937 + t1972 * t1971 + t2006 * t2005 + t2082 * t2081 
     #+ t2158 * t2157 + t2327 * t2326 + t2400 * t2399 + t2441 * t2440
      rrgq2qght8s2e1 = t1182 + t2443

      end function



      doubleprecision function rrgq2qght8s2e0
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t1 ** 2
      t15 = t14 ** 2
      t16 = t13 * t15
      t19 = log(0.4D1 * x3 * t10 * t16)
      t20 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t22 = t19 ** 2
      t23 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t30 = lh * t4
      t31 = pi * t6
      t37 = lh ** 2
      t39 = pi ** 2
      t41 = 0.180D3 * t37 - 0.30D2 * t39
      t42 = t41 * t4
      t43 = t31 * t23
      t44 = t42 * t43
      t46 = 0.1D1 / x3
      t54 = 0.60D2 * lh * t39 - 0.240D3 * zeta3 - 0.120D3 * t37 * lh
      t56 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t58 = t13 * t10
      t59 = t58 * t15
      t61 = log(0.4D1 * t59)
      t62 = t61 ** 2
      t65 = t62 * t61
      t83 = x2 * x3
      t86 = log(0.4D1 * t83 * t59)
      t93 = 0.180D3 * t30 * t43
      t96 = 0.1D1 / x2
      t99 = x2 * t15
      t102 = log(0.4D1 * t99 * t58)
      t104 = t102 ** 2
      t119 = x1 ** 2
      t120 = x3 * t119
      t123 = log(0.4D1 * t120 * t59)
      t131 = 0.1D1 / x1
      t134 = t5 * t6
      t136 = t96 * t131
      t140 = t119 * x2
      t143 = log(0.4D1 * t140 * t59)
      t153 = t119 * t10
      t154 = t153 * t16
      t156 = log(0.4D1 * t154)
      t158 = t156 ** 2
      t173 = -(0.90D2 * t5 * t6 * (-t7 + t19 * t20 - t22 * t23 / 0.2D1) 
     #- 0.180D3 * t30 * t31 * (-t20 + t19 * t23) - t44) * t46 / 0.1440D4
     # + (t23 * t54 + 0.90D2 * t56 + 0.45D2 * t62 * t20 - 0.15D2 * t65 *
     # t23 - 0.90D2 * t61 * t7 - 0.180D3 * (t7 - t61 * t20 + t62 * t23 /
     # 0.2D1) * lh + (t20 - t61 * t23) * t41) * t6 * t5 / 0.1440D4 - (0.
     #90D2 * t5 * t6 * (-t20 + t86 * t23) + t93) * t46 * t96 / 0.1440D4 
     #+ (0.90D2 * t5 * t6 * (t7 - t102 * t20 + t104 * t23 / 0.2D1) - 0.1
     #80D3 * t30 * t31 * (t20 - t102 * t23) + t44) * t96 / 0.1440D4 - (0
     #.90D2 * t5 * t6 * (-t20 + t123 * t23) + t93) * t46 * t131 / 0.720D
     #3 + t134 * t23 * t46 * t136 / 0.8D1 + (0.90D2 * t5 * t6 * (-t143 *
     # t23 + t20) - t93) * t96 * t131 / 0.720D3 + (0.90D2 * t5 * t6 * (t
     #7 - t156 * t20 + t158 * t23 / 0.2D1) - 0.180D3 * t30 * t31 * (t20 
     #- t156 * t23) + t44) * t131 / 0.720D3
      t174 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t173)
      t177 = x2 * s * t1
      t178 = -0.1D1 + x2
      t179 = t178 * s
      t180 = t179 * t1
      t182 = t178 ** 2
      t183 = t58 * t182
      t186 = log(0.4D1 * t83 * t15 * t183)
      t187 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t180, t177, 0.
     #0D0, 0.0D0, 0.0D0)
      t189 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t180, t177, 0.
     #0D0, 0.0D0, 0.0D0)
      t194 = t31 * t187
      t196 = 0.180D3 * t30 * t194
      t203 = log(0.4D1 * t99 * t183)
      t205 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t180, t177, 0.
     #0D0, 0.0D0, 0.0D0)
      t206 = t203 ** 2
      t226 = t140 * t15
      t229 = log(0.4D1 * t226 * t183)
      t239 = -(0.90D2 * t5 * t6 * (-t186 * t187 + t189) - t196) * t46 * 
     #t96 / 0.1440D4 + (0.90D2 * t5 * t6 * (t203 * t189 - t205 - t206 * 
     #t187 / 0.2D1) - 0.180D3 * t30 * t31 * (-t189 + t203 * t187) - t42 
     #* t194) * t96 / 0.1440D4 - t134 * t187 * t46 * t136 / 0.8D1 + (0.9
     #0D2 * t5 * t6 * (-t189 + t229 * t187) + t196) * t96 * t131 / 0.720
     #D3
      t240 = FJET(XB1, XB2, s, t177, 0.0D0, -t180, 0.0D0, 0.0D0, t239)
      t242 = -0.1D1 + x1
      t243 = t1 * t242
      t244 = t179 * t243
      t245 = t2 * x1
      t247 = x1 * z
      t248 = 0.1D1 - x1 + t247
      t249 = 0.1D1 / t248
      t251 = t2 * t242 * x2 * t249
      t256 = s * t14 * x2 * x1 * t242 * t249
      t257 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t244, -t251, t2
     #45, 0.0D0, -t256)
      t262 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t244, -t251, t2
     #45, 0.0D0, -t256)
      t263 = t15 * t10
      t266 = t242 ** 2
      t271 = log(0.4D1 * t140 * t263 * t13 * t249 * t266 * t182)
      t284 = t134 * t257 * t46 * t136 / 0.8D1 + (0.90D2 * t5 * t6 * (t26
     #2 - t271 * t257) - 0.180D3 * t30 * t31 * t257) * t96 * t131 / 0.72
     #0D3
      t285 = FJET(XB1, XB2, s, t244, t245, -t251, 0.0D0, -t256, t284)
      t287 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t244, -t251, t2
     #45, 0.0D0, -t256)
      t292 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t244, -t251, t2
     #45, 0.0D0, -t256)
      t305 = t134 * t287 * t46 * t136 / 0.8D1 + (0.90D2 * t5 * t6 * (t29
     #2 - t271 * t287) - 0.180D3 * t30 * t31 * t287) * t96 * t131 / 0.72
     #0D3
      t306 = FJET(XB1, XB2, s, -t251, 0.0D0, t244, t245, -t256, t305)
      t308 = t2 * t242
      t309 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t308, 0.0D0, t
     #245, 0.0D0, 0.0D0)
      t311 = t249 * t266
      t315 = log(0.4D1 * t120 * t10 * t16 * t311)
      t316 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t308, 0.0D0, t
     #245, 0.0D0, 0.0D0)
      t322 = t31 * t316
      t324 = 0.180D3 * t30 * t322
      t336 = log(0.4D1 * t226 * t58 * t311)
      t351 = log(0.4D1 * t153 * t13 * t15 * t249 * t266)
      t353 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t308, 0.0D0, t
     #245, 0.0D0, 0.0D0)
      t354 = t351 ** 2
      t370 = -(0.90D2 * t5 * t6 * (t309 - t315 * t316) - t324) * t46 * t
     #131 / 0.720D3 - t134 * t316 * t46 * t136 / 0.8D1 + (0.90D2 * t5 * 
     #t6 * (-t309 + t336 * t316) + t324) * t96 * t131 / 0.720D3 + (0.90D
     #2 * t5 * t6 * (t351 * t309 - t353 - t354 * t316 / 0.2D1) - 0.180D3
     # * t30 * t31 * (-t309 + t351 * t316) - t42 * t322) * t131 / 0.720D
     #3
      t371 = FJET(XB1, XB2, s, t245, -t308, 0.0D0, 0.0D0, 0.0D0, t370)
      t373 = cos(t8)
      t374 = -0.1D1 + x3
      t376 = Sqrt(-t83 * t374)
      t377 = t373 * t376
      t378 = 0.2D1 * t377
      t381 = -0.1D1 + t83
      t382 = 0.1D1 / t381
      t384 = t2 * t178 * (-t83 - 0.1D1 + x3 + t378) * t382
      t385 = 0.3D1 * t83
      t386 = x2 ** 2
      t387 = t386 * x3
      t389 = 0.2D1 * t377 * x2
      t392 = t2 * (-x2 - x3 + t385 - t387 - t378 + t389) * t382
      t393 = x2 * z
      t394 = 0.1D1 - x2 + t393
      t395 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t384, t392, 0.
     #0D0, 0.0D0, 0.0D0)
      t399 = t381 ** 2
      t405 = log(-0.4D1 * t83 * t263 * t13 * t182 * t374 / t399)
      t406 = t405 * t394
      t407 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t384, t392, 0.
     #0D0, 0.0D0, 0.0D0)
      t411 = t387 * z
      t412 = t83 * z
      t413 = 0.2D1 * t83
      t417 = 0.1D1 / (-t411 + t387 - t393 + t412 + x2 - t413 - t389 + 0.
     #2D1 * t377 * t393 + t378 - 0.1D1)
      t421 = t30 * pi
      t422 = t6 * t394
      t423 = t407 * t417
      t431 = t5 * t422
      t433 = t46 * t96 * t131
      t437 = -(0.90D2 * t5 * t6 * (t394 * t395 - t406 * t407) * t417 - 0
     #.180D3 * t421 * t422 * t423) * t46 * t96 / 0.1440D4 - t431 * t423 
     #* t433 / 0.8D1
      t438 = FJET(XB1, XB2, s, -t384, 0.0D0, t392, 0.0D0, 0.0D0, t437)
      t440 = t2 * t374
      t441 = t2 * x3
      t442 = x3 * t15
      t443 = t58 * t374
      t446 = log(-0.4D1 * t442 * t443)
      t447 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t440, t441, 0.
     #0D0, 0.0D0, 0.0D0)
      t449 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t440, t441, 0.
     #0D0, 0.0D0, 0.0D0)
      t450 = t446 ** 2
      t451 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t440, t441, 0.
     #0D0, 0.0D0, 0.0D0)
      t463 = t31 * t451
      t473 = log(-0.4D1 * x2 * t10 * t13 * t442 * t374)
      t480 = 0.180D3 * t30 * t463
      t488 = log(-0.4D1 * t120 * t15 * t443)
      t502 = -(0.90D2 * t5 * t6 * (-t446 * t447 + t449 + t450 * t451 / 0
     #.2D1) - 0.180D3 * t30 * t31 * (t447 - t446 * t451) + t42 * t463) *
     # t46 / 0.1440D4 - (0.90D2 * t5 * t6 * (t447 - t473 * t451) - t480)
     # * t46 * t96 / 0.1440D4 - (0.90D2 * t5 * t6 * (t447 - t488 * t451)
     # - t480) * t46 * t131 / 0.720D3 - t134 * t451 * t46 * t136 / 0.8D1
      t503 = FJET(XB1, XB2, s, -t440, 0.0D0, t441, 0.0D0, 0.0D0, t502)
      t505 = x3 * x1
      t506 = t505 * z
      t510 = Sqrt(-x3 * t248 * x2 * t374)
      t511 = t373 * t510
      t512 = 0.2D1 * t511
      t517 = t308 * t178 * (-t83 - 0.1D1 + x3 + x1 - t505 - t247 + t506 
     #+ t512) * t249 * t382
      t518 = t374 * s
      t519 = t1 * x1
      t521 = t518 * t519 * t382
      t522 = t83 * t247
      t524 = t387 * t247
      t526 = 0.2D1 * t511 * x2
      t527 = t83 * x1
      t529 = t387 * x1
      t530 = 0.2D1 * t522 - t524 - t512 - x2 - x3 + t526 - t506 - 0.2D1 
     #* t527 + t385 + t505 - t387 + t529
      t533 = t308 * t530 * t249 * t382
      t536 = t245 * x3 * t178 * t382
      t538 = x2 * x1
      t539 = t538 * z
      t540 = -0.1D1 - t247 + x1 + x2 + t539 - t538 - t393
      t542 = t5 * t6 * t248 * t540
      t543 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t517, -t533, t5
     #21, t536, -t256)
      t547 = t119 * t12
      t549 = t12 * x2
      t561 = 0.1D1 - t512 + 0.2D1 * t538 + t119 - 0.3D1 * t527 + t529 - 
     #0.3D1 * t539 - t547 * x2 + t549 * x1 + 0.2D1 * t140 * z + 0.2D1 * 
     #t511 * x1 + t120 * x2 + 0.2D1 * t511 * t539 + 0.4D1 * t522 - t524 
     #- 0.2D1 * t511 * t538
      t575 = -0.2D1 * t511 * t247 - 0.2D1 * t511 * t393 - x3 * t12 * t53
     #8 - 0.2D1 * t120 * t393 + t120 * t549 + t413 - t387 + t393 + t526 
     #- x2 + t411 - t412 + 0.2D1 * t247 - 0.2D1 * x1 - t140 - 0.2D1 * t1
     #19 * z + t547
      t577 = 0.1D1 / (t561 + t575)
      t582 = FJET(XB1, XB2, s, t517, t521, -t533, t536, -t256, t542 * t5
     #43 * t577 * t433 / 0.8D1)
      t584 = t31 * t248
      t591 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t440, t441, 0.
     #0D0, 0.0D0, 0.0D0)
      t593 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t440, t441, 0.
     #0D0, 0.0D0, 0.0D0)
      t598 = t31 * t591
      t600 = 0.180D3 * t30 * t598
      t609 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t440, t441, 0.
     #0D0, 0.0D0, 0.0D0)
      t635 = -(0.90D2 * t5 * t6 * (-t488 * t591 + t593) - t600) * t46 * 
     #t131 / 0.720D3 - t134 * t591 * t46 * t136 / 0.8D1 - (0.90D2 * t5 *
     # t6 * (t609 + t450 * t591 / 0.2D1 - t446 * t593) - 0.180D3 * t30 *
     # t31 * (-t446 * t591 + t593) + t42 * t598) * t46 / 0.1440D4 - (0.9
     #0D2 * t5 * t6 * (t593 - t473 * t591) - t600) * t46 * t96 / 0.1440D
     #4
      t636 = FJET(XB1, XB2, s, 0.0D0, -t440, 0.0D0, t441, 0.0D0, t635)
      t638 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t639 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t641 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t653 = t31 * t641
      t654 = t42 * t653
      t661 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t686 = 0.180D3 * t30 * t653
      t743 = -(0.90D2 * t5 * t6 * (-t638 + t19 * t639 - t22 * t641 / 0.2
     #D1) - 0.180D3 * t30 * t31 * (t19 * t641 - t639) - t654) * t46 / 0.
     #1440D4 + ((-t61 * t641 + t639) * t41 + 0.90D2 * t661 - 0.180D3 * (
     #t638 - t61 * t639 + t62 * t641 / 0.2D1) * lh + 0.45D2 * t62 * t639
     # - 0.15D2 * t65 * t641 + t641 * t54 - 0.90D2 * t61 * t638) * t6 * 
     #t5 / 0.1440D4 - (0.90D2 * t5 * t6 * (t86 * t641 - t639) + t686) * 
     #t46 * t96 / 0.1440D4 + (0.90D2 * t5 * t6 * (t638 - t102 * t639 + t
     #104 * t641 / 0.2D1) - 0.180D3 * t30 * t31 * (-t102 * t641 + t639) 
     #+ t654) * t96 / 0.1440D4 - (0.90D2 * t5 * t6 * (-t639 + t123 * t64
     #1) + t686) * t46 * t131 / 0.720D3 + t134 * t641 * t46 * t136 / 0.8
     #D1 + (0.90D2 * t5 * t6 * (t639 - t143 * t641) - t686) * t96 * t131
     # / 0.720D3 + (0.90D2 * t5 * t6 * (t638 - t156 * t639 + t158 * t641
     # / 0.2D1) - 0.180D3 * t30 * t31 * (-t156 * t641 + t639) + t654) * 
     #t131 / 0.720D3
      t744 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t743)
      t746 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t384, t392, 0.
     #0D0, 0.0D0, 0.0D0)
      t748 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t384, t392, 0.
     #0D0, 0.0D0, 0.0D0)
      t755 = t748 * t417
      t766 = -(0.90D2 * t5 * t6 * (t394 * t746 - t406 * t748) * t417 - 0
     #.180D3 * t421 * t422 * t755) * t46 * t96 / 0.1440D4 - t431 * t755 
     #* t433 / 0.8D1
      t767 = FJET(XB1, XB2, s, 0.0D0, -t384, 0.0D0, t392, 0.0D0, t766)
      t769 = t518 * t519
      t770 = t518 * t243
      t771 = t2 * t505
      t773 = x3 * s * t243
      t774 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t770, -t773, -t
     #769, t771, 0.0D0)
      t779 = log(-0.4D1 * t154 * x3 * t374 * t311)
      t780 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t770, -t773, -t
     #769, t771, 0.0D0)
      t797 = -(0.90D2 * t5 * t6 * (-t774 + t779 * t780) + 0.180D3 * t30 
     #* t31 * t780) * t46 * t131 / 0.720D3 + t134 * t780 * t46 * t136 / 
     #0.8D1
      t798 = FJET(XB1, XB2, s, -t769, t770, t771, -t773, 0.0D0, t797)
      t800 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t180, t177, 0.
     #0D0, 0.0D0, 0.0D0)
      t805 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t180, t177, 0.
     #0D0, 0.0D0, 0.0D0)
      t811 = t31 * t800
      t813 = 0.180D3 * t30 * t811
      t827 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t180, t177, 0.
     #0D0, 0.0D0, 0.0D0)
      t844 = -t134 * t800 * t46 * t136 / 0.8D1 + (0.90D2 * t5 * t6 * (-t
     #805 + t229 * t800) + t813) * t96 * t131 / 0.720D3 - (0.90D2 * t5 *
     # t6 * (t805 - t186 * t800) - t813) * t46 * t96 / 0.1440D4 + (0.90D
     #2 * t5 * t6 * (-t827 - t206 * t800 / 0.2D1 + t203 * t805) - 0.180D
     #3 * t30 * t31 * (-t805 + t203 * t800) - t42 * t811) * t96 / 0.1440
     #D4
      t845 = FJET(XB1, XB2, s, 0.0D0, t177, 0.0D0, -t180, 0.0D0, t844)
      t847 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t384, t392, 0.
     #0D0, 0.0D0, 0.0D0)
      t849 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t384, t392, 0.
     #0D0, 0.0D0, 0.0D0)
      t856 = t849 * t417
      t867 = -(0.90D2 * t5 * t6 * (t394 * t847 - t406 * t849) * t417 - 0
     #.180D3 * t421 * t422 * t856) * t46 * t96 / 0.1440D4 - t431 * t856 
     #* t433 / 0.8D1
      t868 = FJET(XB1, XB2, s, t392, 0.0D0, -t384, 0.0D0, 0.0D0, t867)
      t870 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t244, -t251, t2
     #45, 0.0D0, -t256)
      t876 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t244, -t251, t2
     #45, 0.0D0, -t256)
      t888 = t134 * t870 * t46 * t136 / 0.8D1 + (0.90D2 * t5 * t6 * (-t2
     #71 * t870 + t876) - 0.180D3 * t30 * t31 * t870) * t96 * t131 / 0.7
     #20D3
      t889 = FJET(XB1, XB2, s, t245, t244, 0.0D0, -t251, -t256, t888)
      t891 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t770, -t773, -t
     #769, t771, 0.0D0)
      t892 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t770, -t773, -t
     #769, t771, 0.0D0)
      t909 = -(0.90D2 * t5 * t6 * (-t891 + t779 * t892) + 0.180D3 * t30 
     #* t31 * t892) * t46 * t131 / 0.720D3 + t134 * t892 * t46 * t136 / 
     #0.8D1
      t910 = FJET(XB1, XB2, s, -t773, t771, t770, -t769, 0.0D0, t909)
      t912 = t174 * t173 + t240 * t239 + t285 * t284 + t306 * t305 + t37
     #1 * t370 + t438 * t437 + t503 * t502 + t582 * t4 * t584 * t540 * t
     #543 * t577 * t433 / 0.8D1 + t636 * t635 + t744 * t743 + t767 * t76
     #6 + t798 * t797 + t845 * t844 + t868 * t867 + t889 * t888 + t910 *
     # t909
      t913 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t180, t177, 0.
     #0D0, 0.0D0, 0.0D0)
      t914 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t180, t177, 0.
     #0D0, 0.0D0, 0.0D0)
      t920 = t31 * t914
      t922 = 0.180D3 * t30 * t920
      t929 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t180, t177, 0.
     #0D0, 0.0D0, 0.0D0)
      t957 = -(0.90D2 * t5 * t6 * (t913 - t186 * t914) - t922) * t46 * t
     #96 / 0.1440D4 + (0.90D2 * t5 * t6 * (-t206 * t914 / 0.2D1 - t929 +
     # t203 * t913) - 0.180D3 * t30 * t31 * (-t913 + t203 * t914) - t42 
     #* t920) * t96 / 0.1440D4 - t134 * t914 * t46 * t136 / 0.8D1 + (0.9
     #0D2 * t5 * t6 * (-t913 + t229 * t914) + t922) * t96 * t131 / 0.720
     #D3
      t958 = FJET(XB1, XB2, s, -t180, 0.0D0, t177, 0.0D0, 0.0D0, t957)
      t960 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t963 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t965 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t975 = t31 * t960
      t976 = t42 * t975
      t986 = 0.180D3 * t30 * t975
      t1051 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1065 = -(0.90D2 * t5 * t6 * (-t22 * t960 / 0.2D1 + t19 * t963 - t
     #965) - 0.180D3 * t30 * t31 * (t19 * t960 - t963) - t976) * t46 / 0
     #.1440D4 - (0.90D2 * t5 * t6 * (t86 * t960 - t963) + t986) * t46 * 
     #t96 / 0.1440D4 + (0.90D2 * t5 * t6 * (t104 * t960 / 0.2D1 + t965 -
     # t102 * t963) - 0.180D3 * t30 * t31 * (t963 - t102 * t960) + t976)
     # * t96 / 0.1440D4 - (0.90D2 * t5 * t6 * (t123 * t960 - t963) + t98
     #6) * t46 * t131 / 0.720D3 + t134 * t960 * t46 * t136 / 0.8D1 + (0.
     #90D2 * t5 * t6 * (t963 - t143 * t960) - t986) * t96 * t131 / 0.720
     #D3 + (0.90D2 * t5 * t6 * (t158 * t960 / 0.2D1 + t965 - t156 * t963
     #) - 0.180D3 * t30 * t31 * (-t156 * t960 + t963) + t976) * t131 / 0
     #.720D3 + (-0.180D3 * (t62 * t960 / 0.2D1 + t965 - t61 * t963) * lh
     # - 0.15D2 * t65 * t960 + 0.90D2 * t1051 + (-t61 * t960 + t963) * t
     #41 - 0.90D2 * t61 * t965 + t960 * t54 + 0.45D2 * t62 * t963) * t6 
     #* t5 / 0.1440D4
      t1066 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1065)
      t1068 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t440, t441, 0
     #.0D0, 0.0D0, 0.0D0)
      t1070 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t440, t441, 0
     #.0D0, 0.0D0, 0.0D0)
      t1075 = t31 * t1068
      t1077 = 0.180D3 * t30 * t1075
      t1084 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t440, t441, 0
     #.0D0, 0.0D0, 0.0D0)
      t1112 = -(0.90D2 * t5 * t6 * (-t473 * t1068 + t1070) - t1077) * t4
     #6 * t96 / 0.1440D4 - (0.90D2 * t5 * t6 * (t450 * t1068 / 0.2D1 + t
     #1084 - t446 * t1070) - 0.180D3 * t30 * t31 * (-t446 * t1068 + t107
     #0) + t42 * t1075) * t46 / 0.1440D4 - (0.90D2 * t5 * t6 * (t1070 - 
     #t488 * t1068) - t1077) * t46 * t131 / 0.720D3 - t134 * t1068 * t46
     # * t136 / 0.8D1
      t1113 = FJET(XB1, XB2, s, 0.0D0, t441, 0.0D0, -t440, 0.0D0, t1112)
      t1115 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t440, t441, 0
     #.0D0, 0.0D0, 0.0D0)
      t1117 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t440, t441, 0
     #.0D0, 0.0D0, 0.0D0)
      t1118 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t440, t441, 0
     #.0D0, 0.0D0, 0.0D0)
      t1130 = t31 * t1118
      t1141 = 0.180D3 * t30 * t1130
      t1159 = -(0.90D2 * t5 * t6 * (-t446 * t1115 + t1117 + t450 * t1118
     # / 0.2D1) - 0.180D3 * t30 * t31 * (t1115 - t446 * t1118) + t42 * t
     #1130) * t46 / 0.1440D4 - (0.90D2 * t5 * t6 * (t1115 - t473 * t1118
     #) - t1141) * t46 * t96 / 0.1440D4 - (0.90D2 * t5 * t6 * (-t488 * t
     #1118 + t1115) - t1141) * t46 * t131 / 0.720D3 - t134 * t1118 * t46
     # * t136 / 0.8D1
      t1160 = FJET(XB1, XB2, s, t441, 0.0D0, -t440, 0.0D0, 0.0D0, t1159)
      t1162 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t308, 0.0D0, 
     #t245, 0.0D0, 0.0D0)
      t1164 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t308, 0.0D0, 
     #t245, 0.0D0, 0.0D0)
      t1169 = t31 * t1162
      t1171 = 0.180D3 * t30 * t1169
      t1190 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t308, 0.0D0, 
     #t245, 0.0D0, 0.0D0)
      t1206 = -(0.90D2 * t5 * t6 * (-t315 * t1162 + t1164) - t1171) * t4
     #6 * t131 / 0.720D3 - t134 * t1162 * t46 * t136 / 0.8D1 + (0.90D2 *
     # t5 * t6 * (-t1164 + t336 * t1162) + t1171) * t96 * t131 / 0.720D3
     # + (0.90D2 * t5 * t6 * (t351 * t1164 - t1190 - t354 * t1162 / 0.2D
     #1) - 0.180D3 * t30 * t31 * (-t1164 + t351 * t1162) - t42 * t1169) 
     #* t131 / 0.720D3
      t1207 = FJET(XB1, XB2, s, -t308, t245, 0.0D0, 0.0D0, 0.0D0, t1206)
      t1209 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t308, 0.0D0, 
     #t245, 0.0D0, 0.0D0)
      t1211 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t308, 0.0D0, 
     #t245, 0.0D0, 0.0D0)
      t1216 = t31 * t1209
      t1218 = 0.180D3 * t30 * t1216
      t1236 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t308, 0.0D0, 
     #t245, 0.0D0, 0.0D0)
      t1253 = -(0.90D2 * t5 * t6 * (-t315 * t1209 + t1211) - t1218) * t4
     #6 * t131 / 0.720D3 - t134 * t1209 * t46 * t136 / 0.8D1 + (0.90D2 *
     # t5 * t6 * (t336 * t1209 - t1211) + t1218) * t96 * t131 / 0.720D3 
     #+ (0.90D2 * t5 * t6 * (-t1236 - t354 * t1209 / 0.2D1 + t351 * t121
     #1) - 0.180D3 * t30 * t31 * (t351 * t1209 - t1211) - t42 * t1216) *
     # t131 / 0.720D3
      t1254 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t308, t245, 0.0D0, t1253)
      t1256 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t770, -t773, -
     #t769, t771, 0.0D0)
      t1257 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t770, -t773, -
     #t769, t771, 0.0D0)
      t1274 = -(0.90D2 * t5 * t6 * (-t1256 + t779 * t1257) + 0.180D3 * t
     #30 * t31 * t1257) * t46 * t131 / 0.720D3 + t134 * t1257 * t46 * t1
     #36 / 0.8D1
      t1275 = FJET(XB1, XB2, s, t770, -t769, -t773, t771, 0.0D0, t1274)
      t1277 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t308, 0.0D0, 
     #t245, 0.0D0, 0.0D0)
      t1278 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t308, 0.0D0, 
     #t245, 0.0D0, 0.0D0)
      t1284 = t31 * t1278
      t1286 = 0.180D3 * t30 * t1284
      t1304 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t308, 0.0D0, 
     #t245, 0.0D0, 0.0D0)
      t1321 = -(0.90D2 * t5 * t6 * (t1277 - t315 * t1278) - t1286) * t46
     # * t131 / 0.720D3 - t134 * t1278 * t46 * t136 / 0.8D1 + (0.90D2 * 
     #t5 * t6 * (-t1277 + t336 * t1278) + t1286) * t96 * t131 / 0.720D3 
     #+ (0.90D2 * t5 * t6 * (-t1304 + t351 * t1277 - t354 * t1278 / 0.2D
     #1) - 0.180D3 * t30 * t31 * (t351 * t1278 - t1277) - t42 * t1284) *
     # t131 / 0.720D3
      t1322 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t245, -t308, 0.0D0, t1321)
      t1324 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t770, -t773, -
     #t769, t771, 0.0D0)
      t1325 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t770, -t773, -
     #t769, t771, 0.0D0)
      t1342 = -(0.90D2 * t5 * t6 * (-t1324 + t779 * t1325) + 0.180D3 * t
     #30 * t31 * t1325) * t46 * t131 / 0.720D3 + t134 * t1325 * t46 * t1
     #36 / 0.8D1
      t1343 = FJET(XB1, XB2, s, t771, -t773, -t769, t770, 0.0D0, t1342)
      t1345 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t384, t392, 0
     #.0D0, 0.0D0, 0.0D0)
      t1346 = t1345 * t417
      t1350 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t384, t392, 0
     #.0D0, 0.0D0, 0.0D0)
      t1365 = -t431 * t1346 * t433 / 0.8D1 - (0.90D2 * t5 * t6 * (t394 *
     # t1350 - t406 * t1345) * t417 - 0.180D3 * t421 * t422 * t1346) * t
     #46 * t96 / 0.1440D4
      t1366 = FJET(XB1, XB2, s, 0.0D0, t392, 0.0D0, -t384, 0.0D0, t1365)
      t1368 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t517, -t533, t
     #521, t536, -t256)
      t1373 = FJET(XB1, XB2, s, t536, -t533, t521, t517, -t256, t542 * t
     #1368 * t577 * t433 / 0.8D1)
      t1381 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t517, -t533, t
     #521, t536, -t256)
      t1386 = FJET(XB1, XB2, s, t521, t517, t536, -t533, -t256, t542 * t
     #1381 * t577 * t433 / 0.8D1)
      t1394 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1396 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1397 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1409 = t31 * t1397
      t1410 = t42 * t1409
      t1430 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0
     #D0, 0.0D0, 0.0D0)
      t1442 = 0.180D3 * t30 * t1409
      t1499 = -(0.90D2 * t5 * t6 * (t19 * t1394 - t1396 - t22 * t1397 / 
     #0.2D1) - 0.180D3 * t30 * t31 * (t19 * t1397 - t1394) - t1410) * t4
     #6 / 0.1440D4 + ((-t61 * t1397 + t1394) * t41 - 0.15D2 * t65 * t139
     #7 + t1397 * t54 - 0.180D3 * (-t61 * t1394 + t1396 + t62 * t1397 / 
     #0.2D1) * lh - 0.90D2 * t61 * t1396 + 0.45D2 * t62 * t1394 + 0.90D2
     # * t1430) * t6 * t5 / 0.1440D4 - (0.90D2 * t5 * t6 * (t86 * t1397 
     #- t1394) + t1442) * t46 * t96 / 0.1440D4 + (0.90D2 * t5 * t6 * (-t
     #102 * t1394 + t1396 + t104 * t1397 / 0.2D1) - 0.180D3 * t30 * t31 
     #* (-t102 * t1397 + t1394) + t1410) * t96 / 0.1440D4 - (0.90D2 * t5
     # * t6 * (t123 * t1397 - t1394) + t1442) * t46 * t131 / 0.720D3 + t
     #134 * t1397 * t46 * t136 / 0.8D1 + (0.90D2 * t5 * t6 * (-t143 * t1
     #397 + t1394) - t1442) * t96 * t131 / 0.720D3 + (0.90D2 * t5 * t6 *
     # (t1396 + t158 * t1397 / 0.2D1 - t156 * t1394) - 0.180D3 * t30 * t
     #31 * (-t156 * t1397 + t1394) + t1410) * t131 / 0.720D3
      t1500 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t1499)
      t1502 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t244, -t251, t
     #245, 0.0D0, -t256)
      t1507 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t244, -t251, t
     #245, 0.0D0, -t256)
      t1520 = t134 * t1502 * t46 * t136 / 0.8D1 + (0.90D2 * t5 * t6 * (t
     #1507 - t271 * t1502) - 0.180D3 * t30 * t31 * t1502) * t96 * t131 /
     # 0.720D3
      t1521 = FJET(XB1, XB2, s, 0.0D0, -t251, t245, t244, -t256, t1520)
      t1523 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t517, -t533, t
     #521, t536, -t256)
      t1528 = FJET(XB1, XB2, s, -t533, t536, t517, t521, -t256, t542 * t
     #1523 * t577 * t433 / 0.8D1)
      t1536 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t180, t177, 0
     #.0D0, 0.0D0, 0.0D0)
      t1538 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t180, t177, 0
     #.0D0, 0.0D0, 0.0D0)
      t1543 = t31 * t1536
      t1545 = 0.180D3 * t30 * t1543
      t1550 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t180, t177, 0
     #.0D0, 0.0D0, 0.0D0)
      t1580 = -(0.90D2 * t5 * t6 * (-t186 * t1536 + t1538) - t1545) * t4
     #6 * t96 / 0.1440D4 + (0.90D2 * t5 * t6 * (-t1550 - t206 * t1536 / 
     #0.2D1 + t203 * t1538) - 0.180D3 * t30 * t31 * (t203 * t1536 - t153
     #8) - t42 * t1543) * t96 / 0.1440D4 - t134 * t1536 * t46 * t136 / 0
     #.8D1 + (0.90D2 * t5 * t6 * (t229 * t1536 - t1538) + t1545) * t96 *
     # t131 / 0.720D3
      t1581 = FJET(XB1, XB2, s, 0.0D0, -t180, 0.0D0, t177, 0.0D0, t1580)
      t1583 = t958 * t957 + t1066 * t1065 + t1113 * t1112 + t1160 * t115
     #9 + t1207 * t1206 + t1254 * t1253 + t1275 * t1274 + t1322 * t1321 
     #+ t1343 * t1342 + t1366 * t1365 + t1373 * t4 * t584 * t540 * t1368
     # * t577 * t433 / 0.8D1 + t1386 * t4 * t584 * t540 * t1381 * t577 *
     # t433 / 0.8D1 + t1500 * t1499 + t1521 * t1520 + t1528 * t4 * t584 
     #* t540 * t1523 * t577 * t433 / 0.8D1 + t1581 * t1580
      rrgq2qght8s2e0 = t912 + t1583

      end function



      doubleprecision function rrgq2qght8s2em1
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = -0.1D1 + x3
      t4 = t2 * t3
      t5 = t2 * x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = t7 * pi
      t9 = 0.1D1 / t1
      t10 = t8 * t9
      t11 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t12 = 0.1D1 / x3
      t13 = t11 * t12
      t14 = 0.1D1 / x1
      t18 = t1 ** 2
      t19 = t18 ** 2
      t21 = x4 * pi
      t22 = Sin(t21)
      t23 = t22 ** 2
      t24 = z ** 2
      t25 = 0.1D1 / t24
      t26 = t23 * t25
      t30 = log(-0.4D1 * x3 * t19 * t26 * t3)
      t32 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t37 = lh * t7
      t38 = pi * t9
      t45 = 0.1D1 / x2
      t49 = -t10 * t13 * t14 / 0.8D1 - (0.90D2 * t8 * t9 * (-t30 * t11 +
     # t32) - 0.180D3 * t37 * t38 * t11) * t12 / 0.1440D4 - t10 * t13 * 
     #t45 / 0.16D2
      t50 = FJET(XB1, XB2, s, 0.0D0, -t4, 0.0D0, t5, 0.0D0, t49)
      t52 = -0.1D1 + x1
      t53 = t2 * t52
      t54 = t2 * x1
      t55 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t53, 0.0D0, t54
     #, 0.0D0, 0.0D0)
      t60 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t53, 0.0D0, t54
     #, 0.0D0, 0.0D0)
      t61 = x1 ** 2
      t62 = t61 * t23
      t66 = 0.1D1 / (0.1D1 - x1 + x1 * z)
      t68 = t52 ** 2
      t72 = log(0.4D1 * t62 * t25 * t19 * t66 * t68)
      t88 = -t10 * t55 * t45 * t14 / 0.8D1 + (0.90D2 * t8 * t9 * (-t60 +
     # t72 * t55) + 0.180D3 * t37 * t38 * t55) * t14 / 0.720D3 - t10 * t
     #55 * t12 * t14 / 0.8D1
      t89 = FJET(XB1, XB2, s, -t53, t54, 0.0D0, 0.0D0, 0.0D0, t88)
      t91 = -0.1D1 + x2
      t92 = t91 * s
      t93 = t92 * t1
      t95 = x2 * s * t1
      t96 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t93, t95, 0.0D0
     #, 0.0D0, 0.0D0)
      t101 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t93, t95, 0.0D
     #0, 0.0D0, 0.0D0)
      t102 = x2 * t19
      t103 = t91 ** 2
      t107 = log(0.4D1 * t102 * t26 * t103)
      t123 = -t10 * t96 * t12 * t45 / 0.16D2 + (0.90D2 * t8 * t9 * (-t10
     #1 + t107 * t96) + 0.180D3 * t37 * t38 * t96) * t45 / 0.1440D4 - t1
     #0 * t96 * t45 * t14 / 0.8D1
      t124 = FJET(XB1, XB2, s, -t93, 0.0D0, t95, 0.0D0, 0.0D0, t123)
      t126 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t53, 0.0D0, t5
     #4, 0.0D0, 0.0D0)
      t132 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t53, 0.0D0, t5
     #4, 0.0D0, 0.0D0)
      t147 = -t10 * t126 * t45 * t14 / 0.8D1 + (0.90D2 * t8 * t9 * (t72 
     #* t126 - t132) + 0.180D3 * t37 * t38 * t126) * t14 / 0.720D3 - t10
     # * t126 * t12 * t14 / 0.8D1
      t148 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t53, t54, 0.0D0, t147)
      t150 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t93, t95, 0.0D
     #0, 0.0D0, 0.0D0)
      t155 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t93, t95, 0.0D
     #0, 0.0D0, 0.0D0)
      t171 = -t10 * t150 * t12 * t45 / 0.16D2 + (0.90D2 * t8 * t9 * (-t1
     #55 + t107 * t150) + 0.180D3 * t37 * t38 * t150) * t45 / 0.1440D4 -
     # t10 * t150 * t45 * t14 / 0.8D1
      t172 = FJET(XB1, XB2, s, t95, 0.0D0, -t93, 0.0D0, 0.0D0, t171)
      t174 = x2 * x3
      t175 = cos(t21)
      t177 = Sqrt(-t174 * t3)
      t178 = t175 * t177
      t179 = 0.2D1 * t178
      t183 = 0.1D1 / (-0.1D1 + t174)
      t185 = t2 * t91 * (-t174 - 0.1D1 + x3 + t179) * t183
      t187 = x2 ** 2
      t188 = t187 * x3
      t190 = 0.2D1 * t178 * x2
      t193 = t2 * (-x2 - x3 + 0.3D1 * t174 - t188 - t179 + t190) * t183
      t194 = x2 * z
      t195 = 0.1D1 - x2 + t194
      t197 = t8 * t9 * t195
      t198 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t185, t193, 0.
     #0D0, 0.0D0, 0.0D0)
      t205 = 0.1D1 / (-t188 * z + t188 - t194 + t174 * z + x2 - 0.2D1 * 
     #t174 - t190 + 0.2D1 * t178 * t194 + t179 - 0.1D1)
      t207 = t12 * t45
      t211 = FJET(XB1, XB2, s, -t185, 0.0D0, t193, 0.0D0, 0.0D0, -t197 *
     # t198 * t205 * t207 / 0.16D2)
      t216 = t205 * t12 * t45
      t220 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t185, t193, 0.
     #0D0, 0.0D0, 0.0D0)
      t225 = FJET(XB1, XB2, s, t193, 0.0D0, -t185, 0.0D0, 0.0D0, -t197 *
     # t220 * t205 * t207 / 0.16D2)
      t232 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t53, 0.0D0, t5
     #4, 0.0D0, 0.0D0)
      t237 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t53, 0.0D0, t5
     #4, 0.0D0, 0.0D0)
      t253 = -t10 * t232 * t45 * t14 / 0.8D1 + (0.90D2 * t8 * t9 * (-t23
     #7 + t72 * t232) + 0.180D3 * t37 * t38 * t232) * t14 / 0.720D3 - t1
     #0 * t232 * t12 * t14 / 0.8D1
      t254 = FJET(XB1, XB2, s, t54, -t53, 0.0D0, 0.0D0, 0.0D0, t253)
      t256 = t1 * t52
      t257 = t92 * t256
      t260 = t2 * t52 * x2 * t66
      t265 = s * t18 * x2 * x1 * t52 * t66
      t266 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t257, -t260, t5
     #4, 0.0D0, -t265)
      t271 = FJET(XB1, XB2, s, t54, t257, 0.0D0, -t260, -t265, t10 * t26
     #6 * t45 * t14 / 0.8D1)
      t275 = t45 * t14
      t279 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t257, -t260, t5
     #4, 0.0D0, -t265)
      t284 = FJET(XB1, XB2, s, -t260, 0.0D0, t257, t54, -t265, t10 * t27
     #9 * t45 * t14 / 0.8D1)
      t291 = t3 * s
      t292 = t291 * t256
      t294 = t291 * t1 * x1
      t296 = x3 * s * t256
      t298 = t2 * x1 * x3
      t299 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t292, -t296, -t
     #294, t298, 0.0D0)
      t304 = FJET(XB1, XB2, s, t292, -t294, -t296, t298, 0.0D0, t10 * t2
     #99 * t12 * t14 / 0.8D1)
      t308 = t12 * t14
      t312 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t93, t95, 0.0D
     #0, 0.0D0, 0.0D0)
      t318 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t93, t95, 0.0D
     #0, 0.0D0, 0.0D0)
      t333 = -t10 * t312 * t12 * t45 / 0.16D2 + (0.90D2 * t8 * t9 * (t10
     #7 * t312 - t318) + 0.180D3 * t37 * t38 * t312) * t45 / 0.1440D4 - 
     #t10 * t312 * t45 * t14 / 0.8D1
      t334 = FJET(XB1, XB2, s, 0.0D0, -t93, 0.0D0, t95, 0.0D0, t333)
      t336 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t53, 0.0D0, t5
     #4, 0.0D0, 0.0D0)
      t342 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t53, 0.0D0, t5
     #4, 0.0D0, 0.0D0)
      t357 = -t10 * t336 * t45 * t14 / 0.8D1 + (0.90D2 * t8 * t9 * (t72 
     #* t336 - t342) + 0.180D3 * t37 * t38 * t336) * t14 / 0.720D3 - t10
     # * t336 * t12 * t14 / 0.8D1
      t358 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t54, -t53, 0.0D0, t357)
      t360 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t93, t95, 0.0D
     #0, 0.0D0, 0.0D0)
      t369 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t93, t95, 0.0D
     #0, 0.0D0, 0.0D0)
      t381 = -t10 * t360 * t45 * t14 / 0.8D1 - t10 * t360 * t12 * t45 / 
     #0.16D2 + (0.90D2 * t8 * t9 * (-t369 + t107 * t360) + 0.180D3 * t37
     # * t38 * t360) * t45 / 0.1440D4
      t382 = FJET(XB1, XB2, s, 0.0D0, t95, 0.0D0, -t93, 0.0D0, t381)
      t384 = t50 * t49 + t89 * t88 + t124 * t123 + t148 * t147 + t172 * 
     #t171 - t211 * t7 * t38 * t195 * t198 * t216 / 0.16D2 - t225 * t7 *
     # t38 * t195 * t220 * t216 / 0.16D2 + t254 * t253 + t271 * t7 * pi 
     #* t9 * t266 * t275 / 0.8D1 + t284 * t7 * pi * t9 * t279 * t275 / 0
     #.8D1 + t304 * t7 * pi * t9 * t299 * t308 / 0.8D1 + t334 * t333 + t
     #358 * t357 + t382 * t381
      t385 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, t5, 0.0D0,
     # 0.0D0, 0.0D0)
      t386 = t385 * t12
      t391 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, t5, 0.0D0,
     # 0.0D0, 0.0D0)
      t405 = -t10 * t386 * t45 / 0.16D2 - (0.90D2 * t8 * t9 * (-t30 * t3
     #85 + t391) - 0.180D3 * t37 * t38 * t385) * t12 / 0.1440D4 - t10 * 
     #t386 * t14 / 0.8D1
      t406 = FJET(XB1, XB2, s, 0.0D0, t5, 0.0D0, -t4, 0.0D0, t405)
      t409 = t25 * t19
      t412 = log(0.4D1 * x3 * t23 * t409)
      t413 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t415 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t422 = 0.180D3 * t37 * t38 * t413
      t426 = t413 * t12
      t432 = log(0.4D1 * t102 * t26)
      t447 = log(0.4D1 * t62 * t409)
      t461 = log(0.4D1 * t26 * t19)
      t466 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t470 = t461 ** 2
      t473 = lh ** 2
      t475 = pi ** 2
      t477 = 0.180D3 * t473 - 0.30D2 * t475
      t483 = -(0.90D2 * t8 * t9 * (t412 * t413 - t415) + t422) * t12 / 0
     #.1440D4 + t10 * t426 * t45 / 0.16D2 + (0.90D2 * t8 * t9 * (t415 - 
     #t432 * t413) - t422) * t45 / 0.1440D4 + t10 * t413 * t45 * t14 / 0
     #.8D1 + (0.90D2 * t8 * t9 * (-t447 * t413 + t415) - t422) * t14 / 0
     #.720D3 + t10 * t426 * t14 / 0.8D1 + (-0.180D3 * (-t461 * t413 + t4
     #15) * lh + 0.90D2 * t466 - 0.90D2 * t461 * t415 + 0.45D2 * t470 * 
     #t413 + t413 * t477) * t9 * t8 / 0.1440D4
      t484 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t483)
      t486 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, t5, 0.0D0,
     # 0.0D0, 0.0D0)
      t487 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, t5, 0.0D0,
     # 0.0D0, 0.0D0)
      t499 = t487 * t12
      t506 = -(0.90D2 * t8 * t9 * (t486 - t30 * t487) - 0.180D3 * t37 * 
     #t38 * t487) * t12 / 0.1440D4 - t10 * t499 * t45 / 0.16D2 - t10 * t
     #499 * t14 / 0.8D1
      t507 = FJET(XB1, XB2, s, t5, 0.0D0, -t4, 0.0D0, 0.0D0, t506)
      t509 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t511 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t518 = 0.180D3 * t37 * t38 * t509
      t522 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t537 = t509 * t12
      t564 = -(0.90D2 * t8 * t9 * (t412 * t509 - t511) + t518) * t12 / 0
     #.1440D4 + (0.90D2 * t522 - 0.90D2 * t461 * t511 + 0.45D2 * t470 * 
     #t509 - 0.180D3 * (-t461 * t509 + t511) * lh + t509 * t477) * t9 * 
     #t8 / 0.1440D4 + t10 * t537 * t45 / 0.16D2 + (0.90D2 * t8 * t9 * (-
     #t432 * t509 + t511) - t518) * t45 / 0.1440D4 + t10 * t509 * t45 * 
     #t14 / 0.8D1 + (0.90D2 * t8 * t9 * (-t447 * t509 + t511) - t518) * 
     #t14 / 0.720D3 + t10 * t537 * t14 / 0.8D1
      t565 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t564)
      t567 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t569 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t576 = 0.180D3 * t37 * t38 * t567
      t587 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t595 = t567 * t12
      t622 = -(0.90D2 * t8 * t9 * (t412 * t567 - t569) + t576) * t12 / 0
     #.1440D4 + (-0.90D2 * t461 * t569 - 0.180D3 * (-t461 * t567 + t569)
     # * lh + t567 * t477 + 0.90D2 * t587 + 0.45D2 * t470 * t567) * t9 *
     # t8 / 0.1440D4 + t10 * t595 * t45 / 0.16D2 + (0.90D2 * t8 * t9 * (
     #-t432 * t567 + t569) - t576) * t45 / 0.1440D4 + t10 * t567 * t45 *
     # t14 / 0.8D1 + (0.90D2 * t8 * t9 * (-t447 * t567 + t569) - t576) *
     # t14 / 0.720D3 + t10 * t595 * t14 / 0.8D1
      t623 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t622)
      t625 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t185, t193, 0.
     #0D0, 0.0D0, 0.0D0)
      t630 = FJET(XB1, XB2, s, 0.0D0, t193, 0.0D0, -t185, 0.0D0, -t197 *
     # t625 * t205 * t207 / 0.16D2)
      t637 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t4, t5, 0.0D0,
     # 0.0D0, 0.0D0)
      t638 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t4, t5, 0.0D0,
     # 0.0D0, 0.0D0)
      t650 = t638 * t12
      t657 = -(0.90D2 * t8 * t9 * (t637 - t30 * t638) - 0.180D3 * t37 * 
     #t38 * t638) * t12 / 0.1440D4 - t10 * t650 * t45 / 0.16D2 - t10 * t
     #650 * t14 / 0.8D1
      t658 = FJET(XB1, XB2, s, -t4, 0.0D0, t5, 0.0D0, 0.0D0, t657)
      t660 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t292, -t296, -t
     #294, t298, 0.0D0)
      t665 = FJET(XB1, XB2, s, t298, -t296, -t294, t292, 0.0D0, t10 * t6
     #60 * t12 * t14 / 0.8D1)
      t672 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t673 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t681 = 0.180D3 * t37 * t38 * t673
      t685 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t700 = t673 * t12
      t727 = -(0.90D2 * t8 * t9 * (-t672 + t412 * t673) + t681) * t12 / 
     #0.1440D4 + (0.90D2 * t685 - 0.180D3 * (t672 - t461 * t673) * lh - 
     #0.90D2 * t461 * t672 + 0.45D2 * t470 * t673 + t673 * t477) * t9 * 
     #t8 / 0.1440D4 + t10 * t700 * t45 / 0.16D2 + (0.90D2 * t8 * t9 * (t
     #672 - t432 * t673) - t681) * t45 / 0.1440D4 + t10 * t673 * t45 * t
     #14 / 0.8D1 + (0.90D2 * t8 * t9 * (t672 - t447 * t673) - t681) * t1
     #4 / 0.720D3 + t10 * t700 * t14 / 0.8D1
      t728 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t727)
      t730 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t292, -t296, -t
     #294, t298, 0.0D0)
      t735 = FJET(XB1, XB2, s, -t296, t298, t292, -t294, 0.0D0, t10 * t7
     #30 * t12 * t14 / 0.8D1)
      t742 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t257, -t260, t5
     #4, 0.0D0, -t265)
      t747 = FJET(XB1, XB2, s, t257, t54, -t260, 0.0D0, -t265, t10 * t74
     #2 * t45 * t14 / 0.8D1)
      t754 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t292, -t296, -t
     #294, t298, 0.0D0)
      t759 = FJET(XB1, XB2, s, -t294, t292, t298, -t296, 0.0D0, t10 * t7
     #54 * t12 * t14 / 0.8D1)
      t766 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t257, -t260, t5
     #4, 0.0D0, -t265)
      t771 = FJET(XB1, XB2, s, 0.0D0, -t260, t54, t257, -t265, t10 * t76
     #6 * t45 * t14 / 0.8D1)
      t778 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t185, t193, 0.
     #0D0, 0.0D0, 0.0D0)
      t783 = FJET(XB1, XB2, s, 0.0D0, -t185, 0.0D0, t193, 0.0D0, -t197 *
     # t778 * t205 * t207 / 0.16D2)
      t790 = t406 * t405 + t484 * t483 + t507 * t506 + t565 * t564 + t62
     #3 * t622 - t630 * t7 * t38 * t195 * t625 * t216 / 0.16D2 + t658 * 
     #t657 + t665 * t7 * pi * t9 * t660 * t308 / 0.8D1 + t728 * t727 + t
     #735 * t7 * pi * t9 * t730 * t308 / 0.8D1 + t747 * t7 * pi * t9 * t
     #742 * t275 / 0.8D1 + t759 * t7 * pi * t9 * t754 * t308 / 0.8D1 + t
     #771 * t7 * pi * t9 * t766 * t275 / 0.8D1 - t783 * t7 * t38 * t195 
     #* t778 * t216 / 0.16D2
      rrgq2qght8s2em1 = t384 + t790

      end function



      doubleprecision function rrgq2qght8s2em2
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = t6 * t7
      t9 = 0.1D1 / x3
      t13 = 0.1D1 / x2
      t17 = 0.1D1 / x1
      t23 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t25 = z ** 2
      t28 = Sin(x4 * pi)
      t29 = t28 ** 2
      t31 = t1 ** 2
      t32 = t31 ** 2
      t35 = log(0.4D1 / t25 * t29 * t32)
      t42 = t5 * t8 * t9 / 0.16D2 + t5 * t8 * t13 / 0.16D2 + t5 * t8 * t
     #17 / 0.8D1 + (-0.180D3 * lh * t7 + 0.90D2 * t23 - 0.90D2 * t35 * t
     #7) * t6 * t5 / 0.1440D4
      t43 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t42)
      t45 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t46 = t6 * t45
      t60 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t66 = t5 * t46 * t9 / 0.16D2 + t5 * t46 * t13 / 0.16D2 + t5 * t46 
     #* t17 / 0.8D1 + (-0.90D2 * t35 * t45 - 0.180D3 * t45 * lh + 0.90D2
     # * t60) * t6 * t5 / 0.1440D4
      t67 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t66)
      t69 = t2 * x1
      t71 = t2 * (-0.1D1 + x1)
      t72 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t71, 0.0D0, t69
     #, 0.0D0, 0.0D0)
      t74 = t6 * t72 * t17
      t77 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t69, -t71, 0.0D0, -t5 * t74 
     #/ 0.8D1)
      t82 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t71, 0.0D0, t69
     #, 0.0D0, 0.0D0)
      t84 = t6 * t82 * t17
      t87 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t71, t69, 0.0D0, -t5 * t84 
     #/ 0.8D1)
      t92 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t93 = t6 * t92
      t105 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t113 = t5 * t93 * t9 / 0.16D2 + t5 * t93 * t13 / 0.16D2 + t5 * t93
     # * t17 / 0.8D1 + (-0.180D3 * t92 * lh + 0.90D2 * t105 - 0.90D2 * t
     #35 * t92) * t6 * t5 / 0.1440D4
      t114 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t113)
      t116 = t2 * x3
      t118 = t2 * (-0.1D1 + x3)
      t119 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t118, t116, 0.
     #0D0, 0.0D0, 0.0D0)
      t121 = t6 * t119 * t9
      t124 = FJET(XB1, XB2, s, 0.0D0, t116, 0.0D0, -t118, 0.0D0, -t5 * t
     #121 / 0.16D2)
      t130 = x2 * s * t1
      t133 = (-0.1D1 + x2) * s * t1
      t134 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t133, t130, 0.
     #0D0, 0.0D0, 0.0D0)
      t136 = t6 * t134 * t13
      t139 = FJET(XB1, XB2, s, 0.0D0, t130, 0.0D0, -t133, 0.0D0, -t5 * t
     #136 / 0.16D2)
      t144 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t118, t116, 0.
     #0D0, 0.0D0, 0.0D0)
      t146 = t6 * t144 * t9
      t149 = FJET(XB1, XB2, s, 0.0D0, -t118, 0.0D0, t116, 0.0D0, -t5 * t
     #146 / 0.16D2)
      t154 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t133, t130, 0.
     #0D0, 0.0D0, 0.0D0)
      t156 = t6 * t154 * t13
      t159 = FJET(XB1, XB2, s, 0.0D0, -t133, 0.0D0, t130, 0.0D0, -t5 * t
     #156 / 0.16D2)
      t164 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t165 = t6 * t164
      t177 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t185 = t5 * t165 * t9 / 0.16D2 + t5 * t165 * t13 / 0.16D2 + t5 * t
     #165 * t17 / 0.8D1 + (-0.180D3 * t164 * lh + 0.90D2 * t177 - 0.90D2
     # * t35 * t164) * t6 * t5 / 0.1440D4
      t186 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t185)
      t188 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t71, 0.0D0, t6
     #9, 0.0D0, 0.0D0)
      t190 = t6 * t188 * t17
      t193 = FJET(XB1, XB2, s, t69, -t71, 0.0D0, 0.0D0, 0.0D0, -t5 * t19
     #0 / 0.8D1)
      t198 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t118, t116, 0.
     #0D0, 0.0D0, 0.0D0)
      t200 = t6 * t198 * t9
      t203 = FJET(XB1, XB2, s, t116, 0.0D0, -t118, 0.0D0, 0.0D0, -t5 * t
     #200 / 0.16D2)
      t208 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t133, t130, 0.
     #0D0, 0.0D0, 0.0D0)
      t210 = t6 * t208 * t13
      t213 = FJET(XB1, XB2, s, t130, 0.0D0, -t133, 0.0D0, 0.0D0, -t5 * t
     #210 / 0.16D2)
      t218 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t118, t116, 0.
     #0D0, 0.0D0, 0.0D0)
      t220 = t6 * t218 * t9
      t223 = FJET(XB1, XB2, s, -t118, 0.0D0, t116, 0.0D0, 0.0D0, -t5 * t
     #220 / 0.16D2)
      t228 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t133, t130, 0.
     #0D0, 0.0D0, 0.0D0)
      t230 = t6 * t228 * t13
      t233 = FJET(XB1, XB2, s, -t133, 0.0D0, t130, 0.0D0, 0.0D0, -t5 * t
     #230 / 0.16D2)
      t238 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t71, 0.0D0, t6
     #9, 0.0D0, 0.0D0)
      t240 = t6 * t238 * t17
      t243 = FJET(XB1, XB2, s, -t71, t69, 0.0D0, 0.0D0, 0.0D0, -t5 * t24
     #0 / 0.8D1)
      rrgq2qght8s2em2 = t42 * t43 + t67 * t66 - t77 * t4 * pi * t74 / 0.
     #8D1 - t87 * t4 * pi * t84 / 0.8D1 + t114 * t113 - t124 * t4 * pi *
     # t121 / 0.16D2 - t139 * t4 * pi * t136 / 0.16D2 - t149 * t4 * pi *
     # t146 / 0.16D2 - t159 * t4 * pi * t156 / 0.16D2 + t186 * t185 - t1
     #93 * t4 * pi * t190 / 0.8D1 - t203 * t4 * pi * t200 / 0.16D2 - t21
     #3 * t4 * pi * t210 / 0.16D2 - t223 * t4 * pi * t220 / 0.16D2 - t23
     #3 * t4 * pi * t230 / 0.16D2 - t243 * t4 * pi * t240 / 0.8D1

      end function



      doubleprecision function rrgq2qght8s2em3
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t5 * t6 * 
     #t7 / 0.16D2)
      t13 = pi * t6
      t16 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t5 * t6 * 
     #t16 / 0.16D2)
      t24 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t5 * t6 * 
     #t24 / 0.16D2)
      t32 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t5 * t6 * 
     #t32 / 0.16D2)
      rrgq2qght8s2em3 = t11 * t4 * t13 * t7 / 0.16D2 + t20 * t4 * t13 * 
     #t16 / 0.16D2 + t28 * t4 * t13 * t24 / 0.16D2 + t36 * t4 * t13 * t3
     #2 / 0.16D2

      end function



      doubleprecision function rrgq2qght8s2em4
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght8s2em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght8s3e1
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t11 = lh * t4
      t12 = pi * t6
      t13 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t14 = t12 * t13
      t17 = lh ** 2
      t18 = 0.180D3 * t17
      t19 = pi ** 2
      t20 = 0.30D2 * t19
      t21 = t18 - t20
      t22 = t21 * t4
      t23 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t24 = t12 * t23
      t26 = 0.90D2 * t5 * t6 * t7 - 0.180D3 * t11 * t14 + t22 * t24
      t27 = z ** 2
      t29 = 0.1D1 / t27 / z
      t30 = x3 * t29
      t31 = x4 * pi
      t32 = Sin(t31)
      t33 = t32 ** 2
      t34 = t1 ** 2
      t35 = t34 ** 2
      t36 = t33 * t35
      t39 = log(0.4D1 * t30 * t36)
      t40 = -0.1D1 + x3
      t41 = 0.1D1 / t40
      t42 = t36 * t41
      t45 = log(-0.4D1 * t30 * t42)
      t47 = cos(t31)
      t48 = x3 * z
      t50 = Sqrt(-t48 * t40)
      t54 = 0.1D1 / (-z - x3 + 0.2D1 * t47 * t50)
      t56 = t39 + t45 * z * t54
      t58 = t6 * t23
      t59 = t45 ** 2
      t63 = t39 ** 2
      t66 = t59 * t45 * z * t54 / 0.6D1 + t63 * t39 / 0.6D1
      t71 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t76 = 0.60D2 * lh * t19
      t77 = 0.240D3 * zeta3
      t79 = 0.120D3 * t17 * lh
      t80 = t76 - t77 - t79
      t81 = t80 * t4
      t82 = t81 * t24
      t88 = -0.1D1 - z * t54
      t95 = 0.90D2 * t5 * t6 * t13 - 0.180D3 * t11 * t24
      t99 = -t63 / 0.2D1 - t59 * z * t54 / 0.2D1
      t102 = 0.1D1 / x3
      t105 = z * t7
      t106 = x2 ** 2
      t107 = x3 * t106
      t108 = t107 * t29
      t111 = log(-0.4D1 * t108 * t42)
      t112 = t111 * z
      t114 = t111 ** 2
      t115 = t114 * z
      t120 = t29 * t33
      t121 = t120 * t35
      t124 = log(0.4D1 * t107 * t121)
      t126 = -0.1D1 + x2
      t127 = t36 * t126
      t130 = log(-0.4D1 * t108 * t127)
      t132 = t124 ** 2
      t135 = t130 ** 2
      t143 = z * t13
      t152 = t22 * pi
      t153 = t6 * z
      t155 = t153 * t23 * t54
      t159 = 0.1D1 / x2
      t162 = t29 * t106
      t165 = log(0.4D1 * t162 * t36)
      t166 = t165 ** 2
      t169 = log(-0.4D1 * t162 * t127)
      t170 = t169 ** 2
      t172 = t166 / 0.2D1 - t170 / 0.2D1
      t177 = t170 * t169 / 0.6D1 - t166 * t165 / 0.6D1
      t181 = -t165 + t169
      t186 = rrgq2qgh82J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t191 = log(0.4D1 * t121)
      t192 = t191 ** 2
      t195 = t192 * t191
      t199 = (-0.90D2 * t192 * lh + t76 - t77 - t79 - 0.15D2 * t195 - t1
     #91 * t21) * t6
      t207 = (0.180D3 * t191 * lh + 0.45D2 * t192 + t18 - t20) * t6
      t214 = (-0.180D3 * lh - 0.90D2 * t191) * t6
      t218 = x1 ** 2
      t219 = x3 * t218
      t222 = log(0.4D1 * t219 * t121)
      t224 = t222 ** 2
      t227 = t219 * t33
      t228 = t29 * t35
      t232 = log(-0.4D1 * t227 * t228 * t41)
      t233 = t232 * z
      t235 = t232 ** 2
      t236 = t235 * z
      t260 = 0.1D1 / x1
      t263 = t218 * t33
      t266 = log(0.4D1 * t263 * t228)
      t271 = t266 ** 2
      t274 = t271 * t266
      t292 = t107 * t218
      t297 = log(-0.4D1 * t292 * t120 * t35 * t41)
      t298 = t297 * z
      t304 = log(0.4D1 * t292 * t121)
      t306 = t35 * t126
      t310 = log(-0.4D1 * t292 * t120 * t306)
      t316 = t11 * pi
      t321 = t159 * t260
      t324 = t106 * t218
      t327 = log(0.4D1 * t324 * t121)
      t329 = t327 ** 2
      t332 = t324 * t33
      t336 = log(-0.4D1 * t332 * t228 * t126)
      t338 = t336 ** 2
      t360 = t19 ** 2
      t361 = t17 ** 2
      t367 = t192 ** 2
      t370 = (0.30D2 * t195 * lh + t192 * t21 / 0.2D1 - t191 * t80 + t36
     #0 + 0.60D2 * t361 + 0.480D3 * lh * zeta3 - 0.60D2 * t17 * t19 + 0.
     #15D2 / 0.4D1 * t367) * t6
      t374 = -(t26 * t56 + 0.90D2 * t5 * t58 * t66 + (t22 * t14 + 0.90D2
     # * t5 * t6 * t71 + t82 - 0.180D3 * t11 * t12 * t7) * t88 + t95 * t
     #99) * t102 / 0.2880D4 - (0.90D2 * t5 * t6 * (-(t105 - t112 * t13 +
     # t115 * t23 / 0.2D1) * t54 + t124 * t13 - t130 * t13 - t132 * t23 
     #/ 0.2D1 + t135 * t23 / 0.2D1) - 0.180D3 * t11 * t12 * (t124 * t23 
     #- (t143 - t112 * t23) * t54 - t130 * t23) - t152 * t155) * t102 * 
     #t159 / 0.1440D4 + (t95 * t172 + 0.90D2 * t5 * t58 * t177 + t26 * t
     #181) * t159 / 0.1440D4 + t5 * t6 * t186 / 0.32D2 + t199 * t5 * t13
     # / 0.2880D4 + t207 * t5 * t7 / 0.2880D4 + t214 * t5 * t71 / 0.2880
     #D4 + (0.90D2 * t5 * t6 * (t7 - t222 * t13 + t224 * t23 / 0.2D1 + (
     #t105 - t233 * t13 + t236 * t23 / 0.2D1) * t54) - 0.180D3 * t11 * t
     #12 * (t13 - t222 * t23 + (t143 - t233 * t23) * t54) + t22 * t12 * 
     #(z * t23 * t54 + t23)) * t102 * t260 / 0.1440D4 + (t22 * t12 * (t1
     #3 - t266 * t23) + 0.90D2 * t5 * t6 * (t71 + t271 * t13 / 0.2D1 - t
     #274 * t23 / 0.6D1 - t266 * t7) + t82 - 0.180D3 * t11 * t12 * (t7 +
     # t271 * t23 / 0.2D1 - t266 * t13)) * t260 / 0.1440D4 + (0.90D2 * t
     #5 * t6 * ((t143 - t298 * t23) * t54 - t304 * t23 + t310 * t23) - 0
     #.180D3 * t316 * t155) * t102 * t321 / 0.720D3 + (0.90D2 * t5 * t6 
     #* (-t327 * t13 + t329 * t23 / 0.2D1 + t336 * t13 - t338 * t23 / 0.
     #2D1) - 0.180D3 * t11 * t12 * (t336 * t23 - t327 * t23)) * t159 * t
     #260 / 0.720D3 + t370 * t5 * t23 / 0.2880D4
      t375 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t374)
      t377 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t381 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t385 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t386 = t12 * t385
      t389 = t12 * t377
      t391 = 0.90D2 * t5 * t6 * t381 - 0.180D3 * t11 * t386 + t22 * t389
      t393 = t6 * t377
      t398 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t402 = t81 * t389
      t413 = 0.90D2 * t5 * t6 * t385 - 0.180D3 * t11 * t389
      t418 = z * t381
      t434 = z * t385
      t445 = t153 * t377 * t54
      t459 = rrgq2qgh84J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t559 = t370 * t5 * t377 / 0.2880D4 - (t391 * t56 + 0.90D2 * t5 * t
     #393 * t66 + (t22 * t386 + 0.90D2 * t5 * t6 * t398 + t402 - 0.180D3
     # * t11 * t12 * t381) * t88 + t413 * t99) * t102 / 0.2880D4 - (0.90
     #D2 * t5 * t6 * (-(t418 - t112 * t385 + t115 * t377 / 0.2D1) * t54 
     #+ t135 * t377 / 0.2D1 + t124 * t385 - t130 * t385 - t132 * t377 / 
     #0.2D1) - 0.180D3 * t11 * t12 * (-(t434 - t112 * t377) * t54 - t130
     # * t377 + t124 * t377) - t152 * t445) * t102 * t159 / 0.1440D4 + (
     #t413 * t172 + 0.90D2 * t5 * t393 * t177 + t391 * t181) * t159 / 0.
     #1440D4 + t5 * t6 * t459 / 0.32D2 + t199 * t5 * t385 / 0.2880D4 + t
     #207 * t5 * t381 / 0.2880D4 + (0.90D2 * t5 * t6 * (-t222 * t385 + t
     #224 * t377 / 0.2D1 + (t418 - t233 * t385 + t236 * t377 / 0.2D1) * 
     #t54 + t381) - 0.180D3 * t11 * t12 * (t385 - t222 * t377 + (t434 - 
     #t233 * t377) * t54) + t22 * t12 * (t377 + z * t377 * t54)) * t102 
     #* t260 / 0.1440D4 + (t22 * t12 * (t385 - t266 * t377) + 0.90D2 * t
     #5 * t6 * (t398 + t271 * t385 / 0.2D1 - t274 * t377 / 0.6D1 - t266 
     #* t381) + t402 - 0.180D3 * t11 * t12 * (t381 - t266 * t385 + t271 
     #* t377 / 0.2D1)) * t260 / 0.1440D4 + (0.90D2 * t5 * t6 * (-t304 * 
     #t377 + (t434 - t298 * t377) * t54 + t310 * t377) - 0.180D3 * t316 
     #* t445) * t102 * t321 / 0.720D3 + (0.90D2 * t5 * t6 * (-t338 * t37
     #7 / 0.2D1 + t329 * t377 / 0.2D1 - t327 * t385 + t336 * t385) - 0.1
     #80D3 * t11 * t12 * (t336 * t377 - t327 * t377)) * t159 * t260 / 0.
     #720D3 + t214 * t5 * t398 / 0.2880D4
      t560 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t559)
      t562 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t566 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t570 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t574 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t575 = t12 * t574
      t578 = t12 * t566
      t580 = 0.90D2 * t5 * t6 * t570 - 0.180D3 * t11 * t575 + t22 * t578
      t582 = t6 * t566
      t590 = t81 * t578
      t601 = 0.90D2 * t5 * t6 * t574 - 0.180D3 * t11 * t578
      t606 = z * t570
      t622 = z * t574
      t633 = t153 * t566 * t54
      t647 = rrgq2qgh81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t744 = t214 * t5 * t562 / 0.2880D4 + t370 * t5 * t566 / 0.2880D4 -
     # (t580 * t56 + 0.90D2 * t5 * t582 * t66 + (t22 * t575 + 0.90D2 * t
     #5 * t6 * t562 + t590 - 0.180D3 * t11 * t12 * t570) * t88 + t601 * 
     #t99) * t102 / 0.2880D4 - (0.90D2 * t5 * t6 * (-(t606 - t112 * t574
     # + t115 * t566 / 0.2D1) * t54 + t135 * t566 / 0.2D1 + t124 * t574 
     #- t130 * t574 - t132 * t566 / 0.2D1) - 0.180D3 * t11 * t12 * (-(t6
     #22 - t112 * t566) * t54 - t130 * t566 + t124 * t566) - t152 * t633
     #) * t102 * t159 / 0.1440D4 + (t601 * t172 + 0.90D2 * t5 * t582 * t
     #177 + t580 * t181) * t159 / 0.1440D4 + t5 * t6 * t647 / 0.32D2 + t
     #199 * t5 * t574 / 0.2880D4 + (0.90D2 * t5 * t6 * (-t222 * t574 + t
     #224 * t566 / 0.2D1 + t570 + (t606 - t233 * t574 + t236 * t566 / 0.
     #2D1) * t54) - 0.180D3 * t11 * t12 * (t574 - t222 * t566 + (t622 - 
     #t233 * t566) * t54) + t22 * t12 * (z * t566 * t54 + t566)) * t102 
     #* t260 / 0.1440D4 + (t22 * t12 * (t574 - t266 * t566) + 0.90D2 * t
     #5 * t6 * (-t274 * t566 / 0.6D1 + t562 + t271 * t574 / 0.2D1 - t266
     # * t570) + t590 - 0.180D3 * t11 * t12 * (t271 * t566 / 0.2D1 - t26
     #6 * t574 + t570)) * t260 / 0.1440D4 + (0.90D2 * t5 * t6 * (-t304 *
     # t566 + t310 * t566 + (t622 - t298 * t566) * t54) - 0.180D3 * t316
     # * t633) * t102 * t321 / 0.720D3 + (0.90D2 * t5 * t6 * (t336 * t57
     #4 - t327 * t574 - t338 * t566 / 0.2D1 + t329 * t566 / 0.2D1) - 0.1
     #80D3 * t11 * t12 * (-t327 * t566 + t336 * t566)) * t159 * t260 / 0
     #.720D3 + t207 * t5 * t570 / 0.2880D4
      t745 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t744)
      t747 = x2 * x3
      t748 = 0.1D1 - x3 + t747
      t749 = 0.1D1 / t748
      t750 = t747 * t749
      t751 = t2 * t750
      t753 = t2 * t40 * t749
      t754 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t753, t751, 0.0D0)
      t757 = t748 ** 2
      t758 = 0.1D1 / t757
      t763 = log(0.4D1 * t107 * t120 * t306 * t40 * t758)
      t764 = t763 * z
      t765 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t753, t751, 0.0D0)
      t767 = t763 ** 2
      t768 = t767 * z
      t769 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t753, t751, 0.0D0)
      t774 = t126 * t40
      t776 = Sqrt(t48 * t774)
      t780 = 0.1D1 / (-z - x3 + t747 + 0.2D1 * t47 * t776)
      t784 = z * t765
      t792 = t153 * t769 * t780
      t798 = t107 * t263
      t799 = t774 * t758
      t803 = log(0.4D1 * t798 * t228 * t799)
      t804 = t803 * z
      t817 = -(0.90D2 * t5 * t6 * (z * t754 - t764 * t765 + t768 * t769 
     #/ 0.2D1) * t780 - 0.180D3 * t316 * t6 * (t784 - t764 * t769) * t78
     #0 + t152 * t792) * t102 * t159 / 0.1440D4 + (-0.90D2 * t5 * t6 * (
     #t784 - t804 * t769) * t780 + 0.180D3 * t316 * t792) * t102 * t321 
     #/ 0.720D3
      t818 = FJET(XB1, XB2, s, 0.0D0, t751, 0.0D0, -t753, 0.0D0, t817)
      t821 = t1 * x1
      t822 = x1 * z
      t823 = -z - x1 + t822
      t824 = 0.1D1 / t823
      t826 = t126 * s * t821 * t824
      t827 = -0.1D1 + x1
      t828 = t2 * t827
      t830 = x2 * s * t821
      t831 = s * t34
      t834 = x1 * t827 * t824
      t835 = t831 * t126 * t834
      t836 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t830, t826, -t8
     #28, 0.0D0, -t835)
      t837 = 0.1D1 / t27
      t838 = t837 * t35
      t839 = t827 ** 2
      t840 = t824 * t839
      t845 = log(0.4D1 * t798 * t838 * t840 * t126)
      t846 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t830, t826, -t8
     #28, 0.0D0, -t835)
      t852 = t12 * t846
      t858 = t33 * t837
      t860 = t35 * t824
      t865 = log(0.4D1 * t324 * t858 * t860 * t839 * t126)
      t866 = t865 ** 2
      t869 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t830, t826, -t8
     #28, 0.0D0, -t835)
      t885 = (0.90D2 * t5 * t6 * (t836 - t845 * t846) - 0.180D3 * t11 * 
     #t852) * t102 * t321 / 0.720D3 + (0.90D2 * t5 * t6 * (t866 * t846 /
     # 0.2D1 + t869 - t865 * t836) - 0.180D3 * t11 * t12 * (-t865 * t846
     # + t836) + t22 * t852) * t159 * t260 / 0.720D3
      t886 = FJET(XB1, XB2, s, 0.0D0, t826, -t828, t830, -t835, t885)
      t889 = t2 * x1 * t824
      t890 = t831 * t834
      t891 = t838 * t840
      t894 = log(-0.4D1 * t227 * t891)
      t895 = t894 ** 2
      t896 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, -
     #t828, 0.0D0, t890)
      t899 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, -
     #t828, 0.0D0, t890)
      t906 = log(0.4D1 * t219 * t858 * t860 * t839 * t41)
      t907 = t906 * z
      t908 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, -
     #t828, 0.0D0, t890)
      t910 = t906 ** 2
      t911 = t910 * z
      t916 = x3 * x1
      t917 = t916 * z
      t919 = 0.2D1 * t219 * z
      t920 = x1 * t27
      t921 = x3 * t27
      t922 = t921 * x1
      t923 = t219 * t27
      t924 = x3 * t823
      t926 = Sqrt(t924 * t40)
      t931 = 0.1D1 / (-t822 - t917 + t919 + t920 + t922 - t923 - t219 - 
     #t48 - t27 + 0.2D1 * t47 * t926 * z)
      t939 = z * t908
      t949 = t823 * t931
      t952 = t12 * (-t896 + z * t896 * t949)
      t958 = t263 * t837
      t959 = t860 * t839
      t962 = log(-0.4D1 * t958 * t959)
      t967 = t962 ** 2
      t970 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, -
     #t828, 0.0D0, t890)
      t971 = t967 * t962
      t979 = t12 * t896
      t995 = log(0.4D1 * t798 * t838 * t840 * t41)
      t996 = t995 * z
      t1003 = log(-0.4D1 * t798 * t891)
      t1017 = log(-0.4D1 * t332 * t891)
      t1018 = t1017 ** 2
      t1036 = (0.90D2 * t5 * t6 * (-t895 * t896 / 0.2D1 - t899 + (z * t8
     #99 - t907 * t908 + t911 * t896 / 0.2D1) * t823 * t931 + t894 * t90
     #8) - 0.180D3 * t11 * t12 * (-t908 + t894 * t896 + (t939 - t907 * t
     #896) * t823 * t931) + t22 * t952) * t102 * t260 / 0.1440D4 + (t22 
     #* t12 * (t962 * t896 - t908) + 0.90D2 * t5 * t6 * (-t967 * t908 / 
     #0.2D1 - t970 + t971 * t896 / 0.6D1 + t962 * t899) - t81 * t979 - 0
     #.180D3 * t11 * t12 * (t962 * t908 - t967 * t896 / 0.2D1 - t899)) *
     # t260 / 0.1440D4 + (0.90D2 * t5 * t6 * ((t939 - t996 * t896) * t82
     #3 * t931 + t1003 * t896 - t908) - 0.180D3 * t11 * t952) * t102 * t
     #321 / 0.720D3 + (0.90D2 * t5 * t6 * (-t1018 * t896 / 0.2D1 + t1017
     # * t908 - t899) - 0.180D3 * t11 * t12 * (-t908 + t1017 * t896) - t
     #22 * t979) * t159 * t260 / 0.720D3
      t1037 = FJET(XB1, XB2, s, 0.0D0, -t828, -t889, 0.0D0, t890, t1036)
      t1039 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, 
     #-t828, 0.0D0, t890)
      t1041 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, 
     #-t828, 0.0D0, t890)
      t1043 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, 
     #-t828, 0.0D0, t890)
      t1056 = z * t1041
      t1069 = t12 * (-t1043 + z * t1043 * t949)
      t1081 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, 
     #-t828, 0.0D0, t890)
      t1089 = t12 * t1043
      t1133 = (0.90D2 * t5 * t6 * ((z * t1039 - t907 * t1041 + t911 * t1
     #043 / 0.2D1) * t823 * t931 + t894 * t1041 - t895 * t1043 / 0.2D1 -
     # t1039) - 0.180D3 * t11 * t12 * (-t1041 + (t1056 - t907 * t1043) *
     # t823 * t931 + t894 * t1043) + t22 * t1069) * t102 * t260 / 0.1440
     #D4 + (t22 * t12 * (-t1041 + t962 * t1043) + 0.90D2 * t5 * t6 * (-t
     #967 * t1041 / 0.2D1 - t1081 + t962 * t1039 + t971 * t1043 / 0.6D1)
     # - t81 * t1089 - 0.180D3 * t11 * t12 * (t962 * t1041 - t967 * t104
     #3 / 0.2D1 - t1039)) * t260 / 0.1440D4 + (0.90D2 * t5 * t6 * (t1003
     # * t1043 + (t1056 - t996 * t1043) * t823 * t931 - t1041) - 0.180D3
     # * t11 * t1069) * t102 * t321 / 0.720D3 + (0.90D2 * t5 * t6 * (-t1
     #018 * t1043 / 0.2D1 - t1039 + t1017 * t1041) - 0.180D3 * t11 * t12
     # * (t1017 * t1043 - t1041) - t22 * t1089) * t159 * t260 / 0.720D3
      t1134 = FJET(XB1, XB2, s, 0.0D0, -t889, -t828, 0.0D0, t890, t1133)
      t1136 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t753, t751, 0.0D0)
      t1138 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t753, t751, 0.0D0)
      t1140 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t753, t751, 0.0D0)
      t1148 = z * t1138
      t1156 = t153 * t1140 * t780
      t1174 = -(0.90D2 * t5 * t6 * (z * t1136 - t764 * t1138 + t768 * t1
     #140 / 0.2D1) * t780 - 0.180D3 * t316 * t6 * (t1148 - t764 * t1140)
     # * t780 + t152 * t1156) * t102 * t159 / 0.1440D4 + (-0.90D2 * t5 *
     # t6 * (t1148 - t804 * t1140) * t780 + 0.180D3 * t316 * t1156) * t1
     #02 * t321 / 0.720D3
      t1175 = FJET(XB1, XB2, s, 0.0D0, -t753, 0.0D0, t751, 0.0D0, t1174)
      t1177 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1180 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1181 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1183 = z * t1180
      t1193 = z * t1181
      t1217 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1225 = t12 * t1177
      t1226 = t81 * t1225
      t1247 = t153 * t1177 * t54
      t1277 = t12 * t1181
      t1281 = 0.90D2 * t5 * t6 * t1180 - 0.180D3 * t11 * t1277 + t22 * t
     #1225
      t1283 = t6 * t1177
      t1301 = 0.90D2 * t5 * t6 * t1181 - 0.180D3 * t11 * t1225
      t1312 = rrgq2qgh83J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1359 = (0.90D2 * t5 * t6 * (t224 * t1177 / 0.2D1 + t1180 - t222 *
     # t1181 + (t1183 - t233 * t1181 + t236 * t1177 / 0.2D1) * t54) - 0.
     #180D3 * t11 * t12 * (t1181 + (t1193 - t233 * t1177) * t54 - t222 *
     # t1177) + t22 * t12 * (t1177 + z * t1177 * t54)) * t102 * t260 / 0
     #.1440D4 + (t22 * t12 * (-t266 * t1177 + t1181) + 0.90D2 * t5 * t6 
     #* (t271 * t1181 / 0.2D1 + t1217 - t266 * t1180 - t274 * t1177 / 0.
     #6D1) + t1226 - 0.180D3 * t11 * t12 * (t1180 - t266 * t1181 + t271 
     #* t1177 / 0.2D1)) * t260 / 0.1440D4 + (0.90D2 * t5 * t6 * (-t304 *
     # t1177 + (t1193 - t298 * t1177) * t54 + t310 * t1177) - 0.180D3 * 
     #t316 * t1247) * t102 * t321 / 0.720D3 + (0.90D2 * t5 * t6 * (-t338
     # * t1177 / 0.2D1 + t329 * t1177 / 0.2D1 + t336 * t1181 - t327 * t1
     #181) - 0.180D3 * t11 * t12 * (-t327 * t1177 + t336 * t1177)) * t15
     #9 * t260 / 0.720D3 - (t1281 * t56 + 0.90D2 * t5 * t1283 * t66 + (t
     #22 * t1277 + 0.90D2 * t5 * t6 * t1217 + t1226 - 0.180D3 * t11 * t1
     #2 * t1180) * t88 + t1301 * t99) * t102 / 0.2880D4 + t207 * t5 * t1
     #180 / 0.2880D4 + t370 * t5 * t1177 / 0.2880D4 + t5 * t6 * t1312 / 
     #0.32D2 + t199 * t5 * t1181 / 0.2880D4 + t214 * t5 * t1217 / 0.2880
     #D4 - (0.90D2 * t5 * t6 * (-(t1183 - t112 * t1181 + t115 * t1177 / 
     #0.2D1) * t54 + t124 * t1181 - t130 * t1181 - t132 * t1177 / 0.2D1 
     #+ t135 * t1177 / 0.2D1) - 0.180D3 * t11 * t12 * (-(t1193 - t112 * 
     #t1177) * t54 - t130 * t1177 + t124 * t1177) - t152 * t1247) * t102
     # * t159 / 0.1440D4 + (t1301 * t172 + 0.90D2 * t5 * t1283 * t177 + 
     #t1281 * t181) * t159 / 0.1440D4
      t1360 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t1359)
      t1362 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t830, t826, -t
     #828, 0.0D0, -t835)
      t1364 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t830, t826, -t
     #828, 0.0D0, -t835)
      t1369 = t12 * t1362
      t1377 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t830, t826, -t
     #828, 0.0D0, -t835)
      t1393 = (0.90D2 * t5 * t6 * (-t845 * t1362 + t1364) - 0.180D3 * t1
     #1 * t1369) * t102 * t321 / 0.720D3 + (0.90D2 * t5 * t6 * (t866 * t
     #1362 / 0.2D1 + t1377 - t865 * t1364) - 0.180D3 * t11 * t12 * (t136
     #4 - t865 * t1362) + t22 * t1369) * t159 * t260 / 0.720D3
      t1394 = FJET(XB1, XB2, s, t830, -t828, t826, 0.0D0, -t835, t1393)
      t1396 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t753, t751, 0.0D0)
      t1397 = z * t1396
      t1398 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t753, t751, 0.0D0)
      t1406 = t153 * t1398 * t780
      t1413 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t753, t751, 0.0D0)
      t1434 = (-0.90D2 * t5 * t6 * (t1397 - t804 * t1398) * t780 + 0.180
     #D3 * t316 * t1406) * t102 * t321 / 0.720D3 - (0.90D2 * t5 * t6 * (
     #z * t1413 - t764 * t1396 + t768 * t1398 / 0.2D1) * t780 - 0.180D3 
     #* t316 * t6 * (t1397 - t764 * t1398) * t780 + t152 * t1406) * t102
     # * t159 / 0.1440D4
      t1435 = FJET(XB1, XB2, s, t751, 0.0D0, -t753, 0.0D0, 0.0D0, t1434)
      t1437 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t830, t826, -t
     #828, 0.0D0, -t835)
      t1439 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t830, t826, -t
     #828, 0.0D0, -t835)
      t1444 = t12 * t1437
      t1452 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t830, t826, -t
     #828, 0.0D0, -t835)
      t1468 = (0.90D2 * t5 * t6 * (-t845 * t1437 + t1439) - 0.180D3 * t1
     #1 * t1444) * t102 * t321 / 0.720D3 + (0.90D2 * t5 * t6 * (t866 * t
     #1437 / 0.2D1 + t1452 - t865 * t1439) - 0.180D3 * t11 * t12 * (-t86
     #5 * t1437 + t1439) + t22 * t1444) * t159 * t260 / 0.720D3
      t1469 = FJET(XB1, XB2, s, t826, 0.0D0, t830, -t828, -t835, t1468)
      t1474 = t40 * s * t1 * t827 * t749
      t1475 = t2 * x1
      t1477 = Sqrt(-t924 * t774)
      t1478 = t47 * t1477
      t1484 = t1475 * x2 * (-x3 + t747 - z + t48 - x1 + t916 + t822 - t9
     #17 + 0.2D1 * t1478) * t824 * t749
      t1485 = t828 * t750
      t1486 = t107 * t822
      t1489 = t107 * x1
      t1494 = t1475 * (-t1486 + 0.2D1 * t1478 * x2 + 0.1D1 - x3 - x2 + t
     #747 + t1489 + t107 * z) * t824 * t749
      t1495 = x2 * x1
      t1496 = t1495 * z
      t1497 = -z - t1495 + t1496
      t1498 = t823 * t1497
      t1499 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t1484, -t1494,
     # t1474, -t1485, -t835)
      t1505 = log(-0.4D1 * t107 * t958 * t959 * t799)
      t1506 = t1505 * t823
      t1507 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t1484, -t1494,
     # t1474, -t1485, -t835)
      t1516 = t27 * x2
      t1518 = t218 * x2
      t1524 = -t1489 + t1496 - 0.2D1 * t1478 * z + t218 * t27 * x2 - t15
     #16 * x1 - 0.2D1 * t1518 * z - t747 * z + t747 * x1 - t219 * x2 + t
     #822 - t920 + t219 + t48
      t1536 = 0.2D1 * t1478 * t1496 + t27 + t1518 + t1486 - 0.2D1 * t747
     # * t822 + t921 * t1495 + 0.2D1 * t219 * x2 * z - t219 * t1516 - 0.
     #2D1 * t1478 * t1495 + t917 - t919 - t922 + t923
      t1538 = 0.1D1 / (t1524 + t1536)
      t1542 = t11 * t12
      t1547 = 0.90D2 * t5 * t6 * (-t1498 * t1499 + t1506 * t1497 * t1507
     #) * t1538 + 0.180D3 * t1542 * t1498 * t1507 * t1538
      t1551 = FJET(XB1, XB2, s, t1474, t1484, -t1485, -t1494, -t835, t15
     #47 * t102 * t321 / 0.720D3)
      t1554 = t102 * t159 * t260
      t1557 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t1484, -t1494,
     # t1474, -t1485, -t835)
      t1559 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t1484, -t1494,
     # t1474, -t1485, -t835)
      t1571 = 0.90D2 * t5 * t6 * (-t1498 * t1557 + t1506 * t1497 * t1559
     #) * t1538 + 0.180D3 * t1542 * t1498 * t1559 * t1538
      t1575 = FJET(XB1, XB2, s, t1484, t1474, -t1494, -t1485, -t835, t15
     #71 * t102 * t321 / 0.720D3)
      t1579 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, 
     #-t828, 0.0D0, t890)
      t1580 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, 
     #-t828, 0.0D0, t890)
      t1584 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, 
     #-t828, 0.0D0, t890)
      t1597 = z * t1584
      t1609 = t12 * (z * t1580 * t949 - t1580)
      t1621 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, 
     #-t828, 0.0D0, t890)
      t1629 = t12 * t1580
      t1673 = (0.90D2 * t5 * t6 * (-t1579 - t895 * t1580 / 0.2D1 + (z * 
     #t1579 - t907 * t1584 + t911 * t1580 / 0.2D1) * t823 * t931 + t894 
     #* t1584) - 0.180D3 * t11 * t12 * (t894 * t1580 - t1584 + (t1597 - 
     #t907 * t1580) * t823 * t931) + t22 * t1609) * t102 * t260 / 0.1440
     #D4 + (t22 * t12 * (-t1584 + t962 * t1580) + 0.90D2 * t5 * t6 * (-t
     #967 * t1584 / 0.2D1 - t1621 + t971 * t1580 / 0.6D1 + t962 * t1579)
     # - t81 * t1629 - 0.180D3 * t11 * t12 * (t962 * t1584 - t1579 - t96
     #7 * t1580 / 0.2D1)) * t260 / 0.1440D4 + (0.90D2 * t5 * t6 * ((t159
     #7 - t996 * t1580) * t823 * t931 + t1003 * t1580 - t1584) - 0.180D3
     # * t11 * t1609) * t102 * t321 / 0.720D3 + (0.90D2 * t5 * t6 * (-t1
     #579 - t1018 * t1580 / 0.2D1 + t1017 * t1584) - 0.180D3 * t11 * t12
     # * (t1017 * t1580 - t1584) - t22 * t1629) * t159 * t260 / 0.720D3
      t1674 = FJET(XB1, XB2, s, -t828, 0.0D0, 0.0D0, -t889, t890, t1673)
      t1676 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t830, t826, -t
     #828, 0.0D0, -t835)
      t1677 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t830, t826, -t
     #828, 0.0D0, -t835)
      t1683 = t12 * t1677
      t1690 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t830, t826, -t
     #828, 0.0D0, -t835)
      t1707 = (0.90D2 * t5 * t6 * (t1676 - t845 * t1677) - 0.180D3 * t11
     # * t1683) * t102 * t321 / 0.720D3 + (0.90D2 * t5 * t6 * (-t865 * t
     #1676 + t1690 + t866 * t1677 / 0.2D1) - 0.180D3 * t11 * t12 * (-t86
     #5 * t1677 + t1676) + t22 * t1683) * t159 * t260 / 0.720D3
      t1708 = FJET(XB1, XB2, s, -t828, t830, 0.0D0, t826, -t835, t1707)
      t1710 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, 
     #-t828, 0.0D0, t890)
      t1711 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, 
     #-t828, 0.0D0, t890)
      t1715 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, 
     #-t828, 0.0D0, t890)
      t1727 = z * t1715
      t1740 = t12 * (z * t1711 * t949 - t1711)
      t1752 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t889, 
     #-t828, 0.0D0, t890)
      t1760 = t12 * t1711
      t1804 = (0.90D2 * t5 * t6 * (-t1710 - t895 * t1711 / 0.2D1 + (z * 
     #t1710 - t907 * t1715 + t911 * t1711 / 0.2D1) * t823 * t931 + t894 
     #* t1715) - 0.180D3 * t11 * t12 * ((t1727 - t907 * t1711) * t823 * 
     #t931 - t1715 + t894 * t1711) + t22 * t1740) * t102 * t260 / 0.1440
     #D4 + (t22 * t12 * (-t1715 + t962 * t1711) + 0.90D2 * t5 * t6 * (-t
     #967 * t1715 / 0.2D1 - t1752 + t971 * t1711 / 0.6D1 + t962 * t1710)
     # - t81 * t1760 - 0.180D3 * t11 * t12 * (t962 * t1715 - t1710 - t96
     #7 * t1711 / 0.2D1)) * t260 / 0.1440D4 + (0.90D2 * t5 * t6 * ((t172
     #7 - t996 * t1711) * t823 * t931 + t1003 * t1711 - t1715) - 0.180D3
     # * t11 * t1740) * t102 * t321 / 0.720D3 + (0.90D2 * t5 * t6 * (-t1
     #018 * t1711 / 0.2D1 + t1017 * t1715 - t1710) - 0.180D3 * t11 * t12
     # * (t1017 * t1711 - t1715) - t22 * t1760) * t159 * t260 / 0.720D3
      t1805 = FJET(XB1, XB2, s, -t889, 0.0D0, 0.0D0, -t828, t890, t1804)
      t1807 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t753, t751, 0.0D0)
      t1808 = z * t1807
      t1809 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t753, t751, 0.0D0)
      t1817 = t153 * t1809 * t780
      t1824 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t753, t751, 0.0D0)
      t1845 = (-0.90D2 * t5 * t6 * (t1808 - t804 * t1809) * t780 + 0.180
     #D3 * t316 * t1817) * t102 * t321 / 0.720D3 - (0.90D2 * t5 * t6 * (
     #z * t1824 - t764 * t1807 + t768 * t1809 / 0.2D1) * t780 - 0.180D3 
     #* t316 * t6 * (t1808 - t764 * t1809) * t780 + t152 * t1817) * t102
     # * t159 / 0.1440D4
      t1846 = FJET(XB1, XB2, s, -t753, 0.0D0, t751, 0.0D0, 0.0D0, t1845)
      t1848 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t1484, -t1494,
     # t1474, -t1485, -t835)
      t1850 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t1484, -t1494,
     # t1474, -t1485, -t835)
      t1862 = 0.90D2 * t5 * t6 * (-t1498 * t1848 + t1506 * t1497 * t1850
     #) * t1538 + 0.180D3 * t1542 * t1498 * t1850 * t1538
      t1866 = FJET(XB1, XB2, s, -t1494, -t1485, t1484, t1474, -t835, t18
     #62 * t102 * t321 / 0.720D3)
      t1870 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t1484, -t1494,
     # t1474, -t1485, -t835)
      t1872 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t1484, -t1494,
     # t1474, -t1485, -t835)
      t1884 = 0.90D2 * t5 * t6 * (-t1498 * t1870 + t1506 * t1497 * t1872
     #) * t1538 + 0.180D3 * t1542 * t1498 * t1872 * t1538
      t1888 = FJET(XB1, XB2, s, -t1485, -t1494, t1474, t1484, -t835, t18
     #84 * t102 * t321 / 0.720D3)
      rrgq2qght8s3e1 = t375 * t374 + t560 * t559 + t745 * t744 + t818 * 
     #t817 + t886 * t885 + t1037 * t1036 + t1134 * t1133 + t1175 * t1174
     # + t1360 * t1359 + t1394 * t1393 + t1435 * t1434 + t1469 * t1468 +
     # t1551 * t1547 * t1554 / 0.720D3 + t1575 * t1571 * t1554 / 0.720D3
     # + t1674 * t1673 + t1708 * t1707 + t1805 * t1804 + t1846 * t1845 +
     # t1866 * t1862 * t1554 / 0.720D3 + t1888 * t1884 * t1554 / 0.720D3

      end function



      doubleprecision function rrgq2qght8s3e0
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t8 = x1 ** 2
      t9 = x3 * t8
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t22 = log(0.4D1 * t9 * t19)
      t23 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t25 = z * t7
      t26 = t9 * t15
      t27 = t12 * t18
      t28 = -0.1D1 + x3
      t29 = 0.1D1 / t28
      t33 = log(-0.4D1 * t26 * t27 * t29)
      t34 = t33 * z
      t37 = cos(t13)
      t38 = x3 * z
      t40 = Sqrt(-t38 * t28)
      t44 = 0.1D1 / (-z - x3 + 0.2D1 * t37 * t40)
      t50 = lh * t4
      t51 = pi * t6
      t59 = 0.1D1 / x3
      t61 = 0.1D1 / x1
      t64 = t6 * z
      t65 = t5 * t64
      t66 = t23 * t44
      t67 = 0.1D1 / x2
      t69 = t59 * t67 * t61
      t73 = t5 * t6
      t74 = x2 ** 2
      t75 = t74 * t8
      t76 = t75 * t15
      t77 = -0.1D1 + x2
      t81 = log(-0.4D1 * t76 * t27 * t77)
      t85 = log(0.4D1 * t75 * t19)
      t92 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t93 = t8 * t15
      t96 = log(0.4D1 * t93 * t27)
      t97 = t96 ** 2
      t110 = lh ** 2
      t111 = 0.180D3 * t110
      t112 = pi ** 2
      t113 = 0.30D2 * t112
      t114 = t111 - t113
      t115 = t114 * t4
      t116 = t51 * t23
      t117 = t115 * t116
      t121 = t6 * t23
      t122 = x3 * t12
      t123 = t15 * t18
      t126 = log(0.4D1 * t122 * t123)
      t127 = t126 ** 2
      t128 = t123 * t29
      t131 = log(-0.4D1 * t122 * t128)
      t132 = t131 ** 2
      t136 = -t127 / 0.2D1 - t132 * z * t44 / 0.2D1
      t145 = 0.90D2 * t5 * t6 * t7 - 0.180D3 * t50 * t116
      t148 = t126 + t131 * z * t44
      t158 = -0.1D1 - z * t44
      t164 = log(0.4D1 * t19)
      t167 = t164 ** 2
      t170 = (0.180D3 * t164 * lh + 0.45D2 * t167 + t111 - t113) * t6
      t174 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t189 = (-0.90D2 * t167 * lh + 0.60D2 * lh * t112 - 0.240D3 * zeta3
     # - 0.120D3 * t110 * lh - 0.15D2 * t167 * t164 - t164 * t114) * t6
      t196 = (-0.180D3 * lh - 0.90D2 * t164) * t6
      t200 = x3 * t74
      t203 = log(0.4D1 * t200 * t19)
      t205 = t200 * t12
      t208 = log(-0.4D1 * t205 * t128)
      t209 = t208 * z
      t213 = t123 * t77
      t216 = log(-0.4D1 * t205 * t213)
      t222 = t50 * pi
      t230 = t12 * t74
      t233 = log(0.4D1 * t230 * t123)
      t234 = t233 ** 2
      t237 = log(-0.4D1 * t230 * t213)
      t238 = t237 ** 2
      t240 = t234 / 0.2D1 - t238 / 0.2D1
      t244 = -t233 + t237
      t249 = (0.90D2 * t5 * t6 * (t7 - t22 * t23 + (t25 - t34 * t23) * t
     #44) - 0.180D3 * t50 * t51 * (z * t23 * t44 + t23)) * t59 * t61 / 0
     #.1440D4 + t65 * t66 * t69 / 0.8D1 + t73 * (t81 * t23 - t85 * t23) 
     #* t67 * t61 / 0.8D1 + (0.90D2 * t5 * t6 * (t92 + t97 * t23 / 0.2D1
     # - t96 * t7) - 0.180D3 * t50 * t51 * (t7 - t96 * t23) + t117) * t6
     #1 / 0.1440D4 - (0.90D2 * t5 * t121 * t136 + t145 * t148 + (0.90D2 
     #* t5 * t6 * t92 - 0.180D3 * t50 * t51 * t7 + t117) * t158) * t59 /
     # 0.2880D4 + t170 * t5 * t7 / 0.2880D4 + t5 * t6 * t174 / 0.32D2 + 
     #t189 * t5 * t23 / 0.2880D4 + t196 * t5 * t92 / 0.2880D4 - (0.90D2 
     #* t5 * t6 * (t203 * t23 - (t25 - t209 * t23) * t44 - t216 * t23) +
     # 0.180D3 * t222 * t64 * t66) * t59 * t67 / 0.1440D4 + (0.90D2 * t5
     # * t121 * t240 + t145 * t244) * t67 / 0.1440D4
      t250 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t249)
      t252 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t253 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t255 = z * t252
      t273 = t253 * t44
      t284 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t297 = t51 * t253
      t298 = t115 * t297
      t302 = t6 * t253
      t311 = 0.90D2 * t5 * t6 * t252 - 0.180D3 * t50 * t297
      t327 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t360 = (0.90D2 * t5 * t6 * (t252 - t22 * t253 + (t255 - t34 * t253
     #) * t44) - 0.180D3 * t50 * t51 * (t253 + z * t253 * t44)) * t59 * 
     #t61 / 0.1440D4 + t65 * t273 * t69 / 0.8D1 + t73 * (t81 * t253 - t8
     #5 * t253) * t67 * t61 / 0.8D1 + (0.90D2 * t5 * t6 * (t284 - t96 * 
     #t252 + t97 * t253 / 0.2D1) - 0.180D3 * t50 * t51 * (t252 - t96 * t
     #253) + t298) * t61 / 0.1440D4 - (0.90D2 * t5 * t302 * t136 + t311 
     #* t148 + (0.90D2 * t5 * t6 * t284 - 0.180D3 * t50 * t51 * t252 + t
     #298) * t158) * t59 / 0.2880D4 + t170 * t5 * t252 / 0.2880D4 + t5 *
     # t6 * t327 / 0.32D2 + t189 * t5 * t253 / 0.2880D4 + t196 * t5 * t2
     #84 / 0.2880D4 - (0.90D2 * t5 * t6 * (-(t255 - t209 * t253) * t44 -
     # t216 * t253 + t203 * t253) + 0.180D3 * t222 * t64 * t273) * t59 *
     # t67 / 0.1440D4 + (0.90D2 * t5 * t302 * t240 + t311 * t244) * t67 
     #/ 0.1440D4
      t361 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t360)
      t363 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t364 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t366 = z * t363
      t384 = t364 * t44
      t398 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t408 = t51 * t364
      t409 = t115 * t408
      t413 = t6 * t364
      t422 = 0.90D2 * t5 * t6 * t363 - 0.180D3 * t50 * t408
      t438 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t471 = (0.90D2 * t5 * t6 * (t363 - t22 * t364 + (t366 - t34 * t364
     #) * t44) - 0.180D3 * t50 * t51 * (z * t364 * t44 + t364)) * t59 * 
     #t61 / 0.1440D4 + t65 * t384 * t69 / 0.8D1 + t73 * (-t85 * t364 + t
     #81 * t364) * t67 * t61 / 0.8D1 + (0.90D2 * t5 * t6 * (t97 * t364 /
     # 0.2D1 - t96 * t363 + t398) - 0.180D3 * t50 * t51 * (t363 - t96 * 
     #t364) + t409) * t61 / 0.1440D4 - (0.90D2 * t5 * t413 * t136 + t422
     # * t148 + (0.90D2 * t5 * t6 * t398 - 0.180D3 * t50 * t51 * t363 + 
     #t409) * t158) * t59 / 0.2880D4 + t170 * t5 * t363 / 0.2880D4 + t5 
     #* t6 * t438 / 0.32D2 + t189 * t5 * t364 / 0.2880D4 + t196 * t5 * t
     #398 / 0.2880D4 - (0.90D2 * t5 * t6 * (-(t366 - t209 * t364) * t44 
     #- t216 * t364 + t203 * t364) + 0.180D3 * t222 * t64 * t384) * t59 
     #* t67 / 0.1440D4 + (0.90D2 * t5 * t413 * t240 + t422 * t244) * t67
     # / 0.1440D4
      t472 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t471)
      t474 = x2 * x3
      t475 = 0.1D1 - x3 + t474
      t476 = 0.1D1 / t475
      t477 = t474 * t476
      t478 = t2 * t477
      t480 = t2 * t28 * t476
      t481 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t480, t478, 0.0D0)
      t482 = t77 * t28
      t484 = Sqrt(t38 * t482)
      t488 = 0.1D1 / (-z - x3 + t474 + 0.2D1 * t37 * t484)
      t489 = t481 * t488
      t493 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t480, t478, 0.0D0)
      t497 = t475 ** 2
      t503 = log(0.4D1 * t200 * t16 * t18 * t77 * t28 / t497)
      t504 = t503 * z
      t518 = -t65 * t489 * t69 / 0.8D1 - (0.90D2 * t5 * t6 * (z * t493 -
     # t504 * t481) * t488 - 0.180D3 * t222 * t64 * t489) * t59 * t67 / 
     #0.1440D4
      t519 = FJET(XB1, XB2, s, 0.0D0, t478, 0.0D0, -t480, 0.0D0, t518)
      t522 = t1 * x1
      t523 = x1 * z
      t524 = -z - x1 + t523
      t525 = 0.1D1 / t524
      t527 = t77 * s * t522 * t525
      t528 = -0.1D1 + x1
      t529 = t2 * t528
      t531 = x2 * s * t522
      t532 = s * t17
      t535 = x1 * t528 * t525
      t536 = t532 * t77 * t535
      t537 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t531, t527, -t5
     #29, 0.0D0, -t536)
      t539 = t67 * t61
      t543 = 0.1D1 / t10
      t544 = t15 * t543
      t546 = t18 * t525
      t547 = t528 ** 2
      t552 = log(0.4D1 * t75 * t544 * t546 * t547 * t77)
      t554 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t531, t527, -t5
     #29, 0.0D0, -t536)
      t566 = t73 * t537 * t59 * t539 / 0.8D1 + (0.90D2 * t5 * t6 * (-t55
     #2 * t537 + t554) - 0.180D3 * t50 * t51 * t537) * t67 * t61 / 0.720
     #D3
      t567 = FJET(XB1, XB2, s, 0.0D0, t527, -t529, t531, -t536, t566)
      t570 = t2 * x1 * t525
      t571 = t532 * t535
      t572 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t570, -
     #t529, 0.0D0, t571)
      t575 = t543 * t18 * t525 * t547
      t578 = log(-0.4D1 * t26 * t575)
      t579 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t570, -
     #t529, 0.0D0, t571)
      t587 = log(0.4D1 * t9 * t544 * t546 * t547 * t29)
      t588 = t587 * z
      t592 = x3 * x1
      t593 = t592 * z
      t595 = 0.2D1 * t9 * z
      t596 = x1 * t10
      t597 = x3 * t10
      t598 = t597 * x1
      t599 = t9 * t10
      t600 = x3 * t524
      t602 = Sqrt(t600 * t28)
      t607 = 0.1D1 / (-t523 - t593 + t595 + t596 + t598 - t599 - t9 - t3
     #8 - t10 + 0.2D1 * t37 * t602 * z)
      t614 = t524 * t607
      t616 = -t579 + z * t579 * t614
      t630 = log(-0.4D1 * t76 * t575)
      t636 = t51 * t579
      t647 = log(-0.4D1 * t93 * t543 * t546 * t547)
      t649 = t647 ** 2
      t652 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t570, -
     #t529, 0.0D0, t571)
      t666 = (0.90D2 * t5 * t6 * (-t572 + t578 * t579 + (z * t572 - t588
     # * t579) * t524 * t607) - 0.180D3 * t50 * t51 * t616) * t59 * t61 
     #/ 0.1440D4 + t73 * t616 * t59 * t539 / 0.8D1 + (0.90D2 * t5 * t6 *
     # (-t572 + t630 * t579) + 0.180D3 * t50 * t636) * t67 * t61 / 0.720
     #D3 + (0.90D2 * t5 * t6 * (t647 * t572 - t649 * t579 / 0.2D1 - t652
     #) - 0.180D3 * t50 * t51 * (t647 * t579 - t572) - t115 * t636) * t6
     #1 / 0.1440D4
      t667 = FJET(XB1, XB2, s, 0.0D0, -t529, -t570, 0.0D0, t571, t666)
      t669 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t570, -
     #t529, 0.0D0, t571)
      t671 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t570, -
     #t529, 0.0D0, t571)
      t683 = -t671 + z * t671 * t614
      t700 = t51 * t671
      t710 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t570, -
     #t529, 0.0D0, t571)
      t724 = (0.90D2 * t5 * t6 * (-t669 + (z * t669 - t588 * t671) * t52
     #4 * t607 + t578 * t671) - 0.180D3 * t50 * t51 * t683) * t59 * t61 
     #/ 0.1440D4 + t73 * t683 * t59 * t539 / 0.8D1 + (0.90D2 * t5 * t6 *
     # (t630 * t671 - t669) + 0.180D3 * t50 * t700) * t67 * t61 / 0.720D
     #3 + (0.90D2 * t5 * t6 * (t647 * t669 - t649 * t671 / 0.2D1 - t710)
     # - 0.180D3 * t50 * t51 * (-t669 + t647 * t671) - t115 * t700) * t6
     #1 / 0.1440D4
      t725 = FJET(XB1, XB2, s, 0.0D0, -t570, -t529, 0.0D0, t571, t724)
      t727 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t480, t478, 0.0D0)
      t728 = t727 * t488
      t732 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t480, t478, 0.0D0)
      t747 = -t65 * t728 * t69 / 0.8D1 - (0.90D2 * t5 * t6 * (z * t732 -
     # t504 * t727) * t488 - 0.180D3 * t222 * t64 * t728) * t59 * t67 / 
     #0.1440D4
      t748 = FJET(XB1, XB2, s, 0.0D0, -t480, 0.0D0, t478, 0.0D0, t747)
      t750 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t751 = z * t750
      t752 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t771 = t752 * t44
      t782 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t795 = t51 * t752
      t796 = t115 * t795
      t806 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t813 = t6 * t752
      t822 = 0.90D2 * t5 * t6 * t750 - 0.180D3 * t50 * t795
      t858 = (0.90D2 * t5 * t6 * (t750 + (t751 - t34 * t752) * t44 - t22
     # * t752) - 0.180D3 * t50 * t51 * (t752 + z * t752 * t44)) * t59 * 
     #t61 / 0.1440D4 + t65 * t771 * t69 / 0.8D1 + t73 * (-t85 * t752 + t
     #81 * t752) * t67 * t61 / 0.8D1 + (0.90D2 * t5 * t6 * (t782 - t96 *
     # t750 + t97 * t752 / 0.2D1) - 0.180D3 * t50 * t51 * (-t96 * t752 +
     # t750) + t796) * t61 / 0.1440D4 + t189 * t5 * t752 / 0.2880D4 + t1
     #70 * t5 * t750 / 0.2880D4 + t5 * t6 * t806 / 0.32D2 + t196 * t5 * 
     #t782 / 0.2880D4 - (0.90D2 * t5 * t813 * t136 + t822 * t148 + (0.90
     #D2 * t5 * t6 * t782 - 0.180D3 * t50 * t51 * t750 + t796) * t158) *
     # t59 / 0.2880D4 - (0.90D2 * t5 * t6 * (-(t751 - t209 * t752) * t44
     # - t216 * t752 + t203 * t752) + 0.180D3 * t222 * t64 * t771) * t59
     # * t67 / 0.1440D4 + (0.90D2 * t5 * t813 * t240 + t822 * t244) * t6
     #7 / 0.1440D4
      t859 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t858)
      t861 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t531, t527, -t5
     #29, 0.0D0, -t536)
      t866 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t531, t527, -t5
     #29, 0.0D0, -t536)
      t879 = t73 * t861 * t59 * t539 / 0.8D1 + (0.90D2 * t5 * t6 * (t866
     # - t552 * t861) - 0.180D3 * t50 * t51 * t861) * t67 * t61 / 0.720D
     #3
      t880 = FJET(XB1, XB2, s, t531, -t529, t527, 0.0D0, -t536, t879)
      t882 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t480, t478, 0.0D0)
      t883 = t882 * t488
      t887 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t480, t478, 0.0D0)
      t902 = -t65 * t883 * t69 / 0.8D1 - (0.90D2 * t5 * t6 * (z * t887 -
     # t504 * t882) * t488 - 0.180D3 * t222 * t64 * t883) * t59 * t67 / 
     #0.1440D4
      t903 = FJET(XB1, XB2, s, t478, 0.0D0, -t480, 0.0D0, 0.0D0, t902)
      t905 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t531, t527, -t5
     #29, 0.0D0, -t536)
      t911 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t531, t527, -t5
     #29, 0.0D0, -t536)
      t923 = t73 * t905 * t59 * t539 / 0.8D1 + (0.90D2 * t5 * t6 * (-t55
     #2 * t905 + t911) - 0.180D3 * t50 * t51 * t905) * t67 * t61 / 0.720
     #D3
      t924 = FJET(XB1, XB2, s, t527, 0.0D0, t531, -t529, -t536, t923)
      t929 = t28 * s * t1 * t528 * t476
      t930 = t2 * x1
      t932 = Sqrt(-t600 * t482)
      t933 = t37 * t932
      t939 = t930 * x2 * (-x3 + t474 - z + t38 - x1 + t592 + t523 - t593
     # + 0.2D1 * t933) * t525 * t476
      t940 = t529 * t477
      t941 = t200 * t523
      t944 = t200 * x1
      t949 = t930 * (-t941 + 0.2D1 * t933 * x2 + 0.1D1 - x3 - x2 + t474 
     #+ t944 + t200 * z) * t525 * t476
      t951 = x2 * x1
      t952 = t951 * z
      t953 = -z - t951 + t952
      t955 = t5 * t6 * t524 * t953
      t956 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t939, -t949, t9
     #29, -t940, -t536)
      t963 = t10 * x2
      t967 = t593 - t595 - t598 + t599 + t941 - 0.2D1 * t474 * t523 + t5
     #97 * t951 + 0.2D1 * t9 * x2 * z - t9 * t963 - 0.2D1 * t933 * t951 
     #- t596 + t9 + t38
      t970 = t8 * x2
      t981 = t10 + 0.2D1 * t933 * t952 + t970 + t523 - t944 + t952 - 0.2
     #D1 * t933 * z + t8 * t10 * x2 - t963 * x1 - 0.2D1 * t970 * z - t47
     #4 * z + t474 * x1 - t9 * x2
      t983 = 0.1D1 / (t967 + t981)
      t988 = FJET(XB1, XB2, s, t929, t939, -t940, -t949, -t536, -t955 * 
     #t956 * t983 * t69 / 0.8D1)
      t990 = t51 * t524
      t997 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t939, -t949, t9
     #29, -t940, -t536)
      t1002 = FJET(XB1, XB2, s, t939, t929, -t949, -t940, -t536, -t955 *
     # t997 * t983 * t69 / 0.8D1)
      t1010 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t570, 
     #-t529, 0.0D0, t571)
      t1012 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t570, 
     #-t529, 0.0D0, t571)
      t1024 = z * t1010 * t614 - t1010
      t1041 = t51 * t1010
      t1049 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t570, 
     #-t529, 0.0D0, t571)
      t1065 = (0.90D2 * t5 * t6 * (t578 * t1010 - t1012 + (z * t1012 - t
     #588 * t1010) * t524 * t607) - 0.180D3 * t50 * t51 * t1024) * t59 *
     # t61 / 0.1440D4 + t73 * t1024 * t59 * t539 / 0.8D1 + (0.90D2 * t5 
     #* t6 * (t630 * t1010 - t1012) + 0.180D3 * t50 * t1041) * t67 * t61
     # / 0.720D3 + (0.90D2 * t5 * t6 * (t647 * t1012 - t1049 - t649 * t1
     #010 / 0.2D1) - 0.180D3 * t50 * t51 * (-t1012 + t647 * t1010) - t11
     #5 * t1041) * t61 / 0.1440D4
      t1066 = FJET(XB1, XB2, s, -t529, 0.0D0, 0.0D0, -t570, t571, t1065)
      t1068 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t531, t527, -t
     #529, 0.0D0, -t536)
      t1074 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t531, t527, -t
     #529, 0.0D0, -t536)
      t1086 = t73 * t1068 * t59 * t539 / 0.8D1 + (0.90D2 * t5 * t6 * (-t
     #552 * t1068 + t1074) - 0.180D3 * t50 * t51 * t1068) * t67 * t61 / 
     #0.720D3
      t1087 = FJET(XB1, XB2, s, -t529, t531, 0.0D0, t527, -t536, t1086)
      t1089 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t570, 
     #-t529, 0.0D0, t571)
      t1091 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t570, 
     #-t529, 0.0D0, t571)
      t1103 = z * t1091 * t614 - t1091
      t1120 = t51 * t1091
      t1128 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t570, 
     #-t529, 0.0D0, t571)
      t1144 = (0.90D2 * t5 * t6 * ((z * t1089 - t588 * t1091) * t524 * t
     #607 - t1089 + t578 * t1091) - 0.180D3 * t50 * t51 * t1103) * t59 *
     # t61 / 0.1440D4 + t73 * t1103 * t59 * t539 / 0.8D1 + (0.90D2 * t5 
     #* t6 * (t630 * t1091 - t1089) + 0.180D3 * t50 * t1120) * t67 * t61
     # / 0.720D3 + (0.90D2 * t5 * t6 * (t647 * t1089 - t1128 - t649 * t1
     #091 / 0.2D1) - 0.180D3 * t50 * t51 * (-t1089 + t647 * t1091) - t11
     #5 * t1120) * t61 / 0.1440D4
      t1145 = FJET(XB1, XB2, s, -t570, 0.0D0, 0.0D0, -t529, t571, t1144)
      t1147 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t480, t478, 0.0D0)
      t1148 = t1147 * t488
      t1152 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t480, t478, 0.0D0)
      t1167 = -t65 * t1148 * t69 / 0.8D1 - (0.90D2 * t5 * t6 * (z * t115
     #2 - t504 * t1147) * t488 - 0.180D3 * t222 * t64 * t1148) * t59 * t
     #67 / 0.1440D4
      t1168 = FJET(XB1, XB2, s, -t480, 0.0D0, t478, 0.0D0, 0.0D0, t1167)
      t1170 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t939, -t949, t
     #929, -t940, -t536)
      t1175 = FJET(XB1, XB2, s, -t949, -t940, t939, t929, -t536, -t955 *
     # t1170 * t983 * t69 / 0.8D1)
      t1183 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t939, -t949, t
     #929, -t940, -t536)
      t1188 = FJET(XB1, XB2, s, -t940, -t949, t929, t939, -t536, -t955 *
     # t1183 * t983 * t69 / 0.8D1)
      rrgq2qght8s3e0 = t250 * t249 + t361 * t360 + t472 * t471 + t519 * 
     #t518 + t567 * t566 + t667 * t666 + t725 * t724 + t748 * t747 + t85
     #9 * t858 + t880 * t879 + t903 * t902 + t924 * t923 - t988 * t4 * t
     #990 * t953 * t956 * t983 * t69 / 0.8D1 - t1002 * t4 * t990 * t953 
     #* t997 * t983 * t69 / 0.8D1 + t1066 * t1065 + t1087 * t1086 + t114
     #5 * t1144 + t1168 * t1167 - t1175 * t4 * t990 * t953 * t1170 * t98
     #3 * t69 / 0.8D1 - t1188 * t4 * t990 * t953 * t1183 * t983 * t69 / 
     #0.8D1

      end function



      doubleprecision function rrgq2qght8s3em1
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t9 = z ** 2
      t11 = 0.1D1 / t9 / z
      t12 = x3 * t11
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t21 = log(0.4D1 * t12 * t18)
      t22 = -0.1D1 + x3
      t27 = log(-0.4D1 * t12 * t18 / t22)
      t29 = cos(t13)
      t30 = x3 * z
      t32 = Sqrt(-t30 * t22)
      t36 = 0.1D1 / (-z - x3 + 0.2D1 * t29 * t32)
      t38 = t21 + t27 * z * t36
      t42 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t46 = lh * t4
      t47 = pi * t6
      t50 = 0.180D3 * t46 * t47 * t7
      t53 = -0.1D1 - z * t36
      t56 = 0.1D1 / x3
      t59 = x1 ** 2
      t60 = t59 * t15
      t64 = log(0.4D1 * t60 * t11 * t17)
      t71 = 0.1D1 / x1
      t74 = t5 * t6
      t83 = t5 * t6 * z
      t85 = 0.1D1 / x2
      t86 = t56 * t85
      t90 = x2 ** 2
      t91 = t11 * t90
      t94 = log(0.4D1 * t91 * t18)
      t95 = -0.1D1 + x2
      t99 = log(-0.4D1 * t91 * t18 * t95)
      t100 = -t94 + t99
      t105 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t113 = log(0.4D1 * t11 * t15 * t17)
      t116 = (-0.180D3 * lh - 0.90D2 * t113) * t6
      t122 = t113 ** 2
      t124 = lh ** 2
      t126 = pi ** 2
      t129 = (0.180D3 * t113 * lh + 0.45D2 * t122 + 0.180D3 * t124 - 0.3
     #0D2 * t126) * t6
      t133 = -(0.90D2 * t5 * t6 * t7 * t38 + (0.90D2 * t5 * t6 * t42 - t
     #50) * t53) * t56 / 0.2880D4 + (0.90D2 * t5 * t6 * (t42 - t64 * t7)
     # - t50) * t71 / 0.1440D4 + t74 * (z * t7 * t36 + t7) * t56 * t71 /
     # 0.16D2 + t83 * t7 * t36 * t86 / 0.16D2 + t74 * t7 * t100 * t85 / 
     #0.16D2 + t5 * t6 * t105 / 0.32D2 + t116 * t5 * t42 / 0.2880D4 + t1
     #29 * t5 * t7 / 0.2880D4
      t134 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t133)
      t136 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t141 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t147 = 0.180D3 * t46 * t47 * t136
      t176 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t186 = -(0.90D2 * t5 * t6 * t136 * t38 + (0.90D2 * t5 * t6 * t141 
     #- t147) * t53) * t56 / 0.2880D4 + (0.90D2 * t5 * t6 * (t141 - t64 
     #* t136) - t147) * t71 / 0.1440D4 + t74 * (t136 + z * t136 * t36) *
     # t56 * t71 / 0.16D2 + t83 * t136 * t36 * t86 / 0.16D2 + t74 * t136
     # * t100 * t85 / 0.16D2 + t5 * t6 * t176 / 0.32D2 + t116 * t5 * t14
     #1 / 0.2880D4 + t129 * t5 * t136 / 0.2880D4
      t187 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t186)
      t189 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t194 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t200 = 0.180D3 * t46 * t47 * t189
      t229 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t239 = -(0.90D2 * t5 * t6 * t189 * t38 + (0.90D2 * t5 * t6 * t194 
     #- t200) * t53) * t56 / 0.2880D4 + (0.90D2 * t5 * t6 * (t194 - t64 
     #* t189) - t200) * t71 / 0.1440D4 + t74 * (z * t189 * t36 + t189) *
     # t56 * t71 / 0.16D2 + t83 * t189 * t36 * t86 / 0.16D2 + t74 * t189
     # * t100 * t85 / 0.16D2 + t5 * t6 * t229 / 0.32D2 + t116 * t5 * t19
     #4 / 0.2880D4 + t129 * t5 * t189 / 0.2880D4
      t240 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t239)
      t242 = x2 * x3
      t244 = 0.1D1 / (0.1D1 - x3 + t242)
      t246 = t2 * t242 * t244
      t248 = t2 * t22 * t244
      t249 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t248, t246, 0.0D0)
      t252 = Sqrt(t30 * t95 * t22)
      t256 = 0.1D1 / (-z - x3 + t242 + 0.2D1 * t29 * t252)
      t261 = FJET(XB1, XB2, s, 0.0D0, t246, 0.0D0, -t248, 0.0D0, -t83 * 
     #t249 * t256 * t86 / 0.16D2)
      t266 = t256 * t56 * t85
      t271 = t1 * x1
      t272 = x1 * z
      t273 = -z - x1 + t272
      t274 = 0.1D1 / t273
      t276 = t95 * s * t271 * t274
      t277 = -0.1D1 + x1
      t278 = t2 * t277
      t280 = x2 * s * t271
      t281 = s * t16
      t284 = x1 * t277 * t274
      t285 = t281 * t95 * t284
      t286 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t280, t276, -t2
     #78, 0.0D0, -t285)
      t291 = FJET(XB1, XB2, s, 0.0D0, t276, -t278, t280, -t285, t74 * t2
     #86 * t85 * t71 / 0.8D1)
      t295 = t85 * t71
      t300 = t2 * x1 * t274
      t301 = t281 * t284
      t302 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t300, -
     #t278, 0.0D0, t301)
      t310 = t277 ** 2
      t314 = log(-0.4D1 * t60 / t9 * t17 * t274 * t310)
      t316 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t300, -
     #t278, 0.0D0, t301)
      t330 = x3 * t59
      t339 = Sqrt(x3 * t273 * t22)
      t345 = t273 / (-t272 - x3 * x1 * z + 0.2D1 * t330 * z + x1 * t9 + 
     #x3 * t9 * x1 - t330 * t9 - t330 - t30 - t9 + 0.2D1 * t29 * t339 * 
     #z)
      t352 = -t74 * t302 * t85 * t71 / 0.8D1 + (0.90D2 * t5 * t6 * (t314
     # * t302 - t316) + 0.180D3 * t46 * t47 * t302) * t71 / 0.1440D4 + t
     #74 * (-t302 + z * t302 * t345) * t56 * t71 / 0.16D2
      t353 = FJET(XB1, XB2, s, 0.0D0, -t278, -t300, 0.0D0, t301, t352)
      t355 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t300, -
     #t278, 0.0D0, t301)
      t360 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t300, -
     #t278, 0.0D0, t301)
      t379 = -t74 * t355 * t85 * t71 / 0.8D1 + (0.90D2 * t5 * t6 * (-t36
     #0 + t314 * t355) + 0.180D3 * t46 * t47 * t355) * t71 / 0.1440D4 + 
     #t74 * (-t355 + z * t355 * t345) * t56 * t71 / 0.16D2
      t380 = FJET(XB1, XB2, s, 0.0D0, -t300, -t278, 0.0D0, t301, t379)
      t382 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t248, t246, 0.0D0)
      t387 = FJET(XB1, XB2, s, 0.0D0, -t248, 0.0D0, t246, 0.0D0, -t83 * 
     #t382 * t256 * t86 / 0.16D2)
      t394 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t399 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t405 = 0.180D3 * t46 * t47 * t394
      t411 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t444 = -(0.90D2 * t5 * t6 * t394 * t38 + (0.90D2 * t5 * t6 * t399 
     #- t405) * t53) * t56 / 0.2880D4 + t5 * t6 * t411 / 0.32D2 + t116 *
     # t5 * t399 / 0.2880D4 + t129 * t5 * t394 / 0.2880D4 + (0.90D2 * t5
     # * t6 * (-t64 * t394 + t399) - t405) * t71 / 0.1440D4 + t74 * (t39
     #4 + z * t394 * t36) * t56 * t71 / 0.16D2 + t83 * t394 * t36 * t86 
     #/ 0.16D2 + t74 * t394 * t100 * t85 / 0.16D2
      t445 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t444)
      t447 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t280, t276, -t2
     #78, 0.0D0, -t285)
      t452 = FJET(XB1, XB2, s, t280, -t278, t276, 0.0D0, -t285, t74 * t4
     #47 * t85 * t71 / 0.8D1)
      t459 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t248, t246, 0.0D0)
      t464 = FJET(XB1, XB2, s, t246, 0.0D0, -t248, 0.0D0, 0.0D0, -t83 * 
     #t459 * t256 * t86 / 0.16D2)
      t471 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t280, t276, -t2
     #78, 0.0D0, -t285)
      t476 = FJET(XB1, XB2, s, t276, 0.0D0, t280, -t278, -t285, t74 * t4
     #71 * t85 * t71 / 0.8D1)
      t483 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t300, -
     #t278, 0.0D0, t301)
      t488 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t300, -
     #t278, 0.0D0, t301)
      t507 = -t74 * t483 * t85 * t71 / 0.8D1 + (0.90D2 * t5 * t6 * (-t48
     #8 + t314 * t483) + 0.180D3 * t46 * t47 * t483) * t71 / 0.1440D4 + 
     #t74 * (z * t483 * t345 - t483) * t56 * t71 / 0.16D2
      t508 = FJET(XB1, XB2, s, -t278, 0.0D0, 0.0D0, -t300, t301, t507)
      t510 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t280, t276, -t2
     #78, 0.0D0, -t285)
      t515 = FJET(XB1, XB2, s, -t278, t280, 0.0D0, t276, -t285, t74 * t5
     #10 * t85 * t71 / 0.8D1)
      t522 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t300, -
     #t278, 0.0D0, t301)
      t527 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t300, -
     #t278, 0.0D0, t301)
      t546 = -t74 * t522 * t85 * t71 / 0.8D1 + (0.90D2 * t5 * t6 * (-t52
     #7 + t314 * t522) + 0.180D3 * t46 * t47 * t522) * t71 / 0.1440D4 + 
     #t74 * (z * t522 * t345 - t522) * t56 * t71 / 0.16D2
      t547 = FJET(XB1, XB2, s, -t300, 0.0D0, 0.0D0, -t278, t301, t546)
      t549 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t248, t246, 0.0D0)
      t554 = FJET(XB1, XB2, s, -t248, 0.0D0, t246, 0.0D0, 0.0D0, -t83 * 
     #t549 * t256 * t86 / 0.16D2)
      rrgq2qght8s3em1 = t134 * t133 + t187 * t186 + t240 * t239 - t261 *
     # t4 * t47 * z * t249 * t266 / 0.16D2 + t291 * t4 * pi * t6 * t286 
     #* t295 / 0.8D1 + t353 * t352 + t380 * t379 - t387 * t4 * t47 * z *
     # t382 * t266 / 0.16D2 + t445 * t444 + t452 * t4 * pi * t6 * t447 *
     # t295 / 0.8D1 - t464 * t4 * t47 * z * t459 * t266 / 0.16D2 + t476 
     #* t4 * pi * t6 * t471 * t295 / 0.8D1 + t508 * t507 + t515 * t4 * p
     #i * t6 * t510 * t295 / 0.8D1 + t547 * t546 - t554 * t4 * t47 * z *
     # t549 * t266 / 0.16D2

      end function



      doubleprecision function rrgq2qght8s3em2
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t9 = 0.1D1 / x1
      t13 = t5 * t6
      t14 = x4 * pi
      t15 = cos(t14)
      t19 = Sqrt(-x3 * z * (-0.1D1 + x3))
      t25 = -0.1D1 - z / (-z - x3 + 0.2D1 * t15 * t19)
      t27 = 0.1D1 / x3
      t31 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t36 = z ** 2
      t39 = Sin(t14)
      t40 = t39 ** 2
      t42 = t1 ** 2
      t43 = t42 ** 2
      t46 = log(0.4D1 / t36 / z * t40 * t43)
      t49 = (-0.180D3 * lh - 0.90D2 * t46) * t6
      t53 = t5 * t6 * t7 * t9 / 0.16D2 - t13 * t7 * t25 * t27 / 0.32D2 +
     # t5 * t6 * t31 / 0.32D2 + t49 * t5 * t7 / 0.2880D4
      t54 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t53)
      t56 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t65 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t72 = t5 * t6 * t56 * t9 / 0.16D2 - t13 * t56 * t25 * t27 / 0.32D2
     # + t5 * t6 * t65 / 0.32D2 + t49 * t5 * t56 / 0.2880D4
      t73 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t72)
      t75 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t84 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t91 = t5 * t6 * t75 * t9 / 0.16D2 - t13 * t75 * t25 * t27 / 0.32D2
     # + t5 * t6 * t84 / 0.32D2 + t49 * t5 * t75 / 0.2880D4
      t92 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t91)
      t94 = -0.1D1 + x1
      t95 = t2 * t94
      t98 = 0.1D1 / (-z - x1 + x1 * z)
      t100 = t2 * x1 * t98
      t104 = s * t42 * x1 * t94 * t98
      t105 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t100, -
     #t95, 0.0D0, t104)
      t107 = t6 * t105 * t9
      t110 = FJET(XB1, XB2, s, 0.0D0, -t95, -t100, 0.0D0, t104, -t5 * t1
     #07 / 0.16D2)
      t115 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t100, -
     #t95, 0.0D0, t104)
      t117 = t6 * t115 * t9
      t120 = FJET(XB1, XB2, s, 0.0D0, -t100, -t95, 0.0D0, t104, -t5 * t1
     #17 / 0.16D2)
      t125 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t134 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t141 = t5 * t6 * t125 * t9 / 0.16D2 - t13 * t125 * t25 * t27 / 0.3
     #2D2 + t5 * t6 * t134 / 0.32D2 + t49 * t5 * t125 / 0.2880D4
      t142 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t141)
      t144 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t100, -
     #t95, 0.0D0, t104)
      t146 = t6 * t144 * t9
      t149 = FJET(XB1, XB2, s, -t95, 0.0D0, 0.0D0, -t100, t104, -t5 * t1
     #46 / 0.16D2)
      t154 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t100, -
     #t95, 0.0D0, t104)
      t156 = t6 * t154 * t9
      t159 = FJET(XB1, XB2, s, -t100, 0.0D0, 0.0D0, -t95, t104, -t5 * t1
     #56 / 0.16D2)
      rrgq2qght8s3em2 = t54 * t53 + t73 * t72 + t92 * t91 - t110 * t4 * 
     #pi * t107 / 0.16D2 - t120 * t4 * pi * t117 / 0.16D2 + t142 * t141 
     #- t149 * t4 * pi * t146 / 0.16D2 - t159 * t4 * pi * t156 / 0.16D2

      end function



      doubleprecision function rrgq2qght8s3em3
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t5 * t6 * 
     #t7 / 0.32D2)
      t13 = pi * t6
      t16 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t5 * t6 * 
     #t16 / 0.32D2)
      t24 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t5 * t6 * 
     #t24 / 0.32D2)
      t32 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t5 * t6 * 
     #t32 / 0.32D2)
      rrgq2qght8s3em3 = t11 * t4 * t13 * t7 / 0.32D2 + t20 * t4 * t13 * 
     #t16 / 0.32D2 + t28 * t4 * t13 * t24 / 0.32D2 + t36 * t4 * t13 * t3
     #2 / 0.32D2

      end function



      doubleprecision function rrgq2qght8s3em4
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght8s3em4 = 0.0D0

      end function


      doubleprecision function rrgq2qght8s4e1
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = -0.1D1 + x3
      t4 = x2 * x3
      t5 = -0.1D1 + t4
      t6 = 0.1D1 / t5
      t8 = t2 * t3 * t6
      t9 = -0.1D1 + x2
      t11 = x3 * t9 * t6
      t12 = t2 * t11
      t13 = s ** 2
      t14 = 0.1D1 / t13
      t15 = t14 * pi
      t16 = 0.1D1 / t1
      t17 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t8
     #, t12, 0.0D0)
      t19 = z ** 2
      t21 = 0.1D1 / t19 / z
      t22 = t1 ** 2
      t23 = t22 ** 2
      t24 = t21 * t23
      t26 = x4 * pi
      t27 = Sin(t26)
      t28 = t27 ** 2
      t29 = t9 ** 2
      t31 = t5 ** 2
      t32 = 0.1D1 / t31
      t37 = log(-0.4D1 * t4 * t24 * t28 * t29 * t3 * t32)
      t38 = t37 * z
      t39 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t8
     #, t12, 0.0D0)
      t41 = t37 ** 2
      t42 = t41 * z
      t43 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t8
     #, t12, 0.0D0)
      t48 = cos(t26)
      t49 = x3 * z
      t50 = x2 * t3
      t52 = Sqrt(-t49 * t50)
      t56 = 0.1D1 / (-z - t4 + 0.2D1 * t48 * t52)
      t60 = lh * t14
      t61 = t60 * pi
      t62 = z * t39
      t69 = lh ** 2
      t70 = 0.180D3 * t69
      t71 = pi ** 2
      t72 = 0.30D2 * t71
      t73 = t70 - t72
      t74 = t73 * t14
      t75 = t74 * pi
      t76 = t16 * z
      t78 = t76 * t43 * t56
      t81 = 0.1D1 / x3
      t83 = 0.1D1 / x2
      t86 = t3 * t29
      t88 = t28 * t21
      t89 = x1 ** 2
      t95 = log(-0.4D1 * t4 * t86 * t88 * t32 * t89 * t23)
      t96 = t95 * z
      t107 = 0.1D1 / x1
      t108 = t83 * t107
      t111 = -(0.90D2 * t15 * t16 * (z * t17 - t38 * t39 + t42 * t43 / 0
     #.2D1) * t56 - 0.180D3 * t61 * t16 * (t62 - t38 * t43) * t56 + t75 
     #* t78) * t81 * t83 / 0.1440D4 - (0.90D2 * t15 * t16 * (t62 - t96 *
     # t43) * t56 - 0.180D3 * t61 * t78) * t81 * t108 / 0.720D3
      t112 = FJET(XB1, XB2, s, 0.0D0, t8, 0.0D0, t12, 0.0D0, t111)
      t114 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #8, t12, 0.0D0)
      t115 = z * t114
      t116 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #8, t12, 0.0D0)
      t124 = t76 * t116 * t56
      t131 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #8, t12, 0.0D0)
      t152 = -(0.90D2 * t15 * t16 * (t115 - t96 * t116) * t56 - 0.180D3 
     #* t61 * t124) * t81 * t108 / 0.720D3 - (0.90D2 * t15 * t16 * (z * 
     #t131 - t38 * t114 + t42 * t116 / 0.2D1) * t56 - 0.180D3 * t61 * t1
     #6 * (t115 - t38 * t116) * t56 + t75 * t124) * t81 * t83 / 0.1440D4
      t153 = FJET(XB1, XB2, s, t8, 0.0D0, t12, 0.0D0, 0.0D0, t152)
      t155 = t2 * x1
      t156 = -0.1D1 + x1
      t157 = t2 * t156
      t158 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -t
     #157, 0.0D0, 0.0D0)
      t159 = x3 * t89
      t161 = 0.1D1 / t19
      t163 = x1 * z
      t164 = -z - x1 + t163
      t165 = 0.1D1 / t164
      t166 = t156 ** 2
      t167 = t165 * t166
      t171 = log(-0.4D1 * t159 * t28 * t161 * t23 * t167)
      t172 = t171 ** 2
      t173 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -t
     #157, 0.0D0, 0.0D0)
      t176 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -t
     #157, 0.0D0, 0.0D0)
      t182 = pi * t16
      t188 = t182 * t173
      t189 = t74 * t188
      t193 = t89 * t28
      t199 = log(-0.4D1 * t193 * t161 * t23 * t165 * t166)
      t204 = t199 ** 2
      t205 = t204 * t199
      t209 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -t
     #157, 0.0D0, 0.0D0)
      t217 = 0.60D2 * lh * t71
      t218 = 0.240D3 * zeta3
      t220 = 0.120D3 * t69 * lh
      t221 = t217 - t218 - t220
      t222 = t221 * t14
      t233 = t89 * t23
      t234 = t4 * t233
      t235 = t28 * t161
      t236 = t235 * t167
      t239 = log(-0.4D1 * t234 * t236)
      t250 = x2 * t89
      t251 = t250 * t23
      t254 = log(-0.4D1 * t251 * t236)
      t255 = t254 ** 2
      t272 = -(0.90D2 * t15 * t16 * (t158 + t172 * t173 / 0.2D1 - t171 *
     # t176) - 0.180D3 * t60 * t182 * (t176 - t171 * t173) + t189) * t81
     # * t107 / 0.720D3 - (t74 * t182 * (-t199 * t173 + t176) + 0.90D2 *
     # t15 * t16 * (-t205 * t173 / 0.6D1 - t199 * t158 + t209 + t204 * t
     #176 / 0.2D1) + t222 * t188 - 0.180D3 * t60 * t182 * (t158 - t199 *
     # t176 + t204 * t173 / 0.2D1)) * t107 / 0.720D3 - (0.90D2 * t15 * t
     #16 * (t176 - t239 * t173) - 0.180D3 * t60 * t188) * t81 * t108 / 0
     #.720D3 - (0.90D2 * t15 * t16 * (t255 * t173 / 0.2D1 - t254 * t176 
     #+ t158) - 0.180D3 * t60 * t182 * (t176 - t254 * t173) + t189) * t8
     #3 * t107 / 0.720D3
      t273 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t155, -t157, 0.0D0, t272)
      t275 = t2 * x3
      t276 = t2 * t3
      t281 = log(-0.4D1 * t159 * t23 * t88 * t3)
      t282 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t276, t275, 0.0D0)
      t284 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t276, t275, 0.0D0)
      t285 = t281 ** 2
      t286 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t276, t275, 0.0D0)
      t298 = t182 * t286
      t299 = t74 * t298
      t304 = t159 * x2
      t305 = t23 * t28
      t310 = log(-0.4D1 * t304 * t305 * t21 * t3)
      t322 = x3 * t21
      t323 = t305 * t3
      t326 = log(-0.4D1 * t322 * t323)
      t332 = t326 ** 2
      t335 = t332 * t326
      t338 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t276, t275, 0.0D0)
      t354 = t4 * t21
      t357 = log(-0.4D1 * t354 * t323)
      t359 = t357 ** 2
      t375 = -(0.90D2 * t15 * t16 * (-t281 * t282 + t284 + t285 * t286 /
     # 0.2D1) - 0.180D3 * t60 * t182 * (-t281 * t286 + t282) + t299) * t
     #81 * t107 / 0.720D3 - (0.90D2 * t15 * t16 * (t282 - t310 * t286) -
     # 0.180D3 * t60 * t298) * t81 * t108 / 0.720D3 - (t74 * t182 * (-t3
     #26 * t286 + t282) + 0.90D2 * t15 * t16 * (-t326 * t284 + t332 * t2
     #82 / 0.2D1 - t335 * t286 / 0.6D1 + t338) + t222 * t298 - 0.180D3 *
     # t60 * t182 * (-t326 * t282 + t284 + t332 * t286 / 0.2D1)) * t81 /
     # 0.1440D4 - (0.90D2 * t15 * t16 * (-t357 * t282 + t284 + t359 * t2
     #86 / 0.2D1) - 0.180D3 * t60 * t182 * (t282 - t357 * t286) + t299) 
     #* t81 * t83 / 0.1440D4
      t376 = FJET(XB1, XB2, s, t275, 0.0D0, -t276, 0.0D0, 0.0D0, t375)
      t378 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t276, t275, 0.0D0)
      t380 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t276, t275, 0.0D0)
      t388 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t276, t275, 0.0D0)
      t390 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t276, t275, 0.0D0)
      t395 = t182 * t378
      t419 = t74 * t395
      t451 = -(t74 * t182 * (-t326 * t378 + t380) + 0.90D2 * t15 * t16 *
     # (t332 * t380 / 0.2D1 - t335 * t378 / 0.6D1 - t326 * t388 + t390) 
     #+ t222 * t395 - 0.180D3 * t60 * t182 * (-t326 * t380 + t332 * t378
     # / 0.2D1 + t388)) * t81 / 0.1440D4 - (0.90D2 * t15 * t16 * (-t281 
     #* t380 + t285 * t378 / 0.2D1 + t388) - 0.180D3 * t60 * t182 * (-t2
     #81 * t378 + t380) + t419) * t81 * t107 / 0.720D3 - (0.90D2 * t15 *
     # t16 * (-t310 * t378 + t380) - 0.180D3 * t60 * t395) * t81 * t108 
     #/ 0.720D3 - (0.90D2 * t15 * t16 * (t388 - t357 * t380 + t359 * t37
     #8 / 0.2D1) - 0.180D3 * t60 * t182 * (t380 - t357 * t378) + t419) *
     # t81 * t83 / 0.1440D4
      t452 = FJET(XB1, XB2, s, 0.0D0, -t276, 0.0D0, t275, 0.0D0, t451)
      t454 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t455 = t88 * t23
      t458 = log(0.4D1 * t159 * t455)
      t459 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t461 = t458 ** 2
      t462 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t474 = t182 * t462
      t475 = t74 * t474
      t482 = log(0.4D1 * t193 * t24)
      t487 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t489 = t482 ** 2
      t490 = t489 * t482
      t499 = t222 * t474
      t510 = t15 * t16
      t515 = log(0.4D1 * t304 * t305 * t21 * t29)
      t519 = log(0.4D1 * t304 * t455)
      t529 = log(0.4D1 * t251 * t88 * t29)
      t533 = log(0.4D1 * t250 * t455)
      t534 = t533 ** 2
      t537 = t529 ** 2
      t555 = t305 * t29
      t558 = log(0.4D1 * t354 * t555)
      t562 = log(0.4D1 * t4 * t455)
      t563 = t562 ** 2
      t566 = t558 ** 2
      t590 = x2 * t21
      t593 = log(0.4D1 * t590 * t305)
      t594 = t593 ** 2
      t597 = log(0.4D1 * t590 * t555)
      t598 = t597 ** 2
      t600 = -t594 / 0.2D1 + t598 / 0.2D1
      t606 = -t598 * t597 / 0.6D1 + t594 * t593 / 0.6D1
      t617 = t593 - t597
      t622 = rrgq2qgh83J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t627 = log(0.4D1 * t455)
      t628 = t627 ** 2
      t631 = t628 * t627
      t635 = (-0.90D2 * t628 * lh + t217 - t218 - t220 - 0.15D2 * t631 -
     # t627 * t73) * t16
      t641 = log(0.4D1 * t322 * t305)
      t646 = t641 ** 2
      t647 = t646 * t641
      t671 = (0.180D3 * t627 * lh + 0.45D2 * t628 + t70 - t72) * t16
      t678 = (-0.180D3 * lh - 0.90D2 * t627) * t16
      t687 = t71 ** 2
      t688 = t69 ** 2
      t694 = t628 ** 2
      t697 = (0.30D2 * t631 * lh + t628 * t73 / 0.2D1 - t627 * t221 + t6
     #87 + 0.60D2 * t688 + 0.480D3 * lh * zeta3 - 0.60D2 * t69 * t71 + 0
     #.15D2 / 0.4D1 * t694) * t16
      t701 = -(0.90D2 * t15 * t16 * (-t454 + t458 * t459 - t461 * t462 /
     # 0.2D1) - 0.180D3 * t60 * t182 * (t458 * t462 - t459) - t475) * t8
     #1 * t107 / 0.720D3 - (t74 * t182 * (-t459 + t482 * t462) + 0.90D2 
     #* t15 * t16 * (-t487 + t482 * t454 + t490 * t462 / 0.6D1 - t489 * 
     #t459 / 0.2D1) - t499 - 0.180D3 * t60 * t182 * (t482 * t459 - t454 
     #- t489 * t462 / 0.2D1)) * t107 / 0.720D3 - t510 * (-t515 * t462 + 
     #t519 * t462) * t81 * t108 / 0.8D1 - (0.90D2 * t15 * t16 * (-t529 *
     # t459 - t534 * t462 / 0.2D1 + t537 * t462 / 0.2D1 + t533 * t459) -
     # 0.180D3 * t60 * t182 * (t533 * t462 - t529 * t462)) * t83 * t107 
     #/ 0.720D3 - (0.90D2 * t15 * t16 * (-t558 * t459 - t563 * t462 / 0.
     #2D1 + t566 * t462 / 0.2D1 + t562 * t459) - 0.180D3 * t60 * t182 * 
     #(-t558 * t462 + t562 * t462)) * t81 * t83 / 0.1440D4 - ((0.90D2 * 
     #t15 * t16 * t459 - 0.180D3 * t60 * t474) * t600 + 0.90D2 * t15 * t
     #16 * t462 * t606 + (0.90D2 * t15 * t16 * t454 - 0.180D3 * t60 * t1
     #82 * t459 + t475) * t617) * t83 / 0.1440D4 + t15 * t16 * t622 / 0.
     #16D2 + t635 * t15 * t459 / 0.1440D4 - (t74 * t182 * (t641 * t462 -
     # t459) + 0.90D2 * t15 * t16 * (-t487 + t647 * t462 / 0.6D1 + t641 
     #* t454 - t646 * t459 / 0.2D1) - t499 - 0.180D3 * t60 * t182 * (-t4
     #54 + t641 * t459 - t646 * t462 / 0.2D1)) * t81 / 0.1440D4 + t671 *
     # t15 * t454 / 0.1440D4 + t678 * t15 * t487 / 0.1440D4 + t697 * t15
     # * t462 / 0.1440D4
      t702 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t701)
      t704 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #8, t12, 0.0D0)
      t705 = z * t704
      t706 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #8, t12, 0.0D0)
      t714 = t76 * t706 * t56
      t721 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #8, t12, 0.0D0)
      t742 = -(0.90D2 * t15 * t16 * (t705 - t96 * t706) * t56 - 0.180D3 
     #* t61 * t714) * t81 * t108 / 0.720D3 - (0.90D2 * t15 * t16 * (z * 
     #t721 - t38 * t704 + t42 * t706 / 0.2D1) * t56 - 0.180D3 * t61 * t1
     #6 * (t705 - t38 * t706) * t56 + t75 * t714) * t81 * t83 / 0.1440D4
      t743 = FJET(XB1, XB2, s, t12, 0.0D0, t8, 0.0D0, 0.0D0, t742)
      t745 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #8, t12, 0.0D0)
      t747 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #8, t12, 0.0D0)
      t749 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #8, t12, 0.0D0)
      t757 = z * t747
      t765 = t76 * t749 * t56
      t783 = -(0.90D2 * t15 * t16 * (z * t745 - t38 * t747 + t42 * t749 
     #/ 0.2D1) * t56 - 0.180D3 * t61 * t16 * (t757 - t38 * t749) * t56 +
     # t75 * t765) * t81 * t83 / 0.1440D4 - (0.90D2 * t15 * t16 * (t757 
     #- t96 * t749) * t56 - 0.180D3 * t61 * t765) * t81 * t108 / 0.720D3
      t784 = FJET(XB1, XB2, s, 0.0D0, t12, 0.0D0, t8, 0.0D0, t783)
      t787 = t1 * t156
      t788 = x3 * s * t787
      t789 = x3 * x1
      t790 = t2 * t789
      t791 = t3 * s
      t792 = t791 * t787
      t793 = t1 * x1
      t794 = t791 * t793
      t796 = t161 * t165
      t801 = log(0.4D1 * t159 * t305 * t796 * t166 * t3)
      t802 = t801 ** 2
      t803 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t794, t790, t7
     #92, -t788, 0.0D0)
      t806 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t794, t790, t7
     #92, -t788, 0.0D0)
      t807 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t794, t790, t7
     #92, -t788, 0.0D0)
      t818 = t182 * t803
      t827 = log(0.4D1 * t234 * t235 * t167 * t3)
      t839 = -(0.90D2 * t15 * t16 * (-t802 * t803 / 0.2D1 - t806 + t801 
     #* t807) - 0.180D3 * t60 * t182 * (-t807 + t801 * t803) - t74 * t81
     #8) * t81 * t107 / 0.720D3 - (0.90D2 * t15 * t16 * (t827 * t803 - t
     #807) + 0.180D3 * t60 * t818) * t81 * t108 / 0.720D3
      t840 = FJET(XB1, XB2, s, -t788, t790, t792, -t794, 0.0D0, t839)
      t843 = t9 * s * t793
      t844 = x2 * x1
      t846 = t2 * t844 * t165
      t851 = s * t22 * x2 * x1 * t156 * t165
      t852 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t843, -t846, -
     #t157, 0.0D0, t851)
      t857 = log(-0.4D1 * t234 * t235 * t167 * t29)
      t858 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t843, -t846, -
     #t157, 0.0D0, t851)
      t864 = t182 * t858
      t875 = log(-0.4D1 * t250 * t305 * t796 * t166 * t29)
      t877 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t843, -t846, -
     #t157, 0.0D0, t851)
      t878 = t875 ** 2
      t895 = -(0.90D2 * t15 * t16 * (-t852 + t857 * t858) + 0.180D3 * t6
     #0 * t864) * t81 * t108 / 0.720D3 - (0.90D2 * t15 * t16 * (t875 * t
     #852 - t877 - t878 * t858 / 0.2D1) - 0.180D3 * t60 * t182 * (-t852 
     #+ t875 * t858) - t74 * t864) * t83 * t107 / 0.720D3
      t896 = FJET(XB1, XB2, s, -t157, -t843, 0.0D0, -t846, t851, t895)
      t898 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -t
     #157, 0.0D0, 0.0D0)
      t899 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -t
     #157, 0.0D0, 0.0D0)
      t902 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -t
     #157, 0.0D0, 0.0D0)
      t913 = t182 * t899
      t914 = t74 * t913
      t925 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -t
     #157, 0.0D0, 0.0D0)
      t968 = -(0.90D2 * t15 * t16 * (t898 + t172 * t899 / 0.2D1 - t171 *
     # t902) - 0.180D3 * t60 * t182 * (t902 - t171 * t899) + t914) * t81
     # * t107 / 0.720D3 - (t74 * t182 * (-t199 * t899 + t902) + 0.90D2 *
     # t15 * t16 * (-t199 * t898 - t205 * t899 / 0.6D1 + t925 + t204 * t
     #902 / 0.2D1) + t222 * t913 - 0.180D3 * t60 * t182 * (t898 + t204 *
     # t899 / 0.2D1 - t199 * t902)) * t107 / 0.720D3 - (0.90D2 * t15 * t
     #16 * (-t239 * t899 + t902) - 0.180D3 * t60 * t913) * t81 * t108 / 
     #0.720D3 - (0.90D2 * t15 * t16 * (t255 * t899 / 0.2D1 - t254 * t902
     # + t898) - 0.180D3 * t60 * t182 * (t902 - t254 * t899) + t914) * t
     #83 * t107 / 0.720D3
      t969 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t157, t155, 0.0D0, t968)
      t971 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t794, t790, t7
     #92, -t788, 0.0D0)
      t973 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t794, t790, t7
     #92, -t788, 0.0D0)
      t976 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, -t794, t790, t7
     #92, -t788, 0.0D0)
      t986 = t182 * t973
      t1002 = -(0.90D2 * t15 * t16 * (t801 * t971 - t802 * t973 / 0.2D1 
     #- t976) - 0.180D3 * t60 * t182 * (t801 * t973 - t971) - t74 * t986
     #) * t81 * t107 / 0.720D3 - (0.90D2 * t15 * t16 * (-t971 + t827 * t
     #973) + 0.180D3 * t60 * t986) * t81 * t108 / 0.720D3
      t1003 = FJET(XB1, XB2, s, t792, -t794, -t788, t790, 0.0D0, t1002)
      t1005 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1006 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1013 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1016 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1022 = t182 * t1006
      t1023 = t222 * t1022
      t1071 = t74 * t1022
      t1083 = rrgq2qgh81J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1159 = -(t74 * t182 * (-t1005 + t641 * t1006) + 0.90D2 * t15 * t1
     #6 * (t647 * t1006 / 0.6D1 - t1013 - t646 * t1005 / 0.2D1 + t641 * 
     #t1016) - t1023 - 0.180D3 * t60 * t182 * (-t646 * t1006 / 0.2D1 + t
     #641 * t1005 - t1016)) * t81 / 0.1440D4 - (0.90D2 * t15 * t16 * (-t
     #558 * t1005 - t563 * t1006 / 0.2D1 + t566 * t1006 / 0.2D1 + t562 *
     # t1005) - 0.180D3 * t60 * t182 * (-t558 * t1006 + t562 * t1006)) *
     # t81 * t83 / 0.1440D4 - ((0.90D2 * t15 * t16 * t1005 - 0.180D3 * t
     #60 * t1022) * t600 + 0.90D2 * t15 * t16 * t1006 * t606 + (0.90D2 *
     # t15 * t16 * t1016 - 0.180D3 * t60 * t182 * t1005 + t1071) * t617)
     # * t83 / 0.1440D4 + t678 * t15 * t1013 / 0.1440D4 + t697 * t15 * t
     #1006 / 0.1440D4 + t15 * t16 * t1083 / 0.16D2 + t635 * t15 * t1005 
     #/ 0.1440D4 - (0.90D2 * t15 * t16 * (t458 * t1005 - t1016 - t461 * 
     #t1006 / 0.2D1) - 0.180D3 * t60 * t182 * (-t1005 + t458 * t1006) - 
     #t1071) * t81 * t107 / 0.720D3 - (t74 * t182 * (-t1005 + t482 * t10
     #06) + 0.90D2 * t15 * t16 * (-t489 * t1005 / 0.2D1 - t1013 + t482 *
     # t1016 + t490 * t1006 / 0.6D1) - t1023 - 0.180D3 * t60 * t182 * (-
     #t489 * t1006 / 0.2D1 + t482 * t1005 - t1016)) * t107 / 0.720D3 - t
     #510 * (-t515 * t1006 + t519 * t1006) * t81 * t108 / 0.8D1 - (0.90D
     #2 * t15 * t16 * (-t534 * t1006 / 0.2D1 - t529 * t1005 + t537 * t10
     #06 / 0.2D1 + t533 * t1005) - 0.180D3 * t60 * t182 * (-t529 * t1006
     # + t533 * t1006)) * t83 * t107 / 0.720D3 + t671 * t15 * t1016 / 0.
     #1440D4
      t1160 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t1159)
      t1162 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t843, -t846, 
     #-t157, 0.0D0, t851)
      t1164 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t843, -t846, 
     #-t157, 0.0D0, t851)
      t1169 = t182 * t1162
      t1176 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, -t843, -t846, 
     #-t157, 0.0D0, t851)
      t1193 = -(0.90D2 * t15 * t16 * (t857 * t1162 - t1164) + 0.180D3 * 
     #t60 * t1169) * t81 * t108 / 0.720D3 - (0.90D2 * t15 * t16 * (t875 
     #* t1164 - t1176 - t878 * t1162 / 0.2D1) - 0.180D3 * t60 * t182 * (
     #t875 * t1162 - t1164) - t74 * t1169) * t83 * t107 / 0.720D3
      t1194 = FJET(XB1, XB2, s, 0.0D0, -t846, -t157, -t843, t851, t1193)
      t1196 = t112 * t111 + t153 * t152 + t273 * t272 + t376 * t375 + t4
     #52 * t451 + t702 * t701 + t743 * t742 + t784 * t783 + t840 * t839 
     #+ t896 * t895 + t969 * t968 + t1003 * t1002 + t1160 * t1159 + t119
     #4 * t1193
      t1197 = t789 * z
      t1200 = Sqrt(x3 * t164 * t50)
      t1201 = t48 * t1200
      t1202 = 0.2D1 * t1201
      t1207 = t155 * t9 * (-t4 - z + t49 - x1 + t789 + t163 - t1197 + t1
     #202) * t165 * t6
      t1209 = t791 * t787 * t6
      t1212 = x2 ** 2
      t1213 = x3 * t1212
      t1214 = t1213 * t163
      t1217 = t4 * z
      t1219 = t4 * x1
      t1221 = t1213 * x1
      t1223 = -0.2D1 * t4 * t163 + t1214 + 0.2D1 * t1201 * x2 - x2 + t11
     #97 + 0.2D1 * t1217 + 0.2D1 * t1219 + t4 - t49 - t789 - t1221 - t12
     #13 * z - t1202
      t1226 = t155 * t1223 * t165 * t6
      t1227 = t157 * t11
      t1228 = t844 * z
      t1229 = z + x1 - t163 - t844 + t1228
      t1230 = t164 * t1229
      t1231 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t1207, -t1226,
     # -t1209, -t1227, t851)
      t1241 = log(0.4D1 * t4 * t233 * t28 * t161 * t166 * t165 * t86 * t
     #32)
      t1242 = t1241 * t164
      t1243 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t1207, -t1226,
     # -t1209, -t1227, t851)
      t1253 = t19 * x2
      t1255 = t89 * t19
      t1266 = t250 + x3 * t19 * t844 + 0.2D1 * t159 * x2 * z - t159 * t1
     #253 - t1214 - t19 + t1255 * x2 + 0.2D1 * t1201 * t1228 - t1253 * x
     #1 - 0.2D1 * t250 * z + 0.2D1 * t1201 * z + 0.2D1 * t1201 * x1
      t1276 = -0.2D1 * t163 - 0.2D1 * t1201 * t844 - 0.2D1 * t1201 * t16
     #3 - t89 + 0.2D1 * x1 * t19 + 0.2D1 * t89 * z - t1255 + t1228 - t12
     #17 - t1219 - t304 + t1221
      t1278 = 0.1D1 / (t1266 + t1276)
      t1282 = t60 * t182
      t1287 = 0.90D2 * t15 * t16 * (t1230 * t1231 - t1242 * t1229 * t124
     #3) * t1278 - 0.180D3 * t1282 * t1230 * t1243 * t1278
      t1291 = FJET(XB1, XB2, s, t1207, -t1209, -t1226, -t1227, t851, -t1
     #287 * t81 * t108 / 0.720D3)
      t1294 = t81 * t83 * t107
      t1297 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t794, t790, t
     #792, -t788, 0.0D0)
      t1299 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t794, t790, t
     #792, -t788, 0.0D0)
      t1302 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t794, t790, t
     #792, -t788, 0.0D0)
      t1312 = t182 * t1299
      t1328 = -(0.90D2 * t15 * t16 * (t801 * t1297 - t802 * t1299 / 0.2D
     #1 - t1302) - 0.180D3 * t60 * t182 * (t801 * t1299 - t1297) - t74 *
     # t1312) * t81 * t107 / 0.720D3 - (0.90D2 * t15 * t16 * (t827 * t12
     #99 - t1297) + 0.180D3 * t60 * t1312) * t81 * t108 / 0.720D3
      t1329 = FJET(XB1, XB2, s, -t794, t792, t790, -t788, 0.0D0, t1328)
      t1331 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1335 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1340 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1345 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1351 = t182 * t1331
      t1352 = t222 * t1351
      t1400 = t74 * t1351
      t1406 = rrgq2qgh84J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1485 = t697 * t15 * t1331 / 0.1440D4 - (t74 * t182 * (-t1335 + t6
     #41 * t1331) + 0.90D2 * t15 * t16 * (-t1340 - t646 * t1335 / 0.2D1 
     #+ t647 * t1331 / 0.6D1 + t641 * t1345) - t1352 - 0.180D3 * t60 * t
     #182 * (-t1345 + t641 * t1335 - t646 * t1331 / 0.2D1)) * t81 / 0.14
     #40D4 - (0.90D2 * t15 * t16 * (t566 * t1331 / 0.2D1 + t562 * t1335 
     #- t558 * t1335 - t563 * t1331 / 0.2D1) - 0.180D3 * t60 * t182 * (-
     #t558 * t1331 + t562 * t1331)) * t81 * t83 / 0.1440D4 - ((0.90D2 * 
     #t15 * t16 * t1335 - 0.180D3 * t60 * t1351) * t600 + 0.90D2 * t15 *
     # t16 * t1331 * t606 + (0.90D2 * t15 * t16 * t1345 - 0.180D3 * t60 
     #* t182 * t1335 + t1400) * t617) * t83 / 0.1440D4 + t15 * t16 * t14
     #06 / 0.16D2 + t635 * t15 * t1335 / 0.1440D4 + t671 * t15 * t1345 /
     # 0.1440D4 - (0.90D2 * t15 * t16 * (t458 * t1335 - t1345 - t461 * t
     #1331 / 0.2D1) - 0.180D3 * t60 * t182 * (-t1335 + t458 * t1331) - t
     #1400) * t81 * t107 / 0.720D3 - (t74 * t182 * (-t1335 + t482 * t133
     #1) + 0.90D2 * t15 * t16 * (-t1340 + t490 * t1331 / 0.6D1 - t489 * 
     #t1335 / 0.2D1 + t482 * t1345) - t1352 - 0.180D3 * t60 * t182 * (t4
     #82 * t1335 - t1345 - t489 * t1331 / 0.2D1)) * t107 / 0.720D3 - t51
     #0 * (t519 * t1331 - t515 * t1331) * t81 * t108 / 0.8D1 - (0.90D2 *
     # t15 * t16 * (-t529 * t1335 + t537 * t1331 / 0.2D1 - t534 * t1331 
     #/ 0.2D1 + t533 * t1335) - 0.180D3 * t60 * t182 * (-t529 * t1331 + 
     #t533 * t1331)) * t83 * t107 / 0.720D3 + t678 * t15 * t1340 / 0.144
     #0D4
      t1486 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t1485)
      t1488 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -
     #t157, 0.0D0, 0.0D0)
      t1490 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -
     #t157, 0.0D0, 0.0D0)
      t1493 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -
     #t157, 0.0D0, 0.0D0)
      t1503 = t182 * t1490
      t1504 = t74 * t1503
      t1514 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -
     #t157, 0.0D0, 0.0D0)
      t1558 = -(0.90D2 * t15 * t16 * (-t171 * t1488 + t172 * t1490 / 0.2
     #D1 + t1493) - 0.180D3 * t60 * t182 * (t1488 - t171 * t1490) + t150
     #4) * t81 * t107 / 0.720D3 - (t74 * t182 * (t1488 - t199 * t1490) +
     # 0.90D2 * t15 * t16 * (-t205 * t1490 / 0.6D1 + t1514 - t199 * t149
     #3 + t204 * t1488 / 0.2D1) + t222 * t1503 - 0.180D3 * t60 * t182 * 
     #(-t199 * t1488 + t1493 + t204 * t1490 / 0.2D1)) * t107 / 0.720D3 -
     # (0.90D2 * t15 * t16 * (-t239 * t1490 + t1488) - 0.180D3 * t60 * t
     #1503) * t81 * t108 / 0.720D3 - (0.90D2 * t15 * t16 * (t255 * t1490
     # / 0.2D1 + t1493 - t254 * t1488) - 0.180D3 * t60 * t182 * (t1488 -
     # t254 * t1490) + t1504) * t83 * t107 / 0.720D3
      t1559 = FJET(XB1, XB2, s, t155, -t157, 0.0D0, 0.0D0, 0.0D0, t1558)
      t1561 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t1207, -t1226,
     # -t1209, -t1227, t851)
      t1563 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t1207, -t1226,
     # -t1209, -t1227, t851)
      t1575 = 0.90D2 * t15 * t16 * (t1230 * t1561 - t1242 * t1229 * t156
     #3) * t1278 - 0.180D3 * t1282 * t1230 * t1563 * t1278
      t1579 = FJET(XB1, XB2, s, -t1226, -t1227, t1207, -t1209, t851, -t1
     #575 * t81 * t108 / 0.720D3)
      t1583 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t1207, -t1226,
     # -t1209, -t1227, t851)
      t1585 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t1207, -t1226,
     # -t1209, -t1227, t851)
      t1597 = 0.90D2 * t15 * t16 * (t1230 * t1583 - t1242 * t1229 * t158
     #5) * t1278 - 0.180D3 * t1282 * t1230 * t1585 * t1278
      t1601 = FJET(XB1, XB2, s, -t1227, -t1226, -t1209, t1207, t851, -t1
     #597 * t81 * t108 / 0.720D3)
      t1605 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t276, t275, 0.0D0)
      t1606 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t276, t275, 0.0D0)
      t1609 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t276, t275, 0.0D0)
      t1620 = t182 * t1606
      t1621 = t74 * t1620
      t1646 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t276, t275, 0.0D0)
      t1678 = -(0.90D2 * t15 * t16 * (t1605 + t285 * t1606 / 0.2D1 - t28
     #1 * t1609) - 0.180D3 * t60 * t182 * (-t281 * t1606 + t1609) + t162
     #1) * t81 * t107 / 0.720D3 - (0.90D2 * t15 * t16 * (t1609 - t310 * 
     #t1606) - 0.180D3 * t60 * t1620) * t81 * t108 / 0.720D3 - (t74 * t1
     #82 * (-t326 * t1606 + t1609) + 0.90D2 * t15 * t16 * (t332 * t1609 
     #/ 0.2D1 - t335 * t1606 / 0.6D1 - t326 * t1605 + t1646) + t222 * t1
     #620 - 0.180D3 * t60 * t182 * (-t326 * t1609 + t1605 + t332 * t1606
     # / 0.2D1)) * t81 / 0.1440D4 - (0.90D2 * t15 * t16 * (t1605 + t359 
     #* t1606 / 0.2D1 - t357 * t1609) - 0.180D3 * t60 * t182 * (t1609 - 
     #t357 * t1606) + t1621) * t81 * t83 / 0.1440D4
      t1679 = FJET(XB1, XB2, s, -t276, 0.0D0, t275, 0.0D0, 0.0D0, t1678)
      t1681 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t794, t790, t
     #792, -t788, 0.0D0)
      t1682 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t794, t790, t
     #792, -t788, 0.0D0)
      t1684 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t794, t790, t
     #792, -t788, 0.0D0)
      t1696 = t182 * t1684
      t1712 = -(0.90D2 * t15 * t16 * (-t1681 + t801 * t1682 - t802 * t16
     #84 / 0.2D1) - 0.180D3 * t60 * t182 * (-t1682 + t801 * t1684) - t74
     # * t1696) * t81 * t107 / 0.720D3 - (0.90D2 * t15 * t16 * (-t1682 +
     # t827 * t1684) + 0.180D3 * t60 * t1696) * t81 * t108 / 0.720D3
      t1713 = FJET(XB1, XB2, s, t790, -t788, -t794, t792, 0.0D0, t1712)
      t1715 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t1207, -t1226,
     # -t1209, -t1227, t851)
      t1717 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t1207, -t1226,
     # -t1209, -t1227, t851)
      t1729 = 0.90D2 * t15 * t16 * (t1230 * t1715 - t1242 * t1229 * t171
     #7) * t1278 - 0.180D3 * t1282 * t1230 * t1717 * t1278
      t1733 = FJET(XB1, XB2, s, -t1209, t1207, -t1227, -t1226, t851, -t1
     #729 * t81 * t108 / 0.720D3)
      t1737 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t843, -t846, 
     #-t157, 0.0D0, t851)
      t1738 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t843, -t846, 
     #-t157, 0.0D0, t851)
      t1744 = t182 * t1738
      t1750 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, -t843, -t846, 
     #-t157, 0.0D0, t851)
      t1768 = -(0.90D2 * t15 * t16 * (-t1737 + t857 * t1738) + 0.180D3 *
     # t60 * t1744) * t81 * t108 / 0.720D3 - (0.90D2 * t15 * t16 * (-t17
     #50 - t878 * t1738 / 0.2D1 + t875 * t1737) - 0.180D3 * t60 * t182 *
     # (-t1737 + t875 * t1738) - t74 * t1744) * t83 * t107 / 0.720D3
      t1769 = FJET(XB1, XB2, s, -t846, 0.0D0, -t843, -t157, t851, t1768)
      t1771 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -
     #t157, 0.0D0, 0.0D0)
      t1774 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -
     #t157, 0.0D0, 0.0D0)
      t1775 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -
     #t157, 0.0D0, 0.0D0)
      t1786 = t182 * t1771
      t1787 = t74 * t1786
      t1796 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, t155, 0.0D0, -
     #t157, 0.0D0, 0.0D0)
      t1841 = -(0.90D2 * t15 * t16 * (t172 * t1771 / 0.2D1 + t1774 - t17
     #1 * t1775) - 0.180D3 * t60 * t182 * (t1775 - t171 * t1771) + t1787
     #) * t81 * t107 / 0.720D3 - (t74 * t182 * (t1775 - t199 * t1771) + 
     #0.90D2 * t15 * t16 * (-t199 * t1774 + t1796 - t205 * t1771 / 0.6D1
     # + t204 * t1775 / 0.2D1) + t222 * t1786 - 0.180D3 * t60 * t182 * (
     #-t199 * t1775 + t1774 + t204 * t1771 / 0.2D1)) * t107 / 0.720D3 - 
     #(0.90D2 * t15 * t16 * (t1775 - t239 * t1771) - 0.180D3 * t60 * t17
     #86) * t81 * t108 / 0.720D3 - (0.90D2 * t15 * t16 * (t255 * t1771 /
     # 0.2D1 - t254 * t1775 + t1774) - 0.180D3 * t60 * t182 * (t1775 - t
     #254 * t1771) + t1787) * t83 * t107 / 0.720D3
      t1842 = FJET(XB1, XB2, s, -t157, t155, 0.0D0, 0.0D0, 0.0D0, t1841)
      t1844 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t843, -t846, 
     #-t157, 0.0D0, t851)
      t1845 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t843, -t846, 
     #-t157, 0.0D0, t851)
      t1851 = t182 * t1845
      t1859 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, -t843, -t846, 
     #-t157, 0.0D0, t851)
      t1875 = -(0.90D2 * t15 * t16 * (-t1844 + t857 * t1845) + 0.180D3 *
     # t60 * t1851) * t81 * t108 / 0.720D3 - (0.90D2 * t15 * t16 * (-t87
     #8 * t1845 / 0.2D1 - t1859 + t875 * t1844) - 0.180D3 * t60 * t182 *
     # (-t1844 + t875 * t1845) - t74 * t1851) * t83 * t107 / 0.720D3
      t1876 = FJET(XB1, XB2, s, -t843, -t157, -t846, 0.0D0, t851, t1875)
      t1878 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t276, t275, 0.0D0)
      t1880 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t276, t275, 0.0D0)
      t1888 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t276, t275, 0.0D0)
      t1889 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t276, t275, 0.0D0)
      t1895 = t182 * t1878
      t1919 = t74 * t1895
      t1951 = -(t74 * t182 * (-t326 * t1878 + t1880) + 0.90D2 * t15 * t1
     #6 * (t332 * t1880 / 0.2D1 - t335 * t1878 / 0.6D1 + t1888 - t326 * 
     #t1889) + t222 * t1895 - 0.180D3 * t60 * t182 * (-t326 * t1880 + t3
     #32 * t1878 / 0.2D1 + t1889)) * t81 / 0.1440D4 - (0.90D2 * t15 * t1
     #6 * (t1889 + t285 * t1878 / 0.2D1 - t281 * t1880) - 0.180D3 * t60 
     #* t182 * (t1880 - t281 * t1878) + t1919) * t81 * t107 / 0.720D3 - 
     #(0.90D2 * t15 * t16 * (t1880 - t310 * t1878) - 0.180D3 * t60 * t18
     #95) * t81 * t108 / 0.720D3 - (0.90D2 * t15 * t16 * (-t357 * t1880 
     #+ t1889 + t359 * t1878 / 0.2D1) - 0.180D3 * t60 * t182 * (-t357 * 
     #t1878 + t1880) + t1919) * t81 * t83 / 0.1440D4
      t1952 = FJET(XB1, XB2, s, 0.0D0, t275, 0.0D0, -t276, 0.0D0, t1951)
      t1954 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1955 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1960 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1965 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1971 = t182 * t1955
      t1972 = t222 * t1971
      t2020 = t74 * t1971
      t2026 = rrgq2qgh82J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t2108 = -(t74 * t182 * (-t1954 + t641 * t1955) + 0.90D2 * t15 * t1
     #6 * (-t1960 - t646 * t1954 / 0.2D1 + t647 * t1955 / 0.6D1 + t641 *
     # t1965) - t1972 - 0.180D3 * t60 * t182 * (-t1965 - t646 * t1955 / 
     #0.2D1 + t641 * t1954)) * t81 / 0.1440D4 - (0.90D2 * t15 * t16 * (t
     #566 * t1955 / 0.2D1 + t562 * t1954 - t563 * t1955 / 0.2D1 - t558 *
     # t1954) - 0.180D3 * t60 * t182 * (t562 * t1955 - t558 * t1955)) * 
     #t81 * t83 / 0.1440D4 - ((0.90D2 * t15 * t16 * t1954 - 0.180D3 * t6
     #0 * t1971) * t600 + 0.90D2 * t15 * t16 * t1955 * t606 + (0.90D2 * 
     #t15 * t16 * t1965 - 0.180D3 * t60 * t182 * t1954 + t2020) * t617) 
     #* t83 / 0.1440D4 + t15 * t16 * t2026 / 0.16D2 + t635 * t15 * t1954
     # / 0.1440D4 + t671 * t15 * t1965 / 0.1440D4 + t678 * t15 * t1960 /
     # 0.1440D4 - (0.90D2 * t15 * t16 * (-t1965 + t458 * t1954 - t461 * 
     #t1955 / 0.2D1) - 0.180D3 * t60 * t182 * (-t1954 + t458 * t1955) - 
     #t2020) * t81 * t107 / 0.720D3 - (t74 * t182 * (-t1954 + t482 * t19
     #55) + 0.90D2 * t15 * t16 * (t482 * t1965 - t489 * t1954 / 0.2D1 + 
     #t490 * t1955 / 0.6D1 - t1960) - t1972 - 0.180D3 * t60 * t182 * (-t
     #1965 + t482 * t1954 - t489 * t1955 / 0.2D1)) * t107 / 0.720D3 - t5
     #10 * (t519 * t1955 - t515 * t1955) * t81 * t108 / 0.8D1 - (0.90D2 
     #* t15 * t16 * (t537 * t1955 / 0.2D1 + t533 * t1954 - t529 * t1954 
     #- t534 * t1955 / 0.2D1) - 0.180D3 * t60 * t182 * (-t529 * t1955 + 
     #t533 * t1955)) * t83 * t107 / 0.720D3 + t697 * t15 * t1955 / 0.144
     #0D4
      t2109 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t2108)
      t2111 = -t1291 * t1287 * t1294 / 0.720D3 + t1329 * t1328 + t1486 *
     # t1485 + t1559 * t1558 - t1579 * t1575 * t1294 / 0.720D3 - t1601 *
     # t1597 * t1294 / 0.720D3 + t1679 * t1678 + t1713 * t1712 - t1733 *
     # t1729 * t1294 / 0.720D3 + t1769 * t1768 + t1842 * t1841 + t1876 *
     # t1875 + t1952 * t1951 + t2109 * t2108
      rrgq2qght8s4e1 = t1196 + t2111

      end function



      doubleprecision function rrgq2qght8s4e0
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = -0.1D1 + x1
      t4 = t2 * t3
      t5 = t2 * x1
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = t7 * pi
      t9 = 0.1D1 / t1
      t10 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4, 
     #0.0D0, 0.0D0)
      t11 = x1 ** 2
      t12 = x3 * t11
      t13 = x4 * pi
      t14 = Sin(t13)
      t15 = t14 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t19 = t1 ** 2
      t20 = t19 ** 2
      t22 = x1 * z
      t23 = -z - x1 + t22
      t24 = 0.1D1 / t23
      t25 = t3 ** 2
      t26 = t24 * t25
      t30 = log(-0.4D1 * t12 * t15 * t18 * t20 * t26)
      t31 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4, 
     #0.0D0, 0.0D0)
      t37 = lh * t7
      t38 = pi * t9
      t39 = t38 * t31
      t41 = 0.180D3 * t37 * t39
      t43 = 0.1D1 / x3
      t45 = 0.1D1 / x1
      t48 = t8 * t9
      t50 = 0.1D1 / x2
      t51 = t50 * t45
      t55 = x2 * t11
      t56 = t55 * t20
      t61 = log(-0.4D1 * t56 * t15 * t18 * t26)
      t71 = t11 * t15
      t77 = log(-0.4D1 * t71 * t18 * t20 * t24 * t25)
      t79 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4, 
     #0.0D0, 0.0D0)
      t80 = t77 ** 2
      t92 = lh ** 2
      t93 = 0.180D3 * t92
      t94 = pi ** 2
      t95 = 0.30D2 * t94
      t96 = t93 - t95
      t97 = t96 * t7
      t102 = -(0.90D2 * t8 * t9 * (t10 - t30 * t31) - t41) * t43 * t45 /
     # 0.720D3 - t48 * t31 * t43 * t51 / 0.8D1 - (0.90D2 * t8 * t9 * (t1
     #0 - t61 * t31) - t41) * t50 * t45 / 0.720D3 - (0.90D2 * t8 * t9 * 
     #(-t77 * t10 + t79 + t80 * t31 / 0.2D1) - 0.180D3 * t37 * t38 * (t1
     #0 - t77 * t31) + t97 * t39) * t45 / 0.720D3
      t103 = FJET(XB1, XB2, s, -t4, t5, 0.0D0, 0.0D0, 0.0D0, t102)
      t105 = -0.1D1 + x2
      t106 = x2 * x3
      t107 = x3 * z
      t108 = x3 * x1
      t109 = t108 * z
      t110 = cos(t13)
      t112 = -0.1D1 + x3
      t113 = x2 * t112
      t115 = Sqrt(x3 * t23 * t113)
      t116 = t110 * t115
      t117 = 0.2D1 * t116
      t120 = -0.1D1 + t106
      t121 = 0.1D1 / t120
      t124 = t5 * t105 * (-t106 - z + t107 - x1 + t108 + t22 - t109 + t1
     #17) * t24 * t121
      t125 = t112 * s
      t126 = t1 * t3
      t128 = t125 * t126 * t121
      t131 = x2 ** 2
      t132 = x3 * t131
      t133 = t132 * t22
      t136 = t106 * z
      t138 = t106 * x1
      t140 = t132 * x1
      t142 = -0.2D1 * t106 * t22 + t133 + 0.2D1 * t116 * x2 - x2 + t109 
     #+ 0.2D1 * t136 + 0.2D1 * t138 + t106 - t107 - t108 - t140 - t132 *
     # z - t117
      t145 = t5 * t142 * t24 * t121
      t147 = x3 * t105 * t121
      t148 = t4 * t147
      t153 = s * t19 * x2 * x1 * t3 * t24
      t155 = x2 * x1
      t156 = t155 * z
      t157 = z + x1 - t22 - t155 + t156
      t159 = t8 * t9 * t23 * t157
      t160 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t124, -t145, -t
     #128, -t148, t153)
      t166 = t17 * x2
      t168 = t11 * t17
      t179 = t55 + x3 * t17 * t155 + 0.2D1 * t12 * x2 * z - t12 * t166 -
     # t133 - t17 + t168 * x2 + 0.2D1 * t116 * t156 - t166 * x1 - 0.2D1 
     #* t55 * z + 0.2D1 * t116 * z + 0.2D1 * t116 * x1
      t190 = -0.2D1 * t22 - 0.2D1 * t116 * t155 - 0.2D1 * t116 * t22 - t
     #11 + 0.2D1 * x1 * t17 + 0.2D1 * t11 * z - t168 + t156 - t136 - t13
     #8 - t12 * x2 + t140
      t192 = 0.1D1 / (t179 + t190)
      t195 = t43 * t50 * t45
      t199 = FJET(XB1, XB2, s, t124, -t128, -t145, -t148, t153, -t159 * 
     #t160 * t192 * t195 / 0.8D1)
      t201 = t38 * t23
      t208 = t2 * t112
      t209 = t2 * x3
      t212 = 0.1D1 / t17 / z
      t213 = t15 * t212
      t217 = log(-0.4D1 * t12 * t20 * t213 * t112)
      t218 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t208, t209, 0.0D0)
      t220 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t208, t209, 0.0D0)
      t225 = t38 * t218
      t227 = 0.180D3 * t37 * t225
      t236 = x3 * t212
      t237 = t20 * t15
      t238 = t237 * t112
      t241 = log(-0.4D1 * t236 * t238)
      t243 = t241 ** 2
      t246 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t208, t209, 0.0D0)
      t260 = t106 * t212
      t263 = log(-0.4D1 * t260 * t238)
      t273 = -(0.90D2 * t8 * t9 * (-t217 * t218 + t220) - t227) * t43 * 
     #t45 / 0.720D3 - t48 * t218 * t43 * t51 / 0.8D1 - (0.90D2 * t8 * t9
     # * (-t241 * t220 + t243 * t218 / 0.2D1 + t246) - 0.180D3 * t37 * t
     #38 * (-t241 * t218 + t220) + t97 * t225) * t43 / 0.1440D4 - (0.90D
     #2 * t8 * t9 * (t220 - t263 * t218) - t227) * t43 * t50 / 0.1440D4
      t274 = FJET(XB1, XB2, s, 0.0D0, -t208, 0.0D0, t209, 0.0D0, t273)
      t276 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t124, -t145, -t
     #128, -t148, t153)
      t281 = FJET(XB1, XB2, s, -t145, -t148, t124, -t128, t153, -t159 * 
     #t276 * t192 * t195 / 0.8D1)
      t289 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t124, -t145, -t
     #128, -t148, t153)
      t294 = FJET(XB1, XB2, s, -t128, t124, -t148, -t145, t153, -t159 * 
     #t289 * t192 * t195 / 0.8D1)
      t302 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t303 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t309 = t38 * t303
      t311 = 0.180D3 * t37 * t309
      t330 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t346 = -(0.90D2 * t8 * t9 * (t302 - t30 * t303) - t311) * t43 * t4
     #5 / 0.720D3 - t48 * t303 * t43 * t51 / 0.8D1 - (0.90D2 * t8 * t9 *
     # (t302 - t61 * t303) - t311) * t50 * t45 / 0.720D3 - (0.90D2 * t8 
     #* t9 * (-t77 * t302 + t330 + t80 * t303 / 0.2D1) - 0.180D3 * t37 *
     # t38 * (t302 - t77 * t303) + t97 * t309) * t45 / 0.720D3
      t347 = FJET(XB1, XB2, s, t5, -t4, 0.0D0, 0.0D0, 0.0D0, t346)
      t350 = t1 * x1
      t351 = t105 * s * t350
      t353 = t2 * t155 * t24
      t354 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t351, -t353, -
     #t4, 0.0D0, t153)
      t359 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t351, -t353, -
     #t4, 0.0D0, t153)
      t361 = t18 * t24
      t362 = t105 ** 2
      t367 = log(-0.4D1 * t55 * t237 * t361 * t25 * t362)
      t380 = t48 * t354 * t43 * t51 / 0.8D1 - (0.90D2 * t8 * t9 * (-t359
     # + t367 * t354) + 0.180D3 * t37 * t38 * t354) * t50 * t45 / 0.720D
     #3
      t381 = FJET(XB1, XB2, s, -t4, -t351, 0.0D0, -t353, t153, t380)
      t383 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t384 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t390 = t38 * t384
      t392 = 0.180D3 * t37 * t390
      t410 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4,
     # 0.0D0, 0.0D0)
      t427 = -(0.90D2 * t8 * t9 * (t383 - t30 * t384) - t392) * t43 * t4
     #5 / 0.720D3 - t48 * t384 * t43 * t51 / 0.8D1 - (0.90D2 * t8 * t9 *
     # (t383 - t61 * t384) - t392) * t50 * t45 / 0.720D3 - (0.90D2 * t8 
     #* t9 * (t410 - t77 * t383 + t80 * t384 / 0.2D1) - 0.180D3 * t37 * 
     #t38 * (-t77 * t384 + t383) + t97 * t390) * t45 / 0.720D3
      t428 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t5, -t4, 0.0D0, t427)
      t430 = t2 * t108
      t432 = x3 * s * t126
      t433 = t125 * t350
      t434 = t125 * t126
      t435 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t433, t430, t4
     #34, -t432, 0.0D0)
      t441 = log(0.4D1 * t12 * t237 * t361 * t25 * t112)
      t442 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t433, t430, t4
     #34, -t432, 0.0D0)
      t459 = -(0.90D2 * t8 * t9 * (-t435 + t441 * t442) + 0.180D3 * t37 
     #* t38 * t442) * t43 * t45 / 0.720D3 + t48 * t442 * t43 * t51 / 0.8
     #D1
      t460 = FJET(XB1, XB2, s, t430, -t432, -t433, t434, 0.0D0, t459)
      t462 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t124, -t145, -t
     #128, -t148, t153)
      t467 = FJET(XB1, XB2, s, -t148, -t145, -t128, t124, t153, -t159 * 
     #t462 * t192 * t195 / 0.8D1)
      t475 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t433, t430, t4
     #34, -t432, 0.0D0)
      t477 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t433, t430, t4
     #34, -t432, 0.0D0)
      t493 = -(0.90D2 * t8 * t9 * (t441 * t475 - t477) + 0.180D3 * t37 *
     # t38 * t475) * t43 * t45 / 0.720D3 + t48 * t475 * t43 * t51 / 0.8D
     #1
      t494 = FJET(XB1, XB2, s, -t433, t434, t430, -t432, 0.0D0, t493)
      t496 = t2 * t147
      t498 = t2 * t112 * t121
      t499 = t9 * z
      t500 = t8 * t499
      t501 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #498, t496, 0.0D0)
      t503 = Sqrt(-t107 * t113)
      t507 = 0.1D1 / (-z - t106 + 0.2D1 * t110 * t503)
      t508 = t501 * t507
      t512 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #498, t496, 0.0D0)
      t514 = t212 * t20
      t517 = t120 ** 2
      t523 = log(-0.4D1 * t106 * t514 * t15 * t362 * t112 / t517)
      t524 = t523 * z
      t531 = t37 * pi
      t539 = -t500 * t508 * t195 / 0.8D1 - (0.90D2 * t8 * t9 * (z * t512
     # - t524 * t501) * t507 - 0.180D3 * t531 * t499 * t508) * t43 * t50
     # / 0.1440D4
      t540 = FJET(XB1, XB2, s, t496, 0.0D0, t498, 0.0D0, 0.0D0, t539)
      t542 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #498, t496, 0.0D0)
      t543 = t542 * t507
      t547 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #498, t496, 0.0D0)
      t562 = -t500 * t543 * t195 / 0.8D1 - (0.90D2 * t8 * t9 * (z * t547
     # - t524 * t542) * t507 - 0.180D3 * t531 * t499 * t543) * t43 * t50
     # / 0.1440D4
      t563 = FJET(XB1, XB2, s, 0.0D0, t496, 0.0D0, t498, 0.0D0, t562)
      t565 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #498, t496, 0.0D0)
      t566 = t565 * t507
      t570 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #498, t496, 0.0D0)
      t585 = -t500 * t566 * t195 / 0.8D1 - (0.90D2 * t8 * t9 * (z * t570
     # - t524 * t565) * t507 - 0.180D3 * t531 * t499 * t566) * t43 * t50
     # / 0.1440D4
      t586 = FJET(XB1, XB2, s, t498, 0.0D0, t496, 0.0D0, 0.0D0, t585)
      t588 = t103 * t102 - t199 * t7 * t201 * t157 * t160 * t192 * t195 
     #/ 0.8D1 + t274 * t273 - t281 * t7 * t201 * t157 * t276 * t192 * t1
     #95 / 0.8D1 - t294 * t7 * t201 * t157 * t289 * t192 * t195 / 0.8D1 
     #+ t347 * t346 + t381 * t380 + t428 * t427 + t460 * t459 - t467 * t
     #7 * t201 * t157 * t462 * t192 * t195 / 0.8D1 + t494 * t493 + t540 
     #* t539 + t563 * t562 + t586 * t585
      t589 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t433, t430, t4
     #34, -t432, 0.0D0)
      t590 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t433, t430, t4
     #34, -t432, 0.0D0)
      t607 = -(0.90D2 * t8 * t9 * (-t589 + t441 * t590) + 0.180D3 * t37 
     #* t38 * t590) * t43 * t45 / 0.720D3 + t48 * t590 * t43 * t51 / 0.8
     #D1
      t608 = FJET(XB1, XB2, s, -t432, t430, t434, -t433, 0.0D0, t607)
      t610 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t611 = t213 * t20
      t614 = log(0.4D1 * t12 * t611)
      t615 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t621 = t38 * t615
      t623 = 0.180D3 * t37 * t621
      t631 = log(0.4D1 * t56 * t213 * t362)
      t635 = log(0.4D1 * t55 * t611)
      t642 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t645 = log(0.4D1 * t71 * t514)
      t647 = t645 ** 2
      t659 = t97 * t621
      t665 = log(0.4D1 * t236 * t237)
      t666 = t665 ** 2
      t683 = log(0.4D1 * t611)
      t686 = t683 ** 2
      t689 = (0.180D3 * t683 * lh + 0.45D2 * t686 + t93 - t95) * t9
      t693 = rrgq2qgh82J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t708 = (-0.90D2 * t686 * lh + 0.60D2 * lh * t94 - 0.240D3 * zeta3 
     #- 0.120D3 * t92 * lh - 0.15D2 * t686 * t683 - t683 * t96) * t9
      t715 = (-0.180D3 * lh - 0.90D2 * t683) * t9
      t721 = log(0.4D1 * t106 * t611)
      t723 = t237 * t362
      t726 = log(0.4D1 * t260 * t723)
      t734 = x2 * t212
      t737 = log(0.4D1 * t734 * t237)
      t738 = t737 ** 2
      t741 = log(0.4D1 * t734 * t723)
      t742 = t741 ** 2
      t744 = -t738 / 0.2D1 + t742 / 0.2D1
      t752 = t737 - t741
      t757 = -(0.90D2 * t8 * t9 * (-t610 + t614 * t615) + t623) * t43 * 
     #t45 / 0.720D3 - t48 * (-t631 * t615 + t635 * t615) * t50 * t45 / 0
     #.8D1 - (0.90D2 * t8 * t9 * (-t642 + t645 * t610 - t647 * t615 / 0.
     #2D1) - 0.180D3 * t37 * t38 * (-t610 + t645 * t615) - t659) * t45 /
     # 0.720D3 - (0.90D2 * t8 * t9 * (-t642 - t666 * t615 / 0.2D1 + t665
     # * t610) - 0.180D3 * t37 * t38 * (-t610 + t665 * t615) - t659) * t
     #43 / 0.1440D4 + t689 * t8 * t610 / 0.1440D4 + t8 * t9 * t693 / 0.1
     #6D2 + t708 * t8 * t615 / 0.1440D4 + t715 * t8 * t642 / 0.1440D4 - 
     #t48 * (t721 * t615 - t726 * t615) * t43 * t50 / 0.16D2 - (0.90D2 *
     # t8 * t9 * t615 * t744 + (0.90D2 * t8 * t9 * t610 - t623) * t752) 
     #* t50 / 0.1440D4
      t758 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t757)
      t760 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t761 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t767 = t38 * t761
      t769 = 0.180D3 * t37 * t767
      t782 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t794 = t97 * t767
      t816 = rrgq2qgh84J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t845 = -(0.90D2 * t8 * t9 * (-t760 + t614 * t761) + t769) * t43 * 
     #t45 / 0.720D3 - t48 * (-t631 * t761 + t635 * t761) * t50 * t45 / 0
     #.8D1 - (0.90D2 * t8 * t9 * (t645 * t760 - t782 - t647 * t761 / 0.2
     #D1) - 0.180D3 * t37 * t38 * (-t760 + t645 * t761) - t794) * t45 / 
     #0.720D3 - (0.90D2 * t8 * t9 * (-t782 + t665 * t760 - t666 * t761 /
     # 0.2D1) - 0.180D3 * t37 * t38 * (-t760 + t665 * t761) - t794) * t4
     #3 / 0.1440D4 + t689 * t8 * t760 / 0.1440D4 + t8 * t9 * t816 / 0.16
     #D2 + t708 * t8 * t761 / 0.1440D4 + t715 * t8 * t782 / 0.1440D4 - t
     #48 * (-t726 * t761 + t721 * t761) * t43 * t50 / 0.16D2 - (0.90D2 *
     # t8 * t9 * t761 * t744 + (0.90D2 * t8 * t9 * t760 - t769) * t752) 
     #* t50 / 0.1440D4
      t846 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t845)
      t848 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t850 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t855 = t38 * t848
      t857 = 0.180D3 * t37 * t855
      t870 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t882 = t97 * t855
      t889 = rrgq2qgh83J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t933 = -(0.90D2 * t8 * t9 * (t614 * t848 - t850) + t857) * t43 * t
     #45 / 0.720D3 - t48 * (t635 * t848 - t631 * t848) * t50 * t45 / 0.8
     #D1 - (0.90D2 * t8 * t9 * (t645 * t850 - t870 - t647 * t848 / 0.2D1
     #) - 0.180D3 * t37 * t38 * (-t850 + t645 * t848) - t882) * t45 / 0.
     #720D3 + t689 * t8 * t850 / 0.1440D4 + t8 * t9 * t889 / 0.16D2 - (0
     #.90D2 * t8 * t9 * (-t870 + t665 * t850 - t666 * t848 / 0.2D1) - 0.
     #180D3 * t37 * t38 * (t665 * t848 - t850) - t882) * t43 / 0.1440D4 
     #+ t715 * t8 * t870 / 0.1440D4 + t708 * t8 * t848 / 0.1440D4 - t48 
     #* (-t726 * t848 + t721 * t848) * t43 * t50 / 0.16D2 - (0.90D2 * t8
     # * t9 * t848 * t744 + (0.90D2 * t8 * t9 * t850 - t857) * t752) * t
     #50 / 0.1440D4
      t934 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t933)
      t936 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t433, t430, t4
     #34, -t432, 0.0D0)
      t938 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, -t433, t430, t4
     #34, -t432, 0.0D0)
      t954 = -(0.90D2 * t8 * t9 * (t441 * t936 - t938) + 0.180D3 * t37 *
     # t38 * t936) * t43 * t45 / 0.720D3 + t48 * t936 * t43 * t51 / 0.8D
     #1
      t955 = FJET(XB1, XB2, s, t434, -t433, -t432, t430, 0.0D0, t954)
      t957 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t208, t209, 0.0D0)
      t959 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t208, t209, 0.0D0)
      t964 = t38 * t957
      t966 = 0.180D3 * t37 * t964
      t976 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t208, t209, 0.0D0)
      t1001 = -(0.90D2 * t8 * t9 * (-t217 * t957 + t959) - t966) * t43 *
     # t45 / 0.720D3 - t48 * t957 * t43 * t51 / 0.8D1 - (0.90D2 * t8 * t
     #9 * (-t241 * t959 + t976 + t243 * t957 / 0.2D1) - 0.180D3 * t37 * 
     #t38 * (-t241 * t957 + t959) + t97 * t964) * t43 / 0.1440D4 - (0.90
     #D2 * t8 * t9 * (t959 - t263 * t957) - t966) * t43 * t50 / 0.1440D4
      t1002 = FJET(XB1, XB2, s, t209, 0.0D0, -t208, 0.0D0, 0.0D0, t1001)
      t1004 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t351, -t353, 
     #-t4, 0.0D0, t153)
      t1009 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, -t351, -t353, 
     #-t4, 0.0D0, t153)
      t1022 = t48 * t1004 * t43 * t51 / 0.8D1 - (0.90D2 * t8 * t9 * (-t1
     #009 + t367 * t1004) + 0.180D3 * t37 * t38 * t1004) * t50 * t45 / 0
     #.720D3
      t1023 = FJET(XB1, XB2, s, -t351, -t4, -t353, 0.0D0, t153, t1022)
      t1025 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t351, -t353, 
     #-t4, 0.0D0, t153)
      t1030 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, -t351, -t353, 
     #-t4, 0.0D0, t153)
      t1043 = t48 * t1025 * t43 * t51 / 0.8D1 - (0.90D2 * t8 * t9 * (-t1
     #030 + t367 * t1025) + 0.180D3 * t37 * t38 * t1025) * t50 * t45 / 0
     #.720D3
      t1044 = FJET(XB1, XB2, s, -t353, 0.0D0, -t351, -t4, t153, t1043)
      t1046 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1047 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1053 = t38 * t1047
      t1055 = 0.180D3 * t37 * t1053
      t1070 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1080 = t97 * t1053
      t1084 = rrgq2qgh81J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t2, 0.0D0, 0.0D0)
      t1131 = -(0.90D2 * t8 * t9 * (-t1046 + t614 * t1047) + t1055) * t4
     #3 * t45 / 0.720D3 - t48 * (-t631 * t1047 + t635 * t1047) * t50 * t
     #45 / 0.8D1 - (0.90D2 * t8 * t9 * (-t647 * t1047 / 0.2D1 + t645 * t
     #1046 - t1070) - 0.180D3 * t37 * t38 * (-t1046 + t645 * t1047) - t1
     #080) * t45 / 0.720D3 + t8 * t9 * t1084 / 0.16D2 + t708 * t8 * t104
     #7 / 0.1440D4 - (0.90D2 * t8 * t9 * (-t666 * t1047 / 0.2D1 + t665 *
     # t1046 - t1070) - 0.180D3 * t37 * t38 * (-t1046 + t665 * t1047) - 
     #t1080) * t43 / 0.1440D4 + t689 * t8 * t1046 / 0.1440D4 + t715 * t8
     # * t1070 / 0.1440D4 - t48 * (-t726 * t1047 + t721 * t1047) * t43 *
     # t50 / 0.16D2 - (0.90D2 * t8 * t9 * t1047 * t744 + (0.90D2 * t8 * 
     #t9 * t1046 - t1055) * t752) * t50 / 0.1440D4
      t1132 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t1131)
      t1134 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t208, t209, 0.0D0)
      t1135 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t208, t209, 0.0D0)
      t1141 = t38 * t1135
      t1143 = 0.180D3 * t37 * t1141
      t1155 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t208, t209, 0.0D0)
      t1178 = -(0.90D2 * t8 * t9 * (t1134 - t217 * t1135) - t1143) * t43
     # * t45 / 0.720D3 - t48 * t1135 * t43 * t51 / 0.8D1 - (0.90D2 * t8 
     #* t9 * (-t241 * t1134 + t243 * t1135 / 0.2D1 + t1155) - 0.180D3 * 
     #t37 * t38 * (-t241 * t1135 + t1134) + t97 * t1141) * t43 / 0.1440D
     #4 - (0.90D2 * t8 * t9 * (-t263 * t1135 + t1134) - t1143) * t43 * t
     #50 / 0.1440D4
      t1179 = FJET(XB1, XB2, s, 0.0D0, t209, 0.0D0, -t208, 0.0D0, t1178)
      t1181 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t208, t209, 0.0D0)
      t1183 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t208, t209, 0.0D0)
      t1188 = t38 * t1181
      t1190 = 0.180D3 * t37 * t1188
      t1200 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #-t208, t209, 0.0D0)
      t1225 = -(0.90D2 * t8 * t9 * (-t217 * t1181 + t1183) - t1190) * t4
     #3 * t45 / 0.720D3 - t48 * t1181 * t43 * t51 / 0.8D1 - (0.90D2 * t8
     # * t9 * (-t241 * t1183 + t1200 + t243 * t1181 / 0.2D1) - 0.180D3 *
     # t37 * t38 * (-t241 * t1181 + t1183) + t97 * t1188) * t43 / 0.1440
     #D4 - (0.90D2 * t8 * t9 * (t1183 - t263 * t1181) - t1190) * t43 * t
     #50 / 0.1440D4
      t1226 = FJET(XB1, XB2, s, -t208, 0.0D0, t209, 0.0D0, 0.0D0, t1225)
      t1228 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t351, -t353, 
     #-t4, 0.0D0, t153)
      t1234 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, -t351, -t353, 
     #-t4, 0.0D0, t153)
      t1246 = t48 * t1228 * t43 * t51 / 0.8D1 - (0.90D2 * t8 * t9 * (t36
     #7 * t1228 - t1234) + 0.180D3 * t37 * t38 * t1228) * t50 * t45 / 0.
     #720D3
      t1247 = FJET(XB1, XB2, s, 0.0D0, -t353, -t4, -t351, t153, t1246)
      t1249 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t498, t496, 0.0D0)
      t1250 = t1249 * t507
      t1254 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t498, t496, 0.0D0)
      t1269 = -t500 * t1250 * t195 / 0.8D1 - (0.90D2 * t8 * t9 * (z * t1
     #254 - t524 * t1249) * t507 - 0.180D3 * t531 * t499 * t1250) * t43 
     #* t50 / 0.1440D4
      t1270 = FJET(XB1, XB2, s, 0.0D0, t498, 0.0D0, t496, 0.0D0, t1269)
      t1272 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4
     #, 0.0D0, 0.0D0)
      t1273 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4
     #, 0.0D0, 0.0D0)
      t1279 = t38 * t1273
      t1281 = 0.180D3 * t37 * t1279
      t1299 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, t5, 0.0D0, -t4
     #, 0.0D0, 0.0D0)
      t1316 = -(0.90D2 * t8 * t9 * (t1272 - t30 * t1273) - t1281) * t43 
     #* t45 / 0.720D3 - t48 * t1273 * t43 * t51 / 0.8D1 - (0.90D2 * t8 *
     # t9 * (t1272 - t61 * t1273) - t1281) * t50 * t45 / 0.720D3 - (0.90
     #D2 * t8 * t9 * (t1299 + t80 * t1273 / 0.2D1 - t77 * t1272) - 0.180
     #D3 * t37 * t38 * (-t77 * t1273 + t1272) + t97 * t1279) * t45 / 0.7
     #20D3
      t1317 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t4, t5, 0.0D0, t1316)
      t1319 = t608 * t607 + t758 * t757 + t846 * t845 + t934 * t933 + t9
     #55 * t954 + t1002 * t1001 + t1023 * t1022 + t1044 * t1043 + t1132 
     #* t1131 + t1179 * t1178 + t1226 * t1225 + t1247 * t1246 + t1270 * 
     #t1269 + t1317 * t1316
      rrgq2qght8s4e0 = t588 + t1319

      end function



      doubleprecision function rrgq2qght8s4em1
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t8 = z ** 2
      t10 = 0.1D1 / t8 / z
      t11 = x3 * t10
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t20 = log(0.4D1 * t11 * t17)
      t21 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t27 = lh * t4
      t28 = pi * t6
      t31 = 0.180D3 * t27 * t28 * t21
      t33 = 0.1D1 / x3
      t36 = x1 ** 2
      t37 = t36 * t14
      t41 = log(0.4D1 * t37 * t10 * t16)
      t48 = 0.1D1 / x1
      t51 = t5 * t6
      t56 = x2 * t10
      t59 = log(0.4D1 * t56 * t17)
      t60 = -0.1D1 + x2
      t61 = t60 ** 2
      t65 = log(0.4D1 * t56 * t17 * t61)
      t66 = t59 - t65
      t68 = 0.1D1 / x2
      t72 = rrgq2qgh82J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t80 = log(0.4D1 * t10 * t14 * t16)
      t83 = (-0.180D3 * lh - 0.90D2 * t80) * t6
      t89 = t80 ** 2
      t91 = lh ** 2
      t93 = pi ** 2
      t96 = (0.180D3 * t80 * lh + 0.45D2 * t89 + 0.180D3 * t91 - 0.30D2 
     #* t93) * t6
      t100 = -(0.90D2 * t5 * t6 * (-t7 + t20 * t21) + t31) * t33 / 0.144
     #0D4 - (0.90D2 * t5 * t6 * (-t7 + t41 * t21) + t31) * t48 / 0.720D3
     # + t51 * t21 * t33 * t48 / 0.8D1 - t51 * t21 * t66 * t68 / 0.16D2 
     #+ t5 * t6 * t72 / 0.16D2 + t83 * t5 * t7 / 0.1440D4 + t96 * t5 * t
     #21 / 0.1440D4
      t101 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t100)
      t103 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t104 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t112 = 0.180D3 * t27 * t28 * t104
      t132 = rrgq2qgh84J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t142 = -(0.90D2 * t5 * t6 * (-t103 + t20 * t104) + t112) * t33 / 0
     #.1440D4 - (0.90D2 * t5 * t6 * (-t103 + t41 * t104) + t112) * t48 /
     # 0.720D3 + t51 * t104 * t33 * t48 / 0.8D1 - t51 * t104 * t66 * t68
     # / 0.16D2 + t5 * t6 * t132 / 0.16D2 + t83 * t5 * t103 / 0.1440D4 +
     # t96 * t5 * t104 / 0.1440D4
      t143 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t142)
      t145 = t2 * x1
      t146 = -0.1D1 + x1
      t147 = t2 * t146
      t148 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t145, 0.0D0, -t
     #147, 0.0D0, 0.0D0)
      t157 = 0.1D1 / (-z - x1 + x1 * z)
      t159 = t146 ** 2
      t163 = log(-0.4D1 * t37 / t8 * t16 * t157 * t159)
      t165 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, t145, 0.0D0, -t
     #147, 0.0D0, 0.0D0)
      t180 = -t51 * t148 * t68 * t48 / 0.8D1 - (0.90D2 * t5 * t6 * (-t16
     #3 * t148 + t165) - 0.180D3 * t27 * t28 * t148) * t48 / 0.720D3 - t
     #51 * t148 * t33 * t48 / 0.8D1
      t181 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t145, -t147, 0.0D0, t180)
      t183 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t145, 0.0D0, -t
     #147, 0.0D0, 0.0D0)
      t189 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, t145, 0.0D0, -t
     #147, 0.0D0, 0.0D0)
      t204 = -t51 * t183 * t68 * t48 / 0.8D1 - (0.90D2 * t5 * t6 * (-t16
     #3 * t183 + t189) - 0.180D3 * t27 * t28 * t183) * t48 / 0.720D3 - t
     #51 * t183 * t33 * t48 / 0.8D1
      t205 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t147, t145, 0.0D0, t204)
      t207 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t208 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t216 = 0.180D3 * t27 * t28 * t208
      t242 = rrgq2qgh81J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t246 = -(0.90D2 * t5 * t6 * (-t207 + t20 * t208) + t216) * t33 / 0
     #.1440D4 - (0.90D2 * t5 * t6 * (-t207 + t41 * t208) + t216) * t48 /
     # 0.720D3 + t51 * t208 * t33 * t48 / 0.8D1 + t83 * t5 * t207 / 0.14
     #40D4 + t96 * t5 * t208 / 0.1440D4 - t51 * t208 * t66 * t68 / 0.16D
     #2 + t5 * t6 * t242 / 0.16D2
      t247 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t246)
      t249 = t2 * x3
      t250 = -0.1D1 + x3
      t251 = t2 * t250
      t252 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t251, t249, 0.0D0)
      t253 = t252 * t33
      t260 = log(-0.4D1 * t11 * t17 * t250)
      t262 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t251, t249, 0.0D0)
      t276 = -t51 * t253 * t48 / 0.8D1 - (0.90D2 * t5 * t6 * (-t260 * t2
     #52 + t262) - 0.180D3 * t27 * t28 * t252) * t33 / 0.1440D4 - t51 * 
     #t253 * t68 / 0.16D2
      t277 = FJET(XB1, XB2, s, 0.0D0, t249, 0.0D0, -t251, 0.0D0, t276)
      t279 = x2 * x3
      t281 = 0.1D1 / (-0.1D1 + t279)
      t283 = t2 * t250 * t281
      t286 = t2 * x3 * t60 * t281
      t288 = t5 * t6 * z
      t289 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #283, t286, 0.0D0)
      t290 = cos(t12)
      t294 = Sqrt(-x3 * z * x2 * t250)
      t298 = 0.1D1 / (-z - t279 + 0.2D1 * t290 * t294)
      t300 = t33 * t68
      t304 = FJET(XB1, XB2, s, 0.0D0, t283, 0.0D0, t286, 0.0D0, -t288 * 
     #t289 * t298 * t300 / 0.16D2)
      t309 = t298 * t33 * t68
      t313 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #283, t286, 0.0D0)
      t318 = FJET(XB1, XB2, s, 0.0D0, t286, 0.0D0, t283, 0.0D0, -t288 * 
     #t313 * t298 * t300 / 0.16D2)
      t325 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t251, t249, 0.0D0)
      t327 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t251, t249, 0.0D0)
      t338 = t325 * t33
      t345 = -(0.90D2 * t5 * t6 * (-t260 * t325 + t327) - 0.180D3 * t27 
     #* t28 * t325) * t33 / 0.1440D4 - t51 * t338 * t68 / 0.16D2 - t51 *
     # t338 * t48 / 0.8D1
      t346 = FJET(XB1, XB2, s, 0.0D0, -t251, 0.0D0, t249, 0.0D0, t345)
      t350 = t2 * x1 * x2 * t157
      t352 = t1 * x1
      t353 = t60 * s * t352
      t358 = s * t15 * x2 * x1 * t146 * t157
      t359 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t353, -t350, -
     #t147, 0.0D0, t358)
      t364 = FJET(XB1, XB2, s, 0.0D0, -t350, -t147, -t353, t358, t51 * t
     #359 * t68 * t48 / 0.8D1)
      t368 = t68 * t48
      t372 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t374 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t381 = 0.180D3 * t27 * t28 * t372
      t400 = rrgq2qgh83J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t411 = -(0.90D2 * t5 * t6 * (t20 * t372 - t374) + t381) * t33 / 0.
     #1440D4 + t83 * t5 * t374 / 0.1440D4 - (0.90D2 * t5 * t6 * (-t374 +
     # t41 * t372) + t381) * t48 / 0.720D3 + t51 * t372 * t33 * t48 / 0.
     #8D1 + t5 * t6 * t400 / 0.16D2 + t96 * t5 * t372 / 0.1440D4 - t51 *
     # t372 * t66 * t68 / 0.16D2
      t412 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t411)
      t414 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t145, 0.0D0, -t
     #147, 0.0D0, 0.0D0)
      t419 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, t145, 0.0D0, -t
     #147, 0.0D0, 0.0D0)
      t435 = -t51 * t414 * t68 * t48 / 0.8D1 - (0.90D2 * t5 * t6 * (t419
     # - t163 * t414) - 0.180D3 * t27 * t28 * t414) * t48 / 0.720D3 - t5
     #1 * t414 * t33 * t48 / 0.8D1
      t436 = FJET(XB1, XB2, s, t145, -t147, 0.0D0, 0.0D0, 0.0D0, t435)
      t438 = t101 * t100 + t143 * t142 + t181 * t180 + t205 * t204 + t24
     #7 * t246 + t277 * t276 - t304 * t4 * t28 * z * t289 * t309 / 0.16D
     #2 - t318 * t4 * t28 * z * t313 * t309 / 0.16D2 + t346 * t345 + t36
     #4 * t4 * pi * t6 * t359 * t368 / 0.8D1 + t412 * t411 + t436 * t435
      t439 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t251, t249, 0.0D0)
      t441 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t251, t249, 0.0D0)
      t452 = t439 * t33
      t459 = -(0.90D2 * t5 * t6 * (-t260 * t439 + t441) - 0.180D3 * t27 
     #* t28 * t439) * t33 / 0.1440D4 - t51 * t452 * t68 / 0.16D2 - t51 *
     # t452 * t48 / 0.8D1
      t460 = FJET(XB1, XB2, s, t249, 0.0D0, -t251, 0.0D0, 0.0D0, t459)
      t463 = t2 * x1 * x3
      t465 = t1 * t146
      t466 = x3 * s * t465
      t467 = t250 * s
      t468 = t467 * t352
      t469 = t467 * t465
      t470 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t468, t463, t4
     #69, -t466, 0.0D0)
      t475 = FJET(XB1, XB2, s, t463, -t466, -t468, t469, 0.0D0, t51 * t4
     #70 * t33 * t48 / 0.8D1)
      t479 = t33 * t48
      t483 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #283, t286, 0.0D0)
      t488 = FJET(XB1, XB2, s, t283, 0.0D0, t286, 0.0D0, 0.0D0, -t288 * 
     #t483 * t298 * t300 / 0.16D2)
      t495 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t468, t463, t4
     #69, -t466, 0.0D0)
      t500 = FJET(XB1, XB2, s, t469, -t468, -t466, t463, 0.0D0, t51 * t4
     #95 * t33 * t48 / 0.8D1)
      t507 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #283, t286, 0.0D0)
      t512 = FJET(XB1, XB2, s, t286, 0.0D0, t283, 0.0D0, 0.0D0, -t288 * 
     #t507 * t298 * t300 / 0.16D2)
      t519 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t145, 0.0D0, -t
     #147, 0.0D0, 0.0D0)
      t524 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, t145, 0.0D0, -t
     #147, 0.0D0, 0.0D0)
      t540 = -t51 * t519 * t68 * t48 / 0.8D1 - (0.90D2 * t5 * t6 * (t524
     # - t163 * t519) - 0.180D3 * t27 * t28 * t519) * t48 / 0.720D3 - t5
     #1 * t519 * t33 * t48 / 0.8D1
      t541 = FJET(XB1, XB2, s, -t147, t145, 0.0D0, 0.0D0, 0.0D0, t540)
      t543 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, -t353, -t350, -
     #t147, 0.0D0, t358)
      t548 = FJET(XB1, XB2, s, -t147, -t353, 0.0D0, -t350, t358, t51 * t
     #543 * t68 * t48 / 0.8D1)
      t555 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t251, t249, 0.0D0)
      t557 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t251, t249, 0.0D0)
      t568 = t555 * t33
      t575 = -(0.90D2 * t5 * t6 * (-t260 * t555 + t557) - 0.180D3 * t27 
     #* t28 * t555) * t33 / 0.1440D4 - t51 * t568 * t68 / 0.16D2 - t51 *
     # t568 * t48 / 0.8D1
      t576 = FJET(XB1, XB2, s, -t251, 0.0D0, t249, 0.0D0, 0.0D0, t575)
      t578 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, -t468, t463, t4
     #69, -t466, 0.0D0)
      t583 = FJET(XB1, XB2, s, -t466, t463, t469, -t468, 0.0D0, t51 * t5
     #78 * t33 * t48 / 0.8D1)
      t590 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t353, -t350, -
     #t147, 0.0D0, t358)
      t595 = FJET(XB1, XB2, s, -t353, -t147, -t350, 0.0D0, t358, t51 * t
     #590 * t68 * t48 / 0.8D1)
      t602 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, -t468, t463, t4
     #69, -t466, 0.0D0)
      t607 = FJET(XB1, XB2, s, -t468, t469, t463, -t466, 0.0D0, t51 * t6
     #02 * t33 * t48 / 0.8D1)
      t614 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, -t353, -t350, -
     #t147, 0.0D0, t358)
      t619 = FJET(XB1, XB2, s, -t350, 0.0D0, -t353, -t147, t358, t51 * t
     #614 * t68 * t48 / 0.8D1)
      t626 = t460 * t459 + t475 * t4 * pi * t6 * t470 * t479 / 0.8D1 - t
     #488 * t4 * t28 * z * t483 * t309 / 0.16D2 + t500 * t4 * pi * t6 * 
     #t495 * t479 / 0.8D1 - t512 * t4 * t28 * z * t507 * t309 / 0.16D2 +
     # t541 * t540 + t548 * t4 * pi * t6 * t543 * t368 / 0.8D1 + t576 * 
     #t575 + t583 * t4 * pi * t6 * t578 * t479 / 0.8D1 + t595 * t4 * pi 
     #* t6 * t590 * t368 / 0.8D1 + t607 * t4 * pi * t6 * t602 * t479 / 0
     #.8D1 + t619 * t4 * pi * t6 * t614 * t368 / 0.8D1
      rrgq2qght8s4em1 = t438 + t626

      end function



      doubleprecision function rrgq2qght8s4em2
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t8 = t6 * t7
      t9 = 0.1D1 / x1
      t13 = 0.1D1 / x3
      t17 = rrgq2qgh82J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t22 = z ** 2
      t26 = Sin(x4 * pi)
      t27 = t26 ** 2
      t29 = t1 ** 2
      t30 = t29 ** 2
      t33 = log(0.4D1 / t22 / z * t27 * t30)
      t36 = (-0.180D3 * lh - 0.90D2 * t33) * t6
      t40 = t5 * t8 * t9 / 0.8D1 + t5 * t8 * t13 / 0.16D2 + t5 * t6 * t1
     #7 / 0.16D2 + t36 * t5 * t7 / 0.1440D4
      t41 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t40)
      t43 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t44 = t6 * t43
      t51 = rrgq2qgh84J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t58 = t5 * t44 * t9 / 0.8D1 + t5 * t44 * t13 / 0.16D2 + t5 * t6 * 
     #t51 / 0.16D2 + t36 * t5 * t43 / 0.1440D4
      t59 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t58)
      t61 = t2 * x1
      t63 = t2 * (-0.1D1 + x1)
      t64 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, t61, 0.0D0, -t63
     #, 0.0D0, 0.0D0)
      t66 = t6 * t64 * t9
      t69 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t61, -t63, 0.0D0, -t5 * t66 
     #/ 0.8D1)
      t74 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, t61, 0.0D0, -t63
     #, 0.0D0, 0.0D0)
      t76 = t6 * t74 * t9
      t79 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t63, t61, 0.0D0, -t5 * t76 
     #/ 0.8D1)
      t84 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t85 = t6 * t84
      t92 = rrgq2qgh81J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t99 = t5 * t85 * t9 / 0.8D1 + t5 * t85 * t13 / 0.16D2 + t5 * t6 * 
     #t92 / 0.16D2 + t36 * t5 * t84 / 0.1440D4
      t100 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t99)
      t102 = t2 * x3
      t104 = t2 * (-0.1D1 + x3)
      t105 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t104, t102, 0.0D0)
      t107 = t6 * t105 * t13
      t110 = FJET(XB1, XB2, s, 0.0D0, t102, 0.0D0, -t104, 0.0D0, -t5 * t
     #107 / 0.16D2)
      t115 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t104, t102, 0.0D0)
      t117 = t6 * t115 * t13
      t120 = FJET(XB1, XB2, s, 0.0D0, -t104, 0.0D0, t102, 0.0D0, -t5 * t
     #117 / 0.16D2)
      t125 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t126 = t6 * t125
      t133 = rrgq2qgh83J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t140 = t5 * t126 * t9 / 0.8D1 + t5 * t126 * t13 / 0.16D2 + t5 * t6
     # * t133 / 0.16D2 + t36 * t5 * t125 / 0.1440D4
      t141 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t140)
      t143 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, t61, 0.0D0, -t6
     #3, 0.0D0, 0.0D0)
      t145 = t6 * t143 * t9
      t148 = FJET(XB1, XB2, s, t61, -t63, 0.0D0, 0.0D0, 0.0D0, -t5 * t14
     #5 / 0.8D1)
      t153 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t104, t102, 0.0D0)
      t155 = t6 * t153 * t13
      t158 = FJET(XB1, XB2, s, t102, 0.0D0, -t104, 0.0D0, 0.0D0, -t5 * t
     #155 / 0.16D2)
      t163 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, t61, 0.0D0, -t6
     #3, 0.0D0, 0.0D0)
      t165 = t6 * t163 * t9
      t168 = FJET(XB1, XB2, s, -t63, t61, 0.0D0, 0.0D0, 0.0D0, -t5 * t16
     #5 / 0.8D1)
      t173 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t104, t102, 0.0D0)
      t175 = t6 * t173 * t13
      t178 = FJET(XB1, XB2, s, -t104, 0.0D0, t102, 0.0D0, 0.0D0, -t5 * t
     #175 / 0.16D2)
      rrgq2qght8s4em2 = t41 * t40 + t59 * t58 - t69 * t4 * pi * t66 / 0.
     #8D1 - t79 * t4 * pi * t76 / 0.8D1 + t100 * t99 - t110 * t4 * pi * 
     #t107 / 0.16D2 - t120 * t4 * pi * t117 / 0.16D2 + t141 * t140 - t14
     #8 * t4 * pi * t145 / 0.8D1 - t158 * t4 * pi * t155 / 0.16D2 - t168
     # * t4 * pi * t165 / 0.8D1 - t178 * t4 * pi * t175 / 0.16D2

      end function



      doubleprecision function rrgq2qght8s4em3
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

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
      t3 = s ** 2
      t4 = 0.1D1 / t3
      t5 = t4 * pi
      t6 = 0.1D1 / t1
      t7 = rrgq2qgh82J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t11 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t5 * t6 * 
     #t7 / 0.16D2)
      t13 = pi * t6
      t16 = rrgq2qgh84J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t20 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t5 * t6 * 
     #t16 / 0.16D2)
      t24 = rrgq2qgh81J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t28 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t5 * t6 * 
     #t24 / 0.16D2)
      t32 = rrgq2qgh83J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t5 * t6 * 
     #t32 / 0.16D2)
      rrgq2qght8s4em3 = t11 * t4 * t13 * t7 / 0.16D2 + t20 * t4 * t13 * 
     #t16 / 0.16D2 + t28 * t4 * t13 * t24 / 0.16D2 + t36 * t4 * t13 * t3
     #2 / 0.16D2

      end function



      doubleprecision function rrgq2qght8s4em4
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
      doubleprecision rrgq2qgh81J1
      doubleprecision rrgq2qgh81J2
      doubleprecision rrgq2qgh81J3
      doubleprecision rrgq2qgh81J4
      doubleprecision rrgq2qgh81J5
      doubleprecision rrgq2qgh81J6
      doubleprecision rrgq2qgh81J7
      doubleprecision rrgq2qgh82J1
      doubleprecision rrgq2qgh82J2
      doubleprecision rrgq2qgh82J3
      doubleprecision rrgq2qgh82J4
      doubleprecision rrgq2qgh82J5
      doubleprecision rrgq2qgh82J6
      doubleprecision rrgq2qgh83J1
      doubleprecision rrgq2qgh83J2
      doubleprecision rrgq2qgh83J3
      doubleprecision rrgq2qgh83J4
      doubleprecision rrgq2qgh83J5
      doubleprecision rrgq2qgh83J6
      doubleprecision rrgq2qgh84J1
      doubleprecision rrgq2qgh84J2
      doubleprecision rrgq2qgh84J3
      doubleprecision rrgq2qgh84J4
      doubleprecision rrgq2qgh84J5
      doubleprecision rrgq2qgh84J6
      doubleprecision rrgq2qgh84J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgq2qght8s4em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgq2qgh81J1
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
      t5 = S13 ** 2
      t7 = S14 ** 2
      rrgq2qgh81J1 = (0.8D1 / 0.9D1 * S12 * S13 + 0.8D1 / 0.9D1 * S13 * 
     #S14 + (0.8D1 / 0.9D1 * t5 * S13 + 0.8D1 / 0.9D1 * t7 * S13 + 0.8D1
     # / 0.9D1 * S14 * t5) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh81J2
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
      t3 = S13 ** 2
      t5 = S14 ** 2
      rrgq2qgh81J2 = (-0.8D1 / 0.9D1 * S13 * S14 - 0.16D2 / 0.9D1 * t3 +
     # (-0.8D1 / 0.9D1 * t5 * S13 - 0.8D1 / 0.9D1 * S14 * t3 - 0.4D1 / 0
     #.9D1 * S14 * S24 * S23) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh81J3
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
      t1 = S24 * S14
      t3 = 0.1D1 / (S12 + S13 + S23)
      t9 = S13 ** 2
      t12 = S14 ** 2
      t13 = t12 * S24
      t22 = S24 ** 2
      rrgq2qgh81J3 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 - 0.8D1 / 0.9D1 * S
     #13 * S14 - 0.16D2 / 0.9D1 * t9 + 0.4D1 / 0.9D1 * t1 - 0.4D1 / 0.9D
     #1 * t13 * t3 + (-0.8D1 / 0.9D1 * t12 * S13 - 0.8D1 / 0.9D1 * S14 *
     # t9 - 0.8D1 / 0.9D1 * t1 * S23 + 0.4D1 / 0.9D1 * S14 * t22 + 0.8D1
     # / 0.9D1 * t13) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh81J4
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
      t1 = S24 * S14
      t3 = 0.1D1 / (S12 + S13 + S23)
      t9 = S13 ** 2
      t12 = S14 ** 2
      t13 = t12 * S24
      t22 = S24 ** 2
      rrgq2qgh81J4 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 - 0.8D1 / 0.9D1 * S
     #13 * S14 - 0.16D2 / 0.9D1 * t9 + 0.4D1 / 0.9D1 * t1 - 0.4D1 / 0.9D
     #1 * t13 * t3 + (-0.8D1 / 0.9D1 * t12 * S13 - 0.8D1 / 0.9D1 * S14 *
     # t9 - 0.8D1 / 0.9D1 * t1 * S23 + 0.4D1 / 0.9D1 * S14 * t22 + 0.8D1
     # / 0.9D1 * t13) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh81J5
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
      t1 = S24 * S14
      t3 = 0.1D1 / (S12 + S13 + S23)
      t9 = S13 ** 2
      t12 = S14 ** 2
      t13 = t12 * S24
      t22 = S24 ** 2
      rrgq2qgh81J5 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 - 0.8D1 / 0.9D1 * S
     #13 * S14 - 0.16D2 / 0.9D1 * t9 + 0.4D1 / 0.9D1 * t1 - 0.4D1 / 0.9D
     #1 * t13 * t3 + (-0.8D1 / 0.9D1 * t12 * S13 - 0.8D1 / 0.9D1 * S14 *
     # t9 - 0.8D1 / 0.9D1 * t1 * S23 + 0.4D1 / 0.9D1 * S14 * t22 + 0.8D1
     # / 0.9D1 * t13) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh81J6
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
      t2 = S24 * S14
      t4 = 0.1D1 / (S12 + S13 + S23)
      t9 = S13 ** 2
      t14 = S14 ** 2
      t15 = t14 * S24
      t27 = S24 ** 2
      rrgq2qgh81J6 = ((-0.8D1 / 0.9D1 * S13 - 0.4D1 / 0.9D1 * t2 * t4) *
     # S12 - 0.16D2 / 0.9D1 * t9 + 0.4D1 / 0.9D1 * t2 - 0.16D2 / 0.9D1 *
     # S13 * S14 - 0.4D1 / 0.9D1 * t15 * t4 + (-0.8D1 / 0.9D1 * t9 * S13
     # - 0.16D2 / 0.9D1 * S14 * t9 + 0.8D1 / 0.9D1 * t15 - 0.8D1 / 0.9D1
     # * t2 * S23 - 0.16D2 / 0.9D1 * t14 * S13 + 0.4D1 / 0.9D1 * S14 * t
     #27) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh81J7
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
      t1 = S24 * S14
      t3 = 0.1D1 / (S12 + S13 + S23)
      t8 = S14 ** 2
      t9 = t8 * S24
      t12 = S24 ** 2
      rrgq2qgh81J7 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 + 0.4D1 / 0.9D1 * t
     #1 - 0.4D1 / 0.9D1 * t9 * t3 + (0.4D1 / 0.9D1 * S14 * t12 - 0.4D1 /
     # 0.9D1 * t1 * S23 + 0.8D1 / 0.9D1 * t9) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh82J1
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t4 = S12 ** 2
      t7 = S13 * S14
      t9 = S13 ** 2
      t10 = 0.2D1 * t9
      t16 = S14 ** 2
      t17 = t16 * S13
      t19 = S14 * t9
      t21 = t9 * S13
      t22 = 0.2D1 * t21
      t25 = t16 * S14
      rrgq2qgh82J1 = (-S13 * t2 * t4 + (-0.4D1 * S14 + S13 + (0.3D1 * t7
     # + t10) * t2) * S12 - t10 - 0.4D1 * t7 + (-0.3D1 * t17 - 0.4D1 * t
     #19 - t22) * t2 + (-0.4D1 * t25 + 0.7D1 * t17 + 0.6D1 * t19 + t22 +
     # (S13 * t25 + 0.2D1 * t21 * S14 + 0.2D1 * t9 * t16) * t2) / S12) /
     # pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh82J2
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t4 = S12 ** 2
      t6 = S13 * S14
      t8 = S13 ** 2
      t9 = 0.2D1 * t8
      t17 = S14 ** 2
      t19 = t17 * S13
      t21 = t8 * S23
      t22 = 0.2D1 * t21
      t23 = S23 ** 2
      t25 = 0.2D1 * S13 * t23
      t26 = S14 * t8
      rrgq2qgh82J2 = (-S13 * t2 * t4 + (S13 + (0.3D1 * t6 + t9) * t2) * 
     #S12 - 0.4D1 * S14 * S23 - t9 - 0.4D1 * t6 + 0.8D1 * t17 + (-0.3D1 
     #* t19 - t22 - t25 - 0.4D1 * t26) * t2 + (t22 - 0.4D1 * S23 * t17 +
     # 0.6D1 * t26 + t25 + 0.7D1 * t19 - 0.4D1 * t23 * S14 + (0.2D1 * t2
     #1 * S14 + 0.2D1 * t8 * t17 + 0.2D1 * t6 * t23 + S13 * t17 * S14) *
     # t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh82J3
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t4 = S12 ** 2
      t6 = S13 * S14
      t8 = S13 ** 2
      t9 = 0.2D1 * t8
      t17 = S14 ** 2
      t19 = t17 * S13
      t21 = t8 * S23
      t22 = 0.2D1 * t21
      t23 = S23 ** 2
      t25 = 0.2D1 * S13 * t23
      t26 = S14 * t8
      rrgq2qgh82J3 = (-S13 * t2 * t4 + (S13 + (0.3D1 * t6 + t9) * t2) * 
     #S12 - 0.4D1 * S14 * S23 - t9 - 0.4D1 * t6 + 0.8D1 * t17 + (-0.3D1 
     #* t19 - t22 - t25 - 0.4D1 * t26) * t2 + (t22 - 0.4D1 * S23 * t17 +
     # 0.6D1 * t26 + t25 + 0.7D1 * t19 - 0.4D1 * t23 * S14 + (0.2D1 * t2
     #1 * S14 + 0.2D1 * t8 * t17 + 0.2D1 * t6 * t23 + S13 * t17 * S14) *
     # t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh82J4
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t4 = S12 ** 2
      t6 = S13 * S14
      t8 = S13 ** 2
      t9 = 0.2D1 * t8
      t17 = S14 ** 2
      t19 = t17 * S13
      t21 = t8 * S23
      t22 = 0.2D1 * t21
      t23 = S23 ** 2
      t25 = 0.2D1 * S13 * t23
      t26 = S14 * t8
      rrgq2qgh82J4 = (-S13 * t2 * t4 + (S13 + (0.3D1 * t6 + t9) * t2) * 
     #S12 - 0.4D1 * S14 * S23 - t9 - 0.4D1 * t6 + 0.8D1 * t17 + (-0.3D1 
     #* t19 - t22 - t25 - 0.4D1 * t26) * t2 + (t22 - 0.4D1 * S23 * t17 +
     # 0.6D1 * t26 + t25 + 0.7D1 * t19 - 0.4D1 * t23 * S14 + (0.2D1 * t2
     #1 * S14 + 0.2D1 * t8 * t17 + 0.2D1 * t6 * t23 + S13 * t17 * S14) *
     # t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh82J5
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t4 = S12 ** 2
      t6 = S13 * S14
      t8 = S13 ** 2
      t9 = 0.2D1 * t8
      t17 = S14 ** 2
      t19 = t17 * S13
      t21 = t8 * S23
      t22 = 0.2D1 * t21
      t23 = S23 ** 2
      t25 = 0.2D1 * S13 * t23
      t26 = S14 * t8
      rrgq2qgh82J5 = (-S13 * t2 * t4 + (S13 + (0.3D1 * t6 + t9) * t2) * 
     #S12 - 0.4D1 * S14 * S23 - t9 - 0.4D1 * t6 + 0.8D1 * t17 + (-0.3D1 
     #* t19 - t22 - t25 - 0.4D1 * t26) * t2 + (t22 - 0.4D1 * S23 * t17 +
     # 0.6D1 * t26 + t25 + 0.7D1 * t19 - 0.4D1 * t23 * S14 + (0.2D1 * t2
     #1 * S14 + 0.2D1 * t8 * t17 + 0.2D1 * t6 * t23 + S13 * t17 * S14) *
     # t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh82J6
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
      t5 = S14 ** 2
      t7 = S13 ** 2
      t8 = t7 * S13
      t9 = t7 * S23
      t10 = S23 ** 2
      t11 = S13 * t10
      t15 = 0.1D1 / (S12 + S14 + S24)
      rrgq2qgh82J6 = (0.4D1 * S12 * S14 - 0.4D1 * S14 * S23 + 0.8D1 * t5
     # + (0.2D1 * t8 - 0.2D1 * t9 - 0.2D1 * t11) * t15 + (0.2D1 * t9 + 0
     #.4D1 * S14 * t5 + 0.2D1 * t11 - 0.2D1 * t8 - 0.4D1 * S23 * t5 - 0.
     #4D1 * t10 * S14 + (-0.2D1 * S14 * t8 + 0.2D1 * S13 * S14 * t10 + 0
     #.2D1 * S14 * t9) * t15) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh83J1
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t4 = S12 ** 2
      t7 = S23 * S24
      t9 = S24 ** 2
      t10 = 0.2D1 * t9
      t16 = t9 * S24
      t17 = 0.2D1 * t16
      t18 = S23 ** 2
      t19 = S24 * t18
      t21 = t9 * S23
      t25 = t18 * S23
      rrgq2qgh83J1 = (-S24 * t2 * t4 + (S24 - 0.4D1 * S23 + (0.3D1 * t7 
     #+ t10) * t2) * S12 - 0.4D1 * t7 - t10 + (-t17 - 0.3D1 * t19 - 0.4D
     #1 * t21) * t2 + (-0.4D1 * t25 + 0.7D1 * t19 + 0.6D1 * t21 + t17 + 
     #(0.2D1 * t16 * S23 + 0.2D1 * t9 * t18 + S24 * t25) * t2) / S12) / 
     #pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh83J2
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t4 = S12 ** 2
      t6 = S23 * S24
      t8 = S24 ** 2
      t9 = 0.2D1 * t8
      t17 = S23 ** 2
      t19 = t8 * S23
      t21 = S14 * t8
      t22 = 0.2D1 * t21
      t23 = S14 ** 2
      t25 = 0.2D1 * t23 * S24
      t26 = S24 * t17
      rrgq2qgh83J2 = (-S24 * t2 * t4 + (S24 + (0.3D1 * t6 + t9) * t2) * 
     #S12 - t9 - 0.4D1 * S14 * S23 - 0.4D1 * t6 + 0.8D1 * t17 + (-0.4D1 
     #* t19 - t22 - t25 - 0.3D1 * t26) * t2 + (-0.4D1 * t17 * S14 - 0.4D
     #1 * S23 * t23 + 0.7D1 * t26 + 0.6D1 * t19 + t22 + t25 + (0.2D1 * t
     #21 * S23 + 0.2D1 * t6 * t23 + 0.2D1 * t8 * t17 + S24 * t17 * S23) 
     #* t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh83J3
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t4 = S12 ** 2
      t6 = S23 * S24
      t8 = S24 ** 2
      t9 = 0.2D1 * t8
      t17 = S23 ** 2
      t19 = t8 * S23
      t21 = S14 * t8
      t22 = 0.2D1 * t21
      t23 = S14 ** 2
      t25 = 0.2D1 * t23 * S24
      t26 = S24 * t17
      rrgq2qgh83J3 = (-S24 * t2 * t4 + (S24 + (0.3D1 * t6 + t9) * t2) * 
     #S12 - t9 - 0.4D1 * S14 * S23 - 0.4D1 * t6 + 0.8D1 * t17 + (-0.4D1 
     #* t19 - t22 - t25 - 0.3D1 * t26) * t2 + (-0.4D1 * t17 * S14 - 0.4D
     #1 * S23 * t23 + 0.7D1 * t26 + 0.6D1 * t19 + t22 + t25 + (0.2D1 * t
     #21 * S23 + 0.2D1 * t6 * t23 + 0.2D1 * t8 * t17 + S24 * t17 * S23) 
     #* t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh83J4
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t4 = S12 ** 2
      t6 = S23 * S24
      t8 = S24 ** 2
      t9 = 0.2D1 * t8
      t17 = S23 ** 2
      t19 = t8 * S23
      t21 = S14 * t8
      t22 = 0.2D1 * t21
      t23 = S14 ** 2
      t25 = 0.2D1 * t23 * S24
      t26 = S24 * t17
      rrgq2qgh83J4 = (-S24 * t2 * t4 + (S24 + (0.3D1 * t6 + t9) * t2) * 
     #S12 - t9 - 0.4D1 * S14 * S23 - 0.4D1 * t6 + 0.8D1 * t17 + (-0.4D1 
     #* t19 - t22 - t25 - 0.3D1 * t26) * t2 + (-0.4D1 * t17 * S14 - 0.4D
     #1 * S23 * t23 + 0.7D1 * t26 + 0.6D1 * t19 + t22 + t25 + (0.2D1 * t
     #21 * S23 + 0.2D1 * t6 * t23 + 0.2D1 * t8 * t17 + S24 * t17 * S23) 
     #* t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh83J5
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t4 = S12 ** 2
      t6 = S23 * S24
      t8 = S24 ** 2
      t9 = 0.2D1 * t8
      t17 = S23 ** 2
      t19 = t8 * S23
      t21 = S14 * t8
      t22 = 0.2D1 * t21
      t23 = S14 ** 2
      t25 = 0.2D1 * t23 * S24
      t26 = S24 * t17
      rrgq2qgh83J5 = (-S24 * t2 * t4 + (S24 + (0.3D1 * t6 + t9) * t2) * 
     #S12 - t9 - 0.4D1 * S14 * S23 - 0.4D1 * t6 + 0.8D1 * t17 + (-0.4D1 
     #* t19 - t22 - t25 - 0.3D1 * t26) * t2 + (-0.4D1 * t17 * S14 - 0.4D
     #1 * S23 * t23 + 0.7D1 * t26 + 0.6D1 * t19 + t22 + t25 + (0.2D1 * t
     #21 * S23 + 0.2D1 * t6 * t23 + 0.2D1 * t8 * t17 + S24 * t17 * S23) 
     #* t2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh83J6
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
      t3 = S23 ** 2
      t7 = S24 ** 2
      t8 = t7 * S24
      t9 = S14 ** 2
      t10 = t9 * S24
      t11 = S14 * t7
      t15 = 0.1D1 / (S12 + S13 + S23)
      rrgq2qgh83J6 = (0.4D1 * S23 * S12 + 0.8D1 * t3 - 0.4D1 * S14 * S23
     # + (0.2D1 * t8 - 0.2D1 * t10 - 0.2D1 * t11) * t15 + (-0.4D1 * t9 *
     # S23 + 0.2D1 * t10 + 0.4D1 * t3 * S23 - 0.2D1 * t8 - 0.4D1 * S14 *
     # t3 + 0.2D1 * t11 + (-0.2D1 * t8 * S23 + 0.2D1 * t11 * S23 + 0.2D1
     # * S24 * S23 * t9) * t15) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh84J1
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
      t5 = S24 ** 2
      t8 = S23 ** 2
      rrgq2qgh84J1 = (0.8D1 / 0.9D1 * S24 * S12 + 0.8D1 / 0.9D1 * S23 * 
     #S24 + (0.8D1 / 0.9D1 * t5 * S24 + 0.8D1 / 0.9D1 * S23 * t5 + 0.8D1
     # / 0.9D1 * t8 * S24) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh84J2
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
      t3 = S24 ** 2
      t7 = S23 ** 2
      rrgq2qgh84J2 = (-0.8D1 / 0.9D1 * S23 * S24 - 0.16D2 / 0.9D1 * t3 +
     # (-0.8D1 / 0.9D1 * t3 * S23 - 0.8D1 / 0.9D1 * t7 * S24 - 0.4D1 / 0
     #.9D1 * S14 * S13 * S23) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh84J3
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
      t1 = S23 * S13
      t3 = 0.1D1 / (S12 + S14 + S24)
      t9 = S24 ** 2
      t12 = S23 ** 2
      t13 = t12 * S13
      t24 = S13 ** 2
      rrgq2qgh84J3 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 - 0.8D1 / 0.9D1 * S
     #23 * S24 - 0.16D2 / 0.9D1 * t9 + 0.4D1 / 0.9D1 * t1 - 0.4D1 / 0.9D
     #1 * t13 * t3 + (-0.8D1 / 0.9D1 * t9 * S23 - 0.8D1 / 0.9D1 * t12 * 
     #S24 - 0.8D1 / 0.9D1 * S14 * S13 * S23 + 0.8D1 / 0.9D1 * t13 + 0.4D
     #1 / 0.9D1 * t24 * S23) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh84J4
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
      t1 = S23 * S13
      t3 = 0.1D1 / (S12 + S14 + S24)
      t9 = S24 ** 2
      t12 = S23 ** 2
      t13 = t12 * S13
      t24 = S13 ** 2
      rrgq2qgh84J4 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 - 0.8D1 / 0.9D1 * S
     #23 * S24 - 0.16D2 / 0.9D1 * t9 + 0.4D1 / 0.9D1 * t1 - 0.4D1 / 0.9D
     #1 * t13 * t3 + (-0.8D1 / 0.9D1 * t9 * S23 - 0.8D1 / 0.9D1 * t12 * 
     #S24 - 0.8D1 / 0.9D1 * S14 * S13 * S23 + 0.8D1 / 0.9D1 * t13 + 0.4D
     #1 / 0.9D1 * t24 * S23) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh84J5
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
      t1 = S23 * S13
      t3 = 0.1D1 / (S12 + S14 + S24)
      t9 = S24 ** 2
      t12 = S23 ** 2
      t13 = t12 * S13
      t24 = S13 ** 2
      rrgq2qgh84J5 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 - 0.8D1 / 0.9D1 * S
     #23 * S24 - 0.16D2 / 0.9D1 * t9 + 0.4D1 / 0.9D1 * t1 - 0.4D1 / 0.9D
     #1 * t13 * t3 + (-0.8D1 / 0.9D1 * t9 * S23 - 0.8D1 / 0.9D1 * t12 * 
     #S24 - 0.8D1 / 0.9D1 * S14 * S13 * S23 + 0.8D1 / 0.9D1 * t13 + 0.4D
     #1 / 0.9D1 * t24 * S23) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh84J6
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
      t2 = S23 * S13
      t4 = 0.1D1 / (S12 + S14 + S24)
      t9 = S24 ** 2
      t14 = S23 ** 2
      t15 = t14 * S13
      t28 = S13 ** 2
      rrgq2qgh84J6 = ((-0.8D1 / 0.9D1 * S24 - 0.4D1 / 0.9D1 * t2 * t4) *
     # S12 - 0.16D2 / 0.9D1 * t9 + 0.4D1 / 0.9D1 * t2 - 0.16D2 / 0.9D1 *
     # S23 * S24 - 0.4D1 / 0.9D1 * t15 * t4 + (-0.8D1 / 0.9D1 * S14 * S1
     #3 * S23 - 0.8D1 / 0.9D1 * t9 * S24 - 0.16D2 / 0.9D1 * t14 * S24 + 
     #0.8D1 / 0.9D1 * t15 - 0.16D2 / 0.9D1 * t9 * S23 + 0.4D1 / 0.9D1 * 
     #t28 * S23) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgq2qgh84J7
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
      t1 = S23 * S13
      t3 = 0.1D1 / (S12 + S14 + S24)
      t8 = S23 ** 2
      t9 = S13 * t8
      t13 = S13 ** 2
      rrgq2qgh84J7 = (-0.4D1 / 0.9D1 * t1 * t3 * S12 + 0.4D1 / 0.9D1 * t
     #1 - 0.4D1 / 0.9D1 * t9 * t3 + (0.8D1 / 0.9D1 * t9 + 0.4D1 / 0.9D1 
     #* t13 * S23 - 0.4D1 / 0.9D1 * S14 * S13 * S23) / S12) / pi * wd / 
     #z

      end function
  
 