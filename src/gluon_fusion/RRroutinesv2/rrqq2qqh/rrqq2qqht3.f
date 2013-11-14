  
      subroutine rrqq2qqht3
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqq2qqh31J1  
      doubleprecision rrqq2qqh31J2  
      doubleprecision rrqq2qqh31J3  
      doubleprecision rrqq2qqht3s1e1  
      doubleprecision rrqq2qqht3s1e0  
      doubleprecision rrqq2qqht3s1em1  
      doubleprecision rrqq2qqht3s1em2  
      doubleprecision rrqq2qqht3s1em3  
      doubleprecision rrqq2qqht3s1em4  
      doubleprecision rrqq2qqht3s2e1  
      doubleprecision rrqq2qqht3s2e0  
      doubleprecision rrqq2qqht3s2em1  
      doubleprecision rrqq2qqht3s2em2  
      doubleprecision rrqq2qqht3s2em3  
      doubleprecision rrqq2qqht3s2em4  
      doubleprecision rrqq2qqht3s3e1  
      doubleprecision rrqq2qqht3s3e0  
      doubleprecision rrqq2qqht3s3em1  
      doubleprecision rrqq2qqht3s3em2  
      doubleprecision rrqq2qqht3s3em3  
      doubleprecision rrqq2qqht3s3em4  
      doubleprecision rrqq2qqht3s4e1  
      doubleprecision rrqq2qqht3s4e0  
      doubleprecision rrqq2qqht3s4em1  
      doubleprecision rrqq2qqht3s4em2  
      doubleprecision rrqq2qqht3s4em3  
      doubleprecision rrqq2qqht3s4em4  
      doubleprecision rrqq2qqht3s5e1  
      doubleprecision rrqq2qqht3s5e0  
      doubleprecision rrqq2qqht3s5em1  
      doubleprecision rrqq2qqht3s5em2  
      doubleprecision rrqq2qqht3s5em3  
      doubleprecision rrqq2qqht3s5em4  
      doubleprecision rrqq2qqht3s6e1  
      doubleprecision rrqq2qqht3s6e0  
      doubleprecision rrqq2qqht3s6em1  
      doubleprecision rrqq2qqht3s6em2  
      doubleprecision rrqq2qqht3s6em3  
      doubleprecision rrqq2qqht3s6em4  
      doubleprecision rrqq2qqht3s7e1  
      doubleprecision rrqq2qqht3s7e0  
      doubleprecision rrqq2qqht3s7em1  
      doubleprecision rrqq2qqht3s7em2  
      doubleprecision rrqq2qqht3s7em3  
      doubleprecision rrqq2qqht3s7em4  
      doubleprecision rrqq2qqht3s8e1  
      doubleprecision rrqq2qqht3s8e0  
      doubleprecision rrqq2qqht3s8em1  
      doubleprecision rrqq2qqht3s8em2  
      doubleprecision rrqq2qqht3s8em3  
      doubleprecision rrqq2qqht3s8em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht3s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqq2qqht3s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqq2qqht3s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqq2qqht3s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqq2qqht3s5e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqq2qqht3s6e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqq2qqht3s7e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqq2qqht3s8e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht3s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqq2qqht3s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqq2qqht3s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqq2qqht3s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqq2qqht3s5e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqq2qqht3s6e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqq2qqht3s7e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqq2qqht3s8e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht3s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqq2qqht3s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqq2qqht3s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqq2qqht3s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqq2qqht3s5em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqq2qqht3s6em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqq2qqht3s7em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqq2qqht3s8em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht3s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqq2qqht3s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqq2qqht3s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqq2qqht3s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqq2qqht3s5em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqq2qqht3s6em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqq2qqht3s7em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqq2qqht3s8em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht3s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqq2qqht3s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqq2qqht3s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqq2qqht3s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqq2qqht3s5em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqq2qqht3s6em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqq2qqht3s7em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqq2qqht3s8em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht3s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqq2qqht3s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrqq2qqht3s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrqq2qqht3s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrqq2qqht3s5em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrqq2qqht3s6em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrqq2qqht3s7em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrqq2qqht3s8em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqq2qqht3s1e1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = x1 ** 2
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t17 * x4
      t19 = -0.1D1 + x4
      t20 = t18 * t19
      t23 = log(-0.4D1 * t15 * t20)
      t24 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t26 = t23 ** 2
      t27 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t30 = t14 * t17
      t31 = t30 * x4
      t34 = log(0.4D1 * t12 * t31)
      t36 = t34 ** 2
      t43 = pi * lh
      t44 = t3 * t7
      t52 = 0.1D1 / x1
      t54 = 0.1D1 / x4
      t57 = pi ** 2
      t59 = lh ** 2
      t61 = -0.30D2 * t57 + 0.180D3 * t59
      t62 = pi * t61
      t63 = t12 * t30
      t65 = log(0.4D1 * t63)
      t70 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t72 = t65 ** 2
      t87 = -0.240D3 * zeta3 - 0.120D3 * t59 * lh + 0.60D2 * lh * t57
      t88 = pi * t87
      t89 = t44 * t27
      t90 = t88 * t89
      t101 = t4 * t7
      t102 = x3 * t8
      t103 = t102 * t11
      t104 = x4 * t19
      t108 = log(-0.4D1 * t103 * t30 * t104)
      t112 = log(0.4D1 * t103 * t31)
      t115 = 0.1D1 / x3
      t117 = t52 * t54
      t121 = t11 * t14
      t122 = t121 * t17
      t123 = t102 * t122
      t125 = log(0.4D1 * t123)
      t127 = t125 ** 2
      t139 = t62 * t89
      t144 = t7 * t24
      t152 = log(0.4D1 * t121 * t18)
      t153 = t152 ** 2
      t156 = log(-0.4D1 * t121 * t20)
      t157 = t156 ** 2
      t161 = t7 * t27
      t169 = t7 * t70
      t182 = log(0.4D1 * t122)
      t183 = t182 * pi
      t186 = t182 ** 2
      t187 = t186 * pi
      t197 = t186 * t182 * pi
      t203 = t57 ** 2
      t204 = t59 ** 2
      t217 = t186 ** 2
      t224 = x3 * t11
      t227 = log(0.4D1 * t224 * t31)
      t229 = t227 ** 2
      t232 = t224 * t14
      t235 = log(-0.4D1 * t232 * t20)
      t237 = t235 ** 2
      t256 = log(0.4D1 * t224 * t30)
      t262 = t256 ** 2
      t282 = -(0.90D2 * t4 * t7 * (-t23 * t24 + t26 * t27 / 0.2D1 + t34 
     #* t24 - t36 * t27 / 0.2D1) - 0.180D3 * t43 * t44 * (-t23 * t27 + t
     #34 * t27)) * t52 * t54 / 0.720D3 + (t62 * t44 * (t24 - t65 * t27) 
     #+ 0.90D2 * t4 * t7 * (-t65 * t70 + t72 * t24 / 0.2D1 - t72 * t65 *
     # t27 / 0.6D1) + t90 - 0.180D3 * t43 * t44 * (t70 - t65 * t24 + t72
     # * t27 / 0.2D1)) * t52 / 0.720D3 - t101 * (-t108 * t27 + t112 * t2
     #7) * t115 * t117 / 0.8D1 + (0.90D2 * t4 * t7 * (t70 - t125 * t24 +
     # t127 * t27 / 0.2D1) - 0.180D3 * t43 * t44 * (t24 - t125 * t27) + 
     #t139) * t115 * t52 / 0.720D3 + ((0.90D2 * t4 * t144 - 0.180D3 * t4
     #3 * t89) * (t153 / 0.2D1 - t157 / 0.2D1) + 0.90D2 * t4 * t161 * (t
     #157 * t156 / 0.6D1 - t153 * t152 / 0.6D1) + (0.90D2 * t4 * t169 - 
     #0.180D3 * t43 * t44 * t24 + t139) * (-t152 + t156)) * t54 / 0.1440
     #D4 + (t62 + 0.180D3 * t183 * lh + 0.45D2 * t187) * t3 * t169 / 0.1
     #440D4 + (t88 - t183 * t61 - 0.90D2 * t187 * lh - 0.15D2 * t197) * 
     #t3 * t144 / 0.1440D4 + (pi * (t203 + 0.60D2 * t204 + 0.480D3 * lh 
     #* zeta3 - 0.60D2 * t59 * t57) - t183 * t87 + t187 * t61 / 0.2D1 + 
     #0.30D2 * t197 * lh + 0.15D2 / 0.4D1 * t217 * pi) * t3 * t161 / 0.1
     #440D4 - (0.90D2 * t4 * t7 * (t227 * t24 - t229 * t27 / 0.2D1 - t23
     #5 * t24 + t237 * t27 / 0.2D1) - 0.180D3 * t43 * t44 * (t227 * t27 
     #- t235 * t27)) * t115 * t54 / 0.1440D4 + (t62 * t44 * (t24 - t256 
     #* t27) + 0.90D2 * t4 * t7 * (-t256 * t70 + t262 * t24 / 0.2D1 - t2
     #62 * t256 * t27 / 0.6D1) + t90 - 0.180D3 * t43 * t44 * (t70 - t256
     # * t24 + t262 * t27 / 0.2D1)) * t115 / 0.1440D4
      t283 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t282)
      t285 = 0.1D1 - x1
      t286 = 0.1D1 - x3
      t287 = KAPPA2(t285, x2, t286, 0.10D1, z)
      t288 = s * t287
      t289 = -t285
      t290 = t1 * t289
      t291 = -t286
      t292 = t290 * t291
      t293 = t288 * t292
      t294 = t290 * x3
      t295 = t288 * t294
      t296 = t1 * x1
      t297 = t288 * t296
      t298 = t287 ** 2
      t301 = t289 * x1
      t303 = s * t298 * t16 * t301 * x3
      t305 = 0.1D1 / (-0.2D1 + t287)
      t306 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t293, t297, -t2
     #95, 0.0D0, -t303)
      t307 = t305 * t306
      t308 = t102 * t121
      t309 = t289 ** 2
      t310 = t17 * t309
      t311 = t291 * x4
      t312 = t298 ** 2
      t317 = log(-0.4D1 * t308 * t310 * t311 * t312)
      t319 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t293, t297, -t2
     #95, 0.0D0, -t303)
      t325 = t43 * t3
      t327 = t7 * t305 * t319
      t333 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t293, t297, -t2
     #95, 0.0D0, -t303)
      t339 = log(-0.4D1 * t308 * t310 * t291 * t312)
      t340 = t339 * t305
      t342 = t339 ** 2
      t355 = t62 * t3
      t361 = -(0.90D2 * t4 * t7 * (t307 - t317 * t305 * t319) - 0.180D3 
     #* t325 * t327) * t115 * t117 / 0.720D3 + (0.90D2 * t4 * t7 * (-t30
     #5 * t333 + t340 * t306 - t342 * t305 * t319 / 0.2D1) - 0.180D3 * t
     #43 * t44 * (-t307 + t340 * t319) - t355 * t327) * t115 * t52 / 0.7
     #20D3
      t362 = FJET(XB1, XB2, s, t293, -t295, t297, 0.0D0, -t303, t361)
      t364 = -t19
      t365 = KAPPA2(t285, x2, t286, t364, z)
      t366 = s * t365
      t367 = t366 * t292
      t368 = t366 * t294
      t369 = t296 * t19
      t370 = t366 * t369
      t371 = t296 * x4
      t372 = t366 * t371
      t373 = t365 ** 2
      t378 = cos(t9)
      t381 = Sqrt(x3 * t291 * t104)
      t386 = s * t373 * t16 * t301 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t378 * t381)
      t388 = 0.1D1 / (-0.2D1 + t365)
      t389 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t367, -t370, -t
     #368, t372, t386)
      t392 = t373 ** 2
      t397 = log(0.4D1 * t123 * t309 * t291 * t104 * t392)
      t399 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t367, -t370, -t
     #368, t372, t386)
      t409 = 0.90D2 * t4 * t7 * (-t388 * t389 + t397 * t388 * t399) + 0.
     #180D3 * t325 * t7 * t388 * t399
      t413 = FJET(XB1, XB2, s, t367, -t368, -t370, t372, t386, -t409 * t
     #115 * t117 / 0.720D3)
      t419 = t2 * t289
      t420 = t2 * x1
      t421 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, -t419, t420, 0.
     #0D0, 0.0D0, 0.0D0)
      t425 = log(0.4D1 * t15 * t310 * x4)
      t426 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t419, t420, 0.
     #0D0, 0.0D0, 0.0D0)
      t428 = t425 ** 2
      t429 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t419, t420, 0.
     #0D0, 0.0D0, 0.0D0)
      t441 = t44 * t429
      t442 = t62 * t441
      t446 = t30 * t309
      t449 = log(0.4D1 * t12 * t446)
      t455 = t449 ** 2
      t475 = t309 * x4
      t479 = log(0.4D1 * t103 * t30 * t475)
      t492 = log(0.4D1 * t103 * t446)
      t494 = t492 ** 2
      t510 = -(0.90D2 * t4 * t7 * (t421 - t425 * t426 + t428 * t429 / 0.
     #2D1) - 0.180D3 * t43 * t44 * (t426 - t425 * t429) + t442) * t52 * 
     #t54 / 0.720D3 + (-t62 * t44 * (t426 - t449 * t429) - 0.90D2 * t4 *
     # t7 * (-t449 * t421 + t455 * t426 / 0.2D1 - t455 * t449 * t429 / 0
     #.6D1) - t88 * t441 + 0.180D3 * t43 * t44 * (t421 - t449 * t426 + t
     #455 * t429 / 0.2D1)) * t52 / 0.720D3 - (0.90D2 * t4 * t7 * (t426 -
     # t479 * t429) - 0.180D3 * t43 * t441) * t115 * t117 / 0.720D3 + (0
     #.90D2 * t4 * t7 * (-t421 + t492 * t426 - t494 * t429 / 0.2D1) - 0.
     #180D3 * t43 * t44 * (-t426 + t492 * t429) - t442) * t115 * t52 / 0
     #.720D3
      t511 = FJET(XB1, XB2, s, -t419, 0.0D0, t420, 0.0D0, 0.0D0, t510)
      t513 = t2 * t291
      t514 = t2 * x3
      t518 = log(-0.4D1 * t103 * t30 * t311)
      t519 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t513, 0.0D0, t
     #514, 0.0D0, 0.0D0)
      t521 = t17 * t291
      t522 = t521 * t104
      t525 = log(0.4D1 * t308 * t522)
      t532 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, -t513, 0.0D0, t
     #514, 0.0D0, 0.0D0)
      t533 = t30 * t291
      t536 = log(-0.4D1 * t103 * t533)
      t537 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t513, 0.0D0, t
     #514, 0.0D0, 0.0D0)
      t539 = t536 ** 2
      t551 = t44 * t519
      t559 = log(0.4D1 * t232 * t522)
      t561 = t559 ** 2
      t567 = log(-0.4D1 * t232 * t521 * x4)
      t569 = t567 ** 2
      t588 = log(-0.4D1 * t224 * t533)
      t594 = t588 ** 2
      t615 = -t101 * (-t518 * t519 + t525 * t519) * t115 * t117 / 0.8D1 
     #+ (-0.90D2 * t4 * t7 * (t532 - t536 * t537 + t539 * t519 / 0.2D1) 
     #+ 0.180D3 * t43 * t44 * (t537 - t536 * t519) - t62 * t551) * t115 
     #* t52 / 0.720D3 - (0.90D2 * t4 * t7 * (t559 * t537 - t561 * t519 /
     # 0.2D1 - t567 * t537 + t569 * t519 / 0.2D1) - 0.180D3 * t43 * t44 
     #* (t559 * t519 - t567 * t519)) * t115 * t54 / 0.1440D4 + (-t62 * t
     #44 * (t537 - t588 * t519) - 0.90D2 * t4 * t7 * (-t588 * t532 + t59
     #4 * t537 / 0.2D1 - t594 * t588 * t519 / 0.6D1) - t88 * t551 + 0.18
     #0D3 * t43 * t44 * (t532 - t588 * t537 + t594 * t519 / 0.2D1)) * t1
     #15 / 0.1440D4
      t616 = FJET(XB1, XB2, s, -t513, t514, 0.0D0, 0.0D0, 0.0D0, t615)
      t618 = KAPPA2(t285, x2, 0.10D1, t364, z)
      t619 = s * t618
      t620 = t619 * t290
      t621 = t619 * t369
      t622 = t619 * t371
      t623 = t618 ** 2
      t627 = s * t623 * t16 * t301 * x4
      t629 = 0.1D1 / (-0.2D1 + t618)
      t630 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, -t620, -t621, 0
     #.0D0, t622, -t627)
      t632 = t623 ** 2
      t637 = log(-0.4D1 * t63 * t475 * t19 * t632)
      t638 = t637 * t629
      t639 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t620, -t621, 0
     #.0D0, t622, -t627)
      t641 = t637 ** 2
      t643 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t620, -t621, 0
     #.0D0, t622, -t627)
      t650 = t629 * t639
      t657 = t7 * t629 * t643
      t666 = log(-0.4D1 * t308 * t310 * t104 * t632)
      t679 = -(0.90D2 * t4 * t7 * (t629 * t630 - t638 * t639 + t641 * t6
     #29 * t643 / 0.2D1) - 0.180D3 * t43 * t44 * (t650 - t638 * t643) + 
     #t355 * t657) * t52 * t54 / 0.720D3 - (0.90D2 * t4 * t7 * (t650 - t
     #666 * t629 * t643) - 0.180D3 * t325 * t657) * t115 * t117 / 0.720D
     #3
      t680 = FJET(XB1, XB2, s, -t620, 0.0D0, -t621, t622, -t627, t679)
      rrqq2qqht3s1e1 = t283 * t282 + t362 * t361 - t413 * t409 * t115 * 
     #t52 * t54 / 0.720D3 + t511 * t510 + t616 * t615 + t680 * t679

      end function



      doubleprecision function rrqq2qqht3s1e0
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = x1 ** 2
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t18 * x4
      t20 = -0.1D1 + x4
      t21 = t19 * t20
      t24 = log(-0.4D1 * t16 * t21)
      t25 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t27 = t15 * t18
      t28 = t27 * x4
      t31 = log(0.4D1 * t13 * t28)
      t34 = 0.1D1 / x1
      t36 = 0.1D1 / x4
      t40 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t41 = x3 * t9
      t42 = t12 * t15
      t43 = t42 * t18
      t46 = log(0.4D1 * t41 * t43)
      t52 = pi * lh
      t53 = t3 * t7
      t54 = t53 * t25
      t56 = 0.180D3 * t52 * t54
      t58 = 0.1D1 / x3
      t62 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t63 = t13 * t27
      t65 = log(0.4D1 * t63)
      t67 = t65 ** 2
      t79 = pi ** 2
      t81 = lh ** 2
      t83 = -0.30D2 * t79 + 0.180D3 * t81
      t84 = pi * t83
      t85 = t84 * t54
      t89 = t7 * t25
      t92 = log(0.4D1 * t42 * t19)
      t93 = t92 ** 2
      t96 = log(-0.4D1 * t42 * t21)
      t97 = t96 ** 2
      t103 = t7 * t40
      t114 = log(0.4D1 * t43)
      t115 = t114 * pi
      t124 = t114 ** 2
      t125 = t124 * pi
      t148 = x3 * t12
      t151 = log(0.4D1 * t148 * t28)
      t153 = t148 * t15
      t156 = log(-0.4D1 * t153 * t21)
      t165 = log(0.4D1 * t148 * t27)
      t167 = t165 ** 2
      t182 = -t8 * (-t24 * t25 + t31 * t25) * t34 * t36 / 0.8D1 + (0.90D
     #2 * t4 * t7 * (t40 - t46 * t25) - t56) * t58 * t34 / 0.720D3 + (0.
     #90D2 * t4 * t7 * (t62 - t65 * t40 + t67 * t25 / 0.2D1) - 0.180D3 *
     # t52 * t53 * (t40 - t65 * t25) + t85) * t34 / 0.720D3 + (0.90D2 * 
     #t4 * t89 * (t93 / 0.2D1 - t97 / 0.2D1) + (0.90D2 * t4 * t103 - t56
     #) * (-t92 + t96)) * t36 / 0.1440D4 + (-0.180D3 * t52 - 0.90D2 * t1
     #15) * t3 * t7 * t62 / 0.1440D4 + (t84 + 0.180D3 * t115 * lh + 0.45
     #D2 * t125) * t3 * t103 / 0.1440D4 + (pi * (-0.240D3 * zeta3 - 0.12
     #0D3 * t81 * lh + 0.60D2 * lh * t79) - t115 * t83 - 0.90D2 * t125 *
     # lh - 0.15D2 * t124 * t114 * pi) * t3 * t89 / 0.1440D4 - t8 * (t15
     #1 * t25 - t156 * t25) * t58 * t36 / 0.16D2 + (0.90D2 * t4 * t7 * (
     #t62 - t165 * t40 + t167 * t25 / 0.2D1) - 0.180D3 * t52 * t53 * (t4
     #0 - t165 * t25) + t85) * t58 / 0.1440D4
      t183 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t182)
      t185 = 0.1D1 - x1
      t186 = 0.1D1 - x3
      t187 = KAPPA2(t185, x2, t186, 0.10D1, z)
      t188 = s * t187
      t189 = -t185
      t190 = t1 * t189
      t191 = -t186
      t192 = t190 * t191
      t193 = t188 * t192
      t194 = t190 * x3
      t195 = t188 * t194
      t196 = t1 * x1
      t197 = t188 * t196
      t198 = t187 ** 2
      t201 = t189 * x1
      t203 = s * t198 * t17 * t201 * x3
      t205 = 0.1D1 / (-0.2D1 + t187)
      t206 = t7 * t205
      t208 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t193, t197, -t1
     #95, 0.0D0, -t203)
      t210 = t34 * t36
      t214 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t193, t197, -t1
     #95, 0.0D0, -t203)
      t217 = t189 ** 2
      t218 = t18 * t217
      t219 = t198 ** 2
      t224 = log(-0.4D1 * t41 * t42 * t218 * t191 * t219)
      t231 = t52 * t3
      t239 = -t4 * t206 * t208 * t58 * t210 / 0.8D1 + (0.90D2 * t4 * t7 
     #* (-t205 * t214 + t224 * t205 * t208) + 0.180D3 * t231 * t206 * t2
     #08) * t58 * t34 / 0.720D3
      t240 = FJET(XB1, XB2, s, t193, -t195, t197, 0.0D0, -t203, t239)
      t242 = -t20
      t243 = KAPPA2(t185, x2, t186, t242, z)
      t244 = s * t243
      t245 = t244 * t192
      t246 = t244 * t194
      t247 = t196 * t20
      t248 = t244 * t247
      t249 = t196 * x4
      t250 = t244 * t249
      t251 = t243 ** 2
      t256 = cos(t10)
      t258 = x4 * t20
      t260 = Sqrt(x3 * t191 * t258)
      t265 = s * t251 * t17 * t201 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t256 * t260)
      t267 = 0.1D1 / (-0.2D1 + t243)
      t270 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t245, -t248, -t
     #246, t250, t265)
      t275 = FJET(XB1, XB2, s, t245, -t246, -t248, t250, t265, t4 * t7 *
     # t267 * t270 * t58 * t210 / 0.8D1)
      t284 = t2 * t189
      t285 = t2 * x1
      t286 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t284, t285, 0.
     #0D0, 0.0D0, 0.0D0)
      t290 = log(0.4D1 * t16 * t218 * x4)
      t291 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t284, t285, 0.
     #0D0, 0.0D0, 0.0D0)
      t297 = t53 * t291
      t299 = 0.180D3 * t52 * t297
      t308 = t41 * t12
      t309 = t27 * t217
      t312 = log(0.4D1 * t308 * t309)
      t322 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, -t284, t285, 0.
     #0D0, 0.0D0, 0.0D0)
      t325 = log(0.4D1 * t13 * t309)
      t327 = t325 ** 2
      t343 = -(0.90D2 * t4 * t7 * (t286 - t290 * t291) - t299) * t34 * t
     #36 / 0.720D3 - t8 * t291 * t58 * t210 / 0.8D1 + (0.90D2 * t4 * t7 
     #* (-t286 + t312 * t291) + t299) * t58 * t34 / 0.720D3 + (-0.90D2 *
     # t4 * t7 * (t322 - t325 * t286 + t327 * t291 / 0.2D1) + 0.180D3 * 
     #t52 * t53 * (t286 - t325 * t291) - t84 * t297) * t34 / 0.720D3
      t344 = FJET(XB1, XB2, s, -t284, 0.0D0, t285, 0.0D0, 0.0D0, t343)
      t346 = t2 * t191
      t347 = t2 * x3
      t348 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t346, 0.0D0, t
     #347, 0.0D0, 0.0D0)
      t349 = t27 * t191
      t352 = log(-0.4D1 * t308 * t349)
      t353 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t346, 0.0D0, t
     #347, 0.0D0, 0.0D0)
      t359 = t53 * t353
      t366 = t18 * t191
      t370 = log(0.4D1 * t153 * t366 * t258)
      t375 = log(-0.4D1 * t153 * t366 * x4)
      t382 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, -t346, 0.0D0, t
     #347, 0.0D0, 0.0D0)
      t385 = log(-0.4D1 * t148 * t349)
      t387 = t385 ** 2
      t403 = (-0.90D2 * t4 * t7 * (t348 - t352 * t353) + 0.180D3 * t52 *
     # t359) * t58 * t34 / 0.720D3 - t8 * (t370 * t353 - t375 * t353) * 
     #t58 * t36 / 0.16D2 + (-0.90D2 * t4 * t7 * (t382 - t385 * t348 + t3
     #87 * t353 / 0.2D1) + 0.180D3 * t52 * t53 * (t348 - t385 * t353) - 
     #t84 * t359) * t58 / 0.1440D4
      t404 = FJET(XB1, XB2, s, -t346, t347, 0.0D0, 0.0D0, 0.0D0, t403)
      t406 = KAPPA2(t185, x2, 0.10D1, t242, z)
      t407 = s * t406
      t408 = t407 * t190
      t409 = t407 * t247
      t410 = t407 * t249
      t411 = t406 ** 2
      t415 = s * t411 * t17 * t201 * x4
      t417 = 0.1D1 / (-0.2D1 + t406)
      t418 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t408, -t409, 0
     #.0D0, t410, -t415)
      t421 = t411 ** 2
      t426 = log(-0.4D1 * t63 * t217 * x4 * t20 * t421)
      t428 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t408, -t409, 0
     #.0D0, t410, -t415)
      t434 = t7 * t417
      t447 = -(0.90D2 * t4 * t7 * (t417 * t418 - t426 * t417 * t428) - 0
     #.180D3 * t231 * t434 * t428) * t34 * t36 / 0.720D3 - t4 * t434 * t
     #428 * t58 * t210 / 0.8D1
      t448 = FJET(XB1, XB2, s, -t408, 0.0D0, -t409, t410, -t415, t447)
      rrqq2qqht3s1e0 = t183 * t182 + t240 * t239 + t275 * pi * t53 * t26
     #7 * t270 * t58 * t34 * t36 / 0.8D1 + t344 * t343 + t404 * t403 + t
     #448 * t447

      end function



      doubleprecision function rrqq2qqht3s1em1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = 0.1D1 / x3
      t12 = 0.1D1 / x1
      t16 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t17 = x1 ** 2
      t19 = sin(x2 * pi)
      t20 = t19 ** 2
      t21 = t17 * t20
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t24 = t1 ** 2
      t25 = t24 ** 2
      t26 = t23 * t25
      t29 = log(0.4D1 * t21 * t26)
      t35 = pi * lh
      t36 = t3 * t7
      t39 = 0.180D3 * t35 * t36 * t9
      t43 = t20 * t23
      t44 = t25 * x4
      t47 = log(0.4D1 * t43 * t44)
      t48 = -0.1D1 + x4
      t52 = log(-0.4D1 * t43 * t44 * t48)
      t55 = 0.1D1 / x4
      t59 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t66 = log(0.4D1 * t43 * t25)
      t67 = t66 * pi
      t74 = pi ** 2
      t76 = lh ** 2
      t82 = t66 ** 2
      t90 = x3 * t20
      t93 = log(0.4D1 * t90 * t26)
      t102 = t8 * t9 * t10 * t12 / 0.8D1 + (0.90D2 * t4 * t7 * (t16 - t2
     #9 * t9) - t39) * t12 / 0.720D3 + t8 * t9 * (-t47 + t52) * t55 / 0.
     #16D2 + t4 * t7 * t59 / 0.16D2 + (-0.180D3 * t35 - 0.90D2 * t67) * 
     #t3 * t7 * t16 / 0.1440D4 + (pi * (-0.30D2 * t74 + 0.180D3 * t76) +
     # 0.180D3 * t67 * lh + 0.45D2 * t82 * pi) * t3 * t7 * t9 / 0.1440D4
     # + (0.90D2 * t4 * t7 * (t16 - t93 * t9) - t39) * t10 / 0.1440D4
      t103 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t102)
      t105 = 0.1D1 - x1
      t106 = 0.1D1 - x3
      t107 = KAPPA2(t105, x2, t106, 0.10D1, z)
      t108 = s * t107
      t109 = -t105
      t110 = t1 * t109
      t111 = -t106
      t113 = t108 * t110 * t111
      t115 = t108 * t110 * x3
      t116 = t1 * x1
      t117 = t108 * t116
      t118 = t107 ** 2
      t121 = t109 * x1
      t123 = s * t118 * t24 * t121 * x3
      t126 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t113, t117, -t1
     #15, 0.0D0, -t123)
      t129 = 0.1D1 / (-0.2D1 + t107) * t126 * t10 * t12
      t132 = FJET(XB1, XB2, s, t113, -t115, t117, 0.0D0, -t123, -t8 * t1
     #29 / 0.8D1)
      t137 = t2 * t109
      t138 = t2 * x1
      t139 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t137, t138, 0.
     #0D0, 0.0D0, 0.0D0)
      t144 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t137, t138, 0.
     #0D0, 0.0D0, 0.0D0)
      t145 = t109 ** 2
      t149 = log(0.4D1 * t21 * t26 * t145)
      t165 = -t8 * t139 * t10 * t12 / 0.8D1 + (-0.90D2 * t4 * t7 * (t144
     # - t149 * t139) + 0.180D3 * t35 * t36 * t139) * t12 / 0.720D3 - t8
     # * t139 * t12 * t55 / 0.8D1
      t166 = FJET(XB1, XB2, s, -t137, 0.0D0, t138, 0.0D0, 0.0D0, t165)
      t168 = t2 * t111
      t169 = t2 * x3
      t170 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t168, 0.0D0, t
     #169, 0.0D0, 0.0D0)
      t175 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t168, 0.0D0, t
     #169, 0.0D0, 0.0D0)
      t179 = log(-0.4D1 * t90 * t26 * t111)
      t191 = -t8 * t170 * t10 * t12 / 0.8D1 + (-0.90D2 * t4 * t7 * (t175
     # - t179 * t170) + 0.180D3 * t35 * t36 * t170) * t10 / 0.1440D4
      t192 = FJET(XB1, XB2, s, -t168, t169, 0.0D0, 0.0D0, 0.0D0, t191)
      t195 = KAPPA2(t105, x2, 0.10D1, -t48, z)
      t196 = s * t195
      t197 = t196 * t110
      t199 = t196 * t116 * t48
      t201 = t196 * t116 * x4
      t202 = t195 ** 2
      t206 = s * t202 * t24 * t121 * x4
      t209 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t197, -t199, 0
     #.0D0, t201, -t206)
      t212 = 0.1D1 / (-0.2D1 + t195) * t209 * t12 * t55
      t215 = FJET(XB1, XB2, s, -t197, 0.0D0, -t199, t201, -t206, -t8 * t
     #212 / 0.8D1)
      rrqq2qqht3s1em1 = t103 * t102 - t132 * pi * t36 * t129 / 0.8D1 + t
     #166 * t165 + t192 * t191 - t215 * pi * t36 * t212 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s1em2
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t21 = sin(x2 * pi)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t30 = log(0.4D1 * t22 / t23 * t27)
      t37 = 0.1D1 / x3
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t7 * t14 / 0.16D2 + (-0.180D3 *
     # pi * lh - 0.90D2 * t30 * pi) * t3 * t9 / 0.1440D4 + t4 * t9 * t37
     # / 0.16D2
      t42 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t41)
      t45 = t2 * (-0.1D1 + x1)
      t46 = t2 * x1
      t47 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t45, t46, 0.0D0
     #, 0.0D0, 0.0D0)
      t49 = t7 * t47 * t10
      t52 = FJET(XB1, XB2, s, -t45, 0.0D0, t46, 0.0D0, 0.0D0, -t4 * t49 
     #/ 0.8D1)
      t58 = t2 * (-0.1D1 + x3)
      t59 = t2 * x3
      t60 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t58, 0.0D0, t59
     #, 0.0D0, 0.0D0)
      t62 = t7 * t60 * t37
      t65 = FJET(XB1, XB2, s, -t58, t59, 0.0D0, 0.0D0, 0.0D0, -t4 * t62 
     #/ 0.16D2)
      rrqq2qqht3s1em2 = t42 * t41 - t52 * pi * t3 * t49 / 0.8D1 - t65 * 
     #pi * t3 * t62 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s1em3
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrqq2qqht3s1em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s1em4
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqq2qqht3s1em4 = 0.0D0

      end function


      doubleprecision function rrqq2qqht3s2e1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = x1 ** 2
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t17 * x4
      t19 = -0.1D1 + x4
      t20 = t18 * t19
      t23 = log(-0.4D1 * t15 * t20)
      t24 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t26 = t23 ** 2
      t27 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t30 = t14 * t17
      t31 = t30 * x4
      t34 = log(0.4D1 * t12 * t31)
      t36 = t34 ** 2
      t43 = pi * lh
      t44 = t3 * t7
      t52 = 0.1D1 / x1
      t54 = 0.1D1 / x4
      t57 = pi ** 2
      t59 = lh ** 2
      t61 = -0.30D2 * t57 + 0.180D3 * t59
      t62 = pi * t61
      t63 = t12 * t30
      t65 = log(0.4D1 * t63)
      t70 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t72 = t65 ** 2
      t87 = -0.240D3 * zeta3 - 0.120D3 * t59 * lh + 0.60D2 * lh * t57
      t88 = pi * t87
      t89 = t44 * t27
      t90 = t88 * t89
      t101 = t4 * t7
      t102 = x3 * t8
      t103 = t102 * t11
      t104 = x4 * t19
      t108 = log(-0.4D1 * t103 * t30 * t104)
      t112 = log(0.4D1 * t103 * t31)
      t115 = 0.1D1 / x3
      t117 = t52 * t54
      t121 = t11 * t14
      t122 = t121 * t17
      t123 = t102 * t122
      t125 = log(0.4D1 * t123)
      t127 = t125 ** 2
      t139 = t62 * t89
      t144 = t7 * t24
      t152 = log(0.4D1 * t121 * t18)
      t153 = t152 ** 2
      t156 = log(-0.4D1 * t121 * t20)
      t157 = t156 ** 2
      t161 = t7 * t27
      t169 = t7 * t70
      t182 = log(0.4D1 * t122)
      t183 = t182 * pi
      t186 = t182 ** 2
      t187 = t186 * pi
      t197 = t186 * t182 * pi
      t203 = t57 ** 2
      t204 = t59 ** 2
      t217 = t186 ** 2
      t224 = x3 * t11
      t225 = t224 * t14
      t228 = log(-0.4D1 * t225 * t20)
      t230 = t228 ** 2
      t235 = log(0.4D1 * t224 * t31)
      t237 = t235 ** 2
      t256 = log(0.4D1 * t224 * t30)
      t262 = t256 ** 2
      t282 = (0.90D2 * t4 * t7 * (t23 * t24 - t26 * t27 / 0.2D1 - t34 * 
     #t24 + t36 * t27 / 0.2D1) - 0.180D3 * t43 * t44 * (t23 * t27 - t34 
     #* t27)) * t52 * t54 / 0.720D3 + (t62 * t44 * (t24 - t65 * t27) + 0
     #.90D2 * t4 * t7 * (-t65 * t70 + t72 * t24 / 0.2D1 - t72 * t65 * t2
     #7 / 0.6D1) + t90 - 0.180D3 * t43 * t44 * (t70 - t65 * t24 + t72 * 
     #t27 / 0.2D1)) * t52 / 0.720D3 - t101 * (-t108 * t27 + t112 * t27) 
     #* t115 * t117 / 0.8D1 + (0.90D2 * t4 * t7 * (t70 - t125 * t24 + t1
     #27 * t27 / 0.2D1) - 0.180D3 * t43 * t44 * (t24 - t125 * t27) + t13
     #9) * t115 * t52 / 0.720D3 - ((-0.90D2 * t4 * t144 + 0.180D3 * t43 
     #* t89) * (t153 / 0.2D1 - t157 / 0.2D1) - 0.90D2 * t4 * t161 * (t15
     #7 * t156 / 0.6D1 - t153 * t152 / 0.6D1) + (-0.90D2 * t4 * t169 + 0
     #.180D3 * t43 * t44 * t24 - t139) * (-t152 + t156)) * t54 / 0.1440D
     #4 + (t62 + 0.180D3 * t183 * lh + 0.45D2 * t187) * t3 * t169 / 0.14
     #40D4 + (t88 - t183 * t61 - 0.90D2 * t187 * lh - 0.15D2 * t197) * t
     #3 * t144 / 0.1440D4 + (pi * (t203 + 0.60D2 * t204 + 0.480D3 * lh *
     # zeta3 - 0.60D2 * t59 * t57) - t183 * t87 + t187 * t61 / 0.2D1 + 0
     #.30D2 * t197 * lh + 0.15D2 / 0.4D1 * t217 * pi) * t3 * t161 / 0.14
     #40D4 + (0.90D2 * t4 * t7 * (t228 * t24 - t230 * t27 / 0.2D1 - t235
     # * t24 + t237 * t27 / 0.2D1) - 0.180D3 * t43 * t44 * (t228 * t27 -
     # t235 * t27)) * t115 * t54 / 0.1440D4 - (t62 * t44 * (-t24 + t256 
     #* t27) + 0.90D2 * t4 * t7 * (t256 * t70 - t262 * t24 / 0.2D1 + t26
     #2 * t256 * t27 / 0.6D1) - t90 - 0.180D3 * t43 * t44 * (-t70 + t256
     # * t24 - t262 * t27 / 0.2D1)) * t115 / 0.1440D4
      t283 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t282)
      t285 = 0.1D1 - x1
      t286 = 0.1D1 - x3
      t287 = KAPPA2(t285, x2, t286, 0.0D0, z)
      t288 = s * t287
      t289 = -t285
      t290 = t1 * t289
      t291 = -t286
      t292 = t290 * t291
      t293 = t288 * t292
      t294 = t290 * x3
      t295 = t288 * t294
      t296 = t1 * x1
      t297 = t288 * t296
      t298 = t287 ** 2
      t301 = t289 * x1
      t303 = s * t298 * t16 * t301 * t291
      t305 = 0.1D1 / (-0.2D1 + t287)
      t306 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t293, 0.0D0, -t
     #295, t297, t303)
      t307 = t305 * t306
      t308 = t102 * t121
      t309 = t289 ** 2
      t310 = t17 * t309
      t311 = t291 * x4
      t312 = t298 ** 2
      t317 = log(-0.4D1 * t308 * t310 * t311 * t312)
      t319 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t293, 0.0D0, -t
     #295, t297, t303)
      t325 = t43 * t3
      t327 = t7 * t305 * t319
      t333 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t293, 0.0D0, -t
     #295, t297, t303)
      t339 = log(-0.4D1 * t308 * t310 * t291 * t312)
      t340 = t339 * t305
      t342 = t339 ** 2
      t355 = t62 * t3
      t361 = -(0.90D2 * t4 * t7 * (t307 - t317 * t305 * t319) - 0.180D3 
     #* t325 * t327) * t115 * t117 / 0.720D3 + (0.90D2 * t4 * t7 * (-t30
     #5 * t333 + t340 * t306 - t342 * t305 * t319 / 0.2D1) - 0.180D3 * t
     #43 * t44 * (-t307 + t340 * t319) - t355 * t327) * t115 * t52 / 0.7
     #20D3
      t362 = FJET(XB1, XB2, s, t293, -t295, 0.0D0, t297, t303, t361)
      t364 = KAPPA2(t285, x2, t286, x4, z)
      t365 = s * t364
      t366 = t365 * t292
      t367 = t365 * t294
      t368 = t296 * x4
      t369 = t365 * t368
      t370 = t296 * t19
      t371 = t365 * t370
      t372 = t364 ** 2
      t377 = cos(t9)
      t380 = Sqrt(x3 * t291 * t104)
      t385 = s * t372 * t16 * t301 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t377 * t380)
      t387 = 0.1D1 / (-0.2D1 + t364)
      t388 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t366, t369, -t3
     #67, -t371, t385)
      t391 = t372 ** 2
      t396 = log(0.4D1 * t123 * t309 * t291 * t104 * t391)
      t398 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t366, t369, -t3
     #67, -t371, t385)
      t408 = 0.90D2 * t4 * t7 * (-t387 * t388 + t396 * t387 * t398) + 0.
     #180D3 * t325 * t7 * t387 * t398
      t412 = FJET(XB1, XB2, s, t366, -t367, t369, -t371, t385, -t408 * t
     #115 * t117 / 0.720D3)
      t418 = t2 * t291
      t419 = t2 * x3
      t423 = log(-0.4D1 * t103 * t30 * t311)
      t424 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t418, 0.0D0, t
     #419, 0.0D0, 0.0D0)
      t426 = t17 * t291
      t427 = t426 * t104
      t430 = log(0.4D1 * t308 * t427)
      t437 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, -t418, 0.0D0, t
     #419, 0.0D0, 0.0D0)
      t438 = t30 * t291
      t441 = log(-0.4D1 * t103 * t438)
      t442 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t418, 0.0D0, t
     #419, 0.0D0, 0.0D0)
      t444 = t441 ** 2
      t456 = t44 * t424
      t464 = log(0.4D1 * t225 * t427)
      t466 = t464 ** 2
      t472 = log(-0.4D1 * t225 * t426 * x4)
      t474 = t472 ** 2
      t493 = log(-0.4D1 * t224 * t438)
      t499 = t493 ** 2
      t520 = -t101 * (-t423 * t424 + t430 * t424) * t115 * t117 / 0.8D1 
     #+ (0.90D2 * t4 * t7 * (-t437 + t441 * t442 - t444 * t424 / 0.2D1) 
     #- 0.180D3 * t43 * t44 * (-t442 + t441 * t424) - t62 * t456) * t115
     # * t52 / 0.720D3 + (0.90D2 * t4 * t7 * (-t464 * t442 + t466 * t424
     # / 0.2D1 + t472 * t442 - t474 * t424 / 0.2D1) - 0.180D3 * t43 * t4
     #4 * (-t464 * t424 + t472 * t424)) * t115 * t54 / 0.1440D4 - (t62 *
     # t44 * (t442 - t493 * t424) + 0.90D2 * t4 * t7 * (-t493 * t437 + t
     #499 * t442 / 0.2D1 - t499 * t493 * t424 / 0.6D1) + t88 * t456 - 0.
     #180D3 * t43 * t44 * (t437 - t493 * t442 + t499 * t424 / 0.2D1)) * 
     #t115 / 0.1440D4
      t521 = FJET(XB1, XB2, s, -t418, t419, 0.0D0, 0.0D0, 0.0D0, t520)
      t523 = KAPPA2(t285, x2, 0.10D1, 0.0D0, z)
      t524 = s * t523
      t525 = t524 * t290
      t526 = t524 * t296
      t527 = t523 ** 2
      t531 = s * t527 * t16 * t289 * x1
      t533 = 0.1D1 / (-0.2D1 + t523)
      t534 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, -t525, 0.0D0, 0
     #.0D0, t526, -t531)
      t535 = t533 * t534
      t536 = t527 ** 2
      t538 = t310 * x4 * t536
      t541 = log(0.4D1 * t15 * t538)
      t542 = t541 * t533
      t543 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t525, 0.0D0, 0
     #.0D0, t526, -t531)
      t545 = t541 ** 2
      t547 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t525, 0.0D0, 0
     #.0D0, t526, -t531)
      t554 = t533 * t543
      t561 = t7 * t533 * t547
      t562 = t355 * t561
      t569 = log(0.4D1 * t15 * t310 * t536)
      t570 = t569 * t533
      t576 = t569 ** 2
      t577 = t576 * t533
      t601 = log(0.4D1 * t308 * t538)
      t617 = log(0.4D1 * t103 * t30 * t309 * t536)
      t618 = t617 * t533
      t620 = t617 ** 2
      t637 = (0.90D2 * t4 * t7 * (t535 - t542 * t543 + t545 * t533 * t54
     #7 / 0.2D1) - 0.180D3 * t43 * t44 * (t554 - t542 * t547) + t562) * 
     #t52 * t54 / 0.720D3 + (t62 * t44 * (t554 - t570 * t547) + 0.90D2 *
     # t4 * t7 * (-t570 * t534 + t577 * t543 / 0.2D1 - t576 * t569 * t53
     #3 * t547 / 0.6D1) + t88 * t3 * t561 - 0.180D3 * t43 * t44 * (t535 
     #- t570 * t543 + t577 * t547 / 0.2D1)) * t52 / 0.720D3 - (0.90D2 * 
     #t4 * t7 * (-t554 + t601 * t533 * t547) + 0.180D3 * t325 * t561) * 
     #t115 * t117 / 0.720D3 + (0.90D2 * t4 * t7 * (t535 - t618 * t543 + 
     #t620 * t533 * t547 / 0.2D1) - 0.180D3 * t43 * t44 * (t554 - t618 *
     # t547) + t562) * t115 * t52 / 0.720D3
      t638 = FJET(XB1, XB2, s, -t525, 0.0D0, 0.0D0, t526, -t531, t637)
      t640 = KAPPA2(t285, x2, 0.10D1, x4, z)
      t641 = s * t640
      t642 = t641 * t290
      t643 = t641 * t368
      t644 = t641 * t370
      t645 = t640 ** 2
      t649 = s * t645 * t16 * t301 * t19
      t651 = 0.1D1 / (-0.2D1 + t640)
      t652 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, -t642, t643, 0.
     #0D0, -t644, t649)
      t655 = t645 ** 2
      t660 = log(-0.4D1 * t63 * t309 * x4 * t19 * t655)
      t661 = t660 * t651
      t662 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t642, t643, 0.
     #0D0, -t644, t649)
      t664 = t660 ** 2
      t666 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t642, t643, 0.
     #0D0, -t644, t649)
      t673 = t651 * t662
      t680 = t7 * t651 * t666
      t689 = log(-0.4D1 * t308 * t310 * t104 * t655)
      t702 = (0.90D2 * t4 * t7 * (-t651 * t652 + t661 * t662 - t664 * t6
     #51 * t666 / 0.2D1) - 0.180D3 * t43 * t44 * (-t673 + t661 * t666) -
     # t355 * t680) * t52 * t54 / 0.720D3 - (-0.90D2 * t4 * t7 * (-t673 
     #+ t689 * t651 * t666) - 0.180D3 * t325 * t680) * t115 * t117 / 0.7
     #20D3
      t703 = FJET(XB1, XB2, s, -t642, 0.0D0, t643, -t644, t649, t702)
      rrqq2qqht3s2e1 = t283 * t282 + t362 * t361 - t412 * t408 * t115 * 
     #t52 * t54 / 0.720D3 + t521 * t520 + t638 * t637 + t703 * t702

      end function



      doubleprecision function rrqq2qqht3s2e0
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = x1 ** 2
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t18 * x4
      t20 = -0.1D1 + x4
      t21 = t19 * t20
      t24 = log(-0.4D1 * t16 * t21)
      t25 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t27 = t15 * t18
      t28 = t27 * x4
      t31 = log(0.4D1 * t13 * t28)
      t34 = 0.1D1 / x1
      t36 = 0.1D1 / x4
      t40 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t41 = x3 * t9
      t42 = t12 * t15
      t43 = t42 * t18
      t46 = log(0.4D1 * t41 * t43)
      t52 = pi * lh
      t53 = t3 * t7
      t54 = t53 * t25
      t56 = 0.180D3 * t52 * t54
      t58 = 0.1D1 / x3
      t62 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t63 = t13 * t27
      t65 = log(0.4D1 * t63)
      t67 = t65 ** 2
      t79 = pi ** 2
      t81 = lh ** 2
      t83 = -0.30D2 * t79 + 0.180D3 * t81
      t84 = pi * t83
      t85 = t84 * t54
      t89 = t7 * t25
      t92 = log(0.4D1 * t42 * t19)
      t93 = t92 ** 2
      t96 = log(-0.4D1 * t42 * t21)
      t97 = t96 ** 2
      t103 = t7 * t40
      t114 = log(0.4D1 * t43)
      t115 = t114 * pi
      t124 = t114 ** 2
      t125 = t124 * pi
      t148 = x3 * t12
      t149 = t148 * t15
      t152 = log(-0.4D1 * t149 * t21)
      t156 = log(0.4D1 * t148 * t28)
      t165 = log(0.4D1 * t148 * t27)
      t167 = t165 ** 2
      t182 = t8 * (t24 * t25 - t31 * t25) * t34 * t36 / 0.8D1 + (0.90D2 
     #* t4 * t7 * (t40 - t46 * t25) - t56) * t58 * t34 / 0.720D3 + (0.90
     #D2 * t4 * t7 * (t62 - t65 * t40 + t67 * t25 / 0.2D1) - 0.180D3 * t
     #52 * t53 * (t40 - t65 * t25) + t85) * t34 / 0.720D3 - (-0.90D2 * t
     #4 * t89 * (t93 / 0.2D1 - t97 / 0.2D1) + (-0.90D2 * t4 * t103 + t56
     #) * (-t92 + t96)) * t36 / 0.1440D4 + (-0.180D3 * t52 - 0.90D2 * t1
     #15) * t3 * t7 * t62 / 0.1440D4 + (t84 + 0.180D3 * t115 * lh + 0.45
     #D2 * t125) * t3 * t103 / 0.1440D4 + (pi * (-0.240D3 * zeta3 - 0.12
     #0D3 * t81 * lh + 0.60D2 * lh * t79) - t115 * t83 - 0.90D2 * t125 *
     # lh - 0.15D2 * t124 * t114 * pi) * t3 * t89 / 0.1440D4 + t8 * (t15
     #2 * t25 - t156 * t25) * t58 * t36 / 0.16D2 - (0.90D2 * t4 * t7 * (
     #-t62 + t165 * t40 - t167 * t25 / 0.2D1) - 0.180D3 * t52 * t53 * (-
     #t40 + t165 * t25) - t85) * t58 / 0.1440D4
      t183 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t182)
      t185 = 0.1D1 - x1
      t186 = 0.1D1 - x3
      t187 = KAPPA2(t185, x2, t186, 0.0D0, z)
      t188 = s * t187
      t189 = -t185
      t190 = t1 * t189
      t191 = -t186
      t192 = t190 * t191
      t193 = t188 * t192
      t194 = t190 * x3
      t195 = t188 * t194
      t196 = t1 * x1
      t197 = t188 * t196
      t198 = t187 ** 2
      t201 = t189 * x1
      t203 = s * t198 * t17 * t201 * t191
      t205 = 0.1D1 / (-0.2D1 + t187)
      t206 = t7 * t205
      t208 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t193, 0.0D0, -t
     #195, t197, t203)
      t210 = t34 * t36
      t214 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t193, 0.0D0, -t
     #195, t197, t203)
      t217 = t189 ** 2
      t218 = t18 * t217
      t219 = t198 ** 2
      t224 = log(-0.4D1 * t41 * t42 * t218 * t191 * t219)
      t231 = t52 * t3
      t239 = -t4 * t206 * t208 * t58 * t210 / 0.8D1 + (0.90D2 * t4 * t7 
     #* (-t205 * t214 + t224 * t205 * t208) + 0.180D3 * t231 * t206 * t2
     #08) * t58 * t34 / 0.720D3
      t240 = FJET(XB1, XB2, s, t193, -t195, 0.0D0, t197, t203, t239)
      t242 = KAPPA2(t185, x2, t186, x4, z)
      t243 = s * t242
      t244 = t243 * t192
      t245 = t243 * t194
      t246 = t196 * x4
      t247 = t243 * t246
      t248 = t196 * t20
      t249 = t243 * t248
      t250 = t242 ** 2
      t255 = cos(t10)
      t257 = x4 * t20
      t259 = Sqrt(x3 * t191 * t257)
      t264 = s * t250 * t17 * t201 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t255 * t259)
      t266 = 0.1D1 / (-0.2D1 + t242)
      t269 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t244, t247, -t2
     #45, -t249, t264)
      t274 = FJET(XB1, XB2, s, t244, -t245, t247, -t249, t264, t4 * t7 *
     # t266 * t269 * t58 * t210 / 0.8D1)
      t283 = t2 * t191
      t284 = t2 * x3
      t285 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t283, 0.0D0, t
     #284, 0.0D0, 0.0D0)
      t286 = t41 * t12
      t287 = t27 * t191
      t290 = log(-0.4D1 * t286 * t287)
      t291 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t283, 0.0D0, t
     #284, 0.0D0, 0.0D0)
      t297 = t53 * t291
      t304 = t18 * t191
      t308 = log(0.4D1 * t149 * t304 * t257)
      t313 = log(-0.4D1 * t149 * t304 * x4)
      t320 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, -t283, 0.0D0, t
     #284, 0.0D0, 0.0D0)
      t323 = log(-0.4D1 * t148 * t287)
      t325 = t323 ** 2
      t341 = (0.90D2 * t4 * t7 * (-t285 + t290 * t291) + 0.180D3 * t52 *
     # t297) * t58 * t34 / 0.720D3 + t8 * (-t308 * t291 + t313 * t291) *
     # t58 * t36 / 0.16D2 - (0.90D2 * t4 * t7 * (t320 - t323 * t285 + t3
     #25 * t291 / 0.2D1) - 0.180D3 * t52 * t53 * (t285 - t323 * t291) + 
     #t84 * t297) * t58 / 0.1440D4
      t342 = FJET(XB1, XB2, s, -t283, t284, 0.0D0, 0.0D0, 0.0D0, t341)
      t344 = KAPPA2(t185, x2, 0.10D1, 0.0D0, z)
      t345 = s * t344
      t346 = t345 * t190
      t347 = t345 * t196
      t348 = t344 ** 2
      t352 = s * t348 * t17 * t189 * x1
      t354 = 0.1D1 / (-0.2D1 + t344)
      t355 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t346, 0.0D0, 0
     #.0D0, t347, -t352)
      t356 = t354 * t355
      t357 = t348 ** 2
      t362 = log(0.4D1 * t16 * t218 * x4 * t357)
      t364 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t346, 0.0D0, 0
     #.0D0, t347, -t352)
      t370 = t7 * t354
      t371 = t370 * t364
      t373 = 0.180D3 * t231 * t371
      t387 = log(0.4D1 * t286 * t27 * t217 * t357)
      t398 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, -t346, 0.0D0, 0
     #.0D0, t347, -t352)
      t403 = log(0.4D1 * t16 * t218 * t357)
      t404 = t403 * t354
      t406 = t403 ** 2
      t424 = (0.90D2 * t4 * t7 * (t356 - t362 * t354 * t364) - t373) * t
     #34 * t36 / 0.720D3 + t4 * t370 * t364 * t58 * t210 / 0.8D1 + (0.90
     #D2 * t4 * t7 * (t356 - t387 * t354 * t364) - t373) * t58 * t34 / 0
     #.720D3 + (0.90D2 * t4 * t7 * (t354 * t398 - t404 * t355 + t406 * t
     #354 * t364 / 0.2D1) - 0.180D3 * t52 * t53 * (t356 - t404 * t364) +
     # t84 * t3 * t371) * t34 / 0.720D3
      t425 = FJET(XB1, XB2, s, -t346, 0.0D0, 0.0D0, t347, -t352, t424)
      t427 = KAPPA2(t185, x2, 0.10D1, x4, z)
      t428 = s * t427
      t429 = t428 * t190
      t430 = t428 * t246
      t431 = t428 * t248
      t432 = t427 ** 2
      t436 = s * t432 * t17 * t201 * t20
      t438 = 0.1D1 / (-0.2D1 + t427)
      t439 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t429, t430, 0.
     #0D0, -t431, t436)
      t442 = t432 ** 2
      t447 = log(-0.4D1 * t63 * t217 * x4 * t20 * t442)
      t449 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t429, t430, 0.
     #0D0, -t431, t436)
      t455 = t7 * t438
      t468 = (0.90D2 * t4 * t7 * (-t438 * t439 + t447 * t438 * t449) + 0
     #.180D3 * t231 * t455 * t449) * t34 * t36 / 0.720D3 - t4 * t455 * t
     #449 * t58 * t210 / 0.8D1
      t469 = FJET(XB1, XB2, s, -t429, 0.0D0, t430, -t431, t436, t468)
      rrqq2qqht3s2e0 = t183 * t182 + t240 * t239 + t274 * pi * t53 * t26
     #6 * t269 * t58 * t34 * t36 / 0.8D1 + t342 * t341 + t425 * t424 + t
     #469 * t468

      end function



      doubleprecision function rrqq2qqht3s2em1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = 0.1D1 / x3
      t12 = 0.1D1 / x1
      t16 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t17 = x1 ** 2
      t19 = sin(x2 * pi)
      t20 = t19 ** 2
      t21 = t17 * t20
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t24 = t1 ** 2
      t25 = t24 ** 2
      t26 = t23 * t25
      t29 = log(0.4D1 * t21 * t26)
      t35 = pi * lh
      t36 = t3 * t7
      t39 = 0.180D3 * t35 * t36 * t9
      t43 = t20 * t23
      t44 = t25 * x4
      t47 = log(0.4D1 * t43 * t44)
      t48 = -0.1D1 + x4
      t52 = log(-0.4D1 * t43 * t44 * t48)
      t55 = 0.1D1 / x4
      t59 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t66 = log(0.4D1 * t43 * t25)
      t67 = t66 * pi
      t74 = pi ** 2
      t76 = lh ** 2
      t82 = t66 ** 2
      t90 = x3 * t20
      t93 = log(0.4D1 * t90 * t26)
      t102 = t8 * t9 * t10 * t12 / 0.8D1 + (0.90D2 * t4 * t7 * (t16 - t2
     #9 * t9) - t39) * t12 / 0.720D3 + t8 * t9 * (-t47 + t52) * t55 / 0.
     #16D2 + t4 * t7 * t59 / 0.16D2 + (-0.180D3 * t35 - 0.90D2 * t67) * 
     #t3 * t7 * t16 / 0.1440D4 + (pi * (-0.30D2 * t74 + 0.180D3 * t76) +
     # 0.180D3 * t67 * lh + 0.45D2 * t82 * pi) * t3 * t7 * t9 / 0.1440D4
     # - (0.90D2 * t4 * t7 * (-t16 + t93 * t9) + t39) * t10 / 0.1440D4
      t103 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t102)
      t105 = 0.1D1 - x1
      t106 = 0.1D1 - x3
      t107 = KAPPA2(t105, x2, t106, 0.0D0, z)
      t108 = s * t107
      t109 = -t105
      t110 = t1 * t109
      t111 = -t106
      t113 = t108 * t110 * t111
      t115 = t108 * t110 * x3
      t116 = t1 * x1
      t117 = t108 * t116
      t118 = t107 ** 2
      t121 = t109 * x1
      t123 = s * t118 * t24 * t121 * t111
      t126 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t113, 0.0D0, -t
     #115, t117, t123)
      t128 = t10 * t12
      t129 = 0.1D1 / (-0.2D1 + t107) * t126 * t128
      t132 = FJET(XB1, XB2, s, t113, -t115, 0.0D0, t117, t123, -t8 * t12
     #9 / 0.8D1)
      t137 = t2 * t111
      t138 = t2 * x3
      t139 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t137, 0.0D0, t
     #138, 0.0D0, 0.0D0)
      t144 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t137, 0.0D0, t
     #138, 0.0D0, 0.0D0)
      t148 = log(-0.4D1 * t90 * t26 * t111)
      t160 = -t8 * t139 * t10 * t12 / 0.8D1 - (0.90D2 * t4 * t7 * (t144 
     #- t148 * t139) - 0.180D3 * t35 * t36 * t139) * t10 / 0.1440D4
      t161 = FJET(XB1, XB2, s, -t137, t138, 0.0D0, 0.0D0, 0.0D0, t160)
      t163 = KAPPA2(t105, x2, 0.10D1, 0.0D0, z)
      t164 = s * t163
      t165 = t164 * t110
      t166 = t164 * t116
      t167 = t163 ** 2
      t171 = s * t167 * t24 * t109 * x1
      t173 = 0.1D1 / (-0.2D1 + t163)
      t174 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t165, 0.0D0, 0
     #.0D0, t166, -t171)
      t175 = t173 * t174
      t179 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t165, 0.0D0, 0
     #.0D0, t166, -t171)
      t182 = t109 ** 2
      t184 = t167 ** 2
      t188 = log(0.4D1 * t21 * t23 * t25 * t182 * t184)
      t203 = t12 * t55
      t207 = t8 * t175 * t128 / 0.8D1 + (0.90D2 * t4 * t7 * (t173 * t179
     # - t188 * t173 * t174) - 0.180D3 * t35 * t3 * t7 * t173 * t174) * 
     #t12 / 0.720D3 + t8 * t175 * t203 / 0.8D1
      t208 = FJET(XB1, XB2, s, -t165, 0.0D0, 0.0D0, t166, -t171, t207)
      t210 = KAPPA2(t105, x2, 0.10D1, x4, z)
      t211 = s * t210
      t212 = t211 * t110
      t214 = t211 * t116 * x4
      t216 = t211 * t116 * t48
      t217 = t210 ** 2
      t221 = s * t217 * t24 * t121 * t48
      t224 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t212, t214, 0.
     #0D0, -t216, t221)
      t226 = 0.1D1 / (-0.2D1 + t210) * t224 * t203
      t229 = FJET(XB1, XB2, s, -t212, 0.0D0, t214, -t216, t221, -t8 * t2
     #26 / 0.8D1)
      rrqq2qqht3s2em1 = t103 * t102 - t132 * pi * t36 * t129 / 0.8D1 + t
     #161 * t160 + t208 * t207 - t229 * pi * t36 * t226 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s2em2
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t21 = sin(x2 * pi)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t30 = log(0.4D1 * t22 / t23 * t27)
      t37 = 0.1D1 / x3
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t7 * t14 / 0.16D2 + (-0.180D3 *
     # pi * lh - 0.90D2 * t30 * pi) * t3 * t9 / 0.1440D4 + t4 * t9 * t37
     # / 0.16D2
      t42 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t41)
      t44 = 0.1D1 - x1
      t45 = KAPPA2(t44, x2, 0.10D1, 0.0D0, z)
      t46 = s * t45
      t47 = -t44
      t49 = t46 * t1 * t47
      t51 = t46 * t1 * x1
      t52 = t45 ** 2
      t56 = s * t52 * t26 * t47 * x1
      t59 = 0.1D1 / (-0.2D1 + t45)
      t60 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t49, 0.0D0, 0.0
     #D0, t51, -t56)
      t65 = FJET(XB1, XB2, s, -t49, 0.0D0, 0.0D0, t51, -t56, t4 * t7 * t
     #59 * t60 * t10 / 0.8D1)
      t74 = t2 * (-0.1D1 + x3)
      t75 = t2 * x3
      t76 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t74, 0.0D0, t75
     #, 0.0D0, 0.0D0)
      t78 = t7 * t76 * t37
      t81 = FJET(XB1, XB2, s, -t74, t75, 0.0D0, 0.0D0, 0.0D0, -t4 * t78 
     #/ 0.16D2)
      rrqq2qqht3s2em2 = t42 * t41 + t65 * pi * t3 * t7 * t59 * t60 * t10
     # / 0.8D1 - t81 * pi * t3 * t78 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s2em3
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrqq2qqht3s2em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s2em4
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqq2qqht3s2em4 = 0.0D0

      end function


      doubleprecision function rrqq2qqht3s3e1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = x1 ** 2
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t17 * x4
      t19 = -0.1D1 + x4
      t20 = t18 * t19
      t23 = log(-0.4D1 * t15 * t20)
      t24 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t26 = t23 ** 2
      t27 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t30 = t14 * t17
      t31 = t30 * x4
      t34 = log(0.4D1 * t12 * t31)
      t36 = t34 ** 2
      t43 = pi * lh
      t44 = t3 * t7
      t52 = 0.1D1 / x1
      t54 = 0.1D1 / x4
      t57 = pi ** 2
      t59 = lh ** 2
      t61 = -0.30D2 * t57 + 0.180D3 * t59
      t62 = pi * t61
      t63 = t12 * t30
      t65 = log(0.4D1 * t63)
      t70 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t72 = t65 ** 2
      t87 = -0.240D3 * zeta3 - 0.120D3 * t59 * lh + 0.60D2 * lh * t57
      t88 = pi * t87
      t89 = t44 * t27
      t90 = t88 * t89
      t101 = t4 * t7
      t102 = x3 * t8
      t103 = t102 * t11
      t106 = log(0.4D1 * t103 * t31)
      t108 = x4 * t19
      t112 = log(-0.4D1 * t103 * t30 * t108)
      t115 = 0.1D1 / x3
      t117 = t52 * t54
      t121 = t11 * t14
      t122 = t121 * t17
      t123 = t102 * t122
      t125 = log(0.4D1 * t123)
      t127 = t125 ** 2
      t139 = t62 * t89
      t144 = t7 * t24
      t152 = log(0.4D1 * t121 * t18)
      t153 = t152 ** 2
      t156 = log(-0.4D1 * t121 * t20)
      t157 = t156 ** 2
      t161 = t7 * t27
      t169 = t7 * t70
      t182 = log(0.4D1 * t122)
      t183 = t182 * pi
      t186 = t182 ** 2
      t187 = t186 * pi
      t197 = t186 * t182 * pi
      t203 = t57 ** 2
      t204 = t59 ** 2
      t217 = t186 ** 2
      t224 = x3 * t11
      t225 = t224 * t14
      t228 = log(-0.4D1 * t225 * t20)
      t230 = t228 ** 2
      t235 = log(0.4D1 * t224 * t31)
      t237 = t235 ** 2
      t256 = log(0.4D1 * t224 * t30)
      t262 = t256 ** 2
      t282 = (0.90D2 * t4 * t7 * (t23 * t24 - t26 * t27 / 0.2D1 - t34 * 
     #t24 + t36 * t27 / 0.2D1) - 0.180D3 * t43 * t44 * (t23 * t27 - t34 
     #* t27)) * t52 * t54 / 0.720D3 + (t62 * t44 * (t24 - t65 * t27) + 0
     #.90D2 * t4 * t7 * (-t65 * t70 + t72 * t24 / 0.2D1 - t72 * t65 * t2
     #7 / 0.6D1) + t90 - 0.180D3 * t43 * t44 * (t70 - t65 * t24 + t72 * 
     #t27 / 0.2D1)) * t52 / 0.720D3 - t101 * (t106 * t27 - t112 * t27) *
     # t115 * t117 / 0.8D1 + (0.90D2 * t4 * t7 * (t70 - t125 * t24 + t12
     #7 * t27 / 0.2D1) - 0.180D3 * t43 * t44 * (t24 - t125 * t27) + t139
     #) * t115 * t52 / 0.720D3 - ((-0.90D2 * t4 * t144 + 0.180D3 * t43 *
     # t89) * (t153 / 0.2D1 - t157 / 0.2D1) - 0.90D2 * t4 * t161 * (t157
     # * t156 / 0.6D1 - t153 * t152 / 0.6D1) + (-0.90D2 * t4 * t169 + 0.
     #180D3 * t43 * t44 * t24 - t139) * (-t152 + t156)) * t54 / 0.1440D4
     # + (t62 + 0.180D3 * t183 * lh + 0.45D2 * t187) * t3 * t169 / 0.144
     #0D4 + (t88 - t183 * t61 - 0.90D2 * t187 * lh - 0.15D2 * t197) * t3
     # * t144 / 0.1440D4 + (pi * (t203 + 0.60D2 * t204 + 0.480D3 * lh * 
     #zeta3 - 0.60D2 * t59 * t57) - t183 * t87 + t187 * t61 / 0.2D1 + 0.
     #30D2 * t197 * lh + 0.15D2 / 0.4D1 * t217 * pi) * t3 * t161 / 0.144
     #0D4 + (0.90D2 * t4 * t7 * (t228 * t24 - t230 * t27 / 0.2D1 - t235 
     #* t24 + t237 * t27 / 0.2D1) - 0.180D3 * t43 * t44 * (t228 * t27 - 
     #t235 * t27)) * t115 * t54 / 0.1440D4 - (t62 * t44 * (-t24 + t256 *
     # t27) + 0.90D2 * t4 * t7 * (t256 * t70 - t262 * t24 / 0.2D1 + t262
     # * t256 * t27 / 0.6D1) - t90 - 0.180D3 * t43 * t44 * (-t70 + t256 
     #* t24 - t262 * t27 / 0.2D1)) * t115 / 0.1440D4
      t283 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t282)
      t285 = 0.1D1 - x1
      t286 = KAPPA2(t285, x2, 0.0D0, 0.10D1, z)
      t287 = s * t286
      t288 = -t285
      t289 = t1 * t288
      t290 = t287 * t289
      t291 = t1 * x1
      t292 = t287 * t291
      t293 = t286 ** 2
      t297 = s * t293 * t16 * t288 * x1
      t299 = 0.1D1 / (-0.2D1 + t286)
      t300 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t292, -t
     #290, 0.0D0, -t297)
      t301 = t299 * t300
      t302 = t288 ** 2
      t303 = t17 * t302
      t304 = t293 ** 2
      t306 = t303 * x4 * t304
      t309 = log(0.4D1 * t15 * t306)
      t310 = t309 * t299
      t311 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t292, -t
     #290, 0.0D0, -t297)
      t313 = t309 ** 2
      t315 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t292, -t
     #290, 0.0D0, -t297)
      t322 = t299 * t311
      t328 = t62 * t3
      t330 = t7 * t299 * t315
      t331 = t328 * t330
      t338 = log(0.4D1 * t15 * t303 * t304)
      t339 = t338 * t299
      t345 = t338 ** 2
      t346 = t345 * t299
      t368 = t102 * t121
      t371 = log(0.4D1 * t368 * t306)
      t378 = t43 * t3
      t388 = log(0.4D1 * t103 * t30 * t302 * t304)
      t389 = t388 * t299
      t391 = t388 ** 2
      t408 = (0.90D2 * t4 * t7 * (t301 - t310 * t311 + t313 * t299 * t31
     #5 / 0.2D1) - 0.180D3 * t43 * t44 * (t322 - t310 * t315) + t331) * 
     #t52 * t54 / 0.720D3 + (t62 * t44 * (t322 - t339 * t315) + 0.90D2 *
     # t4 * t7 * (-t339 * t300 + t346 * t311 / 0.2D1 - t345 * t338 * t29
     #9 * t315 / 0.6D1) + t88 * t3 * t330 - 0.180D3 * t43 * t44 * (t301 
     #- t339 * t311 + t346 * t315 / 0.2D1)) * t52 / 0.720D3 - (0.90D2 * 
     #t4 * t7 * (-t322 + t371 * t299 * t315) + 0.180D3 * t378 * t330) * 
     #t115 * t117 / 0.720D3 + (0.90D2 * t4 * t7 * (t301 - t389 * t311 + 
     #t391 * t299 * t315 / 0.2D1) - 0.180D3 * t43 * t44 * (t322 - t389 *
     # t315) + t331) * t115 * t52 / 0.720D3
      t409 = FJET(XB1, XB2, s, 0.0D0, -t290, t292, 0.0D0, -t297, t408)
      t411 = -t19
      t412 = KAPPA2(t285, x2, 0.0D0, t411, z)
      t413 = s * t412
      t414 = t413 * t289
      t415 = t291 * t19
      t416 = t413 * t415
      t417 = t291 * x4
      t418 = t413 * t417
      t419 = t412 ** 2
      t422 = t288 * x1
      t424 = s * t419 * t16 * t422 * t19
      t426 = 0.1D1 / (-0.2D1 + t412)
      t427 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t416, -
     #t414, t418, t424)
      t430 = t419 ** 2
      t435 = log(-0.4D1 * t63 * t302 * x4 * t19 * t430)
      t436 = t435 * t426
      t437 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t416, -
     #t414, t418, t424)
      t439 = t435 ** 2
      t441 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t416, -
     #t414, t418, t424)
      t448 = t426 * t437
      t455 = t7 * t426 * t441
      t464 = log(-0.4D1 * t368 * t303 * t108 * t430)
      t477 = (-0.90D2 * t4 * t7 * (t426 * t427 - t436 * t437 + t439 * t4
     #26 * t441 / 0.2D1) + 0.180D3 * t43 * t44 * (t448 - t436 * t441) - 
     #t328 * t455) * t52 * t54 / 0.720D3 - (0.90D2 * t4 * t7 * (t448 - t
     #464 * t426 * t441) - 0.180D3 * t378 * t455) * t115 * t117 / 0.720D
     #3
      t478 = FJET(XB1, XB2, s, 0.0D0, -t414, -t416, t418, t424, t477)
      t480 = t2 * x3
      t481 = -0.1D1 + x3
      t482 = t2 * t481
      t483 = t481 * x4
      t487 = log(-0.4D1 * t103 * t30 * t483)
      t488 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t480, 0.0D0, -t
     #482, 0.0D0, 0.0D0)
      t490 = t17 * t481
      t491 = t490 * t108
      t494 = log(0.4D1 * t368 * t491)
      t501 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t480, 0.0D0, -t
     #482, 0.0D0, 0.0D0)
      t502 = t30 * t481
      t505 = log(-0.4D1 * t103 * t502)
      t506 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t480, 0.0D0, -t
     #482, 0.0D0, 0.0D0)
      t508 = t505 ** 2
      t520 = t44 * t488
      t528 = log(0.4D1 * t225 * t491)
      t530 = t528 ** 2
      t536 = log(-0.4D1 * t225 * t490 * x4)
      t538 = t536 ** 2
      t557 = log(-0.4D1 * t224 * t502)
      t563 = t557 ** 2
      t584 = -t101 * (-t487 * t488 + t494 * t488) * t115 * t117 / 0.8D1 
     #+ (0.90D2 * t4 * t7 * (-t501 + t505 * t506 - t508 * t488 / 0.2D1) 
     #- 0.180D3 * t43 * t44 * (-t506 + t505 * t488) - t62 * t520) * t115
     # * t52 / 0.720D3 + (0.90D2 * t4 * t7 * (-t528 * t506 + t530 * t488
     # / 0.2D1 + t536 * t506 - t538 * t488 / 0.2D1) - 0.180D3 * t43 * t4
     #4 * (-t528 * t488 + t536 * t488)) * t115 * t54 / 0.1440D4 - (-t62 
     #* t44 * (-t506 + t557 * t488) - 0.90D2 * t4 * t7 * (t557 * t501 - 
     #t563 * t506 / 0.2D1 + t563 * t557 * t488 / 0.6D1) + t88 * t520 + 0
     #.180D3 * t43 * t44 * (-t501 + t557 * t506 - t563 * t488 / 0.2D1)) 
     #* t115 / 0.1440D4
      t585 = FJET(XB1, XB2, s, t480, -t482, 0.0D0, 0.0D0, 0.0D0, t584)
      t587 = KAPPA2(t285, x2, x3, 0.10D1, z)
      t588 = s * t587
      t589 = t289 * x3
      t590 = t588 * t589
      t591 = t289 * t481
      t592 = t588 * t591
      t593 = t588 * t291
      t594 = t587 ** 2
      t598 = s * t594 * t16 * t422 * t481
      t600 = 0.1D1 / (-0.2D1 + t587)
      t601 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t590, t593, t5
     #92, 0.0D0, t598)
      t602 = t600 * t601
      t603 = t594 ** 2
      t608 = log(-0.4D1 * t368 * t303 * t483 * t603)
      t610 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t590, t593, t5
     #92, 0.0D0, t598)
      t617 = t7 * t600 * t610
      t623 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, -t590, t593, t5
     #92, 0.0D0, t598)
      t629 = log(-0.4D1 * t368 * t303 * t481 * t603)
      t630 = t629 * t600
      t632 = t629 ** 2
      t650 = -(0.90D2 * t4 * t7 * (t602 - t608 * t600 * t610) - 0.180D3 
     #* t378 * t617) * t115 * t117 / 0.720D3 + (0.90D2 * t4 * t7 * (-t60
     #0 * t623 + t630 * t601 - t632 * t600 * t610 / 0.2D1) - 0.180D3 * t
     #43 * t44 * (-t602 + t630 * t610) - t328 * t617) * t115 * t52 / 0.7
     #20D3
      t651 = FJET(XB1, XB2, s, -t590, t592, t593, 0.0D0, t598, t650)
      t653 = KAPPA2(t285, x2, x3, t411, z)
      t654 = s * t653
      t655 = t654 * t589
      t656 = t654 * t591
      t657 = t654 * t415
      t658 = t654 * t417
      t659 = t653 ** 2
      t664 = cos(t9)
      t667 = Sqrt(x3 * t481 * t108)
      t672 = s * t659 * t16 * t422 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t664 * t667)
      t674 = 0.1D1 / (-0.2D1 + t653)
      t675 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t655, -t657, t
     #656, t658, t672)
      t678 = t659 ** 2
      t683 = log(0.4D1 * t123 * t302 * t481 * t108 * t678)
      t685 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t655, -t657, t
     #656, t658, t672)
      t695 = -0.90D2 * t4 * t7 * (t674 * t675 - t683 * t674 * t685) + 0.
     #180D3 * t378 * t7 * t674 * t685
      t699 = FJET(XB1, XB2, s, -t655, t656, -t657, t658, t672, -t695 * t
     #115 * t117 / 0.720D3)
      rrqq2qqht3s3e1 = t283 * t282 + t409 * t408 + t478 * t477 + t585 * 
     #t584 + t651 * t650 - t699 * t695 * t115 * t52 * t54 / 0.720D3

      end function



      doubleprecision function rrqq2qqht3s3e0
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = x1 ** 2
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t18 * x4
      t20 = -0.1D1 + x4
      t21 = t19 * t20
      t24 = log(-0.4D1 * t16 * t21)
      t25 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t27 = t15 * t18
      t28 = t27 * x4
      t31 = log(0.4D1 * t13 * t28)
      t34 = 0.1D1 / x1
      t36 = 0.1D1 / x4
      t40 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t41 = x3 * t9
      t42 = t12 * t15
      t43 = t42 * t18
      t46 = log(0.4D1 * t41 * t43)
      t52 = pi * lh
      t53 = t3 * t7
      t54 = t53 * t25
      t56 = 0.180D3 * t52 * t54
      t58 = 0.1D1 / x3
      t62 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t63 = t13 * t27
      t65 = log(0.4D1 * t63)
      t67 = t65 ** 2
      t79 = pi ** 2
      t81 = lh ** 2
      t83 = -0.30D2 * t79 + 0.180D3 * t81
      t84 = pi * t83
      t85 = t84 * t54
      t89 = t7 * t25
      t92 = log(0.4D1 * t42 * t19)
      t93 = t92 ** 2
      t96 = log(-0.4D1 * t42 * t21)
      t97 = t96 ** 2
      t103 = t7 * t40
      t114 = log(0.4D1 * t43)
      t115 = t114 * pi
      t124 = t114 ** 2
      t125 = t124 * pi
      t148 = x3 * t12
      t149 = t148 * t15
      t152 = log(-0.4D1 * t149 * t21)
      t156 = log(0.4D1 * t148 * t28)
      t165 = log(0.4D1 * t148 * t27)
      t167 = t165 ** 2
      t182 = t8 * (t24 * t25 - t31 * t25) * t34 * t36 / 0.8D1 + (0.90D2 
     #* t4 * t7 * (t40 - t46 * t25) - t56) * t58 * t34 / 0.720D3 + (0.90
     #D2 * t4 * t7 * (t62 - t65 * t40 + t67 * t25 / 0.2D1) - 0.180D3 * t
     #52 * t53 * (t40 - t65 * t25) + t85) * t34 / 0.720D3 - (-0.90D2 * t
     #4 * t89 * (t93 / 0.2D1 - t97 / 0.2D1) + (-0.90D2 * t4 * t103 + t56
     #) * (-t92 + t96)) * t36 / 0.1440D4 + (-0.180D3 * t52 - 0.90D2 * t1
     #15) * t3 * t7 * t62 / 0.1440D4 + (t84 + 0.180D3 * t115 * lh + 0.45
     #D2 * t125) * t3 * t103 / 0.1440D4 + (pi * (-0.240D3 * zeta3 - 0.12
     #0D3 * t81 * lh + 0.60D2 * lh * t79) - t115 * t83 - 0.90D2 * t125 *
     # lh - 0.15D2 * t124 * t114 * pi) * t3 * t89 / 0.1440D4 + t8 * (t15
     #2 * t25 - t156 * t25) * t58 * t36 / 0.16D2 - (0.90D2 * t4 * t7 * (
     #-t62 + t165 * t40 - t167 * t25 / 0.2D1) - 0.180D3 * t52 * t53 * (-
     #t40 + t165 * t25) - t85) * t58 / 0.1440D4
      t183 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t182)
      t185 = 0.1D1 - x1
      t186 = KAPPA2(t185, x2, 0.0D0, 0.10D1, z)
      t187 = s * t186
      t188 = -t185
      t189 = t1 * t188
      t190 = t187 * t189
      t191 = t1 * x1
      t192 = t187 * t191
      t193 = t186 ** 2
      t197 = s * t193 * t17 * t188 * x1
      t199 = 0.1D1 / (-0.2D1 + t186)
      t200 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t192, -t
     #190, 0.0D0, -t197)
      t201 = t199 * t200
      t202 = t188 ** 2
      t203 = t18 * t202
      t204 = t193 ** 2
      t209 = log(0.4D1 * t16 * t203 * x4 * t204)
      t211 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t192, -t
     #190, 0.0D0, -t197)
      t217 = t52 * t3
      t218 = t7 * t199
      t219 = t218 * t211
      t221 = 0.180D3 * t217 * t219
      t228 = t34 * t36
      t232 = t41 * t12
      t237 = log(0.4D1 * t232 * t27 * t202 * t204)
      t248 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t192, -t
     #190, 0.0D0, -t197)
      t253 = log(0.4D1 * t16 * t203 * t204)
      t254 = t253 * t199
      t256 = t253 ** 2
      t274 = (0.90D2 * t4 * t7 * (t201 - t209 * t199 * t211) - t221) * t
     #34 * t36 / 0.720D3 + t4 * t218 * t211 * t58 * t228 / 0.8D1 + (0.90
     #D2 * t4 * t7 * (t201 - t237 * t199 * t211) - t221) * t58 * t34 / 0
     #.720D3 + (0.90D2 * t4 * t7 * (t199 * t248 - t254 * t200 + t256 * t
     #199 * t211 / 0.2D1) - 0.180D3 * t52 * t53 * (t201 - t254 * t211) +
     # t84 * t3 * t219) * t34 / 0.720D3
      t275 = FJET(XB1, XB2, s, 0.0D0, -t190, t192, 0.0D0, -t197, t274)
      t277 = -t20
      t278 = KAPPA2(t185, x2, 0.0D0, t277, z)
      t279 = s * t278
      t280 = t279 * t189
      t281 = t191 * t20
      t282 = t279 * t281
      t283 = t191 * x4
      t284 = t279 * t283
      t285 = t278 ** 2
      t288 = t188 * x1
      t290 = s * t285 * t17 * t288 * t20
      t292 = 0.1D1 / (-0.2D1 + t278)
      t293 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t282, -
     #t280, t284, t290)
      t296 = t285 ** 2
      t301 = log(-0.4D1 * t63 * t202 * x4 * t20 * t296)
      t303 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t282, -
     #t280, t284, t290)
      t309 = t7 * t292
      t322 = (-0.90D2 * t4 * t7 * (t292 * t293 - t301 * t292 * t303) + 0
     #.180D3 * t217 * t309 * t303) * t34 * t36 / 0.720D3 - t4 * t309 * t
     #303 * t58 * t228 / 0.8D1
      t323 = FJET(XB1, XB2, s, 0.0D0, -t280, -t282, t284, t290, t322)
      t325 = t2 * x3
      t326 = -0.1D1 + x3
      t327 = t2 * t326
      t328 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t325, 0.0D0, -t
     #327, 0.0D0, 0.0D0)
      t329 = t27 * t326
      t332 = log(-0.4D1 * t232 * t329)
      t333 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t325, 0.0D0, -t
     #327, 0.0D0, 0.0D0)
      t339 = t53 * t333
      t346 = t18 * t326
      t347 = x4 * t20
      t351 = log(0.4D1 * t149 * t346 * t347)
      t356 = log(-0.4D1 * t149 * t346 * x4)
      t363 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t325, 0.0D0, -t
     #327, 0.0D0, 0.0D0)
      t366 = log(-0.4D1 * t148 * t329)
      t368 = t366 ** 2
      t384 = (0.90D2 * t4 * t7 * (-t328 + t332 * t333) + 0.180D3 * t52 *
     # t339) * t58 * t34 / 0.720D3 + t8 * (-t351 * t333 + t356 * t333) *
     # t58 * t36 / 0.16D2 - (-0.90D2 * t4 * t7 * (-t363 + t366 * t328 - 
     #t368 * t333 / 0.2D1) + 0.180D3 * t52 * t53 * (-t328 + t366 * t333)
     # + t84 * t339) * t58 / 0.1440D4
      t385 = FJET(XB1, XB2, s, t325, -t327, 0.0D0, 0.0D0, 0.0D0, t384)
      t387 = KAPPA2(t185, x2, x3, 0.10D1, z)
      t388 = s * t387
      t389 = t189 * x3
      t390 = t388 * t389
      t391 = t189 * t326
      t392 = t388 * t391
      t393 = t388 * t191
      t394 = t387 ** 2
      t398 = s * t394 * t17 * t288 * t326
      t400 = 0.1D1 / (-0.2D1 + t387)
      t401 = t7 * t400
      t403 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t390, t393, t3
     #92, 0.0D0, t398)
      t408 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t390, t393, t3
     #92, 0.0D0, t398)
      t411 = t394 ** 2
      t416 = log(-0.4D1 * t41 * t42 * t203 * t326 * t411)
      t430 = -t4 * t401 * t403 * t58 * t228 / 0.8D1 + (0.90D2 * t4 * t7 
     #* (-t400 * t408 + t416 * t400 * t403) + 0.180D3 * t217 * t401 * t4
     #03) * t58 * t34 / 0.720D3
      t431 = FJET(XB1, XB2, s, -t390, t392, t393, 0.0D0, t398, t430)
      t433 = KAPPA2(t185, x2, x3, t277, z)
      t434 = s * t433
      t435 = t434 * t389
      t436 = t434 * t391
      t437 = t434 * t281
      t438 = t434 * t283
      t439 = t433 ** 2
      t444 = cos(t10)
      t447 = Sqrt(x3 * t326 * t347)
      t452 = s * t439 * t17 * t288 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t444 * t447)
      t454 = 0.1D1 / (-0.2D1 + t433)
      t457 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t435, -t437, t
     #436, t438, t452)
      t462 = FJET(XB1, XB2, s, -t435, t436, -t437, t438, t452, t4 * t7 *
     # t454 * t457 * t58 * t228 / 0.8D1)
      rrqq2qqht3s3e0 = t183 * t182 + t275 * t274 + t323 * t322 + t385 * 
     #t384 + t431 * t430 + t462 * pi * t53 * t454 * t457 * t58 * t34 * t
     #36 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s3em1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t10 = 0.1D1 / x3
      t12 = 0.1D1 / x1
      t16 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t17 = x1 ** 2
      t19 = sin(x2 * pi)
      t20 = t19 ** 2
      t21 = t17 * t20
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t24 = t1 ** 2
      t25 = t24 ** 2
      t26 = t23 * t25
      t29 = log(0.4D1 * t21 * t26)
      t35 = pi * lh
      t36 = t3 * t7
      t39 = 0.180D3 * t35 * t36 * t9
      t43 = t20 * t23
      t44 = t25 * x4
      t47 = log(0.4D1 * t43 * t44)
      t48 = -0.1D1 + x4
      t52 = log(-0.4D1 * t43 * t44 * t48)
      t55 = 0.1D1 / x4
      t59 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t66 = log(0.4D1 * t43 * t25)
      t67 = t66 * pi
      t74 = pi ** 2
      t76 = lh ** 2
      t82 = t66 ** 2
      t90 = x3 * t20
      t93 = log(0.4D1 * t90 * t26)
      t102 = t8 * t9 * t10 * t12 / 0.8D1 + (0.90D2 * t4 * t7 * (t16 - t2
     #9 * t9) - t39) * t12 / 0.720D3 + t8 * t9 * (-t47 + t52) * t55 / 0.
     #16D2 + t4 * t7 * t59 / 0.16D2 + (-0.180D3 * t35 - 0.90D2 * t67) * 
     #t3 * t7 * t16 / 0.1440D4 + (pi * (-0.30D2 * t74 + 0.180D3 * t76) +
     # 0.180D3 * t67 * lh + 0.45D2 * t82 * pi) * t3 * t7 * t9 / 0.1440D4
     # - (0.90D2 * t4 * t7 * (-t16 + t93 * t9) + t39) * t10 / 0.1440D4
      t103 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t102)
      t105 = 0.1D1 - x1
      t106 = KAPPA2(t105, x2, 0.0D0, 0.10D1, z)
      t107 = s * t106
      t108 = -t105
      t109 = t1 * t108
      t110 = t107 * t109
      t111 = t1 * x1
      t112 = t107 * t111
      t113 = t106 ** 2
      t117 = s * t113 * t24 * t108 * x1
      t119 = 0.1D1 / (-0.2D1 + t106)
      t120 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t112, -t
     #110, 0.0D0, -t117)
      t121 = t119 * t120
      t122 = t10 * t12
      t126 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t112, -t
     #110, 0.0D0, -t117)
      t129 = t108 ** 2
      t131 = t113 ** 2
      t135 = log(0.4D1 * t21 * t23 * t25 * t129 * t131)
      t150 = t12 * t55
      t154 = t8 * t121 * t122 / 0.8D1 + (0.90D2 * t4 * t7 * (t119 * t126
     # - t135 * t119 * t120) - 0.180D3 * t35 * t3 * t7 * t119 * t120) * 
     #t12 / 0.720D3 + t8 * t121 * t150 / 0.8D1
      t155 = FJET(XB1, XB2, s, 0.0D0, -t110, t112, 0.0D0, -t117, t154)
      t158 = KAPPA2(t105, x2, 0.0D0, -t48, z)
      t159 = s * t158
      t160 = t159 * t109
      t162 = t159 * t111 * t48
      t164 = t159 * t111 * x4
      t165 = t158 ** 2
      t168 = t108 * x1
      t170 = s * t165 * t24 * t168 * t48
      t173 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t162, -
     #t160, t164, t170)
      t175 = 0.1D1 / (-0.2D1 + t158) * t173 * t150
      t178 = FJET(XB1, XB2, s, 0.0D0, -t160, -t162, t164, t170, -t8 * t1
     #75 / 0.8D1)
      t183 = t2 * x3
      t184 = -0.1D1 + x3
      t185 = t2 * t184
      t186 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t183, 0.0D0, -t
     #185, 0.0D0, 0.0D0)
      t191 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t183, 0.0D0, -t
     #185, 0.0D0, 0.0D0)
      t195 = log(-0.4D1 * t90 * t26 * t184)
      t207 = -t8 * t186 * t10 * t12 / 0.8D1 - (-0.90D2 * t4 * t7 * (-t19
     #1 + t195 * t186) - 0.180D3 * t35 * t36 * t186) * t10 / 0.1440D4
      t208 = FJET(XB1, XB2, s, t183, -t185, 0.0D0, 0.0D0, 0.0D0, t207)
      t210 = KAPPA2(t105, x2, x3, 0.10D1, z)
      t211 = s * t210
      t213 = t211 * t109 * x3
      t215 = t211 * t109 * t184
      t216 = t211 * t111
      t217 = t210 ** 2
      t221 = s * t217 * t24 * t168 * t184
      t224 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t213, t216, t2
     #15, 0.0D0, t221)
      t226 = 0.1D1 / (-0.2D1 + t210) * t224 * t122
      t229 = FJET(XB1, XB2, s, -t213, t215, t216, 0.0D0, t221, -t8 * t22
     #6 / 0.8D1)
      rrqq2qqht3s3em1 = t103 * t102 + t155 * t154 - t178 * pi * t36 * t1
     #75 / 0.8D1 + t208 * t207 - t229 * pi * t36 * t226 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s3em2
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t21 = sin(x2 * pi)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t30 = log(0.4D1 * t22 / t23 * t27)
      t37 = 0.1D1 / x3
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t7 * t14 / 0.16D2 + (-0.180D3 *
     # pi * lh - 0.90D2 * t30 * pi) * t3 * t9 / 0.1440D4 + t4 * t9 * t37
     # / 0.16D2
      t42 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t41)
      t44 = 0.1D1 - x1
      t45 = KAPPA2(t44, x2, 0.0D0, 0.10D1, z)
      t46 = s * t45
      t47 = -t44
      t49 = t46 * t1 * t47
      t51 = t46 * t1 * x1
      t52 = t45 ** 2
      t56 = s * t52 * t26 * t47 * x1
      t59 = 0.1D1 / (-0.2D1 + t45)
      t60 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t51, -t49
     #, 0.0D0, -t56)
      t65 = FJET(XB1, XB2, s, 0.0D0, -t49, t51, 0.0D0, -t56, t4 * t7 * t
     #59 * t60 * t10 / 0.8D1)
      t73 = t2 * x3
      t75 = t2 * (-0.1D1 + x3)
      t76 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t73, 0.0D0, -t75
     #, 0.0D0, 0.0D0)
      t78 = t7 * t76 * t37
      t81 = FJET(XB1, XB2, s, t73, -t75, 0.0D0, 0.0D0, 0.0D0, -t4 * t78 
     #/ 0.16D2)
      rrqq2qqht3s3em2 = t42 * t41 + t65 * pi * t3 * t7 * t59 * t60 * t10
     # / 0.8D1 - t81 * pi * t3 * t78 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s3em3
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrqq2qqht3s3em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s3em4
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqq2qqht3s3em4 = 0.0D0

      end function


      doubleprecision function rrqq2qqht3s4e1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = x1 ** 2
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = t8 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t17 * x4
      t19 = -0.1D1 + x4
      t20 = t18 * t19
      t23 = log(-0.4D1 * t15 * t20)
      t24 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t26 = t23 ** 2
      t27 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t30 = t14 * t17
      t31 = t30 * x4
      t34 = log(0.4D1 * t12 * t31)
      t36 = t34 ** 2
      t43 = pi * lh
      t44 = t3 * t7
      t52 = 0.1D1 / x1
      t54 = 0.1D1 / x4
      t57 = pi ** 2
      t59 = lh ** 2
      t61 = -0.30D2 * t57 + 0.180D3 * t59
      t62 = pi * t61
      t63 = t12 * t30
      t65 = log(0.4D1 * t63)
      t70 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t72 = t65 ** 2
      t87 = -0.240D3 * zeta3 - 0.120D3 * t59 * lh + 0.60D2 * lh * t57
      t88 = pi * t87
      t89 = t44 * t27
      t90 = t88 * t89
      t101 = t4 * t7
      t102 = x3 * t8
      t103 = t102 * t11
      t106 = log(0.4D1 * t103 * t31)
      t108 = x4 * t19
      t112 = log(-0.4D1 * t103 * t30 * t108)
      t115 = 0.1D1 / x3
      t117 = t52 * t54
      t121 = t11 * t14
      t122 = t121 * t17
      t123 = t102 * t122
      t125 = log(0.4D1 * t123)
      t127 = t125 ** 2
      t146 = log(-0.4D1 * t121 * t20)
      t148 = t146 ** 2
      t153 = log(0.4D1 * t121 * t18)
      t155 = t153 ** 2
      t187 = log(0.4D1 * t122)
      t188 = t187 * pi
      t191 = t187 ** 2
      t192 = t191 * pi
      t203 = t191 * t187 * pi
      t210 = t57 ** 2
      t211 = t59 ** 2
      t224 = t191 ** 2
      t232 = x3 * t11
      t233 = t232 * t14
      t236 = log(-0.4D1 * t233 * t20)
      t238 = t236 ** 2
      t243 = log(0.4D1 * t232 * t31)
      t245 = t243 ** 2
      t264 = log(0.4D1 * t232 * t30)
      t270 = t264 ** 2
      t290 = (0.90D2 * t4 * t7 * (t23 * t24 - t26 * t27 / 0.2D1 - t34 * 
     #t24 + t36 * t27 / 0.2D1) - 0.180D3 * t43 * t44 * (t23 * t27 - t34 
     #* t27)) * t52 * t54 / 0.720D3 - (t62 * t44 * (-t24 + t65 * t27) + 
     #0.90D2 * t4 * t7 * (t65 * t70 - t72 * t24 / 0.2D1 + t72 * t65 * t2
     #7 / 0.6D1) - t90 - 0.180D3 * t43 * t44 * (-t70 + t65 * t24 - t72 *
     # t27 / 0.2D1)) * t52 / 0.720D3 - t101 * (t106 * t27 - t112 * t27) 
     #* t115 * t117 / 0.8D1 + (0.90D2 * t4 * t7 * (t70 - t125 * t24 + t1
     #27 * t27 / 0.2D1) - 0.180D3 * t43 * t44 * (t24 - t125 * t27) + t62
     # * t89) * t115 * t52 / 0.720D3 + (-0.180D3 * t43 * t44 * (t146 * t
     #24 - t148 * t27 / 0.2D1 - t153 * t24 + t155 * t27 / 0.2D1) + 0.90D
     #2 * t4 * t7 * (-t153 * t70 + t155 * t24 / 0.2D1 - t155 * t153 * t2
     #7 / 0.6D1 + t146 * t70 - t148 * t24 / 0.2D1 + t148 * t146 * t27 / 
     #0.6D1) + t62 * t44 * (t146 * t27 - t153 * t27)) * t54 / 0.1440D4 +
     # (t62 + 0.180D3 * t188 * lh + 0.45D2 * t192) * t3 * t7 * t70 / 0.1
     #440D4 + (t88 - t188 * t61 - 0.90D2 * t192 * lh - 0.15D2 * t203) * 
     #t3 * t7 * t24 / 0.1440D4 + (pi * (t210 + 0.60D2 * t211 + 0.480D3 *
     # lh * zeta3 - 0.60D2 * t59 * t57) - t188 * t87 + t192 * t61 / 0.2D
     #1 + 0.30D2 * t203 * lh + 0.15D2 / 0.4D1 * t224 * pi) * t3 * t7 * t
     #27 / 0.1440D4 - (0.90D2 * t4 * t7 * (-t236 * t24 + t238 * t27 / 0.
     #2D1 + t243 * t24 - t245 * t27 / 0.2D1) - 0.180D3 * t43 * t44 * (-t
     #236 * t27 + t243 * t27)) * t115 * t54 / 0.1440D4 + (t62 * t44 * (t
     #24 - t264 * t27) + 0.90D2 * t4 * t7 * (-t264 * t70 + t270 * t24 / 
     #0.2D1 - t270 * t264 * t27 / 0.6D1) + t90 - 0.180D3 * t43 * t44 * (
     #t70 - t264 * t24 + t270 * t27 / 0.2D1)) * t115 / 0.1440D4
      t291 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t290)
      t293 = -0.1D1 + x1
      t294 = t2 * t293
      t295 = t2 * x1
      t296 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t294, t295, 0.0D0)
      t297 = t293 ** 2
      t298 = t17 * t297
      t302 = log(0.4D1 * t15 * t298 * x4)
      t303 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t294, t295, 0.0D0)
      t305 = t302 ** 2
      t306 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t294, t295, 0.0D0)
      t318 = t44 * t306
      t319 = t62 * t318
      t323 = t30 * t297
      t326 = log(0.4D1 * t12 * t323)
      t332 = t326 ** 2
      t352 = t297 * x4
      t356 = log(0.4D1 * t103 * t30 * t352)
      t369 = log(0.4D1 * t103 * t323)
      t371 = t369 ** 2
      t387 = (0.90D2 * t4 * t7 * (-t296 + t302 * t303 - t305 * t306 / 0.
     #2D1) - 0.180D3 * t43 * t44 * (-t303 + t302 * t306) - t319) * t52 *
     # t54 / 0.720D3 - (t62 * t44 * (t303 - t326 * t306) + 0.90D2 * t4 *
     # t7 * (-t326 * t296 + t332 * t303 / 0.2D1 - t332 * t326 * t306 / 0
     #.6D1) + t88 * t318 - 0.180D3 * t43 * t44 * (t296 - t326 * t303 + t
     #332 * t306 / 0.2D1)) * t52 / 0.720D3 - (0.90D2 * t4 * t7 * (t303 -
     # t356 * t306) - 0.180D3 * t43 * t318) * t115 * t117 / 0.720D3 + (0
     #.90D2 * t4 * t7 * (-t296 + t369 * t303 - t371 * t306 / 0.2D1) - 0.
     #180D3 * t43 * t44 * (-t303 + t369 * t306) - t319) * t115 * t52 / 0
     #.720D3
      t388 = FJET(XB1, XB2, s, 0.0D0, -t294, 0.0D0, t295, 0.0D0, t387)
      t390 = -t293
      t391 = KAPPA2(t390, x2, 0.0D0, x4, z)
      t392 = s * t391
      t393 = t1 * t293
      t394 = t392 * t393
      t395 = t1 * x1
      t396 = t395 * x4
      t397 = t392 * t396
      t398 = t395 * t19
      t399 = t392 * t398
      t400 = t391 ** 2
      t403 = t293 * x1
      t405 = s * t400 * t16 * t403 * x4
      t407 = 0.1D1 / (-0.2D1 + t391)
      t408 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t397, -t
     #394, -t399, -t405)
      t410 = t400 ** 2
      t415 = log(-0.4D1 * t63 * t352 * t19 * t410)
      t416 = t415 * t407
      t417 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t397, -t
     #394, -t399, -t405)
      t419 = t415 ** 2
      t421 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t397, -t
     #394, -t399, -t405)
      t428 = t407 * t417
      t434 = t62 * t3
      t436 = t7 * t407 * t421
      t441 = t102 * t121
      t446 = log(-0.4D1 * t441 * t298 * t108 * t410)
      t453 = t43 * t3
      t460 = (-0.90D2 * t4 * t7 * (t407 * t408 - t416 * t417 + t419 * t4
     #07 * t421 / 0.2D1) + 0.180D3 * t43 * t44 * (t428 - t416 * t421) - 
     #t434 * t436) * t52 * t54 / 0.720D3 - (0.90D2 * t4 * t7 * (t428 - t
     #446 * t407 * t421) - 0.180D3 * t453 * t436) * t115 * t117 / 0.720D
     #3
      t461 = FJET(XB1, XB2, s, 0.0D0, -t394, t397, -t399, -t405, t460)
      t463 = t2 * x3
      t464 = -0.1D1 + x3
      t465 = t2 * t464
      t466 = t464 * x4
      t470 = log(-0.4D1 * t103 * t30 * t466)
      t471 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t463, 0.0D0, -t
     #465, 0.0D0, 0.0D0)
      t473 = t17 * t464
      t474 = t473 * t108
      t477 = log(0.4D1 * t441 * t474)
      t484 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t463, 0.0D0, -t
     #465, 0.0D0, 0.0D0)
      t485 = t30 * t464
      t488 = log(-0.4D1 * t103 * t485)
      t489 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t463, 0.0D0, -t
     #465, 0.0D0, 0.0D0)
      t491 = t488 ** 2
      t503 = t44 * t471
      t512 = log(-0.4D1 * t233 * t473 * x4)
      t514 = t512 ** 2
      t519 = log(0.4D1 * t233 * t474)
      t521 = t519 ** 2
      t540 = log(-0.4D1 * t232 * t485)
      t546 = t540 ** 2
      t567 = -t101 * (-t470 * t471 + t477 * t471) * t115 * t117 / 0.8D1 
     #+ (0.90D2 * t4 * t7 * (-t484 + t488 * t489 - t491 * t471 / 0.2D1) 
     #- 0.180D3 * t43 * t44 * (-t489 + t488 * t471) - t62 * t503) * t115
     # * t52 / 0.720D3 - (0.90D2 * t4 * t7 * (-t512 * t489 + t514 * t471
     # / 0.2D1 + t519 * t489 - t521 * t471 / 0.2D1) - 0.180D3 * t43 * t4
     #4 * (-t512 * t471 + t519 * t471)) * t115 * t54 / 0.1440D4 + (-t62 
     #* t44 * (t489 - t540 * t471) - 0.90D2 * t4 * t7 * (-t540 * t484 + 
     #t546 * t489 / 0.2D1 - t546 * t540 * t471 / 0.6D1) - t88 * t503 + 0
     #.180D3 * t43 * t44 * (t484 - t540 * t489 + t546 * t471 / 0.2D1)) *
     # t115 / 0.1440D4
      t568 = FJET(XB1, XB2, s, t463, -t465, 0.0D0, 0.0D0, 0.0D0, t567)
      t570 = KAPPA2(t390, x2, x3, 0.0D0, z)
      t571 = s * t570
      t572 = t393 * x3
      t573 = t571 * t572
      t574 = t393 * t464
      t575 = t571 * t574
      t576 = t571 * t395
      t577 = t570 ** 2
      t581 = s * t577 * t16 * t403 * x3
      t583 = 0.1D1 / (-0.2D1 + t570)
      t584 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t573, 0.0D0, t
     #575, t576, -t581)
      t585 = t583 * t584
      t586 = t577 ** 2
      t591 = log(-0.4D1 * t441 * t298 * t466 * t586)
      t593 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t573, 0.0D0, t
     #575, t576, -t581)
      t600 = t7 * t583 * t593
      t606 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, -t573, 0.0D0, t
     #575, t576, -t581)
      t612 = log(-0.4D1 * t441 * t298 * t464 * t586)
      t613 = t612 * t583
      t615 = t612 ** 2
      t633 = -(0.90D2 * t4 * t7 * (t585 - t591 * t583 * t593) - 0.180D3 
     #* t453 * t600) * t115 * t117 / 0.720D3 + (-0.90D2 * t4 * t7 * (t58
     #3 * t606 - t613 * t584 + t615 * t583 * t593 / 0.2D1) + 0.180D3 * t
     #43 * t44 * (t585 - t613 * t593) - t434 * t600) * t115 * t52 / 0.72
     #0D3
      t634 = FJET(XB1, XB2, s, -t573, t575, 0.0D0, t576, -t581, t633)
      t636 = KAPPA2(t390, x2, x3, x4, z)
      t637 = s * t636
      t638 = t637 * t572
      t639 = t637 * t574
      t640 = t637 * t396
      t641 = t637 * t398
      t642 = t636 ** 2
      t647 = cos(t9)
      t650 = Sqrt(x3 * t464 * t108)
      t655 = s * t642 * t16 * t403 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t647 * t650)
      t657 = 0.1D1 / (-0.2D1 + t636)
      t658 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t638, t640, t6
     #39, -t641, t655)
      t661 = t642 ** 2
      t666 = log(0.4D1 * t123 * t297 * t464 * t108 * t661)
      t668 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t638, t640, t6
     #39, -t641, t655)
      t678 = -0.90D2 * t4 * t7 * (t657 * t658 - t666 * t657 * t668) + 0.
     #180D3 * t453 * t7 * t657 * t668
      t682 = FJET(XB1, XB2, s, -t638, t639, t640, -t641, t655, -t678 * t
     #115 * t117 / 0.720D3)
      rrqq2qqht3s4e1 = t291 * t290 + t388 * t387 + t461 * t460 + t568 * 
     #t567 + t634 * t633 - t682 * t678 * t115 * t52 * t54 / 0.720D3

      end function



      doubleprecision function rrqq2qqht3s4e0
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = x1 ** 2
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t13 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t18 * x4
      t20 = -0.1D1 + x4
      t21 = t19 * t20
      t24 = log(-0.4D1 * t16 * t21)
      t25 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t27 = t15 * t18
      t28 = t27 * x4
      t31 = log(0.4D1 * t13 * t28)
      t34 = 0.1D1 / x1
      t36 = 0.1D1 / x4
      t40 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t41 = x3 * t9
      t42 = t12 * t15
      t43 = t42 * t18
      t46 = log(0.4D1 * t41 * t43)
      t52 = pi * lh
      t53 = t3 * t7
      t54 = t53 * t25
      t58 = 0.1D1 / x3
      t62 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t63 = t13 * t27
      t65 = log(0.4D1 * t63)
      t67 = t65 ** 2
      t79 = pi ** 2
      t81 = lh ** 2
      t83 = -0.30D2 * t79 + 0.180D3 * t81
      t84 = pi * t83
      t85 = t84 * t54
      t91 = log(-0.4D1 * t42 * t21)
      t93 = t91 ** 2
      t98 = log(0.4D1 * t42 * t19)
      t100 = t98 ** 2
      t118 = log(0.4D1 * t43)
      t119 = t118 * pi
      t128 = t118 ** 2
      t129 = t128 * pi
      t154 = x3 * t12
      t155 = t154 * t15
      t158 = log(-0.4D1 * t155 * t21)
      t162 = log(0.4D1 * t154 * t28)
      t171 = log(0.4D1 * t154 * t27)
      t173 = t171 ** 2
      t188 = t8 * (t24 * t25 - t31 * t25) * t34 * t36 / 0.8D1 + (0.90D2 
     #* t4 * t7 * (t40 - t46 * t25) - 0.180D3 * t52 * t54) * t58 * t34 /
     # 0.720D3 - (0.90D2 * t4 * t7 * (-t62 + t65 * t40 - t67 * t25 / 0.2
     #D1) - 0.180D3 * t52 * t53 * (-t40 + t65 * t25) - t85) * t34 / 0.72
     #0D3 + (0.90D2 * t4 * t7 * (t91 * t40 - t93 * t25 / 0.2D1 - t98 * t
     #40 + t100 * t25 / 0.2D1) - 0.180D3 * t52 * t53 * (t91 * t25 - t98 
     #* t25)) * t36 / 0.1440D4 + (-0.180D3 * t52 - 0.90D2 * t119) * t3 *
     # t7 * t62 / 0.1440D4 + (t84 + 0.180D3 * t119 * lh + 0.45D2 * t129)
     # * t3 * t7 * t40 / 0.1440D4 + (pi * (-0.240D3 * zeta3 - 0.120D3 * 
     #t81 * lh + 0.60D2 * lh * t79) - t119 * t83 - 0.90D2 * t129 * lh - 
     #0.15D2 * t128 * t118 * pi) * t3 * t7 * t25 / 0.1440D4 - t8 * (-t15
     #8 * t25 + t162 * t25) * t58 * t36 / 0.16D2 + (0.90D2 * t4 * t7 * (
     #t62 - t171 * t40 + t173 * t25 / 0.2D1) - 0.180D3 * t52 * t53 * (t4
     #0 - t171 * t25) + t85) * t58 / 0.1440D4
      t189 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t188)
      t191 = -0.1D1 + x1
      t192 = t2 * t191
      t193 = t2 * x1
      t194 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t192, t193, 0.0D0)
      t195 = t191 ** 2
      t196 = t18 * t195
      t200 = log(0.4D1 * t16 * t196 * x4)
      t201 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t192, t193, 0.0D0)
      t207 = t53 * t201
      t209 = 0.180D3 * t52 * t207
      t215 = t34 * t36
      t219 = t41 * t12
      t220 = t27 * t195
      t223 = log(0.4D1 * t219 * t220)
      t233 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t192, t193, 0.0D0)
      t236 = log(0.4D1 * t13 * t220)
      t238 = t236 ** 2
      t254 = (0.90D2 * t4 * t7 * (-t194 + t200 * t201) + t209) * t34 * t
     #36 / 0.720D3 - t8 * t201 * t58 * t215 / 0.8D1 + (0.90D2 * t4 * t7 
     #* (-t194 + t223 * t201) + t209) * t58 * t34 / 0.720D3 - (0.90D2 * 
     #t4 * t7 * (t233 - t236 * t194 + t238 * t201 / 0.2D1) - 0.180D3 * t
     #52 * t53 * (t194 - t236 * t201) + t84 * t207) * t34 / 0.720D3
      t255 = FJET(XB1, XB2, s, 0.0D0, -t192, 0.0D0, t193, 0.0D0, t254)
      t257 = -t191
      t258 = KAPPA2(t257, x2, 0.0D0, x4, z)
      t259 = s * t258
      t260 = t1 * t191
      t261 = t259 * t260
      t262 = t1 * x1
      t263 = t262 * x4
      t264 = t259 * t263
      t265 = t262 * t20
      t266 = t259 * t265
      t267 = t258 ** 2
      t270 = t191 * x1
      t272 = s * t267 * t17 * t270 * x4
      t274 = 0.1D1 / (-0.2D1 + t258)
      t275 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t264, -t
     #261, -t266, -t272)
      t278 = t267 ** 2
      t283 = log(-0.4D1 * t63 * t195 * x4 * t20 * t278)
      t285 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t264, -t
     #261, -t266, -t272)
      t291 = t52 * t3
      t292 = t7 * t274
      t305 = (-0.90D2 * t4 * t7 * (t274 * t275 - t283 * t274 * t285) + 0
     #.180D3 * t291 * t292 * t285) * t34 * t36 / 0.720D3 - t4 * t292 * t
     #285 * t58 * t215 / 0.8D1
      t306 = FJET(XB1, XB2, s, 0.0D0, -t261, t264, -t266, -t272, t305)
      t308 = t2 * x3
      t309 = -0.1D1 + x3
      t310 = t2 * t309
      t311 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t308, 0.0D0, -t
     #310, 0.0D0, 0.0D0)
      t312 = t27 * t309
      t315 = log(-0.4D1 * t219 * t312)
      t316 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t308, 0.0D0, -t
     #310, 0.0D0, 0.0D0)
      t322 = t53 * t316
      t329 = t18 * t309
      t333 = log(-0.4D1 * t155 * t329 * x4)
      t335 = x4 * t20
      t339 = log(0.4D1 * t155 * t329 * t335)
      t346 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t308, 0.0D0, -t
     #310, 0.0D0, 0.0D0)
      t349 = log(-0.4D1 * t154 * t312)
      t351 = t349 ** 2
      t367 = (0.90D2 * t4 * t7 * (-t311 + t315 * t316) + 0.180D3 * t52 *
     # t322) * t58 * t34 / 0.720D3 - t8 * (-t333 * t316 + t339 * t316) *
     # t58 * t36 / 0.16D2 + (-0.90D2 * t4 * t7 * (t346 - t349 * t311 + t
     #351 * t316 / 0.2D1) + 0.180D3 * t52 * t53 * (t311 - t349 * t316) -
     # t84 * t322) * t58 / 0.1440D4
      t368 = FJET(XB1, XB2, s, t308, -t310, 0.0D0, 0.0D0, 0.0D0, t367)
      t370 = KAPPA2(t257, x2, x3, 0.0D0, z)
      t371 = s * t370
      t372 = t260 * x3
      t373 = t371 * t372
      t374 = t260 * t309
      t375 = t371 * t374
      t376 = t371 * t262
      t377 = t370 ** 2
      t381 = s * t377 * t17 * t270 * x3
      t383 = 0.1D1 / (-0.2D1 + t370)
      t384 = t7 * t383
      t386 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t373, 0.0D0, t
     #375, t376, -t381)
      t391 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t373, 0.0D0, t
     #375, t376, -t381)
      t394 = t377 ** 2
      t399 = log(-0.4D1 * t41 * t42 * t196 * t309 * t394)
      t413 = -t4 * t384 * t386 * t58 * t215 / 0.8D1 + (-0.90D2 * t4 * t7
     # * (t383 * t391 - t399 * t383 * t386) + 0.180D3 * t291 * t384 * t3
     #86) * t58 * t34 / 0.720D3
      t414 = FJET(XB1, XB2, s, -t373, t375, 0.0D0, t376, -t381, t413)
      t416 = KAPPA2(t257, x2, x3, x4, z)
      t417 = s * t416
      t418 = t417 * t372
      t419 = t417 * t374
      t420 = t417 * t263
      t421 = t417 * t265
      t422 = t416 ** 2
      t427 = cos(t10)
      t430 = Sqrt(x3 * t309 * t335)
      t435 = s * t422 * t17 * t270 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t427 * t430)
      t437 = 0.1D1 / (-0.2D1 + t416)
      t440 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t418, t420, t4
     #19, -t421, t435)
      t445 = FJET(XB1, XB2, s, -t418, t419, t420, -t421, t435, t4 * t7 *
     # t437 * t440 * t58 * t215 / 0.8D1)
      rrqq2qqht3s4e0 = t189 * t188 + t255 * t254 + t305 * t306 + t368 * 
     #t367 + t414 * t413 + t445 * pi * t53 * t437 * t440 * t58 * t34 * t
     #36 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s4em1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = t4 * t7
      t9 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t10 = 0.1D1 / x3
      t12 = 0.1D1 / x1
      t16 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t17 = x1 ** 2
      t19 = sin(x2 * pi)
      t20 = t19 ** 2
      t21 = t17 * t20
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t24 = t1 ** 2
      t25 = t24 ** 2
      t26 = t23 * t25
      t29 = log(0.4D1 * t21 * t26)
      t35 = pi * lh
      t36 = t3 * t7
      t39 = 0.180D3 * t35 * t36 * t9
      t43 = t20 * t23
      t44 = t25 * x4
      t45 = -0.1D1 + x4
      t49 = log(-0.4D1 * t43 * t44 * t45)
      t53 = log(0.4D1 * t43 * t44)
      t57 = 0.1D1 / x4
      t61 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t68 = log(0.4D1 * t43 * t25)
      t69 = t68 * pi
      t76 = pi ** 2
      t78 = lh ** 2
      t84 = t68 ** 2
      t92 = x3 * t20
      t95 = log(0.4D1 * t92 * t26)
      t104 = t8 * t9 * t10 * t12 / 0.8D1 - (0.90D2 * t4 * t7 * (-t16 + t
     #29 * t9) + t39) * t12 / 0.720D3 + t4 * t7 * (t49 * t9 - t53 * t9) 
     #* t57 / 0.16D2 + t4 * t7 * t61 / 0.16D2 + (-0.180D3 * t35 - 0.90D2
     # * t69) * t3 * t7 * t16 / 0.1440D4 + (pi * (-0.30D2 * t76 + 0.180D
     #3 * t78) + 0.180D3 * t69 * lh + 0.45D2 * t84 * pi) * t3 * t7 * t9 
     #/ 0.1440D4 + (0.90D2 * t4 * t7 * (t16 - t95 * t9) - t39) * t10 / 0
     #.1440D4
      t105 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t104)
      t107 = -0.1D1 + x1
      t108 = t2 * t107
      t109 = t2 * x1
      t110 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t108, t109, 0.0D0)
      t115 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t108, t109, 0.0D0)
      t116 = t107 ** 2
      t120 = log(0.4D1 * t21 * t26 * t116)
      t136 = -t8 * t110 * t10 * t12 / 0.8D1 - (0.90D2 * t4 * t7 * (t115 
     #- t120 * t110) - 0.180D3 * t35 * t36 * t110) * t12 / 0.720D3 - t8 
     #* t110 * t12 * t57 / 0.8D1
      t137 = FJET(XB1, XB2, s, 0.0D0, -t108, 0.0D0, t109, 0.0D0, t136)
      t139 = -t107
      t140 = KAPPA2(t139, x2, 0.0D0, x4, z)
      t141 = s * t140
      t142 = t1 * t107
      t143 = t141 * t142
      t144 = t1 * x1
      t146 = t141 * t144 * x4
      t148 = t141 * t144 * t45
      t149 = t140 ** 2
      t152 = t107 * x1
      t154 = s * t149 * t24 * t152 * x4
      t157 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t146, -t
     #143, -t148, -t154)
      t160 = 0.1D1 / (-0.2D1 + t140) * t157 * t12 * t57
      t163 = FJET(XB1, XB2, s, 0.0D0, -t143, t146, -t148, -t154, -t8 * t
     #160 / 0.8D1)
      t168 = t2 * x3
      t169 = -0.1D1 + x3
      t170 = t2 * t169
      t171 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t168, 0.0D0, -t
     #170, 0.0D0, 0.0D0)
      t176 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t168, 0.0D0, -t
     #170, 0.0D0, 0.0D0)
      t180 = log(-0.4D1 * t92 * t26 * t169)
      t192 = -t8 * t171 * t10 * t12 / 0.8D1 + (-0.90D2 * t4 * t7 * (t176
     # - t180 * t171) + 0.180D3 * t35 * t36 * t171) * t10 / 0.1440D4
      t193 = FJET(XB1, XB2, s, t168, -t170, 0.0D0, 0.0D0, 0.0D0, t192)
      t195 = KAPPA2(t139, x2, x3, 0.0D0, z)
      t196 = s * t195
      t198 = t196 * t142 * x3
      t200 = t196 * t142 * t169
      t201 = t196 * t144
      t202 = t195 ** 2
      t206 = s * t202 * t24 * t152 * x3
      t209 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t198, 0.0D0, t
     #200, t201, -t206)
      t212 = 0.1D1 / (-0.2D1 + t195) * t209 * t10 * t12
      t215 = FJET(XB1, XB2, s, -t198, t200, 0.0D0, t201, -t206, -t8 * t2
     #12 / 0.8D1)
      rrqq2qqht3s4em1 = t105 * t104 + t137 * t136 - t163 * pi * t36 * t1
     #60 / 0.8D1 + t193 * t192 - t215 * pi * t36 * t212 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s4em2
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t21 = sin(x2 * pi)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t30 = log(0.4D1 * t22 / t23 * t27)
      t37 = 0.1D1 / x3
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t7 * t14 / 0.16D2 + (-0.180D3 *
     # pi * lh - 0.90D2 * t30 * pi) * t3 * t9 / 0.1440D4 + t4 * t9 * t37
     # / 0.16D2
      t42 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t41)
      t45 = t2 * (-0.1D1 + x1)
      t46 = t2 * x1
      t47 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -t
     #45, t46, 0.0D0)
      t49 = t7 * t47 * t10
      t52 = FJET(XB1, XB2, s, 0.0D0, -t45, 0.0D0, t46, 0.0D0, -t4 * t49 
     #/ 0.8D1)
      t57 = t2 * x3
      t59 = t2 * (-0.1D1 + x3)
      t60 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t57, 0.0D0, -t59
     #, 0.0D0, 0.0D0)
      t62 = t7 * t60 * t37
      t65 = FJET(XB1, XB2, s, t57, -t59, 0.0D0, 0.0D0, 0.0D0, -t4 * t62 
     #/ 0.16D2)
      rrqq2qqht3s4em2 = t42 * t41 - t52 * pi * t3 * t49 / 0.8D1 - t65 * 
     #pi * t3 * t62 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s4em3
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrqq2qqht3s4em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s4em4
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqq2qqht3s4em4 = 0.0D0

      end function


      doubleprecision function rrqq2qqht3s5e1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = x1 ** 2
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = t18 * x4
      t22 = log(0.4D1 * t13 * t19)
      t23 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t25 = t22 ** 2
      t26 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t33 = pi * lh
      t34 = t3 * t7
      t40 = pi ** 2
      t42 = lh ** 2
      t44 = -0.30D2 * t40 + 0.180D3 * t42
      t45 = pi * t44
      t46 = t34 * t26
      t47 = t45 * t46
      t49 = 0.1D1 / x1
      t51 = 0.1D1 / x4
      t54 = t13 * t18
      t56 = log(0.4D1 * t54)
      t62 = t56 ** 2
      t77 = -0.240D3 * zeta3 - 0.120D3 * t42 * lh + 0.60D2 * lh * t40
      t78 = pi * t77
      t79 = t78 * t46
      t90 = t4 * t7
      t91 = x3 * t9
      t92 = t91 * t12
      t95 = log(0.4D1 * t92 * t19)
      t97 = -0.1D1 + x3
      t98 = t97 * x4
      t102 = log(-0.4D1 * t92 * t18 * t98)
      t105 = 0.1D1 / x3
      t107 = t49 * t51
      t111 = t18 * t97
      t114 = log(-0.4D1 * t92 * t111)
      t116 = t114 ** 2
      t119 = t12 * t15
      t120 = t119 * t17
      t121 = t91 * t120
      t123 = log(0.4D1 * t121)
      t125 = t123 ** 2
      t142 = t17 * x4
      t145 = log(0.4D1 * t119 * t142)
      t151 = t145 ** 2
      t171 = x3 * t12
      t174 = log(0.4D1 * t171 * t19)
      t176 = t174 ** 2
      t179 = t171 * t15
      t180 = t17 * t97
      t184 = log(-0.4D1 * t179 * t180 * x4)
      t186 = t184 ** 2
      t203 = t7 * t23
      t211 = log(-0.4D1 * t171 * t111)
      t212 = t211 ** 2
      t215 = log(0.4D1 * t171 * t18)
      t216 = t215 ** 2
      t220 = t7 * t26
      t228 = t7 * t8
      t241 = log(0.4D1 * t120)
      t242 = t241 * pi
      t245 = t241 ** 2
      t246 = t245 * pi
      t256 = t245 * t241 * pi
      t262 = t40 ** 2
      t263 = t42 ** 2
      t276 = t245 ** 2
      t283 = (0.90D2 * t4 * t7 * (t8 - t22 * t23 + t25 * t26 / 0.2D1) - 
     #0.180D3 * t33 * t34 * (t23 - t22 * t26) + t47) * t49 * t51 / 0.720
     #D3 + (t45 * t34 * (t23 - t56 * t26) + 0.90D2 * t4 * t7 * (-t56 * t
     #8 + t62 * t23 / 0.2D1 - t62 * t56 * t26 / 0.6D1) + t79 - 0.180D3 *
     # t33 * t34 * (t8 - t56 * t23 + t62 * t26 / 0.2D1)) * t49 / 0.720D3
     # + t90 * (-t95 * t26 + t102 * t26) * t105 * t107 / 0.8D1 - (0.90D2
     # * t4 * t7 * (-t114 * t23 + t116 * t26 / 0.2D1 + t123 * t23 - t125
     # * t26 / 0.2D1) - 0.180D3 * t33 * t34 * (-t114 * t26 + t123 * t26)
     #) * t105 * t49 / 0.720D3 - (t45 * t34 * (-t23 + t145 * t26) + 0.90
     #D2 * t4 * t7 * (t145 * t8 - t151 * t23 / 0.2D1 + t151 * t145 * t26
     # / 0.6D1) - t79 - 0.180D3 * t33 * t34 * (-t8 + t145 * t23 - t151 *
     # t26 / 0.2D1)) * t51 / 0.1440D4 + (0.90D2 * t4 * t7 * (-t174 * t23
     # + t176 * t26 / 0.2D1 + t184 * t23 - t186 * t26 / 0.2D1) - 0.180D3
     # * t33 * t34 * (-t174 * t26 + t184 * t26)) * t105 * t51 / 0.1440D4
     # - ((-0.90D2 * t4 * t203 + 0.180D3 * t33 * t46) * (-t212 / 0.2D1 +
     # t216 / 0.2D1) - 0.90D2 * t4 * t220 * (-t216 * t215 / 0.6D1 + t212
     # * t211 / 0.6D1) + (-0.90D2 * t4 * t228 + 0.180D3 * t33 * t34 * t2
     #3 - t47) * (t211 - t215)) * t105 / 0.1440D4 + (t45 + 0.180D3 * t24
     #2 * lh + 0.45D2 * t246) * t3 * t228 / 0.1440D4 + (t78 - t242 * t44
     # - 0.90D2 * t246 * lh - 0.15D2 * t256) * t3 * t203 / 0.1440D4 + (p
     #i * (t262 + 0.60D2 * t263 + 0.480D3 * lh * zeta3 - 0.60D2 * t42 * 
     #t40) - t242 * t77 + t246 * t44 / 0.2D1 + 0.30D2 * t256 * lh + 0.15
     #D2 / 0.4D1 * t276 * pi) * t3 * t220 / 0.1440D4
      t284 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t283)
      t286 = -0.1D1 + x4
      t287 = t2 * t286
      t288 = t2 * x4
      t289 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t287, 0
     #.0D0, t288, 0.0D0)
      t290 = t13 * t15
      t291 = t142 * t286
      t294 = log(-0.4D1 * t291 * t290)
      t295 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t287, 0
     #.0D0, t288, 0.0D0)
      t297 = t294 ** 2
      t298 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t287, 0
     #.0D0, t288, 0.0D0)
      t310 = t34 * t298
      t316 = t91 * t119
      t317 = x4 * t286
      t318 = t180 * t317
      t321 = log(0.4D1 * t316 * t318)
      t326 = log(-0.4D1 * t92 * t18 * t317)
      t335 = log(-0.4D1 * t119 * t291)
      t341 = t335 ** 2
      t364 = log(-0.4D1 * t179 * t291)
      t366 = t364 ** 2
      t371 = log(0.4D1 * t179 * t318)
      t373 = t371 ** 2
      t390 = (0.90D2 * t4 * t7 * (-t289 + t294 * t295 - t297 * t298 / 0.
     #2D1) - 0.180D3 * t33 * t34 * (-t295 + t294 * t298) - t45 * t310) *
     # t49 * t51 / 0.720D3 + t90 * (-t321 * t298 + t326 * t298) * t105 *
     # t107 / 0.8D1 - (t45 * t34 * (t295 - t335 * t298) + 0.90D2 * t4 * 
     #t7 * (-t335 * t289 + t341 * t295 / 0.2D1 - t341 * t335 * t298 / 0.
     #6D1) + t78 * t310 - 0.180D3 * t33 * t34 * (t289 - t335 * t295 + t3
     #41 * t298 / 0.2D1)) * t51 / 0.1440D4 + (0.90D2 * t4 * t7 * (t364 *
     # t295 - t366 * t298 / 0.2D1 - t371 * t295 + t373 * t298 / 0.2D1) -
     # 0.180D3 * t33 * t34 * (t364 * t298 - t371 * t298)) * t105 * t51 /
     # 0.1440D4
      t391 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t287, t288, 0.0D0, t390)
      t393 = t2 * x1
      t394 = -0.1D1 + x1
      t395 = t2 * t394
      t396 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t393, -t395, 0.
     #0D0, 0.0D0, 0.0D0)
      t397 = t394 ** 2
      t398 = t17 * t397
      t402 = log(0.4D1 * t290 * t398 * x4)
      t403 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t393, -t395, 0.
     #0D0, 0.0D0, 0.0D0)
      t405 = t402 ** 2
      t406 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t393, -t395, 0.
     #0D0, 0.0D0, 0.0D0)
      t418 = t34 * t406
      t419 = t45 * t418
      t423 = t18 * t397
      t426 = log(0.4D1 * t13 * t423)
      t432 = t426 ** 2
      t452 = t397 * x4
      t456 = log(0.4D1 * t92 * t18 * t452)
      t469 = log(0.4D1 * t92 * t423)
      t471 = t469 ** 2
      t487 = (0.90D2 * t4 * t7 * (-t396 + t402 * t403 - t405 * t406 / 0.
     #2D1) - 0.180D3 * t33 * t34 * (-t403 + t402 * t406) - t419) * t49 *
     # t51 / 0.720D3 + (-t45 * t34 * (t403 - t426 * t406) - 0.90D2 * t4 
     #* t7 * (-t426 * t396 + t432 * t403 / 0.2D1 - t432 * t426 * t406 / 
     #0.6D1) - t78 * t418 + 0.180D3 * t33 * t34 * (t396 - t426 * t403 + 
     #t432 * t406 / 0.2D1)) * t49 / 0.720D3 + (0.90D2 * t4 * t7 * (-t403
     # + t456 * t406) + 0.180D3 * t33 * t418) * t105 * t107 / 0.720D3 - 
     #(0.90D2 * t4 * t7 * (t396 - t469 * t403 + t471 * t406 / 0.2D1) - 0
     #.180D3 * t33 * t34 * (t403 - t469 * t406) + t419) * t105 * t49 / 0
     #.720D3
      t488 = FJET(XB1, XB2, s, t393, 0.0D0, -t395, 0.0D0, 0.0D0, t487)
      t490 = -t286
      t491 = KAPPA2(x1, x2, 0.10D1, t490, z)
      t492 = s * t491
      t493 = t1 * x1
      t494 = t492 * t493
      t495 = t1 * t394
      t496 = t495 * t286
      t497 = t492 * t496
      t498 = t495 * x4
      t499 = t492 * t498
      t500 = t491 ** 2
      t503 = x1 * t394
      t505 = s * t500 * t16 * t503 * x4
      t507 = 0.1D1 / (-0.2D1 + t491)
      t508 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t494, t497, 0.0
     #D0, -t499, -t505)
      t510 = t500 ** 2
      t515 = log(-0.4D1 * t54 * t452 * t286 * t510)
      t516 = t515 * t507
      t517 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t494, t497, 0.0
     #D0, -t499, -t505)
      t519 = t515 ** 2
      t521 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t494, t497, 0.0
     #D0, -t499, -t505)
      t528 = t507 * t517
      t534 = t45 * t3
      t536 = t7 * t507 * t521
      t545 = log(-0.4D1 * t316 * t398 * t317 * t510)
      t552 = t33 * t3
      t559 = (-0.90D2 * t4 * t7 * (t507 * t508 - t516 * t517 + t519 * t5
     #07 * t521 / 0.2D1) + 0.180D3 * t33 * t34 * (t528 - t516 * t521) - 
     #t534 * t536) * t49 * t51 / 0.720D3 + (0.90D2 * t4 * t7 * (-t528 + 
     #t545 * t507 * t521) + 0.180D3 * t552 * t536) * t105 * t107 / 0.720
     #D3
      t560 = FJET(XB1, XB2, s, t494, 0.0D0, t497, -t499, -t505, t559)
      t562 = -t97
      t563 = KAPPA2(x1, x2, t562, 0.10D1, z)
      t564 = s * t563
      t565 = t493 * t97
      t566 = t564 * t565
      t567 = t493 * x3
      t568 = t564 * t567
      t569 = t564 * t495
      t570 = t563 ** 2
      t574 = s * t570 * t16 * t503 * x3
      t576 = 0.1D1 / (-0.2D1 + t563)
      t577 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t566, -t569, t
     #568, 0.0D0, -t574)
      t578 = t576 * t577
      t579 = t570 ** 2
      t584 = log(-0.4D1 * t316 * t398 * t98 * t579)
      t586 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t566, -t569, t
     #568, 0.0D0, -t574)
      t593 = t7 * t576 * t586
      t599 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, -t566, -t569, t
     #568, 0.0D0, -t574)
      t605 = log(-0.4D1 * t316 * t398 * t97 * t579)
      t606 = t605 * t576
      t608 = t605 ** 2
      t626 = (0.90D2 * t4 * t7 * (-t578 + t584 * t576 * t586) + 0.180D3 
     #* t552 * t593) * t105 * t107 / 0.720D3 - (0.90D2 * t4 * t7 * (t576
     # * t599 - t606 * t577 + t608 * t576 * t586 / 0.2D1) - 0.180D3 * t3
     #3 * t34 * (t578 - t606 * t586) + t534 * t593) * t105 * t49 / 0.720
     #D3
      t627 = FJET(XB1, XB2, s, -t566, t568, -t569, 0.0D0, -t574, t626)
      t629 = KAPPA2(x1, x2, t562, t490, z)
      t630 = s * t629
      t631 = t630 * t565
      t632 = t630 * t567
      t633 = t630 * t496
      t634 = t630 * t498
      t635 = t629 ** 2
      t640 = cos(t10)
      t643 = Sqrt(x3 * t97 * t317)
      t648 = s * t635 * t16 * t503 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t640 * t643)
      t650 = 0.1D1 / (-0.2D1 + t629)
      t651 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t631, t633, t6
     #32, -t634, t648)
      t654 = t635 ** 2
      t659 = log(0.4D1 * t121 * t397 * t97 * t317 * t654)
      t661 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t631, t633, t6
     #32, -t634, t648)
      t671 = 0.90D2 * t4 * t7 * (t650 * t651 - t659 * t650 * t661) - 0.1
     #80D3 * t552 * t7 * t650 * t661
      t675 = FJET(XB1, XB2, s, -t631, t632, t633, -t634, t648, t671 * t1
     #05 * t107 / 0.720D3)
      rrqq2qqht3s5e1 = t284 * t283 + t391 * t390 + t488 * t487 + t560 * 
     #t559 + t627 * t626 + t675 * t671 * t105 * t49 * t51 / 0.720D3

      end function



      doubleprecision function rrqq2qqht3s5e0
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t16 * x4
      t20 = log(0.4D1 * t14 * t17)
      t21 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t23 = t20 ** 2
      t24 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t31 = pi * lh
      t32 = t3 * t7
      t38 = pi ** 2
      t40 = lh ** 2
      t42 = -0.30D2 * t38 + 0.180D3 * t40
      t43 = pi * t42
      t44 = t32 * t24
      t45 = t43 * t44
      t47 = 0.1D1 / x4
      t51 = t14 * t16
      t53 = log(0.4D1 * t51)
      t54 = t53 * pi
      t63 = t53 ** 2
      t64 = t63 * pi
      t68 = t7 * t21
      t86 = t7 * t24
      t89 = x1 ** 2
      t90 = t89 * t11
      t91 = t13 * t16
      t92 = t91 * x4
      t95 = log(0.4D1 * t90 * t92)
      t102 = 0.180D3 * t31 * t44
      t104 = 0.1D1 / x1
      t108 = t4 * t7
      t109 = x3 * t89
      t110 = t109 * t11
      t111 = -0.1D1 + x3
      t112 = t91 * t111
      t115 = log(-0.4D1 * t110 * t112)
      t119 = log(0.4D1 * t109 * t51)
      t122 = 0.1D1 / x3
      t127 = t90 * t91
      t129 = log(0.4D1 * t127)
      t131 = t129 ** 2
      t146 = x3 * t11
      t149 = log(0.4D1 * t146 * t92)
      t151 = t146 * t13
      t152 = t16 * t111
      t156 = log(-0.4D1 * t151 * t152 * x4)
      t165 = log(-0.4D1 * t146 * t112)
      t166 = t165 ** 2
      t169 = log(0.4D1 * t146 * t91)
      t170 = t169 ** 2
      t184 = -(0.90D2 * t4 * t7 * (-t8 + t20 * t21 - t23 * t24 / 0.2D1) 
     #- 0.180D3 * t31 * t32 * (-t21 + t20 * t24) - t45) * t47 / 0.1440D4
     # + (-0.180D3 * t31 - 0.90D2 * t54) * t3 * t7 * t8 / 0.1440D4 + (t4
     #3 + 0.180D3 * t54 * lh + 0.45D2 * t64) * t3 * t68 / 0.1440D4 + (pi
     # * (-0.240D3 * zeta3 - 0.120D3 * lh * t40 + 0.60D2 * lh * t38) - t
     #54 * t42 - 0.90D2 * t64 * lh - 0.15D2 * t63 * t53 * pi) * t3 * t86
     # / 0.1440D4 + (0.90D2 * t4 * t7 * (t21 - t95 * t24) - t102) * t104
     # * t47 / 0.720D3 - t108 * (-t115 * t24 + t119 * t24) * t122 * t104
     # / 0.8D1 + (0.90D2 * t4 * t7 * (t8 - t129 * t21 + t131 * t24 / 0.2
     #D1) - 0.180D3 * t31 * t32 * (t21 - t129 * t24) + t45) * t104 / 0.7
     #20D3 + t108 * (-t149 * t24 + t156 * t24) * t122 * t47 / 0.16D2 - (
     #-0.90D2 * t4 * t86 * (-t166 / 0.2D1 + t170 / 0.2D1) + (-0.90D2 * t
     #4 * t68 + t102) * (t165 - t169)) * t122 / 0.1440D4
      t185 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t184)
      t187 = -0.1D1 + x4
      t188 = t2 * t187
      t189 = t2 * x4
      t190 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t188, 0
     #.0D0, t189, 0.0D0)
      t191 = t90 * t13
      t192 = t17 * t187
      t195 = log(-0.4D1 * t191 * t192)
      t196 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t188, 0
     #.0D0, t189, 0.0D0)
      t202 = t32 * t196
      t211 = log(-0.4D1 * t151 * t192)
      t213 = x4 * t187
      t217 = log(0.4D1 * t151 * t152 * t213)
      t224 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t188, 0
     #.0D0, t189, 0.0D0)
      t227 = log(-0.4D1 * t14 * t192)
      t229 = t227 ** 2
      t245 = (0.90D2 * t4 * t7 * (-t190 + t195 * t196) + 0.180D3 * t31 *
     # t202) * t104 * t47 / 0.720D3 + t108 * (t211 * t196 - t217 * t196)
     # * t122 * t47 / 0.16D2 - (0.90D2 * t4 * t7 * (t224 - t227 * t190 +
     # t229 * t196 / 0.2D1) - 0.180D3 * t31 * t32 * (t190 - t227 * t196)
     # + t43 * t202) * t47 / 0.1440D4
      t246 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t188, t189, 0.0D0, t245)
      t248 = t2 * x1
      t249 = -0.1D1 + x1
      t250 = t2 * t249
      t251 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t248, -t250, 0.
     #0D0, 0.0D0, 0.0D0)
      t252 = t249 ** 2
      t253 = t16 * t252
      t257 = log(0.4D1 * t191 * t253 * x4)
      t258 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t248, -t250, 0.
     #0D0, 0.0D0, 0.0D0)
      t264 = t32 * t258
      t266 = 0.180D3 * t31 * t264
      t272 = t104 * t47
      t276 = t91 * t252
      t279 = log(0.4D1 * t110 * t276)
      t289 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t248, -t250, 0.
     #0D0, 0.0D0, 0.0D0)
      t292 = log(0.4D1 * t90 * t276)
      t294 = t292 ** 2
      t310 = (0.90D2 * t4 * t7 * (-t251 + t257 * t258) + t266) * t104 * 
     #t47 / 0.720D3 - t108 * t258 * t122 * t272 / 0.8D1 - (0.90D2 * t4 *
     # t7 * (t251 - t279 * t258) - t266) * t122 * t104 / 0.720D3 + (-0.9
     #0D2 * t4 * t7 * (t289 - t292 * t251 + t294 * t258 / 0.2D1) + 0.180
     #D3 * t31 * t32 * (t251 - t292 * t258) - t43 * t264) * t104 / 0.720
     #D3
      t311 = FJET(XB1, XB2, s, t248, 0.0D0, -t250, 0.0D0, 0.0D0, t310)
      t313 = -t187
      t314 = KAPPA2(x1, x2, 0.10D1, t313, z)
      t315 = s * t314
      t316 = t1 * x1
      t317 = t315 * t316
      t318 = t1 * t249
      t319 = t318 * t187
      t320 = t315 * t319
      t321 = t318 * x4
      t322 = t315 * t321
      t323 = t314 ** 2
      t326 = x1 * t249
      t328 = s * t323 * t15 * t326 * x4
      t330 = 0.1D1 / (-0.2D1 + t314)
      t331 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t317, t320, 0.0
     #D0, -t322, -t328)
      t334 = t323 ** 2
      t339 = log(-0.4D1 * t127 * t252 * x4 * t187 * t334)
      t341 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t317, t320, 0.0
     #D0, -t322, -t328)
      t347 = t31 * t3
      t348 = t7 * t330
      t361 = (-0.90D2 * t4 * t7 * (t330 * t331 - t339 * t330 * t341) + 0
     #.180D3 * t347 * t348 * t341) * t104 * t47 / 0.720D3 - t4 * t348 * 
     #t341 * t122 * t272 / 0.8D1
      t362 = FJET(XB1, XB2, s, t317, 0.0D0, t320, -t322, -t328, t361)
      t364 = -t111
      t365 = KAPPA2(x1, x2, t364, 0.10D1, z)
      t366 = s * t365
      t367 = t316 * t111
      t368 = t366 * t367
      t369 = t316 * x3
      t370 = t366 * t369
      t371 = t366 * t318
      t372 = t365 ** 2
      t376 = s * t372 * t15 * t326 * x3
      t378 = 0.1D1 / (-0.2D1 + t365)
      t379 = t7 * t378
      t381 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t368, -t371, t
     #370, 0.0D0, -t376)
      t386 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t368, -t371, t
     #370, 0.0D0, -t376)
      t389 = t372 ** 2
      t394 = log(-0.4D1 * t109 * t14 * t253 * t111 * t389)
      t408 = -t4 * t379 * t381 * t122 * t272 / 0.8D1 - (0.90D2 * t4 * t7
     # * (t378 * t386 - t394 * t378 * t381) - 0.180D3 * t347 * t379 * t3
     #81) * t122 * t104 / 0.720D3
      t409 = FJET(XB1, XB2, s, -t368, t370, -t371, 0.0D0, -t376, t408)
      t411 = KAPPA2(x1, x2, t364, t313, z)
      t412 = s * t411
      t413 = t412 * t367
      t414 = t412 * t369
      t415 = t412 * t319
      t416 = t412 * t321
      t417 = t411 ** 2
      t422 = cos(t9)
      t425 = Sqrt(x3 * t111 * t213)
      t430 = s * t417 * t15 * t326 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t422 * t425)
      t432 = 0.1D1 / (-0.2D1 + t411)
      t435 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t413, t415, t4
     #14, -t416, t430)
      t440 = FJET(XB1, XB2, s, -t413, t414, t415, -t416, t430, t4 * t7 *
     # t432 * t435 * t122 * t272 / 0.8D1)
      rrqq2qqht3s5e0 = t185 * t184 + t246 * t245 + t311 * t310 + t362 * 
     #t361 + t409 * t408 + t440 * pi * t32 * t432 * t435 * t122 * t104 *
     # t47 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s5em1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = sin(x2 * pi)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t16 * x4
      t20 = log(0.4D1 * t14 * t17)
      t21 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t27 = pi * lh
      t28 = t3 * t7
      t31 = 0.180D3 * t27 * t28 * t21
      t33 = 0.1D1 / x4
      t36 = x1 ** 2
      t37 = t36 * t11
      t38 = t13 * t16
      t41 = log(0.4D1 * t37 * t38)
      t48 = 0.1D1 / x1
      t51 = t4 * t7
      t56 = x3 * t11
      t57 = -0.1D1 + x3
      t61 = log(-0.4D1 * t56 * t38 * t57)
      t64 = log(0.4D1 * t56 * t38)
      t67 = 0.1D1 / x3
      t71 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t78 = log(0.4D1 * t14 * t16)
      t79 = t78 * pi
      t86 = pi ** 2
      t88 = lh ** 2
      t94 = t78 ** 2
      t102 = -(0.90D2 * t4 * t7 * (-t8 + t20 * t21) + t31) * t33 / 0.144
     #0D4 + (0.90D2 * t4 * t7 * (t8 - t41 * t21) - t31) * t48 / 0.720D3 
     #+ t51 * t21 * t48 * t33 / 0.8D1 + t51 * t21 * (t61 - t64) * t67 / 
     #0.16D2 + t4 * t7 * t71 / 0.16D2 + (-0.180D3 * t27 - 0.90D2 * t79) 
     #* t3 * t7 * t8 / 0.1440D4 + (pi * (-0.30D2 * t86 + 0.180D3 * t88) 
     #+ 0.180D3 * t79 * lh + 0.45D2 * t94 * pi) * t3 * t7 * t21 / 0.1440
     #D4
      t103 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t102)
      t105 = -0.1D1 + x4
      t106 = t2 * t105
      t107 = t2 * x4
      t108 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t106, 0
     #.0D0, t107, 0.0D0)
      t112 = log(-0.4D1 * t14 * t17 * t105)
      t113 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t106, 0
     #.0D0, t107, 0.0D0)
      t129 = -(0.90D2 * t4 * t7 * (t108 - t112 * t113) - 0.180D3 * t27 *
     # t28 * t113) * t33 / 0.1440D4 - t51 * t113 * t48 * t33 / 0.8D1
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t106, t107, 0.0D0, t129)
      t132 = t2 * x1
      t133 = -0.1D1 + x1
      t134 = t2 * t133
      t135 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t132, -t134, 0.
     #0D0, 0.0D0, 0.0D0)
      t140 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t132, -t134, 0.
     #0D0, 0.0D0, 0.0D0)
      t141 = t133 ** 2
      t145 = log(0.4D1 * t37 * t38 * t141)
      t161 = -t51 * t135 * t67 * t48 / 0.8D1 + (-0.90D2 * t4 * t7 * (t14
     #0 - t145 * t135) + 0.180D3 * t27 * t28 * t135) * t48 / 0.720D3 - t
     #51 * t135 * t48 * t33 / 0.8D1
      t162 = FJET(XB1, XB2, s, t132, 0.0D0, -t134, 0.0D0, 0.0D0, t161)
      t165 = KAPPA2(x1, x2, 0.10D1, -t105, z)
      t166 = s * t165
      t167 = t1 * x1
      t168 = t166 * t167
      t169 = t1 * t133
      t171 = t166 * t169 * t105
      t173 = t166 * t169 * x4
      t174 = t165 ** 2
      t177 = x1 * t133
      t179 = s * t174 * t15 * t177 * x4
      t182 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t168, t171, 0.0
     #D0, -t173, -t179)
      t185 = 0.1D1 / (-0.2D1 + t165) * t182 * t48 * t33
      t188 = FJET(XB1, XB2, s, t168, 0.0D0, t171, -t173, -t179, -t51 * t
     #185 / 0.8D1)
      t194 = KAPPA2(x1, x2, -t57, 0.10D1, z)
      t195 = s * t194
      t197 = t195 * t167 * t57
      t199 = t195 * t167 * x3
      t200 = t195 * t169
      t201 = t194 ** 2
      t205 = s * t201 * t15 * t177 * x3
      t208 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t197, -t200, t
     #199, 0.0D0, -t205)
      t211 = 0.1D1 / (-0.2D1 + t194) * t208 * t67 * t48
      t214 = FJET(XB1, XB2, s, -t197, t199, -t200, 0.0D0, -t205, -t51 * 
     #t211 / 0.8D1)
      rrqq2qqht3s5em1 = t103 * t102 + t130 * t129 + t162 * t161 - t188 *
     # pi * t28 * t185 / 0.8D1 - t214 * pi * t28 * t211 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s5em2
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = 0.1D1 / x4
      t18 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t25 = sin(x2 * pi)
      t26 = t25 ** 2
      t27 = z ** 2
      t30 = t1 ** 2
      t31 = t30 ** 2
      t34 = log(0.4D1 * t26 / t27 * t31)
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t9 * t14 / 0.16D2 + t4 * t7 * t
     #18 / 0.16D2 + (-0.180D3 * pi * lh - 0.90D2 * t34 * pi) * t3 * t9 /
     # 0.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t41)
      t44 = t2 * x1
      t46 = t2 * (-0.1D1 + x1)
      t47 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t44, -t46, 0.0D0
     #, 0.0D0, 0.0D0)
      t49 = t7 * t47 * t10
      t52 = FJET(XB1, XB2, s, t44, 0.0D0, -t46, 0.0D0, 0.0D0, -t4 * t49 
     #/ 0.8D1)
      t58 = t2 * (-0.1D1 + x4)
      t59 = t2 * x4
      t60 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t58, 0.0
     #D0, t59, 0.0D0)
      t62 = t7 * t60 * t14
      t65 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t58, t59, 0.0D0, -t4 * t62 
     #/ 0.16D2)
      rrqq2qqht3s5em2 = t42 * t41 - t52 * pi * t3 * t49 / 0.8D1 - t65 * 
     #pi * t3 * t62 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s5em3
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrqq2qqht3s5em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s5em4
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqq2qqht3s5em4 = 0.0D0

      end function


      doubleprecision function rrqq2qqht3s6e1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t9 = x1 ** 2
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = t18 * x4
      t22 = log(0.4D1 * t13 * t19)
      t23 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t25 = t22 ** 2
      t26 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t33 = pi * lh
      t34 = t3 * t7
      t40 = pi ** 2
      t42 = lh ** 2
      t44 = -0.30D2 * t40 + 0.180D3 * t42
      t45 = pi * t44
      t46 = t34 * t26
      t47 = t45 * t46
      t49 = 0.1D1 / x1
      t51 = 0.1D1 / x4
      t54 = t13 * t18
      t56 = log(0.4D1 * t54)
      t62 = t56 ** 2
      t77 = -0.240D3 * zeta3 - 0.120D3 * t42 * lh + 0.60D2 * lh * t40
      t78 = pi * t77
      t79 = t78 * t46
      t90 = t4 * t7
      t91 = x3 * t9
      t92 = t91 * t12
      t93 = -0.1D1 + x3
      t94 = t93 * x4
      t98 = log(-0.4D1 * t92 * t18 * t94)
      t102 = log(0.4D1 * t92 * t19)
      t105 = 0.1D1 / x3
      t107 = t49 * t51
      t111 = t12 * t15
      t112 = t111 * t17
      t113 = t91 * t112
      t115 = log(0.4D1 * t113)
      t117 = t115 ** 2
      t120 = t18 * t93
      t123 = log(-0.4D1 * t92 * t120)
      t125 = t123 ** 2
      t142 = t17 * x4
      t145 = log(0.4D1 * t111 * t142)
      t151 = t145 ** 2
      t171 = x3 * t12
      t174 = log(0.4D1 * t171 * t19)
      t176 = t174 ** 2
      t179 = t171 * t15
      t180 = t17 * t93
      t184 = log(-0.4D1 * t179 * t180 * x4)
      t186 = t184 ** 2
      t203 = t7 * t23
      t211 = log(-0.4D1 * t171 * t120)
      t212 = t211 ** 2
      t215 = log(0.4D1 * t171 * t18)
      t216 = t215 ** 2
      t220 = t7 * t26
      t228 = t7 * t8
      t241 = log(0.4D1 * t112)
      t242 = t241 * pi
      t245 = t241 ** 2
      t246 = t245 * pi
      t256 = t245 * t241 * pi
      t262 = t40 ** 2
      t263 = t42 ** 2
      t276 = t245 ** 2
      t283 = (0.90D2 * t4 * t7 * (t8 - t22 * t23 + t25 * t26 / 0.2D1) - 
     #0.180D3 * t33 * t34 * (t23 - t22 * t26) + t47) * t49 * t51 / 0.720
     #D3 + (t45 * t34 * (t23 - t56 * t26) + 0.90D2 * t4 * t7 * (-t56 * t
     #8 + t62 * t23 / 0.2D1 - t62 * t56 * t26 / 0.6D1) + t79 - 0.180D3 *
     # t33 * t34 * (t8 - t56 * t23 + t62 * t26 / 0.2D1)) * t49 / 0.720D3
     # - t90 * (-t98 * t26 + t102 * t26) * t105 * t107 / 0.8D1 + (0.90D2
     # * t4 * t7 * (-t115 * t23 + t117 * t26 / 0.2D1 + t123 * t23 - t125
     # * t26 / 0.2D1) - 0.180D3 * t33 * t34 * (-t115 * t26 + t123 * t26)
     #) * t105 * t49 / 0.720D3 + (t45 * t34 * (t23 - t145 * t26) + 0.90D
     #2 * t4 * t7 * (-t145 * t8 + t151 * t23 / 0.2D1 - t151 * t145 * t26
     # / 0.6D1) + t79 - 0.180D3 * t33 * t34 * (t8 - t145 * t23 + t151 * 
     #t26 / 0.2D1)) * t51 / 0.1440D4 - (0.90D2 * t4 * t7 * (t174 * t23 -
     # t176 * t26 / 0.2D1 - t184 * t23 + t186 * t26 / 0.2D1) - 0.180D3 *
     # t33 * t34 * (t174 * t26 - t184 * t26)) * t105 * t51 / 0.1440D4 - 
     #((-0.90D2 * t4 * t203 + 0.180D3 * t33 * t46) * (-t212 / 0.2D1 + t2
     #16 / 0.2D1) - 0.90D2 * t4 * t220 * (-t216 * t215 / 0.6D1 + t212 * 
     #t211 / 0.6D1) + (-0.90D2 * t4 * t228 + 0.180D3 * t33 * t34 * t23 -
     # t47) * (t211 - t215)) * t105 / 0.1440D4 + (t45 + 0.180D3 * t242 *
     # lh + 0.45D2 * t246) * t3 * t228 / 0.1440D4 + (t78 - t242 * t44 - 
     #0.90D2 * t246 * lh - 0.15D2 * t256) * t3 * t203 / 0.1440D4 + (pi *
     # (t262 + 0.60D2 * t263 + 0.480D3 * lh * zeta3 - 0.60D2 * t42 * t40
     #) - t242 * t77 + t246 * t44 / 0.2D1 + 0.30D2 * t256 * lh + 0.15D2 
     #/ 0.4D1 * t276 * pi) * t3 * t220 / 0.1440D4
      t284 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t283)
      t286 = t2 * x4
      t287 = -0.1D1 + x4
      t288 = t2 * t287
      t289 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t286, 0.
     #0D0, -t288, 0.0D0)
      t290 = t13 * t15
      t291 = t142 * t287
      t294 = log(-0.4D1 * t291 * t290)
      t295 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t286, 0.
     #0D0, -t288, 0.0D0)
      t297 = t294 ** 2
      t298 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t286, 0.
     #0D0, -t288, 0.0D0)
      t310 = t34 * t298
      t316 = t91 * t111
      t317 = x4 * t287
      t318 = t180 * t317
      t321 = log(0.4D1 * t316 * t318)
      t326 = log(-0.4D1 * t92 * t18 * t317)
      t335 = log(-0.4D1 * t111 * t291)
      t341 = t335 ** 2
      t364 = log(-0.4D1 * t179 * t291)
      t366 = t364 ** 2
      t371 = log(0.4D1 * t179 * t318)
      t373 = t371 ** 2
      t390 = (0.90D2 * t4 * t7 * (-t289 + t294 * t295 - t297 * t298 / 0.
     #2D1) - 0.180D3 * t33 * t34 * (-t295 + t294 * t298) - t45 * t310) *
     # t49 * t51 / 0.720D3 - t90 * (t321 * t298 - t326 * t298) * t105 * 
     #t107 / 0.8D1 + (-t45 * t34 * (t295 - t335 * t298) - 0.90D2 * t4 * 
     #t7 * (-t335 * t289 + t341 * t295 / 0.2D1 - t341 * t335 * t298 / 0.
     #6D1) - t78 * t310 + 0.180D3 * t33 * t34 * (t289 - t335 * t295 + t3
     #41 * t298 / 0.2D1)) * t51 / 0.1440D4 - (0.90D2 * t4 * t7 * (-t364 
     #* t295 + t366 * t298 / 0.2D1 + t371 * t295 - t373 * t298 / 0.2D1) 
     #- 0.180D3 * t33 * t34 * (-t364 * t298 + t371 * t298)) * t105 * t51
     # / 0.1440D4
      t391 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t286, -t288, 0.0D0, t390)
      t393 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t394 = s * t393
      t395 = t1 * x1
      t396 = t394 * t395
      t397 = -0.1D1 + x1
      t398 = t1 * t397
      t399 = t394 * t398
      t400 = t393 ** 2
      t404 = s * t400 * t16 * x1 * t397
      t406 = 0.1D1 / (-0.2D1 + t393)
      t407 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t396, 0.0D0, 0.
     #0D0, -t399, -t404)
      t408 = t406 * t407
      t409 = t397 ** 2
      t410 = t17 * t409
      t411 = t400 ** 2
      t413 = t410 * x4 * t411
      t416 = log(0.4D1 * t290 * t413)
      t417 = t416 * t406
      t418 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t396, 0.0D0, 0.
     #0D0, -t399, -t404)
      t420 = t416 ** 2
      t422 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t396, 0.0D0, 0.
     #0D0, -t399, -t404)
      t429 = t406 * t418
      t435 = t45 * t3
      t437 = t7 * t406 * t422
      t438 = t435 * t437
      t445 = log(0.4D1 * t290 * t410 * t411)
      t446 = t445 * t406
      t452 = t445 ** 2
      t453 = t452 * t406
      t477 = log(0.4D1 * t316 * t413)
      t484 = t33 * t3
      t494 = log(0.4D1 * t92 * t18 * t409 * t411)
      t495 = t494 * t406
      t497 = t494 ** 2
      t514 = (0.90D2 * t4 * t7 * (t408 - t417 * t418 + t420 * t406 * t42
     #2 / 0.2D1) - 0.180D3 * t33 * t34 * (t429 - t417 * t422) + t438) * 
     #t49 * t51 / 0.720D3 + (t45 * t34 * (t429 - t446 * t422) + 0.90D2 *
     # t4 * t7 * (-t446 * t407 + t453 * t418 / 0.2D1 - t452 * t445 * t40
     #6 * t422 / 0.6D1) + t78 * t3 * t437 - 0.180D3 * t33 * t34 * (t408 
     #- t446 * t418 + t453 * t422 / 0.2D1)) * t49 / 0.720D3 - (0.90D2 * 
     #t4 * t7 * (-t429 + t477 * t406 * t422) + 0.180D3 * t484 * t437) * 
     #t105 * t107 / 0.720D3 + (0.90D2 * t4 * t7 * (t408 - t495 * t418 + 
     #t497 * t406 * t422 / 0.2D1) - 0.180D3 * t33 * t34 * (t429 - t495 *
     # t422) + t438) * t105 * t49 / 0.720D3
      t515 = FJET(XB1, XB2, s, t396, 0.0D0, 0.0D0, -t399, -t404, t514)
      t517 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t518 = s * t517
      t519 = t518 * t395
      t520 = t398 * x4
      t521 = t518 * t520
      t522 = t398 * t287
      t523 = t518 * t522
      t524 = t517 ** 2
      t527 = x1 * t397
      t529 = s * t524 * t16 * t527 * t287
      t531 = 0.1D1 / (-0.2D1 + t517)
      t532 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t519, -t521, 0.
     #0D0, t523, t529)
      t535 = t524 ** 2
      t540 = log(-0.4D1 * t54 * t409 * x4 * t287 * t535)
      t541 = t540 * t531
      t542 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t519, -t521, 0.
     #0D0, t523, t529)
      t544 = t540 ** 2
      t546 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t519, -t521, 0.
     #0D0, t523, t529)
      t553 = t531 * t542
      t560 = t7 * t531 * t546
      t569 = log(-0.4D1 * t316 * t410 * t317 * t535)
      t582 = (0.90D2 * t4 * t7 * (-t531 * t532 + t541 * t542 - t544 * t5
     #31 * t546 / 0.2D1) - 0.180D3 * t33 * t34 * (-t553 + t541 * t546) -
     # t435 * t560) * t49 * t51 / 0.720D3 - (0.90D2 * t4 * t7 * (t553 - 
     #t569 * t531 * t546) - 0.180D3 * t484 * t560) * t105 * t107 / 0.720
     #D3
      t583 = FJET(XB1, XB2, s, t519, 0.0D0, -t521, t523, t529, t582)
      t585 = -t93
      t586 = KAPPA2(x1, x2, t585, 0.0D0, z)
      t587 = s * t586
      t588 = t395 * t93
      t589 = t587 * t588
      t590 = t395 * x3
      t591 = t587 * t590
      t592 = t587 * t398
      t593 = t586 ** 2
      t597 = s * t593 * t16 * t527 * t93
      t599 = 0.1D1 / (-0.2D1 + t586)
      t600 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t589, 0.0D0, t
     #591, -t592, t597)
      t601 = t599 * t600
      t602 = t593 ** 2
      t607 = log(-0.4D1 * t316 * t410 * t94 * t602)
      t609 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t589, 0.0D0, t
     #591, -t592, t597)
      t616 = t7 * t599 * t609
      t622 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, -t589, 0.0D0, t
     #591, -t592, t597)
      t628 = log(-0.4D1 * t316 * t410 * t93 * t602)
      t629 = t628 * t599
      t631 = t628 ** 2
      t649 = -(0.90D2 * t4 * t7 * (t601 - t607 * t599 * t609) - 0.180D3 
     #* t484 * t616) * t105 * t107 / 0.720D3 + (0.90D2 * t4 * t7 * (-t59
     #9 * t622 + t629 * t600 - t631 * t599 * t609 / 0.2D1) - 0.180D3 * t
     #33 * t34 * (-t601 + t629 * t609) - t435 * t616) * t105 * t49 / 0.7
     #20D3
      t650 = FJET(XB1, XB2, s, -t589, t591, 0.0D0, -t592, t597, t649)
      t652 = KAPPA2(x1, x2, t585, x4, z)
      t653 = s * t652
      t654 = t653 * t588
      t655 = t653 * t590
      t656 = t653 * t520
      t657 = t653 * t522
      t658 = t652 ** 2
      t663 = cos(t10)
      t666 = Sqrt(x3 * t93 * t317)
      t671 = s * t658 * t16 * t527 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t663 * t666)
      t673 = 0.1D1 / (-0.2D1 + t652)
      t674 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t654, -t656, t
     #655, t657, t671)
      t677 = t658 ** 2
      t682 = log(0.4D1 * t113 * t409 * t93 * t317 * t677)
      t684 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t654, -t656, t
     #655, t657, t671)
      t694 = -0.90D2 * t4 * t7 * (t673 * t674 - t682 * t673 * t684) + 0.
     #180D3 * t484 * t7 * t673 * t684
      t698 = FJET(XB1, XB2, s, -t654, t655, -t656, t657, t671, -t694 * t
     #105 * t107 / 0.720D3)
      rrqq2qqht3s6e1 = t284 * t283 + t391 * t390 + t515 * t514 + t583 * 
     #t582 + t650 * t649 - t698 * t694 * t105 * t49 * t51 / 0.720D3

      end function



      doubleprecision function rrqq2qqht3s6e0
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t16 * x4
      t20 = log(0.4D1 * t14 * t17)
      t21 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t23 = t20 ** 2
      t24 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t31 = pi * lh
      t32 = t3 * t7
      t38 = pi ** 2
      t40 = lh ** 2
      t42 = -0.30D2 * t38 + 0.180D3 * t40
      t43 = pi * t42
      t44 = t32 * t24
      t45 = t43 * t44
      t47 = 0.1D1 / x4
      t51 = t14 * t16
      t53 = log(0.4D1 * t51)
      t54 = t53 * pi
      t63 = t53 ** 2
      t64 = t63 * pi
      t68 = t7 * t21
      t86 = t7 * t24
      t89 = x1 ** 2
      t90 = t89 * t11
      t91 = t13 * t16
      t92 = t91 * x4
      t95 = log(0.4D1 * t90 * t92)
      t102 = 0.180D3 * t31 * t44
      t104 = 0.1D1 / x1
      t108 = t4 * t7
      t109 = x3 * t89
      t112 = log(0.4D1 * t109 * t51)
      t114 = t109 * t11
      t115 = -0.1D1 + x3
      t116 = t91 * t115
      t119 = log(-0.4D1 * t114 * t116)
      t122 = 0.1D1 / x3
      t127 = t90 * t91
      t129 = log(0.4D1 * t127)
      t131 = t129 ** 2
      t146 = x3 * t11
      t149 = log(0.4D1 * t146 * t92)
      t151 = t146 * t13
      t152 = t16 * t115
      t156 = log(-0.4D1 * t151 * t152 * x4)
      t165 = log(-0.4D1 * t146 * t116)
      t166 = t165 ** 2
      t169 = log(0.4D1 * t146 * t91)
      t170 = t169 ** 2
      t184 = (0.90D2 * t4 * t7 * (t8 - t20 * t21 + t23 * t24 / 0.2D1) - 
     #0.180D3 * t31 * t32 * (t21 - t20 * t24) + t45) * t47 / 0.1440D4 + 
     #(-0.180D3 * t31 - 0.90D2 * t54) * t3 * t7 * t8 / 0.1440D4 + (t43 +
     # 0.180D3 * t54 * lh + 0.45D2 * t64) * t3 * t68 / 0.1440D4 + (pi * 
     #(-0.240D3 * zeta3 - 0.120D3 * lh * t40 + 0.60D2 * lh * t38) - t54 
     #* t42 - 0.90D2 * t64 * lh - 0.15D2 * t63 * t53 * pi) * t3 * t86 / 
     #0.1440D4 + (0.90D2 * t4 * t7 * (t21 - t95 * t24) - t102) * t104 * 
     #t47 / 0.720D3 + t108 * (-t112 * t24 + t119 * t24) * t122 * t104 / 
     #0.8D1 + (0.90D2 * t4 * t7 * (t8 - t129 * t21 + t131 * t24 / 0.2D1)
     # - 0.180D3 * t31 * t32 * (t21 - t129 * t24) + t45) * t104 / 0.720D
     #3 - t108 * (t149 * t24 - t156 * t24) * t122 * t47 / 0.16D2 - (-0.9
     #0D2 * t4 * t86 * (-t166 / 0.2D1 + t170 / 0.2D1) + (-0.90D2 * t4 * 
     #t68 + t102) * (t165 - t169)) * t122 / 0.1440D4
      t185 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t184)
      t187 = t2 * x4
      t188 = -0.1D1 + x4
      t189 = t2 * t188
      t190 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t187, 0.
     #0D0, -t189, 0.0D0)
      t191 = t90 * t13
      t192 = t17 * t188
      t195 = log(-0.4D1 * t191 * t192)
      t196 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t187, 0.
     #0D0, -t189, 0.0D0)
      t202 = t32 * t196
      t211 = log(-0.4D1 * t151 * t192)
      t213 = x4 * t188
      t217 = log(0.4D1 * t151 * t152 * t213)
      t224 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t187, 0.
     #0D0, -t189, 0.0D0)
      t227 = log(-0.4D1 * t14 * t192)
      t229 = t227 ** 2
      t245 = (0.90D2 * t4 * t7 * (-t190 + t195 * t196) + 0.180D3 * t31 *
     # t202) * t104 * t47 / 0.720D3 - t108 * (-t211 * t196 + t217 * t196
     #) * t122 * t47 / 0.16D2 + (-0.90D2 * t4 * t7 * (t224 - t227 * t190
     # + t229 * t196 / 0.2D1) + 0.180D3 * t31 * t32 * (t190 - t227 * t19
     #6) - t43 * t202) * t47 / 0.1440D4
      t246 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t187, -t189, 0.0D0, t245)
      t248 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t249 = s * t248
      t250 = t1 * x1
      t251 = t249 * t250
      t252 = -0.1D1 + x1
      t253 = t1 * t252
      t254 = t249 * t253
      t255 = t248 ** 2
      t259 = s * t255 * t15 * x1 * t252
      t261 = 0.1D1 / (-0.2D1 + t248)
      t262 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t251, 0.0D0, 0.
     #0D0, -t254, -t259)
      t263 = t261 * t262
      t264 = t252 ** 2
      t265 = t16 * t264
      t266 = t255 ** 2
      t271 = log(0.4D1 * t191 * t265 * x4 * t266)
      t273 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t251, 0.0D0, 0.
     #0D0, -t254, -t259)
      t279 = t31 * t3
      t280 = t7 * t261
      t281 = t280 * t273
      t283 = 0.180D3 * t279 * t281
      t290 = t104 * t47
      t298 = log(0.4D1 * t114 * t91 * t264 * t266)
      t309 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t251, 0.0D0, 0.
     #0D0, -t254, -t259)
      t314 = log(0.4D1 * t191 * t265 * t266)
      t315 = t314 * t261
      t317 = t314 ** 2
      t335 = (0.90D2 * t4 * t7 * (t263 - t271 * t261 * t273) - t283) * t
     #104 * t47 / 0.720D3 + t4 * t280 * t273 * t122 * t290 / 0.8D1 + (0.
     #90D2 * t4 * t7 * (t263 - t298 * t261 * t273) - t283) * t122 * t104
     # / 0.720D3 + (0.90D2 * t4 * t7 * (t261 * t309 - t315 * t262 + t317
     # * t261 * t273 / 0.2D1) - 0.180D3 * t31 * t32 * (t263 - t315 * t27
     #3) + t43 * t3 * t281) * t104 / 0.720D3
      t336 = FJET(XB1, XB2, s, t251, 0.0D0, 0.0D0, -t254, -t259, t335)
      t338 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t339 = s * t338
      t340 = t339 * t250
      t341 = t253 * x4
      t342 = t339 * t341
      t343 = t253 * t188
      t344 = t339 * t343
      t345 = t338 ** 2
      t348 = x1 * t252
      t350 = s * t345 * t15 * t348 * t188
      t352 = 0.1D1 / (-0.2D1 + t338)
      t353 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t340, -t342, 0.
     #0D0, t344, t350)
      t356 = t345 ** 2
      t361 = log(-0.4D1 * t127 * t264 * x4 * t188 * t356)
      t363 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t340, -t342, 0.
     #0D0, t344, t350)
      t369 = t7 * t352
      t382 = (0.90D2 * t4 * t7 * (-t352 * t353 + t361 * t352 * t363) + 0
     #.180D3 * t279 * t369 * t363) * t104 * t47 / 0.720D3 - t4 * t369 * 
     #t363 * t122 * t290 / 0.8D1
      t383 = FJET(XB1, XB2, s, t340, 0.0D0, -t342, t344, t350, t382)
      t385 = -t115
      t386 = KAPPA2(x1, x2, t385, 0.0D0, z)
      t387 = s * t386
      t388 = t250 * t115
      t389 = t387 * t388
      t390 = t250 * x3
      t391 = t387 * t390
      t392 = t387 * t253
      t393 = t386 ** 2
      t397 = s * t393 * t15 * t348 * t115
      t399 = 0.1D1 / (-0.2D1 + t386)
      t400 = t7 * t399
      t402 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t389, 0.0D0, t
     #391, -t392, t397)
      t407 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, -t389, 0.0D0, t
     #391, -t392, t397)
      t410 = t393 ** 2
      t415 = log(-0.4D1 * t109 * t14 * t265 * t115 * t410)
      t429 = -t4 * t400 * t402 * t122 * t290 / 0.8D1 + (0.90D2 * t4 * t7
     # * (-t399 * t407 + t415 * t399 * t402) + 0.180D3 * t279 * t400 * t
     #402) * t122 * t104 / 0.720D3
      t430 = FJET(XB1, XB2, s, -t389, t391, 0.0D0, -t392, t397, t429)
      t432 = KAPPA2(x1, x2, t385, x4, z)
      t433 = s * t432
      t434 = t433 * t388
      t435 = t433 * t390
      t436 = t433 * t341
      t437 = t433 * t343
      t438 = t432 ** 2
      t443 = cos(t9)
      t446 = Sqrt(x3 * t115 * t213)
      t451 = s * t438 * t15 * t348 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t443 * t446)
      t453 = 0.1D1 / (-0.2D1 + t432)
      t456 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t434, -t436, t
     #435, t437, t451)
      t461 = FJET(XB1, XB2, s, -t434, t435, -t436, t437, t451, t4 * t7 *
     # t453 * t456 * t122 * t290 / 0.8D1)
      rrqq2qqht3s6e0 = t185 * t184 + t246 * t245 + t336 * t335 + t383 * 
     #t382 + t430 * t429 + t461 * pi * t32 * t453 * t456 * t122 * t104 *
     # t47 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s6em1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t10 = sin(x2 * pi)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t16 * x4
      t20 = log(0.4D1 * t14 * t17)
      t21 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t27 = pi * lh
      t28 = t3 * t7
      t31 = 0.180D3 * t27 * t28 * t21
      t33 = 0.1D1 / x4
      t36 = x1 ** 2
      t37 = t36 * t11
      t38 = t13 * t16
      t41 = log(0.4D1 * t37 * t38)
      t48 = 0.1D1 / x1
      t51 = t4 * t7
      t56 = x3 * t11
      t57 = -0.1D1 + x3
      t61 = log(-0.4D1 * t56 * t38 * t57)
      t64 = log(0.4D1 * t56 * t38)
      t67 = 0.1D1 / x3
      t71 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t78 = log(0.4D1 * t14 * t16)
      t79 = t78 * pi
      t86 = pi ** 2
      t88 = lh ** 2
      t94 = t78 ** 2
      t102 = (0.90D2 * t4 * t7 * (t8 - t20 * t21) - t31) * t33 / 0.1440D
     #4 + (0.90D2 * t4 * t7 * (t8 - t41 * t21) - t31) * t48 / 0.720D3 + 
     #t51 * t21 * t48 * t33 / 0.8D1 + t51 * t21 * (t61 - t64) * t67 / 0.
     #16D2 + t4 * t7 * t71 / 0.16D2 + (-0.180D3 * t27 - 0.90D2 * t79) * 
     #t3 * t7 * t8 / 0.1440D4 + (pi * (-0.30D2 * t86 + 0.180D3 * t88) + 
     #0.180D3 * t79 * lh + 0.45D2 * t94 * pi) * t3 * t7 * t21 / 0.1440D4
      t103 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t102)
      t105 = t2 * x4
      t106 = -0.1D1 + x4
      t107 = t2 * t106
      t108 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t105, 0.
     #0D0, -t107, 0.0D0)
      t112 = log(-0.4D1 * t14 * t17 * t106)
      t113 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t105, 0.
     #0D0, -t107, 0.0D0)
      t129 = (-0.90D2 * t4 * t7 * (t108 - t112 * t113) + 0.180D3 * t27 *
     # t28 * t113) * t33 / 0.1440D4 - t51 * t113 * t48 * t33 / 0.8D1
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t105, -t107, 0.0D0, t129)
      t132 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t133 = s * t132
      t134 = t1 * x1
      t135 = t133 * t134
      t136 = -0.1D1 + x1
      t137 = t1 * t136
      t138 = t133 * t137
      t139 = t132 ** 2
      t143 = s * t139 * t15 * x1 * t136
      t145 = 0.1D1 / (-0.2D1 + t132)
      t146 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t135, 0.0D0, 0.
     #0D0, -t138, -t143)
      t147 = t145 * t146
      t148 = t67 * t48
      t152 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t135, 0.0D0, 0.
     #0D0, -t138, -t143)
      t155 = t136 ** 2
      t157 = t139 ** 2
      t161 = log(0.4D1 * t37 * t13 * t16 * t155 * t157)
      t176 = t48 * t33
      t180 = t51 * t147 * t148 / 0.8D1 + (0.90D2 * t4 * t7 * (t145 * t15
     #2 - t161 * t145 * t146) - 0.180D3 * t27 * t3 * t7 * t145 * t146) *
     # t48 / 0.720D3 + t51 * t147 * t176 / 0.8D1
      t181 = FJET(XB1, XB2, s, t135, 0.0D0, 0.0D0, -t138, -t143, t180)
      t183 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t184 = s * t183
      t185 = t184 * t134
      t187 = t184 * t137 * x4
      t189 = t184 * t137 * t106
      t190 = t183 ** 2
      t193 = x1 * t136
      t195 = s * t190 * t15 * t193 * t106
      t198 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t185, -t187, 0.
     #0D0, t189, t195)
      t200 = 0.1D1 / (-0.2D1 + t183) * t198 * t176
      t203 = FJET(XB1, XB2, s, t185, 0.0D0, -t187, t189, t195, -t51 * t2
     #00 / 0.8D1)
      t209 = KAPPA2(x1, x2, -t57, 0.0D0, z)
      t210 = s * t209
      t212 = t210 * t134 * t57
      t214 = t210 * t134 * x3
      t215 = t210 * t137
      t216 = t209 ** 2
      t220 = s * t216 * t15 * t193 * t57
      t223 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, -t212, 0.0D0, t
     #214, -t215, t220)
      t225 = 0.1D1 / (-0.2D1 + t209) * t223 * t148
      t228 = FJET(XB1, XB2, s, -t212, t214, 0.0D0, -t215, t220, -t51 * t
     #225 / 0.8D1)
      rrqq2qqht3s6em1 = t103 * t102 + t130 * t129 + t181 * t180 - t203 *
     # pi * t28 * t200 / 0.8D1 - t228 * pi * t28 * t225 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s6em2
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = 0.1D1 / x4
      t18 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t25 = sin(x2 * pi)
      t26 = t25 ** 2
      t27 = z ** 2
      t30 = t1 ** 2
      t31 = t30 ** 2
      t34 = log(0.4D1 * t26 / t27 * t31)
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t9 * t14 / 0.16D2 + t4 * t7 * t
     #18 / 0.16D2 + (-0.180D3 * pi * lh - 0.90D2 * t34 * pi) * t3 * t9 /
     # 0.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t41)
      t44 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t45 = s * t44
      t47 = t45 * t1 * x1
      t48 = -0.1D1 + x1
      t50 = t45 * t1 * t48
      t51 = t44 ** 2
      t55 = s * t51 * t30 * x1 * t48
      t58 = 0.1D1 / (-0.2D1 + t44)
      t59 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t47, 0.0D0, 0.0D
     #0, -t50, -t55)
      t64 = FJET(XB1, XB2, s, t47, 0.0D0, 0.0D0, -t50, -t55, t4 * t7 * t
     #58 * t59 * t10 / 0.8D1)
      t72 = t2 * x4
      t74 = t2 * (-0.1D1 + x4)
      t75 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t72, 0.0D
     #0, -t74, 0.0D0)
      t77 = t7 * t75 * t14
      t80 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t72, -t74, 0.0D0, -t4 * t77 
     #/ 0.16D2)
      rrqq2qqht3s6em2 = t42 * t41 + t64 * pi * t3 * t7 * t58 * t59 * t10
     # / 0.8D1 - t80 * pi * t3 * t77 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s6em3
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrqq2qqht3s6em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s6em4
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqq2qqht3s6em4 = 0.0D0

      end function


      doubleprecision function rrqq2qqht3s7e1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = x1 ** 2
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = t18 * x4
      t22 = log(0.4D1 * t13 * t19)
      t23 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t25 = t22 ** 2
      t26 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t33 = pi * lh
      t34 = t3 * t7
      t40 = pi ** 2
      t42 = lh ** 2
      t44 = -0.30D2 * t40 + 0.180D3 * t42
      t45 = pi * t44
      t46 = t34 * t26
      t47 = t45 * t46
      t49 = 0.1D1 / x1
      t51 = 0.1D1 / x4
      t54 = t13 * t18
      t56 = log(0.4D1 * t54)
      t62 = t56 ** 2
      t77 = -0.240D3 * zeta3 - 0.120D3 * t42 * lh + 0.60D2 * lh * t40
      t78 = pi * t77
      t79 = t78 * t46
      t90 = t4 * t7
      t91 = x3 * t9
      t92 = t91 * t12
      t95 = log(0.4D1 * t92 * t19)
      t97 = -0.1D1 + x3
      t98 = t97 * x4
      t102 = log(-0.4D1 * t92 * t18 * t98)
      t105 = 0.1D1 / x3
      t107 = t49 * t51
      t111 = t12 * t15
      t112 = t111 * t17
      t113 = t91 * t112
      t115 = log(0.4D1 * t113)
      t117 = t115 ** 2
      t120 = t18 * t97
      t123 = log(-0.4D1 * t92 * t120)
      t125 = t123 ** 2
      t142 = t17 * x4
      t145 = log(0.4D1 * t111 * t142)
      t151 = t145 ** 2
      t171 = x3 * t12
      t174 = log(0.4D1 * t171 * t19)
      t176 = t174 ** 2
      t179 = t171 * t15
      t180 = t17 * t97
      t184 = log(-0.4D1 * t179 * t180 * x4)
      t186 = t184 ** 2
      t203 = t7 * t23
      t211 = log(-0.4D1 * t171 * t120)
      t212 = t211 ** 2
      t213 = t171 * t18
      t215 = log(0.4D1 * t213)
      t216 = t215 ** 2
      t220 = t7 * t26
      t228 = t7 * t8
      t241 = log(0.4D1 * t112)
      t242 = t241 * pi
      t245 = t241 ** 2
      t246 = t245 * pi
      t256 = t245 * t241 * pi
      t262 = t40 ** 2
      t263 = t42 ** 2
      t276 = t245 ** 2
      t283 = -(0.90D2 * t4 * t7 * (-t8 + t22 * t23 - t25 * t26 / 0.2D1) 
     #- 0.180D3 * t33 * t34 * (-t23 + t22 * t26) - t47) * t49 * t51 / 0.
     #720D3 - (t45 * t34 * (-t23 + t56 * t26) + 0.90D2 * t4 * t7 * (t56 
     #* t8 - t62 * t23 / 0.2D1 + t62 * t56 * t26 / 0.6D1) - t79 - 0.180D
     #3 * t33 * t34 * (-t8 + t56 * t23 - t62 * t26 / 0.2D1)) * t49 / 0.7
     #20D3 + t90 * (-t95 * t26 + t102 * t26) * t105 * t107 / 0.8D1 + (0.
     #90D2 * t4 * t7 * (-t115 * t23 + t117 * t26 / 0.2D1 + t123 * t23 - 
     #t125 * t26 / 0.2D1) - 0.180D3 * t33 * t34 * (-t115 * t26 + t123 * 
     #t26)) * t105 * t49 / 0.720D3 + (t45 * t34 * (t23 - t145 * t26) + 0
     #.90D2 * t4 * t7 * (-t145 * t8 + t151 * t23 / 0.2D1 - t151 * t145 *
     # t26 / 0.6D1) + t79 - 0.180D3 * t33 * t34 * (t8 - t145 * t23 + t15
     #1 * t26 / 0.2D1)) * t51 / 0.1440D4 + (0.90D2 * t4 * t7 * (-t174 * 
     #t23 + t176 * t26 / 0.2D1 + t184 * t23 - t186 * t26 / 0.2D1) - 0.18
     #0D3 * t33 * t34 * (-t174 * t26 + t184 * t26)) * t105 * t51 / 0.144
     #0D4 + ((0.90D2 * t4 * t203 - 0.180D3 * t33 * t46) * (-t212 / 0.2D1
     # + t216 / 0.2D1) + 0.90D2 * t4 * t220 * (-t216 * t215 / 0.6D1 + t2
     #12 * t211 / 0.6D1) + (0.90D2 * t4 * t228 - 0.180D3 * t33 * t34 * t
     #23 + t47) * (t211 - t215)) * t105 / 0.1440D4 + (t45 + 0.180D3 * t2
     #42 * lh + 0.45D2 * t246) * t3 * t228 / 0.1440D4 + (t78 - t242 * t4
     #4 - 0.90D2 * t246 * lh - 0.15D2 * t256) * t3 * t203 / 0.1440D4 + (
     #pi * (t262 + 0.60D2 * t263 + 0.480D3 * lh * zeta3 - 0.60D2 * t42 *
     # t40) - t242 * t77 + t246 * t44 / 0.2D1 + 0.30D2 * t256 * lh + 0.1
     #5D2 / 0.4D1 * t276 * pi) * t3 * t220 / 0.1440D4
      t284 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t283)
      t286 = -0.1D1 + x4
      t287 = t2 * t286
      t288 = t2 * x4
      t289 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t287, 0
     #.0D0, t288, 0.0D0)
      t291 = t142 * t286
      t294 = log(-0.4D1 * t13 * t15 * t291)
      t295 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t287, 0
     #.0D0, t288, 0.0D0)
      t297 = t294 ** 2
      t298 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t287, 0
     #.0D0, t288, 0.0D0)
      t310 = t34 * t298
      t317 = x4 * t286
      t318 = t180 * t317
      t321 = log(0.4D1 * t91 * t111 * t318)
      t326 = log(-0.4D1 * t92 * t18 * t317)
      t335 = log(-0.4D1 * t111 * t291)
      t341 = t335 ** 2
      t364 = log(-0.4D1 * t179 * t291)
      t366 = t364 ** 2
      t371 = log(0.4D1 * t179 * t318)
      t373 = t371 ** 2
      t390 = -(0.90D2 * t4 * t7 * (t289 - t294 * t295 + t297 * t298 / 0.
     #2D1) - 0.180D3 * t33 * t34 * (t295 - t294 * t298) + t45 * t310) * 
     #t49 * t51 / 0.720D3 + t90 * (-t321 * t298 + t326 * t298) * t105 * 
     #t107 / 0.8D1 + (-t45 * t34 * (t295 - t335 * t298) - 0.90D2 * t4 * 
     #t7 * (-t335 * t289 + t341 * t295 / 0.2D1 - t341 * t335 * t298 / 0.
     #6D1) - t78 * t310 + 0.180D3 * t33 * t34 * (t289 - t335 * t295 + t3
     #41 * t298 / 0.2D1)) * t51 / 0.1440D4 + (0.90D2 * t4 * t7 * (t364 *
     # t295 - t366 * t298 / 0.2D1 - t371 * t295 + t373 * t298 / 0.2D1) -
     # 0.180D3 * t33 * t34 * (t364 * t298 - t371 * t298)) * t105 * t51 /
     # 0.1440D4
      t391 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t287, t288, 0.0D0, t390)
      t393 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t394 = s * t393
      t395 = t1 * x1
      t396 = t394 * t395
      t397 = -0.1D1 + x1
      t398 = t1 * t397
      t399 = t394 * t398
      t400 = t393 ** 2
      t404 = s * t400 * t16 * x1 * t397
      t406 = 0.1D1 / (-0.2D1 + t393)
      t407 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t399, t
     #396, 0.0D0, -t404)
      t408 = t406 * t407
      t409 = t397 ** 2
      t410 = t9 * t409
      t411 = t400 ** 2
      t413 = t410 * x4 * t411
      t416 = log(0.4D1 * t112 * t413)
      t417 = t416 * t406
      t418 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t399, t
     #396, 0.0D0, -t404)
      t420 = t416 ** 2
      t422 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t399, t
     #396, 0.0D0, -t404)
      t429 = t406 * t418
      t435 = t45 * t3
      t437 = t7 * t406 * t422
      t438 = t435 * t437
      t445 = log(0.4D1 * t112 * t410 * t411)
      t446 = t445 * t406
      t452 = t445 ** 2
      t453 = t452 * t406
      t477 = log(0.4D1 * t213 * t413)
      t484 = t33 * t3
      t495 = log(0.4D1 * t179 * t17 * t9 * t409 * t411)
      t496 = t495 * t406
      t498 = t495 ** 2
      t515 = -(0.90D2 * t4 * t7 * (-t408 + t417 * t418 - t420 * t406 * t
     #422 / 0.2D1) - 0.180D3 * t33 * t34 * (-t429 + t417 * t422) - t438)
     # * t49 * t51 / 0.720D3 - (-t45 * t34 * (t429 - t446 * t422) - 0.90
     #D2 * t4 * t7 * (-t446 * t407 + t453 * t418 / 0.2D1 - t452 * t445 *
     # t406 * t422 / 0.6D1) - t78 * t3 * t437 + 0.180D3 * t33 * t34 * (t
     #408 - t446 * t418 + t453 * t422 / 0.2D1)) * t49 / 0.720D3 + (0.90D
     #2 * t4 * t7 * (t429 - t477 * t406 * t422) - 0.180D3 * t484 * t437)
     # * t105 * t107 / 0.720D3 + (0.90D2 * t4 * t7 * (t408 - t496 * t418
     # + t498 * t406 * t422 / 0.2D1) - 0.180D3 * t33 * t34 * (t429 - t49
     #6 * t422) + t438) * t105 * t49 / 0.720D3
      t516 = FJET(XB1, XB2, s, 0.0D0, t396, -t399, 0.0D0, -t404, t515)
      t518 = -t286
      t519 = KAPPA2(x1, x2, 0.0D0, t518, z)
      t520 = s * t519
      t521 = t520 * t395
      t522 = t398 * t286
      t523 = t520 * t522
      t524 = t398 * x4
      t525 = t520 * t524
      t526 = t519 ** 2
      t529 = x1 * t397
      t531 = s * t526 * t16 * t529 * t286
      t533 = 0.1D1 / (-0.2D1 + t519)
      t534 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t523, t5
     #21, -t525, t531)
      t537 = t526 ** 2
      t542 = log(-0.4D1 * t54 * t409 * x4 * t286 * t537)
      t543 = t542 * t533
      t544 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t523, t5
     #21, -t525, t531)
      t546 = t542 ** 2
      t548 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t523, t5
     #21, -t525, t531)
      t555 = t533 * t544
      t562 = t7 * t533 * t548
      t571 = log(-0.4D1 * t213 * t410 * t317 * t537)
      t584 = -(0.90D2 * t4 * t7 * (t533 * t534 - t543 * t544 + t546 * t5
     #33 * t548 / 0.2D1) - 0.180D3 * t33 * t34 * (t555 - t543 * t548) + 
     #t435 * t562) * t49 * t51 / 0.720D3 + (0.90D2 * t4 * t7 * (-t555 + 
     #t571 * t533 * t548) + 0.180D3 * t484 * t562) * t105 * t107 / 0.720
     #D3
      t585 = FJET(XB1, XB2, s, 0.0D0, t521, t523, -t525, t531, t584)
      t587 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t588 = s * t587
      t589 = t395 * x3
      t590 = t588 * t589
      t591 = t395 * t97
      t592 = t588 * t591
      t593 = t588 * t398
      t594 = t587 ** 2
      t598 = s * t594 * t16 * t529 * t97
      t600 = 0.1D1 / (-0.2D1 + t587)
      t601 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t590, -t593, -t
     #592, 0.0D0, t598)
      t602 = t600 * t601
      t603 = t594 ** 2
      t608 = log(-0.4D1 * t213 * t410 * t98 * t603)
      t610 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t590, -t593, -t
     #592, 0.0D0, t598)
      t617 = t7 * t600 * t610
      t623 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t590, -t593, -t
     #592, 0.0D0, t598)
      t629 = log(-0.4D1 * t213 * t410 * t97 * t603)
      t630 = t629 * t600
      t632 = t629 ** 2
      t650 = (0.90D2 * t4 * t7 * (-t602 + t608 * t600 * t610) + 0.180D3 
     #* t484 * t617) * t105 * t107 / 0.720D3 + (0.90D2 * t4 * t7 * (-t60
     #0 * t623 + t630 * t601 - t632 * t600 * t610 / 0.2D1) - 0.180D3 * t
     #33 * t34 * (-t602 + t630 * t610) - t435 * t617) * t105 * t49 / 0.7
     #20D3
      t651 = FJET(XB1, XB2, s, t590, -t592, -t593, 0.0D0, t598, t650)
      t653 = KAPPA2(x1, x2, x3, t518, z)
      t654 = s * t653
      t655 = t654 * t589
      t656 = t654 * t591
      t657 = t654 * t522
      t658 = t654 * t524
      t659 = t653 ** 2
      t664 = cos(t10)
      t667 = Sqrt(x3 * t97 * t317)
      t672 = s * t659 * t16 * t529 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t664 * t667)
      t674 = 0.1D1 / (-0.2D1 + t653)
      t675 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t655, t657, -t6
     #56, -t658, t672)
      t678 = t659 ** 2
      t683 = log(0.4D1 * t113 * t409 * t97 * t317 * t678)
      t685 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t655, t657, -t6
     #56, -t658, t672)
      t695 = 0.90D2 * t4 * t7 * (t674 * t675 - t683 * t674 * t685) - 0.1
     #80D3 * t484 * t7 * t674 * t685
      t699 = FJET(XB1, XB2, s, t655, -t656, t657, -t658, t672, t695 * t1
     #05 * t107 / 0.720D3)
      rrqq2qqht3s7e1 = t284 * t283 + t391 * t390 + t516 * t515 + t585 * 
     #t584 + t651 * t650 + t699 * t695 * t105 * t49 * t51 / 0.720D3

      end function



      doubleprecision function rrqq2qqht3s7e0
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t16 * x4
      t20 = log(0.4D1 * t14 * t17)
      t21 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t23 = t20 ** 2
      t24 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t31 = pi * lh
      t32 = t3 * t7
      t38 = pi ** 2
      t40 = lh ** 2
      t42 = -0.30D2 * t38 + 0.180D3 * t40
      t43 = pi * t42
      t44 = t32 * t24
      t45 = t43 * t44
      t47 = 0.1D1 / x4
      t51 = t14 * t16
      t53 = log(0.4D1 * t51)
      t54 = t53 * pi
      t63 = t53 ** 2
      t64 = t63 * pi
      t68 = t7 * t21
      t86 = t7 * t24
      t89 = x1 ** 2
      t90 = t89 * t11
      t91 = t13 * t16
      t92 = t91 * x4
      t95 = log(0.4D1 * t90 * t92)
      t102 = 0.180D3 * t31 * t44
      t104 = 0.1D1 / x1
      t108 = t4 * t7
      t109 = x3 * t89
      t112 = log(0.4D1 * t109 * t51)
      t115 = -0.1D1 + x3
      t116 = t91 * t115
      t119 = log(-0.4D1 * t109 * t11 * t116)
      t122 = 0.1D1 / x3
      t127 = t90 * t91
      t129 = log(0.4D1 * t127)
      t131 = t129 ** 2
      t146 = x3 * t11
      t149 = log(0.4D1 * t146 * t92)
      t151 = t146 * t13
      t152 = t16 * t115
      t156 = log(-0.4D1 * t151 * t152 * x4)
      t165 = log(-0.4D1 * t146 * t116)
      t166 = t165 ** 2
      t167 = t146 * t91
      t169 = log(0.4D1 * t167)
      t170 = t169 ** 2
      t184 = (0.90D2 * t4 * t7 * (t8 - t20 * t21 + t23 * t24 / 0.2D1) - 
     #0.180D3 * t31 * t32 * (t21 - t20 * t24) + t45) * t47 / 0.1440D4 + 
     #(-0.180D3 * t31 - 0.90D2 * t54) * t3 * t7 * t8 / 0.1440D4 + (t43 +
     # 0.180D3 * t54 * lh + 0.45D2 * t64) * t3 * t68 / 0.1440D4 + (pi * 
     #(-0.240D3 * zeta3 - 0.120D3 * lh * t40 + 0.60D2 * lh * t38) - t54 
     #* t42 - 0.90D2 * t64 * lh - 0.15D2 * t63 * t53 * pi) * t3 * t86 / 
     #0.1440D4 - (0.90D2 * t4 * t7 * (-t21 + t95 * t24) + t102) * t104 *
     # t47 / 0.720D3 + t108 * (-t112 * t24 + t119 * t24) * t122 * t104 /
     # 0.8D1 - (0.90D2 * t4 * t7 * (-t8 + t129 * t21 - t131 * t24 / 0.2D
     #1) - 0.180D3 * t31 * t32 * (-t21 + t129 * t24) - t45) * t104 / 0.7
     #20D3 + t108 * (-t149 * t24 + t156 * t24) * t122 * t47 / 0.16D2 + (
     #0.90D2 * t4 * t86 * (-t166 / 0.2D1 + t170 / 0.2D1) + (0.90D2 * t4 
     #* t68 - t102) * (t165 - t169)) * t122 / 0.1440D4
      t185 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t184)
      t187 = -0.1D1 + x4
      t188 = t2 * t187
      t189 = t2 * x4
      t190 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t188, 0
     #.0D0, t189, 0.0D0)
      t192 = t17 * t187
      t195 = log(-0.4D1 * t90 * t13 * t192)
      t196 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t188, 0
     #.0D0, t189, 0.0D0)
      t202 = t32 * t196
      t211 = log(-0.4D1 * t151 * t192)
      t213 = x4 * t187
      t217 = log(0.4D1 * t151 * t152 * t213)
      t224 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t188, 0
     #.0D0, t189, 0.0D0)
      t227 = log(-0.4D1 * t14 * t192)
      t229 = t227 ** 2
      t245 = -(0.90D2 * t4 * t7 * (t190 - t195 * t196) - 0.180D3 * t31 *
     # t202) * t104 * t47 / 0.720D3 + t108 * (t211 * t196 - t217 * t196)
     # * t122 * t47 / 0.16D2 + (-0.90D2 * t4 * t7 * (t224 - t227 * t190 
     #+ t229 * t196 / 0.2D1) + 0.180D3 * t31 * t32 * (t190 - t227 * t196
     #) - t43 * t202) * t47 / 0.1440D4
      t246 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t188, t189, 0.0D0, t245)
      t248 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t249 = s * t248
      t250 = t1 * x1
      t251 = t249 * t250
      t252 = -0.1D1 + x1
      t253 = t1 * t252
      t254 = t249 * t253
      t255 = t248 ** 2
      t259 = s * t255 * t15 * x1 * t252
      t261 = 0.1D1 / (-0.2D1 + t248)
      t262 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t254, t
     #251, 0.0D0, -t259)
      t263 = t261 * t262
      t264 = t252 ** 2
      t265 = t89 * t264
      t266 = t255 ** 2
      t271 = log(0.4D1 * t51 * t265 * x4 * t266)
      t273 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t254, t
     #251, 0.0D0, -t259)
      t279 = t31 * t3
      t280 = t7 * t261
      t281 = t280 * t273
      t283 = 0.180D3 * t279 * t281
      t290 = t104 * t47
      t299 = log(0.4D1 * t151 * t16 * t89 * t264 * t266)
      t310 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t254, t
     #251, 0.0D0, -t259)
      t315 = log(0.4D1 * t51 * t265 * t266)
      t316 = t315 * t261
      t318 = t315 ** 2
      t336 = -(0.90D2 * t4 * t7 * (-t263 + t271 * t261 * t273) + t283) *
     # t104 * t47 / 0.720D3 + t4 * t280 * t273 * t122 * t290 / 0.8D1 + (
     #0.90D2 * t4 * t7 * (t263 - t299 * t261 * t273) - t283) * t122 * t1
     #04 / 0.720D3 - (-0.90D2 * t4 * t7 * (t261 * t310 - t316 * t262 + t
     #318 * t261 * t273 / 0.2D1) + 0.180D3 * t31 * t32 * (t263 - t316 * 
     #t273) - t43 * t3 * t281) * t104 / 0.720D3
      t337 = FJET(XB1, XB2, s, 0.0D0, t251, -t254, 0.0D0, -t259, t336)
      t339 = -t187
      t340 = KAPPA2(x1, x2, 0.0D0, t339, z)
      t341 = s * t340
      t342 = t341 * t250
      t343 = t253 * t187
      t344 = t341 * t343
      t345 = t253 * x4
      t346 = t341 * t345
      t347 = t340 ** 2
      t350 = x1 * t252
      t352 = s * t347 * t15 * t350 * t187
      t354 = 0.1D1 / (-0.2D1 + t340)
      t355 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t344, t3
     #42, -t346, t352)
      t358 = t347 ** 2
      t363 = log(-0.4D1 * t127 * t264 * x4 * t187 * t358)
      t365 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t344, t3
     #42, -t346, t352)
      t371 = t7 * t354
      t384 = -(0.90D2 * t4 * t7 * (t354 * t355 - t363 * t354 * t365) - 0
     #.180D3 * t279 * t371 * t365) * t104 * t47 / 0.720D3 - t4 * t371 * 
     #t365 * t122 * t290 / 0.8D1
      t385 = FJET(XB1, XB2, s, 0.0D0, t342, t344, -t346, t352, t384)
      t387 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t388 = s * t387
      t389 = t250 * x3
      t390 = t388 * t389
      t391 = t250 * t115
      t392 = t388 * t391
      t393 = t388 * t253
      t394 = t387 ** 2
      t398 = s * t394 * t15 * t350 * t115
      t400 = 0.1D1 / (-0.2D1 + t387)
      t401 = t7 * t400
      t403 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t390, -t393, -t
     #392, 0.0D0, t398)
      t408 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t390, -t393, -t
     #392, 0.0D0, t398)
      t410 = t394 ** 2
      t415 = log(-0.4D1 * t167 * t265 * t115 * t410)
      t429 = -t4 * t401 * t403 * t122 * t290 / 0.8D1 + (0.90D2 * t4 * t7
     # * (-t400 * t408 + t415 * t400 * t403) + 0.180D3 * t279 * t401 * t
     #403) * t122 * t104 / 0.720D3
      t430 = FJET(XB1, XB2, s, t390, -t392, -t393, 0.0D0, t398, t429)
      t432 = KAPPA2(x1, x2, x3, t339, z)
      t433 = s * t432
      t434 = t433 * t389
      t435 = t433 * t391
      t436 = t433 * t343
      t437 = t433 * t345
      t438 = t432 ** 2
      t443 = cos(t9)
      t446 = Sqrt(x3 * t115 * t213)
      t451 = s * t438 * t15 * t350 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t443 * t446)
      t453 = 0.1D1 / (-0.2D1 + t432)
      t456 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t434, t436, -t4
     #35, -t437, t451)
      t461 = FJET(XB1, XB2, s, t434, -t435, t436, -t437, t451, t4 * t7 *
     # t453 * t456 * t122 * t290 / 0.8D1)
      rrqq2qqht3s7e0 = t185 * t184 + t246 * t245 + t337 * t336 + t385 * 
     #t384 + t430 * t429 + t461 * pi * t32 * t453 * t456 * t122 * t104 *
     # t47 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s7em1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = sin(x2 * pi)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t16 * x4
      t20 = log(0.4D1 * t14 * t17)
      t21 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t27 = pi * lh
      t28 = t3 * t7
      t31 = 0.180D3 * t27 * t28 * t21
      t33 = 0.1D1 / x4
      t36 = x1 ** 2
      t38 = t13 * t16
      t41 = log(0.4D1 * t36 * t11 * t38)
      t48 = 0.1D1 / x1
      t51 = t4 * t7
      t56 = x3 * t11
      t57 = -0.1D1 + x3
      t61 = log(-0.4D1 * t56 * t38 * t57)
      t64 = log(0.4D1 * t56 * t38)
      t67 = 0.1D1 / x3
      t71 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t76 = t14 * t16
      t78 = log(0.4D1 * t76)
      t79 = t78 * pi
      t86 = pi ** 2
      t88 = lh ** 2
      t94 = t78 ** 2
      t102 = (0.90D2 * t4 * t7 * (t8 - t20 * t21) - t31) * t33 / 0.1440D
     #4 - (0.90D2 * t4 * t7 * (-t8 + t41 * t21) + t31) * t48 / 0.720D3 +
     # t51 * t21 * t48 * t33 / 0.8D1 + t51 * t21 * (t61 - t64) * t67 / 0
     #.16D2 + t4 * t7 * t71 / 0.16D2 + (-0.180D3 * t27 - 0.90D2 * t79) *
     # t3 * t7 * t8 / 0.1440D4 + (pi * (-0.30D2 * t86 + 0.180D3 * t88) +
     # 0.180D3 * t79 * lh + 0.45D2 * t94 * pi) * t3 * t7 * t21 / 0.1440D
     #4
      t103 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t102)
      t105 = -0.1D1 + x4
      t106 = t2 * t105
      t107 = t2 * x4
      t108 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t106, 0
     #.0D0, t107, 0.0D0)
      t112 = log(-0.4D1 * t14 * t17 * t105)
      t113 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t106, 0
     #.0D0, t107, 0.0D0)
      t129 = (-0.90D2 * t4 * t7 * (t108 - t112 * t113) + 0.180D3 * t27 *
     # t28 * t113) * t33 / 0.1440D4 - t51 * t113 * t48 * t33 / 0.8D1
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t106, t107, 0.0D0, t129)
      t132 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t133 = s * t132
      t134 = t1 * x1
      t135 = t133 * t134
      t136 = -0.1D1 + x1
      t137 = t1 * t136
      t138 = t133 * t137
      t139 = t132 ** 2
      t143 = s * t139 * t15 * x1 * t136
      t145 = 0.1D1 / (-0.2D1 + t132)
      t146 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t138, t
     #135, 0.0D0, -t143)
      t147 = t145 * t146
      t148 = t67 * t48
      t152 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t138, t
     #135, 0.0D0, -t143)
      t154 = t136 ** 2
      t156 = t139 ** 2
      t160 = log(0.4D1 * t76 * t36 * t154 * t156)
      t175 = t48 * t33
      t179 = t51 * t147 * t148 / 0.8D1 - (-0.90D2 * t4 * t7 * (t145 * t1
     #52 - t160 * t145 * t146) + 0.180D3 * t27 * t3 * t7 * t145 * t146) 
     #* t48 / 0.720D3 + t51 * t147 * t175 / 0.8D1
      t180 = FJET(XB1, XB2, s, 0.0D0, t135, -t138, 0.0D0, -t143, t179)
      t183 = KAPPA2(x1, x2, 0.0D0, -t105, z)
      t184 = s * t183
      t185 = t184 * t134
      t187 = t184 * t137 * t105
      t189 = t184 * t137 * x4
      t190 = t183 ** 2
      t193 = x1 * t136
      t195 = s * t190 * t15 * t193 * t105
      t198 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t187, t1
     #85, -t189, t195)
      t200 = 0.1D1 / (-0.2D1 + t183) * t198 * t175
      t203 = FJET(XB1, XB2, s, 0.0D0, t185, t187, -t189, t195, -t51 * t2
     #00 / 0.8D1)
      t208 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t209 = s * t208
      t211 = t209 * t134 * x3
      t213 = t209 * t134 * t57
      t214 = t209 * t137
      t215 = t208 ** 2
      t219 = s * t215 * t15 * t193 * t57
      t222 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t211, -t214, -t
     #213, 0.0D0, t219)
      t224 = 0.1D1 / (-0.2D1 + t208) * t222 * t148
      t227 = FJET(XB1, XB2, s, t211, -t213, -t214, 0.0D0, t219, -t51 * t
     #224 / 0.8D1)
      rrqq2qqht3s7em1 = t103 * t102 + t130 * t129 + t180 * t179 - t203 *
     # pi * t28 * t200 / 0.8D1 - t227 * pi * t28 * t224 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s7em2
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = 0.1D1 / x4
      t18 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t25 = sin(x2 * pi)
      t26 = t25 ** 2
      t27 = z ** 2
      t30 = t1 ** 2
      t31 = t30 ** 2
      t34 = log(0.4D1 * t26 / t27 * t31)
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t9 * t14 / 0.16D2 + t4 * t7 * t
     #18 / 0.16D2 + (-0.180D3 * pi * lh - 0.90D2 * t34 * pi) * t3 * t9 /
     # 0.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t41)
      t44 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t45 = s * t44
      t47 = t45 * t1 * x1
      t48 = -0.1D1 + x1
      t50 = t45 * t1 * t48
      t51 = t44 ** 2
      t55 = s * t51 * t30 * x1 * t48
      t58 = 0.1D1 / (-0.2D1 + t44)
      t59 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t50, t47
     #, 0.0D0, -t55)
      t64 = FJET(XB1, XB2, s, 0.0D0, t47, -t50, 0.0D0, -t55, t4 * t7 * t
     #58 * t59 * t10 / 0.8D1)
      t73 = t2 * (-0.1D1 + x4)
      t74 = t2 * x4
      t75 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t73, 0.0
     #D0, t74, 0.0D0)
      t77 = t7 * t75 * t14
      t80 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t73, t74, 0.0D0, -t4 * t77 
     #/ 0.16D2)
      rrqq2qqht3s7em2 = t42 * t41 + t64 * pi * t3 * t7 * t58 * t59 * t10
     # / 0.8D1 - t80 * pi * t3 * t77 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s7em3
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrqq2qqht3s7em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s7em4
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqq2qqht3s7em4 = 0.0D0

      end function


      doubleprecision function rrqq2qqht3s8e1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t9 = x1 ** 2
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = t9 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t1 ** 2
      t17 = t16 ** 2
      t18 = t15 * t17
      t19 = t18 * x4
      t22 = log(0.4D1 * t13 * t19)
      t23 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t25 = t22 ** 2
      t26 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t33 = pi * lh
      t34 = t3 * t7
      t40 = pi ** 2
      t42 = lh ** 2
      t44 = -0.30D2 * t40 + 0.180D3 * t42
      t45 = pi * t44
      t46 = t34 * t26
      t47 = t45 * t46
      t49 = 0.1D1 / x1
      t51 = 0.1D1 / x4
      t54 = t13 * t18
      t56 = log(0.4D1 * t54)
      t62 = t56 ** 2
      t77 = -0.240D3 * zeta3 - 0.120D3 * t42 * lh + 0.60D2 * lh * t40
      t78 = pi * t77
      t79 = t78 * t46
      t90 = t4 * t7
      t91 = x3 * t9
      t92 = t91 * t12
      t93 = -0.1D1 + x3
      t94 = t93 * x4
      t98 = log(-0.4D1 * t92 * t18 * t94)
      t102 = log(0.4D1 * t92 * t19)
      t105 = 0.1D1 / x3
      t107 = t49 * t51
      t111 = t12 * t15
      t112 = t111 * t17
      t113 = t91 * t112
      t115 = log(0.4D1 * t113)
      t117 = t115 ** 2
      t120 = t18 * t93
      t123 = log(-0.4D1 * t92 * t120)
      t125 = t123 ** 2
      t142 = t17 * x4
      t145 = log(0.4D1 * t111 * t142)
      t151 = t145 ** 2
      t171 = x3 * t12
      t174 = log(0.4D1 * t171 * t19)
      t176 = t174 ** 2
      t179 = t171 * t15
      t180 = t17 * t93
      t184 = log(-0.4D1 * t179 * t180 * x4)
      t186 = t184 ** 2
      t203 = t7 * t23
      t211 = log(-0.4D1 * t171 * t120)
      t212 = t211 ** 2
      t215 = log(0.4D1 * t171 * t18)
      t216 = t215 ** 2
      t220 = t7 * t26
      t228 = t7 * t8
      t241 = log(0.4D1 * t112)
      t242 = t241 * pi
      t245 = t241 ** 2
      t246 = t245 * pi
      t256 = t245 * t241 * pi
      t262 = t40 ** 2
      t263 = t42 ** 2
      t276 = t245 ** 2
      t283 = -(0.90D2 * t4 * t7 * (-t8 + t22 * t23 - t25 * t26 / 0.2D1) 
     #- 0.180D3 * t33 * t34 * (-t23 + t22 * t26) - t47) * t49 * t51 / 0.
     #720D3 + (t45 * t34 * (t23 - t56 * t26) + 0.90D2 * t4 * t7 * (-t56 
     #* t8 + t62 * t23 / 0.2D1 - t62 * t56 * t26 / 0.6D1) + t79 - 0.180D
     #3 * t33 * t34 * (t8 - t56 * t23 + t62 * t26 / 0.2D1)) * t49 / 0.72
     #0D3 - t90 * (-t98 * t26 + t102 * t26) * t105 * t107 / 0.8D1 - (0.9
     #0D2 * t4 * t7 * (t115 * t23 - t117 * t26 / 0.2D1 - t123 * t23 + t1
     #25 * t26 / 0.2D1) - 0.180D3 * t33 * t34 * (t115 * t26 - t123 * t26
     #)) * t105 * t49 / 0.720D3 + (t45 * t34 * (t23 - t145 * t26) + 0.90
     #D2 * t4 * t7 * (-t145 * t8 + t151 * t23 / 0.2D1 - t151 * t145 * t2
     #6 / 0.6D1) + t79 - 0.180D3 * t33 * t34 * (t8 - t145 * t23 + t151 *
     # t26 / 0.2D1)) * t51 / 0.1440D4 - (0.90D2 * t4 * t7 * (t174 * t23 
     #- t176 * t26 / 0.2D1 - t184 * t23 + t186 * t26 / 0.2D1) - 0.180D3 
     #* t33 * t34 * (t174 * t26 - t184 * t26)) * t105 * t51 / 0.1440D4 -
     # ((-0.90D2 * t4 * t203 + 0.180D3 * t33 * t46) * (-t212 / 0.2D1 + t
     #216 / 0.2D1) - 0.90D2 * t4 * t220 * (-t216 * t215 / 0.6D1 + t212 *
     # t211 / 0.6D1) + (-0.90D2 * t4 * t228 + 0.180D3 * t33 * t34 * t23 
     #- t47) * (t211 - t215)) * t105 / 0.1440D4 + (t45 + 0.180D3 * t242 
     #* lh + 0.45D2 * t246) * t3 * t228 / 0.1440D4 + (t78 - t242 * t44 -
     # 0.90D2 * t246 * lh - 0.15D2 * t256) * t3 * t203 / 0.1440D4 + (pi 
     #* (t262 + 0.60D2 * t263 + 0.480D3 * lh * zeta3 - 0.60D2 * t42 * t4
     #0) - t242 * t77 + t246 * t44 / 0.2D1 + 0.30D2 * t256 * lh + 0.15D2
     # / 0.4D1 * t276 * pi) * t3 * t220 / 0.1440D4
      t284 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t283)
      t286 = t2 * x4
      t287 = -0.1D1 + x4
      t288 = t2 * t287
      t289 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t286, 0.
     #0D0, -t288, 0.0D0)
      t290 = t13 * t15
      t291 = t142 * t287
      t294 = log(-0.4D1 * t290 * t291)
      t295 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t286, 0.
     #0D0, -t288, 0.0D0)
      t297 = t294 ** 2
      t298 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t286, 0.
     #0D0, -t288, 0.0D0)
      t310 = t34 * t298
      t316 = t91 * t111
      t317 = x4 * t287
      t318 = t180 * t317
      t321 = log(0.4D1 * t316 * t318)
      t326 = log(-0.4D1 * t92 * t18 * t317)
      t335 = log(-0.4D1 * t111 * t291)
      t341 = t335 ** 2
      t364 = log(-0.4D1 * t179 * t291)
      t366 = t364 ** 2
      t371 = log(0.4D1 * t179 * t318)
      t373 = t371 ** 2
      t390 = -(0.90D2 * t4 * t7 * (t289 - t294 * t295 + t297 * t298 / 0.
     #2D1) - 0.180D3 * t33 * t34 * (t295 - t294 * t298) + t45 * t310) * 
     #t49 * t51 / 0.720D3 - t90 * (t321 * t298 - t326 * t298) * t105 * t
     #107 / 0.8D1 + (-t45 * t34 * (t295 - t335 * t298) - 0.90D2 * t4 * t
     #7 * (-t335 * t289 + t341 * t295 / 0.2D1 - t341 * t335 * t298 / 0.6
     #D1) - t78 * t310 + 0.180D3 * t33 * t34 * (t289 - t335 * t295 + t34
     #1 * t298 / 0.2D1)) * t51 / 0.1440D4 - (0.90D2 * t4 * t7 * (-t364 *
     # t295 + t366 * t298 / 0.2D1 + t371 * t295 - t373 * t298 / 0.2D1) -
     # 0.180D3 * t33 * t34 * (-t364 * t298 + t371 * t298)) * t105 * t51 
     #/ 0.1440D4
      t391 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t286, -t288, 0.0D0, t390)
      t393 = t2 * x1
      t394 = -0.1D1 + x1
      t395 = t2 * t394
      t396 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #393, -t395, 0.0D0)
      t397 = t394 ** 2
      t398 = t17 * t397
      t402 = log(0.4D1 * t290 * t398 * x4)
      t403 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #393, -t395, 0.0D0)
      t405 = t402 ** 2
      t406 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #393, -t395, 0.0D0)
      t418 = t34 * t406
      t419 = t45 * t418
      t423 = t18 * t397
      t426 = log(0.4D1 * t13 * t423)
      t432 = t426 ** 2
      t452 = t397 * x4
      t456 = log(0.4D1 * t92 * t18 * t452)
      t469 = log(0.4D1 * t92 * t423)
      t471 = t469 ** 2
      t487 = -(0.90D2 * t4 * t7 * (t396 - t402 * t403 + t405 * t406 / 0.
     #2D1) - 0.180D3 * t33 * t34 * (t403 - t402 * t406) + t419) * t49 * 
     #t51 / 0.720D3 + (-t45 * t34 * (t403 - t426 * t406) - 0.90D2 * t4 *
     # t7 * (-t426 * t396 + t432 * t403 / 0.2D1 - t432 * t426 * t406 / 0
     #.6D1) - t78 * t418 + 0.180D3 * t33 * t34 * (t396 - t426 * t403 + t
     #432 * t406 / 0.2D1)) * t49 / 0.720D3 - (0.90D2 * t4 * t7 * (t403 -
     # t456 * t406) - 0.180D3 * t33 * t418) * t105 * t107 / 0.720D3 - (0
     #.90D2 * t4 * t7 * (t396 - t469 * t403 + t471 * t406 / 0.2D1) - 0.1
     #80D3 * t33 * t34 * (t403 - t469 * t406) + t419) * t105 * t49 / 0.7
     #20D3
      t488 = FJET(XB1, XB2, s, 0.0D0, t393, 0.0D0, -t395, 0.0D0, t487)
      t490 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t491 = s * t490
      t492 = t1 * x1
      t493 = t491 * t492
      t494 = t1 * t394
      t495 = t494 * x4
      t496 = t491 * t495
      t497 = t494 * t287
      t498 = t491 * t497
      t499 = t490 ** 2
      t502 = x1 * t394
      t504 = s * t499 * t16 * t502 * x4
      t506 = 0.1D1 / (-0.2D1 + t490)
      t507 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t496, t
     #493, t498, -t504)
      t509 = t499 ** 2
      t514 = log(-0.4D1 * t54 * t452 * t287 * t509)
      t515 = t514 * t506
      t516 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t496, t
     #493, t498, -t504)
      t518 = t514 ** 2
      t520 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t496, t
     #493, t498, -t504)
      t527 = t506 * t516
      t533 = t45 * t3
      t535 = t7 * t506 * t520
      t544 = log(-0.4D1 * t316 * t398 * t317 * t509)
      t551 = t33 * t3
      t558 = -(-0.90D2 * t4 * t7 * (-t506 * t507 + t516 * t515 - t518 * 
     #t506 * t520 / 0.2D1) + 0.180D3 * t33 * t34 * (-t527 + t515 * t520)
     # + t533 * t535) * t49 * t51 / 0.720D3 - (0.90D2 * t4 * t7 * (t527 
     #- t544 * t506 * t520) - 0.180D3 * t551 * t535) * t105 * t107 / 0.7
     #20D3
      t559 = FJET(XB1, XB2, s, 0.0D0, t493, -t496, t498, -t504, t558)
      t561 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t562 = s * t561
      t563 = t492 * x3
      t564 = t562 * t563
      t565 = t492 * t93
      t566 = t562 * t565
      t567 = t562 * t494
      t568 = t561 ** 2
      t572 = s * t568 * t16 * t502 * x3
      t574 = 0.1D1 / (-0.2D1 + t561)
      t575 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t564, 0.0D0, -t
     #566, -t567, -t572)
      t576 = t574 * t575
      t577 = t568 ** 2
      t582 = log(-0.4D1 * t316 * t398 * t94 * t577)
      t584 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t564, 0.0D0, -t
     #566, -t567, -t572)
      t591 = t7 * t574 * t584
      t597 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, t564, 0.0D0, -t
     #566, -t567, -t572)
      t603 = log(-0.4D1 * t316 * t398 * t93 * t577)
      t604 = t603 * t574
      t606 = t603 ** 2
      t624 = -(0.90D2 * t4 * t7 * (t576 - t582 * t574 * t584) - 0.180D3 
     #* t551 * t591) * t105 * t107 / 0.720D3 - (0.90D2 * t4 * t7 * (t574
     # * t597 - t604 * t575 + t606 * t574 * t584 / 0.2D1) - 0.180D3 * t3
     #3 * t34 * (t576 - t604 * t584) + t533 * t591) * t105 * t49 / 0.720
     #D3
      t625 = FJET(XB1, XB2, s, t564, -t566, 0.0D0, -t567, -t572, t624)
      t627 = KAPPA2(x1, x2, x3, x4, z)
      t628 = s * t627
      t629 = t628 * t563
      t630 = t628 * t565
      t631 = t628 * t495
      t632 = t628 * t497
      t633 = t627 ** 2
      t638 = cos(t10)
      t641 = Sqrt(x3 * t93 * t317)
      t646 = s * t633 * t16 * t502 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t638 * t641)
      t648 = 0.1D1 / (-0.2D1 + t627)
      t649 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t629, -t631, -t
     #630, t632, t646)
      t652 = t633 ** 2
      t657 = log(0.4D1 * t113 * t397 * t93 * t317 * t652)
      t659 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t629, -t631, -t
     #630, t632, t646)
      t669 = -0.90D2 * t4 * t7 * (t648 * t649 - t657 * t648 * t659) + 0.
     #180D3 * t551 * t7 * t648 * t659
      t673 = FJET(XB1, XB2, s, t629, -t630, -t631, t632, t646, -t669 * t
     #105 * t107 / 0.720D3)
      rrqq2qqht3s8e1 = t284 * t283 + t391 * t390 + t488 * t487 + t559 * 
     #t558 + t625 * t624 - t673 * t669 * t105 * t49 * t51 / 0.720D3

      end function



      doubleprecision function rrqq2qqht3s8e0
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t16 * x4
      t20 = log(0.4D1 * t14 * t17)
      t21 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t23 = t20 ** 2
      t24 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t31 = pi * lh
      t32 = t3 * t7
      t38 = pi ** 2
      t40 = lh ** 2
      t42 = -0.30D2 * t38 + 0.180D3 * t40
      t43 = pi * t42
      t44 = t32 * t24
      t45 = t43 * t44
      t47 = 0.1D1 / x4
      t51 = t14 * t16
      t53 = log(0.4D1 * t51)
      t54 = t53 * pi
      t63 = t53 ** 2
      t64 = t63 * pi
      t68 = t7 * t21
      t86 = t7 * t24
      t89 = x1 ** 2
      t90 = t89 * t11
      t91 = t13 * t16
      t92 = t91 * x4
      t95 = log(0.4D1 * t90 * t92)
      t102 = 0.180D3 * t31 * t44
      t104 = 0.1D1 / x1
      t108 = t4 * t7
      t109 = x3 * t89
      t112 = log(0.4D1 * t109 * t51)
      t114 = t109 * t11
      t115 = -0.1D1 + x3
      t116 = t91 * t115
      t119 = log(-0.4D1 * t114 * t116)
      t122 = 0.1D1 / x3
      t127 = t90 * t91
      t129 = log(0.4D1 * t127)
      t131 = t129 ** 2
      t146 = x3 * t11
      t149 = log(0.4D1 * t146 * t92)
      t151 = t146 * t13
      t152 = t16 * t115
      t156 = log(-0.4D1 * t151 * t152 * x4)
      t165 = log(-0.4D1 * t146 * t116)
      t166 = t165 ** 2
      t169 = log(0.4D1 * t146 * t91)
      t170 = t169 ** 2
      t184 = (0.90D2 * t4 * t7 * (t8 - t20 * t21 + t23 * t24 / 0.2D1) - 
     #0.180D3 * t31 * t32 * (t21 - t20 * t24) + t45) * t47 / 0.1440D4 + 
     #(-0.180D3 * t31 - 0.90D2 * t54) * t3 * t7 * t8 / 0.1440D4 + (t43 +
     # 0.180D3 * t54 * lh + 0.45D2 * t64) * t3 * t68 / 0.1440D4 + (pi * 
     #(-0.240D3 * zeta3 - 0.120D3 * lh * t40 + 0.60D2 * lh * t38) - t54 
     #* t42 - 0.90D2 * t64 * lh - 0.15D2 * t63 * t53 * pi) * t3 * t86 / 
     #0.1440D4 - (0.90D2 * t4 * t7 * (-t21 + t95 * t24) + t102) * t104 *
     # t47 / 0.720D3 - t108 * (t112 * t24 - t119 * t24) * t122 * t104 / 
     #0.8D1 + (0.90D2 * t4 * t7 * (t8 - t129 * t21 + t131 * t24 / 0.2D1)
     # - 0.180D3 * t31 * t32 * (t21 - t129 * t24) + t45) * t104 / 0.720D
     #3 - t108 * (t149 * t24 - t156 * t24) * t122 * t47 / 0.16D2 - (-0.9
     #0D2 * t4 * t86 * (-t166 / 0.2D1 + t170 / 0.2D1) + (-0.90D2 * t4 * 
     #t68 + t102) * (t165 - t169)) * t122 / 0.1440D4
      t185 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t184)
      t187 = t2 * x4
      t188 = -0.1D1 + x4
      t189 = t2 * t188
      t190 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t187, 0.
     #0D0, -t189, 0.0D0)
      t191 = t90 * t13
      t192 = t17 * t188
      t195 = log(-0.4D1 * t191 * t192)
      t196 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t187, 0.
     #0D0, -t189, 0.0D0)
      t202 = t32 * t196
      t211 = log(-0.4D1 * t151 * t192)
      t213 = x4 * t188
      t217 = log(0.4D1 * t151 * t152 * t213)
      t224 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t187, 0.
     #0D0, -t189, 0.0D0)
      t227 = log(-0.4D1 * t14 * t192)
      t229 = t227 ** 2
      t245 = -(0.90D2 * t4 * t7 * (t190 - t195 * t196) - 0.180D3 * t31 *
     # t202) * t104 * t47 / 0.720D3 - t108 * (-t211 * t196 + t217 * t196
     #) * t122 * t47 / 0.16D2 + (-0.90D2 * t4 * t7 * (t224 - t227 * t190
     # + t229 * t196 / 0.2D1) + 0.180D3 * t31 * t32 * (t190 - t227 * t19
     #6) - t43 * t202) * t47 / 0.1440D4
      t246 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t187, -t189, 0.0D0, t245)
      t248 = t2 * x1
      t249 = -0.1D1 + x1
      t250 = t2 * t249
      t251 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #248, -t250, 0.0D0)
      t252 = t249 ** 2
      t253 = t16 * t252
      t257 = log(0.4D1 * t191 * t253 * x4)
      t258 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #248, -t250, 0.0D0)
      t264 = t32 * t258
      t266 = 0.180D3 * t31 * t264
      t272 = t104 * t47
      t276 = t91 * t252
      t279 = log(0.4D1 * t114 * t276)
      t289 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #248, -t250, 0.0D0)
      t292 = log(0.4D1 * t90 * t276)
      t294 = t292 ** 2
      t310 = -(0.90D2 * t4 * t7 * (t251 - t257 * t258) - t266) * t104 * 
     #t47 / 0.720D3 - t108 * t258 * t122 * t272 / 0.8D1 - (0.90D2 * t4 *
     # t7 * (t251 - t279 * t258) - t266) * t122 * t104 / 0.720D3 + (-0.9
     #0D2 * t4 * t7 * (t289 - t292 * t251 + t294 * t258 / 0.2D1) + 0.180
     #D3 * t31 * t32 * (t251 - t292 * t258) - t43 * t264) * t104 / 0.720
     #D3
      t311 = FJET(XB1, XB2, s, 0.0D0, t248, 0.0D0, -t250, 0.0D0, t310)
      t313 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t314 = s * t313
      t315 = t1 * x1
      t316 = t314 * t315
      t317 = t1 * t249
      t318 = t317 * x4
      t319 = t314 * t318
      t320 = t317 * t188
      t321 = t314 * t320
      t322 = t313 ** 2
      t325 = x1 * t249
      t327 = s * t322 * t15 * t325 * x4
      t329 = 0.1D1 / (-0.2D1 + t313)
      t330 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t319, t
     #316, t321, -t327)
      t333 = t322 ** 2
      t338 = log(-0.4D1 * t127 * t252 * x4 * t188 * t333)
      t340 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t319, t
     #316, t321, -t327)
      t346 = t31 * t3
      t347 = t7 * t329
      t360 = -(-0.90D2 * t4 * t7 * (-t329 * t330 + t338 * t329 * t340) -
     # 0.180D3 * t346 * t347 * t340) * t104 * t47 / 0.720D3 - t4 * t347 
     #* t340 * t122 * t272 / 0.8D1
      t361 = FJET(XB1, XB2, s, 0.0D0, t316, -t319, t321, -t327, t360)
      t363 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t364 = s * t363
      t365 = t315 * x3
      t366 = t364 * t365
      t367 = t315 * t115
      t368 = t364 * t367
      t369 = t364 * t317
      t370 = t363 ** 2
      t374 = s * t370 * t15 * t325 * x3
      t376 = 0.1D1 / (-0.2D1 + t363)
      t377 = t7 * t376
      t379 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t366, 0.0D0, -t
     #368, -t369, -t374)
      t384 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, t366, 0.0D0, -t
     #368, -t369, -t374)
      t387 = t370 ** 2
      t392 = log(-0.4D1 * t109 * t14 * t253 * t115 * t387)
      t406 = -t4 * t377 * t379 * t122 * t272 / 0.8D1 - (0.90D2 * t4 * t7
     # * (t376 * t384 - t392 * t376 * t379) - 0.180D3 * t346 * t377 * t3
     #79) * t122 * t104 / 0.720D3
      t407 = FJET(XB1, XB2, s, t366, -t368, 0.0D0, -t369, -t374, t406)
      t409 = KAPPA2(x1, x2, x3, x4, z)
      t410 = s * t409
      t411 = t410 * t365
      t412 = t410 * t367
      t413 = t410 * t318
      t414 = t410 * t320
      t415 = t409 ** 2
      t420 = cos(t9)
      t423 = Sqrt(x3 * t115 * t213)
      t428 = s * t415 * t15 * t325 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t420 * t423)
      t430 = 0.1D1 / (-0.2D1 + t409)
      t433 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t411, -t413, -t
     #412, t414, t428)
      t438 = FJET(XB1, XB2, s, t411, -t412, -t413, t414, t428, t4 * t7 *
     # t430 * t433 * t122 * t272 / 0.8D1)
      rrqq2qqht3s8e0 = t185 * t184 + t246 * t245 + t311 * t310 + t361 * 
     #t360 + t407 * t406 + t438 * pi * t32 * t430 * t433 * t122 * t104 *
     # t47 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s8em1
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t10 = sin(x2 * pi)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t16 * x4
      t20 = log(0.4D1 * t14 * t17)
      t21 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t27 = pi * lh
      t28 = t3 * t7
      t31 = 0.180D3 * t27 * t28 * t21
      t33 = 0.1D1 / x4
      t36 = x1 ** 2
      t37 = t36 * t11
      t38 = t13 * t16
      t41 = log(0.4D1 * t37 * t38)
      t48 = 0.1D1 / x1
      t51 = t4 * t7
      t56 = x3 * t11
      t57 = -0.1D1 + x3
      t61 = log(-0.4D1 * t56 * t38 * t57)
      t64 = log(0.4D1 * t56 * t38)
      t67 = 0.1D1 / x3
      t71 = rrqq2qqh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t78 = log(0.4D1 * t14 * t16)
      t79 = t78 * pi
      t86 = pi ** 2
      t88 = lh ** 2
      t94 = t78 ** 2
      t102 = (0.90D2 * t4 * t7 * (t8 - t20 * t21) - t31) * t33 / 0.1440D
     #4 + (0.90D2 * t4 * t7 * (t8 - t41 * t21) - t31) * t48 / 0.720D3 + 
     #t51 * t21 * t48 * t33 / 0.8D1 + t51 * t21 * (t61 - t64) * t67 / 0.
     #16D2 + t4 * t7 * t71 / 0.16D2 + (-0.180D3 * t27 - 0.90D2 * t79) * 
     #t3 * t7 * t8 / 0.1440D4 + (pi * (-0.30D2 * t86 + 0.180D3 * t88) + 
     #0.180D3 * t79 * lh + 0.45D2 * t94 * pi) * t3 * t7 * t21 / 0.1440D4
      t103 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t102)
      t105 = t2 * x4
      t106 = -0.1D1 + x4
      t107 = t2 * t106
      t108 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t105, 0.
     #0D0, -t107, 0.0D0)
      t112 = log(-0.4D1 * t14 * t17 * t106)
      t113 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t105, 0.
     #0D0, -t107, 0.0D0)
      t129 = (-0.90D2 * t4 * t7 * (t108 - t112 * t113) + 0.180D3 * t27 *
     # t28 * t113) * t33 / 0.1440D4 - t51 * t113 * t48 * t33 / 0.8D1
      t130 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t105, -t107, 0.0D0, t129)
      t132 = t2 * x1
      t133 = -0.1D1 + x1
      t134 = t2 * t133
      t135 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #132, -t134, 0.0D0)
      t140 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #132, -t134, 0.0D0)
      t141 = t133 ** 2
      t145 = log(0.4D1 * t37 * t38 * t141)
      t161 = -t51 * t135 * t67 * t48 / 0.8D1 + (-0.90D2 * t4 * t7 * (t14
     #0 - t145 * t135) + 0.180D3 * t27 * t28 * t135) * t48 / 0.720D3 - t
     #51 * t135 * t48 * t33 / 0.8D1
      t162 = FJET(XB1, XB2, s, 0.0D0, t132, 0.0D0, -t134, 0.0D0, t161)
      t164 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t165 = s * t164
      t166 = t1 * x1
      t167 = t165 * t166
      t168 = t1 * t133
      t170 = t165 * t168 * x4
      t172 = t165 * t168 * t106
      t173 = t164 ** 2
      t176 = x1 * t133
      t178 = s * t173 * t15 * t176 * x4
      t181 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t170, t
     #167, t172, -t178)
      t184 = 0.1D1 / (-0.2D1 + t164) * t181 * t48 * t33
      t187 = FJET(XB1, XB2, s, 0.0D0, t167, -t170, t172, -t178, -t51 * t
     #184 / 0.8D1)
      t192 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t193 = s * t192
      t195 = t193 * t166 * x3
      t197 = t193 * t166 * t57
      t198 = t193 * t168
      t199 = t192 ** 2
      t203 = s * t199 * t15 * t176 * x3
      t206 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, t195, 0.0D0, -t
     #197, -t198, -t203)
      t209 = 0.1D1 / (-0.2D1 + t192) * t206 * t67 * t48
      t212 = FJET(XB1, XB2, s, t195, -t197, 0.0D0, -t198, -t203, -t51 * 
     #t209 / 0.8D1)
      rrqq2qqht3s8em1 = t103 * t102 + t130 * t129 + t162 * t161 - t187 *
     # pi * t28 * t184 / 0.8D1 - t212 * pi * t28 * t209 / 0.8D1

      end function



      doubleprecision function rrqq2qqht3s8em2
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t4 = pi * t3
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x1
      t14 = 0.1D1 / x4
      t18 = rrqq2qqh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t25 = sin(x2 * pi)
      t26 = t25 ** 2
      t27 = z ** 2
      t30 = t1 ** 2
      t31 = t30 ** 2
      t34 = log(0.4D1 * t26 / t27 * t31)
      t41 = t4 * t9 * t10 / 0.8D1 + t4 * t9 * t14 / 0.16D2 + t4 * t7 * t
     #18 / 0.16D2 + (-0.180D3 * pi * lh - 0.90D2 * t34 * pi) * t3 * t9 /
     # 0.1440D4
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t41)
      t44 = t2 * x1
      t46 = t2 * (-0.1D1 + x1)
      t47 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t4
     #4, -t46, 0.0D0)
      t49 = t7 * t47 * t10
      t52 = FJET(XB1, XB2, s, 0.0D0, t44, 0.0D0, -t46, 0.0D0, -t4 * t49 
     #/ 0.8D1)
      t57 = t2 * x4
      t59 = t2 * (-0.1D1 + x4)
      t60 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t57, 0.0D
     #0, -t59, 0.0D0)
      t62 = t7 * t60 * t14
      t65 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t57, -t59, 0.0D0, -t4 * t62 
     #/ 0.16D2)
      rrqq2qqht3s8em2 = t42 * t41 - t52 * pi * t3 * t49 / 0.8D1 - t65 * 
     #pi * t3 * t62 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s8em3
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = z - 0.1D1
      t2 = s * t1
      t3 = 0.1D1 / t1
      t5 = s ** 2
      t7 = 0.1D1 / t5 / s
      t8 = rrqq2qqh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, pi * t3 * 
     #t7 * t8 / 0.16D2)
      rrqq2qqht3s8em3 = t12 * pi * t3 * t7 * t8 / 0.16D2

      end function



      doubleprecision function rrqq2qqht3s8em4
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
      doubleprecision rrqq2qqh31J1
      doubleprecision rrqq2qqh31J2
      doubleprecision rrqq2qqh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqq2qqht3s8em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqq2qqh31J1
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
      t1 = S12 ** 2
      t3 = S34 ** 2
      rrqq2qqh31J1 = (-0.16D2 / 0.27D2 * S34 * t1 - 0.16D2 / 0.27D2 * t3
     # * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqq2qqh31J2
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
      t1 = S12 ** 2
      t4 = S34 ** 2
      rrqq2qqh31J2 = (0.16D2 / 0.27D2 * S34 * t1 + 0.32D2 / 0.27D2 * t4 
     #* S12 + 0.16D2 / 0.27D2 * t4 * S34 + (-0.16D2 / 0.27D2 * S23 * S24
     # - 0.16D2 / 0.27D2 * S14 * S24 - 0.16D2 / 0.27D2 * S23 * S13 - 0.1
     #6D2 / 0.27D2 * S14 * S13) * S34) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqq2qqh31J3
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
      t1 = S12 ** 2
      t4 = S34 ** 2
      t7 = 0.16D2 / 0.27D2 * S23 + 0.16D2 / 0.27D2 * S24 + 0.16D2 / 0.27
     #D2 * S13 + 0.16D2 / 0.27D2 * S14
      rrqq2qqh31J3 = (0.16D2 / 0.27D2 * S34 * t1 + (0.32D2 / 0.27D2 * t4
     # + t7 * S34) * S12 + 0.16D2 / 0.27D2 * t4 * S34 + t4 * t7 + (0.16D
     #2 / 0.27D2 * S23 * S13 + 0.16D2 / 0.27D2 * S23 * S24 + 0.16D2 / 0.
     #27D2 * S14 * S13 + 0.16D2 / 0.27D2 * S14 * S24) * S34) / pi * wd /
     # z

      end function
  
 