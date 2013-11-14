  
      subroutine rrgg2gght5
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2ggh51J1  
      doubleprecision rrgg2ggh51J2  
      doubleprecision rrgg2ggh51J3  
      doubleprecision rrgg2ggh51J4  
      doubleprecision rrgg2ggh51J5  
      doubleprecision rrgg2ggh51J6  
      doubleprecision rrgg2gght5s1e1  
      doubleprecision rrgg2gght5s1e0  
      doubleprecision rrgg2gght5s1em1  
      doubleprecision rrgg2gght5s1em2  
      doubleprecision rrgg2gght5s1em3  
      doubleprecision rrgg2gght5s1em4  
      doubleprecision rrgg2gght5s2e1  
      doubleprecision rrgg2gght5s2e0  
      doubleprecision rrgg2gght5s2em1  
      doubleprecision rrgg2gght5s2em2  
      doubleprecision rrgg2gght5s2em3  
      doubleprecision rrgg2gght5s2em4  
      doubleprecision rrgg2gght5s3e1  
      doubleprecision rrgg2gght5s3e0  
      doubleprecision rrgg2gght5s3em1  
      doubleprecision rrgg2gght5s3em2  
      doubleprecision rrgg2gght5s3em3  
      doubleprecision rrgg2gght5s3em4  
      doubleprecision rrgg2gght5s4e1  
      doubleprecision rrgg2gght5s4e0  
      doubleprecision rrgg2gght5s4em1  
      doubleprecision rrgg2gght5s4em2  
      doubleprecision rrgg2gght5s4em3  
      doubleprecision rrgg2gght5s4em4  
      doubleprecision rrgg2gght5s5e1  
      doubleprecision rrgg2gght5s5e0  
      doubleprecision rrgg2gght5s5em1  
      doubleprecision rrgg2gght5s5em2  
      doubleprecision rrgg2gght5s5em3  
      doubleprecision rrgg2gght5s5em4  
      doubleprecision rrgg2gght5s6e1  
      doubleprecision rrgg2gght5s6e0  
      doubleprecision rrgg2gght5s6em1  
      doubleprecision rrgg2gght5s6em2  
      doubleprecision rrgg2gght5s6em3  
      doubleprecision rrgg2gght5s6em4  
      doubleprecision rrgg2gght5s7e1  
      doubleprecision rrgg2gght5s7e0  
      doubleprecision rrgg2gght5s7em1  
      doubleprecision rrgg2gght5s7em2  
      doubleprecision rrgg2gght5s7em3  
      doubleprecision rrgg2gght5s7em4  
      doubleprecision rrgg2gght5s8e1  
      doubleprecision rrgg2gght5s8e0  
      doubleprecision rrgg2gght5s8em1  
      doubleprecision rrgg2gght5s8em2  
      doubleprecision rrgg2gght5s8em3  
      doubleprecision rrgg2gght5s8em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght5s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght5s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght5s3e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght5s4e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgg2gght5s5e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgg2gght5s6e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgg2gght5s7e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgg2gght5s8e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght5s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght5s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght5s3e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght5s4e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgg2gght5s5e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgg2gght5s6e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgg2gght5s7e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgg2gght5s8e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght5s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght5s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght5s3em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght5s4em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgg2gght5s5em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgg2gght5s6em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgg2gght5s7em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgg2gght5s8em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght5s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght5s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght5s3em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght5s4em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgg2gght5s5em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgg2gght5s6em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgg2gght5s7em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgg2gght5s8em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght5s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght5s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght5s3em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght5s4em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgg2gght5s5em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgg2gght5s6em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgg2gght5s7em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgg2gght5s8em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght5s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght5s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.3)then  
         fff=rrgg2gght5s3em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.4)then  
         fff=rrgg2gght5s4em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.5)then  
         fff=rrgg2gght5s5em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.6)then  
         fff=rrgg2gght5s6em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.7)then  
         fff=rrgg2gght5s7em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.8)then  
         fff=rrgg2gght5s8em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght5s1e1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x1 ** 2
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t20 = t17 ** 2
      t21 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t24 = -0.1D1 + x4
      t25 = t14 * t24
      t28 = log(-0.4D1 * t11 * t25)
      t30 = t28 ** 2
      t36 = pi * lh
      t44 = 0.1D1 / x1
      t46 = 0.1D1 / x4
      t49 = pi ** 2
      t51 = lh ** 2
      t53 = -0.30D2 * t49 + 0.180D3 * t51
      t54 = pi * t53
      t55 = t11 * t13
      t57 = log(0.4D1 * t55)
      t62 = t57 ** 2
      t65 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t69 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t79 = -0.240D3 * zeta3 - 0.120D3 * t51 * lh + 0.60D2 * lh * t49
      t80 = pi * t79
      t81 = t5 * t21
      t82 = t80 * t81
      t93 = x3 * t7
      t94 = t93 * t10
      t97 = log(-0.4D1 * t94 * t25)
      t99 = t10 * t13
      t100 = t99 * x4
      t103 = log(0.4D1 * t93 * t100)
      t107 = 0.1D1 / x3
      t109 = t107 * t44 * t46
      t112 = t93 * t99
      t114 = log(0.4D1 * t112)
      t116 = t114 ** 2
      t127 = t54 * t81
      t138 = log(0.4D1 * t100)
      t139 = t138 ** 2
      t140 = x4 * t24
      t143 = log(-0.4D1 * t99 * t140)
      t144 = t143 ** 2
      t167 = log(0.4D1 * t99)
      t168 = t167 * pi
      t170 = t167 ** 2
      t171 = t170 * pi
      t175 = t170 * t167 * pi
      t194 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t197 = t49 ** 2
      t198 = t51 ** 2
      t211 = t170 ** 2
      t218 = x3 * t10
      t221 = log(0.4D1 * t218 * t14)
      t223 = t221 ** 2
      t228 = log(-0.4D1 * t218 * t25)
      t230 = t228 ** 2
      t246 = t218 * t13
      t248 = log(0.4D1 * t246)
      t253 = t248 ** 2
      t273 = (0.90D2 * t6 * (t17 * t18 - t20 * t21 / 0.2D1 - t28 * t18 +
     # t30 * t21 / 0.2D1) - 0.180D3 * t36 * t5 * (t17 * t21 - t28 * t21)
     #) * t44 * t46 / 0.720D3 - (t54 * t5 * (t18 - t57 * t21) + 0.90D2 *
     # t6 * (t62 * t18 / 0.2D1 + t65 - t62 * t57 * t21 / 0.6D1 - t57 * t
     #69) + t82 - 0.180D3 * t36 * t5 * (-t57 * t18 + t62 * t21 / 0.2D1 +
     # t69)) * t44 / 0.720D3 - t6 * (t97 * t21 - t103 * t21) * t109 / 0.
     #8D1 + (0.90D2 * t6 * (t114 * t18 - t116 * t21 / 0.2D1 - t69) - 0.1
     #80D3 * t36 * t5 * (-t18 + t114 * t21) - t127) * t107 * t44 / 0.720
     #D3 + ((-0.90D2 * t6 * t18 + 0.180D3 * t36 * t81) * (t139 / 0.2D1 -
     # t144 / 0.2D1) - 0.90D2 * t6 * t21 * (t144 * t143 / 0.6D1 - t139 *
     # t138 / 0.6D1) + (0.180D3 * t36 * t5 * t18 - t127 - 0.90D2 * t6 * 
     #t69) * (-t138 + t143)) * t46 / 0.1440D4 - (t80 - t168 * t53 - 0.90
     #D2 * t171 * lh - 0.15D2 * t175) * t5 * t18 / 0.1440D4 - (t54 + 0.1
     #80D3 * t168 * lh + 0.45D2 * t171) * t5 * t69 / 0.1440D4 - (-0.180D
     #3 * t36 - 0.90D2 * t168) * t5 * t65 / 0.1440D4 - t6 * t194 / 0.16D
     #2 - (pi * (t197 + 0.60D2 * t198 + 0.480D3 * lh * zeta3 - 0.60D2 * 
     #t51 * t49) - t168 * t79 + t171 * t53 / 0.2D1 + 0.30D2 * t175 * lh 
     #+ 0.15D2 / 0.4D1 * t211 * pi) * t5 * t21 / 0.1440D4 + (0.90D2 * t6
     # * (t221 * t18 - t223 * t21 / 0.2D1 - t228 * t18 + t230 * t21 / 0.
     #2D1) - 0.180D3 * t36 * t5 * (t221 * t21 - t228 * t21)) * t107 * t4
     #6 / 0.1440D4 - (t54 * t5 * (t18 - t248 * t21) + 0.90D2 * t6 * (t25
     #3 * t18 / 0.2D1 + t65 - t253 * t248 * t21 / 0.6D1 - t248 * t69) + 
     #t82 - 0.180D3 * t36 * t5 * (-t248 * t18 + t253 * t21 / 0.2D1 + t69
     #)) * t107 / 0.1440D4
      t274 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t273)
      t276 = 0.1D1 - x1
      t277 = 0.1D1 - x3
      t278 = KAPPA2(t276, x2, t277, 0.10D1, z)
      t279 = s * t278
      t280 = -t276
      t281 = t1 * t280
      t282 = -t277
      t283 = t281 * t282
      t284 = t279 * t283
      t285 = t281 * x3
      t286 = t279 * t285
      t287 = t1 * x1
      t288 = t279 * t287
      t289 = t278 ** 2
      t291 = t1 ** 2
      t293 = t280 * x1
      t295 = s * t289 * t291 * t293 * x3
      t297 = 0.1D1 / (-0.2D1 + t278)
      t298 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t284, t288, -t2
     #86, 0.0D0, -t295)
      t299 = t297 * t298
      t300 = t280 ** 2
      t301 = t300 * t282
      t302 = t289 ** 2
      t307 = log(-0.4D1 * t112 * t301 * x4 * t302)
      t309 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t284, t288, -t2
     #86, 0.0D0, -t295)
      t315 = t5 * t297 * t309
      t320 = t44 * t46
      t327 = log(-0.4D1 * t94 * t13 * t300 * t282 * t302)
      t328 = t327 * t297
      t330 = t327 ** 2
      t334 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t284, t288, -t2
     #86, 0.0D0, -t295)
      t349 = -(0.90D2 * t6 * (-t299 + t307 * t297 * t309) + 0.180D3 * t3
     #6 * t315) * t107 * t320 / 0.720D3 + (0.90D2 * t6 * (-t328 * t298 +
     # t330 * t297 * t309 / 0.2D1 + t297 * t334) - 0.180D3 * t36 * t5 * 
     #(t299 - t328 * t309) + t54 * t315) * t107 * t44 / 0.720D3
      t350 = FJET(XB1, XB2, s, t284, -t286, t288, 0.0D0, -t295, t349)
      t352 = -t24
      t353 = KAPPA2(t276, x2, t277, t352, z)
      t354 = s * t353
      t355 = t354 * t283
      t356 = t354 * t285
      t357 = t287 * t24
      t358 = t354 * t357
      t359 = t287 * x4
      t360 = t354 * t359
      t361 = t353 ** 2
      t366 = cos(t8)
      t369 = Sqrt(x3 * t282 * t140)
      t374 = s * t361 * t291 * t293 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D
     #1 * t366 * t369)
      t376 = 0.1D1 / (-0.2D1 + t353)
      t377 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t355, -t358, -t
     #356, t360, t374)
      t379 = t361 ** 2
      t384 = log(0.4D1 * t112 * t301 * t140 * t379)
      t386 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t355, -t358, -t
     #356, t360, t374)
      t395 = 0.90D2 * t6 * (t376 * t377 - t384 * t376 * t386) - 0.180D3 
     #* t36 * t5 * t376 * t386
      t399 = FJET(XB1, XB2, s, t355, -t356, -t358, t360, t374, -t395 * t
     #107 * t320 / 0.720D3)
      t403 = t2 * t280
      t404 = t2 * x1
      t405 = t7 * t300
      t406 = t405 * x4
      t409 = log(0.4D1 * t99 * t406)
      t410 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t403, t404, 0.
     #0D0, 0.0D0, 0.0D0)
      t412 = t409 ** 2
      t413 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t403, t404, 0.
     #0D0, 0.0D0, 0.0D0)
      t416 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t403, t404, 0.
     #0D0, 0.0D0, 0.0D0)
      t425 = t5 * t413
      t426 = t54 * t425
      t432 = log(0.4D1 * t99 * t405)
      t437 = t432 ** 2
      t440 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, -t403, t404, 0.
     #0D0, 0.0D0, 0.0D0)
      t460 = log(0.4D1 * t246 * t406)
      t474 = log(0.4D1 * t218 * t13 * t7 * t300)
      t476 = t474 ** 2
      t491 = (0.90D2 * t6 * (-t409 * t410 + t412 * t413 / 0.2D1 + t416) 
     #- 0.180D3 * t36 * t5 * (t410 - t409 * t413) + t426) * t44 * t46 / 
     #0.720D3 - (-t54 * t5 * (t410 - t432 * t413) - 0.90D2 * t6 * (t437 
     #* t410 / 0.2D1 + t440 - t437 * t432 * t413 / 0.6D1 - t432 * t416) 
     #- t80 * t425 + 0.180D3 * t36 * t5 * (-t432 * t410 + t437 * t413 / 
     #0.2D1 + t416)) * t44 / 0.720D3 - (0.90D2 * t6 * (-t410 + t460 * t4
     #13) + 0.180D3 * t36 * t425) * t107 * t320 / 0.720D3 + (0.90D2 * t6
     # * (-t474 * t410 + t476 * t413 / 0.2D1 + t416) - 0.180D3 * t36 * t
     #5 * (t410 - t474 * t413) + t426) * t107 * t44 / 0.720D3
      t492 = FJET(XB1, XB2, s, -t403, 0.0D0, t404, 0.0D0, 0.0D0, t491)
      t494 = t2 * t282
      t495 = t2 * x3
      t496 = t13 * t282
      t497 = t496 * x4
      t500 = log(-0.4D1 * t94 * t497)
      t501 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t494, 0.0D0, t
     #495, 0.0D0, 0.0D0)
      t507 = log(0.4D1 * t94 * t14 * t24 * t282)
      t516 = log(-0.4D1 * t93 * t99 * t282)
      t517 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t494, 0.0D0, t
     #495, 0.0D0, 0.0D0)
      t519 = t516 ** 2
      t522 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t494, 0.0D0, t
     #495, 0.0D0, 0.0D0)
      t531 = t5 * t501
      t539 = log(-0.4D1 * t218 * t497)
      t541 = t539 ** 2
      t547 = log(0.4D1 * t246 * t140 * t282)
      t549 = t547 ** 2
      t567 = log(-0.4D1 * t218 * t496)
      t572 = t567 ** 2
      t575 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, -t494, 0.0D0, t
     #495, 0.0D0, 0.0D0)
      t594 = -t6 * (t500 * t501 - t507 * t501) * t109 / 0.8D1 + (0.90D2 
     #* t6 * (-t516 * t517 + t519 * t501 / 0.2D1 + t522) - 0.180D3 * t36
     # * t5 * (t517 - t516 * t501) + t54 * t531) * t107 * t44 / 0.720D3 
     #+ (0.90D2 * t6 * (-t539 * t517 + t541 * t501 / 0.2D1 + t547 * t517
     # - t549 * t501 / 0.2D1) - 0.180D3 * t36 * t5 * (-t539 * t501 + t54
     #7 * t501)) * t107 * t46 / 0.1440D4 - (-t54 * t5 * (t517 - t567 * t
     #501) - 0.90D2 * t6 * (t572 * t517 / 0.2D1 + t575 - t572 * t567 * t
     #501 / 0.6D1 - t567 * t522) - t80 * t531 + 0.180D3 * t36 * t5 * (-t
     #567 * t517 + t572 * t501 / 0.2D1 + t522)) * t107 / 0.1440D4
      t595 = FJET(XB1, XB2, s, -t494, t495, 0.0D0, 0.0D0, 0.0D0, t594)
      t597 = KAPPA2(t276, x2, 0.10D1, t352, z)
      t598 = s * t597
      t599 = t598 * t281
      t600 = t598 * t357
      t601 = t598 * t359
      t602 = t597 ** 2
      t606 = s * t602 * t291 * t293 * x4
      t608 = t602 ** 2
      t610 = t300 * x4 * t24 * t608
      t613 = log(-0.4D1 * t55 * t610)
      t615 = 0.1D1 / (-0.2D1 + t597)
      t616 = t613 * t615
      t617 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t599, -t600, 0
     #.0D0, t601, -t606)
      t619 = t613 ** 2
      t621 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t599, -t600, 0
     #.0D0, t601, -t606)
      t624 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t599, -t600, 0
     #.0D0, t601, -t606)
      t629 = t615 * t617
      t636 = t5 * t615 * t621
      t643 = log(-0.4D1 * t112 * t610)
      t655 = (0.90D2 * t6 * (-t616 * t617 + t619 * t615 * t621 / 0.2D1 +
     # t615 * t624) - 0.180D3 * t36 * t5 * (t629 - t616 * t621) + t54 * 
     #t636) * t44 * t46 / 0.720D3 - (-0.90D2 * t6 * (t629 - t643 * t615 
     #* t621) + 0.180D3 * t36 * t636) * t107 * t320 / 0.720D3
      t656 = FJET(XB1, XB2, s, -t599, 0.0D0, -t600, t601, -t606, t655)
      rrgg2gght5s1e1 = t274 * t273 + t350 * t349 - t399 * t395 * t109 / 
     #0.720D3 + t492 * t491 + t595 * t594 + t656 * t655

      end function



      doubleprecision function rrgg2gght5s1e0
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x1 ** 2
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t20 = -0.1D1 + x4
      t21 = t14 * t20
      t24 = log(-0.4D1 * t11 * t21)
      t27 = 0.1D1 / x1
      t29 = 0.1D1 / x4
      t33 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t34 = x3 * t7
      t35 = t10 * t13
      t38 = log(0.4D1 * t34 * t35)
      t43 = pi * lh
      t44 = t5 * t18
      t46 = 0.180D3 * t43 * t44
      t48 = 0.1D1 / x3
      t52 = t11 * t13
      t54 = log(0.4D1 * t52)
      t56 = t54 ** 2
      t59 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t68 = pi ** 2
      t70 = lh ** 2
      t72 = -0.30D2 * t68 + 0.180D3 * t70
      t73 = pi * t72
      t74 = t73 * t44
      t80 = log(0.4D1 * t35 * x4)
      t81 = t80 ** 2
      t82 = x4 * t20
      t85 = log(-0.4D1 * t35 * t82)
      t86 = t85 ** 2
      t101 = log(0.4D1 * t35)
      t102 = t101 * pi
      t105 = t101 ** 2
      t106 = t105 * pi
      t112 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t138 = x3 * t10
      t141 = log(0.4D1 * t138 * t14)
      t145 = log(-0.4D1 * t138 * t21)
      t152 = t138 * t13
      t154 = log(0.4D1 * t152)
      t156 = t154 ** 2
      t170 = t6 * (t17 * t18 - t24 * t18) * t27 * t29 / 0.8D1 + (0.90D2 
     #* t6 * (-t33 + t38 * t18) + t46) * t48 * t27 / 0.720D3 - (0.90D2 *
     # t6 * (-t54 * t33 + t56 * t18 / 0.2D1 + t59) - 0.180D3 * t43 * t5 
     #* (t33 - t54 * t18) + t74) * t27 / 0.720D3 + (-0.90D2 * t6 * t18 *
     # (t81 / 0.2D1 - t86 / 0.2D1) + (-0.90D2 * t6 * t33 + t46) * (-t80 
     #+ t85)) * t29 / 0.1440D4 - (t73 + 0.180D3 * t102 * lh + 0.45D2 * t
     #106) * t5 * t33 / 0.1440D4 - t6 * t112 / 0.16D2 - (pi * (-0.240D3 
     #* zeta3 - 0.120D3 * t70 * lh + 0.60D2 * lh * t68) - t102 * t72 - 0
     #.90D2 * t106 * lh - 0.15D2 * t105 * t101 * pi) * t5 * t18 / 0.1440
     #D4 - (-0.180D3 * t43 - 0.90D2 * t102) * t5 * t59 / 0.1440D4 + t6 *
     # (t141 * t18 - t145 * t18) * t48 * t29 / 0.16D2 - (0.90D2 * t6 * (
     #-t154 * t33 + t156 * t18 / 0.2D1 + t59) - 0.180D3 * t43 * t5 * (t3
     #3 - t154 * t18) + t74) * t48 / 0.1440D4
      t171 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t170)
      t173 = 0.1D1 - x1
      t174 = 0.1D1 - x3
      t175 = KAPPA2(t173, x2, t174, 0.10D1, z)
      t176 = s * t175
      t177 = -t173
      t178 = t1 * t177
      t179 = -t174
      t180 = t178 * t179
      t181 = t176 * t180
      t182 = t178 * x3
      t183 = t176 * t182
      t184 = t1 * x1
      t185 = t176 * t184
      t186 = t175 ** 2
      t188 = t1 ** 2
      t190 = t177 * x1
      t192 = s * t186 * t188 * t190 * x3
      t194 = 0.1D1 / (-0.2D1 + t175)
      t196 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t181, t185, -t1
     #83, 0.0D0, -t192)
      t198 = t27 * t29
      t202 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t181, t185, -t1
     #83, 0.0D0, -t192)
      t205 = t177 ** 2
      t207 = t186 ** 2
      t212 = log(-0.4D1 * t34 * t10 * t13 * t205 * t179 * t207)
      t226 = t6 * t194 * t196 * t48 * t198 / 0.8D1 + (0.90D2 * t6 * (t19
     #4 * t202 - t212 * t194 * t196) - 0.180D3 * t43 * t5 * t194 * t196)
     # * t48 * t27 / 0.720D3
      t227 = FJET(XB1, XB2, s, t181, -t183, t185, 0.0D0, -t192, t226)
      t229 = -t20
      t230 = KAPPA2(t173, x2, t174, t229, z)
      t231 = s * t230
      t232 = t231 * t180
      t233 = t231 * t182
      t234 = t184 * t20
      t235 = t231 * t234
      t236 = t184 * x4
      t237 = t231 * t236
      t238 = t230 ** 2
      t243 = cos(t8)
      t246 = Sqrt(x3 * t179 * t82)
      t251 = s * t238 * t188 * t190 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D
     #1 * t243 * t246)
      t253 = 0.1D1 / (-0.2D1 + t230)
      t255 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t232, -t235, -t
     #233, t237, t251)
      t257 = t255 * t48 * t198
      t260 = FJET(XB1, XB2, s, t232, -t233, -t235, t237, t251, -t6 * t25
     #3 * t257 / 0.8D1)
      t266 = t2 * t177
      t267 = t2 * x1
      t268 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t266, t267, 0.
     #0D0, 0.0D0, 0.0D0)
      t269 = t7 * t205
      t273 = log(0.4D1 * t35 * t269 * x4)
      t274 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t266, t267, 0.
     #0D0, 0.0D0, 0.0D0)
      t279 = t5 * t274
      t281 = 0.180D3 * t43 * t279
      t295 = log(0.4D1 * t138 * t13 * t7 * t205)
      t306 = log(0.4D1 * t35 * t269)
      t308 = t306 ** 2
      t311 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t266, t267, 0.
     #0D0, 0.0D0, 0.0D0)
      t324 = (0.90D2 * t6 * (t268 - t274 * t273) - t281) * t27 * t29 / 0
     #.720D3 + t6 * t274 * t48 * t27 * t29 / 0.8D1 + (0.90D2 * t6 * (t26
     #8 - t295 * t274) - t281) * t48 * t27 / 0.720D3 - (-0.90D2 * t6 * (
     #-t306 * t268 + t308 * t274 / 0.2D1 + t311) + 0.180D3 * t43 * t5 * 
     #(t268 - t306 * t274) - t73 * t279) * t27 / 0.720D3
      t325 = FJET(XB1, XB2, s, -t266, 0.0D0, t267, 0.0D0, 0.0D0, t324)
      t327 = t2 * t179
      t328 = t2 * x3
      t329 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t327, 0.0D0, t
     #328, 0.0D0, 0.0D0)
      t333 = log(-0.4D1 * t34 * t35 * t179)
      t334 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t327, 0.0D0, t
     #328, 0.0D0, 0.0D0)
      t339 = t5 * t334
      t346 = t13 * t179
      t350 = log(-0.4D1 * t138 * t346 * x4)
      t355 = log(0.4D1 * t152 * t82 * t179)
      t364 = log(-0.4D1 * t138 * t346)
      t366 = t364 ** 2
      t369 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t327, 0.0D0, t
     #328, 0.0D0, 0.0D0)
      t382 = (0.90D2 * t6 * (t329 - t333 * t334) - 0.180D3 * t43 * t339)
     # * t48 * t27 / 0.720D3 + t6 * (-t350 * t334 + t355 * t334) * t48 *
     # t29 / 0.16D2 - (-0.90D2 * t6 * (-t364 * t329 + t366 * t334 / 0.2D
     #1 + t369) + 0.180D3 * t43 * t5 * (t329 - t364 * t334) - t73 * t339
     #) * t48 / 0.1440D4
      t383 = FJET(XB1, XB2, s, -t327, t328, 0.0D0, 0.0D0, 0.0D0, t382)
      t385 = KAPPA2(t173, x2, 0.10D1, t229, z)
      t386 = s * t385
      t387 = t386 * t178
      t388 = t386 * t234
      t389 = t386 * t236
      t390 = t385 ** 2
      t394 = s * t390 * t188 * t190 * x4
      t396 = 0.1D1 / (-0.2D1 + t385)
      t397 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t387, -t388, 0
     #.0D0, t389, -t394)
      t400 = t390 ** 2
      t405 = log(-0.4D1 * t52 * t205 * x4 * t20 * t400)
      t407 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t387, -t388, 0
     #.0D0, t389, -t394)
      t425 = (0.90D2 * t6 * (t396 * t397 - t405 * t396 * t407) - 0.180D3
     # * t43 * t5 * t396 * t407) * t27 * t29 / 0.720D3 + t6 * t396 * t40
     #7 * t48 * t198 / 0.8D1
      t426 = FJET(XB1, XB2, s, -t387, 0.0D0, -t388, t389, -t394, t425)
      rrgg2gght5s1e0 = t171 * t170 + t227 * t226 - t260 * pi * t5 * t253
     # * t257 / 0.8D1 + t325 * t324 + t383 * t382 + t426 * t425

      end function



      doubleprecision function rrgg2gght5s1em1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = 0.1D1 / x3
      t10 = 0.1D1 / x1
      t14 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t15 = x1 ** 2
      t17 = sin(x2 * pi)
      t18 = t17 ** 2
      t20 = z ** 2
      t21 = 0.1D1 / t20
      t24 = log(0.4D1 * t15 * t18 * t21)
      t29 = pi * lh
      t32 = 0.180D3 * t29 * t5 * t7
      t36 = t18 * t21
      t39 = log(0.4D1 * t36 * x4)
      t40 = -0.1D1 + x4
      t44 = log(-0.4D1 * t36 * x4 * t40)
      t47 = 0.1D1 / x4
      t53 = log(0.4D1 * t36)
      t54 = t53 * pi
      t60 = pi ** 2
      t62 = lh ** 2
      t68 = t53 ** 2
      t75 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t78 = x3 * t18
      t81 = log(0.4D1 * t78 * t21)
      t89 = -t6 * t7 * t8 * t10 / 0.8D1 - (0.90D2 * t6 * (t14 - t24 * t7
     #) - t32) * t10 / 0.720D3 - t6 * t7 * (-t39 + t44) * t47 / 0.16D2 -
     # (-0.180D3 * t29 - 0.90D2 * t54) * t5 * t14 / 0.1440D4 - (pi * (-0
     #.30D2 * t60 + 0.180D3 * t62) + 0.180D3 * t54 * lh + 0.45D2 * t68 *
     # pi) * t5 * t7 / 0.1440D4 - t6 * t75 / 0.16D2 - (0.90D2 * t6 * (t1
     #4 - t81 * t7) - t32) * t8 / 0.1440D4
      t90 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t89)
      t92 = 0.1D1 - x1
      t93 = 0.1D1 - x3
      t94 = KAPPA2(t92, x2, t93, 0.10D1, z)
      t95 = s * t94
      t96 = -t92
      t97 = t1 * t96
      t98 = -t93
      t100 = t95 * t97 * t98
      t102 = t95 * t97 * x3
      t103 = t1 * x1
      t104 = t95 * t103
      t105 = t94 ** 2
      t107 = t1 ** 2
      t109 = t96 * x1
      t111 = s * t105 * t107 * t109 * x3
      t113 = 0.1D1 / (-0.2D1 + t94)
      t115 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t100, t104, -t1
     #02, 0.0D0, -t111)
      t120 = FJET(XB1, XB2, s, t100, -t102, t104, 0.0D0, -t111, t6 * t11
     #3 * t115 * t8 * t10 / 0.8D1)
      t128 = t2 * t96
      t129 = t2 * x1
      t130 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t128, t129, 0.
     #0D0, 0.0D0, 0.0D0)
      t135 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t128, t129, 0.
     #0D0, 0.0D0, 0.0D0)
      t136 = t96 ** 2
      t140 = log(0.4D1 * t36 * t15 * t136)
      t155 = t6 * t130 * t8 * t10 / 0.8D1 - (-0.90D2 * t6 * (t135 - t140
     # * t130) + 0.180D3 * t29 * t5 * t130) * t10 / 0.720D3 + t6 * t130 
     #* t10 * t47 / 0.8D1
      t156 = FJET(XB1, XB2, s, -t128, 0.0D0, t129, 0.0D0, 0.0D0, t155)
      t158 = t2 * t98
      t159 = t2 * x3
      t160 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t158, 0.0D0, t
     #159, 0.0D0, 0.0D0)
      t165 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t158, 0.0D0, t
     #159, 0.0D0, 0.0D0)
      t169 = log(-0.4D1 * t78 * t21 * t98)
      t180 = t6 * t160 * t8 * t10 / 0.8D1 - (-0.90D2 * t6 * (t165 - t169
     # * t160) + 0.180D3 * t29 * t5 * t160) * t8 / 0.1440D4
      t181 = FJET(XB1, XB2, s, -t158, t159, 0.0D0, 0.0D0, 0.0D0, t180)
      t184 = KAPPA2(t92, x2, 0.10D1, -t40, z)
      t185 = s * t184
      t186 = t185 * t97
      t188 = t185 * t103 * t40
      t190 = t185 * t103 * x4
      t191 = t184 ** 2
      t195 = s * t191 * t107 * t109 * x4
      t197 = 0.1D1 / (-0.2D1 + t184)
      t199 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t186, -t188, 0
     #.0D0, t190, -t195)
      t204 = FJET(XB1, XB2, s, -t186, 0.0D0, -t188, t190, -t195, t6 * t1
     #97 * t199 * t10 * t47 / 0.8D1)
      rrgg2gght5s1em1 = t90 * t89 + t120 * pi * t5 * t113 * t115 * t8 * 
     #t10 / 0.8D1 + t156 * t155 + t181 * t180 + t204 * pi * t5 * t197 * 
     #t199 * t10 * t47 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s1em2
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = 0.1D1 / x1
      t12 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t18 = sin(x2 * pi)
      t19 = t18 ** 2
      t20 = z ** 2
      t24 = log(0.4D1 * t19 / t20)
      t31 = 0.1D1 / x3
      t35 = -t6 * t7 * t8 / 0.8D1 - t6 * t12 / 0.16D2 - (-0.180D3 * pi *
     # lh - 0.90D2 * t24 * pi) * t5 * t7 / 0.1440D4 - t6 * t7 * t31 / 0.
     #16D2
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t35)
      t39 = t2 * (-0.1D1 + x1)
      t40 = t2 * x1
      t41 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t39, t40, 0.0D0
     #, 0.0D0, 0.0D0)
      t45 = FJET(XB1, XB2, s, -t39, 0.0D0, t40, 0.0D0, 0.0D0, t6 * t41 *
     # t8 / 0.8D1)
      t52 = t2 * (-0.1D1 + x3)
      t53 = t2 * x3
      t54 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t52, 0.0D0, t53
     #, 0.0D0, 0.0D0)
      t58 = FJET(XB1, XB2, s, -t52, t53, 0.0D0, 0.0D0, 0.0D0, t6 * t54 *
     # t31 / 0.16D2)
      rrgg2gght5s1em2 = t36 * t35 + t45 * pi * t5 * t41 * t8 / 0.8D1 + t
     #58 * pi * t5 * t54 * t31 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s1em3
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -pi * t5 *
     # t7 / 0.16D2)
      rrgg2gght5s1em3 = -t10 * pi * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s1em4
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght5s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght5s2e1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x1 ** 2
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t20 = t17 ** 2
      t21 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t24 = -0.1D1 + x4
      t25 = t14 * t24
      t28 = log(-0.4D1 * t11 * t25)
      t30 = t28 ** 2
      t36 = pi * lh
      t44 = 0.1D1 / x1
      t46 = 0.1D1 / x4
      t49 = pi ** 2
      t51 = lh ** 2
      t53 = -0.30D2 * t49 + 0.180D3 * t51
      t54 = t53 * pi
      t55 = t11 * t13
      t57 = log(0.4D1 * t55)
      t62 = t57 ** 2
      t65 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t69 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t79 = -0.240D3 * zeta3 - 0.120D3 * t51 * lh + 0.60D2 * lh * t49
      t80 = pi * t79
      t81 = t5 * t21
      t82 = t80 * t81
      t93 = x3 * t7
      t94 = t93 * t10
      t97 = log(-0.4D1 * t94 * t25)
      t99 = t10 * t13
      t100 = t99 * x4
      t103 = log(0.4D1 * t93 * t100)
      t107 = 0.1D1 / x3
      t109 = t107 * t44 * t46
      t112 = t93 * t99
      t114 = log(0.4D1 * t112)
      t116 = t114 ** 2
      t127 = t54 * t81
      t137 = x4 * t24
      t140 = log(-0.4D1 * t99 * t137)
      t141 = t140 ** 2
      t143 = log(0.4D1 * t100)
      t144 = t143 ** 2
      t167 = log(0.4D1 * t99)
      t168 = t167 * pi
      t170 = t167 ** 2
      t171 = t170 * pi
      t175 = t170 * t167 * pi
      t194 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t197 = t49 ** 2
      t198 = t51 ** 2
      t211 = t170 ** 2
      t218 = x3 * t10
      t221 = log(0.4D1 * t218 * t14)
      t223 = t221 ** 2
      t228 = log(-0.4D1 * t218 * t25)
      t230 = t228 ** 2
      t246 = t218 * t13
      t248 = log(0.4D1 * t246)
      t253 = t248 ** 2
      t273 = (0.90D2 * t6 * (t17 * t18 - t20 * t21 / 0.2D1 - t28 * t18 +
     # t30 * t21 / 0.2D1) - 0.180D3 * t36 * t5 * (t17 * t21 - t28 * t21)
     #) * t44 * t46 / 0.720D3 + (t54 * t5 * (-t18 + t57 * t21) + 0.90D2 
     #* t6 * (-t62 * t18 / 0.2D1 - t65 + t62 * t57 * t21 / 0.6D1 + t57 *
     # t69) - t82 - 0.180D3 * t36 * t5 * (t57 * t18 - t62 * t21 / 0.2D1 
     #- t69)) * t44 / 0.720D3 - t6 * (t97 * t21 - t103 * t21) * t109 / 0
     #.8D1 - (0.90D2 * t6 * (-t114 * t18 + t116 * t21 / 0.2D1 + t69) - 0
     #.180D3 * t36 * t5 * (t18 - t114 * t21) + t127) * t107 * t44 / 0.72
     #0D3 + ((0.90D2 * t6 * t18 - 0.180D3 * t36 * t81) * (t141 / 0.2D1 -
     # t144 / 0.2D1) + 0.90D2 * t6 * t21 * (t144 * t143 / 0.6D1 - t141 *
     # t140 / 0.6D1) + (-0.180D3 * t36 * t5 * t18 + t127 + 0.90D2 * t6 *
     # t69) * (-t140 + t143)) * t46 / 0.1440D4 - (t80 - t168 * t53 - 0.9
     #0D2 * t171 * lh - 0.15D2 * t175) * t5 * t18 / 0.1440D4 - (t54 + 0.
     #180D3 * t168 * lh + 0.45D2 * t171) * t5 * t69 / 0.1440D4 - (-0.180
     #D3 * t36 - 0.90D2 * t168) * t5 * t65 / 0.1440D4 - t6 * t194 / 0.16
     #D2 - (pi * (t197 + 0.60D2 * t198 + 0.480D3 * lh * zeta3 - 0.60D2 *
     # t51 * t49) - t168 * t79 + t171 * t53 / 0.2D1 + 0.30D2 * t175 * lh
     # + 0.15D2 / 0.4D1 * t211 * pi) * t5 * t21 / 0.1440D4 + (0.90D2 * t
     #6 * (t221 * t18 - t223 * t21 / 0.2D1 - t228 * t18 + t230 * t21 / 0
     #.2D1) - 0.180D3 * t36 * t5 * (t221 * t21 - t228 * t21)) * t107 * t
     #46 / 0.1440D4 + (t54 * t5 * (-t18 + t248 * t21) + 0.90D2 * t6 * (-
     #t253 * t18 / 0.2D1 - t65 + t253 * t248 * t21 / 0.6D1 + t248 * t69)
     # - t82 - 0.180D3 * t36 * t5 * (t248 * t18 - t253 * t21 / 0.2D1 - t
     #69)) * t107 / 0.1440D4
      t274 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t273)
      t276 = 0.1D1 - x1
      t277 = 0.1D1 - x3
      t278 = KAPPA2(t276, x2, t277, 0.0D0, z)
      t279 = s * t278
      t280 = -t276
      t281 = t1 * t280
      t282 = -t277
      t283 = t281 * t282
      t284 = t279 * t283
      t285 = t281 * x3
      t286 = t279 * t285
      t287 = t1 * x1
      t288 = t279 * t287
      t289 = t278 ** 2
      t291 = t1 ** 2
      t293 = t280 * x1
      t295 = s * t289 * t291 * t293 * t282
      t297 = 0.1D1 / (-0.2D1 + t278)
      t298 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t284, 0.0D0, -t
     #286, t288, t295)
      t299 = t297 * t298
      t300 = t280 ** 2
      t301 = t300 * t282
      t302 = t289 ** 2
      t307 = log(-0.4D1 * t112 * t301 * x4 * t302)
      t309 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t284, 0.0D0, -t
     #286, t288, t295)
      t315 = t5 * t297 * t309
      t320 = t44 * t46
      t322 = t13 * t300
      t327 = log(-0.4D1 * t94 * t322 * t282 * t302)
      t328 = t327 * t297
      t330 = t327 ** 2
      t334 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t284, 0.0D0, -t
     #286, t288, t295)
      t349 = -(0.90D2 * t6 * (-t299 + t307 * t297 * t309) + 0.180D3 * t3
     #6 * t315) * t107 * t320 / 0.720D3 - (0.90D2 * t6 * (t328 * t298 - 
     #t330 * t297 * t309 / 0.2D1 - t297 * t334) - 0.180D3 * t36 * t5 * (
     #-t299 + t328 * t309) - t54 * t315) * t107 * t44 / 0.720D3
      t350 = FJET(XB1, XB2, s, t284, -t286, 0.0D0, t288, t295, t349)
      t352 = KAPPA2(t276, x2, t277, x4, z)
      t353 = s * t352
      t354 = t353 * t283
      t355 = t353 * t285
      t356 = t287 * x4
      t357 = t353 * t356
      t358 = t287 * t24
      t359 = t353 * t358
      t360 = t352 ** 2
      t365 = cos(t8)
      t368 = Sqrt(x3 * t282 * t137)
      t373 = s * t360 * t291 * t293 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x
     #4 + 0.2D1 * t365 * t368)
      t375 = 0.1D1 / (-0.2D1 + t352)
      t376 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t354, t357, -t3
     #55, -t359, t373)
      t378 = t360 ** 2
      t383 = log(0.4D1 * t112 * t301 * t137 * t378)
      t385 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t354, t357, -t3
     #55, -t359, t373)
      t394 = 0.90D2 * t6 * (t375 * t376 - t383 * t375 * t385) - 0.180D3 
     #* t36 * t5 * t375 * t385
      t398 = FJET(XB1, XB2, s, t354, -t355, t357, -t359, t373, -t394 * t
     #107 * t320 / 0.720D3)
      t402 = t2 * t282
      t403 = t2 * x3
      t404 = t13 * t282
      t405 = t404 * x4
      t408 = log(-0.4D1 * t94 * t405)
      t409 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t402, 0.0D0, t
     #403, 0.0D0, 0.0D0)
      t415 = log(0.4D1 * t94 * t14 * t24 * t282)
      t424 = log(-0.4D1 * t93 * t99 * t282)
      t425 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t402, 0.0D0, t
     #403, 0.0D0, 0.0D0)
      t427 = t424 ** 2
      t430 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t402, 0.0D0, t
     #403, 0.0D0, 0.0D0)
      t439 = t5 * t409
      t447 = log(-0.4D1 * t218 * t405)
      t449 = t447 ** 2
      t455 = log(0.4D1 * t246 * t137 * t282)
      t457 = t455 ** 2
      t475 = log(-0.4D1 * t218 * t404)
      t480 = t475 ** 2
      t483 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, -t402, 0.0D0, t
     #403, 0.0D0, 0.0D0)
      t502 = -t6 * (t408 * t409 - t415 * t409) * t109 / 0.8D1 - (0.90D2 
     #* t6 * (t424 * t425 - t427 * t409 / 0.2D1 - t430) - 0.180D3 * t36 
     #* t5 * (-t425 + t424 * t409) - t54 * t439) * t107 * t44 / 0.720D3 
     #+ (0.90D2 * t6 * (-t447 * t425 + t449 * t409 / 0.2D1 + t455 * t425
     # - t457 * t409 / 0.2D1) - 0.180D3 * t36 * t5 * (-t447 * t409 + t45
     #5 * t409)) * t107 * t46 / 0.1440D4 + (t54 * t5 * (t425 - t475 * t4
     #09) + 0.90D2 * t6 * (t480 * t425 / 0.2D1 + t483 - t480 * t475 * t4
     #09 / 0.6D1 - t475 * t430) + t80 * t439 - 0.180D3 * t36 * t5 * (-t4
     #75 * t425 + t480 * t409 / 0.2D1 + t430)) * t107 / 0.1440D4
      t503 = FJET(XB1, XB2, s, -t402, t403, 0.0D0, 0.0D0, 0.0D0, t502)
      t505 = KAPPA2(t276, x2, 0.10D1, 0.0D0, z)
      t506 = s * t505
      t507 = t506 * t281
      t508 = t506 * t287
      t509 = t505 ** 2
      t513 = s * t509 * t291 * t280 * x1
      t514 = t300 * x4
      t515 = t509 ** 2
      t519 = log(0.4D1 * t55 * t514 * t515)
      t521 = 0.1D1 / (-0.2D1 + t505)
      t522 = t519 * t521
      t523 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t507, 0.0D0, 0
     #.0D0, t508, -t513)
      t525 = t519 ** 2
      t527 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t507, 0.0D0, 0
     #.0D0, t508, -t513)
      t530 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t507, 0.0D0, 0
     #.0D0, t508, -t513)
      t531 = t521 * t530
      t535 = t521 * t523
      t542 = t5 * t521 * t527
      t543 = t54 * t542
      t547 = t322 * t515
      t550 = log(0.4D1 * t11 * t547)
      t551 = t550 * t521
      t556 = t550 ** 2
      t557 = t556 * t521
      t560 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, -t507, 0.0D0, 0
     #.0D0, t508, -t513)
      t584 = log(0.4D1 * t94 * t322 * x4 * t515)
      t597 = log(0.4D1 * t94 * t547)
      t598 = t597 * t521
      t600 = t597 ** 2
      t616 = (0.90D2 * t6 * (t522 * t523 - t525 * t521 * t527 / 0.2D1 - 
     #t531) - 0.180D3 * t36 * t5 * (-t535 + t522 * t527) - t543) * t44 *
     # t46 / 0.720D3 + (-t54 * t5 * (t535 - t551 * t527) - 0.90D2 * t6 *
     # (t557 * t523 / 0.2D1 + t521 * t560 - t556 * t550 * t521 * t527 / 
     #0.6D1 - t551 * t530) - t80 * t542 + 0.180D3 * t36 * t5 * (-t551 * 
     #t523 + t557 * t527 / 0.2D1 + t531)) * t44 / 0.720D3 - (0.90D2 * t6
     # * (t535 - t584 * t521 * t527) - 0.180D3 * t36 * t542) * t107 * t3
     #20 / 0.720D3 - (0.90D2 * t6 * (-t598 * t523 + t600 * t521 * t527 /
     # 0.2D1 + t531) - 0.180D3 * t36 * t5 * (t535 - t598 * t527) + t543)
     # * t107 * t44 / 0.720D3
      t617 = FJET(XB1, XB2, s, -t507, 0.0D0, 0.0D0, t508, -t513, t616)
      t619 = KAPPA2(t276, x2, 0.10D1, x4, z)
      t620 = s * t619
      t621 = t620 * t281
      t622 = t620 * t356
      t623 = t620 * t358
      t624 = t619 ** 2
      t628 = s * t624 * t291 * t293 * t24
      t629 = t624 ** 2
      t631 = t514 * t24 * t629
      t634 = log(-0.4D1 * t55 * t631)
      t636 = 0.1D1 / (-0.2D1 + t619)
      t637 = t634 * t636
      t638 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t621, t622, 0.
     #0D0, -t623, t628)
      t640 = t634 ** 2
      t642 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t621, t622, 0.
     #0D0, -t623, t628)
      t645 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t621, t622, 0.
     #0D0, -t623, t628)
      t650 = t636 * t638
      t657 = t5 * t636 * t642
      t664 = log(-0.4D1 * t112 * t631)
      t676 = (-0.90D2 * t6 * (t637 * t638 - t640 * t636 * t642 / 0.2D1 -
     # t636 * t645) + 0.180D3 * t36 * t5 * (-t650 + t637 * t642) + t54 *
     # t657) * t44 * t46 / 0.720D3 - (0.90D2 * t6 * (-t650 + t664 * t636
     # * t642) + 0.180D3 * t36 * t657) * t107 * t320 / 0.720D3
      t677 = FJET(XB1, XB2, s, -t621, 0.0D0, t622, -t623, t628, t676)
      rrgg2gght5s2e1 = t274 * t273 + t350 * t349 - t398 * t394 * t109 / 
     #0.720D3 + t503 * t502 + t617 * t616 + t677 * t676

      end function



      doubleprecision function rrgg2gght5s2e0
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x1 ** 2
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t20 = -0.1D1 + x4
      t21 = t14 * t20
      t24 = log(-0.4D1 * t11 * t21)
      t27 = 0.1D1 / x1
      t29 = 0.1D1 / x4
      t33 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t34 = x3 * t7
      t35 = t10 * t13
      t38 = log(0.4D1 * t34 * t35)
      t43 = pi * lh
      t44 = t5 * t18
      t46 = 0.180D3 * t43 * t44
      t48 = 0.1D1 / x3
      t52 = t11 * t13
      t54 = log(0.4D1 * t52)
      t56 = t54 ** 2
      t59 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t68 = pi ** 2
      t70 = lh ** 2
      t72 = -0.30D2 * t68 + 0.180D3 * t70
      t73 = pi * t72
      t74 = t73 * t44
      t78 = x4 * t20
      t81 = log(-0.4D1 * t35 * t78)
      t82 = t81 ** 2
      t85 = log(0.4D1 * t35 * x4)
      t86 = t85 ** 2
      t101 = log(0.4D1 * t35)
      t102 = t101 * pi
      t105 = t101 ** 2
      t106 = t105 * pi
      t112 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D
     #0, 0.0D0, 0.0D0)
      t138 = x3 * t10
      t141 = log(0.4D1 * t138 * t14)
      t145 = log(-0.4D1 * t138 * t21)
      t152 = t138 * t13
      t154 = log(0.4D1 * t152)
      t156 = t154 ** 2
      t170 = t6 * (t17 * t18 - t24 * t18) * t27 * t29 / 0.8D1 - (0.90D2 
     #* t6 * (t33 - t38 * t18) - t46) * t48 * t27 / 0.720D3 + (0.90D2 * 
     #t6 * (t54 * t33 - t56 * t18 / 0.2D1 - t59) - 0.180D3 * t43 * t5 * 
     #(-t33 + t54 * t18) - t74) * t27 / 0.720D3 + (0.90D2 * t6 * t18 * (
     #t82 / 0.2D1 - t86 / 0.2D1) + (0.90D2 * t6 * t33 - t46) * (-t81 + t
     #85)) * t29 / 0.1440D4 - (t73 + 0.180D3 * t102 * lh + 0.45D2 * t106
     #) * t5 * t33 / 0.1440D4 - t6 * t112 / 0.16D2 - (pi * (-0.240D3 * z
     #eta3 - 0.120D3 * t70 * lh + 0.60D2 * lh * t68) - t102 * t72 - 0.90
     #D2 * t106 * lh - 0.15D2 * t105 * t101 * pi) * t5 * t18 / 0.1440D4 
     #- (-0.180D3 * t43 - 0.90D2 * t102) * t5 * t59 / 0.1440D4 + t6 * (t
     #141 * t18 - t145 * t18) * t48 * t29 / 0.16D2 + (0.90D2 * t6 * (t15
     #4 * t33 - t156 * t18 / 0.2D1 - t59) - 0.180D3 * t43 * t5 * (-t33 +
     # t154 * t18) - t74) * t48 / 0.1440D4
      t171 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t170)
      t173 = 0.1D1 - x1
      t174 = 0.1D1 - x3
      t175 = KAPPA2(t173, x2, t174, 0.0D0, z)
      t176 = s * t175
      t177 = -t173
      t178 = t1 * t177
      t179 = -t174
      t180 = t178 * t179
      t181 = t176 * t180
      t182 = t178 * x3
      t183 = t176 * t182
      t184 = t1 * x1
      t185 = t176 * t184
      t186 = t175 ** 2
      t188 = t1 ** 2
      t190 = t177 * x1
      t192 = s * t186 * t188 * t190 * t179
      t194 = 0.1D1 / (-0.2D1 + t175)
      t196 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t181, 0.0D0, -t
     #183, t185, t192)
      t198 = t27 * t29
      t202 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t181, 0.0D0, -t
     #183, t185, t192)
      t204 = t34 * t10
      t205 = t177 ** 2
      t206 = t13 * t205
      t207 = t186 ** 2
      t212 = log(-0.4D1 * t204 * t206 * t179 * t207)
      t226 = t6 * t194 * t196 * t48 * t198 / 0.8D1 - (0.90D2 * t6 * (-t1
     #94 * t202 + t212 * t194 * t196) + 0.180D3 * t43 * t5 * t194 * t196
     #) * t48 * t27 / 0.720D3
      t227 = FJET(XB1, XB2, s, t181, -t183, 0.0D0, t185, t192, t226)
      t229 = KAPPA2(t173, x2, t174, x4, z)
      t230 = s * t229
      t231 = t230 * t180
      t232 = t230 * t182
      t233 = t184 * x4
      t234 = t230 * t233
      t235 = t184 * t20
      t236 = t230 * t235
      t237 = t229 ** 2
      t242 = cos(t8)
      t245 = Sqrt(x3 * t179 * t78)
      t250 = s * t237 * t188 * t190 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x
     #4 + 0.2D1 * t242 * t245)
      t252 = 0.1D1 / (-0.2D1 + t229)
      t254 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t231, t234, -t2
     #32, -t236, t250)
      t256 = t254 * t48 * t198
      t259 = FJET(XB1, XB2, s, t231, -t232, t234, -t236, t250, -t6 * t25
     #2 * t256 / 0.8D1)
      t265 = t2 * t179
      t266 = t2 * x3
      t267 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t265, 0.0D0, t
     #266, 0.0D0, 0.0D0)
      t271 = log(-0.4D1 * t34 * t35 * t179)
      t272 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t265, 0.0D0, t
     #266, 0.0D0, 0.0D0)
      t277 = t5 * t272
      t284 = t13 * t179
      t288 = log(-0.4D1 * t138 * t284 * x4)
      t293 = log(0.4D1 * t152 * t78 * t179)
      t302 = log(-0.4D1 * t138 * t284)
      t304 = t302 ** 2
      t307 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t265, 0.0D0, t
     #266, 0.0D0, 0.0D0)
      t320 = -(0.90D2 * t6 * (-t267 + t271 * t272) + 0.180D3 * t43 * t27
     #7) * t48 * t27 / 0.720D3 + t6 * (-t288 * t272 + t293 * t272) * t48
     # * t29 / 0.16D2 + (0.90D2 * t6 * (-t302 * t267 + t304 * t272 / 0.2
     #D1 + t307) - 0.180D3 * t43 * t5 * (t267 - t302 * t272) + t73 * t27
     #7) * t48 / 0.1440D4
      t321 = FJET(XB1, XB2, s, -t265, t266, 0.0D0, 0.0D0, 0.0D0, t320)
      t323 = KAPPA2(t173, x2, 0.10D1, 0.0D0, z)
      t324 = s * t323
      t325 = t324 * t178
      t326 = t324 * t184
      t327 = t323 ** 2
      t331 = s * t327 * t188 * t177 * x1
      t333 = 0.1D1 / (-0.2D1 + t323)
      t334 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t325, 0.0D0, 0
     #.0D0, t326, -t331)
      t335 = t333 * t334
      t336 = t205 * x4
      t337 = t327 ** 2
      t341 = log(0.4D1 * t52 * t336 * t337)
      t343 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t325, 0.0D0, 0
     #.0D0, t326, -t331)
      t349 = t5 * t333 * t343
      t351 = 0.180D3 * t43 * t349
      t361 = t206 * t337
      t364 = log(0.4D1 * t204 * t361)
      t376 = log(0.4D1 * t11 * t361)
      t377 = t376 * t333
      t379 = t376 ** 2
      t383 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t325, 0.0D0, 0
     #.0D0, t326, -t331)
      t397 = (0.90D2 * t6 * (-t335 + t341 * t333 * t343) + t351) * t27 *
     # t29 / 0.720D3 - t6 * t333 * t343 * t48 * t198 / 0.8D1 - (0.90D2 *
     # t6 * (t335 - t364 * t333 * t343) - t351) * t48 * t27 / 0.720D3 + 
     #(-0.90D2 * t6 * (-t377 * t334 + t379 * t333 * t343 / 0.2D1 + t333 
     #* t383) + 0.180D3 * t43 * t5 * (t335 - t377 * t343) - t73 * t349) 
     #* t27 / 0.720D3
      t398 = FJET(XB1, XB2, s, -t325, 0.0D0, 0.0D0, t326, -t331, t397)
      t400 = KAPPA2(t173, x2, 0.10D1, x4, z)
      t401 = s * t400
      t402 = t401 * t178
      t403 = t401 * t233
      t404 = t401 * t235
      t405 = t400 ** 2
      t409 = s * t405 * t188 * t190 * t20
      t411 = 0.1D1 / (-0.2D1 + t400)
      t412 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t402, t403, 0.
     #0D0, -t404, t409)
      t414 = t405 ** 2
      t419 = log(-0.4D1 * t52 * t336 * t20 * t414)
      t421 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t402, t403, 0.
     #0D0, -t404, t409)
      t439 = (-0.90D2 * t6 * (-t411 * t412 + t419 * t411 * t421) - 0.180
     #D3 * t43 * t5 * t411 * t421) * t27 * t29 / 0.720D3 + t6 * t411 * t
     #421 * t48 * t198 / 0.8D1
      t440 = FJET(XB1, XB2, s, -t402, 0.0D0, t403, -t404, t409, t439)
      rrgg2gght5s2e0 = t171 * t170 + t227 * t226 - t259 * pi * t5 * t252
     # * t256 / 0.8D1 + t321 * t320 + t398 * t397 + t440 * t439

      end function



      doubleprecision function rrgg2gght5s2em1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = 0.1D1 / x3
      t10 = 0.1D1 / x1
      t14 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t15 = x1 ** 2
      t17 = sin(x2 * pi)
      t18 = t17 ** 2
      t19 = t15 * t18
      t20 = z ** 2
      t21 = 0.1D1 / t20
      t24 = log(0.4D1 * t19 * t21)
      t29 = pi * lh
      t32 = 0.180D3 * t29 * t5 * t7
      t36 = t18 * t21
      t37 = -0.1D1 + x4
      t41 = log(-0.4D1 * t36 * x4 * t37)
      t44 = log(0.4D1 * t36 * x4)
      t47 = 0.1D1 / x4
      t53 = log(0.4D1 * t36)
      t54 = t53 * pi
      t60 = pi ** 2
      t62 = lh ** 2
      t68 = t53 ** 2
      t75 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t78 = x3 * t18
      t81 = log(0.4D1 * t78 * t21)
      t89 = -t6 * t7 * t8 * t10 / 0.8D1 + (0.90D2 * t6 * (-t14 + t24 * t
     #7) + t32) * t10 / 0.720D3 + t6 * t7 * (-t41 + t44) * t47 / 0.16D2 
     #- (-0.180D3 * t29 - 0.90D2 * t54) * t5 * t14 / 0.1440D4 - (pi * (-
     #0.30D2 * t60 + 0.180D3 * t62) + 0.180D3 * t54 * lh + 0.45D2 * t68 
     #* pi) * t5 * t7 / 0.1440D4 - t6 * t75 / 0.16D2 + (0.90D2 * t6 * (-
     #t14 + t81 * t7) + t32) * t8 / 0.1440D4
      t90 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t89)
      t92 = 0.1D1 - x1
      t93 = 0.1D1 - x3
      t94 = KAPPA2(t92, x2, t93, 0.0D0, z)
      t95 = s * t94
      t96 = -t92
      t97 = t1 * t96
      t98 = -t93
      t100 = t95 * t97 * t98
      t102 = t95 * t97 * x3
      t103 = t1 * x1
      t104 = t95 * t103
      t105 = t94 ** 2
      t107 = t1 ** 2
      t109 = t96 * x1
      t111 = s * t105 * t107 * t109 * t98
      t113 = 0.1D1 / (-0.2D1 + t94)
      t115 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t100, 0.0D0, -t
     #102, t104, t111)
      t120 = FJET(XB1, XB2, s, t100, -t102, 0.0D0, t104, t111, t6 * t113
     # * t115 * t8 * t10 / 0.8D1)
      t128 = t2 * t98
      t129 = t2 * x3
      t130 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t128, 0.0D0, t
     #129, 0.0D0, 0.0D0)
      t135 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t128, 0.0D0, t
     #129, 0.0D0, 0.0D0)
      t139 = log(-0.4D1 * t78 * t21 * t98)
      t150 = t6 * t130 * t8 * t10 / 0.8D1 + (0.90D2 * t6 * (t135 - t139 
     #* t130) - 0.180D3 * t29 * t5 * t130) * t8 / 0.1440D4
      t151 = FJET(XB1, XB2, s, -t128, t129, 0.0D0, 0.0D0, 0.0D0, t150)
      t153 = KAPPA2(t92, x2, 0.10D1, 0.0D0, z)
      t154 = s * t153
      t155 = t154 * t97
      t156 = t154 * t103
      t157 = t153 ** 2
      t161 = s * t157 * t107 * t96 * x1
      t163 = 0.1D1 / (-0.2D1 + t153)
      t164 = t6 * t163
      t165 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t155, 0.0D0, 0
     #.0D0, t156, -t161)
      t170 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t155, 0.0D0, 0
     #.0D0, t156, -t161)
      t172 = t96 ** 2
      t174 = t157 ** 2
      t178 = log(0.4D1 * t19 * t21 * t172 * t174)
      t195 = -t164 * t165 * t8 * t10 / 0.8D1 + (-0.90D2 * t6 * (t163 * t
     #170 - t178 * t163 * t165) + 0.180D3 * t29 * t5 * t163 * t165) * t1
     #0 / 0.720D3 - t164 * t165 * t10 * t47 / 0.8D1
      t196 = FJET(XB1, XB2, s, -t155, 0.0D0, 0.0D0, t156, -t161, t195)
      t198 = KAPPA2(t92, x2, 0.10D1, x4, z)
      t199 = s * t198
      t200 = t199 * t97
      t202 = t199 * t103 * x4
      t204 = t199 * t103 * t37
      t205 = t198 ** 2
      t209 = s * t205 * t107 * t109 * t37
      t211 = 0.1D1 / (-0.2D1 + t198)
      t213 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t200, t202, 0.
     #0D0, -t204, t209)
      t218 = FJET(XB1, XB2, s, -t200, 0.0D0, t202, -t204, t209, t6 * t21
     #1 * t213 * t10 * t47 / 0.8D1)
      rrgg2gght5s2em1 = t90 * t89 + t120 * pi * t5 * t113 * t115 * t8 * 
     #t10 / 0.8D1 + t151 * t150 + t196 * t195 + t218 * pi * t5 * t211 * 
     #t213 * t10 * t47 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s2em2
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = 0.1D1 / x1
      t12 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0
     #, 0.0D0, 0.0D0)
      t18 = sin(x2 * pi)
      t19 = t18 ** 2
      t20 = z ** 2
      t24 = log(0.4D1 * t19 / t20)
      t31 = 0.1D1 / x3
      t35 = -t6 * t7 * t8 / 0.8D1 - t6 * t12 / 0.16D2 - (-0.180D3 * pi *
     # lh - 0.90D2 * t24 * pi) * t5 * t7 / 0.1440D4 - t6 * t7 * t31 / 0.
     #16D2
      t36 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t35)
      t38 = 0.1D1 - x1
      t39 = KAPPA2(t38, x2, 0.10D1, 0.0D0, z)
      t40 = s * t39
      t41 = -t38
      t43 = t40 * t1 * t41
      t45 = t40 * t1 * x1
      t46 = t39 ** 2
      t48 = t1 ** 2
      t51 = s * t46 * t48 * t41 * x1
      t54 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t43, 0.0D0, 0.0
     #D0, t45, -t51)
      t56 = 0.1D1 / (-0.2D1 + t39) * t54 * t8
      t59 = FJET(XB1, XB2, s, -t43, 0.0D0, 0.0D0, t45, -t51, -t6 * t56 /
     # 0.8D1)
      t65 = t2 * (-0.1D1 + x3)
      t66 = t2 * x3
      t67 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t65, 0.0D0, t66
     #, 0.0D0, 0.0D0)
      t71 = FJET(XB1, XB2, s, -t65, t66, 0.0D0, 0.0D0, 0.0D0, t6 * t67 *
     # t31 / 0.16D2)
      rrgg2gght5s2em2 = t36 * t35 - t59 * pi * t5 * t56 / 0.8D1 + t71 * 
     #pi * t5 * t67 * t31 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s2em3
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t2, 0.0D0, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = FJET(XB1, XB2, s, t2, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -pi * t5 *
     # t7 / 0.16D2)
      rrgg2gght5s2em3 = -t10 * pi * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s2em4
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght5s2em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght5s3e1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x1 ** 2
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t15 = -0.1D1 + x4
      t16 = t14 * t15
      t19 = log(-0.4D1 * t11 * t16)
      t20 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t22 = t19 ** 2
      t23 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t28 = log(0.4D1 * t11 * t14)
      t30 = t28 ** 2
      t36 = pi * lh
      t44 = 0.1D1 / x1
      t46 = 0.1D1 / x4
      t49 = pi ** 2
      t51 = lh ** 2
      t53 = -0.30D2 * t49 + 0.180D3 * t51
      t54 = pi * t53
      t55 = t11 * t13
      t57 = log(0.4D1 * t55)
      t62 = t57 ** 2
      t65 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t69 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t79 = -0.240D3 * zeta3 - 0.120D3 * t51 * lh + 0.60D2 * lh * t49
      t80 = pi * t79
      t81 = t5 * t23
      t82 = t80 * t81
      t93 = x3 * t7
      t94 = t10 * t13
      t95 = t94 * x4
      t98 = log(0.4D1 * t93 * t95)
      t100 = t93 * t10
      t103 = log(-0.4D1 * t100 * t16)
      t107 = 0.1D1 / x3
      t109 = t107 * t44 * t46
      t112 = t93 * t94
      t114 = log(0.4D1 * t112)
      t116 = t114 ** 2
      t127 = t54 * t81
      t137 = x4 * t15
      t140 = log(-0.4D1 * t94 * t137)
      t141 = t140 ** 2
      t143 = log(0.4D1 * t95)
      t144 = t143 ** 2
      t167 = log(0.4D1 * t94)
      t168 = t167 * pi
      t170 = t167 ** 2
      t171 = t170 * pi
      t175 = t170 * t167 * pi
      t194 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t197 = t49 ** 2
      t198 = t51 ** 2
      t211 = t170 ** 2
      t218 = x3 * t10
      t221 = log(-0.4D1 * t218 * t16)
      t223 = t221 ** 2
      t228 = log(0.4D1 * t218 * t14)
      t230 = t228 ** 2
      t246 = t218 * t13
      t248 = log(0.4D1 * t246)
      t253 = t248 ** 2
      t273 = (0.90D2 * t6 * (-t19 * t20 + t22 * t23 / 0.2D1 + t28 * t20 
     #- t30 * t23 / 0.2D1) - 0.180D3 * t36 * t5 * (-t19 * t23 + t28 * t2
     #3)) * t44 * t46 / 0.720D3 + (t54 * t5 * (-t20 + t57 * t23) + 0.90D
     #2 * t6 * (-t62 * t20 / 0.2D1 - t65 + t62 * t57 * t23 / 0.6D1 + t57
     # * t69) - t82 - 0.180D3 * t36 * t5 * (t57 * t20 - t62 * t23 / 0.2D
     #1 - t69)) * t44 / 0.720D3 + t6 * (t98 * t23 - t103 * t23) * t109 /
     # 0.8D1 - (0.90D2 * t6 * (-t114 * t20 + t116 * t23 / 0.2D1 + t69) -
     # 0.180D3 * t36 * t5 * (t20 - t114 * t23) + t127) * t107 * t44 / 0.
     #720D3 - ((-0.90D2 * t6 * t20 + 0.180D3 * t36 * t81) * (t141 / 0.2D
     #1 - t144 / 0.2D1) - 0.90D2 * t6 * t23 * (t144 * t143 / 0.6D1 - t14
     #1 * t140 / 0.6D1) + (0.180D3 * t36 * t5 * t20 - t127 - 0.90D2 * t6
     # * t69) * (-t140 + t143)) * t46 / 0.1440D4 - (t80 - t168 * t53 - 0
     #.90D2 * t171 * lh - 0.15D2 * t175) * t5 * t20 / 0.1440D4 - (t54 + 
     #0.180D3 * t168 * lh + 0.45D2 * t171) * t5 * t69 / 0.1440D4 - (-0.1
     #80D3 * t36 - 0.90D2 * t168) * t5 * t65 / 0.1440D4 - t6 * t194 / 0.
     #16D2 - (pi * (t197 + 0.60D2 * t198 + 0.480D3 * lh * zeta3 - 0.60D2
     # * t51 * t49) - t168 * t79 + t171 * t53 / 0.2D1 + 0.30D2 * t175 * 
     #lh + 0.15D2 / 0.4D1 * t211 * pi) * t5 * t23 / 0.1440D4 - (0.90D2 *
     # t6 * (t221 * t20 - t223 * t23 / 0.2D1 - t228 * t20 + t230 * t23 /
     # 0.2D1) - 0.180D3 * t36 * t5 * (t221 * t23 - t228 * t23)) * t107 *
     # t46 / 0.1440D4 + (t54 * t5 * (-t20 + t248 * t23) + 0.90D2 * t6 * 
     #(-t253 * t20 / 0.2D1 - t65 + t253 * t248 * t23 / 0.6D1 + t248 * t6
     #9) - t82 - 0.180D3 * t36 * t5 * (t248 * t20 - t253 * t23 / 0.2D1 -
     # t69)) * t107 / 0.1440D4
      t274 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t273)
      t276 = 0.1D1 - x1
      t277 = KAPPA2(t276, x2, 0.0D0, 0.10D1, z)
      t278 = s * t277
      t279 = -t276
      t280 = t1 * t279
      t281 = t278 * t280
      t282 = t1 * x1
      t283 = t278 * t282
      t284 = t277 ** 2
      t286 = t1 ** 2
      t289 = s * t284 * t286 * t279 * x1
      t290 = t279 ** 2
      t291 = t290 * x4
      t292 = t284 ** 2
      t296 = log(0.4D1 * t55 * t291 * t292)
      t298 = 0.1D1 / (-0.2D1 + t277)
      t299 = t296 * t298
      t300 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t283, -t
     #281, 0.0D0, -t289)
      t302 = t296 ** 2
      t304 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t283, -t
     #281, 0.0D0, -t289)
      t307 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t283, -t
     #281, 0.0D0, -t289)
      t308 = t298 * t307
      t312 = t298 * t300
      t319 = t5 * t298 * t304
      t320 = t54 * t319
      t324 = t13 * t290
      t325 = t324 * t292
      t328 = log(0.4D1 * t11 * t325)
      t329 = t328 * t298
      t334 = t328 ** 2
      t335 = t334 * t298
      t338 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t283, -t
     #281, 0.0D0, -t289)
      t362 = log(0.4D1 * t100 * t324 * x4 * t292)
      t372 = t44 * t46
      t376 = log(0.4D1 * t100 * t325)
      t377 = t376 * t298
      t379 = t376 ** 2
      t395 = (0.90D2 * t6 * (t299 * t300 - t302 * t298 * t304 / 0.2D1 - 
     #t308) - 0.180D3 * t36 * t5 * (-t312 + t299 * t304) - t320) * t44 *
     # t46 / 0.720D3 + (-t54 * t5 * (t312 - t329 * t304) - 0.90D2 * t6 *
     # (t335 * t300 / 0.2D1 + t298 * t338 - t334 * t328 * t298 * t304 / 
     #0.6D1 - t329 * t307) - t80 * t319 + 0.180D3 * t36 * t5 * (-t329 * 
     #t300 + t335 * t304 / 0.2D1 + t308)) * t44 / 0.720D3 + (0.90D2 * t6
     # * (-t312 + t362 * t298 * t304) + 0.180D3 * t36 * t319) * t107 * t
     #372 / 0.720D3 - (0.90D2 * t6 * (-t377 * t300 + t379 * t298 * t304 
     #/ 0.2D1 + t308) - 0.180D3 * t36 * t5 * (t312 - t377 * t304) + t320
     #) * t107 * t44 / 0.720D3
      t396 = FJET(XB1, XB2, s, 0.0D0, -t281, t283, 0.0D0, -t289, t395)
      t398 = -t15
      t399 = KAPPA2(t276, x2, 0.0D0, t398, z)
      t400 = s * t399
      t401 = t400 * t280
      t402 = t282 * t15
      t403 = t400 * t402
      t404 = t282 * x4
      t405 = t400 * t404
      t406 = t399 ** 2
      t409 = t279 * x1
      t411 = s * t406 * t286 * t409 * t15
      t412 = t406 ** 2
      t414 = t291 * t15 * t412
      t417 = log(-0.4D1 * t55 * t414)
      t419 = 0.1D1 / (-0.2D1 + t399)
      t420 = t417 * t419
      t421 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t403, -
     #t401, t405, t411)
      t423 = t417 ** 2
      t425 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t403, -
     #t401, t405, t411)
      t428 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t403, -
     #t401, t405, t411)
      t433 = t419 * t421
      t440 = t5 * t419 * t425
      t447 = log(-0.4D1 * t112 * t414)
      t459 = (0.90D2 * t6 * (-t420 * t421 + t423 * t419 * t425 / 0.2D1 +
     # t419 * t428) - 0.180D3 * t36 * t5 * (t433 - t420 * t425) + t54 * 
     #t440) * t44 * t46 / 0.720D3 + (0.90D2 * t6 * (t433 - t447 * t419 *
     # t425) - 0.180D3 * t36 * t440) * t107 * t372 / 0.720D3
      t460 = FJET(XB1, XB2, s, 0.0D0, -t401, -t403, t405, t411, t459)
      t462 = t2 * x3
      t463 = -0.1D1 + x3
      t464 = t2 * t463
      t469 = log(0.4D1 * t100 * t14 * t15 * t463)
      t470 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t462, 0.0D0, -t
     #464, 0.0D0, 0.0D0)
      t472 = t13 * t463
      t473 = t472 * x4
      t476 = log(-0.4D1 * t100 * t473)
      t485 = log(-0.4D1 * t93 * t94 * t463)
      t486 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t462, 0.0D0, -t
     #464, 0.0D0, 0.0D0)
      t488 = t485 ** 2
      t491 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t462, 0.0D0, -t
     #464, 0.0D0, 0.0D0)
      t500 = t5 * t470
      t509 = log(0.4D1 * t246 * t137 * t463)
      t511 = t509 ** 2
      t516 = log(-0.4D1 * t218 * t473)
      t518 = t516 ** 2
      t536 = log(-0.4D1 * t218 * t472)
      t541 = t536 ** 2
      t544 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, t462, 0.0D0, -t
     #464, 0.0D0, 0.0D0)
      t563 = t6 * (t469 * t470 - t476 * t470) * t109 / 0.8D1 - (0.90D2 *
     # t6 * (t485 * t486 - t488 * t470 / 0.2D1 - t491) - 0.180D3 * t36 *
     # t5 * (-t486 + t485 * t470) - t54 * t500) * t107 * t44 / 0.720D3 -
     # (0.90D2 * t6 * (-t509 * t486 + t511 * t470 / 0.2D1 + t516 * t486 
     #- t518 * t470 / 0.2D1) - 0.180D3 * t36 * t5 * (-t509 * t470 + t516
     # * t470)) * t107 * t46 / 0.1440D4 + (t54 * t5 * (t486 - t536 * t47
     #0) + 0.90D2 * t6 * (t541 * t486 / 0.2D1 + t544 - t541 * t536 * t47
     #0 / 0.6D1 - t536 * t491) + t80 * t500 - 0.180D3 * t36 * t5 * (-t53
     #6 * t486 + t541 * t470 / 0.2D1 + t491)) * t107 / 0.1440D4
      t564 = FJET(XB1, XB2, s, t462, -t464, 0.0D0, 0.0D0, 0.0D0, t563)
      t566 = KAPPA2(t276, x2, x3, 0.10D1, z)
      t567 = s * t566
      t568 = t280 * x3
      t569 = t567 * t568
      t570 = t280 * t463
      t571 = t567 * t570
      t572 = t567 * t282
      t573 = t566 ** 2
      t577 = s * t573 * t286 * t409 * t463
      t579 = 0.1D1 / (-0.2D1 + t566)
      t580 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t569, t572, t5
     #71, 0.0D0, t577)
      t581 = t579 * t580
      t582 = t290 * t463
      t583 = t573 ** 2
      t588 = log(-0.4D1 * t112 * t582 * x4 * t583)
      t590 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t569, t572, t5
     #71, 0.0D0, t577)
      t596 = t5 * t579 * t590
      t606 = log(-0.4D1 * t100 * t324 * t463 * t583)
      t607 = t606 * t579
      t609 = t606 ** 2
      t613 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t569, t572, t5
     #71, 0.0D0, t577)
      t628 = (0.90D2 * t6 * (t581 - t588 * t579 * t590) - 0.180D3 * t36 
     #* t596) * t107 * t372 / 0.720D3 - (0.90D2 * t6 * (t607 * t580 - t6
     #09 * t579 * t590 / 0.2D1 - t579 * t613) - 0.180D3 * t36 * t5 * (-t
     #581 + t607 * t590) - t54 * t596) * t107 * t44 / 0.720D3
      t629 = FJET(XB1, XB2, s, -t569, t571, t572, 0.0D0, t577, t628)
      t631 = KAPPA2(t276, x2, x3, t398, z)
      t632 = s * t631
      t633 = t632 * t568
      t634 = t632 * t570
      t635 = t632 * t402
      t636 = t632 * t404
      t637 = t631 ** 2
      t642 = cos(t8)
      t645 = Sqrt(x3 * t463 * t137)
      t650 = s * t637 * t286 * t409 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x
     #4 + 0.2D1 * t642 * t645)
      t652 = 0.1D1 / (-0.2D1 + t631)
      t653 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t633, -t635, t
     #634, t636, t650)
      t655 = t637 ** 2
      t660 = log(0.4D1 * t112 * t582 * t137 * t655)
      t662 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t633, -t635, t
     #634, t636, t650)
      t671 = -0.90D2 * t6 * (t652 * t653 - t660 * t652 * t662) + 0.180D3
     # * t36 * t5 * t652 * t662
      t675 = FJET(XB1, XB2, s, -t633, t634, -t635, t636, t650, t671 * t1
     #07 * t372 / 0.720D3)
      rrgg2gght5s3e1 = t274 * t273 + t396 * t395 + t460 * t459 + t564 * 
     #t563 + t629 * t628 + t675 * t671 * t109 / 0.720D3

      end function



      doubleprecision function rrgg2gght5s3e0
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x1 ** 2
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t15 = -0.1D1 + x4
      t16 = t14 * t15
      t19 = log(-0.4D1 * t11 * t16)
      t20 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t24 = log(0.4D1 * t11 * t14)
      t27 = 0.1D1 / x1
      t29 = 0.1D1 / x4
      t33 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t34 = x3 * t7
      t35 = t10 * t13
      t38 = log(0.4D1 * t34 * t35)
      t43 = pi * lh
      t44 = t5 * t20
      t46 = 0.180D3 * t43 * t44
      t48 = 0.1D1 / x3
      t52 = t11 * t13
      t54 = log(0.4D1 * t52)
      t56 = t54 ** 2
      t59 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t68 = pi ** 2
      t70 = lh ** 2
      t72 = -0.30D2 * t68 + 0.180D3 * t70
      t73 = pi * t72
      t74 = t73 * t44
      t78 = x4 * t15
      t81 = log(-0.4D1 * t35 * t78)
      t82 = t81 ** 2
      t85 = log(0.4D1 * t35 * x4)
      t86 = t85 ** 2
      t101 = log(0.4D1 * t35)
      t102 = t101 * pi
      t105 = t101 ** 2
      t106 = t105 * pi
      t112 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t138 = x3 * t10
      t141 = log(-0.4D1 * t138 * t16)
      t145 = log(0.4D1 * t138 * t14)
      t152 = t138 * t13
      t154 = log(0.4D1 * t152)
      t156 = t154 ** 2
      t170 = t6 * (-t19 * t20 + t24 * t20) * t27 * t29 / 0.8D1 - (0.90D2
     # * t6 * (t33 - t38 * t20) - t46) * t48 * t27 / 0.720D3 + (0.90D2 *
     # t6 * (t54 * t33 - t56 * t20 / 0.2D1 - t59) - 0.180D3 * t43 * t5 *
     # (-t33 + t54 * t20) - t74) * t27 / 0.720D3 - (-0.90D2 * t6 * t20 *
     # (t82 / 0.2D1 - t86 / 0.2D1) + (-0.90D2 * t6 * t33 + t46) * (-t81 
     #+ t85)) * t29 / 0.1440D4 - (t73 + 0.180D3 * t102 * lh + 0.45D2 * t
     #106) * t5 * t33 / 0.1440D4 - t6 * t112 / 0.16D2 - (pi * (-0.240D3 
     #* zeta3 - 0.120D3 * t70 * lh + 0.60D2 * lh * t68) - t102 * t72 - 0
     #.90D2 * t106 * lh - 0.15D2 * t105 * t101 * pi) * t5 * t20 / 0.1440
     #D4 - (-0.180D3 * t43 - 0.90D2 * t102) * t5 * t59 / 0.1440D4 - t6 *
     # (t141 * t20 - t145 * t20) * t48 * t29 / 0.16D2 + (0.90D2 * t6 * (
     #t154 * t33 - t156 * t20 / 0.2D1 - t59) - 0.180D3 * t43 * t5 * (-t3
     #3 + t154 * t20) - t74) * t48 / 0.1440D4
      t171 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t170)
      t173 = 0.1D1 - x1
      t174 = KAPPA2(t173, x2, 0.0D0, 0.10D1, z)
      t175 = s * t174
      t176 = -t173
      t177 = t1 * t176
      t178 = t175 * t177
      t179 = t1 * x1
      t180 = t175 * t179
      t181 = t174 ** 2
      t183 = t1 ** 2
      t186 = s * t181 * t183 * t176 * x1
      t188 = 0.1D1 / (-0.2D1 + t174)
      t189 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t180, -t
     #178, 0.0D0, -t186)
      t190 = t188 * t189
      t191 = t176 ** 2
      t192 = t191 * x4
      t193 = t181 ** 2
      t197 = log(0.4D1 * t52 * t192 * t193)
      t199 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t180, -t
     #178, 0.0D0, -t186)
      t205 = t5 * t188 * t199
      t207 = 0.180D3 * t43 * t205
      t214 = t27 * t29
      t218 = t34 * t10
      t219 = t13 * t191
      t220 = t219 * t193
      t223 = log(0.4D1 * t218 * t220)
      t235 = log(0.4D1 * t11 * t220)
      t236 = t235 * t188
      t238 = t235 ** 2
      t242 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t180, -t
     #178, 0.0D0, -t186)
      t256 = (0.90D2 * t6 * (-t190 + t197 * t188 * t199) + t207) * t27 *
     # t29 / 0.720D3 - t6 * t188 * t199 * t48 * t214 / 0.8D1 - (0.90D2 *
     # t6 * (t190 - t223 * t188 * t199) - t207) * t48 * t27 / 0.720D3 + 
     #(-0.90D2 * t6 * (-t236 * t189 + t238 * t188 * t199 / 0.2D1 + t188 
     #* t242) + 0.180D3 * t43 * t5 * (t190 - t236 * t199) - t73 * t205) 
     #* t27 / 0.720D3
      t257 = FJET(XB1, XB2, s, 0.0D0, -t178, t180, 0.0D0, -t186, t256)
      t259 = -t15
      t260 = KAPPA2(t173, x2, 0.0D0, t259, z)
      t261 = s * t260
      t262 = t261 * t177
      t263 = t179 * t15
      t264 = t261 * t263
      t265 = t179 * x4
      t266 = t261 * t265
      t267 = t260 ** 2
      t270 = t176 * x1
      t272 = s * t267 * t183 * t270 * t15
      t274 = 0.1D1 / (-0.2D1 + t260)
      t275 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t264, -
     #t262, t266, t272)
      t277 = t267 ** 2
      t282 = log(-0.4D1 * t52 * t192 * t15 * t277)
      t284 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t264, -
     #t262, t266, t272)
      t302 = (0.90D2 * t6 * (t274 * t275 - t282 * t274 * t284) - 0.180D3
     # * t43 * t5 * t274 * t284) * t27 * t29 / 0.720D3 + t6 * t274 * t28
     #4 * t48 * t214 / 0.8D1
      t303 = FJET(XB1, XB2, s, 0.0D0, -t262, -t264, t266, t272, t302)
      t305 = t2 * x3
      t306 = -0.1D1 + x3
      t307 = t2 * t306
      t308 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t305, 0.0D0, -t
     #307, 0.0D0, 0.0D0)
      t312 = log(-0.4D1 * t34 * t35 * t306)
      t313 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t305, 0.0D0, -t
     #307, 0.0D0, 0.0D0)
      t318 = t5 * t313
      t328 = log(0.4D1 * t152 * t78 * t306)
      t330 = t13 * t306
      t334 = log(-0.4D1 * t138 * t330 * x4)
      t343 = log(-0.4D1 * t138 * t330)
      t345 = t343 ** 2
      t348 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t305, 0.0D0, -t
     #307, 0.0D0, 0.0D0)
      t361 = -(0.90D2 * t6 * (-t308 + t312 * t313) + 0.180D3 * t43 * t31
     #8) * t48 * t27 / 0.720D3 - t6 * (-t328 * t313 + t334 * t313) * t48
     # * t29 / 0.16D2 + (0.90D2 * t6 * (-t343 * t308 + t345 * t313 / 0.2
     #D1 + t348) - 0.180D3 * t43 * t5 * (t308 - t343 * t313) + t73 * t31
     #8) * t48 / 0.1440D4
      t362 = FJET(XB1, XB2, s, t305, -t307, 0.0D0, 0.0D0, 0.0D0, t361)
      t364 = KAPPA2(t173, x2, x3, 0.10D1, z)
      t365 = s * t364
      t366 = t177 * x3
      t367 = t365 * t366
      t368 = t177 * t306
      t369 = t365 * t368
      t370 = t365 * t179
      t371 = t364 ** 2
      t375 = s * t371 * t183 * t270 * t306
      t377 = 0.1D1 / (-0.2D1 + t364)
      t379 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t367, t370, t3
     #69, 0.0D0, t375)
      t384 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t367, t370, t3
     #69, 0.0D0, t375)
      t386 = t371 ** 2
      t391 = log(-0.4D1 * t218 * t219 * t306 * t386)
      t405 = t6 * t377 * t379 * t48 * t214 / 0.8D1 - (0.90D2 * t6 * (-t3
     #77 * t384 + t391 * t377 * t379) + 0.180D3 * t43 * t5 * t377 * t379
     #) * t48 * t27 / 0.720D3
      t406 = FJET(XB1, XB2, s, -t367, t369, t370, 0.0D0, t375, t405)
      t408 = KAPPA2(t173, x2, x3, t259, z)
      t409 = s * t408
      t410 = t409 * t366
      t411 = t409 * t368
      t412 = t409 * t263
      t413 = t409 * t265
      t414 = t408 ** 2
      t419 = cos(t8)
      t422 = Sqrt(x3 * t306 * t78)
      t427 = s * t414 * t183 * t270 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x
     #4 + 0.2D1 * t419 * t422)
      t429 = 0.1D1 / (-0.2D1 + t408)
      t431 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t410, -t412, t
     #411, t413, t427)
      t433 = t431 * t48 * t214
      t436 = FJET(XB1, XB2, s, -t410, t411, -t412, t413, t427, -t6 * t42
     #9 * t433 / 0.8D1)
      rrgg2gght5s3e0 = t171 * t170 + t257 * t256 + t303 * t302 + t362 * 
     #t361 + t406 * t405 - t436 * pi * t5 * t429 * t433 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s3em1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t8 = 0.1D1 / x3
      t10 = 0.1D1 / x1
      t14 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t15 = x1 ** 2
      t17 = sin(x2 * pi)
      t18 = t17 ** 2
      t19 = t15 * t18
      t20 = z ** 2
      t21 = 0.1D1 / t20
      t24 = log(0.4D1 * t19 * t21)
      t29 = pi * lh
      t32 = 0.180D3 * t29 * t5 * t7
      t36 = t18 * t21
      t37 = -0.1D1 + x4
      t41 = log(-0.4D1 * t36 * x4 * t37)
      t44 = log(0.4D1 * t36 * x4)
      t47 = 0.1D1 / x4
      t53 = log(0.4D1 * t36)
      t54 = pi * t53
      t60 = pi ** 2
      t62 = lh ** 2
      t68 = t53 ** 2
      t75 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t78 = x3 * t18
      t81 = log(0.4D1 * t78 * t21)
      t89 = -t6 * t7 * t8 * t10 / 0.8D1 + (0.90D2 * t6 * (-t14 + t24 * t
     #7) + t32) * t10 / 0.720D3 + t6 * t7 * (-t41 + t44) * t47 / 0.16D2 
     #- (-0.180D3 * t29 - 0.90D2 * t54) * t5 * t14 / 0.1440D4 - (pi * (-
     #0.30D2 * t60 + 0.180D3 * t62) + 0.180D3 * t54 * lh + 0.45D2 * t68 
     #* pi) * t5 * t7 / 0.1440D4 - t6 * t75 / 0.16D2 + (0.90D2 * t6 * (-
     #t14 + t81 * t7) + t32) * t8 / 0.1440D4
      t90 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t89)
      t92 = 0.1D1 - x1
      t93 = KAPPA2(t92, x2, 0.0D0, 0.10D1, z)
      t94 = s * t93
      t95 = -t92
      t96 = t1 * t95
      t97 = t94 * t96
      t98 = t1 * x1
      t99 = t94 * t98
      t100 = t93 ** 2
      t102 = t1 ** 2
      t105 = s * t100 * t102 * t95 * x1
      t107 = 0.1D1 / (-0.2D1 + t93)
      t108 = t6 * t107
      t109 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t99, -t9
     #7, 0.0D0, -t105)
      t114 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t99, -t9
     #7, 0.0D0, -t105)
      t116 = t95 ** 2
      t118 = t100 ** 2
      t122 = log(0.4D1 * t19 * t21 * t116 * t118)
      t139 = -t108 * t109 * t8 * t10 / 0.8D1 + (-0.90D2 * t6 * (t107 * t
     #114 - t122 * t107 * t109) + 0.180D3 * t29 * t5 * t107 * t109) * t1
     #0 / 0.720D3 - t108 * t109 * t10 * t47 / 0.8D1
      t140 = FJET(XB1, XB2, s, 0.0D0, -t97, t99, 0.0D0, -t105, t139)
      t143 = KAPPA2(t92, x2, 0.0D0, -t37, z)
      t144 = s * t143
      t145 = t144 * t96
      t147 = t144 * t98 * t37
      t149 = t144 * t98 * x4
      t150 = t143 ** 2
      t153 = t95 * x1
      t155 = s * t150 * t102 * t153 * t37
      t157 = 0.1D1 / (-0.2D1 + t143)
      t159 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t147, -
     #t145, t149, t155)
      t164 = FJET(XB1, XB2, s, 0.0D0, -t145, -t147, t149, t155, t6 * t15
     #7 * t159 * t10 * t47 / 0.8D1)
      t172 = t2 * x3
      t173 = -0.1D1 + x3
      t174 = t2 * t173
      t175 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t172, 0.0D0, -t
     #174, 0.0D0, 0.0D0)
      t180 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t172, 0.0D0, -t
     #174, 0.0D0, 0.0D0)
      t184 = log(-0.4D1 * t78 * t21 * t173)
      t195 = t6 * t175 * t8 * t10 / 0.8D1 + (0.90D2 * t6 * (t180 - t184 
     #* t175) - 0.180D3 * t29 * t5 * t175) * t8 / 0.1440D4
      t196 = FJET(XB1, XB2, s, t172, -t174, 0.0D0, 0.0D0, 0.0D0, t195)
      t198 = KAPPA2(t92, x2, x3, 0.10D1, z)
      t199 = s * t198
      t201 = t199 * t96 * x3
      t203 = t199 * t96 * t173
      t204 = t199 * t98
      t205 = t198 ** 2
      t209 = s * t205 * t102 * t153 * t173
      t211 = 0.1D1 / (-0.2D1 + t198)
      t213 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t201, t204, t2
     #03, 0.0D0, t209)
      t218 = FJET(XB1, XB2, s, -t201, t203, t204, 0.0D0, t209, t6 * t211
     # * t213 * t8 * t10 / 0.8D1)
      rrgg2gght5s3em1 = t90 * t89 + t140 * t139 + t164 * pi * t5 * t157 
     #* t159 * t10 * t47 / 0.8D1 + t196 * t195 + t218 * pi * t5 * t211 *
     # t213 * t8 * t10 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s3em2
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t8 = 0.1D1 / x1
      t12 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t18 = sin(x2 * pi)
      t19 = t18 ** 2
      t20 = z ** 2
      t24 = log(0.4D1 * t19 / t20)
      t31 = 0.1D1 / x3
      t35 = -t6 * t7 * t8 / 0.8D1 - t6 * t12 / 0.16D2 - (-0.180D3 * pi *
     # lh - 0.90D2 * t24 * pi) * t5 * t7 / 0.1440D4 - t6 * t7 * t31 / 0.
     #16D2
      t36 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t35)
      t38 = 0.1D1 - x1
      t39 = KAPPA2(t38, x2, 0.0D0, 0.10D1, z)
      t40 = s * t39
      t41 = -t38
      t43 = t40 * t1 * t41
      t45 = t40 * t1 * x1
      t46 = t39 ** 2
      t48 = t1 ** 2
      t51 = s * t46 * t48 * t41 * x1
      t54 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t45, -t43
     #, 0.0D0, -t51)
      t56 = 0.1D1 / (-0.2D1 + t39) * t54 * t8
      t59 = FJET(XB1, XB2, s, 0.0D0, -t43, t45, 0.0D0, -t51, -t6 * t56 /
     # 0.8D1)
      t64 = t2 * x3
      t66 = t2 * (-0.1D1 + x3)
      t67 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t64, 0.0D0, -t66
     #, 0.0D0, 0.0D0)
      t71 = FJET(XB1, XB2, s, t64, -t66, 0.0D0, 0.0D0, 0.0D0, t6 * t67 *
     # t31 / 0.16D2)
      rrgg2gght5s3em2 = t36 * t35 - t59 * pi * t5 * t56 / 0.8D1 + t71 * 
     #pi * t5 * t67 * t31 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s3em3
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t10 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -pi * t5 *
     # t7 / 0.16D2)
      rrgg2gght5s3em3 = -t10 * pi * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s3em4
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght5s3em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght5s4e1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x1 ** 2
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t20 = t17 ** 2
      t21 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t24 = -0.1D1 + x4
      t25 = t14 * t24
      t28 = log(-0.4D1 * t11 * t25)
      t30 = t28 ** 2
      t36 = pi * lh
      t44 = 0.1D1 / x1
      t46 = 0.1D1 / x4
      t49 = pi ** 2
      t51 = lh ** 2
      t53 = -0.30D2 * t49 + 0.180D3 * t51
      t54 = pi * t53
      t55 = t11 * t13
      t57 = log(0.4D1 * t55)
      t62 = t57 ** 2
      t65 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t69 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t79 = -0.240D3 * zeta3 - 0.120D3 * t51 * lh + 0.60D2 * lh * t49
      t80 = pi * t79
      t81 = t5 * t21
      t82 = t80 * t81
      t93 = x3 * t7
      t94 = t10 * t13
      t95 = t94 * x4
      t98 = log(0.4D1 * t93 * t95)
      t100 = t93 * t10
      t103 = log(-0.4D1 * t100 * t25)
      t107 = 0.1D1 / x3
      t109 = t107 * t44 * t46
      t112 = t93 * t94
      t114 = log(0.4D1 * t112)
      t116 = t114 ** 2
      t127 = t54 * t81
      t137 = x4 * t24
      t140 = log(-0.4D1 * t94 * t137)
      t141 = t140 ** 2
      t143 = log(0.4D1 * t95)
      t144 = t143 ** 2
      t167 = log(0.4D1 * t94)
      t168 = t167 * pi
      t170 = t167 ** 2
      t171 = t170 * pi
      t175 = t170 * t167 * pi
      t194 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t197 = t49 ** 2
      t198 = t51 ** 2
      t211 = t170 ** 2
      t218 = x3 * t10
      t221 = log(-0.4D1 * t218 * t25)
      t223 = t221 ** 2
      t228 = log(0.4D1 * t218 * t14)
      t230 = t228 ** 2
      t246 = t218 * t13
      t248 = log(0.4D1 * t246)
      t253 = t248 ** 2
      t273 = -(0.90D2 * t6 * (-t17 * t18 + t20 * t21 / 0.2D1 + t28 * t18
     # - t30 * t21 / 0.2D1) - 0.180D3 * t36 * t5 * (-t17 * t21 + t28 * t
     #21)) * t44 * t46 / 0.720D3 + (t54 * t5 * (-t18 + t57 * t21) + 0.90
     #D2 * t6 * (-t62 * t18 / 0.2D1 - t65 + t62 * t57 * t21 / 0.6D1 + t5
     #7 * t69) - t82 - 0.180D3 * t36 * t5 * (t57 * t18 - t62 * t21 / 0.2
     #D1 - t69)) * t44 / 0.720D3 + t6 * (t98 * t21 - t103 * t21) * t109 
     #/ 0.8D1 - (0.90D2 * t6 * (-t114 * t18 + t116 * t21 / 0.2D1 + t69) 
     #- 0.180D3 * t36 * t5 * (t18 - t114 * t21) + t127) * t107 * t44 / 0
     #.720D3 - ((-0.90D2 * t6 * t18 + 0.180D3 * t36 * t81) * (t141 / 0.2
     #D1 - t144 / 0.2D1) - 0.90D2 * t6 * t21 * (t144 * t143 / 0.6D1 - t1
     #41 * t140 / 0.6D1) + (0.180D3 * t36 * t5 * t18 - t127 - 0.90D2 * t
     #6 * t69) * (-t140 + t143)) * t46 / 0.1440D4 - (t80 - t168 * t53 - 
     #0.90D2 * t171 * lh - 0.15D2 * t175) * t5 * t18 / 0.1440D4 - (t54 +
     # 0.180D3 * t168 * lh + 0.45D2 * t171) * t5 * t69 / 0.1440D4 - (-0.
     #180D3 * t36 - 0.90D2 * t168) * t5 * t65 / 0.1440D4 - t6 * t194 / 0
     #.16D2 - (pi * (t197 + 0.60D2 * t198 + 0.480D3 * lh * zeta3 - 0.60D
     #2 * t51 * t49) - t168 * t79 + t171 * t53 / 0.2D1 + 0.30D2 * t175 *
     # lh + 0.15D2 / 0.4D1 * t211 * pi) * t5 * t21 / 0.1440D4 - (0.90D2 
     #* t6 * (t221 * t18 - t223 * t21 / 0.2D1 - t228 * t18 + t230 * t21 
     #/ 0.2D1) - 0.180D3 * t36 * t5 * (t221 * t21 - t228 * t21)) * t107 
     #* t46 / 0.1440D4 + (t54 * t5 * (-t18 + t248 * t21) + 0.90D2 * t6 *
     # (-t253 * t18 / 0.2D1 - t65 + t253 * t248 * t21 / 0.6D1 + t248 * t
     #69) - t82 - 0.180D3 * t36 * t5 * (t248 * t18 - t253 * t21 / 0.2D1 
     #- t69)) * t107 / 0.1440D4
      t274 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t273)
      t276 = -0.1D1 + x1
      t277 = t2 * t276
      t278 = t2 * x1
      t279 = t276 ** 2
      t280 = t7 * t279
      t281 = t280 * x4
      t284 = log(0.4D1 * t94 * t281)
      t285 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t277, t278, 0.0D0)
      t287 = t284 ** 2
      t288 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t277, t278, 0.0D0)
      t291 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t277, t278, 0.0D0)
      t300 = t5 * t288
      t301 = t54 * t300
      t307 = log(0.4D1 * t94 * t280)
      t312 = t307 ** 2
      t315 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t277, t278, 0.0D0)
      t335 = log(0.4D1 * t246 * t281)
      t344 = t44 * t46
      t350 = log(0.4D1 * t218 * t13 * t7 * t279)
      t352 = t350 ** 2
      t367 = -(0.90D2 * t6 * (t284 * t285 - t287 * t288 / 0.2D1 - t291) 
     #- 0.180D3 * t36 * t5 * (-t285 + t284 * t288) - t301) * t44 * t46 /
     # 0.720D3 + (t54 * t5 * (t285 - t307 * t288) + 0.90D2 * t6 * (t312 
     #* t285 / 0.2D1 + t315 - t312 * t307 * t288 / 0.6D1 - t307 * t291) 
     #+ t80 * t300 - 0.180D3 * t36 * t5 * (-t307 * t285 + t312 * t288 / 
     #0.2D1 + t291)) * t44 / 0.720D3 + (0.90D2 * t6 * (t285 - t335 * t28
     #8) - 0.180D3 * t36 * t300) * t107 * t344 / 0.720D3 - (0.90D2 * t6 
     #* (t350 * t285 - t352 * t288 / 0.2D1 - t291) - 0.180D3 * t36 * t5 
     #* (-t285 + t350 * t288) - t301) * t107 * t44 / 0.720D3
      t368 = FJET(XB1, XB2, s, 0.0D0, -t277, 0.0D0, t278, 0.0D0, t367)
      t370 = -t276
      t371 = KAPPA2(t370, x2, 0.0D0, x4, z)
      t372 = s * t371
      t373 = t1 * t276
      t374 = t372 * t373
      t375 = t1 * x1
      t376 = t375 * x4
      t377 = t372 * t376
      t378 = t375 * t24
      t379 = t372 * t378
      t380 = t371 ** 2
      t382 = t1 ** 2
      t384 = t276 * x1
      t386 = s * t380 * t382 * t384 * x4
      t388 = t380 ** 2
      t390 = t279 * x4 * t24 * t388
      t393 = log(-0.4D1 * t55 * t390)
      t395 = 0.1D1 / (-0.2D1 + t371)
      t396 = t393 * t395
      t397 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t377, -t
     #374, -t379, -t386)
      t399 = t393 ** 2
      t401 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t377, -t
     #374, -t379, -t386)
      t404 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t377, -t
     #374, -t379, -t386)
      t409 = t395 * t397
      t416 = t5 * t395 * t401
      t423 = log(-0.4D1 * t112 * t390)
      t435 = -(-0.90D2 * t6 * (-t396 * t397 + t399 * t395 * t401 / 0.2D1
     # + t395 * t404) + 0.180D3 * t36 * t5 * (t409 - t396 * t401) - t54 
     #* t416) * t44 * t46 / 0.720D3 + (0.90D2 * t6 * (t409 - t423 * t395
     # * t401) - 0.180D3 * t36 * t416) * t107 * t344 / 0.720D3
      t436 = FJET(XB1, XB2, s, 0.0D0, -t374, t377, -t379, -t386, t435)
      t438 = t2 * x3
      t439 = -0.1D1 + x3
      t440 = t2 * t439
      t445 = log(0.4D1 * t100 * t14 * t24 * t439)
      t446 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t438, 0.0D0, -t
     #440, 0.0D0, 0.0D0)
      t448 = t13 * t439
      t449 = t448 * x4
      t452 = log(-0.4D1 * t100 * t449)
      t461 = log(-0.4D1 * t93 * t94 * t439)
      t462 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t438, 0.0D0, -t
     #440, 0.0D0, 0.0D0)
      t464 = t461 ** 2
      t467 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t438, 0.0D0, -t
     #440, 0.0D0, 0.0D0)
      t476 = t5 * t446
      t485 = log(0.4D1 * t246 * t137 * t439)
      t487 = t485 ** 2
      t492 = log(-0.4D1 * t218 * t449)
      t494 = t492 ** 2
      t512 = log(-0.4D1 * t218 * t448)
      t517 = t512 ** 2
      t520 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, t438, 0.0D0, -t
     #440, 0.0D0, 0.0D0)
      t539 = t6 * (t445 * t446 - t452 * t446) * t109 / 0.8D1 - (0.90D2 *
     # t6 * (t461 * t462 - t464 * t446 / 0.2D1 - t467) - 0.180D3 * t36 *
     # t5 * (-t462 + t461 * t446) - t54 * t476) * t107 * t44 / 0.720D3 -
     # (0.90D2 * t6 * (-t485 * t462 + t487 * t446 / 0.2D1 + t492 * t462 
     #- t494 * t446 / 0.2D1) - 0.180D3 * t36 * t5 * (-t485 * t446 + t492
     # * t446)) * t107 * t46 / 0.1440D4 + (t54 * t5 * (t462 - t512 * t44
     #6) + 0.90D2 * t6 * (t517 * t462 / 0.2D1 + t520 - t517 * t512 * t44
     #6 / 0.6D1 - t512 * t467) + t80 * t476 - 0.180D3 * t36 * t5 * (-t51
     #2 * t462 + t517 * t446 / 0.2D1 + t467)) * t107 / 0.1440D4
      t540 = FJET(XB1, XB2, s, t438, -t440, 0.0D0, 0.0D0, 0.0D0, t539)
      t542 = KAPPA2(t370, x2, x3, 0.0D0, z)
      t543 = s * t542
      t544 = t373 * x3
      t545 = t543 * t544
      t546 = t373 * t439
      t547 = t543 * t546
      t548 = t543 * t375
      t549 = t542 ** 2
      t553 = s * t549 * t382 * t384 * x3
      t555 = 0.1D1 / (-0.2D1 + t542)
      t556 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t545, 0.0D0, t
     #547, t548, -t553)
      t557 = t555 * t556
      t558 = t279 * t439
      t559 = t549 ** 2
      t564 = log(-0.4D1 * t112 * t558 * x4 * t559)
      t566 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t545, 0.0D0, t
     #547, t548, -t553)
      t572 = t5 * t555 * t566
      t583 = log(-0.4D1 * t100 * t13 * t279 * t439 * t559)
      t584 = t583 * t555
      t586 = t583 ** 2
      t590 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t545, 0.0D0, t
     #547, t548, -t553)
      t605 = (0.90D2 * t6 * (t557 - t564 * t555 * t566) - 0.180D3 * t36 
     #* t572) * t107 * t344 / 0.720D3 - (-0.90D2 * t6 * (-t584 * t556 + 
     #t586 * t555 * t566 / 0.2D1 + t555 * t590) + 0.180D3 * t36 * t5 * (
     #t557 - t584 * t566) - t54 * t572) * t107 * t44 / 0.720D3
      t606 = FJET(XB1, XB2, s, -t545, t547, 0.0D0, t548, -t553, t605)
      t608 = KAPPA2(t370, x2, x3, x4, z)
      t609 = s * t608
      t610 = t609 * t544
      t611 = t609 * t546
      t612 = t609 * t376
      t613 = t609 * t378
      t614 = t608 ** 2
      t619 = cos(t8)
      t622 = Sqrt(x3 * t439 * t137)
      t627 = s * t614 * t382 * t384 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D
     #1 * t619 * t622)
      t629 = 0.1D1 / (-0.2D1 + t608)
      t630 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t610, t612, t6
     #11, -t613, t627)
      t632 = t614 ** 2
      t637 = log(0.4D1 * t112 * t558 * t137 * t632)
      t639 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t610, t612, t6
     #11, -t613, t627)
      t648 = -0.90D2 * t6 * (t629 * t630 - t637 * t629 * t639) + 0.180D3
     # * t36 * t5 * t629 * t639
      t652 = FJET(XB1, XB2, s, -t610, t611, t612, -t613, t627, t648 * t1
     #07 * t344 / 0.720D3)
      rrgg2gght5s4e1 = t274 * t273 + t368 * t367 + t436 * t435 + t540 * 
     #t539 + t606 * t605 + t652 * t648 * t109 / 0.720D3

      end function



      doubleprecision function rrgg2gght5s4e0
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x1 ** 2
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t20 = -0.1D1 + x4
      t21 = t14 * t20
      t24 = log(-0.4D1 * t11 * t21)
      t27 = 0.1D1 / x1
      t29 = 0.1D1 / x4
      t33 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t34 = x3 * t7
      t35 = t10 * t13
      t38 = log(0.4D1 * t34 * t35)
      t43 = pi * lh
      t44 = t5 * t18
      t46 = 0.180D3 * t43 * t44
      t48 = 0.1D1 / x3
      t52 = t11 * t13
      t54 = log(0.4D1 * t52)
      t56 = t54 ** 2
      t59 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t68 = pi ** 2
      t70 = lh ** 2
      t72 = -0.30D2 * t68 + 0.180D3 * t70
      t73 = pi * t72
      t74 = t73 * t44
      t78 = x4 * t20
      t81 = log(-0.4D1 * t35 * t78)
      t82 = t81 ** 2
      t85 = log(0.4D1 * t35 * x4)
      t86 = t85 ** 2
      t101 = log(0.4D1 * t35)
      t102 = t101 * pi
      t105 = t101 ** 2
      t106 = t105 * pi
      t112 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #2, 0.0D0, 0.0D0)
      t138 = x3 * t10
      t141 = log(-0.4D1 * t138 * t21)
      t145 = log(0.4D1 * t138 * t14)
      t152 = t138 * t13
      t154 = log(0.4D1 * t152)
      t156 = t154 ** 2
      t170 = -t6 * (-t17 * t18 + t24 * t18) * t27 * t29 / 0.8D1 - (0.90D
     #2 * t6 * (t33 - t38 * t18) - t46) * t48 * t27 / 0.720D3 + (0.90D2 
     #* t6 * (t54 * t33 - t56 * t18 / 0.2D1 - t59) - 0.180D3 * t43 * t5 
     #* (-t33 + t54 * t18) - t74) * t27 / 0.720D3 - (-0.90D2 * t6 * t18 
     #* (t82 / 0.2D1 - t86 / 0.2D1) + (-0.90D2 * t6 * t33 + t46) * (-t81
     # + t85)) * t29 / 0.1440D4 - (t73 + 0.180D3 * t102 * lh + 0.45D2 * 
     #t106) * t5 * t33 / 0.1440D4 - t6 * t112 / 0.16D2 - (pi * (-0.240D3
     # * zeta3 - 0.120D3 * t70 * lh + 0.60D2 * lh * t68) - t102 * t72 - 
     #0.90D2 * t106 * lh - 0.15D2 * t105 * t101 * pi) * t5 * t18 / 0.144
     #0D4 - (-0.180D3 * t43 - 0.90D2 * t102) * t5 * t59 / 0.1440D4 - t6 
     #* (t141 * t18 - t145 * t18) * t48 * t29 / 0.16D2 + (0.90D2 * t6 * 
     #(t154 * t33 - t156 * t18 / 0.2D1 - t59) - 0.180D3 * t43 * t5 * (-t
     #33 + t154 * t18) - t74) * t48 / 0.1440D4
      t171 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t170)
      t173 = -0.1D1 + x1
      t174 = t2 * t173
      t175 = t2 * x1
      t176 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t174, t175, 0.0D0)
      t177 = t173 ** 2
      t178 = t7 * t177
      t182 = log(0.4D1 * t35 * t178 * x4)
      t183 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t174, t175, 0.0D0)
      t188 = t5 * t183
      t190 = 0.180D3 * t43 * t188
      t204 = log(0.4D1 * t138 * t13 * t7 * t177)
      t215 = log(0.4D1 * t35 * t178)
      t217 = t215 ** 2
      t220 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t174, t175, 0.0D0)
      t233 = -(0.90D2 * t6 * (-t176 + t182 * t183) + t190) * t27 * t29 /
     # 0.720D3 + t6 * t183 * t48 * t27 * t29 / 0.8D1 - (0.90D2 * t6 * (-
     #t176 + t204 * t183) + t190) * t48 * t27 / 0.720D3 + (0.90D2 * t6 *
     # (-t215 * t176 + t217 * t183 / 0.2D1 + t220) - 0.180D3 * t43 * t5 
     #* (t176 - t215 * t183) + t73 * t188) * t27 / 0.720D3
      t234 = FJET(XB1, XB2, s, 0.0D0, -t174, 0.0D0, t175, 0.0D0, t233)
      t236 = -t173
      t237 = KAPPA2(t236, x2, 0.0D0, x4, z)
      t238 = s * t237
      t239 = t1 * t173
      t240 = t238 * t239
      t241 = t1 * x1
      t242 = t241 * x4
      t243 = t238 * t242
      t244 = t241 * t20
      t245 = t238 * t244
      t246 = t237 ** 2
      t248 = t1 ** 2
      t250 = t173 * x1
      t252 = s * t246 * t248 * t250 * x4
      t254 = 0.1D1 / (-0.2D1 + t237)
      t255 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t243, -t
     #240, -t245, -t252)
      t258 = t246 ** 2
      t263 = log(-0.4D1 * t52 * t177 * x4 * t20 * t258)
      t265 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t243, -t
     #240, -t245, -t252)
      t280 = t27 * t29
      t284 = -(-0.90D2 * t6 * (t254 * t255 - t263 * t254 * t265) + 0.180
     #D3 * t43 * t5 * t254 * t265) * t27 * t29 / 0.720D3 + t6 * t254 * t
     #265 * t48 * t280 / 0.8D1
      t285 = FJET(XB1, XB2, s, 0.0D0, -t240, t243, -t245, -t252, t284)
      t287 = t2 * x3
      t288 = -0.1D1 + x3
      t289 = t2 * t288
      t290 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t287, 0.0D0, -t
     #289, 0.0D0, 0.0D0)
      t294 = log(-0.4D1 * t34 * t35 * t288)
      t295 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t287, 0.0D0, -t
     #289, 0.0D0, 0.0D0)
      t300 = t5 * t295
      t310 = log(0.4D1 * t152 * t78 * t288)
      t312 = t13 * t288
      t316 = log(-0.4D1 * t138 * t312 * x4)
      t325 = log(-0.4D1 * t138 * t312)
      t327 = t325 ** 2
      t330 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t287, 0.0D0, -t
     #289, 0.0D0, 0.0D0)
      t343 = -(0.90D2 * t6 * (-t290 + t294 * t295) + 0.180D3 * t43 * t30
     #0) * t48 * t27 / 0.720D3 - t6 * (-t310 * t295 + t316 * t295) * t48
     # * t29 / 0.16D2 + (0.90D2 * t6 * (-t325 * t290 + t327 * t295 / 0.2
     #D1 + t330) - 0.180D3 * t43 * t5 * (t290 - t325 * t295) + t73 * t30
     #0) * t48 / 0.1440D4
      t344 = FJET(XB1, XB2, s, t287, -t289, 0.0D0, 0.0D0, 0.0D0, t343)
      t346 = KAPPA2(t236, x2, x3, 0.0D0, z)
      t347 = s * t346
      t348 = t239 * x3
      t349 = t347 * t348
      t350 = t239 * t288
      t351 = t347 * t350
      t352 = t347 * t241
      t353 = t346 ** 2
      t357 = s * t353 * t248 * t250 * x3
      t359 = 0.1D1 / (-0.2D1 + t346)
      t361 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t349, 0.0D0, t
     #351, t352, -t357)
      t366 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t349, 0.0D0, t
     #351, t352, -t357)
      t370 = t353 ** 2
      t375 = log(-0.4D1 * t34 * t10 * t13 * t177 * t288 * t370)
      t389 = t6 * t359 * t361 * t48 * t280 / 0.8D1 - (-0.90D2 * t6 * (t3
     #59 * t366 - t375 * t359 * t361) + 0.180D3 * t43 * t5 * t359 * t361
     #) * t48 * t27 / 0.720D3
      t390 = FJET(XB1, XB2, s, -t349, t351, 0.0D0, t352, -t357, t389)
      t392 = KAPPA2(t236, x2, x3, x4, z)
      t393 = s * t392
      t394 = t393 * t348
      t395 = t393 * t350
      t396 = t393 * t242
      t397 = t393 * t244
      t398 = t392 ** 2
      t403 = cos(t8)
      t406 = Sqrt(x3 * t288 * t78)
      t411 = s * t398 * t248 * t250 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D
     #1 * t403 * t406)
      t413 = 0.1D1 / (-0.2D1 + t392)
      t415 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t394, t396, t3
     #95, -t397, t411)
      t417 = t415 * t48 * t280
      t420 = FJET(XB1, XB2, s, -t394, t395, t396, -t397, t411, -t6 * t41
     #3 * t417 / 0.8D1)
      rrgg2gght5s4e0 = t171 * t170 + t234 * t233 + t284 * t285 + t344 * 
     #t343 + t390 * t389 - t420 * pi * t5 * t413 * t417 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s4em1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t8 = 0.1D1 / x3
      t10 = 0.1D1 / x1
      t14 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t15 = x1 ** 2
      t17 = sin(x2 * pi)
      t18 = t17 ** 2
      t20 = z ** 2
      t21 = 0.1D1 / t20
      t24 = log(0.4D1 * t15 * t18 * t21)
      t29 = pi * lh
      t32 = 0.180D3 * t29 * t5 * t7
      t36 = t18 * t21
      t37 = -0.1D1 + x4
      t41 = log(-0.4D1 * t36 * x4 * t37)
      t44 = log(0.4D1 * t36 * x4)
      t47 = 0.1D1 / x4
      t53 = log(0.4D1 * t36)
      t54 = t53 * pi
      t60 = pi ** 2
      t62 = lh ** 2
      t68 = t53 ** 2
      t75 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t78 = x3 * t18
      t81 = log(0.4D1 * t78 * t21)
      t89 = -t6 * t7 * t8 * t10 / 0.8D1 + (0.90D2 * t6 * (-t14 + t24 * t
     #7) + t32) * t10 / 0.720D3 + t6 * t7 * (-t41 + t44) * t47 / 0.16D2 
     #- (-0.180D3 * t29 - 0.90D2 * t54) * t5 * t14 / 0.1440D4 - (pi * (-
     #0.30D2 * t60 + 0.180D3 * t62) + 0.180D3 * t54 * lh + 0.45D2 * t68 
     #* pi) * t5 * t7 / 0.1440D4 - t6 * t75 / 0.16D2 + (0.90D2 * t6 * (-
     #t14 + t81 * t7) + t32) * t8 / 0.1440D4
      t90 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t89)
      t92 = -0.1D1 + x1
      t93 = t2 * t92
      t94 = t2 * x1
      t95 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -t
     #93, t94, 0.0D0)
      t100 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -
     #t93, t94, 0.0D0)
      t101 = t92 ** 2
      t105 = log(0.4D1 * t36 * t15 * t101)
      t120 = t6 * t95 * t8 * t10 / 0.8D1 + (0.90D2 * t6 * (t100 - t105 *
     # t95) - 0.180D3 * t29 * t5 * t95) * t10 / 0.720D3 + t6 * t95 * t10
     # * t47 / 0.8D1
      t121 = FJET(XB1, XB2, s, 0.0D0, -t93, 0.0D0, t94, 0.0D0, t120)
      t123 = -t92
      t124 = KAPPA2(t123, x2, 0.0D0, x4, z)
      t125 = s * t124
      t126 = t1 * t92
      t127 = t125 * t126
      t128 = t1 * x1
      t130 = t125 * t128 * x4
      t132 = t125 * t128 * t37
      t133 = t124 ** 2
      t135 = t1 ** 2
      t137 = t92 * x1
      t139 = s * t133 * t135 * t137 * x4
      t141 = 0.1D1 / (-0.2D1 + t124)
      t143 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t130, -t
     #127, -t132, -t139)
      t148 = FJET(XB1, XB2, s, 0.0D0, -t127, t130, -t132, -t139, t6 * t1
     #41 * t143 * t10 * t47 / 0.8D1)
      t156 = t2 * x3
      t157 = -0.1D1 + x3
      t158 = t2 * t157
      t159 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t156, 0.0D0, -t
     #158, 0.0D0, 0.0D0)
      t164 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t156, 0.0D0, -t
     #158, 0.0D0, 0.0D0)
      t168 = log(-0.4D1 * t78 * t21 * t157)
      t179 = t6 * t159 * t8 * t10 / 0.8D1 + (0.90D2 * t6 * (t164 - t168 
     #* t159) - 0.180D3 * t29 * t5 * t159) * t8 / 0.1440D4
      t180 = FJET(XB1, XB2, s, t156, -t158, 0.0D0, 0.0D0, 0.0D0, t179)
      t182 = KAPPA2(t123, x2, x3, 0.0D0, z)
      t183 = s * t182
      t185 = t183 * t126 * x3
      t187 = t183 * t126 * t157
      t188 = t183 * t128
      t189 = t182 ** 2
      t193 = s * t189 * t135 * t137 * x3
      t195 = 0.1D1 / (-0.2D1 + t182)
      t197 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t185, 0.0D0, t
     #187, t188, -t193)
      t202 = FJET(XB1, XB2, s, -t185, t187, 0.0D0, t188, -t193, t6 * t19
     #5 * t197 * t8 * t10 / 0.8D1)
      rrgg2gght5s4em1 = t90 * t89 + t121 * t120 + t148 * pi * t5 * t141 
     #* t143 * t10 * t47 / 0.8D1 + t180 * t179 + t202 * pi * t5 * t195 *
     # t197 * t8 * t10 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s4em2
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t8 = 0.1D1 / x1
      t12 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2
     #, 0.0D0, 0.0D0)
      t18 = sin(x2 * pi)
      t19 = t18 ** 2
      t20 = z ** 2
      t24 = log(0.4D1 * t19 / t20)
      t31 = 0.1D1 / x3
      t35 = -t6 * t7 * t8 / 0.8D1 - t6 * t12 / 0.16D2 - (-0.180D3 * pi *
     # lh - 0.90D2 * t24 * pi) * t5 * t7 / 0.1440D4 - t6 * t7 * t31 / 0.
     #16D2
      t36 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, t35)
      t39 = t2 * (-0.1D1 + x1)
      t40 = t2 * x1
      t41 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, -t
     #39, t40, 0.0D0)
      t45 = FJET(XB1, XB2, s, 0.0D0, -t39, 0.0D0, t40, 0.0D0, t6 * t41 *
     # t8 / 0.8D1)
      t51 = t2 * x3
      t53 = t2 * (-0.1D1 + x3)
      t54 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t51, 0.0D0, -t53
     #, 0.0D0, 0.0D0)
      t58 = FJET(XB1, XB2, s, t51, -t53, 0.0D0, 0.0D0, 0.0D0, t6 * t54 *
     # t31 / 0.16D2)
      rrgg2gght5s4em2 = t36 * t35 + t45 * pi * t5 * t41 * t8 / 0.8D1 + t
     #58 * pi * t5 * t54 * t31 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s4em3
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t2,
     # 0.0D0, 0.0D0)
      t10 = FJET(XB1, XB2, s, 0.0D0, t2, 0.0D0, 0.0D0, 0.0D0, -pi * t5 *
     # t7 / 0.16D2)
      rrgg2gght5s4em3 = -t10 * pi * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s4em4
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght5s4em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght5s5e1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x1 ** 2
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t20 = t17 ** 2
      t21 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t24 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t28 = pi * lh
      t34 = pi ** 2
      t36 = lh ** 2
      t38 = -0.30D2 * t34 + 0.180D3 * t36
      t39 = pi * t38
      t40 = t5 * t21
      t41 = t39 * t40
      t43 = 0.1D1 / x1
      t45 = 0.1D1 / x4
      t48 = t11 * t13
      t50 = log(0.4D1 * t48)
      t55 = t50 ** 2
      t58 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t71 = -0.240D3 * zeta3 - 0.120D3 * t36 * lh + 0.60D2 * lh * t34
      t72 = pi * t71
      t73 = t72 * t40
      t84 = x3 * t7
      t85 = t84 * t10
      t86 = -0.1D1 + x3
      t87 = t13 * t86
      t88 = t87 * x4
      t91 = log(-0.4D1 * t85 * t88)
      t93 = t10 * t13
      t94 = t93 * x4
      t97 = log(0.4D1 * t84 * t94)
      t101 = 0.1D1 / x3
      t103 = t101 * t43 * t45
      t109 = log(-0.4D1 * t84 * t93 * t86)
      t111 = t109 ** 2
      t114 = t84 * t93
      t116 = log(0.4D1 * t114)
      t118 = t116 ** 2
      t135 = log(0.4D1 * t94)
      t140 = t135 ** 2
      t160 = x3 * t10
      t163 = log(-0.4D1 * t160 * t88)
      t165 = t163 ** 2
      t170 = log(0.4D1 * t160 * t14)
      t172 = t170 ** 2
      t193 = t160 * t13
      t195 = log(0.4D1 * t193)
      t196 = t195 ** 2
      t199 = log(-0.4D1 * t160 * t87)
      t200 = t199 ** 2
      t223 = log(0.4D1 * t93)
      t224 = t223 * pi
      t226 = t223 ** 2
      t227 = t226 * pi
      t231 = t226 * t223 * pi
      t250 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t253 = t34 ** 2
      t254 = t36 ** 2
      t267 = t226 ** 2
      t274 = -(0.90D2 * t6 * (-t17 * t18 + t20 * t21 / 0.2D1 + t24) - 0.
     #180D3 * t28 * t5 * (t18 - t17 * t21) + t41) * t43 * t45 / 0.720D3 
     #- (t39 * t5 * (t18 - t50 * t21) + 0.90D2 * t6 * (t55 * t18 / 0.2D1
     # + t58 - t55 * t50 * t21 / 0.6D1 - t50 * t24) + t73 - 0.180D3 * t2
     #8 * t5 * (-t50 * t18 + t55 * t21 / 0.2D1 + t24)) * t43 / 0.720D3 -
     # t6 * (t91 * t21 - t97 * t21) * t103 / 0.8D1 - (0.90D2 * t6 * (t10
     #9 * t18 - t111 * t21 / 0.2D1 - t116 * t18 + t118 * t21 / 0.2D1) - 
     #0.180D3 * t28 * t5 * (t109 * t21 - t116 * t21)) * t101 * t43 / 0.7
     #20D3 + (t39 * t5 * (-t18 + t135 * t21) + 0.90D2 * t6 * (-t140 * t1
     #8 / 0.2D1 - t58 + t140 * t135 * t21 / 0.6D1 + t135 * t24) - t73 - 
     #0.180D3 * t28 * t5 * (t135 * t18 - t140 * t21 / 0.2D1 - t24)) * t4
     #5 / 0.1440D4 + (0.90D2 * t6 * (-t163 * t18 + t165 * t21 / 0.2D1 + 
     #t170 * t18 - t172 * t21 / 0.2D1) - 0.180D3 * t28 * t5 * (-t163 * t
     #21 + t170 * t21)) * t101 * t45 / 0.1440D4 + ((-0.90D2 * t6 * t18 +
     # 0.180D3 * t28 * t40) * (t196 / 0.2D1 - t200 / 0.2D1) - 0.90D2 * t
     #6 * t21 * (t200 * t199 / 0.6D1 - t196 * t195 / 0.6D1) + (0.180D3 *
     # t28 * t5 * t18 - t41 - 0.90D2 * t6 * t24) * (-t195 + t199)) * t10
     #1 / 0.1440D4 - (t72 - t224 * t38 - 0.90D2 * t227 * lh - 0.15D2 * t
     #231) * t5 * t18 / 0.1440D4 - (t39 + 0.180D3 * t224 * lh + 0.45D2 *
     # t227) * t5 * t24 / 0.1440D4 - (-0.180D3 * t28 - 0.90D2 * t224) * 
     #t5 * t58 / 0.1440D4 - t6 * t250 / 0.16D2 - (pi * (t253 + 0.60D2 * 
     #t254 + 0.480D3 * lh * zeta3 - 0.60D2 * t36 * t34) - t224 * t71 + t
     #227 * t38 / 0.2D1 + 0.30D2 * t231 * lh + 0.15D2 / 0.4D1 * t267 * p
     #i) * t5 * t21 / 0.1440D4
      t275 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t274)
      t277 = -0.1D1 + x4
      t278 = t2 * t277
      t279 = t2 * x4
      t280 = t14 * t277
      t283 = log(-0.4D1 * t11 * t280)
      t284 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t278, 0
     #.0D0, t279, 0.0D0)
      t286 = t283 ** 2
      t287 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t278, 0
     #.0D0, t279, 0.0D0)
      t290 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t278, 0
     #.0D0, t279, 0.0D0)
      t299 = t5 * t287
      t307 = log(-0.4D1 * t85 * t280)
      t313 = log(0.4D1 * t85 * t14 * t277 * t86)
      t319 = x4 * t277
      t322 = log(-0.4D1 * t93 * t319)
      t327 = t322 ** 2
      t330 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t278, 0
     #.0D0, t279, 0.0D0)
      t352 = log(0.4D1 * t193 * t319 * t86)
      t354 = t352 ** 2
      t359 = log(-0.4D1 * t160 * t280)
      t361 = t359 ** 2
      t377 = -(0.90D2 * t6 * (t283 * t284 - t286 * t287 / 0.2D1 - t290) 
     #- 0.180D3 * t28 * t5 * (-t284 + t283 * t287) - t39 * t299) * t43 *
     # t45 / 0.720D3 - t6 * (t307 * t287 - t313 * t287) * t103 / 0.8D1 +
     # (t39 * t5 * (t284 - t322 * t287) + 0.90D2 * t6 * (t327 * t284 / 0
     #.2D1 + t330 - t327 * t322 * t287 / 0.6D1 - t322 * t290) + t72 * t2
     #99 - 0.180D3 * t28 * t5 * (-t322 * t284 + t327 * t287 / 0.2D1 + t2
     #90)) * t45 / 0.1440D4 + (0.90D2 * t6 * (t352 * t284 - t354 * t287 
     #/ 0.2D1 - t359 * t284 + t361 * t287 / 0.2D1) - 0.180D3 * t28 * t5 
     #* (t352 * t287 - t359 * t287)) * t101 * t45 / 0.1440D4
      t378 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t278, t279, 0.0D0, t377)
      t380 = t2 * x1
      t381 = -0.1D1 + x1
      t382 = t2 * t381
      t383 = t381 ** 2
      t384 = t7 * t383
      t385 = t384 * x4
      t388 = log(0.4D1 * t93 * t385)
      t389 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t380, -t382, 0.
     #0D0, 0.0D0, 0.0D0)
      t391 = t388 ** 2
      t392 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t380, -t382, 0.
     #0D0, 0.0D0, 0.0D0)
      t395 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t380, -t382, 0.
     #0D0, 0.0D0, 0.0D0)
      t404 = t5 * t392
      t405 = t39 * t404
      t411 = log(0.4D1 * t93 * t384)
      t416 = t411 ** 2
      t419 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, t380, -t382, 0.
     #0D0, 0.0D0, 0.0D0)
      t439 = log(0.4D1 * t193 * t385)
      t448 = t43 * t45
      t454 = log(0.4D1 * t160 * t13 * t7 * t383)
      t456 = t454 ** 2
      t471 = -(0.90D2 * t6 * (t388 * t389 - t391 * t392 / 0.2D1 - t395) 
     #- 0.180D3 * t28 * t5 * (-t389 + t388 * t392) - t405) * t43 * t45 /
     # 0.720D3 - (-t39 * t5 * (t389 - t411 * t392) - 0.90D2 * t6 * (t416
     # * t389 / 0.2D1 + t419 - t416 * t411 * t392 / 0.6D1 - t411 * t395)
     # - t72 * t404 + 0.180D3 * t28 * t5 * (-t411 * t389 + t416 * t392 /
     # 0.2D1 + t395)) * t43 / 0.720D3 - (0.90D2 * t6 * (-t389 + t439 * t
     #392) + 0.180D3 * t28 * t404) * t101 * t448 / 0.720D3 - (0.90D2 * t
     #6 * (t454 * t389 - t456 * t392 / 0.2D1 - t395) - 0.180D3 * t28 * t
     #5 * (-t389 + t454 * t392) - t405) * t101 * t43 / 0.720D3
      t472 = FJET(XB1, XB2, s, t380, 0.0D0, -t382, 0.0D0, 0.0D0, t471)
      t474 = -t277
      t475 = KAPPA2(x1, x2, 0.10D1, t474, z)
      t476 = s * t475
      t477 = t1 * x1
      t478 = t476 * t477
      t479 = t1 * t381
      t480 = t479 * t277
      t481 = t476 * t480
      t482 = t479 * x4
      t483 = t476 * t482
      t484 = t475 ** 2
      t486 = t1 ** 2
      t488 = x1 * t381
      t490 = s * t484 * t486 * t488 * x4
      t492 = t484 ** 2
      t494 = t383 * x4 * t277 * t492
      t497 = log(-0.4D1 * t48 * t494)
      t499 = 0.1D1 / (-0.2D1 + t475)
      t500 = t497 * t499
      t501 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t478, t481, 0.0
     #D0, -t483, -t490)
      t503 = t497 ** 2
      t505 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t478, t481, 0.0
     #D0, -t483, -t490)
      t508 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t478, t481, 0.0
     #D0, -t483, -t490)
      t513 = t499 * t501
      t520 = t5 * t499 * t505
      t527 = log(-0.4D1 * t114 * t494)
      t539 = -(-0.90D2 * t6 * (-t500 * t501 + t503 * t499 * t505 / 0.2D1
     # + t499 * t508) + 0.180D3 * t28 * t5 * (t513 - t500 * t505) - t39 
     #* t520) * t43 * t45 / 0.720D3 - (0.90D2 * t6 * (-t513 + t527 * t49
     #9 * t505) + 0.180D3 * t28 * t520) * t101 * t448 / 0.720D3
      t540 = FJET(XB1, XB2, s, t478, 0.0D0, t481, -t483, -t490, t539)
      t542 = -t86
      t543 = KAPPA2(x1, x2, t542, 0.10D1, z)
      t544 = s * t543
      t545 = t477 * t86
      t546 = t544 * t545
      t547 = t477 * x3
      t548 = t544 * t547
      t549 = t544 * t479
      t550 = t543 ** 2
      t554 = s * t550 * t486 * t488 * x3
      t556 = 0.1D1 / (-0.2D1 + t543)
      t557 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t546, -t549, t
     #548, 0.0D0, -t554)
      t558 = t556 * t557
      t559 = t383 * t86
      t560 = t550 ** 2
      t565 = log(-0.4D1 * t114 * t559 * x4 * t560)
      t567 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t546, -t549, t
     #548, 0.0D0, -t554)
      t573 = t5 * t556 * t567
      t584 = log(-0.4D1 * t85 * t13 * t383 * t86 * t560)
      t585 = t584 * t556
      t587 = t584 ** 2
      t591 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t546, -t549, t
     #548, 0.0D0, -t554)
      t606 = -(0.90D2 * t6 * (-t558 + t565 * t556 * t567) + 0.180D3 * t2
     #8 * t573) * t101 * t448 / 0.720D3 - (-0.90D2 * t6 * (-t585 * t557 
     #+ t587 * t556 * t567 / 0.2D1 + t556 * t591) + 0.180D3 * t28 * t5 *
     # (t558 - t585 * t567) - t39 * t573) * t101 * t43 / 0.720D3
      t607 = FJET(XB1, XB2, s, -t546, t548, -t549, 0.0D0, -t554, t606)
      t609 = KAPPA2(x1, x2, t542, t474, z)
      t610 = s * t609
      t611 = t610 * t545
      t612 = t610 * t547
      t613 = t610 * t480
      t614 = t610 * t482
      t615 = t609 ** 2
      t620 = cos(t8)
      t623 = Sqrt(x3 * t86 * t319)
      t628 = s * t615 * t486 * t488 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D
     #1 * t620 * t623)
      t630 = 0.1D1 / (-0.2D1 + t609)
      t631 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t611, t613, t6
     #12, -t614, t628)
      t633 = t615 ** 2
      t638 = log(0.4D1 * t114 * t559 * t319 * t633)
      t640 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t611, t613, t6
     #12, -t614, t628)
      t649 = 0.90D2 * t6 * (t630 * t631 - t638 * t630 * t640) - 0.180D3 
     #* t28 * t5 * t630 * t640
      t653 = FJET(XB1, XB2, s, -t611, t612, t613, -t614, t628, -t649 * t
     #101 * t448 / 0.720D3)
      rrgg2gght5s5e1 = t275 * t274 + t378 * t377 + t472 * t471 + t540 * 
     #t539 + t607 * t606 - t653 * t649 * t103 / 0.720D3

      end function



      doubleprecision function rrgg2gght5s5e0
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x2 * pi
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t15 = log(0.4D1 * t12 * x4)
      t16 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t18 = t15 ** 2
      t19 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t22 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t26 = pi * lh
      t32 = pi ** 2
      t34 = lh ** 2
      t36 = -0.30D2 * t32 + 0.180D3 * t34
      t37 = pi * t36
      t38 = t5 * t19
      t39 = t37 * t38
      t41 = 0.1D1 / x4
      t45 = log(0.4D1 * t12)
      t46 = t45 * pi
      t49 = t45 ** 2
      t50 = t49 * pi
      t56 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t82 = x1 ** 2
      t83 = t82 * t9
      t84 = t11 * x4
      t87 = log(0.4D1 * t83 * t84)
      t93 = 0.180D3 * t26 * t38
      t95 = 0.1D1 / x1
      t99 = x3 * t82
      t100 = -0.1D1 + x3
      t104 = log(-0.4D1 * t99 * t12 * t100)
      t108 = log(0.4D1 * t99 * t12)
      t111 = 0.1D1 / x3
      t116 = t83 * t11
      t118 = log(0.4D1 * t116)
      t120 = t118 ** 2
      t134 = x3 * t9
      t135 = t11 * t100
      t139 = log(-0.4D1 * t134 * t135 * x4)
      t143 = log(0.4D1 * t134 * t84)
      t150 = t134 * t11
      t152 = log(0.4D1 * t150)
      t153 = t152 ** 2
      t156 = log(-0.4D1 * t134 * t135)
      t157 = t156 ** 2
      t171 = (0.90D2 * t6 * (t15 * t16 - t18 * t19 / 0.2D1 - t22) - 0.18
     #0D3 * t26 * t5 * (-t16 + t15 * t19) - t39) * t41 / 0.1440D4 - (t37
     # + 0.180D3 * t46 * lh + 0.45D2 * t50) * t5 * t16 / 0.1440D4 - t6 *
     # t56 / 0.16D2 - (pi * (-0.240D3 * zeta3 - 0.120D3 * lh * t34 + 0.6
     #0D2 * lh * t32) - t46 * t36 - 0.90D2 * t50 * lh - 0.15D2 * t49 * t
     #45 * pi) * t5 * t19 / 0.1440D4 - (-0.180D3 * t26 - 0.90D2 * t46) *
     # t5 * t22 / 0.1440D4 - (0.90D2 * t6 * (t16 - t87 * t19) - t93) * t
     #95 * t41 / 0.720D3 - t6 * (t104 * t19 - t108 * t19) * t111 * t95 /
     # 0.8D1 - (0.90D2 * t6 * (-t118 * t16 + t120 * t19 / 0.2D1 + t22) -
     # 0.180D3 * t26 * t5 * (t16 - t118 * t19) + t39) * t95 / 0.720D3 + 
     #t6 * (-t139 * t19 + t143 * t19) * t111 * t41 / 0.16D2 + (-0.90D2 *
     # t6 * t19 * (t153 / 0.2D1 - t157 / 0.2D1) + (-0.90D2 * t6 * t16 + 
     #t93) * (-t152 + t156)) * t111 / 0.1440D4
      t172 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t171)
      t174 = -0.1D1 + x4
      t175 = t2 * t174
      t176 = t2 * x4
      t177 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t175, 0
     #.0D0, t176, 0.0D0)
      t178 = t84 * t174
      t181 = log(-0.4D1 * t83 * t178)
      t182 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t175, 0
     #.0D0, t176, 0.0D0)
      t187 = t5 * t182
      t194 = x4 * t174
      t198 = log(0.4D1 * t150 * t194 * t100)
      t202 = log(-0.4D1 * t134 * t178)
      t211 = log(-0.4D1 * t12 * t194)
      t213 = t211 ** 2
      t216 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t175, 0
     #.0D0, t176, 0.0D0)
      t229 = -(0.90D2 * t6 * (-t177 + t181 * t182) + 0.180D3 * t26 * t18
     #7) * t95 * t41 / 0.720D3 + t6 * (t198 * t182 - t202 * t182) * t111
     # * t41 / 0.16D2 + (0.90D2 * t6 * (-t211 * t177 + t213 * t182 / 0.2
     #D1 + t216) - 0.180D3 * t26 * t5 * (t177 - t211 * t182) + t37 * t18
     #7) * t41 / 0.1440D4
      t230 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t175, t176, 0.0D0, t229)
      t232 = t2 * x1
      t233 = -0.1D1 + x1
      t234 = t2 * t233
      t235 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t232, -t234, 0.
     #0D0, 0.0D0, 0.0D0)
      t236 = t233 ** 2
      t237 = t82 * t236
      t241 = log(0.4D1 * t12 * t237 * x4)
      t242 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t232, -t234, 0.
     #0D0, 0.0D0, 0.0D0)
      t247 = t5 * t242
      t249 = 0.180D3 * t26 * t247
      t263 = log(0.4D1 * t134 * t11 * t82 * t236)
      t274 = log(0.4D1 * t12 * t237)
      t276 = t274 ** 2
      t279 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t232, -t234, 0.
     #0D0, 0.0D0, 0.0D0)
      t292 = -(0.90D2 * t6 * (-t235 + t241 * t242) + t249) * t95 * t41 /
     # 0.720D3 + t6 * t242 * t111 * t95 * t41 / 0.8D1 - (0.90D2 * t6 * (
     #-t235 + t263 * t242) + t249) * t111 * t95 / 0.720D3 - (-0.90D2 * t
     #6 * (-t274 * t235 + t276 * t242 / 0.2D1 + t279) + 0.180D3 * t26 * 
     #t5 * (t235 - t274 * t242) - t37 * t247) * t95 / 0.720D3
      t293 = FJET(XB1, XB2, s, t232, 0.0D0, -t234, 0.0D0, 0.0D0, t292)
      t295 = -t174
      t296 = KAPPA2(x1, x2, 0.10D1, t295, z)
      t297 = s * t296
      t298 = t1 * x1
      t299 = t297 * t298
      t300 = t1 * t233
      t301 = t300 * t174
      t302 = t297 * t301
      t303 = t300 * x4
      t304 = t297 * t303
      t305 = t296 ** 2
      t307 = t1 ** 2
      t309 = x1 * t233
      t311 = s * t305 * t307 * t309 * x4
      t313 = 0.1D1 / (-0.2D1 + t296)
      t314 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t299, t302, 0.0
     #D0, -t304, -t311)
      t317 = t305 ** 2
      t322 = log(-0.4D1 * t116 * t236 * x4 * t174 * t317)
      t324 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t299, t302, 0.0
     #D0, -t304, -t311)
      t339 = t95 * t41
      t343 = -(-0.90D2 * t6 * (t313 * t314 - t322 * t313 * t324) + 0.180
     #D3 * t26 * t5 * t313 * t324) * t95 * t41 / 0.720D3 + t6 * t313 * t
     #324 * t111 * t339 / 0.8D1
      t344 = FJET(XB1, XB2, s, t299, 0.0D0, t302, -t304, -t311, t343)
      t346 = -t100
      t347 = KAPPA2(x1, x2, t346, 0.10D1, z)
      t348 = s * t347
      t349 = t298 * t100
      t350 = t348 * t349
      t351 = t298 * x3
      t352 = t348 * t351
      t353 = t348 * t300
      t354 = t347 ** 2
      t358 = s * t354 * t307 * t309 * x3
      t360 = 0.1D1 / (-0.2D1 + t347)
      t362 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t350, -t353, t
     #352, 0.0D0, -t358)
      t367 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t350, -t353, t
     #352, 0.0D0, -t358)
      t371 = t354 ** 2
      t376 = log(-0.4D1 * t99 * t9 * t11 * t236 * t100 * t371)
      t390 = t6 * t360 * t362 * t111 * t339 / 0.8D1 - (-0.90D2 * t6 * (t
     #360 * t367 - t376 * t360 * t362) + 0.180D3 * t26 * t5 * t360 * t36
     #2) * t111 * t95 / 0.720D3
      t391 = FJET(XB1, XB2, s, -t350, t352, -t353, 0.0D0, -t358, t390)
      t393 = KAPPA2(x1, x2, t346, t295, z)
      t394 = s * t393
      t395 = t394 * t349
      t396 = t394 * t351
      t397 = t394 * t301
      t398 = t394 * t303
      t399 = t393 ** 2
      t404 = cos(t7)
      t407 = Sqrt(x3 * t100 * t194)
      t412 = s * t399 * t307 * t309 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D
     #1 * t404 * t407)
      t414 = 0.1D1 / (-0.2D1 + t393)
      t416 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t395, t397, t3
     #96, -t398, t412)
      t418 = t416 * t111 * t339
      t421 = FJET(XB1, XB2, s, -t395, t396, t397, -t398, t412, -t6 * t41
     #4 * t418 / 0.8D1)
      rrgg2gght5s5e0 = t172 * t171 + t230 * t229 + t293 * t292 + t344 * 
     #t343 + t391 * t390 - t421 * pi * t5 * t414 * t418 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s5em1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = sin(x2 * pi)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t16 = log(0.4D1 * t13 * x4)
      t17 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t22 = pi * lh
      t25 = 0.180D3 * t22 * t5 * t17
      t27 = 0.1D1 / x4
      t30 = x1 ** 2
      t34 = log(0.4D1 * t30 * t10 * t12)
      t40 = 0.1D1 / x1
      t47 = x3 * t10
      t50 = log(0.4D1 * t47 * t12)
      t51 = -0.1D1 + x3
      t55 = log(-0.4D1 * t47 * t12 * t51)
      t58 = 0.1D1 / x3
      t64 = log(0.4D1 * t13)
      t65 = t64 * pi
      t71 = pi ** 2
      t73 = lh ** 2
      t79 = t64 ** 2
      t86 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t89 = (0.90D2 * t6 * (-t7 + t16 * t17) + t25) * t27 / 0.1440D4 - (
     #0.90D2 * t6 * (t7 - t34 * t17) - t25) * t40 / 0.720D3 - t6 * t17 *
     # t40 * t27 / 0.8D1 - t6 * t17 * (-t50 + t55) * t58 / 0.16D2 - (-0.
     #180D3 * t22 - 0.90D2 * t65) * t5 * t7 / 0.1440D4 - (pi * (-0.30D2 
     #* t71 + 0.180D3 * t73) + 0.180D3 * t65 * lh + 0.45D2 * t79 * pi) *
     # t5 * t17 / 0.1440D4 - t6 * t86 / 0.16D2
      t90 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t89)
      t92 = -0.1D1 + x4
      t93 = t2 * t92
      t94 = t2 * x4
      t95 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t93, 0.0
     #D0, t94, 0.0D0)
      t99 = log(-0.4D1 * t13 * x4 * t92)
      t100 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t93, 0.
     #0D0, t94, 0.0D0)
      t115 = (0.90D2 * t6 * (t95 - t99 * t100) - 0.180D3 * t22 * t5 * t1
     #00) * t27 / 0.1440D4 + t6 * t100 * t40 * t27 / 0.8D1
      t116 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t93, t94, 0.0D0, t115)
      t118 = t2 * x1
      t119 = -0.1D1 + x1
      t120 = t2 * t119
      t121 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t118, -t120, 0.
     #0D0, 0.0D0, 0.0D0)
      t126 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t118, -t120, 0.
     #0D0, 0.0D0, 0.0D0)
      t127 = t119 ** 2
      t131 = log(0.4D1 * t13 * t30 * t127)
      t146 = t6 * t121 * t58 * t40 / 0.8D1 - (-0.90D2 * t6 * (t126 - t13
     #1 * t121) + 0.180D3 * t22 * t5 * t121) * t40 / 0.720D3 + t6 * t121
     # * t40 * t27 / 0.8D1
      t147 = FJET(XB1, XB2, s, t118, 0.0D0, -t120, 0.0D0, 0.0D0, t146)
      t150 = KAPPA2(x1, x2, 0.10D1, -t92, z)
      t151 = s * t150
      t152 = t1 * x1
      t153 = t151 * t152
      t154 = t1 * t119
      t156 = t151 * t154 * t92
      t158 = t151 * t154 * x4
      t159 = t150 ** 2
      t161 = t1 ** 2
      t163 = x1 * t119
      t165 = s * t159 * t161 * t163 * x4
      t167 = 0.1D1 / (-0.2D1 + t150)
      t169 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t153, t156, 0.0
     #D0, -t158, -t165)
      t174 = FJET(XB1, XB2, s, t153, 0.0D0, t156, -t158, -t165, t6 * t16
     #7 * t169 * t40 * t27 / 0.8D1)
      t183 = KAPPA2(x1, x2, -t51, 0.10D1, z)
      t184 = s * t183
      t186 = t184 * t152 * t51
      t188 = t184 * t152 * x3
      t189 = t184 * t154
      t190 = t183 ** 2
      t194 = s * t190 * t161 * t163 * x3
      t196 = 0.1D1 / (-0.2D1 + t183)
      t198 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t186, -t189, t
     #188, 0.0D0, -t194)
      t203 = FJET(XB1, XB2, s, -t186, t188, -t189, 0.0D0, -t194, t6 * t1
     #96 * t198 * t58 * t40 / 0.8D1)
      rrgg2gght5s5em1 = t90 * t89 + t116 * t115 + t147 * t146 + t174 * p
     #i * t5 * t167 * t169 * t40 * t27 / 0.8D1 + t203 * pi * t5 * t196 *
     # t198 * t58 * t40 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s5em2
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = 0.1D1 / x1
      t12 = 0.1D1 / x4
      t16 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t22 = sin(x2 * pi)
      t23 = t22 ** 2
      t24 = z ** 2
      t28 = log(0.4D1 * t23 / t24)
      t35 = -t6 * t7 * t8 / 0.8D1 - t6 * t7 * t12 / 0.16D2 - t6 * t16 / 
     #0.16D2 - (-0.180D3 * pi * lh - 0.90D2 * t28 * pi) * t5 * t7 / 0.14
     #40D4
      t36 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t35)
      t38 = t2 * x1
      t40 = t2 * (-0.1D1 + x1)
      t41 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t38, -t40, 0.0D0
     #, 0.0D0, 0.0D0)
      t45 = FJET(XB1, XB2, s, t38, 0.0D0, -t40, 0.0D0, 0.0D0, t6 * t41 *
     # t8 / 0.8D1)
      t52 = t2 * (-0.1D1 + x4)
      t53 = t2 * x4
      t54 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t52, 0.0
     #D0, t53, 0.0D0)
      t58 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t52, t53, 0.0D0, t6 * t54 *
     # t12 / 0.16D2)
      rrgg2gght5s5em2 = t36 * t35 + t45 * pi * t5 * t41 * t8 / 0.8D1 + t
     #58 * pi * t5 * t54 * t12 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s5em3
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -pi * t5 *
     # t7 / 0.16D2)
      rrgg2gght5s5em3 = -t10 * pi * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s5em4
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght5s5em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght5s6e1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x1 ** 2
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t20 = t17 ** 2
      t21 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t24 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t28 = pi * lh
      t34 = pi ** 2
      t36 = lh ** 2
      t38 = -0.30D2 * t34 + 0.180D3 * t36
      t39 = pi * t38
      t40 = t5 * t21
      t41 = t39 * t40
      t43 = 0.1D1 / x1
      t45 = 0.1D1 / x4
      t48 = t11 * t13
      t50 = log(0.4D1 * t48)
      t55 = t50 ** 2
      t58 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t71 = -0.240D3 * zeta3 - 0.120D3 * t36 * lh + 0.60D2 * lh * t34
      t72 = pi * t71
      t73 = t72 * t40
      t84 = x3 * t7
      t85 = t10 * t13
      t86 = t85 * x4
      t89 = log(0.4D1 * t84 * t86)
      t91 = t84 * t10
      t92 = -0.1D1 + x3
      t93 = t13 * t92
      t94 = t93 * x4
      t97 = log(-0.4D1 * t91 * t94)
      t101 = 0.1D1 / x3
      t103 = t101 * t43 * t45
      t109 = log(-0.4D1 * t84 * t85 * t92)
      t111 = t109 ** 2
      t114 = t84 * t85
      t116 = log(0.4D1 * t114)
      t118 = t116 ** 2
      t135 = log(0.4D1 * t86)
      t140 = t135 ** 2
      t160 = x3 * t10
      t163 = log(0.4D1 * t160 * t14)
      t165 = t163 ** 2
      t170 = log(-0.4D1 * t160 * t94)
      t172 = t170 ** 2
      t193 = t160 * t13
      t195 = log(0.4D1 * t193)
      t196 = t195 ** 2
      t199 = log(-0.4D1 * t160 * t93)
      t200 = t199 ** 2
      t223 = log(0.4D1 * t85)
      t224 = t223 * pi
      t226 = t223 ** 2
      t227 = t226 * pi
      t231 = t226 * t223 * pi
      t250 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t253 = t34 ** 2
      t254 = t36 ** 2
      t267 = t226 ** 2
      t274 = (0.90D2 * t6 * (t17 * t18 - t20 * t21 / 0.2D1 - t24) - 0.18
     #0D3 * t28 * t5 * (-t18 + t17 * t21) - t41) * t43 * t45 / 0.720D3 +
     # (t39 * t5 * (-t18 + t50 * t21) + 0.90D2 * t6 * (-t55 * t18 / 0.2D
     #1 - t58 + t55 * t50 * t21 / 0.6D1 + t50 * t24) - t73 - 0.180D3 * t
     #28 * t5 * (t50 * t18 - t55 * t21 / 0.2D1 - t24)) * t43 / 0.720D3 +
     # t6 * (t89 * t21 - t97 * t21) * t103 / 0.8D1 + (0.90D2 * t6 * (-t1
     #09 * t18 + t111 * t21 / 0.2D1 + t116 * t18 - t118 * t21 / 0.2D1) -
     # 0.180D3 * t28 * t5 * (-t109 * t21 + t116 * t21)) * t101 * t43 / 0
     #.720D3 - (t39 * t5 * (t18 - t135 * t21) + 0.90D2 * t6 * (t140 * t1
     #8 / 0.2D1 + t58 - t140 * t135 * t21 / 0.6D1 - t135 * t24) + t73 - 
     #0.180D3 * t28 * t5 * (-t135 * t18 + t140 * t21 / 0.2D1 + t24)) * t
     #45 / 0.1440D4 - (0.90D2 * t6 * (-t163 * t18 + t165 * t21 / 0.2D1 +
     # t170 * t18 - t172 * t21 / 0.2D1) - 0.180D3 * t28 * t5 * (-t163 * 
     #t21 + t170 * t21)) * t101 * t45 / 0.1440D4 + ((-0.90D2 * t6 * t18 
     #+ 0.180D3 * t28 * t40) * (t196 / 0.2D1 - t200 / 0.2D1) - 0.90D2 * 
     #t6 * t21 * (t200 * t199 / 0.6D1 - t196 * t195 / 0.6D1) + (0.180D3 
     #* t28 * t5 * t18 - t41 - 0.90D2 * t6 * t24) * (-t195 + t199)) * t1
     #01 / 0.1440D4 - (t72 - t224 * t38 - 0.90D2 * t227 * lh - 0.15D2 * 
     #t231) * t5 * t18 / 0.1440D4 - (t39 + 0.180D3 * t224 * lh + 0.45D2 
     #* t227) * t5 * t24 / 0.1440D4 - (-0.180D3 * t28 - 0.90D2 * t224) *
     # t5 * t58 / 0.1440D4 - t6 * t250 / 0.16D2 - (pi * (t253 + 0.60D2 *
     # t254 + 0.480D3 * lh * zeta3 - 0.60D2 * t36 * t34) - t224 * t71 + 
     #t227 * t38 / 0.2D1 + 0.30D2 * t231 * lh + 0.15D2 / 0.4D1 * t267 * 
     #pi) * t5 * t21 / 0.1440D4
      t275 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t274)
      t277 = t2 * x4
      t278 = -0.1D1 + x4
      t279 = t2 * t278
      t280 = t14 * t278
      t283 = log(-0.4D1 * t11 * t280)
      t284 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t277, 0.
     #0D0, -t279, 0.0D0)
      t286 = t283 ** 2
      t287 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t277, 0.
     #0D0, -t279, 0.0D0)
      t290 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t277, 0.
     #0D0, -t279, 0.0D0)
      t299 = t5 * t287
      t307 = log(-0.4D1 * t91 * t280)
      t313 = log(0.4D1 * t91 * t14 * t278 * t92)
      t319 = x4 * t278
      t322 = log(-0.4D1 * t85 * t319)
      t327 = t322 ** 2
      t330 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t277, 0.
     #0D0, -t279, 0.0D0)
      t352 = log(0.4D1 * t193 * t319 * t92)
      t354 = t352 ** 2
      t359 = log(-0.4D1 * t160 * t280)
      t361 = t359 ** 2
      t377 = (0.90D2 * t6 * (-t283 * t284 + t286 * t287 / 0.2D1 + t290) 
     #- 0.180D3 * t28 * t5 * (t284 - t283 * t287) + t39 * t299) * t43 * 
     #t45 / 0.720D3 + t6 * (-t307 * t287 + t313 * t287) * t103 / 0.8D1 -
     # (t39 * t5 * (-t284 + t322 * t287) + 0.90D2 * t6 * (-t327 * t284 /
     # 0.2D1 - t330 + t327 * t322 * t287 / 0.6D1 + t322 * t290) - t72 * 
     #t299 - 0.180D3 * t28 * t5 * (t322 * t284 - t327 * t287 / 0.2D1 - t
     #290)) * t45 / 0.1440D4 - (0.90D2 * t6 * (-t352 * t284 + t354 * t28
     #7 / 0.2D1 + t359 * t284 - t361 * t287 / 0.2D1) - 0.180D3 * t28 * t
     #5 * (-t352 * t287 + t359 * t287)) * t101 * t45 / 0.1440D4
      t378 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t277, -t279, 0.0D0, t377)
      t380 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t381 = s * t380
      t382 = t1 * x1
      t383 = t381 * t382
      t384 = -0.1D1 + x1
      t385 = t1 * t384
      t386 = t381 * t385
      t387 = t380 ** 2
      t389 = t1 ** 2
      t392 = s * t387 * t389 * x1 * t384
      t393 = t384 ** 2
      t395 = t387 ** 2
      t399 = log(0.4D1 * t48 * t393 * x4 * t395)
      t401 = 0.1D1 / (-0.2D1 + t380)
      t402 = t399 * t401
      t403 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t383, 0.0D0, 0.
     #0D0, -t386, -t392)
      t405 = t399 ** 2
      t407 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t383, 0.0D0, 0.
     #0D0, -t386, -t392)
      t410 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t383, 0.0D0, 0.
     #0D0, -t386, -t392)
      t411 = t401 * t410
      t415 = t401 * t403
      t422 = t5 * t401 * t407
      t423 = t39 * t422
      t427 = t13 * t393
      t428 = t427 * t395
      t431 = log(0.4D1 * t11 * t428)
      t432 = t431 * t401
      t437 = t431 ** 2
      t438 = t437 * t401
      t441 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, t383, 0.0D0, 0.
     #0D0, -t386, -t392)
      t465 = log(0.4D1 * t91 * t427 * x4 * t395)
      t475 = t43 * t45
      t479 = log(0.4D1 * t91 * t428)
      t480 = t479 * t401
      t482 = t479 ** 2
      t498 = (0.90D2 * t6 * (t402 * t403 - t405 * t401 * t407 / 0.2D1 - 
     #t411) - 0.180D3 * t28 * t5 * (-t415 + t402 * t407) - t423) * t43 *
     # t45 / 0.720D3 + (-t39 * t5 * (t415 - t432 * t407) - 0.90D2 * t6 *
     # (t438 * t403 / 0.2D1 + t401 * t441 - t437 * t431 * t401 * t407 / 
     #0.6D1 - t432 * t410) - t72 * t422 + 0.180D3 * t28 * t5 * (-t432 * 
     #t403 + t438 * t407 / 0.2D1 + t411)) * t43 / 0.720D3 + (0.90D2 * t6
     # * (-t415 + t465 * t401 * t407) + 0.180D3 * t28 * t422) * t101 * t
     #475 / 0.720D3 + (0.90D2 * t6 * (t480 * t403 - t482 * t401 * t407 /
     # 0.2D1 - t411) - 0.180D3 * t28 * t5 * (-t415 + t480 * t407) - t423
     #) * t101 * t43 / 0.720D3
      t499 = FJET(XB1, XB2, s, t383, 0.0D0, 0.0D0, -t386, -t392, t498)
      t501 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t502 = s * t501
      t503 = t502 * t382
      t504 = t385 * x4
      t505 = t502 * t504
      t506 = t385 * t278
      t507 = t502 * t506
      t508 = t501 ** 2
      t511 = x1 * t384
      t513 = s * t508 * t389 * t511 * t278
      t514 = t508 ** 2
      t516 = t319 * t514 * t393
      t519 = log(-0.4D1 * t48 * t516)
      t521 = 0.1D1 / (-0.2D1 + t501)
      t522 = t519 * t521
      t523 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t503, -t505, 0.
     #0D0, t507, t513)
      t525 = t519 ** 2
      t527 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t503, -t505, 0.
     #0D0, t507, t513)
      t530 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t503, -t505, 0.
     #0D0, t507, t513)
      t535 = t521 * t523
      t542 = t5 * t521 * t527
      t549 = log(-0.4D1 * t114 * t516)
      t561 = (0.90D2 * t6 * (-t522 * t523 + t525 * t521 * t527 / 0.2D1 +
     # t521 * t530) - 0.180D3 * t28 * t5 * (t535 - t522 * t527) + t39 * 
     #t542) * t43 * t45 / 0.720D3 + (0.90D2 * t6 * (t535 - t549 * t521 *
     # t527) - 0.180D3 * t28 * t542) * t101 * t475 / 0.720D3
      t562 = FJET(XB1, XB2, s, t503, 0.0D0, -t505, t507, t513, t561)
      t564 = -t92
      t565 = KAPPA2(x1, x2, t564, 0.0D0, z)
      t566 = s * t565
      t567 = t382 * t92
      t568 = t566 * t567
      t569 = t382 * x3
      t570 = t566 * t569
      t571 = t566 * t385
      t572 = t565 ** 2
      t576 = s * t572 * t389 * t511 * t92
      t578 = 0.1D1 / (-0.2D1 + t565)
      t579 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t568, 0.0D0, t
     #570, -t571, t576)
      t580 = t578 * t579
      t581 = t393 * t92
      t582 = t572 ** 2
      t587 = log(-0.4D1 * t114 * t581 * x4 * t582)
      t589 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t568, 0.0D0, t
     #570, -t571, t576)
      t595 = t5 * t578 * t589
      t605 = log(-0.4D1 * t91 * t427 * t92 * t582)
      t606 = t605 * t578
      t608 = t605 ** 2
      t612 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, -t568, 0.0D0, t
     #570, -t571, t576)
      t627 = (0.90D2 * t6 * (t580 - t587 * t578 * t589) - 0.180D3 * t28 
     #* t595) * t101 * t475 / 0.720D3 + (-0.90D2 * t6 * (t606 * t579 - t
     #608 * t578 * t589 / 0.2D1 - t578 * t612) + 0.180D3 * t28 * t5 * (-
     #t580 + t606 * t589) + t39 * t595) * t101 * t43 / 0.720D3
      t628 = FJET(XB1, XB2, s, -t568, t570, 0.0D0, -t571, t576, t627)
      t630 = KAPPA2(x1, x2, t564, x4, z)
      t631 = s * t630
      t632 = t631 * t567
      t633 = t631 * t569
      t634 = t631 * t504
      t635 = t631 * t506
      t636 = t630 ** 2
      t641 = cos(t8)
      t644 = Sqrt(x3 * t92 * t319)
      t649 = s * t636 * t389 * t511 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x
     #4 + 0.2D1 * t641 * t644)
      t651 = 0.1D1 / (-0.2D1 + t630)
      t652 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t632, -t634, t
     #633, t635, t649)
      t654 = t636 ** 2
      t659 = log(0.4D1 * t114 * t319 * t581 * t654)
      t661 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t632, -t634, t
     #633, t635, t649)
      t670 = -0.90D2 * t6 * (t651 * t652 - t659 * t651 * t661) + 0.180D3
     # * t28 * t5 * t651 * t661
      t674 = FJET(XB1, XB2, s, -t632, t633, -t634, t635, t649, t670 * t1
     #01 * t475 / 0.720D3)
      rrgg2gght5s6e1 = t275 * t274 + t378 * t377 + t499 * t498 + t562 * 
     #t561 + t628 * t627 + t674 * t670 * t103 / 0.720D3

      end function



      doubleprecision function rrgg2gght5s6e0
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x2 * pi
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t15 = log(0.4D1 * t12 * x4)
      t16 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t18 = t15 ** 2
      t19 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t22 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t26 = pi * lh
      t32 = pi ** 2
      t34 = lh ** 2
      t36 = -0.30D2 * t32 + 0.180D3 * t34
      t37 = pi * t36
      t38 = t5 * t19
      t39 = t37 * t38
      t41 = 0.1D1 / x4
      t45 = log(0.4D1 * t12)
      t46 = t45 * pi
      t49 = t45 ** 2
      t50 = t49 * pi
      t56 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t82 = x1 ** 2
      t83 = t82 * t9
      t84 = t11 * x4
      t87 = log(0.4D1 * t83 * t84)
      t93 = 0.180D3 * t26 * t38
      t95 = 0.1D1 / x1
      t99 = x3 * t82
      t100 = -0.1D1 + x3
      t104 = log(-0.4D1 * t99 * t12 * t100)
      t108 = log(0.4D1 * t99 * t12)
      t111 = 0.1D1 / x3
      t116 = t83 * t11
      t118 = log(0.4D1 * t116)
      t120 = t118 ** 2
      t134 = x3 * t9
      t137 = log(0.4D1 * t134 * t84)
      t139 = t11 * t100
      t143 = log(-0.4D1 * t134 * t139 * x4)
      t150 = t134 * t11
      t152 = log(0.4D1 * t150)
      t153 = t152 ** 2
      t156 = log(-0.4D1 * t134 * t139)
      t157 = t156 ** 2
      t171 = -(0.90D2 * t6 * (-t15 * t16 + t18 * t19 / 0.2D1 + t22) - 0.
     #180D3 * t26 * t5 * (t16 - t15 * t19) + t39) * t41 / 0.1440D4 - (t3
     #7 + 0.180D3 * t46 * lh + 0.45D2 * t50) * t5 * t16 / 0.1440D4 - t6 
     #* t56 / 0.16D2 - (pi * (-0.240D3 * zeta3 - 0.120D3 * lh * t34 + 0.
     #60D2 * lh * t32) - t46 * t36 - 0.90D2 * t50 * lh - 0.15D2 * t49 * 
     #t45 * pi) * t5 * t19 / 0.1440D4 - (-0.180D3 * t26 - 0.90D2 * t46) 
     #* t5 * t22 / 0.1440D4 + (0.90D2 * t6 * (-t16 + t87 * t19) + t93) *
     # t95 * t41 / 0.720D3 + t6 * (-t104 * t19 + t108 * t19) * t111 * t9
     #5 / 0.8D1 + (0.90D2 * t6 * (t118 * t16 - t120 * t19 / 0.2D1 - t22)
     # - 0.180D3 * t26 * t5 * (-t16 + t118 * t19) - t39) * t95 / 0.720D3
     # - t6 * (-t137 * t19 + t143 * t19) * t111 * t41 / 0.16D2 + (-0.90D
     #2 * t6 * t19 * (t153 / 0.2D1 - t157 / 0.2D1) + (-0.90D2 * t6 * t16
     # + t93) * (-t152 + t156)) * t111 / 0.1440D4
      t172 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t171)
      t174 = t2 * x4
      t175 = -0.1D1 + x4
      t176 = t2 * t175
      t177 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t174, 0.
     #0D0, -t176, 0.0D0)
      t178 = t84 * t175
      t181 = log(-0.4D1 * t83 * t178)
      t182 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t174, 0.
     #0D0, -t176, 0.0D0)
      t187 = t5 * t182
      t194 = x4 * t175
      t198 = log(0.4D1 * t150 * t194 * t100)
      t202 = log(-0.4D1 * t134 * t178)
      t211 = log(-0.4D1 * t12 * t194)
      t213 = t211 ** 2
      t216 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t174, 0.
     #0D0, -t176, 0.0D0)
      t229 = (0.90D2 * t6 * (t177 - t181 * t182) - 0.180D3 * t26 * t187)
     # * t95 * t41 / 0.720D3 - t6 * (-t198 * t182 + t202 * t182) * t111 
     #* t41 / 0.16D2 - (0.90D2 * t6 * (t211 * t177 - t213 * t182 / 0.2D1
     # - t216) - 0.180D3 * t26 * t5 * (-t177 + t211 * t182) - t37 * t187
     #) * t41 / 0.1440D4
      t230 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t174, -t176, 0.0D0, t229)
      t232 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t233 = s * t232
      t234 = t1 * x1
      t235 = t233 * t234
      t236 = -0.1D1 + x1
      t237 = t1 * t236
      t238 = t233 * t237
      t239 = t232 ** 2
      t241 = t1 ** 2
      t244 = s * t239 * t241 * x1 * t236
      t246 = 0.1D1 / (-0.2D1 + t232)
      t247 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t235, 0.0D0, 0.
     #0D0, -t238, -t244)
      t248 = t246 * t247
      t249 = t236 ** 2
      t251 = t239 ** 2
      t255 = log(0.4D1 * t116 * t249 * x4 * t251)
      t257 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t235, 0.0D0, 0.
     #0D0, -t238, -t244)
      t263 = t5 * t246 * t257
      t265 = 0.180D3 * t26 * t263
      t272 = t95 * t41
      t276 = t99 * t9
      t277 = t11 * t249
      t278 = t277 * t251
      t281 = log(0.4D1 * t276 * t278)
      t293 = log(0.4D1 * t83 * t278)
      t294 = t293 * t246
      t296 = t293 ** 2
      t300 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t235, 0.0D0, 0.
     #0D0, -t238, -t244)
      t314 = (0.90D2 * t6 * (-t248 + t255 * t246 * t257) + t265) * t95 *
     # t41 / 0.720D3 - t6 * t246 * t257 * t111 * t272 / 0.8D1 + (0.90D2 
     #* t6 * (-t248 + t281 * t246 * t257) + t265) * t111 * t95 / 0.720D3
     # + (-0.90D2 * t6 * (-t294 * t247 + t296 * t246 * t257 / 0.2D1 + t2
     #46 * t300) + 0.180D3 * t26 * t5 * (t248 - t294 * t257) - t37 * t26
     #3) * t95 / 0.720D3
      t315 = FJET(XB1, XB2, s, t235, 0.0D0, 0.0D0, -t238, -t244, t314)
      t317 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t318 = s * t317
      t319 = t318 * t234
      t320 = t237 * x4
      t321 = t318 * t320
      t322 = t237 * t175
      t323 = t318 * t322
      t324 = t317 ** 2
      t327 = x1 * t236
      t329 = s * t324 * t241 * t327 * t175
      t331 = 0.1D1 / (-0.2D1 + t317)
      t332 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t319, -t321, 0.
     #0D0, t323, t329)
      t334 = t324 ** 2
      t339 = log(-0.4D1 * t116 * t194 * t334 * t249)
      t341 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t319, -t321, 0.
     #0D0, t323, t329)
      t359 = (0.90D2 * t6 * (t331 * t332 - t339 * t331 * t341) - 0.180D3
     # * t26 * t5 * t331 * t341) * t95 * t41 / 0.720D3 + t6 * t331 * t34
     #1 * t111 * t272 / 0.8D1
      t360 = FJET(XB1, XB2, s, t319, 0.0D0, -t321, t323, t329, t359)
      t362 = -t100
      t363 = KAPPA2(x1, x2, t362, 0.0D0, z)
      t364 = s * t363
      t365 = t234 * t100
      t366 = t364 * t365
      t367 = t234 * x3
      t368 = t364 * t367
      t369 = t364 * t237
      t370 = t363 ** 2
      t374 = s * t370 * t241 * t327 * t100
      t376 = 0.1D1 / (-0.2D1 + t363)
      t378 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t366, 0.0D0, t
     #368, -t369, t374)
      t383 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, -t366, 0.0D0, t
     #368, -t369, t374)
      t385 = t370 ** 2
      t390 = log(-0.4D1 * t276 * t277 * t100 * t385)
      t404 = t6 * t376 * t378 * t111 * t272 / 0.8D1 + (-0.90D2 * t6 * (-
     #t376 * t383 + t390 * t376 * t378) - 0.180D3 * t26 * t5 * t376 * t3
     #78) * t111 * t95 / 0.720D3
      t405 = FJET(XB1, XB2, s, -t366, t368, 0.0D0, -t369, t374, t404)
      t407 = KAPPA2(x1, x2, t362, x4, z)
      t408 = s * t407
      t409 = t408 * t365
      t410 = t408 * t367
      t411 = t408 * t320
      t412 = t408 * t322
      t413 = t407 ** 2
      t418 = cos(t7)
      t421 = Sqrt(x3 * t100 * t194)
      t426 = s * t413 * t241 * t327 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x
     #4 + 0.2D1 * t418 * t421)
      t428 = 0.1D1 / (-0.2D1 + t407)
      t430 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t409, -t411, t
     #410, t412, t426)
      t432 = t430 * t111 * t272
      t435 = FJET(XB1, XB2, s, -t409, t410, -t411, t412, t426, -t6 * t42
     #8 * t432 / 0.8D1)
      rrgg2gght5s6e0 = t172 * t171 + t230 * t229 + t315 * t314 + t360 * 
     #t359 + t405 * t404 - t435 * pi * t5 * t428 * t432 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s6em1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t9 = sin(x2 * pi)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t16 = log(0.4D1 * t13 * x4)
      t17 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t22 = pi * lh
      t25 = 0.180D3 * t22 * t5 * t17
      t27 = 0.1D1 / x4
      t30 = x1 ** 2
      t31 = t30 * t10
      t34 = log(0.4D1 * t31 * t12)
      t40 = 0.1D1 / x1
      t47 = x3 * t10
      t50 = log(0.4D1 * t47 * t12)
      t51 = -0.1D1 + x3
      t55 = log(-0.4D1 * t47 * t12 * t51)
      t58 = 0.1D1 / x3
      t64 = log(0.4D1 * t13)
      t65 = t64 * pi
      t71 = pi ** 2
      t73 = lh ** 2
      t79 = t64 ** 2
      t86 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t89 = -(0.90D2 * t6 * (t7 - t16 * t17) - t25) * t27 / 0.1440D4 + (
     #0.90D2 * t6 * (-t7 + t34 * t17) + t25) * t40 / 0.720D3 - t6 * t17 
     #* t40 * t27 / 0.8D1 - t6 * t17 * (-t50 + t55) * t58 / 0.16D2 - (-0
     #.180D3 * t22 - 0.90D2 * t65) * t5 * t7 / 0.1440D4 - (pi * (-0.30D2
     # * t71 + 0.180D3 * t73) + 0.180D3 * t65 * lh + 0.45D2 * t79 * pi) 
     #* t5 * t17 / 0.1440D4 - t6 * t86 / 0.16D2
      t90 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t89)
      t92 = t2 * x4
      t93 = -0.1D1 + x4
      t94 = t2 * t93
      t95 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t92, 0.0D
     #0, -t94, 0.0D0)
      t99 = log(-0.4D1 * t13 * x4 * t93)
      t100 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t92, 0.0
     #D0, -t94, 0.0D0)
      t115 = -(0.90D2 * t6 * (-t95 + t99 * t100) + 0.180D3 * t22 * t5 * 
     #t100) * t27 / 0.1440D4 + t6 * t100 * t40 * t27 / 0.8D1
      t116 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t92, -t94, 0.0D0, t115)
      t118 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t119 = s * t118
      t120 = t1 * x1
      t121 = t119 * t120
      t122 = -0.1D1 + x1
      t123 = t1 * t122
      t124 = t119 * t123
      t125 = t118 ** 2
      t127 = t1 ** 2
      t130 = s * t125 * t127 * x1 * t122
      t132 = 0.1D1 / (-0.2D1 + t118)
      t133 = t6 * t132
      t134 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t121, 0.0D0, 0.
     #0D0, -t124, -t130)
      t139 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t121, 0.0D0, 0.
     #0D0, -t124, -t130)
      t141 = t122 ** 2
      t143 = t125 ** 2
      t147 = log(0.4D1 * t31 * t12 * t141 * t143)
      t164 = -t133 * t134 * t58 * t40 / 0.8D1 + (-0.90D2 * t6 * (t132 * 
     #t139 - t147 * t132 * t134) + 0.180D3 * t22 * t5 * t132 * t134) * t
     #40 / 0.720D3 - t133 * t134 * t40 * t27 / 0.8D1
      t165 = FJET(XB1, XB2, s, t121, 0.0D0, 0.0D0, -t124, -t130, t164)
      t167 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t168 = s * t167
      t169 = t168 * t120
      t171 = t168 * t123 * x4
      t173 = t168 * t123 * t93
      t174 = t167 ** 2
      t177 = x1 * t122
      t179 = s * t174 * t127 * t177 * t93
      t181 = 0.1D1 / (-0.2D1 + t167)
      t183 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t169, -t171, 0.
     #0D0, t173, t179)
      t188 = FJET(XB1, XB2, s, t169, 0.0D0, -t171, t173, t179, t6 * t181
     # * t183 * t40 * t27 / 0.8D1)
      t197 = KAPPA2(x1, x2, -t51, 0.0D0, z)
      t198 = s * t197
      t200 = t198 * t120 * t51
      t202 = t198 * t120 * x3
      t203 = t198 * t123
      t204 = t197 ** 2
      t208 = s * t204 * t127 * t177 * t51
      t210 = 0.1D1 / (-0.2D1 + t197)
      t212 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, -t200, 0.0D0, t
     #202, -t203, t208)
      t217 = FJET(XB1, XB2, s, -t200, t202, 0.0D0, -t203, t208, t6 * t21
     #0 * t212 * t58 * t40 / 0.8D1)
      rrgg2gght5s6em1 = t90 * t89 + t116 * t115 + t165 * t164 + t188 * p
     #i * t5 * t181 * t183 * t40 * t27 / 0.8D1 + t217 * pi * t5 * t210 *
     # t212 * t58 * t40 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s6em2
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t8 = 0.1D1 / x1
      t12 = 0.1D1 / x4
      t16 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t22 = sin(x2 * pi)
      t23 = t22 ** 2
      t24 = z ** 2
      t28 = log(0.4D1 * t23 / t24)
      t35 = -t6 * t7 * t8 / 0.8D1 - t6 * t7 * t12 / 0.16D2 - t6 * t16 / 
     #0.16D2 - (-0.180D3 * pi * lh - 0.90D2 * t28 * pi) * t5 * t7 / 0.14
     #40D4
      t36 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t35)
      t38 = KAPPA2(x1, x2, 0.10D1, 0.0D0, z)
      t39 = s * t38
      t41 = t39 * t1 * x1
      t42 = -0.1D1 + x1
      t44 = t39 * t1 * t42
      t45 = t38 ** 2
      t47 = t1 ** 2
      t50 = s * t45 * t47 * x1 * t42
      t53 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t41, 0.0D0, 0.0D
     #0, -t44, -t50)
      t55 = 0.1D1 / (-0.2D1 + t38) * t53 * t8
      t58 = FJET(XB1, XB2, s, t41, 0.0D0, 0.0D0, -t44, -t50, -t6 * t55 /
     # 0.8D1)
      t63 = t2 * x4
      t65 = t2 * (-0.1D1 + x4)
      t66 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t63, 0.0D
     #0, -t65, 0.0D0)
      t70 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t63, -t65, 0.0D0, t6 * t66 *
     # t12 / 0.16D2)
      rrgg2gght5s6em2 = t36 * t35 - t58 * pi * t5 * t55 / 0.8D1 + t70 * 
     #pi * t5 * t66 * t12 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s6em3
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -pi * t5 *
     # t7 / 0.16D2)
      rrgg2gght5s6em3 = -t10 * pi * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s6em4
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght5s6em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght5s7e1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x1 ** 2
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t20 = t17 ** 2
      t21 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t24 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t28 = pi * lh
      t34 = pi ** 2
      t36 = lh ** 2
      t38 = -0.30D2 * t34 + 0.180D3 * t36
      t39 = pi * t38
      t40 = t5 * t21
      t41 = t39 * t40
      t43 = 0.1D1 / x1
      t45 = 0.1D1 / x4
      t48 = t11 * t13
      t50 = log(0.4D1 * t48)
      t55 = t50 ** 2
      t58 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t71 = -0.240D3 * zeta3 - 0.120D3 * t36 * lh + 0.60D2 * lh * t34
      t72 = pi * t71
      t73 = t72 * t40
      t84 = x3 * t7
      t85 = t10 * t13
      t86 = t85 * x4
      t89 = log(0.4D1 * t84 * t86)
      t91 = t84 * t10
      t92 = -0.1D1 + x3
      t93 = t13 * t92
      t94 = t93 * x4
      t97 = log(-0.4D1 * t91 * t94)
      t101 = 0.1D1 / x3
      t103 = t101 * t43 * t45
      t109 = log(-0.4D1 * t84 * t85 * t92)
      t111 = t109 ** 2
      t114 = t84 * t85
      t116 = log(0.4D1 * t114)
      t118 = t116 ** 2
      t135 = log(0.4D1 * t86)
      t140 = t135 ** 2
      t160 = x3 * t10
      t163 = log(-0.4D1 * t160 * t94)
      t165 = t163 ** 2
      t170 = log(0.4D1 * t160 * t14)
      t172 = t170 ** 2
      t193 = t160 * t13
      t195 = log(0.4D1 * t193)
      t196 = t195 ** 2
      t199 = log(-0.4D1 * t160 * t93)
      t200 = t199 ** 2
      t223 = log(0.4D1 * t85)
      t224 = t223 * pi
      t226 = t223 ** 2
      t227 = t226 * pi
      t231 = t226 * t223 * pi
      t250 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D
     #0, 0.0D0, 0.0D0)
      t253 = t34 ** 2
      t254 = t36 ** 2
      t267 = t226 ** 2
      t274 = -(0.90D2 * t6 * (-t17 * t18 + t20 * t21 / 0.2D1 + t24) - 0.
     #180D3 * t28 * t5 * (t18 - t17 * t21) + t41) * t43 * t45 / 0.720D3 
     #- (t39 * t5 * (t18 - t50 * t21) + 0.90D2 * t6 * (t55 * t18 / 0.2D1
     # + t58 - t55 * t50 * t21 / 0.6D1 - t50 * t24) + t73 - 0.180D3 * t2
     #8 * t5 * (-t50 * t18 + t55 * t21 / 0.2D1 + t24)) * t43 / 0.720D3 +
     # t6 * (t89 * t21 - t97 * t21) * t103 / 0.8D1 + (0.90D2 * t6 * (-t1
     #09 * t18 + t111 * t21 / 0.2D1 + t116 * t18 - t118 * t21 / 0.2D1) -
     # 0.180D3 * t28 * t5 * (-t109 * t21 + t116 * t21)) * t101 * t43 / 0
     #.720D3 + (t39 * t5 * (-t18 + t135 * t21) + 0.90D2 * t6 * (-t140 * 
     #t18 / 0.2D1 - t58 + t140 * t135 * t21 / 0.6D1 + t135 * t24) - t73 
     #- 0.180D3 * t28 * t5 * (t135 * t18 - t140 * t21 / 0.2D1 - t24)) * 
     #t45 / 0.1440D4 + (0.90D2 * t6 * (-t163 * t18 + t165 * t21 / 0.2D1 
     #+ t170 * t18 - t172 * t21 / 0.2D1) - 0.180D3 * t28 * t5 * (-t163 *
     # t21 + t170 * t21)) * t101 * t45 / 0.1440D4 + ((-0.90D2 * t6 * t18
     # + 0.180D3 * t28 * t40) * (t196 / 0.2D1 - t200 / 0.2D1) - 0.90D2 *
     # t6 * t21 * (t200 * t199 / 0.6D1 - t196 * t195 / 0.6D1) + (0.180D3
     # * t28 * t5 * t18 - t41 - 0.90D2 * t6 * t24) * (-t195 + t199)) * t
     #101 / 0.1440D4 - (t72 - t224 * t38 - 0.90D2 * t227 * lh - 0.15D2 *
     # t231) * t5 * t18 / 0.1440D4 - (t39 + 0.180D3 * t224 * lh + 0.45D2
     # * t227) * t5 * t24 / 0.1440D4 - (-0.180D3 * t28 - 0.90D2 * t224) 
     #* t5 * t58 / 0.1440D4 - t6 * t250 / 0.16D2 - (pi * (t253 + 0.60D2 
     #* t254 + 0.480D3 * lh * zeta3 - 0.60D2 * t36 * t34) - t224 * t71 +
     # t227 * t38 / 0.2D1 + 0.30D2 * t231 * lh + 0.15D2 / 0.4D1 * t267 *
     # pi) * t5 * t21 / 0.1440D4
      t275 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t274)
      t277 = -0.1D1 + x4
      t278 = t2 * t277
      t279 = t2 * x4
      t280 = t14 * t277
      t283 = log(-0.4D1 * t11 * t280)
      t284 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t278, 0
     #.0D0, t279, 0.0D0)
      t286 = t283 ** 2
      t287 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t278, 0
     #.0D0, t279, 0.0D0)
      t290 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t278, 0
     #.0D0, t279, 0.0D0)
      t299 = t5 * t287
      t309 = log(0.4D1 * t91 * t14 * t277 * t92)
      t313 = log(-0.4D1 * t91 * t280)
      t319 = x4 * t277
      t322 = log(-0.4D1 * t85 * t319)
      t327 = t322 ** 2
      t330 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t278, 0
     #.0D0, t279, 0.0D0)
      t352 = log(0.4D1 * t193 * t319 * t92)
      t354 = t352 ** 2
      t359 = log(-0.4D1 * t160 * t280)
      t361 = t359 ** 2
      t377 = -(0.90D2 * t6 * (t283 * t284 - t286 * t287 / 0.2D1 - t290) 
     #- 0.180D3 * t28 * t5 * (-t284 + t283 * t287) - t39 * t299) * t43 *
     # t45 / 0.720D3 + t6 * (t309 * t287 - t313 * t287) * t103 / 0.8D1 +
     # (t39 * t5 * (t284 - t322 * t287) + 0.90D2 * t6 * (t327 * t284 / 0
     #.2D1 + t330 - t327 * t322 * t287 / 0.6D1 - t322 * t290) + t72 * t2
     #99 - 0.180D3 * t28 * t5 * (-t322 * t284 + t327 * t287 / 0.2D1 + t2
     #90)) * t45 / 0.1440D4 + (0.90D2 * t6 * (t352 * t284 - t354 * t287 
     #/ 0.2D1 - t359 * t284 + t361 * t287 / 0.2D1) - 0.180D3 * t28 * t5 
     #* (t352 * t287 - t359 * t287)) * t101 * t45 / 0.1440D4
      t378 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t278, t279, 0.0D0, t377)
      t380 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t381 = s * t380
      t382 = t1 * x1
      t383 = t381 * t382
      t384 = -0.1D1 + x1
      t385 = t1 * t384
      t386 = t381 * t385
      t387 = t380 ** 2
      t389 = t1 ** 2
      t392 = s * t387 * t389 * x1 * t384
      t393 = t384 ** 2
      t394 = t393 * x4
      t395 = t387 ** 2
      t399 = log(0.4D1 * t48 * t394 * t395)
      t401 = 0.1D1 / (-0.2D1 + t380)
      t402 = t399 * t401
      t403 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t386, t
     #383, 0.0D0, -t392)
      t405 = t399 ** 2
      t407 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t386, t
     #383, 0.0D0, -t392)
      t410 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t386, t
     #383, 0.0D0, -t392)
      t411 = t401 * t410
      t415 = t401 * t403
      t422 = t5 * t401 * t407
      t423 = t39 * t422
      t427 = t7 * t393
      t428 = t427 * t395
      t431 = log(0.4D1 * t85 * t428)
      t432 = t431 * t401
      t437 = t431 ** 2
      t438 = t437 * t401
      t441 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t386, t
     #383, 0.0D0, -t392)
      t465 = log(0.4D1 * t193 * t427 * x4 * t395)
      t475 = t43 * t45
      t479 = log(0.4D1 * t193 * t428)
      t480 = t479 * t401
      t482 = t479 ** 2
      t498 = -(0.90D2 * t6 * (-t402 * t403 + t405 * t401 * t407 / 0.2D1 
     #+ t411) - 0.180D3 * t28 * t5 * (t415 - t402 * t407) + t423) * t43 
     #* t45 / 0.720D3 - (t39 * t5 * (t415 - t432 * t407) + 0.90D2 * t6 *
     # (t438 * t403 / 0.2D1 + t401 * t441 - t437 * t431 * t401 * t407 / 
     #0.6D1 - t432 * t410) + t72 * t422 - 0.180D3 * t28 * t5 * (-t432 * 
     #t403 + t438 * t407 / 0.2D1 + t411)) * t43 / 0.720D3 + (0.90D2 * t6
     # * (-t415 + t465 * t401 * t407) + 0.180D3 * t28 * t422) * t101 * t
     #475 / 0.720D3 + (0.90D2 * t6 * (t480 * t403 - t482 * t401 * t407 /
     # 0.2D1 - t411) - 0.180D3 * t28 * t5 * (-t415 + t480 * t407) - t423
     #) * t101 * t43 / 0.720D3
      t499 = FJET(XB1, XB2, s, 0.0D0, t383, -t386, 0.0D0, -t392, t498)
      t501 = -t277
      t502 = KAPPA2(x1, x2, 0.0D0, t501, z)
      t503 = s * t502
      t504 = t503 * t382
      t505 = t385 * t277
      t506 = t503 * t505
      t507 = t385 * x4
      t508 = t503 * t507
      t509 = t502 ** 2
      t512 = x1 * t384
      t514 = s * t509 * t389 * t512 * t277
      t515 = t509 ** 2
      t517 = t394 * t277 * t515
      t520 = log(-0.4D1 * t48 * t517)
      t522 = 0.1D1 / (-0.2D1 + t502)
      t523 = t520 * t522
      t524 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t506, t5
     #04, -t508, t514)
      t526 = t520 ** 2
      t528 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t506, t5
     #04, -t508, t514)
      t531 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t506, t5
     #04, -t508, t514)
      t536 = t522 * t524
      t543 = t5 * t522 * t528
      t550 = log(-0.4D1 * t114 * t517)
      t562 = -(-0.90D2 * t6 * (-t523 * t524 + t526 * t522 * t528 / 0.2D1
     # + t522 * t531) + 0.180D3 * t28 * t5 * (t536 - t523 * t528) - t39 
     #* t543) * t43 * t45 / 0.720D3 + (0.90D2 * t6 * (t536 - t550 * t522
     # * t528) - 0.180D3 * t28 * t543) * t101 * t475 / 0.720D3
      t563 = FJET(XB1, XB2, s, 0.0D0, t504, t506, -t508, t514, t562)
      t565 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t566 = s * t565
      t567 = t382 * x3
      t568 = t566 * t567
      t569 = t382 * t92
      t570 = t566 * t569
      t571 = t566 * t385
      t572 = t565 ** 2
      t576 = s * t572 * t389 * t512 * t92
      t578 = 0.1D1 / (-0.2D1 + t565)
      t579 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t568, -t571, -t
     #570, 0.0D0, t576)
      t580 = t578 * t579
      t581 = t393 * t92
      t582 = t572 ** 2
      t587 = log(-0.4D1 * t114 * t581 * x4 * t582)
      t589 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t568, -t571, -t
     #570, 0.0D0, t576)
      t595 = t5 * t578 * t589
      t605 = log(-0.4D1 * t193 * t427 * t92 * t582)
      t606 = t605 * t578
      t608 = t605 ** 2
      t612 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t568, -t571, -t
     #570, 0.0D0, t576)
      t627 = (0.90D2 * t6 * (t580 - t587 * t578 * t589) - 0.180D3 * t28 
     #* t595) * t101 * t475 / 0.720D3 + (-0.90D2 * t6 * (t606 * t579 - t
     #608 * t578 * t589 / 0.2D1 - t578 * t612) + 0.180D3 * t28 * t5 * (-
     #t580 + t606 * t589) + t39 * t595) * t101 * t43 / 0.720D3
      t628 = FJET(XB1, XB2, s, t568, -t570, -t571, 0.0D0, t576, t627)
      t630 = KAPPA2(x1, x2, x3, t501, z)
      t631 = s * t630
      t632 = t631 * t567
      t633 = t631 * t569
      t634 = t631 * t505
      t635 = t631 * t507
      t636 = t630 ** 2
      t641 = cos(t8)
      t644 = Sqrt(x3 * t92 * t319)
      t649 = s * t636 * t389 * t512 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x
     #4 + 0.2D1 * t641 * t644)
      t651 = 0.1D1 / (-0.2D1 + t630)
      t652 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t632, t634, -t6
     #33, -t635, t649)
      t654 = t636 ** 2
      t659 = log(0.4D1 * t114 * t319 * t581 * t654)
      t661 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t632, t634, -t6
     #33, -t635, t649)
      t670 = -0.90D2 * t6 * (t651 * t652 - t659 * t651 * t661) + 0.180D3
     # * t28 * t5 * t651 * t661
      t674 = FJET(XB1, XB2, s, t632, -t633, t634, -t635, t649, t670 * t1
     #01 * t475 / 0.720D3)
      rrgg2gght5s7e1 = t275 * t274 + t378 * t377 + t499 * t498 + t563 * 
     #t562 + t628 * t627 + t674 * t670 * t103 / 0.720D3

      end function



      doubleprecision function rrgg2gght5s7e0
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x2 * pi
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t15 = log(0.4D1 * t12 * x4)
      t16 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t18 = t15 ** 2
      t19 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t22 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t26 = pi * lh
      t32 = pi ** 2
      t34 = lh ** 2
      t36 = -0.30D2 * t32 + 0.180D3 * t34
      t37 = pi * t36
      t38 = t5 * t19
      t39 = t37 * t38
      t41 = 0.1D1 / x4
      t45 = log(0.4D1 * t12)
      t46 = t45 * pi
      t49 = t45 ** 2
      t50 = t49 * pi
      t56 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t82 = x1 ** 2
      t83 = t82 * t9
      t84 = t11 * x4
      t87 = log(0.4D1 * t83 * t84)
      t93 = 0.180D3 * t26 * t38
      t95 = 0.1D1 / x1
      t99 = x3 * t82
      t100 = -0.1D1 + x3
      t104 = log(-0.4D1 * t99 * t12 * t100)
      t108 = log(0.4D1 * t99 * t12)
      t111 = 0.1D1 / x3
      t116 = t83 * t11
      t118 = log(0.4D1 * t116)
      t120 = t118 ** 2
      t134 = x3 * t9
      t135 = t11 * t100
      t139 = log(-0.4D1 * t134 * t135 * x4)
      t143 = log(0.4D1 * t134 * t84)
      t150 = t134 * t11
      t152 = log(0.4D1 * t150)
      t153 = t152 ** 2
      t156 = log(-0.4D1 * t134 * t135)
      t157 = t156 ** 2
      t171 = (0.90D2 * t6 * (t15 * t16 - t18 * t19 / 0.2D1 - t22) - 0.18
     #0D3 * t26 * t5 * (-t16 + t15 * t19) - t39) * t41 / 0.1440D4 - (t37
     # + 0.180D3 * t46 * lh + 0.45D2 * t50) * t5 * t16 / 0.1440D4 - t6 *
     # t56 / 0.16D2 - (pi * (-0.240D3 * zeta3 - 0.120D3 * lh * t34 + 0.6
     #0D2 * lh * t32) - t46 * t36 - 0.90D2 * t50 * lh - 0.15D2 * t49 * t
     #45 * pi) * t5 * t19 / 0.1440D4 - (-0.180D3 * t26 - 0.90D2 * t46) *
     # t5 * t22 / 0.1440D4 - (0.90D2 * t6 * (t16 - t87 * t19) - t93) * t
     #95 * t41 / 0.720D3 + t6 * (-t104 * t19 + t108 * t19) * t111 * t95 
     #/ 0.8D1 - (0.90D2 * t6 * (-t118 * t16 + t120 * t19 / 0.2D1 + t22) 
     #- 0.180D3 * t26 * t5 * (t16 - t118 * t19) + t39) * t95 / 0.720D3 +
     # t6 * (-t139 * t19 + t143 * t19) * t111 * t41 / 0.16D2 + (-0.90D2 
     #* t6 * t19 * (t153 / 0.2D1 - t157 / 0.2D1) + (-0.90D2 * t6 * t16 +
     # t93) * (-t152 + t156)) * t111 / 0.1440D4
      t172 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t171)
      t174 = -0.1D1 + x4
      t175 = t2 * t174
      t176 = t2 * x4
      t177 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t175, 0
     #.0D0, t176, 0.0D0)
      t178 = t84 * t174
      t181 = log(-0.4D1 * t83 * t178)
      t182 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t175, 0
     #.0D0, t176, 0.0D0)
      t187 = t5 * t182
      t194 = x4 * t174
      t198 = log(0.4D1 * t150 * t194 * t100)
      t202 = log(-0.4D1 * t134 * t178)
      t211 = log(-0.4D1 * t12 * t194)
      t213 = t211 ** 2
      t216 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t175, 0
     #.0D0, t176, 0.0D0)
      t229 = -(0.90D2 * t6 * (-t177 + t181 * t182) + 0.180D3 * t26 * t18
     #7) * t95 * t41 / 0.720D3 + t6 * (t198 * t182 - t202 * t182) * t111
     # * t41 / 0.16D2 + (0.90D2 * t6 * (-t211 * t177 + t213 * t182 / 0.2
     #D1 + t216) - 0.180D3 * t26 * t5 * (t177 - t211 * t182) + t37 * t18
     #7) * t41 / 0.1440D4
      t230 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t175, t176, 0.0D0, t229)
      t232 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t233 = s * t232
      t234 = t1 * x1
      t235 = t233 * t234
      t236 = -0.1D1 + x1
      t237 = t1 * t236
      t238 = t233 * t237
      t239 = t232 ** 2
      t241 = t1 ** 2
      t244 = s * t239 * t241 * x1 * t236
      t246 = 0.1D1 / (-0.2D1 + t232)
      t247 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t238, t
     #235, 0.0D0, -t244)
      t248 = t246 * t247
      t249 = t236 ** 2
      t250 = t249 * x4
      t251 = t239 ** 2
      t255 = log(0.4D1 * t116 * t250 * t251)
      t257 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t238, t
     #235, 0.0D0, -t244)
      t263 = t5 * t246 * t257
      t265 = 0.180D3 * t26 * t263
      t272 = t95 * t41
      t276 = t82 * t249
      t277 = t276 * t251
      t280 = log(0.4D1 * t150 * t277)
      t292 = log(0.4D1 * t12 * t277)
      t293 = t292 * t246
      t295 = t292 ** 2
      t299 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t238, t
     #235, 0.0D0, -t244)
      t313 = -(0.90D2 * t6 * (t248 - t255 * t246 * t257) - t265) * t95 *
     # t41 / 0.720D3 - t6 * t246 * t257 * t111 * t272 / 0.8D1 + (0.90D2 
     #* t6 * (-t248 + t280 * t246 * t257) + t265) * t111 * t95 / 0.720D3
     # - (0.90D2 * t6 * (-t293 * t247 + t295 * t246 * t257 / 0.2D1 + t24
     #6 * t299) - 0.180D3 * t26 * t5 * (t248 - t293 * t257) + t37 * t263
     #) * t95 / 0.720D3
      t314 = FJET(XB1, XB2, s, 0.0D0, t235, -t238, 0.0D0, -t244, t313)
      t316 = -t174
      t317 = KAPPA2(x1, x2, 0.0D0, t316, z)
      t318 = s * t317
      t319 = t318 * t234
      t320 = t237 * t174
      t321 = t318 * t320
      t322 = t237 * x4
      t323 = t318 * t322
      t324 = t317 ** 2
      t327 = x1 * t236
      t329 = s * t324 * t241 * t327 * t174
      t331 = 0.1D1 / (-0.2D1 + t317)
      t332 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t321, t3
     #19, -t323, t329)
      t334 = t324 ** 2
      t339 = log(-0.4D1 * t116 * t250 * t174 * t334)
      t341 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t321, t3
     #19, -t323, t329)
      t359 = -(-0.90D2 * t6 * (t331 * t332 - t339 * t331 * t341) + 0.180
     #D3 * t26 * t5 * t331 * t341) * t95 * t41 / 0.720D3 + t6 * t331 * t
     #341 * t111 * t272 / 0.8D1
      t360 = FJET(XB1, XB2, s, 0.0D0, t319, t321, -t323, t329, t359)
      t362 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t363 = s * t362
      t364 = t234 * x3
      t365 = t363 * t364
      t366 = t234 * t100
      t367 = t363 * t366
      t368 = t363 * t237
      t369 = t362 ** 2
      t373 = s * t369 * t241 * t327 * t100
      t375 = 0.1D1 / (-0.2D1 + t362)
      t377 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t365, -t368, -t
     #367, 0.0D0, t373)
      t382 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t365, -t368, -t
     #367, 0.0D0, t373)
      t384 = t369 ** 2
      t389 = log(-0.4D1 * t150 * t276 * t100 * t384)
      t403 = t6 * t375 * t377 * t111 * t272 / 0.8D1 + (-0.90D2 * t6 * (-
     #t375 * t382 + t389 * t375 * t377) - 0.180D3 * t26 * t5 * t375 * t3
     #77) * t111 * t95 / 0.720D3
      t404 = FJET(XB1, XB2, s, t365, -t367, -t368, 0.0D0, t373, t403)
      t406 = KAPPA2(x1, x2, x3, t316, z)
      t407 = s * t406
      t408 = t407 * t364
      t409 = t407 * t366
      t410 = t407 * t320
      t411 = t407 * t322
      t412 = t406 ** 2
      t417 = cos(t7)
      t420 = Sqrt(x3 * t100 * t194)
      t425 = s * t412 * t241 * t327 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x
     #4 + 0.2D1 * t417 * t420)
      t427 = 0.1D1 / (-0.2D1 + t406)
      t429 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t408, t410, -t4
     #09, -t411, t425)
      t431 = t429 * t111 * t272
      t434 = FJET(XB1, XB2, s, t408, -t409, t410, -t411, t425, -t6 * t42
     #7 * t431 / 0.8D1)
      rrgg2gght5s7e0 = t172 * t171 + t230 * t229 + t314 * t313 + t360 * 
     #t359 + t404 * t403 - t434 * pi * t5 * t427 * t431 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s7em1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t9 = sin(x2 * pi)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t16 = log(0.4D1 * t13 * x4)
      t17 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t22 = pi * lh
      t25 = 0.180D3 * t22 * t5 * t17
      t27 = 0.1D1 / x4
      t30 = x1 ** 2
      t34 = log(0.4D1 * t30 * t10 * t12)
      t40 = 0.1D1 / x1
      t47 = x3 * t10
      t50 = log(0.4D1 * t47 * t12)
      t51 = -0.1D1 + x3
      t55 = log(-0.4D1 * t47 * t12 * t51)
      t58 = 0.1D1 / x3
      t64 = log(0.4D1 * t13)
      t65 = t64 * pi
      t71 = pi ** 2
      t73 = lh ** 2
      t79 = t64 ** 2
      t86 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t89 = (0.90D2 * t6 * (-t7 + t16 * t17) + t25) * t27 / 0.1440D4 - (
     #0.90D2 * t6 * (t7 - t34 * t17) - t25) * t40 / 0.720D3 - t6 * t17 *
     # t40 * t27 / 0.8D1 - t6 * t17 * (-t50 + t55) * t58 / 0.16D2 - (-0.
     #180D3 * t22 - 0.90D2 * t65) * t5 * t7 / 0.1440D4 - (pi * (-0.30D2 
     #* t71 + 0.180D3 * t73) + 0.180D3 * t65 * lh + 0.45D2 * t79 * pi) *
     # t5 * t17 / 0.1440D4 - t6 * t86 / 0.16D2
      t90 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t89)
      t92 = -0.1D1 + x4
      t93 = t2 * t92
      t94 = t2 * x4
      t95 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t93, 0.0
     #D0, t94, 0.0D0)
      t99 = log(-0.4D1 * t13 * x4 * t92)
      t100 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t93, 0.
     #0D0, t94, 0.0D0)
      t115 = (0.90D2 * t6 * (t95 - t99 * t100) - 0.180D3 * t22 * t5 * t1
     #00) * t27 / 0.1440D4 + t6 * t100 * t40 * t27 / 0.8D1
      t116 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t93, t94, 0.0D0, t115)
      t118 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t119 = s * t118
      t120 = t1 * x1
      t121 = t119 * t120
      t122 = -0.1D1 + x1
      t123 = t1 * t122
      t124 = t119 * t123
      t125 = t118 ** 2
      t127 = t1 ** 2
      t130 = s * t125 * t127 * x1 * t122
      t132 = 0.1D1 / (-0.2D1 + t118)
      t133 = t6 * t132
      t134 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t124, t
     #121, 0.0D0, -t130)
      t139 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t124, t
     #121, 0.0D0, -t130)
      t141 = t122 ** 2
      t143 = t125 ** 2
      t147 = log(0.4D1 * t13 * t30 * t141 * t143)
      t164 = -t133 * t134 * t58 * t40 / 0.8D1 - (0.90D2 * t6 * (t132 * t
     #139 - t147 * t132 * t134) - 0.180D3 * t22 * t5 * t132 * t134) * t4
     #0 / 0.720D3 - t133 * t134 * t40 * t27 / 0.8D1
      t165 = FJET(XB1, XB2, s, 0.0D0, t121, -t124, 0.0D0, -t130, t164)
      t168 = KAPPA2(x1, x2, 0.0D0, -t92, z)
      t169 = s * t168
      t170 = t169 * t120
      t172 = t169 * t123 * t92
      t174 = t169 * t123 * x4
      t175 = t168 ** 2
      t178 = x1 * t122
      t180 = s * t175 * t127 * t178 * t92
      t182 = 0.1D1 / (-0.2D1 + t168)
      t184 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t172, t1
     #70, -t174, t180)
      t189 = FJET(XB1, XB2, s, 0.0D0, t170, t172, -t174, t180, t6 * t182
     # * t184 * t40 * t27 / 0.8D1)
      t197 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t198 = s * t197
      t200 = t198 * t120 * x3
      t202 = t198 * t120 * t51
      t203 = t198 * t123
      t204 = t197 ** 2
      t208 = s * t204 * t127 * t178 * t51
      t210 = 0.1D1 / (-0.2D1 + t197)
      t212 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t200, -t203, -t
     #202, 0.0D0, t208)
      t217 = FJET(XB1, XB2, s, t200, -t202, -t203, 0.0D0, t208, t6 * t21
     #0 * t212 * t58 * t40 / 0.8D1)
      rrgg2gght5s7em1 = t90 * t89 + t116 * t115 + t165 * t164 + t189 * p
     #i * t5 * t182 * t184 * t40 * t27 / 0.8D1 + t217 * pi * t5 * t210 *
     # t212 * t58 * t40 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s7em2
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t8 = 0.1D1 / x1
      t12 = 0.1D1 / x4
      t16 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0
     #, 0.0D0, 0.0D0)
      t22 = sin(x2 * pi)
      t23 = t22 ** 2
      t24 = z ** 2
      t28 = log(0.4D1 * t23 / t24)
      t35 = -t6 * t7 * t8 / 0.8D1 - t6 * t7 * t12 / 0.16D2 - t6 * t16 / 
     #0.16D2 - (-0.180D3 * pi * lh - 0.90D2 * t28 * pi) * t5 * t7 / 0.14
     #40D4
      t36 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, t35)
      t38 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t39 = s * t38
      t41 = t39 * t1 * x1
      t42 = -0.1D1 + x1
      t44 = t39 * t1 * t42
      t45 = t38 ** 2
      t47 = t1 ** 2
      t50 = s * t45 * t47 * x1 * t42
      t53 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t44, t41
     #, 0.0D0, -t50)
      t55 = 0.1D1 / (-0.2D1 + t38) * t53 * t8
      t58 = FJET(XB1, XB2, s, 0.0D0, t41, -t44, 0.0D0, -t50, -t6 * t55 /
     # 0.8D1)
      t64 = t2 * (-0.1D1 + x4)
      t65 = t2 * x4
      t66 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t64, 0.0
     #D0, t65, 0.0D0)
      t70 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, -t64, t65, 0.0D0, t6 * t66 *
     # t12 / 0.16D2)
      rrgg2gght5s7em2 = t36 * t35 - t58 * pi * t5 * t55 / 0.8D1 + t70 * 
     #pi * t5 * t66 * t12 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s7em3
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t2, 0.0D0,
     # 0.0D0, 0.0D0)
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t2, 0.0D0, 0.0D0, -pi * t5 *
     # t7 / 0.16D2)
      rrgg2gght5s7em3 = -t10 * pi * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s7em4
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght5s7em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght5s8e1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x1 ** 2
      t8 = x2 * pi
      t9 = sin(t8)
      t10 = t9 ** 2
      t11 = t7 * t10
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t13 * x4
      t17 = log(0.4D1 * t11 * t14)
      t18 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t20 = t17 ** 2
      t21 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t24 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t28 = pi * lh
      t34 = pi ** 2
      t36 = lh ** 2
      t38 = -0.30D2 * t34 + 0.180D3 * t36
      t39 = pi * t38
      t40 = t5 * t21
      t41 = t39 * t40
      t43 = 0.1D1 / x1
      t45 = 0.1D1 / x4
      t48 = t11 * t13
      t50 = log(0.4D1 * t48)
      t55 = t50 ** 2
      t58 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t71 = -0.240D3 * zeta3 - 0.120D3 * t36 * lh + 0.60D2 * lh * t34
      t72 = pi * t71
      t73 = t72 * t40
      t84 = x3 * t7
      t85 = t10 * t13
      t86 = t85 * x4
      t89 = log(0.4D1 * t84 * t86)
      t91 = t84 * t10
      t92 = -0.1D1 + x3
      t93 = t13 * t92
      t94 = t93 * x4
      t97 = log(-0.4D1 * t91 * t94)
      t101 = 0.1D1 / x3
      t103 = t101 * t43 * t45
      t109 = log(-0.4D1 * t84 * t85 * t92)
      t111 = t109 ** 2
      t114 = t84 * t85
      t116 = log(0.4D1 * t114)
      t118 = t116 ** 2
      t135 = log(0.4D1 * t86)
      t140 = t135 ** 2
      t160 = x3 * t10
      t163 = log(0.4D1 * t160 * t14)
      t165 = t163 ** 2
      t170 = log(-0.4D1 * t160 * t94)
      t172 = t170 ** 2
      t193 = t160 * t13
      t195 = log(0.4D1 * t193)
      t196 = t195 ** 2
      t199 = log(-0.4D1 * t160 * t93)
      t200 = t199 ** 2
      t223 = log(0.4D1 * t85)
      t224 = t223 * pi
      t226 = t223 ** 2
      t227 = t226 * pi
      t231 = t226 * t223 * pi
      t250 = rrgg2ggh51J5(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0
     #.0D0, t2, 0.0D0)
      t253 = t34 ** 2
      t254 = t36 ** 2
      t267 = t226 ** 2
      t274 = -(0.90D2 * t6 * (-t17 * t18 + t20 * t21 / 0.2D1 + t24) - 0.
     #180D3 * t28 * t5 * (t18 - t17 * t21) + t41) * t43 * t45 / 0.720D3 
     #- (t39 * t5 * (t18 - t50 * t21) + 0.90D2 * t6 * (t55 * t18 / 0.2D1
     # + t58 - t55 * t50 * t21 / 0.6D1 - t50 * t24) + t73 - 0.180D3 * t2
     #8 * t5 * (-t50 * t18 + t55 * t21 / 0.2D1 + t24)) * t43 / 0.720D3 +
     # t6 * (t89 * t21 - t97 * t21) * t103 / 0.8D1 + (0.90D2 * t6 * (-t1
     #09 * t18 + t111 * t21 / 0.2D1 + t116 * t18 - t118 * t21 / 0.2D1) -
     # 0.180D3 * t28 * t5 * (-t109 * t21 + t116 * t21)) * t101 * t43 / 0
     #.720D3 + (t39 * t5 * (-t18 + t135 * t21) + 0.90D2 * t6 * (-t140 * 
     #t18 / 0.2D1 - t58 + t140 * t135 * t21 / 0.6D1 + t135 * t24) - t73 
     #- 0.180D3 * t28 * t5 * (t135 * t18 - t140 * t21 / 0.2D1 - t24)) * 
     #t45 / 0.1440D4 + (0.90D2 * t6 * (t163 * t18 - t165 * t21 / 0.2D1 -
     # t170 * t18 + t172 * t21 / 0.2D1) - 0.180D3 * t28 * t5 * (t163 * t
     #21 - t170 * t21)) * t101 * t45 / 0.1440D4 - ((-0.90D2 * t6 * t18 +
     # 0.180D3 * t28 * t40) * (-t196 / 0.2D1 + t200 / 0.2D1) - 0.90D2 * 
     #t6 * t21 * (-t200 * t199 / 0.6D1 + t196 * t195 / 0.6D1) + (0.180D3
     # * t28 * t5 * t18 - t41 - 0.90D2 * t6 * t24) * (t195 - t199)) * t1
     #01 / 0.1440D4 - (t72 - t224 * t38 - 0.90D2 * t227 * lh - 0.15D2 * 
     #t231) * t5 * t18 / 0.1440D4 - (t39 + 0.180D3 * t224 * lh + 0.45D2 
     #* t227) * t5 * t24 / 0.1440D4 - (-0.180D3 * t28 - 0.90D2 * t224) *
     # t5 * t58 / 0.1440D4 - t6 * t250 / 0.16D2 - (pi * (t253 + 0.60D2 *
     # t254 + 0.480D3 * lh * zeta3 - 0.60D2 * t36 * t34) - t224 * t71 + 
     #t227 * t38 / 0.2D1 + 0.30D2 * t231 * lh + 0.15D2 / 0.4D1 * t267 * 
     #pi) * t5 * t21 / 0.1440D4
      t275 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t274)
      t277 = t2 * x4
      t278 = -0.1D1 + x4
      t279 = t2 * t278
      t280 = t14 * t278
      t283 = log(-0.4D1 * t11 * t280)
      t284 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t277, 0.
     #0D0, -t279, 0.0D0)
      t286 = t283 ** 2
      t287 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t277, 0.
     #0D0, -t279, 0.0D0)
      t290 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t277, 0.
     #0D0, -t279, 0.0D0)
      t299 = t5 * t287
      t307 = log(-0.4D1 * t91 * t280)
      t313 = log(0.4D1 * t91 * t14 * t278 * t92)
      t319 = x4 * t278
      t322 = log(-0.4D1 * t85 * t319)
      t327 = t322 ** 2
      t330 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t277, 0.
     #0D0, -t279, 0.0D0)
      t352 = log(0.4D1 * t193 * t319 * t92)
      t354 = t352 ** 2
      t359 = log(-0.4D1 * t160 * t280)
      t361 = t359 ** 2
      t377 = -(0.90D2 * t6 * (t283 * t284 - t286 * t287 / 0.2D1 - t290) 
     #- 0.180D3 * t28 * t5 * (-t284 + t283 * t287) - t39 * t299) * t43 *
     # t45 / 0.720D3 + t6 * (-t307 * t287 + t313 * t287) * t103 / 0.8D1 
     #+ (t39 * t5 * (t284 - t322 * t287) + 0.90D2 * t6 * (t327 * t284 / 
     #0.2D1 + t330 - t327 * t322 * t287 / 0.6D1 - t322 * t290) + t72 * t
     #299 - 0.180D3 * t28 * t5 * (-t322 * t284 + t327 * t287 / 0.2D1 + t
     #290)) * t45 / 0.1440D4 + (0.90D2 * t6 * (t352 * t284 - t354 * t287
     # / 0.2D1 - t359 * t284 + t361 * t287 / 0.2D1) - 0.180D3 * t28 * t5
     # * (t352 * t287 - t359 * t287)) * t101 * t45 / 0.1440D4
      t378 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t277, -t279, 0.0D0, t377)
      t380 = t2 * x1
      t381 = -0.1D1 + x1
      t382 = t2 * t381
      t383 = t381 ** 2
      t384 = t7 * t383
      t385 = t384 * x4
      t388 = log(0.4D1 * t85 * t385)
      t389 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #380, -t382, 0.0D0)
      t391 = t388 ** 2
      t392 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #380, -t382, 0.0D0)
      t395 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #380, -t382, 0.0D0)
      t404 = t5 * t392
      t405 = t39 * t404
      t411 = log(0.4D1 * t85 * t384)
      t416 = t411 ** 2
      t419 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #380, -t382, 0.0D0)
      t439 = log(0.4D1 * t193 * t385)
      t448 = t43 * t45
      t454 = log(0.4D1 * t160 * t13 * t7 * t383)
      t456 = t454 ** 2
      t471 = -(0.90D2 * t6 * (t388 * t389 - t391 * t392 / 0.2D1 - t395) 
     #- 0.180D3 * t28 * t5 * (-t389 + t388 * t392) - t405) * t43 * t45 /
     # 0.720D3 - (-t39 * t5 * (t389 - t411 * t392) - 0.90D2 * t6 * (t416
     # * t389 / 0.2D1 + t419 - t416 * t411 * t392 / 0.6D1 - t411 * t395)
     # - t72 * t404 + 0.180D3 * t28 * t5 * (-t411 * t389 + t416 * t392 /
     # 0.2D1 + t395)) * t43 / 0.720D3 + (0.90D2 * t6 * (t389 - t439 * t3
     #92) - 0.180D3 * t28 * t404) * t101 * t448 / 0.720D3 + (0.90D2 * t6
     # * (-t454 * t389 + t456 * t392 / 0.2D1 + t395) - 0.180D3 * t28 * t
     #5 * (t389 - t454 * t392) + t405) * t101 * t43 / 0.720D3
      t472 = FJET(XB1, XB2, s, 0.0D0, t380, 0.0D0, -t382, 0.0D0, t471)
      t474 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t475 = s * t474
      t476 = t1 * x1
      t477 = t475 * t476
      t478 = t1 * t381
      t479 = t478 * x4
      t480 = t475 * t479
      t481 = t478 * t278
      t482 = t475 * t481
      t483 = t474 ** 2
      t485 = t1 ** 2
      t487 = x1 * t381
      t489 = s * t483 * t485 * t487 * x4
      t490 = t483 ** 2
      t492 = t319 * t490 * t383
      t495 = log(-0.4D1 * t48 * t492)
      t497 = 0.1D1 / (-0.2D1 + t474)
      t498 = t495 * t497
      t499 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, t
     #477, t482, -t489)
      t501 = t495 ** 2
      t503 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, t
     #477, t482, -t489)
      t506 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t480, t
     #477, t482, -t489)
      t511 = t497 * t499
      t518 = t5 * t497 * t503
      t525 = log(-0.4D1 * t114 * t492)
      t537 = -(-0.90D2 * t6 * (-t498 * t499 + t501 * t497 * t503 / 0.2D1
     # + t497 * t506) + 0.180D3 * t28 * t5 * (t511 - t498 * t503) - t39 
     #* t518) * t43 * t45 / 0.720D3 + (0.90D2 * t6 * (t511 - t525 * t497
     # * t503) - 0.180D3 * t28 * t518) * t101 * t448 / 0.720D3
      t538 = FJET(XB1, XB2, s, 0.0D0, t477, -t480, t482, -t489, t537)
      t540 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t541 = s * t540
      t542 = t476 * x3
      t543 = t541 * t542
      t544 = t476 * t92
      t545 = t541 * t544
      t546 = t541 * t478
      t547 = t540 ** 2
      t551 = s * t547 * t485 * t487 * x3
      t553 = 0.1D1 / (-0.2D1 + t540)
      t554 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t543, 0.0D0, -t
     #545, -t546, -t551)
      t555 = t553 * t554
      t556 = t383 * t92
      t557 = t547 ** 2
      t562 = log(-0.4D1 * t114 * t556 * x4 * t557)
      t564 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t543, 0.0D0, -t
     #545, -t546, -t551)
      t570 = t5 * t553 * t564
      t580 = log(-0.4D1 * t193 * t384 * t92 * t557)
      t581 = t580 * t553
      t583 = t580 ** 2
      t587 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, t543, 0.0D0, -t
     #545, -t546, -t551)
      t602 = (0.90D2 * t6 * (t555 - t562 * t553 * t564) - 0.180D3 * t28 
     #* t570) * t101 * t448 / 0.720D3 + (0.90D2 * t6 * (-t581 * t554 + t
     #583 * t553 * t564 / 0.2D1 + t553 * t587) - 0.180D3 * t28 * t5 * (t
     #555 - t581 * t564) + t39 * t570) * t101 * t43 / 0.720D3
      t603 = FJET(XB1, XB2, s, t543, -t545, 0.0D0, -t546, -t551, t602)
      t605 = KAPPA2(x1, x2, x3, x4, z)
      t606 = s * t605
      t607 = t606 * t542
      t608 = t606 * t544
      t609 = t606 * t479
      t610 = t606 * t481
      t611 = t605 ** 2
      t616 = cos(t8)
      t619 = Sqrt(x3 * t92 * t319)
      t624 = s * t611 * t485 * t487 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D
     #1 * t616 * t619)
      t626 = 0.1D1 / (-0.2D1 + t605)
      t627 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t607, -t609, -t
     #608, t610, t624)
      t629 = t611 ** 2
      t634 = log(0.4D1 * t114 * t319 * t556 * t629)
      t636 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t607, -t609, -t
     #608, t610, t624)
      t645 = -0.90D2 * t6 * (t626 * t627 - t634 * t626 * t636) + 0.180D3
     # * t28 * t5 * t626 * t636
      t649 = FJET(XB1, XB2, s, t607, -t608, -t609, t610, t624, t645 * t1
     #01 * t448 / 0.720D3)
      rrgg2gght5s8e1 = t275 * t274 + t378 * t377 + t472 * t471 + t538 * 
     #t537 + t603 * t602 + t649 * t645 * t103 / 0.720D3

      end function



      doubleprecision function rrgg2gght5s8e0
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = x2 * pi
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t15 = log(0.4D1 * t12 * x4)
      t16 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t18 = t15 ** 2
      t19 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t22 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t26 = pi * lh
      t32 = pi ** 2
      t34 = lh ** 2
      t36 = -0.30D2 * t32 + 0.180D3 * t34
      t37 = pi * t36
      t38 = t5 * t19
      t39 = t37 * t38
      t41 = 0.1D1 / x4
      t45 = log(0.4D1 * t12)
      t46 = t45 * pi
      t49 = t45 ** 2
      t50 = t49 * pi
      t56 = rrgg2ggh51J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t82 = x1 ** 2
      t83 = t82 * t9
      t84 = t11 * x4
      t87 = log(0.4D1 * t83 * t84)
      t93 = 0.180D3 * t26 * t38
      t95 = 0.1D1 / x1
      t99 = x3 * t82
      t100 = -0.1D1 + x3
      t104 = log(-0.4D1 * t99 * t12 * t100)
      t108 = log(0.4D1 * t99 * t12)
      t111 = 0.1D1 / x3
      t116 = t83 * t11
      t118 = log(0.4D1 * t116)
      t120 = t118 ** 2
      t134 = x3 * t9
      t137 = log(0.4D1 * t134 * t84)
      t139 = t11 * t100
      t143 = log(-0.4D1 * t134 * t139 * x4)
      t150 = t134 * t11
      t152 = log(0.4D1 * t150)
      t153 = t152 ** 2
      t156 = log(-0.4D1 * t134 * t139)
      t157 = t156 ** 2
      t171 = (0.90D2 * t6 * (t15 * t16 - t18 * t19 / 0.2D1 - t22) - 0.18
     #0D3 * t26 * t5 * (-t16 + t15 * t19) - t39) * t41 / 0.1440D4 - (t37
     # + 0.180D3 * t46 * lh + 0.45D2 * t50) * t5 * t16 / 0.1440D4 - t6 *
     # t56 / 0.16D2 - (pi * (-0.240D3 * zeta3 - 0.120D3 * lh * t34 + 0.6
     #0D2 * lh * t32) - t46 * t36 - 0.90D2 * t50 * lh - 0.15D2 * t49 * t
     #45 * pi) * t5 * t19 / 0.1440D4 - (-0.180D3 * t26 - 0.90D2 * t46) *
     # t5 * t22 / 0.1440D4 - (0.90D2 * t6 * (t16 - t87 * t19) - t93) * t
     #95 * t41 / 0.720D3 + t6 * (-t104 * t19 + t108 * t19) * t111 * t95 
     #/ 0.8D1 - (0.90D2 * t6 * (-t118 * t16 + t120 * t19 / 0.2D1 + t22) 
     #- 0.180D3 * t26 * t5 * (t16 - t118 * t19) + t39) * t95 / 0.720D3 +
     # t6 * (t137 * t19 - t143 * t19) * t111 * t41 / 0.16D2 - (-0.90D2 *
     # t6 * t19 * (-t153 / 0.2D1 + t157 / 0.2D1) + (-0.90D2 * t6 * t16 +
     # t93) * (t152 - t156)) * t111 / 0.1440D4
      t172 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t171)
      t174 = t2 * x4
      t175 = -0.1D1 + x4
      t176 = t2 * t175
      t177 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t174, 0.
     #0D0, -t176, 0.0D0)
      t178 = t84 * t175
      t181 = log(-0.4D1 * t83 * t178)
      t182 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t174, 0.
     #0D0, -t176, 0.0D0)
      t187 = t5 * t182
      t194 = x4 * t175
      t198 = log(0.4D1 * t150 * t194 * t100)
      t202 = log(-0.4D1 * t134 * t178)
      t211 = log(-0.4D1 * t12 * t194)
      t213 = t211 ** 2
      t216 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t174, 0.
     #0D0, -t176, 0.0D0)
      t229 = -(0.90D2 * t6 * (-t177 + t181 * t182) + 0.180D3 * t26 * t18
     #7) * t95 * t41 / 0.720D3 + t6 * (t198 * t182 - t202 * t182) * t111
     # * t41 / 0.16D2 + (0.90D2 * t6 * (-t211 * t177 + t213 * t182 / 0.2
     #D1 + t216) - 0.180D3 * t26 * t5 * (t177 - t211 * t182) + t37 * t18
     #7) * t41 / 0.1440D4
      t230 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t174, -t176, 0.0D0, t229)
      t232 = t2 * x1
      t233 = -0.1D1 + x1
      t234 = t2 * t233
      t235 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #232, -t234, 0.0D0)
      t236 = t233 ** 2
      t237 = t82 * t236
      t241 = log(0.4D1 * t12 * t237 * x4)
      t242 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #232, -t234, 0.0D0)
      t247 = t5 * t242
      t249 = 0.180D3 * t26 * t247
      t263 = log(0.4D1 * t134 * t11 * t82 * t236)
      t274 = log(0.4D1 * t12 * t237)
      t276 = t274 ** 2
      t279 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #232, -t234, 0.0D0)
      t292 = -(0.90D2 * t6 * (-t235 + t241 * t242) + t249) * t95 * t41 /
     # 0.720D3 + t6 * t242 * t111 * t95 * t41 / 0.8D1 + (0.90D2 * t6 * (
     #t235 - t263 * t242) - t249) * t111 * t95 / 0.720D3 - (-0.90D2 * t6
     # * (-t274 * t235 + t276 * t242 / 0.2D1 + t279) + 0.180D3 * t26 * t
     #5 * (t235 - t274 * t242) - t37 * t247) * t95 / 0.720D3
      t293 = FJET(XB1, XB2, s, 0.0D0, t232, 0.0D0, -t234, 0.0D0, t292)
      t295 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t296 = s * t295
      t297 = t1 * x1
      t298 = t296 * t297
      t299 = t1 * t233
      t300 = t299 * x4
      t301 = t296 * t300
      t302 = t299 * t175
      t303 = t296 * t302
      t304 = t295 ** 2
      t306 = t1 ** 2
      t308 = x1 * t233
      t310 = s * t304 * t306 * t308 * x4
      t312 = 0.1D1 / (-0.2D1 + t295)
      t313 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t301, t
     #298, t303, -t310)
      t315 = t304 ** 2
      t320 = log(-0.4D1 * t116 * t194 * t315 * t236)
      t322 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t301, t
     #298, t303, -t310)
      t337 = t95 * t41
      t341 = -(-0.90D2 * t6 * (t312 * t313 - t320 * t312 * t322) + 0.180
     #D3 * t26 * t5 * t312 * t322) * t95 * t41 / 0.720D3 + t6 * t312 * t
     #322 * t111 * t337 / 0.8D1
      t342 = FJET(XB1, XB2, s, 0.0D0, t298, -t301, t303, -t310, t341)
      t344 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t345 = s * t344
      t346 = t297 * x3
      t347 = t345 * t346
      t348 = t297 * t100
      t349 = t345 * t348
      t350 = t345 * t299
      t351 = t344 ** 2
      t355 = s * t351 * t306 * t308 * x3
      t357 = 0.1D1 / (-0.2D1 + t344)
      t359 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t347, 0.0D0, -t
     #349, -t350, -t355)
      t364 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, t347, 0.0D0, -t
     #349, -t350, -t355)
      t366 = t351 ** 2
      t371 = log(-0.4D1 * t150 * t237 * t100 * t366)
      t385 = t6 * t357 * t359 * t111 * t337 / 0.8D1 + (0.90D2 * t6 * (t3
     #57 * t364 - t371 * t357 * t359) - 0.180D3 * t26 * t5 * t357 * t359
     #) * t111 * t95 / 0.720D3
      t386 = FJET(XB1, XB2, s, t347, -t349, 0.0D0, -t350, -t355, t385)
      t388 = KAPPA2(x1, x2, x3, x4, z)
      t389 = s * t388
      t390 = t389 * t346
      t391 = t389 * t348
      t392 = t389 * t300
      t393 = t389 * t302
      t394 = t388 ** 2
      t399 = cos(t7)
      t402 = Sqrt(x3 * t100 * t194)
      t407 = s * t394 * t306 * t308 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D
     #1 * t399 * t402)
      t409 = 0.1D1 / (-0.2D1 + t388)
      t411 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t390, -t392, -t
     #391, t393, t407)
      t413 = t411 * t111 * t337
      t416 = FJET(XB1, XB2, s, t390, -t391, -t392, t393, t407, -t6 * t40
     #9 * t413 / 0.8D1)
      rrgg2gght5s8e0 = t172 * t171 + t230 * t229 + t293 * t292 + t342 * 
     #t341 + t386 * t385 - t416 * pi * t5 * t409 * t413 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s8em1
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

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
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t9 = sin(x2 * pi)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t13 = t10 * t12
      t16 = log(0.4D1 * t13 * x4)
      t17 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t22 = pi * lh
      t25 = 0.180D3 * t22 * t5 * t17
      t27 = 0.1D1 / x4
      t30 = x1 ** 2
      t34 = log(0.4D1 * t30 * t10 * t12)
      t40 = 0.1D1 / x1
      t47 = x3 * t10
      t50 = log(0.4D1 * t47 * t12)
      t51 = -0.1D1 + x3
      t55 = log(-0.4D1 * t47 * t12 * t51)
      t58 = 0.1D1 / x3
      t64 = log(0.4D1 * t13)
      t65 = t64 * pi
      t71 = pi ** 2
      t73 = lh ** 2
      t79 = t64 ** 2
      t86 = rrgg2ggh51J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t89 = (0.90D2 * t6 * (-t7 + t16 * t17) + t25) * t27 / 0.1440D4 - (
     #0.90D2 * t6 * (t7 - t34 * t17) - t25) * t40 / 0.720D3 - t6 * t17 *
     # t40 * t27 / 0.8D1 + t6 * t17 * (t50 - t55) * t58 / 0.16D2 - (-0.1
     #80D3 * t22 - 0.90D2 * t65) * t5 * t7 / 0.1440D4 - (pi * (-0.30D2 *
     # t71 + 0.180D3 * t73) + 0.180D3 * t65 * lh + 0.45D2 * t79 * pi) * 
     #t5 * t17 / 0.1440D4 - t6 * t86 / 0.16D2
      t90 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t89)
      t92 = t2 * x4
      t93 = -0.1D1 + x4
      t94 = t2 * t93
      t95 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t92, 0.0D
     #0, -t94, 0.0D0)
      t99 = log(-0.4D1 * t13 * x4 * t93)
      t100 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t92, 0.0
     #D0, -t94, 0.0D0)
      t115 = (0.90D2 * t6 * (t95 - t99 * t100) - 0.180D3 * t22 * t5 * t1
     #00) * t27 / 0.1440D4 + t6 * t100 * t40 * t27 / 0.8D1
      t116 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t92, -t94, 0.0D0, t115)
      t118 = t2 * x1
      t119 = -0.1D1 + x1
      t120 = t2 * t119
      t121 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #118, -t120, 0.0D0)
      t126 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t
     #118, -t120, 0.0D0)
      t127 = t119 ** 2
      t131 = log(0.4D1 * t13 * t30 * t127)
      t146 = t6 * t121 * t58 * t40 / 0.8D1 - (-0.90D2 * t6 * (t126 - t13
     #1 * t121) + 0.180D3 * t22 * t5 * t121) * t40 / 0.720D3 + t6 * t121
     # * t40 * t27 / 0.8D1
      t147 = FJET(XB1, XB2, s, 0.0D0, t118, 0.0D0, -t120, 0.0D0, t146)
      t149 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t150 = s * t149
      t151 = t1 * x1
      t152 = t150 * t151
      t153 = t1 * t119
      t155 = t150 * t153 * x4
      t157 = t150 * t153 * t93
      t158 = t149 ** 2
      t160 = t1 ** 2
      t162 = x1 * t119
      t164 = s * t158 * t160 * t162 * x4
      t166 = 0.1D1 / (-0.2D1 + t149)
      t168 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t155, t
     #152, t157, -t164)
      t173 = FJET(XB1, XB2, s, 0.0D0, t152, -t155, t157, -t164, t6 * t16
     #6 * t168 * t40 * t27 / 0.8D1)
      t181 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t182 = s * t181
      t184 = t182 * t151 * x3
      t186 = t182 * t151 * t51
      t187 = t182 * t153
      t188 = t181 ** 2
      t192 = s * t188 * t160 * t162 * x3
      t194 = 0.1D1 / (-0.2D1 + t181)
      t196 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, t184, 0.0D0, -t
     #186, -t187, -t192)
      t201 = FJET(XB1, XB2, s, t184, -t186, 0.0D0, -t187, -t192, t6 * t1
     #94 * t196 * t58 * t40 / 0.8D1)
      rrgg2gght5s8em1 = t90 * t89 + t116 * t115 + t147 * t146 + t173 * p
     #i * t5 * t166 * t168 * t40 * t27 / 0.8D1 + t201 * pi * t5 * t194 *
     # t196 * t58 * t40 / 0.8D1

      end function



      doubleprecision function rrgg2gght5s8em2
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t6 = pi * t5
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t8 = 0.1D1 / x1
      t12 = 0.1D1 / x4
      t16 = rrgg2ggh51J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.
     #0D0, t2, 0.0D0)
      t22 = sin(x2 * pi)
      t23 = t22 ** 2
      t24 = z ** 2
      t28 = log(0.4D1 * t23 / t24)
      t35 = -t6 * t7 * t8 / 0.8D1 - t6 * t7 * t12 / 0.16D2 - t6 * t16 / 
     #0.16D2 - (-0.180D3 * pi * lh - 0.90D2 * t28 * pi) * t5 * t7 / 0.14
     #40D4
      t36 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, t35)
      t38 = t2 * x1
      t40 = t2 * (-0.1D1 + x1)
      t41 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #8, -t40, 0.0D0)
      t45 = FJET(XB1, XB2, s, 0.0D0, t38, 0.0D0, -t40, 0.0D0, t6 * t41 *
     # t8 / 0.8D1)
      t51 = t2 * x4
      t53 = t2 * (-0.1D1 + x4)
      t54 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t51, 0.0D
     #0, -t53, 0.0D0)
      t58 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t51, -t53, 0.0D0, t6 * t54 *
     # t12 / 0.16D2)
      rrgg2gght5s8em2 = t36 * t35 + t45 * pi * t5 * t41 * t8 / 0.8D1 + t
     #58 * pi * t5 * t54 * t12 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s8em3
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = s ** 2
      t5 = 0.1D1 / t3 / s
      t7 = rrgg2ggh51J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 0.0
     #D0, t2, 0.0D0)
      t10 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, t2, 0.0D0, -pi * t5 *
     # t7 / 0.16D2)
      rrgg2gght5s8em3 = -t10 * pi * t5 * t7 / 0.16D2

      end function



      doubleprecision function rrgg2gght5s8em4
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
      doubleprecision rrgg2ggh51J1
      doubleprecision rrgg2ggh51J2
      doubleprecision rrgg2ggh51J3
      doubleprecision rrgg2ggh51J4
      doubleprecision rrgg2ggh51J5
      doubleprecision rrgg2ggh51J6

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght5s8em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh51J1
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
      t1 = S13 + S24 + S23 + S14
      t2 = 0.18D2 * t1
      t6 = S24 ** 2
      t8 = S13 ** 2
      t10 = S14 ** 2
      t12 = S23 ** 2
      t14 = S34 ** 2
      t28 = 0.1D1 / S12
      t33 = S12 ** 2
      t42 = t14 ** 2
      rrgg2ggh51J1 = ((t2 * S12 + 0.36D2 * t1 * S34 + 0.9D1 * t6 + 0.9D1
     # * t8 + 0.9D1 * t10 + 0.9D1 * t12 + (t2 * t14 + (0.9D1 * t6 + 0.9D
     #1 * t8 + 0.9D1 * t10 + 0.9D1 * t12) * S34 + 0.9D1 * t8 * S13 + 0.9
     #D1 * t6 * S24 + 0.9D1 * t10 * S14 + 0.9D1 * t12 * S23) * t28) * s 
     #* z + 0.9D1 * t33 * S12 + 0.18D2 * t33 * S34 + 0.27D2 * S12 * t14 
     #+ 0.18D2 * t14 * S34 + 0.9D1 * t42 * t28) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh51J2
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
      t1 = S13 + S24 + S23 + S14
      t2 = 0.18D2 * t1
      t6 = S24 ** 2
      t8 = S13 ** 2
      t10 = S14 ** 2
      t12 = S23 ** 2
      t14 = S34 ** 2
      t28 = 0.1D1 / S12
      t33 = S12 ** 2
      t42 = t14 ** 2
      rrgg2ggh51J2 = ((t2 * S12 + 0.9D1 * t1 * S34 + 0.9D1 * t6 + 0.9D1 
     #* t8 + 0.9D1 * t10 + 0.9D1 * t12 + (t2 * t14 + (0.9D1 * t6 + 0.9D1
     # * t8 + 0.9D1 * t10 + 0.9D1 * t12) * S34 + 0.9D1 * t8 * S13 + 0.9D
     #1 * t6 * S24 + 0.9D1 * t10 * S14 + 0.9D1 * t12 * S23) * t28) * s *
     # z + 0.9D1 * t33 * S12 + 0.18D2 * t33 * S34 + 0.27D2 * S12 * t14 +
     # 0.18D2 * t14 * S34 + 0.9D1 * t42 * t28) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh51J3
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
      t2 = 0.18D2 * S13 + 0.18D2 * S24 + 0.18D2 * S23 + 0.18D2 * S14
      t6 = S24 ** 2
      t8 = S13 ** 2
      t10 = S14 ** 2
      t12 = S23 ** 2
      t14 = S34 ** 2
      t28 = 0.1D1 / S12
      t33 = S12 ** 2
      t42 = t14 ** 2
      rrgg2ggh51J3 = ((t2 * S12 - t2 * S34 + 0.9D1 * t6 + 0.9D1 * t8 + 0
     #.9D1 * t10 + 0.9D1 * t12 + (t2 * t14 + (0.9D1 * t6 + 0.9D1 * t8 + 
     #0.9D1 * t10 + 0.9D1 * t12) * S34 + 0.9D1 * t8 * S13 + 0.9D1 * t6 *
     # S24 + 0.9D1 * t10 * S14 + 0.9D1 * t12 * S23) * t28) * s * z + 0.9
     #D1 * t33 * S12 + 0.18D2 * t33 * S34 + 0.27D2 * S12 * t14 + 0.18D2 
     #* t14 * S34 + 0.9D1 * t42 * t28) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh51J4
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
      t1 = S13 + S24 + S23 + S14
      t2 = 0.18D2 * t1
      t7 = S24 ** 2
      t9 = S13 ** 2
      t11 = S14 ** 2
      t13 = S23 ** 2
      t15 = S34 ** 2
      t29 = 0.1D1 / S12
      t34 = S12 ** 2
      t43 = t15 ** 2
      rrgg2ggh51J4 = ((t2 * S12 - 0.45D2 * t1 * S34 + 0.9D1 * t7 + 0.9D1
     # * t9 + 0.9D1 * t11 + 0.9D1 * t13 + (t2 * t15 + (0.9D1 * t7 + 0.9D
     #1 * t9 + 0.9D1 * t11 + 0.9D1 * t13) * S34 + 0.9D1 * t9 * S13 + 0.9
     #D1 * t7 * S24 + 0.9D1 * t11 * S14 + 0.9D1 * t13 * S23) * t29) * s 
     #* z + 0.9D1 * t34 * S12 + 0.18D2 * t34 * S34 + 0.27D2 * S12 * t15 
     #+ 0.18D2 * t15 * S34 + 0.9D1 * t43 * t29) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh51J5
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
      t1 = S13 + S24 + S23 + S14
      t2 = 0.18D2 * t1
      t7 = S24 ** 2
      t9 = S13 ** 2
      t11 = S14 ** 2
      t13 = S23 ** 2
      t15 = S34 ** 2
      t29 = 0.1D1 / S12
      t34 = S12 ** 2
      t43 = t15 ** 2
      rrgg2ggh51J5 = ((t2 * S12 - 0.72D2 * t1 * S34 + 0.9D1 * t7 + 0.9D1
     # * t9 + 0.9D1 * t11 + 0.9D1 * t13 + (t2 * t15 + (0.9D1 * t7 + 0.9D
     #1 * t9 + 0.9D1 * t11 + 0.9D1 * t13) * S34 + 0.9D1 * t9 * S13 + 0.9
     #D1 * t7 * S24 + 0.9D1 * t11 * S14 + 0.9D1 * t13 * S23) * t29) * s 
     #* z + 0.9D1 * t34 * S12 + 0.18D2 * t34 * S34 + 0.27D2 * S12 * t15 
     #+ 0.18D2 * t15 * S34 + 0.9D1 * t43 * t29) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh51J6
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
      t1 = -S13 - S24 - S23 - S14
      t2 = 0.90D2 * t1
      t6 = S24 ** 2
      t8 = S13 ** 2
      t10 = S14 ** 2
      t12 = S23 ** 2
      t14 = S34 ** 2
      t28 = 0.1D1 / S12
      t33 = S12 ** 2
      t42 = t14 ** 2
      rrgg2ggh51J6 = ((t2 * S12 + 0.315D3 * t1 * S34 - 0.45D2 * t6 - 0.4
     #5D2 * t8 - 0.45D2 * t10 - 0.45D2 * t12 + (t2 * t14 + (-0.45D2 * t6
     # - 0.45D2 * t8 - 0.45D2 * t10 - 0.45D2 * t12) * S34 - 0.45D2 * t8 
     #* S13 - 0.45D2 * t6 * S24 - 0.45D2 * t10 * S14 - 0.45D2 * t12 * S2
     #3) * t28) * s * z - 0.45D2 * t33 * S12 - 0.90D2 * t33 * S34 - 0.13
     #5D3 * S12 * t14 - 0.90D2 * t14 * S34 - 0.45D2 * t42 * t28) / pi * 
     #wd / z

      end function
  
 