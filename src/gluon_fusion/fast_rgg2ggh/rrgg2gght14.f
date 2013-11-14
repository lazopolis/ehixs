  
      subroutine rrgg2gght14
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gght14s1e1  
      doubleprecision rrgg2gght14s1e0  
      doubleprecision rrgg2gght14s1em1  
      doubleprecision rrgg2gght14s1em2  
      doubleprecision rrgg2gght14s1em3  
      doubleprecision rrgg2gght14s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght14s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght14s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght14s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght14s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght14s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght14s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght14s1e1
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
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = wd * z
      t7 = x2 * x3
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t14 = t10 * t12 * t4
      t17 = log(-0.4D1 * t7 * t14)
      t18 = t17 ** 2
      t20 = cos(t8)
      t21 = t20 ** 2
      t22 = t1 ** 2
      t23 = t22 ** 2
      t25 = t4 * x2
      t26 = Sqrt(-t25)
      t27 = t26 ** 2
      t28 = x2 * z
      t30 = (-x2 + 0.1D1 + t28) ** 2
      t31 = t30 ** 2
      t32 = 0.1D1 / t31
      t33 = t27 * t32
      t34 = t21 * t23 * t33
      t41 = pi ** 2
      t43 = lh ** 2
      t45 = -0.30D2 * t41 + 0.180D3 * t43
      t46 = t6 * t45
      t48 = 0.16D2 * t46 * t34
      t50 = 0.1D1 / x3
      t53 = wd * t21
      t57 = t12 * t4
      t60 = log(-0.4D1 * x2 * t10 * t57)
      t61 = t60 * wd
      t63 = (-0.2D1 * wd - t61) * t21
      t66 = t60 ** 2
      t67 = t66 * wd
      t70 = (wd + 0.2D1 * t61 + t67 / 0.2D1) * t21
      t76 = t23 * z
      t106 = x1 ** 2
      t107 = x3 * t106
      t108 = t107 * x2
      t111 = log(-0.4D1 * t108 * t14)
      t115 = t6 * lh
      t120 = 0.1D1 / x1
      t124 = t10 * t106
      t128 = log(-0.4D1 * t124 * t57 * x2)
      t129 = t128 ** 2
      t134 = t53 * t76
      t142 = -(-0.720D3 * t6 * t18 * t34 - 0.2880D4 * t6 * lh * t17 * t3
     #4 - t48) * t50 / 0.320D3 + (-0.180D3 * (0.3D1 * t53 + 0.2D1 * t63 
     #+ t70) * t23 * z * lh + t53 * t76 * (-0.240D3 * zeta3 - 0.120D3 * 
     #t43 * lh + 0.60D2 * lh * t41) + 0.90D2 * (0.4D1 * t53 + 0.3D1 * t6
     #3 + 0.2D1 * t70 + (-t61 - t67 - t66 * t60 * wd / 0.6D1) * t21) * t
     #23 * z + (0.2D1 * t53 + t63) * t23 * z * t45) * t27 * t32 / 0.20D2
     # - (0.1440D4 * t6 * t111 * t34 + 0.2880D4 * t115 * t34) * t50 * t1
     #20 / 0.160D3 - (-0.720D3 * t53 * t23 * z * t129 * t33 - 0.2880D4 *
     # t134 * lh * t128 * t33 - t48) * t120 / 0.160D3
      t143 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t142)
      t145 = 0.2D1 * t7
      t146 = sqrt(x3)
      t147 = t20 * t146
      t148 = -0.1D1 + t146
      t149 = t146 + 0.1D1
      t152 = Sqrt(t25 * t148 * t149)
      t154 = 0.2D1 * t147 * t152
      t156 = t2 * (0.1D1 - x2 - x3 + t145 + t154)
      t158 = t2 * (-x3 + t145 - x2 + t154)
      t162 = t148 * x2
      t163 = t149 * t12 * t162
      t166 = log(0.4D1 * t10 * x3 * t4 * t163)
      t167 = t166 ** 2
      t169 = 0.3D1 * t146
      t170 = t146 * x2
      t172 = 0.5D1 * t170 * z
      t173 = t146 * x3
      t174 = t173 * x2
      t176 = 0.2D1 * t174 * z
      t177 = t20 * x3
      t180 = z * t20
      t184 = 0.2D1 * t146 * z
      t185 = t173 * z
      t186 = 0.2D1 * t174
      t189 = 0.5D1 * t170
      t193 = -t169 - t172 + t176 - 0.2D1 * t177 * t152 - 0.4D1 * t180 * 
     #t152 + t173 + t184 - t185 - t186 + 0.4D1 * t20 * t152 + t189 + 0.2
     #D1 * t177 * t152 * z
      t194 = t193 ** 2
      t198 = x3 * z
      t200 = 0.2D1 * t7 * z
      t202 = (x2 + 0.2D1 * t180 * t146 * t152 - 0.1D1 - t198 - t145 + x3
     # - t28 + t200 - t154) ** 2
      t207 = t194 / t202 / t30 * t22
      t222 = log(0.4D1 * t124 * x3 * t4 * t163)
      t232 = -(0.45D2 * t6 * t167 * t207 + 0.180D3 * t6 * lh * t166 * t2
     #07 + t46 * t207) * t50 / 0.320D3 - (-0.90D2 * t6 * t222 * t207 - 0
     #.180D3 * t115 * t207) * t50 * t120 / 0.160D3
      t233 = FJET(XB1, XB2, s, 0.0D0, t156, 0.0D0, -t158, 0.0D0, t232)
      t235 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t142)
      t237 = FJET(XB1, XB2, s, 0.0D0, -t158, 0.0D0, t156, 0.0D0, t232)
      t239 = -0.1D1 + x1
      t241 = x1 * z
      t242 = 0.1D1 - x1 + t241
      t243 = 0.1D1 / t242
      t245 = t2 * t239 * x2 * t243
      t246 = t2 * x1
      t249 = t4 * s * t1 * t239
      t254 = s * t22 * x2 * x1 * t239 * t243
      t257 = t239 ** 2
      t259 = t12 * t243 * t257 * t4
      t262 = log(-0.4D1 * t7 * t124 * t259)
      t265 = t257 * t239
      t267 = t4 * t242
      t269 = Sqrt(-t267 * x2)
      t270 = t269 ** 2
      t272 = x1 * x2
      t273 = t272 * z
      t275 = (x2 - t272 - 0.1D1 + x1 - t28 + t273 - t241) ** 2
      t276 = t275 ** 2
      t277 = 0.1D1 / t276
      t279 = t265 * t21 * t23 * t270 * t277
      t293 = log(-0.4D1 * x2 * t106 * t10 * t259)
      t294 = t293 ** 2
      t297 = t265 * t270 * t277
      t314 = -(0.1440D4 * t6 * t262 * t242 * t279 + 0.2880D4 * t6 * lh *
     # t242 * t279) * t50 * t120 / 0.160D3 - (-0.720D3 * t134 * t294 * t
     #242 * t297 - 0.2880D4 * t53 * t76 * lh * t293 * t242 * t297 - 0.16
     #D2 * t134 * t45 * t242 * t297) * t120 / 0.160D3
      t315 = FJET(XB1, XB2, s, 0.0D0, -t245, t246, t249, -t254, t314)
      t317 = FJET(XB1, XB2, s, t246, t249, 0.0D0, -t245, -t254, t314)
      t319 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t142)
      t321 = FJET(XB1, XB2, s, t156, 0.0D0, -t158, 0.0D0, 0.0D0, t232)
      t323 = x3 * x1
      t324 = t2 * t323
      t325 = t323 * z
      t326 = t323 * x2
      t327 = t323 * t28
      t330 = Sqrt(t267 * t162 * t149)
      t332 = 0.2D1 * t147 * t330
      t336 = t2 * t239 * (-x3 + t323 - t325 + t145 - t326 + t327 - x2 + 
     #t332) * t243
      t339 = t2 * x1 * t148 * t149
      t340 = 0.1D1 - x1 + t241 - x2 + t272 - t273 - x3 + t323 - t325 + t
     #145 - t326 + t327 + t332
      t343 = t2 * t239 * t340 * t243
      t353 = log(0.4D1 * t243 * t257 * t124 * x3 * t4 * t149 * t12 * t14
     #8 * x2)
      t354 = t20 * t330
      t356 = x1 * t173
      t357 = t146 * t106
      t358 = t146 * x1
      t360 = t330 * z
      t366 = t330 * x1
      t375 = x2 * t11
      t380 = t106 * t11
      t383 = -0.4D1 * t354 + t356 + t357 - 0.4D1 * t358 - t184 + t185 + 
     #t186 - t189 + t169 + 0.2D1 * t177 * t360 * x1 - 0.2D1 * t177 * t36
     #0 - 0.2D1 * t177 * t366 - 0.4D1 * t180 * t366 + 0.4D1 * t174 * t24
     #1 - 0.11D2 * t358 * t28 + 0.3D1 * t358 * t375 + 0.6D1 * t357 * t28
     # - 0.3D1 * t380 * t170
      t384 = t106 * t173
      t413 = -0.2D1 * t384 * t28 - t356 * t375 + t384 * t375 - 0.2D1 * t
     #146 * t11 * x1 - 0.2D1 * t356 * z + t356 * t11 + t384 * x2 - 0.3D1
     # * t356 * x2 - 0.2D1 * t357 * z - 0.3D1 * t357 * x2 + t380 * t146 
     #+ 0.8D1 * t358 * x2 + 0.6D1 * t358 * z + 0.2D1 * t177 * t330 + 0.4
     #D1 * t180 * t330 + 0.4D1 * t354 * x1 - t173 + t172 - t176
      t415 = (t383 + t413) ** 2
      t422 = t146 * t330
      t431 = 0.1D1 + t145 + 0.4D1 * t327 - t323 * t375 - 0.2D1 * t180 * 
     #t422 + 0.2D1 * t180 * t422 * x1 - 0.2D1 * t107 * t28 + t380 * t7 +
     # t332 + t28 + t272 + t198
      t437 = t323 - x1 - t273 - x2 - x3 + t241 - 0.2D1 * t147 * t366 + t
     #323 * t11 + t108 - t200 - 0.3D1 * t326 - 0.2D1 * t325
      t439 = (t431 + t437) ** 2
      t443 = 0.1D1 / t275 * t242 * t239 / t439 * t22
      t450 = -0.90D2 * t6 * t353 * t415 * t443 - 0.180D3 * t6 * lh * t41
     #5 * t443
      t453 = t450 * t50 * t120 / 0.160D3
      t454 = FJET(XB1, XB2, s, t324, t336, -t339, -t343, -t254, -t453)
      t456 = t50 * t120
      t459 = FJET(XB1, XB2, s, t249, t246, -t245, 0.0D0, -t254, t314)
      t461 = FJET(XB1, XB2, s, t336, t324, -t343, -t339, -t254, -t453)
      t465 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t142)
      t467 = FJET(XB1, XB2, s, -t158, 0.0D0, t156, 0.0D0, 0.0D0, t232)
      t469 = FJET(XB1, XB2, s, -t339, -t343, t324, t336, -t254, -t453)
      t473 = FJET(XB1, XB2, s, -t245, 0.0D0, t249, t246, -t254, t314)
      t475 = FJET(XB1, XB2, s, -t343, -t339, t336, t324, -t254, -t453)
      rrgg2gght14s1e1 = t143 * t142 + t233 * t232 + t235 * t142 + t237 *
     # t232 + t315 * t314 + t317 * t314 + t319 * t142 + t321 * t232 - t4
     #54 * t450 * t456 / 0.160D3 + t459 * t314 - t461 * t450 * t456 / 0.
     #160D3 + t465 * t142 + t467 * t232 - t469 * t450 * t456 / 0.160D3 +
     # t473 * t314 - t475 * t450 * t456 / 0.160D3

      end function



      doubleprecision function rrgg2gght14s1e0
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
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = wd * z
      t7 = x2 * x3
      t8 = x4 * pi
      t9 = Sin(t8)
      t10 = t9 ** 2
      t11 = z ** 2
      t12 = 0.1D1 / t11
      t17 = log(-0.4D1 * t7 * t10 * t12 * t4)
      t19 = cos(t8)
      t20 = t19 ** 2
      t21 = t1 ** 2
      t22 = t21 ** 2
      t23 = t20 * t22
      t24 = t4 * x2
      t25 = Sqrt(-t24)
      t26 = t25 ** 2
      t27 = x2 * z
      t29 = (-x2 + 0.1D1 + t27) ** 2
      t30 = t29 ** 2
      t31 = 0.1D1 / t30
      t32 = t26 * t31
      t33 = t23 * t32
      t36 = t6 * lh
      t38 = 0.2880D4 * t36 * t33
      t40 = 0.1D1 / x3
      t43 = t6 * t23
      t44 = 0.1D1 / x1
      t45 = t40 * t44
      t49 = wd * t20
      t51 = x1 ** 2
      t53 = t12 * t4
      t57 = log(-0.4D1 * t10 * t51 * t53 * x2)
      t70 = log(-0.4D1 * x2 * t10 * t53)
      t71 = t70 * wd
      t73 = (-0.2D1 * wd - t71) * t20
      t82 = t70 ** 2
      t92 = pi ** 2
      t94 = lh ** 2
      t103 = -(0.1440D4 * t6 * t17 * t33 + t38) * t40 / 0.320D3 + 0.9D1 
     #* t43 * t32 * t45 - (0.1440D4 * t49 * t22 * z * t57 * t32 + t38) *
     # t44 / 0.160D3 + (-0.180D3 * (0.2D1 * t49 + t73) * t22 * z * lh + 
     #0.90D2 * (0.3D1 * t49 + 0.2D1 * t73 + (wd + 0.2D1 * t71 + t82 * wd
     # / 0.2D1) * t20) * t22 * z + t49 * t22 * z * (-0.30D2 * t92 + 0.18
     #0D3 * t94)) * t26 * t31 / 0.20D2
      t104 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t103)
      t106 = 0.2D1 * t7
      t107 = sqrt(x3)
      t108 = t19 * t107
      t109 = -0.1D1 + t107
      t110 = t107 + 0.1D1
      t113 = Sqrt(t24 * t109 * t110)
      t115 = 0.2D1 * t108 * t113
      t117 = t2 * (0.1D1 - x2 - x3 + t106 + t115)
      t119 = t2 * (-x3 + t106 - x2 + t115)
      t123 = t109 * x2
      t127 = log(0.4D1 * t10 * x3 * t4 * t110 * t12 * t123)
      t129 = 0.3D1 * t107
      t130 = t107 * x2
      t132 = 0.5D1 * t130 * z
      t133 = t107 * x3
      t134 = t133 * x2
      t136 = 0.2D1 * t134 * z
      t137 = t19 * x3
      t140 = z * t19
      t144 = 0.2D1 * t107 * z
      t145 = t133 * z
      t146 = 0.2D1 * t134
      t149 = 0.5D1 * t130
      t153 = -t129 - t132 + t136 - 0.2D1 * t137 * t113 - 0.4D1 * t140 * 
     #t113 + t133 + t144 - t145 - t146 + 0.4D1 * t19 * t113 + t149 + 0.2
     #D1 * t137 * t113 * z
      t154 = t153 ** 2
      t158 = x3 * z
      t160 = 0.2D1 * t7 * z
      t162 = (x2 + 0.2D1 * t140 * t107 * t113 - 0.1D1 - t158 - t106 + x3
     # - t27 + t160 - t115) ** 2
      t164 = t154 / t162
      t166 = 0.1D1 / t29 * t21
      t167 = t164 * t166
      t179 = -(-0.90D2 * t6 * t127 * t167 - 0.180D3 * t36 * t167) * t40 
     #/ 0.320D3 - 0.9D1 / 0.16D2 * t6 * t164 * t166 * t45
      t180 = FJET(XB1, XB2, s, 0.0D0, t117, 0.0D0, -t119, 0.0D0, t179)
      t182 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t103)
      t184 = FJET(XB1, XB2, s, 0.0D0, -t119, 0.0D0, t117, 0.0D0, t179)
      t186 = -0.1D1 + x1
      t188 = x1 * z
      t189 = 0.1D1 - x1 + t188
      t190 = 0.1D1 / t189
      t192 = t2 * t186 * x2 * t190
      t193 = t2 * x1
      t196 = t4 * s * t1 * t186
      t201 = s * t21 * x2 * x1 * t186 * t190
      t202 = t186 ** 2
      t203 = t202 * t186
      t207 = t4 * t189
      t209 = Sqrt(-t207 * x2)
      t210 = t209 ** 2
      t211 = t22 * t210
      t212 = x1 * x2
      t213 = t212 * z
      t215 = (x2 - t212 - 0.1D1 + x1 - t27 + t213 - t188) ** 2
      t216 = t215 ** 2
      t217 = 0.1D1 / t216
      t229 = log(-0.4D1 * x2 * t51 * t10 * t53 * t202 * t190)
      t246 = 0.9D1 * t6 * t189 * t203 * t20 * t211 * t217 * t40 * t44 - 
     #(0.1440D4 * t43 * t229 * t189 * t203 * t210 * t217 + 0.2880D4 * t6
     # * lh * t189 * t203 * t20 * t211 * t217) * t44 / 0.160D3
      t247 = FJET(XB1, XB2, s, 0.0D0, -t192, t193, t196, -t201, t246)
      t249 = FJET(XB1, XB2, s, t193, t196, 0.0D0, -t192, -t201, t246)
      t251 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t103)
      t253 = FJET(XB1, XB2, s, t117, 0.0D0, -t119, 0.0D0, 0.0D0, t179)
      t255 = x3 * x1
      t256 = t2 * t255
      t257 = t255 * z
      t258 = t255 * x2
      t259 = t255 * t27
      t262 = Sqrt(t207 * t123 * t110)
      t264 = 0.2D1 * t108 * t262
      t268 = t2 * t186 * (-x3 + t255 - t257 + t106 - t258 + t259 - x2 + 
     #t264) * t190
      t271 = t2 * x1 * t109 * t110
      t272 = 0.1D1 - x1 + t188 - x2 + t212 - t213 - x3 + t255 - t257 + t
     #106 - t258 + t259 + t264
      t275 = t2 * t186 * t272 * t190
      t276 = t262 * z
      t282 = t262 * x1
      t289 = t107 * x1
      t292 = x2 * t11
      t295 = t107 * t51
      t298 = t51 * t11
      t301 = t51 * t133
      t304 = x1 * t133
      t306 = t132 - t136 - t133 + 0.2D1 * t137 * t276 * x1 - t144 + t145
     # + t146 - t149 - 0.2D1 * t137 * t276 - 0.2D1 * t137 * t282 - 0.4D1
     # * t140 * t282 + 0.4D1 * t134 * t188 - 0.11D2 * t289 * t27 + 0.3D1
     # * t289 * t292 + 0.6D1 * t295 * t27 - 0.3D1 * t298 * t130 - 0.2D1 
     #* t301 * t27 - t304 * t292
      t330 = t19 * t262
      t335 = t301 * t292 + t129 - 0.2D1 * t107 * t11 * x1 - 0.2D1 * t304
     # * z + t304 * t11 + t301 * x2 - 0.3D1 * t304 * x2 - 0.2D1 * t295 *
     # z - 0.3D1 * t295 * x2 + t298 * t107 + 0.8D1 * t289 * x2 + 0.6D1 *
     # t289 * z + 0.2D1 * t137 * t262 + 0.4D1 * t140 * t262 + 0.4D1 * t3
     #30 * x1 - 0.4D1 * t330 + t304 + t295 - 0.4D1 * t289
      t337 = (t306 + t335) ** 2
      t338 = 0.1D1 / t215
      t344 = t107 * t262
      t350 = x3 * t51
      t354 = 0.1D1 + t106 + 0.4D1 * t259 - t255 * t292 - 0.2D1 * t140 * 
     #t344 + 0.2D1 * t140 * t344 * x1 - 0.2D1 * t350 * t27 + t298 * t7 +
     # t264 + t27 + t212 + t158
      t361 = t255 - x1 - t213 - x2 - x3 + t188 - 0.2D1 * t108 * t282 + t
     #255 * t11 + t350 * x2 - t160 - 0.3D1 * t258 - 0.2D1 * t257
      t363 = (t354 + t361) ** 2
      t364 = 0.1D1 / t363
      t367 = t21 * t40 * t44
      t370 = 0.9D1 / 0.16D2 * t6 * t337 * t338 * t189 * t186 * t364 * t3
     #67
      t371 = FJET(XB1, XB2, s, t256, t268, -t271, -t275, -t201, -t370)
      t374 = z * t337 * t338
      t378 = t189 * t186 * t364 * t367
      t381 = FJET(XB1, XB2, s, t196, t193, -t192, 0.0D0, -t201, t246)
      t383 = FJET(XB1, XB2, s, t268, t256, -t275, -t271, -t201, -t370)
      t388 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t103)
      t390 = FJET(XB1, XB2, s, -t119, 0.0D0, t117, 0.0D0, 0.0D0, t179)
      t392 = FJET(XB1, XB2, s, -t271, -t275, t256, t268, -t201, -t370)
      t397 = FJET(XB1, XB2, s, -t192, 0.0D0, t196, t193, -t201, t246)
      t399 = FJET(XB1, XB2, s, -t275, -t271, t268, t256, -t201, -t370)
      rrgg2gght14s1e0 = t104 * t103 + t180 * t179 + t182 * t103 + t184 *
     # t179 + t247 * t246 + t249 * t246 + t251 * t103 + t253 * t179 - 0.
     #9D1 / 0.16D2 * t371 * wd * t374 * t378 + t381 * t246 - 0.9D1 / 0.1
     #6D2 * t383 * wd * t374 * t378 + t388 * t103 + t390 * t179 - 0.9D1 
     #/ 0.16D2 * t392 * wd * t374 * t378 + t397 * t246 - 0.9D1 / 0.16D2 
     #* t399 * wd * t374 * t378

      end function



      doubleprecision function rrgg2gght14s1em1
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
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t6 = wd * z
      t7 = x4 * pi
      t8 = cos(t7)
      t9 = t8 ** 2
      t10 = t6 * t9
      t11 = t1 ** 2
      t12 = t11 ** 2
      t13 = t4 * x2
      t14 = Sqrt(-t13)
      t15 = t14 ** 2
      t16 = t12 * t15
      t17 = x2 * z
      t19 = (-x2 + 0.1D1 + t17) ** 2
      t20 = t19 ** 2
      t21 = 0.1D1 / t20
      t22 = 0.1D1 / x1
      t27 = wd * t9
      t34 = Sin(t7)
      t35 = t34 ** 2
      t37 = z ** 2
      t42 = log(-0.4D1 * x2 * t35 / t37 * t4)
      t54 = 0.1D1 / x3
      t59 = 0.9D1 * t10 * t16 * t21 * t22 + (-0.180D3 * t27 * t12 * z * 
     #lh + 0.90D2 * (0.2D1 * t27 + (-0.2D1 * wd - t42 * wd) * t9) * t12 
     #* z) * t15 * t21 / 0.20D2 + 0.9D1 / 0.2D1 * t10 * t16 * t21 * t54
      t60 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t59)
      t62 = x2 * x3
      t63 = 0.2D1 * t62
      t64 = sqrt(x3)
      t70 = Sqrt(t13 * (-0.1D1 + t64) * (t64 + 0.1D1))
      t72 = 0.2D1 * t8 * t64 * t70
      t74 = t2 * (0.1D1 - x2 - x3 + t63 + t72)
      t76 = t2 * (-x3 + t63 - x2 + t72)
      t78 = t64 * x2
      t81 = t64 * x3
      t82 = t81 * x2
      t85 = t8 * x3
      t88 = z * t8
      t101 = -0.3D1 * t64 - 0.5D1 * t78 * z + 0.2D1 * t82 * z - 0.2D1 * 
     #t85 * t70 - 0.4D1 * t88 * t70 + t81 + 0.2D1 * t64 * z - t81 * z - 
     #0.2D1 * t82 + 0.4D1 * t8 * t70 + 0.5D1 * t78 + 0.2D1 * t85 * t70 *
     # z
      t102 = t101 ** 2
      t111 = (x2 + 0.2D1 * t88 * t64 * t70 - 0.1D1 - x3 * z - t63 + x3 -
     # t17 + 0.2D1 * t62 * z - t72) ** 2
      t116 = 0.1D1 / t111 / t19 * t11 * t54
      t118 = 0.9D1 / 0.32D2 * t6 * t102 * t116
      t119 = FJET(XB1, XB2, s, 0.0D0, t74, 0.0D0, -t76, 0.0D0, -t118)
      t121 = z * t102
      t125 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t59)
      t127 = FJET(XB1, XB2, s, 0.0D0, -t76, 0.0D0, t74, 0.0D0, -t118)
      t132 = -0.1D1 + x1
      t134 = x1 * z
      t135 = 0.1D1 - x1 + t134
      t136 = 0.1D1 / t135
      t138 = t2 * t132 * x2 * t136
      t139 = t2 * x1
      t142 = t4 * s * t1 * t132
      t147 = s * t11 * x2 * x1 * t132 * t136
      t148 = t132 ** 2
      t149 = t148 * t132
      t155 = Sqrt(-t4 * t135 * x2)
      t156 = t155 ** 2
      t157 = x1 * x2
      t160 = (x2 - t157 - 0.1D1 + x1 - t17 + t157 * z - t134) ** 2
      t161 = t160 ** 2
      t165 = t9 * t12 * t156 / t161 * t22
      t167 = 0.9D1 * t6 * t135 * t149 * t165
      t168 = FJET(XB1, XB2, s, 0.0D0, -t138, t139, t142, -t147, t167)
      t171 = z * t135 * t149
      t175 = FJET(XB1, XB2, s, t139, t142, 0.0D0, -t138, -t147, t167)
      t180 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t59)
      t182 = FJET(XB1, XB2, s, t74, 0.0D0, -t76, 0.0D0, 0.0D0, -t118)
      t187 = FJET(XB1, XB2, s, t142, t139, -t138, 0.0D0, -t147, t167)
      t192 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t59)
      t194 = FJET(XB1, XB2, s, -t138, 0.0D0, t142, t139, -t147, t167)
      t199 = FJET(XB1, XB2, s, -t76, 0.0D0, t74, 0.0D0, 0.0D0, -t118)
      rrgg2gght14s1em1 = t60 * t59 - 0.9D1 / 0.32D2 * t119 * wd * t121 *
     # t116 + t125 * t59 - 0.9D1 / 0.32D2 * t127 * wd * t121 * t116 + 0.
     #9D1 * t168 * wd * t171 * t165 + 0.9D1 * t175 * wd * t171 * t165 + 
     #t180 * t59 - 0.9D1 / 0.32D2 * t182 * wd * t121 * t116 + 0.9D1 * t1
     #87 * wd * t171 * t165 + t192 * t59 + 0.9D1 * t194 * wd * t171 * t1
     #65 - 0.9D1 / 0.32D2 * t199 * wd * t121 * t116

      end function



      doubleprecision function rrgg2gght14s1em2
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
      t3 = t2 * x2
      t4 = -0.1D1 + x2
      t5 = t2 * t4
      t8 = cos(x4 * pi)
      t9 = t8 ** 2
      t11 = t1 ** 2
      t12 = t11 ** 2
      t14 = Sqrt(-t4 * x2)
      t15 = t14 ** 2
      t19 = (-x2 + 0.1D1 + x2 * z) ** 2
      t20 = t19 ** 2
      t21 = 0.1D1 / t20
      t24 = 0.9D1 / 0.2D1 * wd * z * t9 * t12 * t15 * t21
      t25 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t24)
      t30 = t9 * t12 * t15 * t21
      t32 = FJET(XB1, XB2, s, 0.0D0, -t5, 0.0D0, t3, 0.0D0, t24)
      t36 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t24)
      t40 = FJET(XB1, XB2, s, -t5, 0.0D0, t3, 0.0D0, 0.0D0, t24)
      rrgg2gght14s1em2 = 0.9D1 / 0.2D1 * t25 * wd * z * t30 + 0.9D1 / 0.
     #2D1 * t32 * wd * z * t30 + 0.9D1 / 0.2D1 * t36 * wd * z * t30 + 0.
     #9D1 / 0.2D1 * t40 * wd * z * t30

      end function



      doubleprecision function rrgg2gght14s1em3
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght14s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght14s1em4
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

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght14s1em4 = 0.0D0

      end function
