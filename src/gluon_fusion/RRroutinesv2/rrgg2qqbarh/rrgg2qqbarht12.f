      subroutine rrgg2qqbarht12
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2qqbarhsoftt12
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2qqbarhhardt12
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2qqbarhhardt12
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhhardt12s1e1  
      doubleprecision rrgg2qqbarhhardt12s1e0  
      doubleprecision rrgg2qqbarhhardt12s1em1  
      doubleprecision rrgg2qqbarhhardt12s1em2  
      doubleprecision rrgg2qqbarhhardt12s1em3  
      doubleprecision rrgg2qqbarhhardt12s1em4  
      doubleprecision rrgg2qqbarhhardt12s2e1  
      doubleprecision rrgg2qqbarhhardt12s2e0  
      doubleprecision rrgg2qqbarhhardt12s2em1  
      doubleprecision rrgg2qqbarhhardt12s2em2  
      doubleprecision rrgg2qqbarhhardt12s2em3  
      doubleprecision rrgg2qqbarhhardt12s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt12s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt12s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt12s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt12s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt12s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt12s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt12s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt12s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt12s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt12s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt12s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhhardt12s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhhardt12s1e1
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
      t4 = -0.1D1 + x3
      t6 = z * wd
      t7 = t6 * nf
      t8 = x2 * x3
      t9 = x4 * pi
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t18 = log(-0.4D1 * t8 * t14 * t4)
      t19 = t18 ** 2
      t20 = cos(t9)
      t21 = t20 ** 2
      t23 = x3 * t4
      t24 = Sqrt(-t23)
      t25 = t24 ** 2
      t29 = 0.90D2 * t7
      t30 = nf * lh
      t32 = 0.180D3 * t6 * t30
      t33 = -t29 + t32
      t38 = lh ** 2
      t40 = pi ** 2
      t42 = -0.180D3 * t38 + 0.30D2 * t40
      t43 = nf * t42
      t45 = t32 - t29 + t6 * t43
      t50 = 0.1D1 / x2
      t55 = log(-0.4D1 * t14 * t23)
      t58 = (0.2D1 - t55) * z * t21
      t59 = t25 * wd
      t60 = t59 * t30
      t64 = t55 ** 2
      t68 = (0.3D1 - 0.2D1 * t55 + t64 / 0.2D1) * z * t21
      t69 = t59 * nf
      t72 = z * t21
      t73 = t72 * t25
      t74 = wd * nf
      t75 = t74 * t42
      t100 = x1 ** 2
      t101 = x2 * t100
      t104 = t13 * x3 * t4
      t107 = log(-0.4D1 * t101 * t11 * t104)
      t112 = t33 * t21
      t117 = 0.1D1 / x1
      t120 = t72 * wd
      t124 = log(-0.4D1 * t100 * t11 * t104)
      t125 = t124 ** 2
      t131 = 0.90D2 * t72 * t74
      t134 = 0.180D3 * t72 * t74 * lh
      t135 = -t131 + t134
      t139 = t134 - t131 + t72 * t75
      t144 = (-0.720D3 * t7 * t19 * t21 * t25 - 0.16D2 * t33 * t18 * t21
     # * t25 + 0.16D2 * t45 * t21 * t25) * t50 / 0.960D3 - 0.3D1 * t58 *
     # t60 + 0.3D1 / 0.2D1 * t68 * t69 - t73 * t75 / 0.60D2 + 0.3D1 * t6
     #8 * t60 + t73 * t74 * (-0.60D2 * lh * t40 + 0.240D3 * zeta3 + 0.12
     #0D3 * t38 * lh) / 0.60D2 - 0.3D1 / 0.2D1 * (0.4D1 - 0.3D1 * t55 + 
     #t64 - t64 * t55 / 0.6D1) * z * t21 * t69 + t58 * t59 * t43 / 0.60D
     #2 + (0.1440D4 * t7 * t107 * t21 * t25 + 0.16D2 * t112 * t25) * t50
     # * t117 / 0.480D3 + (-0.45D2 * t120 * nf * t125 * t25 - t135 * t12
     #4 * t25 + t139 * t25) * t117 / 0.30D2
      t145 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #144)
      t147 = -0.1D1 + x1
      t148 = x3 * x1
      t149 = t148 * z
      t150 = 0.2D1 * t8
      t151 = t8 * x1
      t152 = x1 * z
      t153 = t8 * t152
      t154 = sqrt(x2)
      t155 = t20 * t154
      t156 = -0.1D1 + t154
      t157 = x3 * t156
      t158 = t154 + 0.1D1
      t159 = 0.1D1 - x1 + t152
      t163 = Sqrt(t157 * t158 * t159 * t4)
      t165 = 0.2D1 * t155 * t163
      t168 = 0.1D1 / t159
      t171 = t2 * t148
      t172 = x2 * x1
      t173 = t172 * z
      t174 = 0.1D1 - x1 + t152 - x2 + t172 - t173 - x3 + t148 - t149 + t
     #150 - t151 + t153 + t165
      t178 = t4 * s
      t180 = t178 * t1 * x1
      t181 = t1 ** 2
      t187 = t13 * t156
      t188 = t158 * x3
      t191 = t147 ** 2
      t198 = log(0.4D1 * t187 * t188 * t4 * t11 * t191 * x2 * t168 * t10
     #0)
      t200 = x2 * z
      t202 = (-t200 + t173 + x2 - t172 - 0.1D1 + x1 - t152) ** 2
      t204 = t20 * x2
      t205 = t163 * z
      t211 = t20 * t163
      t214 = t154 * x1
      t217 = x3 * t154
      t219 = 0.2D1 * t217 * z
      t222 = x3 * t100
      t226 = t154 * t100
      t231 = t154 * x2
      t232 = t231 * x3
      t237 = t100 * t231
      t240 = x1 * t231
      t244 = 0.4D1 * t232 * z
      t245 = t231 * z
      t248 = 0.3D1 * t154
      t249 = 0.2D1 * t231
      t250 = 0.4D1 * t204 * t205 * x1 + 0.4D1 * t204 * t163 + 0.4D1 * t2
     #11 * x1 + 0.6D1 * t214 * z + t219 + 0.10D2 * t217 * x1 - 0.4D1 * t
     #222 * t154 - t214 * t12 - 0.4D1 * t226 * z + 0.2D1 * t226 * t12 + 
     #0.2D1 * t232 * t100 - 0.6D1 * t232 * x1 + 0.2D1 * t237 * z + t240 
     #* t12 - t237 * t12 - t244 - 0.4D1 * t245 * x1 + t248 - t249
      t265 = t154 * z
      t280 = 0.6D1 * t217
      t281 = 0.4D1 * t232
      t282 = 0.2D1 * t245
      t287 = -0.4D1 * t211 * t152 - 0.4D1 * t204 * t163 * x1 - 0.4D1 * t
     #204 * t205 + 0.8D1 * t232 * t152 - 0.12D2 * t217 * t152 + 0.2D1 * 
     #x3 * t12 * t214 + 0.8D1 * t222 * t265 - 0.4D1 * t222 * t12 * t154 
     #- 0.2D1 * t232 * t12 * x1 - 0.4D1 * t232 * t100 * z + 0.2D1 * t232
     # * t12 * t100 - t280 + t281 + t282 - 0.4D1 * t211 - t265 - 0.5D1 *
     # t214 + 0.3D1 * t240 - t237 + 0.2D1 * t226
      t289 = (t250 + t287) ** 2
      t290 = 0.1D1 / t202 * t289
      t296 = 0.90D2 * t7 * t198 * t168 * t290 + t33 * t168 * t290
      t300 = FJET(XB1, XB2, s, t2 * t147 * (-x3 + t148 - t149 + t150 - t
     #151 + t153 - x2 + t165) * t168, t171, -t2 * t147 * t174 * t168, -t
     #180, -s * t181 * x2 * x1 * t147 * t168, t296 * t50 * t117 / 0.480D
     #3)
      t307 = Sqrt(t157 * t158 * t4)
      t309 = 0.2D1 * t155 * t307
      t319 = log(0.4D1 * t187 * t158 * t23 * t11 * x2)
      t320 = t319 ** 2
      t328 = t280 - t281 - t282 + 0.4D1 * t20 * t307 + t249 - t219 - t24
     #8 + 0.4D1 * t204 * t307 * z - 0.4D1 * t204 * t307 + t244 + t265
      t329 = t328 ** 2
      t332 = (t200 - x2 + 0.1D1) ** 2
      t333 = 0.1D1 / t332
      t350 = log(0.4D1 * t187 * t188 * t4 * t11 * t101)
      t361 = (0.45D2 * t7 * t320 * t329 * t333 + t33 * t319 * t329 * t33
     #3 - t45 * t329 * t333) * t50 / 0.960D3 + (-0.90D2 * t7 * t350 * t3
     #29 * t333 - t33 * t329 * t333) * t50 * t117 / 0.480D3
      t362 = FJET(XB1, XB2, s, -t2 * (-x3 + t150 - x2 + t309), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t150 + t309), 0.0D0, 0.0D0, t361)
      t365 = t1 * t147
      t368 = t100 * t168
      t374 = log(-0.4D1 * t368 * t14 * t191 * x2 * t23)
      t378 = Sqrt(-x3 * t159 * t4)
      t379 = t378 ** 2
      t380 = t168 * t379
      t395 = log(-0.4D1 * t368 * t11 * t13 * t191 * t23)
      t396 = t395 ** 2
      t408 = (-0.1440D4 * t7 * t374 * t21 * t380 - 0.16D2 * t112 * t380)
     # * t50 * t117 / 0.480D3 + (0.45D2 * t120 * nf * t396 * t380 + t135
     # * t395 * t380 - t139 * t168 * t379) * t117 / 0.30D2
      t409 = FJET(XB1, XB2, s, -x3 * s * t365, t171, t178 * t365, -t180,
     # 0.0D0, t408)
      rrgg2qqbarhhardt12s1e1 = t145 * t144 + t300 * t296 * t50 * t117 / 
     #0.480D3 + t362 * t361 + t409 * t408

      end function



      doubleprecision function rrgg2qqbarhhardt12s1e0
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
      t4 = -0.1D1 + x3
      t6 = z * wd
      t7 = t6 * nf
      t8 = x2 * x3
      t9 = x4 * pi
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t18 = log(-0.4D1 * t8 * t14 * t4)
      t19 = cos(t9)
      t20 = t19 ** 2
      t22 = x3 * t4
      t23 = Sqrt(-t22)
      t24 = t23 ** 2
      t29 = nf * lh
      t32 = -0.90D2 * t7 + 0.180D3 * t6 * t29
      t37 = 0.1D1 / x2
      t41 = 0.1D1 / x1
      t42 = t37 * t41
      t46 = z * t20
      t47 = t46 * wd
      t48 = x1 ** 2
      t54 = log(-0.4D1 * t48 * t11 * t13 * x3 * t4)
      t59 = wd * nf
      t60 = t46 * t59
      t62 = t59 * lh
      t65 = -0.90D2 * t60 + 0.180D3 * t46 * t62
      t70 = t46 * t24
      t75 = log(-0.4D1 * t14 * t22)
      t78 = (0.2D1 - t75) * z * t20
      t79 = t24 * wd
      t80 = t79 * nf
      t87 = t75 ** 2
      t94 = lh ** 2
      t96 = pi ** 2
      t102 = (0.1440D4 * t7 * t18 * t20 * t24 + 0.16D2 * t32 * t20 * t24
     #) * t37 / 0.960D3 - 0.3D1 * t7 * t20 * t24 * t42 + (0.90D2 * t47 *
     # nf * t54 * t24 + t65 * t24) * t41 / 0.30D2 - 0.3D1 * t70 * t62 + 
     #0.3D1 / 0.2D1 * t78 * t80 + 0.3D1 * t78 * t79 * t29 - 0.3D1 / 0.2D
     #1 * (0.3D1 - 0.2D1 * t75 + t87 / 0.2D1) * z * t20 * t80 + t70 * t5
     #9 * (-0.180D3 * t94 + 0.30D2 * t96) / 0.60D2
      t103 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #102)
      t105 = -0.1D1 + x1
      t106 = x3 * x1
      t107 = t106 * z
      t108 = 0.2D1 * t8
      t109 = t8 * x1
      t110 = x1 * z
      t111 = t8 * t110
      t112 = sqrt(x2)
      t113 = t19 * t112
      t114 = -0.1D1 + t112
      t115 = x3 * t114
      t116 = t112 + 0.1D1
      t117 = 0.1D1 - x1 + t110
      t121 = Sqrt(t115 * t116 * t117 * t4)
      t123 = 0.2D1 * t113 * t121
      t126 = 0.1D1 / t117
      t129 = t2 * t106
      t130 = x2 * x1
      t131 = t130 * z
      t132 = 0.1D1 - x1 + t110 - x2 + t130 - t131 - x3 + t106 - t107 + t
     #108 - t109 + t111 + t123
      t136 = t4 * s
      t138 = t136 * t1 * x1
      t139 = t1 ** 2
      t147 = x2 * z
      t149 = (-t147 + t131 + x2 - t130 - 0.1D1 + x1 - t110) ** 2
      t150 = 0.1D1 / t149
      t151 = x3 * t112
      t152 = 0.6D1 * t151
      t153 = t112 * x2
      t154 = t153 * x3
      t155 = 0.4D1 * t154
      t156 = t153 * z
      t157 = 0.2D1 * t156
      t158 = t19 * t121
      t160 = t112 * z
      t161 = t112 * x1
      t163 = t153 * x1
      t165 = t48 * t153
      t166 = t112 * t48
      t177 = x3 * t48
      t190 = t19 * x2
      t191 = t121 * z
      t197 = -t152 + t155 + t157 - 0.4D1 * t158 - t160 - 0.5D1 * t161 + 
     #0.3D1 * t163 - t165 + 0.2D1 * t166 + 0.2D1 * t154 * t12 * t48 - 0.
     #4D1 * t154 * t48 * z - 0.2D1 * t154 * t12 * x1 - 0.4D1 * t177 * t1
     #2 * t112 + 0.8D1 * t177 * t160 + 0.2D1 * x3 * t12 * t161 - 0.12D2 
     #* t151 * t110 + 0.8D1 * t154 * t110 - 0.4D1 * t190 * t191 - 0.4D1 
     #* t190 * t121 * x1
      t203 = 0.3D1 * t112
      t205 = 0.4D1 * t154 * z
      t215 = 0.2D1 * t151 * z
      t233 = 0.2D1 * t153
      t234 = -0.4D1 * t158 * t110 + 0.4D1 * t190 * t191 * x1 + t203 - t2
     #05 - 0.4D1 * t156 * x1 + 0.4D1 * t190 * t121 + 0.4D1 * t158 * x1 +
     # 0.6D1 * t161 * z + t215 + 0.10D2 * t151 * x1 - 0.4D1 * t177 * t11
     #2 - t161 * t12 - 0.4D1 * t166 * z + 0.2D1 * t166 * t12 + 0.2D1 * t
     #154 * t48 - 0.6D1 * t154 * x1 + 0.2D1 * t165 * z + t163 * t12 - t1
     #65 * t12 - t233
      t236 = (t197 + t234) ** 2
      t241 = FJET(XB1, XB2, s, t2 * t105 * (-x3 + t106 - t107 + t108 - t
     #109 + t111 - x2 + t123) * t126, t129, -t2 * t105 * t132 * t126, -t
     #138, -s * t139 * x2 * x1 * t105 * t126, -0.3D1 / 0.16D2 * t6 * nf 
     #* t126 * t150 * t236 * t42)
      t252 = Sqrt(t115 * t116 * t4)
      t254 = 0.2D1 * t113 * t252
      t265 = log(0.4D1 * t13 * t114 * t116 * t22 * t11 * x2)
      t273 = t152 - t155 - t157 + 0.4D1 * t19 * t252 + t233 - t215 - t20
     #3 + 0.4D1 * t190 * t252 * z - 0.4D1 * t190 * t252 + t205 + t160
      t274 = t273 ** 2
      t277 = (t147 - x2 + 0.1D1) ** 2
      t278 = 0.1D1 / t277
      t291 = (-0.90D2 * t7 * t265 * t274 * t278 - t32 * t274 * t278) * t
     #37 / 0.960D3 + 0.3D1 / 0.16D2 * t7 * t274 * t278 * t42
      t292 = FJET(XB1, XB2, s, -t2 * (-x3 + t108 - x2 + t254), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t108 + t254), 0.0D0, 0.0D0, t291)
      t295 = t1 * t105
      t300 = Sqrt(-x3 * t117 * t4)
      t301 = t300 ** 2
      t302 = t126 * t301
      t308 = t105 ** 2
      t313 = log(-0.4D1 * t48 * t126 * t11 * t13 * t308 * t22)
      t323 = 0.3D1 * t60 * t302 * t42 + (-0.90D2 * t47 * nf * t313 * t30
     #2 - t65 * t126 * t301) * t41 / 0.30D2
      t324 = FJET(XB1, XB2, s, -x3 * s * t295, t129, t136 * t295, -t138,
     # 0.0D0, t323)
      rrgg2qqbarhhardt12s1e0 = t103 * t102 - 0.3D1 / 0.16D2 * t241 * z *
     # t59 * t126 * t150 * t236 * t37 * t41 + t292 * t291 + t324 * t323

      end function



      doubleprecision function rrgg2qqbarhhardt12s1em1
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
      t4 = -0.1D1 + x3
      t7 = z * wd * nf
      t8 = x4 * pi
      t9 = cos(t8)
      t10 = t9 ** 2
      t11 = x3 * t4
      t12 = Sqrt(-t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t15 = 0.1D1 / x1
      t19 = z * t10
      t21 = t13 * wd * nf
      t25 = wd * nf
      t29 = Sin(t8)
      t30 = t29 ** 2
      t31 = z ** 2
      t36 = log(-0.4D1 * t30 / t31 * t11)
      t42 = 0.1D1 / x2
      t46 = -0.3D1 * t7 * t14 * t15 + 0.3D1 / 0.2D1 * t19 * t21 + 0.3D1 
     #* t19 * t13 * t25 * lh - 0.3D1 / 0.2D1 * (0.2D1 - t36) * z * t10 *
     # t21 - 0.3D1 / 0.2D1 * t7 * t14 * t42
      t47 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t4
     #6)
      t51 = t1 * (-0.1D1 + x1)
      t55 = t4 * s
      t60 = 0.1D1 - x1 + x1 * z
      t65 = Sqrt(-x3 * t60 * t4)
      t66 = t65 ** 2
      t68 = t10 / t60 * t66 * t15
      t71 = FJET(XB1, XB2, s, -x3 * s * t51, t2 * x1 * x3, t55 * t51, -t
     #55 * t1 * x1, 0.0D0, 0.3D1 * t7 * t68)
      t77 = 0.2D1 * x2 * x3
      t78 = sqrt(x2)
      t85 = Sqrt(x3 * (-0.1D1 + t78) * (t78 + 0.1D1) * t4)
      t87 = 0.2D1 * t9 * t78 * t85
      t92 = x3 * t78
      t94 = t78 * x2
      t95 = t94 * x3
      t105 = t9 * x2
      t114 = 0.6D1 * t92 - 0.4D1 * t95 - 0.2D1 * t94 * z + 0.4D1 * t9 * 
     #t85 + 0.2D1 * t94 - 0.2D1 * t92 * z - 0.3D1 * t78 + 0.4D1 * t105 *
     # t85 * z - 0.4D1 * t105 * t85 + 0.4D1 * t95 * z + t78 * z
      t115 = t114 ** 2
      t118 = (x2 * z - x2 + 0.1D1) ** 2
      t119 = 0.1D1 / t118
      t124 = FJET(XB1, XB2, s, -t2 * (-x3 + t77 - x2 + t87), 0.0D0, t2 *
     # (0.1D1 - x2 - x3 + t77 + t87), 0.0D0, 0.0D0, 0.3D1 / 0.32D2 * t7 
     #* t115 * t119 * t42)
      rrgg2qqbarhhardt12s1em1 = t47 * t46 + 0.3D1 * t71 * z * t25 * t68 
     #+ 0.3D1 / 0.32D2 * t124 * z * wd * nf * t115 * t119 * t42

      end function



      doubleprecision function rrgg2qqbarhhardt12s1em2
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
      t2 = (-0.1D1 + z) * s
      t4 = -0.1D1 + x3
      t7 = cos(x4 * pi)
      t8 = t7 ** 2
      t11 = Sqrt(-x3 * t4)
      t12 = t11 ** 2
      t14 = t12 * wd * nf
      t17 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, -0
     #.3D1 / 0.2D1 * z * t8 * t14)
      rrgg2qqbarhhardt12s1em2 = -0.3D1 / 0.2D1 * t17 * z * t8 * t14

      end function



      doubleprecision function rrgg2qqbarhhardt12s1em3
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
      rrgg2qqbarhhardt12s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhhardt12s1em4
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
      rrgg2qqbarhhardt12s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarhhardt12s2e1
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
      t4 = -0.1D1 + x3
      t6 = wd * nf
      t7 = sqrt(x2)
      t8 = -0.1D1 + t7
      t9 = t7 + 0.1D1
      t10 = t8 * t9
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t4 * t14
      t16 = z ** 2
      t18 = 0.1D1 / t16 / z
      t19 = x2 * t18
      t23 = log(0.4D1 * t10 * x3 * t15 * t19)
      t24 = t23 ** 2
      t25 = x3 * t7
      t27 = 0.2D1 * t25 * z
      t28 = t7 * z
      t30 = cos(t12)
      t31 = x3 * t8
      t35 = Sqrt(t31 * t9 * z * t4)
      t39 = (t27 - t28 + 0.2D1 * t25 + 0.4D1 * t30 * t35 - t7) ** 2
      t46 = log(-0.4D1 * t19 * t14 * x3 * t4)
      t47 = t46 ** 2
      t48 = t30 ** 2
      t50 = x3 * z
      t52 = Sqrt(-t50 * t4)
      t53 = t52 ** 2
      t59 = 0.90D2 * t6
      t60 = t6 * lh
      t61 = 0.180D3 * t60
      t62 = -t59 + t61
      t69 = lh ** 2
      t71 = pi ** 2
      t73 = -0.180D3 * t69 + 0.30D2 * t71
      t74 = t6 * t73
      t78 = -t39 + 0.16D2 * t48 * t53
      t81 = 0.1D1 / x2
      t84 = x3 * t18
      t87 = log(-0.4D1 * t84 * t15)
      t89 = (0.2D1 - t87) * t48
      t93 = t87 ** 2
      t96 = (0.3D1 - 0.2D1 * t87 + t93 / 0.2D1) * t48
      t99 = t48 * wd
      t101 = t99 * nf * t73
      t123 = x1 ** 2
      t124 = x2 * t123
      t129 = log(-0.4D1 * t124 * t14 * t84 * t4)
      t133 = x3 * t4
      t140 = log(0.4D1 * t10 * t133 * t14 * x2 * t123 * t18)
      t149 = 0.1D1 / x1
      t152 = x3 * t123
      t157 = log(-0.4D1 * t152 * t14 * t18 * t4)
      t158 = t157 ** 2
      t163 = t99 * nf
      t164 = 0.90D2 * t163
      t167 = 0.180D3 * t99 * nf * lh
      t168 = -t164 + t167
      t172 = t167 - t164 + t101
      t178 = (-0.90D2 * t6 * (-t24 * t39 / 0.2D1 + 0.8D1 * t47 * t48 * t
     #53) + t62 * (t23 * t39 - 0.16D2 * t46 * t48 * t53) + (t61 - t59 + 
     #t74) * t78) * t81 / 0.960D3 + (-0.180D3 * t89 * t60 + 0.90D2 * t96
     # * t6 - t101 + 0.180D3 * t96 * t60 + t99 * nf * (-0.60D2 * lh * t7
     #1 + 0.240D3 * zeta3 + 0.120D3 * t69 * lh) - 0.90D2 * (0.4D1 - 0.3D
     #1 * t87 + t93 - t93 * t87 / 0.6D1) * t48 * t6 + t89 * t74) * t53 /
     # 0.60D2 - (-0.90D2 * t6 * (0.16D2 * t129 * t48 * t53 - t140 * t39)
     # - t62 * t78) * t81 * t149 / 0.480D3 + (-0.720D3 * t99 * nf * t158
     # * t53 - 0.16D2 * t168 * t157 * t53 + 0.16D2 * t172 * t53) * t149 
     #/ 0.480D3
      t179 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #178)
      t181 = x3 * x1
      t184 = -0.1D1 + x1
      t185 = t1 * t184
      t186 = x3 * s * t185
      t187 = t4 * s
      t190 = t187 * t185
      t191 = 0.1D1 / t16
      t194 = x1 * z
      t195 = -z - x1 + t194
      t196 = 0.1D1 / t195
      t197 = t184 ** 2
      t199 = t133 * t196 * t197
      t202 = log(0.4D1 * t124 * t14 * t191 * t199)
      t207 = Sqrt(x3 * t195 * t4)
      t208 = t207 ** 2
      t209 = t196 * t208
      t225 = log(0.4D1 * t123 * t14 * t191 * t199)
      t226 = t225 ** 2
      t232 = z * t196
      t242 = -(-0.1440D4 * t6 * t202 * z * t48 * t209 - 0.16D2 * t62 * z
     # * t48 * t196 * t208) * t81 * t149 / 0.480D3 + (-0.720D3 * t163 * 
     #t226 * z * t209 - 0.16D2 * t168 * t225 * t232 * t208 + 0.16D2 * t1
     #72 * z * t209) * t149 / 0.480D3
      t243 = FJET(XB1, XB2, s, t2 * t181, -t186, -t187 * t1 * x1, t190, 
     #0.0D0, t242)
      t245 = t181 * z
      t246 = x2 * x3
      t247 = t246 * z
      t248 = t246 * x1
      t249 = t246 * t194
      t254 = Sqrt(-t31 * t9 * t195 * t4)
      t256 = 0.2D1 * t30 * t7 * t254
      t262 = x2 * x1
      t263 = t262 * z
      t264 = z + x1 - t194 - x2 * z - t262 + t263 - t50 - t181 + t245 + 
     #t247 + t248 - t249 + t246 + t256
      t268 = t1 ** 2
      t284 = log(-0.4D1 * t196 * x2 * t123 * t197 * t14 * t191 * t8 * t9
     # * x3 * t4)
      t286 = t7 * x1
      t287 = t7 * x2
      t288 = t287 * x1
      t289 = t7 * t16
      t290 = t123 * t287
      t291 = t7 * t123
      t293 = t30 * t254
      t312 = t287 * x3
      t315 = -t286 + t288 - t289 + t290 - 0.2D1 * t291 + 0.4D1 * t293 * 
     #z + 0.4D1 * t293 * x1 - 0.2D1 * t286 * z + 0.2D1 * t25 * x1 + 0.4D
     #1 * t152 * t7 + 0.3D1 * t286 * t16 + 0.4D1 * t291 * z - 0.2D1 * t2
     #91 * t16 + 0.2D1 * t25 * t16 - 0.2D1 * t312 * t123
      t322 = t30 * x2
      t323 = t254 * x1
      t349 = -0.2D1 * t312 * x1 - 0.2D1 * t290 * z - t288 * t16 + t290 *
     # t16 + 0.4D1 * t322 * t323 * z - 0.4D1 * t293 * t194 - 0.4D1 * t32
     #2 * t323 + 0.4D1 * t25 * t194 - 0.6D1 * x3 * t16 * t286 - 0.8D1 * 
     #t152 * t28 + 0.4D1 * t152 * t289 + 0.2D1 * t312 * t16 * x1 + 0.4D1
     # * t312 * t123 * z - 0.2D1 * t312 * t16 * t123 + t27 - t28
      t351 = (t315 + t349) ** 2
      t353 = (z + t263 - t262 + x1 - t194) ** 2
      t354 = 0.1D1 / t353
      t363 = 0.90D2 * t6 * t284 * t351 * t354 * t232 + t62 * t351 * t354
     # * z * t196
      t367 = FJET(XB1, XB2, s, t2 * x1 * (-t50 - t181 + t245 + t247 + t2
     #48 - t249 - x2 + t246 + t256) * t196, -t186, -t2 * x1 * t264 * t19
     #6, t190, s * t268 * x2 * x1 * t184 * t196, -t363 * t81 * t149 / 0.
     #480D3)
      rrgg2qqbarhhardt12s2e1 = t179 * t178 + t243 * t242 - t367 * t363 *
     # t81 * t149 / 0.480D3

      end function



      doubleprecision function rrgg2qqbarhhardt12s2e0
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
      t4 = -0.1D1 + x3
      t6 = wd * nf
      t7 = sqrt(x2)
      t8 = -0.1D1 + t7
      t9 = t7 + 0.1D1
      t12 = x4 * pi
      t13 = Sin(t12)
      t14 = t13 ** 2
      t15 = t4 * t14
      t16 = z ** 2
      t18 = 0.1D1 / t16 / z
      t19 = x2 * t18
      t23 = log(0.4D1 * t8 * t9 * x3 * t15 * t19)
      t24 = x3 * t7
      t26 = 0.2D1 * t24 * z
      t27 = t7 * z
      t29 = cos(t12)
      t30 = x3 * t8
      t34 = Sqrt(t30 * t9 * z * t4)
      t38 = (t26 - t27 + 0.2D1 * t24 + 0.4D1 * t29 * t34 - t7) ** 2
      t44 = log(-0.4D1 * t19 * t14 * x3 * t4)
      t45 = t29 ** 2
      t47 = x3 * z
      t49 = Sqrt(-t47 * t4)
      t50 = t49 ** 2
      t57 = t6 * lh
      t62 = -t38 + 0.16D2 * t45 * t50
      t65 = 0.1D1 / x2
      t70 = 0.1D1 / x1
      t74 = t45 * wd
      t75 = x1 ** 2
      t76 = x3 * t75
      t81 = log(-0.4D1 * t76 * t14 * t18 * t4)
      t86 = t74 * nf
      t90 = 0.180D3 * t74 * nf * lh
      t91 = -0.90D2 * t86 + t90
      t100 = log(-0.4D1 * x3 * t18 * t15)
      t102 = (0.2D1 - t100) * t45
      t108 = t100 ** 2
      t114 = lh ** 2
      t116 = pi ** 2
      t124 = (-0.90D2 * t6 * (t23 * t38 - 0.16D2 * t44 * t45 * t50) + (-
     #0.90D2 * t6 + 0.180D3 * t57) * t62) * t65 / 0.960D3 - 0.3D1 / 0.16
     #D2 * t6 * t62 * t65 * t70 + (0.1440D4 * t74 * nf * t81 * t50 + 0.1
     #6D2 * t91 * t50) * t70 / 0.480D3 + (-t90 + 0.90D2 * t102 * t6 + 0.
     #180D3 * t102 * t57 - 0.90D2 * (0.3D1 - 0.2D1 * t100 + t108 / 0.2D1
     #) * t45 * t6 + t74 * nf * (-0.180D3 * t114 + 0.30D2 * t116)) * t50
     # / 0.60D2
      t125 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #124)
      t127 = x3 * x1
      t130 = -0.1D1 + x1
      t131 = t1 * t130
      t132 = x3 * s * t131
      t133 = t4 * s
      t136 = t133 * t131
      t139 = x1 * z
      t140 = -z - x1 + t139
      t141 = 0.1D1 / t140
      t144 = Sqrt(x3 * t140 * t4)
      t145 = t144 ** 2
      t146 = t141 * t145
      t147 = t65 * t70
      t155 = t130 ** 2
      t160 = log(0.4D1 * t75 * t14 / t16 * x3 * t4 * t141 * t155)
      t171 = -0.3D1 * t6 * z * t45 * t146 * t147 + (0.1440D4 * t86 * t16
     #0 * z * t146 + 0.16D2 * t91 * z * t146) * t70 / 0.480D3
      t172 = FJET(XB1, XB2, s, t2 * t127, -t132, -t133 * t1 * x1, t136, 
     #0.0D0, t171)
      t174 = t127 * z
      t175 = x2 * x3
      t176 = t175 * z
      t177 = t175 * x1
      t178 = t175 * t139
      t183 = Sqrt(-t30 * t9 * t140 * t4)
      t185 = 0.2D1 * t29 * t7 * t183
      t191 = x2 * x1
      t192 = t191 * z
      t193 = z + x1 - t139 - x2 * z - t191 + t192 - t47 - t127 + t174 + 
     #t176 + t177 - t178 + t175 + t185
      t197 = t1 ** 2
      t203 = t29 * t183
      t206 = t29 * x2
      t207 = t183 * x1
      t213 = t7 * x1
      t218 = t7 * t16
      t221 = t7 * x2
      t222 = t221 * x3
      t244 = -0.4D1 * t203 * t139 - 0.4D1 * t206 * t207 + 0.4D1 * t24 * 
     #t139 - 0.6D1 * x3 * t16 * t213 - 0.8D1 * t76 * t27 + 0.4D1 * t76 *
     # t218 + 0.2D1 * t222 * t16 * x1 + 0.4D1 * t222 * t75 * z - 0.2D1 *
     # t222 * t16 * t75 + 0.4D1 * t203 * z + 0.4D1 * t203 * x1 - 0.2D1 *
     # t213 * z + 0.2D1 * t24 * x1 + 0.4D1 * t76 * t7 + 0.3D1 * t213 * t
     #16
      t245 = t7 * t75
      t256 = t75 * t221
      t259 = x1 * t221
      t266 = 0.4D1 * t245 * z - 0.2D1 * t245 * t16 + 0.2D1 * t24 * t16 -
     # 0.2D1 * t222 * t75 - 0.2D1 * t222 * x1 - 0.2D1 * t256 * z - t259 
     #* t16 + t256 * t16 + t26 - t213 + t259 - t218 + t256 - 0.2D1 * t24
     #5 + 0.4D1 * t206 * t207 * z - t27
      t268 = (t244 + t266) ** 2
      t270 = (z + t192 - t191 + x1 - t139) ** 2
      t271 = 0.1D1 / t270
      t278 = FJET(XB1, XB2, s, t2 * x1 * (-t47 - t127 + t174 + t176 + t1
     #77 - t178 - x2 + t175 + t185) * t141, -t132, -t2 * x1 * t193 * t14
     #1, t136, s * t197 * x2 * x1 * t130 * t141, 0.3D1 / 0.16D2 * t6 * t
     #268 * t271 * z * t141 * t147)
      rrgg2qqbarhhardt12s2e0 = t125 * t124 + t172 * t171 + 0.3D1 / 0.16D
     #2 * t278 * wd * nf * t268 * t271 * z * t141 * t65 * t70

      end function



      doubleprecision function rrgg2qqbarhhardt12s2em1
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
      t4 = -0.1D1 + x3
      t6 = x4 * pi
      t7 = cos(t6)
      t8 = t7 ** 2
      t9 = t8 * wd
      t12 = Sqrt(-x3 * z * t4)
      t13 = t12 ** 2
      t15 = 0.1D1 / x1
      t19 = t9 * nf
      t24 = z ** 2
      t28 = Sin(t6)
      t29 = t28 ** 2
      t33 = log(-0.4D1 * x3 / t24 / z * t29 * t4)
      t36 = wd * nf
      t42 = sqrt(x2)
      t43 = x3 * t42
      t54 = Sqrt(x3 * (-0.1D1 + t42) * (t42 + 0.1D1) * z * t4)
      t58 = (0.2D1 * t43 * z - t42 * z + 0.2D1 * t43 + 0.4D1 * t7 * t54 
     #- t42) ** 2
      t66 = -0.3D1 * t9 * nf * t13 * t15 + (0.90D2 * t19 + 0.180D3 * t9 
     #* nf * lh - 0.90D2 * (0.2D1 - t33) * t8 * t36) * t13 / 0.60D2 - 0.
     #3D1 / 0.32D2 * t36 * (-t58 + 0.16D2 * t8 * t13) / x2
      t67 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t6
     #6)
      t73 = t1 * (-0.1D1 + x1)
      t75 = t4 * s
      t80 = -z - x1 + x1 * z
      t85 = Sqrt(x3 * t80 * t4)
      t86 = t85 ** 2
      t88 = z / t80 * t86 * t15
      t91 = FJET(XB1, XB2, s, t2 * x1 * x3, -x3 * s * t73, -t75 * t1 * x
     #1, t73 * t75, 0.0D0, -0.3D1 * t19 * t88)
      rrgg2qqbarhhardt12s2em1 = t67 * t66 - 0.3D1 * t91 * t8 * t36 * t88

      end function



      doubleprecision function rrgg2qqbarhhardt12s2em2
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
      t2 = (-0.1D1 + z) * s
      t4 = -0.1D1 + x3
      t7 = cos(x4 * pi)
      t8 = t7 ** 2
      t12 = Sqrt(-x3 * z * t4)
      t13 = t12 ** 2
      t17 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, -0
     #.3D1 / 0.2D1 * t8 * wd * nf * t13)
      rrgg2qqbarhhardt12s2em2 = -0.3D1 / 0.2D1 * t17 * t8 * wd * nf * t1
     #3

      end function



      doubleprecision function rrgg2qqbarhhardt12s2em3
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
      rrgg2qqbarhhardt12s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhhardt12s2em4
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
      rrgg2qqbarhhardt12s2em4 = 0.0D0

      end function
  
      subroutine rrgg2qqbarhsoftt12
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhsoftt12s1e1  
      doubleprecision rrgg2qqbarhsoftt12s1e0  
      doubleprecision rrgg2qqbarhsoftt12s1em1  
      doubleprecision rrgg2qqbarhsoftt12s1em2  
      doubleprecision rrgg2qqbarhsoftt12s1em3  
      doubleprecision rrgg2qqbarhsoftt12s1em4  
      doubleprecision rrgg2qqbarhsoftt12s2e1  
      doubleprecision rrgg2qqbarhsoftt12s2e0  
      doubleprecision rrgg2qqbarhsoftt12s2em1  
      doubleprecision rrgg2qqbarhsoftt12s2em2  
      doubleprecision rrgg2qqbarhsoftt12s2em3  
      doubleprecision rrgg2qqbarhsoftt12s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt12s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt12s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt12s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt12s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt12s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt12s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt12s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt12s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt12s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt12s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt12s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarhsoftt12s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhsoftt12s1e1
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
      t1 = wd * nf
      t3 = -0.1D1 + x3
      t5 = x4 * pi
      t6 = Sin(t5)
      t7 = t6 ** 2
      t8 = sqrt(x2)
      t9 = -0.1D1 + t8
      t11 = t8 + 0.1D1
      t15 = log(0.4D1 * x2 * x3 * t3 * t7 * t9 * t11)
      t16 = t15 ** 2
      t19 = cos(t5)
      t23 = Sqrt(x3 * t9 * t11 * t3)
      t27 = (0.2D1 * t8 * x3 + 0.2D1 * t19 * t23 - t8) ** 2
      t30 = x3 * t3
      t33 = log(-0.4D1 * x2 * t7 * t30)
      t34 = t33 ** 2
      t35 = t19 ** 2
      t37 = Sqrt(-t30)
      t38 = t37 ** 2
      t44 = 0.180D3 * lh
      t55 = lh ** 2
      t56 = 0.180D3 * t55
      t57 = pi ** 2
      t58 = 0.30D2 * t57
      t62 = t35 * t38
      t68 = 0.1D1 / x2
      t73 = t7 * x3 * t3
      t75 = log(-0.4D1 * t73)
      t76 = 0.90D2 * t75
      t81 = -0.1D1 - t75
      t84 = t75 ** 2
      t85 = 0.45D2 * t84
      t110 = x1 ** 2
      t111 = x2 * t110
      t114 = log(-0.4D1 * t111 * t73)
      t119 = (-0.1D1 + x1) ** 2
      t124 = t3 * t7 * t9 * t11
      t127 = log(0.4D1 * t111 * t119 * x3 * t124)
      t133 = log(0.4D1 * t111 * x3 * t124)
      t137 = t30 * t119
      t140 = log(-0.4D1 * t111 * t7 * t137)
      t146 = 0.1D1 / x1
      t150 = t110 * t7
      t153 = log(-0.4D1 * t150 * t137)
      t154 = t153 ** 2
      t157 = log(-0.4D1 * t150 * t30)
      t158 = t157 ** 2
      t176 = -(-0.90D2 * t1 * (t16 * t27 - 0.4D1 * t34 * t35 * t38) + (-
     #0.90D2 + t44) * wd * nf * (-0.2D1 * t15 * t27 + 0.8D1 * t33 * t35 
     #* t38) + (-0.90D2 + t44 - t56 + t58) * wd * nf * (0.2D1 * t27 - 0.
     #8D1 * t62)) * t68 / 0.480D3 + (-0.360D3 * t62 + 0.3D1 * (t44 + 0.9
     #0D2 + t76) * t35 * t38 + 0.2D1 * (0.180D3 * t81 * lh - t76 - t85 -
     # t56 + t58) * t35 * t38 + (0.180D3 * (t75 + t84 / 0.2D1) * lh - 0.
     #60D2 * lh * t57 + 0.240D3 * zeta3 + 0.120D3 * t55 * lh + t85 + 0.1
     #5D2 * t84 * t75 + t81 * (-t56 + t58)) * t35 * t38) * wd * nf / 0.6
     #0D2 - 0.3D1 / 0.8D1 * t1 * (-0.8D1 * t114 * t35 * t38 - 0.2D1 * t1
     #27 * t27 + 0.2D1 * t133 * t27 + 0.8D1 * t140 * t35 * t38) * t68 * 
     #t146 - (-0.90D2 * t62 * t1 * (t154 / 0.2D1 - t158 / 0.2D1) + (-0.1
     #80D3 * t62 + (t44 + 0.90D2) * t35 * t38) * wd * nf * (-t153 + t157
     #)) * t146 / 0.30D2
      t177 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t176)
      rrgg2qqbarhsoftt12s1e1 = t177 * t176

      end function



      doubleprecision function rrgg2qqbarhsoftt12s1e0
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
      t3 = -0.1D1 + x3
      t5 = x4 * pi
      t6 = Sin(t5)
      t7 = t6 ** 2
      t8 = sqrt(x2)
      t9 = -0.1D1 + t8
      t11 = t8 + 0.1D1
      t15 = log(0.4D1 * x2 * x3 * t3 * t7 * t9 * t11)
      t18 = cos(t5)
      t22 = Sqrt(x3 * t9 * t11 * t3)
      t26 = (0.2D1 * t8 * x3 + 0.2D1 * t18 * t22 - t8) ** 2
      t30 = x3 * t3
      t33 = log(-0.4D1 * x2 * t7 * t30)
      t34 = t18 ** 2
      t36 = Sqrt(-t30)
      t37 = t36 ** 2
      t43 = 0.180D3 * lh
      t47 = t34 * t37
      t57 = x1 ** 2
      t58 = t57 * t7
      t60 = (-0.1D1 + x1) ** 2
      t64 = log(-0.4D1 * t58 * t30 * t60)
      t67 = log(-0.4D1 * t58 * t30)
      t78 = log(-0.4D1 * t7 * x3 * t3)
      t79 = 0.90D2 * t78
      t87 = t78 ** 2
      t89 = lh ** 2
      t91 = pi ** 2
      t100 = -(-0.90D2 * wd * nf * (-0.2D1 * t15 * t26 + 0.8D1 * t33 * t
     #34 * t37) + (-0.90D2 + t43) * wd * nf * (0.2D1 * t26 - 0.8D1 * t47
     #)) / x2 / 0.480D3 + 0.3D1 * t47 * wd * nf * (-t64 + t67) / x1 + (-
     #0.270D3 * t47 + 0.2D1 * (t43 + 0.90D2 + t79) * t34 * t37 + (0.180D
     #3 * (-0.1D1 - t78) * lh - t79 - 0.45D2 * t87 - 0.180D3 * t89 + 0.3
     #0D2 * t91) * t34 * t37) * wd * nf / 0.60D2
      t101 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t100)
      rrgg2qqbarhsoftt12s1e0 = t101 * t100

      end function



      doubleprecision function rrgg2qqbarhsoftt12s1em1
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
      t1 = x4 * pi
      t2 = cos(t1)
      t3 = t2 ** 2
      t4 = -0.1D1 + x3
      t6 = Sqrt(-x3 * t4)
      t7 = t6 ** 2
      t8 = t3 * t7
      t11 = Sin(t1)
      t12 = t11 ** 2
      t16 = log(-0.4D1 * t12 * x3 * t4)
      t26 = sqrt(x2)
      t34 = Sqrt(x3 * (-0.1D1 + t26) * (t26 + 0.1D1) * t4)
      t38 = (0.2D1 * t26 * x3 + 0.2D1 * t2 * t34 - t26) ** 2
      t46 = (-0.180D3 * t8 + (0.180D3 * lh + 0.90D2 + 0.90D2 * t16) * t3
     # * t7) * wd * nf / 0.60D2 + 0.3D1 / 0.16D2 * wd * nf * (0.2D1 * t3
     #8 - 0.8D1 * t8) / x2
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t46)
      rrgg2qqbarhsoftt12s1em1 = t47 * t46

      end function



      doubleprecision function rrgg2qqbarhsoftt12s1em2
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
      t2 = cos(x4 * pi)
      t3 = t2 ** 2
      t6 = Sqrt(-x3 * (-0.1D1 + x3))
      t7 = t6 ** 2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -0.3D1 
     #/ 0.2D1 * t3 * t7 * wd * nf)
      rrgg2qqbarhsoftt12s1em2 = -0.3D1 / 0.2D1 * t12 * t3 * t7 * wd * nf

      end function



      doubleprecision function rrgg2qqbarhsoftt12s1em3
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
      rrgg2qqbarhsoftt12s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt12s1em4
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
      rrgg2qqbarhsoftt12s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarhsoftt12s2e1
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
      t1 = wd * nf
      t3 = -0.1D1 + x3
      t5 = x4 * pi
      t6 = Sin(t5)
      t7 = t6 ** 2
      t8 = sqrt(x2)
      t9 = -0.1D1 + t8
      t11 = t8 + 0.1D1
      t15 = log(0.4D1 * x2 * x3 * t3 * t7 * t9 * t11)
      t16 = t15 ** 2
      t19 = cos(t5)
      t23 = Sqrt(x3 * t9 * t11 * t3)
      t27 = (0.2D1 * t8 * x3 + 0.2D1 * t19 * t23 - t8) ** 2
      t30 = x3 * t3
      t33 = log(-0.4D1 * x2 * t7 * t30)
      t34 = t33 ** 2
      t35 = t19 ** 2
      t37 = Sqrt(-t30)
      t38 = t37 ** 2
      t44 = 0.180D3 * lh
      t55 = lh ** 2
      t56 = 0.180D3 * t55
      t57 = pi ** 2
      t58 = 0.30D2 * t57
      t62 = t35 * t38
      t68 = 0.1D1 / x2
      t73 = t7 * x3 * t3
      t75 = log(-0.4D1 * t73)
      t76 = 0.90D2 * t75
      t81 = -0.1D1 - t75
      t84 = t75 ** 2
      t85 = 0.45D2 * t84
      t110 = x1 ** 2
      t111 = x2 * t110
      t114 = log(-0.4D1 * t111 * t73)
      t119 = (-0.1D1 + x1) ** 2
      t124 = t3 * t7 * t9 * t11
      t127 = log(0.4D1 * t111 * t119 * x3 * t124)
      t133 = log(0.4D1 * t111 * x3 * t124)
      t137 = t30 * t119
      t140 = log(-0.4D1 * t111 * t7 * t137)
      t146 = 0.1D1 / x1
      t150 = t110 * t7
      t153 = log(-0.4D1 * t150 * t137)
      t154 = t153 ** 2
      t157 = log(-0.4D1 * t150 * t30)
      t158 = t157 ** 2
      t176 = -(-0.90D2 * t1 * (t16 * t27 - 0.4D1 * t34 * t35 * t38) + (-
     #0.90D2 + t44) * wd * nf * (-0.2D1 * t15 * t27 + 0.8D1 * t33 * t35 
     #* t38) + (-0.90D2 + t44 - t56 + t58) * wd * nf * (0.2D1 * t27 - 0.
     #8D1 * t62)) * t68 / 0.480D3 + (-0.360D3 * t62 + 0.3D1 * (t44 + 0.9
     #0D2 + t76) * t35 * t38 + 0.2D1 * (0.180D3 * t81 * lh - t76 - t85 -
     # t56 + t58) * t35 * t38 + (0.180D3 * (t75 + t84 / 0.2D1) * lh - 0.
     #60D2 * lh * t57 + 0.240D3 * zeta3 + 0.120D3 * t55 * lh + t85 + 0.1
     #5D2 * t84 * t75 + t81 * (-t56 + t58)) * t35 * t38) * wd * nf / 0.6
     #0D2 - 0.3D1 / 0.8D1 * t1 * (-0.8D1 * t114 * t35 * t38 - 0.2D1 * t1
     #27 * t27 + 0.2D1 * t133 * t27 + 0.8D1 * t140 * t35 * t38) * t68 * 
     #t146 - (-0.90D2 * t62 * t1 * (t154 / 0.2D1 - t158 / 0.2D1) + (-0.1
     #80D3 * t62 + (t44 + 0.90D2) * t35 * t38) * wd * nf * (-t153 + t157
     #)) * t146 / 0.30D2
      t177 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t176)
      rrgg2qqbarhsoftt12s2e1 = t177 * t176

      end function



      doubleprecision function rrgg2qqbarhsoftt12s2e0
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
      t3 = -0.1D1 + x3
      t5 = x4 * pi
      t6 = Sin(t5)
      t7 = t6 ** 2
      t8 = sqrt(x2)
      t9 = -0.1D1 + t8
      t11 = t8 + 0.1D1
      t15 = log(0.4D1 * x2 * x3 * t3 * t7 * t9 * t11)
      t18 = cos(t5)
      t22 = Sqrt(x3 * t9 * t11 * t3)
      t26 = (0.2D1 * t8 * x3 + 0.2D1 * t18 * t22 - t8) ** 2
      t30 = x3 * t3
      t33 = log(-0.4D1 * x2 * t7 * t30)
      t34 = t18 ** 2
      t36 = Sqrt(-t30)
      t37 = t36 ** 2
      t43 = 0.180D3 * lh
      t47 = t34 * t37
      t57 = x1 ** 2
      t58 = t57 * t7
      t60 = (-0.1D1 + x1) ** 2
      t64 = log(-0.4D1 * t58 * t30 * t60)
      t67 = log(-0.4D1 * t58 * t30)
      t78 = log(-0.4D1 * t7 * x3 * t3)
      t79 = 0.90D2 * t78
      t87 = t78 ** 2
      t89 = lh ** 2
      t91 = pi ** 2
      t100 = -(-0.90D2 * wd * nf * (-0.2D1 * t15 * t26 + 0.8D1 * t33 * t
     #34 * t37) + (-0.90D2 + t43) * wd * nf * (0.2D1 * t26 - 0.8D1 * t47
     #)) / x2 / 0.480D3 + 0.3D1 * t47 * wd * nf * (-t64 + t67) / x1 + (-
     #0.270D3 * t47 + 0.2D1 * (t43 + 0.90D2 + t79) * t34 * t37 + (0.180D
     #3 * (-0.1D1 - t78) * lh - t79 - 0.45D2 * t87 - 0.180D3 * t89 + 0.3
     #0D2 * t91) * t34 * t37) * wd * nf / 0.60D2
      t101 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t100)
      rrgg2qqbarhsoftt12s2e0 = t101 * t100

      end function



      doubleprecision function rrgg2qqbarhsoftt12s2em1
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
      t1 = x4 * pi
      t2 = cos(t1)
      t3 = t2 ** 2
      t4 = -0.1D1 + x3
      t6 = Sqrt(-x3 * t4)
      t7 = t6 ** 2
      t8 = t3 * t7
      t11 = Sin(t1)
      t12 = t11 ** 2
      t16 = log(-0.4D1 * t12 * x3 * t4)
      t26 = sqrt(x2)
      t34 = Sqrt(x3 * (-0.1D1 + t26) * (t26 + 0.1D1) * t4)
      t38 = (0.2D1 * t26 * x3 + 0.2D1 * t2 * t34 - t26) ** 2
      t46 = (-0.180D3 * t8 + (0.180D3 * lh + 0.90D2 + 0.90D2 * t16) * t3
     # * t7) * wd * nf / 0.60D2 + 0.3D1 / 0.16D2 * wd * nf * (0.2D1 * t3
     #8 - 0.8D1 * t8) / x2
      t47 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t46)
      rrgg2qqbarhsoftt12s2em1 = t47 * t46

      end function



      doubleprecision function rrgg2qqbarhsoftt12s2em2
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
      t2 = cos(x4 * pi)
      t3 = t2 ** 2
      t6 = Sqrt(-x3 * (-0.1D1 + x3))
      t7 = t6 ** 2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -0.3D1 
     #/ 0.2D1 * t3 * t7 * wd * nf)
      rrgg2qqbarhsoftt12s2em2 = -0.3D1 / 0.2D1 * t12 * t3 * t7 * wd * nf

      end function



      doubleprecision function rrgg2qqbarhsoftt12s2em3
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
      rrgg2qqbarhsoftt12s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt12s2em4
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
      rrgg2qqbarhsoftt12s2em4 = 0.0D0

      end function
