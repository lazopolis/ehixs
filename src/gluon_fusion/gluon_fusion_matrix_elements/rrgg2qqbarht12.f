  
      subroutine rrgg2qqbarht12
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarht12s1e1  
      doubleprecision rrgg2qqbarht12s1e0  
      doubleprecision rrgg2qqbarht12s1em1  
      doubleprecision rrgg2qqbarht12s1em2  
      doubleprecision rrgg2qqbarht12s1em3  
      doubleprecision rrgg2qqbarht12s1em4  
      doubleprecision rrgg2qqbarht12s2e1  
      doubleprecision rrgg2qqbarht12s2e0  
      doubleprecision rrgg2qqbarht12s2em1  
      doubleprecision rrgg2qqbarht12s2em2  
      doubleprecision rrgg2qqbarht12s2em3  
      doubleprecision rrgg2qqbarht12s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht12s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht12s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht12s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht12s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht12s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht12s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht12s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht12s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht12s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht12s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht12s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht12s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht12s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t7 = z * wd * nf
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t15 = t13 * x3 * t4
      t18 = log(-0.4D1 * x2 * t10 * t15)
      t19 = t18 ** 2
      t20 = cos(t8)
      t21 = t20 ** 2
      t23 = x3 * t4
      t24 = Sqrt(-t23)
      t25 = t24 ** 2
      t30 = 0.180D3 * z * lh
      t31 = 0.90D2 * z
      t33 = (-t30 + t31) * wd
      t34 = t33 * nf
      t39 = lh ** 2
      t41 = 0.3141592653589793D1 ** 2
      t43 = 0.180D3 * t39 - 0.30D2 * t41
      t46 = (-t30 + t31 + z * t43) * wd
      t48 = nf * t21 * t25
      t52 = 0.1D1 / x2
      t55 = t21 * t25
      t56 = t55 * z
      t58 = t10 * t13
      t59 = t58 * t23
      t61 = log(-0.4D1 * t59)
      t64 = t25 * z
      t65 = (-0.1D1 - t61) * t21 * t64
      t67 = t61 ** 2
      t68 = t67 / 0.2D1
      t71 = (t61 + t68) * t21 * t64
      t98 = x1 ** 2
      t99 = x2 * t98
      t103 = log(-0.4D1 * t99 * t10 * t15)
      t112 = 0.1D1 / x1
      t115 = t21 * z
      t116 = t115 * wd
      t117 = t98 * t10
      t120 = log(-0.4D1 * t117 * t15)
      t121 = t120 ** 2
      t127 = 0.180D3 * t115 * lh
      t128 = 0.90D2 * t115
      t130 = (-t127 + t128) * wd
      t136 = (-t127 + t128 + t115 * t43) * wd
      t142 = (-0.720D3 * t7 * t19 * t21 * t25 + 0.16D2 * t34 * t18 * t21
     # * t25 - 0.16D2 * t46 * t48) * t52 / 0.960D3 - (-0.180D3 * (0.3D1 
     #* t56 + 0.2D1 * t65 + t71) * lh + t55 * z * (0.60D2 * lh * t41 - 0
     #.2884936567583026D3 - 0.120D3 * t39 * lh) + 0.360D3 * t56 + 0.270D
     #3 * t65 + 0.180D3 * t71 + 0.90D2 * (-t68 - t67 * t61 / 0.6D1) * t2
     #1 * t64 + (0.2D1 * t56 + t65) * t43) * wd * nf / 0.60D2 + (0.1440D
     #4 * t7 * t103 * t21 * t25 - 0.16D2 * t33 * t48) * t52 * t112 / 0.4
     #80D3 - (0.45D2 * t116 * nf * t121 * t25 - t130 * nf * t120 * t25 +
     # t136 * nf * t25) * t112 / 0.30D2
      t143 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #142)
      t145 = -0.1D1 + x1
      t146 = x3 * x1
      t147 = t146 * z
      t148 = x2 * x3
      t149 = 0.2D1 * t148
      t150 = t148 * x1
      t151 = x1 * z
      t152 = t148 * t151
      t153 = sqrt(x2)
      t154 = t20 * t153
      t155 = -0.1D1 + t153
      t156 = x3 * t155
      t157 = t153 + 0.1D1
      t158 = 0.1D1 - x1 + t151
      t162 = Sqrt(t156 * t157 * t158 * t4)
      t164 = 0.2D1 * t154 * t162
      t167 = 0.1D1 / t158
      t170 = t2 * t146
      t171 = x2 * x1
      t172 = t171 * z
      t173 = 0.1D1 - x1 + t151 - x2 + t171 - t172 - x3 + t146 - t147 + t
     #149 - t150 + t152 + t164
      t177 = t4 * s
      t179 = t177 * t1 * x1
      t180 = t1 ** 2
      t186 = t145 ** 2
      t194 = log(0.4D1 * t99 * t186 * t155 * t157 * t23 * t58 * t167)
      t196 = x2 * z
      t198 = (-0.1D1 + x1 - t196 + t172 - t151 + x2 - t171) ** 2
      t199 = 0.1D1 / t198
      t200 = t20 * x2
      t201 = t162 * z
      t205 = t153 * x2
      t206 = t205 * x3
      t208 = 0.4D1 * t206 * z
      t213 = t98 * t205
      t216 = x1 * t205
      t219 = t153 * x1
      t222 = x3 * t153
      t224 = 0.2D1 * t222 * z
      t227 = x3 * t98
      t231 = t153 * t98
      t238 = t205 * z
      t241 = t20 * t162
      t250 = 0.4D1 * t200 * t201 * x1 - t208 + 0.2D1 * t206 * t98 - 0.6D
     #1 * t206 * x1 + 0.2D1 * t213 * z + t216 * t12 - t213 * t12 + 0.6D1
     # * t219 * z + t224 + 0.10D2 * t222 * x1 - 0.4D1 * t227 * t153 - t2
     #19 * t12 - 0.4D1 * t231 * z + 0.2D1 * t231 * t12 + 0.4D1 * t200 * 
     #t162 - 0.4D1 * t238 * x1 + 0.4D1 * t241 * x1 - 0.2D1 * t206 * t12 
     #* x1 - 0.4D1 * t206 * t98 * z
      t259 = t153 * z
      t274 = 0.2D1 * t205
      t275 = 0.4D1 * t206
      t276 = 0.2D1 * t238
      t277 = 0.6D1 * t222
      t282 = 0.3D1 * t153
      t283 = 0.2D1 * t206 * t12 * t98 - 0.12D2 * t222 * t151 + 0.2D1 * x
     #3 * t12 * t219 + 0.8D1 * t227 * t259 - 0.4D1 * t227 * t12 * t153 -
     # 0.4D1 * t200 * t162 * x1 - 0.4D1 * t241 * t151 + 0.8D1 * t206 * t
     #151 - 0.4D1 * t200 * t201 - t274 + t275 + t276 - t277 - 0.4D1 * t2
     #41 + 0.2D1 * t231 + 0.3D1 * t216 - t213 - t259 - 0.5D1 * t219 + t2
     #82
      t285 = (t250 + t283) ** 2
      t293 = 0.90D2 * t7 * t194 * t167 * t199 * t285 - t34 * t167 * t199
     # * t285
      t297 = FJET(XB1, XB2, s, t2 * t145 * (-x3 + t146 - t147 + t149 - t
     #150 + t152 - x2 + t164) * t167, t170, -t2 * t145 * t173 * t167, -t
     #179, -s * t180 * x2 * x1 * t145 * t167, t293 * t52 * t112 / 0.480D
     #3)
      t304 = Sqrt(t156 * t157 * t4)
      t306 = 0.2D1 * t154 * t304
      t315 = log(0.4D1 * x2 * t155 * t157 * t59)
      t316 = t315 ** 2
      t324 = -t224 + 0.4D1 * t200 * t304 * z + t274 - 0.4D1 * t200 * t30
     #4 + t208 - t275 - t276 + 0.4D1 * t20 * t304 + t277 + t259 - t282
      t325 = t324 ** 2
      t328 = (0.1D1 + t196 - x2) ** 2
      t329 = 0.1D1 / t328
      t337 = nf * t325 * t329
      t346 = log(0.4D1 * t99 * t155 * t157 * t59)
      t356 = (0.45D2 * t7 * t316 * t325 * t329 - t34 * t315 * t325 * t32
     #9 + t46 * t337) * t52 / 0.960D3 + (-0.90D2 * t7 * t346 * t325 * t3
     #29 + t33 * t337) * t52 * t112 / 0.480D3
      t357 = FJET(XB1, XB2, s, -t2 * (-x3 + t149 - x2 + t306), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t149 + t306), 0.0D0, 0.0D0, t356)
      t369 = log(-0.4D1 * t148 * t4 * t167 * t58 * t186 * t98)
      t373 = Sqrt(-x3 * t158 * t4)
      t374 = t373 ** 2
      t375 = t374 * t167
      t392 = log(-0.4D1 * t117 * t13 * t23 * t167 * t186)
      t393 = t392 ** 2
      t408 = (-0.1440D4 * t7 * t369 * t21 * t375 + 0.16D2 * t34 * t21 * 
     #t374 * t167) * t52 * t112 / 0.480D3 - (-0.45D2 * t116 * nf * t393 
     #* t375 + t130 * nf * t392 * t374 * t167 - t136 * nf * t374 * t167)
     # * t112 / 0.30D2
      t409 = FJET(XB1, XB2, s, -t2 * t145 * x3, t170, t177 * t1 * t145, 
     #-t179, 0.0D0, t408)
      rrgg2qqbarht12s1e1 = t143 * t142 + t297 * t293 * t52 * t112 / 0.48
     #0D3 + t357 * t356 + t409 * t408

      end function



      doubleprecision function rrgg2qqbarht12s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = z * wd
      t7 = t6 * nf
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t15 = t13 * x3 * t4
      t18 = log(-0.4D1 * x2 * t10 * t15)
      t19 = cos(t8)
      t20 = t19 ** 2
      t22 = x3 * t4
      t23 = Sqrt(-t22)
      t24 = t23 ** 2
      t32 = (-0.180D3 * z * lh + 0.90D2 * z) * wd
      t33 = nf * t20
      t38 = 0.1D1 / x2
      t41 = t20 * t24
      t42 = 0.1D1 / x1
      t43 = t38 * t42
      t47 = t20 * z
      t48 = t47 * wd
      t49 = x1 ** 2
      t50 = t49 * t10
      t53 = log(-0.4D1 * t50 * t15)
      t62 = (-0.180D3 * t47 * lh + 0.90D2 * t47) * wd
      t68 = t41 * z
      t71 = t10 * t13 * t22
      t73 = log(-0.4D1 * t71)
      t76 = t24 * z
      t77 = (-0.1D1 - t73) * t20 * t76
      t83 = t73 ** 2
      t89 = lh ** 2
      t91 = 0.3141592653589793D1 ** 2
      t100 = (0.1440D4 * t7 * t18 * t20 * t24 - 0.16D2 * t32 * t33 * t24
     #) * t38 / 0.960D3 - 0.3D1 * t7 * t41 * t43 - (-0.90D2 * t48 * nf *
     # t53 * t24 + t62 * nf * t24) * t42 / 0.30D2 - (-0.180D3 * (0.2D1 *
     # t68 + t77) * lh + 0.270D3 * t68 + 0.180D3 * t77 + 0.90D2 * (t73 +
     # t83 / 0.2D1) * t20 * t76 + t41 * z * (0.180D3 * t89 - 0.30D2 * t9
     #1)) * wd * nf / 0.60D2
      t101 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #100)
      t103 = -0.1D1 + x1
      t104 = x3 * x1
      t105 = t104 * z
      t106 = x2 * x3
      t107 = 0.2D1 * t106
      t108 = t106 * x1
      t109 = x1 * z
      t110 = t106 * t109
      t111 = sqrt(x2)
      t112 = t19 * t111
      t113 = -0.1D1 + t111
      t114 = x3 * t113
      t115 = t111 + 0.1D1
      t116 = 0.1D1 - x1 + t109
      t120 = Sqrt(t114 * t115 * t116 * t4)
      t122 = 0.2D1 * t112 * t120
      t125 = 0.1D1 / t116
      t128 = t2 * t104
      t129 = x2 * x1
      t130 = t129 * z
      t131 = 0.1D1 - x1 + t109 - x2 + t129 - t130 - x3 + t104 - t105 + t
     #107 - t108 + t110 + t122
      t135 = t4 * s
      t137 = t135 * t1 * x1
      t138 = t1 ** 2
      t146 = x2 * z
      t148 = (-0.1D1 + x1 - t146 + t130 - t109 + x2 - t129) ** 2
      t149 = 0.1D1 / t148
      t150 = 0.3D1 * t111
      t151 = t19 * x2
      t155 = t19 * t120
      t158 = t111 * x2
      t159 = t158 * x3
      t162 = t120 * z
      t174 = x3 * t111
      t178 = t111 * x1
      t181 = x3 * t49
      t182 = t111 * z
      t192 = t49 * t158
      t195 = x1 * t158
      t201 = 0.2D1 * t174 * z
      t202 = t150 - 0.4D1 * t151 * t120 * x1 - 0.4D1 * t155 * t109 + 0.8
     #D1 * t159 * t109 - 0.4D1 * t151 * t162 - 0.2D1 * t159 * t12 * x1 -
     # 0.4D1 * t159 * t49 * z + 0.2D1 * t159 * t12 * t49 - 0.12D2 * t174
     # * t109 + 0.2D1 * x3 * t12 * t178 + 0.8D1 * t181 * t182 - 0.4D1 * 
     #t181 * t12 * t111 + 0.2D1 * t159 * t49 - 0.6D1 * t159 * x1 + 0.2D1
     # * t192 * z + t195 * t12 - t192 * t12 + 0.6D1 * t178 * z + t201
      t208 = t111 * t49
      t215 = t158 * z
      t221 = 0.4D1 * t159 * z
      t222 = 0.4D1 * t159
      t223 = 0.2D1 * t215
      t224 = 0.6D1 * t174
      t232 = 0.2D1 * t158
      t233 = 0.10D2 * t174 * x1 - 0.4D1 * t181 * t111 - t178 * t12 - 0.4
     #D1 * t208 * z + 0.2D1 * t208 * t12 + 0.4D1 * t151 * t120 - 0.4D1 *
     # t215 * x1 + 0.4D1 * t155 * x1 - t221 + t222 + t223 - t224 - 0.4D1
     # * t155 + 0.2D1 * t208 + 0.3D1 * t195 - t192 - t182 - 0.5D1 * t178
     # + 0.4D1 * t151 * t162 * x1 - t232
      t235 = (t202 + t233) ** 2
      t240 = FJET(XB1, XB2, s, t2 * t103 * (-x3 + t104 - t105 + t107 - t
     #108 + t110 - x2 + t122) * t125, t128, -t2 * t103 * t131 * t125, -t
     #137, -s * t138 * x2 * x1 * t103 * t125, -0.3D1 / 0.16D2 * t6 * nf 
     #* t125 * t149 * t235 * t43)
      t252 = Sqrt(t114 * t115 * t4)
      t254 = 0.2D1 * t112 * t252
      t263 = log(0.4D1 * x2 * t113 * t115 * t71)
      t271 = -t201 + 0.4D1 * t151 * t252 * z + t232 - 0.4D1 * t151 * t25
     #2 + t221 - t222 - t223 + 0.4D1 * t19 * t252 + t224 + t182 - t150
      t272 = t271 ** 2
      t275 = (0.1D1 + t146 - x2) ** 2
      t276 = 0.1D1 / t275
      t290 = (-0.90D2 * t7 * t263 * t272 * t276 + t32 * nf * t272 * t276
     #) * t38 / 0.960D3 + 0.3D1 / 0.16D2 * t7 * t272 * t276 * t43
      t291 = FJET(XB1, XB2, s, -t2 * (-x3 + t107 - x2 + t254), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t107 + t254), 0.0D0, 0.0D0, t290)
      t300 = Sqrt(-x3 * t116 * t4)
      t301 = t300 ** 2
      t302 = t301 * t125
      t307 = t103 ** 2
      t312 = log(-0.4D1 * t50 * t13 * t22 * t125 * t307)
      t323 = 0.3D1 * t6 * t33 * t302 * t43 - (0.90D2 * t48 * nf * t312 *
     # t302 - t62 * nf * t301 * t125) * t42 / 0.30D2
      t324 = FJET(XB1, XB2, s, -t2 * t103 * x3, t128, t135 * t1 * t103, 
     #-t137, 0.0D0, t323)
      rrgg2qqbarht12s1e0 = t101 * t100 - 0.3D1 / 0.16D2 * t240 * z * wd 
     #* nf * t125 * t149 * t235 * t38 * t42 + t291 * t290 + t324 * t323

      end function



      doubleprecision function rrgg2qqbarht12s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t7 = z * wd * nf
      t8 = x4 * 0.3141592653589793D1
      t9 = cos(t8)
      t10 = t9 ** 2
      t11 = x3 * t4
      t12 = Sqrt(-t11)
      t13 = t12 ** 2
      t14 = t10 * t13
      t15 = 0.1D1 / x1
      t24 = Sin(t8)
      t25 = t24 ** 2
      t26 = z ** 2
      t31 = log(-0.4D1 * t25 / t26 * t11)
      t41 = 0.1D1 / x2
      t45 = -0.3D1 * t7 * t14 * t15 - (-0.180D3 * t14 * z * lh + 0.180D3
     # * t14 * z + 0.90D2 * (-0.1D1 - t31) * t10 * t13 * z) * wd * nf / 
     #0.60D2 - 0.3D1 / 0.2D1 * t7 * t14 * t41
      t46 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t4
     #5)
      t48 = -0.1D1 + x1
      t53 = t4 * s
      t59 = 0.1D1 - x1 + x1 * z
      t62 = Sqrt(-x3 * t59 * t4)
      t63 = t62 ** 2
      t67 = t10 * t63 / t59 * t15
      t70 = FJET(XB1, XB2, s, -t2 * t48 * x3, t2 * x1 * x3, t53 * t1 * t
     #48, -t53 * t1 * x1, 0.0D0, 0.3D1 * t7 * t67)
      t77 = 0.2D1 * x2 * x3
      t78 = sqrt(x2)
      t85 = Sqrt(x3 * (-0.1D1 + t78) * (t78 + 0.1D1) * t4)
      t87 = 0.2D1 * t9 * t78 * t85
      t92 = x3 * t78
      t95 = t9 * x2
      t99 = t78 * x2
      t103 = t99 * x3
      t114 = -0.2D1 * t92 * z + 0.4D1 * t95 * t85 * z + 0.2D1 * t99 - 0.
     #4D1 * t95 * t85 + 0.4D1 * t103 * z - 0.4D1 * t103 - 0.2D1 * t99 * 
     #z + 0.4D1 * t9 * t85 + 0.6D1 * t92 + t78 * z - 0.3D1 * t78
      t115 = t114 ** 2
      t118 = (0.1D1 + x2 * z - x2) ** 2
      t119 = 0.1D1 / t118
      t124 = FJET(XB1, XB2, s, -t2 * (-x3 + t77 - x2 + t87), 0.0D0, t2 *
     # (0.1D1 - x2 - x3 + t77 + t87), 0.0D0, 0.0D0, 0.3D1 / 0.32D2 * t7 
     #* t115 * t119 * t41)
      rrgg2qqbarht12s1em1 = t46 * t45 + 0.3D1 * t70 * z * wd * nf * t67 
     #+ 0.3D1 / 0.32D2 * t124 * z * wd * nf * t115 * t119 * t41

      end function



      doubleprecision function rrgg2qqbarht12s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t2 = (-0.1D1 + z) * s
      t4 = -0.1D1 + x3
      t8 = cos(x4 * 0.3141592653589793D1)
      t9 = t8 ** 2
      t12 = Sqrt(-x3 * t4)
      t13 = t12 ** 2
      t14 = nf * t9 * t13
      t17 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, -0
     #.3D1 / 0.2D1 * z * wd * t14)
      rrgg2qqbarht12s1em2 = -0.3D1 / 0.2D1 * t17 * z * wd * t14

      end function



      doubleprecision function rrgg2qqbarht12s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2qqbarht12s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht12s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2qqbarht12s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht12s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = wd * nf
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = t9 * x2
      t11 = sqrt(x2)
      t12 = -0.1D1 + t11
      t14 = t11 + 0.1D1
      t16 = x4 * 0.3141592653589793D1
      t17 = Sin(t16)
      t18 = t17 ** 2
      t19 = t4 * t18
      t20 = t14 * x3 * t19
      t23 = log(0.4D1 * t10 * t12 * t20)
      t24 = t23 ** 2
      t25 = t11 * z
      t26 = x3 * t11
      t28 = 0.2D1 * t26 * z
      t30 = x3 * t12
      t34 = Sqrt(t30 * t14 * z * t4)
      t35 = cos(t16)
      t39 = (-t25 + t28 - t11 + 0.2D1 * t26 + 0.4D1 * t34 * t35) ** 2
      t46 = log(-0.4D1 * t10 * t18 * x3 * t4)
      t47 = t46 ** 2
      t48 = t35 ** 2
      t50 = x3 * z
      t52 = Sqrt(-t50 * t4)
      t53 = t52 ** 2
      t59 = 0.180D3 * lh
      t61 = (-t59 + 0.90D2) * wd
      t69 = lh ** 2
      t70 = 0.180D3 * t69
      t71 = 0.3141592653589793D1 ** 2
      t72 = 0.30D2 * t71
      t78 = nf * (-t39 + 0.16D2 * t48 * t53)
      t81 = 0.1D1 / x2
      t85 = x3 * t9
      t88 = log(-0.4D1 * t85 * t19)
      t90 = (-0.1D1 - t88) * t48
      t92 = t88 ** 2
      t93 = t92 / 0.2D1
      t95 = (t88 + t93) * t48
      t115 = t70 - t72
      t119 = nf * t53
      t122 = x1 ** 2
      t123 = x2 * t122
      t128 = log(-0.4D1 * t123 * t18 * t85 * t4)
      t137 = log(0.4D1 * t122 * t9 * x2 * t12 * t20)
      t145 = 0.1D1 / x1
      t148 = t48 * wd
      t149 = x3 * t122
      t154 = log(-0.4D1 * t149 * t18 * t9 * t4)
      t155 = t154 ** 2
      t161 = 0.180D3 * t48 * lh
      t162 = 0.90D2 * t48
      t164 = (-t161 + t162) * wd
      t171 = (-t161 + t162 + t48 * t115) * wd
      t177 = -(0.90D2 * t6 * (-t24 * t39 / 0.2D1 + 0.8D1 * t47 * t48 * t
     #53) + t61 * nf * (t23 * t39 - 0.16D2 * t46 * t48 * t53) + (-t59 + 
     #0.90D2 + t70 - t72) * wd * t78) * t81 / 0.960D3 - (-0.180D3 * (0.3
     #D1 * t48 + 0.2D1 * t90 + t95) * lh + t48 * (0.60D2 * lh * t71 - 0.
     #2884936567583026D3 - 0.120D3 * t69 * lh) + 0.360D3 * t48 + 0.270D3
     # * t90 + 0.180D3 * t95 + 0.90D2 * (-t93 - t92 * t88 / 0.6D1) * t48
     # + (0.2D1 * t48 + t90) * t115) * wd * t119 / 0.60D2 - (0.90D2 * t6
     # * (-0.16D2 * t128 * t48 * t53 + t137 * t39) + t61 * t78) * t81 * 
     #t145 / 0.480D3 - (0.720D3 * t148 * nf * t155 * t53 - 0.16D2 * t164
     # * nf * t154 * t53 + 0.16D2 * t171 * t119) * t145 / 0.480D3
      t178 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #177)
      t180 = x3 * x1
      t182 = -0.1D1 + x1
      t184 = t2 * t182 * x3
      t185 = t4 * s
      t189 = t185 * t1 * t182
      t190 = 0.1D1 / t7
      t193 = x3 * t4
      t194 = x1 * z
      t195 = -z - x1 + t194
      t196 = 0.1D1 / t195
      t197 = t182 ** 2
      t199 = t193 * t196 * t197
      t202 = log(0.4D1 * t123 * t18 * t190 * t199)
      t206 = Sqrt(x3 * t195 * t4)
      t207 = t206 ** 2
      t209 = t196 * z
      t210 = t207 * t48 * t209
      t213 = t61 * nf
      t224 = log(0.4D1 * t122 * t18 * t190 * t199)
      t225 = t224 ** 2
      t243 = -(-0.1440D4 * t6 * t202 * t210 + 0.16D2 * t213 * t210) * t8
     #1 * t145 / 0.480D3 - (0.720D3 * t148 * nf * t225 * t207 * t209 - 0
     #.16D2 * t164 * nf * t224 * t207 * t209 + 0.16D2 * t171 * nf * t207
     # * t196 * z) * t145 / 0.480D3
      t244 = FJET(XB1, XB2, s, t2 * t180, -t184, -t185 * t1 * x1, t189, 
     #0.0D0, t243)
      t246 = t180 * z
      t247 = x2 * x3
      t248 = t247 * z
      t249 = t247 * x1
      t250 = t247 * t194
      t255 = Sqrt(-t30 * t14 * t195 * t4)
      t257 = 0.2D1 * t35 * t11 * t255
      t263 = x2 * x1
      t264 = t263 * z
      t265 = z + x1 - t194 - x2 * z - t263 + t264 - t50 - t180 + t246 + 
     #t248 + t249 - t250 + t247 + t257
      t269 = t1 ** 2
      t284 = log(-0.4D1 * t12 * t14 * t193 * t190 * t122 * t197 * t18 * 
     #t196 * x2)
      t288 = t11 * x2
      t289 = t288 * x3
      t294 = t122 * t288
      t297 = x1 * t288
      t300 = t35 * t255
      t305 = t11 * x1
      t314 = t11 * t122
      t319 = t35 * x2
      t320 = t255 * x1
      t323 = -0.2D1 * t26 * t7 + 0.2D1 * t289 * t122 + 0.2D1 * t289 * x1
     # + 0.2D1 * t294 * z + t297 * t7 - t294 * t7 - 0.4D1 * t300 * z - 0
     #.4D1 * t300 * x1 + 0.2D1 * t305 * z - 0.2D1 * t26 * x1 - 0.4D1 * t
     #149 * t11 - 0.3D1 * t305 * t7 - 0.4D1 * t314 * z + 0.2D1 * t314 * 
     #t7 + 0.4D1 * t319 * t320
      t342 = t11 * t7
      t349 = 0.4D1 * t300 * t194 - 0.2D1 * t289 * t7 * x1 - 0.4D1 * t289
     # * t122 * z + 0.2D1 * t289 * t7 * t122 - 0.4D1 * t26 * t194 + 0.6D
     #1 * x3 * t7 * t305 + 0.8D1 * t149 * t25 - 0.4D1 * t149 * t342 - 0.
     #4D1 * t319 * t320 * z - t28 + 0.2D1 * t314 - t297 + t342 - t294 + 
     #t305 + t25
      t351 = (t323 + t349) ** 2
      t354 = (z + x1 + t264 - t194 - t263) ** 2
      t357 = t196 * t351 / t354 * z
      t361 = 0.90D2 * t6 * t284 * t357 - t213 * t357
      t365 = FJET(XB1, XB2, s, t2 * x1 * (-t50 - t180 + t246 + t248 + t2
     #49 - t250 - x2 + t247 + t257) * t196, -t184, -t2 * x1 * t265 * t19
     #6, t189, s * t269 * x2 * x1 * t182 * t196, -t361 * t81 * t145 / 0.
     #480D3)
      rrgg2qqbarht12s2e1 = t178 * t177 + t244 * t243 - t365 * t361 * t81
     # * t145 / 0.480D3

      end function



      doubleprecision function rrgg2qqbarht12s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = wd * nf
      t7 = z ** 2
      t9 = 0.1D1 / t7 / z
      t10 = t9 * x2
      t11 = sqrt(x2)
      t12 = -0.1D1 + t11
      t14 = t11 + 0.1D1
      t16 = x4 * 0.3141592653589793D1
      t17 = Sin(t16)
      t18 = t17 ** 2
      t19 = t4 * t18
      t23 = log(0.4D1 * t10 * t12 * t14 * x3 * t19)
      t24 = t11 * z
      t25 = x3 * t11
      t27 = 0.2D1 * t25 * z
      t29 = x3 * t12
      t33 = Sqrt(t29 * t14 * z * t4)
      t34 = cos(t16)
      t38 = (-t24 + t27 - t11 + 0.2D1 * t25 + 0.4D1 * t33 * t34) ** 2
      t44 = log(-0.4D1 * t10 * t18 * x3 * t4)
      t45 = t34 ** 2
      t47 = x3 * z
      t49 = Sqrt(-t47 * t4)
      t50 = t49 ** 2
      t61 = -t38 + 0.16D2 * t45 * t50
      t65 = 0.1D1 / x2
      t69 = 0.1D1 / x1
      t73 = t45 * wd
      t74 = x1 ** 2
      t75 = x3 * t74
      t80 = log(-0.4D1 * t75 * t18 * t9 * t4)
      t89 = (-0.180D3 * t45 * lh + 0.90D2 * t45) * wd
      t90 = nf * t50
      t100 = log(-0.4D1 * x3 * t9 * t19)
      t102 = (-0.1D1 - t100) * t45
      t108 = t100 ** 2
      t113 = lh ** 2
      t115 = 0.3141592653589793D1 ** 2
      t123 = -(0.90D2 * t6 * (t23 * t38 - 0.16D2 * t44 * t45 * t50) + (-
     #0.180D3 * lh + 0.90D2) * wd * nf * t61) * t65 / 0.960D3 - 0.3D1 / 
     #0.16D2 * t6 * t61 * t65 * t69 - (-0.1440D4 * t73 * nf * t80 * t50 
     #+ 0.16D2 * t89 * t90) * t69 / 0.480D3 - (-0.180D3 * (0.2D1 * t45 +
     # t102) * lh + 0.270D3 * t45 + 0.180D3 * t102 + 0.90D2 * (t100 + t1
     #08 / 0.2D1) * t45 + t45 * (0.180D3 * t113 - 0.30D2 * t115)) * wd *
     # t90 / 0.60D2
      t124 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #123)
      t126 = x3 * x1
      t128 = -0.1D1 + x1
      t130 = t2 * t128 * x3
      t131 = t4 * s
      t135 = t131 * t1 * t128
      t136 = x1 * z
      t137 = -z - x1 + t136
      t140 = Sqrt(x3 * t137 * t4)
      t141 = t140 ** 2
      t144 = 0.1D1 / t137
      t145 = t144 * z
      t146 = t65 * t69
      t155 = t128 ** 2
      t160 = log(0.4D1 * t74 * t18 / t7 * x3 * t4 * t144 * t155)
      t173 = -0.3D1 * t6 * t141 * t45 * t145 * t146 - (-0.1440D4 * t73 *
     # nf * t160 * t141 * t145 + 0.16D2 * t89 * nf * t141 * t144 * z) * 
     #t69 / 0.480D3
      t174 = FJET(XB1, XB2, s, t2 * t126, -t130, -t131 * t1 * x1, t135, 
     #0.0D0, t173)
      t176 = t126 * z
      t177 = x2 * x3
      t178 = t177 * z
      t179 = t177 * x1
      t180 = t177 * t136
      t185 = Sqrt(-t29 * t14 * t137 * t4)
      t187 = 0.2D1 * t34 * t11 * t185
      t193 = x2 * x1
      t194 = t193 * z
      t195 = z + x1 - t136 - x2 * z - t193 + t194 - t47 - t126 + t176 + 
     #t178 + t179 - t180 + t177 + t187
      t199 = t1 ** 2
      t205 = t34 * x2
      t206 = t185 * x1
      t209 = t34 * t185
      t212 = t11 * x2
      t213 = t212 * x3
      t226 = t11 * x1
      t231 = t11 * t7
      t234 = t11 * t74
      t236 = t212 * x1
      t237 = t74 * t212
      t238 = t24 + 0.4D1 * t205 * t206 + 0.4D1 * t209 * t136 - 0.2D1 * t
     #213 * t7 * x1 - 0.4D1 * t213 * t74 * z + 0.2D1 * t213 * t7 * t74 -
     # 0.4D1 * t25 * t136 + 0.6D1 * x3 * t7 * t226 + 0.8D1 * t75 * t24 -
     # 0.4D1 * t75 * t231 + 0.2D1 * t234 - t236 + t231 - t237 + t226
      t268 = -0.4D1 * t205 * t206 * z - t27 - 0.2D1 * t25 * t7 + 0.2D1 *
     # t213 * t74 + 0.2D1 * t213 * x1 + 0.2D1 * t237 * z + t236 * t7 - t
     #237 * t7 - 0.4D1 * t209 * z - 0.4D1 * t209 * x1 + 0.2D1 * t226 * z
     # - 0.2D1 * t25 * x1 - 0.4D1 * t75 * t11 - 0.3D1 * t226 * t7 - 0.4D
     #1 * t234 * z + 0.2D1 * t234 * t7
      t270 = (t238 + t268) ** 2
      t274 = (z + x1 + t194 - t136 - t193) ** 2
      t275 = 0.1D1 / t274
      t280 = FJET(XB1, XB2, s, t2 * x1 * (-t47 - t126 + t176 + t178 + t1
     #79 - t180 - x2 + t177 + t187) * t144, -t130, -t2 * x1 * t195 * t14
     #4, t135, s * t199 * x2 * x1 * t128 * t144, 0.3D1 / 0.16D2 * t6 * t
     #144 * t270 * t275 * z * t146)
      rrgg2qqbarht12s2e0 = t124 * t123 + t174 * t173 + 0.3D1 / 0.16D2 * 
     #t280 * wd * nf * t144 * t270 * t275 * z * t65 * t69

      end function



      doubleprecision function rrgg2qqbarht12s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = x4 * 0.3141592653589793D1
      t7 = cos(t6)
      t8 = t7 ** 2
      t9 = t8 * wd
      t12 = Sqrt(-x3 * z * t4)
      t13 = t12 ** 2
      t14 = nf * t13
      t15 = 0.1D1 / x1
      t22 = z ** 2
      t26 = Sin(t6)
      t27 = t26 ** 2
      t31 = log(-0.4D1 * x3 / t22 / z * t27 * t4)
      t39 = wd * nf
      t40 = sqrt(x2)
      t42 = x3 * t40
      t52 = Sqrt(x3 * (-0.1D1 + t40) * (t40 + 0.1D1) * z * t4)
      t56 = (-t40 * z + 0.2D1 * t42 * z - t40 + 0.2D1 * t42 + 0.4D1 * t5
     #2 * t7) ** 2
      t64 = -0.3D1 * t9 * t14 * t15 - (-0.180D3 * t8 * lh + 0.180D3 * t8
     # + 0.90D2 * (-0.1D1 - t31) * t8) * wd * t14 / 0.60D2 - 0.3D1 / 0.3
     #2D2 * t39 * (-t56 + 0.16D2 * t8 * t13) / x2
      t65 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t6
     #4)
      t69 = -0.1D1 + x1
      t72 = t4 * s
      t79 = -z - x1 + x1 * z
      t82 = Sqrt(x3 * t79 * t4)
      t83 = t82 ** 2
      t87 = t83 / t79 * z * t15
      t90 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t69 * x3, -t72 * t1 * 
     #x1, t72 * t1 * t69, 0.0D0, -0.3D1 * t9 * nf * t87)
      rrgg2qqbarht12s2em1 = t65 * t64 - 0.3D1 * t90 * t8 * t39 * t87

      end function



      doubleprecision function rrgg2qqbarht12s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t2 = (-0.1D1 + z) * s
      t4 = -0.1D1 + x3
      t7 = cos(x4 * 0.3141592653589793D1)
      t8 = t7 ** 2
      t12 = Sqrt(-x3 * z * t4)
      t13 = t12 ** 2
      t17 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, -0
     #.3D1 / 0.2D1 * t8 * wd * nf * t13)
      rrgg2qqbarht12s2em2 = -0.3D1 / 0.2D1 * t17 * t8 * wd * nf * t13

      end function



      doubleprecision function rrgg2qqbarht12s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2qqbarht12s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht12s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2qqbarht12s2em4 = 0.0D0

      end function
