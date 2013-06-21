  
      subroutine rrgg2qqbarht4
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarh41J1  
      doubleprecision rrgg2qqbarh41J2  
      doubleprecision rrgg2qqbarh41J3  
      doubleprecision rrgg2qqbarh41J4  
      doubleprecision rrgg2qqbarh41J5  
      doubleprecision rrgg2qqbarh41J6  
      doubleprecision rrgg2qqbarht4s1e1  
      doubleprecision rrgg2qqbarht4s1e0  
      doubleprecision rrgg2qqbarht4s1em1  
      doubleprecision rrgg2qqbarht4s1em2  
      doubleprecision rrgg2qqbarht4s1em3  
      doubleprecision rrgg2qqbarht4s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht4s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht4s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht4s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht4s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht4s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht4s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht4s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh41J1
      doubleprecision rrgg2qqbarh41J2
      doubleprecision rrgg2qqbarh41J3
      doubleprecision rrgg2qqbarh41J4
      doubleprecision rrgg2qqbarh41J5
      doubleprecision rrgg2qqbarh41J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = x1 ** 2
      t16 = t4 ** 2
      t17 = t15 * t16
      t18 = t17 * x4
      t21 = log(0.4D1 * t14 * t18)
      t22 = rrgg2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.0D0)
      t24 = t21 ** 2
      t25 = rrgg2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.0D0)
      t28 = rrgg2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.0D0)
      t33 = 0.3141592653589793D1 * lh
      t34 = t6 * t8
      t40 = lh ** 2
      t42 = 0.3141592653589793D1 ** 2
      t44 = 0.180D3 * t40 - 0.30D2 * t42
      t45 = 0.3141592653589793D1 * t44
      t46 = t34 * t25
      t47 = t45 * t46
      t49 = 0.1D1 / x4
      t54 = log(0.4D1 * t14 * t17)
      t55 = t54 * 0.3141592653589793D1
      t58 = t54 ** 2
      t59 = t58 * 0.3141592653589793D1
      t66 = rrgg2qqbarh41J4(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.0D0)
      t94 = x3 * t11
      t95 = t94 * t13
      t98 = log(0.4D1 * t95 * t18)
      t107 = 0.1D1 / x3
      t115 = log(0.4D1 * t94 * t13 * t15 * t16)
      t117 = t115 ** 2
      t132 = (0.90D2 * t7 * t8 * (-t21 * t22 + t24 * t25 / 0.2D1 + t28) 
     #- 0.180D3 * t33 * t34 * (t22 - t21 * t25) + t47) * t49 / 0.720D3 +
     # (t45 + 0.180D3 * t55 * lh + 0.45D2 * t59) * t6 * t8 * t22 / 0.720
     #D3 + t7 * t8 * t66 / 0.8D1 + (0.3141592653589793D1 * (0.60D2 * lh 
     #* t42 - 0.2884936567583026D3 - 0.120D3 * t40 * lh) - t55 * t44 - 0
     #.90D2 * t59 * lh - 0.15D2 * t58 * t54 * 0.3141592653589793D1) * t6
     # * t8 * t25 / 0.720D3 + (-0.180D3 * t33 - 0.90D2 * t55) * t6 * t8 
     #* t28 / 0.720D3 - (0.90D2 * t7 * t8 * (-t22 + t98 * t25) + 0.180D3
     # * t33 * t46) * t107 * t49 / 0.720D3 + (0.90D2 * t7 * t8 * (-t115 
     #* t22 + t117 * t25 / 0.2D1 + t28) - 0.180D3 * t33 * t34 * (t22 - t
     #115 * t25) + t47) * t107 / 0.720D3
      t133 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t132)
      t135 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t136 = s * t135
      t137 = t1 * x1
      t138 = t136 * t137
      t139 = t1 * t4
      t140 = t139 * x4
      t141 = t136 * t140
      t142 = -0.1D1 + x4
      t143 = t139 * t142
      t144 = t136 * t143
      t145 = t135 ** 2
      t148 = x1 * t4
      t150 = s * t145 * t6 * t148 * x4
      t153 = x4 * t142
      t154 = t145 ** 2
      t156 = t153 * t154 * t16
      t159 = log(-0.4D1 * t15 * t11 * t13 * t156)
      t160 = t159 * t145
      t162 = 0.1D1 / (-0.2D1 + t135)
      t163 = rrgg2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t166 = t159 ** 2
      t168 = rrgg2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t169 = t162 * t168
      t172 = t145 * t162
      t173 = rrgg2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t179 = t172 * t163
      t185 = t45 * t6
      t187 = t8 * t145 * t169
      t192 = x3 * t15 * t14
      t195 = log(-0.4D1 * t192 * t156)
      t202 = t33 * t6
      t209 = (0.90D2 * t7 * t8 * (-t160 * t162 * t163 + t166 * t145 * t1
     #69 / 0.2D1 + t172 * t173) - 0.180D3 * t33 * t34 * (t179 - t160 * t
     #169) + t185 * t187) * t49 / 0.720D3 - (0.90D2 * t7 * t8 * (-t179 +
     # t195 * t145 * t169) + 0.180D3 * t202 * t187) * t107 * t49 / 0.720
     #D3
      t210 = FJET(XB1, XB2, s, 0.0D0, t138, -t141, t144, -t150, t209)
      t212 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t132)
      t214 = FJET(XB1, XB2, s, t138, 0.0D0, t144, -t141, -t150, t209)
      t216 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t217 = s * t216
      t218 = t137 * x3
      t219 = t217 * t218
      t220 = -0.1D1 + x3
      t221 = t137 * t220
      t222 = t217 * t221
      t223 = t217 * t139
      t224 = t216 ** 2
      t228 = s * t224 * t6 * t148 * x3
      t230 = 0.1D1 / (-0.2D1 + t216)
      t231 = t224 * t230
      t232 = rrgg2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0
     #D0)
      t233 = t231 * t232
      t234 = t16 * t220
      t235 = t224 ** 2
      t240 = log(-0.4D1 * t192 * t234 * x4 * t235)
      t242 = rrgg2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0
     #D0)
      t243 = t230 * t242
      t250 = t8 * t224 * t243
      t260 = log(-0.4D1 * t95 * t17 * t220 * t235)
      t261 = t260 * t224
      t264 = t260 ** 2
      t268 = rrgg2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0
     #D0)
      t283 = -(0.90D2 * t7 * t8 * (-t233 + t240 * t224 * t243) + 0.180D3
     # * t202 * t250) * t107 * t49 / 0.720D3 + (0.90D2 * t7 * t8 * (-t26
     #1 * t230 * t232 + t264 * t224 * t243 / 0.2D1 + t231 * t268) - 0.18
     #0D3 * t33 * t34 * (t233 - t261 * t243) + t185 * t250) * t107 / 0.7
     #20D3
      t284 = FJET(XB1, XB2, s, t219, -t222, 0.0D0, -t223, -t228, t283)
      t286 = KAPPA2(x1, x2, x3, x4, z)
      t287 = s * t286
      t288 = t287 * t218
      t289 = t287 * t221
      t290 = t287 * t140
      t291 = t287 * t143
      t292 = t286 ** 2
      t297 = cos(t9)
      t300 = Sqrt(x3 * t220 * t153)
      t305 = s * t292 * t6 * t148 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t297 * t300)
      t307 = 0.1D1 / (-0.2D1 + t286)
      t309 = rrgg2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t311 = t292 ** 2
      t316 = log(0.4D1 * t192 * t153 * t234 * t311)
      t318 = rrgg2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t319 = t307 * t318
      t329 = 0.90D2 * t7 * t8 * (t292 * t307 * t309 - t316 * t292 * t319
     #) - 0.180D3 * t202 * t8 * t292 * t319
      t332 = t329 * t107 * t49 / 0.720D3
      t333 = FJET(XB1, XB2, s, t288, -t289, -t290, t291, t305, -t332)
      t335 = t107 * t49
      t338 = FJET(XB1, XB2, s, -t222, t219, -t223, 0.0D0, -t228, t283)
      t340 = FJET(XB1, XB2, s, -t289, t288, t291, -t290, t305, -t332)
      rrgg2qqbarht4s1e1 = t133 * t132 + t210 * t209 + t212 * t132 + t214
     # * t209 + t284 * t283 - t333 * t329 * t335 / 0.720D3 + t338 * t283
     # - t340 * t329 * t335 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarht4s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh41J1
      doubleprecision rrgg2qqbarh41J2
      doubleprecision rrgg2qqbarh41J3
      doubleprecision rrgg2qqbarh41J4
      doubleprecision rrgg2qqbarh41J5
      doubleprecision rrgg2qqbarh41J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.
     #0D0)
      t10 = x2 * 0.3141592653589793D1
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t15 = t12 * t14
      t16 = x1 ** 2
      t17 = t4 ** 2
      t18 = t16 * t17
      t22 = log(0.4D1 * t15 * t18 * x4)
      t23 = rrgg2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.0D0)
      t29 = 0.3141592653589793D1 * lh
      t30 = t6 * t8
      t33 = 0.180D3 * t29 * t30 * t23
      t35 = 0.1D1 / x4
      t39 = 0.1D1 / x3
      t44 = x3 * t12
      t49 = log(0.4D1 * t44 * t14 * t16 * t17)
      t61 = log(0.4D1 * t15 * t18)
      t62 = t61 * 0.3141592653589793D1
      t69 = lh ** 2
      t71 = 0.3141592653589793D1 ** 2
      t77 = t61 ** 2
      t85 = rrgg2qqbarh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.0D0)
      t89 = (0.90D2 * t7 * t8 * (t9 - t22 * t23) - t33) * t35 / 0.720D3 
     #+ t7 * t8 * t23 * t39 * t35 / 0.8D1 + (0.90D2 * t7 * t8 * (t9 - t4
     #9 * t23) - t33) * t39 / 0.720D3 + (-0.180D3 * t29 - 0.90D2 * t62) 
     #* t6 * t8 * t9 / 0.720D3 + (0.3141592653589793D1 * (0.180D3 * t69 
     #- 0.30D2 * t71) + 0.180D3 * t62 * lh + 0.45D2 * t77 * 0.3141592653
     #589793D1) * t6 * t8 * t23 / 0.720D3 + t7 * t8 * t85 / 0.8D1
      t90 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t89)
      t92 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t93 = s * t92
      t94 = t1 * x1
      t95 = t93 * t94
      t96 = t1 * t4
      t97 = t96 * x4
      t98 = t93 * t97
      t99 = -0.1D1 + x4
      t100 = t96 * t99
      t101 = t93 * t100
      t102 = t92 ** 2
      t105 = x1 * t4
      t107 = s * t102 * t6 * t105 * x4
      t109 = 0.1D1 / (-0.2D1 + t92)
      t111 = rrgg2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t115 = x4 * t99
      t116 = t102 ** 2
      t121 = log(-0.4D1 * t16 * t12 * t14 * t115 * t116 * t17)
      t123 = rrgg2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t124 = t109 * t123
      t130 = t29 * t6
      t131 = t8 * t102
      t139 = t39 * t35
      t143 = (0.90D2 * t7 * t8 * (t102 * t109 * t111 - t121 * t102 * t12
     #4) - 0.180D3 * t130 * t131 * t124) * t35 / 0.720D3 + t7 * t131 * t
     #124 * t139 / 0.8D1
      t144 = FJET(XB1, XB2, s, 0.0D0, t95, -t98, t101, -t107, t143)
      t146 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t89)
      t148 = FJET(XB1, XB2, s, t95, 0.0D0, t101, -t98, -t107, t143)
      t150 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t151 = s * t150
      t152 = t94 * x3
      t153 = t151 * t152
      t154 = -0.1D1 + x3
      t155 = t94 * t154
      t156 = t151 * t155
      t157 = t151 * t96
      t158 = t150 ** 2
      t162 = s * t158 * t6 * t105 * x3
      t163 = t8 * t158
      t166 = 0.1D1 / (-0.2D1 + t150)
      t167 = rrgg2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0
     #D0)
      t168 = t166 * t167
      t173 = rrgg2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0
     #D0)
      t176 = t158 ** 2
      t181 = log(-0.4D1 * t44 * t14 * t18 * t154 * t176)
      t194 = t7 * t163 * t168 * t139 / 0.8D1 + (0.90D2 * t7 * t8 * (t158
     # * t166 * t173 - t181 * t158 * t168) - 0.180D3 * t130 * t163 * t16
     #8) * t39 / 0.720D3
      t195 = FJET(XB1, XB2, s, t153, -t156, 0.0D0, -t157, -t162, t194)
      t197 = KAPPA2(x1, x2, x3, x4, z)
      t198 = s * t197
      t199 = t198 * t152
      t200 = t198 * t155
      t201 = t198 * t97
      t202 = t198 * t100
      t203 = t197 ** 2
      t208 = cos(t10)
      t211 = Sqrt(x3 * t154 * t115)
      t216 = s * t203 * t6 * t105 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t208 * t211)
      t220 = 0.1D1 / (-0.2D1 + t197)
      t221 = rrgg2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t225 = t7 * t8 * t203 * t220 * t221 * t139 / 0.8D1
      t226 = FJET(XB1, XB2, s, t199, -t200, -t201, t202, t216, -t225)
      t232 = t203 * t220 * t221 * t39 * t35
      t235 = FJET(XB1, XB2, s, -t156, t153, -t157, 0.0D0, -t162, t194)
      t237 = FJET(XB1, XB2, s, -t200, t199, t202, -t201, t216, -t225)
      rrgg2qqbarht4s1e0 = t90 * t89 + t144 * t143 + t146 * t89 + t148 * 
     #t143 + t195 * t194 - t226 * 0.3141592653589793D1 * t30 * t232 / 0.
     #8D1 + t235 * t194 - t237 * 0.3141592653589793D1 * t30 * t232 / 0.8
     #D1

      end function



      doubleprecision function rrgg2qqbarht4s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh41J1
      doubleprecision rrgg2qqbarh41J2
      doubleprecision rrgg2qqbarh41J3
      doubleprecision rrgg2qqbarh41J4
      doubleprecision rrgg2qqbarh41J5
      doubleprecision rrgg2qqbarh41J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.
     #0D0)
      t10 = t8 * t9
      t11 = 0.1D1 / x3
      t15 = rrgg2qqbarh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.0D0)
      t22 = sin(x2 * 0.3141592653589793D1)
      t23 = t22 ** 2
      t24 = z ** 2
      t27 = x1 ** 2
      t28 = t4 ** 2
      t32 = log(0.4D1 * t23 / t24 * t27 * t28)
      t39 = 0.1D1 / x4
      t43 = t7 * t10 * t11 / 0.8D1 + t7 * t8 * t15 / 0.8D1 + (-0.180D3 *
     # 0.3141592653589793D1 * lh - 0.90D2 * t32 * 0.3141592653589793D1) 
     #* t6 * t10 / 0.720D3 + t7 * t10 * t39 / 0.8D1
      t44 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t43)
      t46 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t47 = s * t46
      t48 = t1 * x1
      t49 = t47 * t48
      t50 = t1 * t4
      t52 = t47 * t50 * x4
      t55 = t47 * t50 * (-0.1D1 + x4)
      t56 = t46 ** 2
      t59 = x1 * t4
      t61 = s * t56 * t6 * t59 * x4
      t62 = t7 * t8
      t66 = rrgg2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x
     #4)
      t68 = t56 / (-0.2D1 + t46) * t66 * t39
      t70 = t62 * t68 / 0.8D1
      t71 = FJET(XB1, XB2, s, 0.0D0, t49, -t52, t55, -t61, t70)
      t73 = t6 * t8
      t77 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t43)
      t79 = FJET(XB1, XB2, s, t49, 0.0D0, t55, -t52, -t61, t70)
      t84 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t85 = s * t84
      t87 = t85 * t48 * x3
      t90 = t85 * t48 * (-0.1D1 + x3)
      t91 = t85 * t50
      t92 = t84 ** 2
      t96 = s * t92 * t6 * t59 * x3
      t100 = rrgg2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0
     #D0)
      t102 = t92 / (-0.2D1 + t84) * t100 * t11
      t104 = t62 * t102 / 0.8D1
      t105 = FJET(XB1, XB2, s, t87, -t90, 0.0D0, -t91, -t96, t104)
      t110 = FJET(XB1, XB2, s, -t90, t87, -t91, 0.0D0, -t96, t104)
      rrgg2qqbarht4s1em1 = t44 * t43 + t71 * 0.3141592653589793D1 * t73 
     #* t68 / 0.8D1 + t77 * t43 + t79 * 0.3141592653589793D1 * t73 * t68
     # / 0.8D1 + t105 * 0.3141592653589793D1 * t73 * t102 / 0.8D1 + t110
     # * 0.3141592653589793D1 * t73 * t102 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht4s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh41J1
      doubleprecision rrgg2qqbarh41J2
      doubleprecision rrgg2qqbarh41J3
      doubleprecision rrgg2qqbarh41J4
      doubleprecision rrgg2qqbarh41J5
      doubleprecision rrgg2qqbarh41J6
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t5 = t2 * (-0.1D1 + x1)
      t6 = t1 ** 2
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.
     #0D0)
      t12 = 0.3141592653589793D1 * t6 * t8 * t9 / 0.8D1
      t13 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t12)
      t16 = t6 * t8 * t9
      t18 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t12)
      rrgg2qqbarht4s1em2 = t13 * 0.3141592653589793D1 * t16 / 0.8D1 + t1
     #8 * 0.3141592653589793D1 * t16 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht4s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh41J1
      doubleprecision rrgg2qqbarh41J2
      doubleprecision rrgg2qqbarh41J3
      doubleprecision rrgg2qqbarh41J4
      doubleprecision rrgg2qqbarh41J5
      doubleprecision rrgg2qqbarh41J6
      rrgg2qqbarht4s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht4s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh41J1
      doubleprecision rrgg2qqbarh41J2
      doubleprecision rrgg2qqbarh41J3
      doubleprecision rrgg2qqbarh41J4
      doubleprecision rrgg2qqbarh41J5
      doubleprecision rrgg2qqbarh41J6
      rrgg2qqbarht4s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh41J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t1 * t2
      t4 = 0.1D1 - z
      t5 = 0.1D1 - x1
      t7 = 0.1D1 - x4
      t10 = t2 ** 2
      t11 = t1 * t10
      t12 = t4 ** 2
      t13 = t5 ** 2
      t15 = t7 ** 2
      t19 = 0.1D1 - x3
      t22 = x1 ** 2
      t24 = t19 ** 2
      rrgg2qqbarh41J1 = wd * (-t1 + t3 * t4 * t5 * t7 - t11 * t12 * t13 
     #* t15 + t3 * t4 * x1 * t19 - t11 * t12 * t22 * t24) * nf / s / z /
     # 0.3141592653589793D1 / 0.3D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh41J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t1 * t2
      t4 = 0.1D1 - z
      t5 = 0.1D1 - x1
      t7 = 0.1D1 - x4
      t9 = t3 * t4 * t5 * t7
      t10 = t2 ** 2
      t11 = t1 * t10
      t12 = t4 ** 2
      t13 = t5 ** 2
      t15 = t7 ** 2
      t17 = t11 * t12 * t13 * t15
      t19 = 0.1D1 - x3
      t21 = t3 * t4 * x1 * t19
      t22 = x1 ** 2
      t24 = t19 ** 2
      t26 = t11 * t12 * t22 * t24
      rrgg2qqbarh41J2 = (0.2D1 * wd * (-t1 + t9 - t17 + t21 - t26) + wd 
     #* (0.2D1 * t11 * t12 * x1 * t19 * t5 * t7 + t26 - 0.2D1 * t9 + t17
     # + 0.2D1 * t1 - 0.2D1 * t21)) * nf / s / z / 0.3141592653589793D1 
     #/ 0.3D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh41J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t1 * t2
      t4 = 0.1D1 - z
      t5 = 0.1D1 - x1
      t7 = 0.1D1 - x4
      t9 = t3 * t4 * t5 * t7
      t10 = t2 ** 2
      t11 = t1 * t10
      t12 = t4 ** 2
      t13 = t5 ** 2
      t15 = t7 ** 2
      t17 = t11 * t12 * t13 * t15
      t19 = 0.1D1 - x3
      t21 = t3 * t4 * x1 * t19
      t22 = x1 ** 2
      t24 = t19 ** 2
      t26 = t11 * t12 * t22 * t24
      rrgg2qqbarh41J3 = (0.3D1 * wd * (-t1 + t9 - t17 + t21 - t26) + 0.2
     #D1 * wd * (0.2D1 * t11 * t12 * x1 * t19 * t5 * t7 + t26 - 0.2D1 * 
     #t9 + t17 + 0.2D1 * t1 - 0.2D1 * t21)) * nf / s / z / 0.31415926535
     #89793D1 / 0.3D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh41J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t1 * t2
      t4 = 0.1D1 - z
      t5 = 0.1D1 - x1
      t7 = 0.1D1 - x4
      t9 = t3 * t4 * t5 * t7
      t10 = t2 ** 2
      t11 = t1 * t10
      t12 = t4 ** 2
      t13 = t5 ** 2
      t15 = t7 ** 2
      t17 = t11 * t12 * t13 * t15
      t19 = 0.1D1 - x3
      t21 = t3 * t4 * x1 * t19
      t22 = x1 ** 2
      t24 = t19 ** 2
      t26 = t11 * t12 * t22 * t24
      rrgg2qqbarh41J4 = (0.4D1 * wd * (-t1 + t9 - t17 + t21 - t26) + 0.3
     #D1 * wd * (0.2D1 * t11 * t12 * x1 * t19 * t5 * t7 + t26 - 0.2D1 * 
     #t9 + t17 + 0.2D1 * t1 - 0.2D1 * t21)) * nf / s / z / 0.31415926535
     #89793D1 / 0.3D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh41J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t1 * t2
      t4 = 0.1D1 - z
      t5 = 0.1D1 - x1
      t7 = 0.1D1 - x4
      t9 = t3 * t4 * t5 * t7
      t10 = t2 ** 2
      t11 = t1 * t10
      t12 = t4 ** 2
      t13 = t5 ** 2
      t15 = t7 ** 2
      t17 = t11 * t12 * t13 * t15
      t19 = 0.1D1 - x3
      t21 = t3 * t4 * x1 * t19
      t22 = x1 ** 2
      t24 = t19 ** 2
      t26 = t11 * t12 * t22 * t24
      rrgg2qqbarh41J5 = (0.5D1 * wd * (-t1 + t9 - t17 + t21 - t26) + 0.4
     #D1 * wd * (0.2D1 * t11 * t12 * x1 * t19 * t5 * t7 + t26 - 0.2D1 * 
     #t9 + t17 + 0.2D1 * t1 - 0.2D1 * t21)) * nf / s / z / 0.31415926535
     #89793D1 / 0.3D1

      end function
  
   
 

      doubleprecision function rrgg2qqbarh41J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t8 = 0.1D1 - x3
      t10 = 0.1D1 - x1
      t11 = 0.1D1 - x4
      t16 = x1 ** 2
      t18 = t8 ** 2
      t21 = t1 * t2
      t26 = t10 ** 2
      t28 = t11 ** 2
      rrgg2qqbarh41J6 = 0.5D1 / 0.3D1 * wd * (0.2D1 * t4 * t6 * x1 * t8 
     #* t10 * t11 + t4 * t6 * t16 * t18 - 0.2D1 * t21 * t5 * t10 * t11 +
     # t4 * t6 * t26 * t28 + 0.2D1 * t1 - 0.2D1 * t21 * t5 * x1 * t8) * 
     #nf / s / z / 0.3141592653589793D1

      end function
  
 