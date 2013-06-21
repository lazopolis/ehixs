  
      subroutine rrqqbar2gght4
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2ggh41J1  
      doubleprecision rrqqbar2ggh41J2  
      doubleprecision rrqqbar2ggh41J3  
      doubleprecision rrqqbar2gght4s1e1  
      doubleprecision rrqqbar2gght4s1e0  
      doubleprecision rrqqbar2gght4s1em1  
      doubleprecision rrqqbar2gght4s1em2  
      doubleprecision rrqqbar2gght4s1em3  
      doubleprecision rrqqbar2gght4s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght4s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght4s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght4s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght4s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght4s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght4s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2gght4s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh41J1
      doubleprecision rrqqbar2ggh41J2
      doubleprecision rrqqbar2ggh41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.
     #0D0)
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t17 = t14 * t16
      t18 = x1 ** 2
      t19 = t4 ** 2
      t20 = t18 * t19
      t24 = log(0.4D1 * t17 * t20 * x4)
      t25 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.0D0)
      t27 = t24 ** 2
      t28 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.0D0)
      t35 = 0.3141592653589793D1 * lh
      t36 = t1 * t7
      t42 = lh ** 2
      t44 = 0.3141592653589793D1 ** 2
      t46 = 0.180D3 * t42 - 0.30D2 * t44
      t47 = 0.3141592653589793D1 * t46
      t48 = t36 * t28
      t49 = t47 * t48
      t51 = 0.1D1 / x4
      t54 = t16 * t18
      t55 = t54 * t19
      t58 = log(0.4D1 * t14 * t55)
      t59 = t58 * 0.3141592653589793D1
      t67 = t58 ** 2
      t68 = t67 * 0.3141592653589793D1
      t90 = x3 * t11
      t91 = t90 * t13
      t96 = log(0.4D1 * t91 * t54 * t19 * x4)
      t105 = 0.1D1 / x3
      t110 = log(0.4D1 * t91 * t55)
      t112 = t110 ** 2
      t127 = -(0.90D2 * t6 * t7 * (t8 - t24 * t25 + t27 * t28 / 0.2D1) -
     # 0.180D3 * t35 * t36 * (t25 - t24 * t28) + t49) * t51 / 0.720D3 - 
     #(-0.180D3 * t35 - 0.90D2 * t59) * t1 * t7 * t8 / 0.720D3 - (t47 + 
     #0.180D3 * t59 * lh + 0.45D2 * t68) * t1 * t7 * t25 / 0.720D3 - (0.
     #3141592653589793D1 * (0.60D2 * lh * t44 - 0.2884936567583026D3 - 0
     #.120D3 * t42 * lh) - t59 * t46 - 0.90D2 * t68 * lh - 0.15D2 * t67 
     #* t58 * 0.3141592653589793D1) * t1 * t7 * t28 / 0.720D3 + (0.90D2 
     #* t6 * t7 * (-t25 + t96 * t28) + 0.180D3 * t35 * t48) * t105 * t51
     # / 0.720D3 - (0.90D2 * t6 * t7 * (t8 - t110 * t25 + t112 * t28 / 0
     #.2D1) - 0.180D3 * t35 * t36 * (t25 - t110 * t28) + t49) * t105 / 0
     #.720D3
      t128 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t127)
      t130 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t131 = s * t130
      t132 = t1 * x1
      t133 = t131 * t132
      t134 = t1 * t4
      t135 = t134 * x4
      t136 = t131 * t135
      t137 = -0.1D1 + x4
      t138 = t134 * t137
      t139 = t131 * t138
      t140 = t130 ** 2
      t143 = x1 * t4
      t145 = s * t140 * t15 * t143 * x4
      t147 = 0.1D1 / (-0.2D1 + t130)
      t148 = t140 * t147
      t149 = rrqqbar2ggh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t152 = t13 * t16
      t154 = x4 * t137
      t155 = t140 ** 2
      t160 = log(-0.4D1 * t18 * t11 * t152 * t154 * t155 * t19)
      t161 = t160 * t140
      t162 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t165 = t160 ** 2
      t167 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t168 = t147 * t167
      t175 = t148 * t162
      t181 = t47 * t1
      t183 = t7 * t140 * t168
      t187 = x3 * t18
      t195 = log(-0.4D1 * t187 * t14 * t16 * x4 * t137 * t19 * t155)
      t202 = t35 * t1
      t209 = -(0.90D2 * t6 * t7 * (t148 * t149 - t161 * t147 * t162 + t1
     #65 * t140 * t168 / 0.2D1) - 0.180D3 * t35 * t36 * (t175 - t161 * t
     #168) + t181 * t183) * t51 / 0.720D3 + (0.90D2 * t6 * t7 * (-t175 +
     # t195 * t140 * t168) + 0.180D3 * t202 * t183) * t105 * t51 / 0.720
     #D3
      t210 = FJET(XB1, XB2, s, 0.0D0, t133, -t136, t139, -t145, t209)
      t212 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t127)
      t214 = FJET(XB1, XB2, s, t133, 0.0D0, t139, -t136, -t145, t209)
      t216 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t217 = s * t216
      t218 = t132 * x3
      t219 = t217 * t218
      t220 = -0.1D1 + x3
      t221 = t132 * t220
      t222 = t217 * t221
      t223 = t217 * t134
      t224 = t216 ** 2
      t228 = s * t224 * t15 * t143 * x3
      t230 = 0.1D1 / (-0.2D1 + t216)
      t231 = t224 * t230
      t232 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0
     #D0)
      t233 = t231 * t232
      t234 = t90 * t152
      t236 = t224 ** 2
      t241 = log(-0.4D1 * t234 * t20 * t220 * x4 * t236)
      t243 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0
     #D0)
      t244 = t230 * t243
      t251 = t7 * t224 * t244
      t257 = rrqqbar2ggh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0
     #D0)
      t263 = log(-0.4D1 * t234 * t20 * t220 * t236)
      t264 = t263 * t224
      t267 = t263 ** 2
      t284 = (0.90D2 * t6 * t7 * (-t233 + t241 * t224 * t244) + 0.180D3 
     #* t202 * t251) * t105 * t51 / 0.720D3 - (0.90D2 * t6 * t7 * (t231 
     #* t257 - t264 * t230 * t232 + t267 * t224 * t244 / 0.2D1) - 0.180D
     #3 * t35 * t36 * (t233 - t264 * t244) + t181 * t251) * t105 / 0.720
     #D3
      t285 = FJET(XB1, XB2, s, t219, -t222, 0.0D0, -t223, -t228, t284)
      t287 = KAPPA2(x1, x2, x3, x4, z)
      t288 = s * t287
      t289 = t288 * t218
      t290 = t288 * t221
      t291 = t288 * t135
      t292 = t288 * t138
      t293 = t287 ** 2
      t298 = cos(t9)
      t301 = Sqrt(x3 * t220 * t154)
      t306 = s * t293 * t15 * t143 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t298 * t301)
      t308 = 0.1D1 / (-0.2D1 + t287)
      t310 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t314 = t293 ** 2
      t319 = log(0.4D1 * t187 * t17 * t154 * t19 * t220 * t314)
      t321 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t322 = t308 * t321
      t332 = 0.90D2 * t6 * t7 * (t293 * t308 * t310 - t319 * t293 * t322
     #) - 0.180D3 * t202 * t7 * t293 * t322
      t335 = t332 * t105 * t51 / 0.720D3
      t336 = FJET(XB1, XB2, s, t289, -t290, -t291, t292, t306, t335)
      t338 = t105 * t51
      t341 = FJET(XB1, XB2, s, -t222, t219, -t223, 0.0D0, -t228, t284)
      t343 = FJET(XB1, XB2, s, -t290, t289, t292, -t291, t306, t335)
      rrqqbar2gght4s1e1 = t128 * t127 + t210 * t209 + t212 * t127 + t214
     # * t209 + t285 * t284 + t336 * t332 * t338 / 0.720D3 + t341 * t284
     # + t343 * t332 * t338 / 0.720D3

      end function



      doubleprecision function rrqqbar2gght4s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh41J1
      doubleprecision rrqqbar2ggh41J2
      doubleprecision rrqqbar2ggh41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.
     #0D0)
      t9 = x2 * 0.3141592653589793D1
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 ** 2
      t18 = x1 ** 2
      t19 = t4 ** 2
      t20 = t18 * t19
      t24 = log(0.4D1 * t14 * t16 * t20 * x4)
      t25 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.0D0)
      t31 = 0.3141592653589793D1 * lh
      t32 = t1 * t7
      t35 = 0.180D3 * t31 * t32 * t25
      t37 = 0.1D1 / x4
      t41 = 0.1D1 / x3
      t46 = x3 * t11
      t49 = t16 * t18 * t19
      t52 = log(0.4D1 * t46 * t13 * t49)
      t61 = rrqqbar2ggh41J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.0D0)
      t68 = log(0.4D1 * t14 * t49)
      t69 = t68 * 0.3141592653589793D1
      t76 = lh ** 2
      t78 = 0.3141592653589793D1 ** 2
      t84 = t68 ** 2
      t92 = -(0.90D2 * t6 * t7 * (t8 - t24 * t25) - t35) * t37 / 0.720D3
     # - t6 * t7 * t25 * t41 * t37 / 0.8D1 - (0.90D2 * t6 * t7 * (t8 - t
     #52 * t25) - t35) * t41 / 0.720D3 - t6 * t7 * t61 / 0.8D1 - (-0.180
     #D3 * t31 - 0.90D2 * t69) * t1 * t7 * t8 / 0.720D3 - (0.31415926535
     #89793D1 * (0.180D3 * t76 - 0.30D2 * t78) + 0.180D3 * t69 * lh + 0.
     #45D2 * t84 * 0.3141592653589793D1) * t1 * t7 * t25 / 0.720D3
      t93 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t92)
      t95 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t96 = s * t95
      t97 = t1 * x1
      t98 = t96 * t97
      t99 = t1 * t4
      t100 = t99 * x4
      t101 = t96 * t100
      t102 = -0.1D1 + x4
      t103 = t99 * t102
      t104 = t96 * t103
      t105 = t95 ** 2
      t108 = x1 * t4
      t110 = s * t105 * t15 * t108 * x4
      t112 = 0.1D1 / (-0.2D1 + t95)
      t114 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t117 = t13 * t16
      t119 = x4 * t102
      t120 = t105 ** 2
      t125 = log(-0.4D1 * t18 * t11 * t117 * t119 * t120 * t19)
      t127 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t128 = t112 * t127
      t134 = t31 * t1
      t135 = t7 * t105
      t143 = t41 * t37
      t147 = -(0.90D2 * t6 * t7 * (t105 * t112 * t114 - t125 * t105 * t1
     #28) - 0.180D3 * t134 * t135 * t128) * t37 / 0.720D3 - t6 * t135 * 
     #t128 * t143 / 0.8D1
      t148 = FJET(XB1, XB2, s, 0.0D0, t98, -t101, t104, -t110, t147)
      t150 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t92)
      t152 = FJET(XB1, XB2, s, t98, 0.0D0, t104, -t101, -t110, t147)
      t154 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t155 = s * t154
      t156 = t97 * x3
      t157 = t156 * t155
      t158 = -0.1D1 + x3
      t159 = t97 * t158
      t160 = t155 * t159
      t161 = t155 * t99
      t162 = t154 ** 2
      t166 = s * t162 * t15 * t108 * x3
      t167 = t7 * t162
      t170 = 0.1D1 / (-0.2D1 + t154)
      t171 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0
     #D0)
      t172 = t170 * t171
      t177 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0
     #D0)
      t180 = t162 ** 2
      t185 = log(-0.4D1 * t46 * t117 * t20 * t158 * t180)
      t198 = -t6 * t167 * t172 * t143 / 0.8D1 - (0.90D2 * t6 * t7 * (t16
     #2 * t170 * t177 - t185 * t162 * t172) - 0.180D3 * t134 * t167 * t1
     #72) * t41 / 0.720D3
      t199 = FJET(XB1, XB2, s, t157, -t160, 0.0D0, -t161, -t166, t198)
      t201 = KAPPA2(x1, x2, x3, x4, z)
      t202 = s * t201
      t203 = t202 * t156
      t204 = t202 * t159
      t205 = t202 * t100
      t206 = t202 * t103
      t207 = t201 ** 2
      t212 = cos(t9)
      t215 = Sqrt(x3 * t158 * t119)
      t220 = s * t207 * t15 * t108 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t212 * t215)
      t224 = 0.1D1 / (-0.2D1 + t201)
      t225 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t229 = t6 * t7 * t207 * t224 * t225 * t143 / 0.8D1
      t230 = FJET(XB1, XB2, s, t203, -t204, -t205, t206, t220, t229)
      t236 = t207 * t224 * t225 * t41 * t37
      t239 = FJET(XB1, XB2, s, -t160, t157, -t161, 0.0D0, -t166, t198)
      t241 = FJET(XB1, XB2, s, -t204, t203, t206, -t205, t220, t229)
      rrqqbar2gght4s1e0 = t93 * t92 + t148 * t147 + t150 * t92 + t152 * 
     #t147 + t199 * t198 + t230 * 0.3141592653589793D1 * t32 * t236 / 0.
     #8D1 + t239 * t198 + t241 * 0.3141592653589793D1 * t32 * t236 / 0.8
     #D1

      end function



      doubleprecision function rrqqbar2gght4s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh41J1
      doubleprecision rrqqbar2ggh41J2
      doubleprecision rrqqbar2ggh41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t4 = -0.1D1 + x1
      t5 = t2 * t4
      t6 = 0.3141592653589793D1 * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.
     #0D0)
      t9 = t7 * t8
      t10 = 0.1D1 / x3
      t14 = rrqqbar2ggh41J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0
     #.0D0)
      t21 = sin(x2 * 0.3141592653589793D1)
      t22 = t21 ** 2
      t23 = z ** 2
      t26 = t1 ** 2
      t27 = t26 ** 2
      t28 = x1 ** 2
      t30 = t4 ** 2
      t34 = log(0.4D1 * t22 / t23 * t27 * t28 * t30)
      t41 = 0.1D1 / x4
      t45 = -t6 * t9 * t10 / 0.8D1 - t6 * t7 * t14 / 0.8D1 - (-0.180D3 *
     # 0.3141592653589793D1 * lh - 0.90D2 * t34 * 0.3141592653589793D1) 
     #* t1 * t9 / 0.720D3 - t6 * t9 * t41 / 0.8D1
      t46 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t45)
      t48 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t49 = s * t48
      t50 = t1 * x1
      t51 = t49 * t50
      t52 = t1 * t4
      t54 = t49 * t52 * x4
      t57 = t49 * t52 * (-0.1D1 + x4)
      t58 = t48 ** 2
      t61 = x1 * t4
      t63 = s * t58 * t26 * t61 * x4
      t64 = t6 * t7
      t68 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x
     #4)
      t70 = t58 / (-0.2D1 + t48) * t68 * t41
      t72 = t64 * t70 / 0.8D1
      t73 = FJET(XB1, XB2, s, 0.0D0, t51, -t54, t57, -t63, -t72)
      t75 = t1 * t7
      t79 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t45)
      t81 = FJET(XB1, XB2, s, t51, 0.0D0, t57, -t54, -t63, -t72)
      t86 = KAPPA2(x1, x2, x3, 0.0D0, z)
      t87 = s * t86
      t89 = t87 * t50 * x3
      t92 = t87 * t50 * (-0.1D1 + x3)
      t93 = t87 * t52
      t94 = t86 ** 2
      t98 = s * t94 * t26 * t61 * x3
      t102 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, 0.0
     #D0)
      t104 = t94 / (-0.2D1 + t86) * t102 * t10
      t106 = t64 * t104 / 0.8D1
      t107 = FJET(XB1, XB2, s, t89, -t92, 0.0D0, -t93, -t98, -t106)
      t112 = FJET(XB1, XB2, s, -t92, t89, -t93, 0.0D0, -t98, -t106)
      rrqqbar2gght4s1em1 = t46 * t45 - t73 * 0.3141592653589793D1 * t75 
     #* t70 / 0.8D1 + t79 * t45 - t81 * 0.3141592653589793D1 * t75 * t70
     # / 0.8D1 - t107 * 0.3141592653589793D1 * t75 * t104 / 0.8D1 - t112
     # * 0.3141592653589793D1 * t75 * t104 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght4s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh41J1
      doubleprecision rrqqbar2ggh41J2
      doubleprecision rrqqbar2ggh41J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x1
      t5 = t2 * (-0.1D1 + x1)
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh41J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 0.
     #0D0)
      t11 = 0.3141592653589793D1 * t1 * t7 * t8 / 0.8D1
      t12 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, -t11)
      t15 = t1 * t7 * t8
      t17 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, -t11)
      rrqqbar2gght4s1em2 = -t12 * 0.3141592653589793D1 * t15 / 0.8D1 - t
     #17 * 0.3141592653589793D1 * t15 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght4s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh41J1
      doubleprecision rrqqbar2ggh41J2
      doubleprecision rrqqbar2ggh41J3
      rrqqbar2gght4s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght4s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh41J1
      doubleprecision rrqqbar2ggh41J2
      doubleprecision rrqqbar2ggh41J3
      rrqqbar2gght4s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqqbar2ggh41J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 ** 2
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t7 = 0.1D1 - x1
      t13 = cos(x2 * 0.3141592653589793D1)
      t14 = 0.1D1 - x3
      t16 = 0.1D1 - x4
      t19 = Sqrt(x3 * t14 * x4 * t16)
      t22 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t13 * t19
      t23 = s ** 2
      t24 = t23 * s
      t25 = t2 ** 2
      t27 = t5 ** 2
      t29 = x1 ** 2
      t30 = t7 ** 2
      t32 = t22 ** 2
      t37 = s * t1
      t38 = x1 * t4
      t46 = t4 * t7
      rrqqbar2ggh41J1 = -0.16D2 / 0.27D2 * wd * t2 * t5 * x1 * t7 * t22 
     #* (-t24 - t24 * t25 * t27 * t29 * t30 * t32) / (s - t37 * t38 * x3
     # - t37 * t38 * t14) / (s - t37 * t46 * x4 - t37 * t46 * t16) / z /
     # 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrqqbar2ggh41J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 ** 2
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t7 = 0.1D1 - x1
      t13 = cos(x2 * 0.3141592653589793D1)
      t14 = 0.1D1 - x3
      t16 = 0.1D1 - x4
      t19 = Sqrt(x3 * t14 * x4 * t16)
      t22 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t13 * t19
      t23 = s * t1
      t24 = x1 * t4
      t29 = s - t23 * t24 * x3 - t23 * t24 * t14
      t31 = t4 * t7
      t36 = s - t23 * t31 * x4 - t23 * t31 * t16
      t38 = s ** 2
      t39 = t38 * s
      t40 = t2 ** 2
      t42 = t5 ** 2
      t44 = x1 ** 2
      t45 = t7 ** 2
      t47 = t22 ** 2
      rrqqbar2ggh41J2 = -0.16D2 / 0.27D2 * wd * t2 * t5 * x1 * t7 * t22 
     #* (-s * t29 * t36 + t39 * t40 * t42 * t44 * t45 * t47 - 0.2D1 * t3
     #9 * t2 * t5 * x1 * t7 * t22 + t39) / t29 / t36 / z / 0.31415926535
     #89793D1

      end function
  
   
 

      doubleprecision function rrqqbar2ggh41J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 ** 2
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 * x1
      t7 = 0.1D1 - x1
      t13 = cos(x2 * 0.3141592653589793D1)
      t14 = 0.1D1 - x3
      t16 = 0.1D1 - x4
      t19 = Sqrt(x3 * t14 * x4 * t16)
      t22 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t13 * t19
      t23 = s * t1
      t24 = x1 * t4
      t29 = s - t23 * t24 * x3 - t23 * t24 * t14
      t31 = t4 * t7
      t36 = s - t23 * t31 * x4 - t23 * t31 * t16
      t37 = s * t29 * t36
      t38 = t1 * t4
      t42 = s ** 2
      t43 = t42 * s
      t52 = t7 * t22
      t57 = t2 ** 2
      t59 = t5 ** 2
      t61 = x1 ** 2
      t62 = t7 ** 2
      t64 = t22 ** 2
      t70 = t36 * t42
      t73 = t14 ** 2
      t76 = t2 * t1
      t77 = t5 * t4
      t85 = t16 ** 2
      t96 = -t37 * t38 * x1 * t14 - 0.2D1 * t43 * t2 * t5 * x1 * t7 * t2
     #2 + 0.2D1 * s * t2 * t6 * t52 * t29 * t36 - t37 + t43 * t57 * t59 
     #* t61 * t62 * t64 - t37 * t38 * t7 * t16 + t43 - t70 * t2 * t5 * t
     #61 * t73 + t70 * t76 * t77 * t61 * t14 * t52 - t42 * t2 * t5 * t62
     # * t85 * t29 + t42 * t76 * t77 * t62 * t16 * t29 * x1 * t22
      rrqqbar2ggh41J3 = -0.16D2 / 0.27D2 * wd * t2 * t6 * t7 * t22 * t96
     # / t29 / t36 / z / 0.3141592653589793D1

      end function
  
 