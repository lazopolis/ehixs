  
      subroutine rrq1q22q1q2ht2
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrq1q22q1q2ht2s1e1  
      doubleprecision rrq1q22q1q2ht2s1e0  
      doubleprecision rrq1q22q1q2ht2s1em1  
      doubleprecision rrq1q22q1q2ht2s1em2  
      doubleprecision rrq1q22q1q2ht2s1em3  
      doubleprecision rrq1q22q1q2ht2s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrq1q22q1q2ht2s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrq1q22q1q2ht2s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrq1q22q1q2ht2s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrq1q22q1q2ht2s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrq1q22q1q2ht2s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrq1q22q1q2ht2s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrq1q22q1q2ht2s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = t11 * t3
      t16 = t15 * x1
      t17 = t6 * wd
      t18 = t16 * t17
      t19 = 0.1D1 / z
      t20 = x2 * 0.3141592653589793D1
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = t19 * t22
      t24 = sin(t20)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t28 = t25 * t27
      t29 = t11 ** 2
      t30 = t28 * t29
      t31 = x1 ** 2
      t32 = t6 ** 2
      t33 = t31 * t32
      t34 = t9 ** 2
      t36 = t33 * x4 * t34
      t39 = log(0.4D1 * t30 * t36)
      t40 = t39 ** 2
      t43 = 0.1D1 / (-0.2D1 + t1)
      t48 = t15 * lh
      t49 = x1 * t6
      t50 = t49 * wd
      t51 = t48 * t50
      t57 = 0.3141592653589793D1 ** 2
      t59 = lh ** 2
      t61 = -0.30D2 * t57 + 0.180D3 * t59
      t62 = t15 * t61
      t64 = wd * t19
      t66 = t22 * t34 * t43
      t67 = t64 * t66
      t68 = t62 * t49 * t67
      t70 = 0.1D1 / x4
      t82 = log(0.4D1 * t30 * t33 * t34)
      t85 = t82 ** 2
      t100 = x3 * t25
      t105 = log(0.4D1 * t100 * t27 * t29 * t36)
      t110 = t48 * t49
      t114 = 0.1D1 / x3
      t119 = t29 * t31
      t124 = log(0.4D1 * t100 * t27 * t119 * t32 * t34)
      t125 = t124 ** 2
      t139 = -0.16D2 / 0.405D3 * (0.45D2 * t18 * t23 * t40 * t34 * t43 +
     # 0.180D3 * t51 * t23 * t39 * t34 * t43 + t68) * t70 - 0.16D2 / 0.4
     #05D3 * (t15 * (-0.2884936567583026D3 - 0.120D3 * t59 * lh + 0.60D2
     # * lh * t57) - t82 * t15 * t61 - 0.90D2 * t85 * t15 * lh - 0.15D2 
     #* t85 * t82 * t15) * x1 * t17 * t19 * t34 * t22 * t43 + 0.4D1 / 0.
     #405D3 * (0.360D3 * t18 * t19 * t105 * t66 + 0.720D3 * t110 * t67) 
     #* t114 * t70 + 0.16D2 / 0.405D3 * (-0.45D2 * t18 * t23 * t125 * t3
     #4 * t43 - 0.180D3 * t51 * t23 * t124 * t34 * t43 - t68) * t114
      t140 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t7, 0.0D0, -s * t9 
     #* t11 * x1 * t6, t139)
      t142 = sqrt(x4)
      t143 = -0.1D1 + t142
      t144 = t142 + 0.1D1
      t145 = t143 * t144
      t146 = KAPPA2(x1, x2, 0.0D0, -t145, z)
      t147 = s * t146
      t151 = t6 * t143 * t144
      t153 = t7 * x4
      t155 = t146 ** 2
      t162 = t16 * t17 * t19
      t165 = t155 ** 2
      t167 = t28 * t145 * t165
      t170 = log(-0.4D1 * t119 * t32 * x4 * t167)
      t171 = t170 ** 2
      t174 = 0.1D1 / (-0.2D1 + t146)
      t176 = Sqrt(-t145)
      t177 = t176 ** 2
      t178 = t165 * t174 * t177
      t186 = t62 * t50
      t187 = t23 * t178
      t192 = t32 * x3
      t194 = t119 * t192 * x4
      t197 = log(-0.4D1 * t194 * t167)
      t210 = -0.16D2 / 0.405D3 * (-0.45D2 * t162 * t22 * t171 * t178 - 0
     #.180D3 * t51 * t23 * t170 * t178 - t186 * t187) * t70 + 0.4D1 / 0.
     #405D3 * (-0.360D3 * t162 * t197 * t165 * t174 * t22 * t177 - 0.720
     #D3 * t51 * t187) * t114 * t70
      t211 = FJET(XB1, XB2, s, 0.0D0, t147 * t4, t147 * t3 * t151, -t147
     # * t153, s * t155 * t11 * t49 * (-0.1D1 + x4), t210)
      t213 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t214 = s * t213
      t215 = t4 * x3
      t218 = sqrt(x3)
      t219 = -0.1D1 + t218
      t221 = t218 + 0.1D1
      t222 = x1 * t219 * t221
      t225 = t213 ** 2
      t231 = t219 * t221
      t232 = t225 ** 2
      t234 = t28 * t231 * t232
      t237 = log(-0.4D1 * t194 * t234)
      t239 = 0.1D1 / (-0.2D1 + t213)
      t242 = Sqrt(-t231)
      t243 = t242 ** 2
      t244 = t232 * t22 * t243
      t249 = t19 * t239 * t244
      t259 = log(-0.4D1 * t119 * t192 * t234)
      t260 = t259 ** 2
      t263 = t239 * t232 * t243
      t275 = 0.4D1 / 0.405D3 * (-0.360D3 * t162 * t237 * t239 * t244 - 0
     #.720D3 * t51 * t249) * t114 * t70 + 0.16D2 / 0.405D3 * (0.45D2 * t
     #162 * t22 * t260 * t263 + 0.180D3 * t51 * t23 * t259 * t263 + t186
     # * t249) * t114
      t276 = FJET(XB1, XB2, s, t214 * t215, -t214 * t3 * t222, -t214 * t
     #7, 0.0D0, s * t225 * t11 * t49 * (-0.1D1 + x3), t275)
      t278 = KAPPA2(x1, x2, x3, -t145, z)
      t279 = s * t278
      t281 = t279 * t3
      t285 = t278 ** 2
      t288 = x3 * x4
      t292 = Sqrt(t231 * t145)
      t304 = t285 ** 2
      t310 = log(0.4D1 * t119 * t32 * t288 * t25 * t27 * t143 * t144 * t
     #304 * t219 * t221)
      t316 = (-t218 * t142 + 0.2D1 * t21 * t292) ** 2
      t320 = t316 / (-0.2D1 + t278) * t304
      t327 = 0.90D2 * t18 * t19 * t310 * t320 + 0.180D3 * t110 * t64 * t
     #320
      t331 = FJET(XB1, XB2, s, t279 * t215, -t281 * t222, t281 * t151, -
     #t279 * t153, s * t285 * t11 * t49 * (x3 - 0.1D1 + x4 - 0.2D1 * t28
     #8 + 0.2D1 * t21 * t218 * t142 * t292), 0.4D1 / 0.405D3 * t327 * t1
     #14 * t70)
      rrq1q22q1q2ht2s1e1 = t140 * t139
     #+ t211 * t210 + t276 * t275 + 0.4D1 /
     # 0.405D3 * t331 * t327 * t114 * t70

      end function



      doubleprecision function rrq1q22q1q2ht2s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = t11 * t3
      t16 = t15 * x1
      t17 = t6 * wd
      t18 = t16 * t17
      t19 = 0.1D1 / z
      t20 = x2 * 0.3141592653589793D1
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = t19 * t22
      t24 = sin(t20)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t28 = t25 * t27
      t29 = t11 ** 2
      t30 = t28 * t29
      t31 = x1 ** 2
      t32 = t6 ** 2
      t33 = t31 * t32
      t34 = t9 ** 2
      t39 = log(0.4D1 * t30 * t33 * x4 * t34)
      t42 = 0.1D1 / (-0.2D1 + t1)
      t47 = t15 * lh
      t48 = x1 * t6
      t51 = t34 * t22
      t55 = 0.180D3 * t47 * t48 * wd * t19 * t51 * t42
      t57 = 0.1D1 / x4
      t61 = t16 * t17 * t19
      t62 = 0.1D1 / x3
      t70 = t29 * t31
      t75 = log(0.4D1 * x3 * t25 * t27 * t70 * t32 * t34)
      t84 = 0.3141592653589793D1 ** 2
      t86 = lh ** 2
      t93 = log(0.4D1 * t30 * t33 * t34)
      t97 = t93 ** 2
      t108 = -0.16D2 / 0.405D3 * (-0.90D2 * t18 * t23 * t39 * t34 * t42 
     #- t55) * t57 - 0.32D2 / 0.9D1 * t61 * t51 * t42 * t62 * t57 + 0.16
     #D2 / 0.405D3 * (0.90D2 * t18 * t23 * t75 * t34 * t42 + t55) * t62 
     #- 0.16D2 / 0.405D3 * (t15 * (-0.30D2 * t84 + 0.180D3 * t86) + 0.18
     #0D3 * t93 * t15 * lh + 0.45D2 * t97 * t15) * x1 * t17 * t19 * t34 
     #* t22 * t42
      t109 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t7, 0.0D0, -s * t9 
     #* t11 * x1 * t6, t108)
      t111 = sqrt(x4)
      t112 = -0.1D1 + t111
      t113 = t111 + 0.1D1
      t114 = t112 * t113
      t115 = KAPPA2(x1, x2, 0.0D0, -t114, z)
      t116 = s * t115
      t120 = t6 * t112 * t113
      t122 = t7 * x4
      t124 = t115 ** 2
      t132 = t124 ** 2
      t137 = log(-0.4D1 * t70 * t32 * x4 * t28 * t114 * t132)
      t140 = 0.1D1 / (-0.2D1 + t115)
      t141 = t132 * t140
      t142 = Sqrt(-t114)
      t143 = t142 ** 2
      t148 = t48 * wd
      t149 = t47 * t148
      t165 = -0.16D2 / 0.405D3 * (0.90D2 * t61 * t22 * t137 * t141 * t14
     #3 + 0.180D3 * t149 * t19 * t132 * t140 * t22 * t143) * t57 + 0.32D
     #2 / 0.9D1 * t61 * t141 * t22 * t143 * t62 * t57
      t166 = FJET(XB1, XB2, s, 0.0D0, t116 * t4, t116 * t3 * t120, -t116
     # * t122, s * t124 * t11 * t48 * (-0.1D1 + x4), t165)
      t168 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t169 = s * t168
      t170 = t4 * x3
      t173 = sqrt(x3)
      t174 = -0.1D1 + t173
      t176 = t173 + 0.1D1
      t177 = x1 * t174 * t176
      t180 = t168 ** 2
      t187 = 0.1D1 / (-0.2D1 + t168)
      t188 = t180 ** 2
      t189 = t187 * t188
      t191 = t174 * t176
      t192 = Sqrt(-t191)
      t193 = t192 ** 2
      t205 = log(-0.4D1 * t70 * t32 * x3 * t28 * t191 * t188)
      t220 = 0.32D2 / 0.9D1 * t61 * t189 * t22 * t193 * t62 * t57 + 0.16
     #D2 / 0.405D3 * (-0.90D2 * t61 * t22 * t205 * t189 * t193 - 0.180D3
     # * t149 * t19 * t187 * t188 * t22 * t193) * t62
      t221 = FJET(XB1, XB2, s, t169 * t170, -t169 * t3 * t177, -t169 * t
     #7, 0.0D0, s * t180 * t11 * t48 * (-0.1D1 + x3), t220)
      t223 = KAPPA2(x1, x2, x3, -t114, z)
      t224 = s * t223
      t226 = t224 * t3
      t230 = t223 ** 2
      t237 = Sqrt(t191 * t114)
      t248 = (-t173 * t111 + 0.2D1 * t21 * t237) ** 2
      t250 = 0.1D1 / (-0.2D1 + t223)
      t252 = t230 ** 2
      t254 = t252 * t62 * t57
      t258 = FJET(XB1, XB2, s, t224 * t170, -t226 * t177, t226 * t120, -
     #t224 * t122, s * t230 * t11 * t48 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 
     #* x4 + 0.2D1 * t21 * t173 * t111 * t237), -0.8D1 / 0.9D1 * t61 * t
     #248 * t250 * t254)
      rrq1q22q1q2ht2s1e0 = t109 * t108
     #+ t166 * t165 + t221 * t220 - 0.8D1 /
     # 0.9D1 * t258 * t15 * t148 * t19 * t248 * t250 * t254

      end function



      doubleprecision function rrq1q22q1q2ht2s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = t11 * t3
      t16 = t15 * x1
      t17 = t6 * wd
      t18 = t16 * t17
      t19 = 0.1D1 / z
      t20 = t9 ** 2
      t21 = t19 * t20
      t22 = x2 * 0.3141592653589793D1
      t23 = cos(t22)
      t24 = t23 ** 2
      t27 = t24 / (-0.2D1 + t1)
      t28 = 0.1D1 / x3
      t35 = sin(t22)
      t36 = t35 ** 2
      t37 = z ** 2
      t40 = t11 ** 2
      t42 = x1 ** 2
      t43 = t6 ** 2
      t48 = log(0.4D1 * t36 / t37 * t40 * t42 * t43 * t20)
      t57 = 0.1D1 / x4
      t62 = -0.32D2 / 0.9D1 * t18 * t21 * t27 * t28 - 0.16D2 / 0.405D3 *
     # (-0.180D3 * t15 * lh - 0.90D2 * t48 * t15) * x1 * t17 * t21 * t27
     # - 0.32D2 / 0.9D1 * t18 * t21 * t27 * t57
      t63 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t7, 0.0D0, -s * t9 *
     # t11 * x1 * t6, t62)
      t65 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t66 = s * t65
      t70 = sqrt(x3)
      t71 = -0.1D1 + t70
      t73 = t70 + 0.1D1
      t77 = t65 ** 2
      t80 = x1 * t6
      t85 = t16 * t17 * t19
      t87 = 0.1D1 / (-0.2D1 + t65)
      t89 = t77 ** 2
      t91 = Sqrt(-t71 * t73)
      t92 = t91 ** 2
      t94 = t89 * t92 * t28
      t98 = FJET(XB1, XB2, s, t66 * t4 * x3, -t66 * t3 * x1 * t71 * t73,
     # -t66 * t7, 0.0D0, s * t77 * t11 * t80 * (-0.1D1 + x3), 0.32D2 / 0
     #.9D1 * t85 * t24 * t87 * t94)
      t100 = t80 * wd
      t102 = t19 * t24
      t107 = sqrt(x4)
      t108 = -0.1D1 + t107
      t109 = t107 + 0.1D1
      t110 = t109 * t108
      t111 = KAPPA2(x1, x2, 0.0D0, -t110, z)
      t112 = s * t111
      t120 = t111 ** 2
      t126 = t120 ** 2
      t130 = Sqrt(-t110)
      t131 = t130 ** 2
      t133 = 0.1D1 / (-0.2D1 + t111) * t131 * t57
      t137 = FJET(XB1, XB2, s, 0.0D0, t112 * t4, t112 * t3 * t6 * t108 *
     # t109, -t112 * t7 * x4, s * t120 * t11 * t80 * (-0.1D1 + x4), 0.32
     #D2 / 0.9D1 * t85 * t24 * t126 * t133)
      rrq1q22q1q2ht2s1em1 = t63 * t62 + 0.32D2 / 0.9D1 * t98 * t15 * t100 * 
     #t102 * t87 * t94 + 0.32D2 / 0.9D1 * t137 * t15 * t100 * t102 * t12
     #6 * t133

      end function



      doubleprecision function rrq1q22q1q2ht2s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
      t6 = -0.1D1 + x1
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = t11 * t3
      t19 = 0.1D1 / z
      t20 = t9 ** 2
      t23 = cos(x2 * 0.3141592653589793D1)
      t24 = t23 ** 2
      t26 = 0.1D1 / (-0.2D1 + t1)
      t31 = FJET(XB1, XB2, s, 0.0D0, t2 * t3 * x1, -t2 * t3 * t6, 0.0D0,
     # -s * t9 * t11 * x1 * t6, -0.32D2 / 0.9D1 * t15 * x1 * t6 * wd * t
     #19 * t20 * t24 * t26)
      rrq1q22q1q2ht2s1em2 = -0.32D2 / 0.9D1 * t31 * t15 * x1 * t6 * wd * t19
     # * t20 * t24 * t26

      end function



      doubleprecision function rrq1q22q1q2ht2s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrq1q22q1q2ht2s1em3 = 0.0D0

      end function



      doubleprecision function rrq1q22q1q2ht2s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrq1q22q1q2ht2s1em4 = 0.0D0

      end function
