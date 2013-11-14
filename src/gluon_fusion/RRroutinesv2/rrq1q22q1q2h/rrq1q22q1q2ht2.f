  
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
      t20 = x2 * pi
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
      t57 = pi ** 2
      t59 = lh ** 2
      t61 = -0.30D2 * t57 + 0.180D3 * t59
      t62 = t15 * t61
      t64 = wd * t19
      t66 = t22 * t34 * t43
      t67 = t64 * t66
      t68 = t62 * t49 * t67
      t70 = 0.1D1 / x4
      t83 = log(0.4D1 * t30 * t33 * t34)
      t86 = t83 ** 2
      t101 = x3 * t25
      t106 = log(0.4D1 * t101 * t27 * t29 * t36)
      t111 = t48 * t49
      t115 = 0.1D1 / x3
      t120 = t29 * t31
      t125 = log(0.4D1 * t101 * t27 * t120 * t32 * t34)
      t126 = t125 ** 2
      t140 = -0.16D2 / 0.405D3 * (0.45D2 * t18 * t23 * t40 * t34 * t43 +
     # 0.180D3 * t51 * t23 * t39 * t34 * t43 + t68) * t70 - 0.16D2 / 0.4
     #05D3 * (t15 * (-0.240D3 * zeta3 - 0.120D3 * t59 * lh + 0.60D2 * lh
     # * t57) - t83 * t15 * t61 - 0.90D2 * t86 * t15 * lh - 0.15D2 * t86
     # * t83 * t15) * x1 * t17 * t19 * t34 * t22 * t43 + 0.4D1 / 0.405D3
     # * (0.360D3 * t18 * t19 * t106 * t66 + 0.720D3 * t111 * t67) * t11
     #5 * t70 + 0.16D2 / 0.405D3 * (-0.45D2 * t18 * t23 * t126 * t34 * t
     #43 - 0.180D3 * t51 * t23 * t125 * t34 * t43 - t68) * t115
      t141 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t7, 0.0D0, -s * t9 
     #* t11 * x1 * t6, t140)
      t143 = sqrt(x4)
      t144 = -0.1D1 + t143
      t145 = t143 + 0.1D1
      t146 = t144 * t145
      t147 = KAPPA2(x1, x2, 0.0D0, -t146, z)
      t148 = s * t147
      t152 = t6 * t144 * t145
      t154 = t7 * x4
      t156 = t147 ** 2
      t163 = t16 * t17 * t19
      t166 = t156 ** 2
      t168 = t28 * t146 * t166
      t171 = log(-0.4D1 * t120 * t32 * x4 * t168)
      t172 = t171 ** 2
      t175 = 0.1D1 / (-0.2D1 + t147)
      t177 = Sqrt(-t146)
      t178 = t177 ** 2
      t179 = t166 * t175 * t178
      t187 = t62 * t50
      t188 = t23 * t179
      t193 = t32 * x3
      t195 = t120 * t193 * x4
      t198 = log(-0.4D1 * t195 * t168)
      t211 = -0.16D2 / 0.405D3 * (-0.45D2 * t163 * t22 * t172 * t179 - 0
     #.180D3 * t51 * t23 * t171 * t179 - t187 * t188) * t70 + 0.4D1 / 0.
     #405D3 * (-0.360D3 * t163 * t198 * t166 * t175 * t22 * t178 - 0.720
     #D3 * t51 * t188) * t115 * t70
      t212 = FJET(XB1, XB2, s, 0.0D0, t148 * t4, t148 * t3 * t152, -t148
     # * t154, s * t156 * t11 * t49 * (-0.1D1 + x4), t211)
      t214 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t215 = s * t214
      t216 = t4 * x3
      t219 = sqrt(x3)
      t220 = -0.1D1 + t219
      t222 = t219 + 0.1D1
      t223 = x1 * t220 * t222
      t226 = t214 ** 2
      t232 = t220 * t222
      t233 = t226 ** 2
      t235 = t28 * t232 * t233
      t238 = log(-0.4D1 * t195 * t235)
      t240 = 0.1D1 / (-0.2D1 + t214)
      t243 = Sqrt(-t232)
      t244 = t243 ** 2
      t245 = t233 * t22 * t244
      t250 = t19 * t240 * t245
      t260 = log(-0.4D1 * t120 * t193 * t235)
      t261 = t260 ** 2
      t264 = t240 * t233 * t244
      t276 = 0.4D1 / 0.405D3 * (-0.360D3 * t163 * t238 * t240 * t245 - 0
     #.720D3 * t51 * t250) * t115 * t70 + 0.16D2 / 0.405D3 * (0.45D2 * t
     #163 * t22 * t261 * t264 + 0.180D3 * t51 * t23 * t260 * t264 + t187
     # * t250) * t115
      t277 = FJET(XB1, XB2, s, t215 * t216, -t215 * t3 * t223, -t215 * t
     #7, 0.0D0, s * t226 * t11 * t49 * (-0.1D1 + x3), t276)
      t279 = KAPPA2(x1, x2, x3, -t146, z)
      t280 = s * t279
      t282 = t280 * t3
      t286 = t279 ** 2
      t289 = x3 * x4
      t293 = Sqrt(t232 * t146)
      t305 = t286 ** 2
      t311 = log(0.4D1 * t120 * t32 * t289 * t25 * t27 * t144 * t145 * t
     #305 * t220 * t222)
      t317 = (-t219 * t143 + 0.2D1 * t21 * t293) ** 2
      t321 = t317 / (-0.2D1 + t279) * t305
      t328 = 0.90D2 * t18 * t19 * t311 * t321 + 0.180D3 * t111 * t64 * t
     #321
      t332 = FJET(XB1, XB2, s, t280 * t216, -t282 * t223, t282 * t152, -
     #t280 * t154, s * t286 * t11 * t49 * (x3 - 0.1D1 + x4 - 0.2D1 * t28
     #9 + 0.2D1 * t21 * t219 * t143 * t293), 0.4D1 / 0.405D3 * t328 * t1
     #15 * t70)
      rrq1q22q1q2ht2s1e1 = t141 * t140 + t212 * t211 + t277 * t276 + 0.4
     #D1 / 0.405D3 * t332 * t328 * t115 * t70

      end function



      doubleprecision function rrq1q22q1q2ht2s1e0
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
      t20 = x2 * pi
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
      t84 = pi ** 2
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
      rrq1q22q1q2ht2s1e0 = t109 * t108 + t166 * t165 + t221 * t220 - 0.8
     #D1 / 0.9D1 * t258 * t15 * t148 * t19 * t248 * t250 * t254

      end function



      doubleprecision function rrq1q22q1q2ht2s1em1
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
      t22 = x2 * pi
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
      rrq1q22q1q2ht2s1em1 = t63 * t62 + 0.32D2 / 0.9D1 * t98 * t15 * t10
     #0 * t102 * t87 * t94 + 0.32D2 / 0.9D1 * t137 * t15 * t100 * t102 *
     # t126 * t133

      end function



      doubleprecision function rrq1q22q1q2ht2s1em2
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
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
      t6 = -0.1D1 + x1
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = t11 * t3
      t19 = 0.1D1 / z
      t20 = t9 ** 2
      t23 = cos(x2 * pi)
      t24 = t23 ** 2
      t26 = 0.1D1 / (-0.2D1 + t1)
      t31 = FJET(XB1, XB2, s, 0.0D0, t2 * t3 * x1, -t2 * t3 * t6, 0.0D0,
     # -s * t9 * t11 * x1 * t6, -0.32D2 / 0.9D1 * t15 * x1 * t6 * wd * t
     #19 * t20 * t24 * t26)
      rrq1q22q1q2ht2s1em2 = -0.32D2 / 0.9D1 * t31 * t15 * x1 * t6 * wd *
     # t19 * t20 * t24 * t26

      end function



      doubleprecision function rrq1q22q1q2ht2s1em3
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
      rrq1q22q1q2ht2s1em3 = 0.0D0

      end function



      doubleprecision function rrq1q22q1q2ht2s1em4
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
      rrq1q22q1q2ht2s1em4 = 0.0D0

      end function
