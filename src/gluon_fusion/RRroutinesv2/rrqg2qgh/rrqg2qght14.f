  
      subroutine rrqg2qght14
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qght14s1e1  
      doubleprecision rrqg2qght14s1e0  
      doubleprecision rrqg2qght14s1em1  
      doubleprecision rrqg2qght14s1em2  
      doubleprecision rrqg2qght14s1em3  
      doubleprecision rrqg2qght14s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght14s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght14s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght14s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght14s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght14s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght14s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght14s1e1
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
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = 0.1D1 / z
      t16 = t15 * wd
      t17 = x1 * t6
      t18 = t16 * t17
      t19 = t11 * t3
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
      t48 = lh * t15
      t49 = wd * x1
      t50 = t49 * t6
      t51 = t48 * t50
      t57 = lh ** 2
      t59 = pi ** 2
      t61 = 0.180D3 * t57 - 0.30D2 * t59
      t62 = t61 * t15
      t64 = t6 * t19
      t66 = t22 * t34 * t43
      t67 = t64 * t66
      t68 = t62 * t49 * t67
      t70 = 0.1D1 / x4
      t81 = log(0.4D1 * t30 * t33 * t34)
      t83 = t81 ** 2
      t93 = x3 * t25
      t98 = log(0.4D1 * t93 * t27 * t29 * t36)
      t103 = t48 * t49
      t107 = 0.1D1 / x3
      t111 = t93 * t27
      t117 = log(0.4D1 * t111 * t29 * t31 * t32 * t34)
      t118 = t117 ** 2
      t132 = 0.4D1 / 0.45D2 * (-0.45D2 * t18 * t23 * t40 * t34 * t43 - 0
     #.180D3 * t51 * t23 * t39 * t34 * t43 - t68) * t70 - 0.4D1 / 0.45D2
     # * (0.60D2 * lh * t59 - 0.240D3 * zeta3 - 0.120D3 * t57 * lh - t81
     # * t61 - 0.90D2 * t83 * lh - 0.15D2 * t83 * t81) * t15 * t49 * t67
     # - (-0.360D3 * t18 * t19 * t98 * t66 - 0.720D3 * t103 * t67) * t10
     #7 * t70 / 0.45D2 + 0.4D1 / 0.45D2 * (-0.45D2 * t18 * t23 * t118 * 
     #t34 * t43 - 0.180D3 * t51 * t23 * t117 * t34 * t43 - t68) * t107
      t133 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t7, 0.0D0, -s * t9 
     #* t11 * x1 * t6, t132)
      t135 = sqrt(x4)
      t136 = -0.1D1 + t135
      t137 = t135 + 0.1D1
      t138 = t136 * t137
      t139 = KAPPA2(x1, x2, 0.0D0, -t138, z)
      t140 = s * t139
      t144 = t6 * t136 * t137
      t146 = t7 * x4
      t148 = t139 ** 2
      t155 = t16 * t17 * t19
      t156 = t148 ** 2
      t157 = t156 * t25
      t158 = t27 * t136
      t161 = t32 * t29
      t162 = t161 * x4
      t166 = log(-0.4D1 * t157 * t158 * t137 * t31 * t162)
      t167 = t166 ** 2
      t170 = 0.1D1 / (-0.2D1 + t139)
      t172 = Sqrt(-t138)
      t173 = t172 ** 2
      t174 = t156 * t170 * t173
      t182 = t62 * t50
      t183 = t23 * t174
      t188 = t158 * t137
      t190 = x3 * t31
      t194 = log(-0.4D1 * t157 * t188 * t190 * t162)
      t207 = 0.4D1 / 0.45D2 * (0.45D2 * t155 * t22 * t167 * t174 + 0.180
     #D3 * t51 * t23 * t166 * t174 + t182 * t183) * t70 - (0.360D3 * t15
     #5 * t194 * t156 * t170 * t173 * t22 + 0.720D3 * t51 * t183) * t107
     # * t70 / 0.45D2
      t208 = FJET(XB1, XB2, s, 0.0D0, t140 * t4, t140 * t3 * t144, -t140
     # * t146, s * t148 * t11 * t17 * (-0.1D1 + x4), t207)
      t210 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t211 = s * t210
      t212 = t4 * x3
      t215 = sqrt(x3)
      t216 = -0.1D1 + t215
      t218 = t215 + 0.1D1
      t219 = x1 * t216 * t218
      t222 = t210 ** 2
      t228 = t216 * t218
      t231 = t222 ** 2
      t236 = log(-0.4D1 * t228 * t111 * t33 * t29 * x4 * t231)
      t239 = 0.1D1 / (-0.2D1 + t210)
      t241 = Sqrt(-t228)
      t242 = t241 ** 2
      t243 = t239 * t22 * t242
      t248 = t19 * t231 * t243
      t260 = log(-0.4D1 * t228 * t28 * t190 * t161 * t231)
      t261 = t260 ** 2
      t264 = t231 * t239 * t242
      t276 = -(0.360D3 * t155 * t236 * t231 * t243 + 0.720D3 * t51 * t24
     #8) * t107 * t70 / 0.45D2 + 0.4D1 / 0.45D2 * (0.45D2 * t155 * t22 *
     # t261 * t264 + 0.180D3 * t51 * t23 * t260 * t264 + t182 * t248) * 
     #t107
      t277 = FJET(XB1, XB2, s, t211 * t212, -t211 * t3 * t219, -t211 * t
     #7, 0.0D0, s * t222 * t11 * t17 * (-0.1D1 + x3), t276)
      t279 = KAPPA2(x1, x2, x3, -t138, z)
      t280 = s * t279
      t282 = t280 * t3
      t286 = t279 ** 2
      t293 = Sqrt(t228 * t138)
      t302 = t286 ** 2
      t308 = log(0.4D1 * t228 * t25 * t188 * t302 * x3 * t31 * t162)
      t314 = (-t215 * t135 + 0.2D1 * t21 * t293) ** 2
      t318 = t302 * t314 / (-0.2D1 + t279)
      t325 = -0.90D2 * t18 * t19 * t308 * t318 - 0.180D3 * t103 * t64 * 
     #t318
      t329 = FJET(XB1, XB2, s, t280 * t212, -t282 * t219, t282 * t144, -
     #t280 * t146, s * t286 * t11 * t17 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 
     #* x4 + 0.2D1 * t21 * t215 * t135 * t293), -t325 * t107 * t70 / 0.4
     #5D2)
      rrqg2qght14s1e1 = t133 * t132 + t208 * t207 + t277 * t276 - t329 *
     # t325 * t107 * t70 / 0.45D2

      end function



      doubleprecision function rrqg2qght14s1e0
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
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = 0.1D1 / z
      t16 = t15 * wd
      t17 = x1 * t6
      t18 = t16 * t17
      t19 = t11 * t3
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
      t47 = lh * t15
      t48 = wd * x1
      t51 = t34 * t42
      t53 = t6 * t19 * t51 * t22
      t55 = 0.180D3 * t47 * t48 * t53
      t57 = 0.1D1 / x4
      t61 = t16 * t17 * t19
      t62 = 0.1D1 / x3
      t64 = t22 * t62 * t57
      t75 = log(0.4D1 * x3 * t25 * t27 * t29 * t31 * t32 * t34)
      t84 = lh ** 2
      t86 = pi ** 2
      t91 = log(0.4D1 * t30 * t33 * t34)
      t94 = t91 ** 2
      t101 = 0.4D1 / 0.45D2 * (0.90D2 * t18 * t23 * t39 * t34 * t42 + t5
     #5) * t57 - 0.8D1 * t61 * t51 * t64 + 0.4D1 / 0.45D2 * (0.90D2 * t1
     #8 * t23 * t75 * t34 * t42 + t55) * t62 - 0.4D1 / 0.45D2 * (0.180D3
     # * t84 - 0.30D2 * t86 + 0.180D3 * t91 * lh + 0.45D2 * t94) * t15 *
     # t48 * t53
      t102 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t7, 0.0D0, -s * t9 
     #* t11 * x1 * t6, t101)
      t104 = sqrt(x4)
      t105 = -0.1D1 + t104
      t106 = t104 + 0.1D1
      t107 = t105 * t106
      t108 = KAPPA2(x1, x2, 0.0D0, -t107, z)
      t109 = s * t108
      t113 = t6 * t105 * t106
      t115 = t7 * x4
      t117 = t108 ** 2
      t123 = t117 ** 2
      t128 = t32 * t29
      t133 = log(-0.4D1 * t123 * t25 * t27 * t105 * t106 * t31 * t128 * 
     #x4)
      t136 = 0.1D1 / (-0.2D1 + t108)
      t138 = Sqrt(-t107)
      t139 = t138 ** 2
      t140 = t123 * t136 * t139
      t144 = t48 * t6
      t145 = t47 * t144
      t158 = 0.4D1 / 0.45D2 * (-0.90D2 * t61 * t22 * t133 * t140 - 0.180
     #D3 * t145 * t19 * t123 * t136 * t139 * t22) * t57 + 0.8D1 * t61 * 
     #t140 * t64
      t159 = FJET(XB1, XB2, s, 0.0D0, t109 * t4, t109 * t3 * t113, -t109
     # * t115, s * t117 * t11 * t17 * (-0.1D1 + x4), t158)
      t161 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t162 = s * t161
      t163 = t4 * x3
      t166 = sqrt(x3)
      t167 = -0.1D1 + t166
      t169 = t166 + 0.1D1
      t170 = x1 * t167 * t169
      t173 = t161 ** 2
      t179 = t173 ** 2
      t181 = 0.1D1 / (-0.2D1 + t161)
      t182 = t179 * t181
      t184 = t167 * t169
      t185 = Sqrt(-t184)
      t186 = t185 ** 2
      t198 = log(-0.4D1 * t184 * t28 * x3 * t31 * t128 * t179)
      t213 = 0.8D1 * t61 * t182 * t22 * t186 * t62 * t57 + 0.4D1 / 0.45D
     #2 * (-0.90D2 * t61 * t22 * t198 * t182 * t186 - 0.180D3 * t145 * t
     #19 * t179 * t181 * t22 * t186) * t62
      t214 = FJET(XB1, XB2, s, t162 * t163, -t162 * t3 * t170, -t162 * t
     #7, 0.0D0, s * t173 * t11 * t17 * (-0.1D1 + x3), t213)
      t216 = KAPPA2(x1, x2, x3, -t107, z)
      t217 = s * t216
      t219 = t217 * t3
      t223 = t216 ** 2
      t230 = Sqrt(t184 * t107)
      t237 = t223 ** 2
      t242 = (-t166 * t104 + 0.2D1 * t21 * t230) ** 2
      t247 = 0.1D1 / (-0.2D1 + t216) * t62 * t57
      t251 = FJET(XB1, XB2, s, t217 * t163, -t219 * t170, t219 * t113, -
     #t217 * t115, s * t223 * t11 * t17 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 
     #* x4 + 0.2D1 * t21 * t166 * t104 * t230), -0.2D1 * t61 * t237 * t2
     #42 * t247)
      rrqg2qght14s1e0 = t102 * t101 + t159 * t158 + t214 * t213 - 0.2D1 
     #* t251 * t15 * t144 * t19 * t237 * t242 * t247

      end function



      doubleprecision function rrqg2qght14s1em1
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
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = 0.1D1 / z
      t16 = t15 * wd
      t17 = x1 * t6
      t18 = t16 * t17
      t19 = t11 * t3
      t20 = t9 ** 2
      t21 = t19 * t20
      t23 = 0.1D1 / (-0.2D1 + t1)
      t24 = x2 * pi
      t25 = cos(t24)
      t26 = t25 ** 2
      t27 = t23 * t26
      t28 = 0.1D1 / x3
      t34 = sin(t24)
      t35 = t34 ** 2
      t36 = z ** 2
      t39 = t11 ** 2
      t41 = x1 ** 2
      t42 = t6 ** 2
      t47 = log(0.4D1 * t35 / t36 * t39 * t41 * t42 * t20)
      t51 = wd * x1
      t59 = 0.1D1 / x4
      t64 = -0.8D1 * t18 * t21 * t27 * t28 - 0.4D1 / 0.45D2 * (-0.180D3 
     #* lh - 0.90D2 * t47) * t15 * t51 * t6 * t19 * t20 * t23 * t26 - 0.
     #8D1 * t18 * t21 * t27 * t59
      t65 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t7, 0.0D0, -s * t9 *
     # t11 * x1 * t6, t64)
      t67 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t68 = s * t67
      t72 = sqrt(x3)
      t73 = -0.1D1 + t72
      t75 = t72 + 0.1D1
      t79 = t67 ** 2
      t86 = t16 * t17 * t19
      t87 = t79 ** 2
      t92 = Sqrt(-t73 * t75)
      t93 = t92 ** 2
      t95 = 0.1D1 / (-0.2D1 + t67) * t93 * t28
      t99 = FJET(XB1, XB2, s, t68 * t4 * x3, -t68 * t3 * x1 * t73 * t75,
     # -t68 * t7, 0.0D0, s * t79 * t11 * t17 * (-0.1D1 + x3), 0.8D1 * t8
     #6 * t26 * t87 * t95)
      t101 = t51 * t6
      t103 = t19 * t26
      t108 = sqrt(x4)
      t109 = -0.1D1 + t108
      t110 = t108 + 0.1D1
      t111 = t109 * t110
      t112 = KAPPA2(x1, x2, 0.0D0, -t111, z)
      t113 = s * t112
      t121 = t112 ** 2
      t127 = t121 ** 2
      t131 = Sqrt(-t111)
      t132 = t131 ** 2
      t134 = 0.1D1 / (-0.2D1 + t112) * t132 * t59
      t138 = FJET(XB1, XB2, s, 0.0D0, t113 * t4, t113 * t3 * t6 * t109 *
     # t110, -t113 * t7 * x4, s * t121 * t11 * t17 * (-0.1D1 + x4), 0.8D
     #1 * t86 * t26 * t127 * t134)
      rrqg2qght14s1em1 = t65 * t64 + 0.8D1 * t99 * t15 * t101 * t103 * t
     #87 * t95 + 0.8D1 * t138 * t15 * t101 * t103 * t127 * t134

      end function



      doubleprecision function rrqg2qght14s1em2
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
      t3 = -0.1D1 + z
      t6 = -0.1D1 + x1
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = 0.1D1 / z
      t19 = t11 * t3
      t20 = t9 ** 2
      t23 = 0.1D1 / (-0.2D1 + t1)
      t25 = cos(x2 * pi)
      t26 = t25 ** 2
      t31 = FJET(XB1, XB2, s, 0.0D0, t2 * t3 * x1, -t2 * t3 * t6, 0.0D0,
     # -s * t9 * t11 * x1 * t6, -0.8D1 * t15 * wd * x1 * t6 * t19 * t20 
     #* t23 * t26)
      rrqg2qght14s1em2 = -0.8D1 * t31 * t15 * wd * x1 * t6 * t19 * t20 *
     # t23 * t26

      end function



      doubleprecision function rrqg2qght14s1em3
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
      rrqg2qght14s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght14s1em4
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
      rrqg2qght14s1em4 = 0.0D0

      end function
