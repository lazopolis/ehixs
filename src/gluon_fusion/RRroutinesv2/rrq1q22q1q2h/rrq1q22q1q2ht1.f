  
      subroutine rrq1q22q1q2ht1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrq1q22q1q2h11J1  
      doubleprecision rrq1q22q1q2h11J2  
      doubleprecision rrq1q22q1q2ht1s1e1  
      doubleprecision rrq1q22q1q2ht1s1e0  
      doubleprecision rrq1q22q1q2ht1s1em1  
      doubleprecision rrq1q22q1q2ht1s1em2  
      doubleprecision rrq1q22q1q2ht1s1em3  
      doubleprecision rrq1q22q1q2ht1s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrq1q22q1q2ht1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrq1q22q1q2ht1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrq1q22q1q2ht1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrq1q22q1q2ht1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrq1q22q1q2ht1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrq1q22q1q2ht1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrq1q22q1q2ht1s1e1
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
      doubleprecision rrq1q22q1q2h11J1
      doubleprecision rrq1q22q1q2h11J2

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
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = pi * t3
      t16 = 0.1D1 / s
      t17 = x2 * pi
      t18 = sin(t17)
      t19 = t18 ** 2
      t20 = z ** 2
      t21 = 0.1D1 / t20
      t22 = t19 * t21
      t23 = t11 ** 2
      t24 = t22 * t23
      t25 = x1 ** 2
      t26 = t6 ** 2
      t27 = t25 * t26
      t28 = t9 ** 2
      t30 = t27 * x4 * t28
      t33 = log(0.4D1 * t24 * t30)
      t34 = t33 * t9
      t36 = 0.1D1 / (-0.2D1 + t1)
      t37 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, 
     #t5, 0.0D0, -t14)
      t38 = t36 * t37
      t40 = t33 ** 2
      t42 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, 
     #t5, 0.0D0, -t14)
      t43 = t36 * t42
      t50 = pi * lh
      t51 = t3 * t16
      t52 = t9 * t36
      t53 = t52 * t37
      t59 = pi ** 2
      t61 = lh ** 2
      t63 = -0.30D2 * t59 + 0.180D3 * t61
      t64 = pi * t63
      t65 = t64 * t3
      t67 = t16 * t9 * t43
      t68 = t65 * t67
      t70 = 0.1D1 / x4
      t75 = log(0.4D1 * t24 * t27 * t28)
      t76 = t75 * pi
      t79 = t75 ** 2
      t80 = t79 * pi
      t104 = x3 * t19
      t105 = t21 * t23
      t106 = t104 * t105
      t109 = log(0.4D1 * t106 * t30)
      t116 = t50 * t3
      t120 = 0.1D1 / x3
      t124 = t23 * t25
      t129 = log(0.4D1 * t104 * t21 * t124 * t26 * t28)
      t130 = t129 * t9
      t132 = t129 ** 2
      t148 = -(0.90D2 * t15 * t16 * (t34 * t38 - t40 * t9 * t43 / 0.2D1)
     # - 0.180D3 * t50 * t51 * (-t53 + t34 * t43) - t68) * t70 / 0.720D3
     # + (t64 + 0.180D3 * t76 * lh + 0.45D2 * t80) * t3 * t16 * t53 / 0.
     #720D3 + (pi * (-0.240D3 * zeta3 - 0.120D3 * t61 * lh + 0.60D2 * lh
     # * t59) - t76 * t63 - 0.90D2 * t80 * lh - 0.15D2 * t79 * t75 * pi)
     # * t3 * t16 * t52 * t42 / 0.720D3 + (0.90D2 * t15 * t16 * (t53 - t
     #109 * t9 * t43) - 0.180D3 * t116 * t67) * t120 * t70 / 0.720D3 + (
     #0.90D2 * t15 * t16 * (-t130 * t38 + t132 * t9 * t43 / 0.2D1) - 0.1
     #80D3 * t50 * t51 * (t53 - t130 * t43) + t68) * t120 / 0.720D3
      t149 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t148)
      t151 = 0.1D1 - x4
      t152 = KAPPA2(x1, x2, 0.0D0, t151, z)
      t153 = s * t152
      t154 = t153 * t4
      t155 = -t151
      t156 = t7 * t155
      t157 = t153 * t156
      t158 = t7 * x4
      t159 = t153 * t158
      t160 = t152 ** 2
      t163 = x1 * t6
      t165 = s * t160 * t11 * t163 * t155
      t168 = t160 ** 2
      t173 = log(-0.4D1 * t22 * t124 * t26 * x4 * t155 * t168)
      t174 = t173 * t160
      t176 = 0.1D1 / (-0.2D1 + t152)
      t177 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t157
     #, t154, -t159, t165)
      t180 = t173 ** 2
      t182 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t157
     #, t154, -t159, t165)
      t183 = t176 * t182
      t191 = t160 * t176 * t177
      t198 = t16 * t160 * t183
      t202 = x4 * t155
      t207 = log(-0.4D1 * t106 * t27 * t202 * t168)
      t220 = -(0.90D2 * t15 * t16 * (-t174 * t176 * t177 + t180 * t160 *
     # t183 / 0.2D1) - 0.180D3 * t50 * t51 * (t191 - t174 * t183) + t65 
     #* t198) * t70 / 0.720D3 + (0.90D2 * t15 * t16 * (-t191 + t207 * t1
     #60 * t183) + 0.180D3 * t116 * t198) * t120 * t70 / 0.720D3
      t221 = FJET(XB1, XB2, s, 0.0D0, t154, t157, -t159, t165, t220)
      t223 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t224 = s * t223
      t225 = t4 * x3
      t226 = t224 * t225
      t227 = -0.1D1 + x3
      t228 = t4 * t227
      t229 = t224 * t228
      t230 = t224 * t7
      t231 = t223 ** 2
      t235 = s * t231 * t11 * t163 * t227
      t237 = 0.1D1 / (-0.2D1 + t223)
      t239 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh, wd, nf, s, t226, -t230
     #, -t229, 0.0D0, t235)
      t240 = t231 * t237 * t239
      t242 = t231 ** 2
      t247 = log(-0.4D1 * t106 * t27 * t227 * x4 * t242)
      t249 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, s, t226, -t230
     #, -t229, 0.0D0, t235)
      t250 = t237 * t249
      t257 = t16 * t231 * t250
      t267 = log(-0.4D1 * t106 * t27 * t227 * t242)
      t268 = t267 * t231
      t271 = t267 ** 2
      t288 = (0.90D2 * t15 * t16 * (-t240 + t247 * t231 * t250) + 0.180D
     #3 * t116 * t257) * t120 * t70 / 0.720D3 + (0.90D2 * t15 * t16 * (t
     #268 * t237 * t239 - t271 * t231 * t250 / 0.2D1) - 0.180D3 * t50 * 
     #t51 * (-t240 + t268 * t250) - t65 * t257) * t120 / 0.720D3
      t289 = FJET(XB1, XB2, s, t226, -t229, -t230, 0.0D0, t235, t288)
      t291 = KAPPA2(x1, x2, x3, t151, z)
      t292 = s * t291
      t293 = t292 * t225
      t294 = t292 * t228
      t295 = t292 * t156
      t296 = t292 * t158
      t297 = t291 ** 2
      t302 = cos(t17)
      t305 = Sqrt(x3 * t227 * t202)
      t310 = s * t297 * t11 * t163 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 * x4 
     #+ 0.2D1 * t302 * t305)
      t312 = 0.1D1 / (-0.2D1 + t291)
      t314 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh, wd, nf, s, t293, t295,
     # -t294, -t296, t310)
      t319 = t297 ** 2
      t324 = log(0.4D1 * t104 * t105 * t25 * t26 * t227 * t202 * t319)
      t326 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, s, t293, t295,
     # -t294, -t296, t310)
      t327 = t312 * t326
      t337 = 0.90D2 * t15 * t16 * (t297 * t312 * t314 - t324 * t297 * t3
     #27) - 0.180D3 * t116 * t16 * t297 * t327
      t341 = FJET(XB1, XB2, s, t293, -t294, t295, -t296, t310, t337 * t1
     #20 * t70 / 0.720D3)
      rrq1q22q1q2ht1s1e1 = t149 * t148 + t221 * t220 + t289 * t288 + t34
     #1 * t337 * t120 * t70 / 0.720D3

      end function



      doubleprecision function rrq1q22q1q2ht1s1e0
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
      doubleprecision rrq1q22q1q2h11J1
      doubleprecision rrq1q22q1q2h11J2

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
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = pi * t3
      t16 = 0.1D1 / s
      t18 = 0.1D1 / (-0.2D1 + t1)
      t19 = t9 * t18
      t20 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, 
     #t5, 0.0D0, -t14)
      t21 = t19 * t20
      t22 = x2 * pi
      t23 = sin(t22)
      t24 = t23 ** 2
      t25 = z ** 2
      t26 = 0.1D1 / t25
      t27 = t24 * t26
      t28 = t11 ** 2
      t29 = t27 * t28
      t30 = x1 ** 2
      t31 = t6 ** 2
      t32 = t30 * t31
      t33 = t9 ** 2
      t38 = log(0.4D1 * t29 * t32 * x4 * t33)
      t40 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, 
     #t5, 0.0D0, -t14)
      t41 = t18 * t40
      t47 = pi * lh
      t48 = t47 * t3
      t49 = t16 * t9
      t52 = 0.180D3 * t48 * t49 * t41
      t54 = 0.1D1 / x4
      t58 = 0.1D1 / x3
      t59 = t58 * t54
      t63 = x3 * t24
      t65 = t28 * t30
      t70 = log(0.4D1 * t63 * t26 * t65 * t31 * t33)
      t84 = log(0.4D1 * t29 * t32 * t33)
      t85 = t84 * pi
      t92 = pi ** 2
      t94 = lh ** 2
      t100 = t84 ** 2
      t109 = -(0.90D2 * t15 * t16 * (-t21 + t38 * t9 * t41) + t52) * t54
     # / 0.720D3 + t15 * t49 * t41 * t59 / 0.8D1 + (0.90D2 * t15 * t16 *
     # (t21 - t70 * t9 * t41) - t52) * t58 / 0.720D3 + (-0.180D3 * t47 -
     # 0.90D2 * t85) * t3 * t16 * t21 / 0.720D3 + (pi * (-0.30D2 * t92 +
     # 0.180D3 * t94) + 0.180D3 * t85 * lh + 0.45D2 * t100 * pi) * t3 * 
     #t16 * t19 * t40 / 0.720D3
      t110 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t109)
      t112 = 0.1D1 - x4
      t113 = KAPPA2(x1, x2, 0.0D0, t112, z)
      t114 = s * t113
      t115 = t114 * t4
      t116 = -t112
      t117 = t7 * t116
      t118 = t114 * t117
      t119 = t7 * x4
      t120 = t114 * t119
      t121 = t113 ** 2
      t124 = x1 * t6
      t126 = s * t121 * t11 * t124 * t116
      t128 = 0.1D1 / (-0.2D1 + t113)
      t130 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t118
     #, t115, -t120, t126)
      t134 = t121 ** 2
      t139 = log(-0.4D1 * t27 * t65 * t31 * x4 * t116 * t134)
      t141 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t118
     #, t115, -t120, t126)
      t142 = t128 * t141
      t148 = t16 * t121
      t159 = -(0.90D2 * t15 * t16 * (t121 * t128 * t130 - t139 * t121 * 
     #t142) - 0.180D3 * t48 * t148 * t142) * t54 / 0.720D3 - t15 * t148 
     #* t142 * t59 / 0.8D1
      t160 = FJET(XB1, XB2, s, 0.0D0, t115, t118, -t120, t126, t159)
      t162 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t163 = s * t162
      t164 = t4 * x3
      t165 = t163 * t164
      t166 = -0.1D1 + x3
      t167 = t4 * t166
      t168 = t163 * t167
      t169 = t163 * t7
      t170 = t162 ** 2
      t174 = s * t170 * t11 * t124 * t166
      t175 = t16 * t170
      t178 = 0.1D1 / (-0.2D1 + t162)
      t179 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, s, t165, -t169
     #, -t168, 0.0D0, t174)
      t180 = t178 * t179
      t185 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh, wd, nf, s, t165, -t169
     #, -t168, 0.0D0, t174)
      t189 = t170 ** 2
      t194 = log(-0.4D1 * t63 * t26 * t28 * t32 * t166 * t189)
      t207 = -t15 * t175 * t180 * t59 / 0.8D1 + (0.90D2 * t15 * t16 * (-
     #t170 * t178 * t185 + t194 * t170 * t180) + 0.180D3 * t48 * t175 * 
     #t180) * t58 / 0.720D3
      t208 = FJET(XB1, XB2, s, t165, -t168, -t169, 0.0D0, t174, t207)
      t210 = KAPPA2(x1, x2, x3, t112, z)
      t211 = s * t210
      t212 = t211 * t164
      t213 = t211 * t167
      t214 = t211 * t117
      t215 = t211 * t119
      t216 = t210 ** 2
      t221 = cos(t22)
      t225 = Sqrt(x3 * t166 * x4 * t116)
      t230 = s * t216 * t11 * t124 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 * x4 
     #+ 0.2D1 * t221 * t225)
      t234 = 0.1D1 / (-0.2D1 + t210)
      t235 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, s, t212, t214,
     # -t213, -t215, t230)
      t240 = FJET(XB1, XB2, s, t212, -t213, t214, -t215, t230, t15 * t16
     # * t216 * t234 * t235 * t59 / 0.8D1)
      rrq1q22q1q2ht1s1e0 = t110 * t109 + t160 * t159 + t208 * t207 + t24
     #0 * pi * t3 * t16 * t216 * t234 * t235 * t58 * t54 / 0.8D1

      end function



      doubleprecision function rrq1q22q1q2ht1s1em1
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
      doubleprecision rrq1q22q1q2h11J1
      doubleprecision rrq1q22q1q2h11J2

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
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t16 = 0.1D1 / s
      t17 = pi * t3 * t16
      t20 = t9 / (-0.2D1 + t1)
      t21 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, 
     #t5, 0.0D0, -t14)
      t22 = 0.1D1 / x3
      t27 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, 
     #t5, 0.0D0, -t14)
      t34 = sin(x2 * pi)
      t35 = t34 ** 2
      t36 = z ** 2
      t39 = t11 ** 2
      t41 = x1 ** 2
      t42 = t6 ** 2
      t44 = t9 ** 2
      t48 = log(0.4D1 * t35 / t36 * t39 * t41 * t42 * t44)
      t57 = 0.1D1 / x4
      t62 = t17 * t20 * t21 * t22 / 0.8D1 + t17 * t20 * t27 / 0.8D1 + (-
     #0.180D3 * pi * lh - 0.90D2 * t48 * pi) * t3 * t16 * t20 * t21 / 0.
     #720D3 + t17 * t20 * t21 * t57 / 0.8D1
      t63 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t62)
      t65 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t66 = s * t65
      t68 = t66 * t4 * x3
      t69 = -0.1D1 + x3
      t71 = t66 * t4 * t69
      t72 = t66 * t7
      t73 = t65 ** 2
      t76 = x1 * t6
      t78 = s * t73 * t11 * t76 * t69
      t82 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, s, t68, -t72, -
     #t71, 0.0D0, t78)
      t84 = t73 / (-0.2D1 + t65) * t82 * t22
      t87 = FJET(XB1, XB2, s, t68, -t71, -t72, 0.0D0, t78, -t17 * t84 / 
     #0.8D1)
      t89 = t3 * t16
      t93 = 0.1D1 - x4
      t94 = KAPPA2(x1, x2, 0.0D0, t93, z)
      t95 = s * t94
      t96 = t95 * t4
      t97 = -t93
      t99 = t95 * t7 * t97
      t101 = t95 * t7 * x4
      t102 = t94 ** 2
      t106 = s * t102 * t11 * t76 * t97
      t110 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t99,
     # t96, -t101, t106)
      t112 = t102 / (-0.2D1 + t94) * t110 * t57
      t115 = FJET(XB1, XB2, s, 0.0D0, t96, t99, -t101, t106, -t17 * t112
     # / 0.8D1)
      rrq1q22q1q2ht1s1em1 = t63 * t62 - t87 * pi * t89 * t84 / 0.8D1 - t
     #115 * pi * t89 * t112 / 0.8D1

      end function



      doubleprecision function rrq1q22q1q2ht1s1em2
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
      doubleprecision rrq1q22q1q2h11J1
      doubleprecision rrq1q22q1q2h11J2

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
      t5 = t2 * t3 * x1
      t6 = -0.1D1 + x1
      t8 = t2 * t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t16 = 0.1D1 / s
      t19 = 0.1D1 / (-0.2D1 + t1)
      t21 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, 
     #t5, 0.0D0, -t14)
      t25 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, pi * t3 * t16
     # * t9 * t19 * t21 / 0.8D1)
      rrq1q22q1q2ht1s1em2 = t25 * pi * t3 * t16 * t9 * t19 * t21 / 0.8D1

      end function



      doubleprecision function rrq1q22q1q2ht1s1em3
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
      doubleprecision rrq1q22q1q2h11J1
      doubleprecision rrq1q22q1q2h11J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrq1q22q1q2ht1s1em3 = 0.0D0

      end function



      doubleprecision function rrq1q22q1q2ht1s1em4
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
      doubleprecision rrq1q22q1q2h11J1
      doubleprecision rrq1q22q1q2h11J2

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrq1q22q1q2ht1s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrq1q22q1q2h11J1
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
      t5 = S14 ** 2
      t7 = S23 ** 2
      t11 = S34 ** 2
      rrq1q22q1q2h11J1 = (0.32D2 / 0.9D1 * S12 - 0.64D2 / 0.9D1 * S34 + 
     #(0.64D2 / 0.9D1 * S13 * S24 + 0.32D2 / 0.9D1 * t5 + 0.32D2 / 0.9D1
     # * t7 - 0.64D2 / 0.9D1 * S14 * S23 + 0.32D2 / 0.9D1 * t11) / S12) 
     #/ pi * wd / z

      end function
  
   
 

      doubleprecision function rrq1q22q1q2h11J2
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
      t5 = S34 ** 2
      t12 = S23 ** 2
      t14 = S14 ** 2
      rrq1q22q1q2h11J2 = (-0.32D2 / 0.9D1 * S12 - 0.64D2 / 0.9D1 * S14 -
     # 0.64D2 / 0.9D1 * S34 - 0.64D2 / 0.9D1 * S23 + (-0.32D2 / 0.9D1 * 
     #t5 + (-0.64D2 / 0.9D1 * S14 - 0.64D2 / 0.9D1 * S23) * S34 - 0.64D2
     # / 0.9D1 * S14 * S23 - 0.32D2 / 0.9D1 * t12 - 0.32D2 / 0.9D1 * t14
     #) / S12) / pi * wd / z

      end function
  
 