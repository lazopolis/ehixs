  
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
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q22q1q2h11J1
      doubleprecision rrq1q22q1q2h11J2
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = 0.3141592653589793D1 * t3
      t16 = 0.1D1 / s
      t17 = x2 * 0.3141592653589793D1
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
      t37 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2,
     #0.0D0, 0.10
     #D1)
      t38 = t36 * t37
      t40 = t33 ** 2
      t42 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2,
     #0.0D0, 0.10
     #D1)
      t43 = t36 * t42
      t50 = 0.3141592653589793D1 * lh
      t51 = t3 * t16
      t52 = t9 * t36
      t53 = t52 * t37
      t59 = 0.3141592653589793D1 ** 2
      t61 = lh ** 2
      t63 = -0.30D2 * t59 + 0.180D3 * t61
      t64 = 0.3141592653589793D1 * t63
      t65 = t64 * t3
      t67 = t16 * t9 * t43
      t68 = t65 * t67
      t70 = 0.1D1 / x4
      t75 = log(0.4D1 * t24 * t27 * t28)
      t76 = t75 * 0.3141592653589793D1
      t79 = t75 ** 2
      t80 = t79 * 0.3141592653589793D1
      t103 = x3 * t19
      t104 = t21 * t23
      t105 = t103 * t104
      t108 = log(0.4D1 * t105 * t30)
      t115 = t50 * t3
      t119 = 0.1D1 / x3
      t123 = t23 * t25
      t128 = log(0.4D1 * t103 * t21 * t123 * t26 * t28)
      t129 = t128 * t9
      t131 = t128 ** 2
      t147 = -(0.90D2 * t15 * t16 * (t34 * t38 - t40 * t9 * t43 / 0.2D1)
     # - 0.180D3 * t50 * t51 * (-t53 + t34 * t43) - t68) * t70 / 0.720D3
     # + (t64 + 0.180D3 * t76 * lh + 0.45D2 * t80) * t3 * t16 * t53 / 0.
     #720D3 + (0.3141592653589793D1 * (-0.2884936567583026D3 - 0.120D3 *
     # t61 * lh + 0.60D2 * lh * t59) - t76 * t63 - 0.90D2 * t80 * lh - 0
     #.15D2 * t79 * t75 * 0.3141592653589793D1) * t3 * t16 * t52 * t42 /
     # 0.720D3 + (0.90D2 * t15 * t16 * (t53 - t108 * t9 * t43) - 0.180D3
     # * t115 * t67) * t119 * t70 / 0.720D3 - (0.90D2 * t15 * t16 * (t12
     #9 * t38 - t131 * t9 * t43 / 0.2D1) - 0.180D3 * t50 * t51 * (-t53 +
     # t129 * t43) - t68) * t119 / 0.720D3
      t148 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t7, 0.0D0, -s * t9 
     #* t11 * x1 * t6, t147)
      t150 = 0.1D1 - x4
      t151 = KAPPA2(x1, x2, 0.0D0, t150, z)
      t152 = s * t151
      t154 = -t150
      t155 = t7 * t154
      t157 = t7 * x4
      t159 = t151 ** 2
      t162 = x1 * t6
      t167 = t159 ** 2
      t172 = log(-0.4D1 * t22 * t123 * t26 * x4 * t154 * t167)
      t173 = t172 * t159
      t175 = 0.1D1 / (-0.2D1 + t151)
      t176 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh,
     #wd, nf, x1, x2, 0.0D0, t15
     #0)
      t179 = t172 ** 2
      t181 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd,
     #nf, x1, x2, 0.0D0, t15
     #0)
      t182 = t175 * t181
      t190 = t159 * t175 * t176
      t197 = t16 * t159 * t182
      t201 = x4 * t154
      t206 = log(-0.4D1 * t105 * t27 * t201 * t167)
      t219 = -(0.90D2 * t15 * t16 * (-t173 * t175 * t176 + t179 * t159 *
     # t182 / 0.2D1) - 0.180D3 * t50 * t51 * (t190 - t173 * t182) + t65 
     #* t197) * t70 / 0.720D3 + (0.90D2 * t15 * t16 * (-t190 + t206 * t1
     #59 * t182) + 0.180D3 * t115 * t197) * t119 * t70 / 0.720D3
      t220 = FJET(XB1, XB2, s, 0.0D0, t152 * t4, t152 * t155, -t152 * t1
     #57, s * t159 * t11 * t162 * t154, t219)
      t222 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t223 = s * t222
      t224 = t4 * x3
      t226 = -0.1D1 + x3
      t227 = t4 * t226
      t230 = t222 ** 2
      t236 = 0.1D1 / (-0.2D1 + t222)
      t238 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh, wd,
     #nf, x1, x2, x3, 0.10D1
     #)
      t239 = t230 * t236 * t238
      t241 = t230 ** 2
      t246 = log(-0.4D1 * t105 * t27 * t226 * x4 * t241)
      t248 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf,
     #x1, x2, x3, 0.10D1
     #)
      t249 = t236 * t248
      t256 = t16 * t230 * t249
      t266 = log(-0.4D1 * t105 * t27 * t226 * t241)
      t267 = t266 * t230
      t270 = t266 ** 2
      t287 = (0.90D2 * t15 * t16 * (-t239 + t246 * t230 * t249) + 0.180D
     #3 * t115 * t256) * t119 * t70 / 0.720D3 - (0.90D2 * t15 * t16 * (-
     #t267 * t236 * t238 + t270 * t230 * t249 / 0.2D1) - 0.180D3 * t50 *
     # t51 * (t239 - t267 * t249) + t65 * t256) * t119 / 0.720D3
      t288 = FJET(XB1, XB2, s, t223 * t224, -t223 * t227, -t223 * t7, 0.
     #0D0, s * t230 * t11 * t162 * t226, t287)
      t290 = KAPPA2(x1, x2, x3, t150, z)
      t291 = s * t290
      t296 = t290 ** 2
      t301 = cos(t17)
      t304 = Sqrt(x3 * t226 * t201)
      t311 = 0.1D1 / (-0.2D1 + t290)
      t313 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh, wd, nf,
     #x1, x2, x3, t150)
      t318 = t296 ** 2
      t323 = log(0.4D1 * t103 * t104 * t25 * t26 * t226 * t201 * t318)
      t325 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, x1,
     #x2, x3, t150)
      t326 = t311 * t325
      t336 = 0.90D2 * t15 * t16 * (t296 * t311 * t313 - t323 * t296 * t3
     #26) - 0.180D3 * t115 * t16 * t296 * t326
      t340 = FJET(XB1, XB2, s, t291 * t224, -t291 * t227, t291 * t155, -
     #t291 * t157, s * t296 * t11 * t162 * (x3 - 0.1D1 + x4 - 0.2D1 * x3
     # * x4 + 0.2D1 * t301 * t304), t336 * t119 * t70 / 0.720D3)
      rrq1q22q1q2ht1s1e1 = t148 * t147 + t220 * t219
     #+ t288 * t287 + t340 *
     #t336 * t119 * t70 / 0.720D3

      end function



      doubleprecision function rrq1q22q1q2ht1s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q22q1q2h11J1
      doubleprecision rrq1q22q1q2h11J2
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t15 = 0.3141592653589793D1 * t3
      t16 = 0.1D1 / s
      t18 = 0.1D1 / (-0.2D1 + t1)
      t19 = t9 * t18
      t20 = rrq1q22q1q2h11J2(s, XB1, XB2, z,
     #lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t21 = t19 * t20
      t22 = x2 * 0.3141592653589793D1
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
      t40 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh,
     #wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t41 = t18 * t40
      t47 = 0.3141592653589793D1 * lh
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
      t85 = t84 * 0.3141592653589793D1
      t92 = 0.3141592653589793D1 ** 2
      t94 = lh ** 2
      t100 = t84 ** 2
      t109 = -(0.90D2 * t15 * t16 * (-t21 + t38 * t9 * t41) + t52) * t54
     # / 0.720D3 + t15 * t49 * t41 * t59 / 0.8D1 - (0.90D2 * t15 * t16 *
     # (-t21 + t70 * t9 * t41) + t52) * t58 / 0.720D3 + (-0.180D3 * t47 
     #- 0.90D2 * t85) * t3 * t16 * t21 / 0.720D3 + (0.3141592653589793D1
     # * (-0.30D2 * t92 + 0.180D3 * t94) + 0.180D3 * t85 * lh + 0.45D2 *
     # t100 * 0.3141592653589793D1) * t3 * t16 * t19 * t40 / 0.720D3
      t110 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t7, 0.0D0, -s * t9 
     #* t11 * x1 * t6, t109)
      t112 = 0.1D1 - x4
      t113 = KAPPA2(x1, x2, 0.0D0, t112, z)
      t114 = s * t113
      t116 = -t112
      t117 = t7 * t116
      t119 = t7 * x4
      t121 = t113 ** 2
      t124 = x1 * t6
      t128 = 0.1D1 / (-0.2D1 + t113)
      t130 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh, wd, nf,
     #x1, x2, 0.0D0, t11
     #2)
      t134 = t121 ** 2
      t139 = log(-0.4D1 * t27 * t65 * t31 * x4 * t116 * t134)
      t141 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, x1,
     #x2, 0.0D0, t11
     #2)
      t142 = t128 * t141
      t148 = t16 * t121
      t159 = -(0.90D2 * t15 * t16 * (t121 * t128 * t130 - t139 * t121 * 
     #t142) - 0.180D3 * t48 * t148 * t142) * t54 / 0.720D3 - t15 * t148 
     #* t142 * t59 / 0.8D1
      t160 = FJET(XB1, XB2, s, 0.0D0, t114 * t4, t114 * t117, -t114 * t1
     #19, s * t121 * t11 * t124 * t116, t159)
      t162 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t163 = s * t162
      t164 = t4 * x3
      t166 = -0.1D1 + x3
      t167 = t4 * t166
      t170 = t162 ** 2
      t175 = t16 * t170
      t178 = 0.1D1 / (-0.2D1 + t162)
      t179 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, x1,
     #x2, x3, 0.10D1
     #)
      t180 = t178 * t179
      t185 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh, wd, nf, x1,
     #x2, x3, 0.10D1
     #)
      t189 = t170 ** 2
      t194 = log(-0.4D1 * t63 * t26 * t28 * t32 * t166 * t189)
      t207 = -t15 * t175 * t180 * t59 / 0.8D1 - (0.90D2 * t15 * t16 * (t
     #170 * t178 * t185 - t194 * t170 * t180) - 0.180D3 * t48 * t175 * t
     #180) * t58 / 0.720D3
      t208 = FJET(XB1, XB2, s, t163 * t164, -t163 * t167, -t163 * t7, 0.
     #0D0, s * t170 * t11 * t124 * t166, t207)
      t210 = KAPPA2(x1, x2, x3, t112, z)
      t211 = s * t210
      t216 = t210 ** 2
      t221 = cos(t22)
      t225 = Sqrt(x3 * t166 * x4 * t116)
      t234 = 0.1D1 / (-0.2D1 + t210)
      t235 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, x1,
     #x2, x3, t112)
      t240 = FJET(XB1, XB2, s, t211 * t164, -t211 * t167, t211 * t117, -
     #t211 * t119, s * t216 * t11 * t124 * (x3 - 0.1D1 + x4 - 0.2D1 * x3
     # * x4 + 0.2D1 * t221 * t225), t15 * t16 * t216 * t234 * t235 * t59
     # / 0.8D1)
      rrq1q22q1q2ht1s1e0 = t110 * t109 + t160 * t159
     #+ t208 * t207 + t240 *
     #0.3141592653589793D1 * t3 * t16 * t216 * t234 * t235 *
     #t58 * t54 /
     # 0.8D1

      end function



      doubleprecision function rrq1q22q1q2ht1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q22q1q2h11J1
      doubleprecision rrq1q22q1q2h11J2
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t16 = 0.1D1 / s
      t17 = 0.3141592653589793D1 * t3 * t16
      t20 = t9 / (-0.2D1 + t1)
      t21 = rrq1q22q1q2h11J1(s, XB1, XB2,
     #z, lh, wd, nf, x1, x2, 0.0D0, 0.10
     #D1)
      t22 = 0.1D1 / x3
      t27 = rrq1q22q1q2h11J2(s, XB1, XB2, z, lh, wd,
     #nf, x1, x2, 0.0D0, 0.10
     #D1)
      t34 = sin(x2 * 0.3141592653589793D1)
      t35 = t34 ** 2
      t36 = z ** 2
      t39 = t11 ** 2
      t41 = x1 ** 2
      t42 = t6 ** 2
      t44 = t9 ** 2
      t48 = log(0.4D1 * t35 / t36 * t39 * t41 * t42 * t44)
      t57 = 0.1D1 / x4
      t62 = t17 * t20 * t21 * t22 / 0.8D1 + t17 * t20 * t27 / 0.8D1 + (-
     #0.180D3 * 0.3141592653589793D1 * lh - 0.90D2 * t48 * 0.31415926535
     #89793D1) * t3 * t16 * t20 * t21 / 0.720D3 + t17 * t20 * t21 * t57 
     #/ 0.8D1
      t63 = FJET(XB1, XB2, s, 0.0D0, t2 * t4, -t2 * t7, 0.0D0, -s * t9 *
     # t11 * x1 * t6, t62)
      t65 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t66 = s * t65
      t69 = -0.1D1 + x3
      t73 = t65 ** 2
      t76 = x1 * t6
      t82 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2,
     #x3, 0.10D1)
      t84 = t73 / (-0.2D1 + t65) * t82 * t22
      t87 = FJET(XB1, XB2, s, t66 * t4 * x3, -t66 * t4 * t69, -t66 * t7,
     # 0.0D0, s * t73 * t11 * t76 * t69, -t17 * t84 / 0.8D1)
      t89 = t3 * t16
      t93 = 0.1D1 - x4
      t94 = KAPPA2(x1, x2, 0.0D0, t93, z)
      t95 = s * t94
      t97 = -t93
      t102 = t94 ** 2
      t110 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2,
     #0.0D0, t93
     #)
      t112 = t102 / (-0.2D1 + t94) * t110 * t57
      t115 = FJET(XB1, XB2, s, 0.0D0, t95 * t4, t95 * t7 * t97, -t95 * t
     #7 * x4, s * t102 * t11 * t76 * t97, -t17 * t112 / 0.8D1)
      rrq1q22q1q2ht1s1em1 = t63 * t62 - t87 *
     #0.3141592653589793D1 * t89 * t
     #84 / 0.8D1 - t115 * 0.3141592653589793D1 * t89 * t112 / 0.8D1

      end function



      doubleprecision function rrq1q22q1q2ht1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q22q1q2h11J1
      doubleprecision rrq1q22q1q2h11J2
      t1 = KAPPA2(x1, x2, 0.0D0, 0.10D1, z)
      t2 = s * t1
      t3 = z - 0.1D1
      t6 = -0.1D1 + x1
      t9 = t1 ** 2
      t11 = t3 ** 2
      t16 = 0.1D1 / s
      t19 = 0.1D1 / (-0.2D1 + t1)
      t21 = rrq1q22q1q2h11J1(s, XB1, XB2, z, lh, wd,
     #nf, x1, x2, 0.0D0, 0.10
     #D1)
      t25 = FJET(XB1, XB2, s, 0.0D0, t2 * t3 * x1, -t2 * t3 * t6, 0.0D0,
     # -s * t9 * t11 * x1 * t6, 0.3141592653589793D1 * t3 * t16 * t9 * t
     #19 * t21 / 0.8D1)
      rrq1q22q1q2ht1s1em2 = t25 * 0.3141592653589793D1 * t3 *
     #t16 * t9 * t19
     # * t21 / 0.8D1

      end function



      doubleprecision function rrq1q22q1q2ht1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q22q1q2h11J1
      doubleprecision rrq1q22q1q2h11J2
      rrq1q22q1q2ht1s1em3 = 0.0D0

      end function



      doubleprecision function rrq1q22q1q2ht1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrq1q22q1q2h11J1
      doubleprecision rrq1q22q1q2h11J2
      rrq1q22q1q2ht1s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrq1q22q1q2h11J1
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
      t6 = (0.1D1 - z) ** 2
      t7 = t4 * t6
      t9 = 0.1D1 - x1
      t10 = 0.1D1 - x4
      t15 = x1 ** 2
      t17 = 0.1D1 - x3
      t18 = t17 ** 2
      t21 = t3 ** 2
      t23 = t6 ** 2
      t25 = t9 ** 2
      t30 = cos(x2 * 0.3141592653589793D1)
      t34 = Sqrt(x3 * t17 * x4 * t10)
      t37 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t30 * t34
      t38 = t37 ** 2
      t42 = x4 ** 2
      rrq1q22q1q2h11J1 = -0.32D2 / 0.9D1 * wd *
     #(-0.2D1 * t7 * x1 * x3 * t9
     #* t10 - t4 * t6 * t15 * t18 - t1 - t1 * t21 * t23 * t15 * t25 * t3
     #8 - t4 * t6 * t25 * t42 + 0.2D1 * t7 * x1 * t9 * t37 + 0.2D1 * t7 
     #* t9 * x4 * x1 * t17) / s / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrq1q22q1q2h11J2
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
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t9 = t1 * t3 * t2 * t7 * t6
      t10 = x1 ** 2
      t11 = 0.1D1 - x3
      t13 = 0.1D1 - x1
      t17 = cos(x2 * 0.3141592653589793D1)
      t22 = Sqrt(x3 * t11 * x4 * (0.1D1 - x4))
      t25 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t17 * t22
      t30 = t1 * t2
      t35 = t1 * t3
      t36 = t35 * t7
      t46 = t13 ** 2
      t56 = t3 ** 2
      t58 = t7 ** 2
      t61 = t25 ** 2
      t65 = t11 ** 2
      t69 = x4 ** 2
      rrq1q22q1q2h11J2 = -0.32D2 / 0.9D1 * wd *
     #(-0.2D1 * t9 * t10 * t11 * t
     #13 * t25 - 0.2D1 * t30 * t6 * x1 * t11 + 0.2D1 * t36 * x1 * t13 * 
     #t25 + 0.2D1 * t36 * t13 * x4 * x1 * t11 - 0.2D1 * t9 * t46 * x4 * 
     #x1 * t25 - 0.2D1 * t30 * t6 * t13 * x4 + t1 * t56 * t58 * t10 * t4
     #6 * t61 + t35 * t7 * t10 * t65 + t35 * t7 * t46 * t69 + t1) / s / 
     #z / 0.3141592653589793D1

      end function
  
 