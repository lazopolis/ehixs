  
      subroutine rrqq2qqht2
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqq2qqht2s1e1  
      doubleprecision rrqq2qqht2s1e0  
      doubleprecision rrqq2qqht2s1em1  
      doubleprecision rrqq2qqht2s1em2  
      doubleprecision rrqq2qqht2s1em3  
      doubleprecision rrqq2qqht2s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht2s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht2s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht2s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht2s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht2s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqq2qqht2s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqq2qqht2s1e1
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
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = t11 * t3
      t16 = t15 * x1
      t17 = 0.1D1 / z
      t18 = t6 * t17
      t19 = t16 * t18
      t20 = x2 * pi
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = wd * t22
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
      t42 = 0.1D1 / (-0.2D1 + t1)
      t48 = lh * t15
      t49 = x1 * t6
      t50 = t49 * t17
      t51 = t48 * t50
      t57 = pi ** 2
      t59 = lh ** 2
      t61 = -0.30D2 * t57 + 0.180D3 * t59
      t62 = t61 * t15
      t64 = t17 * wd
      t66 = t22 * t42 * t34
      t67 = t64 * t66
      t68 = t62 * t49 * t67
      t70 = 0.1D1 / x4
      t81 = log(0.4D1 * t30 * t33 * t34)
      t83 = t81 ** 2
      t93 = x3 * t25
      t98 = log(0.4D1 * t93 * t27 * t29 * t36)
      t103 = t48 * t49
      t107 = 0.1D1 / x3
      t112 = t29 * t31
      t117 = log(0.4D1 * t93 * t27 * t112 * t32 * t34)
      t118 = t117 ** 2
      t132 = 0.8D1 / 0.405D3 * (-0.45D2 * t19 * t23 * t40 * t42 * t34 - 
     #0.180D3 * t51 * t23 * t39 * t42 * t34 - t68) * t70 - 0.8D1 / 0.405
     #D3 * (-0.240D3 * zeta3 - 0.120D3 * t59 * lh + 0.60D2 * lh * t57 - 
     #t81 * t61 - 0.90D2 * t83 * lh - 0.15D2 * t83 * t81) * t15 * t49 * 
     #t67 - 0.2D1 / 0.405D3 * (-0.360D3 * t19 * wd * t98 * t66 - 0.720D3
     # * t103 * t67) * t107 * t70 + 0.8D1 / 0.405D3 * (-0.45D2 * t19 * t
     #23 * t118 * t42 * t34 - 0.180D3 * t51 * t23 * t117 * t42 * t34 - t
     #68) * t107
      t133 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t132)
      t135 = sqrt(x4)
      t136 = -0.1D1 + t135
      t137 = t135 + 0.1D1
      t138 = t136 * t137
      t139 = KAPPA2(x1, x2, 0.0D0, -t138, z)
      t140 = s * t139
      t141 = t140 * t4
      t144 = t6 * t136 * t137
      t145 = t140 * t3 * t144
      t146 = t7 * x4
      t147 = t140 * t146
      t148 = t139 ** 2
      t153 = s * t148 * t11 * t49 * (-0.1D1 + x4)
      t155 = t16 * t18 * wd
      t156 = t32 * t136
      t159 = t148 ** 2
      t165 = log(-0.4D1 * t112 * t156 * t137 * t27 * t25 * t159 * x4)
      t166 = t165 ** 2
      t169 = 0.1D1 / (-0.2D1 + t139)
      t171 = Sqrt(-t138)
      t172 = t171 ** 2
      t173 = t169 * t159 * t172
      t181 = t62 * t50
      t182 = t23 * t173
      t194 = log(-0.4D1 * t112 * t156 * t137 * t28 * t159 * x3 * x4)
      t207 = 0.8D1 / 0.405D3 * (0.45D2 * t155 * t22 * t166 * t173 + 0.18
     #0D3 * t51 * t23 * t165 * t173 + t181 * t182) * t70 - 0.2D1 / 0.405
     #D3 * (0.360D3 * t155 * t194 * t169 * t159 * t22 * t172 + 0.720D3 *
     # t51 * t182) * t107 * t70
      t208 = FJET(XB1, XB2, s, 0.0D0, t141, t145, -t147, t153, t207)
      t210 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t132)
      t212 = FJET(XB1, XB2, s, t141, 0.0D0, -t147, t145, t153, t207)
      t214 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t215 = s * t214
      t216 = t4 * x3
      t217 = t215 * t216
      t219 = sqrt(x3)
      t220 = -0.1D1 + t219
      t222 = t219 + 0.1D1
      t223 = x1 * t220 * t222
      t224 = t215 * t3 * t223
      t225 = t215 * t7
      t226 = t214 ** 2
      t231 = s * t226 * t11 * t49 * (-0.1D1 + x3)
      t232 = t32 * t220
      t235 = x3 * x4
      t236 = t226 ** 2
      t241 = log(-0.4D1 * t112 * t232 * t222 * t28 * t235 * t236)
      t243 = t220 * t222
      t244 = Sqrt(-t243)
      t245 = t244 ** 2
      t249 = t245 / (-0.2D1 + t214) * t236
      t253 = t23 * t249
      t266 = log(-0.4D1 * t112 * t232 * t222 * t27 * t93 * t236)
      t267 = t266 ** 2
      t280 = -0.2D1 / 0.405D3 * (0.360D3 * t155 * t241 * t22 * t249 + 0.
     #720D3 * t51 * t253) * t107 * t70 + 0.8D1 / 0.405D3 * (0.45D2 * t15
     #5 * t22 * t267 * t249 + 0.180D3 * t51 * t23 * t266 * t249 + t181 *
     # t253) * t107
      t281 = FJET(XB1, XB2, s, t217, -t224, -t225, 0.0D0, t231, t280)
      t283 = KAPPA2(x1, x2, x3, -t138, z)
      t284 = s * t283
      t285 = t284 * t216
      t286 = t284 * t3
      t287 = t286 * t223
      t288 = t286 * t144
      t289 = t284 * t146
      t290 = t283 ** 2
      t296 = Sqrt(t243 * t138)
      t302 = s * t290 * t11 * t49 * (x3 - 0.1D1 + x4 - 0.2D1 * t235 + 0.
     #2D1 * t21 * t219 * t135 * t296)
      t304 = t290 ** 2
      t312 = log(0.4D1 * t112 * t32 * t138 * t304 * t243 * t27 * t93 * x
     #4)
      t321 = (-t219 * t135 + 0.2D1 * t21 * t296) ** 2
      t322 = t304 / (-0.2D1 + t283) * t321
      t329 = -0.90D2 * t19 * wd * t312 * t322 - 0.180D3 * t103 * t64 * t
     #322
      t332 = 0.2D1 / 0.405D3 * t329 * t107 * t70
      t333 = FJET(XB1, XB2, s, t285, -t287, t288, -t289, t302, -t332)
      t335 = t107 * t70
      t338 = FJET(XB1, XB2, s, -t224, t217, 0.0D0, -t225, t231, t280)
      t340 = FJET(XB1, XB2, s, -t287, t285, -t289, t288, t302, -t332)
      rrqq2qqht2s1e1 = t133 * t132 + t208 * t207 + t210 * t132 + t212 * 
     #t207 + t281 * t280 - 0.2D1 / 0.405D3 * t333 * t329 * t335 + t338 *
     # t280 - 0.2D1 / 0.405D3 * t340 * t329 * t335

      end function



      doubleprecision function rrqq2qqht2s1e0
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
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = t11 * t3
      t16 = t15 * x1
      t17 = 0.1D1 / z
      t18 = t6 * t17
      t19 = t16 * t18
      t20 = x2 * pi
      t21 = cos(t20)
      t22 = t21 ** 2
      t23 = wd * t22
      t24 = sin(t20)
      t25 = t24 ** 2
      t26 = z ** 2
      t27 = 0.1D1 / t26
      t29 = t11 ** 2
      t30 = t25 * t27 * t29
      t31 = x1 ** 2
      t32 = t6 ** 2
      t33 = t31 * t32
      t34 = t9 ** 2
      t39 = log(0.4D1 * t30 * t33 * x4 * t34)
      t41 = 0.1D1 / (-0.2D1 + t1)
      t47 = lh * t15
      t48 = x1 * t6
      t51 = t41 * t34
      t53 = t17 * wd * t51 * t22
      t55 = 0.180D3 * t47 * t48 * t53
      t57 = 0.1D1 / x4
      t61 = t16 * t18 * wd
      t62 = 0.1D1 / x3
      t68 = x3 * t25
      t70 = t29 * t31
      t75 = log(0.4D1 * t68 * t27 * t70 * t32 * t34)
      t84 = pi ** 2
      t86 = lh ** 2
      t91 = log(0.4D1 * t30 * t33 * t34)
      t94 = t91 ** 2
      t101 = 0.8D1 / 0.405D3 * (0.90D2 * t19 * t23 * t39 * t41 * t34 + t
     #55) * t57 - 0.16D2 / 0.9D1 * t61 * t51 * t22 * t62 * t57 + 0.8D1 /
     # 0.405D3 * (0.90D2 * t19 * t23 * t75 * t41 * t34 + t55) * t62 - 0.
     #8D1 / 0.405D3 * (-0.30D2 * t84 + 0.180D3 * t86 + 0.180D3 * t91 * l
     #h + 0.45D2 * t94) * t15 * t48 * t53
      t102 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t101)
      t104 = sqrt(x4)
      t105 = -0.1D1 + t104
      t106 = t104 + 0.1D1
      t107 = t105 * t106
      t108 = KAPPA2(x1, x2, 0.0D0, -t107, z)
      t109 = s * t108
      t110 = t109 * t4
      t113 = t6 * t105 * t106
      t114 = t109 * t3 * t113
      t115 = t7 * x4
      t116 = t109 * t115
      t117 = t108 ** 2
      t122 = s * t117 * t11 * t48 * (-0.1D1 + x4)
      t126 = t117 ** 2
      t132 = log(-0.4D1 * t70 * t32 * t105 * t106 * t27 * t25 * t126 * x
     #4)
      t135 = 0.1D1 / (-0.2D1 + t108)
      t136 = t135 * t126
      t137 = Sqrt(-t107)
      t138 = t137 ** 2
      t143 = t48 * t17
      t144 = t47 * t143
      t160 = 0.8D1 / 0.405D3 * (-0.90D2 * t61 * t22 * t132 * t136 * t138
     # - 0.180D3 * t144 * wd * t135 * t126 * t22 * t138) * t57 + 0.16D2 
     #/ 0.9D1 * t61 * t136 * t22 * t138 * t62 * t57
      t161 = FJET(XB1, XB2, s, 0.0D0, t110, t114, -t116, t122, t160)
      t163 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t101)
      t165 = FJET(XB1, XB2, s, t110, 0.0D0, -t116, t114, t122, t160)
      t167 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t168 = s * t167
      t169 = t4 * x3
      t170 = t168 * t169
      t172 = sqrt(x3)
      t173 = -0.1D1 + t172
      t175 = t172 + 0.1D1
      t176 = x1 * t173 * t175
      t177 = t168 * t3 * t176
      t178 = t168 * t7
      t179 = t167 ** 2
      t184 = s * t179 * t11 * t48 * (-0.1D1 + x3)
      t185 = t173 * t175
      t186 = Sqrt(-t185)
      t187 = t186 ** 2
      t190 = 0.1D1 / (-0.2D1 + t167)
      t192 = t179 ** 2
      t205 = log(-0.4D1 * t70 * t32 * t173 * t175 * t27 * t68 * t192)
      t208 = t187 * t190 * t192
      t218 = 0.16D2 / 0.9D1 * t61 * t22 * t187 * t190 * t192 * t62 * t57
     # + 0.8D1 / 0.405D3 * (-0.90D2 * t61 * t22 * t205 * t208 - 0.180D3 
     #* t144 * t23 * t208) * t62
      t219 = FJET(XB1, XB2, s, t170, -t177, -t178, 0.0D0, t184, t218)
      t221 = KAPPA2(x1, x2, x3, -t107, z)
      t222 = s * t221
      t223 = t222 * t169
      t224 = t222 * t3
      t225 = t224 * t176
      t226 = t224 * t113
      t227 = t222 * t115
      t228 = t221 ** 2
      t235 = Sqrt(t185 * t107)
      t241 = s * t228 * t11 * t48 * (x3 - 0.1D1 + x4 - 0.2D1 * x3 * x4 +
     # 0.2D1 * t21 * t172 * t104 * t235)
      t242 = t228 ** 2
      t244 = 0.1D1 / (-0.2D1 + t221)
      t250 = (-t172 * t104 + 0.2D1 * t21 * t235) ** 2
      t252 = t250 * t62 * t57
      t255 = 0.4D1 / 0.9D1 * t61 * t242 * t244 * t252
      t256 = FJET(XB1, XB2, s, t223, -t225, t226, -t227, t241, -t255)
      t261 = wd * t242 * t244 * t252
      t264 = FJET(XB1, XB2, s, -t177, t170, 0.0D0, -t178, t184, t218)
      t266 = FJET(XB1, XB2, s, -t225, t223, -t227, t226, t241, -t255)
      rrqq2qqht2s1e0 = t102 * t101 + t161 * t160 + t163 * t101 + t165 * 
     #t160 + t219 * t218 - 0.4D1 / 0.9D1 * t256 * t15 * t143 * t261 + t2
     #64 * t218 - 0.4D1 / 0.9D1 * t266 * t15 * t143 * t261

      end function



      doubleprecision function rrqq2qqht2s1em1
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
      t5 = t2 * t4
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t2 * t7
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = t11 * t3
      t16 = t15 * x1
      t17 = 0.1D1 / z
      t18 = t6 * t17
      t19 = t16 * t18
      t21 = 0.1D1 / (-0.2D1 + t1)
      t22 = wd * t21
      t23 = t9 ** 2
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
      t47 = log(0.4D1 * t35 / t36 * t39 * t41 * t42 * t23)
      t51 = x1 * t6
      t59 = 0.1D1 / x4
      t64 = -0.16D2 / 0.9D1 * t19 * t22 * t27 * t28 - 0.8D1 / 0.405D3 * 
     #(-0.180D3 * lh - 0.90D2 * t47) * t15 * t51 * t17 * wd * t21 * t23 
     #* t26 - 0.16D2 / 0.9D1 * t19 * t22 * t27 * t59
      t65 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t64)
      t67 = sqrt(x4)
      t68 = -0.1D1 + t67
      t69 = t67 + 0.1D1
      t70 = t68 * t69
      t71 = KAPPA2(x1, x2, 0.0D0, -t70, z)
      t72 = s * t71
      t73 = t72 * t4
      t77 = t72 * t3 * t6 * t68 * t69
      t79 = t72 * t7 * x4
      t80 = t71 ** 2
      t85 = s * t80 * t11 * t51 * (-0.1D1 + x4)
      t87 = t16 * t18 * wd
      t89 = 0.1D1 / (-0.2D1 + t71)
      t90 = t80 ** 2
      t92 = Sqrt(-t70)
      t93 = t92 ** 2
      t95 = t26 * t93 * t59
      t98 = 0.16D2 / 0.9D1 * t87 * t89 * t90 * t95
      t99 = FJET(XB1, XB2, s, 0.0D0, t73, t77, -t79, t85, t98)
      t101 = t51 * t17
      t105 = wd * t89 * t90 * t95
      t108 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t64)
      t110 = FJET(XB1, XB2, s, t73, 0.0D0, -t79, t77, t85, t98)
      t115 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t116 = s * t115
      t118 = t116 * t4 * x3
      t120 = sqrt(x3)
      t121 = -0.1D1 + t120
      t123 = t120 + 0.1D1
      t125 = t116 * t3 * x1 * t121 * t123
      t126 = t116 * t7
      t127 = t115 ** 2
      t132 = s * t127 * t11 * t51 * (-0.1D1 + x3)
      t134 = Sqrt(-t121 * t123)
      t135 = t134 ** 2
      t139 = t127 ** 2
      t141 = 0.1D1 / (-0.2D1 + t115) * t139 * t28
      t144 = 0.16D2 / 0.9D1 * t87 * t26 * t135 * t141
      t145 = FJET(XB1, XB2, s, t118, -t125, -t126, 0.0D0, t132, t144)
      t150 = wd * t26 * t135 * t141
      t153 = FJET(XB1, XB2, s, -t125, t118, 0.0D0, -t126, t132, t144)
      rrqq2qqht2s1em1 = t65 * t64 + 0.16D2 / 0.9D1 * t99 * t15 * t101 * 
     #t105 + t108 * t64 + 0.16D2 / 0.9D1 * t110 * t15 * t101 * t105 + 0.
     #16D2 / 0.9D1 * t145 * t15 * t101 * t150 + 0.16D2 / 0.9D1 * t153 * 
     #t15 * t101 * t150

      end function



      doubleprecision function rrqq2qqht2s1em2
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
      t5 = t2 * t3 * x1
      t6 = -0.1D1 + x1
      t8 = t2 * t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t15 = t11 * t3
      t17 = 0.1D1 / z
      t21 = 0.1D1 / (-0.2D1 + t1)
      t23 = t9 ** 2
      t25 = cos(x2 * pi)
      t26 = t25 ** 2
      t30 = 0.16D2 / 0.9D1 * t15 * x1 * t6 * t17 * wd * t21 * t23 * t26
      t31 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, -t30)
      t33 = x1 * t6
      t38 = t17 * wd * t21 * t23 * t26
      t40 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, -t30)
      rrqq2qqht2s1em2 = -0.16D2 / 0.9D1 * t31 * t15 * t33 * t38 - 0.16D2
     # / 0.9D1 * t40 * t15 * t33 * t38

      end function



      doubleprecision function rrqq2qqht2s1em3
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
      rrqq2qqht2s1em3 = 0.0D0

      end function



      doubleprecision function rrqq2qqht2s1em4
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
      rrqq2qqht2s1em4 = 0.0D0

      end function
