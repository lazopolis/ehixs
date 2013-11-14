  
      subroutine rrqqbar2gght3
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2ggh31J1  
      doubleprecision rrqqbar2ggh31J2  
      doubleprecision rrqqbar2ggh31J3  
      doubleprecision rrqqbar2gght3s1e1  
      doubleprecision rrqqbar2gght3s1e0  
      doubleprecision rrqqbar2gght3s1em1  
      doubleprecision rrqqbar2gght3s1em2  
      doubleprecision rrqqbar2gght3s1em3  
      doubleprecision rrqqbar2gght3s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght3s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght3s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght3s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght3s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght3s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght3s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2gght3s1e1
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
      doubleprecision rrqqbar2ggh31J1
      doubleprecision rrqqbar2ggh31J2
      doubleprecision rrqqbar2ggh31J3

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
      t20 = rrqqbar2ggh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t
     #5, 0.0D0, -t14)
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
      t35 = t32 * x4 * t33
      t38 = log(0.4D1 * t29 * t35)
      t39 = t38 * t9
      t40 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t
     #5, 0.0D0, -t14)
      t41 = t18 * t40
      t43 = t38 ** 2
      t45 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t
     #5, 0.0D0, -t14)
      t46 = t18 * t45
      t53 = pi * lh
      t54 = t3 * t16
      t55 = t19 * t40
      t61 = lh ** 2
      t63 = pi ** 2
      t65 = 0.180D3 * t61 - 0.30D2 * t63
      t66 = pi * t65
      t67 = t66 * t3
      t69 = t16 * t9 * t46
      t70 = t67 * t69
      t72 = 0.1D1 / x4
      t78 = log(0.4D1 * t29 * t32 * t33)
      t79 = t78 * pi
      t87 = t78 ** 2
      t88 = t87 * pi
      t112 = x3 * t24
      t113 = t26 * t28
      t114 = t112 * t113
      t117 = log(0.4D1 * t114 * t35)
      t124 = t53 * t3
      t128 = 0.1D1 / x3
      t132 = t28 * t30
      t137 = log(0.4D1 * t112 * t26 * t132 * t31 * t33)
      t138 = t137 * t9
      t140 = t137 ** 2
      t156 = -(0.90D2 * t15 * t16 * (-t21 + t39 * t41 - t43 * t9 * t46 /
     # 0.2D1) - 0.180D3 * t53 * t54 * (-t55 + t39 * t46) - t70) * t72 / 
     #0.720D3 + (-0.180D3 * t53 - 0.90D2 * t79) * t3 * t16 * t21 / 0.720
     #D3 + (t66 + 0.180D3 * t79 * lh + 0.45D2 * t88) * t3 * t16 * t55 / 
     #0.720D3 + (pi * (0.60D2 * lh * t63 - 0.240D3 * zeta3 - 0.120D3 * t
     #61 * lh) - t79 * t65 - 0.90D2 * t88 * lh - 0.15D2 * t87 * t78 * pi
     #) * t3 * t16 * t19 * t45 / 0.720D3 + (0.90D2 * t15 * t16 * (t55 - 
     #t117 * t9 * t46) - 0.180D3 * t124 * t69) * t128 * t72 / 0.720D3 + 
     #(0.90D2 * t15 * t16 * (t21 - t138 * t41 + t140 * t9 * t46 / 0.2D1)
     # - 0.180D3 * t53 * t54 * (t55 - t138 * t46) + t70) * t128 / 0.720D
     #3
      t157 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t156)
      t159 = 0.1D1 - x4
      t160 = KAPPA2(x1, x2, 0.0D0, t159, z)
      t161 = s * t160
      t162 = t161 * t4
      t163 = -t159
      t164 = t7 * t163
      t165 = t161 * t164
      t166 = t7 * x4
      t167 = t161 * t166
      t168 = t160 ** 2
      t171 = x1 * t6
      t173 = s * t168 * t11 * t171 * t163
      t175 = 0.1D1 / (-0.2D1 + t160)
      t176 = t168 * t175
      t177 = rrqqbar2ggh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t165,
     # t162, -t167, t173)
      t181 = t168 ** 2
      t186 = log(-0.4D1 * t27 * t132 * t31 * x4 * t163 * t181)
      t187 = t186 * t168
      t188 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t165,
     # t162, -t167, t173)
      t191 = t186 ** 2
      t193 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t165,
     # t162, -t167, t173)
      t194 = t175 * t193
      t201 = t176 * t188
      t208 = t16 * t168 * t194
      t212 = x4 * t163
      t217 = log(-0.4D1 * t114 * t32 * t212 * t181)
      t230 = -(0.90D2 * t15 * t16 * (t176 * t177 - t187 * t175 * t188 + 
     #t191 * t168 * t194 / 0.2D1) - 0.180D3 * t53 * t54 * (t201 - t187 *
     # t194) + t67 * t208) * t72 / 0.720D3 + (0.90D2 * t15 * t16 * (-t20
     #1 + t217 * t168 * t194) + 0.180D3 * t124 * t208) * t128 * t72 / 0.
     #720D3
      t231 = FJET(XB1, XB2, s, 0.0D0, t162, t165, -t167, t173, t230)
      t233 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t156)
      t235 = FJET(XB1, XB2, s, t162, 0.0D0, -t167, t165, t173, t230)
      t237 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t238 = s * t237
      t239 = t4 * x3
      t240 = t238 * t239
      t241 = -0.1D1 + x3
      t242 = t4 * t241
      t243 = t238 * t242
      t244 = t238 * t7
      t245 = t237 ** 2
      t249 = s * t245 * t11 * t171 * t241
      t251 = 0.1D1 / (-0.2D1 + t237)
      t252 = t245 * t251
      t253 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, s, t240, -t244,
     # -t243, 0.0D0, t249)
      t254 = t252 * t253
      t256 = t245 ** 2
      t261 = log(-0.4D1 * t114 * t32 * t241 * x4 * t256)
      t263 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, s, t240, -t244,
     # -t243, 0.0D0, t249)
      t264 = t251 * t263
      t271 = t16 * t245 * t264
      t277 = rrqqbar2ggh31J3(s, XB1, XB2, z, lh, wd, nf, s, t240, -t244,
     # -t243, 0.0D0, t249)
      t283 = log(-0.4D1 * t114 * t32 * t241 * t256)
      t284 = t283 * t245
      t287 = t283 ** 2
      t304 = (0.90D2 * t15 * t16 * (-t254 + t261 * t245 * t264) + 0.180D
     #3 * t124 * t271) * t128 * t72 / 0.720D3 + (0.90D2 * t15 * t16 * (-
     #t252 * t277 + t284 * t251 * t253 - t287 * t245 * t264 / 0.2D1) - 0
     #.180D3 * t53 * t54 * (-t254 + t284 * t264) - t67 * t271) * t128 / 
     #0.720D3
      t305 = FJET(XB1, XB2, s, t240, -t243, -t244, 0.0D0, t249, t304)
      t307 = KAPPA2(x1, x2, x3, t159, z)
      t308 = s * t307
      t309 = t308 * t239
      t310 = t308 * t242
      t311 = t308 * t164
      t312 = t308 * t166
      t313 = t307 ** 2
      t318 = cos(t22)
      t321 = Sqrt(x3 * t241 * t212)
      t326 = s * t313 * t11 * t171 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t318 * t321)
      t328 = 0.1D1 / (-0.2D1 + t307)
      t330 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, s, t309, t311, 
     #-t310, -t312, t326)
      t335 = t313 ** 2
      t340 = log(0.4D1 * t112 * t113 * t30 * t31 * t241 * t212 * t335)
      t342 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, s, t309, t311, 
     #-t310, -t312, t326)
      t343 = t328 * t342
      t353 = 0.90D2 * t15 * t16 * (t313 * t328 * t330 - t340 * t313 * t3
     #43) - 0.180D3 * t124 * t16 * t313 * t343
      t356 = t353 * t128 * t72 / 0.720D3
      t357 = FJET(XB1, XB2, s, t309, -t310, t311, -t312, t326, t356)
      t359 = t128 * t72
      t362 = FJET(XB1, XB2, s, -t243, t240, 0.0D0, -t244, t249, t304)
      t364 = FJET(XB1, XB2, s, -t310, t309, -t312, t311, t326, t356)
      rrqqbar2gght3s1e1 = t157 * t156 + t231 * t230 + t233 * t156 + t235
     # * t230 + t305 * t304 + t357 * t353 * t359 / 0.720D3 + t362 * t304
     # + t364 * t353 * t359 / 0.720D3

      end function



      doubleprecision function rrqqbar2gght3s1e0
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
      doubleprecision rrqqbar2ggh31J1
      doubleprecision rrqqbar2ggh31J2
      doubleprecision rrqqbar2ggh31J3

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
      t20 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t
     #5, 0.0D0, -t14)
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
      t40 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t
     #5, 0.0D0, -t14)
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
      t81 = rrqqbar2ggh31J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t
     #5, 0.0D0, -t14)
      t89 = log(0.4D1 * t29 * t32 * t33)
      t90 = t89 * pi
      t97 = lh ** 2
      t99 = pi ** 2
      t105 = t89 ** 2
      t114 = -(0.90D2 * t15 * t16 * (-t21 + t38 * t9 * t41) + t52) * t54
     # / 0.720D3 + t15 * t49 * t41 * t59 / 0.8D1 + (0.90D2 * t15 * t16 *
     # (t21 - t70 * t9 * t41) - t52) * t58 / 0.720D3 + t15 * t16 * t19 *
     # t81 / 0.8D1 + (-0.180D3 * t47 - 0.90D2 * t90) * t3 * t16 * t21 / 
     #0.720D3 + (pi * (0.180D3 * t97 - 0.30D2 * t99) + 0.180D3 * t90 * l
     #h + 0.45D2 * t105 * pi) * t3 * t16 * t19 * t40 / 0.720D3
      t115 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t114)
      t117 = 0.1D1 - x4
      t118 = KAPPA2(x1, x2, 0.0D0, t117, z)
      t119 = s * t118
      t120 = t119 * t4
      t121 = -t117
      t122 = t7 * t121
      t123 = t119 * t122
      t124 = t7 * x4
      t125 = t119 * t124
      t126 = t118 ** 2
      t129 = x1 * t6
      t131 = s * t126 * t11 * t129 * t121
      t133 = 0.1D1 / (-0.2D1 + t118)
      t135 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t123,
     # t120, -t125, t131)
      t139 = t126 ** 2
      t144 = log(-0.4D1 * t27 * t65 * t31 * x4 * t121 * t139)
      t146 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t123,
     # t120, -t125, t131)
      t147 = t133 * t146
      t153 = t16 * t126
      t164 = -(0.90D2 * t15 * t16 * (t126 * t133 * t135 - t144 * t126 * 
     #t147) - 0.180D3 * t48 * t153 * t147) * t54 / 0.720D3 - t15 * t153 
     #* t147 * t59 / 0.8D1
      t165 = FJET(XB1, XB2, s, 0.0D0, t120, t123, -t125, t131, t164)
      t167 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t114)
      t169 = FJET(XB1, XB2, s, t120, 0.0D0, -t125, t123, t131, t164)
      t171 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t172 = s * t171
      t173 = t4 * x3
      t174 = t172 * t173
      t175 = -0.1D1 + x3
      t176 = t4 * t175
      t177 = t172 * t176
      t178 = t172 * t7
      t179 = t171 ** 2
      t183 = s * t179 * t11 * t129 * t175
      t184 = t16 * t179
      t187 = 0.1D1 / (-0.2D1 + t171)
      t188 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, s, t174, -t178,
     # -t177, 0.0D0, t183)
      t189 = t187 * t188
      t194 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, s, t174, -t178,
     # -t177, 0.0D0, t183)
      t198 = t179 ** 2
      t203 = log(-0.4D1 * t63 * t26 * t28 * t32 * t175 * t198)
      t216 = -t15 * t184 * t189 * t59 / 0.8D1 + (0.90D2 * t15 * t16 * (-
     #t179 * t187 * t194 + t203 * t179 * t189) + 0.180D3 * t48 * t184 * 
     #t189) * t58 / 0.720D3
      t217 = FJET(XB1, XB2, s, t174, -t177, -t178, 0.0D0, t183, t216)
      t219 = KAPPA2(x1, x2, x3, t117, z)
      t220 = s * t219
      t221 = t220 * t173
      t222 = t220 * t176
      t223 = t220 * t122
      t224 = t220 * t124
      t225 = t219 ** 2
      t230 = cos(t22)
      t234 = Sqrt(x3 * t175 * x4 * t121)
      t239 = s * t225 * t11 * t129 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t230 * t234)
      t243 = 0.1D1 / (-0.2D1 + t219)
      t244 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, s, t221, t223, 
     #-t222, -t224, t239)
      t248 = t15 * t16 * t225 * t243 * t244 * t59 / 0.8D1
      t249 = FJET(XB1, XB2, s, t221, -t222, t223, -t224, t239, t248)
      t251 = t3 * t16
      t256 = t225 * t243 * t244 * t58 * t54
      t259 = FJET(XB1, XB2, s, -t177, t174, 0.0D0, -t178, t183, t216)
      t261 = FJET(XB1, XB2, s, -t222, t221, -t224, t223, t239, t248)
      rrqqbar2gght3s1e0 = t115 * t114 + t165 * t164 + t167 * t114 + t169
     # * t164 + t217 * t216 + t249 * pi * t251 * t256 / 0.8D1 + t259 * t
     #216 + t261 * pi * t251 * t256 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght3s1em1
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
      doubleprecision rrqqbar2ggh31J1
      doubleprecision rrqqbar2ggh31J2
      doubleprecision rrqqbar2ggh31J3

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
      t21 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t
     #5, 0.0D0, -t14)
      t22 = 0.1D1 / x3
      t27 = rrqqbar2ggh31J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t
     #5, 0.0D0, -t14)
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
      t65 = 0.1D1 - x4
      t66 = KAPPA2(x1, x2, 0.0D0, t65, z)
      t67 = s * t66
      t68 = t67 * t4
      t69 = -t65
      t71 = t67 * t7 * t69
      t73 = t67 * t7 * x4
      t74 = t66 ** 2
      t77 = x1 * t6
      t79 = s * t74 * t11 * t77 * t69
      t83 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t71, t
     #68, -t73, t79)
      t85 = t74 / (-0.2D1 + t66) * t83 * t57
      t87 = t17 * t85 / 0.8D1
      t88 = FJET(XB1, XB2, s, 0.0D0, t68, t71, -t73, t79, -t87)
      t90 = t3 * t16
      t94 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t62)
      t96 = FJET(XB1, XB2, s, t68, 0.0D0, -t73, t71, t79, -t87)
      t101 = KAPPA2(x1, x2, x3, 0.10D1, z)
      t102 = s * t101
      t104 = t102 * t4 * x3
      t105 = -0.1D1 + x3
      t107 = t102 * t4 * t105
      t108 = t102 * t7
      t109 = t101 ** 2
      t113 = s * t109 * t11 * t77 * t105
      t117 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, s, t104, -t108,
     # -t107, 0.0D0, t113)
      t119 = t109 / (-0.2D1 + t101) * t117 * t22
      t121 = t17 * t119 / 0.8D1
      t122 = FJET(XB1, XB2, s, t104, -t107, -t108, 0.0D0, t113, -t121)
      t127 = FJET(XB1, XB2, s, -t107, t104, 0.0D0, -t108, t113, -t121)
      rrqqbar2gght3s1em1 = t63 * t62 - t88 * pi * t90 * t85 / 0.8D1 + t9
     #4 * t62 - t96 * pi * t90 * t85 / 0.8D1 - t122 * pi * t90 * t119 / 
     #0.8D1 - t127 * pi * t90 * t119 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght3s1em2
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
      doubleprecision rrqqbar2ggh31J1
      doubleprecision rrqqbar2ggh31J2
      doubleprecision rrqqbar2ggh31J3

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
      t5 = t2 * t3 * x1
      t6 = -0.1D1 + x1
      t8 = t2 * t3 * t6
      t9 = t1 ** 2
      t11 = t3 ** 2
      t14 = s * t9 * t11 * x1 * t6
      t16 = 0.1D1 / s
      t19 = 0.1D1 / (-0.2D1 + t1)
      t21 = rrqqbar2ggh31J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t8, t
     #5, 0.0D0, -t14)
      t24 = pi * t3 * t16 * t9 * t19 * t21 / 0.8D1
      t25 = FJET(XB1, XB2, s, 0.0D0, t5, -t8, 0.0D0, -t14, t24)
      t30 = t16 * t9 * t19 * t21
      t32 = FJET(XB1, XB2, s, t5, 0.0D0, 0.0D0, -t8, -t14, t24)
      rrqqbar2gght3s1em2 = t25 * pi * t3 * t30 / 0.8D1 + t32 * pi * t3 *
     # t30 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght3s1em3
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
      doubleprecision rrqqbar2ggh31J1
      doubleprecision rrqqbar2ggh31J2
      doubleprecision rrqqbar2ggh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght3s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght3s1em4
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
      doubleprecision rrqqbar2ggh31J1
      doubleprecision rrqqbar2ggh31J2
      doubleprecision rrqqbar2ggh31J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght3s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqqbar2ggh31J1
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
      rrqqbar2ggh31J1 = (0.128D3 / 0.27D2 * S34 - 0.128D3 / 0.27D2 * S14
     # * S23 / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqqbar2ggh31J2
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
      t2 = 0.1D1 / (S12 + S14 + S24)
      t4 = 0.1D1 / (S12 + S13 + S23)
      t7 = S34 ** 2
      t14 = S23 ** 2
      t17 = S14 ** 2
      t26 = S14 * S23
      rrqqbar2ggh31J2 = ((0.64D2 / 0.27D2 * t2 + 0.64D2 / 0.27D2 * t4) *
     # t7 + (0.128D3 / 0.27D2 * S23 * t2 + 0.128D3 / 0.27D2 * S14 * t4) 
     #* S34 + 0.64D2 / 0.27D2 * t14 * t2 + 0.64D2 / 0.27D2 * t17 * t4 + 
     #((0.128D3 / 0.27D2 - 0.64D2 / 0.27D2 * S14 * t2 - 0.64D2 / 0.27D2 
     #* S23 * t4) * t7 + (-0.128D3 / 0.27D2 * t26 * t2 - 0.128D3 / 0.27D
     #2 * t26 * t4) * S34 - 0.64D2 / 0.27D2 * t14 - 0.64D2 / 0.27D2 * t1
     #7 - 0.64D2 / 0.27D2 * t14 * S14 * t2 - 0.64D2 / 0.27D2 * S23 * t17
     # * t4) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqqbar2ggh31J3
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
      t2 = 0.1D1 / (S12 + S13 + S23)
      t4 = 0.1D1 / (S12 + S14 + S24)
      t6 = s ** 2
      t8 = z ** 2
      t13 = -t4 - t2
      t14 = 0.128D3 / 0.9D1 * t13
      t27 = S12 ** 2
      t43 = S34 ** 2
      t57 = S14 ** 2
      t59 = S23 ** 2
      t62 = 0.64D2 / 0.27D2 * S14 * S23
      t101 = 0.128D3 / 0.27D2 * t2 * t4 * t6 * s * t8 * z + t14 * t6 * t
     #8 + (-t14 * S12 + 0.256D3 / 0.9D1 + 0.128D3 / 0.9D1 * S23 * t4 + 0
     #.128D3 / 0.9D1 * S14 * t2) * s * z + 0.112D3 / 0.27D2 * t13 * t27 
     #+ (0.32D2 / 0.9D1 * t13 * S34 - 0.224D3 / 0.9D1 + (-0.32D2 / 0.3D1
     # * S23 - 0.16D2 / 0.9D1 * S14) * t4 + (-0.32D2 / 0.3D1 * S14 - 0.1
     #6D2 / 0.9D1 * S23) * t2) * S12 - 0.128D3 / 0.27D2 * t13 * t43 + (-
     #0.64D2 / 0.9D1 + (0.64D2 / 0.9D1 * S14 + 0.64D2 / 0.27D2 * S23) * 
     #t4 + (0.64D2 / 0.27D2 * S14 + 0.64D2 / 0.9D1 * S23) * t2) * S34 - 
     #0.416D3 / 0.27D2 * S14 - 0.416D3 / 0.27D2 * S23 + (0.16D2 / 0.9D1 
     #* t57 - 0.128D3 / 0.27D2 * t59 + t62) * t4 + (t62 - 0.128D3 / 0.27
     #D2 * t57 + 0.16D2 / 0.9D1 * t59) * t2 + ((0.128D3 / 0.27D2 - 0.128
     #D3 / 0.27D2 * S14 * t4 - 0.128D3 / 0.27D2 * S23 * t2) * t43 + (0.3
     #2D2 / 0.27D2 * S14 + 0.32D2 / 0.27D2 * S23 + (-0.32D2 / 0.9D1 * t5
     #7 - t62) * t4 + (-t62 - 0.32D2 / 0.9D1 * t59) * t2) * S34 + 0.16D2
     # / 0.27D2 * t57 - t62 + 0.16D2 / 0.27D2 * t59 + (-0.16D2 / 0.27D2 
     #* t57 * S14 - 0.32D2 / 0.27D2 * S23 * t57) * t4 + (-0.32D2 / 0.27D
     #2 * t59 * S14 - 0.16D2 / 0.27D2 * t59 * S23) * t2) / S12
      rrqqbar2ggh31J3 = t101 / pi * wd / z

      end function
  
 