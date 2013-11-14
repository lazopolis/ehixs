  
      subroutine rrgg2qqbarht2
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarh21J1  
      doubleprecision rrgg2qqbarh21J2  
      doubleprecision rrgg2qqbarh21J3  
      doubleprecision rrgg2qqbarh21J4  
      doubleprecision rrgg2qqbarh21J5  
      doubleprecision rrgg2qqbarh21J6  
      doubleprecision rrgg2qqbarh21J7  
      doubleprecision rrgg2qqbarht2s1e1  
      doubleprecision rrgg2qqbarht2s1e0  
      doubleprecision rrgg2qqbarht2s1em1  
      doubleprecision rrgg2qqbarht2s1em2  
      doubleprecision rrgg2qqbarht2s1em3  
      doubleprecision rrgg2qqbarht2s1em4  
      doubleprecision rrgg2qqbarht2s2e1  
      doubleprecision rrgg2qqbarht2s2e0  
      doubleprecision rrgg2qqbarht2s2em1  
      doubleprecision rrgg2qqbarht2s2em2  
      doubleprecision rrgg2qqbarht2s2em3  
      doubleprecision rrgg2qqbarht2s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht2s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht2s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht2s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht2s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht2s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht2s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht2s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht2s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht2s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht2s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht2s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht2s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht2s1e1
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
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = pi * t6
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t10 = t8 * t9
      t11 = x2 * pi
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = x3 * t13
      t15 = z ** 2
      t16 = 0.1D1 / t15
      t18 = x4 * t4
      t19 = -0.1D1 + x3
      t23 = log(0.4D1 * t14 * t16 * t18 * t19)
      t24 = t23 ** 2
      t25 = t16 * x4
      t26 = t25 * t4
      t29 = log(-0.4D1 * t14 * t26)
      t30 = t29 ** 2
      t36 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t37 = t8 * t36
      t40 = pi * lh
      t41 = t6 * t8
      t42 = t41 * t9
      t49 = 0.1D1 / x3
      t52 = lh ** 2
      t54 = pi ** 2
      t56 = 0.180D3 * t52 - 0.30D2 * t54
      t57 = pi * t56
      t58 = t13 * t16
      t61 = log(-0.4D1 * t58 * t18)
      t62 = t61 * pi
      t65 = t61 ** 2
      t66 = t65 * pi
      t72 = rrgg2qqbarh21J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t97 = rrgg2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t102 = x1 ** 2
      t103 = x3 * t102
      t104 = t103 * t13
      t109 = log(0.4D1 * t104 * t25 * t4 * t19)
      t113 = log(-0.4D1 * t104 * t26)
      t117 = 0.1D1 / x1
      t121 = t102 * t13
      t124 = log(-0.4D1 * t121 * t26)
      t126 = t124 ** 2
      t142 = -(0.90D2 * t7 * t10 * (t24 / 0.2D1 - t30 / 0.2D1) + (0.90D2
     # * t7 * t37 - 0.180D3 * t40 * t42) * (-t23 + t29)) * t49 / 0.1440D
     #4 + (t57 + 0.180D3 * t62 * lh + 0.45D2 * t66) * t6 * t37 / 0.1440D
     #4 + t7 * t8 * t72 / 0.16D2 + (pi * (0.60D2 * lh * t54 - 0.240D3 * 
     #zeta3 - 0.120D3 * t52 * lh) - t62 * t56 - 0.90D2 * t66 * lh - 0.15
     #D2 * t65 * t61 * pi) * t6 * t10 / 0.1440D4 + (-0.180D3 * t40 - 0.9
     #0D2 * t62) * t6 * t8 * t97 / 0.1440D4 - t7 * t8 * (-t109 * t9 + t1
     #13 * t9) * t49 * t117 / 0.8D1 + (0.90D2 * t7 * t8 * (t97 - t124 * 
     #t36 + t126 * t9 / 0.2D1) - 0.180D3 * t40 * t41 * (t36 - t124 * t9)
     # + t57 * t42) * t117 / 0.720D3
      t143 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t142)
      t145 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t142)
      t147 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t148 = s * t147
      t149 = t1 * x1
      t150 = t148 * t149
      t151 = -0.1D1 + x1
      t152 = t1 * t151
      t153 = t152 * x4
      t154 = t148 * t153
      t155 = t152 * t4
      t156 = t148 * t155
      t157 = t147 ** 2
      t160 = x1 * t151
      t162 = s * t157 * t6 * t160 * t4
      t163 = t151 * t157
      t165 = 0.1D1 / (-0.2D1 + t147)
      t166 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, s, t150, -t154,
     # 0.0D0, t156, t162)
      t168 = t163 * t165 * t166
      t169 = t103 * t58
      t170 = t151 ** 2
      t171 = t157 ** 2
      t173 = t18 * t170 * t171
      t176 = log(-0.4D1 * t169 * t173)
      t178 = t157 * t165
      t179 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, t150, -t154,
     # 0.0D0, t156, t162)
      t180 = t178 * t179
      t186 = t40 * t41
      t188 = t163 * t165 * t179
      t194 = rrgg2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, s, t150, -t154,
     # 0.0D0, t156, t162)
      t200 = log(-0.4D1 * t121 * t16 * t173)
      t201 = t200 * t151
      t204 = t200 ** 2
      t222 = -(0.90D2 * t7 * t8 * (t168 - t176 * t151 * t180) - 0.180D3 
     #* t186 * t188) * t49 * t117 / 0.720D3 + (-0.90D2 * t7 * t8 * (t163
     # * t165 * t194 - t201 * t178 * t166 + t204 * t151 * t180 / 0.2D1) 
     #+ 0.180D3 * t40 * t41 * (t168 - t201 * t180) - t57 * t41 * t188) *
     # t117 / 0.720D3
      t223 = FJET(XB1, XB2, s, t150, 0.0D0, -t154, t156, t162, t222)
      t225 = FJET(XB1, XB2, s, -t154, t156, t150, 0.0D0, t162, t222)
      t228 = KAPPA2(x1, x2, -t19, x4, z)
      t229 = s * t228
      t231 = t229 * t149 * t19
      t233 = t229 * t149 * x3
      t234 = t229 * t153
      t235 = t229 * t155
      t236 = t228 ** 2
      t241 = cos(t11)
      t244 = Sqrt(x3 * t19 * t18)
      t249 = s * t236 * t6 * t160 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4 
     #+ 0.2D1 * t241 * t244)
      t250 = t151 * t236
      t252 = 0.1D1 / (-0.2D1 + t228)
      t253 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, s, -t231, -t234
     #, t233, t235, t249)
      t257 = t236 ** 2
      t262 = log(0.4D1 * t169 * t18 * t170 * t19 * t257)
      t265 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, -t231, -t234
     #, t233, t235, t249)
      t276 = -0.90D2 * t7 * t8 * (t250 * t252 * t253 - t262 * t151 * t23
     #6 * t252 * t265) + 0.180D3 * t186 * t250 * t252 * t265
      t279 = t276 * t49 * t117 / 0.720D3
      t280 = FJET(XB1, XB2, s, -t231, t233, -t234, t235, t249, -t279)
      t282 = t49 * t117
      t285 = FJET(XB1, XB2, s, -t234, t235, -t231, t233, t249, -t279)
      rrgg2qqbarht2s1e1 = t143 * t142 + t145 * t142 + t223 * t222 + t225
     # * t222 - t280 * t276 * t282 / 0.720D3 - t285 * t276 * t282 / 0.72
     #0D3

      end function



      doubleprecision function rrgg2qqbarht2s1e0
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
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = pi * t6
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t13 = pi * lh
      t15 = x2 * pi
      t16 = sin(t15)
      t17 = t16 ** 2
      t18 = z ** 2
      t19 = 0.1D1 / t18
      t21 = x4 * t4
      t24 = log(-0.4D1 * t17 * t19 * t21)
      t25 = t24 * pi
      t29 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t33 = lh ** 2
      t35 = pi ** 2
      t41 = t24 ** 2
      t46 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t51 = x3 * t17
      t53 = -0.1D1 + x3
      t57 = log(0.4D1 * t51 * t19 * t21 * t53)
      t59 = t19 * x4 * t4
      t62 = log(-0.4D1 * t51 * t59)
      t65 = 0.1D1 / x3
      t69 = x1 ** 2
      t70 = t69 * t17
      t73 = log(-0.4D1 * t70 * t59)
      t79 = t6 * t8
      t84 = 0.1D1 / x1
      t87 = t7 * t8 * t9 / 0.16D2 + (-0.180D3 * t13 - 0.90D2 * t25) * t6
     # * t8 * t29 / 0.1440D4 + (pi * (0.180D3 * t33 - 0.30D2 * t35) + 0.
     #180D3 * t25 * lh + 0.45D2 * t41 * pi) * t6 * t8 * t46 / 0.1440D4 -
     # t7 * t8 * t46 * (-t57 + t62) * t65 / 0.16D2 + (0.90D2 * t7 * t8 *
     # (t29 - t73 * t46) - 0.180D3 * t13 * t79 * t46) * t84 / 0.720D3
      t88 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t87)
      t90 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t87)
      t92 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t93 = s * t92
      t94 = t1 * x1
      t95 = t93 * t94
      t96 = -0.1D1 + x1
      t97 = t1 * t96
      t98 = t97 * x4
      t99 = t93 * t98
      t100 = t97 * t4
      t101 = t93 * t100
      t102 = t92 ** 2
      t105 = x1 * t96
      t107 = s * t102 * t6 * t105 * t4
      t109 = t7 * t8 * t96
      t111 = 0.1D1 / (-0.2D1 + t92)
      t112 = t102 * t111
      t113 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, t95, -t99, 0
     #.0D0, t101, t107)
      t119 = t96 * t102
      t120 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, s, t95, -t99, 0
     #.0D0, t101, t107)
      t124 = t96 ** 2
      t125 = t102 ** 2
      t130 = log(-0.4D1 * t70 * t19 * t21 * t124 * t125)
      t146 = -t109 * t112 * t113 * t65 * t84 / 0.8D1 + (-0.90D2 * t7 * t
     #8 * (t119 * t111 * t120 - t130 * t96 * t112 * t113) + 0.180D3 * t1
     #3 * t79 * t119 * t111 * t113) * t84 / 0.720D3
      t147 = FJET(XB1, XB2, s, t95, 0.0D0, -t99, t101, t107, t146)
      t149 = FJET(XB1, XB2, s, -t99, t101, t95, 0.0D0, t107, t146)
      t152 = KAPPA2(x1, x2, -t53, x4, z)
      t153 = s * t152
      t155 = t153 * t94 * t53
      t157 = t153 * t94 * x3
      t158 = t153 * t98
      t159 = t153 * t100
      t160 = t152 ** 2
      t165 = cos(t15)
      t168 = Sqrt(x3 * t53 * t21)
      t173 = s * t160 * t6 * t105 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4 
     #+ 0.2D1 * t165 * t168)
      t177 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, -t155, -t158
     #, t157, t159, t173)
      t180 = t160 / (-0.2D1 + t152) * t177 * t65 * t84
      t182 = t109 * t180 / 0.8D1
      t183 = FJET(XB1, XB2, s, -t155, t157, -t158, t159, t173, t182)
      t185 = t79 * t96
      t189 = FJET(XB1, XB2, s, -t158, t159, -t155, t157, t173, t182)
      rrgg2qqbarht2s1e0 = t88 * t87 + t90 * t87 + t147 * t146 + t149 * t
     #146 + t183 * pi * t185 * t180 / 0.8D1 + t189 * pi * t185 * t180 / 
     #0.8D1

      end function



      doubleprecision function rrgg2qqbarht2s1em1
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
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = pi * t6
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t16 = sin(x2 * pi)
      t17 = t16 ** 2
      t18 = z ** 2
      t24 = log(-0.4D1 * t17 / t18 * x4 * t4)
      t29 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t30 = t8 * t29
      t33 = 0.1D1 / x1
      t37 = t7 * t8 * t9 / 0.16D2 + (-0.180D3 * pi * lh - 0.90D2 * t24 *
     # pi) * t6 * t30 / 0.1440D4 + t7 * t30 * t33 / 0.8D1
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t37)
      t40 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t37)
      t42 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t43 = s * t42
      t45 = t43 * t1 * x1
      t46 = -0.1D1 + x1
      t47 = t1 * t46
      t49 = t43 * t47 * x4
      t51 = t43 * t47 * t4
      t52 = t42 ** 2
      t57 = s * t52 * t6 * x1 * t46 * t4
      t61 = 0.1D1 / (-0.2D1 + t42)
      t63 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, t45, -t49, 0.
     #0D0, t51, t57)
      t67 = t7 * t8 * t46 * t52 * t61 * t63 * t33 / 0.8D1
      t68 = FJET(XB1, XB2, s, t45, 0.0D0, -t49, t51, t57, -t67)
      t70 = t6 * t8
      t75 = t46 * t52 * t61 * t63 * t33
      t78 = FJET(XB1, XB2, s, -t49, t51, t45, 0.0D0, t57, -t67)
      rrgg2qqbarht2s1em1 = t38 * t37 + t40 * t37 - t68 * pi * t70 * t75 
     #/ 0.8D1 - t78 * pi * t70 * t75 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht2s1em2
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
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = t1 ** 2
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t12 = pi * t6 * t8 * t9 / 0.16D2
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t12)
      t16 = t6 * t8 * t9
      t18 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t12)
      rrgg2qqbarht2s1em2 = t13 * pi * t16 / 0.16D2 + t18 * pi * t16 / 0.
     #16D2

      end function



      doubleprecision function rrgg2qqbarht2s1em3
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
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht2s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht2s1em4
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
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht2s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht2s2e1
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
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = pi * t6
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t10 = t8 * t9
      t11 = x2 * pi
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = x3 * t13
      t15 = z ** 2
      t16 = 0.1D1 / t15
      t18 = x4 * t4
      t19 = -0.1D1 + x3
      t23 = log(0.4D1 * t14 * t16 * t18 * t19)
      t24 = t23 ** 2
      t25 = t16 * x4
      t26 = t25 * t4
      t29 = log(-0.4D1 * t14 * t26)
      t30 = t29 ** 2
      t36 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t37 = t8 * t36
      t40 = pi * lh
      t41 = t6 * t8
      t42 = t41 * t9
      t49 = 0.1D1 / x3
      t52 = lh ** 2
      t54 = pi ** 2
      t56 = 0.180D3 * t52 - 0.30D2 * t54
      t57 = pi * t56
      t58 = t13 * t16
      t61 = log(-0.4D1 * t58 * t18)
      t62 = t61 * pi
      t65 = t61 ** 2
      t66 = t65 * pi
      t72 = rrgg2qqbarh21J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t97 = rrgg2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t102 = x1 ** 2
      t103 = x3 * t102
      t104 = t103 * t13
      t109 = log(0.4D1 * t104 * t25 * t4 * t19)
      t113 = log(-0.4D1 * t104 * t26)
      t117 = 0.1D1 / x1
      t121 = t102 * t13
      t124 = log(-0.4D1 * t121 * t26)
      t126 = t124 ** 2
      t142 = -(0.90D2 * t7 * t10 * (t24 / 0.2D1 - t30 / 0.2D1) + (0.90D2
     # * t7 * t37 - 0.180D3 * t40 * t42) * (-t23 + t29)) * t49 / 0.1440D
     #4 + (t57 + 0.180D3 * t62 * lh + 0.45D2 * t66) * t6 * t37 / 0.1440D
     #4 + t7 * t8 * t72 / 0.16D2 + (pi * (0.60D2 * lh * t54 - 0.240D3 * 
     #zeta3 - 0.120D3 * t52 * lh) - t62 * t56 - 0.90D2 * t66 * lh - 0.15
     #D2 * t65 * t61 * pi) * t6 * t10 / 0.1440D4 + (-0.180D3 * t40 - 0.9
     #0D2 * t62) * t6 * t8 * t97 / 0.1440D4 - t7 * t8 * (-t109 * t9 + t1
     #13 * t9) * t49 * t117 / 0.8D1 + (0.90D2 * t7 * t8 * (t97 - t124 * 
     #t36 + t126 * t9 / 0.2D1) - 0.180D3 * t40 * t41 * (t36 - t124 * t9)
     # + t57 * t42) * t117 / 0.720D3
      t143 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t142)
      t145 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t146 = s * t145
      t147 = t1 * x1
      t148 = t146 * t147
      t149 = -0.1D1 + x1
      t150 = t1 * t149
      t151 = t150 * x4
      t152 = t146 * t151
      t153 = t150 * t4
      t154 = t146 * t153
      t155 = t145 ** 2
      t158 = x1 * t149
      t160 = s * t155 * t6 * t158 * x4
      t161 = t149 * t155
      t163 = 0.1D1 / (-0.2D1 + t145)
      t164 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t152
     #, t148, t154, -t160)
      t166 = t161 * t163 * t164
      t167 = t103 * t58
      t168 = t149 ** 2
      t169 = t155 ** 2
      t171 = t18 * t168 * t169
      t174 = log(-0.4D1 * t167 * t171)
      t176 = t155 * t163
      t177 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t152
     #, t148, t154, -t160)
      t178 = t176 * t177
      t184 = t40 * t41
      t186 = t161 * t163 * t177
      t191 = (0.90D2 * t7 * t8 * (t166 - t174 * t149 * t178) - 0.180D3 *
     # t184 * t186) * t49 * t117
      t192 = rrgg2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t152
     #, t148, t154, -t160)
      t198 = log(-0.4D1 * t121 * t16 * t171)
      t199 = t198 * t149
      t202 = t198 ** 2
      t206 = -t161 * t163 * t192 + t199 * t176 * t164 - t202 * t149 * t1
     #78 / 0.2D1
      t211 = -t166 + t199 * t178
      t216 = t57 * t41 * t186
      t220 = -t191 / 0.720D3 + (0.90D2 * t7 * t8 * t206 - 0.180D3 * t40 
     #* t41 * t211 - t216) * t117 / 0.720D3
      t221 = FJET(XB1, XB2, s, 0.0D0, t148, -t152, t154, -t160, t220)
      t223 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t142)
      t225 = KAPPA2(x1, x2, x3, x4, z)
      t226 = s * t225
      t228 = t226 * t147 * x3
      t230 = t226 * t147 * t19
      t231 = t226 * t151
      t232 = t226 * t153
      t233 = t225 ** 2
      t238 = cos(t11)
      t241 = Sqrt(x3 * t19 * t18)
      t246 = s * t233 * t6 * t158 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t238 * t241)
      t247 = t149 * t233
      t249 = 0.1D1 / (-0.2D1 + t225)
      t250 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, s, t228, -t231,
     # -t230, t232, t246)
      t254 = t233 ** 2
      t259 = log(0.4D1 * t167 * t18 * t168 * t19 * t254)
      t262 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, t228, -t231,
     # -t230, t232, t246)
      t265 = -t247 * t249 * t250 + t259 * t149 * t233 * t249 * t262
      t272 = 0.180D3 * t184 * t247 * t249 * t262
      t273 = 0.90D2 * t7 * t8 * t265 + t272
      t277 = FJET(XB1, XB2, s, t228, -t230, -t231, t232, t246, -t273 * t
     #49 * t117 / 0.720D3)
      t279 = t49 * t117
      t293 = -t191 / 0.720D3 + (0.90D2 * t7 * t8 * t206 - 0.180D3 * t40 
     #* t41 * t211 - t216) * t117 / 0.720D3
      t294 = FJET(XB1, XB2, s, -t152, t154, 0.0D0, t148, -t160, t293)
      t300 = 0.90D2 * t7 * t8 * t265 + t272
      t304 = FJET(XB1, XB2, s, -t231, t232, t228, -t230, t246, -t300 * t
     #49 * t117 / 0.720D3)
      rrgg2qqbarht2s2e1 = t143 * t142 + t221 * t220 + t223 * t142 - t277
     # * t273 * t279 / 0.720D3 + t294 * t293 - t304 * t300 * t279 / 0.72
     #0D3

      end function



      doubleprecision function rrgg2qqbarht2s2e0
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
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = pi * t6
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t13 = pi * lh
      t15 = x2 * pi
      t16 = sin(t15)
      t17 = t16 ** 2
      t18 = z ** 2
      t19 = 0.1D1 / t18
      t21 = x4 * t4
      t24 = log(-0.4D1 * t17 * t19 * t21)
      t25 = t24 * pi
      t29 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t33 = lh ** 2
      t35 = pi ** 2
      t41 = t24 ** 2
      t46 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t51 = x3 * t17
      t53 = -0.1D1 + x3
      t57 = log(0.4D1 * t51 * t19 * t21 * t53)
      t59 = t19 * x4 * t4
      t62 = log(-0.4D1 * t51 * t59)
      t65 = 0.1D1 / x3
      t69 = x1 ** 2
      t70 = t69 * t17
      t73 = log(-0.4D1 * t70 * t59)
      t79 = t6 * t8
      t84 = 0.1D1 / x1
      t87 = t7 * t8 * t9 / 0.16D2 + (-0.180D3 * t13 - 0.90D2 * t25) * t6
     # * t8 * t29 / 0.1440D4 + (pi * (0.180D3 * t33 - 0.30D2 * t35) + 0.
     #180D3 * t25 * lh + 0.45D2 * t41 * pi) * t6 * t8 * t46 / 0.1440D4 -
     # t7 * t8 * t46 * (-t57 + t62) * t65 / 0.16D2 + (0.90D2 * t7 * t8 *
     # (t29 - t73 * t46) - 0.180D3 * t13 * t79 * t46) * t84 / 0.720D3
      t88 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t87)
      t90 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t91 = s * t90
      t92 = t1 * x1
      t93 = t91 * t92
      t94 = -0.1D1 + x1
      t95 = t1 * t94
      t96 = t95 * x4
      t97 = t91 * t96
      t98 = t95 * t4
      t99 = t91 * t98
      t100 = t90 ** 2
      t103 = x1 * t94
      t105 = s * t100 * t6 * t103 * x4
      t107 = t7 * t8 * t94
      t109 = 0.1D1 / (-0.2D1 + t90)
      t110 = t100 * t109
      t111 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t97,
     # t93, t99, -t105)
      t116 = t107 * t110 * t111 * t65 * t84 / 0.8D1
      t117 = t94 * t100
      t118 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t97,
     # t93, t99, -t105)
      t122 = t100 ** 2
      t123 = t94 ** 2
      t128 = log(-0.4D1 * t70 * t19 * t21 * t122 * t123)
      t132 = -t117 * t109 * t118 + t128 * t94 * t110 * t111
      t140 = 0.180D3 * t13 * t79 * t117 * t109 * t111
      t144 = -t116 + (0.90D2 * t7 * t8 * t132 + t140) * t84 / 0.720D3
      t145 = FJET(XB1, XB2, s, 0.0D0, t93, -t97, t99, -t105, t144)
      t147 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t87)
      t149 = KAPPA2(x1, x2, x3, x4, z)
      t150 = s * t149
      t152 = t150 * t92 * x3
      t154 = t150 * t92 * t53
      t155 = t150 * t96
      t156 = t150 * t98
      t157 = t149 ** 2
      t162 = cos(t15)
      t165 = Sqrt(x3 * t53 * t21)
      t170 = s * t157 * t6 * t103 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t162 * t165)
      t174 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, t152, -t155,
     # -t154, t156, t170)
      t177 = t157 / (-0.2D1 + t149) * t174 * t65 * t84
      t179 = t107 * t177 / 0.8D1
      t180 = FJET(XB1, XB2, s, t152, -t154, -t155, t156, t170, t179)
      t182 = t79 * t94
      t193 = -t116 + (0.90D2 * t7 * t8 * t132 + t140) * t84 / 0.720D3
      t194 = FJET(XB1, XB2, s, -t97, t99, 0.0D0, t93, -t105, t193)
      t196 = FJET(XB1, XB2, s, -t155, t156, t152, -t154, t170, t179)
      rrgg2qqbarht2s2e0 = t88 * t87 + t145 * t144 + t147 * t87 + t180 * 
     #pi * t182 * t177 / 0.8D1 + t194 * t193 + t196 * pi * t182 * t177 /
     # 0.8D1

      end function



      doubleprecision function rrgg2qqbarht2s2em1
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
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = pi * t6
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t16 = sin(x2 * pi)
      t17 = t16 ** 2
      t18 = z ** 2
      t24 = log(-0.4D1 * t17 / t18 * x4 * t4)
      t29 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t30 = t8 * t29
      t33 = 0.1D1 / x1
      t37 = t7 * t8 * t9 / 0.16D2 + (-0.180D3 * pi * lh - 0.90D2 * t24 *
     # pi) * t6 * t30 / 0.1440D4 + t7 * t30 * t33 / 0.8D1
      t38 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t37)
      t40 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t41 = s * t40
      t43 = t41 * t1 * x1
      t44 = -0.1D1 + x1
      t45 = t1 * t44
      t47 = t41 * t45 * x4
      t49 = t41 * t45 * t4
      t50 = t40 ** 2
      t55 = s * t50 * t6 * x1 * t44 * x4
      t59 = 0.1D1 / (-0.2D1 + t40)
      t61 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t47, 
     #t43, t49, -t55)
      t65 = t7 * t8 * t44 * t50 * t59 * t61 * t33 / 0.8D1
      t66 = FJET(XB1, XB2, s, 0.0D0, t43, -t47, t49, -t55, -t65)
      t68 = t6 * t8
      t73 = t44 * t50 * t59 * t61 * t33
      t76 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t37)
      t78 = FJET(XB1, XB2, s, -t47, t49, 0.0D0, t43, -t55, -t65)
      rrgg2qqbarht2s2em1 = t38 * t37 - t66 * pi * t68 * t73 / 0.8D1 + t7
     #6 * t37 - t78 * pi * t68 * t73 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht2s2em2
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
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = t1 ** 2
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t12 = pi * t6 * t8 * t9 / 0.16D2
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t12)
      t16 = t6 * t8 * t9
      t18 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t12)
      rrgg2qqbarht2s2em2 = t13 * pi * t16 / 0.16D2 + t18 * pi * t16 / 0.
     #16D2

      end function



      doubleprecision function rrgg2qqbarht2s2em3
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
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht2s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht2s2em4
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
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht2s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh21J1
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
      t1 = S23 + S13
      t8 = S13 ** 2
      t10 = S23 ** 2
      rrgg2qqbarh21J1 = (-0.11D2 / 0.2D1 * t1 * nf + (-0.275D3 / 0.96D2 
     #* t1 * nf * S34 - (0.306D3 * t8 + 0.306D3 * t10 + 0.122D3 * S23 * 
     #S24 + 0.122D3 * S13 * S14 - 0.17D2 * S13 * S24 - 0.17D2 * S14 * S2
     #3) * nf / 0.96D2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh21J2
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
      t3 = S23 + S13
      t10 = S13 ** 2
      t12 = S23 ** 2
      rrgg2qqbarh21J2 = (nf * S34 / 0.6D1 - 0.19D2 / 0.6D1 * t3 * nf + (
     #-0.197D3 / 0.48D2 * t3 * nf * S34 - (0.422D3 * t10 + 0.422D3 * t12
     # + 0.110D3 * S23 * S24 + 0.110D3 * S13 * S14 + 0.456D3 * S13 * S24
     # + 0.456D3 * S14 * S23) * nf / 0.96D2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh21J3
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
      t16 = S13 ** 2
      t18 = S23 ** 2
      rrgg2qqbarh21J3 = (-nf * S12 / 0.3D1 + nf * S34 / 0.2D1 - (0.16D2 
     #* S14 + 0.16D2 * S24 + 0.74376D5 * S13 + 0.74376D5 * S23) * nf / 0
     #.96D2 + (-(-0.27859D5 * S13 - 0.27859D5 * S23) * nf * S34 / 0.96D2
     # - (0.28762D5 * t16 + 0.28762D5 * t18 - 0.206D3 * S23 * S24 + 0.26
     #149D5 * S14 * S23 - 0.206D3 * S13 * S14 + 0.26149D5 * S13 * S24) *
     # nf / 0.96D2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh21J4
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
      t16 = S13 ** 2
      t18 = S23 ** 2
      rrgg2qqbarh21J4 = (-0.2D1 / 0.3D1 * nf * S12 + 0.5D1 / 0.6D1 * nf 
     #* S34 - (0.32D2 * S14 + 0.32D2 * S24 + 0.148448D6 * S13 + 0.148448
     #D6 * S23) * nf / 0.96D2 + (-(-0.56112D5 * S13 - 0.56112D5 * S23) *
     # nf * S34 / 0.96D2 - (0.57102D5 * t16 + 0.57102D5 * t18 - 0.522D3 
     #* S23 * S24 + 0.51842D5 * S14 * S23 - 0.522D3 * S13 * S14 + 0.5184
     #2D5 * S13 * S24) * nf / 0.96D2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh21J5
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
      t15 = S13 ** 2
      t17 = S23 ** 2
      rrgg2qqbarh21J5 = (-nf * S12 + 0.7D1 / 0.6D1 * nf * S34 - (0.48D2 
     #* S14 + 0.48D2 * S24 + 0.222520D6 * S13 + 0.222520D6 * S23) * nf /
     # 0.96D2 + (-(-0.84365D5 * S13 - 0.84365D5 * S23) * nf * S34 / 0.96
     #D2 - (0.85442D5 * t15 + 0.85442D5 * t17 - 0.838D3 * S23 * S24 + 0.
     #77535D5 * S14 * S23 - 0.838D3 * S13 * S14 + 0.77535D5 * S13 * S24)
     # * nf / 0.96D2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh21J6
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
      t16 = S13 ** 2
      t18 = S23 ** 2
      rrgg2qqbarh21J6 = (-0.4D1 / 0.3D1 * nf * S12 + 0.3D1 / 0.2D1 * nf 
     #* S34 - (0.64D2 * S14 + 0.64D2 * S24 + 0.293424D6 * S13 + 0.293424
     #D6 * S23) * nf / 0.96D2 + (-(-0.114268D6 * S13 - 0.114268D6 * S23)
     # * nf * S34 / 0.96D2 - (0.111946D6 * t16 + 0.111946D6 * t18 - 0.18
     #86D4 * S23 * S24 + 0.103330D6 * S14 * S23 - 0.1886D4 * S13 * S14 +
     # 0.103330D6 * S13 * S24) * nf / 0.96D2) / S12) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh21J7
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
      t24 = S23 ** 2
      t26 = S13 ** 2
      rrgg2qqbarh21J7 = (-0.5D1 / 0.3D1 * nf * S12 + 0.5D1 / 0.6D1 * nf 
     #* S34 - 0.5D1 / 0.96D2 * (0.74296D5 * S13 + 0.16D2 * S24 + 0.74296
     #D5 * S23 + 0.16D2 * S14) * nf + (-0.5D1 / 0.96D2 * (-0.28372D5 * S
     #23 - 0.28372D5 * S13) * nf * S34 - 0.5D1 / 0.96D2 * (0.25220D5 * S
     #13 * S24 + 0.25220D5 * S14 * S23 - 0.304D3 * S13 * S14 - 0.304D3 *
     # S23 * S24 + 0.28224D5 * t24 + 0.28224D5 * t26) * nf) / S12) / pi 
     #* wd / z

      end function
  
 