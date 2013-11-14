  
      subroutine rrgg2gght2
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2ggh21J1  
      doubleprecision rrgg2ggh21J2  
      doubleprecision rrgg2ggh21J3  
      doubleprecision rrgg2ggh21J4  
      doubleprecision rrgg2ggh21J5  
      doubleprecision rrgg2ggh21J6  
      doubleprecision rrgg2ggh21J7  
      doubleprecision rrgg2gght2s1e1  
      doubleprecision rrgg2gght2s1e0  
      doubleprecision rrgg2gght2s1em1  
      doubleprecision rrgg2gght2s1em2  
      doubleprecision rrgg2gght2s1em3  
      doubleprecision rrgg2gght2s1em4  
      doubleprecision rrgg2gght2s2e1  
      doubleprecision rrgg2gght2s2e0  
      doubleprecision rrgg2gght2s2em1  
      doubleprecision rrgg2gght2s2em2  
      doubleprecision rrgg2gght2s2em3  
      doubleprecision rrgg2gght2s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght2s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght2s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght2s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght2s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght2s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght2s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght2s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght2s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght2s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght2s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght2s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght2s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght2s1e1
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
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7

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
      t9 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
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
      t36 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t37 = t8 * t36
      t40 = pi * lh
      t41 = t6 * t8
      t42 = t41 * t9
      t49 = 0.1D1 / x3
      t52 = pi ** 2
      t54 = lh ** 2
      t56 = -0.30D2 * t52 + 0.180D3 * t54
      t57 = pi * t56
      t58 = t13 * t16
      t61 = log(-0.4D1 * t58 * t18)
      t62 = t61 * pi
      t65 = t61 ** 2
      t66 = t65 * pi
      t72 = rrgg2ggh21J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t97 = rrgg2ggh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
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
     #4 + t7 * t8 * t72 / 0.16D2 + (pi * (-0.240D3 * zeta3 - 0.120D3 * t
     #54 * lh + 0.60D2 * lh * t52) - t62 * t56 - 0.90D2 * t66 * lh - 0.1
     #5D2 * t65 * t61 * pi) * t6 * t10 / 0.1440D4 + (-0.180D3 * t40 - 0.
     #90D2 * t62) * t6 * t8 * t97 / 0.1440D4 - t7 * t8 * (-t109 * t9 + t
     #113 * t9) * t49 * t117 / 0.8D1 + (0.90D2 * t7 * t8 * (t97 - t124 *
     # t36 + t126 * t9 / 0.2D1) - 0.180D3 * t40 * t41 * (t36 - t124 * t9
     #) + t57 * t42) * t117 / 0.720D3
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
      t166 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, t150, -t154, 0.
     #0D0, t156, t162)
      t168 = t163 * t165 * t166
      t169 = t103 * t58
      t170 = t151 ** 2
      t171 = t157 ** 2
      t173 = t18 * t170 * t171
      t176 = log(-0.4D1 * t169 * t173)
      t178 = t157 * t165
      t179 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, t150, -t154, 0.
     #0D0, t156, t162)
      t180 = t178 * t179
      t186 = t40 * t41
      t188 = t163 * t165 * t179
      t194 = rrgg2ggh21J3(s, XB1, XB2, z, lh, wd, nf, s, t150, -t154, 0.
     #0D0, t156, t162)
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
      t253 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, -t231, -t234, t
     #233, t235, t249)
      t257 = t236 ** 2
      t262 = log(0.4D1 * t169 * t18 * t170 * t19 * t257)
      t265 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, -t231, -t234, t
     #233, t235, t249)
      t276 = -0.90D2 * t7 * t8 * (t250 * t252 * t253 - t262 * t151 * t23
     #6 * t252 * t265) + 0.180D3 * t186 * t250 * t252 * t265
      t279 = t276 * t49 * t117 / 0.720D3
      t280 = FJET(XB1, XB2, s, -t231, t233, -t234, t235, t249, -t279)
      t282 = t49 * t117
      t285 = FJET(XB1, XB2, s, -t234, t235, -t231, t233, t249, -t279)
      rrgg2gght2s1e1 = t143 * t142 + t145 * t142 + t223 * t222 + t225 * 
     #t222 - t280 * t276 * t282 / 0.720D3 - t285 * t276 * t282 / 0.720D3

      end function



      doubleprecision function rrgg2gght2s1e0
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
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7

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
      t9 = rrgg2ggh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
      t13 = pi * lh
      t15 = x2 * pi
      t16 = sin(t15)
      t17 = t16 ** 2
      t18 = z ** 2
      t19 = 0.1D1 / t18
      t21 = x4 * t4
      t24 = log(-0.4D1 * t17 * t19 * t21)
      t25 = t24 * pi
      t29 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t33 = pi ** 2
      t35 = lh ** 2
      t41 = t24 ** 2
      t46 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
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
     # * t8 * t29 / 0.1440D4 + (pi * (-0.30D2 * t33 + 0.180D3 * t35) + 0
     #.180D3 * t25 * lh + 0.45D2 * t41 * pi) * t6 * t8 * t46 / 0.1440D4 
     #- t7 * t8 * t46 * (-t57 + t62) * t65 / 0.16D2 + (0.90D2 * t7 * t8 
     #* (t29 - t73 * t46) - 0.180D3 * t13 * t79 * t46) * t84 / 0.720D3
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
      t113 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, t95, -t99, 0.0D
     #0, t101, t107)
      t119 = t96 * t102
      t120 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, t95, -t99, 0.0D
     #0, t101, t107)
      t124 = t102 ** 2
      t125 = t96 ** 2
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
      t177 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, -t155, -t158, t
     #157, t159, t173)
      t180 = t160 / (-0.2D1 + t152) * t177 * t65 * t84
      t182 = t109 * t180 / 0.8D1
      t183 = FJET(XB1, XB2, s, -t155, t157, -t158, t159, t173, t182)
      t185 = t79 * t96
      t189 = FJET(XB1, XB2, s, -t158, t159, -t155, t157, t173, t182)
      rrgg2gght2s1e0 = t88 * t87 + t90 * t87 + t147 * t146 + t149 * t146
     # + t183 * pi * t185 * t180 / 0.8D1 + t189 * pi * t185 * t180 / 0.8
     #D1

      end function



      doubleprecision function rrgg2gght2s1em1
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
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7

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
      t9 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
      t16 = sin(x2 * pi)
      t17 = t16 ** 2
      t18 = z ** 2
      t24 = log(-0.4D1 * t17 / t18 * x4 * t4)
      t29 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
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
      t63 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, t45, -t49, 0.0D0
     #, t51, t57)
      t67 = t7 * t8 * t46 * t52 * t61 * t63 * t33 / 0.8D1
      t68 = FJET(XB1, XB2, s, t45, 0.0D0, -t49, t51, t57, -t67)
      t70 = t6 * t8
      t75 = t46 * t52 * t61 * t63 * t33
      t78 = FJET(XB1, XB2, s, -t49, t51, t45, 0.0D0, t57, -t67)
      rrgg2gght2s1em1 = t38 * t37 + t40 * t37 - t68 * pi * t70 * t75 / 0
     #.8D1 - t78 * pi * t70 * t75 / 0.8D1

      end function



      doubleprecision function rrgg2gght2s1em2
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
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7

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
      t9 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
      t12 = pi * t6 * t8 * t9 / 0.16D2
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t12)
      t16 = t6 * t8 * t9
      t18 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t12)
      rrgg2gght2s1em2 = t13 * pi * t16 / 0.16D2 + t18 * pi * t16 / 0.16D
     #2

      end function



      doubleprecision function rrgg2gght2s1em3
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
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght2s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght2s1em4
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
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght2s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght2s2e1
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
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7

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
      t9 = x2 * pi
      t10 = sin(t9)
      t11 = t10 ** 2
      t12 = x3 * t11
      t13 = z ** 2
      t14 = 0.1D1 / t13
      t16 = x4 * t4
      t17 = -0.1D1 + x3
      t21 = log(0.4D1 * t12 * t14 * t16 * t17)
      t22 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t24 = t21 ** 2
      t25 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t28 = t14 * x4
      t29 = t28 * t4
      t32 = log(-0.4D1 * t12 * t29)
      t34 = t32 ** 2
      t41 = pi * lh
      t42 = t6 * t8
      t50 = 0.1D1 / x3
      t53 = pi ** 2
      t55 = lh ** 2
      t57 = -0.30D2 * t53 + 0.180D3 * t55
      t58 = pi * t57
      t59 = t11 * t14
      t62 = log(-0.4D1 * t59 * t16)
      t63 = t62 * pi
      t66 = t62 ** 2
      t67 = t66 * pi
      t74 = rrgg2ggh21J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t100 = rrgg2ggh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D
     #0, -t5, 0.0D0)
      t105 = x1 ** 2
      t106 = x3 * t105
      t107 = t106 * t11
      t112 = log(0.4D1 * t107 * t28 * t4 * t17)
      t116 = log(-0.4D1 * t107 * t29)
      t120 = 0.1D1 / x1
      t124 = t105 * t11
      t127 = log(-0.4D1 * t124 * t29)
      t129 = t127 ** 2
      t146 = -(0.90D2 * t7 * t8 * (-t21 * t22 + t24 * t25 / 0.2D1 + t32 
     #* t22 - t34 * t25 / 0.2D1) - 0.180D3 * t41 * t42 * (-t21 * t25 + t
     #32 * t25)) * t50 / 0.1440D4 + (t58 + 0.180D3 * t63 * lh + 0.45D2 *
     # t67) * t6 * t8 * t22 / 0.1440D4 + t7 * t8 * t74 / 0.16D2 + (pi * 
     #(-0.240D3 * zeta3 - 0.120D3 * t55 * lh + 0.60D2 * lh * t53) - t63 
     #* t57 - 0.90D2 * t67 * lh - 0.15D2 * t66 * t62 * pi) * t6 * t8 * t
     #25 / 0.1440D4 + (-0.180D3 * t41 - 0.90D2 * t63) * t6 * t8 * t100 /
     # 0.1440D4 - t7 * t8 * (-t112 * t25 + t116 * t25) * t50 * t120 / 0.
     #8D1 + (0.90D2 * t7 * t8 * (t100 - t127 * t22 + t129 * t25 / 0.2D1)
     # - 0.180D3 * t41 * t42 * (t22 - t127 * t25) + t58 * t42 * t25) * t
     #120 / 0.720D3
      t147 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t146)
      t149 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t150 = s * t149
      t151 = t1 * x1
      t152 = t150 * t151
      t153 = -0.1D1 + x1
      t154 = t1 * t153
      t155 = t154 * x4
      t156 = t150 * t155
      t157 = t154 * t4
      t158 = t150 * t157
      t159 = t149 ** 2
      t162 = x1 * t153
      t164 = s * t159 * t6 * t162 * x4
      t165 = t153 * t159
      t167 = 0.1D1 / (-0.2D1 + t149)
      t168 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t156, t
     #152, t158, -t164)
      t170 = t165 * t167 * t168
      t171 = t106 * t59
      t172 = t153 ** 2
      t173 = t159 ** 2
      t175 = t16 * t172 * t173
      t178 = log(-0.4D1 * t171 * t175)
      t180 = t159 * t167
      t181 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t156, t
     #152, t158, -t164)
      t182 = t180 * t181
      t188 = t41 * t42
      t190 = t165 * t167 * t181
      t195 = (0.90D2 * t7 * t8 * (t170 - t178 * t153 * t182) - 0.180D3 *
     # t188 * t190) * t50 * t120
      t196 = rrgg2ggh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t156, t
     #152, t158, -t164)
      t202 = log(-0.4D1 * t124 * t14 * t175)
      t203 = t202 * t153
      t206 = t202 ** 2
      t210 = -t165 * t167 * t196 + t203 * t180 * t168 - t206 * t153 * t1
     #82 / 0.2D1
      t215 = -t170 + t203 * t182
      t220 = t58 * t42 * t190
      t224 = -t195 / 0.720D3 + (0.90D2 * t7 * t8 * t210 - 0.180D3 * t41 
     #* t42 * t215 - t220) * t120 / 0.720D3
      t225 = FJET(XB1, XB2, s, 0.0D0, t152, -t156, t158, -t164, t224)
      t227 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t146)
      t229 = KAPPA2(x1, x2, x3, x4, z)
      t230 = s * t229
      t232 = t230 * t151 * x3
      t234 = t230 * t151 * t17
      t235 = t230 * t155
      t236 = t230 * t157
      t237 = t229 ** 2
      t242 = cos(t9)
      t245 = Sqrt(x3 * t17 * t16)
      t250 = s * t237 * t6 * t162 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t242 * t245)
      t251 = t153 * t237
      t253 = 0.1D1 / (-0.2D1 + t229)
      t254 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, t232, -t235, -t
     #234, t236, t250)
      t258 = t237 ** 2
      t263 = log(0.4D1 * t171 * t16 * t172 * t17 * t258)
      t266 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, t232, -t235, -t
     #234, t236, t250)
      t269 = -t251 * t253 * t254 + t263 * t153 * t237 * t253 * t266
      t276 = 0.180D3 * t188 * t251 * t253 * t266
      t277 = 0.90D2 * t7 * t8 * t269 + t276
      t281 = FJET(XB1, XB2, s, t232, -t234, -t235, t236, t250, -t277 * t
     #50 * t120 / 0.720D3)
      t283 = t50 * t120
      t297 = -t195 / 0.720D3 + (0.90D2 * t7 * t8 * t210 - 0.180D3 * t41 
     #* t42 * t215 - t220) * t120 / 0.720D3
      t298 = FJET(XB1, XB2, s, -t156, t158, 0.0D0, t152, -t164, t297)
      t304 = 0.90D2 * t7 * t8 * t269 + t276
      t308 = FJET(XB1, XB2, s, -t235, t236, t232, -t234, t250, -t304 * t
     #50 * t120 / 0.720D3)
      rrgg2gght2s2e1 = t147 * t146 + t225 * t224 + t227 * t146 - t281 * 
     #t277 * t283 / 0.720D3 + t298 * t297 - t308 * t304 * t283 / 0.720D3

      end function



      doubleprecision function rrgg2gght2s2e0
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
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7

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
      t9 = rrgg2ggh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
      t13 = pi * lh
      t15 = x2 * pi
      t16 = sin(t15)
      t17 = t16 ** 2
      t18 = z ** 2
      t19 = 0.1D1 / t18
      t21 = x4 * t4
      t24 = log(-0.4D1 * t17 * t19 * t21)
      t25 = t24 * pi
      t29 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t33 = pi ** 2
      t35 = lh ** 2
      t41 = t24 ** 2
      t46 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
      t50 = x3 * t17
      t52 = -0.1D1 + x3
      t56 = log(0.4D1 * t50 * t19 * t21 * t52)
      t59 = t19 * x4 * t4
      t62 = log(-0.4D1 * t50 * t59)
      t66 = 0.1D1 / x3
      t70 = x1 ** 2
      t71 = t70 * t17
      t74 = log(-0.4D1 * t71 * t59)
      t80 = t6 * t8
      t85 = 0.1D1 / x1
      t88 = t7 * t8 * t9 / 0.16D2 + (-0.180D3 * t13 - 0.90D2 * t25) * t6
     # * t8 * t29 / 0.1440D4 + (pi * (-0.30D2 * t33 + 0.180D3 * t35) + 0
     #.180D3 * t25 * lh + 0.45D2 * t41 * pi) * t6 * t8 * t46 / 0.1440D4 
     #- t7 * t8 * (-t56 * t46 + t62 * t46) * t66 / 0.16D2 + (0.90D2 * t7
     # * t8 * (t29 - t74 * t46) - 0.180D3 * t13 * t80 * t46) * t85 / 0.7
     #20D3
      t89 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t88)
      t91 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t92 = s * t91
      t93 = t1 * x1
      t94 = t92 * t93
      t95 = -0.1D1 + x1
      t96 = t1 * t95
      t97 = t96 * x4
      t98 = t92 * t97
      t99 = t96 * t4
      t100 = t92 * t99
      t101 = t91 ** 2
      t104 = x1 * t95
      t106 = s * t101 * t6 * t104 * x4
      t108 = t7 * t8 * t95
      t110 = 0.1D1 / (-0.2D1 + t91)
      t111 = t101 * t110
      t112 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t98, t9
     #4, t100, -t106)
      t117 = t108 * t111 * t112 * t66 * t85 / 0.8D1
      t118 = t95 * t101
      t119 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t98, t9
     #4, t100, -t106)
      t123 = t101 ** 2
      t124 = t95 ** 2
      t129 = log(-0.4D1 * t71 * t19 * t21 * t123 * t124)
      t133 = -t118 * t110 * t119 + t129 * t95 * t111 * t112
      t141 = 0.180D3 * t13 * t80 * t118 * t110 * t112
      t145 = -t117 + (0.90D2 * t7 * t8 * t133 + t141) * t85 / 0.720D3
      t146 = FJET(XB1, XB2, s, 0.0D0, t94, -t98, t100, -t106, t145)
      t148 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t88)
      t150 = KAPPA2(x1, x2, x3, x4, z)
      t151 = s * t150
      t153 = t151 * t93 * x3
      t155 = t151 * t93 * t52
      t156 = t151 * t97
      t157 = t151 * t99
      t158 = t150 ** 2
      t163 = cos(t15)
      t166 = Sqrt(x3 * t52 * t21)
      t171 = s * t158 * t6 * t104 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t163 * t166)
      t175 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, t153, -t156, -t
     #155, t157, t171)
      t178 = t158 / (-0.2D1 + t150) * t175 * t66 * t85
      t180 = t108 * t178 / 0.8D1
      t181 = FJET(XB1, XB2, s, t153, -t155, -t156, t157, t171, t180)
      t183 = t80 * t95
      t194 = -t117 + (0.90D2 * t7 * t8 * t133 + t141) * t85 / 0.720D3
      t195 = FJET(XB1, XB2, s, -t98, t100, 0.0D0, t94, -t106, t194)
      t197 = FJET(XB1, XB2, s, -t156, t157, t153, -t155, t171, t180)
      rrgg2gght2s2e0 = t89 * t88 + t146 * t145 + t148 * t88 + t181 * pi 
     #* t183 * t178 / 0.8D1 + t195 * t194 + t197 * pi * t183 * t178 / 0.
     #8D1

      end function



      doubleprecision function rrgg2gght2s2em1
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
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7

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
      t9 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
      t16 = sin(x2 * pi)
      t17 = t16 ** 2
      t18 = z ** 2
      t24 = log(-0.4D1 * t17 / t18 * x4 * t4)
      t29 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0
     #, -t5, 0.0D0)
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
      t61 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t47, t43
     #, t49, -t55)
      t65 = t7 * t8 * t44 * t50 * t59 * t61 * t33 / 0.8D1
      t66 = FJET(XB1, XB2, s, 0.0D0, t43, -t47, t49, -t55, -t65)
      t68 = t6 * t8
      t73 = t44 * t50 * t59 * t61 * t33
      t76 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t37)
      t78 = FJET(XB1, XB2, s, -t47, t49, 0.0D0, t43, -t55, -t65)
      rrgg2gght2s2em1 = t38 * t37 - t66 * pi * t68 * t73 / 0.8D1 + t76 *
     # t37 - t78 * pi * t68 * t73 / 0.8D1

      end function



      doubleprecision function rrgg2gght2s2em2
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
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7

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
      t9 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0D0,
     # -t5, 0.0D0)
      t12 = pi * t6 * t8 * t9 / 0.16D2
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t12)
      t16 = t6 * t8 * t9
      t18 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t12)
      rrgg2gght2s2em2 = t13 * pi * t16 / 0.16D2 + t18 * pi * t16 / 0.16D
     #2

      end function



      doubleprecision function rrgg2gght2s2em3
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
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght2s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght2s2em4
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
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght2s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh21J1
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
      t1 = S13 + S23
      t2 = 0.216D3 * t1
      t4 = 0.1D1 / (S12 + S13 + S23)
      t6 = 0.1D1 / S12
      t7 = s ** 2
      t9 = z ** 2
      t17 = S13 * S24
      t18 = S14 * S23
      t26 = 0.1D1 / (S12 + S14 + S24)
      t28 = S34 ** 2
      t30 = S23 * S24
      t32 = S23 ** 2
      t35 = S13 ** 2
      t38 = S13 * S14
      t43 = t38 * S24
      t44 = 0.9D1 * t43
      t45 = t18 * S24
      t46 = 0.9D1 * t45
      t47 = t35 * S13
      t49 = S14 ** 2
      t50 = t49 * S23
      t51 = 0.9D1 / 0.2D1 * t50
      t52 = S24 ** 2
      t53 = S13 * t52
      t54 = 0.9D1 / 0.2D1 * t53
      t55 = t35 * S24
      t57 = S13 * t49
      t58 = 0.9D1 / 0.2D1 * t57
      t59 = t35 * S14
      t61 = S14 * t32
      t63 = t32 * S24
      t65 = t52 * S23
      t66 = 0.9D1 / 0.2D1 * t65
      t67 = t32 * S23
      t69 = -t44 - t46 + 0.18D2 * t47 + t51 + t54 - 0.27D2 / 0.2D1 * t55
     # + t58 + 0.27D2 / 0.2D1 * t59 - 0.27D2 / 0.2D1 * t61 + 0.27D2 / 0.
     #2D1 * t63 + t66 + 0.18D2 * t67
      t72 = S12 ** 2
      t73 = 0.1D1 / t72
      t82 = -t1
      t91 = -t18 - t17 + t38 + t30
      t113 = 0.549D3 / 0.2D1 * S13
      t116 = 0.549D3 / 0.2D1 * S23
      t139 = 0.18D2 * t82
      t198 = -0.603D3 / 0.16D2 * t45 - 0.333D3 / 0.8D1 * t50 + 0.1845D4 
     #/ 0.16D2 * t63 + 0.1197D4 / 0.16D2 * t55 + 0.1845D4 / 0.16D2 * t59
     # - 0.333D3 / 0.8D1 * t53 + 0.63D2 / 0.16D2 * t57 + 0.27D2 * t47 - 
     #0.603D3 / 0.16D2 * t43 + 0.1197D4 / 0.16D2 * t61 + 0.63D2 / 0.16D2
     # * t65 + 0.27D2 * t67
      t210 = t49 * S14
      t213 = t52 * S24
      t230 = 0.9D1 * t61 - 0.171D3 / 0.8D1 * t50 - 0.45D2 / 0.16D2 * t67
     # - 0.585D3 / 0.4D1 * t57 + 0.9D1 * t55 - 0.45D2 / 0.16D2 * t47 - 0
     #.171D3 / 0.8D1 * t53 - 0.99D2 * t43 - 0.99D2 * t45 - 0.585D3 / 0.4
     #D1 * t65 + (0.9D1 / 0.8D1 * S13 * t210 - 0.9D1 / 0.8D1 * S13 * t21
     #3 - 0.27D2 / 0.8D1 * t18 * t52 - 0.27D2 / 0.8D1 * t57 * S24 + 0.27
     #D2 / 0.8D1 * t50 * S24 + 0.27D2 / 0.8D1 * t38 * t52 - 0.9D1 / 0.8D
     #1 * S23 * t210 + 0.9D1 / 0.8D1 * S23 * t213) * t26
      t232 = t139 * t26 * t4 * t28 * S34 + (0.18D2 + 0.27D2 * t1 * t26 +
     # (-0.171D3 / 0.8D1 * S13 - 0.171D3 / 0.8D1 * S23 + 0.27D2 / 0.2D1 
     #* t91 * t26) * t4) * t28 + (0.72D2 * S14 + t113 + 0.72D2 * S24 + t
     #116 + (0.54D2 * t35 + 0.1197D4 / 0.16D2 * t18 + 0.1845D4 / 0.16D2 
     #* t38 + 0.1845D4 / 0.16D2 * t30 + 0.1197D4 / 0.16D2 * t17 + 0.54D2
     # * t32) * t26 + (-0.99D2 * t30 + 0.9D1 * t17 - 0.99D2 * t38 + 0.9D
     #1 * t18 - 0.45D2 * t32 - 0.45D2 * t35 + (-t54 - t51 - t66 + t46 - 
     #t58 + t44) * t26) * t4) * S34 - 0.387D3 / 0.8D1 * t30 + 0.18D2 * t
     #52 + 0.54D2 * S14 * S24 + 0.549D3 / 0.2D1 * t18 + 0.549D3 / 0.2D1 
     #* t32 - 0.387D3 / 0.8D1 * t38 + 0.549D3 / 0.2D1 * t17 + 0.18D2 * t
     #49 + 0.549D3 / 0.2D1 * t35 + t198 * t26 + t230 * t4
      t256 = -t139 * t28 + (0.54D2 * t30 + 0.72D2 * t17 + 0.54D2 * t38 +
     # 0.72D2 * t18 + 0.63D2 * t35 + 0.63D2 * t32) * S34 + 0.90D2 * t61 
     #+ 0.72D2 * t45 + 0.72D2 * t43 + 0.18D2 * t50 + 0.63D2 * t63 + 0.9D
     #1 * t67 + 0.18D2 * t53 + 0.9D1 * t47 + 0.90D2 * t55 + 0.63D2 * t59
     # + 0.18D2 * t65 + 0.18D2 * t57
      t258 = t2 * t4 * t6 * t7 * t9 + ((-t2 * t4 * S34 - 0.432D3 * S13 -
     # 0.432D3 * S23 + (-0.216D3 * t17 - 0.216D3 * t18) * t4) * t6 + (0.
     #9D1 * t1 * t26 * t28 + (-0.9D1 / 0.2D1 * t30 + 0.9D1 * t32 + 0.9D1
     # / 0.2D1 * t18 + 0.9D1 * t35 + 0.9D1 / 0.2D1 * t17 - 0.9D1 / 0.2D1
     # * t38) * t26 * S34 + t69 * t26) * t73) * s * z + 0.189D3 / 0.8D1 
     #* t1 * t26 * t4 * t72 + (0.117D3 / 0.2D1 * t82 * t26 * t4 * S34 + 
     #0.9D1 + 0.333D3 / 0.8D1 * t82 * t26 + (-0.45D2 / 0.16D2 * S13 - 0.
     #45D2 / 0.16D2 * S23 + 0.63D2 / 0.8D1 * t91 * t26) * t4) * S12 + 0.
     #135D3 / 0.2D1 * t1 * t26 * t4 * t28 + (0.90D2 + 0.1197D4 / 0.16D2 
     #* t1 * t26 + (0.9D1 * S13 + 0.9D1 * S23 - 0.27D2 * t91 * t26) * t4
     #) * S34 + t113 + 0.63D2 * S24 + 0.63D2 * S14 + t116 + (0.1197D4 / 
     #0.16D2 * t32 + 0.1197D4 / 0.16D2 * t35 - 0.333D3 / 0.4D1 * t18 - 0
     #.603D3 / 0.16D2 * t38 - 0.333D3 / 0.4D1 * t17 - 0.603D3 / 0.16D2 *
     # t30) * t26 + (0.333D3 / 0.8D1 * t32 - 0.45D2 * t18 + 0.333D3 / 0.
     #8D1 * t35 - 0.45D2 * t17 + (0.27D2 / 0.8D1 * t50 + 0.27D2 / 0.8D1 
     #* t53 + 0.27D2 / 0.8D1 * t65 + 0.27D2 / 0.8D1 * t57 - 0.27D2 / 0.4
     #D1 * t45 - 0.27D2 / 0.4D1 * t43) * t26) * t4 + t232 * t6 + t256 * 
     #t73
      rrgg2ggh21J1 = t258 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh21J2
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
      t1 = S13 + S23
      t2 = 0.216D3 * t1
      t4 = 0.1D1 / (S12 + S13 + S23)
      t6 = 0.1D1 / S12
      t7 = s ** 2
      t9 = z ** 2
      t17 = S13 * S24
      t18 = S14 * S23
      t26 = 0.1D1 / (S12 + S14 + S24)
      t28 = S34 ** 2
      t30 = S23 * S24
      t32 = S23 ** 2
      t35 = S13 ** 2
      t38 = S13 * S14
      t43 = t32 * S23
      t45 = S14 ** 2
      t46 = S13 * t45
      t47 = 0.9D1 / 0.2D1 * t46
      t48 = t35 * S13
      t50 = t45 * S23
      t51 = 0.9D1 / 0.2D1 * t50
      t52 = S24 ** 2
      t53 = t52 * S23
      t54 = 0.9D1 / 0.2D1 * t53
      t55 = t38 * S24
      t56 = 0.9D1 * t55
      t57 = t18 * S24
      t58 = 0.9D1 * t57
      t59 = S13 * t52
      t60 = 0.9D1 / 0.2D1 * t59
      t64 = S12 ** 2
      t65 = 0.1D1 / t64
      t74 = -t1
      t83 = -t18 - t17 + t38 + t30
      t105 = 0.3411D4 / 0.16D2 * S13
      t108 = 0.3411D4 / 0.16D2 * S23
      t131 = 0.18D2 * t74
      t180 = t32 * S24
      t182 = t35 * S24
      t184 = t35 * S14
      t190 = S14 * t32
      t194 = -0.657D3 / 0.8D1 * t57 - 0.243D3 / 0.4D1 * t50 + 0.693D3 / 
     #0.4D1 * t180 + 0.531D3 / 0.4D1 * t182 + 0.693D3 / 0.4D1 * t184 - 0
     #.243D3 / 0.4D1 * t59 - 0.171D3 / 0.8D1 * t46 - 0.27D2 / 0.2D1 * t4
     #8 - 0.657D3 / 0.8D1 * t55 + 0.531D3 / 0.4D1 * t190 - 0.171D3 / 0.8
     #D1 * t53 - 0.27D2 / 0.2D1 * t43
      t206 = t45 * S14
      t209 = t52 * S24
      t226 = 0.9D1 * t190 - 0.171D3 / 0.8D1 * t50 - 0.45D2 / 0.16D2 * t4
     #3 - 0.585D3 / 0.4D1 * t46 + 0.9D1 * t182 - 0.45D2 / 0.16D2 * t48 -
     # 0.171D3 / 0.8D1 * t59 - 0.99D2 * t55 - 0.99D2 * t57 - 0.585D3 / 0
     #.4D1 * t53 + (0.9D1 / 0.8D1 * S13 * t206 - 0.9D1 / 0.8D1 * S13 * t
     #209 - 0.27D2 / 0.8D1 * t18 * t52 - 0.27D2 / 0.8D1 * t46 * S24 + 0.
     #27D2 / 0.8D1 * t50 * S24 + 0.27D2 / 0.8D1 * t38 * t52 - 0.9D1 / 0.
     #8D1 * S23 * t206 + 0.9D1 / 0.8D1 * S23 * t209) * t26
      t228 = t131 * t26 * t4 * t28 * S34 + (0.18D2 + 0.27D2 / 0.2D1 * t7
     #4 * t26 + (-0.171D3 / 0.8D1 * S13 - 0.171D3 / 0.8D1 * S23 + 0.27D2
     # / 0.2D1 * t83 * t26) * t4) * t28 + (0.72D2 * S14 + t105 + 0.72D2 
     #* S24 + t108 + (-0.27D2 * t35 + 0.531D3 / 0.4D1 * t18 + 0.693D3 / 
     #0.4D1 * t38 + 0.693D3 / 0.4D1 * t30 + 0.531D3 / 0.4D1 * t17 - 0.27
     #D2 * t32) * t26 + (-0.99D2 * t30 + 0.9D1 * t17 - 0.99D2 * t38 + 0.
     #9D1 * t18 - 0.45D2 * t32 - 0.45D2 * t35 + (-t60 - t51 - t54 + t58 
     #- t47 + t56) * t26) * t4) * S34 + 0.9D1 / 0.4D1 * t30 + 0.18D2 * t
     #52 + 0.54D2 * S14 * S24 + 0.3411D4 / 0.16D2 * t18 + 0.3411D4 / 0.1
     #6D2 * t32 + 0.9D1 / 0.4D1 * t38 + 0.3411D4 / 0.16D2 * t17 + 0.18D2
     # * t45 + 0.3411D4 / 0.16D2 * t35 + t194 * t26 + t226 * t4
      t250 = -t131 * t28 + (0.54D2 * t30 + 0.54D2 * t32 + 0.72D2 * t17 +
     # 0.54D2 * t38 + 0.72D2 * t18 + 0.54D2 * t35) * S34 + 0.81D2 * t190
     # + 0.72D2 * t57 + 0.72D2 * t55 + 0.18D2 * t50 + 0.54D2 * t180 + 0.
     #54D2 * t184 + 0.18D2 * t59 + 0.81D2 * t182 + 0.18D2 * t53 + 0.18D2
     # * t46
      t252 = t2 * t4 * t6 * t7 * t9 + ((-t2 * t4 * S34 - 0.432D3 * S13 -
     # 0.432D3 * S23 + (-0.216D3 * t17 - 0.216D3 * t18) * t4) * t6 + (0.
     #9D1 * t1 * t26 * t28 + (-0.9D1 / 0.2D1 * t30 + 0.9D1 * t32 + 0.9D1
     # / 0.2D1 * t18 + 0.9D1 * t35 + 0.9D1 / 0.2D1 * t17 - 0.9D1 / 0.2D1
     # * t38) * t26 * S34 + (0.18D2 * t43 + t47 + 0.18D2 * t48 + t51 + t
     #54 - t56 - t58 + t60) * t26) * t65) * s * z + 0.189D3 / 0.8D1 * t1
     # * t26 * t4 * t64 + (0.117D3 / 0.2D1 * t74 * t26 * t4 * S34 + 0.24
     #3D3 / 0.4D1 * t74 * t26 + (-0.45D2 / 0.16D2 * S13 - 0.45D2 / 0.16D
     #2 * S23 + 0.63D2 / 0.8D1 * t83 * t26) * t4) * S12 + 0.135D3 / 0.2D
     #1 * t1 * t26 * t4 * t28 + (0.81D2 + 0.531D3 / 0.4D1 * t1 * t26 + (
     #0.9D1 * S13 + 0.9D1 * S23 - 0.27D2 * t83 * t26) * t4) * S34 + t105
     # + 0.54D2 * S24 + 0.54D2 * S14 + t108 + (0.531D3 / 0.4D1 * t32 + 0
     #.531D3 / 0.4D1 * t35 - 0.243D3 / 0.2D1 * t18 - 0.657D3 / 0.8D1 * t
     #38 - 0.243D3 / 0.2D1 * t17 - 0.657D3 / 0.8D1 * t30) * t26 + (0.333
     #D3 / 0.8D1 * t32 - 0.45D2 * t18 + 0.333D3 / 0.8D1 * t35 - 0.45D2 *
     # t17 + (0.27D2 / 0.8D1 * t50 + 0.27D2 / 0.8D1 * t59 + 0.27D2 / 0.8
     #D1 * t53 + 0.27D2 / 0.8D1 * t46 - 0.27D2 / 0.4D1 * t57 - 0.27D2 / 
     #0.4D1 * t55) * t26) * t4 + t228 * t6 + t250 * t65
      rrgg2ggh21J2 = t252 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh21J3
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
      t1 = S13 + S23
      t2 = 0.216D3 * t1
      t4 = 0.1D1 / (S12 + S13 + S23)
      t6 = 0.1D1 / S12
      t7 = s ** 2
      t9 = z ** 2
      t17 = S13 * S24
      t18 = S14 * S23
      t26 = 0.1D1 / (S12 + S14 + S24)
      t28 = S34 ** 2
      t30 = S23 * S24
      t32 = S23 ** 2
      t35 = S13 ** 2
      t38 = S13 * S14
      t43 = t32 * S23
      t45 = t35 * S13
      t47 = S14 ** 2
      t48 = t47 * S23
      t49 = 0.9D1 / 0.2D1 * t48
      t50 = t35 * S24
      t52 = t32 * S24
      t54 = t35 * S14
      t56 = t38 * S24
      t57 = 0.9D1 * t56
      t58 = S13 * t47
      t59 = 0.9D1 / 0.2D1 * t58
      t60 = S24 ** 2
      t61 = t60 * S23
      t62 = 0.9D1 / 0.2D1 * t61
      t63 = t18 * S24
      t64 = 0.9D1 * t63
      t65 = S14 * t32
      t67 = S13 * t60
      t68 = 0.9D1 / 0.2D1 * t67
      t69 = 0.18D2 * t43 + 0.18D2 * t45 + t49 + 0.27D2 / 0.2D1 * t50 - 0
     #.27D2 / 0.2D1 * t52 - 0.27D2 / 0.2D1 * t54 - t57 + t59 + t62 - t64
     # + 0.27D2 / 0.2D1 * t65 + t68
      t72 = S12 ** 2
      t73 = 0.1D1 / t72
      t82 = -t1
      t91 = -t18 - t17 + t38 + t30
      t115 = 0.1431D4 / 0.8D1 * S13
      t116 = 0.1431D4 / 0.8D1 * S23
      t139 = 0.18D2 * t82
      t198 = -0.54D2 * t45 + 0.3339D4 / 0.16D2 * t50 - 0.27D2 / 0.16D2 *
     # t61 - 0.54D2 * t43 - 0.27D2 / 0.16D2 * t58 - 0.927D3 / 0.8D1 * t6
     #7 + 0.4131D4 / 0.16D2 * t54 + 0.4131D4 / 0.16D2 * t52 - 0.2169D4 /
     # 0.16D2 * t63 + 0.3339D4 / 0.16D2 * t65 - 0.2169D4 / 0.16D2 * t56 
     #- 0.927D3 / 0.8D1 * t48
      t210 = t47 * S14
      t213 = t60 * S24
      t230 = -0.585D3 / 0.4D1 * t61 - 0.585D3 / 0.4D1 * t58 - 0.99D2 * t
     #63 + 0.243D3 / 0.16D2 * t45 - 0.99D2 * t56 + 0.18D2 * t50 - 0.171D
     #3 / 0.8D1 * t67 + 0.18D2 * t65 - 0.171D3 / 0.8D1 * t48 + 0.243D3 /
     # 0.16D2 * t43 + (0.9D1 / 0.8D1 * S13 * t210 - 0.9D1 / 0.8D1 * S13 
     #* t213 - 0.27D2 / 0.8D1 * t18 * t60 - 0.27D2 / 0.8D1 * t58 * S24 +
     # 0.27D2 / 0.8D1 * t48 * S24 + 0.27D2 / 0.8D1 * t38 * t60 - 0.9D1 /
     # 0.8D1 * S23 * t210 + 0.9D1 / 0.8D1 * S23 * t213) * t26
      t232 = t139 * t26 * t4 * t28 * S34 + (0.18D2 + 0.54D2 * t82 * t26 
     #+ (-0.171D3 / 0.8D1 * S13 - 0.171D3 / 0.8D1 * S23 + 0.27D2 / 0.2D1
     # * t91 * t26) * t4) * t28 + (0.72D2 * S24 + 0.72D2 * S14 + t116 + 
     #t115 + (0.3627D4 / 0.16D2 * t18 + 0.4131D4 / 0.16D2 * t38 + 0.3627
     #D4 / 0.16D2 * t17 - 0.108D3 * t32 + 0.4131D4 / 0.16D2 * t30 - 0.10
     #8D3 * t35) * t26 + (-0.54D2 * t35 - 0.54D2 * t32 + 0.9D1 * t17 - 0
     #.99D2 * t38 - 0.99D2 * t30 + 0.9D1 * t18 + (-t68 - t49 - t62 + t64
     # - t59 + t57) * t26) * t4) * S34 + 0.1431D4 / 0.8D1 * t32 + 0.1431
     #D4 / 0.8D1 * t17 + 0.1431D4 / 0.8D1 * t18 + 0.54D2 * S14 * S24 - 0
     #.297D3 / 0.8D1 * t38 + 0.18D2 * t47 - 0.297D3 / 0.8D1 * t30 + 0.18
     #D2 * t60 + 0.1431D4 / 0.8D1 * t35 + t198 * t26 + t230 * t4
      t256 = -t139 * t28 + (0.54D2 * t30 + 0.72D2 * t17 + 0.54D2 * t38 +
     # 0.72D2 * t18 + 0.45D2 * t35 + 0.45D2 * t32) * S34 + 0.72D2 * t65 
     #+ 0.72D2 * t63 + 0.72D2 * t56 + 0.18D2 * t48 + 0.45D2 * t52 - 0.9D
     #1 * t43 + 0.18D2 * t67 - 0.9D1 * t45 + 0.72D2 * t50 + 0.45D2 * t54
     # + 0.18D2 * t61 + 0.18D2 * t58
      t258 = t2 * t4 * t6 * t7 * t9 + ((-t2 * t4 * S34 - 0.432D3 * S13 -
     # 0.432D3 * S23 + (-0.216D3 * t17 - 0.216D3 * t18) * t4) * t6 + (0.
     #9D1 * t1 * t26 * t28 + (-0.9D1 / 0.2D1 * t30 + 0.9D1 * t32 + 0.9D1
     # / 0.2D1 * t18 + 0.9D1 * t35 + 0.9D1 / 0.2D1 * t17 - 0.9D1 / 0.2D1
     # * t38) * t26 * S34 + t69 * t26) * t73) * s * z + 0.333D3 / 0.8D1 
     #* t1 * t26 * t4 * t72 + (0.189D3 / 0.2D1 * t82 * t26 * t4 * S34 - 
     #0.9D1 + 0.927D3 / 0.8D1 * t82 * t26 + (0.243D3 / 0.16D2 * S23 + 0.
     #243D3 / 0.16D2 * S13 + 0.63D2 / 0.8D1 * t91 * t26) * t4) * S12 + 0
     #.171D3 / 0.2D1 * t1 * t26 * t4 * t28 + (0.72D2 + 0.3339D4 / 0.16D2
     # * t1 * t26 + (0.18D2 * S13 + 0.18D2 * S23 - 0.27D2 * t91 * t26) *
     # t4) * S34 + 0.45D2 * S24 + 0.45D2 * S14 + t115 + t116 + (-0.1215D
     #4 / 0.4D1 * t17 - 0.2169D4 / 0.16D2 * t38 + 0.3627D4 / 0.16D2 * t3
     #2 - 0.1215D4 / 0.4D1 * t18 - 0.2169D4 / 0.16D2 * t30 + 0.3627D4 / 
     #0.16D2 * t35) * t26 + (-0.54D2 * t18 + 0.621D3 / 0.8D1 * t32 - 0.5
     #4D2 * t17 + 0.621D3 / 0.8D1 * t35 + (0.27D2 / 0.8D1 * t48 + 0.27D2
     # / 0.8D1 * t67 + 0.27D2 / 0.8D1 * t61 + 0.27D2 / 0.8D1 * t58 - 0.2
     #7D2 / 0.4D1 * t63 - 0.27D2 / 0.4D1 * t56) * t26) * t4 + t232 * t6 
     #+ t256 * t73
      rrgg2ggh21J3 = t258 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh21J4
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
      t1 = S13 + S23
      t2 = 0.216D3 * t1
      t4 = 0.1D1 / (S12 + S13 + S23)
      t6 = 0.1D1 / S12
      t7 = s ** 2
      t9 = z ** 2
      t17 = S13 * S24
      t18 = S14 * S23
      t26 = 0.1D1 / (S12 + S14 + S24)
      t28 = S34 ** 2
      t30 = S23 * S24
      t32 = S23 ** 2
      t35 = S13 ** 2
      t38 = S13 * S14
      t43 = t32 * S23
      t44 = 0.18D2 * t43
      t45 = t35 * S13
      t46 = 0.18D2 * t45
      t47 = S14 ** 2
      t48 = t47 * S23
      t49 = 0.9D1 / 0.2D1 * t48
      t50 = t35 * S24
      t51 = 0.27D2 * t50
      t52 = t32 * S24
      t54 = t35 * S14
      t56 = t38 * S24
      t57 = 0.9D1 * t56
      t58 = S13 * t47
      t59 = 0.9D1 / 0.2D1 * t58
      t60 = S24 ** 2
      t61 = t60 * S23
      t62 = 0.9D1 / 0.2D1 * t61
      t63 = t18 * S24
      t64 = 0.9D1 * t63
      t65 = S14 * t32
      t66 = 0.27D2 * t65
      t67 = S13 * t60
      t68 = 0.9D1 / 0.2D1 * t67
      t69 = t44 + t46 + t49 + t51 - 0.27D2 * t52 - 0.27D2 * t54 - t57 + 
     #t59 + t62 - t64 + t66 + t68
      t72 = S12 ** 2
      t73 = 0.1D1 / t72
      t82 = -t1
      t91 = -t18 - t17 + t38 + t30
      t115 = 0.2313D4 / 0.16D2 * S13
      t116 = 0.2313D4 / 0.16D2 * S23
      t139 = 0.18D2 * t82
      t188 = 0.18D2 * t61
      t190 = 0.18D2 * t58
      t198 = -0.189D3 / 0.2D1 * t45 + 0.2277D4 / 0.8D1 * t50 + t188 - 0.
     #189D3 / 0.2D1 * t43 + t190 - 0.171D3 * t67 + 0.2745D4 / 0.8D1 * t5
     #4 + 0.2745D4 / 0.8D1 * t52 - 0.189D3 * t63 + 0.2277D4 / 0.8D1 * t6
     #5 - 0.189D3 * t56 - 0.171D3 * t48
      t208 = t47 * S14
      t211 = t60 * S24
      t228 = -0.585D3 / 0.4D1 * t61 - 0.585D3 / 0.4D1 * t58 - 0.99D2 * t
     #63 + 0.531D3 / 0.16D2 * t45 - 0.99D2 * t56 + t51 - 0.171D3 / 0.8D1
     # * t67 + t66 - 0.171D3 / 0.8D1 * t48 + 0.531D3 / 0.16D2 * t43 + (0
     #.9D1 / 0.8D1 * S13 * t208 - 0.9D1 / 0.8D1 * S13 * t211 - 0.27D2 / 
     #0.8D1 * t18 * t60 - 0.27D2 / 0.8D1 * t58 * S24 + 0.27D2 / 0.8D1 * 
     #t48 * S24 + 0.27D2 / 0.8D1 * t38 * t60 - 0.9D1 / 0.8D1 * S23 * t20
     #8 + 0.9D1 / 0.8D1 * S23 * t211) * t26
      t230 = t139 * t26 * t4 * t28 * S34 + (0.18D2 + 0.189D3 / 0.2D1 * t
     #82 * t26 + (-0.171D3 / 0.8D1 * S13 - 0.171D3 / 0.8D1 * S23 + 0.27D
     #2 / 0.2D1 * t91 * t26) * t4) * t28 + (0.72D2 * S24 + 0.72D2 * S14 
     #+ t116 + t115 + (0.2565D4 / 0.8D1 * t18 + 0.2745D4 / 0.8D1 * t38 +
     # 0.2565D4 / 0.8D1 * t17 - 0.189D3 * t32 + 0.2745D4 / 0.8D1 * t30 -
     # 0.189D3 * t35) * t26 + (-0.63D2 * t35 - 0.63D2 * t32 + 0.9D1 * t1
     #7 - 0.99D2 * t38 - 0.99D2 * t30 + 0.9D1 * t18 + (-t68 - t49 - t62 
     #+ t64 - t59 + t57) * t26) * t4) * S34 + 0.2313D4 / 0.16D2 * t32 + 
     #0.2313D4 / 0.16D2 * t17 + 0.2313D4 / 0.16D2 * t18 + 0.54D2 * S14 *
     # S24 - 0.153D3 / 0.2D1 * t38 + 0.18D2 * t47 - 0.153D3 / 0.2D1 * t3
     #0 + 0.18D2 * t60 + 0.2313D4 / 0.16D2 * t35 + t198 * t26 + t228 * t
     #4
      t250 = -t139 * t28 + (0.54D2 * t30 + 0.72D2 * t17 + 0.54D2 * t38 +
     # 0.72D2 * t18 + 0.36D2 * t35 + 0.36D2 * t32) * S34 + 0.63D2 * t65 
     #+ 0.72D2 * t63 + 0.72D2 * t56 + 0.18D2 * t48 + 0.36D2 * t52 - t44 
     #+ 0.18D2 * t67 - t46 + 0.63D2 * t50 + 0.36D2 * t54 + t188 + t190
      t252 = t2 * t4 * t6 * t7 * t9 + ((-t2 * t4 * S34 - 0.432D3 * S13 -
     # 0.432D3 * S23 + (-0.216D3 * t17 - 0.216D3 * t18) * t4) * t6 + (0.
     #9D1 * t1 * t26 * t28 + (-0.9D1 / 0.2D1 * t30 + 0.9D1 * t32 + 0.9D1
     # / 0.2D1 * t18 + 0.9D1 * t35 + 0.9D1 / 0.2D1 * t17 - 0.9D1 / 0.2D1
     # * t38) * t26 * S34 + t69 * t26) * t73) * s * z + 0.477D3 / 0.8D1 
     #* t1 * t26 * t4 * t72 + (0.261D3 / 0.2D1 * t82 * t26 * t4 * S34 - 
     #0.18D2 + 0.171D3 * t82 * t26 + (0.531D3 / 0.16D2 * S13 + 0.531D3 /
     # 0.16D2 * S23 + 0.63D2 / 0.8D1 * t91 * t26) * t4) * S12 + 0.207D3 
     #/ 0.2D1 * t1 * t26 * t4 * t28 + (0.63D2 + 0.2277D4 / 0.8D1 * t1 * 
     #t26 + (0.27D2 * S23 + 0.27D2 * S13 - 0.27D2 * t91 * t26) * t4) * S
     #34 + 0.36D2 * S24 + 0.36D2 * S14 + t115 + t116 + (-0.486D3 * t17 -
     # 0.189D3 * t38 + 0.2565D4 / 0.8D1 * t32 - 0.486D3 * t18 - 0.189D3 
     #* t30 + 0.2565D4 / 0.8D1 * t35) * t26 + (-0.63D2 * t18 + 0.909D3 /
     # 0.8D1 * t32 - 0.63D2 * t17 + 0.909D3 / 0.8D1 * t35 + (0.27D2 / 0.
     #8D1 * t48 + 0.27D2 / 0.8D1 * t67 + 0.27D2 / 0.8D1 * t61 + 0.27D2 /
     # 0.8D1 * t58 - 0.27D2 / 0.4D1 * t63 - 0.27D2 / 0.4D1 * t56) * t26)
     # * t4 + t230 * t6 + t250 * t73
      rrgg2ggh21J4 = t252 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh21J5
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
      t1 = S13 + S23
      t2 = 0.216D3 * t1
      t4 = 0.1D1 / (S12 + S13 + S23)
      t6 = 0.1D1 / S12
      t7 = s ** 2
      t9 = z ** 2
      t17 = S13 * S24
      t18 = S14 * S23
      t26 = 0.1D1 / (S12 + S14 + S24)
      t28 = S34 ** 2
      t30 = S23 * S24
      t32 = S23 ** 2
      t35 = S13 ** 2
      t38 = S13 * S14
      t43 = t32 * S23
      t45 = t35 * S13
      t47 = S14 ** 2
      t48 = t47 * S23
      t49 = 0.9D1 / 0.2D1 * t48
      t50 = t35 * S24
      t52 = t32 * S24
      t54 = t35 * S14
      t56 = t38 * S24
      t57 = 0.9D1 * t56
      t58 = S13 * t47
      t59 = 0.9D1 / 0.2D1 * t58
      t60 = S24 ** 2
      t61 = t60 * S23
      t62 = 0.9D1 / 0.2D1 * t61
      t63 = t18 * S24
      t64 = 0.9D1 * t63
      t65 = S14 * t32
      t67 = S13 * t60
      t68 = 0.9D1 / 0.2D1 * t67
      t69 = 0.18D2 * t43 + 0.18D2 * t45 + t49 + 0.81D2 / 0.2D1 * t50 - 0
     #.81D2 / 0.2D1 * t52 - 0.81D2 / 0.2D1 * t54 - t57 + t59 + t62 - t64
     # + 0.81D2 / 0.2D1 * t65 + t68
      t72 = S12 ** 2
      t73 = 0.1D1 / t72
      t82 = -t1
      t91 = -t18 - t17 + t38 + t30
      t115 = 0.441D3 / 0.4D1 * S13
      t116 = 0.441D3 / 0.4D1 * S23
      t125 = 0.72D2 * t18
      t127 = 0.72D2 * t17
      t139 = 0.18D2 * t82
      t198 = -0.135D3 * t45 + 0.5769D4 / 0.16D2 * t50 + 0.603D3 / 0.16D2
     # * t61 - 0.135D3 * t43 + 0.603D3 / 0.16D2 * t58 - 0.1809D4 / 0.8D1
     # * t67 + 0.6849D4 / 0.16D2 * t54 + 0.6849D4 / 0.16D2 * t52 - 0.387
     #9D4 / 0.16D2 * t63 + 0.5769D4 / 0.16D2 * t65 - 0.3879D4 / 0.16D2 *
     # t56 - 0.1809D4 / 0.8D1 * t48
      t210 = t47 * S14
      t213 = t60 * S24
      t230 = -0.585D3 / 0.4D1 * t61 - 0.585D3 / 0.4D1 * t58 - 0.99D2 * t
     #63 + 0.819D3 / 0.16D2 * t45 - 0.99D2 * t56 + 0.36D2 * t50 - 0.171D
     #3 / 0.8D1 * t67 + 0.36D2 * t65 - 0.171D3 / 0.8D1 * t48 + 0.819D3 /
     # 0.16D2 * t43 + (0.9D1 / 0.8D1 * S13 * t210 - 0.9D1 / 0.8D1 * S13 
     #* t213 - 0.27D2 / 0.8D1 * t18 * t60 - 0.27D2 / 0.8D1 * t58 * S24 +
     # 0.27D2 / 0.8D1 * t48 * S24 + 0.27D2 / 0.8D1 * t38 * t60 - 0.9D1 /
     # 0.8D1 * S23 * t210 + 0.9D1 / 0.8D1 * S23 * t213) * t26
      t232 = t139 * t26 * t4 * t28 * S34 + (0.18D2 + 0.135D3 * t82 * t26
     # + (-0.171D3 / 0.8D1 * S13 - 0.171D3 / 0.8D1 * S23 + 0.27D2 / 0.2D
     #1 * t91 * t26) * t4) * t28 + (0.72D2 * S24 + 0.72D2 * S14 + t116 +
     # t115 + (0.6633D4 / 0.16D2 * t18 + 0.6849D4 / 0.16D2 * t38 + 0.663
     #3D4 / 0.16D2 * t17 - 0.270D3 * t32 + 0.6849D4 / 0.16D2 * t30 - 0.2
     #70D3 * t35) * t26 + (-0.72D2 * t35 - 0.72D2 * t32 + 0.9D1 * t17 - 
     #0.99D2 * t38 - 0.99D2 * t30 + 0.9D1 * t18 + (-t68 - t49 - t62 + t6
     #4 - t59 + t57) * t26) * t4) * S34 + 0.441D3 / 0.4D1 * t32 + 0.441D
     #3 / 0.4D1 * t17 + 0.441D3 / 0.4D1 * t18 + 0.54D2 * S14 * S24 - 0.9
     #27D3 / 0.8D1 * t38 + 0.18D2 * t47 - 0.927D3 / 0.8D1 * t30 + 0.18D2
     # * t60 + 0.441D3 / 0.4D1 * t35 + t198 * t26 + t230 * t4
      t254 = -t139 * t28 + (0.54D2 * t30 + t127 + 0.54D2 * t38 + t125 + 
     #0.27D2 * t35 + 0.27D2 * t32) * S34 + 0.54D2 * t65 + 0.72D2 * t63 +
     # 0.72D2 * t56 + 0.18D2 * t48 + 0.27D2 * t52 - 0.27D2 * t43 + 0.18D
     #2 * t67 - 0.27D2 * t45 + 0.54D2 * t50 + 0.27D2 * t54 + 0.18D2 * t6
     #1 + 0.18D2 * t58
      t256 = t2 * t4 * t6 * t7 * t9 + ((-t2 * t4 * S34 - 0.432D3 * S13 -
     # 0.432D3 * S23 + (-0.216D3 * t17 - 0.216D3 * t18) * t4) * t6 + (0.
     #9D1 * t1 * t26 * t28 + (-0.9D1 / 0.2D1 * t30 + 0.9D1 * t32 + 0.9D1
     # / 0.2D1 * t18 + 0.9D1 * t35 + 0.9D1 / 0.2D1 * t17 - 0.9D1 / 0.2D1
     # * t38) * t26 * S34 + t69 * t26) * t73) * s * z + 0.621D3 / 0.8D1 
     #* t1 * t26 * t4 * t72 + (0.333D3 / 0.2D1 * t82 * t26 * t4 * S34 - 
     #0.27D2 + 0.1809D4 / 0.8D1 * t82 * t26 + (0.819D3 / 0.16D2 * S13 + 
     #0.819D3 / 0.16D2 * S23 + 0.63D2 / 0.8D1 * t91 * t26) * t4) * S12 +
     # 0.243D3 / 0.2D1 * t1 * t26 * t4 * t28 + (0.54D2 + 0.5769D4 / 0.16
     #D2 * t1 * t26 + (0.36D2 * S13 + 0.36D2 * S23 - 0.27D2 * t91 * t26)
     # * t4) * S34 + 0.27D2 * S24 + 0.27D2 * S14 + t115 + t116 + (-0.267
     #3D4 / 0.4D1 * t17 - 0.3879D4 / 0.16D2 * t38 + 0.6633D4 / 0.16D2 * 
     #t32 - 0.2673D4 / 0.4D1 * t18 - 0.3879D4 / 0.16D2 * t30 + 0.6633D4 
     #/ 0.16D2 * t35) * t26 + (-t125 + 0.1197D4 / 0.8D1 * t32 - t127 + 0
     #.1197D4 / 0.8D1 * t35 + (0.27D2 / 0.8D1 * t48 + 0.27D2 / 0.8D1 * t
     #67 + 0.27D2 / 0.8D1 * t61 + 0.27D2 / 0.8D1 * t58 - 0.27D2 / 0.4D1 
     #* t63 - 0.27D2 / 0.4D1 * t56) * t26) * t4 + t232 * t6 + t254 * t73
      rrgg2ggh21J5 = t256 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh21J6
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
      t1 = -S13 - S23
      t2 = 0.1080D4 * t1
      t4 = 0.1D1 / (S12 + S13 + S23)
      t6 = 0.1D1 / S12
      t7 = s ** 2
      t9 = z ** 2
      t17 = S13 * S24
      t18 = S14 * S23
      t26 = 0.1D1 / (S12 + S14 + S24)
      t28 = S34 ** 2
      t30 = S13 ** 2
      t33 = S23 * S24
      t36 = S23 ** 2
      t38 = S13 * S14
      t43 = t36 * S23
      t44 = 0.90D2 * t43
      t45 = t30 * S13
      t46 = 0.90D2 * t45
      t47 = S14 ** 2
      t48 = t47 * S23
      t49 = 0.45D2 / 0.2D1 * t48
      t50 = t30 * S24
      t52 = t36 * S24
      t54 = t30 * S14
      t56 = t38 * S24
      t57 = 0.45D2 * t56
      t58 = S13 * t47
      t59 = 0.45D2 / 0.2D1 * t58
      t60 = S24 ** 2
      t61 = t60 * S23
      t62 = 0.45D2 / 0.2D1 * t61
      t63 = t18 * S24
      t64 = 0.45D2 * t63
      t65 = S14 * t36
      t67 = S13 * t60
      t68 = 0.45D2 / 0.2D1 * t67
      t69 = -t44 - t46 - t49 + 0.135D3 * t50 - 0.135D3 * t52 - 0.135D3 *
     # t54 + t57 - t59 - t62 + t64 + 0.135D3 * t65 - t68
      t72 = S12 ** 2
      t73 = 0.1D1 / t72
      t82 = -t1
      t91 = t18 + t17 - t38 - t33
      t113 = 0.360D3 * S24
      t114 = 0.360D3 * S14
      t115 = 0.25137D5 / 0.16D2 * S13
      t116 = 0.25137D5 / 0.16D2 * S23
      t139 = 0.90D2 * t82
      t196 = -0.675D3 / 0.2D1 * t45 - 0.99D2 / 0.8D1 * t50 + 0.135D3 / 0
     #.4D1 * t61 - 0.675D3 / 0.2D1 * t43 + 0.135D3 / 0.4D1 * t58 - 0.63D
     #2 / 0.2D1 * t67 - 0.1431D4 / 0.8D1 * t54 - 0.1431D4 / 0.8D1 * t52 
     #- 0.279D3 / 0.4D1 * t63 - 0.99D2 / 0.8D1 * t65 - 0.279D3 / 0.4D1 *
     # t56 - 0.63D2 / 0.2D1 * t48
      t212 = t60 * S24
      t215 = t47 * S14
      t228 = 0.2925D4 / 0.4D1 * t61 + 0.2925D4 / 0.4D1 * t58 + 0.495D3 *
     # t63 + 0.1377D4 / 0.16D2 * t45 + 0.495D3 * t56 - 0.9D1 * t50 + 0.8
     #55D3 / 0.8D1 * t67 - 0.9D1 * t65 + 0.855D3 / 0.8D1 * t48 + 0.1377D
     #4 / 0.16D2 * t43 + (0.135D3 / 0.8D1 * t18 * t60 + 0.135D3 / 0.8D1 
     #* t58 * S24 + 0.45D2 / 0.8D1 * S13 * t212 + 0.45D2 / 0.8D1 * S23 *
     # t215 - 0.45D2 / 0.8D1 * S23 * t212 - 0.45D2 / 0.8D1 * S13 * t215 
     #- 0.135D3 / 0.8D1 * t38 * t60 - 0.135D3 / 0.8D1 * t48 * S24) * t26
      t230 = t139 * t26 * t4 * t28 * S34 + (-0.90D2 + 0.675D3 / 0.2D1 * 
     #t1 * t26 + (0.855D3 / 0.8D1 * S23 + 0.855D3 / 0.8D1 * S13 + 0.135D
     #3 / 0.2D1 * t91 * t26) * t4) * t28 + (-t113 - t114 - t115 - t116 +
     # (0.477D3 / 0.8D1 * t18 - 0.1431D4 / 0.8D1 * t38 + 0.477D3 / 0.8D1
     # * t17 - 0.675D3 * t36 - 0.1431D4 / 0.8D1 * t33 - 0.675D3 * t30) *
     # t26 + (0.189D3 * t30 + 0.189D3 * t36 - 0.45D2 * t17 + 0.495D3 * t
     #38 + 0.495D3 * t33 - 0.45D2 * t18 + (t68 + t59 + t49 - t57 - t64 +
     # t62) * t26) * t4) * S34 - 0.25137D5 / 0.16D2 * t36 - 0.25137D5 / 
     #0.16D2 * t17 - 0.25137D5 / 0.16D2 * t18 - 0.270D3 * S14 * S24 + 0.
     #135D3 * t38 - 0.90D2 * t47 + 0.135D3 * t33 - 0.90D2 * t60 - 0.2513
     #7D5 / 0.16D2 * t30 + t196 * t26 + t228 * t4
      t252 = -t139 * t28 + (-0.270D3 * t33 - 0.360D3 * t17 - 0.270D3 * t
     #38 - 0.360D3 * t18 - 0.360D3 * t30 - 0.360D3 * t36) * S34 - 0.495D
     #3 * t65 - 0.360D3 * t63 - 0.360D3 * t56 - 0.90D2 * t48 - 0.360D3 *
     # t52 - t44 - 0.90D2 * t67 - t46 - 0.495D3 * t50 - 0.360D3 * t54 - 
     #0.90D2 * t61 - 0.90D2 * t58
      t254 = t2 * t4 * t6 * t7 * t9 + ((-t2 * t4 * S34 + 0.2160D4 * S23 
     #+ 0.2160D4 * S13 + (0.1080D4 * t17 + 0.1080D4 * t18) * t4) * t6 + 
     #(0.45D2 * t1 * t26 * t28 + (-0.45D2 * t30 - 0.45D2 / 0.2D1 * t17 +
     # 0.45D2 / 0.2D1 * t33 - 0.45D2 / 0.2D1 * t18 - 0.45D2 * t36 + 0.45
     #D2 / 0.2D1 * t38) * t26 * S34 + t69 * t26) * t73) * s * z + 0.369D
     #3 / 0.8D1 * t1 * t26 * t4 * t72 + (0.297D3 / 0.2D1 * t82 * t26 * t
     #4 * S34 - 0.90D2 + 0.63D2 / 0.2D1 * t1 * t26 + (0.1377D4 / 0.16D2 
     #* S13 + 0.1377D4 / 0.16D2 * S23 + 0.315D3 / 0.8D1 * t91 * t26) * t
     #4) * S12 + 0.531D3 / 0.2D1 * t1 * t26 * t4 * t28 + (-0.495D3 + 0.9
     #9D2 / 0.8D1 * t1 * t26 + (-0.9D1 * S13 - 0.9D1 * S23 - 0.135D3 * t
     #91 * t26) * t4) * S34 - t113 - t114 - t115 - t116 + (-0.351D3 * t1
     #7 - 0.279D3 / 0.4D1 * t38 + 0.477D3 / 0.8D1 * t36 - 0.351D3 * t18 
     #- 0.279D3 / 0.4D1 * t33 + 0.477D3 / 0.8D1 * t30) * t26 + (0.189D3 
     #* t18 - 0.513D3 / 0.8D1 * t36 + 0.189D3 * t17 - 0.513D3 / 0.8D1 * 
     #t30 + (-0.135D3 / 0.8D1 * t48 - 0.135D3 / 0.8D1 * t67 - 0.135D3 / 
     #0.8D1 * t61 - 0.135D3 / 0.8D1 * t58 + 0.135D3 / 0.4D1 * t56 + 0.13
     #5D3 / 0.4D1 * t63) * t26) * t4 + t230 * t6 + t252 * t73
      rrgg2ggh21J6 = t254 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh21J7
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
      t1 = S13 + S23
      t2 = 0.90D2 * t1
      t4 = 0.1D1 / (S12 + S14 + S24)
      t5 = t2 * t4
      t7 = 0.1D1 / (S12 + S13 + S23)
      t8 = S12 ** 2
      t13 = -0.180D3 * t1 * t4
      t19 = S34 ** 2
      t26 = 0.135D3 * S13
      t27 = 0.135D3 * S23
      t28 = S23 ** 2
      t29 = 0.180D3 * t28
      t30 = S14 * S23
      t32 = S13 * S24
      t34 = S13 * S14
      t36 = S23 * S24
      t38 = S13 ** 2
      t39 = 0.180D3 * t38
      t65 = S24 ** 2
      t68 = S14 * t28
      t70 = t38 * S24
      t74 = S14 ** 2
      rrgg2ggh21J7 = (t5 * t7 * t8 + (t13 * t7 * S34 + t13 + t2 * t7) * 
     #S12 + t5 * t7 * t19 + (t5 + 0.45D2 * t1 * t7) * S34 + t26 + t27 + 
     #(t29 - 0.720D3 * t30 - 0.720D3 * t32 - 0.45D2 * t34 - 0.45D2 * t36
     # + t39) * t4 + (t29 - 0.45D2 * t30 - 0.45D2 * t32 + t39) * t7 + ((
     #t26 + t27 + (0.135D3 * t36 + 0.180D3 * t32 + 0.180D3 * t30 + 0.135
     #D3 * t34) * t4 + (-0.45D2 * t28 - 0.45D2 * t38) * t7) * S34 - 0.45
     #0D3 * t36 + 0.135D3 * t28 - 0.450D3 * t34 + 0.135D3 * t32 + 0.135D
     #3 * t38 + 0.135D3 * t30 + (0.135D3 * t28 * S24 + 0.225D3 * t65 * S
     #23 + 0.90D2 * t68 + 0.90D2 * t70 + 0.135D3 * t38 * S14 - 0.180D3 *
     # t74 * S23 - 0.45D2 * t34 * S24 - 0.180D3 * S13 * t65 + 0.225D3 * 
     #S13 * t74 - 0.45D2 * t30 * S24) * t4 + (0.90D2 * t38 * S13 + 0.45D
     #2 * t70 + 0.90D2 * t28 * S23 + 0.45D2 * t68) * t7) / S12) / pi * w
     #d / z

      end function
  
 