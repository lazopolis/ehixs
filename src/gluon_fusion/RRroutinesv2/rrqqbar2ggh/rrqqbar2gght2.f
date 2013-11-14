  
      subroutine rrqqbar2gght2
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2ggh21J1  
      doubleprecision rrqqbar2ggh21J2  
      doubleprecision rrqqbar2ggh21J3  
      doubleprecision rrqqbar2gght2s1e1  
      doubleprecision rrqqbar2gght2s1e0  
      doubleprecision rrqqbar2gght2s1em1  
      doubleprecision rrqqbar2gght2s1em2  
      doubleprecision rrqqbar2gght2s1em3  
      doubleprecision rrqqbar2gght2s1em4  
      doubleprecision rrqqbar2gght2s2e1  
      doubleprecision rrqqbar2gght2s2e0  
      doubleprecision rrqqbar2gght2s2em1  
      doubleprecision rrqqbar2gght2s2em2  
      doubleprecision rrqqbar2gght2s2em3  
      doubleprecision rrqqbar2gght2s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght2s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght2s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght2s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght2s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght2s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght2s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght2s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght2s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght2s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght2s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght2s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrqqbar2gght2s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2gght2s1e1
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
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3

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
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t9 = t7 * t8
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = x3 * t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t18 * x4
      t20 = -0.1D1 + x3
      t22 = t19 * t4 * t20
      t25 = log(0.4D1 * t16 * t22)
      t26 = t25 ** 2
      t27 = t19 * t4
      t30 = log(-0.4D1 * t16 * t27)
      t31 = t30 ** 2
      t37 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t38 = t7 * t37
      t41 = pi * lh
      t42 = t1 * t7
      t43 = t42 * t8
      t50 = 0.1D1 / x3
      t54 = t12 * t15
      t57 = log(-0.4D1 * t54 * t27)
      t58 = t57 * pi
      t62 = rrqqbar2ggh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t66 = lh ** 2
      t68 = pi ** 2
      t70 = 0.180D3 * t66 - 0.30D2 * t68
      t71 = pi * t70
      t74 = t57 ** 2
      t75 = t74 * pi
      t99 = x1 ** 2
      t100 = x3 * t99
      t101 = t100 * t54
      t104 = log(0.4D1 * t101 * t22)
      t107 = t15 * t18
      t108 = x4 * t4
      t112 = log(-0.4D1 * t100 * t12 * t107 * t108)
      t116 = 0.1D1 / x1
      t120 = t99 * t12
      t124 = log(-0.4D1 * t120 * t15 * t27)
      t126 = t124 ** 2
      t142 = (0.90D2 * t6 * t9 * (t26 / 0.2D1 - t31 / 0.2D1) + (0.90D2 *
     # t6 * t38 - 0.180D3 * t41 * t43) * (-t25 + t30)) * t50 / 0.1440D4 
     #- (-0.180D3 * t41 - 0.90D2 * t58) * t1 * t7 * t62 / 0.1440D4 - (t7
     #1 + 0.180D3 * t58 * lh + 0.45D2 * t75) * t1 * t38 / 0.1440D4 - (pi
     # * (0.60D2 * lh * t68 - 0.240D3 * zeta3 - 0.120D3 * t66 * lh) - t5
     #8 * t70 - 0.90D2 * t75 * lh - 0.15D2 * t74 * t57 * pi) * t1 * t9 /
     # 0.1440D4 - t6 * t7 * (t104 * t8 - t112 * t8) * t50 * t116 / 0.8D1
     # + (0.90D2 * t6 * t7 * (-t62 + t124 * t37 - t126 * t8 / 0.2D1) - 0
     #.180D3 * t41 * t42 * (-t37 + t124 * t8) - t71 * t43) * t116 / 0.72
     #0D3
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
      t162 = s * t157 * t17 * t160 * t4
      t163 = t151 * t157
      t165 = 0.1D1 / (-0.2D1 + t147)
      t166 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, t150, -t154,
     # 0.0D0, t156, t162)
      t168 = t163 * t165 * t166
      t169 = t151 ** 2
      t171 = t157 ** 2
      t176 = log(-0.4D1 * t101 * t19 * t4 * t169 * t171)
      t178 = t157 * t165
      t179 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, t150, -t154,
     # 0.0D0, t156, t162)
      t180 = t178 * t179
      t186 = t41 * t42
      t188 = t163 * t165 * t179
      t194 = rrqqbar2ggh21J3(s, XB1, XB2, z, lh, wd, nf, s, t150, -t154,
     # 0.0D0, t156, t162)
      t202 = log(-0.4D1 * t120 * t107 * t108 * t171 * t169)
      t203 = t202 * t151
      t206 = t202 ** 2
      t224 = -(0.90D2 * t6 * t7 * (-t168 + t176 * t151 * t180) + 0.180D3
     # * t186 * t188) * t50 * t116 / 0.720D3 + (0.90D2 * t6 * t7 * (t163
     # * t165 * t194 - t203 * t178 * t166 + t206 * t151 * t180 / 0.2D1) 
     #- 0.180D3 * t41 * t42 * (t168 - t203 * t180) + t71 * t42 * t188) *
     # t116 / 0.720D3
      t225 = FJET(XB1, XB2, s, t150, 0.0D0, -t154, t156, t162, t224)
      t227 = FJET(XB1, XB2, s, -t154, t156, t150, 0.0D0, t162, t224)
      t230 = KAPPA2(x1, x2, -t20, x4, z)
      t231 = s * t230
      t233 = t231 * t149 * t20
      t235 = t231 * t149 * x3
      t236 = t231 * t153
      t237 = t231 * t155
      t238 = t230 ** 2
      t243 = cos(t10)
      t246 = Sqrt(x3 * t20 * t108)
      t251 = s * t238 * t17 * t160 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t243 * t246)
      t252 = t151 * t238
      t254 = 0.1D1 / (-0.2D1 + t230)
      t255 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, -t233, -t236
     #, t235, t237, t251)
      t261 = t238 ** 2
      t266 = log(0.4D1 * t100 * t54 * t18 * t108 * t169 * t20 * t261)
      t269 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, -t233, -t236
     #, t235, t237, t251)
      t280 = 0.90D2 * t6 * t7 * (t252 * t254 * t255 - t266 * t151 * t238
     # * t254 * t269) - 0.180D3 * t186 * t252 * t254 * t269
      t283 = t280 * t50 * t116 / 0.720D3
      t284 = FJET(XB1, XB2, s, -t233, t235, -t236, t237, t251, -t283)
      t286 = t50 * t116
      t289 = FJET(XB1, XB2, s, -t236, t237, -t233, t235, t251, -t283)
      rrqqbar2gght2s1e1 = t143 * t142 + t145 * t142 + t225 * t224 + t227
     # * t224 - t284 * t280 * t286 / 0.720D3 - t289 * t280 * t286 / 0.72
     #0D3

      end function



      doubleprecision function rrqqbar2gght2s1e0
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
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3

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
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t12 = pi * lh
      t14 = x2 * pi
      t15 = sin(t14)
      t16 = t15 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = t21 * x4
      t23 = t22 * t4
      t26 = log(-0.4D1 * t16 * t18 * t23)
      t27 = t26 * pi
      t31 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t35 = lh ** 2
      t37 = pi ** 2
      t43 = t26 ** 2
      t48 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t54 = x3 * t16 * t18
      t55 = -0.1D1 + x3
      t60 = log(0.4D1 * t54 * t22 * t4 * t55)
      t63 = log(-0.4D1 * t54 * t23)
      t66 = 0.1D1 / x3
      t70 = x1 ** 2
      t71 = t70 * t16
      t75 = log(-0.4D1 * t71 * t18 * t23)
      t81 = t1 * t7
      t86 = 0.1D1 / x1
      t89 = -t6 * t7 * t8 / 0.16D2 - (-0.180D3 * t12 - 0.90D2 * t27) * t
     #1 * t7 * t31 / 0.1440D4 - (pi * (0.180D3 * t35 - 0.30D2 * t37) + 0
     #.180D3 * t27 * lh + 0.45D2 * t43 * pi) * t1 * t7 * t48 / 0.1440D4 
     #+ t6 * t7 * t48 * (-t60 + t63) * t66 / 0.16D2 + (0.90D2 * t6 * t7 
     #* (-t31 + t75 * t48) + 0.180D3 * t12 * t81 * t48) * t86 / 0.720D3
      t90 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t89)
      t92 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t89)
      t94 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t95 = s * t94
      t96 = t1 * x1
      t97 = t95 * t96
      t98 = -0.1D1 + x1
      t99 = t1 * t98
      t100 = t99 * x4
      t101 = t95 * t100
      t102 = t99 * t4
      t103 = t95 * t102
      t104 = t94 ** 2
      t107 = x1 * t98
      t109 = s * t104 * t20 * t107 * t4
      t111 = t6 * t7 * t98
      t113 = 0.1D1 / (-0.2D1 + t94)
      t114 = t104 * t113
      t115 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, t97, -t101, 
     #0.0D0, t103, t109)
      t121 = t98 * t104
      t122 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, t97, -t101, 
     #0.0D0, t103, t109)
      t127 = x4 * t4
      t128 = t104 ** 2
      t129 = t98 ** 2
      t134 = log(-0.4D1 * t71 * t18 * t21 * t127 * t128 * t129)
      t150 = t111 * t114 * t115 * t66 * t86 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (t121 * t113 * t122 - t134 * t98 * t114 * t115) - 0.180D3 * t12 
     #* t81 * t121 * t113 * t115) * t86 / 0.720D3
      t151 = FJET(XB1, XB2, s, t97, 0.0D0, -t101, t103, t109, t150)
      t153 = FJET(XB1, XB2, s, -t101, t103, t97, 0.0D0, t109, t150)
      t156 = KAPPA2(x1, x2, -t55, x4, z)
      t157 = s * t156
      t159 = t157 * t96 * t55
      t161 = t157 * t96 * x3
      t162 = t157 * t100
      t163 = t157 * t102
      t164 = t156 ** 2
      t169 = cos(t14)
      t172 = Sqrt(x3 * t55 * t127)
      t177 = s * t164 * t20 * t107 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t169 * t172)
      t181 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, -t159, -t162
     #, t161, t163, t177)
      t184 = t164 / (-0.2D1 + t156) * t181 * t66 * t86
      t186 = t111 * t184 / 0.8D1
      t187 = FJET(XB1, XB2, s, -t159, t161, -t162, t163, t177, -t186)
      t189 = t81 * t98
      t193 = FJET(XB1, XB2, s, -t162, t163, -t159, t161, t177, -t186)
      rrqqbar2gght2s1e0 = t90 * t89 + t92 * t89 + t151 * t150 + t153 * t
     #150 - t187 * pi * t189 * t184 / 0.8D1 - t193 * pi * t189 * t184 / 
     #0.8D1

      end function



      doubleprecision function rrqqbar2gght2s1em1
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
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3

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
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t15 = sin(x2 * pi)
      t16 = t15 ** 2
      t17 = z ** 2
      t20 = t1 ** 2
      t21 = t20 ** 2
      t26 = log(-0.4D1 * t16 / t17 * t21 * x4 * t4)
      t31 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t32 = t7 * t31
      t35 = 0.1D1 / x1
      t39 = -t6 * t7 * t8 / 0.16D2 - (-0.180D3 * pi * lh - 0.90D2 * t26 
     #* pi) * t1 * t32 / 0.1440D4 - t6 * t32 * t35 / 0.8D1
      t40 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t39)
      t42 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t39)
      t44 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t45 = s * t44
      t47 = t45 * t1 * x1
      t48 = -0.1D1 + x1
      t49 = t1 * t48
      t51 = t45 * t49 * x4
      t53 = t45 * t49 * t4
      t54 = t44 ** 2
      t59 = s * t54 * t20 * x1 * t48 * t4
      t63 = 0.1D1 / (-0.2D1 + t44)
      t65 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, t47, -t51, 0.
     #0D0, t53, t59)
      t69 = t6 * t7 * t48 * t54 * t63 * t65 * t35 / 0.8D1
      t70 = FJET(XB1, XB2, s, t47, 0.0D0, -t51, t53, t59, t69)
      t72 = t1 * t7
      t77 = t48 * t54 * t63 * t65 * t35
      t80 = FJET(XB1, XB2, s, -t51, t53, t47, 0.0D0, t59, t69)
      rrqqbar2gght2s1em1 = t40 * t39 + t42 * t39 + t70 * pi * t72 * t77 
     #/ 0.8D1 + t80 * pi * t72 * t77 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght2s1em2
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
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3

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
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t11 = pi * t1 * t7 * t8 / 0.16D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, -t11)
      t15 = t1 * t7 * t8
      t17 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrqqbar2gght2s1em2 = -t12 * pi * t15 / 0.16D2 - t17 * pi * t15 / 0
     #.16D2

      end function



      doubleprecision function rrqqbar2gght2s1em3
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
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght2s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght2s1em4
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
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght2s1em4 = 0.0D0

      end function


      doubleprecision function rrqqbar2gght2s2e1
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
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3

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
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t9 = t7 * t8
      t10 = x2 * pi
      t11 = sin(t10)
      t12 = t11 ** 2
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = x3 * t12 * t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t18 * x4
      t20 = -0.1D1 + x3
      t22 = t19 * t4 * t20
      t25 = log(0.4D1 * t16 * t22)
      t26 = t25 ** 2
      t27 = t19 * t4
      t30 = log(-0.4D1 * t16 * t27)
      t31 = t30 ** 2
      t37 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t38 = t7 * t37
      t41 = pi * lh
      t42 = t1 * t7
      t43 = t42 * t8
      t50 = 0.1D1 / x3
      t54 = t12 * t15
      t57 = log(-0.4D1 * t54 * t27)
      t58 = t57 * pi
      t62 = rrqqbar2ggh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t66 = lh ** 2
      t68 = pi ** 2
      t70 = 0.180D3 * t66 - 0.30D2 * t68
      t71 = pi * t70
      t74 = t57 ** 2
      t75 = t74 * pi
      t99 = x1 ** 2
      t100 = x3 * t99
      t101 = t100 * t54
      t104 = log(0.4D1 * t101 * t22)
      t107 = t15 * t18
      t108 = x4 * t4
      t112 = log(-0.4D1 * t100 * t12 * t107 * t108)
      t116 = 0.1D1 / x1
      t120 = t99 * t12
      t124 = log(-0.4D1 * t120 * t15 * t27)
      t126 = t124 ** 2
      t142 = (0.90D2 * t6 * t9 * (t26 / 0.2D1 - t31 / 0.2D1) + (0.90D2 *
     # t6 * t38 - 0.180D3 * t41 * t43) * (-t25 + t30)) * t50 / 0.1440D4 
     #- (-0.180D3 * t41 - 0.90D2 * t58) * t1 * t7 * t62 / 0.1440D4 - (t7
     #1 + 0.180D3 * t58 * lh + 0.45D2 * t75) * t1 * t38 / 0.1440D4 - (pi
     # * (0.60D2 * lh * t68 - 0.240D3 * zeta3 - 0.120D3 * t66 * lh) - t5
     #8 * t70 - 0.90D2 * t75 * lh - 0.15D2 * t74 * t57 * pi) * t1 * t9 /
     # 0.1440D4 - t6 * t7 * (t104 * t8 - t112 * t8) * t50 * t116 / 0.8D1
     # + (0.90D2 * t6 * t7 * (-t62 + t124 * t37 - t126 * t8 / 0.2D1) - 0
     #.180D3 * t41 * t42 * (-t37 + t124 * t8) - t71 * t43) * t116 / 0.72
     #0D3
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
      t160 = s * t155 * t17 * t158 * x4
      t161 = t149 * t155
      t163 = 0.1D1 / (-0.2D1 + t145)
      t164 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t152
     #, t148, t154, -t160)
      t166 = t161 * t163 * t164
      t167 = t149 ** 2
      t169 = t155 ** 2
      t174 = log(-0.4D1 * t101 * t19 * t4 * t167 * t169)
      t176 = t155 * t163
      t177 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t152
     #, t148, t154, -t160)
      t178 = t176 * t177
      t184 = t41 * t42
      t186 = t161 * t163 * t177
      t192 = rrqqbar2ggh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t152
     #, t148, t154, -t160)
      t200 = log(-0.4D1 * t120 * t107 * t108 * t169 * t167)
      t201 = t200 * t149
      t204 = t200 ** 2
      t222 = -(0.90D2 * t6 * t7 * (-t166 + t174 * t149 * t178) + 0.180D3
     # * t184 * t186) * t50 * t116 / 0.720D3 + (0.90D2 * t6 * t7 * (t161
     # * t163 * t192 - t201 * t176 * t164 + t204 * t149 * t178 / 0.2D1) 
     #- 0.180D3 * t41 * t42 * (t166 - t201 * t178) + t71 * t42 * t186) *
     # t116 / 0.720D3
      t223 = FJET(XB1, XB2, s, 0.0D0, t148, -t152, t154, -t160, t222)
      t225 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t142)
      t227 = KAPPA2(x1, x2, x3, x4, z)
      t228 = s * t227
      t230 = t228 * t147 * x3
      t232 = t228 * t147 * t20
      t233 = t228 * t151
      t234 = t228 * t153
      t235 = t227 ** 2
      t240 = cos(t10)
      t243 = Sqrt(x3 * t20 * t108)
      t248 = s * t235 * t17 * t158 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t240 * t243)
      t249 = t149 * t235
      t251 = 0.1D1 / (-0.2D1 + t227)
      t252 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, t230, -t233,
     # -t232, t234, t248)
      t258 = t235 ** 2
      t263 = log(0.4D1 * t100 * t54 * t18 * t108 * t167 * t20 * t258)
      t266 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, t230, -t233,
     # -t232, t234, t248)
      t277 = 0.90D2 * t6 * t7 * (t249 * t251 * t252 - t263 * t149 * t235
     # * t251 * t266) - 0.180D3 * t184 * t249 * t251 * t266
      t280 = t277 * t50 * t116 / 0.720D3
      t281 = FJET(XB1, XB2, s, t230, -t232, -t233, t234, t248, -t280)
      t283 = t50 * t116
      t286 = FJET(XB1, XB2, s, -t152, t154, 0.0D0, t148, -t160, t222)
      t288 = FJET(XB1, XB2, s, -t233, t234, t230, -t232, t248, -t280)
      rrqqbar2gght2s2e1 = t143 * t142 + t223 * t222 + t225 * t142 - t281
     # * t277 * t283 / 0.720D3 + t286 * t222 - t288 * t277 * t283 / 0.72
     #0D3

      end function



      doubleprecision function rrqqbar2gght2s2e0
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
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3

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
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh21J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t12 = pi * lh
      t14 = x2 * pi
      t15 = sin(t14)
      t16 = t15 ** 2
      t17 = z ** 2
      t18 = 0.1D1 / t17
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = t21 * x4
      t23 = t22 * t4
      t26 = log(-0.4D1 * t16 * t18 * t23)
      t27 = t26 * pi
      t31 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t35 = lh ** 2
      t37 = pi ** 2
      t43 = t26 ** 2
      t48 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t54 = x3 * t16 * t18
      t55 = -0.1D1 + x3
      t60 = log(0.4D1 * t54 * t22 * t4 * t55)
      t63 = log(-0.4D1 * t54 * t23)
      t66 = 0.1D1 / x3
      t70 = x1 ** 2
      t71 = t70 * t16
      t75 = log(-0.4D1 * t71 * t18 * t23)
      t81 = t1 * t7
      t86 = 0.1D1 / x1
      t89 = -t6 * t7 * t8 / 0.16D2 - (-0.180D3 * t12 - 0.90D2 * t27) * t
     #1 * t7 * t31 / 0.1440D4 - (pi * (0.180D3 * t35 - 0.30D2 * t37) + 0
     #.180D3 * t27 * lh + 0.45D2 * t43 * pi) * t1 * t7 * t48 / 0.1440D4 
     #+ t6 * t7 * t48 * (-t60 + t63) * t66 / 0.16D2 + (0.90D2 * t6 * t7 
     #* (-t31 + t75 * t48) + 0.180D3 * t12 * t81 * t48) * t86 / 0.720D3
      t90 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t89)
      t92 = KAPPA2(x1, x2, 0.0D0, x4, z)
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
      t107 = s * t102 * t20 * t105 * x4
      t109 = t6 * t7 * t96
      t111 = 0.1D1 / (-0.2D1 + t92)
      t112 = t102 * t111
      t113 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t99,
     # t95, t101, -t107)
      t119 = t96 * t102
      t120 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t99,
     # t95, t101, -t107)
      t125 = x4 * t4
      t126 = t102 ** 2
      t127 = t96 ** 2
      t132 = log(-0.4D1 * t71 * t18 * t21 * t125 * t126 * t127)
      t148 = t109 * t112 * t113 * t66 * t86 / 0.8D1 + (0.90D2 * t6 * t7 
     #* (t119 * t111 * t120 - t132 * t96 * t112 * t113) - 0.180D3 * t12 
     #* t81 * t119 * t111 * t113) * t86 / 0.720D3
      t149 = FJET(XB1, XB2, s, 0.0D0, t95, -t99, t101, -t107, t148)
      t151 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t89)
      t153 = KAPPA2(x1, x2, x3, x4, z)
      t154 = s * t153
      t156 = t154 * t94 * x3
      t158 = t154 * t94 * t55
      t159 = t154 * t98
      t160 = t154 * t100
      t161 = t153 ** 2
      t166 = cos(t14)
      t169 = Sqrt(x3 * t55 * t125)
      t174 = s * t161 * t20 * t105 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t166 * t169)
      t178 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, t156, -t159,
     # -t158, t160, t174)
      t181 = t161 / (-0.2D1 + t153) * t178 * t66 * t86
      t183 = t109 * t181 / 0.8D1
      t184 = FJET(XB1, XB2, s, t156, -t158, -t159, t160, t174, -t183)
      t186 = t81 * t96
      t190 = FJET(XB1, XB2, s, -t99, t101, 0.0D0, t95, -t107, t148)
      t192 = FJET(XB1, XB2, s, -t159, t160, t156, -t158, t174, -t183)
      rrqqbar2gght2s2e0 = t90 * t89 + t148 * t149 + t151 * t89 - t184 * 
     #pi * t186 * t181 / 0.8D1 + t190 * t148 - t192 * pi * t186 * t181 /
     # 0.8D1

      end function



      doubleprecision function rrqqbar2gght2s2em1
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
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3

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
      t6 = pi * t1
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh21J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t15 = sin(x2 * pi)
      t16 = t15 ** 2
      t17 = z ** 2
      t20 = t1 ** 2
      t21 = t20 ** 2
      t26 = log(-0.4D1 * t16 / t17 * t21 * x4 * t4)
      t31 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.
     #0D0, -t5, 0.0D0)
      t32 = t7 * t31
      t35 = 0.1D1 / x1
      t39 = -t6 * t7 * t8 / 0.16D2 - (-0.180D3 * pi * lh - 0.90D2 * t26 
     #* pi) * t1 * t32 / 0.1440D4 - t6 * t32 * t35 / 0.8D1
      t40 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t39)
      t42 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t43 = s * t42
      t45 = t43 * t1 * x1
      t46 = -0.1D1 + x1
      t47 = t1 * t46
      t49 = t43 * t47 * x4
      t51 = t43 * t47 * t4
      t52 = t42 ** 2
      t57 = s * t52 * t20 * x1 * t46 * x4
      t61 = 0.1D1 / (-0.2D1 + t42)
      t63 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, -t49, 
     #t45, t51, -t57)
      t67 = t6 * t7 * t46 * t52 * t61 * t63 * t35 / 0.8D1
      t68 = FJET(XB1, XB2, s, 0.0D0, t45, -t49, t51, -t57, t67)
      t70 = t1 * t7
      t75 = t46 * t52 * t61 * t63 * t35
      t78 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t39)
      t80 = FJET(XB1, XB2, s, -t49, t51, 0.0D0, t45, -t57, t67)
      rrqqbar2gght2s2em1 = t40 * t39 + t68 * pi * t70 * t75 / 0.8D1 + t7
     #8 * t39 + t80 * pi * t70 * t75 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght2s2em2
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
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3

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
      t7 = 0.1D1 / s
      t8 = rrqqbar2ggh21J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, t3, 0.0
     #D0, -t5, 0.0D0)
      t11 = pi * t1 * t7 * t8 / 0.16D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, -t11)
      t15 = t1 * t7 * t8
      t17 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, -t11)
      rrqqbar2gght2s2em2 = -t12 * pi * t15 / 0.16D2 - t17 * pi * t15 / 0
     #.16D2

      end function



      doubleprecision function rrqqbar2gght2s2em3
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
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght2s2em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght2s2em4
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
      doubleprecision rrqqbar2ggh21J1
      doubleprecision rrqqbar2ggh21J2
      doubleprecision rrqqbar2ggh21J3

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrqqbar2gght2s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqqbar2ggh21J1
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
      t1 = -S23 - S13
      t3 = S12 + S13 + S23
      t4 = t3 ** 2
      t5 = 0.1D1 / t4
      t7 = S13 ** 2
      t8 = S23 ** 2
      t9 = t7 + t8
      t12 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t19 = -t1
      t21 = 0.1D1 / t3
      t32 = 0.1D1 / (S12 + S14 + S24)
      t34 = S12 ** 2
      t38 = 0.16D2 / 0.3D1 * t19 * t32
      t45 = S13 * S24
      t46 = S13 * S14
      t47 = S23 * S24
      t48 = S14 * S23
      t49 = t45 - t46 - t47 + t48
      t58 = S34 ** 2
      t65 = -t49
      t86 = S14 ** 2
      t87 = S23 * t86
      t89 = S24 ** 2
      t90 = S23 * t89
      t92 = t45 * S14
      t94 = S13 * t86
      t96 = S13 * t89
      t98 = t47 * S14
      t179 = t86 * S14
      t182 = t89 * S24
      t197 = -0.4D1 / 0.3D1 * S13 * t179 + 0.4D1 / 0.3D1 * S13 * t182 + 
     #0.4D1 * t45 * t86 + 0.4D1 * t90 * S14 - 0.4D1 * t47 * t86 - 0.4D1 
     #* t96 * S14 + 0.4D1 / 0.3D1 * S23 * t179 - 0.4D1 / 0.3D1 * S23 * t
     #182
      t199 = 0.16D2 / 0.27D2 * t98 + 0.47D2 / 0.27D2 * t7 * S13 + 0.47D2
     # / 0.27D2 * t8 * S23 + 0.32D2 / 0.9D1 * t8 * S14 - 0.4D1 / 0.9D1 *
     # t94 + 0.148D3 / 0.27D2 * t7 * S14 + 0.16D2 / 0.27D2 * t92 + 0.32D
     #2 / 0.9D1 * t7 * S24 + 0.748D3 / 0.27D2 * t96 - 0.4D1 / 0.9D1 * t9
     #0 + 0.748D3 / 0.27D2 * t87 + 0.148D3 / 0.27D2 * t8 * S24 + t197 * 
     #t32
      t201 = t38 * t21 * t58 * S34 + (-0.32D2 / 0.27D2 + 0.56D2 / 0.27D2
     # * t19 * t32 + (0.104D3 / 0.9D1 * S23 + 0.104D3 / 0.9D1 * S13 + 0.
     #8D1 * t49 * t32) * t21) * t58 + (-0.32D2 / 0.27D2 * S14 - 0.176D3 
     #/ 0.9D1 * S23 - 0.176D3 / 0.9D1 * S13 - 0.32D2 / 0.27D2 * S24 + (0
     #.328D3 / 0.27D2 * t46 + 0.584D3 / 0.27D2 * t45 + 0.328D3 / 0.27D2 
     #* t47 + 0.584D3 / 0.27D2 * t48) * t32 + (0.880D3 / 0.27D2 * t48 - 
     #0.80D2 / 0.9D1 * t47 + 0.128D3 / 0.27D2 * t7 + 0.880D3 / 0.27D2 * 
     #t45 - 0.80D2 / 0.9D1 * t46 + 0.128D3 / 0.27D2 * t8 + (0.16D2 / 0.3
     #D1 * t94 + 0.16D2 / 0.3D1 * t96 + 0.16D2 / 0.3D1 * t90 - 0.32D2 / 
     #0.3D1 * t92 + 0.16D2 / 0.3D1 * t87 - 0.32D2 / 0.3D1 * t98) * t32) 
     #* t21) * S34 - 0.32D2 / 0.27D2 * t86 + 0.17D2 / 0.27D2 * t7 - 0.32
     #D2 / 0.27D2 * t89 + 0.17D2 / 0.27D2 * t8 - 0.34D2 / 0.3D1 * t45 - 
     #0.34D2 / 0.3D1 * t48 - 0.122D3 / 0.27D2 * t47 - 0.122D3 / 0.27D2 *
     # t46 + (0.430D3 / 0.27D2 * t87 + 0.430D3 / 0.27D2 * t96 + 0.10D2 /
     # 0.27D2 * t94 + 0.328D3 / 0.27D2 * t92 + 0.10D2 / 0.27D2 * t90 + 0
     #.328D3 / 0.27D2 * t98) * t32 + t199 * t21
      t213 = 0.16D2 / 0.3D1 * t92
      t216 = 0.16D2 / 0.3D1 * t98
      t232 = (0.64D2 / 0.27D2 * t1 * t5 + 0.64D2 / 0.27D2 * t9 * t5 * t1
     #2) * t15 * t17 + (0.128D3 / 0.27D2 * t19 * t21 - 0.128D3 / 0.27D2 
     #* t9 * t21 * t12) * s * z + 0.28D2 / 0.27D2 * t1 * t32 * t21 * t34
     # + (t38 * t21 * S34 + 0.64D2 / 0.9D1 * t19 * t32 + (0.47D2 / 0.27D
     #2 * S13 + 0.47D2 / 0.27D2 * S23 + 0.4D1 / 0.3D1 * t49 * t32) * t21
     #) * S12 + 0.56D2 / 0.9D1 * t1 * t32 * t21 * t58 + (0.400D3 / 0.27D
     #2 * t19 * t32 + (-0.128D3 / 0.27D2 * S13 - 0.128D3 / 0.27D2 * S23 
     #+ 0.112D3 / 0.27D2 * t65 * t32) * t21) * S34 - 0.275D3 / 0.27D2 * 
     #S13 - 0.275D3 / 0.27D2 * S23 + (0.182D3 / 0.27D2 * t47 + 0.530D3 /
     # 0.27D2 * t45 + 0.182D3 / 0.27D2 * t46 + 0.530D3 / 0.27D2 * t48) *
     # t32 + (-0.148D3 / 0.27D2 * t46 - 0.94D2 / 0.27D2 * t7 - 0.94D2 / 
     #0.27D2 * t8 - 0.148D3 / 0.27D2 * t47 - 0.32D2 / 0.9D1 * t48 - 0.32
     #D2 / 0.9D1 * t45 + (-0.28D2 / 0.27D2 * t87 - 0.28D2 / 0.27D2 * t90
     # + 0.56D2 / 0.27D2 * t92 - 0.28D2 / 0.27D2 * t94 - 0.28D2 / 0.27D2
     # * t96 + 0.56D2 / 0.27D2 * t98) * t32) * t21 + t201 * t12 + ((-0.8
     #D1 / 0.3D1 * S13 - 0.8D1 / 0.3D1 * S23 + 0.8D1 / 0.3D1 * t65 * t32
     #) * t58 + (-0.8D1 * t48 + 0.8D1 / 0.3D1 * t47 - 0.8D1 * t45 + 0.8D
     #1 / 0.3D1 * t46 + (t213 - 0.8D1 / 0.3D1 * t90 - 0.8D1 / 0.3D1 * t9
     #6 + t216 - 0.8D1 / 0.3D1 * t87 - 0.8D1 / 0.3D1 * t94) * t32) * S34
     # - 0.28D2 / 0.3D1 * t87 + t213 - 0.28D2 / 0.3D1 * t96 - 0.4D1 / 0.
     #3D1 * t90 + t216 - 0.4D1 / 0.3D1 * t94 - t197 * t32) / t34
      rrqqbar2ggh21J1 = t232 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqqbar2ggh21J2
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
      t3 = S12 + S13 + S23
      t4 = t3 ** 2
      t5 = 0.1D1 / t4
      t7 = S13 ** 2
      t8 = S23 ** 2
      t9 = -t7 - t8
      t12 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t19 = -t1
      t20 = 0.256D3 / 0.27D2 * t19
      t21 = 0.1D1 / t3
      t32 = 0.1D1 / (S12 + S14 + S24)
      t34 = S12 ** 2
      t44 = S23 * S24
      t45 = S14 * S23
      t46 = S13 * S14
      t47 = S13 * S24
      t48 = t44 - t45 + t46 - t47
      t49 = 0.8D1 / 0.3D1 * t48
      t50 = t49 * t32
      t57 = S34 ** 2
      t85 = 0.640D3 / 0.27D2 * t47
      t86 = 0.640D3 / 0.27D2 * t45
      t123 = S24 ** 2
      t126 = S14 ** 2
      t134 = t7 * S14
      t136 = S13 * t126
      t138 = S23 * t123
      t140 = S23 * t126
      t142 = t47 * S14
      t144 = t8 * S23
      t146 = t7 * S13
      t148 = S13 * t123
      t150 = t7 * S24
      t152 = t8 * S14
      t154 = t44 * S14
      t156 = t8 * S24
      t158 = 0.256D3 / 0.27D2 * t134 - 0.62D2 / 0.27D2 * t136 - 0.62D2 /
     # 0.27D2 * t138 - 0.542D3 / 0.27D2 * t140 - 0.604D3 / 0.27D2 * t142
     # + 0.136D3 / 0.27D2 * t144 + 0.136D3 / 0.27D2 * t146 - 0.542D3 / 0
     #.27D2 * t148 + 0.256D3 / 0.27D2 * t150 + 0.256D3 / 0.27D2 * t152 -
     # 0.604D3 / 0.27D2 * t154 + 0.256D3 / 0.27D2 * t156
      t172 = -0.16D2 / 0.27D2 * t136 - 0.8D1 * t134 - 0.640D3 / 0.27D2 *
     # t150 - 0.608D3 / 0.27D2 * t154 - 0.2D1 * t144 - 0.2D1 * t146 - 0.
     #608D3 / 0.27D2 * t142 - 0.8D1 * t156 - 0.16D2 / 0.27D2 * t138 - 0.
     #592D3 / 0.27D2 * t140 - 0.592D3 / 0.27D2 * t148 - 0.640D3 / 0.27D2
     # * t152
      t174 = 0.16D2 / 0.3D1 * t19 * t32 * t21 * t57 * S34 + (0.64D2 / 0.
     #27D2 + 0.8D1 / 0.9D1 * t1 * t32 + (-0.88D2 / 0.9D1 * S13 - 0.88D2 
     #/ 0.9D1 * S23 + t50) * t21) * t57 + (0.64D2 / 0.27D2 * S24 + 0.64D
     #2 / 0.27D2 * S14 + 0.752D3 / 0.27D2 * S13 + 0.752D3 / 0.27D2 * S23
     # + (-0.544D3 / 0.27D2 * t47 - 0.256D3 / 0.27D2 * t44 + 0.8D1 * t7 
     #+ 0.8D1 * t8 - 0.256D3 / 0.27D2 * t46 - 0.544D3 / 0.27D2 * t45) * 
     #t32 + (-t85 - 0.496D3 / 0.27D2 * t8 - 0.64D2 / 0.27D2 * t46 - 0.49
     #6D3 / 0.27D2 * t7 - t86 - 0.64D2 / 0.27D2 * t44) * t21) * S34 - 0.
     #490D3 / 0.27D2 * t8 + 0.32D2 / 0.27D2 * t123 - 0.490D3 / 0.27D2 * 
     #t7 + 0.32D2 / 0.27D2 * t126 + 0.190D3 / 0.27D2 * t47 + 0.134D3 / 0
     #.27D2 * t46 + 0.190D3 / 0.27D2 * t45 + 0.134D3 / 0.27D2 * t44 + 0.
     #64D2 / 0.27D2 * S24 * S14 + t158 * t32 + t172 * t21
      t201 = (0.128D3 / 0.27D2 * t1 * t5 + 0.128D3 / 0.27D2 * t9 * t5 * 
     #t12) * t15 * t17 + (t20 * t21 - 0.256D3 / 0.27D2 * t9 * t21 * t12)
     # * s * z + 0.56D2 / 0.27D2 * t1 * t32 * t21 * t34 + (t20 * t32 * t
     #21 * S34 + 0.10D2 / 0.9D1 * t19 * t32 + (-0.2D1 * S13 - 0.2D1 * S2
     #3 + t50) * t21) * S12 + 0.344D3 / 0.27D2 * t1 * t32 * t21 * t57 + 
     #(0.32D2 / 0.3D1 * t19 * t32 + (0.496D3 / 0.27D2 * S23 + 0.496D3 / 
     #0.27D2 * S13 - 0.16D2 / 0.3D1 * t48 * t32) * t21) * S34 + 0.52D2 /
     # 0.9D1 * S23 + 0.52D2 / 0.9D1 * S13 + (-0.4D1 * t46 + 0.256D3 / 0.
     #27D2 * t7 + 0.256D3 / 0.27D2 * t8 - 0.4D1 * t44 - 0.148D3 / 0.9D1 
     #* t45 - 0.148D3 / 0.9D1 * t47) * t32 + (0.8D1 * t46 + 0.8D1 * t44 
     #+ 0.4D1 * t7 + 0.4D1 * t8 + t85 + t86) * t21 + t174 * t12 + ((0.8D
     #1 / 0.3D1 * S23 + 0.8D1 / 0.3D1 * S13 - t49 * t32) * t57 + (-0.8D1
     # / 0.3D1 * t8 - 0.8D1 / 0.3D1 * t7 + (-0.8D1 / 0.3D1 * t152 + 0.8D
     #1 / 0.3D1 * t134 + 0.8D1 / 0.3D1 * t156 - 0.8D1 / 0.3D1 * t150) * 
     #t32) * S34 - 0.8D1 / 0.3D1 * t146 - 0.8D1 / 0.3D1 * t144 + (0.8D1 
     #/ 0.3D1 * t144 * S24 - 0.8D1 / 0.3D1 * t146 * S24 + 0.8D1 / 0.3D1 
     #* t146 * S14 - 0.8D1 / 0.3D1 * t144 * S14) * t32) / t34
      rrqqbar2ggh21J2 = t201 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrqqbar2ggh21J3
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
      t1 = -S23 - S13
      t2 = 0.64D2 / 0.27D2 * t1
      t3 = S12 + S13 + S23
      t4 = t3 ** 2
      t5 = 0.1D1 / t4
      t7 = S13 ** 2
      t8 = S23 ** 2
      t9 = t7 + t8
      t12 = 0.1D1 / S12
      t15 = s ** 2
      t17 = z ** 2
      t19 = -t1
      t21 = 0.1D1 / t3
      t32 = 0.1D1 / (S12 + S14 + S24)
      t33 = 0.16D2 / 0.3D1 * t1 * t32
      t34 = S12 ** 2
      t47 = S34 ** 2
      t58 = S14 * S23
      t60 = S13 * S24
      t62 = S13 * S14
      t66 = S23 * S24
      t70 = 0.128D3 / 0.27D2 * t66
      t72 = 0.8D1 / 0.27D2 * t7
      t73 = 0.8D1 / 0.27D2 * t8
      t75 = 0.128D3 / 0.27D2 * t62
      t108 = S24 ** 2
      t109 = S23 * t108
      t111 = S14 ** 2
      t112 = S23 * t111
      t114 = S13 * t108
      t116 = S13 * t111
      t120 = t66 * S14
      t126 = t60 * S14
      t144 = (t2 * t5 + 0.64D2 / 0.27D2 * t9 * t5 * t12) * t15 * t17 + (
     #0.512D3 / 0.27D2 * t19 * t21 - 0.128D3 / 0.27D2 * t9 * t21 * t12) 
     #* s * z + t33 * t21 * t34 + (0.32D2 / 0.3D1 * t19 * t32 * t21 * S3
     #4 + 0.28792D5 / 0.27D2 * t1 * t32 + 0.28D2 / 0.3D1 * t1 * t21) * S
     #12 + t33 * t21 * t47 + (0.74288D5 / 0.27D2 * t19 * t32 + 0.8D1 / 0
     #.27D2 * t1 * t21) * S34 + 0.28372D5 / 0.27D2 * S23 + 0.28372D5 / 0
     #.27D2 * S13 + (-0.536D3 / 0.27D2 * t58 - 0.536D3 / 0.27D2 * t60 - 
     #0.29128D5 / 0.27D2 * t62 + 0.8416D4 / 0.9D1 * t7 + 0.8416D4 / 0.9D
     #1 * t8 - 0.29128D5 / 0.27D2 * t66) * t32 + (-t70 - 0.256D3 / 0.27D
     #2 * t60 - t72 - t73 - 0.256D3 / 0.27D2 * t58 - t75) * t21 + ((0.32
     #D2 / 0.27D2 * t19 * t32 - t2 * t21) * t47 + (-0.74296D5 / 0.27D2 *
     # S13 - 0.74296D5 / 0.27D2 * S23 + (0.80D2 / 0.27D2 * t7 + 0.24736D
     #5 / 0.9D1 * t60 + 0.80D2 / 0.27D2 * t8 + 0.74320D5 / 0.27D2 * t66 
     #+ 0.74320D5 / 0.27D2 * t62 + 0.24736D5 / 0.9D1 * t58) * t32 + (t70
     # + t75 + 0.128D3 / 0.27D2 * t60 + t72 + 0.128D3 / 0.27D2 * t58 + t
     #73) * t21) * S34 - 0.3136D4 / 0.3D1 * t60 - 0.25220D5 / 0.27D2 * t
     #7 - 0.3136D4 / 0.3D1 * t58 + 0.304D3 / 0.27D2 * t62 - 0.25220D5 / 
     #0.27D2 * t8 + 0.304D3 / 0.27D2 * t66 + (0.8432D4 / 0.9D1 * t8 * S2
     #4 - 0.304D3 / 0.27D2 * t109 + 0.28208D5 / 0.27D2 * t112 + 0.28208D
     #5 / 0.27D2 * t114 - 0.304D3 / 0.27D2 * t116 + 0.8432D4 / 0.9D1 * t
     #7 * S14 + 0.27920D5 / 0.27D2 * t120 + 0.25264D5 / 0.27D2 * t7 * S2
     #4 + 0.25264D5 / 0.27D2 * t8 * S14 + 0.27920D5 / 0.27D2 * t126) * t
     #32 + (0.4D1 / 0.27D2 * t8 * S23 + 0.4D1 / 0.27D2 * t7 * S13 + 0.12
     #8D3 / 0.27D2 * t120 + 0.64D2 / 0.27D2 * t109 + 0.64D2 / 0.27D2 * t
     #116 + 0.64D2 / 0.27D2 * t114 + 0.128D3 / 0.27D2 * t126 + 0.64D2 / 
     #0.27D2 * t112) * t21) * t12
      rrqqbar2ggh21J3 = t144 / pi * wd / z

      end function
  
 