  
      subroutine rrgg2qqbarht1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarh11J1  
      doubleprecision rrgg2qqbarh11J2  
      doubleprecision rrgg2qqbarh11J3  
      doubleprecision rrgg2qqbarh11J4  
      doubleprecision rrgg2qqbarh11J5  
      doubleprecision rrgg2qqbarh11J6  
      doubleprecision rrgg2qqbarh11J7  
      doubleprecision rrgg2qqbarht1s1e1  
      doubleprecision rrgg2qqbarht1s1e0  
      doubleprecision rrgg2qqbarht1s1em1  
      doubleprecision rrgg2qqbarht1s1em2  
      doubleprecision rrgg2qqbarht1s1em3  
      doubleprecision rrgg2qqbarht1s1em4  
      doubleprecision rrgg2qqbarht1s2e1  
      doubleprecision rrgg2qqbarht1s2e0  
      doubleprecision rrgg2qqbarht1s2em1  
      doubleprecision rrgg2qqbarht1s2em2  
      doubleprecision rrgg2qqbarht1s2em3  
      doubleprecision rrgg2qqbarht1s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht1s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht1s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht1s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht1s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht1s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht1s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht1s1e1
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
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7

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
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0
     #, 0.0D0, 0.0D0)
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x2 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t15 * x3
      t17 = t16 * t4
      t20 = log(-0.4D1 * t13 * t17)
      t21 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D
     #0, 0.0D0, 0.0D0)
      t23 = t20 ** 2
      t24 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D
     #0, 0.0D0, 0.0D0)
      t30 = pi * lh
      t36 = lh ** 2
      t38 = pi ** 2
      t40 = 0.180D3 * t36 - 0.30D2 * t38
      t41 = pi * t40
      t42 = t7 * t24
      t43 = t41 * t42
      t45 = 0.1D1 / x2
      t48 = t12 * t15
      t49 = x3 * t4
      t52 = log(-0.4D1 * t48 * t49)
      t53 = t52 * pi
      t56 = t52 ** 2
      t57 = t56 * pi
      t63 = rrgg2qqbarh11J4(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D
     #0, 0.0D0, 0.0D0)
      t89 = x1 ** 2
      t90 = x2 * t89
      t91 = t90 * t12
      t94 = log(-0.4D1 * t91 * t17)
      t102 = 0.1D1 / x1
      t106 = t89 * t12
      t109 = log(-0.4D1 * t106 * t17)
      t111 = t109 ** 2
      t125 = -(0.90D2 * t8 * (-t9 + t20 * t21 - t23 * t24 / 0.2D1) - 0.1
     #80D3 * t30 * t7 * (-t21 + t20 * t24) - t43) * t45 / 0.1440D4 + (0.
     #180D3 * t53 * lh + 0.45D2 * t57 + t41) * t7 * t21 / 0.1440D4 + t8 
     #* t63 / 0.16D2 + (-0.90D2 * t57 * lh + pi * (0.60D2 * lh * t38 - 0
     #.240D3 * zeta3 - 0.120D3 * t36 * lh) - 0.15D2 * t56 * t52 * pi - t
     #53 * t40) * t7 * t24 / 0.1440D4 + (-0.180D3 * t30 - 0.90D2 * t53) 
     #* t7 * t9 / 0.1440D4 - (0.90D2 * t8 * (-t21 + t94 * t24) + 0.180D3
     # * t30 * t42) * t102 * t45 / 0.720D3 + (0.90D2 * t8 * (t9 - t109 *
     # t21 + t111 * t24 / 0.2D1) - 0.180D3 * t30 * t7 * (t21 - t109 * t2
     #4) + t43) * t102 / 0.720D3
      t126 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t125)
      t128 = -0.1D1 + x1
      t129 = x3 * x1
      t130 = t129 * z
      t132 = 0.2D1 * x2 * x3
      t133 = t129 * x2
      t134 = x2 * z
      t135 = t129 * t134
      t136 = cos(t10)
      t137 = -0.1D1 + x2
      t139 = x1 * z
      t140 = 0.1D1 - x1 + t139
      t144 = Sqrt(x3 * t137 * t140 * x2 * t4)
      t146 = 0.2D1 * t136 * t144
      t149 = 0.1D1 / t140
      t151 = t2 * t128 * (-x3 + t129 - t130 + t132 - t133 + t135 - x2 + 
     #t146) * t149
      t152 = t2 * t129
      t153 = x1 * x2
      t154 = t153 * z
      t155 = 0.1D1 - x1 + t139 - x2 + t153 - t154 - x3 + t129 - t130 + t
     #132 - t133 + t135 + t146
      t158 = t2 * t128 * t155 * t149
      t160 = t2 * x1 * t4
      t161 = t1 ** 2
      t166 = s * t161 * x2 * x1 * t128 * t149
      t168 = 0.1D1 / (-0.1D1 + x1 - t139 + x2 - t153 - t134 + t154)
      t169 = t140 * t168
      t170 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, s, t151, -t158,
     # t152, -t160, -t166)
      t172 = t90 * t48
      t173 = t128 ** 2
      t174 = t149 * t173
      t179 = log(0.4D1 * t172 * t49 * t174 * t137)
      t181 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, t151, -t158,
     # t152, -t160, -t166)
      t191 = 0.90D2 * t8 * (t169 * t170 - t179 * t140 * t168 * t181) - 0
     #.180D3 * t30 * t7 * t169 * t181
      t195 = FJET(XB1, XB2, s, t151, t152, -t158, -t160, -t166, -t191 * 
     #t102 * t45 / 0.720D3)
      t202 = Sqrt(x2 * t137 * t49)
      t204 = 0.2D1 * t136 * t202
      t206 = t2 * (-x3 + t132 - x2 + t204)
      t208 = t2 * (0.1D1 - x2 - x3 + t132 + t204)
      t210 = 0.1D1 / (0.1D1 - x2 + t134)
      t211 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, s, -t206, t208,
     # 0.0D0, 0.0D0, 0.0D0)
      t217 = log(0.4D1 * t13 * t15 * t49 * t137)
      t218 = t217 * t210
      t219 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, s, -t206, t208,
     # 0.0D0, 0.0D0, 0.0D0)
      t221 = t217 ** 2
      t223 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t206, t208,
     # 0.0D0, 0.0D0, 0.0D0)
      t229 = t210 * t219
      t236 = t7 * t210 * t223
      t245 = log(0.4D1 * t91 * t16 * t4 * t137)
      t257 = -(0.90D2 * t8 * (t210 * t211 - t218 * t219 + t221 * t210 * 
     #t223 / 0.2D1) - 0.180D3 * t30 * t7 * (t229 - t218 * t223) + t41 * 
     #t236) * t45 / 0.1440D4 - (0.90D2 * t8 * (t229 - t245 * t210 * t223
     #) - 0.180D3 * t30 * t236) * t102 * t45 / 0.720D3
      t258 = FJET(XB1, XB2, s, -t206, 0.0D0, t208, 0.0D0, 0.0D0, t257)
      t261 = t2 * t128 * x3
      t263 = t2 * t128 * t4
      t264 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, s, -t261, t263,
     # t152, -t160, 0.0D0)
      t265 = t49 * t174
      t268 = log(-0.4D1 * t172 * t265)
      t269 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t261, t263,
     # t152, -t160, 0.0D0)
      t274 = t7 * t269
      t280 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, s, -t261, t263,
     # t152, -t160, 0.0D0)
      t284 = log(-0.4D1 * t106 * t15 * t265)
      t286 = t284 ** 2
      t301 = -(0.90D2 * t8 * (t264 - t268 * t269) - 0.180D3 * t30 * t274
     #) * t102 * t45 / 0.720D3 + (-0.90D2 * t8 * (t280 - t284 * t264 + t
     #286 * t269 / 0.2D1) + 0.180D3 * t30 * t7 * (t264 - t284 * t269) - 
     #t41 * t274) * t102 / 0.720D3
      t302 = FJET(XB1, XB2, s, -t261, t152, t263, -t160, 0.0D0, t301)
      rrgg2qqbarht1s1e1 = t126 * t125 - t195 * t191 * t102 * t45 / 0.720
     #D3 + t258 * t257 + t302 * t301

      end function



      doubleprecision function rrgg2qqbarht1s1e0
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
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7

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
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0
     #, 0.0D0, 0.0D0)
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x2 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = t15 * x3 * t4
      t20 = log(-0.4D1 * t13 * t17)
      t21 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D
     #0, 0.0D0, 0.0D0)
      t26 = pi * lh
      t29 = 0.180D3 * t26 * t7 * t21
      t31 = 0.1D1 / x2
      t34 = 0.1D1 / x1
      t39 = x1 ** 2
      t40 = t39 * t12
      t43 = log(-0.4D1 * t40 * t17)
      t51 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D
     #0, 0.0D0, 0.0D0)
      t56 = x3 * t4
      t59 = log(-0.4D1 * t12 * t15 * t56)
      t60 = t59 * pi
      t68 = t59 ** 2
      t71 = lh ** 2
      t73 = pi ** 2
      t81 = -(0.90D2 * t8 * (-t9 + t20 * t21) + t29) * t31 / 0.1440D4 + 
     #t8 * t21 * t34 * t31 / 0.8D1 + (0.90D2 * t8 * (t9 - t43 * t21) - t
     #29) * t34 / 0.720D3 + t8 * t51 / 0.16D2 + (-0.180D3 * t26 - 0.90D2
     # * t60) * t7 * t9 / 0.1440D4 + (0.180D3 * t60 * lh + 0.45D2 * t68 
     #* pi + pi * (0.180D3 * t71 - 0.30D2 * t73)) * t7 * t21 / 0.1440D4
      t82 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t81)
      t84 = -0.1D1 + x1
      t85 = x3 * x1
      t86 = t85 * z
      t88 = 0.2D1 * x2 * x3
      t89 = t85 * x2
      t90 = x2 * z
      t91 = t85 * t90
      t92 = cos(t10)
      t93 = -0.1D1 + x2
      t95 = x1 * z
      t96 = 0.1D1 - x1 + t95
      t100 = Sqrt(x3 * t93 * t96 * x2 * t4)
      t102 = 0.2D1 * t92 * t100
      t105 = 0.1D1 / t96
      t107 = t2 * t84 * (-x3 + t85 - t86 + t88 - t89 + t91 - x2 + t102) 
     #* t105
      t108 = t2 * t85
      t109 = x1 * x2
      t110 = t109 * z
      t111 = 0.1D1 - x1 + t95 - x2 + t109 - t110 - x3 + t85 - t86 + t88 
     #- t89 + t91 + t102
      t114 = t2 * t84 * t111 * t105
      t116 = t2 * x1 * t4
      t117 = t1 ** 2
      t122 = s * t117 * x2 * x1 * t84 * t105
      t126 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, t107, -t114,
     # t108, -t116, -t122)
      t129 = 0.1D1 / (-0.1D1 + x1 - t95 + x2 - t109 - t90 + t110) * t126
     # * t34 * t31
      t132 = FJET(XB1, XB2, s, t107, t108, -t114, -t116, -t122, -t8 * t9
     #6 * t129 / 0.8D1)
      t140 = Sqrt(x2 * t93 * t56)
      t142 = 0.2D1 * t92 * t140
      t144 = t2 * (-x3 + t88 - x2 + t142)
      t146 = t2 * (0.1D1 - x2 - x3 + t88 + t142)
      t148 = 0.1D1 / (0.1D1 - x2 + t90)
      t149 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, s, -t144, t146,
     # 0.0D0, 0.0D0, 0.0D0)
      t155 = log(0.4D1 * t13 * t15 * t56 * t93)
      t157 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t144, t146,
     # 0.0D0, 0.0D0, 0.0D0)
      t174 = -(0.90D2 * t8 * (t148 * t149 - t155 * t148 * t157) - 0.180D
     #3 * t26 * t7 * t148 * t157) * t31 / 0.1440D4 - t8 * t148 * t157 * 
     #t34 * t31 / 0.8D1
      t175 = FJET(XB1, XB2, s, -t144, 0.0D0, t146, 0.0D0, 0.0D0, t174)
      t178 = t2 * t84 * x3
      t180 = t2 * t84 * t4
      t181 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t178, t180,
     # t108, -t116, 0.0D0)
      t186 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, s, -t178, t180,
     # t108, -t116, 0.0D0)
      t188 = t84 ** 2
      t193 = log(-0.4D1 * t40 * t15 * t56 * t105 * t188)
      t204 = -t8 * t181 * t34 * t31 / 0.8D1 + (-0.90D2 * t8 * (t186 - t1
     #93 * t181) + 0.180D3 * t26 * t7 * t181) * t34 / 0.720D3
      t205 = FJET(XB1, XB2, s, -t178, t108, t180, -t116, 0.0D0, t204)
      rrgg2qqbarht1s1e0 = t82 * t81 - t132 * pi * t7 * t96 * t129 / 0.8D
     #1 + t175 * t174 + t205 * t204

      end function



      doubleprecision function rrgg2qqbarht1s1em1
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
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0
     #, 0.0D0, 0.0D0)
      t10 = 0.1D1 / x1
      t14 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D
     #0, 0.0D0, 0.0D0)
      t19 = x4 * pi
      t20 = Sin(t19)
      t21 = t20 ** 2
      t22 = z ** 2
      t25 = x3 * t4
      t28 = log(-0.4D1 * t21 / t22 * t25)
      t35 = 0.1D1 / x2
      t39 = t8 * t9 * t10 / 0.8D1 + t8 * t14 / 0.16D2 + (-0.180D3 * pi *
     # lh - 0.90D2 * t28 * pi) * t7 * t9 / 0.1440D4 + t8 * t9 * t35 / 0.
     #16D2
      t40 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t39)
      t42 = -0.1D1 + x1
      t44 = t2 * t42 * x3
      t46 = t2 * x1 * x3
      t48 = t2 * t42 * t4
      t50 = t2 * x1 * t4
      t51 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t44, t48, t4
     #6, -t50, 0.0D0)
      t55 = FJET(XB1, XB2, s, -t44, t46, t48, -t50, 0.0D0, -t8 * t51 * t
     #10 / 0.8D1)
      t62 = 0.2D1 * x2 * x3
      t63 = cos(t19)
      t67 = Sqrt(x2 * (-0.1D1 + x2) * t25)
      t69 = 0.2D1 * t63 * t67
      t71 = t2 * (-x3 + t62 - x2 + t69)
      t73 = t2 * (0.1D1 - x2 - x3 + t62 + t69)
      t77 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t71, t73, 0.
     #0D0, 0.0D0, 0.0D0)
      t79 = 0.1D1 / (0.1D1 - x2 + x2 * z) * t77 * t35
      t82 = FJET(XB1, XB2, s, -t71, 0.0D0, t73, 0.0D0, 0.0D0, -t8 * t79 
     #/ 0.16D2)
      rrgg2qqbarht1s1em1 = t40 * t39 - t55 * pi * t7 * t51 * t10 / 0.8D1
     # - t82 * pi * t7 * t79 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht1s1em2
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
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = t2 * x3
      t5 = t2 * (-0.1D1 + x3)
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t9 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0
     #, 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, pi * t7 * t9
     # / 0.16D2)
      rrgg2qqbarht1s1em2 = t12 * pi * t7 * t9 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht1s1em3
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
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht1s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht1s1em4
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
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht1s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht1s2e1
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
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7

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
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t3, -t5, 0.0D0)
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x2 * t12
      t14 = x4 * pi
      t15 = Sin(t14)
      t16 = t15 ** 2
      t18 = x3 * t4
      t19 = -0.1D1 + x2
      t23 = log(0.4D1 * t13 * t16 * t18 * t19)
      t24 = t23 ** 2
      t29 = log(-0.4D1 * t13 * t16 * x3 * t4)
      t30 = t29 ** 2
      t36 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t39 = pi * lh
      t40 = t7 * t9
      t47 = 0.1D1 / x2
      t53 = log(-0.4D1 * t12 * t16 * t18)
      t54 = t53 * pi
      t57 = t53 ** 2
      t58 = t57 * pi
      t60 = lh ** 2
      t62 = pi ** 2
      t64 = -0.180D3 * t60 + 0.30D2 * t62
      t65 = pi * t64
      t70 = rrgg2qqbarh11J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t94 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t97 = x1 ** 2
      t98 = x2 * t97
      t99 = t98 * t16
      t100 = t12 * x3
      t101 = t100 * t4
      t104 = log(-0.4D1 * t99 * t101)
      t110 = log(0.4D1 * t99 * t100 * t4 * t19)
      t113 = 0.1D1 / x1
      t118 = t97 * t16
      t121 = log(-0.4D1 * t118 * t101)
      t123 = t121 ** 2
      t138 = (-0.90D2 * t8 * t9 * (t24 / 0.2D1 - t30 / 0.2D1) + (-0.90D2
     # * t8 * t36 + 0.180D3 * t40 * t39) * (-t23 + t29)) * t47 / 0.1440D
     #4 - (-0.180D3 * t54 * lh - 0.45D2 * t58 + t65) * t7 * t36 / 0.1440
     #D4 + t8 * t70 / 0.16D2 - (0.90D2 * t58 * lh + pi * (-0.60D2 * lh *
     # t62 + 0.240D3 * zeta3 + 0.120D3 * t60 * lh) + 0.15D2 * t57 * t53 
     #* pi - t54 * t64) * t7 * t9 / 0.1440D4 - (0.180D3 * t39 + 0.90D2 *
     # t54) * t7 * t94 / 0.1440D4 + t8 * (-t104 * t9 + t110 * t9) * t113
     # * t47 / 0.8D1 + (-0.90D2 * t8 * (-t94 + t121 * t36 - t123 * t9 / 
     #0.2D1) + 0.180D3 * t39 * t7 * (-t36 + t121 * t9) - t65 * t40) * t1
     #13 / 0.720D3
      t139 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t138)
      t141 = x3 * x1
      t142 = t2 * t141
      t143 = -0.1D1 + x1
      t145 = t2 * t143 * x3
      t147 = t2 * x1 * t4
      t149 = t2 * t143 * t4
      t150 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, s, t142, -t147,
     # -t145, t149, 0.0D0)
      t151 = 0.1D1 / t10
      t153 = t98 * t16 * t151
      t154 = x1 * z
      t155 = -z - x1 + t154
      t156 = 0.1D1 / t155
      t157 = t143 ** 2
      t158 = t156 * t157
      t159 = t18 * t158
      t162 = log(0.4D1 * t153 * t159)
      t163 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, t142, -t147,
     # -t145, t149, 0.0D0)
      t168 = t7 * t163
      t174 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, s, t142, -t147,
     # -t145, t149, 0.0D0)
      t178 = log(0.4D1 * t118 * t151 * t159)
      t180 = t178 ** 2
      t195 = -(-0.90D2 * t8 * (-t150 + t162 * t163) - 0.180D3 * t39 * t1
     #68) * t113 * t47 / 0.720D3 + (-0.90D2 * t8 * (t174 - t178 * t150 +
     # t180 * t163 / 0.2D1) + 0.180D3 * t39 * t7 * (t150 - t178 * t163) 
     #+ t65 * t168) * t113 / 0.720D3
      t196 = FJET(XB1, XB2, s, t142, -t145, -t147, t149, 0.0D0, t195)
      t198 = x3 * z
      t199 = t141 * z
      t200 = x2 * x3
      t201 = t200 * z
      t202 = t141 * x2
      t203 = x2 * z
      t204 = t141 * t203
      t205 = cos(t14)
      t210 = Sqrt(-x3 * t19 * t155 * x2 * t4)
      t212 = 0.2D1 * t205 * t210
      t216 = t2 * x1 * (-t198 - t141 + t199 + t201 + t202 - t204 - x2 + 
     #t200 + t212) * t156
      t217 = x1 * x2
      t218 = t217 * z
      t219 = z + x1 - t154 - t203 - t217 + t218 - t198 - t141 + t199 + t
     #201 + t202 - t204 + t200 + t212
      t222 = t2 * x1 * t219 * t156
      t223 = t1 ** 2
      t228 = s * t223 * x2 * x1 * t143 * t156
      t230 = 0.1D1 / (z + x1 - t154 - t217 + t218)
      t231 = t155 * t230
      t232 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, s, t216, -t222,
     # -t145, t149, t228)
      t238 = log(-0.4D1 * t153 * t18 * t158 * t19)
      t240 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, t216, -t222,
     # -t145, t149, t228)
      t250 = 0.90D2 * t8 * (t231 * t232 - t238 * t155 * t230 * t240) - 0
     #.180D3 * t39 * t7 * t231 * t240
      t254 = FJET(XB1, XB2, s, t216, -t145, -t222, t149, t228, -t250 * t
     #113 * t47 / 0.720D3)
      rrgg2qqbarht1s2e1 = t139 * t138 + t196 * t195 - t254 * t250 * t113
     # * t47 / 0.720D3

      end function



      doubleprecision function rrgg2qqbarht1s2e0
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
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7

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
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2qqbarh11J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t3, -t5, 0.0D0)
      t12 = pi * lh
      t14 = z ** 2
      t16 = 0.1D1 / t14 / z
      t17 = x4 * pi
      t18 = Sin(t17)
      t19 = t18 ** 2
      t21 = x3 * t4
      t24 = log(-0.4D1 * t16 * t19 * t21)
      t25 = t24 * pi
      t29 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t34 = t24 ** 2
      t37 = lh ** 2
      t39 = pi ** 2
      t45 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t48 = x2 * t16
      t50 = -0.1D1 + x2
      t54 = log(0.4D1 * t48 * t19 * t21 * t50)
      t59 = log(-0.4D1 * t48 * t19 * x3 * t4)
      t62 = 0.1D1 / x2
      t66 = x1 ** 2
      t67 = t66 * t19
      t72 = log(-0.4D1 * t67 * t16 * x3 * t4)
      t81 = 0.1D1 / x1
      t84 = t8 * t9 / 0.16D2 - (0.180D3 * t12 + 0.90D2 * t25) * t7 * t29
     # / 0.1440D4 - (-0.180D3 * t25 * lh - 0.45D2 * t34 * pi + pi * (-0.
     #180D3 * t37 + 0.30D2 * t39)) * t7 * t45 / 0.1440D4 - t8 * t45 * (-
     #t54 + t59) * t62 / 0.16D2 + (-0.90D2 * t8 * (-t29 + t72 * t45) - 0
     #.180D3 * t12 * t7 * t45) * t81 / 0.720D3
      t85 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t84)
      t87 = x3 * x1
      t88 = t2 * t87
      t89 = -0.1D1 + x1
      t91 = t2 * t89 * x3
      t93 = t2 * x1 * t4
      t95 = t2 * t89 * t4
      t96 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, t88, -t93, -t
     #91, t95, 0.0D0)
      t101 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, s, t88, -t93, -
     #t91, t95, 0.0D0)
      t104 = x1 * z
      t105 = -z - x1 + t104
      t106 = 0.1D1 / t105
      t107 = t89 ** 2
      t112 = log(0.4D1 * t67 / t14 * t21 * t106 * t107)
      t123 = -t8 * t96 * t81 * t62 / 0.8D1 + (-0.90D2 * t8 * (t101 - t11
     #2 * t96) + 0.180D3 * t12 * t7 * t96) * t81 / 0.720D3
      t124 = FJET(XB1, XB2, s, t88, -t91, -t93, t95, 0.0D0, t123)
      t126 = x3 * z
      t127 = t87 * z
      t128 = x2 * x3
      t129 = t128 * z
      t130 = t87 * x2
      t131 = x2 * z
      t132 = t87 * t131
      t133 = cos(t17)
      t138 = Sqrt(-x3 * t50 * t105 * x2 * t4)
      t140 = 0.2D1 * t133 * t138
      t144 = t2 * x1 * (-t126 - t87 + t127 + t129 + t130 - t132 - x2 + t
     #128 + t140) * t106
      t145 = x1 * x2
      t146 = t145 * z
      t147 = z + x1 - t104 - t131 - t145 + t146 - t126 - t87 + t127 + t1
     #29 + t130 - t132 + t128 + t140
      t150 = t2 * x1 * t147 * t106
      t151 = t1 ** 2
      t156 = s * t151 * x2 * x1 * t89 * t106
      t160 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, t144, -t150,
     # -t91, t95, t156)
      t163 = 0.1D1 / (z + x1 - t104 - t145 + t146) * t160 * t81 * t62
      t166 = FJET(XB1, XB2, s, t144, -t91, -t150, t95, t156, -t8 * t105 
     #* t163 / 0.8D1)
      rrgg2qqbarht1s2e0 = t85 * t84 + t124 * t123 - t166 * pi * t7 * t10
     #5 * t163 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht1s2em1
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
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2qqbarh11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t3, -t5, 0.0D0)
      t14 = z ** 2
      t18 = Sin(x4 * pi)
      t19 = t18 ** 2
      t24 = log(-0.4D1 / t14 / z * t19 * x3 * t4)
      t29 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t32 = 0.1D1 / x1
      t36 = t8 * t9 / 0.16D2 - (0.180D3 * pi * lh + 0.90D2 * t24 * pi) *
     # t7 * t29 / 0.1440D4 + t8 * t29 * t32 / 0.8D1
      t37 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t36)
      t40 = t2 * x1 * x3
      t41 = -0.1D1 + x1
      t43 = t2 * t41 * x3
      t45 = t2 * x1 * t4
      t47 = t2 * t41 * t4
      t48 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, t40, -t45, -t
     #43, t47, 0.0D0)
      t52 = FJET(XB1, XB2, s, t40, -t43, -t45, t47, 0.0D0, -t8 * t48 * t
     #32 / 0.8D1)
      rrgg2qqbarht1s2em1 = t37 * t36 - t52 * pi * t7 * t48 * t32 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht1s2em2
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
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = s * (-0.1D1 + z)
      t3 = t2 * x3
      t5 = t2 * (-0.1D1 + x3)
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t9 = rrgg2qqbarh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, 
     #t3, -t5, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, pi * t7 * t9
     # / 0.16D2)
      rrgg2qqbarht1s2em2 = t12 * pi * t7 * t9 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht1s2em3
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
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht1s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht1s2em4
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
      doubleprecision rrgg2qqbarh11J1
      doubleprecision rrgg2qqbarh11J2
      doubleprecision rrgg2qqbarh11J3
      doubleprecision rrgg2qqbarh11J4
      doubleprecision rrgg2qqbarh11J5
      doubleprecision rrgg2qqbarh11J6
      doubleprecision rrgg2qqbarh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarht1s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh11J1
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t5 = z ** 2
      t10 = nf * S34
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = t13 * nf
      t20 = S34 ** 2
      t21 = nf * t20
      t23 = -t12 - t14
      t27 = -S14 - S13
      t33 = -S23 - S24
      t38 = S13 * S24
      t40 = S23 * S24
      t41 = 0.1152D4 * t40
      t42 = S13 * S14
      t43 = 0.1152D4 * t42
      t44 = S13 ** 2
      t45 = 0.576D3 * t44
      t46 = S14 * S23
      t48 = S14 ** 2
      t49 = 0.576D3 * t48
      t50 = S23 ** 2
      t51 = 0.576D3 * t50
      t52 = S24 ** 2
      t53 = 0.576D3 * t52
      t54 = S24 * S14
      t56 = S23 * S13
      t63 = 0.288D3 * t48
      t64 = 0.144D3 * t52
      t65 = 0.288D3 * t44
      t66 = 0.144D3 * t50
      t67 = 0.288D3 * t56
      t68 = 0.288D3 * t40
      t69 = 0.288D3 * t54
      t70 = 0.288D3 * t38
      t71 = 0.288D3 * t46
      t75 = 0.144D3 * t48
      t76 = 0.288D3 * t52
      t77 = 0.144D3 * t44
      t78 = 0.288D3 * t50
      t79 = 0.288D3 * t42
      t85 = S12 ** 2
      t86 = 0.1D1 / t85
      t135 = 0.864D3 * t54
      t136 = 0.864D3 * t56
      t142 = 0.144D3 * t46
      t143 = 0.576D3 * t40
      t144 = 0.144D3 * t56
      t145 = 0.144D3 * t54
      t146 = 0.144D3 * t38
      t151 = 0.576D3 * t42
      t179 = 0.14976D5 * t38
      t180 = 0.14832D5 * t56
      t181 = 0.14832D5 * t54
      t182 = 0.14976D5 * t46
      rrgg2qqbarh11J1 = (0.6D1 * t1 * nf * t3 * s * t5 * z + (0.18D2 * t
     #10 - 0.18D2 * t12 - 0.18D2 * t14) * t1 * t3 * t5 + ((0.18D2 * t21 
     #+ 0.18D2 * t23 * S34 + (0.36D2 * t14 - 0.18D2 * t27 * nf) * t11 - 
     #0.18D2 * t33 * nf * t13 - (0.1152D4 * t38 + t41 + t43 - t45 + 0.11
     #52D4 * t46 - t49 - t51 - t53 - 0.1152D4 * t54 - 0.1152D4 * t56) * 
     #nf / 0.96D2) * t1 + (-(-t63 - t64 - t65 - t66 - t67 + t68 - t69 + 
     #t70 + t71) * nf * t11 / 0.96D2 - (-t75 - t76 - t77 - t78 + t71 - t
     #67 - t69 + t79 + t70) * nf * t13 / 0.96D2) * t86) * s * z + (-0.7D
     #1 / 0.3D1 * t10 + 0.15D2 * t12 + 0.15D2 * t14) * S12 + 0.4D1 / 0.3
     #D1 * t21 + 0.17D2 * t23 * S34 + (0.24D2 * t14 - 0.15D2 * t27 * nf)
     # * t11 - 0.15D2 * t33 * nf * t13 + (0.6D1 * nf * t20 * S34 - 0.173
     #3D4 / 0.48D2 * t23 * t20 + ((-0.329D3 / 0.4D1 * t14 - (0.1244D4 * 
     #S13 - 0.3523D4 * S24 + 0.1244D4 * S14 - 0.3523D4 * S23) * nf / 0.9
     #6D2) * t11 - (-0.3523D4 * S14 + 0.1244D4 * S24 - 0.3523D4 * S13 + 
     #0.1244D4 * S23) * nf * t13 / 0.96D2 - (-t51 - t49 - t45 + 0.864D3 
     #* t38 + t41 - t53 + 0.864D3 * t46 - t135 - t136 + t43) * nf / 0.96
     #D2) * S34 - (-t142 + t76 - t143 + t78 + t144 + t145 - t146) * nf *
     # t11 / 0.96D2 - (-t142 + t63 + t145 - t151 + t144 + t65 - t146) * 
     #nf * t13 / 0.96D2) * t1 + ((-(0.14832D5 * S14 + 0.144D3 * S24 + 0.
     #144D3 * S23 + 0.14832D5 * S13) * nf * t11 / 0.96D2 - (0.14832D5 * 
     #S23 + 0.144D3 * S13 + 0.144D3 * S14 + 0.14832D5 * S24) * nf * t13 
     #/ 0.96D2) * t20 + ((-(-0.14976D5 * S14 - 0.14976D5 * S13 - 0.14976
     #D5 * S23 - 0.14976D5 * S24) * nf * t13 / 0.96D2 - (t179 + t68 + t1
     #51 + t180 + t64 + t66 + t181 + t182) * nf / 0.96D2) * t11 - (t182 
     #+ t77 + t143 + t79 + t180 + t179 + t181 + t75) * nf * t13 / 0.96D2
     #) * S34 - (-0.864D3 * t40 - t135 - 0.864D3 * t42 - 0.1440D4 * t38 
     #- 0.1440D4 * t46 - t136 - 0.432D3 * t52 - 0.432D3 * t44 - 0.432D3 
     #* t50 - 0.432D3 * t48) * nf * t13 * t11 / 0.96D2) * t86) / pi * wd
     # / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh11J2
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t5 = z ** 2
      t10 = nf * S34
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = t13 * nf
      t20 = S34 ** 2
      t21 = nf * t20
      t23 = -t12 - t14
      t27 = -S14 - S13
      t33 = -S23 - S24
      t38 = S14 ** 2
      t39 = 0.1152D4 * t38
      t40 = S24 ** 2
      t41 = 0.1152D4 * t40
      t42 = S23 * S24
      t43 = 0.2304D4 * t42
      t44 = S13 ** 2
      t45 = 0.1152D4 * t44
      t46 = S23 ** 2
      t47 = 0.1152D4 * t46
      t48 = S13 * S24
      t50 = S13 * S14
      t51 = 0.2304D4 * t50
      t52 = S23 * S13
      t54 = S24 * S14
      t56 = S14 * S23
      t63 = 0.864D3 * t54
      t64 = 0.864D3 * t48
      t65 = 0.288D3 * t38
      t66 = 0.288D3 * t44
      t67 = 0.864D3 * t56
      t68 = 0.864D3 * t52
      t69 = 0.576D3 * t50
      t73 = 0.576D3 * t42
      t74 = 0.288D3 * t40
      t75 = 0.288D3 * t46
      t81 = S12 ** 2
      t82 = 0.1D1 / t81
      t93 = -t23
      t106 = t20 * S34
      t138 = S23 + S14 + S13 + S24
      t143 = 0.216D3 * t56
      t146 = 0.216D3 * t48
      t147 = 0.216D3 * t52
      t148 = 0.216D3 * t54
      t190 = 0.13968D5 * t54
      t193 = 0.288D3 * t50
      t194 = 0.13968D5 * t52
      t197 = 0.14400D5 * t56
      t198 = 0.14400D5 * t48
      t205 = 0.288D3 * t42
      rrgg2qqbarh11J2 = (0.6D1 * t1 * nf * t3 * s * t5 * z + (0.18D2 * t
     #10 - 0.18D2 * t12 - 0.18D2 * t14) * t1 * t3 * t5 + ((0.18D2 * t21 
     #+ 0.18D2 * t23 * S34 + (0.36D2 * t14 - 0.18D2 * t27 * nf) * t11 - 
     #0.18D2 * t33 * nf * t13 - (-t39 - t41 + t43 - t45 - t47 + 0.2304D4
     # * t48 + t51 - 0.2304D4 * t52 - 0.2304D4 * t54 + 0.2304D4 * t56) *
     # nf / 0.96D2) * t1 + (-(-t63 + t64 - t65 - t66 + t67 - t68 + t69) 
     #* nf * t11 / 0.96D2 - (-t63 - t68 + t73 - t74 - t75 + t64 + t67) *
     # nf * t13 / 0.96D2) * t82) * s * z + (0.29D2 / 0.3D1 * t10 + 0.15D
     #2 * t12 + 0.15D2 * t14) * S12 - 0.8D1 * t21 + 0.97D2 / 0.6D1 * t93
     # * S34 + (0.3D1 * t14 - 0.12D2 * t27 * nf) * t11 - 0.12D2 * t33 * 
     #nf * t13 + (0.25D2 / 0.3D1 * nf * t106 + 0.371D3 / 0.3D1 * t93 * t
     #20 + ((-0.1571D4 / 0.6D1 * t14 - (-0.1288D4 * S14 - 0.1288D4 * S13
     # - 0.12312D5 * S23 - 0.12312D5 * S24) * nf / 0.96D2) * t11 - (-0.1
     #2312D5 * S14 - 0.12312D5 * S13 - 0.1288D4 * S23 - 0.1288D4 * S24) 
     #* nf * t13 / 0.96D2 - (t43 - 0.1728D4 * t54 + 0.1728D4 * t48 + t51
     # + 0.1728D4 * t56 - t41 - t45 - t39 - t47 - 0.1728D4 * t52) * nf /
     # 0.96D2) * S34 + (-0.117D3 / 0.4D1 * t138 * nf * t13 - (t65 + t66 
     #+ 0.576D3 * t46 - t143 + 0.576D3 * t40 - 0.1152D4 * t42 + t69 - t1
     #46 + t147 + t148) * nf / 0.96D2) * t11 - (-0.1152D4 * t50 + t74 + 
     #0.576D3 * t44 + t75 + t73 + 0.576D3 * t38 + t148 - t146 - t143 + t
     #147) * nf * t13 / 0.96D2) * t1 + (0.129D3 / 0.4D1 * t93 * t106 + (
     #(-0.129D3 / 0.2D1 * t14 - (0.14112D5 * S13 + 0.14112D5 * S14 - 0.1
     #872D4 * S23 - 0.1872D4 * S24) * nf / 0.96D2) * t11 - (0.14112D5 * 
     #S23 - 0.1872D4 * S13 - 0.1872D4 * S14 + 0.14112D5 * S24) * nf * t1
     #3 / 0.96D2) * t20 + ((0.639D3 / 0.4D1 * t138 * nf * t13 - (-0.432D
     #3 * t44 + t190 + 0.1224D4 * t40 + 0.2448D4 * t42 + t193 + t194 + 0
     #.1224D4 * t46 - 0.432D3 * t38 + t197 + t198) * nf / 0.96D2) * t11 
     #- (-0.432D3 * t46 + t194 + t205 + 0.2448D4 * t50 + t197 + t198 + t
     #190 - 0.432D3 * t40 + 0.1224D4 * t44 + 0.1224D4 * t38) * nf * t13 
     #/ 0.96D2) * S34 - (t205 + 0.288D3 * t54 + t193 - 0.1152D4 * t48 - 
     #0.1152D4 * t56 + 0.288D3 * t52 + 0.144D3 * t40 + 0.144D3 * t44 + 0
     #.144D3 * t46 + 0.144D3 * t38) * nf * t13 * t11 / 0.96D2) * t82) / 
     #pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh11J3
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t5 = z ** 2
      t10 = nf * S34
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = t13 * nf
      t20 = S34 ** 2
      t21 = nf * t20
      t23 = -t12 - t14
      t27 = -S14 - S13
      t33 = -S23 - S24
      t38 = S13 * S24
      t40 = S23 * S24
      t41 = 0.3456D4 * t40
      t42 = S14 * S23
      t44 = S23 * S13
      t46 = S13 * S14
      t47 = 0.3456D4 * t46
      t48 = S13 ** 2
      t49 = 0.1728D4 * t48
      t50 = S24 * S14
      t52 = S14 ** 2
      t53 = 0.1728D4 * t52
      t54 = S23 ** 2
      t55 = 0.1728D4 * t54
      t56 = S24 ** 2
      t57 = 0.1728D4 * t56
      t66 = 0.1440D4 * t44
      t68 = 0.1440D4 * t50
      t69 = 0.1440D4 * t38
      t70 = 0.1440D4 * t42
      t71 = 0.1152D4 * t46
      t80 = 0.1152D4 * t40
      t87 = S12 ** 2
      t88 = 0.1D1 / t87
      t99 = -t23
      t112 = t20 * S34
      t144 = S23 + S14 + S13 + S24
      t150 = 0.864D3 * t54
      t151 = 0.288D3 * t42
      t152 = 0.864D3 * t56
      t154 = 0.288D3 * t38
      t155 = 0.288D3 * t44
      t156 = 0.288D3 * t50
      t164 = 0.864D3 * t48
      t166 = 0.864D3 * t52
      t200 = 0.13824D5 * t42
      t203 = 0.13104D5 * t44
      t204 = 0.13824D5 * t38
      t205 = 0.13104D5 * t50
      rrgg2qqbarh11J3 = (0.6D1 * t1 * nf * t3 * s * t5 * z + (0.18D2 * t
     #10 - 0.18D2 * t12 - 0.18D2 * t14) * t1 * t3 * t5 + ((0.18D2 * t21 
     #+ 0.18D2 * t23 * S34 + (0.36D2 * t14 - 0.18D2 * t27 * nf) * t11 - 
     #0.18D2 * t33 * nf * t13 - (0.3456D4 * t38 + t41 + 0.3456D4 * t42 -
     # 0.3456D4 * t44 + t47 - t49 - 0.3456D4 * t50 - t53 - t55 - t57) * 
     #nf / 0.96D2) * t1 + (-(-0.288D3 * t52 - 0.288D3 * t48 + 0.144D3 * 
     #t54 - t66 - 0.288D3 * t40 - t68 + t69 + t70 + t71 + 0.144D3 * t56)
     # * nf * t11 / 0.96D2 - (0.144D3 * t52 - 0.288D3 * t56 + 0.144D3 * 
     #t48 - 0.288D3 * t54 + t70 - t66 + t80 - 0.288D3 * t46 + t69 - t68)
     # * nf * t13 / 0.96D2) * t88) * s * z + (0.47D2 / 0.3D1 * t10 + 0.1
     #5D2 * t12 + 0.15D2 * t14) * S12 - 0.16D2 / 0.3D1 * t21 + 0.239D3 /
     # 0.6D1 * t99 * S34 + (-0.18D2 * t14 - 0.9D1 * t27 * nf) * t11 - 0.
     #9D1 * t33 * nf * t13 + (0.14D2 / 0.3D1 * nf * t112 + 0.3309D4 / 0.
     #16D2 * t99 * t20 + ((-0.5081D4 / 0.12D2 * t14 - (-0.3180D4 * S14 -
     # 0.3180D4 * S13 - 0.20321D5 * S23 - 0.20321D5 * S24) * nf / 0.96D2
     #) * t11 - (-0.20321D5 * S14 - 0.20321D5 * S13 - 0.3180D4 * S23 - 0
     #.3180D4 * S24) * nf * t13 / 0.96D2 - (t41 - 0.2592D4 * t50 + 0.259
     #2D4 * t38 + t47 + 0.2592D4 * t42 - t57 - t49 - t53 - t55 - 0.2592D
     #4 * t44) * nf / 0.96D2) * S34 + (-0.117D3 / 0.2D1 * t144 * nf * t1
     #3 - (0.576D3 * t52 + 0.576D3 * t48 + t150 - t151 + t152 - 0.1728D4
     # * t40 + t71 - t154 + t155 + t156) * nf / 0.96D2) * t11 - (-0.1728
     #D4 * t46 + 0.576D3 * t56 + t164 + 0.576D3 * t54 + t80 + t166 + t15
     #6 - t154 - t151 + t155) * nf * t13 / 0.96D2) * t1 + (0.129D3 / 0.2
     #D1 * t99 * t112 + ((-0.129D3 * t14 - (0.13392D5 * S13 + 0.13392D5 
     #* S14 - 0.3888D4 * S23 - 0.3888D4 * S24) * nf / 0.96D2) * t11 - (0
     #.13392D5 * S23 - 0.3888D4 * S13 - 0.3888D4 * S14 + 0.13392D5 * S24
     #) * nf * t13 / 0.96D2) * t20 + ((0.327D3 / 0.2D1 * t144 * nf * t13
     # - (0.4608D4 * t40 + t200 + 0.2304D4 * t54 - t166 - t164 + 0.2304D
     #4 * t56 + t203 + t204 + t205) * nf / 0.96D2) * t11 - (0.2304D4 * t
     #52 + 0.4608D4 * t46 - t150 + 0.2304D4 * t48 + t204 + t205 + t200 +
     # t203 - t152) * nf * t13 / 0.96D2) * S34 - (0.1440D4 * t40 + t68 +
     # 0.1440D4 * t46 - 0.864D3 * t38 - 0.864D3 * t42 + t66 + 0.720D3 * 
     #t54 + 0.720D3 * t52 + 0.720D3 * t56 + 0.720D3 * t48) * nf * t13 * 
     #t11 / 0.96D2) * t88) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh11J4
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t5 = z ** 2
      t10 = nf * S34
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = t13 * nf
      t20 = S34 ** 2
      t21 = nf * t20
      t23 = -t12 - t14
      t27 = -S14 - S13
      t33 = -S23 - S24
      t38 = S13 * S24
      t40 = S23 * S24
      t41 = 0.4608D4 * t40
      t42 = S14 * S23
      t44 = S23 * S13
      t46 = S13 * S14
      t47 = 0.4608D4 * t46
      t48 = S13 ** 2
      t49 = 0.2304D4 * t48
      t50 = S24 * S14
      t52 = S14 ** 2
      t53 = 0.2304D4 * t52
      t54 = S23 ** 2
      t55 = 0.2304D4 * t54
      t56 = S24 ** 2
      t57 = 0.2304D4 * t56
      t63 = 0.288D3 * t52
      t64 = 0.288D3 * t48
      t65 = 0.288D3 * t54
      t66 = 0.2016D4 * t44
      t68 = 0.2016D4 * t50
      t69 = 0.2016D4 * t38
      t70 = 0.2016D4 * t42
      t71 = 0.1728D4 * t46
      t72 = 0.288D3 * t56
      t76 = 0.1728D4 * t40
      t83 = S12 ** 2
      t84 = 0.1D1 / t83
      t95 = -t23
      t108 = t20 * S34
      t139 = S23 + S14 + S13 + S24
      t146 = 0.360D3 * t42
      t149 = 0.360D3 * t38
      t150 = 0.360D3 * t44
      t151 = 0.360D3 * t50
      t194 = 0.1296D4 * t48
      t195 = 0.12240D5 * t50
      t199 = 0.12240D5 * t44
      t201 = 0.1296D4 * t52
      t202 = 0.13248D5 * t42
      t203 = 0.13248D5 * t38
      t209 = 0.1296D4 * t54
      t212 = 0.1296D4 * t56
      rrgg2qqbarh11J4 = (0.6D1 * t1 * nf * t3 * s * t5 * z + (0.18D2 * t
     #10 - 0.18D2 * t12 - 0.18D2 * t14) * t1 * t3 * t5 + ((0.18D2 * t21 
     #+ 0.18D2 * t23 * S34 + (0.36D2 * t14 - 0.18D2 * t27 * nf) * t11 - 
     #0.18D2 * t33 * nf * t13 - (0.4608D4 * t38 + t41 + 0.4608D4 * t42 -
     # 0.4608D4 * t44 + t47 - t49 - 0.4608D4 * t50 - t53 - t55 - t57) * 
     #nf / 0.96D2) * t1 + (-(-t63 - t64 + t65 - t66 - 0.576D3 * t40 - t6
     #8 + t69 + t70 + t71 + t72) * nf * t11 / 0.96D2 - (t63 - t72 + t64 
     #- t65 + t70 - t66 + t76 - 0.576D3 * t46 + t69 - t68) * nf * t13 / 
     #0.96D2) * t84) * s * z + (0.65D2 / 0.3D1 * t10 + 0.15D2 * t12 + 0.
     #15D2 * t14) * S12 - 0.8D1 / 0.3D1 * t21 + 0.127D3 / 0.2D1 * t95 * 
     #S34 + (-0.39D2 * t14 - 0.6D1 * t27 * nf) * t11 - 0.6D1 * t33 * nf 
     #* t13 + (nf * t108 + 0.6959D4 / 0.24D2 * t95 * t20 + ((-0.585D3 * 
     #t14 - (-0.5072D4 * S14 - 0.5072D4 * S13 - 0.28330D5 * S23 - 0.2833
     #0D5 * S24) * nf / 0.96D2) * t11 - (-0.28330D5 * S14 - 0.28330D5 * 
     #S13 - 0.5072D4 * S23 - 0.5072D4 * S24) * nf * t13 / 0.96D2 - (0.34
     #56D4 * t38 + t41 + 0.3456D4 * t42 - 0.3456D4 * t44 + t47 - t49 - 0
     #.3456D4 * t50 - t53 - t55 - t57) * nf / 0.96D2) * S34 + (-0.351D3 
     #/ 0.4D1 * t139 * nf * t13 - (0.864D3 * t52 + 0.864D3 * t48 + 0.115
     #2D4 * t54 - t146 + 0.1152D4 * t56 - 0.2304D4 * t40 + t71 - t149 + 
     #t150 + t151) * nf / 0.96D2) * t11 - (-0.2304D4 * t46 + 0.864D3 * t
     #56 + 0.1152D4 * t48 + 0.864D3 * t54 + t76 + 0.1152D4 * t52 + t151 
     #- t149 - t146 + t150) * nf * t13 / 0.96D2) * t1 + (0.387D3 / 0.4D1
     # * t95 * t108 + ((-0.387D3 / 0.2D1 * t14 - (0.12672D5 * S13 + 0.12
     #672D5 * S14 - 0.5904D4 * S23 - 0.5904D4 * S24) * nf / 0.96D2) * t1
     #1 - (0.12672D5 * S23 - 0.5904D4 * S13 - 0.5904D4 * S14 + 0.12672D5
     # * S24) * nf * t13 / 0.96D2) * t20 + ((0.669D3 / 0.4D1 * t139 * nf
     # * t13 - (-t194 + t195 + 0.3384D4 * t56 + 0.6768D4 * t40 - 0.288D3
     # * t46 + t199 + 0.3384D4 * t54 - t201 + t202 + t203) * nf / 0.96D2
     #) * t11 - (-t209 + t199 - 0.288D3 * t40 + 0.6768D4 * t46 + t202 + 
     #t203 + t195 - t212 + 0.3384D4 * t48 + 0.3384D4 * t52) * nf * t13 /
     # 0.96D2) * S34 - (0.2592D4 * t40 + 0.2592D4 * t50 + 0.2592D4 * t46
     # - 0.576D3 * t38 - 0.576D3 * t42 + 0.2592D4 * t44 + t212 + t194 + 
     #t209 + t201) * nf * t13 * t11 / 0.96D2) * t84) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh11J5
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t5 = z ** 2
      t10 = nf * S34
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = t13 * nf
      t20 = S34 ** 2
      t23 = -t12 - t14
      t27 = -S14 - S13
      t33 = -S23 - S24
      t38 = S24 ** 2
      t39 = 0.2880D4 * t38
      t40 = S13 ** 2
      t41 = 0.2880D4 * t40
      t42 = S23 * S24
      t43 = 0.5760D4 * t42
      t44 = S13 * S14
      t45 = 0.5760D4 * t44
      t46 = S23 ** 2
      t47 = 0.2880D4 * t46
      t48 = S14 * S23
      t50 = S23 * S13
      t52 = S24 * S14
      t54 = S13 * S24
      t56 = S14 ** 2
      t57 = 0.2880D4 * t56
      t63 = 0.2592D4 * t52
      t65 = 0.2592D4 * t54
      t66 = 0.2592D4 * t50
      t67 = 0.2592D4 * t48
      t72 = 0.2304D4 * t44
      t81 = 0.2304D4 * t42
      t87 = S12 ** 2
      t88 = 0.1D1 / t87
      t98 = -t23
      t111 = t20 * S34
      t143 = S23 + S14 + S13 + S24
      t148 = 0.432D3 * t50
      t150 = 0.432D3 * t48
      t151 = 0.432D3 * t54
      t152 = 0.432D3 * t52
      t199 = 0.11376D5 * t52
      t203 = 0.11376D5 * t50
      t206 = 0.12672D5 * t48
      t207 = 0.12672D5 * t54
      rrgg2qqbarh11J5 = (0.6D1 * t1 * nf * t3 * s * t5 * z + (0.18D2 * t
     #10 - 0.18D2 * t12 - 0.18D2 * t14) * t1 * t3 * t5 + ((0.18D2 * nf *
     # t20 + 0.18D2 * t23 * S34 + (0.36D2 * t14 - 0.18D2 * t27 * nf) * t
     #11 - 0.18D2 * t33 * nf * t13 - (-t39 - t41 + t43 + t45 - t47 + 0.5
     #760D4 * t48 - 0.5760D4 * t50 - 0.5760D4 * t52 + 0.5760D4 * t54 - t
     #57) * nf / 0.96D2) * t1 + (-(-t63 - 0.864D3 * t42 + t65 - t66 + t6
     #7 + 0.432D3 * t46 - 0.288D3 * t56 + 0.432D3 * t38 - 0.288D3 * t40 
     #+ t72) * nf * t11 / 0.96D2 - (-0.288D3 * t38 + 0.432D3 * t40 - 0.2
     #88D3 * t46 + 0.432D3 * t56 - t63 + t65 + t67 - 0.864D3 * t44 - t66
     # + t81) * nf * t13 / 0.96D2) * t88) * s * z + (0.83D2 / 0.3D1 * t1
     #0 + 0.15D2 * t12 + 0.15D2 * t14) * S12 + 0.523D3 / 0.6D1 * t98 * S
     #34 + (-0.60D2 * t14 - 0.3D1 * t27 * nf) * t11 - 0.3D1 * t33 * nf *
     # t13 + (-0.8D1 / 0.3D1 * nf * t111 + 0.17909D5 / 0.48D2 * t98 * t2
     #0 + ((-0.8959D4 / 0.12D2 * t14 - (-0.6964D4 * S13 - 0.36339D5 * S2
     #4 - 0.6964D4 * S14 - 0.36339D5 * S23) * nf / 0.96D2) * t11 - (-0.6
     #964D4 * S24 - 0.36339D5 * S14 - 0.36339D5 * S13 - 0.6964D4 * S23) 
     #* nf * t13 / 0.96D2 - (t45 + 0.4320D4 * t48 + 0.4320D4 * t54 - 0.4
     #320D4 * t50 - t39 - 0.4320D4 * t52 - t41 - t57 - t47 + t43) * nf /
     # 0.96D2) * S34 + (-0.117D3 * t143 * nf * t13 - (0.1440D4 * t46 + t
     #148 - 0.2880D4 * t42 - t150 - t151 + t152 + 0.1152D4 * t56 + 0.144
     #0D4 * t38 + t72 + 0.1152D4 * t40) * nf / 0.96D2) * t11 - (-t151 - 
     #0.2880D4 * t44 + t152 + 0.1440D4 * t56 - t150 + t148 + t81 + 0.144
     #0D4 * t40 + 0.1152D4 * t38 + 0.1152D4 * t46) * nf * t13 / 0.96D2) 
     #* t1 + (0.129D3 * t98 * t111 + ((-0.258D3 * t14 - (0.11952D5 * S13
     # + 0.11952D5 * S14 - 0.7920D4 * S23 - 0.7920D4 * S24) * nf / 0.96D
     #2) * t11 - (0.11952D5 * S23 - 0.7920D4 * S13 - 0.7920D4 * S14 + 0.
     #11952D5 * S24) * nf * t13 / 0.96D2) * t20 + ((0.171D3 * t143 * nf 
     #* t13 - (-0.1728D4 * t40 + t199 + 0.4464D4 * t38 + 0.8928D4 * t42 
     #- 0.576D3 * t44 + t203 + 0.4464D4 * t46 - 0.1728D4 * t56 + t206 + 
     #t207) * nf / 0.96D2) * t11 - (-0.1728D4 * t46 + t203 - 0.576D3 * t
     #42 + 0.8928D4 * t44 + t206 + t207 + t199 - 0.1728D4 * t38 + 0.4464
     #D4 * t40 + 0.4464D4 * t56) * nf * t13 / 0.96D2) * S34 - (0.3744D4 
     #* t42 + 0.3744D4 * t52 + 0.3744D4 * t44 - 0.288D3 * t54 - 0.288D3 
     #* t48 + 0.3744D4 * t50 + 0.1872D4 * t38 + 0.1872D4 * t40 + 0.1872D
     #4 * t46 + 0.1872D4 * t56) * nf * t13 * t11 / 0.96D2) * t88) / pi *
     # wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh11J6
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
      t1 = 0.1D1 / S12
      t3 = s ** 2
      t5 = z ** 2
      t10 = nf * S34
      t11 = S13 + S14 + S34
      t12 = nf * t11
      t13 = S23 + S24 + S34
      t14 = t13 * nf
      t20 = S34 ** 2
      t21 = nf * t20
      t23 = t12 + t14
      t30 = (0.8640D4 * S14 + 0.8640D4 * S13) * nf / 0.96D2
      t37 = (0.8640D4 * S24 + 0.8640D4 * S23) * nf * t13 / 0.96D2
      t40 = S13 * S24
      t41 = 0.1440D4 * t40
      t42 = S24 ** 2
      t43 = 0.1440D4 * t42
      t44 = S23 * S13
      t45 = 0.1440D4 * t44
      t46 = S13 * S14
      t47 = 0.2880D4 * t46
      t48 = S14 * S23
      t49 = 0.1440D4 * t48
      t50 = S14 ** 2
      t51 = 0.1440D4 * t50
      t52 = S13 ** 2
      t53 = 0.1440D4 * t52
      t54 = S23 ** 2
      t55 = 0.1440D4 * t54
      t56 = S24 * S14
      t57 = 0.1440D4 * t56
      t58 = S23 * S24
      t59 = 0.2880D4 * t58
      t68 = S12 ** 2
      t69 = 0.1D1 / t68
      t85 = t20 * S34
      t110 = S23 + S14 + S13 + S24
      t114 = 0.360D3 * t56
      t115 = 0.360D3 * t48
      t116 = 0.360D3 * t40
      t117 = 0.360D3 * t44
      t155 = 0.78480D5 * t56
      t159 = 0.78480D5 * t44
      t162 = 0.77760D5 * t48
      t163 = 0.77760D5 * t40
      rrgg2qqbarh11J6 = (-0.30D2 * t1 * nf * t3 * s * t5 * z + (-0.90D2 
     #* t10 + 0.90D2 * t12 + 0.90D2 * t14) * t1 * t3 * t5 + ((-0.90D2 * 
     #t21 + 0.90D2 * t23 * S34 + (-0.180D3 * t14 - t30) * t11 - t37) * t
     #1 + (-(t41 + t43 - t45 + t47 + t49 + t51 + t53 + t55 - t57 - t59) 
     #* nf * t11 / 0.96D2 - (t49 + t55 + t51 + t43 + t53 - t47 - t45 - t
     #57 + t41 + t59) * nf * t13 / 0.96D2) * t69) * s * z + (0.143D3 / 0
     #.3D1 * t10 - 0.75D2 * t12 - 0.75D2 * t14) * S12 - 0.16D2 / 0.3D1 *
     # t21 + 0.1277D4 / 0.6D1 * t23 * S34 + (-0.225D3 * t14 - t30) * t11
     # - t37 + (-0.127D3 / 0.3D1 * nf * t85 + 0.1917D4 / 0.8D1 * t23 * t
     #20 + ((-0.1244D4 / 0.3D1 * t14 - (-0.16320D5 * S13 - 0.23210D5 * S
     #23 - 0.23210D5 * S24 - 0.16320D5 * S14) * nf / 0.96D2) * t11 - (-0
     #.23210D5 * S13 - 0.16320D5 * S23 - 0.16320D5 * S24 - 0.23210D5 * S
     #14) * nf * t13 / 0.96D2) * S34 + (-0.585D3 / 0.4D1 * t110 * nf * t
     #13 - (t53 - t114 + t115 + t51 + t47 + t116 - t117) * nf / 0.96D2) 
     #* t11 - (-t114 + t115 + t55 - t117 + t43 + t59 + t116) * nf * t13 
     #/ 0.96D2) * t1 + (0.645D3 / 0.4D1 * t23 * t85 + ((-0.645D3 / 0.2D1
     # * t14 - (-0.77760D5 * S13 - 0.77760D5 * S14 - 0.10800D5 * S23 - 0
     #.10800D5 * S24) * nf / 0.96D2) * t11 - (-0.77760D5 * S23 - 0.10800
     #D5 * S13 - 0.10800D5 * S14 - 0.77760D5 * S24) * nf * t13 / 0.96D2)
     # * t20 + ((-0.3045D4 / 0.4D1 * t110 * nf * t13 - (-0.2160D4 * t52 
     #- t155 + 0.4680D4 * t42 + 0.9360D4 * t58 - 0.4320D4 * t46 - t159 +
     # 0.4680D4 * t54 - 0.2160D4 * t50 - t162 - t163) * nf / 0.96D2) * t
     #11 - (-0.2160D4 * t54 - t159 - 0.4320D4 * t58 + 0.9360D4 * t46 - t
     #162 - t163 - t155 - 0.2160D4 * t42 + 0.4680D4 * t52 + 0.4680D4 * t
     #50) * nf * t13 / 0.96D2) * S34 - (0.10080D5 * t58 + 0.10080D5 * t5
     #6 + 0.10080D5 * t46 + 0.8640D4 * t40 + 0.8640D4 * t48 + 0.10080D5 
     #* t44 + 0.5040D4 * t42 + 0.5040D4 * t52 + 0.5040D4 * t54 + 0.5040D
     #4 * t50) * nf * t13 * t11 / 0.96D2) * t69) / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2qqbarh11J7
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
      t4 = S34 ** 2
      t7 = S13 + S14 + S34
      t9 = S23 + S24 + S34
      t10 = t9 * nf
      t11 = -nf * t7 - t10
      rrgg2qqbarh11J7 = (-0.30D2 * nf * S34 * S12 + 0.60D2 * nf * t4 + 0
     #.95D2 / 0.2D1 * t11 * S34 + (-0.30D2 * nf * t4 * S34 + 0.265D3 / 0
     #.12D2 * t11 * t4 + ((0.90D2 * t10 - 0.5D1 / 0.96D2 * (0.640D3 * S1
     #3 + 0.640D3 * S14 + 0.780D3 * S23 + 0.780D3 * S24) * nf) * t7 - 0.
     #5D1 / 0.96D2 * (0.780D3 * S14 + 0.640D3 * S24 + 0.640D3 * S23 + 0.
     #780D3 * S13) * nf * t9) * S34) / S12) / pi * wd / z

      end function
  
 