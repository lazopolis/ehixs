      subroutine rrgg2gght1
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf
      if(z.eq.1d0)then
         call rrgg2gghsoftt1
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2gghhardt1
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2gghhardt1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghhard11J1  
      doubleprecision rrgg2gghhard11J2  
      doubleprecision rrgg2gghhard11J3  
      doubleprecision rrgg2gghhard11J4  
      doubleprecision rrgg2gghhard11J5  
      doubleprecision rrgg2gghhard11J6  
      doubleprecision rrgg2gghhard11J7  
      doubleprecision rrgg2gghhardt1s1e1  
      doubleprecision rrgg2gghhardt1s1e0  
      doubleprecision rrgg2gghhardt1s1em1  
      doubleprecision rrgg2gghhardt1s1em2  
      doubleprecision rrgg2gghhardt1s1em3  
      doubleprecision rrgg2gghhardt1s1em4  
      doubleprecision rrgg2gghhardt1s2e1  
      doubleprecision rrgg2gghhardt1s2e0  
      doubleprecision rrgg2gghhardt1s2em1  
      doubleprecision rrgg2gghhardt1s2em2  
      doubleprecision rrgg2gghhardt1s2em3  
      doubleprecision rrgg2gghhardt1s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt1s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt1s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt1s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt1s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt1s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghhardt1s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghhardt1s1e1
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
      doubleprecision rrgg2gghhard11J1
      doubleprecision rrgg2gghhard11J2
      doubleprecision rrgg2gghhard11J3
      doubleprecision rrgg2gghhard11J4
      doubleprecision rrgg2gghhard11J5
      doubleprecision rrgg2gghhard11J6
      doubleprecision rrgg2gghhard11J7

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
      t9 = rrgg2gghhard11J3(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D
     #0, 0.0D0, 0.0D0)
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x2 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t15 * x3
      t17 = t16 * t4
      t20 = log(-0.4D1 * t13 * t17)
      t21 = rrgg2gghhard11J2(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0
     #D0, 0.0D0, 0.0D0)
      t23 = t20 ** 2
      t24 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0
     #D0, 0.0D0, 0.0D0)
      t30 = pi * lh
      t36 = lh ** 2
      t38 = pi ** 2
      t40 = -0.180D3 * t36 + 0.30D2 * t38
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
      t63 = rrgg2gghhard11J4(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0
     #D0, 0.0D0, 0.0D0)
      t89 = x1 ** 2
      t90 = x2 * t89
      t91 = t90 * t12
      t94 = log(-0.4D1 * t91 * t17)
      t102 = 0.1D1 / x1
      t106 = t89 * t12
      t109 = log(-0.4D1 * t106 * t17)
      t111 = t109 ** 2
      t125 = (-0.90D2 * t8 * (-t9 + t20 * t21 - t23 * t24 / 0.2D1) + 0.1
     #80D3 * t30 * t7 * (-t21 + t20 * t24) - t43) * t45 / 0.1440D4 - (-0
     #.180D3 * t53 * lh - 0.45D2 * t57 + t41) * t7 * t21 / 0.1440D4 + t8
     # * t63 / 0.16D2 - (0.90D2 * t57 * lh + pi * (-0.60D2 * lh * t38 + 
     #0.240D3 * zeta3 + 0.120D3 * t36 * lh) + 0.15D2 * t56 * t52 * pi - 
     #t53 * t40) * t7 * t24 / 0.1440D4 - (0.180D3 * t30 + 0.90D2 * t53) 
     #* t7 * t9 / 0.1440D4 - (-0.90D2 * t8 * (t21 - t94 * t24) + 0.180D3
     # * t30 * t42) * t102 * t45 / 0.720D3 - (-0.90D2 * t8 * (t9 - t109 
     #* t21 + t111 * t24 / 0.2D1) + 0.180D3 * t30 * t7 * (t21 - t109 * t
     #24) + t43) * t102 / 0.720D3
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
      t170 = rrgg2gghhard11J2(s, XB1, XB2, z, lh, wd, nf, s, t151, -t158
     #, t152, -t160, -t166)
      t172 = t90 * t48
      t173 = t128 ** 2
      t174 = t149 * t173
      t179 = log(0.4D1 * t172 * t49 * t174 * t137)
      t181 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, t151, -t158
     #, t152, -t160, -t166)
      t191 = -0.90D2 * t8 * (-t169 * t170 + t179 * t140 * t168 * t181) -
     # 0.180D3 * t30 * t7 * t169 * t181
      t195 = FJET(XB1, XB2, s, t151, t152, -t158, -t160, -t166, -t191 * 
     #t102 * t45 / 0.720D3)
      t202 = Sqrt(x2 * t137 * t49)
      t204 = 0.2D1 * t136 * t202
      t206 = t2 * (-x3 + t132 - x2 + t204)
      t208 = t2 * (0.1D1 - x2 - x3 + t132 + t204)
      t210 = 0.1D1 / (0.1D1 - x2 + t134)
      t211 = rrgg2gghhard11J3(s, XB1, XB2, z, lh, wd, nf, s, -t206, t208
     #, 0.0D0, 0.0D0, 0.0D0)
      t217 = log(0.4D1 * t13 * t15 * t49 * t137)
      t218 = t217 * t210
      t219 = rrgg2gghhard11J2(s, XB1, XB2, z, lh, wd, nf, s, -t206, t208
     #, 0.0D0, 0.0D0, 0.0D0)
      t221 = t217 ** 2
      t223 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, -t206, t208
     #, 0.0D0, 0.0D0, 0.0D0)
      t229 = t210 * t219
      t236 = t7 * t210 * t223
      t245 = log(0.4D1 * t91 * t16 * t4 * t137)
      t257 = (-0.90D2 * t8 * (t210 * t211 - t218 * t219 + t221 * t210 * 
     #t223 / 0.2D1) + 0.180D3 * t30 * t7 * (t229 - t218 * t223) + t41 * 
     #t236) * t45 / 0.1440D4 - (-0.90D2 * t8 * (-t229 + t245 * t210 * t2
     #23) - 0.180D3 * t30 * t236) * t102 * t45 / 0.720D3
      t258 = FJET(XB1, XB2, s, -t206, 0.0D0, t208, 0.0D0, 0.0D0, t257)
      t261 = t2 * t128 * x3
      t263 = t2 * t128 * t4
      t264 = rrgg2gghhard11J2(s, XB1, XB2, z, lh, wd, nf, s, -t261, t263
     #, t152, -t160, 0.0D0)
      t265 = t49 * t174
      t268 = log(-0.4D1 * t172 * t265)
      t269 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, -t261, t263
     #, t152, -t160, 0.0D0)
      t274 = t7 * t269
      t280 = rrgg2gghhard11J3(s, XB1, XB2, z, lh, wd, nf, s, -t261, t263
     #, t152, -t160, 0.0D0)
      t284 = log(-0.4D1 * t106 * t15 * t265)
      t286 = t284 ** 2
      t301 = -(0.90D2 * t8 * (t264 - t268 * t269) - 0.180D3 * t30 * t274
     #) * t102 * t45 / 0.720D3 - (0.90D2 * t8 * (t280 - t284 * t264 + t2
     #86 * t269 / 0.2D1) - 0.180D3 * t30 * t7 * (t264 - t284 * t269) - t
     #41 * t274) * t102 / 0.720D3
      t302 = FJET(XB1, XB2, s, -t261, t152, t263, -t160, 0.0D0, t301)
      rrgg2gghhardt1s1e1 = t126 * t125 - t195 * t191 * t102 * t45 / 0.72
     #0D3 + t258 * t257 + t302 * t301

      end function



      doubleprecision function rrgg2gghhardt1s1e0
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
      doubleprecision rrgg2gghhard11J1
      doubleprecision rrgg2gghhard11J2
      doubleprecision rrgg2gghhard11J3
      doubleprecision rrgg2gghhard11J4
      doubleprecision rrgg2gghhard11J5
      doubleprecision rrgg2gghhard11J6
      doubleprecision rrgg2gghhard11J7

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
      t9 = rrgg2gghhard11J2(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D
     #0, 0.0D0, 0.0D0)
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x2 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = t15 * x3 * t4
      t20 = log(-0.4D1 * t13 * t17)
      t21 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0
     #D0, 0.0D0, 0.0D0)
      t26 = pi * lh
      t29 = 0.180D3 * t26 * t7 * t21
      t31 = 0.1D1 / x2
      t34 = 0.1D1 / x1
      t39 = x1 ** 2
      t40 = t39 * t12
      t43 = log(-0.4D1 * t40 * t17)
      t51 = rrgg2gghhard11J3(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0
     #D0, 0.0D0, 0.0D0)
      t56 = x3 * t4
      t59 = log(-0.4D1 * t12 * t15 * t56)
      t60 = t59 * pi
      t68 = t59 ** 2
      t71 = lh ** 2
      t73 = pi ** 2
      t81 = (-0.90D2 * t8 * (-t9 + t20 * t21) - t29) * t31 / 0.1440D4 + 
     #t8 * t21 * t34 * t31 / 0.8D1 - (-0.90D2 * t8 * (t9 - t43 * t21) + 
     #t29) * t34 / 0.720D3 + t8 * t51 / 0.16D2 - (0.180D3 * t26 + 0.90D2
     # * t60) * t7 * t9 / 0.1440D4 - (-0.180D3 * t60 * lh - 0.45D2 * t68
     # * pi + pi * (-0.180D3 * t71 + 0.30D2 * t73)) * t7 * t21 / 0.1440D
     #4
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
      t126 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, t107, -t114
     #, t108, -t116, -t122)
      t129 = 0.1D1 / (-0.1D1 + x1 - t95 + x2 - t109 - t90 + t110) * t126
     # * t34 * t31
      t132 = FJET(XB1, XB2, s, t107, t108, -t114, -t116, -t122, -t8 * t9
     #6 * t129 / 0.8D1)
      t140 = Sqrt(x2 * t93 * t56)
      t142 = 0.2D1 * t92 * t140
      t144 = t2 * (-x3 + t88 - x2 + t142)
      t146 = t2 * (0.1D1 - x2 - x3 + t88 + t142)
      t148 = 0.1D1 / (0.1D1 - x2 + t90)
      t149 = rrgg2gghhard11J2(s, XB1, XB2, z, lh, wd, nf, s, -t144, t146
     #, 0.0D0, 0.0D0, 0.0D0)
      t155 = log(0.4D1 * t13 * t15 * t56 * t93)
      t157 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, -t144, t146
     #, 0.0D0, 0.0D0, 0.0D0)
      t174 = (-0.90D2 * t8 * (t148 * t149 - t155 * t148 * t157) + 0.180D
     #3 * t26 * t7 * t148 * t157) * t31 / 0.1440D4 - t8 * t148 * t157 * 
     #t34 * t31 / 0.8D1
      t175 = FJET(XB1, XB2, s, -t144, 0.0D0, t146, 0.0D0, 0.0D0, t174)
      t178 = t2 * t84 * x3
      t180 = t2 * t84 * t4
      t181 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, -t178, t180
     #, t108, -t116, 0.0D0)
      t186 = rrgg2gghhard11J2(s, XB1, XB2, z, lh, wd, nf, s, -t178, t180
     #, t108, -t116, 0.0D0)
      t188 = t84 ** 2
      t193 = log(-0.4D1 * t40 * t15 * t56 * t105 * t188)
      t204 = -t8 * t181 * t34 * t31 / 0.8D1 - (0.90D2 * t8 * (t186 - t19
     #3 * t181) - 0.180D3 * t26 * t7 * t181) * t34 / 0.720D3
      t205 = FJET(XB1, XB2, s, -t178, t108, t180, -t116, 0.0D0, t204)
      rrgg2gghhardt1s1e0 = t82 * t81 - t132 * pi * t7 * t96 * t129 / 0.8
     #D1 + t175 * t174 + t205 * t204

      end function



      doubleprecision function rrgg2gghhardt1s1em1
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
      doubleprecision rrgg2gghhard11J1
      doubleprecision rrgg2gghhard11J2
      doubleprecision rrgg2gghhard11J3
      doubleprecision rrgg2gghhard11J4
      doubleprecision rrgg2gghhard11J5
      doubleprecision rrgg2gghhard11J6
      doubleprecision rrgg2gghhard11J7

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
      t9 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D
     #0, 0.0D0, 0.0D0)
      t10 = 0.1D1 / x1
      t14 = rrgg2gghhard11J2(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0
     #D0, 0.0D0, 0.0D0)
      t19 = x4 * pi
      t20 = Sin(t19)
      t21 = t20 ** 2
      t22 = z ** 2
      t25 = x3 * t4
      t28 = log(-0.4D1 * t21 / t22 * t25)
      t35 = 0.1D1 / x2
      t39 = t8 * t9 * t10 / 0.8D1 + t8 * t14 / 0.16D2 - (0.180D3 * pi * 
     #lh + 0.90D2 * t28 * pi) * t7 * t9 / 0.1440D4 + t8 * t9 * t35 / 0.1
     #6D2
      t40 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, t39)
      t42 = -0.1D1 + x1
      t44 = t2 * t42 * x3
      t46 = t2 * x1 * x3
      t48 = t2 * t42 * t4
      t50 = t2 * x1 * t4
      t51 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, -t44, t48, t
     #46, -t50, 0.0D0)
      t55 = FJET(XB1, XB2, s, -t44, t46, t48, -t50, 0.0D0, -t8 * t51 * t
     #10 / 0.8D1)
      t62 = 0.2D1 * x2 * x3
      t63 = cos(t19)
      t67 = Sqrt(x2 * (-0.1D1 + x2) * t25)
      t69 = 0.2D1 * t63 * t67
      t71 = t2 * (-x3 + t62 - x2 + t69)
      t73 = t2 * (0.1D1 - x2 - x3 + t62 + t69)
      t77 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, -t71, t73, 0
     #.0D0, 0.0D0, 0.0D0)
      t79 = 0.1D1 / (0.1D1 - x2 + x2 * z) * t77 * t35
      t82 = FJET(XB1, XB2, s, -t71, 0.0D0, t73, 0.0D0, 0.0D0, -t8 * t79 
     #/ 0.16D2)
      rrgg2gghhardt1s1em1 = t40 * t39 - t55 * pi * t7 * t51 * t10 / 0.8D
     #1 - t82 * pi * t7 * t79 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt1s1em2
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
      doubleprecision rrgg2gghhard11J1
      doubleprecision rrgg2gghhard11J2
      doubleprecision rrgg2gghhard11J3
      doubleprecision rrgg2gghhard11J4
      doubleprecision rrgg2gghhard11J5
      doubleprecision rrgg2gghhard11J6
      doubleprecision rrgg2gghhard11J7

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
      t9 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D
     #0, 0.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, pi * t7 * t9
     # / 0.16D2)
      rrgg2gghhardt1s1em2 = t12 * pi * t7 * t9 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt1s1em3
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
      doubleprecision rrgg2gghhard11J1
      doubleprecision rrgg2gghhard11J2
      doubleprecision rrgg2gghhard11J3
      doubleprecision rrgg2gghhard11J4
      doubleprecision rrgg2gghhard11J5
      doubleprecision rrgg2gghhard11J6
      doubleprecision rrgg2gghhard11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt1s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt1s1em4
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
      doubleprecision rrgg2gghhard11J1
      doubleprecision rrgg2gghhard11J2
      doubleprecision rrgg2gghhard11J3
      doubleprecision rrgg2gghhard11J4
      doubleprecision rrgg2gghhard11J5
      doubleprecision rrgg2gghhard11J6
      doubleprecision rrgg2gghhard11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt1s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghhardt1s2e1
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
      doubleprecision rrgg2gghhard11J1
      doubleprecision rrgg2gghhard11J2
      doubleprecision rrgg2gghhard11J3
      doubleprecision rrgg2gghhard11J4
      doubleprecision rrgg2gghhard11J5
      doubleprecision rrgg2gghhard11J6
      doubleprecision rrgg2gghhard11J7

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
      t9 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
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
      t36 = rrgg2gghhard11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t3, -t5, 0.0D0)
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
      t70 = rrgg2gghhard11J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t3, -t5, 0.0D0)
      t94 = rrgg2gghhard11J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t3, -t5, 0.0D0)
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
      t150 = rrgg2gghhard11J2(s, XB1, XB2, z, lh, wd, nf, s, t142, -t147
     #, -t145, t149, 0.0D0)
      t151 = 0.1D1 / t10
      t153 = t98 * t16 * t151
      t154 = x1 * z
      t155 = -z - x1 + t154
      t156 = 0.1D1 / t155
      t157 = t143 ** 2
      t158 = t156 * t157
      t159 = t18 * t158
      t162 = log(0.4D1 * t153 * t159)
      t163 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, t142, -t147
     #, -t145, t149, 0.0D0)
      t168 = t7 * t163
      t174 = rrgg2gghhard11J3(s, XB1, XB2, z, lh, wd, nf, s, t142, -t147
     #, -t145, t149, 0.0D0)
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
      t232 = rrgg2gghhard11J2(s, XB1, XB2, z, lh, wd, nf, s, t216, -t222
     #, -t145, t149, t228)
      t238 = log(-0.4D1 * t153 * t18 * t158 * t19)
      t240 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, t216, -t222
     #, -t145, t149, t228)
      t250 = 0.90D2 * t8 * (t231 * t232 - t238 * t155 * t230 * t240) - 0
     #.180D3 * t39 * t7 * t231 * t240
      t254 = FJET(XB1, XB2, s, t216, -t145, -t222, t149, t228, -t250 * t
     #113 * t47 / 0.720D3)
      rrgg2gghhardt1s2e1 = t139 * t138 + t196 * t195 - t254 * t250 * t11
     #3 * t47 / 0.720D3

      end function



      doubleprecision function rrgg2gghhardt1s2e0
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
      doubleprecision rrgg2gghhard11J1
      doubleprecision rrgg2gghhard11J2
      doubleprecision rrgg2gghhard11J3
      doubleprecision rrgg2gghhard11J4
      doubleprecision rrgg2gghhard11J5
      doubleprecision rrgg2gghhard11J6
      doubleprecision rrgg2gghhard11J7

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
      t9 = rrgg2gghhard11J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t12 = pi * lh
      t14 = z ** 2
      t16 = 0.1D1 / t14 / z
      t17 = x4 * pi
      t18 = Sin(t17)
      t19 = t18 ** 2
      t21 = x3 * t4
      t24 = log(-0.4D1 * t16 * t19 * t21)
      t25 = t24 * pi
      t29 = rrgg2gghhard11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t3, -t5, 0.0D0)
      t34 = t24 ** 2
      t37 = lh ** 2
      t39 = pi ** 2
      t45 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t3, -t5, 0.0D0)
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
      t96 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, t88, -t93, -
     #t91, t95, 0.0D0)
      t101 = rrgg2gghhard11J2(s, XB1, XB2, z, lh, wd, nf, s, t88, -t93, 
     #-t91, t95, 0.0D0)
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
      t160 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, t144, -t150
     #, -t91, t95, t156)
      t163 = 0.1D1 / (z + x1 - t104 - t145 + t146) * t160 * t81 * t62
      t166 = FJET(XB1, XB2, s, t144, -t91, -t150, t95, t156, -t8 * t105 
     #* t163 / 0.8D1)
      rrgg2gghhardt1s2e0 = t85 * t84 + t124 * t123 - t166 * pi * t7 * t1
     #05 * t163 / 0.8D1

      end function



      doubleprecision function rrgg2gghhardt1s2em1
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
      doubleprecision rrgg2gghhard11J1
      doubleprecision rrgg2gghhard11J2
      doubleprecision rrgg2gghhard11J3
      doubleprecision rrgg2gghhard11J4
      doubleprecision rrgg2gghhard11J5
      doubleprecision rrgg2gghhard11J6
      doubleprecision rrgg2gghhard11J7

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
      t9 = rrgg2gghhard11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t14 = z ** 2
      t18 = Sin(x4 * pi)
      t19 = t18 ** 2
      t24 = log(-0.4D1 / t14 / z * t19 * x3 * t4)
      t29 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0
     #, t3, -t5, 0.0D0)
      t32 = 0.1D1 / x1
      t36 = t8 * t9 / 0.16D2 - (0.180D3 * pi * lh + 0.90D2 * t24 * pi) *
     # t7 * t29 / 0.1440D4 + t8 * t29 * t32 / 0.8D1
      t37 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t36)
      t40 = t2 * x1 * x3
      t41 = -0.1D1 + x1
      t43 = t2 * t41 * x3
      t45 = t2 * x1 * t4
      t47 = t2 * t41 * t4
      t48 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, t40, -t45, -
     #t43, t47, 0.0D0)
      t52 = FJET(XB1, XB2, s, t40, -t43, -t45, t47, 0.0D0, -t8 * t48 * t
     #32 / 0.8D1)
      rrgg2gghhardt1s2em1 = t37 * t36 - t52 * pi * t7 * t48 * t32 / 0.8D
     #1

      end function



      doubleprecision function rrgg2gghhardt1s2em2
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
      doubleprecision rrgg2gghhard11J1
      doubleprecision rrgg2gghhard11J2
      doubleprecision rrgg2gghhard11J3
      doubleprecision rrgg2gghhard11J4
      doubleprecision rrgg2gghhard11J5
      doubleprecision rrgg2gghhard11J6
      doubleprecision rrgg2gghhard11J7

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
      t9 = rrgg2gghhard11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0,
     # t3, -t5, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, pi * t7 * t9
     # / 0.16D2)
      rrgg2gghhardt1s2em2 = t12 * pi * t7 * t9 / 0.16D2

      end function



      doubleprecision function rrgg2gghhardt1s2em3
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
      doubleprecision rrgg2gghhard11J1
      doubleprecision rrgg2gghhard11J2
      doubleprecision rrgg2gghhard11J3
      doubleprecision rrgg2gghhard11J4
      doubleprecision rrgg2gghhard11J5
      doubleprecision rrgg2gghhard11J6
      doubleprecision rrgg2gghhard11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt1s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt1s2em4
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
      doubleprecision rrgg2gghhard11J1
      doubleprecision rrgg2gghhard11J2
      doubleprecision rrgg2gghhard11J3
      doubleprecision rrgg2gghhard11J4
      doubleprecision rrgg2gghhard11J5
      doubleprecision rrgg2gghhard11J6
      doubleprecision rrgg2gghhard11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gghhardt1s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2gghhard11J1
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
      t2 = S12 + S13 + S23
      t3 = 0.1D1 / t2
      t4 = S12 + S14 + S24
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = S13 + S14 + S34
      t9 = S23 + S24 + S34
      t11 = S12 ** 2
      t12 = 0.1D1 / t11
      t17 = s ** 2
      t20 = z ** 2
      t23 = t4 ** 2
      t24 = 0.1D1 / t23
      t27 = t2 ** 2
      t28 = 0.1D1 / t27
      t36 = -t5 - t3
      t37 = 0.216D3 * t36
      t41 = 0.216D3 * S13
      t42 = 0.216D3 * S14
      t43 = 0.432D3 * S34
      t45 = 0.216D3 * S24
      t50 = t7 * S34
      t52 = S13 ** 2
      t53 = 0.18D2 * t52
      t54 = S13 * S24
      t55 = 0.36D2 * t54
      t56 = S24 ** 2
      t57 = 0.18D2 * t56
      t58 = t53 - t55 + t57
      t60 = S14 * S23
      t61 = 0.36D2 * t60
      t62 = S14 ** 2
      t63 = 0.18D2 * t62
      t64 = S23 ** 2
      t65 = 0.18D2 * t64
      t66 = -t61 + t63 + t65
      t78 = S14 * S24
      t79 = 0.9D1 * t78
      t82 = S23 * S24
      t83 = S13 * S14
      t89 = S13 * S23
      t90 = 0.9D1 * t89
      t106 = S34 ** 2
      t108 = -t36
      t132 = 0.36D2 * t78
      t133 = 0.36D2 * t82
      t134 = 0.36D2 * t83
      t135 = 0.36D2 * t89
      t136 = -0.216D3 * t106 + ((0.2313D4 / 0.4D1 * t108 * t9 + 0.2313D4
     # / 0.4D1 - 0.216D3 * S24 * t5 - 0.216D3 * S23 * t3) * t7 + (0.2313
     #D4 / 0.4D1 - 0.216D3 * S14 * t5 - 0.216D3 * S13 * t3) * t9) * S34 
     #+ (-0.432D3 * S23 - 0.432D3 * S24 - t43 - t41 - t42) * t7 + (-0.21
     #6D3 * S23 - 0.216D3 * S24) * t9 - t132 + t133 + t61 + t134 - t63 -
     # t65 - t53 - t135 - t57 + t55
      t138 = -S13 - S23
      t154 = ((t61 + t55 + t134 + t53 - t57 + t135 - t63 + t65 - t132 + 
     #t133) * t5 + (t134 + t132 + t55 + t61 + t133 - t135 - t65 + t63 + 
     #t57 - t53) * t3) * t9 + t63 - t65 + t53 - t57 - t133 + t61 + t135 
     #+ t132 + t134 + t55
      t163 = 0.9D1 / 0.8D1 * t108
      t166 = t7 * t163 + t163 * t9
      t168 = t6 * t9
      t170 = 0.18D2 * t5
      t171 = 0.18D2 * t3
      t175 = 0.18D2 * t36 * t9
      t180 = 0.27D2 / 0.8D1 * S24
      t181 = 0.27D2 / 0.8D1 * S14
      t182 = 0.9D1 / 0.2D1 * S13
      t185 = 0.9D1 / 0.2D1 * S14
      t186 = 0.27D2 / 0.8D1 * S23
      t187 = 0.27D2 / 0.8D1 * S13
      t192 = 0.9D1 / 0.2D1 * S23
      t195 = 0.9D1 / 0.2D1 * S24
      t213 = 0.36D2 * S14 - 0.27D2 * S23
      t217 = -0.27D2 * S24 + 0.36D2 * S13
      t223 = -0.27D2 * S13 + 0.36D2 * S24
      t227 = 0.36D2 * S23 - 0.27D2 * S14
      t233 = 0.1845D4 / 0.16D2 * S13
      t234 = 0.603D3 / 0.16D2 * S14
      t235 = 0.1845D4 / 0.16D2 * S23
      t236 = 0.603D3 / 0.16D2 * S24
      t239 = 0.1845D4 / 0.16D2 * S14
      t240 = 0.603D3 / 0.16D2 * S13
      t241 = 0.1845D4 / 0.16D2 * S24
      t242 = 0.603D3 / 0.16D2 * S23
      t249 = 0.9D1 * t83
      t250 = 0.27D2 * t54
      t251 = 0.27D2 / 0.2D1 * t52
      t252 = 0.27D2 / 0.4D1 * t78
      t253 = 0.27D2 / 0.8D1 * t62
      t254 = 0.63D2 / 0.8D1 * t56
      t257 = 0.27D2 / 0.4D1 * t89
      t258 = 0.27D2 / 0.8D1 * t52
      t259 = 0.27D2 / 0.2D1 * t62
      t260 = 0.27D2 * t60
      t261 = 0.63D2 / 0.8D1 * t64
      t268 = 0.9D1 * t82
      t269 = 0.27D2 / 0.8D1 * t56
      t270 = 0.63D2 / 0.8D1 * t62
      t271 = 0.27D2 / 0.2D1 * t64
      t274 = 0.27D2 / 0.8D1 * t64
      t275 = 0.27D2 / 0.2D1 * t56
      t276 = 0.63D2 / 0.8D1 * t52
      t285 = t106 * S34
      t305 = 0.18D2 * t54
      t309 = 0.18D2 * t60
      t328 = ((0.1863D4 / 0.2D1 + (t135 - t57 - t65 - t63 - t53 + t132 +
     # t260 - 0.27D2 * t83 - 0.27D2 * t82 + t250) * t5 * t3) * t9 + (t26
     #0 + 0.135D3 * t52 - t63 - t305 - t65 - 0.72D2 * t56) * t5 + (-t309
     # - t57 - 0.72D2 * t64 + 0.135D3 * t62 + t250 - t53) * t3) * t7 + (
     #(-t309 + 0.135D3 * t64 - t53 - 0.72D2 * t62 - t57 + t250) * t5 + (
     #-t305 + 0.135D3 * t56 - t65 - 0.72D2 * t52 + t260 - t63) * t3) * t
     #9 + t134 - 0.27D2 * t89 - 0.27D2 * t78 + t250 - t53 - t57 - t63 - 
     #t65 + t133 + t260
      t330 = 0.549D3 / 0.2D1 * S23
      t331 = 0.549D3 / 0.2D1 * S24
      t332 = 0.549D3 / 0.2D1 * S13
      t333 = 0.549D3 / 0.2D1 * S14
      t335 = 0.1197D4 / 0.16D2 * t54
      t336 = 0.1197D4 / 0.16D2 * t83
      t337 = 0.27D2 * t52
      t338 = 0.333D3 / 0.8D1 * t56
      t339 = 0.1197D4 / 0.16D2 * t82
      t340 = 0.333D3 / 0.8D1 * t62
      t341 = 0.1197D4 / 0.16D2 * t60
      t342 = 0.27D2 * t64
      t348 = 0.27D2 * t56
      t349 = 0.27D2 * t62
      t350 = 0.333D3 / 0.8D1 * t64
      t351 = 0.333D3 / 0.8D1 * t52
      t356 = 0.171D3 / 0.8D1 * t52
      t357 = 0.171D3 / 0.8D1 * t62
      t358 = 0.45D2 / 0.16D2 * t64
      t359 = 0.45D2 * t89
      t360 = 0.45D2 * t78
      t362 = 0.9D1 * t60
      t363 = 0.45D2 / 0.16D2 * t56
      t364 = 0.9D1 * t54
      t365 = t62 * S14
      t366 = 0.9D1 / 0.8D1 * t365
      t367 = t52 * S13
      t368 = 0.18D2 * t367
      t369 = S13 * t56
      t370 = 0.117D3 / 0.2D1 * t369
      t371 = t62 * S24
      t373 = t52 * S14
      t375 = t83 * S24
      t376 = 0.27D2 * t375
      t377 = S14 * t56
      t379 = t56 * S24
      t380 = 0.189D3 / 0.8D1 * t379
      t381 = t52 * S24
      t382 = 0.135D3 / 0.2D1 * t381
      t383 = S13 * t62
      t387 = 0.18D2 * t365
      t388 = 0.9D1 / 0.8D1 * t367
      t389 = t64 * S23
      t390 = 0.189D3 / 0.8D1 * t389
      t391 = t52 * S23
      t394 = t62 * S23
      t395 = 0.135D3 / 0.2D1 * t394
      t396 = S14 * t64
      t397 = 0.117D3 / 0.2D1 * t396
      t399 = S13 * t64
      t401 = t83 * S23
      t402 = 0.27D2 * t401
      t405 = (t330 + t331 + t332 + t333 + (-0.333D3 / 0.4D1 * t78 + t335
     # + t336 + t337 - t338 + t339 - t340 + t341 + t342 + 0.54D2 * t89) 
     #* t5 + (-0.333D3 / 0.4D1 * t89 + t341 + 0.54D2 * t78 + t339 + t348
     # + t336 + t349 - t350 - t351 + t335) * t3) * t9 + t249 - t356 - t3
     #57 - t358 - t359 - t360 + 0.333D3 / 0.8D1 * t82 + t362 - t363 + t3
     #64 + (-t366 - t368 - t370 + 0.27D2 / 0.8D1 * t371 - 0.27D2 / 0.2D1
     # * t373 + t376 - 0.63D2 / 0.8D1 * t377 + t380 + t382 - 0.9D1 / 0.2
     #D1 * t383) * t5 + (-t387 - t388 + t390 + 0.27D2 / 0.8D1 * t391 - 0
     #.9D1 / 0.2D1 * t373 + t395 - t397 - 0.27D2 / 0.2D1 * t383 - 0.63D2
     # / 0.8D1 * t399 + t402) * t3
      t407 = 0.171D3 / 0.8D1 * t56
      t408 = 0.45D2 / 0.16D2 * t52
      t410 = 0.45D2 / 0.16D2 * t62
      t411 = 0.171D3 / 0.8D1 * t64
      t412 = 0.9D1 / 0.8D1 * t379
      t413 = 0.18D2 * t389
      t415 = 0.117D3 / 0.2D1 * t394
      t416 = 0.189D3 / 0.8D1 * t365
      t417 = 0.135D3 / 0.2D1 * t396
      t418 = t56 * S23
      t420 = t60 * S24
      t421 = 0.27D2 * t420
      t423 = t64 * S24
      t427 = 0.189D3 / 0.8D1 * t367
      t428 = 0.9D1 / 0.8D1 * t389
      t431 = t54 * S23
      t432 = 0.27D2 * t431
      t434 = 0.135D3 / 0.2D1 * t369
      t435 = 0.117D3 / 0.2D1 * t381
      t436 = 0.18D2 * t379
      t440 = -t407 - t408 + 0.333D3 / 0.8D1 * t83 - t360 - t410 - t411 -
     # t359 + t268 + t362 + t364 + (-t412 - t413 + 0.27D2 / 0.8D1 * t377
     # - t415 + t416 + t417 - 0.9D1 / 0.2D1 * t418 + t421 - 0.63D2 / 0.8
     #D1 * t371 - 0.27D2 / 0.2D1 * t423) * t5 + (t427 - t428 + 0.27D2 / 
     #0.8D1 * t399 - 0.9D1 / 0.2D1 * t423 + t432 - 0.63D2 / 0.8D1 * t391
     # + t434 - t435 - t436 - 0.27D2 / 0.2D1 * t418) * t3
      t444 = t106 ** 2
      t469 = 0.27D2 / 0.4D1 * t82
      t476 = 0.27D2 / 0.4D1 * t83
      t485 = 0.45D2 * t82
      t487 = 0.45D2 * t83
      t496 = 0.1197D4 / 0.16D2 * t78
      t498 = 0.1197D4 / 0.16D2 * t89
      t511 = (t330 + t331 + t332 + t333 + (-t363 - t485 + t90 + t364 - t
     #356 + t362 - t411 + 0.333D3 / 0.8D1 * t78 - t410 - t487) * t5 + (-
     #t485 - t358 + 0.333D3 / 0.8D1 * t89 + t362 + t79 - t487 + t364 - t
     #408 - t357 - t407) * t3) * t9 + 0.54D2 * t83 + t337 + t335 + t349 
     #- t350 + t496 - t338 - 0.333D3 / 0.4D1 * t82 + t341 + t498 + (-t37
     #0 - 0.27D2 / 0.2D1 * t391 + t380 + 0.27D2 / 0.8D1 * t423 - 0.63D2 
     #/ 0.8D1 * t418 - t368 - 0.9D1 / 0.2D1 * t399 - t428 + t382 + t432)
     # * t5 + (-t387 + t390 + t421 - t412 - 0.27D2 / 0.2D1 * t371 - 0.9D
     #1 / 0.2D1 * t377 + t395 + 0.27D2 / 0.8D1 * t418 - t397 - 0.63D2 / 
     #0.8D1 * t423) * t3
      t527 = t341 + t335 - 0.333D3 / 0.4D1 * t83 - t340 + t342 - t351 + 
     #t348 + t498 + t496 + 0.54D2 * t82 + (-t388 + 0.27D2 / 0.8D1 * t373
     # - 0.63D2 / 0.8D1 * t383 - 0.27D2 / 0.2D1 * t399 + t402 - t415 + t
     #417 + t416 - t413 - 0.9D1 / 0.2D1 * t391) * t5 + (-t366 + t427 + 0
     #.27D2 / 0.8D1 * t383 - 0.9D1 / 0.2D1 * t371 - t436 + t434 - 0.27D2
     # / 0.2D1 * t377 - 0.63D2 / 0.8D1 * t373 + t376 - t435) * t3
      t541 = 0.18D2 * t396
      t542 = 0.18D2 * t399
      t543 = 0.36D2 * t401
      t544 = 0.18D2 * t383
      t545 = 0.18D2 * t418
      t546 = 0.18D2 * t381
      t547 = 0.36D2 * t431
      t548 = 0.36D2 * t394
      t549 = 0.18D2 * t391
      t550 = 0.36D2 * t369
      t551 = t436 + t541 + t542 - t543 + t387 + t544 + t545 + t546 - t54
     #7 - t548 + t549 - t550
      t553 = 0.18D2 * t423
      t554 = 0.18D2 * t394
      t555 = 0.18D2 * t371
      t556 = 0.18D2 * t377
      t557 = 0.36D2 * t420
      t558 = 0.36D2 * t375
      t559 = 0.18D2 * t369
      t560 = 0.36D2 * t396
      t561 = 0.18D2 * t373
      t562 = 0.36D2 * t381
      t563 = t368 + t413 + t553 + t554 + t555 + t556 - t557 - t558 + t55
     #9 - t560 + t561 - t562
      t565 = -0.81D2 * t62 - 0.81D2 * t52 + 0.117D3 * t54 + 0.18D2 * t89
     # + 0.18D2 * t78 + 0.117D3 * t60 + 0.18D2 * t82 - 0.81D2 * t56 + 0.
     #18D2 * t83 - 0.81D2 * t64 + t551 * t5 + t563 * t3
      t567 = t565 * t9 + t546 - t550 + t561 + t544 + t556 + t542 - t558 
     #- t543 + t436 + t413 + t554 - t560
      t569 = t549 + t545 + t368 - t557 - t547 + t541 - t562 + t559 + t55
     #5 + t553 + t387 - t548
      t573 = (-0.72D2 * t1 - 0.72D2 * t6 * t7 * S34 * t9 * t12) * t17 * 
     #s * t20 * z + (((((0.81D2 / 0.2D1 * t24 - 0.216D3 * t6 + 0.81D2 / 
     #0.2D1 * t28) * t9 - 0.216D3 * t5 - 0.216D3 * t3) * t7 - 0.216D3 + 
     #t37 * t9) * S34 + t41 + t42 + t43 + 0.216D3 * S23 + t45) * t1 + (-
     #t37 * t9 * t50 + (t58 * t5 + t66 * t3) * t7 + (t66 * t5 + t58 * t3
     #) * t9) * t12 + ((0.9D1 / 0.2D1 * t62 + 0.9D1 / 0.2D1 * t56 - t79)
     # * t24 + (-0.9D1 * t82 + 0.9D1 * t54 + 0.9D1 * t60 - 0.9D1 * t83) 
     #* t5 * t3 + (0.9D1 / 0.2D1 * t64 - t90 + 0.9D1 / 0.2D1 * t52) * t2
     #8) * t9 * t50 / t11 / S12) * t17 * t20 + (-0.216D3 * t6 * t50 * t9
     # + t136 * t1 + ((-0.432D3 + 0.216D3 * t138 * t5 + (-t42 - t45 + (t
     #135 - t63 - t134 + t55 + t61 - t53 - t133 + t132 - t65 - t57) * t5
     #) * t3) * t9 * t50 + t154 * t7 + (-t134 + t61 + t132 + t133 + t135
     # + t57 - t63 + t65 - t53 + t55) * t9) * t12) * s * z + t166 * t11 
     #+ (((-0.72D2 * t168 - t170 - t171) * t7 + 0.135D3 + t175) * S34 + 
     #(0.63D2 / 0.16D2 * t108 * t9 - 0.585D3 / 0.4D1 + (t180 - t181 - t1
     #82) * t5 + (-t185 + t186 - t187) * t3) * t7 + (-0.585D3 / 0.4D1 + 
     #(-t192 - t180 + t181) * t5 + (-t195 - t186 + t187) * t3) * t9) * S
     #12 + ((-0.18D2 * t168 + 0.27D2 * t5 + 0.27D2 * t3) * t7 - 0.18D2 +
     # 0.27D2 * t108 * t9) * t106 + ((t213 * t5 + t217 * t3) * t7 + (t22
     #3 * t5 + t227 * t3) * t9) * S34 + ((-0.387D3 / 0.8D1 + (t233 - t23
     #4 + t235 - t236) * t5 + (t239 - t240 + t241 - t242) * t3) * t9 - 0
     #.99D2 * S14 - 0.99D2 * S13 + (t249 - t250 + t251 - t252 + t253 + t
     #254) * t5 + (-t257 + t258 + t259 - t260 + t261 + t249) * t3) * t7 
     #+ (-0.99D2 * S23 - 0.99D2 * S24 + (-t260 - t252 + t268 + t269 + t2
     #70 + t271) * t5 + (t268 - t257 - t250 + t274 + t275 + t276) * t3) 
     #* t9 + (((0.135D3 * t168 - t170 - t171) * t7 - 0.72D2 + t175) * t2
     #85 + ((t227 * t5 + t223 * t3) * t7 + (t217 * t5 + t213 * t3) * t9)
     # * t106 + t328 * S34 + t405 * t7 + t440 * t9) * t1 + (t166 * t444 
     #+ ((0.585D3 / 0.4D1 * t36 * t9 + 0.63D2 / 0.16D2 + (t180 - t182 - 
     #t186) * t5 + (-t185 - t180 + t186) * t3) * t7 + (0.63D2 / 0.16D2 +
     # (t181 - t192 - t187) * t5 + (-t195 + t187 - t181) * t3) * t9) * t
     #285 + (((-0.387D3 / 0.8D1 + 0.99D2 * t138 * t5 + (-0.99D2 * S14 - 
     #0.99D2 * S24) * t3) * t9 + t233 + t239 - t236 - t242 + (-t250 + t2
     #51 + t90 + t254 + t274 - t469) * t5 + (t79 - t260 + t261 + t259 + 
     #t269 - t469) * t3) * t7 + (t241 - t234 + t235 - t240 + (-t260 + t9
     #0 + t258 + t271 + t270 - t476) * t5 + (t275 + t79 - t250 + t276 + 
     #t253 - t476) * t3) * t9) * t106 + (t511 * t7 + t527 * t9) * S34 + 
     #t567 * t7 + t569 * t9) * t12
      rrgg2gghhard11J1 = t573 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard11J2
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
      t2 = S12 + S13 + S23
      t3 = 0.1D1 / t2
      t4 = S12 + S14 + S24
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = S13 + S14 + S34
      t9 = S23 + S24 + S34
      t11 = S12 ** 2
      t12 = 0.1D1 / t11
      t17 = s ** 2
      t20 = z ** 2
      t23 = t6 * t9
      t27 = -t5 - t3
      t28 = 0.216D3 * t27
      t32 = 0.216D3 * S13
      t33 = 0.216D3 * S14
      t34 = 0.432D3 * S34
      t36 = 0.216D3 * S24
      t41 = t7 * S34
      t43 = S13 ** 2
      t44 = 0.18D2 * t43
      t45 = S13 * S24
      t46 = 0.36D2 * t45
      t47 = S24 ** 2
      t48 = 0.18D2 * t47
      t49 = t44 - t46 + t48
      t51 = S14 * S23
      t52 = 0.36D2 * t51
      t53 = S14 ** 2
      t54 = 0.18D2 * t53
      t55 = S23 ** 2
      t56 = 0.18D2 * t55
      t57 = -t52 + t54 + t56
      t73 = S34 ** 2
      t75 = -t27
      t99 = S14 * S24
      t100 = 0.36D2 * t99
      t101 = S23 * S24
      t102 = 0.36D2 * t101
      t103 = S13 * S14
      t104 = 0.36D2 * t103
      t105 = S13 * S23
      t106 = 0.36D2 * t105
      t107 = -0.216D3 * t73 + ((0.1233D4 / 0.4D1 * t75 * t9 + 0.1233D4 /
     # 0.4D1 - 0.216D3 * S24 * t5 - 0.216D3 * S23 * t3) * t7 + (0.1233D4
     # / 0.4D1 - 0.216D3 * S14 * t5 - 0.216D3 * S13 * t3) * t9) * S34 + 
     #(-0.432D3 * S23 - 0.432D3 * S24 - t34 - t32 - t33) * t7 + (-0.216D
     #3 * S23 - 0.216D3 * S24) * t9 - t100 + t102 + t52 + t104 - t54 - t
     #56 - t44 - t106 - t48 + t46
      t109 = -S13 - S23
      t119 = 0.45D2 * t51
      t120 = 0.45D2 * t45
      t121 = 0.27D2 * t103
      t122 = 0.9D1 * t47
      t123 = 0.9D1 * t53
      t125 = 0.27D2 * t101
      t129 = 0.9D1 * t55
      t130 = 0.9D1 * t43
      t136 = 0.27D2 * t105
      t137 = 0.27D2 * t99
      t138 = ((t119 + t120 + t121 + t44 - t122 + t106 - t123 + t56 - 0.5
     #4D2 * t99 + t125) * t5 + (t121 + t100 + t120 + t119 + t125 - 0.54D
     #2 * t105 - t129 + t54 + t48 - t130) * t3) * t9 + t54 - t129 + t44 
     #- t122 - 0.54D2 * t101 + t119 + t136 + t137 + t104 + t120
      t166 = 0.9D1 / 0.8D1 * t75
      t169 = t166 * t7 + t166 * t9
      t172 = 0.18D2 * t5
      t173 = 0.18D2 * t3
      t177 = 0.18D2 * t27 * t9
      t182 = 0.27D2 / 0.8D1 * S24
      t183 = 0.27D2 / 0.8D1 * S14
      t184 = 0.9D1 / 0.2D1 * S13
      t187 = 0.9D1 / 0.2D1 * S14
      t188 = 0.27D2 / 0.8D1 * S23
      t189 = 0.27D2 / 0.8D1 * S13
      t194 = 0.9D1 / 0.2D1 * S23
      t197 = 0.9D1 / 0.2D1 * S24
      t215 = 0.36D2 * S14 - 0.27D2 * S23
      t219 = -0.27D2 * S24 + 0.36D2 * S13
      t225 = -0.27D2 * S13 + 0.36D2 * S24
      t229 = 0.36D2 * S23 - 0.27D2 * S14
      t235 = 0.693D3 / 0.4D1 * S13
      t236 = 0.657D3 / 0.8D1 * S14
      t237 = 0.693D3 / 0.4D1 * S23
      t238 = 0.657D3 / 0.8D1 * S24
      t241 = 0.693D3 / 0.4D1 * S14
      t242 = 0.657D3 / 0.8D1 * S13
      t243 = 0.693D3 / 0.4D1 * S24
      t244 = 0.657D3 / 0.8D1 * S23
      t251 = 0.9D1 * t103
      t252 = 0.27D2 * t45
      t253 = 0.27D2 / 0.2D1 * t43
      t254 = 0.27D2 / 0.4D1 * t99
      t255 = 0.27D2 / 0.8D1 * t53
      t256 = 0.63D2 / 0.8D1 * t47
      t259 = 0.27D2 / 0.4D1 * t105
      t260 = 0.27D2 / 0.8D1 * t43
      t261 = 0.27D2 / 0.2D1 * t53
      t262 = 0.27D2 * t51
      t263 = 0.63D2 / 0.8D1 * t55
      t270 = 0.9D1 * t101
      t271 = 0.27D2 / 0.8D1 * t47
      t272 = 0.63D2 / 0.8D1 * t53
      t273 = 0.27D2 / 0.2D1 * t55
      t276 = 0.27D2 / 0.8D1 * t55
      t277 = 0.27D2 / 0.2D1 * t47
      t278 = 0.63D2 / 0.8D1 * t43
      t287 = t73 * S34
      t324 = ((0.2619D4 / 0.2D1 + (t106 - t48 - t56 - t54 - t44 + t100 +
     # t262 - t121 - t125 + t252) * t5 * t3) * t9 + (t262 + 0.108D3 * t4
     #3 - t54 + t46 - t56 - 0.99D2 * t47) * t5 + (t52 - t48 - 0.99D2 * t
     #55 + 0.108D3 * t53 + t252 - t44) * t3) * t7 + ((t52 + 0.108D3 * t5
     #5 - t44 - 0.99D2 * t53 - t48 + t252) * t5 + (t46 + 0.108D3 * t47 -
     # t56 - 0.99D2 * t43 + t262 - t54) * t3) * t9 + t104 - t136 - t137 
     #+ t252 - t44 - t48 - t54 - t56 + t102 + t262
      t326 = 0.3411D4 / 0.16D2 * S23
      t327 = 0.3411D4 / 0.16D2 * S24
      t328 = 0.3411D4 / 0.16D2 * S13
      t329 = 0.3411D4 / 0.16D2 * S14
      t331 = 0.531D3 / 0.4D1 * t45
      t332 = 0.531D3 / 0.4D1 * t103
      t333 = 0.243D3 / 0.4D1 * t47
      t334 = 0.531D3 / 0.4D1 * t101
      t335 = 0.243D3 / 0.4D1 * t53
      t336 = 0.531D3 / 0.4D1 * t51
      t340 = 0.243D3 / 0.4D1 * t55
      t341 = 0.243D3 / 0.4D1 * t43
      t346 = 0.171D3 / 0.8D1 * t43
      t347 = 0.171D3 / 0.8D1 * t53
      t348 = 0.45D2 / 0.16D2 * t55
      t349 = 0.45D2 * t105
      t350 = 0.45D2 * t99
      t352 = 0.9D1 * t51
      t353 = 0.45D2 / 0.16D2 * t47
      t354 = 0.9D1 * t45
      t355 = t53 * S14
      t356 = 0.9D1 / 0.8D1 * t355
      t357 = t43 * S13
      t358 = 0.18D2 * t357
      t359 = S13 * t47
      t360 = 0.117D3 / 0.2D1 * t359
      t361 = t53 * S24
      t363 = t43 * S14
      t365 = t103 * S24
      t366 = 0.27D2 * t365
      t367 = S14 * t47
      t369 = t47 * S24
      t370 = 0.189D3 / 0.8D1 * t369
      t371 = t43 * S24
      t372 = 0.135D3 / 0.2D1 * t371
      t373 = S13 * t53
      t377 = 0.18D2 * t355
      t378 = 0.9D1 / 0.8D1 * t357
      t379 = t55 * S23
      t380 = 0.189D3 / 0.8D1 * t379
      t381 = t43 * S23
      t384 = t53 * S23
      t385 = 0.135D3 / 0.2D1 * t384
      t386 = S14 * t55
      t387 = 0.117D3 / 0.2D1 * t386
      t389 = S13 * t55
      t391 = t103 * S23
      t392 = 0.27D2 * t391
      t395 = (t326 + t327 + t328 + t329 + (-0.243D3 / 0.2D1 * t99 + t331
     # + t332 - t253 - t333 + t334 - t335 + t336 - t273 - t136) * t5 + (
     #-0.243D3 / 0.2D1 * t105 + t336 - t137 + t334 - t277 + t332 - t261 
     #- t340 - t341 + t331) * t3) * t9 + t251 - t346 - t347 - t348 - t34
     #9 - t350 + 0.333D3 / 0.8D1 * t101 + t352 - t353 + t354 + (-t356 - 
     #t358 - t360 + 0.27D2 / 0.8D1 * t361 - 0.27D2 / 0.2D1 * t363 + t366
     # - 0.63D2 / 0.8D1 * t367 + t370 + t372 - 0.9D1 / 0.2D1 * t373) * t
     #5 + (-t377 - t378 + t380 + 0.27D2 / 0.8D1 * t381 - 0.9D1 / 0.2D1 *
     # t363 + t385 - t387 - 0.27D2 / 0.2D1 * t373 - 0.63D2 / 0.8D1 * t38
     #9 + t392) * t3
      t397 = 0.171D3 / 0.8D1 * t47
      t398 = 0.45D2 / 0.16D2 * t43
      t400 = 0.45D2 / 0.16D2 * t53
      t401 = 0.171D3 / 0.8D1 * t55
      t402 = 0.9D1 / 0.8D1 * t369
      t403 = 0.18D2 * t379
      t405 = 0.117D3 / 0.2D1 * t384
      t406 = 0.189D3 / 0.8D1 * t355
      t407 = 0.135D3 / 0.2D1 * t386
      t408 = t47 * S23
      t410 = t51 * S24
      t411 = 0.27D2 * t410
      t413 = t55 * S24
      t417 = 0.189D3 / 0.8D1 * t357
      t418 = 0.9D1 / 0.8D1 * t379
      t421 = t45 * S23
      t422 = 0.27D2 * t421
      t424 = 0.135D3 / 0.2D1 * t359
      t425 = 0.117D3 / 0.2D1 * t371
      t426 = 0.18D2 * t369
      t430 = -t397 - t398 + 0.333D3 / 0.8D1 * t103 - t350 - t400 - t401 
     #- t349 + t270 + t352 + t354 + (-t402 - t403 + 0.27D2 / 0.8D1 * t36
     #7 - t405 + t406 + t407 - 0.9D1 / 0.2D1 * t408 + t411 - 0.63D2 / 0.
     #8D1 * t361 - 0.27D2 / 0.2D1 * t413) * t5 + (t417 - t418 + 0.27D2 /
     # 0.8D1 * t389 - 0.9D1 / 0.2D1 * t413 + t422 - 0.63D2 / 0.8D1 * t38
     #1 + t424 - t425 - t426 - 0.27D2 / 0.2D1 * t408) * t3
      t434 = t73 ** 2
      t459 = 0.9D1 * t105
      t460 = 0.27D2 / 0.4D1 * t101
      t463 = 0.9D1 * t99
      t468 = 0.27D2 / 0.4D1 * t103
      t477 = 0.45D2 * t101
      t479 = 0.45D2 * t103
      t487 = 0.531D3 / 0.4D1 * t99
      t489 = 0.531D3 / 0.4D1 * t105
      t502 = (t326 + t327 + t328 + t329 + (-t353 - t477 + t459 + t354 - 
     #t346 + t352 - t401 + 0.333D3 / 0.8D1 * t99 - t400 - t479) * t5 + (
     #-t477 - t348 + 0.333D3 / 0.8D1 * t105 + t352 + t463 - t479 + t354 
     #- t398 - t347 - t397) * t3) * t9 - t121 - t253 + t331 - t261 - t34
     #0 + t487 - t333 - 0.243D3 / 0.2D1 * t101 + t336 + t489 + (-t360 - 
     #0.27D2 / 0.2D1 * t381 + t370 + 0.27D2 / 0.8D1 * t413 - 0.63D2 / 0.
     #8D1 * t408 - t358 - 0.9D1 / 0.2D1 * t389 - t418 + t372 + t422) * t
     #5 + (-t377 + t380 + t411 - t402 - 0.27D2 / 0.2D1 * t361 - 0.9D1 / 
     #0.2D1 * t367 + t385 + 0.27D2 / 0.8D1 * t408 - t387 - 0.63D2 / 0.8D
     #1 * t413) * t3
      t517 = t336 + t331 - 0.243D3 / 0.2D1 * t103 - t335 - t273 - t341 -
     # t277 + t489 + t487 - t125 + (-t378 + 0.27D2 / 0.8D1 * t363 - 0.63
     #D2 / 0.8D1 * t373 - 0.27D2 / 0.2D1 * t389 + t392 - t405 + t407 + t
     #406 - t403 - 0.9D1 / 0.2D1 * t381) * t5 + (-t356 + t417 + 0.27D2 /
     # 0.8D1 * t373 - 0.9D1 / 0.2D1 * t361 - t426 + t424 - 0.27D2 / 0.2D
     #1 * t367 - 0.63D2 / 0.8D1 * t363 + t366 - t425) * t3
      t531 = 0.18D2 * t386
      t532 = 0.18D2 * t389
      t533 = 0.36D2 * t391
      t534 = 0.18D2 * t373
      t535 = 0.18D2 * t408
      t536 = 0.18D2 * t371
      t537 = 0.36D2 * t421
      t538 = 0.36D2 * t384
      t539 = 0.18D2 * t381
      t540 = 0.36D2 * t359
      t541 = t426 + t531 + t532 - t533 + t377 + t534 + t535 + t536 - t53
     #7 - t538 + t539 - t540
      t543 = 0.18D2 * t413
      t544 = 0.18D2 * t384
      t545 = 0.18D2 * t361
      t546 = 0.18D2 * t367
      t547 = 0.36D2 * t410
      t548 = 0.36D2 * t365
      t549 = 0.18D2 * t359
      t550 = 0.36D2 * t386
      t551 = 0.18D2 * t363
      t552 = 0.36D2 * t371
      t553 = t358 + t403 + t543 + t544 + t545 + t546 - t547 - t548 + t54
     #9 - t550 + t551 - t552
      t555 = -0.54D2 * t53 - 0.54D2 * t43 + 0.171D3 * t45 + 0.72D2 * t10
     #5 + 0.72D2 * t99 + 0.171D3 * t51 + 0.72D2 * t101 - 0.54D2 * t47 + 
     #0.72D2 * t103 - 0.54D2 * t55 + t541 * t5 + t553 * t3
      t557 = t555 * t9 + t536 - t540 + t551 + t534 + t546 + t532 - t548 
     #- t533 + t426 + t403 + t544 - t550
      t559 = t539 + t535 + t358 - t547 - t537 + t531 - t552 + t549 + t54
     #5 + t543 + t377 - t538
      t563 = (-0.72D2 * t1 - 0.72D2 * t6 * t7 * S34 * t9 * t12) * t17 * 
     #s * t20 * z + ((((-0.216D3 * t23 - 0.216D3 * t5 - 0.216D3 * t3) * 
     #t7 - 0.216D3 + t28 * t9) * S34 + t32 + t33 + t34 + 0.216D3 * S23 +
     # t36) * t1 + (-t28 * t9 * t41 + (t49 * t5 + t57 * t3) * t7 + (t57 
     #* t5 + t49 * t3) * t9) * t12) * t17 * t20 + (-0.216D3 * t6 * t41 *
     # t9 + t107 * t1 + ((-0.432D3 + 0.216D3 * t109 * t5 + (-t33 - t36 +
     # (t106 - t54 - t104 + t46 + t52 - t44 - t102 + t100 - t56 - t48) *
     # t5) * t3) * t9 * t41 + t138 * t7 + (-0.54D2 * t103 + t119 + t137 
     #+ t102 + t136 + t48 - t123 + t56 - t130 + t120) * t9) * t12 + ((-t
     #122 - t123 + 0.18D2 * t99) * t5 * t2 + 0.18D2 * t101 - 0.18D2 * t4
     #5 + 0.18D2 * t103 - 0.18D2 * t51 + (0.18D2 * t105 - t129 - t130) *
     # t4 * t3) * t9 * t7 / t11 / S12) * s * z + t169 * t11 + (((-0.99D2
     # * t23 - t172 - t173) * t7 + 0.108D3 + t177) * S34 + (0.171D3 / 0.
     #8D1 * t27 * t9 - 0.585D3 / 0.4D1 + (t182 - t183 - t184) * t5 + (-t
     #187 + t188 - t189) * t3) * t7 + (-0.585D3 / 0.4D1 + (-t194 - t182 
     #+ t183) * t5 + (-t197 - t188 + t189) * t3) * t9) * S12 + ((0.36D2 
     #* t23 + 0.27D2 * t5 + 0.27D2 * t3) * t7 + 0.36D2 + 0.27D2 * t75 * 
     #t9) * t73 + ((t215 * t5 + t219 * t3) * t7 + (t225 * t5 + t229 * t3
     #) * t9) * S34 + ((0.9D1 / 0.4D1 + (t235 - t236 + t237 - t238) * t5
     # + (t241 - t242 + t243 - t244) * t3) * t9 - 0.99D2 * S14 - 0.99D2 
     #* S13 + (t251 - t252 + t253 - t254 + t255 + t256) * t5 + (-t259 + 
     #t260 + t261 - t262 + t263 + t251) * t3) * t7 + (-0.99D2 * S23 - 0.
     #99D2 * S24 + (-t262 - t254 + t270 + t271 + t272 + t273) * t5 + (t2
     #70 - t259 - t252 + t276 + t277 + t278) * t3) * t9 + (((0.108D3 * t
     #23 - t172 - t173) * t7 - 0.99D2 + t177) * t287 + ((t229 * t5 + t22
     #5 * t3) * t7 + (t219 * t5 + t215 * t3) * t9) * t73 + t324 * S34 + 
     #t395 * t7 + t430 * t9) * t1 + (t169 * t434 + ((0.585D3 / 0.4D1 * t
     #27 * t9 - 0.171D3 / 0.8D1 + (t182 - t184 - t188) * t5 + (-t187 - t
     #182 + t188) * t3) * t7 + (-0.171D3 / 0.8D1 + (t183 - t194 - t189) 
     #* t5 + (-t197 + t189 - t183) * t3) * t9) * t287 + (((0.9D1 / 0.4D1
     # + 0.99D2 * t109 * t5 + (-0.99D2 * S14 - 0.99D2 * S24) * t3) * t9 
     #+ t235 + t241 - t238 - t244 + (-t252 + t253 + t459 + t256 + t276 -
     # t460) * t5 + (t463 - t262 + t263 + t261 + t271 - t460) * t3) * t7
     # + (t243 - t236 + t237 - t242 + (-t262 + t459 + t260 + t273 + t272
     # - t468) * t5 + (t277 + t463 - t252 + t278 + t255 - t468) * t3) * 
     #t9) * t73 + (t502 * t7 + t517 * t9) * S34 + t557 * t7 + t559 * t9)
     # * t12
      rrgg2gghhard11J2 = t563 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard11J3
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
      t2 = S12 + S13 + S23
      t3 = 0.1D1 / t2
      t4 = S12 + S14 + S24
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = S13 + S14 + S34
      t9 = S23 + S24 + S34
      t11 = S12 ** 2
      t12 = 0.1D1 / t11
      t17 = s ** 2
      t20 = z ** 2
      t23 = t6 * t9
      t27 = -t5 - t3
      t28 = 0.216D3 * t27
      t32 = 0.216D3 * S13
      t33 = 0.216D3 * S14
      t34 = 0.432D3 * S34
      t36 = 0.216D3 * S24
      t41 = t7 * S34
      t43 = S13 ** 2
      t44 = 0.18D2 * t43
      t45 = S24 ** 2
      t46 = 0.54D2 * t45
      t47 = S13 * S24
      t48 = 0.36D2 * t47
      t51 = S14 * S23
      t52 = 0.36D2 * t51
      t53 = S14 ** 2
      t54 = 0.18D2 * t53
      t55 = S23 ** 2
      t56 = 0.54D2 * t55
      t61 = 0.54D2 * t53
      t62 = 0.18D2 * t55
      t65 = 0.54D2 * t43
      t66 = 0.18D2 * t45
      t79 = S34 ** 2
      t81 = -t27
      t105 = S14 * S24
      t106 = 0.36D2 * t105
      t107 = S23 * S24
      t108 = 0.36D2 * t107
      t109 = S13 * S14
      t110 = 0.36D2 * t109
      t111 = S13 * S23
      t112 = 0.36D2 * t111
      t113 = -0.216D3 * t79 + ((0.801D3 / 0.4D1 * t81 * t9 + 0.801D3 / 0
     #.4D1 - 0.216D3 * S24 * t5 - 0.216D3 * S23 * t3) * t7 + (0.801D3 / 
     #0.4D1 - 0.216D3 * S14 * t5 - 0.216D3 * S13 * t3) * t9) * S34 + (-0
     #.432D3 * S23 - 0.432D3 * S24 - t34 - t32 - t33) * t7 + (-0.216D3 *
     # S23 - 0.216D3 * S24) * t9 - t106 + t108 + t52 + t110 - t54 - t62 
     #- t44 - t112 - t66 + t48
      t115 = -S13 - S23
      t125 = 0.45D2 * t51
      t126 = 0.45D2 * t47
      t127 = 0.27D2 * t109
      t128 = 0.108D3 * t105
      t129 = 0.27D2 * t107
      t132 = 0.108D3 * t111
      t137 = 0.108D3 * t107
      t138 = 0.27D2 * t111
      t139 = 0.27D2 * t105
      t140 = ((t125 + t126 + t127 + t44 - t46 + t112 - t61 + t62 - t128 
     #+ t129) * t5 + (t127 + t106 + t126 + t125 + t129 - t132 - t56 + t5
     #4 + t66 - t65) * t3) * t9 + t54 - t56 + t44 - t46 - t137 + t125 + 
     #t138 + t139 + t110 + t126
      t142 = 0.108D3 * t109
      t154 = 0.18D2 * t47
      t156 = 0.18D2 * t51
      t172 = 0.9D1 / 0.8D1 * t81
      t175 = t172 * t7 + t172 * t9
      t178 = 0.18D2 * t5
      t179 = 0.18D2 * t3
      t183 = 0.18D2 * t27 * t9
      t188 = 0.27D2 / 0.8D1 * S24
      t189 = 0.27D2 / 0.8D1 * S14
      t190 = 0.9D1 / 0.2D1 * S13
      t193 = 0.9D1 / 0.2D1 * S14
      t194 = 0.27D2 / 0.8D1 * S23
      t195 = 0.27D2 / 0.8D1 * S13
      t200 = 0.9D1 / 0.2D1 * S23
      t203 = 0.9D1 / 0.2D1 * S24
      t221 = 0.36D2 * S14 - 0.27D2 * S23
      t225 = -0.27D2 * S24 + 0.36D2 * S13
      t231 = -0.27D2 * S13 + 0.36D2 * S24
      t235 = 0.36D2 * S23 - 0.27D2 * S14
      t241 = 0.4131D4 / 0.16D2 * S13
      t242 = 0.2169D4 / 0.16D2 * S14
      t243 = 0.4131D4 / 0.16D2 * S23
      t244 = 0.2169D4 / 0.16D2 * S24
      t247 = 0.4131D4 / 0.16D2 * S14
      t248 = 0.2169D4 / 0.16D2 * S13
      t249 = 0.4131D4 / 0.16D2 * S24
      t250 = 0.2169D4 / 0.16D2 * S23
      t257 = 0.9D1 * t109
      t258 = 0.27D2 * t47
      t259 = 0.27D2 / 0.2D1 * t43
      t260 = 0.27D2 / 0.4D1 * t105
      t261 = 0.27D2 / 0.8D1 * t53
      t262 = 0.63D2 / 0.8D1 * t45
      t265 = 0.27D2 / 0.4D1 * t111
      t266 = 0.27D2 / 0.8D1 * t43
      t267 = 0.27D2 / 0.2D1 * t53
      t268 = 0.27D2 * t51
      t269 = 0.63D2 / 0.8D1 * t55
      t276 = 0.9D1 * t107
      t277 = 0.27D2 / 0.8D1 * t45
      t278 = 0.63D2 / 0.8D1 * t53
      t279 = 0.27D2 / 0.2D1 * t55
      t282 = 0.27D2 / 0.8D1 * t55
      t283 = 0.27D2 / 0.2D1 * t45
      t284 = 0.63D2 / 0.8D1 * t43
      t293 = t79 * S34
      t311 = 0.108D3 * t47
      t315 = 0.108D3 * t51
      t332 = ((0.2403D4 / 0.2D1 + (t112 - t66 - t62 - t54 - t44 + t106 +
     # t268 - t127 - t129 + t258) * t5 * t3) * t9 + (t268 + 0.180D3 * t4
     #3 - t54 - t311 - t62 - 0.27D2 * t45) * t5 + (-t315 - t66 - 0.27D2 
     #* t55 + 0.180D3 * t53 + t258 - t44) * t3) * t7 + ((-t315 + 0.180D3
     # * t55 - t44 - 0.27D2 * t53 - t66 + t258) * t5 + (-t311 + 0.180D3 
     #* t45 - t62 - 0.27D2 * t43 + t268 - t54) * t3) * t9 + t110 - t138 
     #- t139 + t258 - t44 - t66 - t54 - t62 + t108 + t268
      t334 = 0.1431D4 / 0.8D1 * S23
      t335 = 0.1431D4 / 0.8D1 * S24
      t336 = 0.1431D4 / 0.8D1 * S13
      t337 = 0.1431D4 / 0.8D1 * S14
      t339 = 0.3339D4 / 0.16D2 * t47
      t340 = 0.3627D4 / 0.16D2 * t109
      t341 = 0.927D3 / 0.8D1 * t45
      t342 = 0.3627D4 / 0.16D2 * t107
      t343 = 0.927D3 / 0.8D1 * t53
      t344 = 0.3339D4 / 0.16D2 * t51
      t348 = 0.927D3 / 0.8D1 * t55
      t349 = 0.927D3 / 0.8D1 * t43
      t354 = 0.171D3 / 0.8D1 * t43
      t355 = 0.171D3 / 0.8D1 * t53
      t356 = 0.243D3 / 0.16D2 * t55
      t357 = 0.54D2 * t111
      t358 = 0.54D2 * t105
      t360 = 0.243D3 / 0.16D2 * t45
      t361 = t53 * S14
      t362 = 0.9D1 / 0.8D1 * t361
      t363 = t43 * S13
      t364 = 0.18D2 * t363
      t365 = S13 * t45
      t366 = 0.189D3 / 0.2D1 * t365
      t367 = t53 * S24
      t369 = t43 * S14
      t371 = t109 * S24
      t372 = 0.27D2 * t371
      t373 = S14 * t45
      t375 = t45 * S24
      t376 = 0.333D3 / 0.8D1 * t375
      t377 = t43 * S24
      t378 = 0.171D3 / 0.2D1 * t377
      t379 = S13 * t53
      t383 = 0.18D2 * t361
      t384 = 0.9D1 / 0.8D1 * t363
      t385 = t55 * S23
      t386 = 0.333D3 / 0.8D1 * t385
      t387 = t43 * S23
      t390 = t53 * S23
      t391 = 0.171D3 / 0.2D1 * t390
      t392 = S14 * t55
      t393 = 0.189D3 / 0.2D1 * t392
      t395 = S13 * t55
      t397 = t109 * S23
      t398 = 0.27D2 * t397
      t401 = (t334 + t335 + t336 + t337 + (-0.1215D4 / 0.4D1 * t105 + t3
     #39 + t340 - t65 - t341 + t342 - t343 + t344 - t56 - t132) * t5 + (
     #-0.1215D4 / 0.4D1 * t111 + t344 - t128 + t342 - t46 + t340 - t61 -
     # t348 - t349 + t339) * t3) * t9 + t257 - t354 - t355 + t356 - t357
     # - t358 + 0.621D3 / 0.8D1 * t107 + t156 + t360 + t154 + (-t362 - t
     #364 - t366 + 0.27D2 / 0.8D1 * t367 - 0.27D2 / 0.2D1 * t369 + t372 
     #- 0.63D2 / 0.8D1 * t373 + t376 + t378 - 0.9D1 / 0.2D1 * t379) * t5
     # + (-t383 - t384 + t386 + 0.27D2 / 0.8D1 * t387 - 0.9D1 / 0.2D1 * 
     #t369 + t391 - t393 - 0.27D2 / 0.2D1 * t379 - 0.63D2 / 0.8D1 * t395
     # + t398) * t3
      t403 = 0.171D3 / 0.8D1 * t45
      t404 = 0.243D3 / 0.16D2 * t43
      t406 = 0.243D3 / 0.16D2 * t53
      t407 = 0.171D3 / 0.8D1 * t55
      t408 = 0.9D1 / 0.8D1 * t375
      t409 = 0.18D2 * t385
      t411 = 0.189D3 / 0.2D1 * t390
      t412 = 0.333D3 / 0.8D1 * t361
      t413 = 0.171D3 / 0.2D1 * t392
      t414 = t45 * S23
      t416 = t51 * S24
      t417 = 0.27D2 * t416
      t419 = t55 * S24
      t423 = 0.333D3 / 0.8D1 * t363
      t424 = 0.9D1 / 0.8D1 * t385
      t427 = t47 * S23
      t428 = 0.27D2 * t427
      t430 = 0.171D3 / 0.2D1 * t365
      t431 = 0.189D3 / 0.2D1 * t377
      t432 = 0.18D2 * t375
      t436 = -t403 + t404 + 0.621D3 / 0.8D1 * t109 - t358 + t406 - t407 
     #- t357 + t276 + t156 + t154 + (-t408 - t409 + 0.27D2 / 0.8D1 * t37
     #3 - t411 + t412 + t413 - 0.9D1 / 0.2D1 * t414 + t417 - 0.63D2 / 0.
     #8D1 * t367 - 0.27D2 / 0.2D1 * t419) * t5 + (t423 - t424 + 0.27D2 /
     # 0.8D1 * t395 - 0.9D1 / 0.2D1 * t419 + t428 - 0.63D2 / 0.8D1 * t38
     #7 + t430 - t431 - t432 - 0.27D2 / 0.2D1 * t414) * t3
      t440 = t79 ** 2
      t465 = 0.9D1 * t111
      t466 = 0.27D2 / 0.4D1 * t107
      t469 = 0.9D1 * t105
      t474 = 0.27D2 / 0.4D1 * t109
      t483 = 0.54D2 * t107
      t485 = 0.54D2 * t109
      t493 = 0.3627D4 / 0.16D2 * t105
      t495 = 0.3627D4 / 0.16D2 * t111
      t508 = (t334 + t335 + t336 + t337 + (t360 - t483 + t465 + t154 - t
     #354 + t156 - t407 + 0.621D3 / 0.8D1 * t105 + t406 - t485) * t5 + (
     #-t483 + t356 + 0.621D3 / 0.8D1 * t111 + t156 + t469 - t485 + t154 
     #+ t404 - t355 - t403) * t3) * t9 - t142 - t65 + t339 - t61 - t348 
     #+ t493 - t341 - 0.1215D4 / 0.4D1 * t107 + t344 + t495 + (-t366 - 0
     #.27D2 / 0.2D1 * t387 + t376 + 0.27D2 / 0.8D1 * t419 - 0.63D2 / 0.8
     #D1 * t414 - t364 - 0.9D1 / 0.2D1 * t395 - t424 + t378 + t428) * t5
     # + (-t383 + t386 + t417 - t408 - 0.27D2 / 0.2D1 * t367 - 0.9D1 / 0
     #.2D1 * t373 + t391 + 0.27D2 / 0.8D1 * t414 - t393 - 0.63D2 / 0.8D1
     # * t419) * t3
      t523 = t344 + t339 - 0.1215D4 / 0.4D1 * t109 - t343 - t56 - t349 -
     # t46 + t495 + t493 - t137 + (-t384 + 0.27D2 / 0.8D1 * t369 - 0.63D
     #2 / 0.8D1 * t379 - 0.27D2 / 0.2D1 * t395 + t398 - t411 + t413 + t4
     #12 - t409 - 0.9D1 / 0.2D1 * t387) * t5 + (-t362 + t423 + 0.27D2 / 
     #0.8D1 * t379 - 0.9D1 / 0.2D1 * t367 - t432 + t430 - 0.27D2 / 0.2D1
     # * t373 - 0.63D2 / 0.8D1 * t369 + t372 - t431) * t3
      t537 = 0.54D2 * t375
      t538 = 0.18D2 * t392
      t540 = 0.36D2 * t397
      t541 = 0.54D2 * t361
      t544 = 0.18D2 * t377
      t545 = 0.36D2 * t427
      t546 = 0.36D2 * t390
      t548 = 0.36D2 * t365
      t549 = t537 + t538 + 0.18D2 * t395 - t540 + t541 + 0.54D2 * t379 +
     # 0.54D2 * t414 + t544 - t545 - t546 + 0.18D2 * t387 - t548
      t551 = 0.54D2 * t363
      t552 = 0.54D2 * t385
      t554 = 0.18D2 * t390
      t557 = 0.36D2 * t416
      t558 = 0.36D2 * t371
      t559 = 0.18D2 * t365
      t560 = 0.36D2 * t392
      t562 = 0.36D2 * t377
      t563 = t551 + t552 + 0.54D2 * t419 + t554 + 0.18D2 * t367 + 0.18D2
     # * t373 - t557 - t558 + t559 - t560 + 0.54D2 * t369 - t562
      t565 = -0.45D2 * t53 - 0.45D2 * t43 + 0.333D3 * t47 + 0.162D3 * t1
     #11 + 0.162D3 * t105 + 0.333D3 * t51 + 0.162D3 * t107 - 0.45D2 * t4
     #5 + 0.162D3 * t109 - 0.45D2 * t55 + t549 * t5 + t563 * t3
      t571 = t565 * t9 + t544 - t548 + 0.18D2 * t369 + 0.18D2 * t379 + 0
     #.54D2 * t373 + 0.54D2 * t395 - t558 - t540 + t537 + t552 + t554 - 
     #t560
      t577 = 0.54D2 * t387 + 0.18D2 * t414 + t551 - t557 - t545 + t538 -
     # t562 + t559 + 0.54D2 * t367 + 0.18D2 * t419 + t541 - t546
      t581 = (-0.72D2 * t1 - 0.72D2 * t6 * t7 * S34 * t9 * t12) * t17 * 
     #s * t20 * z + ((((-0.216D3 * t23 - 0.216D3 * t5 - 0.216D3 * t3) * 
     #t7 - 0.216D3 + t28 * t9) * S34 + t32 + t33 + t34 + 0.216D3 * S23 +
     # t36) * t1 + (-t28 * t9 * t41 + ((t44 + t46 - t48) * t5 + (-t52 + 
     #t54 + t56) * t3) * t7 + ((-t52 + t61 + t62) * t5 + (t65 + t66 - t4
     #8) * t3) * t9) * t12) * t17 * t20 + (-0.216D3 * t6 * t41 * t9 + t1
     #13 * t1 + ((-0.432D3 + 0.216D3 * t115 * t5 + (-t33 - t36 + (t112 -
     # t54 - t110 + t48 + t52 - t44 - t108 + t106 - t62 - t66) * t5) * t
     #3) * t9 * t41 + t140 * t7 + (-t142 + t125 + t139 + t108 + t138 + t
     #66 - t61 + t62 - t65 + t126) * t9) * t12 + ((-0.9D1 * t45 - 0.9D1 
     #* t53 + 0.18D2 * t105) * t5 * t2 + 0.18D2 * t107 - t154 + 0.18D2 *
     # t109 - t156 + (0.18D2 * t111 - 0.9D1 * t55 - 0.9D1 * t43) * t4 * 
     #t3) * t9 * t7 / t11 / S12) * s * z + t175 * t11 + (((-0.27D2 * t23
     # - t178 - t179) * t7 + 0.180D3 + t183) * S34 + (0.27D2 / 0.16D2 * 
     #t27 * t9 - 0.585D3 / 0.4D1 + (t188 - t189 - t190) * t5 + (-t193 + 
     #t194 - t195) * t3) * t7 + (-0.585D3 / 0.4D1 + (-t200 - t188 + t189
     #) * t5 + (-t203 - t194 + t195) * t3) * t9) * S12 + ((-0.108D3 * t2
     #3 + 0.27D2 * t5 + 0.27D2 * t3) * t7 - 0.108D3 + 0.27D2 * t81 * t9)
     # * t79 + ((t221 * t5 + t225 * t3) * t7 + (t231 * t5 + t235 * t3) *
     # t9) * S34 + ((-0.297D3 / 0.8D1 + (t241 - t242 + t243 - t244) * t5
     # + (t247 - t248 + t249 - t250) * t3) * t9 - 0.99D2 * S14 - 0.99D2 
     #* S13 + (t257 - t258 + t259 - t260 + t261 + t262) * t5 + (-t265 + 
     #t266 + t267 - t268 + t269 + t257) * t3) * t7 + (-0.99D2 * S23 - 0.
     #99D2 * S24 + (-t268 - t260 + t276 + t277 + t278 + t279) * t5 + (t2
     #76 - t265 - t258 + t282 + t283 + t284) * t3) * t9 + (((0.180D3 * t
     #23 - t178 - t179) * t7 - 0.27D2 + t183) * t293 + ((t235 * t5 + t23
     #1 * t3) * t7 + (t225 * t5 + t221 * t3) * t9) * t79 + t332 * S34 + 
     #t401 * t7 + t436 * t9) * t1 + (t175 * t440 + ((0.585D3 / 0.4D1 * t
     #27 * t9 - 0.27D2 / 0.16D2 + (t188 - t190 - t194) * t5 + (-t193 - t
     #188 + t194) * t3) * t7 + (-0.27D2 / 0.16D2 + (t189 - t200 - t195) 
     #* t5 + (-t203 + t195 - t189) * t3) * t9) * t293 + (((-0.297D3 / 0.
     #8D1 + 0.99D2 * t115 * t5 + (-0.99D2 * S14 - 0.99D2 * S24) * t3) * 
     #t9 + t241 + t247 - t244 - t250 + (-t258 + t259 + t465 + t262 + t28
     #2 - t466) * t5 + (t469 - t268 + t269 + t267 + t277 - t466) * t3) *
     # t7 + (t249 - t242 + t243 - t248 + (-t268 + t465 + t266 + t279 + t
     #278 - t474) * t5 + (t283 + t469 - t258 + t284 + t261 - t474) * t3)
     # * t9) * t79 + (t508 * t7 + t523 * t9) * S34 + t571 * t7 + t577 * 
     #t9) * t12
      rrgg2gghhard11J3 = t581 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard11J4
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
      t2 = S12 + S13 + S23
      t3 = 0.1D1 / t2
      t4 = S12 + S14 + S24
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = S13 + S14 + S34
      t9 = S23 + S24 + S34
      t11 = S12 ** 2
      t12 = 0.1D1 / t11
      t17 = s ** 2
      t20 = z ** 2
      t23 = t6 * t9
      t27 = -t5 - t3
      t28 = 0.216D3 * t27
      t32 = 0.216D3 * S13
      t33 = 0.216D3 * S14
      t34 = 0.432D3 * S34
      t36 = 0.216D3 * S24
      t41 = t7 * S34
      t43 = S13 ** 2
      t44 = 0.18D2 * t43
      t45 = S13 * S24
      t46 = 0.36D2 * t45
      t47 = S24 ** 2
      t51 = S14 * S23
      t52 = 0.36D2 * t51
      t53 = S14 ** 2
      t54 = 0.18D2 * t53
      t55 = S23 ** 2
      t61 = 0.18D2 * t55
      t66 = 0.18D2 * t47
      t79 = S34 ** 2
      t81 = -t27
      t105 = S13 * S23
      t106 = 0.36D2 * t105
      t107 = S23 * S24
      t108 = 0.36D2 * t107
      t109 = S14 * S24
      t110 = 0.36D2 * t109
      t111 = S13 * S14
      t112 = 0.36D2 * t111
      t113 = -0.216D3 * t79 + ((0.369D3 / 0.4D1 * t81 * t9 + 0.369D3 / 0
     #.4D1 - 0.216D3 * S24 * t5 - 0.216D3 * S23 * t3) * t7 + (0.369D3 / 
     #0.4D1 - 0.216D3 * S14 * t5 - 0.216D3 * S13 * t3) * t9) * S34 + (-0
     #.432D3 * S23 - 0.432D3 * S24 - t34 - t32 - t33) * t7 + (-0.216D3 *
     # S23 - 0.216D3 * S24) * t9 - t106 + t108 - t44 + t52 + t46 - t110 
     #- t61 - t54 + t112 - t66
      t115 = -S13 - S23
      t125 = 0.99D2 * t47
      t126 = 0.99D2 * t53
      t127 = 0.45D2 * t51
      t129 = 0.27D2 * t107
      t130 = 0.45D2 * t45
      t131 = 0.27D2 * t111
      t134 = 0.99D2 * t55
      t135 = 0.99D2 * t43
      t141 = 0.27D2 * t109
      t142 = 0.27D2 * t105
      t144 = ((t61 - t125 + t44 - t126 + t127 - 0.162D3 * t109 + t129 + 
     #t106 + t130 + t131) * t5 + (-t134 + t130 - t135 + t66 - 0.162D3 * 
     #t105 + t54 + t110 + t129 + t127 + t131) * t3) * t9 - t134 + t54 - 
     #t125 + t44 + t141 + t112 + t130 + t142 + t127 - 0.162D3 * t107
      t176 = 0.9D1 / 0.8D1 * t81
      t179 = t176 * t7 + t176 * t9
      t182 = 0.18D2 * t5
      t183 = 0.18D2 * t3
      t186 = 0.18D2 * t27
      t187 = t186 * t9
      t192 = 0.27D2 / 0.8D1 * S24
      t193 = 0.27D2 / 0.8D1 * S14
      t194 = 0.9D1 / 0.2D1 * S13
      t197 = 0.9D1 / 0.2D1 * S14
      t198 = 0.27D2 / 0.8D1 * S23
      t199 = 0.27D2 / 0.8D1 * S13
      t204 = 0.9D1 / 0.2D1 * S23
      t207 = 0.9D1 / 0.2D1 * S24
      t214 = 0.252D3 * t23
      t225 = 0.36D2 * S14 - 0.27D2 * S23
      t229 = -0.27D2 * S24 + 0.36D2 * S13
      t235 = -0.27D2 * S13 + 0.36D2 * S24
      t239 = 0.36D2 * S23 - 0.27D2 * S14
      t245 = 0.189D3 * S24
      t246 = 0.2745D4 / 0.8D1 * S23
      t247 = 0.2745D4 / 0.8D1 * S13
      t248 = 0.189D3 * S14
      t251 = 0.2745D4 / 0.8D1 * S14
      t252 = 0.189D3 * S13
      t253 = 0.189D3 * S23
      t254 = 0.2745D4 / 0.8D1 * S24
      t261 = 0.9D1 * t111
      t262 = 0.27D2 * t45
      t263 = 0.27D2 / 0.2D1 * t43
      t264 = 0.27D2 / 0.4D1 * t109
      t265 = 0.27D2 / 0.8D1 * t53
      t266 = 0.63D2 / 0.8D1 * t47
      t269 = 0.27D2 / 0.4D1 * t105
      t270 = 0.27D2 / 0.8D1 * t43
      t271 = 0.27D2 / 0.2D1 * t53
      t272 = 0.27D2 * t51
      t273 = 0.63D2 / 0.8D1 * t55
      t280 = 0.9D1 * t107
      t281 = 0.27D2 / 0.8D1 * t47
      t282 = 0.63D2 / 0.8D1 * t53
      t283 = 0.27D2 / 0.2D1 * t55
      t286 = 0.27D2 / 0.8D1 * t55
      t287 = 0.27D2 / 0.2D1 * t47
      t288 = 0.63D2 / 0.8D1 * t43
      t296 = t79 * S34
      t315 = 0.252D3 * t45
      t320 = 0.252D3 * t51
      t335 = ((0.2187D4 / 0.2D1 + (t106 - t66 - t61 - t54 - t44 + t110 +
     # t272 - t131 - t129 + t262) * t5 * t3) * t9 + (-t61 + 0.252D3 * t4
     #3 + t272 + 0.45D2 * t47 - t315 - t54) * t5 + (-t44 + 0.252D3 * t53
     # + 0.45D2 * t55 - t320 - t66 + t262) * t3) * t7 + ((0.45D2 * t53 -
     # t320 - t44 + t262 - t66 + 0.252D3 * t55) * t5 + (-t61 + t272 + 0.
     #252D3 * t47 - t54 - t315 + 0.45D2 * t43) * t3) * t9 - t61 - t141 +
     # t112 + t262 - t142 - t54 - t44 + t272 + t108 - t66
      t337 = 0.2313D4 / 0.16D2 * S24
      t338 = 0.2313D4 / 0.16D2 * S14
      t339 = 0.2313D4 / 0.16D2 * S13
      t340 = 0.2313D4 / 0.16D2 * S23
      t341 = 0.2565D4 / 0.8D1 * t111
      t343 = 0.171D3 * t47
      t344 = 0.2277D4 / 0.8D1 * t51
      t345 = 0.189D3 / 0.2D1 * t55
      t346 = 0.189D3 / 0.2D1 * t43
      t348 = 0.171D3 * t53
      t349 = 0.2565D4 / 0.8D1 * t107
      t350 = 0.2277D4 / 0.8D1 * t45
      t353 = 0.171D3 * t43
      t355 = 0.189D3 / 0.2D1 * t47
      t356 = 0.171D3 * t55
      t357 = 0.189D3 / 0.2D1 * t53
      t363 = 0.63D2 * t105
      t365 = 0.531D3 / 0.16D2 * t55
      t366 = 0.63D2 * t109
      t367 = 0.531D3 / 0.16D2 * t47
      t368 = 0.171D3 / 0.8D1 * t43
      t369 = 0.171D3 / 0.8D1 * t53
      t370 = t43 * S24
      t371 = 0.207D3 / 0.2D1 * t370
      t372 = t53 * S14
      t373 = 0.9D1 / 0.8D1 * t372
      t374 = t43 * S13
      t375 = 0.18D2 * t374
      t376 = t47 * S24
      t377 = 0.477D3 / 0.8D1 * t376
      t378 = S13 * t53
      t380 = S13 * t47
      t381 = 0.261D3 / 0.2D1 * t380
      t382 = t53 * S24
      t384 = t43 * S14
      t386 = S14 * t47
      t388 = t111 * S24
      t389 = 0.27D2 * t388
      t392 = 0.18D2 * t372
      t393 = 0.9D1 / 0.8D1 * t374
      t394 = t55 * S23
      t395 = 0.477D3 / 0.8D1 * t394
      t397 = t53 * S23
      t398 = 0.207D3 / 0.2D1 * t397
      t399 = S14 * t55
      t400 = 0.261D3 / 0.2D1 * t399
      t401 = t43 * S23
      t403 = t111 * S23
      t404 = 0.27D2 * t403
      t406 = S13 * t55
      t410 = (t337 + t338 + t339 + t340 + (t341 - 0.486D3 * t109 - t343 
     #+ t344 - t345 - t346 - 0.189D3 * t105 - t348 + t349 + t350) * t5 +
     # (-t353 - 0.189D3 * t109 + t349 + t350 + t344 + t341 - t355 - t356
     # - t357 - 0.486D3 * t105) * t3) * t9 + t261 - t363 + t272 + 0.909D
     #3 / 0.8D1 * t107 + t365 - t366 + t367 - t368 + t262 - t369 + (t371
     # - t373 - t375 + t377 - 0.9D1 / 0.2D1 * t378 - t381 + 0.27D2 / 0.8
     #D1 * t382 - 0.27D2 / 0.2D1 * t384 - 0.63D2 / 0.8D1 * t386 + t389) 
     #* t5 + (-t392 - t393 + t395 - 0.9D1 / 0.2D1 * t384 + t398 - t400 +
     # 0.27D2 / 0.8D1 * t401 + t404 - 0.27D2 / 0.2D1 * t378 - 0.63D2 / 0
     #.8D1 * t406) * t3
      t413 = 0.171D3 / 0.8D1 * t55
      t414 = 0.531D3 / 0.16D2 * t53
      t415 = 0.171D3 / 0.8D1 * t47
      t416 = 0.531D3 / 0.16D2 * t43
      t417 = t55 * S24
      t419 = 0.207D3 / 0.2D1 * t399
      t420 = 0.477D3 / 0.8D1 * t372
      t421 = t47 * S23
      t423 = t51 * S24
      t424 = 0.27D2 * t423
      t426 = 0.9D1 / 0.8D1 * t376
      t427 = 0.261D3 / 0.2D1 * t397
      t429 = 0.18D2 * t394
      t432 = 0.261D3 / 0.2D1 * t370
      t433 = 0.207D3 / 0.2D1 * t380
      t437 = 0.9D1 / 0.8D1 * t394
      t438 = 0.18D2 * t376
      t439 = 0.477D3 / 0.8D1 * t374
      t440 = t45 * S23
      t441 = 0.27D2 * t440
      t445 = 0.909D3 / 0.8D1 * t111 + t262 + t272 + t280 - t363 - t366 -
     # t413 + t414 - t415 + t416 + (-0.27D2 / 0.2D1 * t417 + t419 + t420
     # - 0.9D1 / 0.2D1 * t421 + t424 + 0.27D2 / 0.8D1 * t386 - t426 - t4
     #27 - 0.63D2 / 0.8D1 * t382 - t429) * t5 + (-t432 + t433 - 0.9D1 / 
     #0.2D1 * t417 - 0.27D2 / 0.2D1 * t421 + 0.27D2 / 0.8D1 * t406 - t43
     #7 - t438 + t439 + t441 - 0.63D2 / 0.8D1 * t401) * t3
      t449 = t79 ** 2
      t474 = 0.9D1 * t105
      t475 = 0.27D2 / 0.4D1 * t107
      t478 = 0.9D1 * t109
      t483 = 0.27D2 / 0.4D1 * t111
      t492 = 0.63D2 * t111
      t494 = 0.63D2 * t107
      t502 = 0.2565D4 / 0.8D1 * t105
      t503 = 0.2565D4 / 0.8D1 * t109
      t518 = (t337 + t338 + t339 + t340 + (-t492 + t367 + t272 - t413 + 
     #t414 + 0.909D3 / 0.8D1 * t109 - t368 + t474 + t262 - t494) * t5 + 
     #(t365 + t416 - t494 + t262 + t272 - t492 - t415 + 0.909D3 / 0.8D1 
     #* t105 - t369 + t478) * t3) * t9 + t502 + t503 + t344 - 0.486D3 * 
     #t107 + t350 - 0.189D3 * t111 - t356 - t357 - t343 - t346 + (0.27D2
     # / 0.8D1 * t417 - t375 - t437 - 0.63D2 / 0.8D1 * t421 + t371 - t38
     #1 + t377 + t441 - 0.27D2 / 0.2D1 * t401 - 0.9D1 / 0.2D1 * t406) * 
     #t5 + (-0.63D2 / 0.8D1 * t417 + 0.27D2 / 0.8D1 * t421 - 0.27D2 / 0.
     #2D1 * t382 - t392 + t395 - t426 + t424 - t400 + t398 - 0.9D1 / 0.2
     #D1 * t386) * t3
      t534 = t503 - 0.189D3 * t107 + t502 + t350 + t344 - 0.486D3 * t111
     # - t345 - t348 - t355 - t353 + (-0.27D2 / 0.2D1 * t406 - t427 + t4
     #19 - 0.63D2 / 0.8D1 * t378 + t404 + t420 + 0.27D2 / 0.8D1 * t384 -
     # t429 - 0.9D1 / 0.2D1 * t401 - t393) * t5 + (-t432 + t433 + t389 -
     # 0.9D1 / 0.2D1 * t382 - t373 + t439 - 0.63D2 / 0.8D1 * t384 + 0.27
     #D2 / 0.8D1 * t378 - 0.27D2 / 0.2D1 * t386 - t438) * t3
      t548 = 0.18D2 * t399
      t550 = 0.90D2 * t372
      t551 = 0.90D2 * t376
      t552 = 0.36D2 * t397
      t553 = 0.18D2 * t370
      t554 = 0.36D2 * t380
      t555 = 0.36D2 * t440
      t556 = 0.36D2 * t403
      t560 = t548 + 0.90D2 * t421 + t550 + t551 - t552 + t553 - t554 - t
     #555 - t556 + 0.18D2 * t406 + 0.90D2 * t378 + 0.18D2 * t401
      t562 = 0.36D2 * t370
      t563 = 0.18D2 * t380
      t566 = 0.36D2 * t388
      t568 = 0.90D2 * t374
      t569 = 0.90D2 * t394
      t570 = 0.36D2 * t423
      t571 = 0.36D2 * t399
      t572 = 0.18D2 * t397
      t574 = -t562 + t563 + 0.90D2 * t417 + 0.90D2 * t384 - t566 + 0.18D
     #2 * t382 + t568 + t569 - t570 - t571 + t572 + 0.18D2 * t386
      t576 = 0.495D3 * t51 + 0.252D3 * t109 + 0.495D3 * t45 + 0.252D3 * 
     #t107 + 0.252D3 * t105 + 0.252D3 * t111 - 0.36D2 * t55 - 0.36D2 * t
     #53 - 0.36D2 * t47 - 0.36D2 * t43 + t560 * t5 + t574 * t3
      t582 = t576 * t9 - t571 + t553 - t566 - t556 + 0.18D2 * t378 + t55
     #1 + t569 - t554 + t572 + 0.90D2 * t386 + 0.90D2 * t406 + 0.18D2 * 
     #t384
      t588 = 0.18D2 * t421 - t570 - t552 + t550 + t568 - t555 + t548 - t
     #562 + t563 + 0.18D2 * t417 + 0.90D2 * t401 + 0.90D2 * t382
      t592 = (-0.72D2 * t1 - 0.72D2 * t6 * t7 * S34 * t9 * t12) * t17 * 
     #s * t20 * z + ((((-0.216D3 * t23 - 0.216D3 * t5 - 0.216D3 * t3) * 
     #t7 - 0.216D3 + t28 * t9) * S34 + t32 + t33 + t34 + 0.216D3 * S23 +
     # t36) * t1 + (-t28 * t9 * t41 + ((t44 - t46 + 0.90D2 * t47) * t5 +
     # (-t52 + t54 + 0.90D2 * t55) * t3) * t7 + ((t61 - t52 + 0.90D2 * t
     #53) * t5 + (-t46 + 0.90D2 * t43 + t66) * t3) * t9) * t12) * t17 * 
     #t20 + (-0.216D3 * t6 * t41 * t9 + t113 * t1 + ((-0.432D3 + 0.216D3
     # * t115 * t5 + (-t33 - t36 + (t106 - t54 - t112 + t46 + t52 - t44 
     #- t108 + t110 - t61 - t66) * t5) * t3) * t9 * t41 + t144 * t7 + (t
     #142 - t126 + t141 + t108 + t66 + t127 + t130 - 0.162D3 * t111 - t1
     #35 + t61) * t9) * t12 + ((-0.9D1 * t47 - 0.9D1 * t53 + 0.18D2 * t1
     #09) * t5 * t2 + 0.18D2 * t107 - 0.18D2 * t45 + 0.18D2 * t111 - 0.1
     #8D2 * t51 + (0.18D2 * t105 - 0.9D1 * t55 - 0.9D1 * t43) * t4 * t3)
     # * t9 * t7 / t11 / S12) * s * z + t179 * t11 + (((0.45D2 * t23 - t
     #182 - t183) * t7 + 0.252D3 + t187) * S34 + (-t186 * t9 - 0.585D3 /
     # 0.4D1 + (t192 - t193 - t194) * t5 + (-t197 + t198 - t199) * t3) *
     # t7 + (-0.585D3 / 0.4D1 + (-t204 - t192 + t193) * t5 + (-t207 - t1
     #98 + t199) * t3) * t9) * S12 + ((-t214 + 0.27D2 * t5 + 0.27D2 * t3
     #) * t7 - 0.252D3 + 0.27D2 * t81 * t9) * t79 + ((t225 * t5 + t229 *
     # t3) * t7 + (t235 * t5 + t239 * t3) * t9) * S34 + ((-0.153D3 / 0.2
     #D1 + (-t245 + t246 + t247 - t248) * t5 + (t251 - t252 - t253 + t25
     #4) * t3) * t9 - 0.99D2 * S13 - 0.99D2 * S14 + (t261 - t262 + t263 
     #- t264 + t265 + t266) * t5 + (-t269 + t270 + t271 - t272 + t273 + 
     #t261) * t3) * t7 + (-0.99D2 * S23 - 0.99D2 * S24 + (-t272 - t264 +
     # t280 + t281 + t282 + t283) * t5 + (t280 - t269 - t262 + t286 + t2
     #87 + t288) * t3) * t9 + (((t214 - t182 - t183) * t7 + 0.45D2 + t18
     #7) * t296 + ((t239 * t5 + t235 * t3) * t7 + (t229 * t5 + t225 * t3
     #) * t9) * t79 + t335 * S34 + t410 * t7 + t445 * t9) * t1 + (t179 *
     # t449 + ((0.585D3 / 0.4D1 * t27 * t9 + 0.18D2 + (t192 - t194 - t19
     #8) * t5 + (-t197 - t192 + t198) * t3) * t7 + (0.18D2 + (t193 - t20
     #4 - t199) * t5 + (-t207 + t199 - t193) * t3) * t9) * t296 + (((-0.
     #153D3 / 0.2D1 + 0.99D2 * t115 * t5 + (-0.99D2 * S14 - 0.99D2 * S24
     #) * t3) * t9 - t245 + t251 + t247 - t253 + (-t262 + t263 + t474 + 
     #t266 + t286 - t475) * t5 + (t478 - t272 + t273 + t271 + t281 - t47
     #5) * t3) * t7 + (t254 + t246 - t248 - t252 + (-t272 + t474 + t270 
     #+ t283 + t282 - t483) * t5 + (t287 + t478 - t262 + t288 + t265 - t
     #483) * t3) * t9) * t79 + (t518 * t7 + t534 * t9) * S34 + t582 * t7
     # + t588 * t9) * t12
      rrgg2gghhard11J4 = t592 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard11J5
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
      t2 = S12 + S13 + S23
      t3 = 0.1D1 / t2
      t4 = S12 + S14 + S24
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = S13 + S14 + S34
      t9 = S23 + S24 + S34
      t11 = S12 ** 2
      t12 = 0.1D1 / t11
      t17 = s ** 2
      t20 = z ** 2
      t23 = t6 * t9
      t27 = -t5 - t3
      t28 = 0.216D3 * t27
      t32 = 0.216D3 * S13
      t33 = 0.216D3 * S14
      t34 = 0.432D3 * S34
      t36 = 0.216D3 * S24
      t41 = t7 * S34
      t43 = S13 ** 2
      t44 = 0.18D2 * t43
      t45 = S13 * S24
      t46 = 0.36D2 * t45
      t47 = S24 ** 2
      t51 = S14 * S23
      t52 = 0.36D2 * t51
      t53 = S14 ** 2
      t54 = 0.18D2 * t53
      t55 = S23 ** 2
      t61 = 0.18D2 * t55
      t66 = 0.18D2 * t47
      t79 = S34 ** 2
      t104 = S13 * S23
      t105 = 0.36D2 * t104
      t106 = S23 * S24
      t107 = 0.36D2 * t106
      t108 = S14 * S24
      t109 = 0.36D2 * t108
      t110 = S13 * S14
      t111 = 0.36D2 * t110
      t112 = -0.216D3 * t79 + ((0.63D2 / 0.4D1 * t27 * t9 - 0.63D2 / 0.4
     #D1 - 0.216D3 * S24 * t5 - 0.216D3 * S23 * t3) * t7 + (-0.63D2 / 0.
     #4D1 - 0.216D3 * S14 * t5 - 0.216D3 * S13 * t3) * t9) * S34 + (-0.4
     #32D3 * S23 - 0.432D3 * S24 - t34 - t32 - t33) * t7 + (-0.216D3 * S
     #23 - 0.216D3 * S24) * t9 - t105 + t107 - t44 + t52 + t46 - t109 - 
     #t61 - t54 + t111 - t66
      t114 = -S13 - S23
      t124 = 0.144D3 * t47
      t125 = 0.144D3 * t53
      t126 = 0.45D2 * t51
      t128 = 0.27D2 * t106
      t129 = 0.45D2 * t45
      t130 = 0.27D2 * t110
      t133 = 0.144D3 * t55
      t134 = 0.144D3 * t43
      t140 = 0.27D2 * t108
      t141 = 0.27D2 * t104
      t143 = ((t61 - t124 + t44 - t125 + t126 - 0.216D3 * t108 + t128 + 
     #t105 + t129 + t130) * t5 + (-t133 + t129 - t134 + t66 - 0.216D3 * 
     #t104 + t54 + t109 + t128 + t126 + t130) * t3) * t9 - t133 + t54 - 
     #t124 + t44 + t140 + t111 + t129 + t141 + t126 - 0.216D3 * t106
      t175 = -t27
      t176 = 0.9D1 / 0.8D1 * t175
      t179 = t176 * t7 + t176 * t9
      t182 = 0.18D2 * t5
      t183 = 0.18D2 * t3
      t187 = 0.18D2 * t27 * t9
      t192 = 0.27D2 / 0.8D1 * S24
      t193 = 0.27D2 / 0.8D1 * S14
      t194 = 0.9D1 / 0.2D1 * S13
      t197 = 0.9D1 / 0.2D1 * S14
      t198 = 0.27D2 / 0.8D1 * S23
      t199 = 0.27D2 / 0.8D1 * S13
      t204 = 0.9D1 / 0.2D1 * S23
      t207 = 0.9D1 / 0.2D1 * S24
      t225 = 0.36D2 * S14 - 0.27D2 * S23
      t229 = -0.27D2 * S24 + 0.36D2 * S13
      t235 = -0.27D2 * S13 + 0.36D2 * S24
      t239 = 0.36D2 * S23 - 0.27D2 * S14
      t245 = 0.3879D4 / 0.16D2 * S24
      t246 = 0.6849D4 / 0.16D2 * S23
      t247 = 0.6849D4 / 0.16D2 * S13
      t248 = 0.3879D4 / 0.16D2 * S14
      t251 = 0.6849D4 / 0.16D2 * S14
      t252 = 0.3879D4 / 0.16D2 * S13
      t253 = 0.3879D4 / 0.16D2 * S23
      t254 = 0.6849D4 / 0.16D2 * S24
      t261 = 0.9D1 * t110
      t262 = 0.27D2 * t45
      t263 = 0.27D2 / 0.2D1 * t43
      t264 = 0.27D2 / 0.4D1 * t108
      t265 = 0.27D2 / 0.8D1 * t53
      t266 = 0.63D2 / 0.8D1 * t47
      t269 = 0.27D2 / 0.4D1 * t104
      t270 = 0.27D2 / 0.8D1 * t43
      t271 = 0.27D2 / 0.2D1 * t53
      t272 = 0.27D2 * t51
      t273 = 0.63D2 / 0.8D1 * t55
      t280 = 0.9D1 * t106
      t281 = 0.27D2 / 0.8D1 * t47
      t282 = 0.63D2 / 0.8D1 * t53
      t283 = 0.27D2 / 0.2D1 * t55
      t286 = 0.27D2 / 0.8D1 * t55
      t287 = 0.27D2 / 0.2D1 * t47
      t288 = 0.63D2 / 0.8D1 * t43
      t297 = t79 * S34
      t316 = 0.396D3 * t45
      t321 = 0.396D3 * t51
      t336 = ((0.1971D4 / 0.2D1 + (t105 - t66 - t61 - t54 - t44 + t109 +
     # t272 - t130 - t128 + t262) * t5 * t3) * t9 + (-t61 + 0.324D3 * t4
     #3 + t272 + 0.117D3 * t47 - t316 - t54) * t5 + (-t44 + 0.324D3 * t5
     #3 + 0.117D3 * t55 - t321 - t66 + t262) * t3) * t7 + ((0.117D3 * t5
     #3 - t321 - t44 + t262 - t66 + 0.324D3 * t55) * t5 + (-t61 + t272 +
     # 0.324D3 * t47 - t54 - t316 + 0.117D3 * t43) * t3) * t9 - t61 - t1
     #40 + t111 + t262 - t141 - t54 - t44 + t272 + t107 - t66
      t338 = 0.441D3 / 0.4D1 * S24
      t339 = 0.441D3 / 0.4D1 * S14
      t340 = 0.441D3 / 0.4D1 * S13
      t341 = 0.441D3 / 0.4D1 * S23
      t342 = 0.6633D4 / 0.16D2 * t110
      t344 = 0.1809D4 / 0.8D1 * t47
      t345 = 0.5769D4 / 0.16D2 * t51
      t346 = 0.135D3 * t55
      t347 = 0.135D3 * t43
      t349 = 0.1809D4 / 0.8D1 * t53
      t350 = 0.6633D4 / 0.16D2 * t106
      t351 = 0.5769D4 / 0.16D2 * t45
      t354 = 0.1809D4 / 0.8D1 * t43
      t356 = 0.135D3 * t47
      t357 = 0.1809D4 / 0.8D1 * t55
      t358 = 0.135D3 * t53
      t364 = 0.72D2 * t104
      t366 = 0.819D3 / 0.16D2 * t55
      t367 = 0.72D2 * t108
      t368 = 0.819D3 / 0.16D2 * t47
      t369 = 0.171D3 / 0.8D1 * t43
      t370 = 0.171D3 / 0.8D1 * t53
      t371 = t43 * S24
      t372 = 0.243D3 / 0.2D1 * t371
      t373 = t53 * S14
      t374 = 0.9D1 / 0.8D1 * t373
      t375 = t43 * S13
      t376 = 0.18D2 * t375
      t377 = t47 * S24
      t378 = 0.621D3 / 0.8D1 * t377
      t379 = S13 * t53
      t381 = S13 * t47
      t382 = 0.333D3 / 0.2D1 * t381
      t383 = t53 * S24
      t385 = t43 * S14
      t387 = S14 * t47
      t389 = t110 * S24
      t390 = 0.27D2 * t389
      t393 = 0.18D2 * t373
      t394 = 0.9D1 / 0.8D1 * t375
      t395 = t55 * S23
      t396 = 0.621D3 / 0.8D1 * t395
      t398 = t53 * S23
      t399 = 0.243D3 / 0.2D1 * t398
      t400 = S14 * t55
      t401 = 0.333D3 / 0.2D1 * t400
      t402 = t43 * S23
      t404 = t110 * S23
      t405 = 0.27D2 * t404
      t407 = S13 * t55
      t411 = (t338 + t339 + t340 + t341 + (t342 - 0.2673D4 / 0.4D1 * t10
     #8 - t344 + t345 - t346 - t347 - 0.270D3 * t104 - t349 + t350 + t35
     #1) * t5 + (-t354 - 0.270D3 * t108 + t350 + t351 + t345 + t342 - t3
     #56 - t357 - t358 - 0.2673D4 / 0.4D1 * t104) * t3) * t9 + t261 - t3
     #64 + t52 + 0.1197D4 / 0.8D1 * t106 + t366 - t367 + t368 - t369 + t
     #46 - t370 + (t372 - t374 - t376 + t378 - 0.9D1 / 0.2D1 * t379 - t3
     #82 + 0.27D2 / 0.8D1 * t383 - 0.27D2 / 0.2D1 * t385 - 0.63D2 / 0.8D
     #1 * t387 + t390) * t5 + (-t393 - t394 + t396 - 0.9D1 / 0.2D1 * t38
     #5 + t399 - t401 + 0.27D2 / 0.8D1 * t402 + t405 - 0.27D2 / 0.2D1 * 
     #t379 - 0.63D2 / 0.8D1 * t407) * t3
      t414 = 0.171D3 / 0.8D1 * t55
      t415 = 0.819D3 / 0.16D2 * t53
      t416 = 0.171D3 / 0.8D1 * t47
      t417 = 0.819D3 / 0.16D2 * t43
      t418 = t55 * S24
      t420 = 0.243D3 / 0.2D1 * t400
      t421 = 0.621D3 / 0.8D1 * t373
      t422 = t47 * S23
      t424 = t51 * S24
      t425 = 0.27D2 * t424
      t427 = 0.9D1 / 0.8D1 * t377
      t428 = 0.333D3 / 0.2D1 * t398
      t430 = 0.18D2 * t395
      t433 = 0.333D3 / 0.2D1 * t371
      t434 = 0.243D3 / 0.2D1 * t381
      t438 = 0.9D1 / 0.8D1 * t395
      t439 = 0.18D2 * t377
      t440 = 0.621D3 / 0.8D1 * t375
      t441 = t45 * S23
      t442 = 0.27D2 * t441
      t446 = 0.1197D4 / 0.8D1 * t110 + t46 + t52 + t280 - t364 - t367 - 
     #t414 + t415 - t416 + t417 + (-0.27D2 / 0.2D1 * t418 + t420 + t421 
     #- 0.9D1 / 0.2D1 * t422 + t425 + 0.27D2 / 0.8D1 * t387 - t427 - t42
     #8 - 0.63D2 / 0.8D1 * t383 - t430) * t5 + (-t433 + t434 - 0.9D1 / 0
     #.2D1 * t418 - 0.27D2 / 0.2D1 * t422 + 0.27D2 / 0.8D1 * t407 - t438
     # - t439 + t440 + t442 - 0.63D2 / 0.8D1 * t402) * t3
      t450 = t79 ** 2
      t475 = 0.9D1 * t104
      t476 = 0.27D2 / 0.4D1 * t106
      t479 = 0.9D1 * t108
      t484 = 0.27D2 / 0.4D1 * t110
      t493 = 0.72D2 * t110
      t495 = 0.72D2 * t106
      t503 = 0.6633D4 / 0.16D2 * t104
      t504 = 0.6633D4 / 0.16D2 * t108
      t519 = (t338 + t339 + t340 + t341 + (-t493 + t368 + t52 - t414 + t
     #415 + 0.1197D4 / 0.8D1 * t108 - t369 + t475 + t46 - t495) * t5 + (
     #t366 + t417 - t495 + t46 + t52 - t493 - t416 + 0.1197D4 / 0.8D1 * 
     #t104 - t370 + t479) * t3) * t9 + t503 + t504 + t345 - 0.2673D4 / 0
     #.4D1 * t106 + t351 - 0.270D3 * t110 - t357 - t358 - t344 - t347 + 
     #(0.27D2 / 0.8D1 * t418 - t376 - t438 - 0.63D2 / 0.8D1 * t422 + t37
     #2 - t382 + t378 + t442 - 0.27D2 / 0.2D1 * t402 - 0.9D1 / 0.2D1 * t
     #407) * t5 + (-0.63D2 / 0.8D1 * t418 + 0.27D2 / 0.8D1 * t422 - 0.27
     #D2 / 0.2D1 * t383 - t393 + t396 - t427 + t425 - t401 + t399 - 0.9D
     #1 / 0.2D1 * t387) * t3
      t535 = t504 - 0.270D3 * t106 + t503 + t351 + t345 - 0.2673D4 / 0.4
     #D1 * t110 - t346 - t349 - t356 - t354 + (-0.27D2 / 0.2D1 * t407 - 
     #t428 + t420 - 0.63D2 / 0.8D1 * t379 + t405 + t421 + 0.27D2 / 0.8D1
     # * t385 - t430 - 0.9D1 / 0.2D1 * t402 - t394) * t5 + (-t433 + t434
     # + t390 - 0.9D1 / 0.2D1 * t383 - t374 + t440 - 0.63D2 / 0.8D1 * t3
     #85 + 0.27D2 / 0.8D1 * t379 - 0.27D2 / 0.2D1 * t387 - t439) * t3
      t549 = 0.18D2 * t400
      t551 = 0.126D3 * t373
      t552 = 0.126D3 * t377
      t553 = 0.36D2 * t398
      t554 = 0.18D2 * t371
      t555 = 0.36D2 * t381
      t556 = 0.36D2 * t441
      t557 = 0.36D2 * t404
      t561 = t549 + 0.126D3 * t422 + t551 + t552 - t553 + t554 - t555 - 
     #t556 - t557 + 0.18D2 * t407 + 0.126D3 * t379 + 0.18D2 * t402
      t563 = 0.36D2 * t371
      t564 = 0.18D2 * t381
      t567 = 0.36D2 * t389
      t569 = 0.126D3 * t375
      t570 = 0.126D3 * t395
      t571 = 0.36D2 * t424
      t572 = 0.36D2 * t400
      t573 = 0.18D2 * t398
      t575 = -t563 + t564 + 0.126D3 * t418 + 0.126D3 * t385 - t567 + 0.1
     #8D2 * t383 + t569 + t570 - t571 - t572 + t573 + 0.18D2 * t387
      t577 = 0.657D3 * t51 + 0.342D3 * t108 + 0.657D3 * t45 + 0.342D3 * 
     #t106 + 0.342D3 * t104 + 0.342D3 * t110 - 0.27D2 * t55 - 0.27D2 * t
     #53 - 0.27D2 * t47 - 0.27D2 * t43 + t561 * t5 + t575 * t3
      t583 = t577 * t9 - t572 + t554 - t567 - t557 + 0.18D2 * t379 + t55
     #2 + t570 - t555 + t573 + 0.126D3 * t387 + 0.126D3 * t407 + 0.18D2 
     #* t385
      t589 = 0.18D2 * t422 - t571 - t553 + t551 + t569 - t556 + t549 - t
     #563 + t564 + 0.18D2 * t418 + 0.126D3 * t402 + 0.126D3 * t383
      t593 = (-0.72D2 * t1 - 0.72D2 * t6 * t7 * S34 * t9 * t12) * t17 * 
     #s * t20 * z + ((((-0.216D3 * t23 - 0.216D3 * t5 - 0.216D3 * t3) * 
     #t7 - 0.216D3 + t28 * t9) * S34 + t32 + t33 + t34 + 0.216D3 * S23 +
     # t36) * t1 + (-t28 * t9 * t41 + ((t44 - t46 + 0.126D3 * t47) * t5 
     #+ (-t52 + t54 + 0.126D3 * t55) * t3) * t7 + ((t61 - t52 + 0.126D3 
     #* t53) * t5 + (-t46 + 0.126D3 * t43 + t66) * t3) * t9) * t12) * t1
     #7 * t20 + (-0.216D3 * t6 * t41 * t9 + t112 * t1 + ((-0.432D3 + 0.2
     #16D3 * t114 * t5 + (-t33 - t36 + (t105 - t54 - t111 + t46 + t52 - 
     #t44 - t107 + t109 - t61 - t66) * t5) * t3) * t9 * t41 + t143 * t7 
     #+ (t141 - t125 + t140 + t107 + t66 + t126 + t129 - 0.216D3 * t110 
     #- t134 + t61) * t9) * t12 + ((-0.9D1 * t47 - 0.9D1 * t53 + 0.18D2 
     #* t108) * t5 * t2 + 0.18D2 * t106 - 0.18D2 * t45 + 0.18D2 * t110 -
     # 0.18D2 * t51 + (0.18D2 * t104 - 0.9D1 * t55 - 0.9D1 * t43) * t4 *
     # t3) * t9 * t7 / t11 / S12) * s * z + t179 * t11 + (((0.117D3 * t2
     #3 - t182 - t183) * t7 + 0.324D3 + t187) * S34 + (0.603D3 / 0.16D2 
     #* t175 * t9 - 0.585D3 / 0.4D1 + (t192 - t193 - t194) * t5 + (-t197
     # + t198 - t199) * t3) * t7 + (-0.585D3 / 0.4D1 + (-t204 - t192 + t
     #193) * t5 + (-t207 - t198 + t199) * t3) * t9) * S12 + ((-0.396D3 *
     # t23 + 0.27D2 * t5 + 0.27D2 * t3) * t7 - 0.396D3 + 0.27D2 * t175 *
     # t9) * t79 + ((t225 * t5 + t229 * t3) * t7 + (t235 * t5 + t239 * t
     #3) * t9) * S34 + ((-0.927D3 / 0.8D1 + (-t245 + t246 + t247 - t248)
     # * t5 + (t251 - t252 - t253 + t254) * t3) * t9 - 0.99D2 * S13 - 0.
     #99D2 * S14 + (t261 - t262 + t263 - t264 + t265 + t266) * t5 + (-t2
     #69 + t270 + t271 - t272 + t273 + t261) * t3) * t7 + (-0.99D2 * S23
     # - 0.99D2 * S24 + (-t272 - t264 + t280 + t281 + t282 + t283) * t5 
     #+ (t280 - t269 - t262 + t286 + t287 + t288) * t3) * t9 + (((0.324D
     #3 * t23 - t182 - t183) * t7 + 0.117D3 + t187) * t297 + ((t239 * t5
     # + t235 * t3) * t7 + (t229 * t5 + t225 * t3) * t9) * t79 + t336 * 
     #S34 + t411 * t7 + t446 * t9) * t1 + (t179 * t450 + ((0.585D3 / 0.4
     #D1 * t27 * t9 + 0.603D3 / 0.16D2 + (t192 - t194 - t198) * t5 + (-t
     #197 - t192 + t198) * t3) * t7 + (0.603D3 / 0.16D2 + (t193 - t204 -
     # t199) * t5 + (-t207 + t199 - t193) * t3) * t9) * t297 + (((-0.927
     #D3 / 0.8D1 + 0.99D2 * t114 * t5 + (-0.99D2 * S14 - 0.99D2 * S24) *
     # t3) * t9 - t245 + t251 + t247 - t253 + (-t262 + t263 + t475 + t26
     #6 + t286 - t476) * t5 + (t479 - t272 + t273 + t271 + t281 - t476) 
     #* t3) * t7 + (t254 + t246 - t248 - t252 + (-t272 + t475 + t270 + t
     #283 + t282 - t484) * t5 + (t287 + t479 - t262 + t288 + t265 - t484
     #) * t3) * t9) * t79 + (t519 * t7 + t535 * t9) * S34 + t583 * t7 + 
     #t589 * t9) * t12
      rrgg2gghhard11J5 = t593 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard11J6
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
      t2 = S12 + S13 + S23
      t3 = 0.1D1 / t2
      t4 = S12 + S14 + S24
      t5 = 0.1D1 / t4
      t6 = t3 * t5
      t7 = S13 + S14 + S34
      t9 = S23 + S24 + S34
      t11 = S12 ** 2
      t12 = 0.1D1 / t11
      t17 = s ** 2
      t20 = z ** 2
      t23 = t4 ** 2
      t24 = 0.1D1 / t23
      t27 = t2 ** 2
      t28 = 0.1D1 / t27
      t36 = t5 + t3
      t37 = 0.1080D4 * t36
      t41 = 0.1080D4 * S13
      t42 = 0.1080D4 * S14
      t43 = 0.2160D4 * S34
      t45 = 0.1080D4 * S24
      t50 = t7 * S34
      t52 = S13 * S24
      t53 = 0.180D3 * t52
      t54 = S24 ** 2
      t56 = S13 ** 2
      t57 = 0.90D2 * t56
      t60 = S14 * S23
      t61 = 0.180D3 * t60
      t62 = S23 ** 2
      t64 = S14 ** 2
      t65 = 0.90D2 * t64
      t71 = 0.90D2 * t62
      t75 = 0.90D2 * t54
      t82 = S14 * S24
      t83 = 0.54D2 * t82
      t88 = S23 * S24
      t89 = S13 * S14
      t96 = S13 * S23
      t97 = 0.54D2 * t96
      t103 = 0.1D1 / t11 / S12
      t112 = S34 ** 2
      t114 = -t36
      t138 = 0.180D3 * t96
      t139 = 0.180D3 * t88
      t140 = 0.180D3 * t82
      t141 = 0.180D3 * t89
      t142 = 0.1080D4 * t112 + ((0.14373D5 / 0.4D1 * t114 * t9 - 0.14373
     #D5 / 0.4D1 + 0.1080D4 * S24 * t5 + 0.1080D4 * S23 * t3) * t7 + (-0
     #.14373D5 / 0.4D1 + 0.1080D4 * S14 * t5 + 0.1080D4 * S13 * t3) * t9
     #) * S34 + (0.2160D4 * S23 + 0.2160D4 * S24 + t43 + t42 + t41) * t7
     # + (0.1080D4 * S23 + 0.1080D4 * S24) * t9 + t138 - t139 + t57 - t6
     #1 - t53 + t140 + t71 + t65 - t141 + t75
      t144 = S13 + S23
      t154 = 0.81D2 * t54
      t155 = 0.81D2 * t64
      t156 = 0.171D3 * t60
      t157 = 0.189D3 * t88
      t158 = 0.171D3 * t52
      t159 = 0.189D3 * t89
      t162 = 0.81D2 * t62
      t163 = 0.81D2 * t56
      t168 = 0.189D3 * t82
      t169 = 0.189D3 * t96
      t171 = ((-t71 - t154 - t57 - t155 - t156 - t83 - t157 - t138 - t15
     #8 - t159) * t5 + (-t162 - t158 - t163 - t75 - t97 - t65 - t140 - t
     #157 - t156 - t159) * t3) * t9 - t162 - t65 - t154 - t57 - t168 - t
     #141 - t158 - t169 - t156 - 0.54D2 * t88
      t201 = 0.45D2 / 0.8D1 * t114
      t204 = t201 * t7 + t201 * t9
      t206 = t6 * t9
      t208 = 0.90D2 * t5
      t209 = 0.90D2 * t3
      t213 = 0.90D2 * t36 * t9
      t218 = 0.135D3 / 0.8D1 * S24
      t219 = 0.135D3 / 0.8D1 * S14
      t220 = 0.45D2 / 0.2D1 * S13
      t223 = 0.45D2 / 0.2D1 * S14
      t224 = 0.135D3 / 0.8D1 * S23
      t225 = 0.135D3 / 0.8D1 * S13
      t230 = 0.45D2 / 0.2D1 * S23
      t233 = 0.45D2 / 0.2D1 * S24
      t251 = -0.180D3 * S14 + 0.135D3 * S23
      t255 = 0.135D3 * S24 - 0.180D3 * S13
      t261 = 0.135D3 * S13 - 0.180D3 * S24
      t265 = -0.180D3 * S23 + 0.135D3 * S14
      t271 = 0.279D3 / 0.4D1 * S24
      t272 = 0.1431D4 / 0.8D1 * S23
      t273 = 0.1431D4 / 0.8D1 * S13
      t274 = 0.279D3 / 0.4D1 * S14
      t277 = 0.1431D4 / 0.8D1 * S14
      t278 = 0.279D3 / 0.4D1 * S13
      t279 = 0.279D3 / 0.4D1 * S23
      t280 = 0.1431D4 / 0.8D1 * S24
      t287 = 0.135D3 / 0.4D1 * t82
      t288 = 0.315D3 / 0.8D1 * t54
      t289 = 0.135D3 / 0.2D1 * t56
      t290 = 0.135D3 / 0.8D1 * t64
      t291 = 0.45D2 * t89
      t292 = 0.135D3 * t52
      t295 = 0.135D3 / 0.8D1 * t56
      t296 = 0.135D3 / 0.4D1 * t96
      t297 = 0.135D3 * t60
      t298 = 0.135D3 / 0.2D1 * t64
      t299 = 0.315D3 / 0.8D1 * t62
      t306 = 0.315D3 / 0.8D1 * t64
      t307 = 0.45D2 * t88
      t308 = 0.135D3 / 0.8D1 * t54
      t309 = 0.135D3 / 0.2D1 * t62
      t312 = 0.135D3 / 0.2D1 * t54
      t313 = 0.135D3 / 0.8D1 * t62
      t314 = 0.315D3 / 0.8D1 * t56
      t323 = t112 * S34
      t344 = 0.432D3 * t52
      t349 = 0.432D3 * t60
      t366 = ((-0.9423D4 / 0.2D1 + (t71 - t292 + 0.135D3 * t89 + 0.135D3
     # * t88 - t140 - t138 + t65 + t57 + t75 - t297) * t5 * t3) * t9 + (
     #t71 - 0.414D3 * t56 - t297 + 0.621D3 * t54 - t344 + t65) * t5 + (t
     #57 - 0.414D3 * t64 + 0.621D3 * t62 - t349 + t75 - t292) * t3) * t7
     # + ((0.621D3 * t64 - t349 + t57 - t292 + t75 - 0.414D3 * t62) * t5
     # + (t71 - t297 - 0.414D3 * t54 + t65 - t344 + 0.621D3 * t56) * t3)
     # * t9 + t71 + 0.135D3 * t82 - t141 - t292 + 0.135D3 * t96 + t65 + 
     #t57 - t297 - t139 + t75
      t368 = 0.25137D5 / 0.16D2 * S24
      t369 = 0.25137D5 / 0.16D2 * S14
      t370 = 0.25137D5 / 0.16D2 * S13
      t371 = 0.25137D5 / 0.16D2 * S23
      t372 = 0.477D3 / 0.8D1 * t89
      t374 = 0.63D2 / 0.2D1 * t54
      t375 = 0.99D2 / 0.8D1 * t60
      t376 = 0.675D3 / 0.2D1 * t62
      t377 = 0.675D3 / 0.2D1 * t56
      t379 = 0.63D2 / 0.2D1 * t64
      t380 = 0.477D3 / 0.8D1 * t88
      t381 = 0.99D2 / 0.8D1 * t52
      t384 = 0.63D2 / 0.2D1 * t56
      t386 = 0.675D3 / 0.2D1 * t54
      t387 = 0.63D2 / 0.2D1 * t62
      t388 = 0.675D3 / 0.2D1 * t64
      t394 = 0.9D1 * t60
      t396 = 0.1377D4 / 0.16D2 * t62
      t397 = 0.1377D4 / 0.16D2 * t54
      t398 = 0.855D3 / 0.8D1 * t56
      t399 = 0.9D1 * t52
      t400 = 0.855D3 / 0.8D1 * t64
      t401 = t56 * S24
      t402 = 0.531D3 / 0.2D1 * t401
      t403 = t64 * S14
      t404 = 0.45D2 / 0.8D1 * t403
      t405 = t56 * S13
      t406 = 0.90D2 * t405
      t407 = t54 * S24
      t408 = 0.369D3 / 0.8D1 * t407
      t409 = S13 * t64
      t411 = S13 * t54
      t412 = 0.297D3 / 0.2D1 * t411
      t413 = t64 * S24
      t415 = t56 * S14
      t417 = S14 * t54
      t419 = t89 * S24
      t420 = 0.135D3 * t419
      t423 = 0.90D2 * t403
      t424 = 0.45D2 / 0.8D1 * t405
      t425 = t62 * S23
      t426 = 0.369D3 / 0.8D1 * t425
      t428 = t64 * S23
      t429 = 0.531D3 / 0.2D1 * t428
      t430 = S14 * t62
      t431 = 0.297D3 / 0.2D1 * t430
      t432 = t56 * S23
      t434 = t89 * S23
      t435 = 0.135D3 * t434
      t437 = S13 * t62
      t441 = (-t368 - t369 - t370 - t371 + (t372 - 0.351D3 * t82 - t374 
     #- t375 - t376 - t377 - 0.675D3 * t96 - t379 + t380 - t381) * t5 + 
     #(-t384 - 0.675D3 * t82 + t380 - t381 - t375 + t372 - t386 - t387 -
     # t388 - 0.351D3 * t96) * t3) * t9 - t291 + t169 - t394 - 0.513D3 /
     # 0.8D1 * t88 + t396 + t168 + t397 + t398 - t399 + t400 + (-t402 + 
     #t404 + t406 - t408 + 0.45D2 / 0.2D1 * t409 + t412 - 0.135D3 / 0.8D
     #1 * t413 + 0.135D3 / 0.2D1 * t415 + 0.315D3 / 0.8D1 * t417 - t420)
     # * t5 + (t423 + t424 - t426 + 0.45D2 / 0.2D1 * t415 - t429 + t431 
     #- 0.135D3 / 0.8D1 * t432 - t435 + 0.135D3 / 0.2D1 * t409 + 0.315D3
     # / 0.8D1 * t437) * t3
      t444 = 0.855D3 / 0.8D1 * t62
      t445 = 0.1377D4 / 0.16D2 * t64
      t446 = 0.855D3 / 0.8D1 * t54
      t447 = 0.1377D4 / 0.16D2 * t56
      t448 = t62 * S24
      t450 = 0.531D3 / 0.2D1 * t430
      t451 = 0.369D3 / 0.8D1 * t403
      t452 = t54 * S23
      t454 = t60 * S24
      t455 = 0.135D3 * t454
      t457 = 0.45D2 / 0.8D1 * t407
      t458 = 0.297D3 / 0.2D1 * t428
      t460 = 0.90D2 * t425
      t463 = 0.297D3 / 0.2D1 * t401
      t464 = 0.531D3 / 0.2D1 * t411
      t468 = 0.45D2 / 0.8D1 * t425
      t469 = 0.90D2 * t407
      t470 = 0.369D3 / 0.8D1 * t405
      t471 = t52 * S23
      t472 = 0.135D3 * t471
      t476 = -0.513D3 / 0.8D1 * t89 - t399 - t394 - t307 + t169 + t168 +
     # t444 + t445 + t446 + t447 + (0.135D3 / 0.2D1 * t448 - t450 - t451
     # + 0.45D2 / 0.2D1 * t452 - t455 - 0.135D3 / 0.8D1 * t417 + t457 + 
     #t458 + 0.315D3 / 0.8D1 * t413 + t460) * t5 + (t463 - t464 + 0.45D2
     # / 0.2D1 * t448 + 0.135D3 / 0.2D1 * t452 - 0.135D3 / 0.8D1 * t437 
     #+ t468 + t469 - t470 - t472 + 0.315D3 / 0.8D1 * t432) * t3
      t480 = t112 ** 2
      t505 = 0.135D3 / 0.4D1 * t88
      t506 = 0.45D2 * t96
      t509 = 0.45D2 * t82
      t514 = 0.135D3 / 0.4D1 * t89
      t531 = 0.477D3 / 0.8D1 * t96
      t532 = 0.477D3 / 0.8D1 * t82
      t547 = (-t368 - t369 - t370 - t371 + (t159 + t397 - t394 + t444 + 
     #t445 - 0.513D3 / 0.8D1 * t82 + t398 - t506 - t399 + t157) * t5 + (
     #t396 + t447 + t157 - t399 - t394 + t159 + t446 - 0.513D3 / 0.8D1 *
     # t96 + t400 - t509) * t3) * t9 + t531 + t532 - t375 - 0.351D3 * t8
     #8 - t381 - 0.675D3 * t89 - t387 - t388 - t374 - t377 + (-0.135D3 /
     # 0.8D1 * t448 + t406 + t468 + 0.315D3 / 0.8D1 * t452 - t402 + t412
     # - t408 - t472 + 0.135D3 / 0.2D1 * t432 + 0.45D2 / 0.2D1 * t437) *
     # t5 + (0.315D3 / 0.8D1 * t448 - 0.135D3 / 0.8D1 * t452 + 0.135D3 /
     # 0.2D1 * t413 + t423 - t426 + t457 - t455 + t431 - t429 + 0.45D2 /
     # 0.2D1 * t417) * t3
      t563 = t532 - 0.675D3 * t88 + t531 - t381 - t375 - 0.351D3 * t89 -
     # t376 - t379 - t386 - t384 + (0.135D3 / 0.2D1 * t437 + t458 - t450
     # + 0.315D3 / 0.8D1 * t409 - t435 - t451 - 0.135D3 / 0.8D1 * t415 +
     # t460 + 0.45D2 / 0.2D1 * t432 + t424) * t5 + (t463 - t464 - t420 +
     # 0.45D2 / 0.2D1 * t413 + t404 - t470 + 0.315D3 / 0.8D1 * t415 - 0.
     #135D3 / 0.8D1 * t409 + 0.135D3 / 0.2D1 * t417 + t469) * t3
      t577 = 0.90D2 * t430
      t579 = 0.54D2 * t403
      t580 = 0.54D2 * t407
      t581 = 0.180D3 * t428
      t582 = 0.90D2 * t401
      t583 = 0.180D3 * t411
      t584 = 0.180D3 * t471
      t585 = 0.180D3 * t434
      t589 = -t577 + 0.54D2 * t452 + t579 + t580 + t581 - t582 + t583 + 
     #t584 + t585 - 0.90D2 * t437 + 0.54D2 * t409 - 0.90D2 * t432
      t591 = 0.180D3 * t401
      t592 = 0.90D2 * t411
      t595 = 0.180D3 * t419
      t597 = 0.54D2 * t405
      t598 = 0.54D2 * t425
      t599 = 0.180D3 * t454
      t600 = 0.180D3 * t430
      t601 = 0.90D2 * t428
      t603 = t591 - t592 + 0.54D2 * t448 + 0.54D2 * t415 + t595 - 0.90D2
     # * t413 + t597 + t598 + t599 + t600 - t601 - 0.90D2 * t417
      t605 = 0.117D3 * t60 + 0.324D3 * t82 + 0.117D3 * t52 + 0.324D3 * t
     #88 + 0.324D3 * t96 + 0.324D3 * t89 + 0.468D3 * t62 + 0.468D3 * t64
     # + 0.468D3 * t54 + 0.468D3 * t56 + t589 * t5 + t603 * t3
      t611 = t605 * t9 + t600 - t582 + t595 + t585 - 0.90D2 * t409 + t58
     #0 + t598 + t583 - t601 + 0.54D2 * t417 + 0.54D2 * t437 - 0.90D2 * 
     #t415
      t617 = -0.90D2 * t452 + t599 + t581 + t579 + t597 + t584 - t577 + 
     #t591 - t592 - 0.90D2 * t448 + 0.54D2 * t432 + 0.54D2 * t413
      t621 = (0.360D3 * t1 + 0.360D3 * t6 * t7 * S34 * t9 * t12) * t17 *
     # s * t20 * z + (((((-0.243D3 * t24 + 0.1080D4 * t6 - 0.243D3 * t28
     #) * t9 + 0.1080D4 * t5 + 0.1080D4 * t3) * t7 + 0.1080D4 + t37 * t9
     #) * S34 - t41 - t42 - t43 - 0.1080D4 * S23 - t45) * t1 + (-t37 * t
     #9 * t50 + ((t53 + 0.54D2 * t54 - t57) * t5 + (t61 + 0.54D2 * t62 -
     # t65) * t3) * t7 + ((0.54D2 * t64 + t61 - t71) * t5 + (0.54D2 * t5
     #6 - t75 + t53) * t3) * t9) * t12 + ((t83 - 0.27D2 * t54 - 0.27D2 *
     # t64) * t24 + (0.54D2 * t88 - 0.54D2 * t52 + 0.54D2 * t89 - 0.54D2
     # * t60) * t5 * t3 + (-0.27D2 * t56 - 0.27D2 * t62 + t97) * t28) * 
     #t9 * t50 * t103) * t17 * t20 + (0.1080D4 * t6 * t50 * t9 + t142 * 
     #t1 + ((0.2160D4 + 0.1080D4 * t144 * t5 + (t45 + t42 + (-t53 + t65 
     #+ t141 + t139 - t140 - t61 + t71 + t57 + t75 - t138) * t5) * t3) *
     # t9 * t50 + t171 * t7 + (-t169 - t155 - t168 - t139 - t75 - t156 -
     # t158 - 0.54D2 * t89 - t163 - t71) * t9) * t12 + ((-0.9D1 * t54 - 
     #0.9D1 * t64 + 0.18D2 * t82) * t5 * t2 + 0.18D2 * t88 - 0.18D2 * t5
     #2 + 0.18D2 * t89 - 0.18D2 * t60 + (0.18D2 * t96 - 0.9D1 * t62 - 0.
     #9D1 * t56) * t4 * t3) * t9 * t7 * t103) * s * z + t204 * t11 + (((
     #0.621D3 * t206 + t208 + t209) * t7 - 0.414D3 + t213) * S34 + (0.13
     #5D3 / 0.4D1 * t36 * t9 + 0.2925D4 / 0.4D1 + (-t218 + t219 + t220) 
     #* t5 + (t223 - t224 + t225) * t3) * t7 + (0.2925D4 / 0.4D1 + (t230
     # + t218 - t219) * t5 + (t233 + t224 - t225) * t3) * t9) * S12 + ((
     #-0.432D3 * t206 - 0.135D3 * t5 - 0.135D3 * t3) * t7 - 0.432D3 + 0.
     #135D3 * t114 * t9) * t112 + ((t251 * t5 + t255 * t3) * t7 + (t261 
     #* t5 + t265 * t3) * t9) * S34 + ((0.135D3 + (-t271 - t272 - t273 -
     # t274) * t5 + (-t277 - t278 - t279 - t280) * t3) * t9 + 0.495D3 * 
     #S13 + 0.495D3 * S14 + (t287 - t288 - t289 - t290 - t291 + t292) * 
     #t5 + (-t295 - t291 + t296 + t297 - t298 - t299) * t3) * t7 + (0.49
     #5D3 * S24 + 0.495D3 * S23 + (-t306 + t287 + t297 - t307 - t308 - t
     #309) * t5 + (-t312 + t292 - t313 + t296 - t314 - t307) * t3) * t9 
     #+ (((-0.414D3 * t206 + t208 + t209) * t7 + 0.621D3 + t213) * t323 
     #+ ((t265 * t5 + t261 * t3) * t7 + (t255 * t5 + t251 * t3) * t9) * 
     #t112 + t366 * S34 + t441 * t7 + t476 * t9) * t1 + (t204 * t480 + (
     #(0.2925D4 / 0.4D1 * t36 * t9 + 0.135D3 / 0.4D1 + (t220 + t224 - t2
     #18) * t5 + (-t224 + t223 + t218) * t3) * t7 + (0.135D3 / 0.4D1 + (
     #t230 - t219 + t225) * t5 + (-t225 + t219 + t233) * t3) * t9) * t32
     #3 + (((0.135D3 + 0.495D3 * t144 * t5 + (0.495D3 * S14 + 0.495D3 * 
     #S24) * t3) * t9 - t271 - t277 - t273 - t279 + (-t288 + t505 - t506
     # - t289 + t292 - t313) * t5 + (-t298 - t509 + t297 - t299 + t505 -
     # t308) * t3) * t7 + (-t280 - t272 - t274 - t278 + (-t306 + t297 - 
     #t309 - t506 - t295 + t514) * t5 + (-t312 - t290 - t314 + t292 + t5
     #14 - t509) * t3) * t9) * t112 + (t547 * t7 + t563 * t9) * S34 + t6
     #11 * t7 + t617 * t9) * t12
      rrgg2gghhard11J6 = t621 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2gghhard11J7
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
      t1 = S12 + S14 + S24
      t2 = t1 ** 2
      t3 = 0.1D1 / t2
      t4 = S12 + S13 + S23
      t5 = t4 ** 2
      t6 = 0.1D1 / t5
      t9 = S23 + S24 + S34
      t11 = S13 + S14 + S34
      t12 = t11 * S34
      t13 = 0.1D1 / S12
      t16 = 0.1D1 / t1
      t17 = S24 ** 2
      t19 = S23 ** 2
      t20 = 0.1D1 / t4
      t25 = S14 ** 2
      t27 = S13 ** 2
      t33 = S12 ** 2
      t34 = 0.1D1 / t33
      t36 = S14 * S24
      t37 = 0.45D2 * t36
      t42 = S13 * S14
      t43 = S13 * S24
      t44 = S14 * S23
      t45 = S23 * S24
      t52 = S13 * S23
      t53 = 0.45D2 * t52
      t59 = 0.1D1 / t33 / S12
      t63 = s ** 2
      t65 = z ** 2
      t67 = t16 + t20
      t78 = 0.270D3 * t17
      t79 = 0.270D3 * t25
      t80 = 0.45D2 * t44
      t81 = 0.45D2 * t43
      t82 = 0.45D2 * t42
      t83 = 0.45D2 * t45
      t84 = 0.180D3 * t36
      t87 = 0.270D3 * t19
      t88 = 0.180D3 * t52
      t89 = 0.270D3 * t27
      t94 = 0.180D3 * t45
      t97 = 0.180D3 * t42
      t110 = 0.90D2 * t43
      t111 = 0.90D2 * t44
      t128 = 0.1D1 + t16 * t20 * t9 * t11
      t129 = 0.495D3 * t128
      t138 = S34 ** 2
      t140 = 0.135D3 * S23
      t142 = 0.45D2 * S14
      t143 = 0.135D3 * S13
      t146 = 0.135D3 * S14
      t147 = 0.45D2 * S13
      t149 = 0.135D3 * S24
      t155 = t138 * S34
      t163 = 0.495D3 * t17 + 0.495D3 * t27 - 0.990D3 * t43
      t168 = 0.495D3 * t25 + 0.495D3 * t19 - 0.990D3 * t44
      t178 = 0.180D3 * t25
      t180 = 0.180D3 * t17
      t183 = 0.180D3 * t27
      t185 = 0.180D3 * t19
      t190 = 0.90D2 * t19
      t191 = 0.90D2 * t17
      t192 = S13 * t17
      t194 = t17 * S24
      t196 = t27 * S24
      t199 = (-0.180D3 * t192 + 0.90D2 * t194 + 0.90D2 * t196) * t16
      t200 = S14 * t19
      t202 = t25 * S23
      t204 = t19 * S23
      t207 = (-0.180D3 * t200 + 0.90D2 * t202 + 0.90D2 * t204) * t20
      t210 = 0.90D2 * t25
      t211 = 0.90D2 * t27
      t213 = t25 * S14
      t217 = (0.90D2 * t200 + 0.90D2 * t213 - 0.180D3 * t202) * t16
      t218 = t27 * S13
      t223 = (0.90D2 * t218 + 0.90D2 * t192 - 0.180D3 * t196) * t20
      t230 = 0.450D3 * S34
      t269 = 0.540D3 * t44 + t97 + 0.540D3 * t43 + t94 + t84 + t88 - t21
     #1 - t191 - t190 - t210 + (0.180D3 * t213 + 0.180D3 * t194 + 0.180D
     #3 * t17 * S23 + 0.180D3 * S13 * t25) * t16 + (0.180D3 * t19 * S24 
     #+ 0.180D3 * t218 + 0.180D3 * t204 + 0.180D3 * t27 * S14) * t20
      rrgg2gghhard11J7 = (((0.405D3 / 0.2D1 * t3 + 0.405D3 / 0.2D1 * t6)
     # * t9 * t12 * t13 + ((0.180D3 * t16 * t17 + 0.180D3 * t19 * t20) *
     # t11 + (0.180D3 * t16 * t25 + 0.180D3 * t27 * t20) * t9) * t34 + (
     #(-t37 + 0.45D2 / 0.2D1 * t25 + 0.45D2 / 0.2D1 * t17) * t3 + (-0.45
     #D2 * t42 + 0.45D2 * t43 + 0.45D2 * t44 - 0.45D2 * t45) * t16 * t20
     # + (0.45D2 / 0.2D1 * t27 + 0.45D2 / 0.2D1 * t19 - t53) * t6) * t9 
     #* t12 * t59) * t63 * t65 + (((0.810D3 + 0.810D3 * t67 * t9) * t11 
     #+ 0.810D3 * S23 + 0.810D3 * S24 + 0.810D3 * S34) * S34 * t13 + (((
     #(-t78 - t79 - t80 - t81 + t82 + t83 - t84) * t16 + (-t87 - t88 - t
     #80 - t89 + t83 - t81 + t82) * t20) * t9 - t87 - t78 + t37 - t80 + 
     #t53 - t94 - t81) * t11 + (-t79 - t89 - t81 - t80 + t37 - t97 + t53
     #) * t9) * t34 + ((0.45D2 * t17 - 0.90D2 * t36 + 0.45D2 * t25) * t1
     #6 * t4 - 0.90D2 * t45 - 0.90D2 * t42 + t110 + t111 + (0.45D2 * t27
     # + 0.45D2 * t19 - 0.90D2 * t52) * t1 * t20) * t9 * t11 * t59) * s 
     #* z + (t129 * S34 + 0.225D3 * t67 * t9 * t11) * S12 - 0.990D3 * t1
     #28 * t138 + (-0.450D3 + (t140 - 0.45D2 * S24 - t142 + t143) * t16 
     #+ (t146 - t147 - 0.45D2 * S23 + t149) * t20) * t9 * t11 + (t129 * 
     #t155 + ((-0.2430D4 * S23 - 0.2430D4 * S24 - 0.2430D4 * S34 + t163 
     #* t16 + t168 * t20) * t11 + (t168 * t16 + t163 * t20) * t9) * S34 
     #+ ((t146 + t149 + t143 + t140 + (-t178 - 0.720D3 * t36 + t110 + t9
     #7 + t94 + t111 - t180) * t16 + (-t183 + t110 + t94 - 0.720D3 * t52
     # + t111 + t97 - t185) * t20) * t9 + t81 + t190 + t191 + t94 - t53 
     #+ t80 - t37 + t199 + t207) * t11 + (t210 - t53 + t97 + t211 - t37 
     #+ t81 + t80 + t217 + t223) * t9) * t13 + ((0.225D3 * S13 + 0.225D3
     # * S14 + t230 + 0.225D3 * S23 + 0.225D3 * S24) * t155 + ((-0.495D3
     # * S23 - 0.495D3 * S24 - t230 + t146 + t143) * t11 + (t149 - t142 
     #- t147 + t140) * t9) * t138 + (((t146 + t149 + t143 + t140 + (t210
     # + t84 + t81 - t82 - t83 + t80 + t191) * t16 + (t211 + t81 - t83 +
     # t88 + t80 - t82 + t190) * t20) * t9 + t110 - t185 - t180 - 0.720D
     #3 * t45 + t88 + t111 + t84 + t199 + t207) * t11 + (-t178 + t88 - 0
     #.720D3 * t42 - t183 + t84 + t110 + t111 + t217 + t223) * t9) * S34
     # + (t269 * t9 + 0.180D3 * t194 + 0.180D3 * t204 + 0.180D3 * S14 * 
     #t17 + 0.180D3 * S13 * t19) * t11 + (0.180D3 * t213 + 0.180D3 * t21
     #8 + 0.180D3 * t27 * S23 + 0.180D3 * t25 * S24) * t9) * t34) / pi *
     # wd / z

      end function
  
   
      subroutine rrgg2gghsoftt1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghsoftt1s1e1  
      doubleprecision rrgg2gghsoftt1s1e0  
      doubleprecision rrgg2gghsoftt1s1em1  
      doubleprecision rrgg2gghsoftt1s1em2  
      doubleprecision rrgg2gghsoftt1s1em3  
      doubleprecision rrgg2gghsoftt1s1em4  
      doubleprecision rrgg2gghsoftt1s2e1  
      doubleprecision rrgg2gghsoftt1s2e0  
      doubleprecision rrgg2gghsoftt1s2em1  
      doubleprecision rrgg2gghsoftt1s2em2  
      doubleprecision rrgg2gghsoftt1s2em3  
      doubleprecision rrgg2gghsoftt1s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt1s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt1s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt1s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt1s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt1s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gghsoftt1s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghsoftt1s1e1
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
      t1 = -0.1D1 + x3
      t2 = x2 * t1
      t4 = Sin(x4 * pi)
      t5 = t4 ** 2
      t6 = x3 * t5
      t7 = -0.1D1 + x2
      t8 = t6 * t7
      t11 = log(0.4D1 * t2 * t8)
      t12 = t11 ** 2
      t15 = log(-0.4D1 * t2 * t6)
      t16 = t15 ** 2
      t24 = -0.90D2 * wd + 0.180D3 * wd * lh
      t28 = 0.1D1 / x2
      t33 = t1 * x3 * t5
      t35 = log(-0.4D1 * t33)
      t36 = t35 * wd
      t37 = 0.2D1 * wd - t36
      t42 = t35 ** 2
      t43 = t42 * wd
      t45 = lh ** 2
      t47 = pi ** 2
      t49 = -0.180D3 * t45 + 0.30D2 * t47
      t71 = x1 ** 2
      t72 = x2 * t71
      t75 = log(-0.4D1 * t72 * t33)
      t76 = t72 * t1
      t78 = (-0.1D1 + x1) ** 2
      t83 = log(0.4D1 * t76 * t6 * t7 * t78)
      t86 = log(0.4D1 * t76 * t8)
      t87 = t6 * t78
      t90 = log(-0.4D1 * t76 * t87)
      t93 = 0.1D1 / x1
      t97 = t71 * t1
      t100 = log(-0.4D1 * t97 * t87)
      t101 = t100 ** 2
      t104 = log(-0.4D1 * t97 * t6)
      t105 = t104 ** 2
      t115 = -(-0.90D2 * wd * (t12 / 0.2D1 - t16 / 0.2D1) + t24 * (-t11 
     #+ t15)) * t28 / 0.20D2 - 0.9D1 * t37 * lh - 0.9D1 / 0.2D1 * wd + 0
     #.9D1 / 0.2D1 * t36 - 0.9D1 / 0.4D1 * t43 - wd * t49 / 0.20D2 + 0.9
     #D1 * (0.3D1 * wd - 0.2D1 * t36 + t43 / 0.2D1) * lh + wd * (-0.60D2
     # * lh * t47 + 0.240D3 * zeta3 + 0.120D3 * t45 * lh) / 0.20D2 + 0.3
     #D1 / 0.4D1 * t42 * t35 * wd + t37 * t49 / 0.20D2 - 0.9D1 * wd * (-
     #t75 - t83 + t86 + t90) * t93 * t28 + (-0.90D2 * wd * (-t101 / 0.2D
     #1 + t105 / 0.2D1) + t24 * (t100 - t104)) * t93 / 0.10D2
      t116 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t115)
      rrgg2gghsoftt1s1e1 = t116 * t115

      end function



      doubleprecision function rrgg2gghsoftt1s1e0
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
      t4 = -0.1D1 + x3
      t7 = Sin(x4 * pi)
      t8 = t7 ** 2
      t11 = log(-0.4D1 * t4 * x3 * t8)
      t12 = t11 * wd
      t18 = t11 ** 2
      t21 = lh ** 2
      t23 = pi ** 2
      t28 = x2 * t4
      t29 = x3 * t8
      t34 = log(0.4D1 * t28 * t29 * (-0.1D1 + x2))
      t37 = log(-0.4D1 * t28 * t29)
      t43 = x1 ** 2
      t44 = t43 * t4
      t46 = (-0.1D1 + x1) ** 2
      t50 = log(-0.4D1 * t44 * t29 * t46)
      t53 = log(-0.4D1 * t44 * t29)
      t59 = -0.9D1 * wd * lh - 0.9D1 / 0.2D1 * wd + 0.9D1 / 0.2D1 * t12 
     #+ 0.9D1 * (0.2D1 * wd - t12) * lh - 0.9D1 / 0.4D1 * t18 * wd + wd 
     #* (-0.180D3 * t21 + 0.30D2 * t23) / 0.20D2 + 0.9D1 / 0.2D1 * wd * 
     #(-t34 + t37) / x2 - 0.9D1 * wd * (t50 - t53) / x1
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t59)
      rrgg2gghsoftt1s1e0 = t60 * t59

      end function



      doubleprecision function rrgg2gghsoftt1s1em1
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
      t7 = Sin(x4 * pi)
      t8 = t7 ** 2
      t11 = log(-0.4D1 * (-0.1D1 + x3) * x3 * t8)
      t14 = -0.9D1 / 0.2D1 * wd + 0.9D1 * wd * lh + 0.9D1 / 0.2D1 * t11 
     #* wd
      t15 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t14)
      rrgg2gghsoftt1s1em1 = t15 * t14

      end function



      doubleprecision function rrgg2gghsoftt1s1em2
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
      t2 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -0.9D1 /
     # 0.2D1 * wd)
      rrgg2gghsoftt1s1em2 = -0.9D1 / 0.2D1 * t2 * wd

      end function



      doubleprecision function rrgg2gghsoftt1s1em3
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
      rrgg2gghsoftt1s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt1s1em4
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
      rrgg2gghsoftt1s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gghsoftt1s2e1
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
      t1 = -0.1D1 + x3
      t2 = x2 * t1
      t4 = Sin(x4 * pi)
      t5 = t4 ** 2
      t6 = x3 * t5
      t7 = -0.1D1 + x2
      t8 = t6 * t7
      t11 = log(0.4D1 * t2 * t8)
      t12 = t11 ** 2
      t15 = log(-0.4D1 * t2 * t6)
      t16 = t15 ** 2
      t24 = -0.90D2 * wd + 0.180D3 * wd * lh
      t28 = 0.1D1 / x2
      t33 = t1 * x3 * t5
      t35 = log(-0.4D1 * t33)
      t36 = t35 * wd
      t37 = 0.2D1 * wd - t36
      t42 = t35 ** 2
      t43 = t42 * wd
      t45 = lh ** 2
      t47 = pi ** 2
      t49 = -0.180D3 * t45 + 0.30D2 * t47
      t71 = x1 ** 2
      t72 = x2 * t71
      t75 = log(-0.4D1 * t72 * t33)
      t76 = t72 * t1
      t78 = (-0.1D1 + x1) ** 2
      t83 = log(0.4D1 * t76 * t6 * t7 * t78)
      t86 = log(0.4D1 * t76 * t8)
      t87 = t6 * t78
      t90 = log(-0.4D1 * t76 * t87)
      t93 = 0.1D1 / x1
      t97 = t71 * t1
      t100 = log(-0.4D1 * t97 * t87)
      t101 = t100 ** 2
      t104 = log(-0.4D1 * t97 * t6)
      t105 = t104 ** 2
      t115 = -(-0.90D2 * wd * (t12 / 0.2D1 - t16 / 0.2D1) + t24 * (-t11 
     #+ t15)) * t28 / 0.20D2 - 0.9D1 * t37 * lh - 0.9D1 / 0.2D1 * wd + 0
     #.9D1 / 0.2D1 * t36 - 0.9D1 / 0.4D1 * t43 - wd * t49 / 0.20D2 + 0.9
     #D1 * (0.3D1 * wd - 0.2D1 * t36 + t43 / 0.2D1) * lh + wd * (-0.60D2
     # * lh * t47 + 0.240D3 * zeta3 + 0.120D3 * t45 * lh) / 0.20D2 + 0.3
     #D1 / 0.4D1 * t42 * t35 * wd + t37 * t49 / 0.20D2 - 0.9D1 * wd * (-
     #t75 - t83 + t86 + t90) * t93 * t28 + (-0.90D2 * wd * (-t101 / 0.2D
     #1 + t105 / 0.2D1) + t24 * (t100 - t104)) * t93 / 0.10D2
      t116 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t115)
      rrgg2gghsoftt1s2e1 = t116 * t115

      end function



      doubleprecision function rrgg2gghsoftt1s2e0
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
      t4 = -0.1D1 + x3
      t7 = Sin(x4 * pi)
      t8 = t7 ** 2
      t11 = log(-0.4D1 * t4 * x3 * t8)
      t12 = t11 * wd
      t18 = t11 ** 2
      t21 = lh ** 2
      t23 = pi ** 2
      t28 = x2 * t4
      t29 = x3 * t8
      t34 = log(0.4D1 * t28 * t29 * (-0.1D1 + x2))
      t37 = log(-0.4D1 * t28 * t29)
      t43 = x1 ** 2
      t44 = t43 * t4
      t46 = (-0.1D1 + x1) ** 2
      t50 = log(-0.4D1 * t44 * t29 * t46)
      t53 = log(-0.4D1 * t44 * t29)
      t59 = -0.9D1 * wd * lh - 0.9D1 / 0.2D1 * wd + 0.9D1 / 0.2D1 * t12 
     #+ 0.9D1 * (0.2D1 * wd - t12) * lh - 0.9D1 / 0.4D1 * t18 * wd + wd 
     #* (-0.180D3 * t21 + 0.30D2 * t23) / 0.20D2 + 0.9D1 / 0.2D1 * wd * 
     #(-t34 + t37) / x2 - 0.9D1 * wd * (t50 - t53) / x1
      t60 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t59)
      rrgg2gghsoftt1s2e0 = t60 * t59

      end function



      doubleprecision function rrgg2gghsoftt1s2em1
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
      t7 = Sin(x4 * pi)
      t8 = t7 ** 2
      t11 = log(-0.4D1 * (-0.1D1 + x3) * x3 * t8)
      t14 = -0.9D1 / 0.2D1 * wd + 0.9D1 * wd * lh + 0.9D1 / 0.2D1 * t11 
     #* wd
      t15 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, t14)
      rrgg2gghsoftt1s2em1 = t15 * t14

      end function



      doubleprecision function rrgg2gghsoftt1s2em2
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
      t2 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, 0.0D0, 0.0D0, 0.0D0, -0.9D1 /
     # 0.2D1 * wd)
      rrgg2gghsoftt1s2em2 = -0.9D1 / 0.2D1 * t2 * wd

      end function



      doubleprecision function rrgg2gghsoftt1s2em3
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
      rrgg2gghsoftt1s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt1s2em4
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
      rrgg2gghsoftt1s2em4 = 0.0D0

      end function
