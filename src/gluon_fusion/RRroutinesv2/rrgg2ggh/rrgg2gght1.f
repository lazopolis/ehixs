  
      subroutine rrgg2gght1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2ggh11J1  
      doubleprecision rrgg2ggh11J2  
      doubleprecision rrgg2ggh11J3  
      doubleprecision rrgg2ggh11J4  
      doubleprecision rrgg2ggh11J5  
      doubleprecision rrgg2ggh11J6  
      doubleprecision rrgg2ggh11J7  
      doubleprecision rrgg2gght1s1e1  
      doubleprecision rrgg2gght1s1e0  
      doubleprecision rrgg2gght1s1em1  
      doubleprecision rrgg2gght1s1em2  
      doubleprecision rrgg2gght1s1em3  
      doubleprecision rrgg2gght1s1em4  
      doubleprecision rrgg2gght1s2e1  
      doubleprecision rrgg2gght1s2e0  
      doubleprecision rrgg2gght1s2em1  
      doubleprecision rrgg2gght1s2em2  
      doubleprecision rrgg2gght1s2em3  
      doubleprecision rrgg2gght1s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght1s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght1s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght1s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght1s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght1s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght1s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght1s1e1
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
      doubleprecision rrgg2ggh11J1
      doubleprecision rrgg2ggh11J2
      doubleprecision rrgg2ggh11J3
      doubleprecision rrgg2ggh11J4
      doubleprecision rrgg2ggh11J5
      doubleprecision rrgg2ggh11J6
      doubleprecision rrgg2ggh11J7

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
      t9 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 0
     #.0D0, 0.0D0)
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x2 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t15 * x3
      t17 = t16 * t4
      t20 = log(-0.4D1 * t13 * t17)
      t21 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t23 = t20 ** 2
      t24 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t30 = pi * lh
      t36 = lh ** 2
      t38 = pi ** 2
      t40 = -0.180D3 * t36 + 0.30D2 * t38
      t41 = pi * t40
      t43 = t41 * t7 * t24
      t45 = 0.1D1 / x2
      t48 = t12 * t15
      t49 = x3 * t4
      t52 = log(-0.4D1 * t48 * t49)
      t53 = t52 * pi
      t56 = t52 ** 2
      t57 = t56 * pi
      t63 = rrgg2ggh11J4(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t90 = 0.180D3 * lh
      t91 = x1 ** 2
      t92 = x2 * t91
      t93 = t92 * t12
      t96 = log(-0.4D1 * t93 * t17)
      t101 = 0.1D1 / x1
      t106 = t91 * t12
      t109 = log(-0.4D1 * t106 * t17)
      t111 = t109 ** 2
      t125 = (-0.90D2 * t8 * (-t9 + t20 * t21 - t23 * t24 / 0.2D1) + 0.1
     #80D3 * t30 * t7 * (-t21 + t20 * t24) - t43) * t45 / 0.1440D4 - (-0
     #.180D3 * t53 * lh - 0.45D2 * t57 + t41) * t7 * t21 / 0.1440D4 + t8
     # * t63 / 0.16D2 - (0.90D2 * t57 * lh + pi * (-0.60D2 * lh * t38 + 
     #0.240D3 * zeta3 + 0.120D3 * t36 * lh) + 0.15D2 * t56 * t52 * pi - 
     #t53 * t40) * t7 * t24 / 0.1440D4 - (0.180D3 * t30 + 0.90D2 * t53) 
     #* t7 * t9 / 0.1440D4 - t8 * (-0.90D2 * t21 - (-t90 - 0.90D2 * t96)
     # * t24) * t101 * t45 / 0.720D3 + (-0.90D2 * t8 * (-t9 + t109 * t21
     # - t111 * t24 / 0.2D1) + 0.180D3 * t30 * t7 * (-t21 + t109 * t24) 
     #- t43) * t101 / 0.720D3
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
      t169 = 0.1D1 / (-0.1D1 + x1 - t139 + x2 - t153 - t134 + t154)
      t171 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, t151, -t158, t1
     #52, -t160, -t166)
      t173 = t92 * t48
      t174 = t128 ** 2
      t175 = t149 * t174
      t180 = log(0.4D1 * t173 * t49 * t175 * t137)
      t184 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t151, -t158, t1
     #52, -t160, -t166)
      t189 = (0.90D2 * t140 * t169 * t171 + (-t90 - 0.90D2 * t180) * t14
     #0 * t169 * t184) * t101 * t45
      t192 = FJET(XB1, XB2, s, t151, t152, -t158, -t160, -t166, -t8 * t1
     #89 / 0.720D3)
      t199 = Sqrt(x2 * t137 * t49)
      t201 = 0.2D1 * t136 * t199
      t203 = t2 * (-x3 + t132 - x2 + t201)
      t205 = t2 * (0.1D1 - x2 - x3 + t132 + t201)
      t207 = 0.1D1 / (0.1D1 - x2 + t134)
      t208 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, s, -t203, t205, 0.
     #0D0, 0.0D0, 0.0D0)
      t214 = log(0.4D1 * t13 * t15 * t49 * t137)
      t215 = t214 * t207
      t216 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, -t203, t205, 0.
     #0D0, 0.0D0, 0.0D0)
      t218 = t214 ** 2
      t220 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t203, t205, 0.
     #0D0, 0.0D0, 0.0D0)
      t226 = t207 * t216
      t243 = log(0.4D1 * t93 * t16 * t4 * t137)
      t253 = (-0.90D2 * t8 * (t207 * t208 - t215 * t216 + t218 * t207 * 
     #t220 / 0.2D1) + 0.180D3 * t30 * t7 * (t226 - t215 * t220) + t41 * 
     #t7 * t207 * t220) * t45 / 0.1440D4 - t8 * (0.90D2 * t226 + (-t90 -
     # 0.90D2 * t243) * t207 * t220) * t101 * t45 / 0.720D3
      t254 = FJET(XB1, XB2, s, -t203, 0.0D0, t205, 0.0D0, 0.0D0, t253)
      t257 = t2 * t128 * x3
      t259 = t2 * t128 * t4
      t260 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, -t257, t259, t1
     #52, -t160, 0.0D0)
      t262 = t49 * t175
      t265 = log(-0.4D1 * t173 * t262)
      t268 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t257, t259, t1
     #52, -t160, 0.0D0)
      t274 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, s, -t257, t259, t1
     #52, -t160, 0.0D0)
      t278 = log(-0.4D1 * t106 * t15 * t262)
      t280 = t278 ** 2
      t296 = -t8 * (0.90D2 * t260 - (t90 + 0.90D2 * t265) * t268) * t101
     # * t45 / 0.720D3 + (0.90D2 * t8 * (-t274 + t278 * t260 - t280 * t2
     #68 / 0.2D1) - 0.180D3 * t30 * t7 * (-t260 + t278 * t268) + t41 * t
     #7 * t268) * t101 / 0.720D3
      t297 = FJET(XB1, XB2, s, -t257, t152, t259, -t160, 0.0D0, t296)
      rrgg2gght1s1e1 = t126 * t125 - t192 * pi * t7 * t189 / 0.720D3 + t
     #254 * t253 + t297 * t296

      end function



      doubleprecision function rrgg2gght1s1e0
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
      doubleprecision rrgg2ggh11J1
      doubleprecision rrgg2ggh11J2
      doubleprecision rrgg2ggh11J3
      doubleprecision rrgg2ggh11J4
      doubleprecision rrgg2ggh11J5
      doubleprecision rrgg2ggh11J6
      doubleprecision rrgg2ggh11J7

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
      t9 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 0
     #.0D0, 0.0D0)
      t10 = x4 * pi
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x2 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = t15 * x3 * t4
      t20 = log(-0.4D1 * t13 * t17)
      t21 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t26 = pi * lh
      t29 = 0.180D3 * t26 * t7 * t21
      t31 = 0.1D1 / x2
      t34 = 0.1D1 / x1
      t39 = x1 ** 2
      t40 = t39 * t12
      t43 = log(-0.4D1 * t40 * t17)
      t51 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 
     #0.0D0, 0.0D0)
      t56 = x3 * t4
      t59 = log(-0.4D1 * t12 * t15 * t56)
      t60 = t59 * pi
      t68 = t59 ** 2
      t71 = lh ** 2
      t73 = pi ** 2
      t81 = (-0.90D2 * t8 * (-t9 + t20 * t21) - t29) * t31 / 0.1440D4 + 
     #t8 * t21 * t34 * t31 / 0.8D1 + (-0.90D2 * t8 * (-t9 + t43 * t21) -
     # t29) * t34 / 0.720D3 + t8 * t51 / 0.16D2 - (0.180D3 * t26 + 0.90D
     #2 * t60) * t7 * t9 / 0.1440D4 - (-0.180D3 * t60 * lh - 0.45D2 * t6
     #8 * pi + pi * (-0.180D3 * t71 + 0.30D2 * t73)) * t7 * t21 / 0.1440
     #D4
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
      t123 = 0.90D2 * t96
      t127 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t107, -t114, t1
     #08, -t116, -t122)
      t130 = 0.1D1 / (-0.1D1 + x1 - t95 + x2 - t109 - t90 + t110) * t127
     # * t34 * t31
      t133 = FJET(XB1, XB2, s, t107, t108, -t114, -t116, -t122, -t8 * t1
     #23 * t130 / 0.720D3)
      t141 = Sqrt(x2 * t93 * t56)
      t143 = 0.2D1 * t92 * t141
      t145 = t2 * (-x3 + t88 - x2 + t143)
      t147 = t2 * (0.1D1 - x2 - x3 + t88 + t143)
      t149 = 0.1D1 / (0.1D1 - x2 + t90)
      t150 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, -t145, t147, 0.
     #0D0, 0.0D0, 0.0D0)
      t156 = log(0.4D1 * t13 * t15 * t56 * t93)
      t158 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t145, t147, 0.
     #0D0, 0.0D0, 0.0D0)
      t175 = (-0.90D2 * t8 * (t149 * t150 - t156 * t149 * t158) + 0.180D
     #3 * t26 * t7 * t149 * t158) * t31 / 0.1440D4 - t8 * t149 * t158 * 
     #t34 * t31 / 0.8D1
      t176 = FJET(XB1, XB2, s, -t145, 0.0D0, t147, 0.0D0, 0.0D0, t175)
      t179 = t2 * t84 * x3
      t181 = t2 * t84 * t4
      t182 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t179, t181, t1
     #08, -t116, 0.0D0)
      t187 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, -t179, t181, t1
     #08, -t116, 0.0D0)
      t189 = t84 ** 2
      t194 = log(-0.4D1 * t40 * t15 * t56 * t105 * t189)
      t205 = -t8 * t182 * t34 * t31 / 0.8D1 + (0.90D2 * t8 * (-t187 + t1
     #94 * t182) + 0.180D3 * t26 * t7 * t182) * t34 / 0.720D3
      t206 = FJET(XB1, XB2, s, -t179, t108, t181, -t116, 0.0D0, t205)
      rrgg2gght1s1e0 = t82 * t81 - t133 * pi * t7 * t123 * t130 / 0.720D
     #3 + t176 * t175 + t206 * t205

      end function



      doubleprecision function rrgg2gght1s1em1
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
      doubleprecision rrgg2ggh11J1
      doubleprecision rrgg2ggh11J2
      doubleprecision rrgg2ggh11J3
      doubleprecision rrgg2ggh11J4
      doubleprecision rrgg2ggh11J5
      doubleprecision rrgg2ggh11J6
      doubleprecision rrgg2ggh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 0
     #.0D0, 0.0D0)
      t10 = 0.1D1 / x1
      t14 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 
     #0.0D0, 0.0D0)
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
      t51 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t44, t48, t46, 
     #-t50, 0.0D0)
      t55 = FJET(XB1, XB2, s, -t44, t46, t48, -t50, 0.0D0, -t8 * t51 * t
     #10 / 0.8D1)
      t62 = 0.2D1 * x2 * x3
      t63 = cos(t19)
      t67 = Sqrt(x2 * (-0.1D1 + x2) * t25)
      t69 = 0.2D1 * t63 * t67
      t71 = t2 * (-x3 + t62 - x2 + t69)
      t73 = t2 * (0.1D1 - x2 - x3 + t62 + t69)
      t77 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, -t71, t73, 0.0D0
     #, 0.0D0, 0.0D0)
      t79 = 0.1D1 / (0.1D1 - x2 + x2 * z) * t77 * t35
      t82 = FJET(XB1, XB2, s, -t71, 0.0D0, t73, 0.0D0, 0.0D0, -t8 * t79 
     #/ 0.16D2)
      rrgg2gght1s1em1 = t40 * t39 - t55 * pi * t7 * t51 * t10 / 0.8D1 - 
     #t82 * pi * t7 * t79 / 0.16D2

      end function



      doubleprecision function rrgg2gght1s1em2
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
      doubleprecision rrgg2ggh11J1
      doubleprecision rrgg2ggh11J2
      doubleprecision rrgg2ggh11J3
      doubleprecision rrgg2ggh11J4
      doubleprecision rrgg2ggh11J5
      doubleprecision rrgg2ggh11J6
      doubleprecision rrgg2ggh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = t2 * x3
      t5 = t2 * (-0.1D1 + x3)
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t9 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t3, -t5, 0.0D0, 0
     #.0D0, 0.0D0)
      t12 = FJET(XB1, XB2, s, t3, 0.0D0, -t5, 0.0D0, 0.0D0, pi * t7 * t9
     # / 0.16D2)
      rrgg2gght1s1em2 = t12 * pi * t7 * t9 / 0.16D2

      end function



      doubleprecision function rrgg2gght1s1em3
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
      doubleprecision rrgg2ggh11J1
      doubleprecision rrgg2ggh11J2
      doubleprecision rrgg2ggh11J3
      doubleprecision rrgg2ggh11J4
      doubleprecision rrgg2ggh11J5
      doubleprecision rrgg2ggh11J6
      doubleprecision rrgg2ggh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght1s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght1s1em4
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
      doubleprecision rrgg2ggh11J1
      doubleprecision rrgg2ggh11J2
      doubleprecision rrgg2ggh11J3
      doubleprecision rrgg2ggh11J4
      doubleprecision rrgg2ggh11J5
      doubleprecision rrgg2ggh11J6
      doubleprecision rrgg2ggh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght1s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght1s2e1
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
      doubleprecision rrgg2ggh11J1
      doubleprecision rrgg2ggh11J2
      doubleprecision rrgg2ggh11J3
      doubleprecision rrgg2ggh11J4
      doubleprecision rrgg2ggh11J5
      doubleprecision rrgg2ggh11J6
      doubleprecision rrgg2ggh11J7

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
      t9 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3,
     # -t5, 0.0D0)
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
      t36 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
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
      t70 = rrgg2ggh11J4(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t94 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
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
     # t54) * t7 * t94 / 0.1440D4 - t8 * (t104 * t9 - t110 * t9) * t113 
     #* t47 / 0.8D1 - (-0.90D2 * t8 * (t94 - t121 * t36 + t123 * t9 / 0.
     #2D1) + 0.180D3 * t39 * t7 * (t36 - t121 * t9) + t65 * t40) * t113 
     #/ 0.720D3
      t139 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t138)
      t141 = x3 * x1
      t142 = t2 * t141
      t143 = -0.1D1 + x1
      t145 = t2 * t143 * x3
      t147 = t2 * x1 * t4
      t149 = t2 * t143 * t4
      t150 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, t142, -t147, -t
     #145, t149, 0.0D0)
      t151 = 0.1D1 / t10
      t153 = t98 * t16 * t151
      t154 = x1 * z
      t155 = -z - x1 + t154
      t156 = 0.1D1 / t155
      t157 = t143 ** 2
      t158 = t156 * t157
      t159 = t18 * t158
      t162 = log(0.4D1 * t153 * t159)
      t163 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t142, -t147, -t
     #145, t149, 0.0D0)
      t168 = t7 * t163
      t174 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, s, t142, -t147, -t
     #145, t149, 0.0D0)
      t178 = log(0.4D1 * t118 * t151 * t159)
      t180 = t178 ** 2
      t195 = (-0.90D2 * t8 * (t150 - t162 * t163) + 0.180D3 * t39 * t168
     #) * t113 * t47 / 0.720D3 - (0.90D2 * t8 * (t174 - t178 * t150 + t1
     #80 * t163 / 0.2D1) - 0.180D3 * t39 * t7 * (t150 - t178 * t163) - t
     #65 * t168) * t113 / 0.720D3
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
      t232 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, t216, -t222, -t
     #145, t149, t228)
      t238 = log(-0.4D1 * t153 * t18 * t158 * t19)
      t240 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t216, -t222, -t
     #145, t149, t228)
      t250 = -0.90D2 * t8 * (t231 * t232 - t238 * t155 * t230 * t240) + 
     #0.180D3 * t39 * t7 * t231 * t240
      t254 = FJET(XB1, XB2, s, t216, -t145, -t222, t149, t228, t250 * t1
     #13 * t47 / 0.720D3)
      rrgg2gght1s2e1 = t139 * t138 + t196 * t195 + t254 * t250 * t113 * 
     #t47 / 0.720D3

      end function



      doubleprecision function rrgg2gght1s2e0
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
      doubleprecision rrgg2ggh11J1
      doubleprecision rrgg2ggh11J2
      doubleprecision rrgg2ggh11J3
      doubleprecision rrgg2ggh11J4
      doubleprecision rrgg2ggh11J5
      doubleprecision rrgg2ggh11J6
      doubleprecision rrgg2ggh11J7

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
      t9 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3,
     # -t5, 0.0D0)
      t12 = pi * lh
      t14 = z ** 2
      t16 = 0.1D1 / t14 / z
      t17 = x4 * pi
      t18 = Sin(t17)
      t19 = t18 ** 2
      t21 = x3 * t4
      t24 = log(-0.4D1 * t16 * t19 * t21)
      t25 = t24 * pi
      t29 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t34 = t24 ** 2
      t37 = lh ** 2
      t39 = pi ** 2
      t45 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
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
     #t54 + t59) * t62 / 0.16D2 - (-0.90D2 * t8 * (t29 - t72 * t45) + 0.
     #180D3 * t12 * t7 * t45) * t81 / 0.720D3
      t85 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t84)
      t87 = x3 * x1
      t88 = t2 * t87
      t89 = -0.1D1 + x1
      t91 = t2 * t89 * x3
      t93 = t2 * x1 * t4
      t95 = t2 * t89 * t4
      t96 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t88, -t93, -t91,
     # t95, 0.0D0)
      t101 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, t88, -t93, -t91
     #, t95, 0.0D0)
      t104 = x1 * z
      t105 = -z - x1 + t104
      t106 = 0.1D1 / t105
      t107 = t89 ** 2
      t112 = log(0.4D1 * t67 / t14 * t21 * t106 * t107)
      t123 = -t8 * t96 * t81 * t62 / 0.8D1 - (0.90D2 * t8 * (t101 - t112
     # * t96) - 0.180D3 * t12 * t7 * t96) * t81 / 0.720D3
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
      t160 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t144, -t150, -t
     #91, t95, t156)
      t163 = 0.1D1 / (z + x1 - t104 - t145 + t146) * t160 * t81 * t62
      t166 = FJET(XB1, XB2, s, t144, -t91, -t150, t95, t156, -t8 * t105 
     #* t163 / 0.8D1)
      rrgg2gght1s2e0 = t85 * t84 + t124 * t123 - t166 * pi * t7 * t105 *
     # t163 / 0.8D1

      end function



      doubleprecision function rrgg2gght1s2em1
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
      doubleprecision rrgg2ggh11J1
      doubleprecision rrgg2ggh11J2
      doubleprecision rrgg2ggh11J3
      doubleprecision rrgg2ggh11J4
      doubleprecision rrgg2ggh11J5
      doubleprecision rrgg2ggh11J6
      doubleprecision rrgg2ggh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = t2 * x3
      t4 = -0.1D1 + x3
      t5 = t2 * t4
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = pi * t7
      t9 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3,
     # -t5, 0.0D0)
      t14 = z ** 2
      t18 = Sin(x4 * pi)
      t19 = t18 ** 2
      t24 = log(-0.4D1 / t14 / z * t19 * x3 * t4)
      t29 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3
     #, -t5, 0.0D0)
      t32 = 0.1D1 / x1
      t36 = t8 * t9 / 0.16D2 - (0.180D3 * pi * lh + 0.90D2 * t24 * pi) *
     # t7 * t29 / 0.1440D4 + t8 * t29 * t32 / 0.8D1
      t37 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, t36)
      t40 = t2 * x1 * x3
      t41 = -0.1D1 + x1
      t43 = t2 * t41 * x3
      t45 = t2 * x1 * t4
      t47 = t2 * t41 * t4
      t48 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, t40, -t45, -t43,
     # t47, 0.0D0)
      t52 = FJET(XB1, XB2, s, t40, -t43, -t45, t47, 0.0D0, -t8 * t48 * t
     #32 / 0.8D1)
      rrgg2gght1s2em1 = t37 * t36 - t52 * pi * t7 * t48 * t32 / 0.8D1

      end function



      doubleprecision function rrgg2gght1s2em2
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
      doubleprecision rrgg2ggh11J1
      doubleprecision rrgg2ggh11J2
      doubleprecision rrgg2ggh11J3
      doubleprecision rrgg2ggh11J4
      doubleprecision rrgg2ggh11J5
      doubleprecision rrgg2ggh11J6
      doubleprecision rrgg2ggh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t2 = (-0.1D1 + z) * s
      t3 = t2 * x3
      t5 = t2 * (-0.1D1 + x3)
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t9 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, s, 0.0D0, 0.0D0, t3,
     # -t5, 0.0D0)
      t12 = FJET(XB1, XB2, s, 0.0D0, t3, 0.0D0, -t5, 0.0D0, pi * t7 * t9
     # / 0.16D2)
      rrgg2gght1s2em2 = t12 * pi * t7 * t9 / 0.16D2

      end function



      doubleprecision function rrgg2gght1s2em3
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
      doubleprecision rrgg2ggh11J1
      doubleprecision rrgg2ggh11J2
      doubleprecision rrgg2ggh11J3
      doubleprecision rrgg2ggh11J4
      doubleprecision rrgg2ggh11J5
      doubleprecision rrgg2ggh11J6
      doubleprecision rrgg2ggh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght1s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght1s2em4
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
      doubleprecision rrgg2ggh11J1
      doubleprecision rrgg2ggh11J2
      doubleprecision rrgg2ggh11J3
      doubleprecision rrgg2ggh11J4
      doubleprecision rrgg2ggh11J5
      doubleprecision rrgg2ggh11J6
      doubleprecision rrgg2ggh11J7

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2gght1s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh11J1
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
      t4 = S34 * t3
      t5 = S12 + S14 + S24
      t6 = 0.1D1 / t5
      t8 = S13 + S14 + S34
      t9 = S23 + S24 + S34
      t11 = S12 ** 2
      t12 = 0.1D1 / t11
      t17 = s ** 2
      t20 = z ** 2
      t23 = t5 ** 2
      t24 = 0.1D1 / t23
      t26 = t6 * t3
      t28 = t2 ** 2
      t29 = 0.1D1 / t28
      t37 = -t6 - t3
      t38 = 0.216D3 * t37
      t42 = 0.216D3 * S13
      t43 = 0.216D3 * S14
      t44 = 0.432D3 * S34
      t46 = 0.216D3 * S24
      t51 = t8 * S34
      t53 = S13 * S24
      t54 = 0.36D2 * t53
      t55 = S24 ** 2
      t56 = 0.18D2 * t55
      t57 = S13 ** 2
      t58 = 0.18D2 * t57
      t59 = -t54 + t56 + t58
      t61 = S23 ** 2
      t62 = 0.18D2 * t61
      t63 = S14 ** 2
      t64 = 0.18D2 * t63
      t65 = S14 * S23
      t66 = 0.36D2 * t65
      t67 = t62 + t64 - t66
      t78 = S14 * S24
      t79 = 0.9D1 * t78
      t83 = S23 * S24
      t84 = S13 * S14
      t91 = S13 * S23
      t92 = 0.9D1 * t91
      t108 = S34 ** 2
      t110 = -t37
      t134 = 0.36D2 * t84
      t135 = 0.36D2 * t83
      t136 = 0.36D2 * t91
      t137 = 0.36D2 * t78
      t138 = -0.216D3 * t108 + ((0.2313D4 / 0.4D1 * t110 * t9 + 0.2313D4
     # / 0.4D1 - 0.216D3 * S24 * t6 - 0.216D3 * S23 * t3) * t8 + (0.2313
     #D4 / 0.4D1 - 0.216D3 * S14 * t6 - 0.216D3 * S13 * t3) * t9) * S34 
     #+ (-0.432D3 * S23 - 0.432D3 * S24 - t44 - t42 - t43) * t8 + (-0.21
     #6D3 * S24 - 0.216D3 * S23) * t9 + t134 + t66 - t62 + t54 + t135 - 
     #t136 - t56 - t137 - t58 - t64
      t140 = -S23 - S13
      t156 = ((t66 - t64 + t54 + t136 - t137 + t134 + t62 + t135 + t58 -
     # t56) * t6 + (-t58 + t54 + t66 - t62 + t135 + t56 + t134 - t136 + 
     #t137 + t64) * t3) * t9 - t56 + t64 - t62 + t54 + t134 + t136 + t13
     #7 - t135 + t66 + t58
      t165 = 0.9D1 / 0.8D1 * t110
      t168 = t165 * t8 + t165 * t9
      t170 = t26 * t9
      t172 = 0.18D2 * t6
      t173 = 0.18D2 * t3
      t177 = 0.18D2 * t37 * t9
      t182 = 0.27D2 / 0.8D1 * S14
      t183 = 0.9D1 / 0.2D1 * S13
      t184 = 0.27D2 / 0.8D1 * S24
      t187 = 0.9D1 / 0.2D1 * S14
      t188 = 0.27D2 / 0.8D1 * S23
      t189 = 0.27D2 / 0.8D1 * S13
      t194 = 0.9D1 / 0.2D1 * S23
      t197 = 0.9D1 / 0.2D1 * S24
      t215 = 0.36D2 * S14 - 0.27D2 * S23
      t219 = 0.36D2 * S13 - 0.27D2 * S24
      t225 = -0.27D2 * S13 + 0.36D2 * S24
      t229 = -0.27D2 * S14 + 0.36D2 * S23
      t235 = 0.1845D4 / 0.16D2 * S13
      t236 = 0.603D3 / 0.16D2 * S24
      t237 = 0.603D3 / 0.16D2 * S14
      t238 = 0.1845D4 / 0.16D2 * S23
      t241 = 0.603D3 / 0.16D2 * S23
      t242 = 0.1845D4 / 0.16D2 * S24
      t243 = 0.1845D4 / 0.16D2 * S14
      t244 = 0.603D3 / 0.16D2 * S13
      t251 = 0.27D2 / 0.4D1 * t78
      t252 = 0.27D2 / 0.2D1 * t57
      t253 = 0.27D2 / 0.8D1 * t63
      t254 = 0.63D2 / 0.8D1 * t55
      t255 = 0.27D2 * t53
      t256 = 0.9D1 * t84
      t259 = 0.27D2 / 0.2D1 * t63
      t260 = 0.63D2 / 0.8D1 * t61
      t261 = 0.27D2 * t65
      t262 = 0.27D2 / 0.4D1 * t91
      t263 = 0.27D2 / 0.8D1 * t57
      t270 = 0.9D1 * t83
      t271 = 0.27D2 / 0.8D1 * t55
      t272 = 0.27D2 / 0.2D1 * t61
      t273 = 0.63D2 / 0.8D1 * t63
      t276 = 0.27D2 / 0.2D1 * t55
      t277 = 0.63D2 / 0.8D1 * t57
      t278 = 0.27D2 / 0.8D1 * t61
      t287 = t108 * S34
      t306 = 0.18D2 * t53
      t313 = 0.18D2 * t65
      t330 = ((0.1863D4 / 0.2D1 + (-t56 + t255 - t64 - t58 - 0.27D2 * t8
     #3 + t136 + t137 - 0.27D2 * t84 + t261 - t62) * t6 * t3) * t9 + (-t
     #306 + 0.135D3 * t57 - t64 + t261 - 0.72D2 * t55 - t62) * t6 + (0.1
     #35D3 * t63 + t255 - 0.72D2 * t61 - t58 - t313 - t56) * t3) * t8 + 
     #((-t313 + 0.135D3 * t61 + t255 - 0.72D2 * t63 - t58 - t56) * t6 + 
     #(-0.72D2 * t57 - t64 + t261 + 0.135D3 * t55 - t62 - t306) * t3) * 
     #t9 - t58 + t134 - t62 - 0.27D2 * t78 + t255 + t261 - 0.27D2 * t91 
     #+ t135 - t64 - t56
      t332 = 0.549D3 / 0.2D1 * S23
      t333 = 0.549D3 / 0.2D1 * S24
      t334 = 0.549D3 / 0.2D1 * S13
      t335 = 0.549D3 / 0.2D1 * S14
      t336 = 0.1197D4 / 0.16D2 * t84
      t337 = 0.27D2 * t61
      t338 = 0.1197D4 / 0.16D2 * t83
      t339 = 0.27D2 * t57
      t340 = 0.333D3 / 0.8D1 * t55
      t341 = 0.1197D4 / 0.16D2 * t53
      t343 = 0.1197D4 / 0.16D2 * t65
      t345 = 0.333D3 / 0.8D1 * t63
      t348 = 0.333D3 / 0.8D1 * t61
      t351 = 0.27D2 * t63
      t352 = 0.27D2 * t55
      t353 = 0.333D3 / 0.8D1 * t57
      t359 = 0.9D1 * t65
      t360 = 0.45D2 / 0.16D2 * t61
      t361 = 0.45D2 * t78
      t362 = 0.171D3 / 0.8D1 * t57
      t363 = 0.45D2 / 0.16D2 * t55
      t364 = 0.9D1 * t53
      t365 = 0.45D2 * t91
      t366 = 0.171D3 / 0.8D1 * t63
      t367 = t84 * S24
      t368 = 0.27D2 * t367
      t369 = t63 * S24
      t371 = S14 * t55
      t373 = S13 * t55
      t374 = 0.117D3 / 0.2D1 * t373
      t375 = t57 * S13
      t376 = 0.18D2 * t375
      t377 = t63 * S14
      t378 = 0.9D1 / 0.8D1 * t377
      t379 = t55 * S24
      t380 = 0.189D3 / 0.8D1 * t379
      t381 = t57 * S14
      t383 = S13 * t63
      t385 = t57 * S24
      t386 = 0.135D3 / 0.2D1 * t385
      t389 = S14 * t61
      t390 = 0.117D3 / 0.2D1 * t389
      t391 = t57 * S23
      t393 = S13 * t61
      t396 = t63 * S23
      t397 = 0.135D3 / 0.2D1 * t396
      t398 = t84 * S23
      t399 = 0.27D2 * t398
      t401 = 0.9D1 / 0.8D1 * t375
      t402 = t61 * S23
      t403 = 0.189D3 / 0.8D1 * t402
      t404 = 0.18D2 * t377
      t407 = (t332 + t333 + t334 + t335 + (t336 + t337 + t338 + t339 - t
     #340 + t341 + 0.54D2 * t91 + t343 - 0.333D3 / 0.4D1 * t78 - t345) *
     # t6 + (t343 + t336 - t348 + t338 + 0.54D2 * t78 + t341 - 0.333D3 /
     # 0.4D1 * t91 + t351 + t352 - t353) * t3) * t9 + 0.333D3 / 0.8D1 * 
     #t83 + t359 - t360 - t361 - t362 - t363 + t364 + t256 - t365 - t366
     # + (t368 + 0.27D2 / 0.8D1 * t369 - 0.63D2 / 0.8D1 * t371 - t374 - 
     #t376 - t378 + t380 - 0.27D2 / 0.2D1 * t381 - 0.9D1 / 0.2D1 * t383 
     #+ t386) * t6 + (-t390 + 0.27D2 / 0.8D1 * t391 - 0.63D2 / 0.8D1 * t
     #393 - 0.27D2 / 0.2D1 * t383 + t397 + t399 - 0.9D1 / 0.2D1 * t381 -
     # t401 + t403 - t404) * t3
      t409 = 0.171D3 / 0.8D1 * t55
      t410 = 0.45D2 / 0.16D2 * t57
      t411 = 0.171D3 / 0.8D1 * t61
      t413 = 0.45D2 / 0.16D2 * t63
      t414 = t65 * S24
      t415 = 0.27D2 * t414
      t416 = t61 * S24
      t420 = 0.9D1 / 0.8D1 * t379
      t421 = 0.135D3 / 0.2D1 * t389
      t422 = 0.18D2 * t402
      t423 = t55 * S23
      t425 = 0.117D3 / 0.2D1 * t396
      t426 = 0.189D3 / 0.8D1 * t377
      t430 = 0.117D3 / 0.2D1 * t385
      t431 = 0.135D3 / 0.2D1 * t373
      t433 = 0.9D1 / 0.8D1 * t402
      t434 = t53 * S23
      t435 = 0.27D2 * t434
      t438 = 0.189D3 / 0.8D1 * t375
      t439 = 0.18D2 * t379
      t442 = -t409 - t361 + t270 + t359 - t410 + t364 - t411 + 0.333D3 /
     # 0.8D1 * t84 - t365 - t413 + (t415 - 0.27D2 / 0.2D1 * t416 - 0.63D
     #2 / 0.8D1 * t369 + 0.27D2 / 0.8D1 * t371 - t420 + t421 - t422 - 0.
     #9D1 / 0.2D1 * t423 - t425 + t426) * t6 + (-0.27D2 / 0.2D1 * t423 -
     # t430 + t431 - 0.9D1 / 0.2D1 * t416 - t433 + t435 - 0.63D2 / 0.8D1
     # * t391 + 0.27D2 / 0.8D1 * t393 + t438 - t439) * t3
      t446 = t108 ** 2
      t471 = 0.27D2 / 0.4D1 * t83
      t478 = 0.27D2 / 0.4D1 * t84
      t487 = 0.45D2 * t84
      t488 = 0.45D2 * t83
      t497 = 0.1197D4 / 0.16D2 * t78
      t500 = 0.1197D4 / 0.16D2 * t91
      t513 = (t332 + t333 + t334 + t335 + (t364 - t487 - t411 - t362 - t
     #363 + t92 - t488 + t359 + 0.333D3 / 0.8D1 * t78 - t413) * t6 + (t3
     #59 - t487 - t360 - t366 + t79 + t364 - t410 - t409 + 0.333D3 / 0.8
     #D1 * t91 - t488) * t3) * t9 + t341 + t343 + t339 - t348 + t497 - 0
     #.333D3 / 0.4D1 * t83 + t351 + 0.54D2 * t84 + t500 - t340 + (-t433 
     #- t376 + t435 - t374 - 0.27D2 / 0.2D1 * t391 + t386 - 0.63D2 / 0.8
     #D1 * t423 + 0.27D2 / 0.8D1 * t416 + t380 - 0.9D1 / 0.2D1 * t393) *
     # t6 + (-0.27D2 / 0.2D1 * t369 - 0.63D2 / 0.8D1 * t416 + 0.27D2 / 0
     #.8D1 * t423 - t390 + t415 - t404 + t403 - t420 + t397 - 0.9D1 / 0.
     #2D1 * t371) * t3
      t529 = t341 + t343 - t353 + t337 + t497 + 0.54D2 * t83 - t345 - 0.
     #333D3 / 0.4D1 * t84 + t500 + t352 + (-t422 - t401 + t399 - 0.63D2 
     #/ 0.8D1 * t383 - 0.9D1 / 0.2D1 * t391 + 0.27D2 / 0.8D1 * t381 + t4
     #21 - t425 + t426 - 0.27D2 / 0.2D1 * t393) * t6 + (-0.9D1 / 0.2D1 *
     # t369 - t430 + t431 + 0.27D2 / 0.8D1 * t383 + t368 + t438 - t378 -
     # t439 - 0.63D2 / 0.8D1 * t381 - 0.27D2 / 0.2D1 * t371) * t3
      t543 = 0.18D2 * t385
      t544 = 0.36D2 * t398
      t545 = 0.18D2 * t383
      t546 = 0.36D2 * t434
      t547 = 0.18D2 * t423
      t548 = 0.36D2 * t373
      t549 = 0.18D2 * t391
      t550 = 0.18D2 * t389
      t551 = 0.18D2 * t393
      t552 = 0.36D2 * t396
      t553 = t439 + t543 - t544 + t545 - t546 + t547 - t548 + t549 + t55
     #0 + t551 - t552 + t404
      t555 = 0.36D2 * t367
      t556 = 0.18D2 * t416
      t557 = 0.36D2 * t414
      t558 = 0.36D2 * t385
      t559 = 0.18D2 * t373
      t560 = 0.18D2 * t381
      t561 = 0.18D2 * t396
      t562 = 0.36D2 * t389
      t563 = 0.18D2 * t369
      t564 = 0.18D2 * t371
      t565 = -t555 + t556 - t557 - t558 + t559 + t376 + t560 + t561 - t5
     #62 + t563 + t564 + t422
      t567 = -0.81D2 * t61 + 0.117D3 * t53 + 0.18D2 * t84 - 0.81D2 * t57
     # - 0.81D2 * t55 - 0.81D2 * t63 + 0.18D2 * t83 + 0.117D3 * t65 + 0.
     #18D2 * t91 + 0.18D2 * t78 + t553 * t6 + t565 * t3
      t569 = t567 * t9 - t555 - t544 + t543 - t548 + t560 + t545 + t564 
     #+ t561 - t562 + t551 + t439 + t422
      t571 = t376 + t404 + t563 - t552 - t558 + t559 + t549 + t547 + t55
     #0 + t556 - t557 - t546
      t575 = (-0.72D2 * t1 - 0.72D2 * t4 * t6 * t8 * t9 * t12) * t17 * s
     # * t20 * z + (((((0.81D2 / 0.2D1 * t24 - 0.216D3 * t26 + 0.81D2 / 
     #0.2D1 * t29) * t9 - 0.216D3 * t6 - 0.216D3 * t3) * t8 - 0.216D3 + 
     #t38 * t9) * S34 + t42 + t43 + t44 + 0.216D3 * S23 + t46) * t1 + (-
     #t38 * t9 * t51 + (t59 * t6 + t67 * t3) * t8 + (t67 * t6 + t59 * t3
     #) * t9) * t12 + ((0.9D1 / 0.2D1 * t63 - t79 + 0.9D1 / 0.2D1 * t55)
     # * t24 + (-0.9D1 * t83 + 0.9D1 * t65 + 0.9D1 * t53 - 0.9D1 * t84) 
     #* t6 * t3 + (0.9D1 / 0.2D1 * t57 + 0.9D1 / 0.2D1 * t61 - t92) * t2
     #9) * t9 * t51 / t11 / S12) * t17 * t20 + (-0.216D3 * t4 * t6 * t8 
     #* t9 + t138 * t1 + ((-0.432D3 + 0.216D3 * t140 * t6 + (-t46 - t43 
     #+ (t137 - t62 - t64 - t58 - t56 - t134 - t135 + t54 + t66 + t136) 
     #* t6) * t3) * t9 * t51 + t156 * t8 + (-t58 + t56 + t62 - t64 + t66
     # + t136 + t135 + t54 - t134 + t137) * t9) * t12) * s * z + t168 * 
     #t11 + (((-0.72D2 * t170 - t172 - t173) * t8 + 0.135D3 + t177) * S3
     #4 + (0.63D2 / 0.16D2 * t110 * t9 - 0.585D3 / 0.4D1 + (-t182 - t183
     # + t184) * t6 + (-t187 + t188 - t189) * t3) * t8 + (-0.585D3 / 0.4
     #D1 + (-t194 - t184 + t182) * t6 + (t189 - t188 - t197) * t3) * t9)
     # * S12 + ((-0.18D2 * t170 + 0.27D2 * t6 + 0.27D2 * t3) * t8 - 0.18
     #D2 + 0.27D2 * t110 * t9) * t108 + ((t215 * t6 + t219 * t3) * t8 + 
     #(t225 * t6 + t229 * t3) * t9) * S34 + ((-0.387D3 / 0.8D1 + (t235 -
     # t236 - t237 + t238) * t6 + (-t241 + t242 + t243 - t244) * t3) * t
     #9 - 0.99D2 * S13 - 0.99D2 * S14 + (-t251 + t252 + t253 + t254 - t2
     #55 + t256) * t6 + (t256 + t259 + t260 - t261 - t262 + t263) * t3) 
     #* t8 + (-0.99D2 * S24 - 0.99D2 * S23 + (t270 - t261 - t251 + t271 
     #+ t272 + t273) * t6 + (t276 - t255 + t270 + t277 - t262 + t278) * 
     #t3) * t9 + (((0.135D3 * t170 - t172 - t173) * t8 - 0.72D2 + t177) 
     #* t287 + ((t229 * t6 + t225 * t3) * t8 + (t219 * t6 + t215 * t3) *
     # t9) * t108 + t330 * S34 + t407 * t8 + t442 * t9) * t1 + (t168 * t
     #446 + ((0.585D3 / 0.4D1 * t37 * t9 + 0.63D2 / 0.16D2 + (-t183 - t1
     #88 + t184) * t6 + (-t184 - t187 + t188) * t3) * t8 + (0.63D2 / 0.1
     #6D2 + (-t194 - t189 + t182) * t6 + (t189 - t182 - t197) * t3) * t9
     #) * t287 + (((-0.387D3 / 0.8D1 + 0.99D2 * t140 * t6 + (-0.99D2 * S
     #24 - 0.99D2 * S14) * t3) * t9 - t241 + t243 - t236 + t235 + (t254 
     #- t471 + t92 + t252 - t255 + t278) * t6 + (-t261 + t259 + t79 + t2
     #71 - t471 + t260) * t3) * t8 + (t242 - t237 + t238 - t244 + (t92 -
     # t478 + t272 + t263 - t261 + t273) * t6 + (t276 - t255 - t478 + t2
     #77 + t79 + t253) * t3) * t9) * t108 + (t513 * t8 + t529 * t9) * S3
     #4 + t569 * t8 + t571 * t9) * t12
      rrgg2ggh11J1 = t575 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh11J2
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
      t4 = S34 * t3
      t5 = S12 + S14 + S24
      t6 = 0.1D1 / t5
      t8 = S13 + S14 + S34
      t9 = S23 + S24 + S34
      t11 = S12 ** 2
      t12 = 0.1D1 / t11
      t17 = s ** 2
      t20 = z ** 2
      t24 = t6 * t3 * t9
      t28 = -t6 - t3
      t29 = 0.216D3 * t28
      t33 = 0.216D3 * S13
      t34 = 0.216D3 * S14
      t35 = 0.432D3 * S34
      t37 = 0.216D3 * S24
      t42 = t8 * S34
      t44 = S13 * S24
      t45 = 0.36D2 * t44
      t46 = S24 ** 2
      t47 = 0.18D2 * t46
      t48 = S13 ** 2
      t49 = 0.18D2 * t48
      t50 = -t45 + t47 + t49
      t52 = S23 ** 2
      t53 = 0.18D2 * t52
      t54 = S14 ** 2
      t55 = 0.18D2 * t54
      t56 = S14 * S23
      t57 = 0.36D2 * t56
      t58 = t53 + t55 - t57
      t75 = S34 ** 2
      t77 = -t28
      t101 = S13 * S14
      t102 = 0.36D2 * t101
      t103 = S14 * S24
      t104 = 0.36D2 * t103
      t105 = S23 * S24
      t106 = 0.36D2 * t105
      t107 = S13 * S23
      t108 = 0.36D2 * t107
      t109 = -0.216D3 * t75 + ((0.1233D4 / 0.4D1 * t77 * t9 + 0.1233D4 /
     # 0.4D1 - 0.216D3 * S24 * t6 - 0.216D3 * S23 * t3) * t8 + (0.1233D4
     # / 0.4D1 - 0.216D3 * S14 * t6 - 0.216D3 * S13 * t3) * t9) * S34 + 
     #(-0.432D3 * S23 - 0.432D3 * S24 - t35 - t33 - t34) * t8 + (-0.216D
     #3 * S24 - 0.216D3 * S23) * t9 - t49 + t102 - t104 + t106 + t57 - t
     #47 - t55 + t45 - t53 - t108
      t111 = -S23 - S13
      t121 = 0.9D1 * t46
      t122 = 0.27D2 * t101
      t123 = 0.9D1 * t54
      t125 = 0.45D2 * t56
      t126 = 0.45D2 * t44
      t127 = 0.27D2 * t105
      t130 = 0.9D1 * t48
      t131 = 0.9D1 * t52
      t138 = 0.27D2 * t107
      t139 = 0.27D2 * t103
      t140 = ((-t121 + t122 + t49 - t123 - 0.54D2 * t103 + t125 + t126 +
     # t108 + t127 + t53) * t6 + (t127 + t104 - t130 + t55 + t126 + t47 
     #- t131 + t125 - 0.54D2 * t107 + t122) * t3) * t9 - t131 + t55 - t1
     #21 + t49 + t102 + t125 - 0.54D2 * t105 + t138 + t126 + t139
      t168 = 0.9D1 / 0.8D1 * t77
      t171 = t168 * t8 + t168 * t9
      t174 = 0.18D2 * t6
      t175 = 0.18D2 * t3
      t179 = 0.18D2 * t28 * t9
      t184 = 0.27D2 / 0.8D1 * S14
      t185 = 0.9D1 / 0.2D1 * S13
      t186 = 0.27D2 / 0.8D1 * S24
      t189 = 0.9D1 / 0.2D1 * S14
      t190 = 0.27D2 / 0.8D1 * S23
      t191 = 0.27D2 / 0.8D1 * S13
      t196 = 0.9D1 / 0.2D1 * S23
      t199 = 0.9D1 / 0.2D1 * S24
      t217 = 0.36D2 * S14 - 0.27D2 * S23
      t221 = 0.36D2 * S13 - 0.27D2 * S24
      t227 = -0.27D2 * S13 + 0.36D2 * S24
      t231 = -0.27D2 * S14 + 0.36D2 * S23
      t237 = 0.693D3 / 0.4D1 * S23
      t238 = 0.657D3 / 0.8D1 * S24
      t239 = 0.693D3 / 0.4D1 * S13
      t240 = 0.657D3 / 0.8D1 * S14
      t243 = 0.657D3 / 0.8D1 * S13
      t244 = 0.693D3 / 0.4D1 * S24
      t245 = 0.657D3 / 0.8D1 * S23
      t246 = 0.693D3 / 0.4D1 * S14
      t253 = 0.27D2 / 0.4D1 * t103
      t254 = 0.27D2 / 0.2D1 * t48
      t255 = 0.27D2 / 0.8D1 * t54
      t256 = 0.63D2 / 0.8D1 * t46
      t257 = 0.27D2 * t44
      t258 = 0.9D1 * t101
      t261 = 0.27D2 / 0.2D1 * t54
      t262 = 0.63D2 / 0.8D1 * t52
      t263 = 0.27D2 * t56
      t264 = 0.27D2 / 0.4D1 * t107
      t265 = 0.27D2 / 0.8D1 * t48
      t272 = 0.9D1 * t105
      t273 = 0.27D2 / 0.8D1 * t46
      t274 = 0.27D2 / 0.2D1 * t52
      t275 = 0.63D2 / 0.8D1 * t54
      t278 = 0.27D2 / 0.2D1 * t46
      t279 = 0.63D2 / 0.8D1 * t48
      t280 = 0.27D2 / 0.8D1 * t52
      t289 = t75 * S34
      t326 = ((0.2619D4 / 0.2D1 + (-t47 + t257 - t55 - t49 - t127 + t108
     # + t104 - t122 + t263 - t53) * t6 * t3) * t9 + (t263 + 0.108D3 * t
     #48 - t55 + t45 - 0.99D2 * t46 - t53) * t6 + (t57 + t257 - t47 + 0.
     #108D3 * t54 - 0.99D2 * t52 - t49) * t3) * t8 + ((t57 + 0.108D3 * t
     #52 - 0.99D2 * t54 + t257 - t49 - t47) * t6 + (-0.99D2 * t48 - t55 
     #+ t263 - t53 + 0.108D3 * t46 + t45) * t3) * t9 - t47 - t49 - t55 +
     # t257 - t53 - t139 - t138 + t102 + t263 + t106
      t328 = 0.3411D4 / 0.16D2 * S14
      t329 = 0.3411D4 / 0.16D2 * S24
      t330 = 0.3411D4 / 0.16D2 * S13
      t331 = 0.3411D4 / 0.16D2 * S23
      t332 = 0.531D3 / 0.4D1 * t101
      t333 = 0.531D3 / 0.4D1 * t44
      t335 = 0.531D3 / 0.4D1 * t56
      t336 = 0.243D3 / 0.4D1 * t54
      t337 = 0.243D3 / 0.4D1 * t46
      t338 = 0.531D3 / 0.4D1 * t105
      t341 = 0.243D3 / 0.4D1 * t48
      t342 = 0.243D3 / 0.4D1 * t52
      t348 = 0.45D2 * t107
      t349 = 0.9D1 * t56
      t351 = 0.45D2 * t103
      t352 = 0.9D1 * t44
      t353 = 0.45D2 / 0.16D2 * t52
      t354 = 0.171D3 / 0.8D1 * t54
      t355 = 0.45D2 / 0.16D2 * t46
      t356 = 0.171D3 / 0.8D1 * t48
      t357 = t101 * S24
      t358 = 0.27D2 * t357
      t359 = t54 * S24
      t361 = S14 * t46
      t363 = S13 * t46
      t364 = 0.117D3 / 0.2D1 * t363
      t365 = t48 * S13
      t366 = 0.18D2 * t365
      t367 = t54 * S14
      t368 = 0.9D1 / 0.8D1 * t367
      t369 = t46 * S24
      t370 = 0.189D3 / 0.8D1 * t369
      t371 = t48 * S14
      t373 = S13 * t54
      t375 = t48 * S24
      t376 = 0.135D3 / 0.2D1 * t375
      t379 = S14 * t52
      t380 = 0.117D3 / 0.2D1 * t379
      t381 = t48 * S23
      t383 = S13 * t52
      t386 = t54 * S23
      t387 = 0.135D3 / 0.2D1 * t386
      t388 = t101 * S23
      t389 = 0.27D2 * t388
      t391 = 0.9D1 / 0.8D1 * t365
      t392 = t52 * S23
      t393 = 0.189D3 / 0.8D1 * t392
      t394 = 0.18D2 * t367
      t397 = (t328 + t329 + t330 + t331 + (-t138 + t332 + t333 - 0.243D3
     # / 0.2D1 * t103 + t335 - t336 - t337 - t274 + t338 - t254) * t6 + 
     #(t333 - t341 - t139 - t278 - t342 - t261 + t338 + t335 - 0.243D3 /
     # 0.2D1 * t107 + t332) * t3) * t9 - t348 + t258 + t349 + 0.333D3 / 
     #0.8D1 * t105 - t351 + t352 - t353 - t354 - t355 - t356 + (t358 + 0
     #.27D2 / 0.8D1 * t359 - 0.63D2 / 0.8D1 * t361 - t364 - t366 - t368 
     #+ t370 - 0.27D2 / 0.2D1 * t371 - 0.9D1 / 0.2D1 * t373 + t376) * t6
     # + (-t380 + 0.27D2 / 0.8D1 * t381 - 0.63D2 / 0.8D1 * t383 - 0.27D2
     # / 0.2D1 * t373 + t387 + t389 - 0.9D1 / 0.2D1 * t371 - t391 + t393
     # - t394) * t3
      t399 = 0.171D3 / 0.8D1 * t46
      t400 = 0.45D2 / 0.16D2 * t48
      t401 = 0.171D3 / 0.8D1 * t52
      t403 = 0.45D2 / 0.16D2 * t54
      t404 = t56 * S24
      t405 = 0.27D2 * t404
      t406 = t52 * S24
      t410 = 0.9D1 / 0.8D1 * t369
      t411 = 0.135D3 / 0.2D1 * t379
      t412 = 0.18D2 * t392
      t413 = t46 * S23
      t415 = 0.117D3 / 0.2D1 * t386
      t416 = 0.189D3 / 0.8D1 * t367
      t420 = 0.117D3 / 0.2D1 * t375
      t421 = 0.135D3 / 0.2D1 * t363
      t423 = 0.9D1 / 0.8D1 * t392
      t424 = t44 * S23
      t425 = 0.27D2 * t424
      t428 = 0.189D3 / 0.8D1 * t365
      t429 = 0.18D2 * t369
      t432 = -t399 - t351 + t272 + t349 - t400 + t352 - t401 + 0.333D3 /
     # 0.8D1 * t101 - t348 - t403 + (t405 - 0.27D2 / 0.2D1 * t406 - 0.63
     #D2 / 0.8D1 * t359 + 0.27D2 / 0.8D1 * t361 - t410 + t411 - t412 - 0
     #.9D1 / 0.2D1 * t413 - t415 + t416) * t6 + (-0.27D2 / 0.2D1 * t413 
     #- t420 + t421 - 0.9D1 / 0.2D1 * t406 - t423 + t425 - 0.63D2 / 0.8D
     #1 * t381 + 0.27D2 / 0.8D1 * t383 + t428 - t429) * t3
      t436 = t75 ** 2
      t461 = 0.27D2 / 0.4D1 * t105
      t462 = 0.9D1 * t107
      t465 = 0.9D1 * t103
      t470 = 0.27D2 / 0.4D1 * t101
      t479 = 0.45D2 * t101
      t480 = 0.45D2 * t105
      t489 = 0.531D3 / 0.4D1 * t107
      t491 = 0.531D3 / 0.4D1 * t103
      t504 = (t328 + t329 + t330 + t331 + (t352 - t479 - t401 - t356 - t
     #355 + t462 - t480 + t349 + 0.333D3 / 0.8D1 * t103 - t403) * t6 + (
     #t349 - t479 - t353 - t354 + t465 + t352 - t400 - t399 + 0.333D3 / 
     #0.8D1 * t107 - t480) * t3) * t9 + t489 - t122 - 0.243D3 / 0.2D1 * 
     #t105 + t491 + t333 + t335 - t342 - t261 - t337 - t254 + (-t423 - t
     #366 + t425 - t364 - 0.27D2 / 0.2D1 * t381 + t376 - 0.63D2 / 0.8D1 
     #* t413 + 0.27D2 / 0.8D1 * t406 + t370 - 0.9D1 / 0.2D1 * t383) * t6
     # + (-0.27D2 / 0.2D1 * t359 - 0.63D2 / 0.8D1 * t406 + 0.27D2 / 0.8D
     #1 * t413 - t380 + t405 - t394 + t393 - t410 + t387 - 0.9D1 / 0.2D1
     # * t361) * t3
      t519 = -0.243D3 / 0.2D1 * t101 + t333 + t335 - t127 + t491 - t336 
     #- t278 - t341 - t274 + t489 + (-t412 - t391 + t389 - 0.63D2 / 0.8D
     #1 * t373 - 0.9D1 / 0.2D1 * t381 + 0.27D2 / 0.8D1 * t371 + t411 - t
     #415 + t416 - 0.27D2 / 0.2D1 * t383) * t6 + (-0.9D1 / 0.2D1 * t359 
     #- t420 + t421 + 0.27D2 / 0.8D1 * t373 + t358 + t428 - t368 - t429 
     #- 0.63D2 / 0.8D1 * t371 - 0.27D2 / 0.2D1 * t361) * t3
      t533 = 0.18D2 * t375
      t534 = 0.36D2 * t388
      t535 = 0.18D2 * t373
      t536 = 0.36D2 * t424
      t537 = 0.18D2 * t413
      t538 = 0.36D2 * t363
      t539 = 0.18D2 * t381
      t540 = 0.18D2 * t379
      t541 = 0.18D2 * t383
      t542 = 0.36D2 * t386
      t543 = t429 + t533 - t534 + t535 - t536 + t537 - t538 + t539 + t54
     #0 + t541 - t542 + t394
      t545 = 0.36D2 * t357
      t546 = 0.18D2 * t406
      t547 = 0.36D2 * t404
      t548 = 0.36D2 * t375
      t549 = 0.18D2 * t363
      t550 = 0.18D2 * t371
      t551 = 0.18D2 * t386
      t552 = 0.36D2 * t379
      t553 = 0.18D2 * t359
      t554 = 0.18D2 * t361
      t555 = -t545 + t546 - t547 - t548 + t549 + t366 + t550 + t551 - t5
     #52 + t553 + t554 + t412
      t557 = 0.72D2 * t101 + 0.72D2 * t107 + 0.171D3 * t44 + 0.171D3 * t
     #56 + 0.72D2 * t105 + 0.72D2 * t103 - 0.54D2 * t46 - 0.54D2 * t48 -
     # 0.54D2 * t52 - 0.54D2 * t54 + t543 * t6 + t555 * t3
      t559 = t557 * t9 + t541 + t533 + t429 + t412 - t534 + t554 - t538 
     #- t545 - t552 + t535 + t551 + t550
      t561 = t366 + t394 + t553 - t542 - t548 + t549 + t539 + t537 + t54
     #0 + t546 - t547 - t536
      t565 = (-0.72D2 * t1 - 0.72D2 * t4 * t6 * t8 * t9 * t12) * t17 * s
     # * t20 * z + ((((-0.216D3 * t24 - 0.216D3 * t6 - 0.216D3 * t3) * t
     #8 - 0.216D3 + t29 * t9) * S34 + t33 + t34 + t35 + 0.216D3 * S23 + 
     #t37) * t1 + (-t29 * t9 * t42 + (t50 * t6 + t58 * t3) * t8 + (t58 *
     # t6 + t50 * t3) * t9) * t12) * t17 * t20 + (-0.216D3 * t4 * t6 * t
     #8 * t9 + t109 * t1 + ((-0.432D3 + 0.216D3 * t111 * t6 + (-t37 - t3
     #4 + (t104 - t53 - t55 - t49 - t47 - t102 - t106 + t45 + t57 + t108
     #) * t6) * t3) * t9 * t42 + t140 * t8 + (-t123 + t53 + t47 - t130 +
     # t125 + t138 + t106 + t139 - 0.54D2 * t101 + t126) * t9) * t12 + (
     #(-t123 - t121 + 0.18D2 * t103) * t6 * t2 - 0.18D2 * t56 + 0.18D2 *
     # t101 + 0.18D2 * t105 - 0.18D2 * t44 + (-t131 + 0.18D2 * t107 - t1
     #30) * t5 * t3) * t9 * t8 / t11 / S12) * s * z + t171 * t11 + (((-0
     #.99D2 * t24 - t174 - t175) * t8 + 0.108D3 + t179) * S34 + (0.171D3
     # / 0.8D1 * t28 * t9 - 0.585D3 / 0.4D1 + (-t184 - t185 + t186) * t6
     # + (-t189 + t190 - t191) * t3) * t8 + (-0.585D3 / 0.4D1 + (-t196 -
     # t186 + t184) * t6 + (t191 - t190 - t199) * t3) * t9) * S12 + ((0.
     #36D2 * t24 + 0.27D2 * t6 + 0.27D2 * t3) * t8 + 0.36D2 + 0.27D2 * t
     #77 * t9) * t75 + ((t217 * t6 + t221 * t3) * t8 + (t227 * t6 + t231
     # * t3) * t9) * S34 + ((0.9D1 / 0.4D1 + (t237 - t238 + t239 - t240)
     # * t6 + (-t243 + t244 - t245 + t246) * t3) * t9 - 0.99D2 * S13 - 0
     #.99D2 * S14 + (-t253 + t254 + t255 + t256 - t257 + t258) * t6 + (t
     #258 + t261 + t262 - t263 - t264 + t265) * t3) * t8 + (-0.99D2 * S2
     #4 - 0.99D2 * S23 + (t272 - t263 - t253 + t273 + t274 + t275) * t6 
     #+ (t278 - t257 + t272 + t279 - t264 + t280) * t3) * t9 + (((0.108D
     #3 * t24 - t174 - t175) * t8 - 0.99D2 + t179) * t289 + ((t231 * t6 
     #+ t227 * t3) * t8 + (t221 * t6 + t217 * t3) * t9) * t75 + t326 * S
     #34 + t397 * t8 + t432 * t9) * t1 + (t171 * t436 + ((0.585D3 / 0.4D
     #1 * t28 * t9 - 0.171D3 / 0.8D1 + (-t185 - t190 + t186) * t6 + (-t1
     #86 - t189 + t190) * t3) * t8 + (-0.171D3 / 0.8D1 + (-t196 - t191 +
     # t184) * t6 + (t191 - t184 - t199) * t3) * t9) * t289 + (((0.9D1 /
     # 0.4D1 + 0.99D2 * t111 * t6 + (-0.99D2 * S24 - 0.99D2 * S14) * t3)
     # * t9 - t238 - t245 + t239 + t246 + (t256 - t461 + t462 + t254 - t
     #257 + t280) * t6 + (-t263 + t261 + t465 + t273 - t461 + t262) * t3
     #) * t8 + (-t240 - t243 + t244 + t237 + (t462 - t470 + t274 + t265 
     #- t263 + t275) * t6 + (t278 - t257 - t470 + t279 + t465 + t255) * 
     #t3) * t9) * t75 + (t504 * t8 + t519 * t9) * S34 + t559 * t8 + t561
     # * t9) * t12
      rrgg2ggh11J2 = t565 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh11J3
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
      t4 = S34 * t3
      t5 = S12 + S14 + S24
      t6 = 0.1D1 / t5
      t8 = S13 + S14 + S34
      t9 = S23 + S24 + S34
      t11 = S12 ** 2
      t12 = 0.1D1 / t11
      t17 = s ** 2
      t20 = z ** 2
      t24 = t6 * t3 * t9
      t28 = -t6 - t3
      t29 = 0.216D3 * t28
      t33 = 0.216D3 * S13
      t34 = 0.216D3 * S14
      t35 = 0.432D3 * S34
      t37 = 0.216D3 * S24
      t42 = t8 * S34
      t44 = S13 * S24
      t45 = 0.36D2 * t44
      t46 = S13 ** 2
      t47 = 0.18D2 * t46
      t48 = S24 ** 2
      t49 = 0.54D2 * t48
      t52 = S14 ** 2
      t53 = 0.18D2 * t52
      t54 = S23 ** 2
      t55 = 0.54D2 * t54
      t56 = S14 * S23
      t57 = 0.36D2 * t56
      t62 = 0.18D2 * t54
      t63 = 0.54D2 * t52
      t66 = 0.54D2 * t46
      t67 = 0.18D2 * t48
      t81 = S34 ** 2
      t83 = -t28
      t107 = S13 * S14
      t108 = 0.36D2 * t107
      t109 = S14 * S24
      t110 = 0.36D2 * t109
      t111 = S23 * S24
      t112 = 0.36D2 * t111
      t113 = S13 * S23
      t114 = 0.36D2 * t113
      t115 = -0.216D3 * t81 + ((0.801D3 / 0.4D1 * t83 * t9 + 0.801D3 / 0
     #.4D1 - 0.216D3 * S24 * t6 - 0.216D3 * S23 * t3) * t8 + (0.801D3 / 
     #0.4D1 - 0.216D3 * S14 * t6 - 0.216D3 * S13 * t3) * t9) * S34 + (-0
     #.432D3 * S23 - 0.432D3 * S24 - t35 - t33 - t34) * t8 + (-0.216D3 *
     # S24 - 0.216D3 * S23) * t9 - t47 + t108 - t110 + t112 + t57 - t67 
     #- t53 + t45 - t62 - t114
      t117 = -S23 - S13
      t127 = 0.27D2 * t107
      t128 = 0.108D3 * t109
      t129 = 0.45D2 * t56
      t130 = 0.45D2 * t44
      t131 = 0.27D2 * t111
      t134 = 0.108D3 * t113
      t139 = 0.108D3 * t111
      t140 = 0.27D2 * t113
      t141 = 0.27D2 * t109
      t142 = ((-t49 + t127 + t47 - t63 - t128 + t129 + t130 + t114 + t13
     #1 + t62) * t6 + (t131 + t110 - t66 + t53 + t130 + t67 - t55 + t129
     # - t134 + t127) * t3) * t9 - t55 + t53 - t49 + t47 + t108 + t129 -
     # t139 + t140 + t130 + t141
      t144 = 0.108D3 * t107
      t155 = 0.18D2 * t56
      t158 = 0.18D2 * t44
      t174 = 0.9D1 / 0.8D1 * t83
      t177 = t174 * t8 + t174 * t9
      t180 = 0.18D2 * t6
      t181 = 0.18D2 * t3
      t185 = 0.18D2 * t28 * t9
      t190 = 0.27D2 / 0.8D1 * S14
      t191 = 0.9D1 / 0.2D1 * S13
      t192 = 0.27D2 / 0.8D1 * S24
      t195 = 0.9D1 / 0.2D1 * S14
      t196 = 0.27D2 / 0.8D1 * S23
      t197 = 0.27D2 / 0.8D1 * S13
      t202 = 0.9D1 / 0.2D1 * S23
      t205 = 0.9D1 / 0.2D1 * S24
      t223 = 0.36D2 * S14 - 0.27D2 * S23
      t227 = 0.36D2 * S13 - 0.27D2 * S24
      t233 = -0.27D2 * S13 + 0.36D2 * S24
      t237 = -0.27D2 * S14 + 0.36D2 * S23
      t243 = 0.4131D4 / 0.16D2 * S23
      t244 = 0.2169D4 / 0.16D2 * S24
      t245 = 0.4131D4 / 0.16D2 * S13
      t246 = 0.2169D4 / 0.16D2 * S14
      t249 = 0.2169D4 / 0.16D2 * S13
      t250 = 0.4131D4 / 0.16D2 * S24
      t251 = 0.2169D4 / 0.16D2 * S23
      t252 = 0.4131D4 / 0.16D2 * S14
      t259 = 0.27D2 / 0.4D1 * t109
      t260 = 0.27D2 / 0.2D1 * t46
      t261 = 0.27D2 / 0.8D1 * t52
      t262 = 0.63D2 / 0.8D1 * t48
      t263 = 0.27D2 * t44
      t264 = 0.9D1 * t107
      t267 = 0.27D2 / 0.2D1 * t52
      t268 = 0.63D2 / 0.8D1 * t54
      t269 = 0.27D2 * t56
      t270 = 0.27D2 / 0.4D1 * t113
      t271 = 0.27D2 / 0.8D1 * t46
      t278 = 0.9D1 * t111
      t279 = 0.27D2 / 0.8D1 * t48
      t280 = 0.27D2 / 0.2D1 * t54
      t281 = 0.63D2 / 0.8D1 * t52
      t284 = 0.27D2 / 0.2D1 * t48
      t285 = 0.63D2 / 0.8D1 * t46
      t286 = 0.27D2 / 0.8D1 * t54
      t295 = t81 * S34
      t313 = 0.108D3 * t44
      t317 = 0.108D3 * t56
      t334 = ((0.2403D4 / 0.2D1 + (-t67 + t263 - t53 - t47 - t131 + t114
     # + t110 - t127 + t269 - t62) * t6 * t3) * t9 + (t269 + 0.180D3 * t
     #46 - t53 - t313 - 0.27D2 * t48 - t62) * t6 + (-t317 + t263 - t67 +
     # 0.180D3 * t52 - 0.27D2 * t54 - t47) * t3) * t8 + ((-t317 + 0.180D
     #3 * t54 - 0.27D2 * t52 + t263 - t47 - t67) * t6 + (-0.27D2 * t46 -
     # t53 + t269 - t62 + 0.180D3 * t48 - t313) * t3) * t9 - t67 - t47 -
     # t53 + t263 - t62 - t141 - t140 + t108 + t269 + t112
      t336 = 0.1431D4 / 0.8D1 * S14
      t337 = 0.1431D4 / 0.8D1 * S24
      t338 = 0.1431D4 / 0.8D1 * S13
      t339 = 0.1431D4 / 0.8D1 * S23
      t340 = 0.3627D4 / 0.16D2 * t107
      t341 = 0.3339D4 / 0.16D2 * t44
      t343 = 0.3339D4 / 0.16D2 * t56
      t344 = 0.927D3 / 0.8D1 * t52
      t345 = 0.927D3 / 0.8D1 * t48
      t346 = 0.3627D4 / 0.16D2 * t111
      t349 = 0.927D3 / 0.8D1 * t46
      t350 = 0.927D3 / 0.8D1 * t54
      t356 = 0.54D2 * t113
      t358 = 0.54D2 * t109
      t359 = 0.243D3 / 0.16D2 * t54
      t360 = 0.171D3 / 0.8D1 * t52
      t361 = 0.243D3 / 0.16D2 * t48
      t362 = 0.171D3 / 0.8D1 * t46
      t363 = t48 * S24
      t364 = 0.333D3 / 0.8D1 * t363
      t365 = t48 * S13
      t366 = 0.189D3 / 0.2D1 * t365
      t367 = S14 * t52
      t368 = 0.9D1 / 0.8D1 * t367
      t369 = S13 * t52
      t371 = t52 * S24
      t373 = t48 * S14
      t375 = t107 * S24
      t376 = 0.27D2 * t375
      t377 = S14 * t46
      t379 = S13 * t46
      t380 = 0.18D2 * t379
      t381 = t46 * S24
      t382 = 0.171D3 / 0.2D1 * t381
      t387 = 0.18D2 * t367
      t388 = t54 * S23
      t389 = 0.333D3 / 0.8D1 * t388
      t390 = t46 * S23
      t392 = S13 * t54
      t394 = t52 * S23
      t395 = 0.171D3 / 0.2D1 * t394
      t396 = t54 * S14
      t397 = 0.189D3 / 0.2D1 * t396
      t398 = 0.9D1 / 0.8D1 * t379
      t399 = t107 * S23
      t400 = 0.27D2 * t399
      t403 = (t336 + t337 + t338 + t339 + (-t134 + t340 + t341 - 0.1215D
     #4 / 0.4D1 * t109 + t343 - t344 - t345 - t55 + t346 - t66) * t6 + (
     #t341 - t349 - t128 - t49 - t350 - t63 + t346 + t343 - 0.1215D4 / 0
     #.4D1 * t113 + t340) * t3) * t9 - t356 + t264 + t155 + 0.621D3 / 0.
     #8D1 * t111 - t358 + t158 + t359 - t360 + t361 - t362 + (t364 - t36
     #6 - t368 - 0.9D1 / 0.2D1 * t369 + 0.27D2 / 0.8D1 * t371 - 0.63D2 /
     # 0.8D1 * t373 + t376 - 0.27D2 / 0.2D1 * t377 - t380 + t382) * t6 +
     # (-0.9D1 / 0.2D1 * t377 - 0.27D2 / 0.2D1 * t369 - t387 + t389 + 0.
     #27D2 / 0.8D1 * t390 - 0.63D2 / 0.8D1 * t392 + t395 - t397 - t398 +
     # t400) * t3
      t406 = 0.171D3 / 0.8D1 * t54
      t407 = 0.243D3 / 0.16D2 * t52
      t408 = 0.171D3 / 0.8D1 * t48
      t409 = 0.243D3 / 0.16D2 * t46
      t411 = t56 * S24
      t412 = 0.27D2 * t411
      t413 = 0.18D2 * t388
      t415 = 0.9D1 / 0.8D1 * t363
      t416 = 0.189D3 / 0.2D1 * t394
      t417 = 0.171D3 / 0.2D1 * t396
      t418 = 0.333D3 / 0.8D1 * t367
      t419 = t48 * S23
      t421 = t54 * S24
      t427 = 0.9D1 / 0.8D1 * t388
      t430 = t44 * S23
      t431 = 0.27D2 * t430
      t432 = 0.189D3 / 0.2D1 * t381
      t433 = 0.18D2 * t363
      t434 = 0.333D3 / 0.8D1 * t379
      t435 = 0.171D3 / 0.2D1 * t365
      t438 = -t356 + 0.621D3 / 0.8D1 * t107 + t278 - t358 + t158 + t155 
     #- t406 + t407 - t408 + t409 + (0.27D2 / 0.8D1 * t373 + t412 - t413
     # - 0.63D2 / 0.8D1 * t371 - t415 - t416 + t417 + t418 - 0.9D1 / 0.2
     #D1 * t419 - 0.27D2 / 0.2D1 * t421) * t6 + (-0.9D1 / 0.2D1 * t421 -
     # 0.27D2 / 0.2D1 * t419 - t427 - 0.63D2 / 0.8D1 * t390 + 0.27D2 / 0
     #.8D1 * t392 + t431 - t432 - t433 + t434 + t435) * t3
      t442 = t81 ** 2
      t467 = 0.27D2 / 0.4D1 * t111
      t468 = 0.9D1 * t113
      t471 = 0.9D1 * t109
      t476 = 0.27D2 / 0.4D1 * t107
      t485 = 0.54D2 * t107
      t486 = 0.54D2 * t111
      t495 = 0.3627D4 / 0.16D2 * t113
      t497 = 0.3627D4 / 0.16D2 * t109
      t510 = (t336 + t337 + t338 + t339 + (-t485 + t468 - t486 + t158 + 
     #t407 + t361 + 0.621D3 / 0.8D1 * t109 + t155 - t362 - t406) * t6 + 
     #(t409 + t158 - t408 + t471 + t155 + 0.621D3 / 0.8D1 * t113 - t486 
     #- t485 - t360 + t359) * t3) * t9 + t495 - t144 - 0.1215D4 / 0.4D1 
     #* t111 + t497 + t341 + t343 - t350 - t63 - t345 - t66 + (t382 - t4
     #27 - t366 + t364 + t431 - 0.27D2 / 0.2D1 * t390 - 0.9D1 / 0.2D1 * 
     #t392 + 0.27D2 / 0.8D1 * t421 - 0.63D2 / 0.8D1 * t419 - t380) * t6 
     #+ (t412 - t387 + t389 + t395 - t397 - t415 - 0.27D2 / 0.2D1 * t371
     # - 0.9D1 / 0.2D1 * t373 - 0.63D2 / 0.8D1 * t421 + 0.27D2 / 0.8D1 *
     # t419) * t3
      t525 = -0.1215D4 / 0.4D1 * t107 + t341 + t343 - t139 + t497 - t344
     # - t49 - t349 - t55 + t495 + (0.27D2 / 0.8D1 * t377 - 0.63D2 / 0.8
     #D1 * t369 - t413 + t418 - t416 - 0.27D2 / 0.2D1 * t392 + t400 - t3
     #98 - 0.9D1 / 0.2D1 * t390 + t417) * t6 + (t435 - t368 - 0.63D2 / 0
     #.8D1 * t377 + 0.27D2 / 0.8D1 * t369 - t432 - t433 - 0.9D1 / 0.2D1 
     #* t371 - 0.27D2 / 0.2D1 * t373 + t434 + t376) * t3
      t539 = 0.36D2 * t365
      t541 = 0.54D2 * t363
      t544 = 0.54D2 * t367
      t545 = 0.36D2 * t394
      t546 = 0.18D2 * t381
      t548 = 0.36D2 * t399
      t549 = 0.36D2 * t430
      t550 = 0.18D2 * t396
      t551 = -t539 + 0.54D2 * t369 + t541 + 0.18D2 * t390 + 0.18D2 * t39
     #2 + t544 - t545 + t546 + 0.54D2 * t419 - t548 - t549 + t550
      t553 = 0.36D2 * t411
      t554 = 0.54D2 * t379
      t555 = 0.54D2 * t388
      t557 = 0.18D2 * t394
      t558 = 0.36D2 * t396
      t559 = 0.36D2 * t375
      t560 = 0.36D2 * t381
      t561 = 0.18D2 * t365
      t565 = -t553 + t554 + t555 + 0.54D2 * t377 + t557 - t558 - t559 - 
     #t560 + t561 + 0.18D2 * t371 + 0.18D2 * t373 + 0.54D2 * t421
      t567 = 0.162D3 * t107 + 0.162D3 * t113 + 0.333D3 * t44 + 0.333D3 *
     # t56 + 0.162D3 * t111 + 0.162D3 * t109 - 0.45D2 * t48 - 0.45D2 * t
     #46 - 0.45D2 * t54 - 0.45D2 * t52 + t551 * t6 + t565 * t3
      t573 = t567 * t9 + 0.54D2 * t392 + t546 + t541 + t555 - t548 + 0.5
     #4D2 * t373 - t539 - t559 - t558 + 0.18D2 * t369 + t557 + 0.18D2 * 
     #t377
      t579 = -t553 + t554 + t544 + t550 + 0.18D2 * t421 + 0.54D2 * t390 
     #+ 0.18D2 * t419 - t545 - t549 + t561 + 0.54D2 * t371 - t560
      t583 = (-0.72D2 * t1 - 0.72D2 * t4 * t6 * t8 * t9 * t12) * t17 * s
     # * t20 * z + ((((-0.216D3 * t24 - 0.216D3 * t6 - 0.216D3 * t3) * t
     #8 - 0.216D3 + t29 * t9) * S34 + t33 + t34 + t35 + 0.216D3 * S23 + 
     #t37) * t1 + (-t29 * t9 * t42 + ((-t45 + t47 + t49) * t6 + (t53 + t
     #55 - t57) * t3) * t8 + ((-t57 + t62 + t63) * t6 + (-t45 + t66 + t6
     #7) * t3) * t9) * t12) * t17 * t20 + (-0.216D3 * t4 * t6 * t8 * t9 
     #+ t115 * t1 + ((-0.432D3 + 0.216D3 * t117 * t6 + (-t37 - t34 + (t1
     #10 - t62 - t53 - t47 - t67 - t108 - t112 + t45 + t57 + t114) * t6)
     # * t3) * t9 * t42 + t142 * t8 + (-t63 + t62 + t67 - t66 + t129 + t
     #140 + t112 + t141 - t144 + t130) * t9) * t12 + ((-0.9D1 * t52 - 0.
     #9D1 * t48 + 0.18D2 * t109) * t6 * t2 - t155 + 0.18D2 * t107 + 0.18
     #D2 * t111 - t158 + (-0.9D1 * t54 + 0.18D2 * t113 - 0.9D1 * t46) * 
     #t5 * t3) * t9 * t8 / t11 / S12) * s * z + t177 * t11 + (((-0.27D2 
     #* t24 - t180 - t181) * t8 + 0.180D3 + t185) * S34 + (0.27D2 / 0.16
     #D2 * t28 * t9 - 0.585D3 / 0.4D1 + (-t190 - t191 + t192) * t6 + (-t
     #195 + t196 - t197) * t3) * t8 + (-0.585D3 / 0.4D1 + (-t202 - t192 
     #+ t190) * t6 + (t197 - t196 - t205) * t3) * t9) * S12 + ((-0.108D3
     # * t24 + 0.27D2 * t6 + 0.27D2 * t3) * t8 - 0.108D3 + 0.27D2 * t83 
     #* t9) * t81 + ((t223 * t6 + t227 * t3) * t8 + (t233 * t6 + t237 * 
     #t3) * t9) * S34 + ((-0.297D3 / 0.8D1 + (t243 - t244 + t245 - t246)
     # * t6 + (-t249 + t250 - t251 + t252) * t3) * t9 - 0.99D2 * S13 - 0
     #.99D2 * S14 + (-t259 + t260 + t261 + t262 - t263 + t264) * t6 + (t
     #264 + t267 + t268 - t269 - t270 + t271) * t3) * t8 + (-0.99D2 * S2
     #4 - 0.99D2 * S23 + (t278 - t269 - t259 + t279 + t280 + t281) * t6 
     #+ (t284 - t263 + t278 + t285 - t270 + t286) * t3) * t9 + (((0.180D
     #3 * t24 - t180 - t181) * t8 - 0.27D2 + t185) * t295 + ((t237 * t6 
     #+ t233 * t3) * t8 + (t227 * t6 + t223 * t3) * t9) * t81 + t334 * S
     #34 + t403 * t8 + t438 * t9) * t1 + (t177 * t442 + ((0.585D3 / 0.4D
     #1 * t28 * t9 - 0.27D2 / 0.16D2 + (-t191 - t196 + t192) * t6 + (-t1
     #92 - t195 + t196) * t3) * t8 + (-0.27D2 / 0.16D2 + (-t202 - t197 +
     # t190) * t6 + (t197 - t190 - t205) * t3) * t9) * t295 + (((-0.297D
     #3 / 0.8D1 + 0.99D2 * t117 * t6 + (-0.99D2 * S24 - 0.99D2 * S14) * 
     #t3) * t9 - t244 - t251 + t245 + t252 + (t262 - t467 + t468 + t260 
     #- t263 + t286) * t6 + (-t269 + t267 + t471 + t279 - t467 + t268) *
     # t3) * t8 + (-t246 - t249 + t250 + t243 + (t468 - t476 + t280 + t2
     #71 - t269 + t281) * t6 + (t284 - t263 - t476 + t285 + t471 + t261)
     # * t3) * t9) * t81 + (t510 * t8 + t525 * t9) * S34 + t573 * t8 + t
     #579 * t9) * t12
      rrgg2ggh11J3 = t583 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh11J4
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
      t4 = S34 * t3
      t5 = S12 + S14 + S24
      t6 = 0.1D1 / t5
      t8 = S13 + S14 + S34
      t9 = S23 + S24 + S34
      t11 = S12 ** 2
      t12 = 0.1D1 / t11
      t17 = s ** 2
      t20 = z ** 2
      t24 = t6 * t3 * t9
      t28 = -t6 - t3
      t29 = 0.216D3 * t28
      t33 = 0.216D3 * S13
      t34 = 0.216D3 * S14
      t35 = 0.432D3 * S34
      t37 = 0.216D3 * S24
      t42 = t8 * S34
      t44 = S13 * S24
      t45 = 0.36D2 * t44
      t46 = S13 ** 2
      t47 = 0.18D2 * t46
      t48 = S24 ** 2
      t52 = S14 ** 2
      t53 = 0.18D2 * t52
      t54 = S23 ** 2
      t56 = S14 * S23
      t57 = 0.36D2 * t56
      t62 = 0.18D2 * t54
      t67 = 0.18D2 * t48
      t81 = S34 ** 2
      t83 = -t28
      t107 = S13 * S14
      t108 = 0.36D2 * t107
      t109 = S14 * S24
      t110 = 0.36D2 * t109
      t111 = S23 * S24
      t112 = 0.36D2 * t111
      t113 = S13 * S23
      t114 = 0.36D2 * t113
      t115 = -0.216D3 * t81 + ((0.369D3 / 0.4D1 * t83 * t9 + 0.369D3 / 0
     #.4D1 - 0.216D3 * S24 * t6 - 0.216D3 * S23 * t3) * t8 + (0.369D3 / 
     #0.4D1 - 0.216D3 * S14 * t6 - 0.216D3 * S13 * t3) * t9) * S34 + (-0
     #.432D3 * S23 - 0.432D3 * S24 - t35 - t33 - t34) * t8 + (-0.216D3 *
     # S24 - 0.216D3 * S23) * t9 - t47 + t108 - t110 + t112 + t57 - t67 
     #- t53 + t45 - t62 - t114
      t117 = -S23 - S13
      t127 = 0.99D2 * t48
      t128 = 0.27D2 * t107
      t129 = 0.99D2 * t52
      t131 = 0.45D2 * t56
      t132 = 0.45D2 * t44
      t133 = 0.27D2 * t111
      t136 = 0.99D2 * t46
      t137 = 0.99D2 * t54
      t144 = 0.27D2 * t113
      t145 = 0.27D2 * t109
      t146 = ((-t127 + t128 + t47 - t129 - 0.162D3 * t109 + t131 + t132 
     #+ t114 + t133 + t62) * t6 + (t133 + t110 - t136 + t53 + t132 + t67
     # - t137 + t131 - 0.162D3 * t113 + t128) * t3) * t9 - t137 + t53 - 
     #t127 + t47 + t108 + t131 - 0.162D3 * t111 + t144 + t132 + t145
      t178 = 0.9D1 / 0.8D1 * t83
      t181 = t178 * t8 + t178 * t9
      t184 = 0.18D2 * t6
      t185 = 0.18D2 * t3
      t188 = 0.18D2 * t28
      t189 = t188 * t9
      t194 = 0.27D2 / 0.8D1 * S14
      t195 = 0.9D1 / 0.2D1 * S13
      t196 = 0.27D2 / 0.8D1 * S24
      t199 = 0.9D1 / 0.2D1 * S14
      t200 = 0.27D2 / 0.8D1 * S23
      t201 = 0.27D2 / 0.8D1 * S13
      t206 = 0.9D1 / 0.2D1 * S23
      t209 = 0.9D1 / 0.2D1 * S24
      t216 = 0.252D3 * t24
      t227 = 0.36D2 * S14 - 0.27D2 * S23
      t231 = 0.36D2 * S13 - 0.27D2 * S24
      t237 = -0.27D2 * S13 + 0.36D2 * S24
      t241 = -0.27D2 * S14 + 0.36D2 * S23
      t247 = 0.2745D4 / 0.8D1 * S23
      t248 = 0.189D3 * S24
      t249 = 0.2745D4 / 0.8D1 * S13
      t250 = 0.189D3 * S14
      t253 = 0.189D3 * S13
      t254 = 0.2745D4 / 0.8D1 * S24
      t255 = 0.189D3 * S23
      t256 = 0.2745D4 / 0.8D1 * S14
      t263 = 0.27D2 / 0.4D1 * t109
      t264 = 0.27D2 / 0.2D1 * t46
      t265 = 0.27D2 / 0.8D1 * t52
      t266 = 0.63D2 / 0.8D1 * t48
      t267 = 0.27D2 * t44
      t268 = 0.9D1 * t107
      t271 = 0.27D2 / 0.2D1 * t52
      t272 = 0.63D2 / 0.8D1 * t54
      t273 = 0.27D2 * t56
      t274 = 0.27D2 / 0.4D1 * t113
      t275 = 0.27D2 / 0.8D1 * t46
      t282 = 0.9D1 * t111
      t283 = 0.27D2 / 0.8D1 * t48
      t284 = 0.27D2 / 0.2D1 * t54
      t285 = 0.63D2 / 0.8D1 * t52
      t288 = 0.27D2 / 0.2D1 * t48
      t289 = 0.63D2 / 0.8D1 * t46
      t290 = 0.27D2 / 0.8D1 * t54
      t298 = t81 * S34
      t316 = 0.252D3 * t44
      t320 = 0.252D3 * t56
      t337 = ((0.2187D4 / 0.2D1 + (-t67 + t267 - t53 - t47 - t133 + t114
     # + t110 - t128 + t273 - t62) * t6 * t3) * t9 + (t273 + 0.252D3 * t
     #46 - t53 - t316 + 0.45D2 * t48 - t62) * t6 + (-t320 + t267 - t67 +
     # 0.252D3 * t52 + 0.45D2 * t54 - t47) * t3) * t8 + ((-t320 + 0.252D
     #3 * t54 + 0.45D2 * t52 + t267 - t47 - t67) * t6 + (0.45D2 * t46 - 
     #t53 + t273 - t62 + 0.252D3 * t48 - t316) * t3) * t9 - t67 - t47 - 
     #t53 + t267 - t62 - t145 - t144 + t108 + t273 + t112
      t339 = 0.2313D4 / 0.16D2 * S14
      t340 = 0.2313D4 / 0.16D2 * S24
      t341 = 0.2313D4 / 0.16D2 * S13
      t342 = 0.2313D4 / 0.16D2 * S23
      t344 = 0.2565D4 / 0.8D1 * t107
      t345 = 0.2277D4 / 0.8D1 * t44
      t347 = 0.2277D4 / 0.8D1 * t56
      t348 = 0.171D3 * t52
      t349 = 0.171D3 * t48
      t350 = 0.189D3 / 0.2D1 * t54
      t351 = 0.2565D4 / 0.8D1 * t111
      t352 = 0.189D3 / 0.2D1 * t46
      t355 = 0.171D3 * t46
      t357 = 0.189D3 / 0.2D1 * t48
      t358 = 0.171D3 * t54
      t359 = 0.189D3 / 0.2D1 * t52
      t365 = 0.63D2 * t113
      t367 = 0.63D2 * t109
      t368 = 0.531D3 / 0.16D2 * t54
      t369 = 0.171D3 / 0.8D1 * t52
      t370 = 0.531D3 / 0.16D2 * t48
      t371 = 0.171D3 / 0.8D1 * t46
      t372 = t48 * S24
      t373 = 0.477D3 / 0.8D1 * t372
      t374 = t48 * S13
      t375 = 0.261D3 / 0.2D1 * t374
      t376 = S14 * t52
      t377 = 0.9D1 / 0.8D1 * t376
      t378 = S13 * t52
      t380 = t52 * S24
      t382 = t48 * S14
      t384 = t107 * S24
      t385 = 0.27D2 * t384
      t386 = S14 * t46
      t388 = S13 * t46
      t389 = 0.18D2 * t388
      t390 = t46 * S24
      t391 = 0.207D3 / 0.2D1 * t390
      t396 = 0.18D2 * t376
      t397 = t54 * S23
      t398 = 0.477D3 / 0.8D1 * t397
      t399 = t46 * S23
      t401 = S13 * t54
      t403 = t52 * S23
      t404 = 0.207D3 / 0.2D1 * t403
      t405 = t54 * S14
      t406 = 0.261D3 / 0.2D1 * t405
      t407 = 0.9D1 / 0.8D1 * t388
      t408 = t107 * S23
      t409 = 0.27D2 * t408
      t412 = (t339 + t340 + t341 + t342 + (-0.189D3 * t113 + t344 + t345
     # - 0.486D3 * t109 + t347 - t348 - t349 - t350 + t351 - t352) * t6 
     #+ (t345 - t355 - 0.189D3 * t109 - t357 - t358 - t359 + t351 + t347
     # - 0.486D3 * t113 + t344) * t3) * t9 - t365 + t268 + t273 + 0.909D
     #3 / 0.8D1 * t111 - t367 + t267 + t368 - t369 + t370 - t371 + (t373
     # - t375 - t377 - 0.9D1 / 0.2D1 * t378 + 0.27D2 / 0.8D1 * t380 - 0.
     #63D2 / 0.8D1 * t382 + t385 - 0.27D2 / 0.2D1 * t386 - t389 + t391) 
     #* t6 + (-0.9D1 / 0.2D1 * t386 - 0.27D2 / 0.2D1 * t378 - t396 + t39
     #8 + 0.27D2 / 0.8D1 * t399 - 0.63D2 / 0.8D1 * t401 + t404 - t406 - 
     #t407 + t409) * t3
      t415 = 0.171D3 / 0.8D1 * t54
      t416 = 0.531D3 / 0.16D2 * t52
      t417 = 0.171D3 / 0.8D1 * t48
      t418 = 0.531D3 / 0.16D2 * t46
      t420 = t56 * S24
      t421 = 0.27D2 * t420
      t422 = 0.18D2 * t397
      t424 = 0.9D1 / 0.8D1 * t372
      t425 = 0.261D3 / 0.2D1 * t403
      t426 = 0.207D3 / 0.2D1 * t405
      t427 = 0.477D3 / 0.8D1 * t376
      t428 = t48 * S23
      t430 = t54 * S24
      t436 = 0.9D1 / 0.8D1 * t397
      t439 = t44 * S23
      t440 = 0.27D2 * t439
      t441 = 0.261D3 / 0.2D1 * t390
      t442 = 0.18D2 * t372
      t443 = 0.477D3 / 0.8D1 * t388
      t444 = 0.207D3 / 0.2D1 * t374
      t447 = -t365 + 0.909D3 / 0.8D1 * t107 + t282 - t367 + t267 + t273 
     #- t415 + t416 - t417 + t418 + (0.27D2 / 0.8D1 * t382 + t421 - t422
     # - 0.63D2 / 0.8D1 * t380 - t424 - t425 + t426 + t427 - 0.9D1 / 0.2
     #D1 * t428 - 0.27D2 / 0.2D1 * t430) * t6 + (-0.9D1 / 0.2D1 * t430 -
     # 0.27D2 / 0.2D1 * t428 - t436 - 0.63D2 / 0.8D1 * t399 + 0.27D2 / 0
     #.8D1 * t401 + t440 - t441 - t442 + t443 + t444) * t3
      t451 = t81 ** 2
      t476 = 0.27D2 / 0.4D1 * t111
      t477 = 0.9D1 * t113
      t480 = 0.9D1 * t109
      t485 = 0.27D2 / 0.4D1 * t107
      t494 = 0.63D2 * t107
      t495 = 0.63D2 * t111
      t504 = 0.2565D4 / 0.8D1 * t113
      t507 = 0.2565D4 / 0.8D1 * t109
      t520 = (t339 + t340 + t341 + t342 + (-t494 + t477 - t495 + t267 + 
     #t416 + t370 + 0.909D3 / 0.8D1 * t109 + t273 - t371 - t415) * t6 + 
     #(t418 + t267 - t417 + t480 + t273 + 0.909D3 / 0.8D1 * t113 - t495 
     #- t494 - t369 + t368) * t3) * t9 + t504 - 0.189D3 * t107 - 0.486D3
     # * t111 + t507 + t345 + t347 - t358 - t359 - t349 - t352 + (t391 -
     # t436 - t375 + t373 + t440 - 0.27D2 / 0.2D1 * t399 - 0.9D1 / 0.2D1
     # * t401 + 0.27D2 / 0.8D1 * t430 - 0.63D2 / 0.8D1 * t428 - t389) * 
     #t6 + (t421 - t396 + t398 + t404 - t406 - t424 - 0.27D2 / 0.2D1 * t
     #380 - 0.9D1 / 0.2D1 * t382 - 0.63D2 / 0.8D1 * t430 + 0.27D2 / 0.8D
     #1 * t428) * t3
      t536 = -0.486D3 * t107 + t345 + t347 - 0.189D3 * t111 + t507 - t34
     #8 - t357 - t355 - t350 + t504 + (0.27D2 / 0.8D1 * t386 - 0.63D2 / 
     #0.8D1 * t378 - t422 + t427 - t425 - 0.27D2 / 0.2D1 * t401 + t409 -
     # t407 - 0.9D1 / 0.2D1 * t399 + t426) * t6 + (t444 - t377 - 0.63D2 
     #/ 0.8D1 * t386 + 0.27D2 / 0.8D1 * t378 - t441 - t442 - 0.9D1 / 0.2
     #D1 * t380 - 0.27D2 / 0.2D1 * t382 + t443 + t385) * t3
      t550 = 0.36D2 * t374
      t552 = 0.90D2 * t372
      t555 = 0.90D2 * t376
      t556 = 0.36D2 * t403
      t557 = 0.18D2 * t390
      t559 = 0.36D2 * t408
      t560 = 0.36D2 * t439
      t561 = 0.18D2 * t405
      t562 = -t550 + 0.90D2 * t378 + t552 + 0.18D2 * t399 + 0.18D2 * t40
     #1 + t555 - t556 + t557 + 0.90D2 * t428 - t559 - t560 + t561
      t564 = 0.36D2 * t420
      t565 = 0.90D2 * t388
      t566 = 0.90D2 * t397
      t568 = 0.18D2 * t403
      t569 = 0.36D2 * t405
      t570 = 0.36D2 * t384
      t571 = 0.36D2 * t390
      t572 = 0.18D2 * t374
      t576 = -t564 + t565 + t566 + 0.90D2 * t386 + t568 - t569 - t570 - 
     #t571 + t572 + 0.18D2 * t380 + 0.18D2 * t382 + 0.90D2 * t430
      t578 = 0.252D3 * t107 + 0.252D3 * t113 + 0.495D3 * t44 + 0.495D3 *
     # t56 + 0.252D3 * t111 + 0.252D3 * t109 - 0.36D2 * t48 - 0.36D2 * t
     #46 - 0.36D2 * t54 - 0.36D2 * t52 + t562 * t6 + t576 * t3
      t584 = t578 * t9 + 0.90D2 * t401 + t557 + t552 + t566 - t559 + 0.9
     #0D2 * t382 - t550 - t570 - t569 + 0.18D2 * t378 + t568 + 0.18D2 * 
     #t386
      t590 = -t564 + t565 + t555 + t561 + 0.18D2 * t430 + 0.90D2 * t399 
     #+ 0.18D2 * t428 - t556 - t560 + t572 + 0.90D2 * t380 - t571
      t594 = (-0.72D2 * t1 - 0.72D2 * t4 * t6 * t8 * t9 * t12) * t17 * s
     # * t20 * z + ((((-0.216D3 * t24 - 0.216D3 * t6 - 0.216D3 * t3) * t
     #8 - 0.216D3 + t29 * t9) * S34 + t33 + t34 + t35 + 0.216D3 * S23 + 
     #t37) * t1 + (-t29 * t9 * t42 + ((-t45 + t47 + 0.90D2 * t48) * t6 +
     # (t53 + 0.90D2 * t54 - t57) * t3) * t8 + ((-t57 + t62 + 0.90D2 * t
     #52) * t6 + (-t45 + 0.90D2 * t46 + t67) * t3) * t9) * t12) * t17 * 
     #t20 + (-0.216D3 * t4 * t6 * t8 * t9 + t115 * t1 + ((-0.432D3 + 0.2
     #16D3 * t117 * t6 + (-t37 - t34 + (t110 - t62 - t53 - t47 - t67 - t
     #108 - t112 + t45 + t57 + t114) * t6) * t3) * t9 * t42 + t146 * t8 
     #+ (-t129 + t62 + t67 - t136 + t131 + t144 + t112 + t145 - 0.162D3 
     #* t107 + t132) * t9) * t12 + ((-0.9D1 * t52 - 0.9D1 * t48 + 0.18D2
     # * t109) * t6 * t2 - 0.18D2 * t56 + 0.18D2 * t107 + 0.18D2 * t111 
     #- 0.18D2 * t44 + (-0.9D1 * t54 + 0.18D2 * t113 - 0.9D1 * t46) * t5
     # * t3) * t9 * t8 / t11 / S12) * s * z + t181 * t11 + (((0.45D2 * t
     #24 - t184 - t185) * t8 + 0.252D3 + t189) * S34 + (-t188 * t9 - 0.5
     #85D3 / 0.4D1 + (-t194 - t195 + t196) * t6 + (-t199 + t200 - t201) 
     #* t3) * t8 + (-0.585D3 / 0.4D1 + (-t206 - t196 + t194) * t6 + (t20
     #1 - t200 - t209) * t3) * t9) * S12 + ((-t216 + 0.27D2 * t6 + 0.27D
     #2 * t3) * t8 - 0.252D3 + 0.27D2 * t83 * t9) * t81 + ((t227 * t6 + 
     #t231 * t3) * t8 + (t237 * t6 + t241 * t3) * t9) * S34 + ((-0.153D3
     # / 0.2D1 + (t247 - t248 + t249 - t250) * t6 + (-t253 + t254 - t255
     # + t256) * t3) * t9 - 0.99D2 * S13 - 0.99D2 * S14 + (-t263 + t264 
     #+ t265 + t266 - t267 + t268) * t6 + (t268 + t271 + t272 - t273 - t
     #274 + t275) * t3) * t8 + (-0.99D2 * S24 - 0.99D2 * S23 + (t282 - t
     #273 - t263 + t283 + t284 + t285) * t6 + (t288 - t267 + t282 + t289
     # - t274 + t290) * t3) * t9 + (((t216 - t184 - t185) * t8 + 0.45D2 
     #+ t189) * t298 + ((t241 * t6 + t237 * t3) * t8 + (t231 * t6 + t227
     # * t3) * t9) * t81 + t337 * S34 + t412 * t8 + t447 * t9) * t1 + (t
     #181 * t451 + ((0.585D3 / 0.4D1 * t28 * t9 + 0.18D2 + (-t195 - t200
     # + t196) * t6 + (-t196 - t199 + t200) * t3) * t8 + (0.18D2 + (-t20
     #6 - t201 + t194) * t6 + (t201 - t194 - t209) * t3) * t9) * t298 + 
     #(((-0.153D3 / 0.2D1 + 0.99D2 * t117 * t6 + (-0.99D2 * S24 - 0.99D2
     # * S14) * t3) * t9 - t248 - t255 + t249 + t256 + (t266 - t476 + t4
     #77 + t264 - t267 + t290) * t6 + (-t273 + t271 + t480 + t283 - t476
     # + t272) * t3) * t8 + (-t250 - t253 + t254 + t247 + (t477 - t485 +
     # t284 + t275 - t273 + t285) * t6 + (t288 - t267 - t485 + t289 + t4
     #80 + t265) * t3) * t9) * t81 + (t520 * t8 + t536 * t9) * S34 + t58
     #4 * t8 + t590 * t9) * t12
      rrgg2ggh11J4 = t594 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh11J5
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
      t4 = S34 * t3
      t5 = S12 + S14 + S24
      t6 = 0.1D1 / t5
      t8 = S13 + S14 + S34
      t9 = S23 + S24 + S34
      t11 = S12 ** 2
      t12 = 0.1D1 / t11
      t17 = s ** 2
      t20 = z ** 2
      t24 = t6 * t3 * t9
      t28 = -t6 - t3
      t29 = 0.216D3 * t28
      t33 = 0.216D3 * S13
      t34 = 0.216D3 * S14
      t35 = 0.432D3 * S34
      t37 = 0.216D3 * S24
      t42 = t8 * S34
      t44 = S13 * S24
      t45 = 0.36D2 * t44
      t46 = S13 ** 2
      t47 = 0.18D2 * t46
      t48 = S24 ** 2
      t52 = S14 ** 2
      t53 = 0.18D2 * t52
      t54 = S23 ** 2
      t56 = S14 * S23
      t57 = 0.36D2 * t56
      t62 = 0.18D2 * t54
      t67 = 0.18D2 * t48
      t81 = S34 ** 2
      t106 = S14 * S24
      t107 = 0.36D2 * t106
      t108 = S13 * S23
      t109 = 0.36D2 * t108
      t110 = S13 * S14
      t111 = 0.36D2 * t110
      t112 = S23 * S24
      t113 = 0.36D2 * t112
      t114 = -0.216D3 * t81 + ((0.63D2 / 0.4D1 * t28 * t9 - 0.63D2 / 0.4
     #D1 - 0.216D3 * S24 * t6 - 0.216D3 * S23 * t3) * t8 + (-0.63D2 / 0.
     #4D1 - 0.216D3 * S14 * t6 - 0.216D3 * S13 * t3) * t9) * S34 + (-0.4
     #32D3 * S23 - 0.432D3 * S24 - t35 - t33 - t34) * t8 + (-0.216D3 * S
     #24 - 0.216D3 * S23) * t9 - t53 - t62 + t57 + t45 - t107 - t109 + t
     #111 + t113 - t47 - t67
      t116 = -S23 - S13
      t126 = 0.144D3 * t52
      t127 = 0.45D2 * t44
      t128 = 0.45D2 * t56
      t130 = 0.27D2 * t110
      t131 = 0.27D2 * t112
      t132 = 0.144D3 * t48
      t135 = 0.144D3 * t46
      t137 = 0.144D3 * t54
      t142 = 0.27D2 * t108
      t143 = 0.27D2 * t106
      t145 = ((-t126 + t127 + t128 - 0.216D3 * t106 + t62 + t109 + t130 
     #+ t47 + t131 - t132) * t6 + (t107 + t67 + t128 - t135 + t53 - 0.21
     #6D3 * t108 + t131 + t127 - t137 + t130) * t3) * t9 + t53 - t137 - 
     #t132 + t47 + t142 + t143 + t128 + t127 + t111 - 0.216D3 * t112
      t177 = -t28
      t178 = 0.9D1 / 0.8D1 * t177
      t181 = t178 * t8 + t178 * t9
      t184 = 0.18D2 * t6
      t185 = 0.18D2 * t3
      t189 = 0.18D2 * t28 * t9
      t194 = 0.27D2 / 0.8D1 * S14
      t195 = 0.9D1 / 0.2D1 * S13
      t196 = 0.27D2 / 0.8D1 * S24
      t199 = 0.9D1 / 0.2D1 * S14
      t200 = 0.27D2 / 0.8D1 * S23
      t201 = 0.27D2 / 0.8D1 * S13
      t206 = 0.9D1 / 0.2D1 * S23
      t209 = 0.9D1 / 0.2D1 * S24
      t227 = 0.36D2 * S14 - 0.27D2 * S23
      t231 = 0.36D2 * S13 - 0.27D2 * S24
      t237 = -0.27D2 * S13 + 0.36D2 * S24
      t241 = -0.27D2 * S14 + 0.36D2 * S23
      t247 = 0.3879D4 / 0.16D2 * S24
      t248 = 0.3879D4 / 0.16D2 * S14
      t249 = 0.6849D4 / 0.16D2 * S23
      t250 = 0.6849D4 / 0.16D2 * S13
      t253 = 0.6849D4 / 0.16D2 * S24
      t254 = 0.3879D4 / 0.16D2 * S13
      t255 = 0.6849D4 / 0.16D2 * S14
      t256 = 0.3879D4 / 0.16D2 * S23
      t263 = 0.27D2 / 0.4D1 * t106
      t264 = 0.27D2 / 0.2D1 * t46
      t265 = 0.27D2 / 0.8D1 * t52
      t266 = 0.63D2 / 0.8D1 * t48
      t267 = 0.27D2 * t44
      t268 = 0.9D1 * t110
      t271 = 0.27D2 / 0.2D1 * t52
      t272 = 0.63D2 / 0.8D1 * t54
      t273 = 0.27D2 * t56
      t274 = 0.27D2 / 0.4D1 * t108
      t275 = 0.27D2 / 0.8D1 * t46
      t282 = 0.9D1 * t112
      t283 = 0.27D2 / 0.8D1 * t48
      t284 = 0.27D2 / 0.2D1 * t54
      t285 = 0.63D2 / 0.8D1 * t52
      t288 = 0.27D2 / 0.2D1 * t48
      t289 = 0.63D2 / 0.8D1 * t46
      t290 = 0.27D2 / 0.8D1 * t54
      t299 = t81 * S34
      t316 = 0.396D3 * t44
      t321 = 0.396D3 * t56
      t338 = ((0.1971D4 / 0.2D1 + (-t67 + t267 - t53 - t47 - t131 + t109
     # + t107 - t130 + t273 - t62) * t6 * t3) * t9 + (t273 - t316 - t53 
     #+ 0.324D3 * t46 + 0.117D3 * t48 - t62) * t6 + (-t321 + 0.117D3 * t
     #54 + 0.324D3 * t52 + t267 - t47 - t67) * t3) * t8 + ((0.117D3 * t5
     #2 + t267 - t321 - t67 + 0.324D3 * t54 - t47) * t6 + (-t316 + 0.324
     #D3 * t48 - t62 + 0.117D3 * t46 + t273 - t53) * t3) * t9 - t53 - t6
     #2 - t67 - t47 + t267 + t113 + t273 - t143 + t111 - t142
      t340 = 0.441D3 / 0.4D1 * S24
      t341 = 0.441D3 / 0.4D1 * S13
      t342 = 0.441D3 / 0.4D1 * S14
      t343 = 0.441D3 / 0.4D1 * S23
      t344 = 0.1809D4 / 0.8D1 * t48
      t346 = 0.6633D4 / 0.16D2 * t112
      t348 = 0.135D3 * t54
      t349 = 0.5769D4 / 0.16D2 * t56
      t350 = 0.5769D4 / 0.16D2 * t44
      t351 = 0.135D3 * t46
      t352 = 0.6633D4 / 0.16D2 * t110
      t353 = 0.1809D4 / 0.8D1 * t52
      t356 = 0.1809D4 / 0.8D1 * t46
      t358 = 0.135D3 * t52
      t360 = 0.135D3 * t48
      t361 = 0.1809D4 / 0.8D1 * t54
      t366 = 0.171D3 / 0.8D1 * t46
      t368 = 0.72D2 * t106
      t369 = 0.72D2 * t108
      t370 = 0.171D3 / 0.8D1 * t52
      t371 = 0.819D3 / 0.16D2 * t54
      t372 = 0.819D3 / 0.16D2 * t48
      t373 = t46 * S24
      t374 = 0.243D3 / 0.2D1 * t373
      t375 = S13 * t48
      t376 = 0.333D3 / 0.2D1 * t375
      t377 = S13 * t52
      t379 = t52 * S24
      t381 = t48 * S24
      t382 = 0.621D3 / 0.8D1 * t381
      t383 = t110 * S24
      t384 = 0.27D2 * t383
      t385 = S14 * t48
      t387 = t46 * S13
      t388 = 0.18D2 * t387
      t389 = t52 * S14
      t390 = 0.9D1 / 0.8D1 * t389
      t391 = t46 * S14
      t395 = t52 * S23
      t396 = 0.243D3 / 0.2D1 * t395
      t397 = t110 * S23
      t398 = 0.27D2 * t397
      t399 = 0.9D1 / 0.8D1 * t387
      t400 = 0.18D2 * t389
      t401 = S14 * t54
      t402 = 0.333D3 / 0.2D1 * t401
      t403 = t46 * S23
      t405 = S13 * t54
      t409 = t54 * S23
      t410 = 0.621D3 / 0.8D1 * t409
      t413 = (t340 + t341 + t342 + t343 + (-t344 - 0.2673D4 / 0.4D1 * t1
     #06 + t346 - 0.270D3 * t108 - t348 + t349 + t350 - t351 + t352 - t3
     #53) * t6 + (-t356 - 0.270D3 * t106 + t349 - t358 + t346 - 0.2673D4
     # / 0.4D1 * t108 + t350 - t360 + t352 - t361) * t3) * t9 + t57 - t3
     #66 + 0.1197D4 / 0.8D1 * t112 - t368 + t45 + t268 - t369 - t370 + t
     #371 + t372 + (t374 - t376 - 0.9D1 / 0.2D1 * t377 + 0.27D2 / 0.8D1 
     #* t379 + t382 + t384 - 0.63D2 / 0.8D1 * t385 - t388 - t390 - 0.27D
     #2 / 0.2D1 * t391) * t6 + (t396 + t398 - t399 - t400 - t402 + 0.27D
     #2 / 0.8D1 * t403 - 0.63D2 / 0.8D1 * t405 - 0.9D1 / 0.2D1 * t391 - 
     #0.27D2 / 0.2D1 * t377 + t410) * t3
      t415 = 0.819D3 / 0.16D2 * t46
      t416 = 0.171D3 / 0.8D1 * t54
      t417 = 0.171D3 / 0.8D1 * t48
      t418 = 0.819D3 / 0.16D2 * t52
      t420 = 0.18D2 * t409
      t421 = 0.9D1 / 0.8D1 * t381
      t422 = t56 * S24
      t423 = 0.27D2 * t422
      t425 = 0.621D3 / 0.8D1 * t389
      t426 = t54 * S24
      t429 = 0.333D3 / 0.2D1 * t395
      t430 = 0.243D3 / 0.2D1 * t401
      t431 = t48 * S23
      t435 = 0.9D1 / 0.8D1 * t409
      t438 = 0.621D3 / 0.8D1 * t387
      t439 = 0.333D3 / 0.2D1 * t373
      t440 = 0.243D3 / 0.2D1 * t375
      t443 = 0.18D2 * t381
      t444 = t44 * S23
      t445 = 0.27D2 * t444
      t448 = t282 + t415 - t416 - t417 + t57 + t418 + 0.1197D4 / 0.8D1 *
     # t110 - t369 - t368 + t45 + (-t420 - t421 + t423 + 0.27D2 / 0.8D1 
     #* t385 + t425 - 0.27D2 / 0.2D1 * t426 - 0.63D2 / 0.8D1 * t379 - t4
     #29 + t430 - 0.9D1 / 0.2D1 * t431) * t6 + (-t435 - 0.63D2 / 0.8D1 *
     # t403 + 0.27D2 / 0.8D1 * t405 + t438 - t439 + t440 - 0.9D1 / 0.2D1
     # * t426 - 0.27D2 / 0.2D1 * t431 - t443 + t445) * t3
      t452 = t81 ** 2
      t477 = 0.27D2 / 0.4D1 * t112
      t478 = 0.9D1 * t108
      t481 = 0.9D1 * t106
      t486 = 0.27D2 / 0.4D1 * t110
      t496 = 0.72D2 * t112
      t497 = 0.72D2 * t110
      t505 = 0.6633D4 / 0.16D2 * t106
      t508 = 0.6633D4 / 0.16D2 * t108
      t521 = (t340 + t341 + t342 + t343 + (0.1197D4 / 0.8D1 * t106 + t41
     #8 - t416 - t496 + t478 - t366 - t497 + t57 + t45 + t372) * t6 + (t
     #415 + t57 - t496 + t481 + t45 + 0.1197D4 / 0.8D1 * t108 - t497 + t
     #371 - t370 - t417) * t3) * t9 - t344 - t358 + t505 - t361 - t351 -
     # 0.2673D4 / 0.4D1 * t112 + t349 + t350 - 0.270D3 * t110 + t508 + (
     #-t388 - t435 - t376 - 0.27D2 / 0.2D1 * t403 - 0.9D1 / 0.2D1 * t405
     # + t374 + t382 + 0.27D2 / 0.8D1 * t426 - 0.63D2 / 0.8D1 * t431 + t
     #445) * t6 + (t396 - t402 - 0.9D1 / 0.2D1 * t385 - 0.27D2 / 0.2D1 *
     # t379 + t423 + t410 - 0.63D2 / 0.8D1 * t426 + 0.27D2 / 0.8D1 * t43
     #1 - t400 - t421) * t3
      t537 = -t353 + t505 - t348 - t360 - t356 - 0.270D3 * t112 + t349 +
     # t350 - 0.2673D4 / 0.4D1 * t110 + t508 + (-t420 - t429 + t398 - 0.
     #9D1 / 0.2D1 * t403 - 0.27D2 / 0.2D1 * t405 + 0.27D2 / 0.8D1 * t391
     # - 0.63D2 / 0.8D1 * t377 + t430 - t399 + t425) * t6 + (-0.63D2 / 0
     #.8D1 * t391 + 0.27D2 / 0.8D1 * t377 - 0.27D2 / 0.2D1 * t385 - 0.9D
     #1 / 0.2D1 * t379 - t390 - t439 + t440 - t443 + t438 + t384) * t3
      t551 = 0.36D2 * t375
      t552 = 0.36D2 * t397
      t554 = 0.36D2 * t444
      t555 = 0.18D2 * t373
      t558 = 0.126D3 * t389
      t560 = 0.18D2 * t401
      t561 = 0.36D2 * t395
      t562 = 0.126D3 * t381
      t563 = -t551 - t552 + 0.126D3 * t377 - t554 + t555 + 0.18D2 * t403
     # + 0.18D2 * t405 + t558 + 0.126D3 * t431 + t560 - t561 + t562
      t565 = 0.18D2 * t395
      t566 = 0.36D2 * t401
      t568 = 0.126D3 * t387
      t569 = 0.36D2 * t373
      t570 = 0.18D2 * t375
      t573 = 0.36D2 * t383
      t574 = 0.36D2 * t422
      t576 = 0.126D3 * t409
      t577 = t565 - t566 + 0.126D3 * t391 + t568 - t569 + t570 + 0.18D2 
     #* t385 + 0.18D2 * t379 - t573 - t574 + 0.126D3 * t426 + t576
      t579 = 0.342D3 * t108 + 0.657D3 * t56 - 0.27D2 * t48 - 0.27D2 * t4
     #6 - 0.27D2 * t52 + 0.657D3 * t44 + 0.342D3 * t110 + 0.342D3 * t112
     # + 0.342D3 * t106 - 0.27D2 * t54 + t563 * t6 + t577 * t3
      t585 = t579 * t9 + t576 + 0.126D3 * t405 + 0.18D2 * t391 + 0.18D2 
     #* t377 - t552 - t566 + 0.126D3 * t385 - t573 + t562 + t555 - t551 
     #+ t565
      t591 = -t561 + t560 + 0.18D2 * t426 + 0.126D3 * t403 - t569 + t568
     # + t558 - t574 + t570 + 0.18D2 * t431 - t554 + 0.126D3 * t379
      t595 = (-0.72D2 * t1 - 0.72D2 * t4 * t6 * t8 * t9 * t12) * t17 * s
     # * t20 * z + ((((-0.216D3 * t24 - 0.216D3 * t6 - 0.216D3 * t3) * t
     #8 - 0.216D3 + t29 * t9) * S34 + t33 + t34 + t35 + 0.216D3 * S23 + 
     #t37) * t1 + (-t29 * t9 * t42 + ((-t45 + t47 + 0.126D3 * t48) * t6 
     #+ (t53 + 0.126D3 * t54 - t57) * t3) * t8 + ((-t57 + t62 + 0.126D3 
     #* t52) * t6 + (-t45 + 0.126D3 * t46 + t67) * t3) * t9) * t12) * t1
     #7 * t20 + (-0.216D3 * t4 * t6 * t8 * t9 + t114 * t1 + ((-0.432D3 +
     # 0.216D3 * t116 * t6 + (-t37 - t34 + (t107 - t62 - t53 - t47 - t67
     # - t111 - t113 + t45 + t57 + t109) * t6) * t3) * t9 * t42 + t145 *
     # t8 + (t67 - t126 + t127 + t128 + t142 + t113 + t143 + t62 - t135 
     #- 0.216D3 * t110) * t9) * t12 + ((-0.9D1 * t52 - 0.9D1 * t48 + 0.1
     #8D2 * t106) * t6 * t2 - 0.18D2 * t56 + 0.18D2 * t110 + 0.18D2 * t1
     #12 - 0.18D2 * t44 + (-0.9D1 * t54 + 0.18D2 * t108 - 0.9D1 * t46) *
     # t5 * t3) * t9 * t8 / t11 / S12) * s * z + t181 * t11 + (((0.117D3
     # * t24 - t184 - t185) * t8 + 0.324D3 + t189) * S34 + (0.603D3 / 0.
     #16D2 * t177 * t9 - 0.585D3 / 0.4D1 + (-t194 - t195 + t196) * t6 + 
     #(-t199 + t200 - t201) * t3) * t8 + (-0.585D3 / 0.4D1 + (-t206 - t1
     #96 + t194) * t6 + (t201 - t200 - t209) * t3) * t9) * S12 + ((-0.39
     #6D3 * t24 + 0.27D2 * t6 + 0.27D2 * t3) * t8 - 0.396D3 + 0.27D2 * t
     #177 * t9) * t81 + ((t227 * t6 + t231 * t3) * t8 + (t237 * t6 + t24
     #1 * t3) * t9) * S34 + ((-0.927D3 / 0.8D1 + (-t247 - t248 + t249 + 
     #t250) * t6 + (t253 - t254 + t255 - t256) * t3) * t9 - 0.99D2 * S14
     # - 0.99D2 * S13 + (-t263 + t264 + t265 + t266 - t267 + t268) * t6 
     #+ (t268 + t271 + t272 - t273 - t274 + t275) * t3) * t8 + (-0.99D2 
     #* S24 - 0.99D2 * S23 + (t282 - t273 - t263 + t283 + t284 + t285) *
     # t6 + (t288 - t267 + t282 + t289 - t274 + t290) * t3) * t9 + (((0.
     #324D3 * t24 - t184 - t185) * t8 + 0.117D3 + t189) * t299 + ((t241 
     #* t6 + t237 * t3) * t8 + (t231 * t6 + t227 * t3) * t9) * t81 + t33
     #8 * S34 + t413 * t8 + t448 * t9) * t1 + (t181 * t452 + ((0.585D3 /
     # 0.4D1 * t28 * t9 + 0.603D3 / 0.16D2 + (-t195 - t200 + t196) * t6 
     #+ (-t196 - t199 + t200) * t3) * t8 + (0.603D3 / 0.16D2 + (-t206 - 
     #t201 + t194) * t6 + (t201 - t194 - t209) * t3) * t9) * t299 + (((-
     #0.927D3 / 0.8D1 + 0.99D2 * t116 * t6 + (-0.99D2 * S24 - 0.99D2 * S
     #14) * t3) * t9 - t247 + t250 + t255 - t256 + (t266 - t477 + t478 +
     # t264 - t267 + t290) * t6 + (-t273 + t271 + t481 + t283 - t477 + t
     #272) * t3) * t8 + (-t254 - t248 + t253 + t249 + (t478 - t486 + t28
     #4 + t275 - t273 + t285) * t6 + (t288 - t267 - t486 + t289 + t481 +
     # t265) * t3) * t9) * t81 + (t521 * t8 + t537 * t9) * S34 + t585 * 
     #t8 + t591 * t9) * t12
      rrgg2ggh11J5 = t595 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh11J6
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
      t4 = S34 * t3
      t5 = S12 + S14 + S24
      t6 = 0.1D1 / t5
      t8 = S13 + S14 + S34
      t9 = S23 + S24 + S34
      t11 = S12 ** 2
      t12 = 0.1D1 / t11
      t17 = s ** 2
      t20 = z ** 2
      t23 = t5 ** 2
      t24 = 0.1D1 / t23
      t26 = t6 * t3
      t28 = t2 ** 2
      t29 = 0.1D1 / t28
      t37 = t6 + t3
      t38 = 0.1080D4 * t37
      t42 = 0.1080D4 * S13
      t43 = 0.1080D4 * S14
      t44 = 0.2160D4 * S34
      t46 = 0.1080D4 * S24
      t51 = t8 * S34
      t53 = S24 ** 2
      t55 = S13 ** 2
      t56 = 0.90D2 * t55
      t57 = S13 * S24
      t58 = 0.180D3 * t57
      t61 = S14 * S23
      t62 = 0.180D3 * t61
      t63 = S14 ** 2
      t64 = 0.90D2 * t63
      t65 = S23 ** 2
      t72 = 0.90D2 * t65
      t75 = 0.90D2 * t53
      t83 = S14 * S24
      t84 = 0.54D2 * t83
      t89 = S13 * S14
      t90 = S23 * S24
      t95 = S13 * S23
      t96 = 0.54D2 * t95
      t104 = 0.1D1 / t11 / S12
      t114 = S34 ** 2
      t116 = -t37
      t140 = 0.180D3 * t83
      t141 = 0.180D3 * t95
      t142 = 0.180D3 * t89
      t143 = 0.180D3 * t90
      t144 = 0.1080D4 * t114 + ((0.14373D5 / 0.4D1 * t116 * t9 - 0.14373
     #D5 / 0.4D1 + 0.1080D4 * S24 * t6 + 0.1080D4 * S23 * t3) * t8 + (-0
     #.14373D5 / 0.4D1 + 0.1080D4 * S14 * t6 + 0.1080D4 * S13 * t3) * t9
     #) * S34 + (0.2160D4 * S23 + 0.2160D4 * S24 + t44 + t43 + t42) * t8
     # + (0.1080D4 * S23 + 0.1080D4 * S24) * t9 + t64 + t72 - t62 - t58 
     #+ t140 + t141 - t142 - t143 + t56 + t75
      t146 = S23 + S13
      t156 = 0.81D2 * t63
      t157 = 0.171D3 * t57
      t158 = 0.171D3 * t61
      t159 = 0.189D3 * t89
      t160 = 0.189D3 * t90
      t161 = 0.81D2 * t53
      t164 = 0.81D2 * t55
      t165 = 0.81D2 * t65
      t170 = 0.189D3 * t95
      t171 = 0.189D3 * t83
      t173 = ((-t156 - t157 - t158 - t84 - t72 - t141 - t159 - t56 - t16
     #0 - t161) * t6 + (-t140 - t75 - t158 - t164 - t64 - t96 - t160 - t
     #157 - t165 - t159) * t3) * t9 - t64 - t165 - t161 - t56 - t170 - t
     #171 - t158 - t157 - t142 - 0.54D2 * t90
      t203 = 0.45D2 / 0.8D1 * t116
      t206 = t203 * t8 + t203 * t9
      t208 = t26 * t9
      t210 = 0.90D2 * t6
      t211 = 0.90D2 * t3
      t215 = 0.90D2 * t37 * t9
      t220 = 0.135D3 / 0.8D1 * S14
      t221 = 0.45D2 / 0.2D1 * S13
      t222 = 0.135D3 / 0.8D1 * S24
      t225 = 0.45D2 / 0.2D1 * S14
      t226 = 0.135D3 / 0.8D1 * S23
      t227 = 0.135D3 / 0.8D1 * S13
      t232 = 0.45D2 / 0.2D1 * S23
      t235 = 0.45D2 / 0.2D1 * S24
      t253 = -0.180D3 * S14 + 0.135D3 * S23
      t257 = -0.180D3 * S13 + 0.135D3 * S24
      t263 = -0.180D3 * S24 + 0.135D3 * S13
      t267 = -0.180D3 * S23 + 0.135D3 * S14
      t273 = 0.279D3 / 0.4D1 * S24
      t274 = 0.279D3 / 0.4D1 * S14
      t275 = 0.1431D4 / 0.8D1 * S23
      t276 = 0.1431D4 / 0.8D1 * S13
      t279 = 0.1431D4 / 0.8D1 * S24
      t280 = 0.279D3 / 0.4D1 * S13
      t281 = 0.1431D4 / 0.8D1 * S14
      t282 = 0.279D3 / 0.4D1 * S23
      t289 = 0.315D3 / 0.8D1 * t53
      t290 = 0.135D3 * t57
      t291 = 0.45D2 * t89
      t292 = 0.135D3 / 0.8D1 * t63
      t293 = 0.135D3 / 0.4D1 * t83
      t294 = 0.135D3 / 0.2D1 * t55
      t297 = 0.135D3 / 0.4D1 * t95
      t298 = 0.315D3 / 0.8D1 * t65
      t299 = 0.135D3 * t61
      t300 = 0.135D3 / 0.8D1 * t55
      t301 = 0.135D3 / 0.2D1 * t63
      t308 = 0.315D3 / 0.8D1 * t63
      t309 = 0.135D3 / 0.2D1 * t65
      t310 = 0.45D2 * t90
      t311 = 0.135D3 / 0.8D1 * t53
      t314 = 0.315D3 / 0.8D1 * t55
      t315 = 0.135D3 / 0.2D1 * t53
      t316 = 0.135D3 / 0.8D1 * t65
      t325 = t114 * S34
      t344 = 0.432D3 * t57
      t349 = 0.432D3 * t61
      t368 = ((-0.9423D4 / 0.2D1 + (t64 + t75 + t56 + 0.135D3 * t90 - t1
     #41 - t140 + t72 - t299 + 0.135D3 * t89 - t290) * t6 * t3) * t9 + (
     #-t299 - t344 + t64 - 0.414D3 * t55 + 0.621D3 * t53 + t72) * t6 + (
     #-t349 + 0.621D3 * t65 - 0.414D3 * t63 - t290 + t56 + t75) * t3) * 
     #t8 + ((0.621D3 * t63 - t290 - t349 + t75 - 0.414D3 * t65 + t56) * 
     #t6 + (-t344 - 0.414D3 * t53 + t72 + 0.621D3 * t55 - t299 + t64) * 
     #t3) * t9 + t64 + t72 + t75 + t56 - t290 - t143 - t299 + 0.135D3 * 
     #t83 - t142 + 0.135D3 * t95
      t370 = 0.25137D5 / 0.16D2 * S24
      t371 = 0.25137D5 / 0.16D2 * S13
      t372 = 0.25137D5 / 0.16D2 * S14
      t373 = 0.25137D5 / 0.16D2 * S23
      t374 = 0.63D2 / 0.2D1 * t53
      t376 = 0.477D3 / 0.8D1 * t90
      t378 = 0.675D3 / 0.2D1 * t65
      t379 = 0.99D2 / 0.8D1 * t61
      t380 = 0.99D2 / 0.8D1 * t57
      t381 = 0.675D3 / 0.2D1 * t55
      t382 = 0.477D3 / 0.8D1 * t89
      t383 = 0.63D2 / 0.2D1 * t63
      t386 = 0.63D2 / 0.2D1 * t55
      t388 = 0.675D3 / 0.2D1 * t63
      t390 = 0.675D3 / 0.2D1 * t53
      t391 = 0.63D2 / 0.2D1 * t65
      t396 = 0.9D1 * t61
      t397 = 0.855D3 / 0.8D1 * t55
      t399 = 0.9D1 * t57
      t400 = 0.855D3 / 0.8D1 * t63
      t401 = 0.1377D4 / 0.16D2 * t65
      t402 = 0.1377D4 / 0.16D2 * t53
      t403 = t55 * S24
      t404 = 0.531D3 / 0.2D1 * t403
      t405 = S13 * t53
      t406 = 0.297D3 / 0.2D1 * t405
      t407 = S13 * t63
      t409 = t63 * S24
      t411 = t53 * S24
      t412 = 0.369D3 / 0.8D1 * t411
      t413 = t89 * S24
      t414 = 0.135D3 * t413
      t415 = S14 * t53
      t417 = t55 * S13
      t418 = 0.90D2 * t417
      t419 = t63 * S14
      t420 = 0.45D2 / 0.8D1 * t419
      t421 = t55 * S14
      t425 = t63 * S23
      t426 = 0.531D3 / 0.2D1 * t425
      t427 = t89 * S23
      t428 = 0.135D3 * t427
      t429 = 0.45D2 / 0.8D1 * t417
      t430 = 0.90D2 * t419
      t431 = S14 * t65
      t432 = 0.297D3 / 0.2D1 * t431
      t433 = t55 * S23
      t435 = S13 * t65
      t439 = t65 * S23
      t440 = 0.369D3 / 0.8D1 * t439
      t443 = (-t370 - t371 - t372 - t373 + (-t374 - 0.351D3 * t83 + t376
     # - 0.675D3 * t95 - t378 - t379 - t380 - t381 + t382 - t383) * t6 +
     # (-t386 - 0.675D3 * t83 - t379 - t388 + t376 - 0.351D3 * t95 - t38
     #0 - t390 + t382 - t391) * t3) * t9 - t396 + t397 - 0.513D3 / 0.8D1
     # * t90 + t171 - t399 - t291 + t170 + t400 + t401 + t402 + (-t404 +
     # t406 + 0.45D2 / 0.2D1 * t407 - 0.135D3 / 0.8D1 * t409 - t412 - t4
     #14 + 0.315D3 / 0.8D1 * t415 + t418 + t420 + 0.135D3 / 0.2D1 * t421
     #) * t6 + (-t426 - t428 + t429 + t430 + t432 - 0.135D3 / 0.8D1 * t4
     #33 + 0.315D3 / 0.8D1 * t435 + 0.45D2 / 0.2D1 * t421 + 0.135D3 / 0.
     #2D1 * t407 - t440) * t3
      t445 = 0.1377D4 / 0.16D2 * t55
      t446 = 0.855D3 / 0.8D1 * t65
      t447 = 0.855D3 / 0.8D1 * t53
      t448 = 0.1377D4 / 0.16D2 * t63
      t450 = 0.90D2 * t439
      t451 = 0.45D2 / 0.8D1 * t411
      t452 = t61 * S24
      t453 = 0.135D3 * t452
      t455 = 0.369D3 / 0.8D1 * t419
      t456 = t65 * S24
      t459 = 0.297D3 / 0.2D1 * t425
      t460 = 0.531D3 / 0.2D1 * t431
      t461 = t53 * S23
      t465 = 0.45D2 / 0.8D1 * t439
      t468 = 0.369D3 / 0.8D1 * t417
      t469 = 0.297D3 / 0.2D1 * t403
      t470 = 0.531D3 / 0.2D1 * t405
      t473 = 0.90D2 * t411
      t474 = t57 * S23
      t475 = 0.135D3 * t474
      t478 = -t310 + t445 + t446 + t447 - t396 + t448 - 0.513D3 / 0.8D1 
     #* t89 + t170 + t171 - t399 + (t450 + t451 - t453 - 0.135D3 / 0.8D1
     # * t415 - t455 + 0.135D3 / 0.2D1 * t456 + 0.315D3 / 0.8D1 * t409 +
     # t459 - t460 + 0.45D2 / 0.2D1 * t461) * t6 + (t465 + 0.315D3 / 0.8
     #D1 * t433 - 0.135D3 / 0.8D1 * t435 - t468 + t469 - t470 + 0.45D2 /
     # 0.2D1 * t456 + 0.135D3 / 0.2D1 * t461 + t473 - t475) * t3
      t482 = t114 ** 2
      t507 = 0.45D2 * t95
      t508 = 0.135D3 / 0.4D1 * t90
      t511 = 0.45D2 * t83
      t516 = 0.135D3 / 0.4D1 * t89
      t533 = 0.477D3 / 0.8D1 * t83
      t536 = 0.477D3 / 0.8D1 * t95
      t549 = (-t370 - t371 - t372 - t373 + (-0.513D3 / 0.8D1 * t83 + t44
     #8 + t446 + t160 - t507 + t397 + t159 - t396 - t399 + t402) * t6 + 
     #(t445 - t396 + t160 - t511 - t399 - 0.513D3 / 0.8D1 * t95 + t159 +
     # t401 + t400 + t447) * t3) * t9 - t374 - t388 + t533 - t391 - t381
     # - 0.351D3 * t90 - t379 - t380 - 0.675D3 * t89 + t536 + (t418 + t4
     #65 + t406 + 0.135D3 / 0.2D1 * t433 + 0.45D2 / 0.2D1 * t435 - t404 
     #- t412 - 0.135D3 / 0.8D1 * t456 + 0.315D3 / 0.8D1 * t461 - t475) *
     # t6 + (-t426 + t432 + 0.45D2 / 0.2D1 * t415 + 0.135D3 / 0.2D1 * t4
     #09 - t453 - t440 + 0.315D3 / 0.8D1 * t456 - 0.135D3 / 0.8D1 * t461
     # + t430 + t451) * t3
      t565 = -t383 + t533 - t378 - t390 - t386 - 0.675D3 * t90 - t379 - 
     #t380 - 0.351D3 * t89 + t536 + (t450 + t459 - t428 + 0.45D2 / 0.2D1
     # * t433 + 0.135D3 / 0.2D1 * t435 - 0.135D3 / 0.8D1 * t421 + 0.315D
     #3 / 0.8D1 * t407 - t460 + t429 - t455) * t6 + (0.315D3 / 0.8D1 * t
     #421 - 0.135D3 / 0.8D1 * t407 + 0.135D3 / 0.2D1 * t415 + 0.45D2 / 0
     #.2D1 * t409 + t420 + t469 - t470 + t473 - t468 - t414) * t3
      t579 = 0.180D3 * t405
      t580 = 0.180D3 * t427
      t582 = 0.180D3 * t474
      t583 = 0.90D2 * t403
      t586 = 0.54D2 * t419
      t588 = 0.90D2 * t431
      t589 = 0.180D3 * t425
      t590 = 0.54D2 * t411
      t591 = t579 + t580 + 0.54D2 * t407 + t582 - t583 - 0.90D2 * t433 -
     # 0.90D2 * t435 + t586 + 0.54D2 * t461 - t588 + t589 + t590
      t593 = 0.90D2 * t425
      t594 = 0.180D3 * t431
      t596 = 0.54D2 * t417
      t597 = 0.180D3 * t403
      t598 = 0.90D2 * t405
      t601 = 0.180D3 * t413
      t602 = 0.180D3 * t452
      t604 = 0.54D2 * t439
      t605 = -t593 + t594 + 0.54D2 * t421 + t596 + t597 - t598 - 0.90D2 
     #* t415 - 0.90D2 * t409 + t601 + t602 + 0.54D2 * t456 + t604
      t607 = 0.324D3 * t95 + 0.117D3 * t61 + 0.468D3 * t53 + 0.468D3 * t
     #55 + 0.468D3 * t63 + 0.117D3 * t57 + 0.324D3 * t89 + 0.324D3 * t90
     # + 0.324D3 * t83 + 0.468D3 * t65 + t591 * t6 + t605 * t3
      t613 = t607 * t9 + t604 + 0.54D2 * t435 - 0.90D2 * t421 - 0.90D2 *
     # t407 + t580 + t594 + 0.54D2 * t415 + t601 + t590 - t583 + t579 - 
     #t593
      t619 = t589 - t588 - 0.90D2 * t456 + 0.54D2 * t433 + t597 + t596 +
     # t586 + t602 - t598 - 0.90D2 * t461 + t582 + 0.54D2 * t409
      t623 = (0.360D3 * t1 + 0.360D3 * t4 * t6 * t8 * t9 * t12) * t17 * 
     #s * t20 * z + (((((-0.243D3 * t24 + 0.1080D4 * t26 - 0.243D3 * t29
     #) * t9 + 0.1080D4 * t6 + 0.1080D4 * t3) * t8 + 0.1080D4 + t38 * t9
     #) * S34 - t42 - t43 - t44 - 0.1080D4 * S23 - t46) * t1 + (-t38 * t
     #9 * t51 + ((0.54D2 * t53 - t56 + t58) * t6 + (t62 - t64 + 0.54D2 *
     # t65) * t3) * t8 + ((t62 + 0.54D2 * t63 - t72) * t6 + (t58 - t75 +
     # 0.54D2 * t55) * t3) * t9) * t12 + ((t84 - 0.27D2 * t53 - 0.27D2 *
     # t63) * t24 + (0.54D2 * t89 - 0.54D2 * t61 + 0.54D2 * t90 - 0.54D2
     # * t57) * t6 * t3 + (t96 - 0.27D2 * t65 - 0.27D2 * t55) * t29) * t
     #9 * t51 * t104) * t17 * t20 + (0.1080D4 * t4 * t6 * t8 * t9 + t144
     # * t1 + ((0.2160D4 + 0.1080D4 * t146 * t6 + (t43 + t46 + (-t62 + t
     #64 - t140 + t72 + t143 + t56 - t141 + t75 + t142 - t58) * t6) * t3
     #) * t9 * t51 + t173 * t8 + (-t75 - t156 - t157 - t158 - t170 - t14
     #3 - t171 - t72 - t164 - 0.54D2 * t89) * t9) * t12 + ((-0.9D1 * t63
     # - 0.9D1 * t53 + 0.18D2 * t83) * t6 * t2 - 0.18D2 * t61 + 0.18D2 *
     # t89 + 0.18D2 * t90 - 0.18D2 * t57 + (-0.9D1 * t65 + 0.18D2 * t95 
     #- 0.9D1 * t55) * t5 * t3) * t9 * t8 * t104) * s * z + t206 * t11 +
     # (((0.621D3 * t208 + t210 + t211) * t8 - 0.414D3 + t215) * S34 + (
     #0.135D3 / 0.4D1 * t37 * t9 + 0.2925D4 / 0.4D1 + (t220 + t221 - t22
     #2) * t6 + (t225 - t226 + t227) * t3) * t8 + (0.2925D4 / 0.4D1 + (t
     #232 + t222 - t220) * t6 + (-t227 + t226 + t235) * t3) * t9) * S12 
     #+ ((-0.432D3 * t208 - 0.135D3 * t6 - 0.135D3 * t3) * t8 - 0.432D3 
     #+ 0.135D3 * t116 * t9) * t114 + ((t253 * t6 + t257 * t3) * t8 + (t
     #263 * t6 + t267 * t3) * t9) * S34 + ((0.135D3 + (-t273 - t274 - t2
     #75 - t276) * t6 + (-t279 - t280 - t281 - t282) * t3) * t9 + 0.495D
     #3 * S14 + 0.495D3 * S13 + (-t289 + t290 - t291 - t292 + t293 - t29
     #4) * t6 + (t297 - t298 + t299 - t300 - t301 - t291) * t3) * t8 + (
     #0.495D3 * S23 + 0.495D3 * S24 + (-t308 + t293 + t299 - t309 - t310
     # - t311) * t6 + (-t314 - t315 - t310 + t290 + t297 - t316) * t3) *
     # t9 + (((-0.414D3 * t208 + t210 + t211) * t8 + 0.621D3 + t215) * t
     #325 + ((t267 * t6 + t263 * t3) * t8 + (t257 * t6 + t253 * t3) * t9
     #) * t114 + t368 * S34 + t443 * t8 + t478 * t9) * t1 + (t206 * t482
     # + ((0.2925D4 / 0.4D1 * t37 * t9 + 0.135D3 / 0.4D1 + (t221 - t222 
     #+ t226) * t6 + (t225 + t222 - t226) * t3) * t8 + (0.135D3 / 0.4D1 
     #+ (-t220 + t227 + t232) * t6 + (t235 - t227 + t220) * t3) * t9) * 
     #t325 + (((0.135D3 + 0.495D3 * t146 * t6 + (0.495D3 * S14 + 0.495D3
     # * S24) * t3) * t9 - t273 - t276 - t281 - t282 + (t290 - t294 - t5
     #07 + t508 - t316 - t289) * t6 + (-t511 + t299 - t298 + t508 - t301
     # - t311) * t3) * t8 + (-t280 - t274 - t279 - t275 + (-t300 + t299 
     #- t308 - t507 + t516 - t309) * t6 + (-t314 + t290 - t292 - t511 - 
     #t315 + t516) * t3) * t9) * t114 + (t549 * t8 + t565 * t9) * S34 + 
     #t613 * t8 + t619 * t9) * t12
      rrgg2ggh11J6 = t623 / pi * wd / z

      end function
  
   
 

      doubleprecision function rrgg2ggh11J7
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
      t37 = S14 * S24
      t38 = 0.45D2 * t37
      t42 = S14 * S23
      t43 = S13 * S14
      t44 = S23 * S24
      t45 = S13 * S24
      t50 = S13 * S23
      t51 = 0.45D2 * t50
      t59 = 0.1D1 / t33 / S12
      t63 = s ** 2
      t65 = z ** 2
      t67 = t16 + t20
      t78 = 0.180D3 * t37
      t79 = 0.45D2 * t45
      t80 = 0.45D2 * t44
      t81 = 0.45D2 * t43
      t82 = 0.45D2 * t42
      t83 = 0.270D3 * t17
      t84 = 0.270D3 * t25
      t87 = 0.180D3 * t50
      t88 = 0.270D3 * t27
      t89 = 0.270D3 * t19
      t94 = 0.180D3 * t44
      t97 = 0.180D3 * t43
      t109 = 0.90D2 * t42
      t111 = 0.90D2 * t45
      t128 = 0.1D1 + t16 * t20 * t9 * t11
      t129 = 0.495D3 * t128
      t138 = S34 ** 2
      t140 = 0.45D2 * S14
      t141 = 0.135D3 * S13
      t142 = 0.135D3 * S23
      t146 = 0.135D3 * S14
      t147 = 0.135D3 * S24
      t148 = 0.45D2 * S13
      t155 = t138 * S34
      t163 = -0.990D3 * t45 + 0.495D3 * t27 + 0.495D3 * t17
      t168 = 0.495D3 * t25 - 0.990D3 * t42 + 0.495D3 * t19
      t178 = 0.180D3 * t25
      t179 = 0.180D3 * t17
      t183 = 0.180D3 * t27
      t185 = 0.180D3 * t19
      t190 = 0.90D2 * t19
      t191 = 0.90D2 * t17
      t192 = S13 * t17
      t194 = t27 * S24
      t196 = t17 * S24
      t199 = (-0.180D3 * t192 + 0.90D2 * t194 + 0.90D2 * t196) * t16
      t200 = t25 * S23
      t202 = S14 * t19
      t204 = t19 * S23
      t207 = (0.90D2 * t200 - 0.180D3 * t202 + 0.90D2 * t204) * t20
      t210 = 0.90D2 * t27
      t211 = 0.90D2 * t25
      t212 = t25 * S14
      t217 = (0.90D2 * t212 + 0.90D2 * t202 - 0.180D3 * t200) * t16
      t219 = t27 * S13
      t223 = (0.90D2 * t192 + 0.90D2 * t219 - 0.180D3 * t194) * t20
      t230 = 0.450D3 * S34
      t269 = -t191 - t211 - t210 + t87 + t78 + 0.540D3 * t45 + 0.540D3 *
     # t42 + t97 + t94 - t190 + (0.180D3 * t212 + 0.180D3 * t196 + 0.180
     #D3 * S13 * t25 + 0.180D3 * t17 * S23) * t16 + (0.180D3 * t27 * S14
     # + 0.180D3 * t219 + 0.180D3 * t204 + 0.180D3 * t19 * S24) * t20
      rrgg2ggh11J7 = (((0.405D3 / 0.2D1 * t3 + 0.405D3 / 0.2D1 * t6) * t
     #9 * t12 * t13 + ((0.180D3 * t16 * t17 + 0.180D3 * t19 * t20) * t11
     # + (0.180D3 * t16 * t25 + 0.180D3 * t27 * t20) * t9) * t34 + ((0.4
     #5D2 / 0.2D1 * t25 - t38 + 0.45D2 / 0.2D1 * t17) * t3 + (0.45D2 * t
     #42 - 0.45D2 * t43 - 0.45D2 * t44 + 0.45D2 * t45) * t16 * t20 + (-t
     #51 + 0.45D2 / 0.2D1 * t19 + 0.45D2 / 0.2D1 * t27) * t6) * t9 * t12
     # * t59) * t63 * t65 + (((0.810D3 + 0.810D3 * t67 * t9) * t11 + 0.8
     #10D3 * S23 + 0.810D3 * S24 + 0.810D3 * S34) * S34 * t13 + ((((-t78
     # - t79 + t80 + t81 - t82 - t83 - t84) * t16 + (-t79 - t87 + t81 - 
     #t82 + t80 - t88 - t89) * t20) * t9 - t82 + t38 + t51 - t94 - t79 -
     # t83 - t89) * t11 + (t38 - t79 - t82 + t51 - t97 - t88 - t84) * t9
     #) * t34 + ((0.45D2 * t25 - 0.90D2 * t37 + 0.45D2 * t17) * t16 * t4
     # - 0.90D2 * t43 + t109 - 0.90D2 * t44 + t111 + (0.45D2 * t19 + 0.4
     #5D2 * t27 - 0.90D2 * t50) * t1 * t20) * t9 * t11 * t59) * s * z + 
     #(t129 * S34 + 0.225D3 * t67 * t9 * t11) * S12 - 0.990D3 * t128 * t
     #138 + (-0.450D3 + (-t140 + t141 + t142 - 0.45D2 * S24) * t16 + (t1
     #46 + t147 - t148 - 0.45D2 * S23) * t20) * t9 * t11 + (t129 * t155 
     #+ ((-0.2430D4 * S23 - 0.2430D4 * S24 - 0.2430D4 * S34 + t163 * t16
     # + t168 * t20) * t11 + (t168 * t16 + t163 * t20) * t9) * S34 + ((t
     #146 + t147 + t142 + t141 + (t97 + t109 + t94 - t178 + t111 - t179 
     #- 0.720D3 * t37) * t16 + (t109 + t94 - t183 - 0.720D3 * t50 - t185
     # + t111 + t97) * t20) * t9 + t190 + t191 + t94 + t82 - t51 - t38 +
     # t79 + t199 + t207) * t11 + (t79 + t210 - t51 - t38 + t97 + t211 +
     # t82 + t217 + t223) * t9) * t13 + ((0.225D3 * S13 + 0.225D3 * S14 
     #+ t230 + 0.225D3 * S23 + 0.225D3 * S24) * t155 + ((-0.495D3 * S23 
     #- 0.495D3 * S24 - t230 + t141 + t146) * t11 + (t147 - t148 - t140 
     #+ t142) * t9) * t138 + (((t146 + t147 + t142 + t141 + (-t81 + t82 
     #- t80 + t211 + t79 + t191 + t78) * t16 + (t82 - t80 + t210 + t87 +
     # t190 + t79 - t81) * t20) * t9 - t185 - t179 - 0.720D3 * t44 + t10
     #9 + t87 + t78 + t111 + t199 + t207) * t11 + (t111 - t183 + t87 + t
     #78 - 0.720D3 * t43 - t178 + t109 + t217 + t223) * t9) * S34 + (t26
     #9 * t9 + 0.180D3 * S13 * t19 + 0.180D3 * t204 + 0.180D3 * t196 + 0
     #.180D3 * S14 * t17) * t11 + (0.180D3 * t212 + 0.180D3 * t27 * S23 
     #+ 0.180D3 * t25 * S24 + 0.180D3 * t219) * t9) * t34) / pi * wd / z

      end function
  
 