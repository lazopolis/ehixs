  
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
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, x
     #4)
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x2 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t16 = t15 * x3
      t17 = t16 * t4
      t20 = log(-0.4D1 * t13 * t17)
      t21 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t23 = t20 ** 2
      t24 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t30 = 0.3141592653589793D1 * lh
      t36 = lh ** 2
      t38 = 0.3141592653589793D1 ** 2
      t40 = 0.180D3 * t36 - 0.30D2 * t38
      t41 = 0.3141592653589793D1 * t40
      t42 = t7 * t24
      t43 = t41 * t42
      t45 = 0.1D1 / x2
      t48 = t12 * t15
      t49 = x3 * t4
      t52 = log(-0.4D1 * t48 * t49)
      t53 = t52 * 0.3141592653589793D1
      t56 = t52 ** 2
      t57 = t56 * 0.3141592653589793D1
      t63 = rrgg2ggh11J4(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t88 = x1 ** 2
      t89 = x2 * t88
      t90 = t89 * t12
      t93 = log(-0.4D1 * t90 * t17)
      t101 = 0.1D1 / x1
      t105 = t88 * t12
      t108 = log(-0.4D1 * t105 * t17)
      t110 = t108 ** 2
      t124 = -(0.90D2 * t8 * (-t9 + t20 * t21 - t23 * t24 / 0.2D1) - 0.1
     #80D3 * t30 * t7 * (-t21 + t20 * t24) - t43) * t45 / 0.1440D4 + (0.
     #180D3 * t53 * lh + 0.45D2 * t57 + t41) * t7 * t21 / 0.1440D4 + t8 
     #* t63 / 0.16D2 + (-0.90D2 * t57 * lh + 0.3141592653589793D1 * (0.6
     #0D2 * lh * t38 - 0.2884936567583026D3 - 0.120D3 * t36 * lh) - 0.15
     #D2 * t56 * t52 * 0.3141592653589793D1 - t53 * t40) * t7 * t24 / 0.
     #1440D4 + (-0.180D3 * t30 - 0.90D2 * t53) * t7 * t9 / 0.1440D4 - (0
     #.90D2 * t8 * (-t21 + t93 * t24) + 0.180D3 * t30 * t42) * t101 * t4
     #5 / 0.720D3 - (0.90D2 * t8 * (-t9 + t108 * t21 - t110 * t24 / 0.2D
     #1) - 0.180D3 * t30 * t7 * (-t21 + t108 * t24) - t43) * t101 / 0.72
     #0D3
      t125 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t
     #124)
      t127 = -0.1D1 + x1
      t128 = x3 * x1
      t129 = t128 * z
      t131 = 0.2D1 * x2 * x3
      t132 = t128 * x2
      t133 = x2 * z
      t134 = t128 * t133
      t135 = cos(t10)
      t136 = -0.1D1 + x2
      t138 = x1 * z
      t139 = 0.1D1 - x1 + t138
      t143 = Sqrt(x3 * t136 * t139 * x2 * t4)
      t145 = 0.2D1 * t135 * t143
      t148 = 0.1D1 / t139
      t151 = t2 * t128
      t152 = x1 * x2
      t153 = t152 * z
      t154 = 0.1D1 - x1 + t138 - x2 + t152 - t153 - x3 + t128 - t129 + t
     #131 - t132 + t134 + t145
      t159 = t2 * x1 * t4
      t160 = t1 ** 2
      t167 = 0.1D1 / (-0.1D1 + x1 - t138 + x2 - t152 - t133 + t153)
      t168 = t139 * t167
      t169 = -t127
      t170 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, t169, x2, x3, x4)
      t172 = t89 * t48
      t173 = t127 ** 2
      t174 = t148 * t173
      t179 = log(0.4D1 * t172 * t49 * t174 * t136)
      t181 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, t169, x2, x3, x4)
      t191 = 0.90D2 * t8 * (t168 * t170 - t179 * t139 * t167 * t181) - 0
     #.180D3 * t30 * t7 * t168 * t181
      t195 = FJET(XB1, XB2, s, t2 * t127 * (-x3 + t128 - t129 + t131 - t
     #132 + t134 - x2 + t145) * t148, t151, -t2 * t127 * t154 * t148, -t
     #159, -s * t160 * x2 * x1 * t127 * t148, -t191 * t101 * t45 / 0.720
     #D3)
      t202 = Sqrt(x2 * t136 * t49)
      t204 = 0.2D1 * t135 * t202
      t210 = 0.1D1 / (0.1D1 - x2 + t133)
      t211 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t217 = log(0.4D1 * t13 * t15 * t49 * t136)
      t218 = t217 * t210
      t219 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t221 = t217 ** 2
      t223 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t229 = t210 * t219
      t236 = t7 * t210 * t223
      t245 = log(0.4D1 * t90 * t16 * t4 * t136)
      t257 = -(0.90D2 * t8 * (t210 * t211 - t218 * t219 + t221 * t210 * 
     #t223 / 0.2D1) - 0.180D3 * t30 * t7 * (t229 - t218 * t223) + t41 * 
     #t236) * t45 / 0.1440D4 - (0.90D2 * t8 * (t229 - t245 * t210 * t223
     #) - 0.180D3 * t30 * t236) * t101 * t45 / 0.720D3
      t258 = FJET(XB1, XB2, s, -t2 * (-x3 + t131 - x2 + t204), 0.0D0, t2
     # * (0.1D1 - x2 - x3 + t131 + t204), 0.0D0, 0.0D0, t257)
      t264 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, t169, 0.0D0, x3, x
     #4)
      t265 = t49 * t174
      t268 = log(-0.4D1 * t172 * t265)
      t269 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, t169, 0.0D0, x3, x
     #4)
      t274 = t7 * t269
      t280 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, t169, 0.0D0, x3, x
     #4)
      t284 = log(-0.4D1 * t105 * t15 * t265)
      t286 = t284 ** 2
      t301 = -(0.90D2 * t8 * (t264 - t268 * t269) - 0.180D3 * t30 * t274
     #) * t101 * t45 / 0.720D3 - (0.90D2 * t8 * (t280 - t284 * t264 + t2
     #86 * t269 / 0.2D1) - 0.180D3 * t30 * t7 * (t264 - t284 * t269) + t
     #41 * t274) * t101 / 0.720D3
      t302 = FJET(XB1, XB2, s, -t2 * t127 * x3, t151, t2 * t127 * t4, -t
     #159, 0.0D0, t301)
      rrgg2gght1s1e1 = t125 * t124 - t195 * t191 * t101 * t45 / 0.720D3 
     #+ t258 * t257 + t302 * t301

      end function



      doubleprecision function rrgg2gght1s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, x
     #4)
      t10 = x4 * 0.3141592653589793D1
      t11 = Sin(t10)
      t12 = t11 ** 2
      t13 = x2 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = t15 * x3 * t4
      t20 = log(-0.4D1 * t13 * t17)
      t21 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t26 = 0.3141592653589793D1 * lh
      t29 = 0.180D3 * t26 * t7 * t21
      t31 = 0.1D1 / x2
      t34 = 0.1D1 / x1
      t39 = x1 ** 2
      t40 = t39 * t12
      t43 = log(-0.4D1 * t40 * t17)
      t51 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t56 = x3 * t4
      t59 = log(-0.4D1 * t12 * t15 * t56)
      t60 = t59 * 0.3141592653589793D1
      t68 = t59 ** 2
      t71 = lh ** 2
      t73 = 0.3141592653589793D1 ** 2
      t81 = -(0.90D2 * t8 * (-t9 + t20 * t21) + t29) * t31 / 0.1440D4 + 
     #t8 * t21 * t34 * t31 / 0.8D1 - (0.90D2 * t8 * (-t9 + t43 * t21) + 
     #t29) * t34 / 0.720D3 + t8 * t51 / 0.16D2 + (-0.180D3 * t26 - 0.90D
     #2 * t60) * t7 * t9 / 0.1440D4 + (0.180D3 * t60 * lh + 0.45D2 * t68
     # * 0.3141592653589793D1 + 0.3141592653589793D1 * (0.180D3 * t71 - 
     #0.30D2 * t73)) * t7 * t21 / 0.1440D4
      t82 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t8
     #1)
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
      t108 = t2 * t85
      t109 = x1 * x2
      t110 = t109 * z
      t111 = 0.1D1 - x1 + t95 - x2 + t109 - t110 - x3 + t85 - t86 + t88 
     #- t89 + t91 + t102
      t116 = t2 * x1 * t4
      t117 = t1 ** 2
      t126 = -t84
      t127 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, t126, x2, x3, x4)
      t130 = 0.1D1 / (-0.1D1 + x1 - t95 + x2 - t109 - t90 + t110) * t127
     # * t34 * t31
      t133 = FJET(XB1, XB2, s, t2 * t84 * (-x3 + t85 - t86 + t88 - t89 +
     # t91 - x2 + t102) * t105, t108, -t2 * t84 * t111 * t105, -t116, -s
     # * t117 * x2 * x1 * t84 * t105, -t8 * t96 * t130 / 0.8D1)
      t141 = Sqrt(x2 * t93 * t56)
      t143 = 0.2D1 * t92 * t141
      t149 = 0.1D1 / (0.1D1 - x2 + t90)
      t150 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t156 = log(0.4D1 * t13 * t15 * t56 * t93)
      t158 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t175 = -(0.90D2 * t8 * (t149 * t150 - t156 * t149 * t158) - 0.180D
     #3 * t26 * t7 * t149 * t158) * t31 / 0.1440D4 - t8 * t149 * t158 * 
     #t34 * t31 / 0.8D1
      t176 = FJET(XB1, XB2, s, -t2 * (-x3 + t88 - x2 + t143), 0.0D0, t2 
     #* (0.1D1 - x2 - x3 + t88 + t143), 0.0D0, 0.0D0, t175)
      t182 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, t126, 0.0D0, x3, x
     #4)
      t187 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, t126, 0.0D0, x3, x
     #4)
      t189 = t84 ** 2
      t194 = log(-0.4D1 * t40 * t15 * t56 * t105 * t189)
      t205 = -t8 * t182 * t34 * t31 / 0.8D1 - (0.90D2 * t8 * (t187 - t19
     #4 * t182) - 0.180D3 * t26 * t7 * t182) * t34 / 0.720D3
      t206 = FJET(XB1, XB2, s, -t2 * t84 * x3, t108, t2 * t84 * t4, -t11
     #6, 0.0D0, t205)
      rrgg2gght1s1e0 = t82 * t81 - t133 * 0.3141592653589793D1 * t7 * t9
     #6 * t130 / 0.8D1 + t176 * t175 + t206 * t205

      end function



      doubleprecision function rrgg2gght1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t2 = (-0.1D1 + z) * s
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, x
     #4)
      t10 = 0.1D1 / x1
      t14 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, 
     #x4)
      t19 = x4 * 0.3141592653589793D1
      t20 = Sin(t19)
      t21 = t20 ** 2
      t22 = z ** 2
      t25 = x3 * t4
      t28 = log(-0.4D1 * t21 / t22 * t25)
      t35 = 0.1D1 / x2
      t39 = t8 * t9 * t10 / 0.8D1 + t8 * t14 / 0.16D2 + (-0.180D3 * 0.31
     #41592653589793D1 * lh - 0.90D2 * t28 * 0.3141592653589793D1) * t7 
     #* t9 / 0.1440D4 + t8 * t9 * t35 / 0.16D2
      t40 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.0D0, t3
     #9)
      t42 = -0.1D1 + x1
      t52 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, -t42, 0.0D0, x3, x4
     #)
      t56 = FJET(XB1, XB2, s, -t2 * t42 * x3, t2 * x1 * x3, t2 * t42 * t
     #4, -t2 * x1 * t4, 0.0D0, -t8 * t52 * t10 / 0.8D1)
      t63 = 0.2D1 * x2 * x3
      t64 = cos(t19)
      t68 = Sqrt(x2 * (-0.1D1 + x2) * t25)
      t70 = 0.2D1 * t64 * t68
      t78 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4)
      t80 = 0.1D1 / (0.1D1 - x2 + x2 * z) * t78 * t35
      t83 = FJET(XB1, XB2, s, -t2 * (-x3 + t63 - x2 + t70), 0.0D0, t2 * 
     #(0.1D1 - x2 - x3 + t63 + t70), 0.0D0, 0.0D0, -t8 * t80 / 0.16D2)
      rrgg2gght1s1em1 = t40 * t39 - t56 * 0.3141592653589793D1 * t7 * t5
     #2 * t10 / 0.8D1 - t83 * 0.3141592653589793D1 * t7 * t80 / 0.16D2

      end function



      doubleprecision function rrgg2gght1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t2 = (-0.1D1 + z) * s
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t9 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, 0.0D0, x3, x
     #4)
      t12 = FJET(XB1, XB2, s, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3), 0.0D0
     #, 0.0D0, 0.3141592653589793D1 * t7 * t9 / 0.16D2)
      rrgg2gght1s1em2 = t12 * 0.3141592653589793D1 * t7 * t9 / 0.16D2

      end function



      doubleprecision function rrgg2gght1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrgg2gght1s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrgg2gght1s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght1s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x2 * t12
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t18 = x3 * t4
      t19 = -0.1D1 + x2
      t23 = log(0.4D1 * t13 * t16 * t18 * t19)
      t24 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t26 = t23 ** 2
      t27 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t30 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t35 = log(-0.4D1 * t13 * t16 * x3 * t4)
      t36 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t38 = t35 ** 2
      t39 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t45 = 0.3141592653589793D1 * lh
      t52 = lh ** 2
      t54 = 0.3141592653589793D1 ** 2
      t56 = 0.180D3 * t52 - 0.30D2 * t54
      t57 = t56 * 0.3141592653589793D1
      t59 = t7 * (t27 - t39)
      t62 = 0.1D1 / x2
      t65 = rrgg2ggh11J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t71 = log(-0.4D1 * t12 * t16 * t18)
      t72 = t71 ** 2
      t73 = t72 * 0.3141592653589793D1
      t85 = t71 * 0.3141592653589793D1
      t104 = x1 ** 2
      t105 = x2 * t104
      t106 = t105 * t16
      t107 = t12 * x3
      t108 = t107 * t4
      t111 = log(-0.4D1 * t106 * t108)
      t117 = log(0.4D1 * t106 * t107 * t4 * t19)
      t125 = 0.1D1 / x1
      t129 = t104 * t16
      t132 = log(-0.4D1 * t129 * t108)
      t134 = t132 ** 2
      t150 = -(0.90D2 * t8 * (t9 - t23 * t24 + t26 * t27 / 0.2D1 - t30 +
     # t35 * t36 - t38 * t39 / 0.2D1) - 0.180D3 * t45 * t7 * (t24 - t23 
     #* t27 - t36 + t35 * t39) + t57 * t59) * t62 / 0.1440D4 + t8 * t65 
     #/ 0.16D2 + (-0.90D2 * t73 * lh + 0.3141592653589793D1 * (0.60D2 * 
     #lh * t54 - 0.2884936567583026D3 - 0.120D3 * t52 * lh) - 0.15D2 * t
     #72 * t71 * 0.3141592653589793D1 - t85 * t56) * t7 * t39 / 0.1440D4
     # + (-0.180D3 * t45 - 0.90D2 * t85) * t7 * t30 / 0.1440D4 + (0.180D
     #3 * t85 * lh + 0.45D2 * t73 + t57) * t7 * t36 / 0.1440D4 - (0.90D2
     # * t8 * (-t36 + t111 * t39 + t24 - t117 * t27) - 0.180D3 * t45 * t
     #59) * t125 * t62 / 0.720D3 - (0.90D2 * t8 * (-t30 + t132 * t36 - t
     #134 * t39 / 0.2D1) - 0.180D3 * t45 * t7 * (-t36 + t132 * t39) - t5
     #7 * t7 * t39) * t125 / 0.720D3
      t151 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #150)
      t153 = x3 * x1
      t155 = -0.1D1 + x1
      t157 = t2 * t155 * x3
      t161 = t2 * t155 * t4
      t162 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t163 = 0.1D1 / t10
      t165 = t105 * t16 * t163
      t166 = x1 * z
      t167 = -z - x1 + t166
      t168 = 0.1D1 / t167
      t169 = t155 ** 2
      t170 = t168 * t169
      t171 = t18 * t170
      t174 = log(0.4D1 * t165 * t171)
      t175 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t180 = t7 * t175
      t186 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t190 = log(0.4D1 * t129 * t163 * t171)
      t192 = t190 ** 2
      t207 = -(0.90D2 * t8 * (t162 - t174 * t175) - 0.180D3 * t45 * t180
     #) * t125 * t62 / 0.720D3 - (0.90D2 * t8 * (t186 - t190 * t162 + t1
     #92 * t175 / 0.2D1) - 0.180D3 * t45 * t7 * (t162 - t190 * t175) + t
     #57 * t180) * t125 / 0.720D3
      t208 = FJET(XB1, XB2, s, t2 * t153, -t157, -t2 * x1 * t4, t161, 0.
     #0D0, t207)
      t210 = x3 * z
      t211 = t153 * z
      t212 = x2 * x3
      t213 = t212 * z
      t214 = t153 * x2
      t215 = x2 * z
      t216 = t153 * t215
      t217 = cos(t14)
      t222 = Sqrt(-x3 * t19 * t167 * x2 * t4)
      t224 = 0.2D1 * t217 * t222
      t229 = x1 * x2
      t230 = t229 * z
      t231 = z + x1 - t166 - t215 - t229 + t230 - t210 - t153 + t211 + t
     #213 + t214 - t216 + t212 + t224
      t235 = t1 ** 2
      t242 = 0.1D1 / (z + x1 - t166 - t229 + t230)
      t243 = t167 * t242
      t244 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t250 = log(-0.4D1 * t165 * t18 * t170 * t19)
      t252 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t262 = 0.90D2 * t8 * (t243 * t244 - t250 * t167 * t242 * t252) - 0
     #.180D3 * t45 * t7 * t243 * t252
      t266 = FJET(XB1, XB2, s, t2 * x1 * (-t210 - t153 + t211 + t213 + t
     #214 - t216 - x2 + t212 + t224) * t168, -t157, -t2 * x1 * t231 * t1
     #68, t161, s * t235 * x2 * x1 * t155 * t168, -t262 * t125 * t62 / 0
     #.720D3)
      rrgg2gght1s2e1 = t151 * t150 + t208 * t207 - t266 * t262 * t125 * 
     #t62 / 0.720D3

      end function



      doubleprecision function rrgg2gght1s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t10 = z ** 2
      t12 = 0.1D1 / t10 / z
      t13 = x2 * t12
      t14 = x4 * 0.3141592653589793D1
      t15 = Sin(t14)
      t16 = t15 ** 2
      t18 = x3 * t4
      t19 = -0.1D1 + x2
      t23 = log(0.4D1 * t13 * t16 * t18 * t19)
      t24 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t26 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t31 = log(-0.4D1 * t13 * t16 * x3 * t4)
      t32 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t37 = 0.3141592653589793D1 * lh
      t38 = t24 - t32
      t43 = 0.1D1 / x2
      t46 = 0.1D1 / x1
      t51 = x1 ** 2
      t52 = t51 * t16
      t57 = log(-0.4D1 * t52 * t12 * x3 * t4)
      t68 = rrgg2ggh11J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t75 = log(-0.4D1 * t12 * t16 * t18)
      t76 = t75 * 0.3141592653589793D1
      t84 = t75 ** 2
      t87 = lh ** 2
      t89 = 0.3141592653589793D1 ** 2
      t97 = -(0.90D2 * t8 * (t9 - t23 * t24 - t26 + t31 * t32) - 0.180D3
     # * t37 * t7 * t38) * t43 / 0.1440D4 - t8 * t38 * t46 * t43 / 0.8D1
     # - (0.90D2 * t8 * (-t26 + t57 * t32) + 0.180D3 * t37 * t7 * t32) *
     # t46 / 0.720D3 + t8 * t68 / 0.16D2 + (-0.180D3 * t37 - 0.90D2 * t7
     #6) * t7 * t26 / 0.1440D4 + (0.180D3 * t76 * lh + 0.45D2 * t84 * 0.
     #3141592653589793D1 + 0.3141592653589793D1 * (0.180D3 * t87 - 0.30D
     #2 * t89)) * t7 * t32 / 0.1440D4
      t98 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t9
     #7)
      t100 = x3 * x1
      t102 = -0.1D1 + x1
      t104 = t2 * t102 * x3
      t108 = t2 * t102 * t4
      t109 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t114 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t117 = x1 * z
      t118 = -z - x1 + t117
      t119 = 0.1D1 / t118
      t120 = t102 ** 2
      t125 = log(0.4D1 * t52 / t10 * t18 * t119 * t120)
      t136 = -t8 * t109 * t46 * t43 / 0.8D1 - (0.90D2 * t8 * (t114 - t12
     #5 * t109) - 0.180D3 * t37 * t7 * t109) * t46 / 0.720D3
      t137 = FJET(XB1, XB2, s, t2 * t100, -t104, -t2 * x1 * t4, t108, 0.
     #0D0, t136)
      t139 = x3 * z
      t140 = t100 * z
      t141 = x2 * x3
      t142 = t141 * z
      t143 = t100 * x2
      t144 = x2 * z
      t145 = t100 * t144
      t146 = cos(t14)
      t151 = Sqrt(-x3 * t19 * t118 * x2 * t4)
      t153 = 0.2D1 * t146 * t151
      t158 = x1 * x2
      t159 = t158 * z
      t160 = z + x1 - t117 - t144 - t158 + t159 - t139 - t100 + t140 + t
     #142 + t143 - t145 + t141 + t153
      t164 = t1 ** 2
      t173 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t176 = 0.1D1 / (z + x1 - t117 - t158 + t159) * t173 * t46 * t43
      t179 = FJET(XB1, XB2, s, t2 * x1 * (-t139 - t100 + t140 + t142 + t
     #143 - t145 - x2 + t141 + t153) * t119, -t104, -t2 * x1 * t160 * t1
     #19, t108, s * t164 * x2 * x1 * t102 * t119, -t8 * t118 * t176 / 0.
     #8D1)
      rrgg2gght1s2e0 = t98 * t97 + t137 * t136 - t179 * 0.31415926535897
     #93D1 * t7 * t118 * t176 / 0.8D1

      end function



      doubleprecision function rrgg2gght1s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t2 = (-0.1D1 + z) * s
      t4 = -0.1D1 + x3
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t8 = 0.3141592653589793D1 * t7
      t9 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x4
     #)
      t10 = 0.1D1 / x1
      t14 = rrgg2ggh11J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x
     #4)
      t19 = z ** 2
      t23 = Sin(x4 * 0.3141592653589793D1)
      t24 = t23 ** 2
      t29 = log(-0.4D1 / t19 / z * t24 * x3 * t4)
      t36 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t42 = t8 * t9 * t10 / 0.8D1 + t8 * t14 / 0.16D2 + (-0.180D3 * 0.31
     #41592653589793D1 * lh - 0.90D2 * t29 * 0.3141592653589793D1) * t7 
     #* t9 / 0.1440D4 - t8 * (t36 - t9) / x2 / 0.16D2
      t43 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t4
     #2)
      t47 = -0.1D1 + x1
      t54 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x4)
      t58 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t47 * x3, -t2 * x1 * t
     #4, t2 * t47 * t4, 0.0D0, -t8 * t54 * t10 / 0.8D1)
      rrgg2gght1s2em1 = t43 * t42 - t58 * 0.3141592653589793D1 * t7 * t5
     #4 * t10 / 0.8D1

      end function



      doubleprecision function rrgg2gght1s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      t2 = (-0.1D1 + z) * s
      t6 = s ** 2
      t7 = 0.1D1 / t6
      t9 = rrgg2ggh11J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, 0.0D0, x3, x4
     #)
      t12 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * (-0.1D1 + x3)
     #, 0.0D0, 0.3141592653589793D1 * t7 * t9 / 0.16D2)
      rrgg2gght1s2em2 = t12 * 0.3141592653589793D1 * t7 * t9 / 0.16D2

      end function



      doubleprecision function rrgg2gght1s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrgg2gght1s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght1s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
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
      rrgg2gght1s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh11J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t6 * t20 - t2 * t24
      t27 = s ** 2
      t28 = t27 ** 2
      t29 = t28 * t27
      t30 = t26 * t29
      t33 = x2 * x3
      t34 = t10 * t7 * t4 + t33 + t19
      t37 = t23 * t10
      t39 = s - t2 * t6 * t34 - t2 * t37
      t40 = t39 ** 2
      t41 = t30 * t40
      t42 = t1 ** 2
      t43 = t42 ** 2
      t44 = x1 ** 2
      t45 = t44 ** 2
      t46 = t43 * t45
      t47 = t4 ** 2
      t49 = 0.1D1 / t47 / t4
      t50 = t20 ** 2
      t51 = t50 * t20
      t53 = t46 * t49 * t51
      t56 = t26 ** 2
      t57 = t28 * s
      t58 = t56 * t57
      t59 = t58 * t40
      t60 = t42 * t44
      t61 = t5 * t20
      t62 = t60 * t61
      t65 = t42 * t1
      t66 = t44 * x1
      t67 = t65 * t66
      t68 = 0.1D1 / t47
      t69 = t68 * t50
      t70 = t67 * t69
      t73 = t57 * z
      t74 = t73 * t56
      t75 = t40 * t42
      t76 = t23 ** 2
      t77 = x3 ** 2
      t78 = t76 * t77
      t82 = t10 ** 2
      t83 = t76 * t82
      t87 = t56 * t29
      t88 = t87 * t39
      t89 = t65 * x1
      t90 = t89 * t83
      t93 = t42 * x1
      t97 = t43 * x1
      t98 = t76 * t23
      t99 = t82 * t10
      t101 = t97 * t98 * t99
      t108 = t89 * t78
      t114 = t77 * x3
      t116 = t97 * t98 * t114
      t126 = z + x1 * t7 * t1
      t127 = t23 * t126
      t128 = t5 * t39
      t132 = 0.2D1 * t41 * t53 + 0.176D3 * t59 * t62 - 0.38D2 * t59 * t7
     #0 + 0.32D2 * t74 * t75 * t78 + 0.32D2 * t74 * t75 * t83 + 0.14D2 *
     # t88 * t90 - 0.6D1 * t88 * t93 * t37 - 0.42D2 * t88 * t101 - 0.32D
     #2 * t59 * t101 - 0.5D1 * t59 * t90 + 0.14D2 * t41 * t108 - 0.6D1 *
     # t41 * t93 * t24 - 0.42D2 * t41 * t116 - 0.32D2 * t59 * t116 - 0.5
     #D1 * t59 * t108 + 0.2D1 * t87 * t1 * t127 * t128
      t134 = t5 * t40
      t135 = t127 * t134
      t142 = t60 * t5 * t34
      t145 = t34 ** 2
      t146 = t68 * t145
      t147 = t67 * t146
      t150 = t145 * t34
      t152 = t46 * t49 * t150
      t175 = t43 * t1
      t176 = t175 * t98
      t177 = t176 * t126
      t178 = t58 * t177
      t179 = t49 * t40
      t180 = t179 * x2
      t181 = t44 * t20
      t182 = t181 * t10
      t183 = t180 * t182
      t186 = t44 * t34
      t187 = t186 * t10
      t191 = -0.260D3 * t58 * t1 * t135 + 0.2D1 * t30 * t1 * t135 + 0.6D
     #1 * t88 * t142 + 0.6D1 * t88 * t147 + 0.2D1 * t88 * t152 + 0.8D1 *
     # t41 * t142 + 0.24D2 * t41 * t147 + 0.32D2 * t41 * t152 + 0.176D3 
     #* t59 * t142 - 0.38D2 * t59 * t147 + 0.8D1 * t88 * t62 + 0.24D2 * 
     #t88 * t70 + 0.32D2 * t88 * t53 + 0.6D1 * t41 * t62 + 0.6D1 * t41 *
     # t70 + 0.133D3 * t178 * t183 + 0.133D3 * t178 * t180 * t187
      t193 = t40 * t43
      t194 = t58 * t193
      t195 = x1 * x2
      t196 = t98 * t5
      t197 = t196 * t77
      t201 = t58 * t75
      t202 = t23 * t5
      t203 = z ** 2
      t204 = t202 * t203
      t208 = t56 * t40
      t210 = t73 * t208 * t42
      t211 = t20 * t23
      t212 = t211 * t10
      t216 = t30 * t193
      t217 = t66 * t68
      t218 = t50 * t23
      t224 = t217 * t218 * t10
      t232 = t40 * t1 * x1
      t237 = t40 * t65
      t238 = t30 * t237
      t239 = t44 * t5
      t240 = t34 * t23
      t241 = t240 * x3
      t242 = t239 * t241
      t245 = t45 * t49
      t247 = t245 * t50 * t34
      t250 = t58 * t237
      t251 = t239 * t212
      t254 = t43 * t98
      t255 = t87 * t254
      t256 = t126 * t5
      t257 = t39 * x1
      t262 = t43 ** 2
      t264 = t87 * t39 * t262
      t265 = x2 ** 2
      t266 = t265 * x2
      t267 = t45 * t266
      t268 = t76 ** 2
      t269 = t268 * t49
      t271 = t267 * t269 * t10
      t274 = t65 * t76
      t275 = t274 * t126
      t276 = t58 * t275
      t277 = t68 * t40
      t278 = t277 * z
      t279 = x1 * t34
      t280 = t279 * t10
      t284 = t65 * t23
      t286 = t58 * t284 * t126
      t288 = t181 * t34
      t292 = t43 * t76
      t293 = t292 * t126
      t294 = t87 * t293
      t295 = t49 * t39
      t296 = t295 * x2
      t301 = 0.32D2 * t194 * t195 * t197 + 0.384D3 * t201 * t195 * t204 
     #- 0.64D2 * t210 * t6 * t212 - 0.6D1 * t216 * t217 * t218 * x3 - 0.
     #32D2 * t194 * t224 + 0.2D1 * t87 * t39 * t1 * x1 - 0.260D3 * t58 *
     # t232 + 0.2D1 * t30 * t232 - 0.48D2 * t238 * t242 + 0.8D1 * t216 *
     # t247 + 0.16D2 * t250 * t251 - 0.48D2 * t255 * t256 * t257 * t77 -
     # 0.6D1 * t264 * t271 + 0.64D2 * t276 * t278 * t280 - 0.64D2 * t286
     # * t179 * z * t288 + 0.384D3 * t294 * t296 * t186 * z
      t302 = t29 * t203
      t303 = t26 * t40
      t304 = t302 * t303
      t308 = t58 * t237 * t44
      t309 = z * t5
      t314 = t175 * t76 * t126
      t315 = t87 * t314
      t316 = t66 * t50
      t317 = t316 * t10
      t321 = t87 * t177
      t322 = t68 * t39
      t323 = t181 * t82
      t327 = t87 * t274
      t332 = t30 * t314
      t333 = t47 ** 2
      t334 = 0.1D1 / t333
      t335 = t334 * t40
      t336 = x2 * t66
      t337 = t336 * t145
      t338 = t335 * t337
      t341 = t43 * t42
      t342 = t40 * t341
      t344 = t30 * t342 * t45
      t345 = x2 * t76
      t346 = t49 * t145
      t352 = t30 * t342 * t66
      t353 = x2 * t98
      t354 = t68 * t34
      t359 = t27 * s
      t360 = t28 * t359
      t361 = t26 * t360
      t362 = t341 * t98
      t363 = t362 * t126
      t364 = t361 * t363
      t365 = t295 * t66
      t366 = x2 * t20
      t367 = t366 * t10
      t368 = t365 * t367
      t371 = t43 * t65
      t373 = t371 * t98 * t126
      t374 = t87 * t373
      t375 = t334 * t39
      t376 = t45 * t265
      t382 = t341 * t76 * t126
      t383 = t30 * t382
      t385 = 0.1D1 / t333 / t4
      t386 = t385 * t40
      t387 = t386 * x2
      t388 = t45 * t20
      t389 = t388 * t145
      t393 = t43 * t268
      t394 = t58 * t393
      t395 = t40 * t82
      t397 = t256 * t395 * x3
      t400 = t44 * t145
      t401 = t400 * t10
      t405 = t254 * t126
      t406 = t87 * t405
      t411 = t262 * t268
      t413 = t126 * t334
      t414 = t39 * t45
      t416 = t413 * t414 * t266
      t419 = t40 * t175
      t420 = t58 * t419
      t421 = t45 * x2
      t422 = t23 * t49
      t423 = t422 * t50
      t424 = t421 * t423
      t427 = t39 * t371
      t428 = t87 * t427
      t429 = t66 * t265
      t430 = t268 * t68
      t432 = t429 * t430 * t82
      t435 = 0.32D2 * t304 * t147 + 0.64D2 * t308 * t309 * t212 + 0.32D2
     # * t315 * t295 * t317 - 0.64D2 * t321 * t322 * t323 + 0.205D3 * t3
     #27 * t256 * t257 * x3 - 0.32D2 * t332 * t338 - 0.120D3 * t344 * t3
     #45 * t346 * x3 + 0.104D3 * t352 * t353 * t354 * t77 - 0.48D2 * t36
     #4 * t368 - 0.176D3 * t374 * t375 * t376 * t20 - 0.6D1 * t383 * t38
     #7 * t389 - 0.32D2 * t394 * t397 + 0.14D2 * t294 * t295 * t401 - 0.
     #6D1 * t406 * t322 * t279 * t82 - 0.240D3 * t361 * t411 * t416 + 0.
     #48D2 * t420 * t424 + 0.14D2 * t428 * t432
      t438 = t360 * t203
      t439 = t438 * t341
      t440 = t98 * t126
      t442 = t439 * t440 * t49
      t444 = t10 * t39
      t449 = t360 * z
      t450 = t26 * t341
      t452 = t449 * t450 * t76
      t453 = t413 * t39
      t454 = t421 * t50
      t458 = t40 * t371
      t459 = t30 * t458
      t461 = t429 * t430 * t77
      t464 = t39 * t341
      t465 = t87 * t464
      t466 = t45 * x1
      t467 = t466 * x2
      t468 = t23 * t334
      t473 = t411 * t126
      t474 = t30 * t473
      t475 = t267 * t34
      t480 = t341 * t268 * t126
      t481 = t87 * t480
      t482 = t44 * x2
      t483 = t482 * t77
      t484 = t322 * t483
      t487 = t56 * t65
      t493 = t42 * t76
      t494 = t58 * t493
      t495 = t40 * x3
      t496 = t256 * t495
      t499 = t43 * t23
      t500 = t58 * t499
      t501 = t40 * t66
      t503 = t413 * t501 * t51
      t509 = t58 * t293
      t510 = t181 * x3
      t511 = t277 * t510
      t514 = t58 * t405
      t516 = x1 * x3 * t10
      t517 = t134 * t516
      t524 = t30 * t499
      t525 = t126 * t49
      t527 = t525 * t501 * t145
      t530 = t30 * t284
      t531 = t126 * t68
      t532 = t40 * t44
      t534 = t531 * t532 * t20
      t537 = t211 * x3
      t538 = t239 * t537
      t541 = -0.16D2 * t442 * t336 * t26 * t444 * t20 + 0.32D2 * t452 * 
     #t453 * t454 + 0.14D2 * t459 * t461 + 0.32D2 * t465 * t467 * t468 *
     # t51 + 0.6D1 * t474 * t386 * t475 + 0.38D2 * t481 * t484 + 0.32D2 
     #* t302 * t487 * t440 * t128 * t77 + 0.176D3 * t494 * t496 - 0.32D2
     # * t500 * t503 + 0.384D3 * t59 * t3 * t203 - 0.32D2 * t509 * t511 
     #- 0.32D2 * t514 * t517 - 0.64D2 * t406 * t322 * t195 * t10 - 0.48D
     #2 * t524 * t527 - 0.67D2 * t530 * t534 - 0.12D2 * t238 * t538
      t542 = x2 * t34
      t543 = t542 * x3
      t544 = t365 * t543
      t547 = t20 * t34
      t548 = t217 * t547
      t551 = t39 * t43
      t552 = t87 * t551
      t555 = t20 * t76
      t557 = t239 * t555 * t82
      t560 = t422 * t145
      t565 = t58 * t193 * x2
      t566 = t44 * t76
      t567 = t354 * t10
      t571 = t400 * x3
      t575 = z * t44
      t580 = t26 * t65
      t584 = t525 * t532 * t50
      t588 = t525 * t501 * t50
      t591 = t58 * t254
      t592 = t40 * x1
      t594 = t256 * t592 * t82
      t603 = t68 * t20
      t604 = t240 * t10
      t605 = t603 * t604
      t608 = t87 * t499
      t609 = t39 * t66
      t620 = t30 * t393
      t625 = -0.48D2 * t364 * t544 + 0.16D2 * t238 * t548 - 0.120D3 * t5
     #52 * t224 + 0.104D3 * t552 * t557 + 0.32D2 * t194 * t336 * t560 + 
     #0.48D2 * t565 * t566 * t567 + 0.104D3 * t294 * t295 * t571 - 0.32D
     #2 * t286 * t179 * t575 * t145 + 0.32D2 * t302 * t580 * t23 * t584 
     #+ 0.144D3 * t500 * t588 + 0.144D3 * t591 * t594 - 0.32D2 * t286 * 
     #t179 * t575 * t50 - 0.48D2 * t87 * t551 * t66 * t605 - 0.48D2 * t6
     #08 * t525 * t609 * t50 + 0.74D2 * t255 * t256 * t257 * t82 - 0.42D
     #2 * t524 * t503 + 0.32D2 * t620 * t256 * t40 * t99
      t627 = t371 * t268
      t628 = t627 * t126
      t629 = t87 * t628
      t630 = t375 * t265
      t631 = t66 * t34
      t636 = t66 * t20
      t642 = t30 * t40 * t262
      t645 = t58 * t363
      t647 = t335 * t429 * t34
      t650 = t58 * t480
      t651 = t265 * t44
      t653 = t179 * t651 * x3
      t656 = t87 * t275
      t657 = t195 * t203
      t661 = t30 * t275
      t665 = t30 * t480
      t666 = t277 * t483
      t671 = t5 * x3 * t10
      t675 = t179 * t66
      t680 = t336 * t34
      t681 = t26 * t39
      t686 = t421 * t145
      t693 = t651 * t10
      t697 = t429 * t20
      t701 = x1 * z
      t702 = t701 * t77
      t706 = t499 * t126
      t707 = t87 * t706
      t708 = t66 * z
      t709 = t708 * t145
      t713 = -0.48D2 * t629 * t630 * t631 * x3 + 0.16D2 * t629 * t630 * 
     #t636 * x3 + 0.6D1 * t642 * t271 + 0.67D2 * t645 * t647 - 0.205D3 *
     # t650 * t653 - 0.384D3 * t656 * t322 * t657 - 0.384D3 * t661 * t27
     #7 * t657 + 0.5D1 * t665 * t666 - 0.64D2 * t565 * x1 * t98 * t671 -
     # 0.384D3 * t332 * t675 * t542 * z - 0.16D2 * t442 * t680 * t681 * 
     #x3 + 0.32D2 * t452 * t453 * t686 + 0.38D2 * t383 * t335 * t686 - 0
     #.205D3 * t650 * t179 * t693 + 0.67D2 * t645 * t335 * t697 - 0.32D2
     # * t406 * t128 * t702 + 0.32D2 * t707 * t295 * t709
      t714 = t30 * t293
      t715 = t277 * t44
      t716 = z * t20
      t717 = t716 * t10
      t725 = t87 * t284
      t726 = t39 * t44
      t731 = t482 * t197
      t734 = t421 * t560
      t737 = t58 * t342
      t738 = t98 * t68
      t740 = t429 * t738 * t10
      t743 = z * t34
      t744 = t743 * x3
      t749 = t245 * t20 * t145
      t752 = t145 * t23
      t754 = t217 * t752 * x3
      t757 = t30 * t177
      t760 = t186 * x3
      t761 = t180 * t760
      t764 = t73 * t208
      t765 = x3 * t10
      t772 = t87 * t363
      t773 = t542 * t10
      t774 = t365 * t773
      t777 = t262 * t1
      t778 = t268 * t23
      t779 = t777 * t778
      t781 = t126 * t385
      t782 = t265 ** 2
      t791 = t413 * t40 * t266 * t66
      t794 = t354 * x3
      t798 = -0.64D2 * t714 * t715 * t717 - 0.67D2 * t327 * t256 * t257 
     #* t10 - 0.67D2 * t725 * t531 * t726 * t34 - 0.74D2 * t420 * t731 +
     # 0.48D2 * t420 * t734 + 0.67D2 * t737 * t740 - 0.64D2 * t714 * t71
     #5 * t744 + 0.24D2 * t216 * t749 - 0.120D3 * t216 * t754 - 0.32D2 *
     # t757 * t183 + 0.48D2 * t757 * t761 - 0.64D2 * t764 * t493 * t765 
     #- 0.64D2 * t210 * t6 * t241 - 0.74D2 * t772 * t774 + 0.2D1 * t30 *
     # t779 * t781 * t40 * t782 * t45 + 0.7D1 * t58 * t627 * t791 - 0.48
     #D2 * t565 * t566 * t794
      t802 = t40 * t77
      t804 = t256 * t802 * t10
      t822 = t58 * t314
      t823 = t335 * x2
      t824 = t636 * t34
      t828 = t175 * t23
      t829 = t828 * t126
      t832 = t45 * t50 * t34
      t836 = t66 * t145
      t837 = t836 * t10
      t846 = t265 * t268 * t68 * x3 * t10
      t852 = t175 * t268
      t858 = t277 * t182
      t863 = t30 * t405
      t868 = t322 * t44
      t870 = t20 * t10 * x3
      t874 = t30 * t274
      t876 = t256 * t592 * t10
      t880 = t531 * t532 * t34
      t883 = 0.8D1 * t620 * t804 - 0.32D2 * t30 * t627 * t791 + 0.48D2 *
     # t30 * t176 * t525 * t40 * t265 * t44 + 0.2D1 * t87 * t779 * t781 
     #* t39 * t782 * t45 - 0.148D3 * t822 * t823 * t824 + 0.32D2 * t30 *
     # t829 * t335 * t832 + 0.32D2 * t332 * t179 * t837 - 0.12D2 * t87 *
     # t427 * t66 * t846 + 0.48D2 * t321 * t296 * t182 + 0.32D2 * t30 * 
     #t852 * t256 * t592 * t114 - 0.208D3 * t509 * t858 + 0.48D2 * t665 
     #* t653 - 0.64D2 * t863 * t277 * t195 * x3 - 0.64D2 * t321 * t868 *
     # t870 + 0.205D3 * t874 * t876 + 0.205D3 * t530 * t880
      t884 = t239 * t604
      t890 = t30 * t342 * t44
      t891 = x2 * t268
      t894 = t891 * t5 * t82 * x3
      t898 = t30 * t458 * t45
      t899 = t265 * t98
      t900 = t49 * t34
      t905 = t716 * x3
      t911 = t385 * t39
      t912 = t376 * t145
      t917 = t262 * t778 * t126
      t919 = t266 * t66
      t924 = t277 * x1
      t925 = t924 * t870
      t928 = t44 * t50
      t930 = t179 * t928 * t10
      t935 = t531 * t532 * x2
      t938 = t438 * t480
      t941 = t487 * t76
      t943 = t40 * x2
      t945 = t531 * t943 * x1
      t948 = t23 * t68
      t954 = t429 * t738 * x3
      t957 = t33 * t10
      t958 = t868 * t957
      t963 = -0.80D2 * t250 * t884 + 0.16D2 * t250 * t548 - 0.6D1 * t890
     # * t894 + 0.16D2 * t898 * t899 * t900 * t10 + 0.64D2 * t714 * t715
     # * t905 + 0.24D2 * t620 * t397 + 0.14D2 * t374 * t911 * t912 + 0.8
     #D1 * t30 * t917 * t335 * t919 * t10 - 0.48D2 * t863 * t925 + 0.104
     #D3 * t714 * t930 - 0.72D2 * t438 * t292 * t935 - 0.8D1 * t938 * t6
     #66 + 0.1028D4 * t73 * t941 * t945 - 0.64D2 * t552 * t336 * t948 * 
     #t34 - 0.64D2 * t465 * t954 + 0.80D2 * t481 * t958 - 0.16D2 * t772 
     #* t544
      t966 = t87 * t427 * t45
      t967 = t49 * t20
      t972 = t419 * t66
      t973 = t58 * t972
      t974 = t603 * x3
      t978 = t852 * t126
      t979 = t58 * t978
      t984 = t335 * t454
      t987 = t482 * t82
      t1004 = t68 * t44
      t1010 = t56 * t43
      t1012 = t76 * t126
      t1017 = t87 * t382
      t1018 = t421 * t150
      t1023 = t341 * t778 * t126
      t1030 = t87 * t39 * t65
      t1034 = t87 * t464 * t45
      t1035 = t49 * t50
      t1040 = t421 * t51
      t1047 = 0.16D2 * t966 * t899 * t967 * x3 + 0.133D3 * t973 * t345 *
     # t974 + 0.96D2 * t979 * t277 * x2 * t516 + 0.5D1 * t383 * t984 + 0
     #.38D2 * t665 * t277 * t987 - 0.32D2 * t509 * t179 * t401 + 0.133D3
     # * t973 * t345 * t567 + 0.32D2 * t87 * t829 * t375 * t389 - 0.133D
     #3 * t707 * t295 * t824 - 0.8D1 * t938 * t1004 * x2 * t56 * t82 - 0
     #.72D2 * t438 * t1010 * t1012 * t1004 * x2 - 0.42D2 * t1017 * t911 
     #* t1018 + 0.32D2 * t87 * t1023 * t322 * t195 * t114 + 0.16D2 * t10
     #30 * t548 + 0.24D2 * t1034 * t345 * t1035 * x3 - 0.42D2 * t383 * t
     #386 * t1040 + 0.2D1 * t383 * t386 * t1018
      t1048 = t322 * t987
      t1052 = t440 * t334
      t1054 = t39 * x2
      t1056 = t547 * x3
      t1060 = t30 * t706
      t1064 = t708 * t50
      t1068 = t701 * t82
      t1072 = t30 * t254
      t1076 = t388 * t34
      t1088 = t525 * t726 * t145
      t1093 = z * x3 * t10
      t1097 = t743 * t10
      t1102 = t365 * t366 * x3
      t1105 = t466 * t266
      t1106 = t98 * t334
      t1111 = t716 * t34
      t1115 = t438 * t382
      t1122 = t482 * t203
      t1123 = t322 * t1122
      t1126 = 0.5D1 * t481 * t1048 - 0.48D2 * t87 * t341 * t1052 * t1054
     # * t66 * t1056 - 0.32D2 * t1060 * t179 * t709 - 0.32D2 * t707 * t2
     #95 * t1064 + 0.32D2 * t406 * t128 * t1068 - 0.48D2 * t1072 * t594 
     #- 0.12D2 * t374 * t911 * t265 * t1076 + 0.32D2 * t302 * t580 * t44
     #0 * t134 * t82 + 0.32D2 * t302 * t487 * t23 * t1088 - 0.64D2 * t40
     #6 * t128 * x1 * t1093 + 0.64D2 * t294 * t868 * t1097 - 0.16D2 * t7
     #72 * t1102 + 0.8D1 * t264 * t1105 * t1106 * t20 - 0.64D2 * t707 * 
     #t365 * t1111 - 0.8D1 * t1115 * t984 + 0.384D3 * t714 * t180 * t575
     # * t20 - 0.384D3 * t294 * t1123
      t1132 = t29 * z
      t1136 = t531 * t726 * x2
      t1141 = x1 * t20
      t1143 = t277 * t1141 * t82
      t1146 = t482 * t202
      t1149 = t87 * t393
      t1158 = t375 * t686
      t1168 = t40 * t45
      t1173 = t203 * z
      t1176 = t26 * t43 * t76
      t1186 = t40 * z
      t1195 = t277 * t760
      t1198 = -0.384D3 * t714 * t277 * t1122 - 0.1028D4 * t1132 * t1010 
     #* t76 * t1136 + 0.64D2 * t509 * t930 - 0.32D2 * t514 * t1143 - 0.3
     #2D2 * t238 * t1146 + 0.2D1 * t1149 * t256 * t39 * t99 + 0.32D2 * t
     #1149 * t256 * t39 * t114 + 0.5D1 * t1017 * t1158 + 0.32D2 * t250 *
     # t708 * t146 + 0.260D3 * t87 * t411 * t416 + 0.260D3 * t30 * t411 
     #* t413 * t1168 * t266 + 0.128D3 * t360 * t1173 * t1176 * t1136 + 0
     #.384D3 * t58 * t1 * t23 * t256 * t40 * t203 + 0.384D3 * t494 * t25
     #6 * t1186 * t10 - 0.176D3 * t629 * t295 * t429 * x3 - 0.133D3 * t7
     #14 * t1195
      t1204 = t42 * t23
      t1205 = t30 * t1204
      t1210 = t65 * t98
      t1211 = t30 * t1210
      t1212 = t256 * t395
      t1215 = t279 * x3
      t1219 = t1141 * x3
      t1228 = t277 * t1219
      t1232 = t256 * t495 * t10
      t1235 = t449 * t26
      t1239 = t1235 * t363
      t1243 = t449 * t450 * t268
      t1244 = t531 * t39
      t1248 = t76 * t5
      t1263 = t58 * t1210
      t1266 = 0.86D2 * t58 * t362 * t525 * t501 * t265 - 0.6D1 * t1205 *
     # t531 * t592 * t20 + 0.24D2 * t1211 * t1212 + 0.64D2 * t276 * t278
     # * t1215 + 0.64D2 * t276 * t278 * t1219 + 0.64D2 * t58 * t1210 * t
     #126 * t134 * t1093 - 0.12D2 * t661 * t1228 + 0.16D2 * t1211 * t123
     #2 + 0.64D2 * t1235 * t480 * t958 - 0.64D2 * t1239 * t774 + 0.32D2 
     #* t1243 * t1244 * t483 + 0.48D2 * t552 * t482 * t1248 * x3 + 0.133
     #D3 * t178 * t180 * t510 - 0.32D2 * t194 * t247 + 0.64D2 * t308 * t
     #309 * t241 - 0.80D2 * t276 * t1228 + 0.16D2 * t1263 * t1232
      t1271 = t76 * t68
      t1282 = t466 * t782 * t268 * t334
      t1285 = t30 * t363
      t1292 = t30 * t419
      t1293 = t429 * t1271
      t1316 = t56 * t39
      t1317 = t302 * t1316
      t1320 = t58 * t1204
      t1328 = t836 * x3
      t1335 = 0.32D2 * t757 * t277 * t323 + 0.32D2 * t194 * t651 * t1271
     # + 0.32D2 * t194 * t336 * t423 + 0.2D1 * t87 * t39 * t777 * t1282 
     #+ 0.8D1 * t1285 * t823 * t837 + 0.16D2 * t276 * t277 * t1215 + 0.4
     #8D2 * t1292 * t1293 + 0.2D1 * t30 * t40 * t777 * t1282 - 0.32D2 * 
     #t327 * t531 * t1054 * x1 + 0.384D3 * t87 * t551 * t44 * t345 * t30
     #9 * t10 + 0.384D3 * t201 * t575 * t61 + 0.384D3 * t201 * t239 * t7
     #43 + 0.32D2 * t1317 * t90 + 0.768D3 * t1320 * t256 * t592 * z + 0.
     #32D2 * t764 * t60 * t146 - 0.64D2 * t315 * t295 * t1328 - 0.133D3 
     #* t294 * t322 * t760
      t1338 = t196 * t82
      t1342 = t76 * t49
      t1344 = t376 * t1342 * t20
      t1347 = t30 * t972
      t1348 = t603 * t10
      t1349 = t345 * t1348
      t1369 = t30 * t493
      t1371 = t256 * t40 * t10
      t1390 = x2 * t23
      t1391 = t967 * t34
      t1397 = -0.133D3 * t714 * t858 + 0.32D2 * t194 * t195 * t1338 - 0.
     #205D3 * t737 * t1344 + 0.48D2 * t1347 * t1349 - 0.64D2 * t302 * t9
     #41 * t1244 * t1215 + 0.64D2 * t210 * t6 * t537 - 0.64D2 * t1285 * 
     #t647 + 0.48D2 * t714 * t179 * t482 * t34 + 0.2D1 * t620 * t256 * t
     #40 * t114 + 0.8D1 * t1369 * t1371 + 0.6D1 * t1369 * t496 - 0.12D2 
     #* t30 * t458 * t66 * t846 - 0.48D2 * t898 * t899 * t900 * x3 + 0.1
     #48D3 * t714 * t511 - 0.133D3 * t863 * t517 + 0.96D2 * t58 * t419 *
     # t45 * t1390 * t1391 - 0.32D2 * t194 * t754
      t1402 = t911 * x2
      t1424 = t179 * t928 * x3
      t1431 = t376 * t1342 * t34
      t1445 = t444 * x3
      t1451 = t256 * t802
      t1460 = -0.6D1 * t1017 * t1402 * t832 + 0.14D2 * t1017 * t1402 * t
     #389 - 0.64D2 * t1060 * t675 * t1111 - 0.64D2 * t294 * t868 * t717 
     #- 0.64D2 * t294 * t868 * t744 - 0.64D2 * t294 * t868 * t905 - 0.32
     #D2 * t394 * t804 - 0.32D2 * t509 * t1424 + 0.14D2 * t383 * t387 * 
     #t832 - 0.205D3 * t737 * t1431 + 0.16D2 * t439 * t1012 * t334 * t42
     #1 * t34 * t681 * t20 + 0.16D2 * t439 * t268 * t126 * t68 * t482 * 
     #t26 * t1445 - 0.38D2 * t1263 * t1212 - 0.38D2 * t1263 * t1451 - 0.
     #128D3 * t315 * t375 * t337 - 0.16D2 * t1285 * t675 * t543
      t1461 = t277 * t187
      t1464 = t179 * t824
      t1467 = t375 * x2
      t1480 = t30 * t373
      t1485 = t30 * t628
      t1486 = t335 * t265
      t1494 = t98 * t49
      t1509 = t376 * t50
      t1521 = t87 * t1210
      t1525 = -0.96D2 * t714 * t1461 - 0.133D3 * t1060 * t1464 + 0.104D3
     # * t772 * t1467 * t1328 - 0.32D2 * t58 * t706 * t1464 + 0.104D3 * 
     #t1285 * t823 * t317 - 0.120D3 * t665 * t180 * t323 - 0.12D2 * t148
     #0 * t386 * t265 * t1076 + 0.16D2 * t1485 * t1486 * t631 * t10 - 0.
     #240D3 * t201 * t195 * t202 + 0.128D3 * t737 * t919 * t1494 + 0.24D
     #2 * t344 * t345 * t346 * t10 + 0.8D1 * t352 * t353 * t354 * t82 + 
     #0.6D1 * t1480 * t386 * t912 + 0.6D1 * t374 * t911 * t1509 - 0.64D2
     # * t302 * t1316 * t65 * t251 - 0.64D2 * t302 * t303 * t65 * t242 +
     # 0.16D2 * t1521 * t256 * t1445
      t1530 = t336 * t50
      t1531 = t335 * t1530
      t1534 = t30 * t978
      t1535 = t195 * t82
      t1536 = t277 * t1535
      t1539 = t195 * t77
      t1540 = t277 * t1539
      t1547 = t1141 * t10
      t1548 = t277 * t1547
      t1551 = t87 * t978
      t1569 = t87 * t464 * t44
      t1577 = t267 * t269 * x3
      t1585 = t87 * t493
      t1589 = -0.12D2 * t656 * t322 * t280 - 0.128D3 * t332 * t1531 + 0.
     #240D3 * t1534 * t1536 - 0.32D2 * t1534 * t1540 - 0.42D2 * t608 * t
     #413 * t609 * t150 + 0.16D2 * t276 * t1548 + 0.240D3 * t1551 * t322
     # * t1539 + 0.67D2 * t737 * t954 - 0.64D2 * t565 * t66 * t23 * t139
     #1 + 0.48D2 * t565 * t566 * t974 + 0.48D2 * t979 * t1540 - 0.74D2 *
     # t822 * t338 + 0.14D2 * t1569 * t894 + 0.8D1 * t642 * t1105 * t110
     #6 * t34 - 0.6D1 * t642 * t1577 + 0.32D2 * t30 * t1023 * t277 * t19
     #5 * t99 + 0.6D1 * t1585 * t256 * t444
      t1598 = t482 * t1338
      t1606 = t345 * t794
      t1621 = t438 * t363
      t1623 = t49 * t66 * x2
      t1625 = t1623 * t495 * t20
      t1628 = t482 * z
      t1633 = t256 * t592 * x3
      t1652 = 0.14D2 * t725 * t1088 - 0.96D2 * t294 * t322 * t510 - 0.13
     #3D3 * t406 * t128 * t516 - 0.74D2 * t420 * t1598 - 0.148D3 * t58 *
     # t419 * t44 * t353 * t671 + 0.133D3 * t973 * t1606 - 0.32D2 * t321
     # * t296 * t760 + 0.80D2 * t665 * t715 * t957 + 0.384D3 * t30 * t19
     #3 * t44 * t345 * t309 * x3 + 0.16D2 * t1621 * t1625 + 0.768D3 * t5
     #09 * t277 * t1628 - 0.67D2 * t874 * t1633 + 0.32D2 * t30 * t828 * 
     #t413 * t1168 * t51 - 0.64D2 * t1239 * t368 - 0.64D2 * t1239 * t110
     #2 + 0.488D3 * t822 * t179 * t680 + 0.488D3 * t178 * t277 * t482 * 
     #t10
      t1655 = t375 * t454
      t1658 = t361 * t382
      t1672 = t186 * t77
      t1680 = t39 * t77
      t1686 = t361 * t480
      t1689 = t603 * t34
      t1696 = t268 * t5
      t1698 = t482 * t1696 * t114
      t1701 = t466 * t265
      t1702 = t76 * t334
      t1707 = t193 * t66
      t1708 = t58 * t1707
      t1709 = t354 * t537
      t1715 = 0.38D2 * t1017 * t1655 + 0.32D2 * t1658 * t1158 + 0.64D2 *
     # t210 * t6 * t604 + 0.384D3 * t194 * t651 * t1271 * z + 0.48D2 * t
     #216 * t482 * t1248 * t10 + 0.32D2 * t321 * t322 * t1672 + 0.8D1 * 
     #t1585 * t256 * t39 * x3 + 0.24D2 * t1521 * t256 * t1680 + 0.32D2 *
     # t1658 * t1655 + 0.32D2 * t1686 * t1048 - 0.64D2 * t764 * t60 * t1
     #689 + 0.32D2 * t764 * t60 * t69 + 0.2D1 * t465 * t1698 + 0.24D2 * 
     #t428 * t1701 * t1702 * t50 + 0.64D2 * t1708 * t1709 - 0.48D2 * t30
     # * t1707 * t1709
      t1721 = t366 * t34
      t1722 = t375 * t45 * t1721
      t1736 = t482 * t204
      t1742 = t87 * t1204
      t1746 = t256 * t592
      t1760 = t482 * t1696 * t99
      t1771 = 0.24D2 * t552 * t247 - 0.64D2 * t364 * t774 + 0.48D2 * t16
     #58 * t1722 - 0.16D2 * t1285 * t675 * t367 - 0.16D2 * t1285 * t675 
     #* t773 + 0.1028D4 * t764 * t65 * t44 * t1390 * t5 - 0.384D3 * t103
     #0 * t1736 + 0.64D2 * t308 * t309 * t604 - 0.7D1 * t1742 * t256 * t
     #257 - 0.7D1 * t1205 * t1746 - 0.48D2 * t1030 * t251 + 0.148D3 * t2
     #94 * t322 * t187 + 0.32D2 * t361 * t362 * t525 * t609 * t265 - 0.4
     #2D2 * t465 * t1760 + 0.64D2 * t1235 * t382 * t1722 + 0.32D2 * t124
     #3 * t1244 * t987 + 0.6D1 * t428 * t461
      t1773 = t353 * t68
      t1775 = t34 * x3 * t10
      t1822 = t361 * t293
      t1836 = t279 * t77
      t1840 = -0.48D2 * t352 * t1773 * t1775 - 0.64D2 * t302 * t580 * t7
     #6 * t531 * t40 * t1547 - 0.120D3 * t481 * t296 * t1672 - 0.64D2 * 
     #t863 * t134 * x1 * t1093 - 0.64D2 * t714 * t715 * t1097 - 0.32D2 *
     # t1347 * t1606 + 0.128D3 * t361 * t292 * t1136 + 0.48D2 * t294 * t
     #295 * t482 * t20 + 0.32D2 * t87 * t828 * t413 * t414 * t150 + 0.32
     #D2 * t87 * t852 * t256 * t257 * t99 + 0.16D2 * t1621 * t1623 * t34
     # * t56 * t10 - 0.1028D4 * t1132 * t1176 * t935 + 0.384D3 * t1822 *
     # t1123 + 0.74D2 * t608 * t525 * t609 * t145 + 0.205D3 * t725 * t53
     #1 * t726 * t20 + 0.64D2 * t509 * t179 * t571 - 0.32D2 * t514 * t27
     #7 * t1836
      t1845 = t891 * t5 * t77 * t10
      t1849 = t87 * t464 * t66
      t1855 = t58 * t284
      t1864 = t39 * t82
      t1874 = t30 * t342
      t1883 = t34 * t76
      t1885 = t239 * t1883 * t82
      t1888 = t39 * t175
      t1889 = t87 * t1888
      t1894 = 0.64D2 * t194 * t557 + 0.14D2 * t890 * t1845 - 0.48D2 * t1
     #849 * t1773 * t870 - 0.16D2 * t772 * t368 - 0.5D1 * t1855 * t525 *
     # t532 * t145 + 0.176D3 * t494 * t1371 + 0.48D2 * t979 * t1536 + 0.
     #6D1 * t1521 * t256 * t1864 - 0.48D2 * t406 * t322 * x1 * t1775 - 0
     #.5D1 * t1855 * t584 + 0.2D1 * t1874 * t1760 - 0.42D2 * t1874 * t16
     #98 + 0.6D1 * t459 * t432 - 0.32D2 * t194 * t749 - 0.32D2 * t194 * 
     #t1885 - 0.32D2 * t1889 * t731 - 0.32D2 * t1889 * t734
      t1906 = t186 * t82
      t1933 = t267 * t20
      t1943 = 0.48D2 * t465 * t1431 - 0.64D2 * t364 * t1102 + 0.48D2 * t
     #1686 * t958 - 0.12D2 * t1030 * t884 + 0.24D2 * t665 * t180 * t1906
     # + 0.32D2 * t1317 * t70 + 0.32D2 * t304 * t108 - 0.208D3 * t509 * 
     #t1195 - 0.64D2 * t332 * t179 * t317 - 0.32D2 * t315 * t375 * t1530
     # - 0.32D2 * t1551 * t322 * t1535 - 0.384D3 * t238 * t1736 - 0.8D1 
     #* t1115 * t334 * t45 * x2 * t145 * t56 - 0.6D1 * t474 * t386 * t19
     #33 + 0.14D2 * t714 * t1424 - 0.6D1 * t863 * t277 * t1141 * t77
      t1951 = t239 * t555 * t77
      t1954 = x2 * z
      t1984 = t256 * t592 * t77
      t2005 = 0.74D2 * t250 * x1 * t76 * t765 + 0.16D2 * t250 * t242 + 0
     #.14D2 * t216 * t1951 - 0.384D3 * t321 * t868 * t1954 * x3 - 0.384D
     #3 * t315 * t365 * t1954 * t20 - 0.384D3 * t757 * t715 * t1954 * t1
     #0 - 0.176D3 * t1485 * t179 * t429 * t10 - 0.176D3 * t1480 * t335 *
     # t376 * t34 + 0.32D2 * t1263 * t256 * t1186 * t82 + 0.32D2 * t1263
     # * t256 * t1186 * t77 + 0.144D3 * t591 * t1984 + 0.144D3 * t500 * 
     #t527 + 0.2D1 * t1017 * t911 * t1040 - 0.32D2 * t863 * t134 * t1068
     # + 0.32D2 * t863 * t134 * t702 - 0.120D3 * t406 * t322 * t1836 + 0
     #.104D3 * t1849 * t353 * t603 * t82
      t2008 = t239 * t1883 * t77
      t2028 = t547 * t10
      t2052 = t58 * t274
      t2058 = 0.64D2 * t194 * t2008 - 0.32D2 * t194 * t1951 + 0.8D1 * t1
     #849 * t353 * t603 * t77 - 0.133D3 * t294 * t322 * t182 + 0.6D1 * t
     #1211 * t1451 + 0.14D2 * t530 * t584 + 0.24D2 * t1149 * t256 * t168
     #0 * t10 - 0.64D2 * t332 * t675 * t2028 - 0.64D2 * t757 * t715 * t1
     #775 - 0.48D2 * t565 * t566 * t1348 + 0.32D2 * t1874 * t467 * t468 
     #* t150 - 0.6D1 * t1569 * t1845 + 0.104D3 * t216 * t2008 + 0.64D2 *
     # t250 * t708 * t1689 + 0.32D2 * t1060 * t179 * t1064 + 0.488D3 * t
     #2052 * t876 - 0.1656D4 * t58 * t292 * t935
      t2076 = t371 * t778 * t126
      t2097 = t267 * t1494
      t2112 = 0.488D3 * t822 * t179 * t336 * t20 + 0.488D3 * t178 * t277
     # * t482 * x3 + 0.384D3 * t494 * t256 * t1186 * x3 - 0.6D1 * t1742 
     #* t531 * t257 * t34 + 0.24D2 * t30 * t2076 * t179 * t651 * t82 - 0
     #.48D2 * t966 * t899 * t967 * t10 - 0.120D3 * t1034 * t345 * t1035 
     #* t10 + 0.488D3 * t1855 * t534 + 0.488D3 * t2052 * t1633 + 0.488D3
     # * t1855 * t880 + 0.7D1 * t58 * t458 * t2097 - 0.32D2 * t459 * t20
     #97 - 0.32D2 * t874 * t945 - 0.64D2 * t1239 * t544 - 0.74D2 * t822 
     #* t1531 - 0.32D2 * t509 * t1461 + 0.80D2 * t1017 * t1722
      t2149 = t316 * x3
      t2153 = t181 * t77
      t2172 = 0.14D2 * t1480 * t386 * t1509 + 0.48D2 * t481 * t295 * t69
     #3 - 0.64D2 * t772 * t375 * t697 - 0.32D2 * t87 * t627 * t413 * t39
     # * t266 * t66 + 0.48D2 * t87 * t176 * t525 * t39 * t265 * t44 - 0.
     #32D2 * t500 * t413 * t501 * t150 + 0.32D2 * t250 * t708 * t69 - 0.
     #32D2 * t250 * t701 * t83 - 0.32D2 * t250 * t701 * t78 + 0.8D1 * t7
     #72 * t1467 * t2149 + 0.24D2 * t481 * t296 * t2153 - 0.74D2 * t1285
     # * t1625 - 0.64D2 * t315 * t365 * t1056 + 0.86D2 * t1320 * t1746 +
     # 0.64D2 * t308 * t309 * t537 - 0.64D2 * t250 * t701 * t76 * x3 * t
     #10
      t2219 = t87 * t473
      t2228 = -0.80D2 * t276 * t277 * t280 + 0.74D2 * t286 * t179 * t288
     # + 0.128D3 * t57 * t1173 * t208 + 0.74D2 * t524 * t588 + 0.8D1 * t
     #1149 * t256 * t1864 * x3 - 0.48D2 * t656 * t322 * t1215 + 0.32D2 *
     # t757 * t277 * t1906 + 0.32D2 * t332 * t179 * t1328 + 0.64D2 * t27
     #6 * t278 * t1547 + 0.32D2 * t315 * t295 * t2149 + 0.32D2 * t321 * 
     #t322 * t2153 + 0.32D2 * t1551 * t128 * x1 * t82 * x3 - 0.48D2 * t6
     #61 * t1548 + 0.133D3 * t178 * t761 - 0.48D2 * t1485 * t1486 * t636
     # * t10 - 0.6D1 * t2219 * t911 * t475 + 0.8D1 * t87 * t917 * t375 *
     # t919 * x3
      t2271 = 0.24D2 * t87 * t2076 * t295 * t651 * t77 - 0.32D2 * t1292 
     #* t424 - 0.32D2 * t1292 * t1598 - 0.128D3 * t1292 * t731 + 0.74D2 
     #* t1072 * t1984 - 0.32D2 * t1030 * t1146 - 0.32D2 * t428 * t2097 +
     # 0.48D2 * t1889 * t1293 + 0.64D2 * t514 * t925 + 0.64D2 * t514 * t
     #924 * t1775 - 0.64D2 * t216 * t336 * t948 * t20 + 0.6D1 * t264 * t
     #1577 + 0.6D1 * t2219 * t911 * t1933 + 0.14D2 * t552 * t1885 + 0.64
     #D2 * t1708 * t605 + 0.240D3 * t1292 * t734 + 0.8D1 * t552 * t749
      t2283 = t87 * t1888 * t66
      t2323 = -0.6D1 * t552 * t217 * t752 * t10 + 0.32D2 * t1686 * t484 
     #- 0.64D2 * t1874 * t740 + 0.48D2 * t1874 * t1344 - 0.32D2 * t2283 
     #* t1349 - 0.120D3 * t863 * t1143 + 0.384D3 * t1822 * t322 * t1628 
     #+ 0.48D2 * t2283 * t1606 + 0.133D3 * t973 * t1349 + 0.240D3 * t188
     #9 * t424 - 0.128D3 * t1889 * t1598 + 0.80D2 * t383 * t335 * t45 * 
     #t1721 - 0.80D2 * t250 * t538 - 0.48D2 * t30 * t341 * t1052 * t943 
     #* t66 * t2028 - 0.64D2 * t757 * t277 * t1672 + 0.32D2 * t1534 * t1
     #34 * x1 * t77 * t10 + 0.24D2 * t459 * t1701 * t1702 * t145
      rrgg2ggh11J1 = -0.9D1 / 0.16D2 * wd * (t625 + t1460 + t301 + t1397
     # + t1266 + t1047 + t883 + t132 + t1771 + t1198 + t1715 + t1525 + t
     #1840 + t1894 + t963 + t713 + t1589 + t435 + t798 + t1652 + t1335 +
     # t541 + t191 + t1943 + t1126 + t2005 + t2058 + t2112 + t2172 + t22
     #28 + t2271 + t2323) / t56 / t359 / t40 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh11J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t6 * t20 - t2 * t24
      t27 = s ** 2
      t28 = t27 ** 2
      t29 = t28 * t27
      t30 = t26 * t29
      t33 = x2 * x3
      t34 = t10 * t7 * t4 + t33 + t19
      t37 = t23 * t10
      t39 = s - t2 * t6 * t34 - t2 * t37
      t40 = t39 ** 2
      t41 = t30 * t40
      t42 = t1 ** 2
      t43 = t42 ** 2
      t44 = x1 ** 2
      t45 = t44 ** 2
      t46 = t43 * t45
      t47 = t4 ** 2
      t49 = 0.1D1 / t47 / t4
      t50 = t20 ** 2
      t51 = t50 * t20
      t53 = t46 * t49 * t51
      t55 = 0.2D1 * t41 * t53
      t56 = t26 ** 2
      t57 = t28 * s
      t58 = t56 * t57
      t59 = t58 * t40
      t60 = t42 * t44
      t61 = t5 * t20
      t62 = t60 * t61
      t64 = 0.176D3 * t59 * t62
      t65 = t42 * t1
      t66 = t44 * x1
      t67 = t65 * t66
      t68 = 0.1D1 / t47
      t69 = t68 * t50
      t70 = t67 * t69
      t72 = 0.38D2 * t59 * t70
      t73 = t57 * z
      t74 = t73 * t56
      t75 = t40 * t42
      t76 = t23 ** 2
      t77 = x3 ** 2
      t78 = t76 * t77
      t81 = 0.32D2 * t74 * t75 * t78
      t82 = t10 ** 2
      t83 = t76 * t82
      t86 = 0.32D2 * t74 * t75 * t83
      t87 = t56 * t29
      t88 = t87 * t39
      t89 = t65 * x1
      t90 = t89 * t83
      t92 = 0.14D2 * t88 * t90
      t93 = t42 * x1
      t96 = 0.6D1 * t88 * t93 * t37
      t97 = t43 * x1
      t98 = t76 * t23
      t99 = t82 * t10
      t101 = t97 * t98 * t99
      t103 = 0.42D2 * t88 * t101
      t105 = 0.32D2 * t59 * t101
      t107 = 0.5D1 * t59 * t90
      t108 = t89 * t78
      t110 = 0.14D2 * t41 * t108
      t113 = 0.6D1 * t41 * t93 * t24
      t114 = t77 * x3
      t116 = t97 * t98 * t114
      t118 = 0.42D2 * t41 * t116
      t120 = 0.32D2 * t59 * t116
      t122 = 0.5D1 * t59 * t108
      t126 = z + x1 * t7 * t1
      t127 = t23 * t126
      t128 = t5 * t39
      t131 = 0.2D1 * t87 * t1 * t127 * t128
      t132 = t55 + t64 - t72 + t81 + t86 + t92 - t96 - t103 - t105 - t10
     #7 + t110 - t113 - t118 - t120 - t122 + t131
      t134 = t5 * t40
      t135 = t127 * t134
      t137 = 0.260D3 * t58 * t1 * t135
      t140 = 0.2D1 * t30 * t1 * t135
      t142 = t60 * t5 * t34
      t144 = 0.6D1 * t88 * t142
      t145 = t34 ** 2
      t146 = t68 * t145
      t147 = t67 * t146
      t149 = 0.6D1 * t88 * t147
      t150 = t145 * t34
      t152 = t46 * t49 * t150
      t154 = 0.2D1 * t88 * t152
      t156 = 0.8D1 * t41 * t142
      t158 = 0.24D2 * t41 * t147
      t160 = 0.32D2 * t41 * t152
      t162 = 0.176D3 * t59 * t142
      t164 = 0.38D2 * t59 * t147
      t166 = 0.8D1 * t88 * t62
      t168 = 0.24D2 * t88 * t70
      t170 = 0.32D2 * t88 * t53
      t172 = 0.6D1 * t41 * t62
      t174 = 0.6D1 * t41 * t70
      t175 = t43 ** 2
      t177 = t30 * t40 * t175
      t178 = x2 ** 2
      t179 = t178 * x2
      t180 = t45 * t179
      t181 = t76 ** 2
      t182 = t181 * t49
      t184 = t180 * t182 * t10
      t186 = 0.6D1 * t177 * t184
      t187 = t43 * t1
      t189 = t187 * t76 * t126
      t190 = t87 * t189
      t191 = t47 ** 2
      t192 = 0.1D1 / t191
      t193 = t192 * t39
      t194 = x2 * t66
      t195 = t194 * t50
      t198 = 0.32D2 * t190 * t193 * t195
      t199 = -t137 + t140 + t144 + t149 + t154 + t156 + t158 + t160 + t1
     #62 - t164 + t166 + t168 + t170 + t172 + t174 + t186 - t198
      t201 = t187 * t181
      t202 = t201 * t126
      t203 = t87 * t202
      t204 = t68 * t39
      t205 = x1 * x2
      t206 = t205 * t82
      t209 = 0.32D2 * t203 * t204 * t206
      t210 = t205 * t77
      t212 = t203 * t204 * t210
      t214 = t40 * t65
      t215 = t58 * t214
      t216 = t66 * z
      t219 = 0.32D2 * t215 * t216 * t69
      t220 = x1 * z
      t222 = t215 * t220 * t83
      t225 = t215 * t220 * t78
      t227 = t56 * t65
      t228 = t227 * t76
      t230 = t126 * t68
      t231 = t40 * x2
      t233 = t230 * t231 * x1
      t234 = t73 * t228 * t233
      t236 = z ** 2
      t237 = t29 * t236
      t238 = t56 * t39
      t241 = t44 * t5
      t242 = t20 * t23
      t243 = t242 * t10
      t244 = t241 * t243
      t246 = 0.64D2 * t237 * t238 * t65 * t244
      t247 = t26 * t40
      t250 = t34 * t23
      t251 = t250 * x3
      t252 = t241 * t251
      t254 = 0.64D2 * t237 * t247 * t65 * t252
      t255 = t39 * t187
      t256 = t87 * t255
      t257 = t44 * x2
      t258 = t98 * t5
      t259 = t258 * t77
      t260 = t257 * t259
      t262 = 0.32D2 * t256 * t260
      t263 = t45 * x2
      t264 = t23 * t49
      t265 = t264 * t145
      t266 = t263 * t265
      t268 = 0.32D2 * t256 * t266
      t269 = t43 * t42
      t270 = t269 * t98
      t271 = t270 * t126
      t272 = t30 * t271
      t273 = t49 * t40
      t274 = t273 * t66
      t275 = x2 * t20
      t276 = t275 * t10
      t279 = 0.16D2 * t272 * t274 * t276
      t280 = x2 * t34
      t281 = t280 * t10
      t284 = 0.16D2 * t272 * t274 * t281
      t285 = t40 * t43
      t286 = t58 * t285
      t289 = 0.32D2 * t286 * t194 * t265
      t291 = t58 * t285 * x2
      t292 = t44 * t76
      t293 = t68 * t34
      t294 = t293 * t10
      t297 = 0.48D2 * t291 * t292 * t294
      t298 = t65 * t76
      t299 = t87 * t298
      t300 = t126 * t5
      t301 = t39 * x1
      t304 = t299 * t300 * t301 * x3
      t306 = t43 * t23
      t307 = t30 * t306
      t308 = t126 * t49
      t309 = t40 * t66
      t311 = t308 * t309 * t50
      t312 = t307 * t311
      t314 = -t209 + 0.240D3 * t212 + t219 - 0.32D2 * t222 - 0.32D2 * t2
     #25 + 0.1028D4 * t234 - t246 - t254 - t262 - t268 - t279 - t284 + t
     #289 + t297 + 0.205D3 * t304 + 0.74D2 * t312
      t315 = t187 * t98
      t316 = t315 * t126
      t317 = t87 * t316
      t318 = t44 * t20
      t319 = t318 * t77
      t322 = 0.32D2 * t317 * t204 * t319
      t323 = t43 * t76
      t324 = t323 * t126
      t325 = t30 * t324
      t326 = t44 * t50
      t328 = t273 * t326 * x3
      t330 = 0.14D2 * t325 * t328
      t331 = t43 * t98
      t332 = t331 * t126
      t333 = t30 * t332
      t334 = t68 * t40
      t335 = x1 * t20
      t339 = 0.6D1 * t333 * t334 * t335 * t77
      t340 = t187 * t23
      t342 = t126 * t192
      t343 = t40 * t45
      t347 = 0.32D2 * t30 * t340 * t342 * t343 * t51
      t348 = t58 * t189
      t351 = t348 * t273 * t194 * t20
      t353 = t30 * t214
      t354 = t23 * t5
      t355 = t257 * t354
      t357 = 0.32D2 * t353 * t355
      t358 = t65 * t98
      t359 = t58 * t358
      t360 = t40 * t77
      t361 = t300 * t360
      t363 = 0.38D2 * t359 * t361
      t367 = 0.48D2 * t325 * t273 * t257 * t34
      t368 = t39 * t269
      t370 = t87 * t368 * t66
      t371 = x2 * t98
      t372 = t68 * t20
      t376 = 0.104D3 * t370 * t371 * t372 * t82
      t378 = t87 * t368 * t44
      t379 = x2 * t181
      t382 = t379 * t5 * t77 * t10
      t384 = 0.6D1 * t378 * t382
      t385 = t40 * t187
      t389 = t5 * x3 * t10
      t391 = t58 * t385 * t44 * t371 * t389
      t393 = t385 * t66
      t394 = t58 * t393
      t395 = x2 * t76
      t396 = t293 * x3
      t397 = t395 * t396
      t398 = t394 * t397
      t400 = t58 * t332
      t401 = x1 * t34
      t402 = t401 * t77
      t405 = 0.32D2 * t400 * t334 * t402
      t406 = t30 * t393
      t407 = t406 * t397
      t410 = t269 * t181 * t126
      t411 = t87 * t410
      t412 = t257 * t82
      t413 = t204 * t412
      t415 = 0.5D1 * t411 * t413
      t416 = t87 * t332
      t417 = t220 * t77
      t420 = 0.32D2 * t416 * t128 * t417
      t421 = t306 * t126
      t422 = t87 * t421
      t423 = t49 * t39
      t424 = t216 * t145
      t426 = t422 * t423 * t424
      t428 = t322 + t330 - t339 + t347 + 0.488D3 * t351 - t357 - t363 + 
     #t367 + t376 - t384 - 0.148D3 * t391 + 0.133D3 * t398 - t405 - 0.32
     #D2 * t407 + t415 - t420 + 0.32D2 * t426
      t431 = t334 * t44
      t432 = z * t20
      t433 = t432 * t10
      t435 = t325 * t431 * t433
      t437 = t40 * z
      t441 = 0.32D2 * t359 * t300 * t437 * t82
      t445 = 0.32D2 * t359 * t300 * t437 * t77
      t448 = z * x3 * t10
      t450 = t416 * t128 * x1 * t448
      t455 = 0.8D1 * t370 * t371 * t372 * t77
      t458 = t379 * t5 * t82 * x3
      t460 = 0.14D2 * t378 * t458
      t461 = t372 * x3
      t464 = 0.48D2 * t291 * t292 * t461
      t465 = t27 * s
      t466 = t28 * t465
      t467 = t466 * z
      t468 = t467 * t26
      t470 = t204 * t44
      t471 = t33 * t10
      t472 = t470 * t471
      t474 = 0.64D2 * t468 * t410 * t472
      t475 = t468 * t271
      t476 = t423 * t66
      t477 = t476 * t281
      t479 = 0.64D2 * t475 * t477
      t481 = t269 * t76 * t126
      t482 = t87 * t481
      t484 = 0.1D1 / t191 / t4
      t485 = t484 * t39
      t486 = t263 * t150
      t489 = 0.42D2 * t482 * t485 * t486
      t490 = t181 * t23
      t492 = t269 * t490 * t126
      t497 = 0.32D2 * t87 * t492 * t204 * t205 * t114
      t498 = t263 * t145
      t499 = t193 * t498
      t501 = 0.5D1 * t482 * t499
      t504 = 0.384D3 * t59 * t3 * t236
      t505 = t58 * t324
      t506 = t44 * t145
      t507 = t506 * t10
      t510 = 0.32D2 * t505 * t273 * t507
      t512 = 0.48D2 * t353 * t252
      t513 = t242 * x3
      t514 = t241 * t513
      t516 = 0.12D2 * t353 * t514
      t517 = -0.64D2 * t435 + t441 + t445 - 0.64D2 * t450 + t455 + t460 
     #+ t464 + t474 - t479 - t489 + t497 + t501 + t504 - t510 - t512 - t
     #516
      t518 = t43 * t65
      t519 = t40 * t518
      t521 = t30 * t519 * t45
      t522 = t178 * t98
      t523 = t49 * t34
      t527 = 0.16D2 * t521 * t522 * t523 * t10
      t528 = t298 * t126
      t529 = t58 * t528
      t530 = t335 * t10
      t531 = t334 * t530
      t533 = 0.16D2 * t529 * t531
      t534 = t30 * t528
      t535 = t205 * t236
      t538 = 0.384D3 * t534 * t334 * t535
      t539 = t43 * t181
      t540 = t58 * t539
      t542 = t300 * t360 * t10
      t544 = 0.32D2 * t540 * t542
      t546 = 0.32D2 * t505 * t328
      t548 = t308 * t309 * t145
      t549 = t307 * t548
      t551 = t87 * t271
      t553 = t476 * t275 * x3
      t555 = 0.16D2 * t551 * t553
      t556 = t65 * t23
      t557 = t58 * t556
      t558 = t40 * t44
      t560 = t230 * t558 * t34
      t561 = t557 * t560
      t563 = t58 * t298
      t564 = t40 * x1
      t566 = t300 * t564 * t10
      t567 = t563 * t566
      t569 = t40 * t82
      t571 = t300 * t569 * x3
      t573 = 0.32D2 * t540 * t571
      t574 = t87 * t324
      t577 = 0.14D2 * t574 * t423 * t507
      t579 = t518 * t98 * t126
      t580 = t87 * t579
      t582 = t45 * t20
      t583 = t582 * t34
      t586 = 0.12D2 * t580 * t485 * t178 * t583
      t587 = t30 * t285
      t588 = t45 * t49
      t590 = t588 * t50 * t34
      t592 = 0.8D1 * t587 * t590
      t594 = t230 * t39
      t595 = t401 * x3
      t598 = 0.64D2 * t237 * t228 * t594 * t595
      t599 = t56 * t40
      t601 = t73 * t599 * t42
      t604 = 0.64D2 * t601 * t6 * t513
      t605 = t40 * t269
      t607 = t30 * t605 * t44
      t609 = 0.6D1 * t607 * t458
      t610 = t42 * t76
      t611 = t30 * t610
      t612 = t40 * x3
      t613 = t300 * t612
      t615 = 0.6D1 * t611 * t613
      t616 = t527 + t533 - t538 - t544 - t546 - 0.48D2 * t549 - t555 + 0
     #.488D3 * t561 + 0.488D3 * t567 - t573 + t577 - t586 + t592 - t598 
     #+ t604 - t609 + t615
      t619 = t394 * t395 * t294
      t625 = 0.64D2 * t58 * t358 * t126 * t134 * t448
      t626 = t334 * z
      t627 = t401 * t10
      t629 = t529 * t626 * t627
      t631 = t192 * t40
      t632 = t631 * x2
      t633 = t66 * t50
      t634 = t633 * t10
      t637 = 0.104D3 * t272 * t632 * t634
      t638 = t58 * t605
      t639 = t45 * t178
      t640 = t76 * t49
      t642 = t639 * t640 * t34
      t643 = t638 * t642
      t648 = 0.2D1 * t87 * t39 * t1 * x1
      t650 = t40 * t1 * x1
      t652 = 0.260D3 * t58 * t650
      t654 = 0.2D1 * t30 * t650
      t655 = t178 * t66
      t657 = t631 * t655 * t34
      t659 = 0.64D2 * t272 * t657
      t660 = t485 * x2
      t662 = t45 * t50 * t34
      t665 = 0.6D1 * t482 * t660 * t662
      t666 = t318 * x3
      t667 = t334 * t666
      t668 = t325 * t667
      t671 = x1 * x3 * t10
      t672 = t134 * t671
      t673 = t333 * t672
      t676 = t529 * t626 * t595
      t678 = t335 * x3
      t680 = t529 * t626 * t678
      t684 = t230 * t558 * x2
      t685 = t58 * t323 * t684
      t692 = 0.48D2 * t30 * t315 * t308 * t40 * t178 * t44
      t693 = t175 * t1
      t694 = t693 * t490
      t696 = t126 * t484
      t697 = t178 ** 2
      t702 = 0.2D1 * t87 * t694 * t696 * t39 * t697 * t45
      t703 = 0.133D3 * t619 + t625 + 0.64D2 * t629 + t637 - 0.205D3 * t6
     #43 + t648 - t652 + t654 - t659 - t665 + 0.148D3 * t668 - 0.133D3 *
     # t673 + 0.64D2 * t676 + 0.64D2 * t680 - 0.1656D4 * t685 + t692 + t
     #702
      t705 = t230 * t558 * t20
      t706 = t557 * t705
      t709 = t300 * t564 * x3
      t710 = t563 * t709
      t712 = t44 * t34
      t713 = t712 * t77
      t716 = 0.32D2 * t317 * t204 * t713
      t722 = t178 * t181 * t68 * x3 * t10
      t724 = 0.12D2 * t30 * t519 * t66 * t722
      t726 = t518 * t490 * t126
      t728 = t178 * t44
      t732 = 0.24D2 * t30 * t726 * t273 * t728 * t82
      t733 = t87 * t331
      t735 = t300 * t301 * t82
      t736 = t733 * t735
      t740 = t733 * t300 * t301 * t77
      t742 = t39 * x2
      t746 = 0.32D2 * t299 * t230 * t742 * x1
      t748 = t98 * t126
      t752 = 0.32D2 * t237 * t227 * t748 * t128 * t77
      t754 = t529 * t626 * t530
      t756 = t30 * t539
      t758 = 0.24D2 * t756 * t571
      t760 = 0.48D2 * t534 * t531
      t761 = t98 * t68
      t763 = t655 * t761 * x3
      t764 = t638 * t763
      t766 = t87 * t358
      t767 = t39 * x3
      t768 = t767 * t10
      t771 = 0.16D2 * t766 * t300 * t768
      t772 = z * t34
      t773 = t772 * t10
      t776 = 0.64D2 * t325 * t431 * t773
      t777 = t30 * t421
      t778 = t432 * t34
      t780 = t777 * t274 * t778
      t782 = t58 * t385
      t783 = t782 * t266
      t785 = 0.488D3 * t706 + 0.488D3 * t710 + t716 - t724 + t732 + 0.74
     #D2 * t736 - 0.48D2 * t740 - t746 + t752 + 0.64D2 * t754 + t758 - t
     #760 + 0.67D2 * t764 + t771 - t776 - 0.64D2 * t780 + 0.48D2 * t783
      t790 = t655 * t761 * t10
      t791 = t638 * t790
      t793 = t87 * t610
      t796 = 0.8D1 * t793 * t300 * t767
      t797 = t39 * t77
      t800 = 0.24D2 * t766 * t300 * t797
      t801 = t66 * t68
      t802 = t145 * t23
      t804 = t801 * t802 * x3
      t806 = 0.32D2 * t286 * t804
      t807 = t280 * x3
      t808 = t476 * t807
      t810 = 0.64D2 * t475 * t808
      t811 = t26 * t269
      t813 = t467 * t811 * t76
      t814 = t342 * t39
      t817 = 0.32D2 * t813 * t814 * t498
      t818 = t30 * t385
      t819 = t76 * t68
      t820 = t655 * t819
      t822 = 0.48D2 * t818 * t820
      t825 = t45 * x1
      t828 = t825 * t697 * t181 * t192
      t830 = 0.2D1 * t30 * t40 * t693 * t828
      t834 = 0.2D1 * t87 * t39 * t693 * t828
      t835 = t58 * t610
      t837 = t300 * t40 * t10
      t839 = 0.176D3 * t835 * t837
      t840 = t87 * t368
      t841 = t825 * x2
      t842 = t23 * t192
      t846 = 0.32D2 * t840 * t841 * t842 * t51
      t847 = t181 * t5
      t849 = t257 * t847 * t99
      t851 = 0.42D2 * t840 * t849
      t852 = t30 * t481
      t853 = t484 * t40
      t854 = t853 * x2
      t857 = 0.14D2 * t852 * t854 * t662
      t858 = t334 * x1
      t860 = t20 * t10 * x3
      t861 = t858 * t860
      t863 = 0.48D2 * t333 * t861
      t864 = t73 * t599
      t866 = x2 * t23
      t869 = t864 * t65 * t44 * t866 * t5
      t872 = t87 * t39 * t65
      t873 = t354 * t236
      t874 = t257 * t873
      t876 = 0.384D3 * t872 * t874
      t877 = 0.67D2 * t791 + t796 + t800 - t806 - t810 + t817 + t822 + t
     #830 + t834 + t839 + t846 - t851 + t857 - t863 + 0.1028D4 * t869 - 
     #t876
      t878 = t42 * t23
      t879 = t87 * t878
      t883 = 0.6D1 * t879 * t230 * t301 * t34
      t884 = t30 * t519
      t885 = t181 * t68
      t887 = t655 * t885 * t77
      t889 = 0.14D2 * t884 * t887
      t890 = t179 * t66
      t891 = t98 * t49
      t893 = t638 * t890 * t891
      t896 = t87 * t39 * t175
      t898 = t180 * t182 * x3
      t900 = 0.6D1 * t896 * t898
      t901 = t26 * t466
      t902 = t901 * t271
      t904 = 0.64D2 * t902 * t553
      t905 = t901 * t410
      t907 = 0.48D2 * t905 * t472
      t908 = t639 * t145
      t911 = 0.14D2 * t580 * t485 * t908
      t914 = 0.64D2 * t317 * t470 * t860
      t915 = t30 * t316
      t916 = x2 * z
      t920 = 0.384D3 * t915 * t431 * t916 * t10
      t921 = t30 * t189
      t925 = 0.384D3 * t921 * t274 * t280 * z
      t926 = t194 * t145
      t927 = t631 * t926
      t929 = 0.32D2 * t921 * t927
      t930 = t901 * t324
      t931 = t257 * z
      t934 = 0.384D3 * t930 * t204 * t931
      t935 = t58 * t316
      t938 = t935 * t334 * t257 * x3
      t940 = t193 * x2
      t941 = t66 * t145
      t942 = t941 * x3
      t945 = 0.104D3 * t551 * t940 * t942
      t950 = 0.32D2 * t30 * t201 * t300 * t564 * t114
      t951 = t318 * t10
      t952 = t334 * t951
      t953 = t505 * t952
      t958 = 0.32D2 * t756 * t300 * t40 * t99
      t959 = -t883 + t889 + 0.128D3 * t893 + t900 - t904 + t907 + t911 -
     # t914 - t920 - t925 - t929 + t934 + 0.488D3 * t938 + t945 + t950 -
     # 0.208D3 * t953 + t958
      t961 = t39 * t518
      t962 = t87 * t961
      t964 = 0.6D1 * t962 * t887
      t966 = t30 * t605 * t66
      t967 = t371 * t68
      t969 = t34 * x3 * t10
      t972 = 0.48D2 * t966 * t967 * t969
      t974 = t574 * t470 * t773
      t977 = 0.120D3 * t587 * t804
      t978 = t34 * t76
      t980 = t241 * t978 * t77
      t982 = 0.104D3 * t587 * t980
      t983 = t39 * t43
      t986 = t5 * z
      t990 = 0.384D3 * t87 * t983 * t44 * t395 * t986 * t10
      t994 = 0.384D3 * t317 * t470 * t916 * x3
      t996 = t257 * t847 * t114
      t998 = 0.2D1 * t840 * t996
      t999 = t825 * t178
      t1000 = t76 * t192
      t1004 = 0.24D2 * t962 * t999 * t1000 * t50
      t1008 = 0.104D3 * t966 * t371 * t293 * t77
      t1010 = 0.14D2 * t607 * t382
      t1011 = t264 * t50
      t1012 = t263 * t1011
      t1013 = t256 * t1012
      t1015 = t258 * t82
      t1016 = t257 * t1015
      t1017 = t256 * t1016
      t1020 = 0.48D2 * t256 * t820
      t1022 = t180 * t891
      t1023 = t58 * t519 * t1022
      t1025 = t30 * t298
      t1026 = t1025 * t709
      t1029 = 0.32D2 * t286 * t590
      t1030 = t964 - t972 + 0.64D2 * t974 - t977 + t982 + t990 - t994 + 
     #t998 + t1004 + t1008 + t1010 + 0.240D3 * t1013 - 0.128D3 * t1017 +
     # t1020 + 0.7D1 * t1023 - 0.67D2 * t1026 - t1029
      t1032 = t87 * t368 * t45
      t1033 = t49 * t50
      t1037 = 0.24D2 * t1032 * t395 * t1033 * x3
      t1038 = t30 * t579
      t1039 = t639 * t50
      t1042 = 0.14D2 * t1038 * t853 * t1039
      t1046 = 0.384D3 * t286 * t728 * t819 * z
      t1048 = t416 * t128 * t671
      t1050 = t712 * x3
      t1052 = t574 * t204 * t1050
      t1054 = t30 * t410
      t1055 = t273 * x2
      t1056 = t318 * t82
      t1059 = 0.120D3 * t1054 * t1055 * t1056
      t1061 = t87 * t961 * t45
      t1062 = t49 * t20
      t1066 = 0.48D2 * t1061 * t522 * t1062 * t10
      t1070 = 0.120D3 * t1032 * t395 * t1033 * t10
      t1071 = t87 * t539
      t1075 = 0.24D2 * t1071 * t300 * t797 * t10
      t1076 = t39 * t82
      t1080 = 0.8D1 * t1071 * t300 * t1076 * x3
      t1081 = t87 * t528
      t1084 = 0.48D2 * t1081 * t204 * t595
      t1085 = t58 * t75
      t1087 = t1085 * t205 * t354
      t1093 = 0.32D2 * t203 * t128 * x1 * t82 * x3
      t1094 = t20 * t34
      t1095 = t1094 * t10
      t1098 = 0.64D2 * t921 * t274 * t1095
      t1101 = 0.64D2 * t915 * t431 * t969
      t1102 = t257 * t236
      t1103 = t204 * t1102
      t1105 = 0.384D3 * t574 * t1103
      t1108 = 0.384D3 * t325 * t334 * t1102
      t1109 = t1037 + t1042 + t1046 - 0.133D3 * t1048 - 0.133D3 * t1052 
     #- t1059 - t1066 - t1070 + t1075 + t1080 - t1084 - 0.240D3 * t1087 
     #+ t1093 - t1098 - t1101 - t1105 - t1108
      t1113 = 0.16D2 * t215 * t252
      t1114 = t20 * t76
      t1116 = t241 * t1114 * t77
      t1118 = 0.14D2 * t587 * t1116
      t1120 = t58 * t214 * t44
      t1121 = t250 * t10
      t1123 = t1120 * t986 * t1121
      t1125 = t372 * t34
      t1128 = 0.64D2 * t215 * t216 * t1125
      t1129 = t216 * t50
      t1131 = t777 * t273 * t1129
      t1134 = t39 * t45
      t1138 = 0.32D2 * t87 * t340 * t342 * t1134 * t150
      t1143 = 0.32D2 * t87 * t201 * t300 * t301 * t99
      t1144 = t263 * t51
      t1147 = 0.2D1 * t482 * t485 * t1144
      t1151 = 0.2D1 * t1071 * t300 * t39 * t99
      t1155 = 0.32D2 * t1071 * t300 * t39 * t114
      t1159 = 0.24D2 * t884 * t999 * t1000 * t145
      t1162 = 0.38D2 * t852 * t631 * t498
      t1165 = 0.80D2 * t529 * t334 * t627
      t1166 = t30 * t202
      t1171 = 0.32D2 * t1166 * t134 * x1 * t77 * t10
      t1172 = t901 * t481
      t1174 = 0.32D2 * t1172 * t499
      t1175 = t1055 * t1050
      t1177 = 0.48D2 * t915 * t1175
      t1178 = t1113 + t1118 + 0.64D2 * t1123 + t1128 + 0.32D2 * t1131 + 
     #t1138 + t1143 + t1147 + t1151 + t1155 + t1159 + t1162 - t1165 + t1
     #171 + t1174 + t1177
      t1179 = t772 * x3
      t1181 = t325 * t431 * t1179
      t1183 = t518 * t181
      t1187 = t342 * t40 * t179 * t66
      t1189 = 0.32D2 * t30 * t1183 * t1187
      t1191 = t588 * t20 * t145
      t1193 = 0.32D2 * t286 * t1191
      t1194 = t263 * t50
      t1195 = t193 * t1194
      t1197 = 0.38D2 * t482 * t1195
      t1198 = t476 * t276
      t1200 = 0.48D2 * t902 * t1198
      t1206 = 0.48D2 * t87 * t315 * t308 * t39 * t178 * t44
      t1209 = 0.32D2 * t915 * t334 * t1056
      t1210 = t340 * t126
      t1214 = 0.32D2 * t30 * t1210 * t631 * t662
      t1218 = 0.64D2 * t333 * t334 * t205 * x3
      t1221 = 0.32D2 * t215 * t216 * t146
      t1222 = t44 * z
      t1225 = 0.384D3 * t1085 * t1222 * t61
      t1226 = t582 * t145
      t1229 = 0.6D1 * t852 * t854 * t1226
      t1231 = t574 * t204 * t951
      t1233 = t712 * t10
      t1235 = t574 * t204 * t1233
      t1237 = t1094 * x3
      t1240 = 0.64D2 * t190 * t476 * t1237
      t1241 = t58 * t878
      t1242 = t300 * t564
      t1243 = t1241 * t1242
      t1246 = 0.64D2 * t902 * t477
      t1247 = -0.64D2 * t1181 - t1189 - t1193 + t1197 - t1200 + t1206 + 
     #t1209 + t1214 - t1218 + t1221 + t1225 - t1229 - 0.133D3 * t1231 + 
     #0.148D3 * t1235 - t1240 + 0.86D2 * t1243 - t1246
      t1250 = t275 * t34
      t1251 = t193 * t45 * t1250
      t1253 = 0.48D2 * t1172 * t1251
      t1254 = t818 * t266
      t1258 = 0.32D2 * t422 * t423 * t1129
      t1259 = t220 * t82
      t1261 = t416 * t128 * t1259
      t1264 = t190 * t193 * t926
      t1266 = t728 * t10
      t1269 = 0.48D2 * t411 * t423 * t1266
      t1270 = t39 * t10
      t1273 = 0.6D1 * t793 * t300 * t1270
      t1274 = t87 * t556
      t1275 = t39 * t44
      t1277 = t308 * t1275 * t145
      t1279 = 0.14D2 * t1274 * t1277
      t1280 = t58 * t202
      t1281 = t334 * t206
      t1282 = t1280 * t1281
      t1284 = t825 * t179
      t1285 = t98 * t192
      t1289 = 0.8D1 * t896 * t1284 * t1285 * t20
      t1293 = 0.8D1 * t177 * t1284 * t1285 * t34
      t1295 = 0.6D1 * t177 * t898
      t1296 = t29 * z
      t1297 = t56 * t43
      t1298 = t1297 * t76
      t1301 = t230 * t1275 * x2
      t1302 = t1296 * t1298 * t1301
      t1304 = t236 * z
      t1306 = t26 * t43
      t1307 = t1306 * t76
      t1310 = 0.128D3 * t466 * t1304 * t1307 * t1301
      t1313 = 0.48D2 * t291 * t292 * t396
      t1314 = t631 * t1194
      t1316 = 0.5D1 * t852 * t1314
      t1319 = 0.38D2 * t1054 * t334 * t412
      t1320 = t1253 + 0.240D3 * t1254 - t1258 + 0.32D2 * t1261 - 0.128D3
     # * t1264 + t1269 + t1273 + t1279 + 0.48D2 * t1282 + t1289 + t1293 
     #- t1295 - 0.1028D4 * t1302 + t1310 - t1313 + t1316 + t1319
      t1321 = t257 * t77
      t1322 = t334 * t1321
      t1324 = 0.5D1 * t1054 * t1322
      t1328 = 0.176D3 * t580 * t193 * t639 * t20
      t1329 = t1183 * t126
      t1330 = t87 * t1329
      t1334 = 0.176D3 * t1330 * t423 * t655 * x3
      t1336 = t175 * t490 * t126
      t1341 = 0.8D1 * t30 * t1336 * t631 * t890 * t10
      t1343 = t748 * t192
      t1348 = 0.48D2 * t87 * t269 * t1343 * t742 * t66 * t1237
      t1354 = 0.48D2 * t30 * t269 * t1343 * t231 * t66 * t1095
      t1355 = t423 * x2
      t1358 = 0.48D2 * t317 * t1355 * t951
      t1360 = t655 * t885 * t82
      t1362 = 0.14D2 * t962 * t1360
      t1364 = 0.6D1 * t896 * t184
      t1365 = t58 * t306
      t1369 = 0.32D2 * t1365 * t342 * t309 * t150
      t1373 = 0.8D1 * t966 * t371 * t293 * t82
      t1376 = t1274 * t230 * t1275 * t20
      t1380 = t299 * t300 * t301 * t10
      t1384 = t1274 * t230 * t1275 * t34
      t1386 = t175 * t181
      t1387 = t1386 * t126
      t1388 = t30 * t1387
      t1389 = t180 * t34
      t1392 = 0.6D1 * t1388 * t853 * t1389
      t1393 = t204 * t1321
      t1395 = 0.38D2 * t411 * t1393
      t1396 = t334 * t1050
      t1397 = t505 * t1396
      t1399 = t1324 - t1328 - t1334 + t1341 - t1348 - t1354 + t1358 + t1
     #362 - t1364 - t1369 + t1373 + 0.205D3 * t1376 - 0.67D2 * t1380 - 0
     #.67D2 * t1384 + t1392 + t1395 - 0.208D3 * t1397
      t1409 = 0.32D2 * t87 * t1183 * t342 * t39 * t179 * t66
      t1410 = t633 * x3
      t1413 = 0.32D2 * t190 * t423 * t1410
      t1414 = t1365 * t311
      t1416 = t334 * t210
      t1417 = t1280 * t1416
      t1420 = 0.80D2 * t215 * t514
      t1422 = x3 * t10
      t1425 = 0.74D2 * t215 * x1 * t76 * t1422
      t1427 = t273 * t728 * x3
      t1429 = 0.48D2 * t1054 * t1427
      t1433 = 0.6D1 * t416 * t204 * t401 * t82
      t1434 = t506 * x3
      t1437 = 0.104D3 * t574 * t423 * t1434
      t1440 = 0.120D3 * t416 * t204 * t402
      t1444 = 0.384D3 * t325 * t1055 * t1222 * t20
      t1447 = 0.384D3 * t1081 * t204 * t535
      t1448 = t782 * t260
      t1451 = t241 * t978 * t82
      t1453 = 0.32D2 * t286 * t1451
      t1456 = 0.64D2 * t864 * t60 * t1125
      t1459 = 0.32D2 * t864 * t60 * t69
      t1460 = -t1409 + t1413 + 0.144D3 * t1414 + 0.48D2 * t1417 - t1420 
     #+ t1425 + t1429 - t1433 + t1437 - t1440 + t1444 - t1447 - 0.74D2 *
     # t1448 - t1453 - t1456 + t1459
      t1461 = t941 * t10
      t1464 = 0.32D2 * t921 * t273 * t1461
      t1465 = t712 * t82
      t1468 = 0.32D2 * t915 * t334 * t1465
      t1469 = t30 * t605
      t1471 = 0.64D2 * t1469 * t790
      t1473 = t639 * t640 * t20
      t1475 = 0.48D2 * t1469 * t1473
      t1476 = t241 * t1121
      t1478 = 0.80D2 * t215 * t1476
      t1479 = t801 * t1094
      t1481 = 0.16D2 * t215 * t1479
      t1484 = 0.6D1 * t766 * t300 * t1076
      t1485 = t30 * t331
      t1487 = t300 * t564 * t82
      t1488 = t1485 * t1487
      t1491 = t300 * t564 * t77
      t1492 = t1485 * t1491
      t1495 = t58 * t556 * t126
      t1498 = t1495 * t273 * t1222 * t145
      t1500 = t26 * t65
      t1504 = t308 * t558 * t50
      t1506 = 0.32D2 * t237 * t1500 * t23 * t1504
      t1508 = t30 * t605 * t45
      t1509 = t49 * t145
      t1513 = 0.24D2 * t1508 * t395 * t1509 * t10
      t1514 = t432 * x3
      t1517 = 0.64D2 * t574 * t470 * t1514
      t1518 = t87 * t1387
      t1519 = t180 * t20
      t1522 = 0.6D1 * t1518 * t485 * t1519
      t1528 = 0.384D3 * t30 * t285 * t44 * t395 * t986 * x3
      t1529 = t466 * t236
      t1530 = t1529 * t271
      t1532 = t49 * t66 * x2
      t1534 = t1532 * t612 * t20
      t1535 = t1530 * t1534
      t1539 = 0.768D3 * t505 * t334 * t931
      t1540 = t1464 + t1468 - t1471 + t1475 - t1478 + t1481 + t1484 - 0.
     #48D2 * t1488 + 0.74D2 * t1492 - 0.32D2 * t1498 + t1506 + t1513 - t
     #1517 + t1522 + t1528 + 0.16D2 * t1535 + t1539
      t1544 = 0.64D2 * t915 * t334 * t713
      t1546 = t325 * t431 * t1514
      t1550 = t333 * t134 * x1 * t448
      t1554 = t230 * t40
      t1557 = 0.64D2 * t237 * t1500 * t76 * t1554 * t530
      t1559 = t334 * t335 * t82
      t1561 = 0.120D3 * t333 * t1559
      t1563 = 0.8D1 * t756 * t542
      t1567 = 0.80D2 * t852 * t631 * t45 * t1250
      t1569 = 0.74D2 * t272 * t1534
      t1572 = t342 * t1134 * t179
      t1574 = 0.260D3 * t87 * t1386 * t1572
      t1579 = 0.260D3 * t30 * t1386 * t342 * t343 * t179
      t1583 = 0.12D2 * t1038 * t853 * t178 * t583
      t1584 = t30 * t1329
      t1585 = t631 * t178
      t1586 = t66 * t34
      t1590 = 0.16D2 * t1584 * t1585 * t1586 * t10
      t1592 = 0.48D2 * t840 * t642
      t1593 = t87 * t983
      t1594 = t23 * t68
      t1598 = 0.64D2 * t1593 * t194 * t1594 * t34
      t1600 = 0.64D2 * t840 * t763
      t1602 = t1062 * t34
      t1605 = 0.64D2 * t291 * t66 * t23 * t1602
      t1609 = 0.5D1 * t557 * t308 * t558 * t145
      t1610 = -t1544 + 0.64D2 * t1546 - 0.64D2 * t1550 - t1557 - t1561 +
     # t1563 + t1567 - t1569 + t1574 + t1579 - t1583 + t1590 + t1592 - t
     #1598 - t1600 - t1605 - t1609
      t1613 = 0.32D2 * t190 * t423 * t634
      t1616 = 0.64D2 * t317 * t204 * t1056
      t1617 = t1025 * t566
      t1619 = t30 * t556
      t1620 = t1619 * t560
      t1625 = 0.32D2 * t87 * t1210 * t193 * t1226
      t1628 = 0.64D2 * t190 * t423 * t942
      t1629 = t66 * t20
      t1630 = t1629 * t34
      t1632 = t422 * t423 * t1630
      t1634 = t1529 * t410
      t1635 = t68 * t44
      t1639 = t1634 * t1635 * x2 * t56 * t82
      t1642 = t76 * t126
      t1645 = t1529 * t1297 * t1642 * t1635 * x2
      t1648 = t58 * t1183 * t1187
      t1651 = 0.64D2 * t400 * t861
      t1654 = 0.64D2 * t400 * t858 * t969
      t1655 = t348 * t927
      t1662 = 0.384D3 * t58 * t1 * t23 * t300 * t40 * t236
      t1663 = t631 * t195
      t1664 = t921 * t1663
      t1666 = t1166 * t1281
      t1669 = 0.32D2 * t1166 * t1416
      t1670 = t1613 - t1616 + 0.205D3 * t1617 + 0.205D3 * t1620 + t1625 
     #- t1628 - 0.133D3 * t1632 - 0.8D1 * t1639 - 0.72D2 * t1645 + 0.7D1
     # * t1648 + t1651 + t1654 - 0.74D2 * t1655 + t1662 - 0.128D3 * t166
     #4 + 0.240D3 * t1666 - t1669
      t1676 = 0.12D2 * t87 * t961 * t66 * t722
      t1678 = 0.32D2 * t872 * t355
      t1680 = 0.32D2 * t962 * t1022
      t1683 = 0.42D2 * t852 * t853 * t1144
      t1686 = 0.2D1 * t852 * t853 * t486
      t1690 = 0.120D3 * t1508 * t395 * t1509 * x3
      t1694 = 0.48D2 * t416 * t204 * x1 * t969
      t1698 = 0.384D3 * t190 * t476 * t916 * t20
      t1699 = t782 * t1012
      t1701 = t782 * t1016
      t1703 = t58 * t331
      t1704 = t1703 * t1487
      t1706 = t1703 * t1491
      t1708 = t1365 * t548
      t1710 = t30 * t878
      t1711 = t1710 * t1242
      t1714 = 0.48D2 * t872 * t244
      t1715 = t1529 * t269
      t1717 = t1715 * t748 * t49
      t1718 = t194 * t34
      t1719 = t26 * t39
      t1722 = t1717 * t1718 * t1719 * x3
      t1724 = -t1676 - t1678 - t1680 - t1683 + t1686 - t1690 - t1694 - t
     #1698 + 0.48D2 * t1699 - 0.74D2 * t1701 + 0.144D3 * t1704 + 0.144D3
     # * t1706 + 0.144D3 * t1708 - 0.7D1 * t1711 - t1714 - 0.16D2 * t172
     #2
      t1726 = t422 * t476 * t778
      t1728 = t1529 * t481
      t1729 = t1728 * t1314
      t1731 = t1055 * t951
      t1732 = t935 * t1731
      t1735 = t935 * t1055 * t1233
      t1739 = t372 * t1121
      t1741 = 0.48D2 * t87 * t983 * t66 * t1739
      t1742 = t87 * t306
      t1743 = t39 * t66
      t1746 = t1742 * t308 * t1743 * t50
      t1748 = t818 * t260
      t1750 = t348 * t1663
      t1752 = t50 * t23
      t1756 = 0.6D1 * t587 * t801 * t1752 * x3
      t1762 = 0.2D1 * t30 * t694 * t696 * t40 * t697 * t45
      t1764 = 0.32D2 * t1025 * t233
      t1768 = 0.176D3 * t1584 * t273 * t655 * t10
      t1772 = 0.176D3 * t1038 * t631 * t639 * t34
      t1775 = 0.384D3 * t1085 * t241 * t772
      t1776 = t237 * t238
      t1778 = 0.32D2 * t1776 * t90
      t1781 = 0.12D2 * t1081 * t204 * t627
      t1783 = t1120 * t986 * t513
      t1785 = -0.64D2 * t1726 - 0.8D1 * t1729 + 0.133D3 * t1732 + 0.133D
     #3 * t1735 - t1741 - 0.48D2 * t1746 - 0.128D3 * t1748 - 0.74D2 * t1
     #750 - t1756 + t1762 - t1764 - t1768 - t1772 + t1775 + t1778 - t178
     #1 + 0.64D2 * t1783
      t1790 = t215 * t220 * t76 * x3 * t10
      t1793 = t394 * t395 * t461
      t1795 = t400 * t672
      t1798 = 0.384D3 * t930 * t1103
      t1802 = 0.64D2 * t291 * x1 * t98 * t389
      t1805 = 0.48D2 * t370 * t967 * t860
      t1807 = 0.16D2 * t551 * t1198
      t1809 = 0.16D2 * t872 * t1479
      t1811 = 0.24D2 * t587 * t1191
      t1813 = 0.32D2 * t884 * t1022
      t1816 = 0.64D2 * t864 * t610 * t1422
      t1819 = 0.64D2 * t601 * t6 * t251
      t1821 = t286 * t728 * t819
      t1824 = t342 * t309 * t51
      t1826 = 0.42D2 * t307 * t1824
      t1829 = 0.64D2 * t468 * t481 * t1251
      t1831 = t467 * t811 * t181
      t1834 = 0.32D2 * t1831 * t594 * t412
      t1837 = 0.14D2 * t482 * t660 * t1226
      t1838 = -0.64D2 * t1790 + 0.133D3 * t1793 - 0.32D2 * t1795 + t1798
     # - t1802 - t1805 - t1807 + t1809 + t1811 - t1813 - t1816 - t1819 +
     # 0.32D2 * t1821 - t1826 + t1829 + t1834 + t1837
      t1840 = t317 * t1355 * t1050
      t1842 = t638 * t1473
      t1844 = t372 * t10
      t1845 = t395 * t1844
      t1847 = 0.48D2 * t406 * t1845
      t1848 = t1619 * t705
      t1851 = t801 * t1752 * t10
      t1853 = 0.32D2 * t286 * t1851
      t1855 = t241 * t1114 * t82
      t1857 = 0.64D2 * t286 * t1855
      t1861 = t1717 * t194 * t26 * t1270 * t20
      t1865 = 0.32D2 * t813 * t814 * t1194
      t1867 = 0.64D2 * t475 * t1198
      t1869 = t935 * t1055 * t666
      t1873 = 0.16D2 * t529 * t334 * t595
      t1875 = t273 * t326 * t10
      t1877 = 0.64D2 * t505 * t1875
      t1879 = 0.16D2 * t551 * t808
      t1881 = 0.32D2 * t1776 * t70
      t1882 = t237 * t247
      t1884 = 0.32D2 * t1882 * t108
      t1887 = 0.8D1 * t272 * t632 * t1461
      t1889 = 0.12D2 * t872 * t1476
      t1890 = -0.32D2 * t1840 - 0.205D3 * t1842 + t1847 - 0.67D2 * t1848
     # - t1853 + t1857 - 0.16D2 * t1861 + t1865 - t1867 + 0.133D3 * t186
     #9 + t1873 + t1877 - t1879 + t1881 + t1884 + t1887 - t1889
      t1895 = 0.16D2 * t215 * t244
      t1897 = 0.64D2 * t475 * t553
      t1899 = t574 * t470 * t433
      t1902 = t574 * t470 * t1179
      t1908 = 0.32D2 * t237 * t1500 * t748 * t134 * t82
      t1912 = 0.32D2 * t237 * t227 * t23 * t1277
      t1914 = 0.32D2 * t1882 * t147
      t1916 = t1120 * t986 * t243
      t1919 = t1529 * t323 * t684
      t1921 = t1634 * t1322
      t1923 = t58 * t271
      t1924 = t1923 * t657
      t1926 = t58 * t410
      t1927 = t1926 * t1427
      t1929 = t334 * t1233
      t1930 = t325 * t1929
      t1932 = t273 * t1630
      t1933 = t777 * t1932
      t1937 = 0.32D2 * t777 * t273 * t424
      t1938 = t505 * t1929
      t1940 = t1895 - t1897 - 0.64D2 * t1899 - 0.64D2 * t1902 + t1908 + 
     #t1912 + t1914 + 0.64D2 * t1916 - 0.72D2 * t1919 - 0.8D1 * t1921 + 
     #0.67D2 * t1924 - 0.205D3 * t1927 - 0.96D2 * t1930 - 0.133D3 * t193
     #3 - t1937 - 0.32D2 * t1938
      t1942 = t58 * t421 * t1932
      t1945 = 0.16D2 * t353 * t1479
      t1947 = 0.120D3 * t1593 * t1851
      t1949 = 0.104D3 * t1593 * t1855
      t1951 = 0.8D1 * t1593 * t1191
      t1955 = 0.6D1 * t1593 * t801 * t802 * t10
      t1959 = 0.48D2 * t521 * t522 * t523 * x3
      t1962 = 0.384D3 * t1085 * t205 * t873
      t1965 = 0.64D2 * t601 * t6 * t243
      t1968 = 0.64D2 * t601 * t6 * t1121
      t1970 = 0.32D2 * t1172 * t1195
      t1972 = 0.32D2 * t905 * t413
      t1975 = 0.6D1 * t1388 * t853 * t1519
      t1980 = 0.8D1 * t87 * t1336 * t193 * t890 * x3
      t1985 = 0.24D2 * t87 * t726 * t423 * t728 * t77
      t1987 = 0.14D2 * t1593 * t1451
      t1988 = t285 * t66
      t1989 = t58 * t1988
      t1991 = 0.64D2 * t1989 * t1739
      t1992 = -0.32D2 * t1942 + t1945 - t1947 + t1949 + t1951 - t1955 - 
     #t1959 + t1962 - t1965 + t1968 + t1970 + t1972 - t1975 + t1980 + t1
     #985 + t1987 + t1991
      t1997 = 0.384D3 * t574 * t1355 * t712 * z
      t2003 = t1715 * t1642 * t192 * t263 * t34 * t1719 * t20
      t2010 = t1715 * t181 * t126 * t68 * t257 * t26 * t768
      t2013 = t1120 * t986 * t251
      t2016 = 0.24D2 * t1593 * t590
      t2020 = 0.48D2 * t574 * t423 * t257 * t20
      t2024 = 0.64D2 * t416 * t204 * t205 * t10
      t2027 = 0.32D2 * t921 * t273 * t942
      t2031 = 0.16D2 * t1061 * t522 * t1062 * x3
      t2034 = t1280 * t334 * x2 * t671
      t2038 = 0.32D2 * t333 * t134 * t1259
      t2040 = t333 * t134 * t417
      t2042 = t293 * t513
      t2044 = 0.64D2 * t1989 * t2042
      t2047 = 0.48D2 * t30 * t1988 * t2042
      t2048 = t76 * t5
      t2052 = 0.48D2 * t1593 * t257 * t2048 * x3
      t2053 = t334 * t678
      t2055 = 0.12D2 * t534 * t2053
      t2056 = t30 * t358
      t2058 = t300 * t612 * t10
      t2060 = 0.16D2 * t2056 * t2058
      t2061 = t1997 + 0.16D2 * t2003 + 0.16D2 * t2010 + 0.64D2 * t2013 +
     # t2016 + t2020 - t2024 + t2027 + t2031 + 0.96D2 * t2034 - t2038 + 
     #0.32D2 * t2040 + t2044 - t2047 + t2052 - t2055 + t2060
      t2063 = t879 * t300 * t301
      t2066 = 0.14D2 * t1619 * t1504
      t2070 = 0.32D2 * t1469 * t841 * t842 * t150
      t2072 = 0.48D2 * t902 * t808
      t2074 = 0.16D2 * t359 * t2058
      t2075 = t325 * t952
      t2078 = 0.64D2 * t286 * t980
      t2080 = 0.32D2 * t286 * t1116
      t2084 = 0.768D3 * t1241 * t300 * t564 * z
      t2087 = 0.32D2 * t864 * t60 * t146
      t2090 = 0.8D1 * t551 * t940 * t1410
      t2093 = 0.24D2 * t411 * t1355 * t319
      t2096 = 0.64D2 * t921 * t273 * t634
      t2098 = t1926 * t273 * t1266
      t2100 = t655 * t20
      t2102 = t1923 * t631 * t2100
      t2105 = 0.32D2 * t905 * t1393
      t2108 = 0.128D3 * t57 * t1304 * t599
      t2109 = -0.7D1 * t2063 + t2066 + t2070 - t2072 + t2074 - 0.133D3 *
     # t2075 + t2078 - t2080 + t2084 + t2087 + t2090 + t2093 - t2096 - 0
     #.205D3 * t2098 + 0.67D2 * t2102 + t2105 + t2108
      t2114 = 0.32D2 * t1831 * t594 * t1321
      t2115 = t935 * t1175
      t2120 = 0.48D2 * t1584 * t1585 * t1629 * t10
      t2122 = t318 * t34
      t2124 = t1495 * t273 * z * t2122
      t2128 = 0.24D2 * t1054 * t1055 * t1465
      t2130 = 0.6D1 * t884 * t1360
      t2135 = 0.32D2 * t30 * t492 * t334 * t205 * t99
      t2137 = t308 * t1743 * t145
      t2138 = t1742 * t2137
      t2141 = t901 * t323 * t1301
      t2144 = t901 * t1386 * t1572
      t2147 = t348 * t273 * t1718
      t2151 = t935 * t334 * t257 * t10
      t2155 = 0.64D2 * t505 * t273 * t1434
      t2157 = t87 * t255 * t66
      t2158 = t2157 * t1845
      t2161 = 0.48D2 * t2157 * t397
      t2163 = 0.80D2 * t529 * t2053
      t2164 = t2114 + 0.133D3 * t2115 - t2120 - 0.64D2 * t2124 + t2128 +
     # t2130 + t2135 + 0.74D2 * t2138 + 0.128D3 * t2141 - 0.240D3 * t214
     #4 + 0.488D3 * t2147 + 0.488D3 * t2151 + t2155 - 0.32D2 * t2158 + t
     #2161 - t2163
      t2167 = 0.80D2 * t1054 * t431 * t471
      t2170 = 0.16D2 * t272 * t274 * t807
      t2171 = t394 * t1845
      t2176 = 0.6D1 * t1710 * t230 * t564 * t20
      t2177 = t300 * t569
      t2179 = 0.24D2 * t2056 * t2177
      t2181 = 0.5D1 * t557 * t1504
      t2183 = 0.38D2 * t359 * t2177
      t2185 = 0.2D1 * t1469 * t849
      t2187 = 0.42D2 * t1469 * t996
      t2191 = t901 * t270 * t308 * t1743 * t178
      t2196 = t58 * t385 * t45 * t866 * t1602
      t2201 = 0.384D3 * t835 * t300 * t437 * t10
      t2205 = 0.384D3 * t835 * t300 * t437 * x3
      t2208 = t1495 * t273 * t1222 * t50
      t2210 = t193 * t178
      t2214 = 0.48D2 * t1330 * t2210 * t1586 * x3
      t2218 = 0.16D2 * t1330 * t2210 * t1629 * x3
      t2220 = 0.80D2 * t482 * t1251
      t2221 = t2167 - t2170 + 0.133D3 * t2171 - t2176 + t2179 - t2181 - 
     #t2183 + t2185 - t2187 + 0.32D2 * t2191 + 0.96D2 * t2196 + t2201 + 
     #t2205 - 0.32D2 * t2208 - t2214 + t2218 + t2220
      t2224 = 0.104D3 * t325 * t1875
      t2226 = 0.384D3 * t353 * t874
      t2231 = t1728 * t192 * t45 * x2 * t145 * t56
      t2236 = t1530 * t1532 * t34 * t56 * t10
      t2239 = t1296 * t1307 * t684
      t2243 = 0.6D1 * t580 * t485 * t1039
      t2244 = t915 * t1731
      t2247 = 0.32D2 * t818 * t1012
      t2249 = 0.32D2 * t818 * t1016
      t2250 = t325 * t1396
      t2255 = 0.42D2 * t1742 * t342 * t1743 * t150
      t2257 = 0.32D2 * t400 * t1559
      t2259 = 0.80D2 * t411 * t472
      t2262 = 0.64D2 * t551 * t193 * t2100
      t2266 = t58 * t270 * t308 * t309 * t178
      t2270 = 0.120D3 * t411 * t1355 * t713
      t2274 = 0.64D2 * t587 * t194 * t1594 * t20
      t2275 = t2224 - t2226 - 0.8D1 * t2231 + 0.16D2 * t2236 - 0.1028D4 
     #* t2239 + t2243 - 0.32D2 * t2244 - t2247 - t2249 - 0.133D3 * t2250
     # - t2255 - t2257 + t2259 - t2262 + 0.86D2 * t2266 - t2270 - t2274
      t2279 = 0.48D2 * t587 * t257 * t2048 * t10
      t2282 = 0.32D2 * t286 * t205 * t1015
      t2285 = 0.32D2 * t286 * t205 * t259
      t2288 = 0.48D2 * t291 * t292 * t1844
      t2291 = 0.74D2 * t1495 * t273 * t2122
      t2293 = 0.176D3 * t835 * t613
      t2295 = 0.32D2 * t1365 * t1824
      t2298 = 0.6D1 * t1518 * t485 * t1389
      t2300 = t574 * t204 * t666
      t2305 = 0.2D1 * t756 * t300 * t40 * t114
      t2307 = 0.8D1 * t611 * t837
      t2310 = 0.32D2 * t286 * t194 * t1011
      t2312 = t348 * t632 * t1630
      t2315 = 0.74D2 * t551 * t477
      t2317 = 0.6D1 * t2056 * t361
      t2320 = 0.6D1 * t1038 * t853 * t908
      t2321 = t505 * t667
      t2323 = t2279 + t2282 + t2285 - t2288 + t2291 + t2293 - t2295 - t2
     #298 - 0.96D2 * t2300 + t2305 + t2307 + t2310 - 0.148D3 * t2312 - t
     #2315 + t2317 + t2320 - 0.32D2 * t2321
      t2331 = -t55 - t64 + t72 - t81 - t86 - t92 + t96 + t103 + t105 + t
     #107 - t110 + t113 + t118 + t120 + t122 - t131 + t137
      t2332 = -t140 - t144 - t149 - t154 - t156 - t158 - t160 - t162 + t
     #164 - t166 - t168 - t170 - t172 - t174 - t186 + t198 + t209
      t2340 = -0.288D3 * t212 - t219 + 0.48D2 * t222 + 0.48D2 * t225 - 0
     #.1508D4 * t234 + t246 + t254 + t262 + t268 + t279 + t284 - t289 - 
     #t297 - 0.102D3 * t304 - 0.40D2 * t312 - t322 - t330
      t2347 = t339 - t347 - 0.597D3 * t351 + t357 + t363 - t367 - t376 +
     # t384 + 0.80D2 * t391 - 0.30D2 * t398 + t405 + 0.128D3 * t407 - t4
     #15 + t420 - 0.48D2 * t426 + 0.48D2 * t435 - t441
      t2351 = -t445 + 0.80D2 * t450 - t455 - t460 - t464 - t474 + t479 +
     # t489 - t497 - t501 - t504 + t510 + t512 + t516 - t527 - t533 + t5
     #38
      t2357 = t544 + t546 + 0.120D3 * t549 + t555 - 0.597D3 * t561 - 0.5
     #97D3 * t567 + t573 - t577 + t586 - t592 + t598 - t604 + t609 - t61
     #5 - 0.30D2 * t619 - t625 - 0.80D2 * t629
      t2367 = -t637 + 0.102D3 * t643 - t648 + t652 - t654 + t659 + t665 
     #- 0.80D2 * t668 + 0.30D2 * t673 - 0.48D2 * t676 - 0.80D2 * t680 + 
     #0.984D3 * t685 - t692 - t702 - 0.597D3 * t706 - 0.597D3 * t710 - t
     #716
      t2375 = t724 - t732 - 0.40D2 * t736 + 0.120D3 * t740 + t746 - t752
     # - 0.48D2 * t754 - t758 + t760 + 0.12D2 * t764 - t771 + t776 + 0.8
     #0D2 * t780 - 0.120D3 * t783 + 0.12D2 * t791 - t796 - t800
      t2381 = t806 + t810 - t817 - t822 - t830 - t834 - t839 - t846 + t8
     #51 - t857 + t863 - 0.1508D4 * t869 + t876 + t883 - t889 - 0.80D2 *
     # t893 - t900
      t2385 = t904 - t907 - t911 + t914 + t920 + t925 + t929 - t934 - 0.
     #597D3 * t938 - t945 - t950 + 0.112D3 * t953 - t958 - t964 + t972 -
     # 0.32D2 * t974 + t977
      t2392 = -t982 - t990 + t994 - t998 - t1004 - t1008 - t1010 - 0.288
     #D3 * t1013 + 0.80D2 * t1017 - t1020 - 0.52D2 * t1023 - 0.12D2 * t1
     #026 + t1029 - t1037 - t1042 - t1046 + 0.30D2 * t1048
      t2396 = 0.30D2 * t1052 + t1059 + t1066 + t1070 - t1075 - t1080 + t
     #1084 + 0.288D3 * t1087 - t1093 + t1098 + t1101 + t1105 + t1108 - t
     #1113 - t1118 - 0.80D2 * t1123 - t1128
      t2401 = -0.48D2 * t1131 - t1138 - t1143 - t1147 - t1151 - t1155 - 
     #t1159 - t1162 + t1165 - t1171 - t1174 - t1177 + 0.48D2 * t1181 + t
     #1189 + t1193 - t1197 + t1200
      t2408 = -t1206 - t1209 - t1214 + t1218 - t1221 - t1225 + t1229 + 0
     #.30D2 * t1231 - 0.80D2 * t1235 + t1240 - 0.176D3 * t1243 + t1246 -
     # t1253 - 0.288D3 * t1254 + t1258 - 0.48D2 * t1261 + 0.80D2 * t1264
      t2412 = -t1269 - t1273 - t1279 - 0.120D3 * t1282 - t1289 - t1293 +
     # t1295 + 0.1508D4 * t1302 - t1310 + t1313 - t1316 - t1319 - t1324 
     #+ t1328 + t1334 - t1341 + t1348
      t2419 = t1354 - t1358 - t1362 + t1364 + t1369 - t1373 - 0.102D3 * 
     #t1376 - 0.12D2 * t1380 - 0.12D2 * t1384 - t1392 - t1395 + 0.112D3 
     #* t1397 + t1409 - t1413 - 0.192D3 * t1414 - 0.120D3 * t1417 + t142
     #0
      t2425 = -t1425 - t1429 + t1433 - t1437 + t1440 - t1444 + t1447 + 0
     #.40D2 * t1448 + t1453 + t1456 - t1459 - t1464 - t1468 + t1471 - t1
     #475 + t1478 - t1481
      t2432 = -t1484 + 0.120D3 * t1488 - 0.40D2 * t1492 + 0.48D2 * t1498
     # - t1506 - t1513 + t1517 - t1522 - t1528 - 0.32D2 * t1535 - t1539 
     #+ t1544 - 0.32D2 * t1546 + 0.80D2 * t1550 + t1557 + t1561 - t1563
      t2436 = -t1567 + t1569 - t1574 - t1579 + t1583 - t1590 - t1592 + t
     #1598 + t1600 + t1605 + t1609 - t1613 + t1616 - 0.102D3 * t1617 - 0
     #.102D3 * t1620 - t1625 + t1628
      t2444 = 0.30D2 * t1632 + 0.16D2 * t1639 + 0.144D3 * t1645 - 0.52D2
     # * t1648 - t1651 - t1654 + 0.40D2 * t1655 - t1662 + 0.80D2 * t1664
     # - 0.288D3 * t1666 + t1669 + t1676 + t1678 + t1680 + t1683 - t1686
     # + t1690
      t2460 = t1694 + t1698 - 0.120D3 * t1699 + 0.40D2 * t1701 - 0.192D3
     # * t1704 - 0.192D3 * t1706 - 0.192D3 * t1708 + 0.52D2 * t1711 + t1
     #714 + 0.32D2 * t1722 + 0.80D2 * t1726 + 0.16D2 * t1729 - 0.30D2 * 
     #t1732 - 0.30D2 * t1735 + t1741 + 0.120D3 * t1746 + 0.80D2 * t1748
      t2466 = 0.40D2 * t1750 + t1756 - t1762 + t1764 + t1768 + t1772 - t
     #1775 - t1778 + t1781 - 0.80D2 * t1783 + 0.32D2 * t1790 - 0.30D2 * 
     #t1793 - 0.64D2 * t1795 - t1798 + t1802 + t1805 + t1807
      t2473 = -t1809 - t1811 + t1813 + t1816 + t1819 - 0.128D3 * t1821 +
     # t1826 - t1829 - t1834 - t1837 + 0.128D3 * t1840 + 0.102D3 * t1842
     # - t1847 - 0.12D2 * t1848 + t1853 - t1857 + 0.32D2 * t1861
      t2477 = -t1865 + t1867 - 0.30D2 * t1869 - t1873 - t1877 + t1879 - 
     #t1881 - t1884 - t1887 + t1889 - t1895 + t1897 + 0.48D2 * t1899 + 0
     #.48D2 * t1902 - t1908 - t1912 - t1914
      t2490 = -0.48D2 * t1916 + 0.144D3 * t1919 + 0.16D2 * t1921 + 0.12D
     #2 * t1924 + 0.102D3 * t1927 + 0.240D3 * t1930 + 0.30D2 * t1933 + t
     #1937 - 0.64D2 * t1938 - 0.64D2 * t1942 - t1945 + t1947 - t1949 - t
     #1951 + t1955 + t1959 - t1962
      t2494 = t1965 - t1968 - t1970 - t1972 + t1975 - t1980 - t1985 - t1
     #987 - t1991 - t1997 - 0.32D2 * t2003 - 0.32D2 * t2010 - 0.48D2 * t
     #2013 - t2016 - t2020 + t2024 - t2027
      t2500 = -t2031 - 0.240D3 * t2034 + t2038 - 0.48D2 * t2040 - t2044 
     #+ t2047 - t2052 + t2055 - t2060 + 0.52D2 * t2063 - t2066 - t2070 +
     # t2072 - t2074 + 0.30D2 * t2075 - t2078 + t2080
      t2505 = t40 * t39
      t2512 = t56 * t26 * t43
      t2518 = t73 * t1298
      t2542 = -t2084 - t2087 - t2090 - t2093 + t2096 + 0.102D3 * t2098 +
     # 0.12D2 * t2102 - t2105 + 0.16D2 * t73 * t1306 * t23 * t308 * t250
     #5 * t66 * t50 - 0.32D2 * t73 * t2512 * t76 * t594 * t1233 + 0.32D2
     # * t2518 * t1554 * t1050 - 0.32D2 * t73 * t1297 * t23 * t308 * t40
     # * t1630 - 0.32D2 * t73 * t1297 * t98 * t300 * t40 * t671 + 0.32D2
     # * t2518 * t1554 * t951 - 0.32D2 * t73 * t1307 * t230 * t2505 * t6
     #66 - t2108 - t2114
      t2553 = -0.30D2 * t2115 + t2120 + 0.32D2 * t2124 - t2128 - t2130 -
     # t2135 - 0.40D2 * t2138 - 0.80D2 * t2141 + 0.288D3 * t2144 - 0.597
     #D3 * t2147 - 0.597D3 * t2151 - t2155 + 0.128D3 * t2158 - t2161 + t
     #2163 - t2167 + t2170
      t2558 = -0.30D2 * t2171 + t2176 - t2179 + t2181 + t2183 - t2185 + 
     #t2187 - 0.128D3 * t2191 - 0.240D3 * t2196 - t2201 - t2205 + 0.48D2
     # * t2208 + t2214 - t2218 - t2220 - t2224 + t2226
      t2566 = 0.16D2 * t2231 - 0.32D2 * t2236 + 0.1508D4 * t2239 - t2243
     # + 0.128D3 * t2244 + t2247 + t2249 + 0.30D2 * t2250 + t2255 + t225
     #7 - t2259 + t2262 - 0.176D3 * t2266 + t2270 + t2274 - t2279 - t228
     #2
      t2585 = -t2285 + t2288 - t2291 - t2293 + t2295 + t2298 + 0.240D3 *
     # t2300 - t2305 - t2307 - t2310 + 0.80D2 * t2312 + t2315 - t2317 - 
     #t2320 - 0.64D2 * t2321 + 0.16D2 * t73 * t2512 * t23 * t2137 + 0.16
     #D2 * t73 * t2512 * t98 * t735 + 0.16D2 * t73 * t1306 * t98 * t300 
     #* t2505 * x1 * t77
      rrgg2ggh11J2 = -0.9D1 / 0.16D2 * (0.2D1 * wd * (t1399 + t959 + t20
     #61 + t1992 + t2323 + t1247 + t517 + t2109 + t1178 + t1610 + t703 +
     # t1109 + t2164 + t1670 + t785 + t877 + t1540 + t2221 + t616 + t199
     # + t1724 + t1460 + t1785 + t1940 + t1030 + t1890 + t2275 + t314 + 
     #t1838 + t1320 + t132 + t428) + wd * (t2340 + t2408 + t2585 + t2351
     # + t2542 + t2477 + t2490 + t2558 + t2401 + t2357 + t2432 + t2566 +
     # t2466 + t2473 + t2381 + t2332 + t2385 + t2436 + t2500 + t2375 + t
     #2347 + t2460 + t2425 + t2331 + t2444 + t2553 + t2367 + t2419 + t23
     #96 + t2494 + t2392 + t2412)) / t56 / t465 / t40 / z / 0.3141592653
     #589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh11J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t6 * t20 - t2 * t24
      t27 = s ** 2
      t28 = t27 ** 2
      t29 = t28 * t27
      t30 = t26 * t29
      t33 = x2 * x3
      t34 = t10 * t7 * t4 + t33 + t19
      t37 = t23 * t10
      t39 = s - t2 * t6 * t34 - t2 * t37
      t40 = t39 ** 2
      t41 = t30 * t40
      t42 = t1 ** 2
      t43 = t42 ** 2
      t44 = x1 ** 2
      t45 = t44 ** 2
      t46 = t43 * t45
      t47 = t4 ** 2
      t49 = 0.1D1 / t47 / t4
      t50 = t20 ** 2
      t51 = t50 * t20
      t53 = t46 * t49 * t51
      t55 = 0.2D1 * t41 * t53
      t56 = t26 ** 2
      t57 = t56 * t29
      t58 = t57 * t39
      t59 = t42 * t44
      t61 = t59 * t5 * t34
      t63 = 0.6D1 * t58 * t61
      t64 = t42 * t1
      t65 = t44 * x1
      t66 = t64 * t65
      t67 = 0.1D1 / t47
      t68 = t34 ** 2
      t69 = t67 * t68
      t70 = t66 * t69
      t72 = 0.6D1 * t58 * t70
      t73 = t68 * t34
      t75 = t46 * t49 * t73
      t77 = 0.2D1 * t58 * t75
      t79 = 0.8D1 * t41 * t61
      t81 = 0.24D2 * t41 * t70
      t83 = 0.32D2 * t41 * t75
      t84 = t28 * s
      t85 = t56 * t84
      t86 = t85 * t40
      t88 = 0.176D3 * t86 * t61
      t90 = 0.38D2 * t86 * t70
      t91 = t5 * t20
      t92 = t59 * t91
      t94 = 0.8D1 * t58 * t92
      t95 = t67 * t50
      t96 = t66 * t95
      t98 = 0.24D2 * t58 * t96
      t100 = 0.32D2 * t58 * t53
      t102 = 0.6D1 * t41 * t92
      t104 = 0.6D1 * t41 * t96
      t108 = z + x1 * t7 * t1
      t109 = t23 * t108
      t110 = t5 * t40
      t111 = t109 * t110
      t113 = 0.2D1 * t30 * t1 * t111
      t115 = 0.176D3 * t86 * t92
      t116 = t55 + t63 + t72 + t77 + t79 + t81 + t83 + t88 - t90 + t94 +
     # t98 + t100 + t102 + t104 + t113 + t115
      t118 = 0.38D2 * t86 * t96
      t119 = t84 * z
      t120 = t119 * t56
      t121 = t40 * t42
      t122 = t23 ** 2
      t123 = x3 ** 2
      t124 = t122 * t123
      t127 = 0.32D2 * t120 * t121 * t124
      t128 = t10 ** 2
      t129 = t122 * t128
      t132 = 0.32D2 * t120 * t121 * t129
      t133 = t64 * x1
      t134 = t133 * t129
      t136 = 0.14D2 * t58 * t134
      t137 = t42 * x1
      t140 = 0.6D1 * t58 * t137 * t37
      t141 = t43 * x1
      t142 = t122 * t23
      t143 = t128 * t10
      t145 = t141 * t142 * t143
      t146 = t58 * t145
      t147 = 0.42D2 * t146
      t148 = t86 * t145
      t149 = 0.32D2 * t148
      t150 = t86 * t134
      t151 = 0.5D1 * t150
      t152 = t133 * t124
      t154 = 0.14D2 * t41 * t152
      t157 = 0.6D1 * t41 * t137 * t24
      t158 = t123 * x3
      t160 = t141 * t142 * t158
      t161 = t41 * t160
      t162 = 0.42D2 * t161
      t163 = t86 * t160
      t164 = 0.32D2 * t163
      t165 = t86 * t152
      t166 = 0.5D1 * t165
      t168 = t5 * t39
      t171 = 0.2D1 * t57 * t1 * t109 * t168
      t174 = 0.260D3 * t85 * t1 * t111
      t175 = t43 * t1
      t176 = t40 * t175
      t177 = t30 * t176
      t178 = t44 * x2
      t179 = t142 * t5
      t180 = t179 * t128
      t181 = t178 * t180
      t183 = 0.32D2 * t177 * t181
      t184 = t179 * t123
      t185 = t178 * t184
      t186 = t177 * t185
      t188 = -t118 + t127 + t132 + t136 - t140 - t147 - t149 - t151 + t1
     #54 - t157 - t162 - t164 - t166 + t171 - t174 - t183 - 0.128D3 * t1
     #86
      t190 = t43 * t42
      t191 = t40 * t190
      t192 = t30 * t191
      t193 = t122 ** 2
      t194 = t193 * t5
      t196 = t178 * t194 * t158
      t197 = t192 * t196
      t198 = 0.42D2 * t197
      t199 = t43 * t64
      t200 = t40 * t199
      t201 = t30 * t200
      t202 = x2 ** 2
      t203 = t65 * t202
      t204 = t193 * t67
      t206 = t203 * t204 * t128
      t208 = 0.6D1 * t201 * t206
      t209 = t64 * t142
      t210 = t57 * t209
      t211 = t108 * t5
      t212 = t39 * t128
      t215 = 0.6D1 * t210 * t211 * t212
      t216 = t42 * t23
      t217 = t57 * t216
      t218 = t39 * x1
      t220 = t217 * t211 * t218
      t222 = t30 * t216
      t223 = t40 * x1
      t224 = t211 * t223
      t225 = t222 * t224
      t227 = t43 * t122
      t228 = t227 * t108
      t229 = t30 * t228
      t230 = t49 * t40
      t231 = t44 * t50
      t233 = t230 * t231 * x3
      t235 = 0.14D2 * t229 * t233
      t236 = t175 * t142
      t237 = t236 * t108
      t238 = t57 * t237
      t239 = t67 * t39
      t240 = t239 * t44
      t242 = t20 * t10 * x3
      t245 = 0.64D2 * t238 * t240 * t242
      t246 = t85 * t209
      t247 = t40 * t128
      t248 = t211 * t247
      t250 = 0.38D2 * t246 * t248
      t251 = t199 * t193
      t253 = t47 ** 2
      t254 = 0.1D1 / t253
      t255 = t108 * t254
      t256 = t202 * x2
      t259 = t255 * t40 * t256 * t65
      t260 = t85 * t251 * t259
      t262 = t30 * t237
      t263 = t67 * t40
      t264 = t263 * t44
      t266 = t34 * x3 * t10
      t269 = 0.64D2 * t262 * t264 * t266
      t270 = t230 * x2
      t271 = t44 * t20
      t272 = t271 * t10
      t273 = t270 * t272
      t274 = t262 * t273
      t276 = t44 * t34
      t277 = t276 * t10
      t278 = t263 * t277
      t279 = t229 * t278
      t281 = t39 * t199
      t282 = t57 * t281
      t283 = t45 * x1
      t284 = t283 * t202
      t285 = t122 * t254
      t289 = 0.24D2 * t282 * t284 * t285 * t50
      t291 = 0.14D2 * t282 * t206
      t292 = t43 ** 2
      t293 = t193 * t23
      t295 = t292 * t293 * t108
      t297 = t254 * t40
      t298 = t256 * t65
      t302 = 0.8D1 * t30 * t295 * t297 * t298 * t10
      t303 = t292 * t193
      t304 = t303 * t108
      t305 = t30 * t304
      t307 = 0.1D1 / t253 / t4
      t308 = t307 * t40
      t309 = t256 * t45
      t310 = t309 * t20
      t313 = 0.6D1 * t305 * t308 * t310
      t314 = -t198 + t208 + t215 - 0.7D1 * t220 - 0.7D1 * t225 + t235 - 
     #t245 - t250 + 0.7D1 * t260 - t269 - 0.32D2 * t274 - 0.96D2 * t279 
     #+ t289 + t291 + t302 - t313
      t315 = t175 * t23
      t316 = t315 * t108
      t319 = t45 * t50 * t34
      t321 = t30 * t316 * t297 * t319
      t322 = 0.32D2 * t321
      t324 = t175 * t122 * t108
      t325 = t30 * t324
      t326 = t65 * t68
      t327 = t326 * t10
      t330 = 0.32D2 * t325 * t230 * t327
      t331 = t64 * t23
      t333 = t85 * t331 * t108
      t334 = t271 * t34
      t336 = t333 * t230 * t334
      t337 = 0.74D2 * t336
      t338 = t43 * t23
      t339 = t57 * t338
      t340 = t108 * t49
      t341 = t39 * t65
      t344 = t339 * t340 * t341 * t50
      t346 = t43 * t142
      t347 = t57 * t346
      t349 = t211 * t218 * t128
      t350 = t347 * t349
      t354 = t339 * t255 * t341 * t73
      t355 = 0.42D2 * t354
      t356 = t43 * t193
      t357 = t57 * t356
      t361 = 0.2D1 * t357 * t211 * t39 * t143
      t365 = 0.32D2 * t357 * t211 * t39 * t158
      t367 = t30 * t40 * t292
      t368 = t193 * t49
      t370 = t309 * t368 * t10
      t372 = 0.6D1 * t367 * t370
      t373 = t30 * t331
      t374 = t40 * t44
      t376 = t340 * t374 * t50
      t378 = 0.14D2 * t373 * t376
      t379 = t142 * t49
      t380 = t309 * t379
      t382 = 0.32D2 * t201 * t380
      t383 = t40 * t64
      t384 = t85 * t383
      t385 = t44 * t5
      t386 = t20 * t23
      t387 = t386 * t10
      t388 = t385 * t387
      t390 = 0.16D2 * t384 * t388
      t391 = t108 * t67
      t395 = 0.6D1 * t222 * t391 * t223 * t20
      t396 = t251 * t108
      t397 = t30 * t396
      t401 = 0.176D3 * t397 * t230 * t203 * t10
      t402 = t85 * t331
      t404 = t391 * t374 * t34
      t405 = t402 * t404
      t407 = t39 * t190
      t408 = t57 * t407
      t410 = t178 * t194 * t143
      t411 = t408 * t410
      t412 = 0.42D2 * t411
      t413 = t175 * t193
      t414 = t413 * t108
      t415 = t30 * t414
      t416 = x1 * x2
      t417 = t416 * t123
      t418 = t263 * t417
      t420 = 0.32D2 * t415 * t418
      t421 = t322 + t330 + t337 - 0.48D2 * t344 + 0.74D2 * t350 - t355 +
     # t361 + t365 + t372 + t378 - t382 + t390 - t395 - t401 + 0.488D3 *
     # t405 - t412 - t420
      t424 = t45 * x2
      t425 = t23 * t49
      t426 = t425 * t50
      t427 = t424 * t426
      t429 = 0.32D2 * t177 * t427
      t431 = t190 * t193 * t108
      t432 = t57 * t431
      t433 = t49 * t39
      t434 = t202 * t44
      t435 = t434 * t10
      t438 = 0.48D2 * t432 * t433 * t435
      t439 = t190 * t142
      t440 = t439 * t108
      t441 = t57 * t440
      t442 = t254 * t39
      t443 = t203 * t20
      t446 = 0.64D2 * t441 * t442 * t443
      t447 = t27 * s
      t448 = t28 * t447
      t449 = t26 * t448
      t451 = t190 * t122 * t108
      t452 = t449 * t451
      t453 = t424 * t50
      t454 = t442 * t453
      t456 = 0.32D2 * t452 * t454
      t457 = t449 * t431
      t458 = t178 * t128
      t459 = t239 * t458
      t461 = 0.32D2 * t457 * t459
      t462 = t40 * t43
      t465 = x2 * t122
      t466 = t5 * z
      t470 = 0.384D3 * t30 * t462 * t44 * t465 * t466 * x3
      t471 = z ** 2
      t472 = t448 * t471
      t473 = t472 * t440
      t475 = t49 * t65 * x2
      t476 = t40 * x3
      t478 = t475 * t476 * t20
      t479 = t473 * t478
      t480 = 0.16D2 * t479
      t481 = t64 * t122
      t482 = t481 * t108
      t483 = t85 * t482
      t484 = x1 * t34
      t485 = t484 * t10
      t487 = t483 * t263 * t485
      t488 = 0.80D2 * t487
      t489 = t338 * t108
      t490 = t57 * t489
      t491 = t65 * t20
      t492 = t491 * t34
      t494 = t490 * t433 * t492
      t496 = t472 * t431
      t497 = t67 * t44
      t501 = t496 * t497 * x2 * t56 * t128
      t502 = 0.8D1 * t501
      t504 = x2 * t20
      t505 = t504 * t34
      t506 = t442 * t45 * t505
      t508 = 0.48D2 * t452 * t506
      t509 = t57 * t228
      t510 = z * t20
      t511 = t510 * t10
      t513 = t509 * t240 * t511
      t515 = z * t34
      t516 = t515 * x3
      t518 = t509 * t240 * t516
      t520 = t276 * x3
      t521 = t263 * t520
      t522 = t229 * t521
      t524 = t85 * t324
      t525 = t65 * x2
      t526 = t525 * t34
      t528 = t524 * t230 * t526
      t531 = t30 * t191 * t45
      t532 = t49 * t68
      t535 = t531 * t465 * t532 * x3
      t536 = 0.120D3 * t535
      t537 = -t429 + t438 - t446 + t456 + t461 + t470 + t480 - t488 - 0.
     #133D3 * t494 - t502 + t508 - 0.64D2 * t513 - 0.64D2 * t518 - 0.133
     #D3 * t522 + 0.488D3 * t528 - t536
      t539 = t30 * t191 * t65
      t540 = x2 * t142
      t541 = t67 * t34
      t544 = t539 * t540 * t541 * t123
      t545 = 0.104D3 * t544
      t546 = t309 * t34
      t549 = 0.6D1 * t305 * t308 * t546
      t551 = t203 * t204 * t123
      t553 = 0.14D2 * t201 * t551
      t554 = t283 * x2
      t555 = t23 * t254
      t559 = 0.32D2 * t408 * t554 * t555 * t51
      t560 = t57 * t324
      t561 = t326 * x3
      t564 = 0.64D2 * t560 * t433 * t561
      t565 = t85 * t462
      t568 = 0.32D2 * t565 * t416 * t180
      t571 = 0.32D2 * t565 * t416 * t184
      t572 = t448 * z
      t573 = t26 * t190
      t575 = t572 * t573 * t122
      t576 = t255 * t39
      t577 = t424 * t68
      t580 = 0.32D2 * t575 * t576 * t577
      t582 = 0.32D2 * t282 * t380
      t583 = t39 * t175
      t584 = t57 * t583
      t585 = t122 * t67
      t586 = t203 * t585
      t588 = 0.48D2 * t584 * t586
      t589 = t472 * t190
      t594 = t10 * t39
      t595 = t594 * x3
      t597 = t589 * t193 * t108 * t67 * t178 * t26 * t595
      t598 = 0.16D2 * t597
      t599 = t142 * t108
      t601 = t589 * t599 * t49
      t605 = t601 * t525 * t26 * t594 * t20
      t606 = 0.16D2 * t605
      t608 = t391 * t374 * t20
      t609 = t373 * t608
      t611 = t30 * t481
      t613 = t211 * t223 * t10
      t614 = t611 * t613
      t616 = t373 * t404
      t618 = t39 * t43
      t619 = t57 * t618
      t620 = t65 * t67
      t621 = t68 * t23
      t625 = 0.6D1 * t619 * t620 * t621 * t10
      t626 = t449 * t440
      t627 = t433 * t65
      t628 = t504 * t10
      t629 = t627 * t628
      t631 = 0.48D2 * t626 * t629
      t632 = t545 + t549 + t553 + t559 - t564 + t568 + t571 + t580 - t58
     #2 + t588 + t598 - t606 - 0.67D2 * t609 + 0.205D3 * t614 + 0.205D3 
     #* t616 - t625 - t631
      t634 = t432 * t459
      t635 = 0.5D1 * t634
      t636 = t85 * t237
      t639 = t636 * t263 * t178 * t10
      t642 = t211 * t223 * x3
      t643 = t611 * t642
      t645 = t572 * t26
      t646 = t645 * t440
      t648 = t627 * t504 * x3
      t650 = 0.64D2 * t646 * t648
      t651 = t449 * t228
      t652 = t178 * t471
      t653 = t239 * t652
      t655 = 0.384D3 * t651 * t653
      t657 = t572 * t573 * t193
      t658 = t391 * t39
      t659 = t178 * t123
      t662 = 0.32D2 * t657 * t658 * t659
      t666 = 0.48D2 * t229 * t230 * t178 * t34
      t667 = t33 * t10
      t668 = t240 * t667
      t669 = t432 * t668
      t670 = 0.80D2 * t669
      t671 = x2 * t34
      t672 = t671 * x3
      t673 = t627 * t672
      t675 = 0.16D2 * t441 * t673
      t676 = t29 * t471
      t677 = t26 * t64
      t682 = 0.32D2 * t676 * t677 * t599 * t110 * t128
      t683 = t56 * t64
      t686 = t39 * t44
      t688 = t340 * t686 * t68
      t689 = t676 * t683 * t23 * t688
      t690 = 0.32D2 * t689
      t693 = 0.64D2 * t645 * t431 * t668
      t694 = t671 * t10
      t695 = t627 * t694
      t697 = 0.64D2 * t646 * t695
      t698 = t85 * t228
      t700 = t230 * t231 * t10
      t702 = 0.64D2 * t698 * t700
      t703 = t346 * t108
      t704 = t85 * t703
      t705 = x1 * t20
      t707 = t263 * t705 * t128
      t709 = 0.32D2 * t704 * t707
      t710 = t34 * t122
      t712 = t385 * t710 * t128
      t714 = 0.14D2 * t619 * t712
      t715 = t57 * t703
      t719 = 0.6D1 * t715 * t239 * t484 * t128
      t720 = t635 + 0.488D3 * t639 - 0.67D2 * t643 - t650 + t655 + t662 
     #+ t666 + t670 - t675 + t682 + t690 + t693 - t697 + t702 - t709 + t
     #714 - t719
      t723 = 0.32D2 * t575 * t576 * t453
      t725 = 0.64D2 * t646 * t629
      t726 = t50 * t23
      t728 = t620 * t726 * t10
      t730 = 0.32D2 * t565 * t728
      t731 = t20 * t122
      t733 = t385 * t731 * t128
      t735 = 0.64D2 * t565 * t733
      t736 = t239 * t659
      t738 = 0.32D2 * t457 * t736
      t740 = t57 * t39 * t64
      t741 = t34 * t23
      t742 = t741 * t10
      t743 = t385 * t742
      t745 = 0.12D2 * t740 * t743
      t747 = t309 * t368 * x3
      t749 = 0.6D1 * t367 * t747
      t751 = 0.38D2 * t432 * t736
      t752 = x1 * z
      t754 = t384 * t752 * t129
      t757 = t384 * t752 * t124
      t762 = t5 * x3 * t10
      t764 = t85 * t176 * t44 * t540 * t762
      t766 = t176 * t65
      t767 = t85 * t766
      t768 = t541 * x3
      t769 = t465 * t768
      t770 = t767 * t769
      t772 = t297 * x2
      t774 = t524 * t772 * t492
      t776 = t57 * t451
      t778 = 0.38D2 * t776 * t454
      t779 = t384 * t743
      t780 = 0.80D2 * t779
      t781 = t30 * t383
      t782 = t23 * t5
      t783 = t782 * t471
      t784 = t178 * t783
      t786 = 0.384D3 * t781 * t784
      t789 = t402 * t340 * t374 * t68
      t790 = 0.5D1 * t789
      t791 = t723 - t725 - t730 + t735 + t738 - t745 - t749 + t751 - 0.3
     #2D2 * t754 - 0.32D2 * t757 - 0.148D3 * t764 + 0.133D3 * t770 - 0.1
     #48D3 * t774 + t778 - t780 - t786 - t790
      t795 = t42 * t122
      t796 = t85 * t795
      t798 = t211 * t40 * t10
      t800 = 0.176D3 * t796 * t798
      t802 = 0.32D2 * t584 * t185
      t803 = t425 * t68
      t804 = t424 * t803
      t806 = 0.32D2 * t584 * t804
      t807 = t30 * t795
      t808 = t211 * t476
      t810 = 0.6D1 * t807 * t808
      t812 = t199 * t142 * t108
      t813 = t30 * t812
      t814 = t45 * t202
      t818 = 0.176D3 * t813 * t297 * t814 * t34
      t823 = 0.32D2 * t676 * t683 * t599 * t168 * t123
      t824 = t263 * z
      t825 = t705 * t10
      t827 = t483 * t824 * t825
      t829 = t56 * t40
      t830 = t119 * t829
      t831 = x3 * t10
      t834 = 0.64D2 * t830 * t795 * t831
      t836 = t119 * t829 * t42
      t837 = t741 * x3
      t840 = 0.64D2 * t836 * t6 * t837
      t841 = t752 * t128
      t843 = t715 * t168 * t841
      t845 = t752 * t123
      t848 = 0.32D2 * t715 * t168 * t845
      t850 = t85 * t383 * t44
      t852 = t850 * t466 * t837
      t854 = t386 * x3
      t856 = t850 * t466 * t854
      t858 = t462 * t65
      t859 = t85 * t858
      t860 = t67 * t20
      t861 = t860 * t742
      t863 = 0.64D2 * t859 * t861
      t864 = t541 * t854
      t866 = 0.64D2 * t859 * t864
      t868 = t57 * t407 * t45
      t869 = t49 * t50
      t873 = 0.24D2 * t868 * t465 * t869 * x3
      t874 = t800 - t802 - t806 + t810 - t818 + t823 + 0.64D2 * t827 - t
     #834 - t840 + 0.32D2 * t843 - t848 + 0.64D2 * t852 + 0.64D2 * t856 
     #+ t863 + t866 + t873
      t876 = t57 * t407 * t65
      t880 = 0.8D1 * t876 * t540 * t860 * t123
      t881 = t85 * t481
      t882 = t881 * t613
      t886 = t636 * t263 * t178 * x3
      t888 = t178 * t782
      t890 = 0.32D2 * t781 * t888
      t892 = t229 * t264 * t516
      t894 = t20 * t34
      t895 = t620 * t894
      t897 = 0.16D2 * t781 * t895
      t898 = t619 * t728
      t899 = 0.120D3 * t898
      t900 = t57 * t331
      t903 = t900 * t391 * t686 * t34
      t905 = t30 * t703
      t906 = t905 * t707
      t907 = 0.120D3 * t906
      t908 = t30 * t356
      t909 = t40 * t123
      t911 = t211 * t909 * t10
      t913 = 0.8D1 * t908 * t911
      t914 = t705 * x3
      t915 = t263 * t914
      t916 = t483 * t915
      t917 = 0.80D2 * t916
      t919 = t211 * t476 * t10
      t921 = 0.16D2 * t246 * t919
      t922 = t510 * x3
      t924 = t229 * t264 * t922
      t925 = 0.64D2 * t924
      t928 = z * x3 * t10
      t930 = t905 * t110 * x1 * t928
      t932 = t271 * t128
      t935 = 0.32D2 * t262 * t263 * t932
      t937 = 0.6D1 * t282 * t551
      t938 = t540 * t67
      t941 = 0.48D2 * t539 * t938 * t266
      t942 = t880 + 0.488D3 * t882 + 0.488D3 * t886 - t890 - 0.64D2 * t8
     #92 + t897 - t899 - 0.67D2 * t903 - t907 + t913 - t917 + t921 + t92
     #5 - 0.64D2 * t930 + t935 + t937 - t941
      t944 = t56 * t39
      t945 = t676 * t944
      t947 = 0.32D2 * t945 * t96
      t948 = t26 * t40
      t949 = t676 * t948
      t950 = t949 * t152
      t951 = 0.32D2 * t950
      t952 = t85 * t121
      t955 = 0.384D3 * t952 * t416 * t783
      t958 = 0.64D2 * t836 * t6 * t387
      t961 = 0.64D2 * t836 * t6 * t742
      t962 = t541 * t10
      t964 = t767 * t465 * t962
      t968 = 0.32D2 * t830 * t59 * t95
      t971 = t391 * t374 * x2
      t972 = t472 * t227 * t971
      t973 = 0.72D2 * t972
      t974 = x2 * t40
      t976 = t497 * t974 * t123
      t977 = t496 * t976
      t978 = 0.8D1 * t977
      t979 = t683 * t122
      t982 = t391 * t974 * x1
      t983 = t119 * t979 * t982
      t985 = t30 * t462
      t986 = t45 * t49
      t988 = t986 * t50 * t34
      t990 = 0.8D1 * t985 * t988
      t991 = t85 * t414
      t992 = t991 * t418
      t994 = t525 * t68
      t995 = t297 * t994
      t996 = t524 * t995
      t998 = t30 * t338
      t999 = t40 * t65
      t1001 = t255 * t999 * t51
      t1002 = t998 * t1001
      t1003 = 0.42D2 * t1002
      t1007 = 0.32D2 * t908 * t211 * t40 * t143
      t1009 = t57 * t39 * t292
      t1010 = t283 * t256
      t1011 = t142 * t254
      t1015 = 0.8D1 * t1009 * t1010 * t1011 * t20
      t1017 = t509 * t239 * t520
      t1019 = t947 + t951 + t955 - t958 + t961 + 0.133D3 * t964 + t968 -
     # t973 - t978 + 0.1028D4 * t983 + t990 + 0.48D2 * t992 - 0.74D2 * t
     #996 - t1003 + t1007 + t1015 - 0.133D3 * t1017
      t1020 = t263 * t272
      t1021 = t229 * t1020
      t1026 = 0.2D1 * t908 * t211 * t40 * t158
      t1030 = 0.48D2 * t509 * t433 * t178 * t20
      t1034 = 0.64D2 * t715 * t239 * t416 * t10
      t1035 = t471 * z
      t1037 = t26 * t43
      t1038 = t1037 * t122
      t1041 = t391 * t686 * x2
      t1043 = 0.128D3 * t448 * t1035 * t1038 * t1041
      t1049 = 0.384D3 * t85 * t1 * t23 * t211 * t40 * t471
      t1050 = t40 * z
      t1054 = 0.384D3 * t796 * t211 * t1050 * t10
      t1056 = 0.6D1 * t1009 * t747
      t1057 = t515 * t10
      t1059 = t509 * t240 * t1057
      t1060 = 0.64D2 * t1059
      t1061 = t30 * t451
      t1062 = t424 * t73
      t1065 = 0.2D1 * t1061 * t308 * t1062
      t1067 = t190 * t293 * t108
      t1072 = 0.32D2 * t30 * t1067 * t263 * t416 * t143
      t1073 = t57 * t812
      t1074 = t307 * t39
      t1076 = t45 * t20
      t1077 = t1076 * t34
      t1080 = 0.12D2 * t1073 * t1074 * t202 * t1077
      t1081 = t30 * t431
      t1082 = t276 * t128
      t1085 = 0.24D2 * t1081 * t270 * t1082
      t1086 = t776 * t506
      t1087 = 0.80D2 * t1086
      t1089 = 0.32D2 * t740 * t888
      t1090 = t85 * t191
      t1092 = t1090 * t298 * t379
      t1095 = 0.32D2 * t325 * t995
      t1096 = -0.133D3 * t1021 + t1026 + t1030 - t1034 + t1043 + t1049 +
     # t1054 + t1056 + t1060 + t1065 + t1072 - t1080 + t1085 + t1087 - t
     #1089 + 0.128D3 * t1092 - t1095
      t1099 = t178 * z
      t1102 = 0.384D3 * t651 * t239 * t1099
      t1103 = t39 * t123
      t1107 = 0.24D2 * t357 * t211 * t1103 * t10
      t1111 = 0.8D1 * t357 * t211 * t212 * x3
      t1112 = t44 * t68
      t1113 = t1112 * t10
      t1116 = 0.14D2 * t509 * t433 * t1113
      t1117 = t122 * t49
      t1119 = t814 * t1117 * t34
      t1121 = 0.48D2 * t408 * t1119
      t1122 = t23 * t67
      t1126 = 0.64D2 * t619 * t525 * t1122 * t34
      t1127 = t1112 * x3
      t1129 = t509 * t433 * t1127
      t1130 = 0.104D3 * t1129
      t1131 = t484 * t123
      t1133 = t715 * t239 * t1131
      t1134 = 0.120D3 * t1133
      t1135 = t56 * t43
      t1137 = t122 * t108
      t1140 = t472 * t1135 * t1137 * t497 * x2
      t1141 = 0.72D2 * t1140
      t1147 = 0.384D3 * t57 * t618 * t44 * t465 * t466 * t10
      t1148 = z * t44
      t1151 = t333 * t230 * t1148 * t68
      t1153 = t30 * t766
      t1154 = t860 * t10
      t1155 = t465 * t1154
      t1157 = 0.48D2 * t1153 * t1155
      t1158 = t433 * x2
      t1161 = 0.48D2 * t238 * t1158 * t272
      t1164 = 0.64D2 * t698 * t230 * t1127
      t1167 = 0.64D2 * t509 * t240 * t922
      t1169 = t905 * t110 * t845
      t1171 = t1102 + t1107 + t1111 + t1116 + t1121 - t1126 + t1130 - t1
     #134 - t1141 + t1147 - 0.32D2 * t1151 + t1157 + t1161 + t1164 - t11
     #67 + 0.32D2 * t1169
      t1172 = t30 * t489
      t1173 = t65 * z
      t1174 = t1173 * t68
      t1177 = 0.32D2 * t1172 * t230 * t1174
      t1183 = t202 * t193 * t67 * x3 * t10
      t1185 = 0.12D2 * t57 * t281 * t65 * t1183
      t1187 = t599 * t254
      t1190 = t894 * t10
      t1193 = 0.48D2 * t30 * t190 * t1187 * t974 * t65 * t1190
      t1195 = t952 * t416 * t782
      t1197 = t385 * t837
      t1199 = 0.16D2 * t384 * t1197
      t1201 = t385 * t731 * t123
      t1203 = 0.14D2 * t985 * t1201
      t1204 = x2 * z
      t1208 = 0.384D3 * t238 * t240 * t1204 * x3
      t1212 = 0.384D3 * t560 * t627 * t1204 * t20
      t1216 = 0.384D3 * t262 * t264 * t1204 * t10
      t1217 = t619 * t733
      t1218 = 0.104D3 * t1217
      t1220 = 0.24D2 * t619 * t988
      t1222 = 0.48D2 * t177 * t586
      t1223 = t292 * t1
      t1226 = t202 ** 2
      t1229 = t283 * t1226 * t193 * t254
      t1231 = 0.2D1 * t30 * t40 * t1223 * t1229
      t1234 = t524 * t230 * t525 * t20
      t1236 = t85 * t440
      t1238 = t297 * t203 * t34
      t1239 = t1236 * t1238
      t1241 = t85 * t431
      t1243 = t230 * t434 * x3
      t1244 = t1241 * t1243
      t1246 = t85 * t176
      t1247 = t1246 * t427
      t1249 = -t1177 - t1185 - t1193 - 0.240D3 * t1195 + t1199 + t1203 -
     # t1208 - t1212 - t1216 + t1218 + t1220 + t1222 + t1231 + 0.488D3 *
     # t1234 + 0.67D2 * t1239 - 0.205D3 * t1244 + 0.48D2 * t1247
      t1251 = t1246 * t181
      t1253 = t698 * t233
      t1254 = 0.32D2 * t1253
      t1255 = t484 * x3
      t1258 = 0.16D2 * t483 * t263 * t1255
      t1259 = t1223 * t293
      t1261 = t108 * t307
      t1266 = 0.2D1 * t30 * t1259 * t1261 * t40 * t1226 * t45
      t1270 = 0.32D2 * t192 * t554 * t555 * t73
      t1273 = 0.384D3 * t952 * t1148 * t91
      t1276 = 0.384D3 * t952 * t385 * t515
      t1277 = t30 * t346
      t1279 = t211 * t223 * t123
      t1280 = t1277 * t1279
      t1283 = t340 * t999 * t68
      t1284 = t998 * t1283
      t1286 = t85 * t346
      t1287 = t1286 * t1279
      t1289 = t85 * t338
      t1290 = t1289 * t1283
      t1293 = t57 * t281 * t45
      t1294 = t202 * t142
      t1295 = t49 * t20
      t1299 = 0.16D2 * t1293 * t1294 * t1295 * x3
      t1302 = 0.48D2 * t876 * t938 * t242
      t1303 = t263 * x1
      t1306 = 0.64D2 * t704 * t1303 * t266
      t1310 = 0.48D2 * t715 * t239 * x1 * t266
      t1312 = 0.8D1 * t807 * t798
      t1314 = t40 * t45
      t1317 = t30 * t315 * t255 * t1314 * t51
      t1318 = 0.32D2 * t1317
      t1319 = -0.74D2 * t1251 - t1254 + t1258 + t1266 + t1270 + t1273 + 
     #t1276 + 0.74D2 * t1280 - 0.48D2 * t1284 + 0.144D3 * t1287 + 0.144D
     #3 * t1290 + t1299 - t1302 + t1306 - t1310 + t1312 + t1318
      t1323 = t30 * t413 * t211 * t223 * t158
      t1324 = 0.32D2 * t1323
      t1325 = t30 * t482
      t1327 = 0.12D2 * t1325 * t915
      t1328 = t30 * t209
      t1330 = 0.16D2 * t1328 * t919
      t1331 = t230 * t492
      t1332 = t1172 * t1331
      t1334 = t57 * t795
      t1338 = 0.8D1 * t1334 * t211 * t39 * x3
      t1339 = t65 * t50
      t1340 = t1339 * x3
      t1343 = 0.32D2 * t560 * t433 * t1340
      t1344 = t271 * t123
      t1347 = 0.32D2 * t238 * t239 * t1344
      t1349 = t620 * t621 * x3
      t1351 = 0.32D2 * t565 * t1349
      t1353 = t385 * t710 * t123
      t1355 = 0.64D2 * t565 * t1353
      t1356 = t57 * t396
      t1357 = t442 * t202
      t1361 = 0.16D2 * t1356 * t1357 * t491 * x3
      t1362 = t402 * t376
      t1363 = 0.5D1 * t1362
      t1365 = 0.176D3 * t796 * t808
      t1366 = t57 * t414
      t1367 = t416 * t128
      t1370 = 0.32D2 * t1366 * t239 * t1367
      t1372 = t636 * t270 * t277
      t1374 = t698 * t521
      t1378 = t900 * t391 * t686 * t20
      t1382 = 0.64D2 * t645 * t451 * t506
      t1383 = t1324 - t1327 + t1330 - 0.133D3 * t1332 + t1338 + t1343 + 
     #t1347 - t1351 + t1355 + t1361 - t1363 + t1365 - t1370 + 0.133D3 * 
     #t1372 - 0.208D3 * t1374 + 0.205D3 * t1378 + t1382
      t1390 = 0.32D2 * t657 * t658 * t458
      t1391 = t442 * x2
      t1393 = t441 * t1391 * t561
      t1394 = 0.104D3 * t1393
      t1395 = t1339 * t10
      t1398 = 0.32D2 * t560 * t433 * t1395
      t1402 = 0.384D3 * t229 * t270 * t1148 * t20
      t1403 = t57 * t482
      t1404 = t416 * t471
      t1407 = 0.384D3 * t1403 * t239 * t1404
      t1410 = 0.6D1 * t1334 * t211 * t594
      t1412 = t211 * t247 * x3
      t1414 = 0.24D2 * t908 * t1412
      t1415 = t263 * t825
      t1417 = 0.48D2 * t1325 * t1415
      t1418 = t814 * t68
      t1421 = 0.6D1 * t813 * t308 * t1418
      t1422 = t30 * t440
      t1423 = t230 * t65
      t1426 = 0.16D2 * t1422 * t1423 * t694
      t1429 = 0.8D1 * t441 * t1391 * t1340
      t1432 = 0.32D2 * t704 * t263 * t1131
      t1435 = 0.64D2 * t238 * t239 * t932
      t1436 = t1289 * t1001
      t1437 = 0.32D2 * t1436
      t1440 = 0.16D2 * t1422 * t1423 * t628
      t1442 = x1 * x3 * t10
      t1443 = t110 * t1442
      t1444 = t704 * t1443
      t1446 = t1390 + t1394 + t1398 + t1402 - t1407 + t1410 + t1414 - t1
     #417 + t1421 - t1426 + t1429 - t1432 - t1435 - t1437 - t1440 - 0.32
     #D2 * t1444
      t1448 = t85 * t462 * x2
      t1450 = t1295 * t34
      t1453 = 0.64D2 * t1448 * t65 * t23 * t1450
      t1454 = t44 * t122
      t1455 = t860 * x3
      t1458 = 0.48D2 * t1448 * t1454 * t1455
      t1460 = 0.16D2 * t441 * t648
      t1461 = t1303 * t242
      t1463 = 0.48D2 * t905 * t1461
      t1464 = t229 * t700
      t1465 = 0.104D3 * t1464
      t1466 = t442 * t577
      t1468 = 0.32D2 * t452 * t1466
      t1469 = t270 * t520
      t1470 = t636 * t1469
      t1472 = t297 * t202
      t1476 = 0.48D2 * t397 * t1472 * t491 * t10
      t1478 = 0.32D2 * t611 * t982
      t1479 = t65 * t34
      t1483 = 0.16D2 * t397 * t1472 * t1479 * t10
      t1485 = t1366 * t239 * t417
      t1488 = t986 * t20 * t68
      t1490 = 0.8D1 * t619 * t1488
      t1492 = t340 * t999 * t50
      t1493 = t998 * t1492
      t1496 = t211 * t223 * t128
      t1497 = t1277 * t1496
      t1499 = t211 * t909
      t1501 = 0.6D1 * t1328 * t1499
      t1504 = t876 * t540 * t860 * t128
      t1505 = 0.104D3 * t1504
      t1509 = 0.24D2 * t531 * t465 * t532 * t10
      t1510 = -t1453 + t1458 - t1460 - t1463 + t1465 + t1468 + 0.133D3 *
     # t1470 - t1476 - t1478 + t1483 + 0.240D3 * t1485 + t1490 + 0.74D2 
     #* t1493 - 0.48D2 * t1497 + t1501 + t1505 + t1509
      t1512 = t525 * t50
      t1513 = t297 * t1512
      t1514 = t325 * t1513
      t1516 = t263 * t1367
      t1517 = t415 * t1516
      t1521 = 0.24D2 * t432 * t1158 * t1344
      t1522 = t985 * t1349
      t1523 = 0.120D3 * t1522
      t1524 = t698 * t1020
      t1526 = t698 * t278
      t1530 = 0.48D2 * t1403 * t239 * t1255
      t1533 = t39 * x2
      t1535 = t894 * x3
      t1538 = 0.48D2 * t57 * t190 * t1187 * t1533 * t65 * t1535
      t1540 = t39 * t45
      t1542 = t255 * t1540 * t256
      t1543 = t449 * t303 * t1542
      t1548 = t449 * t439 * t340 * t341 * t202
      t1551 = t85 * t227 * t971
      t1556 = t384 * t752 * t122 * x3 * t10
      t1557 = 0.64D2 * t1556
      t1559 = t850 * t466 * t742
      t1562 = t1422 * t772 * t1395
      t1563 = 0.104D3 * t1562
      t1565 = t1081 * t270 * t932
      t1566 = 0.120D3 * t1565
      t1569 = 0.32D2 * t565 * t525 * t803
      t1571 = 0.24D2 * t1328 * t248
      t1572 = -0.128D3 * t1514 + 0.240D3 * t1517 + t1521 - t1523 - 0.208
     #D3 * t1524 - 0.32D2 * t1526 - t1530 - t1538 - 0.240D3 * t1543 + 0.
     #32D2 * t1548 - 0.1656D4 * t1551 - t1557 + 0.64D2 * t1559 + t1563 -
     # t1566 + t1569 + t1571
      t1574 = 0.16D2 * t483 * t1415
      t1576 = t490 * t433 * t1174
      t1579 = t229 * t264 * t511
      t1584 = 0.6D1 * t217 * t391 * t218 * t34
      t1586 = t85 * t489 * t1331
      t1588 = t271 * x3
      t1589 = t263 * t1588
      t1590 = t698 * t1589
      t1594 = t347 * t211 * t218 * t123
      t1597 = t340 * t341 * t68
      t1598 = t339 * t1597
      t1600 = t308 * x2
      t1603 = 0.14D2 * t1061 * t1600 * t319
      t1605 = t715 * t168 * t1442
      t1607 = t510 * t34
      t1609 = t490 * t627 * t1607
      t1611 = t1173 * t50
      t1614 = 0.32D2 * t490 * t433 * t1611
      t1615 = t814 * t50
      t1618 = 0.14D2 * t813 * t308 * t1615
      t1621 = 0.16D2 * t1422 * t1423 * t672
      t1622 = t57 * t481
      t1626 = 0.32D2 * t1622 * t391 * t1533 * x1
      t1627 = t565 * t712
      t1628 = 0.32D2 * t1627
      t1630 = 0.2D1 * t192 * t410
      t1631 = t1574 + 0.32D2 * t1576 - 0.64D2 * t1579 - t1584 - 0.32D2 *
     # t1586 - 0.32D2 * t1590 - 0.48D2 * t1594 + 0.74D2 * t1598 + t1603 
     #- 0.133D3 * t1605 - 0.64D2 * t1609 - t1614 + t1618 - t1621 - t1626
     # - t1628 + t1630
      t1635 = 0.64D2 * t1422 * t1238
      t1637 = 0.64D2 * t626 * t648
      t1639 = 0.48D2 * t457 * t668
      t1641 = 0.48D2 * t626 * t673
      t1645 = 0.64D2 * t676 * t948 * t64 * t1197
      t1647 = 0.16D2 * t441 * t629
      t1651 = t26 * t39
      t1654 = t589 * t1137 * t254 * t424 * t34 * t1651 * t20
      t1655 = 0.16D2 * t1654
      t1657 = t509 * t239 * t272
      t1660 = t509 * t239 * t277
      t1664 = 0.32D2 * t905 * t110 * t841
      t1667 = t391 * t40
      t1670 = 0.64D2 * t676 * t677 * t122 * t1667 * t825
      t1674 = 0.64D2 * t676 * t979 * t658 * t1255
      t1677 = 0.64D2 * t836 * t6 * t854
      t1680 = t1061 * t297 * t45 * t505
      t1681 = 0.80D2 * t1680
      t1684 = 0.768D3 * t698 * t263 * t1099
      t1685 = t142 * t67
      t1687 = t203 * t1685 * x3
      t1689 = 0.64D2 * t408 * t1687
      t1690 = -t1635 - t1637 + t1639 - t1641 - t1645 - t1647 + t1655 - 0
     #.133D3 * t1657 + 0.148D3 * t1660 - t1664 - t1670 - t1674 + t1677 +
     # t1681 + t1684 - t1689
      t1691 = t122 * t5
      t1695 = 0.48D2 * t619 * t178 * t1691 * x3
      t1698 = 0.14D2 * t1073 * t1074 * t1418
      t1701 = 0.38D2 * t1061 * t297 * t577
      t1705 = t85 * t439 * t340 * t999 * t202
      t1708 = 0.2D1 * t408 * t196
      t1709 = t441 * t695
      t1710 = 0.74D2 * t1709
      t1712 = t57 * t583 * t65
      t1713 = t1712 * t1155
      t1716 = 0.48D2 * t1712 * t769
      t1719 = 0.6D1 * t1073 * t1074 * t1615
      t1720 = t524 * t1513
      t1727 = 0.2D1 * t57 * t1259 * t1261 * t39 * t1226 * t45
      t1731 = 0.12D2 * t30 * t200 * t65 * t1183
      t1732 = t1246 * t804
      t1735 = t203 * t1685 * t10
      t1736 = t1090 * t1735
      t1739 = t814 * t1117 * t20
      t1741 = 0.48D2 * t192 * t1739
      t1743 = t30 * t191 * t44
      t1744 = x2 * t193
      t1747 = t1744 * t5 * t123 * t10
      t1749 = 0.14D2 * t1743 * t1747
      t1752 = t1744 * t5 * t128 * x3
      t1754 = 0.6D1 * t1743 * t1752
      t1755 = t1695 + t1698 + t1701 + 0.86D2 * t1705 + t1708 - t1710 - 0
     #.32D2 * t1713 + t1716 + t1719 - 0.74D2 * t1720 + t1727 - t1731 + 0
     #.48D2 * t1732 + 0.67D2 * t1736 + t1741 + t1749 - t1754
      t1760 = 0.384D3 * t565 * t434 * t585 * z
      t1762 = x2 * t23
      t1765 = t830 * t64 * t44 * t1762 * t5
      t1768 = 0.384D3 * t740 * t784
      t1770 = t1076 * t68
      t1772 = t57 * t316 * t442 * t1770
      t1773 = 0.32D2 * t1772
      t1775 = t565 * t434 * t585
      t1779 = 0.32D2 * t565 * t525 * t426
      t1782 = 0.32D2 * t30 * t251 * t259
      t1788 = 0.48D2 * t30 * t236 * t340 * t40 * t202 * t44
      t1792 = 0.48D2 * t1293 * t1294 * t1295 * t10
      t1795 = t868 * t465 * t869 * t10
      t1796 = 0.120D3 * t1795
      t1798 = t560 * t442 * t994
      t1801 = t636 * t270 * t1588
      t1805 = t991 * t263 * x2 * t1442
      t1810 = 0.48D2 * t1356 * t1357 * t1479 * x3
      t1814 = 0.48D2 * t985 * t178 * t1691 * t10
      t1815 = t57 * t304
      t1818 = 0.6D1 * t1815 * t1074 * t310
      t1822 = 0.48D2 * t57 * t618 * t65 * t861
      t1823 = t1760 + 0.1028D4 * t1765 - t1768 + t1773 + 0.32D2 * t1775 
     #+ t1779 - t1782 + t1788 - t1792 - t1796 - 0.128D3 * t1798 + 0.133D
     #3 * t1801 + 0.96D2 * t1805 - t1810 + t1814 + t1818 - t1822
      t1824 = t402 * t608
      t1826 = t881 * t642
      t1830 = 0.16D2 * t210 * t211 * t595
      t1833 = 0.12D2 * t1403 * t239 * t485
      t1835 = 0.64D2 * t704 * t1461
      t1837 = t30 * t200 * t45
      t1838 = t49 * t34
      t1842 = 0.48D2 * t1837 * t1294 * t1838 * x3
      t1843 = t424 * t51
      t1846 = 0.2D1 * t776 * t1074 * t1843
      t1848 = t776 * t1074 * t1062
      t1849 = 0.42D2 * t1848
      t1853 = 0.8D1 * t539 * t540 * t541 * t128
      t1856 = 0.32D2 * t384 * t1173 * t95
      t1858 = 0.48D2 * t781 * t1197
      t1859 = t385 * t854
      t1861 = 0.12D2 * t781 * t1859
      t1862 = t1090 * t1687
      t1865 = 0.32D2 * t565 * t988
      t1866 = t636 * t273
      t1868 = t472 * t451
      t1869 = t254 * t45
      t1871 = t1869 * t974 * t50
      t1872 = t1868 * t1871
      t1873 = 0.8D1 * t1872
      t1874 = t85 * t216
      t1878 = 0.768D3 * t1874 * t211 * t223 * z
      t1879 = 0.488D3 * t1824 + 0.488D3 * t1826 + t1830 - t1833 + t1835 
     #- t1842 + t1846 - t1849 + t1853 + t1856 - t1858 - t1861 + 0.67D2 *
     # t1862 - t1865 + 0.133D3 * t1866 - t1873 + t1878
      t1885 = t676 * t677 * t23 * t376
      t1886 = 0.32D2 * t1885
      t1890 = 0.64D2 * t985 * t525 * t1122 * t20
      t1894 = 0.2D1 * t57 * t39 * t1223 * t1229
      t1895 = t1090 * t1739
      t1899 = 0.6D1 * t1815 * t1074 * t546
      t1904 = 0.8D1 * t57 * t295 * t442 * t298 * x3
      t1905 = t276 * t123
      t1907 = t432 * t1158 * t1905
      t1908 = 0.120D3 * t1907
      t1910 = 0.32D2 * t949 * t70
      t1912 = t850 * t466 * t387
      t1914 = t945 * t134
      t1915 = 0.32D2 * t1914
      t1917 = t698 * t230 * t1113
      t1918 = 0.32D2 * t1917
      t1921 = 0.260D3 * t57 * t303 * t1542
      t1926 = 0.260D3 * t30 * t303 * t255 * t1314 * t256
      t1927 = t85 * t356
      t1929 = 0.32D2 * t1927 * t1412
      t1932 = 0.32D2 * t384 * t1173 * t69
      t1935 = 0.64D2 * t325 * t230 * t1395
      t1936 = t1886 - t1890 + t1894 - 0.205D3 * t1895 - t1899 + t1904 - 
     #t1908 + t1910 + 0.64D2 * t1912 + t1915 - t1918 + t1921 + t1926 - t
     #1929 + t1932 - t1935
      t1939 = 0.384D3 * t86 * t3 * t471
      t1941 = t449 * t227 * t1041
      t1944 = 0.38D2 * t246 * t1499
      t1947 = t715 * t168 * x1 * t928
      t1954 = 0.32D2 * t57 * t251 * t255 * t39 * t256 * t65
      t1960 = 0.48D2 * t57 * t236 * t340 * t39 * t202 * t44
      t1962 = t483 * t824 * t1255
      t1965 = t483 * t824 * t914
      t1971 = 0.64D2 * t85 * t209 * t108 * t110 * t928
      t1975 = 0.6D1 * t905 * t263 * t705 * t123
      t1979 = 0.176D3 * t1073 * t442 * t814 * t20
      t1982 = 0.6D1 * t1061 * t1600 * t1770
      t1985 = 0.8D1 * t1422 * t772 * t327
      t1988 = 0.38D2 * t1081 * t263 * t458
      t1989 = t1081 * t976
      t1990 = 0.5D1 * t1989
      t1993 = 0.32D2 * t560 * t442 * t1512
      t1994 = t1074 * x2
      t1997 = 0.14D2 * t776 * t1994 * t1770
      t1998 = t1939 + 0.128D3 * t1941 - t1944 - 0.64D2 * t1947 - t1954 +
     # t1960 + 0.64D2 * t1962 + 0.64D2 * t1965 + t1971 - t1975 - t1979 -
     # t1982 + t1985 + t1988 + t1990 - t1993 + t1997
      t2003 = 0.384D3 * t325 * t1423 * t671 * z
      t2005 = 0.6D1 * t1009 * t370
      t2009 = 0.2D1 * t57 * t39 * t1 * x1
      t2011 = t40 * t1 * x1
      t2013 = 0.260D3 * t85 * t2011
      t2015 = 0.2D1 * t30 * t2011
      t2016 = t29 * z
      t2018 = t2016 * t1038 * t971
      t2022 = 0.48D2 * t1448 * t1454 * t1154
      t2024 = t57 * t407 * t44
      t2026 = 0.14D2 * t2024 * t1752
      t2028 = 0.14D2 * t900 * t688
      t2030 = 0.48D2 * t740 * t388
      t2031 = t1246 * t185
      t2034 = 0.16D2 * t740 * t895
      t2035 = t1153 * t769
      t2038 = 0.64D2 * t626 * t695
      t2042 = 0.64D2 * t676 * t944 * t64 * t388
      t2047 = 0.32D2 * t57 * t1067 * t239 * t416 * t158
      t2051 = 0.32D2 * t246 * t211 * t1050 * t128
      t2052 = -t2003 - t2005 + t2009 - t2013 + t2015 - 0.1028D4 * t2018 
     #- t2022 + t2026 + t2028 - t2030 - 0.74D2 * t2031 + t2034 - 0.32D2 
     #* t2035 - t2038 - t2042 + t2047 + t2051
      t2056 = 0.32D2 * t246 * t211 * t1050 * t123
      t2057 = t860 * t34
      t2060 = 0.64D2 * t384 * t1173 * t2057
      t2062 = t1172 * t230 * t1611
      t2066 = 0.32D2 * t262 * t263 * t1082
      t2067 = t1090 * t1119
      t2072 = t1366 * t168 * x1 * t128 * x3
      t2073 = 0.32D2 * t2072
      t2076 = 0.64D2 * t325 * t1423 * t1190
      t2078 = t199 * t293 * t108
      t2083 = 0.24D2 * t57 * t2078 * t433 * t434 * t123
      t2084 = t991 * t1516
      t2089 = t85 * t176 * t45 * t1762 * t1450
      t2092 = t767 * t465 * t1455
      t2094 = t767 * t1155
      t2097 = 0.16D2 * t384 * t895
      t2098 = t384 * t1859
      t2099 = 0.80D2 * t2098
      t2102 = t384 * x1 * t122 * t831
      t2103 = 0.74D2 * t2102
      t2105 = t483 * t824 * t485
      t2109 = t333 * t230 * z * t334
      t2110 = 0.64D2 * t2109
      t2111 = t2056 + t2060 + 0.32D2 * t2062 + t2066 - 0.205D3 * t2067 +
     # t2073 - t2076 + t2083 + 0.48D2 * t2084 + 0.96D2 * t2089 + 0.133D3
     # * t2092 + 0.133D3 * t2094 + t2097 - t2099 + t2103 + 0.64D2 * t210
     #5 - t2110
      t2116 = 0.48D2 * t1448 * t1454 * t768
      t2119 = 0.48D2 * t30 * t858 * t864
      t2123 = 0.384D3 * t509 * t1158 * t276 * z
      t2126 = 0.32D2 * t830 * t59 * t69
      t2127 = t229 * t1589
      t2129 = t905 * t1443
      t2131 = t1289 * t1492
      t2133 = t1286 * t1496
      t2136 = t238 * t1158 * t520
      t2139 = t509 * t239 * t1588
      t2144 = 0.16D2 * t1837 * t1294 * t1838 * t10
      t2145 = t565 * t1201
      t2146 = 0.32D2 * t2145
      t2147 = t584 * t427
      t2149 = t584 * t181
      t2153 = 0.48D2 * t1448 * t1454 * t962
      t2155 = t1241 * t230 * t435
      t2157 = -t2116 - t2119 + t2123 + t2126 + 0.148D3 * t2127 - 0.133D3
     # * t2129 + 0.144D3 * t2131 + 0.144D3 * t2133 - 0.32D2 * t2136 - 0.
     #96D2 * t2139 + t2144 - t2146 + 0.240D3 * t2147 - 0.128D3 * t2149 +
     # t2153 - 0.205D3 * t2155
      t2159 = t1236 * t297 * t443
      t2161 = t776 * t1466
      t2162 = 0.5D1 * t2161
      t2163 = t1422 * t478
      t2164 = 0.74D2 * t2163
      t2166 = t1081 * t264 * t667
      t2167 = 0.80D2 * t2166
      t2170 = 0.64D2 * t229 * t264 * t1057
      t2172 = t1172 * t1423 * t1607
      t2176 = 0.64D2 * t262 * t263 * t1905
      t2180 = t415 * t110 * x1 * t123 * t10
      t2181 = 0.32D2 * t2180
      t2183 = 0.64D2 * t646 * t673
      t2187 = 0.176D3 * t1356 * t433 * t203 * x3
      t2188 = t1061 * t1871
      t2189 = 0.5D1 * t2188
      t2192 = t1289 * t255 * t999 * t73
      t2193 = 0.32D2 * t2192
      t2196 = 0.32D2 * t325 * t230 * t561
      t2198 = 0.48D2 * t1081 * t1243
      t2202 = 0.64D2 * t905 * t263 * t416 * x3
      t2203 = t985 * t1353
      t2204 = 0.104D3 * t2203
      t2208 = 0.6D1 * t985 * t620 * t726 * x3
      t2209 = 0.67D2 * t2159 + t2162 - t2164 + t2167 - t2170 - 0.64D2 * 
     #t2172 - t2176 + t2181 - t2183 - t2187 + t2189 - t2193 + t2196 + t2
     #198 - t2202 + t2204 - t2208
      t2212 = 0.24D2 * t985 * t1488
      t2215 = 0.64D2 * t560 * t627 * t1535
      t2216 = t1874 * t224
      t2220 = 0.24D2 * t210 * t211 * t1103
      t2223 = t1622 * t211 * t218 * x3
      t2227 = t601 * t526 * t1651 * x3
      t2228 = 0.16D2 * t2227
      t2233 = 0.24D2 * t30 * t2078 * t230 * t434 * t128
      t2235 = 0.6D1 * t2024 * t1747
      t2237 = t85 * t200 * t380
      t2239 = t177 * t804
      t2242 = 0.64D2 * t192 * t1735
      t2246 = 0.12D2 * t813 * t308 * t202 * t1077
      t2248 = 0.32D2 * t1927 * t911
      t2252 = 0.24D2 * t201 * t284 * t285 * t68
      t2256 = 0.8D1 * t367 * t1010 * t1011 * t34
      t2260 = t57 * t315 * t255 * t1540 * t73
      t2261 = 0.32D2 * t2260
      t2265 = t57 * t413 * t211 * t218 * t143
      t2266 = 0.32D2 * t2265
      t2267 = t2212 - t2215 + 0.86D2 * t2216 + t2220 + 0.205D3 * t2223 -
     # t2228 + t2233 - t2235 + 0.7D1 * t2237 + 0.240D3 * t2239 - t2242 -
     # t2246 - t2248 + t2252 + t2256 + t2261 + t2266
      t2270 = 0.128D3 * t84 * t1035 * t829
      t2273 = t1622 * t211 * t218 * t10
      t2276 = 0.32D2 * t565 * t1488
      t2278 = 0.48D2 * t262 * t1469
      t2281 = 0.6D1 * t776 * t1994 * t319
      t2285 = t1868 * t1869 * x2 * t68 * t56
      t2286 = 0.8D1 * t2285
      t2289 = 0.64D2 * t830 * t59 * t2057
      t2292 = 0.32D2 * t238 * t239 * t1905
      t2295 = 0.384D3 * t1325 * t263 * t1404
      t2299 = t473 * t475 * t34 * t56 * t10
      t2300 = 0.16D2 * t2299
      t2302 = 0.384D3 * t509 * t653
      t2305 = 0.384D3 * t229 * t263 * t652
      t2306 = t1135 * t122
      t2308 = t2016 * t2306 * t1041
      t2313 = 0.384D3 * t796 * t211 * t1050 * x3
      t2316 = t333 * t230 * t1148 * t50
      t2319 = t1061 * t308 * t1843
      t2320 = 0.42D2 * t2319
      t2324 = 0.64D2 * t1448 * x1 * t142 * t762
      t2325 = t2270 - 0.67D2 * t2273 - t2276 + t2278 - t2281 - t2286 - t
     #2289 + t2292 - t2295 + t2300 - t2302 - t2305 - 0.1028D4 * t2308 + 
     #t2313 - 0.32D2 * t2316 - t2320 - t2324
      t2333 = -t55 - t63 - t72 - t77 - t79 - t81 - t83 - t88 + t90 - t94
     # - t98 - t100 - t102 - t104 - t113 - t115 + t118
      t2335 = -t127 - t132 - t136 + t140 + t147 + t149 + t151 - t154 + t
     #157 + t162 + t164 + t166 - t171 + t174 + t183 + 0.80D2 * t186 + t1
     #98
      t2342 = -t208 - t215 + 0.52D2 * t220 + 0.52D2 * t225 - t235 + t245
     # + t250 - 0.52D2 * t260 + t269 + 0.128D3 * t274 + 0.240D3 * t279 -
     # t289 - t291 - t302 + t313 - t322 - t330
      t2346 = -t337 + 0.120D3 * t344 - 0.40D2 * t350 + t355 - t361 - t36
     #5 - t372 - t378 + t382 - t390 + t395 + t401 - 0.597D3 * t405 + t41
     #2 + t420 + t429 - t438
      t2356 = t446 - t456 - t461 - t470 - 0.32D2 * t479 + t488 + 0.30D2 
     #* t494 + 0.16D2 * t501 - t508 + 0.48D2 * t513 + 0.48D2 * t518 + 0.
     #30D2 * t522 - 0.597D3 * t528 + t536 - t545 - t549 - t553
      t2364 = -t559 + t564 - t568 - t571 - t580 + t582 - t588 - 0.32D2 *
     # t597 + 0.32D2 * t605 - 0.12D2 * t609 - 0.102D3 * t614 - 0.102D3 *
     # t616 + t625 + t631 - t635 - 0.597D3 * t639 - 0.12D2 * t643
      t2366 = t119 * t2306
      t2369 = 0.32D2 * t2366 * t1667 * t272
      t2370 = t650 - t655 - t662 - t666 - t670 + t675 - t682 - t690 - t6
     #93 + t697 - t702 + t709 - t714 + t719 + t2369 - t723 + t725
      t2373 = t40 * t39
      t2378 = 0.16D2 * t119 * t1037 * t142 * t211 * t2373 * x1 * t123
      t2384 = t730 - t735 - t738 + t745 + t2378 + t749 - t751 + 0.48D2 *
     # t754 + 0.48D2 * t757 + 0.80D2 * t764 - 0.30D2 * t770 + 0.80D2 * t
     #774 - t778 + t780 + t786 + t790 - t800
      t2393 = t802 + t806 - t810 + t818 - t823 - 0.48D2 * t827 + t834 + 
     #t840 - 0.48D2 * t843 + t848 - 0.48D2 * t852 - 0.80D2 * t856 - t863
     # - t866 - t873 - t880 - 0.597D3 * t882
      t2399 = -0.597D3 * t886 + t890 + 0.48D2 * t892 - t897 + t899 - 0.1
     #2D2 * t903 + t907 - t913 + t917 - t921 - 0.32D2 * t924 + 0.80D2 * 
     #t930 - t935 - t937 + t941 - t947 - t951
      t2411 = 0.32D2 * t119 * t1038 * t391 * t2373 * t1588
      t2414 = -t955 + t958 - t961 - 0.30D2 * t964 - t968 + 0.144D3 * t97
     #2 + 0.16D2 * t977 - 0.1508D4 * t983 - t990 - 0.120D3 * t992 + 0.40
     #D2 * t996 + t1003 - t1007 - t1015 - t2411 + 0.30D2 * t1017 + 0.30D
     #2 * t1021
      t2417 = -t1026 - t1030 + t1034 - t1043 - t1049 - t1054 - t1056 - 0
     #.32D2 * t1059 - t1065 - t1072 + t1080 - t1085 - t1087 + t1089 - 0.
     #80D2 * t1092 + t1095 - t1102
      t2423 = -t1107 - t1111 - t1116 - t1121 + t1126 - t1130 + t1134 + 0
     #.144D3 * t1140 - t1147 + 0.48D2 * t1151 - t1157 - t1161 - t1164 + 
     #t1167 - 0.48D2 * t1169 + t1177 + t1185
      t2430 = t1193 + 0.288D3 * t1195 - t1199 - t1203 + t1208 + t1212 + 
     #t1216 - t1218 - t1220 - t1222 - t1231 - 0.597D3 * t1234 + 0.12D2 *
     # t1239 + 0.102D3 * t1244 - 0.120D3 * t1247 + 0.40D2 * t1251 + t125
     #4
      t2436 = -t1258 - t1266 - t1270 - t1273 - t1276 - 0.40D2 * t1280 + 
     #0.120D3 * t1284 - 0.192D3 * t1287 - 0.192D3 * t1290 - t1299 + t130
     #2 - t1306 + t1310 - t1312 - t1318 - t1324 + t1327
      t2440 = 0.32D2 * t2366 * t1667 * t520
      t2444 = -t1330 + 0.30D2 * t1332 - t1338 - t1343 - t1347 + t1351 - 
     #t1355 - t1361 + t2440 + t1363 - t1365 + t1370 - 0.30D2 * t1372 + 0
     #.112D3 * t1374 - 0.102D3 * t1378 - t1382 - t1390
      t2450 = t56 * t26 * t43
      t2455 = 0.32D2 * t119 * t2450 * t122 * t658 * t277
      t2456 = 0.64D2 * t1444
      t2457 = -t1394 - t1398 - t1402 + t1407 - t1410 - t1414 + t1417 - t
     #1421 + t1426 - t1429 + t1432 + t1435 + t1437 + t1440 - t2455 - t24
     #56 + t1453
      t2463 = -t1458 + t1460 + t1463 - t1465 - t1468 - 0.30D2 * t1470 + 
     #t1476 + t1478 - t1483 - 0.288D3 * t1485 - t1490 - 0.40D2 * t1493 +
     # 0.120D3 * t1497 - t1501 - t1505 - t1509 + 0.80D2 * t1514
      t2469 = 0.16D2 * t119 * t2450 * t23 * t1597
      t2471 = 0.64D2 * t1526
      t2477 = -0.288D3 * t1517 - t1521 + t1523 + t2469 + 0.112D3 * t1524
     # - t2471 + t1530 + t1538 + 0.288D3 * t1543 - 0.128D3 * t1548 + 0.9
     #84D3 * t1551 + 0.32D2 * t1556 - 0.80D2 * t1559 - t1563 + t1566 - t
     #1569 - t1571
      t2486 = 0.16D2 * t119 * t1037 * t23 * t340 * t2373 * t65 * t50
      t2487 = 0.64D2 * t1586
      t2488 = 0.64D2 * t1590
      t2493 = -t1574 - 0.48D2 * t1576 + 0.48D2 * t1579 + t1584 + t2486 -
     # t2487 - t2488 + 0.120D3 * t1594 - 0.40D2 * t1598 - t1603 + 0.30D2
     # * t1605 + 0.80D2 * t1609 + t1614 - t1618 + t1621 + t1626 + t1628
      t2499 = -t1630 + t1635 + t1637 - t1639 + t1641 + t1645 + t1647 - 0
     #.32D2 * t1654 + 0.30D2 * t1657 - 0.80D2 * t1660 + t1664 + t1670 + 
     #t1674 - t1677 - t1681 - t1684 + t1689
      t2505 = -t1695 - t1698 - t1701 - 0.176D3 * t1705 - t1708 + t1710 +
     # 0.128D3 * t1713 - t1716 - t1719 + 0.40D2 * t1720 - t1727 + t1731 
     #- 0.120D3 * t1732 + 0.12D2 * t1736 - t1741 - t1749 + t1754
      t2513 = 0.32D2 * t119 * t1135 * t23 * t340 * t40 * t492
      t2518 = -t1760 - 0.1508D4 * t1765 + t1768 - t2513 - t1773 - 0.128D
     #3 * t1775 - t1779 + t1782 - t1788 + t1792 + t1796 + 0.80D2 * t1798
     # - 0.30D2 * t1801 - 0.240D3 * t1805 + t1810 - t1814 - t1818
      t2524 = t1822 - 0.597D3 * t1824 - 0.597D3 * t1826 - t1830 + t1833 
     #- t1835 + t1842 - t1846 + t1849 - t1853 - t1856 + t1858 + t1861 + 
     #0.12D2 * t1862 + t1865 - 0.30D2 * t1866 + 0.16D2 * t1872
      t2532 = 0.16D2 * t119 * t2450 * t142 * t349
      t2534 = -t1878 - t1886 + t1890 - t1894 + 0.102D3 * t1895 + t2532 +
     # t1899 - t1904 + t1908 - t1910 - 0.48D2 * t1912 - t1915 + t1918 - 
     #t1921 - t1926 + t1929 - t1932
      t2539 = t1935 - t1939 - 0.80D2 * t1941 + t1944 + 0.80D2 * t1947 + 
     #t1954 - t1960 - 0.48D2 * t1962 - 0.80D2 * t1965 - t1971 + t1975 + 
     #t1979 + t1982 - t1985 - t1988 - t1990 + t1993
      t2544 = -t1997 + t2003 + t2005 - t2009 + t2013 - t2015 + 0.1508D4 
     #* t2018 + t2022 - t2026 - t2028 + t2030 + 0.40D2 * t2031 - t2034 +
     # 0.128D3 * t2035 + t2038 + t2042 - t2047
      t2552 = -t2051 - t2056 - t2060 - 0.48D2 * t2062 - t2066 + 0.102D3 
     #* t2067 - t2073 + t2076 - t2083 - 0.120D3 * t2084 - 0.240D3 * t208
     #9 - 0.30D2 * t2092 - 0.30D2 * t2094 - t2097 + t2099 - t2103 - 0.80
     #D2 * t2105
      t2565 = 0.32D2 * t2109 + t2116 + t2119 - t2123 - t2126 - 0.80D2 * 
     #t2127 + 0.30D2 * t2129 - 0.192D3 * t2131 - 0.192D3 * t2133 + 0.128
     #D3 * t2136 + 0.240D3 * t2139 - t2144 + t2146 - 0.288D3 * t2147 + 0
     #.80D2 * t2149 - t2153 + 0.102D3 * t2155
      t2568 = 0.12D2 * t2159 - t2162 + t2164 - t2167 + t2170 + 0.80D2 * 
     #t2172 + t2176 - t2181 + t2183 + t2187 - t2189 + t2193 - t2196 - t2
     #198 + t2202 - t2204 + t2208
      t2575 = -t2212 + t2215 - 0.176D3 * t2216 - t2220 - 0.102D3 * t2223
     # + 0.32D2 * t2227 - t2233 + t2235 - 0.52D2 * t2237 - 0.288D3 * t22
     #39 + t2242 + t2246 + t2248 - t2252 - t2256 - t2261 - t2266
      t2585 = 0.32D2 * t119 * t1135 * t142 * t211 * t40 * t1442
      t2587 = -t2270 - 0.12D2 * t2273 + t2276 - t2278 + t2281 + 0.16D2 *
     # t2285 + t2289 - t2292 + t2295 - 0.32D2 * t2299 + t2302 + t2305 + 
     #0.1508D4 * t2308 - t2585 - t2313 + 0.48D2 * t2316 + t2320 + t2324
      t2608 = -0.32D2 * t146 - 0.64D2 * t148 + 0.32D2 * t150 - 0.32D2 * 
     #t161 - 0.64D2 * t163 + 0.32D2 * t165 + 0.176D3 * t186 - 0.32D2 * t
     #197 - 0.80D2 * t220 - 0.80D2 * t225 + 0.80D2 * t260 - 0.352D3 * t2
     #74 + 0.64D2 * t321
      t2619 = 0.64D2 * t336 + 0.64D2 * t350 - 0.32D2 * t354 + t390 + 0.4
     #8D2 * t405 - 0.32D2 * t411 + t480 - 0.16D2 * t487 - 0.64D2 * t494 
     #- t502 + 0.16D2 * t513 + 0.16D2 * t518 - 0.32D2 * t522
      t2631 = 0.48D2 * t528 - 0.32D2 * t535 + 0.64D2 * t544 + t598 - t60
     #6 - 0.16D2 * t609 + 0.48D2 * t614 + 0.48D2 * t616 - 0.32D2 * t634 
     #+ 0.48D2 * t639 - 0.16D2 * t643 + 0.16D2 * t669 - t675
      t2644 = 0.64D2 * t689 - t2369 - t2378 - 0.96D2 * t754 - 0.96D2 * t
     #757 - 0.256D3 * t764 + 0.32D2 * t770 - 0.256D3 * t774 - 0.16D2 * t
     #779 + 0.32D2 * t789 - 0.16D2 * t827 + 0.96D2 * t843 - 0.16D2 * t85
     #2 + 0.16D2 * t856
      t2657 = 0.48D2 * t882 + 0.48D2 * t886 + 0.16D2 * t892 - 0.32D2 * t
     #898 - 0.16D2 * t903 - 0.32D2 * t906 - 0.16D2 * t916 + t925 - 0.16D
     #2 * t930 + 0.64D2 * t950 + 0.64D2 * t964 - t973 - t978
      t2668 = 0.288D3 * t983 - 0.64D2 * t996 - 0.32D2 * t1002 + t2411 - 
     #0.32D2 * t1017 - 0.32D2 * t1021 + t1060 + 0.16D2 * t1086 - 0.176D3
     # * t1092 + 0.64D2 * t1129 - 0.32D2 * t1133 - t1141 - 0.96D2 * t115
     #1
      t2681 = 0.96D2 * t1169 - 0.176D3 * t1195 + t1199 + 0.64D2 * t1217 
     #+ 0.48D2 * t1234 + 0.16D2 * t1239 - 0.48D2 * t1244 - 0.64D2 * t125
     #1 - 0.64D2 * t1253 + t1258 + 0.64D2 * t1280 + 0.32D2 * t1287 + 0.3
     #2D2 * t1290
      t2692 = 0.64D2 * t1317 + 0.64D2 * t1323 - 0.64D2 * t1332 - t2440 +
     # 0.32D2 * t1362 + 0.64D2 * t1372 - 0.192D3 * t1374 + 0.48D2 * t137
     #8 + 0.64D2 * t1393 - 0.64D2 * t1436 - t1440 + t2455 - t2456 + 0.64
     #D2 * t1464
      t2707 = 0.32D2 * t1470 + 0.176D3 * t1485 + 0.64D2 * t1493 + 0.64D2
     # * t1504 + 0.176D3 * t1514 + 0.176D3 * t1517 - 0.32D2 * t1522 - t2
     #469 - 0.192D3 * t1524 - t2471 - 0.176D3 * t1543 + 0.352D3 * t1548 
     #+ 0.864D3 * t1551
      t2716 = -t1557 + 0.16D2 * t1559 + 0.64D2 * t1562 - 0.32D2 * t1565 
     #+ t1574 + 0.96D2 * t1576 + 0.16D2 * t1579 - t2486 - t2487 - t2488 
     #+ 0.64D2 * t1598 - 0.64D2 * t1605 - 0.16D2 * t1609
      t2728 = -t1621 - 0.64D2 * t1627 - t1647 + t1655 - 0.32D2 * t1657 +
     # 0.256D3 * t1660 + 0.16D2 * t1680 + 0.160D3 * t1705 - 0.64D2 * t17
     #09 - 0.352D3 * t1713 - 0.64D2 * t1720 + 0.16D2 * t1736 + 0.288D3 *
     # t1765
      t2741 = t2513 + 0.64D2 * t1772 + 0.352D3 * t1775 - 0.32D2 * t1795 
     #+ 0.176D3 * t1798 + 0.64D2 * t1801 + 0.48D2 * t1824 + 0.48D2 * t18
     #26 - 0.32D2 * t1848 + 0.16D2 * t1862 + 0.32D2 * t1866 - t1873 + 0.
     #64D2 * t1885 - 0.48D2 * t1895
      t2756 = -t2532 - 0.32D2 * t1907 - 0.16D2 * t1912 + 0.64D2 * t1914 
     #- 0.64D2 * t1917 - 0.176D3 * t1941 - 0.16D2 * t1947 - 0.16D2 * t19
     #62 + 0.16D2 * t1965 - 0.32D2 * t1989 - 0.288D3 * t2018 - 0.64D2 * 
     #t2031 - 0.352D3 * t2035
      t2770 = 0.96D2 * t2062 - 0.48D2 * t2067 + 0.64D2 * t2072 + 0.64D2 
     #* t2092 + 0.32D2 * t2094 - 0.16D2 * t2098 + 0.64D2 * t2102 + 0.16D
     #2 * t2105 - t2110 + 0.256D3 * t2127 - 0.64D2 * t2129 + 0.32D2 * t2
     #131 + 0.32D2 * t2133 - 0.352D3 * t2136
      t2785 = -0.64D2 * t2145 + 0.176D3 * t2147 + 0.176D3 * t2149 - 0.48
     #D2 * t2155 + 0.16D2 * t2159 - 0.32D2 * t2161 - 0.64D2 * t2163 + 0.
     #16D2 * t2166 - 0.16D2 * t2172 + 0.64D2 * t2180 - 0.32D2 * t2188 - 
     #0.64D2 * t2192 + 0.64D2 * t2203
      t2796 = 0.160D3 * t2216 + 0.48D2 * t2223 - t2228 + 0.80D2 * t2237 
     #+ 0.176D3 * t2239 + 0.64D2 * t2260 + 0.64D2 * t2265 - 0.16D2 * t22
     #73 - t2286 + t2300 - 0.288D3 * t2308 + t2585 - 0.96D2 * t2316 - 0.
     #32D2 * t2319
      rrgg2ggh11J3 = -0.9D1 / 0.16D2 * (0.3D1 * wd * (t942 + t421 + t131
     #9 + t1823 + t1510 + t1998 + t2052 + t632 + t188 + t1019 + t116 + t
     #1446 + t1572 + t2325 + t2209 + t1879 + t314 + t874 + t1755 + t2157
     # + t537 + t1249 + t1936 + t791 + t2267 + t1383 + t1096 + t1171 + t
     #2111 + t1631 + t1690 + t720) + 0.2D1 * wd * (t2552 + t2444 + t2384
     # + t2393 + t2436 + t2493 + t2463 + t2423 + t2430 + t2414 + t2399 +
     # t2518 + t2524 + t2575 + t2370 + t2356 + t2565 + t2568 + t2457 + t
     #2544 + t2534 + t2539 + t2417 + t2499 + t2505 + t2587 + t2477 + t23
     #42 + t2346 + t2333 + t2335 + t2364) + wd * (t2608 + t2619 + t2631 
     #+ t2644 + t2657 + t2668 + t2681 + t2692 + t2707 + t2716 + t2728 + 
     #t2741 + t2756 + t2770 + t2785 + t2796)) / t56 / t447 / t40 / z / 0
     #.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh11J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t6 * t20 - t2 * t24
      t27 = t26 ** 2
      t28 = s ** 2
      t29 = t28 ** 2
      t30 = t29 * t28
      t31 = t27 * t30
      t34 = x2 * x3
      t35 = t10 * t7 * t4 + t34 + t19
      t38 = t23 * t10
      t40 = s - t2 * t6 * t35 - t2 * t38
      t41 = t1 ** 2
      t42 = t41 * t1
      t43 = t41 ** 2
      t44 = t43 * t42
      t45 = t40 * t44
      t46 = t31 * t45
      t47 = x1 ** 2
      t48 = t47 ** 2
      t49 = t48 * x1
      t50 = x2 ** 2
      t51 = t49 * t50
      t52 = t23 ** 2
      t53 = t4 ** 2
      t54 = t53 ** 2
      t55 = 0.1D1 / t54
      t56 = t52 * t55
      t57 = t20 ** 2
      t61 = 0.24D2 * t46 * t51 * t56 * t57
      t62 = t52 ** 2
      t63 = t43 * t62
      t64 = t31 * t63
      t67 = z + x1 * t7 * t1
      t68 = t67 * t5
      t69 = t10 ** 2
      t70 = t40 * t69
      t74 = 0.8D1 * t64 * t68 * t70 * x3
      t75 = t26 * t30
      t76 = t42 * t23
      t77 = t75 * t76
      t78 = 0.1D1 / t53
      t79 = t67 * t78
      t80 = t40 ** 2
      t81 = t80 * t47
      t83 = t79 * t81 * t20
      t84 = t77 * t83
      t86 = t43 * t41
      t87 = t40 * t86
      t88 = t31 * t87
      t89 = t47 * x1
      t90 = t89 * t50
      t91 = t52 * t23
      t92 = t91 * t78
      t94 = t90 * t92 * x3
      t96 = 0.64D2 * t88 * t94
      t97 = t40 * t43
      t98 = t31 * t97
      t99 = t47 * x2
      t100 = t52 * t5
      t104 = 0.48D2 * t98 * t99 * t100 * x3
      t105 = t29 * s
      t106 = t27 * t105
      t107 = t43 * t91
      t108 = t107 * t67
      t109 = t106 * t108
      t110 = t78 * t80
      t111 = x1 * t20
      t113 = t110 * t111 * t69
      t115 = 0.32D2 * t109 * t113
      t116 = t80 * t44
      t117 = t75 * t116
      t118 = t62 * t78
      t119 = x3 ** 2
      t121 = t90 * t118 * t119
      t123 = 0.14D2 * t117 * t121
      t124 = t75 * t80
      t125 = t41 * t47
      t127 = t125 * t5 * t35
      t129 = 0.8D1 * t124 * t127
      t130 = t42 * t89
      t131 = t35 ** 2
      t132 = t78 * t131
      t133 = t130 * t132
      t135 = 0.24D2 * t124 * t133
      t136 = t43 * t48
      t138 = 0.1D1 / t53 / t4
      t139 = t131 * t35
      t141 = t136 * t138 * t139
      t143 = 0.32D2 * t124 * t141
      t144 = t106 * t80
      t146 = 0.176D3 * t144 * t127
      t148 = 0.38D2 * t144 * t133
      t149 = t31 * t40
      t150 = t5 * t20
      t151 = t125 * t150
      t153 = 0.8D1 * t149 * t151
      t154 = t78 * t57
      t155 = t130 * t154
      t157 = 0.24D2 * t149 * t155
      t158 = t57 * t20
      t160 = t136 * t138 * t158
      t162 = 0.32D2 * t149 * t160
      t164 = 0.6D1 * t124 * t151
      t165 = t61 + t74 - 0.67D2 * t84 - t96 + t104 - t115 + t123 + t129 
     #+ t135 + t143 + t146 - t148 + t153 + t157 + t162 + t164
      t167 = 0.6D1 * t124 * t155
      t169 = 0.2D1 * t124 * t160
      t171 = 0.2D1 * t149 * t141
      t172 = t43 * t1
      t173 = t80 * t172
      t176 = x2 * t23
      t177 = t138 * t20
      t178 = t177 * t35
      t180 = t106 * t173 * t48 * t176 * t178
      t182 = t80 * t42
      t183 = t106 * t182
      t184 = x1 * z
      t185 = t52 * t119
      t187 = t183 * t184 * t185
      t189 = t89 * z
      t192 = 0.32D2 * t183 * t189 * t132
      t193 = t67 * t138
      t195 = t193 * t81 * t57
      t197 = 0.14D2 * t77 * t195
      t198 = t41 * t23
      t199 = t75 * t198
      t200 = t80 * x1
      t204 = 0.6D1 * t199 * t79 * t200 * t20
      t205 = t40 * t172
      t206 = t31 * t205
      t207 = t91 * t5
      t208 = t207 * t69
      t209 = t99 * t208
      t210 = t206 * t209
      t212 = t42 * t52
      t213 = t75 * t212
      t214 = t80 * x2
      t216 = t79 * t214 * x1
      t218 = 0.32D2 * t213 * t216
      t219 = t28 * s
      t220 = t29 * t219
      t221 = t26 * t220
      t223 = t86 * t52 * t67
      t224 = t221 * t223
      t225 = t55 * t40
      t226 = t48 * x2
      t227 = t226 * t57
      t228 = t225 * t227
      t230 = 0.32D2 * t224 * t228
      t232 = t86 * t62 * t67
      t233 = t221 * t232
      t234 = t78 * t40
      t235 = t99 * t69
      t236 = t234 * t235
      t238 = 0.32D2 * t233 * t236
      t239 = t172 * t91
      t245 = 0.48D2 * t31 * t239 * t193 * t40 * t50 * t47
      t246 = t43 * t52
      t247 = t246 * t67
      t248 = t31 * t247
      t249 = t234 * t47
      t250 = z * t35
      t251 = t250 * x3
      t253 = t248 * t249 * t251
      t255 = z * t20
      t256 = t255 * x3
      t259 = 0.64D2 * t248 * t249 * t256
      t260 = t31 * t232
      t261 = t34 * t10
      t262 = t249 * t261
      t263 = t260 * t262
      t264 = 0.80D2 * t263
      t265 = t43 * t23
      t266 = t31 * t265
      t267 = t40 * t89
      t269 = t193 * t267 * t131
      t270 = t266 * t269
      t272 = t167 + t169 + t171 + 0.96D2 * t180 - 0.32D2 * t187 + t192 +
     # t197 - t204 - 0.128D3 * t210 - t218 + t230 + t238 + t245 - 0.64D2
     # * t253 - t259 + t264 + 0.74D2 * t270
      t274 = t42 * t91
      t275 = t31 * t274
      t276 = t40 * t119
      t279 = 0.24D2 * t275 * t68 * t276
      t280 = t68 * t200
      t281 = t199 * t280
      t283 = t89 * t78
      t284 = t57 * t23
      t286 = t283 * t284 * t10
      t287 = t98 * t286
      t288 = 0.120D3 * t287
      t290 = t44 * t91 * t67
      t291 = t31 * t290
      t293 = 0.1D1 / t54 / t4
      t294 = t293 * t40
      t295 = t50 * t48
      t296 = t295 * t57
      t299 = 0.6D1 * t291 * t294 * t296
      t300 = t138 * t40
      t301 = t300 * x2
      t302 = t47 * t35
      t303 = t302 * t119
      t305 = t260 * t301 * t303
      t306 = 0.120D3 * t305
      t308 = 0.6D1 * t149 * t133
      t309 = t173 * t89
      t310 = t106 * t309
      t311 = x2 * t52
      t312 = t78 * t35
      t313 = t312 * t10
      t315 = t310 * t311 * t313
      t317 = t62 * t5
      t318 = t119 * x3
      t320 = t99 * t317 * t318
      t322 = 0.2D1 * t88 * t320
      t323 = t69 * t10
      t325 = t99 * t317 * t323
      t326 = t88 * t325
      t327 = 0.42D2 * t326
      t328 = t49 * x2
      t329 = t23 * t55
      t333 = 0.32D2 * t88 * t328 * t329 * t158
      t334 = t172 * t62
      t335 = t334 * t67
      t336 = t106 * t335
      t337 = x1 * x2
      t338 = t337 * t69
      t339 = t110 * t338
      t340 = t336 * t339
      t342 = t75 * t232
      t343 = t110 * t47
      t345 = t342 * t343 * t261
      t346 = 0.80D2 * t345
      t347 = t86 * t91
      t348 = t347 * t67
      t349 = t31 * t348
      t350 = t225 * x2
      t351 = t89 * t131
      t352 = t351 * x3
      t354 = t349 * t350 * t352
      t355 = 0.104D3 * t354
      t361 = t50 * t62 * t78 * x3 * t10
      t363 = 0.12D2 * t75 * t116 * t89 * t361
      t365 = t75 * t116 * t48
      t366 = t50 * t91
      t367 = t138 * t35
      t371 = 0.48D2 * t365 * t366 * t367 * x3
      t375 = 0.16D2 * t365 * t366 * t367 * t10
      t376 = t279 - 0.7D1 * t281 - t288 + t299 - t306 + t308 + 0.133D3 *
     # t315 + t322 - t327 + t333 + 0.48D2 * t340 + t346 + t355 - t363 - 
     #t371 + t375
      t377 = t212 * t67
      t378 = t106 * t377
      t379 = t110 * z
      t380 = x1 * t35
      t381 = t380 * x3
      t383 = t378 * t379 * t381
      t385 = t106 * t247
      t386 = t138 * t80
      t387 = t47 * t131
      t388 = t387 * x3
      t391 = 0.64D2 * t385 * t386 * t388
      t392 = t42 * x1
      t393 = t52 * t69
      t394 = t392 * t393
      t395 = t144 * t394
      t396 = 0.5D1 * t395
      t397 = t392 * t185
      t399 = 0.14D2 * t124 * t397
      t400 = t41 * x1
      t403 = 0.6D1 * t124 * t400 * t24
      t404 = t43 * x1
      t406 = t404 * t91 * t318
      t407 = t124 * t406
      t408 = 0.42D2 * t407
      t409 = t144 * t406
      t410 = 0.32D2 * t409
      t411 = t144 * t397
      t412 = 0.5D1 * t411
      t414 = t23 * t67
      t415 = t5 * t40
      t418 = 0.2D1 * t31 * t1 * t414 * t415
      t420 = t5 * t80
      t421 = t414 * t420
      t423 = 0.260D3 * t106 * t1 * t421
      t426 = 0.2D1 * t75 * t1 * t421
      t428 = 0.6D1 * t149 * t127
      t429 = t44 * t62
      t431 = t67 * t55
      t432 = t50 * x2
      t435 = t431 * t80 * t432 * t89
      t436 = t106 * t429 * t435
      t440 = 0.6D1 * t149 * t400 * t38
      t442 = t404 * t91 * t323
      t443 = t149 * t442
      t444 = 0.42D2 * t443
      t445 = t144 * t442
      t446 = 0.32D2 * t445
      t448 = 0.176D3 * t144 * t151
      t449 = 0.64D2 * t383 + t391 - t396 + t399 - t403 - t408 - t410 - t
     #412 + t418 - t423 + t426 + t428 + 0.7D1 * t436 - t440 - t444 - t44
     #6 + t448
      t453 = 0.38D2 * t144 * t155
      t454 = t105 * z
      t455 = t454 * t27
      t456 = t80 * t41
      t459 = 0.32D2 * t455 * t456 * t185
      t462 = 0.32D2 * t455 * t456 * t393
      t464 = 0.14D2 * t149 * t394
      t466 = t68 * t200 * x3
      t467 = t213 * t466
      t469 = t75 * t108
      t470 = t110 * x1
      t472 = t20 * t10 * x3
      t473 = t470 * t472
      t475 = 0.48D2 * t469 * t473
      t476 = t75 * t290
      t477 = t55 * t80
      t481 = 0.176D3 * t476 * t477 * t295 * t35
      t482 = t106 * t76
      t483 = t482 * t83
      t485 = t75 * t335
      t489 = t485 * t420 * x1 * t119 * t10
      t490 = 0.32D2 * t489
      t493 = x1 * x3 * t10
      t495 = t336 * t110 * x2 * t493
      t497 = t47 * t5
      t498 = t35 * t23
      t499 = t498 * t10
      t500 = t497 * t499
      t501 = t183 * t500
      t502 = 0.80D2 * t501
      t503 = t31 * t223
      t504 = t294 * x2
      t505 = t48 * t20
      t506 = t505 * t131
      t509 = 0.14D2 * t503 * t504 * t506
      t511 = t90 * t118 * t69
      t513 = 0.14D2 * t46 * t511
      t514 = t265 * t67
      t515 = t75 * t514
      t516 = t189 * t57
      t518 = t515 * t386 * t516
      t520 = t106 * t173
      t521 = t23 * t138
      t522 = t521 * t131
      t523 = t226 * t522
      t524 = t520 * t523
      t526 = t80 * t86
      t527 = t106 * t526
      t529 = t90 * t92 * t10
      t530 = t527 * t529
      t532 = -t453 + t459 + t462 + t464 - 0.67D2 * t467 - t475 - t481 + 
     #0.488D3 * t483 + t490 + 0.96D2 * t495 - t502 + t509 + t513 + 0.32D
     #2 * t518 + 0.48D2 * t524 + 0.67D2 * t530
      t533 = t75 * t173
      t534 = t521 * t57
      t535 = t226 * t534
      t537 = 0.32D2 * t533 * t535
      t538 = t172 * t23
      t540 = t80 * t48
      t543 = t75 * t538 * t431 * t540 * t158
      t544 = 0.32D2 * t543
      t546 = t505 * t35
      t549 = 0.12D2 * t291 * t294 * t50 * t546
      t550 = t31 * t108
      t552 = t550 * t415 * t493
      t554 = t75 * t526
      t556 = 0.2D1 * t554 * t325
      t557 = t386 * t89
      t558 = t255 * t35
      t560 = t515 * t557 * t558
      t562 = t80 * t43
      t563 = t106 * t562
      t566 = 0.32D2 * t563 * t337 * t208
      t567 = t239 * t67
      t568 = t106 * t567
      t569 = t386 * x2
      t570 = t302 * x3
      t571 = t569 * t570
      t572 = t568 * t571
      t574 = t312 * x3
      t575 = t311 * t574
      t576 = t310 * t575
      t578 = t420 * t493
      t579 = t109 * t578
      t581 = z ** 2
      t582 = t581 * z
      t584 = t27 * t80
      t586 = 0.128D3 * t105 * t582 * t584
      t590 = 0.12D2 * t31 * t45 * t89 * t361
      t592 = t91 * t67
      t593 = t592 * t55
      t596 = t20 * t35
      t597 = t596 * t10
      t600 = 0.48D2 * t75 * t86 * t593 * t214 * t89 * t597
      t601 = t106 * t456
      t602 = t23 * t5
      t604 = t601 * t337 * t602
      t608 = z * x3 * t10
      t610 = t550 * t415 * x1 * t608
      t612 = t250 * t10
      t614 = t248 * t249 * t612
      t615 = 0.64D2 * t614
      t616 = t31 * t514
      t617 = t300 * t89
      t619 = t616 * t617 * t558
      t621 = -t537 + t544 - t549 - 0.133D3 * t552 + t556 - 0.64D2 * t560
     # + t566 + 0.133D3 * t572 + 0.133D3 * t576 - 0.32D2 * t579 + t586 -
     # t590 - t600 - 0.240D3 * t604 - 0.64D2 * t610 + t615 - 0.64D2 * t6
     #19
      t623 = t47 * t20
      t624 = t623 * t10
      t626 = t248 * t234 * t624
      t628 = t220 * t581
      t629 = t628 * t223
      t630 = t55 * t48
      t632 = t630 * t214 * t57
      t633 = t629 * t632
      t634 = 0.8D1 * t633
      t635 = t429 * t67
      t636 = t75 * t635
      t637 = t477 * t50
      t638 = t89 * t20
      t642 = 0.48D2 * t636 * t637 * t638 * t10
      t643 = t75 * t567
      t645 = t35 * x3 * t10
      t648 = 0.64D2 * t643 * t343 * t645
      t649 = t31 * t567
      t652 = 0.64D2 * t649 * t249 * t472
      t654 = t172 * t52 * t67
      t655 = t106 * t654
      t656 = x2 * t89
      t657 = t656 * t131
      t658 = t477 * t657
      t659 = t655 * t658
      t661 = t78 * t20
      t662 = t661 * x3
      t664 = t310 * t311 * t662
      t666 = t569 * t624
      t667 = t568 * t666
      t669 = t106 * t212
      t670 = t669 * t466
      t672 = t31 * t198
      t673 = t40 * x1
      t675 = t672 * t68 * t673
      t677 = t41 * t52
      t678 = t106 * t677
      t680 = t68 * t80 * t10
      t682 = 0.176D3 * t678 * t680
      t683 = t106 * t107
      t685 = t68 * t200 * t119
      t686 = t683 * t685
      t688 = t106 * t265
      t689 = t80 * t89
      t691 = t193 * t689 * t131
      t692 = t688 * t691
      t694 = t31 * t335
      t695 = t337 * t119
      t697 = t694 * t234 * t695
      t699 = t43 ** 2
      t700 = t699 * t62
      t705 = 0.260D3 * t75 * t700 * t431 * t540 * t432
      t707 = 0.48D2 * t643 * t571
      t709 = t48 * t57 * t35
      t712 = 0.6D1 * t503 * t504 * t709
      t713 = -0.133D3 * t626 - t634 - t642 - t648 - t652 - 0.74D2 * t659
     # + 0.133D3 * t664 + 0.133D3 * t667 + 0.488D3 * t670 - 0.7D1 * t675
     # + t682 + 0.144D3 * t686 + 0.144D3 * t692 + 0.240D3 * t697 + t705 
     #+ t707 - t712
      t714 = t80 * x3
      t715 = t68 * t714
      t717 = 0.176D3 * t678 * t715
      t720 = 0.384D3 * t601 * t497 * t250
      t721 = t30 * t581
      t722 = t27 * t40
      t723 = t721 * t722
      t724 = t723 * t394
      t725 = 0.32D2 * t724
      t726 = t31 * t654
      t727 = t89 * t57
      t728 = t727 * x3
      t731 = 0.32D2 * t726 * t300 * t728
      t734 = 0.64D2 * t726 * t300 * t352
      t737 = 0.32D2 * t649 * t234 * t303
      t738 = t699 * t1
      t741 = t50 ** 2
      t744 = t49 * t741 * t62 * t55
      t746 = 0.2D1 * t31 * t40 * t738 * t744
      t747 = t477 * x2
      t748 = t638 * t35
      t750 = t655 * t747 * t748
      t754 = 0.6D1 * t275 * t68 * t70
      t758 = 0.6D1 * t672 * t79 * t673 * t35
      t761 = t79 * t81 * x2
      t762 = t628 * t246 * t761
      t763 = 0.72D2 * t762
      t764 = t221 * t348
      t765 = x2 * t35
      t766 = t765 * t10
      t767 = t617 * t766
      t769 = 0.64D2 * t764 * t767
      t771 = t726 * t225 * t657
      t774 = t106 * t76 * t67
      t776 = t623 * t35
      t778 = t774 * t386 * z * t776
      t779 = 0.64D2 * t778
      t780 = t26 * t42
      t783 = t79 * t80
      t784 = t111 * t10
      t787 = 0.64D2 * t721 * t780 * t52 * t783 * t784
      t788 = t52 * t138
      t790 = t295 * t788 * t20
      t791 = t527 * t790
      t793 = t48 * t138
      t795 = t793 * t20 * t131
      t797 = 0.8D1 * t98 * t795
      t798 = t717 + t720 + t725 + t731 - t734 + t737 + t746 - 0.148D3 * 
     #t750 + t754 - t758 - t763 - t769 - 0.128D3 * t771 - t779 - t787 - 
     #0.205D3 * t791 + t797
      t803 = t75 * t526 * t48
      t804 = t138 * t131
      t808 = 0.24D2 * t803 * t311 * t804 * t10
      t809 = t628 * t348
      t811 = t138 * t89 * x2
      t813 = t811 * t714 * t20
      t814 = t809 * t813
      t815 = 0.16D2 * t814
      t816 = t62 * t23
      t817 = t738 * t816
      t819 = t67 * t293
      t824 = 0.2D1 * t31 * t817 * t819 * t40 * t741 * t48
      t825 = t75 * t309
      t826 = t661 * t10
      t827 = t311 * t826
      t829 = 0.48D2 * t825 * t827
      t830 = t656 * t57
      t833 = 0.32D2 * t726 * t225 * t830
      t836 = 0.32D2 * t694 * t234 * t338
      t837 = t75 * t182
      t838 = t498 * x3
      t839 = t497 * t838
      t841 = 0.48D2 * t837 * t839
      t842 = t283 * t596
      t844 = 0.16D2 * t183 * t842
      t845 = t52 * t78
      t846 = t90 * t845
      t848 = 0.48D2 * t533 * t846
      t851 = x2 * t91
      t853 = t5 * x3 * t10
      t855 = t106 * t173 * t47 * t851 * t853
      t857 = t90 * t20
      t860 = 0.64D2 * t349 * t225 * t857
      t861 = t106 * t348
      t863 = t477 * t90 * t35
      t864 = t861 * t863
      t866 = t106 * t232
      t867 = t50 * t47
      t869 = t386 * t867 * x3
      t870 = t866 * t869
      t872 = t386 * t748
      t873 = t515 * t872
      t875 = t75 * t247
      t876 = t302 * t10
      t877 = t110 * t876
      t878 = t875 * t877
      t880 = t106 * t198
      t881 = t880 * t280
      t883 = t808 + t815 + t824 + t829 - t833 - t836 - t841 + t844 + t84
     #8 - 0.148D3 * t855 - t860 + 0.67D2 * t864 - 0.205D3 * t870 - 0.133
     #D3 * t873 - 0.96D2 * t878 + 0.86D2 * t881
      t884 = t469 * t113
      t885 = 0.120D3 * t884
      t888 = 0.16D2 * t378 * t110 * t381
      t889 = t255 * t10
      t891 = t875 * t343 * t889
      t893 = t623 * t69
      t896 = 0.64D2 * t649 * t234 * t893
      t899 = t482 * t193 * t81 * t131
      t900 = 0.5D1 * t899
      t901 = t189 * t131
      t904 = 0.32D2 * t515 * t386 * t901
      t907 = 0.32D2 * t616 * t300 * t516
      t908 = t75 * t348
      t909 = t908 * t813
      t910 = 0.74D2 * t909
      t914 = 0.48D2 * t248 * t300 * t99 * t20
      t916 = t31 * t45 * t48
      t920 = 0.48D2 * t916 * t366 * t177 * t10
      t921 = t26 * t80
      t922 = t721 * t921
      t923 = t922 * t397
      t924 = 0.32D2 * t923
      t928 = t809 * t811 * t35 * t27 * t10
      t929 = 0.16D2 * t928
      t930 = t30 * z
      t931 = t26 * t43
      t932 = t931 * t52
      t934 = t930 * t932 * t761
      t937 = t875 * t343 * t251
      t942 = 0.64D2 * t550 * t234 * t337 * t10
      t943 = t727 * t10
      t945 = t908 * t747 * t943
      t946 = 0.104D3 * t945
      t948 = t342 * t569 * t893
      t949 = 0.120D3 * t948
      t950 = -t885 + t888 - 0.64D2 * t891 - t896 - t900 - t904 - t907 - 
     #t910 + t914 - t920 + t924 + t929 - 0.1028D4 * t934 - 0.64D2 * t937
     # - t942 + t946 - t949
      t952 = t27 * t42
      t953 = t952 * t52
      t955 = t79 * t40
      t958 = 0.64D2 * t721 * t953 * t955 * t381
      t960 = t454 * t584 * t41
      t961 = t20 * t23
      t962 = t961 * x3
      t965 = 0.64D2 * t960 * t6 * t962
      t966 = t454 * t584
      t970 = t966 * t42 * t47 * t176 * t5
      t973 = t31 * t40 * t42
      t974 = t602 * t581
      t975 = t99 * t974
      t977 = 0.384D3 * t973 * t975
      t980 = t5 * z
      t984 = 0.384D3 * t31 * t97 * t47 * t311 * t980 * t10
      t985 = x2 * z
      t989 = 0.384D3 * t649 * t249 * t985 * x3
      t990 = t643 * t666
      t995 = t75 * t334 * t68 * t200 * t318
      t996 = 0.32D2 * t995
      t997 = t75 * t562
      t998 = t35 * t52
      t1000 = t497 * t998 * t119
      t1001 = t997 * t1000
      t1002 = 0.104D3 * t1001
      t1003 = t99 * t581
      t1006 = 0.384D3 * t875 * t110 * t1003
      t1007 = t27 * t43
      t1008 = t1007 * t52
      t1010 = t40 * t47
      t1012 = t79 * t1010 * x2
      t1013 = t930 * t1008 * t1012
      t1015 = t302 * t69
      t1018 = 0.32D2 * t643 * t110 * t1015
      t1019 = t75 * t654
      t1022 = 0.32D2 * t1019 * t386 * t352
      t1023 = t961 * t10
      t1026 = 0.64D2 * t960 * t6 * t1023
      t1029 = 0.64D2 * t960 * t6 * t499
      t1033 = 0.384D3 * t563 * t867 * t845 * z
      t1034 = t220 * z
      t1035 = t1034 * t26
      t1036 = t1035 * t348
      t1037 = x2 * t20
      t1039 = t617 * t1037 * x3
      t1041 = 0.64D2 * t1036 * t1039
      t1042 = -t958 + t965 + 0.1028D4 * t970 - t977 + t984 - t989 - 0.32
     #D2 * t990 + t996 + t1002 - t1006 - 0.1028D4 * t1013 + t1018 + t102
     #2 - t1026 + t1029 + t1033 - t1041
      t1043 = t623 * x3
      t1044 = t110 * t1043
      t1045 = t385 * t1044
      t1050 = 0.6D1 * t997 * t283 * t284 * x3
      t1051 = t226 * t158
      t1054 = 0.2D1 * t503 * t294 * t1051
      t1057 = 0.32D2 * t183 * t189 * t154
      t1059 = t183 * t184 * t393
      t1062 = t44 * t816 * t67
      t1067 = 0.24D2 * t31 * t1062 * t300 * t867 * t119
      t1068 = t538 * t67
      t1071 = t75 * t1068 * t477 * t709
      t1072 = 0.32D2 * t1071
      t1073 = t351 * t10
      t1076 = 0.32D2 * t1019 * t386 * t1073
      t1077 = t75 * t223
      t1078 = t293 * t80
      t1079 = t1078 * x2
      t1082 = 0.14D2 * t1077 * t1079 * t709
      t1083 = t226 * t139
      t1085 = t503 * t294 * t1083
      t1086 = 0.42D2 * t1085
      t1087 = t75 * t265
      t1089 = t193 * t689 * t57
      t1090 = t1087 * t1089
      t1092 = t75 * t107
      t1094 = t68 * t200 * t69
      t1095 = t1092 * t1094
      t1097 = t1092 * t685
      t1100 = 0.16D2 * t837 * t842
      t1102 = t295 * t788 * t35
      t1104 = 0.48D2 * t88 * t1102
      t1105 = t207 * t119
      t1108 = 0.32D2 * t563 * t337 * t1105
      t1109 = t628 * t86
      t1114 = t10 * t40
      t1115 = t1114 * x3
      t1117 = t1109 * t62 * t67 * t78 * t99 * t26 * t1115
      t1118 = 0.16D2 * t1117
      t1119 = -0.32D2 * t1045 - t1050 + t1054 + t1057 - 0.32D2 * t1059 +
     # t1067 + t1072 + t1076 + t1082 - t1086 + 0.74D2 * t1090 - 0.48D2 *
     # t1095 + 0.74D2 * t1097 + t1100 + t1104 + t1108 + t1118
      t1122 = t380 * t119
      t1125 = 0.32D2 * t109 * t110 * t1122
      t1126 = t47 * t57
      t1128 = t386 * t1126 * x3
      t1129 = t385 * t1128
      t1130 = 0.32D2 * t1129
      t1132 = t1037 * t35
      t1134 = t1077 * t477 * t48 * t1132
      t1135 = 0.80D2 * t1134
      t1137 = 0.32D2 * t533 * t209
      t1139 = t75 * t526 * t89
      t1143 = 0.8D1 * t1139 * t851 * t312 * t69
      t1145 = t52 * t67
      t1146 = t78 * t47
      t1149 = t628 * t1007 * t1145 * t1146 * x2
      t1150 = 0.72D2 * t1149
      t1151 = t106 * t274
      t1152 = t80 * z
      t1156 = 0.32D2 * t1151 * t68 * t1152 * t69
      t1160 = 0.32D2 * t1151 * t68 * t1152 * t119
      t1164 = 0.384D3 * t726 * t617 * t985 * t20
      t1165 = t1037 * t10
      t1168 = 0.16D2 * t908 * t557 * t1165
      t1169 = t31 * t76
      t1172 = t1169 * t79 * t1010 * t20
      t1176 = 0.32D2 * t966 * t125 * t154
      t1178 = t31 * t40 * t699
      t1179 = t48 * t432
      t1180 = t62 * t138
      t1182 = t1179 * t1180 * t10
      t1184 = 0.6D1 * t1178 * t1182
      t1185 = t527 * t94
      t1187 = t110 * t624
      t1188 = t385 * t1187
      t1192 = t31 * t1068 * t225 * t506
      t1193 = 0.32D2 * t1192
      t1194 = -t1125 - t1130 + t1135 - t1137 + t1143 - t1150 + t1156 + t
     #1160 - t1164 - t1168 + 0.205D3 * t1172 + t1176 - t1184 + 0.67D2 * 
     #t1185 - 0.208D3 * t1188 + t1193
      t1196 = t31 * t87 * t48
      t1197 = t138 * t57
      t1200 = t1196 * t311 * t1197 * t10
      t1201 = 0.120D3 * t1200
      t1203 = 0.32D2 * t563 * t286
      t1204 = t49 * t432
      t1205 = t91 * t55
      t1209 = 0.8D1 * t1178 * t1204 * t1205 * t20
      t1212 = 0.16D2 * t908 * t557 * t766
      t1215 = 0.2D1 * t1077 * t1078 * t1083
      t1217 = t431 * t689 * t158
      t1218 = t688 * t1217
      t1219 = 0.32D2 * t1218
      t1222 = t688 * t431 * t689 * t139
      t1223 = 0.32D2 * t1222
      t1224 = t31 * t212
      t1227 = t1224 * t68 * t673 * t10
      t1230 = t248 * t300 * t388
      t1231 = 0.104D3 * t1230
      t1233 = t550 * t234 * t1122
      t1234 = 0.120D3 * t1233
      t1238 = t183 * t184 * t52 * x3 * t10
      t1239 = 0.64D2 * t1238
      t1241 = t106 * t182 * t47
      t1243 = t1241 * t980 * t499
      t1248 = 0.384D3 * t643 * t343 * t985 * t10
      t1249 = t31 * t377
      t1250 = t380 * t10
      t1253 = 0.12D2 * t1249 * t234 * t1250
      t1256 = t225 * t48 * t1132
      t1258 = 0.64D2 * t1035 * t223 * t1256
      t1260 = t248 * t234 * t876
      t1263 = 0.32D2 * t922 * t133
      t1264 = -t1201 - t1203 + t1209 - t1212 + t1215 - t1219 - t1223 - 0
     #.67D2 * t1227 + t1231 - t1234 - t1239 + 0.64D2 * t1243 - t1248 - t
     #1253 + t1258 + 0.148D3 * t1260 + t1263
      t1266 = t387 * t10
      t1269 = 0.14D2 * t248 * t300 * t1266
      t1273 = 0.6D1 * t550 * t234 * t380 * t69
      t1274 = t26 * t86
      t1276 = t1034 * t1274 * t52
      t1277 = t431 * t40
      t1278 = t226 * t131
      t1281 = 0.32D2 * t1276 * t1277 * t1278
      t1283 = 0.384D3 * t837 * t975
      t1284 = t75 * t377
      t1285 = t337 * t581
      t1288 = 0.384D3 * t1284 * t110 * t1285
      t1289 = t184 * t69
      t1292 = 0.32D2 * t469 * t420 * t1289
      t1293 = t184 * t119
      t1295 = t469 * t420 * t1293
      t1299 = 0.384D3 * t144 * t3 * t581
      t1301 = 0.24D2 * t997 * t795
      t1302 = t131 * t23
      t1304 = t283 * t1302 * x3
      t1305 = t997 * t1304
      t1306 = 0.120D3 * t1305
      t1308 = t616 * t300 * t901
      t1310 = t75 * t63
      t1311 = t80 * t119
      t1313 = t68 * t1311 * t10
      t1315 = 0.8D1 * t1310 * t1313
      t1317 = t568 * t569 * t876
      t1322 = t26 * t40
      t1325 = t1109 * t1145 * t55 * t226 * t35 * t1322 * t20
      t1326 = 0.16D2 * t1325
      t1330 = 0.16D2 * t916 * t366 * t177 * x3
      t1332 = t31 * t87 * t89
      t1333 = t851 * t78
      t1336 = 0.48D2 * t1332 * t1333 * t472
      t1337 = t20 * t52
      t1339 = t497 * t1337 * t69
      t1341 = 0.64D2 * t563 * t1339
      t1342 = t1269 - t1273 + t1281 - t1283 - t1288 - t1292 + 0.32D2 * t
     #1295 + t1299 + t1301 - t1306 + 0.32D2 * t1308 + t1315 + 0.133D3 * 
     #t1317 + t1326 + t1330 - t1336 + t1341
      t1343 = t99 * t1105
      t1344 = t533 * t1343
      t1346 = t477 * t830
      t1347 = t1019 * t1346
      t1349 = t485 * t339
      t1353 = 0.6D1 * t1077 * t1079 * t506
      t1354 = t825 * t575
      t1357 = t221 * t246 * t1012
      t1360 = t79 * t81 * t35
      t1361 = t482 * t1360
      t1364 = t68 * t200 * t10
      t1365 = t669 * t1364
      t1367 = t596 * x3
      t1370 = 0.64D2 * t726 * t617 * t1367
      t1372 = t31 * t205 * t89
      t1374 = 0.48D2 * t1372 * t575
      t1376 = t1034 * t1274 * t62
      t1379 = 0.32D2 * t1376 * t955 * t235
      t1380 = t688 * t1089
      t1382 = t683 * t1094
      t1384 = t628 * t232
      t1386 = t1146 * t214 * t119
      t1387 = t1384 * t1386
      t1388 = 0.8D1 * t1387
      t1389 = t99 * t602
      t1391 = 0.32D2 * t973 * t1389
      t1393 = t106 * t562 * x2
      t1394 = t47 * t52
      t1397 = 0.48D2 * t1393 * t1394 * t313
      t1401 = 0.64D2 * t1393 * t89 * t23 * t178
      t1402 = -0.128D3 * t1344 - 0.128D3 * t1347 + 0.240D3 * t1349 - t13
     #53 - 0.32D2 * t1354 + 0.128D3 * t1357 + 0.488D3 * t1361 + 0.488D3 
     #* t1365 - t1370 + t1374 + t1379 + 0.144D3 * t1380 + 0.144D3 * t138
     #2 - t1388 - t1391 + t1397 - t1401
      t1408 = t454 * t953 * t216
      t1412 = t497 * t1023
      t1414 = 0.64D2 * t721 * t722 * t42 * t1412
      t1415 = t110 * t695
      t1416 = t336 * t1415
      t1421 = 0.32D2 * t554 * t328 * t329 * t139
      t1423 = 0.14D2 * t875 * t1128
      t1427 = 0.6D1 * t469 * t110 * t111 * t119
      t1428 = t497 * t962
      t1429 = t183 * t1428
      t1430 = 0.80D2 * t1429
      t1432 = x3 * t10
      t1434 = t183 * x1 * t52 * t1432
      t1435 = 0.74D2 * t1434
      t1437 = t378 * t379 * t1250
      t1440 = t378 * t110 * t1250
      t1441 = 0.80D2 * t1440
      t1443 = t774 * t386 * t776
      t1444 = 0.74D2 * t1443
      t1446 = t563 * t867 * t845
      t1450 = 0.32D2 * t563 * t656 * t534
      t1451 = t31 * t635
      t1455 = 0.176D3 * t1451 * t300 * t90 * x3
      t1456 = t1077 * t632
      t1457 = 0.5D1 * t1456
      t1459 = t248 * t234 * t570
      t1461 = 0.1028D4 * t1408 - t1414 + 0.48D2 * t1416 + t1421 + t1423 
     #- t1427 - t1430 + t1435 + 0.64D2 * t1437 - t1441 + t1444 + 0.32D2 
     #* t1446 + t1450 - t1455 + t1457 - 0.133D3 * t1459
      t1462 = t875 * t1187
      t1466 = t1332 * t851 * t661 * t69
      t1467 = 0.104D3 * t1466
      t1469 = t31 * t87 * t47
      t1470 = x2 * t62
      t1473 = t1470 * t5 * t119 * t10
      t1475 = 0.6D1 * t1469 * t1473
      t1476 = t225 * t1278
      t1478 = 0.32D2 * t224 * t1476
      t1481 = t803 * t311 * t804 * x3
      t1482 = 0.120D3 * t1481
      t1485 = t1139 * t851 * t312 * t119
      t1486 = 0.104D3 * t1485
      t1489 = 0.8D1 * t908 * t747 * t1073
      t1491 = t40 * t48
      t1494 = t31 * t538 * t431 * t1491 * t139
      t1495 = 0.32D2 * t1494
      t1499 = t31 * t334 * t68 * t673 * t323
      t1500 = 0.32D2 * t1499
      t1501 = t225 * t50
      t1502 = t89 * t35
      t1506 = 0.48D2 * t1451 * t1501 * t1502 * x3
      t1510 = 0.16D2 * t1451 * t1501 * t638 * x3
      t1514 = 0.384D3 * t678 * t68 * t1152 * x3
      t1515 = t75 * t274
      t1516 = t80 * t69
      t1517 = t68 * t1516
      t1519 = 0.24D2 * t1515 * t1517
      t1520 = t110 * t784
      t1522 = 0.16D2 * t378 * t1520
      t1523 = t98 * t1339
      t1524 = 0.104D3 * t1523
      t1526 = t793 * t57 * t35
      t1528 = 0.24D2 * t98 * t1526
      t1531 = 0.32D2 * t550 * t415 * t1293
      t1532 = -0.133D3 * t1462 + t1467 - t1475 + t1478 - t1482 + t1486 +
     # t1489 + t1495 + t1500 - t1506 + t1510 + t1514 + t1519 + t1522 + t
     #1524 + t1528 - t1531
      t1535 = 0.12D2 * t837 * t1428
      t1537 = 0.32D2 * t563 * t1526
      t1539 = t1241 * t980 * t838
      t1542 = t1241 * t980 * t962
      t1545 = 0.64D2 * t109 * t473
      t1548 = 0.64D2 * t1019 * t557 * t597
      t1549 = t206 * t535
      t1552 = 0.48D2 * t224 * t1256
      t1554 = 0.64D2 * t764 * t1039
      t1555 = t110 * t570
      t1556 = t385 * t1555
      t1559 = t68 * t714 * t10
      t1561 = 0.16D2 * t1151 * t1559
      t1563 = 0.32D2 * t485 * t1415
      t1565 = 0.32D2 * t723 * t155
      t1566 = t221 * t247
      t1567 = t234 * t1003
      t1569 = 0.384D3 * t1566 * t1567
      t1570 = t99 * z
      t1573 = 0.384D3 * t1566 * t234 * t1570
      t1574 = t661 * t35
      t1577 = 0.64D2 * t966 * t125 * t1574
      t1578 = z * t47
      t1581 = t774 * t386 * t1578 * t131
      t1583 = -t1535 - t1537 + 0.64D2 * t1539 + 0.64D2 * t1542 + t1545 -
     # t1548 + 0.240D3 * t1549 + t1552 - t1554 - 0.208D3 * t1556 + t1561
     # - t1563 + t1565 + t1569 + t1573 - t1577 - 0.32D2 * t1581
      t1585 = t86 * t816 * t67
      t1590 = 0.32D2 * t75 * t1585 * t110 * t337 * t323
      t1591 = t520 * t535
      t1594 = 0.16D2 * t349 * t1039
      t1597 = 0.8D1 * t349 * t350 * t728
      t1598 = t867 * t10
      t1601 = 0.48D2 * t260 * t300 * t1598
      t1604 = 0.48D2 * t1249 * t234 * t381
      t1605 = t111 * x3
      t1606 = t110 * t1605
      t1607 = t378 * t1606
      t1608 = 0.80D2 * t1607
      t1609 = t99 * t119
      t1610 = t234 * t1609
      t1612 = 0.32D2 * t233 * t1610
      t1614 = t378 * t379 * t1605
      t1618 = t266 * t193 * t267 * t57
      t1620 = t31 * t107
      t1622 = t68 * t673 * t69
      t1623 = t1620 * t1622
      t1626 = t248 * t249 * t889
      t1628 = t700 * t67
      t1629 = t31 * t1628
      t1630 = t1179 * t35
      t1633 = 0.6D1 * t1629 * t294 * t1630
      t1634 = t1087 * t1217
      t1635 = 0.42D2 * t1634
      t1639 = 0.32D2 * t1310 * t68 * t80 * t323
      t1640 = t106 * t63
      t1642 = t68 * t1516 * x3
      t1644 = 0.32D2 * t1640 * t1642
      t1645 = t533 * t523
      t1647 = t1590 + 0.48D2 * t1591 - t1594 + t1597 + t1601 - t1604 - t
     #1608 + t1612 + 0.64D2 * t1614 - 0.48D2 * t1618 + 0.74D2 * t1623 - 
     #0.64D2 * t1626 - t1633 - t1635 + t1639 - t1644 + 0.240D3 * t1645
      t1652 = 0.32D2 * t1376 * t955 * t1609
      t1653 = t765 * x3
      t1654 = t617 * t1653
      t1656 = 0.64D2 * t1036 * t1654
      t1660 = 0.2D1 * t75 * t80 * t738 * t744
      t1664 = t629 * t630 * x2 * t131 * t27
      t1665 = 0.8D1 * t1664
      t1667 = t866 * t386 * t1598
      t1669 = t432 * t89
      t1670 = t91 * t138
      t1672 = t527 * t1669 * t1670
      t1677 = 0.6D1 * t98 * t283 * t1302 * t10
      t1680 = 0.32D2 * t75 * t429 * t435
      t1682 = t861 * t477 * t857
      t1684 = t503 * t1476
      t1685 = 0.5D1 * t1684
      t1689 = 0.176D3 * t291 * t225 * t295 * t20
      t1691 = t550 * t415 * t1289
      t1694 = t385 * t386 * t1266
      t1695 = 0.32D2 * t1694
      t1698 = t431 * t1491 * t432
      t1700 = 0.260D3 * t31 * t700 * t1698
      t1702 = t1109 * t592 * t138
      t1706 = t1702 * t656 * t26 * t1114 * t20
      t1707 = 0.16D2 * t1706
      t1709 = t248 * t234 * t1043
      t1711 = t1652 - t1656 + t1660 - t1665 - 0.205D3 * t1667 + 0.128D3 
     #* t1672 - t1677 - t1680 + 0.67D2 * t1682 + t1685 - t1689 + 0.32D2 
     #* t1691 - t1695 + t1700 - t1707 - 0.96D2 * t1709
      t1714 = 0.64D2 * t109 * t470 * t645
      t1718 = 0.48D2 * t550 * t234 * x1 * t645
      t1724 = 0.48D2 * t75 * t239 * t193 * t80 * t50 * t47
      t1727 = 0.16D2 * t275 * t68 * t1115
      t1729 = 0.24D2 * t1310 * t1642
      t1731 = 0.48D2 * t1284 * t1520
      t1735 = 0.384D3 * t875 * t569 * t1578 * t20
      t1738 = 0.384D3 * t1249 * t234 * t1285
      t1740 = t875 * t343 * t256
      t1741 = 0.64D2 * t1740
      t1745 = t106 * t347 * t193 * t689 * t50
      t1748 = t221 * t700 * t1698
      t1753 = t221 * t347 * t193 * t267 * t50
      t1756 = 0.32D2 * t563 * t795
      t1759 = 0.48D2 * t1393 * t1394 * t826
      t1762 = 0.48D2 * t1393 * t1394 * t662
      t1766 = 0.64D2 * t1393 * x1 * t91 * t853
      t1768 = 0.12D2 * t1284 * t1606
      t1769 = t1714 - t1718 + t1724 + t1727 + t1729 - t1731 + t1735 - t1
     #738 + t1741 + 0.86D2 * t1745 - 0.240D3 * t1748 + 0.32D2 * t1753 - 
     #t1756 - t1759 + t1762 - t1766 - t1768
      t1772 = 0.16D2 * t1515 * t1559
      t1775 = 0.14D2 * t476 * t1078 * t296
      t1780 = 0.24D2 * t75 * t1062 * t386 * t867 * t69
      t1782 = t106 * t246 * t761
      t1785 = 0.32D2 * t1640 * t1313
      t1789 = 0.24D2 * t1196 * t311 * t1197 * x3
      t1792 = t469 * t420 * x1 * t608
      t1796 = 0.64D2 * t875 * t343 * t612
      t1797 = t503 * t1256
      t1798 = 0.80D2 * t1797
      t1800 = 0.32D2 * t1019 * t658
      t1803 = 0.38D2 * t342 * t110 * t235
      t1804 = t342 * t1386
      t1805 = 0.5D1 * t1804
      t1811 = 0.32D2 * t31 * t429 * t431 * t40 * t432 * t89
      t1814 = 0.768D3 * t385 * t110 * t1570
      t1816 = 0.384D3 * t248 * t1567
      t1821 = 0.32D2 * t31 * t1585 * t234 * t337 * t318
      t1826 = 0.64D2 * t106 * t274 * t67 * t420 * t608
      t1827 = t1772 + t1775 + t1780 - 0.1656D4 * t1782 - t1785 + t1789 -
     # 0.64D2 * t1792 - t1796 + t1798 - t1800 + t1803 + t1805 - t1811 + 
     #t1814 - t1816 + t1821 + t1826
      t1829 = t193 * t1010 * t131
      t1831 = 0.14D2 * t1169 * t1829
      t1832 = t31 * t677
      t1836 = 0.8D1 * t1832 * t68 * t40 * x3
      t1838 = 0.64D2 * t554 * t529
      t1840 = 0.48D2 * t554 * t790
      t1842 = 0.32D2 * t206 * t1343
      t1844 = 0.32D2 * t206 * t523
      t1848 = 0.32D2 * t64 * t68 * t40 * t318
      t1854 = 0.2D1 * t75 * t817 * t819 * t80 * t741 * t48
      t1857 = t655 * t386 * t656 * t20
      t1861 = 0.64D2 * t1019 * t386 * t943
      t1864 = 0.32D2 * t643 * t110 * t893
      t1865 = t562 * t89
      t1866 = t106 * t1865
      t1867 = t312 * t962
      t1869 = 0.64D2 * t1866 * t1867
      t1872 = t1169 * t79 * t1010 * t35
      t1876 = t1224 * t68 * t673 * x3
      t1880 = t266 * t431 * t267 * t139
      t1881 = 0.42D2 * t1880
      t1884 = t568 * t110 * t99 * x3
      t1886 = t656 * t35
      t1888 = t655 * t386 * t1886
      t1890 = t1831 + t1836 - t1838 + t1840 - t1842 - t1844 + t1848 + t1
     #854 + 0.488D3 * t1857 - t1861 + t1864 + t1869 - 0.67D2 * t1872 + 0
     #.205D3 * t1876 - t1881 + 0.488D3 * t1884 + 0.488D3 * t1888
      t1895 = 0.12D2 * t973 * t500
      t1898 = 0.24D2 * t342 * t569 * t1015
      t1903 = 0.32D2 * t721 * t952 * t592 * t415 * t119
      t1905 = 0.16D2 * t183 * t839
      t1907 = t497 * t1337 * t119
      t1909 = 0.14D2 * t997 * t1907
      t1910 = t68 * t1311
      t1912 = 0.38D2 * t1151 * t1910
      t1913 = t875 * t1044
      t1915 = t469 * t578
      t1920 = 0.128D3 * t220 * t582 * t932 * t1012
      t1921 = t623 * t119
      t1924 = 0.24D2 * t260 * t301 * t1921
      t1925 = t1179 * t1670
      t1927 = 0.32D2 * t46 * t1925
      t1928 = t875 * t1555
      t1930 = t213 * t1364
      t1932 = t77 * t1360
      t1939 = 0.384D3 * t106 * t1 * t23 * t68 * t80 * t581
      t1941 = t378 * t379 * t784
      t1943 = -t1895 + t1898 + t1903 + t1905 + t1909 - t1912 + 0.148D3 *
     # t1913 - 0.133D3 * t1915 + t1920 + t1924 - t1927 - 0.133D3 * t1928
     # + 0.205D3 * t1930 + 0.205D3 * t1932 + t1939 + 0.64D2 * t1941
      t1946 = 0.384D3 * t601 * t1578 * t150
      t1949 = 0.32D2 * t726 * t300 * t943
      t1951 = t497 * t998 * t69
      t1952 = t563 * t1951
      t1953 = 0.32D2 * t1952
      t1957 = 0.176D3 * t636 * t386 * t90 * t10
      t1961 = 0.8D1 * t1332 * t851 * t661 * t119
      t1963 = 0.8D1 * t997 * t1526
      t1965 = 0.16D2 * t183 * t1412
      t1967 = 0.64D2 * t908 * t863
      t1970 = 0.48D2 * t649 * t301 * t624
      t1971 = t655 * t1346
      t1974 = t616 * t300 * t748
      t1979 = t1384 * t1146 * x2 * t27 * t69
      t1980 = 0.8D1 * t1979
      t1981 = t385 * t877
      t1984 = 0.14D2 * t98 * t1951
      t1985 = t661 * t499
      t1987 = 0.64D2 * t1866 * t1985
      t1989 = t1241 * t980 * t1023
      t1994 = 0.16D2 * t636 * t637 * t1502 * t10
      t1995 = t1946 + t1949 - t1953 - t1957 + t1961 + t1963 + t1965 - t1
     #967 + t1970 - 0.74D2 * t1971 - 0.133D3 * t1974 - t1980 - 0.32D2 * 
     #t1981 + t1984 + t1987 + 0.64D2 * t1989 + t1994
      t2000 = 0.12D2 * t476 * t1078 * t50 * t546
      t2002 = t699 * t816 * t67
      t2007 = 0.8D1 * t75 * t2002 * t477 * t1669 * t10
      t2008 = t75 * t1628
      t2009 = t1179 * t20
      t2012 = 0.6D1 * t2008 * t1078 * t2009
      t2014 = 0.64D2 * t1036 * t767
      t2015 = t617 * t1165
      t2017 = 0.16D2 * t349 * t2015
      t2021 = 0.2D1 * t1310 * t68 * t80 * t318
      t2022 = t75 * t677
      t2024 = 0.8D1 * t2022 * t680
      t2028 = 0.2D1 * t31 * t40 * t1 * x1
      t2030 = t80 * t1 * x1
      t2032 = 0.260D3 * t106 * t2030
      t2034 = 0.2D1 * t75 * t2030
      t2039 = 0.32D2 * t721 * t780 * t592 * t420 * t69
      t2042 = t721 * t952 * t23 * t1829
      t2043 = 0.32D2 * t2042
      t2047 = t694 * t415 * x1 * t69 * x3
      t2048 = 0.32D2 * t2047
      t2052 = 0.2D1 * t64 * t68 * t40 * t323
      t2055 = 0.48D2 * t1393 * t1394 * t574
      t2057 = 0.32D2 * t837 * t1389
      t2059 = 0.32D2 * t117 * t1925
      t2060 = -t2000 + t2007 - t2012 - t2014 - t2017 + t2021 + t2024 + t
     #2028 - t2032 + t2034 + t2039 + t2043 + t2048 + t2052 - t2055 - t20
     #57 - t2059
      t2062 = 0.16D2 * t349 * t1654
      t2066 = 0.384D3 * t678 * t68 * t1152 * t10
      t2070 = 0.48D2 * t875 * t386 * t99 * t35
      t2072 = t568 * t569 * t1043
      t2075 = t386 * t1126 * t10
      t2076 = t875 * t2075
      t2077 = 0.104D3 * t2076
      t2080 = t568 * t110 * t99 * t10
      t2083 = 0.38D2 * t503 * t228
      t2087 = 0.384D3 * t1019 * t557 * t765 * z
      t2090 = t1702 * t1886 * t1322 * x3
      t2091 = 0.16D2 * t2090
      t2094 = 0.6D1 * t1629 * t294 * t2009
      t2095 = t295 * t131
      t2098 = 0.14D2 * t291 * t294 * t2095
      t2099 = t260 * t236
      t2100 = 0.5D1 * t2099
      t2104 = 0.64D2 * t721 * t921 * t42 * t839
      t2107 = 0.384D3 * t601 * t337 * t974
      t2108 = t349 * t767
      t2109 = 0.74D2 * t2108
      t2111 = 0.48D2 * t973 * t1412
      t2113 = 0.16D2 * t973 * t842
      t2114 = -t2062 + t2066 + t2070 + 0.133D3 * t2072 + t2077 + 0.488D3
     # * t2080 + t2083 - t2087 - t2091 + t2094 + t2098 + t2100 - t2104 +
     # t2107 - t2109 - t2111 + t2113
      t2118 = 0.48D2 * t342 * t869
      t2122 = 0.64D2 * t469 * t110 * t337 * x3
      t2124 = t1179 * t1180 * x3
      t2126 = 0.6D1 * t1178 * t2124
      t2128 = 0.6D1 * t46 * t121
      t2130 = 0.6D1 * t2022 * t715
      t2131 = t520 * t209
      t2133 = t520 * t1343
      t2137 = t40 * x2
      t2141 = 0.48D2 * t31 * t86 * t593 * t2137 * t89 * t1367
      t2142 = t482 * t195
      t2143 = 0.5D1 * t2142
      t2145 = 0.38D2 * t1151 * t1517
      t2148 = 0.6D1 * t1832 * t68 * t1114
      t2150 = t75 * t526 * t47
      t2152 = 0.14D2 * t2150 * t1473
      t2155 = t1470 * t5 * t69 * x3
      t2157 = 0.6D1 * t2150 * t2155
      t2159 = 0.14D2 * t1469 * t2155
      t2161 = 0.32D2 * t563 * t1304
      t2164 = 0.16D2 * t908 * t557 * t1653
      t2165 = t2118 - t2122 + t2126 + t2128 + t2130 - 0.74D2 * t2131 - 0
     #.74D2 * t2133 - t2141 - t2143 - t2145 + t2148 + t2152 - t2157 + t2
     #159 - t2161 - t2164
      t2169 = 0.32D2 * t1224 * t79 * t2137 * x1
      t2171 = t106 * t514 * t872
      t2175 = t721 * t780 * t23 * t195
      t2176 = 0.32D2 * t2175
      t2179 = t1620 * t68 * t673 * t119
      t2183 = 0.48D2 * t75 * t1865 * t1867
      t2187 = 0.48D2 * t31 * t97 * t89 * t1985
      t2189 = 0.6D1 * t1515 * t1910
      t2192 = 0.64D2 * t183 * t189 * t1574
      t2195 = 0.32D2 * t563 * t656 * t522
      t2199 = 0.48D2 * t997 * t99 * t100 * t10
      t2200 = t23 * t78
      t2204 = 0.64D2 * t997 * t656 * t2200 * t20
      t2210 = 0.384D3 * t75 * t562 * t47 * t311 * t980 * x3
      t2213 = 0.64D2 * t643 * t110 * t303
      t2215 = 0.64D2 * t385 * t2075
      t2217 = 0.38D2 * t260 * t1610
      t2221 = 0.24D2 * t64 * t68 * t276 * t10
      t2225 = 0.768D3 * t880 * t68 * t200 * z
      t2226 = -t2169 - 0.32D2 * t2171 + t2176 - 0.48D2 * t2179 - t2183 -
     # t2187 + t2189 + t2192 + t2195 + t2199 - t2204 + t2210 - t2213 + t
     #2215 + t2217 + t2221 + t2225
      t2230 = 0.32D2 * t966 * t125 * t132
      t2232 = t75 * t80 * t699
      t2234 = 0.6D1 * t2232 * t1182
      t2235 = t1372 * t827
      t2239 = 0.48D2 * t1139 * t1333 * t645
      t2241 = 0.48D2 * t764 * t2015
      t2244 = 0.6D1 * t2008 * t1078 * t1630
      t2247 = 0.6D1 * t476 * t1078 * t2095
      t2250 = 0.64D2 * t960 * t6 * t838
      t2252 = t649 * t301 * t570
      t2258 = 0.8D1 * t31 * t2002 * t225 * t1669 * x3
      t2260 = 0.48D2 * t233 * t262
      t2262 = 0.48D2 * t764 * t1654
      t2263 = t554 * t320
      t2264 = 0.42D2 * t2263
      t2266 = 0.6D1 * t117 * t511
      t2267 = t1087 * t691
      t2272 = 0.64D2 * t98 * t656 * t2200 * t35
      t2275 = t774 * t386 * t1578 * t57
      t2277 = t2230 + t2234 - 0.32D2 * t2235 - t2239 - t2241 + t2244 + t
     #2247 - t2250 - 0.32D2 * t2252 + t2258 + t2260 - t2262 - t2264 + t2
     #266 - 0.48D2 * t2267 - t2272 - 0.32D2 * t2275
      t2280 = 0.32D2 * t1276 * t1277 * t227
      t2282 = 0.64D2 * t1036 * t2015
      t2285 = 0.64D2 * t966 * t677 * t1432
      t2288 = 0.38D2 * t1077 * t477 * t1278
      t2291 = 0.64D2 * t1035 * t232 * t262
      t2295 = 0.384D3 * t248 * t301 * t302 * z
      t2296 = t527 * t1102
      t2299 = t1077 * t1078 * t1051
      t2300 = 0.42D2 * t2299
      t2303 = 0.32D2 * t649 * t234 * t1921
      t2307 = 0.24D2 * t117 * t51 * t56 * t131
      t2311 = 0.8D1 * t2232 * t1204 * t1205 * t35
      t2313 = 0.6D1 * t2232 * t2124
      t2314 = t310 * t827
      t2317 = 0.48D2 * t206 * t846
      t2319 = t106 * t116 * t1925
      t2322 = 0.64D2 * t563 * t1000
      t2323 = t563 * t1907
      t2324 = 0.32D2 * t2323
      t2325 = t2280 - t2282 - t2285 + t2288 + t2291 + t2295 - 0.205D3 * 
     #t2296 - t2300 + t2303 + t2307 + t2311 - t2313 + 0.133D3 * t2314 + 
     #t2317 + 0.7D1 * t2319 + t2322 - t2324
      t2334 = -t61 - t74 - 0.12D2 * t84 + t96 - t104 + t115 - t123 - t12
     #9 - t135 - t143 - t146 + t148 - t153 - t157 - t162 - t164 - t167
      t2340 = -t169 - t171 - 0.240D3 * t180 + 0.48D2 * t187 - t192 - t19
     #7 + t204 + 0.80D2 * t210 + t218 - t230 - t238 - t245 + 0.48D2 * t2
     #53 + t259 - t264 - 0.40D2 * t270 - t279
      t2346 = 0.52D2 * t281 + t288 - t299 + t306 - t308 - 0.30D2 * t315 
     #- t322 + t327 - t333 - 0.120D3 * t340 - t346 - t355 + t363 + t371 
     #- t375 - 0.48D2 * t383 - t391
      t2348 = t396 - t399 + t403 + t408 + t410 + t412 - t418 + t423 - t4
     #26 - t428 - 0.52D2 * t436 + t440 + t444 + t446 - t448 + t453 - t45
     #9
      t2352 = t27 * t26 * t43
      t2356 = 0.16D2 * t454 * t2352 * t91 * t1622
      t2364 = t80 * t40
      t2368 = 0.32D2 * t454 * t932 * t79 * t2364 * t1043
      t2369 = -t462 - t464 + t2356 - 0.12D2 * t467 + t475 + t481 - 0.597
     #D3 * t483 - t490 - 0.240D3 * t495 + t502 - t509 - t513 - 0.48D2 * 
     #t518 - 0.120D3 * t524 + 0.12D2 * t530 - t2368 + t537
      t2374 = 0.64D2 * t579
      t2381 = 0.16D2 * t454 * t931 * t91 * t68 * t2364 * x1 * t119
      t2386 = -t544 + t549 + 0.30D2 * t552 - t556 + 0.80D2 * t560 - t566
     # - 0.30D2 * t572 - 0.30D2 * t576 - t2374 + t2381 - t586 + t590 + t
     #600 + 0.288D3 * t604 + 0.80D2 * t610 - 0.32D2 * t614 + 0.80D2 * t6
     #19
      t2398 = 0.30D2 * t626 + 0.16D2 * t633 + t642 + t648 + t652 + 0.40D
     #2 * t659 - 0.30D2 * t664 - 0.30D2 * t667 - 0.597D3 * t670 + 0.52D2
     # * t675 - t682 - 0.192D3 * t686 - 0.192D3 * t692 - 0.288D3 * t697 
     #- t705 - t707 + t712
      t2405 = 0.16D2 * t454 * t931 * t23 * t193 * t2364 * t89 * t57
      t2411 = -t717 - t720 - t725 + t2405 - t731 + t734 - t737 - t746 + 
     #0.80D2 * t750 - t754 + t758 + 0.144D3 * t762 + t769 + 0.80D2 * t77
     #1 + 0.32D2 * t778 + t787 + 0.102D3 * t791
      t2422 = -t797 - t808 - 0.32D2 * t814 - t824 - t829 + t833 + t836 +
     # t841 - t844 - t848 + 0.80D2 * t855 + t860 + 0.12D2 * t864 + 0.102
     #D3 * t870 + 0.30D2 * t873 + 0.240D3 * t878 - 0.176D3 * t881
      t2427 = t885 - t888 + 0.48D2 * t891 + t896 + t900 + t904 + t907 + 
     #t910 - t914 + t920 - t924 - 0.32D2 * t928 + 0.1508D4 * t934 + 0.48
     #D2 * t937 + t942 - t946 + t949
      t2432 = t958 - t965 - 0.1508D4 * t970 + t977 - t984 + t989 + 0.128
     #D3 * t990 - t996 - t1002 + t1006 + 0.1508D4 * t1013 - t1018 - t102
     #2 + t1026 - t1029 - t1033 + t1041
      t2433 = 0.64D2 * t1045
      t2439 = -t2433 + t1050 - t1054 - t1057 + 0.48D2 * t1059 - t1067 - 
     #t1072 - t1076 - t1082 + t1086 - 0.40D2 * t1090 + 0.120D3 * t1095 -
     # 0.40D2 * t1097 - t1100 - t1104 - t1108 - 0.32D2 * t1117
      t2446 = t1125 + t1130 - t1135 + t1137 - t1143 + 0.144D3 * t1149 - 
     #t1156 - t1160 + t1164 + t1168 - 0.102D3 * t1172 - t1176 + t1184 + 
     #0.12D2 * t1185 + 0.112D3 * t1188 - t1193 + t1201
      t2450 = 0.16D2 * t454 * t2352 * t23 * t269
      t2454 = t454 * t1008
      t2457 = 0.32D2 * t2454 * t783 * t624
      t2459 = t1203 - t1209 + t2450 + t1212 - t1215 + t1219 + t1223 - 0.
     #12D2 * t1227 - t1231 + t1234 + 0.32D2 * t1238 - 0.80D2 * t1243 + t
     #1248 + t1253 + t2457 - t1258 - 0.80D2 * t1260
      t2465 = -t1263 - t1269 + t1273 - t1281 + t1283 + t1288 + t1292 - 0
     #.48D2 * t1295 - t1299 - t1301 + t1306 - 0.48D2 * t1308 - t1315 - 0
     #.30D2 * t1317 - 0.32D2 * t1325 - t1330 + t1336
      t2481 = 0.32D2 * t454 * t1007 * t23 * t193 * t80 * t748
      t2482 = -t1341 + 0.80D2 * t1344 + 0.80D2 * t1347 - 0.288D3 * t1349
     # + t1353 + 0.128D3 * t1354 - 0.80D2 * t1357 - 0.597D3 * t1361 - 0.
     #597D3 * t1365 + t1370 - t1374 - t1379 - 0.192D3 * t1380 - 0.192D3 
     #* t1382 + 0.16D2 * t1387 + t1391 - t2481
      t2492 = 0.32D2 * t454 * t1007 * t91 * t68 * t80 * t493
      t2495 = 0.32D2 * t2454 * t783 * t570
      t2500 = -t2492 + t2495 - t1397 + t1401 - 0.1508D4 * t1408 + t1414 
     #- 0.120D3 * t1416 - t1421 - t1423 + t1427 + t1430 - t1435 - 0.80D2
     # * t1437 + t1441 - t1444 - 0.128D3 * t1446 - t1450
      t2503 = t1455 - t1457 + 0.30D2 * t1459 + 0.30D2 * t1462 - t1467 + 
     #t1475 - t1478 + t1482 - t1486 - t1489 - t1495 - t1500 + t1506 - t1
     #510 - t1514 - t1519 - t1522
      t2509 = -t1524 - t1528 + t1531 + t1535 + t1537 - 0.48D2 * t1539 - 
     #0.80D2 * t1542 - t1545 + t1548 - 0.288D3 * t1549 - t1552 + t1554 +
     # 0.112D3 * t1556 - t1561 + t1563 - t1565 - t1569
      t2517 = 0.32D2 * t454 * t2352 * t52 * t955 * t876
      t2521 = -t1573 + t1577 + 0.48D2 * t1581 - t1590 - 0.120D3 * t1591 
     #+ t1594 - t1597 - t1601 + t1604 + t1608 - t1612 - 0.80D2 * t1614 -
     # t2517 + 0.120D3 * t1618 - 0.40D2 * t1623 + 0.48D2 * t1626 + t1633
      t2530 = t1635 - t1639 + t1644 - 0.288D3 * t1645 - t1652 + t1656 - 
     #t1660 + 0.16D2 * t1664 + 0.102D3 * t1667 - 0.80D2 * t1672 + t1677 
     #+ t1680 + 0.12D2 * t1682 - t1685 + t1689 - 0.48D2 * t1691 + t1695
      t2537 = -t1700 + 0.32D2 * t1706 + 0.240D3 * t1709 - t1714 + t1718 
     #- t1724 - t1727 - t1729 + t1731 - t1735 + t1738 - 0.32D2 * t1740 -
     # 0.176D3 * t1745 + 0.288D3 * t1748 - 0.128D3 * t1753 + t1756 + t17
     #59
      t2541 = -t1762 + t1766 + t1768 - t1772 - t1775 - t1780 + 0.984D3 *
     # t1782 + t1785 - t1789 + 0.80D2 * t1792 + t1796 - t1798 + t1800 - 
     #t1803 - t1805 + t1811 - t1814
      t2545 = t1816 - t1821 - t1826 - t1831 - t1836 + t1838 - t1840 + t1
     #842 + t1844 - t1848 - t1854 - 0.597D3 * t1857 + t1861 - t1864 - t1
     #869 - 0.12D2 * t1872 - 0.102D3 * t1876
      t2556 = t1881 - 0.597D3 * t1884 - 0.597D3 * t1888 + t1895 - t1898 
     #- t1903 - t1905 - t1909 + t1912 - 0.80D2 * t1913 + 0.30D2 * t1915 
     #- t1920 - t1924 + t1927 + 0.30D2 * t1928 - 0.102D3 * t1930 - 0.102
     #D3 * t1932
      t2561 = 0.64D2 * t1981
      t2562 = -t1939 - 0.48D2 * t1941 - t1946 - t1949 + t1953 + t1957 - 
     #t1961 - t1963 - t1965 + t1967 - t1970 + 0.40D2 * t1971 + 0.30D2 * 
     #t1974 + 0.16D2 * t1979 - t2561 - t1984 - t1987
      t2565 = -0.48D2 * t1989 - t1994 + t2000 - t2007 + t2012 + t2014 + 
     #t2017 - t2021 - t2024 - t2028 + t2032 - t2034 - t2039 - t2043 - t2
     #048 - t2052 + t2055
      t2569 = t2057 + t2059 + t2062 - t2066 - t2070 - 0.30D2 * t2072 - t
     #2077 - 0.597D3 * t2080 - t2083 + t2087 + 0.32D2 * t2090 - t2094 - 
     #t2098 - t2100 + t2104 - t2107 + t2109
      t2574 = t2111 - t2113 - t2118 + t2122 - t2126 - t2128 - t2130 + 0.
     #40D2 * t2131 + 0.40D2 * t2133 + t2141 + t2143 + t2145 - t2148 - t2
     #152 + t2157 - t2159 + t2161
      t2575 = 0.64D2 * t2171
      t2577 = t2164 + t2169 - t2575 - t2176 + 0.120D3 * t2179 + t2183 + 
     #t2187 - t2189 - t2192 - t2195 - t2199 + t2204 - t2210 + t2213 - t2
     #215 - t2217 - t2221
      t2582 = -t2225 - t2230 - t2234 + 0.128D3 * t2235 + t2239 + t2241 -
     # t2244 - t2247 + t2250 + 0.128D3 * t2252 - t2258 - t2260 + t2262 +
     # t2264 - t2266 + 0.120D3 * t2267 + t2272
      t2587 = 0.48D2 * t2275 - t2280 + t2282 + t2285 - t2288 - t2291 - t
     #2295 + 0.102D3 * t2296 + t2300 - t2303 - t2307 - t2311 + t2313 - 0
     #.30D2 * t2314 - t2317 - 0.52D2 * t2319 - t2322 + t2324
      t2608 = -0.16D2 * t84 - 0.96D2 * t187 + 0.176D3 * t210 + 0.16D2 * 
     #t253 + 0.16D2 * t263 + 0.64D2 * t270 - 0.80D2 * t281 - 0.32D2 * t2
     #87 - 0.32D2 * t305 + 0.64D2 * t315 - 0.32D2 * t326 + 0.16D2 * t345
     # + 0.64D2 * t354
      t2621 = -0.16D2 * t383 + 0.32D2 * t395 - 0.32D2 * t407 - 0.64D2 * 
     #t409 + 0.32D2 * t411 + 0.80D2 * t436 - 0.32D2 * t443 - 0.64D2 * t4
     #45 - t2356 - 0.16D2 * t467 + 0.48D2 * t483 + 0.64D2 * t489 - 0.16D
     #2 * t501
      t2632 = 0.96D2 * t518 + 0.16D2 * t530 + t2368 + 0.64D2 * t543 - 0.
     #64D2 * t552 - 0.16D2 * t560 + 0.32D2 * t572 + 0.32D2 * t576 - t237
     #4 - t2381 - 0.176D3 * t604 - 0.16D2 * t610 + t615
      t2645 = -0.16D2 * t619 - 0.32D2 * t626 - t634 - 0.64D2 * t659 + 0.
     #64D2 * t664 + 0.32D2 * t667 + 0.48D2 * t670 - 0.80D2 * t675 + 0.32
     #D2 * t686 + 0.32D2 * t692 + 0.176D3 * t697 + 0.64D2 * t724 - t2405
     # - 0.256D3 * t750
      t2657 = -t763 + 0.176D3 * t771 - t779 - 0.48D2 * t791 + t815 - 0.2
     #56D3 * t855 + 0.16D2 * t864 - 0.48D2 * t870 - 0.64D2 * t873 + 0.16
     #0D3 * t881 - 0.32D2 * t884 + t888 + 0.16D2 * t891
      t2670 = 0.32D2 * t899 - 0.64D2 * t909 + 0.64D2 * t923 + t929 - 0.2
     #88D3 * t934 + 0.16D2 * t937 + 0.64D2 * t945 - 0.32D2 * t948 + 0.28
     #8D3 * t970 - 0.352D3 * t990 + 0.64D2 * t995 + 0.64D2 * t1001 - 0.2
     #88D3 * t1013
      t2681 = -t2433 - 0.96D2 * t1059 + 0.64D2 * t1071 - 0.32D2 * t1085 
     #+ 0.64D2 * t1090 + 0.64D2 * t1097 + t1118 - 0.64D2 * t1129 + 0.16D
     #2 * t1134 - t1150 - t1168 + 0.48D2 * t1172 + 0.16D2 * t1185
      t2693 = -0.192D3 * t1188 + 0.64D2 * t1192 - 0.32D2 * t1200 - t2450
     # - 0.64D2 * t1218 - 0.64D2 * t1222 - 0.16D2 * t1227 + 0.64D2 * t12
     #30 - 0.32D2 * t1233 - t1239 + 0.16D2 * t1243 - t2457 + 0.256D3 * t
     #1260 + 0.96D2 * t1295
      t2709 = -0.32D2 * t1305 + 0.96D2 * t1308 + 0.64D2 * t1317 + t1326 
     #+ 0.176D3 * t1344 + 0.176D3 * t1347 + 0.176D3 * t1349 - 0.352D3 * 
     #t1354 - 0.176D3 * t1357 + 0.48D2 * t1361 + 0.48D2 * t1365 + 0.32D2
     # * t1380 + 0.32D2 * t1382
      t2719 = -t1388 + t2481 + t2492 - t2495 + 0.288D3 * t1408 - 0.16D2 
     #* t1429 + 0.64D2 * t1434 + 0.16D2 * t1437 - 0.16D2 * t1440 + 0.64D
     #2 * t1443 + 0.352D3 * t1446 - 0.32D2 * t1456 - 0.32D2 * t1459
      t2733 = -0.32D2 * t1462 + 0.64D2 * t1466 - 0.32D2 * t1481 + 0.64D2
     # * t1485 + 0.64D2 * t1494 + 0.64D2 * t1499 + t1522 + 0.64D2 * t152
     #3 - 0.16D2 * t1539 + 0.16D2 * t1542 + 0.176D3 * t1549 - 0.192D3 * 
     #t1556 - 0.96D2 * t1581
      t2746 = -0.16D2 * t1607 + 0.16D2 * t1614 + t2517 + 0.64D2 * t1623 
     #+ 0.16D2 * t1626 - 0.32D2 * t1634 + 0.176D3 * t1645 - t1665 - 0.48
     #D2 * t1667 - 0.176D3 * t1672 + 0.16D2 * t1682 - 0.32D2 * t1684 + 0
     #.96D2 * t1691 - 0.64D2 * t1694
      t2760 = -t1707 + t1741 + 0.160D3 * t1745 - 0.176D3 * t1748 + 0.352
     #D3 * t1753 + 0.864D3 * t1782 - 0.16D2 * t1792 + 0.16D2 * t1797 - 0
     #.32D2 * t1804 + 0.48D2 * t1857 - 0.16D2 * t1872 + 0.48D2 * t1876 -
     # 0.32D2 * t1880
      t2772 = 0.48D2 * t1884 + 0.48D2 * t1888 + t1905 + 0.256D3 * t1913 
     #- 0.64D2 * t1915 - 0.32D2 * t1928 + 0.48D2 * t1930 + 0.48D2 * t193
     #2 - 0.16D2 * t1941 - 0.64D2 * t1952 + t1965 - 0.64D2 * t1971 - 0.6
     #4D2 * t1974 - t1980
      t2783 = -t2561 - 0.16D2 * t1989 - t2017 + 0.64D2 * t2042 + 0.64D2 
     #* t2047 - t2062 + 0.64D2 * t2072 + 0.64D2 * t2076 + 0.48D2 * t2080
     # - t2091 - 0.32D2 * t2099 - 0.64D2 * t2108 - 0.64D2 * t2131
      t2796 = -0.64D2 * t2133 + 0.32D2 * t2142 - t2164 - t2575 + 0.64D2 
     #* t2175 - 0.352D3 * t2235 - 0.352D3 * t2252 - 0.32D2 * t2263 - 0.9
     #6D2 * t2275 - 0.48D2 * t2296 - 0.32D2 * t2299 + 0.32D2 * t2314 + 0
     #.80D2 * t2319 - 0.64D2 * t2323
      rrgg2ggh11J4 = -0.9D1 / 0.16D2 * (0.4D1 * wd * (t2325 + t532 + t14
     #61 + t621 + t1194 + t376 + t1943 + t1583 + t449 + t2060 + t713 + t
     #1342 + t1827 + t1769 + t2165 + t883 + t1264 + t798 + t1890 + t1711
     # + t1042 + t2226 + t1402 + t165 + t272 + t2277 + t1532 + t2114 + t
     #1119 + t1995 + t1647 + t950) + 0.3D1 * wd * (t2334 + t2411 + t2439
     # + t2446 + t2459 + t2465 + t2369 + t2386 + t2432 + t2569 + t2574 +
     # t2577 + t2582 + t2587 + t2340 + t2346 + t2348 + t2482 + t2500 + t
     #2503 + t2509 + t2521 + t2530 + t2537 + t2541 + t2545 + t2556 + t25
     #62 + t2565 + t2398 + t2422 + t2427) + 0.2D1 * wd * (t2608 + t2621 
     #+ t2632 + t2645 + t2657 + t2670 + t2681 + t2693 + t2709 + t2719 + 
     #t2733 + t2746 + t2760 + t2772 + t2783 + t2796)) / t27 / t219 / t80
     # / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh11J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t6 * t20 - t2 * t24
      t27 = t26 ** 2
      t28 = s ** 2
      t29 = t28 ** 2
      t30 = t29 * t28
      t31 = t27 * t30
      t34 = x2 * x3
      t35 = t10 * t7 * t4 + t34 + t19
      t38 = t23 * t10
      t40 = s - t2 * t6 * t35 - t2 * t38
      t41 = t31 * t40
      t42 = t1 ** 2
      t43 = t42 ** 2
      t44 = x1 ** 2
      t45 = t44 ** 2
      t46 = t43 * t45
      t47 = t4 ** 2
      t49 = 0.1D1 / t47 / t4
      t50 = t35 ** 2
      t51 = t50 * t35
      t53 = t46 * t49 * t51
      t55 = 0.2D1 * t41 * t53
      t56 = t26 * t30
      t57 = t40 ** 2
      t58 = t56 * t57
      t59 = t42 * t44
      t61 = t59 * t5 * t35
      t63 = 0.8D1 * t58 * t61
      t64 = t42 * t1
      t65 = t44 * x1
      t66 = t64 * t65
      t67 = 0.1D1 / t47
      t68 = t67 * t50
      t69 = t66 * t68
      t71 = 0.24D2 * t58 * t69
      t73 = 0.32D2 * t58 * t53
      t74 = t29 * s
      t75 = t27 * t74
      t76 = t75 * t57
      t78 = 0.176D3 * t76 * t61
      t80 = 0.38D2 * t76 * t69
      t81 = t5 * t20
      t82 = t59 * t81
      t84 = 0.8D1 * t41 * t82
      t85 = t20 ** 2
      t86 = t67 * t85
      t87 = t66 * t86
      t89 = 0.24D2 * t41 * t87
      t90 = t85 * t20
      t92 = t46 * t49 * t90
      t94 = 0.32D2 * t41 * t92
      t96 = 0.6D1 * t58 * t82
      t98 = 0.6D1 * t58 * t87
      t100 = 0.2D1 * t58 * t92
      t102 = 0.6D1 * t41 * t69
      t103 = t74 * z
      t104 = t103 * t27
      t105 = t57 * t42
      t106 = t23 ** 2
      t107 = x3 ** 2
      t108 = t106 * t107
      t111 = 0.32D2 * t104 * t105 * t108
      t112 = t10 ** 2
      t113 = t106 * t112
      t116 = 0.32D2 * t104 * t105 * t113
      t117 = t64 * x1
      t118 = t117 * t113
      t120 = 0.14D2 * t41 * t118
      t121 = t55 + t63 + t71 + t73 + t78 - t80 + t84 + t89 + t94 + t96 +
     # t98 + t100 + t102 + t111 + t116 + t120
      t122 = t42 * x1
      t125 = 0.6D1 * t41 * t122 * t38
      t126 = t43 * x1
      t127 = t106 * t23
      t128 = t112 * t10
      t130 = t126 * t127 * t128
      t131 = t41 * t130
      t132 = 0.42D2 * t131
      t133 = t76 * t130
      t134 = 0.32D2 * t133
      t135 = t76 * t118
      t136 = 0.5D1 * t135
      t137 = t117 * t108
      t139 = 0.14D2 * t58 * t137
      t142 = 0.6D1 * t58 * t122 * t24
      t143 = t107 * x3
      t145 = t126 * t127 * t143
      t146 = t58 * t145
      t147 = 0.42D2 * t146
      t148 = t76 * t145
      t149 = 0.32D2 * t148
      t150 = t76 * t137
      t151 = 0.5D1 * t150
      t155 = z + x1 * t7 * t1
      t156 = t23 * t155
      t157 = t5 * t40
      t160 = 0.2D1 * t31 * t1 * t156 * t157
      t162 = t5 * t57
      t163 = t156 * t162
      t165 = 0.260D3 * t75 * t1 * t163
      t168 = 0.2D1 * t56 * t1 * t163
      t170 = 0.6D1 * t41 * t61
      t172 = 0.176D3 * t76 * t82
      t174 = 0.38D2 * t76 * t87
      t175 = t64 * t23
      t176 = t31 * t175
      t177 = t155 * t49
      t178 = t40 * t44
      t180 = t177 * t178 * t50
      t182 = 0.14D2 * t176 * t180
      t183 = t42 * t106
      t184 = t31 * t183
      t185 = t155 * t5
      t186 = t40 * x3
      t189 = 0.8D1 * t184 * t185 * t186
      t190 = -t125 - t132 - t134 - t136 + t139 - t142 - t147 - t149 - t1
     #51 + t160 - t165 + t168 + t170 + t172 - t174 + t182 + t189
      t192 = t64 * t106
      t193 = t192 * t155
      t194 = t75 * t193
      t195 = t67 * t57
      t196 = x1 * t20
      t197 = t196 * x3
      t198 = t195 * t197
      t199 = t194 * t198
      t200 = 0.80D2 * t199
      t201 = t64 * t127
      t202 = t75 * t201
      t203 = t57 * x3
      t205 = t185 * t203 * t10
      t207 = 0.16D2 * t202 * t205
      t208 = t43 * t1
      t210 = t208 * t106 * t155
      t211 = t31 * t210
      t212 = t49 * t40
      t213 = t65 * t50
      t214 = t213 * x3
      t217 = 0.64D2 * t211 * t212 * t214
      t218 = t43 * t42
      t220 = t127 * t155
      t221 = t47 ** 2
      t222 = 0.1D1 / t221
      t223 = t220 * t222
      t225 = t57 * x2
      t227 = t20 * t35
      t228 = t227 * t10
      t231 = 0.48D2 * t56 * t218 * t223 * t225 * t65 * t228
      t233 = t218 * t106 * t155
      t234 = t56 * t233
      t235 = t222 * t57
      t236 = t45 * x2
      t237 = t236 * t50
      t240 = 0.38D2 * t234 * t235 * t237
      t241 = t43 * t106
      t243 = t155 * t67
      t244 = t57 * t44
      t246 = t243 * t244 * x2
      t247 = t75 * t241 * t246
      t249 = t75 * t183
      t250 = t57 * z
      t254 = 0.384D3 * t249 * t185 * t250 * x3
      t256 = t75 * t175 * t155
      t257 = t49 * t57
      t258 = z * t44
      t261 = t256 * t257 * t258 * t85
      t263 = t43 * t64
      t264 = t106 ** 2
      t265 = t263 * t264
      t266 = t265 * t155
      t267 = t56 * t266
      t268 = x2 ** 2
      t269 = t235 * t268
      t270 = t65 * t35
      t274 = 0.16D2 * t267 * t269 * t270 * t10
      t275 = t43 * t23
      t276 = t275 * t155
      t277 = t56 * t276
      t278 = t65 * t20
      t279 = t278 * t35
      t280 = t257 * t279
      t281 = t277 * t280
      t283 = t57 * t263
      t285 = t56 * t283 * t45
      t286 = t268 * t127
      t287 = t49 * t35
      t291 = 0.16D2 * t285 * t286 * t287 * t10
      t297 = t268 * t264 * t67 * x3 * t10
      t299 = 0.12D2 * t56 * t283 * t65 * t297
      t300 = t241 * t155
      t301 = t31 * t300
      t302 = t67 * t40
      t303 = t44 * t35
      t304 = t303 * x3
      t306 = t301 * t302 * t304
      t308 = t208 * t264
      t309 = t308 * t155
      t310 = t75 * t309
      t313 = x1 * x3 * t10
      t315 = t310 * t195 * x2 * t313
      t317 = x1 * t35
      t318 = t317 * t10
      t320 = t194 * t195 * t318
      t321 = 0.80D2 * t320
      t322 = t27 * t64
      t323 = t322 * t106
      t326 = t243 * t225 * x1
      t327 = t103 * t323 * t326
      t329 = -t200 + t207 - t217 - t231 + t240 - 0.1656D4 * t247 + t254 
     #- 0.32D2 * t261 + t274 - 0.133D3 * t281 + t291 - t299 - 0.133D3 * 
     #t306 + 0.96D2 * t315 - t321 + 0.1028D4 * t327
      t330 = z ** 2
      t331 = t30 * t330
      t332 = t27 * t40
      t335 = t44 * t5
      t336 = t20 * t23
      t337 = t336 * t10
      t338 = t335 * t337
      t340 = 0.64D2 * t331 * t332 * t64 * t338
      t341 = t43 * t127
      t342 = t341 * t155
      t343 = t31 * t342
      t344 = x1 * z
      t345 = t344 * t107
      t348 = 0.32D2 * t343 * t157 * t345
      t349 = t57 * t43
      t351 = t75 * t349 * x2
      t353 = t49 * t20
      t354 = t353 * t35
      t357 = 0.64D2 * t351 * t65 * t23 * t354
      t358 = t208 * t23
      t360 = t155 * t222
      t361 = t57 * t45
      t364 = t56 * t358 * t360 * t361 * t90
      t365 = 0.32D2 * t364
      t366 = t40 * t218
      t368 = t31 * t366 * t44
      t369 = x2 * t264
      t372 = t369 * t5 * t112 * x3
      t374 = 0.14D2 * t368 * t372
      t375 = t56 * t300
      t376 = t44 * t85
      t378 = t257 * t376 * x3
      t380 = 0.14D2 * t375 * t378
      t381 = t56 * t193
      t383 = 0.12D2 * t381 * t198
      t384 = t28 * s
      t385 = t29 * t384
      t386 = t26 * t385
      t387 = t386 * t233
      t388 = t222 * t40
      t389 = t388 * t237
      t391 = 0.32D2 * t387 * t389
      t395 = 0.48D2 * t285 * t286 * t287 * x3
      t396 = t57 * t208
      t397 = t56 * t396
      t398 = t23 * t49
      t399 = t398 * t85
      t400 = t236 * t399
      t402 = 0.32D2 * t397 * t400
      t403 = t218 * t127
      t404 = t403 * t155
      t405 = t56 * t404
      t406 = t268 * t65
      t408 = t235 * t406 * t35
      t410 = 0.64D2 * t405 * t408
      t411 = t208 * t127
      t412 = t411 * t155
      t413 = t56 * t412
      t414 = t257 * x2
      t415 = t414 * t304
      t417 = 0.48D2 * t413 * t415
      t418 = t31 * t233
      t420 = 0.1D1 / t221 / t4
      t421 = t420 * t40
      t422 = t421 * x2
      t424 = t45 * t85 * t35
      t427 = 0.6D1 * t418 * t422 * t424
      t428 = t57 * t218
      t430 = t56 * t428 * t65
      t431 = x2 * t127
      t432 = t431 * t67
      t434 = t35 * x3 * t10
      t437 = 0.48D2 * t430 * t432 * t434
      t438 = t195 * t304
      t439 = t375 * t438
      t441 = t75 * t404
      t442 = t406 * t20
      t444 = t441 * t235 * t442
      t446 = t56 * t210
      t447 = t257 * t65
      t450 = 0.64D2 * t446 * t447 * t228
      t451 = -t340 - t348 - t357 + t365 + t374 + t380 - t383 + t391 - t3
     #95 - t402 - t410 + t417 - t427 - t437 - 0.133D3 * t439 + 0.67D2 * 
     #t444 - t450
      t454 = t43 * t264
      t455 = t56 * t454
      t456 = t57 * t112
      t458 = t185 * t456 * x3
      t460 = 0.24D2 * t455 * t458
      t462 = t268 * x2
      t465 = t360 * t57 * t462 * t65
      t467 = 0.32D2 * t56 * t265 * t465
      t473 = 0.48D2 * t56 * t411 * t177 * t57 * t268 * t44
      t474 = t195 * z
      t476 = t194 * t474 * t197
      t478 = x2 * t20
      t479 = t478 * t10
      t482 = 0.16D2 * t405 * t447 * t479
      t483 = t26 * t64
      t486 = t243 * t57
      t487 = t196 * t10
      t490 = 0.64D2 * t331 * t483 * t106 * t486 * t487
      t492 = t243 * t40
      t493 = t317 * x3
      t496 = 0.64D2 * t331 * t323 * t492 * t493
      t497 = t75 * t300
      t498 = t303 * t10
      t499 = t195 * t498
      t500 = t497 * t499
      t503 = t257 * t376 * t10
      t505 = 0.64D2 * t497 * t503
      t506 = t75 * t342
      t507 = t195 * x1
      t510 = 0.64D2 * t506 * t507 * t434
      t512 = t31 * t366 * t65
      t513 = t67 * t20
      t517 = 0.8D1 * t512 * t431 * t513 * t107
      t521 = 0.176D3 * t267 * t257 * t406 * t10
      t522 = t26 * t57
      t525 = t35 * t23
      t526 = t525 * x3
      t527 = t335 * t526
      t529 = 0.64D2 * t331 * t522 * t64 * t527
      t530 = t75 * t105
      t531 = x1 * x2
      t532 = t23 * t5
      t533 = t532 * t330
      t536 = 0.384D3 * t530 * t531 * t533
      t537 = t56 * t175
      t539 = t177 * t244 * t85
      t541 = 0.14D2 * t537 * t539
      t542 = t42 * t23
      t543 = t56 * t542
      t544 = t57 * x1
      t548 = 0.6D1 * t543 * t243 * t544 * t20
      t549 = t460 - t467 + t473 + 0.64D2 * t476 - t482 - t490 - t496 - 0
     #.32D2 * t500 + t505 + t510 + t517 - t521 - t529 + t536 + t541 - t5
     #48
      t550 = t75 * t428
      t551 = t45 * t268
      t552 = t106 * t49
      t554 = t551 * t552 * t35
      t555 = t550 * t554
      t557 = t127 * t67
      t559 = t406 * t557 * x3
      t560 = t550 * t559
      t562 = t43 ** 2
      t563 = t264 * t23
      t565 = t562 * t563 * t155
      t567 = t462 * t65
      t571 = 0.8D1 * t56 * t565 * t235 * t567 * t10
      t572 = t375 * t499
      t575 = t243 * t244 * t20
      t576 = t537 * t575
      t578 = t56 * t192
      t580 = t185 * t544 * t10
      t581 = t578 * t580
      t586 = 0.48D2 * t343 * t302 * x1 * t434
      t587 = t75 * t412
      t588 = t44 * x2
      t591 = t587 * t195 * t588 * t10
      t593 = t75 * t275
      t594 = t57 * t65
      t596 = t177 * t594 * t50
      t597 = t593 * t596
      t599 = t386 * t300
      t600 = t588 * t330
      t601 = t302 * t600
      t603 = 0.384D3 * t599 * t601
      t604 = t31 * t341
      t605 = t40 * x1
      t608 = t604 * t185 * t605 * t107
      t611 = t406 * t557 * t10
      t612 = t550 * t611
      t615 = t551 * t552 * t20
      t616 = t550 * t615
      t618 = t75 * t349
      t619 = t20 * t106
      t621 = t335 * t619 * t112
      t623 = 0.64D2 * t618 * t621
      t624 = t44 * t106
      t625 = t513 * t10
      t628 = 0.48D2 * t351 * t624 * t625
      t629 = t317 * t107
      t631 = t343 * t302 * t629
      t632 = 0.120D3 * t631
      t633 = t56 * t342
      t634 = t162 * t313
      t635 = t633 * t634
      t637 = -0.205D3 * t555 + 0.67D2 * t560 + t571 - 0.96D2 * t572 - 0.
     #67D2 * t576 + 0.205D3 * t581 - t586 + 0.488D3 * t591 + 0.144D3 * t
     #597 + t603 - 0.48D2 * t608 + 0.67D2 * t612 - 0.205D3 * t616 + t623
     # - t628 - t632 - 0.133D3 * t635
      t640 = t31 * t40 * t562
      t641 = t45 * t462
      t642 = t264 * t49
      t644 = t641 * t642 * t10
      t646 = 0.6D1 * t640 * t644
      t647 = t44 * t20
      t648 = t647 * x3
      t649 = t195 * t648
      t650 = t375 * t649
      t653 = t127 * t49
      t654 = t641 * t653
      t655 = t75 * t283 * t654
      t657 = t40 * t208
      t658 = t31 * t657
      t659 = t658 * t400
      t664 = t31 * t308 * t185 * t605 * t128
      t665 = 0.32D2 * t664
      t667 = t478 * t35
      t668 = t388 * t45 * t667
      t669 = t418 * t668
      t670 = 0.80D2 * t669
      t671 = t31 * t404
      t672 = t212 * t65
      t674 = t672 * t478 * x3
      t676 = 0.16D2 * t671 * t674
      t677 = t396 * t65
      t678 = t75 * t677
      t679 = x2 * t106
      t680 = t67 * t35
      t681 = t680 * x3
      t682 = t679 * t681
      t683 = t678 * t682
      t687 = t176 * t243 * t178 * t20
      t689 = t31 * t192
      t692 = t689 * t185 * t605 * t10
      t694 = t57 * t107
      t695 = t185 * t694
      t697 = 0.38D2 * t202 * t695
      t698 = t75 * t175
      t701 = t698 * t177 * t244 * t50
      t702 = 0.5D1 * t701
      t703 = t647 * t10
      t704 = t195 * t703
      t705 = t375 * t704
      t707 = z * t20
      t708 = t707 * t35
      t710 = t277 * t447 * t708
      t712 = t40 * t43
      t715 = t5 * z
      t719 = 0.384D3 * t31 * t712 * t44 * t679 * t715 * t10
      t720 = t330 * z
      t722 = t27 * t57
      t724 = 0.128D3 * t74 * t720 * t722
      t725 = t698 * t539
      t726 = 0.5D1 * t725
      t727 = -t646 + 0.148D3 * t650 + 0.7D1 * t655 + 0.240D3 * t659 + t6
     #65 + t670 - t676 + 0.133D3 * t683 + 0.205D3 * t687 - 0.67D2 * t692
     # - t697 - t702 - 0.133D3 * t705 - 0.64D2 * t710 + t719 + t724 - t7
     #26
      t730 = t689 * t185 * t605 * x3
      t732 = t31 * t309
      t736 = t732 * t157 * x1 * t112 * x3
      t737 = 0.32D2 * t736
      t738 = t57 * t64
      t739 = t75 * t738
      t740 = t65 * z
      t743 = 0.32D2 * t739 * t740 * t68
      t749 = 0.48D2 * t31 * t411 * t177 * t40 * t268 * t44
      t751 = t195 * t196 * t112
      t753 = 0.32D2 * t506 * t751
      t755 = t263 * t127 * t155
      t756 = t56 * t755
      t757 = t420 * t57
      t758 = t551 * t85
      t761 = 0.14D2 * t756 * t757 * t758
      t767 = 0.384D3 * t56 * t349 * t44 * t679 * t715 * x3
      t768 = t385 * t330
      t769 = t768 * t404
      t771 = t49 * t65 * x2
      t773 = t771 * t203 * t20
      t774 = t769 * t773
      t775 = 0.16D2 * t774
      t776 = t587 * t415
      t778 = t31 * t275
      t779 = t40 * t65
      t781 = t177 * t779 * t50
      t782 = t778 * t781
      t784 = t768 * t233
      t785 = t222 * t45
      t787 = t785 * t225 * t85
      t788 = t784 * t787
      t789 = 0.8D1 * t788
      t790 = t75 * t542
      t794 = 0.768D3 * t790 * t185 * t544 * z
      t795 = x2 * t65
      t796 = t398 * t50
      t799 = 0.32D2 * t618 * t795 * t796
      t800 = t127 * t5
      t801 = t800 * t112
      t802 = t588 * t801
      t804 = 0.32D2 * t397 * t802
      t806 = 0.16D2 * t739 * t338
      t807 = t525 * t10
      t808 = t335 * t807
      t809 = t739 * t808
      t810 = 0.80D2 * t809
      t811 = t331 * t522
      t812 = t811 * t137
      t813 = 0.32D2 * t812
      t814 = 0.205D3 * t730 + t737 + t743 + t749 - t753 + t761 + t767 + 
     #t775 + 0.133D3 * t776 + 0.74D2 * t782 - t789 + t794 + t799 - t804 
     #+ t806 - t810 + t813
      t819 = 0.32D2 * t811 * t69
      t821 = t739 * t344 * t108
      t823 = t740 * t85
      t825 = t277 * t257 * t823
      t828 = t263 * t563 * t155
      t830 = t268 * t44
      t834 = 0.24D2 * t31 * t828 * t212 * t830 * t107
      t838 = t784 * t785 * x2 * t50 * t27
      t839 = 0.8D1 * t838
      t841 = t103 * t722 * t42
      t844 = 0.64D2 * t841 * t6 * t807
      t845 = t385 * z
      t846 = t845 * t26
      t849 = 0.64D2 * t846 * t233 * t668
      t850 = t26 * t218
      t852 = t845 * t850 * t106
      t853 = t360 * t40
      t854 = t236 * t85
      t857 = 0.32D2 * t852 * t853 * t854
      t858 = t588 * z
      t861 = 0.768D3 * t497 * t195 * t858
      t863 = t739 * t344 * t113
      t867 = 0.64D2 * t841 * t6 * t526
      t870 = 0.32D2 * t739 * t740 * t86
      t871 = t331 * t332
      t872 = t871 * t118
      t873 = 0.32D2 * t872
      t875 = 0.32D2 * t871 * t87
      t877 = t56 * t428 * t45
      t878 = t49 * t50
      t882 = 0.24D2 * t877 * t679 * t878 * t10
      t888 = 0.32D2 * t31 * t265 * t360 * t40 * t462 * t65
      t889 = t819 - 0.32D2 * t821 + 0.32D2 * t825 + t834 - t839 + t844 +
     # t849 + t857 + t861 - 0.32D2 * t863 - t867 + t870 + t873 + t875 + 
     #t882 - t888
      t890 = t44 * t50
      t891 = t890 * t10
      t894 = 0.14D2 * t301 * t212 * t891
      t896 = t218 * t563 * t155
      t901 = 0.32D2 * t31 * t896 * t302 * t531 * t143
      t902 = t31 * t755
      t905 = 0.6D1 * t902 * t421 * t758
      t906 = t75 * t396
      t907 = t800 * t107
      t908 = t588 * t907
      t909 = t906 * t908
      t911 = t236 * t796
      t912 = t906 * t911
      t914 = t103 * t722
      t917 = 0.32D2 * t914 * t59 * t68
      t918 = t31 * t266
      t919 = t388 * t268
      t923 = 0.48D2 * t918 * t919 * t270 * x3
      t925 = t218 * t264 * t155
      t926 = t75 * t925
      t928 = t257 * t830 * x3
      t929 = t926 * t928
      t931 = t40 * x2
      t935 = 0.32D2 * t689 * t243 * t931 * x1
      t936 = t906 * t400
      t938 = t906 * t802
      t940 = t397 * t908
      t943 = t497 * t257 * t891
      t944 = 0.32D2 * t943
      t946 = t641 * t642 * x3
      t948 = 0.6D1 * t640 * t946
      t949 = t40 * t263
      t950 = t31 * t949
      t951 = t264 * t67
      t953 = t406 * t951 * t107
      t955 = 0.6D1 * t950 * t953
      t956 = t31 * t193
      t959 = 0.48D2 * t956 * t302 * t493
      t960 = t31 * t201
      t961 = t186 * t10
      t964 = 0.16D2 * t960 * t185 * t961
      t965 = t894 + t901 + t905 - 0.74D2 * t909 + 0.48D2 * t912 + t917 -
     # t923 - 0.205D3 * t929 - t935 + 0.48D2 * t936 - 0.74D2 * t938 - 0.
     #128D3 * t940 - t944 + t948 + t955 - t959 + t964
      t969 = z * x3 * t10
      t971 = t343 * t157 * x1 * t969
      t975 = 0.32D2 * t618 * t795 * t399
      t976 = t562 * t264
      t977 = t976 * t155
      t978 = t31 * t977
      t979 = t641 * t35
      t982 = 0.6D1 * t978 * t421 * t979
      t987 = 0.8D1 * t31 * t565 * t388 * t567 * x3
      t989 = t243 * t244 * t35
      t990 = t698 * t989
      t992 = t75 * t192
      t993 = t992 * t580
      t995 = t31 * t454
      t999 = 0.32D2 * t995 * t185 * t40 * t143
      t1000 = t40 * t10
      t1003 = 0.6D1 * t184 * t185 * t1000
      t1004 = t768 * t218
      t1006 = t1004 * t220 * t49
      t1007 = t795 * t35
      t1008 = t26 * t40
      t1011 = t1006 * t1007 * t1008 * x3
      t1012 = 0.16D2 * t1011
      t1013 = t106 * t155
      t1019 = t1004 * t1013 * t222 * t236 * t35 * t1008 * t20
      t1020 = 0.16D2 * t1019
      t1021 = t302 * t44
      t1022 = t707 * x3
      t1025 = 0.64D2 * t301 * t1021 * t1022
      t1026 = t75 * t210
      t1027 = t795 * t50
      t1028 = t235 * t1027
      t1029 = t1026 * t1028
      t1032 = t56 * t57 * t562
      t1034 = 0.6D1 * t1032 * t644
      t1038 = 0.12D2 * t31 * t949 * t65 * t297
      t1040 = t40 * t45
      t1043 = t31 * t358 * t360 * t1040 * t51
      t1044 = 0.32D2 * t1043
      t1048 = 0.48D2 * t375 * t257 * t588 * t35
      t1050 = t406 * t951 * t112
      t1052 = 0.14D2 * t950 * t1050
      t1053 = -0.64D2 * t971 + t975 - t982 + t987 + 0.488D3 * t990 + 0.4
     #88D3 * t993 + t999 + t1003 - t1012 + t1020 - t1025 - 0.74D2 * t102
     #9 + t1034 - t1038 + t1044 + t1048 + t1052
      t1054 = t56 * t283
      t1055 = t45 * x1
      t1056 = t1055 * t268
      t1057 = t106 * t222
      t1061 = 0.24D2 * t1054 * t1056 * t1057 * t50
      t1062 = t336 * x3
      t1065 = 0.64D2 * t841 * t6 * t1062
      t1066 = x3 * t10
      t1069 = 0.64D2 * t914 * t183 * t1066
      t1070 = t551 * t50
      t1073 = 0.14D2 * t902 * t421 * t1070
      t1074 = t441 * t408
      t1076 = t56 * t201
      t1077 = t185 * t456
      t1079 = 0.24D2 * t1076 * t1077
      t1080 = t56 * t341
      t1082 = t185 * t544 * t107
      t1083 = t1080 * t1082
      t1086 = t301 * t302 * t703
      t1089 = t301 * t302 * t498
      t1091 = t757 * x2
      t1094 = 0.14D2 * t234 * t1091 * t424
      t1095 = t890 * x3
      t1097 = t301 * t212 * t1095
      t1098 = 0.104D3 * t1097
      t1101 = t331 * t322 * t23 * t180
      t1102 = 0.32D2 * t1101
      t1103 = t633 * t751
      t1104 = 0.120D3 * t1103
      t1105 = t846 * t404
      t1106 = t672 * t479
      t1108 = 0.64D2 * t1105 * t1106
      t1111 = 0.32D2 * t506 * t195 * t629
      t1112 = t75 * t454
      t1114 = t185 * t694 * t10
      t1116 = 0.32D2 * t1112 * t1114
      t1120 = t739 * t344 * t106 * x3 * t10
      t1121 = 0.64D2 * t1120
      t1122 = t1061 + t1065 - t1069 + t1073 + 0.67D2 * t1074 + t1079 + 0
     #.74D2 * t1083 - 0.133D3 * t1086 + 0.148D3 * t1089 + t1094 + t1098 
     #+ t1102 - t1104 - t1108 - t1111 - t1116 - t1121
      t1127 = 0.32D2 * t618 * t531 * t801
      t1128 = t418 * t389
      t1129 = 0.5D1 * t1128
      t1133 = 0.176D3 * t902 * t388 * t551 * t20
      t1136 = t587 * t195 * t588 * x3
      t1139 = t1026 * t257 * t1007
      t1141 = t45 * t20
      t1142 = t1141 * t50
      t1145 = 0.14D2 * t418 * t422 * t1142
      t1146 = t56 * t349
      t1147 = t45 * t49
      t1149 = t1147 * t85 * t35
      t1151 = 0.8D1 * t1146 * t1149
      t1152 = t31 * t712
      t1153 = t35 * t106
      t1155 = t335 * t1153 * t112
      t1157 = 0.14D2 * t1152 * t1155
      t1159 = t185 * t544 * t112
      t1160 = t1080 * t1159
      t1162 = t185 * t544
      t1163 = t790 * t1162
      t1167 = t512 * t431 * t513 * t112
      t1168 = 0.104D3 * t1167
      t1171 = t369 * t5 * t107 * t10
      t1173 = 0.6D1 * t368 * t1171
      t1174 = t537 * t989
      t1178 = t877 * t679 * t878 * x3
      t1179 = 0.120D3 * t1178
      t1181 = t335 * t1153 * t107
      t1182 = t1146 * t1181
      t1183 = 0.104D3 * t1182
      t1187 = 0.176D3 * t918 * t212 * t406 * x3
      t1188 = t1127 + t1129 - t1133 + 0.488D3 * t1136 + 0.488D3 * t1139 
     #+ t1145 + t1151 + t1157 - 0.48D2 * t1160 + 0.86D2 * t1163 + t1168 
     #- t1173 + 0.205D3 * t1174 - t1179 + t1183 - t1187
      t1189 = t405 * t773
      t1190 = 0.74D2 * t1189
      t1191 = t65 * t67
      t1192 = t50 * t23
      t1196 = 0.6D1 * t1152 * t1191 * t1192 * t10
      t1199 = 0.64D2 * t841 * t6 * t337
      t1201 = t20 * t10 * x3
      t1202 = t507 * t1201
      t1204 = 0.64D2 * t506 * t1202
      t1207 = t778 * t177 * t779 * t85
      t1209 = t1191 * t227
      t1211 = 0.16D2 * t739 * t1209
      t1212 = t56 * t275
      t1213 = t1212 * t596
      t1216 = t75 * t738 * t44
      t1218 = t1216 * t715 * t807
      t1220 = t31 * t412
      t1221 = t647 * t112
      t1224 = 0.64D2 * t1220 * t302 * t1221
      t1225 = x2 * z
      t1229 = 0.384D3 * t211 * t672 * t1225 * t20
      t1230 = t195 * t44
      t1234 = 0.384D3 * t413 * t1230 * t1225 * t10
      t1235 = x2 * t35
      t1239 = 0.384D3 * t446 * t447 * t1235 * z
      t1240 = t562 * t1
      t1241 = t1240 * t563
      t1243 = t155 * t420
      t1244 = t268 ** 2
      t1249 = 0.2D1 * t56 * t1241 * t1243 * t57 * t1244 * t45
      t1251 = t75 * t265 * t465
      t1253 = t349 * t65
      t1254 = t75 * t1253
      t1255 = t513 * t807
      t1257 = 0.64D2 * t1254 * t1255
      t1258 = t358 * t155
      t1261 = t56 * t1258 * t235 * t424
      t1262 = 0.32D2 * t1261
      t1263 = t213 * t10
      t1266 = 0.32D2 * t446 * t257 * t1263
      t1267 = -t1190 - t1196 - t1199 + t1204 - 0.48D2 * t1207 + t1211 - 
     #0.48D2 * t1213 + 0.64D2 * t1218 - t1224 - t1229 - t1234 - t1239 + 
     #t1249 + 0.7D1 * t1251 + t1257 + t1262 + t1266
      t1269 = t497 * t378
      t1270 = 0.32D2 * t1269
      t1272 = 0.32D2 * t1112 * t458
      t1276 = 0.6D1 * t633 * t195 * t196 * t107
      t1277 = t31 * t542
      t1281 = 0.6D1 * t1277 * t243 * t605 * t35
      t1283 = t360 * t594 * t90
      t1284 = t1212 * t1283
      t1285 = 0.42D2 * t1284
      t1286 = t65 * t85
      t1287 = t1286 * t10
      t1290 = 0.32D2 * t211 * t212 * t1287
      t1292 = t335 * t619 * t107
      t1294 = 0.14D2 * t1146 * t1292
      t1295 = t23 * t67
      t1299 = 0.64D2 * t1146 * t795 * t1295 * t20
      t1300 = t106 * t5
      t1304 = 0.48D2 * t1146 * t588 * t1300 * t10
      t1306 = t185 * t544 * x3
      t1307 = t992 * t1306
      t1309 = t740 * t50
      t1312 = 0.32D2 * t277 * t257 * t1309
      t1313 = t31 * t276
      t1316 = 0.32D2 * t1313 * t212 * t823
      t1318 = 0.38D2 * t202 * t1077
      t1323 = 0.32D2 * t331 * t483 * t220 * t162 * t112
      t1326 = 0.64D2 * t497 * t257 * t1095
      t1329 = 0.32D2 * t914 * t59 * t86
      t1330 = t698 * t575
      t1332 = -t1270 - t1272 - t1276 - t1281 - t1285 + t1290 + t1294 - t
     #1299 + t1304 + 0.488D3 * t1307 - t1312 - t1316 - t1318 + t1323 + t
     #1326 + t1329 + 0.488D3 * t1330
      t1334 = t647 * t35
      t1336 = t256 * t257 * z * t1334
      t1337 = 0.64D2 * t1336
      t1338 = t388 * t854
      t1340 = 0.38D2 * t418 * t1338
      t1341 = t344 * t112
      t1344 = 0.32D2 * t633 * t162 * t1341
      t1346 = t633 * t162 * t345
      t1349 = 0.6D1 * t1054 * t1050
      t1352 = t176 * t243 * t178 * t35
      t1355 = t343 * t157 * t313
      t1358 = t1147 * t20 * t50
      t1360 = 0.8D1 * t1152 * t1358
      t1362 = 0.32D2 * t1054 * t654
      t1363 = t106 * t67
      t1364 = t406 * t1363
      t1366 = 0.48D2 * t397 * t1364
      t1371 = t1055 * t1244 * t264 * t222
      t1373 = 0.2D1 * t56 * t57 * t1240 * t1371
      t1374 = t85 * t23
      t1378 = 0.6D1 * t1146 * t1191 * t1374 * x3
      t1379 = t397 * t911
      t1382 = t34 * t10
      t1383 = t1021 * t1382
      t1385 = 0.64D2 * t846 * t925 * t1383
      t1386 = t1235 * t10
      t1387 = t672 * t1386
      t1389 = 0.64D2 * t1105 * t1387
      t1392 = t633 * t162 * x1 * t969
      t1394 = z * t35
      t1395 = t1394 * t10
      t1398 = 0.64D2 * t375 * t1230 * t1395
      t1399 = -t1337 + t1340 - t1344 + 0.32D2 * t1346 + t1349 - 0.67D2 *
     # t1352 - 0.133D3 * t1355 + t1360 - t1362 + t1366 + t1373 - t1378 +
     # 0.240D3 * t1379 + t1385 - t1389 - 0.64D2 * t1392 - t1398
      t1405 = t587 * t414 * t498
      t1408 = 0.16D2 * t739 * t527
      t1409 = t31 * t925
      t1410 = t212 * x2
      t1411 = t303 * t107
      t1413 = t1409 * t1410 * t1411
      t1414 = 0.120D3 * t1413
      t1415 = t388 * x2
      t1416 = t1286 * x3
      t1419 = 0.8D1 * t671 * t1415 * t1416
      t1420 = t56 * t428
      t1422 = 0.48D2 * t1420 * t615
      t1423 = t56 * t738
      t1425 = 0.16D2 * t1423 * t1209
      t1427 = t1191 * t1374 * t10
      t1428 = t1152 * t1427
      t1429 = 0.120D3 * t1428
      t1430 = t618 * t1155
      t1431 = 0.32D2 * t1430
      t1433 = t1191 * t1192 * x3
      t1435 = 0.32D2 * t618 * t1433
      t1438 = t5 * x3 * t10
      t1441 = 0.64D2 * t351 * x1 * t127 * t1438
      t1442 = t1394 * x3
      t1444 = t375 * t1230 * t1442
      t1447 = t375 * t1230 * t1022
      t1448 = 0.64D2 * t1447
      t1450 = t768 * t241 * t246
      t1451 = 0.72D2 * t1450
      t1452 = t768 * t925
      t1453 = t67 * t44
      t1455 = t1453 * t225 * t107
      t1456 = t1452 * t1455
      t1457 = 0.8D1 * t1456
      t1459 = t185 * t605 * t112
      t1460 = t604 * t1459
      t1462 = t40 * t107
      t1465 = 0.24D2 * t960 * t185 * t1462
      t1466 = 0.133D3 * t1405 + t1408 - t1414 + t1419 + t1422 + t1425 - 
     #t1429 - t1431 - t1435 - t1441 - 0.64D2 * t1444 + t1448 - t1451 - t
     #1457 + 0.74D2 * t1460 + t1465
      t1470 = 0.24D2 * t995 * t185 * t1462 * t10
      t1471 = t40 * t112
      t1475 = 0.8D1 * t995 * t185 * t1471 * x3
      t1479 = 0.16D2 * t918 * t919 * t278 * x3
      t1481 = t1141 * t35
      t1484 = 0.12D2 * t756 * t757 * t268 * t1481
      t1488 = t227 * x3
      t1491 = 0.48D2 * t31 * t218 * t223 * t931 * t65 * t1488
      t1493 = t587 * t414 * t648
      t1496 = t1216 * t715 * t526
      t1499 = t1216 * t715 * t1062
      t1502 = 0.24D2 * t1146 * t1358
      t1503 = t707 * t10
      t1505 = t301 * t1021 * t1503
      t1507 = t513 * x3
      t1509 = t678 * t679 * t1507
      t1514 = t75 * t396 * t44 * t431 * t1438
      t1516 = t56 * t677
      t1517 = t1516 * t682
      t1523 = 0.24D2 * t56 * t828 * t257 * t830 * t112
      t1524 = t234 * t787
      t1525 = 0.5D1 * t1524
      t1527 = 0.16D2 * t1076 * t205
      t1530 = 0.64D2 * t446 * t257 * t1287
      t1531 = t1470 + t1475 + t1479 - t1484 - t1491 + 0.133D3 * t1493 + 
     #0.64D2 * t1496 + 0.64D2 * t1499 + t1502 - 0.64D2 * t1505 + 0.133D3
     # * t1509 - 0.148D3 * t1514 - 0.32D2 * t1517 + t1523 + t1525 + t152
     #7 - t1530
      t1535 = 0.32D2 * t413 * t195 * t1221
      t1538 = t31 * t1258 * t388 * t1142
      t1539 = 0.32D2 * t1538
      t1542 = t430 * t431 * t680 * t107
      t1543 = 0.104D3 * t1542
      t1546 = t360 * t1040 * t462
      t1547 = t386 * t976 * t1546
      t1552 = t386 * t403 * t177 * t779 * t268
      t1556 = 0.64D2 * t671 * t388 * t442
      t1560 = t1452 * t1453 * x2 * t27 * t112
      t1561 = 0.8D1 * t1560
      t1563 = t194 * t474 * t493
      t1566 = t1313 * t212 * t279
      t1570 = 0.48D2 * t351 * t624 * t681
      t1571 = t588 * t532
      t1573 = 0.32D2 * t1423 * t1571
      t1577 = t1006 * t795 * t26 * t1000 * t20
      t1578 = 0.16D2 * t1577
      t1579 = t56 * t925
      t1580 = t588 * t112
      t1583 = 0.38D2 * t1579 * t195 * t1580
      t1586 = t256 * t257 * t258 * t50
      t1590 = t331 * t483 * t23 * t539
      t1591 = 0.32D2 * t1590
      t1594 = 0.32D2 * t211 * t212 * t1416
      t1595 = t647 * t107
      t1598 = 0.32D2 * t1220 * t302 * t1595
      t1599 = t1535 + t1539 + t1543 - 0.240D3 * t1547 + 0.32D2 * t1552 -
     # t1556 - t1561 + 0.64D2 * t1563 - 0.133D3 * t1566 - t1570 - t1573 
     #- t1578 + t1583 - 0.32D2 * t1586 + t1591 + t1594 + t1598
      t1602 = t778 * t360 * t779 * t51
      t1603 = 0.42D2 * t1602
      t1607 = 0.2D1 * t995 * t185 * t40 * t128
      t1609 = t301 * t1021 * t1442
      t1613 = 0.64D2 * t1220 * t1021 * t1201
      t1618 = 0.260D3 * t56 * t976 * t360 * t361 * t462
      t1619 = t830 * t10
      t1621 = t926 * t257 * t1619
      t1624 = t31 * t366 * t45
      t1625 = t49 * t85
      t1629 = 0.24D2 * t1624 * t679 * t1625 * x3
      t1630 = t1055 * x2
      t1631 = t23 * t222
      t1635 = 0.32D2 * t1420 * t1630 * t1631 * t51
      t1636 = t264 * t5
      t1638 = t588 * t1636 * t128
      t1640 = 0.2D1 * t1420 * t1638
      t1642 = t301 * t1021 * t1395
      t1643 = 0.64D2 * t1642
      t1645 = t1313 * t672 * t708
      t1647 = t386 * t404
      t1648 = t1235 * x3
      t1649 = t672 * t1648
      t1651 = 0.48D2 * t1647 * t1649
      t1652 = t386 * t925
      t1653 = t588 * t107
      t1654 = t302 * t1653
      t1656 = 0.32D2 * t1652 * t1654
      t1659 = 0.6D1 * t960 * t185 * t1471
      t1661 = t845 * t850 * t264
      t1664 = 0.32D2 * t1661 * t492 * t1580
      t1665 = t641 * t20
      t1668 = 0.6D1 * t978 * t421 * t1665
      t1670 = 0.48D2 * t1423 * t527
      t1671 = -t1603 + t1607 - 0.64D2 * t1609 - t1613 + t1618 - 0.205D3 
     #* t1621 + t1629 + t1635 + t1640 + t1643 - 0.64D2 * t1645 - t1651 +
     # t1656 + t1659 + t1664 + t1668 - t1670
      t1675 = 0.32D2 * t658 * t908
      t1677 = 0.32D2 * t658 * t911
      t1679 = t194 * t474 * t318
      t1681 = t302 * t1580
      t1682 = t1409 * t1681
      t1683 = 0.5D1 * t1682
      t1687 = 0.8D1 * t430 * t431 * t680 * t112
      t1690 = 0.12D2 * t956 * t302 * t318
      t1694 = 0.384D3 * t375 * t414 * t258 * t20
      t1695 = t531 * t330
      t1698 = 0.384D3 * t956 * t302 * t1695
      t1702 = 0.32D2 * t202 * t185 * t250 * t112
      t1706 = 0.32D2 * t202 * t185 * t250 * t107
      t1708 = 0.48D2 * t633 * t1202
      t1709 = t375 * t503
      t1710 = 0.104D3 * t1709
      t1711 = t497 * t438
      t1713 = t303 * t112
      t1716 = 0.24D2 * t1579 * t414 * t1713
      t1718 = t185 * t57 * t10
      t1720 = 0.176D3 * t249 * t1718
      t1721 = t185 * t203
      t1723 = 0.176D3 * t249 * t1721
      t1724 = -t1675 - t1677 + 0.64D2 * t1679 + t1683 + t1687 - t1690 + 
     #t1694 - t1698 + t1702 + t1706 - t1708 + t1710 - 0.208D3 * t1711 + 
     #t1716 + t1720 + t1723
      t1725 = t56 * t309
      t1726 = t531 * t107
      t1727 = t195 * t1726
      t1729 = 0.32D2 * t1725 * t1727
      t1731 = 0.32D2 * t446 * t1028
      t1733 = 0.14D2 * t1054 * t953
      t1734 = t31 * t366
      t1738 = 0.32D2 * t1734 * t1630 * t1631 * t90
      t1741 = 0.48D2 * t1220 * t1410 * t703
      t1743 = t343 * t157 * t1341
      t1745 = t56 * t183
      t1747 = 0.6D1 * t1745 * t1721
      t1749 = 0.6D1 * t1076 * t695
      t1752 = 0.32D2 * t446 * t257 * t214
      t1753 = t30 * z
      t1754 = t27 * t43
      t1755 = t1754 * t106
      t1758 = t243 * t178 * x2
      t1759 = t1753 * t1755 * t1758
      t1762 = t26 * t43
      t1763 = t1762 * t106
      t1766 = 0.128D3 * t385 * t720 * t1763 * t1758
      t1772 = 0.384D3 * t75 * t1 * t23 * t185 * t57 * t330
      t1774 = 0.48D2 * t658 * t1364
      t1776 = 0.48D2 * t1579 * t928
      t1780 = 0.64D2 * t633 * t195 * t531 * x3
      t1781 = t75 * t341
      t1782 = t1781 * t1159
      t1784 = t680 * t10
      t1787 = 0.48D2 * t351 * t624 * t1784
      t1788 = -t1729 - t1731 + t1733 + t1738 + t1741 + 0.32D2 * t1743 + 
     #t1747 + t1749 + t1752 - 0.1028D4 * t1759 + t1766 + t1772 + t1774 +
     # t1776 - t1780 + 0.144D3 * t1782 + t1787
      t1791 = t588 * t1636 * t143
      t1792 = t1420 * t1791
      t1793 = 0.42D2 * t1792
      t1795 = t386 * t241 * t1758
      t1798 = t31 * t40 * t64
      t1799 = t588 * t533
      t1801 = 0.384D3 * t1798 * t1799
      t1802 = t1055 * t462
      t1803 = t127 * t222
      t1807 = 0.8D1 * t1032 * t1802 * t1803 * t35
      t1810 = 0.48D2 * t351 * t624 * t1507
      t1812 = 0.32D2 * t387 * t1338
      t1814 = 0.32D2 * t1652 * t1681
      t1817 = 0.384D3 * t76 * t3 * t330
      t1820 = 0.48D2 * t1409 * t212 * t1619
      t1822 = t301 * t302 * t648
      t1825 = t31 * t657 * t65
      t1826 = t679 * t625
      t1827 = t1825 * t1826
      t1830 = 0.48D2 * t1825 * t682
      t1836 = t1004 * t264 * t155 * t67 * t588 * t26 * t961
      t1837 = 0.16D2 * t1836
      t1838 = t236 * t51
      t1841 = 0.2D1 * t234 * t757 * t1838
      t1844 = 0.6D1 * t234 * t1091 * t1142
      t1845 = t235 * x2
      t1847 = t1026 * t1845 * t279
      t1850 = 0.32D2 * t578 * t326
      t1851 = -t1793 + 0.128D3 * t1795 - t1801 + t1807 + t1810 + t1812 +
     # t1814 + t1817 + t1820 - 0.96D2 * t1822 - 0.32D2 * t1827 + t1830 +
     # t1837 + t1841 - t1844 - 0.148D3 * t1847 - t1850
      t1853 = 0.48D2 * t1734 * t554
      t1857 = 0.64D2 * t1152 * t795 * t1295 * t35
      t1859 = t678 * t679 * t1784
      t1864 = 0.48D2 * t301 * t212 * t588 * t20
      t1868 = 0.64D2 * t343 * t302 * t531 * t10
      t1869 = t1152 * t621
      t1870 = 0.104D3 * t1869
      t1872 = 0.24D2 * t1152 * t1149
      t1873 = t1579 * t1455
      t1874 = 0.5D1 * t1873
      t1876 = t375 * t1230 * t1503
      t1881 = 0.384D3 * t618 * t830 * t1363 * z
      t1883 = x2 * t23
      t1886 = t914 * t64 * t44 * t1883 * t5
      t1889 = 0.32D2 * t618 * t1427
      t1891 = 0.64D2 * t1647 * t674
      t1894 = 0.8D1 * t405 * t1845 * t1263
      t1899 = 0.32D2 * t331 * t322 * t220 * t157 * t107
      t1901 = t194 * t474 * t487
      t1905 = 0.32D2 * t618 * t531 * t907
      t1906 = t1853 - t1857 + 0.133D3 * t1859 + t1864 - t1868 + t1870 + 
     #t1872 + t1874 - 0.64D2 * t1876 + t1881 + 0.1028D4 * t1886 - t1889 
     #- t1891 + t1894 + t1899 + 0.64D2 * t1901 + t1905
      t1913 = 0.176D3 * t756 * t235 * t551 * t35
      t1915 = 0.64D2 * t1647 * t1387
      t1918 = t1026 * t257 * t795 * t20
      t1922 = 0.64D2 * t413 * t1230 * t434
      t1923 = t506 * t634
      t1929 = 0.64D2 * t75 * t201 * t155 * t162 * t969
      t1930 = t195 * t487
      t1932 = 0.16D2 * t194 * t1930
      t1934 = 0.6D1 * t1032 * t946
      t1936 = t1277 * t185 * t605
      t1938 = t543 * t1162
      t1940 = t236 * t90
      t1943 = 0.2D1 * t418 * t421 * t1940
      t1945 = t418 * t421 * t1838
      t1946 = 0.42D2 * t1945
      t1950 = 0.24D2 * t950 * t1056 * t1057 * t85
      t1952 = t1579 * t414 * t1221
      t1953 = 0.120D3 * t1952
      t1955 = t75 * t276 * t280
      t1957 = t497 * t649
      t1959 = -t1913 - t1915 + 0.488D3 * t1918 - t1922 - 0.32D2 * t1923 
     #+ t1929 + t1932 - t1934 - 0.7D1 * t1936 - 0.7D1 * t1938 + t1943 - 
     #t1946 + t1950 - t1953 - 0.32D2 * t1955 - 0.32D2 * t1957
      t1960 = t56 * t977
      t1963 = 0.6D1 * t1960 * t757 * t1665
      t1968 = 0.32D2 * t56 * t896 * t195 * t531 * t128
      t1972 = 0.12D2 * t902 * t421 * t268 * t1481
      t1974 = 0.48D2 * t1798 * t338
      t1976 = 0.16D2 * t1798 * t1209
      t1979 = 0.16D2 * t405 * t447 * t1648
      t1981 = t31 * t949 * t45
      t1985 = 0.48D2 * t1981 * t286 * t353 * t10
      t1988 = t1624 * t679 * t1625 * t10
      t1989 = 0.120D3 * t1988
      t1991 = t177 * t594 * t85
      t1992 = t1212 * t1991
      t1994 = t414 * t703
      t1995 = t587 * t1994
      t1998 = t732 * t302 * t1726
      t2001 = t211 * t388 * t1027
      t2005 = 0.384D3 * t381 * t195 * t1695
      t2009 = t56 * t308 * t185 * t544 * t143
      t2010 = 0.32D2 * t2009
      t2013 = 0.32D2 * t413 * t195 * t1713
      t2017 = t75 * t403 * t177 * t594 * t268
      t2021 = 0.260D3 * t31 * t976 * t1546
      t2022 = -t1963 + t1968 - t1972 - t1974 + t1976 - t1979 - t1985 - t
     #1989 + 0.74D2 * t1992 + 0.133D3 * t1995 + 0.240D3 * t1998 - 0.128D
     #3 * t2001 - t2005 + t2010 + t2013 + 0.86D2 * t2017 + t2021
      t2027 = 0.48D2 * t1152 * t588 * t1300 * x3
      t2031 = 0.384D3 * t1220 * t1021 * t1225 * x3
      t2033 = t256 * t257 * t1334
      t2034 = 0.74D2 * t2033
      t2035 = t1409 * t1383
      t2036 = 0.80D2 * t2035
      t2038 = t671 * t1415 * t214
      t2039 = 0.104D3 * t2038
      t2043 = t769 * t771 * t35 * t27 * t10
      t2044 = 0.16D2 * t2043
      t2046 = t1753 * t1763 * t246
      t2048 = t658 * t802
      t2050 = t513 * t35
      t2053 = 0.64D2 * t914 * t59 * t2050
      t2054 = t680 * t1062
      t2056 = 0.64D2 * t1254 * t2054
      t2058 = 0.48D2 * t1652 * t1383
      t2060 = t1313 * t212 * t1309
      t2063 = t56 * t428 * t44
      t2065 = 0.14D2 * t2063 * t1171
      t2067 = 0.6D1 * t2063 * t372
      t2070 = 0.384D3 * t530 * t258 * t81
      t2073 = 0.384D3 * t530 * t335 * t1394
      t2077 = 0.32D2 * t455 * t185 * t57 * t128
      t2078 = t2027 - t2031 + t2034 + t2036 + t2039 + t2044 - 0.1028D4 *
     # t2046 - 0.128D3 * t2048 - t2053 + t2056 + t2058 + 0.32D2 * t2060 
     #+ t2065 - t2067 + t2070 + t2073 + t2077
      t2082 = 0.2D1 * t455 * t185 * t57 * t143
      t2084 = 0.8D1 * t1745 * t1718
      t2086 = 0.48D2 * t1516 * t1826
      t2088 = 0.16D2 * t671 * t1106
      t2089 = t671 * t1387
      t2090 = 0.74D2 * t2089
      t2093 = 0.384D3 * t599 * t302 * t858
      t2096 = 0.64D2 * t413 * t195 * t1411
      t2100 = t1725 * t162 * x1 * t107 * t10
      t2101 = 0.32D2 * t2100
      t2102 = t618 * t1292
      t2103 = 0.32D2 * t2102
      t2107 = 0.2D1 * t31 * t40 * t1240 * t1371
      t2109 = 0.32D2 * t1798 * t1571
      t2112 = 0.48D2 * t512 * t432 * t1201
      t2114 = 0.48D2 * t387 * t668
      t2116 = t530 * t531 * t532
      t2119 = t550 * t567 * t653
      t2121 = t497 * t704
      t2124 = t1220 * t1410 * t304
      t2126 = t2082 + t2084 + t2086 - t2088 - t2090 + t2093 - t2096 + t2
     #101 - t2103 + t2107 - t2109 - t2112 + t2114 - 0.240D3 * t2116 + 0.
     #128D3 * t2119 - 0.208D3 * t2121 - 0.32D2 * t2124
      t2129 = t413 * t1994
      t2132 = 0.32D2 * t950 * t654
      t2133 = t593 * t1991
      t2138 = 0.384D3 * t249 * t185 * t250 * t10
      t2140 = 0.12D2 * t1798 * t808
      t2141 = t335 * t1062
      t2143 = 0.12D2 * t1423 * t2141
      t2146 = 0.16D2 * t405 * t447 * t1386
      t2149 = t234 * t235 * t45 * t667
      t2150 = 0.80D2 * t2149
      t2151 = t593 * t1283
      t2152 = 0.32D2 * t2151
      t2153 = t1734 * t1638
      t2154 = 0.42D2 * t2153
      t2156 = 0.2D1 * t1734 * t1791
      t2159 = 0.32D2 * t1220 * t302 * t1411
      t2162 = t593 * t360 * t594 * t51
      t2163 = 0.32D2 * t2162
      t2165 = 0.16D2 * t671 * t1649
      t2167 = 0.8D1 * t455 * t1114
      t2171 = 0.2D1 * t31 * t40 * t1 * x1
      t2172 = -0.32D2 * t2129 - t2132 + 0.144D3 * t2133 + t2138 - t2140 
     #- t2143 - t2146 + t2150 - t2152 - t2154 + t2156 + t2159 - t2163 - 
     #t2165 + t2167 + t2171
      t2174 = t57 * t1 * x1
      t2176 = 0.260D3 * t75 * t2174
      t2178 = 0.2D1 * t56 * t2174
      t2181 = 0.32D2 * t852 * t853 * t237
      t2183 = 0.384D3 * t1423 * t1799
      t2184 = t795 * t85
      t2185 = t235 * t2184
      t2186 = t446 * t2185
      t2188 = t531 * t112
      t2189 = t195 * t2188
      t2190 = t1725 * t2189
      t2193 = t1216 * t715 * t337
      t2197 = 0.6D1 * t756 * t757 * t1070
      t2200 = 0.6D1 * t1960 * t757 * t979
      t2202 = t1579 * t1230 * t1382
      t2203 = 0.80D2 * t2202
      t2207 = 0.16D2 * t1981 * t286 * t353 * x3
      t2209 = 0.64D2 * t1420 * t611
      t2213 = t75 * t396 * t45 * t1883 * t354
      t2215 = t310 * t1727
      t2217 = t739 * t2141
      t2218 = 0.80D2 * t2217
      t2220 = 0.38D2 * t1409 * t1654
      t2223 = 0.32D2 * t211 * t388 * t2184
      t2224 = -t2176 + t2178 + t2181 - t2183 - 0.128D3 * t2186 + 0.240D3
     # * t2190 + 0.64D2 * t2193 + t2197 + t2200 + t2203 + t2207 - t2209 
     #+ 0.96D2 * t2213 + 0.48D2 * t2215 - t2218 + t2220 - t2223
      t2228 = 0.32D2 * t732 * t302 * t2188
      t2230 = 0.48D2 * t1647 * t1106
      t2233 = 0.32D2 * t1661 * t492 * t1653
      t2235 = 0.64D2 * t1105 * t1649
      t2237 = t405 * t1845 * t1287
      t2238 = 0.104D3 * t2237
      t2240 = 0.48D2 * t381 * t1930
      t2243 = 0.24D2 * t1409 * t1410 * t1595
      t2246 = 0.384D3 * t375 * t195 * t600
      t2248 = 0.64D2 * t1105 * t674
      t2252 = 0.384D3 * t301 * t1410 * t303 * z
      t2254 = 0.384D3 * t301 * t601
      t2260 = 0.2D1 * t31 * t1241 * t1243 * t40 * t1244 * t45
      t2262 = t234 * t757 * t1940
      t2263 = 0.42D2 * t2262
      t2266 = 0.64D2 * t739 * t740 * t2050
      t2270 = 0.6D1 * t343 * t302 * t317 * t112
      t2272 = t618 * t830 * t1363
      t2274 = t1781 * t1082
      t2276 = -t2228 - t2230 + t2233 - t2235 + t2238 - t2240 + t2243 - t
     #2246 - t2248 + t2252 - t2254 + t2260 - t2263 + t2266 - t2270 + 0.3
     #2D2 * t2272 + 0.144D3 * t2274
      t2277 = t678 * t1826
      t2281 = 0.16D2 * t194 * t195 * t493
      t2285 = 0.8D1 * t640 * t1802 * t1803 * t20
      t2289 = t768 * t1754 * t1013 * t1453 * x2
      t2290 = 0.72D2 * t2289
      t2292 = 0.64D2 * t1734 * t559
      t2293 = t578 * t1306
      t2296 = 0.32D2 * t618 * t1149
      t2298 = 0.32D2 * t618 * t1358
      t2299 = t1026 * t2185
      t2301 = t310 * t2189
      t2306 = 0.48D2 * t267 * t269 * t278 * t10
      t2307 = t1146 * t1433
      t2308 = 0.120D3 * t2307
      t2310 = 0.64D2 * t618 * t1181
      t2313 = 0.64D2 * t211 * t672 * t1488
      t2316 = t739 * x1 * t106 * t1066
      t2317 = 0.74D2 * t2316
      t2320 = 0.48D2 * t56 * t1253 * t2054
      t2324 = 0.48D2 * t31 * t712 * t65 * t1255
      t2325 = 0.133D3 * t2277 + t2281 + t2285 - t2290 - t2292 - 0.67D2 *
     # t2293 - t2296 - t2298 - 0.74D2 * t2299 + 0.48D2 * t2301 - t2306 -
     # t2308 + t2310 - t2313 + t2317 - t2320 - t2324
      t2333 = -t55 - t63 - t71 - t73 - t78 + t80 - t84 - t89 - t94 - t96
     # - t98 - t100 - t102 - t111 - t116 - t120 + t125
      t2334 = t132 + t134 + t136 - t139 + t142 + t147 + t149 + t151 - t1
     #60 + t165 - t168 - t170 - t172 + t174 - t182 - t189 + t200
      t2342 = -t207 + t217 + t231 - t240 + 0.984D3 * t247 - t254 + 0.48D
     #2 * t261 - t274 + 0.30D2 * t281 - t291 + t299 + 0.30D2 * t306 - 0.
     #240D3 * t315 + t321 - 0.1508D4 * t327 + t340 + t348
      t2345 = t357 - t365 - t374 - t380 + t383 - t391 + t395 + t402 + t4
     #10 - t417 + t427 + t437 + 0.30D2 * t439 + 0.12D2 * t444 + t450 - t
     #460 + t467
      t2349 = 0.64D2 * t500
      t2352 = -t473 - 0.80D2 * t476 + t482 + t490 + t496 - t2349 - t505 
     #- t510 - t517 + t521 + t529 - t536 - t541 + t548 + 0.102D3 * t555 
     #+ 0.12D2 * t560 - t571
      t2364 = 0.240D3 * t572 - 0.12D2 * t576 - 0.102D3 * t581 + t586 - 0
     #.597D3 * t591 - 0.192D3 * t597 - t603 + 0.120D3 * t608 + 0.12D2 * 
     #t612 + 0.102D3 * t616 - t623 + t628 + t632 + 0.30D2 * t635 + t646 
     #- 0.80D2 * t650 - 0.52D2 * t655
      t2373 = -0.288D3 * t659 - t665 - t670 + t676 - 0.30D2 * t683 - 0.1
     #02D3 * t687 - 0.12D2 * t692 + t697 + t702 + 0.30D2 * t705 + 0.80D2
     # * t710 - t719 - t724 + t726 - 0.102D3 * t730 - t737 - t743
      t2380 = -t749 + t753 - t761 - t767 - 0.32D2 * t774 - 0.30D2 * t776
     # - 0.40D2 * t782 + 0.16D2 * t788 - t794 - t799 + t804 - t806 + t81
     #0 - t813 - t819 + 0.48D2 * t821 - 0.48D2 * t825
      t2387 = -t834 + 0.16D2 * t838 - t844 - t849 - t857 - t861 + 0.48D2
     # * t863 + t867 - t870 - t873 - t875 - t882 + t888 - t894 - t901 - 
     #t905 + 0.40D2 * t909
      t2394 = -0.120D3 * t912 - t917 + t923 + 0.102D3 * t929 + t935 - 0.
     #120D3 * t936 + 0.40D2 * t938 + 0.80D2 * t940 + t944 - t948 - t955 
     #+ t959 - t964 + 0.80D2 * t971 - t975 + t982 - t987
      t2401 = -0.597D3 * t990 - 0.597D3 * t993 - t999 - t1003 + 0.32D2 *
     # t1011 - 0.32D2 * t1019 + t1025 + 0.40D2 * t1029 - t1034 + t1038 -
     # t1044 - t1048 - t1052 - t1061 - t1065 + t1069 - t1073
      t2408 = 0.12D2 * t1074 - t1079 - 0.40D2 * t1083 + 0.30D2 * t1086 -
     # 0.80D2 * t1089 - t1094 - t1098 - t1102 + t1104 + t1108 + t1111 + 
     #t1116 + 0.32D2 * t1120 - t1127 - t1129 + t1133 - 0.597D3 * t1136
      t2416 = -0.597D3 * t1139 - t1145 - t1151 - t1157 + 0.120D3 * t1160
     # - 0.176D3 * t1163 - t1168 + t1173 - 0.102D3 * t1174 + t1179 - t11
     #83 + t1187 + t1190 + t1196 + t1199 - t1204 + 0.120D3 * t1207
      t2420 = -t1211 + 0.120D3 * t1213 - 0.80D2 * t1218 + t1224 + t1229 
     #+ t1234 + t1239 - t1249 - 0.52D2 * t1251 - t1257 - t1262 - t1266 +
     # t1270 + t1272 + t1276 + t1281 + t1285
      t2426 = -t1290 - t1294 + t1299 - t1304 - 0.597D3 * t1307 + t1312 +
     # t1316 + t1318 - t1323 - t1326 - t1329 - 0.597D3 * t1330 + 0.32D2 
     #* t1336 - t1340 + t1344 - 0.48D2 * t1346 - t1349
      t2432 = -0.12D2 * t1352 + 0.30D2 * t1355 - t1360 + t1362 - t1366 -
     # t1373 + t1378 - 0.288D3 * t1379 - t1385 + t1389 + 0.80D2 * t1392 
     #+ t1398 - 0.30D2 * t1405 - t1408 + t1414 - t1419 - t1422
      t2443 = -t1425 + t1429 + t1431 + t1435 + t1441 + 0.48D2 * t1444 - 
     #0.32D2 * t1447 + 0.144D3 * t1450 + 0.16D2 * t1456 - 0.40D2 * t1460
     # - t1465 - t1470 - t1475 - t1479 + t1484 + t1491 - 0.30D2 * t1493
      t2452 = -0.48D2 * t1496 - 0.80D2 * t1499 - t1502 + 0.48D2 * t1505 
     #- 0.30D2 * t1509 + 0.80D2 * t1514 + 0.128D3 * t1517 - t1523 - t152
     #5 - t1527 + t1530 - t1535 - t1539 - t1543 + 0.288D3 * t1547 - 0.12
     #8D3 * t1552 + t1556
      t2461 = 0.16D2 * t1560 - 0.48D2 * t1563 + 0.30D2 * t1566 + t1570 +
     # t1573 + 0.32D2 * t1577 - t1583 + 0.48D2 * t1586 - t1591 - t1594 -
     # t1598 + t1603 - t1607 + 0.48D2 * t1609 + t1613 - t1618 + 0.102D3 
     #* t1621
      t2465 = -t1629 - t1635 - t1640 - 0.32D2 * t1642 + 0.80D2 * t1645 +
     # t1651 - t1656 - t1659 - t1664 - t1668 + t1670 + t1675 + t1677 - 0
     #.80D2 * t1679 - t1683 - t1687 + t1690
      t2470 = -t1694 + t1698 - t1702 - t1706 + t1708 - t1710 + 0.112D3 *
     # t1711 - t1716 - t1720 - t1723 + t1729 + t1731 - t1733 - t1738 - t
     #1741 - 0.48D2 * t1743 - t1747
      t2474 = -t1749 - t1752 + 0.1508D4 * t1759 - t1766 - t1772 - t1774 
     #- t1776 + t1780 - 0.192D3 * t1782 - t1787 + t1793 - 0.80D2 * t1795
     # + t1801 - t1807 - t1810 - t1812 - t1814
      t2481 = -t1817 - t1820 + 0.240D3 * t1822 + 0.128D3 * t1827 - t1830
     # - 0.32D2 * t1836 - t1841 + t1844 + 0.80D2 * t1847 + t1850 - t1853
     # + t1857 - 0.30D2 * t1859 - t1864 + t1868 - t1870 - t1872
      t2486 = 0.64D2 * t1923
      t2487 = -t1874 + 0.48D2 * t1876 - t1881 - 0.1508D4 * t1886 + t1889
     # + t1891 - t1894 - t1899 - 0.48D2 * t1901 - t1905 + t1913 + t1915 
     #- 0.597D3 * t1918 + t1922 - t2486 - t1929 - t1932
      t2493 = 0.64D2 * t1955
      t2494 = 0.64D2 * t1957
      t2495 = t1934 + 0.52D2 * t1936 + 0.52D2 * t1938 - t1943 + t1946 - 
     #t1950 + t1953 - t2493 - t2494 + t1963 - t1968 + t1972 + t1974 - t1
     #976 + t1979 + t1985 + t1989
      t2504 = -0.40D2 * t1992 - 0.30D2 * t1995 - 0.288D3 * t1998 + 0.80D
     #2 * t2001 + t2005 - t2010 - t2013 - 0.176D3 * t2017 - t2021 - t202
     #7 + t2031 - t2034 - t2036 - t2039 - 0.32D2 * t2043 + 0.1508D4 * t2
     #046 + 0.80D2 * t2048
      t2507 = t2053 - t2056 - t2058 - 0.48D2 * t2060 - t2065 + t2067 - t
     #2070 - t2073 - t2077 - t2082 - t2084 - t2086 + t2088 + t2090 - t20
     #93 + t2096 - t2101
      t2514 = t2103 - t2107 + t2109 + t2112 - t2114 + 0.288D3 * t2116 - 
     #0.80D2 * t2119 + 0.112D3 * t2121 + 0.128D3 * t2124 + 0.128D3 * t21
     #29 + t2132 - 0.192D3 * t2133 - t2138 + t2140 + t2143 + t2146 - t21
     #50
      t2520 = t2152 + t2154 - t2156 - t2159 + t2163 + t2165 - t2167 - t2
     #171 + t2176 - t2178 - t2181 + t2183 + 0.80D2 * t2186 - 0.288D3 * t
     #2190 - 0.48D2 * t2193 - t2197 - t2200
      t2523 = -t2203 - t2207 + t2209 - 0.240D3 * t2213 - 0.120D3 * t2215
     # + t2218 - t2220 + t2223 + t2228 + t2230 - t2233 + t2235 - t2238 +
     # t2240 - t2243 + t2246 + t2248
      t2526 = t27 * t26 * t43
      t2530 = 0.16D2 * t103 * t2526 * t23 * t781
      t2534 = 0.16D2 * t103 * t2526 * t127 * t1459
      t2537 = t57 * t40
      t2542 = 0.16D2 * t103 * t1762 * t127 * t185 * t2537 * x1 * t107
      t2548 = 0.32D2 * t103 * t1754 * t127 * t185 * t57 * t313
      t2549 = t103 * t1755
      t2552 = 0.32D2 * t2549 * t486 * t703
      t2557 = 0.32D2 * t103 * t1763 * t243 * t2537 * t648
      t2565 = 0.16D2 * t103 * t1762 * t23 * t177 * t2537 * t65 * t85
      t2570 = 0.32D2 * t103 * t2526 * t106 * t492 * t498
      t2573 = 0.32D2 * t2549 * t486 * t304
      t2579 = 0.32D2 * t103 * t1754 * t23 * t177 * t57 * t279
      t2580 = -t2252 + t2254 + t2530 + t2534 + t2542 - t2260 + t2263 - t
     #2548 + t2552 - t2557 - t2266 + t2270 - 0.128D3 * t2272 + t2565 - t
     #2570 + t2573 - t2579
      t2587 = -0.192D3 * t2274 - 0.30D2 * t2277 - t2281 - t2285 + 0.144D
     #3 * t2289 + t2292 - 0.12D2 * t2293 + t2296 + t2298 + 0.40D2 * t229
     #9 - 0.120D3 * t2301 + t2306 + t2308 - t2310 + t2313 - t2317 + t232
     #0 + t2324
      t2608 = -0.32D2 * t131 - 0.64D2 * t133 + 0.32D2 * t135 - 0.32D2 * 
     #t146 - 0.64D2 * t148 + 0.32D2 * t150 - 0.16D2 * t199 + 0.864D3 * t
     #247 - 0.96D2 * t261 - 0.64D2 * t281 - 0.32D2 * t306 - 0.16D2 * t32
     #0 + 0.288D3 * t327
      t2620 = 0.64D2 * t364 - 0.32D2 * t439 + 0.16D2 * t444 + 0.16D2 * t
     #476 - t482 - t2349 - 0.48D2 * t555 + 0.16D2 * t560 - 0.16D2 * t576
     # + 0.48D2 * t581 + 0.48D2 * t591 + 0.32D2 * t597 + 0.16D2 * t612
      t2635 = -0.48D2 * t616 - 0.32D2 * t631 - 0.64D2 * t635 + 0.256D3 *
     # t650 + 0.80D2 * t655 + 0.176D3 * t659 + 0.64D2 * t664 + 0.16D2 * 
     #t669 + 0.32D2 * t683 + 0.48D2 * t687 - 0.16D2 * t692 + 0.32D2 * t7
     #01 - 0.32D2 * t705
      t2646 = -0.16D2 * t710 + 0.32D2 * t725 + 0.48D2 * t730 + 0.64D2 * 
     #t736 + t775 + 0.32D2 * t776 + 0.64D2 * t782 - t789 + t806 - 0.16D2
     # * t809 + 0.64D2 * t812 - 0.96D2 * t821 + 0.96D2 * t825 - t839
      t2660 = -0.96D2 * t863 + 0.64D2 * t872 - 0.64D2 * t909 - 0.48D2 * 
     #t929 - 0.64D2 * t938 + 0.176D3 * t940 - 0.64D2 * t943 - 0.16D2 * t
     #971 + 0.48D2 * t990 + 0.48D2 * t993 - t1012 + t1020 - 0.64D2 * t10
     #29
      t2673 = 0.64D2 * t1043 + 0.16D2 * t1074 + 0.64D2 * t1083 - 0.32D2 
     #* t1086 + 0.256D3 * t1089 + 0.64D2 * t1097 + 0.64D2 * t1101 - 0.32
     #D2 * t1103 - t1121 - 0.32D2 * t1128 + 0.48D2 * t1136 + 0.48D2 * t1
     #139 + 0.160D3 * t1163
      t2687 = 0.64D2 * t1167 + 0.48D2 * t1174 - 0.32D2 * t1178 + 0.64D2 
     #* t1182 - 0.64D2 * t1189 + 0.16D2 * t1218 + 0.80D2 * t1251 + 0.64D
     #2 * t1261 - 0.64D2 * t1269 - 0.32D2 * t1284 + 0.48D2 * t1307 + 0.4
     #8D2 * t1330 - t1337
      t2698 = 0.96D2 * t1346 - 0.16D2 * t1352 - 0.64D2 * t1355 + 0.176D3
     # * t1379 - 0.16D2 * t1392 + 0.64D2 * t1405 + t1408 - 0.32D2 * t141
     #3 - 0.32D2 * t1428 - 0.64D2 * t1430 + 0.16D2 * t1444 + t1448 - t14
     #51 - t1457
      t2715 = 0.64D2 * t1460 + 0.64D2 * t1493 - 0.16D2 * t1496 + 0.16D2 
     #* t1499 + 0.16D2 * t1505 + 0.64D2 * t1509 - 0.256D3 * t1514 - 0.35
     #2D3 * t1517 - 0.32D2 * t1524 + 0.64D2 * t1538 + 0.64D2 * t1542 - 0
     #.176D3 * t1547 + 0.352D3 * t1552
      t2726 = -t1561 - 0.16D2 * t1563 - 0.64D2 * t1566 - t1578 - 0.96D2 
     #* t1586 + 0.64D2 * t1590 - 0.32D2 * t1602 + 0.16D2 * t1609 - 0.48D
     #2 * t1621 + t1643 - 0.16D2 * t1645 + 0.16D2 * t1679 - 0.32D2 * t16
     #82
      t2740 = 0.64D2 * t1709 - 0.192D3 * t1711 + 0.96D2 * t1743 - 0.288D
     #3 * t1759 + 0.32D2 * t1782 - 0.32D2 * t1792 - 0.176D3 * t1795 - 0.
     #352D3 * t1827 + t1837 - 0.256D3 * t1847 + 0.64D2 * t1859 + 0.64D2 
     #* t1869 - 0.32D2 * t1873
      t2750 = 0.16D2 * t1876 + 0.288D3 * t1886 - 0.16D2 * t1901 + 0.48D2
     # * t1918 - t2486 + t1932 - 0.80D2 * t1936 - 0.80D2 * t1938 - 0.32D
     #2 * t1945 - 0.32D2 * t1952 - t2493 - t2494 - t1979 - 0.32D2 * t198
     #8
      t2765 = 0.64D2 * t1992 + 0.32D2 * t1995 + 0.176D3 * t1998 + 0.176D
     #3 * t2001 + 0.64D2 * t2009 + 0.160D3 * t2017 + 0.64D2 * t2033 + 0.
     #16D2 * t2035 + 0.64D2 * t2038 + t2044 - 0.288D3 * t2046 + 0.176D3 
     #* t2048 + 0.96D2 * t2060
      t2779 = -t2088 - 0.64D2 * t2089 + 0.64D2 * t2100 - 0.64D2 * t2102 
     #- 0.176D3 * t2116 - 0.176D3 * t2119 - 0.192D3 * t2121 - 0.352D3 * 
     #t2124 - 0.352D3 * t2129 + 0.32D2 * t2133 + 0.16D2 * t2149 - 0.64D2
     # * t2151 - 0.32D2 * t2153 - 0.64D2 * t2162
      t2788 = -t2165 + 0.176D3 * t2186 + 0.176D3 * t2190 - 0.16D2 * t219
     #3 + 0.16D2 * t2202 - 0.16D2 * t2217 + 0.64D2 * t2237 - t2530 - t25
     #34 - t2542 - 0.32D2 * t2262 + t2548 - t2552
      t2796 = t2557 + 0.352D3 * t2272 - t2565 + t2570 - t2573 + t2579 + 
     #0.32D2 * t2274 + 0.32D2 * t2277 + t2281 - t2290 - 0.16D2 * t2293 -
     # 0.64D2 * t2299 - 0.32D2 * t2307 + 0.64D2 * t2316
      rrgg2ggh11J5 = -0.9D1 / 0.16D2 * (0.5D1 * wd * (t1959 + t1599 + t8
     #89 + t2276 + t1332 + t2172 + t1267 + t637 + t451 + t2325 + t121 + 
     #t1053 + t1906 + t329 + t2078 + t1671 + t814 + t549 + t2126 + t1851
     # + t1788 + t1531 + t1399 + t2022 + t1724 + t1466 + t2224 + t1122 +
     # t190 + t965 + t727 + t1188) + 0.4D1 * wd * (t2387 + t2580 + t2520
     # + t2364 + t2342 + t2504 + t2507 + t2481 + t2334 + t2420 + t2495 +
     # t2474 + t2452 + t2470 + t2587 + t2461 + t2487 + t2465 + t2416 + t
     #2380 + t2333 + t2432 + t2426 + t2352 + t2345 + t2373 + t2523 + t23
     #94 + t2443 + t2401 + t2408 + t2514) + 0.3D1 * wd * (t2608 + t2620 
     #+ t2635 + t2646 + t2660 + t2673 + t2687 + t2698 + t2715 + t2726 + 
     #t2740 + t2750 + t2765 + t2779 + t2788 + t2796)) / t27 / t384 / t57
     # / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh11J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t23 = 0.1D1 - x1
      t24 = t23 * x3
      t26 = s - t2 * t6 * t20 - t2 * t24
      t27 = t26 ** 2
      t28 = s ** 2
      t29 = t28 ** 2
      t30 = t29 * t28
      t31 = t27 * t30
      t32 = t1 ** 2
      t33 = t32 * t1
      t34 = t32 ** 2
      t35 = t34 * t33
      t36 = t23 ** 2
      t37 = t36 ** 2
      t38 = t37 * t23
      t42 = z + x1 * t7 * t1
      t43 = t35 * t38 * t42
      t45 = t4 ** 2
      t47 = 0.1D1 / t45 / t4
      t50 = x2 * x3
      t51 = t10 * t7 * t4 + t50 + t19
      t54 = t23 * t10
      t56 = s - t2 * t6 * t51 - t2 * t54
      t57 = t47 * t56
      t58 = x2 ** 2
      t59 = x1 ** 2
      t60 = t58 * t59
      t61 = x3 ** 2
      t66 = t34 ** 2
      t68 = t66 * t38 * t42
      t70 = t45 ** 2
      t71 = 0.1D1 / t70
      t72 = t71 * t56
      t73 = t58 * x2
      t74 = t59 * x1
      t75 = t73 * t74
      t80 = t29 * s
      t81 = t80 * z
      t82 = t81 * t27
      t83 = t56 ** 2
      t84 = t83 * t32
      t85 = t10 ** 2
      t86 = t36 * t85
      t90 = t31 * t56
      t91 = t33 * x1
      t92 = t91 * t86
      t95 = t32 * x1
      t99 = t34 * x1
      t100 = t36 * t23
      t101 = t85 * t10
      t103 = t99 * t100 * t101
      t104 = t90 * t103
      t106 = t27 * t80
      t107 = t106 * t83
      t108 = t107 * t103
      t110 = t107 * t92
      t112 = t26 * t30
      t113 = t112 * t83
      t114 = t36 * t61
      t115 = t91 * t114
      t121 = t61 * x3
      t123 = t99 * t100 * t121
      t124 = t113 * t123
      t126 = t107 * t123
      t128 = t107 * t115
      t131 = t23 * t42
      t132 = t5 * t56
      t137 = t5 * t83
      t138 = t131 * t137
      t144 = t32 * t59
      t146 = t144 * t5 * t51
      t149 = -0.24D2 * t31 * t43 * t57 * t60 * t61 - 0.8D1 * t31 * t68 *
     # t72 * t75 * x3 - 0.32D2 * t82 * t84 * t86 - 0.14D2 * t90 * t92 + 
     #0.6D1 * t90 * t95 * t54 + 0.42D2 * t104 + 0.32D2 * t108 + 0.5D1 * 
     #t110 - 0.14D2 * t113 * t115 + 0.6D1 * t113 * t95 * t24 + 0.42D2 * 
     #t124 + 0.32D2 * t126 + 0.5D1 * t128 - 0.2D1 * t31 * t1 * t131 * t1
     #32 + 0.260D3 * t106 * t1 * t138 - 0.2D1 * t112 * t1 * t138 - 0.6D1
     # * t90 * t146
      t150 = t33 * t74
      t151 = 0.1D1 / t45
      t152 = t51 ** 2
      t153 = t151 * t152
      t154 = t150 * t153
      t157 = t59 ** 2
      t158 = t34 * t157
      t159 = t152 * t51
      t161 = t158 * t47 * t159
      t174 = t5 * t20
      t175 = t144 * t174
      t178 = t20 ** 2
      t179 = t151 * t178
      t180 = t150 * t179
      t183 = t178 * t20
      t185 = t158 * t47 * t183
      t197 = t33 * t23
      t198 = t112 * t197
      t199 = t42 * t151
      t200 = t83 * t59
      t202 = t199 * t200 * t20
      t203 = t198 * t202
      t205 = t33 * t36
      t206 = t112 * t205
      t207 = t42 * t5
      t208 = t83 * x1
      t210 = t207 * t208 * t10
      t211 = t206 * t210
      t213 = t28 * s
      t214 = t29 * t213
      t215 = t26 * t214
      t216 = t34 * t32
      t217 = t216 * t100
      t218 = t217 * t42
      t219 = t215 * t218
      t220 = t57 * t74
      t221 = x2 * t51
      t222 = t221 * x3
      t223 = t220 * t222
      t226 = -0.6D1 * t90 * t154 - 0.2D1 * t90 * t161 - 0.8D1 * t113 * t
     #146 - 0.24D2 * t113 * t154 - 0.32D2 * t113 * t161 - 0.176D3 * t107
     # * t146 + 0.38D2 * t107 * t154 - 0.8D1 * t90 * t175 - 0.24D2 * t90
     # * t180 - 0.32D2 * t90 * t185 - 0.6D1 * t113 * t175 - 0.6D1 * t113
     # * t180 - 0.2D1 * t113 * t185 - 0.32D2 * t82 * t84 * t114 - 0.12D2
     # * t203 - 0.102D3 * t211 + 0.48D2 * t219 * t223
      t228 = t34 * t1
      t229 = t228 * t100
      t230 = t229 * t42
      t231 = t106 * t230
      t232 = t47 * t83
      t233 = t232 * x2
      t234 = t59 * t20
      t235 = t234 * x3
      t237 = t231 * t233 * t235
      t240 = t228 * t36 * t42
      t241 = t106 * t240
      t242 = t71 * t83
      t243 = t242 * x2
      t244 = t74 * t20
      t245 = t244 * t51
      t247 = t241 * t243 * t245
      t249 = z ** 2
      t250 = t30 * t249
      t251 = t27 * t33
      t254 = t42 * t47
      t255 = t56 * t59
      t257 = t254 * t255 * t152
      t258 = t250 * t251 * t23 * t257
      t262 = t59 * t51
      t263 = t262 * x3
      t264 = t233 * t263
      t265 = t231 * t264
      t267 = t228 * t37
      t268 = t267 * t42
      t269 = t106 * t268
      t270 = t151 * t83
      t273 = x1 * x3 * t10
      t277 = t26 * t33
      t281 = t254 * t200 * t178
      t282 = t250 * t277 * t23 * t281
      t285 = t100 * t42
      t290 = t83 * t228
      t291 = t106 * t290
      t292 = t59 * x2
      t293 = t100 * t5
      t294 = t293 * t61
      t295 = t292 * t294
      t296 = t291 * t295
      t298 = t56 * t34
      t299 = t31 * t298
      t300 = t36 * t5
      t305 = t35 * t37
      t306 = t305 * t42
      t307 = t112 * t306
      t308 = t242 * t58
      t313 = t234 * t10
      t314 = t233 * t313
      t315 = t231 * t314
      t317 = t83 * t33
      t319 = t106 * t317 * t59
      t320 = z * t5
      t321 = t51 * t23
      t322 = t321 * t10
      t324 = t319 * t320 * t322
      t326 = t31 * t230
      t327 = t151 * t56
      t328 = t327 * t59
      t330 = t20 * t10 * x3
      t334 = t31 * t197
      t339 = t32 * t23
      t340 = t112 * t339
      t345 = -0.30D2 * t237 + 0.80D2 * t247 - 0.32D2 * t258 + 0.38D2 * t
     #107 * t180 - 0.30D2 * t265 - 0.240D3 * t269 * t270 * x2 * t273 - 0
     #.32D2 * t282 - 0.32D2 * t250 * t277 * t285 * t137 * t85 + 0.40D2 *
     # t296 - 0.48D2 * t299 * t292 * t300 * x3 + 0.48D2 * t307 * t308 * 
     #t244 * t10 - 0.30D2 * t315 - 0.80D2 * t324 + 0.64D2 * t326 * t328 
     #* t330 - 0.14D2 * t334 * t257 - 0.176D3 * t107 * t175 + 0.6D1 * t3
     #40 * t199 * t208 * t20
      t346 = t83 * t34
      t348 = t106 * t346 * x2
      t351 = t5 * x3 * t10
      t356 = t35 * t100 * t42
      t357 = t112 * t356
      t359 = 0.1D1 / t70 / t4
      t360 = t359 * t83
      t362 = t157 * t20
      t363 = t362 * t51
      t367 = t112 * t218
      t368 = t74 * t152
      t369 = t368 * t10
      t373 = t74 * t178
      t374 = t373 * t10
      t376 = t367 * t243 * t374
      t378 = t262 * t10
      t380 = t231 * t233 * t378
      t382 = t26 * t34
      t385 = t83 * t56
      t390 = 0.16D2 * t81 * t382 * t100 * t207 * t385 * x1 * t61
      t391 = t157 * x2
      t392 = t23 * t47
      t393 = t392 * t152
      t394 = t391 * t393
      t397 = t34 * t100
      t398 = t106 * t397
      t400 = t207 * t208 * t85
      t401 = t398 * t400
      t404 = t207 * t208 * t61
      t405 = t398 * t404
      t407 = t34 * t36
      t408 = t407 * t42
      t409 = t215 * t408
      t410 = t292 * z
      t414 = t214 * z
      t415 = t414 * t26
      t417 = t216 * t36 * t42
      t420 = x2 * t20
      t421 = t420 * t51
      t422 = t72 * t157 * t421
      t425 = t106 * t346
      t426 = x1 * x2
      t430 = t74 * t151
      t431 = t152 * t23
      t436 = t56 * t228
      t437 = t31 * t436
      t438 = t293 * t85
      t439 = t292 * t438
      t440 = t437 * t439
      t442 = t27 * t83
      t444 = t81 * t442 * t32
      t448 = t426 * t85
      t449 = t270 * t448
      t453 = t216 * t37 * t42
      t454 = t112 * t453
      t455 = t292 * t61
      t456 = t270 * t455
      t457 = t454 * t456
      t459 = 0.64D2 * t348 * x1 * t100 * t351 + 0.12D2 * t357 * t360 * t
     #58 * t363 - 0.8D1 * t367 * t243 * t369 - 0.104D3 * t376 - 0.30D2 *
     # t380 + t390 - 0.120D3 * t291 * t394 - 0.192D3 * t401 - 0.192D3 * 
     #t405 - 0.384D3 * t409 * t327 * t410 - 0.64D2 * t415 * t417 * t422 
     #- 0.32D2 * t425 * t426 * t294 + 0.6D1 * t299 * t430 * t431 * t10 +
     # 0.80D2 * t440 - 0.64D2 * t444 * t6 * t322 - 0.120D3 * t269 * t449
     # - 0.5D1 * t457
      t462 = t56 * t35
      t463 = t31 * t462
      t464 = t74 * t58
      t465 = t37 * t151
      t467 = t464 * t465 * t61
      t470 = t83 * t216
      t472 = t112 * t470 * t74
      t473 = x2 * t100
      t474 = t473 * t151
      t476 = t51 * x3 * t10
      t480 = t59 * t5
      t481 = t20 * t36
      t483 = t480 * t481 * t85
      t486 = t205 * t42
      t487 = t31 * t486
      t488 = x1 * t51
      t489 = t488 * x3
      t493 = t33 * t100
      t494 = t31 * t493
      t495 = t56 * x3
      t496 = t495 * t10
      t500 = t59 * t36
      t501 = t151 * t20
      t502 = t501 * t10
      t509 = t106 * t290 * t59 * t473 * t351
      t511 = t106 * t408
      t512 = t270 * t313
      t513 = t511 * t512
      t517 = t199 * t83
      t518 = x1 * t20
      t519 = t518 * t10
      t523 = t106 * t486
      t524 = t488 * t10
      t526 = t523 * t270 * t524
      t533 = t81 * t442
      t534 = t501 * t51
      t541 = t214 * t249
      t544 = t199 * t200 * x2
      t545 = t541 * t407 * t544
      t547 = t346 * t74
      t548 = t106 * t547
      t549 = t151 * t51
      t550 = t20 * t23
      t551 = t550 * x3
      t552 = t549 * t551
      t555 = t106 * t493
      t556 = t83 * z
      t561 = t270 * t378
      t563 = 0.64D2 * t511 * t561
      t564 = -0.6D1 * t463 * t467 + 0.48D2 * t472 * t474 * t476 - 0.64D2
     # * t425 * t483 + 0.48D2 * t487 * t327 * t489 - 0.16D2 * t494 * t20
     #7 * t496 + 0.48D2 * t348 * t500 * t502 + 0.80D2 * t509 + 0.112D3 *
     # t513 + 0.64D2 * t250 * t277 * t36 * t517 * t519 + 0.80D2 * t526 -
     # 0.32D2 * t250 * t251 * t285 * t132 * t61 + 0.64D2 * t533 * t144 *
     # t534 - 0.32D2 * t533 * t144 * t179 + 0.144D3 * t545 - 0.64D2 * t5
     #48 * t552 - 0.32D2 * t555 * t207 * t556 * t61 - t563
      t565 = t31 * t417
      t566 = t391 * t152
      t567 = t72 * t566
      t568 = t565 * t567
      t570 = t31 * t356
      t571 = t157 * t58
      t576 = t106 * t470
      t577 = t36 * t47
      t579 = t571 * t577 * t20
      t580 = t576 * t579
      t582 = t100 * t151
      t584 = t464 * t582 * t10
      t585 = t576 * t584
      t588 = x2 * t23
      t591 = t533 * t33 * t59 * t588 * t5
      t593 = t112 * t408
      t594 = t292 * t249
      t599 = t31 * t436 * t74
      t600 = x2 * t36
      t601 = t600 * t502
      t602 = t599 * t601
      t604 = t549 * x3
      t605 = t600 * t604
      t608 = t112 * t346
      t610 = t480 * t481 * t61
      t613 = t31 * t240
      t614 = t51 * t20
      t615 = t614 * x3
      t619 = t32 * t36
      t620 = t31 * t619
      t624 = t157 * t47
      t626 = t624 * t178 * t51
      t629 = t397 * t42
      t630 = t31 * t629
      t635 = t415 * t218
      t636 = t420 * x3
      t637 = t220 * t636
      t641 = t31 * t56 * t66
      t642 = t157 * x1
      t643 = t642 * t73
      t644 = t100 * t71
      t649 = t157 * t73
      t650 = t37 * t47
      t652 = t649 * t650 * x3
      t655 = t112 * t417
      t658 = t655 * t242 * t157 * t421
      t660 = -0.5D1 * t568 + 0.176D3 * t570 * t72 * t571 * t20 + 0.102D3
     # * t580 + 0.12D2 * t585 - 0.1508D4 * t591 + 0.384D3 * t593 * t270 
     #* t594 + 0.128D3 * t602 - 0.48D2 * t599 * t605 - 0.14D2 * t608 * t
     #610 + 0.64D2 * t613 * t220 * t615 - 0.8D1 * t620 * t207 * t495 + 0
     #.32D2 * t425 * t626 + 0.48D2 * t630 * t327 * x1 * t476 + 0.64D2 * 
     #t635 * t637 - 0.8D1 * t641 * t643 * t644 * t20 - 0.6D1 * t641 * t6
     #52 - 0.80D2 * t658
      t662 = t232 * t74
      t663 = t662 * t636
      t664 = t367 * t663
      t666 = t270 * t59
      t667 = t50 * t10
      t669 = t454 * t666 * t667
      t671 = t34 * t37
      t672 = t31 * t671
      t677 = z * t20
      t678 = t677 * t10
      t680 = t593 * t666 * t678
      t685 = t31 * t218
      t687 = 0.16D2 * t685 * t223
      t688 = t251 * t36
      t690 = t199 * t56
      t694 = t541 * t216
      t696 = t694 * t285 * t47
      t697 = t74 * x2
      t699 = t10 * t56
      t702 = t696 * t697 * t26 * t699 * t20
      t704 = t642 * t58
      t705 = t36 * t71
      t710 = t74 * t51
      t715 = t34 * t23
      t716 = t715 * t42
      t718 = t232 * t245
      t720 = 0.64D2 * t106 * t716 * t718
      t721 = t112 * t486
      t722 = t270 * t519
      t725 = t31 * t408
      t727 = t725 * t327 * t378
      t729 = t541 * t453
      t730 = t151 * t59
      t734 = t729 * t730 * x2 * t27 * t85
      t736 = t27 * t34
      t738 = t36 * t42
      t741 = t541 * t736 * t738 * t730 * x2
      t744 = t624 * t20 * t152
      t747 = t420 * t10
      t748 = t220 * t747
      t750 = 0.16D2 * t685 * t748
      t751 = 0.74D2 * t664 - 0.80D2 * t669 - 0.2D1 * t672 * t207 * t56 *
     # t101 + 0.48D2 * t680 - 0.384D3 * t107 * t3 * t249 + t687 + 0.64D2
     # * t250 * t688 * t690 * t489 + 0.32D2 * t702 - 0.24D2 * t463 * t70
     #4 * t705 * t178 - 0.16D2 * t307 * t308 * t710 * t10 - t720 + 0.48D
     #2 * t721 * t722 - 0.80D2 * t727 + 0.16D2 * t734 + 0.144D3 * t741 -
     # 0.24D2 * t608 * t744 + t750
      t752 = t83 * t35
      t753 = t112 * t752
      t756 = t56 * t216
      t757 = t31 * t756
      t758 = t642 * x2
      t759 = t23 * t71
      t764 = t249 * z
      t766 = t382 * t36
      t769 = t199 * t255 * x2
      t778 = t571 * t178
      t782 = t26 * t83
      t783 = t250 * t782
      t791 = t262 * t61
      t797 = t47 * t20
      t798 = t797 * t51
      t802 = t290 * t74
      t803 = t106 * t802
      t804 = t501 * x3
      t806 = t803 * t600 * t804
      t808 = t571 * t152
      t812 = t66 * t37
      t813 = t812 * t42
      t814 = t112 * t813
      t815 = t649 * t51
      t819 = t697 * t178
      t820 = t242 * t819
      t821 = t241 * t820
      t824 = t31 * t756 * t59
      t825 = x2 * t37
      t828 = t825 * t5 * t61 * t10
      t831 = t26 * t216
      t833 = t414 * t831 * t37
      t839 = t59 * t152
      t840 = t839 * x3
      t844 = -0.14D2 * t753 * t467 - 0.32D2 * t757 * t758 * t759 * t183 
     #- 0.128D3 * t214 * t764 * t766 * t769 - 0.384D3 * t106 * t1 * t23 
     #* t207 * t83 * t249 - 0.14D2 * t357 * t360 * t778 - 0.32D2 * t783 
     #* t154 - 0.24D2 * t112 * t43 * t232 * t60 * t85 - 0.32D2 * t326 * 
     #t327 * t791 - 0.240D3 * t106 * t290 * t157 * t588 * t798 - 0.30D2 
     #* t806 - 0.6D1 * t357 * t360 * t808 - 0.6D1 * t814 * t360 * t815 +
     # 0.40D2 * t821 + 0.6D1 * t824 * t828 - 0.32D2 * t833 * t690 * t455
     # + 0.32D2 * t425 * t744 - 0.64D2 * t511 * t232 * t840
      t848 = t391 * t178
      t849 = t72 * t848
      t857 = t106 * t197 * t42
      t858 = z * t59
      t861 = t857 * t232 * t858 * t152
      t864 = t56 * x1
      t867 = t31 * t267 * t207 * t864 * t101
      t869 = t270 * t235
      t871 = 0.64D2 * t511 * t869
      t872 = t106 * t629
      t874 = t270 * t518 * t85
      t877 = t783 * t115
      t885 = t31 * t716
      t886 = t74 * z
      t887 = t886 * t178
      t891 = x1 * z
      t892 = t891 * t85
      t894 = t630 * t132 * t892
      t900 = t839 * t10
      t904 = t373 * x3
      t910 = t321 * x3
      t911 = t480 * t910
      t914 = t106 * t84
      t915 = t23 * t5
      t916 = t915 * t249
      t920 = t242 * t848
      t921 = t655 * t920
      t923 = t31 * t306
      t928 = -0.38D2 * t565 * t849 + 0.176D3 * t357 * t242 * t571 * t51 
     #+ 0.48D2 * t861 - 0.32D2 * t867 - t871 + 0.32D2 * t872 * t874 - 0.
     #32D2 * t877 - 0.48D2 * t31 * t229 * t254 * t56 * t58 * t59 + 0.32D
     #2 * t885 * t57 * t887 - 0.48D2 * t894 + 0.6D1 * t630 * t327 * t488
     # * t85 - 0.14D2 * t725 * t57 * t900 - 0.32D2 * t613 * t57 * t904 +
     # 0.64D2 * t250 * t782 * t33 * t911 - 0.384D3 * t914 * t426 * t916 
     #- 0.5D1 * t921 + 0.176D3 * t923 * t57 * t464 * x3
      t929 = t57 * x2
      t935 = t207 * t208 * x3
      t936 = t206 * t935
      t938 = t106 * t715
      t939 = t83 * t74
      t941 = t254 * t939 * t152
      t942 = t938 * t941
      t945 = t725 * t327 * t313
      t947 = t106 * t197
      t948 = t947 * t281
      t952 = 0.16D2 * t367 * t662 * t222
      t953 = t31 * t205
      t954 = t56 * x2
      t960 = t42 * t71
      t966 = t178 * t23
      t968 = t430 * t966 * t10
      t969 = t299 * t968
      t971 = t106 * t619
      t980 = t137 * t273
      t982 = 0.64D2 * t872 * t980
      t983 = t270 * t263
      t984 = t511 * t983
      t986 = t360 * x2
      t988 = t157 * t178 * t51
      t992 = t292 * t85
      t996 = t362 * t152
      t1000 = t234 * t85
      t1002 = t454 * t233 * t1000
      t1004 = -0.384D3 * t725 * t929 * t262 * z - 0.12D2 * t936 - 0.192D
     #3 * t942 + 0.30D2 * t945 + 0.5D1 * t948 + t952 + 0.32D2 * t953 * t
     #199 * t954 * x1 + 0.32D2 * t31 * t305 * t960 * t56 * t73 * t74 + 0
     #.120D3 * t969 - 0.384D3 * t971 * t207 * t556 * t10 - 0.384D3 * t97
     #1 * t207 * t556 * x3 - t982 + 0.112D3 * t984 - 0.14D2 * t655 * t98
     #6 * t988 - 0.38D2 * t454 * t270 * t992 + 0.6D1 * t655 * t986 * t99
     #6 + 0.120D3 * t1002
      t1006 = t59 * t178
      t1008 = t232 * t1006 * x3
      t1009 = t511 * t1008
      t1011 = t550 * t10
      t1013 = t319 * t320 * t1011
      t1015 = t112 * t397
      t1016 = t1015 * t404
      t1018 = t112 * t629
      t1019 = t270 * x1
      t1020 = t1019 * t330
      t1026 = x3 * t10
      t1031 = t285 * t71
      t1038 = t31 * t56 * t33
      t1039 = t480 * t1011
      t1042 = t112 * t290
      t1043 = t36 * t151
      t1044 = t464 * t1043
      t1047 = t112 * t802
      t1050 = t112 * t317
      t1053 = t31 * t397
      t1058 = t56 * t61
      t1062 = t480 * t322
      t1065 = t221 * t10
      t1066 = t220 * t1065
      t1069 = t292 * t916
      t1078 = 0.32D2 * t1009 - 0.48D2 * t1013 - 0.40D2 * t1016 + 0.48D2 
     #* t1018 * t1020 - 0.64D2 * t444 * t6 * t551 + 0.64D2 * t533 * t619
     # * t1026 + 0.48D2 * t31 * t216 * t1031 * t954 * t74 * t615 + 0.48D
     #2 * t1038 * t1039 - 0.48D2 * t1042 * t1044 - 0.48D2 * t1047 * t601
     # + 0.48D2 * t1050 * t911 + 0.120D3 * t1053 * t207 * t864 * t61 - 0
     #.24D2 * t494 * t207 * t1058 + 0.12D2 * t1038 * t1062 + 0.64D2 * t2
     #19 * t1066 + 0.384D3 * t1038 * t1069 - 0.384D3 * t112 * t346 * t59
     # * t600 * t320 * x3
      t1079 = t541 * t218
      t1080 = t1079 * t663
      t1083 = t27 * t26 * t34
      t1088 = 0.32D2 * t81 * t1083 * t36 * t690 * t378
      t1091 = t83 * t61
      t1092 = t207 * t1091
      t1097 = t947 * t254 * t200 * t152
      t1099 = t112 * t715
      t1101 = t254 * t939 * t178
      t1102 = t1099 * t1101
      t1110 = t112 * t230
      t1115 = t571 * t577 * t51
      t1116 = t576 * t1115
      t1118 = t891 * t61
      t1120 = t1018 * t137 * t1118
      t1122 = t299 * t483
      t1124 = t27 * t56
      t1125 = t250 * t1124
      t1129 = t725 * t57 * t840
      t1132 = t112 * t83 * t66
      t1142 = -0.32D2 * t1080 - t1088 + 0.32D2 * t437 * t295 + 0.38D2 * 
     #t555 * t1092 + 0.5D1 * t1097 - 0.40D2 * t1102 + 0.120D3 * t1015 * 
     #t400 - 0.384D3 * t425 * t60 * t1043 * z - 0.32D2 * t1110 * t270 * 
     #t1000 + 0.102D3 * t1116 - 0.48D2 * t1120 - 0.104D3 * t1122 - 0.32D
     #2 * t1125 * t180 - 0.104D3 * t1129 - 0.8D1 * t1132 * t643 * t644 *
     # t51 + 0.6D1 * t1132 * t652 + 0.48D2 * t348 * t500 * t604
      t1145 = t112 * t240
      t1146 = t614 * t10
      t1150 = t292 * t915
      t1153 = t1125 * t92
      t1157 = t83 * x2
      t1163 = t914 * t426 * t915
      t1171 = 0.16D2 * t81 * t382 * t23 * t254 * t385 * t74 * t178
      t1173 = t430 * t431 * x3
      t1176 = t51 * t36
      t1178 = t480 * t1176 * t61
      t1181 = x2 * z
      t1192 = t426 * t249
      t1196 = t56 * t85
      t1203 = t51 * z
      t1207 = t736 * t36
      t1208 = t81 * t1207
      t1211 = 0.32D2 * t1208 * t517 * t263
      t1217 = 0.32D2 * t81 * t736 * t23 * t254 * t83 * t245
      t1218 = 0.64D2 * t1145 * t662 * t1146 + 0.32D2 * t1050 * t1150 - 0
     #.32D2 * t1153 + 0.48D2 * t112 * t216 * t1031 * t1157 * t74 * t1146
     # + 0.288D3 * t1163 + t1171 + 0.32D2 * t425 * t1173 - 0.64D2 * t425
     # * t1178 + 0.384D3 * t326 * t328 * t1181 * x3 + 0.64D2 * t635 * t1
     #066 - 0.384D3 * t593 * t233 * t858 * t20 + 0.384D3 * t487 * t327 *
     # t1192 - 0.6D1 * t494 * t207 * t1196 - 0.384D3 * t914 * t858 * t17
     #4 - 0.384D3 * t914 * t480 * t1203 + t1211 - t1217
      t1219 = t1203 * x3
      t1221 = t593 * t666 * t1219
      t1223 = t106 * t205
      t1224 = t1223 * t935
      t1226 = t106 * t339
      t1227 = t207 * t208
      t1228 = t1226 * t1227
      t1230 = t228 * t23
      t1232 = t56 * t157
      t1235 = t31 * t1230 * t960 * t1232 * t159
      t1237 = t1230 * t42
      t1240 = t112 * t1237 * t242 * t988
      t1242 = t31 * t715
      t1243 = t56 * t74
      t1246 = t1242 * t960 * t1243 * t159
      t1248 = t392 * t178
      t1249 = t391 * t1248
      t1250 = t437 * t1249
      t1253 = t106 * t407 * t544
      t1257 = t480 * t551
      t1260 = t430 * t614
      t1265 = t334 * t199 * t255 * t20
      t1277 = t262 * t85
      t1281 = t234 * t61
      t1285 = 0.48D2 * t1221 - 0.597D3 * t1224 - 0.176D3 * t1228 - 0.32D
     #2 * t1235 - 0.32D2 * t1240 + 0.42D2 * t1246 - 0.288D3 * t1250 + 0.
     #984D3 * t1253 - 0.14D2 * t198 * t281 + 0.12D2 * t1050 * t1257 - 0.
     #16D2 * t1050 * t1260 - 0.102D3 * t1265 + 0.384D3 * t613 * t220 * t
     #1181 * t20 + 0.64D2 * t444 * t6 * t910 - 0.32D2 * t1145 * t232 * t
     #369 - 0.32D2 * t1110 * t270 * t1277 - 0.32D2 * t326 * t327 * t1281
      t1290 = t549 * t10
      t1294 = t685 * t1066
      t1297 = t232 * t1006 * t10
      t1298 = t593 * t1297
      t1300 = t1018 * t874
      t1302 = t425 * t610
      t1306 = t334 * t199 * t255 * t51
      t1310 = t953 * t207 * t864 * x3
      t1316 = t231 * t270 * t292 * x3
      t1320 = t241 * t232 * t697 * t20
      t1323 = t199 * t200 * t51
      t1324 = t198 * t1323
      t1326 = t106 * t671
      t1327 = t83 * t85
      t1329 = t207 * t1327 * x3
      t1338 = t234 * t51
      t1340 = t857 * t232 * t1338
      t1342 = t518 * x3
      t1343 = t270 * t1342
      t1344 = t523 * t1343
      t1346 = -0.32D2 * t425 * t697 * t393 - 0.48D2 * t348 * t500 * t129
     #0 + 0.74D2 * t1294 - 0.104D3 * t1298 + 0.120D3 * t1300 + 0.32D2 * 
     #t1302 - 0.12D2 * t1306 - 0.102D3 * t1310 - 0.64D2 * t511 * t1297 -
     # 0.597D3 * t1316 - 0.597D3 * t1320 - 0.102D3 * t1324 + 0.32D2 * t1
     #326 * t1329 + 0.64D2 * t348 * t74 * t23 * t798 + 0.64D2 * t219 * t
     #637 - 0.74D2 * t1340 + 0.80D2 * t1344
      t1347 = t112 * t619
      t1349 = t207 * t83 * t10
      t1353 = t464 * t582 * x3
      t1354 = t576 * t1353
      t1358 = t83 * x3
      t1359 = t207 * t1358
      t1365 = t368 * x3
      t1369 = t31 * t453
      t1370 = t327 * t455
      t1375 = t100 * t47
      t1376 = t649 * t1375
      t1383 = t464 * t465 * t85
      t1387 = t649 * t650 * t10
      t1393 = t270 * z
      t1395 = t523 * t1393 * t519
      t1397 = t565 * t422
      t1402 = t215 * t417
      t1405 = -0.8D1 * t1347 * t1349 + 0.12D2 * t1354 - 0.176D3 * t971 *
     # t1349 - 0.176D3 * t971 * t1359 - 0.48D2 * t348 * t500 * t804 - 0.
     #32D2 * t1145 * t232 * t1365 - 0.38D2 * t1369 * t1370 + 0.12D2 * t7
     #21 * t1343 + 0.32D2 * t753 * t1376 + 0.64D2 * t1110 * t666 * t476 
     #- 0.14D2 * t463 * t1383 + 0.6D1 * t641 * t1387 + 0.64D2 * t1110 * 
     #t270 * t791 - 0.48D2 * t1395 - 0.80D2 * t1397 - 0.38D2 * t655 * t2
     #42 * t566 - 0.48D2 * t1402 * t422
      t1410 = t112 * t493
      t1411 = t207 * t1327
      t1416 = t66 * t1
      t1419 = t58 ** 2
      t1422 = t642 * t1419 * t37 * t71
      t1436 = t106 * t752 * t1376
      t1442 = t947 * t1323
      t1444 = t327 * t992
      t1445 = t1369 * t1444
      t1448 = t523 * t1393 * t1342
      t1451 = t414 * t831 * t36
      t1452 = t960 * t56
      t1456 = t112 * t671
      t1458 = t207 * t1091 * t10
      t1472 = -0.24D2 * t1410 * t1411 - 0.24D2 * t299 * t626 - 0.2D1 * t
     #112 * t83 * t1416 * t1422 - 0.8D1 * t608 * t626 + 0.6D1 * t608 * t
     #430 * t966 * x3 + 0.32D2 * t425 * t968 - 0.16D2 * t1038 * t1260 - 
     #0.52D2 * t1436 - 0.2D1 * t31 * t56 * t1416 * t1422 - 0.597D3 * t14
     #42 - 0.5D1 * t1445 - 0.80D2 * t1448 - 0.32D2 * t1451 * t1452 * t56
     #6 - 0.8D1 * t1456 * t1458 - 0.32D2 * t672 * t207 * t56 * t121 - 0.
     #6D1 * t620 * t207 * t699 + 0.176D3 * t307 * t232 * t464 * t10
      t1474 = t960 * t939 * t183
      t1475 = t938 * t1474
      t1479 = t938 * t960 * t939 * t159
      t1483 = t215 * t453
      t1484 = t328 * t667
      t1498 = t1047 * t605
      t1508 = t857 * t232 * t858 * t178
      t1512 = z * x3 * t10
      t1514 = t1018 * t137 * x1 * t1512
      t1518 = 0.16D2 * t523 * t270 * t489
      t1519 = t488 * t61
      t1521 = t630 * t327 * t1519
      t1527 = t803 * t601
      t1529 = 0.32D2 * t1475 + 0.32D2 * t1479 + 0.38D2 * t555 * t1411 - 
     #0.48D2 * t1483 * t1484 - 0.128D3 * t80 * t764 * t442 + 0.384D3 * t
     #721 * t270 * t1192 - 0.64D2 * t872 * t1020 - 0.64D2 * t872 * t1019
     # * t476 + 0.128D3 * t1498 + 0.64D2 * t444 * t6 * t1011 + 0.32D2 * 
     #t630 * t132 * t1118 + 0.48D2 * t1508 + 0.80D2 * t1514 - t1518 + 0.
     #120D3 * t1521 - 0.24D2 * t672 * t207 * t1058 * t10 - 0.30D2 * t152
     #7
      t1532 = t576 * t75 * t1375
      t1534 = t37 * t5
      t1536 = t292 * t1534 * t101
      t1537 = t757 * t1536
      t1540 = t292 * t1534 * t121
      t1545 = t541 * t417
      t1550 = t1545 * t71 * t157 * x2 * t152 * t27
      t1552 = t31 * t339
      t1558 = t425 * t60 * t1043
      t1560 = t106 * t317
      t1565 = t31 * t756 * t157
      t1566 = t47 * t178
      t1572 = t31 * t756 * t74
      t1579 = t112 * t268
      t1583 = t1579 * t137 * x1 * t61 * t10
      t1591 = t112 * t267 * t207 * t208 * t121
      t1594 = t523 * t1393 * t489
      t1598 = t953 * t207 * t864 * t10
      t1602 = -0.80D2 * t1532 + 0.42D2 * t1537 - 0.2D1 * t757 * t1540 + 
     #0.384D3 * t1050 * t1069 + 0.16D2 * t1550 + 0.6D1 * t1552 * t199 * 
     #t864 * t51 - 0.128D3 * t1558 - 0.32D2 * t1560 * t886 * t179 - 0.24
     #D2 * t1565 * t600 * t1566 * x3 - 0.8D1 * t1572 * t473 * t501 * t61
     # + 0.48D2 * t219 * t748 - 0.32D2 * t1583 - 0.32D2 * t613 * t57 * t
     #374 - 0.32D2 * t1591 - 0.48D2 * t1594 - 0.12D2 * t1598 - 0.14D2 * 
     #t593 * t1008
      t1604 = 0.16D2 * t1560 * t1039
      t1605 = t1560 * t1062
      t1609 = t1369 * t1484
      t1614 = t215 * t407 * t769
      t1617 = t254 * t1243 * t152
      t1618 = t1242 * t1617
      t1620 = t501 * t322
      t1627 = t1560 * t891 * t86
      t1630 = t1560 * t891 * t114
      t1637 = t1079 * t47 * t74 * x2 * t51 * t27 * t10
      t1639 = t697 * t51
      t1641 = t241 * t232 * t1639
      t1647 = 0.32D2 * t81 * t766 * t199 * t385 * t235
      t1650 = t1099 * t1474
      t1652 = t886 * t152
      t1654 = t885 * t57 * t1652
      t1656 = -t1604 + 0.80D2 * t1605 + 0.16D2 * t685 * t637 - 0.80D2 * 
     #t1609 + 0.64D2 * t635 * t748 - 0.80D2 * t1614 - 0.40D2 * t1618 - 0
     #.64D2 * t548 * t1620 - 0.32D2 * t833 * t690 * t992 + 0.48D2 * t162
     #7 + 0.48D2 * t1630 - 0.32D2 * t1637 - 0.597D3 * t1641 - t1647 + 0.
     #64D2 * t635 * t223 + 0.42D2 * t1650 - 0.48D2 * t1654
      t1659 = t426 * t61
      t1660 = t270 * t1659
      t1663 = t697 * t152
      t1664 = t242 * t1663
      t1665 = t241 * t1664
      t1667 = t30 * z
      t1669 = t1667 * t1207 * t769
      t1673 = t825 * t5 * t85 * x3
      t1676 = t677 * t51
      t1678 = t885 * t220 * t1676
      t1680 = t1545 * t920
      t1693 = t803 * t600 * t1290
      t1697 = t960 * t1232 * t73
      t1698 = t215 * t812 * t1697
      t1701 = t207 * t1358 * t10
      t1710 = t729 * t456
      t1714 = t199 * t1157 * x1
      t1715 = t81 * t688 * t1714
      t1717 = t112 * t716
      t1719 = t1717 * t232 * t887
      t1721 = -0.120D3 * t269 * t1660 + 0.40D2 * t1665 + 0.1508D4 * t166
     #9 - 0.14D2 * t824 * t1673 + 0.80D2 * t1678 + 0.16D2 * t1680 - 0.76
     #8D3 * t1226 * t207 * t208 * z - 0.32D2 * t425 * t697 * t1248 - 0.2
     #4D2 * t454 * t233 * t1277 - 0.30D2 * t1693 + 0.288D3 * t1698 - 0.1
     #6D2 * t555 * t1701 - 0.8D1 * t672 * t207 * t1196 * x3 - 0.16D2 * t
     #1560 * t1260 + 0.16D2 * t1710 - 0.1508D4 * t1715 - 0.48D2 * t1719
      t1723 = t523 * t1393 * t524
      t1727 = t857 * t232 * z * t1338
      t1732 = t106 * t453
      t1733 = t60 * t10
      t1735 = t1732 * t232 * t1733
      t1737 = t106 * t218
      t1738 = t464 * t20
      t1740 = t1737 * t242 * t1738
      t1743 = t242 * t464 * t51
      t1744 = t1737 * t1743
      t1747 = t232 * t60 * x3
      t1748 = t1732 * t1747
      t1754 = t326 * t929 * t263
      t1756 = t1110 * t314
      t1761 = t725 * t328 * t678
      t1764 = t725 * t328 * t1219
      t1766 = t359 * t56
      t1767 = t1766 * x2
      t1778 = t72 * x2
      t1780 = t685 * t1778 * t1365
      t1782 = -0.80D2 * t1723 + 0.32D2 * t1727 + 0.64D2 * t326 * t327 * 
     #t1000 + 0.102D3 * t1735 + 0.12D2 * t1740 + 0.12D2 * t1744 + 0.102D
     #3 * t1748 - 0.48D2 * t326 * t929 * t313 + 0.128D3 * t1754 + 0.128D
     #3 * t1756 - 0.48D2 * t1110 * t264 + 0.48D2 * t1761 + 0.48D2 * t176
     #4 + 0.6D1 * t565 * t1767 * t988 - 0.14D2 * t565 * t1767 * t996 + 0
     #.6D1 * t1018 * t270 * t518 * t61 - 0.104D3 * t1780
      t1786 = 0.16D2 * t367 * t662 * t747
      t1790 = t1560 * t1257
      t1792 = t31 * t268
      t1796 = t1792 * t132 * x1 * t85 * x3
      t1804 = t31 * t462 * t157
      t1805 = t58 * t100
      t1812 = t1565 * t600 * t1566 * t10
      t1816 = t1572 * t473 * t501 * t85
      t1821 = t207 * t864 * t85
      t1823 = 0.16D2 * t81 * t1083 * t100 * t1821
      t1831 = t694 * t37 * t42 * t151 * t292 * t26 * t496
      t1836 = t1223 * t210
      t1849 = t1786 + 0.16D2 * t367 * t662 * t1065 + 0.80D2 * t1790 - 0.
     #32D2 * t1796 + 0.32D2 * t872 * t270 * t1519 + 0.32D2 * t1326 * t14
     #58 + 0.48D2 * t1804 * t1805 * t797 * t10 + 0.120D3 * t1812 - 0.104
     #D3 * t1816 + t1823 + 0.32D2 * t206 * t1714 - 0.32D2 * t1831 - 0.64
     #D2 * t415 * t453 * t1484 - 0.597D3 * t1836 - 0.32D2 * t1456 * t207
     # * t83 * t101 - 0.2D1 * t1456 * t207 * t83 * t121 - 0.768D3 * t511
     # * t270 * t410
      t1850 = t1203 * t10
      t1855 = t1717 * t662 * t1676
      t1858 = t1369 * t929 * t791
      t1876 = t72 * t58
      t1886 = t511 * t232 * t900
      t1890 = t231 * t270 * t292 * t10
      t1893 = 0.16D2 * t523 * t722
      t1902 = t1053 * t1821
      t1904 = 0.64D2 * t593 * t666 * t1850 + 0.80D2 * t1855 + 0.120D3 * 
     #t1858 - 0.8D1 * t685 * t1778 * t904 - 0.24D2 * t1369 * t929 * t128
     #1 + 0.32D2 * t463 * t1376 - 0.48D2 * t437 * t1044 + 0.32D2 * t1038
     # * t1150 + 0.12D2 * t570 * t1766 * t58 * t363 + 0.48D2 * t923 * t1
     #876 * t710 * x3 - 0.16D2 * t923 * t1876 * t244 * x3 + 0.32D2 * t18
     #86 - 0.597D3 * t1890 - t1893 + 0.384D3 * t1110 * t666 * t1181 * t1
     #0 + 0.120D3 * t1242 * t254 * t1243 * t178 - 0.40D2 * t1902
      t1908 = t677 * x3
      t1915 = 0.16D2 * t81 * t1083 * t23 * t1617
      t1925 = t215 * t217 * t254 * t1243 * t58
      t1927 = t391 * t159
      t1932 = t216 * t38 * t42
      t1941 = t1552 * t207 * t864
      t1950 = t1667 * t766 * t544
      t1956 = t1416 * t38
      t1958 = t42 * t359
      t1969 = 0.32D2 * t81 * t736 * t100 * t207 * t83 * t273
      t1972 = 0.32D2 * t1208 * t517 * t313
      t1976 = t960 * t83 * t73 * t74
      t1977 = t106 * t305 * t1976
      t1979 = 0.64D2 * t725 * t328 * t1908 + t1915 + 0.32D2 * t1018 * t1
     #37 * t892 - 0.32D2 * t1560 * t886 * t153 - 0.128D3 * t1925 - 0.2D1
     # * t655 * t360 * t1927 - 0.32D2 * t112 * t1932 * t270 * t426 * t10
     #1 - 0.16D2 * t1410 * t1701 + 0.52D2 * t1941 + 0.120D3 * t1099 * t9
     #41 + 0.384D3 * t1145 * t662 * t221 * z + 0.1508D4 * t1950 + 0.64D2
     # * t250 * t1124 * t33 * t1039 - 0.2D1 * t112 * t1956 * t1958 * t83
     # * t1419 * t157 - t1969 + t1972 - 0.52D2 * t1977
      t1983 = t608 * t1173
      t1985 = t608 * t1178
      t1987 = t938 * t1101
      t1991 = t31 * t1237 * t72 * t996
      t1994 = t319 * t320 * t910
      t2000 = t1792 * t327 * t1659
      t2003 = t613 * t72 * t1663
      t2005 = t803 * t605
      t2013 = 0.16D2 * t1560 * t911
      t2025 = t885 * t57 * t245
      t2027 = 0.32D2 * t613 * t72 * t819 + 0.120D3 * t1983 - 0.104D3 * t
     #1985 - 0.192D3 * t1987 - 0.32D2 * t1991 - 0.48D2 * t1994 + 0.32D2 
     #* t1792 * t327 * t448 - 0.288D3 * t2000 + 0.80D2 * t2003 - 0.30D2 
     #* t2005 - 0.24D2 * t1456 * t1329 + 0.64D2 * t613 * t57 * t1365 - t
     #2013 - 0.48D2 * t1369 * t57 * t1733 + 0.64D2 * t685 * t72 * t1738 
     #- 0.48D2 * t725 * t57 * t292 * t20 + 0.30D2 * t2025
      t2037 = t112 * t752 * t157
      t2038 = t47 * t51
      t2054 = t1042 * t295
      t2061 = t112 * t470 * t157
      t2062 = t47 * t152
      t2074 = t327 * t594
      t2081 = t630 * t132 * t273
      t2084 = t725 * t327 * t263
      t2086 = -0.16D2 * t1804 * t1805 * t797 * x3 + 0.48D2 * t1572 * t47
     #4 * t330 + 0.48D2 * t2037 * t1805 * t2038 * x3 + 0.48D2 * t31 * t2
     #98 * t74 * t1620 + 0.48D2 * t112 * t547 * t552 + 0.32D2 * t1042 * 
     #t1249 + 0.32D2 * t1042 * t439 + 0.80D2 * t2054 - 0.32D2 * t1483 * 
     #t1370 - 0.32D2 * t1402 * t567 - 0.24D2 * t2061 * t600 * t2062 * t1
     #0 - 0.8D1 * t472 * t473 * t549 * t85 + 0.32D2 * t1717 * t232 * t16
     #52 + 0.384D3 * t725 * t2074 + 0.240D3 * t725 * t327 * t235 + 0.30D
     #2 * t2081 + 0.30D2 * t2084
      t2087 = t593 * t512
      t2091 = t2061 * t600 * t2062 * x3
      t2095 = t472 * t473 * t549 * t61
      t2098 = t112 * t470 * t59
      t2112 = t58 * t37 * t151 * x3 * t10
      t2126 = t1717 * t718
      t2128 = t593 * t869
      t2130 = t1018 * t980
      t2132 = t593 * t983
      t2134 = t1042 * t394
      t2136 = t112 * t470
      t2141 = 0.30D2 * t2087 + 0.120D3 * t2091 - 0.104D3 * t2095 - 0.14D
     #2 * t2098 * t828 + 0.6D1 * t2098 * t1673 - 0.16D2 * t2037 * t1805 
     #* t2038 * t10 + 0.12D2 * t112 * t752 * t74 * t2112 + 0.32D2 * t112
     # * t305 * t1976 - 0.48D2 * t112 * t229 * t254 * t83 * t58 * t59 + 
     #0.240D3 * t593 * t561 + 0.30D2 * t2126 - 0.80D2 * t2128 + 0.30D2 *
     # t2130 + 0.30D2 * t2132 - 0.288D3 * t2134 + 0.64D2 * t2136 * t584 
     #- 0.48D2 * t2136 * t579
      t2144 = t23 * t151
      t2159 = t2136 * t1540
      t2172 = t83 * t1 * x1
      t2179 = t630 * t132 * x1 * t1512
      t2182 = t725 * t328 * t1850
      t2186 = t340 * t1227
      t2196 = 0.64D2 * t608 * t697 * t2144 * t20 - 0.48D2 * t608 * t292 
     #* t300 * t10 - 0.32D2 * t2136 * t758 * t759 * t159 - 0.2D1 * t2136
     # * t1536 + 0.42D2 * t2159 + 0.64D2 * t299 * t697 * t2144 * t51 + 0
     #.64D2 * t757 * t1353 - 0.2D1 * t31 * t56 * t1 * x1 + 0.260D3 * t10
     #6 * t2172 - 0.2D1 * t112 * t2172 + 0.80D2 * t2179 - 0.32D2 * t2182
     # + 0.32D2 * t1145 * t1664 + 0.52D2 * t2186 + 0.64D2 * t367 * t1743
     # - 0.48D2 * t593 * t232 * t292 * t51 - 0.48D2 * t454 * t1747
      t2205 = t291 * t439
      t2222 = t649 * t20
      t2243 = t1145 * t820
      t2245 = t1579 * t449
      t2247 = 0.64D2 * t1018 * t270 * t426 * x3 - 0.8D1 * t299 * t744 - 
     #0.120D3 * t291 * t1249 + 0.40D2 * t2205 - 0.32D2 * t533 * t144 * t
     #153 + 0.12D2 * t31 * t462 * t74 * t2112 - 0.64D2 * t1560 * t886 * 
     #t534 - 0.8D1 * t112 * t68 * t242 * t75 * t10 + 0.6D1 * t814 * t360
     # * t2222 - 0.32D2 * t555 * t207 * t556 * t85 + 0.12D2 * t487 * t32
     #7 * t524 - 0.48D2 * t757 * t1115 + 0.32D2 * t437 * t394 + 0.64D2 *
     # t630 * t327 * t426 * t10 - 0.6D1 * t1410 * t1092 + 0.80D2 * t2243
     # - 0.288D3 * t2245
      t2252 = t319 * t320 * t551
      t2257 = t1560 * t891 * t36 * x3 * t10
      t2261 = t1560 * x1 * t36 * t1026
      t2272 = t947 * t202
      t2278 = t83 * t157
      t2281 = t112 * t1230 * t960 * t2278 * t183
      t2284 = t480 * t1176 * t85
      t2285 = t425 * t2284
      t2293 = t391 * t183
      t2295 = t655 * t360 * t2293
      t2300 = t26 * t56
      t2303 = t696 * t1639 * t2300 * x3
      t2310 = t694 * t738 * t71 * t391 * t51 * t2300 * t20
      t2316 = t565 * t1766 * t1927
      t2318 = 0.32D2 * t1579 * t1660 - 0.80D2 * t2252 + 0.32D2 * t2257 -
     # 0.74D2 * t2261 - 0.32D2 * t425 * t426 * t438 - 0.384D3 * t31 * t2
     #98 * t59 * t600 * t320 * t10 - 0.597D3 * t2272 + 0.64D2 * t1145 * 
     #t232 * t374 - 0.32D2 * t2281 + 0.32D2 * t2285 - 0.2D1 * t31 * t195
     #6 * t1958 * t56 * t1419 * t157 + 0.42D2 * t2295 - 0.2D1 * t565 * t
     #1766 * t2293 + 0.32D2 * t2303 - 0.32D2 * t2310 - 0.32D2 * t1451 * 
     #t1452 * t848 + 0.42D2 * t2316
      t2330 = t106 * t217 * t254 * t939 * t58
      t2360 = t593 * t666 * t1908
      t2366 = t31 * t813
      t2376 = -0.32D2 * t31 * t1932 * t327 * t426 * t121 - 0.6D1 * t570 
     #* t1766 * t778 - 0.176D3 * t2330 - 0.260D3 * t31 * t812 * t1697 - 
     #0.260D3 * t112 * t812 * t960 * t2278 * t73 - 0.6D1 * t1347 * t1359
     # - 0.32D2 * t1402 * t849 - 0.32D2 * t1483 * t1444 - 0.14D2 * t299 
     #* t2284 - 0.6D1 * t1132 * t1387 - 0.24D2 * t753 * t704 * t705 * t1
     #52 - 0.64D2 * t106 * t493 * t42 * t137 * t1512 - 0.32D2 * t2360 - 
     #0.384D3 * t409 * t2074 - 0.6D1 * t753 * t1383 - 0.6D1 * t2366 * t1
     #766 * t2222 - 0.14D2 * t570 * t1766 * t808 + 0.6D1 * t2366 * t1766
     # * t815
      t2397 = -0.32D2 * t104 - 0.64D2 * t108 + 0.32D2 * t110 - 0.32D2 * 
     #t124 - 0.64D2 * t126 + 0.32D2 * t128 - 0.16D2 * t203 + 0.48D2 * t2
     #11 + 0.64D2 * t237 - 0.256D3 * t247 + 0.64D2 * t258 + 0.32D2 * t26
     #5 + 0.64D2 * t282
      t2410 = -0.64D2 * t296 + 0.32D2 * t315 + 0.16D2 * t324 + 0.64D2 * 
     #t376 + 0.64D2 * t380 - t390 + 0.32D2 * t401 + 0.32D2 * t405 + 0.17
     #6D3 * t440 - 0.32D2 * t457 - 0.256D3 * t509 - 0.192D3 * t513 - 0.1
     #6D2 * t526
      t2423 = -0.72D2 * t545 - t563 - 0.32D2 * t568 - 0.48D2 * t580 + 0.
     #16D2 * t585 + 0.288D3 * t591 - 0.352D3 * t602 + 0.16D2 * t658 - 0.
     #64D2 * t664 + 0.16D2 * t669 + 0.16D2 * t680 - t687 - 0.16D2 * t702
      t2435 = -t720 + 0.256D3 * t727 - 0.8D1 * t734 - 0.72D2 * t741 - t7
     #50 + 0.64D2 * t806 - 0.64D2 * t821 - 0.96D2 * t861 + 0.64D2 * t867
     # - t871 + 0.64D2 * t877 + 0.96D2 * t894 - 0.32D2 * t921 - 0.16D2 *
     # t936
      t2448 = 0.32D2 * t942 - 0.32D2 * t945 + 0.32D2 * t948 - t952 - 0.3
     #2D2 * t969 - t982 - 0.192D3 * t984 - 0.32D2 * t1002 - 0.64D2 * t10
     #09 - 0.16D2 * t1013 + 0.64D2 * t1016 + 0.16D2 * t1080 + t1088
      t2459 = 0.32D2 * t1097 + 0.64D2 * t1102 - 0.48D2 * t1116 + 0.96D2 
     #* t1120 + 0.64D2 * t1122 + 0.64D2 * t1129 + 0.64D2 * t1153 - 0.176
     #D3 * t1163 - t1171 - t1211 + t1217 + 0.16D2 * t1221 + 0.48D2 * t12
     #24
      t2474 = 0.160D3 * t1228 + 0.64D2 * t1235 + 0.64D2 * t1240 - 0.32D2
     # * t1246 + 0.176D3 * t1250 + 0.864D3 * t1253 + 0.48D2 * t1265 - 0.
     #64D2 * t1294 + 0.64D2 * t1298 - 0.32D2 * t1300 - 0.64D2 * t1302 - 
     #0.16D2 * t1306 + 0.48D2 * t1310
      t2489 = 0.48D2 * t1316 + 0.48D2 * t1320 + 0.48D2 * t1324 + 0.64D2 
     #* t1340 - 0.16D2 * t1344 + 0.16D2 * t1354 - 0.16D2 * t1395 + 0.16D
     #2 * t1397 + 0.80D2 * t1436 + 0.48D2 * t1442 - 0.32D2 * t1445 + 0.1
     #6D2 * t1448 - 0.64D2 * t1475 - 0.64D2 * t1479
      t2505 = -0.352D3 * t1498 - 0.96D2 * t1508 - 0.16D2 * t1514 + t1518
     # - 0.32D2 * t1521 + 0.32D2 * t1527 - 0.176D3 * t1532 - 0.32D2 * t1
     #537 - 0.8D1 * t1550 + 0.352D3 * t1558 + 0.64D2 * t1583 + 0.64D2 * 
     #t1591 - 0.16D2 * t1594
      t2517 = -0.16D2 * t1598 + t1604 - 0.16D2 * t1605 + 0.16D2 * t1609 
     #- 0.176D3 * t1614 + 0.64D2 * t1618 - 0.96D2 * t1627 - 0.96D2 * t16
     #30 + 0.16D2 * t1637 + 0.48D2 * t1641 + t1647 - 0.32D2 * t1650 + 0.
     #96D2 * t1654
      t2532 = -0.64D2 * t1665 - 0.288D3 * t1669 - 0.16D2 * t1678 - 0.8D1
     # * t1680 + 0.64D2 * t1693 - 0.176D3 * t1698 - 0.8D1 * t1710 + 0.28
     #8D3 * t1715 + 0.96D2 * t1719 + 0.16D2 * t1723 - 0.64D2 * t1727 - 0
     #.48D2 * t1735 + 0.16D2 * t1740
      t2545 = 0.16D2 * t1744 - 0.48D2 * t1748 - 0.352D3 * t1754 - 0.352D
     #3 * t1756 + 0.16D2 * t1761 + 0.16D2 * t1764 + 0.64D2 * t1780 - t17
     #86 - 0.16D2 * t1790 + 0.64D2 * t1796 - 0.32D2 * t1812 + 0.64D2 * t
     #1816 - t1823 + 0.16D2 * t1831
      t2557 = 0.48D2 * t1836 - 0.16D2 * t1855 - 0.32D2 * t1858 - 0.64D2 
     #* t1886 + 0.48D2 * t1890 + t1893 + 0.64D2 * t1902 - t1915 + 0.352D
     #3 * t1925 - 0.80D2 * t1941 - 0.288D3 * t1950 + t1969 - t1972
      t2571 = 0.80D2 * t1977 - 0.32D2 * t1983 + 0.64D2 * t1985 + 0.32D2 
     #* t1987 + 0.64D2 * t1991 - 0.16D2 * t1994 + 0.176D3 * t2000 + 0.17
     #6D3 * t2003 + 0.32D2 * t2005 + t2013 - 0.64D2 * t2025 + 0.176D3 * 
     #t2054 - 0.64D2 * t2081 - 0.32D2 * t2084
      t2586 = -0.32D2 * t2087 - 0.32D2 * t2091 + 0.64D2 * t2095 - 0.64D2
     # * t2126 + 0.256D3 * t2128 - 0.64D2 * t2130 - 0.32D2 * t2132 + 0.1
     #76D3 * t2134 - 0.32D2 * t2159 - 0.16D2 * t2179 + 0.64D2 * t2182 - 
     #0.80D2 * t2186 - 0.64D2 * t2205
      t2601 = 0.176D3 * t2243 + 0.176D3 * t2245 + 0.16D2 * t2252 - 0.64D
     #2 * t2257 + 0.64D2 * t2261 + 0.48D2 * t2272 + 0.64D2 * t2281 - 0.6
     #4D2 * t2285 - 0.32D2 * t2295 - 0.16D2 * t2303 + 0.16D2 * t2310 - 0
     #.32D2 * t2316 + 0.160D3 * t2330 + 0.64D2 * t2360
      rrgg2ggh11J6 = -0.9D1 / 0.16D2 * (0.5D1 * wd * (t564 + t1656 + t84
     #4 + t1078 + t1472 + t2318 + t1346 + t226 + t1142 + t2376 + t149 + 
     #t928 + t1405 + t751 + t1218 + t2086 + t2196 + t1782 + t1529 + t459
     # + t2027 + t1721 + t1849 + t1602 + t2247 + t1904 + t1004 + t345 + 
     #t660 + t2141 + t1285 + t1979) + 0.4D1 * wd * (t2397 + t2410 + t242
     #3 + t2435 + t2448 + t2459 + t2474 + t2489 + t2505 + t2517 + t2532 
     #+ t2545 + t2557 + t2571 + t2586 + t2601)) / t27 / t213 / t83 / z /
     # 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh11J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t4 = z + x1 * t1
      t5 = 0.1D1 / t4
      t6 = x1 * t5
      t7 = 0.1D1 - x2
      t8 = x3 * t7
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t19 = 0.2D1 * t13 * t17
      t20 = t8 * t4 + x2 * t10 - t19
      t23 = 0.1D1 - x1
      t26 = s - t2 * t6 * t20 - t2 * t23 * x3
      t27 = t26 ** 2
      t28 = s ** 2
      t29 = t28 ** 2
      t30 = t29 * s
      t31 = t27 * t30
      t34 = x2 * x3
      t35 = t10 * t7 * t4 + t34 + t19
      t40 = s - t2 * t6 * t35 - t2 * t23 * t10
      t41 = t40 ** 2
      t42 = t1 ** 2
      t43 = t42 ** 2
      t44 = t43 * t42
      t45 = t41 * t44
      t46 = t31 * t45
      t47 = x1 ** 2
      t48 = t47 ** 2
      t49 = x2 ** 2
      t50 = t48 * t49
      t51 = t23 ** 2
      t52 = t4 ** 2
      t54 = 0.1D1 / t52 / t4
      t55 = t51 * t54
      t60 = t42 * t1
      t61 = t60 * t23
      t62 = t31 * t61
      t65 = z + x1 * t7 * t1
      t66 = t65 * t54
      t67 = t41 * t47
      t68 = t35 ** 2
      t73 = t29 * t28
      t74 = t27 * t73
      t75 = t51 * t23
      t76 = t44 * t75
      t77 = t76 * t65
      t78 = t74 * t77
      t79 = t52 ** 2
      t80 = 0.1D1 / t79
      t81 = t80 * t40
      t83 = t47 * x1
      t89 = t43 * t51
      t90 = t89 * t65
      t91 = t74 * t90
      t92 = 0.1D1 / t52
      t93 = t92 * t40
      t94 = t47 * t35
      t95 = t94 * t10
      t99 = t43 * t23
      t100 = t99 * t65
      t101 = t74 * t100
      t102 = t54 * t40
      t104 = t83 * t20 * t35
      t108 = t43 * t1
      t110 = t108 * t51 * t65
      t112 = t83 * x2
      t113 = t112 * t68
      t117 = t26 * t73
      t119 = t80 * t41
      t120 = t20 ** 2
      t122 = t119 * t112 * t120
      t125 = t94 * x3
      t129 = t43 * t75
      t130 = t31 * t129
      t131 = t65 * t5
      t132 = t41 * x1
      t133 = t10 ** 2
      t138 = t31 * t77
      t139 = t49 * t83
      t144 = t51 ** 2
      t146 = t44 * t144 * t65
      t147 = t31 * t146
      t148 = t54 * t41
      t149 = t49 * t47
      t154 = t74 * t99
      t155 = t65 * t80
      t156 = t40 * t83
      t157 = t68 * t35
      t162 = t117 * t99
      t163 = t41 * t83
      t164 = t120 * t20
      t166 = t155 * t163 * t164
      t169 = -0.48D2 * t46 * t50 * t55 * t35 + 0.32D2 * t62 * t66 * t67 
     #* t68 + 0.64D2 * t78 * t81 * x2 * t83 * t68 * x3 + 0.256D3 * t91 *
     # t93 * t95 - 0.64D2 * t101 * t102 * t104 + 0.176D3 * t74 * t110 * 
     #t81 * t113 + 0.176D3 * t117 * t110 * t122 - 0.32D2 * t91 * t93 * t
     #125 + 0.32D2 * t130 * t131 * t132 * t133 + 0.16D2 * t138 * t119 * 
     #t139 * t35 - 0.48D2 * t147 * t148 * t149 * x3 - 0.32D2 * t154 * t1
     #55 * t156 * t157 - 0.32D2 * t162 * t166
      t172 = x2 * t51
      t180 = x2 * t75
      t181 = t92 * t35
      t182 = x3 ** 2
      t187 = t31 * t90
      t188 = t92 * t41
      t189 = t47 * t20
      t190 = t189 * x3
      t191 = t188 * t190
      t194 = t31 * t110
      t200 = t108 * t75 * t65
      t201 = t31 * t200
      t202 = t47 * x2
      t207 = t74 * t146
      t212 = t28 * s
      t213 = t29 * t212
      t214 = z ** 2
      t215 = t213 * t214
      t216 = t215 * t146
      t217 = t92 * t47
      t223 = t27 * t43
      t225 = t51 * t65
      t235 = t73 * z
      t236 = t223 * t51
      t238 = t65 * t92
      t239 = t40 * t47
      t241 = t238 * t239 * x2
      t245 = t66 * t156 * t68
      t248 = t129 * t65
      t249 = t117 * t248
      t250 = t5 * t41
      t253 = z * x3 * t10
      t257 = t117 * t100
      t258 = t148 * t83
      t259 = z * t20
      t260 = t259 * t35
      t264 = -0.32D2 * t117 * t45 * t48 * t172 * t54 * t68 * x3 + 0.64D2
     # * t117 * t45 * t83 * t180 * t181 * t182 - 0.64D2 * t187 * t191 + 
     #0.48D2 * t194 * t148 * t112 * t20 + 0.48D2 * t201 * t188 * t202 * 
     #t10 - 0.32D2 * t207 * t93 * t202 * t133 - 0.8D1 * t216 * t217 * x2
     # * t27 * t133 - 0.72D2 * t215 * t223 * t225 * t217 * x2 + 0.160D3 
     #* t31 * t76 * t66 * t163 * t49 - 0.288D3 * t235 * t236 * t241 + 0.
     #64D2 * t154 * t245 - 0.16D2 * t249 * t250 * x1 * t253 - 0.16D2 * t
     #257 * t258 * t260
      t266 = t60 * t51
      t268 = t31 * t266 * t65
      t269 = x1 * t20
      t270 = t269 * x3
      t274 = t75 * t92
      t279 = t41 * t108
      t280 = t279 * t83
      t283 = t172 * t181 * x3
      t286 = x1 * z
      t291 = t49 * x2
      t293 = t75 * t54
      t297 = t74 * t61
      t302 = t74 * t266
      t303 = t40 * x1
      t308 = t102 * t83
      t309 = x2 * t35
      t310 = t309 * x3
      t314 = t117 * t61
      t316 = t238 * t67 * t35
      t319 = t117 * t266
      t321 = t131 * t132 * x3
      t324 = t108 * t23
      t331 = t31 * t99
      t338 = -0.16D2 * t268 * t188 * t270 + 0.16D2 * t46 * t139 * t274 *
     # x3 - 0.352D3 * t117 * t280 * t283 + 0.96D2 * t249 * t250 * t286 *
     # t182 - 0.176D3 * t46 * t291 * t83 * t293 - 0.16D2 * t297 * t238 *
     # t239 * t35 + 0.48D2 * t302 * t131 * t303 * x3 - 0.16D2 * t78 * t3
     #08 * t310 + 0.48D2 * t314 * t316 - 0.16D2 * t319 * t321 + 0.64D2 *
     # t117 * t324 * t155 * t41 * t48 * t164 - 0.64D2 * t331 * t166 - 0.
     #64D2 * t331 * t155 * t163 * t157
      t339 = t41 * t60
      t340 = t31 * t339
      t346 = t148 * x2
      t350 = t40 * t44
      t360 = t92 * t20
      t365 = t117 * t90
      t366 = t188 * t125
      t369 = t47 * t5
      t370 = t35 * t23
      t371 = t370 * t10
      t375 = t20 * t23
      t376 = t375 * x3
      t380 = t117 * t279
      t381 = t48 * x2
      t382 = t23 * t54
      t388 = t144 * t5
      t389 = t182 * x3
      t394 = t188 * t47
      t395 = z * t35
      t396 = t395 * x3
      t404 = t74 * t248
      t405 = t5 * t40
      t410 = t26 * t213
      t414 = t43 ** 2
      t417 = t40 * t48
      t422 = 0.64D2 * t340 * x1 * t51 * x3 * t10 + 0.32D2 * t201 * t346 
     #* t125 - 0.32D2 * t74 * t350 * t48 * t172 * t54 * t120 * t10 + 0.6
     #4D2 * t74 * t350 * t83 * t180 * t360 * t133 - 0.32D2 * t365 * t366
     # - 0.16D2 * t340 * t369 * t371 - 0.16D2 * t340 * t369 * t376 + 0.1
     #76D3 * t380 * t381 * t382 * t68 - 0.32D2 * t117 * t45 * t202 * t38
     #8 * t389 + 0.16D2 * t365 * t394 * t396 + 0.64D2 * t365 * t394 * t2
     #59 * x3 - 0.16D2 * t404 * t405 * x1 * t253 - 0.176D3 * t410 * t89 
     #* t241 - 0.176D3 * t410 * t414 * t144 * t155 * t417 * t291
      t432 = t31 * t280
      t437 = x1 * x3 * t10
      t438 = t250 * t437
      t446 = t30 * z
      t455 = t215 * t77
      t457 = t54 * t83 * x2
      t460 = t457 * t41 * x3 * t20
      t465 = x1 * x2
      t471 = t27 * t26 * t43
      t479 = t131 * t303 * t133
      t482 = t26 * t43
      t485 = t41 * t40
      t492 = t31 * t61 * t65
      t494 = t189 * t35
      t498 = t51 * t133
      t502 = -0.256D3 * t31 * t279 * t47 * t180 * t5 * x3 * t10 + 0.32D2
     # * t432 * t283 - 0.64D2 * t31 * t248 * t438 - 0.192D3 * t187 * t36
     #6 + 0.64D2 * t201 * t346 * t190 + 0.288D3 * t446 * t27 * t41 * t60
     # * t47 * x2 * t23 * t5 + 0.16D2 * t455 * t460 - 0.176D3 * t31 * t4
     #1 * t42 * t465 * t23 * t5 - 0.16D2 * t446 * t471 * t23 * t245 - 0.
     #16D2 * t446 * t471 * t75 * t479 - 0.16D2 * t446 * t482 * t75 * t13
     #1 * t485 * x1 * t182 - 0.64D2 * t492 * t148 * z * t494 - 0.96D2 * 
     #t340 * t286 * t498
      t503 = t51 * t182
      t507 = t446 * t236
      t508 = t238 * t41
      t510 = t47 * t10 * t20
      t514 = t482 * t51
      t521 = t44 * t51 * t65
      t522 = t74 * t521
      t527 = t117 * t521
      t529 = 0.1D1 / t79 / t4
      t541 = t31 * t339 * t47
      t542 = z * t5
      t543 = t375 * t10
      t547 = t370 * x3
      t551 = t108 * t144
      t553 = t133 * t10
      t559 = t238 * t67 * t20
      t562 = x1 * t35
      t563 = t562 * x3
      t567 = t47 * t68
      t572 = -0.96D2 * t340 * t286 * t503 - 0.32D2 * t507 * t508 * t510 
     #+ 0.32D2 * t446 * t514 * t238 * t485 * t190 - 0.32D2 * t522 * t81 
     #* t381 * t68 - 0.32D2 * t527 * t529 * t41 * t381 * t164 - 0.64D2 *
     # t194 * t122 - 0.64D2 * t187 * t188 * t95 - 0.16D2 * t541 * t542 *
     # t543 - 0.16D2 * t541 * t542 * t547 + 0.64D2 * t74 * t551 * t131 *
     # t303 * t553 + 0.48D2 * t62 * t559 + 0.16D2 * t268 * t188 * t563 -
     # 0.64D2 * t187 * t148 * t567 * t10
      t574 = t23 * t42
      t576 = t131 * t132
      t584 = t324 * t65
      t591 = t551 * t65
      t592 = t117 * t591
      t598 = z * t47
      t608 = t188 * z
      t609 = t562 * t10
      t614 = t74 * t40 * t43
      t615 = t83 * t92
      t621 = t51 * t20
      t629 = t148 * t104
      t632 = t215 * t44
      t635 = t632 * t75 * t65 * t54
      t636 = t112 * t35
      t637 = t26 * t40
      t646 = 0.160D3 * t31 * t574 * t576 + 0.64D2 * t74 * t324 * t155 * 
     #t417 * t157 + 0.64D2 * t117 * t584 * t119 * t48 * t120 * t35 + 0.6
     #4D2 * t592 * t250 * x1 * t182 * t10 - 0.96D2 * t492 * t148 * t598 
     #* t120 - 0.32D2 * t74 * t350 * t202 * t388 * t553 + 0.16D2 * t268 
     #* t608 * t609 - 0.32D2 * t614 * t615 * t120 * t23 * t10 + 0.64D2 *
     # t614 * t369 * t621 * t133 - 0.64D2 * t404 * t405 * t437 - 0.64D2 
     #* t257 * t629 - 0.16D2 * t635 * t636 * t637 * x3 + 0.64D2 * t432 *
     # t172 * t181 * t10
      t650 = t83 * z
      t681 = t188 * t510
      t684 = x2 * t20
      t685 = t684 * t10
      t702 = t102 * x2
      t707 = 0.16D2 * t541 * t542 * t371 + 0.96D2 * t257 * t148 * t650 *
     # t120 - 0.32D2 * t507 * t508 * t125 + 0.16D2 * t340 * t369 * t543 
     #- 0.16D2 * t446 * t482 * t23 * t66 * t485 * t83 * t120 + 0.32D2 * 
     #t446 * t471 * t51 * t238 * t40 * t95 - 0.64D2 * t194 * t119 * t113
     # - 0.48D2 * t147 * t148 * t149 * t10 - 0.32D2 * t365 * t681 - 0.16
     #D2 * t78 * t308 * t685 - 0.64D2 * t78 * t308 * t309 * t10 - 0.64D2
     # * t340 * t286 * t51 * x3 * t10 + 0.64D2 * t91 * t102 * t567 * x3 
     #- 0.32D2 * t207 * t702 * t94 * t182
      t711 = t73 * t214
      t712 = t27 * t60
      t719 = t269 * t10
      t723 = t41 * t43
      t724 = t31 * t723
      t729 = t43 * t60
      t736 = t40 * t108
      t737 = t74 * t736
      t742 = t31 * t279
      t743 = t75 * t5
      t745 = t202 * t743 * t182
      t767 = t10 * t40
      t781 = t31 * t41
      t782 = t60 * x1
      t783 = t782 * t503
      t786 = 0.64D2 * t711 * t712 * t23 * t66 * t239 * t68 - 0.16D2 * t2
     #68 * t608 * t719 + 0.352D3 * t724 * t149 * t51 * t92 + 0.80D2 * t3
     #1 * t41 * t729 * t48 * t291 * t293 + 0.176D3 * t737 * t381 * t382 
     #* t120 - 0.64D2 * t742 * t745 + 0.16D2 * t46 * t139 * t274 * t10 -
     # 0.48D2 * t46 * t50 * t55 * t20 + 0.16D2 * t632 * t225 * t80 * t38
     #1 * t35 * t637 * t20 + 0.16D2 * t632 * t144 * t65 * t92 * t202 * t
     #26 * t767 * x3 - 0.16D2 * t635 * t112 * t26 * t767 * t20 + 0.48D2 
     #* t201 * t188 * t202 * x3 + 0.32D2 * t781 * t783
      t787 = t43 * x1
      t789 = t787 * t75 * t389
      t795 = t782 * t498
      t799 = t787 * t75 * t553
      t825 = t119 * t381 * t120
      t829 = t131 * t132 * t182
      t832 = -0.64D2 * t781 * t789 - 0.32D2 * t117 * t41 * t789 + 0.32D2
     # * t781 * t795 - 0.64D2 * t781 * t799 - 0.32D2 * t74 * t40 * t799 
     #- 0.16D2 * t268 * t608 * t563 + 0.16D2 * t268 * t608 * t270 + 0.64
     #D2 * t711 * t27 * t40 * t795 + 0.64D2 * t711 * t26 * t41 * t783 + 
     #0.176D3 * t380 * t745 + 0.16D2 * t541 * t542 * t376 - 0.32D2 * t52
     #7 * t825 + 0.32D2 * t130 * t829
      t841 = t117 * t146
      t843 = t188 * t202 * t182
      t853 = t117 * t77
      t860 = t119 * x2
      t867 = t47 * t120
      t878 = t202 * t743 * t133
      t883 = 0.32D2 * t331 * t66 * t163 * t68 - 0.32D2 * t91 * t93 * t51
     #0 - 0.32D2 * t841 * t843 + 0.48D2 * t194 * t148 * t636 + 0.16D2 * 
     #t138 * t119 * t139 * t20 - 0.16D2 * t853 * t258 * t685 + 0.64D2 * 
     #t201 * t346 * t95 - 0.256D3 * t194 * t860 * t104 + 0.16D2 * t340 *
     # t369 * t547 - 0.64D2 * t187 * t148 * t867 * x3 + 0.64D2 * t117 * 
     #t551 * t131 * t132 * t389 + 0.176D3 * t737 * t878 - 0.64D2 * t742 
     #* t878
      t887 = t131 * t132 * t10
      t890 = t93 * t47
      t932 = t74 * t591
      t942 = -0.16D2 * t314 * t559 + 0.48D2 * t319 * t887 + 0.64D2 * t91
     # * t890 * t395 * t10 + 0.96D2 * t404 * t405 * t286 * t133 + 0.96D2
     # * t101 * t102 * t650 * t68 + 0.48D2 * t297 * t238 * t239 * t20 - 
     #0.16D2 * t302 * t131 * t303 * t10 - 0.16D2 * t101 * t308 * t260 + 
     #0.64D2 * t853 * t860 * t83 * t120 * t10 - 0.32D2 * t841 * t346 * t
     #189 * t133 + 0.64D2 * t117 * t129 * t829 + 0.64D2 * t74 * t584 * t
     #81 * t48 * t20 * t68 + 0.64D2 * t932 * t405 * x1 * t133 * x3 + 0.6
     #4D2 * t432 * t172 * t360 * x3
      t963 = t66 * t67 * t120
      t966 = t346 * t510
      t969 = t31 * t266
      t973 = t66 * t163 * t120
      t988 = t35 * t51
      t997 = 0.352D3 * t410 * t76 * t66 * t156 * t49 + 0.80D2 * t31 * t7
     #29 * t144 * t155 * t41 * t291 * t83 - 0.192D3 * t187 * t681 + 0.64
     #D2 * t711 * t26 * t60 * t23 * t963 + 0.32D2 * t201 * t966 + 0.48D2
     # * t969 * t887 + 0.32D2 * t331 * t973 - 0.352D3 * t74 * t200 * t70
     #2 * t125 - 0.352D3 * t117 * t200 * t966 + 0.64D2 * t162 * t973 - 0
     #.64D2 * t31 * t100 * t629 - 0.64D2 * t724 * t369 * t988 * t133 - 0
     #.64D2 * t724 * t369 * t621 * t182
      t1003 = t238 * t67 * x2
      t1015 = t117 * t723
      t1020 = t34 * t10
      t1029 = t684 * t35
      t1050 = t215 * t521
      t1053 = 0.64D2 * t492 * t148 * t494 - 0.72D2 * t215 * t89 * t1003 
     #- 0.8D1 * t216 * t843 + 0.288D3 * t446 * t712 * t51 * t238 * t41 *
     # x2 * x1 + 0.64D2 * t1015 * t369 * t988 * t182 + 0.16D2 * t841 * t
     #394 * t1020 + 0.176D3 * t932 * t93 * t465 * t182 + 0.16D2 * t527 *
     # t119 * t48 * t1029 - 0.32D2 * t404 * t93 * t562 * t182 + 0.64D2 *
     # t365 * t148 * t867 * t10 - 0.16D2 * t853 * t258 * t310 + 0.16D2 *
     # t268 * t188 * t719 - 0.16D2 * t268 * t188 * t609 - 0.8D1 * t1050 
     #* t825
      t1101 = 0.16D2 * t455 * t457 * t35 * t27 * t10 - 0.288D3 * t235 * 
     #t514 * t1003 - 0.8D1 * t1050 * t80 * t48 * x2 * t68 * t27 + 0.64D2
     # * t74 * t129 * t479 - 0.64D2 * t853 * t460 + 0.864D3 * t31 * t89 
     #* t1003 + 0.176D3 * t592 * t188 * t465 * t133 - 0.32D2 * t522 * t5
     #29 * t40 * t381 * t157 - 0.32D2 * t249 * t188 * t269 * t133 - 0.80
     #D2 * t74 * t574 * t131 * t303 - 0.80D2 * t117 * t574 * t576 + 0.25
     #6D3 * t365 * t191 - 0.64D2 * t249 * t438
      t1113 = t172 * t360 * t10
      t1125 = t259 * t10
      t1154 = -0.96D2 * t492 * t148 * t598 * t68 + 0.48D2 * t969 * t321 
     #+ 0.48D2 * t62 * t316 - 0.352D3 * t74 * t736 * t83 * t1113 + 0.32D
     #2 * t432 * t1113 + 0.16D2 * t522 * t81 * t48 * t1029 + 0.16D2 * t2
     #07 * t890 * t1020 + 0.16D2 * t91 * t890 * t1125 + 0.16D2 * t91 * t
     #890 * t396 + 0.32D2 * t62 * t963 + 0.16D2 * t365 * t394 * t1125 + 
     #0.32D2 * t446 * t223 * t23 * t66 * t41 * t104 + 0.32D2 * t446 * t2
     #23 * t75 * t131 * t41 * t437 - 0.32D2 * t1015 * t615 * t68 * t23 *
     # x3
      rrgg2ggh11J7 = -0.45D2 / 0.16D2 * wd * (t169 + t264 + t338 + t422 
     #+ t502 + t572 + t646 + t707 + t786 + t832 + t883 + t942 + t997 + t
     #1053 + t1101 + t1154) / t27 / t212 / t41 / z / 0.3141592653589793D
     #1

      end function
  
 