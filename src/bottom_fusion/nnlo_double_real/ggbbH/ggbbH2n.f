  
      subroutine ggbbH2n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision ggbbH21J1  
      doubleprecision ggbbH21J2  
      doubleprecision ggbbH21J3  
      doubleprecision ggbbH2n1e1  
      doubleprecision ggbbH2n1e0  
      doubleprecision ggbbH2n1em1  
      doubleprecision ggbbH2n1em2  
      doubleprecision ggbbH2n1em3  
      doubleprecision ggbbH2n1em4  
      doubleprecision ggbbH2n2e1  
      doubleprecision ggbbH2n2e0  
      doubleprecision ggbbH2n2em1  
      doubleprecision ggbbH2n2em2  
      doubleprecision ggbbH2n2em3  
      doubleprecision ggbbH2n2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=ggbbH2n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=ggbbH2n2e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=ggbbH2n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=ggbbH2n2e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=ggbbH2n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=ggbbH2n2em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=ggbbH2n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=ggbbH2n2em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=ggbbH2n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=ggbbH2n2em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=ggbbH2n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=ggbbH2n2em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function ggbbH2n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH21J1
      doubleprecision ggbbH21J2
      doubleprecision ggbbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t7 = x2 * 0.3141592653589793D1
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t13 = t4 * x4
      t16 = log(-0.4D1 * t12 * t13)
      t19 = t1 ** 2
      t21 = 0.1D1 / s
      t22 = ggbbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t26 = lh ** 2
      t27 = 0.180D3 * t26
      t28 = 0.3141592653589793D1 ** 2
      t29 = 0.30D2 * t28
      t32 = t16 ** 2
      t36 = ggbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t44 = t27 - t29
      t52 = ggbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t53 = t21 * t52
      t56 = lh * t19
      t57 = 0.1D1 - x3
      t58 = ggbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t59 = x3 * t9
      t61 = -t57
      t65 = log(0.4D1 * t59 * t11 * t13 * t61)
      t66 = ggbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t68 = t11 * x4
      t69 = t68 * t4
      t72 = log(-0.4D1 * t59 * t69)
      t78 = t19 * t21
      t80 = t72 ** 2
      t83 = ggbbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t57, x4)
      t85 = t65 ** 2
      t91 = t44 * t19
      t93 = t21 * (t66 - t52)
      t96 = 0.1D1 / x3
      t99 = x1 ** 2
      t100 = x3 * t99
      t101 = t100 * t9
      t104 = log(-0.4D1 * t101 * t69)
      t110 = log(0.4D1 * t101 * t68 * t4 * t61)
      t119 = 0.1D1 / x1
      t122 = t99 * t9
      t125 = log(-0.4D1 * t122 * t69)
      t132 = t125 ** 2
      t142 = (-0.180D3 * lh - 0.90D2 * t16) * t19 * t21 * t22 / 0.5760D4
     # + (t27 - t29 + 0.180D3 * t16 * lh + 0.45D2 * t32) * t19 * t21 * t
     #36 / 0.5760D4 + (0.60D2 * lh * t28 - 0.2884936567583026D3 - 0.120D
     #3 * t26 * lh - t16 * t44 - 0.90D2 * t32 * lh - 0.15D2 * t32 * t16)
     # * t19 * t53 / 0.5760D4 - (-0.180D3 * t56 * t21 * (t58 - t65 * t66
     # - t36 + t72 * t52) + 0.90D2 * t78 * (-t22 + t72 * t36 - t80 * t52
     # / 0.2D1 + t83 - t65 * t58 + t85 * t66 / 0.2D1) + t91 * t93) * t96
     # / 0.5760D4 - (0.90D2 * t78 * (-t36 + t104 * t52 + t58 - t110 * t6
     #6) - 0.180D3 * t56 * t93) * t96 * t119 / 0.2880D4 + (-0.180D3 * t5
     #6 * t21 * (t36 - t125 * t52) + 0.90D2 * t78 * (t22 - t125 * t36 + 
     #t132 * t52 / 0.2D1) + t91 * t53) * t119 / 0.2880D4
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
      t162 = s * t157 * t19 * t160 * t4
      t163 = t151 * t157
      t165 = 0.1D1 / (-0.2D1 + t147)
      t166 = ggbbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t168 = t163 * t165 * t166
      t169 = t100 * t12
      t170 = t151 ** 2
      t171 = t157 ** 2
      t173 = t13 * t170 * t171
      t176 = log(-0.4D1 * t169 * t173)
      t178 = t157 * t165
      t179 = ggbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t180 = t178 * t179
      t185 = t56 * t21
      t187 = t163 * t165 * t179
      t196 = log(-0.4D1 * t122 * t11 * t173)
      t197 = t196 * t151
      t203 = ggbbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t208 = t196 ** 2
      t220 = -(0.90D2 * t78 * (t168 - t176 * t151 * t180) - 0.180D3 * t1
     #85 * t187) * t96 * t119 / 0.2880D4 + (0.180D3 * t56 * t21 * (t168 
     #- t197 * t180) - 0.90D2 * t78 * (t163 * t165 * t203 - t197 * t178 
     #* t166 + t208 * t151 * t180 / 0.2D1) - t91 * t21 * t187) * t119 / 
     #0.2880D4
      t221 = FJET(XB1, XB2, s, t150, 0.0D0, -t154, t156, t162, t220)
      t223 = FJET(XB1, XB2, s, -t154, t156, t150, 0.0D0, t162, t220)
      t225 = KAPPA2(x1, x2, t57, x4, z)
      t226 = s * t225
      t228 = t226 * t149 * t61
      t230 = t226 * t149 * x3
      t231 = t226 * t153
      t232 = t226 * t155
      t233 = t225 ** 2
      t238 = cos(t7)
      t241 = sqrt(x3 * t61 * t13)
      t246 = s * t233 * t19 * t160 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4
     # + 0.2D1 * t238 * t241)
      t247 = t151 * t233
      t249 = 0.1D1 / (-0.2D1 + t225)
      t250 = ggbbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, t57, x4)
      t254 = t233 ** 2
      t259 = log(0.4D1 * t169 * t13 * t170 * t61 * t254)
      t262 = ggbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t57, x4)
      t272 = -0.90D2 * t78 * (t247 * t249 * t250 - t259 * t151 * t233 * 
     #t249 * t262) + 0.180D3 * t185 * t247 * t249 * t262
      t275 = t272 * t96 * t119 / 0.2880D4
      t276 = FJET(XB1, XB2, s, -t228, t230, -t231, t232, t246, -t275)
      t278 = t96 * t119
      t281 = FJET(XB1, XB2, s, -t231, t232, -t228, t230, t246, -t275)
      ggbbH2n1e1 = t143 * t142 + t145 * t142 + t221 * t220 + t223 * t220
     # - t276 * t272 * t278 / 0.2880D4 - t281 * t272 * t278 / 0.2880D4

      end function



      doubleprecision function ggbbH2n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH21J1
      doubleprecision ggbbH21J2
      doubleprecision ggbbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = 0.1D1 - x3
      t10 = ggbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t9, x4)
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = x3 * t13
      t15 = z ** 2
      t16 = 0.1D1 / t15
      t18 = t4 * x4
      t19 = -t9
      t23 = log(0.4D1 * t14 * t16 * t18 * t19)
      t24 = ggbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, t9, x4)
      t26 = ggbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t28 = t16 * x4 * t4
      t31 = log(-0.4D1 * t14 * t28)
      t32 = ggbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t37 = lh * t6
      t38 = t24 - t32
      t43 = 0.1D1 / x3
      t47 = 0.1D1 / x1
      t51 = x1 ** 2
      t52 = t51 * t13
      t55 = log(-0.4D1 * t52 * t28)
      t60 = t7 * t32
      t66 = ggbbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t73 = log(-0.4D1 * t13 * t16 * t18)
      t80 = lh ** 2
      t82 = 0.3141592653589793D1 ** 2
      t86 = t73 ** 2
      t92 = -(0.90D2 * t8 * (t10 - t23 * t24 - t26 + t31 * t32) - 0.180D
     #3 * t37 * t7 * t38) * t43 / 0.5760D4 - t8 * t38 * t43 * t47 / 0.32
     #D2 + (0.90D2 * t8 * (t26 - t55 * t32) - 0.180D3 * t37 * t60) * t47
     # / 0.2880D4 + t8 * t66 / 0.64D2 + (-0.180D3 * lh - 0.90D2 * t73) *
     # t6 * t7 * t26 / 0.5760D4 + (0.180D3 * t80 - 0.30D2 * t82 + 0.180D
     #3 * t73 * lh + 0.45D2 * t86) * t6 * t60 / 0.5760D4
      t93 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t92)
      t95 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t92)
      t97 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t98 = s * t97
      t99 = t1 * x1
      t100 = t98 * t99
      t101 = -0.1D1 + x1
      t102 = t1 * t101
      t103 = t102 * x4
      t104 = t98 * t103
      t105 = t102 * t4
      t106 = t98 * t105
      t107 = t97 ** 2
      t110 = x1 * t101
      t112 = s * t107 * t6 * t110 * t4
      t113 = t101 * t107
      t116 = 0.1D1 / (-0.2D1 + t97)
      t117 = ggbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t118 = t116 * t117
      t119 = t43 * t47
      t123 = ggbbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t127 = t107 ** 2
      t128 = t101 ** 2
      t133 = log(-0.4D1 * t52 * t16 * t18 * t127 * t128)
      t148 = -t8 * t113 * t118 * t119 / 0.32D2 + (-0.90D2 * t8 * (t113 *
     # t116 * t123 - t133 * t101 * t107 * t116 * t117) + 0.180D3 * t37 *
     # t7 * t113 * t118) * t47 / 0.2880D4
      t149 = FJET(XB1, XB2, s, t100, 0.0D0, -t104, t106, t112, t148)
      t151 = FJET(XB1, XB2, s, -t104, t106, t100, 0.0D0, t112, t148)
      t153 = KAPPA2(x1, x2, t9, x4, z)
      t154 = s * t153
      t156 = t154 * t99 * t19
      t158 = t154 * t99 * x3
      t159 = t154 * t103
      t160 = t154 * t105
      t161 = t153 ** 2
      t166 = cos(t11)
      t169 = sqrt(x3 * t19 * t18)
      t174 = s * t161 * t6 * t110 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4 
     #+ 0.2D1 * t166 * t169)
      t178 = 0.1D1 / (-0.2D1 + t153)
      t179 = ggbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t9, x4)
      t183 = t8 * t101 * t161 * t178 * t179 * t119 / 0.32D2
      t184 = FJET(XB1, XB2, s, -t156, t158, -t159, t160, t174, t183)
      t186 = t7 * t101
      t191 = t161 * t178 * t179 * t43 * t47
      t194 = FJET(XB1, XB2, s, -t159, t160, -t156, t158, t174, t183)
      ggbbH2n1e0 = t93 * t92 + t95 * t92 + t148 * t149 + t151 * t148 + t
     #184 * t6 * t186 * t191 / 0.32D2 + t194 * t6 * t186 * t191 / 0.32D2

      end function



      doubleprecision function ggbbH2n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH21J1
      doubleprecision ggbbH21J2
      doubleprecision ggbbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = ggbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t14 = sin(x2 * 0.3141592653589793D1)
      t15 = t14 ** 2
      t16 = z ** 2
      t22 = log(-0.4D1 * t15 / t16 * x4 * t4)
      t26 = ggbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t31 = ggbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.1D1 - x3, x4)
      t37 = 0.1D1 / x1
      t41 = t8 * t9 / 0.64D2 + (-0.180D3 * lh - 0.90D2 * t22) * t6 * t7 
     #* t26 / 0.5760D4 - t8 * (t31 - t26) / x3 / 0.64D2 + t8 * t26 * t37
     # / 0.32D2
      t42 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t41)
      t44 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t41)
      t46 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t47 = s * t46
      t49 = t47 * t1 * x1
      t50 = -0.1D1 + x1
      t51 = t1 * t50
      t53 = t47 * t51 * x4
      t55 = t47 * t51 * t4
      t56 = t46 ** 2
      t61 = s * t56 * t6 * x1 * t50 * t4
      t66 = ggbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t68 = t56 / (-0.2D1 + t46) * t66 * t37
      t70 = t8 * t50 * t68 / 0.32D2
      t71 = FJET(XB1, XB2, s, t49, 0.0D0, -t53, t55, t61, -t70)
      t73 = t7 * t50
      t77 = FJET(XB1, XB2, s, -t53, t55, t49, 0.0D0, t61, -t70)
      ggbbH2n1em1 = t42 * t41 + t44 * t41 - t71 * t6 * t73 * t68 / 0.32D
     #2 - t77 * t6 * t73 * t68 / 0.32D2

      end function



      doubleprecision function ggbbH2n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH21J1
      doubleprecision ggbbH21J2
      doubleprecision ggbbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t9 = ggbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.10D1, x4)
      t11 = t6 * t7 * t9 / 0.64D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t11)
      t14 = t7 * t9
      t16 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t11)
      ggbbH2n1em2 = t12 * t6 * t14 / 0.64D2 + t16 * t6 * t14 / 0.64D2

      end function



      doubleprecision function ggbbH2n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH21J1
      doubleprecision ggbbH21J2
      doubleprecision ggbbH21J3
      ggbbH2n1em3 = 0.0D0

      end function



      doubleprecision function ggbbH2n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH21J1
      doubleprecision ggbbH21J2
      doubleprecision ggbbH21J3
      ggbbH2n1em4 = 0.0D0

      end function


      doubleprecision function ggbbH2n2e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH21J1
      doubleprecision ggbbH21J2
      doubleprecision ggbbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t7 = x2 * 0.3141592653589793D1
      t8 = sin(t7)
      t9 = t8 ** 2
      t10 = z ** 2
      t11 = 0.1D1 / t10
      t12 = t9 * t11
      t13 = x4 * t4
      t16 = log(-0.4D1 * t12 * t13)
      t19 = t1 ** 2
      t21 = 0.1D1 / s
      t22 = ggbbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t26 = lh ** 2
      t27 = 0.180D3 * t26
      t28 = 0.3141592653589793D1 ** 2
      t29 = 0.30D2 * t28
      t32 = t16 ** 2
      t36 = ggbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t44 = t27 - t29
      t52 = ggbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t53 = t21 * t52
      t56 = lh * t19
      t57 = ggbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t58 = x3 * t9
      t60 = -0.1D1 + x3
      t64 = log(0.4D1 * t58 * t11 * t13 * t60)
      t65 = ggbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t67 = t11 * x4
      t68 = t67 * t4
      t71 = log(-0.4D1 * t58 * t68)
      t77 = t19 * t21
      t79 = t71 ** 2
      t82 = ggbbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t84 = t64 ** 2
      t90 = t44 * t19
      t91 = t65 - t52
      t95 = 0.1D1 / x3
      t98 = x1 ** 2
      t99 = x3 * t98
      t100 = t99 * t9
      t105 = log(0.4D1 * t100 * t67 * t4 * t60)
      t109 = log(-0.4D1 * t100 * t68)
      t120 = 0.1D1 / x1
      t123 = t98 * t9
      t126 = log(-0.4D1 * t123 * t68)
      t133 = t126 ** 2
      t143 = (-0.180D3 * lh - 0.90D2 * t16) * t19 * t21 * t22 / 0.5760D4
     # + (t27 - t29 + 0.180D3 * t16 * lh + 0.45D2 * t32) * t19 * t21 * t
     #36 / 0.5760D4 + (0.60D2 * lh * t28 - 0.2884936567583026D3 - 0.120D
     #3 * t26 * lh - t16 * t44 - 0.90D2 * t32 * lh - 0.15D2 * t32 * t16)
     # * t19 * t53 / 0.5760D4 - (-0.180D3 * t56 * t21 * (t57 - t64 * t65
     # - t36 + t71 * t52) + 0.90D2 * t77 * (-t22 + t71 * t36 - t79 * t52
     # / 0.2D1 + t82 - t64 * t57 + t84 * t65 / 0.2D1) + t90 * t21 * t91)
     # * t95 / 0.5760D4 + (0.90D2 * t77 * (-t57 + t105 * t65 + t36 - t10
     #9 * t52) + 0.180D3 * t56 * t21 * t91) * t95 * t120 / 0.2880D4 - (-
     #0.180D3 * t56 * t21 * (-t36 + t126 * t52) + 0.90D2 * t77 * (-t22 +
     # t126 * t36 - t133 * t52 / 0.2D1) - t90 * t53) * t120 / 0.2880D4
      t144 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t143)
      t146 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t147 = s * t146
      t148 = t1 * x1
      t149 = t147 * t148
      t150 = -0.1D1 + x1
      t151 = t1 * t150
      t152 = t151 * x4
      t153 = t147 * t152
      t154 = t151 * t4
      t155 = t147 * t154
      t156 = t146 ** 2
      t159 = x1 * t150
      t161 = s * t156 * t19 * t159 * x4
      t162 = t150 * t156
      t164 = 0.1D1 / (-0.2D1 + t146)
      t165 = ggbbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t167 = t162 * t164 * t165
      t168 = t99 * t12
      t169 = t150 ** 2
      t170 = t156 ** 2
      t172 = t13 * t169 * t170
      t175 = log(-0.4D1 * t168 * t172)
      t177 = t156 * t164
      t178 = ggbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t179 = t177 * t178
      t184 = t56 * t21
      t186 = t162 * t164 * t178
      t195 = log(-0.4D1 * t123 * t11 * t172)
      t196 = t195 * t150
      t202 = ggbbH21J3(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t207 = t195 ** 2
      t219 = (0.90D2 * t77 * (-t167 + t175 * t150 * t179) + 0.180D3 * t1
     #84 * t186) * t95 * t120 / 0.2880D4 - (-0.180D3 * t56 * t21 * (t167
     # - t196 * t179) + 0.90D2 * t77 * (t162 * t164 * t202 - t196 * t177
     # * t165 + t207 * t150 * t179 / 0.2D1) + t90 * t21 * t186) * t120 /
     # 0.2880D4
      t220 = FJET(XB1, XB2, s, 0.0D0, t149, -t153, t155, -t161, t219)
      t222 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t143)
      t224 = KAPPA2(x1, x2, x3, x4, z)
      t225 = s * t224
      t227 = t225 * t148 * x3
      t229 = t225 * t148 * t60
      t230 = t225 * t152
      t231 = t225 * t154
      t232 = t224 ** 2
      t237 = cos(t7)
      t240 = sqrt(x3 * t60 * t13)
      t245 = s * t232 * t19 * t159 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1
     # * t237 * t240)
      t246 = t150 * t232
      t248 = 0.1D1 / (-0.2D1 + t224)
      t249 = ggbbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t253 = t232 ** 2
      t258 = log(0.4D1 * t168 * t13 * t169 * t60 * t253)
      t261 = ggbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t271 = 0.90D2 * t77 * (t246 * t248 * t249 - t258 * t150 * t232 * t
     #248 * t261) - 0.180D3 * t184 * t246 * t248 * t261
      t274 = t271 * t95 * t120 / 0.2880D4
      t275 = FJET(XB1, XB2, s, t227, -t229, -t230, t231, t245, t274)
      t277 = t95 * t120
      t280 = FJET(XB1, XB2, s, -t153, t155, 0.0D0, t149, -t161, t219)
      t282 = FJET(XB1, XB2, s, -t230, t231, t227, -t229, t245, t274)
      ggbbH2n2e1 = t144 * t143 + t220 * t219 + t222 * t143 + t275 * t271
     # * t277 / 0.2880D4 + t280 * t219 + t282 * t271 * t277 / 0.2880D4

      end function



      doubleprecision function ggbbH2n2e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH21J1
      doubleprecision ggbbH21J2
      doubleprecision ggbbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = ggbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t10 = x2 * 0.3141592653589793D1
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = x4 * t4
      t18 = -0.1D1 + x3
      t22 = log(0.4D1 * t13 * t15 * t17 * t18)
      t23 = ggbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t25 = ggbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t27 = t15 * x4 * t4
      t30 = log(-0.4D1 * t13 * t27)
      t31 = ggbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t36 = lh * t6
      t37 = t23 - t31
      t42 = 0.1D1 / x3
      t47 = 0.1D1 / x1
      t51 = x1 ** 2
      t52 = t51 * t12
      t55 = log(-0.4D1 * t52 * t27)
      t60 = t7 * t31
      t66 = ggbbH21J3(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t73 = log(-0.4D1 * t12 * t15 * t17)
      t80 = lh ** 2
      t82 = 0.3141592653589793D1 ** 2
      t86 = t73 ** 2
      t92 = -(0.90D2 * t8 * (t9 - t22 * t23 - t25 + t30 * t31) - 0.180D3
     # * t36 * t7 * t37) * t42 / 0.5760D4 - t8 * t37 * t42 * t47 / 0.32D
     #2 - (0.90D2 * t8 * (-t25 + t55 * t31) + 0.180D3 * t36 * t60) * t47
     # / 0.2880D4 + t8 * t66 / 0.64D2 + (-0.180D3 * lh - 0.90D2 * t73) *
     # t6 * t7 * t25 / 0.5760D4 + (0.180D3 * t80 - 0.30D2 * t82 + 0.180D
     #3 * t73 * lh + 0.45D2 * t86) * t6 * t60 / 0.5760D4
      t93 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t92)
      t95 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t96 = s * t95
      t97 = t1 * x1
      t98 = t96 * t97
      t99 = -0.1D1 + x1
      t100 = t1 * t99
      t101 = t100 * x4
      t102 = t96 * t101
      t103 = t100 * t4
      t104 = t96 * t103
      t105 = t95 ** 2
      t108 = x1 * t99
      t110 = s * t105 * t6 * t108 * x4
      t111 = t99 * t105
      t114 = 0.1D1 / (-0.2D1 + t95)
      t115 = ggbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t116 = t114 * t115
      t117 = t42 * t47
      t121 = ggbbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t125 = t105 ** 2
      t126 = t99 ** 2
      t131 = log(-0.4D1 * t52 * t15 * t17 * t125 * t126)
      t146 = -t8 * t111 * t116 * t117 / 0.32D2 - (0.90D2 * t8 * (t111 * 
     #t114 * t121 - t131 * t99 * t105 * t114 * t115) - 0.180D3 * t36 * t
     #7 * t111 * t116) * t47 / 0.2880D4
      t147 = FJET(XB1, XB2, s, 0.0D0, t98, -t102, t104, -t110, t146)
      t149 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t92)
      t151 = KAPPA2(x1, x2, x3, x4, z)
      t152 = s * t151
      t154 = t152 * t97 * x3
      t156 = t152 * t97 * t18
      t157 = t152 * t101
      t158 = t152 * t103
      t159 = t151 ** 2
      t164 = cos(t10)
      t167 = sqrt(x3 * t18 * t17)
      t172 = s * t159 * t6 * t108 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t164 * t167)
      t176 = 0.1D1 / (-0.2D1 + t151)
      t177 = ggbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t181 = t8 * t99 * t159 * t176 * t177 * t117 / 0.32D2
      t182 = FJET(XB1, XB2, s, t154, -t156, -t157, t158, t172, t181)
      t184 = t7 * t99
      t189 = t159 * t176 * t177 * t42 * t47
      t192 = FJET(XB1, XB2, s, -t102, t104, 0.0D0, t98, -t110, t146)
      t194 = FJET(XB1, XB2, s, -t157, t158, t154, -t156, t172, t181)
      ggbbH2n2e0 = t93 * t92 + t147 * t146 + t149 * t92 + t182 * t6 * t1
     #84 * t189 / 0.32D2 + t192 * t146 + t194 * t6 * t184 * t189 / 0.32D
     #2

      end function



      doubleprecision function ggbbH2n2em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH21J1
      doubleprecision ggbbH21J2
      doubleprecision ggbbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t8 = t6 * t7
      t9 = ggbbH21J2(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t14 = sin(x2 * 0.3141592653589793D1)
      t15 = t14 ** 2
      t16 = z ** 2
      t22 = log(-0.4D1 * t15 / t16 * x4 * t4)
      t26 = ggbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t30 = ggbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, x3, x4)
      t36 = 0.1D1 / x1
      t40 = t8 * t9 / 0.64D2 + (-0.180D3 * lh - 0.90D2 * t22) * t6 * t7 
     #* t26 / 0.5760D4 - t8 * (t30 - t26) / x3 / 0.64D2 + t8 * t26 * t36
     # / 0.32D2
      t41 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t40)
      t43 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t44 = s * t43
      t46 = t44 * t1 * x1
      t47 = -0.1D1 + x1
      t48 = t1 * t47
      t50 = t44 * t48 * x4
      t52 = t44 * t48 * t4
      t53 = t43 ** 2
      t58 = s * t53 * t6 * x1 * t47 * x4
      t63 = ggbbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.0D0, x4)
      t65 = t53 / (-0.2D1 + t43) * t63 * t36
      t67 = t8 * t47 * t65 / 0.32D2
      t68 = FJET(XB1, XB2, s, 0.0D0, t46, -t50, t52, -t58, -t67)
      t70 = t7 * t47
      t74 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t40)
      t76 = FJET(XB1, XB2, s, -t50, t52, 0.0D0, t46, -t58, -t67)
      ggbbH2n2em1 = t41 * t40 - t68 * t6 * t70 * t65 / 0.32D2 + t74 * t4
     #0 - t76 * t6 * t70 * t65 / 0.32D2

      end function



      doubleprecision function ggbbH2n2em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH21J1
      doubleprecision ggbbH21J2
      doubleprecision ggbbH21J3
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = t1 ** 2
      t7 = 0.1D1 / s
      t9 = ggbbH21J1(s, XB1, XB2, z, lh, wd, 0.0D0, x2, 0.0D0, x4)
      t11 = t6 * t7 * t9 / 0.64D2
      t12 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t11)
      t14 = t7 * t9
      t16 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t11)
      ggbbH2n2em2 = t12 * t6 * t14 / 0.64D2 + t16 * t6 * t14 / 0.64D2

      end function



      doubleprecision function ggbbH2n2em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH21J1
      doubleprecision ggbbH21J2
      doubleprecision ggbbH21J3
      ggbbH2n2em3 = 0.0D0

      end function



      doubleprecision function ggbbH2n2em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH21J1
      doubleprecision ggbbH21J2
      doubleprecision ggbbH21J3
      ggbbH2n2em4 = 0.0D0

      end function
  
 

      doubleprecision function ggbbH21J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t8 = 0.1D1 - x4
      t11 = s - t2 * t5 * x4 - t2 * t5 * t8
      t12 = s ** 2
      t13 = t11 * t12
      t14 = t1 ** 2
      t15 = t3 ** 2
      t16 = t14 * t15
      t17 = t13 * t16
      t18 = 0.1D1 - x3
      t19 = x1 * t18
      t20 = t3 * x1
      t21 = t20 * x3
      t25 = s - t2 * t21 - t2 * t20 * t18
      t26 = t25 ** 2
      t27 = t26 * t4
      t32 = t12 * s
      t33 = t26 * t32
      t34 = t33 * t16
      t35 = x3 * x1
      t37 = z * t4 * x4
      t38 = t35 * t37
      t41 = t32 * t14
      t42 = t15 * x1
      t43 = t41 * t42
      t44 = t18 * t26
      t48 = t11 * t26
      t52 = t20 * x3 * z
      t55 = t48 * t12
      t60 = t14 * t1
      t61 = t15 * t3
      t62 = t60 * t61
      t63 = t33 * t62
      t64 = t4 ** 2
      t65 = x4 ** 2
      t66 = t64 * t65
      t67 = t35 * t66
      t70 = t12 ** 2
      t72 = t61 * x1
      t74 = t18 * t25
      t78 = x4 * t4
      t82 = t32 * t60
      t83 = x1 ** 2
      t86 = t18 ** 2
      t87 = t86 * t26
      t91 = t35 * t78
      t93 = t70 * t25
      t97 = t93 * t16
      t99 = 0.18D2 * t17 * t19 * t27 * t8 - 0.18D2 * t34 * t38 + 0.18D2 
     #* t43 * t44 * t37 + 0.18D2 * t48 * t12 * t1 * t52 + 0.6D1 * t55 - 
     #0.6D1 * t48 * t12 * z - 0.9D1 * t63 * t67 + 0.9D1 * t70 * t60 * t7
     #2 * t74 * t66 + 0.19D2 * t43 * t44 * t78 + 0.18D2 * t82 * t61 * t8
     #3 * t87 * t78 - t34 * t91 + 0.9D1 * t93 * t62 * t67 + t97 * t91
      t101 = t70 * t14 * t42
      t105 = t11 * t32
      t106 = t105 * t16
      t107 = t25 * t4
      t117 = t32 * t1 * t3
      t122 = t70 * t1 * t3
      t126 = t33 * t1
      t128 = t93 * t1
      t130 = t33 * t14
      t131 = t15 * t83
      t132 = x3 ** 2
      t136 = t41 * t15
      t137 = t83 * t86
      t142 = t83 * x1
      t154 = t1 * t3
      t156 = t26 * z
      t157 = t19 * t156
      t161 = t48 * t12 * t14
      t162 = x3 * t4
      t167 = -0.19D2 * t101 * t74 * t78 - 0.9D1 * t106 * t19 * t107 * t8
     # + 0.9D1 * t106 * t19 * t107 * x4 - 0.11D2 * t117 * t19 * t26 + 0.
     #11D2 * t122 * t19 * t25 - t126 * t21 + t128 * t21 + 0.7D1 * t130 *
     # t131 * t132 - 0.11D2 * t136 * t137 * t26 - 0.9D1 * t33 * t60 * t6
     #1 * t142 * t132 * x3 - 0.9D1 * t82 * t61 * t142 * t86 * t18 * t26 
     #+ 0.36D2 * t13 * t154 * t157 + 0.18D2 * t161 * t42 * t162 * t8
      t169 = z ** 2
      t183 = t20 * x3 * t169
      t194 = t83 * t132
      t202 = t25 * z
      t203 = t19 * t202
      t207 = t154 * t35
      t219 = -0.18D2 * t117 * t19 * t26 * t169 - 0.3D1 * t55 * t154 * t4
     # * t8 - 0.18D2 * t130 * t131 * t132 * z - 0.18D2 * t126 * t183 + 0
     #.9D1 * t17 * t19 * t27 * x4 + 0.36D2 * t13 * t14 * t131 * t87 + 0.
     #18D2 * t55 * t16 * t194 - 0.18D2 * t136 * t137 * t156 - 0.27D2 * t
     #105 * t154 * t203 - 0.8D1 * t105 * t202 * t207 - 0.27D2 * t101 * t
     #74 * t37 + 0.27D2 * t97 * t38 - 0.9D1 * t82 * t72 * t44 * t66
      t236 = t70 * z
      t246 = t154 * t19
      t262 = -0.24D2 * t55 * t207 + 0.36D2 * t128 * t183 + 0.3D1 * t128 
     #* t52 - 0.20D2 * t126 * t52 - 0.30D2 * t117 * t157 + 0.30D2 * t122
     # * t203 - 0.10D2 * t105 * t1 * t20 * t74 - 0.8D1 * t236 * t14 * t1
     #31 * t132 * t11 + 0.36D2 * t122 * t19 * t25 * t169 - 0.14D2 * t55 
     #* t246 - 0.3D1 * t55 * t154 * t78 - 0.8D1 * t236 * t11 * t246 + 0.
     #9D1 * t161 * t42 * t162 * x4 - 0.18D2 * t63 * t194 * t78
      ggbbH21J1 = 0.16D2 / 0.3D1 * wd * (t99 + t167 + t219 + t262) / t11
     # / s / t26

      end function
  
   
 

      doubleprecision function ggbbH21J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t8 = 0.1D1 - x4
      t11 = s - t2 * t5 * x4 - t2 * t5 * t8
      t12 = s ** 2
      t13 = t11 * t12
      t14 = t1 ** 2
      t15 = t3 ** 2
      t16 = t14 * t15
      t17 = t13 * t16
      t18 = 0.1D1 - x3
      t19 = x1 * t18
      t20 = t3 * x1
      t21 = t20 * x3
      t25 = s - t2 * t21 - t2 * t20 * t18
      t26 = t25 ** 2
      t27 = t26 * t4
      t32 = t12 * s
      t34 = t32 * t1 * t3
      t38 = t12 ** 2
      t40 = t38 * t1 * t3
      t44 = t26 * t32
      t45 = t44 * t1
      t47 = t38 * t25
      t48 = t1 * t47
      t51 = x1 ** 2
      t52 = t15 * t51
      t53 = x3 ** 2
      t57 = t32 * t14
      t59 = t18 ** 2
      t64 = t14 * t1
      t66 = t15 * t3
      t67 = t51 * x1
      t73 = t32 * t64
      t80 = t11 * t32
      t81 = t80 * t16
      t82 = t25 * t4
      t90 = -0.18D2 * t17 * t19 * t27 * t8 + 0.11D2 * t34 * t19 * t26 - 
     #0.11D2 * t40 * t19 * t25 + t45 * t21 - t48 * t21 - 0.15D2 * t44 * 
     #t14 * t52 * t53 + 0.12D2 * t57 * t15 * t51 * t59 * t26 + 0.9D1 * t
     #44 * t64 * t66 * t67 * t53 * x3 + 0.9D1 * t73 * t66 * t67 * t59 * 
     #t18 * t26 + 0.8D1 * t81 * t19 * t82 * t8 - t81 * t19 * t82 * x4
      t91 = t11 * t26
      t92 = t91 * t12
      t96 = t59 * t26
      t97 = x4 * t4
      t102 = x3 * x1
      t103 = t102 * t97
      t106 = t91 * t12 * t14
      t107 = t15 * x1
      t108 = x3 * t4
      t110 = t107 * t108 * t8
      t114 = t107 * t108 * x4
      t117 = t1 * t3
      t119 = t25 * z
      t120 = t19 * t119
      t123 = t64 * t66
      t125 = t4 ** 2
      t126 = x4 ** 2
      t127 = t125 * t126
      t128 = t102 * t127
      t131 = t47 * t16
      t134 = t38 * t14 * t107
      t135 = t18 * t25
      t144 = t117 * t102
      t146 = -0.5D1 * t92 - 0.27D2 * t73 * t66 * t51 * t96 * t97 + t44 *
     # t16 * t103 - 0.18D2 * t106 * t110 - 0.9D1 * t106 * t114 - 0.8D1 *
     # t80 * t117 * t120 - 0.9D1 * t47 * t123 * t128 - t131 * t103 + 0.1
     #9D2 * t134 * t135 * t97 - 0.9D1 * t17 * t19 * t27 * x4 - t80 * t11
     #9 * t144
      t149 = z * t4 * x4
      t156 = t66 * x1
      t158 = t18 * t26
      t162 = t44 * t123
      t163 = t51 * t53
      t170 = t80 * t25 * t14
      t189 = 0.9D1 * t134 * t135 * t149 - 0.9D1 * t131 * t102 * t149 + 0
     #.9D1 * t73 * t156 * t158 * t127 + 0.27D2 * t162 * t163 * t97 + 0.9
     #D1 * t162 * t128 - t170 * t114 + 0.8D1 * t170 * t110 - 0.9D1 * t38
     # * t64 * t156 * t135 * t127 - 0.19D2 * t57 * t107 * t158 * t97 + t
     #91 * t12 * z + 0.2D1 * t92 * t117 * t4 * t8
      t206 = t20 * x3 * z
      t210 = t38 * z
      t220 = t117 * t19
      t229 = -0.9D1 * t92 * t16 * t163 - 0.36D2 * t13 * t14 * t52 * t96 
     #+ 0.24D2 * t92 * t144 - 0.10D2 * t40 * t120 + 0.11D2 * t34 * t19 *
     # t26 * z + 0.27D2 * t45 * t206 - t48 * t206 + 0.16D2 * t210 * t14 
     #* t52 * t53 * t11 + 0.10D2 * t80 * t1 * t20 * t135 + 0.14D2 * t92 
     #* t220 + 0.16D2 * t210 * t11 * t220 + 0.2D1 * t92 * t117 * t97
      ggbbH21J2 = 0.16D2 / 0.3D1 * wd * (t90 + t146 + t189 + t229) / t11
     # / s / t26

      end function
  
   
 

      doubleprecision function ggbbH21J3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t8 = 0.1D1 - x4
      t11 = s - t2 * t5 * x4 - t2 * t5 * t8
      t12 = s ** 2
      t13 = t12 * s
      t14 = t11 * t13
      t15 = t3 * x1
      t16 = t15 * x3
      t18 = 0.1D1 - x3
      t21 = s - t2 * t16 - t2 * t15 * t18
      t22 = t21 * z
      t24 = t1 * t3
      t25 = x3 * x1
      t26 = t24 * t25
      t29 = t21 ** 2
      t30 = t29 * t11
      t33 = t30 * t12
      t35 = t1 ** 2
      t37 = t14 * t21 * t35
      t38 = t3 ** 2
      t39 = t38 * x1
      t46 = t13 * t1 * t3
      t47 = x1 * t18
      t51 = t12 ** 2
      t53 = t51 * t1 * t3
      t57 = t13 * t29
      t60 = t51 * t21
      t61 = t60 * t1
      t64 = x1 ** 2
      t65 = t38 * t64
      t66 = x3 ** 2
      t70 = t13 * t35
      t72 = t18 ** 2
      t79 = x4 * t4
      t83 = t35 * t38
      t85 = t25 * t79
      t91 = -0.7D1 * t14 * t22 * t26 + t30 * t12 * z - 0.3D1 * t33 - 0.8
     #D1 * t37 * t39 * x3 * t4 * t8 + 0.10D2 * t46 * t47 * t29 - 0.10D2 
     #* t53 * t47 * t21 + t57 * t1 * t16 - t61 * t16 + 0.9D1 * t57 * t35
     # * t65 * t66 + 0.9D1 * t70 * t38 * t64 * t72 * t29 - 0.9D1 * t70 *
     # t39 * t18 * t29 * t79 + 0.9D1 * t57 * t83 * t85 - 0.9D1 * t60 * t
     #83 * t85
      t94 = t18 * t21
      t103 = t14 * t83
      t104 = t21 * t4
      t113 = t47 * t22
      t130 = t51 * z
      t146 = 0.9D1 * t51 * t35 * t39 * t94 * t79 + t37 * t38 * t4 * x4 *
     # x1 * x3 + t103 * t47 * t104 * t8 - 0.8D1 * t103 * t47 * t104 * x4
     # + t14 * t24 * t113 + t33 * t24 * t4 * t8 + 0.9D1 * t33 * t26 - 0.
     #9D1 * t53 * t113 + 0.8D1 * t46 * t47 * t29 * z - 0.9D1 * t61 * t15
     # * x3 * z - 0.8D1 * t130 * t35 * t65 * t66 * t11 + 0.9D1 * t14 * t
     #1 * t15 * t94 - 0.8D1 * t130 * t11 * t24 * t47 + t33 * t24 * t79
      ggbbH21J3 = 0.16D2 / 0.3D1 * wd * (t91 + t146) / t11 / s / t29

      end function
  
 