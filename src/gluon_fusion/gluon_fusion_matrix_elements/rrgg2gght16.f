  
      subroutine rrgg2gght16
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gght16s1e1  
      doubleprecision rrgg2gght16s1e0  
      doubleprecision rrgg2gght16s1em1  
      doubleprecision rrgg2gght16s1em2  
      doubleprecision rrgg2gght16s1em3  
      doubleprecision rrgg2gght16s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght16s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght16s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght16s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght16s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght16s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght16s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght16s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * x1
      t3 = -0.1D1 + x1
      t4 = -0.1D1 + x4
      t7 = kappaf(-t2 * t3 * t4)
      t8 = s * t7
      t10 = sqrt(x4)
      t11 = -0.1D1 + t10
      t13 = t10 + 0.1D1
      t14 = t3 * t11 * t13
      t15 = t8 * t1 * t14
      t16 = t8 * t2
      t17 = t1 * t3
      t18 = t17 * x4
      t19 = t8 * t18
      t20 = t7 ** 2
      t22 = t1 ** 2
      t24 = x1 * t3
      t26 = s * t20 * t22 * t24 * t4
      t27 = t7 * x1
      t28 = t27 * z
      t29 = t7 * z
      t30 = x1 * x4
      t31 = t29 * t30
      t32 = t29 * x4
      t33 = t7 * x4
      t34 = t27 * x4
      t36 = 0.1D1 / (-0.1D1 - t28 + t27 + t31 - t32 + t33 - t34)
      t39 = (-0.1D1 + t28 - t27) ** 2
      t41 = (t31 - t28 - t34 + t27 - t32 + t29 + t33 - t7 - 0.1D1) ** 2
      t45 = 0.1D1 / (-0.2D1 + t7)
      t49 = 0.1D1 / (-0.1D1 + t31 - t34 - t28 + t27) * t20
      t50 = 0.1D1 / x3
      t56 = x2 * 0.3141592653589793D1
      t57 = sin(t56)
      t58 = t57 ** 2
      t59 = x1 ** 2
      t61 = z ** 2
      t62 = 0.1D1 / t61
      t65 = t3 ** 2
      t67 = t20 ** 2
      t72 = log(-0.4D1 * t58 * t59 * t11 * t62 * t65 * x4 * t13 * t67)
      t76 = 0.90D2 - 0.180D3 * lh
      t86 = -0.9D1 / 0.8D1 * wd * t36 * t39 * t41 * t45 * t22 * t49 * t5
     #0 - (-0.90D2 * t22 * wd * t72 + t76 * t22 * wd) * t36 * t39 * t41 
     #* t45 * t49 / 0.80D2
      t87 = FJET(XB1, XB2, s, 0.0D0, t15, t16, -t19, t26, t86)
      t89 = -0.1D1 + x3
      t92 = kappaf(-t2 * t3 * t89)
      t93 = s * t92
      t95 = sqrt(x3)
      t96 = -0.1D1 + t95
      t98 = t95 + 0.1D1
      t99 = x1 * t96 * t98
      t100 = t93 * t1 * t99
      t101 = t93 * t17
      t102 = t2 * x3
      t103 = t93 * t102
      t104 = t92 ** 2
      t108 = s * t104 * t22 * t24 * t89
      t109 = cos(t56)
      t110 = t109 ** 2
      t114 = t22 ** 2
      t115 = 0.1D1 / (-0.2D1 + t92) * t114
      t119 = t92 * x1
      t122 = x3 * x1
      t125 = (-0.1D1 - t92 * x3 * x1 - t119 * z + t92 * z * t122 + t119)
     # ** 2
      t127 = 0.1D1 / t125 * t59
      t128 = t96 * t98
      t129 = Sqrt(-t128)
      t130 = t129 ** 2
      t131 = t104 ** 2
      t132 = t130 * t131
      t133 = 0.1D1 / x4
      t146 = log(-0.4D1 * x3 * t96 * t98 * t62 * t65 * t58 * t59 * t131)
      t156 = -0.18D2 * wd * t110 * t115 * t127 * t132 * t133 - (-0.90D2 
     #* wd * t146 + t76 * wd) * t110 * t115 * t127 * t132 / 0.5D1
      t157 = FJET(XB1, XB2, s, 0.0D0, -t100, -t101, t103, t108, t156)
      t159 = FJET(XB1, XB2, s, t16, -t19, 0.0D0, t15, t26, t86)
      t166 = Sqrt(t128 * t11 * t13)
      t167 = t10 * t166
      t170 = 0.1D1 - x4 - x3 + 0.2D1 * x3 * x4 + 0.2D1 * t109 * t95 * t1
     #67
      t173 = kappaf(t2 * t3 * t170)
      t174 = s * t173
      t175 = t174 * t102
      t176 = t174 * t1
      t177 = t176 * t14
      t178 = t176 * t99
      t179 = t174 * t18
      t180 = t173 ** 2
      t184 = s * t180 * t22 * t24 * t170
      t185 = t173 * x3
      t186 = t185 * x1
      t187 = t173 * x1
      t188 = t187 * z
      t189 = t173 * z
      t190 = t189 * t122
      t191 = t189 * t30
      t194 = t187 * x4
      t196 = 0.1D1 / (-0.1D1 - t186 - t188 + t190 + t187 + t191 - t189 *
     # x4 + t173 * x4 - t194)
      t200 = t189 * t109
      t201 = x1 * t95
      t205 = t173 * t109
      t214 = -t190 - t191 + t194 - 0.2D1 * t185 * t30 + 0.2D1 * t200 * t
     #201 * t167 - 0.2D1 * t205 * x1 * t95 * t10 * t166 + t188 + 0.2D1 *
     # t189 * t122 * x4 + t186 - t187 + 0.1D1
      t215 = 0.1D1 / t214
      t216 = x4 * t180
      t217 = t216 * t109
      t218 = t201 * t166
      t227 = t216 * z
      t229 = t95 * t166
      t230 = t109 * x1 * t229
      t233 = t216 * t61
      t237 = t109 * t59 * t229
      t240 = t10 * x4
      t241 = t180 * t240
      t244 = t240 * t173
      t247 = t10 * t180
      t251 = t173 * t10
      t254 = x3 * z
      t255 = t254 * t59
      t258 = t61 * x3
      t259 = t258 * t59
      t262 = t254 * x1
      t265 = -0.2D1 * t217 * t218 + 0.2D1 * t217 * t59 * t95 * t166 - 0.
     #4D1 * t200 * t218 + 0.4D1 * t227 * t230 - 0.2D1 * t233 * t230 + 0.
     #2D1 * t233 * t237 - t241 * t59 + t241 * x1 - t244 * x1 - t244 * z 
     #+ t247 * t59 - t247 * x1 + t189 * t10 - t251 + t244 - t10 + 0.4D1 
     #* t205 * t218 - 0.4D1 * t241 * t255 + 0.2D1 * t241 * t259 + 0.4D1 
     #* t241 * t262
      t266 = t258 * x1
      t277 = t61 * t59
      t279 = z * t59
      t282 = t61 * x1
      t284 = x1 * z
      t291 = x3 * t59
      t306 = t244 * t284 - 0.2D1 * t241 * t122 + 0.2D1 * t241 * t291 - t
     #247 * t291 - t247 * t282 + t247 * t277 + 0.2D1 * t247 * t284 + 0.3
     #D1 * t251 * t122 - 0.2D1 * t247 * t279 + t247 * t122 - 0.4D1 * t22
     #7 * t237
      t309 = (t265 - 0.2D1 * t241 * t266 + t247 * t266 - t247 * t259 - 0
     #.2D1 * t247 * t262 - 0.3D1 * t251 * t262 + 0.2D1 * t247 * t255 - t
     #241 * t277 + 0.2D1 * t241 * t279 + t241 * t282 - 0.2D1 * t241 * t2
     #84 + t306) ** 2
      t317 = 0.1D1 / (-0.2D1 + t173) * t180 * t22 * t50 * t133
      t319 = 0.9D1 / 0.8D1 * wd * t196 * t215 * t309 * t317
      t320 = FJET(XB1, XB2, s, t175, t177, -t178, -t179, -t184, -t319)
      t323 = t196 * t215 * t309
      t327 = FJET(XB1, XB2, s, t103, -t101, -t100, 0.0D0, t108, t156)
      t329 = FJET(XB1, XB2, s, t177, t175, -t179, -t178, -t184, -t319)
      t334 = FJET(XB1, XB2, s, t15, 0.0D0, -t19, t16, t26, t86)
      t336 = FJET(XB1, XB2, s, -t101, t103, 0.0D0, -t100, t108, t156)
      t338 = FJET(XB1, XB2, s, -t179, -t178, t177, t175, -t184, -t319)
      t343 = FJET(XB1, XB2, s, -t19, t16, t15, 0.0D0, t26, t86)
      t345 = FJET(XB1, XB2, s, -t178, -t179, t175, t177, -t184, -t319)
      t350 = FJET(XB1, XB2, s, -t100, 0.0D0, t103, -t101, t108, t156)
      rrgg2gght16s1e1 = t87 * t86 + t157 * t156 + t159 * t86 - 0.9D1 / 0
     #.8D1 * t320 * wd * t323 * t317 + t327 * t156 - 0.9D1 / 0.8D1 * t32
     #9 * wd * t323 * t317 + t334 * t86 + t336 * t156 - 0.9D1 / 0.8D1 * 
     #t338 * wd * t323 * t317 + t343 * t86 - 0.9D1 / 0.8D1 * t345 * wd *
     # t323 * t317 + t350 * t156

      end function



      doubleprecision function rrgg2gght16s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = t1 * x1
      t3 = -0.1D1 + x1
      t4 = -0.1D1 + x4
      t7 = kappaf(-t2 * t3 * t4)
      t8 = s * t7
      t10 = sqrt(x4)
      t15 = t8 * t1 * t3 * (-0.1D1 + t10) * (t10 + 0.1D1)
      t16 = t8 * t2
      t17 = t1 * t3
      t19 = t8 * t17 * x4
      t20 = t7 ** 2
      t22 = t1 ** 2
      t24 = x1 * t3
      t26 = s * t20 * t22 * t24 * t4
      t28 = t7 * x1
      t29 = t28 * z
      t30 = t7 * z
      t32 = t30 * x1 * x4
      t33 = t30 * x4
      t34 = t7 * x4
      t35 = t28 * x4
      t37 = 0.1D1 / (-0.1D1 - t29 + t28 + t32 - t33 + t34 - t35)
      t39 = (-0.1D1 + t29 - t28) ** 2
      t43 = (t32 - t29 - t35 + t28 - t33 + t30 + t34 - t7 - 0.1D1) ** 2
      t45 = 0.1D1 / (-0.2D1 + t7)
      t48 = 0.1D1 / (-0.1D1 + t32 - t35 - t29 + t28)
      t52 = 0.9D1 / 0.8D1 * t22 * wd * t37 * t39 * t43 * t45 * t48 * t20
      t53 = FJET(XB1, XB2, s, 0.0D0, t15, t16, -t19, t26, -t52)
      t55 = wd * t37
      t60 = t39 * t43 * t45 * t48 * t20
      t63 = -0.1D1 + x3
      t66 = kappaf(-t2 * t3 * t63)
      t67 = s * t66
      t69 = sqrt(x3)
      t70 = -0.1D1 + t69
      t72 = t69 + 0.1D1
      t74 = t67 * t1 * x1 * t70 * t72
      t75 = t67 * t17
      t77 = t67 * t2 * x3
      t78 = t66 ** 2
      t82 = s * t78 * t22 * t24 * t63
      t84 = cos(x2 * 0.3141592653589793D1)
      t85 = t84 ** 2
      t88 = 0.1D1 / (-0.2D1 + t66)
      t89 = t22 ** 2
      t94 = t66 * x1
      t100 = (-0.1D1 - t66 * x3 * x1 - t94 * z + t66 * z * x3 * x1 + t94
     #) ** 2
      t101 = 0.1D1 / t100
      t102 = x1 ** 2
      t105 = Sqrt(-t70 * t72)
      t106 = t105 ** 2
      t107 = t78 ** 2
      t111 = 0.18D2 * wd * t85 * t88 * t89 * t101 * t102 * t106 * t107
      t112 = FJET(XB1, XB2, s, 0.0D0, -t74, -t75, t77, t82, -t111)
      t114 = t85 * t88
      t119 = t89 * t101 * t102 * t106 * t107
      t122 = FJET(XB1, XB2, s, t16, -t19, 0.0D0, t15, t26, -t52)
      t127 = FJET(XB1, XB2, s, t77, -t75, -t74, 0.0D0, t82, -t111)
      t132 = FJET(XB1, XB2, s, t15, 0.0D0, -t19, t16, t26, -t52)
      t137 = FJET(XB1, XB2, s, -t75, t77, 0.0D0, -t74, t82, -t111)
      t142 = FJET(XB1, XB2, s, -t74, 0.0D0, t77, -t75, t82, -t111)
      t147 = FJET(XB1, XB2, s, -t19, t16, t15, 0.0D0, t26, -t52)
      rrgg2gght16s1e0 = -0.9D1 / 0.8D1 * t53 * t22 * t55 * t60 - 0.18D2 
     #* t112 * wd * t114 * t119 - 0.9D1 / 0.8D1 * t122 * t22 * t55 * t60
     # - 0.18D2 * t127 * wd * t114 * t119 - 0.9D1 / 0.8D1 * t132 * t22 *
     # t55 * t60 - 0.18D2 * t137 * wd * t114 * t119 - 0.18D2 * t142 * wd
     # * t114 * t119 - 0.9D1 / 0.8D1 * t147 * t22 * t55 * t60

      end function



      doubleprecision function rrgg2gght16s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght16s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2gght16s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght16s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gght16s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght16s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght16s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2gght16s1em4 = 0.0D0

      end function
