  
      subroutine gbgbH7n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision gbgbH7n1e1  
      doubleprecision gbgbH7n1e0  
      doubleprecision gbgbH7n1em1  
      doubleprecision gbgbH7n1em2  
      doubleprecision gbgbH7n1em3  
      doubleprecision gbgbH7n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=gbgbH7n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=gbgbH7n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=gbgbH7n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=gbgbH7n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=gbgbH7n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=gbgbH7n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function gbgbH7n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = z * lh
      t8 = x4 * 0.3141592653589793D1
      t9 = Sin(t8)
      t10 = t9 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t17 = log(-0.4D1 * x3 * t10 * t13 * t4)
      t18 = t17 * z
      t25 = lh ** 2
      t27 = 0.3141592653589793D1 ** 2
      t29 = 0.180D3 * t25 - 0.30D2 * t27
      t30 = z * t29
      t31 = t17 ** 2
      t32 = t31 * z
      t56 = t1 * wd
      t57 = 0.8D1 * x3
      t58 = x2 * x3
      t59 = t10 * t13
      t60 = t59 * t4
      t63 = log(-0.4D1 * t58 * t60)
      t64 = t63 * t4
      t70 = z * t1
      t71 = 0.4D1 * x3
      t73 = t63 ** 2
      t82 = -0.4D1 * t56 * t4
      t85 = 0.1D1 / x2
      t88 = x1 ** 2
      t89 = t58 * t88
      t92 = log(-0.4D1 * t89 * t60)
      t103 = 0.1D1 / x1
      t106 = t70 * t4
      t107 = t1 * t4
      t108 = t6 * t107
      t111 = (-0.180D3 * t106 - 0.180D3 * t108) * wd
      t112 = x3 * t88
      t115 = log(-0.4D1 * t112 * t60)
      t117 = t4 * wd
      t118 = t115 ** 2
      t126 = (0.90D2 * t106 + 0.360D3 * t108 + t30 * t107) * wd
      t130 = ((-0.180D3 * t6 - 0.90D2 * t18) * t1 * t4 - 0.2D1 * (0.180D
     #3 * t18 * lh + t30 + 0.45D2 * t32) * t1 * t4 + (-t18 * t29 - 0.90D
     #2 * t32 * lh + z * (-0.2884936567583026D3 - 0.120D3 * t25 * lh + 0
     #.60D2 * lh * t27) - 0.15D2 * t31 * t17 * z) * t1 * t4) * wd / 0.13
     #5D3 - (-0.180D3 * t6 * t56 * (-0.8D1 + t57 + 0.4D1 * t64) + 0.90D2
     # * t70 * wd * (0.4D1 - t71 - 0.8D1 * t64 - 0.2D1 * t73 * t4) + t30
     # * t82) * t85 / 0.540D3 - (0.90D2 * t70 * wd * (-0.8D1 + t57 + 0.4
     #D1 * t92 * t4) - 0.180D3 * t6 * t82) * t85 * t103 / 0.270D3 + 0.2D
     #1 / 0.135D3 * (-t111 * t115 + 0.45D2 * t70 * t117 * t118 + t126) *
     # t103
      t131 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t
     #130)
      t133 = 0.2D1 * t58
      t134 = cos(t8)
      t135 = -0.1D1 + x2
      t139 = Sqrt(x2 * t135 * x3 * t4)
      t141 = 0.2D1 * t134 * t139
      t146 = x2 * z
      t148 = (0.1D1 + t146 - x2) ** 2
      t149 = 0.1D1 / t148
      t152 = 0.5D1 * x2 - 0.5D1 * t146 + 0.4D1 - t71
      t153 = t149 * t152
      t159 = log(0.4D1 * t58 * t10 * t13 * t135 * t4)
      t161 = (-0.1D1 - t159) * t149
      t162 = 0.13D2 * x2
      t163 = 0.13D2 * t146
      t164 = -0.4D1 + t71 - t162 + t163
      t171 = t159 ** 2
      t182 = wd * t149 * t164
      t191 = log(0.4D1 * t89 * t59 * t135 * t4)
      t199 = t6 * t1
      t206 = -(-0.180D3 * t6 * t56 * (t153 + t161 * t164) + 0.90D2 * t70
     # * wd * (t161 * t152 + (t159 + t171 / 0.2D1) * t149 * t164) + t30 
     #* t1 * t182) * t85 / 0.540D3 - (0.90D2 * t70 * wd * (t153 + (-0.1D
     #1 - t191) * t149 * t164) - 0.180D3 * t199 * t182) * t85 * t103 / 0
     #.270D3
      t207 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t133 - x2 + t141), 0.
     #0D0, t2 * (0.1D1 - x2 - x3 + t133 + t141), 0.0D0, t206)
      t209 = x1 * x3
      t210 = t2 * t209
      t211 = -0.1D1 + x1
      t212 = t209 * z
      t213 = t58 * x1
      t214 = x1 * z
      t215 = t58 * t214
      t217 = 0.1D1 - x1 + t214
      t221 = Sqrt(x3 * t135 * t217 * x2 * t4)
      t223 = 0.2D1 * t134 * t221
      t226 = 0.1D1 / t217
      t229 = t4 * s
      t231 = t229 * t1 * x1
      t232 = x2 * x1
      t233 = t232 * z
      t234 = 0.1D1 - x1 + t214 - x2 + t232 - t233 - x3 + t209 - t212 + t
     #133 - t213 + t215 + t223
      t238 = t1 ** 2
      t254 = t58 * t88 * t10
      t255 = t13 * t226
      t256 = t211 ** 2
      t262 = log(0.4D1 * t254 * t255 * t256 * t135 * t4)
      t269 = 0.4D1 - 0.13D2 * t232 + 0.4D1 * t209 - t163 - 0.4D1 * x1 + 
     #t162 - t71 + 0.4D1 * t214 + 0.13D2 * t233 - 0.4D1 * t212
      t271 = -0.8D1 + 0.18D2 * t232 - 0.8D1 * t209 + 0.18D2 * t146 + 0.8
     #D1 * x1 - 0.18D2 * x2 + t57 - 0.8D1 * t214 - 0.18D2 * t233 + 0.8D1
     # * t212 - t262 * t269
      t274 = (-0.1D1 + x2 - t232 - t146 + t233 - t214 + x1) ** 2
      t275 = 0.1D1 / t274
      t284 = 0.90D2 * t70 * wd * t271 * t217 * t275 - 0.180D3 * t199 * w
     #d * t269 * t217 * t275
      t288 = FJET(XB1, XB2, s, t210, t2 * t211 * (-x3 + t209 - t212 + t1
     #33 - t213 + t215 - x2 + t223) * t226, -t231, -t2 * t211 * t234 * t
     #226, -s * t238 * x2 * t211 * x1 * t226, -t284 * t85 * t103 / 0.270
     #D3)
      t298 = t255 * t256 * t4
      t301 = log(-0.4D1 * t254 * t298)
      t317 = log(-0.4D1 * t112 * t10 * t298)
      t319 = t317 ** 2
      t326 = -(0.360D3 * t70 * wd * (-0.2D1 - t301) * t4 - 0.720D3 * t6 
     #* t56 * t4) * t85 * t103 / 0.270D3 + 0.2D1 / 0.135D3 * (t111 * t31
     #7 - 0.45D2 * t70 * t117 * t319 - t126) * t103
      t327 = FJET(XB1, XB2, s, t210, -t2 * t211 * x3, -t231, t229 * t1 *
     # t211, 0.0D0, t326)
      gbgbH7n1e1 = t131 * t130 + t207 * t206 - t288 * t284 * t85 * t103 
     #/ 0.270D3 + t327 * t326

      end function



      doubleprecision function gbgbH7n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = z * t1
      t8 = x2 * x3
      t9 = x4 * 0.3141592653589793D1
      t10 = Sin(t9)
      t11 = t10 ** 2
      t12 = z ** 2
      t13 = 0.1D1 / t12
      t15 = t11 * t13 * t4
      t18 = log(-0.4D1 * t8 * t15)
      t25 = z * lh
      t26 = t1 * wd
      t28 = -0.4D1 * t4
      t33 = 0.1D1 / x2
      t36 = t6 * wd
      t38 = 0.1D1 / x1
      t42 = t4 * wd
      t43 = x1 ** 2
      t44 = x3 * t43
      t47 = log(-0.4D1 * t44 * t15)
      t51 = t6 * t4
      t56 = (-0.180D3 * t51 - 0.180D3 * t25 * t1 * t4) * wd
      t66 = log(-0.4D1 * x3 * t11 * t13 * t4)
      t67 = t66 * z
      t75 = lh ** 2
      t77 = 0.3141592653589793D1 ** 2
      t81 = t66 ** 2
      t90 = -(0.90D2 * t6 * wd * (-0.8D1 + 0.8D1 * x3 + 0.4D1 * t18 * t4
     #) - 0.180D3 * t25 * t26 * t28) * t33 / 0.540D3 - t36 * t28 * t33 *
     # t38 / 0.3D1 + 0.2D1 / 0.135D3 * (-0.90D2 * t6 * t42 * t47 + t56) 
     #* t38 + (0.90D2 * t51 - 0.2D1 * (-0.180D3 * t25 - 0.90D2 * t67) * 
     #t1 * t4 + (0.180D3 * t67 * lh + z * (0.180D3 * t75 - 0.30D2 * t77)
     # + 0.45D2 * t81 * z) * t1 * t4) * wd / 0.135D3
      t91 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t9
     #0)
      t93 = 0.2D1 * t8
      t94 = cos(t9)
      t95 = -0.1D1 + x2
      t99 = Sqrt(x2 * t95 * x3 * t4)
      t101 = 0.2D1 * t94 * t99
      t106 = x2 * z
      t108 = (0.1D1 + t106 - x2) ** 2
      t109 = 0.1D1 / t108
      t112 = 0.4D1 * x3
      t120 = log(0.4D1 * t8 * t11 * t13 * t95 * t4)
      t123 = 0.13D2 * x2
      t124 = 0.13D2 * t106
      t125 = -0.4D1 + t112 - t123 + t124
      t140 = t33 * t38
      t144 = -(0.90D2 * t6 * wd * (t109 * (0.5D1 * x2 - 0.5D1 * t106 + 0
     #.4D1 - t112) + (-0.1D1 - t120) * t109 * t125) - 0.180D3 * t25 * t1
     # * wd * t109 * t125) * t33 / 0.540D3 - t36 * t109 * t125 * t140 / 
     #0.3D1
      t145 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t93 - x2 + t101), 0.0
     #D0, t2 * (0.1D1 - x2 - x3 + t93 + t101), 0.0D0, t144)
      t147 = x1 * x3
      t148 = t2 * t147
      t149 = -0.1D1 + x1
      t150 = t147 * z
      t151 = t8 * x1
      t152 = x1 * z
      t153 = t8 * t152
      t155 = 0.1D1 - x1 + t152
      t159 = Sqrt(x3 * t95 * t155 * x2 * t4)
      t161 = 0.2D1 * t94 * t159
      t164 = 0.1D1 / t155
      t167 = t4 * s
      t169 = t167 * t1 * x1
      t170 = x2 * x1
      t171 = t170 * z
      t172 = 0.1D1 - x1 + t152 - x2 + t170 - t171 - x3 + t147 - t150 + t
     #93 - t151 + t153 + t161
      t176 = t1 ** 2
      t188 = 0.4D1 - 0.13D2 * t170 + 0.4D1 * t147 - t124 - 0.4D1 * x1 + 
     #t123 - t112 + 0.4D1 * t152 + 0.13D2 * t171 - 0.4D1 * t150
      t192 = (-0.1D1 + x2 - t170 - t106 + t171 - t152 + x1) ** 2
      t193 = 0.1D1 / t192
      t198 = FJET(XB1, XB2, s, t148, t2 * t149 * (-x3 + t147 - t150 + t9
     #3 - t151 + t153 - x2 + t161) * t164, -t169, -t2 * t149 * t172 * t1
     #64, -s * t176 * x2 * t149 * x1 * t164, -t6 * wd * t188 * t155 * t1
     #93 * t140 / 0.3D1)
      t217 = t149 ** 2
      t222 = log(-0.4D1 * t44 * t11 * t13 * t164 * t217 * t4)
      t229 = -0.4D1 / 0.3D1 * t36 * t4 * t33 * t38 + 0.2D1 / 0.135D3 * (
     #0.90D2 * t6 * t42 * t222 - t56) * t38
      t230 = FJET(XB1, XB2, s, t148, -t2 * t149 * x3, -t169, t167 * t1 *
     # t149, 0.0D0, t229)
      gbgbH7n1e0 = t91 * t90 + t145 * t144 - t198 * z * t26 * t188 * t15
     #5 * t193 * t33 * t38 / 0.3D1 + t230 * t229

      end function



      doubleprecision function gbgbH7n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t6 = z * t1
      t11 = x4 * 0.3141592653589793D1
      t12 = Sin(t11)
      t13 = t12 ** 2
      t15 = z ** 2
      t20 = log(-0.4D1 * x3 * t13 / t15 * t4)
      t32 = 0.1D1 / x2
      t38 = t4 * wd / x1
      t40 = 0.4D1 / 0.3D1 * t6 * t38
      t41 = (-0.180D3 * t6 * t4 + (-0.180D3 * z * lh - 0.90D2 * t20 * z)
     # * t1 * t4) * wd / 0.135D3 + 0.2D1 / 0.3D1 * t6 * t4 * wd * t32 + 
     #t40
      t42 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, t4
     #1)
      t45 = 0.2D1 * x2 * x3
      t46 = cos(t11)
      t51 = Sqrt(x2 * (-0.1D1 + x2) * x3 * t4)
      t53 = 0.2D1 * t46 * t51
      t59 = x2 * z
      t61 = (0.1D1 + t59 - x2) ** 2
      t62 = 0.1D1 / t61
      t66 = -0.4D1 + 0.4D1 * x3 - 0.13D2 * x2 + 0.13D2 * t59
      t71 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t45 - x2 + t53), 0.0D0
     #, t2 * (0.1D1 - x2 - x3 + t45 + t53), 0.0D0, -t6 * wd * t62 * t66 
     #* t32 / 0.6D1)
      t81 = -0.1D1 + x1
      t84 = t4 * s
      t89 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t81 * x3, -t84 * t1 * 
     #x1, t84 * t1 * t81, 0.0D0, -t40)
      gbgbH7n1em1 = t42 * t41 - t71 * z * t1 * wd * t62 * t66 * t32 / 0.
     #6D1 - 0.4D1 / 0.3D1 * t89 * z * t1 * t38

      end function



      doubleprecision function gbgbH7n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = -0.1D1 + x3
      t10 = FJET(XB1, XB2, s, 0.0D0, t2 * x3, 0.0D0, -t2 * t4, 0.0D0, 0.
     #2D1 / 0.3D1 * z * t1 * t4 * wd)
      gbgbH7n1em2 = 0.2D1 / 0.3D1 * t10 * z * t1 * t4 * wd

      end function



      doubleprecision function gbgbH7n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      gbgbH7n1em3 = 0.0D0

      end function



      doubleprecision function gbgbH7n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      gbgbH7n1em4 = 0.0D0

      end function
