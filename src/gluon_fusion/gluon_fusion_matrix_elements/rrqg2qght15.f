  
      subroutine rrqg2qght15
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqg2qght15s1e1  
      doubleprecision rrqg2qght15s1e0  
      doubleprecision rrqg2qght15s1em1  
      doubleprecision rrqg2qght15s1em2  
      doubleprecision rrqg2qght15s1em3  
      doubleprecision rrqg2qght15s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght15s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqg2qght15s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqg2qght15s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqg2qght15s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqg2qght15s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqg2qght15s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqg2qght15s1e1
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
      t4 = -0.1D1 + x3
      t7 = kappaf(-t2 * t3 * t4)
      t8 = s * t7
      t10 = sqrt(x3)
      t11 = -0.1D1 + t10
      t13 = t10 + 0.1D1
      t14 = x1 * t11 * t13
      t16 = t1 * t3
      t18 = t2 * x3
      t20 = t7 ** 2
      t22 = t1 ** 2
      t24 = x1 * t3
      t27 = t7 * x1
      t32 = x3 * x1
      t35 = (-0.1D1 + t27 - t27 * z - t7 * x3 * x1 + t7 * z * t32) ** 2
      t36 = 0.1D1 / t35
      t38 = x2 * 0.3141592653589793D1
      t39 = cos(t38)
      t40 = t39 ** 2
      t41 = t11 * t13
      t42 = Sqrt(-t41)
      t43 = t42 ** 2
      t44 = t40 * t43
      t46 = t20 ** 2
      t48 = t46 * t22 * t1
      t49 = x1 ** 2
      t52 = t49 / (-0.2D1 + t7)
      t53 = 0.1D1 / x4
      t59 = z ** 2
      t60 = 0.1D1 / t59
      t63 = sin(t38)
      t64 = t63 ** 2
      t65 = t3 ** 2
      t66 = t64 * t65
      t67 = t22 ** 2
      t73 = log(-0.4D1 * x3 * t11 * t13 * t60 * t66 * t46 * t67 * t49)
      t76 = 0.90D2 * wd
      t78 = 0.180D3 * wd * lh
      t85 = 0.16D2 * wd * t36 * t44 * t48 * t52 * t53 + 0.8D1 / 0.45D2 *
     # (-0.90D2 * wd * t73 + t76 - t78) * t36 * t44 * t48 * t52
      t86 = FJET(XB1, XB2, s, 0.0D0, -t8 * t1 * t14, -t8 * t16, t8 * t18
     #, s * t20 * t22 * t24 * t4, t85)
      t91 = sqrt(x4)
      t92 = -0.1D1 + t91
      t93 = t91 + 0.1D1
      t96 = Sqrt(t41 * t92 * t93)
      t97 = t91 * t96
      t100 = 0.1D1 - x4 - x3 + 0.2D1 * x3 * x4 + 0.2D1 * t39 * t10 * t97
      t103 = kappaf(t2 * t3 * t100)
      t104 = s * t103
      t105 = t16 * x4
      t107 = t104 * t1
      t110 = t3 * t92 * t93
      t113 = t103 ** 2
      t119 = t91 * x4
      t120 = t113 * t119
      t121 = t59 * x1
      t123 = x1 * z
      t126 = t59 * t49
      t128 = z * t49
      t131 = t91 * t113
      t134 = x3 * t49
      t138 = t103 * t91
      t143 = t119 * t103
      t150 = x4 * t113
      t151 = t150 * t39
      t156 = x1 * t10
      t157 = t156 * t96
      t160 = t103 * z
      t161 = t160 * t39
      t166 = t120 * t121 - 0.2D1 * t120 * t123 - t120 * t126 + 0.2D1 * t
     #120 * t128 - t131 * t121 + t131 * t32 - t131 * t134 + 0.2D1 * t131
     # * t123 + 0.3D1 * t138 * t32 + 0.2D1 * t120 * t134 + t143 * t123 -
     # 0.2D1 * t120 * t32 + t131 * t126 - 0.2D1 * t131 * t128 - t138 + 0
     #.2D1 * t151 * t49 * t10 * t96 - 0.2D1 * t151 * t157 - 0.4D1 * t161
     # * t157 - t143 * x1 - t143 * z
      t172 = x3 * z
      t173 = t172 * t49
      t176 = t172 * x1
      t181 = t59 * x3
      t182 = t181 * t49
      t185 = t103 * t39
      t190 = t181 * x1
      t198 = t150 * t59
      t200 = t10 * t96
      t201 = t39 * x1 * t200
      t204 = t150 * z
      t208 = t39 * t49 * t200
      t213 = 0.4D1 * t185 * t157 + 0.4D1 * t120 * t176 - 0.2D1 * t120 * 
     #t190 + 0.2D1 * t120 * t182 - 0.4D1 * t120 * t173 + t131 * t190 + t
     #143 - 0.2D1 * t198 * t201 + 0.4D1 * t204 * t201 + 0.2D1 * t198 * t
     #208 - 0.4D1 * t204 * t208
      t216 = (t166 + t131 * t49 - t131 * x1 - t120 * t49 + t120 * x1 + t
     #160 * t91 - t91 + 0.2D1 * t131 * t173 - 0.3D1 * t138 * t176 - 0.2D
     #1 * t131 * t176 - t131 * t182 + t213) ** 2
      t219 = t103 * x1
      t220 = t219 * z
      t232 = t103 * x3
      t233 = x1 * x4
      t236 = t219 * x4
      t237 = t160 * t233
      t238 = t232 * x1
      t239 = t160 * t32
      t240 = t220 - 0.2D1 * t185 * x1 * t10 * t91 * t96 + 0.2D1 * t160 *
     # t32 * x4 - t219 + 0.2D1 * t161 * t156 * t97 - 0.2D1 * t232 * t233
     # + t236 - t237 + t238 - t239 + 0.1D1
      t249 = 0.1D1 / x3
      t252 = 0.1D1 / t240 / (-0.1D1 + t219 - t220 - t238 + t239 + t103 *
     # x4 - t236 - t160 * x4 + t237) / (-0.2D1 + t103) * t249 * t53
      t254 = FJET(XB1, XB2, s, -t104 * t105, -t107 * t14, t107 * t110, t
     #104 * t18, -s * t113 * t22 * t24 * t100, wd * t113 * t1 * t216 * t
     #252)
      t260 = -0.1D1 + x4
      t263 = kappaf(-t2 * t3 * t260)
      t264 = s * t263
      t269 = t263 ** 2
      t275 = t263 * x1
      t276 = t275 * z
      t278 = (-0.1D1 + t276 - t275) ** 2
      t279 = t263 * z
      t280 = t279 * t233
      t281 = t275 * x4
      t282 = t279 * x4
      t283 = t263 * x4
      t285 = (-t276 + t280 + t275 - t281 + t279 - t282 - t263 + t283 - 0
     #.1D1) ** 2
      t289 = 0.1D1 / (-t276 - 0.1D1 + t275 - t281 + t280)
      t295 = 0.1D1 / (-0.1D1 + t275 - t276 + t283 - t281 - t282 + t280) 
     #/ (-0.2D1 + t263)
      t300 = t269 ** 2
      t309 = log(-0.4D1 * t300 * t49 * t93 * t60 * t92 * t67 * t66 * x4)
      t321 = wd * t269 * t278 * t285 * t289 * t1 * t295 * t249 + (-0.90D
     #2 * wd * t1 * t309 + (t76 - t78) * t1) * t269 * t278 * t285 * t289
     # * t295 / 0.90D2
      t322 = FJET(XB1, XB2, s, -t264 * t105, t264 * t2, t264 * t1 * t110
     #, 0.0D0, s * t269 * t22 * t24 * t260, t321)
      rrqg2qght15s1e1 = t86 * t85 + t254 * wd * t113 * t1 * t216 * t252 
     #+ t322 * t321

      end function



      doubleprecision function rrqg2qght15s1e0
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
      t4 = -0.1D1 + x3
      t7 = kappaf(-t2 * t3 * t4)
      t8 = s * t7
      t10 = sqrt(x3)
      t11 = -0.1D1 + t10
      t13 = t10 + 0.1D1
      t16 = t1 * t3
      t20 = t7 ** 2
      t22 = t1 ** 2
      t24 = x1 * t3
      t27 = t7 * x1
      t35 = (-0.1D1 + t27 - t27 * z - t7 * x3 * x1 + t7 * z * x3 * x1) *
     #* 2
      t36 = 0.1D1 / t35
      t39 = cos(x2 * 0.3141592653589793D1)
      t40 = t39 ** 2
      t42 = Sqrt(-t11 * t13)
      t43 = t42 ** 2
      t46 = t20 ** 2
      t47 = t22 * t1
      t49 = x1 ** 2
      t51 = 0.1D1 / (-0.2D1 + t7)
      t56 = FJET(XB1, XB2, s, 0.0D0, -t8 * t1 * x1 * t11 * t13, -t8 * t1
     #6, t8 * t2 * x3, s * t20 * t22 * t24 * t4, 0.16D2 * wd * t36 * t40
     # * t43 * t46 * t47 * t49 * t51)
      t66 = -0.1D1 + x4
      t69 = kappaf(-t2 * t3 * t66)
      t70 = s * t69
      t75 = sqrt(x4)
      t81 = t69 ** 2
      t87 = t69 * x1
      t88 = t87 * z
      t90 = (-0.1D1 + t88 - t87) ** 2
      t93 = t69 * z
      t95 = t93 * x1 * x4
      t96 = t87 * x4
      t97 = t93 * x4
      t98 = t69 * x4
      t100 = (-t88 + t95 + t87 - t96 + t93 - t97 - t69 + t98 - 0.1D1) **
     # 2
      t102 = 0.1D1 / (-t88 - 0.1D1 + t87 - t96 + t95)
      t105 = 0.1D1 / (-0.1D1 + t87 - t88 + t98 - t96 - t97 + t95)
      t107 = 0.1D1 / (-0.2D1 + t69)
      t111 = FJET(XB1, XB2, s, -t70 * t16 * x4, t70 * t2, t70 * t1 * t3 
     #* (-0.1D1 + t75) * (t75 + 0.1D1), 0.0D0, s * t81 * t22 * t24 * t66
     #, wd * t1 * t81 * t90 * t100 * t102 * t105 * t107)
      rrqg2qght15s1e0 = 0.16D2 * t56 * wd * t36 * t40 * t43 * t46 * t47 
     #* t49 * t51 + t111 * wd * t1 * t81 * t90 * t100 * t102 * t105 * t1
     #07

      end function



      doubleprecision function rrqg2qght15s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqg2qght15s1em1 = 0.0D0

      end function



      doubleprecision function rrqg2qght15s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqg2qght15s1em2 = 0.0D0

      end function



      doubleprecision function rrqg2qght15s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqg2qght15s1em3 = 0.0D0

      end function



      doubleprecision function rrqg2qght15s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrqg2qght15s1em4 = 0.0D0

      end function
