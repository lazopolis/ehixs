  
      subroutine rrqqbar2gght1
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrqqbar2ggh11J1  
      doubleprecision rrqqbar2ggh11J2  
      doubleprecision rrqqbar2ggh11J3  
      doubleprecision rrqqbar2gght1s1e1  
      doubleprecision rrqqbar2gght1s1e0  
      doubleprecision rrqqbar2gght1s1em1  
      doubleprecision rrqqbar2gght1s1em2  
      doubleprecision rrqqbar2gght1s1em3  
      doubleprecision rrqqbar2gght1s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght1s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght1s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght1s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght1s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght1s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrqqbar2gght1s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrqqbar2gght1s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh11J1
      doubleprecision rrqqbar2ggh11J2
      doubleprecision rrqqbar2ggh11J3
      t1 = x3 * x1
      t2 = -0.1D1 + z
      t3 = t2 * s
      t5 = -0.1D1 + x1
      t7 = t3 * t5 * x3
      t8 = -0.1D1 + x3
      t12 = t3 * t5 * t8
      t13 = 0.3141592653589793D1 * t2
      t14 = rrqqbar2ggh11J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x
     #4)
      t15 = x1 * z
      t16 = -z - x1 + t15
      t17 = 0.1D1 / t16
      t18 = x2 * t17
      t19 = x4 * 0.3141592653589793D1
      t20 = Sin(t19)
      t21 = t20 ** 2
      t22 = z ** 2
      t23 = 0.1D1 / t22
      t24 = t21 * t23
      t26 = t2 ** 2
      t27 = t26 ** 2
      t28 = x1 ** 2
      t30 = t5 ** 2
      t36 = log(0.4D1 * t18 * t24 * t27 * t28 * t30 * x3 * t8)
      t37 = rrqqbar2ggh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x
     #4)
      t42 = 0.3141592653589793D1 * lh
      t47 = 0.1D1 / x2
      t50 = rrqqbar2ggh11J3(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x
     #4)
      t57 = t28 * t30
      t58 = x3 * t8
      t62 = log(0.4D1 * t17 * t21 * t23 * t27 * t57 * t58)
      t63 = t62 * 0.3141592653589793D1
      t71 = t62 ** 2
      t74 = lh ** 2
      t76 = 0.3141592653589793D1 ** 2
      t84 = -(0.90D2 * t13 * (-t14 + t36 * t37) + 0.180D3 * t42 * t2 * t
     #37) * t47 / 0.720D3 + t13 * t50 / 0.8D1 + (-0.180D3 * t42 - 0.90D2
     # * t63) * t2 * t14 / 0.720D3 + (0.180D3 * t63 * lh + 0.45D2 * t71 
     #* 0.3141592653589793D1 + 0.3141592653589793D1 * (0.180D3 * t74 - 0
     #.30D2 * t76)) * t2 * t37 / 0.720D3
      t85 = FJET(XB1, XB2, s, t1 * t3, -t7, -t8 * x1 * t3, t12, 0.0D0, t
     #84)
      t87 = x3 * z
      t88 = t1 * z
      t89 = x2 * x3
      t90 = t89 * z
      t91 = t89 * x1
      t92 = t89 * t15
      t93 = cos(t19)
      t94 = -0.1D1 + x2
      t99 = Sqrt(-x3 * t94 * t16 * x2 * t8)
      t101 = 0.2D1 * t93 * t99
      t107 = x2 * x1
      t109 = z + x1 - t15 - x2 * z - t107 + t107 * z - t87 - t1 + t88 + 
     #t90 + t91 - t92 + t89 + t101
      t118 = rrqqbar2ggh11J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t125 = log(-0.4D1 * t18 * t24 * t27 * t57 * t58 * t94)
      t126 = rrqqbar2ggh11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t134 = 0.90D2 * t13 * (t118 - t125 * t126) - 0.180D3 * t42 * t2 * 
     #t126
      t137 = FJET(XB1, XB2, s, t3 * x1 * (-t87 - t1 + t88 + t90 + t91 - 
     #t92 - x2 + t89 + t101) * t17, -t7, -t3 * x1 * t109 * t17, t12, s *
     # t26 * x2 * x1 * t5 * t17, -t134 * t47 / 0.720D3)
      rrqqbar2gght1s1e1 = t85 * t84 - t137 * t134 * t47 / 0.720D3

      end function



      doubleprecision function rrqqbar2gght1s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh11J1
      doubleprecision rrqqbar2ggh11J2
      doubleprecision rrqqbar2ggh11J3
      t1 = x3 * x1
      t2 = -0.1D1 + z
      t3 = t2 * s
      t5 = -0.1D1 + x1
      t7 = t3 * t5 * x3
      t8 = -0.1D1 + x3
      t12 = t3 * t5 * t8
      t13 = 0.3141592653589793D1 * t2
      t14 = rrqqbar2ggh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x
     #4)
      t15 = 0.1D1 / x2
      t19 = rrqqbar2ggh11J2(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x
     #4)
      t24 = x1 * z
      t25 = -z - x1 + t24
      t26 = 0.1D1 / t25
      t27 = x4 * 0.3141592653589793D1
      t28 = Sin(t27)
      t29 = t28 ** 2
      t31 = z ** 2
      t33 = t2 ** 2
      t34 = t33 ** 2
      t37 = x1 ** 2
      t38 = t5 ** 2
      t44 = log(0.4D1 * t26 * t29 / t31 * t34 * t37 * t38 * x3 * t8)
      t51 = t13 * t14 * t15 / 0.8D1 + t13 * t19 / 0.8D1 + (-0.180D3 * 0.
     #3141592653589793D1 * lh - 0.90D2 * t44 * 0.3141592653589793D1) * t
     #2 * t14 / 0.720D3
      t52 = FJET(XB1, XB2, s, t1 * t3, -t7, -t8 * x1 * t3, t12, 0.0D0, t
     #51)
      t54 = x3 * z
      t55 = t1 * z
      t56 = x2 * x3
      t57 = t56 * z
      t58 = t56 * x1
      t59 = t56 * t24
      t60 = cos(t27)
      t66 = Sqrt(-x3 * (-0.1D1 + x2) * t25 * x2 * t8)
      t68 = 0.2D1 * t60 * t66
      t74 = x2 * x1
      t76 = z + x1 - t24 - x2 * z - t74 + t74 * z - t54 - t1 + t55 + t57
     # + t58 - t59 + t56 + t68
      t85 = rrqqbar2ggh11J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t89 = FJET(XB1, XB2, s, t3 * x1 * (-t54 - t1 + t55 + t57 + t58 - t
     #59 - x2 + t56 + t68) * t26, -t7, -t3 * x1 * t76 * t26, t12, s * t3
     #3 * x2 * x1 * t5 * t26, -t13 * t85 * t15 / 0.8D1)
      rrqqbar2gght1s1e0 = t52 * t51 - t89 * 0.3141592653589793D1 * t2 * 
     #t85 * t15 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght1s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh11J1
      doubleprecision rrqqbar2ggh11J2
      doubleprecision rrqqbar2ggh11J3
      t2 = -0.1D1 + z
      t3 = t2 * s
      t5 = -0.1D1 + x1
      t8 = -0.1D1 + x3
      t14 = rrqqbar2ggh11J1(s, XB1, XB2, z, lh, wd, nf, x1, 0.0D0, x3, x
     #4)
      t17 = FJET(XB1, XB2, s, x3 * x1 * t3, -t3 * t5 * x3, -t8 * x1 * t3
     #, t3 * t5 * t8, 0.0D0, 0.3141592653589793D1 * t2 * t14 / 0.8D1)
      rrqqbar2gght1s1em1 = t17 * 0.3141592653589793D1 * t2 * t14 / 0.8D1

      end function



      doubleprecision function rrqqbar2gght1s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh11J1
      doubleprecision rrqqbar2ggh11J2
      doubleprecision rrqqbar2ggh11J3
      rrqqbar2gght1s1em2 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght1s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh11J1
      doubleprecision rrqqbar2ggh11J2
      doubleprecision rrqqbar2ggh11J3
      rrqqbar2gght1s1em3 = 0.0D0

      end function



      doubleprecision function rrqqbar2gght1s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrqqbar2ggh11J1
      doubleprecision rrqqbar2ggh11J2
      doubleprecision rrqqbar2ggh11J3
      rrqqbar2gght1s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrqqbar2ggh11J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = s * t3
      t5 = x1 * t3
      t6 = z + t5
      t7 = 0.1D1 / t6
      t8 = x1 * t7
      t9 = 0.1D1 - x2
      t10 = x3 * t9
      t12 = 0.1D1 - x3
      t15 = cos(x4 * 0.3141592653589793D1)
      t19 = Sqrt(t10 * t6 * x2 * t12)
      t21 = 0.2D1 * t15 * t19
      t22 = t10 * t6 + x2 * t12 - t21
      t23 = t8 * t22
      t25 = 0.1D1 - x1
      t26 = t25 * x3
      t28 = s - t4 * t23 - t4 * t26
      t29 = t2 * t28
      t33 = t12 * t9 * t6 + x2 * x3 + t21
      t34 = t8 * t33
      t36 = t25 * t12
      t38 = s - t4 * t34 - t4 * t36
      t39 = t38 ** 2
      t40 = t3 ** 2
      t41 = t39 * t40
      t42 = t29 * t41
      t43 = t26 * t23
      t46 = t1 * s
      t47 = t28 ** 2
      t48 = t46 * t47
      t49 = t48 * t41
      t52 = t2 * s
      t53 = t40 ** 2
      t54 = t53 * t40
      t55 = t52 * t54
      t56 = x2 ** 2
      t57 = t56 * x2
      t58 = x1 ** 2
      t59 = t58 * x1
      t60 = t57 * t59
      t62 = t25 ** 2
      t63 = t62 * t25
      t64 = t6 ** 2
      t66 = 0.1D1 / t64 / t6
      t67 = t63 * t66
      t68 = t28 * t38
      t73 = t2 * t54 * t60
      t74 = t28 * t39
      t78 = t52 * z
      t79 = t53 * x2
      t81 = t78 * t79 * t59
      t82 = t25 * t66
      t83 = t33 ** 2
      t85 = t82 * t68 * t83
      t88 = t40 * t3
      t89 = t2 * t88
      t90 = x2 * t58
      t92 = t89 * t90 * t25
      t93 = 0.1D1 / t64
      t94 = t93 * t47
      t95 = t38 * z
      t100 = t2 * t53
      t101 = x2 * x1
      t102 = t100 * t101
      t103 = t63 * t7
      t104 = x3 ** 2
      t109 = t53 * t3
      t110 = t2 * t109
      t111 = t56 * t59
      t112 = t110 * t111
      t113 = t62 * t66
      t114 = t74 * t33
      t118 = t52 * t53
      t119 = x2 * t59
      t120 = t119 * t25
      t121 = t118 * t120
      t122 = z ** 2
      t125 = t33 * t38
      t126 = t125 * t22
      t130 = t47 * t38
      t134 = t56 * t58
      t135 = t100 * t134
      t136 = t62 * t93
      t140 = -0.288D3 * t42 * t43 + 0.864D3 * t49 * t43 - 0.224D3 * t55 
     #* t60 * t67 * t68 + 0.1440D4 * t73 * t67 * t74 + 0.576D3 * t81 * t
     #85 - 0.1728D4 * t92 * t94 * t95 * t22 - 0.288D3 * t102 * t103 * t7
     #4 * t104 - 0.1440D4 * t112 * t113 * t114 + 0.288D3 * t121 * t66 * 
     #t122 * t28 * t126 + 0.1440D4 * t73 * t67 * t130 - 0.1632D4 * t135 
     #* t136 * t130
      t141 = t2 * z
      t143 = t141 * t130 * t40
      t146 = t8 * t33 * t25 * x3
      t149 = t90 * t62
      t150 = t118 * t149
      t151 = t93 * t122
      t152 = t12 * t47
      t153 = t152 * t33
      t157 = t100 * t119
      t163 = t46 * t40 * t101
      t164 = t25 * t7
      t165 = t47 * t39
      t171 = t78 * t79 * t58
      t172 = t136 * t28
      t174 = t38 * t12 * t33
      t178 = t110 * t134
      t179 = t63 * t93
      t180 = t130 * x3
      t184 = t89 * t90
      t185 = t25 * t93
      t193 = t130 * t22
      t202 = -0.288D3 * t143 * t146 + 0.288D3 * t150 * t151 * t153 - 0.2
     #88D3 * t157 * t82 * t130 * t83 + 0.3456D4 * t163 * t164 * t165 * z
     # - 0.1152D4 * t171 * t172 * t174 - 0.1440D4 * t178 * t179 * t180 +
     # 0.1244D4 * t184 * t185 * t114 - 0.3523D4 * t184 * t185 * t74 * t2
     #2 - 0.1440D4 * t112 * t113 * t193 + 0.1440D4 * t49 * t146 - 0.1632
     #D4 * t135 * t136 * t74
      t204 = t118 * t101
      t205 = t12 ** 2
      t211 = t22 ** 2
      t213 = t82 * t68 * t211
      t217 = t78 * t79 * x1
      t219 = t103 * t68 * t205
      t222 = t36 * t34
      t225 = t66 * t28
      t229 = t93 * t28
      t231 = t38 * t22 * t12
      t236 = t2 * t40 * t101
      t245 = t141 * t74
      t246 = t40 * t58
      t248 = t246 * t93 * t83
      t251 = t122 * t39
      t260 = -0.144D3 * t204 * t103 * t122 * t205 * t47 + 0.576D3 * t81 
     #* t213 + 0.576D3 * t217 * t219 - 0.288D3 * t143 * t222 + 0.864D3 *
     # t121 * t225 * t126 - 0.864D3 * t150 * t229 * t231 - 0.1728D4 * t2
     #36 * t164 * t74 * z - 0.1728D4 * t236 * t164 * t130 * z + 0.288D3 
     #* t245 * t248 - 0.144D3 * t204 * t103 * t251 * t104 + 0.128D3 * t1
     #18 * t134 * t136 * t68
      t261 = t89 * t101
      t262 = t62 * t7
      t263 = t74 * t12
      t267 = t52 * t40
      t268 = t267 * t101
      t280 = x1 * t25
      t281 = t7 * t28
      t286 = t101 * t63
      t287 = t118 * t286
      t288 = t38 * x3
      t289 = t288 * t12
      t293 = t100 * t286
      t294 = t39 * x3
      t295 = t294 * t12
      t299 = t100 * t120
      t300 = t39 * t22
      t301 = t300 * t33
      t316 = t141 * t74 * t40
      t319 = t125 * x3
      t323 = 0.1244D4 * t261 * t262 * t263 + 0.576D3 * t268 * t164 * t68
     # + 0.1728D4 * t268 * t164 * t68 * z + 0.576D3 * t52 * t122 * z * t
     #40 * x2 * t280 * t281 * t38 + 0.864D3 * t287 * t281 * t289 - 0.144
     #D3 * t293 * t281 * t295 - 0.144D3 * t299 * t225 * t301 + 0.288D3 *
     # t287 * t7 * t122 * t12 * t68 * x3 - 0.3523D4 * t261 * t262 * t74 
     #* x3 - 0.288D3 * t316 * t146 - 0.864D3 * t150 * t229 * t319
      t332 = t118 * t90
      t333 = t294 * t22
      t337 = t39 * z
      t343 = t89 * t101 * t62
      t348 = t100 * t149
      t349 = t39 * t33
      t350 = t349 * x3
      t365 = t141 * t130
      t368 = t40 * t62
      t370 = t368 * x3 * t12
      t373 = -0.1152D4 * t171 * t172 * t319 - 0.1152D4 * t171 * t172 * t
     #231 - 0.752D3 * t332 * t136 * t333 - 0.1728D4 * t92 * t229 * t337 
     #* t33 - 0.1728D4 * t343 * t281 * t337 * t12 + 0.144D3 * t348 * t22
     #9 * t350 + 0.576D3 * t348 * t229 * t333 - 0.3523D4 * t261 * t262 *
     # t130 * t12 - 0.3523D4 * t184 * t185 * t130 * t33 + 0.144D3 * t365
     # * t248 + 0.288D3 * t365 * t370
      t382 = t8 * t22 * t25 * t12
      t388 = t7 * t47
      t392 = t118 * t119
      t397 = t288 * t22
      t406 = t246 * t93 * t211
      t412 = 0.1244D4 * t261 * t262 * t180 + 0.1244D4 * t184 * t185 * t1
     #93 - 0.288D3 * t316 * t382 - 0.1440D4 * t178 * t179 * t263 - 0.144
     #D3 * t293 * t388 * t289 + 0.576D3 * t392 * t85 + 0.576D3 * t392 * 
     #t213 - 0.1152D4 * t171 * t172 * t397 + 0.1152D4 * t217 * t103 * t2
     #8 * t289 + 0.144D3 * t245 * t406 + 0.3466D4 * t236 * t164 * t130
      t414 = t66 * t47
      t422 = t103 * t68 * t104
      t429 = t246 * t93 * t22 * t33
      t449 = -0.144D3 * t299 * t414 * t126 + 0.144D3 * t348 * t94 * t319
     # + 0.576D3 * t204 * t422 + 0.576D3 * t204 * t219 + 0.288D3 * t365 
     #* t429 - 0.288D3 * t316 * t43 + 0.288D3 * t245 * t370 + 0.288D3 * 
     #t245 * t429 - 0.288D3 * t102 * t103 * t130 * t205 - 0.1728D4 * t34
     #3 * t388 * t95 * x3 + 0.288D3 * t150 * t151 * t333
      t458 = t118 * t56
      t459 = t58 * t62
      t463 = t55 * t57
      t464 = t59 * t63
      t468 = t267 * x2
      t472 = t2 * t47
      t473 = t472 * t38
      t477 = t5 * t7 * t33
      t480 = t29 * t39
      t483 = t48 * t39
      t492 = 0.1728D4 * t268 * t164 * t68 * t122 + 0.1152D4 * t81 * t82 
     #* t28 * t126 + 0.1088D4 * t458 * t459 * t94 - 0.272D3 * t463 * t46
     #4 * t414 - 0.472D3 * t468 * t280 * t388 - 0.144D3 * t473 * t248 + 
     #0.144D3 * t473 * t477 + 0.14832D5 * t480 * t477 - 0.14976D5 * t483
     # * t477 + 0.432D3 * t483 * t248 + 0.1088D4 * t458 * t459 * t93 * t
     #39
      t505 = t5 * t7 * t22
      t522 = t141 * t47
      t523 = t38 * t40
      t524 = t62 * t205
      t528 = -0.272D3 * t463 * t464 * t66 * t39 - 0.472D3 * t468 * t280 
     #* t7 * t39 + 0.14832D5 * t473 * t505 - 0.144D3 * t480 * t406 + 0.1
     #44D3 * t480 * t505 - 0.14976D5 * t483 * t505 + 0.432D3 * t483 * t4
     #06 - 0.14832D5 * t473 * t370 - 0.14832D5 * t480 * t370 + 0.864D3 *
     # t483 * t370 + 0.144D3 * t522 * t523 * t524
      t529 = t141 * t28
      t533 = t62 * t104
      t540 = t300 * t12
      t547 = t3 * t25
      t548 = t547 * x3
      t555 = t547 * t12
      t562 = 0.288D3 * t529 * t41 * t524 + 0.288D3 * t522 * t523 * t533 
     #+ 0.144D3 * t529 * t41 * t533 + 0.144D3 * t348 * t229 * t540 - 0.1
     #152D4 * t150 * t229 * t397 + 0.14832D5 * t473 * t548 + 0.144D3 * t
     #480 * t548 - 0.14976D5 * t483 * t548 + 0.144D3 * t473 * t555 + 0.1
     #4832D5 * t480 * t555 - 0.14976D5 * t483 * t555
      t564 = t368 * t104
      t569 = t368 * t205
      t596 = -0.144D3 * t480 * t564 + 0.432D3 * t483 * t564 - 0.144D3 * 
     #t473 * t569 + 0.432D3 * t483 * t569 - 0.7896D4 * t163 * t164 * t16
     #5 + 0.3466D4 * t236 * t164 * t74 + 0.2304D4 * t46 * t53 * t134 * t
     #136 * t165 - 0.1152D4 * t150 * t229 * t174 - 0.144D3 * t392 * t82 
     #* t251 * t211 - 0.288D3 * t143 * t382 + 0.288D3 * t365 * t406
      t610 = t52 * t88
      t611 = t610 * t101
      t612 = t47 * x3
      t616 = t610 * t90
      t617 = t47 * t22
      t621 = t52 * t109
      t622 = t621 * t134
      t626 = t621 * t111
      t642 = -0.752D3 * t332 * t136 * t153 + 0.576D3 * t217 * t422 - 0.2
     #72D3 * t392 * t82 * t47 * t211 - 0.272D3 * t204 * t103 * t47 * t10
     #4 - 0.1088D4 * t611 * t262 * t612 - 0.1088D4 * t616 * t185 * t617 
     #+ 0.544D3 * t622 * t179 * t612 + 0.544D3 * t626 * t113 * t617 - 0.
     #544D3 * t332 * t136 * t612 * t22 - 0.288D3 * t157 * t82 * t74 * t2
     #11 - 0.376D3 * t204 * t103 * t47 * t205
      t678 = t39 * t12
      t685 = -0.1088D4 * t622 * t179 * t152 + 0.848D3 * t611 * t262 * t1
     #52 + 0.1088D4 * t204 * t103 * t612 * t12 - 0.144D3 * t392 * t82 * 
     #t122 * t47 * t83 + 0.1088D4 * t332 * t136 * t617 * t12 - 0.272D3 *
     # t204 * t103 * t39 * t205 - 0.272D3 * t392 * t82 * t39 * t83 - 0.1
     #088D4 * t616 * t185 * t349 + 0.544D3 * t626 * t113 * t349 + 0.544D
     #3 * t622 * t179 * t678 - 0.1088D4 * t611 * t262 * t678
      t706 = t472 * t523
      t709 = t58 * t93 * t22 * t33
      t723 = -0.544D3 * t332 * t136 * t678 * t33 - 0.376D3 * t204 * t103
     # * t39 * t104 + 0.848D3 * t611 * t262 * t294 - 0.1088D4 * t622 * t
     #179 * t294 + 0.1088D4 * t204 * t103 * t295 + 0.1088D4 * t332 * t13
     #6 * t350 - 0.14832D5 * t706 * t709 - 0.14832D5 * t42 * t709 + 0.14
     #4D3 * t348 * t94 * t231 + 0.864D3 * t49 * t709 - 0.376D3 * t392 * 
     #t82 * t47 * t83
      t725 = t47 * t33
      t759 = 0.848D3 * t616 * t185 * t725 - 0.288D3 * t150 * t151 * t12 
     #* t68 * t22 - 0.288D3 * t150 * t151 * t28 * t319 - 0.1088D4 * t626
     # * t113 * t725 + 0.1088D4 * t392 * t82 * t617 * t33 + 0.1088D4 * t
     #332 * t136 * t725 * x3 - 0.14976D5 * t706 * t382 - 0.288D3 * t706 
     #* t222 - 0.14976D5 * t42 * t382 - 0.576D3 * t42 * t222 + 0.864D3 *
     # t49 * t222
      t795 = 0.1440D4 * t49 * t382 - 0.376D3 * t392 * t82 * t39 * t211 +
     # 0.848D3 * t616 * t185 * t300 - 0.1728D4 * t236 * t164 * t130 * t1
     #22 - 0.1728D4 * t236 * t164 * t74 * t122 - 0.1088D4 * t626 * t113 
     #* t300 + 0.1088D4 * t392 * t82 * t301 + 0.1088D4 * t332 * t136 * t
     #540 - 0.14976D5 * t706 * t146 - 0.576D3 * t706 * t43 + 0.576D3 * t
     #348 * t94 * t174 - 0.14976D5 * t42 * t146
      rrqqbar2ggh11J1 = wd * (t140 + t202 + t260 + t323 + t373 + t412 + 
     #t449 + t492 + t528 + t562 + t596 + t642 + t685 + t723 + t759 + t79
     #5) / t46 / t47 / t39 / z / 0.3141592653589793D1 / 0.27D2

      end function
  
   
 

      doubleprecision function rrqqbar2ggh11J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = 0.1D1 - z
      t4 = t3 ** 2
      t6 = x2 * x1
      t7 = t2 * t4 * t6
      t8 = 0.1D1 - x1
      t9 = x1 * t3
      t10 = z + t9
      t11 = 0.1D1 / t10
      t12 = t8 * t11
      t13 = s * t3
      t14 = x1 * t11
      t15 = 0.1D1 - x2
      t16 = x3 * t15
      t18 = 0.1D1 - x3
      t21 = cos(x4 * 0.3141592653589793D1)
      t25 = Sqrt(t16 * t10 * x2 * t18)
      t27 = 0.2D1 * t21 * t25
      t28 = t16 * t10 + x2 * t18 - t27
      t29 = t14 * t28
      t31 = t8 * x3
      t33 = s - t13 * t29 - t13 * t31
      t34 = t33 ** 2
      t38 = t18 * t15 * t10 + x2 * x3 + t27
      t39 = t14 * t38
      t41 = t8 * t18
      t43 = s - t13 * t39 - t13 * t41
      t44 = t34 * t43
      t49 = z ** 2
      t54 = t43 ** 2
      t55 = t33 * t54
      t60 = t4 * t3
      t61 = t2 * t60
      t62 = x1 ** 2
      t63 = x2 * t62
      t65 = t61 * t63 * t8
      t66 = t10 ** 2
      t67 = 0.1D1 / t66
      t68 = t67 * t34
      t69 = t43 * z
      t74 = t8 ** 2
      t76 = t61 * t6 * t74
      t77 = t11 * t34
      t82 = t67 * t33
      t83 = t54 * z
      t88 = t11 * t33
      t93 = t2 * s
      t94 = t4 ** 2
      t95 = t93 * t94
      t96 = t74 * t8
      t97 = t6 * t96
      t101 = t33 * t43
      t106 = t63 * t74
      t107 = t95 * t106
      t108 = t67 * t49
      t115 = t38 * t43
      t116 = t115 * x3
      t120 = t62 * x1
      t121 = x2 * t120
      t122 = t121 * t8
      t125 = 0.1D1 / t66 / t10
      t128 = t115 * t28
      t132 = 0.1728D4 * t7 * t12 * t44 * z + 0.1728D4 * t7 * t12 * t44 *
     # t49 + 0.1728D4 * t7 * t12 * t55 * t49 + 0.1728D4 * t65 * t68 * t6
     #9 * t28 + 0.1728D4 * t76 * t77 * t69 * x3 + 0.1728D4 * t65 * t82 *
     # t83 * t38 + 0.1728D4 * t76 * t88 * t83 * t18 - 0.288D3 * t95 * t9
     #7 * t11 * t49 * t18 * t101 * x3 + 0.288D3 * t107 * t108 * t18 * t1
     #01 * t28 + 0.288D3 * t107 * t108 * t33 * t116 - 0.288D3 * t95 * t1
     #22 * t125 * t49 * t33 * t128
      t133 = t2 * z
      t135 = t133 * t55 * t4
      t138 = t14 * t28 * t8 * t18
      t141 = t95 * t121
      t142 = t8 * t125
      t143 = t49 * t54
      t144 = t28 ** 2
      t149 = t133 * t44
      t150 = t4 * t74
      t152 = t150 * x3 * t18
      t156 = t133 * t44 * t4
      t159 = t14 * t38 * t8 * x3
      t162 = t93 * t4
      t163 = t162 * t6
      t176 = x1 * t8
      t181 = t1 * s
      t183 = t181 * t4 * t6
      t184 = t34 * t54
      t193 = t95 * t63
      t194 = t74 * t67
      t195 = t54 * x3
      t196 = t195 * t28
      t200 = t61 * t6
      t201 = t74 * t11
      t202 = t55 * t18
      t206 = -0.288D3 * t135 * t138 + 0.144D3 * t141 * t142 * t143 * t14
     #4 + 0.288D3 * t149 * t152 - 0.288D3 * t156 * t159 - 0.1728D4 * t16
     #3 * t12 * t101 * t49 - 0.1728D4 * t163 * t12 * t101 * z - 0.576D3 
     #* t93 * t49 * z * t4 * x2 * t176 * t88 * t43 - 0.3456D4 * t183 * t
     #12 * t184 * z + 0.1728D4 * t7 * t12 * t55 * z + 0.1168D4 * t193 * 
     #t194 * t196 - 0.3776D4 * t200 * t201 * t202
      t208 = t61 * t63
      t209 = t8 * t67
      t210 = t55 * t38
      t218 = t44 * t28
      t222 = t94 * t3
      t223 = t2 * t222
      t224 = x2 ** 2
      t225 = t224 * t62
      t226 = t223 * t225
      t227 = t96 * t67
      t228 = t44 * x3
      t232 = t224 * t120
      t233 = t223 * t232
      t234 = t74 * t125
      t238 = t93 * t222
      t239 = t238 * t232
      t240 = t54 * t38
      t244 = t238 * t225
      t245 = t54 * t18
      t249 = t93 * t60
      t250 = t249 * t6
      t254 = t245 * t38
      t258 = t95 * t6
      t259 = t96 * t11
      t260 = x3 ** 2
      t268 = -0.3776D4 * t208 * t209 * t210 - 0.5266D4 * t200 * t201 * t
     #55 * x3 - 0.3776D4 * t208 * t209 * t218 + 0.1728D4 * t226 * t227 *
     # t228 + 0.1728D4 * t233 * t234 * t218 - 0.864D3 * t239 * t234 * t2
     #40 - 0.864D3 * t244 * t227 * t245 + 0.1728D4 * t250 * t201 * t245 
     #+ 0.864D3 * t193 * t194 * t254 + 0.584D3 * t258 * t259 * t54 * t26
     #0 - 0.1360D4 * t250 * t201 * t195
      t269 = t2 * t33
      t270 = t54 * t4
      t271 = t269 * t270
      t274 = t41 * t39
      t277 = t181 * t34
      t278 = t277 * t270
      t281 = t2 * t94
      t282 = t281 * t122
      t284 = t54 * t28
      t285 = t284 * t38
      t289 = t281 * t106
      t290 = t240 * x3
      t294 = t181 * t60
      t295 = t294 * t63
      t300 = t294 * t6
      t305 = t34 * x3
      t309 = t34 * t28
      t317 = t18 ** 2
      t322 = t34 * t18
      t326 = 0.15552D5 * t271 * t138 + 0.864D3 * t271 * t274 - 0.2016D4 
     #* t278 * t274 + 0.72D2 * t282 * t125 * t33 * t285 - 0.72D2 * t289 
     #* t82 * t290 + 0.2808D4 * t295 * t209 * t184 * t38 + 0.2808D4 * t3
     #00 * t201 * t184 * x3 - 0.864D3 * t244 * t227 * t305 - 0.864D3 * t
     #239 * t234 * t309 + 0.864D3 * t193 * t194 * t305 * t28 + 0.584D3 *
     # t258 * t259 * t34 * t317 + 0.1728D4 * t244 * t227 * t322
      t336 = t269 * t54
      t337 = t150 * t317
      t340 = t2 * t34
      t341 = t340 * t43
      t342 = t150 * t260
      t345 = t3 * t8
      t346 = t345 * x3
      t351 = t277 * t54
      t354 = t345 * t18
      t363 = -0.1360D4 * t250 * t201 * t322 - 0.1728D4 * t258 * t259 * t
     #305 * t18 + 0.432D3 * t336 * t337 + 0.432D3 * t341 * t342 - 0.1555
     #2D5 * t341 * t346 - 0.2160D4 * t336 * t346 + 0.14616D5 * t351 * t3
     #46 - 0.2160D4 * t341 * t354 - 0.15552D5 * t336 * t354 + 0.14616D5 
     #* t351 * t354 - 0.936D3 * t336 * t342
      t371 = t38 ** 2
      t382 = t31 * t29
      t385 = t181 * z
      t387 = t385 * t184 * t4
      t392 = -0.1008D4 * t351 * t342 - 0.936D3 * t341 * t337 - 0.1008D4 
     #* t351 * t337 + 0.144D3 * t141 * t142 * t49 * t34 * t371 + 0.576D3
     # * t156 * t274 - 0.288D3 * t156 * t138 - 0.576D3 * t135 * t274 - 0
     #.576D3 * t156 * t382 + 0.576D3 * t387 * t159 + 0.3096D4 * t341 + 0
     #.3096D4 * t336
      t407 = t249 * t63
      t411 = t281 * t225
      t430 = -0.6192D4 * t351 - 0.1728D4 * t193 * t194 * t309 * t18 + 0.
     #432D3 * t258 * t259 * t54 * t317 + 0.432D3 * t141 * t142 * t54 * t
     #371 + 0.1728D4 * t407 * t209 * t240 + 0.4816D4 * t411 * t194 * t55
     # + 0.4940D4 * t7 * t12 * t44 + 0.4816D4 * t411 * t194 * t44 - 0.17
     #28D4 * t278 * t138 + 0.584D3 * t141 * t142 * t54 * t144 - 0.1360D4
     # * t407 * t209 * t284
      t437 = t284 * t18
      t444 = t195 * t18
      t451 = t43 * t4
      t452 = t340 * t451
      t455 = t62 * t67 * t28 * t38
      t460 = t385 * t184
      t461 = t4 * t62
      t464 = t461 * t67 * t38 * t28
      t467 = t34 * t33
      t468 = t467 * t43
      t471 = t461 * t67 * t371
      t474 = t43 * x3
      t482 = 0.1728D4 * t239 * t234 * t284 - 0.1728D4 * t141 * t142 * t2
     #85 - 0.1728D4 * t193 * t194 * t437 + 0.1728D4 * t244 * t227 * t195
     # - 0.1728D4 * t258 * t259 * t444 - 0.1728D4 * t193 * t194 * t290 +
     # 0.15696D5 * t452 * t455 + 0.15696D5 * t271 * t455 - 0.576D3 * t46
     #0 * t464 + 0.288D3 * t385 * t468 * t471 - 0.576D3 * t289 * t68 * t
     #474 * t28 - 0.576D3 * t289 * t82 * t254
      t500 = t34 * t38
      t509 = t54 * t43
      t510 = t33 * t509
      t517 = t461 * t67 * t144
      t526 = 0.432D3 * t141 * t142 * t34 * t144 + 0.432D3 * t258 * t259 
     #* t34 * t260 + 0.1728D4 * t250 * t201 * t305 + 0.1728D4 * t407 * t
     #209 * t309 - 0.1728D4 * t193 * t194 * t500 * x3 + 0.15552D5 * t452
     # * t138 - 0.1872D4 * t452 * t274 - 0.576D3 * t385 * t510 * t4 * t3
     #82 + 0.288D3 * t385 * t510 * t517 + 0.144D3 * t258 * t259 * t143 *
     # t260 - 0.288D3 * t149 * t517
      t527 = t133 * t55
      t534 = t95 * t224
      t535 = t62 * t74
      t539 = t94 * t4
      t540 = t93 * t539
      t541 = t224 * x2
      t542 = t540 * t541
      t543 = t120 * t96
      t544 = t125 * t34
      t548 = t162 * x2
      t555 = t9 * t11 * t38
      t564 = -0.288D3 * t527 * t517 + 0.288D3 * t149 * t464 + 0.288D3 * 
     #t527 * t464 - 0.1728D4 * t534 * t535 * t68 + 0.432D3 * t542 * t543
     # * t544 + 0.776D3 * t548 * t176 * t77 - 0.936D3 * t341 * t471 - 0.
     #2160D4 * t341 * t555 - 0.15552D5 * t336 * t555 + 0.14616D5 * t351 
     #* t555 - 0.1008D4 * t351 * t471
      t579 = t9 * t11 * t28
      t596 = -0.1728D4 * t534 * t535 * t67 * t54 + 0.432D3 * t542 * t543
     # * t125 * t54 + 0.776D3 * t548 * t176 * t11 * t54 - 0.15552D5 * t3
     #41 * t579 - 0.936D3 * t336 * t517 - 0.2160D4 * t336 * t579 + 0.146
     #16D5 * t351 * t579 - 0.1008D4 * t351 * t517 + 0.15696D5 * t341 * t
     #152 + 0.15696D5 * t336 * t152 - 0.2016D4 * t351 * t152
      t597 = t133 * t34
      t598 = t74 * t317
      t599 = t451 * t598
      t602 = t133 * t33
      t606 = t74 * t260
      t633 = t281 * t6
      t640 = -0.288D3 * t597 * t599 - 0.288D3 * t602 * t270 * t598 - 0.2
     #88D3 * t597 * t451 * t606 - 0.288D3 * t602 * t270 * t606 + 0.432D3
     # * t341 * t517 + 0.432D3 * t336 * t471 + 0.288D3 * t385 * t467 * t
     #599 + 0.288D3 * t385 * t33 * t509 * t4 * t606 + 0.2808D4 * t295 * 
     #t209 * t184 * t28 + 0.2808D4 * t300 * t201 * t184 * t18 - 0.288D3 
     #* t633 * t259 * t44 * t260 - 0.1872D4 * t271 * t382
      t675 = -0.2016D4 * t278 * t382 - 0.1728D4 * t278 * t159 - 0.72D2 *
     # t289 * t82 * t437 + 0.144D3 * t258 * t259 * t49 * t317 * t34 - 0.
     #288D3 * t149 * t471 - 0.288D3 * t527 * t471 - 0.2016D4 * t278 * t4
     #55 + 0.584D3 * t141 * t142 * t34 * t371 - 0.1360D4 * t407 * t209 *
     # t500 + 0.1728D4 * t239 * t234 * t500 - 0.1728D4 * t141 * t142 * t
     #309 * t38
      t677 = t541 * t120
      t678 = t2 * t539 * t677
      t679 = t96 * t125
      t716 = -0.1440D4 * t678 * t679 * t44 - 0.1440D4 * t678 * t679 * t5
     #5 - 0.1024D4 * t95 * t225 * t194 * t101 - 0.352D3 * t163 * t12 * t
     #101 - 0.576D3 * t460 * t152 - 0.576D3 * t385 * t468 * t4 * t274 + 
     #0.576D3 * t387 * t138 + 0.1376D4 * t540 * t677 * t679 * t101 - 0.9
     #344D4 * t183 * t12 * t184 - 0.4320D4 * t181 * t94 * t225 * t194 * 
     #t184 + 0.4940D4 * t7 * t12 * t55
      t718 = t322 * t38
      t743 = t281 * t97
      t748 = t281 * t121
      t761 = 0.1168D4 * t193 * t194 * t718 - 0.5266D4 * t208 * t209 * t5
     #5 * t28 - 0.5266D4 * t200 * t201 * t44 * t18 - 0.5266D4 * t208 * t
     #209 * t44 * t38 - 0.3776D4 * t200 * t201 * t228 + 0.1728D4 * t233 
     #* t234 * t210 + 0.1728D4 * t226 * t227 * t202 + 0.72D2 * t743 * t7
     #7 * t474 * t18 - 0.288D3 * t748 * t142 * t44 * t144 - 0.288D3 * t6
     #33 * t259 * t55 * t317 - 0.288D3 * t748 * t142 * t55 * t371
      t794 = 0.15552D5 * t452 * t159 + 0.864D3 * t452 * t382 + 0.15552D5
     # * t271 * t159 + 0.576D3 * t135 * t382 + 0.288D3 * t527 * t152 - 0
     #.288D3 * t135 * t159 - 0.288D3 * t107 * t108 * t718 - 0.288D3 * t1
     #07 * t108 * t196 + 0.72D2 * t282 * t544 * t128 - 0.72D2 * t289 * t
     #68 * t116 - 0.72D2 * t289 * t68 * t43 * t28 * t18 + 0.72D2 * t743 
     #* t88 * t444
      rrqqbar2ggh11J2 = wd * (t132 + t206 + t268 + t326 + t363 + t392 + 
     #t430 + t482 + t526 + t564 + t596 + t640 + t675 + t716 + t761 + t79
     #4) / t181 / t34 / t54 / z / 0.3141592653589793D1 / 0.27D2

      end function
  
   
 

      doubleprecision function rrqqbar2ggh11J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = 0.1D1 - z
      t5 = t4 ** 2
      t6 = t5 ** 2
      t7 = t3 * t6
      t8 = x2 ** 2
      t9 = t7 * t8
      t10 = x1 ** 2
      t11 = 0.1D1 - x1
      t12 = t11 ** 2
      t13 = t10 * t12
      t15 = z + x1 * t4
      t16 = t15 ** 2
      t17 = 0.1D1 / t16
      t18 = s * t4
      t19 = 0.1D1 / t15
      t20 = x1 * t19
      t21 = 0.1D1 - x2
      t22 = x3 * t21
      t24 = 0.1D1 - x3
      t27 = cos(x4 * 0.3141592653589793D1)
      t31 = Sqrt(t22 * t15 * x2 * t24)
      t33 = 0.2D1 * t27 * t31
      t34 = t22 * t15 + x2 * t24 - t33
      t39 = s - t18 * t20 * t34 - t18 * t11 * x3
      t40 = t39 ** 2
      t46 = t3 * t6 * t5
      t47 = t8 * x2
      t48 = t46 * t47
      t49 = t10 * x1
      t50 = t12 * t11
      t51 = t49 * t50
      t53 = 0.1D1 / t16 / t15
      t58 = t3 * t5
      t59 = x2 * t58
      t60 = x1 * t11
      t68 = t24 * t21 * t15 + x2 * x3 + t33
      t73 = s - t18 * t20 * t68 - t18 * t11 * t24
      t74 = t73 ** 2
      t88 = t7 * x2 * t49
      t89 = t11 * t53
      t90 = t34 ** 2
      t95 = x2 * x1
      t96 = t7 * t95
      t97 = t50 * t19
      t98 = x3 ** 2
      t103 = t5 * t4
      t104 = t3 * t103
      t105 = t104 * t95
      t106 = t12 * t19
      t107 = t40 * x3
      t111 = x2 * t10
      t112 = t104 * t111
      t113 = t11 * t17
      t114 = t40 * t34
      t119 = t3 * t6 * t4
      t120 = t8 * t10
      t121 = t119 * t120
      t122 = t50 * t17
      t127 = t119 * t8 * t49
      t128 = t12 * t53
      t132 = t7 * t111
      t133 = t12 * t17
      t138 = t24 ** 2
      t143 = 0.640D3 * t9 * t13 * t17 * t40 - 0.160D3 * t48 * t51 * t53 
     #* t40 - 0.304D3 * t59 * t60 * t19 * t40 + 0.640D3 * t9 * t13 * t17
     # * t74 - 0.160D3 * t48 * t51 * t53 * t74 - 0.304D3 * t59 * t60 * t
     #19 * t74 - 0.160D3 * t88 * t89 * t40 * t90 - 0.160D3 * t96 * t97 *
     # t40 * t98 - 0.640D3 * t105 * t106 * t107 - 0.640D3 * t112 * t113 
     #* t114 + 0.320D3 * t121 * t122 * t107 + 0.320D3 * t127 * t128 * t1
     #14 - 0.320D3 * t132 * t133 * t107 * t34 - 0.208D3 * t96 * t97 * t4
     #0 * t138
      t144 = t40 * t24
      t163 = t68 ** 2
      t168 = t74 * t68
      t175 = t74 * t24
      t190 = t74 * x3
      t201 = -0.640D3 * t121 * t122 * t144 + 0.512D3 * t105 * t106 * t14
     #4 + 0.640D3 * t96 * t97 * t107 * t24 + 0.640D3 * t132 * t133 * t11
     #4 * t24 - 0.160D3 * t96 * t97 * t74 * t138 - 0.160D3 * t88 * t89 *
     # t74 * t163 - 0.640D3 * t112 * t113 * t168 + 0.320D3 * t127 * t128
     # * t168 + 0.320D3 * t121 * t122 * t175 - 0.640D3 * t105 * t106 * t
     #175 - 0.320D3 * t132 * t133 * t175 * t68 - 0.208D3 * t96 * t97 * t
     #74 * t98 + 0.512D3 * t105 * t106 * t190 - 0.640D3 * t121 * t122 * 
     #t190 + 0.640D3 * t96 * t97 * t190 * t24
      t211 = t40 * t68
      t230 = t74 * t34
      t246 = t2 * t6 * t120
      t247 = t39 * t74
      t252 = t2 * t5 * t95
      t253 = t11 * t19
      t254 = t40 * t73
      t261 = 0.640D3 * t132 * t133 * t168 * x3 - 0.208D3 * t88 * t89 * t
     #40 * t163 + 0.512D3 * t112 * t113 * t211 - 0.640D3 * t127 * t128 *
     # t211 + 0.640D3 * t88 * t89 * t114 * t68 + 0.640D3 * t132 * t133 *
     # t211 * x3 - 0.208D3 * t88 * t89 * t74 * t90 + 0.512D3 * t112 * t1
     #13 * t230 - 0.640D3 * t127 * t128 * t230 + 0.640D3 * t88 * t89 * t
     #230 * t68 + 0.640D3 * t132 * t133 * t230 * t24 - 0.912D3 * t246 * 
     #t133 * t247 - 0.424D3 * t252 * t253 * t254 - 0.912D3 * t246 * t133
     # * t254
      t263 = t39 * t73
      t277 = t1 * s
      t295 = t2 * t103
      t296 = t295 * t95
      t301 = t295 * t111
      t330 = 0.1152D4 * t7 * t120 * t133 * t263 - 0.576D3 * t58 * t95 * 
     #t253 * t263 - 0.576D3 * t46 * t47 * t49 * t50 * t53 * t263 + 0.172
     #8D4 * t277 * t5 * t95 * t253 * t40 * t74 - 0.424D3 * t252 * t253 *
     # t247 - 0.416D3 * t132 * t133 * t144 * t68 - 0.416D3 * t132 * t133
     # * t190 * t34 + 0.640D3 * t296 * t106 * t247 * t24 + 0.640D3 * t30
     #1 * t113 * t247 * t68 + 0.780D3 * t296 * t106 * t247 * x3 + 0.780D
     #3 * t301 * t113 * t247 * t34 + 0.780D3 * t296 * t106 * t254 * t24 
     #+ 0.780D3 * t301 * t113 * t254 * t68 + 0.640D3 * t296 * t106 * t25
     #4 * x3 + 0.640D3 * t301 * t113 * t254 * t34
      rrqqbar2ggh11J3 = wd * (t143 + t201 + t261 + t330) / t277 / t40 / 
     #t74 / z / 0.3141592653589793D1 / 0.27D2

      end function
  
 