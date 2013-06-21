  
      subroutine rrgg2qqbarht9
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarh91J1  
      doubleprecision rrgg2qqbarh91J2  
      doubleprecision rrgg2qqbarh91J3  
      doubleprecision rrgg2qqbarh91J4  
      doubleprecision rrgg2qqbarh91J5  
      doubleprecision rrgg2qqbarh91J6  
      doubleprecision rrgg2qqbarh91J7  
      doubleprecision rrgg2qqbarht9s1e1  
      doubleprecision rrgg2qqbarht9s1e0  
      doubleprecision rrgg2qqbarht9s1em1  
      doubleprecision rrgg2qqbarht9s1em2  
      doubleprecision rrgg2qqbarht9s1em3  
      doubleprecision rrgg2qqbarht9s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht9s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht9s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht9s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht9s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht9s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht9s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht9s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh91J1
      doubleprecision rrgg2qqbarh91J2
      doubleprecision rrgg2qqbarh91J3
      doubleprecision rrgg2qqbarh91J4
      doubleprecision rrgg2qqbarh91J5
      doubleprecision rrgg2qqbarh91J6
      doubleprecision rrgg2qqbarh91J7
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t10 = x3 * t9
      t12 = Sqrt(x2 * t7 * t10)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = t1 ** 2
      t20 = 0.3141592653589793D1 * t19
      t21 = 0.1D1 / s
      t22 = x2 * z
      t24 = (0.1D1 + t22 - x2) ** 2
      t25 = 0.1D1 / t24
      t26 = rrgg2qqbarh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 
     #x4)
      t28 = x1 ** 2
      t30 = Sin(t5)
      t31 = t30 ** 2
      t32 = z ** 2
      t33 = 0.1D1 / t32
      t39 = log(0.4D1 * t3 * t28 * t31 * t33 * t7 * t9)
      t41 = rrgg2qqbarh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 
     #x4)
      t43 = -t25 * t26 + t39 * t25 * t41
      t47 = 0.3141592653589793D1 * lh
      t49 = t21 * t25
      t50 = t49 * t41
      t52 = 0.180D3 * t47 * t19 * t50
      t54 = 0.1D1 / x1
      t57 = rrgg2qqbarh91J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 
     #x4)
      t60 = t20 * t49 * t57 / 0.16D2
      t67 = log(0.4D1 * x2 * t31 * t33 * t10 * t7)
      t68 = t67 * 0.3141592653589793D1
      t74 = (0.180D3 * t47 + 0.90D2 * t68) * t19 * t49 * t26 / 0.1440D4
      t77 = t67 ** 2
      t80 = lh ** 2
      t82 = 0.3141592653589793D1 ** 2
      t89 = (-0.180D3 * t68 * lh - 0.45D2 * t77 * 0.3141592653589793D1 +
     # 0.3141592653589793D1 * (-0.180D3 * t80 + 0.30D2 * t82)) * t19 * t
     #50 / 0.1440D4
      t90 = -(-0.90D2 * t20 * t21 * t43 - t52) * t54 / 0.720D3 - t60 + t
     #74 + t89
      t91 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, t90)
      t93 = x3 * x1
      t94 = t2 * t93
      t95 = -0.1D1 + x1
      t96 = t93 * z
      t97 = t3 * x1
      t98 = x1 * z
      t99 = t3 * t98
      t101 = 0.1D1 - x1 + t98
      t105 = Sqrt(x3 * t7 * t101 * x2 * t9)
      t107 = 0.2D1 * t6 * t105
      t110 = 0.1D1 / t101
      t112 = t2 * t95 * (-x3 + t93 - t96 + t4 - t97 + t99 - x2 + t107) *
     # t110
      t115 = t9 * s * t1 * x1
      t116 = x2 * x1
      t117 = t116 * z
      t118 = 0.1D1 - x1 + t98 - x2 + t116 - t117 - x3 + t93 - t96 + t4 -
     # t97 + t99 + t107
      t121 = t2 * t95 * t118 * t110
      t126 = s * t19 * x2 * x1 * t95 * t110
      t127 = t101 * t95
      t129 = (-0.1D1 + x1 - t22 + t117 - t98 + x2 - t116) ** 2
      t130 = 0.1D1 / t129
      t131 = -t95
      t132 = rrgg2qqbarh91J2(s, XB1, XB2, z, lh, wd, nf, t131, x2, x3, x
     #4)
      t138 = t95 ** 2
      t144 = log(0.4D1 * t3 * t28 * t31 * t33 * t110 * t138 * t7 * t9)
      t147 = rrgg2qqbarh91J1(s, XB1, XB2, z, lh, wd, nf, t131, x2, x3, x
     #4)
      t160 = -0.90D2 * t20 * t21 * (-t127 * t130 * t132 + t144 * t101 * 
     #t95 * t130 * t147) - 0.180D3 * t47 * t19 * t21 * t127 * t130 * t14
     #7
      t162 = t160 * t54 / 0.720D3
      t163 = FJET(XB1, XB2, s, t94, t112, -t115, -t121, -t126, -t162)
      t167 = FJET(XB1, XB2, s, t112, t94, -t121, -t115, -t126, -t162)
      t178 = -(-0.90D2 * t20 * t21 * t43 - t52) * t54 / 0.720D3 - t60 + 
     #t74 + t89
      t179 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, t178)
      rrgg2qqbarht9s1e1 = t91 * t90 - t163 * t160 * t54 / 0.720D3 - t167
     # * t160 * t54 / 0.720D3 + t179 * t178

      end function



      doubleprecision function rrgg2qqbarht9s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh91J1
      doubleprecision rrgg2qqbarh91J2
      doubleprecision rrgg2qqbarh91J3
      doubleprecision rrgg2qqbarh91J4
      doubleprecision rrgg2qqbarh91J5
      doubleprecision rrgg2qqbarh91J6
      doubleprecision rrgg2qqbarh91J7
      t1 = -0.1D1 + z
      t2 = t1 * s
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t10 = x3 * t9
      t12 = Sqrt(x2 * t7 * t10)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = t1 ** 2
      t20 = 0.3141592653589793D1 * t19
      t21 = 0.1D1 / s
      t23 = x2 * z
      t25 = (0.1D1 + t23 - x2) ** 2
      t26 = 0.1D1 / t25
      t27 = rrgg2qqbarh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 
     #x4)
      t29 = 0.1D1 / x1
      t33 = t21 * t26
      t34 = rrgg2qqbarh91J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 
     #x4)
      t40 = Sin(t5)
      t41 = t40 ** 2
      t43 = z ** 2
      t49 = log(0.4D1 * x2 * t41 / t43 * t10 * t7)
      t57 = -t20 * t21 * t26 * t27 * t29 / 0.8D1 - t20 * t33 * t34 / 0.1
     #6D2 + (0.180D3 * 0.3141592653589793D1 * lh + 0.90D2 * t49 * 0.3141
     #592653589793D1) * t19 * t33 * t27 / 0.1440D4
      t58 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, t57)
      t60 = x3 * x1
      t61 = t2 * t60
      t62 = -0.1D1 + x1
      t63 = t60 * z
      t64 = t3 * x1
      t65 = x1 * z
      t66 = t3 * t65
      t68 = 0.1D1 - x1 + t65
      t72 = Sqrt(x3 * t7 * t68 * x2 * t9)
      t74 = 0.2D1 * t6 * t72
      t77 = 0.1D1 / t68
      t79 = t2 * t62 * (-x3 + t60 - t63 + t4 - t64 + t66 - x2 + t74) * t
     #77
      t82 = t9 * s * t1 * x1
      t83 = x2 * x1
      t84 = t83 * z
      t85 = 0.1D1 - x1 + t65 - x2 + t83 - t84 - x3 + t60 - t63 + t4 - t6
     #4 + t66 + t74
      t88 = t2 * t62 * t85 * t77
      t93 = s * t19 * x2 * x1 * t62 * t77
      t97 = (-0.1D1 + x1 - t23 + t84 - t65 + x2 - t83) ** 2
      t98 = 0.1D1 / t97
      t101 = rrgg2qqbarh91J1(s, XB1, XB2, z, lh, wd, nf, -t62, x2, x3, x
     #4)
      t105 = t20 * t21 * t68 * t62 * t98 * t101 * t29 / 0.8D1
      t106 = FJET(XB1, XB2, s, t61, t79, -t82, -t88, -t93, -t105)
      t108 = t19 * t21
      t113 = t68 * t62 * t98 * t101 * t29
      t116 = FJET(XB1, XB2, s, t79, t61, -t88, -t82, -t93, -t105)
      t121 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, t57)
      rrgg2qqbarht9s1e0 = t58 * t57 - t106 * 0.3141592653589793D1 * t108
     # * t113 / 0.8D1 - t116 * 0.3141592653589793D1 * t108 * t113 / 0.8D
     #1 + t121 * t57

      end function



      doubleprecision function rrgg2qqbarht9s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh91J1
      doubleprecision rrgg2qqbarh91J2
      doubleprecision rrgg2qqbarh91J3
      doubleprecision rrgg2qqbarh91J4
      doubleprecision rrgg2qqbarh91J5
      doubleprecision rrgg2qqbarh91J6
      doubleprecision rrgg2qqbarh91J7
      t1 = -0.1D1 + z
      t2 = t1 * s
      t4 = 0.2D1 * x2 * x3
      t6 = cos(x4 * 0.3141592653589793D1)
      t12 = Sqrt(x2 * (-0.1D1 + x2) * x3 * (-0.1D1 + x3))
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = t1 ** 2
      t24 = (0.1D1 + x2 * z - x2) ** 2
      t27 = rrgg2qqbarh91J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, 
     #x4)
      t28 = 0.1D1 / s / t24 * t27
      t30 = 0.3141592653589793D1 * t19 * t28 / 0.16D2
      t31 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, -t30)
      t35 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, -t30)
      rrgg2qqbarht9s1em1 = -t31 * 0.3141592653589793D1 * t19 * t28 / 0.1
     #6D2 - t35 * 0.3141592653589793D1 * t19 * t28 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht9s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh91J1
      doubleprecision rrgg2qqbarh91J2
      doubleprecision rrgg2qqbarh91J3
      doubleprecision rrgg2qqbarh91J4
      doubleprecision rrgg2qqbarh91J5
      doubleprecision rrgg2qqbarh91J6
      doubleprecision rrgg2qqbarh91J7
      rrgg2qqbarht9s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht9s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh91J1
      doubleprecision rrgg2qqbarh91J2
      doubleprecision rrgg2qqbarh91J3
      doubleprecision rrgg2qqbarh91J4
      doubleprecision rrgg2qqbarh91J5
      doubleprecision rrgg2qqbarh91J6
      doubleprecision rrgg2qqbarh91J7
      rrgg2qqbarht9s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht9s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh91J1
      doubleprecision rrgg2qqbarh91J2
      doubleprecision rrgg2qqbarh91J3
      doubleprecision rrgg2qqbarh91J4
      doubleprecision rrgg2qqbarh91J5
      doubleprecision rrgg2qqbarh91J6
      doubleprecision rrgg2qqbarh91J7
      rrgg2qqbarht9s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh91J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = x1 ** 2
      t7 = z + t2 * x1
      t8 = t7 ** 2
      t9 = 0.1D1 / t8
      t10 = t5 * t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t15 = x2 * t14
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t7 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t7 + t15 - t23
      t25 = t24 ** 2
      t29 = 0.1D1 - x1
      t30 = t29 ** 2
      t31 = t14 ** 2
      t36 = t3 ** 2
      t38 = x2 ** 2
      t44 = x3 ** 2
      t50 = x2 * x3
      t51 = t14 * t11 * t7 + t50 + t23
      t52 = t51 ** 2
      t56 = t1 * t2
      t57 = t29 * t14
      t64 = t29 * x3
      t67 = 0.1D1 / t7
      t68 = x1 * t67
      t74 = t9 * t24
      t81 = t4 * x1
      t82 = t67 * t51
      t86 = t4 * t29
      t88 = t67 * t24
      t98 = t1 * t3 * t2
      t99 = t98 * t30
      t103 = t98 * t5
      t104 = x2 * t29
      t122 = -0.68D2 * t4 * t5 * t74 * t51 + 0.68D2 * t56 * t68 * t24 + 
     #0.136D3 * t81 * t82 * t64 + 0.136D3 * t86 * x3 * x1 * t88 + 0.136D
     #3 * t4 * x2 * x1 * t29 * t67 + 0.106D3 * t99 * t50 * t68 - 0.136D3
     # * t103 * t74 * t104 - 0.136D3 * t103 * t9 * t51 * t104 + 0.136D3 
     #* t81 * t88 * t57 + 0.136D3 * t86 * t14 * x1 * t82 + 0.106D3 * t99
     # * t15 * t68
      rrgg2qqbarh91J1 = wd * (-0.34D2 * t4 * t10 * t25 - 0.47D2 * t4 * t
     #30 * t31 - 0.34D2 * t1 - 0.59D2 * t1 * t36 * t38 * t5 * t30 * t9 -
     # 0.47D2 * t4 * t30 * t44 - 0.34D2 * t4 * t10 * t52 - 0.136D3 * t56
     # * t57 - 0.94D2 * t4 * t30 * x3 * t14 - 0.136D3 * t56 * t64 + 0.68
     #D2 * t56 * t68 * t51 + t122) * nf / s / z / 0.3141592653589793D1 /
     # 0.12D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh91J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = x1 ** 2
      t7 = z + t2 * x1
      t8 = t7 ** 2
      t9 = 0.1D1 / t8
      t10 = t5 * t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t15 = x2 * t14
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t7 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t7 + t15 - t23
      t25 = t24 ** 2
      t27 = t4 * t10 * t25
      t29 = 0.1D1 - x1
      t30 = t29 ** 2
      t31 = t14 ** 2
      t33 = t4 * t30 * t31
      t36 = t3 ** 2
      t38 = x2 ** 2
      t42 = t1 * t36 * t38 * t5 * t30 * t9
      t44 = x3 ** 2
      t46 = t4 * t30 * t44
      t50 = x2 * x3
      t51 = t14 * t11 * t7 + t50 + t23
      t52 = t51 ** 2
      t54 = t4 * t10 * t52
      t56 = t1 * t2
      t57 = t29 * t14
      t58 = t56 * t57
      t62 = t4 * t30 * x3 * t14
      t64 = t29 * x3
      t65 = t56 * t64
      t67 = 0.1D1 / t7
      t68 = x1 * t67
      t70 = t56 * t68 * t51
      t74 = t9 * t24
      t76 = t4 * t5 * t74 * t51
      t79 = t56 * t68 * t24
      t81 = t4 * x1
      t82 = t67 * t51
      t84 = t81 * t82 * t64
      t86 = t4 * t29
      t88 = t67 * t24
      t90 = t86 * x3 * x1 * t88
      t95 = t4 * x2 * x1 * t29 * t67
      t98 = t1 * t3 * t2
      t99 = t98 * t30
      t101 = t99 * t50 * t68
      t103 = t98 * t5
      t104 = x2 * t29
      t106 = t103 * t74 * t104
      t110 = t103 * t9 * t51 * t104
      t113 = t81 * t88 * t57
      t117 = t86 * t14 * x1 * t82
      t120 = t99 * t15 * t68
      t122 = -0.68D2 * t76 + 0.68D2 * t79 + 0.136D3 * t84 + 0.136D3 * t9
     #0 + 0.136D3 * t95 + 0.106D3 * t101 - 0.136D3 * t106 - 0.136D3 * t1
     #10 + 0.136D3 * t113 + 0.136D3 * t117 + 0.106D3 * t120
      t147 = -0.80D2 * t90 - 0.80D2 * t113 - 0.40D2 * t70 + 0.20D2 * t54
     # + 0.26D2 * t33 + 0.20D2 * t27 + 0.26D2 * t46 + 0.38D2 * t42 + 0.2
     #0D2 * t1 - 0.64D2 * t101 - 0.80D2 * t117
      rrgg2qqbarh91J2 = -(-wd * (-0.34D2 * t27 - 0.47D2 * t33 - 0.34D2 *
     # t1 - 0.59D2 * t42 - 0.47D2 * t46 - 0.34D2 * t54 - 0.136D3 * t58 -
     # 0.94D2 * t62 - 0.136D3 * t65 + 0.68D2 * t70 + t122) - wd * (-0.80
     #D2 * t95 + 0.80D2 * t58 + 0.80D2 * t106 + 0.40D2 * t76 + 0.52D2 * 
     #t62 + 0.80D2 * t110 - 0.80D2 * t84 + 0.80D2 * t65 - 0.64D2 * t120 
     #- 0.40D2 * t79 + t147)) * nf / s / z / 0.3141592653589793D1 / 0.12
     #D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh91J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = x1 ** 2
      t7 = z + t2 * x1
      t8 = t7 ** 2
      t9 = 0.1D1 / t8
      t10 = t5 * t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t15 = x2 * t14
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t7 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t7 + t15 - t23
      t25 = t24 ** 2
      t27 = t4 * t10 * t25
      t29 = 0.1D1 - x1
      t30 = t29 ** 2
      t31 = t14 ** 2
      t33 = t4 * t30 * t31
      t36 = t3 ** 2
      t38 = x2 ** 2
      t42 = t1 * t36 * t38 * t5 * t30 * t9
      t44 = x3 ** 2
      t46 = t4 * t30 * t44
      t50 = x2 * x3
      t51 = t14 * t11 * t7 + t50 + t23
      t52 = t51 ** 2
      t54 = t4 * t10 * t52
      t56 = t1 * t2
      t57 = t29 * t14
      t58 = t56 * t57
      t62 = t4 * t30 * x3 * t14
      t64 = t29 * x3
      t65 = t56 * t64
      t67 = 0.1D1 / t7
      t68 = x1 * t67
      t70 = t56 * t68 * t51
      t74 = t9 * t24
      t76 = t4 * t5 * t74 * t51
      t79 = t56 * t68 * t24
      t81 = t4 * x1
      t82 = t67 * t51
      t84 = t81 * t82 * t64
      t86 = t4 * t29
      t88 = t67 * t24
      t90 = t86 * x3 * x1 * t88
      t95 = t4 * x2 * x1 * t29 * t67
      t98 = t1 * t3 * t2
      t99 = t98 * t30
      t101 = t99 * t50 * t68
      t103 = t98 * t5
      t104 = x2 * t29
      t106 = t103 * t74 * t104
      t110 = t103 * t9 * t51 * t104
      t113 = t81 * t88 * t57
      t117 = t86 * t14 * x1 * t82
      t120 = t99 * t15 * t68
      t122 = -0.68D2 * t76 + 0.68D2 * t79 + 0.136D3 * t84 + 0.136D3 * t9
     #0 + 0.136D3 * t95 + 0.106D3 * t101 - 0.136D3 * t106 - 0.136D3 * t1
     #10 + 0.136D3 * t113 + 0.136D3 * t117 + 0.106D3 * t120
      t147 = -0.80D2 * t90 - 0.80D2 * t113 - 0.40D2 * t70 + 0.20D2 * t54
     # + 0.26D2 * t33 + 0.20D2 * t27 + 0.26D2 * t46 + 0.38D2 * t42 + 0.2
     #0D2 * t1 - 0.64D2 * t101 - 0.80D2 * t117
      rrgg2qqbarh91J3 = -(-wd * (-0.34D2 * t27 - 0.47D2 * t33 - 0.34D2 *
     # t1 - 0.59D2 * t42 - 0.47D2 * t46 - 0.34D2 * t54 - 0.136D3 * t58 -
     # 0.94D2 * t62 - 0.136D3 * t65 + 0.68D2 * t70 + t122) - wd * (-0.80
     #D2 * t95 + 0.80D2 * t58 + 0.80D2 * t106 + 0.40D2 * t76 + 0.52D2 * 
     #t62 + 0.80D2 * t110 - 0.80D2 * t84 + 0.80D2 * t65 - 0.64D2 * t120 
     #- 0.40D2 * t79 + t147)) * nf / s / z / 0.3141592653589793D1 / 0.12
     #D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh91J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = x1 ** 2
      t7 = z + t2 * x1
      t8 = t7 ** 2
      t9 = 0.1D1 / t8
      t10 = t5 * t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t15 = x2 * t14
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t7 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t7 + t15 - t23
      t25 = t24 ** 2
      t27 = t4 * t10 * t25
      t29 = 0.1D1 - x1
      t30 = t29 ** 2
      t31 = t14 ** 2
      t33 = t4 * t30 * t31
      t36 = t3 ** 2
      t38 = x2 ** 2
      t42 = t1 * t36 * t38 * t5 * t30 * t9
      t44 = x3 ** 2
      t46 = t4 * t30 * t44
      t50 = x2 * x3
      t51 = t14 * t11 * t7 + t50 + t23
      t52 = t51 ** 2
      t54 = t4 * t10 * t52
      t56 = t1 * t2
      t57 = t29 * t14
      t58 = t56 * t57
      t62 = t4 * t30 * x3 * t14
      t64 = t29 * x3
      t65 = t56 * t64
      t67 = 0.1D1 / t7
      t68 = x1 * t67
      t70 = t56 * t68 * t51
      t74 = t9 * t24
      t76 = t4 * t5 * t74 * t51
      t79 = t56 * t68 * t24
      t81 = t4 * x1
      t82 = t67 * t51
      t84 = t81 * t82 * t64
      t86 = t4 * t29
      t88 = t67 * t24
      t90 = t86 * x3 * x1 * t88
      t95 = t4 * x2 * x1 * t29 * t67
      t98 = t1 * t3 * t2
      t99 = t98 * t30
      t101 = t99 * t50 * t68
      t103 = t98 * t5
      t104 = x2 * t29
      t106 = t103 * t74 * t104
      t110 = t103 * t9 * t51 * t104
      t113 = t81 * t88 * t57
      t117 = t86 * t14 * x1 * t82
      t120 = t99 * t15 * t68
      t122 = -0.68D2 * t76 + 0.68D2 * t79 + 0.136D3 * t84 + 0.136D3 * t9
     #0 + 0.136D3 * t95 + 0.106D3 * t101 - 0.136D3 * t106 - 0.136D3 * t1
     #10 + 0.136D3 * t113 + 0.136D3 * t117 + 0.106D3 * t120
      t147 = -0.80D2 * t90 - 0.80D2 * t113 - 0.40D2 * t70 + 0.20D2 * t54
     # + 0.26D2 * t33 + 0.20D2 * t27 + 0.26D2 * t46 + 0.38D2 * t42 + 0.2
     #0D2 * t1 - 0.64D2 * t101 - 0.80D2 * t117
      rrgg2qqbarh91J4 = -(-wd * (-0.34D2 * t27 - 0.47D2 * t33 - 0.34D2 *
     # t1 - 0.59D2 * t42 - 0.47D2 * t46 - 0.34D2 * t54 - 0.136D3 * t58 -
     # 0.94D2 * t62 - 0.136D3 * t65 + 0.68D2 * t70 + t122) - wd * (-0.80
     #D2 * t95 + 0.80D2 * t58 + 0.80D2 * t106 + 0.40D2 * t76 + 0.52D2 * 
     #t62 + 0.80D2 * t110 - 0.80D2 * t84 + 0.80D2 * t65 - 0.64D2 * t120 
     #- 0.40D2 * t79 + t147)) * nf / s / z / 0.3141592653589793D1 / 0.12
     #D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh91J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = x1 ** 2
      t7 = z + t2 * x1
      t8 = t7 ** 2
      t9 = 0.1D1 / t8
      t10 = t5 * t9
      t11 = 0.1D1 - x2
      t12 = x3 * t11
      t14 = 0.1D1 - x3
      t15 = x2 * t14
      t17 = cos(x4 * 0.3141592653589793D1)
      t21 = Sqrt(t12 * t7 * x2 * t14)
      t23 = 0.2D1 * t17 * t21
      t24 = t12 * t7 + t15 - t23
      t25 = t24 ** 2
      t27 = t4 * t10 * t25
      t29 = 0.1D1 - x1
      t30 = t29 ** 2
      t31 = t14 ** 2
      t33 = t4 * t30 * t31
      t36 = t3 ** 2
      t38 = x2 ** 2
      t42 = t1 * t36 * t38 * t5 * t30 * t9
      t44 = x3 ** 2
      t46 = t4 * t30 * t44
      t50 = x2 * x3
      t51 = t14 * t11 * t7 + t50 + t23
      t52 = t51 ** 2
      t54 = t4 * t10 * t52
      t56 = t1 * t2
      t57 = t29 * t14
      t58 = t56 * t57
      t62 = t4 * t30 * x3 * t14
      t64 = t29 * x3
      t65 = t56 * t64
      t67 = 0.1D1 / t7
      t68 = x1 * t67
      t70 = t56 * t68 * t51
      t74 = t9 * t24
      t76 = t4 * t5 * t74 * t51
      t79 = t56 * t68 * t24
      t81 = t4 * x1
      t82 = t67 * t51
      t84 = t81 * t82 * t64
      t86 = t4 * t29
      t88 = t67 * t24
      t90 = t86 * x3 * x1 * t88
      t95 = t4 * x2 * x1 * t29 * t67
      t98 = t1 * t3 * t2
      t99 = t98 * t30
      t101 = t99 * t50 * t68
      t103 = t98 * t5
      t104 = x2 * t29
      t106 = t103 * t74 * t104
      t110 = t103 * t9 * t51 * t104
      t113 = t81 * t88 * t57
      t117 = t86 * t14 * x1 * t82
      t120 = t99 * t15 * t68
      t122 = -0.68D2 * t76 + 0.68D2 * t79 + 0.136D3 * t84 + 0.136D3 * t9
     #0 + 0.136D3 * t95 + 0.106D3 * t101 - 0.136D3 * t106 - 0.136D3 * t1
     #10 + 0.136D3 * t113 + 0.136D3 * t117 + 0.106D3 * t120
      t147 = -0.80D2 * t90 - 0.80D2 * t113 - 0.40D2 * t70 + 0.20D2 * t54
     # + 0.26D2 * t33 + 0.20D2 * t27 + 0.26D2 * t46 + 0.38D2 * t42 + 0.2
     #0D2 * t1 - 0.64D2 * t101 - 0.80D2 * t117
      rrgg2qqbarh91J5 = -(-wd * (-0.34D2 * t27 - 0.47D2 * t33 - 0.34D2 *
     # t1 - 0.59D2 * t42 - 0.47D2 * t46 - 0.34D2 * t54 - 0.136D3 * t58 -
     # 0.94D2 * t62 - 0.136D3 * t65 + 0.68D2 * t70 + t122) - wd * (-0.80
     #D2 * t95 + 0.80D2 * t58 + 0.80D2 * t106 + 0.40D2 * t76 + 0.52D2 * 
     #t62 + 0.80D2 * t110 - 0.80D2 * t84 + 0.80D2 * t65 - 0.64D2 * t120 
     #- 0.40D2 * t79 + t147)) * nf / s / z / 0.3141592653589793D1 / 0.12
     #D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh91J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t6 = 0.1D1 - x1
      t9 = z + t2 * x1
      t10 = 0.1D1 / t9
      t12 = t4 * x2 * x1 * t6 * t10
      t14 = t1 * t2
      t15 = 0.1D1 - x3
      t16 = t6 * t15
      t17 = t14 * t16
      t20 = t1 * t3 * t2
      t21 = x1 ** 2
      t22 = t20 * t21
      t23 = t9 ** 2
      t24 = 0.1D1 / t23
      t25 = 0.1D1 - x2
      t26 = x3 * t25
      t28 = x2 * t15
      t30 = cos(x4 * 0.3141592653589793D1)
      t34 = Sqrt(t26 * t9 * x2 * t15)
      t36 = 0.2D1 * t30 * t34
      t37 = t26 * t9 + t28 - t36
      t38 = t24 * t37
      t39 = x2 * t6
      t41 = t22 * t38 * t39
      t46 = x2 * x3
      t47 = t15 * t25 * t9 + t46 + t36
      t49 = t4 * t21 * t38 * t47
      t51 = t6 ** 2
      t54 = t4 * t51 * x3 * t15
      t58 = t22 * t24 * t47 * t39
      t60 = t4 * x1
      t61 = t10 * t47
      t62 = t6 * x3
      t64 = t60 * t61 * t62
      t66 = t14 * t62
      t68 = t20 * t51
      t69 = x1 * t10
      t71 = t68 * t28 * t69
      t74 = t14 * t69 * t37
      t77 = t4 * t6
      t79 = t10 * t37
      t81 = t77 * x3 * x1 * t79
      t84 = t60 * t79 * t16
      t87 = t14 * t69 * t47
      t89 = t21 * t24
      t90 = t47 ** 2
      t92 = t4 * t89 * t90
      t94 = t15 ** 2
      t96 = t4 * t51 * t94
      t98 = t37 ** 2
      t100 = t4 * t89 * t98
      t102 = x3 ** 2
      t104 = t4 * t51 * t102
      t106 = t3 ** 2
      t108 = x2 ** 2
      t112 = t1 * t106 * t108 * t21 * t51 * t24
      t116 = t68 * t46 * t69
      t120 = t77 * t15 * x1 * t61
      t122 = -0.80D2 * t81 - 0.80D2 * t84 - 0.40D2 * t87 + 0.20D2 * t92 
     #+ 0.26D2 * t96 + 0.20D2 * t100 + 0.26D2 * t104 + 0.38D2 * t112 + 0
     #.20D2 * t1 - 0.64D2 * t116 - 0.80D2 * t120
      t147 = -0.68D2 * t49 + 0.68D2 * t74 + 0.136D3 * t64 + 0.136D3 * t8
     #1 + 0.136D3 * t12 + 0.106D3 * t116 - 0.136D3 * t41 - 0.136D3 * t58
     # + 0.136D3 * t84 + 0.136D3 * t120 + 0.106D3 * t71
      rrgg2qqbarh91J6 = -(-wd * (-0.80D2 * t12 + 0.80D2 * t17 + 0.80D2 *
     # t41 + 0.40D2 * t49 + 0.52D2 * t54 + 0.80D2 * t58 - 0.80D2 * t64 +
     # 0.80D2 * t66 - 0.64D2 * t71 - 0.40D2 * t74 + t122) + 0.5D1 * wd *
     # (-0.34D2 * t100 - 0.47D2 * t96 - 0.34D2 * t1 - 0.59D2 * t112 - 0.
     #47D2 * t104 - 0.34D2 * t92 - 0.136D3 * t17 - 0.94D2 * t54 - 0.136D
     #3 * t66 + 0.68D2 * t87 + t147)) * nf / s / z / 0.3141592653589793D
     #1 / 0.12D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh91J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = 0.1D1 - z
      t3 = t2 ** 2
      t4 = t1 * t3
      t6 = 0.1D1 - x1
      t9 = z + t2 * x1
      t10 = 0.1D1 / t9
      t14 = t1 * t2
      t15 = 0.1D1 - x3
      t16 = t6 * t15
      t20 = t1 * t3 * t2
      t21 = x1 ** 2
      t22 = t20 * t21
      t23 = t9 ** 2
      t24 = 0.1D1 / t23
      t25 = 0.1D1 - x2
      t26 = x3 * t25
      t28 = x2 * t15
      t30 = cos(x4 * 0.3141592653589793D1)
      t34 = Sqrt(t26 * t9 * x2 * t15)
      t36 = 0.2D1 * t30 * t34
      t37 = t26 * t9 + t28 - t36
      t38 = t24 * t37
      t39 = x2 * t6
      t46 = x2 * x3
      t47 = t15 * t25 * t9 + t46 + t36
      t51 = t6 ** 2
      t60 = t4 * x1
      t61 = t10 * t47
      t62 = t6 * x3
      t68 = t20 * t51
      t69 = x1 * t10
      t77 = t4 * t6
      t79 = t10 * t37
      t89 = t21 * t24
      t90 = t47 ** 2
      t94 = t15 ** 2
      t98 = t37 ** 2
      t102 = x3 ** 2
      t106 = t3 ** 2
      t108 = x2 ** 2
      t122 = -0.80D2 * t77 * x3 * x1 * t79 - 0.80D2 * t60 * t79 * t16 - 
     #0.40D2 * t14 * t69 * t47 + 0.20D2 * t4 * t89 * t90 + 0.26D2 * t4 *
     # t51 * t94 + 0.20D2 * t4 * t89 * t98 + 0.26D2 * t4 * t51 * t102 + 
     #0.38D2 * t1 * t106 * t108 * t21 * t51 * t24 + 0.20D2 * t1 - 0.64D2
     # * t68 * t46 * t69 - 0.80D2 * t77 * t15 * x1 * t61
      rrgg2qqbarh91J7 = -0.5D1 / 0.12D2 * wd * (-0.80D2 * t4 * x2 * x1 *
     # t6 * t10 + 0.80D2 * t14 * t16 + 0.80D2 * t22 * t38 * t39 + 0.40D2
     # * t4 * t21 * t38 * t47 + 0.52D2 * t4 * t51 * x3 * t15 + 0.80D2 * 
     #t22 * t24 * t47 * t39 - 0.80D2 * t60 * t61 * t62 + 0.80D2 * t14 * 
     #t62 - 0.64D2 * t68 * t28 * t69 - 0.40D2 * t14 * t69 * t37 + t122) 
     #* nf / s / z / 0.3141592653589793D1

      end function
  
 