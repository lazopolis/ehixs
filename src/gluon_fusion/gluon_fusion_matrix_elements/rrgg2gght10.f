  
      subroutine rrgg2gght10
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2ggh101J1  
      doubleprecision rrgg2ggh101J2  
      doubleprecision rrgg2ggh101J3  
      doubleprecision rrgg2ggh101J4  
      doubleprecision rrgg2ggh101J5  
      doubleprecision rrgg2ggh101J6  
      doubleprecision rrgg2ggh101J7  
      doubleprecision rrgg2gght10s1e1  
      doubleprecision rrgg2gght10s1e0  
      doubleprecision rrgg2gght10s1em1  
      doubleprecision rrgg2gght10s1em2  
      doubleprecision rrgg2gght10s1em3  
      doubleprecision rrgg2gght10s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght10s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght10s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght10s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght10s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght10s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght10s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght10s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh101J1
      doubleprecision rrgg2ggh101J2
      doubleprecision rrgg2ggh101J3
      doubleprecision rrgg2ggh101J4
      doubleprecision rrgg2ggh101J5
      doubleprecision rrgg2ggh101J6
      doubleprecision rrgg2ggh101J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t12 = Sqrt(x2 * t7 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = t1 ** 2
      t20 = 0.3141592653589793D1 * t19
      t21 = 0.1D1 / s
      t22 = x2 * z
      t24 = (0.1D1 - x2 + t22) ** 2
      t25 = 0.1D1 / t24
      t26 = rrgg2ggh101J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t28 = x1 ** 2
      t30 = Sin(t5)
      t31 = t30 ** 2
      t32 = z ** 2
      t33 = 0.1D1 / t32
      t39 = log(0.4D1 * t3 * t28 * t31 * t33 * t7 * t9)
      t41 = rrgg2ggh101J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t47 = 0.3141592653589793D1 * lh
      t49 = t21 * t25
      t50 = t49 * t41
      t54 = 0.1D1 / x1
      t57 = rrgg2ggh101J3(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t67 = log(0.4D1 * t3 * t31 * t33 * t7 * t9)
      t68 = t67 * 0.3141592653589793D1
      t77 = t67 ** 2
      t80 = lh ** 2
      t82 = 0.3141592653589793D1 ** 2
      t90 = (-0.90D2 * t20 * t21 * (t25 * t26 - t39 * t25 * t41) + 0.180
     #D3 * t47 * t19 * t50) * t54 / 0.720D3 - t20 * t49 * t57 / 0.16D2 +
     # (0.180D3 * t47 + 0.90D2 * t68) * t19 * t49 * t26 / 0.1440D4 + (-0
     #.180D3 * t68 * lh - 0.45D2 * t77 * 0.3141592653589793D1 + 0.314159
     #2653589793D1 * (-0.180D3 * t80 + 0.30D2 * t82)) * t19 * t50 / 0.14
     #40D4
      t91 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, t90)
      t93 = x3 * x1
      t94 = t2 * t93
      t95 = -0.1D1 + x1
      t96 = t93 * z
      t97 = t93 * x2
      t98 = t93 * t22
      t100 = x1 * z
      t101 = 0.1D1 - x1 + t100
      t105 = Sqrt(x3 * t7 * t101 * x2 * t9)
      t107 = 0.2D1 * t6 * t105
      t110 = 0.1D1 / t101
      t112 = t2 * t95 * (-x3 + t93 - t96 + t4 - t97 + t98 - x2 + t107) *
     # t110
      t115 = t9 * s * t1 * x1
      t116 = x1 * x2
      t117 = t116 * z
      t118 = 0.1D1 - x1 + t100 - x2 + t116 - t117 - x3 + t93 - t96 + t4 
     #- t97 + t98 + t107
      t121 = t2 * t95 * t118 * t110
      t126 = s * t19 * x2 * x1 * t95 * t110
      t127 = t101 * t95
      t129 = (-0.1D1 + x1 - t100 + x2 - t116 - t22 + t117) ** 2
      t130 = 0.1D1 / t129
      t131 = -t95
      t132 = rrgg2ggh101J2(s, XB1, XB2, z, lh, wd, nf, t131, x2, x3, x4)
      t138 = t95 ** 2
      t144 = log(0.4D1 * t3 * t28 * t31 * t33 * t110 * t138 * t7 * t9)
      t147 = rrgg2ggh101J1(s, XB1, XB2, z, lh, wd, nf, t131, x2, x3, x4)
      t160 = -0.90D2 * t20 * t21 * (t127 * t130 * t132 - t144 * t101 * t
     #95 * t130 * t147) + 0.180D3 * t47 * t19 * t21 * t127 * t130 * t147
      t162 = t160 * t54 / 0.720D3
      t163 = FJET(XB1, XB2, s, t94, t112, -t115, -t121, -t126, t162)
      t167 = FJET(XB1, XB2, s, t112, t94, -t121, -t115, -t126, t162)
      t171 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, t90)
      rrgg2gght10s1e1 = t91 * t90 + t163 * t160 * t54 / 0.720D3 + t167 *
     # t160 * t54 / 0.720D3 + t171 * t90

      end function



      doubleprecision function rrgg2gght10s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh101J1
      doubleprecision rrgg2ggh101J2
      doubleprecision rrgg2ggh101J3
      doubleprecision rrgg2ggh101J4
      doubleprecision rrgg2ggh101J5
      doubleprecision rrgg2ggh101J6
      doubleprecision rrgg2ggh101J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x2 * x3
      t4 = 0.2D1 * t3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t9 = -0.1D1 + x3
      t12 = Sqrt(x2 * t7 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = t1 ** 2
      t20 = 0.3141592653589793D1 * t19
      t21 = 0.1D1 / s
      t23 = x2 * z
      t25 = (0.1D1 - x2 + t23) ** 2
      t26 = 0.1D1 / t25
      t27 = rrgg2ggh101J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t29 = 0.1D1 / x1
      t33 = t21 * t26
      t34 = rrgg2ggh101J2(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t40 = Sin(t5)
      t41 = t40 ** 2
      t43 = z ** 2
      t49 = log(0.4D1 * t3 * t41 / t43 * t7 * t9)
      t57 = -t20 * t21 * t26 * t27 * t29 / 0.8D1 - t20 * t33 * t34 / 0.1
     #6D2 + (0.180D3 * 0.3141592653589793D1 * lh + 0.90D2 * t49 * 0.3141
     #592653589793D1) * t19 * t33 * t27 / 0.1440D4
      t58 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, t57)
      t60 = x3 * x1
      t61 = t2 * t60
      t62 = -0.1D1 + x1
      t63 = t60 * z
      t64 = t60 * x2
      t65 = t60 * t23
      t67 = x1 * z
      t68 = 0.1D1 - x1 + t67
      t72 = Sqrt(x3 * t7 * t68 * x2 * t9)
      t74 = 0.2D1 * t6 * t72
      t77 = 0.1D1 / t68
      t79 = t2 * t62 * (-x3 + t60 - t63 + t4 - t64 + t65 - x2 + t74) * t
     #77
      t82 = t9 * s * t1 * x1
      t83 = x1 * x2
      t84 = t83 * z
      t85 = 0.1D1 - x1 + t67 - x2 + t83 - t84 - x3 + t60 - t63 + t4 - t6
     #4 + t65 + t74
      t88 = t2 * t62 * t85 * t77
      t93 = s * t19 * x2 * x1 * t62 * t77
      t97 = (-0.1D1 + x1 - t67 + x2 - t83 - t23 + t84) ** 2
      t98 = 0.1D1 / t97
      t101 = rrgg2ggh101J1(s, XB1, XB2, z, lh, wd, nf, -t62, x2, x3, x4)
      t105 = t20 * t21 * t68 * t62 * t98 * t101 * t29 / 0.8D1
      t106 = FJET(XB1, XB2, s, t61, t79, -t82, -t88, -t93, -t105)
      t108 = t19 * t21
      t113 = t68 * t62 * t98 * t101 * t29
      t116 = FJET(XB1, XB2, s, t79, t61, -t88, -t82, -t93, -t105)
      t121 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, t57)
      rrgg2gght10s1e0 = t58 * t57 - t106 * 0.3141592653589793D1 * t108 *
     # t113 / 0.8D1 - t116 * 0.3141592653589793D1 * t108 * t113 / 0.8D1 
     #+ t121 * t57

      end function



      doubleprecision function rrgg2gght10s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh101J1
      doubleprecision rrgg2ggh101J2
      doubleprecision rrgg2ggh101J3
      doubleprecision rrgg2ggh101J4
      doubleprecision rrgg2ggh101J5
      doubleprecision rrgg2ggh101J6
      doubleprecision rrgg2ggh101J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = 0.2D1 * x2 * x3
      t6 = cos(x4 * 0.3141592653589793D1)
      t12 = Sqrt(x2 * (-0.1D1 + x2) * x3 * (-0.1D1 + x3))
      t14 = 0.2D1 * t6 * t12
      t16 = t2 * (-x3 + t4 - x2 + t14)
      t18 = t2 * (0.1D1 - x2 - x3 + t4 + t14)
      t19 = t1 ** 2
      t24 = (0.1D1 - x2 + x2 * z) ** 2
      t27 = rrgg2ggh101J1(s, XB1, XB2, z, lh, wd, nf, 0.10D1, x2, x3, x4
     #)
      t28 = 0.1D1 / s / t24 * t27
      t30 = 0.3141592653589793D1 * t19 * t28 / 0.16D2
      t31 = FJET(XB1, XB2, s, 0.0D0, -t16, 0.0D0, t18, 0.0D0, -t30)
      t35 = FJET(XB1, XB2, s, -t16, 0.0D0, t18, 0.0D0, 0.0D0, -t30)
      rrgg2gght10s1em1 = -t31 * 0.3141592653589793D1 * t19 * t28 / 0.16D
     #2 - t35 * 0.3141592653589793D1 * t19 * t28 / 0.16D2

      end function



      doubleprecision function rrgg2gght10s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh101J1
      doubleprecision rrgg2ggh101J2
      doubleprecision rrgg2ggh101J3
      doubleprecision rrgg2ggh101J4
      doubleprecision rrgg2ggh101J5
      doubleprecision rrgg2ggh101J6
      doubleprecision rrgg2ggh101J7
      rrgg2gght10s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gght10s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh101J1
      doubleprecision rrgg2ggh101J2
      doubleprecision rrgg2ggh101J3
      doubleprecision rrgg2ggh101J4
      doubleprecision rrgg2ggh101J5
      doubleprecision rrgg2ggh101J6
      doubleprecision rrgg2ggh101J7
      rrgg2gght10s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght10s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh101J1
      doubleprecision rrgg2ggh101J2
      doubleprecision rrgg2ggh101J3
      doubleprecision rrgg2ggh101J4
      doubleprecision rrgg2ggh101J5
      doubleprecision rrgg2ggh101J6
      doubleprecision rrgg2ggh101J7
      rrgg2gght10s1em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh101J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2ggh101J1 = 0.81D2 / 0.2D1 * s * z * wd / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh101J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2ggh101J2 = 0.0D0

      end function
  
   
 

      doubleprecision function rrgg2ggh101J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2ggh101J3 = 0.0D0

      end function
  
   
 

      doubleprecision function rrgg2ggh101J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2ggh101J4 = 0.0D0

      end function
  
   
 

      doubleprecision function rrgg2ggh101J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2ggh101J5 = 0.0D0

      end function
  
   
 

      doubleprecision function rrgg2ggh101J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2ggh101J6 = -0.243D3 * s * z * wd / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh101J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      rrgg2ggh101J7 = 0.405D3 / 0.2D1 * s * z * wd / 0.3141592653589793D
     #1

      end function
  
 