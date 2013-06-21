  
      subroutine qbqbH4n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision qbqbH41J1  
      doubleprecision qbqbH41J2  
      doubleprecision qbqbH4n1e1  
      doubleprecision qbqbH4n1e0  
      doubleprecision qbqbH4n1em1  
      doubleprecision qbqbH4n1em2  
      doubleprecision qbqbH4n1em3  
      doubleprecision qbqbH4n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=qbqbH4n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=qbqbH4n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=qbqbH4n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=qbqbH4n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=qbqbH4n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=qbqbH4n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function qbqbH4n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH41J1
      doubleprecision qbqbH41J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = 0.2D1 * x2 * x3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t8 = x2 * t7
      t9 = -0.1D1 + x3
      t12 = Sqrt(t8 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t19 = t1 ** 2
      t20 = 0.1D1 / s
      t21 = t19 * t20
      t22 = x2 * z
      t24 = (0.1D1 - x2 + t22) ** 2
      t25 = 0.1D1 / t24
      t26 = qbqbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t28 = x1 ** 2
      t29 = x3 * t28
      t30 = Sin(t5)
      t31 = t30 ** 2
      t33 = z ** 2
      t34 = 0.1D1 / t33
      t40 = log(0.4D1 * t29 * t31 * t34 * x2 * t7 * t9)
      t42 = qbqbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t47 = lh * t19
      t48 = t20 * t25
      t49 = t48 * t42
      t53 = 0.1D1 / x1
      t62 = log(0.4D1 * x3 * t31 * t34 * t8 * t9)
      t71 = lh ** 2
      t73 = 0.3141592653589793D1 ** 2
      t75 = t62 ** 2
      t81 = -(-0.90D2 * t21 * (-t25 * t26 + t40 * t25 * t42) - 0.180D3 *
     # t47 * t49) * t53 / 0.2880D4 + (0.180D3 * lh + 0.90D2 * t62) * t19
     # * t48 * t26 / 0.5760D4 + (-0.180D3 * t62 * lh - 0.180D3 * t71 + 0
     #.30D2 * t73 - 0.45D2 * t75) * t19 * t49 / 0.5760D4
      t82 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t4 - x2 + t14), 0.0D0,
     # t2 * (0.1D1 - x2 - x3 + t4 + t14), 0.0D0, t81)
      t84 = x3 * x1
      t86 = -0.1D1 + x1
      t87 = t84 * z
      t88 = t84 * x2
      t89 = t84 * t22
      t91 = x1 * z
      t92 = 0.1D1 - x1 + t91
      t96 = Sqrt(x3 * t7 * t92 * x2 * t9)
      t98 = 0.2D1 * t6 * t96
      t101 = 0.1D1 / t92
      t106 = x1 * x2
      t107 = t106 * z
      t108 = 0.1D1 - x1 + t91 - x2 + t106 - t107 - x3 + t84 - t87 + t4 -
     # t88 + t89 + t98
      t117 = t92 * t86
      t119 = (-0.1D1 + x1 - t91 + x2 - t106 - t22 + t107) ** 2
      t120 = 0.1D1 / t119
      t121 = -t86
      t122 = qbqbH41J2(s, XB1, XB2, z, lh, wd, t121, x2, x3, x4)
      t127 = t86 ** 2
      t133 = log(0.4D1 * t29 * t31 * t34 * t8 * t101 * t127 * t9)
      t136 = qbqbH41J1(s, XB1, XB2, z, lh, wd, t121, x2, x3, x4)
      t147 = 0.90D2 * t21 * (t117 * t120 * t122 - t133 * t92 * t86 * t12
     #0 * t136) - 0.180D3 * t47 * t20 * t117 * t120 * t136
      t150 = FJET(XB1, XB2, s, t2 * t84, t2 * t86 * (-x3 + t84 - t87 + t
     #4 - t88 + t89 - x2 + t98) * t101, -t2 * x1 * t9, -t2 * t86 * t108 
     #* t101, -s * t19 * x2 * x1 * t86 * t101, -t147 * t53 / 0.2880D4)
      qbqbH4n1e1 = t82 * t81 - t150 * t147 * t53 / 0.2880D4

      end function



      doubleprecision function qbqbH4n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH41J1
      doubleprecision qbqbH41J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = 0.2D1 * x2 * x3
      t5 = x4 * 0.3141592653589793D1
      t6 = cos(t5)
      t7 = -0.1D1 + x2
      t8 = x2 * t7
      t9 = -0.1D1 + x3
      t12 = Sqrt(t8 * x3 * t9)
      t14 = 0.2D1 * t6 * t12
      t19 = t1 ** 2
      t20 = 0.1D1 / s
      t21 = t19 * t20
      t22 = x2 * z
      t24 = (0.1D1 - x2 + t22) ** 2
      t25 = 0.1D1 / t24
      t26 = qbqbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t28 = 0.1D1 / x1
      t32 = qbqbH41J2(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t37 = Sin(t5)
      t38 = t37 ** 2
      t40 = z ** 2
      t46 = log(0.4D1 * x3 * t38 / t40 * t8 * t9)
      t54 = -t21 * t25 * t26 * t28 / 0.32D2 - t21 * t25 * t32 / 0.64D2 +
     # (0.180D3 * lh + 0.90D2 * t46) * t19 * t20 * t25 * t26 / 0.5760D4
      t55 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t4 - x2 + t14), 0.0D0,
     # t2 * (0.1D1 - x2 - x3 + t4 + t14), 0.0D0, t54)
      t57 = x3 * x1
      t59 = -0.1D1 + x1
      t60 = t57 * z
      t61 = t57 * x2
      t62 = t57 * t22
      t64 = x1 * z
      t65 = 0.1D1 - x1 + t64
      t69 = Sqrt(x3 * t7 * t65 * x2 * t9)
      t71 = 0.2D1 * t6 * t69
      t74 = 0.1D1 / t65
      t79 = x1 * x2
      t80 = t79 * z
      t81 = 0.1D1 - x1 + t64 - x2 + t79 - t80 - x3 + t57 - t60 + t4 - t6
     #1 + t62 + t71
      t92 = (-0.1D1 + x1 - t64 + x2 - t79 - t22 + t80) ** 2
      t96 = qbqbH41J1(s, XB1, XB2, z, lh, wd, -t59, x2, x3, x4)
      t98 = t59 / t92 * t96 * t28
      t101 = FJET(XB1, XB2, s, t2 * t57, t2 * t59 * (-x3 + t57 - t60 + t
     #4 - t61 + t62 - x2 + t71) * t74, -t2 * x1 * t9, -t2 * t59 * t81 * 
     #t74, -s * t19 * x2 * x1 * t59 * t74, -t21 * t65 * t98 / 0.32D2)
      qbqbH4n1e0 = t55 * t54 - t101 * t19 * t20 * t65 * t98 / 0.32D2

      end function



      doubleprecision function qbqbH4n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH41J1
      doubleprecision qbqbH41J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t4 = 0.2D1 * x2 * x3
      t6 = cos(x4 * 0.3141592653589793D1)
      t12 = Sqrt(x2 * (-0.1D1 + x2) * x3 * (-0.1D1 + x3))
      t14 = 0.2D1 * t6 * t12
      t19 = t1 ** 2
      t20 = 0.1D1 / s
      t24 = (0.1D1 - x2 + x2 * z) ** 2
      t25 = 0.1D1 / t24
      t26 = qbqbH41J1(s, XB1, XB2, z, lh, wd, 0.10D1, x2, x3, x4)
      t30 = FJET(XB1, XB2, s, 0.0D0, -t2 * (-x3 + t4 - x2 + t14), 0.0D0,
     # t2 * (0.1D1 - x2 - x3 + t4 + t14), 0.0D0, -t19 * t20 * t25 * t26 
     #/ 0.64D2)
      qbqbH4n1em1 = -t30 * t19 * t20 * t25 * t26 / 0.64D2

      end function



      doubleprecision function qbqbH4n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH41J1
      doubleprecision qbqbH41J2
      qbqbH4n1em2 = 0.0D0

      end function



      doubleprecision function qbqbH4n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH41J1
      doubleprecision qbqbH41J2
      qbqbH4n1em3 = 0.0D0

      end function



      doubleprecision function qbqbH4n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH41J1
      doubleprecision qbqbH41J2
      qbqbH4n1em4 = 0.0D0

      end function
  
 

      doubleprecision function qbqbH41J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      qbqbH41J1 = -0.16D2 * s * z * wd

      end function
  
   
 

      doubleprecision function qbqbH41J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      qbqbH41J2 = 0.16D2 * s * z * wd

      end function
  
 