  
      subroutine qbqbH1n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision qbqbH11J1  
      doubleprecision qbqbH11J2  
      doubleprecision qbqbH1n1e1  
      doubleprecision qbqbH1n1e0  
      doubleprecision qbqbH1n1em1  
      doubleprecision qbqbH1n1em2  
      doubleprecision qbqbH1n1em3  
      doubleprecision qbqbH1n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=qbqbH1n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=qbqbH1n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=qbqbH1n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=qbqbH1n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=qbqbH1n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=qbqbH1n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function qbqbH1n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH11J1
      doubleprecision qbqbH11J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x3 * z
      t4 = x3 * x1
      t5 = t4 * z
      t6 = x2 * x3
      t7 = t6 * z
      t8 = t4 * x2
      t9 = x2 * z
      t10 = t4 * t9
      t11 = x4 * 0.3141592653589793D1
      t12 = cos(t11)
      t14 = x3 * (-0.1D1 + x2)
      t15 = x1 * z
      t16 = -z - x1 + t15
      t18 = -0.1D1 + x3
      t21 = Sqrt(-t14 * t16 * x2 * t18)
      t23 = 0.2D1 * t12 * t21
      t26 = 0.1D1 / t16
      t29 = -0.1D1 + x1
      t32 = x1 * x2
      t34 = z + x1 - t15 - t9 - t32 + t32 * z - t3 - t4 + t5 + t7 + t8 -
     # t10 + t6 + t23
      t40 = t1 ** 2
      t46 = t40 * t1
      t48 = qbqbH11J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t54 = Sin(t11)
      t55 = t54 ** 2
      t57 = z ** 2
      t59 = x1 ** 2
      t62 = t29 ** 2
      t68 = log(-0.4D1 * t26 * t55 / t57 * t59 * t62 * x2 * t14 * t18)
      t73 = qbqbH11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t77 = t26 * t46 * t29 * t48 / 0.32D2 + (-0.180D3 * t26 * lh - 0.90
     #D2 * t68 * t26) * t46 * t29 * t73 / 0.2880D4
      t78 = FJET(XB1, XB2, s, t2 * x1 * (-t3 - t4 + t5 + t7 + t8 - t10 -
     # x2 + t6 + t23) * t26, -t2 * t29 * x3, -t2 * x1 * t34 * t26, t2 * 
     #t29 * t18, s * t40 * x2 * x1 * t29 * t26, t77)
      qbqbH1n1e1 = t78 * t77

      end function



      doubleprecision function qbqbH1n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH11J1
      doubleprecision qbqbH11J2
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x3 * z
      t4 = x3 * x1
      t5 = t4 * z
      t6 = x2 * x3
      t7 = t6 * z
      t8 = t4 * x2
      t9 = x2 * z
      t10 = t4 * t9
      t12 = cos(x4 * 0.3141592653589793D1)
      t15 = x1 * z
      t16 = -z - x1 + t15
      t18 = -0.1D1 + x3
      t21 = Sqrt(-x3 * (-0.1D1 + x2) * t16 * x2 * t18)
      t23 = 0.2D1 * t12 * t21
      t26 = 0.1D1 / t16
      t29 = -0.1D1 + x1
      t32 = x1 * x2
      t34 = z + x1 - t15 - t9 - t32 + t32 * z - t3 - t4 + t5 + t7 + t8 -
     # t10 + t6 + t23
      t40 = t1 ** 2
      t46 = t40 * t1
      t48 = qbqbH11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t52 = FJET(XB1, XB2, s, t2 * x1 * (-t3 - t4 + t5 + t7 + t8 - t10 -
     # x2 + t6 + t23) * t26, -t2 * t29 * x3, -t2 * x1 * t34 * t26, t2 * 
     #t29 * t18, s * t40 * x2 * x1 * t29 * t26, t26 * t46 * t29 * t48 / 
     #0.32D2)
      qbqbH1n1e0 = t52 * t26 * t46 * t29 * t48 / 0.32D2

      end function



      doubleprecision function qbqbH1n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH11J1
      doubleprecision qbqbH11J2
      qbqbH1n1em1 = 0.0D0

      end function



      doubleprecision function qbqbH1n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH11J1
      doubleprecision qbqbH11J2
      qbqbH1n1em2 = 0.0D0

      end function



      doubleprecision function qbqbH1n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH11J1
      doubleprecision qbqbH11J2
      qbqbH1n1em3 = 0.0D0

      end function



      doubleprecision function qbqbH1n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qbqbH11J1
      doubleprecision qbqbH11J2
      qbqbH1n1em4 = 0.0D0

      end function
  
 

      doubleprecision function qbqbH11J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t5 = 0.1D1 / t4
      t8 = x3 * (0.1D1 - x2)
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t23 = 0.1D1 - x1
      t26 = s - t2 * x1 * t5 * (t8 * t4 + x2 * t10 - 0.2D1 * t13 * t17) 
     #- t2 * t23 * x3
      t27 = t26 * s
      t31 = t26 ** 2
      t37 = s ** 2
      t40 = t1 ** 2
      qbqbH11J1 = -0.16D2 * wd * (-0.2D1 * t27 * z - t27 * t3 - t31 + 0.
     #2D1 * t27 + 0.2D1 * t27 * t1 * t23 * x3 - t37 * z * t3 - 0.4D1 * t
     #27 * t40 * x2 * x1 * t23 * t5) / t31

      end function
  
   
 

      doubleprecision function qbqbH11J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = 0.1D1 - z
      t2 = s * t1
      t3 = x1 * t1
      t4 = z + t3
      t8 = x3 * (0.1D1 - x2)
      t10 = 0.1D1 - x3
      t13 = cos(x4 * 0.3141592653589793D1)
      t17 = Sqrt(t8 * t4 * x2 * t10)
      t26 = s - t2 * x1 / t4 * (t8 * t4 + x2 * t10 - 0.2D1 * t13 * t17) 
     #- t2 * (0.1D1 - x1) * x3
      t27 = t26 ** 2
      t28 = t26 * s
      t32 = s ** 2
      qbqbH11J2 = -0.16D2 * wd * (t27 - t28 * t3 - 0.2D1 * t28 * z + t32
     # * z * t3) / t27

      end function
  
 