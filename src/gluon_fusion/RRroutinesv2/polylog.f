
      

      
cccccccccccccccccccccccccccccccccccccccccccccccccc
c     My polylog chaplin wrapper:                c
cccccccccccccccccccccccccccccccccccccccccccccccccc
      doubleprecision function polylog(n,x)
      implicit none
      doubleprecision x,ReLi2
      integer n
      doublecomplex wgplg,HPL2,HPL3,HPL4,y,dilog,trilog
	  doublecomplex polylog2,polylogdiff
      y=dcmplx(x,0d0)     

      if(n.lt.2)then
         write(6,*)"poylog: fuck off i am not doing logs here"         
      else if(n.eq.2)then
         !polylog=dreal(wgplg(1,1,y))  
		 polylog2 = HPL2(0,1,y)
      else if(n.eq.3)then
         !polylog=dreal(wgplg(2,1,y))
		 polylog2 = HPL3(0,0,1,y)
      else if(n.eq.4)then
         !polylog=dreal(wgplg(3,1,y))
		 polylog2 = HPL4(0,0,0,1,y)
      else
         write(6,*)"only do up to weight 4, bitch!"
      end if
	  polylog=polylog2
      return
      
      end function


      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     Stefan's dilog implementation for complex argument 
C     analytically continued to the entire complex plane
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C =============================    
CCC   Real part of Li2 of argument z=x_r+i*x_i
      double precision function ReLi2(x_r,x_i)
      implicit none
      double precision x_r,x_i
      double complex z
      double complex myLi2
      external myLi2

      z=dcmplx(x_r,x_i)
      ReLi2=dreal(myLi2(z))

      return
      end

C =============================    
CCC   Real part of Li2 of argument z=x_r+i*x_i
      double precision function ImLi2(x_r,x_i)
      implicit none
      double precision x_r,x_i
      double complex z
      double complex myLi2
      external myLi2

      z=dcmplx(x_r,x_i)
      ImLi2=dimag(myLi2(z))

      return
      end


C =============================     
C     Daleo's Li2 functions, modified by S. Bucherer
CCC   z is complex
CCC   ATTENTION: might be unstable close to real(z) ~ 0.5 -> to be checked
      double complex function myLi2(z)
      implicit none
      double complex z,zz,zsig,zxn,zxni

      double precision PI
      parameter( PI = 3.14159265358979323846264d0 )

      double complex myLi2s
      external myLi2s

      zsig=dcmplx(1d0,0d0)

      myLi2=dcmplx(0d0,0d0)
      if ((dreal(z).eq.1d0).and.(dimag(z).eq.0d0)) then
         myLi2=PI**2/6d0
         goto 210
      elseif ((dreal(z).eq.-1d0).and.(dimag(z).eq.0d0)) then
         myLi2=-PI**2/12d0
         goto 210
      elseif (dreal(z).gt.1d0) then
         myLi2=PI**2/3d0-1d0/2d0*zlog(z)**2
         zz=1d0/z
         zsig=dcmplx(-1d0,0d0)
         if (dimag(z).gt.0d0) then
            myLi2=myLi2+dcmplx(0d0,1d0)*PI*zlog(z)
         else
            myLi2=myLi2-dcmplx(0d0,1d0)*PI*zlog(z)
         endif
      elseif ((zabs(z).gt.1d0)) then
         myLi2=-PI**2/6d0-1d0/2d0*zlog(-z)**2
         zz=1d0/z
         zsig=dcmplx(-1d0,0d0)
      else
         zz=z
         zsig=dcmplx(1d0,0d0)
      endif

      if(dreal(zz).gt.0.5d0) then
         myLi2=myLi2+zsig*(PI**2/6d0-zlog(zz)*zlog(1d0-zz))
         zz=1d0-zz
         zsig=dcmplx(-1d0,0d0)*zsig
      endif

      myLi2=myLi2+zsig*myLi2s(zz)
     
 210  continue

      return
      end


C =============================     
       double complex function myLi2s(z)
      implicit none
      double complex z,zxn,zz

      double precision coeff(38)
      integer i,nmax
      parameter(nmax=38)

      DATA coeff / 
     .  1d0,
     .  -0.25d0,
     .  0.0277777777777777777777778d0,
     .  0d0,
     .  -0.000277777777777777777777778d0,
     .  0d0,
     .  4.72411186696900982615268d-6,
     .  0d0,
     .  -9.18577307466196355085244d-8,
     .  0d0,
     .  1.89788699889709990720092d-9,
     .  0d0,
     .  -4.06476164514422552680591d-11,
     .  0d0,
     .  8.92169102045645255521799d-13,
     .  0d0,
     .  -1.99392958607210756872364d-14,
     .  0d0,
     .  4.51898002961991819165048d-16,
     .  0d0,
     .  -1.03565176121812470144834d-17,
     .  0d0,
     .  2.39521862102618674574028d-19,
     .  0d0,
     .  -5.58178587432500933628307d-21,
     .  0d0,
     .  1.30915075541832128581231d-22,
     .  0d0,
     .  -3.08741980242674029324228d-24,
     .  0d0,
     .  7.31597565270220342035791d-26,
     .  0d0,
     .  -1.74084565723400074098906d-27,
     .  0d0,
     .  4.1576356446138997196179d-29,
     .  0d0,
     .  -9.96214848828462210319401d-31,
     .  0d0/

  
      zxn=-zlog(1d0-z)
      myLi2s=coeff(nmax)
      do i=nmax-1,1,-1
         myLi2s=zxn*myLi2s+coeff(i)
      enddo
      myLi2s=zxn*myLi2s
      return
      end
