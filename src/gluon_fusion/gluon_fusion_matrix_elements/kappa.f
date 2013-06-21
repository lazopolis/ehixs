ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c       My kappa function's:
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

c       The kappa function:
	doubleprecision function kappa(x1,x2,x3,x4,z)
	implicit none
	doubleprecision x1
	doubleprecision x2
	doubleprecision x3
	doubleprecision x4
	doubleprecision z,x34,x

	if(x3.eq.0.d0)then
	   x34=x4
	else if(x4.eq.0.d0)then
	   x34=x3	   
	else if(x3.eq.1.d0)then
	   x34=1.d0-x4
	else if(x4.eq.1.d0)then
	   x34=1.d0-x3
	else
	   x34=(x3-x4)**2/(x3+x4-2.d0*x3*x4
     &+2.d0*dcos(x2*0.3141592653589793D1)
     &*dsqrt(x3*x4*(1.d0-x3)*(1.d0-x4)))
	end if

	x=x1*(1.d0-x1)*(1.d0-z)*x34

	if(x.lt.0.00001d0)then
	   kappa=1.d0+x+2.d0*x**2+5.d0*x**3+14.d0*x**4
	   kappa=kappa+42.d0*x**5+132.d0*x**6
	else
	   kappa = (1d0-dsqrt(1d0-4d0*x))/(2d0*x)
	end if

	if(kappa.gt.2.d0.or.kappa.lt.1.d0)then
	   write(6,*)"!!!!!!!!!!!!!!!!!!!¨kappa!!!!!!!!!!!!!!!!!"
	   write(6,*)"kappa=",kappa
	   write(6,*)"!!!!!!!!!!!!!!!!!!!¨kappa!!!!!!!!!!!!!!!!!"
	end if

	return
	end


c**********************************************************************

	
c       The 2nd kappa function:
	doubleprecision function kappa2(x1,x2,x3,x4,z)
	implicit none
	doubleprecision x1
	doubleprecision x2
	doubleprecision x3
	doubleprecision x4
	doubleprecision z,x34,x
	
	x34=x3+x4-2.d0*x3*x4
     &-2.d0*dcos(x2*0.3141592653589793D1)
     &*dsqrt(x3*x4*(1.d0-x3)*(1.d0-x4))

	x=x1*(1.d0-x1)*(1.d0-z)*x34

	if(x.lt.0.00001d0)then
	   kappa2=1.d0+x+2.d0*x**2+5.d0*x**3+14.d0*x**4
	   kappa2=kappa2+42.d0*x**5+132.d0*x**6
	else
	   kappa2 = (1d0-dsqrt(1d0-4d0*x))/(2d0*x)
	end if

	if(kappa2.gt.2.d0.or.kappa2.lt.1.d0)then
	   write(6,*)"!!!!!!!!!!!!!!!!!!!¨kappa!!!!!!!!!!!!!!!!!"
	   write(6,*)"kappa=",kappa2,x
	   write(6,*)"!!!!!!!!!!!!!!!!!!!¨kappa!!!!!!!!!!!!!!!!!"
	end if

	return
	end
	
c**********************************************************************

c       The actual kappa function:
	doubleprecision function kappaf(x)
	implicit none
	doubleprecision x
	
	if(x.lt.0.001d0)then
	   kappaf=1.d0+x+2.d0*x**2+5.d0*x**3+14.d0*x**4
     &+42.d0*x**5+132.d0*x**6
	else
	   kappaf = (1d0-dsqrt(1d0-4d0*x))/(2d0*x)
	end if

	end function
