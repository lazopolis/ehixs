	

c=======================================================================	
c
c			form factors
c
c
	double complex function FA_higgs(s,t,u,mhsq)
	!code generated from FA_higgs.mw maple file 
	!at ihiggs_analytic_info
	implicit none
	double complex andre(0:2)
	double complex c0,c1,c2,c3,c4,c10,c11,c12,c13,c14
	double complex box6,box4,bubmh,bubEs,bubEt,tadmt,tadmw
	double complex triAs,triAt,triaEs,triaEt
	double complex mhsq
	double complex zero,Delta
	double precision s,t,u
	double complex cs,ct,cu,cmhsq
	
	!print*,"[FA_higgs] start"
	
	cs=dcmplx(s,0d0)
	ct=dcmplx(t,0d0)
	cu=dcmplx(u,0d0)
	cmhsq = cs+ct+cu
	
	zero = dcmplx(0d0,0d0)
	
	call avh_olo_d0c(andre,zero,zero,zero,cmhsq,
     #			cs,ct,
     #			mhsq,zero,zero,mhsq)
	!print*,"[FA_higgs]",cs,ct,cu,mhsq,cmhsq
	box4=andre(0)
	call avh_olo_c0c(andre,zero,cmhsq,cs,zero,mhsq,mhsq)
	triaEs=andre(0)
	call avh_olo_c0c(andre,ct,cmhsq,zero,zero,mhsq,mhsq)
	triaEt=andre(0)
	
	! triA
	call avh_olo_c0c(andre,zero,cs,zero,zero,zero,mhsq)
	triAs=andre(0)
	call avh_olo_c0c(andre,zero,zero,ct,zero,zero,mhsq)
	triAt=andre(0)
	
	call avh_olo_b0c(andre,cmhsq,mhsq,mhsq)
	bubmh=andre(0)
	call avh_olo_b0c(andre,ct,zero,mhsq)
	bubEt=andre(0)
	
	Delta = s*mhsq+t*mhsq-s*t
	
	box6= -Delta ** 2 / s / t / u * box4 / 0.2D1 
     #- Delta / t / u * triAs / 0.2D1 
     #- Delta / s / u * triAt / 0.2D1 
     #+ (t ** 2 * mhsq - s * t
     # ** 2 + t * u * mhsq - s * t * u + s * t * mhsq - s * u * mhsq) / 
     #s / t / u * triaEs / 0.2D1 
     #+ (s ** 2 * mhsq - t * s ** 2 + s * t * mhsq + s * u * mhsq 
     #	- s * t * u - t * u * mhsq) / s / t / u 
     #* triaEt / 0.2D1
 
	
	FA_higgs = -0.2D1 * t * mhsq / Delta / s * box6 
     #+ 0.2D1 / s / (u + s) * bubEt 
     #- 0.2D1 * mhsq ** 2 / s / Delta * triaEs 
     #+ 0.2D1 * (-t * u +mhsq * u + 0.2D1 * s * mhsq 
     #	+ t * mhsq - 0.2D1 * s * t) / s * mhsq / (u + s) / Delta 
     #	* triaEt 
     #- 0.2D1 / s * bubmh / (u + s)
	
	
	!print*,"[FA_higgs] ends: ",FA_higgs
	
	end
c=======================================================================	
	double complex function FA_massless(s,t,u,mhsq,mzsq)
	!code generated from FA_massless.mw maple file 
	!at ihiggs_analytic_info
	implicit none
	double complex andre(0:2)
	double complex c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14
	double complex box6,box4,dm0,bubmh,bubEs,bubEt,tadmt,tadmw
	double complex triaEs,triaEt,b1s,b1t,tad1,triaEsm1,triaEtm1
	double complex mhsq,mzsq,BuBE_ep,bp2,triAs,triAt,triAsm1
	double complex triAtm1
	double complex zero,Delta, b_pole_2,b_pole_1,tm1,tad,dm1,bm1t
	double complex bm1s
	double precision s,t,u
	double complex cs,ct,cu
	!print*,"FA_massless starts ***"
	cs=dcmplx(s,0d0)
	ct=dcmplx(t,0d0)
	cu=dcmplx(u,0d0)
	!print*,"[FA_massless]",cs,ct,cu
	zero = dcmplx(0d0,0d0)
	
	call avh_olo_d0c(andre,zero,zero,mhsq,zero,
     #			cs,ct,
     #			zero,zero,mzsq,mzsq)
	!print*,"[FA_massless]",dm0
	dm0=andre(0)
	dm1 = andre(1)
	!triB
	call avh_olo_c0c(andre,zero,mhsq,cs,zero,mzsq,mzsq)
	triaEs=andre(0)
	call avh_olo_c0c(andre,zero,mhsq,ct,zero,mzsq,mzsq)
	triaEt=andre(0)
	! triA
	call avh_olo_c0c(andre,zero,cs,zero,zero,zero,mzsq)
	triAs=andre(0)
	call avh_olo_c0c(andre,zero,zero,ct,zero,zero,mzsq)
	triAt=andre(0)
	!print*,"[FA_massless]",mhsq,cs,ct,mzsq
	!bubmh
	call avh_olo_b0c(andre,mhsq,mzsq,mzsq)
	bubmh=andre(0)
	!---- we are missing bp2!
	bp2=0d0
	! BubE(s)
	!call avh_olo_b0c(andre,cs,zero,mzsq)
	!bubEs=andre(0)
	!bm1s = andre(1)
	! BubE(t)
	call avh_olo_b0c(andre,ct,zero,mzsq)
	

	bubEt=andre(0)
	bm1t = andre(1)
	call avh_olo_b0c(andre,mhsq,mzsq,mzsq)
	bubmh=andre(0)
	!Tad
	call avh_olo_a0c(andre,mzsq)
	tm1 = andre(1)
	tad=andre(0)
	tad1 = mzsq * log(mzsq) ** 2 / 0.2D1 - mzsq * log(mzsq) + mzsq
	
	Delta = (s+t)*mzsq-s*t

	

	box6= -Delta ** 2 / s / t / u * dm0 / 0.2D1 
     #- Delta / t / u * triAs / 0.2D1 
     #- Delta / s / u * triAt / 0.2D1 
     #+ (t ** 2 * mzsq - s * t
     # ** 2 + t * u * mzsq - s * t * u + s * t * mzsq - s * u * mzsq) / 
     #s / t / u * triaEs / 0.2D1 
     #+ (s ** 2 * mzsq - t * s ** 2 + s * t * mzsq + s * u * mzsq 
     #	- s * t * u - t * u * mzsq) / s / t / u 
     #* triaEt / 0.2D1
    
	!print*,"[FA_massless] implementation2"
	FA_massless = (mzsq - s) * t / Delta / s * box6 
     #- 0.1D1 / s / (u + s) * bubEt 
     #+ 0.1D1 / s / (u + s) * bubmh 
     #+ (mzsq - s) * mzsq / Delta / s * triaEs 
     #- (0.2D1 * mzsq ** 2 * s + mzsq ** 2 * u + mzsq ** 2 * t -
     # s ** 2 * mzsq - s * u * mzsq - 0.2D1 * s * t * mzsq - t * u * mzs
     #q + t * s ** 2 + s * t * u) / Delta / s / (u + s) * triaEt
	!print*,"[FA_massless] ------"
	!FA_massless = -FA_massless
	!comparison with the form factor of 0905.2775, via the mv->infty limit:
	! FA_massless = -1/4 * Fv(t,u,s,mh,mv)
	! and at the limit FA_massless -> (-1/4)(-5/9/mv^4) = 5/36/mv^4 
	end

c=======================================================================	
	double complex function FA_massive(s,t,u,mhsq,mtsq,mwsq)
	!code generated from FA_massive.mw maple file 
	!at ihiggs_analytic_info
	implicit none
	double complex andre(0:2)
	double complex c0,c1,c2,c3,c4,c5,c6,c7,c8,c9,c10,c11,c12,c13,c14
	double complex box6,box4,bubmh,bubEs,bubEt,tadmt,tadmw
	double complex triAs,triAt,triBs,triBt
	double complex mhsq,mtsq,mwsq,cmhsq
	double complex zero
	double precision s,t,u
	double complex cs,ct,cu
	
	cs=dcmplx(s,0d0)
	ct=dcmplx(t,0d0)
	cu=dcmplx(u,0d0)
      cmhsq = cs+ct+cu
	zero = dcmplx(0d0,0d0)
	
	call avh_olo_d0c(andre,zero,zero,zero,cmhsq,
     #			cs,ct,
     #			mwsq,mtsq,mtsq,mwsq)
	box4=andre(0)
	call avh_olo_c0c(andre,cs,zero,cmhsq,mwsq,mtsq,mwsq)
	triBs=andre(0)
	call avh_olo_c0c(andre,zero,ct,cmhsq,mwsq,mtsq,mwsq)
	triBt=andre(0)
	call avh_olo_c0c(andre,zero,zero,cs,mwsq,mtsq,mtsq)
	triAs=andre(0)
	call avh_olo_c0c(andre,ct,zero,zero,mwsq,mtsq,mtsq)
	triAt=andre(0)
	call avh_olo_b0c(andre,cmhsq,mwsq,mwsq)
	bubmh=andre(0)
	call avh_olo_b0c(andre,cs,mtsq,mwsq)
	bubEs=andre(0)
	call avh_olo_b0c(andre,ct,mtsq,mwsq)
	bubEt=andre(0)
	call avh_olo_a0c(andre,mwsq)
	tadmw = andre(0)
	call avh_olo_a0c(andre,mtsq)
	tadmt = andre(0)
	
	
	c0 = t * (-0.4D1 * mwsq ** 3 * s ** 2 + 0.4D1 * mwsq ** 4 * s + 0.
     #4D1 * mwsq ** 4 * t + mtsq ** 3 * s ** 2 + mtsq ** 3 * t ** 2 - mt
     #sq * s * u * mwsq * t + 0.3D1 * mtsq ** 2 * s * u * t + 0.8D1 * mt
     #sq * s * mwsq ** 2 * t - mtsq * t ** 2 * s * mwsq - mtsq * s ** 2 
     #* mwsq * t - 0.2D1 * mtsq ** 2 * s * mwsq * t - 0.4D1 * mtsq * s *
     #* 2 * u * mwsq + 0.2D1 * mtsq ** 2 * u * mwsq * s + 0.9D1 * mtsq *
     # u * mwsq ** 2 * s + mtsq * t * u * mwsq ** 2 - 0.2D1 * mtsq ** 2 
     #* t * u * mwsq + 0.2D1 * mtsq ** 3 * s * t + mtsq ** 2 * s ** 2 * 
     #t + mtsq ** 2 * t ** 2 * s + 0.2D1 * mtsq ** 2 * s ** 2 * u + mtsq
     # ** 3 * s * u + 0.2D1 * mtsq ** 2 * u ** 2 * s + mtsq ** 3 * t * u
     # - 0.8D1 * mwsq ** 3 * s * t + 0.4D1 * s ** 2 * mwsq ** 2 * t + 0.
     #5D1 * mtsq * s ** 2 * mwsq ** 2 - 0.2D1 * mtsq ** 2 * s ** 2 * mws
     #q + mtsq * t ** 2 * mwsq ** 2 - 0.2D1 * mtsq ** 2 * t ** 2 * mwsq 
     #- 0.6D1 * mtsq * mwsq ** 3 * s + 0.2D1 * mtsq ** 3 * mwsq * s - 0.
     #6D1 * mtsq * mwsq ** 3 * t + 0.2D1 * mtsq ** 3 * mwsq * t) / s / (
     #t ** 2 * mwsq ** 2 + 0.2D1 * s * t * mwsq ** 2 - 0.2D1 * s * t ** 
     #2 * mwsq + s ** 2 * mwsq ** 2 - 0.2D1 * s ** 2 * t * mwsq + s ** 2
     # * t ** 2 - 0.2D1 * mtsq * t ** 2 * mwsq - 0.4D1 * mtsq * s * t * 
     #mwsq + 0.4D1 * mtsq * s * t * u + 0.2D1 * mtsq * s * t ** 2 - 0.2D
     #1 * mtsq * s ** 2 * mwsq + 0.2D1 * mtsq * s ** 2 * t + mtsq ** 2 *
     # t ** 2 + 0.2D1 * mtsq ** 2 * s * t + mtsq ** 2 * s ** 2) / mwsq *
     #* 2 / 0.4D1

      c1 = mtsq * (-mwsq + mtsq) / s ** 2 / mwsq ** 2 / 0.4D1

      c10 = -(t ** 2 * mwsq ** 2 + 0.2D1 * s * t * mwsq ** 2 - 0.2D1 * s
     # * t ** 2 * mwsq + s ** 2 * mwsq ** 2 - 0.2D1 * s ** 2 * t * mwsq 
     #+ s ** 2 * t ** 2 - 0.2D1 * mtsq * t ** 2 * mwsq - 0.4D1 * mtsq * 
     #s * t * mwsq + 0.4D1 * mtsq * s * t * u + 0.2D1 * mtsq * s * t ** 
     #2 - 0.2D1 * mtsq * s ** 2 * mwsq + 0.2D1 * mtsq * s ** 2 * t + mts
     #q ** 2 * t ** 2 + 0.2D1 * mtsq ** 2 * s * t + mtsq ** 2 * s ** 2) 
     #/ s / t / u / 0.2D1

      c11 = (-t * mwsq - s * mwsq + s * t + mtsq * t + mtsq * s) / t / u
     # / 0.2D1

      c12 = (-t * mwsq - s * mwsq + s * t + mtsq * t + mtsq * s) / s / u
     # / 0.2D1

      c13 = -(-t ** 2 * mwsq + s * t ** 2 + mtsq * t ** 2 - t * u * mwsq
     # + mtsq * t * u + s * t * u - s * t * mwsq + mtsq * s * t - mtsq *
     # s * u + s * u * mwsq) / s / t / u / 0.2D1

      c14 = -(-s ** 2 * mwsq + t * s ** 2 + mtsq * s ** 2 - s * t * mwsq
     # + mtsq * s * u + mtsq * s * t - s * u * mwsq + s * t * u + t * u 
     #* mwsq - mtsq * t * u) / s / t / u / 0.2D1

      c2 = -(mtsq * s + 0.2D1 * mtsq * mwsq + mtsq * u + 0.4D1 * mwsq **
     # 2 + mtsq * t) / s / (u + s) / mwsq ** 2 / 0.4D1

      c3 = (mtsq * s + 0.2D1 * mtsq * mwsq + mtsq * u + 0.4D1 * mwsq ** 
     #2 + mtsq * t) / s / (u + s) / mwsq ** 2 / 0.4D1

      c4 = -mtsq / s ** 2 / mwsq ** 2 / 0.4D1

      c5 = mtsq / s ** 2 / mwsq ** 2 / 0.4D1

      c6 = (0.4D1 * mwsq ** 3 * s ** 2 - 0.2D1 * mwsq ** 2 * s ** 3 + mt
     #sq ** 3 * s ** 2 - mtsq ** 2 * s ** 3 + mtsq ** 3 * t ** 2 - mtsq 
     #* s * u * mwsq * t - mtsq * s ** 2 * u * t + 0.5D1 * mtsq ** 2 * s
     # * u * t + 0.4D1 * mtsq * s * mwsq ** 2 * t - 0.3D1 * mtsq * t ** 
     #2 * s * mwsq - 0.2D1 * mtsq * s ** 2 * mwsq * t - 0.2D1 * mtsq ** 
     #2 * s * mwsq * t + mtsq * s ** 2 * u * mwsq - mtsq * s ** 3 * t + 
     #0.2D1 * mtsq ** 3 * s * t + 0.2D1 * mtsq ** 2 * s ** 2 * t + 0.3D1
     # * mtsq ** 2 * t ** 2 * s - mtsq ** 2 * s ** 2 * u - 0.4D1 * mwsq 
     #** 3 * s * t + 0.2D1 * s ** 3 * t * mwsq + 0.2D1 * s ** 2 * mwsq *
     #* 2 * t - mtsq * s ** 2 * mwsq ** 2 - 0.4D1 * mtsq ** 2 * s ** 2 *
     # mwsq + 0.3D1 * mtsq * s ** 3 * mwsq + mtsq * t ** 2 * mwsq ** 2 -
     # 0.2D1 * mtsq ** 2 * t ** 2 * mwsq) * mtsq / s / (t ** 2 * mwsq **
     # 2 + 0.2D1 * s * t * mwsq ** 2 - 0.2D1 * s * t ** 2 * mwsq + s ** 
     #2 * mwsq ** 2 - 0.2D1 * s ** 2 * t * mwsq + s ** 2 * t ** 2 - 0.2D
     #1 * mtsq * t ** 2 * mwsq - 0.4D1 * mtsq * s * t * mwsq + 0.4D1 * m
     #tsq * s * t * u + 0.2D1 * mtsq * s * t ** 2 - 0.2D1 * mtsq * s ** 
     #2 * mwsq + 0.2D1 * mtsq * s ** 2 * t + mtsq ** 2 * t ** 2 + 0.2D1 
     #* mtsq ** 2 * s * t + mtsq ** 2 * s ** 2) / mwsq ** 2 / 0.4D1

      c7 = -t * mtsq / s * (-0.2D1 * s * t * mwsq ** 2 - 0.3D1 * mtsq * 
     #s ** 2 * mwsq + mtsq * s * t ** 2 + mtsq * t ** 2 * mwsq - 0.2D1 *
     # s ** 2 * t * mwsq + mtsq ** 2 * s ** 2 + mtsq * s ** 2 * t + 0.2D
     #1 * mwsq * mtsq ** 2 * s + 0.2D1 * mwsq ** 2 * mtsq * s - mtsq ** 
     #2 * t ** 2 + 0.2D1 * s ** 2 * mwsq ** 2 + mtsq * s * t * u + t * m
     #tsq * mwsq * u - u * mtsq * mwsq * s - 0.2D1 * t * mwsq ** 2 * mts
     #q - 0.2D1 * t * mwsq * mtsq ** 2 + 0.4D1 * t * mwsq ** 3 - 0.4D1 *
     # mwsq ** 3 * s - mtsq ** 2 * t * u + mtsq ** 2 * u * s) / (t ** 2 
     #* mwsq ** 2 + 0.2D1 * s * t * mwsq ** 2 - 0.2D1 * s * t ** 2 * mws
     #q + s ** 2 * mwsq ** 2 - 0.2D1 * s ** 2 * t * mwsq + s ** 2 * t **
     # 2 - 0.2D1 * mtsq * t ** 2 * mwsq - 0.4D1 * mtsq * s * t * mwsq + 
     #0.4D1 * mtsq * s * t * u + 0.2D1 * mtsq * s * t ** 2 - 0.2D1 * mts
     #q * s ** 2 * mwsq + 0.2D1 * mtsq * s ** 2 * t + mtsq ** 2 * t ** 2
     # + 0.2D1 * mtsq ** 2 * s * t + mtsq ** 2 * s ** 2) / mwsq ** 2 / 0
     #.4D1

      c8 = -0.1D1 / s * (0.4D1 * mwsq ** 4 * s ** 2 - 0.4D1 * mwsq ** 5 
     #* s - 0.4D1 * mwsq ** 5 * t + mtsq ** 4 * s ** 2 + mtsq ** 3 * t *
     #* 3 + mtsq ** 4 * t ** 2 + 0.3D1 * mtsq * s * u * mwsq ** 2 * t - 
     #0.3D1 * mtsq ** 2 * s * u * mwsq * t + 0.2D1 * mtsq * s ** 2 * u *
     # mwsq * t + mtsq ** 3 * t ** 2 * s - mtsq ** 2 * t ** 2 * s ** 2 -
     # mtsq ** 2 * t ** 3 * s + 0.2D1 * mtsq ** 4 * s * t - mtsq ** 2 * 
     #s ** 2 * u * t - mtsq ** 2 * u ** 2 * s * t - 0.2D1 * mtsq ** 2 * 
     #t ** 2 * s * u + 0.2D1 * mtsq ** 3 * s * u * t + mtsq ** 2 * s ** 
     #2 * mwsq * t + 0.3D1 * mtsq * s ** 2 * mwsq ** 2 * t + 0.2D1 * mts
     #q * t ** 2 * s ** 2 * mwsq - 0.12D2 * mtsq * mwsq ** 3 * s * t - 0
     #.6D1 * mtsq ** 3 * s * mwsq * t + 0.8D1 * mtsq ** 2 * s * mwsq ** 
     #2 * t - 0.2D1 * mtsq ** 2 * t ** 2 * s * mwsq + 0.3D1 * mtsq * t *
     #* 2 * s * mwsq ** 2 - mtsq ** 2 * mwsq * s * u ** 2 - mtsq ** 3 * 
     #mwsq * s * u + 0.2D1 * mtsq * s ** 2 * u * mwsq ** 2 - 0.5D1 * mts
     #q * mwsq ** 3 * s * u + 0.5D1 * mtsq ** 2 * mwsq ** 2 * s * u - 0.
     #3D1 * mtsq ** 2 * s ** 2 * u * mwsq - mtsq ** 2 * mwsq * t * u ** 
     #2 - mtsq ** 3 * mwsq * t * u - 0.2D1 * mtsq ** 2 * t ** 2 * u * mw
     #sq - 0.5D1 * mtsq * mwsq ** 3 * t * u + 0.5D1 * mtsq ** 2 * mwsq *
     #* 2 * t * u + mtsq ** 4 * s * u + mtsq ** 3 * s * u ** 2 + mtsq **
     # 3 * s ** 2 * u + mtsq ** 3 * t * u ** 2 + mtsq ** 4 * t * u + 0.2
     #D1 * mtsq ** 3 * t ** 2 * u - 0.4D1 * mwsq ** 3 * s ** 2 * t + 0.8
     #D1 * mwsq ** 4 * s * t + 0.10D2 * mtsq * mwsq ** 4 * s - 0.6D1 * m
     #tsq ** 2 * mwsq ** 3 * s - 0.2D1 * mtsq ** 3 * mwsq ** 2 * s + 0.2
     #D1 * mtsq ** 4 * mwsq * s - 0.9D1 * mtsq * mwsq ** 3 * s ** 2 - 0.
     #3D1 * mtsq ** 3 * s ** 2 * mwsq + 0.7D1 * mtsq ** 2 * s ** 2 * mws
     #q ** 2 - mtsq ** 2 * t ** 3 * mwsq - mtsq ** 3 * t ** 2 * mwsq + 0
     #.10D2 * mtsq * mwsq ** 4 * t - 0.6D1 * mtsq ** 2 * mwsq ** 3 * t -
     # 0.2D1 * mtsq ** 3 * mwsq ** 2 * t + 0.2D1 * mtsq ** 4 * mwsq * t 
     #- 0.5D1 * t ** 2 * mtsq * mwsq ** 3 + 0.5D1 * mtsq ** 2 * t ** 2 *
     # mwsq ** 2) / mwsq ** 2 / (t ** 2 * mwsq ** 2 + 0.2D1 * s * t * mw
     #sq ** 2 - 0.2D1 * s * t ** 2 * mwsq + s ** 2 * mwsq ** 2 - 0.2D1 *
     # s ** 2 * t * mwsq + s ** 2 * t ** 2 - 0.2D1 * mtsq * t ** 2 * mws
     #q - 0.4D1 * mtsq * s * t * mwsq + 0.4D1 * mtsq * s * t * u + 0.2D1
     # * mtsq * s * t ** 2 - 0.2D1 * mtsq * s ** 2 * mwsq + 0.2D1 * mtsq
     # * s ** 2 * t + mtsq ** 2 * t ** 2 + 0.2D1 * mtsq ** 2 * s * t + m
     #tsq ** 2 * s ** 2) / 0.4D1

      c9 = 0.1D1 / s * (0.4D1 * mwsq ** 4 * s ** 3 - 0.8D1 * mwsq ** 5 *
     # s ** 2 - 0.4D1 * t ** 2 * mwsq ** 5 + mtsq ** 3 * s ** 4 + 0.2D1 
     #* mtsq ** 4 * s ** 3 + mtsq ** 4 * t ** 3 - mtsq * t ** 3 * s * u 
     #* mwsq - mtsq * t ** 2 * u ** 2 * s * mwsq - 0.3D1 * mtsq * t ** 2
     # * s ** 2 * u * mwsq - 0.2D1 * mtsq * u ** 2 * s ** 2 * mwsq * t +
     # 0.12D2 * mtsq * u ** 2 * s * mwsq ** 2 * t + 0.12D2 * mtsq * t **
     # 2 * s * u * mwsq ** 2 - 0.6D1 * mtsq ** 2 * u ** 2 * s * mwsq * t
     # - 0.14D2 * mtsq ** 2 * t ** 2 * s * u * mwsq - 0.13D2 * mtsq ** 2
     # * s ** 2 * u * t * mwsq - 0.4D1 * mtsq * s ** 3 * u * t * mwsq + 
     #0.22D2 * mtsq * s ** 2 * u * t * mwsq ** 2 - 0.41D2 * mtsq * mwsq 
     #** 3 * s * t * u + 0.27D2 * mtsq ** 2 * mwsq ** 2 * s * t * u - 0.
     #3D1 * mtsq ** 3 * mwsq * s * t * u + mtsq ** 2 * u ** 3 * s * t + 
     #mtsq ** 2 * t ** 3 * s * u + 0.2D1 * mtsq ** 2 * t ** 2 * u ** 2 *
     # s + 0.3D1 * mtsq ** 2 * u ** 2 * s ** 2 * t + 0.5D1 * mtsq ** 2 *
     # t ** 2 * s ** 2 * u + 0.8D1 * mtsq ** 3 * s * t * u ** 2 + 0.10D2
     # * mtsq ** 3 * s * t ** 2 * u + 0.11D2 * mtsq ** 3 * s ** 2 * t * 
     #u + 0.5D1 * mtsq ** 4 * s * t * u + 0.3D1 * s ** 3 * mtsq ** 2 * u
     # * t - 0.8D1 * t ** 2 * mwsq ** 3 * s * u + 0.12D2 * mwsq ** 4 * s
     # * t * u + 0.4D1 * t ** 2 * s ** 2 * u * mwsq ** 2 - 0.8D1 * mwsq 
     #** 3 * s ** 2 * u * t - 0.6D1 * mtsq ** 2 * t ** 3 * s * mwsq - 0.
     #2D1 * mtsq * t ** 3 * s ** 2 * mwsq + 0.15D2 * mtsq * mwsq ** 2 * 
     #s ** 2 * t ** 2 - 0.22D2 * mtsq * mwsq ** 3 * s * t ** 2 - 0.33D2 
     #* mtsq * mwsq ** 3 * s ** 2 * t + 0.30D2 * mtsq * mwsq ** 4 * s * 
     #t - 0.9D1 * mtsq ** 2 * mwsq * s ** 2 * t ** 2 + 0.12D2 * mtsq ** 
     #2 * mwsq ** 2 * s * t ** 2 + 0.17D2 * mtsq ** 2 * mwsq ** 2 * s **
     # 2 * t - 0.18D2 * mtsq ** 2 * mwsq ** 3 * s * t - 0.6D1 * mtsq ** 
     #3 * mwsq * s * t ** 2 - 0.9D1 * mtsq ** 3 * mwsq * s ** 2 * t - 0.
     #6D1 * mtsq ** 3 * mwsq ** 2 * s * t + 0.6D1 * mtsq ** 4 * s * t * 
     #mwsq - 0.2D1 * mtsq * s ** 4 * mwsq * t + 0.3D1 * mtsq * t ** 3 * 
     #s * mwsq ** 2 - 0.8D1 * s ** 3 * mtsq ** 2 * t * mwsq + 0.10D2 * m
     #tsq * s ** 3 * t * mwsq ** 2 - 0.2D1 * mtsq * s ** 3 * t ** 2 * mw
     #sq - mtsq ** 2 * u ** 3 * s * mwsq - mtsq ** 3 * mwsq * s * u ** 2
     # - 0.5D1 * mtsq * mwsq ** 3 * s * u ** 2 - 0.19D2 * mtsq * mwsq **
     # 3 * s ** 2 * u + 0.10D2 * mtsq * mwsq ** 4 * s * u + 0.5D1 * mtsq
     # ** 2 * mwsq ** 2 * s * u ** 2 + 0.17D2 * mtsq ** 2 * mwsq ** 2 * 
     #s ** 2 * u - 0.6D1 * mtsq ** 2 * mwsq ** 3 * s * u - 0.5D1 * mtsq 
     #** 3 * mwsq * s ** 2 * u - 0.2D1 * mtsq ** 3 * mwsq ** 2 * s * u +
     # 0.2D1 * mtsq * u ** 2 * s ** 2 * mwsq ** 2 + 0.2D1 * mtsq ** 4 * 
     #u * mwsq * s - 0.7D1 * s ** 3 * mtsq ** 2 * u * mwsq + 0.4D1 * mts
     #q * s ** 3 * u * mwsq ** 2 - 0.5D1 * mtsq ** 2 * s ** 2 * mwsq * u
     # ** 2 + mtsq * t ** 3 * u * mwsq ** 2 - mtsq ** 3 * mwsq * t * u *
     #* 2 + mtsq * t ** 2 * u ** 2 * mwsq ** 2 - mtsq ** 2 * u ** 3 * mw
     #sq * t - 0.5D1 * mtsq * mwsq ** 3 * t * u ** 2 - 0.8D1 * mtsq * mw
     #sq ** 3 * t ** 2 * u + 0.10D2 * mtsq * mwsq ** 4 * t * u + 0.5D1 *
     # mtsq ** 2 * mwsq ** 2 * t * u ** 2 + 0.6D1 * mtsq ** 2 * mwsq ** 
     #2 * t ** 2 * u - 0.6D1 * mtsq ** 2 * mwsq ** 3 * t * u - 0.4D1 * m
     #tsq ** 3 * mwsq * t ** 2 * u - 0.2D1 * mtsq ** 3 * mwsq ** 2 * t *
     # u + 0.2D1 * mtsq ** 4 * t * mwsq * u - 0.3D1 * mtsq ** 2 * t ** 2
     # * u ** 2 * mwsq - 0.2D1 * mtsq ** 2 * t ** 3 * u * mwsq + mtsq **
     # 2 * s ** 4 * t + 0.3D1 * mtsq ** 3 * s * t ** 3 + 0.4D1 * mtsq **
     # 3 * s ** 3 * t + 0.6D1 * mtsq ** 3 * s ** 2 * t ** 2 + 0.5D1 * mt
     #sq ** 4 * s ** 2 * t + 0.2D1 * mtsq ** 2 * t ** 3 * s ** 2 + 0.3D1
     # * mtsq ** 2 * s ** 3 * t ** 2 + 0.4D1 * mtsq ** 4 * s * t ** 2 + 
     #mtsq ** 4 * s * u ** 2 + mtsq ** 3 * s * u ** 3 + 0.3D1 * mtsq ** 
     #3 * s ** 3 * u + 0.3D1 * mtsq ** 4 * s ** 2 * u + 0.3D1 * mtsq ** 
     #3 * s ** 2 * u ** 2 + mtsq ** 3 * t * u ** 3 + mtsq ** 4 * t * u *
     #* 2 + mtsq ** 3 * t ** 3 * u + 0.2D1 * mtsq ** 4 * t ** 2 * u + 0.
     #2D1 * mtsq ** 3 * t ** 2 * u ** 2 + 0.20D2 * mwsq ** 4 * s ** 2 * 
     #t - 0.12D2 * mwsq ** 5 * s * t + 0.12D2 * mwsq ** 4 * s * t ** 2 +
     # 0.4D1 * mwsq ** 2 * s ** 3 * t ** 2 - 0.12D2 * mwsq ** 3 * s ** 2
     # * t ** 2 - 0.8D1 * s ** 3 * mwsq ** 3 * t + 0.4D1 * mwsq ** 4 * s
     # ** 2 * u - 0.4D1 * mwsq ** 5 * s * u + 0.4D1 * mwsq ** 4 * t ** 2
     # * u - 0.4D1 * mwsq ** 5 * t * u - 0.14D2 * mtsq * mwsq ** 3 * s *
     #* 3 - 0.4D1 * mtsq ** 3 * mwsq ** 2 * s ** 2 + 0.4D1 * mtsq ** 4 *
     # mwsq * s ** 2 - 0.12D2 * mtsq ** 2 * mwsq ** 3 * s ** 2 - 0.4D1 *
     # mtsq ** 3 * mwsq * s ** 3 + 0.20D2 * mtsq * mwsq ** 4 * s ** 2 + 
     #0.12D2 * mtsq ** 2 * mwsq ** 2 * s ** 3 + 0.2D1 * mtsq * s ** 4 * 
     #mwsq ** 2 - 0.3D1 * mtsq ** 2 * s ** 4 * mwsq - t ** 3 * mwsq ** 3
     # * mtsq - 0.6D1 * mtsq ** 2 * mwsq ** 3 * t ** 2 + 0.3D1 * t ** 3 
     #* mwsq ** 2 * mtsq ** 2 - 0.3D1 * t ** 3 * mwsq * mtsq ** 3 + 0.10
     #D2 * t ** 2 * mwsq ** 4 * mtsq - 0.2D1 * t ** 2 * mwsq ** 2 * mtsq
     # ** 3 + 0.2D1 * mtsq ** 4 * t ** 2 * mwsq) / (u + s) / mwsq ** 2 /
     # (t ** 2 * mwsq ** 2 + 0.2D1 * s * t * mwsq ** 2 - 0.2D1 * s * t *
     #* 2 * mwsq + s ** 2 * mwsq ** 2 - 0.2D1 * s ** 2 * t * mwsq + s **
     # 2 * t ** 2 - 0.2D1 * mtsq * t ** 2 * mwsq - 0.4D1 * mtsq * s * t 
     #* mwsq + 0.4D1 * mtsq * s * t * u + 0.2D1 * mtsq * s * t ** 2 - 0.
     #2D1 * mtsq * s ** 2 * mwsq + 0.2D1 * mtsq * s ** 2 * t + mtsq ** 2
     # * t ** 2 + 0.2D1 * mtsq ** 2 * s * t + mtsq ** 2 * s ** 2) / 0.4D
     #1

	
	box6 = c10*box4+c11*triAs+c12*triAt+c13*triBs+c14*triBt
	
	FA_massive= c0*box6
     .		+c1*bubEs
     .		+c2*bubEt
     .		+c3*bubmh
     .		+c4*tadmt
     .		+c5*tadmw
     .		+c6*triAs
     .		+c7*triAt
     .		+c8*triBs
     .		+c9*triBt
	
	
      end




	subroutine ewk_collinear_lims()
	implicit none
	double complex FA_massless,FA_massive,Ie,FA_higgs,QQQ
	double complex mhsq,mwsq,res,resmass,mtsq,ewk_uub,ewk_ug,lim
	double precision z,lambda
	integer i
	double precision s,t,u
	Ie = dcmplx(0d0,1d-15)
	
	!print*,"EWK testing"
	!mhsq = dcmplx(2d0,0d0)-Ie
	!mwsq = dcmplx(1.1d0,0d0)-Ie
	!mtsq = dcmplx(1.5d0,0d0)-Ie
	
	!z=0.9d0
	!lambda=0.23d0
	!s = dreal(mhsq)/z 
	!t = -s*(1d0-z)*lambda
	!u = -s*(1d0-z)*(1d0-lambda)
	!print*,"Sanity test "
	!res = FA_massless(s,t,u,mhsq,mwsq)
	!print*,"FA_massless=",res
	!res = FA_higgs(s,t,u,mhsq,mwsq)
	!print*,"** FA_higgs=",res,mhsq
c	print*,"u->0 (lambda -> 1)"
c	do i=1,10
c		z=1d0-0.8d0!/10d0**i
c		lambda=1d0-0.1d0/10d0**i
c		s = mhsq/z
c		t = -s*(1d0-z)*lambda
c		u = -s*(1d0-z)*(1d0-lambda)
c	
c		res = FA_massless(s,t,u,mhsq,mwsq)
c		resmass=FA_massive(s,t,u,mhsq,mtsq,mwsq)
c		!print*,"lambda=",lambda," FA_massless = ",res,resmass
c	enddo

	print*,"t->0 (lambda -> 0)"
	do i=1,10
		z=1d0-0.8d0!/10d0**i
		lambda=0.1d0/10d0**i
		mwsq=dcmplx(3d0,0d0)
		mhsq=dcmplx(9d0,0d0)
		t = mhsq/z
		s = -t*(1d0-z)*lambda
		u = -t*(1d0-z)*(1d0-lambda)
c		QQQ = dcmplx(1d0,0d0)
c		res = ewk_uub(z,lambda,QQQ,1)
c		resmass=ewk_ug(z,lambda,QQQ,1)
		res = FA_massless(s,t,u,mhsq,mwsq)
		resmass = FA_massless(s,u,t,mhsq,mwsq)
		print*,"lambda,t =",lambda,t," FA_massless = ",res,resmass
	enddo

c	print*,"mv->infty"
c	do i=1,1000
c		z=1d0-0.8d0!/10d0**i
c		lambda=0.1d0!/10d0**i
c		mhsq = dcmplx(1d0,0d0)
c		mwsq = dcmplx(0.05d0,0d0) + dcmplx(10d0*i,0d0)
c		
c		s = mhsq/z
c		t = -s*(1d0-z)*lambda
c		u = -s*(1d0-z)*(1d0-lambda)
c		res = FA_massless(s,t,u,mhsq,mwsq)
c		lim = 5d0/36d0/mwsq**2
c		print*,"mvsq=",mwsq," res=",dreal(res),
c    $		dreal(lim),dreal(res-lim)/dreal(lim)
c	enddo
	
c	print*,"s,t->0 (z-> 1)"
c	do i=1,10
c		z=1d0-0.8d0/10d0**i
c		lambda=0.1d0!/10d0**i
c		s = mhsq/z
c		t = -s*(1d0-z)*lambda
c		u = -s*(1d0-z)*(1d0-lambda)
c	
c		res = FA_massless(s,t,u,mhsq,mwsq)
c		resmass=FA_massive(s,t,u,mhsq,mtsq,mwsq)
c		!print*,"lambda=",lambda," FA_massless = ",res,resmass
c	enddo
	

	
	
	end
