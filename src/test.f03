PROGRAM test
USE environment_settings
USE rad_decay_class
USE rad_decay_chain
USE integrators
IMPLICIT NONE

	type(Integrator)   :: ODE
	type(DecayChain)   :: chain
	real(kind=rknd)    :: ti, tf, dt, relerr, abserr, thlf
	integer(kind=iknd) :: i, j, k, stepmax
	logical            :: header
	character(len=50)  :: fmt, temp

	CALL chain%init_chain()

	ti      = 0.0d0
	tf      = 24*60*60*100.0d0
	dt      = 1.0d+2
	relerr  = 1.0d-10
	abserr  = 1.0d-10
	stepmax = 999999999
	call ODE%init(chain, ti, tf, dt, stepmax, relerr, abserr, 'forward_euler')

	OPEN(UNIT=26, FILE='../output_data/decay.dat', STATUS="REPLACE")
	WRITE(temp,*) ODE%neqn+1
	fmt = TRIM('(')//TRIM(ADJUSTL(temp))//TRIM('(A,X))')
	WRITE(26,fmt) 'time', (ADJUSTL(TRIM(ODE%chain%isotope_list(i)%name)), i=1,ODE%neqn)

	fmt = '(X,A,2X,I9,2X,300(ES8.2,2X))'
	DO WHILE (ODE%t .lt. ODE%tf .and. ODE%steps .lt. ODE%stepmax)
		CALL ODE%step()
		WRITE(26,*) ODE%t, (ODE%y(i), i=1,ODE%neqn)

		WRITE(*,fmt) "step, t, dt:", ODE%steps, 100*ODE%t/ODE%tf, ODE%dt
	ENDDO
	CLOSE(26)

	header = .TRUE.
	DO i=1,3
		CALL chain%isotope_list(i)%print_info(header)
		header=.FALSE.
	ENDDO

END PROGRAM test
