
MODULE integrators
USE environment_settings
USE rad_decay_class
USE rad_decay_chain
IMPLICIT NONE
PRIVATE

	TYPE, PUBLIC :: Integrator
		TYPE(DecayChain)             :: chain
		REAL(kind=rknd), ALLOCATABLE :: ynew(:)
		REAL(kind=rknd), ALLOCATABLE :: y(:)
		REAL(kind=rknd), ALLOCATABLE :: yp(:)
		REAL(kind=rknd)              :: relerr, abserr
		REAL(kind=rknd)              :: t, ti, tf, dt 
		INTEGER(kind=iknd)           :: stepmax, steps, neqn
		CHARACTER(LEN=100)           :: intg_type
		CONTAINS
			procedure :: init => initialize_integrator
			procedure :: f    => function
			procedure :: step => integration_step
	END TYPE


CONTAINS


	SUBROUTINE initialize_integrator(this, chain, ti, tf, dt, stepmax, relerr, abserr, intg_type)
		CLASS(Integrator)  :: this
		TYPE(DecayChain)   :: chain
		REAL(kind=rknd)    :: relerr, abserr
		REAL(kind=rknd)    :: ti, tf, dt 
		INTEGER(kind=iknd) :: stepmax
		CHARACTER(LEN=*)   :: intg_type
		INTEGER(kind=iknd) :: k,j,i
		this%chain     = chain
		this%t         = ti
		this%ti        = ti
		this%tf        = tf
		this%dt        = dt
		this%stepmax   = stepmax
		this%relerr    = relerr
		this%abserr    = abserr
		this%intg_type = intg_type
		this%neqn      = this%chain%nisos
		ALLOCATE(this%ynew(this%neqn))
		ALLOCATE(this%y(this%neqn))
		ALLOCATE(this%yp(this%neqn))
		this%yp   = 0.0d0
		this%ynew = 0.0d0
		DO k = 1, this%neqn
			this%y(k) = this%chain%isotope_list(k)%c0
		ENDDO
		this%steps = 0
	END SUBROUTINE initialize_integrator



	SUBROUTINE function(this)
	IMPLICIT NONE
		CLASS(Integrator)  :: this
		INTEGER(KIND=iknd) :: k, j, kd, jd
		REAL(KIND=rknd)    :: fx, lm
		this%yp = 0.0d0
		DO k = 1, this%neqn
			IF (this%chain%isotope_list(k)%t_half .le. 0.0d0) CYCLE
			kd = this%chain%isotope_list(k)%ID
			lm = this%chain%isotope_list(k)%lambda
			fx = lm * this%y(kd)
			this%yp(kd) = this%yp(kd) - fx
			DO j = 1, this%chain%isotope_list(k)%nprods
				jd = this%chain%isotope_list(k)%prodID(j)
				this%yp(jd) = this%yp(jd) + fx
			ENDDO
		ENDDO
	END SUBROUTINE function



	SUBROUTINE integration_step(this)
		CLASS(Integrator)  :: this
		INTEGER(KIND=iknd) :: k,j,i
		CALL this%f()
		IF (this%tf - this%t .lt. this%dt) this%dt = this%tf - this%t
		SELECT CASE (TRIM(this%intg_type))

		CASE('forward_euler')
			CALL forward_euler(this%neqn, this%y, this%yp, this%t, this%dt)
		END SELECT
		
		DO k = 1, this%chain%nisos
			j = this%chain%isotope_list(k)%ID
			this%chain%isotope_list(k)%c = this%y(j)
		ENDDO
		this%steps = this%steps + 1
	END SUBROUTINE integration_step



	SUBROUTINE forward_euler(n,y,yp,t,dt)
	IMPLICIT NONE
		INTEGER(KIND=iknd), intent(IN) :: n
		REAL(KIND=rknd), intent(INOUT) :: y(n), t
		REAL(KIND=rknd),    intent(IN) :: yp(n), dt
		INTEGER(KIND=iknd)             :: i
		t = t + dt
		DO i = 1, n
			y(i) = y(i) + dt*yp(i)
		ENDDO
	END SUBROUTINE forward_euler

END MODULE integrators