
MODULE rad_decay_chain
USE rad_decay_class
IMPLICIT NONE
	TYPE, PUBLIC :: DecayChain
		INTEGER(KIND=iknd)                       :: nisos
		TYPE(Isotope), DIMENSION(:), ALLOCATABLE :: isotope_list
		INTEGER(KIND=iknd)                       :: ncols, nlines, nchars 
		CONTAINS
			procedure :: init_chain      => initialize_decay_chain
			procedure :: load_isotopes   => load_isotopes_from_file
			procedure :: load_abundances => load_abundances_from_file
			procedure :: allocate_chain  => allocate_decay_chain
			procedure :: map_products    => map_decay_chain_products
			procedure :: print_map       => print_decay_chain_map
	END TYPE

CONTAINS

	SUBROUTINE initialize_decay_chain(this)
		CLASS(DecayChain)  :: this
		CALL this%allocate_chain()
		CALL this%load_isotopes()
		CALL this%load_abundances()
		CALL this%map_products()
		! CALL this%print_map()
	END SUBROUTINE initialize_decay_chain
	

	SUBROUTINE allocate_decay_chain(this)
		CLASS(DecayChain)  :: this
		CALL execute_command_line('wc -L ../input_data/isotopes.dat > wc.txt')
		CALL execute_command_line('wc -l ../input_data/isotopes.dat >> wc.txt')
		CALL execute_command_line("awk -F' ' '{print NF; exit}' ../input_data/isotopes.dat >> wc.txt")
		CALL execute_command_line('wc -l ../input_data/abundances.dat >> wc.txt')
		OPEN(UNIT=21, FILE='wc.txt', STATUS='OLD', ACTION="READ")
		READ(21,*) this%nchars
		READ(21,*) this%nlines
		READ(21,*) this%ncols
		READ(21,*) this%nisos
		CLOSE(21)
		ALLOCATE(this%isotope_list(this%nlines))
	END SUBROUTINE allocate_decay_chain


	SUBROUTINE load_isotopes_from_file(this)
		CLASS(DecayChain)  :: this
		INTEGER(KIND=iknd) :: i, j, idx, pnum
		CHARACTER(LEN=100) :: dataline, tempstr
		CHARACTER(LEN=16)  :: parent, dtype, prods(3)
		INTEGER(KIND=iknd) :: A, Z
		REAL(KIND=rknd)    :: thlf

		OPEN(UNIT=21, FILE='../input_data/isotopes.dat', STATUS='OLD', ACTION='READ')
		DO i = 1, this%nlines
			READ(21,'(A)') dataline

			idx = SCAN(dataline,' ')
			parent = ADJUSTL(dataline(1:idx-1))
			dataline = ADJUSTL(dataline(idx+1:))

			idx = SCAN(dataline,' ')
			dtype = ADJUSTL(TRIM(dataline(1:idx-1)))
			dataline = ADJUSTL(TRIM(dataline(idx+1:)))

			idx = SCAN(dataline,' ')
			tempstr = ADJUSTL(TRIM(dataline(1:idx-1)))
			READ(tempstr,*) A
			dataline = ADJUSTL(TRIM(dataline(idx+1:)))

			idx = SCAN(dataline,' ')
			tempstr = ADJUSTL(TRIM(dataline(1:idx-1)))
			READ(tempstr,*) Z
			dataline = ADJUSTL(TRIM(dataline(idx+1:)))
			
			idx = SCAN(dataline,' ')
			tempstr = ADJUSTL(TRIM(dataline(1:idx-1)))
			READ(tempstr, *) thlf
			dataline = ADJUSTL(TRIM(dataline(idx+1:)))
			
			pnum = 0
			DO j = 1, this%ncols - 5
				idx = SCAN(dataline,' ')
				prods(j) = ADJUSTL(TRIM(dataline(1:idx-1)))
				dataline = ADJUSTL(TRIM(dataline(idx+1:)))
				IF (LEN_TRIM(prods(j)) .gt. 0) pnum = pnum + 1
			ENDDO
			CALL this%isotope_list(i)%init(i, parent, dtype, pnum, prods, A, Z, thlf)
		ENDDO
		CLOSE(21)
	END SUBROUTINE load_isotopes_from_file

	SUBROUTINE load_abundances_from_file(this)
		CLASS(DecayChain)  :: this
		INTEGER(KIND=iknd) :: i, j
		CHARACTER(LEN=16)  :: parent
		REAL(KIND=rknd)    :: conc

		OPEN(UNIT=21, FILE='../input_data/abundances.dat', STATUS='OLD', ACTION='READ')
		DO i = 1, this%nisos
			READ(21,*) parent, conc
			DO j = 1, this%nisos
				IF (TRIM(this%isotope_list(j)%name) == TRIM(parent)) THEN
					this%isotope_list(j)%c0 = conc
				ENDIF
			ENDDO
		ENDDO
		CLOSE(21)
	END SUBROUTINE load_abundances_from_file

	SUBROUTINE map_decay_chain_products(this)
		CLASS(DecayChain)       :: this
		INTEGER(KIND=iknd)      :: k,j,i
		DO k = 1, this%nisos
			DO j = 1, this%nisos
				IF (k .ne. j) THEN
					DO i = 1, this%isotope_list(k)%nprods
						IF (this%isotope_list(j)%name .eq. this%isotope_list(k)%products(i)) THEN
							this%isotope_list(k)%prodID(i) = this%isotope_list(j)%ID
						ENDIF
					ENDDO
				ENDIF
			ENDDO
		ENDDO
	END SUBROUTINE map_decay_chain_products


	SUBROUTINE print_decay_chain_map(this)
		CLASS(DecayChain)  :: this
		INTEGER(KIND=iknd) :: k,j,i, kID, jID
		CHARACTER(LEN=20)  :: fmt, kname, jname
		fmt = '(A,X,I1,3X,A,X,I1)'
		DO k = 1,this%nisos
			kname = this%isotope_list(k)%name
			kID   = this%isotope_list(k)%ID
			DO j = 1, this%isotope_list(k)%nprods
				jname = this%isotope_list(k)%products(j)
				jID   = this%isotope_list(k)%prodID(j)
				WRITE(*,fmt) kname, kID, jname, jID
			ENDDO
		ENDDO
	END SUBROUTINE print_decay_chain_map
END MODULE rad_decay_chain