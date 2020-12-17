
MODULE rad_decay_class
USE environment_settings
IMPLICIT NONE
PUBLIC

	real(kind=rknd), PRIVATE, PARAMETER :: PI = 4.d0 * ATAN(1.0d0)  ! Most standard/accurate way to set value of PI
	real(kind=rknd), PRIVATE, PARAMETER :: log2 = LOG(2.0d0)

	TYPE, PUBLIC :: Isotope
		! inputs, private
		character(len=16)               :: name
		character(len=16)               :: decay_type
		integer(kind=iknd)              :: nprods
		character(len=16),  allocatable :: products(:)
		integer(kind=iknd)              :: A
		integer(kind=iknd)              :: Z
		integer(kind=iknd)              :: N
		real(kind=rknd)                 :: c0
		real(kind=rknd)                 :: t_half
		real(kind=rknd)                 :: lambda
		integer(kind=iknd)              :: ID
		integer(kind=iknd), allocatable :: prodID(:)


		! 
		real(kind=rknd) :: c
		real(kind=rknd) :: dc

		CONTAINS
			procedure :: init         => initialize_isotope
			procedure :: print_info   => print_isotope_info
			procedure :: update_c     => update_isotope_abundance
			procedure :: map_products => map_decay_products
	END TYPE Isotope

CONTAINS

	SUBROUTINE initialize_isotope(iso, ID, name, decay_type, nprods, products, A, Z, t_half)
		! input variables
		class(isotope)     :: iso
		integer(kind=iknd) :: ID
		character(len=16)  :: name
		character(len=16)  :: decay_type
		integer(kind=iknd) :: nprods
		character(len=16)  :: products(nprods)
		integer(kind=iknd) :: A
		integer(kind=iknd) :: Z
		real(kind=rknd)    :: t_half
		real(kind=rknd)    :: c0

		! local variables
		integer(kind=iknd) :: i

		iso%ID     = ID
		iso%name         = name
		iso%decay_type   = decay_type
		iso%nprods = nprods

		ALLOCATE(iso%products(nprods))
		ALLOCATE(iso%prodID(nprods))
		DO i = 1, nprods
			iso%products(i)   = products(i)
			iso%prodID(i) = 0
		ENDDO

		iso%A      = A
		iso%Z      = Z
		iso%N      = A-Z
		iso%c0     = 0.0d0
		iso%t_half = t_half
		iso%lambda = 0.0d0
		IF (t_half .gt. 0.0d0) iso%lambda = log2 / t_half
	END SUBROUTINE initialize_isotope

	SUBROUTINE map_decay_products(iso, prodID)
		class(isotope)     :: iso
		integer(kind=iknd) :: prodID
		integer(kind=iknd) :: i
		DO i = 1, iso%nprods
			iso%prodID(i) = prodID(i)
		ENDDO
	END SUBROUTINE map_decay_products

	SUBROUTINE print_isotope_info(iso,multiple)
		class(isotope)     :: iso
		integer(kind=iknd) :: i
		character(len=16)  :: str(7)
		character(len=99)  :: fmt(6)
		logical, optional  :: multiple
		
		fmt(1) = '(I3)'
		fmt(2) = '(ES10.4)'
		fmt(3) = '(A)'
		fmt(4) = '(A21,X,A,X,A)'
		fmt(5) = '(A,X,I1,8X,A,X,A,X,A)'
		fmt(6) = '(A)'
		
		WRITE(str(1), fmt(1)) iso%ID
		WRITE(str(2), fmt(1)) iso%A
		WRITE(str(3), fmt(1)) Iso%Z
		WRITE(str(4), fmt(1)) iso%N
		WRITE(str(5), fmt(2)) iso%c0
		WRITE(str(6), fmt(2)) iso%c
		WRITE(str(7), fmt(2)) iso%t_half

		IF ((PRESENT(multiple) .and. multiple .eqv. .TRUE.) .or. (.not. PRESENT(multiple))) THEN
			WRITE(*, fmt(3))     NEW_LINE('A')//"#"//REPEAT('-',38)//"#"
			WRITE(*, fmt(3))     "#-------- ISOTOPIC INFORMATION --------#"
			WRITE(*, fmt(3))     "#"//REPEAT('-',38)//"#"
		ENDIF
		WRITE(*, fmt(4))     "# ISOTOPE NAME      =", iso%name,        "#"
		WRITE(*, fmt(4))     "# ISOTOPE ID        =", ADJUSTL(str(1)), "#"
		WRITE(*, fmt(4))     "# ATOMIC NUMBER     =", ADJUSTL(str(2)), "#"
		WRITE(*, fmt(4))     "# PROTON NUMBER     =", ADJUSTL(str(3)), "#"
		WRITE(*, fmt(4))     "# NEUTRON NUMBER    =", ADJUSTL(str(4)), "#"
		WRITE(*, fmt(4))     "# INITIAL ABUNDANCE =", ADJUSTL(str(5)), "#"
		WRITE(*, fmt(4))     "# CURRENT ABUNDANCE =", ADJUSTL(str(6)), "#"
		WRITE(*, fmt(4))     "# HALF-LIFE         =", ADJUSTL(str(7)), "#"
		WRITE(*, fmt(4))     "# DECAY TYPE        =", iso%decay_type,  "#"
		DO i = 1, iso%nprods
			WRITE(*, fmt(5)) "# DAUGHTER", i,    "=", iso%products(i), "#"
		ENDDO
		WRITE(*, fmt(6))     "#"//REPEAT('-',38)//"#"
	END SUBROUTINE print_isotope_info

	SUBROUTINE update_isotope_abundance(iso,c)
	IMPLICIT NONE
		class(Isotope)  :: iso
		real(kind=rknd) :: c
		iso%c = c
	END SUBROUTINE update_isotope_abundance

END MODULE rad_decay_class