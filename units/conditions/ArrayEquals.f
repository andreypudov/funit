!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

module ArrayEqualsConditionUnit

    use FUnit
    use Conditions

    implicit none
    private

    double precision, parameter :: DOUBLE_PRECISION_DELTA = 1D-15
    real,             parameter :: REAL_DELTA             = 1E-15

    type, extends(UnitSuite), public :: ArrayEqualsConditionSuite
    private
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type
contains
    subroutine init(self, name)
        class(ArrayEqualsConditionSuite), intent(in out) :: self
        character(len=*), optional,       intent(in)     :: name

        call self%UnitSuite%init('ArrayEquals condition')

        call self%add(arrayEquals_normal,   'Normal case')
        call self%add(arrayEquals_negative, 'Negative case')
    end subroutine

    subroutine clean(self)
        class(ArrayEqualsConditionSuite), intent(in out) :: self

        call self%UnitSuite%clean()
    end subroutine

    subroutine arrayEquals_normal(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        character,        dimension(4) :: character1, character2
        complex,          dimension(4) :: complex1, complex2
        double precision, dimension(4) :: dbleprcsn1, dbleprcsn2
        integer,          dimension(4) :: integer1, integer2
        logical,          dimension(4) :: logical1, logical2
        real,             dimension(4) :: real1, real2

        character1 = (/ 'a', 'b', 'c', 'd' /)
        character2 = (/ 'a', 'b', 'c', 'd' /)

        complex1   = (/ (0.0, 0.0), (0.5, 0.5), (1.0, 1.0), (1.5, 1.5) /)
        complex2   = (/ (0.0, 0.0), (0.5, 0.5), (1.0, 1.0), (1.5, 1.5) /)

        dbleprcsn1 = (/ 0.0_8, 0.5_8, 1.0_8, 1.5_8 /)
        dbleprcsn2 = (/ 0.0_8, 0.5_8, 1.0_8, 1.5_8 /)

        integer1   = (/ 0, 1, 2, 3 /)
        integer2   = (/ 0, 1, 2, 3 /)

        logical1   = (/ .false., .true., .true., .false. /)
        logical2   = (/ .false., .true., .true., .false. /)

        real1      = (/ 0.0, 0.5, 1.0, 1.5 /)
        real2      = (/ 0.0, 0.5, 1.0, 1.5 /)

        if (.not. arrayEquals(character1, character2)) then
            call asserts%fail('array of character variables')
        end if

        if (.not. arrayEquals(complex1, complex2, REAL_DELTA)) then
            call asserts%fail('array of complex variables')
        end if

        if (.not. arrayEquals(dbleprcsn1, dbleprcsn2, DOUBLE_PRECISION_DELTA)) then
            call asserts%fail('array of deouble precision variables')
        end if

        if (.not. arrayEquals(integer1, integer2)) then
            call asserts%fail('array of integer variables')
        end if

        if (.not. arrayEquals(logical1, logical2)) then
            call asserts%fail('array of logical variables')
        end if

        if (.not. arrayEquals(real1, real2, REAL_DELTA)) then
            call asserts%fail('array of real variables')
        end if
    end subroutine

    subroutine arrayEquals_negative(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        character,        dimension(4) :: character1, character2
        complex,          dimension(4) :: complex1, complex2
        double precision, dimension(4) :: dbleprcsn1, dbleprcsn2
        integer,          dimension(4) :: integer1, integer2
        logical,          dimension(4) :: logical1, logical2
        real,             dimension(4) :: real1, real2

        character1 = (/ 'a', 'b', 'c', 'd' /)
        character2 = (/ 'b', 'c', 'd', 'e' /)

        complex1   = (/ (0.0, 0.0), (0.5, 0.5), (1.0, 1.0), (1.5, 1.5) /)
        complex2   = (/ (1.0, 0.0), (1.5, 0.5), (2.0, 1.0), (2.5, 1.5) /)

        dbleprcsn1 = (/ 0.0_8, 0.5_8, 1.0_8, 1.5_8 /)
        dbleprcsn2 = (/ 1.0_8, 1.5_8, 2.0_8, 2.5_8 /)

        integer1   = (/ 0, 1, 2, 3 /)
        integer2   = (/ 1, 2, 3, 4 /)

        logical1   = (/ .false., .true., .true., .false. /)
        logical2   = (/ .true., .false., .false., .true. /)

        real1      = (/ 0.0, 0.5, 1.0, 1.5 /)
        real2      = (/ 1.0, 1.5, 2.0, 2.5 /)

        if (arrayEquals(character1, character2)) then
            call asserts%fail('array of character variables')
        end if

        if (arrayEquals(complex1, complex2, REAL_DELTA)) then
            call asserts%fail('array of complex variables')
        end if

        if (arrayEquals(dbleprcsn1, dbleprcsn2, DOUBLE_PRECISION_DELTA)) then
            call asserts%fail('array of double precision variables')
        end if

        if (arrayEquals(integer1, integer2)) then
            call asserts%fail('array of integer variables')
        end if

        if (arrayEquals(logical1, logical2)) then
            call asserts%fail('array of logical variables')
        end if

        if (arrayEquals(real1, real2, REAL_DELTA)) then
            call asserts%fail('array of real variables')
        end if
    end subroutine
end module
