!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

module EqualsConditionUnit

    use FUnit
    use Conditions

    implicit none
    private

    double precision, parameter :: DOUBLE_PRECISION_DELTA = 1D-15
    real,             parameter :: REAL_DELTA             = 1E-15

    type, extends(UnitSuite), public :: EqualsConditionSuite
    private
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type
contains
    subroutine init(self, name)
        class(EqualsConditionSuite), intent(in out) :: self
        character(len=*), optional,      intent(in)     :: name

        call self%UnitSuite%init('Equals condition')

        call self%add(equals_normal,   'Normal case')
        call self%add(equals_negative, 'Negative case')
    end subroutine

    subroutine clean(self)
        class(EqualsConditionSuite), intent(in out) :: self

        call self%UnitSuite%clean()
    end subroutine

    subroutine equals_normal(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        character        :: character1, character2
        complex          :: complex1, complex2
        double precision :: dbleprcsn1, dbleprcsn2
        integer          :: integer1, integer2
        logical          :: logical1, logical2
        real             :: real1, real2

        character1 = 'a'
        character2 = 'a'

        complex1   = (1.0, 0.1)
        complex2   = (1.0, 0.1)

        dbleprcsn1 = 1.0_8
        dbleprcsn2 = 1.0_8

        integer1   = 1
        integer2   = 1

        logical1   = .false.
        logical2   = .false.

        real1      = 1.0
        real2      = 1.0

        if (.not. equals(character1, character2)) then
            call asserts%fail('array of character variables')
        end if

        if (.not. equals(complex1, complex2, REAL_DELTA)) then
            call asserts%fail('array of complex variables')
        end if

        if (.not. equals(dbleprcsn1, dbleprcsn2, DOUBLE_PRECISION_DELTA)) then
            call asserts%fail('array of deouble precision variables')
        end if

        if (.not. equals(integer1, integer2)) then
            call asserts%fail('array of integer variables')
        end if

        if (.not. equals(logical1, logical2)) then
            call asserts%fail('array of logical variables')
        end if

        if (.not. equals(real1, real2, REAL_DELTA)) then
            call asserts%fail('array of real variables')
        end if
    end subroutine

    subroutine equals_negative(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        character        :: character1, character2
        complex          :: complex1, complex2
        double precision :: dbleprcsn1, dbleprcsn2
        integer          :: integer1, integer2
        logical          :: logical1, logical2
        real             :: real1, real2

        character1 = 'a'
        character2 = 'b'

        complex1   = (0.0, 0.0)
        complex2   = (1.0, 0.0)

        dbleprcsn1 = 0.0_8
        dbleprcsn2 = 1.0_8

        integer1   = 0
        integer2   = 1

        logical1   = .false.
        logical2   = .true.

        real1      = 0.0
        real2      = 1.0

        if (equals(character1, character2)) then
            call asserts%fail('array of character variables')
        end if

        if (equals(complex1, complex2, REAL_DELTA)) then
            call asserts%fail('array of complex variables')
        end if

        if (equals(dbleprcsn1, dbleprcsn2, DOUBLE_PRECISION_DELTA)) then
            call asserts%fail('array of double precision variables')
        end if

        if (equals(integer1, integer2)) then
            call asserts%fail('array of integer variables')
        end if

        if (equals(logical1, logical2)) then
            call asserts%fail('array of logical variables')
        end if

        if (equals(real1, real2, REAL_DELTA)) then
            call asserts%fail('array of real variables')
        end if
    end subroutine
end module
