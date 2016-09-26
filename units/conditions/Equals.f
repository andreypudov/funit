!
! A unit testing library for Fortran
!
! The MIT License
!
! Copyright 2011-2016 Andrey Pudov
!
! Permission is hereby granted, free of charge, to any person obtaining a copy
! of this software and associated documentation files (the 'Software'), to deal
! in the Software without restriction, including without limitation the rights
! to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
! copies of the Software, and to permit persons to whom the Software is
! furnished to do so, subject to the following conditions:
!
! The above copyright notice and this permission notice shall be included in
! all copies or substantial portions of the Software.
!
! THE SOFTWARE IS PROVIDED 'AS IS', WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.
!

module EqualsConditionUnit

    use Unit
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
