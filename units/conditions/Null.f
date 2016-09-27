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

module NullConditionUnit

    use Unit
    use Conditions

    implicit none
    private

    type, extends(UnitSuite), public :: NullConditionSuite
    private
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type
contains
    subroutine init(self, name)
        class(NullConditionSuite), intent(in out) :: self
        character(len=*), optional, intent(in)       :: name

        call self%UnitSuite%init('Null condition')

        call self%add(null_normal,   'Normal case')
        call self%add(null_negative, 'Negative case')
    end subroutine

    subroutine clean(self)
        class(NullConditionSuite), intent(in out) :: self

        call self%UnitSuite%clean()
    end subroutine

    subroutine null_normal(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        character,        pointer :: character1
        complex,          pointer :: complex1
        double precision, pointer :: dbleprcsn1
        integer,          pointer :: integer1
        logical,          pointer :: logical1
        real,             pointer :: real1

        character1 => null()
        complex1   => null()
        dbleprcsn1 => null()
        integer1   => null()
        logical1   => null()
        real1      => null()

        if (.not. notNull(character1)) then
            call asserts%fail('character value is not null')
        end if

        if (.not. notNull(complex1)) then
            call asserts%fail('complex value is not null')
        end if

        if (.not. notNull(dbleprcsn1)) then
            call asserts%fail('double precision value is not null')
        end if

        if (.not. notNull(integer1)) then
            call asserts%fail('integer value is not null')
        end if

        if (.not. notNull(logical1)) then
            call asserts%fail('logical value is not null')
        end if

        if (.not. notNull(real1)) then
            call asserts%fail('real value is not null')
        end if
    end subroutine

    subroutine null_negative(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        character,        pointer :: character1
        complex,          pointer :: complex1
        double precision, pointer :: dbleprcsn1
        integer,          pointer :: integer1
        logical,          pointer :: logical1
        real,             pointer :: real1

        character,        target :: character2
        complex,          target :: complex2
        double precision, target :: dbleprcsn2
        integer,          target :: integer2
        logical,          target :: logical2
        real,             target :: real2

        character2 = 'a'
        complex2   = (1.0, 0.1)
        dbleprcsn2 = 1.0_8
        integer2   = 1
        logical2   = .false.
        real2      = 1.0

        character1 => character2
        complex1   => complex2
        dbleprcsn1 => dbleprcsn2
        integer1   => integer2
        logical1   => logical2
        real1      => real2

        if (notNull(character1)) then
            call asserts%fail('character value is null')
        end if

        if (notNull(complex1)) then
            call asserts%fail('complex value is null')
        end if

        if (notNull(dbleprcsn1)) then
            call asserts%fail('double precision value is null')
        end if

        if (notNull(integer1)) then
            call asserts%fail('integer value is null')
        end if

        if (notNull(logical1)) then
            call asserts%fail('logical value is null')
        end if

        if (notNull(real1)) then
            call asserts%fail('real value is null')
        end if
    end subroutine
end module
