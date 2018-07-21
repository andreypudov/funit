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

module NotSameConditionUnit

    use Unit
    use Conditions

    implicit none
    private

    type, extends(UnitSuite), public :: NotSameConditionSuite
    private
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type
contains
    subroutine init(self, name)
        class(NotSameConditionSuite), intent(in out) :: self
        character(len=*), optional, intent(in)       :: name

        call self%UnitSuite%init('Not Same condition')

        call self%add(notSame_normal,   'Normal case')
        call self%add(notSame_negative, 'Negative case')
    end subroutine

    subroutine clean(self)
        class(NotSameConditionSuite), intent(in out) :: self

        call self%UnitSuite%clean()
    end subroutine

    subroutine notSame_normal(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        character,        pointer :: character1
        complex,          pointer :: complex1
        double precision, pointer :: dbleprcsn1
        integer,          pointer :: integer1
        logical,          pointer :: logical1
        real,             pointer :: real1

        character,        pointer :: character2
        complex,          pointer :: complex2
        double precision, pointer :: dbleprcsn2
        integer,          pointer :: integer2
        logical,          pointer :: logical2
        real,             pointer :: real2

        character,        target :: character3
        complex,          target :: complex3
        double precision, target :: dbleprcsn3
        integer,          target :: integer3
        logical,          target :: logical3
        real,             target :: real3

        character,        target :: character4
        complex,          target :: complex4
        double precision, target :: dbleprcsn4
        integer,          target :: integer4
        logical,          target :: logical4
        real,             target :: real4

        character3 = 'a'
        complex3   = (1.0, 0.1)
        dbleprcsn3 = 1.0_8
        integer3   = 1
        logical3   = .false.
        real3      = 1.0

        character4 = 'a'
        complex4   = (1.0, 0.1)
        dbleprcsn4 = 1.0_8
        integer4   = 1
        logical4   = .false.
        real4      = 1.0

        character1 => character3
        complex1   => complex3
        dbleprcsn1 => dbleprcsn3
        integer1   => integer3
        logical1   => logical3
        real1      => real3

        character2 => character4
        complex2   => complex4
        dbleprcsn2 => dbleprcsn4
        integer2   => integer4
        logical2   => logical4
        real2      => real4

        if (.not. notSame(character1, character2)) then
            print *, (.not. associated(character1, character2))
            call asserts%fail('character values are same')
        end if

        if (.not. notSame(complex1, complex2)) then
            call asserts%fail('complex values are same')
        end if

        if (.not. notSame(dbleprcsn1, dbleprcsn2)) then
            call asserts%fail('double precision values are same')
        end if

        if (.not. notSame(integer1, integer2)) then
            call asserts%fail('integer values are same')
        end if

        if (.not. notSame(logical1, logical2)) then
            call asserts%fail('logical values are same')
        end if

        if (.not. notSame(real1, real2)) then
            call asserts%fail('real values are same')
        end if
    end subroutine

    subroutine notSame_negative(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        character,        pointer :: character1
        complex,          pointer :: complex1
        double precision, pointer :: dbleprcsn1
        integer,          pointer :: integer1
        logical,          pointer :: logical1
        real,             pointer :: real1

        character,        pointer :: character2
        complex,          pointer :: complex2
        double precision, pointer :: dbleprcsn2
        integer,          pointer :: integer2
        logical,          pointer :: logical2
        real,             pointer :: real2

        character,        target :: character3
        complex,          target :: complex3
        double precision, target :: dbleprcsn3
        integer,          target :: integer3
        logical,          target :: logical3
        real,             target :: real3

        character3 = 'a'
        complex3   = (1.0, 0.1)
        dbleprcsn3 = 1.0_8
        integer3   = 1
        logical3   = .false.
        real3      = 1.0

        character1 => character3
        complex1   => complex3
        dbleprcsn1 => dbleprcsn3
        integer1   => integer3
        logical1   => logical3
        real1      => real3

        character2 => character3
        complex2   => complex3
        dbleprcsn2 => dbleprcsn3
        integer2   => integer3
        logical2   => logical3
        real2      => real3

        if (notSame(character1, character2)) then
            call asserts%fail('character values are not same')
        end if

        if (notSame(complex1, complex2)) then
            call asserts%fail('complex values are not same')
        end if

        if (notSame(dbleprcsn1, dbleprcsn2)) then
            call asserts%fail('double precision values are not same')
        end if

        if (notSame(integer1, integer2)) then
            call asserts%fail('integer values are not same')
        end if

        if (notSame(logical1, logical2)) then
            call asserts%fail('logical values are not same')
        end if

        if (notSame(real1, real2)) then
            call asserts%fail('real values are not same')
        end if
    end subroutine
end module
