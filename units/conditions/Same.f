!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

module SameConditionUnit

    use FUnit
    use Conditions

    implicit none
    private

    type, extends(UnitSuite), public :: SameConditionSuite
    private
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type
contains
    subroutine init(self, name)
        class(SameConditionSuite), intent(in out) :: self
        character(len=*), optional, intent(in)    :: name

        call self%UnitSuite%init('Same condition')

        call self%add(same_normal,   'Normal case')
        call self%add(same_negative, 'Negative case')
    end subroutine

    subroutine clean(self)
        class(SameConditionSuite), intent(in out) :: self

        call self%UnitSuite%clean()
    end subroutine

    subroutine same_normal(self)
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

        if (.not. same(character1, character2)) then
            print *, (.not. associated(character1, character2))
            call asserts%fail('character values are same')
        end if

        if (.not. same(complex1, complex2)) then
            call asserts%fail('complex values are same')
        end if

        if (.not. same(dbleprcsn1, dbleprcsn2)) then
            call asserts%fail('double precision values are same')
        end if

        if (.not. same(integer1, integer2)) then
            call asserts%fail('integer values are same')
        end if

        if (.not. same(logical1, logical2)) then
            call asserts%fail('logical values are same')
        end if

        if (.not. same(real1, real2)) then
            call asserts%fail('real values are same')
        end if
    end subroutine

    subroutine same_negative(self)
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

        if (same(character1, character2)) then
            call asserts%fail('character values are not same')
        end if

        if (same(complex1, complex2)) then
            call asserts%fail('complex values are not same')
        end if

        if (same(dbleprcsn1, dbleprcsn2)) then
            call asserts%fail('double precision values are not same')
        end if

        if (same(integer1, integer2)) then
            call asserts%fail('integer values are not same')
        end if

        if (same(logical1, logical2)) then
            call asserts%fail('logical values are not same')
        end if

        if (same(real1, real2)) then
            call asserts%fail('real values are not same')
        end if
    end subroutine
end module
