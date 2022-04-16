!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

module NullConditionUnit

    use FUnit
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

        if (.not. null(character1)) then
            print *, null(character1)
            call asserts%fail('character value is not null')
        end if

        if (.not. null(complex1)) then
            call asserts%fail('complex value is not null')
        end if

        if (.not. null(dbleprcsn1)) then
            call asserts%fail('double precision value is not null')
        end if

        if (.not. null(integer1)) then
            call asserts%fail('integer value is not null')
        end if

        if (.not. null(logical1)) then
            call asserts%fail('logical value is not null')
        end if

        if (.not. null(real1)) then
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

        if (null(character1)) then
            call asserts%fail('character value is null')
        end if

        if (null(complex1)) then
            call asserts%fail('complex value is null')
        end if

        if (null(dbleprcsn1)) then
            call asserts%fail('double precision value is null')
        end if

        if (null(integer1)) then
            call asserts%fail('integer value is null')
        end if

        if (null(logical1)) then
            call asserts%fail('logical value is null')
        end if

        if (null(real1)) then
            call asserts%fail('real value is null')
        end if
    end subroutine
end module
