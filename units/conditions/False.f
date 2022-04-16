!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

module FalseConditionUnit

    use Unit
    use Conditions

    implicit none
    private

    type, extends(UnitSuite), public :: FalseConditionSuite
    private
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type
contains
    subroutine init(self, name)
        class(FalseConditionSuite),  intent(in out) :: self
        character(len=*), optional, intent(in)     :: name

        call self%UnitSuite%init('False condition')

        call self%add(false_normal,   'Normal case')
        call self%add(false_negative, 'Negative case')
    end subroutine

    subroutine clean(self)
        class(FalseConditionSuite), intent(in out) :: self

        call self%UnitSuite%clean()
    end subroutine

    subroutine false_normal(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        if (.not. false(.false.)) then
            call asserts%fail()
        end if
    end subroutine

    subroutine false_negative(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        if (false(.true.)) then
            call asserts%fail()
        end if
    end subroutine
end module
