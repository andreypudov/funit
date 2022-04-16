!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

module TrueConditionUnit

    use FUnit
    use Conditions

    implicit none
    private

    type, extends(UnitSuite), public :: TrueConditionSuite
    private
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type
contains
    subroutine init(self, name)
        class(TrueConditionSuite),   intent(in out) :: self
        character(len=*), optional, intent(in)     :: name

        call self%UnitSuite%init('True condition')

        call self%add(true_normal,   'Normal case')
        call self%add(true_negative, 'Negative case')
    end subroutine

    subroutine clean(self)
        class(TrueConditionSuite), intent(in out) :: self

        call self%UnitSuite%clean()
    end subroutine

    subroutine true_normal(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        if (.not. true(.true.)) then
            call asserts%fail()
        end if
    end subroutine

    subroutine true_negative(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        if (true(.false.)) then
            call asserts%fail()
        end if
    end subroutine
end module
