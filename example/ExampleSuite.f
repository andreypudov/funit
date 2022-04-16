!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

module ExampleUnit

    use FUnit

    implicit none
    private

    type, extends(UnitSuite), public :: ExampleSuite
    private
    contains
        procedure, pass :: init
        procedure, pass :: clean
    end type
contains
    subroutine init(self, name)
        class(ExampleSuite),   intent(in out) :: self
        character(len=*), optional, intent(in)     :: name

        call self%UnitSuite%init('Example suite')

        call self%add(true_normal,   'Normal case')
        call self%add(true_negative, 'Negative case')
    end subroutine

    subroutine clean(self)
        class(ExampleSuite), intent(in out) :: self

        call self%UnitSuite%clean()
    end subroutine

    subroutine true_normal(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts
        type(Expects) expects

        ! succesfull assert call
        call asserts%true(.true.)

        call expects%true(.false.)

        ! failure assert call
        ! call asserts%true(.false., 'True condition')
    end subroutine

    subroutine true_negative(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        call asserts%false(.false.)
    end subroutine
end module
