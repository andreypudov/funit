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
        class(ExampleSuite),    intent(in out) :: self
        character(len=*), optional, intent(in) :: name

        call self%UnitSuite%init('Example suite')

        call self%add(assertTrue_trueConstant_true, 'Positive assert case')
        call self%add(expectTrue_trueConstant_true, 'Positive expect case')
        ! call self%add(assertTrue_trueConstant_fail, 'Failing assert case')
        ! call self%add(expectTrue_trueConstant_fail, 'Failing expect case')
    end subroutine

    subroutine clean(self)
        class(ExampleSuite), intent(in out) :: self

        call self%UnitSuite%clean()
    end subroutine

    subroutine assertTrue_trueConstant_true(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        ! succesfull assert call
        call asserts%true(.true.)
    end subroutine

    subroutine expectTrue_trueConstant_true(self)
        class(UnitSuite), intent(in out) :: self
        type(Expects) expects

        ! succesfull expect call
        call expects%true(.true.)
    end subroutine expectTrue_trueConstant_true

    subroutine assertTrue_trueConstant_fail(self)
        class(UnitSuite), intent(in out) :: self
        type(Asserts) asserts

        ! failure assert call
        call asserts%true(.false., 'True condition')
    end subroutine assertTrue_trueConstant_fail

    subroutine expectTrue_trueConstant_fail(self)
        class(UnitSuite), intent(in out) :: self
        type(Expects) expects

        ! failure assert call
        call expects%true(.false., 'True condition')
    end subroutine
end module
