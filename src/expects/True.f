!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (FUnit) TrueExpects

    use Conditions

    implicit none

    character(len=*), parameter :: default = 'True condition'

contains
    module subroutine true_expect(condition, message)
        character(len=*), optional, intent(in) :: message
        logical, intent(in) :: condition

        if (.not. true(condition)) then
            call fail_expect(message, default)
        end if
    end subroutine
end submodule
