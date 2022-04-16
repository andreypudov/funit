!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (Unit) FalseAsserts

    use Conditions

    implicit none

    character(len=*), parameter :: default = 'False condition'

contains
    module subroutine false_assert(condition, message)
        character(len=*), optional, intent(in) :: message
        logical, intent(in) :: condition

        if (.not. false(condition)) then
            call fail_assert(message, default)
        end if
    end subroutine
end submodule
