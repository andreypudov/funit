!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (Conditions) True

    implicit none

contains
    module pure function true(condition) result(value)
        logical, intent(in) :: condition
        logical value

        value = condition
    end function
end submodule
