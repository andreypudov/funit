!
! A unit testing library for Fortran
!
! Copyright 2011-2022 Andrey Pudov. All Rights Reserved.
!
! Licensed under the Apache License, Version 2.0.
! See LICENSE.txt in the project root for license information.
!

submodule (Conditions) False

    implicit none

contains
    module pure function false(condition) result(value)
        logical, intent(in) :: condition
        logical value

        value = (.not. condition)
    end function
end submodule
