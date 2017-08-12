# The Zeta Types Haskell Project

## The Project

This project is an implementation of tannakian symbols and zeta types in haskell.

## Installation

 1. Download the haskell files in [src/core](https://github.com/torstein-vik/zeta-types/tree/master/src/core)
 2. Install the haskell platform (for instance [https://www.haskell.org/platform/](https://www.haskell.org/platform/))
 3. Open a command prompt (cmd, bash, etc...)
 4. Navigate to the folder containing the haskell files
 5. Type "ghci *.hs" to compile and load the haskell modules
 6. Type ":load TannakianSymbols" to load the TannakianSymbols module (or any other for that matter) 

#### Note:

It is probable that during these stages of early development, some modules might not compile, especially LocalZetaTypes. The other modules however are still usable, and ghci will tell you which.

## Contributors
_Ask me ([torsteinv64@gmail.com](torsteinv64@gmail.com)) to add you here if you contribute to this project_
* Torstein Vik

## Copyright/License

This framework is and will remain completely open source, under the GNU General Public License version 3+:

    Copyright (C) 2017, Torstein Vik.

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.

## FOLDER STRUCTURE

* src -- source code
* -> core -- main implementation of project
* -> multiplicative-functions -- implementation specific to tannakian symbols from multiplicative functions
* -> pattern -- pattern detection code
* -> case studies -- code for specific cases
