# The Zeta Types Project
#### _A framework for compution and storing of zeta types_

## The Project

The project mainly features a haskell backend, interfacing with python/sagemath via FFI and C. Haskell is used because its type- and pattern-matching system and are very practical for working with local zeta types (which may appear in at least four different forms.) We also plan to create a website giving access to information and resources about the grand project, as well as hopefully a large database of zeta-types from various L-functions, and a framework for interacting and computing.

## Installation

So far, only the haskell core is being developed. To interface with this, you can:
 1. Download the haskell files in [src/backend/core](https://github.com/torstein-vik/zeta-types/tree/master/src/backend/core)
 2. Install the haskell platform (for instance [https://www.haskell.org/platform/](https://www.haskell.org/platform/))
 3. Open a command prompt (cmd, bash, etc...)
 4. Navigate to the folder containing the haskell files
 5. Type "ghci *.hs" to compile and load the haskell modules
 6. Type ":load TannakianSymbols" to load the TannakianSymbols module (or any other for that matter) 

## Contributors
_Ask me ([torsteinv64@gmail.com](torsteinv64@gmail.com)) to add you here if you contribute to this project_
* Torstein Vik, Everything
* Andreas Holmstrøm, Design

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
    
## LANGUAGES / FRAMEWORKS

* Core: C (used to interface with other languages)
* Type system and primitive computation: Haskell
* Experiment Managment: Python/sagemath
* Database: RethinkDB / MongoDB
* Website: HTML, CSS, Javascript, JQuery (duh)

## FOLDER STRUCTURE

* doc -- all kinds of documentation
* src -- source code
* -> backend -- main backend logic of the program
* -> -> core -- data-types and primitive computation
* -> web -- the website
