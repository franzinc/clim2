The Common Lisp Interface Manager
=================================

Description
-----------

The contents of this repo are known to compile and work in
Allegro CL 8.2 and later.  No work has been done to make it work on
other Common Lisps, even though there is considerable
conditionalization for many other CLs.

See the file LICENSE for information on the license for this source
code.


To build at a clim using an mlisp at a non-Franz-Inc site:

 1. There are a number of Makefile.nonfranz.x files (where x is a
    particular architecture and/or operating system) in the
    makefile.templates direcotry.  Select one that is closest and
    modify for your needs, and copy or create a new one for your
    specific machine and os.  Modify paths needed for mlisp programs
    and images, as needed.

 2. On the local clim2 direcotry on your machine, link the selected or
    created file to Makefile.nonfranz, e.g.:

    ```
    ln -s makefile.templates/Makefile.nonfranz.macosarm64 Makefile.nonfranz
    ```

    If you are working on Windows, and don't use Cygwin, you may have
    to copy the selected makefile, rather than soft-linking it,
    because Windows does not itself support soft-links.

 3. Type

    ```
    make clean
    make
    ```
