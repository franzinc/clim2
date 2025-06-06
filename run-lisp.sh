#! /bin/bash
#
# Note: This script file is copied and renamed from runlisp.sh in the cl
# repo.  It is not intended to track any runlisp.sh changes, and should be
# self-sufficient to allow clim2 builds whether at Franz sites or not.
#
# A portable way to run lisp from a makefile.  Needed because lisp.exe
# on Windows can't read from `stdin' and you can't redirect the output
# either.
#
# On Windows, use of this script relies on Cygwin.
# See http://cygwin.com for more information.
#
# Usage: runlisp [-e] [-i] [-t title] [-o output_file] [-w wrapper_script]
#                [-f file | form] ...
# run
#		-e	 ignore the exit status (for printing a message
#                        about the errors)
#		-i	 run in `interactive' mode (ie, lisp doesn't exit
#                        if an error occurs)
#		-t	 set the title of the window to `title'
#		-o	 send output to `output_file'.  `-' means no file
#		-f	 read forms from `file'
#               -w       the script that will invoke the lisp, if any;
#                        to support running under gdb; defaults to
#                        $RUNLISP_WRAPPER; see doc/run-under-gdb for more
#                        information
#		form use `form' to send to lisp
#		...	 other arguments passed to lisp
#
# `-f file | form' must be just before `...'.  -t is ignore on UNIX.
# An output_file of `-' does not work on Windows.

# For debugging uncomment the following line:
#set -x

#### done via other mechanisms now
# So we can run this script from places other than the src/ directory:
#SCRIPTDIR=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
#source $SCRIPTDIR/env.sh

if test -d c:/ -o -d //C/; then
    ON_WINDOWS=1
else
    ON_WINDOWS=0
fi

WRAPPER=${RUNLISP_WRAPPER:-""}

from_file=
output=
title=
batch="-batch -backtrace-on-error"
ignore_exit_status=xxx
if [ "$ON_WINDOWS" -eq 1 ]; then
    plus_args="+M +B +cn"
fi
form=

while test $# -gt 0; do
	case $1 in
	+p)	plus_args="$plus_args +p"
		;;
	-e)	ignore_exit_status=
		;;
	-f)	shift
		from_file=xxx
		form=$1
		;;
	-i)	batch=
		;;
	-o)	shift
		if test "X$1" != "X-"; then
			output=$1
		fi
		;;
	-t)	shift
		title="$1"
		;;
        -w)     shift
                WRAPPER="$1"
		;;
	*)	break
		;;
	esac
	shift
done

if test -z "$form"; then
	form=$1
	shift
fi

if test -n "$title" -a '(' -d c:/ -o -d //C/ ')'; then
	cl="$1 +t $title"
else
	cl=$1
fi
shift

other_lisp_args="$* -q $batch"

if [ "$ON_WINDOWS" -eq 1 -o -n "$WRAPPER" ]; then
# On Windows, we must have an output file, so send it to runlisp.out
# if the user didn't request one.
	if test -z "$output"; then
		output="runlisp.out"
	fi
	other_lisp_args="$other_lisp_args -d $output"
	if test -n "$from_file"; then
                # The location os the !@#$ +s arg is different on unix
                # and windows.
                if [ "$ON_WINDOWS" -eq 1 ]; then
	            echo $WRAPPER $cl $plus_args +s $form $other_lisp_args
	            echo "[lisp output redirected to $output]"
	            $WRAPPER $cl $plus_args +s $form $other_lisp_args
                else
	            echo $WRAPPER $cl $plus_args $other_lisp_args +s $form
	            echo "[lisp output redirected to $output]"
	            $WRAPPER $cl $plus_args $other_lisp_args +s $form
                fi
		status=$?
	else
		rm -f runlisp.tmp
		echo $form > runlisp.tmp
                if [ "$ON_WINDOWS" -eq 1 ]; then
		    echo $WRAPPER $cl $plus_args +s runlisp.tmp $other_lisp_args
		    echo "[lisp output redirected to $output]"
		    $WRAPPER $cl $plus_args +s runlisp.tmp $other_lisp_args
                else
		    echo $WRAPPER $cl $plus_args $other_lisp_args +s runlisp.tmp
		    echo "[lisp output redirected to $output]"
		    $WRAPPER $cl $plus_args $other_lisp_args +s runlisp.tmp
                fi
		status=$?
#		rm -f runlisp.tmp
	fi
	if test -n "${RUNLISP_SH_SHOW_OUTPUT-}"; then
		cat $output
	fi
## astore uses 124, and it's convenient not to have it flagged as an
## "error"
	if test -n "$ignore_exit_status" -a $status -ne 0 \
			-a $status -ne 123 -a $status -ne 124; then
		echo ""
		echo "==== check \"$output\" for errors and/or warnings ===="
	fi
else # on UNIX
	echo "$cl $other_lisp_args"
	if test -n "$from_file"; then
		start="cat $form"
	else
		start="echo $form"
	fi
	if test -n "$output"; then
		echo "[lisp output redirected to $output]"
		$start | $cl $other_lisp_args > $output 2>&1
		status=$?
		if test -n "$RUNLISP_SH_SHOW_OUTPUT"; then
			cat $output
		fi
	else
		$start | $cl $other_lisp_args
		status=$?
	fi
fi
exit $status
