#!/bin/sh
#
# $fiHeader$
#
# make a tree of links
#

original=$1
linkdir=$2

# remove any existing links

if test -d $linkdir; then
  echo "$linkdir already exists - removing old links"
  find $linkdir -type l -print | xargs /bin/rm -f
else
  echo "creating $linkdir"
  mkdir $linkdir
fi

# create the directory tree

for dir in `(cd $original; 
	     find * -type d -a ! -name CVS -print)` ; do
	if test ! -d $linkdir/$dir; then 
	  echo "creating $linkdir/$dir"
	  mkdir $linkdir/$dir
	fi
done

# and now do the links

echo "creating new links"

for file in `(cd $original;
	      find * -type f ! -name .\* ! -name \*~ ! -name \#\*\# \
		     -print \
		      -o -name CVS -prune)` ; do
	ln -s $original/$file $linkdir/$file
done

