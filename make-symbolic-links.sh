#!/bin/sh
#
# $fiHeader: make-symbolic-links.sh,v 1.1 1994/12/04 23:56:36 colin Exp $
#
# make a tree of links
#

# 'original' must be changed whenever the source tree is moved
# It must be a pathname accessible from any machine on the net

original=/net/louie/usr/tech/colin/clim-2.0
linkdir=$1

if test -z "$linkdir"; then
	linkdir=.
fi

echo "linking directory tree $linkdir to $original"

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

