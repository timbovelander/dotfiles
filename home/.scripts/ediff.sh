#!/usr/bin/env bash

# test args
if [ ! ${#} -ge 2 ]; then
  echo 1>&2 "Ediff: ${0} LOCAL REMOTE [MERGED BASE]"
  exit 1
fi

# args
_LOCAL=${1}
_REMOTE=${2}
if [ ${3} ] ; then
  _MERGED=${3}
else
  _MERGED=${_REMOTE}
fi
if [ ${4} -a -r ${4} ] ; then
  _BASE=${4}
  _EDIFF=ediff-merge-files-with-ancestor
  _EVAL="${_EDIFF} \"${_LOCAL}\" \"${_REMOTE}\" \"${_BASE}\" nil \"${_MERGED}\""
elif [ ${_REMOTE} = ${_MERGED} ] ; then
  _EDIFF=ediff
  _EVAL="${_EDIFF} \"${_LOCAL}\" \"${_REMOTE}\""
else
  _EDIFF=ediff-merge-files
  _EVAL="${_EDIFF} \"${_LOCAL}\" \"${_REMOTE}\" nil \"${_MERGED}\""
fi

# run emacs
emacs -e "(${_EVAL})" 2>&1

# check modified file
if [ ! $(egrep -c '^(<<<<<<<|=======|>>>>>>>|####### Ancestor)' ${_MERGED}) = 0 ]; then
  _MERGEDSAVE=$(mktemp --tmpdir `basename ${_MERGED}`.XXXXXXXXXX)
  cp ${_MERGED} ${_MERGEDSAVE}
  echo 1>&2 "Oops! Conflict markers detected in $_MERGED."
  echo 1>&2 "Saved your changes to ${_MERGEDSAVE}"
  echo 1>&2 "Exiting with code 1."
  exit 1
fi

exit 0
