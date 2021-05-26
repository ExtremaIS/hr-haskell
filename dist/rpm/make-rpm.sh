#!/usr/bin/env bash

set -o errexit
set -o nounset
#set -o xtrace

##############################################################################
# Library

die () {
  echo "error: ${*}" >&2
  exit 1
}

section () {
  echo "### ${*} ###"
}

##############################################################################
# Parse Arguments

if [ "$#" -ne "1" ] ; then
  echo "usage: ${0} hr-haskell-VERSION.tar.xz" >&2
  exit 2
fi

hr_source="${1}"
hr_dir="${hr_source%.tar.xz}"
[ "${hr_dir}" != "${hr_source}" ] \
  || die "invalid source filename: ${hr_source}"
hr_version="${hr_dir#hr-haskell-}"
[ "${hr_version}" != "${hr_dir}" ] \
  || die "invalid source filename: ${hr_source}"

##############################################################################
# Confirm Environment

test -n "${RPMFULLNAME}" || die "RPMFULLNAME not set"
test -n "${RPMEMAIL}" || die "RPMEMAIL not set"

arch="$(arch)"

[ -d "/host" ] || die "/host not mounted"
[ -f "/host/${hr_source}" ] || die "source not found: ${hr_source}"

##############################################################################
# Main

section "Building .rpm"
rpmdev-setuptree
cp "/host/${hr_source}" "/home/docker/rpmbuild/SOURCES"
cd "/tmp"
tar -Jxf "/host/${hr_source}" "${hr_dir}/dist/rpm/hr-haskell.spec"
sed \
  -e "s/{{ARCH}}/${arch}/" \
  -e "s/{{VERSION}}/${hr_version}/g" \
  -e "s/{{DATE}}/$(env LC_ALL=C date '+%a %b %d %Y')/" \
  -e "s/{{RPMFULLNAME}}/${RPMFULLNAME}/" \
  -e "s/{{RPMEMAIL}}/${RPMEMAIL}/" \
  "${hr_dir}/dist/rpm/hr-haskell.spec" \
  > "hr-haskell.spec"
rpmbuild -bs "hr-haskell.spec"
rpmbuild --rebuild "/home/docker/rpmbuild/SRPMS/"*".src.rpm"
cp "/home/docker/rpmbuild/SRPMS/"*".src.rpm" "/host"
cp "/home/docker/rpmbuild/RPMS/${arch}/"*".rpm" "/host"
