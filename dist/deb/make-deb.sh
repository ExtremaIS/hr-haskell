#!/usr/bin/env bash

set -o errexit
set -o nounset
#set -o xtrace

##############################################################################
# Constants

term_bold="$(tput bold)"
term_reset="$(tput sgr0)"

##############################################################################
# Library

die () {
  echo "error: ${*}" >&2
  exit 1
}

section () {
  echo "${term_bold}${*}${term_reset}"
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

test -n "${DEBFULLNAME}" || die "DEBFULLNAME not set"
test -n "${DEBEMAIL}" || die "DEBEMAIL not set"

arch="$(dpkg --print-architecture)"

[ -d "/host" ] || die "/host not mounted"
[ -f "/host/${hr_source}" ] || die "source not found: ${hr_source}"

##############################################################################
# Main

section "Building .deb"
cd "/tmp"
tar -Jxf "/host/${hr_source}"
[ -d "${hr_dir}" ] || die "source directory not found: ${hr_dir}"
cd "${hr_dir}"
dh_make --single --yes -f "/host/${hr_source}"
cd "debian"
rm -rf README.* hr-haskell* ./*.ex source
sed -i "s/^  \\*.*/  * Release ${hr_version}/" changelog
cd ..
sed "s/{ARCH}/${arch}/" dist/deb/control > debian/control
cp dist/deb/copyright debian
cp dist/deb/Makefile .
dpkg-buildpackage -us -uc
cd "/tmp"
rm -rf "${hr_dir}"
cp hr-haskell* /host
