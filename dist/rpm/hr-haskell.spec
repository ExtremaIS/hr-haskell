Name:          hr-haskell
Version:       {{VERSION}}
Release:       1%{?dist}
Summary:       Horizontal rule for the terminal
License:       MIT
URL:           https://github.com/ExtremaIS/hr-haskell
Source0:       hr-haskell-{{VERSION}}.tar.xz
BuildArch:     {{ARCH}}
BuildRequires: make
Requires:      glibc,gmp
#ExcludeArch:

%description
hr is a utility for displaying a horizontal rule in a terminal.  It is useful
for marking a position in your terminal so that you can easily find it again.
For example, use `hr` to display a horizontal rule before each build of a
project so that you can easily find the beginning of the output of the last
build.

%global debug_package %{nil}

%prep
%setup -q

%build

%install
make install DESTDIR=%{buildroot} PREFIX=/usr

%check
make test

%files
%{_bindir}/hr
%{_mandir}/man1/hr.1.gz
%{_datadir}/doc/%{name}/

%changelog
* {{DATE}} {{RPMFULLNAME}} <{{RPMEMAIL}}> - {{VERSION}}-1
- Release {{VERSION}}
