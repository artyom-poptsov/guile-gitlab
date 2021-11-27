## Makefile.am -- Config file for GNU Automake.
##
## Copyright (C) 2021 Artyom V. Poptsov <poptsov.artyom@gmail.com>
##
## This file is part of Guile-GitLab.
##
## Guile-GitLab is free software: you can redistribute it and/or
## modify it under the terms of the GNU General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## Guile-GitLab is distributed in the hope that it will be useful, but
## WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
## General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with Guile-GitLab.  If not, see <http://www.gnu.org/licenses/>.

include $(top_srcdir)/build-aux/am/guile.am

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)/gitlab/api
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache/gitlab/api

SOURCES = \
	common.scm	\
	user.scm	\
	group.scm	\
	project.scm	\
	version.scm

SUFFIXES = .scm .go

guilec_opts = --load-path=$(abs_top_srcdir)/modules/
.scm.go:
	$(AM_V_GUILEC)$(guilec_env) $(GUILEC) $(guilec_opts) \
	--output=$@ $<

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

# Makefile.am ends here
