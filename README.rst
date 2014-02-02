#######
lfe-hop
#######

A Hierarchical Ordered Planner for Lisp Flavored Erlang

Yes, this is Artificial Intelligence. The AI Winter is over. Lisp has
survived, and in fact, will probably never die. Get over it.


Introduction
============

Hierarchical ordered planners are part of what is called
"automated planning systems" in AI. The University of Maryland has
sponsored/created several hierarchical ordered planners and has the
following information on their site:

* `The UMD SHOP Project`_
* SourceForge links to `SHOP, JSHOP, SHOP2, and JSHOP2`_ downloads
* Bitbucket link to `pyhop`_, a Python implementation

lfe-hop aims to provide a hierarchical ordered planner implementation in LFE
for use in Erlang projects.

In addition to the efforts linked above, lfe-hop borrows heavily from
`Peter Norvig`_'s famous Lisp AI tour de force, `PAIP`_. Also, please note:
though inspired by the work at UMD, lfe-hop is not affiliated with that
institution.


Further Information
-------------------

* `Lecture notes & slides`_ for an `automated planning course`_ taught at UMD
* `Automated planning and scheduling`_ on Wikipedia
* `Hierarchical task networks`_ on Wikipedia


Dependencies
------------

This project assumes that you have `rebar`_ installed somwhere in your
``$PATH``.

This project depends upon the following, which installed to the ``deps``
directory of this project when you run ``make deps``:

* `LFE`_ (Lisp Flavored Erlang; needed only to compile)
* `lfeunit`_ (needed only to run the unit tests)


Installation
============

Add content to me here!


Usage
=====

Add content to me here!


.. Links
.. -----
.. _rebar: https://github.com/rebar/rebar
.. _LFE: https://github.com/rvirding/lfe
.. _lfeunit: https://github.com/lfe/lfeunit
.. _The UMD SHOP Project: http://www.cs.umd.edu/projects/shop/description.html
.. _SHOP, JSHOP, SHOP2, and JSHOP2: http://sourceforge.net/projects/shop/files/
.. _pyhop: https://bitbucket.org/dananau/pyhop/src
.. _Lecture notes & slides: http://www.cs.umd.edu/~nau/planning/slides/
.. _automated planning course: http://www.cs.umd.edu/~nau/cmsc722/
.. _Automated planning and scheduling: https://en.wikipedia.org/wiki/Automated_planning_and_scheduling
.. _Hierarchical task networks: https://en.wikipedia.org/wiki/Hierarchical_task_network
.. _Peter Norvig: http://norvig.com/
.. _PAIP: http://norvig.com/paip.html
