Haskell Libraries
=================

The Haskell libraries that we recommend in general and when using streamly.

.. contents:: Table of Contents
   :depth: 1

Essential/High Level
--------------------

This is a list of essential libraries, not complete in any way. These are the
libraries that you should prefer over the second list when functionality is
overlapping. These are also the libraries that you should get familiar with
when beginning to write applications in Haskell.

Fundamental
~~~~~~~~~~~

* base
* template-haskell

Effect System
~~~~~~~~~~~~~

* transformers
* mtl
* exceptions

High Level Concurrency, Arrays, Streams, Parsers
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

* streamly

Data structures
~~~~~~~~~~~~~~~

* hashable
* containers
* heaps

Algorithms
~~~~~~~~~~

* random

Testing
~~~~~~~

* QuickCheck
* hspec

Benchmarking
~~~~~~~~~~~~

* gauge
* bench-show

Domain specific
~~~~~~~~~~~~~~~

Others/Low Level
----------------

GHC
~~~

* ghc-prim
* ghc

Effect Systems
~~~~~~~~~~~~~~

* monad-control

Low Level Concurrency
~~~~~~~~~~~~~~~~~~~~~

* stm
* atomic-primops
* lockfree-queue

Data Structures
~~~~~~~~~~~~~~~

* primitive
* abstract-deque

System
~~~~~~

* unix
* Win32
* process
* time

Networking
~~~~~~~~~~

* network

Others
~~~~~~

* deepseq
* base-orphans
* base-compat
* transformers-compat

Alternatives
------------

Canonical alternatives that we suggest for some exiting libraries:

* array => streamly
* async => streamly
* conduit => streamly
* pipes => streamly
* streaming => streamly
* vector => streamly
