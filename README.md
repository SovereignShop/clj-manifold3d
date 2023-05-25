# clj-manifold3d

This library provides a Clojure(Script) wrapper over Emmett Lalish's incredible Manifold 3D geometry library. It is based on JNI bindings to c++ produced via. javacpp: see https://github.com/SovereignShop/manifold

It implements most of the library functionality, plus extends it to support convex hulls (2D and 3D), partial
revolutions, and polyhedrons. It provides a full superset of OpenSCAD functionality. It's intended to make 
migration as easy as possible

Manifold represents a dramatic advance in the state-of-the-art in the open-source programmatic CAD. It has been adopted by most major CAD kernels.



The library aspires to achieve code compatibility between Clojure and ClojureScript so that models
build in the more friendly Java environment can be shared and distributed in the javascript
environment. However, there are challenges in the way the Manifold js library is provided
as a promise. To (mostly) support this, this library elects to accept promises at the API level.
Working with promises can be pretty annoying, especially without a type system that supports
them well. For this reason, the CLJS API generally also works on non-promise objects.

# Status

Alpha, core API is unlikely to change much but test coverage is not complete. There likely are bugs. The CLJS implementation especially is likely to change.

# Documentation

See the core namespace for some documentation. Refer to the original library for more complete documentation. 
