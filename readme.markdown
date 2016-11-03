[![maven central](https://maven-badges.herokuapp.com/maven-central/org.squeryl/squeryl_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.squeryl/squeryl_2.12)
[![javadoc](http://javadoc-badge.appspot.com/org.squeryl/squeryl_2.12.svg?label=scaladoc)](http://javadoc-badge.appspot.com/org.squeryl/squeryl_2.12/org/squeryl/index.html)

## How to build
1. Download or clone repository:  
     `git clone git://github.com/squeryl/squeryl.git`
2. Open a shell in the project's root directory and launch SBT with `./sbt`
   this will fetch the required version of Scala both for
   SBT itself and for Squeryl.
3. The 'test' command will run the test suite against the
   minimalist but very complete H2 database.
   Type 'package' to create a jar in ./target/[scala version].

## For more information
Documentation can be found at [The Squeryl Website][1] and [Scaladoc] [3]. Questions can be directed to the [Google Group][2]

[![Build Status](https://travis-ci.org/squeryl/squeryl.svg?branch=master)](https://travis-ci.org/squeryl/squeryl)

[1]: https://squeryl.github.io
[2]: https://groups.google.com/forum/#!forum/squeryl
[3]: http://javadoc-badge.appspot.com/org.squeryl/squeryl_2.12/org/squeryl/index.html
