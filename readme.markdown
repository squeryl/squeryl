[![maven central](https://maven-badges.herokuapp.com/maven-central/org.squeryl/squeryl_2.12/badge.svg)](https://maven-badges.herokuapp.com/maven-central/org.squeryl/squeryl_2.12)
[![javadoc](https://javadoc.io/badge2/org.squeryl/squeryl_2.13/javadoc.svg)](https://javadoc.io/doc/org.squeryl/squeryl_2.13)

## How to build
1. Download or clone repository:  
     `git clone git@github.com:squeryl/squeryl.git`
2. Open a shell in the project's root directory and launch SBT with `./sbt`
   this will fetch the required version of Scala both for
   SBT itself and for Squeryl.
3. The 'test' command will run the test suite against the
   minimalist but very complete H2 database.
   Type 'package' to create a jar in ./target/[scala version].

## For more information
Documentation can be found at [The Squeryl Website][1] and [Scaladoc][3]. Questions can be directed to the [Google Group][2]

[1]: https://www.squeryl.org
[2]: https://groups.google.com/forum/#!forum/squeryl
[3]: https://javadoc.io/doc/org.squeryl/squeryl_2.13
