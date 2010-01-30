

# Setting up a build/development environment

1. Download and setup SBT, version xsbt-launch-0.6.12.jar or later :
   http://code.google.com/p/simple-build-tool, the setup is quite simple 
   http://code.google.com/p/simple-build-tool/wiki/Setup

2. Download or clone repository : git clone git://github.com/max-l/Squeryl.git

3. Download Squeryl's single dependency : cglib-nodep-2.2.jar
   from : http://sourceforge.net/projects/cglib/files/ and
   copy it to a 'lib' directory in you_local_checkout_root, i.e.
   place it into : you_local_checkout_root/lib

4. In a command shell, cd to the project's root directory, and launch SBT :
   by typing `sbt`. This will fetch the required version of Scala both for
   SBT itself and for Squeryl

5. You can now compile with the sbt 'compile' or create the jar with SBTs
  'package' command. Note that SBT must have javac in the shell's path
   variable.

6. To run the test suite, use the SBT target test-run, this will run the
  test against the H2 database, that can be downloaded from :
  http://www.h2database.com/html/download.html or 
  http://repo2.maven.org/maven2/com/h2database/h2/1.2.127/h2-1.2.127.jar
  The jar must be placed in : you_local_checkout_root/lib
  Other jdbc drivers are needed to run the full test suite, and of course
  the databases must be installed and configured.
  
7. The repo contains IDEAs .iml files, and the required filed in .idea,
   So to get up and running installing IDEA and the Scala plugin should be
   all that is necessary.
   http://www.jetbrains.net/confluence/display/SCA/Getting+Started+with+IntelliJ+IDEA+Scala+Plugin
   

# Dependencies
  cglib-nodep-2.2.jar