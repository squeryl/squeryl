if test -f .sbtconfig; then
  . .sbtconfig
fi
exec java ${SBT_OPTS} -jar sbt-launch.jar "$@"
