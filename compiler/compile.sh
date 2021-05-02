#ZEPHOME=/Users/jac/tech/zeppelin-0.9.0-bin-netinst
ZEPHOME=/usr/local/zeppelin-0.9.0-preview1-bin-all
(cd $PWD/../executor/spark && exec sbt package)
(cd $ZEPHOME/bin && sh zeppelin-daemon.sh restart)
echo "DONE!!"
